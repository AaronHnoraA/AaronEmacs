;;; tools/agent/notifications.el --- Quiet agent event notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs owns notification policy.  Providers and document workflows emit
;; structured events; this module never loads Noema or starts an agent.
;; See docs/design/agent-management.md C7-C9.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'aaron-lib)
(require 'aaron-notify)

(defgroup +agent-notify nil "Quiet agent notifications." :group 'applications)

(defcustom +agent-notify-events '(permission input failed)
  "Agent events eligible for a background notification.
Other terminal/attention events still enter notification history.  Include
`completed' to receive completion reminders too.  nil keeps all events quiet."
  :type '(set (const permission) (const input) (const failed) (const completed))
  :group '+agent-notify)

(defcustom +agent-notify-delay 2
  "Seconds to wait before reminding about an unresolved agent event."
  :type 'number :group '+agent-notify)

(defcustom +agent-notify-cooldown 30
  "Minimum seconds between background agent reminders."
  :type 'number :group '+agent-notify)

(defvar +agent-notify-inhibit-functions nil
  "Functions called with BUFFER and KIND before native ACP events.
A non-nil result means another workflow owns this event's delivery.")

(defvar +agent-notify-context-functions nil
  "Functions called with BUFFER to supply optional :label and :source.
The first non-nil plist wins.  These functions must only inspect state.")

(defvar +agent-notify--pending (make-hash-table :test #'equal))
(defvar +agent-notify--timer nil)
(defvar +agent-notify--last-delivered 0)
(defvar +agent-notify--next-id 0)
(defvar-local +agent-notify--last-key nil)
(defvar-local +agent-notify--subscription nil)
(defvar-local +agent-notify--subscription-state nil)
(defvar-local +agent-notify--permission-ids nil)
(defvar-local +agent-notify--input-ids nil)
(defvar-local +agent-notify-muted nil
  "Non-nil makes this internal agent session ineligible for notifications.")

(defvar agent-shell--state)
(defvar +agent-acp-notify-function)
(defvar +agent-acp-ephemeral-origins)
(defvar +agent-notifications-mode)
(declare-function agent-shell-subscribe-to "agent-shell" (&rest args))
(declare-function agent-shell-unsubscribe "agent-shell" (&rest args))
(declare-function agent-shell--display-buffer "agent-shell" (buffer))

(defun +agent-notify--cancel-timer ()
  "Release the one pending delivery timer."
  (when (timerp +agent-notify--timer)
    (cancel-timer +agent-notify--timer))
  (setq +agent-notify--timer nil))

(defun +agent-notify--clear (buffer)
  "Withdraw pending events of BUFFER and allow its next state to be recorded."
  (let (keys)
    (maphash (lambda (key event)
               (when (eq buffer (plist-get event :buffer)) (push key keys)))
             +agent-notify--pending)
    (dolist (key keys) (remhash key +agent-notify--pending)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (setq +agent-notify--last-key nil)))
  (when (zerop (hash-table-count +agent-notify--pending))
    (+agent-notify--cancel-timer)))

(defun +agent-notify--visit (buffer)
  "Visit BUFFER only after a person activates a notification."
  (if (buffer-live-p buffer)
      (if (and (fboundp 'agent-shell--display-buffer)
               (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
          (agent-shell--display-buffer buffer)
        (pop-to-buffer buffer))
    (aaron/notifications)))

(defun +agent-notify--record (event &optional silent)
  "Send EVENT through the shared notification boundary, optionally SILENT."
  (aaron-notify (plist-get event :title)
                :body (plist-get event :body)
                :level (plist-get event :level)
                :source (plist-get event :source)
                :id (plist-get event :history-id)
                :action (plist-get event :action)
                :silent silent))

(defun +agent-notify--flush ()
  "Coalesce pending events without interrupting a focused Emacs."
  (setq +agent-notify--timer nil)
  (let (events)
    (maphash (lambda (_ event)
               (when (and (buffer-live-p (plist-get event :buffer))
                          (not (buffer-local-value '+agent-notify-muted (plist-get event :buffer)))
                          (memq (plist-get event :kind) +agent-notify-events))
                 (push event events)))
             +agent-notify--pending)
    (clrhash +agent-notify--pending)
    (when (and events (not noninteractive) (not (aaron-notify--user-here-p)))
      (setq +agent-notify--last-delivered (float-time))
      (if (null (cdr events))
          (+agent-notify--record (car events))
        (aaron-notify
         (format "Agents · %d need attention" (length events))
         :body (mapconcat (lambda (kind)
                            (let ((count (cl-count kind events
                                                   :key (lambda (event) (plist-get event :kind)))))
                              (when (> count 0)
                                (format "%d %s" count
                                        (pcase kind
                                          ('permission "awaiting approval")
                                          ('input "awaiting an answer")
                                          ('failed "failed")
                                          (_ "completed"))))))
                          (seq-filter (lambda (kind)
                                        (cl-find kind events
                                                 :key (lambda (event) (plist-get event :kind))))
                                      '(permission input failed completed))
                          " · ")
         :source "Agents" :level 'warning :id "agents-attention"
         :action #'aaron/notifications)))))

(defun +agent-notify--schedule ()
  "Schedule a single coalesced reminder, respecting the global cooldown."
  (unless (timerp +agent-notify--timer)
    (setq +agent-notify--timer
          (run-at-time
           (max 0 +agent-notify-delay
                (- (+ +agent-notify--last-delivered +agent-notify-cooldown)
                   (float-time)))
           nil #'+agent-notify--flush))))

(cl-defun +agent-notify-event (buffer kind &key id label (source "Agents") action)
  "Consume structured agent KIND for BUFFER, optionally scoped by ID.
KIND is permission, input, failed, completed, started, resolved or cancelled.
LABEL and SOURCE are short metadata, never prompt or tool output.  ACTION
runs only on an explicit click.  Return non-nil when this policy owns delivery."
  (when (and +agent-notifications-mode (buffer-live-p buffer))
    (if (memq kind '(started resolved cancelled))
        (+agent-notify--clear buffer)
      (when (memq kind '(permission input failed completed))
        (let ((key (list (or id buffer) kind)))
          (unless (equal key (buffer-local-value '+agent-notify--last-key buffer))
            (+agent-notify--clear buffer)
            (with-current-buffer buffer (setq +agent-notify--last-key key))
            (let* ((description (pcase kind
                                  ('permission "needs approval")
                                  ('input "needs an answer")
                                  ('failed "failed")
                                  (_ "completed")))
                   (event (list :buffer buffer :kind kind :source source
                                :title (format "%s · %s" source description)
                                :body (or label (buffer-name buffer))
                                :level (pcase kind ('failed 'error) ('completed 'success) (_ 'warning))
                                :history-id (format "agent-event-%d" (cl-incf +agent-notify--next-id))
                                :action (or action (apply-partially #'+agent-notify--visit buffer)))))
              (with-current-buffer buffer
                (add-hook 'kill-buffer-hook #'+agent-notify--killed-h nil t))
              (+agent-notify--record event t)
              (when (memq kind +agent-notify-events)
                (puthash (or id buffer) event +agent-notify--pending)
                (+agent-notify--schedule)))))))
    t))

(defun +agent-notify--killed-h ()
  "Withdraw pending reminders for the current buffer."
  (+agent-notify--clear (current-buffer)))

(defun +agent-notify--native-event (buffer event)
  "Translate native ACP EVENT from BUFFER into the common event vocabulary."
  (let ((kind (pcase (map-elt event :event)
                ('permission-request 'permission)
                ('elicitation-request 'input)
                ('error 'failed)
                ('turn-complete
                 (if (equal (map-elt (map-elt event :data) :stop-reason) "cancelled")
                     'cancelled 'completed))
                ((or 'permission-response 'elicitation-response 'input-submitted 'clean-up) 'resolved))))
    (when (and (buffer-live-p buffer)
               (memq (map-elt event :event)
                     '(permission-request permission-response elicitation-request elicitation-response)))
      (with-current-buffer buffer
        (let* ((type (map-elt event :event))
               (request-id (map-elt (map-elt event :data) :request-id))
               (slot (if (memq type '(permission-request permission-response))
                         '+agent-notify--permission-ids '+agent-notify--input-ids)))
          (if (memq type '(permission-request elicitation-request))
              (when request-id (set slot (cl-adjoin request-id (symbol-value slot) :test #'equal)))
            (set slot (delete request-id (symbol-value slot)))
            (setq kind (cond (+agent-notify--permission-ids 'permission)
                             (+agent-notify--input-ids 'input)
                             (t 'resolved)))))))
    (when (and (buffer-live-p buffer)
               (memq (map-elt event :event) '(input-submitted clean-up turn-complete)))
      (with-current-buffer buffer
        (setq +agent-notify--permission-ids nil +agent-notify--input-ids nil)))
    (when (and kind (buffer-live-p buffer)
               (not (buffer-local-value '+agent-notify-muted buffer))
               (not (run-hook-with-args-until-success
                     '+agent-notify-inhibit-functions buffer kind)))
      (let ((context (run-hook-with-args-until-success
                      '+agent-notify-context-functions buffer)))
        (+agent-notify-event buffer kind
                             :label (plist-get context :label)
                             :source (or (plist-get context :source) "Agents"))))))

(defun +agent-notify--watch-h ()
  "Subscribe once to the current agent's initialized state."
  (when (and +agent-notifications-mode
             (bound-and-true-p agent-shell--state)
             (not (eq agent-shell--state +agent-notify--subscription-state)))
    (let ((buffer (current-buffer)) (state agent-shell--state))
      (+agent-notify--clear buffer)
      (setq +agent-notify--permission-ids nil +agent-notify--input-ids nil)
      (setq +agent-notify--subscription-state state
            +agent-notify--subscription
            (agent-shell-subscribe-to
             :shell-buffer buffer
             :on-event (lambda (event)
                         (when (and (buffer-live-p buffer)
                                    (eq state (buffer-local-value 'agent-shell--state buffer)))
                           (+agent-notify--native-event buffer event))))))))

(defun +agent-notify--enable ()
  "Watch existing and future native sessions without opening any."
  (add-hook 'agent-shell-mode-hook #'+agent-notify--watch-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode) (+agent-notify--watch-h)))))

(defun +agent-notify--stop ()
  "Release pending deliveries and native event subscriptions."
  (+agent-notify--cancel-timer)
  (clrhash +agent-notify--pending)
  (remove-hook 'agent-shell-mode-hook #'+agent-notify--watch-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and +agent-notify--subscription
                 (eq (bound-and-true-p agent-shell--state) +agent-notify--subscription-state))
        (agent-shell-unsubscribe :subscription +agent-notify--subscription))
      (setq +agent-notify--subscription nil +agent-notify--subscription-state nil
            +agent-notify--last-key nil +agent-notify--permission-ids nil +agent-notify--input-ids nil)
      (remove-hook 'kill-buffer-hook #'+agent-notify--killed-h t))))

(define-minor-mode +agent-notifications-mode
  "Record agent events quietly and merge actionable background reminders."
  :global t :group '+agent-notify
  (if +agent-notifications-mode
      (progn
        (add-hook 'kill-emacs-hook #'+agent-notify--stop)
        (when (featurep 'agent-shell) (+agent-notify--enable)))
    (remove-hook 'kill-emacs-hook #'+agent-notify--stop)
    (+agent-notify--stop)))

(after! agent-shell
  (when +agent-notifications-mode (+agent-notify--enable)))

(defun +agent-notify--runtime-context (buffer)
  "Use the shared runtime's session label without querying another service."
  (when (local-variable-p '+agent-acp-session-name buffer)
    (list :label (or (buffer-local-value '+agent-acp-session-name buffer) (buffer-name buffer))
          :source "Agents")))

(defun +agent-notify--runtime-inhibit (buffer kind)
  "Leave runtime attention to its structured hook and mute internal sessions."
  (or (and (local-variable-p '+agent-acp-session-origin buffer)
           (memq (buffer-local-value '+agent-acp-session-origin buffer)
                 (cons 'probe +agent-acp-ephemeral-origins)))
      (and (memq kind '(permission input failed completed))
           (local-variable-p '+agent-acp--attention-watched buffer)
           (buffer-local-value '+agent-acp--attention-watched buffer))))

(defun +agent-notify--runtime-attention (buffer reason)
  "Apply shared policy to BUFFER's runtime attention REASON."
  (when (memq +agent-acp-notify-function '(nil +agent-acp-notify-default))
    (when +agent-acp-notify-function
      (+agent-notify-event buffer (if (eq reason 'done) 'completed reason)
                           :label (or (buffer-local-value '+agent-acp-session-name buffer)
                                      (buffer-name buffer))))
    t))

(after! tools/agent/acp
  (add-hook '+agent-notify-context-functions #'+agent-notify--runtime-context)
  (add-hook '+agent-notify-inhibit-functions #'+agent-notify--runtime-inhibit)
  (add-hook '+agent-acp-notification-functions #'+agent-notify--runtime-attention))

(provide 'tools/agent/notifications)
;;; tools/agent/notifications.el ends here
