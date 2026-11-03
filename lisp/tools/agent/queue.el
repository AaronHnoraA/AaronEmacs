;;; tools/agent/queue.el --- Review and edit queued prompts -*- lexical-binding: t; -*-

;;; Commentary:
;; agent-shell owns delivery of `:pending-prompts'; the runtime holds what a
;; stop or failure took out of it (C37).  This module edits both as one ordered
;; list and resumes it.  The panel owns only its selection: closing it changes
;; nothing, and it never sends a prompt without an explicit key.  See C39.

;;; Code:

(require 'seq)
(require 'tools/agent/sessions)
(require 'aaron-ui-etaf)
(require 'tools/agent/ui)

(declare-function aaron-ui-float-show "aaron-ui-float" (buffer &rest options))
(declare-function aaron-ui-float-panel-for-buffer "aaron-ui-float" (buffer))
(declare-function aaron-ui-float-hide "aaron-ui-float" (&optional panel))
(declare-function agent-shell-prompt-steer "agent-shell-prompt-queue" (&optional prompt))
(declare-function agent-shell-steering-supported-p "agent-shell" (&rest args))
(declare-function agent-shell--prompt-queue-process-next "agent-shell-prompt-queue" ())
(declare-function agent-shell--prompt-queue-read "agent-shell-prompt-queue" (&rest args))
(defvar agent-shell-prompt-queue-merge)

(defconst +agent-queue-buffer-name "*Agent Queue*")

;;;; The queue as one list

(defun +agent-queue-items (buffer)
  "Return BUFFER's queued prompts in sending order, held ones first."
  (append (buffer-local-value '+agent-acp--held-prompts buffer)
          (+agent-acp-state-value buffer '(:pending-prompts))))

(defun +agent-queue--store (buffer items held)
  "Make ITEMS BUFFER's queue, the first HELD of them held."
  (with-current-buffer buffer
    (setq +agent-acp--held-prompts (seq-take items held))
    (map-put! agent-shell--state :pending-prompts (seq-drop items held))
    (+agent-acp--changed)))

(defun +agent-queue--require-index (buffer index)
  "Signal unless INDEX still names one of BUFFER's queued prompts."
  (unless (and (natnump index) (< index (length (+agent-queue-items buffer))))
    (user-error "That prompt has left the queue")))

(defun +agent-queue--ready (buffer)
  "Signal unless BUFFER's client can accept a prompt now."
  (when (buffer-local-value 'remote-buffer-disconnected-p buffer)
    (user-error "Remote target is disconnected; reconnect the session first"))
  (when (+agent-acp-client-exited-p buffer)
    (user-error "The agent process has ended; reconnect the session first"))
  (unless (eq (+agent-acp-state-value buffer '(:init-finished)) t)
    (user-error "The agent is still starting")))

(defun +agent-queue-remove (buffer index)
  "Remove the prompt at INDEX of BUFFER's queue."
  (+agent-queue--require-index buffer index)
  (let ((held (+agent-acp-held-prompt-count buffer))
        (items (+agent-queue-items buffer)))
    (+agent-queue--store buffer
                         (append (seq-take items index) (seq-drop items (1+ index)))
                         (if (< index held) (1- held) held))))

(defun +agent-queue-edit (buffer index text)
  "Replace the prompt at INDEX of BUFFER's queue with TEXT."
  (+agent-queue--require-index buffer index)
  (when (string-empty-p (string-trim text)) (user-error "The prompt is empty"))
  (let ((items (copy-sequence (+agent-queue-items buffer))))
    (setf (nth index items) text)
    (+agent-queue--store buffer items (+agent-acp-held-prompt-count buffer))))

(defun +agent-queue-move (buffer index delta)
  "Move the prompt at INDEX of BUFFER's queue by DELTA; return its new index.
Held is a property of the positions, so a prompt moved past them joins them."
  (+agent-queue--require-index buffer index)
  (let* ((items (copy-sequence (+agent-queue-items buffer)))
         (target (max 0 (min (1- (length items)) (+ index delta))))
         (prompt (nth index items)))
    (setq items (append (seq-take items index) (seq-drop items (1+ index))))
    (setq items (append (seq-take items target) (list prompt) (seq-drop items target)))
    (+agent-queue--store buffer items (+agent-acp-held-prompt-count buffer))
    target))

(defun +agent-queue-clear (buffer)
  "Remove every queued prompt of BUFFER."
  (+agent-queue--store buffer nil 0))

(defun +agent-queue-hold (buffer)
  "Hold all of BUFFER's queue until an explicit resume."
  (let ((items (+agent-queue-items buffer)))
    (+agent-queue--store buffer items (length items))))

;;;###autoload
(defun +agent-queue-resume (&optional buffer)
  "Release BUFFER's held prompts and send the queue, now or after this turn."
  (interactive)
  (let* ((buffer (+agent-acp-command-buffer buffer))
         (items (+agent-queue-items buffer)))
    (unless items (user-error "No queued prompts"))
    (+agent-queue--ready buffer)
    (+agent-queue--store buffer items 0)
    (if (+agent-acp-busy-p buffer)
        (message "Queued prompts will follow the running turn")
      (with-current-buffer buffer (agent-shell--prompt-queue-process-next)))))

(defun +agent-queue-send-now (buffer index)
  "Send the prompt at INDEX of BUFFER's queue without waiting for its turn.
A running turn receives it by steering.  An agent that cannot steer refuses:
interrupting its work is a separate, explicit decision."
  (+agent-queue--require-index buffer index)
  (+agent-queue--ready buffer)
  (let ((busy (+agent-acp-busy-p buffer))
        (items (+agent-queue-items buffer))
        (held (+agent-acp-held-prompt-count buffer))
        (sent nil))
    (when (and busy (not (with-current-buffer buffer (agent-shell-steering-supported-p))))
      (user-error "This agent cannot take input during a turn; stop it first"))
    ;; The prompt leaves the queue only once it has gone: agent-shell may ask
    ;; first ("Steer anyway?" while a request waits on you), and a no there
    ;; must leave it where it was.  A steer declined later is held (C37).
    (unwind-protect
        (let ((prompt (nth index items)))
          (+agent-queue-remove buffer index)
          (with-current-buffer buffer
            (if busy
                (agent-shell-prompt-steer prompt)
              (map-put! agent-shell--state :pending-prompts
                        (cons prompt (map-elt agent-shell--state :pending-prompts)))
              (let ((agent-shell-prompt-queue-merge nil))
                (agent-shell--prompt-queue-process-next))))
          (setq sent t))
      (unless sent
        (+agent-queue--store buffer items held)))))

;;;; Panel

(defvar-local +agent-queue--session nil "The agent buffer this panel shows.")
(defvar-local +agent-queue--selected 0 "Index of the selected prompt.")
(defvar-local +agent-queue--model nil)
(defvar-local +agent-queue--origin nil)

(defun +agent-queue--row-ref (index)
  "Return the retained Host identity of row INDEX."
  (intern (format "agent-queue-row-%d" index)))

(defun +agent-queue--session-live ()
  "Return the panel's session, or signal that it has ended."
  (unless (+agent-acp-agent-buffer-p +agent-queue--session)
    (user-error "This session has ended"))
  +agent-queue--session)

(defun +agent-queue--state-line (session)
  "Describe agent SESSION's execution state in one line."
  (let ((snapshot (+agent-session-snapshot session)))
    (string-join (cons (+agent-sessions--state snapshot)
                       (+agent-session-progress snapshot))
                 " · ")))

(defun +agent-queue-refresh ()
  "Redraw the panel from the session's real queue."
  (interactive)
  (when (and +agent-queue--model (+agent-acp-agent-buffer-p +agent-queue--session))
    (let* ((session +agent-queue--session)
           (items (+agent-queue-items session))
           (count (length items)))
      (setq +agent-queue--selected (max 0 (min +agent-queue--selected (1- count))))
      (etaf-set-value +agent-queue--model
                      (list :items items :selected +agent-queue--selected
                            :held (+agent-acp-held-prompt-count session)
                            :title (+agent-acp--tab-label session)
                            :state (+agent-queue--state-line session))))))

(defun +agent-queue--focus ()
  "Keep keyboard focus on the selected row after a redraw."
  (when-let* ((runtime (etaf-runtime-for-buffer (current-buffer)))
              ((+agent-queue-items +agent-queue--session)))
    (etaf-runtime-flush runtime)
    (etaf-focus runtime (+agent-queue--row-ref +agent-queue--selected))
    (etaf-scroll-into-view runtime (+agent-queue--row-ref +agent-queue--selected))))

(defun +agent-queue--changed-h (source)
  "Follow changes to the shown session while the panel is on screen."
  (when-let* ((panel (get-buffer +agent-queue-buffer-name)))
    (with-current-buffer panel
      (cond ((not (get-buffer-window panel 'visible)) (+agent-queue--suspend))
            ((not (buffer-live-p +agent-queue--session)) (+agent-queue-hide))
            ((eq source +agent-queue--session) (+agent-queue-refresh))))))

(defun +agent-queue--suspend (&optional _panel)
  "Release the panel's event subscription; the queue is unaffected."
  (remove-hook '+agent-acp-changed-functions #'+agent-queue--changed-h))

(defun +agent-queue-hide ()
  "Close the panel and return to where it was opened."
  (interactive)
  (let ((origin +agent-queue--origin) (buffer (current-buffer)))
    (+agent-queue--suspend)
    (if-let* (((featurep 'aaron-ui-float))
              (panel (aaron-ui-float-panel-for-buffer buffer)))
        (aaron-ui-float-hide panel)
      (quit-window))
    (when (window-live-p origin) (select-window origin))))

(defun +agent-queue-next (&optional count)
  "Select the COUNTth next prompt."
  (interactive "p")
  (let ((length (length (+agent-queue-items (+agent-queue--session-live)))))
    (when (> length 0)
      (setq +agent-queue--selected (mod (+ +agent-queue--selected (or count 1)) length))
      (+agent-queue-refresh)
      (+agent-queue--focus))))

(defun +agent-queue-previous () "Select the previous prompt." (interactive) (+agent-queue-next -1))

(defun +agent-queue-edit-selected ()
  "Edit the selected prompt in the minibuffer."
  (interactive)
  (let* ((session (+agent-queue--session-live))
         (index +agent-queue--selected)
         (old (progn (+agent-queue--require-index session index)
                     (nth index (+agent-queue-items session))))
         (text (with-current-buffer session
                 (agent-shell--prompt-queue-read :initial old))))
    (+agent-queue-edit session index text)
    (+agent-queue-refresh)))

(defun +agent-queue-remove-selected ()
  "Remove the selected prompt."
  (interactive)
  (+agent-queue-remove (+agent-queue--session-live) +agent-queue--selected)
  (+agent-queue-refresh)
  (+agent-queue--focus))

(defun +agent-queue--move-selected (delta)
  "Move the selected prompt by DELTA and keep it selected."
  (setq +agent-queue--selected
        (+agent-queue-move (+agent-queue--session-live) +agent-queue--selected delta))
  (+agent-queue-refresh)
  (+agent-queue--focus))

(defun +agent-queue-move-down () "Send the selected prompt later." (interactive) (+agent-queue--move-selected 1))
(defun +agent-queue-move-up () "Send the selected prompt earlier." (interactive) (+agent-queue--move-selected -1))

(defun +agent-queue-send-selected ()
  "Send the selected prompt now."
  (interactive)
  (+agent-queue-send-now (+agent-queue--session-live) +agent-queue--selected)
  (+agent-queue-refresh))

(defun +agent-queue-hold-all ()
  "Hold the whole queue."
  (interactive)
  (+agent-queue-hold (+agent-queue--session-live))
  (+agent-queue-refresh))

(defun +agent-queue-resume-all ()
  "Resume the whole queue."
  (interactive)
  (+agent-queue-resume (+agent-queue--session-live))
  (+agent-queue-refresh))

(defun +agent-queue-clear-all ()
  "Remove the whole queue, after asking."
  (interactive)
  (let* ((session (+agent-queue--session-live))
         (count (length (+agent-queue-items session))))
    (when (and (> count 0)
               (y-or-n-p (format "Remove all %d queued prompts? " count)))
      (+agent-queue-clear session)
      (+agent-queue-refresh))))

(defun +agent-queue--hint (held count)
  "Say what will happen to COUNT prompts, HELD of them held."
  (cond ((zerop count) "Nothing is queued.")
        ((> held 0) "Held after a stop or failure. r resumes them.")
        (t "Sent when the running turn ends normally.")))

(etaf-define-component +agent-queue-page (&key model owner)
  "The queued prompts of one session."
  :setup (aaron-ui-etaf-provide-theme)
  :render
  (let* ((_theme (etaf-value (aaron-ui-etaf-theme-ref)))
         (data (etaf-value model))
         (items (plist-get data :items))
         (held (or (plist-get data :held) 0))
         (selected (or (plist-get data :selected) 0)))
    (etaf-view
     (column :width '(vw 100) :height '(vh 100) :overflow 'hidden
             :padding '((lh 1) (ch 2)) :item-gap '(lh 0.5)
             :on-dismiss (lambda () (with-current-buffer owner (+agent-queue-hide)))
             :help "Queued prompts wait here. Closing this panel changes nothing."
             :keys '(("j" +agent-queue-next "Next prompt")
                     ("k" +agent-queue-previous "Previous prompt")
                     ("e" +agent-queue-edit-selected "Edit prompt")
                     ("d" +agent-queue-remove-selected "Remove prompt")
                     ("J" +agent-queue-move-down "Send later")
                     ("K" +agent-queue-move-up "Send earlier")
                     ("s" +agent-queue-send-selected "Send now")
                     ("r" +agent-queue-resume-all "Resume queue")
                     ("h" +agent-queue-hold-all "Hold queue")
                     ("x" +agent-queue-clear-all "Remove all")
                     ("g" +agent-queue-refresh "Refresh")
                     ("q" +agent-queue-hide "Close"))
       (etaf-label :text "Message queue" :variant 'heading)
       (etaf-label :text (or (plist-get data :title) "Queue") :variant 'strong)
       (etaf-label :text (or (plist-get data :state) "") :variant 'muted :wrap-mode 'word)
       (row :width 'stretch :item-gap '(ch 2)
         (etaf-badge :text (format "%d pending" (length items)) :variant 'info)
         (expr (when (> held 0)
                 (etaf-view (etaf-badge :text (format "%d held" held) :variant 'warning)))))
       (etaf-label :text (+agent-queue--hint held (length items)) :wrap-mode 'word :variant 'muted)
       (column :ref 'agent-queue-list :width 'stretch
               :height (list 'lh (max 3 (- (or (cdr (etaf-viewport)) 24) 10)))
               :overflow 'scroll :item-gap '(lh 0.5)
         (expr (when (null items)
                 (etaf-view (etaf-empty :title "Nothing queued"
                                       :description "Follow-up messages appear here while the agent works."))))
         (expr
          (seq-map-indexed
           (lambda (prompt index)
             (etaf-view
              (etaf-button
               :key (+agent-queue--row-ref index) :ref (+agent-queue--row-ref index)
               :variant 'ghost :aria-selected (= index selected) :padding '((lh 0) (ch 2))
               :bgcolor (aaron-ui-color (if (= index selected) 'bg-surface-strong 'bg-base))
               :label (format "%d%s  %s" (1+ index) (if (< index held) " held" "")
                              (truncate-string-to-width
                               (replace-regexp-in-string "[ \t\n]+" " " prompt) 70 nil nil "…"))
               :help-echo prompt
               :on-press (lambda ()
                           (with-current-buffer owner
                             (setq +agent-queue--selected index)
                             (+agent-queue-refresh)))
               (column :width 'stretch :item-gap '(lh 0)
                 (etaf-label :text (format "%02d · %s" (1+ index)
                                          (if (< index held) "Held for review" "After this turn"))
                             :color (aaron-ui-color (if (< index held) 'accent-yellow 'fg-muted)))
                 (etaf-label :text (truncate-string-to-width
                                   (replace-regexp-in-string "[ \t\n]+" " " prompt) 180 nil nil "…")
                             :wrap-mode 'word)))))
           items)))
       (etaf-label :text "e edit · d remove · J/K order · s send now · r resume · h hold · q close"
                   :variant 'muted :wrap-mode 'word)))))

(defun +agent-queue--mount (session)
  "Mount or reuse the panel buffer for agent SESSION and return it."
  (let ((buffer (get-buffer-create +agent-queue-buffer-name)))
    (with-current-buffer buffer
      (unless (and +agent-queue--model (etaf-runtime-for-buffer buffer))
        (let ((model (etaf-ref (list :items nil))))
          (etaf-mount buffer (etaf-view (+agent-queue-page :model model :owner buffer)))
          (setq-local +agent-queue--model model)))
      (setq-local +agent-queue--session session
                  +agent-queue--selected 0
                  default-directory (buffer-local-value 'default-directory session))
      (+agent-queue-refresh))
    buffer))

;;;###autoload
(defun +agent/queue (&optional session)
  "Manage the queued prompts of agent SESSION."
  (interactive)
  (let* ((session (+agent-acp-command-buffer session))
         (origin (selected-window))
         (buffer (+agent-queue--mount session)))
    (with-current-buffer buffer (setq-local +agent-queue--origin origin))
    (add-hook '+agent-acp-changed-functions #'+agent-queue--changed-h)
    (if (and (display-graphic-p) (require 'aaron-ui-float nil t) (posframe-workable-p))
        (aaron-ui-float-show buffer :key 'agent-queue :title "Agent queue"
                             :width '(fraction 0.6) :height '(fraction 0.5)
                             :owned t :on-hide #'+agent-queue--suspend)
      (pop-to-buffer buffer))
    (with-current-buffer buffer (+agent-queue--focus))
    buffer))

(provide 'tools/agent/queue)
;;; tools/agent/queue.el ends here
