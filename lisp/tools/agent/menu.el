;;; menu.el --- Discoverable actions for real agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; One keyboard menu for the manager and the original agent-shell buffer.
;; The scope captures an entity, never a name or a copied transcript.

;;; Code:
(require 'transient)
(require 'tools/agent/sessions)

(defvar +agent-manager--selected)
(defvar +agent-manager--model)
(declare-function +agent-manager-hide "tools/agent/manager" ())

(defun +agent-menu--buffer ()
  "Resolve the captured session again before every operation."
  (let ((buffer (or (transient-scope '+agent-session-menu)
                    (and (bound-and-true-p +agent-manager--model)
                         +agent-manager--selected)
                    (+agent-acp-command-buffer))))
    (unless (+agent-acp-agent-buffer-p buffer) (user-error "This session has ended"))
    buffer))

(defun +agent-menu--ready-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-session-live-p buffer)
         (eq (+agent-acp-state-value buffer '(:init-finished)) t)
         (not (+agent-acp-busy-p buffer)))))

(defun +agent-menu--review-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (memq (plist-get (+agent-session-snapshot buffer) :attention-kind) '(permission input))))

(defun +agent-menu--model-p ()
  (and (+agent-menu--ready-p) (+agent-acp-available-model-ids (+agent-menu--buffer))))

(defun +agent-menu--mode-p ()
  (and (+agent-menu--ready-p)
       (+agent-acp-state-value (+agent-menu--buffer) '(:available-modes :modes))))

(defun +agent-menu--busy-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer)))) (+agent-acp-busy-p buffer)))

(defun +agent-menu--history-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-menu--ready-p)
         (+agent-session-capability-p buffer :list)
         (or (+agent-session-capability-p buffer :load)
             (+agent-session-capability-p buffer :resume)))))

(defun +agent-menu--fork-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-menu--ready-p) (+agent-session-operation-supported-p 'fork buffer))))

(defun +agent-menu--queued-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (> (+agent-acp-pending-prompt-count buffer) 0)))

(defun +agent-menu--queue-p ()
  (and (+agent-menu--ready-p) (+agent-menu--queued-p)))

(defun +agent-menu--changes-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (buffer-local-value '+agent-acp--changes buffer)))

(defun +agent-menu--supported-p (operation)
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (+agent-session-operation-supported-p operation buffer)))

(defun +agent-menu--stop-group (buffer)
  "Stop the entire project pool through each session's execution owner."
  (let ((snapshot (+agent-session-snapshot buffer)))
    (+agent-pool-stop (plist-get snapshot :root) (plist-get snapshot :target))))

(defun +agent-menu--show (buffer)
  "Leave the navigation surface before entering native interactive controls."
  (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
  (+agent-acp-show-buffer buffer))

(defun +agent-menu--act (operation)
  "Apply OPERATION to the real scoped session through its public owner."
  (let ((buffer (+agent-menu--buffer)))
    (pcase operation
      ('stop (+agent-acp-stop buffer))
      ('stop-group (+agent-menu--stop-group buffer))
      ('rename (+agent-session-rename buffer))
      ('close (+agent-acp-close buffer))
      ('copy (with-current-buffer buffer (agent-shell-copy-last-output)))
      ('queue (+agent-queue-resume buffer))
      ('queue-panel (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
                    (+agent/queue buffer))
      ('changes (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
                (+agent-session-changes buffer))
      (_
       (+agent-menu--show buffer)
       (pcase operation
         ('open nil)
         ('input (+agent-acp-focus-input buffer))
         ('review (+agent-acp-review-request buffer))
         ('restart (+agent-session-restart buffer))
         ('fork (+agent-session-fork buffer))
         ('jump (+agent-session-jump buffer))
         ('archive (+agent-session-archive buffer))
         ('compact (+agent-session-compact buffer))
         (_ (with-current-buffer buffer
              (call-interactively
               (pcase operation
                 ('history #'+agent-shell/resume)
                 ('model #'agent-shell-set-session-model)
                 ('mode #'agent-shell-set-session-mode)
                 ('usage #'+agent-top-report)
                 ('fold #'agent-shell-ui-toggle-all-fragments)
                 ('previous #'agent-shell-previous-item)
                 ('next #'agent-shell-next-item)
                 ('help #'+agent-acp-help))))))))))

(autoload '+agent-top-report "tools/agent/top" nil t)

(dolist (operation '(open input review stop stop-group rename close restart fork history model mode queue queue-panel changes copy fold previous next help jump archive compact usage))
  (let ((action operation))
    (defalias (intern (format "+agent-menu-%s" action))
      (lambda () (interactive) (+agent-menu--act action)))))

;;;###autoload
(transient-define-prefix +agent-session-menu ()
  "Session actions; opening this menu never sends or approves a request."
  [:description (lambda () (format "Agent · %s" (+agent-acp--tab-label (+agent-menu--buffer))))
   ["Conversation"
    ("RET" "Open" +agent-menu-open)
    ("i" "Write" +agent-menu-input)
    ("a" "Review request" +agent-menu-review :if +agent-menu--review-p)
    ("h" "History" +agent-menu-history :if +agent-menu--history-p)
    ("j" "Next waiting session" +agent/next-attention)
    ("y" "Copy last reply" +agent-menu-copy)]
   ["Session"
    ("r" "Rename" +agent-menu-rename)
    ("m" "Model" +agent-menu-model :if +agent-menu--model-p)
    ("v" "Mode" +agent-menu-mode :if +agent-menu--mode-p)
    ("f" "Fork" +agent-menu-fork :if +agent-menu--fork-p)
    ("q" "Queue…" +agent-menu-queue-panel :if +agent-menu--queued-p)
    ("Q" "Continue queue" +agent-menu-queue :if +agent-menu--queue-p)]
   ["Lifecycle"
    ("s" "Stop turn" +agent-menu-stop :if +agent-menu--busy-p)
    ("S" "Stop project pool" +agent-menu-stop-group)
    ("R" "Reconnect / restart" +agent-menu-restart)
    ("x" "End session…" +agent-menu-close)
    ("M" "Manager" +agent/manager)
    ("?" "Key help" +agent-menu-help)]
   ["Inspect"
    ("D" "Detailed usage" +agent-menu-usage)
    ("l" "Changed files" +agent-menu-changes :if +agent-menu--changes-p)
    ("J" "Associated work" +agent-menu-jump :if (lambda () (+agent-menu--supported-p 'jump)))
    ("c" "Context rollover" +agent-menu-compact :if (lambda () (+agent-menu--supported-p 'compact)))
    ("A" "Archive Research…" +agent-menu-archive :if (lambda () (+agent-menu--supported-p 'archive)))]]
  (interactive)
  (transient-setup '+agent-session-menu nil nil :scope (+agent-menu--buffer)))

(provide 'tools/agent/menu)
;;; menu.el ends here
