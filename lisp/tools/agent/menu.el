;;; menu.el --- Discoverable actions for real agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; One keyboard menu for the manager and the original agent-shell buffer.
;; The scope captures an entity, never a name or a copied transcript.

;;; Code:
(require 'transient)
(require 'tools/agent/actions)
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

(defun +agent-menu--scope ()
  "Return the session in question, or nil where none exists yet."
  (ignore-errors (+agent-menu--buffer)))

(defun +agent-menu--title ()
  "Name the menu after its session."
  (if-let* ((buffer (+agent-menu--scope)))
      (format "Agent · %s" (+agent-acp--tab-label buffer))
    "Agent"))

(defun +agent-menu--session-p ()
  (and (+agent-menu--scope) t))

(defun +agent-menu--jump-p () (+agent-menu--supported-p 'jump))
(defun +agent-menu--compact-p () (+agent-menu--supported-p 'compact))
(defun +agent-menu--archive-p () (+agent-menu--supported-p 'archive))

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

(defun +agent-menu--fork-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-menu--ready-p) (+agent-session-operation-supported-p 'fork buffer))))

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

(autoload '+agent/resume-saved "tools/agent/acp" nil t)

(defun +agent-menu-new ()
  "Create a conversation through its management owner, or an execution agent."
  (interactive)
  (let ((buffer (or (transient-scope '+agent-session-menu) (current-buffer))))
    (unless (run-hook-with-args-until-success '+agent-session-action-functions 'new buffer)
      (call-interactively #'+agent/start))))
(autoload '+agent/queue "tools/agent/queue" nil t)

(defun +agent-menu--show (buffer)
  "Leave the navigation surface before entering native interactive controls."
  (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
  (+agent-acp-show-buffer buffer))

(defun +agent-menu-history ()
  "Pick a saved conversation, in this session when it can list them."
  (interactive)
  (let ((buffer (+agent-menu--scope)))
    (cond
     ((and buffer (eq (+agent-session-kind buffer) 'pi))
      (with-current-buffer buffer (call-interactively #'+agent-acp-session-manager)))
     ((and buffer (+agent-menu--ready-p)
             (+agent-session-capability-p buffer :list)
             (or (+agent-session-capability-p buffer :load)
                 (+agent-session-capability-p buffer :resume)))
        (progn (+agent-menu--show buffer)
               (with-current-buffer buffer (call-interactively #'+agent-shell/resume))))
     (t
      (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
      (call-interactively #'+agent/resume-saved)))))

(defun +agent-menu-next (&optional project)
  "Go to the next waiting session, from any surface; PROJECT limits the pool."
  (interactive "P")
  (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
  (+agent/next-attention project))

(defun +agent-menu--act (operation)
  "Apply OPERATION to the real scoped session through its public owner."
  (let ((buffer (+agent-menu--buffer)))
    (pcase operation
      ('stop (+agent-acp-stop buffer))
      ('stop-group (+agent-menu--stop-group buffer))
      ('rename (+agent-session-rename buffer))
      ('close (+agent-acp-close buffer))
      ('copy (with-current-buffer buffer (agent-shell-copy-last-output)))
      ((or 'queue 'changes 'tree 'side-chat 'close-others)
       (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
       (pcase operation
         ('queue (+agent/queue buffer))
         ('changes (+agent-session-changes buffer))
         ('tree (+agent-acp-conversation-tree buffer))
         ('side-chat (+agent-session-side-chat buffer))
         ('close-others (+agent-acp-close-others buffer))))
      (_
       (+agent-menu--show buffer)
       (pcase operation
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
                 ('model #'agent-shell-set-session-model)
                 ('mode #'agent-shell-set-session-mode)
                 ('usage #'+agent-top-report))))))))))

(autoload '+agent-top-report "tools/agent/top" nil t)

(dolist (operation '(input review stop stop-group rename close restart fork model mode queue changes tree side-chat close-others copy jump archive compact usage))
  (let ((action operation))
    (defalias (intern (format "+agent-menu-%s" action))
      (lambda () (interactive) (+agent-menu--act action)))))

;; The layout is generated: `tools/agent/actions' is the only list of actions.
(eval-and-compile
  (defun +agent-menu--suffix (row)
    "Return the Transient suffix of action ROW."
    (pcase-let ((`(,key ,label ,command . ,properties) row))
      (append (list key label command)
              (when-let* ((predicate (plist-get properties :if))) (list :if predicate))))))

(defmacro +agent-menu--define (name doc table &rest extra)
  "Define the Transient prefix NAME with DOC from action TABLE and EXTRA suffixes."
  `(transient-define-prefix ,name ()
     ,doc
     [:description +agent-menu--title
      ,@(mapcar (lambda (group)
                  (vconcat (list (car group))
                           (mapcar #'+agent-menu--suffix (cdr group))))
                (symbol-value table))
      ,@extra]
     (interactive)
     (let ((buffer (+agent-menu--scope)))
       (if (and (eq ',name '+agent-session-menu) buffer (eq (+agent-session-kind buffer) 'pi))
           (with-current-buffer buffer (call-interactively #'+agent-acp-session-manager))
         (transient-setup ',name nil nil :scope buffer)))))

(+agent-menu--define +agent-send-menu "Hand material to an agent." +agent-send-actions)

;;;###autoload (autoload '+agent-session-menu "tools/agent/menu" nil t)
(+agent-menu--define +agent-session-menu
  "Every agent action; opening this menu never sends or approves a request."
  +agent-actions
  ["Send" ("c" "Send…" +agent-send-menu)])

(provide 'tools/agent/menu)
;;; menu.el ends here
