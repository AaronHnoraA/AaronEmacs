;;; tools/agent/sessions.el --- Shared agent management and reversible preview -*- lexical-binding: t; -*-

;;; Commentary:
;; A view of the runtime's real buffers, shared by every caller.  Domain
;; actions are optional extensions; listing or editing a local session never
;; needs a Research host.  Preview follows the HQ interaction using Consult
;; and the existing window, without a second workspace or transcript copy.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'tools/agent/acp)

(defun +agent-session-live-p (buffer)
  "Whether BUFFER still owns a live provider process."
  (when (+agent-acp-agent-buffer-p buffer)
    (let ((process (+agent-acp-state-value buffer '(:client :process))))
      (and (processp process) (process-live-p process)))))

(cl-defun +agent-session-read (&key (prompt "Agent session: ") root live-only default copy-to-clipboard)
  "Choose an existing shared session; never resume or start one implicitly."
  (let* ((sessions (+agent-acp-sessions))
         (sessions (seq-remove (lambda (s) (run-hook-with-args-until-success '+agent-session-private-functions s)) sessions))
         (sessions (if live-only (seq-filter (lambda (s) (+agent-session-live-p (plist-get s :buffer))) sessions) sessions))
         (sessions (sort sessions (lambda (a b)
                                    (and (equal (plist-get a :root) root) (not (equal (plist-get b :root) root))))))
         (choices (mapcar (lambda (s)
                            (cons (format "%s · %s · %s" (or (plist-get s :name) (buffer-name (plist-get s :buffer)))
                                          (plist-get s :agent) (plist-get s :root)) (plist-get s :buffer))) sessions)))
    (when copy-to-clipboard (setq choices (append choices '(("Copy to clipboard" . clipboard)))))
    (unless choices (user-error "No live agent session; start or resume one explicitly"))
    (cdr (assoc (completing-read prompt choices nil t nil nil (car (rassq default choices))) choices))))

(defvar-local +agent-sessions--root nil)
(defvar-local +agent-sessions--attention-only nil)
(defvar-local +agent-sessions--timer nil)

(defun +agent-session--dispatch (operation buffer &rest args)
  "Offer OPERATION on BUFFER with ARGS to optional domain adapters."
  (apply #'run-hook-with-args-until-success
         '+agent-session-action-functions operation buffer args))

(defun +agent-session-restart (&optional buffer)
  "Restart BUFFER through its domain adapter or native agent-shell."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (unless (+agent-session--dispatch 'restart buffer)
      (when (+agent-acp-confirm-stop buffer "restart")
        (with-current-buffer buffer (agent-shell-restart))))))

(defun +agent-session-rename (&optional buffer name)
  "Rename BUFFER to NAME without detaching shell-maker's process."
  (interactive)
  (let* ((buffer (+agent-acp-command-buffer buffer))
         (name (or name (read-string "Session name: "
                                    (buffer-local-value '+agent-acp-session-name buffer)))))
    (when (string-empty-p (string-trim name)) (user-error "Session name is empty"))
    (unless (+agent-session--dispatch 'rename buffer name)
      (let ((other (+agent-acp-session-buffer
                    name (buffer-local-value '+agent-acp-session-root buffer))))
        (when (and other (not (eq other buffer))) (user-error "Session name already exists: %s" name)))
      (+agent-acp-mark-session-buffer
       buffer name (buffer-local-value '+agent-acp-session-agent buffer)
       (buffer-local-value '+agent-acp-session-root buffer))
      (with-current-buffer buffer (+agent-acp--changed)))))

(defun +agent-session-fork (&optional buffer name)
  "Fork BUFFER through its domain adapter or the provider's real capability."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (unless (+agent-session--dispatch 'fork buffer name)
      (unless (+agent-session-capability-p buffer :fork)
        (user-error "This agent does not report native session fork support"))
      (with-current-buffer buffer (call-interactively #'agent-shell-fork)))))

(defun +agent-session-side-chat (&optional buffer)
  "Start a temporary independent conversation beside BUFFER."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (unless (+agent-session--dispatch 'side-chat buffer)
      (let* ((config (+agent-acp-state-value buffer '(:agent-config)))
             (directory (buffer-local-value 'default-directory buffer))
             (side (+agent-acp-start :config config :directory directory :origin 'side :focus t)))
        (+agent-acp-adopt side :origin 'side
                          :root (buffer-local-value '+agent-acp-session-root buffer))))))

(defun +agent-session-compact (&optional buffer)
  "Request the owning workflow's supported context rollover for BUFFER."
  (interactive)
  (unless (+agent-session--dispatch 'compact (+agent-acp-command-buffer buffer))
    (user-error "No context rollover operation is reported for this session")))

(defun +agent-session-jump (&optional buffer)
  "Visit BUFFER's associated work through its optional domain adapter."
  (interactive)
  (unless (+agent-session--dispatch 'jump (+agent-acp-command-buffer buffer))
    (user-error "This session has no associated work document")))

(defun +agent-session-archive (&optional buffer)
  "Archive BUFFER's domain record when its owner supports that operation."
  (interactive)
  (unless (+agent-session--dispatch 'archive (+agent-acp-command-buffer buffer))
    (user-error "This session has no durable domain record to archive")))

(defun +agent-session-list (&optional buffer)
  "List the shared sessions of BUFFER's project."
  (interactive)
  (let ((default-directory
         (if buffer (buffer-local-value '+agent-acp-session-root buffer) default-directory)))
    (+agent/sessions t)))

(defun +agent-sessions--state (session)
  "Describe SESSION's live execution and attention separately."
  (let* ((buffer (plist-get session :buffer))
         (snapshot (+agent-session-snapshot buffer))
         (process (+agent-acp-state-value buffer '(:client :process))))
    (cond ((and (processp process) (not (process-live-p process))) "dead")
          ((eq (plist-get snapshot :attention-kind) 'permission) "approval")
          ((eq (plist-get snapshot :attention-kind) 'input) "answer")
          ((eq (plist-get snapshot :attention-kind) 'failed) "failed")
          ((plist-get session :busy) "running")
          ((plist-get snapshot :last-error) "failed")
          ((not process) "starting")
          (t "idle"))))

(defun +agent-sessions-refresh ()
  "Render the shared in-memory registry without querying a domain service."
  (interactive)
  (let* ((selected (tabulated-list-get-id))
         (sessions (+agent-acp-sessions +agent-sessions--root)))
    (setq tabulated-list-entries
          (mapcar
           (lambda (session)
             (let* ((buffer (plist-get session :buffer))
                    (usage (+agent-acp-usage buffer))
                    (root (plist-get session :root)))
               (list buffer
                     (vector (+agent-sessions--state session)
                             (or (and (plist-get session :unread-kind)
                                      (symbol-name (plist-get session :unread-kind))) "-")
                             (or (plist-get session :name) (buffer-name buffer))
                             (format "%s" (or (plist-get session :agent) "-"))
                             (or (plist-get usage :model) "-")
                             (abbreviate-file-name (or root ""))))))
           (if +agent-sessions--attention-only
               (seq-filter (lambda (session)
                             (let ((buffer (plist-get session :buffer)))
                               (or (plist-get (+agent-session-snapshot buffer) :attention-kind)
                                   (plist-get session :unread-kind)
                                   (member (+agent-sessions--state session) '("dead" "failed"))))) sessions)
             sessions)))
    (tabulated-list-print t)
    (when selected
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eq (tabulated-list-get-id) selected))) (forward-line 1)))))

(defun +agent-sessions--selected ()
  "Return the session buffer on this row."
  (let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer) (user-error "Choose a live session row")) buffer))

(defun +agent-sessions-visit () (interactive) (+agent-acp-show-buffer (+agent-sessions--selected)))
(defun +agent-sessions-stop () (interactive) (+agent-acp-stop (+agent-sessions--selected)))
(defun +agent-sessions-close () (interactive) (+agent-acp-close (+agent-sessions--selected)))
(defun +agent-sessions-rename () (interactive) (+agent-session-rename (+agent-sessions--selected)))
(defun +agent-sessions-restart () (interactive) (+agent-session-restart (+agent-sessions--selected)))
(defun +agent-sessions-fork () (interactive) (+agent-session-fork (+agent-sessions--selected)))

(defun +agent-sessions--stop-timer ()
  (when (timerp +agent-sessions--timer) (cancel-timer +agent-sessions--timer))
  (setq +agent-sessions--timer nil))

(defun +agent-sessions--changed (_source)
  "Coalesce registry events only while the native list is visible."
  (when-let* ((buffer (get-buffer "*Agent Sessions*"))
              ((get-buffer-window buffer 'visible)))
    (with-current-buffer buffer
      (unless (timerp +agent-sessions--timer)
        (setq +agent-sessions--timer
              (run-at-time 0.2 nil
                           (lambda ()
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (setq +agent-sessions--timer nil)
                                 (when (get-buffer-window buffer 'visible)
                                   (+agent-sessions-refresh)))))))))))

(define-derived-mode +agent-sessions-mode tabulated-list-mode "Agents"
  "All live agent sessions; actions operate on the original buffers."
  (setq tabulated-list-format [("State" 10 t) ("Unread" 10 t) ("Session" 26 t) ("Agent" 12 t) ("Model" 24 t) ("Workspace" 0 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'+agent-sessions--stop-timer nil t))

(dolist (binding '(("RET" . +agent-sessions-visit) ("g" . +agent-sessions-refresh)
                   ("s" . +agent-sessions-stop) ("k" . +agent-sessions-close)
                   ("r" . +agent-sessions-rename) ("R" . +agent-sessions-restart)
                   ("f" . +agent-sessions-fork)))
  (define-key +agent-sessions-mode-map (kbd (car binding)) (cdr binding)))

(with-eval-after-load 'evil
  (evil-set-initial-state '+agent-sessions-mode 'emacs))

(add-hook '+agent-acp-changed-functions #'+agent-sessions--changed)

;;;###autoload
(defun +agent/sessions (&optional project)
  "List all agent sessions, or only the current PROJECT with a prefix."
  (interactive "P")
  (let ((root (and project (+agent-acp-project-root))))
    (with-current-buffer (get-buffer-create "*Agent Sessions*")
      (unless (derived-mode-p '+agent-sessions-mode) (+agent-sessions-mode))
      (setq +agent-sessions--root root +agent-sessions--attention-only nil)
      (+agent-sessions-refresh)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun +agent/inbox ()
  "List live sessions with unread attention or a dead connection."
  (interactive)
  (+agent/sessions)
  (setq +agent-sessions--attention-only t)
  (+agent-sessions-refresh))

;;;###autoload
(defun +agent/peek ()
  "Preview live agent buffers, restoring the original window before returning."
  (interactive)
  (let* ((sessions (+agent-acp-sessions))
         (choices (mapcar (lambda (session)
                            (cons (format "%s · %s · %s"
                                          (or (plist-get session :name) (buffer-name (plist-get session :buffer)))
                                          (plist-get session :agent) (plist-get session :root))
                                  (plist-get session :buffer))) sessions))
         (window (selected-window)) (original (window-buffer window))
         (point (window-point window)) (start (window-start window))
         (+agent-acp-previewing t) selected)
    (unless choices (user-error "No live agent sessions"))
    (unwind-protect
        (setq selected
              (if (require 'consult nil t)
                  (consult--read
                   (mapcar #'car choices) :prompt "Agent session: " :require-match t :sort nil
                   :state (lambda (action candidate)
                            (when (and (eq action 'preview) candidate (window-live-p window))
                              (when-let* ((buffer (cdr (assoc candidate choices))) ((buffer-live-p buffer)))
                                (set-window-buffer window buffer)
                                (+agent-render-flush buffer)))))
                (completing-read "Agent session: " choices nil t)))
      (when (and (window-live-p window) (buffer-live-p original))
        (set-window-buffer window original)
        (set-window-point window point)
        (set-window-start window start t)))
    (when-let* ((buffer (cdr (assoc selected choices))))
      (+agent-acp-show-buffer buffer))))

(defalias 'agent-sessions #'+agent/sessions)
(defalias 'agent-peek #'+agent/peek)
(defalias 'agent-inbox #'+agent/inbox)

(provide 'tools/agent/sessions)
;;; tools/agent/sessions.el ends here
