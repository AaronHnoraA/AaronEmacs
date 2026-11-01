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

(defun +agent/pool (&optional choose)
  "Recall this project's agent pool, or return from its current chat view.
With prefix CHOOSE select a pool member.  An empty pool never starts an
agent until the user explicitly chooses creation."
  (interactive "P")
  (if (and (not choose) (+agent-acp-agent-buffer-p (current-buffer)))
      (+agent-acp-hide)
    (let* ((root (or +agent-acp-session-root (+agent-acp-project-root)))
           (sessions (sort (+agent-pool-sessions root)
                           (lambda (a b) (> (or (plist-get a :last-used) 0)
                                             (or (plist-get b :last-used) 0)))))
           (buffer (and (not choose) (plist-get (car sessions) :buffer))))
      (if buffer
          (+agent-acp-show-buffer buffer)
        (let* ((choices
                (append (mapcar (lambda (session)
                                  (let ((buffer (plist-get session :buffer)))
                                    (cons (format "%s · %s [%s]"
                                                  (plist-get session :agent)
                                                  (buffer-name buffer)
                                                  (plist-get session :process-state))
                                          buffer)))
                                sessions)
                        '(("＋ New agent session" . :new)
                          ("↺ Resume saved conversation" . :resume))))
               (choice (cdr (assoc (completing-read "Project agents: " choices nil t)
                                   choices))))
          (cond ((bufferp choice) (+agent-acp-show-buffer choice))
                ((eq choice :new) (call-interactively #'+agent/start))
                ((eq choice :resume) (call-interactively #'+agent/resume-saved))
                (t (user-error "No agent session selected"))))))))

(defun +agent/pool-switch ()
  "Select an existing member of the current project's agent pool."
  (interactive)
  (+agent/pool t))

(defun +agent-pool-stop (root &optional target)
  "Stop active work in ROOT's pool through each session's execution owner."
  (let ((count 0))
    (dolist (session (+agent-pool-sessions root target) count)
      (when (plist-get session :busy)
        (+agent-acp-stop (plist-get session :buffer))
        (cl-incf count)))))

(defun +agent-session-live-p (buffer)
  "Whether BUFFER still owns a live provider process."
  (when (+agent-acp-agent-buffer-p buffer)
    (let ((process (+agent-acp-state-value buffer '(:client :process))))
      (and (processp process) (process-live-p process)))))

(cl-defun +agent-session-read (&key (prompt "Agent session: ") root live-only default copy-to-clipboard)
  "Choose an existing shared session; never resume or start one implicitly."
  (let* ((sessions (+agent-pool-sessions))
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
      (+agent-acp-restart-native buffer))))

(defun +agent-session-rename (&optional buffer name)
  "Rename BUFFER to NAME without detaching shell-maker's process."
  (interactive)
  (let* ((buffer (+agent-acp-command-buffer buffer))
         (name (or name (read-string "Session name: "
                                    (buffer-local-value '+agent-acp-session-name buffer)))))
    (when (string-empty-p (string-trim name)) (user-error "Session name is empty"))
    (unless (+agent-session--dispatch 'rename buffer name)
      (let ((other (with-current-buffer buffer
                     (+agent-acp-session-buffer name +agent-acp-session-root))))
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
  (with-current-buffer (or buffer (current-buffer)) (+agent/sessions t)))

(defun +agent-session-attention-rank (session)
  "Return how urgently SESSION needs the person, lower first, or nil."
  (let ((kind (plist-get session :attention-kind)))
    (cond ((eq kind 'permission) 0)
          ((eq kind 'input) 1)
          ((eq kind 'failed) 2)
          ;; A domain adapter may report a reason of its own.
          (kind 3)
          ((memq (plist-get session :process-state) '(dead disconnected)) 4)
          ((plist-get session :unread-kind) 5))))

(defun +agent-session-attention-queue (sessions)
  "Return those of SESSIONS needing the person, most urgent first."
  (sort (seq-filter #'+agent-session-attention-rank sessions)
        (lambda (a b)
          (let ((rank-a (+agent-session-attention-rank a))
                (rank-b (+agent-session-attention-rank b)))
            ;; Not by recency: visiting one must not reorder the rest.
            (if (= rank-a rank-b)
                (string-lessp (buffer-name (plist-get a :buffer))
                              (buffer-name (plist-get b :buffer)))
              (< rank-a rank-b))))))

;;;###autoload
(defun +agent/next-attention (&optional project)
  "Go to the next session waiting on you, cycling across projects.
With prefix PROJECT stay in this project's pool.  A pending approval or
question lands on its native control; nothing is decided for you."
  (interactive "P")
  (let* ((root (and project (or +agent-acp-session-root (+agent-acp-project-root))))
         (waiting (+agent-session-attention-queue (+agent-pool-sessions root)))
         (buffers (mapcar (lambda (session) (plist-get session :buffer)) waiting))
         (index (seq-position buffers (current-buffer)))
         (session (and waiting (nth (mod (1+ (or index -1)) (length waiting)) waiting))))
    (unless session (user-error "No agent session needs attention"))
    (if (memq (plist-get session :attention-kind) '(permission input))
        (+agent-acp-review-request (plist-get session :buffer))
      (+agent-acp-show-buffer (plist-get session :buffer)))))

(defun +agent-session-error-text (session)
  "Return SESSION's last error as one line of text, or nil."
  (let ((value (plist-get session :last-error)))
    (cond ((stringp value) value)
          ((and (consp value) (stringp (map-elt value :message))) (map-elt value :message))
          (value "The session reported an error"))))

(defun +agent-session-duration (seconds)
  "Format SECONDS of running time compactly."
  (let ((seconds (max 0 (floor seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          (t (format "%dh%02dm" (/ seconds 3600) (/ (% seconds 3600) 60))))))

(defun +agent-session-progress (session)
  "Return SESSION's elapsed turn and queue as a list of short phrases."
  (let ((started (plist-get session :turn-started-at))
        (queued (or (plist-get session :queued) 0))
        (held (or (plist-get session :queue-held) 0)))
    (delq nil
          (list (and started (plist-get session :busy)
                     (+agent-session-duration (- (float-time) started)))
                (and (> queued 0)
                     (format "%d queued%s" queued
                             (if (> held 0) (format " (%d held)" held) "")))))))

(defun +agent-session-changes (&optional buffer)
  "Choose a file BUFFER's agent reported changing and open it."
  (interactive)
  (let* ((buffer (+agent-acp-command-buffer buffer))
         (changes (buffer-local-value '+agent-acp--changes buffer))
         (now (float-time))
         (choices (mapcar (lambda (change)
                            (cons (format "%s  ×%d · %s ago"
                                          (abbreviate-file-name (car change))
                                          (plist-get (cdr change) :count)
                                          (+agent-session-duration
                                           (- now (plist-get (cdr change) :at))))
                                  (car change)))
                          changes)))
    (unless choices (user-error "This session reported no file changes"))
    (let* ((choice (let ((completion-extra-properties '(:display-sort-function identity)))
                     (completing-read "Changed file: " choices nil t)))
           (file (with-current-buffer buffer
                   (agent-shell--resolve-path (cdr (assoc choice choices))))))
      (if (+agent-acp-agent-buffer-p (window-buffer (selected-window)))
          (find-file-other-window file)
        (find-file file)))))

;;;###autoload
(defun +agent/changes (&optional buffer)
  "Open a file changed by this agent session."
  (interactive)
  (+agent-session-changes buffer))

(defun +agent-sessions--state (session)
  "Describe SESSION's live execution and attention separately."
  (let ((process (plist-get session :process-state))
        (turn (plist-get session :turn-state)))
    (cond ((memq process '(dead disconnected)) (symbol-name process))
          ((memq turn '(stopping stop-unconfirmed)) (symbol-name turn))
          ((eq (plist-get session :attention-kind) 'permission) "approval")
          ((eq (plist-get session :attention-kind) 'input) "answer")
          ((eq (plist-get session :attention-kind) 'failed) "failed")
          ((eq turn 'starting) "starting")
          ((plist-get session :busy) "running")
          ((plist-get session :last-error) "failed")
          ((eq process 'starting) "starting")
          (t "idle"))))

(defun +agent-sessions-refresh ()
  "Render the shared in-memory registry without querying a domain service."
  (interactive)
  (let* ((selected (tabulated-list-get-id))
         (sessions (+agent-pool-sessions +agent-sessions--root)))
    (setq tabulated-list-entries
          (mapcar
           (lambda (session)
             (let* ((buffer (plist-get session :buffer))
                    (usage (plist-get session :usage))
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
                             (or (plist-get session :attention-kind)
                                 (plist-get session :unread-kind)
                                 (member (+agent-sessions--state session) '("dead" "failed"))))
                           sessions)
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
(defun +agent/sessions-table (&optional project attention)
  "Diagnostic/TTY list, optionally restricted to PROJECT or ATTENTION."
  (interactive "P")
  (let ((root (and project (or +agent-acp-session-root (+agent-acp-project-root))))
        (directory default-directory))
    (with-current-buffer (get-buffer-create "*Agent Sessions*")
      (unless (derived-mode-p '+agent-sessions-mode) (+agent-sessions-mode))
      (setq default-directory directory)
      (setq +agent-sessions--root root +agent-sessions--attention-only attention)
      (+agent-sessions-refresh)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun +agent/sessions (&optional project)
  "Open the shared sessions UI, restricted to PROJECT with a prefix."
  (interactive "P")
  (require 'tools/agent/manager)
  (+agent-manager-show 'sessions project))

;;;###autoload
(defun +agent/inbox ()
  "List live sessions with unread attention or a dead connection."
  (interactive)
  (require 'tools/agent/manager)
  (+agent-manager-show 'attention))

;;;###autoload
(defun +agent/peek ()
  "Preview live agent buffers, restoring the original window before returning."
  (interactive)
  (let* ((sessions (+agent-pool-sessions))
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
