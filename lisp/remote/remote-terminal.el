;;; remote-terminal.el --- Workspace-scoped routed terminals -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remote terminals are workspace-owned PTY processes backed by the routed
;; process API.  The built-in frontend is comint; native terminal frontends
;; such as vterm can keep their own process/filter implementation and register
;; the resulting buffer through `remote-terminal-adopt'.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'seq)
(require 'remote-core)
(require 'remote-process)
(require 'remote-path)
(require 'remote-workspace)

(cl-defstruct (remote-terminal-profile
               (:constructor remote-terminal-profile-create))
  id shell arguments environment predicate)

(cl-defstruct (remote-terminal
               (:constructor remote-terminal-create))
  id name workspace-id profile-name buffer process state opened-at
  error metadata)

(defvar remote-terminal-profiles (make-hash-table :test #'equal)
  "Registered remote terminal profiles.")

(defvar remote-terminals (make-hash-table :test #'equal)
  "Open remote terminals keyed by ID.")

(defvar-local remote-terminal-instance nil
  "Terminal object owned by the current buffer.")

(defun remote-terminal-put-metadata (terminal property value)
  "Set TERMINAL metadata PROPERTY to VALUE and return TERMINAL."
  (unless (remote-terminal-p terminal)
    (signal 'wrong-type-argument
            (list 'remote-terminal-p terminal)))
  (setf
   (remote-terminal-metadata terminal)
   (plist-put
    (remote-terminal-metadata terminal)
    property value))
  terminal)

(cl-defun remote-register-terminal-profile
    (id &key shell arguments environment predicate)
  "Register terminal profile ID."
  (let* ((id (remote-normalize-id id))
         (profile
          (remote-terminal-profile-create
           :id id
           :shell shell
           :arguments (copy-sequence arguments)
           :environment (copy-tree environment)
           :predicate predicate)))
    (puthash id profile remote-terminal-profiles)
    profile))

(defun remote-get-terminal-profile (id)
  "Return terminal profile ID, or nil."
  (gethash (remote-normalize-id id t) remote-terminal-profiles))

(defun remote-terminal--profile (workspace profile)
  "Resolve PROFILE for WORKSPACE."
  (if profile
      (if (remote-terminal-profile-p profile)
          profile
        (or (remote-get-terminal-profile profile)
            (error "Unknown remote terminal profile: %S" profile)))
    (or
     (seq-find
      (lambda (candidate)
        (if-let* ((predicate
                   (remote-terminal-profile-predicate candidate)))
            (funcall predicate workspace)
          t))
      (hash-table-values remote-terminal-profiles))
     (error "No remote terminal profile is available"))))

(defun remote-terminal--nonempty-shell (value)
  "Return normalized shell VALUE, or nil when it is unusable."
  (when (stringp value)
    (let ((value (string-trim value)))
      (and (not (string-empty-p value)) value))))

(defun remote-terminal--fallback-shell (context)
  "Return the first target executable among zsh, bash, and sh for CONTEXT."
  (let ((remote-environment-inhibit t))
    (seq-some
     (lambda (program)
       (ignore-errors
         (remote-executable-find program context)))
     '("zsh" "bash" "sh"))))

(defun remote-terminal--shell (workspace profile &optional probe)
  "Return target-native shell for WORKSPACE and PROFILE.
When PROBE is non-nil, resolve and cache the target login shell before falling
back to `/bin/sh'.  Probe failures do not prevent terminal startup."
  (let* ((target-id (remote-workspace-target-id workspace))
         (target (remote-get-target target-id))
         (context (remote-workspace-context workspace))
         (facts
          (when probe
            (condition-case error-data
                (remote-path-probe context)
              (error
               (remote-log
                'terminal-shell-probe-error
                :target target-id
                :error (error-message-string error-data))
               nil))))
         (environment (remote-workspace-environment workspace)))
    (or
     (remote-terminal--nonempty-shell
      (remote-terminal-profile-shell profile))
     (remote-terminal--nonempty-shell
      (and target (remote-target-shell target)))
     (remote-terminal--nonempty-shell
     (and facts (remote-path-facts-shell facts)))
     (remote-terminal--nonempty-shell
      (and environment
           (cdr
            (assoc-string
             "SHELL"
             (remote-environment-vars environment)
             t))))
     (and probe
          (remote-terminal--nonempty-shell
           (remote-terminal--fallback-shell context)))
     "/bin/sh")))

(defun remote-terminal-command (&optional workspace profile probe)
  "Return the target-native interactive shell command for WORKSPACE.
PROFILE is a registered terminal profile or profile object.  The result is a
list suitable for routed process APIs and native terminal frontends.  With
PROBE, discover and cache the target account's login shell first."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (remote-workspace-open workspace
                                     :adapter "process"
                                     :capability 'pty
                                     :load-environment t)))
         (profile (remote-terminal--profile workspace profile)))
    (cons (remote-terminal--shell workspace profile probe)
          (copy-sequence
           (remote-terminal-profile-arguments profile)))))

(defun remote-terminal--detach (terminal)
  "Detach TERMINAL from registries and its owning workspace."
  (setf (remote-terminal-state terminal) 'closed)
  (remhash (remote-terminal-id terminal) remote-terminals)
  (when-let* ((workspace
               (remote-get-workspace
                (remote-terminal-workspace-id terminal))))
    (setf
     (remote-workspace-resources workspace)
     (seq-remove
      (lambda (resource)
        (eq (remote-workspace-resource-value resource) terminal))
      (remote-workspace-resources workspace))))
  terminal)

(defun remote-terminal--process-finished (terminal)
  "Record TERMINAL as closed after its frontend process finishes."
  (when (remote-terminal-p terminal)
    (if (eq (remote-terminal-state terminal) 'disconnected)
        (setf (remote-terminal-process terminal) nil)
      (remote-terminal--detach terminal))))

(defun remote-terminal--buffer-killed ()
  "Close the terminal process owned by the current buffer."
  (when (and remote-terminal-instance
             (remote-terminal-p remote-terminal-instance))
    (let ((process
           (remote-terminal-process remote-terminal-instance)))
      (when (and (processp process)
                 (process-live-p process))
        (delete-process process)))
    (remote-terminal--detach remote-terminal-instance)))

(cl-defun remote-terminal-adopt
    (workspace buffer
               &key process name profile metadata)
  "Adopt a native terminal frontend BUFFER into WORKSPACE.
PROCESS defaults to BUFFER's process.  NAME and PROFILE are descriptive;
METADATA can record the frontend and consumer.  The frontend keeps its native
mode, process filter, and sentinel behavior while the framework owns process,
buffer, and workspace teardown."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (error "Unknown remote workspace: %S" workspace)))
         (buffer
          (or (and (buffer-live-p buffer) buffer)
              (error "Terminal frontend returned a dead buffer")))
         (process (or process (get-buffer-process buffer)))
         (profile
          (and profile
               (if (remote-terminal-profile-p profile)
                   profile
                 (or (remote-get-terminal-profile profile)
                     (error "Unknown remote terminal profile: %S"
                            profile)))))
         (name (or name (buffer-name buffer)))
         (id
          (format "%s/terminal-%s"
                  (remote-workspace-id workspace)
                  (substring
                   (secure-hash
                    'sha1
                    (format "%s:%s:%s"
                            name (float-time) (random)))
                   0 10)))
         (terminal
          (remote-terminal-create
           :id id
           :name name
           :workspace-id (remote-workspace-id workspace)
           :profile-name
           (and profile (remote-terminal-profile-id profile))
           :buffer buffer
           :process process
           :state 'open
           :opened-at (current-time)
           :metadata metadata)))
    (unless (and (processp process) (process-live-p process))
      (error "Terminal frontend has no live process: %S" buffer))
    ;; The workspace owns teardown.  Killing a terminal buffer must not leave
    ;; Emacs waiting on the generic live-process query before our hook runs.
    (set-process-query-on-exit-flag process nil)
    (when-let* ((existing
                 (buffer-local-value 'remote-terminal-instance buffer)))
      (if (and (remote-terminal-p existing)
               (eq (remote-terminal-process existing) process)
               (equal (remote-terminal-workspace-id existing)
                      (remote-workspace-id workspace)))
          (cl-return-from remote-terminal-adopt existing)
        (error "Terminal buffer %s already belongs to workspace %s"
               (buffer-name buffer)
               (and (remote-terminal-p existing)
                    (remote-terminal-workspace-id existing)))))
    (let ((frontend-sentinel (process-sentinel process)))
      (set-process-sentinel
       process
       (lambda (finished event)
         (unwind-protect
             (when frontend-sentinel
               (funcall frontend-sentinel finished event))
           (when (memq (process-status finished)
                       '(exit signal failed closed))
             (remote-terminal--process-finished terminal))))))
    (with-current-buffer buffer
      (setq-local remote-terminal-instance terminal)
      (add-hook 'kill-buffer-hook
                #'remote-terminal--buffer-killed nil t))
    (process-put process 'remote-terminal terminal)
    (puthash id terminal remote-terminals)
    (remote-workspace-register-resource
     workspace 'terminal terminal
     (lambda (value _reason)
       (remote-terminal-close value))
     metadata)
    terminal))

(defun remote-terminal-mark-disconnected (terminal &optional reason)
  "Keep TERMINAL visible but stop it after transport loss.
REASON is retained for UI and Doctor output.  The shell is not replayed."
  (when (and (remote-terminal-p terminal)
             (not (eq (remote-terminal-state terminal) 'closed)))
    (setf (remote-terminal-state terminal) 'disconnected
          (remote-terminal-error terminal) reason)
    (when-let* ((buffer (remote-terminal-buffer terminal)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert
             "\n[remote] transport disconnected; use "
             "M-x remote-terminal-restart to start a new shell.\n")))))
    (when-let* ((process (remote-terminal-process terminal)))
      (when (process-live-p process)
        (delete-process process)))
    terminal))

(cl-defun remote-terminal-open
    (&optional workspace
               &key name profile shell arguments environment display)
  "Open a routed terminal for WORKSPACE.
PROFILE supplies shell, arguments, and environment defaults.  SHELL,
ARGUMENTS, and ENVIRONMENT override the profile.  With DISPLAY, show the
terminal buffer."
  (let* ((workspace
         (or (remote-get-workspace workspace)
              (remote-workspace-open workspace
                                     :adapter "process"
                                     :capability 'pty
                                     :load-environment t)))
         (_environment
          (or (remote-workspace-environment workspace)
              (remote-workspace-refresh-environment workspace)))
         (profile (remote-terminal--profile workspace profile))
         (command (remote-terminal-command workspace profile))
         (shell (or shell (car command)))
         (arguments (or arguments (cdr command)))
         (environment
          (append
           (remote-terminal-profile-environment profile)
           environment))
         (name
          (or name
              (format "%s:%s"
                      (remote-workspace-target-id workspace)
                      (remote-workspace-workspace-id workspace))))
         (buffer
          (generate-new-buffer
           (format "*remote-terminal:%s*" name)))
         terminal process)
    (condition-case error
        (progn
          (with-current-buffer buffer
            (setq default-directory
                  (remote-workspace-root workspace))
            (comint-mode)
            (setq process
                  (remote-make-process
                   :name (format "remote-terminal-%s" name)
                   :buffer buffer
                   :command (cons shell arguments)
                   :connection-type 'pty
                   :coding 'utf-8-unix
                   :noquery t
                   :filter #'comint-output-filter
                   :remote-adapter "process"
                   :remote-context
                   (remote-workspace-context workspace)
                   :remote-environment environment
                   :sentinel #'internal-default-process-sentinel)))
          (setq terminal
                (remote-terminal-adopt
                 workspace buffer
                 :process process
                 :name name
                 :profile profile
                 :metadata
                 (list
                  :frontend 'comint
                  :restart
                  (list
                   :name name
                   :profile (remote-terminal-profile-id profile)
                   :shell shell
                   :arguments arguments
                   :environment environment
                   :display display))))
          (when display
            (pop-to-buffer buffer))
          terminal)
      (error
       (when (processp process)
         (when (process-live-p process)
           (delete-process process)))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car error) (cdr error))))))

(defun remote-terminal-restart (terminal)
  "Explicitly replace disconnected TERMINAL with a fresh shell."
  (interactive
   (list
    (or remote-terminal-instance
        (car (remote-terminal-list
              (remote-get-workspace (remote-context)))))))
  (unless (remote-terminal-p terminal)
    (error "No remote terminal to restart"))
  (unless (eq (remote-terminal-state terminal) 'disconnected)
    (error "Remote terminal is not disconnected: %s"
           (remote-terminal-name terminal)))
  (let* ((workspace
          (or (remote-get-workspace
               (remote-terminal-workspace-id terminal))
              (error "Terminal workspace is closed")))
         (restart-function
          (plist-get
           (remote-terminal-metadata terminal)
           :restart-function))
         (arguments
          (plist-get (remote-terminal-metadata terminal) :restart)))
    (unless (or restart-function arguments)
      (error "Terminal frontend did not provide a restart recipe"))
    (remote-terminal-close terminal)
    (if restart-function
        (funcall restart-function terminal workspace)
      (apply #'remote-terminal-open workspace arguments))))

(defun remote-terminal-send-string (terminal string)
  "Send STRING to TERMINAL."
  (let ((process (remote-terminal-process terminal)))
    (unless (and (processp process)
                 (process-live-p process))
      (error "Remote terminal is not live: %S" terminal))
    (process-send-string process string)))

(defun remote-terminal-close (terminal)
  "Close TERMINAL and its buffer."
  (when (remote-terminal-p terminal)
    (let ((process (remote-terminal-process terminal))
          (buffer (remote-terminal-buffer terminal)))
      (when (and (processp process)
                 (process-live-p process))
        (delete-process process))
      (setf (remote-terminal-state terminal) 'closed)
      (remote-terminal--detach terminal)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    terminal))

(defun remote-terminal-list (&optional workspace)
  "Return open terminals, optionally only those for WORKSPACE."
  (let ((workspace-id
         (and workspace
              (remote-workspace-id
               (or (remote-get-workspace workspace)
                   (error "Unknown remote workspace: %S"
                          workspace)))))
        result)
    (maphash
     (lambda (_id terminal)
       (when (or (null workspace-id)
                 (equal
                  workspace-id
                  (remote-terminal-workspace-id terminal)))
         (push terminal result)))
     remote-terminals)
    (sort
     result
     (lambda (left right)
       (time-less-p
        (remote-terminal-opened-at left)
        (remote-terminal-opened-at right))))))

(remote-register-terminal-profile
 "default"
 :arguments '("-l"))

(provide 'remote-terminal)
;;; remote-terminal.el ends here
