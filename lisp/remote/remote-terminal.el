;;; remote-terminal.el --- Workspace-scoped routed terminals -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remote terminals are workspace-owned PTY processes backed by the routed
;; process API.  The built-in frontend is comint; native terminal frontends
;; can keep their own process/filter implementation and register
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

(defcustom remote-terminal-auto-close-on-exit t
  "Kill a remote terminal buffer when its process exits normally.
Transport loss keeps the buffer and its restart action.  Buffer cleanup runs
after the process sentinel returns, so window and popup hooks run normally."
  :type 'boolean
  :group 'remote)

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

(defconst remote-terminal--target-shell-command
  '("/bin/sh" "-c" "exec \"${SHELL:-/bin/sh}\" \"$@\"" "sh")
  "Command prefix that starts the target account's own login shell.
Profile arguments are appended and reach that shell verbatim.  The target
resolves `$SHELL' inside the terminal's own session, so starting a terminal
needs no file session just to learn which shell to run.")

(defun remote-terminal--shell (workspace profile &optional probe)
  "Return the target-native shell for WORKSPACE and PROFILE, or nil.
Known facts answer first.  With PROBE, ask the target for its account's login
shell, but only where that opens no new session.  Nil means the shell is not
known here; `remote-terminal-command' then lets the target start it."
  (let* ((target-id (remote-workspace-target-id workspace))
         (target (remote-get-target target-id))
         (environment (remote-workspace-environment workspace)))
    (or
     (remote-terminal--nonempty-shell
      (remote-terminal-profile-shell profile))
     (remote-terminal--nonempty-shell
      (and target (remote-target-shell target)))
     (remote-terminal--nonempty-shell
      (and environment
           (cdr
            (assoc-string
             "SHELL"
             (remote-environment-vars environment)
             t))))
     (and probe
          (remote-terminal--nonempty-shell
           (condition-case error-data
               (remote-path-login-shell
                (remote-workspace-context workspace) t)
             (error
              (remote-log
               'terminal-shell-probe-error
               :target target-id
               :error (error-message-string error-data))
              nil)))))))

(defun remote-terminal-command (&optional workspace profile probe)
  "Return the target-native interactive shell command for WORKSPACE.
PROFILE is a registered terminal profile or profile object.  The result is a
list suitable for routed process APIs and native terminal frontends.  With
PROBE, ask the target for its account's login shell when that needs no new
session.  A shell that is not known is started by the target itself through
`remote-terminal--target-shell-command'."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (remote-workspace-open workspace
                                     :adapter "process"
                                     :capability 'pty
                                     :load-environment nil)))
         (profile (remote-terminal--profile workspace profile))
         (shell (remote-terminal--shell workspace profile probe))
         (arguments
          (copy-sequence (remote-terminal-profile-arguments profile))))
    (if shell
        (cons shell arguments)
      (append remote-terminal--target-shell-command arguments))))

(cl-defun remote-terminal-prepare-client-launch
    (&optional workspace &key command directory environment profile client-environment-script)
  "Prepare a client-native PTY launch for WORKSPACE without spawning it.
COMMAND is a target argv or shell string; nil selects the terminal profile.
The returned plist holds :command (client argv), :workspace, :route and the
logical :directory.  It contains the invocation environment; do not log it.
CLIENT-ENVIRONMENT-SCRIPT is trusted frontend POSIX code run by the native
host before clearing its environment.  It may append NAME=VALUE entries to
the initially empty positional arguments, for host-generated UI variables.
Explicit plan environment entries take precedence over those defaults.
Backends must provide a client-executable process plan without an around-start
callback.  The native frontend later registers through `remote-terminal-adopt'."
  (let* ((remote-environment-inhibit t)
         (workspace (or (remote-get-workspace workspace)
                        (remote-workspace-open workspace :adapter "process"
                                               :capability 'pty :load-environment nil)))
         (context (remote-workspace-context workspace))
         (directory (if directory (remote-resolve-emacs-file-name directory)
                      (remote-workspace-root workspace)))
         (profile (remote-terminal--profile workspace profile)))
    (unless (equal (remote-file-name-target directory) (remote-workspace-target-id workspace))
      (user-error "Terminal directory belongs to another target: %s" directory))
    (setq command (cond ((null command) (remote-terminal-command workspace profile))
                        ((stringp command)
                         (append (remote-terminal-command workspace profile) (list "-c" command)))
                        (t command)))
    (unless (and (consp command)
                 (seq-every-p (lambda (value) (and (stringp value) (not (string-search "\0" value))))
                              command))
      (user-error "Terminal command must contain strings without NUL"))
    (let ((environment (append (when-let* ((capsule (remote-workspace-environment workspace)))
                                 (remote-environment-vars capsule))
                               (remote-terminal-profile-environment profile) environment)))
      (remote--call-with-process-route
       "process" 'pty context nil
       (lambda (route physical-directory resolved-environment)
         (let* ((overrides (remote--merge-environment-overrides resolved-environment environment))
                (process-environment (remote--apply-environment (remote-client-process-environment) overrides))
                (exec-path (remote--exec-path-for-environment overrides (remote-client-exec-path)))
                (execution (remote--prepare-backend-execution
                            route context command overrides physical-directory directory))
                (plan (remote-backend-prepare-process
                       route execution (list :command (remote-backend-execution-command execution)
                                             :connection-type 'pty) overrides))
                (client-directory (remote-client-file-name
                                   (remote-canonicalize-emacs-file-name
                                    (remote-backend-process-plan-default-directory plan))))
                (client-environment (or (remote-backend-process-plan-process-environment plan)
                                        process-environment))
                (client-command (copy-sequence
                                 (plist-get (remote-backend-process-plan-arguments plan) :command)))
                (seen (make-hash-table :test #'equal)) assignments)
           (unless (and client-directory (not (remote-backend-process-plan-around-start plan)))
             (signal 'remote-backend-unsupported
                     (list "Backend has no client-native PTY launch plan" (remote-route-backend-id route))))
           (unless (file-name-directory (car client-command))
             (let* ((default-directory client-directory)
                    (process-environment client-environment)
                    (exec-path (or (remote-backend-process-plan-exec-path plan) exec-path))
                    (program (executable-find (car client-command))))
               (unless program
                 (signal 'file-missing (list "Client PTY program is unavailable" (car client-command))))
               (setcar client-command program)))
           ;; A native host may have started before Emacs acquired its current
           ;; environment.  Make the complete client process plan explicit,
           ;; retaining make-process's first-entry precedence for duplicates.
           (dolist (entry client-environment)
             (let* ((separator (string-search "=" entry))
                    (name (if separator (substring entry 0 separator) entry)))
               (unless (gethash name seen)
                 (puthash name t seen)
                 (when separator (push entry assignments)))))
           (let* ((arguments (append (nreverse assignments)
                                     (list "/bin/sh" "-c" "cd -- \"$1\" && shift && exec \"$@\""
                                           "remote-terminal" client-directory)
                                     client-command))
                  (argv (if client-environment-script
                            (list "/bin/sh" "-c"
                                  (concat "set --; " client-environment-script
                                          "; exec /usr/bin/env -i \"$@\" "
                                          (mapconcat #'shell-quote-argument arguments " "))
                                  "remote-terminal-client-environment")
                          (append (list "/usr/bin/env" "-i") arguments))))
             (list :workspace workspace :route route :directory (file-name-as-directory directory)
                   :client-directory client-directory :command argv))))))))

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

(defun remote-terminal--routed-process-p (process)
  "Return whether PROCESS was started through the routed process boundary.
A frontend may hand over a handle of its own instead, such as the event pipe
of a native PTY.  Such a handle carries no route, so its end says nothing
about a transport."
  (and (processp process)
       (remote-route-p (process-get process 'remote-route))))

(defun remote-terminal--lost-process-p (terminal process)
  "Return whether PROCESS ended underneath an open TERMINAL.
A lost SSH PTY can exit while the file workspace stays healthy."
  (and (eq (remote-terminal-state terminal) 'open)
       (remote-terminal--routed-process-p process)
       (or (memq (process-status process) '(signal failed closed))
           (and (eq (process-status process) 'exit)
                (= (process-exit-status process) 255)))))

(defun remote-terminal--close-exited-buffer (terminal buffer)
  "Close BUFFER after TERMINAL's process sentinel has returned."
  (when (and (buffer-live-p buffer)
             (eq (remote-terminal-state terminal) 'closed)
             (eq (buffer-local-value 'remote-terminal-instance buffer) terminal))
    (kill-buffer buffer)))

(defun remote-terminal--process-finished (terminal process)
  "Record that TERMINAL's PROCESS has ended.
A disconnected terminal keeps its buffer and restart recipe; any other one is
removed with its process."
  (when (remote-terminal-p terminal)
    (if (eq (remote-terminal-state terminal) 'disconnected)
        (when (eq (remote-terminal-process terminal) process)
          (setf (remote-terminal-process terminal) nil))
      (let ((close (and remote-terminal-auto-close-on-exit
                        (eq (remote-terminal-state terminal) 'open)))
            (buffer (remote-terminal-buffer terminal)))
        (remote-terminal--detach terminal)
        (when (and close (buffer-live-p buffer))
          (run-at-time 0 nil #'remote-terminal--close-exited-buffer
                       terminal buffer))))))

(defun remote-terminal--buffer-killed ()
  "Close the terminal process owned by the current buffer."
  (when (and remote-terminal-instance
             (remote-terminal-p remote-terminal-instance))
    (let* ((terminal remote-terminal-instance)
           (process (remote-terminal-process terminal)))
      (setq remote-terminal-instance nil)
      ;; Announce the kill first: deleting the process runs its sentinel,
      ;; which would otherwise read the signal as a lost transport.
      (unless (eq (remote-terminal-state terminal) 'closed)
        (setf (remote-terminal-state terminal) 'closing))
      (when (and (processp process)
                 (process-live-p process))
        (delete-process process))
      (remote-terminal--close-frontend terminal)
      (remote-terminal--detach terminal))))

(defun remote-terminal-live-p (terminal)
  "Whether TERMINAL has a live process or native frontend handle."
  (and (remote-terminal-p terminal)
       (eq (remote-terminal-state terminal) 'open)
       (buffer-live-p (remote-terminal-buffer terminal))
       (if-let* ((function (plist-get (remote-terminal-metadata terminal) :live-function)))
           (with-current-buffer (remote-terminal-buffer terminal) (funcall function terminal))
         (let ((process (remote-terminal-process terminal)))
           (and (processp process) (process-live-p process))))))

(defun remote-terminal--close-frontend (terminal)
  "Release TERMINAL's optional native handle exactly once."
  (when-let* ((function (plist-get (remote-terminal-metadata terminal) :close-function)))
    (remote-terminal-put-metadata terminal :close-function nil)
    (condition-case error-data
        (when (buffer-live-p (remote-terminal-buffer terminal))
          (with-current-buffer (remote-terminal-buffer terminal) (funcall function terminal)))
      (error (remote-log 'terminal-close-function-error
                         :terminal (remote-terminal-id terminal)
                         :error (error-message-string error-data))))))

(defun remote-terminal-frontend-exited (terminal &optional status)
  "Report native TERMINAL's exit STATUS; nil means its result is unknown."
  (when (and (remote-terminal-p terminal) (eq (remote-terminal-state terminal) 'open))
    (if (and (integerp status) (zerop status))
        (remote-terminal--process-finished terminal nil)
      (remote-terminal-mark-disconnected terminal (list 'frontend-exit status)))))

(cl-defun remote-terminal-adopt
    (workspace buffer
               &key process name profile metadata)
  "Adopt a native terminal frontend BUFFER into WORKSPACE.
PROCESS defaults to BUFFER's process.  NAME and PROFILE are descriptive;
METADATA can record the frontend and consumer.  The frontend keeps its native
mode, process filter, and sentinel behavior while the framework owns process,
buffer, and workspace teardown.

For a frontend without an Emacs process, METADATA supplies `:live-function'
and optionally `:send-function' and `:close-function'.  Each receives the
terminal in BUFFER; send also receives its string.  Close is called once.
`:route' registers a native frontend's prepared route with the workspace.
`:disconnect-function' is
called with the terminal and the reason, in BUFFER, once when the terminal
loses its transport and before the frontend's sentinel sees the process end;
a frontend that removes its buffer on exit uses it to keep the buffer instead.
`:restart-function' is called with the terminal and its workspace by
`remote-terminal-restart' and creates the replacement."
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
    (unless (remote-terminal-live-p terminal)
      (error "Terminal frontend has no live process: %S" buffer))
    ;; The workspace owns teardown.  Killing a terminal buffer must not leave
    ;; Emacs waiting on the generic live-process query before our hook runs.
    (when (processp process) (set-process-query-on-exit-flag process nil))
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
    (when (processp process)
      (let ((frontend-sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (finished event)
           (let ((ended (memq (process-status finished) '(exit signal failed closed))))
             ;; Decide before the frontend runs: its sentinel may remove the
             ;; buffer, and a lost terminal has to keep it.
             (when (and ended (remote-terminal--lost-process-p terminal finished))
               (remote-terminal-mark-disconnected
                terminal (list 'process-exit (process-exit-status finished))))
             (unwind-protect
                 (when frontend-sentinel (funcall frontend-sentinel finished event))
               (when ended (remote-terminal--process-finished terminal finished))))))))
    (with-current-buffer buffer
      (setq-local remote-terminal-instance terminal)
      (add-hook 'kill-buffer-hook
                #'remote-terminal--buffer-killed nil t))
    (when (processp process) (process-put process 'remote-terminal terminal))
    (puthash id terminal remote-terminals)
    (remote-workspace-register-resource
     workspace 'terminal terminal
     (lambda (value _reason)
       (remote-terminal-close value))
     metadata)
    (when-let* ((route (or (and (processp process) (process-get process 'remote-route))
                          (plist-get metadata :route))))
      (remote-workspace-track-live-route workspace route))
    terminal))

(defun remote-terminal--run-disconnect-function (terminal reason)
  "Let TERMINAL's frontend prepare for losing its transport for REASON."
  (when-let* ((function
               (plist-get (remote-terminal-metadata terminal)
                          :disconnect-function))
              (buffer (remote-terminal-buffer terminal))
              ((buffer-live-p buffer)))
    (condition-case error-data
        (with-current-buffer buffer
          (funcall function terminal reason))
      (error
       (remote-log
        'terminal-disconnect-function-error
        :terminal (remote-terminal-id terminal)
        :error (error-message-string error-data))))))

(defun remote-terminal-mark-disconnected (terminal &optional reason)
  "Keep TERMINAL visible but stop it after transport loss.
REASON is retained for UI and Doctor output.  The shell is not replayed.
Marking a terminal that is already disconnected only updates REASON."
  (when (and (remote-terminal-p terminal)
             (not (memq (remote-terminal-state terminal)
                        '(closing closed))))
    (let ((first (not (eq (remote-terminal-state terminal)
                          'disconnected)))
          (process (remote-terminal-process terminal)))
      (setf (remote-terminal-state terminal) 'disconnected
            (remote-terminal-error terminal) reason)
      (when first
        (remote-terminal--run-disconnect-function terminal reason))
      ;; Deleting the process runs the frontend's sentinel, which finishes
      ;; its own teardown and may write to the buffer.
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (eq (remote-terminal-process terminal) process)
        (setf (remote-terminal-process terminal) nil))
      (when-let* ((buffer (and first (remote-terminal-buffer terminal)))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert
               "\n[remote] transport disconnected; use "
               "M-x remote-terminal-restart to start a new shell.\n"))))))
    terminal))

(cl-defun remote-terminal-open
    (&optional workspace
               &key name profile shell (arguments nil arguments-supplied-p)
               environment directory display)
  "Open a routed terminal for WORKSPACE.
PROFILE supplies shell, arguments, and environment defaults.  SHELL,
ARGUMENTS, and ENVIRONMENT override the profile.  DIRECTORY selects a logical
cwd on the workspace target; it defaults to the workspace root and survives
restart.  With DISPLAY, show the terminal buffer."
  (let* ((remote-environment-inhibit t)
         (workspace
         (or (remote-get-workspace workspace)
              (remote-workspace-open workspace
                                     :adapter "process"
                                     :capability 'pty
                                     :load-environment nil)))
         (directory
          (if directory (remote-resolve-emacs-file-name directory)
            (remote-workspace-root workspace)))
         (_target
          (unless (equal (remote-file-name-target directory)
                         (remote-workspace-target-id workspace))
            (user-error "Terminal directory belongs to another target: %s" directory)))
         (profile (remote-terminal--profile workspace profile))
         (command (remote-terminal-command workspace profile))
         (shell (or shell (car command)))
         (arguments (if arguments-supplied-p arguments (cdr command)))
         (environment
          (append
           (when-let* ((capsule (remote-workspace-environment workspace)))
             (remote-environment-vars capsule))
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
                  (file-name-as-directory directory))
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
                   :remote-process-class 'interactive
                   :remote-context
                   (remote-workspace-context workspace)
                   :remote-directory directory
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
                   :directory directory
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

(defun remote-terminal--disconnected (&optional workspace)
  "Return the first disconnected terminal, optionally only in WORKSPACE."
  (seq-find
   (lambda (terminal)
     (eq (remote-terminal-state terminal) 'disconnected))
   (remote-terminal-list workspace)))

(defun remote-terminal-restart (terminal)
  "Explicitly replace disconnected TERMINAL with a fresh shell.
The old terminal and its buffer are given up only once the replacement
exists, so a restart that fails can be repeated."
  (interactive
   (list
    (or remote-terminal-instance
        (remote-terminal--disconnected
         (remote-get-workspace (remote-context))))))
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
          (plist-get (remote-terminal-metadata terminal) :restart))
         (buffer (remote-terminal-buffer terminal))
         (name (and (buffer-live-p buffer) (buffer-name buffer)))
         replacement)
    (unless (or restart-function arguments)
      (error "Terminal frontend did not provide a restart recipe"))
    ;; A frontend recreates its buffer under the original name, so the old
    ;; buffer steps aside for the attempt and takes the name back on failure.
    (when name
      (with-current-buffer buffer
        (rename-buffer (format "%s (disconnected)" name) t)))
    (condition-case error-data
        (setq replacement
              (if restart-function
                  (funcall restart-function terminal workspace)
                (apply #'remote-terminal-open workspace arguments)))
      ((error quit)
       (when (and name (buffer-live-p buffer) (not (get-buffer name)))
         (with-current-buffer buffer
           (rename-buffer name)))
       (signal (car error-data) (cdr error-data))))
    (remote-terminal-close terminal)
    replacement))

(defun remote-terminal-send-string (terminal string)
  "Send STRING to TERMINAL."
  (let ((process (remote-terminal-process terminal))
        (function (plist-get (remote-terminal-metadata terminal) :send-function)))
    (unless (remote-terminal-live-p terminal)
      (error "Remote terminal is not live: %S" terminal))
    (if function
        (with-current-buffer (remote-terminal-buffer terminal) (funcall function terminal string))
      (process-send-string process string))))

(defun remote-terminal-close (terminal)
  "Close TERMINAL and its buffer."
  (when (remote-terminal-p terminal)
    (let ((process (remote-terminal-process terminal))
          (buffer (remote-terminal-buffer terminal)))
      (setf (remote-terminal-state terminal) 'closing)
      (when (and (processp process)
                 (process-live-p process))
        (delete-process process))
      (remote-terminal--close-frontend terminal)
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
