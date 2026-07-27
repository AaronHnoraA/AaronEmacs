;;; remote-process.el --- Routed process execution -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Process callers select a target and capability.  The route owns the
;; physical default-directory; callers keep target-native command arguments
;; and canonical `/fs:' file identities.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-connection)
(require 'remote-backend)

(declare-function remote-environment-vars "remote-environment" (environment))
(declare-function remote-environment-resolve "remote-environment"
                  (&optional context force))

(defvar remote-environment-inhibit nil)
(defvar remote-buffer-environment nil)
(defvar remote--buffer-base-process-environment nil
  "Client environment captured before a logical target environment is applied.")
(defvar remote--buffer-base-exec-path nil
  "Client executable search path captured before target activation.")

(define-error 'remote-exec-error "Remote command failed")

(cl-defstruct (remote-exec-result
               (:constructor remote-exec-result-create))
  status stdout stderr route context command)

(defconst remote--tramp-stderr-wrapper
  (concat
   "remote_token=$1\n"
   "shift\n"
   "remote_stderr=$(mktemp \"${TMPDIR:-/tmp}/emacs-remote-stderr.XXXXXX\")"
   " || exit 125\n"
   "trap 'rm -f \"$remote_stderr\"' EXIT HUP INT TERM\n"
   "\"$@\" 2>\"$remote_stderr\"\n"
   "remote_status=$?\n"
   "printf '\\036%s\\037' \"$remote_token\"\n"
   "cat \"$remote_stderr\"\n"
   "exit \"$remote_status\"\n")
  "Shell wrapper used to multiplex stderr over one standard TRAMP process.
Passing a separate `:stderr' destination to remote `make-process' makes
TRAMP create a FIFO and a second SSH connection.  Some SSH hosts cannot stat
that FIFO reliably, so the routed API frames stderr on stdout and separates it
again at the callback boundary.")

(defun remote--plist-delete (plist key)
  "Return a copy of PLIST without KEY and its value."
  (cl-loop for (item value) on plist by #'cddr
           unless (eq item key)
           append (list item value)))

(defun remote--stderr-frame-token ()
  "Return an unpredictable token for one asynchronous stderr frame."
  (md5 (format "%s:%s:%s:%s"
               (float-time) (emacs-pid) (random) (current-time))))

(defun remote--tramp-stderr-command (command token)
  "Wrap COMMAND so its stderr is framed with TOKEN on stdout."
  (append
   (list "/bin/sh" "-c" remote--tramp-stderr-wrapper
         "remote-exec-stderr" token)
   command))

(defun remote--split-stderr-frame (output token)
  "Split framed OUTPUT using TOKEN and return `(STDOUT . STDERR)'.
When the process did not emit its frame, retain all received data as stdout."
  (let ((marker (concat "\036" token "\037")))
    (if-let* ((position (string-match (regexp-quote marker) output)))
        (cons (substring output 0 position)
              (substring output (+ position (length marker))))
      (cons output ""))))

(defun remote--transport-error-p (error)
  "Return non-nil when ERROR plausibly means that a route is unavailable."
  (let ((type (car-safe error))
        (message (downcase (error-message-string error))))
    (and (memq type '(file-error remote-file-error))
         (string-match-p
          (rx (or "connection"
                  "connect"
                  "network"
                  "timeout"
                  "timed out"
                  "host is down"
                  "no route"
                  "connection refused"
                  "connection reset"
                  "connection closed"
                  "broken pipe"
                  "tramp failed"))
          message))))

(defun remote--context-value (&optional context)
  "Return CONTEXT as a `remote-context'."
  (cond
   ((remote-context-p context) context)
   ((stringp context) (remote-context context))
   (t (remote-context))))

(defun remote--environment-vars (&optional context)
  "Return environment overrides for CONTEXT."
  (unless remote-environment-inhibit
    (when (fboundp 'remote-environment-resolve)
      (when-let* ((environment
                   (ignore-errors
                     (remote-environment-resolve
                      (remote--context-value context)))))
        (if (fboundp 'remote-environment-vars)
            (remote-environment-vars environment)
          environment)))))

(defun remote--apply-environment (base overrides)
  "Apply environment OVERRIDES to BASE and return a fresh environment."
  (let ((overrides
         (if (and (fboundp 'remote-environment-p)
                  (remote-environment-p overrides))
             (remote-environment-vars overrides)
           overrides))
        (result (copy-sequence base)))
    (dolist (entry overrides)
      (let* ((name (format "%s" (car entry)))
             (value (cdr entry))
             (prefix (concat name "=")))
        (setq result
              (cl-delete-if
               (lambda (item) (string-prefix-p prefix item))
               result))
        (when value
          (push (concat prefix (format "%s" value)) result))))
    result))

(defun remote--merge-environment-overrides (base overrides)
  "Overlay environment OVERRIDES on BASE as a normalized alist.
Later entries win by variable name.  A small explicit override therefore
augments the resolved target capsule instead of replacing it wholesale."
  (let ((base
         (if (and (fboundp 'remote-environment-p)
                  (remote-environment-p base))
             (remote-environment-vars base)
           base))
        (overrides
         (if (and (fboundp 'remote-environment-p)
                  (remote-environment-p overrides))
             (remote-environment-vars overrides)
           overrides))
        result)
    (dolist (entry (append base overrides))
      (let ((name (format "%s" (car entry))))
        (setq result
              (cl-delete-if
               (lambda (known)
                 (string-equal-ignore-case
                  name (format "%s" (car known))))
               result))
        (setq result
              (append result (list (cons name (cdr entry)))))))
    result))

(defun remote--exec-path-for-environment (environment fallback)
  "Return target-native exec path from ENVIRONMENT or FALLBACK."
  (when (and (fboundp 'remote-environment-p)
             (remote-environment-p environment))
    (setq environment (remote-environment-vars environment)))
  (if-let* ((path (cdr (assoc-string "PATH" environment t))))
      (append (split-string path path-separator t)
              (and (boundp 'exec-directory) (list exec-directory)))
    fallback))

;; `remote-process.el' is also a practical hot-reload boundary while debugging
;; a live connection.  Define the core cleanup primitive when that Emacs
;; instance predates it, so reloading this file repairs already-running
;; configurations without resetting registries by reloading `remote-core.el'.
(unless (fboundp 'remote--kill-internal-buffer)
  (defun remote--kill-internal-buffer (buffer)
    "Detach processes and kill framework-owned BUFFER without user hooks."
    (when (buffer-live-p buffer)
      (when-let* ((process (get-buffer-process buffer)))
        (set-process-sentinel process #'ignore)
        (set-process-buffer process nil))
      (with-current-buffer buffer
        (let ((kill-buffer-hook nil)
              (kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defalias 'remote--kill-internal-process-buffer
  #'remote--kill-internal-buffer)

(defun remote--dispose-async-capture-resources
    (stderr-process stdout-buffer stderr-buffer)
  "Dispose one async request's STDERR-PROCESS and capture buffers.
This function runs after the owning process sentinel has returned.  Detaching
or killing a process's own buffer from inside its sentinel can re-enter Emacs'
process/file-handler cleanup and fail before either capture buffer is freed."
  (when (and (processp stderr-process)
             (process-live-p stderr-process))
    (delete-process stderr-process))
  (dolist (buffer (list stdout-buffer stderr-buffer))
    (condition-case cleanup-error
        (remote--kill-internal-process-buffer buffer)
      (error
       (remote-log
        'process-cleanup-error
        :buffer (and (buffer-live-p buffer) (buffer-name buffer))
        :error (error-message-string cleanup-error))))))

(defun remote--schedule-async-capture-cleanup
    (owner-process stderr-process stdout-buffer stderr-buffer)
  "Detach and later clean one async request's capture resources.
OWNER-PROCESS is already terminal when called from its sentinel.  Detaching
its buffer synchronously makes completion observable immediately without
killing a buffer from inside that sentinel; actual buffer/process disposal is
deferred until the event loop has unwound."
  (when (processp owner-process)
    (set-process-buffer owner-process nil))
  (when (processp stderr-process)
    (set-process-buffer stderr-process nil))
  (run-at-time
   0 nil #'remote--dispose-async-capture-resources
   stderr-process stdout-buffer stderr-buffer))

(defun remote--logical-process-directory (context &optional directory)
  "Return the canonical logical working directory for CONTEXT.
DIRECTORY, when non-nil, is the caller's actual `default-directory'.  It
takes precedence over the workspace root so native `make-process' callers
keep Emacs' normal working-directory semantics."
  (or (and directory
           (remote-canonicalize-file-name directory))
      (remote-context-workspace-root context)
      (remote-make-file-name
       (remote-context-target-id context)
       (file-name-directory (remote-context-localname context)))))

(defun remote--prepare-backend-execution
    (route context command environment physical-directory
           &optional logical-directory)
  "Prepare COMMAND for ROUTE while preserving legacy plugin compatibility.
LOGICAL-DIRECTORY is an explicit invocation cwd when supplied."
  (let* ((context (remote--context-value context))
         (logical-directory
          (remote--logical-process-directory context logical-directory)))
    (if (remote-route-backend route)
        (remote-backend-prepare-execution
         route context command environment
         :logical-directory logical-directory)
      ;; Custom link plugins created against the first framework API have no
      ;; `remote-backend' object.  Keep them usable while presenting the same
      ;; execution record to process callers.
      (remote-backend-execution-create
       :backend-id (remote-route-link-plugin-id route)
       :route route
       :context context
       :logical-directory logical-directory
       :physical-directory physical-directory
       :command (copy-sequence command)
       :environment environment
       :metadata '(:program-form search)))))

(defun remote--call-with-process-route
    (adapter capability context constraints function)
  "Call FUNCTION with each eligible route until startup succeeds.
FUNCTION receives the route, a physical default directory, and environment
overrides.  Failover is limited to transport errors."
  (let* ((context (remote--context-value context))
         (routes (remote-routes adapter capability context constraints))
         (environment (remote--environment-vars context))
         last-error)
    (unless routes
      (error "No %s route for target %s"
             capability (remote-context-target-id context)))
    (catch 'done
      (while routes
        (let ((route (pop routes)))
          (condition-case err
              (let* ((remote-current-connection
                      (remote-connection-ensure route context))
                     (logical-directory
                      (remote--logical-process-directory context))
                     (physical-directory
                      (file-name-as-directory
                       (remote-project-file-name
                        logical-directory route)))
                     (value
                      (funcall function
                               route physical-directory environment)))
                (remote-report-route-success route)
                (remote-log
                 'process
                 :target (remote-route-target-id route)
                 :link (remote-route-link-id route)
                 :plugin (remote-route-link-plugin-id route)
                 :capability capability
                 :adapter adapter)
                (throw 'done value))
            (error
             (setq last-error err)
             (let* ((failure-scope
                     (remote-report-route-failure route err))
                    (transport-error (eq failure-scope 'transport))
                    (backend-error (eq failure-scope 'backend)))
               (when transport-error
                 (setq routes
                       (seq-remove
                        (lambda (candidate)
                          (equal (remote-route-link-id candidate)
                                 (remote-route-link-id route)))
                        routes)))
               (when backend-error
                 (setq routes
                       (seq-remove
                        (lambda (candidate)
                          (and
                           (equal (remote-route-link-id candidate)
                                  (remote-route-link-id route))
                           (equal
                            (remote-route-link-plugin-id candidate)
                            (remote-route-link-plugin-id route))))
                        routes)))
               (unless (and routes
                            (or transport-error backend-error))
                 (signal (car err) (cdr err))))))))
      (if last-error
          (signal (car last-error) (cdr last-error))
        (error "No usable %s route" capability)))))

(defun remote--project-process-file-path (path route)
  "Project logical process-file PATH through ROUTE when applicable.
Same-target files use the selected backend.  A different target may still
expose a directly client-accessible path (for example the `local' target);
otherwise retain the logical spelling so a capable remote file handler can
stage it."
  (if (and
       (stringp path)
       (or (remote-fs-file-name-p path)
           (file-remote-p path)))
      (let* ((logical (remote-canonicalize-file-name path))
             (target (remote-file-name-target logical)))
        (cond
         ((equal target (remote-route-target-id route))
          (remote-project-file-name logical route))
         ((ignore-errors
            (remote-client-file-name logical)))
         (t logical)))
    path))

(defun remote--project-process-file-destination (destination route)
  "Project the stderr file in process-file DESTINATION through ROUTE.
The stdout half remains a buffer designator and is never interpreted as a
file name."
  (if (and (consp destination)
           (stringp (cadr destination)))
      (list
       (car destination)
       (remote--project-process-file-path
        (cadr destination) route))
    destination))

(defun remote--process-file-raw
    (program infile destination display args adapter context constraints
             &optional environment)
  "Run PROGRAM synchronously without recursively resolving environment."
  (let ((remote-environment-inhibit t))
    (remote--call-with-process-route
     adapter 'process-sync context constraints
     (lambda (route physical-directory resolved-environment)
       (let* ((overrides
               (remote--merge-environment-overrides
                resolved-environment environment))
              (execution
               (remote--prepare-backend-execution
                route context (cons program args) overrides
                physical-directory))
              (default-directory
               (remote-backend-execution-physical-directory execution))
              (command (remote-backend-execution-command execution))
              (process-environment
               (remote--apply-environment process-environment overrides))
              (exec-path
               (remote--exec-path-for-environment overrides exec-path))
              (remote-current-route route)
              (remote-current-adapter-id adapter)
              (infile
               (remote--project-process-file-path infile route))
              (destination
               (remote--project-process-file-destination
                destination route)))
         (apply #'process-file
                (car command) infile destination display
                (cdr command)))))))

(defun remote-process-file
    (program &optional infile destination display &rest args)
  "Run PROGRAM synchronously in the current logical target.
The call has the same positional interface as `process-file'.  Bind
`remote-current-adapter-id' or use `remote-with-route' to select a specialized
adapter."
  (let ((adapter (or remote-current-adapter-id "process")))
    (remote--call-with-process-route
     adapter 'process-sync nil nil
     (lambda (route physical-directory environment)
       (let* ((execution
               (remote--prepare-backend-execution
                route nil (cons program args) environment
                physical-directory))
              (default-directory
               (remote-backend-execution-physical-directory execution))
              (command (remote-backend-execution-command execution))
              (process-environment
               (remote--apply-environment process-environment environment))
              (exec-path
               (remote--exec-path-for-environment environment exec-path))
              (remote-current-route route)
              (remote-current-adapter-id adapter)
              (infile
               (remote--project-process-file-path infile route))
              (destination
               (remote--project-process-file-destination
                destination route)))
         (apply #'process-file
                (car command) infile destination display
                (cdr command)))))))

(defun remote-make-process (&rest plist)
  "Create an asynchronous process in the selected logical target.
PLIST accepts all `make-process' keys plus `:remote-adapter',
`:remote-context', `:remote-link', `:remote-environment', and
`:remote-directory'.  The latter preserves an official `make-process'
caller's logical `default-directory' independently of its workspace root."
  (let* ((adapter
          (or (plist-get plist :remote-adapter)
              remote-current-adapter-id
              "process"))
         (context (plist-get plist :remote-context))
         (link (plist-get plist :remote-link))
         (explicit-environment (plist-get plist :remote-environment))
         (logical-directory (plist-get plist :remote-directory))
         (stderr-token (plist-get plist :remote-stderr-token))
         (arguments (copy-sequence plist)))
    (dolist (key '(:remote-adapter :remote-context :remote-link
                   :remote-environment :remote-directory
                   :remote-stderr-token))
      (setq arguments (remote--plist-delete arguments key)))
    (remote--call-with-process-route
     adapter 'process-async context (and link (list :link link))
     (lambda (route physical-directory resolved-environment)
       (let* ((context-value (remote--context-value context))
              (overrides
               (remote--merge-environment-overrides
                resolved-environment explicit-environment))
              (command (plist-get arguments :command))
              (execution
               (remote--prepare-backend-execution
                route context-value command overrides physical-directory
                logical-directory))
              (default-directory
               (or (remote-backend-execution-physical-directory execution)
                   physical-directory))
              (process-environment
               (remote--apply-environment process-environment overrides))
              (exec-path
               (remote--exec-path-for-environment overrides exec-path))
              (remote-current-route route)
              (remote-current-adapter-id adapter)
              (command (remote-backend-execution-command execution))
              (program (car-safe command))
              (resolved-program
               (and
                (stringp program)
                (not (file-name-absolute-p program))
                (executable-find program t)))
              ;; Resolve at the official `make-process' boundary after the
              ;; target environment and physical directory are active.
              ;; Backends differ in whether their remote spawn primitive
              ;; searches the supplied PATH; an absolute target-native
              ;; executable makes the contract uniform.
              (arguments
               (if resolved-program
                   (let ((resolved-command
                          (cons
                           (remote-file-local-name resolved-program)
                           (cdr command))))
                     (setf
                      (remote-backend-execution-command execution)
                      resolved-command)
                     (plist-put arguments :command resolved-command))
                 arguments))
              ;; Backend placement is represented as data.  The common API
              ;; does not inspect target IDs, TRAMP methods, or backend names.
              (plan
               (remote-backend-prepare-process
                route execution arguments overrides))
              (arguments
               (remote-backend-process-plan-arguments plan))
              (framed-stderr
               (and stderr-token
                    (eq
                     (remote-backend-process-plan-stderr-mode plan)
                     'framed)))
              (arguments
               (if framed-stderr
                   (let* ((without-stderr
                           (remote--plist-delete arguments :stderr))
                          (spawn-command
                           (plist-get without-stderr :command)))
                     (plist-put
                      without-stderr :command
                      (remote--tramp-stderr-command
                       spawn-command stderr-token)))
                 arguments))
              (process
               (let ((default-directory
                      (or
                       (remote-backend-process-plan-default-directory plan)
                       default-directory)))
                 (apply #'make-process arguments))))
         (process-put process 'remote-route route)
         (process-put process 'remote-context context-value)
         (when framed-stderr
           (process-put process 'remote-stderr-token stderr-token))
         (dolist
             (property
              (remote-backend-process-plan-process-properties plan))
           (process-put process (car property) (cdr property)))
         (setf (remote-backend-execution-command execution)
               (plist-get arguments :command))
         (process-put process 'remote-backend-execution execution)
         process)))))

(defun remote-make-client-process (&rest plist)
  "Create a native client process from a logical target buffer.

PLIST accepts the official `make-process' keys plus
`:remote-client-directory', `:remote-client-environment', and
`:remote-client-exec-path'.  The three extension keys select native client
state and are removed before calling `make-process'.  By default this uses the
buffer's environment snapshot from before a target capsule was installed and
starts in `temporary-file-directory'.

This is the explicit placement boundary for a local UI or protocol proxy whose
stdio peer may be reached through `remote-local-bridge-command'.  It never
routes through a `/fs:' file-name handler."
  (let* ((arguments (copy-sequence plist))
         (directory
          (or (plist-get arguments :remote-client-directory)
              temporary-file-directory))
         (environment
          (or (plist-get arguments :remote-client-environment)
              remote--buffer-base-process-environment
              (default-value 'process-environment)))
         (client-exec-path
          (or (plist-get arguments :remote-client-exec-path)
              remote--buffer-base-exec-path
              (default-value 'exec-path))))
    (dolist (key '(:remote-client-directory
                   :remote-client-environment
                   :remote-client-exec-path))
      (setq arguments (remote--plist-delete arguments key)))
    (when (or (remote-fs-file-name-p directory)
              (file-remote-p directory))
      (signal 'wrong-type-argument
              (list 'native-client-directory-p directory)))
    (setq directory
          (file-name-as-directory
           (expand-file-name directory temporary-file-directory)))
    (let ((default-directory directory)
          (process-environment (copy-sequence environment))
          (exec-path (copy-sequence client-exec-path))
          ;; We are deliberately crossing out of the logical target.  Prevent
          ;; an outer `/fs:' dispatch from being inherited by the native spawn.
          (inhibit-file-name-operation 'make-process)
          (inhibit-file-name-handlers
           (cons #'remote-file-name-handler
                 (cons #'tramp-file-name-handler
                       inhibit-file-name-handlers))))
      (apply #'make-process
             (plist-put arguments :file-handler nil)))))

(defun remote-executable-find (program &optional context)
  "Find PROGRAM on CONTEXT's logical target.
The return value is a target-native path, never a physical TRAMP link name."
  (remote--call-with-process-route
   (or remote-current-adapter-id "process")
   'process-sync context nil
   (lambda (_route physical-directory environment)
     (let* ((default-directory physical-directory)
            (process-environment
             (remote--apply-environment process-environment environment))
            (exec-path
             (remote--exec-path-for-environment environment exec-path))
            (found (executable-find program t)))
       (and found (remote-file-local-name found))))))

(cl-defun remote-local-bridge-command
    (program &key args context (adapter "exec") link environment directory)
  "Return local argv which bridges stdio to target PROGRAM.
ARGS are target-native arguments.  CONTEXT, ADAPTER, LINK, and ENVIRONMENT use
the same routing contract as `remote-exec'.  DIRECTORY is the logical target
working directory and defaults to the context workspace.

The selected backend decides how to implement the bridge; callers never branch
on whether the target is local or remote.  This API is for local protocol
proxies which own UI/network state while their language server, debugger, or
other stdio peer remains on the selected target."
  (let ((context (remote--context-value context)))
    (remote--call-with-process-route
     adapter 'process-async context (and link (list :link link))
     (lambda (route physical-directory resolved-environment)
       (let* ((overrides
               (remote--merge-environment-overrides
                resolved-environment environment))
              (logical-directory
               (or directory
                   (remote-context-workspace-root context)))
              (execution
               (remote--prepare-backend-execution
                route context (cons program args) overrides
                physical-directory logical-directory)))
         (remote-backend-stdio-bridge-command execution))))))

(cl-defun remote-exec
    (program &key args context (adapter "exec") link environment check trim)
  "Execute PROGRAM on a logical target and return a `remote-exec-result'.
ARGS is a list passed verbatim.  CONTEXT is a context or logical path.
ADAPTER and LINK constrain routing.  ENVIRONMENT is an override alist.
With CHECK, signal `remote-exec-error' for a nonzero status.  With TRIM,
trim surrounding whitespace from stdout and stderr."
  (let* ((context (remote--context-value context))
         (stdout-buffer (generate-new-buffer " *remote-exec-stdout*"))
         (stderr-file (make-temp-file "remote-exec-stderr-"))
         result)
    (unwind-protect
        (setq result
              (remote--call-with-process-route
               adapter 'process-sync context
               (and link (list :link link))
               (lambda (route physical-directory resolved-environment)
                 (let* ((overrides
                         (remote--merge-environment-overrides
                          resolved-environment environment))
                        (execution
                         (remote--prepare-backend-execution
                          route context (cons program args) overrides
                          physical-directory))
                        (physical-directory
                         (remote-backend-execution-physical-directory
                          execution))
                        (command
                         (remote-backend-execution-command execution))
                        (base-process-environment process-environment)
                        (base-exec-path exec-path)
                        (remote-current-route route)
                        (remote-current-adapter-id adapter)
                        (status
                         (with-current-buffer stdout-buffer
                           ;; `default-directory' is buffer-local.  Bind the
                           ;; physical route after selecting the actual
                           ;; execution/output buffer, or process-file silently
                           ;; falls back to that buffer's local directory.
                           (let ((default-directory physical-directory)
                                 (process-environment
                                  (remote--apply-environment
                                   base-process-environment overrides))
                                 (exec-path
                                  (remote--exec-path-for-environment
                                   overrides base-exec-path)))
                             (apply #'process-file
                                    (car command) nil
                                    (list stdout-buffer stderr-file)
                                    nil (cdr command)))))
                        (stdout
                         (with-current-buffer stdout-buffer
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                        (stderr
                         (with-temp-buffer
                           (when (file-readable-p stderr-file)
                             (insert-file-contents stderr-file))
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
                   (remote-exec-result-create
                    :status status
                    :stdout (if trim (string-trim stdout) stdout)
                    :stderr (if trim (string-trim stderr) stderr)
                    :route route
                    :context context
                    :command command)))))
      (remote--kill-internal-process-buffer stdout-buffer)
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))
    (when (and check
               (not (and (integerp (remote-exec-result-status result))
                         (zerop (remote-exec-result-status result)))))
      (signal
       'remote-exec-error
       (list
        (format "%s exited with %s: %s"
                program
                (remote-exec-result-status result)
                (remote-exec-result-stderr result))
        result)))
    result))

(cl-defun remote-exec-output (program &rest options)
  "Execute PROGRAM with OPTIONS and return stdout.
OPTIONS are forwarded to `remote-exec'."
  (remote-exec-result-stdout
   (apply #'remote-exec program options)))

(cl-defun remote-exec-async
    (program &key args context (adapter "exec") link environment
             callback name coding)
  "Execute PROGRAM asynchronously through the routed `make-process' boundary.
CALLBACK receives one `remote-exec-result' after the process exits.  CONTEXT,
ADAPTER, LINK, ENVIRONMENT, and ARGS have the same meaning as in
  `remote-exec'.  The returned process retains its `remote-route' property."
  (let* ((context (remote--context-value context))
         (origin-buffer (current-buffer))
         (stderr-token (remote--stderr-frame-token))
         (command (cons program args))
         stdout-buffer
         stderr-buffer
         stderr-process
         process)
    (condition-case err
        (progn
          ;; Allocate every owned resource inside the protected region.
          ;; `make-pipe-process' can fail before the command process exists;
          ;; buffers created just before it must still be reclaimed.
          (setq stdout-buffer
                (generate-new-buffer " *remote-exec-async-stdout*")
                stderr-buffer
                (generate-new-buffer " *remote-exec-async-stderr*"))
          ;; Supplying a buffer as `make-process' :stderr makes Emacs allocate
          ;; an auxiliary process with `internal-default-process-sentinel'.
          ;; That sentinel appends "Process ... stderr finished" to the buffer,
          ;; corrupting the command's stderr.  Own the pipe explicitly so its
          ;; sentinel is quiet and its lifetime is bounded with this request.
          (setq stderr-process
                (make-pipe-process
                 :name
                 (generate-new-buffer-name
                  (format "remote-%s-stderr"
                          (file-name-nondirectory program)))
                 :buffer stderr-buffer
                 :noquery t
                 :sentinel #'ignore))
          (setq process
                (remote-make-process
                 :name (or name
                           (format "remote-%s"
                                   (file-name-nondirectory program)))
                 :buffer stdout-buffer
                 :stderr stderr-process
                 :command command
                 :coding coding
                 :connection-type 'pipe
                 :noquery t
                 :remote-adapter adapter
                 :remote-context context
                 :remote-link link
                 :remote-environment environment
                 :remote-stderr-token stderr-token
                 :sentinel
                 (lambda (finished _event)
                   (when (and
                          (memq (process-status finished)
                                '(exit signal failed closed))
                          (not (process-get finished
                                            'remote-exec-callback-done)))
                     (process-put finished 'remote-exec-callback-done t)
                     (let* ((combined
                             (if (buffer-live-p stdout-buffer)
                                 (with-current-buffer stdout-buffer
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))
                               ""))
                            (framed-token
                             (process-get finished 'remote-stderr-token))
                            (streams
                             (and framed-token
                                  (remote--split-stderr-frame
                                   combined framed-token)))
                            (stdout (if streams (car streams) combined))
                            (stderr
                             (if streams
                                 (cdr streams)
                               (if (buffer-live-p stderr-buffer)
                                   (with-current-buffer stderr-buffer
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))
                                 "")))
                            (result
                             (remote-exec-result-create
                              :status
                              (if (memq (process-status finished)
                                        '(exit signal))
                                  (process-exit-status finished)
                                1)
                              :stdout stdout
                              :stderr stderr
                              :route (process-get finished 'remote-route)
                              :context context
                              :command command)))
                       (unwind-protect
                           (when callback
                             (condition-case callback-error
                                 (if (buffer-live-p origin-buffer)
                                     (with-current-buffer origin-buffer
                                       (funcall callback result))
                                   (funcall callback result))
                               (error
                                (remote-log
                                 'process-callback-error
                                 :process (process-name finished)
                                 :target
                                 (remote-context-target-id context)
                                 :error
                                 (error-message-string callback-error)))))
                         (remote--schedule-async-capture-cleanup
                          finished stderr-process
                          stdout-buffer stderr-buffer))))))))
      (error
       (remote--dispose-async-capture-resources
        stderr-process stdout-buffer stderr-buffer)
       (signal (car err) (cdr err))))
    process))

(remote-register-adapter
 "process"
 :capabilities '(process-sync process-async pty environment)
 :preferences '((default . ("native" "tramp-rpc" "tramp"))))

(remote-register-adapter
 "exec"
 :capabilities '(process-sync process-async environment)
 :preferences '((default . ("tramp-rpc" "tramp" "native"))))

(provide 'remote-process)
;;; remote-process.el ends here
