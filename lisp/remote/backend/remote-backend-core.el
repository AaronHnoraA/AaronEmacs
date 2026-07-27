;;; remote-backend-core.el --- Transport backend contract -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A backend adapts one selected route to an implementation understood by
;; Emacs.  It owns physical path projection, session lifecycle operations, and
;; optional process/channel preparation.  Logical identities and route policy
;; remain in `remote-core'; callers must not inspect backend path syntax.
;;
;; `remote-register-backend' mirrors each backend into the older
;; `remote-link-plugin' registry.  That compatibility bridge lets existing
;; callers migrate without creating two competing routing systems.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)

(define-error 'remote-backend-error "Remote backend error")
(define-error 'remote-backend-unsupported
              "Remote backend operation is unsupported"
              'remote-backend-error)

(cl-defstruct (remote-backend
               (:constructor remote-backend-create))
  id capabilities
  available-function
  project-function
  client-file-function
  expand-function
  prepare-function
  connect-function
  live-function
  disconnect-function
  prepare-process-function
  stdio-bridge-function
  network-function
  stream-function
  forward-function
  classify-error-function
  describe-function
  program-form)

(cl-defstruct (remote-backend-execution
               (:constructor remote-backend-execution-create))
  backend-id route context
  logical-directory physical-directory
  command environment metadata)

(cl-defstruct (remote-backend-process-plan
               (:constructor remote-backend-process-plan-create))
  arguments default-directory stderr-mode process-properties metadata)

(cl-defstruct (remote-forward
               (:constructor remote-forward-create))
  backend-id route context handle close-function
  local-endpoint remote-endpoint state metadata)

(defvar remote-backends (make-hash-table :test #'equal)
  "Registered transport backends keyed by normalized backend ID.")

(defun remote-get-backend (id)
  "Return the backend named ID, or nil."
  (gethash (remote-normalize-id id t) remote-backends))

(defun remote-route-backend (route)
  "Return the backend selected by ROUTE."
  (and (remote-route-p route)
       (remote-get-backend (remote-route-link-plugin-id route))))

(defun remote-backend-supports-p (backend capability)
  "Return non-nil when BACKEND implements CAPABILITY."
  (and (remote-backend-p backend)
       (memq capability (remote-backend-capabilities backend))))

(defun remote-backend--available-p (backend link context)
  "Return whether BACKEND is available for LINK and CONTEXT."
  (if-let* ((function (remote-backend-available-function backend)))
      (funcall function link context)
    t))

(defun remote-backend--legacy-project (backend file link route)
  "Project FILE through BACKEND for legacy LINK and ROUTE callers."
  (if-let* ((function (remote-backend-project-function backend)))
      (funcall function file link route)
    (signal 'remote-backend-unsupported
            (list (remote-backend-id backend) 'project-file-name))))

(defun remote-backend--legacy-connect (backend route context)
  "Connect BACKEND for legacy ROUTE and CONTEXT callers."
  (if-let* ((function (remote-backend-connect-function backend)))
      (funcall function route context)
    t))

(defun remote-backend--legacy-live-p
    (backend connection route context)
  "Check BACKEND CONNECTION for legacy ROUTE and CONTEXT callers."
  (if-let* ((function (remote-backend-live-function backend)))
      (funcall function connection route context)
    t))

(defun remote-backend--legacy-disconnect (backend connection route)
  "Disconnect BACKEND CONNECTION for legacy ROUTE callers."
  (when-let* ((function (remote-backend-disconnect-function backend)))
    (funcall function connection route)))

(cl-defun remote-register-backend
    (id &key capabilities available project client-file-name
        expand-localname prepare
        connect live disconnect
        prepare-process stdio-bridge
        make-network-process open-network-stream port-forward
        classify-error describe (program-form 'search))
  "Register transport backend ID and return it.

CAPABILITIES are route capabilities implemented by the backend.  PROJECT maps
a logical file and selected link to a physical Emacs file name.
CLIENT-FILE-NAME maps a logical file to an ordinary path directly accessible
to client-side tools, or returns nil when the target filesystem is not shared.
EXPAND-LOCALNAME resolves target-relative names such as `~/src' and must
return a target-native absolute localname; this keeps target HOME lookup out
of the logical identity layer.  PREPARE can normalize a
`remote-backend-execution' before a process is started.  CONNECT, LIVE, and
DISCONNECT own the backend session lifecycle.

PREPARE-PROCESS receives an execution record, an official `make-process'
argument plist, and target environment overrides.  It returns a
`remote-backend-process-plan' describing placement and stderr behavior.
STDIO-BRIDGE receives an execution record and returns client-local argv whose
stdio is connected to that target execution.  These operations keep
backend-specific spawn decisions out of the common process API.

MAKE-NETWORK-PROCESS, OPEN-NETWORK-STREAM, and PORT-FORWARD are optional
channel operations.  PROGRAM-FORM is `search' when the backend accepts a bare
program name and `absolute' when its spawn protocol requires an absolute
target-native executable."
  (let* ((id (remote-normalize-id id))
         (unknown (seq-difference capabilities remote-capabilities)))
    (when unknown
      (error "Unknown capabilities for backend %s: %S" id unknown))
    (let ((backend
           (remote-backend-create
            :id id
            :capabilities (copy-sequence capabilities)
            :available-function available
            :project-function project
            :client-file-function client-file-name
            :expand-function expand-localname
            :prepare-function prepare
            :connect-function connect
            :live-function live
            :disconnect-function disconnect
            :prepare-process-function prepare-process
            :stdio-bridge-function stdio-bridge
            :network-function make-network-process
            :stream-function open-network-stream
            :forward-function port-forward
            :classify-error-function classify-error
            :describe-function describe
            :program-form program-form)))
      (puthash id backend remote-backends)
      ;; Compatibility for the current route resolver and connection pool.
      (remote-register-link-plugin
       id
       :capabilities capabilities
       :available-p
       (lambda (link context)
         (remote-backend--available-p backend link context))
       :project-file-name
       (lambda (file link route)
         (remote-backend--legacy-project backend file link route))
       :connect
       (lambda (route context)
         (remote-backend--legacy-connect backend route context))
       :connection-live-p
       (lambda (connection route context)
         (remote-backend--legacy-live-p
          backend connection route context))
       :disconnect
       (lambda (connection route)
         (remote-backend--legacy-disconnect backend connection route))
       :describe describe)
      backend)))

(defun remote-backend-expand-localname
    (route name &optional directory)
  "Resolve NAME on ROUTE and return its target-native absolute localname.
DIRECTORY, when non-nil, is already a target-native absolute localname.
Backends must resolve leading `~' on the target rather than on the Emacs
client.  Pure absolute and relative names have a conservative default so
legacy backends remain usable, but a backend must implement the operation to
support target HOME expansion."
  (let* ((backend (or (remote-route-backend route)
                      (error "No backend registered for route %S" route)))
         (function (remote-backend-expand-function backend)))
    (cond
     (function
      (funcall function
               name (or directory "/")
               (remote-route-link route) route))
     ((string-prefix-p "~" name)
      (signal
       'remote-backend-unsupported
       (list
        (format
         "Backend %s cannot resolve target home paths"
         (remote-backend-id backend)))))
     (t
      (let ((inhibit-file-name-handlers
             (cons #'tramp-file-name-handler
                   inhibit-file-name-handlers))
            (inhibit-file-name-operation 'expand-file-name))
        (expand-file-name name (or directory "/")))))))

(defun remote-backend-project-file-name (route file-name)
  "Project logical FILE-NAME through ROUTE's backend."
  (let ((backend (or (remote-route-backend route)
                     (error "No backend registered for route %S" route))))
    (remote-backend--legacy-project
     backend file-name (remote-route-link route) route)))

(defun remote-backend-client-file-name (route file-name)
  "Return FILE-NAME as a directly client-accessible path on ROUTE.
Nil means that client-side programs cannot access the target file namespace.
This is a placement capability, not a local-versus-remote target test."
  (when-let* ((backend (remote-route-backend route))
              (function (remote-backend-client-file-function backend)))
    (funcall function file-name route)))

(cl-defun remote-backend-prepare-execution
    (route context command environment
           &key logical-directory metadata)
  "Prepare a process execution for ROUTE.

COMMAND is a list of target-native strings and ENVIRONMENT is the resolved
environment representation supplied by the process layer.  LOGICAL-DIRECTORY
defaults to CONTEXT's workspace.  The returned execution always contains both
logical and physical working directories."
  (let* ((backend (or (remote-route-backend route)
                      (error "No backend registered for route %S" route)))
         (logical-directory
          (or logical-directory
              (remote-context-workspace-root context)))
         (physical-directory
          (and logical-directory
               (remote-backend-project-file-name
                route logical-directory)))
         (execution
          (remote-backend-execution-create
           :backend-id (remote-backend-id backend)
           :route route
           :context context
           :logical-directory logical-directory
           :physical-directory physical-directory
           :command (copy-sequence command)
           :environment environment
           :metadata
           (plist-put
            (copy-sequence metadata)
            :program-form
            (remote-backend-program-form backend)))))
    (if-let* ((function (remote-backend-prepare-function backend)))
        (or (funcall function execution) execution)
      execution)))

(defun remote-backend--default-process-plan (execution arguments)
  "Return the native Emacs process plan for EXECUTION and ARGUMENTS."
  (remote-backend-process-plan-create
   :arguments
   (if (plist-member arguments :file-handler)
       (copy-sequence arguments)
     (plist-put (copy-sequence arguments) :file-handler t))
   :default-directory
   (remote-backend-execution-physical-directory execution)
   :stderr-mode 'native))

(defun remote-backend-prepare-process
    (route execution arguments environment)
  "Return a backend-neutral async process plan for ROUTE.
EXECUTION is a `remote-backend-execution', ARGUMENTS is the official
`make-process' plist, and ENVIRONMENT is the resolved target override alist.
Legacy link plugins receive the default projected Emacs plan."
  (let* ((backend (remote-route-backend route))
         (function
          (and backend
               (remote-backend-prepare-process-function backend)))
         (plan
          (if function
              (funcall function execution arguments environment)
            (remote-backend--default-process-plan execution arguments))))
    (unless (remote-backend-process-plan-p plan)
      (error "Backend %s returned an invalid process plan: %S"
             (and backend (remote-backend-id backend)) plan))
    (unless (plist-get
             (remote-backend-process-plan-arguments plan)
             :command)
      (error "Backend process plan has no command: %S" plan))
    plan))

(defun remote-backend-stdio-bridge-command (execution)
  "Return client-local stdio bridge argv for EXECUTION.
The selected backend owns every placement distinction.  Callers must not
branch on target IDs or physical path syntax."
  (unless (remote-backend-execution-p execution)
    (error "Not a backend execution: %S" execution))
  (let* ((route (remote-backend-execution-route execution))
         (backend (remote-route-backend route))
         (function
          (and backend
               (remote-backend-stdio-bridge-function backend))))
    (unless function
      (signal
       'remote-backend-unsupported
       (list
        (format "Backend %s has no client stdio bridge"
                (if backend
                    (remote-backend-id backend)
                  (remote-route-link-plugin-id route))))))
    (let ((command (funcall function execution)))
      (unless (and (listp command)
                   (stringp (car command)))
        (error "Backend %s returned invalid stdio bridge argv: %S"
               (remote-backend-id backend) command))
      command)))

(defun remote-backend-default-classify-error (error &optional phase)
  "Return a transport-neutral classification for ERROR during PHASE."
  (let ((type (car-safe error))
        (message (downcase (error-message-string error))))
    (cond
     ((eq type 'remote-backend-unsupported)
      (list :scope 'backend :phase phase :retryable t :error error))
     ((or
       (eq type 'remote-transport-error)
       (and
       (memq type '(file-error remote-file-error))
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
        message)))
      (list :scope 'transport :phase phase :retryable t :error error))
     (t
      (list :scope 'operation :phase phase :retryable nil :error error)))))

(defun remote-backend-classify-error (route error &optional phase)
  "Return structured classification for ERROR on ROUTE during PHASE."
  (let* ((backend (remote-route-backend route))
         (function
          (and backend
               (remote-backend-classify-error-function backend))))
    (or (and function (funcall function error phase))
        (remote-backend-default-classify-error
         error (or phase 'unknown)))))

(defun remote-backend-describe (backend)
  "Return a descriptive plist for BACKEND."
  (let* ((backend
          (if (remote-backend-p backend)
              backend
            (or (remote-get-backend backend)
                (error "Unknown remote backend: %S" backend))))
         (function (remote-backend-describe-function backend)))
    (append
     (list :id (remote-backend-id backend)
           :capabilities
           (copy-sequence (remote-backend-capabilities backend))
           :program-form (remote-backend-program-form backend))
     (and function (funcall function)))))

(provide 'remote-backend-core)
;;; remote-backend-core.el ends here
