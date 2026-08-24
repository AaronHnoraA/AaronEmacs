;;; remote-service.el --- Workspace-side remote services and tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; VS Code Remote installs a workspace-side server which hosts remote
;; extensions.  Emacs does not require one monolithic server, but it still
;; needs one lifecycle contract for optional helpers, language servers,
;; debug adapters, indexers, and other target-side tools.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)

(declare-function remote-workspace-id "remote-workspace" (workspace))
(declare-function remote-workspace-target-id "remote-workspace" (workspace))
(declare-function remote-workspace-context "remote-workspace" (workspace))

(define-error 'remote-service-error "Remote workspace service failed")
(define-error 'remote-service-untrusted
              "Remote service provisioning requires a trusted target"
              'remote-service-error)

(defconst remote-service-capabilities
  '(files processes watch terminal search scm lsp debug test
    tasks channels provision)
  "Capabilities advertised by workspace-side services.")

(cl-defstruct (remote-service
               (:constructor remote-service-create))
  id capabilities scope trusted-only
  probe-function provision-function start-function
  live-function stop-function describe-function)

(cl-defstruct (remote-service-instance
               (:constructor remote-service-instance-create))
  key service-id workspace-id target-id
  capabilities version handle state
  started-at last-used-at use-count error metadata)

(defvar remote-services (make-hash-table :test #'equal)
  "Registered remote service definitions.")

(defvar remote-service-instances (make-hash-table :test #'equal)
  "Running remote service instances.")

(defcustom remote-service-auto-provision nil
  "Whether missing services may be provisioned automatically.
Provisioning still requires a trusted target."
  :type 'boolean
  :group 'remote)

(defun remote-service--safe-relative-path-p (path &optional allow-dot)
  "Return non-nil when PATH is a safe relative archive path.
When ALLOW-DOT is non-nil, the spelling `.' is accepted."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (or allow-dot (not (equal path ".")))
       (not (member ".." (split-string path "/" t)))))

(defun remote-service--safe-install-directory-p (directory)
  "Return non-nil when target-native DIRECTORY is a safe install leaf."
  (let ((directory (directory-file-name directory)))
    (and (file-name-absolute-p directory)
         (not
          (member
           directory
           '("/" "/bin" "/boot" "/dev" "/etc" "/home" "/lib"
             "/lib64" "/opt" "/proc" "/root" "/run" "/sbin"
             "/srv" "/sys" "/tmp" "/usr" "/var" "/Users"))))))

(defun remote-service--directory-ready-p
    (directory ready-file ready-kind context adapter)
  "Return whether DIRECTORY contains READY-FILE of READY-KIND.
CONTEXT and ADAPTER keep this probe inside the public process boundary, so it
does not depend on whether the logical `/fs:' file handler is installed."
  (let* ((native-directory
          (remote-file-local-name (directory-file-name directory)))
         (path
          (if (equal ready-file ".")
              native-directory
            (concat (file-name-as-directory native-directory) ready-file)))
         (flag
          (pcase ready-kind
            ('executable "-x")
            ('directory "-d")
            ('file "-f")
            (_ "-e"))))
    (condition-case nil
        (zerop
         (remote-exec-result-status
          (remote-exec
           "test" :args (list flag path)
           :context context :adapter adapter
           :filesystem-effects 'none)))
      (error nil))))

(cl-defun remote-service-provision-directory
    (service source install-directory
     &key context (adapter "service") ready-file
     (ready-kind 'exists) (payload-directory ".") prepare)
  "Provision a client-local directory as a versioned target-side SERVICE.

SOURCE is packed on the client, transferred through the Remote bulk-copy
boundary, extracted into a target-side staging directory, and atomically
published as INSTALL-DIRECTORY.  INSTALL-DIRECTORY may be native, logical
`/fs:', or physical TRAMP syntax; it is normalized to the CONTEXT target.

READY-FILE is a safe path below the installation and READY-KIND is one of
`exists', `file', `directory', or `executable'.  PAYLOAD-DIRECTORY places the
archive below that relative staging subdirectory.  PREPARE, when non-nil, is
called with CONTEXT and the logical staging directory after extraction and
before readiness validation.  It is the language/service-specific boundary
for small steps such as creating a launcher or changing its mode.

Provisioning is allowed only for trusted targets.  Return the canonical
logical installation directory."
  (unless (and (stringp source)
               (file-name-absolute-p source)
               (not (file-remote-p source))
               (file-directory-p source))
    (error "Provision source must be a client-local directory: %S" source))
  (unless (remote-service--safe-relative-path-p ready-file t)
    (error "Invalid provisioning readiness path: %S" ready-file))
  (unless (memq ready-kind '(exists file directory executable))
    (error "Invalid provisioning readiness kind: %S" ready-kind))
  (unless (remote-service--safe-relative-path-p payload-directory t)
    (error "Invalid provisioning payload directory: %S" payload-directory))
  (unless (or (null prepare) (functionp prepare))
    (error "Provision prepare hook is not callable: %S" prepare))
  (let* ((context
          (cond
           ((remote-context-p context) context)
           (context (remote-context context))
           (t (remote-context install-directory))))
         (target-id (remote-context-target-id context))
         (target (remote-get-target target-id))
         (install-directory
          (file-name-as-directory
           (remote-expand-file-name install-directory nil context)))
         (native-install
          (remote-file-local-name
           (directory-file-name install-directory))))
    (unless (and target (remote-target-trusted target))
      (signal 'remote-service-untrusted (list service target-id)))
    (unless (equal (remote-file-name-target install-directory) target-id)
      (error "Provision destination %s does not belong to target %s"
             install-directory target-id))
    (unless (remote-service--safe-install-directory-p native-install)
      (error "Refusing unsafe service installation directory: %s"
             native-install))
    (if (remote-service--directory-ready-p
         install-directory ready-file ready-kind context adapter)
        install-directory
      (let* ((service-name
              (replace-regexp-in-string
               "[^[:alnum:]._-]+" "-" (remote-normalize-id service)))
             (token
              (substring
               (secure-hash
                'sha1
                (format "%s:%s:%s:%s"
                        service-name native-install (emacs-pid)
                        (float-time)))
               0 16))
             (parent
              (file-name-directory
               (directory-file-name install-directory)))
             (staging
              (file-name-as-directory
               (concat (directory-file-name install-directory)
                       ".tmp." token)))
             (archive
              (make-temp-file
               (format "emacs-remote-%s-" service-name) nil ".tar.gz"))
             (target-archive
              (expand-file-name
               (format ".emacs-remote-%s-%s.tar.gz" service-name token)
               parent))
             (native-parent
              (remote-file-local-name (directory-file-name parent)))
             (native-staging
              (remote-file-local-name (directory-file-name staging)))
             (native-archive (remote-file-local-name target-archive)))
        (unwind-protect
            (progn
              (unless
                  (zerop
                   (call-process
                    "tar" nil nil nil "-czf" archive "-C" source "."))
                (error "Could not package local %s service bundle" service))
              (remote-exec
               "mkdir" :args (list "-p" native-parent)
               :context context :adapter adapter :check t)
              (remote-copy-file-to-target
               archive target-archive
               :context context :adapter adapter :overwrite t)
              (remote-exec
               "sh"
               :args
               (list
                "-c"
                (concat
                 "set -eu\n"
                 "archive=$1\n"
                 "staging=$2\n"
                 "payload=$3\n"
                 "rm -rf -- \"$staging\"\n"
                 "mkdir -p -- \"$staging/$payload\"\n"
                 "tar -xzf \"$archive\" -C \"$staging/$payload\"\n")
                (format "%s-extract" service-name)
                native-archive native-staging payload-directory)
               :context context :adapter adapter :check t)
              (when prepare
                (funcall prepare context staging))
              (unless
                  (remote-service--directory-ready-p
                   staging ready-file ready-kind context adapter)
                (signal
                 'remote-service-error
                 (list
                  (format
                   "Staged %s bundle does not contain ready %s"
                   service ready-file))))
              ;; A versioned cache may survive an interrupted older install.
              ;; Replace only this validated leaf, never its cache parent.
              (remote-exec
               "sh"
               :args
               (list
                "-c"
                (concat
                 "set -eu\n"
                 "staging=$1\n"
                 "install=$2\n"
                 "if test -e \"$install\"; then rm -rf -- \"$install\"; fi\n"
                 "mv -- \"$staging\" \"$install\"\n")
                (format "%s-publish" service-name)
                native-staging native-install)
               :context context :adapter adapter :check t)
              (unless
                  (remote-service--directory-ready-p
                   install-directory ready-file ready-kind context adapter)
                (signal
                 'remote-service-error
                 (list
                  (format
                   "Provisioned %s service is not ready at %s"
                   service install-directory))))
              install-directory)
          (when (file-exists-p archive)
            (delete-file archive))
          (ignore-errors
            (remote-exec
             "rm" :args (list "-f" native-archive)
             :context context :adapter adapter :check t))
          (ignore-errors
            (remote-exec
             "rm" :args (list "-rf" native-staging)
             :context context :adapter adapter :check t)))))))

(defun remote-get-service (id)
  "Return registered service ID, or nil."
  (gethash (remote-normalize-id id t) remote-services))

(cl-defun remote-register-service
    (id &key capabilities (scope 'workspace) trusted-only
        probe provision start live stop describe)
  "Register a workspace-side service ID.
SCOPE is `workspace' or `target'.  PROBE must not modify the target.
PROVISION may install or update the service and is trust-gated.  START returns
an opaque handle or a plist containing `:handle', `:version', `:capabilities',
and `:metadata'."
  (unless (memq scope '(workspace target))
    (error "Invalid remote service scope: %S" scope))
  (let ((unknown
         (seq-difference capabilities remote-service-capabilities)))
    (when unknown
      (error "Unknown remote service capabilities for %s: %S"
             id unknown)))
  (let* ((id (remote-normalize-id id))
         (service
          (remote-service-create
           :id id
           :capabilities (copy-sequence capabilities)
           :scope scope
           :trusted-only trusted-only
           :probe-function probe
           :provision-function provision
           :start-function start
           :live-function live
           :stop-function stop
           :describe-function describe)))
    (puthash id service remote-services)
    service))

(defun remote-service--instance-key (service workspace)
  "Return the instance key for SERVICE in WORKSPACE."
  (list
   (remote-service-id service)
   (pcase (remote-service-scope service)
     ('target (remote-workspace-target-id workspace))
     (_ (remote-workspace-id workspace)))))

(defun remote-service--probe (service workspace)
  "Probe SERVICE in WORKSPACE and normalize its result."
  (if-let* ((probe (remote-service-probe-function service)))
      (let ((value
             (funcall probe
                      (remote-workspace-context workspace))))
        (cond
         ((null value) nil)
         ((stringp value)
          (list :available t :version value))
         ((eq value t) '(:available t))
         ((listp value) value)
         (t (list :available t :metadata value))))
    '(:available t)))

(defun remote-service--trusted-p (workspace)
  "Return whether WORKSPACE belongs to a trusted target."
  (when-let* ((target
               (remote-get-target
                (remote-workspace-target-id workspace))))
    (remote-target-trusted target)))

(defun remote-service-instance-live-p (instance)
  "Return non-nil when service INSTANCE remains usable."
  (and
   (remote-service-instance-p instance)
   (eq (remote-service-instance-state instance) 'running)
   (when-let* ((service
                (remote-get-service
                 (remote-service-instance-service-id instance))))
     (if-let* ((live (remote-service-live-function service)))
         (condition-case nil
             (funcall live instance)
           (error nil))
       t))))

(cl-defun remote-service--ready-probe
    (service workspace &key provision)
  "Return an available probe result for SERVICE in WORKSPACE.
PROVISION has the same trust-gated meaning as in `remote-service-ensure'."
  (let ((probe (remote-service--probe service workspace)))
    (unless (plist-get probe :available)
      (when (or provision remote-service-auto-provision)
        (unless (remote-service--trusted-p workspace)
          (signal
           'remote-service-untrusted
           (list (remote-service-id service)
                 (remote-workspace-target-id workspace))))
        (if-let* ((installer
                   (remote-service-provision-function service)))
            (funcall installer
                     (remote-workspace-context workspace)
                     probe)
          (error "Service %s has no provision function"
                 (remote-service-id service)))
        (setq probe (remote-service--probe service workspace))))
    (unless (plist-get probe :available)
      (signal
       'remote-service-error
       (list
        (format "Service %s is unavailable"
                (remote-service-id service)))))
    (when (and (remote-service-trusted-only service)
               (not (remote-service--trusted-p workspace)))
      (signal
       'remote-service-untrusted
       (list (remote-service-id service)
             (remote-workspace-target-id workspace))))
    probe))

(defun remote-service--start-data (service workspace probe)
  "Start SERVICE in WORKSPACE and return normalized instance data."
  (let* ((starter (remote-service-start-function service))
         (started
          (and starter
               (funcall starter
                        (remote-workspace-context workspace)
                        probe)))
         (started-plist
          (and (listp started)
               (keywordp (car started))
               started)))
    (list
     :capabilities
     (or (plist-get started-plist :capabilities)
         (plist-get probe :capabilities)
         (copy-sequence
          (remote-service-capabilities service)))
     :version
     (or (plist-get started-plist :version)
         (plist-get probe :version))
     :handle
     (if started-plist
         (plist-get started-plist :handle)
       started)
     :metadata
     (append
      (plist-get probe :metadata)
      (plist-get started-plist :metadata)))))

(defun remote-service--apply-start-data (instance data)
  "Update INSTANCE in place from normalized start DATA."
  (let ((now (current-time)))
    (setf
     (remote-service-instance-capabilities instance)
     (plist-get data :capabilities)
     (remote-service-instance-version instance)
     (plist-get data :version)
     (remote-service-instance-handle instance)
     (plist-get data :handle)
     (remote-service-instance-metadata instance)
     (plist-get data :metadata)
     (remote-service-instance-state instance) 'running
     (remote-service-instance-started-at instance) now
     (remote-service-instance-last-used-at instance) now
     (remote-service-instance-error instance) nil)
    instance))

(defun remote-service--stop-handle (service instance reason)
  "Ask SERVICE to stop INSTANCE's current handle for REASON.
Registry ownership and reference counts are deliberately left unchanged."
  (when (and service
             (remote-service-stop-function service))
    (condition-case error
        (funcall
         (remote-service-stop-function service)
         instance reason)
      (error
       (setf (remote-service-instance-error instance) error)
       (remote-log
        'service-stop-error
        :service (remote-service-instance-service-id instance)
        :error (error-message-string error))))))

(cl-defun remote-service-restart
    (instance workspace &key provision (reason 'restart))
  "Restart INSTANCE for WORKSPACE without changing its object identity.
Target-scoped services can be referenced by several workspaces.  Mutating the
shared instance in place ensures every owner observes the replacement handle
and keeps the existing reference count."
  (unless (remote-service-instance-p instance)
    (error "Not a remote service instance: %S" instance))
  (let* ((service
          (or
           (remote-get-service
            (remote-service-instance-service-id instance))
           (error "Unknown remote service: %S"
                  (remote-service-instance-service-id instance))))
         ;; Probe and provision before stopping a still-usable handle.  A
         ;; failed precondition must not take a shared service away from its
         ;; other owners.
         (probe
          (remote-service--ready-probe
           service workspace :provision provision)))
    (setf (remote-service-instance-state instance) 'restarting)
    (remote-service--stop-handle service instance reason)
    (condition-case error
        (progn
          (remote-service--apply-start-data
           instance
           (remote-service--start-data service workspace probe))
          (puthash
           (remote-service-instance-key instance)
           instance remote-service-instances)
          (remote-log
           'service-restart
           :service (remote-service-id service)
           :target (remote-service-instance-target-id instance)
           :workspace (remote-workspace-id workspace)
           :uses (remote-service-instance-use-count instance))
          instance)
      (error
       (setf (remote-service-instance-state instance) 'failed
             (remote-service-instance-error instance) error)
       (remote-log
        'service-restart-error
        :service (remote-service-id service)
        :target (remote-service-instance-target-id instance)
        :error (error-message-string error))
       (signal (car error) (cdr error))))))

(cl-defun remote-service-ensure (service workspace &key provision force)
  "Return a live SERVICE instance for WORKSPACE.
SERVICE is an ID or service object.  With PROVISION, install a missing service
when its target is trusted.  FORCE restarts an existing shared instance in
place before acquiring the caller's reference."
  (let* ((service
          (if (remote-service-p service)
              service
            (or (remote-get-service service)
                (error "Unknown remote service: %S" service))))
         (key (remote-service--instance-key service workspace))
         (existing (gethash key remote-service-instances)))
    (if existing
        (progn
          (when (or force
                    (not
                     (remote-service-instance-live-p existing)))
            (remote-service-restart
             existing workspace
             :provision provision
             :reason
             (if force 'forced-restart 'stale-restart)))
          (setf (remote-service-instance-last-used-at existing)
                (current-time)
                (remote-service-instance-use-count existing)
                (1+ (remote-service-instance-use-count existing)))
          existing)
      (let* ((probe
              (remote-service--ready-probe
               service workspace :provision provision))
             (instance
              (remote-service-instance-create
               :key key
               :service-id (remote-service-id service)
               :workspace-id (remote-workspace-id workspace)
               :target-id (remote-workspace-target-id workspace)
               :use-count 1)))
        (condition-case error
            (progn
              (remote-service--apply-start-data
               instance
               (remote-service--start-data service workspace probe))
          (puthash key instance remote-service-instances)
          (remote-log
           'service-start
           :service (remote-service-id service)
           :target (remote-workspace-target-id workspace)
           :workspace (remote-workspace-id workspace)
           :version (remote-service-instance-version instance))
              instance)
          (error
           (setf (remote-service-instance-state instance) 'failed
                 (remote-service-instance-error instance) error)
           (signal (car error) (cdr error))))))))

(defun remote-service-stop (instance &optional reason)
  "Stop service INSTANCE and record REASON."
  (when (remote-service-instance-p instance)
    (let ((service
           (remote-get-service
            (remote-service-instance-service-id instance))))
      (when
          (eq
           (gethash
            (remote-service-instance-key instance)
            remote-service-instances)
           instance)
        (remhash
         (remote-service-instance-key instance)
         remote-service-instances))
      (remote-service--stop-handle service instance reason)
      (setf (remote-service-instance-state instance) 'stopped
            (remote-service-instance-handle instance) nil)
      instance)))

(defun remote-service-release (instance &optional force reason)
  "Release one reference to service INSTANCE.
With FORCE, stop it regardless of its remaining users."
  (when (remote-service-instance-p instance)
    (setf (remote-service-instance-use-count instance)
          (max 0 (1- (remote-service-instance-use-count instance))))
    (when (or force
              (zerop (remote-service-instance-use-count instance)))
      (remote-service-stop instance reason))
    instance))

(defun remote-service-list ()
  "Return stable summaries of running remote services."
  (let (result)
    (maphash
     (lambda (_key instance)
       (push
        (list
         :service (remote-service-instance-service-id instance)
         :target (remote-service-instance-target-id instance)
         :workspace (remote-service-instance-workspace-id instance)
         :state (remote-service-instance-state instance)
         :version (remote-service-instance-version instance)
         :capabilities
         (remote-service-instance-capabilities instance))
        result))
     remote-service-instances)
    (sort
     result
     (lambda (left right)
       (string-lessp
        (format "%s/%s"
                (plist-get left :workspace)
                (plist-get left :service))
        (format "%s/%s"
                (plist-get right :workspace)
                (plist-get right :service)))))))

(defun remote-service-clear (&optional reason)
  "Stop every remote service instance."
  (let (instances)
    (maphash
     (lambda (_key instance) (push instance instances))
     remote-service-instances)
    (dolist (instance instances)
      (remote-service-stop instance (or reason 'clear)))
    (length instances)))

(provide 'remote-service)
;;; remote-service.el ends here
