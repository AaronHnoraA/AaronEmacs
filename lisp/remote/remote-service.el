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
(require 'remote-core)
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

(cl-defun remote-service-ensure (service workspace &key provision force)
  "Return a live SERVICE instance for WORKSPACE.
SERVICE is an ID or service object.  With PROVISION, install a missing service
when its target is trusted.  FORCE ignores an existing live instance."
  (let* ((service
          (if (remote-service-p service)
              service
            (or (remote-get-service service)
                (error "Unknown remote service: %S" service))))
         (key (remote-service--instance-key service workspace))
         (existing (gethash key remote-service-instances)))
    (if (and (not force)
             (remote-service-instance-live-p existing))
        (progn
          (setf (remote-service-instance-last-used-at existing)
                (current-time)
                (remote-service-instance-use-count existing)
                (1+ (remote-service-instance-use-count existing)))
          existing)
      (when existing
        (remote-service-stop existing 'restart))
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
        (let* ((starter (remote-service-start-function service))
               (started
                (and starter
                     (funcall starter
                              (remote-workspace-context workspace)
                              probe)))
               (started-plist
                (and (listp started)
                     (keywordp (car started))
                     started))
               (now (current-time))
               (instance
                (remote-service-instance-create
                 :key key
                 :service-id (remote-service-id service)
                 :workspace-id (remote-workspace-id workspace)
                 :target-id (remote-workspace-target-id workspace)
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
                 :state 'running
                 :started-at now
                 :last-used-at now
                 :use-count 1
                 :metadata
                 (append
                  (plist-get probe :metadata)
                  (plist-get started-plist :metadata)))))
          (puthash key instance remote-service-instances)
          (remote-log
           'service-start
           :service (remote-service-id service)
           :target (remote-workspace-target-id workspace)
           :workspace (remote-workspace-id workspace)
           :version (remote-service-instance-version instance))
          instance)))))

(defun remote-service-stop (instance &optional reason)
  "Stop service INSTANCE and record REASON."
  (when (remote-service-instance-p instance)
    (let ((service
           (remote-get-service
            (remote-service-instance-service-id instance))))
      (remhash
       (remote-service-instance-key instance)
       remote-service-instances)
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
            :error (error-message-string error)))))
      (setf (remote-service-instance-state instance) 'stopped)
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
