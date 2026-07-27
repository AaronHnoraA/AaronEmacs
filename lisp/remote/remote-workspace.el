;;; remote-workspace.el --- Remote workspace lifecycle and resources -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A workspace is the long-lived development unit above transport sessions.
;; It owns terminals, tasks, forwards, helper services, and workspace-scoped
;; environment state while file buffers retain their ordinary Emacs lifetime.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-session)
(require 'remote-channel)
(require 'remote-environment)
(require 'remote-service)

(cl-defstruct (remote-workspace-resource
               (:constructor remote-workspace-resource-create))
  id kind value close-function recovery-function recovery-policy
  state attempts error metadata)

(cl-defstruct (remote-workspace
               (:constructor remote-workspace-create))
  id key target-id workspace-id root context
  state routes primary-route resources services environment settings
  opened-at last-used-at error metadata)

(defvar remote-workspaces (make-hash-table :test #'equal)
  "Open logical workspaces keyed by target and canonical root.")

(defvar remote-workspace-open-hook nil
  "Hook run with the new workspace dynamically bound.")

(defvar remote-workspace-close-hook nil
  "Hook run before a workspace's owned resources are closed.")

(defvar remote-current-workspace nil
  "Dynamically active `remote-workspace'.")

(defvar remote-workspace--resource-counter 0)

(defcustom remote-workspace-auto-reconnect t
  "Whether transport failures schedule workspace reconnection."
  :type 'boolean
  :group 'remote)

(defcustom remote-workspace-reconnect-delays '(1 2 4)
  "Seconds to wait before automatic transport reconnect attempts."
  :type '(repeat number)
  :group 'remote)

(defun remote-workspace--resource-policy (kind metadata)
  "Return recovery policy for resource KIND and METADATA."
  (or (plist-get metadata :recovery)
      (if (memq kind '(environment service forward watch lsp channel))
          'auto
        'manual)))

(defun remote-workspace--context (value)
  "Return VALUE as a logical context."
  (cond
   ((remote-context-p value) value)
   ((remote-workspace-p value)
    (remote-workspace-context value))
   ((stringp value) (remote-context value))
   (t (remote-context))))

(defun remote-workspace--identity (context)
  "Return (ID KEY ROOT) for CONTEXT."
  (let* ((target-id (remote-context-target-id context))
         (root
          (file-name-as-directory
           (or (remote-context-workspace-root context)
               (remote-make-file-name
                target-id
                (file-name-directory
                 (remote-context-localname context))))))
         (workspace-id
          (or
           (remote-context-workspace-id context)
           (format
            "workspace-%s"
            (substring (secure-hash 'sha1 root) 0 12))))
         (id (format "%s@%s" target-id workspace-id)))
    (list id (list target-id root) root workspace-id)))

(defun remote-workspace-context-id (&optional context)
  "Return the stable workspace ID implied by CONTEXT.
CONTEXT accepts the same values as `remote-workspace-open'.  This is an
I/O-free identity operation and does not open a workspace or a connection."
  (car
   (remote-workspace--identity
    (remote-workspace--context context))))

(defun remote-get-workspace (value)
  "Return an open workspace identified by VALUE.
VALUE may be a workspace object, workspace ID, context, or logical path."
  (cond
   ((remote-workspace-p value) value)
   ((stringp value)
    (or
     (seq-find
      (lambda (workspace)
        (equal (remote-workspace-id workspace) value))
      (hash-table-values remote-workspaces))
     (condition-case nil
         (let* ((context (remote-context value))
                (identity (remote-workspace--identity context)))
           (gethash (nth 1 identity) remote-workspaces))
       (error nil))))
   ((remote-context-p value)
    (let ((identity (remote-workspace--identity value)))
      (gethash (nth 1 identity) remote-workspaces)))))

(defun remote-workspace-live-p (workspace)
  "Return non-nil when WORKSPACE is open."
  (and (remote-workspace-p workspace)
       (eq (remote-workspace-state workspace) 'open)))

(cl-defun remote-workspace-open
    (&optional context
               &key (connect t) (adapter "process")
               (capability 'process-sync) constraints
               load-environment force)
  "Open and return the logical workspace for CONTEXT.
With CONNECT, acquire a route for CAPABILITY through ADAPTER.  With
LOAD-ENVIRONMENT, build its environment capsule.  FORCE replaces an existing
workspace object and closes resources owned by the old one."
  (let* ((context (remote-workspace--context context))
         (identity (remote-workspace--identity context))
         (id (nth 0 identity))
         (key (nth 1 identity))
         (root (nth 2 identity))
         (workspace-id (nth 3 identity))
         (existing (gethash key remote-workspaces)))
    (cond
     ((and existing
           (not force)
           (remote-workspace-live-p existing))
      (setf (remote-workspace-last-used-at existing) (current-time))
      existing)
     (t
      (when existing
        (remote-workspace-close existing 'reopen))
      (let* ((route
              (and connect
                   (remote-resolve
                    adapter capability context constraints)))
             (_session
              (and route
                   (remote-session-acquire route context)))
             (now (current-time))
             (workspace
              (remote-workspace-create
               :id id
               :key key
               :target-id (remote-context-target-id context)
               :workspace-id workspace-id
               :root root
               :context context
               :state 'opening
               :routes (and route (list route))
               :primary-route route
               :settings
               (let ((target
                      (remote-get-target
                       (remote-context-target-id context))))
                 (list
                  :target
                  (and target
                       (remote-target-preferences target))
                  :workspace (remote-context-source context)))
               :opened-at now
               :last-used-at now)))
        (condition-case error
            (progn
              (when load-environment
                (setf (remote-workspace-environment workspace)
                      (remote-environment-ensure context)))
              (setf (remote-workspace-state workspace) 'open)
              (puthash key workspace remote-workspaces)
              (let ((remote-current-workspace workspace))
                (run-hooks 'remote-workspace-open-hook))
              (remote-log
               'workspace-open
               :workspace id
               :target (remote-workspace-target-id workspace)
               :root root)
              workspace)
          (error
           (setf (remote-workspace-state workspace) 'failed
                 (remote-workspace-error workspace) error)
           (signal (car error) (cdr error)))))))))

(defalias 'remote-workspace-ensure #'remote-workspace-open)

(defun remote-workspace-route
    (workspace adapter capability &optional constraints)
  "Resolve and remember a route for WORKSPACE."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (error "Unknown remote workspace: %S" workspace)))
         (route
          (remote-resolve
           adapter capability
           (remote-workspace-context workspace)
           constraints)))
    (remote-session-acquire
     route (remote-workspace-context workspace))
    (setf (remote-workspace-routes workspace)
          (cons
           route
           (seq-remove
            (lambda (known)
              (equal
               (remote-connection-route-key known)
               (remote-connection-route-key route)))
            (remote-workspace-routes workspace)))
          (remote-workspace-last-used-at workspace)
          (current-time))
    route))

(defun remote-workspace-register-resource
    (workspace kind value &optional close-function metadata)
  "Register an owned resource VALUE of KIND in WORKSPACE."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (error "Unknown remote workspace: %S" workspace)))
         (resource
          (remote-workspace-resource-create
           :id
           (format "%s/resource-%d"
                   (remote-workspace-id workspace)
                   (cl-incf remote-workspace--resource-counter))
           :kind kind
           :value value
           :close-function close-function
           :recovery-function (plist-get metadata :recover)
           :recovery-policy
           (remote-workspace--resource-policy kind metadata)
           :state 'open
           :attempts 0
           :metadata metadata)))
    (setf (remote-workspace-resources workspace)
          (cons resource
                (remote-workspace-resources workspace)))
    resource))

(cl-defun remote-workspace-register-recoverable-resource
    (workspace kind value &key close recover metadata
               (recovery 'auto))
  "Register recoverable VALUE of KIND in WORKSPACE.
RECOVER is called with the resource and workspace after transport recovery and
must return the replacement value.  CLOSE follows the ordinary resource close
contract."
  (remote-workspace-register-resource
   workspace kind value close
   (append
    (list :recover recover :recovery recovery)
    metadata)))

(defun remote-workspace--mark-terminals-disconnected (workspace reason)
  "Mark terminal resources in WORKSPACE disconnected because of REASON."
  (dolist (resource (remote-workspace-resources workspace))
    (when (eq (remote-workspace-resource-kind resource) 'terminal)
      (setf (remote-workspace-resource-state resource) 'disconnected
            (remote-workspace-resource-error resource) reason)
      (when (fboundp 'remote-terminal-mark-disconnected)
        (remote-terminal-mark-disconnected
         (remote-workspace-resource-value resource) reason)))))

(defun remote-workspace-recover-resource (workspace resource)
  "Recover one automatic RESOURCE owned by WORKSPACE."
  (when (and
         (eq (remote-workspace-resource-recovery-policy resource) 'auto)
         (remote-workspace-resource-recovery-function resource))
    (let ((old (remote-workspace-resource-value resource)))
      (condition-case error
          (let ((_closed
                 (when (remote-workspace-resource-close-function resource)
                   (funcall
                    (remote-workspace-resource-close-function resource)
                    old 'transport-recovery)))
                (value
                 (funcall
                  (remote-workspace-resource-recovery-function resource)
                  resource workspace)))
            (setf (remote-workspace-resource-value resource) value
                  (remote-workspace-resource-state resource) 'open
                  (remote-workspace-resource-error resource) nil
                  (remote-workspace-resource-attempts resource)
                  (1+ (remote-workspace-resource-attempts resource)))
            (when (eq (remote-workspace-resource-kind resource) 'service)
              (setf
               (remote-workspace-services workspace)
               (cons
                value
                (delq old
                      (remote-workspace-services workspace)))))
            value)
        (error
         (setf (remote-workspace-resource-state resource) 'failed
               (remote-workspace-resource-error resource) error
               (remote-workspace-resource-attempts resource)
               (1+ (remote-workspace-resource-attempts resource)))
         (remote-log
          'workspace-resource-recovery-error
          :workspace (remote-workspace-id workspace)
          :resource (remote-workspace-resource-id resource)
          :kind (remote-workspace-resource-kind resource)
          :error (error-message-string error))
         nil)))))

(defun remote-workspace-close-resource (workspace resource &optional reason)
  "Close RESOURCE owned by WORKSPACE."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (error "Unknown remote workspace: %S" workspace)))
         (resource
          (if (remote-workspace-resource-p resource)
              resource
            (seq-find
             (lambda (candidate)
               (equal
                (remote-workspace-resource-id candidate)
                (format "%s" resource)))
             (remote-workspace-resources workspace))))
         (value
          (and resource
               (remote-workspace-resource-value resource))))
    (when resource
      (setf (remote-workspace-resources workspace)
            (delq resource
                  (remote-workspace-resources workspace)))
      (condition-case error
          (cond
           ((remote-workspace-resource-close-function resource)
            (funcall
             (remote-workspace-resource-close-function resource)
             value reason))
           ((processp value)
            (when (process-live-p value)
              (delete-process value)))
           ((remote-forward-p value)
            (remote-close-channel value))
           ((functionp value)
            (funcall value)))
        (error
         (remote-log
          'workspace-resource-close-error
          :workspace (remote-workspace-id workspace)
          :resource (remote-workspace-resource-id resource)
          :error (error-message-string error))))
      resource)))

(cl-defun remote-workspace-ensure-service
    (workspace service &key provision force)
  "Ensure SERVICE in WORKSPACE and register its shared lifetime."
  (let* ((workspace
          (or (remote-get-workspace workspace)
              (error "Unknown remote workspace: %S" workspace)))
         (instance
          (remote-service-ensure
           service workspace
           :provision provision
           :force force)))
    (unless
        (seq-find
         (lambda (known)
           (eq known instance))
         (remote-workspace-services workspace))
      (push instance (remote-workspace-services workspace))
      (remote-workspace-register-resource
       workspace 'service instance
       (lambda (value reason)
         (remote-service-release value nil reason))
       (list
        :recovery 'auto
        :recover
        (lambda (_resource owner)
          (remote-service-ensure
           (remote-service-instance-service-id instance)
           owner :provision provision :force t)))))
    instance))

(defun remote-workspace-refresh-environment (workspace &optional force)
  "Refresh and return WORKSPACE's environment capsule."
  (let ((workspace
         (or (remote-get-workspace workspace)
             (error "Unknown remote workspace: %S" workspace))))
    (setf
     (remote-workspace-environment workspace)
     (remote-environment-resolve
      (remote-workspace-context workspace)
      force))))

(defun remote-workspace--transport-error-p (workspace error)
  "Return whether ERROR represents transport loss for WORKSPACE."
  (let ((route (or (remote-workspace-primary-route workspace)
                   (car (remote-workspace-routes workspace)))))
    (and route
         (eq
          (plist-get
           (remote-backend-classify-error route error 'reconnect)
           :scope)
          'transport))))

(defun remote-workspace--recover-after-transport (workspace)
  "Restore automatic WORKSPACE resources after its routes reconnect."
  (when (remote-workspace-environment workspace)
    (condition-case error
        (remote-workspace-refresh-environment workspace t)
      (error
       (remote-log
        'workspace-environment-recovery-error
        :workspace (remote-workspace-id workspace)
        :error (error-message-string error)))))
  (dolist (resource
           (reverse
            (copy-sequence
             (remote-workspace-resources workspace))))
    (unless (eq (remote-workspace-resource-kind resource) 'terminal)
      (remote-workspace-recover-resource workspace resource)))
  (if (seq-some
       (lambda (resource)
         (eq (remote-workspace-resource-state resource) 'failed))
       (remote-workspace-resources workspace))
      'degraded
    'open))

(defun remote-workspace--reconnect-once (workspace)
  "Perform one transport reconnect attempt for WORKSPACE."
  (dolist (route (remote-workspace-routes workspace))
    (remote-session-invalidate route t 'workspace-reconnect)
    (remote-session-acquire
     route (remote-workspace-context workspace)))
  (setf (remote-workspace-state workspace)
        (remote-workspace--recover-after-transport workspace)
        (remote-workspace-error workspace) nil
        (remote-workspace-last-used-at workspace)
        (current-time))
  workspace)

(defun remote-workspace-reconnect (workspace)
  "Reconnect WORKSPACE with bounded transport backoff.
Automatic resources are recreated.  Terminals remain disconnected until
`remote-terminal-restart' is invoked explicitly."
  (interactive (list (remote-get-workspace (remote-context))))
  (let ((workspace
         (or (remote-get-workspace workspace)
             (error "Unknown remote workspace: %S" workspace)))
        (delays (copy-sequence remote-workspace-reconnect-delays))
        done last-error)
    (setf (remote-workspace-state workspace) 'reconnecting)
    (remote-workspace--mark-terminals-disconnected
     workspace 'workspace-reconnect)
    (while (not done)
      (condition-case error
          (progn
            (remote-workspace--reconnect-once workspace)
            (setq done t))
        (error
         (setq last-error error)
         (if (and delays
                  (remote-workspace--transport-error-p workspace error))
             (sleep-for (pop delays))
           (setf (remote-workspace-state workspace) 'failed
                 (remote-workspace-error workspace) error)
           (signal (car error) (cdr error))))))
    (or (and done workspace)
        (signal (car last-error) (cdr last-error)))))

(defun remote-workspace--auto-reconnect-step (workspace delays)
  "Attempt automatic reconnect of WORKSPACE using remaining DELAYS."
  (when (and (remote-workspace-p workspace)
             (memq (remote-workspace-state workspace)
                   '(disconnected reconnecting)))
    (setf (remote-workspace-state workspace) 'reconnecting
          (remote-workspace-metadata workspace)
          (plist-put
           (remote-workspace-metadata workspace)
           :reconnect-timer nil))
    (condition-case error
        (remote-workspace--reconnect-once workspace)
      (error
       (if (and delays
                (remote-workspace--transport-error-p workspace error))
           (let ((timer
                  (run-at-time
                   (car delays) nil
                   #'remote-workspace--auto-reconnect-step
                   workspace (cdr delays))))
             (setf
              (remote-workspace-state workspace) 'disconnected
              (remote-workspace-error workspace) error
              (remote-workspace-metadata workspace)
              (plist-put
               (remote-workspace-metadata workspace)
               :reconnect-timer timer)))
         (setf (remote-workspace-state workspace) 'failed
               (remote-workspace-error workspace) error))))))

(defun remote-workspace-handle-transport-failure (route error)
  "Mark workspaces using ROUTE disconnected and schedule recovery."
  (maphash
   (lambda (_key workspace)
     (when (seq-some
            (lambda (known)
              (equal
               (remote-route-pipeline-id known)
               (remote-route-pipeline-id route)))
            (remote-workspace-routes workspace))
       (setf (remote-workspace-state workspace) 'disconnected
             (remote-workspace-error workspace) error)
       (remote-workspace--mark-terminals-disconnected workspace error)
       (when (and remote-workspace-auto-reconnect
                  (not
                   (plist-get
                    (remote-workspace-metadata workspace)
                    :reconnect-timer)))
         (let ((timer
                (run-at-time
                 0 nil #'remote-workspace--auto-reconnect-step
                 workspace
                 (copy-sequence remote-workspace-reconnect-delays))))
           (setf
            (remote-workspace-metadata workspace)
            (plist-put
             (remote-workspace-metadata workspace)
             :reconnect-timer timer))))))
   remote-workspaces))

(defun remote-workspace-close (workspace &optional reason)
  "Close resources owned by WORKSPACE and record REASON."
  (interactive (list (remote-get-workspace (remote-context))))
  (let ((workspace (remote-get-workspace workspace)))
    (when workspace
      (unless (eq (remote-workspace-state workspace) 'closed)
        (setf (remote-workspace-state workspace) 'closing)
        (when-let* ((timer
                     (plist-get
                      (remote-workspace-metadata workspace)
                      :reconnect-timer)))
          (cancel-timer timer))
        (let ((remote-current-workspace workspace))
          (run-hooks 'remote-workspace-close-hook))
        (dolist
            (resource
             (copy-sequence
              (remote-workspace-resources workspace)))
          (remote-workspace-close-resource
           workspace resource reason))
        (remhash (remote-workspace-key workspace)
                 remote-workspaces)
        (setf (remote-workspace-state workspace) 'closed
              (remote-workspace-error workspace) reason)
        (remote-log
         'workspace-close
         :workspace (remote-workspace-id workspace)
         :target (remote-workspace-target-id workspace)
         :reason
         (cond
          ((null reason) nil)
          ((symbolp reason) (symbol-name reason))
          ((listp reason) (error-message-string reason))
          (t (format "%s" reason)))))
      workspace)))

(defun remote-workspace-list ()
  "Return stable summaries of open workspaces."
  (let (result)
    (maphash
     (lambda (_key workspace)
       (push
        (list
         :id (remote-workspace-id workspace)
         :target (remote-workspace-target-id workspace)
         :root (remote-workspace-root workspace)
         :state (remote-workspace-state workspace)
         :resources
         (length (remote-workspace-resources workspace))
         :services
         (mapcar
          #'remote-service-instance-service-id
          (remote-workspace-services workspace)))
        result))
     remote-workspaces)
    (sort
     result
     (lambda (left right)
       (string-lessp
        (plist-get left :id)
        (plist-get right :id))))))

(defun remote-workspace-clear (&optional reason)
  "Close every open workspace."
  (let (workspaces)
    (maphash
     (lambda (_key workspace) (push workspace workspaces))
     remote-workspaces)
    (dolist (workspace workspaces)
      (remote-workspace-close workspace (or reason 'clear)))
    (length workspaces)))

(add-hook 'remote-transport-failure-hook
          #'remote-workspace-handle-transport-failure)

(provide 'remote-workspace)
;;; remote-workspace.el ends here
