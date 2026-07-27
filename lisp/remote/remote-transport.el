;;; remote-transport.el --- Executable transport pipeline stages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A transport stage describes one reachability transformation such as an
;; address overlay, SSH hop, WSL boundary, or container boundary.  Stages are
;; opened from client to target and closed in reverse order.  Their runtime is
;; shared by all backend sessions using the same target/pipeline pair.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-pipeline)

(define-error 'remote-transport-error "Remote transport pipeline failed")
(define-error 'remote-transport-unsupported
              "Remote transport stage is unsupported"
              'remote-transport-error)
(define-error 'remote-pipeline-busy
              "Remote transport pipeline is already opening")
(define-error 'remote-pipeline-cancelled
              "Remote transport pipeline opening was cancelled")

(cl-defstruct (remote-endpoint
               (:constructor remote-endpoint-create))
  target-id host port user method hops attributes)

(cl-defstruct (remote-transport
               (:constructor remote-transport-create))
  id capabilities
  prepare-function connect-function live-function disconnect-function
  describe-function)

(cl-defstruct (remote-transport-result
               (:constructor remote-transport-result-create))
  endpoint handle metadata)

(cl-defstruct (remote-stage-runtime
               (:constructor remote-stage-runtime-create))
  stage transport-id input-endpoint output-endpoint
  handle state metadata opened-at)

(cl-defstruct (remote-pipeline-runtime
               (:constructor remote-pipeline-runtime-create))
  key pipeline-id route context stages endpoint
  state opened-at last-used-at use-count error metadata)

(defvar remote-transports (make-hash-table :test #'equal)
  "Registered transport stage implementations.")

(defvar remote-pipeline-runtime-pool (make-hash-table :test #'equal)
  "Live transport runtimes keyed by target and pipeline.")

(defvar remote-current-pipeline-runtime nil
  "Dynamically active `remote-pipeline-runtime'.")

(defun remote-transport--object-value (object key)
  "Return KEY from plist or alist OBJECT."
  (cond
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal)))))

(defun remote-get-transport (id)
  "Return transport stage implementation ID, or nil."
  (gethash (remote-normalize-id id t) remote-transports))

(cl-defun remote-register-transport
    (id &key capabilities prepare connect live disconnect describe)
  "Register transport stage implementation ID.
PREPARE transforms an endpoint without allocating resources.  CONNECT may
return a `remote-transport-result', an endpoint, or an opaque handle.  LIVE
and DISCONNECT own resources allocated by CONNECT."
  (let* ((id (remote-normalize-id id))
         (transport
          (remote-transport-create
           :id id
           :capabilities (copy-sequence capabilities)
           :prepare-function prepare
           :connect-function connect
           :live-function live
           :disconnect-function disconnect
           :describe-function describe)))
    (puthash id transport remote-transports)
    transport))

(defun remote-pipeline-route-key (route)
  "Return the shared transport runtime key for ROUTE."
  (list (remote-route-target-id route)
        (remote-route-link-id route)))

(defun remote-transport--endpoint-from-config (target-id config)
  "Create a target endpoint for TARGET-ID from CONFIG."
  (remote-endpoint-create
   :target-id target-id
   :host (remote-transport--object-value config 'host)
   :port (remote-transport--object-value config 'port)
   :user (remote-transport--object-value config 'user)
   :method (remote-transport--object-value config 'method)
   :attributes (copy-tree config)))

(defun remote-transport--overlay-endpoint (endpoint config)
  "Return a copy of ENDPOINT updated by CONFIG."
  (let ((result (copy-remote-endpoint endpoint)))
    (dolist (field '(host port user method))
      (when-let* ((value (remote-transport--object-value config field)))
        (pcase field
          ('host (setf (remote-endpoint-host result) value))
          ('port (setf (remote-endpoint-port result) value))
          ('user (setf (remote-endpoint-user result) value))
          ('method (setf (remote-endpoint-method result) value)))))
    (setf (remote-endpoint-attributes result)
          (append (copy-tree config)
                  (remote-endpoint-attributes result)))
    result))

(defun remote-transport--address-prepare (stage endpoint _runtime)
  "Apply address STAGE to ENDPOINT."
  (remote-transport--overlay-endpoint
   endpoint (remote-pipeline-stage-config stage)))

(defun remote-transport--hop-prepare (method stage endpoint _runtime)
  "Append a METHOD hop described by STAGE to ENDPOINT."
  (let* ((config (remote-pipeline-stage-config stage))
         (stage-host
          (remote-transport--object-value config 'host))
         (configured
          (if stage-host
              (remote-endpoint-create
               :target-id (remote-endpoint-target-id endpoint)
               :host stage-host
               :port (remote-transport--object-value config 'port)
               :user (remote-transport--object-value config 'user)
               :method (remote-transport--object-value config 'method)
               :hops (copy-sequence (remote-endpoint-hops endpoint))
               :attributes (copy-tree config))
            (remote-transport--overlay-endpoint endpoint config)))
         (method
          (or (remote-transport--object-value
               config 'method)
              method))
         (host (remote-endpoint-host configured)))
    (unless (and (stringp host) (not (string-empty-p host)))
      (signal
       'remote-transport-error
       (list
        (format "Transport stage %s has no host"
                (remote-pipeline-stage-id stage)))))
    (setf (remote-endpoint-method configured) method
          (remote-endpoint-hops configured)
          (append
           (remote-endpoint-hops endpoint)
           (list
            (remote-endpoint-create
             :target-id (remote-endpoint-target-id configured)
             :host host
             :port (remote-endpoint-port configured)
             :user (remote-endpoint-user configured)
             :method method
             :attributes
             (copy-tree (remote-endpoint-attributes configured))))))
    configured))

(defun remote-transport--normalize-result (value endpoint)
  "Normalize transport result VALUE using ENDPOINT as the default."
  (cond
   ((remote-transport-result-p value) value)
   ((remote-endpoint-p value)
    (remote-transport-result-create :endpoint value))
   (t
    (remote-transport-result-create
     :endpoint endpoint :handle value))))

(defun remote-pipeline-plan (pipeline)
  "Purely project PIPELINE stages and return their final endpoint.
Only transport PREPARE functions run; no connections or other resources are
allocated."
  (let ((endpoint
         (remote-transport--endpoint-from-config
          (remote-pipeline-target-id pipeline)
          (remote-pipeline-config pipeline))))
    (dolist (stage (remote-pipeline-stages pipeline))
      (let* ((transport-id
              (remote-normalize-id
               (remote-pipeline-stage-transport stage)))
             (transport
              (or
               (remote-get-transport transport-id)
               (signal
                'remote-transport-unsupported
                (list transport-id
                      (remote-pipeline-id pipeline)))))
             (prepare (remote-transport-prepare-function transport)))
        (when prepare
          (setq endpoint
                (or (funcall prepare stage endpoint nil)
                    endpoint)))))
    endpoint))

(defun remote-transport--disconnect-stage (stage-runtime runtime)
  "Close STAGE-RUNTIME belonging to RUNTIME."
  (when (eq (remote-stage-runtime-state stage-runtime) 'open)
    (when-let* ((transport
                 (remote-get-transport
                  (remote-stage-runtime-transport-id stage-runtime)))
                (disconnect
                 (remote-transport-disconnect-function transport)))
      (condition-case error
          (funcall disconnect stage-runtime runtime)
        (error
         (remote-log
          'transport-close-error
          :pipeline (remote-pipeline-runtime-pipeline-id runtime)
          :stage
          (remote-pipeline-stage-id
           (remote-stage-runtime-stage stage-runtime))
          :error (error-message-string error)))))
    (setf (remote-stage-runtime-state stage-runtime) 'closed)))

(defun remote-pipeline-open (route context &optional runtime)
  "Open a new transport runtime for ROUTE and CONTEXT.
RUNTIME, when non-nil, is an `opening' placeholder already installed by the
pool to guard against event-loop reentrancy.
Use `remote-pipeline-acquire' when the runtime should participate in pooling."
  (let* ((pipeline
          (or (remote-route-pipeline route)
              (error "Route has no pipeline: %S" route)))
         (runtime
          (or
           runtime
           (remote-pipeline-runtime-create
            :key (remote-pipeline-route-key route)
            :pipeline-id (remote-pipeline-id pipeline)
            :route route
            :context context
            :state 'opening
            :opened-at (current-time)
            :last-used-at (current-time)
            :use-count 0)))
         (endpoint
          (remote-transport--endpoint-from-config
           (remote-route-target-id route)
           (remote-pipeline-config pipeline))))
    (condition-case error
        (progn
          (dolist (stage (remote-pipeline-stages pipeline))
            (let* ((transport-id
                    (remote-normalize-id
                     (remote-pipeline-stage-transport stage)))
                   (transport
                    (or
                     (remote-get-transport transport-id)
                     (signal
                      'remote-transport-unsupported
                      (list transport-id
                            (remote-pipeline-id pipeline)))))
                   (input (copy-remote-endpoint endpoint))
                   (prepare (remote-transport-prepare-function transport))
                   (prepared
                    (if prepare
                        (or (funcall prepare stage endpoint runtime)
                            endpoint)
                      endpoint))
                   (connect (remote-transport-connect-function transport))
                   (result
                    (remote-transport--normalize-result
                     (and connect
                          (funcall connect stage prepared runtime))
                     prepared))
                   (stage-runtime
                    (remote-stage-runtime-create
                     :stage stage
                     :transport-id transport-id
                     :input-endpoint input
                     :output-endpoint
                     (or (remote-transport-result-endpoint result)
                         prepared)
                     :handle (remote-transport-result-handle result)
                     :metadata (remote-transport-result-metadata result)
                     :state 'open
                     :opened-at (current-time))))
              (setq endpoint
                    (remote-stage-runtime-output-endpoint stage-runtime))
              (setf (remote-pipeline-runtime-stages runtime)
                    (append
                     (remote-pipeline-runtime-stages runtime)
                     (list stage-runtime)))
              ;; `remote-pipeline-close' may run while CONNECT yields to the
              ;; event loop.  Do not resurrect that cancelled runtime after
              ;; the stage returns with a newly allocated handle.
              (unless (eq (remote-pipeline-runtime-state runtime) 'opening)
                (signal
                 'remote-pipeline-cancelled
                 (list
                  (format "Pipeline %s was cancelled while opening stage %s"
                          (remote-pipeline-id pipeline)
                          (remote-pipeline-stage-id stage)))))))
          (unless (eq (remote-pipeline-runtime-state runtime) 'opening)
            (signal
             'remote-pipeline-cancelled
             (list
              (format "Pipeline %s was cancelled while opening"
                      (remote-pipeline-id pipeline)))))
          (setf (remote-pipeline-runtime-endpoint runtime) endpoint
                (remote-pipeline-runtime-state runtime) 'open)
          (remote-log
           'pipeline-open
           :target (remote-route-target-id route)
           :pipeline (remote-pipeline-id pipeline)
           :stages
           (mapcar
            (lambda (stage-runtime)
              (remote-stage-runtime-transport-id stage-runtime))
            (remote-pipeline-runtime-stages runtime)))
          runtime)
      (error
       (setf (remote-pipeline-runtime-state runtime) 'failed
             (remote-pipeline-runtime-error runtime) error)
       (dolist
           (stage-runtime
            (reverse
             (copy-sequence
              (remote-pipeline-runtime-stages runtime))))
         (remote-transport--disconnect-stage stage-runtime runtime))
       (signal (car error) (cdr error))))))

(defun remote-pipeline-runtime-live-p (runtime)
  "Return non-nil when every stage in RUNTIME remains live."
  (and
   (remote-pipeline-runtime-p runtime)
   (eq (remote-pipeline-runtime-state runtime) 'open)
   (seq-every-p
    (lambda (stage-runtime)
      (and
       (eq (remote-stage-runtime-state stage-runtime) 'open)
       (let* ((transport
               (remote-get-transport
                (remote-stage-runtime-transport-id stage-runtime)))
              (live
               (and transport
                    (remote-transport-live-function transport))))
         (or (null live)
             (condition-case nil
                 (funcall live stage-runtime runtime)
               (error nil))))))
    (remote-pipeline-runtime-stages runtime))))

(defun remote-pipeline-close (runtime &optional reason)
  "Close RUNTIME in reverse stage order and record REASON."
  (when (remote-pipeline-runtime-p runtime)
    (unless (eq (remote-pipeline-runtime-state runtime) 'closed)
      (dolist
          (stage-runtime
           (reverse
            (copy-sequence
             (remote-pipeline-runtime-stages runtime))))
        (remote-transport--disconnect-stage stage-runtime runtime))
      (setf (remote-pipeline-runtime-state runtime) 'closed
            (remote-pipeline-runtime-error runtime) reason)
      (when
          (eq
           (gethash
            (remote-pipeline-runtime-key runtime)
            remote-pipeline-runtime-pool)
           runtime)
        (remhash
         (remote-pipeline-runtime-key runtime)
         remote-pipeline-runtime-pool))
      (remote-log
       'pipeline-close
       :pipeline (remote-pipeline-runtime-pipeline-id runtime)
       :reason
       (cond
        ((null reason) nil)
        ((symbolp reason) (symbol-name reason))
        ((listp reason) (error-message-string reason))
        (t (format "%s" reason)))))
    runtime))

(defun remote-pipeline-acquire (route context)
  "Acquire a shared live transport runtime for ROUTE and CONTEXT."
  (let* ((key (remote-pipeline-route-key route))
         (runtime (gethash key remote-pipeline-runtime-pool)))
    (when (and runtime
               (eq (remote-pipeline-runtime-state runtime) 'opening))
      (remote-log
       'pipeline-busy
       :target (remote-route-target-id route)
       :pipeline (remote-route-link-id route))
      (signal
       'remote-pipeline-busy
       (list
        (format "Pipeline %s for %s is already opening"
                (remote-route-link-id route)
                (remote-route-target-id route)))))
    (unless (remote-pipeline-runtime-live-p runtime)
      (when runtime
        (remote-pipeline-close runtime 'stale))
      (let* ((pipeline
              (or (remote-route-pipeline route)
                  (error "Route has no pipeline: %S" route)))
             (placeholder
              (remote-pipeline-runtime-create
               :key key
               :pipeline-id (remote-pipeline-id pipeline)
               :route route
               :context context
               :state 'opening
               :opened-at (current-time)
               :last-used-at (current-time)
               :use-count 0)))
        ;; Pipeline transport stages may yield to timers or invoke hooks.  The
        ;; placeholder makes a nested acquire fail instead of opening a second
        ;; runtime that would later be overwritten and leaked.
        (setq runtime placeholder)
        (puthash key runtime remote-pipeline-runtime-pool)
        (condition-case err
            (progn
              (remote-pipeline-open route context runtime)
              (unless
                  (and
                   (eq (gethash key remote-pipeline-runtime-pool) runtime)
                   (eq (remote-pipeline-runtime-state runtime) 'open))
                (remote-pipeline-close runtime 'opening-cancelled)
                (signal
                 'remote-pipeline-cancelled
                 (list
                  (format "Pipeline %s was replaced while opening"
                          (remote-pipeline-id pipeline))))))
          (error
           (when (eq (gethash key remote-pipeline-runtime-pool) runtime)
             (remhash key remote-pipeline-runtime-pool))
           (signal (car err) (cdr err))))))
    (setf (remote-pipeline-runtime-use-count runtime)
          (1+ (remote-pipeline-runtime-use-count runtime))
          (remote-pipeline-runtime-last-used-at runtime)
          (current-time))
    runtime))

(defun remote-pipeline-release (runtime &optional force reason)
  "Release one reference to RUNTIME.
When FORCE is non-nil, close it regardless of remaining references."
  (when (remote-pipeline-runtime-p runtime)
    (setf (remote-pipeline-runtime-use-count runtime)
          (max 0 (1- (remote-pipeline-runtime-use-count runtime))))
    (when (or force
              (zerop (remote-pipeline-runtime-use-count runtime)))
      (remote-pipeline-close runtime reason))
    runtime))

(defun remote-pipeline-runtime-list ()
  "Return stable summaries of pooled transport runtimes."
  (let (result)
    (maphash
     (lambda (_key runtime)
       (push
        (list
         :target
         (remote-route-target-id
          (remote-pipeline-runtime-route runtime))
         :pipeline (remote-pipeline-runtime-pipeline-id runtime)
         :state (remote-pipeline-runtime-state runtime)
         :uses (remote-pipeline-runtime-use-count runtime)
         :stages
         (mapcar
          (lambda (stage-runtime)
            (list
             :id
             (remote-pipeline-stage-id
              (remote-stage-runtime-stage stage-runtime))
             :transport
             (remote-stage-runtime-transport-id stage-runtime)
             :state (remote-stage-runtime-state stage-runtime)))
          (remote-pipeline-runtime-stages runtime)))
        result))
     remote-pipeline-runtime-pool)
    (sort
     result
     (lambda (left right)
       (string-lessp
        (plist-get left :pipeline)
        (plist-get right :pipeline))))))

(defun remote-pipeline-runtime-clear (&optional reason)
  "Close and remove every pooled transport runtime."
  (let (runtimes)
    (maphash
     (lambda (_key runtime) (push runtime runtimes))
     remote-pipeline-runtime-pool)
    (dolist (runtime runtimes)
      (remote-pipeline-close runtime (or reason 'pool-clear)))
    (length runtimes)))

(defun remote-pipeline-active-runtime (&optional pipeline)
  "Return the active runtime, optionally only for PIPELINE."
  (let ((runtime
         (or
          remote-current-pipeline-runtime
          (and
           (boundp 'remote-current-connection)
           remote-current-connection
           (fboundp 'remote-connection-pipeline-runtime)
           (remote-connection-pipeline-runtime
            remote-current-connection)))))
    (and
     runtime
     (or
      (null pipeline)
      (equal
       (remote-pipeline-runtime-pipeline-id runtime)
       (if (remote-pipeline-p pipeline)
           (remote-pipeline-id pipeline)
         (format "%s" pipeline))))
     runtime)))

(defun remote-pipeline-effective-config (pipeline &optional runtime)
  "Return PIPELINE config projected through RUNTIME's final endpoint."
  (let* ((runtime
          (or runtime (remote-pipeline-active-runtime pipeline)))
         (endpoint
          (if runtime
              (remote-pipeline-runtime-endpoint runtime)
            (remote-pipeline-plan pipeline)))
         (config (copy-tree (remote-pipeline-config pipeline))))
    (if (null endpoint)
        config
      (dolist
          (pair
           `((:host . ,(remote-endpoint-host endpoint))
             (:port . ,(remote-endpoint-port endpoint))
             (:user . ,(remote-endpoint-user endpoint))
             (:method . ,(remote-endpoint-method endpoint))
             (:hops . ,(remote-endpoint-hops endpoint))))
        (when (cdr pair)
          (setq config (plist-put config (car pair) (cdr pair)))))
      config)))

(defun remote-transport-register-builtins ()
  "Register built-in declarative and TRAMP-compatible transports."
  (remote-register-transport
   "direct"
   :capabilities '(address)
   :prepare #'remote-transport--address-prepare
   :describe (lambda () '(:kind direct :managed nil)))
  (remote-register-transport
   "native"
   :capabilities '(address)
   :prepare #'remote-transport--address-prepare
   :describe (lambda () '(:kind direct :managed nil)))
  (dolist (id '("address" "tailscale" "frp" "tunnel"))
    (remote-register-transport
     id
     :capabilities '(address)
     :prepare #'remote-transport--address-prepare
     :describe
     (lambda () '(:kind address-overlay :managed external))))
  (dolist
      (entry
       '(("ssh" . "ssh")
         ("sshx" . "sshx")
         ("scp" . "scp")
         ("wsl" . "wsl")
         ("docker" . "docker")
         ("podman" . "podman")
         ("toolbox" . "toolbox")
         ("kubectl" . "kubectl")
         ("sudo" . "sudo")))
    (let ((id (car entry))
          (method (cdr entry)))
      (remote-register-transport
       id
       :capabilities '(hop)
       :prepare
       (lambda (stage endpoint runtime)
         (remote-transport--hop-prepare
          method stage endpoint runtime))
       :describe
       (lambda ()
         (list :kind 'tramp-hop :method method :managed 'backend))))))

(remote-transport-register-builtins)

(provide 'remote-transport)
;;; remote-transport.el ends here
