;;; remote-channel.el --- Routed network channels and forwards -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Network APIs do not participate in Emacs magic file-name dispatch.  This
;; module supplies the explicit framework boundary.  Native channels work
;; immediately; remote backends expose only the channel capabilities they
;; genuinely implement, so an unsupported socket never falls back locally.

;;; Code:

(require 'cl-lib)
(require 'remote-backend)
(require 'remote-fs)
(require 'remote-session)

(declare-function remote-get-workspace "remote-workspace" (value))
(declare-function remote-workspace-register-recoverable-resource
                  "remote-workspace"
                  (workspace kind value &rest keys))

(cl-defstruct (remote-channel
               (:constructor remote-channel-create))
  id kind route context handle state opened-at
  recovery-function close-function metadata)

(defvar remote-channels (make-hash-table :test #'equal)
  "Routed channel descriptors keyed by generated channel ID.")

(defvar remote-channel-native-api-inhibit nil
  "Non-nil while a backend is invoking Emacs' native network API.

Compatibility decorators for third-party packages must honor this guard so a
routed `open-network-stream' or `make-network-process' call cannot route
itself recursively.")

(defvar remote-channel--counter 0)

(defvar remote-channel--process-contact-advice-installed nil)

(defvar remote-channel-failure-hook nil
  "Hook run with a failed `remote-channel' after its handle terminates.")

(defun remote-channel--finish (channel state)
  "Move CHANNEL to terminal STATE and remove it from the live registry."
  (when (remote-channel-p channel)
    ;; An explicit close wins over a process sentinel reporting the signal
    ;; caused by that close.
    (unless (eq (remote-channel-state channel) 'closed)
      (setf (remote-channel-state channel) state))
    (remhash (remote-channel-id channel) remote-channels))
  channel)

(defun remote-channel--watch-process (process channel &optional forward)
  "Attach CHANNEL lifecycle cleanup to PROCESS.
When FORWARD is non-nil, preserve its failed/closed state as well."
  (cl-labels
      ((finish ()
         (unless (memq (remote-channel-state channel) '(closed failed))
           (when (and forward
                      (not (eq (remote-forward-state forward) 'closed)))
             (setf (remote-forward-state forward) 'failed))
           (remote-channel--finish
            channel
            (if (and forward
                     (eq (remote-forward-state forward) 'failed))
                'failed
              'closed))
           (when (eq (remote-channel-state channel) 'failed)
             (run-hook-with-args
              'remote-channel-failure-hook channel)))))
    (process-put process 'remote-channel channel)
    (if (memq (process-status process) '(exit signal failed closed))
        (finish)
      (let ((sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (finished event)
           (unwind-protect
               (when sentinel
                 (funcall sentinel finished event))
             (when (memq (process-status finished)
                         '(exit signal failed closed))
               (finish)))))))))

(defun remote-channel--adopt
    (kind route context handle &optional recover close metadata)
  "Create and attach a channel descriptor for HANDLE."
  (or
   (remote-channel-of handle)
   (let* ((id
          (format "%s/channel-%d"
                  (remote-context-target-id context)
                  (cl-incf remote-channel--counter)))
         (channel
          (remote-channel-create
           :id id :kind kind :route route :context context :handle handle
           :state 'open :opened-at (current-time)
           :recovery-function recover :close-function close
           :metadata metadata)))
    (puthash id channel remote-channels)
    (cond
     ((processp handle)
      (remote-channel--watch-process handle channel))
     ((remote-forward-p handle)
      (setf (remote-forward-metadata handle)
            (plist-put
             (remote-forward-metadata handle)
             :remote-channel channel))
      (when-let* ((process (remote-forward-handle handle))
                  ((processp process)))
        (remote-channel--watch-process process channel handle))))
     channel)))

(cl-defun remote-channel-adopt
    (handle &key (kind 'network) context route
            (adapter "network") capability pipeline
            recover close metadata workspace (register t))
  "Adopt third-party network HANDLE into the Remote lifecycle.

HANDLE may be a process or `remote-forward'.  CONTEXT, ROUTE and the
adapter/capability keys identify the logical target just as the routed
constructors do.  Repeated adoption of the same HANDLE is idempotent.
When WORKSPACE resolves to an open workspace, register the channel as an
owned recoverable resource as well."
  (or
   (remote-channel-of handle)
   (let* ((context (remote-channel--context context))
          (capability
           (or capability
               (if (eq kind 'listener)
                   'network-server
                 'network-client)))
          (route
           (or route
               (remote-channel--route
                adapter capability context pipeline)))
          (channel
           (remote-channel--adopt
            kind route context handle recover close metadata)))
     (when (and register
                workspace
                (fboundp 'remote-workspace-register-recoverable-resource))
       (when-let* ((owner (remote-get-workspace workspace)))
         (remote-workspace-register-recoverable-resource
          owner 'channel handle
          :close (lambda (value _reason)
                   (remote-close-channel value))
          :recover
          (and recover
               (lambda (_resource _owner)
                 (funcall recover)))
          :metadata metadata)))
     channel)))

(defun remote-channel-of (value)
  "Return the `remote-channel' descriptor associated with VALUE."
  (cond
   ((remote-channel-p value) value)
   ((processp value) (process-get value 'remote-channel))
   ((remote-forward-p value)
    (plist-get (remote-forward-metadata value) :remote-channel))))

(defun remote-channel-live-p (value)
  "Return non-nil when routed channel VALUE remains usable."
  (when-let* ((channel (remote-channel-of value)))
    (and
     (eq (remote-channel-state channel) 'open)
     (let ((handle (remote-channel-handle channel)))
       (cond
        ((processp handle) (process-live-p handle))
        ((remote-forward-p handle)
         (and
          (eq (remote-forward-state handle) 'open)
          (let ((process (remote-forward-handle handle)))
            (or (not (processp process))
                (process-live-p process)))))
        (t t))))))

(defun remote-channel-endpoint (value &optional side)
  "Return logical endpoint information for routed channel VALUE.
SIDE is `local' or `remote'.  When SIDE is nil, return a plist containing
both endpoints.  Unlike `process-contact', this function never exposes a
client-side relay port as the target listener's logical endpoint."
  (let* ((channel (remote-channel-of value))
         (handle
          (cond
           ((remote-forward-p value) value)
           (channel (remote-channel-handle channel))
           (t value)))
         (forward
          (cond
           ((remote-forward-p handle) handle)
           ((processp handle) (process-get handle 'remote-forward))))
         (local
          (or
           (and forward (remote-forward-local-endpoint forward))
           (and (processp handle)
                (process-get handle 'remote-local-endpoint))))
         (remote
          (or
           (and forward (remote-forward-remote-endpoint forward))
           (and (processp handle)
                (or
                 (process-get handle 'remote-listen-endpoint)
                 (process-get handle 'remote-remote-endpoint))))))
    (pcase side
      ('local local)
      ('remote remote)
      ('nil (list :local local :remote remote))
      (_ (error "Unknown remote channel endpoint side: %S" side)))))

(defun remote-channel-list (&optional target)
  "Return stable summaries of live routed channels.
When TARGET is non-nil, include only that normalized target ID."
  (let ((target (and target (remote-normalize-id target)))
        result)
    (maphash
     (lambda (_id channel)
       (when (or (null target)
                 (equal
                  target
                  (remote-context-target-id
                   (remote-channel-context channel))))
         (push
          (list
           :id (remote-channel-id channel)
           :target
           (remote-context-target-id
            (remote-channel-context channel))
           :kind (remote-channel-kind channel)
           :pipeline
           (remote-route-pipeline-id (remote-channel-route channel))
           :backend
           (remote-route-backend-id (remote-channel-route channel))
           :state (remote-channel-state channel)
           :local-endpoint
           (remote-channel-endpoint channel 'local)
           :remote-endpoint
           (remote-channel-endpoint channel 'remote)
           :metadata (remote-channel-metadata channel)
           :opened-at (remote-channel-opened-at channel))
          result)))
     remote-channels)
    (sort result
          (lambda (left right)
            (string-lessp
             (plist-get left :id)
             (plist-get right :id))))))

(defun remote-channel--process-contact-a
    (function process &optional key no-block)
  "Expose PROCESS's target listener contact through FUNCTION.
Only routed reverse-forward listeners are changed; every ordinary Emacs
process uses the original `process-contact' implementation unchanged."
  (if-let* ((endpoint
             (and (processp process)
                  (process-get process 'remote-listen-endpoint))))
      (pcase key
        ('nil
         (list (plist-get endpoint :host)
               (plist-get endpoint :port)))
        (:host (plist-get endpoint :host))
        (:service (plist-get endpoint :port))
        (_ (funcall function process key no-block)))
    (funcall function process key no-block)))

(defun remote-channel-install-compatibility ()
  "Install narrow native API compatibility for routed listeners."
  (unless remote-channel--process-contact-advice-installed
    (advice-add
     'process-contact :around #'remote-channel--process-contact-a)
    (setq remote-channel--process-contact-advice-installed t)))

(defun remote-channel-uninstall-compatibility ()
  "Remove routed listener compatibility advice."
  (when remote-channel--process-contact-advice-installed
    (advice-remove
     'process-contact #'remote-channel--process-contact-a)
    (setq remote-channel--process-contact-advice-installed nil)))

(defun remote-channel--report-transport-failure (channel)
  "Feed failed transport-backed CHANNEL into route recovery."
  (when-let* ((route (remote-channel-route channel)))
    (remote-report-route-failure
     route
     (list
      'remote-transport-error
      (format "Channel %s (%s) terminated"
              (remote-channel-id channel)
              (remote-channel-kind channel))))))

(defun remote-channel--strip-keys (plist keys)
  "Return PLIST without KEYS."
  (cl-loop for (key value) on plist by #'cddr
           unless (memq key keys)
           append (list key value)))

(defun remote-channel--context (value)
  "Return a logical context for VALUE."
  (cond
   ((remote-context-p value) value)
   ((stringp value) (remote-context value))
   (t (remote-context))))

(defun remote-channel--route (adapter capability context pipeline)
  "Resolve a channel route for ADAPTER and CAPABILITY."
  (remote-resolve
   adapter capability context
   (and pipeline (list :link pipeline))))

(defun remote-channel--stream-process (result)
  "Return the process carried by native stream RESULT, or nil.
`open-network-stream' returns either a process or `(PROCESS . PROPERTIES)'
when `:return-list' is non-nil."
  (cond
   ((processp result) result)
   ((and (consp result)
         (processp (car result)))
    (car result))))

(defun remote-make-network-process (&rest plist)
  "Create a routed network process.
Accept all `make-network-process' keys plus `:remote-context',
`:remote-adapter', and `:remote-pipeline'."
  (let* ((context
          (remote-channel--context
           (plist-get plist :remote-context)))
         (adapter (or (plist-get plist :remote-adapter) "network"))
         (pipeline (plist-get plist :remote-pipeline))
         (capability
          (if (plist-get plist :server)
              'network-server
            'network-client))
         (route
          (remote-channel--route
           adapter capability context pipeline))
         (backend (remote-route-backend route))
         (function
          (and backend (remote-backend-network-function backend)))
         (session
          (and function
               (remote-session-acquire route context)))
         (arguments
          (remote-channel--strip-keys
           plist
           '(:remote-context :remote-adapter :remote-pipeline))))
    (unless function
      (signal
       'remote-backend-unsupported
       (list (remote-route-link-plugin-id route) capability)))
    (let ((remote-current-connection session)
          (remote-channel-native-api-inhibit t)
          (process (funcall function route context arguments)))
      (when (processp process)
        (process-put process 'remote-route route)
        (process-put process 'remote-context context)
        (if (plist-get plist :server)
            (unless (process-get process 'remote-listen-endpoint)
              (process-put
               process 'remote-listen-endpoint
               (list
                :host (process-contact process :host)
                :port (process-contact process :service))))
          (unless (process-get process 'remote-forward)
            (process-put
             process 'remote-remote-endpoint
             (list :host (plist-get arguments :host)
                   :port (plist-get arguments :service)))))
        (remote-channel--adopt
         (if (plist-get plist :server) 'listener 'network)
         route context process nil #'remote-close-channel
         (list :arguments arguments))
        (when (and (process-get process 'remote-listen-endpoint)
                   (process-get process 'remote-forward))
          (remote-channel-install-compatibility)))
      process)))

(defun remote-open-network-stream
    (name buffer host service &rest parameters)
  "Open a routed network stream with native-compatible arguments.
PARAMETERS also accepts `:remote-context', `:remote-adapter', and
`:remote-pipeline'."
  (let* ((context
          (remote-channel--context
           (plist-get parameters :remote-context)))
         (adapter (or (plist-get parameters :remote-adapter) "network"))
         (pipeline (plist-get parameters :remote-pipeline))
         (route
          (remote-channel--route
           adapter 'network-client context pipeline))
         (backend (remote-route-backend route))
         (function
          (and backend (remote-backend-stream-function backend)))
         (session
          (and function
               (remote-session-acquire route context)))
         (arguments
          (remote-channel--strip-keys
           parameters
           '(:remote-context :remote-adapter :remote-pipeline))))
    (unless function
      (signal
       'remote-backend-unsupported
       (list (remote-route-link-plugin-id route) 'network-client)))
    (let* ((remote-current-connection session)
           (remote-channel-native-api-inhibit t)
           (result
            (funcall function route context
                     name buffer host service arguments))
           (process (remote-channel--stream-process result)))
      (when (processp process)
        (process-put process 'remote-route route)
        (process-put process 'remote-context context)
        (unless (process-get process 'remote-forward)
          (process-put
           process 'remote-remote-endpoint
           (list :host host :port service)))
        (remote-channel--adopt
         'stream route context process nil #'remote-close-channel
         (list :name name :host host :service service
               :parameters arguments)))
      result)))

(cl-defun remote-channel--port-forward
    (capability direction remote-endpoint local-endpoint
                &key context (adapter "network") pipeline metadata
                workspace (register t) stable-endpoint)
  "Open a routed DIRECTION forward implementing CAPABILITY."
  (let* ((context (remote-channel--context context))
         (route
          (remote-channel--route
           adapter capability context pipeline))
         (backend (remote-route-backend route))
         (function
          (and backend (remote-backend-forward-function backend)))
         (session
          (and function
               (remote-session-acquire route context)))
         (metadata
          (plist-put
           (copy-sequence metadata) :direction direction))
         forward descriptor recovery-remote-endpoint)
    (unless function
      (signal
       'remote-backend-unsupported
       (list (remote-route-link-plugin-id route) capability)))
    (setq forward
          (or (let ((remote-current-connection session))
                (funcall function route context
                         local-endpoint remote-endpoint metadata))
              (error "Backend %s returned no forward"
                     (remote-backend-id backend))))
    (setq recovery-remote-endpoint
          (if (and stable-endpoint (eq direction 'reverse))
              (or (remote-forward-remote-endpoint forward)
                  remote-endpoint)
            remote-endpoint))
    (setq descriptor
          (remote-channel--adopt
           'forward route context forward nil
           #'remote-close-channel metadata))
    (setf
     (remote-channel-recovery-function descriptor)
     (lambda ()
       (remote-channel--port-forward
        capability direction recovery-remote-endpoint local-endpoint
        :context context :adapter adapter :pipeline pipeline
        :metadata metadata :register nil
        :stable-endpoint stable-endpoint)))
    (when (and register
               (fboundp 'remote-workspace-register-recoverable-resource))
      (when-let* ((owner
                   (or
                    (and workspace
                         (remote-get-workspace workspace))
                    (and (boundp 'remote-current-workspace)
                         remote-current-workspace)
                    (and (fboundp 'remote-get-workspace)
                         (remote-get-workspace context)))))
        (remote-workspace-register-recoverable-resource
         owner 'forward forward
         :close (lambda (value _reason)
                  (remote-close-channel value))
         :recover
         (lambda (_resource _owner)
           (remote-channel--port-forward
            capability direction recovery-remote-endpoint local-endpoint
            :context context :adapter adapter :pipeline pipeline
            :metadata metadata :register nil
            :stable-endpoint stable-endpoint))
         :metadata metadata)))
    forward))

(cl-defun remote-port-forward
    (remote-endpoint &key local-endpoint context
                     (adapter "network") pipeline metadata
                     workspace (register t))
  "Listen at LOCAL-ENDPOINT and forward to target REMOTE-ENDPOINT."
  (remote-channel--port-forward
   'port-forward 'local remote-endpoint local-endpoint
   :context context :adapter adapter :pipeline pipeline
   :metadata metadata :workspace workspace :register register))

(cl-defun remote-reverse-port-forward
    (local-endpoint &key remote-endpoint context
                    (adapter "network") pipeline metadata
                    workspace (register t) stable-endpoint)
  "Listen on target REMOTE-ENDPOINT and forward to LOCAL-ENDPOINT.
An omitted REMOTE-ENDPOINT binds target loopback on a dynamically allocated
port.  Use `remote-channel-endpoint' to obtain the allocated target port."
  (remote-channel--port-forward
   'reverse-forward 'reverse
   (or remote-endpoint '(:host "127.0.0.1" :port 0))
   local-endpoint
   :context context :adapter adapter :pipeline pipeline
   :metadata metadata :workspace workspace :register register
   :stable-endpoint stable-endpoint))

(defun remote-close-channel (channel)
  "Close routed process or forward CHANNEL."
  (when-let* ((descriptor (remote-channel-of channel)))
    (setf (remote-channel-state descriptor) 'closed)
    (remhash (remote-channel-id descriptor) remote-channels))
  (cond
   ((remote-channel-p channel)
    (remote-close-channel (remote-channel-handle channel)))
   ((processp channel)
    (when-let* ((forward (process-get channel 'remote-forward)))
      (remote-close-channel forward))
    (when (process-live-p channel)
      (delete-process channel)))
   ((remote-forward-p channel)
    (unless (eq (remote-forward-state channel) 'closed)
      (if-let* ((close (remote-forward-close-function channel)))
          (funcall close channel)
        (let ((handle (remote-forward-handle channel)))
          (when (and (processp handle)
                     (process-live-p handle))
            (delete-process handle))))
      (setf (remote-forward-state channel) 'closed)))
   (t (error "Not a remote channel: %S" channel))))

(defun remote-channel-clear (&optional target)
  "Close every routed channel, optionally only those belonging to TARGET."
  (let ((target (and target (remote-normalize-id target)))
        channels)
    (maphash
     (lambda (_id channel)
       (when (or
              (null target)
              (equal
               target
               (remote-context-target-id
                (remote-channel-context channel))))
         (push channel channels)))
     remote-channels)
    (dolist (channel channels)
      (remote-close-channel channel))
    (length channels)))

(defun remote-channel-recover (value)
  "Close and recreate recoverable routed channel VALUE.
Return the new native process or forward object.  Callers which own a
long-lived resource slot should replace the old VALUE with this result."
  (let* ((channel
          (or (remote-channel-of value)
              (error "Not a routed channel: %S" value)))
         (recover (remote-channel-recovery-function channel)))
    (unless recover
      (error "Channel %s has no recovery contract"
             (remote-channel-id channel)))
    (remote-close-channel channel)
    (condition-case error
        (funcall recover)
      (error
       (setf (remote-channel-state channel) 'failed
             (remote-channel-metadata channel)
             (plist-put
              (remote-channel-metadata channel)
              :recovery-error error))
       (remote-log
        'channel-recovery-error
        :channel (remote-channel-id channel)
        :target
        (remote-context-target-id
         (remote-channel-context channel))
        :error (error-message-string error))
       (signal (car error) (cdr error))))))

(defun remote-channel-register-adapter ()
  "Register the built-in routed network adapter."
  (remote-register-adapter
   "network"
   :capabilities
   '(network-client network-server port-forward reverse-forward)
   :preferences '((default . ("native" "tramp-rpc" "tramp")))
   :placement 'target))

(remote-channel-register-adapter)

(add-hook 'remote-channel-failure-hook
          #'remote-channel--report-transport-failure)

(provide 'remote-channel)
;;; remote-channel.el ends here
