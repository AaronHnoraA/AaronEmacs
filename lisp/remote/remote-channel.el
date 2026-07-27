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

(defvar remote-channel--counter 0)

(defun remote-channel--adopt
    (kind route context handle &optional recover close metadata)
  "Create and attach a channel descriptor for HANDLE."
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
      (process-put handle 'remote-channel channel)
      (let ((sentinel (process-sentinel handle)))
        (set-process-sentinel
         handle
         (lambda (finished event)
           (unwind-protect
               (when sentinel
                 (funcall sentinel finished event))
             (when (memq (process-status finished)
                         '(exit signal failed closed))
               (setf (remote-channel-state channel) 'closed)
               (remhash
                (remote-channel-id channel)
                remote-channels)))))))
     ((remote-forward-p handle)
      (setf (remote-forward-metadata handle)
            (plist-put
             (remote-forward-metadata handle)
             :remote-channel channel))))
    channel))

(defun remote-channel-of (value)
  "Return the `remote-channel' descriptor associated with VALUE."
  (cond
   ((remote-channel-p value) value)
   ((processp value) (process-get value 'remote-channel))
   ((remote-forward-p value)
    (plist-get (remote-forward-metadata value) :remote-channel))))

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
          (process (funcall function route context arguments)))
      (when (processp process)
        (process-put process 'remote-route route)
        (process-put process 'remote-context context)
        (remote-channel--adopt
         (if (plist-get plist :server) 'listener 'network)
         route context process nil #'remote-close-channel
         (list :arguments arguments)))
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
    (let ((remote-current-connection session)
          (process
           (funcall function route context
                    name buffer host service arguments)))
      (when (processp process)
        (process-put process 'remote-route route)
        (process-put process 'remote-context context)
        (remote-channel--adopt
         'stream route context process nil #'remote-close-channel
         (list :name name :host host :service service
               :parameters arguments)))
      process)))

(cl-defun remote-port-forward
    (remote-endpoint &key local-endpoint context
                     (adapter "network") pipeline metadata
                     workspace (register t))
  "Open a routed port forward to REMOTE-ENDPOINT."
  (let* ((context (remote-channel--context context))
         (route
          (remote-channel--route
           adapter 'port-forward context pipeline))
         (backend (remote-route-backend route))
         (function
          (and backend (remote-backend-forward-function backend)))
         (session
          (and function
               (remote-session-acquire route context)))
         forward)
    (unless function
      (signal
       'remote-backend-unsupported
       (list (remote-route-link-plugin-id route) 'port-forward)))
    (setq forward
          (or (let ((remote-current-connection session))
                (funcall function route context
                         local-endpoint remote-endpoint metadata))
              (error "Backend %s returned no forward"
                     (remote-backend-id backend))))
    (remote-channel--adopt
     'forward route context forward
     (lambda ()
       (remote-port-forward
        remote-endpoint
        :local-endpoint local-endpoint
        :context context :adapter adapter :pipeline pipeline
        :metadata metadata :register nil))
     #'remote-close-channel metadata)
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
           (remote-port-forward
            remote-endpoint
            :local-endpoint local-endpoint
            :context context :adapter adapter :pipeline pipeline
            :metadata metadata :register nil))
         :metadata metadata)))
    forward))

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

(defun remote-channel-register-adapter ()
  "Register the built-in routed network adapter."
  (remote-register-adapter
   "network"
   :capabilities '(network-client network-server port-forward)
   :preferences '((default . ("native" "tramp-rpc" "tramp")))
   :placement 'target))

(remote-channel-register-adapter)

(provide 'remote-channel)
;;; remote-channel.el ends here
