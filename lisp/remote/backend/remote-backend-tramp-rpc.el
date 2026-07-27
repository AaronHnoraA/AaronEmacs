;;; remote-backend-tramp-rpc.el --- tramp-rpc backend -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'remote-backend-core)
(require 'remote-backend-tramp)
(require 'remote-fs)

(declare-function msgpack-encode "msgpack" (object))
(declare-function msgpack-encode-alist "msgpack" (alist))
(declare-function msgpack-unsigned-to-bytes "msgpack" (integer size))
(defvar tramp-rpc-deploy-git-build-policy)

(defcustom remote-backend-tramp-rpc-adapter-request-timeouts
  '(("direnv" . 120)
    ("environment" . 60))
  "Long `process.run' deadlines for framework adapters using tramp-rpc.
tramp-rpc intentionally defaults every synchronous RPC call to 30 seconds.
Environment discovery can legitimately exceed that during a cold Nix/direnv
evaluation, so only these explicitly named adapters receive a longer bound."
  :type '(alist :key-type string :value-type number)
  :group 'remote)

(defun remote-backend-tramp-rpc--msgpack-large-map-broken-p ()
  "Return non-nil when the installed msgpack has the large-map encoder bug."
  (and
   (fboundp 'msgpack-encode-alist)
   (condition-case nil
       (progn
         (msgpack-encode-alist
          (cl-loop for index below 16
                   collect (cons (format "k%d" index) "value")))
         nil)
     (wrong-type-argument t))))

(defun remote-backend-tramp-rpc--msgpack-encode-alist-a
    (function alist)
  "Encode large ALIST maps correctly, otherwise delegate to FUNCTION.
msgpack.el releases affected by this compatibility advice pass their two- or
four-byte map length string as one argument to `unibyte-string'.  tramp-rpc
hits that branch whenever a process environment contains more than 15
variables, which is routine for direnv and Nix shells."
  (let ((length (length alist)))
    (if (<= length 15)
        (funcall function alist)
      (concat
       (cond
        ((<= length #xffff)
         (concat
          (unibyte-string #xde)
          (msgpack-unsigned-to-bytes length 2)))
        ((<= length #xffffffff)
         (concat
          (unibyte-string #xdf)
          (msgpack-unsigned-to-bytes length 4)))
        (t
         (error "MessagePack map is too large: %d" length)))
       (mapconcat
        (lambda (entry)
          (concat
           (msgpack-encode (car entry))
           (msgpack-encode (cdr entry))))
        alist "")))))

(defun remote-backend-tramp-rpc--locked-to-release-p ()
  "Return non-nil when tramp-rpc is configured at its latest release."
  (when-let* ((recipes
               (or (and (boundp 'my/package-vc-recipes)
                        my/package-vc-recipes)
                   (and (boundp 'package-vc-selected-packages)
                        package-vc-selected-packages)))
              (recipe (alist-get 'tramp-rpc recipes)))
    (eq (plist-get recipe :rev) :last-release)))

(defun remote-backend-tramp-rpc--arch-to-rust-target-a
    (function architecture)
  "Map published release ARCHITECTURE values, otherwise call FUNCTION."
  (pcase architecture
    ((or "armv7l-linux" "armv7-linux")
     "armv7-unknown-linux-musleabihf")
    ((or "armv6l-linux" "arm-linux")
     "arm-unknown-linux-musleabihf")
    ((or "armv5tel-linux" "armv5te-linux")
     "armv5te-unknown-linux-musleabi")
    ((or "i386-linux" "i486-linux" "i586-linux" "i686-linux")
     "i686-unknown-linux-musl")
    (_ (funcall function architecture))))

(defun remote-backend-tramp-rpc--local-relay-cwd-a
    (function &rest arguments)
  "Run tramp-rpc async handler FUNCTION with local relay cwd isolation.
tramp-rpc creates local `cat' relay processes while `default-directory'
still names the remote `/rpc:' directory.  Local process creation must not
inherit that directory; the handler itself still sees it and therefore sends
the correct target-local cwd to the RPC server."
  (let ((start-process-function (symbol-function 'start-process)))
    (cl-letf
        (((symbol-function 'start-process)
          (lambda (&rest start-arguments)
            (let ((default-directory temporary-file-directory))
              (apply start-process-function start-arguments)))))
      (apply function arguments))))

(defun remote-backend-tramp-rpc--adapter-timeout-a
    (function vector method params)
  "Call tramp-rpc FUNCTION with an adapter-scoped long request deadline.
VECTOR, METHOD, and PARAMS are tramp-rpc's ordinary request arguments."
  (let ((timeout
         (cdr
          (assoc-string
           (or remote-current-adapter-id "")
           remote-backend-tramp-rpc-adapter-request-timeouts t))))
    (if (and timeout
             (equal method "process.run")
             (fboundp 'tramp-rpc--call-with-timeout))
        (tramp-rpc--call-with-timeout vector method params timeout 0.1)
      (funcall function vector method params))))

(defun remote-backend-tramp-rpc-install ()
  "Install deployment compatibility owned by the tramp-rpc backend."
  (when (require 'msgpack nil t)
    (advice-remove
     'msgpack-encode-alist
     #'remote-backend-tramp-rpc--msgpack-encode-alist-a)
    (when (remote-backend-tramp-rpc--msgpack-large-map-broken-p)
      (advice-add
       'msgpack-encode-alist
       :around #'remote-backend-tramp-rpc--msgpack-encode-alist-a)))
  (when (require 'tramp-rpc-deploy nil t)
    ;; Environment capsules are the single owner of direnv state.
    (when (boundp 'tramp-rpc-use-direnv)
      (setq tramp-rpc-use-direnv nil))
    ;; package-vc uses a git checkout even for a release tag.  This
    ;; configuration explicitly pins :last-release, so use the matching
    ;; published server instead of cross-compiling Linux on local Darwin.
    (when (and (eq tramp-rpc-deploy-git-build-policy 'auto)
               (remote-backend-tramp-rpc--locked-to-release-p))
      (setq tramp-rpc-deploy-git-build-policy 'release))
    ;; Upstream publishes these targets but does not map every uname spelling.
    (advice-remove
     'tramp-rpc-deploy--arch-to-rust-target
     #'remote-backend-tramp-rpc--arch-to-rust-target-a)
    (advice-add
     'tramp-rpc-deploy--arch-to-rust-target
     :around #'remote-backend-tramp-rpc--arch-to-rust-target-a))
  (when (fboundp 'tramp-rpc-handle-make-process)
    (advice-remove
     'tramp-rpc-handle-make-process
     #'remote-backend-tramp-rpc--local-relay-cwd-a)
    (advice-add
     'tramp-rpc-handle-make-process
     :around #'remote-backend-tramp-rpc--local-relay-cwd-a))
  (when (fboundp 'tramp-rpc--call)
    (advice-remove
     'tramp-rpc--call
     #'remote-backend-tramp-rpc--adapter-timeout-a)
    (advice-add
     'tramp-rpc--call
     :around #'remote-backend-tramp-rpc--adapter-timeout-a)))

(with-eval-after-load 'tramp-rpc-process
  (remote-backend-tramp-rpc-install))

(with-eval-after-load 'tramp-rpc
  (remote-backend-tramp-rpc-install))

(defun remote-backend-tramp-rpc-project (file-name link _route)
  "Project logical FILE-NAME through tramp-rpc LINK."
  (remote-backend-tramp-file-name
   (remote-fs-localname file-name) link "rpc"))

(defun remote-backend-tramp-rpc-expand-localname
    (name directory link _route)
  "Resolve target-native NAME against DIRECTORY through tramp-rpc LINK."
  (remote-backend-tramp-expand-localname-with-method
   name directory link "rpc"))

(defun remote-backend-tramp-rpc-available-p (link _context)
  "Return whether tramp-rpc may serve LINK."
  (and (remote-target-trusted
        (remote-get-target (remote-link-target-id link)))
       (or (featurep 'tramp-rpc)
           (locate-library "tramp-rpc"))))

(defun remote-backend-tramp-rpc-prepare (execution)
  "Mark EXECUTION as requiring an absolute target executable."
  (setf
   (remote-backend-execution-metadata execution)
   (plist-put
    (remote-backend-execution-metadata execution)
    :require-absolute-program t))
  execution)

(defun remote-backend-tramp-rpc-classify-error (error phase)
  "Classify tramp-rpc ERROR raised during PHASE."
  (let ((message (downcase (error-message-string error))))
    (when
        (string-match-p
         (rx (or "unknown architecture"
                 "tramp-rpc-server"
                 "rpc response"
                 "method=system.info"
                 "rpc process"
                 ;; tramp-rpc can collapse deployment/bootstrap failures into
                 ;; TRAMP's generic connection error.  Keep that first failure
                 ;; backend-local so standard TRAMP on the same SSH pipeline
                 ;; still receives one bounded attempt.
                 "tramp failed to connect"))
         message)
      (list :scope 'backend
            :phase phase
            :retryable t
            :status
            (if (string-match-p "unknown architecture" message)
                'incompatible
              'failed)
            :error error))))

(defun remote-backend-tramp-rpc-register ()
  "Register the tramp-rpc backend."
  (remote-backend-tramp-rpc-install)
  (remote-register-backend
   "tramp-rpc"
   :capabilities remote-backend-tramp-capabilities
   :available #'remote-backend-tramp-rpc-available-p
   :project #'remote-backend-tramp-rpc-project
   :expand-localname #'remote-backend-tramp-rpc-expand-localname
   :prepare #'remote-backend-tramp-rpc-prepare
   :connect #'remote-backend-tramp-connect
   :live #'remote-backend-tramp-live-p
   :disconnect #'remote-backend-tramp-disconnect
   :prepare-process #'remote-backend-tramp-handler-process-plan
   :stdio-bridge #'remote-backend-tramp-stdio-bridge
   :make-network-process #'remote-backend-tramp-network
   :open-network-stream #'remote-backend-tramp-stream
   :port-forward #'remote-backend-tramp-forward
   :classify-error #'remote-backend-tramp-rpc-classify-error
   :program-form 'absolute
   :describe
   (lambda ()
     '(:kind tramp-rpc
       :session-owner tramp-rpc
       :spawn-program absolute))))

(remote-backend-tramp-rpc-install)

(provide 'remote-backend-tramp-rpc)
;;; remote-backend-tramp-rpc.el ends here
