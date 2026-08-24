;;; remote-backend-tramp-rpc.el --- tramp-rpc backend -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'remote-backend-core)
(require 'remote-backend-tramp)
(require 'remote-fs)

(declare-function msgpack-encode "msgpack" (object))
(declare-function msgpack-encode-alist "msgpack" (alist))
(declare-function msgpack-unsigned-to-bytes "msgpack" (integer size))
(declare-function tramp-rpc--cached-system-info "tramp-rpc" (vec))
(defvar tramp-rpc-deploy-git-build-policy)
(defvar tramp-rpc-deploy-version)

(defconst remote-backend-tramp-rpc--private-contracts
  '((tramp-rpc--cached-system-info . (1 . 1))
    (tramp-rpc--call . (3 . 3))
    (tramp-rpc--call-with-timeout . (5 . 5))
    (tramp-rpc-deploy--arch-to-rust-target . (1 . 1)))
  "Private upstream seams isolated by this backend adapter.")

(defun remote-backend-tramp-rpc--private-compatible-p (symbol)
  "Return non-nil when private SYMBOL retains its expected call contract."
  (when-let* ((expected
               (alist-get symbol
                          remote-backend-tramp-rpc--private-contracts)))
    (and (fboundp symbol)
         (or
          (equal (func-arity symbol) expected)
          ;; `advice-add' exposes a variadic combined function even when the
          ;; verified upstream definition beneath it has exact arity.  Our
          ;; installer always removes and revalidates before adding these.
          (pcase symbol
            ('tramp-rpc--call
             (advice-member-p
              #'remote-backend-tramp-rpc--adapter-timeout-a symbol))
            ('tramp-rpc-deploy--arch-to-rust-target
             (advice-member-p
              #'remote-backend-tramp-rpc--arch-to-rust-target-a symbol))))
         t)))

(defun remote-backend-tramp-rpc-compat-report ()
  "Describe private tramp-rpc seams without depending on package versions."
  (list
   :private-interfaces
   (mapcar
    (lambda (contract)
      (let ((symbol (car contract))
            (expected (cdr contract)))
        (list :symbol symbol
              :available (fboundp symbol)
              :arity (and (fboundp symbol) (func-arity symbol))
              :expected expected
              :compatible
              (remote-backend-tramp-rpc--private-compatible-p symbol))))
    remote-backend-tramp-rpc--private-contracts)
   :release (remote-backend-tramp-rpc-release-contract)))

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

(defun remote-backend-tramp-rpc--source-root ()
  "Return the installed tramp-rpc source root, or nil."
  (when-let* ((library (locate-library "tramp-rpc")))
    (expand-file-name
     (or (locate-dominating-file library ".git")
         (file-name-directory
          (directory-file-name (file-name-directory library)))))))

(defun remote-backend-tramp-rpc--git-output (root &rest arguments)
  "Run git ARGUMENTS in ROOT and return trimmed stdout, or nil."
  (when (and root (executable-find "git"))
    (with-temp-buffer
      (let ((default-directory temporary-file-directory)
            (status
             (apply #'process-file
                    (executable-find "git") nil t nil
                    "-C" root arguments)))
        (when (zerop status)
          (string-trim (buffer-string)))))))

(defun remote-backend-tramp-rpc-release-contract ()
  "Describe whether the installed tramp-rpc client is a release checkout."
  (let* ((root (remote-backend-tramp-rpc--source-root))
         (git-root
          (when-let* ((found (and root
                                  (locate-dominating-file root ".git"))))
            (expand-file-name found)))
         ;; The package owns this variable and may not have loaded its deploy
         ;; module yet.  `symbol-value' keeps the optional boundary explicit
         ;; and also behaves correctly under test/runtime dynamic bindings.
         (version (and (boundp 'tramp-rpc-deploy-version)
                       (symbol-value 'tramp-rpc-deploy-version)))
         (tag (and git-root
                   (remote-backend-tramp-rpc--git-output
                    git-root "describe" "--exact-match" "--tags" "HEAD")))
         (revision (and git-root
                        (remote-backend-tramp-rpc--git-output
                         git-root "rev-parse" "HEAD")))
         (dirty (and git-root
                     (not
                      (string-empty-p
                       (or (remote-backend-tramp-rpc--git-output
                            git-root "status" "--porcelain"
                            "--untracked-files=no")
                           "")))))
         (release-tag
          (and tag version
               (member tag (list version (concat "v" version))))))
    (list :source-root root
          :git-checkout (and git-root t)
          :revision revision
          :tag tag
          :dirty dirty
          :client-version version
          :release-checkout
          (if git-root (and release-tag (not dirty)) t))))

(defun remote-backend-tramp-rpc--locked-to-release-p ()
  "Return non-nil when installed tramp-rpc exactly matches its release tag."
  (plist-get (remote-backend-tramp-rpc-release-contract)
             :release-checkout))

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
    (_
     (condition-case error
         (funcall function architecture)
       (remote-file-error
        (signal
         'remote-backend-incompatible
         (list (error-message-string error)
               (list :architecture architecture))))))))

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
    (when (fboundp 'tramp-rpc-deploy--arch-to-rust-target)
      (advice-remove
       'tramp-rpc-deploy--arch-to-rust-target
       #'remote-backend-tramp-rpc--arch-to-rust-target-a)
      (when (remote-backend-tramp-rpc--private-compatible-p
             'tramp-rpc-deploy--arch-to-rust-target)
        (advice-add
         'tramp-rpc-deploy--arch-to-rust-target
         :around #'remote-backend-tramp-rpc--arch-to-rust-target-a))))
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
    (when (and
           (remote-backend-tramp-rpc--private-compatible-p
            'tramp-rpc--call)
           (remote-backend-tramp-rpc--private-compatible-p
            'tramp-rpc--call-with-timeout))
      (advice-add
       'tramp-rpc--call
       :around #'remote-backend-tramp-rpc--adapter-timeout-a))))

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

(defun remote-backend-tramp-rpc--info-value (key info)
  "Return KEY from tramp-rpc system INFO with symbol/string tolerance."
  (or (alist-get key info)
      (alist-get (symbol-name key) info nil nil #'equal)))

(defun remote-backend-tramp-rpc-probe (_route _context handle)
  "Negotiate the client/server contract for tramp-rpc HANDLE."
  (require 'tramp-rpc-deploy nil t)
  (let* ((release (remote-backend-tramp-rpc-release-contract))
         (client-version (plist-get release :client-version))
         (vec (and (stringp handle)
                   (ignore-errors
                     (tramp-dissect-file-name handle nil))))
         (info (and vec
                    (remote-backend-tramp-rpc--private-compatible-p
                     'tramp-rpc--cached-system-info)
                    (tramp-rpc--cached-system-info vec)))
         (server-version
          (remote-backend-tramp-rpc--info-value 'version info))
         (watcher
          (remote-backend-tramp-rpc--info-value 'watcher info))
         (capabilities
          (if (member watcher '(nil "null" "unknown"))
              (delq 'watch
                    (copy-sequence remote-backend-tramp-capabilities))
            (copy-sequence remote-backend-tramp-capabilities)))
         (match (and client-version server-version
                     (equal client-version server-version))))
    (append
     (list
      :status (if match 'ok 'incompatible)
      :capabilities capabilities
      :implementation-version client-version
      :protocol-version "2.0"
      :server-version server-version
      :watcher watcher
      :detail
      (unless match
        (format "tramp-rpc client %s and server %s do not match"
                (or client-version "unknown")
                (or server-version "unknown"))))
     release)))

(defun remote-backend-tramp-rpc-classify-error (error phase)
  "Classify tramp-rpc ERROR raised during PHASE."
  (let ((message (downcase (error-message-string error))))
    (when
        (string-match-p
         (rx (or "tramp-rpc-server"
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
            :status 'failed
            :error error))))

(defun remote-backend-tramp-rpc-register ()
  "Register the tramp-rpc backend."
  (remote-backend-tramp-rpc-install)
  (remote-register-backend
   "tramp-rpc"
   :capabilities remote-backend-tramp-capabilities
   :available #'remote-backend-tramp-rpc-available-p
   :probe #'remote-backend-tramp-rpc-probe
   :project #'remote-backend-tramp-rpc-project
   :expand-localname #'remote-backend-tramp-rpc-expand-localname
   :prepare #'remote-backend-tramp-rpc-prepare
   :connect #'remote-backend-tramp-connect
   :live #'remote-backend-tramp-live-p
   :disconnect #'remote-backend-tramp-disconnect
   :prepare-process #'remote-backend-tramp-handler-process-plan
   :stdio-bridge #'remote-backend-tramp-stdio-bridge
   :copy-file-to-target #'remote-backend-tramp-direct-copy-file
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
