;;; remote-backend-tramp-rpc.el --- tramp-rpc backend -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'remote-backend-core)
(require 'remote-backend-tramp)
(require 'remote-backend-tramp-rpc-metadata)
(require 'remote-fs)

(declare-function msgpack-encode "msgpack" (object))
(declare-function msgpack-encode-alist "msgpack" (alist))
(declare-function msgpack-unsigned-to-bytes "msgpack" (integer size))
(declare-function tramp-rpc--cached-system-info "tramp-rpc" (vec))
(declare-function tramp-rpc--invalidate-cache-for-path
                  "tramp-rpc-magit" (filename))
(declare-function tramp-rpc--invalidate-cache-for-subtree
                  "tramp-rpc-magit" (directory))
(declare-function tramp-rpc--call "tramp-rpc-transport"
                  (vec method params &optional connection))
(declare-function tramp-rpc--decode-output "tramp-rpc-protocol" (data))
(declare-function tramp-rpc--call-with-timeout "tramp-rpc-transport"
                  (vec method params total-timeout poll-interval
                       &optional connection))
(declare-function tramp-rpc--connection-key "tramp-rpc-transport" (vec))
(declare-function tramp-rpc--get-connection "tramp-rpc-transport" (vec))
(declare-function tramp-rpc--process-connection "tramp-rpc-connection"
                  (process))
(declare-function tramp-rpc-connection-p "tramp-rpc-connection" (object))
(declare-function tramp-rpc-connection-process "tramp-rpc-connection"
                  (connection))
(declare-function tramp-rpc-connection-buffer "tramp-rpc-connection"
                  (connection))
(declare-function tramp-rpc-connection-stderr-buffer "tramp-rpc-connection"
                  (connection))
(declare-function tramp-rpc-connection-cleanup-started "tramp-rpc-connection"
                  (connection))
(declare-function tramp-rpc--invalidate-timed-out-connection
                  "tramp-rpc-transport" (process vec event))
(declare-function tramp-rpc--acl-enabled-p "tramp-rpc" (vec))
(declare-function tramp-rpc--cache-entry-valid-p "tramp-rpc-cache" (timestamp))
(declare-function tramp-rpc--call-batch "tramp-rpc-transport" (vec requests))
(declare-function tramp-rpc--encode-path "tramp-rpc-protocol" (path))
(declare-function tramp-rpc--decode-filename "tramp-rpc" (entry))
(declare-function tramp-rpc--batch-error-p "tramp-rpc-transport" (result))
(declare-function tramp-rpc--cache-file-stat-result "tramp-rpc-cache"
                  (vec localname stat &optional lstat))
(declare-function remote-fs-remember-ancestor-listing "remote-fs"
                  (directory names))
(declare-function remote-backend-project-file-name "remote-backend"
                  (route file-name))
(declare-function tramp-rpc--selinux-enabled-p "tramp-rpc" (vec))
(declare-function remote-connection-cached-p "remote-connection" (route))
(declare-function remote-connection-handle "remote-connection" (connection))
(declare-function remote-workspace-routes "remote-workspace" (workspace))
(declare-function remote-report-route-failure "remote-core" (route error))
(defvar remote-workspaces)
(defvar tramp-rpc-deploy-git-build-policy)
(defvar tramp-rpc-deploy-version)
(defvar remote-fs-current-retry-safe-query)
(defvar remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip)
(defvar remote-backend-tramp-rpc--retired-shims)

(defconst remote-backend-tramp-rpc--verified-client-version "0.16.0"
  "tramp-rpc release whose private compatibility shims were tested here.")

(defconst remote-backend-tramp-rpc--verified-client-revision
  "543b3bf30b8a80b8cd0200c98d00f22a4e0220ac"
  "Exact tramp-rpc client commit tested by private compatibility shims.")

(defconst remote-backend-tramp-rpc--private-contracts
  '((tramp-rpc--cached-system-info . (1 . 1))
    (tramp-rpc--call . (3 . 4))
    (tramp-rpc--call-with-timeout . (5 . 6))
    (tramp-rpc--decode-output . (1 . 1))
    (tramp-rpc--connection-key . (1 . 1))
    (tramp-rpc--get-connection . (1 . 1))
    (tramp-rpc--process-connection . (1 . 1))
    (tramp-rpc-connection-p . (1 . 1))
    (tramp-rpc-connection-process . (1 . 1))
    (tramp-rpc-connection-buffer . (1 . 1))
    (tramp-rpc-connection-stderr-buffer . (1 . 1))
    (tramp-rpc-connection-cleanup-started . (1 . 1))
    (tramp-rpc--controlmaster-socket-path . (1 . 1))
    (tramp-rpc--ensure-controlmaster-directory . (0 . 0))
    (tramp-rpc--acl-enabled-p . (1 . 1))
    (tramp-rpc--selinux-enabled-p . (1 . 1))
    (tramp-rpc-handle-write-region . (3 . 7))
    (tramp-rpc-handle-set-file-uid-gid . (1 . 3))
    (tramp-rpc--connection-transport-death . (3 . 3))
    (tramp-rpc--invalidate-timed-out-connection . (3 . 3))
    (tramp-rpc--cleanup-connection-generation . (4 . 6))
    (tramp-rpc--make-file-notify-descriptor . (3 . 3))
    (tramp-rpc--make-direct-ssh-pty-process . (9 . 10))
    (tramp-rpc--file-access-p . (2 . 2))
    (tramp-rpc--cache-entry-valid-p . (1 . 1))
    (tramp-rpc--call-batch . (2 . 2))
    (tramp-rpc--encode-path . (1 . 1))
    (tramp-rpc--decode-filename . (1 . 1))
    (tramp-rpc--batch-error-p . (1 . 1))
    (tramp-rpc--cache-file-stat-result . (3 . 4)))
  "Private upstream seams isolated by this backend adapter.
A connection generation is a `tramp-rpc-connection' record; it is read only
through `remote-backend-tramp-rpc--connection-slot'.")

(defconst remote-backend-tramp-rpc--connection-accessors
  '((:process . tramp-rpc-connection-process)
    (:buffer . tramp-rpc-connection-buffer)
    (:stderr-buffer . tramp-rpc-connection-stderr-buffer)
    (:cleanup-started . tramp-rpc-connection-cleanup-started))
  "Slots of a tramp-rpc connection generation this adapter reads.")

(defvar remote-backend-tramp-rpc--connection-shape nil
  "Non-nil when tramp-rpc connection generations have the verified shape.
Set by `remote-backend-tramp-rpc-install', which runs again whenever
tramp-rpc is loaded, so the check costs nothing per RPC.")

(defun remote-backend-tramp-rpc--connection-shape-check ()
  "Return non-nil when every connection accessor keeps its contract."
  (cl-every #'remote-backend-tramp-rpc--private-compatible-p
            (cons 'tramp-rpc-connection-p
                  (mapcar #'cdr remote-backend-tramp-rpc--connection-accessors))))

(defun remote-backend-tramp-rpc--connection-readable-p ()
  "Return non-nil when connection generations have the verified shape."
  remote-backend-tramp-rpc--connection-shape)

(defun remote-backend-tramp-rpc--connection-slot (connection slot)
  "Return SLOT of tramp-rpc CONNECTION, or nil.
SLOT is a key of `remote-backend-tramp-rpc--connection-accessors'.  A
connection of any other shape answers nil rather than a guess, so a changed
upstream representation disables the reader instead of misreading it."
  (when-let* ((accessor
               (alist-get slot remote-backend-tramp-rpc--connection-accessors))
              ((remote-backend-tramp-rpc--connection-readable-p))
              ((tramp-rpc-connection-p connection)))
    (funcall accessor connection)))

(defun remote-backend-tramp-rpc--vec-connection-slot (vec slot)
  "Return SLOT of VEC's current tramp-rpc connection generation, or nil."
  (when (remote-backend-tramp-rpc--private-compatible-p
         'tramp-rpc--get-connection)
    (remote-backend-tramp-rpc--connection-slot
     (ignore-errors (tramp-rpc--get-connection vec)) slot)))

(defun remote-backend-tramp-rpc--upstream-arity (symbol)
  "Return SYMBOL's underlying arity through Emacs advice, or nil.
Only inspect advice's private representation when its accessors exist.
If a later Emacs changes that representation, private shims stay disabled."
  (when (fboundp symbol)
    (condition-case nil
        (let ((function (symbol-function symbol))
              (depth 0))
          (while (and (fboundp 'advice--p)
                      (advice--p function)
                      (< depth 32))
            (unless (fboundp 'advice--cdr)
              (error "Cannot inspect advised function"))
            (setq function (advice--cdr function)
                  depth (1+ depth)))
          (unless (and (fboundp 'advice--p)
                       (advice--p function))
            (func-arity function)))
      (error nil))))

(defun remote-backend-tramp-rpc--private-compatible-p (symbol)
  "Return non-nil when private SYMBOL retains its expected call contract.
Check the upstream definition beneath all advice.  A tracing or third-party
advice must neither disable a verified optimization nor hide an API change."
  (when-let* ((expected
               (alist-get symbol
                          remote-backend-tramp-rpc--private-contracts)))
    (equal (remote-backend-tramp-rpc--upstream-arity symbol) expected)))

(defun remote-backend-tramp-rpc-compat-report ()
  "Describe private tramp-rpc seams and active version-gated shims."
  (let ((release (remote-backend-tramp-rpc-release-contract)))
    (list
     :private-interfaces
     (mapcar
      (lambda (contract)
        (let ((symbol (car contract))
              (expected (cdr contract)))
          (list :symbol symbol
                :available (fboundp symbol)
                :arity (and (fboundp symbol) (func-arity symbol))
                :upstream-arity
                (remote-backend-tramp-rpc--upstream-arity symbol)
                :expected expected
                :compatible
                (remote-backend-tramp-rpc--private-compatible-p symbol))))
      remote-backend-tramp-rpc--private-contracts)
     :release release
     :verified-client-version
     (remote-backend-tramp-rpc--verified-release-p release)
     :connection-shape (remote-backend-tramp-rpc--connection-readable-p)
     :shims (remote-backend-tramp-rpc--shim-report)
     :retired-shims
     (cl-loop for (symbol . advice) in remote-backend-tramp-rpc--retired-shims
              collect (list :symbol symbol :advice advice
                            :installed (and (fboundp symbol)
                                            (advice-member-p advice symbol)
                                            t))))))

(defcustom remote-backend-tramp-rpc-adapter-request-timeouts
  '(("direnv" . 120)
    ("environment" . 60))
  "Long `process.run' deadlines for framework adapters using tramp-rpc.
tramp-rpc intentionally defaults every synchronous RPC call to 30 seconds.
Environment discovery can legitimately exceed that during a cold Nix/direnv
evaluation, so only these explicitly named adapters receive a longer bound."
  :type '(alist :key-type string :value-type number)
  :group 'remote)

(defcustom remote-backend-tramp-rpc-read-query-timeouts
  '(("file.stat" . 5)
    ("dir.list" . 10))
  "RPC deadlines in seconds for routed retry-safe file queries.
Only known `/fs:' metadata and directory reads use these bounds.  Direct
`/rpc:' access, writes, and operations with path-valued results retain the
upstream deadline.  A timed-out RPC transport is closed before recovery."
  :type '(alist :key-type string :value-type number)
  :group 'remote)

(defcustom remote-backend-tramp-rpc-attribute-probe-ttl 60
  "Seconds to reuse ACL and SELinux availability on one RPC transport.
An RPC process replacement starts with no cached result.  Zero disables
reuse.  Only a successful availability check or a proven missing target
command is cached; other failures remain retryable.  This workaround is
installed only for the verified tramp-rpc release and private call shapes."
  :type '(number :tag "Seconds, or zero to disable")
  :group 'remote)

(defcustom remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip t
  "Skip redundant ACL/SELinux/ownership round trips on in-place writes.
The pinned tramp-rpc server truncates an existing inode with `file.write',
which preserves its extended attributes and owner.  Unknown client or Emacs
versions retain TRAMP's ordinary metadata handling."
  :type 'boolean
  :group 'remote)

(defvar remote-backend-tramp-rpc--in-place-write-file nil
  "Dynamically scoped physical file name during a verified RPC write.")

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

(defvar remote-backend-tramp-rpc--git-cache nil
  "Git answers about the client checkout, as (STAMP . ((KEY . VALUE)...)).
The release contract is asked for by every install, of which a startup runs
several, by every backend probe and by every `~USER' lookup.")

(defun remote-backend-tramp-rpc--git-stamp (root)
  "Return what changes in ROOT's checkout when a git answer can change."
  (let ((default-directory temporary-file-directory))
    (mapcar
     (lambda (name)
       (file-attribute-modification-time
        (file-attributes (expand-file-name name root))))
     '(".git/HEAD" ".git/index"))))

(defun remote-backend-tramp-rpc--git-output (root &rest arguments)
  "Run git ARGUMENTS in ROOT and return trimmed stdout, or nil.
ROOT is this package's own checkout on the client, so the call has to stay on
the client.  It runs from the backend probe while the target connection is
still opening: a `default-directory' inherited from the caller would route
`process-file' back into that half-open connection, and the whole operation
would fail with `remote-connection-busy' instead of reporting the contract.
An answer is reused until ROOT's HEAD or index changes."
  (when-let* ((root root)
              (git (remote-client-executable-find "git")))
    (let ((stamp (remote-backend-tramp-rpc--git-stamp root))
          (key (cons root arguments)))
      (unless (equal stamp (car remote-backend-tramp-rpc--git-cache))
        (setq remote-backend-tramp-rpc--git-cache (list stamp)))
      (if-let* ((known (assoc key (cdr remote-backend-tramp-rpc--git-cache))))
          (cdr known)
        (let ((value
               (with-temp-buffer
                 (let* ((default-directory temporary-file-directory)
                        (process-environment
                         (remote-client-process-environment))
                        (exec-path (remote-client-exec-path))
                        ;; Refreshing the index would change the stamp.
                        (status
                         (apply #'process-file git nil t nil
                                "--no-optional-locks" "-C" root arguments)))
                   (when (zerop status)
                     (string-trim (buffer-string)))))))
          (push (cons key value)
                (cdr remote-backend-tramp-rpc--git-cache))
          value)))))

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

(defun remote-backend-tramp-rpc--verified-release-p (&optional release)
  "Return non-nil for the exact tramp-rpc RELEASE tested by private shims."
  (let ((release (or release (remote-backend-tramp-rpc-release-contract))))
    (and (equal (plist-get release :client-version)
                remote-backend-tramp-rpc--verified-client-version)
         (plist-get release :git-checkout)
         (equal (plist-get release :revision)
                remote-backend-tramp-rpc--verified-client-revision)
         (plist-get release :release-checkout))))

(defun remote-backend-tramp-rpc--local-controlmaster-path-a
    (function &rest arguments)
  "Run FUNCTION with ARGUMENTS where tramp-rpc means this Emacs client.
The verified client expands `~/.ssh/tramp-rpc/%C' under the current buffer's
`default-directory'.  Direct SSH PTYs call this from a target buffer, where
that expansion otherwise points to the target account's home and SSH exits
before starting a shell.  The control socket and its directory live locally.
The direct PTY itself is a local `ssh' started with `start-process'; from a
routed `/fs:' call the physical `/rpc:' directory is not handled there, so
Emacs would try to enter it locally (\"Setting current directory\").  The
remote working directory travels in the PTY's own command line."
  (let ((default-directory temporary-file-directory)
        (process-environment (remote-client-process-environment))
        (exec-path (remote-client-exec-path))
        (remote-current-adapter-id nil)
        (remote-current-route nil))
    (apply function arguments)))

(defcustom remote-backend-tramp-rpc-liveness-probe-timeout 2
  "Seconds a timed-out RPC transport has to answer a ping.
A bounded request that timed out may have met a slow server or a dead
link.  The transport is kept when it still answers within this time, as
VS Code Remote keeps a slow connection, and is closed otherwise so the
reconnect job never reuses a dead stream.  Zero closes it at once."
  :type 'number
  :group 'remote)

(defvar remote-backend-tramp-rpc--probe-slot nil
  "Record index of the verified `probing' slot of a tramp-rpc connection.
Set by `remote-backend-tramp-rpc-install' from the struct's own layout.")

(defmacro remote-backend-tramp-rpc--with-probe-guard (generation guard &rest body)
  "Run BODY with GENERATION's upstream probe flag at index GUARD set.
The previous value is restored afterwards.  A nil GUARD runs BODY alone."
  (declare (indent 2))
  (let ((g (make-symbol "generation"))
        (i (make-symbol "guard"))
        (old (make-symbol "old")))
    `(let* ((,g ,generation)
            (,i ,guard)
            (,old (and ,i (aref ,g ,i))))
       (unwind-protect
           (progn
             (when ,i (aset ,g ,i t))
             ,@body)
         (when ,i (aset ,g ,i ,old))))))

(defun remote-backend-tramp-rpc--probe-guard (generation)
  "Return the record index of GENERATION's upstream probe flag, or nil.
Setting that flag is how upstream stops a probe from probing again; nil
means the verified slot is absent and upstream keeps its own probe."
  (and remote-backend-tramp-rpc--probe-slot
       (remote-backend-tramp-rpc--connection-readable-p)
       (tramp-rpc-connection-p generation)
       remote-backend-tramp-rpc--probe-slot))

(defun remote-backend-tramp-rpc--retire-unless-alive
    (vector generation guard method)
  "Close VECTOR's timed-out GENERATION unless it still answers a ping.
GUARD is the upstream probe flag index set around the ping, or nil.
METHOD names the request that timed out."
  (let ((process (remote-backend-tramp-rpc--connection-slot
                  generation :process))
        (timeout remote-backend-tramp-rpc-liveness-probe-timeout))
    (when (and (processp process) (process-live-p process))
      (unless (and guard (numberp timeout) (> timeout 0)
                   (condition-case nil
                       (remote-backend-tramp-rpc--with-probe-guard
                           generation guard
                         (tramp-rpc--call-with-timeout
                          vector "system.ping" nil timeout 0.01 generation)
                         t)
                     (error nil)))
        ;; Claiming the generation removes it from the connection table, so
        ;; the transport-death notice would no longer recognize it.
        ;; Best effort: the caller must still see the original timeout.
        (ignore-errors
          (remote-backend-tramp-rpc--report-transport-failure
           vector (format "tramp-rpc transport stopped answering (method=%s)"
                          method)))
        (if (remote-backend-tramp-rpc--private-compatible-p
             'tramp-rpc--invalidate-timed-out-connection)
            ;; Also tears down a half-open ControlMaster, which a reconnect
            ;; would otherwise reuse.
            (ignore-errors
              (tramp-rpc--invalidate-timed-out-connection
               process vector
               (format "RPC timeout without liveness (method=%s)\n" method)))
          (ignore-errors (delete-process process)))))))

(defun remote-backend-tramp-rpc--adapter-timeout-a
    (function vector method params &optional connection)
  "Call tramp-rpc FUNCTION with a deadline suited to the routed request.
CONNECTION preserves the upstream captured connection generation."
  (let* ((long-timeout
          (and (equal method "process.run")
               (cdr
                (assoc-string
                 (or remote-current-adapter-id "")
                 remote-backend-tramp-rpc-adapter-request-timeouts t))))
         (read-timeout
          (and remote-fs-current-retry-safe-query
               (cdr
                (assoc-string
                 method remote-backend-tramp-rpc-read-query-timeouts t))))
         (timeout (or long-timeout read-timeout)))
    (if (and (numberp timeout) (> timeout 0)
             (fboundp 'tramp-rpc--call-with-timeout))
        (let* ((generation (or connection (tramp-rpc--get-connection vector)))
               (guard (remote-backend-tramp-rpc--probe-guard generation)))
          (condition-case err
              ;; Upstream answers a timeout with its own 10 s ping; this
              ;; caller's deadline is the whole budget, so the short ping in
              ;; `remote-backend-tramp-rpc--retire-unless-alive' replaces it.
              (remote-backend-tramp-rpc--with-probe-guard generation guard
                (tramp-rpc--call-with-timeout
                 vector method params timeout 0.1 connection))
            (remote-file-error
             (when (and generation
                        (string-match-p
                         "[Tt]imeout waiting for RPC response"
                         (error-message-string err))
                        (eq generation (tramp-rpc--get-connection vector)))
               (remote-backend-tramp-rpc--retire-unless-alive
                vector generation guard method))
             (signal (car err) (cdr err)))))
      (funcall function vector method params connection))))

(defun remote-backend-tramp-rpc--attribute-command-missing-p (error)
  "Return non-nil when ERROR identifies a missing target executable."
  (let ((data (car (last error))))
    (and (eq (car error) 'file-missing)
         (or
          (and (listp data)
               (eq (alist-get 'spawn_not_found data) t)
               (eql (alist-get 'os_errno data) 2))
          ;; TRAMP's write-region skeleton can flatten the same typed RPC
          ;; error into one message string before the attribute hook sees it.
          ;; Require both the spawn marker and errno so an unrelated missing
          ;; file is never mistaken for an unavailable command.
          (and (stringp data)
               (string-match-p
                (regexp-quote
                 "Failed to spawn process: No such file or directory (os error 2)")
                data)
               (string-match-p
                (regexp-quote "spawn_not_found . t") data))))))

(defun remote-backend-tramp-rpc--attribute-enabled-a
    (function vector property command arguments)
  "Check a target attribute command, caching success on its RPC process.
FUNCTION is the verified upstream availability check.  PROPERTY belongs to
the live process, so replacement transports cannot inherit an old result.
COMMAND and ARGUMENTS are the exact upstream probe for this release."
  (let* ((before-process
          (remote-backend-tramp-rpc--vec-connection-slot vector :process))
         (cache (and (processp before-process)
                     (process-live-p before-process)
                     (process-get before-process property)))
         (ttl remote-backend-tramp-rpc-attribute-probe-ttl))
    (if (and (consp cache)
             (numberp (car cache))
             (numberp ttl) (> ttl 0)
             (< (- (float-time) (car cache)) ttl))
        (cdr cache)
      (let* ((response
              (condition-case err
                  ;; A missing probe command is an expected answer, not a
                  ;; user-visible failure: `tramp-error' would otherwise echo
                  ;; "File is missing ... spawn_not_found" on the first save
                  ;; to every host without SELinux or ACL tools.
                  (let ((tramp-verbose 0)
                        (inhibit-message t)
                        (message-log-max nil))
                    (tramp-rpc--call
                     vector "process.run"
                     `((cmd . ,command)
                       (args . ,arguments)
                       (cwd . "/"))))
                (file-missing
                 (if (remote-backend-tramp-rpc--attribute-command-missing-p err)
                     :missing-command
                   nil))
                (error nil)))
             (code (cond
                    ((eq response :missing-command) 127)
                    ((listp response)
                     (alist-get 'exit_code response)))))
        (if (integerp code)
            (let* ((enabled (zerop code))
                   (after-process
                    (remote-backend-tramp-rpc--vec-connection-slot
                     vector :process)))
              (when (and (or enabled
                             (eq response :missing-command))
                         (numberp ttl) (> ttl 0)
                         (processp after-process)
                         (process-live-p after-process)
                         (or (null before-process)
                             (eq before-process after-process)))
                (process-put after-process property
                             (cons (float-time) enabled)))
              enabled)
          ;; A changed response shape is a package compatibility question;
          ;; retain the upstream implementation instead of inventing a state.
          (if response (funcall function vector) nil))))))

(defun remote-backend-tramp-rpc--acl-enabled-a (function vector)
  "Cache the verified upstream ACL availability check for VECTOR."
  (remote-backend-tramp-rpc--attribute-enabled-a
   function vector 'remote-backend-tramp-rpc--acl-available
   "getfacl" ["--version"]))

(defun remote-backend-tramp-rpc--selinux-enabled-a (function vector)
  "Cache the verified upstream SELinux availability check for VECTOR."
  (remote-backend-tramp-rpc--attribute-enabled-a
   function vector 'remote-backend-tramp-rpc--selinux-available
   "selinuxenabled" []))

(defun remote-backend-tramp-rpc--write-region-in-place-a
    (function start end filename &rest optional)
  "Call FUNCTION for FILENAME with its in-place metadata scope active."
  (let ((remote-backend-tramp-rpc--in-place-write-file
         (and (stringp filename) (expand-file-name filename))))
    (apply function start end filename optional)))

(defun remote-backend-tramp-rpc--uid-gid-in-place-a
    (function filename &optional uid gid)
  "Skip FUNCTION's chown of FILENAME after the verified in-place RPC write.
TRAMP's write skeleton chowns every non-visiting write to the owner it read
before writing.  The verified server truncates the existing inode, so that
owner is already in place; a new file keeps the server's default owner and
group, which is what a local Emacs write produces.  UID and GID are passed
through unchanged for every other caller."
  (if (and remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
           remote-backend-tramp-rpc--in-place-write-file
           (equal (expand-file-name filename)
                  remote-backend-tramp-rpc--in-place-write-file))
      nil
    (funcall function filename uid gid)))

(defun remote-backend-tramp-rpc--extended-attributes-in-place-a
    (function filename)
  "Keep FILENAME's existing metadata on the verified in-place RPC write.
The verified server's `file.write' uses `OpenOptions::truncate' on the same
inode.  TRAMP's generic skeleton reads and reapplies attributes for backends
that replace files, but those extra remote commands are unnecessary here."
  (if (and remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
           remote-backend-tramp-rpc--in-place-write-file
           (equal filename remote-backend-tramp-rpc--in-place-write-file))
      nil
    (funcall function filename)))

(defun remote-backend-tramp-rpc--transport-death-before-a
    (process vec event)
  "Notify Remote before tramp-rpc closes relays owned by PROCESS.
EVENT is the transport's process event.  Only the current, unclaimed
generation of VEC is reported: a generation upstream already claimed was
retired on purpose, and `remote-backend-tramp-rpc--retire-unless-alive'
reports its own."
  (let ((current (ignore-errors (tramp-rpc--get-connection vec))))
    (when (and (processp process)
               (eq process
                   (remote-backend-tramp-rpc--connection-slot current :process))
               (not (remote-backend-tramp-rpc--connection-slot
                     current :cleanup-started)))
      (remote-backend-tramp-rpc--report-transport-failure
       vec (format "tramp-rpc transport exited: %s" event)))))

(defun remote-backend-tramp-rpc--report-transport-failure (vec message)
  "Report a lost tramp-rpc transport for VEC to every Remote route using it.
MESSAGE explains the loss.  Reported before tramp-rpc closes the relays,
so LSP and watch resources stay attached to their workspace until Remote
has reopened the physical session.  An explicit TRAMP cleanup is a
disconnect, not a failure, and never schedules a reconnect."
  (when (and (not remote-backend-tramp-explicit-cleanup)
             (boundp 'remote-workspaces)
             (hash-table-p remote-workspaces)
             (> (hash-table-count remote-workspaces) 0)
             (remote-backend-tramp-rpc--private-compatible-p
              'tramp-rpc--connection-key))
    (let ((dead-key (tramp-rpc--connection-key vec))
          (reported (make-hash-table :test #'equal)))
      (dolist (workspace (hash-table-values remote-workspaces))
        (dolist (route (remote-workspace-routes workspace))
          (when (and (equal (remote-route-link-plugin-id route) "tramp-rpc")
                     (not (gethash (remote-route-link-id route) reported)))
            (when-let* ((connection (remote-connection-cached-p route))
                        (physical (remote-connection-handle connection)))
              (when (and (stringp physical)
                         (condition-case nil
                             (equal
                              (tramp-rpc--connection-key
                               (tramp-dissect-file-name physical))
                              dead-key)
                           (error nil)))
                (puthash (remote-route-link-id route) t reported)
                (remote-report-route-failure
                 route (list 'remote-transport-error message))))))))))

;;;; Connection buffer hygiene

;; The verified tramp-rpc release leaves three kinds of client state behind
;; a closed connection.  TRAMP's generic cleanup kills `tramp-buffer-name' but not the
;; sibling "NAME stderr" buffer that `tramp-rpc--start-server-process' creates,
;; and that buffer's stderr relay process outlives its transport long enough to
;; collide with a reconnect ("NAME stderr<1>").  Each file-notify descriptor is
;; a `make-pipe-process', which always creates a buffer named after the process
;; even without `:buffer', so watching leaves a "tramp-rpc" buffer forever.

(defconst remote-backend-tramp-rpc--descriptor-buffer-name "tramp-rpc"
  "Buffer `make-pipe-process' creates for tramp-rpc watch descriptors.")

(defun remote-backend-tramp-rpc--current-stderr-buffer (vec)
  "Return the stderr buffer of VEC's live tramp-rpc generation, or nil."
  (remote-backend-tramp-rpc--vec-connection-slot vec :stderr-buffer))

(defun remote-backend-tramp-rpc--release-stderr (buffer &optional kill)
  "Delete BUFFER's stderr relay process; when KILL, also kill BUFFER."
  (when (buffer-live-p buffer)
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (when kill
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun remote-backend-tramp-rpc--generation-cleanup-after-a
    (process vec &rest _)
  "Release the stderr relay of PROCESS's finished generation for VEC.
The buffer stays for diagnostics until TRAMP cleanup or a reconnect reuses
it.  A late cleanup of an old generation never touches the replacement's
relay, which reuses the same buffer."
  (when-let* ((connection
               (and (processp process)
                    (remote-backend-tramp-rpc--private-compatible-p
                     'tramp-rpc--process-connection)
                    (tramp-rpc--process-connection process)))
              (stderr (remote-backend-tramp-rpc--connection-slot
                       connection :stderr-buffer)))
    (unless (eq stderr (remote-backend-tramp-rpc--current-stderr-buffer vec))
      (remote-backend-tramp-rpc--release-stderr stderr))))

(defun remote-backend-tramp-rpc--kill-idle-descriptor-buffer ()
  "Kill the shared watch-descriptor buffer when no process uses it."
  (when-let* ((buffer (get-buffer
                       remote-backend-tramp-rpc--descriptor-buffer-name)))
    (unless (or (get-buffer-process buffer)
                (buffer-local-value 'buffer-file-name buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun remote-backend-tramp-rpc--detach-descriptor-buffer-a (descriptor)
  "Detach and drop the buffer `make-pipe-process' gave DESCRIPTOR.
Return DESCRIPTOR.  The descriptor never receives output; upstream already
intends it to have no buffer."
  (when (processp descriptor)
    (let ((buffer (process-buffer descriptor)))
      (set-process-buffer descriptor nil)
      (when (and (buffer-live-p buffer)
                 (equal (buffer-name buffer)
                        remote-backend-tramp-rpc--descriptor-buffer-name)
                 (not (eq buffer (current-buffer))))
        (remote-backend-tramp-rpc--kill-idle-descriptor-buffer))))
  descriptor)

(defun remote-backend-tramp-rpc--cleanup-connection-buffers-h (vec)
  "Kill tramp-rpc client buffers TRAMP's cleanup of VEC leaves behind.
Runs from `tramp-cleanup-connection-hook'.  A buffer already reused by a live
replacement generation is kept."
  (when (tramp-file-name-p vec)
    (let* ((full-cleanup (and remote-backend-tramp-explicit-cleanup
                              (equal (tramp-file-name-method vec) "rpc")))
           (pattern (concat "\\`"
                            (regexp-quote (tramp-buffer-name vec))
                            (if full-cleanup "\\(?: stderr\\)?" " stderr")
                            "\\(?:<[0-9]+>\\)?\\'"))
           (current (remote-backend-tramp-rpc--current-stderr-buffer vec))
           (current-main
            (and full-cleanup
                 (remote-backend-tramp-rpc--vec-connection-slot vec :buffer))))
      (dolist (buffer (buffer-list))
        (when (and (string-match-p pattern (buffer-name buffer))
                   (not (buffer-local-value 'buffer-file-name buffer))
                   (not (eq buffer current))
                   (not (eq buffer current-main)))
          (remote-backend-tramp-rpc--release-stderr buffer t)))))
  (remote-backend-tramp-rpc--kill-idle-descriptor-buffer))

(defun remote-backend-tramp-rpc--cleanup-all-buffers-h ()
  "Kill tramp-rpc client buffers left after `tramp-cleanup-all-connections'."
  ;; A failed or already-removed generation need not be in any connection
  ;; table.  Sweep its owned namespace as well as the current generations.
  (dolist (buffer (buffer-list))
    (when (and (string-match-p
                "\\`\\*tramp/rpc .*\\*\\(?: stderr\\)?\\(?:<[0-9]+>\\)?\\'"
                (buffer-name buffer))
               (not (buffer-local-value 'buffer-file-name buffer)))
      (remote-backend-tramp-rpc--release-stderr buffer t)))
  (remote-backend-tramp-rpc--kill-idle-descriptor-buffer))

;;;; Directory search answers

(defvar remote-backend-tramp-rpc--directory-access
  (make-hash-table :test #'equal)
  "Directory search answers: DIRECTORY -> (TIMESTAMP PROCESS . ANSWER).")

(defvar remote-backend-tramp-rpc--directory-access-hits 0
  "Directory search checks answered without a round trip.")

(defconst remote-backend-tramp-rpc--directory-access-limit 2048
  "Entries kept before the directory search table starts over.")

(defun remote-backend-tramp-rpc--directory-access-key (filename)
  "Return the table key for directory FILENAME."
  (file-name-as-directory (expand-file-name filename)))

(defun remote-backend-tramp-rpc--directory-access-process (key)
  "Return the live RPC transport serving KEY, or nil."
  (when-let* ((process (remote-backend-tramp-rpc--vec-connection-slot
                        (ignore-errors (tramp-dissect-file-name key))
                        :process))
              ((process-live-p process)))
    process))

(defun remote-backend-tramp-rpc--directory-access-a (function filename mode)
  "Answer FUNCTION's directory search check of FILENAME from memory.
MODE `directory' asks whether FILENAME can be entered; every other MODE
goes to FUNCTION.  tramp-rpc keeps that answer as a TRAMP file property,
which each `process-file' flushes, so a tool running one command after
another (Magit checks `default-directory' before every git) asked the
server each time.  An answer here lives on its transport generation,
within tramp-rpc's own metadata TTL, which already honours
`remote-file-name-inhibit-cache' and a degraded watcher, and until a
filesystem event touches the directory."
  (if (not (eq mode 'directory))
      (funcall function filename mode)
    (let* ((key (remote-backend-tramp-rpc--directory-access-key filename))
           (entry (gethash key remote-backend-tramp-rpc--directory-access))
           (process (and entry
                         (remote-backend-tramp-rpc--directory-access-process
                          key))))
      (if (and entry process
               (eq (cadr entry) process)
               (tramp-rpc--cache-entry-valid-p (car entry)))
          (progn
            (cl-incf remote-backend-tramp-rpc--directory-access-hits)
            (cddr entry))
        (let ((answer (funcall function filename mode)))
          ;; The check itself may have opened the generation.
          (when-let* (((memq answer '(t nil)))
                      (process
                       (remote-backend-tramp-rpc--directory-access-process
                        key)))
            (when (>= (hash-table-count remote-backend-tramp-rpc--directory-access)
                      remote-backend-tramp-rpc--directory-access-limit)
              (clrhash remote-backend-tramp-rpc--directory-access))
            (puthash key (cons (float-time) (cons process answer))
                     remote-backend-tramp-rpc--directory-access))
          answer)))))

(defun remote-backend-tramp-rpc--directory-access-events-h (vec paths)
  "Forget directory search answers that filesystem events on VEC can change.
PATHS are the changed TRAMP names, or nil when the server lost events.  A
changed path drops its own answer and, for a directory that may have gone,
every answer below it."
  (let ((host (and (tramp-file-name-p vec) (tramp-make-tramp-file-name vec ""))))
    (when (and host
               (> (hash-table-count remote-backend-tramp-rpc--directory-access) 0))
      (let ((prefixes (if paths
                          (mapcar #'file-name-as-directory paths)
                        (list host)))
            stale)
        (maphash (lambda (key _entry)
                   (when (cl-some (lambda (prefix) (string-prefix-p prefix key))
                                  prefixes)
                     (push key stale)))
                 remote-backend-tramp-rpc--directory-access)
        (dolist (key stale)
          (remhash key remote-backend-tramp-rpc--directory-access))))))

;;;; Ancestor marker prefetch

(defvar remote-backend-tramp-rpc--ancestor-prefetch nil
  "Non-nil when the verified release has every seam the prefetch uses.
Set by `remote-backend-tramp-rpc-install'.")

(defconst remote-backend-tramp-rpc--ancestor-depth-limit 32
  "Ancestors listed by one prefetch, well inside the server's batch limit.")

(defun remote-backend-tramp-rpc--ancestors (localname)
  "Return LOCALNAME and its ancestors as directory names, nearest first."
  (let ((directory (file-name-as-directory localname))
        result)
    (while (and directory
                (< (length result) remote-backend-tramp-rpc--ancestor-depth-limit))
      (push directory result)
      (setq directory
            (unless (equal directory "/")
              (file-name-directory (directory-file-name directory)))))
    (nreverse result)))

(defun remote-backend-tramp-rpc-prefetch-ancestor-markers
    (route directory names)
  "Learn which NAMES exist above DIRECTORY on tramp-rpc ROUTE in one batch.
Each ancestor is listed by name only.  A marker missing from its listing
is recorded in tramp-rpc's own stat cache as missing, under its usual TTL
and invalidation, so a consumer walking upwards (Projectile, VC, direnv)
asks nothing more for it; a marker that exists keeps its real stat.  A
listing the server refuses (too large) leaves that directory to ordinary
lookups.  Return non-nil when the batch answered."
  (when (and remote-backend-tramp-rpc--ancestor-prefetch
             (equal (remote-route-link-plugin-id route) "tramp-rpc"))
    (condition-case nil
        (let* ((physical (file-name-as-directory
                          (remote-backend-project-file-name route directory)))
               (vec (tramp-dissect-file-name physical nil))
               (local (tramp-file-name-localname vec))
               (ancestors (remote-backend-tramp-rpc--ancestors
                           (if (string-empty-p local) "/" local)))
               (results
                (tramp-rpc--call-batch
                 vec (mapcar (lambda (ancestor)
                               (cons "dir.list"
                                     (tramp-rpc--encode-path ancestor)))
                             ancestors)))
               (target-id (remote-route-target-id route)))
          (when (= (length results) (length ancestors))
            (cl-loop
             for ancestor in ancestors
             for listing in results
             unless (or (tramp-rpc--batch-error-p listing)
                        (not (listp listing)))
             do (let ((present (mapcar #'tramp-rpc--decode-filename listing)))
                  (when (fboundp 'remote-fs-remember-ancestor-listing)
                    (remote-fs-remember-ancestor-listing
                     (remote-make-file-name target-id ancestor) present))
                  (dolist (name names)
                    (unless (member name present)
                      (let ((file (concat ancestor name)))
                        (tramp-rpc--cache-file-stat-result vec file nil)
                        (tramp-rpc--cache-file-stat-result vec file nil t))))))
            t))
      (error nil))))

(add-hook 'remote-ancestor-prefetch-functions
          #'remote-backend-tramp-rpc-prefetch-ancestor-markers)

;;;; Version-gated shims

(defconst remote-backend-tramp-rpc--shims
  '((local-controlmaster-path
     tramp-rpc--controlmaster-socket-path :around
     remote-backend-tramp-rpc--local-controlmaster-path-a)
    (local-controlmaster-path
     tramp-rpc--ensure-controlmaster-directory :around
     remote-backend-tramp-rpc--local-controlmaster-path-a)
    (local-ssh-pty
     tramp-rpc--make-direct-ssh-pty-process :around
     remote-backend-tramp-rpc--local-controlmaster-path-a)
    (adapter-timeout
     tramp-rpc--call :around remote-backend-tramp-rpc--adapter-timeout-a
     tramp-rpc--call-with-timeout tramp-rpc--get-connection
     tramp-rpc--invalidate-timed-out-connection)
    (attribute-probe-cache
     tramp-rpc--acl-enabled-p :around remote-backend-tramp-rpc--acl-enabled-a
     tramp-rpc--call tramp-rpc--get-connection)
    (attribute-probe-cache
     tramp-rpc--selinux-enabled-p :around
     remote-backend-tramp-rpc--selinux-enabled-a
     tramp-rpc--call tramp-rpc--get-connection)
    (in-place-write-metadata
     tramp-rpc-handle-write-region :around
     remote-backend-tramp-rpc--write-region-in-place-a)
    (in-place-write-metadata
     file-extended-attributes :around
     remote-backend-tramp-rpc--extended-attributes-in-place-a
     tramp-rpc-handle-write-region)
    (in-place-write-metadata
     tramp-rpc-handle-set-file-uid-gid :around
     remote-backend-tramp-rpc--uid-gid-in-place-a
     tramp-rpc-handle-write-region)
    (transport-death-notice
     tramp-rpc--connection-transport-death :before
     remote-backend-tramp-rpc--transport-death-before-a
     tramp-rpc--connection-key tramp-rpc--get-connection)
    (stderr-relay-release
     tramp-rpc--cleanup-connection-generation :after
     remote-backend-tramp-rpc--generation-cleanup-after-a
     tramp-rpc--process-connection)
    (watch-descriptor-buffer
     tramp-rpc--make-file-notify-descriptor :filter-return
     remote-backend-tramp-rpc--detach-descriptor-buffer-a)
    (directory-access-cache
     tramp-rpc--file-access-p :around
     remote-backend-tramp-rpc--directory-access-a
     tramp-rpc--cache-entry-valid-p tramp-rpc--get-connection))
  "Workarounds verified against one exact tramp-rpc release.
Each row is (GROUP SYMBOL HOW ADVICE . REQUIRES).  A row is installed only
on the verified release, when SYMBOL and every private REQUIRES keep their
contract, and when GROUP's own condition holds.  Every other release keeps
upstream behaviour unchanged.")

(defconst remote-backend-tramp-rpc--retired-shims
  '((tramp-rpc-deploy--arch-to-rust-target
     . remote-backend-tramp-rpc--arch-to-rust-target-a)
    (tramp-rpc-handle-make-process
     . remote-backend-tramp-rpc--local-relay-cwd-a)
    (tramp-rpc--compute-remote-path
     . remote-backend-tramp-rpc--compute-remote-path-a)
    (tramp-rpc--deliver-process-output
     . remote-backend-tramp-rpc--closed-relay-exit-a))
  "Advice of earlier adapter versions whose fix is now upstream.
0.16.0 maps the published ARM releases, starts local relays from a client
directory, checks PATH in one batch (returning a different shape), and
holds a process exit until its queued output is written.  Install removes
these by name, so a live upgrade cannot keep an old shim on new code.")

(defun remote-backend-tramp-rpc--shim-group-condition-p (group)
  "Return non-nil when shim GROUP's own preconditions hold."
  (pcase group
    ('in-place-write-metadata
     (and remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
          (= emacs-major-version 31)
          (fboundp 'file-extended-attributes)
          (equal (remote-backend-tramp-rpc--upstream-arity
                  'file-extended-attributes)
                 '(1 . 1))))
    ((or 'adapter-timeout 'attribute-probe-cache 'transport-death-notice
         'stderr-relay-release 'directory-access-cache)
     (remote-backend-tramp-rpc--connection-readable-p))
    (_ t)))

(defun remote-backend-tramp-rpc--shim-state (shim verified)
  "Return the install state of SHIM on a release VERIFIED or not.
The state is `active', `absent', `unverified', `contract' or `condition'."
  (pcase-let ((`(,group ,symbol ,_how ,_advice . ,requires) shim))
    (cond
     ((not (fboundp symbol)) 'absent)
     ((not verified) 'unverified)
     ((not (cl-every
            (lambda (required)
              (or (not (assq required
                             remote-backend-tramp-rpc--private-contracts))
                  (remote-backend-tramp-rpc--private-compatible-p required)))
            (cons symbol requires)))
      'contract)
     ((not (remote-backend-tramp-rpc--shim-group-condition-p group))
      'condition)
     (t 'active))))

(defun remote-backend-tramp-rpc--install-shims (verified)
  "Install exactly the shims valid on a release VERIFIED or not.
All shim advice, current and retired, is removed first."
  (pcase-dolist (`(,symbol . ,advice) remote-backend-tramp-rpc--retired-shims)
    (when (fboundp symbol)
      (advice-remove symbol advice)))
  ;; Remove every row before judging any: a group's condition can read a
  ;; function that another row of the same group advises.
  (pcase-dolist (`(,_group ,symbol ,_how ,advice . ,_)
                 remote-backend-tramp-rpc--shims)
    (when (fboundp symbol)
      (advice-remove symbol advice)))
  (let ((active
         (seq-filter
          (lambda (shim)
            (eq (remote-backend-tramp-rpc--shim-state shim verified) 'active))
          remote-backend-tramp-rpc--shims)))
    (pcase-dolist (`(,_group ,symbol ,how ,advice . ,_) active)
      (advice-add symbol how advice))))

(defun remote-backend-tramp-rpc--shim-report ()
  "Describe each version-gated shim and whether it is installed."
  (let ((verified (remote-backend-tramp-rpc--verified-release-p)))
    (mapcar
     (lambda (shim)
       (pcase-let ((`(,group ,symbol ,_how ,advice . ,_) shim))
         (list :group group :symbol symbol
               :state (remote-backend-tramp-rpc--shim-state shim verified)
               :installed (and (fboundp symbol)
                               (advice-member-p advice symbol)
                               t))))
     remote-backend-tramp-rpc--shims)))

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
  (let (release)
    (when (require 'tramp-rpc-deploy nil t)
      (setq release (remote-backend-tramp-rpc-release-contract))
      ;; Environment capsules are the single owner of direnv state.
      (when (boundp 'tramp-rpc-use-direnv)
        (setq tramp-rpc-use-direnv nil))
      ;; package-vc uses a git checkout even for a release tag.  Any exact
      ;; release may use its matching published server instead of compiling
      ;; Linux binaries on local Darwin.
      (when (and (boundp 'tramp-rpc-deploy-git-build-policy)
                 (eq tramp-rpc-deploy-git-build-policy 'auto)
                 (plist-get release :release-checkout))
        (setq tramp-rpc-deploy-git-build-policy 'release)))
    (let ((verified (and release
                         (remote-backend-tramp-rpc--verified-release-p
                          release))))
      (setq remote-backend-tramp-rpc--connection-shape
            (remote-backend-tramp-rpc--connection-shape-check)
            remote-backend-tramp-rpc--probe-slot
            (and verified remote-backend-tramp-rpc--connection-shape
                 (ignore-errors
                   (cl-struct-slot-offset 'tramp-rpc-connection 'probing))))
      (remote-backend-tramp-rpc--install-shims verified)
      (setq remote-backend-tramp-rpc--ancestor-prefetch
            (and verified
                 (cl-every #'remote-backend-tramp-rpc--private-compatible-p
                           '(tramp-rpc--call-batch
                             tramp-rpc--encode-path
                             tramp-rpc--decode-filename
                             tramp-rpc--batch-error-p
                             tramp-rpc--cache-file-stat-result))))
      ;; Buffer hygiene.  The cleanup hooks only use public TRAMP names and
      ;; read a generation only through the verified accessors.
      (clrhash remote-backend-tramp-rpc--directory-access)
      (remove-hook 'tramp-rpc-fs-events-functions
                   #'remote-backend-tramp-rpc--directory-access-events-h)
      (when (advice-member-p #'remote-backend-tramp-rpc--directory-access-a
                             'tramp-rpc--file-access-p)
        (add-hook 'tramp-rpc-fs-events-functions
                  #'remote-backend-tramp-rpc--directory-access-events-h))
      (when (featurep 'tramp-rpc)
        (add-hook 'tramp-cleanup-connection-hook
                  #'remote-backend-tramp-rpc--cleanup-connection-buffers-h t)
        (add-hook 'tramp-cleanup-all-connections-hook
                  #'remote-backend-tramp-rpc--cleanup-all-buffers-h t))
      (remote-backend-tramp-rpc-metadata-install
       verified
       (lambda (contract)
         (equal (remote-backend-tramp-rpc--upstream-arity (car contract))
                (cdr contract)))))))

(with-eval-after-load 'tramp-rpc-process
  (remote-backend-tramp-rpc-install))

(with-eval-after-load 'tramp-rpc
  (remote-backend-tramp-rpc-install))

(defun remote-backend-tramp-rpc-project (file-name link _route)
  "Project logical FILE-NAME through tramp-rpc LINK."
  (remote-backend-tramp-file-name
   (remote-fs-localname file-name) link "rpc"))

(defun remote-backend-tramp-rpc--seed-named-current-home (name link)
  "Seed the current account's `~USER' home from verified RPC system info.
The verified client uses `getent' for a named user when the connection vector
omits USER.  macOS has no `getent', although the server already supplied the
current HOME.  Confirm the effective account through target `id -un' before
associating that HOME with a named account.  Other names keep upstream lookup."
  (when-let*
      ((user (remote-backend-tramp--named-home-user name))
       ((remote-backend-tramp-rpc--verified-release-p))
       ((remote-backend-tramp-rpc--private-compatible-p
         'tramp-rpc--cached-system-info))
       ((remote-backend-tramp-rpc--private-compatible-p 'tramp-rpc--call))
       ((remote-backend-tramp-rpc--private-compatible-p
         'tramp-rpc--decode-output))
       (physical (remote-backend-tramp-file-name name link "rpc"))
       (vec (tramp-dissect-file-name physical nil))
       ((not (tramp-get-connection-property vec (concat "~" user) nil)))
       (info (tramp-rpc--cached-system-info vec))
       (home (or (alist-get 'home info)
                 (alist-get "home" info nil nil #'equal)))
       ((stringp home))
       ((file-name-absolute-p home))
       ((not (file-remote-p home))))
    (let* ((result
            (condition-case nil
                (tramp-rpc--call
                 vec "process.run"
                 '((cmd . "id") (args . ["-un"]) (cwd . "/")))
              (error nil)))
           (output
            (and (equal (alist-get 'exit_code result) 0)
                 (tramp-rpc--decode-output (alist-get 'stdout result)))))
      (when (and (stringp output)
                 (equal (string-trim output) user))
        (tramp-set-connection-property vec (concat "~" user) home)))))

(defun remote-backend-tramp-rpc-expand-localname
    (name directory link _route)
  "Resolve target-native NAME against DIRECTORY through tramp-rpc LINK."
  (remote-backend-tramp-rpc--seed-named-current-home name link)
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
    (cond
     ((and (eq (car-safe error) 'remote-file-error)
           (string-match-p "timeout waiting for rpc response" message))
      (list :scope 'transport
            :phase phase
            :retryable t
            :status 'failed
            :error error))
     ;; No server binary can be produced for this target at all: the client
     ;; cannot build one and the installed client is not allowed to fall back
     ;; on a published artifact.  That is a property of this client/target
     ;; pair, not a transient failure.  Retrying it on the ordinary cooldown
     ;; costs a fresh bootstrap connection and another attempted build on
     ;; every remote operation, which is exactly the cost the cooldown exists
     ;; to avoid, so record the backend as incompatible instead.  Clearing
     ;; route health (a config reload or `remote-reset') re-evaluates it.
     ((or (and (string-match-p "failed to obtain tramp-rpc-server" message)
               (string-match-p
                (rx (or "cannot cross-compile" "unknown architecture"))
                message))
          ;; The deploy step also signals an unmapped target on its own.
          (string-match-p (rx bos "unknown architecture") message))
      (list :scope 'backend
            :phase phase
            :retryable nil
            :status 'incompatible
            :error error))
     ((string-match-p
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
            :error error)))))

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
   :client-shell #'remote-backend-tramp-client-shell
   :make-network-process #'remote-backend-tramp-network
   :open-network-stream #'remote-backend-tramp-stream
   :port-forward #'remote-backend-tramp-forward
   :classify-error #'remote-backend-tramp-rpc-classify-error
   :program-form 'absolute
   :describe
   (lambda ()
     '(:kind tramp-rpc
       :session-owner tramp-rpc
       :spawn-program absolute
       :file-operation-cost batched
       ;; One stat is still one round trip; ancestor walks batch here.
       :ancestor-prefetch t
       ;; The server pushes inotify events over the RPC stream, and both
       ;; stat spellings report the same integer-second mtime.
       :file-watch-cost push
       :mtime-compare exact
       ;; Input to a target process goes through the reliable RPC queue.
       :process-write-cost queued))))

(defun remote-backend-tramp-rpc-invalidate-cache (physical &optional subtree)
  "Forget RPC metadata for PHYSICAL after a target-side process changes it.
TRAMP-RPC keeps its own stat and existence caches in addition to TRAMP's
file properties.  Keep that private cache boundary inside this adapter."
  (if (and (fboundp 'tramp-rpc--invalidate-cache-for-path)
           (or (not subtree)
               (fboundp 'tramp-rpc--invalidate-cache-for-subtree)))
      (if subtree
          (tramp-rpc--invalidate-cache-for-subtree physical)
        (tramp-rpc--invalidate-cache-for-path physical))
    (with-parsed-tramp-file-name physical nil
      (tramp-flush-file-properties v localname)
      (tramp-flush-directory-properties
       v (if subtree localname (file-name-directory localname))))))

(defun remote-backend-tramp-rpc-transport-process (physical)
  "Return the current RPC transport process for PHYSICAL, if connected."
  (remote-backend-tramp-rpc--vec-connection-slot
   (tramp-dissect-file-name physical) :process))

(remote-backend-tramp-rpc-install)

(provide 'remote-backend-tramp-rpc)
;;; remote-backend-tramp-rpc.el ends here
