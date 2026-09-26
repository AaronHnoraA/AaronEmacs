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
(declare-function tramp-rpc--call "tramp-rpc"
                  (vec method params &optional connection))
(declare-function tramp-rpc--decode-output "tramp-rpc" (data encoding))
(declare-function tramp-rpc--effective-remote-path-spec "tramp-rpc" (vec))
(declare-function tramp-rpc--expand-remote-path-entry "tramp-rpc"
                  (vec entry))
(declare-function tramp-rpc--append-path-entries "tramp-rpc" (entries result))
(declare-function tramp-rpc--fetch-default-remote-path "tramp-rpc" (vec))
(declare-function tramp-rpc--fetch-remote-exec-path "tramp-rpc" (vec))
(declare-function tramp-rpc--call-with-timeout "tramp-rpc"
                  (vec method params total-timeout poll-interval
                       &optional connection))
(declare-function tramp-rpc--connection-key "tramp-rpc" (vec))
(declare-function tramp-rpc--get-connection "tramp-rpc" (vec))
(declare-function tramp-rpc--acl-enabled-p "tramp-rpc" (vec))
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

(defconst remote-backend-tramp-rpc--verified-client-version "0.13.1"
  "tramp-rpc release whose private compatibility shims were tested here.")

(defconst remote-backend-tramp-rpc--verified-client-revision
  "e1d4632d576ecf2472c321de1e713b776ea2b78f"
  "Exact tramp-rpc client commit tested by private compatibility shims.")

(defconst remote-backend-tramp-rpc--private-contracts
  '((tramp-rpc--cached-system-info . (1 . 1))
    (tramp-rpc--call . (3 . 4))
    (tramp-rpc--call-with-timeout . (5 . 6))
    (tramp-rpc--decode-output . (2 . 2))
    (tramp-rpc--compute-remote-path . (1 . 1))
    (tramp-rpc--effective-remote-path-spec . (1 . 1))
    (tramp-rpc--expand-remote-path-entry . (2 . 2))
    (tramp-rpc--append-path-entries . (2 . 2))
    (tramp-rpc--fetch-default-remote-path . (1 . 1))
    (tramp-rpc--fetch-remote-exec-path . (1 . 1))
    (tramp-rpc--connection-key . (1 . 1))
    (tramp-rpc--get-connection . (1 . 1))
    (tramp-rpc--controlmaster-socket-path . (1 . 1))
    (tramp-rpc--ensure-controlmaster-directory . (0 . 0))
    (tramp-rpc--acl-enabled-p . (1 . 1))
    (tramp-rpc--selinux-enabled-p . (1 . 1))
    (tramp-rpc-handle-write-region . (3 . 7))
    (tramp-rpc--deliver-process-output . (4 . 4))
    (tramp-rpc--connection-transport-death . (3 . 3))
    (tramp-rpc-deploy--arch-to-rust-target . (1 . 1)))
  "Private upstream seams isolated by this backend adapter.")

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
     :architecture-map
     (and (fboundp 'tramp-rpc-deploy--arch-to-rust-target)
          (advice-member-p
           #'remote-backend-tramp-rpc--arch-to-rust-target-a
           'tramp-rpc-deploy--arch-to-rust-target)
          t)
     :local-relay-cwd
     (and (fboundp 'tramp-rpc-handle-make-process)
          (advice-member-p
           #'remote-backend-tramp-rpc--local-relay-cwd-a
           'tramp-rpc-handle-make-process)
          t)
     :local-controlmaster-path
     (and (fboundp 'tramp-rpc--controlmaster-socket-path)
          (advice-member-p
           #'remote-backend-tramp-rpc--local-controlmaster-path-a
           'tramp-rpc--controlmaster-socket-path)
          t)
     :adapter-timeout
     (and (fboundp 'tramp-rpc--call)
          (advice-member-p
           #'remote-backend-tramp-rpc--adapter-timeout-a
           'tramp-rpc--call)
          t)
     :batched-path-check
     (and (fboundp 'tramp-rpc--compute-remote-path)
          (advice-member-p
           #'remote-backend-tramp-rpc--compute-remote-path-a
           'tramp-rpc--compute-remote-path)
          t)
     :attribute-probe-cache
     (and (fboundp 'tramp-rpc--acl-enabled-p)
          (fboundp 'tramp-rpc--selinux-enabled-p)
          (advice-member-p
           #'remote-backend-tramp-rpc--acl-enabled-a
           'tramp-rpc--acl-enabled-p)
          (advice-member-p
           #'remote-backend-tramp-rpc--selinux-enabled-a
           'tramp-rpc--selinux-enabled-p)
          t)
     :in-place-write-metadata
     (and remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
          (fboundp 'tramp-rpc-handle-write-region)
          (advice-member-p
           #'remote-backend-tramp-rpc--write-region-in-place-a
           'tramp-rpc-handle-write-region)
          (advice-member-p
           #'remote-backend-tramp-rpc--extended-attributes-in-place-a
           'file-extended-attributes)
          t)
     :closed-relay-exit-race
     (and (fboundp 'tramp-rpc--deliver-process-output)
          (advice-member-p
           #'remote-backend-tramp-rpc--closed-relay-exit-a
           'tramp-rpc--deliver-process-output)
          t))))

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
  "Skip redundant ACL/SELinux read and restore on verified in-place writes.
The pinned tramp-rpc server truncates an existing inode with `file.write',
which preserves its extended attributes.  Unknown client or Emacs versions
retain TRAMP's ordinary metadata handling."
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

(defun remote-backend-tramp-rpc--git-output (root &rest arguments)
  "Run git ARGUMENTS in ROOT and return trimmed stdout, or nil.
ROOT is this package's own checkout on the client, so the call has to stay on
the client.  It runs from the backend probe while the target connection is
still opening: a `default-directory' inherited from the caller would route
`process-file' back into that half-open connection, and the whole operation
would fail with `remote-connection-busy' instead of reporting the contract."
  (when-let* ((root root)
              (git (remote-client-executable-find "git")))
    (with-temp-buffer
      (let* ((default-directory temporary-file-directory)
             (process-environment (remote-client-process-environment))
             (exec-path (remote-client-exec-path))
             (status
              (apply #'process-file git nil t nil "-C" root arguments)))
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

(defun remote-backend-tramp-rpc--verified-release-p (&optional release)
  "Return non-nil for the exact tramp-rpc RELEASE tested by private shims."
  (let ((release (or release (remote-backend-tramp-rpc-release-contract))))
    (and (equal (plist-get release :client-version)
                remote-backend-tramp-rpc--verified-client-version)
         (plist-get release :git-checkout)
         (equal (plist-get release :revision)
                remote-backend-tramp-rpc--verified-client-revision)
         (plist-get release :release-checkout))))

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

(defun remote-backend-tramp-rpc--local-controlmaster-path-a
    (function &rest arguments)
  "Resolve tramp-rpc's SSH control socket on this Emacs client.
The verified client expands `~/.ssh/tramp-rpc/%C' under the current buffer's
`default-directory'.  Direct SSH PTYs call this from a target buffer, where
that expansion otherwise points to the target account's home and SSH exits
before starting a shell.  The control socket and its directory live locally."
  (let ((default-directory temporary-file-directory)
        (process-environment (remote-client-process-environment))
        (exec-path (remote-client-exec-path))
        (remote-current-adapter-id nil)
        (remote-current-route nil))
    (apply function arguments)))

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
        (let ((generation
               (and timeout
                    (or connection (tramp-rpc--get-connection vector)))))
          (condition-case err
              (tramp-rpc--call-with-timeout
               vector method params timeout 0.1 connection)
            (remote-file-error
             (when (and generation
                        (string-match-p
                         "[Tt]imeout waiting for RPC response"
                         (error-message-string err))
                        (eq generation (tramp-rpc--get-connection vector)))
               ;; A live but unresponsive SSH process must not be reused by
               ;; the reconnect job.  Its sentinel releases relays and watches.
               (when-let* ((process (plist-get generation :process)))
                 (when (process-live-p process)
                   (ignore-errors (delete-process process)))))
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
  (let* ((before (tramp-rpc--get-connection vector))
         (before-process (plist-get before :process))
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
                   (after (tramp-rpc--get-connection vector))
                   (after-process (plist-get after :process)))
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

(defun remote-backend-tramp-rpc--extended-attributes-in-place-a
    (function filename)
  "Keep FILENAME's existing metadata on the verified in-place RPC write.
The 0.13.1 server's `file.write' uses `OpenOptions::truncate' on the same
inode.  TRAMP's generic skeleton reads and reapplies attributes for backends
that replace files, but those extra remote commands are unnecessary here."
  (if (and remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
           remote-backend-tramp-rpc--in-place-write-file
           (equal filename remote-backend-tramp-rpc--in-place-write-file))
      nil
    (funcall function filename)))

(defconst remote-backend-tramp-rpc--path-directory-script
  "for p do if [ -d \"$p\" ]; then printf 1; else printf 0; fi; done"
  "Return one existence bit per target PATH entry without shell interpolation.")

(defun remote-backend-tramp-rpc--existing-paths (vector paths)
  "Return (t . EXISTING) for target PATHS, or nil on unsupported output.
The shell receives directory names as positional arguments, so spaces and
shell metacharacters in a configured PATH cannot change the probe program."
  (if (null paths)
      (cons t nil)
    (let* ((result
            (tramp-rpc--call
             vector "process.run"
             `((cmd . "/bin/sh")
               (args . ,(vconcat
                         (list "-c"
                               remote-backend-tramp-rpc--path-directory-script
                               "sh")
                         paths))
               (cwd . "/"))))
           (bits
            (when (equal (alist-get 'exit_code result) 0)
              (tramp-rpc--decode-output
               (alist-get 'stdout result)
               (alist-get 'stdout_encoding result)))))
      (when (and (stringp bits)
                 (= (length bits) (length paths))
                 (string-match-p "\\`[01]+\\'" bits))
        (cons t
              (cl-loop for path in paths
                       for bit across bits
                       when (eq bit ?1)
                       collect path))))))

(defun remote-backend-tramp-rpc--compute-remote-path-a (function vector)
  "Compute VECTOR's configured PATH with one directory probe.
This mirrors tramp-rpc 0.13.1's PATH assembly only while its exact release
and all private helper signatures match.  Any unexpected target response
returns to FUNCTION's ordinary directory-by-directory implementation."
  (let ((checked
         (condition-case nil
             (let (own-path default-path result)
               (dolist (entry (tramp-rpc--effective-remote-path-spec vector))
                 (setq entry
                       (tramp-rpc--expand-remote-path-entry vector entry))
                 (cond
                  ((eq entry 'tramp-default-remote-path)
                   (unless default-path
                     (setq default-path
                           (tramp-rpc--fetch-default-remote-path vector)))
                   (setq result
                         (tramp-rpc--append-path-entries default-path result)))
                  ((memq entry '(tramp-own-remote-path
                                 tramp-rpc-own-remote-path))
                   (unless own-path
                     (setq own-path
                           (or (tramp-rpc--fetch-remote-exec-path vector) '())))
                   (setq result
                         (tramp-rpc--append-path-entries own-path result)))
                  ((stringp entry)
                   (setq result
                         (tramp-rpc--append-path-entries
                          (list entry) result)))))
               (remote-backend-tramp-rpc--existing-paths vector result))
           (error nil))))
    (if checked (cdr checked) (funcall function vector))))

(defun remote-backend-tramp-rpc--transport-death-before-a
    (process vec event)
  "Notify Remote before tramp-rpc closes relays owned by PROCESS.
The early notification keeps LSP and watch resources attached to their
workspace until Remote has reopened the physical session."
  (when (and (boundp 'remote-workspaces)
             (hash-table-p remote-workspaces)
             (processp process)
             (eq process
                 (plist-get (tramp-rpc--get-connection vec) :process))
             (not (process-get process :tramp-rpc-cleanup-started)))
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
                 route
                 (list 'remote-transport-error
                       (format "tramp-rpc transport exited: %s" event)))))))))))

(defun remote-backend-tramp-rpc--closed-relay-exit-a
    (function process stdout stderr stderr-buffer)
  "Ignore a late RPC output chunk only after PROCESS has exited.
On tramp-rpc 0.13.1 a large, short-lived `ls' may close its local cat relay
while a queued output callback is still entering `process-send-string'.  The
underlying write then signals an error from a timer, although the process is
already terminal.  An active process or any other error still propagates."
  (condition-case err
      (funcall function process stdout stderr stderr-buffer)
    (error
     (if (and (processp process)
              (process-get process :tramp-rpc-exited)
              (equal (cadr err)
                     (format "Output file descriptor of %s is closed"
                             (process-name process))))
         nil
       (signal (car err) (cdr err))))))

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
      ;; These private workarounds were verified against one exact client
      ;; release.  Remove old advice first so a live package upgrade cannot
      ;; leave a stale shim installed on a changed upstream implementation.
      (when (fboundp 'tramp-rpc-deploy--arch-to-rust-target)
        (advice-remove
         'tramp-rpc-deploy--arch-to-rust-target
         #'remote-backend-tramp-rpc--arch-to-rust-target-a)
        (when (and verified
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc-deploy--arch-to-rust-target))
          (advice-add
           'tramp-rpc-deploy--arch-to-rust-target
           :around #'remote-backend-tramp-rpc--arch-to-rust-target-a)))
      (when (fboundp 'tramp-rpc-handle-make-process)
        (advice-remove
         'tramp-rpc-handle-make-process
         #'remote-backend-tramp-rpc--local-relay-cwd-a)
        (when verified
          (advice-add
           'tramp-rpc-handle-make-process
           :around #'remote-backend-tramp-rpc--local-relay-cwd-a)))
      (dolist (symbol '(tramp-rpc--controlmaster-socket-path
                        tramp-rpc--ensure-controlmaster-directory))
        (when (fboundp symbol)
          (advice-remove
           symbol #'remote-backend-tramp-rpc--local-controlmaster-path-a)
          (when (and verified
                     (remote-backend-tramp-rpc--private-compatible-p symbol))
            (advice-add
             symbol :around
             #'remote-backend-tramp-rpc--local-controlmaster-path-a))))
      (when (fboundp 'tramp-rpc--call)
        (advice-remove
         'tramp-rpc--call
         #'remote-backend-tramp-rpc--adapter-timeout-a)
        (when (and verified
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--call)
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--call-with-timeout)
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--get-connection))
          (advice-add
           'tramp-rpc--call
           :around #'remote-backend-tramp-rpc--adapter-timeout-a)))
      (when (fboundp 'tramp-rpc--compute-remote-path)
        (advice-remove
         'tramp-rpc--compute-remote-path
         #'remote-backend-tramp-rpc--compute-remote-path-a)
        (when (and verified
                   (cl-every
                    #'remote-backend-tramp-rpc--private-compatible-p
                    '(tramp-rpc--call
                      tramp-rpc--decode-output
                      tramp-rpc--compute-remote-path
                      tramp-rpc--effective-remote-path-spec
                      tramp-rpc--expand-remote-path-entry
                      tramp-rpc--append-path-entries
                      tramp-rpc--fetch-default-remote-path
                      tramp-rpc--fetch-remote-exec-path)))
          (advice-add
           'tramp-rpc--compute-remote-path
           :around #'remote-backend-tramp-rpc--compute-remote-path-a)))
      (dolist (entry
               '((tramp-rpc--acl-enabled-p
                  . remote-backend-tramp-rpc--acl-enabled-a)
                 (tramp-rpc--selinux-enabled-p
                  . remote-backend-tramp-rpc--selinux-enabled-a)))
        (when (fboundp (car entry))
          (advice-remove (car entry) (cdr entry))
          (when (and verified
                     (remote-backend-tramp-rpc--private-compatible-p
                      'tramp-rpc--call)
                     (remote-backend-tramp-rpc--private-compatible-p
                      'tramp-rpc--get-connection)
                     (remote-backend-tramp-rpc--private-compatible-p
                     (car entry)))
            (advice-add (car entry) :around (cdr entry)))))
      (when (fboundp 'tramp-rpc-handle-write-region)
        (advice-remove
         'tramp-rpc-handle-write-region
         #'remote-backend-tramp-rpc--write-region-in-place-a))
      (when (fboundp 'file-extended-attributes)
        (advice-remove
         'file-extended-attributes
         #'remote-backend-tramp-rpc--extended-attributes-in-place-a))
      (when (and verified
                 remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip
                 (= emacs-major-version 31)
                 (remote-backend-tramp-rpc--private-compatible-p
                  'tramp-rpc-handle-write-region)
                 (fboundp 'file-extended-attributes)
                 (equal (func-arity 'file-extended-attributes) '(1 . 1)))
        (advice-add
         'tramp-rpc-handle-write-region :around
         #'remote-backend-tramp-rpc--write-region-in-place-a)
        (advice-add
         'file-extended-attributes :around
         #'remote-backend-tramp-rpc--extended-attributes-in-place-a))
      (when (fboundp 'tramp-rpc--deliver-process-output)
        (advice-remove
         'tramp-rpc--deliver-process-output
         #'remote-backend-tramp-rpc--closed-relay-exit-a)
        (when (and verified
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--deliver-process-output))
          (advice-add
           'tramp-rpc--deliver-process-output
           :around #'remote-backend-tramp-rpc--closed-relay-exit-a)))
      (when (fboundp 'tramp-rpc--connection-transport-death)
        (advice-remove
         'tramp-rpc--connection-transport-death
         #'remote-backend-tramp-rpc--transport-death-before-a)
        (when (and verified
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--connection-key)
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--get-connection)
                   (remote-backend-tramp-rpc--private-compatible-p
                    'tramp-rpc--connection-transport-death))
          (advice-add
           'tramp-rpc--connection-transport-death
           :before #'remote-backend-tramp-rpc--transport-death-before-a))))))

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
The 0.13.1 client uses `getent' for a named user when the connection vector
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
                 (tramp-rpc--decode-output
                  (alist-get 'stdout result)
                  (alist-get 'stdout_encoding result)))))
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
     ((and (string-match-p "failed to obtain tramp-rpc-server" message)
           (string-match-p
            (rx (or "cannot cross-compile" "unknown architecture"))
            message))
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
       :file-operation-cost batched))))

(remote-backend-tramp-rpc-install)

(provide 'remote-backend-tramp-rpc)
;;; remote-backend-tramp-rpc.el ends here
