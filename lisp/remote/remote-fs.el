;;; remote-fs.el --- Canonical /fs: target file names -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `/fs:TARGET:/absolute/path' is the canonical Emacs file-name form.
;; `fs://TARGET/absolute/path' is its external URI spelling.  This module is a
;; routing file-name handler layered over native files and existing TRAMP
;; backends; underlying link paths never become buffer identity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tramp)
(require 'url-util)
(require 'remote-core)
(require 'remote-connection)
(require 'remote-backend)
(require 'remote-pipeline)

(declare-function remote-make-process "remote-process" (&rest plist))
(defvar remote-config-settings)

(defconst remote-fs-method "fs")
(defconst remote-fs-uri-regexp
  "\\`fs://\\([a-z0-9][a-z0-9._-]*\\)\\(/.*\\)\\'")

(defvar remote-fs--normalizing nil
  "Non-nil while visited-file names are being canonicalized.")

(defvar remote-fs--advice-installed nil)

(defvar remote-fs-path-expansion-cache (make-hash-table :test #'equal)
  "Resolved target-relative configured paths keyed by target and spelling.")

(defun remote-fs-register-method ()
  "Teach TRAMP to parse `/fs:' syntax without enabling its handler.
Logical identity helpers must work whenever this library is loaded; actual
file interception remains controlled by `remote-fs-install'."
  (unless (assoc remote-fs-method tramp-methods)
    (add-to-list 'tramp-methods (list remote-fs-method))))

(remote-fs-register-method)

(defun remote-fs-file-name-p (file-name)
  "Return non-nil when FILE-NAME uses the logical fs method."
  (and (stringp file-name)
       (string-match-p "\\`/fs:[^:]+:" file-name)))

(defun remote-fs--vector (file-name)
  "Return FILE-NAME's parsed fs TRAMP vector, or nil."
  (when (remote-fs-file-name-p file-name)
    (let ((vec (tramp-dissect-file-name file-name nil)))
      (and (string= (tramp-file-name-method vec) remote-fs-method)
           vec))))

(defun remote-fs-target-id (file-name)
  "Return target ID encoded by FILE-NAME."
  (when-let* ((vec (remote-fs--vector file-name)))
    (remote-normalize-id (tramp-file-name-host vec))))

(defun remote-fs-localname (file-name)
  "Return target-native localname encoded by FILE-NAME."
  (when-let* ((vec (remote-fs--vector file-name)))
    (tramp-file-name-localname vec)))

(defun remote-make-file-name (target-id localname)
  "Build a canonical fs file name for TARGET-ID and LOCALNAME."
  (let* ((target-id (remote-normalize-id target-id))
         ;; TRAMP connection buffers deliberately use a file name with no
         ;; localname, such as `/ssh:host:'.  At the logical file boundary
         ;; that denotes the target root.
         (localname
          (if (or (null localname)
                  (and (stringp localname)
                       (string-empty-p localname)))
              "/"
            localname))
         (localname
          (if (and (file-name-absolute-p localname)
                   ;; Emacs treats `~/' as absolute, but expanding it here
                   ;; would use the client machine's HOME.  A logical name
                   ;; must already contain the target-native absolute path.
                   (not (string-prefix-p "~" localname)))
              (let ((inhibit-file-name-handlers
                     (cons #'tramp-file-name-handler
                           inhibit-file-name-handlers))
                    (inhibit-file-name-operation 'expand-file-name))
                (expand-file-name localname "/"))
            (error "Remote localname is not absolute: %S" localname))))
    (format "/fs:%s:%s" target-id localname)))

(defun remote-fs--target-id-value (target)
  "Return the normalized target ID represented by TARGET."
  (cond
   ((remote-target-p target) (remote-target-id target))
   ((remote-context-p target) (remote-context-target-id target))
   ((null target) nil)
   (t (remote-normalize-id target))))

(defun remote-fs--path-target-hint (path)
  "Return PATH's explicit target ID without canonicalizing PATH.
Relative and tilde-prefixed names return nil so their base directory decides
the target.  This helper must stay non-recursive: it is used while
`remote-expand-file-name' is itself choosing that base."
  (cond
   ((not (stringp path)) nil)
   ((remote-fs-file-name-p path)
    (remote-fs-target-id path))
   ((string-match remote-fs-uri-regexp path)
    (remote-normalize-id (match-string 1 path)))
   ((file-remote-p path)
    (remote-file-name-target path))
   ((and (file-name-absolute-p path)
         (not (string-prefix-p "~" path)))
    "local")
   (t nil)))

(defun remote-fs--directory-on-target (directory target-id)
  "Return logical DIRECTORY on TARGET-ID without resolving symlinks."
  (let ((directory
         (cond
          ((or (remote-fs-file-name-p directory)
               (and (stringp directory)
                    (string-match-p "\\`fs://" directory))
               (and (stringp directory)
                    (file-remote-p directory)))
           (remote-canonicalize-file-name directory))
          ((and (stringp directory)
                (file-name-absolute-p directory)
                (not (string-prefix-p "~" directory)))
           (remote-make-file-name target-id directory))
          ((stringp directory)
           (remote-expand-file-name directory nil target-id))
          (t
           (remote-make-file-name target-id "/")))))
    (unless (equal (remote-fs-target-id directory) target-id)
      (error "Directory %S belongs to target %s, not %s"
             directory (remote-fs-target-id directory) target-id))
    (file-name-as-directory directory)))

(defun remote-expand-file-name (file-name &optional directory target)
  "Expand FILE-NAME in a target-aware logical namespace.
The argument order mirrors `expand-file-name'; TARGET is an optional
`remote-target', `remote-context', or target ID.  Leading `~' is resolved by
the selected backend on TARGET.  This function performs lexical path
expansion only: it deliberately does not chase symbolic links."
  (unless (stringp file-name)
    (error "Remote file name is not a string: %S" file-name))
  (let* ((explicit-target (remote-fs--target-id-value target))
         (context-target
          (and (remote-context-p target)
               (remote-context-target-id target)))
         (directory-target
          (remote-fs--path-target-hint directory))
         (default-target
          (and (not explicit-target)
               (or (remote-fs--path-target-hint default-directory)
                   ;; A tilde spelling cannot encode a remote target by
                   ;; itself.  Treat it as local here; resolving the tilde is
                   ;; a later backend operation.
                   (and (stringp default-directory)
                        (string-prefix-p "~" default-directory)
                        "local"))))
         (target-id
          (or explicit-target context-target directory-target
              default-target "local")))
    (cond
     ((or (remote-fs-file-name-p file-name)
          (string-match-p "\\`fs://" file-name)
          (file-remote-p file-name))
      (let ((canonical (remote-canonicalize-file-name file-name directory)))
        (when (and explicit-target
                   (remote-fs-file-name-p canonical)
                   (not (equal
                         (remote-fs-target-id canonical) target-id)))
          (error "File %S belongs to target %s, not %s"
                 file-name (remote-fs-target-id canonical) target-id))
        canonical))
     (t
      (let* ((base
              (remote-fs--directory-on-target
              (or directory
                   (and (remote-context-p target)
                        (remote-context-workspace-root target))
                   (and (not (string-prefix-p "~" file-name))
                        (stringp default-directory)
                        (equal
                         (ignore-errors
                           (remote-file-name-target default-directory))
                         target-id)
                        default-directory)
                   "/")
               target-id))
             (base-localname (remote-fs-localname base))
             (localname
              (cond
               ((and (file-name-absolute-p file-name)
                     (not (string-prefix-p "~" file-name)))
                (let ((inhibit-file-name-handlers
                       (cons #'tramp-file-name-handler
                             inhibit-file-name-handlers))
                      (inhibit-file-name-operation 'expand-file-name))
                  (expand-file-name file-name "/")))
               ((not (string-prefix-p "~" file-name))
                (let ((inhibit-file-name-handlers
                       (cons #'tramp-file-name-handler
                             inhibit-file-name-handlers))
                      (inhibit-file-name-operation 'expand-file-name))
                  (expand-file-name file-name base-localname)))
               (t
                (let* ((context (remote-fs--context base))
                       (route
                        (remote-resolve
                         (or remote-current-adapter-id "emacs-file")
                         'metadata context nil))
                       (remote-current-connection
                        (remote-connection-ensure route context)))
                  (remote-backend-expand-localname
                   route file-name base-localname))))))
        (unless (and (file-name-absolute-p localname)
                     (not (string-prefix-p "~" localname)))
          (error "Target %s did not resolve %S to an absolute path"
                 target-id file-name))
        (when (string-prefix-p "~" file-name)
          (puthash (list target-id file-name)
                   localname remote-fs-path-expansion-cache))
        (remote-make-file-name target-id localname))))))

(defun remote-fs--absolute-tramp-localname (file-name vector)
  "Return FILE-NAME's target-absolute localname from TRAMP VECTOR.
TRAMP legitimately returns abbreviated names such as
`/ssh:host:~/project/'.  Expanding that spelling through the official TRAMP
`expand-file-name' boundary is essential: expanding the vector's localname as
a native path would silently substitute the client machine's HOME."
  (let ((localname (or (tramp-file-name-localname vector) "")))
    (cond
     ((string-empty-p localname) "/")
     ((and (file-name-absolute-p localname)
           (not (string-prefix-p "~" localname)))
      localname)
     (t
      (let* ((expanded (expand-file-name file-name))
             (expanded-vector
              (tramp-dissect-file-name expanded nil))
             (absolute
              (tramp-file-name-localname expanded-vector)))
        (unless (and absolute
                     (file-name-absolute-p absolute)
                     (not (string-prefix-p "~" absolute)))
          (error "TRAMP did not resolve target home in %S" file-name))
        absolute)))))

(defun remote-file-name-to-uri (file-name)
  "Return canonical external fs URI for FILE-NAME."
  (let* ((file-name (remote-canonicalize-file-name file-name))
         (target (remote-fs-target-id file-name))
         (localname (remote-fs-localname file-name)))
    (unless target
      (error "Cannot represent file as fs URI: %S" file-name))
    (concat "fs://" target
            (url-hexify-string
             localname
             (cons ?/ url-unreserved-chars)))))

(defun remote-uri-to-file-name (uri)
  "Convert external fs URI to canonical internal file name."
  (unless (and (stringp uri)
               (string-match remote-fs-uri-regexp uri))
    (error "Invalid fs URI: %S" uri))
  (remote-make-file-name
   (match-string 1 uri)
   (url-unhex-string (match-string 2 uri))))

(defun remote-file-local-name (file-name)
  "Return target-native absolute path represented by FILE-NAME."
  (cond
   ((remote-fs-file-name-p file-name)
    (remote-fs-localname file-name))
   ((file-remote-p file-name 'localname))
   (t file-name)))

(defun remote-file-name-target (file-name)
  "Return the logical target ID for FILE-NAME.
Native paths and `fs://local/' URIs belong to the `local' target."
  (remote-fs-target-id (remote-canonicalize-file-name file-name)))

(defun remote-file-equal-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same logical file.
The comparison deliberately ignores which physical link currently serves the
target.  It does not contact either machine."
  (let ((left (remote-canonicalize-file-name left))
        (right (remote-canonicalize-file-name right)))
    (and (equal (remote-fs-target-id left)
                (remote-fs-target-id right))
         (equal (remote-fs-localname left)
                (remote-fs-localname right)))))

(defun remote--workspace-path (workspace)
  "Return target-native path from WORKSPACE config."
  (cond
   ((stringp workspace) workspace)
   ((listp workspace)
    (or (alist-get 'path workspace)
        (plist-get workspace :path)))))

(defun remote-target-default-localname (target)
  "Return the configured default target-native directory for TARGET.
The result may begin with `~'; callers needing a logical identity must pass it
through `remote-expand-file-name'."
  (let ((path
         (or (remote--workspace-path
              (car (remote-target-workspaces target)))
             (and (boundp 'remote-config-settings)
                  (alist-get 'defaultPath remote-config-settings))
             "/")))
    (if (and (stringp path) (file-name-absolute-p path))
        (file-name-as-directory path)
      "/")))

(defun remote-target-file-name (target &optional localname)
  "Return canonical file name on TARGET for LOCALNAME.
Configured home-relative paths are expanded by TARGET's selected backend."
  (let* ((target-object
          (if (remote-target-p target)
              target
            (or (remote-get-target target)
                (error "Unknown target: %s" target))))
         (target-id (remote-target-id target-object)))
    (remote-expand-file-name
     (or localname
         (remote-target-default-localname target-object))
     nil target-id)))

(defun remote-fs--workspace-property (workspace property)
  "Return PROPERTY from WORKSPACE."
  (cond
   ((and (listp workspace) (keywordp (car workspace)))
    (plist-get workspace
               (intern (concat ":" (symbol-name property)))))
   ((listp workspace) (alist-get property workspace))
   ((eq property 'path) workspace)))

(defun remote-fs--workspace-for (target localname)
  "Return TARGET workspace best matching LOCALNAME."
  (car
   (sort
    (seq-filter
     (lambda (workspace)
       (when-let* ((path
                    (remote-fs--workspace-property workspace 'path)))
         (let ((path
                (or
                 (gethash
                  (list (remote-target-id target) path)
                  remote-fs-path-expansion-cache)
                 (and (file-name-absolute-p path)
                      (not (string-prefix-p "~" path))
                      path))))
           (and path
                (string-prefix-p
                 (file-name-as-directory path)
                 (file-name-as-directory localname))))))
     (copy-sequence (and target (remote-target-workspaces target))))
    (lambda (left right)
      (> (length (remote-fs--workspace-property left 'path))
         (length (remote-fs--workspace-property right 'path)))))))

(defun remote-fs--context (canonical)
  "Build an I/O-free logical context for CANONICAL."
  (let* ((target-id (or (remote-fs-target-id canonical) "local"))
         (localname (or (remote-fs-localname canonical)
                        (expand-file-name canonical)))
         (target (remote-get-target target-id))
         (workspace (remote-fs--workspace-for target localname))
         (workspace-path
          (when-let* ((path
                       (remote-fs--workspace-property workspace 'path)))
            (or
             (gethash
              (list target-id path)
              remote-fs-path-expansion-cache)
             path)))
         (workspace-root
          (if workspace-path
              (remote-make-file-name
               target-id (file-name-as-directory workspace-path))
            (if (string-suffix-p "/" canonical)
                (file-name-as-directory canonical)
              (file-name-directory canonical))))
         (workspace-id
          (or (remote-fs--workspace-property workspace 'id)
              (and workspace-path
                   (remote-fs--slug workspace-path)))))
    (remote-context-create
     :target-id target-id
     :localname localname
     :workspace-id
     (and workspace-id (remote-normalize-id workspace-id))
     :workspace-root workspace-root
     :source workspace)))

(defun remote-context (&optional path)
  "Return logical context for PATH or the current buffer."
  (let* ((path (or path buffer-file-name default-directory))
         (canonical (remote-canonicalize-file-name path)))
    (remote-fs--context canonical)))

(defun remote-fs-register-link-plugins ()
  "Register the built-in backend modules.
This compatibility dispatcher keeps callers independent of backend layout."
  (remote-backend-register-builtins))

(defun remote-fs--slug (value)
  "Return a stable target ID candidate derived from VALUE."
  (let ((slug
         (downcase
          (replace-regexp-in-string
           "\\`-+\\|-+\\'" ""
           (replace-regexp-in-string "[^[:alnum:]._-]+" "-" value)))))
    (if (string-match-p remote-id-regexp slug)
        slug
      (format "target-%s" (substring (secure-hash 'sha1 value) 0 10)))))

(defun remote-fs--link-matches-vector-p (link vec)
  "Return non-nil when LINK represents physical TRAMP VEC."
  (let* ((config (remote-link-config link))
         (method (tramp-file-name-method vec))
         (plugins (remote-link-plugin-ids link))
         (backend-matches
          (if (equal method "rpc")
              (member "tramp-rpc" plugins)
            (and (member "tramp" plugins)
                 (equal method
                        (or (plist-get config :method) "ssh"))))))
    (and backend-matches
         (equal (plist-get config :host) (tramp-file-name-host vec))
         (equal (plist-get config :user) (tramp-file-name-user vec))
         (equal (format "%s" (or (plist-get config :port) ""))
                (format "%s" (or (tramp-file-name-port vec) ""))))))

(defun remote-fs--target-for-tramp-vector (vec)
  "Return target for physical TRAMP VEC, creating a session target if needed."
  (let (found)
    (maphash
     (lambda (_id link)
       (when (and (null found)
                  (remote-fs--link-matches-vector-p link vec))
         (setq found (remote-get-target
                      (remote-link-target-id link)))))
     remote-links)
    (or found
        (let* ((host (or (tramp-file-name-host vec) "remote"))
               (id (remote-fs--slug host))
               (method (tramp-file-name-method vec))
               (plugin (if (equal method "rpc") "tramp-rpc" "tramp"))
               (transport
                (if (member method '("ssh" "sshx" "scp" "rpc"))
                    "ssh"
                  method))
               (target (or (remote-get-target id)
                           (remote-register-target
                            id :label host :transient t :source 'tramp))))
          (unless (seq-some
                   (lambda (link)
                     (remote-fs--link-matches-vector-p link vec))
                   (remote-links-for-target id))
            (remote-register-pipeline
             id
             (format "%s-%s" transport
                     (substring
                      (secure-hash
                       'sha1
                       (format "%S"
                               (list (tramp-file-name-user vec)
                                     host
                                     (tramp-file-name-port vec))))
                      0 8))
             plugin
             :priority 0
             :config (list :method
                           (if (equal method "rpc") "ssh" method)
                           :host host
                           :user (tramp-file-name-user vec)
                           :port (tramp-file-name-port vec))
             :source 'tramp))
          target))))

(defun remote-canonicalize-file-name (file-name &optional directory)
  "Return canonical logical name for FILE-NAME relative to DIRECTORY."
  (cond
   ((not (stringp file-name)) file-name)
   ((string-prefix-p "~" file-name)
    (remote-expand-file-name file-name directory))
   ((string-match-p "\\`fs://" file-name)
    (remote-uri-to-file-name file-name))
   ((remote-fs-file-name-p file-name)
    (remote-make-file-name
     (remote-fs-target-id file-name)
     (remote-fs-localname file-name)))
   ((file-remote-p file-name)
    (let* ((vec (tramp-dissect-file-name file-name nil))
           (method (tramp-file-name-method vec)))
      (if (member method '("ssh" "sshx" "scp" "rpc"))
          (remote-make-file-name
           (remote-target-id
            (remote-fs--target-for-tramp-vector vec))
           (remote-fs--absolute-tramp-localname file-name vec))
        file-name)))
   (t
    (let ((directory (or directory default-directory)))
      (if (remote-fs-file-name-p directory)
          (remote-fs-handle-expand-file-name file-name directory)
        (remote-make-file-name
         "local"
         (let ((inhibit-file-name-handlers
                (cons #'tramp-file-name-handler
                      inhibit-file-name-handlers))
               (inhibit-file-name-operation 'expand-file-name))
           (expand-file-name file-name directory))))))))

(defun remote-fs-handle-expand-file-name (name &optional directory)
  "Expand NAME relative to logical DIRECTORY."
  (cond
   ((remote-fs-file-name-p name)
    (remote-make-file-name
     (remote-fs-target-id name)
     (remote-fs-localname name)))
   ((string-match-p "\\`fs://" name)
    (remote-uri-to-file-name name))
   ((remote-fs-file-name-p directory)
    (if (string-prefix-p "~" name)
        (remote-expand-file-name
         name directory (remote-fs-target-id directory))
      (let* ((target (remote-fs-target-id directory))
           (base (remote-fs-localname directory))
           (local
            (let ((inhibit-file-name-handlers
                   (cons #'tramp-file-name-handler
                         inhibit-file-name-handlers))
                  (inhibit-file-name-operation 'expand-file-name))
              (expand-file-name name base))))
        (remote-make-file-name target local))))
   (t
    (remote-canonicalize-file-name
     (let ((inhibit-file-name-handlers
            (cons #'tramp-file-name-handler inhibit-file-name-handlers))
           (inhibit-file-name-operation 'expand-file-name))
       (expand-file-name name directory))))))

(defun remote-fs--context-for-file (file-name)
  "Return context rooted at FILE-NAME's target."
  (remote-fs--context
   (remote-canonicalize-file-name file-name)))

(cl-defstruct (remote-file-operation-spec
               (:constructor remote-file-operation-spec-create))
  operation capability mutating path-arguments result-kind retry-safe)

(defvar remote-file-operations (make-hash-table :test #'eq)
  "File-handler operation contract keyed by Emacs operation symbol.")

(defvar remote-fs--unknown-operations (make-hash-table :test #'eq)
  "Unknown operations already reported to the route log.")

(cl-defun remote-register-file-operation
    (operation &key (capability 'metadata) mutating
               path-arguments (result-kind 'pass) (retry-safe t))
  "Register the routing contract for file-name OPERATION.
PATH-ARGUMENTS contains zero-based positions of file-name arguments, or `all'
for an extension whose signature is not known.  RESULT-KIND is one of `pass',
`path', `path-list', `path-alist', `visit', `symlink-target', or
`local-copy'.  RETRY-SAFE controls route failover independently of capability."
  (unless (memq capability remote-capabilities)
    (error "Unknown capability for file operation %s: %S"
           operation capability))
  (unless (or (eq path-arguments 'all)
              (and (listp path-arguments)
                   (seq-every-p #'natnump path-arguments)))
    (error "Invalid path arguments for file operation %s: %S"
           operation path-arguments))
  (unless (memq result-kind
                '(pass path path-list path-alist visit
                       symlink-target local-copy))
    (error "Invalid result kind for file operation %s: %S"
           operation result-kind))
  (let ((spec
         (remote-file-operation-spec-create
          :operation operation
          :capability capability
          :mutating mutating
          :path-arguments path-arguments
          :result-kind result-kind
          :retry-safe retry-safe)))
    (puthash operation spec remote-file-operations)
    spec))

(defun remote-fs--register-operation-group
    (operations capability path-arguments result-kind retry-safe)
  "Register OPERATIONS with the shared routing contract."
  (dolist (operation operations)
    (remote-register-file-operation
     operation
     :capability capability
     :mutating (not retry-safe)
     :path-arguments path-arguments
     :result-kind result-kind
     :retry-safe retry-safe)))

(defun remote-fs-register-standard-operations ()
  "Register the Emacs 31/32 primary file-handler operation contract."
  (clrhash remote-file-operations)
  ;; One explicit file name, metadata result.
  (remote-fs--register-operation-group
   '(diff-latest-backup-file file-acl
     file-accessible-directory-p file-attributes file-directory-p
     file-executable-p file-exists-p file-locked-p file-modes
     file-name-case-insensitive-p file-ownership-preserved-p file-readable-p
     file-regular-p file-selinux-context file-symlink-p file-system-info
     file-writable-p get-file-buffer vc-registered dired-uncache)
   'metadata '(0) 'pass t)
  (remote-fs--register-operation-group
   '(byte-compiler-base-file-name file-name-sans-versions
     make-lock-file-name)
   'metadata '(0) 'path t)
  (remote-fs--register-operation-group
   '(access-file file-local-copy insert-file-contents load)
   'file-read '(0) 'pass t)
  (remote-register-file-operation
   'file-local-copy :capability 'file-read :path-arguments '(0)
   :result-kind 'local-copy :retry-safe t)
  (remote-register-file-operation
   'insert-file-contents :capability 'file-read :path-arguments '(0)
   :result-kind 'visit :retry-safe t)
  (remote-register-file-operation
   'file-truename :capability 'metadata :path-arguments '(0)
   :result-kind 'pass :retry-safe t)
  ;; Directory enumeration and completion.
  (remote-fs--register-operation-group
   '(directory-files directory-files-and-attributes insert-directory)
   'directory '(0) 'pass t)
  (remote-fs--register-operation-group
   '(file-name-all-completions file-name-completion)
   'directory '(1) 'pass t)
  ;; Two-name read-only comparisons.
  (remote-fs--register-operation-group
   '(file-equal-p file-in-directory-p file-newer-than-file-p)
   'metadata '(0 1) 'pass t)
  ;; Mutations are deliberately never replayed after an ambiguous failure.
  (remote-fs--register-operation-group
   '(delete-directory delete-file lock-file
     make-directory set-file-acl set-file-modes set-file-selinux-context
     set-file-times unlock-file)
   'file-write '(0) 'pass nil)
  (remote-register-file-operation
   'dired-compress-file :capability 'file-write :mutating t
   :path-arguments '(0) :result-kind 'path :retry-safe nil)
  (remote-fs--register-operation-group
   '(add-name-to-file copy-directory copy-file make-symbolic-link rename-file)
   'file-write '(0 1) 'pass nil)
  (remote-register-file-operation
   'write-region :capability 'file-write :mutating t
   :path-arguments '(2) :retry-safe nil)
  ;; Path-valued operations need their physical return value rewrapped.
  (remote-fs--register-operation-group
   '(make-nearby-temp-file)
   'file-write '(0) 'path nil)
  (remote-fs--register-operation-group
   '(find-backup-file-name)
   'metadata '(0) 'path-alist t)
  ;; Watches and process operations derive placement from a file or the
  ;; current default-directory, but their return values are opaque handles.
  (remote-register-file-operation
   'file-notify-add-watch :capability 'watch
   :mutating t :path-arguments '(0) :retry-safe nil)
  (remote-fs--register-operation-group
   '(file-notify-rm-watch file-notify-valid-p)
   'watch nil 'pass t)
  (remote-fs--register-operation-group
   '(make-process start-file-process)
   'process-async nil 'pass nil)
  (remote-fs--register-operation-group
   '(list-system-processes memory-info process-attributes
     process-file shell-command)
   'process-sync nil 'pass t)
  (remote-register-file-operation
   'exec-path :capability 'environment :path-arguments nil
   :result-kind 'path-list :retry-safe t)
  (remote-register-file-operation
   'temporary-file-directory :capability 'metadata :path-arguments nil
   :result-kind 'path :retry-safe t)
  (remote-fs--register-operation-group
   '(file-group-gid file-user-uid)
   'metadata nil 'pass t)
  ;; Operations implemented lexically or from buffer state are still
  ;; registered so Doctor and extension code can inspect a complete contract.
  (remote-fs--register-operation-group
   '(abbreviate-file-name directory-file-name expand-file-name
     file-name-as-directory file-name-directory file-name-nondirectory
     file-remote-p substitute-in-file-name unhandled-file-name-directory)
   'metadata '(0) 'pass t)
  (remote-fs--register-operation-group
   '(make-auto-save-file-name set-visited-file-modtime
     verify-visited-file-modtime)
   'metadata nil 'pass t)
  remote-file-operations)

(remote-fs-register-standard-operations)

(defun remote-fs--operation-spec (operation)
  "Return OPERATION's registered contract or a conservative fallback."
  (or (gethash operation remote-file-operations)
      (progn
        (unless (gethash operation remote-fs--unknown-operations)
          (puthash operation t remote-fs--unknown-operations)
          (remote-log
           'file-operation-warning
           :operation operation
           :message
           "Unknown file operation routed once as non-idempotent file-write"))
        (remote-file-operation-spec-create
         :operation operation
         :capability 'file-write
         :mutating t
         :path-arguments 'all
         :result-kind 'pass
         :retry-safe nil))))

(defun remote-fs--operation-capability (operation)
  "Return route capability for OPERATION."
  (remote-file-operation-spec-capability
   (remote-fs--operation-spec operation)))

(defun remote-fs--path-argument-p (spec index)
  "Return non-nil when argument INDEX is a path according to SPEC."
  (let ((positions
         (remote-file-operation-spec-path-arguments spec)))
    (or (eq positions 'all)
        (memq index positions))))

(defun remote-fs--primary-file (operation args)
  "Return logical primary file for OPERATION and ARGS."
  (let ((spec (remote-fs--operation-spec operation)))
    (or (cl-loop for value in args
                 for index from 0
                 when (and (remote-fs--path-argument-p spec index)
                           (remote-fs-file-name-p value))
                 return value)
      (and (remote-fs-file-name-p default-directory)
           default-directory)
      (ignore-errors
          (apply #'tramp-file-name-for-operation operation args)))))

(defun remote-fs--call-underlying (operation args default)
  "Call OPERATION with ARGS and physical DEFAULT directory."
  (let ((default-directory default)
        (inhibit-file-name-operation nil)
        (inhibit-file-name-handlers
         (delq #'tramp-file-name-handler
               (copy-sequence inhibit-file-name-handlers))))
    (apply operation args)))

(defun remote-fs--process-argument (argument)
  "Return the process represented by ARGUMENT, or nil."
  (cond
   ((processp argument) argument)
   ((stringp argument) (get-process argument))
   ((bufferp argument) (get-buffer-process argument))))

(defun remote-fs--call-process-operation (operation args)
  "Call process OPERATION without redispatching through a logical buffer.
TRAMP chooses a handler for these operations from the process buffer's
`default-directory'.  A native process whose buffer uses `/fs:local:' would
therefore recurse into this handler forever.  Temporarily give that buffer a
native directory; a genuinely remote process still carries its physical
`tramp-vector', so TRAMP or tramp-rpc continues to receive the operation."
  (let* ((process (remote-fs--process-argument (car args)))
         (process-buffer (and process (process-buffer process)))
         (source-buffer (current-buffer)))
    (if (buffer-live-p process-buffer)
        (with-current-buffer process-buffer
          (let ((default-directory temporary-file-directory))
            (with-current-buffer source-buffer
              (apply operation args))))
      (let ((inhibit-file-name-handlers
             (cons #'tramp-file-name-handler
                   inhibit-file-name-handlers))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

(defun remote-fs--run-real-buffer-operation (operation args)
  "Run buffer-based OPERATION with ARGS without re-entering this handler.
The current buffer and its logical `buffer-file-name' stay unchanged.  This is
the same boundary used by TRAMP for primitives which derive their file identity
from the visiting buffer rather than from an explicit file-name argument."
  (tramp-run-real-handler operation args))

(defun remote-fs-handle-make-process (&rest plist)
  "Route official `make-process' PLIST through the remote process API.
This is the compatibility boundary used by Eglot and third-party packages.
The routed API resolves a relative executable on the target before handing
the request to TRAMP or tramp-rpc."
  ;; Emacs enters a file-name handler with `make-process' and
  ;; `tramp-file-name-handler' inhibited to prevent recursive handling of the
  ;; original `/fs:' name.  The routed call ends on a different, physical
  ;; backend name such as `/rpc:' or `/ssh:', so that handler must become
  ;; eligible again.  Otherwise Emacs tries to chdir locally into the literal
  ;; TRAMP name before tramp-rpc can strip its localname.
  (let ((inhibit-file-name-handlers
         (delq #'tramp-file-name-handler
               (copy-sequence inhibit-file-name-handlers)))
        (inhibit-file-name-operation nil))
    (apply
     #'remote-make-process
     (append
      plist
      (list :remote-adapter
            (or remote-current-adapter-id "process")
            :remote-context (remote-context default-directory)
            :remote-directory default-directory)))))

(defun remote-fs-handle-start-file-process
    (name buffer program &rest program-args)
  "Implement `start-file-process' using the routed process boundary."
  (let ((inhibit-file-name-handlers
         (delq #'tramp-file-name-handler
               (copy-sequence inhibit-file-name-handlers)))
        (inhibit-file-name-operation nil))
    (remote-make-process
     :name name
     :buffer buffer
     :command (cons program program-args)
     :remote-adapter (or remote-current-adapter-id "process")
     :remote-context (remote-context default-directory)
     :remote-directory default-directory)))

(defun remote-fs--translate-args (operation args route)
  "Project logical path arguments of OPERATION through ROUTE."
  (let ((spec (remote-fs--operation-spec operation)))
    (cl-loop
     for value in args
     for index from 0
     collect
     (cond
      ((not (remote-fs--path-argument-p spec index)) value)
      ;; TARGET is link contents, not a transport path.  Keep relative and
      ;; native absolute spellings byte-for-byte.  A logical target on the
      ;; same machine is converted only to its target-native spelling.
      ((and (eq operation 'make-symbolic-link) (zerop index))
       (if (remote-fs-file-name-p value)
           (progn
             (unless (equal
                      (remote-fs-target-id value)
                      (remote-route-target-id route))
               (error
                "Cannot create a filesystem symlink across targets: %s -> %s"
                (remote-route-target-id route)
                (remote-fs-target-id value)))
             (remote-fs-localname value))
         value))
      ((remote-fs-file-name-p value)
       (let ((value-target (remote-fs-target-id value)))
         (remote-project-file-name
          value
          (if (equal value-target (remote-route-target-id route))
              route
            (remote-resolve
             (or remote-current-adapter-id "emacs-file")
             (remote-file-operation-spec-capability spec)
             (remote-fs--context-for-file value)
             nil)))))
      (t value)))))

(defun remote-fs--argument-target (value)
  "Return the logical target of path argument VALUE, if explicit."
  (cond
   ((remote-fs-file-name-p value) (remote-fs-target-id value))
   ((and (stringp value)
         (file-name-absolute-p value)
         (not (file-remote-p value)))
    "local")))

(defun remote-fs--validate-cross-target-operation (operation args)
  "Reject filesystem mutations OPERATION cannot perform across target roots."
  (when (memq operation
              '(rename-file add-name-to-file make-symbolic-link))
    (let* ((spec (remote-fs--operation-spec operation))
           (targets
            (delete-dups
             (delq nil
                   (cl-loop for value in args
                            for index from 0
                            when (remote-fs--path-argument-p spec index)
                            collect
                            (remote-fs--argument-target value))))))
      (when (> (length targets) 1)
        (error
         "%s cannot cross logical targets (%s); use copy instead"
         operation (string-join targets ", "))))))

(defun remote-fs--transform-result (spec result target-id)
  "Map physical RESULT according to SPEC back into TARGET-ID."
  (pcase (remote-file-operation-spec-result-kind spec)
    ('path
     (remote-fs--rewrap-physical result target-id))
    ('path-list
     (mapcar
      (lambda (path) (remote-fs--rewrap-physical path target-id))
      result))
    ('path-alist
     (cond
      ((and (consp result) (stringp (car result)))
       (cons (remote-fs--rewrap-physical (car result) target-id)
             (cdr result)))
      ((listp result)
       (mapcar
        (lambda (entry)
          (if (and (consp entry) (stringp (car entry)))
              (cons
               (remote-fs--rewrap-physical (car entry) target-id)
               (cdr entry))
            entry))
        result))
      (t result)))
    (_ result)))

(defun remote-fs--rewrap-physical (physical target-id)
  "Return PHYSICAL path in TARGET-ID's logical namespace."
  (cond
   ((not (stringp physical)) physical)
   ((remote-fs-file-name-p physical) physical)
   ((file-remote-p physical)
    (remote-make-file-name
     target-id (file-remote-p physical 'localname)))
   ((file-name-absolute-p physical)
    (remote-make-file-name target-id physical))
   (t physical)))

(defun remote-fs--call-routed (operation args)
  "Route OPERATION with ARGS through the selected link."
  (remote-fs--validate-cross-target-operation operation args)
  (let* ((spec (remote-fs--operation-spec operation))
         (logical (or (remote-fs--primary-file operation args)
                      (error "No logical fs context for %s" operation)))
         (context (remote-fs--context-for-file logical))
         (capability (remote-file-operation-spec-capability spec))
         (routes (remote-routes
                  (or remote-current-adapter-id "emacs-file")
                  capability context))
         (retry-safe
          (remote-file-operation-spec-retry-safe spec))
         last-error result done)
    (while (and routes (not done))
      (let ((route (pop routes)))
        (condition-case err
            (let* ((remote-current-connection
                    (remote-connection-ensure route context))
                   (physical-default
                    (when (remote-fs-file-name-p default-directory)
                      (remote-project-file-name
                       default-directory route)))
                   (translated
                    (remote-fs--translate-args operation args route)))
              (setq result
                    (remote-fs--transform-result
                     spec
                     (remote-fs--call-underlying
                      operation translated
                      (or physical-default temporary-file-directory))
                     (remote-context-target-id context))
                    done t)
              (remote-report-route-success route)
              (remote-log
               'route
               :target (remote-route-target-id route)
               :link (remote-route-link-id route)
               :plugin (remote-route-link-plugin-id route)
               :capability capability
               :adapter (remote-route-adapter-id route)))
          (error
           (setq last-error err)
           (let* ((failure-scope
                   (remote-report-route-failure route err))
                  (transport-error (eq failure-scope 'transport))
                  (backend-error (eq failure-scope 'backend)))
             ;; Backend plugins on one link share the same physical
             ;; reachability path.  A network failure must fail over to a
             ;; different link (FRP, Tailscale, ...), not retry the same SSH
             ;; endpoint through both TRAMP and tramp-rpc.
             (when transport-error
               (setq routes
                     (seq-remove
                      (lambda (candidate)
                        (equal (remote-route-link-id candidate)
                               (remote-route-link-id route)))
                      routes)))
             (when backend-error
               (setq routes
                     (seq-remove
                      (lambda (candidate)
                        (and
                         (equal (remote-route-link-id candidate)
                                (remote-route-link-id route))
                         (equal
                          (remote-route-link-plugin-id candidate)
                          (remote-route-link-plugin-id route))))
                      routes)))
             (unless (and retry-safe
                          routes
                          (or transport-error backend-error))
               (signal (car err) (cdr err))))))))
    (if done result
      (signal (car last-error) (cdr last-error)))))

(defun remote-fs-handle-file-remote-p
    (file-name &optional identification _connected)
  "Implement `file-remote-p' for logical FILE-NAME."
  (let ((target (remote-fs-target-id file-name)))
    (unless (equal target "local")
      (pcase identification
        ((or 'nil 't) (format "/fs:%s:" target))
        ('method "fs")
        ('user nil)
        ('host target)
        ('localname (remote-fs-localname file-name))
        (_ (format "/fs:%s:" target))))))

(defun remote-fs-handle-file-name-directory (file-name)
  "Implement `file-name-directory' for logical FILE-NAME."
  (when-let* ((directory
               (let ((inhibit-file-name-handlers
                      (cons #'tramp-file-name-handler
                            inhibit-file-name-handlers))
                     (inhibit-file-name-operation 'file-name-directory))
                 (file-name-directory
                  (remote-fs-localname file-name)))))
    (remote-make-file-name
     (remote-fs-target-id file-name) directory)))

(defun remote-fs-handle-file-name-nondirectory (file-name)
  "Implement `file-name-nondirectory' for logical FILE-NAME."
  (let ((inhibit-file-name-handlers
         (cons #'tramp-file-name-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'file-name-nondirectory))
    (file-name-nondirectory (remote-fs-localname file-name))))

(defun remote-fs-handle-file-name-as-directory (file-name)
  "Implement `file-name-as-directory' for logical FILE-NAME."
  (remote-make-file-name
   (remote-fs-target-id file-name)
   (file-name-as-directory (remote-fs-localname file-name))))

(defun remote-fs-handle-directory-file-name (file-name)
  "Implement `directory-file-name' for logical FILE-NAME."
  (remote-make-file-name
   (remote-fs-target-id file-name)
   (directory-file-name (remote-fs-localname file-name))))

(defun remote-fs-handle-abbreviate-file-name (file-name)
  "Abbreviate FILE-NAME without exposing its selected physical link."
  ;; Despite its name this operation participates in visited-file identity:
  ;; `find-file-noselect' stores its result in `buffer-file-name'.  A target
  ;; HOME spelling such as `~/...' cannot be embedded in the canonical `/fs:'
  ;; grammar, whose localname is always absolute.  Keep the logical name
  ;; unchanged; UI consumers may abbreviate `remote-file-local-name' when
  ;; they explicitly need a native local display.
  file-name)

(defun remote-fs-handle-substitute-in-file-name (file-name)
  "Substitute environment variables in logical FILE-NAME."
  (remote-make-file-name
   (remote-fs-target-id file-name)
   (let ((inhibit-file-name-handlers
          (cons #'tramp-file-name-handler inhibit-file-name-handlers))
         (inhibit-file-name-operation 'substitute-in-file-name))
     (substitute-in-file-name (remote-fs-localname file-name)))))

(defun remote-fs-handle-file-truename (file-name)
  "Return logical truename for FILE-NAME."
  (let* ((target (remote-fs-target-id file-name))
         (physical (remote-fs--call-routed 'file-truename (list file-name))))
    (remote-fs--rewrap-physical physical target)))

(defun remote-fs-handle-set-visited-file-modtime (&optional time-list)
  "Update the current buffer's recorded modification TIME-LIST.
When TIME-LIST is nil, read it from the logical visiting file through its
selected route.  The recorded value remains ordinary Emacs buffer state and is
therefore independent of the physical link used for that read."
  (unless buffer-file-name
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
           (buffer-name)))
  (unless time-list
    (let ((remote-file-name-inhibit-cache t))
      (setq time-list
            (or (file-attribute-modification-time
                 (file-attributes buffer-file-name))
                tramp-time-doesnt-exist))))
  (unless (time-equal-p time-list tramp-time-dont-know)
    (remote-fs--run-real-buffer-operation
     #'set-visited-file-modtime (list time-list))))

(defun remote-fs-handle-verify-visited-file-modtime (&optional buffer)
  "Return non-nil when BUFFER's logical visiting file is unchanged.
File metadata is obtained through the current route, while the comparison is
against BUFFER's standard `visited-file-modtime' record.  A two-second window
matches TRAMP's handling of transports with coarse timestamp resolution."
  (with-current-buffer (or buffer (current-buffer))
    (let ((file buffer-file-name)
          (visited (visited-file-modtime)))
      (if (or (not file)
              (zerop (float-time visited)))
          t
        (let* ((remote-file-name-inhibit-cache t)
               (attributes (file-attributes file))
               (modified
                (file-attribute-modification-time attributes)))
          (cond
           ((and attributes
                 (not (time-equal-p modified tramp-time-dont-know)))
            (< (abs (float-time (time-subtract modified visited))) 2))
           (attributes t)
           (t (time-equal-p visited tramp-time-doesnt-exist))))))))

(defun remote-fs-handle-make-auto-save-file-name ()
  "Return the standard Emacs auto-save name for the logical visiting buffer.
The logical name remains visible to `auto-save-file-name-transforms', so local
and remote buffers follow the user's normal Emacs auto-save policy."
  (remote-fs--run-real-buffer-operation #'make-auto-save-file-name nil))

(defun remote-fs-handle-insert-file-contents
    (file-name &optional visit beg end replace)
  "Insert logical FILE-NAME while preserving canonical buffer identity."
  (let* ((canonical (remote-canonicalize-file-name file-name))
         (result
          (remote-fs--call-routed
           'insert-file-contents
           (list canonical visit beg end replace))))
    (when visit
      (setq buffer-file-name canonical
            default-directory (file-name-directory canonical)
            buffer-file-truename (file-truename canonical)))
    (if (consp result)
        (cons canonical (cdr result))
      result)))

(defun remote-fs-handle-directory-files
    (directory &optional full match nosort count)
  "Return entries in logical DIRECTORY."
  (let* ((target (remote-fs-target-id directory))
         (result
          (remote-fs--call-routed
           'directory-files (list directory full match nosort count))))
    (if full
        (mapcar (lambda (path)
                  (remote-fs--rewrap-physical path target))
                result)
      result)))

(defun remote-fs-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format count)
  "Return logical DIRECTORY entries and attributes."
  (let* ((target (remote-fs-target-id directory))
         (result
          (remote-fs--call-routed
           'directory-files-and-attributes
           (list directory full match nosort id-format count))))
    (if full
        (mapcar
         (lambda (entry)
           (cons (remote-fs--rewrap-physical (car entry) target)
                 (cdr entry)))
         result)
      result)))

(defun remote-fs-handle-file-symlink-p (file-name)
  "Return symlink target for logical FILE-NAME."
  ;; Emacs specifies that this is the target string stored in the link.  It is
  ;; not a file identity and must not be rewritten to `/fs:'.
  (remote-fs--call-routed 'file-symlink-p (list file-name)))

(defun remote-fs-handle-file-notify-add-watch
    (file flags callback)
  "Watch logical FILE and keep event paths in its target namespace."
  (let ((target (remote-fs-target-id file)))
    (remote-fs--call-routed
     'file-notify-add-watch
     (list
      file flags
      (lambda (event)
        (let ((event (copy-sequence event)))
          (when (stringp (nth 2 event))
            (setf (nth 2 event)
                  (remote-fs--rewrap-physical
                   (nth 2 event) target)))
          (when (stringp (nth 3 event))
            (setf (nth 3 event)
                  (remote-fs--rewrap-physical
                   (nth 3 event) target)))
          (funcall callback event)))))))

(defun remote-fs-file-name-handler (operation &rest args)
  "Handle file-name OPERATION for logical fs ARGS."
  (pcase operation
    ('expand-file-name (apply #'remote-fs-handle-expand-file-name args))
    ('abbreviate-file-name
     (apply #'remote-fs-handle-abbreviate-file-name args))
    ('file-remote-p (apply #'remote-fs-handle-file-remote-p args))
    ('file-name-directory
     (apply #'remote-fs-handle-file-name-directory args))
    ('file-name-nondirectory
     (apply #'remote-fs-handle-file-name-nondirectory args))
    ('file-name-as-directory
     (apply #'remote-fs-handle-file-name-as-directory args))
    ('directory-file-name
     (apply #'remote-fs-handle-directory-file-name args))
    ('substitute-in-file-name
     (apply #'remote-fs-handle-substitute-in-file-name args))
    ('file-truename (apply #'remote-fs-handle-file-truename args))
    ('set-visited-file-modtime
     (apply #'remote-fs-handle-set-visited-file-modtime args))
    ('verify-visited-file-modtime
     (apply #'remote-fs-handle-verify-visited-file-modtime args))
    ('make-auto-save-file-name
     (remote-fs-handle-make-auto-save-file-name))
    ('insert-file-contents
     (apply #'remote-fs-handle-insert-file-contents args))
    ('directory-files
     (apply #'remote-fs-handle-directory-files args))
    ('directory-files-and-attributes
     (apply #'remote-fs-handle-directory-files-and-attributes args))
    ('file-symlink-p
     (apply #'remote-fs-handle-file-symlink-p args))
    ('file-notify-add-watch
     (apply #'remote-fs-handle-file-notify-add-watch args))
    ('make-process
     (apply #'remote-fs-handle-make-process args))
    ('start-file-process
     (apply #'remote-fs-handle-start-file-process args))
    ('unhandled-file-name-directory nil)
    ((guard
      (eq (alist-get operation
                     tramp-file-name-for-operation-external)
          'process))
     (remote-fs--call-process-operation operation args))
    (_ (remote-fs--call-routed operation args))))

(defun remote-fs-foreign-p (vec)
  "Return non-nil when TRAMP VEC uses the fs logical method."
  ;; Foreign predicates run for every TRAMP backend, including partially
  ;; initialized bootstrap vectors.  They must be total: an error here makes
  ;; TRAMP disable the predicate globally.
  (condition-case nil
      (equal (tramp-file-name-method vec) remote-fs-method)
    (error nil)))

(defun remote-fs--file-local-name-a (fn file-name)
  "Return target-native path for logical FILE-NAME, otherwise call FN."
  (if (remote-fs-file-name-p file-name)
      (remote-fs-localname file-name)
    (funcall fn file-name)))

(defun remote-fs-install ()
  "Install the fs method and foreign handler."
  (remote-fs-register-method)
  ;; The logical handler is reached through TRAMP's outer file-name dispatcher.
  ;; Startup accelerators and init reloads may temporarily remove that entry
  ;; even though TRAMP remains loaded.  Re-register the public dispatcher here
  ;; so enabling `remote-mode' is a complete, self-contained operation.
  (tramp-register-file-name-handlers)
  (unless (assoc #'remote-fs-foreign-p
                 tramp-foreign-file-name-handler-alist)
    (add-to-list 'tramp-foreign-file-name-handler-alist
                 (cons #'remote-fs-foreign-p
                       #'remote-fs-file-name-handler)))
  (remote-fs-register-link-plugins)
  (unless (advice-member-p
           #'remote-fs--file-local-name-a 'file-local-name)
    (advice-add 'file-local-name
                :around #'remote-fs--file-local-name-a))
  (setq remote-fs--advice-installed t))

(defun remote-fs-uninstall ()
  "Remove fs compatibility advice.
The handler remains registered while fs buffers exist."
  (when remote-fs--advice-installed
    (advice-remove 'file-local-name
                   #'remote-fs--file-local-name-a)
    (setq remote-fs--advice-installed nil)))

(provide 'remote-fs)
;;; remote-fs.el ends here
