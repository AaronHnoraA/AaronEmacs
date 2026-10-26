;;; tools/lsp/toolchain.el --- Project toolchains for language servers -*- lexical-binding: t; -*-

;;; Commentary:
;; A language-neutral toolchain selector.  Providers discover
;; language-specific runtimes, while project-local profiles can describe any
;; server/runtime without adding language-specific code to this module.

;;; Code:

(require 'cl-lib)
(require 'tools/lsp/runtime)
(require 'remote-environment)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup +language-server-toolchain nil
  "Project toolchains used to launch language servers."
  :group 'tools)

(defvar +language-server-toolchain-providers nil
  "Registered language toolchain providers.

Each entry is a plist containing `:family', `:modes', `:discover', and
optional `:discover-fast', `:apply' and `:after-select' callbacks.")

(defvar +language-server-toolchain--overrides (make-hash-table :test #'equal)
  "Session-local project toolchain selections.")

(defvar +language-server-toolchain--candidate-cache (make-hash-table :test #'equal)
  "Provider candidate cache keyed by project root and language family.")

(defvar +language-server-toolchain--fast-candidate-cache
  (make-hash-table :test #'equal)
  "Fast automatic-start candidates, separate from full picker results.")

(defvar +language-server--workspace-configuration)

(defvar-local +language-server-toolchain--applied-profile nil
  "Toolchain profile currently applied to this buffer.")

(defvar-local +language-server-toolchain--saved-process-environment nil)
(defvar-local +language-server-toolchain--saved-exec-path nil)
(defvar-local +language-server-toolchain--saved-workspace nil)
(defvar-local +language-server-toolchain--saved-workspace-local-p nil)
(defvar-local +language-server-toolchain-server-program nil
  "Server command selected by the active toolchain profile, if any.
A registered client resolves its command through `lsp-resolve-value', so a
profile overrides the binary by setting this restorable buffer-local
variable rather than by rewriting a global client list.")
(defvar-local +language-server-toolchain--saved-remote-environment nil)
(defvar-local +language-server-toolchain--saved-lsp-variables nil
  "Variables made buffer-local by the active toolchain.
Each entry is `(SYMBOL LOCAL-P VALUE)'.")

(declare-function lsp-disconnect "lsp-mode" ())
(declare-function +direnv-update-environment-maybe
                  "tools/direnv/config" (&optional path callback))
(declare-function +language-server--canonical-root "tools/lsp/config" (root))
(declare-function +language-server--project-root-for-buffer "tools/lsp/config" ())
(declare-function +language-server--merge-values "tools/lsp/config" (base override))
(declare-function +language-server-set-workspace-configuration
                  "tools/lsp/config" (configuration))
(declare-function +language-server-apply-lsp-local-settings "tools/lsp/config")
(declare-function +language-server-apply-process-environment "tools/lsp/config")
(declare-function +language-server/ensure "tools/lsp/config")
(declare-function +language-server-preferred-backend "tools/lsp/config")
(declare-function +project-local-root "tools/project-local/config")
(declare-function +project-local-value "tools/project-local/config" (key &optional root))

(defun +language-server-toolchain--canonical-root (&optional buffer)
  "Return a stable project root for BUFFER.
Shares `+language-server--canonical-root' with the rest of the LSP
stack so toolchain selection and the server's own workspace root always
agree on target identity.  The previous `file-remote-p' branch here
misclassified `/fs:' names (which `file-remote-p' does not recognize as
remote), so a target-only root could be truenamed as if it were local."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (fboundp '+language-server--project-root-for-buffer)
             (+language-server--project-root-for-buffer))
        (+language-server--canonical-root
         (or (and (fboundp '+project-local-root)
                  (+project-local-root))
             (when-let* ((project (project-current nil default-directory)))
               (project-root project))
             default-directory))
        (file-name-as-directory default-directory))))

(defun +language-server-toolchain--mode-match-p (modes)
  "Return non-nil when current buffer derives from one of MODES."
  (let ((modes (if (listp modes) modes (list modes))))
    (and (delq nil modes)
         (apply #'derived-mode-p (delq nil modes)))))

(defun +language-server-toolchain--normalize-profile (entry &optional family)
  "Normalize profile ENTRY and supply FAMILY when missing."
  (let (id profile)
    (cond
     ((and (listp entry) (keywordp (car entry)))
      (setq profile (copy-tree entry)
            id (plist-get profile :id)))
     ((consp entry)
      (setq id (car entry)
            profile (copy-tree (cdr entry))))
     ((or (symbolp entry) (stringp entry))
      (setq id entry
            profile nil)))
    (when id
      (setq profile (plist-put profile :id id))
      (unless (plist-member profile :label)
        (setq profile (plist-put profile :label (format "%s" id))))
      (unless (plist-member profile :family)
        (setq profile (plist-put profile :family family)))
      profile)))

(defun +language-server-toolchain--project-profile-entries (&optional root)
  "Return normalized project-local profiles for ROOT."
  (let ((entries (and (fboundp '+project-local-value)
                      (+project-local-value :toolchain-profiles root))))
    (delq nil
          (mapcar #'+language-server-toolchain--normalize-profile entries))))

(defun +language-server-toolchain-provider (&optional buffer)
  "Return the registered provider matching BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (seq-find
     (lambda (provider)
       (+language-server-toolchain--mode-match-p (plist-get provider :modes)))
     +language-server-toolchain-providers)))

(defun +language-server-toolchain-family (&optional buffer)
  "Return the toolchain family for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (plist-get (+language-server-toolchain-provider) :family)
        (when-let* ((profile
                     (seq-find
                      (lambda (item)
                        (+language-server-toolchain--mode-match-p
                         (plist-get item :modes)))
                      (+language-server-toolchain--project-profile-entries))))
          (plist-get profile :family))
        major-mode)))

(defun +language-server-toolchain--key (&optional buffer)
  "Return the session selection key for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cons (+language-server-toolchain--canonical-root)
          (+language-server-toolchain-family))))

(defun +language-server-toolchain--call (function root)
  "Call FUNCTION with ROOT, accepting legacy zero-argument callbacks."
  (when (functionp function)
    (condition-case nil
        (funcall function root)
      (wrong-number-of-arguments (funcall function)))))

(defun +language-server-toolchain--candidate-key (root family)
  "Return the candidate-cache key for ROOT, FAMILY and this buffer's environment."
  (list root family
        (and (boundp 'remote-buffer-environment)
             remote-buffer-environment
             (remote-environment-key remote-buffer-environment))))

(defun +language-server-toolchain--clear-candidates (root family)
  "Forget cached candidates for ROOT and FAMILY across all environments."
  (dolist (cache (list +language-server-toolchain--candidate-cache
                       +language-server-toolchain--fast-candidate-cache))
    (maphash
     (lambda (key _value)
       (when (and (equal (nth 0 key) root)
                  (equal (nth 1 key) family))
         (remhash key cache)))
     cache)))

(defun +language-server-toolchain--provider-candidates (&optional root family fast)
  "Return cached provider candidates for ROOT and FAMILY.
When FAST is non-nil, use the provider's fast discovery if it has one."
  (let* ((root (or root (+language-server-toolchain--canonical-root)))
         (family (or family (+language-server-toolchain-family)))
         ;; The same project root can acquire a new .envrc while buffers from
         ;; the old environment are still open.  Do not reuse its cached PATH
         ;; candidates for the new capsule.
         (key (+language-server-toolchain--candidate-key root family))
         (provider (+language-server-toolchain-provider))
         (fast-discover (and fast (plist-get provider :discover-fast)))
         (cache (if fast-discover
                    +language-server-toolchain--fast-candidate-cache
                  +language-server-toolchain--candidate-cache))
         (cached (gethash key cache 'missing)))
    (if (not (eq cached 'missing))
        (copy-tree cached)
      (let* ((raw (+language-server-toolchain--call
                   (or fast-discover (plist-get provider :discover)) root))
             (profiles
              (delq nil
                    (mapcar
                     (lambda (entry)
                       (+language-server-toolchain--normalize-profile entry family))
                     raw))))
        (puthash key (copy-tree profiles) cache)
        profiles))))

(defun +language-server-toolchain-candidates (&optional root family fast)
  "Return project and provider profiles for ROOT and FAMILY.
FAST requests bounded discovery for automatic startup when available."
  (let* ((root (or root (+language-server-toolchain--canonical-root)))
         (family (or family (+language-server-toolchain-family)))
         (project-profiles
          (seq-filter
           (lambda (profile)
             (or (eq (plist-get profile :family) family)
                 (and (null (plist-get profile :family))
                      (+language-server-toolchain--mode-match-p
                       (plist-get profile :modes)))))
           (+language-server-toolchain--project-profile-entries root)))
         (profiles (append project-profiles
                           (+language-server-toolchain--provider-candidates
                            root family fast)))
         result)
    (dolist (profile profiles (nreverse result))
      (unless (seq-find (lambda (seen)
                          (equal (plist-get seen :id) (plist-get profile :id)))
                        result)
        (push profile result)))))

(defun +language-server-toolchain--configured-id (&optional root family)
  "Return the .dir-locals toolchain id for ROOT and FAMILY."
  (let* ((root (or root (+language-server-toolchain--canonical-root)))
         (family (or family (+language-server-toolchain-family)))
         (value (and (fboundp '+project-local-value)
                     (+project-local-value :toolchain root))))
    (cond
     ((or (symbolp value) (stringp value)) value)
     ((listp value)
      (or (alist-get family value nil nil #'eq)
          (alist-get family value nil nil #'equal)))
     (t nil))))

(defun +language-server-toolchain--profile-by-id (id profiles)
  "Find ID in PROFILES, accepting symbol/string spelling equivalence."
  (when id
    (seq-find
     (lambda (profile)
       (string= (format "%s" id) (format "%s" (plist-get profile :id))))
     profiles)))

(defun +language-server-current-toolchain-profile (&optional buffer)
  "Return the effective toolchain profile for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (fboundp '+language-server-runtime-current-profile)
             (+language-server-runtime-current-profile))
        (let* ((root (+language-server-toolchain--canonical-root))
               (family (+language-server-toolchain-family))
               (override (gethash (cons root family)
                                  +language-server-toolchain--overrides))
               (configured
                (+language-server-toolchain--configured-id root family))
               (profiles
                (+language-server-toolchain-candidates
                 root family (not (or override configured)))))
          (or (+language-server-toolchain--profile-by-id override profiles)
              (+language-server-toolchain--profile-by-id configured profiles)
              (seq-find (lambda (profile) (plist-get profile :default))
                        profiles))))))

(defun +language-server-toolchain-description (&optional buffer)
  "Return a concise description of BUFFER's effective toolchain."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((profile (+language-server-current-toolchain-profile)))
        (let ((label (plist-get profile :label))
              (executable (plist-get profile :executable)))
          (if executable
              (format "%s — %s" label (abbreviate-file-name executable))
            (format "%s" label)))
      "automatic / PATH")))

(defun +register-language-server-toolchain-provider (family modes discover &rest properties)
  "Register a toolchain provider for FAMILY and MODES.

DISCOVER returns profile plists for a project root.  PROPERTIES accepts
`:discover-fast', `:apply', `:after-select', `:label', and `:source'."
  (let ((entry (append (list :family family :modes modes :discover discover)
                       properties)))
    (clrhash +language-server-toolchain--candidate-cache)
    (clrhash +language-server-toolchain--fast-candidate-cache)
    (setq +language-server-toolchain-providers
          (cons entry
                (seq-remove
                 (lambda (provider)
                   (eq (plist-get provider :family) family))
                 +language-server-toolchain-providers)))
    entry))

(defun +language-server-toolchain--resolve-value (value root)
  "Resolve profile VALUE for ROOT."
  (if (functionp value)
      (+language-server-toolchain--call value root)
    value))

(defun +language-server-toolchain--merge-workspace (base overlay)
  "Merge OVERLAY workspace configuration into BASE."
  (if (fboundp '+language-server--merge-values)
      (+language-server--merge-values base overlay)
    (append (copy-tree overlay) (copy-tree base))))

(defun +language-server-toolchain-restore-buffer ()
  "Remove the previously applied toolchain from the current buffer."
  (when +language-server-toolchain--applied-profile
    (setq-local process-environment
                (copy-sequence +language-server-toolchain--saved-process-environment))
    (setq-local exec-path (copy-sequence +language-server-toolchain--saved-exec-path))
    (when (boundp 'remote-buffer-environment)
      (setq-local remote-buffer-environment
                  +language-server-toolchain--saved-remote-environment))
    (if +language-server-toolchain--saved-workspace-local-p
        (setq-local +language-server--workspace-configuration
                    (copy-tree +language-server-toolchain--saved-workspace))
      (kill-local-variable '+language-server--workspace-configuration))
    (dolist (entry +language-server-toolchain--saved-lsp-variables)
      (pcase-let ((`(,variable ,local-p ,value) entry))
        (if local-p
            (set (make-local-variable variable) value)
          (kill-local-variable variable))))
    (setq +language-server-toolchain--applied-profile nil
          +language-server-toolchain--saved-process-environment nil
          +language-server-toolchain--saved-exec-path nil
          +language-server-toolchain--saved-workspace nil
          +language-server-toolchain--saved-workspace-local-p nil
          +language-server-toolchain--saved-remote-environment nil
          +language-server-toolchain--saved-lsp-variables nil)))

(defun +language-server-toolchain-set-local-variable (variable value)
  "Set VARIABLE buffer-locally to VALUE and make it restorable.
Providers use this for client-specific lsp-mode settings without leaking one
project's SDK, server cache, or workspace directory into another buffer."
  (unless (assq variable +language-server-toolchain--saved-lsp-variables)
    (push (list variable
                (local-variable-p variable)
                (and (boundp variable) (symbol-value variable)))
          +language-server-toolchain--saved-lsp-variables))
  (set (make-local-variable variable) value))

(defun +language-server-toolchain--prepend-path (directories)
  "Prepend existing DIRECTORIES to buffer-local PATH and `exec-path'."
  (let* ((root (+language-server-toolchain--canonical-root))
         (directories
          (delq nil
                (mapcar
                 (lambda (directory)
                   (when (stringp directory)
                     (let ((path (expand-file-name directory root)))
                       (when (file-directory-p path) path))))
                 directories))))
    (when directories
      (setq-local exec-path
                  (append directories
                          (seq-remove (lambda (path) (member path directories)) exec-path)))
      (let* ((current (split-string (or (getenv "PATH") "") path-separator t))
             (path (append directories
                           (seq-remove (lambda (item) (member item directories)) current))))
        (setenv "PATH" (mapconcat #'identity path path-separator))))))

(defun +language-server-toolchain--target-paths (directories root)
  "Return existing target-native DIRECTORIES relative to ROOT."
  (delq nil
        (mapcar
         (lambda (directory)
           (when (stringp directory)
             (let ((path (expand-file-name directory root)))
               (when (file-directory-p path)
                 (if (fboundp 'remote-file-local-name)
                     (remote-file-local-name path)
                   path)))))
         directories)))

(defun +language-server-toolchain-apply-environment ()
  "Apply the effective toolchain environment to the current buffer."
  (+language-server-toolchain-restore-buffer)
  (when-let* ((profile (+language-server-current-toolchain-profile))
              (root (+language-server-toolchain--canonical-root)))
    (setq +language-server-toolchain--saved-process-environment
          (copy-sequence process-environment)
          +language-server-toolchain--saved-exec-path (copy-sequence exec-path)
          +language-server-toolchain--saved-workspace-local-p
          (local-variable-p '+language-server--workspace-configuration)
          +language-server-toolchain--saved-workspace
          (and (boundp '+language-server--workspace-configuration)
               (copy-tree +language-server--workspace-configuration))
          +language-server-toolchain--saved-remote-environment
          (and (boundp 'remote-buffer-environment)
               remote-buffer-environment))
    (setq-local process-environment (copy-sequence process-environment))
    (let* ((environment
            (+language-server-toolchain--resolve-value
             (plist-get profile :env) root))
           (paths
            (+language-server-toolchain--resolve-value
             (plist-get profile :path-prepend) root))
           (paths (if (listp paths) paths (and paths (list paths)))))
      (if (and (boundp 'remote-buffer-environment)
               remote-buffer-environment
               (fboundp 'remote-environment-derive)
               (fboundp 'remote-environment-apply))
          (let* ((profile-id (format "%s" (plist-get profile :id)))
                 (id
                  (or (and (fboundp 'remote-normalize-id)
                           (remote-normalize-id profile-id t))
                      (format "toolchain-%s"
                              (substring
                               (secure-hash 'sha1 profile-id) 0 10))))
                 (derived
                  (remote-environment-derive
                   remote-buffer-environment id
                   :scope 'toolchain
                   :vars environment
                   :path-prepend
                   (+language-server-toolchain--target-paths
                    paths root)
                   :source (list 'toolchain id))))
            (remote-environment-apply derived))
        (dolist (entry environment)
          (when (consp entry)
            (setenv (format "%s" (car entry))
                    (when (cdr entry) (format "%s" (cdr entry))))))
        (+language-server-toolchain--prepend-path paths)))
    (when-let* ((program
                 (+language-server-toolchain--resolve-value
                  (plist-get profile :server-program) root)))
      ;; lsp-mode resolves a client's command through `lsp-resolve-value', so a
      ;; profile-selected binary is expressed as a restorable buffer-local
      ;; variable rather than by rewriting a global client list.
      (+language-server-toolchain-set-local-variable
       '+language-server-toolchain-server-program program))
    (when-let* ((provider (+language-server-toolchain-provider))
                (apply-function (plist-get provider :apply)))
      (funcall apply-function profile root))
    (setq +language-server-toolchain--applied-profile profile)))

(defun +language-server-toolchain-apply-lsp-settings ()
  "Merge the effective toolchain workspace settings into this buffer."
  (when-let* ((profile +language-server-toolchain--applied-profile)
              (root (+language-server-toolchain--canonical-root))
              (workspace
               (+language-server-toolchain--resolve-value
                (plist-get profile :workspace) root)))
    (setq-local +language-server--workspace-configuration
                (+language-server-toolchain--merge-workspace
                 (and (boundp '+language-server--workspace-configuration)
                      +language-server--workspace-configuration)
                 workspace))))

(defun +language-server-toolchain--affected-buffers (root family)
  "Return live programming buffers belonging to ROOT and FAMILY."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'prog-mode)
            (equal root (+language-server-toolchain--canonical-root))
            (eq family (+language-server-toolchain-family)))))
   (buffer-list)))

(defun +language-server-toolchain--reapply (root family)
  "Reapply ROOT/FAMILY and restart the active language servers."
  (let* ((buffers (+language-server-toolchain--affected-buffers root family))
         (managed
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (bound-and-true-p lsp-managed-mode)))
           buffers)))
    (dolist (buffer managed)
      (with-current-buffer buffer
        (when (fboundp 'lsp-disconnect)
          (ignore-errors (lsp-disconnect)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (+language-server-toolchain-restore-buffer)
        (when (fboundp '+direnv-update-environment-maybe)
          (+direnv-update-environment-maybe root))
        (when (fboundp '+language-server-apply-process-environment)
          (+language-server-apply-process-environment))
        (when (and (fboundp '+language-server-preferred-backend)
                   (eq (+language-server-preferred-backend) 'lsp-mode)
                   (fboundp '+language-server-apply-lsp-local-settings))
          (+language-server-apply-lsp-local-settings))))
    (dolist (buffer managed)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (fboundp '+language-server/ensure)
            (+language-server/ensure)))))))

;;;###autoload
(defun +language-server/select-toolchain ()
  "Select a toolchain for the current project and language family."
  (interactive)
  (when (and (fboundp '+language-server-runtime-current-profile)
             (+language-server-runtime-current-profile))
    (user-error "Toolchain is controlled by runtime context: %s"
                (+language-server-runtime-description)))
  (let* ((root (+language-server-toolchain--canonical-root))
         (family (+language-server-toolchain-family))
         (profiles (+language-server-toolchain-candidates root family))
         (current (+language-server-current-toolchain-profile)))
    (unless profiles
      (user-error "No %s toolchains discovered or configured" family))
    (let* ((choices
            (mapcar
             (lambda (profile)
               (cons (format "%s%s  [%s]"
                             (if (equal (plist-get profile :id)
                                        (plist-get current :id)) "● " "  ")
                             (plist-get profile :label)
                             (plist-get profile :id))
                     profile))
             profiles))
           (choice (completing-read
                    (format "Select %s toolchain: " family)
                    choices nil t))
           (profile (cdr (assoc choice choices)))
           (provider (+language-server-toolchain-provider)))
      (puthash (cons root family) (plist-get profile :id)
               +language-server-toolchain--overrides)
      (when-let* ((after-select (plist-get provider :after-select)))
        (funcall after-select profile root))
      (+language-server-toolchain--reapply root family)
      (message "%s toolchain: %s" family
               (+language-server-toolchain-description)))))

;;;###autoload
(defun +language-server/reset-toolchain ()
  "Reset the session toolchain override to the project default."
  (interactive)
  (let* ((root (+language-server-toolchain--canonical-root))
         (family (+language-server-toolchain-family)))
    (remhash (cons root family) +language-server-toolchain--overrides)
    (+language-server-toolchain--reapply root family)
    (message "%s toolchain reset: %s" family
             (+language-server-toolchain-description))))

;;;###autoload
(defun +language-server/refresh-toolchains ()
  "Refresh discovered toolchains for the current project and language."
  (interactive)
  (let ((key (+language-server-toolchain--key)))
    (+language-server-toolchain--clear-candidates (car key) (cdr key))
    (message "Refreshed %s toolchains (%d candidates)"
             (cdr key) (length (+language-server-toolchain-candidates)))))

(provide 'tools/lsp/toolchain)
;;; tools/lsp/toolchain.el ends here
