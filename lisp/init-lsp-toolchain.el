;;; init-lsp-toolchain.el --- Project toolchains for Eglot -*- lexical-binding: t; -*-

;;; Commentary:
;; A language-neutral toolchain selector for Eglot.  Providers discover
;; language-specific runtimes, while project-local profiles can describe any
;; server/runtime without adding language-specific code to this module.

;;; Code:

(require 'cl-lib)
(require 'init-lsp-runtime)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup my/language-server-toolchain nil
  "Project toolchains used to launch language servers."
  :group 'tools)

(defvar my/language-server-toolchain-providers nil
  "Registered language toolchain providers.

Each entry is a plist containing `:family', `:modes', `:discover', and
optional `:apply' and `:after-select' callbacks.")

(defvar my/language-server-toolchain--overrides (make-hash-table :test #'equal)
  "Session-local project toolchain selections.")

(defvar my/language-server-toolchain--candidate-cache (make-hash-table :test #'equal)
  "Provider candidate cache keyed by project root and language family.")

(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)

(defvar-local my/language-server-toolchain--applied-profile nil
  "Toolchain profile currently applied to this buffer.")

(defvar-local my/language-server-toolchain--saved-process-environment nil)
(defvar-local my/language-server-toolchain--saved-exec-path nil)
(defvar-local my/language-server-toolchain--saved-workspace nil)
(defvar-local my/language-server-toolchain--saved-workspace-local-p nil)
(defvar-local my/language-server-toolchain--saved-server-programs nil)
(defvar-local my/language-server-toolchain--saved-server-programs-local-p nil)
(defvar-local my/language-server-toolchain--saved-remote-environment nil)
(defvar-local my/language-server-toolchain--saved-lsp-variables nil
  "Variables made buffer-local by the active toolchain.
Each entry is `(SYMBOL LOCAL-P VALUE)'.")

(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-shutdown "eglot" (server))
(declare-function lsp-disconnect "lsp-mode" ())
(declare-function my/direnv-update-environment-maybe
                  "init-direnv" (&optional path callback))
(declare-function my/language-server--merge-values "init-lsp" (base override))
(declare-function my/language-server-apply-eglot-local-settings "init-lsp")
(declare-function my/language-server-apply-lsp-local-settings "init-lsp")
(declare-function my/language-server-apply-process-environment "init-lsp")
(declare-function my/language-server-ensure "init-lsp")
(declare-function my/language-server-preferred-backend "init-lsp")
(declare-function my/project-local-root "init-project-local")
(declare-function my/project-local-value "init-project-local" (key &optional root))

(defun my/language-server-toolchain--canonical-root (&optional buffer)
  "Return a stable project root for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((root (or (and (fboundp 'my/project-local-root)
                          (my/project-local-root))
                     (when-let* ((project (project-current nil default-directory)))
                       (project-root project))
                     default-directory))
           (expanded (file-name-as-directory (expand-file-name root))))
      (if (file-remote-p expanded)
          expanded
        (file-name-as-directory
         (or (ignore-errors (file-truename expanded)) expanded))))))

(defun my/language-server-toolchain--mode-match-p (modes)
  "Return non-nil when current buffer derives from one of MODES."
  (let ((modes (if (listp modes) modes (list modes))))
    (and (delq nil modes)
         (apply #'derived-mode-p (delq nil modes)))))

(defun my/language-server-toolchain--normalize-profile (entry &optional family)
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

(defun my/language-server-toolchain--project-profile-entries (&optional root)
  "Return normalized project-local profiles for ROOT."
  (let ((entries (and (fboundp 'my/project-local-value)
                      (my/project-local-value :toolchain-profiles root))))
    (delq nil
          (mapcar #'my/language-server-toolchain--normalize-profile entries))))

(defun my/language-server-toolchain-provider (&optional buffer)
  "Return the registered provider matching BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (seq-find
     (lambda (provider)
       (my/language-server-toolchain--mode-match-p (plist-get provider :modes)))
     my/language-server-toolchain-providers)))

(defun my/language-server-toolchain-family (&optional buffer)
  "Return the toolchain family for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (plist-get (my/language-server-toolchain-provider) :family)
        (when-let* ((profile
                     (seq-find
                      (lambda (item)
                        (my/language-server-toolchain--mode-match-p
                         (plist-get item :modes)))
                      (my/language-server-toolchain--project-profile-entries))))
          (plist-get profile :family))
        major-mode)))

(defun my/language-server-toolchain--key (&optional buffer)
  "Return the session selection key for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cons (my/language-server-toolchain--canonical-root)
          (my/language-server-toolchain-family))))

(defun my/language-server-toolchain--call (function root)
  "Call FUNCTION with ROOT, accepting legacy zero-argument callbacks."
  (when (functionp function)
    (condition-case nil
        (funcall function root)
      (wrong-number-of-arguments (funcall function)))))

(defun my/language-server-toolchain--provider-candidates (&optional root family)
  "Return cached provider candidates for ROOT and FAMILY."
  (let* ((root (or root (my/language-server-toolchain--canonical-root)))
         (family (or family (my/language-server-toolchain-family)))
         (key (cons root family))
         (cached (gethash key my/language-server-toolchain--candidate-cache 'missing)))
    (if (not (eq cached 'missing))
        (copy-tree cached)
      (let* ((provider (my/language-server-toolchain-provider))
             (raw (my/language-server-toolchain--call
                   (plist-get provider :discover) root))
             (profiles
              (delq nil
                    (mapcar
                     (lambda (entry)
                       (my/language-server-toolchain--normalize-profile entry family))
                     raw))))
        (puthash key (copy-tree profiles)
                 my/language-server-toolchain--candidate-cache)
        profiles))))

(defun my/language-server-toolchain-candidates (&optional root family)
  "Return project and provider profiles for ROOT and FAMILY."
  (let* ((root (or root (my/language-server-toolchain--canonical-root)))
         (family (or family (my/language-server-toolchain-family)))
         (project-profiles
          (seq-filter
           (lambda (profile)
             (or (eq (plist-get profile :family) family)
                 (and (null (plist-get profile :family))
                      (my/language-server-toolchain--mode-match-p
                       (plist-get profile :modes)))))
           (my/language-server-toolchain--project-profile-entries root)))
         (profiles (append project-profiles
                           (my/language-server-toolchain--provider-candidates root family)))
         result)
    (dolist (profile profiles (nreverse result))
      (unless (seq-find (lambda (seen)
                          (equal (plist-get seen :id) (plist-get profile :id)))
                        result)
        (push profile result)))))

(defun my/language-server-toolchain--configured-id (&optional root family)
  "Return the .dir-locals toolchain id for ROOT and FAMILY."
  (let* ((root (or root (my/language-server-toolchain--canonical-root)))
         (family (or family (my/language-server-toolchain-family)))
         (value (and (fboundp 'my/project-local-value)
                     (my/project-local-value :toolchain root))))
    (cond
     ((or (symbolp value) (stringp value)) value)
     ((listp value)
      (or (alist-get family value nil nil #'eq)
          (alist-get family value nil nil #'equal)))
     (t nil))))

(defun my/language-server-toolchain--profile-by-id (id profiles)
  "Find ID in PROFILES, accepting symbol/string spelling equivalence."
  (when id
    (seq-find
     (lambda (profile)
       (string= (format "%s" id) (format "%s" (plist-get profile :id))))
     profiles)))

(defun my/language-server-current-toolchain-profile (&optional buffer)
  "Return the effective toolchain profile for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((runtime-profile
            (and (fboundp 'my/language-server-runtime-current-profile)
                 (my/language-server-runtime-current-profile)))
           (root (my/language-server-toolchain--canonical-root))
           (family (my/language-server-toolchain-family))
           (profiles (my/language-server-toolchain-candidates root family))
           (override (gethash (cons root family)
                              my/language-server-toolchain--overrides))
           (configured (my/language-server-toolchain--configured-id root family)))
      (or runtime-profile
          (my/language-server-toolchain--profile-by-id override profiles)
          (my/language-server-toolchain--profile-by-id configured profiles)
          (seq-find (lambda (profile) (plist-get profile :default)) profiles)))))

(defun my/language-server-toolchain-description (&optional buffer)
  "Return a concise description of BUFFER's effective toolchain."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((profile (my/language-server-current-toolchain-profile)))
        (let ((label (plist-get profile :label))
              (executable (plist-get profile :executable)))
          (if executable
              (format "%s — %s" label (abbreviate-file-name executable))
            (format "%s" label)))
      "automatic / PATH")))

(defun my/register-language-server-toolchain-provider (family modes discover &rest properties)
  "Register a toolchain provider for FAMILY and MODES.

DISCOVER returns profile plists for a project root.  PROPERTIES accepts
`:apply', `:after-select', `:label', and `:source'."
  (let ((entry (append (list :family family :modes modes :discover discover)
                       properties)))
    (setq my/language-server-toolchain-providers
          (cons entry
                (seq-remove
                 (lambda (provider)
                   (eq (plist-get provider :family) family))
                 my/language-server-toolchain-providers)))
    entry))

(defun my/language-server-toolchain--resolve-value (value root)
  "Resolve profile VALUE for ROOT."
  (if (functionp value)
      (my/language-server-toolchain--call value root)
    value))

(defun my/language-server-toolchain--merge-workspace (base overlay)
  "Merge OVERLAY workspace configuration into BASE."
  (if (fboundp 'my/language-server--merge-values)
      (my/language-server--merge-values base overlay)
    (append (copy-tree overlay) (copy-tree base))))

(defun my/language-server-toolchain-restore-buffer ()
  "Remove the previously applied toolchain from the current buffer."
  (when my/language-server-toolchain--applied-profile
    (setq-local process-environment
                (copy-sequence my/language-server-toolchain--saved-process-environment))
    (setq-local exec-path (copy-sequence my/language-server-toolchain--saved-exec-path))
    (when (boundp 'remote-buffer-environment)
      (setq-local remote-buffer-environment
                  my/language-server-toolchain--saved-remote-environment))
    (if my/language-server-toolchain--saved-workspace-local-p
        (setq-local eglot-workspace-configuration
                    (copy-tree my/language-server-toolchain--saved-workspace))
      (kill-local-variable 'eglot-workspace-configuration))
    (if my/language-server-toolchain--saved-server-programs-local-p
        (setq-local eglot-server-programs
                    (copy-tree my/language-server-toolchain--saved-server-programs))
      (kill-local-variable 'eglot-server-programs))
    (dolist (entry my/language-server-toolchain--saved-lsp-variables)
      (pcase-let ((`(,variable ,local-p ,value) entry))
        (if local-p
            (set (make-local-variable variable) value)
          (kill-local-variable variable))))
    (setq my/language-server-toolchain--applied-profile nil
          my/language-server-toolchain--saved-process-environment nil
          my/language-server-toolchain--saved-exec-path nil
          my/language-server-toolchain--saved-workspace nil
          my/language-server-toolchain--saved-workspace-local-p nil
          my/language-server-toolchain--saved-server-programs nil
          my/language-server-toolchain--saved-server-programs-local-p nil
          my/language-server-toolchain--saved-remote-environment nil
          my/language-server-toolchain--saved-lsp-variables nil)))

(defun my/language-server-toolchain-set-local-variable (variable value)
  "Set VARIABLE buffer-locally to VALUE and make it restorable.
Providers use this for client-specific lsp-mode settings without leaking one
project's SDK, server cache, or workspace directory into another buffer."
  (unless (assq variable my/language-server-toolchain--saved-lsp-variables)
    (push (list variable
                (local-variable-p variable)
                (and (boundp variable) (symbol-value variable)))
          my/language-server-toolchain--saved-lsp-variables))
  (set (make-local-variable variable) value))

(defun my/language-server-toolchain--prepend-path (directories)
  "Prepend existing DIRECTORIES to buffer-local PATH and `exec-path'."
  (let* ((root (my/language-server-toolchain--canonical-root))
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

(defun my/language-server-toolchain--target-paths (directories root)
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

(defun my/language-server-toolchain-apply-environment ()
  "Apply the effective toolchain environment to the current buffer."
  (my/language-server-toolchain-restore-buffer)
  (when-let* ((profile (my/language-server-current-toolchain-profile))
              (root (my/language-server-toolchain--canonical-root)))
    (setq my/language-server-toolchain--saved-process-environment
          (copy-sequence process-environment)
          my/language-server-toolchain--saved-exec-path (copy-sequence exec-path)
          my/language-server-toolchain--saved-workspace-local-p
          (local-variable-p 'eglot-workspace-configuration)
          my/language-server-toolchain--saved-workspace
          (and (boundp 'eglot-workspace-configuration)
               (copy-tree eglot-workspace-configuration))
          my/language-server-toolchain--saved-server-programs-local-p
          (local-variable-p 'eglot-server-programs)
          my/language-server-toolchain--saved-server-programs
          (and (boundp 'eglot-server-programs) (copy-tree eglot-server-programs))
          my/language-server-toolchain--saved-remote-environment
          (and (boundp 'remote-buffer-environment)
               remote-buffer-environment))
    (setq-local process-environment (copy-sequence process-environment))
    (let* ((environment
            (my/language-server-toolchain--resolve-value
             (plist-get profile :env) root))
           (paths
            (my/language-server-toolchain--resolve-value
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
                   (my/language-server-toolchain--target-paths
                    paths root)
                   :source (list 'toolchain id))))
            (remote-environment-apply derived))
        (dolist (entry environment)
          (when (consp entry)
            (setenv (format "%s" (car entry))
                    (when (cdr entry) (format "%s" (cdr entry))))))
        (my/language-server-toolchain--prepend-path paths)))
    (when-let* ((program
                 (my/language-server-toolchain--resolve-value
                  (plist-get profile :server-program) root)))
      (setq-local eglot-server-programs
                  (cons (cons (or (plist-get profile :modes) major-mode) program)
                        (copy-tree eglot-server-programs))))
    (when-let* ((provider (my/language-server-toolchain-provider))
                (apply-function (plist-get provider :apply)))
      (funcall apply-function profile root))
    (setq my/language-server-toolchain--applied-profile profile)))

(defun my/language-server-toolchain-apply-eglot-settings ()
  "Merge the effective toolchain workspace settings into this buffer."
  (when-let* ((profile my/language-server-toolchain--applied-profile)
              (root (my/language-server-toolchain--canonical-root))
              (workspace
               (my/language-server-toolchain--resolve-value
                (plist-get profile :workspace) root)))
    (setq-local eglot-workspace-configuration
                (my/language-server-toolchain--merge-workspace
                 (and (boundp 'eglot-workspace-configuration)
                      eglot-workspace-configuration)
                 workspace))))

(defun my/language-server-toolchain--affected-buffers (root family)
  "Return live programming buffers belonging to ROOT and FAMILY."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'prog-mode)
            (equal root (my/language-server-toolchain--canonical-root))
            (eq family (my/language-server-toolchain-family)))))
   (buffer-list)))

(defun my/language-server-toolchain--reapply (root family)
  "Reapply ROOT/FAMILY and restart active Eglot or lsp-mode clients."
  (let* ((buffers (my/language-server-toolchain--affected-buffers root family))
         (eglot-managed
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (and (fboundp 'eglot-managed-p) (eglot-managed-p))))
           buffers))
         (lsp-managed
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (bound-and-true-p lsp-managed-mode)))
           buffers))
         (managed (delete-dups (append eglot-managed lsp-managed)))
         servers)
    (dolist (buffer eglot-managed)
      (with-current-buffer buffer
        (when-let* ((server (ignore-errors (eglot-current-server))))
          (cl-pushnew server servers))))
    (dolist (server servers)
      (ignore-errors (eglot-shutdown server)))
    (dolist (buffer lsp-managed)
      (with-current-buffer buffer
        (when (fboundp 'lsp-disconnect)
          (ignore-errors (lsp-disconnect)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (my/language-server-toolchain-restore-buffer)
        (when (fboundp 'my/direnv-update-environment-maybe)
          (my/direnv-update-environment-maybe root))
        (when (fboundp 'my/language-server-apply-process-environment)
          (my/language-server-apply-process-environment))
        (pcase
            (and (fboundp 'my/language-server-preferred-backend)
                 (my/language-server-preferred-backend))
          ('lsp-mode
           (when (fboundp 'my/language-server-apply-lsp-local-settings)
             (my/language-server-apply-lsp-local-settings)))
          (_
           (when (fboundp 'my/language-server-apply-eglot-local-settings)
             (my/language-server-apply-eglot-local-settings))))))
    (dolist (buffer managed)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (fboundp 'my/language-server-ensure)
            (my/language-server-ensure)))))))

;;;###autoload
(defun my/language-server-select-toolchain ()
  "Select a toolchain for the current project and language family."
  (interactive)
  (when (and (fboundp 'my/language-server-runtime-current-profile)
             (my/language-server-runtime-current-profile))
    (user-error "Toolchain is controlled by runtime context: %s"
                (my/language-server-runtime-description)))
  (let* ((root (my/language-server-toolchain--canonical-root))
         (family (my/language-server-toolchain-family))
         (profiles (my/language-server-toolchain-candidates root family))
         (current (my/language-server-current-toolchain-profile)))
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
           (provider (my/language-server-toolchain-provider)))
      (puthash (cons root family) (plist-get profile :id)
               my/language-server-toolchain--overrides)
      (when-let* ((after-select (plist-get provider :after-select)))
        (funcall after-select profile root))
      (my/language-server-toolchain--reapply root family)
      (message "%s toolchain: %s" family
               (my/language-server-toolchain-description)))))

;;;###autoload
(defun my/language-server-reset-toolchain ()
  "Reset the session toolchain override to the project default."
  (interactive)
  (let* ((root (my/language-server-toolchain--canonical-root))
         (family (my/language-server-toolchain-family)))
    (remhash (cons root family) my/language-server-toolchain--overrides)
    (my/language-server-toolchain--reapply root family)
    (message "%s toolchain reset: %s" family
             (my/language-server-toolchain-description))))

;;;###autoload
(defun my/language-server-refresh-toolchains ()
  "Refresh discovered toolchains for the current project and language."
  (interactive)
  (let ((key (my/language-server-toolchain--key)))
    (remhash key my/language-server-toolchain--candidate-cache)
    (message "Refreshed %s toolchains (%d candidates)"
             (cdr key) (length (my/language-server-toolchain-candidates)))))

(provide 'init-lsp-toolchain)
;;; init-lsp-toolchain.el ends here
