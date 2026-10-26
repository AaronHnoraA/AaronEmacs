;;; lang/python/config.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'json)
(require 'remote-core)
(require 'remote-process)
(require 'remote-service)
(require 'seq)
(require 'subr-x)

(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function +language-server-set-workspace-configuration
                  "tools/lsp/config" (configuration))
(declare-function set-language-server! "tools/lsp/config")
(declare-function set-language-server-toolchain-provider! "tools/lsp/toolchain"
                  (family modes discover &rest properties))
(declare-function +python-imenu-create-index "lang/python/config" ())
(declare-function +python/setup-imenu "lang/python/config" ())
(declare-function +python-refresh-open-buffers "lang/python/config" ())
(declare-function +python-ensure-imenu-around "lang/python/config"
                  (function &optional noerror))
(declare-function python-imenu-create-index "python" ())
(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-expand-file-name "remote-fs"
                  (file-name &optional directory target))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function remote-executable-find "remote-process"
                  (program &optional context))
(defvar imenu-create-index-function)
(defvar lsp-enabled-clients)
(defvar-local +python-imenu-backend nil
  "Original Python imenu backend for the current buffer.")

(defcustom +lsp-python-target-cache-root
  "~/.cache/emacs-remote/lsp-python/"
  "Target-native cache root for provisioned Python language servers."
  :type 'string
  :group '+language-server)

(defcustom +lsp-python-auto-provision-target t
  "Whether trusted targets may receive the client's Pyright bundle.
Provisioning is used when the target PATH has no preferred Pyright-family
server, before falling back to less complete analyzers such as pylsp.  The
analyzer still executes on the target and therefore sees the same filesystem,
interpreter and environment as the remote buffer."
  :type 'boolean
  :group '+language-server)

(defun +lsp-python--local-pyright-directory ()
  "Return the client Pyright package directory, or nil when unavailable."
  (when-let* ((launcher (remote-client-executable-find "pyright-langserver"))
              (real (ignore-errors (file-truename launcher)))
              (directory (file-name-directory real))
              ((file-exists-p (expand-file-name "package.json" directory)))
              ((file-exists-p
                (expand-file-name "langserver.index.js" directory))))
    (file-name-as-directory directory)))

(defun +lsp-python--pyright-version (directory)
  "Return Pyright's package version from DIRECTORY."
  (or
   (condition-case nil
       (let ((object
              (json-parse-string
               (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "package.json" directory))
                 (buffer-string))
               :object-type 'alist)))
         (alist-get 'version object))
     (error nil))
   (substring (secure-hash 'sha1 directory) 0 12)))

(defun +lsp-python--target-install-directory (context source)
  "Return Pyright's versioned target directory for CONTEXT and SOURCE."
  (file-name-as-directory
   (remote-expand-file-name
    (format "%spyright/%s/"
            (file-name-as-directory +lsp-python-target-cache-root)
            (+lsp-python--pyright-version source))
    nil context)))

(defun +lsp-python--target-launcher (directory)
  "Return the target Pyright launcher below DIRECTORY."
  (expand-file-name "bin/pyright-langserver" directory))

(defun +lsp-python--provision-pyright (context source directory)
  "Provision Pyright SOURCE for CONTEXT into target DIRECTORY."
  (unless +lsp-python-auto-provision-target
    (error "Python language server is missing and provisioning is disabled"))
  (let ((remote-current-adapter-id "language-server"))
    (unless (remote-executable-find "node" context)
      (error "Provisioned Pyright requires node on target %s"
             (remote-context-target-id context))))
  (remote-service-provision-directory
   "pyright" source directory
   :context context
   :adapter "language-server"
   :payload-directory "lib"
   :ready-file "bin/pyright-langserver"
   :ready-kind 'executable
   :prepare
   (lambda (provision-context staging)
     (remote-exec
      "sh"
      :args
      (list
       "-c"
       (concat
        "set -eu\n"
        "staging=$1\n"
        "mkdir -p -- \"$staging/bin\"\n"
        "cat > \"$staging/bin/pyright-langserver\" <<'PYRIGHT_LAUNCHER'\n"
        "#!/bin/sh\n"
        "root=$(CDPATH= cd -- \"$(dirname -- \"$0\")/..\" && pwd)\n"
        "exec node \"$root/lib/langserver.index.js\" \"$@\"\n"
        "PYRIGHT_LAUNCHER\n"
        "chmod u+x \"$staging/bin/pyright-langserver\"\n")
       "pyright-prepare"
       (remote-file-local-name (directory-file-name staging)))
      :context provision-context :adapter "language-server" :check t)))
  (unless (file-executable-p (+lsp-python--target-launcher directory))
    (error "Target Pyright provisioning did not produce a launcher")))

(defun +lsp-python--provisioned-command ()
  "Return a target-provisioned Pyright command, or nil if unavailable."
  (when-let* ((source (+lsp-python--local-pyright-directory))
              (context (remote-context default-directory))
              (directory
               (+lsp-python--target-install-directory context source))
              (launcher (+lsp-python--target-launcher directory)))
    (unless (file-executable-p launcher)
      (+lsp-python--provision-pyright context source directory))
    (list (remote-file-local-name launcher) "--stdio")))

(defun +python-toolchain--command-json (program &rest arguments)
  "Run PROGRAM with ARGUMENTS and return the last JSON object it prints."
  (when (and program
             (file-executable-p
              (+python-toolchain--logical-executable program)))
    (with-temp-buffer
      (when (zerop (apply #'process-file program nil t nil arguments))
        (goto-char (point-max))
        (when (re-search-backward "^[[:space:]]*{" nil t)
          (condition-case nil
              (json-parse-buffer :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil)
            (error nil)))))))

(defun +python-toolchain--logical-executable (path)
  "Project target-native executable PATH into the current logical target."
  (if (and (stringp path)
           (file-name-absolute-p path))
      (remote-expand-file-name path nil (remote-context))
    path))

(defun +python-toolchain--canonical-executable (path)
  "Return executable PATH as it must be invoked, or nil when it cannot run.
Symlinks are kept: a virtual environment's `bin/python' usually links to the
base interpreter, and Python finds `pyvenv.cfg' from the path it was started
by.  Resolving the link would silently run the base interpreter instead."
  (when-let* ((logical (and (stringp path)
                            (+python-toolchain--logical-executable path)))
              ((file-executable-p logical)))
    (remote-file-local-name (expand-file-name logical))))

(defun +python-toolchain--identity (executable)
  "Return the key under which EXECUTABLE duplicates another profile.
Two names for one interpreter in the same directory (`python', `python3')
are one profile; the same interpreter behind a venv directory is not."
  (let ((logical (+python-toolchain--logical-executable executable)))
    (cons (file-name-directory executable)
          (remote-file-local-name
           (or (ignore-errors (file-truename logical)) logical)))))

(defun +python-toolchain--workspace (executable &optional extra-paths)
  "Build workspace settings for EXECUTABLE and EXTRA-PATHS."
  (let ((paths (vconcat (delq nil extra-paths))))
    `(:python (:pythonPath ,executable
               :analysis (:autoSearchPaths t
                          :useLibraryCodeForTypes t
                          ,@(when (> (length paths) 0)
                              `(:extraPaths ,paths))))
      :pyright (:pythonPath ,executable)
      :basedpyright (:pythonPath ,executable)
      :pylsp (:plugins (:jedi (:environment ,executable))))))

(defun +python-toolchain--profile (id label executable &rest properties)
  "Create a Python profile ID and LABEL around EXECUTABLE and PROPERTIES."
  (when-let* ((executable (+python-toolchain--canonical-executable executable)))
    (append
     (list :id id
           :label label
           :family 'python
           :executable executable
           :path-prepend (list (file-name-directory executable))
           :workspace (+python-toolchain--workspace executable))
     properties)))

(defun +python-toolchain--project-venvs (root)
  "Return virtual-environment profiles found below ROOT."
  (let (profiles)
    (dolist (name '(".venv" "venv"))
      (let* ((environment (expand-file-name name root))
             (python (expand-file-name "bin/python" environment)))
        (when-let* ((profile
                     (+python-toolchain--profile
                      (format "venv:%s" name)
                      (format "Python %s" name)
                      python
                      :default (string= name ".venv")
                      :kind 'venv
                      :env `(("VIRTUAL_ENV" . ,environment)
                             ("CONDA_PREFIX" . nil)
                             ("CONDA_DEFAULT_ENV" . nil)))))
          (push profile profiles))))
    (nreverse profiles)))

(defun +python-toolchain--conda-profiles ()
  "Return profiles reported by the active Conda installation."
  (when-let* ((conda (+language-server-executable-find "conda"))
              (data (+python-toolchain--command-json conda "env" "list" "--json"))
              (environments (alist-get "envs" data nil nil #'string=)))
    (let* ((info (+python-toolchain--command-json conda "info" "--json"))
           (base (and info (alist-get "root_prefix" info nil nil #'string=)))
           profiles)
      (dolist (environment environments (nreverse profiles))
        (let* ((name (file-name-nondirectory (directory-file-name environment)))
               (base-p (and base (string= environment base)))
               (python (expand-file-name "bin/python" environment)))
          (when-let* ((profile
                       (+python-toolchain--profile
                        (format "conda:%s" (if base-p "base" name))
                        (format "Conda %s" (if base-p "base" name))
                        python
                        :kind 'conda
                        :env `(("CONDA_PREFIX" . ,environment)
                               ("CONDA_DEFAULT_ENV" . ,(if base-p "base" name))
                               ("VIRTUAL_ENV" . nil)))))
            (push profile profiles)))))))

(defun +python-toolchain--sage-profile (root)
  "Return a dynamically resolved Sage profile for ROOT."
  (when-let* ((sage (+language-server-executable-find "sage"))
              (code (concat
                     "import json, site, sys\n"
                     "try:\n import sage.version as sv; version = sv.version\n"
                     "except Exception:\n version = ''\n"
                     "paths = list(dict.fromkeys(site.getsitepackages() + [site.getusersitepackages()]))\n"
                     "print(json.dumps({'python': sys.executable, 'version': version, 'paths': paths}))"))
              (data (+python-toolchain--command-json sage "--python" "-c" code))
              (python (alist-get "python" data nil nil #'string=))
              (paths (alist-get "paths" data nil nil #'string=))
              (profile
               (+python-toolchain--profile
                'sage
                (format "SageMath%s"
                        (if-let* ((version (alist-get "version" data nil nil #'string=)))
                            (format " %s" version)
                          ""))
                python
                :kind 'sage
                :sage-executable sage
                :shell-program sage
                :shell-args '("--python" "-i")
                :env (lambda (_project-root)
                       (let* ((site-path (car paths))
                              (old (getenv "PYTHONPATH"))
                              (pythonpath
                               (string-join
                                (delete-dups
                                 (append paths
                                         (and old (split-string old path-separator t))))
                                path-separator)))
                         `(("SAGE_PYTHON" . ,python)
                           ("SAGE_SITE_PACKAGES" . ,site-path)
                           ("DOT_SAGE" . ,(expand-file-name ".sage" root))
                           ("PYTHONPATH" . ,pythonpath))))
                :workspace (+python-toolchain--workspace python paths))))
    profile))

(defun +python-toolchain--path-profiles ()
  "Return Python executables visible on PATH."
  (let (profiles)
    (dolist (program '("python3" "python") (nreverse profiles))
      (when-let* ((executable
                   (+language-server-executable-find program))
                  (profile
                   (+python-toolchain--profile
                    (format "python:%s" executable)
                    (format "PATH %s" program)
                    executable
                    :kind 'path
                    :default (null profiles))))
        (push profile profiles)))))

(defun +python-toolchain--unique-profiles (profiles)
  "Return PROFILES with one entry per executable, preserving priority."
  (let (seen result)
    (dolist (profile profiles (nreverse result))
      (let ((identity (+python-toolchain--identity
                       (plist-get profile :executable))))
        (unless (member identity seen)
          (push identity seen)
          (push profile result))))))

(defun +python-toolchain-discover-fast (root)
  "Discover startup Python profiles for ROOT without running Conda or Sage.
The full picker still enumerates those environments on explicit selection."
  (+python-toolchain--unique-profiles
   (append (+python-toolchain--project-venvs root)
           (+python-toolchain--path-profiles))))

(defun +python-toolchain-discover (root)
  "Discover Python and Sage toolchains for project ROOT."
  (+python-toolchain--unique-profiles
   (append (+python-toolchain--project-venvs root)
           (+python-toolchain--conda-profiles)
           (delq nil (list (+python-toolchain--sage-profile root)))
           (+python-toolchain--path-profiles))))

(defun +python-toolchain-apply (profile _root)
  "Apply Python-specific settings from PROFILE."
  (let ((program (or (plist-get profile :shell-program)
                     (plist-get profile :executable)))
        (arguments (plist-get profile :shell-args)))
    (when program
      (setq-local python-shell-interpreter program)
      (setq-local python-shell-interpreter-args
                  (mapconcat #'shell-quote-argument arguments " ")))))

(defun +python-toolchain-after-select (_profile _root)
  "Offer to restart an existing Python process after selecting a toolchain."
  (when (and (fboundp 'python-shell-get-process)
             (ignore-errors (python-shell-get-process)))
    (when (y-or-n-p "Restart the current Python REPL with this toolchain? ")
      (when-let* ((process (python-shell-get-process)))
        (delete-process process)))))

(defconst +python-language-server-contacts
  '(("pyright-langserver" "--stdio")
    ("basedpyright-langserver" "--stdio")
    ("pylsp")
    ("jedi-language-server"))
  "Python language servers in preference order.
Each entry is the executable name followed by its arguments.")

(defun +python-language-server-command ()
  "Return the first available Python language-server command on the target.

Every candidate is probed with `+language-server-executable-find', so the
choice reflects the workspace target's environment (venv, conda, Sage, or
target PATH) rather than whatever happens to be installed on the client."
  (let ((find-contact
         (lambda (contacts)
           (seq-some
            (lambda (contact)
              (when-let* ((executable
                           (ignore-errors
                             (+language-server-executable-find
                              (car contact)))))
                (cons executable (cdr contact))))
            contacts))))
    (or
     ;; Prefer the target's own Pyright/BasedPyright.  When neither exists,
     ;; a trusted target may use the versioned relocatable Pyright bundle.
     (funcall find-contact (seq-take +python-language-server-contacts 2))
     (ignore-errors (+lsp-python--provisioned-command))
     (funcall find-contact (seq-drop +python-language-server-contacts 2))
     ;; Nothing found: hand back the highest-preference name so lsp-mode
     ;; reports a missing binary against the server we actually want.
     (car +python-language-server-contacts))))

(defun +python-language-server-workspace-configuration ()
  "Return Python workspace configuration for the active server."
  '(:python (:analysis (:autoSearchPaths t
                        :useLibraryCodeForTypes t))
    :basedpyright (:analysis (:typeCheckingMode "basic"
                              :diagnosticMode "openFilesOnly"))
    :pyright (:analysis (:typeCheckingMode "basic"
                       :diagnosticMode "openFilesOnly"))
    :pylsp (:plugins (:jedi_completion (:fuzzy t)
                       :jedi_definition (:follow_imports t)
                       :jedi_hover (:enabled t)
                       :rope_autoimport (:enabled t)))))

(defun +python-language-server-setup-h ()
  "Install base Python workspace settings before the server starts."
  ;; Keep one target-aware Python route authoritative.  lsp-mode eagerly
  ;; requires every installed client package at first startup, so an installed
  ;; lsp-pyright would otherwise outrank `my-python' and bypass the shared
  ;; Remote command, environment and workspace-configuration contracts.
  (setq-local lsp-enabled-clients '(my-python))
  (when (fboundp '+language-server-set-workspace-configuration)
    (+language-server-set-workspace-configuration
     (+python-language-server-workspace-configuration))))

(add-hook 'python-mode-hook #'+python-language-server-setup-h)
(add-hook 'python-ts-mode-hook #'+python-language-server-setup-h)

(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    (set-language-server!
     '(python-mode python-ts-mode)
     #'+python-language-server-command
     :server-id 'my-python
     :priority 1
     :label "Target Python language server"
     :executables '("pyright-langserver"
                    "basedpyright-langserver"
                    "pylsp"
                    "jedi-language-server")
     :note "Select the first available server from the workspace target environment.")))

(set-language-server-toolchain-provider!
 'python
 '(python-mode python-ts-mode)
 #'+python-toolchain-discover
 :discover-fast #'+python-toolchain-discover-fast
 :apply #'+python-toolchain-apply
 :after-select #'+python-toolchain-after-select
 :label "Python / Sage")

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.sage\\'" . python-mode))
  :hook ((python-mode . +python/setup-imenu)
         (python-ts-mode . +python/setup-imenu))
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (defun +python-imenu-create-index ()
    "Return a Python imenu index with a safe fallback.

Prefer the mode-provided backend, but fall back to
`python-imenu-create-index' when the tree-sitter backend is unavailable or
fails.  This keeps `consult-imenu' and Treemacs tags working in
`python-ts-mode' buffers even when the Python grammar is missing."
    (or (and (functionp +python-imenu-backend)
             (ignore-errors
               (funcall +python-imenu-backend)))
        (python-imenu-create-index)))

  (defun +python/setup-imenu ()
    "Ensure Python buffers always expose a usable imenu index."
    (setq-local +python-imenu-backend
                (let ((backend imenu-create-index-function))
                  (if (and (functionp backend)
                           (not (eq backend #'+python-imenu-create-index)))
                      backend
                    #'python-imenu-create-index)))
    (setq-local imenu-create-index-function #'+python-imenu-create-index))

  (defun +python-refresh-open-buffers ()
    "Reapply Python buffer setup to already-open buffers.

This updates existing Python buffers when this module is re-evaluated, without
waiting for them to be reopened."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'python-mode 'python-ts-mode)
          (+python/setup-imenu)))))

  (defun +python-ensure-imenu-around (orig-fn &optional noerror)
    "Ensure Python buffers expose imenu before calling ORIG-FN.

This catches callers like Treemacs that may build imenu indices from temporary
buffers where normal mode hooks were skipped."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (+python/setup-imenu))
    (funcall orig-fn noerror))

  (with-eval-after-load 'imenu
    (advice-add 'imenu--make-index-alist :around #'+python-ensure-imenu-around))

  (+python-refresh-open-buffers))
(provide 'lang/python/config)
;;; lang/python/config.el ends here
