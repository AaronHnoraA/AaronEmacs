;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/eglot-set-workspace-configuration "init-lsp" (configuration))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))
(declare-function my/register-language-server-toolchain-provider "init-lsp-toolchain"
                  (family modes discover &rest properties))
(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-expand-file-name "remote-fs"
                  (file-name &optional directory target))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function eglot-alternatives "eglot" (alternatives))
(defvar imenu-create-index-function)
(defvar-local my/python-imenu-backend nil
  "Original Python imenu backend for the current buffer.")

(defun my/python-toolchain--command-json (program &rest arguments)
  "Run PROGRAM with ARGUMENTS and return the last JSON object it prints."
  (when (and program
             (file-executable-p
              (my/python-toolchain--logical-executable program)))
    (with-temp-buffer
      (when (zerop (apply #'process-file program nil t nil arguments))
        (goto-char (point-max))
        (when (re-search-backward "^[[:space:]]*{" nil t)
          (condition-case nil
              (json-parse-buffer :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil)
            (error nil)))))))

(defun my/python-toolchain--logical-executable (path)
  "Project target-native executable PATH into the current logical target."
  (if (and (stringp path)
           (file-name-absolute-p path))
      (remote-expand-file-name path nil (remote-context))
    path))

(defun my/python-toolchain--canonical-executable (path)
  "Return a canonical executable PATH, or nil when it cannot be run."
  (when-let* ((logical (and (stringp path)
                            (my/python-toolchain--logical-executable path)))
              ((file-executable-p logical))
              (canonical
               (or (ignore-errors (file-truename logical))
                   (expand-file-name logical))))
    (remote-file-local-name canonical)))

(defun my/python-toolchain--workspace (executable &optional extra-paths)
  "Build Eglot workspace settings for EXECUTABLE and EXTRA-PATHS."
  (let ((paths (vconcat (delq nil extra-paths))))
    `(:python (:pythonPath ,executable
               :analysis (:autoSearchPaths t
                          :useLibraryCodeForTypes t
                          ,@(when (> (length paths) 0)
                              `(:extraPaths ,paths))))
      :pyright (:pythonPath ,executable)
      :basedpyright (:pythonPath ,executable)
      :pylsp (:plugins (:jedi (:environment ,executable))))))

(defun my/python-toolchain--profile (id label executable &rest properties)
  "Create a Python profile ID and LABEL around EXECUTABLE and PROPERTIES."
  (when-let* ((executable (my/python-toolchain--canonical-executable executable)))
    (append
     (list :id id
           :label label
           :family 'python
           :executable executable
           :path-prepend (list (file-name-directory executable))
           :workspace (my/python-toolchain--workspace executable))
     properties)))

(defun my/python-toolchain--project-venvs (root)
  "Return virtual-environment profiles found below ROOT."
  (let (profiles)
    (dolist (name '(".venv" "venv"))
      (let* ((environment (expand-file-name name root))
             (python (expand-file-name "bin/python" environment)))
        (when-let* ((profile
                     (my/python-toolchain--profile
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

(defun my/python-toolchain--conda-profiles ()
  "Return profiles reported by the active Conda installation."
  (when-let* ((conda (my/language-server-executable-find "conda"))
              (data (my/python-toolchain--command-json conda "env" "list" "--json"))
              (environments (alist-get "envs" data nil nil #'string=)))
    (let* ((info (my/python-toolchain--command-json conda "info" "--json"))
           (base (and info (alist-get "root_prefix" info nil nil #'string=)))
           profiles)
      (dolist (environment environments (nreverse profiles))
        (let* ((name (file-name-nondirectory (directory-file-name environment)))
               (base-p (and base (string= environment base)))
               (python (expand-file-name "bin/python" environment)))
          (when-let* ((profile
                       (my/python-toolchain--profile
                        (format "conda:%s" (if base-p "base" name))
                        (format "Conda %s" (if base-p "base" name))
                        python
                        :kind 'conda
                        :env `(("CONDA_PREFIX" . ,environment)
                               ("CONDA_DEFAULT_ENV" . ,(if base-p "base" name))
                               ("VIRTUAL_ENV" . nil)))))
            (push profile profiles)))))))

(defun my/python-toolchain--sage-profile (root)
  "Return a dynamically resolved Sage profile for ROOT."
  (when-let* ((sage (my/language-server-executable-find "sage"))
              (code (concat
                     "import json, site, sys\n"
                     "try:\n import sage.version as sv; version = sv.version\n"
                     "except Exception:\n version = ''\n"
                     "paths = list(dict.fromkeys(site.getsitepackages() + [site.getusersitepackages()]))\n"
                     "print(json.dumps({'python': sys.executable, 'version': version, 'paths': paths}))"))
              (data (my/python-toolchain--command-json sage "--python" "-c" code))
              (python (alist-get "python" data nil nil #'string=))
              (paths (alist-get "paths" data nil nil #'string=))
              (profile
               (my/python-toolchain--profile
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
                :workspace (my/python-toolchain--workspace python paths))))
    profile))

(defun my/python-toolchain--path-profiles ()
  "Return Python executables visible on PATH."
  (let (profiles)
    (dolist (program '("python3" "python") (nreverse profiles))
      (when-let* ((executable
                   (my/language-server-executable-find program))
                  (profile
                   (my/python-toolchain--profile
                    (format "python:%s" executable)
                    (format "PATH %s" program)
                    executable
                    :kind 'path
                    :default (string= program "python3"))))
        (push profile profiles)))))

(defun my/python-toolchain-discover (root)
  "Discover Python and Sage toolchains for project ROOT."
  (let ((profiles (append (my/python-toolchain--project-venvs root)
                          (my/python-toolchain--conda-profiles)
                          (delq nil (list (my/python-toolchain--sage-profile root)))
                          (my/python-toolchain--path-profiles)))
        seen
        result)
    (dolist (profile profiles (nreverse result))
      (let ((executable (plist-get profile :executable)))
        (unless (member executable seen)
          (push executable seen)
          (push profile result))))))

(defun my/python-toolchain-apply (profile _root)
  "Apply Python-specific settings from PROFILE."
  (let ((program (or (plist-get profile :shell-program)
                     (plist-get profile :executable)))
        (arguments (plist-get profile :shell-args)))
    (when program
      (setq-local python-shell-interpreter program)
      (setq-local python-shell-interpreter-args
                  (mapconcat #'shell-quote-argument arguments " ")))))

(defun my/python-toolchain-after-select (_profile _root)
  "Offer to restart an existing Python process after selecting a toolchain."
  (when (and (fboundp 'python-shell-get-process)
             (ignore-errors (python-shell-get-process)))
    (when (y-or-n-p "Restart the current Python REPL with this toolchain? ")
      (when-let* ((process (python-shell-get-process)))
        (delete-process process)))))

(defun my/python-eglot-contact (&optional interactive project)
  "Return the first available Python Eglot contact on the active target."
  (let ((contacts
         '(("pyright-langserver" "--stdio")
           ("basedpyright-langserver" "--stdio")
           ("pylsp")
           ("jedi-language-server"))))
    (or
     (seq-some
      (lambda (contact)
        (when-let* ((executable
                     (ignore-errors
                       (my/language-server-executable-find
                        (car contact)))))
          (cons executable (cdr contact))))
      contacts)
     (progn
       (require 'eglot)
       (funcall
        (eglot-alternatives contacts)
        interactive
        project)))))

(defun my/python-eglot-workspace-configuration ()
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

(defun my/python-eglot-ensure ()
  "Start Eglot for Python buffers."
  (my/eglot-set-workspace-configuration
   (my/python-eglot-workspace-configuration))
  (my/eglot-ensure-unless-lsp-mode))

(use-package eglot
  :ensure nil
  :hook ((python-mode . my/python-eglot-ensure)
         (python-ts-mode . my/python-eglot-ensure)))

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(python-mode python-ts-mode)
     #'my/python-eglot-contact
     :label "Target Python language server"
     :executables '("pyright-langserver"
                    "basedpyright-langserver"
                    "pylsp"
                    "jedi-language-server")
     :note "Select the first available server from the workspace target environment.")))

(my/register-language-server-toolchain-provider
 'python
 '(python-mode python-ts-mode)
 #'my/python-toolchain-discover
 :apply #'my/python-toolchain-apply
 :after-select #'my/python-toolchain-after-select
 :label "Python / Sage")

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.sage\\'" . python-mode))
  :hook ((python-mode . my/python-setup-imenu)
         (python-ts-mode . my/python-setup-imenu))
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (defun my/python-imenu-create-index ()
    "Return a Python imenu index with a safe fallback.

Prefer the mode-provided backend, but fall back to
`python-imenu-create-index' when the tree-sitter backend is unavailable or
fails.  This keeps `consult-imenu' and Treemacs tags working in
`python-ts-mode' buffers even when the Python grammar is missing."
    (or (and (functionp my/python-imenu-backend)
             (ignore-errors
               (funcall my/python-imenu-backend)))
        (python-imenu-create-index)))

  (defun my/python-setup-imenu ()
    "Ensure Python buffers always expose a usable imenu index."
    (setq-local my/python-imenu-backend
                (let ((backend imenu-create-index-function))
                  (if (and (functionp backend)
                           (not (eq backend #'my/python-imenu-create-index)))
                      backend
                    #'python-imenu-create-index)))
    (setq-local imenu-create-index-function #'my/python-imenu-create-index))

  (defun my/python-refresh-open-buffers ()
    "Reapply Python buffer setup to already-open buffers.

This makes `my/reload-init' immediately fix existing Python buffers instead of
waiting for them to be reopened."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'python-mode 'python-ts-mode)
          (my/python-setup-imenu)))))

  (defun my/python-ensure-imenu-around (orig-fn &optional noerror)
    "Ensure Python buffers expose imenu before calling ORIG-FN.

This catches callers like Treemacs that may build imenu indices from temporary
buffers where normal mode hooks were skipped."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (my/python-setup-imenu))
    (funcall orig-fn noerror))

  (with-eval-after-load 'imenu
    (advice-add 'imenu--make-index-alist :around #'my/python-ensure-imenu-around))

  (my/python-refresh-open-buffers))
(provide 'init-python)
;;; init-python.el ends here
