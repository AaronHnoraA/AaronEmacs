;;; noema/jupyter/project.el --- Project tools for kernels -*- lexical-binding: t; -*-

;;; Commentary:
;; A view of the existing kernelspec, not another SSH/profile registry.
;; Paths belong to Remote's existing file-name handler (TRAMP underneath).
;; Navigation never uploads a local notebook or changes its file identity.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'noema/jupyter/cell)
(require 'tools/jupyter/management)
(require 'tools/lsp/runtime)
(require 'remote-fs)
(require 'remote-environment)
(require 'tools/direnv/direnv)
(require 'shell)

(declare-function +language-server/ensure "tools/lsp/config" ())
(declare-function +noema-jupyter--context "noema/jupyter/runtime" (file))
(declare-function +jupyter/ssh-open-project "tools/project/config" (&optional project))

(cl-defstruct (+noema-jupyter-project
               (:constructor +noema-jupyter-project-create))
  target root interpreter kernel entry lsp-settings)

(defun +noema-jupyter-project-metadata (spec)
  "Return the optional metadata.aaron.project object in SPEC."
  (+jupyter-management-get
   'project (+jupyter-management-get 'aaron (+jupyter-management-get 'metadata spec))))

(defvar +noema-jupyter-project--bindings (make-hash-table :test #'equal)
  "Session associations from canonical project roots to kernelspec entries.
Entries reference their original kernel.json, not another saved profile.")

(defvar-local +noema-jupyter-project--shell-pending nil)

(defun +noema-jupyter-project-local-kernelspec (kernel &optional source)
  "Read KERNEL's client-side profile for project file and shell commands.
The profile lives in this machine's kernelspec directories, so SOURCE must be
on a target whose files the client can open directly.  That is a placement
question, not a target name.  LSP never uses this profile."
  (when (and (stringp kernel)
             (ignore-errors
               (remote-client-file-name
                (or source buffer-file-name default-directory))))
    (catch 'found
      (dolist (directory
               (delete-dups
                (list (expand-file-name "~/Library/Jupyter/kernels/")
                      (expand-file-name "~/.local/share/jupyter/kernels/")
                      (expand-file-name "~/.jupyter/kernels/")
                      (and (boundp '+noema-jupyter-kernelspec-directory)
                           +noema-jupyter-kernelspec-directory))))
        (when (and directory (file-directory-p directory))
          (let ((file (expand-file-name
                       "kernel.json" (expand-file-name kernel directory))))
            (when (file-readable-p file)
              (condition-case nil
                  (throw 'found
                         `((name . ,kernel)
                           (spec . ,(json-parse-string
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (buffer-string))
                                     :object-type 'alist :array-type 'list
                                     :null-object nil :false-object :json-false))
                           (resourceDir . ,(file-name-directory file))))
                (error nil)))))))))

(defun +noema-jupyter-project-config (spec)
  "Read SPEC's effective remote configuration, with argv taking precedence.
The launch argv is what Jupyter actually executes.  Metadata is a fallback
for older profiles and editor hints, never an independent execution truth."
  (let* ((get #'+jupyter-management-get)
         (config (copy-tree (funcall get 'config (+jupyter-management-remote-metadata spec))))
         (argv (append (funcall get 'argv spec) nil)))
    (when (or config (member "remote_ikernel" argv))
      (dolist (key '(interface host workdir kernel_cmd))
        (when-let* ((value (+jupyter-management-argv-option
                           argv (concat "--" (symbol-name key)))))
          (setf (alist-get key config) value)))
      config)))

(defun +noema-jupyter-project-resolve-target (host)
  "Resolve HOST using the existing Remote registry, without parsing SSH config."
  (when (and (stringp host) (not (string-empty-p host)))
    (or (ignore-errors (remote-get-target host))
        (seq-find (lambda (target) (equal host (remote-target-label target)))
                  (hash-table-values remote-targets)))))

(defun +noema-jupyter-project--fresh-entry (entry &optional source)
  "Read ENTRY's original kernelspec, on SOURCE's target when supplied."
  (if-let* ((directory (+jupyter-management-get 'resourceDir entry)))
      (let* ((directory (if (and source (not (file-remote-p directory))
                                       (not (remote-fs-file-name-p directory)))
                                  (remote-make-file-name (remote-file-name-target source) directory)
                                directory))
             (file (expand-file-name "kernel.json" directory))
             (spec (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-string (buffer-string) :object-type 'alist
                                        :array-type 'list :null-object nil
                                        :false-object :json-false))))
        `((name . ,(+jupyter-management-get 'name entry))
          (resourceDir . ,directory) (spec . ,spec)))
    entry))

(defun +noema-jupyter-project-from-entry (entry source)
  "Resolve configured project information in ENTRY for notebook SOURCE.
Never infer configured directories from a live kernel's mutable cwd."
  (let* ((spec (+jupyter-management-get 'spec entry))
         (config (+noema-jupyter-project-config spec))
         (project (+noema-jupyter-project-metadata spec))
         (source-context (+noema-jupyter--context source))
         (host (+jupyter-management-get 'host config))
         (target (if-let* ((id (+jupyter-management-get 'target project)))
                     (+noema-jupyter-project-resolve-target id)
                   (if config (+noema-jupyter-project-resolve-target host)
                   (remote-get-target (remote-context-target-id source-context)))))
         (root (or (+jupyter-management-get 'root project)
                   (if config (+jupyter-management-get 'workdir config)
                 (or (remote-context-workspace-root source-context)
                     (file-name-directory source)))))
         (command (+jupyter-management-get 'kernel_cmd config))
         (interpreter (or (+jupyter-management-get 'python project)
                          (if command (car (split-string-and-unquote command))
                            (car (append (+jupyter-management-get 'argv spec) nil))))))
    (when (and config (not (equal "ssh" (+jupyter-management-get 'interface config))))
      (user-error "This kernel launcher has no SSH project filesystem"))
    (unless target (user-error "Kernel host %s is not registered in Remote" host))
    (when (and config (+noema-jupyter-project-resolve-target host)
               (not (equal (remote-target-id target)
                           (remote-target-id (+noema-jupyter-project-resolve-target host)))))
      (user-error "Project target and kernel SSH host disagree; correct kernel.json"))
    (unless (and (stringp root) (file-name-absolute-p root))
      (user-error "Set an absolute workdir in this kernel profile before opening project tools"))
    (setq root (file-name-as-directory
                (if (or config project) (remote-target-file-name target root)
                  (remote-canonicalize-emacs-file-name root))))
    (when (and interpreter (not (file-name-absolute-p interpreter))
               (string-match-p "/" interpreter))
      (setq interpreter (file-local-name (expand-file-name interpreter root))))
    (+noema-jupyter-project-create
     :target (remote-target-id target) :root root :interpreter interpreter
     :kernel (+jupyter-management-get 'name entry) :entry entry
     :lsp-settings (+jupyter-management-get 'lsp project))))

(defun +noema-jupyter-project-current ()
  "Resolve the current notebook's configured execution project."
  (let* ((source (or +noema-jupyter-cell-source-file buffer-file-name))
         (entry (or (and (equal +noema-jupyter-cell-kernel
                                (+jupyter-management-get 'name +noema-jupyter-cell-kernel-spec))
                         +noema-jupyter-cell-kernel-spec)
                    (and source (+noema-jupyter-project-local-kernelspec
                                 +noema-jupyter-cell-kernel source))
                    (+noema-jupyter-project-entry-for-source source))))
    (unless entry (user-error "No resolved kernel profile; select a kernel first"))
    (+noema-jupyter-project-from-entry
     (+noema-jupyter-project--fresh-entry
      entry (when (eq entry +noema-jupyter-cell-kernel-spec) source)) source)))

(defun +noema-jupyter-project-entry-for-source (source)
  "Return the explicitly associated kernelspec for remote SOURCE, if any."
  (when source
    (let ((canonical (remote-canonicalize-emacs-file-name source)) matches)
      (maphash (lambda (root entry)
                 (when (string-prefix-p root canonical)
                   (push (cons root entry) matches)))
               +noema-jupyter-project--bindings)
      (when matches
        (setq-local +language-server-runtime-required t)
        (+noema-jupyter-project--fresh-entry
         (cdr (car (sort matches (lambda (a b) (> (length (car a)) (length (car b))))))))))))

(defun +noema-jupyter-project--directory (project)
  "Check PROJECT's directory and associate source files with its existing profile."
  (let ((root (+noema-jupyter-project-root project)))
    (unless (file-directory-p root)
      (user-error "Project directory is missing or inaccessible: %s" root))
    (puthash root (+noema-jupyter-project-entry project) +noema-jupyter-project--bindings)
    root))

(defun +noema-jupyter/open-project-directory ()
  "Open the kernel's configured project root in ordinary Dired."
  (interactive)
  (dired (+noema-jupyter-project--directory (+noema-jupyter-project-current))))

(defun +noema-jupyter/open-project-file ()
  "Visit an actual project file, using normal remote file completion."
  (interactive)
  (let ((default-directory (+noema-jupyter-project--directory (+noema-jupyter-project-current))))
    (find-file (read-file-name "Project file: " default-directory nil t))))

(defun +noema-jupyter/open-project-shell ()
  "Open a standard Emacs shell at the configured project root.
Remote's file-name handler and comint start the process.  No raw SSH command
or terminal-specific transport is constructed here."
  (interactive)
  (let* ((project (+noema-jupyter-project-current))
         (default-directory (+noema-jupyter-project--directory project))
         (name (format "*Project shell %s:%s*" (+noema-jupyter-project-target project)
                       (file-local-name default-directory)))
         (buffer (get-buffer-create name)))
    (pop-to-buffer buffer)
    (unless (or (comint-check-proc buffer) +noema-jupyter-project--shell-pending)
      (setq default-directory (+noema-jupyter-project-root project)
            +noema-jupyter-project--shell-pending t)
      (let* ((context (remote-context default-directory))
             (spec (+jupyter-management-get 'spec (+noema-jupyter-project-entry project)))
             (metadata (+noema-jupyter-project-metadata spec))
             (start
              (lambda (project-environment error)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq +noema-jupyter-project--shell-pending nil)
                    (if error
                        (progn
                          (insert (format "Project environment failed: %s\n" (error-message-string error)))
                          (message "Project shell not started: %s" (error-message-string error)))
                      (let* ((python (+noema-jupyter-project-interpreter project))
                             (environment
                              (remote-environment-derive
                               (or project-environment
                                   (remote-environment-resolve context))
                               "jupyter-shell" :scope 'invocation
                               :vars (+jupyter-management-get 'env spec)
                               :path-prepend (when (and python (file-name-absolute-p python))
                                               (list (file-name-directory python)))
                               :source 'jupyter-project)))
                        (remote-environment-apply environment)
                        (shell buffer
                               (or (remote-target-shell (remote-get-target (+noema-jupyter-project-target project)))
                                   (if (equal "local" (+noema-jupyter-project-target project))
                                       shell-file-name "/bin/sh"))))))))))
        (if (or (eq t (+jupyter-management-get 'direnv metadata))
                (bound-and-true-p +enable-direnv))
            (let ((environment (remote-environment-resolve context)))
              (if (and environment
                       (equal (nth 1 (direnv--environment-source environment))
                              default-directory))
                  (funcall start environment nil)
                (when (eq 'ready (direnv-environment-ensure-async default-directory start))
                  (funcall start (remote-environment-resolve context) nil))))
          (funcall start nil nil))))
    buffer))

(defun +noema-jupyter/project-lsp ()
  "Restart LSP for the current notebook's own filesystem environment."
  (interactive)
  (unless buffer-file-name
    (user-error "Visit a notebook or source file before starting LSP"))
  (when (and (fboundp 'lsp-workspaces)
             (ignore-errors (lsp-workspaces)))
    (lsp-disconnect))
  (when (fboundp '+language-server/runtime-invalidate)
    (+language-server/runtime-invalidate))
  (setq-local +lsp-mode--start-request nil
              +lsp-mode--waiting-for-direnv nil
              +language-server--waiting-for-runtime nil)
  (+language-server/ensure))

(defun +noema-jupyter/project-inspect ()
  "Show configured project information separately from a fresh Python probe.
The probe starts a separate process and changes no configuration."
  (interactive)
  (let* ((project (+noema-jupyter-project-current))
         (source buffer-file-name)
         (default-directory (+noema-jupyter-project--directory project))
         (python (+noema-jupyter-project-interpreter project))
         (report (with-temp-buffer
                   (if (not python) "No configured interpreter"
                     (condition-case err
                         (let ((status (process-file python nil t nil "-c"
                                        "import json,os,sys,socket; print(json.dumps(dict(host=socket.gethostname(),cwd=os.getcwd(),executable=sys.executable,version=sys.version),indent=2))")))
                           (format "Exit %s\n%s" status (buffer-string)))
                       (error (error-message-string err)))))))
    (with-help-window "*Jupyter Project*"
      (princ (format "Notebook: %s\nConfigured target: %s\nConfigured root: %s\nConfigured interpreter: %s\nKernel profile: %s\n\nProject interpreter probe (not the running kernel):\n%s\n\nFiles and shell use the configured root. Local notebooks are not mirrored.\n"
                     source (+noema-jupyter-project-target project)
                     (+noema-jupyter-project-root project) python
                     (+noema-jupyter-project-kernel project) report)))))

(keymap-set +noema-jupyter-cell-mode-map "C-c i p d" #'+noema-jupyter/open-project-directory)
(keymap-set +noema-jupyter-cell-mode-map "C-c i p f" #'+noema-jupyter/open-project-file)
(keymap-set +noema-jupyter-cell-mode-map "C-c i p s" #'+noema-jupyter/open-project-shell)
(keymap-set +noema-jupyter-cell-mode-map "C-c i p l" #'+noema-jupyter/project-lsp)
(keymap-set +noema-jupyter-cell-mode-map "C-c i p ?" #'+noema-jupyter/project-inspect)
(keymap-set +noema-jupyter-cell-mode-map "C-c i p o" #'+jupyter/ssh-open-project)
(provide 'noema/jupyter/project)
;;; noema/jupyter/project.el ends here
