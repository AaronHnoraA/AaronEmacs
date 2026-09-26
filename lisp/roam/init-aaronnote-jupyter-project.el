;;; init-aaronnote-jupyter-project.el --- Project tools for kernels -*- lexical-binding: t; -*-

;;; Commentary:
;; A view of the existing kernelspec, not another SSH/profile registry.
;; Paths belong to Remote's existing file-name handler (TRAMP underneath).
;; Navigation never uploads a local notebook or changes its file identity.

;;; Code:
(require 'cl-lib)
(require 'init-aaronnote-jupyter-cell)
(require 'init-jupyter-management)
(require 'init-lsp-runtime)
(require 'remote-fs)
(require 'remote-environment)
(require 'direnv)
(require 'shell)

(declare-function my/noema-jupyter-cell--lsp-local-kernelspec "init-aaronnote-jupyter-lsp" (kernel &optional source))
(declare-function my/language-server-ensure "init-lsp" ())
(declare-function my/noema-jupyter-cell-lsp-runtime-changing "init-aaronnote-jupyter-lsp" ())

(cl-defstruct (my/noema-jupyter-project
               (:constructor my/noema-jupyter-project-create))
  target root interpreter kernel entry lsp-settings)

(defun my/noema-jupyter-project-metadata (spec)
  "Return the optional metadata.aaron.project object in SPEC."
  (my/jupyter-management-get
   'project (my/jupyter-management-get 'aaron (my/jupyter-management-get 'metadata spec))))

(defvar my/noema-jupyter-project--bindings (make-hash-table :test #'equal)
  "Session associations from canonical project roots to kernelspec entries.
Entries reference their original kernel.json, not another saved profile.")

(defvar-local my/noema-jupyter-project--shell-pending nil)

(defun my/noema-jupyter-project-config (spec)
  "Read SPEC's effective remote configuration, with argv taking precedence.
The launch argv is what Jupyter actually executes.  Metadata is a fallback
for older profiles and editor hints, never an independent execution truth."
  (let* ((get #'my/jupyter-management-get)
         (config (copy-tree (funcall get 'config (my/jupyter-management-remote-metadata spec))))
         (argv (append (funcall get 'argv spec) nil)))
    (when (or config (member "remote_ikernel" argv))
      (dolist (key '(interface host workdir kernel_cmd))
        (when-let* ((value (my/jupyter-management-argv-option
                           argv (concat "--" (symbol-name key)))))
          (setf (alist-get key config) value)))
      config)))

(defun my/noema-jupyter-project-resolve-target (host)
  "Resolve HOST using the existing Remote registry, without parsing SSH config."
  (when (and (stringp host) (not (string-empty-p host)))
    (or (ignore-errors (remote-get-target host))
        (seq-find (lambda (target) (equal host (remote-target-label target)))
                  (hash-table-values remote-targets)))))

(defun my/noema-jupyter-project--fresh-entry (entry &optional source)
  "Read ENTRY's original kernelspec, on SOURCE's target when supplied."
  (if-let* ((directory (my/jupyter-management-get 'resourceDir entry)))
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
        `((name . ,(my/jupyter-management-get 'name entry))
          (resourceDir . ,directory) (spec . ,spec)))
    entry))

(defun my/noema-jupyter-project-from-entry (entry source)
  "Resolve configured project information in ENTRY for notebook SOURCE.
Never infer configured directories from a live kernel's mutable cwd."
  (let* ((spec (my/jupyter-management-get 'spec entry))
         (config (my/noema-jupyter-project-config spec))
         (project (my/noema-jupyter-project-metadata spec))
         (source-context (remote-context source))
         (host (my/jupyter-management-get 'host config))
         (target (if-let* ((id (my/jupyter-management-get 'target project)))
                     (my/noema-jupyter-project-resolve-target id)
                   (if config (my/noema-jupyter-project-resolve-target host)
                   (remote-get-target (remote-context-target-id source-context)))))
         (root (or (my/jupyter-management-get 'root project)
                   (if config (my/jupyter-management-get 'workdir config)
                 (or (remote-context-workspace-root source-context)
                     (file-name-directory source)))))
         (command (my/jupyter-management-get 'kernel_cmd config))
         (interpreter (or (my/jupyter-management-get 'python project)
                          (if command (car (split-string-and-unquote command))
                            (car (append (my/jupyter-management-get 'argv spec) nil))))))
    (when (and config (not (equal "ssh" (my/jupyter-management-get 'interface config))))
      (user-error "This kernel launcher has no SSH project filesystem"))
    (unless target (user-error "Kernel host %s is not registered in Remote" host))
    (when (and config (my/noema-jupyter-project-resolve-target host)
               (not (equal (remote-target-id target)
                           (remote-target-id (my/noema-jupyter-project-resolve-target host)))))
      (user-error "Project target and kernel SSH host disagree; correct kernel.json"))
    (unless (and (stringp root) (file-name-absolute-p root))
      (user-error "Set an absolute workdir in this kernel profile before opening project tools"))
    (setq root (file-name-as-directory
                (if (or config project) (remote-target-file-name target root)
                  (remote-canonicalize-file-name root))))
    (when (and interpreter (not (file-name-absolute-p interpreter))
               (string-match-p "/" interpreter))
      (setq interpreter (file-local-name (expand-file-name interpreter root))))
    (my/noema-jupyter-project-create
     :target (remote-target-id target) :root root :interpreter interpreter
     :kernel (my/jupyter-management-get 'name entry) :entry entry
     :lsp-settings (my/jupyter-management-get 'lsp project))))

(defun my/noema-jupyter-project-current ()
  "Resolve the current notebook's configured execution project."
  (let* ((source (or my/noema-jupyter-cell-source-file buffer-file-name))
         (entry (or (and (equal my/noema-jupyter-cell-kernel
                                (my/jupyter-management-get 'name my/noema-jupyter-cell-kernel-spec))
                         my/noema-jupyter-cell-kernel-spec)
                    (and source (my/noema-jupyter-cell--lsp-local-kernelspec
                                 my/noema-jupyter-cell-kernel source))
                    (my/noema-jupyter-project-entry-for-source source))))
    (unless entry (user-error "No resolved kernel profile; select a kernel first"))
    (my/noema-jupyter-project-from-entry
     (my/noema-jupyter-project--fresh-entry
      entry (when (eq entry my/noema-jupyter-cell-kernel-spec) source)) source)))

(defun my/noema-jupyter-project-entry-for-source (source)
  "Return the explicitly associated kernelspec for remote SOURCE, if any."
  (when source
    (let ((canonical (remote-canonicalize-file-name source)) matches)
      (maphash (lambda (root entry)
                 (when (string-prefix-p root canonical)
                   (push (cons root entry) matches)))
               my/noema-jupyter-project--bindings)
      (when matches
        (setq-local my/language-server-runtime-required t)
        (my/noema-jupyter-project--fresh-entry
         (cdr (car (sort matches (lambda (a b) (> (length (car a)) (length (car b))))))))))))

(defun my/noema-jupyter-project--directory (project)
  "Check PROJECT's directory and associate source files with its existing profile."
  (let ((root (my/noema-jupyter-project-root project)))
    (unless (file-directory-p root)
      (user-error "Project directory is missing or inaccessible: %s" root))
    (puthash root (my/noema-jupyter-project-entry project) my/noema-jupyter-project--bindings)
    root))

(defun my/noema-jupyter-open-project-directory ()
  "Open the kernel's configured project root in ordinary Dired."
  (interactive)
  (dired (my/noema-jupyter-project--directory (my/noema-jupyter-project-current))))

(defun my/noema-jupyter-open-project-file ()
  "Visit an actual project file, using normal remote file completion."
  (interactive)
  (let ((default-directory (my/noema-jupyter-project--directory (my/noema-jupyter-project-current))))
    (find-file (read-file-name "Project file: " default-directory nil t))))

(defun my/noema-jupyter-open-project-shell ()
  "Open a standard Emacs shell at the configured project root.
Remote's file-name handler and comint start the process.  No raw SSH command
or terminal-specific transport is constructed here."
  (interactive)
  (let* ((project (my/noema-jupyter-project-current))
         (default-directory (my/noema-jupyter-project--directory project))
         (name (format "*Project shell %s:%s*" (my/noema-jupyter-project-target project)
                       (file-local-name default-directory)))
         (buffer (get-buffer-create name)))
    (pop-to-buffer buffer)
    (unless (or (comint-check-proc buffer) my/noema-jupyter-project--shell-pending)
      (setq default-directory (my/noema-jupyter-project-root project)
            my/noema-jupyter-project--shell-pending t)
      (let* ((context (remote-context default-directory))
             (spec (my/jupyter-management-get 'spec (my/noema-jupyter-project-entry project)))
             (metadata (my/noema-jupyter-project-metadata spec))
             (start
              (lambda (_environment error)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq my/noema-jupyter-project--shell-pending nil)
                    (if error
                        (progn
                          (insert (format "Project environment failed: %s\n" (error-message-string error)))
                          (message "Project shell not started: %s" (error-message-string error)))
                      (let* ((python (my/noema-jupyter-project-interpreter project))
                             (environment
                              (remote-environment-derive
                               (remote-environment-resolve context)
                               "jupyter-shell" :scope 'invocation
                               :vars (my/jupyter-management-get 'env spec)
                               :path-prepend (when (and python (file-name-absolute-p python))
                                               (list (file-name-directory python)))
                               :source 'jupyter-project)))
                        (remote-environment-apply environment)
                        (shell buffer
                               (or (remote-target-shell (remote-get-target (my/noema-jupyter-project-target project)))
                                   (if (equal "local" (my/noema-jupyter-project-target project))
                                       shell-file-name "/bin/sh"))))))))))
        (if (or (eq t (my/jupyter-management-get 'direnv metadata))
                (bound-and-true-p my/enable-direnv))
            (when (eq 'ready (direnv-environment-ensure-async default-directory start))
              (funcall start nil nil))
          (funcall start nil nil))))
    buffer))

(defun my/noema-jupyter-project-lsp ()
  "Refresh LSP for a project source, or open remote source from a local notebook."
  (interactive)
  (let* ((project (my/noema-jupyter-project-current))
         (root (my/noema-jupyter-project--directory project))
         (source (remote-canonicalize-file-name buffer-file-name)))
    (unless (string-prefix-p root source)
      (let ((default-directory root))
        (find-file (read-file-name "Open project source for remote LSP: " root nil t))))
    (unless (string-prefix-p root (remote-canonicalize-file-name buffer-file-name))
      (user-error "Select a source file inside %s" root))
    (my/noema-jupyter-cell-lsp-runtime-changing)
    (my/language-server-ensure)))

(defun my/noema-jupyter-project-inspect ()
  "Show configured project information separately from a fresh Python probe.
The probe starts a separate process and changes no configuration."
  (interactive)
  (let* ((project (my/noema-jupyter-project-current))
         (source buffer-file-name)
         (default-directory (my/noema-jupyter-project--directory project))
         (python (my/noema-jupyter-project-interpreter project))
         (report (with-temp-buffer
                   (if (not python) "No configured interpreter"
                     (condition-case err
                         (let ((status (process-file python nil t nil "-c"
                                        "import json,os,sys,socket; print(json.dumps(dict(host=socket.gethostname(),cwd=os.getcwd(),executable=sys.executable,version=sys.version),indent=2))")))
                           (format "Exit %s\n%s" status (buffer-string)))
                       (error (error-message-string err)))))))
    (with-help-window "*Jupyter Project*"
      (princ (format "Notebook: %s\nConfigured target: %s\nConfigured root: %s\nConfigured interpreter: %s\nKernel profile: %s\n\nProject interpreter probe (not the running kernel):\n%s\n\nFiles and shell use the configured root. Local notebooks are not mirrored.\n"
                     source (my/noema-jupyter-project-target project)
                     (my/noema-jupyter-project-root project) python
                     (my/noema-jupyter-project-kernel project) report)))))

(keymap-set my/noema-jupyter-cell-mode-map "C-c i p d" #'my/noema-jupyter-open-project-directory)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i p f" #'my/noema-jupyter-open-project-file)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i p s" #'my/noema-jupyter-open-project-shell)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i p l" #'my/noema-jupyter-project-lsp)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i p ?" #'my/noema-jupyter-project-inspect)
(provide 'init-aaronnote-jupyter-project)
;;; init-aaronnote-jupyter-project.el ends here
