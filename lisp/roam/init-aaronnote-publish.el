;;; init-aaronnote-publish.el --- Noema site publish commands -*- lexical-binding: t; -*-
;;
;; Lazy-loaded: not required at startup.  Loaded on first publish command.
;; Both interactive commands and batch (make publish-*) entry points live here.

;;; Code:

(require 'config)

(require 'cl-lib)

;; Derive config root from this file's path so batch mode (-Q) gets the right
;; user-emacs-directory instead of the default ~/.emacs.d/.
(defconst my/noema-publish--config-root
  (if load-file-name
      (file-truename
       (expand-file-name "../../" (file-name-directory (file-truename load-file-name))))
    user-emacs-directory)
  "Emacs config root, derived from load-file-name for batch-mode correctness.")

(defgroup my/noema-publish nil
  "Noema static-site build and deployment."
  :group 'applications)

(config-defvar my/noema-publish-root nil
  "Path to the publish git repo root (output lands here directly)."
  :type 'directory
  :group 'my/noema-publish)

(config-defvar my/noema-publish-engine nil
  "Path to the Python publish engine."
  :type 'file
  :group 'my/noema-publish)

(config-defvar my/noema-publish-assets-dir nil
  "Path to publish source assets (css/, kinds/, homepage.html, etc.)."
  :type 'directory
  :group 'my/noema-publish)

(config-defvar my/noema-publish-state-dir nil
  "Path to publish cache/state directory (deps/, state.json, book/, cv/)."
  :type 'directory
  :group 'my/noema-publish)

(config-defvar my/noema-publish-cv-dir nil
  "Path to the CV LaTeX source directory."
  :type 'directory
  :group 'my/noema-publish)

(config-defvar my/noema-publish-nas-target nil
  "rsync target for NAS deployment."
  :type 'string
  :group 'my/noema-publish)

(config-defvar my/noema-publish-nas-enable nil
  "When non-nil, rsync to NAS after git push during deploy."
  :type 'boolean
  :group 'my/noema-publish)

(defconst my/noema-publish--log-buffer "*Noema Publish*"
  "Name of the publish log buffer.")

(defvar my/noema-publish--process nil
  "Current publish subprocess, or nil.")

;;; Internal helpers

(defun my/noema-publish--runtime-root ()
  "Return the Noema runtime root path."
  (expand-file-name "lisp/roam/Noema" my/noema-publish--config-root))

(defun my/noema-publish--roam-root ()
  "Return the canonical Noema workspace root."
  (if (fboundp 'my/noema-workspace-root)
      (my/noema-workspace-root)
    (expand-file-name "~/Documents/Noema")))

(defun my/noema-publish--env (&optional extra-env)
  "Return process-environment list for the publish engine subprocess.
EXTRA-ENV is an optional alist of (VAR . VALUE) pairs prepended as strings.
Uses string \"VAR=VALUE\" format so both call-process and make-process work."
  (let ((runtime (my/noema-publish--runtime-root)))
    (append
     (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) extra-env)
     (list (format "AARONNOTE_PUBLISH_OUTPUT=%s"    my/noema-publish-root)
           (format "AARONNOTE_RUNTIME_ROOT=%s"      runtime)
           (format "AARONNOTE_PUBLISH_ASSETS=%s"    my/noema-publish-assets-dir)
           (format "AARONNOTE_PUBLISH_STATE_DIR=%s" my/noema-publish-state-dir)
           (format "AARONNOTE_ROAM_ROOT=%s"         (my/noema-publish--roam-root)))
     process-environment)))

(defun my/noema-publish--log (msg)
  "Append MSG to the publish log buffer."
  (with-current-buffer (get-buffer-create my/noema-publish--log-buffer)
    (goto-char (point-max))
    (insert msg)))

(defun my/noema-publish--show-log ()
  "Show the publish log buffer without stealing focus."
  (let ((buf (get-buffer-create my/noema-publish--log-buffer)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-at-bottom . ((window-height . 0.25)))))))

(defun my/noema-publish--run (label args &optional sentinel)
  "Run ENGINE with ARGS in the publish repo, logging to the log buffer.
LABEL is shown in progress messages.  SENTINEL is called when the process exits."
  (when (and my/noema-publish--process
             (process-live-p my/noema-publish--process))
    (user-error "A publish process is already running; wait for it to finish"))
  (make-directory my/noema-publish-state-dir t)
  (with-current-buffer (get-buffer-create my/noema-publish--log-buffer)
    (goto-char (point-max))
    (insert (format "\n[%s] %s\n" (format-time-string "%H:%M:%S") label)))
  (my/noema-publish--show-log)
  (message "Noema publish: %s…" label)
  (let ((process-environment (my/noema-publish--env))
        (default-directory my/noema-publish-root))
    (setq my/noema-publish--process
          (make-process
           :name "aaronnote-publish"
           :buffer my/noema-publish--log-buffer
           :command args
           :sentinel
           (lambda (proc event)
             (let ((ok (string-match-p "finished" event))
                   (exit-code (process-exit-status proc)))
               (if ok
                   (message "Noema publish: %s done." label)
                 (message "Noema publish: %s FAILED (exit %s). See %s"
                          label exit-code my/noema-publish--log-buffer))
               (setq my/noema-publish--process nil)
               (when sentinel (funcall sentinel ok exit-code))))))))

(defun my/noema-publish--run-sync (label args &optional extra-env)
  "Run ARGS synchronously (for batch/make use), printing output to stdout.
EXTRA-ENV is an alist of additional env vars.  Signals error on non-zero exit."
  (make-directory my/noema-publish-state-dir t)
  (princ (format "[publish] %s...\n" label))
  (let* ((process-environment (my/noema-publish--env extra-env))
         (default-directory my/noema-publish-root)
         (out-buf (generate-new-buffer " *Noema publish output*"))
         exit-code)
    (unwind-protect
        (progn
          (setq exit-code (apply #'call-process (car args) nil out-buf nil (cdr args)))
          (let ((output (with-current-buffer out-buf (buffer-string))))
            (unless (string-empty-p output)
              (princ output)))
          (if (zerop exit-code)
              (princ (format "[publish] %s done.\n" label))
            (error "Noema publish: %s failed (exit %d)" label exit-code)))
      (kill-buffer out-buf))))

(defun my/noema-publish--prepare-git-commit (out-buf)
  "Stage publish output and commit it when it changed, logging to OUT-BUF.
Signal an error when staging, inspection, or committing fails."
  (let ((default-directory my/noema-publish-root))
    (unless (zerop (call-process "git" nil out-buf nil "add" "-A"))
      (error "git add failed in %s" my/noema-publish-root))
    (pcase (call-process "git" nil out-buf nil "diff" "--cached" "--quiet")
      (0 nil)
      (1
       (unless (zerop (call-process
                       "git" nil out-buf nil
                       "commit" "-m"
                       (format "site update: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (error "git commit failed in %s" my/noema-publish-root)))
      (status
       (error "git diff --cached failed (exit %d)" status)))))

(defun my/noema-publish--deploy-sync ()
  "Run git add+commit+push and optional NAS rsync, printing output to stdout."
  (let ((default-directory my/noema-publish-root)
        (out-buf (generate-new-buffer " *Noema deploy output*")))
    (unwind-protect
        (progn
          (princ (format "[publish] deploy: git add + commit (%s)...\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")))
          (my/noema-publish--prepare-git-commit out-buf)
          (princ (with-current-buffer out-buf (buffer-string)))
          (with-current-buffer out-buf (erase-buffer))
          (princ "[publish] git push...\n")
          (let ((exit (call-process "git" nil out-buf nil "push")))
            (princ (with-current-buffer out-buf (buffer-string)))
            (unless (zerop exit)
              (error "git push failed (exit %d)" exit)))
          (when my/noema-publish-nas-enable
            (with-current-buffer out-buf (erase-buffer))
            (princ (format "[publish] rsync → %s...\n" my/noema-publish-nas-target))
            (let ((exit (call-process "rsync" nil out-buf nil
                                      "-avh" "--delete"
                                      "--exclude" ".deps/"
                                      "--exclude" "state.json"
                                      "--exclude" ".DS_Store"
                                      "--progress" "-e" "ssh"
                                      (file-name-as-directory my/noema-publish-root)
                                      my/noema-publish-nas-target)))
              (princ (with-current-buffer out-buf (buffer-string)))
              (unless (zerop exit)
                (error "rsync NAS failed (exit %d)" exit)))))
      (kill-buffer out-buf))))

(defun my/noema-publish--cv-build-sync ()
  "Compile the LaTeX CV synchronously; copy PDF to publish root."
  (let* ((cv-state (expand-file-name "cv" my/noema-publish-state-dir))
         (jobname "Aaron_He_CV")
         (pdf-in (expand-file-name (concat jobname ".pdf") cv-state))
         (pdf-out (expand-file-name (concat "CV/" jobname ".pdf") my/noema-publish-root)))
    (make-directory cv-state t)
    (make-directory (expand-file-name "CV" my/noema-publish-root) t)
    (princ "[publish] compiling CV...\n")
    (let ((exit-code (call-process
                      "latexmk" nil (get-buffer-create my/noema-publish--log-buffer) nil
                      "-xelatex" "-interaction=nonstopmode" "-halt-on-error"
                      (concat "-outdir=" cv-state)
                      (concat "-jobname=" jobname)
                      (expand-file-name "main.tex" my/noema-publish-cv-dir))))
      (if (zerop exit-code)
          (if (file-exists-p pdf-in)
              (progn
                (copy-file pdf-in pdf-out t)
                (princ (format "[publish] CV compiled → %s\n" pdf-out)))
            (error "CV compiler succeeded but produced no PDF: %s" pdf-in))
        (error "CV compilation failed (exit %d)" exit-code)))))

;;; Public interactive commands

;;;###autoload
(defun my/noema-publish-build ()
  "Build the static site (render notes + copy assets + compile CV)."
  (interactive)
  (my/noema-publish--cv-build-sync)
  (my/noema-publish--run
   "build"
   (list "python3" my/noema-publish-engine)))

;;;###autoload
(defun my/noema-publish-deploy ()
  "Deploy: git commit+push in publish repo, optionally rsync to NAS."
  (interactive)
  (let ((default-directory my/noema-publish-root))
    (my/noema-publish--log
     (format "\n[%s] deploy: git add + commit + push\n" (format-time-string "%H:%M:%S")))
    (my/noema-publish--show-log)
    (message "Noema publish: committing…")
    (my/noema-publish--prepare-git-commit
     (get-buffer-create my/noema-publish--log-buffer))
    (my/noema-publish--run
     "git push"
     (list "git" "-C" my/noema-publish-root "push")
     (when my/noema-publish-nas-enable
       (lambda (ok _code)
         (when ok
           (my/noema-publish--run
            "rsync NAS"
            (list "rsync" "-avh" "--delete"
                  "--exclude" ".deps/"
                  "--exclude" "state.json"
                  "--exclude" ".DS_Store"
                  "--progress" "-e" "ssh"
                  (file-name-as-directory my/noema-publish-root)
                  my/noema-publish-nas-target))))))))

;;;###autoload
(defun my/noema-publish ()
  "Build and deploy the static site."
  (interactive)
  (my/noema-publish--cv-build-sync)
  (my/noema-publish--run
   "build"
   (list "python3" my/noema-publish-engine)
   (lambda (ok _code)
     (when ok
       (my/noema-publish-deploy)))))

;;;###autoload
(defun my/noema-publish-clean ()
  "Remove publish state/cache and generated CV intermediates."
  (interactive)
  (when my/noema-publish--process
    (when (process-live-p my/noema-publish--process)
      (kill-process my/noema-publish--process))
    (setq my/noema-publish--process nil))
  (dolist (name '("deps" "tmp" "book" "cv"))
    (let ((path (expand-file-name name my/noema-publish-state-dir)))
      (when (file-directory-p path)
        (delete-directory path t))))
  (dolist (name '("state.json" ".publish-state.json"))
    (let ((path (expand-file-name name my/noema-publish-state-dir)))
      (when (file-exists-p path)
        (delete-file path))))
  (when (buffer-live-p (get-buffer my/noema-publish--log-buffer))
    (kill-buffer my/noema-publish--log-buffer))
  (message "Noema publish: cleaned state/cache."))

;;; Batch entry points (used by make publish-*)

(defun my/noema-publish-batch ()
  "Batch entry: build + deploy.  Calls `kill-emacs' with non-zero on error."
  (condition-case err
      (progn
        (my/noema-publish--cv-build-sync)
        (my/noema-publish--run-sync
         "build" (list "python3" my/noema-publish-engine))
        (my/noema-publish--deploy-sync))
    (error
     (princ (format "Noema publish FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/noema-publish-force-batch ()
  "Batch entry: force build + deploy, skipping incremental state check."
  (condition-case err
      (progn
        (my/noema-publish--cv-build-sync)
        (my/noema-publish--run-sync
         "build (forced)" (list "python3" my/noema-publish-engine)
         (list (cons "PUBLISH_FORCE" "1")))
        (my/noema-publish--deploy-sync))
    (error
     (princ (format "Noema publish-force FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/noema-publish-build-batch ()
  "Batch entry: build only (no deploy)."
  (condition-case err
      (progn
        (my/noema-publish--cv-build-sync)
        (my/noema-publish--run-sync
         "build" (list "python3" my/noema-publish-engine)))
    (error
     (princ (format "Noema publish-build FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/noema-publish-deploy-batch ()
  "Batch entry: deploy only (git push + optional NAS rsync)."
  (condition-case err
      (my/noema-publish--deploy-sync)
    (error
     (princ (format "Noema publish-deploy FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/noema-publish-clean-batch ()
  "Batch entry: clean publish state."
  (my/noema-publish-clean))

(provide 'init-aaronnote-publish)
;;; init-aaronnote-publish.el ends here
