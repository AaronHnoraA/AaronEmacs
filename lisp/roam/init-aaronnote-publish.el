;;; init-aaronnote-publish.el --- Noema site publish commands -*- lexical-binding: t; -*-
;;
;; Lazy-loaded: not required at startup.  Loaded on first publish command.
;; Both interactive commands and batch (make publish-*) entry points live here.

;;; Code:

(require 'config)

(require 'cl-lib)

;; Derive config root from this file's path so batch mode (-Q) gets the right
;; user-emacs-directory instead of the default ~/.emacs.d/.
(defconst my/aaronnote-publish--config-root
  (if load-file-name
      (file-truename
       (expand-file-name "../../" (file-name-directory (file-truename load-file-name))))
    user-emacs-directory)
  "Emacs config root, derived from load-file-name for batch-mode correctness.")

(defgroup my/aaronnote-publish nil
  "Noema static-site build and deployment."
  :group 'applications)

(config-defvar my/aaronnote-publish-root nil
  "Path to the publish git repo root (output lands here directly)."
  :type 'directory
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-engine nil
  "Path to the Python publish engine."
  :type 'file
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-assets-dir nil
  "Path to publish source assets (css/, kinds/, homepage.html, etc.)."
  :type 'directory
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-state-dir nil
  "Path to publish cache/state directory (deps/, state.json, book/, cv/)."
  :type 'directory
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-cv-dir nil
  "Path to the CV LaTeX source directory."
  :type 'directory
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-nas-target nil
  "rsync target for NAS deployment."
  :type 'string
  :group 'my/aaronnote-publish)

(config-defvar my/aaronnote-publish-nas-enable nil
  "When non-nil, rsync to NAS after git push during deploy."
  :type 'boolean
  :group 'my/aaronnote-publish)

(defconst my/aaronnote-publish--log-buffer "*Noema Publish*"
  "Name of the publish log buffer.")

(defvar my/aaronnote-publish--process nil
  "Current publish subprocess, or nil.")

;;; Internal helpers

(defun my/aaronnote-publish--runtime-root ()
  "Return the Noema runtime root path."
  (expand-file-name "lisp/roam/Noema" my/aaronnote-publish--config-root))

(defun my/aaronnote-publish--roam-root ()
  "Return the notes vault root."
  (expand-file-name ".roam" my/aaronnote-publish--config-root))

(defun my/aaronnote-publish--env (&optional extra-env)
  "Return process-environment list for the publish engine subprocess.
EXTRA-ENV is an optional alist of (VAR . VALUE) pairs prepended as strings.
Uses string \"VAR=VALUE\" format so both call-process and make-process work."
  (let ((runtime (my/aaronnote-publish--runtime-root)))
    (append
     (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) extra-env)
     (list (format "AARONNOTE_PUBLISH_OUTPUT=%s"    my/aaronnote-publish-root)
           (format "AARONNOTE_RUNTIME_ROOT=%s"      runtime)
           (format "AARONNOTE_PUBLISH_ASSETS=%s"    my/aaronnote-publish-assets-dir)
           (format "AARONNOTE_PUBLISH_STATE_DIR=%s" my/aaronnote-publish-state-dir)
           (format "AARONNOTE_ROAM_ROOT=%s"         (my/aaronnote-publish--roam-root)))
     process-environment)))

(defun my/aaronnote-publish--log (msg)
  "Append MSG to the publish log buffer."
  (with-current-buffer (get-buffer-create my/aaronnote-publish--log-buffer)
    (goto-char (point-max))
    (insert msg)))

(defun my/aaronnote-publish--show-log ()
  "Show the publish log buffer without stealing focus."
  (let ((buf (get-buffer-create my/aaronnote-publish--log-buffer)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-at-bottom . ((window-height . 0.25)))))))

(defun my/aaronnote-publish--run (label args &optional sentinel)
  "Run ENGINE with ARGS in the publish repo, logging to the log buffer.
LABEL is shown in progress messages.  SENTINEL is called when the process exits."
  (when (and my/aaronnote-publish--process
             (process-live-p my/aaronnote-publish--process))
    (user-error "A publish process is already running; wait for it to finish"))
  (make-directory my/aaronnote-publish-state-dir t)
  (with-current-buffer (get-buffer-create my/aaronnote-publish--log-buffer)
    (goto-char (point-max))
    (insert (format "\n[%s] %s\n" (format-time-string "%H:%M:%S") label)))
  (my/aaronnote-publish--show-log)
  (message "Noema publish: %s…" label)
  (let ((process-environment (my/aaronnote-publish--env))
        (default-directory my/aaronnote-publish-root))
    (setq my/aaronnote-publish--process
          (make-process
           :name "aaronnote-publish"
           :buffer my/aaronnote-publish--log-buffer
           :command args
           :sentinel
           (lambda (proc event)
             (let ((ok (string-match-p "finished" event))
                   (exit-code (process-exit-status proc)))
               (if ok
                   (message "Noema publish: %s done." label)
                 (message "Noema publish: %s FAILED (exit %s). See %s"
                          label exit-code my/aaronnote-publish--log-buffer))
               (setq my/aaronnote-publish--process nil)
               (when sentinel (funcall sentinel ok exit-code))))))))

(defun my/aaronnote-publish--run-sync (label args &optional extra-env)
  "Run ARGS synchronously (for batch/make use), printing output to stdout.
EXTRA-ENV is an alist of additional env vars.  Signals error on non-zero exit."
  (make-directory my/aaronnote-publish-state-dir t)
  (princ (format "[publish] %s...\n" label))
  (let* ((process-environment (my/aaronnote-publish--env extra-env))
         (default-directory my/aaronnote-publish-root)
         (out-buf (generate-new-buffer " *aaronnote-publish-out*"))
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

(defun my/aaronnote-publish--prepare-git-commit (out-buf)
  "Stage publish output and commit it when it changed, logging to OUT-BUF.
Signal an error when staging, inspection, or committing fails."
  (let ((default-directory my/aaronnote-publish-root))
    (unless (zerop (call-process "git" nil out-buf nil "add" "-A"))
      (error "git add failed in %s" my/aaronnote-publish-root))
    (pcase (call-process "git" nil out-buf nil "diff" "--cached" "--quiet")
      (0 nil)
      (1
       (unless (zerop (call-process
                       "git" nil out-buf nil
                       "commit" "-m"
                       (format "site update: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (error "git commit failed in %s" my/aaronnote-publish-root)))
      (status
       (error "git diff --cached failed (exit %d)" status)))))

(defun my/aaronnote-publish--deploy-sync ()
  "Run git add+commit+push and optional NAS rsync, printing output to stdout."
  (let ((default-directory my/aaronnote-publish-root)
        (out-buf (generate-new-buffer " *aaronnote-deploy-out*")))
    (unwind-protect
        (progn
          (princ (format "[publish] deploy: git add + commit (%s)...\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")))
          (my/aaronnote-publish--prepare-git-commit out-buf)
          (princ (with-current-buffer out-buf (buffer-string)))
          (with-current-buffer out-buf (erase-buffer))
          (princ "[publish] git push...\n")
          (let ((exit (call-process "git" nil out-buf nil "push")))
            (princ (with-current-buffer out-buf (buffer-string)))
            (unless (zerop exit)
              (error "git push failed (exit %d)" exit)))
          (when my/aaronnote-publish-nas-enable
            (with-current-buffer out-buf (erase-buffer))
            (princ (format "[publish] rsync → %s...\n" my/aaronnote-publish-nas-target))
            (let ((exit (call-process "rsync" nil out-buf nil
                                      "-avh" "--delete"
                                      "--exclude" ".deps/"
                                      "--exclude" "state.json"
                                      "--exclude" ".DS_Store"
                                      "--progress" "-e" "ssh"
                                      (file-name-as-directory my/aaronnote-publish-root)
                                      my/aaronnote-publish-nas-target)))
              (princ (with-current-buffer out-buf (buffer-string)))
              (unless (zerop exit)
                (error "rsync NAS failed (exit %d)" exit)))))
      (kill-buffer out-buf))))

(defun my/aaronnote-publish--cv-build-sync ()
  "Compile the LaTeX CV synchronously; copy PDF to publish root."
  (let* ((cv-state (expand-file-name "cv" my/aaronnote-publish-state-dir))
         (jobname "Aaron_He_CV")
         (pdf-in (expand-file-name (concat jobname ".pdf") cv-state))
         (pdf-out (expand-file-name (concat "CV/" jobname ".pdf") my/aaronnote-publish-root)))
    (make-directory cv-state t)
    (make-directory (expand-file-name "CV" my/aaronnote-publish-root) t)
    (princ "[publish] compiling CV...\n")
    (let ((exit-code (call-process
                      "latexmk" nil (get-buffer-create my/aaronnote-publish--log-buffer) nil
                      "-xelatex" "-interaction=nonstopmode" "-halt-on-error"
                      (concat "-outdir=" cv-state)
                      (concat "-jobname=" jobname)
                      (expand-file-name "main.tex" my/aaronnote-publish-cv-dir))))
      (if (zerop exit-code)
          (if (file-exists-p pdf-in)
              (progn
                (copy-file pdf-in pdf-out t)
                (princ (format "[publish] CV compiled → %s\n" pdf-out)))
            (error "CV compiler succeeded but produced no PDF: %s" pdf-in))
        (error "CV compilation failed (exit %d)" exit-code)))))

;;; Public interactive commands

;;;###autoload
(defun my/aaronnote-publish-build ()
  "Build the static site (render notes + copy assets + compile CV)."
  (interactive)
  (my/aaronnote-publish--cv-build-sync)
  (my/aaronnote-publish--run
   "build"
   (list "python3" my/aaronnote-publish-engine)))

;;;###autoload
(defun my/aaronnote-publish-deploy ()
  "Deploy: git commit+push in publish repo, optionally rsync to NAS."
  (interactive)
  (let ((default-directory my/aaronnote-publish-root))
    (my/aaronnote-publish--log
     (format "\n[%s] deploy: git add + commit + push\n" (format-time-string "%H:%M:%S")))
    (my/aaronnote-publish--show-log)
    (message "Noema publish: committing…")
    (my/aaronnote-publish--prepare-git-commit
     (get-buffer-create my/aaronnote-publish--log-buffer))
    (my/aaronnote-publish--run
     "git push"
     (list "git" "-C" my/aaronnote-publish-root "push")
     (when my/aaronnote-publish-nas-enable
       (lambda (ok _code)
         (when ok
           (my/aaronnote-publish--run
            "rsync NAS"
            (list "rsync" "-avh" "--delete"
                  "--exclude" ".deps/"
                  "--exclude" "state.json"
                  "--exclude" ".DS_Store"
                  "--progress" "-e" "ssh"
                  (file-name-as-directory my/aaronnote-publish-root)
                  my/aaronnote-publish-nas-target))))))))

;;;###autoload
(defun my/aaronnote-publish ()
  "Build and deploy the static site."
  (interactive)
  (my/aaronnote-publish--cv-build-sync)
  (my/aaronnote-publish--run
   "build"
   (list "python3" my/aaronnote-publish-engine)
   (lambda (ok _code)
     (when ok
       (my/aaronnote-publish-deploy)))))

;;;###autoload
(defun my/aaronnote-publish-clean ()
  "Remove publish state/cache and generated CV intermediates."
  (interactive)
  (when my/aaronnote-publish--process
    (when (process-live-p my/aaronnote-publish--process)
      (kill-process my/aaronnote-publish--process))
    (setq my/aaronnote-publish--process nil))
  (dolist (name '("deps" "tmp" "book" "cv"))
    (let ((path (expand-file-name name my/aaronnote-publish-state-dir)))
      (when (file-directory-p path)
        (delete-directory path t))))
  (dolist (name '("state.json" ".publish-state.json"))
    (let ((path (expand-file-name name my/aaronnote-publish-state-dir)))
      (when (file-exists-p path)
        (delete-file path))))
  (when (buffer-live-p (get-buffer my/aaronnote-publish--log-buffer))
    (kill-buffer my/aaronnote-publish--log-buffer))
  (message "Noema publish: cleaned state/cache."))

;;; Batch entry points (used by make publish-*)

(defun my/aaronnote-publish-batch ()
  "Batch entry: build + deploy.  Calls `kill-emacs' with non-zero on error."
  (condition-case err
      (progn
        (my/aaronnote-publish--cv-build-sync)
        (my/aaronnote-publish--run-sync
         "build" (list "python3" my/aaronnote-publish-engine))
        (my/aaronnote-publish--deploy-sync))
    (error
     (princ (format "Noema publish FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/aaronnote-publish-force-batch ()
  "Batch entry: force build + deploy, skipping incremental state check."
  (condition-case err
      (progn
        (my/aaronnote-publish--cv-build-sync)
        (my/aaronnote-publish--run-sync
         "build (forced)" (list "python3" my/aaronnote-publish-engine)
         (list (cons "PUBLISH_FORCE" "1")))
        (my/aaronnote-publish--deploy-sync))
    (error
     (princ (format "Noema publish-force FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/aaronnote-publish-build-batch ()
  "Batch entry: build only (no deploy)."
  (condition-case err
      (progn
        (my/aaronnote-publish--cv-build-sync)
        (my/aaronnote-publish--run-sync
         "build" (list "python3" my/aaronnote-publish-engine)))
    (error
     (princ (format "Noema publish-build FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/aaronnote-publish-deploy-batch ()
  "Batch entry: deploy only (git push + optional NAS rsync)."
  (condition-case err
      (my/aaronnote-publish--deploy-sync)
    (error
     (princ (format "Noema publish-deploy FAILED: %s\n" (error-message-string err)))
     (kill-emacs 1))))

(defun my/aaronnote-publish-clean-batch ()
  "Batch entry: clean publish state."
  (my/aaronnote-publish-clean))

(provide 'init-aaronnote-publish)
;;; init-aaronnote-publish.el ends here
