;;; init-aaronnote-publish.el --- Aaronnote site publish commands -*- lexical-binding: t; -*-
;;
;; Lazy-loaded: not required at startup.  Loaded on first publish command.
;; Both interactive commands and batch (make publish-*) entry points live here.

;;; Code:

(require 'cl-lib)

(defgroup my/aaronnote-publish nil
  "Aaronnote static-site build and deployment."
  :group 'applications)

(defcustom my/aaronnote-publish-root
  (expand-file-name "publish" user-emacs-directory)
  "Path to the publish git repo root (output lands here directly)."
  :type 'directory
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-engine
  (expand-file-name "lisp/roam/aaronnote/publish/publish-site" user-emacs-directory)
  "Path to the Python publish engine."
  :type 'file
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-assets-dir
  (expand-file-name "lisp/roam/aaronnote/publish/assets" user-emacs-directory)
  "Path to publish source assets (css/, kinds/, homepage.html, etc.)."
  :type 'directory
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-state-dir
  (expand-file-name "var/aaronnote/publish" user-emacs-directory)
  "Path to publish cache/state directory (deps/, state.json, book/, cv/)."
  :type 'directory
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-cv-dir
  (expand-file-name "lisp/roam/aaronnote/publish/CV" user-emacs-directory)
  "Path to the CV LaTeX source directory."
  :type 'directory
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-nas-target "Aaron-nas:/volume1/web/public/"
  "rsync target for NAS deployment."
  :type 'string
  :group 'my/aaronnote-publish)

(defcustom my/aaronnote-publish-nas-enable nil
  "When non-nil, rsync to NAS after git push during deploy."
  :type 'boolean
  :group 'my/aaronnote-publish)

(defconst my/aaronnote-publish--log-buffer "*Aaronnote Publish*"
  "Name of the publish log buffer.")

(defvar my/aaronnote-publish--process nil
  "Current publish subprocess, or nil.")

;;; Internal helpers

(defun my/aaronnote-publish--runtime-root ()
  "Return the Aaronnote runtime root path."
  (expand-file-name "lisp/roam/aaronnote" user-emacs-directory))

(defun my/aaronnote-publish--roam-root ()
  "Return the notes vault root."
  (expand-file-name ".roam" user-emacs-directory))

(defun my/aaronnote-publish--env ()
  "Return env-var alist for the publish engine process."
  (let ((runtime (my/aaronnote-publish--runtime-root)))
    (append
     (list (cons "AARONNOTE_PUBLISH_OUTPUT"   my/aaronnote-publish-root)
           (cons "AARONNOTE_RUNTIME_ROOT"     runtime)
           (cons "AARONNOTE_PUBLISH_ASSETS"   my/aaronnote-publish-assets-dir)
           (cons "AARONNOTE_PUBLISH_STATE_DIR" my/aaronnote-publish-state-dir)
           (cons "AARONNOTE_ROAM_ROOT"        (my/aaronnote-publish--roam-root)))
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
  (message "Aaronnote publish: %s…" label)
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
                   (message "Aaronnote publish: %s done." label)
                 (message "Aaronnote publish: %s FAILED (exit %s). See %s"
                          label exit-code my/aaronnote-publish--log-buffer))
               (setq my/aaronnote-publish--process nil)
               (when sentinel (funcall sentinel ok exit-code))))))))

(defun my/aaronnote-publish--run-sync (label args)
  "Run ARGS synchronously (for batch/make use).  Signal error on non-zero exit."
  (make-directory my/aaronnote-publish-state-dir t)
  (message "Aaronnote publish: %s…" label)
  (let* ((process-environment (my/aaronnote-publish--env))
         (default-directory my/aaronnote-publish-root)
         (exit-code (apply #'call-process (car args) nil t nil (cdr args))))
    (unless (zerop exit-code)
      (error "Aaronnote publish: %s failed (exit %d)" label exit-code))
    (message "Aaronnote publish: %s done." label)))

(defun my/aaronnote-publish--cv-build-sync ()
  "Compile the LaTeX CV synchronously; copy PDF to publish root."
  (let* ((cv-state (expand-file-name "cv" my/aaronnote-publish-state-dir))
         (jobname "Aaron_He_CV")
         (pdf-in (expand-file-name (concat jobname ".pdf") cv-state))
         (pdf-out (expand-file-name (concat "CV/" jobname ".pdf") my/aaronnote-publish-root)))
    (make-directory cv-state t)
    (make-directory (expand-file-name "CV" my/aaronnote-publish-root) t)
    (message "Aaronnote publish: compiling CV…")
    (let ((exit-code (call-process
                      "latexmk" nil (get-buffer-create my/aaronnote-publish--log-buffer) nil
                      "-xelatex" "-interaction=nonstopmode" "-halt-on-error"
                      (concat "-outdir=" cv-state)
                      (concat "-jobname=" jobname)
                      (expand-file-name "main.tex" my/aaronnote-publish-cv-dir))))
      (if (zerop exit-code)
          (progn
            (copy-file pdf-in pdf-out t)
            (message "Aaronnote publish: CV compiled → %s" pdf-out))
        (message "Aaronnote publish: CV compilation failed; see %s"
                 my/aaronnote-publish--log-buffer)))))

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
    (message "Aaronnote publish: committing…")
    (shell-command
     (format "cd %s && git add -A && git diff --cached --quiet || git commit -m 'site update: %s'"
             (shell-quote-argument my/aaronnote-publish-root)
             (format-time-string "%Y-%m-%d %H:%M:%S"))
     my/aaronnote-publish--log-buffer)
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
  (let ((tmp (expand-file-name "tmp" my/aaronnote-publish-state-dir))
        (cv  (expand-file-name "cv" my/aaronnote-publish-state-dir)))
    (when (file-directory-p tmp) (delete-directory tmp t))
    (when (file-directory-p cv)  (delete-directory cv t)))
  (when (buffer-live-p (get-buffer my/aaronnote-publish--log-buffer))
    (kill-buffer my/aaronnote-publish--log-buffer))
  (message "Aaronnote publish: cleaned state/cache."))

;;; Batch entry points (used by make publish-*)

(defun my/aaronnote-publish-batch ()
  "Batch entry: build + deploy.  Calls `kill-emacs' with non-zero on error."
  (condition-case err
      (progn
        (my/aaronnote-publish--cv-build-sync)
        (my/aaronnote-publish--run-sync
         "build" (list "python3" my/aaronnote-publish-engine))
        (let ((default-directory my/aaronnote-publish-root))
          (call-process-shell-command
           (format "git add -A && git diff --cached --quiet || git commit -m 'site update: %s'"
                   (format-time-string "%Y-%m-%d %H:%M:%S"))
           nil t)
          (let ((exit (call-process "git" nil t nil "-C" my/aaronnote-publish-root "push")))
            (unless (zerop exit)
              (error "git push failed (exit %d)" exit)))
          (when my/aaronnote-publish-nas-enable
            (call-process "rsync" nil t nil
                          "-avh" "--delete"
                          "--exclude" ".deps/" "--exclude" "state.json"
                          "--exclude" ".DS_Store" "--progress" "-e" "ssh"
                          (file-name-as-directory my/aaronnote-publish-root)
                          my/aaronnote-publish-nas-target))))
    (error (message "Aaronnote publish failed: %s" (error-message-string err))
           (kill-emacs 1))))

(defun my/aaronnote-publish-build-batch ()
  "Batch entry: build only (no deploy)."
  (condition-case err
      (progn
        (my/aaronnote-publish--cv-build-sync)
        (my/aaronnote-publish--run-sync
         "build" (list "python3" my/aaronnote-publish-engine)))
    (error (message "Aaronnote publish-build failed: %s" (error-message-string err))
           (kill-emacs 1))))

(defun my/aaronnote-publish-deploy-batch ()
  "Batch entry: deploy only (git push + optional NAS rsync)."
  (condition-case err
      (let ((default-directory my/aaronnote-publish-root))
        (call-process-shell-command
         (format "git add -A && git diff --cached --quiet || git commit -m 'site update: %s'"
                 (format-time-string "%Y-%m-%d %H:%M:%S"))
         nil t)
        (let ((exit (call-process "git" nil t nil "-C" my/aaronnote-publish-root "push")))
          (unless (zerop exit) (error "git push failed (exit %d)" exit)))
        (when my/aaronnote-publish-nas-enable
          (call-process "rsync" nil t nil
                        "-avh" "--delete"
                        "--exclude" ".deps/" "--exclude" "state.json"
                        "--exclude" ".DS_Store" "--progress" "-e" "ssh"
                        (file-name-as-directory my/aaronnote-publish-root)
                        my/aaronnote-publish-nas-target)))
    (error (message "Aaronnote publish-deploy failed: %s" (error-message-string err))
           (kill-emacs 1))))

(defun my/aaronnote-publish-clean-batch ()
  "Batch entry: clean publish state."
  (my/aaronnote-publish-clean))

(provide 'init-aaronnote-publish)
;;; init-aaronnote-publish.el ends here
