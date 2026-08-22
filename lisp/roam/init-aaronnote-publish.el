;;; init-aaronnote-publish.el --- Personal site build and deploy -*- lexical-binding: t; -*-
;;
;; Lazy-loaded: not required at startup.  Loaded on first publish command.
;; Both interactive commands and batch (make publish-*) entry points live here.
;;
;; The website itself is hand-written and lives in the publish repository; see
;; `docs/publish-workflow.md'.  Nothing here renders it.  This module compiles
;; the LaTeX CV into that repository and then ships the repository: git commit
;; and push, plus an optional rsync to the NAS that serves it.

;;; Code:

(require 'config)

;; Derive config root from this file's path so batch mode (-Q) gets the right
;; user-emacs-directory instead of the default ~/.emacs.d/.
(defconst my/noema-publish--config-root
  (if load-file-name
      (file-truename
       (expand-file-name "../../" (file-name-directory (file-truename load-file-name))))
    user-emacs-directory)
  "Emacs config root, derived from load-file-name for batch-mode correctness.")

(defgroup my/noema-publish nil
  "Personal site build and deployment."
  :group 'applications)

(config-defvar my/noema-publish-root nil
  "Path to the publish git repo root.  The website lives here."
  :type 'directory
  :group 'my/noema-publish)

(config-defvar my/noema-publish-state-dir nil
  "Path to publish build state.  Holds CV compilation intermediates."
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

(defconst my/noema-publish--required-files
  '("index.html"
    "assets/css/site.css"
    "assets/js/site.js"
    "assets/js/world/index.js"
    "assets/js/world/curve.js"
    "assets/js/world/circuit.js"
    "assets/js/world/flight.js"
    "assets/js/world/rig.js"
    "assets/js/world/panels.js"
    "assets/js/world/states.js"
    "assets/js/world/css3d.js"
    "assets/js/world/math.js"
    ;; three.module.min.js imports three.core.min.js, and the page imports the
    ;; CSS3DRenderer addon: a missing one of these is a blank page, not a
    ;; degraded one.
    "vendor/three/three.module.min.js"
    "vendor/three/three.core.min.js"
    "vendor/three/CSS3DRenderer.js"
    "vendor/anime/anime.esm.min.js"
    "vendor/katex/katex.mjs"
    "vendor/katex/katex.min.css"
    "vendor/katex/fonts/KaTeX_Main-Regular.woff2"
    "LICENSE")
  "Files that must exist in the publish repo before it is worth deploying.
The vendored licences are listed deliberately: shipping the libraries without
them would be a licence violation, so a missing one is a hard failure.")

(defconst my/noema-publish--required-licences
  '("vendor/three/LICENSE" "vendor/anime/LICENSE.md" "vendor/katex/LICENSE")
  "Third-party licence files that must ship alongside the vendored code.")

(defvar my/noema-publish--process nil
  "Current publish subprocess, or nil.")

;;; Internal helpers

(defun my/noema-publish--check-site ()
  "Signal an error unless the publish repo holds a complete site."
  (let (missing)
    (dolist (rel (append my/noema-publish--required-files
                         my/noema-publish--required-licences))
      (unless (file-exists-p (expand-file-name rel my/noema-publish-root))
        (push rel missing)))
    (when missing
      (error "Publish repo is incomplete, refusing to deploy; missing: %s"
             (string-join (nreverse missing) ", ")))))

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
  "Run ARGS in the publish repo, logging to the log buffer.
LABEL is shown in progress messages.  SENTINEL is called when the process exits."
  (when (and my/noema-publish--process
             (process-live-p my/noema-publish--process))
    (user-error "A publish process is already running; wait for it to finish"))
  (with-current-buffer (get-buffer-create my/noema-publish--log-buffer)
    (goto-char (point-max))
    (insert (format "\n[%s] %s\n" (format-time-string "%H:%M:%S") label)))
  (my/noema-publish--show-log)
  (message "Noema publish: %s…" label)
  (let ((default-directory my/noema-publish-root))
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

(defun my/noema-publish--rsync-args ()
  "Return the rsync argument list for a NAS deployment."
  (list "-avh" "--delete"
        "--exclude" ".git/"
        "--exclude" ".github/"
        "--exclude" ".DS_Store"
        "--progress" "-e" "ssh"
        (file-name-as-directory my/noema-publish-root)
        my/noema-publish-nas-target))

(defun my/noema-publish--deploy-sync ()
  "Run git add+commit+push and optional NAS rsync, printing output to stdout."
  (my/noema-publish--check-site)
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
            (let ((exit (apply #'call-process "rsync" nil out-buf nil
                               (my/noema-publish--rsync-args))))
              (princ (with-current-buffer out-buf (buffer-string)))
              (unless (zerop exit)
                (error "rsync NAS failed (exit %d)" exit)))))
      (kill-buffer out-buf))))

(defun my/noema-publish--cv-build-sync ()
  "Compile the LaTeX CV synchronously; copy PDF into the publish repo."
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
  "Build what the site needs generating: the CV PDF.
The pages themselves are hand-written and need no build step."
  (interactive)
  (my/noema-publish--cv-build-sync)
  (my/noema-publish--check-site)
  (message "Noema publish: build done."))

;;;###autoload
(defun my/noema-publish-deploy ()
  "Deploy: git commit+push in publish repo, optionally rsync to NAS."
  (interactive)
  (my/noema-publish--check-site)
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
           (my/noema-publish--run "rsync NAS"
                                  (cons "rsync" (my/noema-publish--rsync-args)))))))))

;;;###autoload
(defun my/noema-publish ()
  "Build the CV and deploy the site."
  (interactive)
  (my/noema-publish--cv-build-sync)
  (my/noema-publish-deploy))

;;;###autoload
(defun my/noema-publish-clean ()
  "Remove publish build state (CV intermediates)."
  (interactive)
  (when my/noema-publish--process
    (when (process-live-p my/noema-publish--process)
      (kill-process my/noema-publish--process))
    (setq my/noema-publish--process nil))
  (let ((cv-state (expand-file-name "cv" my/noema-publish-state-dir)))
    (when (file-directory-p cv-state)
      (delete-directory cv-state t)))
  (when (buffer-live-p (get-buffer my/noema-publish--log-buffer))
    (kill-buffer my/noema-publish--log-buffer))
  (message "Noema publish: cleaned build state."))

;;; Batch entry points (used by make publish-*)

(defmacro my/noema-publish--batch (label &rest body)
  "Run BODY as a batch entry point, exiting non-zero when it signals.
LABEL names the operation in the failure message."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (princ (format "Noema %s FAILED: %s\n" ,label (error-message-string err)))
      (kill-emacs 1))))

(defun my/noema-publish-batch ()
  "Batch entry: build the CV and deploy."
  (my/noema-publish--batch "publish"
    (my/noema-publish--cv-build-sync)
    (my/noema-publish--deploy-sync)))

(defun my/noema-publish-build-batch ()
  "Batch entry: build only (no deploy)."
  (my/noema-publish--batch "publish-build"
    (my/noema-publish--cv-build-sync)
    (my/noema-publish--check-site)
    (princ "[publish] site complete.\n")))

(defun my/noema-publish-deploy-batch ()
  "Batch entry: deploy only (git push + optional NAS rsync)."
  (my/noema-publish--batch "publish-deploy"
    (my/noema-publish--deploy-sync)))

(defun my/noema-publish-clean-batch ()
  "Batch entry: clean build state."
  (my/noema-publish-clean))

(provide 'init-aaronnote-publish)
;;; init-aaronnote-publish.el ends here
