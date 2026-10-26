;;; noema/bridge/publish.el --- Personal site build and deploy -*- lexical-binding: t; -*-
;;
;; Lazy-loaded: not required at startup.  Loaded on first publish command.
;; Both interactive commands and batch (make publish-*) entry points live here.
;;
;; The website itself is hand-written and lives in the publish repository; see
;; `docs/guide/publish-workflow.md'.  Nothing here renders it.  This module compiles
;; the LaTeX CV into that repository and then ships the repository: git commit
;; and push, plus an optional rsync to the NAS that serves it.

;;; Code:

(require 'config)

;; Derive config root from this file's path so batch mode (-Q) gets the right
;; user-emacs-directory instead of the default ~/.emacs.d/.
(defconst +noema-publish--config-root
  (if load-file-name
      (file-truename
       (expand-file-name "../../../" (file-name-directory (file-truename load-file-name))))
    user-emacs-directory)
  "Emacs config root, derived from load-file-name for batch-mode correctness.")

(defgroup +noema/publish nil
  "Personal site build and deployment."
  :group 'applications)

(config-defvar +noema-publish-root nil
  "Path to the publish git repo root.  The website lives here."
  :type 'directory
  :group '+noema/publish)

(config-defvar +noema-publish-state-dir nil
  "Path to publish build state.  Holds CV compilation intermediates."
  :type 'directory
  :group '+noema/publish)

(config-defvar +noema-publish-cv-dir nil
  "Path to the CV LaTeX source directory."
  :type 'directory
  :group '+noema/publish)

(config-defvar +noema-publish-nas-target nil
  "rsync target for NAS deployment."
  :type 'string
  :group '+noema/publish)

(config-defvar +noema-publish-nas-enable nil
  "When non-nil, rsync to NAS after git push during deploy."
  :type 'boolean
  :group '+noema/publish)

(defconst +noema-publish--log-buffer "*Noema Publish*"
  "Name of the publish log buffer.")

(defconst +noema-publish--required-files
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

(defconst +noema-publish--required-licences
  '("vendor/three/LICENSE" "vendor/anime/LICENSE.md" "vendor/katex/LICENSE")
  "Third-party licence files that must ship alongside the vendored code.")

(defvar +noema-publish--process nil
  "Current publish subprocess, or nil.")

;;; Internal helpers

(defun +noema-publish--check-site ()
  "Signal an error unless the publish repo holds a complete site."
  (let (missing)
    (dolist (rel (append +noema-publish--required-files
                         +noema-publish--required-licences))
      (unless (file-exists-p (expand-file-name rel +noema-publish-root))
        (push rel missing)))
    (when missing
      (error "Publish repo is incomplete, refusing to deploy; missing: %s"
             (string-join (nreverse missing) ", ")))))

(defun +noema-publish--log (msg)
  "Append MSG to the publish log buffer."
  (with-current-buffer (get-buffer-create +noema-publish--log-buffer)
    (goto-char (point-max))
    (insert msg)))

(defun +noema-publish--show-log ()
  "Show the publish log buffer without stealing focus."
  (let ((buf (get-buffer-create +noema-publish--log-buffer)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-at-bottom . ((window-height . 0.25)))))))

(defun +noema-publish--run (label args &optional sentinel)
  "Run ARGS in the publish repo, logging to the log buffer.
LABEL is shown in progress messages.  SENTINEL is called when the process exits."
  (when (and +noema-publish--process
             (process-live-p +noema-publish--process))
    (user-error "A publish process is already running; wait for it to finish"))
  (with-current-buffer (get-buffer-create +noema-publish--log-buffer)
    (goto-char (point-max))
    (insert (format "\n[%s] %s\n" (format-time-string "%H:%M:%S") label)))
  (+noema-publish--show-log)
  (message "Noema publish: %s…" label)
  (let ((default-directory +noema-publish-root))
    (setq +noema-publish--process
          (make-process
           :name "noema-publish"
           :buffer +noema-publish--log-buffer
           :command args
           :sentinel
           (lambda (proc event)
             (let ((ok (string-match-p "finished" event))
                   (exit-code (process-exit-status proc)))
               (if ok
                   (message "Noema publish: %s done." label)
                 (message "Noema publish: %s FAILED (exit %s). See %s"
                          label exit-code +noema-publish--log-buffer))
               (setq +noema-publish--process nil)
               (when sentinel (funcall sentinel ok exit-code))))))))

(defun +noema-publish--prepare-git-commit (out-buf)
  "Stage publish output and commit it when it changed, logging to OUT-BUF.
Signal an error when staging, inspection, or committing fails."
  (let ((default-directory +noema-publish-root))
    (unless (zerop (call-process "git" nil out-buf nil "add" "-A"))
      (error "git add failed in %s" +noema-publish-root))
    (pcase (call-process "git" nil out-buf nil "diff" "--cached" "--quiet")
      (0 nil)
      (1
       (unless (zerop (call-process
                       "git" nil out-buf nil
                       "commit" "-m"
                       (format "site update: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (error "git commit failed in %s" +noema-publish-root)))
      (status
       (error "git diff --cached failed (exit %d)" status)))))

(defun +noema-publish--rsync-args ()
  "Return the rsync argument list for a NAS deployment."
  (list "-avh" "--delete"
        "--exclude" ".git/"
        "--exclude" ".github/"
        "--exclude" ".DS_Store"
        "--progress" "-e" "ssh"
        (file-name-as-directory +noema-publish-root)
        +noema-publish-nas-target))

(defun +noema-publish--deploy-sync ()
  "Run git add+commit+push and optional NAS rsync, printing output to stdout."
  (+noema-publish--check-site)
  (let ((default-directory +noema-publish-root)
        (out-buf (generate-new-buffer " *Noema deploy output*")))
    (unwind-protect
        (progn
          (princ (format "[publish] deploy: git add + commit (%s)...\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")))
          (+noema-publish--prepare-git-commit out-buf)
          (princ (with-current-buffer out-buf (buffer-string)))
          (with-current-buffer out-buf (erase-buffer))
          (princ "[publish] git push...\n")
          (let ((exit (call-process "git" nil out-buf nil "push")))
            (princ (with-current-buffer out-buf (buffer-string)))
            (unless (zerop exit)
              (error "git push failed (exit %d)" exit)))
          (when +noema-publish-nas-enable
            (with-current-buffer out-buf (erase-buffer))
            (princ (format "[publish] rsync → %s...\n" +noema-publish-nas-target))
            (let ((exit (apply #'call-process "rsync" nil out-buf nil
                               (+noema-publish--rsync-args))))
              (princ (with-current-buffer out-buf (buffer-string)))
              (unless (zerop exit)
                (error "rsync NAS failed (exit %d)" exit)))))
      (kill-buffer out-buf))))

(defun +noema-publish--cv-build-sync ()
  "Compile the LaTeX CV synchronously; copy PDF into the publish repo."
  (let* ((cv-state (expand-file-name "cv" +noema-publish-state-dir))
         (jobname "Aaron_He_CV")
         (pdf-in (expand-file-name (concat jobname ".pdf") cv-state))
         (pdf-out (expand-file-name (concat "CV/" jobname ".pdf") +noema-publish-root)))
    (make-directory cv-state t)
    (make-directory (expand-file-name "CV" +noema-publish-root) t)
    (princ "[publish] compiling CV...\n")
    (let ((exit-code (call-process
                      "latexmk" nil (get-buffer-create +noema-publish--log-buffer) nil
                      "-xelatex" "-interaction=nonstopmode" "-halt-on-error"
                      (concat "-outdir=" cv-state)
                      (concat "-jobname=" jobname)
                      (expand-file-name "main.tex" +noema-publish-cv-dir))))
      (if (zerop exit-code)
          (if (file-exists-p pdf-in)
              (progn
                (copy-file pdf-in pdf-out t)
                (princ (format "[publish] CV compiled → %s\n" pdf-out)))
            (error "CV compiler succeeded but produced no PDF: %s" pdf-in))
        (error "CV compilation failed (exit %d)" exit-code)))))

;;; Public interactive commands

;;;###autoload
(defun +noema/publish-build ()
  "Build what the site needs generating: the CV PDF.
The pages themselves are hand-written and need no build step."
  (interactive)
  (+noema-publish--cv-build-sync)
  (+noema-publish--check-site)
  (message "Noema publish: build done."))

;;;###autoload
(defun +noema/publish-deploy ()
  "Deploy: git commit+push in publish repo, optionally rsync to NAS."
  (interactive)
  (+noema-publish--check-site)
  (let ((default-directory +noema-publish-root))
    (+noema-publish--log
     (format "\n[%s] deploy: git add + commit + push\n" (format-time-string "%H:%M:%S")))
    (+noema-publish--show-log)
    (message "Noema publish: committing…")
    (+noema-publish--prepare-git-commit
     (get-buffer-create +noema-publish--log-buffer))
    (+noema-publish--run
     "git push"
     (list "git" "-C" +noema-publish-root "push")
     (when +noema-publish-nas-enable
       (lambda (ok _code)
         (when ok
           (+noema-publish--run "rsync NAS"
                                  (cons "rsync" (+noema-publish--rsync-args)))))))))

;;;###autoload
(defun +noema/publish ()
  "Build the CV and deploy the site."
  (interactive)
  (+noema-publish--cv-build-sync)
  (+noema/publish-deploy))

;;;###autoload
(defun +noema/publish-clean ()
  "Remove publish build state (CV intermediates)."
  (interactive)
  (when +noema-publish--process
    (when (process-live-p +noema-publish--process)
      (kill-process +noema-publish--process))
    (setq +noema-publish--process nil))
  (let ((cv-state (expand-file-name "cv" +noema-publish-state-dir)))
    (when (file-directory-p cv-state)
      (delete-directory cv-state t)))
  (when (buffer-live-p (get-buffer +noema-publish--log-buffer))
    (kill-buffer +noema-publish--log-buffer))
  (message "Noema publish: cleaned build state."))

;;; Batch entry points (used by make publish-*)

(defmacro +noema-publish--batch (label &rest body)
  "Run BODY as a batch entry point, exiting non-zero when it signals.
LABEL names the operation in the failure message."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (princ (format "Noema %s FAILED: %s\n" ,label (error-message-string err)))
      (kill-emacs 1))))

(defun +noema-publish-batch ()
  "Batch entry: build the CV and deploy."
  (+noema-publish--batch "publish"
    (+noema-publish--cv-build-sync)
    (+noema-publish--deploy-sync)))

(defun +noema-publish-build-batch ()
  "Batch entry: build only (no deploy)."
  (+noema-publish--batch "publish-build"
    (+noema-publish--cv-build-sync)
    (+noema-publish--check-site)
    (princ "[publish] site complete.\n")))

(defun +noema-publish-deploy-batch ()
  "Batch entry: deploy only (git push + optional NAS rsync)."
  (+noema-publish--batch "publish-deploy"
    (+noema-publish--deploy-sync)))

(defun +noema-publish-clean-batch ()
  "Batch entry: clean build state."
  (+noema/publish-clean))

(provide 'noema/bridge/publish)
;;; noema/bridge/publish.el ends here
