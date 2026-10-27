;;; lang/go/config.el --- Go build and run through project Eshell -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep Go's direct build key with the language.  The queue, timeout and
;; floating display are provided by the shared Eshell command-shell API.

;;; Code:

(require 'cl-lib)

(declare-function +eshell-build-root "term/shell/eshell" (source-directory markers))
(declare-function +eshell-build-output-file "term/shell/eshell" (source))
(declare-function +eshell-run-build "term/shell/eshell" (build-command &rest keys))

(defgroup +go-eshell-build nil
  "Go builds sent to a named Eshell session."
  :group 'languages)

(defcustom +go-eshell-build-command nil
  "Project build command, or nil to use make, go build, or a single file."
  :type '(choice (const :tag "Automatic" nil) string)
  :group '+go-eshell-build)

(defcustom +go-eshell-run-command nil
  "Run command after a successful Go build, or nil for automatic defaults."
  :type '(choice (const :tag "Automatic" nil) string)
  :group '+go-eshell-build)

(defcustom +go-eshell-run-after-build nil
  "If non-nil, `C-c C-c' builds and then runs Go programs.
A prefix argument also requests a run for one invocation."
  :type 'boolean
  :group '+go-eshell-build)

(defun +go-eshell--makefile-p (directory)
  "Return non-nil if DIRECTORY contains a makefile."
  (cl-some (lambda (name)
             (file-regular-p (expand-file-name name directory)))
           '("Makefile" "makefile" "GNUmakefile")))

(defun +go/eshell-build (&optional run)
  "Build this Go file or project in its named Eshell.
With prefix RUN, also execute the program after a successful build."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Visit a Go source file before building"))
  (when (file-remote-p buffer-file-name)
    (user-error "This Go build demo currently supports local files only"))
  (require 'term/shell/eshell)
  (save-buffer)
  (let* ((source buffer-file-name)
         (root (+eshell-build-root
                (file-name-directory source)
                '("Makefile" "makefile" "GNUmakefile" "go.mod")))
         (make-project (+go-eshell--makefile-p root))
         (module-project (file-regular-p (expand-file-name "go.mod" root)))
         (binary (+eshell-build-output-file source))
         (build (or +go-eshell-build-command
                    (cond
                     (make-project "make")
                     (module-project "go build ./...")
                     (t (format "go build -o %s %s"
                                (shell-quote-argument binary)
                                (shell-quote-argument
                                 (file-name-nondirectory source)))))))
         (run-command
          (when (or run +go-eshell-run-after-build)
            (or +go-eshell-run-command
                (cond
                 ((or make-project +go-eshell-build-command) nil)
                 (module-project "go run .")
                 (t (shell-quote-argument (concat "./" binary))))))))
    (when (and (or run +go-eshell-run-after-build)
               (not run-command))
      (user-error "Set +go-eshell-run-command for this project"))
    (+eshell-run-build build :directory root :run run-command)))

(defun +go-eshell-build-setup-h ()
  "Make C-c C-c the direct Go project build entry."
  (local-set-key (kbd "C-c C-c") #'+go/eshell-build))

(dolist (mode '(go-mode go-ts-mode))
  (add-hook (intern (format "%s-hook" mode)) #'+go-eshell-build-setup-h))

(provide 'lang/go/config)
;;; lang/go/config.el ends here
