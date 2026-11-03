;;; remote-strict-compile.el --- Warning-clean remote boundary check -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile every framework file into a disposable directory.  This is a
;; local compatibility tripwire: new Emacs/TRAMP warnings fail the check
;; without leaving generated artifacts in the source tree.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'package)
(require 'seq)

(defconst remote-strict--consumer-files
  '("lisp/term/shell/eshell.el" "lisp/tools/agent/queue.el"
    "lisp/tools/agent/config.el" "lisp/tools/agent/acp.el")
  "Placement consumers compiled in the full configuration lane.")

(defun remote-strict--configuration-p (file)
  "Whether FILE is a configuration entry requiring the full init context."
  (string-match-p "/\\(?:init-remote\\|framework/\\(?:config\\|tramp\\)\\)\\.el\\'" file))

(defun remote-strict-byte-compile (&optional configuration)
  "Compile Remote libraries strictly, or CONFIGURATION after a full init.
The library lane stays independent of the user's package bootstrap.  The
configuration lane checks the remaining entry points with their real deps."
  (let* ((root (expand-file-name "lisp/remote" user-emacs-directory))
         (destination (make-temp-file "remote-byte-compile-" t))
         (files
          (append (funcall (if configuration #'seq-filter #'seq-remove)
                           #'remote-strict--configuration-p
                           (directory-files-recursively root "\\.el\\'"))
                  (when configuration
                    (mapcar (lambda (file) (expand-file-name file user-emacs-directory))
                            remote-strict--consumer-files))))
         (byte-compile-error-on-warn t)
         (byte-compile-warnings t)
         (load-prefer-newer t))
    (add-to-list 'load-path
                 (expand-file-name "lisp/core" user-emacs-directory))
    (unless configuration
      (setq package-user-dir
            (file-name-as-directory
             (expand-file-name "elpa" user-emacs-directory)))
      (package-initialize))
    (unwind-protect
        (cl-letf
            (((symbol-function 'byte-compile-dest-file)
              (lambda (source)
                (expand-file-name
                 (concat (secure-hash 'sha1 source) ".elc")
                 destination))))
          (dolist (file files)
            (message "Strict byte compile: %s"
                     (file-relative-name file user-emacs-directory))
            (unless (byte-compile-file file)
              (error "Byte compilation returned nil for %s" file))))
      (delete-directory destination t))))

(provide 'remote-strict-compile)
;;; remote-strict-compile.el ends here
