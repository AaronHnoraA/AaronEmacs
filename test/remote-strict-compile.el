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

(defun remote-strict-byte-compile ()
  "Byte compile remote libraries with warnings promoted to errors."
  (let* ((root (expand-file-name "lisp/remote" user-emacs-directory))
         (destination (make-temp-file "remote-byte-compile-" t))
         ;; `init-remote.el' is a user-config entry point whose `config!'
         ;; macro intentionally executes package bootstrap at compile time.
         ;; The compatibility boundary is the reusable framework beneath it.
         (files
          (seq-remove
           (lambda (file) (string-suffix-p "/init-remote.el" file))
           (directory-files-recursively root "\\.el\\'")))
         (byte-compile-error-on-warn t)
         (byte-compile-warnings t)
         (load-prefer-newer t))
    (add-to-list 'load-path
                 (expand-file-name "site-lisp/config" user-emacs-directory))
    (setq package-user-dir
          (file-name-as-directory
           (expand-file-name "elpa" user-emacs-directory)))
    (package-initialize)
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
