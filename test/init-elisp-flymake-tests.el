;;; init-elisp-flymake-tests.el --- Byte-compile Flymake in this config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-elisp-flymake-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'flymake)
(require 'lang/elisp/config)

(ert-deftest +elisp-flymake-checks-a-directory-module-past-its-requires ()
  "A module's config.el is compiled with the session's features in reach.
It used to stop at its first `require': the core feature was missing, or
\"./config.el\" (the module itself) shadowed the core `config'."
  (let* ((dir (file-name-as-directory (make-temp-file "flymake-module-" t)))
         (file (expand-file-name "config.el" dir))
         (trusted-content :all)
         diagnostics done)
    (unwind-protect
        (progn
          (write-region
           (concat ";;; config.el --- test -*- lexical-binding: t; -*-\n"
                   "(require 'config)\n(require 'aaron-funcs)\n"
                   "(defun +flymake-test-probe () (+flymake-test-undefined))\n"
                   "(provide 'test/module/config)\n")
           nil file nil 'silent)
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (elisp-flymake-byte-compile
                   (lambda (found &rest _) (setq diagnostics found done t)))
                  (with-timeout (60 (ert-fail "Flymake byte-compile timed out"))
                    (while (not done) (accept-process-output nil 0.1))))
              (kill-buffer)))
          (let ((texts (mapcar #'flymake-diagnostic-text diagnostics)))
            (should-not (seq-find (lambda (text)
                                    (string-match-p "Cannot open load file\\|Recursive" text))
                                  texts))
            (should (seq-find (lambda (text)
                                (string-match-p "+flymake-test-undefined" text))
                              texts))))
      (delete-directory dir t))))

;;; init-elisp-flymake-tests.el ends here
