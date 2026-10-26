;;; init-macos-locate-tests.el --- locate through Spotlight on macOS -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-macos-locate-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(ert-deftest +macos-locate-uses-spotlight-without-log-noise ()
  "locate and consult-locate ask Spotlight, and only paths come back."
  (skip-unless (eq system-type 'darwin))
  (require 'locate)
  (require 'consult)
  (should (equal consult-locate-args "mdfind -name"))
  (let ((cmd (funcall locate-make-command-line "Makefile")))
    (with-temp-buffer
      (apply #'call-process (car cmd) nil t nil (cdr cmd))
      (dolist (line (split-string (buffer-string) "\n" t))
        (should (string-prefix-p "/" line))))))

;;; init-macos-locate-tests.el ends here
