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

(ert-deftest +macos-reveal-in-finder-selects-this-machine-files-only ()
  "Reveal asks Finder to select a local file and refuses another target's."
  (skip-unless (eq system-type 'darwin))
  (require 'os/macos-tools/config)
  (let (called)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq called args) 0)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "early-init.el" user-emacs-directory))
        (macos-reveal-in-finder)
        (setq buffer-file-name nil))
      (should (equal (seq-take called 5) '("open" nil nil nil "-R")))
      (setq called nil)
      (cl-letf (((symbol-function 'remote-client-file-name) #'ignore))
        (with-temp-buffer
          (setq buffer-file-name "/fs:elsewhere:/srv/a.txt")
          (should-error (macos-reveal-in-finder) :type 'user-error)
          (setq buffer-file-name nil)))
      (should-not called))))

;;; init-macos-locate-tests.el ends here
