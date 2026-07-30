;;; init-interaction-utils-tests.el --- Small interaction command tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-interaction-utils-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-open)
(require 'init-smartparens)
(require 'init-navigation-extra)

(ert-deftest my/pairs-cycle-delimiters-forward-and-backward ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(alpha [beta])")
    (goto-char (+ (point-min) 3))
    (my/pairs-cycle-delimiters)
    (should (equal (buffer-string) "[alpha [beta]]"))
    (my/pairs-cycle-delimiters)
    (should (equal (buffer-string) "{alpha [beta]}"))
    (my/pairs-cycle-delimiters -1)
    (should (equal (buffer-string) "[alpha [beta]]"))))

(ert-deftest my/pairs-cycle-delimiters-works-at-closing-edge ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(value)")
    (goto-char (point-max))
    (my/pairs-cycle-delimiters)
    (should (equal (buffer-string) "[value]"))))

(ert-deftest my/pairs-cycle-delimiters-rejects-string-content ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"(text)\"")
    (goto-char (+ (point-min) 2))
    (should-error (my/pairs-cycle-delimiters) :type 'user-error)))

(ert-deftest my/navigation-parse-location-formats ()
  (should
   (equal (my/navigation-parse-location "src/main.el:12:3")
          '(:file "src/main.el" :line 12 :column 3)))
  (should
   (equal (my/navigation-parse-location "/ssh:user@host:/tmp/main.el:8:2")
          '(:file "/ssh:user@host:/tmp/main.el" :line 8 :column 2)))
  (should
   (equal (my/navigation-parse-location "file:///tmp/main.el#L7C4")
          '(:file "/tmp/main.el" :line 7 :column 4)))
  (should
   (equal (my/navigation-parse-location "plain.el")
          '(:file "plain.el" :line nil :column nil))))

(ert-deftest my/navigation-parse-location-rejects-zero-position ()
  (should-error (my/navigation-parse-location "main.el:0") :type 'user-error)
  (should-error (my/navigation-parse-location "main.el:2:0") :type 'user-error))

(ert-deftest my/navigation-open-location-visits-position ()
  (let* ((directory (make-temp-file "emacs-location-test-" t))
         (file (expand-file-name "sample.el" directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "first\nsecond line\nthird\n"))
          (let ((default-directory directory))
            (setq buffer (my/navigation-open-location "sample.el:2:4")))
          (with-current-buffer buffer
            (should (= (line-number-at-pos) 2))
            (should (= (current-column) 3))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest my/open-keybinding-guide-inhibits-markdown-redirect ()
  (let (redirect-inhibited buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'find-file)
                   (lambda (_file)
                     (setq redirect-inhibited my/noema--inhibit-redirect
                           buffer (generate-new-buffer " *keybinding-guide-test*"))
                     buffer)))
          (should (eq (my/open-keybinding-guide) buffer))
          (should redirect-inhibited)
          (should (buffer-local-value 'view-mode buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'init-interaction-utils-tests)

;;; init-interaction-utils-tests.el ends here
