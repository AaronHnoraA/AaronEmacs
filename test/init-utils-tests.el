;;; init-utils-tests.el --- Portal utility tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-utils)

(ert-deftest my/portal-wrap-validates-command ()
  (dolist (command '(nil () "" "  " ("") ("tool" 1)))
    (should-error (my/portal-wrap command) :type 'user-error)))

(ert-deftest my/portal-wrap-passes-argv-and-directory-to-portal ()
  (let ((buffer (generate-new-buffer " *portal-wrap-test*"))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'my/portal-create)
                   (lambda (&rest args)
                     (setq called args)
                     buffer)))
          (should (eq (my/portal-wrap '("tool" "two words")
                                      :directory temporary-file-directory
                                      :display nil)
                      buffer))
          (should (equal (plist-get called :command) '("tool" "two words")))
          (should (equal (plist-get called :directory)
                         temporary-file-directory))
          (should-not (plist-get called :display)))
      (kill-buffer buffer))))

(provide 'init-utils-tests)
;;; init-utils-tests.el ends here
