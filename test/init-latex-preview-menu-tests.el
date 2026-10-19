;;; init-latex-preview-menu-tests.el --- LaTeX preview entry tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-auctex)

(ert-deftest latex-preview-menu-is-searchable-and-uses-existing-actions ()
  (should (member "latex-preview-menu"
                  (all-completions "latex-preview-menu" obarray #'commandp)))
  (dolist (entry '(("g" . my/latex-sync-forward)
                   ("v" . my/latex-preview-open-pdf)
                   ("a" . my/latex-preview-compile-and-view)
                   ("V" . my/latex-preview-view-pdf)))
    (should (eq (plist-get (cdr (transient-get-suffix
                                'latex-preview-menu (car entry))) :command)
                (cdr entry)))))

(ert-deftest latex-preview-actions-reject-non-tex-buffer-before-native-calls ()
  (with-temp-buffer
    (dolist (command '(my/latex-sync-forward
                       my/latex-preview-open-pdf
                       my/latex-preview-compile-and-view
                       my/latex-preview-view-pdf))
      (should (equal (error-message-string
                      (should-error (funcall command) :type 'user-error))
                     "Not in a file-backed LaTeX buffer")))))

(ert-deftest latex-preview-explicit-local-pdf-stays-local-from-remote-buffer ()
  (let ((preview (generate-new-buffer " *local-pdf-preview*"))
        checked opened visit-directory)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:box:/work/")
          (let ((source (current-buffer)))
            (cl-letf (((symbol-function 'file-exists-p)
                       (lambda (file)
                         (setq checked file)
                         (should (equal default-directory
                                        temporary-file-directory))
                         t))
                      ((symbol-function 'find-file-noselect)
                       (lambda (file &rest _)
                         (setq opened file visit-directory default-directory)
                         preview))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil)))
              (my/latex-preview-open-pdf "/Users/test/project/result.pdf"))
            (should (equal checked "/Users/test/project/result.pdf"))
            (should (equal opened checked))
            (should (equal visit-directory temporary-file-directory))
            (should (equal (buffer-local-value 'default-directory source)
                           "/fs:box:/work/"))))
      (kill-buffer preview))))

(provide 'init-latex-preview-menu-tests)
;;; init-latex-preview-menu-tests.el ends here
