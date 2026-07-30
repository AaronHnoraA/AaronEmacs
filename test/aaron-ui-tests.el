;;; aaron-ui-tests.el --- Aaron Elegant design-token tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'aaron-ui)

(ert-deftest aaron-ui-resolves-elegant-semantic-roles ()
  (should (equal (aaron-ui-token 'role-strong) "#EEF3FF"))
  (should (equal (aaron-ui-token 'role-salient) "#A9CBFF"))
  (should (equal (aaron-ui-token 'role-subtle) "#414B61"))
  (should (equal (aaron-ui-token 'space-4) "16px")))

(ert-deftest aaron-ui-rejects-circular-token-aliases ()
  (should-error
   (aaron-ui--resolve-color 'left '((left . right) (right . left)) nil)
   :type 'error))

(ert-deftest aaron-ui-noema-css-export-is-current ()
  (let ((file
         (expand-file-name
          "lisp/roam/Noema/src/styles/aaron-ui-tokens.css"
          user-emacs-directory)))
    (should (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-string)
                     (aaron-ui-css-tokens 'wave))))))

(provide 'aaron-ui-tests)
;;; aaron-ui-tests.el ends here
