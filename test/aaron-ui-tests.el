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

(ert-deftest aaron-ui-theme-switch-keeps-interface-colors-in-sync ()
  "The light and dark variants must style UI surfaces from their own palettes."
  (let ((previous-themes custom-enabled-themes)
        (previous-variant aaron-ui-current-variant)
        (previous-colors kanagawa-themes-custom-colors))
    (unwind-protect
        (dolist (variant '(wave dragon lotus))
          (aaron-ui-load-theme variant)
          (should (equal (face-background 'default nil t)
                         (aaron-ui-color 'surface-base)))
          (should (equal (face-background 'tab-bar nil t)
                         (aaron-ui-color 'bg-m1)))
          (should (equal (face-background 'mode-line nil t)
                         (aaron-ui-color 'bg-p1)))
          (should (string-match-p
                   (if (eq variant 'lotus) "color-scheme: light" "color-scheme: dark")
                   (aaron-ui-css-tokens variant))))
      (mapc #'disable-theme custom-enabled-themes)
      (setq aaron-ui-current-variant previous-variant
            kanagawa-themes-custom-colors previous-colors)
      (dolist (theme (reverse previous-themes))
        (enable-theme theme)))))

(ert-deftest aaron-ui-noema-css-export-is-current ()
  (let ((file
         (expand-file-name
          "site-lisp/noema/src/styles/aaron-ui-tokens.css"
          user-emacs-directory)))
    (should (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-string)
                     (aaron-ui-css-tokens 'wave))))))

(provide 'aaron-ui-tests)
;;; aaron-ui-tests.el ends here
