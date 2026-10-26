;;; init-editing-indent-tests.el --- Buffer indentation commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-editing-indent-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'editor/editing/config)

(ert-deftest +editing-set-indent-width-sets-the-mode-variables ()
  "The width reaches the mode's own indent variable and Evil's shift width."
  (with-temp-buffer
    (python-mode)
    (+editing/set-indent-width 2)
    (should (= python-indent-offset 2))
    (should (local-variable-p 'python-indent-offset))
    (when (featurep 'evil)
      (should (= evil-shift-width 2))))
  (with-temp-buffer
    (c-mode)
    (+editing/set-indent-width 8)
    (should (= c-basic-offset 8))))

(ert-deftest +editing-retab-follows-the-buffer-style ()
  "Retab converts to the buffer's style, or the other one with a prefix."
  (with-temp-buffer
    (setq-local tab-width 4 indent-tabs-mode nil)
    (insert "\tx\n")
    (+editing/retab nil nil nil)
    (should (equal (buffer-string) "    x\n"))
    (+editing/retab t nil nil)
    (should (equal (buffer-string) "\tx\n"))
    (+editing/toggle-indent-style)
    (should indent-tabs-mode)
    (should (local-variable-p 'indent-tabs-mode))))

;;; init-editing-indent-tests.el ends here
