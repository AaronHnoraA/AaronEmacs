;;; init-patches-tests.el --- Compatibility patch tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -l test/init-patches-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'patches/init-patches)

(ert-deftest my/patch-derived-type-registers-legacy-properties ()
  (let* ((name (make-symbol "my-test-derived-type"))
         (expander (lambda () '(satisfies integerp))))
    (cl--define-derived-type name expander #'integerp '(integer))
    (should (eq (get name 'cl-deftype-handler) expander))
    (should (eq (get name 'cl-deftype-satisfies) #'integerp))))

(ert-deftest my/patch-globalized-mode-buffer-lists-are-bound ()
  (dolist (variable '(global-atomic-chrome-edit-mode-buffers
                      better-jumper-mode-buffers))
    (should (boundp variable))
    (should (listp (symbol-value variable)))))

(provide 'init-patches-tests)
;;; init-patches-tests.el ends here
