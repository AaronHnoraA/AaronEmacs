;;; init-evil-leader-shortcuts-tests.el --- Doom leader shortcuts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest evil-leader-doom-free-keys-resolve-in-normal-state ()
  (with-temp-buffer
    (text-mode)
    (evil-local-mode 1)
    (evil-normal-state)
    (dolist (binding '(("d" . debug-menu)
                       ("." . file-open)
                       ("," . +workspace/switch-buffer)
                       ("`" . evil-switch-to-windows-last-buffer)
                       ("*" . +search/symbol-at-point)
                       ("/" . +project/search-current)
                       ("x" . +scratch/toggle)
                       ("u" . universal-argument)
                       ("RET" . consult-bookmark)
                       ("~" . popper-toggle)))
      (should (commandp (cdr binding)))
      (should (eq (key-binding (kbd (concat "SPC " (car binding))))
                  (cdr binding))))))

(ert-deftest project-search-current-uses-current-root-and-directory-fallback ()
  (let ((default-directory "/tmp/")
        seen)
    (cl-letf (((symbol-function '+project-current-root)
               (lambda () "/tmp/project/"))
              ((symbol-function '+project/ripgrep)
               (lambda (root &rest _) (setq seen root))))
      (+project/search-current)
      (should (equal seen "/tmp/project/")))
    (cl-letf (((symbol-function '+project-current-root) (lambda () nil))
              ((symbol-function '+project/ripgrep)
               (lambda (root &rest _) (setq seen root))))
      (+project/search-current)
      (should (equal seen "/tmp/")))))

;;; init-evil-leader-shortcuts-tests.el ends here
