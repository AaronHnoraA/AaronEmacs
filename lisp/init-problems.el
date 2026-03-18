;;; init-problems.el --- Diagnostics pickers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function consult-flymake "consult-flymake" (&optional project))

(defun my/problems-buffer ()
  "Show diagnostics for the current buffer."
  (interactive)
  (unless (bound-and-true-p flymake-mode)
    (user-error "Flymake is not active in the current buffer"))
  (if (fboundp 'consult-flymake)
      (consult-flymake)
    (flymake-show-buffer-diagnostics)))

(defun my/problems-project ()
  "Show diagnostics for the current project."
  (interactive)
  (unless (bound-and-true-p flymake-mode)
    (user-error "Flymake is not active in the current buffer"))
  (if (fboundp 'consult-flymake)
      (consult-flymake t)
    (flymake-show-buffer-diagnostics)))

(my/evil-global-leader-set "c !" #'my/problems-buffer "problems")
(my/evil-global-leader-set "c ?" #'my/problems-project "project problems")

(provide 'init-problems)
;;; init-problems.el ends here
