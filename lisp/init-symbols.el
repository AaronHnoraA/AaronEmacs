;;; init-symbols.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function consult-imenu "consult-imenu")
(declare-function consult-imenu-multi "consult-imenu" (&optional query))

(defun my/symbols-buffer ()
  "Search symbols in the current buffer."
  (interactive)
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (imenu nil)))

(defun my/symbols-project ()
  "Search symbols across current-project peer buffers."
  (interactive)
  (if (fboundp 'consult-imenu-multi)
      (consult-imenu-multi)
    (my/symbols-buffer)))

(my/evil-global-leader-set "s b" #'my/symbols-buffer "buffer symbols")
(my/evil-global-leader-set "s I" #'my/symbols-project "project symbols")

(provide 'init-symbols)
;;; init-symbols.el ends here
