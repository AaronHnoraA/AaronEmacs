;;; init-multiple-cursors.el --- Multi-cursor editing -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             mc/mark-all-like-this-dwim
             mc/skip-to-next-like-this
             mc/skip-to-previous-like-this)
  :init
  (setq mc/list-file (expand-file-name "var/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         :map evil-visual-state-map
         ("g n"         . mc/mark-next-like-this)
         ("g p"         . mc/mark-previous-like-this)
         ("g a"         . mc/mark-all-like-this-dwim)))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
