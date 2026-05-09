;;; init-previewer.el --- Realtime text preview -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(let ((previewer-dir (locate-user-emacs-file "site-lisp/previewer")))
  (when (file-directory-p previewer-dir)
    (add-to-list 'load-path previewer-dir)))

(use-package previewer
  :ensure nil
  :commands (previewer-mode previewer-cleanup)
  :custom
  (previewer-render-modes
   '(org-mode markdown-mode markdown-ts-mode html-mode html-ts-mode mhtml-mode web-mode vue-html-mode)))

(provide 'init-previewer)
;;; init-previewer.el ends here
