;;; editor/snippets/config.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-char)        ; 跳到某个字符
   ("C-:" . avy-goto-char-2)      ; 跳到两个字符
   ("C-'" . avy-goto-word-1)      ; 跳到词开头
  )
)
(provide 'editor/avy/config)



;;; core/aaron-base.el ends here
