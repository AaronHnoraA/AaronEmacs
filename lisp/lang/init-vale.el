;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package vale-mode
  :ensure t
  :custom
  (vale-interact-path "/PATH/TO/interact.py")
  :mode ("\\.vaf\\'" . vale-mode))


(unless (package-installed-p 'vale)
  (package-vc-install
   '(vale
     :url "https://github.com/tpeacock19/flymake-vale.git"
     :rev :last-release)))
(require 'flymake-vale)

(add-hook 'text-mode-hook #'flymake-vale-load)
(add-hook 'latex-mode-hook #'flymake-vale-load)
(add-hook 'org-mode-hook #'flymake-vale-load)
(add-hook 'markdown-mode-hook #'flymake-vale-load)
(add-hook 'message-mode-hook #'flymake-vale-load)

(add-hook 'find-file-hook 'flymake-vale-maybe-load)
;; flymake-vale-modes defaults to: 
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)

(add-to-list 'flymake-vale-modes 'adoc-mode)




(provide 'init-vale)

;;; init-elisp.el ends here
