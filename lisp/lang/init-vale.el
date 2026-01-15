;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package vale-mode
  :ensure t
  :custom
  (vale-interact-path "/PATH/TO/interact.py")
  :mode ("\\.vaf\\'" . vale-mode))

(provide 'init-vale)

;;; init-elisp.el ends here
