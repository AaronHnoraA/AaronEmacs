;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:



(use-package smartparens
  :ensure t
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(provide 'init-smartparens)



;;; init-base.el ends here
