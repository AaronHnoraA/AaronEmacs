;;; init-direnv.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package direnv
  :ensure t
  :defer 1
  :config
  (direnv-mode 1))

(provide 'init-direnv)



;;; init-base.el ends here
