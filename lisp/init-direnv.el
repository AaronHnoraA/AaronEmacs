;;; init-direnv.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defcustom my/enable-direnv nil
  "Whether to enable `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(use-package direnv
  :ensure t
  :if my/enable-direnv
  :hook (after-init . direnv-mode))

(provide 'init-direnv)



;;; init-base.el ends here
