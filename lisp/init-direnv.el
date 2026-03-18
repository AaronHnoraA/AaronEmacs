;;; init-direnv.el --- Project environment integration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function direnv-update-environment "direnv" (&optional file-name force-summary))

(defcustom my/enable-direnv t
  "Whether to enable `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(defun my/direnv-update-environment-maybe (&optional path)
  "Refresh the buffer environment from direnv for PATH or the current buffer."
  (when (and my/enable-direnv
             (or (fboundp 'direnv-update-environment)
                 (require 'direnv nil t)))
    (let ((target (or path buffer-file-name default-directory)))
      (when (and target
                 (not (file-remote-p target)))
        (let ((direnv-always-show-summary nil))
          (ignore-errors
            (direnv-update-environment target nil)))))))

(use-package direnv
  :ensure t
  :if my/enable-direnv
  :demand t
  :hook (find-file . my/direnv-update-environment-maybe)
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode 1))

(provide 'init-direnv)

;;; init-direnv.el ends here
