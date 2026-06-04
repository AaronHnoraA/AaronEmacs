;;; init-format.el --- Formatting helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/language-server-format-buffer "init-lsp")

(defvar-local my/format-on-save-mode nil
  "Whether current buffer should format itself before save.")

(defun my/format-buffer-dwim ()
  "Format the current buffer when a language server is active."
  (interactive)
  (unless (and (fboundp 'my/current-language-server-backend)
               (my/current-language-server-backend))
    (user-error "No active language server in the current buffer"))
  (my/language-server-format-buffer))

(defun my/format-buffer-maybe ()
  "Format the current buffer when `my/format-on-save-mode' is enabled."
  (when (and my/format-on-save-mode
             (fboundp 'my/current-language-server-backend)
             (my/current-language-server-backend))
    (condition-case err
        (my/language-server-format-buffer)
      (error
       (message "Format-on-save skipped: %s" (error-message-string err))))))

(define-minor-mode my/format-on-save-mode
  "Buffer-local format-on-save toggle."
  :init-value nil
  :lighter " FmtSave"
  (if my/format-on-save-mode
      (add-hook 'before-save-hook #'my/format-buffer-maybe nil t)
    (remove-hook 'before-save-hook #'my/format-buffer-maybe t)))

(defun my/format-toggle-on-save ()
  "Toggle buffer-local format-on-save."
  (interactive)
  (if my/format-on-save-mode
      (my/format-on-save-mode -1)
    (my/format-on-save-mode 1))
  (message "Format-on-save %s" (if my/format-on-save-mode "enabled" "disabled")))

(use-package ws-butler
  :ensure t
  :hook (after-init . ws-butler-global-mode)
  :config
  (dolist (mode '(special-mode
                  comint-mode
                  term-mode
                  eshell-mode
                  diff-mode))
    (add-to-list 'ws-butler-global-exempt-modes mode)))

(my/leader!
  "c F" '(:def my/format-toggle-on-save :which-key "format on save"))

(provide 'init-format)
;;; init-format.el ends here
