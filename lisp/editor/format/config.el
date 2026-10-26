;;; editor/format/config.el --- Formatting helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)

(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +language-server/format-buffer "tools/lsp/config")

(defvar-local +format-on-save-mode nil
  "Whether current buffer should format itself before save.")

(defun +format/buffer-dwim ()
  "Format the current buffer when a language server is active."
  (interactive)
  (unless (and (fboundp '+current-language-server-backend)
               (+current-language-server-backend))
    (user-error "No active language server in the current buffer"))
  (+language-server/format-buffer))

(defun +format-buffer-maybe ()
  "Format the current buffer when `+format-on-save-mode' is enabled."
  (when (and +format-on-save-mode
             (fboundp '+current-language-server-backend)
             (+current-language-server-backend))
    (condition-case err
        (+language-server/format-buffer)
      (error
       (message "Format-on-save skipped: %s" (error-message-string err))))))

(define-minor-mode +format-on-save-mode
  "Buffer-local format-on-save toggle."
  :init-value nil
  :lighter " FmtSave"
  (if +format-on-save-mode
      (add-hook 'before-save-hook #'+format-buffer-maybe nil t)
    (remove-hook 'before-save-hook #'+format-buffer-maybe t)))

(defun +format/toggle-on-save ()
  "Toggle buffer-local format-on-save."
  (interactive)
  (if +format-on-save-mode
      (+format-on-save-mode -1)
    (+format-on-save-mode 1))
  (message "Format-on-save %s" (if +format-on-save-mode "enabled" "disabled")))

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

(define-leader-key!
  "c F" '(:def +format/toggle-on-save :which-key "format on save"))

(provide 'editor/format/config)
;;; editor/format/config.el ends here
