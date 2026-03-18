;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;  Loads GPT configuration from var/mygpt.json

;;; Code:

(require 'json)

(defvar gptel-backend)
(defvar gptel-model)

(declare-function gptel-make-openai "gptel")
(declare-function gptel-make-preset "gptel")
(declare-function copilot--request "copilot")
(declare-function copilot-login "copilot")
(declare-function copilot-logout "copilot")
(declare-function copilot-accept-completion "copilot")
(declare-function copilot-accept-completion-to-char "copilot")

(defun my/gptel-load-config ()
  "Load GPT backend settings from `var/mygpt.json'."
  (let ((gpt-config-file (expand-file-name "var/mygpt.json" user-emacs-directory)))
    (if (file-exists-p gpt-config-file)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (config-data (json-read-file gpt-config-file))
               (host (alist-get 'host config-data))
               (endpoint (alist-get 'endpoint config-data))
               (key (alist-get 'key config-data))
               (model (alist-get 'model config-data))
               (backend-name (alist-get 'backend_name config-data)))
          (setq gptel-backend
                (gptel-make-openai backend-name
                  :host host
                  :endpoint endpoint
                  :stream t
                  :key key
                  :models (list model)))
          (setq gptel-model model)
          (gptel-make-preset 'gemini-coding
            :description "Coding preset"
            :backend backend-name
            :model model
            :system "You are an expert coding assistant. Provide correct, minimal, and maintainable code. Explain key decisions briefly."
            :tools nil))
      (unless noninteractive
        (display-warning 'init-gpt "GPT config file (var/mygpt.json) not found!" :warning)))))

(defun my/copilot-check-status ()
  "Report current `copilot.el' authentication status.

Compatibility wrapper for old `lsp-copilot-check-status' workflows."
  (interactive)
  (let* ((response (copilot--request 'checkStatus nil))
         (status (plist-get response :status))
         (user (plist-get response :user)))
    (message "%s"
             (cond
              ((and (stringp user) (not (string-empty-p user)))
               (format "Copilot is signed in as %s%s"
                       user
                       (if (and (stringp status) (not (string-empty-p status)))
                           (format " [%s]" status)
                         "")))
              ((and (stringp status) (not (string-empty-p status)))
               (format "Copilot status: %s" status))
              (t
               (format "Copilot status response: %S" response))))))

(defun my/copilot-check-quota ()
  "Report quota or entitlement information from the Copilot server."
  (interactive)
  (message "Copilot quota: %S" (copilot--request 'checkQuota nil)))

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu gptel-rewrite)
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-abort))
  :init
  (setq gptel-default-mode 'markdown-mode)
  :config
  (my/gptel-load-config)
  ;; Note: gptel-rewrite is usually set to a model name, setting it here to match your JSON
  (setq gptel-rewrite gptel-model))


(use-package copilot
  :ensure t
  :hook ((prog-mode . copilot-mode)
         (org-mode . copilot-mode))
  :custom
  (copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory))
  (copilot-idle-delay 0.30)
  (copilot-indent-offset-warning-disable t)
  (copilot-lsp-settings '(:github (:copilot ())))
  :config
  (when (and (fboundp 'my/copilot--suppress-cancelled-errors)
             (advice-member-p #'my/copilot--suppress-cancelled-errors 'copilot--log))
    (advice-remove 'copilot--log #'my/copilot--suppress-cancelled-errors))
  (defalias 'lsp-copilot-check-status #'my/copilot-check-status)
  (defalias 'lsp-copilot-login #'copilot-login)
  (defalias 'lsp-copilot-logout #'copilot-logout)
  (define-key copilot-completion-map (kbd "M-]") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-}") #'copilot-accept-completion-to-char))





;{
;  "host": "hiapi.online",
;  "endpoint": "/v1/chat/completions",
;  "key": "sk-",
;  "model": "....",
;  "backend_name": "HiAPI"
;}


(provide 'init-gpt)
;;; init-gpt.el ends here
