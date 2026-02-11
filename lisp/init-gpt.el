;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;  Loads GPT configuration from var/mygpt.json

;;; Code:

(require 'json)

(use-package gptel
  :ensure t
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-abort))
  :config
  ;; Define path to config file: ~/.emacs.d/var/mygpt.json
  (let ((gpt-config-file (expand-file-name "var/mygpt.json" user-emacs-directory)))
    
    (if (file-exists-p gpt-config-file)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               ;; Read the JSON file
               (config-data (json-read-file gpt-config-file))
               ;; Extract variables
               (host (alist-get 'host config-data))
               (endpoint (alist-get 'endpoint config-data))
               (key (alist-get 'key config-data))
               (model (alist-get 'model config-data))
               (backend-name (alist-get 'backend_name config-data)))

          ;; 1. Configure Backend
          (setq gptel-backend
                (gptel-make-openai backend-name
                  :host host
                  :endpoint endpoint
                  :stream t
                  :key key
                  :models (list model)))

          ;; 2. Set Default Model
          (setq gptel-model model)

          ;; 3. Create Coding Preset
          (gptel-make-preset 'gemini-coding
            :description "Coding preset"
            :backend backend-name
            :model model
            :system "You are an expert coding assistant. Provide correct, minimal, and maintainable code. Explain key decisions briefly."
            :tools nil))
      
      ;; Error handling if file is missing
      (display-warning 'init-gpt "GPT config file (var/mygpt.json) not found!" :warning)))

  ;; General settings
  (setq gptel-default-mode 'markdown-mode)
  ;; Note: gptel-rewrite is usually set to a model name, setting it here to match your JSON
  (setq gptel-rewrite gptel-model))


(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "M-]") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-}") #'copilot-accept-completion-to-char))

(add-hook 'org-mode-hook #'copilot-mode)

(provide 'init-gpt)
;;; init-gpt.el ends here
