;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(use-package gptel
  :ensure t
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
         ("C-c C-g" . gptel-abort))
  :config
  ;; ---- Backend: HiAPI (OpenAI-compatible) ----
  (setq gptel-backend
        (gptel-make-openai "HiAPI"
          ;; 对应你截图的 https://hiapi.online/v1
          :host "hiapi.online"
          :endpoint "/v1/chat/completions"
          :stream t
          ;; 从 auth-source 取 Bearer token
          :key "sk-6CSNDcbV9Dm7OkMAf3TXhbZULA3riM2nO3dutj4sAXJ9Irxi"
          :models '(gemini-3-pro-preview)))

    (setq gptel-model "gemini-3-pro-preview")
    ;; preset：coding
      (gptel-make-preset 'gemini-coding
        :description "Coding preset"
        :backend "HiAPI"
        :model "gemini-3-pro-preview"
        :system "You are an expert coding assistant. Provide correct, minimal, and maintainable code. Explain key decisions briefly."
        :tools nil) 
    :custom
      (gptel-default-mode 'markdown-mode)
      (gptel-model 'gemini-3-pro-preview)
      (gptel-rewrite 'gemini-3-pro-preview)
    )

;; (use-package aider
;;   :ensure t
;;   :config
;;   ;; For latest claude sonnet model
;;   (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect")) ;; add --no-auto-commits if you don't want it
;;   (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   ;; Or chatgpt model
;;   ;; (setq aider-args '("--model" "o4-mini"))
;;   ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;   ;; Or use your personal config file
;;   ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
;;   ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
;;   (aider-magit-setup-transients) ;; add aider magit function to magit menu
;;   ;; auto revert buffer
;;   (global-auto-revert-mode 1)
;;   (auto-revert-mode 1))


(use-package copilot
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "M-]") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-}") #'copilot-accept-completion-to-char))


(add-hook 'org-mode-hook #'copilot-mode)


(provide 'init-gpt)



;;; init-gpt.el ends here
;;;

