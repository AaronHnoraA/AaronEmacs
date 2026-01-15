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
          :models '(gemini-3-pro-thinking)))

    (setq gptel-model "gemini-3-pro-thinking")
    ;; preset：coding
      (gptel-make-preset 'gemini-coding
        :description "Coding preset"
        :backend "HiAPI"
        :model "gemini-3-pro-thinking"
        :system "You are an expert coding assistant. Provide correct, minimal, and maintainable code. Explain key decisions briefly."
        :tools nil) 
    :custom
      (gptel-default-mode 'markdown-mode)
      (gptel-model 'gemini-3-pro-thinking)
      (gptel-rewrite 'gemini-3-pro-thinking)
    )

(provide 'init-gpt)



;;; init-gpt.el ends here
;;;

