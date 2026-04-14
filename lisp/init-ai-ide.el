;;; init-zoxide.el --- zoxide integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Extra helpers built on top of `zoxide.el'.

;;; Code:


(my/package-ensure-vc 'claude-code-ide "https://github.com/manzaltu/claude-code-ide.el")

(use-package claude-code-ide
	:ensure nil
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
	(setq claude-code-ide-cli-path "/Users/hc/.local/bin/claude")
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'init-ai-ide)
;;; init-zoxide.el ends here
