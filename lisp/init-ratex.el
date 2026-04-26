;;; init-ratex.el --- Local RaTeX package wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'aaron-ui)

(use-package ratex
  :load-path "~/.emacs.d/site-lisp/ratex.el/lisp"
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.30
        ratex-edit-preview-scan-lines 1
        ratex-font-size 32.0
        ratex-inline-preview nil
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-render-color (aaron-ui-color 'fg-soft)
        ratex-posframe-background-color (aaron-ui-color 'bg-ratex)
        ratex-posframe-border-color (aaron-ui-color 'border-ratex))
  :hook ((org-mode . ratex-turn-on)
         (latex-mode . ratex-turn-on)
         (LaTeX-mode . ratex-turn-on)
         (tex-mode . ratex-turn-on)
         (TeX-mode . ratex-turn-on)
         (markdown-mode . ratex-turn-on)
         (plain-tex-mode . ratex-turn-on)
         (plain-TeX-mode . ratex-turn-on)
         (docTeX-mode . ratex-turn-on)))

(provide 'init-ratex)
;;; init-ratex.el ends here
