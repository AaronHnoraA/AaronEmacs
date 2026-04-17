;;; init-ratex.el --- Local RaTeX package wiring -*- lexical-binding: t; -*-

;;; Code:

(use-package ratex
  :load-path "~/.emacs.d/site-lisp/ratex.el/lisp"
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.18
        ratex-edit-preview-scan-lines 2
        ratex-font-size 32.0
        ratex-inline-preview nil
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-render-color "#d8dee9"
        ratex-posframe-background-color "#2b3140"
        ratex-posframe-border-color "#5f6f8f")
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
