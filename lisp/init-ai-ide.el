;;; init-ai-ide.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Manages AI coding assistants:
;;
;;   Claude Code (claude-code-ide)  — full agentic coding via Claude CLI
;;     C-c C-'   claude-code-ide-menu  (transient menu)
;;     C-c a     claude-code-ide-menu
;;     F12       claude-code-ide-menu  (global function key, set in init-function-keys.el)
;;     H-l       claude-code-ide-menu  (macOS Option shortcut, set in init-macos.el)
;;
;;   Codex CLI (codex-cli)  — OpenAI Codex terminal assistant
;;     C-c c t   toggle panel
;;     C-c c s/q start / stop
;;     C-c c p/r/f  send prompt / region / file
;;     C-c c a/n/b  show-all / next page / prev page

;;; Code:

;;; ── Claude Code ────────────────────────────────────────────────────────────

(my/package-ensure-vc 'claude-code-ide "https://github.com/manzaltu/claude-code-ide.el")

(use-package claude-code-ide
  :ensure nil
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c a"   . claude-code-ide-menu))
  :init
  ;; Point to the Claude CLI binary (installed via npm / brew / manual).
  (setq claude-code-ide-cli-path "/Users/hc/.local/bin/claude"
        claude-code-ide-window-side 'bottom
        claude-code-ide-window-height 18)
  :config
  ;; Expose Emacs MCP tools so Claude can read/edit buffers, eval Elisp, etc.
  (claude-code-ide-emacs-tools-setup))

;;; ── Codex CLI ──────────────────────────────────────────────────────────────

(my/package-ensure-vc 'codex-cli "https://github.com/bennfocus/codex-cli.el")

(use-package codex-cli
  :ensure nil
  :bind (("C-c c t" . codex-cli-toggle)
         ("C-c c s" . codex-cli-start)
         ("C-c c q" . codex-cli-stop)
         ("C-c c Q" . codex-cli-stop-all)
         ("C-c c p" . codex-cli-send-prompt)
         ("C-c c r" . codex-cli-send-region)
         ("C-c c f" . codex-cli-send-file)
         ("C-c c a" . codex-cli-toggle-all)
         ("C-c c n" . codex-cli-toggle-all-next-page)
         ("C-c c b" . codex-cli-toggle-all-prev-page))
  :init
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90)
  :config
  (defun my/codex-cli-setup-bottom-window (buffer)
    "Display BUFFER in a splittable bottom window."
    (display-buffer-at-bottom
     buffer
     '((window-height . 18)
       (dedicated . nil))))
  (advice-add 'codex-cli--setup-side-window :override
              #'my/codex-cli-setup-bottom-window))


(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
