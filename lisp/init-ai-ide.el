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

;;; ── GitHub Copilot ────────────────────────────────────────────────────────

(declare-function copilot-server-executable "copilot" ())

(defun my/copilot-available-p ()
  "Return non-nil when Copilot can start in the current environment."
  (and (not buffer-read-only)
       (not (minibufferp))
       (ignore-errors
         (when-let* ((server (copilot-server-executable)))
           (file-exists-p server)))))

(defun my/copilot-mode-maybe-enable ()
  "Enable `copilot-mode' only when its server is available."
  (when (my/copilot-available-p)
    (copilot-mode 1)))

(use-package copilot
  :ensure t
  :hook ((prog-mode . my/copilot-mode-maybe-enable)
         (org-mode . my/copilot-mode-maybe-enable))
  :bind (:map copilot-mode-map
              ("M-]" . my/forward-delimiter-or-copilot-dwim)
              ("M-[" . my/backward-delimiter-or-snippet-dwim)
              ("M-(" . my/forward-delimiter-or-copilot-by-word-dwim)
              ("M-)" . my/forward-delimiter-or-copilot-to-char-dwim)
              ("M-}" . my/forward-delimiter-or-copilot-by-line-dwim))
  :config
  ;; Keep local Copilot keymaps aligned with the global DWIM policy:
  ;; snippet > Copilot > delimiter jump.
  (define-key copilot-completion-map (kbd "M-]") #'my/forward-delimiter-or-copilot-dwim)
  (define-key copilot-completion-map (kbd "M-[") #'my/backward-delimiter-or-snippet-dwim)
  (define-key copilot-completion-map (kbd "M-(") #'my/forward-delimiter-or-copilot-by-word-dwim)
  (define-key copilot-completion-map (kbd "M-)") #'my/forward-delimiter-or-copilot-to-char-dwim)
  (define-key copilot-completion-map (kbd "M-}") #'my/forward-delimiter-or-copilot-by-line-dwim))

(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
