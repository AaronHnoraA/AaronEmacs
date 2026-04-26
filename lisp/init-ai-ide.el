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
;;
;;   AI Workbench (ai-workbench) — unified Emacs-native entry layer
;;     M-x ai-workbench               open backend interactive buffer
;;     M-x ai-workbench-compose-buffer open compose buffer
;;     C-c M-a     ai-workbench-context-prompt
;;     C-c A W     ai-workbench
;;     C-c A .     ai-workbench-context-prompt
;;     C-c A /     ai-workbench-context-preview
;;     C-c A w     ai-workbench-writing-prompt
;;     C-c A k     ai-workbench-kill
;;     C-c A m     ai-workbench-compose-buffer
;;     C-c A i r/b/f send region / buffer / file via current backend

;;; Code:

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/ai-workbench")))

(require 'ai-workbench)
(require 'ai-workbench-vendor)
(ai-workbench-setup-vendor-load-paths)

(defvar-keymap my/ai-workbench-prefix-map
  :doc "Prefix map for ai-workbench commands."
  "w" #'ai-workbench-writing-prompt
  "W" #'ai-workbench
  "." #'ai-workbench-context-prompt
  "/" #'ai-workbench-context-preview
  "k" #'ai-workbench-kill
  "m" #'ai-workbench-compose-buffer
  "i r" #'ai-workbench-send-region
  "i b" #'ai-workbench-send-current-buffer
  "i f" #'ai-workbench-send-file)

(global-set-key (kbd "C-c M-a") #'ai-workbench-context-prompt)
(global-set-key (kbd "C-c A") my/ai-workbench-prefix-map)

;;; ── Claude Code ────────────────────────────────────────────────────────────

(use-package claude-code-ide
  :ensure nil
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c a"   . claude-code-ide-menu))
  :init
  ;; Point to the Claude CLI binary (installed via npm / brew / manual).
  (setq claude-code-ide-cli-path "/Users/hc/.local/bin/claude"
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 90)
  :config
  ;; Expose Emacs MCP tools so Claude can read/edit buffers, eval Elisp, etc.
  (claude-code-ide-emacs-tools-setup))

;;; ── Codex CLI ──────────────────────────────────────────────────────────────

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
  :config)


(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
