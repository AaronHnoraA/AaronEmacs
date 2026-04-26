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
;;     C-c M-a     ai-workbench-context-prompt
;;     C-c A W     ai-workbench
;;     C-c A .     ai-workbench-context-prompt
;;     C-c A w     ai-workbench-writing-prompt
;;     C-c A k     ai-workbench-kill
;;     C-c A i r/b/f send region / buffer / file via current backend

;;; Code:

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/ai-workbench")))

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file
               "site-lisp/ai-workbench/vendor/claude-code-ide")))
(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file
               "site-lisp/ai-workbench/vendor/codex-cli")))

(autoload 'ai-workbench "ai-workbench" nil t)
(autoload 'ai-workbench-open "ai-workbench" nil t)
(autoload 'ai-workbench-kill "ai-workbench" nil t)
(autoload 'ai-workbench-send-region "ai-workbench" nil t)
(autoload 'ai-workbench-send-current-buffer "ai-workbench" nil t)
(autoload 'ai-workbench-send-file "ai-workbench" nil t)
(autoload 'ai-workbench-context-prompt "ai-workbench-tools" nil t)
(autoload 'ai-workbench-writing-prompt "ai-workbench-tools" nil t)

(defvar-keymap my/ai-workbench-prefix-map
  :doc "Prefix map for ai-workbench commands."
  "w" #'ai-workbench-writing-prompt
  "W" #'ai-workbench
  "." #'ai-workbench-context-prompt
  "k" #'ai-workbench-kill
  "i r" #'ai-workbench-send-region
  "i b" #'ai-workbench-send-current-buffer
  "i f" #'ai-workbench-send-file)

(global-set-key (kbd "C-c M-a") #'ai-workbench-context-prompt)
(global-set-key (kbd "C-c A") my/ai-workbench-prefix-map)

;;; ── Claude Code ────────────────────────────────────────────────────────────

(defvar claude-code-ide-cli-path)
(defvar claude-code-ide-window-side)
(defvar claude-code-ide-window-width)

(declare-function claude-code-ide-emacs-tools-setup
                  "claude-code-ide-emacs-tools" ())

(autoload 'claude-code-ide-menu "claude-code-ide-transient" nil t)
(autoload 'claude-code-ide "claude-code-ide" nil t)

;; Point to the Claude CLI binary (installed via npm / brew / manual).
(setq claude-code-ide-cli-path "/Users/hc/.local/bin/claude"
      claude-code-ide-window-side 'right
      claude-code-ide-window-width 90)

(global-set-key (kbd "C-c C-'") #'claude-code-ide-menu)
(global-set-key (kbd "C-c a") #'claude-code-ide-menu)

(with-eval-after-load 'claude-code-ide
  ;; Expose Emacs MCP tools so Claude can read/edit buffers, eval Elisp, etc.
  (require 'claude-code-ide-emacs-tools)
  (claude-code-ide-emacs-tools-setup))

;;; ── Codex CLI ──────────────────────────────────────────────────────────────

(defvar codex-cli-executable)
(defvar codex-cli-terminal-backend)
(defvar codex-cli-side)
(defvar codex-cli-width)

(autoload 'codex-cli-toggle "codex-cli" nil t)
(autoload 'codex-cli-start "codex-cli" nil t)
(autoload 'codex-cli-stop "codex-cli" nil t)
(autoload 'codex-cli-stop-all "codex-cli" nil t)
(autoload 'codex-cli-send-prompt "codex-cli" nil t)
(autoload 'codex-cli-send-region "codex-cli" nil t)
(autoload 'codex-cli-send-file "codex-cli" nil t)
(autoload 'codex-cli-toggle-all "codex-cli" nil t)
(autoload 'codex-cli-toggle-all-next-page "codex-cli" nil t)
(autoload 'codex-cli-toggle-all-prev-page "codex-cli" nil t)

(setq codex-cli-executable "codex"
      codex-cli-terminal-backend 'vterm
      codex-cli-side 'right
      codex-cli-width 90)

(global-set-key (kbd "C-c c t") #'codex-cli-toggle)
(global-set-key (kbd "C-c c s") #'codex-cli-start)
(global-set-key (kbd "C-c c q") #'codex-cli-stop)
(global-set-key (kbd "C-c c Q") #'codex-cli-stop-all)
(global-set-key (kbd "C-c c p") #'codex-cli-send-prompt)
(global-set-key (kbd "C-c c r") #'codex-cli-send-region)
(global-set-key (kbd "C-c c f") #'codex-cli-send-file)
(global-set-key (kbd "C-c c a") #'codex-cli-toggle-all)
(global-set-key (kbd "C-c c n") #'codex-cli-toggle-all-next-page)
(global-set-key (kbd "C-c c b") #'codex-cli-toggle-all-prev-page)


(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
