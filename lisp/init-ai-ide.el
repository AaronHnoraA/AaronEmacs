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
;;     C-c A W     ai-workbench (open/select engine)
;;     C-c A .     ai-workbench-context-prompt
;;     C-c A w     ai-workbench-writing-prompt
;;     C-c A k     ai-workbench-kill
;;     C-c A i r/b/f send region / buffer / file via current backend
;;     C-c A H     ai-workbench-hub (management dashboard)
;;     C-c A ?     ai-workbench-docs-ask (:c CC, :o OpenCode, default Codex)
;;
;;   AI Engine chat (ai-workbench-engine + CLI agents via one-shot exec)
;;     C-c g       ai-workbench-chat  (open engine buffer backed by CLI agent)
;;     C-c A m     ai-workbench-menu  (engine transient menu)
;;     C-c A r     ai-workbench-rewrite  (rewrite region)
;;
;;   OpenCode (opencode) — opencode CLI agent
;;     C-c o t     toggle panel
;;     C-c o s/q   start / stop
;;     C-c o p/r/f send prompt / region / file

;;; Code:

(require 'init-package-utils)

(my/package-register-vc
 'claude-code-ide
 '(:url "https://github.com/manzaltu/claude-code-ide.el"
   :rev :last-release))
(my/package-register-vc
 'codex-cli
 '(:url "https://github.com/bennfocus/codex-cli.el"
   :rev :last-release))

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/ai-workbench")))
(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/ai-workbench/vendor/codex-cli")))
(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/ai-workbench/vendor/ai-workbench-engine")))

(autoload 'ai-workbench "ai-workbench" nil t)
(autoload 'ai-workbench-open "ai-workbench" nil t)
(autoload 'ai-workbench-kill "ai-workbench" nil t)
(autoload 'ai-workbench-send-region "ai-workbench" nil t)
(autoload 'ai-workbench-send-current-buffer "ai-workbench" nil t)
(autoload 'ai-workbench-send-file "ai-workbench" nil t)
(autoload 'ai-workbench-context-prompt "ai-workbench-tools" nil t)
(autoload 'ai-workbench-writing-prompt "ai-workbench-tools" nil t)
(autoload 'ai-workbench-docs-ask "ai-workbench-docs" nil t)
(autoload 'ai-workbench-chat "ai-workbench-chat" nil t)
(autoload 'ai-workbench-chat-open-buffer "ai-workbench-chat" nil t)
(autoload 'ai-workbench-hub "ai-workbench-hub" nil t)

;; Engine commands (from vendored ai-workbench-engine, loaded lazily via chat-load).
;; These autoloads ensure keybindings activate load before the engine is required.
(autoload 'ai-workbench-send "ai-workbench-engine" nil t)
(autoload 'ai-workbench-rewrite "ai-workbench-rewrite" nil t)
(autoload 'ai-workbench-menu "ai-workbench-transient" nil t)
(autoload 'ai-workbench-add "ai-workbench-context" nil t)

(defvar-keymap my/ai-workbench-prefix-map
  :doc "Prefix map for ai-workbench commands."
  "w" #'ai-workbench-writing-prompt
  "W" #'ai-workbench
  "." #'ai-workbench-context-prompt
  "?" #'ai-workbench-docs-ask
  "k" #'ai-workbench-kill
  "H" #'ai-workbench-hub
  "m" #'ai-workbench-menu
  "r" #'ai-workbench-rewrite
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

;; ── OpenCode ────────────────────────────────────────────────────────────────

(autoload 'ai-workbench-opencode-open-buffer "ai-workbench-adapter-opencode" nil t)
(autoload 'ai-workbench-opencode-send-prompt "ai-workbench-adapter-opencode" nil t)
(autoload 'ai-workbench-opencode-stop "ai-workbench-adapter-opencode" nil t)

(global-set-key (kbd "C-c o t") #'ai-workbench-opencode-open-buffer)
(global-set-key (kbd "C-c o s") #'ai-workbench-open)
(global-set-key (kbd "C-c o q") #'ai-workbench-opencode-stop)
(global-set-key (kbd "C-c o p") #'ai-workbench-context-prompt)
(global-set-key (kbd "C-c o r") #'ai-workbench-send-region)
(global-set-key (kbd "C-c o f") #'ai-workbench-send-file)

;; ── AI Engine chat (ai-workbench-engine + CLI) ──────────────────────────────

(global-set-key (kbd "C-c g") #'ai-workbench-chat)

;; Register CLI backends + restore persisted backend after Emacs finishes
;; initialising.  `after-init-hook' runs after all init files so transient
;; and other deps are guaranteed to be loaded by then.
(add-hook 'after-init-hook
          (lambda ()
            (condition-case err
                (when (require 'ai-workbench-vendor nil t)
                  (when (ai-workbench-vendor-package-present-p 'ai-workbench-engine)
                    (require 'ai-workbench-chat nil t)
                    (ai-workbench-chat-load)))
              (error (message "ai-workbench: init hook error: %s" err)))))

(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
