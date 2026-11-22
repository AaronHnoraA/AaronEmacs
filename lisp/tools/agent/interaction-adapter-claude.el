;;; tools/agent/interaction-adapter-claude.el --- Claude CLI through Portal -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'tools/agent/interaction-cli)

(defgroup +agent-interaction-claude nil
  "Claude terminal integration for +agent-interaction."
  :group '+agent-interaction)

(defcustom +agent-interaction-claude-executable "claude"
  "Path to the Claude CLI."
  :type 'string
  :group '+agent-interaction-claude)

(defcustom +agent-interaction-claude-extra-args nil
  "Additional arguments for interactive Claude sessions."
  :type '(repeat string)
  :group '+agent-interaction-claude)

(define-minor-mode +agent-interaction-claude-mode
  "Mark a Noema Claude Portal buffer."
  :init-value nil
  :lighter " AI-Claude")

(+agent-interaction-cli-register-tool 'claude
  :name "CC – Claude Code"
  :executable-var '+agent-interaction-claude-executable
  :extra-args-var '+agent-interaction-claude-extra-args
  :env-vars '("TERM_PROGRAM=emacs")
  :buffer-prefix "claude"
  :popup-kind 'ai-claude
  :minor-mode '+agent-interaction-claude-mode
  :exec-args-fn
  (lambda (prompt _output-file _root)
    (list +agent-interaction-claude-executable
          "-p" "--output-format" "text" prompt))
  :exec-output 'stdout)

(defun +agent-interaction-claude-available-p ()
  "Return non-nil when the Claude CLI is available."
  (+agent-interaction-cli-available-p 'claude))

(defun +agent-interaction-load-claude ()
  "Validate the Claude CLI and Portal backend."
  (unless (+agent-interaction-claude-available-p)
    (user-error "Claude executable not found: %s" +agent-interaction-claude-executable))
  (+agent-interaction-cli--ensure-terminal-backend 'claude))

(defun +agent-interaction-claude-buffer (&optional project-root)
  "Return the Claude session buffer for PROJECT-ROOT."
  (+agent-interaction-cli-buffer 'claude project-root))

(defun +agent-interaction-claude-open-buffer ()
  "Open the current project's Claude session."
  (interactive)
  (+agent-interaction-cli-open-buffer 'claude (+agent-interaction-project-root)))

(defun +agent-interaction-claude-stop (&optional project-root)
  "Stop the Claude session for PROJECT-ROOT."
  (interactive)
  (+agent-interaction-cli-stop 'claude project-root))

(defun +agent-interaction-claude-session-live-p (&optional project-root)
  "Return non-nil when Claude is running for PROJECT-ROOT."
  (+agent-interaction-cli-session-live-p 'claude project-root))

(defun +agent-interaction-claude-ensure-session (&optional project-root)
  "Ensure Claude is running for PROJECT-ROOT."
  (+agent-interaction-cli-ensure-session 'claude project-root))

(defun +agent-interaction-claude-prime-session (&optional project-root)
  "Send the project profile to Claude for PROJECT-ROOT."
  (+agent-interaction-cli-prime-session 'claude project-root))

(defun +agent-interaction-claude-send-prompt (prompt &optional project-root)
  "Send PROMPT to Claude for PROJECT-ROOT."
  (+agent-interaction-cli-send-prompt 'claude prompt project-root))

(defun +agent-interaction-claude-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into Claude for PROJECT-ROOT without submitting."
  (+agent-interaction-cli-draft-prompt 'claude prompt project-root))

(provide 'tools/agent/interaction-adapter-claude)
;;; tools/agent/interaction-adapter-claude.el ends here
