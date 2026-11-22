;;; tools/agent/interaction-adapter-opencode.el --- OpenCode adapter for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers the OpenCode CLI tool spec with `+agent-interaction-cli' and exposes
;; thin public wrappers that the rest of +agent-interaction calls by name.
;; All session logic lives in +agent-interaction-cli.el.

;;; Code:

(require 'tools/agent/interaction-cli)

(defgroup +agent-interaction-opencode nil
  "OpenCode terminal integration for +agent-interaction."
  :group '+agent-interaction
  :prefix "+agent-interaction-opencode-")

(defcustom +agent-interaction-opencode-executable "opencode"
  "Path to the OpenCode executable."
  :type 'string
  :group '+agent-interaction-opencode)

(defcustom +agent-interaction-opencode-extra-args nil
  "Additional command line arguments passed to OpenCode."
  :type '(repeat string)
  :group '+agent-interaction-opencode)

(defcustom +agent-interaction-opencode-terminal-backend 'portal
  "Terminal backend used for OpenCode sessions."
  :type '(choice (const portal) (const eat))
  :group '+agent-interaction-opencode)

(define-minor-mode +agent-interaction-opencode-mode
  "Minor mode marker for +agent-interaction OpenCode terminal buffers."
  :init-value nil
  :lighter " AI-OpenCode")

;; ── Tool spec registration ────────────────────────────────────────────────────

(+agent-interaction-cli-register-tool 'opencode
  :name "OpenCode"
  :executable-var '+agent-interaction-opencode-executable
  :extra-args-var '+agent-interaction-opencode-extra-args
  :terminal-backend-var '+agent-interaction-opencode-terminal-backend
  :env-vars '("TERM_PROGRAM=emacs")
  :buffer-prefix "opencode"
  :popup-kind 'ai-opencode
  :minor-mode '+agent-interaction-opencode-mode
  :exec-args-fn
  (lambda (prompt _output-file _root)
    ;; `opencode run <message>' runs non-interactively and exits.
    (let ((exe (if (and (boundp '+agent-interaction-opencode-executable)
                        (stringp +agent-interaction-opencode-executable)
                        (not (string-empty-p +agent-interaction-opencode-executable)))
                   +agent-interaction-opencode-executable
                 "opencode")))
      (list exe "run" prompt)))
  :exec-output 'stdout)

;; ── Public wrappers ───────────────────────────────────────────────────────────

(defun +agent-interaction-opencode-available-p ()
  "Return non-nil when the OpenCode executable is available."
  (+agent-interaction-cli-available-p 'opencode))

(defun +agent-interaction-opencode-load ()
  "Validate the OpenCode executable and terminal backend."
  (unless (+agent-interaction-opencode-available-p)
    (error "OpenCode executable not found: %s" +agent-interaction-opencode-executable))
  (+agent-interaction-cli--ensure-terminal-backend 'opencode))

(defun +agent-interaction-opencode-buffer (&optional project-root)
  "Return the OpenCode session buffer for PROJECT-ROOT, or nil."
  (+agent-interaction-cli-buffer 'opencode project-root))

(defun +agent-interaction-opencode-session-live-p (&optional project-root)
  "Return non-nil when the OpenCode session for PROJECT-ROOT is live."
  (+agent-interaction-cli-session-live-p 'opencode project-root))

(defun +agent-interaction-opencode-ensure-session (&optional project-root)
  "Ensure a live OpenCode session exists for PROJECT-ROOT."
  (+agent-interaction-cli-ensure-session 'opencode project-root))

(defun +agent-interaction-opencode-open-buffer ()
  "Open the current project's OpenCode session buffer via popup."
  (interactive)
  (+agent-interaction-cli-open-buffer 'opencode (+agent-interaction-project-root)))

(defun +agent-interaction-opencode-prime-session (&optional project-root)
  "Inject the working directory and profile into OpenCode for PROJECT-ROOT."
  (+agent-interaction-cli-prime-session 'opencode project-root))

(defun +agent-interaction-opencode-send-prompt (prompt &optional project-root)
  "Send PROMPT to OpenCode, starting a session for PROJECT-ROOT when needed."
  (+agent-interaction-cli-send-prompt 'opencode prompt project-root))

(defun +agent-interaction-opencode-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into OpenCode without submitting for PROJECT-ROOT."
  (+agent-interaction-cli-draft-prompt 'opencode prompt project-root))

(defun +agent-interaction-opencode-stop (&optional project-root)
  "Stop the active OpenCode session for PROJECT-ROOT."
  (interactive)
  (+agent-interaction-cli-stop 'opencode project-root))

(provide 'tools/agent/interaction-adapter-opencode)
;;; tools/agent/interaction-adapter-opencode.el ends here
