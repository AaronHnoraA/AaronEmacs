;;; tools/agent/interaction-adapter-codex.el --- Codex adapter for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers the Codex CLI tool spec with `+agent-interaction-cli' and exposes
;; thin public wrappers that the rest of +agent-interaction calls by name.
;; All session logic lives in +agent-interaction-cli.el.

;;; Code:

(require 'tools/agent/interaction-cli)

(defgroup +agent-interaction-codex nil
  "Codex terminal integration for +agent-interaction."
  :group '+agent-interaction
  :prefix "+agent-interaction-codex-")

(defcustom +agent-interaction-codex-executable "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group '+agent-interaction-codex)

(defcustom +agent-interaction-codex-extra-args nil
  "Additional command line arguments passed to Codex."
  :type '(repeat string)
  :group '+agent-interaction-codex)

(defcustom +agent-interaction-codex-terminal-backend 'portal
  "Terminal backend used for Codex sessions."
  :type '(choice (const portal) (const eat))
  :group '+agent-interaction-codex)

(defvar +agent-interaction-codex-use-exec nil
  "Deprecated toggle kept for compatibility. Codex defaults to terminal mode.")

(define-minor-mode +agent-interaction-codex-mode
  "Minor mode marker for +agent-interaction Codex terminal buffers."
  :init-value nil
  :lighter " AI-Codex")

;; ── Tool spec registration ────────────────────────────────────────────────────

(+agent-interaction-cli-register-tool 'codex
  :name "Codex CLI"
  :executable-var '+agent-interaction-codex-executable
  :extra-args-var '+agent-interaction-codex-extra-args
  :terminal-backend-var '+agent-interaction-codex-terminal-backend
  :env-vars '("TERM_PROGRAM=emacs" "FORCE_CODE_TERMINAL=true")
  :buffer-prefix "codex"
  :popup-kind 'ai-codex
  :minor-mode '+agent-interaction-codex-mode
  :exec-args-fn
  (lambda (prompt output-file root)
    (list (if (and (boundp '+agent-interaction-codex-executable)
                   (stringp +agent-interaction-codex-executable)
                   (not (string-empty-p +agent-interaction-codex-executable)))
              +agent-interaction-codex-executable
            "codex")
          "exec"
          "--skip-git-repo-check"
          "--ephemeral"
          "--color" "never"
          "-C" root
          "-s" "workspace-write"
          "-o" output-file
          prompt))
  :exec-output 'file)

;; ── Public wrappers ───────────────────────────────────────────────────────────

(defun +agent-interaction-codex-available-p ()
  "Return non-nil when the Codex executable is available."
  (+agent-interaction-cli-available-p 'codex))

(defun +agent-interaction-codex-load ()
  "Validate the Codex executable and terminal backend."
  (unless (+agent-interaction-codex-available-p)
    (error "Codex executable not found: %s" +agent-interaction-codex-executable))
  (+agent-interaction-cli--ensure-terminal-backend 'codex))

(defun +agent-interaction-codex-buffer (&optional project-root)
  "Return the Codex session buffer for PROJECT-ROOT, or nil."
  (+agent-interaction-cli-buffer 'codex project-root))

(defun +agent-interaction-codex-session-live-p (&optional project-root)
  "Return non-nil when the Codex session for PROJECT-ROOT is live."
  (+agent-interaction-cli-session-live-p 'codex project-root))

(defun +agent-interaction-codex-ensure-session (&optional project-root)
  "Ensure a live Codex session exists for PROJECT-ROOT."
  (+agent-interaction-cli-ensure-session 'codex project-root))

(defun +agent-interaction-codex-open-buffer ()
  "Open the current project's Codex session buffer via popup."
  (interactive)
  (+agent-interaction-cli-open-buffer 'codex (+agent-interaction-project-root)))

(defalias '+agent-interaction-codex-open-active-buffer #'+agent-interaction-codex-open-buffer)

(defun +agent-interaction-codex-prime-session (&optional project-root)
  "Inject the working directory and profile into Codex for PROJECT-ROOT."
  (+agent-interaction-cli-prime-session 'codex project-root))

(defun +agent-interaction-codex-send-prompt (prompt &optional project-root)
  "Send PROMPT to Codex, starting a session for PROJECT-ROOT when needed."
  (+agent-interaction-cli-send-prompt 'codex prompt project-root))

(defun +agent-interaction-codex-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into Codex without submitting for PROJECT-ROOT."
  (+agent-interaction-cli-draft-prompt 'codex prompt project-root))

(defun +agent-interaction-codex-stop (&optional project-root)
  "Stop the active Codex session for PROJECT-ROOT."
  (interactive)
  (+agent-interaction-cli-stop 'codex project-root))

;; ── Deprecated shims ──────────────────────────────────────────────────────────

(defun +agent-interaction-codex-execution-mode ()
  "Return the current Codex execution mode (always terminal)."
  'terminal)

(defun +agent-interaction-codex-toggle-execution-mode ()
  "No-op kept for compatibility. Codex always uses terminal mode."
  (interactive)
  (setq +agent-interaction-codex-use-exec nil)
  (message "+agent-interaction Codex mode: terminal"))

(provide 'tools/agent/interaction-adapter-codex)
;;; tools/agent/interaction-adapter-codex.el ends here
