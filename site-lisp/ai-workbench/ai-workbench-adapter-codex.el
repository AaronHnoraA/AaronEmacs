;;; ai-workbench-adapter-codex.el --- Codex adapter for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers the Codex CLI tool spec with `ai-workbench-cli' and exposes
;; thin public wrappers that the rest of ai-workbench calls by name.
;; All session logic lives in ai-workbench-cli.el.

;;; Code:

(require 'ai-workbench-cli)

(defgroup ai-workbench-codex nil
  "Codex terminal integration for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-codex-")

(defcustom ai-workbench-codex-executable "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-workbench-codex)

(defcustom ai-workbench-codex-extra-args nil
  "Additional command line arguments passed to Codex."
  :type '(repeat string)
  :group 'ai-workbench-codex)

(defcustom ai-workbench-codex-terminal-backend 'vterm
  "Terminal backend used for Codex sessions."
  :type '(choice (const vterm) (const eat))
  :group 'ai-workbench-codex)

(defvar ai-workbench-codex-use-exec nil
  "Deprecated toggle kept for compatibility. Codex defaults to terminal mode.")

(define-minor-mode ai-workbench-codex-mode
  "Minor mode marker for ai-workbench Codex terminal buffers."
  :init-value nil
  :lighter " AI-Codex")

;; ── Tool spec registration ────────────────────────────────────────────────────

(ai-workbench-cli-register-tool 'codex
  :name "Codex CLI"
  :executable-var 'ai-workbench-codex-executable
  :extra-args-var 'ai-workbench-codex-extra-args
  :terminal-backend-var 'ai-workbench-codex-terminal-backend
  :env-vars '("TERM_PROGRAM=emacs" "FORCE_CODE_TERMINAL=true")
  :buffer-prefix "codex"
  :popup-kind 'ai-codex
  :minor-mode 'ai-workbench-codex-mode
  :exec-args-fn
  (lambda (prompt output-file root)
    (list (if (and (boundp 'ai-workbench-codex-executable)
                   (stringp ai-workbench-codex-executable)
                   (not (string-empty-p ai-workbench-codex-executable)))
              ai-workbench-codex-executable
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

(defun ai-workbench-codex-available-p ()
  "Return non-nil when the Codex executable is available."
  (ai-workbench-cli-available-p 'codex))

(defun ai-workbench-codex-load ()
  "Validate the Codex executable and terminal backend."
  (unless (ai-workbench-codex-available-p)
    (error "Codex executable not found: %s" ai-workbench-codex-executable))
  (ai-workbench-cli--ensure-terminal-backend 'codex))

(defun ai-workbench-codex-buffer (&optional project-root)
  "Return the Codex session buffer for PROJECT-ROOT, or nil."
  (ai-workbench-cli-buffer 'codex project-root))

(defun ai-workbench-codex-session-live-p (&optional project-root)
  "Return non-nil when the Codex session for PROJECT-ROOT is live."
  (ai-workbench-cli-session-live-p 'codex project-root))

(defun ai-workbench-codex-ensure-session (&optional project-root)
  "Ensure a live Codex session exists for PROJECT-ROOT."
  (ai-workbench-cli-ensure-session 'codex project-root))

(defun ai-workbench-codex-open-buffer ()
  "Open the current project's Codex session buffer via popup."
  (interactive)
  (ai-workbench-cli-open-buffer 'codex (ai-workbench-project-root)))

(defalias 'ai-workbench-codex-open-active-buffer #'ai-workbench-codex-open-buffer)

(defun ai-workbench-codex-prime-session (&optional project-root)
  "Inject the working directory and profile into Codex for PROJECT-ROOT."
  (ai-workbench-cli-prime-session 'codex project-root))

(defun ai-workbench-codex-send-prompt (prompt &optional project-root)
  "Send PROMPT to Codex, starting a session for PROJECT-ROOT when needed."
  (ai-workbench-cli-send-prompt 'codex prompt project-root))

(defun ai-workbench-codex-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into Codex without submitting for PROJECT-ROOT."
  (ai-workbench-cli-draft-prompt 'codex prompt project-root))

(defun ai-workbench-codex-stop (&optional project-root)
  "Stop the active Codex session for PROJECT-ROOT."
  (interactive)
  (ai-workbench-cli-stop 'codex project-root))

;; ── Deprecated shims ──────────────────────────────────────────────────────────

(defun ai-workbench-codex-execution-mode ()
  "Return the current Codex execution mode (always terminal)."
  'terminal)

(defun ai-workbench-codex-toggle-execution-mode ()
  "No-op kept for compatibility. Codex always uses terminal mode."
  (interactive)
  (setq ai-workbench-codex-use-exec nil)
  (message "ai-workbench Codex mode: terminal"))

(provide 'ai-workbench-adapter-codex)
;;; ai-workbench-adapter-codex.el ends here
