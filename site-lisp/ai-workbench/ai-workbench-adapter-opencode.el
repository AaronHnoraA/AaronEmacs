;;; ai-workbench-adapter-opencode.el --- OpenCode adapter for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers the OpenCode CLI tool spec with `ai-workbench-cli' and exposes
;; thin public wrappers that the rest of ai-workbench calls by name.
;; All session logic lives in ai-workbench-cli.el.

;;; Code:

(require 'ai-workbench-cli)

(defgroup ai-workbench-opencode nil
  "OpenCode terminal integration for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-opencode-")

(defcustom ai-workbench-opencode-executable "opencode"
  "Path to the OpenCode executable."
  :type 'string
  :group 'ai-workbench-opencode)

(defcustom ai-workbench-opencode-extra-args nil
  "Additional command line arguments passed to OpenCode."
  :type '(repeat string)
  :group 'ai-workbench-opencode)

(defcustom ai-workbench-opencode-terminal-backend 'vterm
  "Terminal backend used for OpenCode sessions."
  :type '(choice (const vterm) (const eat))
  :group 'ai-workbench-opencode)

(define-minor-mode ai-workbench-opencode-mode
  "Minor mode marker for ai-workbench OpenCode terminal buffers."
  :init-value nil
  :lighter " AI-OpenCode")

;; ── Tool spec registration ────────────────────────────────────────────────────

(ai-workbench-cli-register-tool 'opencode
  :name "OpenCode"
  :executable-var 'ai-workbench-opencode-executable
  :extra-args-var 'ai-workbench-opencode-extra-args
  :terminal-backend-var 'ai-workbench-opencode-terminal-backend
  :env-vars '("TERM_PROGRAM=emacs")
  :buffer-prefix "opencode"
  :popup-kind 'ai-opencode
  :minor-mode 'ai-workbench-opencode-mode
  :exec-args-fn
  (lambda (prompt _output-file _root)
    ;; `opencode run <message>' runs non-interactively and exits.
    (let ((exe (if (and (boundp 'ai-workbench-opencode-executable)
                        (stringp ai-workbench-opencode-executable)
                        (not (string-empty-p ai-workbench-opencode-executable)))
                   ai-workbench-opencode-executable
                 "opencode")))
      (list exe "run" prompt)))
  :exec-output 'stdout)

;; ── Public wrappers ───────────────────────────────────────────────────────────

(defun ai-workbench-opencode-available-p ()
  "Return non-nil when the OpenCode executable is available."
  (ai-workbench-cli-available-p 'opencode))

(defun ai-workbench-opencode-load ()
  "Validate the OpenCode executable and terminal backend."
  (unless (ai-workbench-opencode-available-p)
    (error "OpenCode executable not found: %s" ai-workbench-opencode-executable))
  (ai-workbench-cli--ensure-terminal-backend 'opencode))

(defun ai-workbench-opencode-buffer (&optional project-root)
  "Return the OpenCode session buffer for PROJECT-ROOT, or nil."
  (ai-workbench-cli-buffer 'opencode project-root))

(defun ai-workbench-opencode-session-live-p (&optional project-root)
  "Return non-nil when the OpenCode session for PROJECT-ROOT is live."
  (ai-workbench-cli-session-live-p 'opencode project-root))

(defun ai-workbench-opencode-ensure-session (&optional project-root)
  "Ensure a live OpenCode session exists for PROJECT-ROOT."
  (ai-workbench-cli-ensure-session 'opencode project-root))

(defun ai-workbench-opencode-open-buffer ()
  "Open the current project's OpenCode session buffer via popup."
  (interactive)
  (ai-workbench-cli-open-buffer 'opencode (ai-workbench-project-root)))

(defun ai-workbench-opencode-prime-session (&optional project-root)
  "Inject the working directory and profile into OpenCode for PROJECT-ROOT."
  (ai-workbench-cli-prime-session 'opencode project-root))

(defun ai-workbench-opencode-send-prompt (prompt &optional project-root)
  "Send PROMPT to OpenCode, starting a session for PROJECT-ROOT when needed."
  (ai-workbench-cli-send-prompt 'opencode prompt project-root))

(defun ai-workbench-opencode-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into OpenCode without submitting for PROJECT-ROOT."
  (ai-workbench-cli-draft-prompt 'opencode prompt project-root))

(defun ai-workbench-opencode-stop (&optional project-root)
  "Stop the active OpenCode session for PROJECT-ROOT."
  (interactive)
  (ai-workbench-cli-stop 'opencode project-root))

(provide 'ai-workbench-adapter-opencode)
;;; ai-workbench-adapter-opencode.el ends here
