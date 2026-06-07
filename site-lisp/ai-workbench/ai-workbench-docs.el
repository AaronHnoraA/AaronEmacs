;;; ai-workbench-docs.el --- One-shot docs Q&A via CLI tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run a single ephemeral CLI request against this Emacs config's docs.
;;
;; Default engine is Codex.  Prefix the question with `:c ` to use CC
;; (Claude via `claude -p`), or `:o ` to use OpenCode.  The prefix is
;; stripped before the question is sent.
;;
;; Examples:
;;   M-x ai-workbench-docs-ask  "how do I add an LSP server?"
;;   M-x ai-workbench-docs-ask  ":c how do I add an LSP server?"
;;   M-x ai-workbench-docs-ask  ":o how do I add an LSP server?"

;;; Code:

(require 'cl-lib)
(require 'lv)
(require 'subr-x)
(require 'ai-workbench-cli)
(require 'ai-workbench-adapter-codex)
(require 'ai-workbench-adapter-opencode)

(declare-function evil-emacs-state "evil" ())
(declare-function turn-off-evil-mode "evil" ())

(defgroup ai-workbench-docs nil
  "One-shot docs Q&A helpers for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-docs-")

(defconst ai-workbench-docs-root-directory
  (let* ((source (or load-file-name
                     (when-let* ((library (locate-library "ai-workbench-docs")))
                       library)
                     buffer-file-name
                     default-directory))
         (dir (file-name-directory (file-truename (expand-file-name source)))))
    (expand-file-name "../.." dir))
  "Root directory of this Emacs configuration.")

(defcustom ai-workbench-docs-directory
  (expand-file-name "docs" ai-workbench-docs-root-directory)
  "Directory containing local documentation used for one-shot Q&A."
  :type 'directory
  :group 'ai-workbench-docs)

(defcustom ai-workbench-docs-agent-file
  (expand-file-name "agent.md" ai-workbench-docs-directory)
  "Instruction file used by one-shot docs Q&A."
  :type 'file
  :group 'ai-workbench-docs)

(defcustom ai-workbench-docs-command-timeout 180
  "Maximum seconds allowed for a one-shot docs request."
  :type 'integer
  :group 'ai-workbench-docs)

(defcustom ai-workbench-docs-cc-executable
  (or (and (boundp 'claude-code-ide-cli-path)
           (stringp claude-code-ide-cli-path)
           (not (string-empty-p claude-code-ide-cli-path))
           claude-code-ide-cli-path)
      "claude")
  "Path to the Claude CLI used for docs-ask CC mode."
  :type 'string
  :group 'ai-workbench-docs)

(defvar ai-workbench-docs--process nil
  "Live process for the current one-shot docs request.")

(defvar ai-workbench-docs--timer nil
  "Timeout timer for the current one-shot docs request (unused; managed by CLI core).")

(defvar ai-workbench-docs--spinner-timer nil
  "Spinner timer for the current one-shot docs request.")

(defconst ai-workbench-docs--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames used while a docs request is running.")

(defvar ai-workbench-docs--spinner-index 0
  "Current spinner frame index.")

;; ── CC headless spec ──────────────────────────────────────────────────────────

;; Register a headless-only spec for the claude CLI (`cc`) used by docs-ask.
;; This spec has no terminal session parts — it is exec-only.
(ai-workbench-cli-register-tool 'cc
  :name "CC (Claude)"
  :exec-args-fn
  (lambda (prompt _output-file _root)
    (list (let ((exe (if (and (boundp 'ai-workbench-docs-cc-executable)
                              (stringp ai-workbench-docs-cc-executable)
                              (not (string-empty-p ai-workbench-docs-cc-executable)))
                         ai-workbench-docs-cc-executable
                       "claude")))
            exe)
          "-p" prompt
          "--output-format" "text"))
  :exec-output 'stdout)

;; ── Prefix parsing ────────────────────────────────────────────────────────────

(defun ai-workbench-docs--parse-question (raw)
  "Parse RAW question string and return (TOOL . QUESTION).
Leading `:c ' routes to CC (claude -p); `:o ' routes to OpenCode.
All other input defaults to Codex.  The prefix is stripped from QUESTION."
  (cond
   ((string-prefix-p ":c " raw)
    (cons 'cc (string-trim (substring raw 3))))
   ((string-prefix-p ":o " raw)
    (cons 'opencode (string-trim (substring raw 3))))
   (t
    (cons 'codex (string-trim raw)))))

;; ── Prerequisite checks ───────────────────────────────────────────────────────

(defun ai-workbench-docs--ensure-ready (tool)
  "Validate local prerequisites for docs Q&A with TOOL."
  (unless (file-directory-p ai-workbench-docs-directory)
    (user-error "Docs directory not found: %s" ai-workbench-docs-directory))
  (unless (file-exists-p ai-workbench-docs-agent-file)
    (user-error "Docs agent file not found: %s" ai-workbench-docs-agent-file))
  (unless (ai-workbench-cli-available-p tool)
    (user-error "%s executable not found for docs-ask" tool)))

;; ── UI helpers ────────────────────────────────────────────────────────────────

(defun ai-workbench-docs-hide ()
  "Hide the transient docs UI."
  (interactive)
  (lv-delete-window))

(defun ai-workbench-docs--dismiss-ui ()
  "Hide the transient docs UI."
  (ai-workbench-docs-hide))

(defun ai-workbench-docs--lv-setup ()
  "Configure the transient LV buffer used by docs Q&A."
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map special-mode-map)
                   (define-key map (kbd "q") #'ai-workbench-docs-hide)
                   map))
  (setq-local cursor-type nil)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state))
  (when (bound-and-true-p evil-local-mode)
    (turn-off-evil-mode)))

(add-hook 'lv-window-hook #'ai-workbench-docs--lv-setup)

(defun ai-workbench-docs--show (text)
  "Show TEXT in the transient docs UI."
  (lv-message "%s" text)
  (when-let* ((buffer (get-buffer " *LV*")))
    (with-current-buffer buffer
      (ai-workbench-docs--lv-setup))))

(defun ai-workbench-docs--cleanup-spinner ()
  "Cancel the spinner timer and clear transient state."
  (when (timerp ai-workbench-docs--spinner-timer)
    (cancel-timer ai-workbench-docs--spinner-timer))
  (setq ai-workbench-docs--spinner-timer nil)
  (setq ai-workbench-docs--process nil))

(defun ai-workbench-docs--spinner-tick ()
  "Refresh the loading spinner UI."
  (when (process-live-p ai-workbench-docs--process)
    (let ((frame (aref ai-workbench-docs--spinner-frames
                       (mod ai-workbench-docs--spinner-index
                            (length ai-workbench-docs--spinner-frames)))))
      (setq ai-workbench-docs--spinner-index (1+ ai-workbench-docs--spinner-index))
      (ai-workbench-docs--show (format "%s Docs ask loading..." frame)))))

;; ── Prompt building ───────────────────────────────────────────────────────────

(defun ai-workbench-docs--build-prompt (question)
  "Return the one-shot prompt for QUESTION."
  (string-join
   (list
    "Read docs/agent.md first, then read the relevant files under docs/ before answering."
    "Answer the user's question about using this Emacs configuration."
    "Do not modify files. Do not create or resume any long-lived session."
    "Keep the answer concise and practical. Use Chinese unless the user asks otherwise."
    "When relevant, mention the docs file path(s) you relied on."
    ""
    "User question:"
    question)
   "\n"))

;; ── Main entry ────────────────────────────────────────────────────────────────

;;;###autoload
(defun ai-workbench-docs-ask (question)
  "Ask a one-shot QUESTION about this Emacs config's docs.
Prefix QUESTION with `:c ' to use CC (Claude), `:o ' to use OpenCode.
Default engine is Codex."
  (interactive
   (list
    (read-from-minibuffer
     "Ask docs (default: Codex, :c CC, :o OpenCode): "
     nil nil nil nil nil t)))
  (unless (and (stringp question)
               (not (string-empty-p (string-trim question))))
    (user-error "Question cannot be empty"))
  (when (process-live-p ai-workbench-docs--process)
    (user-error "A docs ask request is already running"))
  (let* ((parsed  (ai-workbench-docs--parse-question (string-trim question)))
         (tool    (car parsed))
         (q-clean (cdr parsed)))
    (ai-workbench-docs--ensure-ready tool)
    (let* ((project-root ai-workbench-docs-root-directory)
           (prompt (ai-workbench-docs--build-prompt q-clean)))
      (setq ai-workbench-docs--spinner-index 0)
      (setq ai-workbench-docs--spinner-timer
            (run-at-time 0 0.12 #'ai-workbench-docs--spinner-tick))
      (setq ai-workbench-docs--process
            (ai-workbench-cli-exec
             tool prompt
             :root project-root
             :timeout ai-workbench-docs-command-timeout
             :callback
             (lambda (result)
               (ai-workbench-docs--cleanup-spinner)
               (ai-workbench-docs--show
                (format "%s\n\n[q to close]"
                        (if (string-empty-p result) "(no output)" result))))
             :on-error
             (lambda (event details)
               (ai-workbench-docs--cleanup-spinner)
               (let ((summary
                      (if (string-empty-p details)
                          (format "Docs ask failed: %s" (string-trim event))
                        (format "Docs ask failed: %s | %s"
                                (string-trim event)
                                (replace-regexp-in-string
                                 "[\n\r\t ]+" " " details)))))
                 (ai-workbench-docs--show
                  (format "%s\n\n[q to close]" summary))))))
      (ai-workbench-docs--show
       (format "⠋ Docs ask [%s] loading...  [q to close]" tool)))))

(provide 'ai-workbench-docs)
;;; ai-workbench-docs.el ends here
