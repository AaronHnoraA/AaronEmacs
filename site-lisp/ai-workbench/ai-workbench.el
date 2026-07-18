;;; ai-workbench.el --- Unified AI workbench entry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified entry points for AI coding sessions.  The embedded Magent runtime
;; owns queueing, durable sessions, lifecycle, and audit state.  API requests
;; use Magent/gptel; CLI requests retain each coding agent's native tools and
;; permissions behind a structured Magent sampler.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-workbench-vendor)
(require 'ai-workbench-backend)
(require 'ai-workbench-session)
(require 'ai-workbench-answer)
(require 'ai-workbench-output)
(require 'ai-workbench-result)
(require 'ai-workbench-profile)
(require 'ai-workbench-status)
(require 'ai-workbench-adapter-claude)
(require 'ai-workbench-adapter-codex)
(require 'ai-workbench-adapter-opencode)
(require 'ai-workbench-tools)
(require 'ai-workbench-magent)

(declare-function ai-workbench-status-open           "ai-workbench-status" ())

(defgroup ai-workbench nil
  "Unified AI workbench."
  :group 'tools
  :prefix "ai-workbench-")

(defcustom ai-workbench-save-before-dispatch t
  "When non-nil, save relevant file buffers before dispatching AI prompts."
  :type 'boolean
  :group 'ai-workbench)

;; ── Backend selection ─────────────────────────────────────────────────────────
;; `ai-workbench' is the interactive vterm agent launcher.  Its picker offers
;; the three CLI engines (CC, Codex, OpenCode); selecting one opens that tool's
;; interactive vterm session.

(defun ai-workbench--available-backends ()
  "Return available Magent API and CLI engine identifiers."
  (cl-remove-if-not
   (lambda (id)
     (ignore-errors (ai-workbench-backend-call id :available-p)))
   (ai-workbench-backend-ids :session)))

(defun ai-workbench--select-backend (_project-root)
  "Prompt for an available Magent API or CLI engine and return its symbol."
  (let* ((ids (ai-workbench--available-backends))
         (candidates (mapcar (lambda (id)
                               (cons (ai-workbench-backend-label id) id))
                             ids))
         (current-backend (ai-workbench-session-backend))
         (default (car (rassq current-backend candidates))))
    (unless candidates
      (user-error "No available ai-workbench backends"))
    (let ((chosen (completing-read "AI engine: " (mapcar #'car candidates)
                                   nil t nil nil default)))
      (cdr (assoc chosen candidates)))))

(defun ai-workbench--ensure-initialized (project-root)
  "Ensure PROJECT-ROOT has an initialized ai-workbench session."
  (unless (ai-workbench-session-initialized-p project-root)
    (ai-workbench-session-set-backend
     (ai-workbench--select-backend project-root)
     project-root)
    (ai-workbench-session-set-profile "default" project-root)
    (ai-workbench-session-set-initialized t project-root)))

;; ── Backend liveness ──────────────────────────────────────────────────────────

(defun ai-workbench--backend-session-live-p (project-root)
  "Return non-nil when the selected backend session is live for PROJECT-ROOT."
  (or (ai-workbench-magent-session-live-p project-root)
      (ai-workbench-backend-live-p (ai-workbench-session-backend project-root)
                                   project-root)))

(defun ai-workbench--reset-selection (project-root)
  "Reset backend selection state for PROJECT-ROOT."
  (ai-workbench-session-set-initialized nil project-root)
  (ai-workbench-session-reset-profile-injected project-root)
  (ai-workbench-session-set-last-status "Backend selection reset" project-root))

;; ── Backend preparation ───────────────────────────────────────────────────────

(defun ai-workbench--prepare-backend (project-root)
  "Prepare the current backend for PROJECT-ROOT."
  (let ((backend (ai-workbench-session-backend project-root)))
    (ai-workbench-magent-runtime-session project-root)
    (ai-workbench-session-set-last-status
     (format "%s Magent session ready" (ai-workbench-backend-label backend))
     project-root)))

;; ── Context helpers ───────────────────────────────────────────────────────────

(defun ai-workbench--context-relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when possible."
  (if (and file project-root (file-in-directory-p file project-root))
      (file-relative-name file project-root)
    (abbreviate-file-name file)))

(defun ai-workbench--context-block (label body &optional metadata)
  "Return a labeled context block with LABEL, BODY, and optional METADATA."
  (concat (format "### %s\n" label)
          (if (and metadata (not (string-empty-p metadata)))
              (concat metadata "\n")
            "")
          body
          "\n"))

(defun ai-workbench--position-line-column (position)
  "Return POSITION as a (line . column) cons cell."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos) (current-column))))

(defun ai-workbench--range-reference (file start end project-root &optional label)
  "Return a reference to FILE from START to END under PROJECT-ROOT."
  (let ((relative-file (ai-workbench--context-relative-path file project-root))
        (start-lc (ai-workbench--position-line-column start))
        (end-lc   (ai-workbench--position-line-column end)))
    (string-join
     (delq nil
           (list
            (format "@range %s:%d:%d-%d:%d"
                    relative-file
                    (car start-lc) (cdr start-lc)
                    (car end-lc)   (cdr end-lc))
            label))
     " ")))

;; ── Save helpers ──────────────────────────────────────────────────────────────

(defun ai-workbench--save-buffer-if-needed (buffer)
  "Save BUFFER when it is a modified local file-visiting buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and ai-workbench-save-before-dispatch
                 buffer-file-name
                 (buffer-modified-p)
                 (not buffer-read-only)
                 (not (file-remote-p buffer-file-name)))
        (save-buffer)))))

(defun ai-workbench--save-current-file-buffer ()
  "Save the current buffer before switching to an AI backend."
  (ai-workbench--save-buffer-if-needed (current-buffer)))

(defun ai-workbench--save-file-buffer-if-open (file)
  "Save FILE's live buffer when it has unsaved edits."
  (when-let* ((buffer (find-buffer-visiting file)))
    (ai-workbench--save-buffer-if-needed buffer)))

;; ── Public: open / cycle / switch ────────────────────────────────────────────

(defun ai-workbench-open ()
  "Select a backend, prepare its Magent runtime, and open its conversation UI."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (ai-workbench--ensure-initialized project-root)
    (ai-workbench--prepare-backend project-root)
    (ai-workbench-open-backend-buffer)))

(defalias 'ai-workbench #'ai-workbench-open)

(defun ai-workbench-cycle-backend ()
  "Cycle the current project vterm engine."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (ids (ai-workbench--available-backends))
         (current (ai-workbench-session-backend project-root))
         (tail (cdr (memq current ids)))
         (next (or (car tail) (car ids))))
    (unless next
      (user-error "No ai-workbench backends are registered"))
    (ai-workbench-session-set-backend next project-root)
    (ai-workbench-session-reset-profile-injected project-root)
    (message "ai-workbench backend: %s" next)
    (ai-workbench-open)))

(defun ai-workbench-switch-profile (&optional profile)
  "Switch the active project profile to PROFILE."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (current (ai-workbench-session-profile project-root))
         (selected (or profile
                       (ai-workbench-profile-read-name-with-summary
                        "AI profile: "
                        current))))
    (ai-workbench-session-set-profile selected project-root)
    (ai-workbench-session-reset-profile-injected project-root)
    (ai-workbench-session-set-last-status
     (format "Profile switched to %s" selected)
     project-root)
    (ai-workbench-output-append
     'status
     (format "Profile switched to %s" selected)
     project-root)
    (message "ai-workbench profile: %s" selected)))

(defun ai-workbench-edit-profile (&optional profile)
  "Open PROFILE for editing."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (selected (or profile
                       (ai-workbench-session-profile project-root)
                       ai-workbench-profile-default-name)))
    (ai-workbench-profile-open selected)))

(defun ai-workbench-preview-profile (&optional profile)
  "Preview PROFILE in a read-only buffer."
  (interactive)
  (ai-workbench-profile-preview
   (or profile
       (ai-workbench-session-profile (ai-workbench-project-root))
       ai-workbench-profile-default-name)))

(defun ai-workbench-create-profile (name &optional base-profile)
  "Create NAME using BASE-PROFILE as a starting point."
  (interactive
   (list (read-string "New profile name: ")
         (ai-workbench-profile-read-name-with-summary
          "Base profile: "
          (ai-workbench-session-profile (ai-workbench-project-root)))))
  (ai-workbench-profile-create name base-profile))

(defun ai-workbench-edit-shared-snippet (&optional name)
  "Edit shared snippet NAME used by all profiles."
  (interactive)
  (ai-workbench-profile-edit-snippet
   (or name
       (completing-read "Shared snippet: "
                        (ai-workbench-profile-snippet-names)
                        nil t nil nil "git-policy"))))

(defun ai-workbench-edit-template (&optional name)
  "Edit prompt template NAME."
  (interactive)
  (ai-workbench-profile-edit-template
   (or name
       (completing-read "Prompt template: "
                        (ai-workbench-profile-template-names)
                        nil t nil nil "context-prompt"))))

(defun ai-workbench-status ()
  "Open the current project's ai-workbench status buffer."
  (interactive)
  (ai-workbench-status-open))

;; ── Public: buffer display ────────────────────────────────────────────────────

(defun ai-workbench-open-backend-buffer ()
  "Open the current backend's Magent-owned conversation buffer."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (ai-workbench-magent-open
     (ai-workbench-session-backend project-root) project-root)))

(defun ai-workbench-open-direct-terminal ()
  "Open the selected CLI's legacy direct terminal session.
This explicit escape hatch bypasses Magent orchestration for interactive use."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root)))
    (when (eq backend 'api)
      (user-error "The API backend has no direct terminal"))
    (ai-workbench-backend-call backend :ensure project-root)
    (ai-workbench-backend-call backend :open project-root)))

(defun ai-workbench-toggle-codex-mode ()
  "Toggle the interactive Codex execution mode (kept for compatibility)."
  (interactive)
  (ai-workbench-codex-toggle-execution-mode)
  (ai-workbench-session-reset-profile-injected (ai-workbench-project-root))
  (message "ai-workbench Codex mode: %s" (ai-workbench-codex-execution-mode)))

;; ── Public: stop / kill ───────────────────────────────────────────────────────

(defun ai-workbench-stop ()
  "Stop active and queued Magent work for the current project."
  (interactive)
  (ai-workbench-magent-cancel (ai-workbench-project-root)))

(defun ai-workbench-cancel ()
  "Cancel the current AI operation in the active backend session."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root)))
    (ai-workbench-magent-cancel project-root)
    (ai-workbench-session-set-last-status (format "Canceled %s operation" backend) project-root)
    (message "ai-workbench canceled %s operation" backend)))

(defun ai-workbench-kill ()
  "Kill the current backend session and reset backend selection."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (ai-workbench-magent-clear project-root)
    (ai-workbench--reset-selection project-root)
    (message "ai-workbench killed current backend session")))

;; ── Compose buffer ────────────────────────────────────────────────────────────

(defvar-keymap ai-workbench-compose-mode-map
  :doc "Keymap for `ai-workbench-compose-mode'."
  "C-c C-c" #'ai-workbench-compose-submit
  "C-c C-k" #'ai-workbench-compose-cancel)

(define-derived-mode ai-workbench-compose-mode text-mode "AI-Compose"
  "Major mode for editing an AI prompt before sending to the backend session.
Type your message, then press \\[ai-workbench-compose-submit] to send."
  (setq-local header-line-format
              "  C-c C-c send · C-c C-k cancel"))

(defvar-local ai-workbench-compose-backend nil
  "Backend symbol for the current compose buffer.")
(defvar-local ai-workbench-compose-root nil
  "Project root for the current compose buffer.")

(defun ai-workbench-compose-submit ()
  "Submit the compose buffer content to the AI backend session."
  (interactive)
  (let* ((buf (current-buffer))
         (backend ai-workbench-compose-backend)
         (root ai-workbench-compose-root)
         (content (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (and backend root)
      (user-error "Not an ai-workbench compose buffer"))
    (unless content
      (user-error "Nothing to send"))
    (unless (ai-workbench-magent-session-live-p root)
      (user-error "Session went away. Reopen with `ai-workbench-open' (C-c A W)"))
    (kill-buffer buf)
    (ai-workbench-send-string backend content root)))

(defun ai-workbench-compose-cancel ()
  "Cancel the compose buffer."
  (interactive)
  (when (y-or-n-p "Discard this draft?")
    (kill-buffer (current-buffer))))

;; ── Public: send / draft ──────────────────────────────────────────────────────

(defun ai-workbench-send-string (backend prompt &optional project-root)
  "Send PROMPT for PROJECT-ROOT through BACKEND."
  (ai-workbench--save-current-file-buffer)
  (let* ((root (or project-root (ai-workbench-project-root)))
         (effective-prompt
          (if (ai-workbench-session-profile-injected-p backend root)
              prompt
            (ai-workbench-profile-wrap-user-prompt prompt root))))
    (ai-workbench-session-set-last-prompt prompt root)
    (ai-workbench-session-set-last-error nil root)
    (ai-workbench-session-set-last-status (format "Sending prompt to %s" backend) root)
    (ai-workbench-output-append
     'prompt
     (format "backend: %s\nproject: %s\n\n%s"
             backend
             (abbreviate-file-name root)
             effective-prompt)
     root)
    (let ((default-directory root))
      (ai-workbench-magent-submit
       backend effective-prompt root
       (lambda ()
         (ai-workbench-session-set-last-status
          (format "Completed prompt with %s" backend) root)
         (message "ai-workbench completed prompt with %s" backend))
       (lambda (message)
         (ai-workbench-session-set-last-error message root)
         (ai-workbench-session-set-last-status
          (format "Failed sending prompt to %s" backend) root))))))

(defun ai-workbench--draft-string-now (backend prompt project-root)
  "Insert PROMPT into BACKEND for PROJECT-ROOT without submitting."
  (ai-workbench-backend-call backend :draft prompt project-root nil nil))

(defun ai-workbench--effective-prompt (backend prompt project-root)
  "Return PROMPT or a profile-wrapped version for BACKEND and PROJECT-ROOT."
  (if (ai-workbench-session-profile-injected-p backend project-root)
      prompt
    (ai-workbench-profile-wrap-user-prompt prompt project-root)))

(defun ai-workbench-draft-string (backend prompt &optional project-root)
  "Open a compose buffer with PROMPT for editing before sending to BACKEND.
The Magent session is created lazily when needed."
  (ai-workbench--save-current-file-buffer)
  (let* ((root (or project-root (ai-workbench-project-root))))
    (ai-workbench-magent-runtime-session root)
    (unless (ai-workbench-session-profile-injected-p backend root)
      (ai-workbench--prepare-backend root))
    (ai-workbench-session-set-last-prompt prompt root)
    (ai-workbench-session-set-last-error nil root)
    (ai-workbench-output-append
     'prompt
     (format "draft backend: %s\nproject: %s\n\n%s"
             backend
             (abbreviate-file-name root)
             prompt)
     root)
    (let ((buf (get-buffer-create "*ai-workbench-compose*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert prompt)
        (goto-char (point-max))
        (ai-workbench-compose-mode)
        (setq-local ai-workbench-compose-backend backend)
        (setq-local ai-workbench-compose-root root))
      (display-buffer buf)
      (pop-to-buffer (ai-workbench-output-buffer root))
      (message "Compose: edit then press C-c C-c to send to %s" backend))))

(defun ai-workbench-resend-last-prompt ()
  "Resend the last prompt for the current project."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (prompt (ai-workbench-session-last-prompt project-root)))
    (unless prompt
      (user-error "No previous prompt for this project"))
    (unless (ai-workbench--backend-session-live-p project-root)
      (user-error "No active session. Start one first with `ai-workbench-open'"))
    (ai-workbench-send-string backend prompt project-root)))

(defun ai-workbench-clear-session ()
  "Clear transient runtime state for the current project."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (ai-workbench-magent-clear project-root)
    (ai-workbench-session-clear-runtime project-root)
    (ai-workbench-output-append 'status "Cleared runtime session state" project-root)
    (message "ai-workbench cleared runtime state")))

;; ── Public: context senders ───────────────────────────────────────────────────

(defun ai-workbench-send-region (start end)
  "Send a reference to the active region to the current backend as a draft."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (source-file (or (buffer-file-name)
                          (user-error "Current buffer is not visiting a file")))
         (prompt (ai-workbench--context-block
                  "Reference: region"
                  (ai-workbench--range-reference
                   source-file start end project-root "selection"))))
    (ai-workbench-draft-string backend prompt project-root)))

(defun ai-workbench-send-current-buffer ()
  "Send a reference to the current buffer to the current backend as a draft."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (source-file (or (buffer-file-name)
                          (user-error "Current buffer is not visiting a file")))
         (prompt (ai-workbench--context-block
                  "Reference: current buffer"
                  (format "@file %s"
                          (ai-workbench--context-relative-path
                           source-file project-root)))))
    (ai-workbench-draft-string backend prompt project-root)))

(defun ai-workbench-send-file (file)
  "Send a reference to FILE to the current backend as a draft."
  (interactive
   (list (read-file-name "Send file: " (ai-workbench-project-root) nil t)))
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (expanded (expand-file-name file))
         (prompt (ai-workbench--context-block
                  "Reference: file"
                  (format "@file %s"
                          (ai-workbench--context-relative-path
                           expanded project-root)))))
    (ai-workbench--save-file-buffer-if-open expanded)
    (ai-workbench-draft-string backend prompt project-root)))

(provide 'ai-workbench)
;;; ai-workbench.el ends here
