;;; ai-workbench.el --- Unified AI workbench entry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified entry points for Claude Code and Codex interactive sessions.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-vendor)
(require 'ai-workbench-session)
(require 'ai-workbench-compose)
(require 'ai-workbench-output)
(require 'ai-workbench-result)
(require 'ai-workbench-profile)
(require 'ai-workbench-status)
(require 'ai-workbench-adapter-claude)
(require 'ai-workbench-adapter-codex)
(require 'ai-workbench-tools)

(declare-function ai-workbench-claude-session-live-p "ai-workbench-adapter-claude" (&optional project-root))
(declare-function ai-workbench-codex-session-live-p "ai-workbench-adapter-codex" (&optional project-root))
(declare-function ai-workbench-claude-stop "ai-workbench-adapter-claude" (&optional project-root))
(declare-function ai-workbench-claude-draft-prompt "ai-workbench-adapter-claude" (prompt &optional project-root))
(declare-function ai-workbench-codex-draft-prompt "ai-workbench-adapter-codex" (prompt &optional project-root))
(declare-function ai-workbench-status-open "ai-workbench-status" ())

(defgroup ai-workbench nil
  "Unified AI workbench."
  :group 'tools
  :prefix "ai-workbench-")

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

(defun ai-workbench--fenced-block (content &optional info-string)
  "Return CONTENT wrapped in a fenced block with INFO-STRING."
  (format "```%s\n%s\n```"
          (or info-string "")
          (string-trim-right content)))

(defun ai-workbench--select-backend (project-root)
  "Prompt for the backend used by PROJECT-ROOT."
  (intern
   (completing-read "AI backend: "
                    '("claude" "codex")
                    nil t nil nil
                    (symbol-name (ai-workbench-session-backend project-root)))))

(defun ai-workbench--ensure-initialized (project-root)
  "Ensure PROJECT-ROOT has an initialized ai-workbench session."
  (unless (ai-workbench-session-initialized-p project-root)
    (ai-workbench-session-set-backend
     (ai-workbench--select-backend project-root)
     project-root)
    (ai-workbench-session-set-profile "default" project-root)
    (ai-workbench-session-set-initialized t project-root)))

(defun ai-workbench--backend-session-live-p (project-root)
  "Return non-nil when the selected backend session is live for PROJECT-ROOT."
  (pcase (ai-workbench-session-backend project-root)
    ('claude (ai-workbench-claude-session-live-p project-root))
    ('codex (ai-workbench-codex-session-live-p project-root))
    (_ nil)))

(defun ai-workbench--reset-selection (project-root)
  "Reset backend selection state for PROJECT-ROOT."
  (ai-workbench-session-set-initialized nil project-root)
  (ai-workbench-session-reset-profile-injected project-root)
  (ai-workbench-session-set-last-status "Backend selection reset" project-root))

(defun ai-workbench--prepare-backend (project-root)
  "Prepare the current backend for PROJECT-ROOT."
  (pcase (ai-workbench-session-backend project-root)
    ('claude
     (ai-workbench-claude-ensure-session project-root)
     (ai-workbench-claude-prime-session project-root)
     (ai-workbench-session-set-last-status "Claude session ready" project-root))
    ('codex
     (ai-workbench-codex-ensure-session project-root)
     (ai-workbench-codex-prime-session project-root)
     (ai-workbench-session-set-last-status "Codex session ready" project-root))
    (_ (user-error "Unsupported backend"))))

(defun ai-workbench-open ()
  "Select a backend if needed, prepare it, then pop the interactive buffer."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (when (and (ai-workbench-session-initialized-p project-root)
               (not (ai-workbench--backend-session-live-p project-root)))
      (ai-workbench--reset-selection project-root))
    (ai-workbench--ensure-initialized project-root)
    (ai-workbench--prepare-backend project-root)
    (ai-workbench-open-backend-buffer)))

(defalias 'ai-workbench #'ai-workbench-open)

(defun ai-workbench-cycle-backend ()
  "Cycle the current project backend between Claude and Codex."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (current (ai-workbench-session-backend project-root))
         (next (if (eq current 'claude) 'codex 'claude)))
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

(defun ai-workbench-open-backend-buffer ()
  "Open the current backend's session buffer."
  (interactive)
  (pcase (ai-workbench-session-backend)
    ('claude (ai-workbench-claude-open-buffer))
    ('codex (ai-workbench-codex-open-buffer))
    (_ (user-error "Unsupported backend"))))

(defun ai-workbench-toggle-codex-mode ()
  "Toggle the interactive Codex execution mode."
  (interactive)
  (ai-workbench-codex-toggle-execution-mode)
  (ai-workbench-session-reset-profile-injected (ai-workbench-project-root))
  (message "ai-workbench Codex mode: %s" (ai-workbench-codex-execution-mode)))

(defun ai-workbench-stop ()
  "Stop the active run for the current backend."
  (interactive)
  (pcase (ai-workbench-session-backend)
    ('codex (ai-workbench-codex-stop))
    ('claude (ai-workbench-claude-stop))
    (_ (user-error "Unsupported backend"))))

(defun ai-workbench-kill ()
  "Kill the current backend session and reset backend selection."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (pcase (ai-workbench-session-backend project-root)
      ('codex (ai-workbench-codex-stop project-root))
      ('claude (ai-workbench-claude-stop project-root))
      (_ (user-error "Unsupported backend")))
    (ai-workbench--reset-selection project-root)
    (message "ai-workbench killed current backend session")))

(defun ai-workbench-send-string (backend prompt &optional project-root)
  "Send PROMPT for PROJECT-ROOT through BACKEND."
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
      (pcase backend
        ('claude (ai-workbench-claude-send-prompt effective-prompt root))
        ('codex (ai-workbench-codex-send-prompt effective-prompt root))
        (_ (user-error "Unsupported backend: %s" backend))))
    (ai-workbench-session-mark-profile-bootstrap-sent backend root)
    (ai-workbench-session-mark-profile-injected backend root)
    (ai-workbench-output-append
     'status
     (format "Sent prompt to %s" backend)
     root)
    (ai-workbench-session-set-last-status (format "Sent prompt to %s" backend) root)
    (message "ai-workbench sent prompt to %s" backend)))

(defun ai-workbench--draft-string-now (backend prompt project-root)
  "Insert PROMPT into BACKEND for PROJECT-ROOT without submitting it."
  (pcase backend
    ('claude (ai-workbench-claude-draft-prompt prompt project-root))
    ('codex (ai-workbench-codex-draft-prompt prompt project-root))
    (_ (user-error "Unsupported backend: %s" backend))))

(defun ai-workbench--effective-prompt (backend prompt project-root)
  "Return PROMPT or a profile-wrapped version for BACKEND and PROJECT-ROOT."
  (if (ai-workbench-session-profile-injected-p backend project-root)
      prompt
    (ai-workbench-profile-wrap-user-prompt prompt project-root)))

(defun ai-workbench-draft-string (backend prompt &optional project-root)
  "Insert PROMPT into BACKEND for PROJECT-ROOT without submitting it.
The profile is injected eagerly by `ai-workbench--prepare-backend';
when that injection just ran the draft is delayed so it does not
collide with the in-flight bootstrap messages."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (profile-already-injected
          (ai-workbench-session-profile-injected-p backend root)))
    (ai-workbench-session-set-last-prompt prompt root)
    (ai-workbench-session-set-last-error nil root)
    (ai-workbench-output-append
     'prompt
     (format "draft backend: %s\nproject: %s\n\n%s"
             backend
             (abbreviate-file-name root)
             prompt)
     root)
    (ai-workbench--prepare-backend root)
    (ai-workbench-open-backend-buffer)
    (if profile-already-injected
        (ai-workbench--draft-string-now backend prompt root)
      (run-with-timer 1.5 nil
                      #'ai-workbench--draft-string-now
                      backend prompt root))
    (ai-workbench-session-set-last-status (format "Drafted prompt to %s" backend) root)
    (message "ai-workbench drafted prompt to %s; press RET in the agent buffer to submit" backend)))

(defun ai-workbench-resend-last-prompt ()
  "Resend the last prompt for the current project."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (prompt (ai-workbench-session-last-prompt project-root)))
    (unless prompt
      (user-error "No previous prompt for this project"))
    (ai-workbench-send-string backend prompt project-root)))

(defun ai-workbench-clear-session ()
  "Clear transient runtime state for the current project."
  (interactive)
  (let ((project-root (ai-workbench-project-root)))
    (ai-workbench-session-clear-runtime project-root)
    (ai-workbench-output-append 'status "Cleared runtime session state" project-root)
    (message "ai-workbench cleared runtime state")))

(defun ai-workbench-send-region (start end)
  "Send the active region to the current backend."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (source-file (buffer-file-name))
         (language (or (and source-file (file-name-extension source-file))
                       (symbol-name major-mode)))
         (metadata (if source-file
                       (format "source: %s"
                               (ai-workbench--context-relative-path source-file project-root))
                     (format "buffer: %s" (buffer-name))))
         (prompt (ai-workbench--context-block
                  "Context: region"
                  (ai-workbench--fenced-block
                   (buffer-substring-no-properties start end)
                   language)
                  metadata)))
    (ai-workbench-send-string backend prompt project-root)))

(defun ai-workbench-send-current-buffer ()
  "Send the current buffer to the current backend."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (source-file (buffer-file-name))
         (language (or (and source-file (file-name-extension source-file))
                       (symbol-name major-mode)))
         (metadata (if source-file
                       (format "source: %s"
                               (ai-workbench--context-relative-path source-file project-root))
                     (format "buffer: %s" (buffer-name))))
         (prompt (ai-workbench--context-block
                  "Context: current buffer"
                  (ai-workbench--fenced-block
                   (buffer-substring-no-properties (point-min) (point-max))
                   language)
                  metadata)))
    (ai-workbench-send-string backend prompt project-root)))

(defun ai-workbench-send-file (file)
  "Send FILE contents to the current backend."
  (interactive
   (list (read-file-name "Send file: " (ai-workbench-project-root) nil t)))
  (let* ((project-root (ai-workbench-project-root))
         (backend (ai-workbench-session-backend project-root))
         (expanded (expand-file-name file))
         (language (or (file-name-extension expanded) "text"))
         (prompt (ai-workbench--context-block
                  "Context: file"
                  (ai-workbench--fenced-block
                   (with-temp-buffer
                     (insert-file-contents expanded)
                     (buffer-string))
                   language)
                  (format "source: %s"
                          (ai-workbench--context-relative-path expanded project-root)))))
    (ai-workbench-send-string backend prompt project-root)))

(provide 'ai-workbench)
;;; ai-workbench.el ends here
