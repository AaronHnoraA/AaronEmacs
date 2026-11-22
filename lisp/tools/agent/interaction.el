;;; tools/agent/interaction.el --- Unified AI workbench entry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified entry points for AI coding sessions.  The embedded Magent runtime
;; owns queueing, durable sessions, lifecycle, and audit state.  API requests
;; use Magent/gptel; CLI requests retain each coding agent's native tools and
;; permissions behind a structured Magent sampler.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'noema-upstream)
(require 'tools/agent/interaction-backend)
(require 'tools/agent/interaction-session)
(require 'tools/agent/interaction-answer)
(require 'tools/agent/interaction-output)
(require 'tools/agent/interaction-result)
(require 'tools/agent/interaction-profile)
(require 'tools/agent/interaction-status)
(require 'tools/agent/interaction-adapter-claude)
(require 'tools/agent/interaction-adapter-codex)
(require 'tools/agent/interaction-adapter-opencode)
(require 'tools/agent/interaction-tools)
(require 'tools/agent/interaction-magent)

(declare-function +agent-interaction-status-open           "tools/agent/interaction-status" ())

(defgroup +agent-interaction nil
  "Unified AI workbench."
  :group 'tools
  :prefix "+agent-interaction-")

(defcustom +agent-interaction-save-before-dispatch t
  "When non-nil, save relevant file buffers before dispatching AI prompts."
  :type 'boolean
  :group '+agent-interaction)

;; ── Backend selection ─────────────────────────────────────────────────────────
;; `+agent-interaction' is the interactive Portal agent launcher.  Its picker offers
;; the three CLI engines (CC, Codex, OpenCode); selecting one opens that tool's
;; interactive Portal session.

(defun +agent-interaction--available-backends ()
  "Return available Magent API and CLI engine identifiers."
  (cl-remove-if-not
   (lambda (id)
     (ignore-errors (+agent-interaction-backend-call id :available-p)))
   (+agent-interaction-backend-ids :session)))

(defun +agent-interaction--select-backend (_project-root)
  "Prompt for an available Magent API or CLI engine and return its symbol."
  (let* ((ids (+agent-interaction--available-backends))
         (candidates (mapcar (lambda (id)
                               (cons (+agent-interaction-backend-label id) id))
                             ids))
         (current-backend (+agent-interaction-session-backend))
         (default (car (rassq current-backend candidates))))
    (unless candidates
      (user-error "No available +agent-interaction backends"))
    (let ((chosen (completing-read "AI engine: " (mapcar #'car candidates)
                                   nil t nil nil default)))
      (cdr (assoc chosen candidates)))))

(defun +agent-interaction--ensure-initialized (project-root)
  "Ensure PROJECT-ROOT has an initialized +agent-interaction session."
  (unless (+agent-interaction-session-initialized-p project-root)
    (+agent-interaction-session-set-backend
     (+agent-interaction--select-backend project-root)
     project-root)
    (+agent-interaction-session-set-profile "default" project-root)
    (+agent-interaction-session-set-initialized t project-root)))

;; ── Backend liveness ──────────────────────────────────────────────────────────

(defun +agent-interaction--backend-session-live-p (project-root)
  "Return non-nil when the selected backend session is live for PROJECT-ROOT."
  (or (+agent-interaction-magent-session-live-p project-root)
      (+agent-interaction-backend-live-p (+agent-interaction-session-backend project-root)
                                   project-root)))

(defun +agent-interaction--reset-selection (project-root)
  "Reset backend selection state for PROJECT-ROOT."
  (+agent-interaction-session-set-initialized nil project-root)
  (+agent-interaction-session-reset-profile-injected project-root)
  (+agent-interaction-session-set-last-status "Backend selection reset" project-root))

;; ── Backend preparation ───────────────────────────────────────────────────────

(defun +agent-interaction--prepare-backend (project-root)
  "Prepare the current backend for PROJECT-ROOT."
  (let ((backend (+agent-interaction-session-backend project-root)))
    (+agent-interaction-magent-runtime-session project-root)
    (+agent-interaction-session-set-last-status
     (format "%s Magent session ready" (+agent-interaction-backend-label backend))
     project-root)))

;; ── Context helpers ───────────────────────────────────────────────────────────

(defun +agent-interaction--context-relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when possible."
  (if (and file project-root (file-in-directory-p file project-root))
      (file-relative-name file project-root)
    (abbreviate-file-name file)))

(defun +agent-interaction--context-block (label body &optional metadata)
  "Return a labeled context block with LABEL, BODY, and optional METADATA."
  (concat (format "### %s\n" label)
          (if (and metadata (not (string-empty-p metadata)))
              (concat metadata "\n")
            "")
          body
          "\n"))

(defun +agent-interaction--position-line-column (position)
  "Return POSITION as a (line . column) cons cell."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos) (current-column))))

(defun +agent-interaction--range-reference (file start end project-root &optional label)
  "Return a reference to FILE from START to END under PROJECT-ROOT."
  (let ((relative-file (+agent-interaction--context-relative-path file project-root))
        (start-lc (+agent-interaction--position-line-column start))
        (end-lc   (+agent-interaction--position-line-column end)))
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

(defun +agent-interaction--save-buffer-if-needed (buffer)
  "Save BUFFER when it is a modified local file-visiting buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and +agent-interaction-save-before-dispatch
                 buffer-file-name
                 (buffer-modified-p)
                 (not buffer-read-only)
                 (not (file-remote-p buffer-file-name)))
        (save-buffer)))))

(defun +agent-interaction--save-current-file-buffer ()
  "Save the current buffer before switching to an AI backend."
  (+agent-interaction--save-buffer-if-needed (current-buffer)))

(defun +agent-interaction--save-file-buffer-if-open (file)
  "Save FILE's live buffer when it has unsaved edits."
  (when-let* ((buffer (find-buffer-visiting file)))
    (+agent-interaction--save-buffer-if-needed buffer)))

;; ── Public: open / cycle / switch ────────────────────────────────────────────

(defun +agent-interaction-open ()
  "Select a backend, prepare its Magent runtime, and open its conversation UI."
  (interactive)
  (let ((project-root (+agent-interaction-project-root)))
    (+agent-interaction--ensure-initialized project-root)
    (+agent-interaction--prepare-backend project-root)
    (+agent-interaction-open-backend-buffer)))

(defalias '+agent-interaction #'+agent-interaction-open)

(defun +agent-interaction-cycle-backend ()
  "Cycle the current project Portal engine."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (ids (+agent-interaction--available-backends))
         (current (+agent-interaction-session-backend project-root))
         (tail (cdr (memq current ids)))
         (next (or (car tail) (car ids))))
    (unless next
      (user-error "No +agent-interaction backends are registered"))
    (+agent-interaction-session-set-backend next project-root)
    (+agent-interaction-session-reset-profile-injected project-root)
    (message "+agent-interaction backend: %s" next)
    (+agent-interaction-open)))

(defun +agent-interaction-switch-profile (&optional profile)
  "Switch the active project profile to PROFILE."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (current (+agent-interaction-session-profile project-root))
         (selected (or profile
                       (+agent-interaction-profile-read-name-with-summary
                        "AI profile: "
                        current))))
    (+agent-interaction-session-set-profile selected project-root)
    (+agent-interaction-session-reset-profile-injected project-root)
    (+agent-interaction-session-set-last-status
     (format "Profile switched to %s" selected)
     project-root)
    (+agent-interaction-output-append
     'status
     (format "Profile switched to %s" selected)
     project-root)
    (message "+agent-interaction profile: %s" selected)))

(defun +agent-interaction-edit-profile (&optional profile)
  "Open PROFILE for editing."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (selected (or profile
                       (+agent-interaction-session-profile project-root)
                       +agent-interaction-profile-default-name)))
    (+agent-interaction-profile-open selected)))

(defun +agent-interaction-preview-profile (&optional profile)
  "Preview PROFILE in a read-only buffer."
  (interactive)
  (+agent-interaction-profile-preview
   (or profile
       (+agent-interaction-session-profile (+agent-interaction-project-root))
       +agent-interaction-profile-default-name)))

(defun +agent-interaction-create-profile (name &optional base-profile)
  "Create NAME using BASE-PROFILE as a starting point."
  (interactive
   (list (read-string "New profile name: ")
         (+agent-interaction-profile-read-name-with-summary
          "Base profile: "
          (+agent-interaction-session-profile (+agent-interaction-project-root)))))
  (+agent-interaction-profile-create name base-profile))

(defun +agent-interaction-edit-shared-snippet (&optional name)
  "Edit shared snippet NAME used by all profiles."
  (interactive)
  (+agent-interaction-profile-edit-snippet
   (or name
       (completing-read "Shared snippet: "
                        (+agent-interaction-profile-snippet-names)
                        nil t nil nil "git-policy"))))

(defun +agent-interaction-edit-template (&optional name)
  "Edit prompt template NAME."
  (interactive)
  (+agent-interaction-profile-edit-template
   (or name
       (completing-read "Prompt template: "
                        (+agent-interaction-profile-template-names)
                        nil t nil nil "context-prompt"))))

(defun +agent-interaction-status ()
  "Open the current project's +agent-interaction status buffer."
  (interactive)
  (+agent-interaction-status-open))

;; ── Public: buffer display ────────────────────────────────────────────────────

(defun +agent-interaction-open-backend-buffer ()
  "Open the current backend's Magent-owned conversation buffer."
  (interactive)
  (let ((project-root (+agent-interaction-project-root)))
    (+agent-interaction-magent-open
     (+agent-interaction-session-backend project-root) project-root)))

(defun +agent-interaction-open-direct-terminal ()
  "Open the selected CLI's legacy direct terminal session.
This explicit escape hatch bypasses Magent orchestration for interactive use."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (backend (+agent-interaction-session-backend project-root)))
    (when (eq backend 'api)
      (user-error "The API backend has no direct terminal"))
    (+agent-interaction-backend-call backend :ensure project-root)
    (+agent-interaction-backend-call backend :open project-root)))

(defun +agent-interaction-toggle-codex-mode ()
  "Toggle the interactive Codex execution mode (kept for compatibility)."
  (interactive)
  (+agent-interaction-codex-toggle-execution-mode)
  (+agent-interaction-session-reset-profile-injected (+agent-interaction-project-root))
  (message "+agent-interaction Codex mode: %s" (+agent-interaction-codex-execution-mode)))

;; ── Public: stop / kill ───────────────────────────────────────────────────────

(defun +agent-interaction-stop ()
  "Stop active and queued Magent work for the current project."
  (interactive)
  (+agent-interaction-magent-cancel (+agent-interaction-project-root)))

(defun +agent-interaction-cancel ()
  "Cancel the current AI operation in the active backend session."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (backend (+agent-interaction-session-backend project-root)))
    (+agent-interaction-magent-cancel project-root)
    (+agent-interaction-session-set-last-status (format "Canceled %s operation" backend) project-root)
    (message "+agent-interaction canceled %s operation" backend)))

(defun +agent-interaction-kill ()
  "Kill the current backend session and reset backend selection."
  (interactive)
  (let ((project-root (+agent-interaction-project-root)))
    (+agent-interaction-magent-clear project-root)
    (+agent-interaction--reset-selection project-root)
    (message "+agent-interaction killed current backend session")))

;; ── Compose buffer ────────────────────────────────────────────────────────────

(defvar-keymap +agent-interaction-compose-mode-map
  :doc "Keymap for `+agent-interaction-compose-mode'."
  "C-c C-c" #'+agent-interaction-compose-submit
  "C-c C-k" #'+agent-interaction-compose-cancel)

(define-derived-mode +agent-interaction-compose-mode text-mode "AI-Compose"
  "Major mode for editing an AI prompt before sending to the backend session.
Type your message, then press \\[+agent-interaction-compose-submit] to send."
  (setq-local header-line-format
              "  C-c C-c send · C-c C-k cancel"))

(defvar-local +agent-interaction-compose-backend nil
  "Backend symbol for the current compose buffer.")
(defvar-local +agent-interaction-compose-root nil
  "Project root for the current compose buffer.")

(defun +agent-interaction-compose-submit ()
  "Submit the compose buffer content to the AI backend session."
  (interactive)
  (let* ((buf (current-buffer))
         (backend +agent-interaction-compose-backend)
         (root +agent-interaction-compose-root)
         (content (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (and backend root)
      (user-error "Not an +agent-interaction compose buffer"))
    (unless content
      (user-error "Nothing to send"))
    (unless (+agent-interaction-magent-session-live-p root)
      (user-error "Session went away. Reopen with `+agent-interaction-open' (C-c A W)"))
    (kill-buffer buf)
    (+agent-interaction-send-string backend content root)))

(defun +agent-interaction-compose-cancel ()
  "Cancel the compose buffer."
  (interactive)
  (when (y-or-n-p "Discard this draft?")
    (kill-buffer (current-buffer))))

;; ── Public: send / draft ──────────────────────────────────────────────────────

(defun +agent-interaction-send-string (backend prompt &optional project-root)
  "Send PROMPT for PROJECT-ROOT through BACKEND."
  (+agent-interaction--save-current-file-buffer)
  (let* ((root (or project-root (+agent-interaction-project-root)))
         (effective-prompt
          (if (+agent-interaction-session-profile-injected-p backend root)
              prompt
            (+agent-interaction-profile-wrap-user-prompt prompt root))))
    (+agent-interaction-session-set-last-prompt prompt root)
    (+agent-interaction-session-set-last-error nil root)
    (+agent-interaction-session-set-last-status (format "Sending prompt to %s" backend) root)
    (+agent-interaction-output-append
     'prompt
     (format "backend: %s\nproject: %s\n\n%s"
             backend
             (abbreviate-file-name root)
             effective-prompt)
     root)
    (let ((default-directory root))
      (+agent-interaction-magent-submit
       backend effective-prompt root
       (lambda ()
         (+agent-interaction-session-set-last-status
          (format "Completed prompt with %s" backend) root)
         (message "+agent-interaction completed prompt with %s" backend))
       (lambda (message)
         (+agent-interaction-session-set-last-error message root)
         (+agent-interaction-session-set-last-status
          (format "Failed sending prompt to %s" backend) root))))))

(defun +agent-interaction--draft-string-now (backend prompt project-root)
  "Insert PROMPT into BACKEND for PROJECT-ROOT without submitting."
  (+agent-interaction-backend-call backend :draft prompt project-root nil nil))

(defun +agent-interaction--effective-prompt (backend prompt project-root)
  "Return PROMPT or a profile-wrapped version for BACKEND and PROJECT-ROOT."
  (if (+agent-interaction-session-profile-injected-p backend project-root)
      prompt
    (+agent-interaction-profile-wrap-user-prompt prompt project-root)))

(defun +agent-interaction-draft-string (backend prompt &optional project-root)
  "Open a compose buffer with PROMPT for editing before sending to BACKEND.
The Magent session is created lazily when needed."
  (+agent-interaction--save-current-file-buffer)
  (let* ((root (or project-root (+agent-interaction-project-root))))
    (+agent-interaction-magent-runtime-session root)
    (unless (+agent-interaction-session-profile-injected-p backend root)
      (+agent-interaction--prepare-backend root))
    (+agent-interaction-session-set-last-prompt prompt root)
    (+agent-interaction-session-set-last-error nil root)
    (+agent-interaction-output-append
     'prompt
     (format "draft backend: %s\nproject: %s\n\n%s"
             backend
             (abbreviate-file-name root)
             prompt)
     root)
    (let ((buf (get-buffer-create "*+agent-interaction-compose*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert prompt)
        (goto-char (point-max))
        (+agent-interaction-compose-mode)
        (setq-local +agent-interaction-compose-backend backend)
        (setq-local +agent-interaction-compose-root root))
      (display-buffer buf)
      (pop-to-buffer (+agent-interaction-output-buffer root))
      (message "Compose: edit then press C-c C-c to send to %s" backend))))

(defun +agent-interaction-resend-last-prompt ()
  "Send this project's last prompt again, to an agent of the pool (C76)."
  (interactive)
  (let ((prompt (+agent-interaction-session-last-prompt (+agent-interaction-project-root))))
    (unless prompt (user-error "No previous prompt for this project"))
    (require 'tools/agent/context)
    (+agent-context-send-prompt prompt)))

(defun +agent-interaction-clear-session ()
  "Clear transient runtime state for the current project."
  (interactive)
  (let ((project-root (+agent-interaction-project-root)))
    (+agent-interaction-magent-clear project-root)
    (+agent-interaction-session-clear-runtime project-root)
    (+agent-interaction-output-append 'status "Cleared runtime session state" project-root)
    (message "+agent-interaction cleared runtime state")))

;; ── Public: context senders ───────────────────────────────────────────────────

;; The senders are the editor's one send path (C76): a send that names a
;; place of a document becomes a task there, handed to an agent of the pool.

(declare-function +agent-context-send-region "tools/agent/context" (&optional pick))
(declare-function +agent-context-send-buffer "tools/agent/context" (&optional pick))
(declare-function +agent-context-send-file "tools/agent/context" (file &optional pick))
(declare-function +agent-context-send-prompt "tools/agent/context" (prompt &rest args))

(defun +agent-interaction-send-region (&optional pick)
  "Make the active region a task and hand it to an agent (C76).
PICK chooses the session."
  (interactive "P")
  (require 'tools/agent/context)
  (+agent-context-send-region pick))

(defun +agent-interaction-send-current-buffer (&optional pick)
  "Make the current file a task and hand it to an agent (C76).
PICK chooses the session."
  (interactive "P")
  (require 'tools/agent/context)
  (+agent-context-send-buffer pick))

(defun +agent-interaction-send-file (file &optional pick)
  "Send FILE to an agent (C76).  PICK chooses the session."
  (interactive (list (read-file-name "Send file: " nil nil t) current-prefix-arg))
  (require 'tools/agent/context)
  (+agent-context-send-file file pick))

(provide 'tools/agent/interaction)
;;; tools/agent/interaction.el ends here
