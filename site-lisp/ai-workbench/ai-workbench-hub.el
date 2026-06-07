;;; ai-workbench-hub.el --- Management hub for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A management dashboard for the ai-workbench system using the aaron-ui-board
;; toolkit.  Shows project state, backend status, configured gptel backends,
;; and provides actions for all lifecycle operations.

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)

(declare-function ai-workbench-open "ai-workbench" ())
(declare-function ai-workbench-open-backend-buffer "ai-workbench" ())
(declare-function ai-workbench-cycle-backend "ai-workbench" ())
(declare-function ai-workbench-switch-profile "ai-workbench" (&optional profile))
(declare-function ai-workbench-preview-profile "ai-workbench" (&optional profile))
(declare-function ai-workbench-create-profile "ai-workbench" (name &optional base-profile))
(declare-function ai-workbench-edit-shared-snippet "ai-workbench" (&optional name))
(declare-function ai-workbench-edit-template "ai-workbench" (&optional name))
(declare-function ai-workbench-stop "ai-workbench" ())
(declare-function ai-workbench-kill "ai-workbench" ())
(declare-function ai-workbench-output-open "ai-workbench-output" ())
(declare-function ai-workbench-result-open "ai-workbench-result" ())
(declare-function ai-workbench-claude-session-live-p "ai-workbench-adapter-claude" (&optional project-root))
(declare-function ai-workbench-codex-session-live-p "ai-workbench-adapter-codex" (&optional project-root))
(declare-function ai-workbench-opencode-session-live-p "ai-workbench-adapter-opencode" (&optional project-root))
(declare-function ai-workbench-gptel-session-live-p "ai-workbench-adapter-gptel" (&optional project-root))
(declare-function ai-workbench-gptel-register-backends "ai-workbench-adapter-gptel" ())
(declare-function ai-workbench-gptel-backend-names "ai-workbench-adapter-gptel" ())

(defconst ai-workbench-hub-buffer-name "*AI Workbench Hub*"
  "Buffer name for the ai-workbench management hub.")

(defvar ai-workbench-hub-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map aaron-ui-board-mode-map)
    (define-key map (kbd "b") #'ai-workbench-hub-cycle-backend)
    (define-key map (kbd "B") #'ai-workbench-hub-open-backend-buffer)
    (define-key map (kbd "p") #'ai-workbench-hub-switch-profile)
    (define-key map (kbd "v") #'ai-workbench-hub-preview-profile)
    (define-key map (kbd "+") #'ai-workbench-hub-create-profile)
    (define-key map (kbd "e") #'ai-workbench-hub-edit-profile)
    (define-key map (kbd "s") #'ai-workbench-hub-edit-snippet)
    (define-key map (kbd "t") #'ai-workbench-hub-edit-template)
    (define-key map (kbd "o") #'ai-workbench-hub-open-output)
    (define-key map (kbd "r") #'ai-workbench-hub-open-result)
    (define-key map (kbd "x") #'ai-workbench-hub-stop)
    (define-key map (kbd "k") #'ai-workbench-hub-kill)
    (define-key map (kbd "c") #'ai-workbench-hub-reload-backends)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ai-workbench-hub-mode'.")

(define-derived-mode ai-workbench-hub-mode aaron-ui-board-mode "AI-Hub"
  "Major mode for the ai-workbench management hub."
  (setq-local truncate-lines t))

;; ── Backend helpers ──────────────────────────────────────────────────────────

(defun ai-workbench-hub--session-live-p (backend &optional project-root)
  "Return non-nil when BACKEND has a live session for PROJECT-ROOT."
  (pcase backend
    ('claude   (and (fboundp 'ai-workbench-claude-session-live-p)
                    (ai-workbench-claude-session-live-p project-root)))
    ('codex    (and (fboundp 'ai-workbench-codex-session-live-p)
                    (ai-workbench-codex-session-live-p project-root)))
    ('opencode (and (fboundp 'ai-workbench-opencode-session-live-p)
                    (ai-workbench-opencode-session-live-p project-root)))
    ('gptel    (and (fboundp 'ai-workbench-gptel-session-live-p)
                    (ai-workbench-gptel-session-live-p project-root)))
    (_ nil)))

(defun ai-workbench-hub--backend-tone (live)
  "Return the badge tone for LIVE status."
  (if live 'success 'muted))

(defun ai-workbench-hub--backend-label (backend)
  "Return a display label for BACKEND."
  (pcase backend
    ('claude "Claude Code")
    ('codex  "Codex CLI")
    ('opencode "OpenCode")
    ('gptel  "gptel")
    (_ (format "%s" backend))))

(defun ai-workbench-hub--backend-icon (backend)
  "Return an icon kind for BACKEND."
  (pcase backend
    ('claude   'terminal)
    ('codex    'terminal)
    ('opencode 'terminal)
    ('gptel    'gear)
    (_ 'note)))

;; ── Actions ──────────────────────────────────────────────────────────────────

(defun ai-workbench-hub--default-directory ()
  "Return the project root tracked by the hub buffer."
  (or (and (derived-mode-p 'ai-workbench-hub-mode)
           (bound-and-true-p default-directory))
      default-directory))

(defun ai-workbench-hub-cycle-backend ()
  "Cycle the active ai-workbench backend."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-cycle-backend)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-open-backend-buffer ()
  "Open the active backend's session buffer."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (ai-workbench-open)
    (ai-workbench-open-backend-buffer)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-switch-profile ()
  "Switch the active profile."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-switch-profile)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-preview-profile ()
  "Preview the active profile."
  (interactive)
  (let* ((root (ai-workbench-hub--default-directory))
         (profile (ai-workbench-session-profile root)))
    (ai-workbench-preview-profile profile)))

(defun ai-workbench-hub-create-profile ()
  "Create a new profile."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-create-profile)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-edit-profile ()
  "Edit the active profile."
  (interactive)
  (let* ((root (ai-workbench-hub--default-directory))
         (profile (ai-workbench-session-profile root)))
    (ai-workbench-profile-open profile)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-edit-snippet ()
  "Edit a shared snippet."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-edit-shared-snippet)))

(defun ai-workbench-hub-edit-template ()
  "Edit a prompt template."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-edit-template)))

(defun ai-workbench-hub-open-output ()
  "Open the ai-workbench output log."
  (interactive)
  (ai-workbench-output-open))

(defun ai-workbench-hub-open-result ()
  "Open the ai-workbench result buffer."
  (interactive)
  (ai-workbench-result-open))

(defun ai-workbench-hub-stop ()
  "Stop the active backend run."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-stop)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-kill ()
  "Kill the active backend session."
  (interactive)
  (let ((default-directory (ai-workbench-hub--default-directory)))
    (call-interactively #'ai-workbench-kill)
    (ai-workbench-hub-refresh)))

(defun ai-workbench-hub-reload-backends ()
  "Reload gptel backend configuration from JSON."
  (interactive)
  (when (fboundp 'ai-workbench-gptel-register-backends)
    (ai-workbench-gptel-register-backends)
    (message "gptel backends reloaded from JSON"))
  (ai-workbench-hub-refresh))

;; ── Render helpers ──────────────────────────────────────────────────────────

(defun ai-workbench-hub--render-section-overview (project-root)
  "Render the overview section for PROJECT-ROOT."
  (aaron-ui-board-insert-section "Overview")
  (aaron-ui-board-insert-field
   "Project"
   (abbreviate-file-name project-root)
   'aaron-ui-board-path)
  (let ((backend (ai-workbench-session-backend project-root)))
    (aaron-ui-board-insert-field
     "Active Backend"
     (ai-workbench-hub--backend-label backend)))
  (let ((profile (ai-workbench-session-profile project-root)))
    (aaron-ui-board-insert-field "Profile" profile))
  (aaron-ui-board-insert-field
   "Initialized"
   (if (ai-workbench-session-initialized-p project-root) "yes" "no"))
  (aaron-ui-board-insert-field
   "Run State"
   (format "%s" (ai-workbench-session-run-state project-root)))
  (aaron-ui-board-insert-field
   "Last Status"
   (or (ai-workbench-session-last-status project-root) "-"))
  (insert "\n"))

(defun ai-workbench-hub--render-section-backends (project-root)
  "Render the backend status section for PROJECT-ROOT."
  (aaron-ui-board-insert-section "Backends" 4)
  (dolist (backend '(claude codex opencode gptel))
    (let* ((live (ai-workbench-hub--session-live-p backend project-root))
           (tone (ai-workbench-hub--backend-tone live))
           (label (ai-workbench-hub--backend-label backend))
           (icon (ai-workbench-hub--backend-icon backend))
           (active (eq backend (ai-workbench-session-backend project-root))))
      (aaron-ui-board-insert-row
       :id backend
       :icon icon
       :badge (if live "live" "idle")
       :badge-tone tone
       :title (if active (concat label "  ●active") label)
       :title-face (if active 'aaron-ui-board-badge-info nil)
       :meta (if live "session running" "no session")
       :action (lambda (_)
                 (let ((default-directory project-root))
                   (ai-workbench-session-set-backend backend)
                   (ai-workbench-open)
                   (ai-workbench-open-backend-buffer)
                   (ai-workbench-hub-refresh)))
       :help (format "RET: switch to %s and open buffer" label))))
  (insert "\n"))

(defun ai-workbench-hub--render-section-gptel-backends ()
  "Render the configured gptel backends when available."
  (when (fboundp 'ai-workbench-gptel-backend-names)
    (let ((names (ai-workbench-gptel-backend-names)))
      (aaron-ui-board-insert-section "gptel Backends (JSON)" (length names))
      (if names
          (dolist (name names)
            (aaron-ui-board-insert-row
             :id (intern name)
             :icon 'gear
             :title name
             :action (lambda (_)
                       (when (fboundp 'ai-workbench-gptel-register-backends)
                         (ai-workbench-gptel-register-backends))
                       (ai-workbench-hub-refresh))
             :help "gptel backend configured via JSON"))
        (aaron-ui-board-insert-empty "No gptel backends configured.
  Add entries to etc/ai-workbench/backends.json"))
      (insert "\n"))))

(defun ai-workbench-hub--render-section-profiles (project-root)
  "Render the profile management section for PROJECT-ROOT."
  (let* ((active (ai-workbench-session-profile project-root)))
    (aaron-ui-board-insert-section "Profiles")
    (dolist (profile (ai-workbench-profile-names))
      (let* ((summary (ai-workbench-profile-summary profile))
             (current (string= profile active)))
        (aaron-ui-board-insert-row
         :id (intern profile)
         :icon 'template
         :badge (if current "active" nil)
         :badge-tone (if current 'info 'muted)
         :title profile
         :title-face (if current 'aaron-ui-board-badge-info nil)
         :meta summary
         :action (lambda (_)
                   (let ((default-directory project-root))
                     (ai-workbench-session-set-profile profile)
                     (ai-workbench-session-reset-profile-injected project-root)
                     (ai-workbench-hub-refresh)))
         :help "RET: switch to this profile")))
    (insert "\n")))

(defun ai-workbench-hub--render-section-actions (&optional _project-root)
  "Render the action toolbar for PROJECT-ROOT."
  (aaron-ui-board-insert-section "Actions")
  (aaron-ui-board-insert-actions
   `((:label "Open Backend" :command ai-workbench-hub-open-backend-buffer
      :help "Open the active backend buffer" :primary t)
     (:label "Cycle" :command ai-workbench-hub-cycle-backend
      :help "Switch to the next backend")
     (:label "Stop" :command ai-workbench-hub-stop
      :help "Stop the active backend run")
     (:label "Kill" :command ai-workbench-hub-kill
      :help "Kill the active backend session")))
  (insert "\n")
  (aaron-ui-board-insert-actions
   `((:label "Switch Profile" :command ai-workbench-hub-switch-profile
      :help "Choose a different profile")
     (:label "Create Profile" :command ai-workbench-hub-create-profile
      :help "Create a new profile")
     (:label "Edit Profile" :command ai-workbench-hub-edit-profile
      :help "Edit the active profile")
     (:label "Edit Snippet" :command ai-workbench-hub-edit-snippet
      :help "Edit a shared snippet")
     (:label "Edit Template" :command ai-workbench-hub-edit-template
      :help "Edit a prompt template")))
  (insert "\n")
  (aaron-ui-board-insert-actions
   `((:label "Reload Backends" :command ai-workbench-hub-reload-backends
      :help "Reload gptel backends from JSON config")
     (:label "Output Log" :command ai-workbench-hub-open-output
      :help "View the output log")
     (:label "Result" :command ai-workbench-hub-open-result
      :help "View the last result")))
  (insert "\n"))

;; ── Public API ───────────────────────────────────────────────────────────────

(defun ai-workbench-hub-refresh ()
  "Refresh the ai-workbench management hub."
  (interactive)
  (let ((project-root (or (and (derived-mode-p 'ai-workbench-hub-mode)
                               (bound-and-true-p default-directory))
                          (ai-workbench-project-root)))
        (inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "AI Workbench Hub"
        :icon 'management
        :subtitle (format "Project: %s" (abbreviate-file-name project-root))
        :stats '(("AI Backends" . info) ("gptel" . info))
        :actions '((:label "Open Backend" :command ai-workbench-hub-open-backend-buffer :primary t)
                   (:label "Cycle" :command ai-workbench-hub-cycle-backend)
                   (:label "Refresh" :command ai-workbench-hub-refresh)))
       (ai-workbench-hub--render-section-overview project-root)
       (ai-workbench-hub--render-section-backends project-root)
       (ai-workbench-hub--render-section-gptel-backends)
       (ai-workbench-hub--render-section-profiles project-root)
       (ai-workbench-hub--render-section-actions project-root)
       (aaron-ui-board-insert-key-hints
        "b cycle  B open  p profile  v preview  + new profile  e edit profile  s snippet  t template  o output  r result  x stop  k kill  c reload backends  g refresh  q quit")))))

(defun ai-workbench-hub ()
  "Open the ai-workbench management hub."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (buffer (get-buffer-create ai-workbench-hub-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-hub-mode)
        (ai-workbench-hub-mode))
      (setq default-directory project-root)
      (aaron-ui-board-set-header "AI Workbench Hub" 'management
                                 (abbreviate-file-name project-root))
      (setq-local aaron-ui-board-refresh-function #'ai-workbench-hub-refresh))
    (pop-to-buffer buffer)
    (ai-workbench-hub-refresh)))

(provide 'ai-workbench-hub)
;;; ai-workbench-hub.el ends here
