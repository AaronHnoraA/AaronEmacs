;;; tools/agent/interaction-hub.el --- Management hub for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A management dashboard for the +agent-interaction system using the aaron-ui-board
;; toolkit.  Shows registered CLI agent backends (CC, Codex, OpenCode) and
;; provides actions for lifecycle, profile, and session management.
;;
;; HTTP model backends have been removed.  CLI agents are the only backends.
;; Use `+agent-interaction' (C-c A W) to pick and open an interactive Portal session.

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'subr-x)
(require 'tools/agent/interaction-session)
(require 'tools/agent/interaction-profile)

(declare-function +agent-interaction-open              "tools/agent/interaction" ())
(declare-function +agent-interaction-open-backend-buffer "tools/agent/interaction" ())
(declare-function +agent-interaction-cycle-backend     "tools/agent/interaction" ())
(declare-function +agent-interaction-switch-profile    "tools/agent/interaction" (&optional profile))
(declare-function +agent-interaction-preview-profile   "tools/agent/interaction" (&optional profile))
(declare-function +agent-interaction-create-profile    "tools/agent/interaction" (name &optional base-profile))
(declare-function +agent-interaction-edit-shared-snippet "tools/agent/interaction" (&optional name))
(declare-function +agent-interaction-edit-template     "tools/agent/interaction" (&optional name))
(declare-function +agent-interaction-stop              "tools/agent/interaction" ())
(declare-function +agent-interaction-kill              "tools/agent/interaction" ())
(declare-function +agent-interaction-output-open       "tools/agent/interaction-output" ())
(declare-function +agent-interaction-result-open       "tools/agent/interaction-result" ())
(declare-function +agent-interaction-claude-session-live-p   "tools/agent/interaction-adapter-claude"   (&optional project-root))
(declare-function +agent-interaction-codex-session-live-p    "tools/agent/interaction-adapter-codex"    (&optional project-root))
(declare-function +agent-interaction-opencode-session-live-p "tools/agent/interaction-adapter-opencode" (&optional project-root))
(declare-function +agent-interaction-engine-cli-activate-backend "tools/agent/interaction-engine-cli" (tool-id))

(defconst +agent-interaction-hub-buffer-name "*AI Workbench Hub*"
  "Buffer name for the +agent-interaction management hub.")

(defvar +agent-interaction-hub-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map aaron-ui-board-mode-map)
    (define-key map (kbd "b") #'+agent-interaction-hub-cycle-backend)
    (define-key map (kbd "B") #'+agent-interaction-hub-open-backend-buffer)
    (define-key map (kbd "p") #'+agent-interaction-hub-switch-profile)
    (define-key map (kbd "v") #'+agent-interaction-hub-preview-profile)
    (define-key map (kbd "+") #'+agent-interaction-hub-create-profile)
    (define-key map (kbd "e") #'+agent-interaction-hub-edit-profile)
    (define-key map (kbd "s") #'+agent-interaction-hub-edit-snippet)
    (define-key map (kbd "t") #'+agent-interaction-hub-edit-template)
    (define-key map (kbd "o") #'+agent-interaction-hub-open-output)
    (define-key map (kbd "r") #'+agent-interaction-hub-open-result)
    (define-key map (kbd "x") #'+agent-interaction-hub-stop)
    (define-key map (kbd "k") #'+agent-interaction-hub-kill)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `+agent-interaction-hub-mode'.")

(define-derived-mode +agent-interaction-hub-mode aaron-ui-board-mode "AI-Hub"
  "Major mode for the +agent-interaction management hub."
  (setq-local truncate-lines t))

;; ── Backend helpers ───────────────────────────────────────────────────────────

(defun +agent-interaction-hub--cli-session-live-p (backend &optional project-root)
  "Return non-nil when CLI BACKEND has a live session for PROJECT-ROOT."
  (pcase backend
    ('claude   (and (fboundp '+agent-interaction-claude-session-live-p)
                    (+agent-interaction-claude-session-live-p project-root)))
    ('codex    (and (fboundp '+agent-interaction-codex-session-live-p)
                    (+agent-interaction-codex-session-live-p project-root)))
    ('opencode (and (fboundp '+agent-interaction-opencode-session-live-p)
                    (+agent-interaction-opencode-session-live-p project-root)))
    (_ nil)))

(defun +agent-interaction-hub--backend-tone (live)
  "Return the badge tone for LIVE status."
  (if live 'success 'muted))

(defun +agent-interaction-hub--cli-label (backend)
  "Return a display label for CLI BACKEND."
  (pcase backend
    ('claude   "CC – Claude Code")
    ('codex    "Codex CLI")
    ('opencode "OpenCode")
    (_ (format "%s" backend))))

;; ── Actions ───────────────────────────────────────────────────────────────────

(defun +agent-interaction-hub--default-directory ()
  "Return the project root tracked by the hub buffer."
  (or (and (derived-mode-p '+agent-interaction-hub-mode)
           (bound-and-true-p default-directory))
      default-directory))

(defun +agent-interaction-hub-cycle-backend ()
  "Cycle the active +agent-interaction backend."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-cycle-backend)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-open-backend-buffer ()
  "Open the active backend's session buffer."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (+agent-interaction-open)
    (+agent-interaction-open-backend-buffer)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-switch-profile ()
  "Switch the active profile."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-switch-profile)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-preview-profile ()
  "Preview the active profile."
  (interactive)
  (let* ((root (+agent-interaction-hub--default-directory))
         (profile (+agent-interaction-session-profile root)))
    (+agent-interaction-preview-profile profile)))

(defun +agent-interaction-hub-create-profile ()
  "Create a new profile."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-create-profile)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-edit-profile ()
  "Edit the active profile."
  (interactive)
  (let* ((root (+agent-interaction-hub--default-directory))
         (profile (+agent-interaction-session-profile root)))
    (+agent-interaction-profile-open profile)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-edit-snippet ()
  "Edit a shared snippet."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-edit-shared-snippet)))

(defun +agent-interaction-hub-edit-template ()
  "Edit a prompt template."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-edit-template)))

(defun +agent-interaction-hub-open-output ()
  "Open the +agent-interaction output log."
  (interactive)
  (+agent-interaction-output-open))

(defun +agent-interaction-hub-open-result ()
  "Open the +agent-interaction result buffer."
  (interactive)
  (+agent-interaction-result-open))

(defun +agent-interaction-hub-stop ()
  "Stop the active backend run."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-stop)
    (+agent-interaction-hub-refresh)))

(defun +agent-interaction-hub-kill ()
  "Kill the active backend session."
  (interactive)
  (let ((default-directory (+agent-interaction-hub--default-directory)))
    (call-interactively #'+agent-interaction-kill)
    (+agent-interaction-hub-refresh)))

;; ── Render helpers ────────────────────────────────────────────────────────────

(defun +agent-interaction-hub--render-section-overview (project-root)
  "Render the overview section for PROJECT-ROOT."
  (aaron-ui-board-insert-section "Overview")
  (aaron-ui-board-insert-field
   "Project"
   (abbreviate-file-name project-root)
   'aaron-ui-board-path)
  (let* ((backend (+agent-interaction-session-backend project-root))
         (label (pcase backend
                  ('claude   "CC – Claude Code")
                  ('codex    "Codex CLI")
                  ('opencode "OpenCode")
                  ('chat     "Chat (engine frontend)")
                  (_ (symbol-name backend)))))
    (aaron-ui-board-insert-field "Active Backend" label))
  (let ((profile (+agent-interaction-session-profile project-root)))
    (aaron-ui-board-insert-field "Profile" (or profile "default")))
  (aaron-ui-board-insert-field
   "Initialized"
   (if (+agent-interaction-session-initialized-p project-root) "yes" "no"))
  (aaron-ui-board-insert-field
   "Run State"
   (format "%s" (+agent-interaction-session-run-state project-root)))
  (aaron-ui-board-insert-field
   "Last Status"
   (or (+agent-interaction-session-last-status project-root) "-"))
  (insert "\n"))

(defun +agent-interaction-hub--render-section-cli-backends (project-root)
  "Render the CLI engine status section for PROJECT-ROOT."
  (aaron-ui-board-insert-section "CLI Engines" 3)
  (dolist (backend '(claude codex opencode))
    (let* ((live   (+agent-interaction-hub--cli-session-live-p backend project-root))
           (tone   (+agent-interaction-hub--backend-tone live))
           (label  (+agent-interaction-hub--cli-label backend))
           (active (eq backend (+agent-interaction-session-backend project-root))))
      (aaron-ui-board-insert-row
       :id backend
       :icon 'terminal
       :badge (if live "live" "idle")
       :badge-tone tone
       :title (if active (concat label "  ●active") label)
       :title-face (if active 'aaron-ui-board-badge-info nil)
       :meta (if live "session running" "no session")
       :action (lambda (_)
                 (let ((default-directory project-root))
                   (+agent-interaction-session-set-backend backend)
                   (+agent-interaction-session-set-initialized t project-root)
                   ;; Sync the engine's +agent-interaction-backend var and persist.
                   (when (fboundp '+agent-interaction-engine-cli-activate-backend)
                     (+agent-interaction-engine-cli-activate-backend backend))
                   (message "+agent-interaction default backend: %s" label)
                   (+agent-interaction-hub-refresh)))
       :help (format "RET: set default backend to %s" label))))
  (insert "\n"))

(defun +agent-interaction-hub--render-section-profiles (project-root)
  "Render the profile management section for PROJECT-ROOT."
  (let* ((active (+agent-interaction-session-profile project-root)))
    (aaron-ui-board-insert-section "Profiles")
    (dolist (profile (+agent-interaction-profile-names))
      (let* ((summary (+agent-interaction-profile-summary profile))
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
                     (+agent-interaction-session-set-profile profile)
                     (+agent-interaction-session-reset-profile-injected project-root)
                     (+agent-interaction-hub-refresh)))
         :help "RET: switch to this profile")))
    (insert "\n")))

(defun +agent-interaction-hub--render-section-actions (&optional _project-root)
  "Render the action toolbar."
  (aaron-ui-board-insert-section "Actions")
  (aaron-ui-board-insert-actions
   `((:label "Open Backend" :command +agent-interaction-hub-open-backend-buffer
      :help "Launch & open the default backend's session" :primary t)
     (:label "Cycle" :command +agent-interaction-hub-cycle-backend
      :help "Cycle default backend: cc → codex → opencode")
     (:label "Stop" :command +agent-interaction-hub-stop
      :help "Stop the active backend run")
     (:label "Kill" :command +agent-interaction-hub-kill
      :help "Kill the active backend session")))
  (insert "\n")
  (aaron-ui-board-insert-actions
   `((:label "Switch Profile" :command +agent-interaction-hub-switch-profile
      :help "Choose a different profile")
     (:label "Create Profile" :command +agent-interaction-hub-create-profile
      :help "Create a new profile")
     (:label "Edit Profile" :command +agent-interaction-hub-edit-profile
      :help "Edit the active profile")
     (:label "Edit Snippet" :command +agent-interaction-hub-edit-snippet
      :help "Edit a shared snippet")
     (:label "Edit Template" :command +agent-interaction-hub-edit-template
      :help "Edit a prompt template")))
  (insert "\n")
  (aaron-ui-board-insert-actions
   `((:label "Output Log" :command +agent-interaction-hub-open-output
      :help "View the output log")
     (:label "Result" :command +agent-interaction-hub-open-result
      :help "View the last result")))
  (insert "\n"))

;; ── Public API ────────────────────────────────────────────────────────────────

(defun +agent-interaction-hub-refresh ()
  "Refresh the +agent-interaction management hub."
  (interactive)
  (let ((project-root (or (and (derived-mode-p '+agent-interaction-hub-mode)
                               (bound-and-true-p default-directory))
                          (+agent-interaction-project-root)))
        (inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "AI Workbench Hub"
        :icon 'management
        :subtitle (format "Project: %s" (abbreviate-file-name project-root))
        :stats '(("CLI Engines" . info))
        :actions '((:label "Open Backend" :command +agent-interaction-hub-open-backend-buffer :primary t)
                   (:label "Cycle" :command +agent-interaction-hub-cycle-backend)
                   (:label "Refresh" :command +agent-interaction-hub-refresh)))
       (+agent-interaction-hub--render-section-overview project-root)
       (+agent-interaction-hub--render-section-cli-backends project-root)
       (+agent-interaction-hub--render-section-profiles project-root)
       (+agent-interaction-hub--render-section-actions project-root)
       (aaron-ui-board-insert-key-hints
        "RET set default  b cycle  B launch open  p profile  v preview  + new profile  e edit  s snippet  t template  o output  r result  x stop  k kill  g refresh  q quit")))))

(defun +agent-interaction-hub ()
  "Open the +agent-interaction management hub."
  (interactive)
  (let* ((project-root (+agent-interaction-project-root))
         (buffer (get-buffer-create +agent-interaction-hub-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-interaction-hub-mode)
        (+agent-interaction-hub-mode))
      (setq default-directory project-root)
      (aaron-ui-board-set-header "AI Workbench Hub" 'management
                                 (abbreviate-file-name project-root))
      (setq-local aaron-ui-board-refresh-function #'+agent-interaction-hub-refresh))
    (pop-to-buffer buffer)
    (+agent-interaction-hub-refresh)))

(provide 'tools/agent/interaction-hub)
;;; tools/agent/interaction-hub.el ends here
