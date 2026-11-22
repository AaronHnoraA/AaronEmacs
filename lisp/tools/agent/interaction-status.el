;;; tools/agent/interaction-status.el --- Status buffer for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a lightweight project-scoped status buffer.

;;; Code:

(require 'subr-x)
(require 'tools/agent/interaction-session)
(require 'tools/agent/interaction-profile)

(declare-function +agent-interaction-open "tools/agent/interaction" ())
(declare-function +agent-interaction-kill "tools/agent/interaction" ())
(declare-function +agent-interaction-open-backend-buffer "tools/agent/interaction" ())
(declare-function +agent-interaction-switch-profile "tools/agent/interaction" (&optional profile))
(declare-function +agent-interaction-preview-profile "tools/agent/interaction" (&optional profile))
(declare-function +agent-interaction-create-profile "tools/agent/interaction" (name &optional base-profile))
(declare-function +agent-interaction-edit-shared-snippet "tools/agent/interaction" (&optional name))
(declare-function +agent-interaction-edit-template "tools/agent/interaction" (&optional name))
(declare-function +agent-interaction-cycle-backend "tools/agent/interaction" ())
(declare-function +agent-interaction-output-open "tools/agent/interaction-output" ())
(declare-function +agent-interaction-result-open "tools/agent/interaction-result" ())
(declare-function +agent-interaction-claude-session-live-p "tools/agent/interaction-adapter-claude" (&optional project-root))
(declare-function +agent-interaction-codex-session-live-p "tools/agent/interaction-adapter-codex" (&optional project-root))

(defvar +agent-interaction-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+agent-interaction-status-refresh)
    (define-key map (kbd "RET") #'+agent-interaction-status-open-backend)
    (define-key map (kbd "b") #'+agent-interaction-status-cycle-backend)
    (define-key map (kbd "p") #'+agent-interaction-status-switch-profile)
    (define-key map (kbd "v") #'+agent-interaction-status-preview-profile)
    (define-key map (kbd "+") #'+agent-interaction-status-create-profile)
    (define-key map (kbd "e") #'+agent-interaction-status-edit-profile)
    (define-key map (kbd "s") #'+agent-interaction-status-edit-shared-snippet)
    (define-key map (kbd "t") #'+agent-interaction-status-edit-template)
    (define-key map (kbd "o") #'+agent-interaction-status-open-output)
    (define-key map (kbd "r") #'+agent-interaction-status-open-result)
    (define-key map (kbd "k") #'+agent-interaction-status-kill-session)
    map)
  "Keymap for `+agent-interaction-status-mode'.")

(defvar-local +agent-interaction-status-project-root nil
  "Project root shown in the current status buffer.")

(define-derived-mode +agent-interaction-status-mode special-mode "AI-Status"
  "Major mode for +agent-interaction status buffers."
  (setq-local truncate-lines nil))

(defun +agent-interaction-status-buffer-name (&optional project-root)
  "Return the status buffer name for PROJECT-ROOT."
  (format "*AI Status: %s*" (+agent-interaction-project-name project-root)))

(defun +agent-interaction-status--session-live-p (backend project-root)
  "Return non-nil when BACKEND has a live session for PROJECT-ROOT."
  (pcase backend
    ('claude (and (fboundp '+agent-interaction-claude-session-live-p)
                  (+agent-interaction-claude-session-live-p project-root)))
    ('codex (and (fboundp '+agent-interaction-codex-session-live-p)
                 (+agent-interaction-codex-session-live-p project-root)))
    (_ nil)))

(defun +agent-interaction-status--format-value (label value)
  "Return a human-readable status line from LABEL and VALUE."
  (format "%-18s %s\n" label (or value "-")))

(defun +agent-interaction-status--profile-catalog (active-profile)
  "Return a compact profile catalog with ACTIVE-PROFILE marked."
  (string-join
   (mapcar
    (lambda (profile)
      (format "%s %-18s %s"
              (if (string= profile active-profile) "*" " ")
              profile
              (+agent-interaction-profile-summary profile)))
    (+agent-interaction-profile-names))
   "\n"))

(defun +agent-interaction-status--template-catalog ()
  "Return a compact template catalog."
  (string-join
   (mapcar
    (lambda (name)
      (let ((file (or (+agent-interaction-profile-locate-template-file name)
                      (+agent-interaction-profile-template-file name))))
        (format "  %-22s %s" name (abbreviate-file-name file))))
    (+agent-interaction-profile-template-names))
   "\n"))

(defun +agent-interaction-status--render (project-root)
  "Render the status view for PROJECT-ROOT."
  (let* ((backend (+agent-interaction-session-backend project-root))
         (profile (+agent-interaction-session-profile project-root))
         (profile-summary (+agent-interaction-profile-summary profile))
         (session-live (if (+agent-interaction-status--session-live-p backend project-root) "yes" "no"))
         (profile-injected (if (+agent-interaction-session-profile-injected-p backend project-root) "yes" "no"))
         (last-status (+agent-interaction-session-last-status project-root))
         (last-error (+agent-interaction-session-last-error project-root))
         (last-prompt (+agent-interaction-session-last-prompt project-root))
         (profile-file (or (+agent-interaction-profile-locate-file profile)
                           (+agent-interaction-profile-file profile))))
    (concat
     "AI Workbench\n\n"
     (+agent-interaction-status--format-value "Project" (abbreviate-file-name project-root))
     (+agent-interaction-status--format-value "Backend" (symbol-name backend))
     (+agent-interaction-status--format-value "Profile" profile)
     (+agent-interaction-status--format-value "Profile summary" profile-summary)
     (+agent-interaction-status--format-value "Profile file" (abbreviate-file-name profile-file))
     (+agent-interaction-status--format-value "Initialized" (if (+agent-interaction-session-initialized-p project-root) "yes" "no"))
     (+agent-interaction-status--format-value "Session live" session-live)
     (+agent-interaction-status--format-value "Profile injected" profile-injected)
     (+agent-interaction-status--format-value "Run state" (format "%s" (+agent-interaction-session-run-state project-root)))
     (+agent-interaction-status--format-value "Last status" last-status)
     (+agent-interaction-status--format-value "Last error" last-error)
     "\nLast prompt preview\n"
     (make-string 72 ?-)
     "\n"
     (if (string-empty-p (or last-prompt ""))
         "(empty)\n"
       (format "%s\n" (truncate-string-to-width last-prompt 200 nil nil t)))
     "\nWriting profiles\n"
     (make-string 72 ?-)
     "\n"
     (+agent-interaction-status--profile-catalog profile)
     "\n\nPrompt templates\n"
     (make-string 72 ?-)
     "\n"
     (+agent-interaction-status--template-catalog)
     "\nKeys\n"
     (make-string 72 ?-)
     "\n"
     "RET open backend  b switch backend  p switch profile  v preview profile\n"
     "+ create profile  e edit profile  s edit snippet  t edit template\n"
     "o output log  r result  k kill session  g refresh\n"
     "Writing: use C-c A w from an Org/Markdown/text buffer\n")))

(defun +agent-interaction-status-buffer (&optional project-root)
  "Return the status buffer for PROJECT-ROOT."
  (let* ((root (or project-root (+agent-interaction-project-root)))
         (buffer (get-buffer-create (+agent-interaction-status-buffer-name root))))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-interaction-status-mode)
        (+agent-interaction-status-mode))
      (setq default-directory root)
      (setq-local +agent-interaction-status-project-root root))
    buffer))

(defun +agent-interaction-status-open ()
  "Open the +agent-interaction status buffer for the current project."
  (interactive)
  (let ((buffer (+agent-interaction-status-buffer)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (+agent-interaction-status-refresh))))

(defun +agent-interaction-status-refresh ()
  "Refresh the current +agent-interaction status buffer."
  (interactive)
  (let ((project-root (or +agent-interaction-status-project-root
                          (+agent-interaction-project-root))))
    (setq +agent-interaction-status-project-root project-root)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+agent-interaction-status--render project-root))
      (goto-char (point-min)))))

(defun +agent-interaction-status-open-backend ()
  "Open the backend buffer tracked by the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (+agent-interaction-open)
    (+agent-interaction-open-backend-buffer)
    (+agent-interaction-status-refresh)))

(defun +agent-interaction-status-cycle-backend ()
  "Cycle backend from the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (+agent-interaction-cycle-backend)
    (+agent-interaction-status-refresh)))

(defun +agent-interaction-status-switch-profile ()
  "Switch profile from the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (call-interactively #'+agent-interaction-switch-profile)
    (+agent-interaction-status-refresh)))

(defun +agent-interaction-status-edit-profile ()
  "Edit the active profile from the current status buffer."
  (interactive)
  (let* ((project-root (or +agent-interaction-status-project-root
                           (+agent-interaction-project-root)))
         (profile (+agent-interaction-session-profile project-root)))
    (+agent-interaction-profile-open profile)
    (+agent-interaction-status-refresh)))

(defun +agent-interaction-status-preview-profile ()
  "Preview the active profile from the current status buffer."
  (interactive)
  (let* ((project-root (or +agent-interaction-status-project-root
                           (+agent-interaction-project-root)))
         (profile (+agent-interaction-session-profile project-root)))
    (+agent-interaction-preview-profile profile)))

(defun +agent-interaction-status-create-profile ()
  "Create a new profile from the current status buffer."
  (interactive)
  (call-interactively #'+agent-interaction-create-profile)
  (+agent-interaction-status-refresh))

(defun +agent-interaction-status-edit-shared-snippet ()
  "Edit a shared snippet from the current status buffer."
  (interactive)
  (call-interactively #'+agent-interaction-edit-shared-snippet)
  (+agent-interaction-status-refresh))

(defun +agent-interaction-status-edit-template ()
  "Edit a prompt template from the current status buffer."
  (interactive)
  (call-interactively #'+agent-interaction-edit-template)
  (+agent-interaction-status-refresh))

(defun +agent-interaction-status-open-output ()
  "Open output buffer from the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (+agent-interaction-output-open)))

(defun +agent-interaction-status-open-result ()
  "Open result buffer from the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (+agent-interaction-result-open)))

(defun +agent-interaction-status-kill-session ()
  "Kill the active session from the current status buffer."
  (interactive)
  (let ((default-directory (or +agent-interaction-status-project-root default-directory)))
    (+agent-interaction-kill)
    (+agent-interaction-status-refresh)))

(provide 'tools/agent/interaction-status)
;;; tools/agent/interaction-status.el ends here
