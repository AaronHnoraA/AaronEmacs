;;; ai-workbench-status.el --- Status buffer for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a lightweight project-scoped status buffer.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)

(declare-function ai-workbench-open "ai-workbench" ())
(declare-function ai-workbench-kill "ai-workbench" ())
(declare-function ai-workbench-open-backend-buffer "ai-workbench" ())
(declare-function ai-workbench-switch-profile "ai-workbench" (&optional profile))
(declare-function ai-workbench-preview-profile "ai-workbench" (&optional profile))
(declare-function ai-workbench-create-profile "ai-workbench" (name &optional base-profile))
(declare-function ai-workbench-edit-shared-snippet "ai-workbench" (&optional name))
(declare-function ai-workbench-cycle-backend "ai-workbench" ())
(declare-function ai-workbench-compose-buffer "ai-workbench-compose" (&optional no-display))
(declare-function ai-workbench-output-open "ai-workbench-output" ())
(declare-function ai-workbench-result-open "ai-workbench-result" ())
(declare-function ai-workbench-claude-session-live-p "ai-workbench-adapter-claude" (&optional project-root))
(declare-function ai-workbench-codex-session-live-p "ai-workbench-adapter-codex" (&optional project-root))

(defvar ai-workbench-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ai-workbench-status-refresh)
    (define-key map (kbd "RET") #'ai-workbench-status-open-backend)
    (define-key map (kbd "b") #'ai-workbench-status-cycle-backend)
    (define-key map (kbd "p") #'ai-workbench-status-switch-profile)
    (define-key map (kbd "v") #'ai-workbench-status-preview-profile)
    (define-key map (kbd "+") #'ai-workbench-status-create-profile)
    (define-key map (kbd "e") #'ai-workbench-status-edit-profile)
    (define-key map (kbd "s") #'ai-workbench-status-edit-shared-snippet)
    (define-key map (kbd "c") #'ai-workbench-status-open-compose)
    (define-key map (kbd "o") #'ai-workbench-status-open-output)
    (define-key map (kbd "r") #'ai-workbench-status-open-result)
    (define-key map (kbd "k") #'ai-workbench-status-kill-session)
    map)
  "Keymap for `ai-workbench-status-mode'.")

(defvar-local ai-workbench-status-project-root nil
  "Project root shown in the current status buffer.")

(define-derived-mode ai-workbench-status-mode special-mode "AI-Status"
  "Major mode for ai-workbench status buffers."
  (setq-local truncate-lines nil))

(defun ai-workbench-status-buffer-name (&optional project-root)
  "Return the status buffer name for PROJECT-ROOT."
  (format "*AI Status: %s*" (ai-workbench-project-name project-root)))

(defun ai-workbench-status--session-live-p (backend project-root)
  "Return non-nil when BACKEND has a live session for PROJECT-ROOT."
  (pcase backend
    ('claude (and (fboundp 'ai-workbench-claude-session-live-p)
                  (ai-workbench-claude-session-live-p project-root)))
    ('codex (and (fboundp 'ai-workbench-codex-session-live-p)
                 (ai-workbench-codex-session-live-p project-root)))
    (_ nil)))

(defun ai-workbench-status--format-value (label value)
  "Return a human-readable status line from LABEL and VALUE."
  (format "%-18s %s\n" label (or value "-")))

(defun ai-workbench-status--render (project-root)
  "Render the status view for PROJECT-ROOT."
  (let* ((backend (ai-workbench-session-backend project-root))
         (profile (ai-workbench-session-profile project-root))
         (profile-summary (ai-workbench-profile-summary profile))
         (session-live (if (ai-workbench-status--session-live-p backend project-root) "yes" "no"))
         (profile-injected (if (ai-workbench-session-profile-injected-p backend project-root) "yes" "no"))
         (last-status (ai-workbench-session-last-status project-root))
         (last-error (ai-workbench-session-last-error project-root))
         (last-prompt (ai-workbench-session-last-prompt project-root))
         (profile-file (or (ai-workbench-profile-locate-file profile)
                           (ai-workbench-profile-file profile))))
    (concat
     "AI Workbench\n\n"
     (ai-workbench-status--format-value "Project" (abbreviate-file-name project-root))
     (ai-workbench-status--format-value "Backend" (symbol-name backend))
     (ai-workbench-status--format-value "Profile" profile)
     (ai-workbench-status--format-value "Profile summary" profile-summary)
     (ai-workbench-status--format-value "Profile file" (abbreviate-file-name profile-file))
     (ai-workbench-status--format-value "Initialized" (if (ai-workbench-session-initialized-p project-root) "yes" "no"))
     (ai-workbench-status--format-value "Session live" session-live)
     (ai-workbench-status--format-value "Profile injected" profile-injected)
     (ai-workbench-status--format-value "Run state" (format "%s" (ai-workbench-session-run-state project-root)))
     (ai-workbench-status--format-value "Last status" last-status)
     (ai-workbench-status--format-value "Last error" last-error)
     "\nLast prompt preview\n"
     (make-string 72 ?-)
     "\n"
     (if (string-empty-p (or last-prompt ""))
         "(empty)\n"
       (format "%s\n" (truncate-string-to-width last-prompt 200 nil nil t)))
     "\nKeys\n"
     (make-string 72 ?-)
     "\n"
     "RET open backend  b switch backend  p switch profile  v preview profile\n"
     "+ create profile  e edit profile  s edit shared snippet\n"
     "c compose  o output log  r result  k kill session  g refresh\n")))

(defun ai-workbench-status-buffer (&optional project-root)
  "Return the status buffer for PROJECT-ROOT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (buffer (get-buffer-create (ai-workbench-status-buffer-name root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-status-mode)
        (ai-workbench-status-mode))
      (setq default-directory root)
      (setq-local ai-workbench-status-project-root root))
    buffer))

(defun ai-workbench-status-open ()
  "Open the ai-workbench status buffer for the current project."
  (interactive)
  (let ((buffer (ai-workbench-status-buffer)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (ai-workbench-status-refresh))))

(defun ai-workbench-status-refresh ()
  "Refresh the current ai-workbench status buffer."
  (interactive)
  (let ((project-root (or ai-workbench-status-project-root
                          (ai-workbench-project-root))))
    (setq ai-workbench-status-project-root project-root)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (ai-workbench-status--render project-root))
      (goto-char (point-min)))))

(defun ai-workbench-status-open-backend ()
  "Open the backend buffer tracked by the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-open)
    (ai-workbench-open-backend-buffer)
    (ai-workbench-status-refresh)))

(defun ai-workbench-status-cycle-backend ()
  "Cycle backend from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-cycle-backend)
    (ai-workbench-status-refresh)))

(defun ai-workbench-status-switch-profile ()
  "Switch profile from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (call-interactively #'ai-workbench-switch-profile)
    (ai-workbench-status-refresh)))

(defun ai-workbench-status-edit-profile ()
  "Edit the active profile from the current status buffer."
  (interactive)
  (let* ((project-root (or ai-workbench-status-project-root
                           (ai-workbench-project-root)))
         (profile (ai-workbench-session-profile project-root)))
    (ai-workbench-profile-open profile)
    (ai-workbench-status-refresh)))

(defun ai-workbench-status-preview-profile ()
  "Preview the active profile from the current status buffer."
  (interactive)
  (let* ((project-root (or ai-workbench-status-project-root
                           (ai-workbench-project-root)))
         (profile (ai-workbench-session-profile project-root)))
    (ai-workbench-preview-profile profile)))

(defun ai-workbench-status-create-profile ()
  "Create a new profile from the current status buffer."
  (interactive)
  (call-interactively #'ai-workbench-create-profile)
  (ai-workbench-status-refresh))

(defun ai-workbench-status-edit-shared-snippet ()
  "Edit a shared snippet from the current status buffer."
  (interactive)
  (call-interactively #'ai-workbench-edit-shared-snippet)
  (ai-workbench-status-refresh))

(defun ai-workbench-status-open-compose ()
  "Open compose buffer from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-compose-buffer)))

(defun ai-workbench-status-open-output ()
  "Open output buffer from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-output-open)))

(defun ai-workbench-status-open-result ()
  "Open result buffer from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-result-open)))

(defun ai-workbench-status-kill-session ()
  "Kill the active session from the current status buffer."
  (interactive)
  (let ((default-directory (or ai-workbench-status-project-root default-directory)))
    (ai-workbench-kill)
    (ai-workbench-status-refresh)))

(provide 'ai-workbench-status)
;;; ai-workbench-status.el ends here
