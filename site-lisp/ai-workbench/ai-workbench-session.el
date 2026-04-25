;;; ai-workbench-session.el --- Session model for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module owns project-scoped state for ai-workbench.

;;; Code:

(require 'project)

(defvar ai-workbench-default-backend 'claude
  "Default backend used for new ai-workbench sessions.")

(defvar ai-workbench--session-table (make-hash-table :test 'equal)
  "Hashtable mapping project roots to ai-workbench session plists.")

(defun ai-workbench-project-root ()
  "Return the current project root or `default-directory'."
  (if-let* ((project (project-current nil default-directory)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun ai-workbench--normalize-backend (backend)
  "Return BACKEND when supported, otherwise fall back to the default backend."
  (if (memq backend '(claude codex))
      backend
    ai-workbench-default-backend))

(defun ai-workbench-session-get (&optional project-root)
  "Return the ai-workbench session plist for PROJECT-ROOT."
  (let ((root (or project-root (ai-workbench-project-root))))
    (or (gethash root ai-workbench--session-table)
        (let ((session (list :project-root root
                             :backend ai-workbench-default-backend
                             :profile "default"
                             :initialized nil
                             :profile-injected-backends nil
                             :compose-buffer nil
                             :last-prompt nil
                             :last-status nil
                             :last-error nil
                             :run-state 'idle)))
          (puthash root session ai-workbench--session-table)
          session))))

(defun ai-workbench-session-backend (&optional project-root)
  "Return the backend configured for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :backend))

(defun ai-workbench-session-profile (&optional project-root)
  "Return the profile configured for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :profile))

(defun ai-workbench-session-set-backend (backend &optional project-root)
  "Store BACKEND in the session for PROJECT-ROOT and return the backend."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root)))
         (value (ai-workbench--normalize-backend backend)))
    (setq session (plist-put session :backend value))
    (puthash root session ai-workbench--session-table)
    value))

(defun ai-workbench-session-set-profile (profile &optional project-root)
  "Store PROFILE in the session for PROJECT-ROOT and return it."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :profile profile))
    (puthash root session ai-workbench--session-table)
    profile))

(defun ai-workbench-session-initialized-p (&optional project-root)
  "Return non-nil when PROJECT-ROOT has completed initial workbench setup."
  (plist-get (ai-workbench-session-get project-root) :initialized))

(defun ai-workbench-session-set-initialized (value &optional project-root)
  "Store VALUE as the initialized flag for PROJECT-ROOT and return VALUE."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :initialized value))
    (puthash root session ai-workbench--session-table)
    value))

(defun ai-workbench-session-profile-injected-backends (&optional project-root)
  "Return the backends with injected profile state for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :profile-injected-backends))

(defun ai-workbench-session-profile-injected-p (backend &optional project-root)
  "Return non-nil when BACKEND already got profile injection for PROJECT-ROOT."
  (memq backend (ai-workbench-session-profile-injected-backends project-root)))

(defun ai-workbench-session-mark-profile-injected (backend &optional project-root)
  "Record BACKEND as having completed profile injection for PROJECT-ROOT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root)))
         (backends (plist-get session :profile-injected-backends)))
    (unless (memq backend backends)
      (setq backends (cons backend backends)))
    (setq session (plist-put session :profile-injected-backends backends))
    (puthash root session ai-workbench--session-table)
    backends))

(defun ai-workbench-session-clear-profile-injected (backend &optional project-root)
  "Clear profile-injected marker for BACKEND in PROJECT-ROOT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root)))
         (backends (delq backend (copy-sequence
                                 (plist-get session :profile-injected-backends)))))
    (setq session (plist-put session :profile-injected-backends backends))
    (puthash root session ai-workbench--session-table)
    backends))

(defun ai-workbench-session-compose-buffer (&optional project-root)
  "Return the compose buffer stored for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :compose-buffer))

(defun ai-workbench-session-set-compose-buffer (buffer &optional project-root)
  "Store BUFFER as the compose buffer for PROJECT-ROOT and return BUFFER."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :compose-buffer buffer))
    (puthash root session ai-workbench--session-table)
    buffer))

(defun ai-workbench-project-name (&optional project-root)
  "Return a short display name for PROJECT-ROOT."
  (file-name-nondirectory
   (directory-file-name (or project-root (ai-workbench-project-root)))))

(defun ai-workbench-session-last-prompt (&optional project-root)
  "Return the last prompt stored for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :last-prompt))

(defun ai-workbench-session-set-last-prompt (prompt &optional project-root)
  "Store PROMPT as the last prompt for PROJECT-ROOT and return PROMPT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :last-prompt prompt))
    (puthash root session ai-workbench--session-table)
    prompt))

(defun ai-workbench-session-last-status (&optional project-root)
  "Return the last status string stored for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :last-status))

(defun ai-workbench-session-set-last-status (status &optional project-root)
  "Store STATUS as the last status for PROJECT-ROOT and return STATUS."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :last-status status))
    (puthash root session ai-workbench--session-table)
    status))

(defun ai-workbench-session-last-error (&optional project-root)
  "Return the last error string stored for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :last-error))

(defun ai-workbench-session-set-last-error (error-text &optional project-root)
  "Store ERROR-TEXT as the last error for PROJECT-ROOT and return ERROR-TEXT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :last-error error-text))
    (puthash root session ai-workbench--session-table)
    error-text))

(defun ai-workbench-session-run-state (&optional project-root)
  "Return the run state stored for PROJECT-ROOT."
  (plist-get (ai-workbench-session-get project-root) :run-state))

(defun ai-workbench-session-set-run-state (state &optional project-root)
  "Store STATE as the run state for PROJECT-ROOT and return STATE."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :run-state state))
    (puthash root session ai-workbench--session-table)
    state))

(defun ai-workbench-session-clear-runtime (&optional project-root)
  "Clear transient runtime state for PROJECT-ROOT."
  (let* ((root (or project-root (ai-workbench-project-root)))
         (session (copy-sequence (ai-workbench-session-get root))))
    (setq session (plist-put session :last-status nil))
    (setq session (plist-put session :last-error nil))
    (setq session (plist-put session :run-state 'idle))
    (puthash root session ai-workbench--session-table)
    session))

(defun ai-workbench-compose-buffer-name (&optional project-root)
  "Return the compose buffer name for PROJECT-ROOT."
  (format "*AI Compose: %s*" (ai-workbench-project-name project-root)))

(defun ai-workbench-panel-buffer-name (&optional project-root)
  "Return the control panel buffer name for PROJECT-ROOT."
  (format "*AI Workbench: %s*" (ai-workbench-project-name project-root)))

(provide 'ai-workbench-session)
;;; ai-workbench-session.el ends here
