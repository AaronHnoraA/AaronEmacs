;;; ui/session/config.el --- Perspective session persistence -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)

(defgroup +session nil
  "Session persistence helpers."
  :group 'tools)

(config-defvar +session-auto-save nil
  "Whether to save perspective state automatically on Emacs exit."
  :type 'boolean
  :group '+session)

(declare-function persp-state-save "perspective" (&optional file interactive?))
(declare-function persp-state-load "perspective" (file))
(defvar persp-mode)
(defvar persp-state-default-file nil)

;; Set the package's own default so its native M-x commands and our shortcuts
;; agree even before either shortcut has been used.  Keep user overrides.
(unless persp-state-default-file
  (setq persp-state-default-file ; aaron-allow: config-setq (computed default; the store has none)
        (expand-file-name "var/session/perspectives.el" user-emacs-directory)))
(config-register 'persp-state-default-file
                 :group '+session
                 :type 'string
                 :doc "File used by native Perspective session save/load commands")

(defun +session--file ()
  "Return the client-local Perspective state file."
  (unless (and (stringp persp-state-default-file)
               (not (equal persp-state-default-file "")))
    (user-error "Set `persp-state-default-file' before saving a workspace session"))
  (let* ((default-directory user-emacs-directory)
         (file (expand-file-name persp-state-default-file)))
    (when (file-remote-p file)
      (user-error "Perspective state file must be local: %s" file))
    file))

(defun +session/save ()
  "Save all Perspective frames to `persp-state-default-file' atomically."
  (interactive)
  (unless (bound-and-true-p persp-mode)
    (user-error "Perspective is not active; no session was saved"))
  (let* ((file (+session--file))
         (directory (file-name-directory file))
         (default-directory user-emacs-directory)
         temporary)
    (make-directory directory t)
    (setq temporary
          (make-temp-file (expand-file-name ".perspectives." directory)))
    (unwind-protect
        (progn
          ;; Perspective remains the sole serializer.  Its default must match
          ;; the temporary target to avoid its existing-file confirmation.
          (let ((persp-state-default-file temporary))
            (persp-state-save temporary))
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun +session/load ()
  "Restore the saved Perspective frames from their configured file."
  (interactive)
  (let* ((file (+session--file))
         (default-directory user-emacs-directory))
    (unless (file-readable-p file)
      (user-error "Perspective session file is missing or unreadable: %s" file))
    (persp-state-load file)))

(defun +session-save-maybe ()
  "Save the current session when auto-save is enabled."
  (when +session-auto-save
    (condition-case err
        (+session/save)
      (error
       (display-warning '+session
                        (format "Could not save Perspective session: %s"
                                (error-message-string err))
                        :error)))))

(defalias 'workspace-session-save #'+session/save)
(defalias 'workspace-session-load #'+session/load)

(add-hook 'kill-emacs-hook #'+session-save-maybe)

(define-leader-key!
  "r s" '(:def workspace-session-save :which-key "save workspace session")
  "r l" '(:def workspace-session-load :which-key "load workspace session"))

(provide 'ui/session/config)
;;; ui/session/config.el ends here
