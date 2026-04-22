;;; init-session.el --- Perspective session persistence -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(defgroup my/session nil
  "Session persistence helpers."
  :group 'tools)

(defcustom my/session-auto-save nil
  "Whether to save perspective state automatically on Emacs exit."
  :type 'boolean
  :group 'my/session)

(defconst my/session-default-file
  (expand-file-name "perspectives.el"
                    (expand-file-name "session"
                                      (expand-file-name "var" user-emacs-directory))))

(declare-function persp-state-save "perspective" (&optional file interactive?))
(declare-function persp-state-load "perspective" (file))
(defvar persp-state-default-file)

(defun my/session--ensure-default-file ()
  "Prepare `persp-state-default-file'."
  (make-directory (file-name-directory my/session-default-file) t)
  (setq persp-state-default-file my/session-default-file))

(defun my/session-save ()
  "Save the current perspective session."
  (interactive)
  (my/session--ensure-default-file)
  (persp-state-save my/session-default-file))

(defun my/session-load ()
  "Load the saved perspective session."
  (interactive)
  (my/session--ensure-default-file)
  (persp-state-load my/session-default-file))

(defun my/session-save-maybe ()
  "Save the current session when auto-save is enabled."
  (when my/session-auto-save
    (ignore-errors
      (my/session-save))))

(add-hook 'kill-emacs-hook #'my/session-save-maybe)

(my/leader!
  "r s" '(:def my/session-save :which-key "save session")
  "r l" '(:def my/session-load :which-key "load session"))

(provide 'init-session)
;;; init-session.el ends here
