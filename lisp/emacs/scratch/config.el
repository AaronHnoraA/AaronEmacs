;;; emacs/scratch/config.el --- Persistent scratch buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'aaron-funcs)

(declare-function +project-current-root "tools/project/config")
(declare-function +project-name "tools/project/config" (project-root))

(defgroup +scratch nil
  "Persistent scratch buffers inspired by Doom Emacs."
  :group 'convenience)

(config-defvar +scratch-directory nil
  "Directory used to persist scratch buffers."
  :type 'directory
  :group '+scratch)

(config-defvar +scratch-initial-major-mode nil
  "Initial major mode for a fresh scratch buffer.
When set to t, inherit the current buffer's major mode when practical."
  :type '(choice (const :tag "Inherit current buffer mode" t) (const :tag "Fundamental mode" nil) (function :tag "Major mode"))
  :group '+scratch)

(defvar +scratch-buffers nil
  "Live scratch buffers managed by `init-scratch'.")

(defvar-local +scratch-storage-id nil
  "Persistent storage identifier for the current scratch buffer.")

(defvar-local +scratch-initialized nil
  "Whether the current scratch buffer has already been initialized.")

(put '+scratch-storage-id 'permanent-local t)
(put '+scratch-initialized 'permanent-local t)

(defun +scratch--default-directory (&optional project-root)
  "Return the default directory for PROJECT-ROOT scratch buffers."
  (file-name-as-directory
   (expand-file-name (or project-root default-directory))))

(defun +scratch--initial-major-mode ()
  "Return the initial major mode for a fresh scratch buffer."
  (cond
   ((eq +scratch-initial-major-mode t)
    (unless (or buffer-read-only
                (derived-mode-p 'special-mode)
                (string-match-p "^ ?\\*" (buffer-name)))
      major-mode))
   ((symbolp +scratch-initial-major-mode)
    +scratch-initial-major-mode)))

(defun +scratch--sanitize-name (name)
  "Return NAME rewritten for safe file and buffer identifiers."
  (let ((name (replace-regexp-in-string "[^[:alnum:]]+" "-" (or name "scratch"))))
    (string-trim (downcase name) "-" "-")))

(defun +scratch--project-hash (project-root)
  "Return the stable identity suffix for PROJECT-ROOT."
  (substring (secure-hash 'sha1
                          (file-name-as-directory (expand-file-name project-root)))
             0 10))

(defun +scratch--project-id (project-root)
  "Return a stable identifier for PROJECT-ROOT."
  (let* ((project-root (expand-file-name project-root))
         (label (if (fboundp '+project-name)
                    (+project-name project-root)
                  (file-name-nondirectory (directory-file-name project-root)))))
    (format "project-%s-%s"
            (+scratch--sanitize-name label)
            (+scratch--project-hash project-root))))

(defun +scratch--storage-id (&optional project-root)
  "Return the storage identifier for PROJECT-ROOT scratch buffers."
  (if project-root
      (+scratch--project-id project-root)
    "default"))

(defun +scratch--display-name (&optional project-root)
  "Return the buffer name for PROJECT-ROOT scratch buffers."
  (if project-root
      (format "*scratch:%s [%s]*"
              (if (fboundp '+project-name)
                  (+project-name project-root)
                (file-name-nondirectory (directory-file-name project-root)))
              (+scratch--project-hash project-root))
    "*scratch*"))

(defun +scratch--state-file (storage-id)
  "Return the state file path for STORAGE-ID."
  (let ((default-directory user-emacs-directory))
    (expand-file-name (concat storage-id ".el") +scratch-directory)))

(defun +scratch--read-state (storage-id)
  "Read persisted scratch state for STORAGE-ID."
  (let ((default-directory user-emacs-directory)
        (state-file (+scratch--state-file storage-id)))
    (when (file-readable-p state-file)
      (with-temp-buffer
        (insert-file-contents state-file)
        (let ((read-eval nil))
          (let ((state (read (current-buffer))))
            (skip-chars-forward " \t\n\r")
            (unless (and (eobp)
                         (pcase state
                           (`(:content ,content :point ,point :mode ,mode
                               :directory ,directory)
                            (and (stringp content) (integerp point) (>= point 1)
                                 (symbolp mode)
                                 (or (null directory) (stringp directory))))))
              (error "Invalid scratch state: %s" state-file))
            state))))))

(defun +scratch-save-buffer-h ()
  "Persist the current scratch buffer."
  (when +scratch-storage-id
    (let ((state (list :content (buffer-substring-no-properties (point-min) (point-max))
                       :point (point)
                       :mode major-mode
                       :directory default-directory))
          (state-file (+scratch--state-file +scratch-storage-id)))
      (let ((default-directory user-emacs-directory))
        (make-directory +scratch-directory t)
        (let ((print-length nil)
              (print-level nil))
          (config--write-string-atomically state-file (prin1-to-string state)))))))

(defun +scratch-unregister-buffer-h ()
  "Forget the current scratch buffer from the live scratch registry."
  (setq +scratch-buffers (delq (current-buffer) +scratch-buffers)))

(defun +scratch-save-all-buffers-h ()
  "Persist every live scratch buffer."
  (setq +scratch-buffers
        (cl-delete-if-not #'buffer-live-p +scratch-buffers))
  (dolist (buffer +scratch-buffers)
    (with-current-buffer buffer
      (+scratch-save-buffer-h))))

(defun +scratch--restore-buffer (storage-id reset-p)
  "Restore current buffer from STORAGE-ID unless RESET-P is non-nil."
  (let ((state (unless reset-p (+scratch--read-state storage-id)))
        (fallback-mode (+scratch--initial-major-mode)))
    (erase-buffer)
    (pcase state
      (`(:content ,content :point ,point :mode ,mode :directory ,directory)
       (when (functionp mode)
         (funcall mode))
       (insert content)
       (goto-char (min point (point-max)))
       ;; Restoring a remote scratch must not contact a disconnected target
       ;; just to validate the remembered working directory.
       (when (and (stringp directory) (file-name-absolute-p directory))
         (setq default-directory (file-name-as-directory directory))))
      (_
       (when (functionp fallback-mode)
         (funcall fallback-mode))
       (goto-char (point-min))))))

(defun +scratch-buffer (&optional reset-p project-root)
  "Return the persistent scratch buffer.
When RESET-P is non-nil, ignore any persisted state.
When PROJECT-ROOT is non-nil, return the scratch buffer associated with it."
  (let* ((project-root (and project-root
                            (file-name-as-directory (expand-file-name project-root))))
         (storage-id (+scratch--storage-id project-root))
         (buffer (get-buffer-create (+scratch--display-name project-root))))
    (with-current-buffer buffer
      (setq default-directory (+scratch--default-directory project-root)
            +scratch-storage-id storage-id)
      (when (or reset-p (not +scratch-initialized))
        (+scratch--restore-buffer storage-id reset-p)
        (setq +scratch-initialized t))
      (unless (memq #'+scratch-save-buffer-h kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'+scratch-save-buffer-h nil t))
      (unless (memq #'+scratch-unregister-buffer-h kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'+scratch-unregister-buffer-h t t))
      (cl-pushnew buffer +scratch-buffers)
      buffer)))

(defun +scratch/open (&optional reset-p)
  "Open the default persistent scratch buffer in another window.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (pop-to-buffer (+scratch-buffer reset-p)))

(defun +scratch/switch (&optional reset-p)
  "Switch to the default persistent scratch buffer in the current window.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (switch-to-buffer (+scratch-buffer reset-p)))

(defun +scratch/toggle (&optional reset-p)
  "Toggle the default persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let* ((buffer (+scratch-buffer reset-p))
         (window (get-buffer-window buffer nil)))
    (if window
        (quit-window nil window)
      (pop-to-buffer buffer))))

(defun +scratch/project-open (&optional reset-p)
  "Open the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let ((project-root (or (and (fboundp '+project-current-root)
                               (+project-current-root))
                          (user-error "Not inside a known project"))))
    (pop-to-buffer (+scratch-buffer reset-p project-root))))

(defun +scratch/project-switch (&optional reset-p)
  "Switch to the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let ((project-root (or (and (fboundp '+project-current-root)
                               (+project-current-root))
                          (user-error "Not inside a known project"))))
    (switch-to-buffer (+scratch-buffer reset-p project-root))))

(defun +scratch/project-toggle (&optional reset-p)
  "Toggle the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let* ((project-root (or (and (fboundp '+project-current-root)
                                (+project-current-root))
                           (user-error "Not inside a known project")))
         (buffer (+scratch-buffer reset-p project-root))
         (window (get-buffer-window buffer nil)))
    (if window
        (quit-window nil window)
      (pop-to-buffer buffer))))

(defun +scratch/revert ()
  "Reload the current scratch buffer from disk."
  (interactive)
  (unless +scratch-storage-id
    (user-error "Current buffer is not a managed scratch buffer"))
  (+scratch--restore-buffer +scratch-storage-id nil)
  (message "Scratch buffer reloaded"))

(defalias 'persistent-scratch #'+scratch/toggle
  "Toggle the persistent scratch buffer in the current frame.")
(defalias 'project-scratch #'+scratch/project-toggle
  "Toggle the current project's persistent scratch in the current frame.")

(add-hook 'kill-emacs-hook #'+scratch-save-all-buffers-h)

(aaron-leader!
  "b x" '(:def +scratch/toggle :which-key "scratch buffer")
  "b X" '(:def +scratch/switch :which-key "scratch here")
  "p x" '(:def +scratch/project-toggle :which-key "project scratch")
  "p X" '(:def +scratch/project-switch :which-key "project scratch here"))

(provide 'emacs/scratch/config)
;;; emacs/scratch/config.el ends here
