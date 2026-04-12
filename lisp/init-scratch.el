;;; init-scratch.el --- Persistent scratch buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)

(declare-function my/project-current-root "init-project")
(declare-function my/project-name "init-project" (project-root))

(defgroup my/scratch nil
  "Persistent scratch buffers inspired by Doom Emacs."
  :group 'convenience)

(defcustom my/scratch-directory
  (expand-file-name "scratch"
                    (or (and (boundp 'my/state-dir) my/state-dir)
                        (expand-file-name "var" user-emacs-directory)))
  "Directory used to persist scratch buffers."
  :type 'directory
  :group 'my/scratch)

(defcustom my/scratch-initial-major-mode t
  "Initial major mode for a fresh scratch buffer.
When set to t, inherit the current buffer's major mode when practical."
  :type '(choice
          (const :tag "Inherit current buffer mode" t)
          (const :tag "Fundamental mode" nil)
          (function :tag "Major mode"))
  :group 'my/scratch)

(defvar my/scratch-buffers nil
  "Live scratch buffers managed by `init-scratch'.")

(defvar-local my/scratch-storage-id nil
  "Persistent storage identifier for the current scratch buffer.")

(defvar-local my/scratch-initialized nil
  "Whether the current scratch buffer has already been initialized.")

(put 'my/scratch-storage-id 'permanent-local t)
(put 'my/scratch-initialized 'permanent-local t)

(defun my/scratch--default-directory (&optional project-root)
  "Return the default directory for PROJECT-ROOT scratch buffers."
  (file-name-as-directory
   (expand-file-name (or project-root default-directory))))

(defun my/scratch--initial-major-mode ()
  "Return the initial major mode for a fresh scratch buffer."
  (cond
   ((eq my/scratch-initial-major-mode t)
    (unless (or buffer-read-only
                (derived-mode-p 'special-mode)
                (string-match-p "^ ?\\*" (buffer-name)))
      major-mode))
   ((symbolp my/scratch-initial-major-mode)
    my/scratch-initial-major-mode)))

(defun my/scratch--sanitize-name (name)
  "Return NAME rewritten for safe file and buffer identifiers."
  (let ((name (replace-regexp-in-string "[^[:alnum:]]+" "-" (or name "scratch"))))
    (string-trim (downcase name) "-" "-")))

(defun my/scratch--project-id (project-root)
  "Return a stable identifier for PROJECT-ROOT."
  (let* ((project-root (expand-file-name project-root))
         (label (if (fboundp 'my/project-name)
                    (my/project-name project-root)
                  (file-name-nondirectory (directory-file-name project-root))))
         (hash (substring (secure-hash 'sha1 project-root) 0 10)))
    (format "project-%s-%s"
            (my/scratch--sanitize-name label)
            hash)))

(defun my/scratch--storage-id (&optional project-root)
  "Return the storage identifier for PROJECT-ROOT scratch buffers."
  (if project-root
      (my/scratch--project-id project-root)
    "default"))

(defun my/scratch--display-name (&optional project-root)
  "Return the buffer name for PROJECT-ROOT scratch buffers."
  (if project-root
      (format "*scratch:%s*"
              (if (fboundp 'my/project-name)
                  (my/project-name project-root)
                (file-name-nondirectory (directory-file-name project-root))))
    "*scratch*"))

(defun my/scratch--state-file (storage-id)
  "Return the state file path for STORAGE-ID."
  (expand-file-name (concat storage-id ".el") my/scratch-directory))

(defun my/scratch--read-state (storage-id)
  "Read persisted scratch state for STORAGE-ID."
  (let ((state-file (my/scratch--state-file storage-id)))
    (when (file-readable-p state-file)
      (with-temp-buffer
        (insert-file-contents state-file)
        (read (current-buffer))))))

(defun my/scratch-save-buffer-h ()
  "Persist the current scratch buffer."
  (when my/scratch-storage-id
    (make-directory my/scratch-directory t)
    (with-temp-file (my/scratch--state-file my/scratch-storage-id)
      (prin1
       (list :content (buffer-substring-no-properties (point-min) (point-max))
             :point (point)
             :mode major-mode
             :directory default-directory)
       (current-buffer)))))

(defun my/scratch-save-all-buffers-h ()
  "Persist every live scratch buffer."
  (setq my/scratch-buffers
        (cl-delete-if-not #'buffer-live-p my/scratch-buffers))
  (dolist (buffer my/scratch-buffers)
    (with-current-buffer buffer
      (my/scratch-save-buffer-h))))

(defun my/scratch--restore-buffer (storage-id reset-p)
  "Restore current buffer from STORAGE-ID unless RESET-P is non-nil."
  (let ((state (unless reset-p (my/scratch--read-state storage-id)))
        (fallback-mode (my/scratch--initial-major-mode)))
    (erase-buffer)
    (pcase state
      (`(:content ,content :point ,point :mode ,mode :directory ,directory)
       (when (functionp mode)
         (funcall mode))
       (insert content)
       (goto-char (min point (point-max)))
       (when (and directory (file-directory-p directory))
         (setq default-directory (file-name-as-directory directory))))
      (_
       (when (functionp fallback-mode)
         (funcall fallback-mode))
       (goto-char (point-min))))))

(defun my/scratch-buffer (&optional reset-p project-root)
  "Return the persistent scratch buffer.
When RESET-P is non-nil, ignore any persisted state.
When PROJECT-ROOT is non-nil, return the scratch buffer associated with it."
  (let* ((project-root (and project-root
                            (file-name-as-directory (expand-file-name project-root))))
         (storage-id (my/scratch--storage-id project-root))
         (buffer (get-buffer-create (my/scratch--display-name project-root))))
    (with-current-buffer buffer
      (setq default-directory (my/scratch--default-directory project-root)
            my/scratch-storage-id storage-id)
      (when (or reset-p (not my/scratch-initialized))
        (my/scratch--restore-buffer storage-id reset-p)
        (setq my/scratch-initialized t))
      (unless (memq #'my/scratch-save-buffer-h kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'my/scratch-save-buffer-h nil t))
      (cl-pushnew buffer my/scratch-buffers)
      buffer)))

(defun my/scratch-open (&optional reset-p)
  "Open the default persistent scratch buffer in another window.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (pop-to-buffer (my/scratch-buffer reset-p)))

(defun my/scratch-switch (&optional reset-p)
  "Switch to the default persistent scratch buffer in the current window.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (switch-to-buffer (my/scratch-buffer reset-p)))

(defun my/scratch-toggle (&optional reset-p)
  "Toggle the default persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let* ((buffer (my/scratch-buffer reset-p))
         (window (get-buffer-window buffer t)))
    (if window
        (quit-window nil window)
      (pop-to-buffer buffer))))

(defun my/scratch-project-open (&optional reset-p)
  "Open the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let ((project-root (or (and (fboundp 'my/project-current-root)
                               (my/project-current-root))
                          (user-error "Not inside a known project"))))
    (pop-to-buffer (my/scratch-buffer reset-p project-root))))

(defun my/scratch-project-switch (&optional reset-p)
  "Switch to the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let ((project-root (or (and (fboundp 'my/project-current-root)
                               (my/project-current-root))
                          (user-error "Not inside a known project"))))
    (switch-to-buffer (my/scratch-buffer reset-p project-root))))

(defun my/scratch-project-toggle (&optional reset-p)
  "Toggle the current project's persistent scratch buffer.
With RESET-P, start from a fresh scratch."
  (interactive "P")
  (let* ((project-root (or (and (fboundp 'my/project-current-root)
                                (my/project-current-root))
                           (user-error "Not inside a known project")))
         (buffer (my/scratch-buffer reset-p project-root))
         (window (get-buffer-window buffer t)))
    (if window
        (quit-window nil window)
      (pop-to-buffer buffer))))

(defun my/scratch-revert ()
  "Reload the current scratch buffer from disk."
  (interactive)
  (unless my/scratch-storage-id
    (user-error "Current buffer is not a managed scratch buffer"))
  (my/scratch--restore-buffer my/scratch-storage-id nil)
  (message "Scratch buffer reloaded"))

(add-hook 'kill-emacs-hook #'my/scratch-save-all-buffers-h)

(my/evil-global-leader-set "b x" #'my/scratch-toggle "scratch buffer")
(my/evil-global-leader-set "b X" #'my/scratch-switch "scratch here")
(my/evil-global-leader-set "p x" #'my/scratch-project-toggle "project scratch")
(my/evil-global-leader-set "p X" #'my/scratch-project-switch "project scratch here")

(provide 'init-scratch)
;;; init-scratch.el ends here
