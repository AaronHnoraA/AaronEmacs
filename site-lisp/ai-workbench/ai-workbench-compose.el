;;; ai-workbench-compose.el --- Compose buffer support for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides editable compose buffers for ai-workbench prompts.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)

(declare-function ai-workbench-open "ai-workbench" ())
(declare-function ai-workbench-switch-profile "ai-workbench" (&optional profile))
(declare-function ai-workbench-open-backend-buffer "ai-workbench" ())
(declare-function ai-workbench-output-append "ai-workbench-output" (kind text &optional project-root))
(declare-function ai-workbench-send-string "ai-workbench" (backend prompt &optional project-root))
(declare-function my/vterm-popup-display-buffer "init-vterm-popup" (buffer))
(declare-function turn-off-evil-mode "evil" ())
(declare-function evil-emacs-state "evil" ())

(defvar my/vterm-popup-kind)
(defvar my/vterm-popup-title)

(defvar ai-workbench-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'ai-workbench-compose-send)
    (define-key map (kbd "C-c C-k") #'ai-workbench-compose-clear)
    (define-key map (kbd "C-c C-b") #'ai-workbench-compose-cycle-backend)
    (define-key map (kbd "C-c C-p") #'ai-workbench-compose-switch-profile)
    (define-key map (kbd "C-c C-r") #'ai-workbench-compose-insert-region)
    (define-key map (kbd "C-c C-f") #'ai-workbench-compose-insert-file)
    (define-key map (kbd "C-c C-e") #'ai-workbench-compose-insert-current-buffer)
    (define-key map (kbd "C-c C-o") #'ai-workbench-open)
    map)
  "Keymap for `ai-workbench-compose-mode'.")

(defvar-local ai-workbench-compose-project-root nil
  "Project root associated with the current compose buffer.")

(defvar-local ai-workbench-compose-backend nil
  "Backend selected for the current compose buffer.")

(define-derived-mode ai-workbench-compose-mode text-mode "AI-Compose"
  "Major mode for ai-workbench compose buffers."
  (setq-local header-line-format
              '(:eval
                (format "AI Compose  backend:%s  profile:%s  project:%s  C-c C-c send  C-c C-b backend  C-c C-p profile  C-c C-r/C-f/C-e inject"
                        ai-workbench-compose-backend
                        (ai-workbench-session-profile ai-workbench-compose-project-root)
                        (abbreviate-file-name ai-workbench-compose-project-root)))))

(defun ai-workbench-compose--relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when possible."
  (if (and file project-root
           (string-prefix-p (expand-file-name project-root)
                            (expand-file-name file)))
      (file-relative-name file project-root)
    (abbreviate-file-name file)))

(defun ai-workbench-compose--insert-context-block (label body &optional metadata)
  "Insert a labeled context block with LABEL, BODY, and optional METADATA."
  (goto-char (point-max))
  (unless (bobp)
    (insert "\n"))
  (insert (format "### %s\n" label))
  (when (and metadata (not (string-empty-p metadata)))
    (insert metadata "\n"))
  (insert body)
  (unless (or (bobp) (eq (char-before) ?\n))
    (insert "\n"))
  (insert "\n"))

(defun ai-workbench-compose--fenced-block (content &optional info-string)
  "Return CONTENT wrapped in a fenced block with INFO-STRING."
  (let ((info (or info-string "")))
    (format "```%s\n%s\n```"
            info
            (string-trim-right content))))

(defun ai-workbench-compose-buffer (&optional no-display)
  "Create or display the compose buffer for the current project.
When NO-DISPLAY is non-nil, return the buffer without popping it."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (buffer (or (ai-workbench-session-compose-buffer project-root)
                     (get-buffer (ai-workbench-compose-buffer-name project-root))
                     (generate-new-buffer (ai-workbench-compose-buffer-name project-root))))
         (backend (ai-workbench-session-backend project-root)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-compose-mode)
        (ai-workbench-compose-mode))
      (setq default-directory project-root)
      (setq-local ai-workbench-compose-project-root project-root)
      (setq-local ai-workbench-compose-backend backend)
      (setq-local my/vterm-popup-kind 'ai-compose)
      (setq-local my/vterm-popup-title
                  (format "Prompt  %s" (abbreviate-file-name project-root)))
      (when (fboundp 'evil-emacs-state)
        (evil-emacs-state))
      (when (bound-and-true-p evil-local-mode)
        (turn-off-evil-mode)))
    (ai-workbench-session-set-compose-buffer buffer project-root)
    (unless no-display
      (require 'init-vterm-popup nil t)
      (if (fboundp 'my/vterm-popup-display-buffer)
          (my/vterm-popup-display-buffer buffer)
        (pop-to-buffer buffer)))
    buffer))

(defun ai-workbench-compose-clear ()
  "Clear the current compose buffer."
  (interactive)
  (erase-buffer))

(defun ai-workbench-compose-cycle-backend ()
  "Cycle the compose buffer backend between Claude and Codex."
  (interactive)
  (setq-local ai-workbench-compose-backend
              (if (eq ai-workbench-compose-backend 'claude) 'codex 'claude))
  (ai-workbench-session-set-backend ai-workbench-compose-backend
                                    ai-workbench-compose-project-root)
  (force-mode-line-update)
  (message "ai-workbench backend: %s" ai-workbench-compose-backend))

(defun ai-workbench-compose-switch-profile ()
  "Switch the project profile from the compose buffer."
  (interactive)
  (let ((default-directory ai-workbench-compose-project-root))
    (call-interactively #'ai-workbench-switch-profile)
    (force-mode-line-update)))

(defun ai-workbench-compose-send ()
  "Send the current compose buffer text to the selected backend."
  (interactive)
  (let* ((project-root ai-workbench-compose-project-root)
         (prompt (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p prompt)
      (user-error "Compose buffer is empty"))
    (ai-workbench-send-string ai-workbench-compose-backend
                              prompt
                              project-root)
    (erase-buffer)
    (when (fboundp 'ai-workbench-open-backend-buffer)
      (ai-workbench-open-backend-buffer))))

(defun ai-workbench-compose-insert-region (start end)
  "Insert the active region from the previous buffer into the compose buffer."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer))
         (project-root (ai-workbench-project-root))
         (content (buffer-substring-no-properties start end))
         (language (or (and source-file (file-name-extension source-file))
                       (symbol-name major-mode)))
         (metadata (if source-file
                       (format "source: %s"
                               (ai-workbench-compose--relative-path source-file project-root))
                     (format "buffer: %s" (buffer-name source-buffer))))
         (compose-buffer (ai-workbench-compose-buffer)))
    (with-current-buffer compose-buffer
      (ai-workbench-compose--insert-context-block
       "Context: region"
       (ai-workbench-compose--fenced-block content language)
       metadata))
    (ai-workbench-output-append
     'context
     (format "Inserted region from %s"
             (or source-file (buffer-name source-buffer)))
     project-root)))

(defun ai-workbench-compose-insert-current-buffer ()
  "Insert the full contents of the current buffer into the compose buffer."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer))
         (project-root (ai-workbench-project-root))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (language (or (and source-file (file-name-extension source-file))
                       (symbol-name major-mode)))
         (metadata (if source-file
                       (format "source: %s"
                               (ai-workbench-compose--relative-path source-file project-root))
                     (format "buffer: %s" (buffer-name source-buffer))))
         (compose-buffer (ai-workbench-compose-buffer)))
    (with-current-buffer compose-buffer
      (ai-workbench-compose--insert-context-block
       "Context: current buffer"
       (ai-workbench-compose--fenced-block content language)
       metadata))
    (ai-workbench-output-append
     'context
     (format "Inserted current buffer from %s"
             (or source-file (buffer-name source-buffer)))
     project-root)))

(defun ai-workbench-compose-insert-file (file)
  "Insert FILE contents into the compose buffer."
  (interactive
   (let ((project-root (ai-workbench-project-root)))
     (list (read-file-name "Insert file: " project-root nil t))))
  (let* ((project-root (ai-workbench-project-root))
         (expanded (expand-file-name file))
         (content (with-temp-buffer
                    (insert-file-contents expanded)
                    (buffer-string)))
         (language (or (file-name-extension expanded) "text"))
         (metadata (format "source: %s"
                           (ai-workbench-compose--relative-path expanded project-root)))
         (compose-buffer (ai-workbench-compose-buffer)))
    (with-current-buffer compose-buffer
      (ai-workbench-compose--insert-context-block
       "Context: file"
       (ai-workbench-compose--fenced-block content language)
       metadata))
    (ai-workbench-output-append
     'context
     (format "Inserted file %s"
             (ai-workbench-compose--relative-path expanded project-root))
     project-root)))

(provide 'ai-workbench-compose)
;;; ai-workbench-compose.el ends here
