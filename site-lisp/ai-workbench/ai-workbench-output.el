;;; ai-workbench-output.el --- Output buffer support for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a shared output buffer for ai-workbench events.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)

(defvar ai-workbench-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ai-workbench-output-refresh)
    (define-key map (kbd "k") #'ai-workbench-output-clear)
    map)
  "Keymap for `ai-workbench-output-mode'.")

(define-derived-mode ai-workbench-output-mode special-mode "AI-Output"
  "Major mode for ai-workbench output buffers.")

(defun ai-workbench-output-buffer-name (&optional project-root)
  "Return the output buffer name for PROJECT-ROOT."
  (format "*AI Output: %s*" (ai-workbench-project-name project-root)))

(defun ai-workbench-output-buffer (&optional project-root)
  "Return the output buffer for PROJECT-ROOT."
  (let ((buffer (get-buffer-create (ai-workbench-output-buffer-name project-root))))
    (with-current-buffer buffer
      (setq default-directory (or project-root (ai-workbench-project-root))))
    buffer))

(defun ai-workbench-output-open ()
  "Open the ai-workbench output buffer for the current project."
  (interactive)
  (let ((buffer (ai-workbench-output-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-output-mode)
        (ai-workbench-output-mode)))
    (pop-to-buffer buffer)))

(defun ai-workbench-output-refresh ()
  "Refresh the ai-workbench output buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-output-mode)
        (ai-workbench-output-mode)))))

(defun ai-workbench-output-clear ()
  "Clear the ai-workbench output buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ai-workbench-output-append (kind text &optional project-root)
  "Append TEXT with KIND to the output buffer for PROJECT-ROOT."
  (let ((buffer (ai-workbench-output-buffer project-root))
        (timestamp (format-time-string "%H:%M:%S")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-output-mode)
        (ai-workbench-output-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] %s\n%s\n\n"
                        timestamp
                        (upcase (format "%s" kind))
                        (string-trim-right text))))))
  nil)

(provide 'ai-workbench-output)
;;; ai-workbench-output.el ends here
