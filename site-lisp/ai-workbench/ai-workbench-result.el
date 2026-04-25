;;; ai-workbench-result.el --- Result buffer support for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a shared result buffer for ai-workbench backend output.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)

(defconst ai-workbench-result-kind-labels
  '((prompt . "Prompt")
    (assistant . "Assistant")
    (tool . "Tool")
    (status . "Status")
    (error . "Error")
    (meta . "Meta")
    (event . "Event")
    (raw . "Raw"))
  "Display labels used in result buffers.")

(defvar ai-workbench-result-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ai-workbench-result-refresh)
    (define-key map (kbd "k") #'ai-workbench-result-clear)
    map)
  "Keymap for `ai-workbench-result-mode'.")

(define-derived-mode ai-workbench-result-mode special-mode "AI-Result"
  "Major mode for ai-workbench result buffers."
  (setq-local truncate-lines nil))

(declare-function ai-workbench-frontend-append "ai-workbench" (kind text &optional project-root))

(defun ai-workbench-result-buffer-name (&optional project-root)
  "Return the result buffer name for PROJECT-ROOT."
  (format "*AI Result: %s*" (ai-workbench-project-name project-root)))

(defun ai-workbench-result-buffer (&optional project-root)
  "Return the result buffer for PROJECT-ROOT."
  (let ((buffer (get-buffer-create (ai-workbench-result-buffer-name project-root))))
    (with-current-buffer buffer
      (setq default-directory (or project-root (ai-workbench-project-root))))
    buffer))

(defun ai-workbench-result-open ()
  "Open the ai-workbench result buffer for the current project."
  (interactive)
  (let ((buffer (ai-workbench-result-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-result-mode)
        (ai-workbench-result-mode)))
    (pop-to-buffer buffer)))

(defun ai-workbench-result-refresh ()
  "Refresh the ai-workbench result buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-result-mode)
        (ai-workbench-result-mode)))))

(defun ai-workbench-result-clear ()
  "Clear the ai-workbench result buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ai-workbench-result-append (kind text &optional project-root)
  "Append TEXT with KIND to the result buffer for PROJECT-ROOT."
  (let ((buffer (ai-workbench-result-buffer project-root))
        (timestamp (format-time-string "%H:%M:%S"))
        (label (or (alist-get kind ai-workbench-result-kind-labels)
                   (capitalize (format "%s" kind)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-result-mode)
        (ai-workbench-result-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "%s  %s\n" label timestamp)
                            'face 'bold))
        (insert (string-trim-right text))
        (insert "\n\n")
        (goto-char (point-max)))))
  (when (and (memq kind '(assistant tool status error meta event))
             (fboundp 'ai-workbench-frontend-append))
    (ai-workbench-frontend-append kind text project-root)))

(provide 'ai-workbench-result)
;;; ai-workbench-result.el ends here
