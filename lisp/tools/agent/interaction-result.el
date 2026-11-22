;;; tools/agent/interaction-result.el --- Result buffer support for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a shared result buffer for +agent-interaction backend output.

;;; Code:

(require 'subr-x)
(require 'tools/agent/interaction-session)

(defconst +agent-interaction-result-kind-labels
  '((prompt . "Prompt")
    (assistant . "Assistant")
    (tool . "Tool")
    (status . "Status")
    (error . "Error")
    (meta . "Meta")
    (event . "Event")
    (raw . "Raw"))
  "Display labels used in result buffers.")

(defvar +agent-interaction-result-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+agent-interaction-result-refresh)
    (define-key map (kbd "k") #'+agent-interaction-result-clear)
    map)
  "Keymap for `+agent-interaction-result-mode'.")

(define-derived-mode +agent-interaction-result-mode special-mode "AI-Result"
  "Major mode for +agent-interaction result buffers."
  (setq-local truncate-lines nil))

(declare-function +agent-interaction-frontend-append "tools/agent/interaction" (kind text &optional project-root))

(defun +agent-interaction-result-buffer-name (&optional project-root)
  "Return the result buffer name for PROJECT-ROOT."
  (format "*AI Result: %s*" (+agent-interaction-project-name project-root)))

(defun +agent-interaction-result-buffer (&optional project-root)
  "Return the result buffer for PROJECT-ROOT."
  (let ((buffer (get-buffer-create (+agent-interaction-result-buffer-name project-root))))
    (with-current-buffer buffer
      (setq default-directory (or project-root (+agent-interaction-project-root))))
    buffer))

(defun +agent-interaction-result-open ()
  "Open the +agent-interaction result buffer for the current project."
  (interactive)
  (let ((buffer (+agent-interaction-result-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-interaction-result-mode)
        (+agent-interaction-result-mode)))
    (pop-to-buffer buffer)))

(defun +agent-interaction-result-refresh ()
  "Refresh the +agent-interaction result buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-interaction-result-mode)
        (+agent-interaction-result-mode)))))

(defun +agent-interaction-result-clear ()
  "Clear the +agent-interaction result buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun +agent-interaction-result-append (kind text &optional project-root)
  "Append TEXT with KIND to the result buffer for PROJECT-ROOT."
  (let ((buffer (+agent-interaction-result-buffer project-root))
        (timestamp (format-time-string "%H:%M:%S"))
        (label (or (alist-get kind +agent-interaction-result-kind-labels)
                   (capitalize (format "%s" kind)))))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-interaction-result-mode)
        (+agent-interaction-result-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "%s  %s\n" label timestamp)
                            'face 'bold))
        (insert (string-trim-right text))
        (insert "\n\n")
        (goto-char (point-max)))))
  (when (and (memq kind '(assistant tool status error meta event))
             (fboundp '+agent-interaction-frontend-append))
    (+agent-interaction-frontend-append kind text project-root)))

(provide 'tools/agent/interaction-result)
;;; tools/agent/interaction-result.el ends here
