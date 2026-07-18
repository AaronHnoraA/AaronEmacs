;;; ai-workbench-output.el --- Output buffer support for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a shared output buffer for ai-workbench events.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)

(defcustom ai-workbench-output-max-bytes (* 16 1024 1024)
  "Soft maximum size of one project output buffer.
Trimming runs only after appends and preserves a generous tail so streaming
does not trigger whole-buffer work for each small delta."
  :type 'integer
  :group 'ai-workbench)

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

(defun ai-workbench-output--trim-if-needed ()
  "Trim the oldest output in the current buffer after its soft size cap."
  (let ((buffer-bytes (1- (position-bytes (point-max)))))
    (when (and (integerp ai-workbench-output-max-bytes)
               (> ai-workbench-output-max-bytes 0)
               (> buffer-bytes ai-workbench-output-max-bytes))
      (let* ((tail-bytes (floor (* ai-workbench-output-max-bytes 0.75)))
             (target-byte (max 1 (- (position-bytes (point-max)) tail-bytes)))
             (target (or (byte-to-position target-byte) (point-min)))
             (cut (save-excursion
                    (goto-char target)
                    (or (search-forward "\n\n[" nil t) target))))
        (delete-region (point-min) cut)))))

(defun ai-workbench-output-stream-start (kind &optional metadata project-root)
  "Start a streaming KIND block and return its insertion marker.
METADATA is inserted once above the streamed body for PROJECT-ROOT."
  (let ((buffer (ai-workbench-output-buffer project-root))
        (timestamp (format-time-string "%H:%M:%S")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-workbench-output-mode)
        (ai-workbench-output-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp (upcase (format "%s" kind))))
        (when (and (stringp metadata) (not (string-empty-p metadata)))
          (insert metadata "\n"))
        (let ((marker (copy-marker (point) t)))
          (ai-workbench-output--trim-if-needed)
          marker)))))

(defun ai-workbench-output-stream-append (marker text)
  "Append TEXT at streaming insertion MARKER in constant local work."
  (when (and (markerp marker)
             (marker-buffer marker)
             (stringp text)
             (not (string-empty-p text)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (insert text))))))

(defun ai-workbench-output-stream-finish (marker)
  "Finish the streaming block at MARKER and release it."
  (when (and (markerp marker) (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (unless (bolp) (insert "\n"))
          (insert "\n"))
        (ai-workbench-output--trim-if-needed)))
    (set-marker marker nil)))

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
                        (string-trim-right text)))
        (ai-workbench-output--trim-if-needed))))
  nil)

(provide 'ai-workbench-output)
;;; ai-workbench-output.el ends here
