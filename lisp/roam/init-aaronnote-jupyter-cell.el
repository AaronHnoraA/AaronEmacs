;;; init-aaronnote-jupyter-cell.el --- Hidden Jupyter cell script buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Aaronnote writes same-kernel Jupyter cells into a hidden source file beside
;; the note.  This minor mode detects those files and handles save events.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function my/aaronnote-command "init-aaronnote" (command &optional detail))

(defvar-local my/aaronnote-jupyter-cell-source-file nil
  "Markdown note file that owns the current hidden Jupyter cell script.")

(defvar-local my/aaronnote-jupyter-cell-kernel nil
  "Jupyter kernel name for the current hidden cell script.")

(defvar-local my/aaronnote-jupyter-cell-session nil
  "Jupyter session name for the current hidden cell script.")

(defvar-local my/aaronnote-jupyter-cell-storage nil
  "Storage mode for the current hidden cell script.")

(defvar my/aaronnote-jupyter-cell-mode)

(defconst my/aaronnote-jupyter-cell--comment-prefix
  "\\(?://\\|--\\|#\\|;\\)"
  "Line comment prefixes used in generated Jupyter cell script files.")

(defconst my/aaronnote-jupyter-cell--source-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*Aaronnote cell source:[ \t]*\\(.+\\)$"))

(defconst my/aaronnote-jupyter-cell--kernel-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*Aaronnote cell kernel:[ \t]*\\(.+\\)$"))

(defconst my/aaronnote-jupyter-cell--session-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*Aaronnote cell session:[ \t]*\\(.+\\)$"))

(defconst my/aaronnote-jupyter-cell--storage-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*Aaronnote cell storage:[ \t]*\\(.+\\)$"))

(defun my/aaronnote-jupyter-cell--header-value (regexp)
  "Return the first generated header value matching REGEXP."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((limit (save-excursion (forward-line 10) (point))))
        (when (re-search-forward regexp limit t)
          (string-trim (match-string-no-properties 1)))))))

(defun my/aaronnote-jupyter-cell--read-header ()
  "Read generated Jupyter cell script metadata from the current buffer."
  (let ((source (my/aaronnote-jupyter-cell--header-value
                 my/aaronnote-jupyter-cell--source-re)))
    (when (and source (not (string-empty-p source)))
      (list :source source
            :kernel (or (my/aaronnote-jupyter-cell--header-value
                         my/aaronnote-jupyter-cell--kernel-re)
                        "")
            :session (or (my/aaronnote-jupyter-cell--header-value
                          my/aaronnote-jupyter-cell--session-re)
                         "")
            :storage (or (my/aaronnote-jupyter-cell--header-value
                          my/aaronnote-jupyter-cell--storage-re)
                         "markdown")))))

(defun my/aaronnote-jupyter-cell-sync-buffer ()
  "Notify Aaronnote that the generated Jupyter cell script was saved."
  (interactive)
  (unless (and my/aaronnote-jupyter-cell-source-file
               (not (string-empty-p my/aaronnote-jupyter-cell-source-file)))
    (user-error "This buffer is not linked to an Aaronnote Jupyter cell source"))
  (my/aaronnote-command
   "jupyter-cell-script-saved"
   `((file . ,my/aaronnote-jupyter-cell-source-file)
     (kernel . ,(or my/aaronnote-jupyter-cell-kernel ""))
     (session . ,(or my/aaronnote-jupyter-cell-session ""))
     (storage . ,(or my/aaronnote-jupyter-cell-storage ""))))
  t)

(defun my/aaronnote-jupyter-cell-after-save-h ()
  "Handle generated Jupyter cell scripts after saving."
  (when my/aaronnote-jupyter-cell-mode
    (condition-case err
        (my/aaronnote-jupyter-cell-sync-buffer)
      (error
       (message "Aaronnote Jupyter cell sync failed: %s" (error-message-string err))))))

;;;###autoload
(define-minor-mode my/aaronnote-jupyter-cell-mode
  "Minor mode for generated Aaronnote Jupyter cell script files."
  :lighter " JCell"
  (if my/aaronnote-jupyter-cell-mode
      (add-hook 'after-save-hook #'my/aaronnote-jupyter-cell-after-save-h nil t)
    (remove-hook 'after-save-hook #'my/aaronnote-jupyter-cell-after-save-h t)))

(defun my/aaronnote-jupyter-cell--candidate-file-p ()
  "Return non-nil when the current buffer might be a generated cell script.
Cheap gate for `find-file-hook' so the header regexp scan (and any TRAMP
round-trip it implies) only runs for files under a `.cell' store directory."
  (when-let* ((file (buffer-file-name)))
    (string-match-p (concat "\\(?:\\`\\|/\\)\\.cell/[^/]+\\'") file)))

;;;###autoload
(defun my/aaronnote-jupyter-cell-maybe-enable-h ()
  "Enable `my/aaronnote-jupyter-cell-mode' in generated Jupyter cell scripts."
  (when-let* (((my/aaronnote-jupyter-cell--candidate-file-p))
              (meta (my/aaronnote-jupyter-cell--read-header))
              (source (plist-get meta :source)))
    (setq-local my/aaronnote-jupyter-cell-source-file source)
    (setq-local my/aaronnote-jupyter-cell-kernel (plist-get meta :kernel))
    (setq-local my/aaronnote-jupyter-cell-session (plist-get meta :session))
    (setq-local my/aaronnote-jupyter-cell-storage (plist-get meta :storage))
    (my/aaronnote-jupyter-cell-mode 1)))

(add-hook 'find-file-hook #'my/aaronnote-jupyter-cell-maybe-enable-h)

(provide 'init-aaronnote-jupyter-cell)

;;; init-aaronnote-jupyter-cell.el ends here
