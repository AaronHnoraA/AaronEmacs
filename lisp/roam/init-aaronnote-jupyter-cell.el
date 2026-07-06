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

(defvar-local my/aaronnote-jupyter-cell-current-id nil
  "Cell id at point in the current hidden Jupyter cell script.")

(defvar-local my/aaronnote-jupyter-cell--start-overlay nil
  "Overlay highlighting the current Jupyter cell start marker.")

(defvar-local my/aaronnote-jupyter-cell--end-overlay nil
  "Overlay highlighting the current Jupyter cell end marker.")

(defvar-local my/aaronnote-jupyter-cell--last-sync-id nil
  "Last cell id synced from this Emacs buffer to Aaronnote.")

(defvar my/aaronnote-jupyter-cell-mode)

(defface my/aaronnote-jupyter-cell-marker-face
  '((t :background "#1f6f43" :foreground "#eafff1" :weight bold))
  "Face for active Aaronnote Jupyter cell boundary markers."
  :group 'my/aaronnote)

(defvar-keymap my/aaronnote-jupyter-cell-mode-map
  "C-c C-c" #'my/aaronnote-jupyter-cell-run-current
  "C-c C-o" #'my/aaronnote-jupyter-cell-sync-cursor
  "M-RET" #'my/aaronnote-jupyter-cell-sync-cursor
  "C-c C-r" #'my/aaronnote-jupyter-cell-restart-run-all
  "C-c C-k" #'my/aaronnote-jupyter-cell-interrupt
  "C-c C-s" #'my/aaronnote-jupyter-cell-sync-buffer)

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

(defconst my/aaronnote-jupyter-cell--start-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*%%[ \t]+aaronnote-cell[ \t]+id=\\([^ \t\n]+\\)[ \t]*$"))

(defconst my/aaronnote-jupyter-cell--end-re
  (concat "^[ \t]*" my/aaronnote-jupyter-cell--comment-prefix
          "[ \t]*%%[ \t]+end-aaronnote-cell[ \t]+id=\\([^ \t\n]+\\)[ \t]*$"))

(defun my/aaronnote-jupyter-cell--ensure-overlay (symbol face)
  "Return buffer-local overlay stored in SYMBOL, creating it with FACE."
  (or (symbol-value symbol)
      (set symbol
           (let ((overlay (make-overlay (point-min) (point-min) nil nil t)))
             (overlay-put overlay 'face face)
             (overlay-put overlay 'evaporate t)
             overlay))))

(defun my/aaronnote-jupyter-cell--hide-overlays ()
  "Hide current cell highlight overlays."
  (dolist (overlay (list my/aaronnote-jupyter-cell--start-overlay
                         my/aaronnote-jupyter-cell--end-overlay))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun my/aaronnote-jupyter-cell--bounds-at-point ()
  "Return plist describing the generated Jupyter cell around point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((origin (point))
            start-id start-beg start-end end-beg end-end)
        (goto-char (line-end-position))
        (when (or (looking-at my/aaronnote-jupyter-cell--start-re)
                  (re-search-backward my/aaronnote-jupyter-cell--start-re nil t))
          (setq start-id (match-string-no-properties 1)
                start-beg (line-beginning-position)
                start-end (line-end-position))
          (goto-char start-end)
          (when (re-search-forward my/aaronnote-jupyter-cell--end-re nil t)
            (while (and (not (string-equal (match-string-no-properties 1) start-id))
                        (re-search-forward my/aaronnote-jupyter-cell--end-re nil t)))
            (when (string-equal (match-string-no-properties 1) start-id)
              (setq end-beg (line-beginning-position)
                    end-end (line-end-position)))))
        (when (and start-id start-beg start-end end-beg end-end
                   (<= start-beg origin)
                   (<= origin end-end))
          (list :id start-id
                :start-beg start-beg
                :start-end start-end
                :body-beg (save-excursion
                            (goto-char start-end)
                            (forward-line 1)
                            (point))
                :body-end end-beg
                :end-beg end-beg
                :end-end end-end))))))

(defun my/aaronnote-jupyter-cell--update-highlight ()
  "Update current cell overlays and `my/aaronnote-jupyter-cell-current-id'."
  (if-let* ((bounds (my/aaronnote-jupyter-cell--bounds-at-point)))
      (let ((start (my/aaronnote-jupyter-cell--ensure-overlay
                    'my/aaronnote-jupyter-cell--start-overlay
                    'my/aaronnote-jupyter-cell-marker-face))
            (end (my/aaronnote-jupyter-cell--ensure-overlay
                  'my/aaronnote-jupyter-cell--end-overlay
                  'my/aaronnote-jupyter-cell-marker-face)))
        (setq-local my/aaronnote-jupyter-cell-current-id (plist-get bounds :id))
        (move-overlay start (plist-get bounds :start-beg) (plist-get bounds :start-end))
        (move-overlay end (plist-get bounds :end-beg) (plist-get bounds :end-end)))
    (setq-local my/aaronnote-jupyter-cell-current-id nil)
    (my/aaronnote-jupyter-cell--hide-overlays)))

(defun my/aaronnote-jupyter-cell--command-detail (&optional cell-id)
  "Return Aaronnote command detail for CELL-ID or the cell at point."
  (let ((cell-id (or cell-id
                     my/aaronnote-jupyter-cell-current-id
                     (plist-get (my/aaronnote-jupyter-cell--bounds-at-point) :id))))
    (unless (and cell-id (not (string-empty-p cell-id)))
      (user-error "Point is not inside an Aaronnote Jupyter cell"))
    `((file . ,my/aaronnote-jupyter-cell-source-file)
      (cellId . ,cell-id)
      (kernel . ,(or my/aaronnote-jupyter-cell-kernel ""))
      (session . ,(or my/aaronnote-jupyter-cell-session ""))
      (storage . ,(or my/aaronnote-jupyter-cell-storage "")))))

(defun my/aaronnote-jupyter-cell--post-command-h ()
  "Track current cell and sync cursor moves to Aaronnote."
  (when my/aaronnote-jupyter-cell-mode
    (my/aaronnote-jupyter-cell--update-highlight)
    (when (and my/aaronnote-jupyter-cell-current-id
               (not (equal my/aaronnote-jupyter-cell-current-id
                           my/aaronnote-jupyter-cell--last-sync-id)))
      (setq-local my/aaronnote-jupyter-cell--last-sync-id
                  my/aaronnote-jupyter-cell-current-id)
      (ignore-errors
        (my/aaronnote-command
         "jupyter-select-cell"
         (my/aaronnote-jupyter-cell--command-detail
          my/aaronnote-jupyter-cell-current-id))))))

(defun my/aaronnote-jupyter-cell-sync-cursor ()
  "Sync the current Emacs Jupyter cell cursor back to Aaronnote."
  (interactive)
  (my/aaronnote-jupyter-cell--update-highlight)
  (my/aaronnote-command
   "jupyter-select-cell"
   (my/aaronnote-jupyter-cell--command-detail)))

(defun my/aaronnote-jupyter-cell-run-current ()
  "Run the current Aaronnote Jupyter cell from this generated script buffer."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (my/aaronnote-jupyter-cell--update-highlight)
  (let* ((detail (my/aaronnote-jupyter-cell--command-detail))
         (cell-id (alist-get 'cellId detail)))
    (my/aaronnote-command "jupyter-run-script-cell" detail)
    (message "Aaronnote Jupyter: run cell %s via web" cell-id)))

(defun my/aaronnote-jupyter-cell-restart-run-all ()
  "Restart this script buffer's kernel and run all cells in Aaronnote."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (my/aaronnote-command
   "jupyter-restart-run-all"
   (my/aaronnote-jupyter-cell--command-detail)))

(defun my/aaronnote-jupyter-cell-interrupt ()
  "Interrupt this script buffer's Aaronnote Jupyter kernel."
  (interactive)
  (my/aaronnote-command
   "jupyter-interrupt"
   (my/aaronnote-jupyter-cell--command-detail)))

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
      (progn
        (add-hook 'after-save-hook #'my/aaronnote-jupyter-cell-after-save-h nil t)
        (add-hook 'post-command-hook #'my/aaronnote-jupyter-cell--post-command-h nil t)
        (my/aaronnote-jupyter-cell--update-highlight))
    (remove-hook 'after-save-hook #'my/aaronnote-jupyter-cell-after-save-h t)
    (remove-hook 'post-command-hook #'my/aaronnote-jupyter-cell--post-command-h t)
    (my/aaronnote-jupyter-cell--hide-overlays)))

(defun my/aaronnote-jupyter-cell--candidate-file-p ()
  "Return non-nil when the current buffer might be a generated cell script.
Cheap gate for `find-file-hook' so the header regexp scan (and any TRAMP
round-trip it implies) only runs for files under a `.cell' store directory."
  (when-let* ((file (buffer-file-name)))
    (string-match-p (concat "\\(?:\\`\\|/\\)\\.cell/[^/]+\\'") file)))

;;;###autoload
(defun my/aaronnote-jupyter-cell-activate-buffer ()
  "Enable `my/aaronnote-jupyter-cell-mode' in an Aaronnote-opened cell script.
This is intentionally explicit: ordinary `find-file' visits to `.cell' files
must not enable the mode unless Aaronnote opened the script via Edit."
  (interactive)
  (when-let* (((my/aaronnote-jupyter-cell--candidate-file-p))
              (meta (my/aaronnote-jupyter-cell--read-header))
              (source (plist-get meta :source)))
    (setq-local my/aaronnote-jupyter-cell-source-file source)
    (setq-local my/aaronnote-jupyter-cell-kernel (plist-get meta :kernel))
    (setq-local my/aaronnote-jupyter-cell-session (plist-get meta :session))
    (setq-local my/aaronnote-jupyter-cell-storage (plist-get meta :storage))
    (my/aaronnote-jupyter-cell-mode 1)))

(provide 'init-aaronnote-jupyter-cell)

;;; init-aaronnote-jupyter-cell.el ends here
