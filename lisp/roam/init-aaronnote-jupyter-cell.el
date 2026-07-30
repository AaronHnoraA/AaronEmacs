;;; init-aaronnote-jupyter-cell.el --- Hidden Jupyter cell script buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Noema writes same-kernel Jupyter cells into a hidden source file beside
;; the note.  This minor mode detects those files and handles save events.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function my/noema-command "init-aaronnote" (command &optional detail))

(defvar-local my/noema-jupyter-cell-source-file nil
  "Markdown note file that owns the current hidden Jupyter cell script.")

(defvar-local my/noema-jupyter-cell-kernel nil
  "Jupyter kernel name for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-session nil
  "Jupyter session name for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-storage nil
  "Storage mode for the current hidden cell script.")

(defvar-local my/noema-jupyter-cell-current-id nil
  "Cell id at point in the current hidden Jupyter cell script.")

(defvar-local my/noema-jupyter-cell--start-overlay nil
  "Overlay highlighting the current Jupyter cell start marker.")

(defvar-local my/noema-jupyter-cell--end-overlay nil
  "Overlay highlighting the current Jupyter cell end marker.")

(defvar my/noema-jupyter-cell-mode)

(defface my/noema-jupyter-cell-marker-face
  '((t :background "#1f6f43" :foreground "#eafff1" :weight bold))
  "Face for active Noema Jupyter cell boundary markers."
  :group 'my/noema)

(defvar-keymap my/noema-jupyter-cell-mode-map
  "C-c C-c" #'my/noema-jupyter-cell-run-current
  "C-c C-o" #'my/noema-jupyter-cell-sync-cursor
  "M-RET" #'my/noema-jupyter-cell-sync-cursor
  "C-c C-r" #'my/noema-jupyter-cell-restart-run-all
  "C-c C-k" #'my/noema-jupyter-cell-interrupt
  "C-c C-s" #'my/noema-jupyter-cell-sync-buffer)

(defconst my/noema-jupyter-cell--comment-prefix
  "\\(?://\\|--\\|#\\|;\\)"
  "Line comment prefixes used in generated Jupyter cell script files.")

(defconst my/noema-jupyter-cell--source-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*\\(?:Noema\\|Aaronnote\\) cell source:[ \t]*\\(.+\\)$"))

(defconst my/noema-jupyter-cell--kernel-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*\\(?:Noema\\|Aaronnote\\) cell kernel:[ \t]*\\(.+\\)$"))

(defconst my/noema-jupyter-cell--session-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*\\(?:Noema\\|Aaronnote\\) cell session:[ \t]*\\(.+\\)$"))

(defconst my/noema-jupyter-cell--storage-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*\\(?:Noema\\|Aaronnote\\) cell storage:[ \t]*\\(.+\\)$"))

(defconst my/noema-jupyter-cell--start-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*%%[ \t]+aaronnote-cell[ \t]+id=\\([^ \t\n]+\\)[ \t]*$"))

(defconst my/noema-jupyter-cell--end-re
  (concat "^[ \t]*" my/noema-jupyter-cell--comment-prefix
          "[ \t]*%%[ \t]+end-aaronnote-cell[ \t]+id=\\([^ \t\n]+\\)[ \t]*$"))

(defun my/noema-jupyter-cell--ensure-overlay (symbol face)
  "Return buffer-local overlay stored in SYMBOL, creating it with FACE."
  (or (symbol-value symbol)
      (set symbol
           (let ((overlay (make-overlay (point-min) (point-min) nil nil t)))
             (overlay-put overlay 'face face)
             (overlay-put overlay 'evaporate t)
             overlay))))

(defun my/noema-jupyter-cell--hide-overlays ()
  "Hide current cell highlight overlays."
  (dolist (overlay (list my/noema-jupyter-cell--start-overlay
                         my/noema-jupyter-cell--end-overlay))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun my/noema-jupyter-cell--bounds-at-point ()
  "Return plist describing the generated Jupyter cell around point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((origin (point))
            start-id start-beg start-end end-beg end-end)
        (goto-char (line-end-position))
        (when (or (looking-at my/noema-jupyter-cell--start-re)
                  (re-search-backward my/noema-jupyter-cell--start-re nil t))
          (setq start-id (match-string-no-properties 1)
                start-beg (line-beginning-position)
                start-end (line-end-position))
          (goto-char start-end)
          (when (re-search-forward my/noema-jupyter-cell--end-re nil t)
            (while (and (not (string-equal (match-string-no-properties 1) start-id))
                        (re-search-forward my/noema-jupyter-cell--end-re nil t)))
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

(defun my/noema-jupyter-cell--update-highlight ()
  "Update current cell overlays and `my/noema-jupyter-cell-current-id'."
  (if-let* ((bounds (my/noema-jupyter-cell--bounds-at-point)))
      (let ((start (my/noema-jupyter-cell--ensure-overlay
                    'my/noema-jupyter-cell--start-overlay
                    'my/noema-jupyter-cell-marker-face))
            (end (my/noema-jupyter-cell--ensure-overlay
                  'my/noema-jupyter-cell--end-overlay
                  'my/noema-jupyter-cell-marker-face)))
        (setq-local my/noema-jupyter-cell-current-id (plist-get bounds :id))
        (move-overlay start (plist-get bounds :start-beg) (plist-get bounds :start-end))
        (move-overlay end (plist-get bounds :end-beg) (plist-get bounds :end-end)))
    (setq-local my/noema-jupyter-cell-current-id nil)
    (my/noema-jupyter-cell--hide-overlays)))

(defun my/noema-jupyter-cell--command-detail (&optional cell-id)
  "Return Noema command detail for CELL-ID or the cell at point."
  (let ((cell-id (or cell-id
                     my/noema-jupyter-cell-current-id
                     (plist-get (my/noema-jupyter-cell--bounds-at-point) :id))))
    (unless (and cell-id (not (string-empty-p cell-id)))
      (user-error "Point is not inside an Noema Jupyter cell"))
    `((file . ,my/noema-jupyter-cell-source-file)
      (cellId . ,cell-id)
      (kernel . ,(or my/noema-jupyter-cell-kernel ""))
      (session . ,(or my/noema-jupyter-cell-session ""))
      (storage . ,(or my/noema-jupyter-cell-storage "")))))

(defun my/noema-jupyter-cell--post-command-h ()
  "Track current cell for local highlighting.
Cursor moves in the Emacs source buffer are intentionally local; use
`my/noema-jupyter-cell-sync-cursor' (`M-RET') to sync Noema."
  (when my/noema-jupyter-cell-mode
    (my/noema-jupyter-cell--update-highlight)))

(defun my/noema-jupyter-cell-sync-cursor ()
  "Sync the current Emacs Jupyter cell cursor back to Noema."
  (interactive)
  (my/noema-jupyter-cell--update-highlight)
  (my/noema-command
   "jupyter-select-cell"
   (my/noema-jupyter-cell--command-detail)))

(defun my/noema-jupyter-cell-run-current ()
  "Run the current Noema Jupyter cell from this generated script buffer."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (my/noema-jupyter-cell--update-highlight)
  (let* ((detail (my/noema-jupyter-cell--command-detail))
         (cell-id (alist-get 'cellId detail)))
    (my/noema-command "jupyter-run-script-cell" detail)
    (message "Noema Jupyter: run cell %s via web" cell-id)))

(defun my/noema-jupyter-cell-restart-run-all ()
  "Restart this script buffer's kernel and run all cells in Noema."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (my/noema-command
   "jupyter-restart-run-all"
   (my/noema-jupyter-cell--command-detail)))

(defun my/noema-jupyter-cell-interrupt ()
  "Interrupt this script buffer's Noema Jupyter kernel."
  (interactive)
  (my/noema-command
   "jupyter-interrupt"
   (my/noema-jupyter-cell--command-detail)))

(defun my/noema-jupyter-cell--header-value (regexp)
  "Return the first generated header value matching REGEXP."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((limit (save-excursion (forward-line 10) (point))))
        (when (re-search-forward regexp limit t)
          (string-trim (match-string-no-properties 1)))))))

(defun my/noema-jupyter-cell--read-header ()
  "Read generated Jupyter cell script metadata from the current buffer."
  (let ((source (my/noema-jupyter-cell--header-value
                 my/noema-jupyter-cell--source-re)))
    (when (and source (not (string-empty-p source)))
      (list :source source
            :kernel (or (my/noema-jupyter-cell--header-value
                         my/noema-jupyter-cell--kernel-re)
                        "")
            :session (or (my/noema-jupyter-cell--header-value
                          my/noema-jupyter-cell--session-re)
                         "")
            :storage (or (my/noema-jupyter-cell--header-value
                          my/noema-jupyter-cell--storage-re)
                         "markdown")))))

(defun my/noema-jupyter-cell-sync-buffer ()
  "Notify Noema that the generated Jupyter cell script was saved."
  (interactive)
  (unless (and my/noema-jupyter-cell-source-file
               (not (string-empty-p my/noema-jupyter-cell-source-file)))
    (user-error "This buffer is not linked to an Noema Jupyter cell source"))
  (my/noema-command
   "jupyter-cell-script-saved"
   `((file . ,my/noema-jupyter-cell-source-file)
     (kernel . ,(or my/noema-jupyter-cell-kernel ""))
     (session . ,(or my/noema-jupyter-cell-session ""))
     (storage . ,(or my/noema-jupyter-cell-storage ""))))
  t)

(defun my/noema-jupyter-cell-after-save-h ()
  "Handle generated Jupyter cell scripts after saving."
  (when my/noema-jupyter-cell-mode
    (condition-case err
        (my/noema-jupyter-cell-sync-buffer)
      (error
       (message "Noema Jupyter cell sync failed: %s" (error-message-string err))))))

;;;###autoload
(define-minor-mode my/noema-jupyter-cell-mode
  "Minor mode for generated Noema Jupyter cell script files."
  :lighter " JCell"
  (if my/noema-jupyter-cell-mode
      (progn
        (add-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h nil t)
        (add-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h nil t)
        (my/noema-jupyter-cell--update-highlight))
    (remove-hook 'after-save-hook #'my/noema-jupyter-cell-after-save-h t)
    (remove-hook 'post-command-hook #'my/noema-jupyter-cell--post-command-h t)
    (my/noema-jupyter-cell--hide-overlays)))

(defun my/noema-jupyter-cell--candidate-file-p ()
  "Return non-nil when the current buffer might be a generated cell script.
Cheap gate for `find-file-hook' so the header regexp scan (and any TRAMP
round-trip it implies) only runs for files under a `.cell' store directory."
  (when-let* ((file (buffer-file-name)))
    (string-match-p (concat "\\(?:\\`\\|/\\)\\.cell/[^/]+\\'") file)))

;;;###autoload
(defun my/noema-jupyter-cell-activate-buffer ()
  "Enable `my/noema-jupyter-cell-mode' in an Noema-opened cell script.
This is intentionally explicit: ordinary `find-file' visits to `.cell' files
must not enable the mode unless Noema opened the script via Edit."
  (interactive)
  (when-let* (((my/noema-jupyter-cell--candidate-file-p))
              (meta (my/noema-jupyter-cell--read-header))
              (source (plist-get meta :source)))
    (setq-local my/noema-jupyter-cell-source-file source)
    (setq-local my/noema-jupyter-cell-kernel (plist-get meta :kernel))
    (setq-local my/noema-jupyter-cell-session (plist-get meta :session))
    (setq-local my/noema-jupyter-cell-storage (plist-get meta :storage))
    (my/noema-jupyter-cell-mode 1)))

(provide 'init-aaronnote-jupyter-cell)

;;; init-aaronnote-jupyter-cell.el ends here
