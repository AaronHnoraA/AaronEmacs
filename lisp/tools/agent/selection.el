;;; tools/agent/selection.el --- Agent editor context -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'seq)
(require 'tabulated-list)

(defvar +agent-selection nil
  "Selected sources: (BUFFER . OVERLAYS), (BUFFER), or (FILE).
A buffer without overlays means its entire saved file. No source is copied.")

(defun +agent-selection-collect (&optional selection)
  "Prune dead sources and return SELECTION, defaulting to the shared selection."
  (let (result)
    (dolist (entry (or selection +agent-selection))
      (let ((source (car entry)))
        (cond
         ((buffer-live-p source)
          (let ((overlays (seq-filter #'overlay-buffer (cdr entry))))
            (when (or (null (cdr entry)) overlays)
              (push (cons source overlays) result))))
         ((stringp source) (push entry result)))))
    (nreverse result)))

(defun +agent-selection-regions (_buffer overlays)
  "Return current endpoints of the tracked region OVERLAYS."
  (mapcar (lambda (overlay) (cons (overlay-start overlay) (overlay-end overlay)))
          (seq-filter #'overlay-buffer overlays)))

(defun +agent-selection-add-region (buffer begin end &optional _advance)
  "Track BUFFER's BEGIN..END without storing or sending its text."
  (setq +agent-selection (+agent-selection-collect))
  (let ((entry (assq buffer +agent-selection)))
    (unless entry
      (setq entry (list buffer))
      (setq +agent-selection (append +agent-selection (list entry))))
    (let ((overlay (make-overlay begin end buffer nil t)))
      (overlay-put overlay 'face 'secondary-selection)
      (setcdr entry (append (cdr entry) (list overlay))))))

(defun +agent-selection-add-buffer (buffer)
  "Select all of BUFFER's saved file."
  (when-let* ((entry (assq buffer +agent-selection)))
    (mapc #'delete-overlay (cdr entry)))
  (setq +agent-selection (append (assq-delete-all buffer +agent-selection) (list (list buffer)))))

(defun +agent-selection-add-file (file)
  "Select FILE by reference."
  (let ((file (expand-file-name file)))
    (unless (assoc file +agent-selection)
      (setq +agent-selection (append +agent-selection (list (list file)))))))

(defun +agent-selection-remove (source)
  "Remove SOURCE from the selection and release its tracked regions."
  (when-let* ((entry (assoc source +agent-selection)))
    (mapc #'delete-overlay (cdr entry))
    (setq +agent-selection (delete entry +agent-selection))))

(defun +agent-selection-clear (&optional _verbose)
  "Release selected context and all of its overlays."
  (interactive)
  (dolist (entry +agent-selection) (mapc #'delete-overlay (cdr entry)))
  (setq +agent-selection nil))

(defun +agent-selection-add (&optional file)
  "Add the active region, buffer, Dired marks, or with prefix FILE."
  (interactive "P")
  (cond
   (file (+agent-selection-add-file (read-file-name "Context file: " nil nil t)))
   ((derived-mode-p 'dired-mode)
    (require 'dired)
    (mapc #'+agent-selection-add-file (dired-get-marked-files)))
   ((use-region-p)
    (+agent-selection-add-region (current-buffer) (region-beginning) (region-end)))
   (t (+agent-selection-add-buffer (current-buffer))))
  (message "Agent: %d context source(s)" (length +agent-selection)))

(defun +agent-selection-review-refresh ()
  "Refresh the context list without reading file bodies."
  (setq +agent-selection (+agent-selection-collect)
        tabulated-list-entries
        (mapcar (lambda (entry)
                  (let ((source (car entry)))
                    (list source (vector
                                  (if (bufferp source) (buffer-name source) source)
                                  (if (cdr entry) (format "%d region(s)" (length (cdr entry))) "File")))))
                +agent-selection))
  (tabulated-list-print t))

(defun +agent-selection-review-remove ()
  "Remove the source at point."
  (interactive)
  (+agent-selection-remove (tabulated-list-get-id))
  (+agent-selection-review-refresh))

(defun +agent-selection-review-visit ()
  "Visit the source at point."
  (interactive)
  (let ((source (tabulated-list-get-id)))
    (if (bufferp source) (pop-to-buffer source) (find-file source))))

(define-derived-mode +agent-selection-review-mode tabulated-list-mode "Agent Context"
  "Review references; d removes, RET visits, C-c C-c sends."
  (setq tabulated-list-format [("Source" 60 t) ("Scope" 16 nil)]
        tabulated-list-padding 2)
  (tabulated-list-init-header))
(define-key +agent-selection-review-mode-map (kbd "d") #'+agent-selection-review-remove)
(define-key +agent-selection-review-mode-map (kbd "RET") #'+agent-selection-review-visit)
(define-key +agent-selection-review-mode-map (kbd "g") #'+agent-selection-review-refresh)

(provide 'tools/agent/selection)
