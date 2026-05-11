;;; init-note-metadata.el --- Typst note metadata editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands for editing file-level Typst note metadata.

;;; Code:

(require 'init-note)
(require 'seq)
(require 'subr-x)

(defun my/note--metadata-body-bounds ()
  "Return (START . END) bounds of the first note metadata body."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#metadata[ \t\n]*((" nil t)
      (let ((start (point)))
        (when (search-forward "))" nil t)
          (cons start (- (point) 2)))))))

(defun my/note--metadata-list-source (values)
  "Return Typst tuple source for string VALUES."
  (concat "("
          (mapconcat (lambda (value) (format "%S" value))
                     values
                     ", ")
          (when values ",")
          ")"))

(defun my/note--metadata-list-bounds (key)
  "Return bounds of metadata tuple value for KEY."
  (when-let* ((bounds (my/note--metadata-body-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward
             (format "\\_<%s\\_>[ \t\n]*:[ \t\n]*("
                     (regexp-quote key))
             (cdr bounds)
             t)
        (let ((start (1- (point)))
              (depth 1))
          (while (and (> depth 0)
                      (< (point) (cdr bounds)))
            (pcase (char-after)
              (?\( (setq depth (1+ depth)))
              (?\) (setq depth (1- depth))))
            (forward-char 1))
          (when (= depth 0)
            (cons start (point))))))))

(defun my/note--metadata-list-values (key)
  "Return metadata tuple values for KEY in the current note."
  (when-let* ((entry (my/note--metadata-body)))
    (or (my/note--metadata-list (car entry) key) nil)))

(defun my/note--set-metadata-list (key values)
  "Set metadata tuple KEY to VALUES in the current note."
  (unless (my/note--metadata-body-bounds)
    (user-error "No note metadata block found"))
  (let ((values (seq-uniq (seq-filter (lambda (value)
                                        (and (stringp value)
                                             (not (string-empty-p value))))
                                      values)
                          #'string=)))
    (if-let* ((bounds (my/note--metadata-list-bounds key)))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert (my/note--metadata-list-source values)))
      (save-excursion
        (goto-char (car (my/note--metadata-body-bounds)))
        (insert (format "\n  %s: %s,"
                        key
                        (my/note--metadata-list-source values)))))))

(defun my/note--read-metadata-value (prompt)
  "Read a non-empty metadata value using PROMPT."
  (let ((value (string-trim (read-string prompt))))
    (when (string-empty-p value)
      (user-error "Value is empty"))
    value))

(defun my/note--known-tags ()
  "Return distinct tag strings recorded in the note index."
  (mapcar #'car
          (my/note--rows
           "select distinct tag from tags where tag <> '' order by tag")))

(defun my/note--read-tag (prompt)
  "Read a tag using PROMPT with completion over existing tags."
  (let* ((known (my/note--known-tags))
         (current (and (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
                       (my/note--metadata-list-values "tags")))
         (candidates (seq-uniq (append current known) #'string=))
         (value (string-trim
                 (completing-read prompt candidates nil nil))))
    (when (string-empty-p value)
      (user-error "Value is empty"))
    value))

(defun my/note--after-metadata-edit (message value)
  "Save current note, sync the index, and report MESSAGE with VALUE."
  (save-buffer)
  (my/note-db-sync)
  (message message value))

;;;###autoload
(defun my/note-tag-add (tag)
  "Add TAG to the current Typst note metadata."
  (interactive (list (my/note--read-tag "Tag: ")))
  (unless (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (user-error "Not in a Typst note buffer"))
  (my/note--set-metadata-list
   "tags"
   (append (my/note--metadata-list-values "tags") (list tag)))
  (my/note--after-metadata-edit "Note tag added: %s" tag))

;;;###autoload
(defun my/note-alias-add (alias)
  "Add ALIAS to the current Typst note metadata."
  (interactive (list (my/note--read-metadata-value "Alias: ")))
  (unless (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (user-error "Not in a Typst note buffer"))
  (my/note--set-metadata-list
   "aliases"
   (append (my/note--metadata-list-values "aliases") (list alias)))
  (my/note--after-metadata-edit "Note alias added: %s" alias))

(provide 'init-note-metadata)
;;; init-note-metadata.el ends here
