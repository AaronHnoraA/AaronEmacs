;;; init-note-capture.el --- Typst note capture and daily entries -*- lexical-binding: t; -*-

;;; Commentary:
;; Replacement for the org-capture / daily flow.  Captures land in
;; `<note-directory>/daily/<category>/<slug>-YYYYMMDD.typ' with a populated
;; note-entry preamble so they immediately participate in the index.

;;; Code:

(require 'init-note)
(require 'subr-x)

(defcustom my/note-capture-categories
  '("idea" "inbox" "mail" "note" "meeting" "protocol" "uni" "life")
  "Daily capture sub-directories under `<note-directory>/daily/'."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-daily-rolling-file-format "%Y-%m-%d"
  "`format-time-string' template for the rolling daily file under daily/."
  :type 'string
  :group 'my/note)

(defun my/note-capture--daily-dir (&optional subdir)
  "Return the daily directory, optionally below SUBDIR."
  (let ((root (expand-file-name
               "daily"
               (file-name-as-directory my/note-directory))))
    (if subdir
        (expand-file-name subdir root)
      root)))

(defun my/note-capture--ensure-categories ()
  "Make sure all configured daily subdirs exist."
  (dolist (sub my/note-capture-categories)
    (make-directory (my/note-capture--daily-dir sub) t)))

(defun my/note-capture--entry-source (id title tags)
  "Return the full Typst preamble for a fresh capture file."
  (concat my/note-helper-import-source
          "\n"
          (my/note--metadata-source id title tags)
          "\n= " title "\n\n"))

;;;###autoload
(defun my/note-capture (category title)
  "Capture a fresh Typst note under daily/<CATEGORY>/.
TITLE drives the filename slug and headline."
  (interactive
   (let* ((cat (completing-read "Category: "
                                my/note-capture-categories nil t))
          (ttl (read-string "Title: ")))
     (when (string-empty-p (string-trim ttl))
       (user-error "Title is empty"))
     (list cat ttl)))
  (my/note-capture--ensure-categories)
  (my/note-ensure-helper-file)
  (let* ((slug (my/note--slugify title))
         (date (format-time-string "%Y%m%d"))
         (dir (my/note-capture--daily-dir category))
         (file (expand-file-name (format "%s-%s.typ" slug date) dir))
         (id (my/note--new-id title))
         (tags (list category)))
    (make-directory dir t)
    (when (file-exists-p file)
      (user-error "Capture already exists: %s" file))
    (find-file file)
    (insert (my/note-capture--entry-source id title tags))
    (save-buffer)
    (my/note-db-sync-file file)
    file))

;;;###autoload
(defun my/note-daily-today ()
  "Open today's rolling daily note and append a `== HH:MM' entry."
  (interactive)
  (my/note-ensure-helper-file)
  (let* ((today (format-time-string my/note-daily-rolling-file-format))
         (dir (my/note-capture--daily-dir))
         (file (expand-file-name (concat today ".typ") dir))
         (fresh (not (file-exists-p file))))
    (make-directory dir t)
    (find-file file)
    (when fresh
      (let ((id (my/note--new-id today)))
        (insert (my/note-capture--entry-source id today (list "daily")))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "== %s\n\n" (format-time-string "%H:%M")))
    (when fresh
      (save-buffer)
      (my/note-db-sync-file file))
    file))

(provide 'init-note-capture)
;;; init-note-capture.el ends here
