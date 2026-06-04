;;; init-typst-roam.el --- Typst roam note navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Roam-style helpers for Typst notes.  Keep these separate from the core
;; Typst editing, LSP, and preview configuration.

;;; Code:

(require 'init-funcs)
(require 'init-typst)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defvar typst-ts-mode-map)

(defgroup my/typst-roam nil
  "Roam-style navigation for Typst notes."
  :group 'my/typst)

(defcustom my/typst-roam-root "/Users/hc/Documents/AaronNote/"
  "Root directory of the Typst roam note vault."
  :type 'directory
  :group 'my/typst-roam)

(defun my/typst-roam-root ()
  "Return the roam notes root, preferring typst.toml discovery."
  (or (when buffer-file-name
        (when-let* ((dir (locate-dominating-file buffer-file-name "typst.toml")))
          (file-truename dir)))
      (file-name-as-directory (expand-file-name my/typst-roam-root))))

(defun my/typst-roam--slug-at-point ()
  "Return the note-link slug at or near point, or nil."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-start line-end))
           (offset (- (point) line-start))
           result)
      (let ((pos 0))
        (while (string-match "#note-link(\"\\([^\"]+\\)\"" line pos)
          (let ((call-start (match-beginning 0))
                (slug (match-string 1 line)))
            (when (<= call-start offset)
              (setq result slug))
            (setq pos (1+ call-start)))))
      result)))

(defun my/typst-roam--all-files ()
  "Return all roam .typ note files, excluding _typst/."
  (seq-filter
   (lambda (file) (not (string-match-p "/_typst/" file)))
   (directory-files-recursively (my/typst-roam-root) "\\.typ$")))

(defun my/typst-roam--file-to-slug (file)
  "Convert FILE path to a roam slug, relative to root and without .typ."
  (string-remove-suffix ".typ"
    (file-relative-name file (my/typst-roam-root))))

(defun my/typst-roam--slug-to-file (slug)
  "Convert SLUG to an absolute .typ path under the roam root."
  (expand-file-name (concat slug ".typ") (my/typst-roam-root)))

(defun my/typst-roam-follow-link ()
  "Jump to the note referenced by the #note-link at point."
  (interactive)
  (if-let* ((slug (my/typst-roam--slug-at-point)))
      (let ((file (my/typst-roam--slug-to-file slug)))
        (if (file-exists-p file)
            (find-file file)
          (when (yes-or-no-p (format "Note '%s' not found. Create it? " slug))
            (my/typst-roam-new-note slug))))
    (user-error "No #note-link found at point")))

(defun my/typst-roam-find-note ()
  "Find a roam note by slug with completion."
  (interactive)
  (let* ((files (my/typst-roam--all-files))
         (slugs (mapcar #'my/typst-roam--file-to-slug files))
         (slug (completing-read "Roam note: " slugs nil t)))
    (find-file (my/typst-roam--slug-to-file slug))))

(defun my/typst-roam-insert-link ()
  "Insert a #note-link at point with slug and display text chosen via completion."
  (interactive)
  (let* ((files (my/typst-roam--all-files))
         (slugs (mapcar #'my/typst-roam--file-to-slug files))
         (slug (completing-read "Link to note: " slugs nil t))
         (default-text (file-name-nondirectory slug))
         (text (read-string (format "Display text [%s]: " default-text)
                            nil nil default-text)))
    (insert (format "#note-link(\"%s\")[%s]" slug text))))

(defun my/typst-roam-new-note (&optional slug)
  "Create a new roam note, prompting for SLUG, title, and tags."
  (interactive)
  (let* ((slug (or slug (read-string "Slug (e.g. math/my-note): ")))
         (title (read-string "Title: "
                             (capitalize (replace-regexp-in-string
                                          "[-/]" " "
                                          (file-name-nondirectory slug)))))
         (tags (read-string "Tags (comma-separated, or blank): "))
         (file (my/typst-roam--slug-to-file slug))
         (tag-str (if (string-empty-p tags)
                      ""
                    (mapconcat (lambda (tag)
                                 (format "\"%s\"" (string-trim tag)))
                               (split-string tags ",") ", "))))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format "\
#import \"/_typst/template.typ\": *
#show: note.with(
  title: \"%s\",
  tags: (%s),
  created: datetime(year: %s, month: %s, day: %s),
)

= %s

"
                      title
                      tag-str
                      (format-time-string "%Y")
                      (string-to-number (format-time-string "%m"))
                      (string-to-number (format-time-string "%d"))
                      title)))))

;; ── Roam DB ──────────────────────────────────────────────────────────────────

(defvar my/typst-roam--db-cache nil)
(defvar my/typst-roam--db-path-cache nil)
(defvar my/typst-roam--db-mtime nil)

(defun my/typst-roam--db-path ()
  "Return path to roam-db.json for the current vault."
  (expand-file-name "_typst/roam-db.json" (my/typst-roam-root)))

(defun my/typst-roam--db ()
  "Return the parsed roam-db.json, refreshing cache when the file changes."
  (let ((path (my/typst-roam--db-path)))
    (when (file-exists-p path)
      (let ((mtime (file-attribute-modification-time (file-attributes path))))
        (when (or (not my/typst-roam--db-cache)
                  (not (equal path my/typst-roam--db-path-cache))
                  (time-less-p my/typst-roam--db-mtime mtime))
          (setq my/typst-roam--db-cache
                (with-temp-buffer
                  (insert-file-contents path)
                  (json-parse-buffer :object-type 'hash-table :array-type 'list))
                my/typst-roam--db-path-cache path
                my/typst-roam--db-mtime mtime))))
    my/typst-roam--db-cache))

(defun my/typst-roam--db-note (slug)
  "Return the DB hash-table for SLUG, or nil."
  (when-let* ((db (my/typst-roam--db)))
    (gethash slug (gethash "notes" db))))

(defun my/typst-roam--current-slug ()
  "Return the roam slug for the current buffer, or nil."
  (when buffer-file-name
    (my/typst-roam--file-to-slug buffer-file-name)))

;; ── DB commands ───────────────────────────────────────────────────────────────

(defun my/typst-roam-update-db ()
  "Run roam-index.py to rebuild roam-db.json."
  (interactive)
  (let* ((root    (my/typst-roam-root))
         (script  (expand-file-name "_typst/roam-index.py" root))
         (buf     (get-buffer-create "*roam-index*")))
    (if (not (file-exists-p script))
        (message "roam-index.py not found at %s" script)
      (setq my/typst-roam--db-cache nil)
      (make-process
       :name "roam-index"
       :buffer buf
       :command (list "python3" script)
       :sentinel (lambda (proc event)
                   (when (string-prefix-p "finished" event)
                     (setq my/typst-roam--db-cache nil)
                     (message "roam-db updated")))))))

(defun my/typst-roam-backlinks ()
  "Show backlinks for the current note in a dedicated buffer."
  (interactive)
  (let* ((slug (my/typst-roam--current-slug))
         (note (and slug (my/typst-roam--db-note slug)))
         (bls  (and note (gethash "backlinks" note)))
         (buf  (get-buffer-create "*roam-backlinks*")))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Backlinks for: %s\n\n"
                        (if note (gethash "title" note) slug)))
        (if (null bls)
            (insert "(no backlinks)\n")
          (dolist (bl bls)
            (let* ((bl-note (my/typst-roam--db-note bl))
                   (title   (if bl-note (gethash "title" bl-note) bl))
                   (file    (my/typst-roam--slug-to-file bl)))
              (insert-text-button
               (format "  %-40s %s\n" title bl)
               'action (lambda (_) (find-file file))
               'follow-link t))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-tags ()
  "Browse notes by tag with completion."
  (interactive)
  (let* ((db      (my/typst-roam--db))
         (tags-ht (and db (gethash "tags" db)))
         (tags    (when tags-ht (hash-table-keys tags-ht)))
         (tag     (completing-read "Tag: " (sort tags #'string<) nil t))
         (slugs   (gethash tag tags-ht))
         (slug    (completing-read (format "Notes tagged [%s]: " tag)
                                   slugs nil t)))
    (find-file (my/typst-roam--slug-to-file slug))))

(defun my/typst-roam-todos ()
  "List all vault todos in a *roam-todos* buffer."
  (interactive)
  (let* ((db    (my/typst-roam--db))
         (todos (and db (gethash "todos" db)))
         (buf   (get-buffer-create "*roam-todos*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Roam TODOs\n\n")
        (if (null todos)
            (insert "(none)\n")
          (dolist (entry todos)
            (let* ((note-slug (gethash "note" entry))
                   (title     (gethash "title" entry))
                   (text      (gethash "text" entry))
                   (file      (my/typst-roam--slug-to-file note-slug)))
              (insert-text-button
               (format "  [%s]  %s\n" title text)
               'action (lambda (_) (find-file file))
               'follow-link t))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;; Enhanced find-note with DB annotation
(defun my/typst-roam--note-annotator (slug)
  "Return annotation for SLUG in completing-read."
  (when-let* ((note (my/typst-roam--db-note slug)))
    (let ((tags (gethash "tags" note))
          (bls  (length (gethash "backlinks" note))))
      (concat "  "
              (if tags (string-join tags ",") "")
              (when (> bls 0) (format " ←%d" bls))))))

;; Auto-update DB on save
(defun my/typst-roam--maybe-update-db ()
  "Run indexer after saving a roam .typ file."
  (when (and buffer-file-name
             (string-suffix-p ".typ" buffer-file-name)
             (not (string-match-p "/_typst/" buffer-file-name))
             (string-prefix-p (my/typst-roam-root)
                              (file-truename buffer-file-name)))
    (my/typst-roam-update-db)))

(add-hook 'after-save-hook #'my/typst-roam--maybe-update-db)

;; ── Keymaps & menus ───────────────────────────────────────────────────────────

(defvar my/typst-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'my/typst-roam-find-note)
    (define-key map (kbd "o") #'my/typst-roam-follow-link)
    (define-key map (kbd "i") #'my/typst-roam-insert-link)
    (define-key map (kbd "n") #'my/typst-roam-new-note)
    (define-key map (kbd "b") #'my/typst-roam-backlinks)
    (define-key map (kbd "t") #'my/typst-roam-tags)
    (define-key map (kbd "T") #'my/typst-roam-todos)
    (define-key map (kbd "u") #'my/typst-roam-update-db)
    (define-key map (kbd "m") #'my/typst-roam-dispatch)
    map)
  "Roam keymap for Typst buffers. Bound to C-c r.")

(transient-define-prefix my/typst-roam-dispatch ()
  "Typst roam command menu."
  [["Notes"
    ("o" "open link"   my/typst-roam-follow-link)
    ("f" "find note"   my/typst-roam-find-note)
    ("i" "insert link" my/typst-roam-insert-link)
    ("n" "new note"    my/typst-roam-new-note)]
   ["DB"
    ("b" "backlinks"   my/typst-roam-backlinks)
    ("t" "tags"        my/typst-roam-tags)
    ("T" "todos"       my/typst-roam-todos)
    ("u" "update db"   my/typst-roam-update-db)]])

(defun my/typst-roam-setup-keys ()
  "Install Typst roam keys in the current buffer."
  (local-set-key (kbd "C-c r") my/typst-roam-map)
  (local-set-key (kbd "C-c C-o") #'my/typst-roam-follow-link))

(with-eval-after-load 'typst-ts-mode
  (define-key typst-ts-mode-map (kbd "C-c r") my/typst-roam-map)
  (define-key typst-ts-mode-map (kbd "C-c C-o") #'my/typst-roam-follow-link)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c C-o" "typst roam open link"
      "C-c r" "typst roam")))

(dolist (hook '(typst-mode-hook my/typst-mode-hook))
  (add-hook hook #'my/typst-roam-setup-keys))

(my/leader!
  "r t" '(:def my/typst-roam-dispatch :which-key "typst roam"))

(provide 'init-typst-roam)
;;; init-typst-roam.el ends here
