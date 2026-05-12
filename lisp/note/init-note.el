;;; init-note.el --- Typst-backed note index helpers -*- lexical-binding: t -*-

;;; Commentary:
;; A small note layer for Typst files.  This is intentionally separate from
;; Org Roam: Typst files are indexed into a local SQLite database and use a
;; lightweight `#note("id")[title]' call for cross-note links.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'xref)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function my/navigation-find-definition "init-navigation" ())
(declare-function my/note-agenda "init-note-agenda" (&optional all))
(declare-function my/note-alias-add "init-note-metadata" (alias))
(declare-function my/note-capture "init-note-capture" (category title))
(declare-function my/note-daily-today "init-note-capture" ())
(declare-function my/note-graph "init-note-graph" ())
(declare-function my/note-reference-insert "init-note-reference" ())
(declare-function my/note-tag-add "init-note-metadata" (tag))
(declare-function my/typst-preview-send-position "init-typst" ())

(defgroup my/note nil
  "Typst-backed note-taking helpers."
  :group 'applications)

(defcustom my/note-root
  (file-truename
   (if (boundp 'my-org-root)
       my-org-root
     (expand-file-name "~/HC/Org/")))
  "Root directory scanned for Typst note files."
  :type 'directory
  :group 'my/note)

(defcustom my/note-directory
  (file-name-as-directory
   (expand-file-name
    "roam"
    (file-name-as-directory my/note-root)))
  "Default directory for newly created Typst notes."
  :type 'directory
  :group 'my/note)

(defcustom my/note-db-file
  (expand-file-name
   "note.db"
   (if (boundp 'my/state-dir)
       (expand-file-name "note" my/state-dir)
     (expand-file-name "var/note" user-emacs-directory)))
  "SQLite database used by the Typst note index."
  :type 'file
  :group 'my/note)

(defvar my/note-command-map)

(defcustom my/note-style-directory
  (locate-user-emacs-file "lisp/note/typst")
  "Directory containing internal Typst note helper modules."
  :type 'directory
  :group 'my/note)

(defcustom my/note-excluded-directories
  '("_typst" "public" "var" ".git" ".direnv" ".venv" "node_modules")
  "Directory names ignored while scanning Typst notes."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-auto-sync-on-save t
  "When non-nil, re-index a Typst note file after saving it."
  :type 'boolean
  :group 'my/note)

(defcustom my/note-before-sync-hook nil
  "Hook run before a full `my/note-db-sync'."
  :type 'hook
  :group 'my/note)

(defcustom my/note-after-sync-hook nil
  "Hook run after a full `my/note-db-sync'.
Hook functions receive CHANGED, REMOVED, and KEPT counts."
  :type 'hook
  :group 'my/note)

(defconst my/note-metadata-label "note"
  "Typst label used for file-level note metadata.")

(defconst my/note-helper-import-source
  "#import \"/_typst/note.typ\": *\n#show: note-entry\n"
  "Root-relative Typst import for the shared note helper.")

(cl-defstruct my/note-type
  name prompt slug-prefix default-tags id-strategy template-fn)

(defun my/note--template-default (id title tags)
  "Return default Typst note source for ID TITLE and TAGS."
  (concat my/note-helper-import-source
          "\n"
          (my/note--metadata-source id title tags)
          "\n= " title "\n\n"))

(defun my/note--template-literature (id title tags)
  "Return literature Typst note source for ID TITLE and TAGS."
  (concat (my/note--template-default id title tags)
          "== Reference\n\n"
          "== Summary\n\n"
          "== Notes\n\n"))

(defun my/note--template-fleeting (id title tags)
  "Return fleeting Typst note source for ID TITLE and TAGS."
  (concat (my/note--template-default id title tags)
          "== Thought\n\n"
          "== Next\n\n"))

(defcustom my/note-types
  (list
   (make-my/note-type
    :name 'default
    :prompt "Default note"
    :slug-prefix ""
    :default-tags nil
    :id-strategy 'timestamp-slug
    :template-fn #'my/note--template-default)
   (make-my/note-type
    :name 'literature
    :prompt "Literature note"
    :slug-prefix "lit-"
    :default-tags '("literature")
    :id-strategy 'timestamp-slug
    :template-fn #'my/note--template-literature)
   (make-my/note-type
    :name 'fleeting
    :prompt "Fleeting note"
    :slug-prefix "fl-"
    :default-tags '("fleeting")
    :id-strategy 'timestamp-slug
    :template-fn #'my/note--template-fleeting))
  "Registered Typst note creation templates."
  :type 'sexp
  :group 'my/note)

(defcustom my/note-new-hook nil
  "Hook run after a new note is created.
Hook functions receive the parsed note plist."
  :type 'hook
  :group 'my/note)

(defconst my/note--schema-version 3
  "Current SQLite schema version for the Typst note index.")

(defvar my/note--db-ro nil
  "Cached read-only SQLite connection for note index queries.")

(defvar my/note--db-ro-file nil
  "Database file currently used by `my/note--db-ro'.")

(defun my/note--sqlite-available-p ()
  "Return non-nil when Emacs has SQLite support."
  (and (fboundp 'sqlite-available-p)
       (sqlite-available-p)))

(defun my/note--require-sqlite ()
  "Signal a clear error when SQLite support is unavailable."
  (unless (my/note--sqlite-available-p)
    (user-error "This Emacs build has no SQLite support; note index is unavailable")))

(defun my/note--db-open (&optional readonly)
  "Open the note database.
When READONLY is non-nil, open it read-only."
  (my/note--require-sqlite)
  (unless readonly
    (make-directory (file-name-directory my/note-db-file) t))
  (sqlite-open my/note-db-file readonly))

(defun my/note--db-version (db)
  "Return SQLite user_version for DB."
  (or (caar (sqlite-select db "pragma user_version")) 0))

(defun my/note--db-set-version (db version)
  "Set SQLite user_version for DB to VERSION."
  (sqlite-execute db (format "pragma user_version = %d" version)))

(defun my/note--db-column-exists-p (db table column)
  "Return non-nil when TABLE has COLUMN in DB."
  (seq-some (lambda (row)
              (equal (nth 1 row) column))
            (sqlite-select db (format "pragma table_info(%s)" table))))

(defun my/note--db-add-column-if-missing (db table column definition)
  "Add COLUMN with DEFINITION to TABLE in DB when it is missing."
  (unless (my/note--db-column-exists-p db table column)
    (sqlite-execute db
                    (format "alter table %s add column %s %s"
                            table column definition))))

(defun my/note--db-init (db)
  "Initialize note schema in DB."
  (dolist (sql
           '("create table if not exists files (
                path text primary key,
                mtime real not null,
                size integer not null default 0,
                title text,
                node_id text
              )"
             "create table if not exists nodes (
                id text primary key,
                file text not null,
                title text not null,
                date text,
                summary text not null default '',
                position integer not null
              )"
             "create table if not exists tags (
                node_id text not null,
                tag text not null
              )"
             "create table if not exists aliases (
                node_id text not null,
                alias text not null
              )"
             "create table if not exists links (
                source_id text not null,
                target_id text not null,
                file text not null,
                line integer not null,
                label text
              )"
             "create index if not exists note_nodes_file_idx on nodes(file)"
             "create index if not exists note_tags_node_idx on tags(node_id)"
             "create index if not exists note_aliases_node_idx on aliases(node_id)"
             "create index if not exists note_links_target_idx on links(target_id)"
             "create index if not exists note_links_source_idx on links(source_id)"))
    (sqlite-execute db sql))
  (my/note--db-add-column-if-missing db "files" "size"
                                    "integer not null default 0")
  (my/note--db-add-column-if-missing db "nodes" "summary"
                                    "text not null default ''")
  (when (< (my/note--db-version db) my/note--schema-version)
    (my/note--db-set-version db my/note--schema-version)))

(defun my/note--db-ro-close ()
  "Close the cached read-only note DB connection."
  (when my/note--db-ro
    (ignore-errors (sqlite-close my/note--db-ro))
    (setq my/note--db-ro nil
          my/note--db-ro-file nil)))

(defun my/note--db-ro ()
  "Return a cached read-only note DB connection."
  (my/note--ensure-db)
  (unless (and my/note--db-ro
               (equal my/note--db-ro-file my/note-db-file))
    (my/note--db-ro-close)
    (setq my/note--db-ro (my/note--db-open t)
          my/note--db-ro-file my/note-db-file))
  my/note--db-ro)

(defmacro my/note--with-write-db (db &rest body)
  "Evaluate BODY with writable note DB bound to DB."
  (declare (indent 1))
  `(progn
     (my/note--db-ro-close)
     (let ((,db (my/note--db-open)))
       (unwind-protect
           (progn ,@body)
         (sqlite-close ,db)))))

(add-hook 'kill-emacs-hook #'my/note--db-ro-close)

(defun my/note--db-clear (db)
  "Clear all indexed note rows in DB."
  (dolist (table '("links" "aliases" "tags" "nodes" "files"))
    (sqlite-execute db (format "delete from %s" table))))

(defun my/note--db-delete-file (db file)
  "Delete DB rows derived from FILE."
  (let ((ids (mapcar #'car
                     (sqlite-select db
                                    "select id from nodes where file = ?"
                                    (vector file)))))
    (dolist (id ids)
      (sqlite-execute db "delete from links where source_id = ?" (vector id))
      (sqlite-execute db "delete from tags where node_id = ?" (vector id))
      (sqlite-execute db "delete from aliases where node_id = ?" (vector id))
      (sqlite-execute db "delete from nodes where id = ?" (vector id)))
    (sqlite-execute db "delete from files where path = ?" (vector file))))

(defun my/note--sql-value (value)
  "Return VALUE or nil for SQLite insertion."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun my/note--db-insert-note (db note)
  "Insert parsed NOTE into DB."
  (let ((file (plist-get note :file))
        (id (plist-get note :id))
        (title (plist-get note :title))
        (date (plist-get note :date))
        (summary (plist-get note :summary))
        (position (plist-get note :position))
        (mtime (plist-get note :mtime))
        (size (plist-get note :size)))
    (sqlite-execute db
                    "insert into files(path, mtime, size, title, node_id)
                     values (?, ?, ?, ?, ?)"
                    (vector file mtime (or size 0) title id))
    (sqlite-execute db
                    "insert into nodes(id, file, title, date, summary, position)
                     values (?, ?, ?, ?, ?, ?)"
                    (vector id file title (my/note--sql-value date)
                            (or summary "") position))
    (dolist (tag (plist-get note :tags))
      (sqlite-execute db "insert into tags values (?, ?)"
                      (vector id tag)))
    (dolist (alias (plist-get note :aliases))
      (sqlite-execute db "insert into aliases values (?, ?)"
                      (vector id alias)))
    (dolist (link (plist-get note :links))
      (sqlite-execute db "insert into links values (?, ?, ?, ?, ?)"
                      (vector id
                              (plist-get link :target)
                              file
                              (plist-get link :line)
                              (my/note--sql-value
                               (plist-get link :label)))))))

(defun my/note--typst-string-unescape (value)
  "Return Typst string VALUE with basic escapes decoded."
  (let ((result (or value "")))
    (setq result (replace-regexp-in-string "\\\\\"" "\"" result t t))
    (setq result (replace-regexp-in-string "\\\\n" "\n" result t t))
    (setq result (replace-regexp-in-string "\\\\t" "\t" result t t))
    (setq result (replace-regexp-in-string "\\\\\\\\" "\\\\" result t t))
    result))

(defun my/note--metadata-body ()
  "Return (BODY . POSITION) for the first note metadata block, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#metadata[ \t\n]*((" nil t)
      (let ((beg (point))
            (pos (match-beginning 0))
            (depth 2)
            close-start
            in-string
            escaped
            end)
        (while (and (not end) (not (eobp)))
          (let ((char (char-after)))
            (cond
             (escaped
              (setq escaped nil))
             ((and in-string (eq char ?\\))
              (setq escaped t))
             ((eq char ?\")
              (setq in-string (not in-string)))
             (in-string)
             ((eq char ?\()
              (setq depth (1+ depth)
                    close-start nil))
             ((eq char ?\))
              (setq depth (1- depth))
              (cond
               ((= depth 1)
                (setq close-start (point)))
               ((zerop depth)
                (setq end close-start))
               (t
                (setq close-start nil)))))
            (forward-char 1)))
        (when end
          (cons (buffer-substring-no-properties beg end) pos))))))

(defun my/note--metadata-string (body key)
  "Return string value for KEY in metadata BODY."
  (when (string-match
         (format "\\_<%s\\_>[ \t\n]*:[ \t\n]*\"\\([^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\\)\""
                 (regexp-quote key))
         body)
    (my/note--typst-string-unescape (match-string 1 body))))

(defun my/note--metadata-list (body key)
  "Return list of string values for KEY in metadata BODY."
  (when (string-match
         (format "\\_<%s\\_>[ \t\n]*:[ \t\n]*(\\([^)]*\\))"
                 (regexp-quote key))
         body)
    (let ((raw (match-string 1 body))
          values)
      (with-temp-buffer
        (insert raw)
        (goto-char (point-min))
        (while (re-search-forward "\"\\([^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\\)\"" nil t)
          (push (my/note--typst-string-unescape (match-string 1)) values)))
      (nreverse values))))

(defun my/note--line-at-pos (pos)
  "Return one-based line number at POS in current buffer."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun my/note--summary-from-current-buffer ()
  "Return a compact plain-text summary for the current Typst note."
  (save-excursion
    (goto-char (point-min))
    (let (lines)
      (while (and (not (eobp)) (< (length lines) 3))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (string-prefix-p "=" line)
                      (string-prefix-p "//" line))
            (push line lines)))
        (forward-line 1))
      (truncate-string-to-width
       (replace-regexp-in-string
        "[ \t\n\r]+" " "
        (string-trim (mapconcat #'identity (nreverse lines) " ")))
       180 nil nil t))))

(defun my/note--scan-links ()
  "Return all `#note(\"id\")[label]' links in the current buffer."
  (let (links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "#note[ \t\n]*(\"\\([^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\\)\")[ \t\n]*\\[\\([^]\n]*\\)\\]"
              nil t)
        (push (list :target (my/note--typst-string-unescape (match-string 1))
                    :label (string-trim (match-string-no-properties 2))
                    :line (my/note--line-at-pos (match-beginning 0)))
              links)))
    (nreverse links)))

(defun my/note-parse-current-buffer (&optional file)
  "Parse the current buffer as a Typst note.
FILE is the file path associated with the buffer."
  (when-let* ((entry (my/note--metadata-body)))
    (let* ((body (car entry))
           (id (my/note--metadata-string body "id"))
           (title (my/note--metadata-string body "title")))
      (when (and id title)
        (list :file (and file (file-truename file))
              :mtime (if (and file (file-exists-p file))
                         (float-time (file-attribute-modification-time
                                      (file-attributes file)))
                       0.0)
              :id id
              :title title
              :date (my/note--metadata-string body "date")
              :summary (my/note--summary-from-current-buffer)
              :tags (or (my/note--metadata-list body "tags") nil)
              :aliases (or (my/note--metadata-list body "aliases") nil)
              :position (cdr entry)
              :size (if (and file (file-exists-p file))
                        (file-attribute-size (file-attributes file))
                      0)
              :links (my/note--scan-links))))))

(defun my/note-parse-file (file)
  "Parse Typst note metadata and links from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (my/note-parse-current-buffer file)))

(defun my/note--excluded-path-p (file)
  "Return non-nil when FILE is below an excluded note directory."
  (let* ((relative (file-relative-name
                    (file-truename file)
                    (file-name-as-directory (file-truename my/note-root))))
         (parts (split-string relative "/" t)))
    (seq-some (lambda (part)
                (member part my/note-excluded-directories))
              parts)))

(defun my/note--typst-files ()
  "Return Typst note candidate files below `my/note-root'."
  (seq-remove #'my/note--excluded-path-p
              (directory-files-recursively my/note-root "\\.typ\\'")))

(defun my/note--file-fingerprint (file)
  "Return (MTIME . SIZE) for FILE."
  (let ((attrs (file-attributes file)))
    (cons (float-time (file-attribute-modification-time attrs))
          (file-attribute-size attrs))))

(defun my/note--db-files-state (db)
  "Return a hash table mapping indexed file paths to (MTIME . SIZE)."
  (let ((state (make-hash-table :test 'equal)))
    (dolist (row (sqlite-select db "select path, mtime, size from files"))
      (pcase-let ((`(,path ,mtime ,size) row))
        (puthash path (cons mtime size) state)))
    state))

(defun my/note--root-relative-typst-path (file)
  "Return Typst root-relative path for FILE."
  (concat "/"
          (file-relative-name
           (file-truename file)
           (file-name-as-directory (file-truename my/note-root)))))

(defconst my/note--wrapper-directory "_typst/notes"
  "Directory below `my/note-root' containing generated note id wrappers.")

(defun my/note--safe-wrapper-id-p (id)
  "Return non-nil when ID can safely be used as a wrapper file name."
  (and (stringp id)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" id)))

(defun my/note--wrapper-file (id)
  "Return generated wrapper file path for note ID."
  (unless (my/note--safe-wrapper-id-p id)
    (user-error "Note id is not safe as a Typst wrapper path: %s" id))
  (expand-file-name
   (concat id ".typ")
   (expand-file-name my/note--wrapper-directory
                     (file-name-as-directory my/note-root))))

(defun my/note--wrapper-source (note)
  "Return Typst wrapper source for NOTE."
  (let ((target (my/note--root-relative-typst-path
                 (plist-get note :file))))
    (format "#import %S: *\n#include %S\n" target target)))

(defun my/note-write-wrapper-files (notes)
  "Write per-note Typst id wrapper files for NOTES."
  (let ((dir (expand-file-name my/note--wrapper-directory
                               (file-name-as-directory my/note-root))))
    (when (file-directory-p dir)
      (delete-directory dir t))
    (make-directory dir t)
    (dolist (note notes)
      (with-temp-file (my/note--wrapper-file (plist-get note :id))
        (insert (my/note--wrapper-source note))))))

(defun my/note-write-wrapper-file (note)
  "Write the generated Typst wrapper for NOTE."
  (let ((file (my/note--wrapper-file (plist-get note :id))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (my/note--wrapper-source note)))
    file))

(defun my/note-delete-wrapper-file (id)
  "Delete generated Typst wrapper for note ID when it exists."
  (when (and id (my/note--safe-wrapper-id-p id))
    (let ((file (my/note--wrapper-file id)))
      (when (file-exists-p file)
        (delete-file file)))))

(defun my/note--path-registry-source (notes)
  "Return Typst source resolving note ids through generated wrappers.
NOTES is accepted for compatibility with callers that pass the indexed set."
  (ignore notes)
  (concat
   "#let note-include-active = state(\"my-note-include-active\", false)\n"
   "#let note-path(id) = \"/_typst/notes/\" + id + \".typ\"\n"
   "#let note-import-path(id) = note-path(id)\n"
   "#let note-include(id) = {\n"
   "  note-include-active.update(true)\n"
   "  include(note-path(id))\n"
   "  note-include-active.update(false)\n"
   "}\n"
   "#let note-transclude(id) = note-include(id)\n\n"))

(defun my/note--style-file (&optional name)
  "Return the Typst note style file named NAME."
  (expand-file-name (or name "note.typ")
                    (file-name-as-directory my/note-style-directory)))

(defun my/note--style-source (&optional name)
  "Return Typst note style source named NAME."
  (let ((file (my/note--style-file name)))
    (unless (file-readable-p file)
      (user-error "Missing Typst note style file: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun my/note--helper-source-for-notes (notes)
  "Return Typst helper source for note references."
  (ignore notes)
  (my/note--style-source "note.typ"))

(defun my/note-write-helper-file (notes)
  "Write the shared Typst note helper for NOTES."
  (let ((file (expand-file-name "_typst/note.typ"
                                (file-name-as-directory my/note-root))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (my/note--helper-source-for-notes notes)))
    file))

(defun my/note-db-sync-file (file)
  "Sync one Typst note FILE into the index."
  (interactive (list (or buffer-file-name
                         (read-file-name "Typst note: " my/note-root nil t))))
  (let ((file (file-truename file))
        synced)
    (my/note--with-write-db db
      (my/note--db-init db)
      (let (committed)
        (unwind-protect
            (progn
              (sqlite-transaction db)
              (let ((old-id (caar (sqlite-select db
                                                 "select node_id from files where path = ?"
                                                 (vector file)))))
                (my/note--db-delete-file db file)
                (if (and (file-exists-p file)
                         (not (my/note--excluded-path-p file)))
                    (when-let* ((note (my/note-parse-file file)))
                      (my/note--db-insert-note db note)
                      (my/note-write-wrapper-file note)
                      (unless (equal old-id (plist-get note :id))
                        (my/note-delete-wrapper-file old-id))
                      (setq synced note))
                  (my/note-delete-wrapper-file old-id)))
              (sqlite-commit db)
              (setq committed t))
          (unless committed
            (ignore-errors (sqlite-rollback db))))))
    synced))

;;;###autoload
(defun my/note-db-sync (&optional force)
  "Sync the Typst note index incrementally.
With prefix FORCE, rebuild the whole index and all generated wrappers."
  (interactive "P")
  (run-hooks 'my/note-before-sync-hook)
  (let ((result
         (my/note--with-write-db db
           (my/note--db-init db)
           (let (committed)
             (unwind-protect
                 (progn
                   (sqlite-transaction db)
                   (let* ((disk-files (mapcar #'file-truename (my/note--typst-files)))
                          (disk-set (make-hash-table :test 'equal))
                          (db-state (my/note--db-files-state db))
                          (changed 0)
                          (removed 0)
                          (kept 0)
                          notes)
                     (when force
                       (my/note--db-clear db)
                       (setq db-state (make-hash-table :test 'equal)))
                     (dolist (file disk-files)
                       (puthash file t disk-set)
                       (let ((fingerprint (my/note--file-fingerprint file)))
                         (if (and (not force)
                                  (equal fingerprint (gethash file db-state)))
                             (setq kept (1+ kept))
                           (my/note--db-delete-file db file)
                           (when-let* ((note (my/note-parse-file file)))
                             (my/note--db-insert-note db note)
                             (my/note-write-wrapper-file note)
                             (push note notes)
                             (setq changed (1+ changed))))))
                     (maphash
                      (lambda (file _fingerprint)
                        (unless (gethash file disk-set)
                          (let ((old-id (caar (sqlite-select
                                               db
                                               "select node_id from files where path = ?"
                                               (vector file)))))
                            (my/note--db-delete-file db file)
                            (my/note-delete-wrapper-file old-id)
                            (setq removed (1+ removed)))))
                      db-state)
                     (when force
                       (my/note-write-wrapper-files
                        (mapcar (lambda (row)
                                  (list :id (nth 0 row) :file (nth 1 row)))
                                (sqlite-select db "select id, file from nodes"))))
                     (my/note-write-helper-file notes)
                     (sqlite-commit db)
                     (setq committed t)
                     (let ((total (caar (sqlite-select db "select count(*) from nodes"))))
                       (list total changed removed kept))))
               (unless committed
                 (ignore-errors (sqlite-rollback db))))))))
    (pcase-let ((`(,total ,changed ,removed ,kept) result))
      (message "Note index synced: %d Typst notes (%d changed, %d removed, %d kept)"
               total changed removed kept)
      (run-hook-with-args 'my/note-after-sync-hook changed removed kept)
      total)))

(defun my/note--ensure-db ()
  "Ensure the note database exists and has schema."
  (if (file-exists-p my/note-db-file)
      (my/note--with-write-db db
        (my/note--db-init db))
    (my/note-db-sync)))

(defun my/note--rows (sql &optional values)
  "Return rows from note database for SQL and VALUES."
  (sqlite-select (my/note--db-ro) sql values))

(defun my/note--node-plist-from-row (row)
  "Return node plist from SQL ROW."
  (pcase-let ((`(,id ,file ,title ,date ,tags ,aliases) row))
    (list :id id
          :file file
          :title title
          :date date
          :tags (if (and tags (not (string-empty-p tags)))
                    (split-string tags "," t)
                  nil)
          :aliases (if (and aliases (not (string-empty-p aliases)))
                       (split-string aliases "\x1f" t)
                     nil))))

(defun my/note--node-rows ()
  "Return indexed note node rows."
  (my/note--rows
   "select n.id,
           n.file,
           n.title,
           n.date,
           coalesce((select group_concat(tag, ',')
                     from tags where node_id = n.id), ''),
           coalesce((select group_concat(alias, char(31))
                     from aliases where node_id = n.id), '')
    from nodes n
    order by lower(n.title), n.file"))

(defun my/note--node-candidates ()
  "Return completion candidates for indexed notes."
  (mapcar
   (lambda (row)
     (let* ((node (my/note--node-plist-from-row row))
            (tags (plist-get node :tags))
            (aliases (plist-get node :aliases))
            (label (format "%s%s  %s"
                           (plist-get node :title)
                           (if tags
                               (format "  :%s:" (string-join tags ":"))
                             "")
                           (file-relative-name
                            (plist-get node :file)
                            (file-name-as-directory my/note-root)))))
       (cons (if aliases
                 (format "%s  aliases:%s" label (string-join aliases ","))
               label)
             node)))
   (my/note--node-rows)))

(defun my/note--read-node (&optional prompt)
  "Read a note node with PROMPT."
  (let* ((candidates (my/note--node-candidates))
         (choice (completing-read (or prompt "Note: ") candidates nil t)))
    (cdr (assoc choice candidates))))

(defun my/note-read-node (&optional prompt)
  "Read an indexed note node with PROMPT."
  (my/note--read-node prompt))

(defun my/note--node-by-id (id)
  "Return the indexed note node for ID, or nil."
  (when (and (stringp id)
             (not (string-empty-p id)))
    (when-let* ((row (car (my/note--rows
                           "select n.id,
                                   n.file,
                                   n.title,
                                   n.date,
                                   coalesce((select group_concat(tag, ',')
                                             from tags where node_id = n.id), ''),
                                   coalesce((select group_concat(alias, char(31))
                                             from aliases where node_id = n.id), '')
                            from nodes n
                            where n.id = ?
                            limit 1"
                           (vector id)))))
      (my/note--node-plist-from-row row))))

(defun my/note--typst-content-escape (value)
  "Escape VALUE for a simple Typst content block."
  (mapconcat
   (lambda (char)
     (pcase char
       (?\\ "\\\\")
       (?\[ "\\[")
       (?\] "\\]")
       (_ (char-to-string char))))
   (or value "")
   ""))

;;;###autoload
(defun my/note-node-find ()
  "Find a Typst note node."
  (interactive)
  (let ((node (my/note--read-node "Find note: ")))
    (find-file (plist-get node :file))))

;;;###autoload
(defun my/note-node-insert ()
  "Insert a Typst note link at point."
  (interactive)
  (let ((node (my/note--read-node "Insert note: ")))
    (insert (format "#note(%S)[%s]"
                    (plist-get node :id)
                    (my/note--typst-content-escape
                     (plist-get node :title))))))

(defun my/note-open-id (id)
  "Open Typst note ID."
  (interactive (list (read-string "Note id: ")))
  (if-let* ((node (my/note--node-by-id id))
            (file (plist-get node :file)))
      (find-file file)
    (user-error "Unknown Typst note id: %s" id)))

(defun my/note--position-to-point (position)
  "Return buffer point for Tinymist zero-based POSITION vector."
  (when (and (vectorp position)
             (>= (length position) 2))
    (save-excursion
      (goto-char (point-min))
      (forward-line (aref position 0))
      (ignore-errors
        (forward-char (aref position 1)))
      (point))))

(defun my/note--link-id-at-point ()
  "Return the `#note' id at point, including when point is on its label."
  (let ((pos (point))
        (beg (max (point-min) (- (point) 2000)))
        (end (min (point-max) (+ (point) 2000)))
        (case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (catch 'id
        (while (re-search-forward
                "#note[ \t\n]*(\"\\([^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\\)\")"
                end t)
          (let ((call-start (match-beginning 0))
                (id (my/note--typst-string-unescape (match-string 1)))
                (after-call (match-end 0)))
            (goto-char after-call)
            (when (looking-at "[ \t\n]*\\[")
              (goto-char (match-end 0))
              (when (search-forward "]" end t)
                (when (<= call-start pos (point))
                  (throw 'id id))))))))))

(defun my/note--zotero-url-at-point ()
  "Return the `#zoterolink' URL at point, including when point is on its label."
  (let ((pos (point))
        (beg (max (point-min) (- (point) 2000)))
        (end (min (point-max) (+ (point) 2000)))
        (case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (catch 'url
        (while (re-search-forward
                "#zoterolink[ \t\n]*(\"\\([^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\\)\")"
                end t)
          (let ((call-start (match-beginning 0))
                (url (my/note--typst-string-unescape (match-string 1)))
                (after-call (match-end 0)))
            (goto-char after-call)
            (when (looking-at "[ \t\n]*\\[")
              (goto-char (match-end 0))
              (when (search-forward "]" end t)
                (when (<= call-start pos (point))
                  (throw 'url url))))))))))

(defun my/note-open-zotero-url (url)
  "Open Zotero URL using the system URL handler."
  (unless (and (stringp url)
               (string-prefix-p "zotero://" url))
    (user-error "Not a Zotero URL: %s" url))
  (let ((command (cond
                  ((eq system-type 'darwin) "open")
                  ((executable-find "xdg-open") "xdg-open")
                  (t nil))))
    (unless command
      (user-error "No system URL opener found for Zotero links"))
    (start-process "zotero-opener" nil command url)
    (message "Opening Zotero link: %s" url)))

(defun my/note--xref-backend ()
  "Return the note xref backend for Typst note links."
  (when (and buffer-file-name
             (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
             (my/note--link-id-at-point))
    'my/note))

;;;###autoload
(defun my/note-open-link-at-file-position (file position)
  "Open a note link at FILE and Tinymist POSITION.
Return non-nil when a note link was opened."
  (when-let* ((point (and (stringp file)
                          (file-readable-p file)
                          (with-current-buffer (or (get-file-buffer file)
                                                   (find-file-noselect file))
                            (my/note--position-to-point position)))))
    (with-current-buffer (or (get-file-buffer file)
                             (find-file-noselect file))
      (save-excursion
        (goto-char point)
        (cond
         ((when-let* ((url (my/note--zotero-url-at-point)))
            (my/note-open-zotero-url url)
            t))
         ((when-let* ((id (my/note--link-id-at-point)))
            (my/note-open-id id)
            t)))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql my/note)))
  "Return the Typst note id at point."
  (my/note--link-id-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql my/note)) id)
  "Return xref definitions for Typst note ID."
  (when-let* ((node (my/note--node-by-id id))
              (file (plist-get node :file)))
    (list
     (xref-make
      (format "%s  %s"
              (plist-get node :title)
              (file-relative-name file (file-name-as-directory my/note-root)))
      (xref-make-file-location file 1 0)))))

(defun my/note-xref-setup ()
  "Enable `gd'/`M-.' navigation for Typst `#note' links."
  (add-hook 'xref-backend-functions #'my/note--xref-backend nil t))

(defun my/note--buffer-note-file-p ()
  "Return non-nil when the current buffer visits an indexed note candidate."
  (and buffer-file-name
       (string-suffix-p ".typ" buffer-file-name)
       (file-in-directory-p (file-truename buffer-file-name)
                            (file-name-as-directory
                             (file-truename my/note-root)))
       (not (my/note--excluded-path-p buffer-file-name))))

(defun my/note-sync-current-buffer ()
  "Sync the current Typst note buffer into the index."
  (interactive)
  (unless (my/note--buffer-note-file-p)
    (user-error "Current buffer is not a Typst note below my/note-root"))
  (save-buffer)
  (my/note-db-sync-file buffer-file-name)
  (message "Note synced: %s"
           (file-relative-name buffer-file-name
                               (file-name-as-directory my/note-root))))

(defun my/note-after-save-sync ()
  "Update the note index after saving a Typst note buffer."
  (when (and my/note-auto-sync-on-save
             (my/note--buffer-note-file-p))
    (condition-case err
        (my/note-db-sync-file buffer-file-name)
      (error
       (message "Note after-save sync failed: %s" err)))))

(defun my/note-buffer-setup ()
  "Enable note xref and after-save indexing for Typst buffers."
  (my/note-xref-setup)
  (add-hook 'after-save-hook #'my/note-after-save-sync nil t))

;;;###autoload
(defun my/note-open-at-point ()
  "Open the Typst note or Zotero link at point."
  (interactive)
  (cond
   ((when-let* ((url (my/note--zotero-url-at-point)))
      (my/note-open-zotero-url url)
      t))
   ((when-let* ((id (my/note--link-id-at-point)))
      (my/note-open-id id)
      t))
   (t
    (user-error "No Typst note or Zotero link at point"))))

(defun my/note--typst-reference-at-point-p ()
  "Return non-nil when point is on a Typst `@label' reference."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "[:alnum:]_:-")
      (or (looking-at-p "@[[:alnum:]_:-]+")
          (and (> (point) (point-min))
               (eq (char-before) ?@)
               (<= (point) pos))))))

;;;###autoload
(defun my/note-open-or-preview-sync ()
  "Open a Typst note/Zotero link, follow a Typst ref, or sync preview."
  (interactive)
  (if-let* ((url (my/note--zotero-url-at-point)))
      (my/note-open-zotero-url url)
    (if-let* ((id (my/note--link-id-at-point)))
        (my/note-open-id id)
    (cond
     ((and (my/note--typst-reference-at-point-p)
           (fboundp 'my/navigation-find-definition))
      (my/navigation-find-definition))
     ((fboundp 'my/typst-preview-send-position)
      (my/typst-preview-send-position))
     (t
      (user-error "Typst preview sync is unavailable"))))))

(defun my/note-current-node-id ()
  "Return the current Typst note node id."
  (or (when buffer-file-name
        (plist-get (my/note-parse-current-buffer buffer-file-name) :id))
      (when buffer-file-name
        (caar
         (my/note--rows
          "select id from nodes where file = ? limit 1"
          (vector (file-truename buffer-file-name)))))
      (user-error "Current buffer is not an indexed Typst note")))

(defun my/note-current-node ()
  "Return the current Typst note node plist."
  (my/note--node-by-id (my/note-current-node-id)))

;;;###autoload
(defun my/note-backlinks ()
  "Show backlinks for the current Typst note."
  (interactive)
  (let* ((id (my/note-current-node-id))
         (rows (my/note--rows
                "select l.source_id, n.title, l.file, l.line, l.label
                 from links l
                 left join nodes n on n.id = l.source_id
                 where l.target_id = ?
                 order by lower(coalesce(n.title, l.source_id)), l.line"
                (vector id)))
         (buffer (get-buffer-create "*note-backlinks*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Backlinks to %s\n\n" id))
        (if rows
            (dolist (row rows)
              (pcase-let ((`(,source-id ,title ,file ,line ,label) row))
                (insert-text-button
                 (format "%s:%s" (file-relative-name file my/note-root) line)
                 'action (lambda (_)
                           (find-file file)
                           (goto-char (point-min))
                           (forward-line (1- line)))
                 'follow-link t)
                (insert (format "  %s%s\n"
                                (or title source-id)
                                (if (and label (not (string-empty-p label)))
                                    (format "  [%s]" label)
                                  "")))))
          (insert "No backlinks.\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun my/note-doctor--duplicate-ids ()
  "Return duplicate note id issues discovered by scanning Typst files."
  (let ((seen (make-hash-table :test 'equal))
        issues)
    (dolist (file (my/note--typst-files))
      (when-let* ((note (ignore-errors (my/note-parse-file file)))
                  (id (plist-get note :id)))
        (if-let* ((first (gethash id seen)))
            (push (list :id id :file file :line 1
                        :message (format "Duplicate id %s; first seen in %s"
                                         id
                                         (file-relative-name
                                          first
                                          (file-name-as-directory my/note-root))))
                  issues)
          (puthash id file seen))))
    (nreverse issues)))

(defun my/note-doctor--collect ()
  "Return a plist describing note index anomalies."
  (let* ((nodes (my/note--rows
                 "select id, file, title, coalesce(date, '') from nodes"))
         (links (my/note--rows
                 "select source_id, target_id, file, line, coalesce(label, '')
                  from links"))
         (aliases (my/note--rows
                   "select node_id, alias from aliases where alias <> ''"))
         (id-set (make-hash-table :test 'equal))
         (outgoing (make-hash-table :test 'equal))
         (incoming (make-hash-table :test 'equal))
         (alias-map (make-hash-table :test 'equal))
         dangling missing-date orphans alias-collisions)
    (dolist (row nodes)
      (pcase-let ((`(,id ,file ,_title ,date) row))
        (puthash id file id-set)
        (when (string-empty-p date)
          (push (list :id id :file file :line 1
                      :message (format "%s has no date" id))
                missing-date))))
    (dolist (row links)
      (pcase-let ((`(,source ,target ,file ,line ,label) row))
        (puthash source t outgoing)
        (puthash target t incoming)
        (unless (gethash target id-set)
          (push (list :id source :file file :line line
                      :message (format "%s -> missing %s%s"
                                       source target
                                       (if (string-empty-p label)
                                           ""
                                         (format " [%s]" label))))
                dangling))))
    (dolist (row nodes)
      (pcase-let ((`(,id ,file ,_title ,_date) row))
        (unless (or (gethash id outgoing)
                    (gethash id incoming))
          (push (list :id id :file file :line 1
                      :message (format "%s has no note links or backlinks" id))
                orphans))))
    (dolist (row aliases)
      (pcase-let ((`(,node-id ,alias) row))
        (let ((owners (gethash alias alias-map)))
          (unless (member node-id owners)
            (puthash alias (cons node-id owners) alias-map)))))
    (maphash
     (lambda (alias owners)
       (when (> (length owners) 1)
         (dolist (owner owners)
           (when-let* ((file (gethash owner id-set)))
             (push (list :id owner :file file :line 1
                         :message (format "Alias %S is shared by %s"
                                          alias
                                          (string-join (sort (copy-sequence owners)
                                                             #'string<)
                                                       ", ")))
                   alias-collisions)))))
     alias-map)
    (list :dangling-links (nreverse dangling)
          :duplicate-ids (my/note-doctor--duplicate-ids)
          :missing-date (nreverse missing-date)
          :orphans (nreverse orphans)
          :alias-collisions (nreverse alias-collisions))))

(defun my/note-doctor--jump (button)
  "Open the issue target stored in BUTTON."
  (let ((file (button-get button 'file))
        (line (or (button-get button 'line) 1)))
    (find-file file)
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))))

(defun my/note-doctor--insert-section (title issues)
  "Insert doctor section TITLE with ISSUES."
  (insert title "\n")
  (if issues
      (dolist (issue issues)
        (let ((file (plist-get issue :file))
              (line (or (plist-get issue :line) 1)))
          (insert-text-button
           (format "  %s:%d"
                   (file-relative-name file
                                       (file-name-as-directory my/note-root))
                   line)
           'file file
           'line line
           'action #'my/note-doctor--jump
           'follow-link t)
          (insert "  " (plist-get issue :message) "\n")))
    (insert "  OK\n"))
  (insert "\n"))

;;;###autoload
(defun my/note-doctor ()
  "Inspect the Typst note knowledge base for common consistency issues."
  (interactive)
  (my/note--ensure-db)
  (let ((report (my/note-doctor--collect))
        (buffer (get-buffer-create "*note-doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (my/note-doctor--insert-section
         "Dangling #note links" (plist-get report :dangling-links))
        (my/note-doctor--insert-section
         "Duplicate ids" (plist-get report :duplicate-ids))
        (my/note-doctor--insert-section
         "Missing date" (plist-get report :missing-date))
        (my/note-doctor--insert-section
         "Orphan notes" (plist-get report :orphans))
        (my/note-doctor--insert-section
         "Alias collisions" (plist-get report :alias-collisions))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)
    report))

(defun my/note--slugify (value)
  "Return a conservative filename slug for VALUE."
  (let ((slug (downcase (string-trim value))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    (or (string-trim slug "-+" "-+") "note")))

(defun my/note--new-id (title)
  "Return a new note id for TITLE."
  (format "%s-%s"
          (format-time-string "%Y%m%dT%H%M%S")
          (my/note--slugify title)))

(defun my/note--new-id-for-type (type title)
  "Return a new note id for TYPE and TITLE."
  (pcase (my/note-type-id-strategy type)
    ('timestamp-slug (my/note--new-id title))
    ('date-only (format-time-string "%Y-%m-%d"))
    ((pred functionp) (funcall (my/note-type-id-strategy type) title))
    (_ (my/note--new-id title))))

(defun my/note--metadata-source (id title tags)
  "Return Typst metadata source for ID TITLE and TAGS."
  (format
   "#metadata((
  kind: \"note\",
  id: %S,
  title: %S,
  date: %S,
  tags: (%s),
  aliases: (),
)) <%s>
"
   id
   title
   (format-time-string "%Y-%m-%d")
   (mapconcat (lambda (tag) (format "%S" tag)) tags ", ")
   my/note-metadata-label))

(defun my/note-ensure-helper-file ()
  "Ensure the shared Typst note helper exists below `my/note-root'."
  (let ((file (expand-file-name "_typst/note.typ"
                                (file-name-as-directory my/note-root))))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (my/note--style-source "note.typ"))))
    file))

(defun my/note--type-by-name (name)
  "Return registered note type NAME."
  (seq-find (lambda (type)
              (eq (my/note-type-name type) name))
            my/note-types))

(defun my/note--read-type ()
  "Read a registered note type."
  (let* ((candidates
          (mapcar (lambda (type)
                    (cons (format "%s  %s"
                                  (my/note-type-name type)
                                  (my/note-type-prompt type))
                          type))
                  my/note-types))
         (choice (completing-read "Note type: " candidates nil t)))
    (cdr (assoc choice candidates))))

(defun my/note--create-note-file (type title tags)
  "Create a note of TYPE with TITLE and TAGS."
  (let* ((title (string-trim title))
         (type (or type (my/note--type-by-name 'default)))
         (tags (seq-uniq
                (seq-filter (lambda (tag)
                              (and (stringp tag)
                                   (not (string-empty-p tag))))
                            (append tags (my/note-type-default-tags type)))
                #'string=)))
    (when (string-empty-p title)
      (user-error "Title is empty"))
    (let* ((id (my/note--new-id-for-type type title))
           (slug (concat (or (my/note-type-slug-prefix type) "")
                         (my/note--slugify title)))
           (directory (file-name-as-directory my/note-directory))
           (file (expand-file-name (concat slug ".typ") directory))
           (template-fn (or (my/note-type-template-fn type)
                            #'my/note--template-default)))
      (make-directory directory t)
      (my/note-ensure-helper-file)
      (when (file-exists-p file)
        (user-error "Note already exists: %s" file))
      (find-file file)
      (insert (funcall template-fn id title tags))
      (save-buffer)
      (when-let* ((note (my/note-db-sync-file file)))
        (run-hook-with-args 'my/note-new-hook note))
      file)))

;;;###autoload
(defun my/note-new (title tag)
  "Create a new Typst note with TITLE and TAG."
  (interactive
   (list (read-string "Title: ")
         (read-string "Tag: ")))
  (my/note--create-note-file
   (my/note--type-by-name 'default)
   title
   (if (string-empty-p tag) nil (list tag))))

;;;###autoload
(defun my/note-new-of-type (type title)
  "Create a new Typst note using registered note TYPE."
  (interactive
   (let ((type (my/note--read-type)))
     (list type (read-string "Title: "))))
  (my/note--create-note-file type title nil))

(defun my/note--setup-keys (map)
  "Bind Typst note keys in MAP."
  (define-key map (kbd "C-c n a") #'my/note-alias-add)
  (define-key map (kbd "C-c n A") #'my/note-agenda)
  (define-key map (kbd "C-c n c") #'my/note-capture)
  (define-key map (kbd "C-c n d") #'my/note-daily-today)
  (define-key map (kbd "C-c n D") #'my/note-doctor)
  (define-key map (kbd "C-c n f") #'my/note-node-find)
  (define-key map (kbd "C-c n g") #'my/note-graph)
  (define-key map (kbd "C-c n i") #'my/note-node-insert)
  (define-key map (kbd "C-c n l") #'my/note-backlinks)
  (define-key map (kbd "C-c n r") #'my/note-reference-insert)
  (define-key map (kbd "C-c n s") #'my/note-db-sync)
  (define-key map (kbd "C-c n n") #'my/note-new)
  (define-key map (kbd "C-c n N") #'my/note-new-of-type)
  (define-key map (kbd "C-c n t") #'my/note-tag-add)
  (define-key map (kbd "C-c n RET") #'my/note-open-at-point))

(define-prefix-command 'my/note-command-map)
(global-set-key (kbd "C-c n") 'my/note-command-map)
(dolist (binding '(("a" . my/note-alias-add)
                   ("A" . my/note-agenda)
                   ("c" . my/note-capture)
                   ("d" . my/note-daily-today)
                   ("D" . my/note-doctor)
                   ("f" . my/note-node-find)
                   ("g" . my/note-graph)
                   ("i" . my/note-node-insert)
                   ("l" . my/note-backlinks)
                   ("n" . my/note-new)
                   ("N" . my/note-new-of-type)
                   ("o" . my/note-open-or-preview-sync)
                   ("r" . my/note-reference-insert)
                   ("RET" . my/note-open-at-point)
                   ("s" . my/note-db-sync)
                   ("t" . my/note-tag-add)))
  (define-key my/note-command-map (kbd (car binding)) (cdr binding)))

(defun my/note--setup-evil-keys (map)
  "Bind Evil normal-state Typst note keys in MAP."
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal map
      (kbd "RET") #'my/note-open-or-preview-sync
      (kbd "<return>") #'my/note-open-or-preview-sync
      (kbd "C-m") #'my/note-open-or-preview-sync)))

(with-eval-after-load 'typst-ts-mode
  (when (boundp 'typst-ts-mode-map)
    (my/note--setup-keys typst-ts-mode-map)
    (my/note--setup-evil-keys typst-ts-mode-map)))

(add-hook 'typst-ts-mode-hook #'my/note-buffer-setup)

(with-eval-after-load 'typst-mode
  (when (boundp 'typst-mode-map)
    (my/note--setup-keys typst-mode-map)
    (my/note--setup-evil-keys typst-mode-map)))

(dolist (hook '(typst-mode-hook my/typst-mode-hook))
  (add-hook hook #'my/note-buffer-setup))

(when (boundp 'my/typst-mode-map)
  (my/note--setup-keys my/typst-mode-map)
  (my/note--setup-evil-keys my/typst-mode-map))

(with-eval-after-load 'evil
  (when (boundp 'typst-ts-mode-map)
    (my/note--setup-evil-keys typst-ts-mode-map))
  (when (boundp 'typst-mode-map)
    (my/note--setup-evil-keys typst-mode-map))
  (when (boundp 'my/typst-mode-map)
    (my/note--setup-evil-keys my/typst-mode-map)))

(provide 'init-note)
;;; init-note.el ends here
