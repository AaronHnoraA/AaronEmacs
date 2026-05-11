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

(defconst my/note-metadata-label "note"
  "Typst label used for file-level note metadata.")

(defconst my/note-helper-import-source
  "#import \"/_typst/note.typ\": *\n#show: note-entry\n"
  "Root-relative Typst import for the shared note helper.")

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

(defun my/note--db-init (db)
  "Initialize note schema in DB."
  (dolist (sql
           '("create table if not exists files (
                path text primary key,
                mtime real not null,
                title text,
                node_id text
              )"
             "create table if not exists nodes (
                id text primary key,
                file text not null,
                title text not null,
                date text,
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
    (sqlite-execute db sql)))

(defun my/note--db-clear (db)
  "Clear all indexed note rows in DB."
  (dolist (table '("links" "aliases" "tags" "nodes" "files"))
    (sqlite-execute db (format "delete from %s" table))))

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
        (position (plist-get note :position))
        (mtime (plist-get note :mtime)))
    (sqlite-execute db "insert into files values (?, ?, ?, ?)"
                    (vector file mtime title id))
    (sqlite-execute db "insert into nodes values (?, ?, ?, ?, ?)"
                    (vector id file title (my/note--sql-value date) position))
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
            (pos (match-beginning 0)))
        (when (search-forward "))" nil t)
          (cons (buffer-substring-no-properties beg (- (point) 2))
                pos))))))

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
              :tags (or (my/note--metadata-list body "tags") nil)
              :aliases (or (my/note--metadata-list body "aliases") nil)
              :position (cdr entry)
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

;;;###autoload
(defun my/note-db-sync ()
  "Rebuild the Typst note index."
  (interactive)
  (let ((db (my/note--db-open)))
    (unwind-protect
        (progn
          (my/note--db-init db)
          (sqlite-transaction db)
          (condition-case err
              (progn
                (my/note--db-clear db)
                (let ((count 0)
                      notes)
                  (dolist (file (my/note--typst-files))
                    (when-let* ((note (my/note-parse-file file)))
                      (my/note--db-insert-note db note)
                      (push note notes)
                      (setq count (1+ count))))
                  (sqlite-commit db)
                  (my/note-write-wrapper-files notes)
                  (my/note-write-helper-file notes)
                  (message "Note index synced: %d Typst notes" count)
                  count))
            (error
             (sqlite-rollback db)
             (signal (car err) (cdr err)))))
      (sqlite-close db))))

(defun my/note--ensure-db ()
  "Ensure the note database exists and has schema."
  (unless (file-exists-p my/note-db-file)
    (my/note-db-sync)))

(defun my/note--rows (sql &optional values)
  "Return rows from note database for SQL and VALUES."
  (my/note--ensure-db)
  (let ((db (my/note--db-open t)))
    (unwind-protect
        (sqlite-select db sql values)
      (sqlite-close db))))

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
        (when-let* ((id (my/note--link-id-at-point)))
          (my/note-open-id id)
          t)))))

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

;;;###autoload
(defun my/note-open-at-point ()
  "Open the Typst note link at point."
  (interactive)
  (if-let* ((id (my/note--link-id-at-point)))
      (my/note-open-id id)
    (user-error "No Typst note link at point")))

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
  "Open a Typst note link, follow a Typst ref, or sync preview to point."
  (interactive)
  (if-let* ((id (my/note--link-id-at-point)))
      (my/note-open-id id)
    (cond
     ((and (my/note--typst-reference-at-point-p)
           (fboundp 'my/navigation-find-definition))
      (my/navigation-find-definition))
     ((fboundp 'my/typst-preview-send-position)
      (my/typst-preview-send-position))
     (t
      (user-error "Typst preview sync is unavailable")))))

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

;;;###autoload
(defun my/note-new (title tag)
  "Create a new Typst note with TITLE and TAG."
  (interactive
   (list (read-string "Title: ")
         (read-string "Tag: ")))
  (let* ((id (my/note--new-id title))
         (slug (my/note--slugify title))
         (directory (file-name-as-directory my/note-directory))
         (file (expand-file-name (concat slug ".typ") directory)))
    (make-directory directory t)
    (my/note-ensure-helper-file)
    (when (file-exists-p file)
      (user-error "Note already exists: %s" file))
    (find-file file)
    (insert my/note-helper-import-source "\n")
    (insert (my/note--metadata-source id title (if (string-empty-p tag)
                                                   nil
                                                 (list tag))))
    (insert "\n= " title "\n\n")
    (save-buffer)
    (my/note-db-sync)
    file))

(defun my/note--setup-keys (map)
  "Bind Typst note keys in MAP."
  (define-key map (kbd "C-c n a") #'my/note-alias-add)
  (define-key map (kbd "C-c n A") #'my/note-agenda)
  (define-key map (kbd "C-c n c") #'my/note-capture)
  (define-key map (kbd "C-c n d") #'my/note-daily-today)
  (define-key map (kbd "C-c n f") #'my/note-node-find)
  (define-key map (kbd "C-c n g") #'my/note-graph)
  (define-key map (kbd "C-c n i") #'my/note-node-insert)
  (define-key map (kbd "C-c n l") #'my/note-backlinks)
  (define-key map (kbd "C-c n r") #'my/note-reference-insert)
  (define-key map (kbd "C-c n s") #'my/note-db-sync)
  (define-key map (kbd "C-c n n") #'my/note-new)
  (define-key map (kbd "C-c n t") #'my/note-tag-add)
  (define-key map (kbd "C-c n RET") #'my/note-open-at-point))

(define-prefix-command 'my/note-command-map)
(global-set-key (kbd "C-c n") 'my/note-command-map)
(dolist (binding '(("a" . my/note-alias-add)
                   ("A" . my/note-agenda)
                   ("c" . my/note-capture)
                   ("d" . my/note-daily-today)
                   ("f" . my/note-node-find)
                   ("g" . my/note-graph)
                   ("i" . my/note-node-insert)
                   ("l" . my/note-backlinks)
                   ("n" . my/note-new)
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

(add-hook 'typst-ts-mode-hook #'my/note-xref-setup)

(with-eval-after-load 'typst-mode
  (when (boundp 'typst-mode-map)
    (my/note--setup-keys typst-mode-map)
    (my/note--setup-evil-keys typst-mode-map)))

(dolist (hook '(typst-mode-hook my/typst-mode-hook))
  (add-hook hook #'my/note-xref-setup))

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
