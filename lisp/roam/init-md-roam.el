;;; init-md-roam.el --- Markdown roam note navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Roam-style helpers for Aaronnote Markdown notes.  The file keeps the old
;; `my/typst-roam-*' names as compatibility aliases for the earlier Typst
;; workflow while the implementation now targets `.md' notes.

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'url-util)
(require 'xref)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")

(defgroup my/typst-roam nil
  "Roam-style navigation for Aaronnote Markdown notes."
  :group 'my/aaronnote)

(defconst my/typst-roam--module-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the Markdown roam Emacs bridge.")

(defcustom my/typst-roam-root
  (expand-file-name ".roam" user-emacs-directory)
  "Root directory of the Markdown roam note vault."
  :type 'directory
  :group 'my/typst-roam)

(defcustom my/typst-roam-recent-limit 24
  "Maximum number of recent Markdown roam notes kept in memory."
  :type 'integer
  :group 'my/typst-roam)

(defcustom my/typst-roam-select-window-height 0.32
  "Height for the bottom Markdown roam selector window."
  :type '(choice (number :tag "Fraction or rows")
                 (function :tag "Window height function"))
  :group 'my/typst-roam)

(defcustom my/typst-roam-runtime-root
  (expand-file-name "aaronnote" my/typst-roam--module-directory)
  "Root of the vendored Aaronnote runtime used by Markdown roam tooling."
  :type 'directory
  :group 'my/typst-roam)

(defcustom my/typst-roam-runtime-cli
  (expand-file-name "roam-cli.mjs" my/typst-roam-runtime-root)
  "Node bridge used to call the vendored Aaronnote roam runtime from Emacs."
  :type 'file
  :group 'my/typst-roam)

(defcustom my/typst-roam-sync-delay 1.5
  "Seconds to debounce automatic incremental roam-db sync after saving."
  :type 'number
  :group 'my/typst-roam)

(defvar my/typst-roam--recent nil
  "Recently opened Markdown roam note ids, newest first.")

(defvar my/typst-roam--runtime-index-cache nil)
(defvar my/typst-roam--runtime-index-cache-key nil)
(defvar my/typst-roam--sync-timer nil)
(defvar my/typst-roam--sync-changed-files nil)

(defun my/typst-roam-root ()
  "Return the Markdown roam notes root."
  (or (when buffer-file-name
        (when-let* ((dir (or (locate-dominating-file
                              buffer-file-name ".aaronnote-sync-state.json")
                             (locate-dominating-file
                              buffer-file-name ".aaronnote-asset-cleanup-state.json"))))
          (file-truename dir)))
      (when (boundp 'my/aaronnote--notes-root)
        (file-name-as-directory (expand-file-name my/aaronnote--notes-root)))
      (file-name-as-directory (expand-file-name my/typst-roam-root))))

(defun my/typst-roam--clear-runtime-cache ()
  "Clear cached Aaronnote runtime payloads."
  (setq my/typst-roam--runtime-index-cache nil
        my/typst-roam--runtime-index-cache-key nil
        my/typst-roam--scan-cache nil
        my/typst-roam--db-cache nil
        my/typst-roam--db-path-cache nil
        my/typst-roam--db-mtime nil))

(defun my/typst-roam--runtime-available-p ()
  "Return non-nil when the Aaronnote runtime bridge is available."
  (and (file-exists-p my/typst-roam-runtime-cli)
       (file-exists-p
        (expand-file-name "server/lib/index.mjs"
                          my/typst-roam-runtime-root))))

(defun my/typst-roam--runtime-call (action &rest args)
  "Call Aaronnote roam runtime ACTION synchronously with ARGS.
Return parsed JSON as hash tables/lists, or nil when the runtime is unavailable
or the command fails."
  (when (my/typst-roam--runtime-available-p)
    (with-temp-buffer
      (let* ((root (my/typst-roam-root))
             (default-directory my/typst-roam--module-directory)
             (process-environment
              (append (list (format "AARONNOTE_ROOT=%s" root)
                            (format "AARONNOTE_RUNTIME_ROOT=%s"
                                    (expand-file-name
                                     my/typst-roam-runtime-root))
                            (format "AARONNOTE_WORKSPACE_ROOT=%s"
                                    user-emacs-directory))
                      process-environment))
             (status (apply #'process-file
                            "node" nil (current-buffer) nil
                            my/typst-roam-runtime-cli
                            action
                            "--root" root
                            "--runtime" my/typst-roam-runtime-root
                            "--workspace" user-emacs-directory
                            args)))
        (if (zerop status)
            (condition-case nil
                (json-parse-buffer :object-type 'hash-table
                                   :array-type 'list)
              (error nil))
          (message "Aaronnote roam runtime failed: %s"
                   (string-trim (buffer-string)))
          nil)))))

(defun my/typst-roam--runtime-index ()
  "Return cached Aaronnote runtime index payload, or nil."
  (let ((key (list (file-truename (my/typst-roam-root))
                   (file-truename
                    (expand-file-name my/typst-roam-runtime-root)))))
    (if (and my/typst-roam--runtime-index-cache
             (equal key my/typst-roam--runtime-index-cache-key))
        my/typst-roam--runtime-index-cache
      (setq my/typst-roam--runtime-index-cache
            (my/typst-roam--runtime-call "index")
            my/typst-roam--runtime-index-cache-key key)
      my/typst-roam--runtime-index-cache)))

(defun my/typst-roam--runtime-sync (&optional full changed-files)
  "Run Aaronnote roam-db sync asynchronously.
When FULL is non-nil, force a full rebuild.  CHANGED-FILES are passed to the
runtime incremental sync."
  (if (not (my/typst-roam--runtime-available-p))
      (message "Aaronnote roam runtime not found; cache refreshed only")
    (let* ((root (my/typst-roam-root))
           (buf (get-buffer-create "*roam-index*"))
           (args (append
                  (list my/typst-roam-runtime-cli
                        "sync"
                        "--root" root
                        "--runtime" my/typst-roam-runtime-root
                        "--workspace" user-emacs-directory)
                  (when full (list "--full"))
                  (mapcan (lambda (file) (list "--changed" file))
                          (delete-dups
                           (seq-filter #'identity changed-files))))))
      (make-process
       :name "aaronnote-roam-sync"
       :buffer buf
       :command (cons "node" args)
       :noquery t
       :sentinel
       (lambda (_proc event)
         (when (memq (process-status _proc) '(exit signal))
           (my/typst-roam--clear-runtime-cache)
           (message "Aaronnote roam sync: %s" (string-trim event))))))))

(defun my/typst-roam--target-at-point ()
  "Return the raw Markdown roam link target at or near point, or nil."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-start line-end))
           (offset (- (point) line-start))
           result)
      (cl-labels
          ((inside-p (beg end)
             (and (<= beg offset) (<= offset end)))
           (clean-href (href)
             (let ((value (string-trim (or href ""))))
               (if (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+[\"']" value)
                   (match-string 1 value)
                 value)))
           (record (beg end target)
             (when (and (not result)
                        (inside-p beg end)
                        (stringp target)
                        (not (string-empty-p (string-trim target))))
               (setq result (clean-href target)))))
        (let ((pos 0))
          (while (string-match "\\(!?\\)\\[[^]\n]*\\](\\([^)\n]+\\))" line pos)
            (unless (equal (match-string 1 line) "!")
              (record (match-beginning 0) (match-end 0)
                      (match-string 2 line)))
            (setq pos (1+ (match-beginning 0)))))
        (let ((pos 0))
          (while (string-match "\\[\\[\\([^]\n]+\\)\\]\\]" line pos)
            (record (match-beginning 0) (match-end 0)
                    (concat "roam://"
                            (my/typst-roam--encode-ref
                             (string-trim (match-string 1 line)))))
            (setq pos (1+ (match-beginning 0)))))
        (let ((pos 0))
          (while (string-match "\\_<roam://[^][<>()[:space:]]+" line pos)
            (record (match-beginning 0) (match-end 0)
                    (match-string 0 line))
            (setq pos (1+ (match-beginning 0)))))
        ;; Keep the old Typst helper readable during migration.
        (let ((pos 0))
          (while (string-match "#note-link(\"\\([^\"]+\\)\"" line pos)
            (record (match-beginning 0) (match-end 0)
                    (match-string 1 line))
            (setq pos (1+ (match-beginning 0))))))
      result)))

(defun my/typst-roam--decode-ref (ref)
  "Percent-decode note REF, returning REF unchanged on malformed input."
  (condition-case nil
      (url-unhex-string (or ref ""))
    (error (or ref ""))))

(defun my/typst-roam--encode-ref (ref)
  "Percent-encode REF for use in Aaronnote roam URLs."
  (url-hexify-string (or ref "")))

(defun my/typst-roam--split-target (target)
  "Split Aaronnote TARGET into note ref plus optional tag or DOM target.
Canonical targets look like `roam://note-id', `roam://note-id#tag', and
`roam://note-id@dom-target'.  Path-like refs are accepted as input and later
resolved using the same note lookup path."
  (when (and (stringp target) (not (string-empty-p target)))
    (let* ((raw (string-trim target))
           (body (replace-regexp-in-string "\\`roam://" "" raw t t))
           (body (replace-regexp-in-string "\\`file://" "" body t t))
           (body (or (car (split-string body "[?&]" t)) ""))
           ref tag dom)
      (cond
       ((string-match "\\`\\(.*?\\)#\\([^#]*\\)\\'" body)
        (setq ref (match-string 1 body)
              tag (my/typst-roam--decode-ref (match-string 2 body))))
       ((string-match "\\`\\(.*?\\)@\\([^#]*\\)\\'" body)
        (setq ref (match-string 1 body)
              dom (my/typst-roam--decode-ref (match-string 2 body))))
       (t
        (setq ref body)))
      (list :raw raw
            :ref (string-trim
                  (replace-regexp-in-string
                   "\\`/+" ""
                   (my/typst-roam--decode-ref (or ref ""))))
            :tag (and tag (not (string-empty-p tag)) tag)
            :dom (and dom (not (string-empty-p dom)) dom)))))

(defun my/typst-roam--parse-target (target)
  "Parse note-link TARGET into Aaronnote-compatible target metadata."
  (when-let* ((parts (my/typst-roam--split-target target)))
    (let* ((ref (plist-get parts :ref))
           (resolved (my/typst-roam--resolve-note ref))
           (id (or (plist-get resolved :id) ref))
           (file (or (plist-get resolved :file)
                     (my/typst-roam--ref-to-file-fallback ref))))
      (append parts
              (list :slug id
                    :note-id id
                    :id (plist-get parts :tag)
                    :file file
                    :key (plist-get resolved :key)
                    :note (plist-get resolved :note))))))

(defun my/typst-roam--slug-at-point ()
  "Return the note-link slug at or near point, or nil."
  (plist-get (my/typst-roam--parse-target (my/typst-roam--target-at-point))
             :slug))

(defun my/typst-roam--all-files ()
  "Return all Markdown roam note files, excluding generated/private dirs."
  (seq-filter
   (lambda (file)
     (let ((rel (file-relative-name file (my/typst-roam-root))))
       (not (string-match-p
             "\\`\\(?:\\.git/\\|\\.lean/\\|_typst/\\|node_modules/\\)"
             rel))))
   (directory-files-recursively
    (my/typst-roam-root) "\\.\\(?:md\\|markdown\\)$")))

(defun my/typst-roam--file-to-slug (file)
  "Convert FILE path to a roam slug, relative to root and without extension."
  (my/typst-roam--path-without-note-extension
   (file-relative-name file (my/typst-roam-root))))

(defun my/typst-roam--file-to-note-id (file)
  "Return the canonical note id for FILE, falling back to its path slug."
  (let* ((slug (my/typst-roam--file-to-slug file))
         (resolved (my/typst-roam--resolve-note slug)))
    (or (plist-get resolved :id) slug)))

(defun my/typst-roam--ref-has-extension-p (ref)
  "Return non-nil when REF already names a note file extension."
  (string-match-p "\\.\\(?:typ\\|md\\|markdown\\)\\'" (or ref "")))

(defun my/typst-roam--path-without-note-extension (path)
  "Remove a note file extension from PATH."
  (replace-regexp-in-string "\\.\\(?:typ\\|md\\|markdown\\)\\'" "" (or path "")))

(defun my/typst-roam--strip-vault-prefix (ref)
  "Remove Aaronnote's exported `roam/' prefix from path REF."
  (let ((clean (replace-regexp-in-string "\\`/+" "" (or ref ""))))
    (if (string-prefix-p "roam/" clean)
        (substring clean 5)
      clean)))

(defun my/typst-roam--ref-to-file-fallback (ref)
  "Return the best filesystem fallback for unresolved note REF."
  (let* ((clean (my/typst-roam--strip-vault-prefix
                 (string-trim (or ref ""))))
         (root (my/typst-roam-root))
         (path (if (file-name-absolute-p clean)
                   clean
                 (expand-file-name clean root))))
    (cond
     ((and (not (string-empty-p clean))
           (file-exists-p path))
      path)
     ((and (not (string-empty-p clean))
           (not (my/typst-roam--ref-has-extension-p clean))
           (file-exists-p (concat path ".md")))
      (concat path ".md"))
     ((and (not (string-empty-p clean))
           (not (my/typst-roam--ref-has-extension-p clean))
           (file-exists-p (concat path ".markdown")))
      (concat path ".markdown"))
     ((my/typst-roam--ref-has-extension-p clean)
      path)
     (t
      (concat path ".md")))))

(defun my/typst-roam--slug-to-file (slug)
  "Convert SLUG, id, or path-like ref to an absolute note path."
  (or (plist-get (my/typst-roam--resolve-note slug) :file)
      (my/typst-roam--ref-to-file-fallback slug)))

(defun my/typst-roam--slugify-title (title)
  "Return an Aaronnote-style slug for TITLE."
  (let ((slug (downcase
               (replace-regexp-in-string
                "-+" "-"
                (replace-regexp-in-string
                 "\\`-\\|-\\'" ""
                 (replace-regexp-in-string
                  "[^[:alnum:]_]+" "-"
                  (string-trim title)))))))
    (if (string-empty-p slug) "untitled" slug)))

(defun my/typst-roam--timestamp-id ()
  "Return an Aaronnote-style timestamp id."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun my/typst-roam--open-slug (slug &optional no-recent)
  "Open roam note SLUG/id/path and record it in recent notes unless NO-RECENT."
  (let* ((resolved (my/typst-roam--resolve-note slug))
         (note-id (or (plist-get resolved :id) slug))
         (file (or (plist-get resolved :file)
                   (my/typst-roam--ref-to-file-fallback slug))))
    (unless (file-exists-p file)
      (user-error "Note not found: %s" slug))
    (unless no-recent
      (my/typst-roam--touch-recent note-id))
    (find-file file)))

(defun my/typst-roam--touch-recent (slug)
  "Move SLUG to the front of the recent list."
  (when (and (stringp slug) (not (string-empty-p slug)))
    (setq my/typst-roam--recent
          (seq-take (cons slug (delete slug my/typst-roam--recent))
                    my/typst-roam-recent-limit))))

(defun my/typst-roam--note-title (slug)
  "Return display title for SLUG."
  (or (when-let* ((note (my/typst-roam--db-note slug)))
        (gethash "title" note))
      (plist-get (my/typst-roam--resolve-note slug) :title)
      (file-name-nondirectory slug)))

(defun my/typst-roam--note-tags (slug)
  "Return tags for SLUG."
  (when-let* ((note (my/typst-roam--db-note slug)))
    (my/typst-roam--note-list-field note "tags")))

(defun my/typst-roam--note-links (slug)
  "Return normalized outgoing link slugs for SLUG."
  (when-let* ((note (my/typst-roam--db-note slug)))
    (delete-dups
     (seq-filter #'identity
                 (mapcar #'my/typst-roam--target-slug
                         (or (my/typst-roam--note-list-field note "links")
                             (my/typst-roam--note-list-field note "refs")))))))

(defun my/typst-roam--note-summary (slug)
  "Return a compact text summary for SLUG."
  (or (when-let* ((note (my/typst-roam--db-note slug)))
        (my/typst-roam--note-field note "summary"))
      (let ((file (my/typst-roam--slug-to-file slug)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 20000)
            (or (my/typst-roam--extract-summary-block)
                (let (parts in-meta)
                  (goto-char (point-min))
                  (while (and (not (eobp))
                              (< (length (string-join (nreverse parts) " "))
                                 220))
                    (let ((line (string-trim
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
                      (cond
                       ((string-match-p "\\`#\\+begin meta\\b" line)
                        (setq in-meta t))
                       ((string-match-p "\\`#\\+end meta\\b" line)
                        (setq in-meta nil))
                       ((or in-meta
                            (string-empty-p line)
                            (string-match-p "\\`#\\+\\(?:begin\\|end\\)\\b" line)
                            (string-match-p "\\`---\\'" line)
                            (string-match-p "\\`#+\\s-+" line))
                        nil)
                       (t
                        (push
                         (replace-regexp-in-string
                          "[#*_`$()[\\]{}]" " "
                          (replace-regexp-in-string
                           "\\[\\([^]\n]+\\)\\]([^)\n]+)" "\\1" line))
                         parts))))
                    (forward-line 1))
                  (truncate-string-to-width
                   (string-join (nreverse parts) " ") 220 nil nil
                   "..."))))))))

(defun my/typst-roam--all-note-summaries ()
  "Return note summary plists for all notes."
  (mapcar (lambda (record)
            (let* ((id (plist-get record :id))
                   (note (plist-get record :note)))
              (list :slug id
                    :title (or (plist-get record :title)
                               (my/typst-roam--note-title id))
                    :path (or (my/typst-roam--note-field note "path")
                              (my/typst-roam--note-field note "link"))
                    :aliases (my/typst-roam--note-list-field note "aliases")
                    :tags (my/typst-roam--note-tags id)
                    :links (my/typst-roam--note-links id)
                    :backlinks (my/typst-roam--db-backlinks-to id)
                    :summary (my/typst-roam--note-summary id))))
          (sort (my/typst-roam--note-records)
                (lambda (a b)
                  (string< (plist-get a :id) (plist-get b :id))))))

(defun my/typst-roam--candidate-haystack (entry)
  "Return searchable text for note summary ENTRY."
  (string-join
   (delq nil
         (list (plist-get entry :slug)
               (plist-get entry :title)
               (plist-get entry :path)
               (plist-get entry :summary)
               (string-join (or (plist-get entry :aliases) nil) " ")
               (string-join (or (plist-get entry :tags) nil) " ")))
   " "))

(defun my/typst-roam--read-note (prompt &optional entries)
  "Read a note slug with PROMPT from ENTRIES or all summaries."
  (let* ((items (or entries (my/typst-roam--all-note-summaries)))
         (table (mapcar (lambda (entry)
                          (cons (plist-get entry :slug) entry))
                        items))
         (slug (completing-read
                prompt
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function
                         . (lambda (candidate)
                             (when-let* ((entry (cdr (assoc candidate table))))
                               (let ((tags (plist-get entry :tags))
                                     (backlinks (length (or (plist-get entry :backlinks) nil))))
                                 (concat
                                  "  "
                                  (or (plist-get entry :title) "")
                                  (when tags
                                    (concat "  #" (string-join tags " #")))
                                  (when (> backlinks 0)
                                    (format " ←%d" backlinks))))))))
                    (complete-with-action action table string pred)))
                nil t)))
    slug))

(defun my/typst-roam--read-note-id (prompt)
  "Read an Aaronnote note id with PROMPT."
  (let* ((records (my/typst-roam--note-records))
         (candidates (mapcar (lambda (record)
                               (plist-get record :id))
                             records))
         (table (mapcar (lambda (record)
                          (cons (plist-get record :id) record))
                        records)))
    (completing-read
     prompt
     (lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata
             (annotation-function
              . (lambda (candidate)
                  (when-let* ((record (cdr (assoc candidate table))))
                    (let ((note (plist-get record :note)))
                      (concat
                       "  "
                       (or (plist-get record :title) "")
                       (when-let* ((path (or (my/typst-roam--note-field note "path")
                                             (my/typst-roam--note-field note "link"))))
                         (concat "  " path))
                       (when-let* ((tags (my/typst-roam--note-list-field note "tags")))
                         (concat "  #" (string-join tags " #")))))))))
         (complete-with-action action candidates string pred)))
     nil t)))

(defun my/typst-roam--roam-href (note-id &optional kind target)
  "Return canonical Aaronnote roam href for NOTE-ID and optional TARGET."
  (concat "roam://"
          (my/typst-roam--encode-ref note-id)
          (pcase kind
            ('tag (concat "#" (my/typst-roam--encode-ref target)))
            ('dom (concat "@" (mapconcat #'my/typst-roam--encode-ref
                                          (my/typst-roam--dom-target-segments target)
                                          "@")))
            (_ ""))))

(defun my/typst-roam--heading-labels (&optional file)
  "Return Markdown heading ids/labels in FILE or the current buffer.
Each entry is a plist with :id, :text, and :pos."
  (let ((text (if (and file (file-exists-p file))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))
                (if file
                    ""
                  (buffer-substring-no-properties (point-min) (point-max)))))
        labels)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#+[ \t]+\\(.+?\\)\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$"
              nil t)
        (let ((heading (string-trim
                        (replace-regexp-in-string
                         "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
                         (match-string 1))))
              (id (match-string 2))
              (pos (or (match-beginning 2) (match-beginning 0))))
          (when id
            (push (list :id id :text heading :pos pos) labels))))
      (goto-char (point-min))
      (while (re-search-forward "{#\\([[:alnum:]_:-]+\\)}" nil t)
        (let ((id (match-string 1)))
          (unless (seq-find (lambda (entry)
                              (equal (plist-get entry :id) id))
                            labels)
            (push (list :id id :text id :pos (match-beginning 0)) labels)))))
    (nreverse labels)))

(defun my/typst-roam--goto-tag-id (id)
  "Jump to Markdown heading/tag ID in the current buffer."
  (goto-char (point-min))
  (cond
   ((re-search-forward
     (format "{#%s}" (regexp-quote id))
     nil t)
    (goto-char (match-beginning 0)))
   ;; Legacy Typst labels remain readable while old notes are being converted.
   ((progn
      (goto-char (point-min))
      (re-search-forward
       (format "<%s>" (regexp-quote id))
       nil t))
    (goto-char (match-beginning 0)))
   ((when-let* ((target (my/typst-roam--find-dom-target id)))
      (my/typst-roam--goto-pos (plist-get target :pos))
      t))
   (t
    (user-error "Tag id not found: %s" id)))
  (recenter-top-bottom))

(defun my/typst-roam--heading-items (&optional file)
  "Return heading plists for FILE or the current buffer."
  (let ((text (if (and file (file-exists-p file))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))
                (if file
                    ""
                  (buffer-substring-no-properties (point-min) (point-max)))))
        items)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*\\(#\\{1,6\\}\\)[ \t]+\\(.+?\\)\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$"
              nil t)
        (push (list :level (length (match-string 1))
                    :text (string-trim
                           (replace-regexp-in-string
                            "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
                            (match-string 2)))
                    :id (match-string 3)
                    :pos (match-beginning 0))
              items)))
    (nreverse items)))

(defun my/typst-roam--goto-pos (pos)
  "Move to POS, treating nil or synthetic zero positions as file start."
  (goto-char (if (and (integerp pos) (>= pos (point-min)))
                 pos
               (point-min))))

(defun my/typst-roam--normalize-dom-target (value)
  "Normalize Aaronnote DOM target VALUE for matching."
  (string-trim
   (replace-regexp-in-string
    "\\s-+" " "
    (replace-regexp-in-string
     "[][\r\n]" " "
     (string-remove-prefix "@" (my/typst-roam--decode-ref (or value "")))))))

(defun my/typst-roam--slug-dom-target (value)
  "Return Aaronnote's DOM target slug for VALUE."
  (let ((clean (downcase
                (replace-regexp-in-string
                 "[`*_~()[\\]{}#+.!<>:;,'\"@]" " "
                 (my/typst-roam--normalize-dom-target value)))))
    (replace-regexp-in-string
     "\\s-+" "-"
     (string-trim clean))))

(defun my/typst-roam--dom-target-segments (value)
  "Return normalized DOM target path segments from VALUE."
  (seq-filter
   (lambda (segment) (not (string-empty-p segment)))
   (mapcar #'my/typst-roam--slug-dom-target
           (split-string (string-remove-prefix "@" (or value "")) "@"))))

(defun my/typst-roam--dom-targets (&optional file note-id)
  "Return Aaronnote-style DOM/TOC targets for FILE or current buffer."
  (let ((items (my/typst-roam--heading-items file))
        (stack nil)
        (label-stack nil)
        targets)
    (when-let* ((note-id)
                (title (plist-get (my/typst-roam--resolve-note note-id) :title)))
      (let ((label (my/typst-roam--normalize-dom-target title))
            (slug (my/typst-roam--slug-dom-target title)))
        (when (and (not (string-empty-p label))
                   (not (string-empty-p slug)))
          (push (list :slug slug
                      :label label
                      :path (list slug)
                      :label-path (list label)
                      :level 1
                      :pos 0
                      :synthetic t
                      :note-id note-id)
                targets))))
    (dolist (item items)
      (let* ((level (max 1 (plist-get item :level)))
             (label (my/typst-roam--normalize-dom-target
                     (plist-get item :text)))
             (slug (my/typst-roam--slug-dom-target label)))
        (when (and (not (string-empty-p label))
                   (not (string-empty-p slug)))
          (setq stack (seq-take stack (1- level))
                label-stack (seq-take label-stack (1- level)))
          (setq stack (append stack (list slug))
                label-stack (append label-stack (list label)))
          (push (list :slug slug
                      :label label
                      :path stack
                      :label-path label-stack
                      :level level
                      :pos (plist-get item :pos)
                      :note-id note-id)
                targets))))
    (nreverse targets)))

(defun my/typst-roam--dom-target-path-label (target)
  "Return a readable label path for TARGET."
  (string-join (plist-get target :label-path) " / "))

(defun my/typst-roam--target-path-matches-p (actual wanted &optional allow-suffix)
  "Return non-nil when ACTUAL target path matches WANTED."
  (let ((actual (mapcar #'my/typst-roam--slug-dom-target actual))
        (wanted (mapcar #'my/typst-roam--slug-dom-target wanted)))
    (cond
     ((or (null actual) (null wanted)) nil)
     ((equal actual wanted) t)
     ((and allow-suffix
           (>= (length actual) (length wanted)))
      (equal (last actual (length wanted)) wanted)))))

(defun my/typst-roam--find-dom-target (dom &optional file note-id)
  "Find DOM target DOM in FILE or current buffer."
  (let* ((wanted (my/typst-roam--dom-target-segments dom))
         (targets (my/typst-roam--dom-targets file note-id)))
    (cond
     ((null wanted) nil)
     ((> (length wanted) 1)
      (or (seq-find (lambda (target)
                      (my/typst-roam--target-path-matches-p
                       (plist-get target :path) wanted))
                    targets)
          (seq-find (lambda (target)
                      (my/typst-roam--target-path-matches-p
                       (plist-get target :path) wanted t))
                    targets)))
     (t
      (let* ((wanted-segment (car wanted))
             (wanted-label (my/typst-roam--normalize-dom-target dom)))
        (seq-find
         (lambda (target)
           (or (equal (plist-get target :slug) wanted-segment)
               (equal (downcase (plist-get target :label))
                      (downcase wanted-label))))
         targets))))))

(defun my/typst-roam--goto-dom-target (dom)
  "Jump to Aaronnote DOM/TOC target DOM in the current buffer."
  (let* ((target (my/typst-roam--find-dom-target dom))
         (pos (and target (plist-get target :pos))))
    (unless pos
      (user-error "DOM target not found: %s" dom))
    (my/typst-roam--goto-pos pos)
    (recenter-top-bottom)))

(defun my/typst-roam--read-dom-target (note-id)
  "Read an Aaronnote DOM/TOC target for NOTE-ID."
  (let* ((record (my/typst-roam--resolve-note note-id))
         (file (plist-get record :file))
         (targets (my/typst-roam--dom-targets file note-id))
         (table (mapcar (lambda (target)
                          (cons (string-join (plist-get target :path) "@")
                                target))
                        targets))
         (choice (completing-read
                  "DOM/TOC target: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata
                          (annotation-function
                           . (lambda (candidate)
                               (when-let* ((target (cdr (assoc candidate table))))
                                 (concat "  "
                                         (my/typst-roam--dom-target-path-label
                                          target))))))
                      (complete-with-action action table string pred)))
                  nil t)))
    (cdr (assoc choice table))))

(defun my/typst-roam--show-toc (&optional file title)
  "Show a heading TOC for FILE or the current buffer."
  (let* ((items (my/typst-roam--heading-items file))
         (buf (get-buffer-create "*roam-toc*"))
         (target-file file))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "TOC: %s\n\n" (or title (or file (buffer-name)))))
        (if (null items)
            (insert "(no headings)\n")
          (dolist (item items)
            (let ((pos (plist-get item :pos))
                  (label (format "%s%s\n"
                                 (make-string (* 2 (1- (plist-get item :level))) ?\s)
                                 (plist-get item :text))))
              (insert-text-button
               label
               'action (lambda (_)
                         (when target-file
                           (find-file target-file))
                         (my/typst-roam--goto-pos pos)
                         (recenter-top-bottom))
               'follow-link t))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-follow-link ()
  "Jump to the note or source region referenced at point.
Targets may use Aaronnote roam syntax:
  roam://note-id
  roam://note-id#tag
  roam://note-id@dom-target
Path-like refs are accepted and resolved to canonical note ids."
  (interactive)
  (if (and (fboundp 'my/note-code-at-point)
           (my/note-code-at-point))
      (my/note-code-open-at-point)
    (if-let* ((target (my/typst-roam--target-at-point))
              (parsed (my/typst-roam--parse-target target))
              (note-id (plist-get parsed :slug))
              (file (plist-get parsed :file)))
        (let ((ref (plist-get parsed :ref)))
          (if (file-exists-p file)
              (progn
                (my/typst-roam--touch-recent note-id)
                (find-file file)
                (cond
                 ((plist-get parsed :id)
                  (my/typst-roam--goto-tag-id (plist-get parsed :id)))
                 ((plist-get parsed :dom)
                  (my/typst-roam--goto-dom-target (plist-get parsed :dom)))))
            (when (yes-or-no-p (format "Note '%s' not found. Create it? " ref))
              (my/typst-roam-new-note ref))))
      (user-error "No Markdown roam link or #note-code found at point"))))

(defun my/typst-roam-find-note ()
  "Find a roam note by Aaronnote id/path/title with completion."
  (interactive)
  (my/typst-roam--open-slug
   (my/typst-roam--read-note-id "Roam note: ")))

(defun my/typst-roam-insert-link ()
  "Open the interactive selector and insert a Markdown roam link."
  (interactive)
  (my/typst-roam-select-link))

(defun my/typst-roam-new-note (&optional slug title tags)
  "Create a new roam note, prompting for SLUG, TITLE, and TAGS."
  (interactive)
  (let* ((slug (or slug (read-string "Slug (e.g. math/my-note): ")))
         (title (or title
                    (read-string "Title: "
                                 (capitalize (replace-regexp-in-string
                                              "[-/]" " "
                                              (file-name-nondirectory slug))))))
         (tags (or tags (read-string "Tags (comma-separated, or blank): ")))
         (file (my/typst-roam--slug-to-file slug))
         (rel (file-relative-name file (my/typst-roam-root)))
         (tag-str (mapconcat #'string-trim (split-string tags "," t) ", ")))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format "\
#+begin meta
id: %s
title: %s
date: %s
kind: default
tags: %s
refs:
source: roam/%s
#+end meta

#+begin summary
#+end summary

# %s

"
                      slug title (format-time-string "%Y-%m-%d")
                      tag-str rel title)))))

(defun my/typst-roam-new-node (&optional title directory)
  "Create a new timestamped Markdown roam node from TITLE in DIRECTORY."
  (interactive)
  (let* ((title (or title (read-string "Node title: ")))
         (directory (or directory (read-string "Directory [.]: " nil nil ".")))
         (id (format "%s-%s"
                     (my/typst-roam--timestamp-id)
                     (my/typst-roam--slugify-title title)))
         (slug (if (string= directory ".")
                   id
                 (concat (string-remove-suffix "/" directory) "/" id))))
    (my/typst-roam-new-note slug title "")))

;; ── Roam DB ──────────────────────────────────────────────────────────────────

(defvar my/typst-roam--db-cache nil)
(defvar my/typst-roam--db-path-cache nil)
(defvar my/typst-roam--db-mtime nil)
(defvar my/typst-roam--scan-cache nil)

(defun my/typst-roam--db-path ()
  "Return path to an optional Markdown roam-db.json for the current vault."
  (let* ((root (my/typst-roam-root))
         (candidates (mapcar
                      (lambda (rel) (expand-file-name rel root))
                      '("roam-db.json"
                        ".aaronnote/roam-db.json"
                        ".aaronnote/index.json"
                        ".roam/roam-db.json"
                        "_roam/roam-db.json"
                        "_typst/roam-db.json"))))
    (or (seq-find #'file-exists-p candidates)
        (car candidates))))

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

(defun my/typst-roam--db-notes ()
  "Return the DB notes hash table, or nil."
  (when-let* ((db (my/typst-roam--db)))
    (gethash "notes" db)))

(defun my/typst-roam--note-field (note key)
  "Return string field KEY from NOTE."
  (when (hash-table-p note)
    (let ((value (gethash key note)))
      (when (and (stringp value) (not (string-empty-p value)))
        value))))

(defun my/typst-roam--note-list-field (note key)
  "Return list field KEY from NOTE."
  (let ((value (and (hash-table-p note) (gethash key note))))
    (cond
     ((listp value) value)
     ((vectorp value) (append value nil))
     ((and (stringp value) (not (string-empty-p value))) (list value)))))

(defun my/typst-roam--split-list-value (value)
  "Split comma/space separated Markdown meta VALUE into a clean string list."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((vectorp value) (append value nil))
   ((stringp value)
    (seq-filter
     (lambda (item) (not (string-empty-p item)))
     (mapcar (lambda (item)
               (let ((clean (string-trim item)))
                 (setq clean (string-remove-prefix "[" clean)
                       clean (string-remove-suffix "]" clean)
                       clean (string-remove-prefix "\"" clean)
                       clean (string-remove-suffix "\"" clean))
                 (string-trim clean)))
             (split-string value "[,\n]" t))))))

(defun my/typst-roam--put-note-field (note key value)
  "Set NOTE KEY to VALUE when VALUE is present."
  (when (and value
             (not (and (stringp value) (string-empty-p (string-trim value)))))
    (puthash key value note)))

(defun my/typst-roam--parse-meta-line (note line)
  "Parse one KEY: VALUE metadata LINE into NOTE."
  (when (string-match "\\`\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (let* ((key (downcase (string-trim (match-string 1 line))))
           (value (string-trim (match-string 2 line))))
      (pcase key
        ((or "tags" "aliases" "refs" "links" "backlinks" "inlinetags")
         (my/typst-roam--put-note-field
          note key (my/typst-roam--split-list-value value)))
        (_
         (my/typst-roam--put-note-field note key value))))))

(defun my/typst-roam--read-org-meta-block (note)
  "Read an Aaronnote `#+begin meta' block at point into NOTE."
  (when (looking-at-p "\\s-*#\\+begin meta\\b")
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at-p "\\s-*#\\+end meta\\b")))
      (my/typst-roam--parse-meta-line
       note
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (forward-line 1))
    t))

(defun my/typst-roam--read-yaml-frontmatter (note)
  "Read simple YAML frontmatter at point into NOTE."
  (when (looking-at-p "\\s-*---\\s-*$")
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at-p "\\s-*---\\s-*$")))
      (my/typst-roam--parse-meta-line
       note
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (forward-line 1))
    t))

(defun my/typst-roam--extract-summary-block ()
  "Return the first `#+begin summary' block text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin summary\\b.*$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^#\\+end summary\\b" nil t)
          (string-trim
           (buffer-substring-no-properties start (match-beginning 0))))))))

(defun my/typst-roam--internal-target-p (target)
  "Return non-nil when Markdown link TARGET is a roam note reference."
  (let ((clean (string-trim (or target ""))))
    (or (string-match-p "\\`roam://" clean)
        (string-match-p "\\.\\(?:md\\|markdown\\)\\(?:[#?@].*\\)?\\'" clean)
        (and (not (string-empty-p clean))
             (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" clean))
             (not (string-prefix-p "#" clean))
             (not (string-match-p
                   "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|webp\\|pdf\\)\\(?:[#?].*\\)?\\'"
                   clean))))))

(defun my/typst-roam--extract-links-from-buffer ()
  "Return Markdown roam references from the current buffer."
  (let (links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]\n]+\\)\\]\\]" nil t)
        (push (concat "roam://"
                      (my/typst-roam--encode-ref
                       (string-trim (match-string 1))))
              links))
      (goto-char (point-min))
      (while (re-search-forward "\\(!?\\)\\[[^]\n]*\\](\\([^)\n]+\\))" nil t)
        (unless (equal (match-string 1) "!")
          (let ((href (string-trim (match-string 2))))
            (when (my/typst-roam--internal-target-p href)
              (push href links)))))
      (goto-char (point-min))
      (while (re-search-forward "\\_<roam://[^][<>()[:space:]]+" nil t)
        (push (match-string 0) links)))
    (delete-dups (nreverse links))))

(defun my/typst-roam--first-markdown-heading ()
  "Return the first Markdown heading text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\{1,6\\}[ \t]+\\(.+?\\)\\(?:[ \t]+{#[[:alnum:]_:-]+}\\)?[ \t]*$" nil t)
      (string-trim
       (replace-regexp-in-string
        "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
        (match-string 1))))))

(defun my/typst-roam--scan-note-file (file)
  "Return a note hash table by scanning Markdown FILE."
  (let* ((root (my/typst-roam-root))
         (rel (file-relative-name file root))
         (note (make-hash-table :test 'equal)))
    (puthash "file" file note)
    (puthash "path" rel note)
    (puthash "link" rel note)
    (puthash "source" (concat "roam/" rel) note)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cond
       ((looking-at-p "\\s-*#\\+begin meta\\b")
        (my/typst-roam--read-org-meta-block note))
       ((looking-at-p "\\s-*---\\s-*$")
        (my/typst-roam--read-yaml-frontmatter note)))
      (my/typst-roam--put-note-field
       note "title"
       (or (my/typst-roam--note-field note "title")
           (my/typst-roam--first-markdown-heading)
           (file-name-base file)))
      (my/typst-roam--put-note-field
       note "id"
       (or (my/typst-roam--note-field note "id")
           (my/typst-roam--path-without-note-extension rel)))
      (my/typst-roam--put-note-field
       note "summary"
       (my/typst-roam--extract-summary-block))
      (my/typst-roam--put-note-field
       note "links"
       (append (my/typst-roam--note-list-field note "refs")
               (my/typst-roam--extract-links-from-buffer))))
    note))

(defun my/typst-roam--canonical-note-id (key note)
  "Return Aaronnote's canonical note id for NOTE with DB KEY."
  (or (my/typst-roam--note-field note "id")
      (my/typst-roam--note-field note "key")
      (my/typst-roam--note-field note "source")
      (my/typst-roam--note-field note "path")
      (my/typst-roam--note-field note "link")
      (my/typst-roam--note-field note "file")
      key))

(defun my/typst-roam--note-file-from-fields (key note)
  "Return the best note file path for DB KEY and NOTE."
  (let* ((root (my/typst-roam-root))
         (raw (my/typst-roam--strip-vault-prefix
               (or (my/typst-roam--note-field note "file")
                   (my/typst-roam--note-field note "path")
                   (my/typst-roam--note-field note "link")
                   (my/typst-roam--note-field note "source")
                   key)))
         (path (and raw
                    (if (file-name-absolute-p raw)
                        raw
                      (expand-file-name raw root)))))
    (cond
     ((and path (file-exists-p path)) path)
     ((and path raw (not (my/typst-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".md")))
      (concat path ".md"))
     ((and path raw (not (my/typst-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".markdown")))
      (concat path ".markdown"))
     (path path))))

(defun my/typst-roam--note-search-values (key note)
  "Return Aaronnote-style searchable values for NOTE with DB KEY."
  (let* ((file (my/typst-roam--note-field note "file"))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/typst-roam-root))))
         (values (append
                  (list key
                        (my/typst-roam--canonical-note-id key note)
                        (my/typst-roam--note-field note "id")
                        (my/typst-roam--note-field note "key")
                        (my/typst-roam--note-field note "title")
                        (my/typst-roam--note-field note "path")
                        (my/typst-roam--note-field note "link")
                        (my/typst-roam--note-field note "source")
                        file
                        rel-file
                        (and rel-file
                             (my/typst-roam--path-without-note-extension rel-file))
                        (and rel-file (concat "roam/" rel-file))
                        (and rel-file
                             (concat "roam/"
                                     (my/typst-roam--path-without-note-extension
                                      rel-file))))
                  (my/typst-roam--note-list-field note "aliases")
                  (my/typst-roam--note-list-field note "tags"))))
    (delete-dups
     (seq-filter (lambda (value)
                   (and (stringp value) (not (string-empty-p value))))
                 values))))

(defun my/typst-roam--scanned-note-records ()
  "Return cached note records by scanning Markdown files."
  (or my/typst-roam--scan-cache
      (setq my/typst-roam--scan-cache
            (mapcar (lambda (file)
                      (let* ((note (my/typst-roam--scan-note-file file))
                             (key (my/typst-roam--file-to-slug file))
                             (id (my/typst-roam--canonical-note-id key note)))
                        (list :key key
                              :id id
                              :note note
                              :file file
                              :title (or (my/typst-roam--note-field note "title")
                                         id)
                              :values (my/typst-roam--note-search-values
                                       key note))))
                    (my/typst-roam--all-files)))))

(defun my/typst-roam--runtime-note-records ()
  "Return note records from the vendored Aaronnote runtime."
  (when-let* ((payload (my/typst-roam--runtime-index))
              (notes (gethash "notes" payload)))
    (mapcar (lambda (note)
              (let* ((key (or (my/typst-roam--note-field note "key")
                              (my/typst-roam--note-field note "id")
                              (my/typst-roam--note-field note "path")
                              (my/typst-roam--note-field note "link")))
                     (id (my/typst-roam--canonical-note-id key note)))
                (list :key key
                      :id id
                      :note note
                      :file (my/typst-roam--note-file-from-fields key note)
                      :title (or (my/typst-roam--note-field note "title") id)
                      :values (my/typst-roam--note-search-values key note))))
            notes)))

(defun my/typst-roam--note-records ()
  "Return note records with :key, :id, :note, :file, :title, and :values."
  (or (my/typst-roam--runtime-note-records)
      (if-let* ((notes (my/typst-roam--db-notes)))
          (let (records)
            (maphash
             (lambda (key note)
               (let ((id (my/typst-roam--canonical-note-id key note)))
                 (push (list :key key
                             :id id
                             :note note
                             :file (my/typst-roam--note-file-from-fields key note)
                             :title (or (my/typst-roam--note-field note "title") id)
                             :values (my/typst-roam--note-search-values key note))
                       records)))
             notes)
            (nreverse records))
        (my/typst-roam--scanned-note-records))))

(defun my/typst-roam--target-note-ref (target)
  "Return the note ref portion of TARGET."
  (plist-get (my/typst-roam--split-target target) :ref))

(defun my/typst-roam--resolve-note (ref)
  "Resolve REF to an Aaronnote note record plist.
Exact id/key/path/title/alias/tag matches win first; substring matches are
accepted as a fallback, matching Aaronnote search behavior."
  (let* ((clean (or (my/typst-roam--target-note-ref ref) ref))
         (clean (string-trim (or clean "")))
         (query (downcase clean))
         (records (my/typst-roam--note-records)))
    (or
     (seq-find
      (lambda (record)
        (member query
                (mapcar #'downcase (plist-get record :values))))
      records)
     (and (not (string-empty-p query))
          (seq-find
           (lambda (record)
             (seq-some
              (lambda (value)
                (string-match-p (regexp-quote query) (downcase value)))
              (plist-get record :values)))
           records)))))

(defun my/typst-roam--db-note (slug)
  "Return the DB hash-table for SLUG/id/path, or nil."
  (plist-get (my/typst-roam--resolve-note slug) :note))

(defun my/typst-roam--target-slug (target)
  "Return normalized canonical note id from a note-link TARGET."
  (plist-get (my/typst-roam--parse-target target) :slug))

(defun my/typst-roam--db-backlinks-to (slug)
  "Return DB backlinks to SLUG/id, normalizing Aaronnote targets."
  (when-let* ((target-id (or (plist-get (my/typst-roam--resolve-note slug) :id)
                             slug)))
    (or (when-let* ((note (my/typst-roam--db-note target-id)))
          (my/typst-roam--note-list-field note "backlinks"))
        (let (backlinks)
          (dolist (record (my/typst-roam--note-records))
            (let* ((note (plist-get record :note))
                   (source (plist-get record :key))
                   (links (or (my/typst-roam--note-list-field note "links")
                              (my/typst-roam--note-list-field note "refs"))))
              (when (member target-id
                            (mapcar #'my/typst-roam--target-slug links))
                (push (my/typst-roam--canonical-note-id source note) backlinks))))
          (delete-dups (nreverse backlinks))))))

(defun my/typst-roam--current-slug ()
  "Return the canonical roam id for the current buffer, or nil."
  (when buffer-file-name
    (my/typst-roam--file-to-note-id buffer-file-name)))

;; ── Tag ids and TOC ───────────────────────────────────────────────────────────

(defun my/typst-roam--slugify-tag-id (text)
  "Return a stable Markdown heading id for TEXT."
  (let* ((plain (string-trim
                 (replace-regexp-in-string
                  "[`$#=<>\\[\\]{}()\"'.,;:!?，。；：！？、]" " "
                  text)))
         (slug (downcase
                (replace-regexp-in-string
                 "-+" "-"
                 (replace-regexp-in-string
                  "\\`-\\|-\\'" ""
                  (replace-regexp-in-string "[^[:alnum:]_]+" "-" plain))))))
    (if (string-empty-p slug)
        (substring (secure-hash 'sha1 text) 0 10)
      slug)))

(defun my/typst-roam--tag-id-exists-p (id)
  "Return non-nil when ID already exists in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward (format "{#%s}" (regexp-quote id)) nil t)
        (progn
          (goto-char (point-min))
          (re-search-forward (format "<%s>" (regexp-quote id)) nil t)))))

(defun my/typst-roam--unique-tag-id (base)
  "Return BASE or BASE-N so it is unique in the current buffer."
  (let ((candidate base)
        (n 2))
    (while (my/typst-roam--tag-id-exists-p candidate)
      (setq candidate (format "%s-%d" base n)
            n (1+ n)))
    candidate))

(defun my/typst-roam-generate-tag-id (&optional text)
  "Generate a unique Markdown heading id from TEXT or context."
  (interactive)
  (let* ((source (or text
                     (when (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))
                     (save-excursion
                       (beginning-of-line)
                       (if (looking-at
                            "^[ \t]*#\\{1,6\\}[ \t]+\\(.+?\\)\\(?:[ \t]+{#[[:alnum:]_:-]+}\\)?[ \t]*$")
                           (replace-regexp-in-string
                            "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
                            (match-string 1))
                         (thing-at-point 'line t)))
                     "tag"))
         (id (my/typst-roam--unique-tag-id
              (my/typst-roam--slugify-tag-id source))))
    (when (called-interactively-p 'interactive)
      (kill-new id)
      (message "Tag id copied: %s" id))
    id))

(defun my/typst-roam-insert-tag-id (&optional id)
  "Insert or append Markdown heading ID at point.
On a heading line, append `{#id}` unless an id already exists."
  (interactive)
  (let ((id (or id
                (read-string "Tag id: "
                             (my/typst-roam-generate-tag-id)))))
    (save-excursion
      (beginning-of-line)
      (if (looking-at
          "^[ \t]*#\\{1,6\\}[ \t]+.+?\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$")
          (if (match-string 1)
              (user-error "Heading already has id: %s" (match-string 1))
            (end-of-line)
            (insert (format " {#%s}" id)))
        (insert (format "{#%s}" id))))))

(defun my/typst-roam-insert-toc-link ()
  "Open the interactive selector and insert a DOM/TOC note-link."
  (interactive)
  (my/typst-roam-select-link 'toc))

(defun my/typst-roam-insert-tag-id-link ()
  "Open the interactive selector and insert a tag-id note-link."
  (interactive)
  (my/typst-roam-select-link 'tag))

;; ── DB commands ───────────────────────────────────────────────────────────────

(defun my/typst-roam-update-db (&optional full)
  "Refresh Markdown roam cache and sync `roam.db' via Aaronnote runtime.
With prefix argument FULL, force a full roam-db rebuild."
  (interactive "P")
  (my/typst-roam--clear-runtime-cache)
  (if (my/typst-roam--runtime-available-p)
      (my/typst-roam--runtime-sync full nil)
    (message "Markdown roam cache refreshed")))

(defun my/typst-roam-backlinks ()
  "Show backlinks for the current note in a dedicated buffer."
  (interactive)
  (let* ((slug (my/typst-roam--current-slug))
         (note (and slug (my/typst-roam--db-note slug)))
         (bls  (or (and slug (my/typst-roam--db-backlinks-to slug))
                   (and note (gethash "backlinks" note))))
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
                   (title   (if bl-note (gethash "title" bl-note) bl)))
              (insert-text-button
               (format "  %-40s %s\n" title bl)
               'action (lambda (_) (my/typst-roam--open-slug bl))
               'follow-link t))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-tags ()
  "Browse notes by tag with completion."
  (interactive)
  (let ((tags-ht (make-hash-table :test 'equal)))
    (dolist (record (my/typst-roam--note-records))
      (dolist (tag (my/typst-roam--note-list-field
                    (plist-get record :note) "tags"))
        (puthash tag
                 (cons (plist-get record :id)
                       (gethash tag tags-ht))
                 tags-ht)))
    (let* ((tags (hash-table-keys tags-ht))
           (tag (completing-read "Tag: " (sort tags #'string<) nil t))
           (slugs (sort (delete-dups (gethash tag tags-ht)) #'string<))
           (slug (completing-read (format "Notes tagged [%s]: " tag)
                                  slugs nil t)))
      (my/typst-roam--open-slug slug))))

(defun my/typst-roam--scan-todos ()
  "Return todo hash tables scanned from Markdown notes."
  (let (todos)
    (dolist (record (my/typst-roam--note-records))
      (let ((file (plist-get record :file)))
        (when (and file (file-exists-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
                (when (or (string-match-p "\\`@@todo\\b" line)
                          (string-match-p "\\`\\(?:[-*+]\\s-+\\)?\\[ \\]" line)
                          (string-match-p "\\_<TODO\\_>" line))
                  (let ((entry (make-hash-table :test 'equal)))
                    (puthash "note" (plist-get record :id) entry)
                    (puthash "title" (plist-get record :title) entry)
                    (puthash "text" line entry)
                    (push entry todos))))
              (forward-line 1))))))
    (nreverse todos)))

(defun my/typst-roam-todos ()
  "List all vault todos in a *roam-todos* buffer."
  (interactive)
  (let* ((runtime (my/typst-roam--runtime-call "todos"))
         (runtime-todos (and runtime (gethash "todos" runtime)))
         (runtime-todos (if (hash-table-p runtime-todos)
                            (gethash "todos" runtime-todos)
                          runtime-todos))
         (db    (my/typst-roam--db))
         (todos (or runtime-todos
                    (and db (gethash "todos" db))
                    (my/typst-roam--scan-todos)))
         (buf   (get-buffer-create "*roam-todos*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Roam TODOs\n\n")
        (if (null todos)
            (insert "(none)\n")
          (dolist (entry todos)
            (let* ((note-slug (or (gethash "note" entry)
                                  (gethash "noteId" entry)
                                  (gethash "noteKey" entry)
                                  (gethash "path" entry)))
                   (title     (or (gethash "title" entry)
                                  (gethash "noteTitle" entry)
                                  note-slug))
                   (text      (or (gethash "text" entry)
                                  (gethash "context" entry)
                                  (gethash "source" entry)
                                  ""))
                   (line      (gethash "line" entry)))
              (insert-text-button
               (format "  [%s]  %s\n" title text)
               'action (lambda (_)
                         (my/typst-roam--open-slug note-slug)
                         (when (integerp line)
                           (goto-char (point-min))
                           (forward-line (1- line))))
               'follow-link t))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;; ── Aaronnote-style note tools ────────────────────────────────────────────────

(defun my/typst-roam--insert-note-button (entry &optional prefix)
  "Insert a clickable note button for summary ENTRY with PREFIX."
  (let* ((slug (plist-get entry :slug))
         (title (or (plist-get entry :title) slug))
         (tags (plist-get entry :tags))
         (summary (plist-get entry :summary)))
    (insert-text-button
     (format "%s%-42s %s\n"
             (or prefix "")
             title
             slug)
     'action (lambda (_) (my/typst-roam--open-slug slug))
     'follow-link t)
    (when tags
      (insert (format "    #%s\n" (string-join tags " #"))))
    (when (and summary (not (string-empty-p summary)))
      (insert (format "    %s\n" summary)))))

(defun my/typst-roam--show-note-list (title entries &optional empty-text)
  "Show TITLE and note ENTRIES in a special buffer."
  (let ((buf (get-buffer-create "*typst-roam-notes*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title "\n\n")
        (if (null entries)
            (insert (or empty-text "(no notes)") "\n")
          (dolist (entry entries)
            (my/typst-roam--insert-note-button entry)
            (insert "\n")))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-search-notes (&optional query)
  "Search notes by path, title, tag, id, and summary."
  (interactive)
  (let* ((query (or query (read-string "Search notes: ")))
         (parts (split-string (downcase query) "\\s-+" t))
         (entries (seq-filter
                   (lambda (entry)
                     (let ((haystack (downcase
                                      (my/typst-roam--candidate-haystack entry))))
                       (seq-every-p
                        (lambda (part) (string-match-p (regexp-quote part) haystack))
                        parts)))
                   (my/typst-roam--all-note-summaries))))
    (if (called-interactively-p 'interactive)
        (if (= (length entries) 1)
            (my/typst-roam--open-slug (plist-get (car entries) :slug))
          (my/typst-roam--show-note-list
           (format "Markdown roam search: %s" query)
           entries
           "(no matching notes)"))
      entries)))

(defun my/typst-roam-recent-notes ()
  "Show recently opened roam notes."
  (interactive)
  (my/typst-roam--show-note-list
   "Recent Markdown roam notes"
   (seq-filter
    #'identity
    (mapcar (lambda (slug)
              (seq-find (lambda (entry)
                          (equal (plist-get entry :slug) slug))
                        (my/typst-roam--all-note-summaries)))
            (seq-filter (lambda (slug)
                          (file-exists-p (my/typst-roam--slug-to-file slug)))
                        my/typst-roam--recent)))
   "(no recent notes)"))

(defun my/typst-roam-related-notes ()
  "Show outgoing links and backlinks for the current note."
  (interactive)
  (let* ((slug (my/typst-roam--current-slug))
         (links (and slug (my/typst-roam--note-links slug)))
         (backlinks (and slug (my/typst-roam--db-backlinks-to slug)))
         (summaries (my/typst-roam--all-note-summaries))
         (by-slug (lambda (target)
                    (seq-find (lambda (entry)
                                (equal (plist-get entry :slug) target))
                              summaries)))
         (buf (get-buffer-create "*typst-roam-related*")))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Related notes: %s\n\n" slug))
        (insert "Links\n\n")
        (if links
            (dolist (target links)
              (when-let* ((entry (funcall by-slug target)))
                (my/typst-roam--insert-note-button entry "  ")))
          (insert "  (none)\n"))
        (insert "\nBacklinks\n\n")
        (if backlinks
            (dolist (target backlinks)
              (when-let* ((entry (funcall by-slug target)))
                (my/typst-roam--insert-note-button entry "  ")))
          (insert "  (none)\n"))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-graph ()
  "Show an Emacs text graph of roam note links."
  (interactive)
  (let* ((entries (my/typst-roam--all-note-summaries))
         (buf (get-buffer-create "*typst-roam-graph*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Markdown roam graph: %d nodes\n\n" (length entries)))
        (dolist (entry entries)
          (let ((slug (plist-get entry :slug))
                (links (plist-get entry :links))
                (backlinks (plist-get entry :backlinks)))
            (insert-text-button
             (format "%s\n" slug)
             'action (lambda (_) (my/typst-roam--open-slug slug))
             'follow-link t)
            (insert (format "  links: %s\n"
                            (if links (string-join links ", ") "(none)")))
            (insert (format "  backlinks: %s\n\n"
                            (if backlinks (string-join backlinks ", ") "(none)")))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun my/typst-roam-management ()
  "Show roam management commands and index status."
  (interactive)
  (let* ((entries (my/typst-roam--all-note-summaries))
         (db (my/typst-roam--db))
         (generated (and db (gethash "generated" db)))
         (buf (get-buffer-create "*typst-roam-management*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Markdown roam management\n\n")
        (insert (format "Root: %s\n" (my/typst-roam-root)))
        (insert (format "Nodes: %d\n" (length entries)))
        (insert (format "DB generated: %s\n\n" (or generated "unknown")))
        (insert-text-button "Sync roam-db"
                            'action (lambda (_) (my/typst-roam-update-db))
                            'follow-link t)
        (insert "\n")
        (insert-text-button "New node"
                            'action (lambda (_) (call-interactively #'my/typst-roam-new-node))
                            'follow-link t)
        (insert "\n")
        (insert-text-button "Search notes"
                            'action (lambda (_) (call-interactively #'my/typst-roam-search-notes))
                            'follow-link t)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;; ── Interactive Markdown roam link selector ──────────────────────────────────

(defvar-local my/typst-roam-select--origin-marker nil)
(defvar-local my/typst-roam-select--current-note-id nil)
(defvar-local my/typst-roam-select--preferred-kind nil)
(defvar-local my/typst-roam-select--view nil)
(defvar-local my/typst-roam-select--path "")
(defvar-local my/typst-roam-select--query nil)
(defvar-local my/typst-roam-select--target-record nil)
(defvar-local my/typst-roam-select--target-basis 'id)
(defvar-local my/typst-roam-select--toc-parent nil)

(defvar my/typst-roam-select-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/typst-roam-select-activate)
    (define-key map (kbd "i") #'my/typst-roam-select-insert-current)
    (define-key map (kbd "/") #'my/typst-roam-select-search)
    (define-key map (kbd "s") #'my/typst-roam-select-search)
    (define-key map (kbd "g") #'my/typst-roam-select-root)
    (define-key map (kbd ".") #'my/typst-roam-select-context)
    (define-key map (kbd "u") #'my/typst-roam-select-up)
    (define-key map (kbd "^") #'my/typst-roam-select-up)
    (define-key map (kbd "r") #'my/typst-roam-select-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my/typst-roam-select-mode'.")

(define-derived-mode my/typst-roam-select-mode special-mode "Roam-Select"
  "Interactive Markdown roam link selector."
  (setq-local truncate-lines t))

(defun my/typst-roam--record-path-ref (record)
  "Return RECORD's path-like link ref."
  (let* ((note (plist-get record :note))
         (file (plist-get record :file))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/typst-roam-root)))))
    (or (my/typst-roam--note-field note "path")
        (my/typst-roam--note-field note "link")
        rel-file
        (plist-get record :key)
        (plist-get record :id))))

(defun my/typst-roam--target-suffix (kind target)
  "Return Markdown roam link suffix for KIND and TARGET."
  (pcase kind
    ('tag (concat "#" (my/typst-roam--encode-ref target)))
    ('dom (concat "@" (mapconcat #'my/typst-roam--encode-ref
                                  (my/typst-roam--dom-target-segments target)
                                  "@")))
    (_ "")))

(defun my/typst-roam--link-target-for-record (record basis &optional kind target)
  "Return Markdown roam link target for RECORD using BASIS, KIND, and TARGET."
  (let ((basis (if (stringp basis) (intern basis) basis)))
    (if (eq basis 'path)
        (concat (my/typst-roam--record-path-ref record)
                (my/typst-roam--target-suffix kind target))
      (my/typst-roam--roam-href (plist-get record :id) kind target))))

(defun my/typst-roam--insert-note-link-target (target text &optional marker)
  "Insert Markdown link TARGET with TEXT at MARKER or point."
  (let ((link (format "[%s](%s)"
                      (replace-regexp-in-string "\\]" "\\\\]" (or text ""))
                      target)))
    (if (and (markerp marker) (marker-buffer marker))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (insert link)))
      (insert link))
    link))

(defun my/typst-roam--tag-targets (record)
  "Return tag target plists for RECORD."
  (let* ((file (plist-get record :file))
         (note (plist-get record :note))
         (labels (and file (my/typst-roam--heading-labels file)))
         (inline-tags (my/typst-roam--note-list-field note "inlineTags"))
         seen targets)
    (dolist (entry labels)
      (let ((id (plist-get entry :id)))
        (when (and id (not (member id seen)))
          (push id seen)
          (push (list :id id
                      :label (or (plist-get entry :text) id))
                targets))))
    (dolist (id inline-tags)
      (when (and id (not (member id seen)))
        (push id seen)
        (push (list :id id :label id) targets)))
    (sort (nreverse targets)
          (lambda (a b)
            (string< (plist-get a :id) (plist-get b :id))))))

(defun my/typst-roam--tag-target-display (target)
  "Return completion display string for tag TARGET."
  (let ((id (plist-get target :id))
        (label (plist-get target :label)))
    (if (and label (not (equal id label)))
        (format "%s  %s" id label)
      id)))

(defun my/typst-roam--read-tag-target (record)
  "Read a tag target for RECORD."
  (let* ((targets (my/typst-roam--tag-targets record))
         (table (mapcar (lambda (target)
                          (cons (my/typst-roam--tag-target-display target)
                                target))
                        targets))
         (choice (if table
                     (completing-read "Tag: " table nil t)
                   (user-error "No tag ids in this note"))))
    (cdr (assoc choice table))))

(defun my/typst-roam-select--toc-targets (record)
  "Return TOC targets for RECORD."
  (let* ((file (plist-get record :file))
         (note-id (plist-get record :id))
         (seen nil)
         targets)
    (dolist (target (my/typst-roam--dom-targets file note-id))
      (let ((key (string-join (plist-get target :path) "@")))
        (when (and (not (plist-get target :synthetic))
                   (not (string-empty-p key))
                   (not (member key seen)))
          (push key seen)
          (push target targets))))
    (nreverse targets)))

(defun my/typst-roam-select--toc-dom (target)
  "Return DOM target string for TOC TARGET."
  (string-join (plist-get target :path) "@"))

(defun my/typst-roam-select--read-basis ()
  "Read target basis for the selected note."
  (intern
   (completing-read "Target ref: "
                    '("id" "path")
                    nil t nil nil "id")))

(defun my/typst-roam-select--read-kind ()
  "Read exact target kind for the selected note."
  (pcase (completing-read "Target kind: "
                          '("note" "tag" "toc")
                          nil t nil nil "note")
    ("tag" 'tag)
    ("toc" 'toc)
    (_ 'note)))

(defun my/typst-roam-select--default-note-text (record)
  "Return default display text for RECORD."
  (or (plist-get record :title)
      (plist-get record :id)))

(defun my/typst-roam-select--finish-target (record basis kind target default-text)
  "Insert final note-link for RECORD, BASIS, KIND, TARGET, and DEFAULT-TEXT."
  (let* ((href (my/typst-roam--link-target-for-record record basis kind target))
         (text (read-string (format "Display text [%s]: " default-text)
                            nil nil default-text)))
    (my/typst-roam--insert-note-link-target
     href text my/typst-roam-select--origin-marker)
    (when-let* (((derived-mode-p 'my/typst-roam-select-mode))
                (window (get-buffer-window (current-buffer))))
      (quit-window t window))))

(defun my/typst-roam-select--choose-record (record)
  "Choose exact target for note RECORD."
  (let* ((basis (my/typst-roam-select--read-basis))
         (kind (or my/typst-roam-select--preferred-kind
                   (my/typst-roam-select--read-kind))))
    (pcase kind
      ('tag
       (let* ((tag (my/typst-roam--read-tag-target record))
              (id (plist-get tag :id))
              (label (or (plist-get tag :label) id)))
         (my/typst-roam-select--finish-target
          record basis 'tag id label)))
      ('toc
       (setq my/typst-roam-select--target-record record
             my/typst-roam-select--target-basis basis
             my/typst-roam-select--toc-parent nil
             my/typst-roam-select--query nil)
       (my/typst-roam-select--render-toc))
      (_
       (my/typst-roam-select--finish-target
        record basis nil nil
        (my/typst-roam-select--default-note-text record))))))

(defun my/typst-roam-select--record-relative-file (record)
  "Return RECORD's relative file under the roam root, or nil."
  (when-let* ((file (plist-get record :file)))
    (let ((rel (file-relative-name file (my/typst-roam-root))))
      (unless (or (string-prefix-p "../" rel)
                  (string-prefix-p "/.." rel)
                  (string-match-p "\\`_typst/" rel))
        rel))))

(defun my/typst-roam-select--directory-items (dir)
  "Return directory and note items immediately inside DIR."
  (let ((dir (if (string-empty-p (or dir "")) "" dir))
        dirs notes seen-dirs)
    (dolist (record (my/typst-roam--note-records))
      (when-let* ((rel (my/typst-roam-select--record-relative-file record)))
        (when (string-prefix-p dir rel)
          (let ((rest (substring rel (length dir))))
            (unless (string-empty-p rest)
              (if (string-match "\\`\\([^/]+\\)/" rest)
                  (let ((name (match-string 1 rest)))
                    (unless (member name seen-dirs)
                      (push name seen-dirs)
                      (push (list :type 'dir
                                  :name name
                                  :path (concat dir name "/"))
                            dirs)))
                (push (list :type 'note :record record)
                      notes)))))))
    (append
     (sort dirs (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name))))
     (sort notes (lambda (a b)
                   (string< (or (plist-get (plist-get a :record) :title) "")
                            (or (plist-get (plist-get b :record) :title) "")))))))

(defun my/typst-roam-select--insert-row (label item &optional face)
  "Insert a selectable row LABEL carrying ITEM."
  (let ((start (point)))
    (insert label "\n")
    (add-text-properties
     start (point)
     `(my/typst-roam-select-item ,item
       mouse-face highlight
       help-echo "RET: open/select, i: insert/select"))
    (when face
      (add-face-text-property start (point) face))))

(defun my/typst-roam-select--note-label (record &optional prefix)
  "Return display label for note RECORD with PREFIX."
  (let* ((title (my/typst-roam-select--default-note-text record))
         (path (my/typst-roam--record-path-ref record))
         (tags (my/typst-roam--note-list-field (plist-get record :note) "tags")))
    (concat (or prefix "")
            (format "%-38s %s" title (plist-get record :id))
            (when path (concat "  " path))
            (when tags (concat "  #" (string-join tags " #"))))))

(defun my/typst-roam-select--render-header (title)
  "Render selector TITLE and help."
  (insert title "\n")
  (insert "RET select/open  i insert/select  / or s search  g root  . current  u up  r refresh  q quit\n\n"))

(defun my/typst-roam-select--render-root (&optional dir)
  "Render roam root tree at DIR."
  (setq my/typst-roam-select--view 'root
        my/typst-roam-select--path (or dir ""))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (my/typst-roam-select--render-header
     (format "Roam root: /%s" my/typst-roam-select--path))
    (let ((items (my/typst-roam-select--directory-items
                  my/typst-roam-select--path)))
      (if items
          (dolist (item items)
            (pcase (plist-get item :type)
              ('dir
               (my/typst-roam-select--insert-row
                (format "[dir]  %s/" (plist-get item :name))
                item 'font-lock-keyword-face))
              ('note
               (my/typst-roam-select--insert-row
                (my/typst-roam-select--note-label
                 (plist-get item :record) "[note] ")
                item))))
        (insert "(empty)\n")))
    (goto-char (point-min))
    (forward-line 2)))

(defun my/typst-roam-select--render-context ()
  "Render current-note context."
  (setq my/typst-roam-select--view 'context)
  (let ((record (and my/typst-roam-select--current-note-id
                     (my/typst-roam--resolve-note
                      my/typst-roam-select--current-note-id)))
        entries)
    (when record
      (push record entries)
      (dolist (id (append (my/typst-roam--note-links (plist-get record :id))
                          (my/typst-roam--db-backlinks-to
                           (plist-get record :id))))
        (when-let* ((related (my/typst-roam--resolve-note id)))
          (push related entries))))
    (setq entries (delete-dups (nreverse entries)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (my/typst-roam-select--render-header
       (format "Current roam context: %s"
               (or my/typst-roam-select--current-note-id "(none)")))
      (if entries
          (dolist (entry entries)
            (my/typst-roam-select--insert-row
             (my/typst-roam-select--note-label entry "[note] ")
             (list :type 'note :record entry)))
        (insert "(not in a roam note; press g for root)\n"))
      (goto-char (point-min))
      (forward-line 2))))

(defun my/typst-roam-select--render-search (query)
  "Render global note search for QUERY."
  (setq my/typst-roam-select--view 'search
        my/typst-roam-select--query query)
  (let ((entries (my/typst-roam-search-notes query)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (my/typst-roam-select--render-header
       (format "Roam search: %s" query))
      (if entries
          (dolist (entry entries)
            (when-let* ((record (my/typst-roam--resolve-note
                                 (plist-get entry :slug))))
              (my/typst-roam-select--insert-row
               (my/typst-roam-select--note-label record "[note] ")
               (list :type 'note :record record))))
        (insert "(no matching notes)\n"))
      (goto-char (point-min))
      (forward-line 2))))

(defun my/typst-roam-select--toc-children (targets parent)
  "Return direct TOC children from TARGETS under PARENT."
  (let ((parent (or parent nil))
        (len (length parent)))
    (seq-filter
     (lambda (target)
       (let ((path (plist-get target :path)))
         (and (= (length path) (1+ len))
              (or (zerop len)
                  (equal (seq-take path len) parent)))))
     targets)))

(defun my/typst-roam-select--toc-has-children-p (targets target)
  "Return non-nil if TARGET has child targets in TARGETS."
  (let* ((path (plist-get target :path))
         (len (length path)))
    (seq-some
     (lambda (candidate)
       (let ((candidate-path (plist-get candidate :path)))
         (and (> (length candidate-path) len)
              (equal (seq-take candidate-path len) path))))
     targets)))

(defun my/typst-roam-select--render-toc ()
  "Render TOC selector for `my/typst-roam-select--target-record'."
  (setq my/typst-roam-select--view 'toc)
  (let* ((record my/typst-roam-select--target-record)
         (targets (my/typst-roam-select--toc-targets record))
         (query my/typst-roam-select--query)
         (visible (if (and query (not (string-empty-p query)))
                      (seq-filter
                       (lambda (target)
                         (let ((haystack (downcase
                                          (string-join
                                           (append (plist-get target :path)
                                                   (plist-get target :label-path))
                                           " "))))
                           (string-match-p
                            (regexp-quote (downcase query))
                            haystack)))
                       targets)
                    (my/typst-roam-select--toc-children
                     targets my/typst-roam-select--toc-parent))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (my/typst-roam-select--render-header
       (format "TOC: %s%s"
               (my/typst-roam-select--default-note-text record)
               (if query (format " / search: %s" query) "")))
      (when my/typst-roam-select--toc-parent
        (insert (format "Path: %s\n\n"
                        (string-join my/typst-roam-select--toc-parent " / "))))
      (if visible
          (dolist (target visible)
            (let* ((has-children (and (not query)
                                      (my/typst-roam-select--toc-has-children-p
                                       targets target)))
                   (label (my/typst-roam--dom-target-path-label target))
                   (prefix (if has-children "[+] " "[toc] ")))
              (my/typst-roam-select--insert-row
               (concat prefix label)
               (list :type 'toc
                     :target target
                     :has-children has-children
                     :search query))))
        (insert "(no TOC targets)\n"))
      (goto-char (point-min))
      (forward-line 2))))

(defun my/typst-roam-select--item-at-point ()
  "Return selector item at point."
  (or (get-text-property (point) 'my/typst-roam-select-item)
      (get-text-property (line-beginning-position) 'my/typst-roam-select-item)
      (get-text-property (max (point-min) (1- (point)))
                         'my/typst-roam-select-item)))

(defun my/typst-roam-select--finish-toc-target (target)
  "Insert the selected TOC TARGET."
  (let* ((record my/typst-roam-select--target-record)
         (dom (my/typst-roam-select--toc-dom target))
         (label (my/typst-roam--dom-target-path-label target)))
    (my/typst-roam-select--finish-target
     record my/typst-roam-select--target-basis 'dom dom label)))

(defun my/typst-roam-select-activate ()
  "Activate the selector item at point."
  (interactive)
  (pcase-let* ((item (my/typst-roam-select--item-at-point))
               (type (plist-get item :type)))
    (pcase type
      ('dir
       (my/typst-roam-select--render-root (plist-get item :path)))
      ('note
       (my/typst-roam-select--choose-record (plist-get item :record)))
      ('toc
       (if (and (plist-get item :has-children)
                (not (plist-get item :search)))
           (progn
             (setq my/typst-roam-select--toc-parent
                   (plist-get (plist-get item :target) :path)
                   my/typst-roam-select--query nil)
             (my/typst-roam-select--render-toc))
         (my/typst-roam-select--finish-toc-target
          (plist-get item :target))))
      (_
       (user-error "No selectable roam item at point")))))

(defun my/typst-roam-select-insert-current ()
  "Insert/select the current selector item without descending."
  (interactive)
  (let ((item (my/typst-roam-select--item-at-point)))
    (pcase (plist-get item :type)
      ('toc
       (my/typst-roam-select--finish-toc-target
        (plist-get item :target)))
      (_
       (my/typst-roam-select-activate)))))

(defun my/typst-roam-select-search ()
  "Search notes globally, or TOC headings inside a TOC view."
  (interactive)
  (let ((query (read-string "Search: ")))
    (if (eq my/typst-roam-select--view 'toc)
        (progn
          (setq my/typst-roam-select--query query
                my/typst-roam-select--toc-parent nil)
          (my/typst-roam-select--render-toc))
      (my/typst-roam-select--render-search query))))

(defun my/typst-roam-select-root ()
  "Render the roam root tree."
  (interactive)
  (my/typst-roam-select--render-root ""))

(defun my/typst-roam-select-context ()
  "Render current-note context."
  (interactive)
  (if my/typst-roam-select--current-note-id
      (my/typst-roam-select--render-context)
    (my/typst-roam-select--render-root "")))

(defun my/typst-roam-select-up ()
  "Move one selector level up."
  (interactive)
  (pcase my/typst-roam-select--view
    ('root
     (let* ((path (string-remove-suffix "/" my/typst-roam-select--path))
            (parent (if (string-match "\\`\\(.*?/\\)?[^/]+\\'" path)
                        (or (match-string 1 path) "")
                      "")))
       (my/typst-roam-select--render-root parent)))
    ('toc
     (setq my/typst-roam-select--query nil
           my/typst-roam-select--toc-parent
           (butlast my/typst-roam-select--toc-parent))
     (my/typst-roam-select--render-toc))
    (_
     (my/typst-roam-select-context))))

(defun my/typst-roam-select-refresh ()
  "Refresh the current selector view."
  (interactive)
  (pcase my/typst-roam-select--view
    ('root (my/typst-roam-select--render-root my/typst-roam-select--path))
    ('search (my/typst-roam-select--render-search my/typst-roam-select--query))
    ('toc (my/typst-roam-select--render-toc))
    (_ (my/typst-roam-select-context))))

(defun my/typst-roam-select--display-buffer (buffer)
  "Display selector BUFFER in a focused bottom side window."
  (let* ((alist `((side . bottom)
                  (slot . 1)
                  (window-height . ,my/typst-roam-select-window-height)))
         (window (or (get-buffer-window buffer)
                     (display-buffer-in-side-window buffer alist))))
    (set-window-buffer window buffer)
    (select-window window)
    window))

(defun my/typst-roam-select-link (&optional preferred-kind)
  "Open an interactive note-link selector.
PREFERRED-KIND may be `tag' or `toc' to skip the target-kind prompt."
  (interactive)
  (let ((origin (copy-marker (point) t))
        (current-note-id (my/typst-roam--current-slug))
        (buf (get-buffer-create "*typst-roam-select*")))
    (with-current-buffer buf
      (my/typst-roam-select-mode)
      (setq-local my/typst-roam-select--origin-marker origin
                  my/typst-roam-select--current-note-id current-note-id
                  my/typst-roam-select--preferred-kind preferred-kind
                  my/typst-roam-select--target-record nil
                  my/typst-roam-select--target-basis 'id
                  my/typst-roam-select--toc-parent nil
                  my/typst-roam-select--query nil)
      (my/typst-roam-select--render-search ""))
    (my/typst-roam-select--display-buffer buf)))

(defun my/typst-roam-copy-link-to-here ()
  "Copy a Markdown roam link to the current note or current heading.
When point is on a heading, ensure a Markdown `{#tag-id}' exists and copy a
canonical `roam://note-id#tag' target."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer has no file"))
  (let* ((note-id (my/typst-roam--current-slug))
         (title (my/typst-roam--note-title note-id))
         target text)
    (unless note-id
      (user-error "Not in a roam note"))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*#\\{1,6\\}[ \t]+\\(.+?\\)\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$")
          (let ((heading (string-trim (match-string 1)))
                (id (match-string 2)))
            (setq heading
                  (string-trim
                   (replace-regexp-in-string
                    "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" "" heading)))
            (unless id
              (setq id (my/typst-roam-generate-tag-id heading))
              (end-of-line)
              (insert (format " {#%s}" id)))
            (setq target (my/typst-roam--roam-href note-id 'tag id)
                  text heading))
        (setq target (my/typst-roam--roam-href note-id)
              text title)))
    (let ((link (format "[%s](%s)"
                        (replace-regexp-in-string "\\]" "\\\\]" (or text ""))
                        target)))
      (kill-new link)
      (message "Copied %s" link))))

;; Enhanced find-note with DB annotation
(defun my/typst-roam--note-annotator (slug)
  "Return annotation for SLUG in completing-read."
  (when-let* ((record (my/typst-roam--resolve-note slug))
              (note (plist-get record :note)))
    (let ((tags (my/typst-roam--note-list-field note "tags"))
          (bls  (length (or (my/typst-roam--db-backlinks-to
                             (plist-get record :id))
                            (gethash "backlinks" note)))))
      (concat "  "
              (if tags (string-join tags ",") "")
              (when (> bls 0) (format " ←%d" bls))))))

;; Auto-update DB on save
(defun my/typst-roam--note-file-p (file)
  "Return non-nil when FILE is a Markdown roam note in the current vault."
  (when (and file
             (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file))
    (let* ((root (file-name-as-directory
                  (file-truename (my/typst-roam-root))))
           (truename (file-truename file))
           (rel (file-relative-name truename root)))
      (and (string-prefix-p root truename)
           (not (string-match-p
                 "\\`\\(?:\\.git/\\|\\.lean/\\|_typst/\\|node_modules/\\)"
                 rel))))))

(defun my/typst-roam--schedule-runtime-sync (file)
  "Debounce an incremental runtime sync for changed Markdown note FILE."
  (when (timerp my/typst-roam--sync-timer)
    (cancel-timer my/typst-roam--sync-timer))
  (push file my/typst-roam--sync-changed-files)
  (setq my/typst-roam--sync-changed-files
        (delete-dups
         (seq-filter #'identity my/typst-roam--sync-changed-files)))
  (setq my/typst-roam--sync-timer
        (run-at-time
         my/typst-roam-sync-delay nil
         (lambda ()
           (let ((changed my/typst-roam--sync-changed-files))
             (setq my/typst-roam--sync-timer nil
                   my/typst-roam--sync-changed-files nil)
             (if (my/typst-roam--runtime-available-p)
                 (my/typst-roam--runtime-sync nil changed)
               (message "Markdown roam cache refreshed")))))))

;; ── Keymaps & menus ───────────────────────────────────────────────────────────

(defvar my/typst-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'my/typst-roam-find-note)
    (define-key map (kbd "o") #'my/typst-roam-follow-link)
    (define-key map (kbd "i") #'my/typst-roam-insert-link)
    (define-key map (kbd "RET") #'my/typst-roam-select-link)
    (define-key map (kbd "I") #'my/typst-roam-insert-tag-id-link)
    (define-key map (kbd "c") #'my/typst-roam-insert-toc-link)
    (define-key map (kbd "y") #'my/typst-roam-copy-link-to-here)
    (define-key map (kbd "n") #'my/typst-roam-new-note)
    (define-key map (kbd "N") #'my/typst-roam-new-node)
    (define-key map (kbd "#") #'my/typst-roam-insert-tag-id)
    (define-key map (kbd "g") #'my/typst-roam-generate-tag-id)
    (define-key map (kbd "s") #'my/typst-roam-search-notes)
    (define-key map (kbd "r") #'my/typst-roam-recent-notes)
    (define-key map (kbd "R") #'my/typst-roam-related-notes)
    (define-key map (kbd "G") #'my/typst-roam-graph)
    (define-key map (kbd "M") #'my/typst-roam-management)
    (define-key map (kbd "b") #'my/typst-roam-backlinks)
    (define-key map (kbd "t") #'my/typst-roam-tags)
    (define-key map (kbd "T") #'my/typst-roam-todos)
    (define-key map (kbd "u") #'my/typst-roam-update-db)
    (define-key map (kbd "m") #'my/typst-roam-dispatch)
    map)
  "Roam keymap for Markdown buffers. Bound to C-c r.")

(my/leader!
  "r m" '(:def my/typst-roam-dispatch :which-key "md roam")
  "r t" '(:def my/typst-roam-dispatch :which-key "md roam"))

;; ── xref backend: gd / M-. for note-link ─────────────────────────────────

(defun my/typst-roam--all-slugs-cached ()
  "Return all canonical roam note ids."
  (mapcar (lambda (record) (plist-get record :id))
          (my/typst-roam--note-records)))

(defun my/typst-roam-xref-backend ()
  "Use typst-roam as xref backend when point is on a Markdown roam link."
  (when (my/typst-roam--target-at-point) 'typst-roam))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql typst-roam)))
  (my/typst-roam--target-at-point))

(defun my/typst-roam-goto-definition ()
  "Jump to the note-link target at point, falling back to normal gd."
  (interactive)
  (if (my/typst-roam--target-at-point)
      (progn
        (when (fboundp 'my/navigation--push-jump)
          (my/navigation--push-jump))
        (my/typst-roam-follow-link))
    (if (fboundp 'my/navigation-find-definition)
        (call-interactively #'my/navigation-find-definition)
      (call-interactively #'xref-find-definitions))))

(defun my/typst-roam--xref-location (file parsed)
  "Return an xref location in FILE for PARSED target."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((pos (cond
                ((plist-get parsed :id)
                 (goto-char (point-min))
                 (or (when (re-search-forward
                             (format "{#%s}"
                                     (regexp-quote (plist-get parsed :id)))
                             nil t)
                       (match-beginning 0))
                     (progn
                       (goto-char (point-min))
                       (when (re-search-forward
                              (format "<%s>"
                                      (regexp-quote (plist-get parsed :id)))
                              nil t)
                         (match-beginning 0)))))
                ((plist-get parsed :dom)
                 (plist-get
                  (my/typst-roam--find-dom-target
                   (plist-get parsed :dom) file (plist-get parsed :slug))
                  :pos)))))
      (if pos
          (progn
            (my/typst-roam--goto-pos pos)
            (xref-make-file-location file
                                     (line-number-at-pos)
                                     (current-column)))
        (xref-make-file-location file 1 0)))))

(cl-defmethod xref-backend-definitions ((_backend (eql typst-roam)) target)
  (when-let* ((parsed (my/typst-roam--parse-target target))
              (file (plist-get parsed :file))
              ((file-exists-p file)))
    (list (xref-make (concat "note: " target)
                     (my/typst-roam--xref-location file parsed)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql typst-roam)))
  (mapcar #'my/typst-roam--roam-href
          (my/typst-roam--all-slugs-cached)))

(defun my/typst-roam--xref-setup ()
  "Register typst-roam xref backend for this buffer (highest priority)."
  (add-hook 'xref-backend-functions #'my/typst-roam-xref-backend -90 t))

;; ── Preview click → note-link intercept ──────────────────────────────────

;; ── Daily note ────────────────────────────────────────────────────────────

(defun my/typst-roam-daily-note ()
  "Open or create today's daily note at daily/YYYY-MM-DD."
  (interactive)
  (let* ((slug (concat "daily/" (format-time-string "%Y-%m-%d")))
         (file (my/typst-roam--slug-to-file slug)))
    (if (file-exists-p file)
        (my/typst-roam--open-slug slug)
      (my/typst-roam-new-note slug))))

;; ── Wire everything up ────────────────────────────────────────────────────

(define-key my/typst-roam-map (kbd "d") #'my/typst-roam-daily-note)

;; Update transient with daily + gd hint
(transient-define-prefix my/typst-roam-dispatch ()
  "Markdown roam command menu."
  [["Notes"
    ("RET" "select link"         my/typst-roam-select-link)
    ("o" "open link   C-c C-o" my/typst-roam-follow-link)
    ("f" "find note"            my/typst-roam-find-note)
    ("i" "insert link"          my/typst-roam-insert-link)
    ("I" "insert tag link"      my/typst-roam-insert-tag-id-link)
    ("c" "insert toc link"      my/typst-roam-insert-toc-link)
    ("y" "copy link here"       my/typst-roam-copy-link-to-here)
    ("n" "new note"             my/typst-roam-new-note)
    ("N" "new node"             my/typst-roam-new-node)
    ("d" "daily note"           my/typst-roam-daily-note)]
   ["Tag ids"
    ("#" "insert tag id"        my/typst-roam-insert-tag-id)
    ("g" "generate tag id"      my/typst-roam-generate-tag-id)]
   ["Explore"
    ("s" "search/filter"        my/typst-roam-search-notes)
    ("r" "recent"               my/typst-roam-recent-notes)
    ("R" "related"              my/typst-roam-related-notes)
    ("G" "graph"                my/typst-roam-graph)
    ("M" "management"           my/typst-roam-management)]
   ["DB"
    ("b" "backlinks"            my/typst-roam-backlinks)
    ("t" "tags"                 my/typst-roam-tags)
    ("T" "todos"                my/typst-roam-todos)
    ("u" "update db"            my/typst-roam-update-db)]
   ["Nav (gd = xref)"
    ("." "xref definition"      xref-find-definitions)
    ("x" "xref references"      xref-find-references)]])

(provide 'init-md-roam)
;;; init-md-roam.el ends here
