;;; init-md-roam.el --- Markdown roam note navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Roam-style helpers for Aaronnote Markdown notes.

;;; Code:

(require 'init-funcs)
(require 'init-md-roam-ui)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'url-util)
(require 'wid-edit)
(require 'xref)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function my/aaronnote-open-file "init-aaronnote" (file))
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")

(defgroup my/aaronnote-roam nil
  "Roam-style navigation for Aaronnote Markdown notes."
  :group 'my/aaronnote)

(defconst my/aaronnote-roam--module-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the Markdown roam Emacs bridge.")

(defcustom my/aaronnote-roam-root
  (expand-file-name ".roam" user-emacs-directory)
  "Root directory of the Markdown roam note vault."
  :type 'directory
  :group 'my/aaronnote-roam)

(defcustom my/aaronnote-roam-recent-limit 24
  "Maximum number of recent Markdown roam notes kept in memory."
  :type 'integer
  :group 'my/aaronnote-roam)

(defcustom my/aaronnote-roam-select-window-height 0.32
  "Height for the bottom Markdown roam selector window."
  :type '(choice (number :tag "Fraction or rows")
                 (function :tag "Window height function"))
  :group 'my/aaronnote-roam)

(defcustom my/aaronnote-roam-runtime-root
  (expand-file-name "aaronnote" my/aaronnote-roam--module-directory)
  "Root of the vendored Aaronnote runtime used by Markdown roam tooling."
  :type 'directory
  :group 'my/aaronnote-roam)

(defcustom my/aaronnote-roam-runtime-cli
  (expand-file-name "roam-cli.mjs" my/aaronnote-roam-runtime-root)
  "Node bridge used to call the vendored Aaronnote roam runtime from Emacs."
  :type 'file
  :group 'my/aaronnote-roam)

(defcustom my/aaronnote-roam-sync-delay 1.5
  "Seconds to debounce automatic incremental roam-db sync after saving."
  :type 'number
  :group 'my/aaronnote-roam)

(defvar my/aaronnote-roam--recent nil
  "Recently opened Markdown roam note ids, newest first.")

(defvar my/aaronnote-roam--runtime-index-cache nil)
(defvar my/aaronnote-roam--runtime-index-cache-key nil)
(defvar my/aaronnote-roam--sync-timer nil)
(defvar my/aaronnote-roam--sync-changed-files nil)

(defun my/aaronnote-roam-root ()
  "Return the Markdown roam notes root."
  (or (when buffer-file-name
        (when-let* ((dir (or (locate-dominating-file
                              buffer-file-name ".aaronnote-sync-state.json")
                             (locate-dominating-file
                              buffer-file-name ".aaronnote-asset-cleanup-state.json"))))
          (file-truename dir)))
      (when (boundp 'my/aaronnote--notes-root)
        (file-name-as-directory (expand-file-name my/aaronnote--notes-root)))
      (file-name-as-directory (expand-file-name my/aaronnote-roam-root))))

(defun my/aaronnote-roam--clear-runtime-cache ()
  "Clear cached Aaronnote runtime payloads."
  (setq my/aaronnote-roam--runtime-index-cache nil
        my/aaronnote-roam--runtime-index-cache-key nil
        my/aaronnote-roam--scan-cache nil
        my/aaronnote-roam--db-cache nil
        my/aaronnote-roam--db-path-cache nil
        my/aaronnote-roam--db-mtime nil))

(defun my/aaronnote-roam--runtime-available-p ()
  "Return non-nil when the Aaronnote runtime bridge is available."
  (and (file-exists-p my/aaronnote-roam-runtime-cli)
       (file-exists-p
        (expand-file-name "server/lib/index.mjs"
                          my/aaronnote-roam-runtime-root))))

(defun my/aaronnote-roam--runtime-call (action &rest args)
  "Call Aaronnote roam runtime ACTION synchronously with ARGS.
Return parsed JSON as hash tables/lists, or nil when the runtime is unavailable
or the command fails."
  (when (my/aaronnote-roam--runtime-available-p)
    (with-temp-buffer
      (let* ((root (my/aaronnote-roam-root))
             (default-directory my/aaronnote-roam--module-directory)
             (process-environment
              (append (list (format "AARONNOTE_ROOT=%s" root)
                            (format "AARONNOTE_RUNTIME_ROOT=%s"
                                    (expand-file-name
                                     my/aaronnote-roam-runtime-root))
                            (format "AARONNOTE_WORKSPACE_ROOT=%s"
                                    user-emacs-directory))
                      process-environment))
             ;; Capture stderr separately so it does not corrupt the JSON stdout.
             (stderr-file (make-temp-file "aaronnote-runtime-"))
             (status (apply #'process-file
                            "node" nil (list (current-buffer) stderr-file) nil
                            my/aaronnote-roam-runtime-cli
                            action
                            "--root" root
                            "--runtime" my/aaronnote-roam-runtime-root
                            "--workspace" user-emacs-directory
                            args)))
        (unwind-protect
            (if (zerop status)
                (condition-case err
                    (progn
                      (goto-char (point-min))
                      (json-parse-buffer :object-type 'hash-table
                                         :array-type 'list))
                  (error
                   (let ((stderr (with-temp-buffer
                                   (ignore-errors (insert-file-contents stderr-file))
                                   (string-trim (buffer-string)))))
                     (message "Aaronnote roam runtime: JSON parse failed: %s%s"
                              err
                              (if (string-empty-p stderr) "" (concat "\n" stderr))))
                   nil))
              (let ((stderr (with-temp-buffer
                               (ignore-errors (insert-file-contents stderr-file))
                               (string-trim (buffer-string)))))
                (message "Aaronnote roam runtime failed (%s): %s"
                         action
                         (if (string-empty-p stderr)
                             (string-trim (buffer-string))
                           stderr))
                nil))
          (ignore-errors (delete-file stderr-file)))))))

(defun my/aaronnote-roam--runtime-index ()
  "Return cached Aaronnote runtime index payload, or nil."
  (let ((key (list (file-truename (my/aaronnote-roam-root))
                   (file-truename
                    (expand-file-name my/aaronnote-roam-runtime-root)))))
    (if (and my/aaronnote-roam--runtime-index-cache
             (equal key my/aaronnote-roam--runtime-index-cache-key))
        my/aaronnote-roam--runtime-index-cache
      (setq my/aaronnote-roam--runtime-index-cache
            (my/aaronnote-roam--runtime-call "index")
            my/aaronnote-roam--runtime-index-cache-key key)
      my/aaronnote-roam--runtime-index-cache)))

(defun my/aaronnote-roam--runtime-sync (&optional full changed-files)
  "Run Aaronnote roam-db sync asynchronously.
When FULL is non-nil, force a full rebuild.  CHANGED-FILES are passed to the
runtime incremental sync."
  (if (not (my/aaronnote-roam--runtime-available-p))
      (message "Aaronnote roam runtime not found; cache refreshed only")
    (let* ((root (my/aaronnote-roam-root))
           (buf (get-buffer-create "*roam-index*"))
           (args (append
                  (list my/aaronnote-roam-runtime-cli
                        "sync"
                        "--root" root
                        "--runtime" my/aaronnote-roam-runtime-root
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
           (my/aaronnote-roam--clear-runtime-cache)
           (message "Aaronnote roam sync: %s" (string-trim event))))))))

(defun my/aaronnote-roam--target-at-point ()
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
                            (my/aaronnote-roam--encode-ref
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

(defun my/aaronnote-roam--decode-ref (ref)
  "Percent-decode note REF, returning REF unchanged on malformed input."
  (condition-case nil
      (url-unhex-string (or ref ""))
    (error (or ref ""))))

(defun my/aaronnote-roam--encode-ref (ref)
  "Percent-encode REF for use in Aaronnote roam URLs."
  (url-hexify-string (or ref "")))

(defun my/aaronnote-roam--split-target (target)
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
              tag (my/aaronnote-roam--decode-ref (match-string 2 body))))
       ((string-match "\\`\\(.*?\\)@\\([^#]*\\)\\'" body)
        (setq ref (match-string 1 body)
              dom (my/aaronnote-roam--decode-ref (match-string 2 body))))
       (t
        (setq ref body)))
      (list :raw raw
            :ref (string-trim
                  (replace-regexp-in-string
                   "\\`/+" ""
                   (my/aaronnote-roam--decode-ref (or ref ""))))
            :tag (and tag (not (string-empty-p tag)) tag)
            :dom (and dom (not (string-empty-p dom)) dom)))))

(defun my/aaronnote-roam--parse-target (target)
  "Parse note-link TARGET into Aaronnote-compatible target metadata."
  (when-let* ((parts (my/aaronnote-roam--split-target target)))
    (let* ((ref (plist-get parts :ref))
           (resolved (my/aaronnote-roam--resolve-note ref))
           (id (or (plist-get resolved :id) ref))
           (file (or (plist-get resolved :file)
                     (my/aaronnote-roam--ref-to-file-fallback ref))))
      (append parts
              (list :slug id
                    :note-id id
                    :id (plist-get parts :tag)
                    :file file
                    :key (plist-get resolved :key)
                    :note (plist-get resolved :note))))))

(defun my/aaronnote-roam--slug-at-point ()
  "Return the note-link slug at or near point, or nil."
  (plist-get (my/aaronnote-roam--parse-target (my/aaronnote-roam--target-at-point))
             :slug))

(defun my/aaronnote-roam--all-files ()
  "Return all Markdown roam note files, excluding generated/private dirs."
  (seq-filter
   (lambda (file)
     (let ((rel (file-relative-name file (my/aaronnote-roam-root))))
       (not (string-match-p
             "\\`\\(?:\\.git/\\|\\.lean/\\|_typst/\\|node_modules/\\)"
             rel))))
   (directory-files-recursively
    (my/aaronnote-roam-root) "\\.\\(?:md\\|markdown\\)$")))

(defun my/aaronnote-roam--file-to-slug (file)
  "Convert FILE path to a roam slug, relative to root and without extension."
  (my/aaronnote-roam--path-without-note-extension
   (file-relative-name file (my/aaronnote-roam-root))))

(defun my/aaronnote-roam--file-to-note-id (file)
  "Return the canonical note id for FILE, falling back to its path slug."
  (let* ((slug (my/aaronnote-roam--file-to-slug file))
         (resolved (my/aaronnote-roam--resolve-note slug)))
    (or (plist-get resolved :id) slug)))

(defun my/aaronnote-roam--ref-has-extension-p (ref)
  "Return non-nil when REF already names a note file extension."
  (string-match-p "\\.\\(?:typ\\|md\\|markdown\\)\\'" (or ref "")))

(defun my/aaronnote-roam--path-without-note-extension (path)
  "Remove a note file extension from PATH."
  (replace-regexp-in-string "\\.\\(?:typ\\|md\\|markdown\\)\\'" "" (or path "")))

(defun my/aaronnote-roam--strip-vault-prefix (ref)
  "Remove Aaronnote's exported `roam/' prefix from path REF."
  (let ((clean (replace-regexp-in-string "\\`/+" "" (or ref ""))))
    (if (string-prefix-p "roam/" clean)
        (substring clean 5)
      clean)))

(defun my/aaronnote-roam--ref-to-file-fallback (ref)
  "Return the best filesystem fallback for unresolved note REF."
  (let* ((clean (my/aaronnote-roam--strip-vault-prefix
                 (string-trim (or ref ""))))
         (root (my/aaronnote-roam-root))
         (path (if (file-name-absolute-p clean)
                   clean
                 (expand-file-name clean root))))
    (cond
     ((and (not (string-empty-p clean))
           (file-exists-p path))
      path)
     ((and (not (string-empty-p clean))
           (not (my/aaronnote-roam--ref-has-extension-p clean))
           (file-exists-p (concat path ".md")))
      (concat path ".md"))
     ((and (not (string-empty-p clean))
           (not (my/aaronnote-roam--ref-has-extension-p clean))
           (file-exists-p (concat path ".markdown")))
      (concat path ".markdown"))
     ((my/aaronnote-roam--ref-has-extension-p clean)
      path)
     (t
      (concat path ".md")))))

(defun my/aaronnote-roam--slug-to-file (slug)
  "Convert SLUG, id, or path-like ref to an absolute note path."
  (or (plist-get (my/aaronnote-roam--resolve-note slug) :file)
      (my/aaronnote-roam--ref-to-file-fallback slug)))

(defun my/aaronnote-roam--slugify-title (title)
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

(defun my/aaronnote-roam--timestamp-id ()
  "Return an Aaronnote-style timestamp id."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun my/aaronnote-roam--open-slug (slug &optional no-recent)
  "Open roam note SLUG/id/path and record it in recent notes unless NO-RECENT."
  (let* ((resolved (my/aaronnote-roam--resolve-note slug))
         (note-id (or (plist-get resolved :id) slug))
         (file (or (plist-get resolved :file)
                   (my/aaronnote-roam--ref-to-file-fallback slug))))
    (unless (file-exists-p file)
      (user-error "Note not found: %s" slug))
    (unless no-recent
      (my/aaronnote-roam--touch-recent note-id))
    (find-file file)))

(defun my/aaronnote-roam--touch-recent (slug)
  "Move SLUG to the front of the recent list."
  (when (and (stringp slug) (not (string-empty-p slug)))
    (setq my/aaronnote-roam--recent
          (seq-take (cons slug (delete slug my/aaronnote-roam--recent))
                    my/aaronnote-roam-recent-limit))))

(defun my/aaronnote-roam--note-title (slug)
  "Return display title for SLUG."
  (or (when-let* ((note (my/aaronnote-roam--db-note slug)))
        (gethash "title" note))
      (plist-get (my/aaronnote-roam--resolve-note slug) :title)
      (file-name-nondirectory slug)))

(defun my/aaronnote-roam--note-tags (slug)
  "Return tags for SLUG."
  (when-let* ((note (my/aaronnote-roam--db-note slug)))
    (my/aaronnote-roam--note-list-field note "tags")))

(defun my/aaronnote-roam--note-links (slug)
  "Return normalized outgoing link slugs for SLUG."
  (when-let* ((note (my/aaronnote-roam--db-note slug)))
    (delete-dups
     (seq-filter #'identity
                 (mapcar #'my/aaronnote-roam--target-slug
                         (or (my/aaronnote-roam--note-list-field note "links")
                             (my/aaronnote-roam--note-list-field note "refs")))))))

(defun my/aaronnote-roam--note-summary (slug)
  "Return a compact text summary for SLUG."
  (or (when-let* ((note (my/aaronnote-roam--db-note slug)))
        (my/aaronnote-roam--note-field note "summary"))
      (let ((file (my/aaronnote-roam--slug-to-file slug)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 20000)
            (or (my/aaronnote-roam--extract-summary-block)
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

(defun my/aaronnote-roam--all-note-summaries ()
  "Return note summary plists for all notes."
  (mapcar (lambda (record)
            (let* ((id (plist-get record :id))
                   (note (plist-get record :note)))
              (list :slug id
                    :title (or (plist-get record :title)
                               (my/aaronnote-roam--note-title id))
                    :path (or (my/aaronnote-roam--note-field note "path")
                              (my/aaronnote-roam--note-field note "link"))
                    :aliases (my/aaronnote-roam--note-list-field note "aliases")
                    :tags (my/aaronnote-roam--note-tags id)
                    :links (my/aaronnote-roam--note-links id)
                    :backlinks (my/aaronnote-roam--db-backlinks-to id)
                    :summary (my/aaronnote-roam--note-summary id))))
          (sort (my/aaronnote-roam--note-records)
                (lambda (a b)
                  (string< (plist-get a :id) (plist-get b :id))))))

(defun my/aaronnote-roam--candidate-haystack (entry)
  "Return searchable text for note summary ENTRY."
  (string-join
   (delq nil
         (list (plist-get entry :slug)
               (plist-get entry :title)
               (plist-get entry :path)
               (plist-get entry :summary)
               (string-join (seq-filter #'stringp (or (plist-get entry :aliases) nil)) " ")
               (string-join (seq-filter #'stringp (or (plist-get entry :tags) nil)) " ")))
   " "))

(defun my/aaronnote-roam--read-note (prompt &optional entries)
  "Read a note slug with PROMPT from ENTRIES or all summaries."
  (let* ((items (or entries (my/aaronnote-roam--all-note-summaries)))
         (table (mapcar (lambda (entry)
                          (cons (plist-get entry :slug) entry))
                        items))
         (slug (completing-read
                prompt
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (annotation-function
                         . ,(lambda (candidate)
                              (when-let* ((entry (cdr (assoc candidate table))))
                                (let ((tags (plist-get entry :tags))
                                      (backlinks (length (or (plist-get entry :backlinks) nil))))
                                  (concat
                                   "  "
                                   (or (plist-get entry :title) "")
                                   (when-let* ((strtags (seq-filter #'stringp tags)))
                                     (concat "  #" (string-join strtags " #")))
                                   (when (> backlinks 0)
                                     (format " ←%d" backlinks))))))))
                    (complete-with-action action table string pred)))
                nil t)))
    slug))

(defun my/aaronnote-roam--read-note-id (prompt)
  "Read an Aaronnote note id with PROMPT."
  (let* ((records (my/aaronnote-roam--note-records))
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
           `(metadata
             (annotation-function
              . ,(lambda (candidate)
                   (when-let* ((record (cdr (assoc candidate table))))
                     (let ((note (plist-get record :note)))
                       (concat
                        "  "
                        (or (plist-get record :title) "")
                        (when-let* ((path (or (my/aaronnote-roam--note-field note "path")
                                              (my/aaronnote-roam--note-field note "link"))))
                          (concat "  " path))
                        (when-let* ((tags (seq-filter #'stringp
                                                      (my/aaronnote-roam--note-list-field note "tags"))))
                          (concat "  #" (string-join tags " #")))))))))
         (complete-with-action action candidates string pred)))
     nil t)))

(defun my/aaronnote-roam--roam-href (note-id &optional kind target)
  "Return canonical Aaronnote roam href for NOTE-ID and optional TARGET."
  (concat "roam://"
          (my/aaronnote-roam--encode-ref note-id)
          (pcase kind
            ('tag (concat "#" (my/aaronnote-roam--encode-ref target)))
            ('dom (concat "@" (mapconcat #'my/aaronnote-roam--encode-ref
                                          (my/aaronnote-roam--dom-target-segments target)
                                          "@")))
            (_ ""))))

(defun my/aaronnote-roam--heading-labels (&optional file)
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

(defun my/aaronnote-roam--goto-tag-id (id)
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
   ((when-let* ((target (my/aaronnote-roam--find-dom-target id)))
      (my/aaronnote-roam--goto-pos (plist-get target :pos))
      t))
   (t
    (user-error "Tag id not found: %s" id)))
  (recenter-top-bottom))

(defun my/aaronnote-roam--heading-items (&optional file)
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

(defun my/aaronnote-roam--goto-pos (pos)
  "Move to POS, treating nil or synthetic zero positions as file start."
  (goto-char (if (and (integerp pos) (>= pos (point-min)))
                 pos
               (point-min))))

(defun my/aaronnote-roam--normalize-dom-target (value)
  "Normalize Aaronnote DOM target VALUE for matching."
  (string-trim
   (replace-regexp-in-string
    "\\s-+" " "
    (replace-regexp-in-string
     "[][\r\n]" " "
     (string-remove-prefix "@" (my/aaronnote-roam--decode-ref (or value "")))))))

(defun my/aaronnote-roam--slug-dom-target (value)
  "Return Aaronnote's DOM target slug for VALUE."
  (let ((clean (downcase
                (replace-regexp-in-string
                 "[`*_~()[\\]{}#+.!<>:;,'\"@]" " "
                 (my/aaronnote-roam--normalize-dom-target value)))))
    (replace-regexp-in-string
     "\\s-+" "-"
     (string-trim clean))))

(defun my/aaronnote-roam--dom-target-segments (value)
  "Return normalized DOM target path segments from VALUE."
  (seq-filter
   (lambda (segment) (not (string-empty-p segment)))
   (mapcar #'my/aaronnote-roam--slug-dom-target
           (split-string (string-remove-prefix "@" (or value "")) "@"))))

(defun my/aaronnote-roam--dom-targets (&optional file note-id)
  "Return Aaronnote-style DOM/TOC targets for FILE or current buffer."
  (let ((items (my/aaronnote-roam--heading-items file))
        (stack nil)
        (label-stack nil)
        targets)
    (when-let* ((note-id)
                (title (plist-get (my/aaronnote-roam--resolve-note note-id) :title)))
      (let ((label (my/aaronnote-roam--normalize-dom-target title))
            (slug (my/aaronnote-roam--slug-dom-target title)))
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
             (label (my/aaronnote-roam--normalize-dom-target
                     (plist-get item :text)))
             (slug (my/aaronnote-roam--slug-dom-target label)))
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

(defun my/aaronnote-roam--dom-target-path-label (target)
  "Return a readable label path for TARGET."
  (string-join (plist-get target :label-path) " / "))

(defun my/aaronnote-roam--target-path-matches-p (actual wanted &optional allow-suffix)
  "Return non-nil when ACTUAL target path matches WANTED."
  (let ((actual (mapcar #'my/aaronnote-roam--slug-dom-target actual))
        (wanted (mapcar #'my/aaronnote-roam--slug-dom-target wanted)))
    (cond
     ((or (null actual) (null wanted)) nil)
     ((equal actual wanted) t)
     ((and allow-suffix
           (>= (length actual) (length wanted)))
      (equal (last actual (length wanted)) wanted)))))

(defun my/aaronnote-roam--find-dom-target (dom &optional file note-id)
  "Find DOM target DOM in FILE or current buffer."
  (let* ((wanted (my/aaronnote-roam--dom-target-segments dom))
         (targets (my/aaronnote-roam--dom-targets file note-id)))
    (cond
     ((null wanted) nil)
     ((> (length wanted) 1)
      (or (seq-find (lambda (target)
                      (my/aaronnote-roam--target-path-matches-p
                       (plist-get target :path) wanted))
                    targets)
          (seq-find (lambda (target)
                      (my/aaronnote-roam--target-path-matches-p
                       (plist-get target :path) wanted t))
                    targets)))
     (t
      (let* ((wanted-segment (car wanted))
             (wanted-label (my/aaronnote-roam--normalize-dom-target dom)))
        (seq-find
         (lambda (target)
           (or (equal (plist-get target :slug) wanted-segment)
               (equal (downcase (plist-get target :label))
                      (downcase wanted-label))))
         targets))))))

(defun my/aaronnote-roam--goto-dom-target (dom)
  "Jump to Aaronnote DOM/TOC target DOM in the current buffer."
  (let* ((target (my/aaronnote-roam--find-dom-target dom))
         (pos (and target (plist-get target :pos))))
    (unless pos
      (user-error "DOM target not found: %s" dom))
    (my/aaronnote-roam--goto-pos pos)
    (recenter-top-bottom)))

(defun my/aaronnote-roam--read-dom-target (note-id)
  "Read an Aaronnote DOM/TOC target for NOTE-ID."
  (let* ((record (my/aaronnote-roam--resolve-note note-id))
         (file (plist-get record :file))
         (targets (my/aaronnote-roam--dom-targets file note-id))
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
                                         (my/aaronnote-roam--dom-target-path-label
                                          target))))))
                      (complete-with-action action table string pred)))
                  nil t)))
    (cdr (assoc choice table))))

(defun my/aaronnote-roam--ui-actions (&optional leading)
  "Return standard native roam view actions after optional LEADING actions."
  (append
   leading
   '((:label "g Refresh"
      :command my/aaronnote-roam-ui-refresh
      :help "Refresh this Aaronnote roam view"
      :primary t)
     (:label "q Close"
      :command quit-window
      :help "Close this Aaronnote roam view"))))

(defun my/aaronnote-roam--prepare-ui-buffer
    (name title icon refresh-function &optional status)
  "Prepare native roam buffer NAME with TITLE, ICON, REFRESH-FUNCTION, and STATUS."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/aaronnote-roam-ui-mode)
        (my/aaronnote-roam-ui-mode))
      (setq-local my/aaronnote-roam-ui-refresh-function refresh-function)
      (my/aaronnote-roam-ui-set-header title icon status))
    buffer))

(defun my/aaronnote-roam--show-toc (&optional file title)
  "Show a heading TOC for FILE or the current buffer."
  (let* ((source-buffer (current-buffer))
         (target-file (or file buffer-file-name))
         (items (if file
                    (my/aaronnote-roam--heading-items file)
                  (with-current-buffer source-buffer
                    (my/aaronnote-roam--heading-items))))
         (display-title (or title file (buffer-name source-buffer)))
         (refresh
          (let ((source source-buffer) (f file) (label title))
            (lambda ()
              (if (buffer-live-p source)
                  (with-current-buffer source
                    (my/aaronnote-roam--show-toc f label))
                (my/aaronnote-roam--show-toc f label)))))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-toc*" "Roam TOC" 'toc refresh
               (format "%d headings" (length items)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Table of contents"
          :icon 'toc
          :subtitle (format "%s" display-title)
          :stats (list (cons (format "%d headings" (length items)) 'info))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Headings" (length items))
         (if (null items)
             (my/aaronnote-roam-ui-insert-empty "No headings in this note.")
           (dolist (item items)
             (let ((pos (plist-get item :pos))
                   (level (plist-get item :level))
                   (text (plist-get item :text)))
               (my/aaronnote-roam-ui-insert-row
                :id pos
                :icon 'toc
                :title text
                :meta (format "H%d" level)
                :indent (1- level)
                :action
                (let ((target target-file)
                      (source source-buffer)
                      (position pos))
                  (lambda (_button)
                    (cond
                     (target (find-file target))
                     ((buffer-live-p source) (pop-to-buffer source))
                     (t (user-error "TOC source buffer is no longer available")))
                    (my/aaronnote-roam--goto-pos position)
                    (recenter-top-bottom))))))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-follow-link ()
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
    (if-let* ((target (my/aaronnote-roam--target-at-point))
              (parsed (my/aaronnote-roam--parse-target target))
              (note-id (plist-get parsed :slug))
              (file (plist-get parsed :file)))
        (let ((ref (plist-get parsed :ref)))
          (if (file-exists-p file)
              (progn
                (my/aaronnote-roam--touch-recent note-id)
                (find-file file)
                (cond
                 ((plist-get parsed :id)
                  (my/aaronnote-roam--goto-tag-id (plist-get parsed :id)))
                 ((plist-get parsed :dom)
                  (my/aaronnote-roam--goto-dom-target (plist-get parsed :dom)))))
            (when (yes-or-no-p (format "Note '%s' not found. Create it? " ref))
              (my/aaronnote-roam-new-note ref))))
      (user-error "No Markdown roam link or #note-code found at point"))))

(defun my/aaronnote-roam-find-note ()
  "Find a roam note by Aaronnote id/path/title with completion."
  (interactive)
  (my/aaronnote-roam--open-slug
   (my/aaronnote-roam--read-note-id "Roam note: ")))

(defun my/aaronnote-roam-insert-link ()
  "Open the interactive selector and insert a Markdown roam link."
  (interactive)
  (my/aaronnote-roam-select-link))

(defvar-local my/aaronnote-roam-new--draft nil
  "Draft plist edited by the current Roam New buffer.")

(defvar-local my/aaronnote-roam-new--templates nil
  "Template records available to the current Roam New buffer.")

(defvar-local my/aaronnote-roam-new--base-directory ""
  "Relative default directory used by the current Roam New buffer.")

(defvar-local my/aaronnote-roam-new--widgets nil
  "Editable widgets in the current Roam New buffer.")

(defvar my/aaronnote-roam-new-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map my/aaronnote-roam-ui-mode-map)
    (define-key map (kbd "c") #'my/aaronnote-roam-new-create)
    (define-key map (kbd "R") #'my/aaronnote-roam-new-reset)
    (define-key map (kbd "t") #'my/aaronnote-roam-new-edit-type)
    (define-key map (kbd "T") #'my/aaronnote-roam-new-edit-template)
    (define-key map (kbd "a") #'my/aaronnote-roam-new-edit-tags)
    (define-key map (kbd "p") #'my/aaronnote-roam-new-edit-path)
    map)
  "Keymap for `my/aaronnote-roam-new-mode'.")

(define-derived-mode my/aaronnote-roam-new-mode my/aaronnote-roam-ui-mode "Roam-New"
  "Native workbench for creating Aaronnote Markdown notes."
  ;; This view is a form.  Keep it writable so Emacs widget fields accept direct
  ;; typing instead of forcing every edit through the minibuffer.
  (setq-local buffer-read-only nil)
  (setq-local my/aaronnote-roam-ui-refresh-function
              #'my/aaronnote-roam-new-refresh)
  (setq-local widget-button-face 'my/aaronnote-roam-ui-action)
  (setq-local widget-field-face 'my/aaronnote-roam-ui-row-title)
  (my/aaronnote-roam-ui-set-header "Roam New" 'new "draft"))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/aaronnote-roam-new-mode 'emacs))

(defun my/aaronnote-roam-new--normalize-directory (directory)
  "Return DIRECTORY as a clean vault-relative directory."
  (let ((directory
         (replace-regexp-in-string
          "/+\\'" ""
          (replace-regexp-in-string
           "\\`\\./" ""
           (string-trim (or directory ""))))))
    (if (member directory '("" "." "Root")) "" directory)))

(defun my/aaronnote-roam-new--default-path (title &optional directory)
  "Return the default Markdown path for TITLE in DIRECTORY."
  (let ((directory (my/aaronnote-roam-new--normalize-directory directory))
        (name (concat (my/aaronnote-roam--slugify-title title) ".md")))
    (if (string-empty-p directory)
        name
      (concat directory "/" name))))

(defun my/aaronnote-roam-new--path-file (path)
  "Return absolute file for vault-relative PATH."
  (expand-file-name path (file-name-as-directory (my/aaronnote-roam-root))))

(defun my/aaronnote-roam-new--unique-path (path)
  "Return PATH, or a numbered variant, that does not already exist."
  (let* ((path (string-trim (or path "")))
         (dir (or (file-name-directory path) ""))
         (base (file-name-base path))
         (ext (or (file-name-extension path t) ".md"))
         (candidate path)
         (n 2))
    (while (and (not (string-empty-p candidate))
                (file-exists-p
                 (my/aaronnote-roam-new--path-file candidate)))
      (setq candidate (concat dir base "-" (number-to-string n) ext)
            n (1+ n)))
    candidate))

(defun my/aaronnote-roam-new--normalize-tags (tags)
  "Return TAGS as a clean string list."
  (delete-dups
   (seq-filter
    (lambda (tag) (not (string-empty-p tag)))
    (mapcar #'string-trim
            (if (listp tags)
                tags
              (split-string (or tags "") "," t))))))

(defun my/aaronnote-roam-new--default-draft (&optional directory)
  "Return a default Roam New draft rooted in DIRECTORY."
  (list :node-type "roam"
        :title "Untitled"
        :path (my/aaronnote-roam-new--unique-path
               (my/aaronnote-roam-new--default-path "Untitled" directory))
        :kind "note"
        :template-key "roam"
        :tags nil))

(defun my/aaronnote-roam-new--draft-for-create (draft &optional directory)
  "Return DRAFT normalized like Aaronnote New before creation.
Empty title, path, and kind fields receive the same defaults as Aaronnote's
New Note form.  DIRECTORY defaults to the current Roam New base directory."
  (let* ((node-type
          (if (string= (downcase (format "%s" (plist-get draft :node-type)))
                       "regular")
              "regular"
            "roam"))
         (title (string-trim (or (plist-get draft :title) "")))
         (title (if (string-empty-p title) "Untitled" title))
         (base-directory
          (or directory my/aaronnote-roam-new--base-directory))
         (raw-path (string-trim (or (plist-get draft :path) "")))
         (untitled-path
          (my/aaronnote-roam-new--default-path "Untitled" base-directory))
         (path (if (or (string-empty-p raw-path)
                       (and (not (string= title "Untitled"))
                            (equal raw-path untitled-path)))
                   (my/aaronnote-roam-new--default-path title base-directory)
                 raw-path))
         (path (my/aaronnote-roam-new--unique-path path))
         (kind (string-trim (or (plist-get draft :kind) "")))
         (kind (if (string-empty-p kind)
                   (if (string= node-type "roam") "note" "default")
                 kind))
         (normalized (copy-sequence draft)))
    (setq normalized (plist-put normalized :node-type node-type)
          normalized (plist-put normalized :title title)
          normalized (plist-put normalized :path path)
          normalized (plist-put normalized :kind kind)
          normalized (plist-put normalized :tags
                                (my/aaronnote-roam-new--normalize-tags
                                 (plist-get draft :tags))))
    normalized))

(defun my/aaronnote-roam-new--template-field (template field)
  "Return FIELD from TEMPLATE, which may be a hash table or plist."
  (if (hash-table-p template)
      (gethash (substring (symbol-name field) 1) template)
    (plist-get template field)))

(defun my/aaronnote-roam-new--load-templates ()
  "Return templates reported by the Aaronnote runtime."
  (let ((response (my/aaronnote-roam--runtime-call "templates" "--force")))
    (or (and response (gethash "templates" response))
        '((:key "basic" :name "Basic Markdown note")
          (:key "daily" :name "Daily note")
          (:key "decision" :name "Decision record")
          (:key "meeting" :name "Meeting notes")
          (:key "project" :name "Project brief")
          (:key "reading" :name "Reading notes")
          (:key "roam" :name "Roam note")
          (:key "task-plan" :name "Task plan")
          (:key "weekly-review" :name "Weekly review")
          (:key "zettel" :name "Zettel")))))

(defun my/aaronnote-roam-new--template-label (key)
  "Return a display label for template KEY."
  (if (string-empty-p (or key ""))
      "None"
    (let ((template
           (seq-find
            (lambda (candidate)
              (equal (my/aaronnote-roam-new--template-field candidate :key)
                     key))
            my/aaronnote-roam-new--templates)))
      (or (and template
               (my/aaronnote-roam-new--template-field template :name))
          key))))

(defun my/aaronnote-roam-new--template-candidates ()
  "Return display-name and key pairs for available templates."
  (let* ((active-kind (plist-get my/aaronnote-roam-new--draft :kind))
         (templates
          (seq-filter
           (lambda (template)
             (let ((kind
                    (my/aaronnote-roam-new--template-field template :kind)))
               (or (string-empty-p (or kind ""))
                   (equal kind active-kind))))
           my/aaronnote-roam-new--templates)))
    (cons
     '("None" . "")
     (mapcar
      (lambda (template)
        (let ((key (my/aaronnote-roam-new--template-field template :key))
              (name (my/aaronnote-roam-new--template-field template :name))
              (kind (my/aaronnote-roam-new--template-field template :kind)))
          (cons (format "%s%s"
                        (or name key "Template")
                        (if (string-empty-p (or kind ""))
                            ""
                          (format " (%s)" kind)))
                key)))
      templates))))

(defun my/aaronnote-roam-new--path-suggestions ()
  "Return vault-relative directory suggestions for Roam New."
  (let ((root (file-name-as-directory (my/aaronnote-roam-root)))
        (directories '("")))
    (dolist (record (my/aaronnote-roam--note-records))
      (when-let* ((file (plist-get record :file))
                  ((file-name-absolute-p file))
                  (relative (file-relative-name file root))
                  (directory (file-name-directory relative)))
        (push directory directories)))
    (sort (delete-dups directories) #'string<)))

(defun my/aaronnote-roam-new--tag-suggestions ()
  "Return known roam tags for Roam New."
  (sort
   (delete-dups
    (apply #'append
           (mapcar
            (lambda (record)
              (my/aaronnote-roam--note-list-field
               (plist-get record :note) "tags"))
            (my/aaronnote-roam--note-records))))
   #'string<))

(defun my/aaronnote-roam-new--kind-suggestions ()
  "Return known note kinds for Roam New."
  (sort
   (delete-dups
    (cons "note"
          (cons "default"
                (delq nil
                      (mapcar
                       (lambda (record)
                         (my/aaronnote-roam--note-field
                          (plist-get record :note) "kind"))
                       (my/aaronnote-roam--note-records))))))
   #'string<))

(defun my/aaronnote-roam-new--set (key value)
  "Set draft KEY to VALUE and rerender the Roam New buffer."
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (setq-local my/aaronnote-roam-new--draft
              (plist-put my/aaronnote-roam-new--draft key value))
  (my/aaronnote-roam-new-render))

(defun my/aaronnote-roam-new--plain-widget-value (key)
  "Return editable widget KEY's plain string value, or nil."
  (when-let* ((widget (alist-get key my/aaronnote-roam-new--widgets)))
    (substring-no-properties (format "%s" (widget-value widget)))))

(defun my/aaronnote-roam-new--sync-draft-from-widgets ()
  "Copy editable field widget values into the current Roam New draft."
  (when (and my/aaronnote-roam-new--widgets my/aaronnote-roam-new--draft)
    (let ((draft my/aaronnote-roam-new--draft))
      (dolist (entry '((:title . title)
                       (:path . path)
                       (:kind . kind)))
        (when-let* ((value (my/aaronnote-roam-new--plain-widget-value
                            (cdr entry))))
          (setq draft (plist-put draft (car entry) value))))
      (when-let* ((tags (my/aaronnote-roam-new--plain-widget-value 'tags)))
        (setq draft
              (plist-put draft :tags
                         (my/aaronnote-roam-new--normalize-tags tags))))
      (setq-local my/aaronnote-roam-new--draft draft))))

(defun my/aaronnote-roam-new-edit-type ()
  "Edit the note type in the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((old (plist-get my/aaronnote-roam-new--draft :node-type))
         (next (completing-read "Note type: " '("roam" "regular")
                                nil t nil nil old))
         (template (plist-get my/aaronnote-roam-new--draft :template-key))
         (kind (plist-get my/aaronnote-roam-new--draft :kind)))
    (setq-local my/aaronnote-roam-new--draft
                (plist-put my/aaronnote-roam-new--draft :node-type next))
    (when (member template '("roam" "basic"))
      (setq-local my/aaronnote-roam-new--draft
                  (plist-put my/aaronnote-roam-new--draft :template-key
                             (if (string= next "roam") "roam" "basic"))))
    (when (member kind '("note" "default"))
      (setq-local my/aaronnote-roam-new--draft
                  (plist-put my/aaronnote-roam-new--draft :kind
                             (if (string= next "roam") "note" "default"))))
    (my/aaronnote-roam-new-render)))

(defun my/aaronnote-roam-new-edit-title ()
  "Edit the title in the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((old-title (plist-get my/aaronnote-roam-new--draft :title))
         (old-default
          (my/aaronnote-roam-new--default-path
           old-title my/aaronnote-roam-new--base-directory))
         (title (read-string "Title: " old-title)))
    (setq-local my/aaronnote-roam-new--draft
                (plist-put my/aaronnote-roam-new--draft :title title))
    (when (equal (plist-get my/aaronnote-roam-new--draft :path) old-default)
      (setq-local my/aaronnote-roam-new--draft
                  (plist-put
                   my/aaronnote-roam-new--draft :path
                   (my/aaronnote-roam-new--default-path
                    title my/aaronnote-roam-new--base-directory))))
    (my/aaronnote-roam-new-render)))

(defun my/aaronnote-roam-new-edit-path ()
  "Edit the save path in the current Roam New draft using file completion."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((root (file-name-as-directory (expand-file-name (my/aaronnote-roam-root))))
         (current (plist-get my/aaronnote-roam-new--draft :path))
         (abs (expand-file-name (or current "") root))
         (raw (read-file-name "Save path: " root abs nil current)))
    (my/aaronnote-roam-new--set
     :path (file-relative-name (expand-file-name raw root) root))))

(defun my/aaronnote-roam-new-edit-kind ()
  "Edit the note kind in the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--set
   :kind
   (completing-read "Kind: " (my/aaronnote-roam-new--kind-suggestions)
                    nil nil nil nil
                    (plist-get my/aaronnote-roam-new--draft :kind))))

(defun my/aaronnote-roam-new-edit-template ()
  "Edit the template in the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((candidates (my/aaronnote-roam-new--template-candidates))
         (current (my/aaronnote-roam-new--template-label
                   (plist-get my/aaronnote-roam-new--draft :template-key)))
         (choice (completing-read "Template: " candidates nil t nil nil current)))
    (my/aaronnote-roam-new--set :template-key
                                (or (cdr (assoc choice candidates)) ""))))

(defun my/aaronnote-roam-new-edit-tags ()
  "Edit tags in the current Roam New draft, adding one at a time."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((suggestions (my/aaronnote-roam-new--tag-suggestions))
         (tags (copy-sequence
                (or (plist-get my/aaronnote-roam-new--draft :tags) nil))))
    (catch 'done
      (while t
        (let* ((status (if tags
                           (concat " [" (string-join tags ", ") "]")
                         ""))
               (input (string-trim
                       (completing-read
                        (format "Add tag%s (empty to finish): " status)
                        suggestions nil nil))))
          (if (string-empty-p input)
              (throw 'done nil)
            (unless (member input tags)
              (setq tags (append tags (list input))))))))
    (my/aaronnote-roam-new--set
     :tags (my/aaronnote-roam-new--normalize-tags tags))))

(defun my/aaronnote-roam-new--insert-field
    (id icon label value detail action &optional tone)
  "Insert one selectable Roam New field."
  (my/aaronnote-roam-ui-insert-row
   :id id
   :icon icon
   :badge label
   :badge-tone (or tone 'muted)
   :title (if (and value (not (string-empty-p (format "%s" value))))
              (format "%s" value)
            "None")
   :detail detail
   :action (lambda (_ignored) (call-interactively action))
   :help (format "RET/mouse-1: edit %s" (downcase label))))

(defun my/aaronnote-roam-new--editable-width ()
  "Return a reasonable width for Roam New editable fields."
  (max 24 (min 72 (- (window-width) 32))))

(defun my/aaronnote-roam-new--insert-editable-field
    (id icon label value detail key &optional placeholder)
  "Insert directly editable Roam New field KEY.
ID, ICON, LABEL, VALUE, DETAIL, and PLACEHOLDER control display."
  (let ((start (point))
        (value (or value "")))
    (insert "   "
            (propertize (my/aaronnote-roam-ui-icon icon)
                        'face 'my/aaronnote-roam-ui-icon)
            "  ")
    (my/aaronnote-roam-ui-insert-badge label 'muted)
    (insert "  ")
    (let* ((label-end (point))
           (widget
            (widget-create
             'editable-field
             :size (my/aaronnote-roam-new--editable-width)
             :format "%v"
             :help-echo (format "Edit %s directly" (downcase label))
             :notify
             (lambda (_widget &rest _ignored)
               (my/aaronnote-roam-new--sync-draft-from-widgets))
             (if (string-empty-p value) (or placeholder "") value))))
      (push (cons key widget) my/aaronnote-roam-new--widgets)
      (insert "\n")
      (when (and detail (not (string-empty-p detail)))
        (insert "      "
                (propertize detail 'face 'my/aaronnote-roam-ui-detail)
                "\n"))
      (let ((end (point))
            (action (let ((w widget))
                      (lambda (_)
                        (when-let* ((marker (widget-get w :from)))
                          (goto-char marker))))))
        (add-text-properties
         start end
         `(my/aaronnote-roam-ui-item-id ,id
           help-echo ,(format "RET: jump into %s field; type to edit" (downcase label))))
        ;; Apply row-action only to the label area so the widget's own keymap is not masked.
        (add-text-properties
         start label-end
         `(my/aaronnote-roam-ui-row-action ,action
           mouse-face my/aaronnote-roam-ui-row-highlight
           keymap ,my/aaronnote-roam-ui-row-map))))))

(defun my/aaronnote-roam-new-render ()
  "Render the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  ;; Delete stale widget registrations before erasing; otherwise widget-setup
  ;; sees both old and new fields and raises "Overlapping fields".
  (dolist (entry my/aaronnote-roam-new--widgets)
    (condition-case nil (widget-delete (cdr entry)) (error nil)))
  (setq-local my/aaronnote-roam-new--widgets nil)
  (let* ((draft my/aaronnote-roam-new--draft)
         (node-type (plist-get draft :node-type))
         (title (plist-get draft :title))
         (path (plist-get draft :path))
         (kind (plist-get draft :kind))
         (template-key (plist-get draft :template-key))
         (template-label (my/aaronnote-roam-new--template-label template-key))
         (tags (plist-get draft :tags))
         (id (plist-get draft :id)))
    (my/aaronnote-roam-ui-set-header
     "Roam New" 'new (format "%s draft" node-type))
    (my/aaronnote-roam-ui-render
     (lambda ()
       (setq-local my/aaronnote-roam-new--widgets nil)
       (my/aaronnote-roam-ui-insert-page-header
        "New note"
        :icon 'new
        :subtitle "Type in fields; p for path; a to add tags one by one; c to create"
        :stats (list (cons (upcase node-type)
                           (if (string= node-type "roam") 'info 'muted))
                     (cons template-label 'muted))
        :actions
        '((:label "c Create"
           :command my/aaronnote-roam-new-create
           :help "Create this note through the Aaronnote runtime"
           :primary t)
          (:label "t Type"
           :command my/aaronnote-roam-new-edit-type
           :help "Switch between roam and regular Markdown notes")
          (:label "T Template"
           :command my/aaronnote-roam-new-edit-template
           :help "Choose a Markdown template")
          (:label "p Path"
           :command my/aaronnote-roam-new-edit-path
           :help "Choose save path with file completion")
          (:label "a Tags"
           :command my/aaronnote-roam-new-edit-tags
           :help "Add tags one by one with vault completion")
          (:label "R Reset"
           :command my/aaronnote-roam-new-reset
           :help "Reset this draft")
          (:label "q Close"
           :command quit-window
           :help "Close without creating")))
       (my/aaronnote-roam-ui-insert-section "Draft" 6)
       (my/aaronnote-roam-new--insert-field
        'type 'status "TYPE" node-type
        "Press RET or t to switch roam / regular."
        #'my/aaronnote-roam-new-edit-type
        (if (string= node-type "roam") 'info 'muted))
       (my/aaronnote-roam-new--insert-editable-field
        'title 'note "TITLE" title
        "Used for the heading, metadata, and default save path."
        'title "Untitled")
       (my/aaronnote-roam-new--insert-editable-field
        'path 'path "SAVE PATH" path
        "Vault-relative .md or .markdown path; p chooses with file completion."
        'path "untitled.md")
       (my/aaronnote-roam-new--insert-editable-field
        'kind 'status "KIND" kind
        "Controls Aaronnote note-kind behavior."
        'kind (if (string= node-type "roam") "note" "default"))
       (my/aaronnote-roam-new--insert-field
        'template 'template "TEMPLATE" template-label
        "Press RET or T to choose a template."
        #'my/aaronnote-roam-new-edit-template)
       (my/aaronnote-roam-new--insert-editable-field
        'tags 'tag "TAGS"
        (if tags (string-join tags ", ") nil)
        "Comma-separated graph tags; a adds with completion."
        'tags "")
       (insert "\n")
       (my/aaronnote-roam-ui-insert-section "Result")
       (my/aaronnote-roam-ui-insert-field
        "Node ID" (or id (if (string= node-type "roam")
                             "timestamped on create"
                           "none")))
       (my/aaronnote-roam-ui-insert-field
        "Absolute path"
        (abbreviate-file-name
         (expand-file-name path (my/aaronnote-roam-root)))
        'my/aaronnote-roam-ui-path)
       (my/aaronnote-roam-ui-insert-field
        "Create engine" "Aaronnote runtime" 'my/aaronnote-roam-ui-meta)
       (widget-setup)))
    (unless (get-text-property (point) 'my/aaronnote-roam-ui-item-id)
      (my/aaronnote-roam-ui-goto-first-item))))

(defun my/aaronnote-roam-new-refresh ()
  "Reload templates and rerender the current Roam New draft."
  (interactive)
  (setq-local my/aaronnote-roam-new--templates
              (my/aaronnote-roam-new--load-templates))
  (my/aaronnote-roam-new-render))

(defun my/aaronnote-roam-new-reset ()
  "Reset the current Roam New draft."
  (interactive)
  (setq-local my/aaronnote-roam-new--draft
              (my/aaronnote-roam-new--default-draft
               my/aaronnote-roam-new--base-directory))
  (my/aaronnote-roam-new-render))

(defun my/aaronnote-roam-new (&optional base-directory draft)
  "Open the native Roam New workbench.
BASE-DIRECTORY is vault-relative.  DRAFT overrides the initial draft plist."
  (interactive)
  (let* ((base-directory
          (my/aaronnote-roam-new--normalize-directory
           (or base-directory
               (when-let* ((file buffer-file-name)
                           ((file-in-directory-p file
                                                 (my/aaronnote-roam-root))))
                 (file-relative-name
                  (file-name-directory file)
                  (my/aaronnote-roam-root)))
               "")))
         (buffer (get-buffer-create "*roam-new*")))
    (with-current-buffer buffer
      (my/aaronnote-roam-new-mode)
      (setq-local my/aaronnote-roam-new--base-directory base-directory
                  my/aaronnote-roam-new--templates
                  (my/aaronnote-roam-new--load-templates)
                  my/aaronnote-roam-new--draft
                  (or draft
                      (my/aaronnote-roam-new--default-draft base-directory)))
      (my/aaronnote-roam-new-render))
    (pop-to-buffer buffer)))

(defun my/aaronnote-roam-new--payload (draft)
  "Return Aaronnote runtime JSON payload for DRAFT."
  (delq nil
        `((nodeType . ,(plist-get draft :node-type))
          (title . ,(plist-get draft :title))
          (path . ,(plist-get draft :path))
          (kind . ,(plist-get draft :kind))
          (templateKey . ,(or (plist-get draft :template-key) ""))
          (tags . ,(vconcat (plist-get draft :tags)))
          ,(when-let* ((id (plist-get draft :id)))
             `(id . ,id)))))

(defun my/aaronnote-roam-new--create-draft (draft)
  "Create and open DRAFT through the Aaronnote runtime."
  (let* ((draft (my/aaronnote-roam-new--draft-for-create draft))
         (payload (my/aaronnote-roam-new--payload draft))
         (json (json-encode payload))
         (response (my/aaronnote-roam--runtime-call "create" "--json" json))
         (file (and response (gethash "file" response))))
    (unless response
      (user-error "Aaronnote runtime failed — see *Messages* for details"))
    (unless (and file (file-exists-p file))
      (user-error "Aaronnote runtime did not create the note (path: %s)"
                  (or file "nil")))
    (my/aaronnote-roam--clear-runtime-cache)
    (when (derived-mode-p 'my/aaronnote-roam-new-mode)
      (kill-buffer (current-buffer)))
    (if (fboundp 'my/aaronnote-open-file)
        (my/aaronnote-open-file file)
      (find-file file))
    file))

(defun my/aaronnote-roam-new-create ()
  "Create the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (my/aaronnote-roam-new--create-draft my/aaronnote-roam-new--draft))

(defun my/aaronnote-roam-new-note (&optional slug title tags)
  "Create a roam note, or open Roam New when called interactively.
Non-interactive SLUG, TITLE, and TAGS calls preserve compatibility with link
creation commands while using the Aaronnote runtime."
  (interactive)
  (if (called-interactively-p 'interactive)
      (my/aaronnote-roam-new)
    (let* ((slug (my/aaronnote-roam--strip-vault-prefix (or slug "")))
           (path (if (my/aaronnote-roam--ref-has-extension-p slug)
                     slug
                   (concat slug ".md")))
           (id (file-name-sans-extension slug))
           (title
            (or title
                (capitalize
                 (replace-regexp-in-string
                  "[-_/]" " " (file-name-nondirectory id))))))
      (my/aaronnote-roam-new--create-draft
       (list :node-type "roam"
             :id id
             :title title
             :path path
             :kind "note"
             :template-key "roam"
             :tags (my/aaronnote-roam-new--normalize-tags tags))))))

(defun my/aaronnote-roam-new-node (&optional title directory)
  "Open Roam New, or create a timestamped node from TITLE in DIRECTORY."
  (interactive)
  (if (called-interactively-p 'interactive)
      (my/aaronnote-roam-new directory)
    (let* ((title (or title "Untitled"))
           (directory (my/aaronnote-roam-new--normalize-directory directory))
           (id (format "%s-%s"
                       (my/aaronnote-roam--timestamp-id)
                       (my/aaronnote-roam--slugify-title title)))
           (path (if (string-empty-p directory)
                     (concat id ".md")
                   (concat directory "/" id ".md"))))
      (my/aaronnote-roam-new--create-draft
       (list :node-type "roam"
             :id id
             :title title
             :path path
             :kind "note"
             :template-key "roam"
             :tags nil)))))

;; ── Roam DB ──────────────────────────────────────────────────────────────────

(defvar my/aaronnote-roam--db-cache nil)
(defvar my/aaronnote-roam--db-path-cache nil)
(defvar my/aaronnote-roam--db-mtime nil)
(defvar my/aaronnote-roam--scan-cache nil)

(defun my/aaronnote-roam--db-path ()
  "Return path to an optional Markdown roam-db.json for the current vault."
  (let* ((root (my/aaronnote-roam-root))
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

(defun my/aaronnote-roam--db ()
  "Return the parsed roam-db.json, refreshing cache when the file changes."
  (let ((path (my/aaronnote-roam--db-path)))
    (when (file-exists-p path)
      (let ((mtime (file-attribute-modification-time (file-attributes path))))
        (when (or (not my/aaronnote-roam--db-cache)
                  (not (equal path my/aaronnote-roam--db-path-cache))
                  (time-less-p my/aaronnote-roam--db-mtime mtime))
          (setq my/aaronnote-roam--db-cache
                (with-temp-buffer
                  (insert-file-contents path)
                  (json-parse-buffer :object-type 'hash-table :array-type 'list))
                my/aaronnote-roam--db-path-cache path
                my/aaronnote-roam--db-mtime mtime))))
    my/aaronnote-roam--db-cache))

(defun my/aaronnote-roam--db-notes ()
  "Return the DB notes hash table, or nil."
  (when-let* ((db (my/aaronnote-roam--db)))
    (gethash "notes" db)))

(defun my/aaronnote-roam--note-field (note key)
  "Return string field KEY from NOTE."
  (when (hash-table-p note)
    (let ((value (gethash key note)))
      (when (and (stringp value) (not (string-empty-p value)))
        value))))

(defun my/aaronnote-roam--note-list-field (note key)
  "Return list field KEY from NOTE."
  (let ((value (and (hash-table-p note) (gethash key note))))
    (cond
     ((listp value) value)
     ((vectorp value) (append value nil))
     ((and (stringp value) (not (string-empty-p value))) (list value)))))

(defun my/aaronnote-roam--split-list-value (value)
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

(defun my/aaronnote-roam--put-note-field (note key value)
  "Set NOTE KEY to VALUE when VALUE is present."
  (when (and value
             (not (and (stringp value) (string-empty-p (string-trim value)))))
    (puthash key value note)))

(defun my/aaronnote-roam--parse-meta-line (note line)
  "Parse one KEY: VALUE metadata LINE into NOTE."
  (when (string-match "\\`\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (let* ((key (downcase (string-trim (match-string 1 line))))
           (value (string-trim (match-string 2 line))))
      (pcase key
        ((or "tags" "aliases" "refs" "links" "backlinks" "inlinetags")
         (my/aaronnote-roam--put-note-field
          note key (my/aaronnote-roam--split-list-value value)))
        (_
         (my/aaronnote-roam--put-note-field note key value))))))

(defun my/aaronnote-roam--read-org-meta-block (note)
  "Read an Aaronnote `#+begin meta' block at point into NOTE."
  (when (looking-at-p "\\s-*#\\+begin meta\\b")
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at-p "\\s-*#\\+end meta\\b")))
      (my/aaronnote-roam--parse-meta-line
       note
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (forward-line 1))
    t))

(defun my/aaronnote-roam--read-yaml-frontmatter (note)
  "Read simple YAML frontmatter at point into NOTE."
  (when (looking-at-p "\\s-*---\\s-*$")
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at-p "\\s-*---\\s-*$")))
      (my/aaronnote-roam--parse-meta-line
       note
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (forward-line 1))
    t))

(defun my/aaronnote-roam--extract-summary-block ()
  "Return the first `#+begin summary' block text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin summary\\b.*$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^#\\+end summary\\b" nil t)
          (string-trim
           (buffer-substring-no-properties start (match-beginning 0))))))))

(defun my/aaronnote-roam--internal-target-p (target)
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

(defun my/aaronnote-roam--extract-links-from-buffer ()
  "Return Markdown roam references from the current buffer."
  (let (links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]\n]+\\)\\]\\]" nil t)
        (push (concat "roam://"
                      (my/aaronnote-roam--encode-ref
                       (string-trim (match-string 1))))
              links))
      (goto-char (point-min))
      (while (re-search-forward "\\(!?\\)\\[[^]\n]*\\](\\([^)\n]+\\))" nil t)
        (unless (equal (match-string 1) "!")
          (let ((href (string-trim (match-string 2))))
            (when (my/aaronnote-roam--internal-target-p href)
              (push href links)))))
      (goto-char (point-min))
      (while (re-search-forward "\\_<roam://[^][<>()[:space:]]+" nil t)
        (push (match-string 0) links)))
    (delete-dups (nreverse links))))

(defun my/aaronnote-roam--first-markdown-heading ()
  "Return the first Markdown heading text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\{1,6\\}[ \t]+\\(.+?\\)\\(?:[ \t]+{#[[:alnum:]_:-]+}\\)?[ \t]*$" nil t)
      (string-trim
       (replace-regexp-in-string
        "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
        (match-string 1))))))

(defun my/aaronnote-roam--scan-note-file (file)
  "Return a note hash table by scanning Markdown FILE."
  (let* ((root (my/aaronnote-roam-root))
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
        (my/aaronnote-roam--read-org-meta-block note))
       ((looking-at-p "\\s-*---\\s-*$")
        (my/aaronnote-roam--read-yaml-frontmatter note)))
      (my/aaronnote-roam--put-note-field
       note "title"
       (or (my/aaronnote-roam--note-field note "title")
           (my/aaronnote-roam--first-markdown-heading)
           (file-name-base file)))
      (my/aaronnote-roam--put-note-field
       note "id"
       (or (my/aaronnote-roam--note-field note "id")
           (my/aaronnote-roam--path-without-note-extension rel)))
      (my/aaronnote-roam--put-note-field
       note "summary"
       (my/aaronnote-roam--extract-summary-block))
      (my/aaronnote-roam--put-note-field
       note "links"
       (append (my/aaronnote-roam--note-list-field note "refs")
               (my/aaronnote-roam--extract-links-from-buffer))))
    note))

(defun my/aaronnote-roam--canonical-note-id (key note)
  "Return Aaronnote's canonical note id for NOTE with DB KEY."
  (or (my/aaronnote-roam--note-field note "id")
      (my/aaronnote-roam--note-field note "key")
      (my/aaronnote-roam--note-field note "source")
      (my/aaronnote-roam--note-field note "path")
      (my/aaronnote-roam--note-field note "link")
      (my/aaronnote-roam--note-field note "file")
      key))

(defun my/aaronnote-roam--note-file-from-fields (key note)
  "Return the best note file path for DB KEY and NOTE."
  (let* ((root (my/aaronnote-roam-root))
         (raw-value (or (my/aaronnote-roam--note-field note "file")
                        (my/aaronnote-roam--note-field note "path")
                        (my/aaronnote-roam--note-field note "link")
                        (my/aaronnote-roam--note-field note "source")
                        key))
         (raw (if (and raw-value (file-name-absolute-p raw-value))
                  raw-value
                (my/aaronnote-roam--strip-vault-prefix raw-value)))
         (path (and raw
                    (if (file-name-absolute-p raw)
                        raw
                      (expand-file-name raw root)))))
    (cond
     ((and path (file-exists-p path)) path)
     ((and path raw (not (my/aaronnote-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".md")))
      (concat path ".md"))
     ((and path raw (not (my/aaronnote-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".markdown")))
      (concat path ".markdown"))
     (path path))))

(defun my/aaronnote-roam--note-search-values (key note)
  "Return Aaronnote-style searchable values for NOTE with DB KEY."
  (let* ((file (my/aaronnote-roam--note-field note "file"))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/aaronnote-roam-root))))
         (values (append
                  (list key
                        (my/aaronnote-roam--canonical-note-id key note)
                        (my/aaronnote-roam--note-field note "id")
                        (my/aaronnote-roam--note-field note "key")
                        (my/aaronnote-roam--note-field note "title")
                        (my/aaronnote-roam--note-field note "path")
                        (my/aaronnote-roam--note-field note "link")
                        (my/aaronnote-roam--note-field note "source")
                        file
                        rel-file
                        (and rel-file
                             (my/aaronnote-roam--path-without-note-extension rel-file))
                        (and rel-file (concat "roam/" rel-file))
                        (and rel-file
                             (concat "roam/"
                                     (my/aaronnote-roam--path-without-note-extension
                                      rel-file))))
                  (my/aaronnote-roam--note-list-field note "aliases")
                  (my/aaronnote-roam--note-list-field note "tags"))))
    (delete-dups
     (seq-filter (lambda (value)
                   (and (stringp value) (not (string-empty-p value))))
                 values))))

(defun my/aaronnote-roam--scanned-note-records ()
  "Return cached note records by scanning Markdown files."
  (or my/aaronnote-roam--scan-cache
      (setq my/aaronnote-roam--scan-cache
            (mapcar (lambda (file)
                      (let* ((note (my/aaronnote-roam--scan-note-file file))
                             (key (my/aaronnote-roam--file-to-slug file))
                             (id (my/aaronnote-roam--canonical-note-id key note)))
                        (list :key key
                              :id id
                              :note note
                              :file file
                              :title (or (my/aaronnote-roam--note-field note "title")
                                         id)
                              :values (my/aaronnote-roam--note-search-values
                                       key note))))
                    (my/aaronnote-roam--all-files)))))

(defun my/aaronnote-roam--runtime-note-records ()
  "Return note records from the vendored Aaronnote runtime."
  (when-let* ((payload (my/aaronnote-roam--runtime-index))
              (notes (gethash "notes" payload)))
    (mapcar (lambda (note)
              (let* ((key (or (my/aaronnote-roam--note-field note "key")
                              (my/aaronnote-roam--note-field note "id")
                              (my/aaronnote-roam--note-field note "path")
                              (my/aaronnote-roam--note-field note "link")))
                     (id (my/aaronnote-roam--canonical-note-id key note)))
                (list :key key
                      :id id
                      :note note
                      :file (my/aaronnote-roam--note-file-from-fields key note)
                      :title (or (my/aaronnote-roam--note-field note "title") id)
                      :values (my/aaronnote-roam--note-search-values key note))))
            notes)))

(defun my/aaronnote-roam--note-records ()
  "Return note records with :key, :id, :note, :file, :title, and :values."
  (or (my/aaronnote-roam--runtime-note-records)
      (if-let* ((notes (my/aaronnote-roam--db-notes)))
          (let (records)
            (maphash
             (lambda (key note)
               (let ((id (my/aaronnote-roam--canonical-note-id key note)))
                 (push (list :key key
                             :id id
                             :note note
                             :file (my/aaronnote-roam--note-file-from-fields key note)
                             :title (or (my/aaronnote-roam--note-field note "title") id)
                             :values (my/aaronnote-roam--note-search-values key note))
                       records)))
             notes)
            (nreverse records))
        (my/aaronnote-roam--scanned-note-records))))

(defun my/aaronnote-roam--target-note-ref (target)
  "Return the note ref portion of TARGET."
  (plist-get (my/aaronnote-roam--split-target target) :ref))

(defun my/aaronnote-roam--resolve-note (ref)
  "Resolve REF to an Aaronnote note record plist.
Exact id/key/path/title/alias/tag matches win first; substring matches are
accepted as a fallback, matching Aaronnote search behavior."
  (let* ((clean (or (my/aaronnote-roam--target-note-ref ref) ref))
         (clean (string-trim (or clean "")))
         (query (downcase clean))
         (records (my/aaronnote-roam--note-records)))
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

(defun my/aaronnote-roam--db-note (slug)
  "Return the DB hash-table for SLUG/id/path, or nil."
  (plist-get (my/aaronnote-roam--resolve-note slug) :note))

(defun my/aaronnote-roam--target-slug (target)
  "Return normalized canonical note id from a note-link TARGET."
  (plist-get (my/aaronnote-roam--parse-target target) :slug))

(defun my/aaronnote-roam--db-backlinks-to (slug)
  "Return DB backlinks to SLUG/id, normalizing Aaronnote targets."
  (when-let* ((target-id (or (plist-get (my/aaronnote-roam--resolve-note slug) :id)
                             slug)))
    (or (when-let* ((note (my/aaronnote-roam--db-note target-id)))
          (my/aaronnote-roam--note-list-field note "backlinks"))
        (let (backlinks)
          (dolist (record (my/aaronnote-roam--note-records))
            (let* ((note (plist-get record :note))
                   (source (plist-get record :key))
                   (links (or (my/aaronnote-roam--note-list-field note "links")
                              (my/aaronnote-roam--note-list-field note "refs"))))
              (when (member target-id
                            (mapcar #'my/aaronnote-roam--target-slug links))
                (push (my/aaronnote-roam--canonical-note-id source note) backlinks))))
          (delete-dups (nreverse backlinks))))))

(defun my/aaronnote-roam--current-slug ()
  "Return the canonical roam id for the current buffer, or nil."
  (when buffer-file-name
    (my/aaronnote-roam--file-to-note-id buffer-file-name)))

;; ── Tag ids and TOC ───────────────────────────────────────────────────────────

(defun my/aaronnote-roam--slugify-tag-id (text)
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

(defun my/aaronnote-roam--tag-id-exists-p (id)
  "Return non-nil when ID already exists in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward (format "{#%s}" (regexp-quote id)) nil t)
        (progn
          (goto-char (point-min))
          (re-search-forward (format "<%s>" (regexp-quote id)) nil t)))))

(defun my/aaronnote-roam--unique-tag-id (base)
  "Return BASE or BASE-N so it is unique in the current buffer."
  (let ((candidate base)
        (n 2))
    (while (my/aaronnote-roam--tag-id-exists-p candidate)
      (setq candidate (format "%s-%d" base n)
            n (1+ n)))
    candidate))

(defun my/aaronnote-roam-generate-tag-id (&optional text)
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
         (id (my/aaronnote-roam--unique-tag-id
              (my/aaronnote-roam--slugify-tag-id source))))
    (when (called-interactively-p 'interactive)
      (kill-new id)
      (message "Tag id copied: %s" id))
    id))

(defun my/aaronnote-roam-insert-tag-id (&optional id)
  "Insert or append Markdown heading ID at point.
On a heading line, append `{#id}` unless an id already exists."
  (interactive)
  (let ((id (or id
                (read-string "Tag id: "
                             (my/aaronnote-roam-generate-tag-id)))))
    (save-excursion
      (beginning-of-line)
      (if (looking-at
          "^[ \t]*#\\{1,6\\}[ \t]+.+?\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$")
          (if (match-string 1)
              (user-error "Heading already has id: %s" (match-string 1))
            (end-of-line)
            (insert (format " {#%s}" id)))
        (insert (format "{#%s}" id))))))

(defun my/aaronnote-roam-insert-toc-link ()
  "Open the interactive selector and insert a DOM/TOC note-link."
  (interactive)
  (my/aaronnote-roam-select-link 'toc))

(defun my/aaronnote-roam-insert-tag-id-link ()
  "Open the interactive selector and insert a tag-id note-link."
  (interactive)
  (my/aaronnote-roam-select-link 'tag))

;; ── DB commands ───────────────────────────────────────────────────────────────

(defun my/aaronnote-roam-update-db (&optional full)
  "Refresh Markdown roam cache and sync `roam.db' via Aaronnote runtime.
With prefix argument FULL, force a full roam-db rebuild."
  (interactive "P")
  (my/aaronnote-roam--clear-runtime-cache)
  (if (my/aaronnote-roam--runtime-available-p)
      (my/aaronnote-roam--runtime-sync full nil)
    (message "Markdown roam cache refreshed")))

(defun my/aaronnote-roam--summary-entry-for-slug (slug &optional summaries)
  "Return a note summary entry for SLUG from optional SUMMARIES."
  (or (seq-find (lambda (entry)
                  (equal (plist-get entry :slug) slug))
                (or summaries (my/aaronnote-roam--all-note-summaries)))
      (list :slug slug
            :title (my/aaronnote-roam--note-title slug)
            :tags (my/aaronnote-roam--note-tags slug)
            :summary (my/aaronnote-roam--note-summary slug))))

(defun my/aaronnote-roam-backlinks (&optional target-slug)
  "Show backlinks for the current note in a dedicated buffer."
  (interactive)
  (let* ((slug (or target-slug (my/aaronnote-roam--current-slug)))
         (note (and slug (my/aaronnote-roam--db-note slug)))
         (bls  (or (and slug (my/aaronnote-roam--db-backlinks-to slug))
                   (and note (gethash "backlinks" note))))
         (summaries (my/aaronnote-roam--all-note-summaries))
         (refresh (let ((target slug))
                    (lambda () (my/aaronnote-roam-backlinks target))))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-backlinks*" "Roam Backlinks" 'backlink refresh
               (format "%d backlinks" (length bls)))))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Backlinks"
          :icon 'backlink
          :subtitle (format "References to %s"
                            (or (and note (gethash "title" note)) slug))
          :stats (list (cons (format "%d backlinks" (length bls)) 'info))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Referenced by" (length bls))
         (if (null bls)
             (my/aaronnote-roam-ui-insert-empty
              "No notes currently link to this note.")
           (dolist (bl bls)
             (my/aaronnote-roam--insert-note-button
              (my/aaronnote-roam--summary-entry-for-slug bl summaries)))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-tags ()
  "Browse notes by tag with completion."
  (interactive)
  (let ((tags-ht (make-hash-table :test 'equal)))
    (dolist (record (my/aaronnote-roam--note-records))
      (dolist (tag (my/aaronnote-roam--note-list-field
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
      (my/aaronnote-roam--open-slug slug))))

(defun my/aaronnote-roam--scan-todos ()
  "Return todo hash tables scanned from Markdown notes."
  (let (todos)
    (dolist (record (my/aaronnote-roam--note-records))
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

(defun my/aaronnote-roam--todos ()
  "Return todos from the Aaronnote runtime, roam DB, or local scan."
  (let* ((runtime (my/aaronnote-roam--runtime-call "todos"))
         (runtime-todos (and runtime (gethash "todos" runtime)))
         (runtime-todos (if (hash-table-p runtime-todos)
                            (gethash "todos" runtime-todos)
                          runtime-todos))
         (db (my/aaronnote-roam--db)))
    (or runtime-todos
        (and db (gethash "todos" db))
        (my/aaronnote-roam--scan-todos))))

(defun my/aaronnote-roam--todo-field (entry &rest keys)
  "Return the first non-nil field from todo ENTRY matching KEYS."
  (seq-some
   (lambda (key)
     (cond
      ((hash-table-p entry) (gethash key entry))
      ((plistp entry) (plist-get entry (intern (concat ":" key))))
      ((listp entry)
       (or (cdr (assoc key entry))
           (cdr (assq (intern key) entry))))
      (t nil)))
   keys))

(defun my/aaronnote-roam--todo-status (entry)
  "Return normalized status string for todo ENTRY."
  (downcase (format "%s" (or (my/aaronnote-roam--todo-field entry "status")
                              "todo"))))

(defun my/aaronnote-roam--todo-tone (entry)
  "Return display tone for todo ENTRY."
  (pcase (my/aaronnote-roam--todo-status entry)
    ((or "done" "complete" "completed") 'success)
    ((or "blocked" "cancelled" "canceled") 'danger)
    ((or "doing" "waiting" "in-progress") 'warning)
    (_ 'info)))

(defun my/aaronnote-roam--visit-todo (entry)
  "Open the note and source line represented by todo ENTRY."
  (let ((note-slug (my/aaronnote-roam--todo-field
                    entry "note" "noteId" "noteKey" "path"))
        (line (my/aaronnote-roam--todo-field entry "line")))
    (unless note-slug
      (user-error "Todo has no source note"))
    (my/aaronnote-roam--open-slug note-slug)
    (when (integerp line)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter))))

(defun my/aaronnote-roam--insert-todo-row (entry &optional deadline-tone)
  "Insert a compact task row for ENTRY using optional DEADLINE-TONE."
  (let* ((note-slug (my/aaronnote-roam--todo-field
                     entry "note" "noteId" "noteKey" "path"))
         (note-title (or (my/aaronnote-roam--todo-field
                          entry "title" "noteTitle")
                         note-slug
                         "Unknown note"))
         (text (or (my/aaronnote-roam--todo-field
                    entry "text" "context" "source")
                   "(empty todo)"))
         (line (my/aaronnote-roam--todo-field entry "line"))
         (ddl (my/aaronnote-roam--todo-ddl entry))
         (status (my/aaronnote-roam--todo-status entry))
         (meta (string-join
                (delq nil
                      (list (and ddl (format "DDL %s" ddl))
                            (and (integerp line) (format "line %d" line))))
                "  ·  ")))
    (my/aaronnote-roam-ui-insert-row
     :id (list note-slug line text)
     :icon 'todo
     :badge (upcase status)
     :badge-tone (or deadline-tone (my/aaronnote-roam--todo-tone entry))
     :title text
     :meta meta
     :detail note-title
     :action (let ((todo entry))
               (lambda (_button)
                 (my/aaronnote-roam--visit-todo todo))))))

(defun my/aaronnote-roam-todos ()
  "List all vault todos in a *roam-todos* buffer."
  (interactive)
  (let* ((todos (my/aaronnote-roam--todos))
         (active (seq-count
                  (lambda (entry)
                    (not (member (my/aaronnote-roam--todo-status entry)
                                 '("done" "complete" "completed"
                                   "cancelled" "canceled"))))
                  todos))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-todos*" "Roam Tasks" 'todo
               #'my/aaronnote-roam-todos
               (format "%d tasks" (length todos)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Tasks"
          :icon 'todo
          :subtitle "All indexed Aaronnote Markdown tasks"
          :stats (list (cons (format "%d active" active) 'warning)
                       (cons (format "%d total" (length todos)) 'info))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "All tasks" (length todos))
         (if (null todos)
             (my/aaronnote-roam-ui-insert-empty "No indexed tasks.")
           (dolist (entry todos)
             (my/aaronnote-roam--insert-todo-row entry))))))
    (display-buffer buf)))

;; ── Aaronnote-style note tools ────────────────────────────────────────────────

(defun my/aaronnote-roam--insert-note-button (entry &optional prefix)
  "Insert a clickable note button for summary ENTRY with PREFIX."
  (let* ((slug (plist-get entry :slug))
         (title (or (plist-get entry :title) slug))
         (path (or (plist-get entry :path) slug))
         (tags (plist-get entry :tags))
         (summary (plist-get entry :summary))
         (indent (/ (length (or prefix "")) 2)))
    (my/aaronnote-roam-ui-insert-row
     :id slug
     :icon 'note
     :title title
     :meta path
     :detail summary
     :tags tags
     :indent indent
     :action (let ((target slug))
               (lambda (_button)
                 (my/aaronnote-roam--open-slug target))))))

(defun my/aaronnote-roam--show-note-list
    (title entries &optional empty-text refresh-function icon)
  "Show TITLE and note ENTRIES in a special buffer."
  (let* ((refresh
          (or refresh-function
              (let ((page-title title)
                    (page-entries entries)
                    (page-empty empty-text)
                    (page-icon icon))
                (lambda ()
                  (my/aaronnote-roam--show-note-list
                   page-title page-entries page-empty nil page-icon)))))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*aaronnote-roam-notes*" title (or icon 'note) refresh
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          title
          :icon (or icon 'note)
          :subtitle "Aaronnote Markdown roam notes"
          :stats (list (cons (format "%d notes" (length entries)) 'info))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Notes" (length entries))
         (if (null entries)
             (my/aaronnote-roam-ui-insert-empty
              (or empty-text "No notes."))
           (dolist (entry entries)
             (my/aaronnote-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/aaronnote-roam--show-search-results (query entries)
  "Show note search QUERY and ENTRIES with a live refresh action."
  (my/aaronnote-roam--show-note-list
   (format "Markdown roam search: %s" query)
   entries
   "No matching notes."
   (let ((search-query query))
     (lambda ()
       (my/aaronnote-roam--show-search-results
        search-query
        (my/aaronnote-roam-search-notes search-query))))
   'search))

(defun my/aaronnote-roam-search-notes (&optional query)
  "Search notes by path, title, tag, id, and summary."
  (interactive)
  (let* ((query (or query (read-string "Search notes: ")))
         (parts (split-string (downcase query) "\\s-+" t))
         (entries (seq-filter
                   (lambda (entry)
                     (let ((haystack (downcase
                                      (my/aaronnote-roam--candidate-haystack entry))))
                       (seq-every-p
                        (lambda (part) (string-match-p (regexp-quote part) haystack))
                        parts)))
                   (my/aaronnote-roam--all-note-summaries))))
    (if (called-interactively-p 'interactive)
        (if (= (length entries) 1)
            (my/aaronnote-roam--open-slug (plist-get (car entries) :slug))
          (my/aaronnote-roam--show-search-results query entries))
      entries)))

(defun my/aaronnote-roam-recent-notes ()
  "Show recently opened roam notes."
  (interactive)
  (my/aaronnote-roam--show-note-list
   "Recent Markdown roam notes"
   (seq-filter
    #'identity
    (mapcar (lambda (slug)
              (seq-find (lambda (entry)
                          (equal (plist-get entry :slug) slug))
                        (my/aaronnote-roam--all-note-summaries)))
            (seq-filter (lambda (slug)
                          (file-exists-p (my/aaronnote-roam--slug-to-file slug)))
                        my/aaronnote-roam--recent)))
   "No recent notes."
   #'my/aaronnote-roam-recent-notes
   'note))

(defun my/aaronnote-roam-related-notes (&optional target-slug)
  "Show outgoing links and backlinks for the current note."
  (interactive)
  (let* ((slug (or target-slug (my/aaronnote-roam--current-slug)))
         (links (and slug (my/aaronnote-roam--note-links slug)))
         (backlinks (and slug (my/aaronnote-roam--db-backlinks-to slug)))
         (summaries (my/aaronnote-roam--all-note-summaries))
         (by-slug (lambda (target)
                    (seq-find (lambda (entry)
                                (equal (plist-get entry :slug) target))
                              summaries)))
         (refresh (let ((target slug))
                    (lambda () (my/aaronnote-roam-related-notes target))))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*aaronnote-roam-related*" "Related Notes" 'related refresh
               (format "%d links · %d backlinks"
                       (length links) (length backlinks)))))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Related notes"
          :icon 'related
          :subtitle (my/aaronnote-roam--note-title slug)
          :stats (list (cons (format "%d outgoing" (length links)) 'info)
                       (cons (format "%d backlinks" (length backlinks)) 'muted))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Outgoing links" (length links))
         (if links
             (dolist (target links)
               (when-let* ((entry (or (funcall by-slug target)
                                      (my/aaronnote-roam--summary-entry-for-slug
                                       target summaries))))
                 (my/aaronnote-roam--insert-note-button entry)))
           (my/aaronnote-roam-ui-insert-empty "No outgoing note links."))
         (insert "\n")
         (my/aaronnote-roam-ui-insert-section "Backlinks" (length backlinks))
         (if backlinks
             (dolist (target backlinks)
               (when-let* ((entry (or (funcall by-slug target)
                                      (my/aaronnote-roam--summary-entry-for-slug
                                       target summaries))))
                 (my/aaronnote-roam--insert-note-button entry)))
           (my/aaronnote-roam-ui-insert-empty "No backlinks.")))))
    (display-buffer buf)))

(defun my/aaronnote-roam-management ()
  "Show roam management commands and index status."
  (interactive)
  (let* ((entries (my/aaronnote-roam--all-note-summaries))
         (db (my/aaronnote-roam--db))
         (generated (and db (gethash "generated" db)))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*aaronnote-roam-management*" "Roam Management" 'management
               #'my/aaronnote-roam-management
               (format "%d nodes" (length entries)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Roam management"
          :icon 'management
          :subtitle "Index status and common Aaronnote operations"
          :stats (list (cons (format "%d nodes" (length entries)) 'info)
                       (cons (if generated "DB ready" "DB unknown")
                             (if generated 'success 'warning)))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Index")
         (my/aaronnote-roam-ui-insert-field
          "Root" (abbreviate-file-name (my/aaronnote-roam-root))
          'my/aaronnote-roam-ui-path)
         (my/aaronnote-roam-ui-insert-field "Nodes" (length entries))
         (my/aaronnote-roam-ui-insert-field
          "DB generated" (or generated "unknown")
          'my/aaronnote-roam-ui-meta)
         (insert "\n")
         (my/aaronnote-roam-ui-insert-section "Actions")
         (insert "   ")
         (my/aaronnote-roam-ui-insert-actions
          '((:label "Sync roam-db"
             :command my/aaronnote-roam-update-db
             :help "Run incremental roam-db sync"
             :primary t)
            (:label "New note"
             :command my/aaronnote-roam-new-note
             :help "Open the native Roam New workbench")
            (:label "Search notes"
             :command my/aaronnote-roam-search-notes
             :help "Search Aaronnote roam notes")
            (:label "DB status"
             :command my/aaronnote-roam-db-status
             :help "Open roam-db status"))))))
    (display-buffer buf)))

;; ── Roam agenda ─────────────────────────────────────────────────────────────

(defun my/aaronnote-roam--todo-ddl (entry)
  "Return deadline string for todo ENTRY, or nil."
  (let ((ddl (my/aaronnote-roam--todo-field entry "ddl" "deadline")))
    (and ddl (not (string-empty-p (or ddl ""))) ddl)))

(defun my/aaronnote-roam--todo-overdue-p (ddl)
  "Return non-nil when DDL string is in the past."
  (when (and ddl (not (string-empty-p ddl)))
    (condition-case nil
        (time-less-p (encode-time (parse-time-string ddl)) (current-time))
      (error nil))))

(defun my/aaronnote-roam-agenda ()
  "Show a roam notes agenda: todos from md notes grouped by status/ddl."
  (interactive)
  (let* ((todos (my/aaronnote-roam--todos))
         (overdue nil) (today nil) (upcoming nil) (no-ddl nil)
         (today-str (format-time-string "%Y-%m-%d")))
    (dolist (entry (or todos '()))
      (let ((ddl (my/aaronnote-roam--todo-ddl entry))
            (status (my/aaronnote-roam--todo-status entry)))
        (unless (member status '("done" "cancelled"))
          (cond
           ((and ddl (string= ddl today-str))
            (push entry today))
           ((and ddl (my/aaronnote-roam--todo-overdue-p ddl))
            (push entry overdue))
           (ddl (push entry upcoming))
           (t   (push entry no-ddl))))))
    (let* ((open-count (+ (length overdue) (length today)
                          (length upcoming) (length no-ddl)))
           (buf (my/aaronnote-roam--prepare-ui-buffer
                 "*roam-agenda*" "Roam Agenda" 'agenda
                 #'my/aaronnote-roam-agenda
                 (format "%d open" open-count))))
      (with-current-buffer buf
        (my/aaronnote-roam-ui-render
         (lambda ()
           (my/aaronnote-roam-ui-insert-page-header
            "Agenda"
            :icon 'agenda
            :subtitle (format "Open Aaronnote tasks for %s" today-str)
            :stats (list (cons (format "%d overdue" (length overdue))
                               (if overdue 'danger 'muted))
                         (cons (format "%d today" (length today))
                               (if today 'warning 'muted))
                         (cons (format "%d open" open-count) 'info))
            :actions (my/aaronnote-roam--ui-actions))
           (cl-labels
               ((insert-group
                 (title group tone)
                 (when group
                   (my/aaronnote-roam-ui-insert-section
                    title (length group) tone)
                   (dolist (entry (nreverse group))
                     (my/aaronnote-roam--insert-todo-row entry tone))
                   (insert "\n"))))
             (insert-group "Overdue" overdue 'danger)
             (insert-group "Today" today 'warning)
             (insert-group "Upcoming" upcoming 'info)
             (insert-group "No deadline" no-ddl 'muted))
           (when (zerop open-count)
             (my/aaronnote-roam-ui-insert-empty "No open tasks.")))))
      (display-buffer buf))))

;; ── Roam DB utilities ─────────────────────────────────────────────────────────

(defun my/aaronnote-roam-sync-full ()
  "Force a full roam-db rebuild (clears incremental state)."
  (interactive)
  (message "Rebuilding roam-db from scratch…")
  (when (my/aaronnote-roam--runtime-available-p)
    (my/aaronnote-roam--runtime-sync t nil))
  (message "Roam-db full rebuild done."))

(defun my/aaronnote-roam-db-status ()
  "Show roam-db sync state from .aaronnote-sync-state.json."
  (interactive)
  (let* ((root (my/aaronnote-roam-root))
         (state-file (expand-file-name ".aaronnote-sync-state.json" root))
         (state
          (when (file-exists-p state-file)
            (condition-case nil
                (json-parse-string
                 (with-temp-buffer
                   (insert-file-contents state-file)
                   (buffer-string))
                 :object-type 'hash-table)
              (error nil))))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-db-status*" "Roam DB Status" 'database
               #'my/aaronnote-roam-db-status
               (if state "state ready" "state missing"))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Roam DB status"
          :icon 'database
          :subtitle "Aaronnote incremental index state"
          :stats (list (cons (if state "State ready" "State missing")
                             (if state 'success 'warning)))
          :actions
          (my/aaronnote-roam--ui-actions
           '((:label "Incremental sync"
              :command my/aaronnote-roam-update-db
              :help "Run incremental roam-db sync"
              :primary t)
             (:label "Full rebuild"
              :command my/aaronnote-roam-sync-full
              :help "Rebuild the roam-db index from scratch"))))
         (my/aaronnote-roam-ui-insert-section "Location")
         (my/aaronnote-roam-ui-insert-field
          "Root" (abbreviate-file-name root) 'my/aaronnote-roam-ui-path)
         (my/aaronnote-roam-ui-insert-field
          "State file" (abbreviate-file-name state-file)
          'my/aaronnote-roam-ui-path)
         (insert "\n")
         (my/aaronnote-roam-ui-insert-section "State")
         (cond
          (state
           (dolist (key (sort (hash-table-keys state) #'string<))
             (let ((value (gethash key state)))
               (my/aaronnote-roam-ui-insert-field
                key (if (eq value :null) "(null)" value)
                'my/aaronnote-roam-ui-meta))))
          ((file-exists-p state-file)
           (my/aaronnote-roam-ui-insert-empty
            "The state file exists but could not be parsed."))
          (t
           (my/aaronnote-roam-ui-insert-empty
            "No sync state yet. Run an incremental sync or full rebuild."))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-magit ()
  "Open magit-status in the roam notes root."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "magit is not available — install it first"))
  (magit-status (my/aaronnote-roam-root)))

(defun my/aaronnote-roam-dired ()
  "Open dired at the roam notes root."
  (interactive)
  (dired (my/aaronnote-roam-root)))

;; ── Roam completion-at-point (roam:// and ../ paths) ─────────────────────────

(defun my/aaronnote-roam-capf ()
  "Completion-at-point for roam:// links and relative paths in Typst/md buffers."
  (let ((roam-prefix "roam://")
        (dotdot-re "\\.\\./"))
    (cond
     ;; roam://... completion
     ((and (looking-back (concat roam-prefix "[^][\n\t ]*") (line-beginning-position) t)
           (save-excursion
             (re-search-backward (concat roam-prefix "\\([^][\n\t ]*\\)")
                                 (line-beginning-position) t)))
      (let* ((start (match-beginning 0))
             (end   (match-end 0))
             (candidates
              (mapcar (lambda (entry)
                        (concat roam-prefix
                                (or (plist-get entry :slug) (plist-get entry :id) "")))
                      (my/aaronnote-roam--all-note-summaries))))
        (when candidates
          (list start end candidates :exclusive 'no))))
     ;; ../  relative path completion
     ((looking-back (concat dotdot-re "[^][\n\t ]*") (line-beginning-position) t)
      (let* ((root (my/aaronnote-roam-root))
             (start (save-excursion
                      (re-search-backward (concat dotdot-re "\\([^][\n\t ]*\\)")
                                          (line-beginning-position) t)
                      (match-beginning 0)))
             (end (point))
             (candidates
              (when (file-directory-p root)
                (let ((rel (buffer-substring-no-properties start end))
                      result)
                  (dolist (f (my/aaronnote-roam--all-files))
                    (let ((r (file-relative-name f (file-name-directory
                                                    (or buffer-file-name root)))))
                      (when (string-prefix-p rel r)
                        (push r result))))
                  result))))
        (when candidates
          (list start end candidates :exclusive 'no))))
     (t nil))))

(defun my/aaronnote-roam--capf-setup ()
  "Register roam capf for this buffer."
  (add-hook 'completion-at-point-functions #'my/aaronnote-roam-capf nil t))

(add-hook 'markdown-mode-hook #'my/aaronnote-roam--capf-setup)

;; ── Interactive Markdown roam link selector ──────────────────────────────────

(defvar-local my/aaronnote-roam-select--origin-marker nil)
(defvar-local my/aaronnote-roam-select--current-note-id nil)
(defvar-local my/aaronnote-roam-select--preferred-kind nil)
(defvar-local my/aaronnote-roam-select--view nil)
(defvar-local my/aaronnote-roam-select--path "")
(defvar-local my/aaronnote-roam-select--query nil)
(defvar-local my/aaronnote-roam-select--target-record nil)
(defvar-local my/aaronnote-roam-select--target-basis 'id)
(defvar-local my/aaronnote-roam-select--toc-parent nil)

(defvar my/aaronnote-roam-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map my/aaronnote-roam-ui-mode-map)
    (define-key map (kbd "RET") #'my/aaronnote-roam-select-activate)
    (define-key map (kbd "i") #'my/aaronnote-roam-select-insert-current)
    (define-key map (kbd "/") #'my/aaronnote-roam-select-search)
    (define-key map (kbd "s") #'my/aaronnote-roam-select-search)
    (define-key map (kbd "g") #'my/aaronnote-roam-select-root)
    (define-key map (kbd ".") #'my/aaronnote-roam-select-context)
    (define-key map (kbd "u") #'my/aaronnote-roam-select-up)
    (define-key map (kbd "^") #'my/aaronnote-roam-select-up)
    (define-key map (kbd "r") #'my/aaronnote-roam-select-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my/aaronnote-roam-select-mode'.")

(define-derived-mode my/aaronnote-roam-select-mode my/aaronnote-roam-ui-mode "Roam-Select"
  "Interactive Markdown roam link selector."
  (setq-local truncate-lines t)
  (setq-local my/aaronnote-roam-ui-refresh-function
              #'my/aaronnote-roam-select-refresh)
  (my/aaronnote-roam-ui-set-header "Roam Selector" 'search "search"))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/aaronnote-roam-select-mode 'emacs))

(defun my/aaronnote-roam--record-path-ref (record)
  "Return RECORD's path-like link ref."
  (let* ((note (plist-get record :note))
         (file (plist-get record :file))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/aaronnote-roam-root)))))
    (or (my/aaronnote-roam--note-field note "path")
        (my/aaronnote-roam--note-field note "link")
        rel-file
        (plist-get record :key)
        (plist-get record :id))))

(defun my/aaronnote-roam--target-suffix (kind target)
  "Return Markdown roam link suffix for KIND and TARGET."
  (pcase kind
    ('tag (concat "#" (my/aaronnote-roam--encode-ref target)))
    ('dom (concat "@" (mapconcat #'my/aaronnote-roam--encode-ref
                                  (my/aaronnote-roam--dom-target-segments target)
                                  "@")))
    (_ "")))

(defun my/aaronnote-roam--link-target-for-record (record basis &optional kind target)
  "Return Markdown roam link target for RECORD using BASIS, KIND, and TARGET."
  (let ((basis (if (stringp basis) (intern basis) basis)))
    (if (eq basis 'path)
        (concat (my/aaronnote-roam--record-path-ref record)
                (my/aaronnote-roam--target-suffix kind target))
      (my/aaronnote-roam--roam-href (plist-get record :id) kind target))))

(defun my/aaronnote-roam--insert-note-link-target (target text &optional marker)
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

(defun my/aaronnote-roam--tag-targets (record)
  "Return tag target plists for RECORD."
  (let* ((file (plist-get record :file))
         (note (plist-get record :note))
         (labels (and file (my/aaronnote-roam--heading-labels file)))
         (inline-tags (my/aaronnote-roam--note-list-field note "inlineTags"))
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

(defun my/aaronnote-roam--tag-target-display (target)
  "Return completion display string for tag TARGET."
  (let ((id (plist-get target :id))
        (label (plist-get target :label)))
    (if (and label (not (equal id label)))
        (format "%s  %s" id label)
      id)))

(defun my/aaronnote-roam--read-tag-target (record)
  "Read a tag target for RECORD."
  (let* ((targets (my/aaronnote-roam--tag-targets record))
         (table (mapcar (lambda (target)
                          (cons (my/aaronnote-roam--tag-target-display target)
                                target))
                        targets))
         (choice (if table
                     (completing-read "Tag: " table nil t)
                   (user-error "No tag ids in this note"))))
    (cdr (assoc choice table))))

(defun my/aaronnote-roam-select--toc-targets (record)
  "Return TOC targets for RECORD."
  (let* ((file (plist-get record :file))
         (note-id (plist-get record :id))
         (seen nil)
         targets)
    (dolist (target (my/aaronnote-roam--dom-targets file note-id))
      (let ((key (string-join (plist-get target :path) "@")))
        (when (and (not (plist-get target :synthetic))
                   (not (string-empty-p key))
                   (not (member key seen)))
          (push key seen)
          (push target targets))))
    (nreverse targets)))

(defun my/aaronnote-roam-select--toc-dom (target)
  "Return DOM target string for TOC TARGET."
  (string-join (plist-get target :path) "@"))

(defun my/aaronnote-roam-select--read-basis ()
  "Read target basis for the selected note."
  (intern
   (completing-read "Target ref: "
                    '("id" "path")
                    nil t nil nil "id")))

(defun my/aaronnote-roam-select--read-kind ()
  "Read exact target kind for the selected note."
  (pcase (completing-read "Target kind: "
                          '("note" "tag" "toc")
                          nil t nil nil "note")
    ("tag" 'tag)
    ("toc" 'toc)
    (_ 'note)))

(defun my/aaronnote-roam-select--default-note-text (record)
  "Return default display text for RECORD."
  (or (plist-get record :title)
      (plist-get record :id)))

(defun my/aaronnote-roam-select--finish-target (record basis kind target default-text)
  "Insert final note-link for RECORD, BASIS, KIND, TARGET, and DEFAULT-TEXT."
  (let* ((href (my/aaronnote-roam--link-target-for-record record basis kind target))
         (text (read-string (format "Display text [%s]: " default-text)
                            nil nil default-text)))
    (my/aaronnote-roam--insert-note-link-target
     href text my/aaronnote-roam-select--origin-marker)
    (when-let* (((derived-mode-p 'my/aaronnote-roam-select-mode))
                (window (get-buffer-window (current-buffer))))
      (quit-window t window))))

(defun my/aaronnote-roam-select--choose-record (record)
  "Choose exact target for note RECORD."
  (let* ((basis (my/aaronnote-roam-select--read-basis))
         (kind (or my/aaronnote-roam-select--preferred-kind
                   (my/aaronnote-roam-select--read-kind))))
    (pcase kind
      ('tag
       (let* ((tag (my/aaronnote-roam--read-tag-target record))
              (id (plist-get tag :id))
              (label (or (plist-get tag :label) id)))
         (my/aaronnote-roam-select--finish-target
          record basis 'tag id label)))
      ('toc
       (setq my/aaronnote-roam-select--target-record record
             my/aaronnote-roam-select--target-basis basis
             my/aaronnote-roam-select--toc-parent nil
             my/aaronnote-roam-select--query nil)
       (my/aaronnote-roam-select--render-toc))
      (_
       (my/aaronnote-roam-select--finish-target
        record basis nil nil
        (my/aaronnote-roam-select--default-note-text record))))))

(defun my/aaronnote-roam-select--record-relative-file (record)
  "Return RECORD's relative file under the roam root, or nil."
  (when-let* ((file (plist-get record :file)))
    (let ((rel (file-relative-name file (my/aaronnote-roam-root))))
      (unless (or (string-prefix-p "../" rel)
                  (string-prefix-p "/.." rel)
                  (string-match-p "\\`_typst/" rel))
        rel))))

(defun my/aaronnote-roam-select--directory-items (dir)
  "Return directory and note items immediately inside DIR."
  (let ((dir (if (string-empty-p (or dir "")) "" dir))
        dirs notes seen-dirs)
    (dolist (record (my/aaronnote-roam--note-records))
      (when-let* ((rel (my/aaronnote-roam-select--record-relative-file record)))
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

(defun my/aaronnote-roam-select--insert-row (label item &optional face)
  "Insert a selectable row LABEL carrying ITEM."
  (let* ((type (plist-get item :type))
         (record (plist-get item :record))
         (target (plist-get item :target))
         (title (pcase type
                  ('dir (format "%s/" (plist-get item :name)))
                  ('note (my/aaronnote-roam-select--default-note-text record))
                  ('toc (my/aaronnote-roam--dom-target-path-label target))
                  (_ label)))
         (meta (pcase type
                 ('dir (plist-get item :path))
                 ('note (my/aaronnote-roam--record-path-ref record))
                 ('toc (if (plist-get item :has-children) "branch" "heading"))
                 (_ nil)))
         (detail (and (eq type 'note) (plist-get record :id)))
         (tags (and (eq type 'note)
                    (my/aaronnote-roam--note-list-field
                     (plist-get record :note) "tags")))
         (id (pcase type
               ('dir (plist-get item :path))
               ('note (plist-get record :id))
               ('toc (my/aaronnote-roam-select--toc-dom target))
               (_ label))))
    (my/aaronnote-roam-ui-insert-row
     :id id
     :icon (pcase type
             ('dir 'directory)
             ('toc 'toc)
             (_ 'note))
     :badge (pcase type
              ('dir "DIR")
              ('toc (if (plist-get item :has-children) "BRANCH" "TOC"))
              (_ nil))
     :badge-tone (if (eq type 'dir) 'warning 'info)
     :title title
     :title-face (or face 'my/aaronnote-roam-ui-row-title)
     :meta meta
     :detail detail
     :tags tags
     :action
     (lambda (_ignored)
       (my/aaronnote-roam-select-activate))
     :help "RET: open/select, i: insert/select"
     :properties `(my/aaronnote-roam-select-item ,item))))

(defun my/aaronnote-roam-select--note-label (record &optional prefix)
  "Return display label for note RECORD with PREFIX."
  (let* ((title (my/aaronnote-roam-select--default-note-text record))
         (path (my/aaronnote-roam--record-path-ref record))
         (tags (my/aaronnote-roam--note-list-field (plist-get record :note) "tags")))
    (concat (or prefix "")
            (format "%-38s %s" title (plist-get record :id))
            (when path (concat "  " path))
            (when-let* ((strtags (seq-filter #'stringp tags)))
              (concat "  #" (string-join strtags " #"))))))

(defun my/aaronnote-roam-select--render-header (title)
  "Render selector TITLE and help."
  (let ((icon (pcase my/aaronnote-roam-select--view
                ('root 'directory)
                ('context 'related)
                ('toc 'toc)
                (_ 'search))))
    (my/aaronnote-roam-ui-set-header
     "Roam Selector" icon
     (format "%s view" (or my/aaronnote-roam-select--view 'search)))
    (my/aaronnote-roam-ui-insert-page-header
     title
     :icon icon
     :subtitle "Choose a note, tag, or TOC target without leaving the keyboard"
     :stats (list (cons (format "%s view"
                               (or my/aaronnote-roam-select--view 'search))
                       'info))
     :actions
     '((:label "RET Select"
        :command my/aaronnote-roam-select-activate
        :help "Open or select the current item"
        :primary t)
       (:label "/ Search"
        :command my/aaronnote-roam-select-search
        :help "Search notes or TOC headings")
       (:label "g Root"
        :command my/aaronnote-roam-select-root
        :help "Show the roam root")
       (:label "r Refresh"
        :command my/aaronnote-roam-select-refresh
        :help "Refresh the current selector view")
       (:label "q Close"
        :command quit-window
        :help "Close the selector")))))

(defun my/aaronnote-roam-select--render-root (&optional dir)
  "Render roam root tree at DIR."
  (setq my/aaronnote-roam-select--view 'root
        my/aaronnote-roam-select--path (or dir ""))
  (my/aaronnote-roam-ui-render
   (lambda ()
     (my/aaronnote-roam-select--render-header
      (format "Roam root: /%s" my/aaronnote-roam-select--path))
     (let ((items (my/aaronnote-roam-select--directory-items
                   my/aaronnote-roam-select--path)))
       (my/aaronnote-roam-ui-insert-section "Contents" (length items))
       (if items
           (dolist (item items)
             (pcase (plist-get item :type)
               ('dir
                (my/aaronnote-roam-select--insert-row
                 (format "%s/" (plist-get item :name))
                 item 'my/aaronnote-roam-ui-row-title))
               ('note
                (my/aaronnote-roam-select--insert-row
                 (my/aaronnote-roam-select--note-label
                  (plist-get item :record))
                 item))))
         (my/aaronnote-roam-ui-insert-empty "This directory is empty.")))))
  (unless (my/aaronnote-roam-select--item-at-point)
    (my/aaronnote-roam-ui-goto-first-item)))

(defun my/aaronnote-roam-select--render-context ()
  "Render current-note context."
  (setq my/aaronnote-roam-select--view 'context)
  (let ((record (and my/aaronnote-roam-select--current-note-id
                     (my/aaronnote-roam--resolve-note
                      my/aaronnote-roam-select--current-note-id)))
        entries)
    (when record
      (push record entries)
      (dolist (id (append (my/aaronnote-roam--note-links (plist-get record :id))
                          (my/aaronnote-roam--db-backlinks-to
                           (plist-get record :id))))
        (when-let* ((related (my/aaronnote-roam--resolve-note id)))
          (push related entries))))
    (setq entries (delete-dups (nreverse entries)))
    (my/aaronnote-roam-ui-render
     (lambda ()
       (my/aaronnote-roam-select--render-header
        (format "Current roam context: %s"
                (or my/aaronnote-roam-select--current-note-id "(none)")))
       (my/aaronnote-roam-ui-insert-section "Context" (length entries))
       (if entries
           (dolist (entry entries)
             (my/aaronnote-roam-select--insert-row
              (my/aaronnote-roam-select--note-label entry)
              (list :type 'note :record entry)))
         (my/aaronnote-roam-ui-insert-empty
          "Not in a roam note. Press g to browse the root."))))
    (unless (my/aaronnote-roam-select--item-at-point)
      (my/aaronnote-roam-ui-goto-first-item))))

(defun my/aaronnote-roam-select--render-search (query)
  "Render global note search for QUERY."
  (setq my/aaronnote-roam-select--view 'search
        my/aaronnote-roam-select--query query)
  (let ((entries (my/aaronnote-roam-search-notes query)))
    (my/aaronnote-roam-ui-render
     (lambda ()
       (my/aaronnote-roam-select--render-header
        (if (string-empty-p (or query ""))
            "Search all roam notes"
          (format "Roam search: %s" query)))
       (my/aaronnote-roam-ui-insert-section "Matches" (length entries))
       (if entries
           (dolist (entry entries)
             (when-let* ((record (my/aaronnote-roam--resolve-note
                                  (plist-get entry :slug))))
               (my/aaronnote-roam-select--insert-row
                (my/aaronnote-roam-select--note-label record)
                (list :type 'note :record record))))
         (my/aaronnote-roam-ui-insert-empty "No matching notes."))))
    (unless (my/aaronnote-roam-select--item-at-point)
      (my/aaronnote-roam-ui-goto-first-item))))

(defun my/aaronnote-roam-select--toc-children (targets parent)
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

(defun my/aaronnote-roam-select--toc-has-children-p (targets target)
  "Return non-nil if TARGET has child targets in TARGETS."
  (let* ((path (plist-get target :path))
         (len (length path)))
    (seq-some
     (lambda (candidate)
       (let ((candidate-path (plist-get candidate :path)))
         (and (> (length candidate-path) len)
              (equal (seq-take candidate-path len) path))))
     targets)))

(defun my/aaronnote-roam-select--render-toc ()
  "Render TOC selector for `my/aaronnote-roam-select--target-record'."
  (setq my/aaronnote-roam-select--view 'toc)
  (let* ((record my/aaronnote-roam-select--target-record)
         (targets (my/aaronnote-roam-select--toc-targets record))
         (query my/aaronnote-roam-select--query)
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
                    (my/aaronnote-roam-select--toc-children
                     targets my/aaronnote-roam-select--toc-parent))))
    (my/aaronnote-roam-ui-render
     (lambda ()
       (my/aaronnote-roam-select--render-header
        (format "TOC: %s%s"
                (my/aaronnote-roam-select--default-note-text record)
                (if query (format " / search: %s" query) "")))
       (when my/aaronnote-roam-select--toc-parent
         (my/aaronnote-roam-ui-insert-field
          "Path" (string-join my/aaronnote-roam-select--toc-parent " / ")
          'my/aaronnote-roam-ui-path)
         (insert "\n"))
       (my/aaronnote-roam-ui-insert-section "Headings" (length visible))
       (if visible
           (dolist (target visible)
             (let* ((has-children
                     (and (not query)
                          (my/aaronnote-roam-select--toc-has-children-p
                           targets target)))
                    (label (my/aaronnote-roam--dom-target-path-label target)))
               (my/aaronnote-roam-select--insert-row
                label
                (list :type 'toc
                      :target target
                      :has-children has-children
                      :search query))))
         (my/aaronnote-roam-ui-insert-empty "No TOC targets."))))
    (unless (my/aaronnote-roam-select--item-at-point)
      (my/aaronnote-roam-ui-goto-first-item))))

(defun my/aaronnote-roam-select--item-at-point ()
  "Return selector item at point."
  (or (get-text-property (point) 'my/aaronnote-roam-select-item)
      (get-text-property (line-beginning-position) 'my/aaronnote-roam-select-item)
      (get-text-property (max (point-min) (1- (point)))
                         'my/aaronnote-roam-select-item)))

(defun my/aaronnote-roam-select--finish-toc-target (target)
  "Insert the selected TOC TARGET."
  (let* ((record my/aaronnote-roam-select--target-record)
         (dom (my/aaronnote-roam-select--toc-dom target))
         (label (my/aaronnote-roam--dom-target-path-label target)))
    (my/aaronnote-roam-select--finish-target
     record my/aaronnote-roam-select--target-basis 'dom dom label)))

(defun my/aaronnote-roam-select-activate ()
  "Activate the selector item at point."
  (interactive)
  (pcase-let* ((item (my/aaronnote-roam-select--item-at-point))
               (type (plist-get item :type)))
    (pcase type
      ('dir
       (my/aaronnote-roam-select--render-root (plist-get item :path)))
      ('note
       (my/aaronnote-roam-select--choose-record (plist-get item :record)))
      ('toc
       (if (and (plist-get item :has-children)
                (not (plist-get item :search)))
           (progn
             (setq my/aaronnote-roam-select--toc-parent
                   (plist-get (plist-get item :target) :path)
                   my/aaronnote-roam-select--query nil)
             (my/aaronnote-roam-select--render-toc))
         (my/aaronnote-roam-select--finish-toc-target
          (plist-get item :target))))
      (_
       (user-error "No selectable roam item at point")))))

(defun my/aaronnote-roam-select-insert-current ()
  "Insert/select the current selector item without descending."
  (interactive)
  (let ((item (my/aaronnote-roam-select--item-at-point)))
    (pcase (plist-get item :type)
      ('toc
       (my/aaronnote-roam-select--finish-toc-target
        (plist-get item :target)))
      (_
       (my/aaronnote-roam-select-activate)))))

(defun my/aaronnote-roam-select-search ()
  "Search notes globally, or TOC headings inside a TOC view."
  (interactive)
  (let ((query (read-string "Search: ")))
    (if (eq my/aaronnote-roam-select--view 'toc)
        (progn
          (setq my/aaronnote-roam-select--query query
                my/aaronnote-roam-select--toc-parent nil)
          (my/aaronnote-roam-select--render-toc))
      (my/aaronnote-roam-select--render-search query))))

(defun my/aaronnote-roam-select-root ()
  "Render the roam root tree."
  (interactive)
  (my/aaronnote-roam-select--render-root ""))

(defun my/aaronnote-roam-select-context ()
  "Render current-note context."
  (interactive)
  (if my/aaronnote-roam-select--current-note-id
      (my/aaronnote-roam-select--render-context)
    (my/aaronnote-roam-select--render-root "")))

(defun my/aaronnote-roam-select-up ()
  "Move one selector level up."
  (interactive)
  (pcase my/aaronnote-roam-select--view
    ('root
     (let* ((path (string-remove-suffix "/" my/aaronnote-roam-select--path))
            (parent (if (string-match "\\`\\(.*?/\\)?[^/]+\\'" path)
                        (or (match-string 1 path) "")
                      "")))
       (my/aaronnote-roam-select--render-root parent)))
    ('toc
     (setq my/aaronnote-roam-select--query nil
           my/aaronnote-roam-select--toc-parent
           (butlast my/aaronnote-roam-select--toc-parent))
     (my/aaronnote-roam-select--render-toc))
    (_
     (my/aaronnote-roam-select-context))))

(defun my/aaronnote-roam-select-refresh ()
  "Refresh the current selector view."
  (interactive)
  (pcase my/aaronnote-roam-select--view
    ('root (my/aaronnote-roam-select--render-root my/aaronnote-roam-select--path))
    ('search (my/aaronnote-roam-select--render-search my/aaronnote-roam-select--query))
    ('toc (my/aaronnote-roam-select--render-toc))
    (_ (my/aaronnote-roam-select-context))))

(defun my/aaronnote-roam-select--display-buffer (buffer)
  "Display selector BUFFER in a focused bottom side window."
  (let* ((alist `((side . bottom)
                  (slot . 1)
                  (window-height . ,my/aaronnote-roam-select-window-height)))
         (window (or (get-buffer-window buffer)
                     (display-buffer-in-side-window buffer alist))))
    (set-window-buffer window buffer)
    (select-window window)
    window))

(defun my/aaronnote-roam-select-link (&optional preferred-kind)
  "Open an interactive note-link selector.
PREFERRED-KIND may be `tag' or `toc' to skip the target-kind prompt."
  (interactive)
  (let ((origin (copy-marker (point) t))
        (current-note-id (my/aaronnote-roam--current-slug))
        (buf (get-buffer-create "*aaronnote-roam-select*")))
    (with-current-buffer buf
      (my/aaronnote-roam-select-mode)
      (setq-local my/aaronnote-roam-select--origin-marker origin
                  my/aaronnote-roam-select--current-note-id current-note-id
                  my/aaronnote-roam-select--preferred-kind preferred-kind
                  my/aaronnote-roam-select--target-record nil
                  my/aaronnote-roam-select--target-basis 'id
                  my/aaronnote-roam-select--toc-parent nil
                  my/aaronnote-roam-select--query nil)
      (my/aaronnote-roam-select--render-search ""))
    (my/aaronnote-roam-select--display-buffer buf)))

(defun my/aaronnote-roam-copy-link-to-here ()
  "Copy a Markdown roam link to the current note or current heading.
When point is on a heading, ensure a Markdown `{#tag-id}' exists and copy a
canonical `roam://note-id#tag' target."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer has no file"))
  (let* ((note-id (my/aaronnote-roam--current-slug))
         (title (my/aaronnote-roam--note-title note-id))
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
              (setq id (my/aaronnote-roam-generate-tag-id heading))
              (end-of-line)
              (insert (format " {#%s}" id)))
            (setq target (my/aaronnote-roam--roam-href note-id 'tag id)
                  text heading))
        (setq target (my/aaronnote-roam--roam-href note-id)
              text title)))
    (let ((link (format "[%s](%s)"
                        (replace-regexp-in-string "\\]" "\\\\]" (or text ""))
                        target)))
      (kill-new link)
      (message "Copied %s" link))))

;; Enhanced find-note with DB annotation
(defun my/aaronnote-roam--note-annotator (slug)
  "Return annotation for SLUG in completing-read."
  (when-let* ((record (my/aaronnote-roam--resolve-note slug))
              (note (plist-get record :note)))
    (let ((tags (my/aaronnote-roam--note-list-field note "tags"))
          (bls  (length (or (my/aaronnote-roam--db-backlinks-to
                             (plist-get record :id))
                            (gethash "backlinks" note)))))
      (concat "  "
              (if tags (string-join (seq-filter #'stringp tags) ",") "")
              (when (> bls 0) (format " ←%d" bls))))))

;; Auto-update DB on save
(defun my/aaronnote-roam--note-file-p (file)
  "Return non-nil when FILE is a Markdown roam note in the current vault."
  (when (and file
             (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file))
    (let* ((root (file-name-as-directory
                  (file-truename (my/aaronnote-roam-root))))
           (truename (file-truename file))
           (rel (file-relative-name truename root)))
      (and (string-prefix-p root truename)
           (not (string-match-p
                 "\\`\\(?:\\.git/\\|\\.lean/\\|_typst/\\|node_modules/\\)"
                 rel))))))

(defun my/aaronnote-roam--schedule-runtime-sync (file)
  "Debounce an incremental runtime sync for changed Markdown note FILE."
  (when (timerp my/aaronnote-roam--sync-timer)
    (cancel-timer my/aaronnote-roam--sync-timer))
  (push file my/aaronnote-roam--sync-changed-files)
  (setq my/aaronnote-roam--sync-changed-files
        (delete-dups
         (seq-filter #'identity my/aaronnote-roam--sync-changed-files)))
  (setq my/aaronnote-roam--sync-timer
        (run-at-time
         my/aaronnote-roam-sync-delay nil
         (lambda ()
           (let ((changed my/aaronnote-roam--sync-changed-files))
             (setq my/aaronnote-roam--sync-timer nil
                   my/aaronnote-roam--sync-changed-files nil)
             (if (my/aaronnote-roam--runtime-available-p)
                 (my/aaronnote-roam--runtime-sync nil changed)
               (message "Markdown roam cache refreshed")))))))

;; ── Keymaps & menus ───────────────────────────────────────────────────────────

(defvar my/aaronnote-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'my/aaronnote-roam-find-note)
    (define-key map (kbd "o") #'my/aaronnote-roam-follow-link)
    (define-key map (kbd "i") #'my/aaronnote-roam-insert-link)
    (define-key map (kbd "RET") #'my/aaronnote-roam-select-link)
    (define-key map (kbd "I") #'my/aaronnote-roam-insert-tag-id-link)
    (define-key map (kbd "c") #'my/aaronnote-roam-insert-toc-link)
    (define-key map (kbd "y") #'my/aaronnote-roam-copy-link-to-here)
    (define-key map (kbd "n") #'my/aaronnote-roam-new-note)
    (define-key map (kbd "N") #'my/aaronnote-roam-new-node)
    (define-key map (kbd "#") #'my/aaronnote-roam-insert-tag-id)
    (define-key map (kbd "g") #'my/aaronnote-roam-generate-tag-id)
    (define-key map (kbd "s") #'my/aaronnote-roam-search-notes)
    (define-key map (kbd "r") #'my/aaronnote-roam-recent-notes)
    (define-key map (kbd "R") #'my/aaronnote-roam-related-notes)
    (define-key map (kbd "G") #'my/aaronnote-roam-graph)
    (define-key map (kbd "M") #'my/aaronnote-roam-management)
    (define-key map (kbd "b") #'my/aaronnote-roam-backlinks)
    (define-key map (kbd "t") #'my/aaronnote-roam-tags)
    (define-key map (kbd "T") #'my/aaronnote-roam-todos)
    (define-key map (kbd "A") #'my/aaronnote-roam-agenda)
    (define-key map (kbd "u") #'my/aaronnote-roam-update-db)
    (define-key map (kbd "U") #'my/aaronnote-roam-sync-full)
    (define-key map (kbd "S") #'my/aaronnote-roam-db-status)
    (define-key map (kbd "V") #'my/aaronnote-roam-magit)
    (define-key map (kbd "D") #'my/aaronnote-roam-dired)
    (define-key map (kbd "Q") #'my/aaronnote-stop)
    (define-key map (kbd "m") #'my/aaronnote-roam-dispatch)
    map)
  "Roam keymap for Markdown buffers. Bound to C-c r.")

(my/leader!
  "r m" '(:def my/aaronnote-roam-dispatch :which-key "md roam")
  "r t" '(:def my/aaronnote-roam-dispatch :which-key "md roam")
  "r a" '(:def my/aaronnote-roam-agenda   :which-key "roam agenda")
  "r d" '(:def my/aaronnote-roam-dired    :which-key "roam dired")
  "r v" '(:def my/aaronnote-roam-magit    :which-key "roam magit")
  "r S" '(:def my/aaronnote-roam-db-status :which-key "roam db status")
  "r e" '(:def my/aaronnote-open-markdown-raw :which-key "edit raw md"))

;; ── xref backend: gd / M-. for note-link ─────────────────────────────────

(defun my/aaronnote-roam--all-slugs-cached ()
  "Return all canonical roam note ids."
  (mapcar (lambda (record) (plist-get record :id))
          (my/aaronnote-roam--note-records)))

(defun my/aaronnote-roam-xref-backend ()
  "Use aaronnote-roam as xref backend when point is on a Markdown roam link."
  (when (my/aaronnote-roam--target-at-point) 'aaronnote-roam))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql aaronnote-roam)))
  (my/aaronnote-roam--target-at-point))

(defun my/aaronnote-roam-goto-definition ()
  "Jump to the note-link target at point, falling back to normal gd."
  (interactive)
  (if (my/aaronnote-roam--target-at-point)
      (progn
        (when (fboundp 'my/navigation--push-jump)
          (my/navigation--push-jump))
        (my/aaronnote-roam-follow-link))
    (if (fboundp 'my/navigation-find-definition)
        (call-interactively #'my/navigation-find-definition)
      (call-interactively #'xref-find-definitions))))

(defun my/aaronnote-roam--xref-location (file parsed)
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
                  (my/aaronnote-roam--find-dom-target
                   (plist-get parsed :dom) file (plist-get parsed :slug))
                  :pos)))))
      (if pos
          (progn
            (my/aaronnote-roam--goto-pos pos)
            (xref-make-file-location file
                                     (line-number-at-pos)
                                     (current-column)))
        (xref-make-file-location file 1 0)))))

(cl-defmethod xref-backend-definitions ((_backend (eql aaronnote-roam)) target)
  (when-let* ((parsed (my/aaronnote-roam--parse-target target))
              (file (plist-get parsed :file))
              ((file-exists-p file)))
    (list (xref-make (concat "note: " target)
                     (my/aaronnote-roam--xref-location file parsed)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql aaronnote-roam)))
  (mapcar #'my/aaronnote-roam--roam-href
          (my/aaronnote-roam--all-slugs-cached)))

(defun my/aaronnote-roam--xref-setup ()
  "Register aaronnote-roam xref backend for this buffer (highest priority)."
  (add-hook 'xref-backend-functions #'my/aaronnote-roam-xref-backend -90 t))

;; ── Preview click → note-link intercept ──────────────────────────────────

;; ── Daily note ────────────────────────────────────────────────────────────

(defun my/aaronnote-roam-daily-note ()
  "Open or create today's daily note at daily/YYYY-MM-DD."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (slug (concat "daily/" date))
         (file (my/aaronnote-roam--slug-to-file slug)))
    (if (file-exists-p file)
        (my/aaronnote-roam--open-slug slug)
      (my/aaronnote-roam-new--create-draft
       (list :node-type "roam"
             :id slug
             :title (format "%s Daily" date)
             :path (concat slug ".md")
             :kind "note"
             :template-key "daily"
             :tags nil)))))

;; ── Wire everything up ────────────────────────────────────────────────────

(define-key my/aaronnote-roam-map (kbd "d") #'my/aaronnote-roam-daily-note)

(defun my/aaronnote-roam-setup-keys ()
  "Set up Aaronnote roam keys and xref for the current Markdown buffer.
Binds `C-c r' to `my/aaronnote-roam-map' and registers the roam xref
backend.  Does not install completion-at-point functions (those are
added separately by `my/aaronnote-roam--capf-setup')."
  (local-set-key (kbd "C-c r") my/aaronnote-roam-map)
  (my/aaronnote-roam--xref-setup))

(add-hook 'markdown-mode-hook #'my/aaronnote-roam-setup-keys)

;; Update transient with daily + gd hint
(transient-define-prefix my/aaronnote-roam-dispatch ()
  "Markdown roam command menu."
  [["Notes"
    ("RET" "select link"         my/aaronnote-roam-select-link)
    ("o" "open link   C-c C-o" my/aaronnote-roam-follow-link)
    ("f" "find note"            my/aaronnote-roam-find-note)
    ("i" "insert link"          my/aaronnote-roam-insert-link)
    ("I" "insert tag link"      my/aaronnote-roam-insert-tag-id-link)
    ("c" "insert toc link"      my/aaronnote-roam-insert-toc-link)
    ("y" "copy link here"       my/aaronnote-roam-copy-link-to-here)
    ("n" "new note UI"          my/aaronnote-roam-new-note)
    ("N" "new node UI"          my/aaronnote-roam-new-node)
    ("d" "daily note"           my/aaronnote-roam-daily-note)]
   ["Tag ids"
    ("#" "insert tag id"        my/aaronnote-roam-insert-tag-id)
    ("g" "generate tag id"      my/aaronnote-roam-generate-tag-id)]
   ["Explore"
    ("s" "search/filter"        my/aaronnote-roam-search-notes)
    ("r" "recent"               my/aaronnote-roam-recent-notes)
    ("R" "related"              my/aaronnote-roam-related-notes)
    ("G" "graph"                my/aaronnote-roam-graph)
    ("M" "management"           my/aaronnote-roam-management)]
   ["DB & Agenda"
    ("b" "backlinks"            my/aaronnote-roam-backlinks)
    ("t" "tags"                 my/aaronnote-roam-tags)
    ("T" "todos"                my/aaronnote-roam-todos)
    ("A" "agenda"               my/aaronnote-roam-agenda)
    ("u" "sync (incremental)"   my/aaronnote-roam-update-db)
    ("U" "sync (full rebuild)"  my/aaronnote-roam-sync-full)
    ("S" "db status"            my/aaronnote-roam-db-status)]
   ["Files"
    ("V" "version (magit)"      my/aaronnote-roam-magit)
    ("D" "dired (file browser)" my/aaronnote-roam-dired)
    ("Q" "stop web-host"        my/aaronnote-stop)]
   ["Nav (gd = xref)"
    ("." "xref definition"      xref-find-definitions)
    ("x" "xref references"      xref-find-references)]])

;;; Public lifecycle API (called from init-aaronnote.el).

(defun my/aaronnote-roam--cancel-sync-timer ()
  "Cancel any pending debounced roam sync and clear the changed-files list."
  (when (timerp my/aaronnote-roam--sync-timer)
    (cancel-timer my/aaronnote-roam--sync-timer))
  (setq my/aaronnote-roam--sync-timer nil
        my/aaronnote-roam--sync-changed-files nil))

(defun my/aaronnote-roam-note-changed (file)
  "Schedule an incremental roam index refresh for a saved in-vault FILE.
Called from init-aaronnote.el when the web editor emits a saved event."
  (when (and (stringp file)
             (not (string-empty-p file))
             (my/aaronnote-roam--note-in-vault-p (expand-file-name file)))
    (my/aaronnote-roam--schedule-runtime-sync (expand-file-name file))))

(provide 'init-md-roam)
;;; init-md-roam.el ends here
