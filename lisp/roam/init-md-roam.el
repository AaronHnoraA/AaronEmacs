;;; init-md-roam.el --- Markdown roam note navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Roam-style helpers for Aaronnote Markdown notes.

;;; Code:

(require 'config)

(require 'init-funcs)
(require 'init-md-roam-ui)
(require 'calendar)
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
(declare-function my/aaronnote--api-call "init-aaronnote" (channel args callback))
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")

(defgroup my/aaronnote-roam nil
  "Roam-style navigation for Aaronnote Markdown notes."
  :group 'my/aaronnote)

(defconst my/aaronnote-roam--module-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the Markdown roam Emacs bridge.")

(config-defvar my/aaronnote-roam-root nil
  "Root directory of the Markdown roam note vault."
  :type 'directory
  :group 'my/aaronnote-roam)

(config-defvar my/aaronnote-roam-recent-limit nil
  "Maximum number of recent Markdown roam notes kept in memory."
  :type 'integer
  :group 'my/aaronnote-roam)

(config-defvar my/aaronnote-roam-select-window-height nil
  "Height for the bottom Markdown roam selector window."
  :type '(choice (number :tag "Fraction or rows") (function :tag "Window height function"))
  :group 'my/aaronnote-roam)

(config-defvar my/aaronnote-roam-runtime-root nil
  "Root of the vendored Aaronnote runtime used by Markdown roam tooling."
  :type 'directory
  :group 'my/aaronnote-roam)

(config-defvar my/aaronnote-roam-runtime-cli nil
  "Node bridge used to call the vendored Aaronnote roam runtime from Emacs."
  :type 'file
  :group 'my/aaronnote-roam)

(defun my/aaronnote-roam--state-root ()
  "Return the Aaronnote var/state directory shared with the web host."
  (expand-file-name
   (if (boundp 'my/aaronnote--state-root)
       my/aaronnote--state-root
     "var/aaronnote")
   user-emacs-directory))

(defun my/aaronnote-roam--tmp-root ()
  "Return the Aaronnote runtime tmp directory shared with the web host."
  (expand-file-name
   (if (boundp 'my/aaronnote--tmp-root)
       my/aaronnote--tmp-root
     "tmp")
   (my/aaronnote-roam--state-root)))

(defvar my/aaronnote-roam--recent nil
  "Recently opened Markdown roam note ids, newest first.")

(defvar my/aaronnote-roam--runtime-index-cache nil)
(defvar my/aaronnote-roam--runtime-index-cache-key nil)
(defvar my/aaronnote-roam--sync-timer nil)
(defvar my/aaronnote-roam--sync-changed-files nil)
(defvar my/aaronnote-roam--sync-process nil
  "In-flight CLI offline sync process, or nil.")
(defvar my/aaronnote-roam--all-files-cache nil
  "Cached result of `my/aaronnote-roam--all-files'.")
(defvar my/aaronnote-roam--all-note-summaries-cache nil
  "Cached result of `my/aaronnote-roam--all-note-summaries'.")

(defun my/aaronnote-roam-root ()
  "Return the Markdown roam notes root."
  (or (when buffer-file-name
        (when-let* ((dir (locate-dominating-file buffer-file-name "roam.db")))
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
        my/aaronnote-roam--db-mtime nil
        my/aaronnote-roam--all-files-cache nil
        my/aaronnote-roam--all-note-summaries-cache nil))

(defun my/aaronnote-roam--runtime-available-p ()
  "Return non-nil when the Aaronnote runtime bridge is available."
  (and (file-exists-p my/aaronnote-roam-runtime-cli)
       (file-exists-p
        (expand-file-name "server/lib/index.mjs"
                          my/aaronnote-roam-runtime-root))))

(defun my/aaronnote-roam--action-to-channel (action)
  "Map roam-cli ACTION keyword to web-host /api channel string, or nil."
  (cdr (assoc action
              '(("index"     . "aaronnote:api:notes:roam-index")
                ("tags"      . "aaronnote:api:completions:tags")
                ("todos"     . "aaronnote:api:notes:todos")
                ("templates" . "aaronnote:api:notes:templates")
                ("update-todo" . "aaronnote:api:notes:update-todo")
                ("create"    . "aaronnote:api:notes:create-node")
                ("delete-node" . "aaronnote:api:notes:delete-node")
                ("sync"      . "aaronnote:api:notes:roam-sync")))))

(defun my/aaronnote-roam--runtime-call-via-api (action args)
  "Delegate ACTION with roam-cli ARGS to the running web-host /api.
Maps the action to its /api channel, converts positional ARGS to
the expected body, and returns parsed JSON or nil."
  (let ((channel (my/aaronnote-roam--action-to-channel action)))
    (when channel
      (let ((api-args
             (pcase action
               ("create"
                (let ((json-str (cadr (member "--json" args))))
                  (when json-str
                    (condition-case nil
                        (vector (json-parse-string json-str :object-type 'hash-table))
                      (error nil)))))
               ("delete-node"
                (vector (or (cadr (member "--file" args))
                            (cadr (member "--path" args))
                            "")))
               ("sync"
                (vector (if (member "--full" args) t :false)))
               ("todos"
                (let ((file (cadr (member "--file" args))))
                  (vector (or file ""))))
               ("update-todo"
                (let ((body (make-hash-table :test 'equal)))
                  (dolist (key '("--file" "--status" "--source" "--id"
                                 "--text" "--index"))
                    (when-let* ((value (cadr (member key args))))
                      (puthash (string-remove-prefix "--" key) value body)))
                  (vector body)))
               ("tags"
                (vector (make-hash-table)))
               (_
                []))))
        (when api-args
          (my/aaronnote--api-call-sync channel api-args))))))

(defun my/aaronnote-roam--runtime-call (action &rest args)
  "Call Aaronnote roam runtime ACTION synchronously with ARGS.
When the web-host is running, delegates to its /api so all callers share the
same in-memory index.  Falls back to spawning roam-cli.mjs when the web-host
is down (offline / not yet started)."
  (or
   ;; Prefer the running web-host's in-memory index.
   (and (boundp 'my/aaronnote--ready)
        my/aaronnote--ready
        (my/aaronnote-roam--runtime-call-via-api action args))
   ;; Fallback: spawn roam-cli.mjs directly.
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
                                    user-emacs-directory)
                            (format "AARONNOTE_STATE_DIR=%s"
                                    (my/aaronnote-roam--state-root))
                            (format "AARONNOTE_TMP_DIR=%s"
                                    (my/aaronnote-roam--tmp-root)))
                      process-environment))
             (stderr-file (make-temp-file "aaronnote-runtime-"))
             (status (apply #'process-file
                            "node" nil (list (current-buffer) stderr-file) nil
                            my/aaronnote-roam-runtime-cli
                            action
                            "--root" root
                            "--runtime" my/aaronnote-roam-runtime-root
                            "--workspace" user-emacs-directory
                            "--state" (my/aaronnote-roam--state-root)
                            "--tmp" (my/aaronnote-roam--tmp-root)
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
          (ignore-errors (delete-file stderr-file))))))))

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
  "Run Aaronnote roam-db sync via CLI subprocess — offline fallback only.
The web-host is the authoritative roam.db writer during normal operation.
Only call this when the web-host is not running.
When FULL is non-nil, force a full rebuild.  CHANGED-FILES are passed as
incremental hints."
  (if (not (my/aaronnote-roam--runtime-available-p))
      (message "Aaronnote roam runtime not found; cache refreshed only")
    (if (and my/aaronnote-roam--sync-process
             (process-live-p my/aaronnote-roam--sync-process))
        (message "Aaronnote roam: CLI sync already in flight, skipping")
      (let* ((root (my/aaronnote-roam-root))
             (buf (generate-new-buffer " *aaronnote-roam-sync*"))
             (args (append
                    (list my/aaronnote-roam-runtime-cli
                          "sync"
                          "--root" root
                          "--runtime" my/aaronnote-roam-runtime-root
                          "--workspace" user-emacs-directory
                          "--state" (my/aaronnote-roam--state-root)
                          "--tmp" (my/aaronnote-roam--tmp-root))
                    (when full (list "--full"))
                    (mapcan (lambda (file) (list "--changed" file))
                            (delete-dups (seq-filter #'identity changed-files)))))
             (process-environment
              (append (list (format "AARONNOTE_ROOT=%s" root)
                            (format "AARONNOTE_RUNTIME_ROOT=%s"
                                    (expand-file-name my/aaronnote-roam-runtime-root))
                            (format "AARONNOTE_WORKSPACE_ROOT=%s" user-emacs-directory)
                            (format "AARONNOTE_STATE_DIR=%s"
                                    (my/aaronnote-roam--state-root))
                            (format "AARONNOTE_TMP_DIR=%s"
                                    (my/aaronnote-roam--tmp-root)))
                      process-environment))
             (proc (make-process
                    :name "aaronnote-roam-sync"
                    :buffer buf
                    :command (cons "node" args)
                    :noquery t
                    :sentinel
                    (lambda (p event)
                      (when (memq (process-status p) '(exit signal))
                        (when (eq p my/aaronnote-roam--sync-process)
                          (setq my/aaronnote-roam--sync-process nil))
                        (my/aaronnote-roam--clear-runtime-cache)
                        (message "Aaronnote roam sync: %s" (string-trim event))
                        (when (buffer-live-p buf)
                          (kill-buffer buf)))))))
        (setq my/aaronnote-roam--sync-process proc)))))

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

(defun my/aaronnote-roam--parse-target (target &optional base-dir)
  "Parse note-link TARGET into Aaronnote-compatible target metadata.
BASE-DIR is forwarded to `my/aaronnote-roam--ref-to-file-fallback' for
plain-relative refs (./x, ../x); defaults to the current buffer's directory."
  (when-let* ((parts (my/aaronnote-roam--split-target target)))
    (let* ((ref (plist-get parts :ref))
           (resolved (my/aaronnote-roam--resolve-note ref))
           (id (or (plist-get resolved :id) ref))
           (file (or (plist-get resolved :file)
                     (my/aaronnote-roam--ref-to-file-fallback ref base-dir))))
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
  (or my/aaronnote-roam--all-files-cache
      (setq my/aaronnote-roam--all-files-cache
            (seq-filter
             (lambda (file)
               (let ((rel (file-relative-name file (my/aaronnote-roam-root))))
                 (not (string-match-p
                       "\\`\\(?:\\.git/\\|\\.lake/\\|_typst/\\|node_modules/\\)"
                       rel))))
             (directory-files-recursively
              (my/aaronnote-roam-root) "\\.\\(?:md\\|markdown\\)$")))))

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

(defun my/aaronnote-roam--ref-to-file-fallback (ref &optional base-dir)
  "Return the best filesystem fallback for unresolved note REF.
BASE-DIR is used for refs starting with ./ or ../ (plain-relative paths);
defaults to the current buffer's directory."
  (let* ((raw (string-trim (or ref ""))))
    (if (string-match-p "\\`\\.\\." raw)
        ;; Plain relative path: resolve against the note's own directory.
        (let* ((base (or base-dir
                         (and buffer-file-name
                              (file-name-directory buffer-file-name))
                         default-directory))
               (path (expand-file-name raw base)))
          (cond
           ((file-exists-p path) path)
           ((and (not (my/aaronnote-roam--ref-has-extension-p raw))
                 (file-exists-p (concat path ".md")))
            (concat path ".md"))
           ((and (not (my/aaronnote-roam--ref-has-extension-p raw))
                 (file-exists-p (concat path ".markdown")))
            (concat path ".markdown"))
           (t path)))
      ;; Non-relative: resolve against the roam vault root.
      (let* ((clean (my/aaronnote-roam--strip-vault-prefix raw))
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
          (concat path ".md")))))))

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

(defun my/aaronnote-roam--backlinks-map (records)
  "Return a hash-table mapping note id → list of backlink ids from RECORDS.
Builds the reverse-link index in one pass to avoid O(n²) per-note lookups."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (record records)
      (let* ((note   (plist-get record :note))
             (source (plist-get record :id))
             (links  (or (my/aaronnote-roam--note-list-field note "links")
                         (my/aaronnote-roam--note-list-field note "refs"))))
        (dolist (link links)
          (let ((target (my/aaronnote-roam--target-slug link)))
            (when target
              (puthash target
                       (cons source (gethash target map))
                       map))))))
    map))

(defun my/aaronnote-roam--all-note-summaries ()
  "Return note summary plists for all notes, memoised between syncs."
  (or my/aaronnote-roam--all-note-summaries-cache
      (setq my/aaronnote-roam--all-note-summaries-cache
            (let* ((records (sort (my/aaronnote-roam--note-records)
                                  (lambda (a b)
                                    (string< (plist-get a :id) (plist-get b :id)))))
                   ;; Build backlink map in one pass; fall back to DB field when present.
                   (bl-map (my/aaronnote-roam--backlinks-map records)))
              (mapcar (lambda (record)
                        (let* ((id   (plist-get record :id))
                               (note (plist-get record :note))
                               ;; Prefer the DB-provided backlinks field when available.
                               (bl   (or (my/aaronnote-roam--note-list-field note "backlinks")
                                         (delete-dups
                                          (nreverse (gethash id bl-map))))))
                          (list :slug      id
                                :title     (or (plist-get record :title)
                                               (my/aaronnote-roam--note-title id))
                                :path      (or (my/aaronnote-roam--note-field note "path")
                                               (my/aaronnote-roam--note-field note "link"))
                                :aliases   (my/aaronnote-roam--note-list-field note "aliases")
                                :tags      (my/aaronnote-roam--note-tags id)
                                :links     (my/aaronnote-roam--note-links id)
                                :backlinks bl
                                :summary   (my/aaronnote-roam--note-summary id))))
                      records)))))

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

(defun my/aaronnote-roam--open-file-smart (file parsed)
  "Open FILE using smart routing based on its type.
PARSED is the plist from `my/aaronnote-roam--parse-target'; it carries the
optional #tag / @dom target for Markdown notes."
  (cond
   ;; Directory → dired.
   ((file-directory-p file)
    (dired file))
   ;; Markdown note → Emacs + optional in-note navigation.
   ((string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
    (my/aaronnote-roam--touch-recent (plist-get parsed :slug))
    (find-file file)
    (cond
     ((plist-get parsed :id)
      (my/aaronnote-roam--goto-tag-id (plist-get parsed :id)))
     ((plist-get parsed :dom)
      (my/aaronnote-roam--goto-dom-target (plist-get parsed :dom)))))
   ;; Everything else (PDF, image, Lean source, etc.) → central open route.
   (t
    (require 'init-open)
    (my/open-file file))))

(defun my/aaronnote-roam-follow-link ()
  "Jump to the note or source region referenced at point.
Targets may use Aaronnote roam syntax:
  roam://note-id
  roam://note-id#tag
  roam://note-id@dom-target
Plain-relative refs (./x, ../x) are resolved against the current note's
directory; /x is resolved against the roam vault root."
  (interactive)
  (if (and (fboundp 'my/note-code-at-point)
           (my/note-code-at-point))
      (my/note-code-open-at-point)
    (let* ((base-dir (and buffer-file-name
                          (file-name-directory buffer-file-name)))
           (target (my/aaronnote-roam--target-at-point))
           (parsed (and target (my/aaronnote-roam--parse-target target base-dir)))
           (file (and parsed (plist-get parsed :file)))
           (ref (and parsed (plist-get parsed :ref))))
      (if (and file (file-exists-p file))
          (my/aaronnote-roam--open-file-smart file parsed)
        (if ref
            (when (yes-or-no-p (format "Note '%s' not found. Create it? " ref))
              (my/aaronnote-roam-new-note ref))
          (user-error "No Markdown roam link found at point"))))))

(defun my/aaronnote-roam-find-note ()
  "Find a roam note by Aaronnote id/path/title with completion."
  (interactive)
  (my/aaronnote-roam--open-slug
   (my/aaronnote-roam--read-note-id "Roam note: ")))

(defun my/aaronnote-roam-delete-node (note-id)
  "Move NOTE-ID's Markdown node to trash through the Aaronnote runtime."
  (interactive (list (my/aaronnote-roam--read-note-id "Delete roam node: ")))
  (let* ((record (seq-find
                  (lambda (candidate)
                    (equal (plist-get candidate :id) note-id))
                  (my/aaronnote-roam--note-records)))
         (file (plist-get record :file))
         (title (plist-get record :title)))
    (unless (and record file (file-exists-p file))
      (user-error "No file found for node: %s" note-id))
    (when (yes-or-no-p
           (format "Move node '%s' (%s) to trash? "
                   (or title note-id)
                   (abbreviate-file-name file)))
      (let ((delete-current-buffer
             (and buffer-file-name
                  (file-equal-p buffer-file-name file))))
        (my/aaronnote-roam--runtime-call "delete-node" "--file" file)
        (my/aaronnote-roam--clear-runtime-cache)
        (when delete-current-buffer
          (kill-buffer (current-buffer)))
        (message "Aaronnote node moved to trash: %s"
                 (abbreviate-file-name file))))))

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

(defun my/aaronnote-roam-new--path-directory (path)
  "Return PATH's vault-relative directory."
  (my/aaronnote-roam-new--normalize-directory
   (or (file-name-directory (or path "")) "")))

(defun my/aaronnote-roam-new--path-basename (path title)
  "Return PATH's filename, falling back to TITLE's default Markdown filename."
  (let ((name (file-name-nondirectory (or path ""))))
    (if (string-empty-p name)
        (file-name-nondirectory
         (my/aaronnote-roam-new--default-path title))
      name)))

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
  (my/aaronnote-roam-new-render t))

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
    (my/aaronnote-roam-new-render t)))

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
    (my/aaronnote-roam-new-render t)))

(defun my/aaronnote-roam-new-edit-path ()
  "Edit the save directory in the current Roam New draft."
  (interactive)
  (my/aaronnote-roam-new--sync-draft-from-widgets)
  (let* ((root (file-name-as-directory (expand-file-name (my/aaronnote-roam-root))))
         (current (plist-get my/aaronnote-roam-new--draft :path))
         (title (plist-get my/aaronnote-roam-new--draft :title))
         (current-dir (my/aaronnote-roam-new--path-directory current))
         (filename (my/aaronnote-roam-new--path-basename current title))
         (initial-dir (expand-file-name current-dir root))
         (raw-dir (read-directory-name "Save directory: "
                                       initial-dir initial-dir nil))
         (selected-dir (file-name-as-directory (expand-file-name raw-dir root))))
    (unless (file-in-directory-p selected-dir root)
      (user-error "Save directory must be inside the Aaronnote vault"))
    (my/aaronnote-roam-new--set
     :path
     (my/aaronnote-roam-new--unique-path
      (concat
       (let ((relative-dir
              (my/aaronnote-roam-new--normalize-directory
               (file-relative-name selected-dir root))))
         (if (string-empty-p relative-dir)
             ""
           (concat relative-dir "/")))
       filename)))))

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
    (id icon label value detail key &optional placeholder action)
  "Insert directly editable Roam New field KEY.
ID, ICON, LABEL, VALUE, DETAIL, PLACEHOLDER, and ACTION control display."
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
            (row-action
             (if action
                 (lambda (_ignored) (call-interactively action))
               (let ((w widget))
                 (lambda (_ignored)
                   (when-let* ((marker (widget-get w :from)))
                     (goto-char marker)))))))
        (add-text-properties
         start end
         `(aaron-ui-board--item-id ,id
           help-echo ,(if action
                          (format "RET: edit %s; type in field to edit directly"
                                  (downcase label))
                        (format "RET: jump into %s field; type to edit"
                                (downcase label)))))
        ;; Apply row-action only to the label area so the widget's own keymap is not masked.
        (add-text-properties
         start label-end
         `(aaron-ui-board--row-action ,row-action
           mouse-face aaron-ui-board-row-highlight
           keymap ,my/aaronnote-roam-ui-row-map))))))

(defun my/aaronnote-roam-new-render (&optional skip-sync)
  "Render the current Roam New draft."
  (interactive)
  (unless skip-sync
    (my/aaronnote-roam-new--sync-draft-from-widgets))
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
        "Vault-relative .md or .markdown path; p chooses a folder."
        'path "untitled.md" #'my/aaronnote-roam-new-edit-path)
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
        'tags "" #'my/aaronnote-roam-new-edit-tags)
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
    (unless (get-text-property (point) 'aaron-ui-board--item-id)
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
  "Refresh Markdown roam cache and sync roam.db via Aaronnote runtime.
With prefix argument FULL, force a full roam-db rebuild.
When the web-host is running, delegates to its /api (async, non-blocking).
Falls back to a CLI subprocess when the web-host is offline."
  (interactive "P")
  (my/aaronnote-roam--clear-runtime-cache)
  (cond
   ;; Online: delegate to web-host /api; it is the authoritative writer.
   ((and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (message "Aaronnote: syncing roam DB...")
    (my/aaronnote--api-call
     (if full "aaronnote:api:notes:roam-sync-full" "aaronnote:api:notes:roam-sync")
     (if full [] [t])
     (lambda (_result)
       (my/aaronnote-roam--clear-runtime-cache)
       (message "Aaronnote roam sync: done"))))
   ;; Offline fallback: CLI subprocess.
   ((my/aaronnote-roam--runtime-available-p)
    (my/aaronnote-roam--runtime-sync full nil))
   (t
    (message "Markdown roam cache refreshed"))))

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
              (let* ((line-start (line-beginning-position))
                     (line-end (line-end-position))
                     (line (string-trim
                            (buffer-substring-no-properties
                             line-start line-end))))
                (when (or (string-match-p "\\`@@todo\\b" line)
                          (string-match-p "\\`\\(?:[-*+]\\s-+\\)?\\[ \\]" line)
                          (string-match-p "\\_<TODO\\_>" line))
                  (let ((entry (make-hash-table :test 'equal)))
                    (puthash "note" (plist-get record :id) entry)
                    (puthash "noteId" (plist-get record :id) entry)
                    (puthash "noteKey" (plist-get record :key) entry)
                    (puthash "title" (plist-get record :title) entry)
                    (puthash "noteTitle" (plist-get record :title) entry)
                    (puthash "file" file entry)
                    (puthash "path" (plist-get record :path) entry)
                    (puthash "line" (line-number-at-pos line-start t) entry)
                    (puthash "column" 1 entry)
                    (puthash "index" (1- line-start) entry)
                    (puthash "source" line entry)
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
  (let ((status (downcase
                 (string-trim
                  (format "%s" (or (my/aaronnote-roam--todo-field entry "status")
                                   "todo"))))))
    (cond
     ((member status '("" " " "open" "unchecked")) "todo")
     ((member status '("~" "-" "wip" "active" "in-progress")) "doing")
     ((member status '("x" "checked" "complete" "completed")) "done")
     ((member status '("!" "block")) "blocked")
     ((member status '("cancel" "canceled" "cancelled")) "cancelled")
     (t status))))

(defun my/aaronnote-roam--todo-tone (entry)
  "Return display tone for todo ENTRY."
  (pcase (my/aaronnote-roam--todo-status entry)
    ((or "done" "complete" "completed") 'success)
    ((or "blocked" "cancelled" "canceled") 'danger)
    ((or "doing" "waiting" "in-progress") 'warning)
    (_ 'info)))

(defun my/aaronnote-roam--visit-todo (entry)
  "Open the note and source line represented by todo ENTRY."
  (let* ((file (my/aaronnote-roam--todo-field entry "file"))
         (note-slug (my/aaronnote-roam--todo-field
                     entry "note" "noteId" "noteKey" "path"))
         (line (my/aaronnote-roam--todo-field entry "line"))
         (column (my/aaronnote-roam--todo-field entry "column"))
         (index (my/aaronnote-roam--todo-field entry "index"))
         (source (my/aaronnote-roam--todo-field entry "source")))
    (cond
     ((and (stringp file) (not (string-empty-p file)) (file-exists-p file))
      (find-file file))
     (note-slug
      (my/aaronnote-roam--open-slug note-slug))
     (t
      (user-error "Todo has no source note")))
    (cond
     ((and (integerp index) (>= index 0))
      (goto-char (min (point-max) (1+ index)))
      (when (and (stringp source) (not (string-empty-p source))
                 (not (looking-at-p (regexp-quote source))))
        (let ((line-end (line-end-position)))
          (when (search-forward source line-end t)
            (goto-char (match-beginning 0))))))
     ((integerp line)
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (when (integerp column)
        (forward-char (min (max 0 (1- column))
                           (- (line-end-position) (point)))))))
    (recenter)))

(defun my/aaronnote-roam--todo-at-point ()
  "Return the todo entry on the current row."
  (or (get-text-property (point) 'my/aaronnote-roam-todo)
      (get-text-property (line-beginning-position) 'my/aaronnote-roam-todo)
      (get-text-property (max (point-min) (1- (point)))
                         'my/aaronnote-roam-todo)))

(defun my/aaronnote-roam--todo-update-local (entry status)
  "Update todo ENTRY to STATUS by editing its source file locally."
  (let* ((file (my/aaronnote-roam--todo-field entry "file"))
         (index (my/aaronnote-roam--todo-field entry "index"))
         (source (my/aaronnote-roam--todo-field entry "source"))
         (status (downcase (format "%s" status)))
         (prefix (if (string= status "todo")
                     "@@todo "
                   (format "@@todo(%s) " status))))
    (unless (and (stringp file) (file-exists-p file))
      (user-error "Todo has no editable source file"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (cond
           ((and (integerp index) (>= index 0))
            (goto-char (min (point-max) (1+ index))))
           (t
            (goto-char (point-min))))
          (unless (or (looking-at "@@todo\\(?:([^)\n]*)\\)?[ \t]+")
                      (and (stringp source)
                           (search-forward source nil t)
                           (goto-char (match-beginning 0))
                           (looking-at "@@todo\\(?:([^)\n]*)\\)?[ \t]+")))
            (user-error "Todo source was not found"))
          (replace-match prefix t t nil 0)))
      (save-buffer)))
  (my/aaronnote-roam--clear-runtime-cache))

(defun my/aaronnote-roam-update-todo-status (status &optional entry)
  "Set current todo ENTRY to STATUS and refresh the current task view."
  (interactive
   (list (completing-read "Todo status: " '("todo" "doing" "blocked" "done" "cancelled")
                          nil t nil nil "done")
         nil))
  (let* ((entry (or entry (my/aaronnote-roam--todo-at-point)))
         (file (my/aaronnote-roam--todo-field entry "file"))
         (index (my/aaronnote-roam--todo-field entry "index"))
         (source (my/aaronnote-roam--todo-field entry "source"))
         (id (my/aaronnote-roam--todo-field entry "id"))
         (text (my/aaronnote-roam--todo-field entry "text")))
    (unless entry
      (user-error "No todo on this line"))
    (or (and file
             (my/aaronnote-roam--runtime-call
              "update-todo"
              "--file" file
              "--status" status
              "--index" (format "%s" (or index ""))
              "--source" (or source "")
              "--id" (or id "")
              "--text" (or text "")))
        (my/aaronnote-roam--todo-update-local entry status))
    (my/aaronnote-roam--clear-runtime-cache)
    (message "Todo marked %s" status)
    (my/aaronnote-roam-ui-refresh)))

(defun my/aaronnote-roam-todo-done ()
  "Mark the current roam todo done."
  (interactive)
  (my/aaronnote-roam-update-todo-status "done"))

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
         (tags (my/aaronnote-roam--todo-tags entry))
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
     :tags tags
     :action (let ((todo entry))
               (lambda (_button)
                 (my/aaronnote-roam--visit-todo todo)))
     :properties `(my/aaronnote-roam-todo ,entry))))

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

(defun my/aaronnote-roam--search-parse-term (term)
  "Parse a search TERM into a plist with :scope and :value.
Scopes: title, category (nested-tag prefix), linksto, plain."
  (cond
   ((string-match "\\`\\(?:intitle\\|title\\):\\(.+\\)\\'" term)
    (list :scope 'title :value (match-string 1 term)))
   ((string-match "\\`\\(?:incategory\\|tag\\):\\(.+\\)\\'" term)
    (list :scope 'category :value (match-string 1 term)))
   ((string-match "\\`linksto:\\(.+\\)\\'" term)
    (list :scope 'linksto :value (match-string 1 term)))
   (t (list :scope 'plain :value term))))

(defun my/aaronnote-roam--search-match-p (entry parsed-term)
  "Return non-nil when ENTRY matches PARSED-TERM."
  (let ((scope (plist-get parsed-term :scope))
        (value (plist-get parsed-term :value)))
    (pcase scope
      ('title
       (string-match-p (regexp-quote value)
                       (downcase (or (plist-get entry :title) ""))))
      ('category
       (seq-some (lambda (tag)
                   (or (string= (downcase tag) value)
                       (string-prefix-p (concat value "/") (downcase tag))))
                 (or (plist-get entry :tags) nil)))
      ('linksto
       (seq-some (lambda (link) (string-match-p (regexp-quote value) (downcase link)))
                 (or (plist-get entry :links) nil)))
      (_
       (string-match-p (regexp-quote value)
                       (downcase (my/aaronnote-roam--candidate-haystack entry)))))))

(defun my/aaronnote-roam-search-notes (&optional query)
  "Search notes with optional scoped operators.
Operators: intitle:TEXT, incategory:TAG, tag:TAG, linksto:SLUG, plain text.
Multiple terms are ANDed."
  (interactive)
  (let* ((query (or query (read-string "Search notes (intitle: incategory: linksto:): ")))
         (raw-parts (split-string (downcase (string-trim query)) "\\s-+" t))
         (parsed (mapcar #'my/aaronnote-roam--search-parse-term raw-parts))
         (entries (seq-filter
                   (lambda (entry)
                     (seq-every-p
                      (lambda (term) (my/aaronnote-roam--search-match-p entry term))
                      parsed))
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

(defconst my/aaronnote-roam--agenda-date-fields
  '("ddl" "deadline" "due" "scheduled" "start" "when" "date")
  "Todo fields considered date-like in the agenda.")

(defconst my/aaronnote-roam--agenda-closed-statuses
  '("done" "complete" "completed" "cancelled" "canceled")
  "Todo statuses treated as closed in the agenda.")

(defun my/aaronnote-roam--todo-value (entry &rest keys)
  "Return the first non-empty todo ENTRY value for KEYS.
This checks top-level todo fields first, then the nested args object."
  (or (seq-some
       (lambda (key)
         (let ((value (my/aaronnote-roam--todo-field entry key)))
           (and value
                (not (and (stringp value) (string-empty-p value)))
                value)))
       keys)
      (when-let* ((args (my/aaronnote-roam--todo-field entry "args")))
        (seq-some
         (lambda (key)
           (let ((value (my/aaronnote-roam--todo-field args key)))
             (and value
                  (not (and (stringp value) (string-empty-p value)))
                  value)))
         keys))))

(defun my/aaronnote-roam--todo-string-value (entry &rest keys)
  "Return a trimmed string todo ENTRY value for KEYS, or nil."
  (when-let* ((value (apply #'my/aaronnote-roam--todo-value entry keys))
              (string (string-trim (format "%s" value))))
    (unless (string-empty-p string)
      string)))

(defun my/aaronnote-roam--todo-list-value (entry &rest keys)
  "Return a string list todo ENTRY value for KEYS."
  (let ((value (apply #'my/aaronnote-roam--todo-value entry keys)))
    (cond
     ((null value) nil)
     ((vectorp value)
      (mapcar (lambda (item) (format "%s" item)) (append value nil)))
     ((listp value)
      (seq-filter
       (lambda (item) (not (string-empty-p item)))
       (mapcar (lambda (item) (string-trim (format "%s" item))) value)))
     ((stringp value)
      (split-string value "[,[:space:]]+" t))
     (t (list (format "%s" value))))))

(defun my/aaronnote-roam--todo-tags (entry)
  "Return file and inline tags inherited by todo ENTRY."
  (delete-dups
   (seq-filter
    (lambda (tag) (not (string-empty-p tag)))
    (append (my/aaronnote-roam--todo-list-value entry "tags")
            (my/aaronnote-roam--todo-list-value entry "inlineTags")))))

(defun my/aaronnote-roam--todo-ddl (entry)
  "Return deadline string for todo ENTRY, or nil."
  (my/aaronnote-roam--todo-string-value entry "ddl" "deadline" "due"))

(defun my/aaronnote-roam--date-day-string (value)
  "Return YYYY-MM-DD for date-like VALUE, or nil."
  (when-let* ((raw (and value (string-trim (format "%s" value)))))
    (unless (string-empty-p raw)
      (let ((lower (downcase raw)))
        (cond
         ((member lower '("today" "今天"))
          (format-time-string "%Y-%m-%d"))
         ((member lower '("tomorrow" "明天"))
          (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 1))))
         ((member lower '("yesterday" "昨天"))
          (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1))))
         ((string-match
           "\\`\\([0-9]\\{4\\}\\)[-./年]\\([0-9]\\{1,2\\}\\)[-./月]\\([0-9]\\{1,2\\}\\)"
           raw)
          (format "%04d-%02d-%02d"
                  (string-to-number (match-string 1 raw))
                  (string-to-number (match-string 2 raw))
                  (string-to-number (match-string 3 raw))))
         ((not (string-match-p "[0-9]" raw))
          nil)
         (t
          (condition-case nil
              (format-time-string "%Y-%m-%d"
                                  (encode-time (parse-time-string raw)))
            (error nil))))))))

(defun my/aaronnote-roam--todo-agenda-date (entry)
  "Return the main agenda date string for todo ENTRY, or nil."
  (seq-some
   (lambda (key)
     (my/aaronnote-roam--date-day-string
      (my/aaronnote-roam--todo-value entry key)))
   my/aaronnote-roam--agenda-date-fields))

(defun my/aaronnote-roam--todo-overdue-p (ddl)
  "Return non-nil when DDL string is in the past."
  (when-let* ((day (my/aaronnote-roam--date-day-string ddl)))
    (string< day (format-time-string "%Y-%m-%d"))))

(defun my/aaronnote-roam--todo-closed-p (entry)
  "Return non-nil when todo ENTRY is done or cancelled."
  (member (my/aaronnote-roam--todo-status entry)
          my/aaronnote-roam--agenda-closed-statuses))

(defun my/aaronnote-roam--todo-cancelled-p (entry)
  "Return non-nil when todo ENTRY is cancelled."
  (member (my/aaronnote-roam--todo-status entry) '("cancelled" "canceled")))

(defun my/aaronnote-roam--todo-done-p (entry)
  "Return non-nil when todo ENTRY is done."
  (member (my/aaronnote-roam--todo-status entry)
          '("done" "complete" "completed")))

(defun my/aaronnote-roam--agenda-status-match-p (entry wanted)
  "Return non-nil when todo ENTRY matches WANTED status."
  (let* ((raw (downcase (string-trim (format "%s" wanted))))
         (wanted (if (string= raw "open")
                     "open"
                   (my/aaronnote-roam--todo-status `(:status ,raw)))))
    (pcase wanted
      ("open" (not (my/aaronnote-roam--todo-closed-p entry)))
      ("done" (my/aaronnote-roam--todo-done-p entry))
      ("cancelled" (my/aaronnote-roam--todo-cancelled-p entry))
      (_ (string= (my/aaronnote-roam--todo-status entry) wanted)))))

(defun my/aaronnote-roam--agenda-todo-haystack (entry)
  "Return searchable text for todo ENTRY."
  (downcase
   (string-join
    (delq nil
          (append
           (list (my/aaronnote-roam--todo-string-value entry "text" "context" "source")
                 (my/aaronnote-roam--todo-string-value entry "title" "noteTitle" "parentTitle")
                 (my/aaronnote-roam--todo-string-value entry "note" "noteId" "roamId" "noteKey")
                 (my/aaronnote-roam--todo-string-value entry "path" "file" "parentFile")
                 (my/aaronnote-roam--todo-string-value entry "groupKey" "groupLabel"))
           (my/aaronnote-roam--todo-tags entry)))
    " ")))

(defun my/aaronnote-roam--agenda-field-contains-p (entry value &rest keys)
  "Return non-nil when ENTRY fields KEYS contain VALUE."
  (let ((needle (downcase value))
        (haystack (downcase
                   (string-join
                    (delq nil
                          (mapcar (lambda (key)
                                    (my/aaronnote-roam--todo-string-value entry key))
                                  keys))
                    " "))))
    (string-match-p (regexp-quote needle) haystack)))

(defun my/aaronnote-roam--agenda-tag-match-p (entry value)
  "Return non-nil when todo ENTRY has a tag matching VALUE."
  (let ((needle (downcase value)))
    (seq-some
     (lambda (tag)
       (let ((tag (downcase tag)))
         (or (string= tag needle)
             (string-prefix-p (concat needle "/") tag)
             (string-match-p (regexp-quote needle) tag))))
     (my/aaronnote-roam--todo-tags entry))))

(defun my/aaronnote-roam--agenda-date-in-range-p (entry from to)
  "Return non-nil when ENTRY agenda date is within FROM and TO."
  (when-let* ((day (my/aaronnote-roam--todo-agenda-date entry)))
    (and (or (null from) (not (string< day from)))
         (or (null to) (not (string< to day))))))

(defun my/aaronnote-roam--agenda-search-term-match-p (entry term)
  "Return non-nil when todo ENTRY matches one search TERM."
  (if (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" term)
      (let ((key (downcase (match-string 1 term)))
            (value (downcase (match-string 2 term))))
        (pcase key
          ("status" (my/aaronnote-roam--agenda-status-match-p entry value))
          ("tag" (my/aaronnote-roam--agenda-tag-match-p entry value))
          ((or "title" "intitle")
           (my/aaronnote-roam--agenda-field-contains-p
            entry value "title" "noteTitle" "parentTitle"))
          ((or "roamid" "id")
           (my/aaronnote-roam--agenda-field-contains-p
            entry value "roamId" "noteId" "note" "noteKey" "id"))
          ("file"
           (my/aaronnote-roam--agenda-field-contains-p
            entry value "path" "file" "parentFile"))
          ((or "parent" "group")
           (my/aaronnote-roam--agenda-field-contains-p
            entry value "groupKey" "groupLabel" "parentFile" "path"))
          ("date"
           (let ((day (my/aaronnote-roam--date-day-string value)))
             (and day (equal day (my/aaronnote-roam--todo-agenda-date entry)))))
          ("from"
           (my/aaronnote-roam--agenda-date-in-range-p
            entry (my/aaronnote-roam--date-day-string value) nil))
          ("to"
           (my/aaronnote-roam--agenda-date-in-range-p
            entry nil (my/aaronnote-roam--date-day-string value)))
          (_
           (string-match-p (regexp-quote value)
                           (my/aaronnote-roam--agenda-todo-haystack entry)))))
    (string-match-p (regexp-quote (downcase term))
                    (my/aaronnote-roam--agenda-todo-haystack entry))))

(defun my/aaronnote-roam--agenda-search-match-p (entry query)
  "Return non-nil when todo ENTRY matches agenda QUERY."
  (let ((terms (split-string (string-trim (or query "")) "\\s-+" t)))
    (or (null terms)
        (seq-every-p
         (lambda (term)
           (my/aaronnote-roam--agenda-search-term-match-p entry term))
         terms))))

(defun my/aaronnote-roam--agenda-filter-todos (todos mode query)
  "Filter TODOS for agenda MODE and optional QUERY."
  (let ((today (format-time-string "%Y-%m-%d")))
    (seq-filter
     (lambda (entry)
       (pcase mode
         ('all t)
         ('done (my/aaronnote-roam--todo-done-p entry))
         ('cancelled (my/aaronnote-roam--todo-cancelled-p entry))
         ('today
          (and (not (my/aaronnote-roam--todo-closed-p entry))
               (equal (my/aaronnote-roam--todo-agenda-date entry) today)))
         ('overdue
          (and (not (my/aaronnote-roam--todo-closed-p entry))
               (my/aaronnote-roam--todo-overdue-p
                (my/aaronnote-roam--todo-agenda-date entry))))
         ('date
          (and (not (my/aaronnote-roam--todo-closed-p entry))
               (equal (my/aaronnote-roam--todo-agenda-date entry)
                      (my/aaronnote-roam--date-day-string query))))
         ('search
          (my/aaronnote-roam--agenda-search-match-p entry query))
         (_
          (not (my/aaronnote-roam--todo-closed-p entry)))))
     (or todos '()))))

(defun my/aaronnote-roam--agenda-sort-todos (todos)
  "Return TODOS sorted by agenda date, title, and text."
  (sort
   (copy-sequence todos)
   (lambda (a b)
     (let ((date-a (my/aaronnote-roam--todo-agenda-date a))
           (date-b (my/aaronnote-roam--todo-agenda-date b))
           (title-a (or (my/aaronnote-roam--todo-string-value a "title" "noteTitle")
                        ""))
           (title-b (or (my/aaronnote-roam--todo-string-value b "title" "noteTitle")
                        ""))
           (text-a (or (my/aaronnote-roam--todo-string-value a "text" "source")
                       ""))
           (text-b (or (my/aaronnote-roam--todo-string-value b "text" "source")
                       "")))
       (cond
        ((and date-a date-b (not (string= date-a date-b)))
         (string< date-a date-b))
        ((and date-a (not date-b)) t)
        ((and date-b (not date-a)) nil)
        ((not (string= title-a title-b)) (string< title-a title-b))
        (t (string< text-a text-b)))))))

(defun my/aaronnote-roam--agenda-groups (todos)
  "Return agenda groups for TODOS."
  (let ((today-str (format-time-string "%Y-%m-%d"))
        overdue today upcoming no-ddl done cancelled)
    (dolist (entry todos)
      (let ((date (my/aaronnote-roam--todo-agenda-date entry)))
        (cond
         ((my/aaronnote-roam--todo-cancelled-p entry)
          (push entry cancelled))
         ((my/aaronnote-roam--todo-done-p entry)
          (push entry done))
         ((and date (string= date today-str))
          (push entry today))
         ((and date (my/aaronnote-roam--todo-overdue-p date))
          (push entry overdue))
         (date
          (push entry upcoming))
         (t
          (push entry no-ddl)))))
    `(("Overdue" . (,(my/aaronnote-roam--agenda-sort-todos overdue) . danger))
      ("Today" . (,(my/aaronnote-roam--agenda-sort-todos today) . warning))
      ("Upcoming" . (,(my/aaronnote-roam--agenda-sort-todos upcoming) . info))
      ("No deadline" . (,(my/aaronnote-roam--agenda-sort-todos no-ddl) . muted))
      ("Done" . (,(my/aaronnote-roam--agenda-sort-todos done) . success))
      ("Cancelled" . (,(my/aaronnote-roam--agenda-sort-todos cancelled) . danger)))))

(defun my/aaronnote-roam--agenda-stats (todos)
  "Return agenda stat badges for all TODOS."
  (let* ((open (seq-count
                (lambda (entry)
                  (not (my/aaronnote-roam--todo-closed-p entry)))
                todos))
         (done (seq-count #'my/aaronnote-roam--todo-done-p todos))
         (cancelled (seq-count #'my/aaronnote-roam--todo-cancelled-p todos))
         (today-str (format-time-string "%Y-%m-%d"))
         (today (seq-count
                 (lambda (entry)
                   (and (not (my/aaronnote-roam--todo-closed-p entry))
                        (equal (my/aaronnote-roam--todo-agenda-date entry)
                               today-str)))
                 todos))
         (overdue (seq-count
                   (lambda (entry)
                     (and (not (my/aaronnote-roam--todo-closed-p entry))
                          (my/aaronnote-roam--todo-overdue-p
                           (my/aaronnote-roam--todo-agenda-date entry))))
                   todos)))
    (list (cons (format "%d overdue" overdue) (if (> overdue 0) 'danger 'muted))
          (cons (format "%d today" today) (if (> today 0) 'warning 'muted))
          (cons (format "%d open" open) 'info)
          (cons (format "%d done" done) 'success)
          (cons (format "%d cancelled" cancelled) (if (> cancelled 0) 'danger 'muted)))))

(defun my/aaronnote-roam--agenda-subtitle (mode query count)
  "Return agenda subtitle for MODE, QUERY, and COUNT."
  (pcase mode
    ('all (format "All Aaronnote tasks, %d shown" count))
    ('done (format "Completed Aaronnote tasks, %d shown" count))
    ('cancelled (format "Cancelled Aaronnote tasks, %d shown" count))
    ('today (format "Open tasks due today, %d shown" count))
    ('overdue (format "Overdue open tasks, %d shown" count))
    ('date (format "Open tasks dated %s, %d shown" query count))
    ('search (format "Agenda search: %s, %d shown" query count))
    (_ (format "Open Aaronnote tasks for %s, %d shown"
               (format-time-string "%Y-%m-%d") count))))

(defun my/aaronnote-roam-agenda-search (&optional query)
  "Search the roam agenda with QUERY."
  (interactive (list (read-string
                      "Agenda search (status: tag: title: roamid: file: parent: date: from: to:): ")))
  (my/aaronnote-roam-agenda 'search query))

(defun my/aaronnote-roam--agenda-actions (mode query)
  "Return agenda toolbar actions for MODE and QUERY."
  (my/aaronnote-roam--ui-actions
   `((:label "Open"
      :command ,(lambda () (my/aaronnote-roam-agenda 'open nil))
      :help "Show open tasks"
      :primary ,(memq mode '(open nil)))
     (:label "All"
      :command ,(lambda () (my/aaronnote-roam-agenda 'all nil))
      :help "Show all tasks"
      :primary ,(eq mode 'all))
     (:label "Done"
      :command ,(lambda () (my/aaronnote-roam-agenda 'done nil))
      :help "Show completed tasks"
      :primary ,(eq mode 'done))
     (:label "Cancelled"
      :command ,(lambda () (my/aaronnote-roam-agenda 'cancelled nil))
      :help "Show cancelled tasks"
      :primary ,(eq mode 'cancelled))
     (:label "Today"
      :command ,(lambda () (my/aaronnote-roam-agenda 'today nil))
      :help "Show open tasks due today"
      :primary ,(eq mode 'today))
     (:label "Overdue"
      :command ,(lambda () (my/aaronnote-roam-agenda 'overdue nil))
      :help "Show overdue open tasks"
      :primary ,(eq mode 'overdue))
     (:label "Search"
      :command my/aaronnote-roam-agenda-search
      :help "Search agenda tasks"
      :primary ,(eq mode 'search))
     (:label "Calendar"
      :command my/aaronnote-roam-agenda-calendar
      :help "Show agenda deadline calendar"))))

(defun my/aaronnote-roam--agenda-update-todo (entry status)
  "Set todo ENTRY to STATUS and refresh the agenda."
  (my/aaronnote-roam-update-todo-status status entry))

(defun my/aaronnote-roam--agenda-compact-text (text width)
  "Return TEXT compacted to display WIDTH."
  (truncate-string-to-width (string-trim (or text "")) width nil nil "…"))

(defun my/aaronnote-roam--insert-agenda-todo-row (entry &optional deadline-tone)
  "Insert one agenda row for todo ENTRY with optional DEADLINE-TONE."
  (let* ((note-title (or (my/aaronnote-roam--todo-string-value
                          entry "title" "noteTitle")
                         (my/aaronnote-roam--todo-string-value
                          entry "note" "noteId" "path")
                         "Unknown note"))
         (text (or (my/aaronnote-roam--todo-string-value
                    entry "text" "context" "source")
                   "(empty todo)"))
         (line (my/aaronnote-roam--todo-field entry "line"))
         (date (or (my/aaronnote-roam--todo-agenda-date entry) "no-date"))
         (status (upcase (my/aaronnote-roam--todo-status entry)))
         (width (max 88 (window-body-width)))
         (reserved 64)
         (text-width (max 22 (min 72 (- width reserved))))
         (start (point)))
    (insert "  ")
    (my/aaronnote-roam-ui-insert-badge
     (format "%-9s" status)
     (or deadline-tone (my/aaronnote-roam--todo-tone entry)))
    (insert " ")
    (insert (propertize (format "%-10s " date)
                        'face 'my/aaronnote-roam-ui-meta))
    (insert-text-button
     (format "%-22s  %s"
             (my/aaronnote-roam--agenda-compact-text note-title 22)
             (my/aaronnote-roam--agenda-compact-text text text-width))
     'action (let ((todo entry))
               (lambda (_button)
                 (my/aaronnote-roam--visit-todo todo)))
     'follow-link t
     'help-echo "Open todo source"
     'face 'my/aaronnote-roam-ui-row-title
     'my/aaronnote-roam-todo entry)
    (insert (propertize
             (format "  L%-4s"
                     (if (integerp line) line "-"))
             'face 'my/aaronnote-roam-ui-meta))
    (insert " ")
    (if (my/aaronnote-roam--todo-closed-p entry)
        (my/aaronnote-roam-ui-insert-actions
         `((:label "Reopen"
            :command ,(let ((todo entry))
                        (lambda ()
                          (my/aaronnote-roam--agenda-update-todo todo "todo")))
            :help "Mark this task todo")
           (:label "Doing"
            :command ,(let ((todo entry))
                        (lambda ()
                          (my/aaronnote-roam--agenda-update-todo todo "doing")))
            :help "Mark this task doing")))
      (my/aaronnote-roam-ui-insert-actions
       `((:label "Done"
          :command ,(let ((todo entry))
                      (lambda ()
                        (my/aaronnote-roam--agenda-update-todo todo "done")))
          :help "Mark this task done"
          :primary t)
         (:label "Cancel"
          :command ,(let ((todo entry))
                      (lambda ()
                        (my/aaronnote-roam--agenda-update-todo todo "cancelled")))
          :help "Mark this task cancelled"))))
    (add-text-properties start (point) `(my/aaronnote-roam-todo ,entry))
    (insert "\n")))

(defun my/aaronnote-roam--current-buffer-todos ()
  "Return lightweight todo entries scanned from the current buffer."
  (let ((file (buffer-file-name))
        todos)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line (string-trim
                        (buffer-substring-no-properties line-start line-end)))
                 (ddl (and
                       (string-match
                        "{[^}\n]*\\(?:ddl\\|deadline\\|due\\)\\s-*[:=]\\s-*\\([^,;} \t\n]+\\)"
                        line)
                       (match-string 1 line))))
            (when (or (string-match-p "\\`@@todo\\b" line)
                      (string-match-p "\\`\\(?:[-*+]\\s-+\\)?\\[ \\]" line)
                      (string-match-p "\\_<TODO\\_>" line))
              (let ((entry (list :file file
                                 :path (and file
                                            (file-relative-name
                                             file (my/aaronnote-roam-root)))
                                 :line (line-number-at-pos line-start t)
                                 :column 1
                                 :index (1- line-start)
                                 :source line
                                 :text line
                                 :ddl ddl
                                 :status (if (string-match
                                              "\\`@@todo(\\([^)\n]+\\))"
                                              line)
                                             (match-string 1 line)
                                           "todo"))))
                (push entry todos))))
          (forward-line 1))))
    (nreverse todos)))

(defun my/aaronnote-roam--current-file-todos ()
  "Return todo entries for the current file."
  (let* ((file (buffer-file-name))
         (truename (and file (file-truename file)))
         (indexed
          (and truename
               (seq-filter
                (lambda (entry)
                  (let ((todo-file (my/aaronnote-roam--todo-field entry "file")))
                    (and (stringp todo-file)
                         (file-exists-p todo-file)
                         (string= (file-truename todo-file) truename))))
                (or (my/aaronnote-roam--todos) '())))))
    (or indexed
        (and file (my/aaronnote-roam--current-buffer-todos)))))

(defun my/aaronnote-roam-jump-file-todo ()
  "Quickly jump to a todo in the current Markdown roam file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((todos (my/aaronnote-roam--current-file-todos))
         (choices
          (mapcar
           (lambda (entry)
             (let* ((line (or (my/aaronnote-roam--todo-field entry "line") 0))
                    (status (upcase (my/aaronnote-roam--todo-status entry)))
                    (date (my/aaronnote-roam--todo-agenda-date entry))
                    (text (or (my/aaronnote-roam--todo-string-value
                               entry "text" "source" "context")
                              "(empty todo)"))
                    (label (format "%5s  L%-4s  %s%s"
                                   status line text
                                   (if date (format "  <%s>" date) ""))))
               (cons label entry)))
           todos)))
    (unless choices
      (user-error "No todos in current file"))
    (my/aaronnote-roam--visit-todo
     (cdr (assoc (completing-read "File todo: " choices nil t) choices)))))

(defun my/aaronnote-roam-agenda (&optional mode query)
  "Show a roam notes agenda: todos from md notes grouped by status/ddl."
  (interactive)
  (let* ((mode (or mode 'open))
         (todos (or (my/aaronnote-roam--todos) '()))
         (filtered (my/aaronnote-roam--agenda-filter-todos todos mode query))
         (shown-count (length filtered))
         (refresh (let ((view-mode mode)
                        (view-query query))
                    (lambda ()
                      (my/aaronnote-roam-agenda view-mode view-query))))
           (buf (my/aaronnote-roam--prepare-ui-buffer
                 "*roam-agenda*" "Roam Agenda" 'agenda
                 refresh
                 (format "%d shown" shown-count))))
      (with-current-buffer buf
        (my/aaronnote-roam-ui-render
         (lambda ()
           (my/aaronnote-roam-ui-insert-page-header
            "Agenda"
            :icon 'agenda
            :subtitle (my/aaronnote-roam--agenda-subtitle mode query shown-count)
            :stats (my/aaronnote-roam--agenda-stats todos)
            :actions (my/aaronnote-roam--agenda-actions mode query))
           (let ((inserted nil))
             (dolist (group (my/aaronnote-roam--agenda-groups filtered))
               (let* ((title (car group))
                      (entries (cadr group))
                      (tone (cddr group)))
                 (when entries
                   (setq inserted t)
                   (my/aaronnote-roam-ui-insert-section
                    title (length entries) tone)
                   (dolist (entry entries)
                     (my/aaronnote-roam--insert-agenda-todo-row entry tone))
                   (insert "\n"))))
             (unless inserted
               (my/aaronnote-roam-ui-insert-empty
                (pcase mode
                  ('search "No matching tasks.")
                  ('date "No open tasks on this date.")
                  ('done "No completed tasks.")
                  ('cancelled "No cancelled tasks.")
                  (_ "No open tasks.")))))))
      (display-buffer buf))))

(defun my/aaronnote-roam--agenda-calendar-counts (todos)
  "Return a date -> open todo entries hash table for TODOS."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry todos)
      (unless (my/aaronnote-roam--todo-closed-p entry)
        (when-let* ((day (my/aaronnote-roam--todo-agenda-date entry)))
          (puthash day (cons entry (gethash day counts)) counts))))
    counts))

(defun my/aaronnote-roam--agenda-calendar-cell-label (day count)
  "Return a fixed-width square calendar label for DAY and COUNT."
  (format "[%02d %2s]"
          day
          (cond
           ((<= count 0) "")
           ((> count 99) "99")
           (t (format "%d" count)))))

(defun my/aaronnote-roam--agenda-calendar-insert-month (year month counts)
  "Insert agenda calendar for YEAR MONTH using date COUNTS."
  (let* ((title (format "%04d-%02d" year month))
         (first-dow (calendar-day-of-week (list month 1 year)))
         (last-day (calendar-last-day-of-month month year))
         (today (format-time-string "%Y-%m-%d")))
    (my/aaronnote-roam-ui-insert-section title)
    (insert "   SUN      MON      TUE      WED      THU      FRI      SAT\n   ")
    (dotimes (_ first-dow)
      (insert "        "))
    (dotimes (index last-day)
      (let* ((day (1+ index))
             (date (format "%04d-%02d-%02d" year month day))
             (entries (gethash date counts))
             (count (length entries))
             (tone (cond
                    ((string= date today) 'warning)
                    ((and (> count 0) (string< date today)) 'danger)
                    ((> count 0) 'info)
                    (t 'muted)))
             (label (my/aaronnote-roam--agenda-calendar-cell-label
                     day count)))
        (insert-text-button
         label
         'action (let ((target-date date))
                   (lambda (_button)
                     (my/aaronnote-roam-agenda 'date target-date)))
         'follow-link t
         'help-echo (format "Show open tasks dated %s" date)
         'face (my/aaronnote-roam-ui--tone-face tone))
        (insert " ")
        (when (= (mod (+ first-dow day) 7) 0)
          (insert "\n   "))))
    (insert "\n\n")))

(defun my/aaronnote-roam-agenda-calendar ()
  "Show a compact deadline calendar for open roam agenda tasks."
  (interactive)
  (let* ((todos (or (my/aaronnote-roam--todos) '()))
         (counts (my/aaronnote-roam--agenda-calendar-counts todos))
         (decoded (decode-time (current-time)))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (next-month (if (= month 12) 1 (1+ month)))
         (next-year (if (= month 12) (1+ year) year))
         (open-count (seq-count
                      (lambda (entry)
                        (not (my/aaronnote-roam--todo-closed-p entry)))
                      todos))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-agenda-calendar*" "Roam Agenda Calendar" 'agenda
               #'my/aaronnote-roam-agenda-calendar
               (format "%d open" open-count))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Agenda Calendar"
          :icon 'agenda
          :subtitle "Open task dates for this month and next month"
          :stats (list (cons (format "%d open" open-count) 'info))
          :actions (my/aaronnote-roam--ui-actions
                    `((:label "Open Agenda"
                       :command ,(lambda ()
                                   (my/aaronnote-roam-agenda 'open nil))
                       :help "Return to the open agenda"
                       :primary t))))
         (my/aaronnote-roam--agenda-calendar-insert-month year month counts)
         (my/aaronnote-roam--agenda-calendar-insert-month
          next-year next-month counts))))
    (display-buffer buf)))

;; ── Roam activity heatmap ────────────────────────────────────────────────────

(defconst my/aaronnote-roam--activity-heatmap-days 70
  "Number of recent days shown in roam activity heatmaps.")

(defun my/aaronnote-roam--activity-date-counts (&optional days)
  "Return an ordered alist of recent note activity counts for DAYS."
  (let* ((days (or days my/aaronnote-roam--activity-heatmap-days))
         (start (time-subtract (current-time) (days-to-time (1- days))))
         (counts (make-hash-table :test 'equal))
         ordered)
    (dotimes (offset days)
      (let ((day (format-time-string
                  "%Y-%m-%d" (time-add start (days-to-time offset)))))
        (push (cons day 0) ordered)
        (puthash day 0 counts)))
    (dolist (record (delete-dups (my/aaronnote-roam--note-records)))
      (when-let* ((file (plist-get record :file))
                  ((file-exists-p file))
                  (attrs (file-attributes file 'integer))
                  (mtime (nth 5 attrs)))
        (when (not (time-less-p mtime start))
          (let ((day (format-time-string "%Y-%m-%d" mtime)))
            (when (gethash day counts)
              (puthash day (1+ (gethash day counts)) counts))))))
    (mapcar (lambda (pair)
              (cons (car pair) (gethash (car pair) counts 0)))
            (nreverse ordered))))

(defun my/aaronnote-roam--activity-heatmap-tone (count)
  "Return a display tone for activity COUNT."
  (cond
   ((>= count 5) 'success)
   ((>= count 2) 'warning)
   ((>= count 1) 'info)
   (t 'muted)))

(defun my/aaronnote-roam--activity-heatmap-cell (count)
  "Return a fixed-width heatmap cell label for COUNT."
  (format " %2s "
          (cond
           ((<= count 0) "")
           ((> count 99) "99")
           (t (format "%d" count)))))

(defun my/aaronnote-roam--activity-heatmap-rows (counts)
  "Return heatmap rows for activity COUNTS.
Each row is a list of strings or (LABEL . TONE) cells."
  (let* ((weeks (ceiling (/ (float (length counts)) 7.0)))
         (grid (make-vector (* weeks 7) nil))
         rows)
    (cl-loop for index from 0
             for pair in counts
             do (aset grid index pair))
    (push (list
           (concat "     "
                   (mapconcat (lambda (n) (format "W%-2d " n))
                              (number-sequence 1 weeks)
                              " ")))
          rows)
    (cl-loop for dow from 0 below 7
             for label in '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
             do
             (push
              (append
               (list (format "%3s  " label))
               (cl-loop
                for week from 0 below weeks
                for pair = (aref grid (+ (* week 7) dow))
                for count = (or (cdr-safe pair) 0)
                collect
                (cons (my/aaronnote-roam--activity-heatmap-cell count)
                      (my/aaronnote-roam--activity-heatmap-tone count))
                unless (= week (1- weeks))
                collect " "))
              rows))
    (nreverse rows)))

(defun my/aaronnote-roam--activity-heatmap-row-width (row)
  "Return display width for heatmap ROW."
  (apply #'+
         (mapcar
          (lambda (cell)
            (string-width (if (consp cell) (car cell) cell)))
          row)))

(defun my/aaronnote-roam--pixel-width-to-columns (pixels)
  "Return the display-column width represented by PIXELS."
  (ceiling (/ (float pixels) (max 1 (frame-char-width)))))

(defun my/aaronnote-roam--region-align-width (start end fallback-width)
  "Return rendered width from START to END for `:align-to'.
Use FALLBACK-WIDTH when pixel measurement is unavailable."
  (or (and (fboundp 'string-pixel-width)
           (let ((pixel-width
                  (string-pixel-width (buffer-substring start end)
                                      (current-buffer))))
             (and (> pixel-width 0)
                  (my/aaronnote-roam--pixel-width-to-columns pixel-width))))
      fallback-width))

(defun my/aaronnote-roam--center-inserted-region (start end width)
  "Center text from START to END using display WIDTH."
  (let* ((align-width (my/aaronnote-roam--region-align-width start end width))
         (prefix (propertize
                  " "
                  'display
                  `(space . (:align-to (- center ,(/ (float align-width) 2)))))))
    (add-text-properties start end
                         `(line-prefix ,prefix indent-prefix ,prefix))))

(defun my/aaronnote-roam--insert-centered-heatmap-row (row &optional face-fn)
  "Insert heatmap ROW centered.  FACE-FN maps a tone to a face."
  (let ((start (point))
        (row-width (my/aaronnote-roam--activity-heatmap-row-width row)))
    (dolist (cell row)
      (if (consp cell)
          (insert (propertize (car cell)
                              'face (if face-fn
                                        (funcall face-fn (cdr cell))
                                      'default)))
        (insert cell)))
    (my/aaronnote-roam--center-inserted-region start (point) row-width)
    (insert "\n")))

(defun my/aaronnote-roam--insert-centered-line (text &optional face)
  "Insert TEXT centered in the selected window with optional FACE."
  (let ((start (point)))
    (insert (if face (propertize text 'face face) text))
    (my/aaronnote-roam--center-inserted-region
     start (point) (string-width text))
    (insert "\n")))

(defun my/aaronnote-roam-ui-insert-activity-heatmap (&optional days)
  "Insert a board-style roam activity heatmap for recent DAYS."
  (let* ((counts (my/aaronnote-roam--activity-date-counts days))
         (total (apply #'+ (mapcar #'cdr counts))))
    (my/aaronnote-roam-ui-insert-section
     (format "Roam activity · last %d days" (length counts))
     total
     (if (> total 0) 'success 'muted))
    (dolist (row (my/aaronnote-roam--activity-heatmap-rows counts))
      (my/aaronnote-roam--insert-centered-heatmap-row
       row
       #'my/aaronnote-roam-ui--tone-face))
    (insert "\n")
    (my/aaronnote-roam--insert-centered-line
     "Each square is one day; value is modified note count."
     'my/aaronnote-roam-ui-meta)
    (insert "\n")
    (my/aaronnote-roam--insert-centered-heatmap-row
     '("Legend  " (" 0 " . muted) " " (" 1 " . info) " "
       (" 2+ " . warning) " " (" 5+ " . success))
     #'my/aaronnote-roam-ui--tone-face)
    (insert "\n")))

(defun my/aaronnote-roam-dashboard-insert-heatmap (&optional days)
  "Insert a compact roam activity heatmap into the main dashboard."
  (condition-case nil
      (let* ((counts (my/aaronnote-roam--activity-date-counts
                      (or days my/aaronnote-roam--activity-heatmap-days)))
             (total (apply #'+ (mapcar #'cdr counts))))
        (when counts
          (my/aaronnote-roam--insert-centered-line
           (format "Roam activity · last %d days · %d changes"
                   (length counts) total)
           (if (facep 'dashboard-heading)
               'dashboard-heading
             'bold))
          (insert "\n")
          (dolist (row (my/aaronnote-roam--activity-heatmap-rows counts))
            (my/aaronnote-roam--insert-centered-heatmap-row
             row
             #'my/aaronnote-roam-ui--tone-face))
          (insert "\n\n")))
    (error nil)))

;; ── Roam DB utilities ─────────────────────────────────────────────────────────

(defun my/aaronnote-roam-sync-full ()
  "Force a full roam-db rebuild (clears incremental state)."
  (interactive)
  (message "Rebuilding roam-db from scratch…")
  (when (my/aaronnote-roam--runtime-available-p)
    (my/aaronnote-roam--runtime-sync t nil))
  (message "Roam-db full rebuild done."))

(defun my/aaronnote-roam-db-status ()
  "Show roam-db sync state from Aaronnote var."
  (interactive)
  (let* ((root (my/aaronnote-roam-root))
         (state-file (expand-file-name "sync/state.json"
                                       (my/aaronnote-roam--state-root)))
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
         (my/aaronnote-roam-ui-insert-activity-heatmap)
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
              (mapcar (lambda (slug) (concat roam-prefix slug))
                      (my/aaronnote-roam--all-slugs-cached))))
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
  (my/aaronnote-roam-ui-set-header "Roam Selector" 'search "search")
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (markerp my/aaronnote-roam-select--origin-marker)
                (set-marker my/aaronnote-roam-select--origin-marker nil)))
            nil t))

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
    (define-key map (kbd "F") #'my/aaronnote-roam-jump-file-todo)
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
    ("d" "daily note"           my/aaronnote-roam-daily-note)
    ("m" "move note"            my/aaronnote-roam-move-note)]
   ["Tag ids"
    ("#" "insert tag id"        my/aaronnote-roam-insert-tag-id)
    ("g" "generate tag id"      my/aaronnote-roam-generate-tag-id)]
   ["Explore"
    ("s" "search/filter"        my/aaronnote-roam-search-notes)
    ("r" "recent"               my/aaronnote-roam-recent-notes)
    ("R" "related"              my/aaronnote-roam-related-notes)
    ("G" "graph"                my/aaronnote-roam-graph)
    ("C" "categories"           my/aaronnote-roam-categories)
    ("M" "management/dashboard" my/aaronnote-roam-management)]
   ["Special pages (wiki)"
    ("!" "reports hub"          my/aaronnote-roam-reports)
    ("!w" "wanted pages"        my/aaronnote-roam-report-wanted)
    ("!o" "orphaned"            my/aaronnote-roam-report-orphaned)
    ("!d" "dead-end"            my/aaronnote-roam-report-dead-end)
    ("!u" "uncategorized"       my/aaronnote-roam-report-uncategorized)
    ("!h" "most-linked (hubs)"  my/aaronnote-roam-report-most-linked)]
   ["DB & Agenda"
    ("b" "backlinks"            my/aaronnote-roam-backlinks)
    ("t" "tags"                 my/aaronnote-roam-tags)
    ("T" "todos"                my/aaronnote-roam-todos)
    ("F" "file todos"           my/aaronnote-roam-jump-file-todo)
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

;;; Wiki knowledge-health reports (MediaWiki Special: pages analog).

(defconst my/aaronnote-roam--report-limit 200
  "Maximum rows shown in a single wiki report.")

(defun my/aaronnote-roam--wiki-stats ()
  "Return a plist of vault-wide wiki statistics from the cached index."
  (let* ((entries (my/aaronnote-roam--all-note-summaries))
         (total (length entries))
         (orphaned (seq-count
                    (lambda (e)
                      (and (null (plist-get e :backlinks))
                           (not (string-prefix-p "daily/"
                                                 (or (plist-get e :slug) "")))))
                    entries))
         (dead-end (seq-count (lambda (e) (null (plist-get e :links))) entries))
         (uncategorized (seq-count (lambda (e) (null (plist-get e :tags))) entries))
         (all-links (seq-mapcat (lambda (e) (plist-get e :links)) entries))
         (link-count (length all-links))
         (wanted-count (hash-table-count
                        (let ((ht (make-hash-table :test 'equal))
                              (known (make-hash-table :test 'equal)))
                          (dolist (e entries)
                            (puthash (plist-get e :slug) t known))
                          (dolist (lnk all-links)
                            (unless (gethash lnk known)
                              (puthash lnk t ht)))
                          ht))))
    (list :total total :orphaned orphaned :dead-end dead-end
          :uncategorized uncategorized :link-count link-count
          :wanted wanted-count)))

(defun my/aaronnote-roam-report-orphaned ()
  "Show notes with no backlinks (MediaWiki Special:LonelyPages analog)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e)
                     (and (null (plist-get e :backlinks))
                          (not (string-prefix-p "daily/"
                                                (or (plist-get e :slug) "")))))
                   (my/aaronnote-roam--all-note-summaries)))
         (entries (seq-take entries my/aaronnote-roam--report-limit))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-orphaned*" "Orphaned Pages" 'orphan
               #'my/aaronnote-roam-report-orphaned
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Orphaned pages"
          :icon 'orphan
          :subtitle "Notes no other note links to"
          :stats (list (cons (format "%d notes" (length entries)) 'warning))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Orphaned" (length entries))
         (if (null entries)
             (my/aaronnote-roam-ui-insert-empty "No orphaned notes.")
           (dolist (entry entries)
             (my/aaronnote-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-report-dead-end ()
  "Show notes that link to no other note (MediaWiki Special:DeadendPages)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e) (null (plist-get e :links)))
                   (my/aaronnote-roam--all-note-summaries)))
         (entries (seq-take entries my/aaronnote-roam--report-limit))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-dead-end*" "Dead-end Pages" 'dead-end
               #'my/aaronnote-roam-report-dead-end
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Dead-end pages"
          :icon 'dead-end
          :subtitle "Notes with no outgoing links"
          :stats (list (cons (format "%d notes" (length entries)) 'warning))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Dead-end" (length entries))
         (if (null entries)
             (my/aaronnote-roam-ui-insert-empty "No dead-end notes.")
           (dolist (entry entries)
             (my/aaronnote-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-report-uncategorized ()
  "Show notes with no tags (MediaWiki Special:UncategorizedPages analog)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e) (null (plist-get e :tags)))
                   (my/aaronnote-roam--all-note-summaries)))
         (entries (seq-take entries my/aaronnote-roam--report-limit))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-uncategorized*" "Uncategorized Pages" 'uncategorized
               #'my/aaronnote-roam-report-uncategorized
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Uncategorized pages"
          :icon 'uncategorized
          :subtitle "Notes with no tags"
          :stats (list (cons (format "%d notes" (length entries)) 'muted))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Uncategorized" (length entries))
         (if (null entries)
             (my/aaronnote-roam-ui-insert-empty "All notes have tags.")
           (dolist (entry entries)
             (my/aaronnote-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-report-most-linked ()
  "Show the most-linked notes (MediaWiki Special:MostLinkedPages analog)."
  (interactive)
  (let* ((entries (my/aaronnote-roam--all-note-summaries))
         (sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (> (length (plist-get a :backlinks))
                            (length (plist-get b :backlinks))))))
         (top (seq-take sorted my/aaronnote-roam--report-limit))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-most-linked*" "Most-linked Pages" 'hub
               #'my/aaronnote-roam-report-most-linked
               (format "top %d" (length top)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Most-linked pages"
          :icon 'hub
          :subtitle "Hub notes sorted by incoming links"
          :stats (list (cons (format "%d notes" (length top)) 'info))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Hubs" (length top))
         (if (null top)
             (my/aaronnote-roam-ui-insert-empty "No linked notes.")
           (dolist (entry top)
             (let ((bls (length (plist-get entry :backlinks))))
               (my/aaronnote-roam-ui-insert-row
                :id (plist-get entry :slug)
                :icon 'note
                :title (or (plist-get entry :title) (plist-get entry :slug))
                :meta (format "%d backlinks" bls)
                :tags (plist-get entry :tags)
                :action (let ((slug (plist-get entry :slug)))
                          (lambda (_b) (my/aaronnote-roam--open-slug slug))))))))))
    (display-buffer buf)))

(defun my/aaronnote-roam--render-wanted-buffer (items)
  "Render wanted-pages ITEMS (a list of alists) into a roam report buffer."
  (let* ((items (seq-take items my/aaronnote-roam--report-limit))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-wanted*" "Wanted Pages" 'wanted
               #'my/aaronnote-roam-report-wanted
               (format "%d targets" (length items)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Wanted pages"
          :icon 'wanted
          :subtitle "Link targets that have no matching note"
          :stats (list (cons (format "%d targets" (length items)) 'danger))
          :actions (my/aaronnote-roam--ui-actions
                    (list (list :label "Create all wanted"
                                :command #'my/aaronnote-roam-report-wanted
                                :help "Refresh after creating notes"))))
         (my/aaronnote-roam-ui-insert-section "Wanted" (length items))
         (if (null items)
             (my/aaronnote-roam-ui-insert-empty "No wanted pages. All links resolve!")
           (dolist (item items)
             (let* ((target (alist-get 'target item))
                    (by (alist-get 'by item))
                    (by (cond ((vectorp by) (append by nil))
                              ((listp by) by)
                              (t nil)))
                    (by-count (length by)))
               (my/aaronnote-roam-ui-insert-row
                :id target
                :icon 'wanted
                :title (format "%s" target)
                :meta (format "linked from %d note%s" by-count
                              (if (= by-count 1) "" "s"))
                :action (let ((ref target))
                          (lambda (_b)
                            (my/aaronnote-roam-new-note ref nil nil))))))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-report-wanted ()
  "Show link targets that have no corresponding note (MediaWiki Special:WantedPages)."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running; start it with H-o o first"))
  (message "Aaronnote: fetching wanted pages...")
  (my/aaronnote--api-call
   "aaronnote:api:notes:wanted" []
   (lambda (result)
     (let ((items (alist-get 'items result)))
       (my/aaronnote-roam--render-wanted-buffer
        (if (vectorp items) (append items nil)
          (or items nil)))))))

(defun my/aaronnote-roam--asset-items (items)
  "Normalize JSON asset ITEMS into a list."
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(defun my/aaronnote-roam--asset-field (asset field)
  "Return ASSET FIELD from an alist or hash table."
  (cond
   ((hash-table-p asset) (gethash (symbol-name field) asset))
   ((listp asset) (alist-get field asset))
   (t nil)))

(defun my/aaronnote-roam--asset-file (asset)
  "Return ASSET absolute file path."
  (format "%s" (or (my/aaronnote-roam--asset-field asset 'file) "")))

(defun my/aaronnote-roam--format-asset-size (asset)
  "Return a human-readable size label for ASSET."
  (let ((size (my/aaronnote-roam--asset-field asset 'size)))
    (if (numberp size)
        (file-size-human-readable size)
      "unknown size")))

(defun my/aaronnote-roam--format-asset-mtime (asset)
  "Return a human-readable modified-time label for ASSET."
  (let ((mtime-ms (my/aaronnote-roam--asset-field asset 'mtimeMs)))
    (if (numberp mtime-ms)
        (format-time-string "%Y-%m-%d %H:%M"
                            (seconds-to-time (/ mtime-ms 1000.0)))
      "unknown mtime")))

(defun my/aaronnote-roam--open-asset (asset)
  "Open ASSET's file in Emacs."
  (let ((file (my/aaronnote-roam--asset-file asset)))
    (if (and (not (string-empty-p file)) (file-exists-p file))
        (find-file file)
      (user-error "Asset file does not exist: %s" file))))

(defun my/aaronnote-roam--trash-orphaned-assets (assets)
  "Move orphaned ASSETS to Trash through the Aaronnote runtime."
  (let* ((assets (my/aaronnote-roam--asset-items assets))
         (files (delq nil
                      (mapcar (lambda (asset)
                                (let ((file (my/aaronnote-roam--asset-file asset)))
                                  (unless (string-empty-p file) file)))
                              assets))))
    (unless files
      (user-error "No orphaned attachments to trash"))
    (when (yes-or-no-p (format "Move %d orphaned attachment%s to Trash? "
                               (length files)
                               (if (= (length files) 1) "" "s")))
      (message "Aaronnote: moving orphaned attachments to Trash...")
      (my/aaronnote--api-call
       "aaronnote:api:assets:trash-orphans" (vector files)
       (lambda (result)
         (let ((next-assets (my/aaronnote-roam--asset-items
                             (alist-get 'assets result)))
               (trashed (my/aaronnote-roam--asset-items
                         (alist-get 'trashed result)))
               (skipped (my/aaronnote-roam--asset-items
                         (alist-get 'skipped result))))
           (message "Aaronnote: trashed %d orphaned attachment%s%s"
                    (length trashed)
                    (if (= (length trashed) 1) "" "s")
                    (if skipped
                        (format ", skipped %d" (length skipped))
                      ""))
           (my/aaronnote-roam--render-orphaned-assets-buffer next-assets)))))))

(defun my/aaronnote-roam--render-orphaned-assets-buffer (assets)
  "Render orphaned attachment ASSETS into a roam report buffer."
  (let* ((items (seq-take (my/aaronnote-roam--asset-items assets)
                          my/aaronnote-roam--report-limit))
         (count (length items))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*roam-orphaned-assets*" "Orphaned Attachments" 'attachment
               #'my/aaronnote-roam-report-orphaned-assets
               (format "%d assets" count))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Orphaned attachments"
          :icon 'attachment
          :subtitle "Files in asset folders that no note currently references"
          :stats (list (cons (format "%d assets" count)
                             (if (> count 0) 'warning 'success)))
          :actions (my/aaronnote-roam--ui-actions
                    (when items
                      `((:label "Trash listed"
                         :command ,(let ((orphans items))
                                     (lambda ()
                                       (my/aaronnote-roam--trash-orphaned-assets
                                        orphans)))
                         :help "Move the listed orphaned attachments to Trash"
                         :primary t)))))
         (my/aaronnote-roam-ui-insert-section "Attachments" count)
         (if (null items)
             (my/aaronnote-roam-ui-insert-empty
              "No orphaned attachments. All scanned assets are referenced.")
           (dolist (asset items)
             (let* ((path (format "%s" (or (my/aaronnote-roam--asset-field asset 'path)
                                           (my/aaronnote-roam--asset-file asset))))
                    (type (format "%s" (or (my/aaronnote-roam--asset-field asset 'type)
                                           "asset"))))
               (my/aaronnote-roam-ui-insert-row
                :id (my/aaronnote-roam--asset-file asset)
                :icon (if (my/aaronnote-roam--asset-field asset 'isImage)
                          'image
                        'attachment)
                :badge type
                :badge-tone 'muted
                :title path
                :meta (my/aaronnote-roam--format-asset-size asset)
                :detail (my/aaronnote-roam--format-asset-mtime asset)
                :action (let ((item asset))
                          (lambda (_b)
                            (my/aaronnote-roam--open-asset item))))))))))
    (display-buffer buf)))

(defun my/aaronnote-roam-report-orphaned-assets ()
  "Show unreferenced Aaronnote attachments and generated media assets."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running; start it with H-o o first"))
  (message "Aaronnote: scanning orphaned attachments...")
  (my/aaronnote--api-call
   "aaronnote:api:assets:scan-orphans" []
   (lambda (result)
     (my/aaronnote-roam--render-orphaned-assets-buffer
      (alist-get 'assets result)))))

(with-eval-after-load 'transient
  (transient-define-prefix my/aaronnote-roam-reports ()
    "Wiki knowledge-health reports."
    [["Special pages"
      ("w" "wanted pages"       my/aaronnote-roam-report-wanted)
      ("o" "orphaned pages"     my/aaronnote-roam-report-orphaned)
      ("a" "orphaned attachments" my/aaronnote-roam-report-orphaned-assets)
      ("d" "dead-end pages"     my/aaronnote-roam-report-dead-end)
      ("u" "uncategorized"      my/aaronnote-roam-report-uncategorized)
      ("h" "most-linked (hubs)" my/aaronnote-roam-report-most-linked)]]))

;;; Wiki category browser (MediaWiki Category: system analog).

(defun my/aaronnote-roam--category-tree (entries)
  "Build a category hierarchy from tag lists in ENTRIES.
Returns a sorted list of (CATEGORY-PATH . MEMBER-SLUGS) conses where
CATEGORY-PATH is a slash-joined string of segments."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (dolist (tag (or (plist-get entry :tags) nil))
        (let ((slug (plist-get entry :slug))
              (segments (split-string (downcase tag) "/" t)))
          (let ((path ""))
            (dolist (seg segments)
              (setq path (if (string-empty-p path) seg (concat path "/" seg)))
              (puthash path
                       (cons slug (gethash path ht))
                       ht))))))
    (let (result)
      (maphash (lambda (path slugs)
                 (push (cons path (delete-dups (nreverse slugs))) result))
               ht)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun my/aaronnote-roam-categories ()
  "Browse notes hierarchically by category (nested tags, MediaWiki Category: analog).
Select a top-level category to drill down; select a note to open it."
  (interactive)
  (let* ((entries (my/aaronnote-roam--all-note-summaries))
         (tree (my/aaronnote-roam--category-tree entries))
         ;; Navigation state: current prefix path being browsed
         (prefix ""))
    (cl-labels
        ((children-of (pfx)
           (seq-filter
            (lambda (pair)
              (let ((p (car pair)))
                (if (string-empty-p pfx)
                    (not (string-match-p "/" p))
                  (and (string-prefix-p (concat pfx "/") p)
                       (not (string-match-p "/"
                             (substring p (1+ (length pfx)))))))))
            tree))
         (members-of (pfx)
           (let ((pair (assoc pfx tree)))
             (when pair
               (seq-filter
                (lambda (e)
                  (seq-some
                   (lambda (tag)
                     (let ((tl (downcase tag)))
                       (or (string= tl pfx)
                           (string-prefix-p (concat pfx "/") tl))))
                   (or (plist-get e :tags) nil)))
                entries))))
         (browse (pfx)
           (let* ((children (children-of pfx))
                  (members (members-of pfx))
                  (child-labels (mapcar (lambda (p)
                                          (cons (format "[+] %s" (car p)) (car p)))
                                        children))
                  (member-labels (mapcar (lambda (e)
                                           (cons (format "    %s" (or (plist-get e :title) (plist-get e :slug)))
                                                 (plist-get e :slug)))
                                         members))
                  (all-choices (append
                                (when pfx (list (cons ".. (up)" :up)))
                                child-labels member-labels))
                  (prompt (if (string-empty-p pfx)
                              "Category: "
                            (format "Category [%s]: " pfx)))
                  (choice (when all-choices
                            (completing-read prompt
                                             (mapcar #'car all-choices)
                                             nil t))))
             (when (and choice (not (string-empty-p choice)))
               (let* ((pair (assoc choice all-choices))
                      (val (cdr pair)))
                 (cond
                  ((eq val :up)
                   (let ((parent (file-name-directory (directory-file-name pfx))))
                     (browse (if parent
                                 (string-trim-right parent "/")
                               ""))))
                  ((member val (mapcar #'cdr child-labels))
                   (browse val))
                  ((stringp val)
                   (my/aaronnote-roam--open-slug val))))))))
      (browse prefix))))

;;; Wiki move-page with automatic link rewrite (MediaWiki "Move page").

(defun my/aaronnote-roam-move-note ()
  "Rename/move a roam note and rewrite all referencing links.
Prompts for the note to move and a new file name, then calls the
backend's fs:rename + roam-tools:rewrite-path-refs pipeline."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running; start it with H-o o first"))
  (let* ((slug (my/aaronnote-roam--read-note-id "Move note: "))
         (file (my/aaronnote-roam--slug-to-file slug))
         (old-name (file-name-nondirectory file))
         (new-name (read-string (format "New file name for '%s': " old-name)
                                old-name))
         (old-rel (file-relative-name file (my/aaronnote-roam-root)))
         (new-rel (concat (file-name-directory old-rel) new-name)))
    (unless (file-exists-p file)
      (user-error "File not found: %s" file))
    (when (string= old-name new-name)
      (user-error "New name is the same as old name"))
    (message "Moving '%s' → '%s' and rewriting links..." old-rel new-rel)
    (my/aaronnote--api-call
     "aaronnote:api:fs:rename"
     (vector (list (cons "file" file) (cons "targetName" new-name)))
     (lambda (result)
       (if (not (alist-get 'ok result))
           (message "Aaronnote move failed: %s" (alist-get 'message result))
         (my/aaronnote--api-call
          "aaronnote:api:roam-tools:rewrite-path-refs"
          (vector (list (cons "oldPath" old-rel) (cons "newPath" new-rel)))
          (lambda (rewrite-result)
            (my/aaronnote-roam--clear-runtime-cache)
            (let ((changed (length (or (alist-get 'changed rewrite-result) []))))
              (message "Moved '%s' → '%s'; rewrote links in %d file%s."
                       old-rel new-rel changed
                       (if (= changed 1) "" "s"))))))))))

;;; Tag management wrappers for the management dashboard.

(defun my/aaronnote-roam-rename-tag ()
  "Rename a tag across all vault notes via the Aaronnote runtime."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running"))
  (let* ((old-tag (read-string "Old tag name: "))
         (new-tag (read-string (format "Rename '%s' to: " old-tag) old-tag)))
    (when (string-empty-p old-tag)
      (user-error "Tag name cannot be empty"))
    (when (string= old-tag new-tag)
      (user-error "New tag is the same as old tag"))
    (message "Renaming tag '%s' → '%s'..." old-tag new-tag)
    (my/aaronnote--api-call
     "aaronnote:api:roam-tools:rename-tag"
     (vector (list (cons "oldTag" old-tag) (cons "newTag" new-tag)))
     (lambda (result)
       (my/aaronnote-roam--clear-runtime-cache)
       (let ((changed (or (alist-get 'changed result) 0)))
         (message "Renamed tag '%s' → '%s' in %d file%s."
                  old-tag new-tag changed (if (= changed 1) "" "s")))))))

(defun my/aaronnote-roam-delete-tag ()
  "Delete a tag from all vault notes via the Aaronnote runtime."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running"))
  (let ((tag (read-string "Delete tag: ")))
    (when (string-empty-p tag)
      (user-error "Tag name cannot be empty"))
    (when (yes-or-no-p (format "Delete tag '%s' from all notes? " tag))
      (message "Deleting tag '%s'..." tag)
      (my/aaronnote--api-call
       "aaronnote:api:roam-tools:delete-tag"
       (vector (list (cons "tag" tag)))
       (lambda (result)
         (my/aaronnote-roam--clear-runtime-cache)
         (let ((changed (or (alist-get 'changed result) 0)))
           (message "Deleted tag '%s' from %d file%s."
                    tag changed (if (= changed 1) "" "s"))))))))

(defun my/aaronnote-roam-tag-overlap ()
  "Show overlapping/redundant tags report via the Aaronnote runtime."
  (interactive)
  (unless (and (boundp 'my/aaronnote--ready) my/aaronnote--ready)
    (user-error "Aaronnote web-host is not running"))
  (message "Analyzing tag overlap...")
  (my/aaronnote--api-call
   "aaronnote:api:roam-tools:tag-overlap"
   []
   (lambda (result)
     (let* ((pairs (alist-get 'pairs result))
            (pairs (if (vectorp pairs) (append pairs nil) (or pairs nil)))
            (buf (get-buffer-create "*roam-tag-overlap*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert "Tag Overlap Report\n\n")
           (if (null pairs)
               (insert "No overlapping tags found.\n")
             (dolist (pair pairs)
               (insert (format "  %s  ↔  %s  (%.0f%% overlap)\n"
                               (alist-get 'a pair "?")
                               (alist-get 'b pair "?")
                               (* 100 (or (alist-get 'overlap pair) 0))))))
           (goto-char (point-min))
           (special-mode)))
       (display-buffer buf)))))

;;; Upgrade management dashboard with wiki statistics.

(defconst my/aaronnote-roam--dashboard-tools
  '((:id find-note
     :icon search
     :badge "FIND"
     :badge-tone info
     :title "Find note"
     :meta "findnode"
     :detail "Jump by Aaronnote id, path, or title."
     :command my/aaronnote-roam-find-note
     :help "Find and open a roam note")
    (:id create-note
     :icon new
     :badge "CREATE"
     :badge-tone success
     :title "Create note"
     :meta "Roam New"
     :detail "Open the native note creation workbench."
     :command my/aaronnote-roam-new-note
     :help "Create a new roam note")
    (:id create-node
     :icon new
     :badge "NODE"
     :badge-tone success
     :title "Create node"
     :meta "timestamped"
     :detail "Start a timestamped node in the selected directory."
     :command my/aaronnote-roam-new-node
     :help "Create a new timestamped node")
    (:id search-notes
     :icon search
     :badge "QUERY"
     :title "Search notes"
     :meta "title/tag/linksto"
     :detail "Filter notes with scoped search operators."
     :command my/aaronnote-roam-search-notes
     :help "Search Aaronnote roam notes")
    (:id daily-note
     :icon note
     :badge "TODAY"
     :badge-tone warning
     :title "Daily note"
     :meta "daily/YYYY-MM-DD"
     :detail "Open or create today's daily note."
     :command my/aaronnote-roam-daily-note
     :help "Open today's daily note")
    (:id agenda
     :icon agenda
     :badge "TASKS"
     :title "Agenda"
     :meta "open tasks"
     :detail "Review open todos grouped by due date and status."
     :command my/aaronnote-roam-agenda
     :help "Open the roam agenda")
    (:id todos
     :icon todo
     :badge "TODO"
     :title "Task list"
     :meta "all todos"
     :detail "List every indexed task across the vault."
     :command my/aaronnote-roam-todos
     :help "List all roam tasks")
    (:id categories
     :icon tag
     :badge "TAGS"
     :title "Categories"
     :meta "nested tags"
     :detail "Browse wiki-style categories from nested tags."
     :command my/aaronnote-roam-categories
     :help "Browse nested tag categories")
    (:id tags
     :icon tag
     :badge "TAG"
     :title "Flat tags"
     :meta "tag picker"
     :detail "Pick a tag, then open one of its notes."
     :command my/aaronnote-roam-tags
     :help "Browse notes by tag")
    (:id recent-notes
     :icon clock
     :badge "RECENT"
     :badge-tone muted
     :title "Recent notes"
     :meta "history"
     :detail "Return to recently opened roam notes."
     :command my/aaronnote-roam-recent-notes
     :help "Show recently opened roam notes"))
  "Command rows shown in the Roam management dashboard quick tools section.")

(defun my/aaronnote-roam--dashboard-tool-action (command)
  "Return a board row action that invokes COMMAND interactively."
  (lambda (_button)
    (if (commandp command)
        (call-interactively command)
      (funcall command))))

(defun my/aaronnote-roam--dashboard-insert-tools ()
  "Insert common command shortcuts into the Roam management dashboard."
  (my/aaronnote-roam-ui-insert-section
   "Quick tools" (length my/aaronnote-roam--dashboard-tools) 'info)
  (dolist (tool my/aaronnote-roam--dashboard-tools)
    (my/aaronnote-roam-ui-insert-row
     :id (list 'dashboard-tool (plist-get tool :id))
     :icon (plist-get tool :icon)
     :badge (plist-get tool :badge)
     :badge-tone (plist-get tool :badge-tone)
     :title (plist-get tool :title)
     :meta (plist-get tool :meta)
     :detail (plist-get tool :detail)
     :action (my/aaronnote-roam--dashboard-tool-action
              (plist-get tool :command))
     :help (plist-get tool :help)))
  (insert "\n"))

(defun my/aaronnote-roam-management ()
  "Show wiki maintenance dashboard: vault stats and all roam operations."
  (interactive)
  (let* ((entries (my/aaronnote-roam--all-note-summaries))
         (stats (my/aaronnote-roam--wiki-stats))
         (db (my/aaronnote-roam--db))
         (generated (and db (gethash "generated" db)))
         (buf (my/aaronnote-roam--prepare-ui-buffer
               "*aaronnote-roam-management*" "Roam Management" 'management
               #'my/aaronnote-roam-management
               (format "%d notes" (plist-get stats :total)))))
    (with-current-buffer buf
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-page-header
          "Wiki maintenance dashboard"
          :icon 'management
          :subtitle "Special:Statistics — vault health and operations"
          :stats (list (cons (format "%d notes" (plist-get stats :total)) 'info)
                       (cons (if generated "DB ready" "DB unknown")
                             (if generated 'success 'warning)))
          :actions (my/aaronnote-roam--ui-actions))
         (my/aaronnote-roam-ui-insert-section "Vault statistics")
         (my/aaronnote-roam-ui-insert-field
          "Root" (abbreviate-file-name (my/aaronnote-roam-root))
          'my/aaronnote-roam-ui-path)
         (my/aaronnote-roam-ui-insert-field "Total notes"    (plist-get stats :total))
         (my/aaronnote-roam-ui-insert-field "Total links"    (plist-get stats :link-count))
         (my/aaronnote-roam-ui-insert-field "Wanted pages"
                                            (let ((n (plist-get stats :wanted)))
                                              (if (> n 0) (format "%d ⚠" n) "0"))
                                            'my/aaronnote-roam-ui-meta)
         (my/aaronnote-roam-ui-insert-field "Orphaned"
                                            (let ((n (plist-get stats :orphaned)))
                                              (if (> n 0) (format "%d ⚠" n) "0"))
                                            'my/aaronnote-roam-ui-meta)
         (my/aaronnote-roam-ui-insert-field "Dead-end"
                                            (let ((n (plist-get stats :dead-end)))
                                              (if (> n 0) (format "%d" n) "0"))
                                            'my/aaronnote-roam-ui-meta)
         (my/aaronnote-roam-ui-insert-field "Uncategorized"
                                            (let ((n (plist-get stats :uncategorized)))
                                              (if (> n 0) (format "%d" n) "0"))
                                            'my/aaronnote-roam-ui-meta)
         (my/aaronnote-roam-ui-insert-field
         "DB generated" (or generated "unknown")
         'my/aaronnote-roam-ui-meta)
         (insert "\n")
         (my/aaronnote-roam--dashboard-insert-tools)
         (my/aaronnote-roam-ui-insert-activity-heatmap)
         (my/aaronnote-roam-ui-insert-section "Special pages")
         (insert "   ")
         (my/aaronnote-roam-ui-insert-actions
          '((:label "Wanted pages"
             :command my/aaronnote-roam-report-wanted
             :help "Links to notes that don't exist"
             :primary t)
            (:label "Orphaned"
             :command my/aaronnote-roam-report-orphaned
             :help "Notes with no incoming links")
            (:label "Orphaned attachments"
             :command my/aaronnote-roam-report-orphaned-assets
             :help "Files in asset folders that no note references")
            (:label "Dead-end"
             :command my/aaronnote-roam-report-dead-end
             :help "Notes with no outgoing links")
            (:label "Uncategorized"
             :command my/aaronnote-roam-report-uncategorized
             :help "Notes with no tags")
            (:label "Most-linked"
             :command my/aaronnote-roam-report-most-linked
             :help "Hub notes by backlink count")))
         (insert "\n")
         (my/aaronnote-roam-ui-insert-section "Tag tools")
         (insert "   ")
         (my/aaronnote-roam-ui-insert-actions
          '((:label "Browse categories"
             :command my/aaronnote-roam-categories
             :help "Drill down through nested tags"
             :primary t)
            (:label "Browse flat tags"
             :command my/aaronnote-roam-tags
             :help "All tags in the vault")
            (:label "Rename tag"
             :command my/aaronnote-roam-rename-tag
             :help "Rename a tag across all notes")
            (:label "Tag overlap"
             :command my/aaronnote-roam-tag-overlap
             :help "Find redundant/overlapping tags")))
         (insert "\n")
         (my/aaronnote-roam-ui-insert-section "DB & files")
         (insert "   ")
         (my/aaronnote-roam-ui-insert-actions
          '((:label "Sync roam-db"
             :command my/aaronnote-roam-update-db
             :help "Run incremental roam-db sync"
             :primary t)
            (:label "Move note"
             :command my/aaronnote-roam-move-note
             :help "Rename note + rewrite backlinks")
            (:label "Recent changes"
             :command my/aaronnote-roam-recent-notes
             :help "Recently opened notes")
            (:label "DB status"
             :command my/aaronnote-roam-db-status
             :help "DB sync state file"))))))
    (display-buffer buf)))

;;; Public lifecycle API (called from init-aaronnote.el).

(defun my/aaronnote-roam--cancel-sync-timer ()
  "Cancel pending debounced roam sync, clear the changed-files list, and
kill any in-flight CLI offline sync process."
  (when (timerp my/aaronnote-roam--sync-timer)
    (cancel-timer my/aaronnote-roam--sync-timer))
  (setq my/aaronnote-roam--sync-timer nil
        my/aaronnote-roam--sync-changed-files nil)
  (when (and my/aaronnote-roam--sync-process
             (process-live-p my/aaronnote-roam--sync-process))
    (delete-process my/aaronnote-roam--sync-process))
  (setq my/aaronnote-roam--sync-process nil))

(defun my/aaronnote-roam--note-in-vault-p (file)
  "Return non-nil when FILE is a Markdown note inside the vault root."
  (let ((root (file-name-as-directory (expand-file-name (my/aaronnote-roam-root)))))
    (and (string-prefix-p root file)
         (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file))))

(defun my/aaronnote-roam-note-changed (file)
  "Invalidate Emacs-side caches when the web-host reports FILE was saved.
The web-host is the authoritative roam.db writer and handles its own sync
via queueRoamDbSync; Emacs must not trigger a redundant sync from this event."
  (when (and (stringp file)
             (not (string-empty-p file))
             (my/aaronnote-roam--note-in-vault-p (expand-file-name file)))
    (my/aaronnote-roam--clear-runtime-cache)))

(provide 'init-md-roam)
;;; init-md-roam.el ends here
