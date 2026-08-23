;;; init-md-roam.el --- Markdown roam note navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Roam-style helpers for Noema Markdown notes.

;;; Code:

(require 'config)

(require 'init-funcs)
(require 'init-md-roam-ui)
(require 'calendar)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'url-util)
(require 'wid-edit)
(require 'xref)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function my/noema-open-file "init-aaronnote" (file))
(declare-function my/noema--ensure-server "init-aaronnote" (&optional callback))
(declare-function my/noema--server-url "init-aaronnote" (&optional path))
(declare-function my/noema--open-url "init-aaronnote" (url &optional file force-new))
(declare-function my/noema--api-call "init-aaronnote" (channel args callback))
(declare-function my/noema--api-call-sync "init-aaronnote" (channel args &optional timeout))
(declare-function my/noema-workspace-layout "init-aaronnote" ())
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")
(defvar my/noema--notes-root nil)

(defvar my/noema--ready nil
  "Non-nil when the Noema web host is available.")

(defgroup my/noema-roam nil
  "Roam-style navigation for Noema Markdown notes."
  :group 'my/noema)

(defconst my/noema-roam--module-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the Markdown roam Emacs bridge.")

(config-defvar my/noema-roam-root nil
  "Root directory of the Markdown roam note vault."
  :type 'directory
  :group 'my/noema-roam)

(config-defvar my/noema-roam-recent-limit nil
  "Maximum number of recent Markdown roam notes kept in memory."
  :type 'integer
  :group 'my/noema-roam)

(config-defvar my/noema-roam-select-window-height nil
  "Height for the bottom Markdown roam selector window."
  :type '(choice (number :tag "Fraction or rows") (function :tag "Window height function"))
  :group 'my/noema-roam)

(config-defvar my/noema-roam-runtime-root nil
  "Root of the vendored Noema runtime used by Markdown roam tooling."
  :type 'directory
  :group 'my/noema-roam)

(config-defvar my/noema-roam-runtime-cli nil
  "Node bridge used to call the vendored Noema roam runtime from Emacs."
  :type 'file
  :group 'my/noema-roam)

(defun my/noema-roam--state-root ()
  "Return the Noema var/state directory shared with the web host."
  (expand-file-name
   (if (boundp 'my/noema--state-root)
       my/noema--state-root
     "var/aaronnote")
   user-emacs-directory))

(defun my/noema-roam--tmp-root ()
  "Return the Noema runtime tmp directory shared with the web host."
  (expand-file-name
   (if (boundp 'my/noema--tmp-root)
       my/noema--tmp-root
     "tmp")
   (my/noema-roam--state-root)))

(defvar my/noema-roam--recent nil
  "Recently opened Markdown roam note ids, newest first.")

(defvar my/noema-roam--runtime-index-cache nil)
(defvar my/noema-roam--runtime-index-cache-key nil)
(defvar my/noema-roam--all-files-cache nil
  "Cached result of `my/noema-roam--all-files'.")
(defvar my/noema-roam--all-note-summaries-cache nil
  "Cached result of `my/noema-roam--all-note-summaries'.")

(defun my/noema-roam-root ()
  "Return the canonical Noema workspace root."
  (or (when (and (boundp 'my/noema--notes-root)
                 (stringp my/noema--notes-root)
                 (not (string-empty-p my/noema--notes-root)))
        (file-name-as-directory (expand-file-name my/noema--notes-root)))
      (when (fboundp 'my/noema-workspace-root)
        (my/noema-workspace-root))
      (file-name-as-directory (expand-file-name "~/Documents/Noema"))))

(defun my/noema-roam--clear-runtime-cache ()
  "Clear cached Noema runtime payloads."
  (setq my/noema-roam--runtime-index-cache nil
        my/noema-roam--runtime-index-cache-key nil
        my/noema-roam--scan-cache nil
        my/noema-roam--all-files-cache nil
        my/noema-roam--all-note-summaries-cache nil))

(defun my/noema-roam--workspace-layout ()
  "Return the Noema workspace layout name, defaulting to \"legacy\"."
  (if (fboundp 'my/noema-workspace-layout)
      (my/noema-workspace-layout)
    (if (equal (downcase (or (getenv "NOEMA_WORKSPACE_LAYOUT") "")) "wiki")
        "wiki"
      "legacy")))

(defun my/noema-roam--runtime-available-p ()
  "Return non-nil when the Noema runtime bridge is available."
  (and (file-exists-p my/noema-roam-runtime-cli)
       (file-exists-p
        (expand-file-name "server/lib/index.mjs"
                          my/noema-roam-runtime-root))))

(defun my/noema-roam--action-to-channel (action)
  "Map roam-cli ACTION keyword to web-host /api channel string, or nil."
  (cdr (assoc action
              '(("index"     . "aaronnote:api:notes:roam-index")
                ("tags"      . "aaronnote:api:completions:tags")
                ("todos"     . "aaronnote:api:notes:todos")
                ("templates" . "aaronnote:api:notes:templates")
                ("update-todo" . "aaronnote:api:notes:update-todo")
                ("agenda"    . "aaronnote:api:notes:agenda")
                ("patch-todo" . "aaronnote:api:notes:patch-todo")
                ("todo-dep-ref" . "aaronnote:api:notes:todo-dep-ref")
                ("todo-refs"  . "aaronnote:api:completions:todo-refs")
                ("create"    . "aaronnote:api:notes:create-node")
                ("delete-node" . "aaronnote:api:notes:delete-node")))))

(defun my/noema-roam--runtime-call-via-api (action args &optional timeout)
  "Delegate ACTION with roam-cli ARGS to the running web-host /api.
Maps the action to its /api channel, converts positional ARGS to
the expected body, and returns parsed JSON or nil.
TIMEOUT bounds the blocking wait; see `my/noema--api-call-sync'."
  (let ((channel (my/noema-roam--action-to-channel action)))
    (when channel
      (let ((api-args
             (pcase action
               ((or "create" "agenda" "patch-todo" "todo-dep-ref" "todo-refs")
                (let ((json-str (or (cadr (member "--json" args)) "{}")))
                  (condition-case nil
                      (vector (json-parse-string json-str :object-type 'hash-table))
                    (error (vector (make-hash-table :test 'equal))))))
               ("delete-node"
                (vector (or (cadr (member "--file" args))
                            (cadr (member "--path" args))
                            "")))
               ("sync"
                (vector (if (member "--full" args) t :false)))
               ("todos"
                (let ((body (make-hash-table :test 'equal))
                      (file (cadr (member "--file" args))))
                  (puthash "file" (or file "") body)
                  (vector body)))
               ("update-todo"
                (let ((body (make-hash-table :test 'equal)))
                  (dolist (key '("--file" "--status" "--source" "--id"
                                 "--text" "--index" "--priority" "--due"
                                 "--scheduled" "--repeat"))
                    (when-let* ((value (cadr (member key args))))
                      (puthash (string-remove-prefix "--" key) value body)))
                  (vector body)))
               ("tags"
                (vector (make-hash-table)))
               (_
                []))))
        (when api-args
          (my/noema--api-call-sync channel api-args timeout))))))

(defconst my/noema-roam--interactive-timeout 0.3
  "Seconds an on-keystroke backend query may block the editor.")

(defun my/noema-roam--runtime-call-interactive (action &rest args)
  "Like `my/noema-roam--runtime-call', but safe to run on a keystroke.

Blocks for at most `my/noema-roam--interactive-timeout' seconds and never
falls back to spawning roam-cli.mjs.  The general call is built for commands:
it waits up to eight seconds and then, if the web-host did not answer, starts
a Node process.  Running that from a `completion-at-point' function meant
every keypress could freeze Emacs for eight seconds plus a cold start.
Returns nil when the host cannot answer in time, and the caller then simply
offers no candidates."
  (when (and (boundp 'my/noema--ready) my/noema--ready)
    (my/noema-roam--runtime-call-via-api
     action args my/noema-roam--interactive-timeout)))

(defun my/noema-roam--runtime-call (action &rest args)
  "Call Noema roam runtime ACTION synchronously with ARGS.
When the web-host is running, delegates to its /api so all callers share the
same in-memory index.  Falls back to spawning roam-cli.mjs when the web-host
is down (offline / not yet started)."
  (or
   ;; Prefer the running web-host's in-memory index.
   (and (boundp 'my/noema--ready)
        my/noema--ready
        (my/noema-roam--runtime-call-via-api action args))
   ;; Fallback: spawn roam-cli.mjs directly.
   (when (my/noema-roam--runtime-available-p)
    (with-temp-buffer
      (let* ((root (my/noema-roam-root))
             (default-directory my/noema-roam--module-directory)
             (process-environment
              (append (list (format "AARONNOTE_ROOT=%s" root)
                            (format "AARONNOTE_RUNTIME_ROOT=%s"
                                    (expand-file-name
                                     my/noema-roam-runtime-root))
                            (format "AARONNOTE_WORKSPACE_ROOT=%s"
                                    user-emacs-directory)
                            (format "AARONNOTE_STATE_DIR=%s"
                                    (my/noema-roam--state-root))
                            (format "AARONNOTE_TMP_DIR=%s"
                                    (my/noema-roam--tmp-root))
                            ;; Without this the CLI configures the runtime as
                            ;; "legacy" and drops every note under `public/',
                            ;; so the offline fallback would disagree with the
                            ;; running web-host about what the vault contains.
                            (format "NOEMA_WORKSPACE_LAYOUT=%s"
                                    (my/noema-roam--workspace-layout)))
                      process-environment))
             (stderr-file (make-temp-file "aaronnote-runtime-"))
             (status (apply #'process-file
                            "node" nil (list (current-buffer) stderr-file) nil
                            my/noema-roam-runtime-cli
                            action
                            "--root" root
                            "--runtime" my/noema-roam-runtime-root
                            "--workspace" user-emacs-directory
                            "--state" (my/noema-roam--state-root)
                            "--tmp" (my/noema-roam--tmp-root)
                            "--layout" (my/noema-roam--workspace-layout)
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
                     (message "Noema roam runtime: JSON parse failed: %s%s"
                              err
                              (if (string-empty-p stderr) "" (concat "\n" stderr))))
                   nil))
              (let ((stderr (with-temp-buffer
                               (ignore-errors (insert-file-contents stderr-file))
                               (string-trim (buffer-string)))))
                (message "Noema roam runtime failed (%s): %s"
                         action
                         (if (string-empty-p stderr)
                             (string-trim (buffer-string))
                           stderr))
                nil))
          (ignore-errors (delete-file stderr-file))))))))

(defun my/noema-roam--runtime-index ()
  "Return cached Noema runtime index payload, or nil."
  (let ((key (list (file-truename (my/noema-roam-root))
                   (file-truename
                    (expand-file-name my/noema-roam-runtime-root)))))
    (if (and my/noema-roam--runtime-index-cache
             (equal key my/noema-roam--runtime-index-cache-key))
        my/noema-roam--runtime-index-cache
      (setq my/noema-roam--runtime-index-cache
            (my/noema-roam--runtime-call "index")
            my/noema-roam--runtime-index-cache-key key)
      my/noema-roam--runtime-index-cache)))

(defun my/noema-roam--target-at-point ()
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
          (while (string-match my/noema-roam--wiki-link-regexp line pos)
            (record (match-beginning 0) (match-end 0)
                    (my/noema-roam--wiki-link-href (match-string 1 line)))
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

(defun my/noema-roam--decode-ref (ref)
  "Percent-decode note REF, returning REF unchanged on malformed input."
  (condition-case nil
      (url-unhex-string (or ref ""))
    (error (or ref ""))))

(defun my/noema-roam--encode-ref (ref)
  "Percent-encode REF for use in Noema roam URLs."
  (url-hexify-string (or ref "")))

(defconst my/noema-roam--wiki-link-regexp
  "\\[\\[\\([^]\n|]+?\\)\\(?:|\\([^]\n]+?\\)\\)?\\]\\]"
  "Regexp matching a `[[target]]' or `[[target|label]]' wiki link.
Mirrors `WIKI_LINK_RE' in Noema's `shared/wiki-link.mjs' so Emacs and the
web editor agree on where a wiki link starts and ends.")

(defun my/noema-roam--wiki-link-href (target)
  "Return the canonical roam href for wiki-link TARGET, or nil.

TARGET is the part before any `|label'.  Two shapes reach us:

- the stable form the web editor inserts, `roam://<id>#<fragment>', which is
  already canonical and must be passed through untouched; and
- a plain page title, which becomes `roam://wiki/<title>' to match
  `wikiHrefForTarget' in `shared/wiki-link.mjs'.

Percent-encoding the stable form was the old bug: `[[roam://id#f|Label]]'
turned into `roam://roam%3A%2F%2Fid%23f%7CLabel', which resolved to nothing."
  (let ((value (string-trim (or target ""))))
    (cond
     ((string-empty-p value) nil)
     ((string-match-p "\\`roam://" value) value)
     (t (concat "roam://wiki/" (my/noema-roam--encode-ref value))))))

(defun my/noema-roam--wiki-link-at (line pos)
  "Return (BEG END HREF) for the wiki link matched in LINE from POS, or nil."
  (when (string-match my/noema-roam--wiki-link-regexp line pos)
    (list (match-beginning 0)
          (match-end 0)
          (my/noema-roam--wiki-link-href (match-string 1 line)))))

(defun my/noema-roam--split-target (target)
  "Split Noema TARGET into note ref plus optional tag or DOM target.
Canonical targets look like `roam://note-id', `roam://note-id#tag', and
`roam://note-id@dom-target'.  `@@parent@child' addresses a hierarchical DOM
target in the current note.  Path-like refs are accepted as input and later
resolved using the same note lookup path."
  (when (and (stringp target) (not (string-empty-p target)))
    (let* ((raw (string-trim target))
           (body (replace-regexp-in-string "\\`roam://" "" raw t t))
           (body (replace-regexp-in-string "\\`file://" "" body t t))
           ;; `wikiHrefForTarget' emits roam://wiki/<title>; the trailing part
           ;; is an ordinary page ref once the namespace marker is removed.
           (body (replace-regexp-in-string "\\`wiki/" "" body t t))
           (body (or (car (split-string body "[?&]" t)) ""))
           (local (string-prefix-p "@@" body))
           ref tag dom)
      (cond
       (local
        (setq ref ""
              dom (my/noema-roam--decode-ref (substring body 2))))
       ((string-match "\\`\\(.*?\\)#\\([^#]*\\)\\'" body)
        (setq ref (match-string 1 body)
              tag (my/noema-roam--decode-ref (match-string 2 body))))
       ((string-match "\\`\\(.*?\\)@\\([^#]*\\)\\'" body)
        (setq ref (match-string 1 body)
              dom (my/noema-roam--decode-ref (match-string 2 body))))
       (t
        (setq ref body)))
      (list :raw raw
            :local local
            :ref (string-trim
                  (replace-regexp-in-string
                   "\\`/+" ""
                   (my/noema-roam--decode-ref (or ref ""))))
            :tag (and tag (not (string-empty-p tag)) tag)
            :dom (and dom (not (string-empty-p dom)) dom)))))

(defun my/noema-roam--parse-target (target &optional base-dir)
  "Parse note-link TARGET into Noema-compatible target metadata.
BASE-DIR is forwarded to `my/noema-roam--ref-to-file-fallback' for
plain-relative refs (./x, ../x); defaults to the current buffer's directory."
  (when-let* ((parts (my/noema-roam--split-target target)))
    (let* ((ref (plist-get parts :ref))
           (local (plist-get parts :local))
           (resolved (my/noema-roam--resolve-note ref))
           (id (or (plist-get resolved :id) ref))
           (file (or (plist-get resolved :file)
                     (and local buffer-file-name)
                     (my/noema-roam--ref-to-file-fallback ref base-dir))))
      (append parts
              (list :slug id
                    :note-id id
                    :id (plist-get parts :tag)
                    :file file
                    :key (plist-get resolved :key)
                    :note (plist-get resolved :note))))))

(defun my/noema-roam--slug-at-point ()
  "Return the note-link slug at or near point, or nil."
  (plist-get (my/noema-roam--parse-target (my/noema-roam--target-at-point))
             :slug))

(defconst my/noema-roam--excluded-directories
  '("_typst" "var" ".git" ".lake" ".noema" ".direnv" ".venv"
    "node_modules" "__pycache__" ".ipynb_checkpoints" ".jupyter"
    ".pytest_cache" ".mypy_cache" ".ruff_cache" ".virtual_documents")
  "Directory names never descended into when scanning for notes.
Mirrors `excludedDirs' in Noema's `server/lib/runtime.mjs' so the Emacs
fallback scanner and the backend agree on what the vault contains.  Unlike
the backend set this also lists `.noema', whose worktrees hold copies of
real notes carrying the same `id:' as their originals.

`public' is deliberately absent: under the wiki layout it is a real note
partition, and the backend now makes the same distinction via its
`isExcludedDir' helper.")

(defun my/noema-roam--descend-directory-p (dir)
  "Return non-nil when the scanner should descend into DIR."
  (not (member (file-name-nondirectory (directory-file-name dir))
               my/noema-roam--excluded-directories)))

(defun my/noema-roam--all-files ()
  "Return all Markdown roam note files, excluding generated/private dirs.
Pruning happens during the walk rather than afterwards: the vault root also
holds `.lake' and `.noema' trees with tens of thousands of files, and
filtering a completed walk meant paying for all of them on every scan."
  (or my/noema-roam--all-files-cache
      (setq my/noema-roam--all-files-cache
            (directory-files-recursively
             (my/noema-roam-root) "\\.\\(?:md\\|markdown\\)\\'"
             nil #'my/noema-roam--descend-directory-p))))

(defun my/noema-roam--file-to-slug (file)
  "Convert FILE path to a roam slug, relative to root and without extension."
  (my/noema-roam--path-without-note-extension
   (file-relative-name file (my/noema-roam-root))))

(defun my/noema-roam--file-to-note-id (file)
  "Return the canonical note id for FILE, falling back to its path slug."
  (let* ((slug (my/noema-roam--file-to-slug file))
         (resolved (my/noema-roam--resolve-note slug)))
    (or (plist-get resolved :id) slug)))

(defun my/noema-roam--ref-has-extension-p (ref)
  "Return non-nil when REF already names a note file extension."
  (string-match-p "\\.\\(?:typ\\|md\\|markdown\\)\\'" (or ref "")))

(defun my/noema-roam--path-without-note-extension (path)
  "Remove a note file extension from PATH."
  (replace-regexp-in-string "\\.\\(?:typ\\|md\\|markdown\\)\\'" "" (or path "")))

(defun my/noema-roam--strip-vault-prefix (ref)
  "Remove Noema's exported `roam/' prefix from path REF."
  (let ((clean (replace-regexp-in-string "\\`/+" "" (or ref ""))))
    (if (string-prefix-p "roam/" clean)
        (substring clean 5)
      clean)))

(defun my/noema-roam--ref-to-file-fallback (ref &optional base-dir)
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
           ((and (not (my/noema-roam--ref-has-extension-p raw))
                 (file-exists-p (concat path ".md")))
            (concat path ".md"))
           ((and (not (my/noema-roam--ref-has-extension-p raw))
                 (file-exists-p (concat path ".markdown")))
            (concat path ".markdown"))
           (t path)))
      ;; Non-relative: resolve against the roam vault root.
      (let* ((clean (my/noema-roam--strip-vault-prefix raw))
             (root (my/noema-roam-root))
             (path (if (file-name-absolute-p clean)
                       clean
                     (expand-file-name clean root))))
        (cond
         ((and (not (string-empty-p clean))
               (file-exists-p path))
          path)
         ((and (not (string-empty-p clean))
               (not (my/noema-roam--ref-has-extension-p clean))
               (file-exists-p (concat path ".md")))
          (concat path ".md"))
         ((and (not (string-empty-p clean))
               (not (my/noema-roam--ref-has-extension-p clean))
               (file-exists-p (concat path ".markdown")))
          (concat path ".markdown"))
         ((my/noema-roam--ref-has-extension-p clean)
          path)
         (t
          (concat path ".md")))))))

(defun my/noema-roam--slug-to-file (slug)
  "Convert SLUG, id, or path-like ref to an absolute note path."
  (or (plist-get (my/noema-roam--resolve-note slug) :file)
      (my/noema-roam--ref-to-file-fallback slug)))

(defun my/noema-roam--slugify-title (title)
  "Return an Noema-style slug for TITLE."
  (let ((slug (downcase
               (replace-regexp-in-string
                "-+" "-"
                (replace-regexp-in-string
                 "\\`-\\|-\\'" ""
                 (replace-regexp-in-string
                  "[^[:alnum:]_]+" "-"
                  (string-trim title)))))))
    (if (string-empty-p slug) "untitled" slug)))

(defun my/noema-roam--timestamp-id ()
  "Return an Noema-style timestamp id."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun my/noema-roam--open-slug (slug &optional no-recent)
  "Open roam note SLUG/id/path and record it in recent notes unless NO-RECENT."
  (let* ((resolved (my/noema-roam--resolve-note slug))
         (note-id (or (plist-get resolved :id) slug))
         (file (or (plist-get resolved :file)
                   (my/noema-roam--ref-to-file-fallback slug))))
    (unless (file-exists-p file)
      (user-error "Note not found: %s" slug))
    (unless no-recent
      (my/noema-roam--touch-recent note-id))
    (find-file file)))

(defun my/noema-roam--touch-recent (slug)
  "Move SLUG to the front of the recent list."
  (when (and (stringp slug) (not (string-empty-p slug)))
    (setq my/noema-roam--recent
          (seq-take (cons slug (delete slug my/noema-roam--recent))
                    my/noema-roam-recent-limit))))

(defun my/noema-roam--note-title (slug)
  "Return display title for SLUG."
  (or (when-let* ((note (my/noema-roam--db-note slug)))
        (gethash "title" note))
      (plist-get (my/noema-roam--resolve-note slug) :title)
      (file-name-nondirectory slug)))

(defun my/noema-roam--note-tags (slug)
  "Return tags for SLUG."
  (when-let* ((note (my/noema-roam--db-note slug)))
    (my/noema-roam--note-list-field note "tags")))

(defun my/noema-roam--note-links (slug)
  "Return normalized outgoing link slugs for SLUG."
  (when-let* ((note (my/noema-roam--db-note slug)))
    (delete-dups
     (seq-filter #'identity
                 (mapcar #'my/noema-roam--target-slug
                         (or (my/noema-roam--note-list-field note "links")
                             (my/noema-roam--note-list-field note "refs")))))))

(defun my/noema-roam--note-summary (slug)
  "Return a compact text summary for SLUG."
  (or (when-let* ((note (my/noema-roam--db-note slug)))
        (my/noema-roam--note-field note "summary"))
      (let ((file (my/noema-roam--slug-to-file slug)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 20000)
            (or (my/noema-roam--extract-summary-block)
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

(defun my/noema-roam--backlinks-map (records)
  "Return a hash-table mapping note id → list of backlink ids from RECORDS.
Builds the reverse-link index in one pass to avoid O(n²) per-note lookups."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (record records)
      (let* ((note   (plist-get record :note))
             (source (plist-get record :id))
             (links  (or (my/noema-roam--note-list-field note "links")
                         (my/noema-roam--note-list-field note "refs"))))
        (dolist (link links)
          (let ((target (my/noema-roam--target-slug link)))
            (when target
              (puthash target
                       (cons source (gethash target map))
                       map))))))
    map))

(defun my/noema-roam--all-note-summaries ()
  "Return note summary plists for all notes, memoised between syncs."
  (or my/noema-roam--all-note-summaries-cache
      (setq my/noema-roam--all-note-summaries-cache
            (let* ((records (sort (my/noema-roam--note-records)
                                  (lambda (a b)
                                    (string< (plist-get a :id) (plist-get b :id)))))
                   ;; Build backlink map in one pass; fall back to DB field when present.
                   (bl-map (my/noema-roam--backlinks-map records)))
              (mapcar (lambda (record)
                        (let* ((id   (plist-get record :id))
                               (note (plist-get record :note))
                               ;; Prefer the DB-provided backlinks field when available.
                               (bl   (or (my/noema-roam--note-list-field note "backlinks")
                                         (delete-dups
                                          (nreverse (gethash id bl-map))))))
                          (list :slug      id
                                :title     (or (plist-get record :title)
                                               (my/noema-roam--note-title id))
                                :path      (or (my/noema-roam--note-field note "path")
                                               (my/noema-roam--note-field note "link"))
                                :aliases   (my/noema-roam--note-list-field note "aliases")
                                :tags      (my/noema-roam--note-tags id)
                                :links     (my/noema-roam--note-links id)
                                :backlinks bl
                                :summary   (my/noema-roam--note-summary id))))
                      records)))))

(defun my/noema-roam--candidate-haystack (entry)
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

(defun my/noema-roam--read-note (prompt &optional entries)
  "Read a note slug with PROMPT from ENTRIES or all summaries."
  (let* ((items (or entries (my/noema-roam--all-note-summaries)))
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

(defun my/noema-roam--read-note-id (prompt)
  "Read an Noema note id with PROMPT."
  (let* ((records (my/noema-roam--note-records))
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
                        (when-let* ((path (or (my/noema-roam--note-field note "path")
                                              (my/noema-roam--note-field note "link"))))
                          (concat "  " path))
                        (when-let* ((tags (seq-filter #'stringp
                                                      (my/noema-roam--note-list-field note "tags"))))
                          (concat "  #" (string-join tags " #")))))))))
         (complete-with-action action candidates string pred)))
     nil t)))

(defun my/noema-roam--roam-href (note-id &optional kind target)
  "Return canonical Noema roam href for NOTE-ID and optional TARGET."
  (concat "roam://"
          (my/noema-roam--encode-ref note-id)
          (pcase kind
            ('tag (concat "#" (my/noema-roam--encode-ref target)))
            ('dom (concat "@" (mapconcat #'my/noema-roam--encode-ref
                                          (my/noema-roam--dom-target-segments target)
                                          "@")))
            (_ ""))))

(defun my/noema-roam--heading-labels (&optional file)
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

(defun my/noema-roam--goto-tag-id (id)
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
   ((when-let* ((target (my/noema-roam--find-dom-target id)))
      (my/noema-roam--goto-pos (plist-get target :pos))
      t))
   (t
    (user-error "Tag id not found: %s" id)))
  (recenter-top-bottom))

(defun my/noema-roam--heading-items (&optional file)
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

(defun my/noema-roam--goto-pos (pos)
  "Move to POS, treating nil or synthetic zero positions as file start."
  (goto-char (if (and (integerp pos) (>= pos (point-min)))
                 pos
               (point-min))))

(defun my/noema-roam--normalize-dom-target (value)
  "Normalize Noema DOM target VALUE for matching."
  (string-trim
   (replace-regexp-in-string
    "\\s-+" " "
    (replace-regexp-in-string
     "[][\r\n]" " "
     (string-remove-prefix "@" (my/noema-roam--decode-ref (or value "")))))))

(defun my/noema-roam--slug-dom-target (value)
  "Return Noema's DOM target slug for VALUE."
  (let ((clean (downcase
                (replace-regexp-in-string
                 "[`*_~()[\\]{}#+.!<>:;,'\"@]" " "
                 (my/noema-roam--normalize-dom-target value)))))
    (replace-regexp-in-string
     "\\s-+" "-"
     (string-trim clean))))

(defun my/noema-roam--dom-target-segments (value)
  "Return normalized DOM target path segments from VALUE."
  (seq-filter
   (lambda (segment) (not (string-empty-p segment)))
   (mapcar #'my/noema-roam--slug-dom-target
           (split-string (string-remove-prefix "@" (or value "")) "@"))))

(defun my/noema-roam--dom-targets (&optional file note-id)
  "Return Noema-style DOM/TOC targets for FILE or current buffer."
  (let ((items (my/noema-roam--heading-items file))
        (stack nil)
        (label-stack nil)
        targets)
    (when-let* ((note-id)
                (title (plist-get (my/noema-roam--resolve-note note-id) :title)))
      (let ((label (my/noema-roam--normalize-dom-target title))
            (slug (my/noema-roam--slug-dom-target title)))
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
             (label (my/noema-roam--normalize-dom-target
                     (plist-get item :text)))
             (slug (my/noema-roam--slug-dom-target label)))
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

(defun my/noema-roam--dom-target-path-label (target)
  "Return a readable label path for TARGET."
  (string-join (plist-get target :label-path) " / "))

(defun my/noema-roam--target-path-matches-p (actual wanted &optional allow-suffix)
  "Return non-nil when ACTUAL target path matches WANTED."
  (let ((actual (mapcar #'my/noema-roam--slug-dom-target actual))
        (wanted (mapcar #'my/noema-roam--slug-dom-target wanted)))
    (cond
     ((or (null actual) (null wanted)) nil)
     ((equal actual wanted) t)
     ((and allow-suffix
           (>= (length actual) (length wanted)))
      (equal (last actual (length wanted)) wanted)))))

(defun my/noema-roam--find-dom-target (dom &optional file note-id)
  "Find DOM target DOM in FILE or current buffer."
  (let* ((wanted (my/noema-roam--dom-target-segments dom))
         (targets (my/noema-roam--dom-targets file note-id)))
    (cond
     ((null wanted) nil)
     ((> (length wanted) 1)
      (or (seq-find (lambda (target)
                      (my/noema-roam--target-path-matches-p
                       (plist-get target :path) wanted))
                    targets)
          (seq-find (lambda (target)
                      (my/noema-roam--target-path-matches-p
                       (plist-get target :path) wanted t))
                    targets)))
     (t
      (let* ((wanted-segment (car wanted))
             (wanted-label (my/noema-roam--normalize-dom-target dom)))
        (seq-find
         (lambda (target)
           (or (equal (plist-get target :slug) wanted-segment)
               (equal (downcase (plist-get target :label))
                      (downcase wanted-label))))
         targets))))))

(defun my/noema-roam--goto-dom-target (dom)
  "Jump to Noema DOM/TOC target DOM in the current buffer."
  (let* ((target (my/noema-roam--find-dom-target dom))
         (pos (and target (plist-get target :pos))))
    (unless pos
      (user-error "DOM target not found: %s" dom))
    (my/noema-roam--goto-pos pos)
    (recenter-top-bottom)))

(defun my/noema-roam--read-dom-target (note-id)
  "Read an Noema DOM/TOC target for NOTE-ID."
  (let* ((record (my/noema-roam--resolve-note note-id))
         (file (plist-get record :file))
         (targets (my/noema-roam--dom-targets file note-id))
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
                                         (my/noema-roam--dom-target-path-label
                                          target))))))
                      (complete-with-action action table string pred)))
                  nil t)))
    (cdr (assoc choice table))))

(defun my/noema-roam--ui-actions (&optional leading)
  "Return standard native roam view actions after optional LEADING actions."
  (append
   leading
   '((:label "g Refresh"
      :command my/noema-roam-ui-refresh
      :help "Refresh this Noema roam view"
      :primary t)
     (:label "q Close"
      :command quit-window
      :help "Close this Noema roam view"))))

(defun my/noema-roam--prepare-ui-buffer
    (name title icon refresh-function &optional status)
  "Prepare native roam buffer NAME with TITLE, ICON, REFRESH-FUNCTION, and STATUS."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/noema-roam-ui-mode)
        (my/noema-roam-ui-mode))
      (setq-local my/noema-roam-ui-refresh-function refresh-function)
      (my/noema-roam-ui-set-header title icon status))
    buffer))

(defun my/noema-roam--show-toc (&optional file title)
  "Show a heading TOC for FILE or the current buffer."
  (let* ((source-buffer (current-buffer))
         (target-file (or file buffer-file-name))
         (items (if file
                    (my/noema-roam--heading-items file)
                  (with-current-buffer source-buffer
                    (my/noema-roam--heading-items))))
         (display-title (or title file (buffer-name source-buffer)))
         (refresh
          (let ((source source-buffer) (f file) (label title))
            (lambda ()
              (if (buffer-live-p source)
                  (with-current-buffer source
                    (my/noema-roam--show-toc f label))
                (my/noema-roam--show-toc f label)))))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-toc*" "Roam TOC" 'toc refresh
               (format "%d headings" (length items)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Table of contents"
          :icon 'toc
          :subtitle (format "%s" display-title)
          :stats (list (cons (format "%d headings" (length items)) 'info))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Headings" (length items))
         (if (null items)
             (my/noema-roam-ui-insert-empty "No headings in this note.")
           (dolist (item items)
             (let ((pos (plist-get item :pos))
                   (level (plist-get item :level))
                   (text (plist-get item :text)))
               (my/noema-roam-ui-insert-row
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
                    (my/noema-roam--goto-pos position)
                    (recenter-top-bottom))))))))))
    (display-buffer buf)))

(defun my/noema-roam--open-file-smart (file parsed)
  "Open FILE using smart routing based on its type.
PARSED is the plist from `my/noema-roam--parse-target'; it carries the
optional #tag / @dom target for Markdown notes."
  (cond
   ;; Directory → dired.
   ((file-directory-p file)
    (dired file))
   ;; Markdown note → Emacs + optional in-note navigation.
   ((string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
    (my/noema-roam--touch-recent (plist-get parsed :slug))
    (find-file file)
    (cond
     ((plist-get parsed :id)
      (my/noema-roam--goto-tag-id (plist-get parsed :id)))
     ((plist-get parsed :dom)
      (my/noema-roam--goto-dom-target (plist-get parsed :dom)))))
   ;; Everything else (PDF, image, Lean source, etc.) → central open route.
   (t
    (require 'init-open)
    (my/open-file file))))

(defun my/noema-roam-follow-link ()
  "Jump to the note or source region referenced at point.
Targets may use Noema roam syntax:
  roam://note-id
  roam://note-id#tag
  roam://note-id@dom-target
  @@parent@child
Plain-relative refs (./x, ../x) are resolved against the current note's
directory; /x is resolved against the roam vault root."
  (interactive)
  (if (and (fboundp 'my/note-code-at-point)
           (my/note-code-at-point))
      (my/note-code-open-at-point)
    (let* ((base-dir (and buffer-file-name
                          (file-name-directory buffer-file-name)))
           (target (my/noema-roam--target-at-point))
           (parsed (and target (my/noema-roam--parse-target target base-dir)))
           (file (and parsed (plist-get parsed :file)))
           (ref (and parsed (plist-get parsed :ref))))
      (if (and file (file-exists-p file))
          (my/noema-roam--open-file-smart file parsed)
        (if ref
            (when (yes-or-no-p (format "Note '%s' not found. Create it? " ref))
              (my/noema-roam--create-linked-node ref))
          (user-error "No Markdown roam link found at point"))))))

(defun my/noema-roam-find-note ()
  "Find a roam note by Noema id/path/title with completion."
  (interactive)
  (my/noema-roam--open-slug
   (my/noema-roam--read-note-id "Roam note: ")))

(defun my/noema-roam-delete-node (note-id)
  "Move NOTE-ID's Markdown node to trash through the Noema runtime."
  (interactive (list (my/noema-roam--read-note-id "Delete roam node: ")))
  (let* ((record (seq-find
                  (lambda (candidate)
                    (equal (plist-get candidate :id) note-id))
                  (my/noema-roam--note-records)))
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
        (my/noema-roam--runtime-call "delete-node" "--file" file)
        (my/noema-roam--clear-runtime-cache)
        (when delete-current-buffer
          (kill-buffer (current-buffer)))
        (message "Noema node moved to trash: %s"
                 (abbreviate-file-name file))))))

(defun my/noema-roam-insert-link ()
  "Open the interactive selector and insert a Markdown roam link."
  (interactive)
  (my/noema-roam-select-link))

(defvar-local my/noema-roam-new--draft nil
  "Draft plist edited by the current Roam Node buffer.")

(defvar-local my/noema-roam-new--templates nil
  "Template records available to the current Roam Node buffer.")

(defvar-local my/noema-roam-new--base-directory ""
  "Relative default directory used by the current Roam Node buffer.")

(defvar-local my/noema-roam-new--widgets nil
  "Editable widgets in the current Roam Node buffer.")

(defvar my/noema-roam-new-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map my/noema-roam-ui-mode-map)
    (define-key map (kbd "c") #'my/noema-roam-new-create)
    (define-key map (kbd "R") #'my/noema-roam-new-reset)
    (define-key map (kbd "t") #'my/noema-roam-new-edit-type)
    (define-key map (kbd "T") #'my/noema-roam-new-edit-template)
    (define-key map (kbd "a") #'my/noema-roam-new-edit-tags)
    (define-key map (kbd "p") #'my/noema-roam-new-edit-path)
    map)
  "Keymap for `my/noema-roam-new-mode'.")

(define-derived-mode my/noema-roam-new-mode my/noema-roam-ui-mode "Roam-Node"
  "Native workbench for creating Noema Markdown nodes."
  ;; This view is a form.  Keep it writable so Emacs widget fields accept direct
  ;; typing instead of forcing every edit through the minibuffer.
  (setq-local buffer-read-only nil)
  (setq-local my/noema-roam-ui-refresh-function
              #'my/noema-roam-new-refresh)
  (setq-local widget-button-face 'my/noema-roam-ui-action)
  (setq-local widget-field-face 'my/noema-roam-ui-row-title)
  (my/noema-roam-ui-set-header "Roam Node" 'new "draft"))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/noema-roam-new-mode 'emacs))

(defun my/noema-roam-new--normalize-directory (directory)
  "Return DIRECTORY as a clean vault-relative directory."
  (let ((directory
         (replace-regexp-in-string
          "/+\\'" ""
          (replace-regexp-in-string
           "\\`\\./" ""
           (string-trim (or directory ""))))))
    (if (member directory '("" "." "Root")) "" directory)))

(defun my/noema-roam-new--default-path (title &optional directory)
  "Return the default Markdown path for TITLE in DIRECTORY."
  (let ((directory (my/noema-roam-new--normalize-directory directory))
        (name (concat (my/noema-roam--slugify-title title) ".md")))
    (if (string-empty-p directory)
        name
      (concat directory "/" name))))

(defun my/noema-roam-new--path-directory (path)
  "Return PATH's vault-relative directory."
  (my/noema-roam-new--normalize-directory
   (or (file-name-directory (or path "")) "")))

(defun my/noema-roam-new--path-basename (path title)
  "Return PATH's filename, falling back to TITLE's default Markdown filename."
  (let ((name (file-name-nondirectory (or path ""))))
    (if (string-empty-p name)
        (file-name-nondirectory
         (my/noema-roam-new--default-path title))
      name)))

(defun my/noema-roam-new--path-file (path)
  "Return absolute file for vault-relative PATH."
  (expand-file-name path (file-name-as-directory (my/noema-roam-root))))

(defun my/noema-roam-new--unique-path (path)
  "Return PATH, or a numbered variant, that does not already exist."
  (let* ((path (string-trim (or path "")))
         (dir (or (file-name-directory path) ""))
         (base (file-name-base path))
         (ext (or (file-name-extension path t) ".md"))
         (candidate path)
         (n 2))
    (while (and (not (string-empty-p candidate))
                (file-exists-p
                 (my/noema-roam-new--path-file candidate)))
      (setq candidate (concat dir base "-" (number-to-string n) ext)
            n (1+ n)))
    candidate))

(defun my/noema-roam-new--normalize-tags (tags)
  "Return TAGS in the same canonical form as the Noema runtime.
TAGS may be a string, list, or vector.  Commas and whitespace separate tags;
an optional leading # is display syntax and is not part of the stored tag.
Duplicates are removed case-insensitively and the result is sorted."
  (let ((by-key (make-hash-table :test #'equal)))
    (dolist (raw (cond
                  ((null tags) nil)
                  ((stringp tags) (split-string tags "[,[:space:]]+" t))
                  ((sequencep tags) (append tags nil))
                  (t (list tags))))
      (let* ((clean (replace-regexp-in-string
                     "\\`#+" "" (string-trim (format "%s" raw))))
             (key (downcase clean))
             (previous (gethash key by-key)))
        (when (and (not (string-empty-p clean))
                   (or (null previous) (string= clean key)))
          (puthash key clean by-key))))
    (sort (hash-table-values by-key)
          (lambda (left right)
            (string-lessp (downcase left) (downcase right))))))

(defun my/noema-roam-new--tag-display (tags)
  "Return TAGS as Roam-style #tag display text."
  (mapconcat (lambda (tag) (concat "#" tag))
             (my/noema-roam-new--normalize-tags tags)
             " "))

(defun my/noema-roam-new--default-draft (&optional directory)
  "Return a default Roam Node draft rooted in DIRECTORY."
  (list :node-type "roam"
        :title "Untitled"
        :path (my/noema-roam-new--unique-path
               (my/noema-roam-new--default-path "Untitled" directory))
        :kind "note"
        :template-key "roam"
        :tags nil))

(defun my/noema-roam-new--draft-for-create (draft &optional directory)
  "Return DRAFT normalized for Noema create-node.
Empty title, path, and kind fields receive the same defaults as the Noema
create-node API.  DIRECTORY defaults to the current Roam Node base directory."
  (let* ((node-type
          (if (string= (downcase (format "%s" (plist-get draft :node-type)))
                       "regular")
              "regular"
            "roam"))
         (title (string-trim (or (plist-get draft :title) "")))
         (title (if (string-empty-p title) "Untitled" title))
         (base-directory
          (or directory my/noema-roam-new--base-directory))
         (raw-path (string-trim (or (plist-get draft :path) "")))
         (untitled-path
          (my/noema-roam-new--default-path "Untitled" base-directory))
         (path (if (or (string-empty-p raw-path)
                       (and (not (string= title "Untitled"))
                            (equal raw-path untitled-path)))
                   (my/noema-roam-new--default-path title base-directory)
                 raw-path))
         (path (my/noema-roam-new--unique-path path))
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
                                (my/noema-roam-new--normalize-tags
                                 (plist-get draft :tags))))
    normalized))

(defun my/noema-roam-new--template-field (template field)
  "Return FIELD from TEMPLATE, which may be a hash table or plist."
  (if (hash-table-p template)
      (gethash (substring (symbol-name field) 1) template)
    (plist-get template field)))

(defun my/noema-roam-new--load-templates ()
  "Return templates reported by the Noema runtime."
  (let ((response (my/noema-roam--runtime-call "templates" "--force")))
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

(defun my/noema-roam-new--template-label (key)
  "Return a display label for template KEY."
  (if (string-empty-p (or key ""))
      "None"
    (let ((template
           (seq-find
            (lambda (candidate)
              (equal (my/noema-roam-new--template-field candidate :key)
                     key))
            my/noema-roam-new--templates)))
      (or (and template
               (my/noema-roam-new--template-field template :name))
          key))))

(defun my/noema-roam-new--template-candidates ()
  "Return display-name and key pairs for available templates."
  (let* ((active-kind (plist-get my/noema-roam-new--draft :kind))
         (templates
          (seq-filter
           (lambda (template)
             (let ((kind
                    (my/noema-roam-new--template-field template :kind)))
               (or (string-empty-p (or kind ""))
                   (equal kind active-kind))))
           my/noema-roam-new--templates)))
    (cons
     '("None" . "")
     (mapcar
      (lambda (template)
        (let ((key (my/noema-roam-new--template-field template :key))
              (name (my/noema-roam-new--template-field template :name))
              (kind (my/noema-roam-new--template-field template :kind)))
          (cons (format "%s%s"
                        (or name key "Template")
                        (if (string-empty-p (or kind ""))
                            ""
                          (format " (%s)" kind)))
                key)))
      templates))))

(defun my/noema-roam-new--path-suggestions ()
  "Return vault-relative directory suggestions for Roam Node."
  (let ((root (file-name-as-directory (my/noema-roam-root)))
        (directories '("")))
    (dolist (record (my/noema-roam--note-records))
      (when-let* ((file (plist-get record :file))
                  ((file-name-absolute-p file))
                  (relative (file-relative-name file root))
                  (directory (file-name-directory relative)))
        (push directory directories)))
    (sort (delete-dups directories) #'string<)))

(defun my/noema-roam-new--tag-suggestions ()
  "Return known roam tags for Roam Node."
  (sort
   (delete-dups
    (apply #'append
           (mapcar
            (lambda (record)
              (my/noema-roam--note-list-field
               (plist-get record :note) "tags"))
            (my/noema-roam--note-records))))
   #'string<))

(defun my/noema-roam-new--kind-suggestions ()
  "Return known note kinds for Roam Node."
  (sort
   (delete-dups
    (cons "note"
          (cons "default"
                (delq nil
                      (mapcar
                       (lambda (record)
                         (my/noema-roam--note-field
                          (plist-get record :note) "kind"))
                       (my/noema-roam--note-records))))))
   #'string<))

(defun my/noema-roam-new--set (key value)
  "Set draft KEY to VALUE and rerender the Roam Node buffer."
  (my/noema-roam-new--sync-draft-from-widgets)
  (setq-local my/noema-roam-new--draft
              (plist-put my/noema-roam-new--draft key value))
  (my/noema-roam-new-render t))

(defun my/noema-roam-new--plain-widget-value (key)
  "Return editable widget KEY's plain string value, or nil."
  (when-let* ((widget (alist-get key my/noema-roam-new--widgets)))
    (substring-no-properties (format "%s" (widget-value widget)))))

(defun my/noema-roam-new--sync-draft-from-widgets ()
  "Copy editable field widget values into the current Roam Node draft."
  (when (and my/noema-roam-new--widgets my/noema-roam-new--draft)
    (let ((draft my/noema-roam-new--draft))
      (dolist (entry '((:title . title)
                       (:path . path)
                       (:kind . kind)))
        (when-let* ((value (my/noema-roam-new--plain-widget-value
                            (cdr entry))))
          (setq draft (plist-put draft (car entry) value))))
      (when-let* ((tags (my/noema-roam-new--plain-widget-value 'tags)))
        (setq draft
              (plist-put draft :tags
                         (my/noema-roam-new--normalize-tags tags))))
      (setq-local my/noema-roam-new--draft draft))))

(defun my/noema-roam-new-edit-type ()
  "Edit the note type in the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (let* ((old (plist-get my/noema-roam-new--draft :node-type))
         (next (completing-read "Note type: " '("roam" "regular")
                                nil t nil nil old))
         (template (plist-get my/noema-roam-new--draft :template-key))
         (kind (plist-get my/noema-roam-new--draft :kind)))
    (setq-local my/noema-roam-new--draft
                (plist-put my/noema-roam-new--draft :node-type next))
    (when (member template '("roam" "basic"))
      (setq-local my/noema-roam-new--draft
                  (plist-put my/noema-roam-new--draft :template-key
                             (if (string= next "roam") "roam" "basic"))))
    (when (member kind '("note" "default"))
      (setq-local my/noema-roam-new--draft
                  (plist-put my/noema-roam-new--draft :kind
                             (if (string= next "roam") "note" "default"))))
    (my/noema-roam-new-render t)))

(defun my/noema-roam-new-edit-title ()
  "Edit the title in the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (let* ((old-title (plist-get my/noema-roam-new--draft :title))
         (old-default
          (my/noema-roam-new--default-path
           old-title my/noema-roam-new--base-directory))
         (title (read-string "Title: " old-title)))
    (setq-local my/noema-roam-new--draft
                (plist-put my/noema-roam-new--draft :title title))
    (when (equal (plist-get my/noema-roam-new--draft :path) old-default)
      (setq-local my/noema-roam-new--draft
                  (plist-put
                   my/noema-roam-new--draft :path
                   (my/noema-roam-new--default-path
                    title my/noema-roam-new--base-directory))))
    (my/noema-roam-new-render t)))

(defun my/noema-roam-new-edit-path ()
  "Edit the save directory in the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (let* ((root (file-name-as-directory (expand-file-name (my/noema-roam-root))))
         (current (plist-get my/noema-roam-new--draft :path))
         (title (plist-get my/noema-roam-new--draft :title))
         (current-dir (my/noema-roam-new--path-directory current))
         (filename (my/noema-roam-new--path-basename current title))
         (initial-dir (expand-file-name current-dir root))
         (raw-dir (read-directory-name "Save directory: "
                                       initial-dir initial-dir nil))
         (selected-dir (file-name-as-directory (expand-file-name raw-dir root))))
    (unless (file-in-directory-p selected-dir root)
      (user-error "Save directory must be inside the Noema vault"))
    (my/noema-roam-new--set
     :path
     (my/noema-roam-new--unique-path
      (concat
       (let ((relative-dir
              (my/noema-roam-new--normalize-directory
               (file-relative-name selected-dir root))))
         (if (string-empty-p relative-dir)
             ""
           (concat relative-dir "/")))
       filename)))))

(defun my/noema-roam-new-edit-kind ()
  "Edit the note kind in the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--set
   :kind
   (completing-read "Kind: " (my/noema-roam-new--kind-suggestions)
                    nil nil nil nil
                    (plist-get my/noema-roam-new--draft :kind))))

(defun my/noema-roam-new-edit-template ()
  "Edit the template in the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (let* ((candidates (my/noema-roam-new--template-candidates))
         (current (my/noema-roam-new--template-label
                   (plist-get my/noema-roam-new--draft :template-key)))
         (choice (completing-read "Template: " candidates nil t nil nil current)))
    (my/noema-roam-new--set :template-key
                                (or (cdr (assoc choice candidates)) ""))))

(defun my/noema-roam-new-edit-tags ()
  "Edit tags in the current Roam Node draft, adding one at a time."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (let* ((suggestions (my/noema-roam-new--tag-suggestions))
         (tags (copy-sequence
                (or (plist-get my/noema-roam-new--draft :tags) nil))))
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
    (my/noema-roam-new--set
     :tags (my/noema-roam-new--normalize-tags tags))))

(defun my/noema-roam-new--insert-field
    (id icon label value detail action &optional tone)
  "Insert one selectable Roam Node field."
  (my/noema-roam-ui-insert-row
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

(defun my/noema-roam-new--editable-width ()
  "Return a reasonable width for Roam Node editable fields."
  (max 24 (min 72 (- (window-width) 32))))

(defun my/noema-roam-new--insert-editable-field
    (id icon label value detail key &optional placeholder action)
  "Insert directly editable Roam Node field KEY.
ID, ICON, LABEL, VALUE, DETAIL, PLACEHOLDER, and ACTION control display."
  (let ((start (point))
        (value (or value "")))
    (insert "   "
            (propertize (my/noema-roam-ui-icon icon)
                        'face 'my/noema-roam-ui-icon)
            "  ")
    (my/noema-roam-ui-insert-badge label 'muted)
    (insert "  ")
    (let* ((label-end (point))
           (widget
            (widget-create
             'editable-field
             :size (my/noema-roam-new--editable-width)
             :format "%v"
             :help-echo (format "Edit %s directly" (downcase label))
             :notify
             (lambda (_widget &rest _ignored)
               (my/noema-roam-new--sync-draft-from-widgets))
             (if (string-empty-p value) (or placeholder "") value))))
      (push (cons key widget) my/noema-roam-new--widgets)
      (insert "\n")
      (when (and detail (not (string-empty-p detail)))
        (insert "      "
                (propertize detail 'face 'my/noema-roam-ui-detail)
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
           keymap ,my/noema-roam-ui-row-map))))))

(defun my/noema-roam-new-render (&optional skip-sync)
  "Render the current Roam Node draft."
  (interactive)
  (unless skip-sync
    (my/noema-roam-new--sync-draft-from-widgets))
  ;; Delete stale widget registrations before erasing; otherwise widget-setup
  ;; sees both old and new fields and raises "Overlapping fields".
  (dolist (entry my/noema-roam-new--widgets)
    (condition-case nil (widget-delete (cdr entry)) (error nil)))
  (setq-local my/noema-roam-new--widgets nil)
  (let* ((draft my/noema-roam-new--draft)
         (node-type (plist-get draft :node-type))
         (title (plist-get draft :title))
         (path (plist-get draft :path))
         (kind (plist-get draft :kind))
         (template-key (plist-get draft :template-key))
         (template-label (my/noema-roam-new--template-label template-key))
         (tags (my/noema-roam-new--normalize-tags
                (plist-get draft :tags)))
         (id (plist-get draft :id)))
    (setq-local my/noema-roam-new--draft
                (plist-put draft :tags tags))
    (my/noema-roam-ui-set-header
     "Roam Node" 'new (format "%s draft" node-type))
    (my/noema-roam-ui-render
     (lambda ()
       (setq-local my/noema-roam-new--widgets nil)
       (my/noema-roam-ui-insert-page-header
        "New node"
        :icon 'new
        :subtitle "Type in fields; p for path; a to add tags one by one; c to create"
        :stats (list (cons (upcase node-type)
                           (if (string= node-type "roam") 'info 'muted))
                     (cons template-label 'muted))
        :actions
        '((:label "c Create"
           :command my/noema-roam-new-create
           :help "Create this node through the Noema runtime"
           :primary t)
          (:label "t Type"
           :command my/noema-roam-new-edit-type
           :help "Switch between a roam node and a regular Markdown document")
          (:label "T Template"
           :command my/noema-roam-new-edit-template
           :help "Choose a Markdown template")
          (:label "p Path"
           :command my/noema-roam-new-edit-path
           :help "Choose save path with file completion")
          (:label "a Tags"
           :command my/noema-roam-new-edit-tags
           :help "Add tags one by one with vault completion")
          (:label "R Reset"
           :command my/noema-roam-new-reset
           :help "Reset this draft")
          (:label "q Close"
           :command quit-window
           :help "Close without creating")))
       (my/noema-roam-ui-insert-section "Draft" 6)
       (my/noema-roam-new--insert-field
        'type 'status "TYPE" node-type
        "Press RET or t to switch roam / regular."
        #'my/noema-roam-new-edit-type
        (if (string= node-type "roam") 'info 'muted))
       (my/noema-roam-new--insert-editable-field
        'title 'note "TITLE" title
        "Used for the heading, metadata, and default save path."
        'title "Untitled")
       (my/noema-roam-new--insert-editable-field
        'path 'path "SAVE PATH" path
        "Vault-relative .md or .markdown path; p chooses a folder."
        'path "untitled.md" #'my/noema-roam-new-edit-path)
       (my/noema-roam-new--insert-editable-field
        'kind 'status "KIND" kind
        "Controls Noema note-kind behavior."
        'kind (if (string= node-type "roam") "note" "default"))
       (my/noema-roam-new--insert-field
        'template 'template "TEMPLATE" template-label
        "Press RET or T to choose a template."
        #'my/noema-roam-new-edit-template)
       (my/noema-roam-new--insert-editable-field
        'tags 'tag "TAGS"
        (my/noema-roam-new--tag-display tags)
        "Roam #tags; commas or spaces also work, and a adds with completion."
        'tags "" #'my/noema-roam-new-edit-tags)
       (insert "\n")
       (my/noema-roam-ui-insert-section "Result")
       (my/noema-roam-ui-insert-field
        "Node ID" (or id (if (string= node-type "roam")
                             "timestamped on create"
                           "none")))
       (my/noema-roam-ui-insert-field
        "Absolute path"
        (abbreviate-file-name
         (expand-file-name path (my/noema-roam-root)))
        'my/noema-roam-ui-path)
       (my/noema-roam-ui-insert-field
        "Create engine" "Noema create-node runtime" 'my/noema-roam-ui-meta)
       (widget-setup)))
    (unless (get-text-property (point) 'aaron-ui-board--item-id)
      (my/noema-roam-ui-goto-first-item))))

(defun my/noema-roam-new-refresh ()
  "Reload templates and rerender the current Roam Node draft."
  (interactive)
  (setq-local my/noema-roam-new--templates
              (my/noema-roam-new--load-templates))
  (my/noema-roam-new-render))

(defun my/noema-roam-new-reset ()
  "Reset the current Roam Node draft."
  (interactive)
  (setq-local my/noema-roam-new--draft
              (my/noema-roam-new--default-draft
               my/noema-roam-new--base-directory))
  (my/noema-roam-new-render))

(defun my/noema-roam-new (&optional base-directory draft)
  "Open the native Roam Node workbench.
BASE-DIRECTORY is vault-relative.  DRAFT overrides the initial draft plist."
  (let* ((base-directory
          (my/noema-roam-new--normalize-directory
           (or base-directory
               (when-let* ((file buffer-file-name)
                           ((file-in-directory-p file
                                                 (my/noema-roam-root))))
                 (file-relative-name
                  (file-name-directory file)
                  (my/noema-roam-root)))
               "")))
         (buffer (get-buffer-create "*roam-new-node*")))
    (with-current-buffer buffer
      (my/noema-roam-new-mode)
      (setq-local my/noema-roam-new--base-directory base-directory
                  my/noema-roam-new--templates
                  (my/noema-roam-new--load-templates)
                  my/noema-roam-new--draft
                  (or draft
                      (my/noema-roam-new--default-draft base-directory)))
      (my/noema-roam-new-render))
    (pop-to-buffer buffer)))

(defun my/noema-roam-new--payload (draft)
  "Return Noema runtime JSON payload for DRAFT."
  (delq nil
        `((nodeType . ,(plist-get draft :node-type))
          (title . ,(plist-get draft :title))
          (path . ,(plist-get draft :path))
          (kind . ,(plist-get draft :kind))
          (templateKey . ,(or (plist-get draft :template-key) ""))
          (tags . ,(vconcat (plist-get draft :tags)))
          ,(when-let* ((id (plist-get draft :id)))
             `(id . ,id)))))

(defun my/noema-roam-new--create-draft (draft)
  "Create and open DRAFT through the Noema runtime."
  (let* ((draft (my/noema-roam-new--draft-for-create draft))
         (payload (my/noema-roam-new--payload draft))
         (json (json-encode payload))
         (response (my/noema-roam--runtime-call "create" "--json" json))
         (file (and (hash-table-p response) (gethash "file" response))))
    (unless response
      (user-error "Noema runtime failed — see *Messages* for details"))
    (unless (and file (file-exists-p file))
      (user-error "Noema runtime did not create the node (path: %s)"
                  (or file "nil")))
    (my/noema-roam--clear-runtime-cache)
    (when (derived-mode-p 'my/noema-roam-new-mode)
      (kill-buffer (current-buffer)))
    (if (fboundp 'my/noema-open-file)
        (my/noema-open-file file)
      (find-file file))
    file))

(defun my/noema-roam-new-create ()
  "Create the current Roam Node draft."
  (interactive)
  (my/noema-roam-new--sync-draft-from-widgets)
  (my/noema-roam-new--create-draft my/noema-roam-new--draft))

(defun my/noema-roam--create-linked-node (slug &optional title tags)
  "Create an internal missing-link node at fixed roam SLUG.
TITLE defaults from SLUG.  TAGS use the canonical create-node tag rules."
  (let* ((slug (my/noema-roam--strip-vault-prefix (or slug "")))
         (path (if (my/noema-roam--ref-has-extension-p slug)
                   slug
                 (concat slug ".md")))
         (id (file-name-sans-extension slug))
         (title
          (or title
              (capitalize
               (replace-regexp-in-string
                "[-_/]" " " (file-name-nondirectory id))))))
    (my/noema-roam-new--create-draft
     (list :node-type "roam"
           :id id
           :title title
           :path path
           :kind "note"
           :template-key "roam"
           :tags (my/noema-roam-new--normalize-tags tags)))))

(defun my/noema-roam-new-node (&optional title directory tags)
  "Open Roam Node, or create a timestamped node from TITLE in DIRECTORY.
Optional TAGS are normalized with the same rules as the interactive form."
  (interactive)
  (if (called-interactively-p 'interactive)
      (my/noema-roam-new directory)
    (let* ((title (or title "Untitled"))
           (directory (my/noema-roam-new--normalize-directory directory))
           (id (format "%s-%s"
                       (my/noema-roam--timestamp-id)
                       (my/noema-roam--slugify-title title)))
           (path (if (string-empty-p directory)
                     (concat id ".md")
                   (concat directory "/" id ".md"))))
      (my/noema-roam-new--create-draft
       (list :node-type "roam"
             :id id
             :title title
             :path path
             :kind "note"
             :template-key "roam"
             :tags (my/noema-roam-new--normalize-tags tags))))))

;; ── Canonical Wiki records ───────────────────────────────────────────────────

(defvar my/noema-roam--scan-cache nil)

(defun my/noema-roam--note-field (note key)
  "Return string field KEY from NOTE."
  (when (hash-table-p note)
    (let ((value (gethash key note)))
      (when (and (stringp value) (not (string-empty-p value)))
        value))))

(defun my/noema-roam--note-list-field (note key)
  "Return list field KEY from NOTE."
  (let ((value (and (hash-table-p note) (gethash key note))))
    (cond
     ((listp value) value)
     ((vectorp value) (append value nil))
     ((and (stringp value) (not (string-empty-p value))) (list value)))))

(defun my/noema-roam--split-list-value (value)
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

(defun my/noema-roam--put-note-field (note key value)
  "Set NOTE KEY to VALUE when VALUE is present."
  (when (and value
             (not (and (stringp value) (string-empty-p (string-trim value)))))
    (puthash key value note)))

(defun my/noema-roam--parse-meta-line (note line)
  "Parse one KEY: VALUE metadata LINE into NOTE."
  (when (string-match "\\`\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (let* ((key (downcase (string-trim (match-string 1 line))))
           (value (string-trim (match-string 2 line))))
      (pcase key
        ((or "tags" "aliases" "refs" "links" "backlinks" "inlinetags")
         (my/noema-roam--put-note-field
          note key (my/noema-roam--split-list-value value)))
        (_
         (my/noema-roam--put-note-field note key value))))))

(defun my/noema-roam--read-org-meta-block (note)
  "Read an Noema `#+begin meta' block at point into NOTE.

A `#+begin summary' block may be nested inside the meta block.  Its body is
prose, not metadata, so it is skipped: a sentence like \"Note: something\"
matches the `key: value' grammar, and a prose line starting \"tags:\" would
otherwise replace the note's real tag list.  Mirrors `maskMetaSummaryContent'
in Noema's `shared/meta-summary.mjs'."
  (when (looking-at-p "\\s-*#\\+begin meta\\b")
    (forward-line 1)
    (let ((summary-depth 0))
      (while (and (not (eobp))
                  (not (looking-at-p "\\s-*#\\+end meta\\b")))
        (cond
         ((looking-at-p "\\s-*#\\+begin summary\\b")
          (setq summary-depth (1+ summary-depth)))
         ((looking-at-p "\\s-*#\\+end summary\\b")
          (setq summary-depth (max 0 (1- summary-depth))))
         ((zerop summary-depth)
          (my/noema-roam--parse-meta-line
           note
           (string-trim (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))))
        (forward-line 1)))
    t))

(defun my/noema-roam--read-yaml-frontmatter (note)
  "Read simple YAML frontmatter at point into NOTE."
  (when (looking-at-p "\\s-*---\\s-*$")
    (forward-line 1)
    (while (and (not (eobp))
                (not (looking-at-p "\\s-*---\\s-*$")))
      (my/noema-roam--parse-meta-line
       note
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (forward-line 1))
    t))

(defun my/noema-roam--extract-summary-block ()
  "Return the first `#+begin summary' block text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin summary\\b.*$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^#\\+end summary\\b" nil t)
          (string-trim
           (buffer-substring-no-properties start (match-beginning 0))))))))

(defun my/noema-roam--internal-target-p (target)
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

(defun my/noema-roam--extract-links-from-buffer ()
  "Return Markdown roam references from the current buffer."
  (let (links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my/noema-roam--wiki-link-regexp nil t)
        (when-let* ((href (my/noema-roam--wiki-link-href (match-string 1))))
          (push href links)))
      (goto-char (point-min))
      (while (re-search-forward "\\(!?\\)\\[[^]\n]*\\](\\([^)\n]+\\))" nil t)
        (unless (equal (match-string 1) "!")
          (let ((href (string-trim (match-string 2))))
            (when (my/noema-roam--internal-target-p href)
              (push href links)))))
      (goto-char (point-min))
      (while (re-search-forward "\\_<roam://[^][<>()[:space:]]+" nil t)
        (push (match-string 0) links)))
    (delete-dups (nreverse links))))

(defun my/noema-roam--first-markdown-heading ()
  "Return the first Markdown heading text in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\{1,6\\}[ \t]+\\(.+?\\)\\(?:[ \t]+{#[[:alnum:]_:-]+}\\)?[ \t]*$" nil t)
      (string-trim
       (replace-regexp-in-string
        "[ \t]+{#[[:alnum:]_:-]+}[ \t]*\\'" ""
        (match-string 1))))))

(defun my/noema-roam--scan-note-file (file)
  "Return a note hash table by scanning Markdown FILE."
  (let* ((root (my/noema-roam-root))
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
        (my/noema-roam--read-org-meta-block note))
       ((looking-at-p "\\s-*---\\s-*$")
        (my/noema-roam--read-yaml-frontmatter note)))
      (my/noema-roam--put-note-field
       note "title"
       (or (my/noema-roam--note-field note "title")
           (my/noema-roam--first-markdown-heading)
           (file-name-base file)))
      (my/noema-roam--put-note-field
       note "id"
       (or (my/noema-roam--note-field note "id")
           (my/noema-roam--path-without-note-extension rel)))
      (my/noema-roam--put-note-field
       note "summary"
       (my/noema-roam--extract-summary-block))
      (my/noema-roam--put-note-field
       note "links"
       (append (my/noema-roam--note-list-field note "refs")
               (my/noema-roam--extract-links-from-buffer))))
    note))

(defun my/noema-roam--canonical-note-id (key note)
  "Return Noema's canonical note id for NOTE with DB KEY."
  (or (my/noema-roam--note-field note "id")
      (my/noema-roam--note-field note "key")
      (my/noema-roam--note-field note "source")
      (my/noema-roam--note-field note "path")
      (my/noema-roam--note-field note "link")
      (my/noema-roam--note-field note "file")
      key))

(defun my/noema-roam--note-file-from-fields (key note)
  "Return the best note file path for DB KEY and NOTE."
  (let* ((root (my/noema-roam-root))
         (raw-value (or (my/noema-roam--note-field note "file")
                        (my/noema-roam--note-field note "path")
                        (my/noema-roam--note-field note "link")
                        (my/noema-roam--note-field note "source")
                        key))
         (raw (if (and raw-value (file-name-absolute-p raw-value))
                  raw-value
                (my/noema-roam--strip-vault-prefix raw-value)))
         (path (and raw
                    (if (file-name-absolute-p raw)
                        raw
                      (expand-file-name raw root)))))
    (cond
     ((and path (file-exists-p path)) path)
     ((and path raw (not (my/noema-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".md")))
      (concat path ".md"))
     ((and path raw (not (my/noema-roam--ref-has-extension-p raw))
           (file-exists-p (concat path ".markdown")))
      (concat path ".markdown"))
     (path path))))

(defun my/noema-roam--note-search-values (key note)
  "Return Noema-style searchable values for NOTE with DB KEY."
  (let* ((file (my/noema-roam--note-field note "file"))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/noema-roam-root))))
         (values (append
                  (list key
                        (my/noema-roam--canonical-note-id key note)
                        (my/noema-roam--note-field note "id")
                        (my/noema-roam--note-field note "key")
                        (my/noema-roam--note-field note "title")
                        (my/noema-roam--note-field note "path")
                        (my/noema-roam--note-field note "link")
                        (my/noema-roam--note-field note "source")
                        file
                        rel-file
                        (and rel-file
                             (my/noema-roam--path-without-note-extension rel-file))
                        (and rel-file (concat "roam/" rel-file))
                        (and rel-file
                             (concat "roam/"
                                     (my/noema-roam--path-without-note-extension
                                      rel-file))))
                  (my/noema-roam--note-list-field note "aliases")
                  (my/noema-roam--note-list-field note "tags"))))
    (delete-dups
     (seq-filter (lambda (value)
                   (and (stringp value) (not (string-empty-p value))))
                 values))))

(defun my/noema-roam--scan-record (file)
  "Return the note record for FILE by reading it from disk."
  (let* ((note (my/noema-roam--scan-note-file file))
         (key (my/noema-roam--file-to-slug file))
         (id (my/noema-roam--canonical-note-id key note)))
    (list :key key
          :id id
          :note note
          :file file
          :title (or (my/noema-roam--note-field note "title") id)
          :values (my/noema-roam--note-search-values key note))))

(defun my/noema-roam--scanned-note-records ()
  "Return cached note records by scanning Markdown files."
  (or my/noema-roam--scan-cache
      (setq my/noema-roam--scan-cache
            (mapcar #'my/noema-roam--scan-record (my/noema-roam--all-files)))))

(defun my/noema-roam--runtime-note-records ()
  "Return note records from the vendored Noema runtime."
  (when-let* ((payload (my/noema-roam--runtime-index))
              (notes (gethash "notes" payload)))
    (mapcar (lambda (note)
              (let* ((key (or (my/noema-roam--note-field note "key")
                              (my/noema-roam--note-field note "id")
                              (my/noema-roam--note-field note "path")
                              (my/noema-roam--note-field note "link")))
                     (id (my/noema-roam--canonical-note-id key note)))
                (list :key key
                      :id id
                      :note note
                      :file (my/noema-roam--note-file-from-fields key note)
                      :title (or (my/noema-roam--note-field note "title") id)
                      :values (my/noema-roam--note-search-values key note))))
            notes)))

(defun my/noema-roam--note-records ()
  "Return note records with :key, :id, :note, :file, :title, and :values."
  (or (my/noema-roam--runtime-note-records)
      (my/noema-roam--scanned-note-records)))

(defun my/noema-roam--target-note-ref (target)
  "Return the note ref portion of TARGET."
  (plist-get (my/noema-roam--split-target target) :ref))

(defun my/noema-roam--resolve-note (ref)
  "Resolve REF to an Noema note record plist, or nil.
Only exact matches against a note's searchable values (id, key, path, link,
title, aliases, tags) resolve.

There used to be an unanchored substring fallback here.  Link resolution has
to be deterministic — it backs `follow-link', the xref backend and
`--file-to-note-id' — and a substring pass makes it depend on scan order:
a ref like \"index\" matched the first note whose *path* merely contained
that word, silently opening the wrong note instead of reporting a broken
link.  Callers already treat nil as \"no such note\" and offer to create it,
which is the honest answer for a dangling ref."
  (let* ((clean (or (my/noema-roam--target-note-ref ref) ref))
         (clean (string-trim (or clean "")))
         (query (downcase clean)))
    (unless (string-empty-p query)
      (seq-find
       (lambda (record)
         (member query
                 (mapcar #'downcase (plist-get record :values))))
       (my/noema-roam--note-records)))))

(defun my/noema-roam--db-note (slug)
  "Return the canonical runtime note for SLUG/id/path, or nil."
  (plist-get (my/noema-roam--resolve-note slug) :note))

(defun my/noema-roam--target-slug (target)
  "Return normalized canonical note id from a note-link TARGET."
  (plist-get (my/noema-roam--parse-target target) :slug))

(defun my/noema-roam--db-backlinks-to (slug)
  "Return canonical Wiki backlinks to SLUG/id."
  (when-let* ((target-id (or (plist-get (my/noema-roam--resolve-note slug) :id)
                             slug)))
    (or (when-let* ((note (my/noema-roam--db-note target-id)))
          (my/noema-roam--note-list-field note "backlinks"))
        (let (backlinks)
          (dolist (record (my/noema-roam--note-records))
            (let* ((note (plist-get record :note))
                   (source (plist-get record :key))
                   (links (or (my/noema-roam--note-list-field note "links")
                              (my/noema-roam--note-list-field note "refs"))))
              (when (member target-id
                            (mapcar #'my/noema-roam--target-slug links))
                (push (my/noema-roam--canonical-note-id source note) backlinks))))
          (delete-dups (nreverse backlinks))))))

(defun my/noema-roam--current-slug ()
  "Return the canonical roam id for the current buffer, or nil."
  (when buffer-file-name
    (my/noema-roam--file-to-note-id buffer-file-name)))

;; ── Tag ids and TOC ───────────────────────────────────────────────────────────

(defun my/noema-roam--slugify-tag-id (text)
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

(defun my/noema-roam--tag-id-exists-p (id)
  "Return non-nil when ID already exists in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward (format "{#%s}" (regexp-quote id)) nil t)
        (progn
          (goto-char (point-min))
          (re-search-forward (format "<%s>" (regexp-quote id)) nil t)))))

(defun my/noema-roam--unique-tag-id (base)
  "Return BASE or BASE-N so it is unique in the current buffer."
  (let ((candidate base)
        (n 2))
    (while (my/noema-roam--tag-id-exists-p candidate)
      (setq candidate (format "%s-%d" base n)
            n (1+ n)))
    candidate))

(defun my/noema-roam-generate-tag-id (&optional text)
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
         (id (my/noema-roam--unique-tag-id
              (my/noema-roam--slugify-tag-id source))))
    (when (called-interactively-p 'interactive)
      (kill-new id)
      (message "Tag id copied: %s" id))
    id))

(defun my/noema-roam-insert-tag-id (&optional id)
  "Insert or append Markdown heading ID at point.
On a heading line, append `{#id}` unless an id already exists."
  (interactive)
  (let ((id (or id
                (read-string "Tag id: "
                             (my/noema-roam-generate-tag-id)))))
    (save-excursion
      (beginning-of-line)
      (if (looking-at
          "^[ \t]*#\\{1,6\\}[ \t]+.+?\\(?:[ \t]+{#\\([[:alnum:]_:-]+\\)}\\)?[ \t]*$")
          (if (match-string 1)
              (user-error "Heading already has id: %s" (match-string 1))
            (end-of-line)
            (insert (format " {#%s}" id)))
        (insert (format "{#%s}" id))))))

(defun my/noema-roam-insert-toc-link ()
  "Open the interactive selector and insert a DOM/TOC note-link."
  (interactive)
  (my/noema-roam-select-link 'toc))

(defun my/noema-roam-insert-tag-id-link ()
  "Open the interactive selector and insert a tag-id note-link."
  (interactive)
  (my/noema-roam-select-link 'tag))

;; ── Wiki index commands ──────────────────────────────────────────────────────

(defun my/noema-roam-update-db (&optional full)
  "Refresh canonical wiki.db; with FULL, rebuild it atomically."
  (interactive "P")
  (my/noema-roam--clear-runtime-cache)
  (unless (fboundp 'my/noema-wiki-refresh)
    (require 'init-aaronnote))
  (my/noema-wiki-refresh full))

(defun my/noema-roam--summary-entry-for-slug (slug &optional summaries)
  "Return a note summary entry for SLUG from optional SUMMARIES."
  (or (seq-find (lambda (entry)
                  (equal (plist-get entry :slug) slug))
                (or summaries (my/noema-roam--all-note-summaries)))
      (list :slug slug
            :title (my/noema-roam--note-title slug)
            :tags (my/noema-roam--note-tags slug)
            :summary (my/noema-roam--note-summary slug))))

(defun my/noema-roam-backlinks (&optional target-slug)
  "Show backlinks for the current note in a dedicated buffer."
  (interactive)
  (let* ((slug (or target-slug (my/noema-roam--current-slug)))
         (note (and slug (my/noema-roam--db-note slug)))
         (bls  (or (and slug (my/noema-roam--db-backlinks-to slug))
                   (and note (gethash "backlinks" note))))
         (summaries (my/noema-roam--all-note-summaries))
         (refresh (let ((target slug))
                    (lambda () (my/noema-roam-backlinks target))))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-backlinks*" "Roam Backlinks" 'backlink refresh
               (format "%d backlinks" (length bls)))))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Backlinks"
          :icon 'backlink
          :subtitle (format "References to %s"
                            (or (and note (gethash "title" note)) slug))
          :stats (list (cons (format "%d backlinks" (length bls)) 'info))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Referenced by" (length bls))
         (if (null bls)
             (my/noema-roam-ui-insert-empty
              "No notes currently link to this note.")
           (dolist (bl bls)
             (my/noema-roam--insert-note-button
              (my/noema-roam--summary-entry-for-slug bl summaries)))))))
    (display-buffer buf)))

(defun my/noema-roam-tags ()
  "Browse notes by tag with completion."
  (interactive)
  (let ((tags-ht (make-hash-table :test 'equal)))
    (dolist (record (my/noema-roam--note-records))
      (dolist (tag (my/noema-roam--note-list-field
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
      (my/noema-roam--open-slug slug))))

(defun my/noema-roam--scan-todos ()
  "Return todo hash tables scanned from Markdown notes."
  (let (todos)
    (dolist (record (my/noema-roam--note-records))
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

(defun my/noema-roam--todos ()
  "Return vault-wide todos from the Noema runtime or local scan.
Fetches through the `agenda' view-model rather than the plain `todos' list so
dependency resolution (`effectiveStatus'/`blockedBy', computed vault-wide) and
the urgency sort are already applied server-side instead of being re-derived
in Elisp."
  (let* ((runtime (my/noema-roam--runtime-call "agenda" "--json" "{}"))
         (runtime-todos (and runtime (gethash "todos" runtime))))
    (or runtime-todos
        (my/noema-roam--scan-todos))))

(defun my/noema-roam--todo-field (entry &rest keys)
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

(defun my/noema-roam--todo-status (entry)
  "Return normalized status string for todo ENTRY.
Prefers the server-computed `effectiveStatus' (dependency-aware: a todo with
an open `after' reference is reported as blocked without any local file
being rewritten) over the raw `status' field when present."
  (let ((status (downcase
                 (string-trim
                  (format "%s" (or (my/noema-roam--todo-field entry "effectiveStatus")
                                   (my/noema-roam--todo-field entry "status")
                                   "todo"))))))
    (cond
     ((member status '("" " " "open" "unchecked")) "todo")
     ((member status '("~" "-" "wip" "active" "in-progress")) "doing")
     ((member status '("x" "checked" "complete" "completed")) "done")
     ((member status '("!" "block")) "blocked")
     ((member status '("cancel" "canceled" "cancelled")) "cancelled")
     (t status))))

(defun my/noema-roam--todo-tone (entry)
  "Return display tone for todo ENTRY."
  (pcase (my/noema-roam--todo-status entry)
    ((or "done" "complete" "completed") 'success)
    ((or "blocked" "cancelled" "canceled") 'danger)
    ((or "doing" "waiting" "in-progress") 'warning)
    (_ 'info)))

(defun my/noema-roam--visit-todo (entry)
  "Open the note and source line represented by todo ENTRY."
  (let* ((file (my/noema-roam--todo-field entry "file"))
         (note-slug (my/noema-roam--todo-field
                     entry "note" "noteId" "noteKey" "path"))
         (line (my/noema-roam--todo-field entry "line"))
         (column (my/noema-roam--todo-field entry "column"))
         (index (my/noema-roam--todo-field entry "index"))
         (source (my/noema-roam--todo-field entry "source")))
    (cond
     ((and (stringp file) (not (string-empty-p file)) (file-exists-p file))
      (find-file file))
     (note-slug
      (my/noema-roam--open-slug note-slug))
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

(defun my/noema-roam--todo-at-point ()
  "Return the todo entry on the current row."
  (or (get-text-property (point) 'my/noema-roam-todo)
      (get-text-property (line-beginning-position) 'my/noema-roam-todo)
      (get-text-property (max (point-min) (1- (point)))
                         'my/noema-roam-todo)))

(defun my/noema-roam--todo-update-local (entry status)
  "Update todo ENTRY to STATUS by editing its source file locally."
  (let* ((file (my/noema-roam--todo-field entry "file"))
         (index (my/noema-roam--todo-field entry "index"))
         (source (my/noema-roam--todo-field entry "source"))
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
  (my/noema-roam--clear-runtime-cache))

(defun my/noema-roam--todo-patch (entry extra)
  "Send a patch-todo request for todo ENTRY merging EXTRA alist fields.
EXTRA keys are canonical (ddl/sche/prio/repeat/warn/after/afterAdd) or their
legacy aliases (priority/due/scheduled); `patchTodo' on the server accepts
both and preserves whichever alias the `@@todo' line already uses.  Returns
the parsed response hash-table, or nil when the runtime is unavailable."
  (let* ((file (my/noema-roam--todo-field entry "file"))
         (index (my/noema-roam--todo-field entry "index"))
         (source (my/noema-roam--todo-field entry "source"))
         (id (my/noema-roam--todo-field entry "id"))
         (text (my/noema-roam--todo-field entry "text"))
         (payload (append
                   (list (cons 'file (or file ""))
                         (cons 'id (or id ""))
                         (cons 'index (or index ""))
                         (cons 'source (or source ""))
                         (cons 'text (or text "")))
                   extra)))
    (unless (and file (not (string-empty-p file)))
      (user-error "Todo has no editable source file"))
    (my/noema-roam--runtime-call "patch-todo" "--json" (json-encode payload))))

(defun my/noema-roam-update-todo-status (status &optional entry)
  "Set current todo ENTRY to STATUS and refresh the current task view.
Setting STATUS to \"done\" runs the repeater engine server-side: a todo with
a `repeat' arg rolls its deadline/scheduled dates forward and resets to
`todo' instead of closing, mirroring org's repeating-task completion."
  (interactive
   (list (completing-read "Todo status: " '("todo" "doing" "blocked" "done" "cancelled")
                          nil t nil nil "done")
         nil))
  (let* ((entry (or entry (my/noema-roam--todo-at-point))))
    (unless entry
      (user-error "No todo on this line"))
    (or (if (string= status "done")
            (my/noema-roam--todo-patch entry '((op . "complete")))
          (my/noema-roam--todo-patch entry `((status . ,status))))
        (my/noema-roam--todo-update-local entry status))
    (my/noema-roam--clear-runtime-cache)
    (message "Todo marked %s" status)
    (my/noema-roam-ui-refresh)))

(defun my/noema-roam-update-todo-metadata (field value &optional entry)
  "Set todo metadata FIELD to VALUE for ENTRY and refresh the current task view.
FIELD is one of priority, due, scheduled, repeat, or warn.  Empty VALUE
clears FIELD."
  (interactive
   (let* ((field (completing-read "Todo field: "
                                  '("priority" "due" "scheduled" "repeat" "warn")
                                  nil t))
          (prompt (format "%s%s: "
                          (if (string-empty-p field) "Value" (capitalize field))
                          (if (member field '("due" "scheduled" "warn")) " (empty clears)" "")))
          (value (read-string prompt)))
     (list field value nil)))
  (let* ((entry (or entry (my/noema-roam--todo-at-point)))
         (field (downcase (format "%s" field)))
         (value (string-trim (format "%s" value))))
    (unless entry
      (user-error "No todo on this line"))
    (unless (member field '("priority" "due" "scheduled" "repeat" "warn"))
      (user-error "Unsupported todo metadata field: %s" field))
    (unless (my/noema-roam--todo-patch entry (list (cons (intern field) value)))
      (user-error "Noema runtime is required for todo metadata updates"))
    (my/noema-roam--clear-runtime-cache)
    (message "Todo %s %s" field (if (string-empty-p value) "cleared" value))
    (my/noema-roam-ui-refresh)))

(defun my/noema-roam-set-todo-priority (&optional priority entry)
  "Set current todo PRIORITY and refresh the current task view."
  (interactive
   (list (completing-read "Priority (empty clears): "
                          '("A" "B" "C" "D" "E" "F" "") nil t)
         nil))
  (my/noema-roam-update-todo-metadata "priority" (or priority "") entry))

(defun my/noema-roam-set-todo-due (&optional due entry)
  "Set current todo due date and refresh the current task view."
  (interactive (list (read-string "Due (empty clears): ") nil))
  (my/noema-roam-update-todo-metadata "due" (or due "") entry))

(defun my/noema-roam-set-todo-scheduled (&optional scheduled entry)
  "Set current todo scheduled date and refresh the current task view."
  (interactive (list (read-string "Scheduled (empty clears): ") nil))
  (my/noema-roam-update-todo-metadata "scheduled" (or scheduled "") entry))

(defun my/noema-roam-set-todo-repeat (&optional repeat entry)
  "Set current todo repeat metadata and refresh the current task view."
  (interactive (list (read-string "Repeat (+1w / ++1w / .+3d; empty clears): ") nil))
  (my/noema-roam-update-todo-metadata "repeat" (or repeat "") entry))

(defun my/noema-roam-set-todo-warn (&optional warn entry)
  "Set current todo's deadline warning lead time and refresh the task view."
  (interactive (list (read-string "Warn lead (e.g. 3d; empty clears): ") nil))
  (my/noema-roam-update-todo-metadata "warn" (or warn "") entry))

(defun my/noema-roam-add-todo-dependency (&optional entry)
  "Add a dependency (`after') reference from ENTRY to another todo.
Prompts for the target todo by note and text, resolves it through the
Noema runtime into a stable, shortest-unique text reference, and appends
it to ENTRY's `after' arg — no ids are ever written to the source file, so
the reference stays a plain, human-readable part of the Markdown."
  (interactive)
  (let* ((entry (or entry (my/noema-roam--todo-at-point))))
    (unless entry
      (user-error "No todo on this line"))
    (let* ((self-id (my/noema-roam--todo-field entry "id"))
           (candidates
            (delq nil
                  (mapcar
                   (lambda (todo)
                     (unless (equal (my/noema-roam--todo-field todo "id") self-id)
                       (cons (format "[%s] %s"
                                     (or (my/noema-roam--todo-field todo "noteTitle" "title") "?")
                                     (or (my/noema-roam--todo-field todo "text") ""))
                             todo)))
                   (my/noema-roam--todos))))
           (choice (completing-read "Depends on: " candidates nil t))
           (target (cdr (assoc choice candidates)))
           (target-id (and target (my/noema-roam--todo-field target "id"))))
      (unless target
        (user-error "No matching todo"))
      (let* ((ref-response
              (my/noema-roam--runtime-call
               "todo-dep-ref" "--json"
               (json-encode `((targetId . ,target-id)
                              (sourceId . ,(or self-id ""))))))
             (ref (and ref-response (gethash "ref" ref-response))))
        (unless ref
          (user-error "Could not build a dependency reference"))
        (unless (my/noema-roam--todo-patch entry (list (cons 'afterAdd ref)))
          (user-error "Noema runtime is required for dependency updates"))
        (my/noema-roam--clear-runtime-cache)
        (message "Depends on: %s" ref)
        (my/noema-roam-ui-refresh)))))

(defun my/noema-roam-todo-done ()
  "Mark the current roam todo done."
  (interactive)
  (my/noema-roam-update-todo-status "done"))

(defun my/noema-roam--insert-todo-row (entry &optional deadline-tone)
  "Insert a compact task row for ENTRY using optional DEADLINE-TONE."
  (let* ((note-slug (my/noema-roam--todo-field
                     entry "note" "noteId" "noteKey" "path"))
         (note-title (or (my/noema-roam--todo-field
                          entry "title" "noteTitle")
                         note-slug
                         "Unknown note"))
         (text (or (my/noema-roam--todo-field
                    entry "text" "context" "source")
                   "(empty todo)"))
         (line (my/noema-roam--todo-field entry "line"))
         (ddl (my/noema-roam--todo-ddl entry))
         (status (my/noema-roam--todo-status entry))
         (tags (my/noema-roam--todo-tags entry))
         (meta (string-join
                (delq nil
                      (list (and ddl (format "DDL %s" ddl))
                            (and (integerp line) (format "line %d" line))))
                "  ·  ")))
    (my/noema-roam-ui-insert-row
     :id (list note-slug line text)
     :icon 'todo
     :badge (upcase status)
     :badge-tone (or deadline-tone (my/noema-roam--todo-tone entry))
     :title text
     :meta meta
     :detail note-title
     :tags tags
     :action (let ((todo entry))
               (lambda (_button)
                 (my/noema-roam--visit-todo todo)))
     :properties `(my/noema-roam-todo ,entry))))

(defun my/noema-roam-todos ()
  "List all vault todos in a *roam-todos* buffer."
  (interactive)
  (let* ((todos (my/noema-roam--todos))
         (active (seq-count
                  (lambda (entry)
                    (not (member (my/noema-roam--todo-status entry)
                                 '("done" "complete" "completed"
                                   "cancelled" "canceled"))))
                  todos))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-todos*" "Roam Tasks" 'todo
               #'my/noema-roam-todos
               (format "%d tasks" (length todos)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Tasks"
          :icon 'todo
          :subtitle "All indexed Noema Markdown tasks"
          :stats (list (cons (format "%d active" active) 'warning)
                       (cons (format "%d total" (length todos)) 'info))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "All tasks" (length todos))
         (if (null todos)
             (my/noema-roam-ui-insert-empty "No indexed tasks.")
           (dolist (entry todos)
             (my/noema-roam--insert-todo-row entry))))))
    (display-buffer buf)))

;; These historical entry points are kept only for existing keymaps.  Their
;; native duplicate dashboards are retired in favor of the canonical Wiki UI.




;; ── Noema-style note tools ────────────────────────────────────────────────

(defun my/noema-roam--insert-note-button (entry &optional prefix)
  "Insert a clickable note button for summary ENTRY with PREFIX."
  (let* ((slug (plist-get entry :slug))
         (title (or (plist-get entry :title) slug))
         (path (or (plist-get entry :path) slug))
         (tags (plist-get entry :tags))
         (summary (plist-get entry :summary))
         (indent (/ (length (or prefix "")) 2)))
    (my/noema-roam-ui-insert-row
     :id slug
     :icon 'note
     :title title
     :meta path
     :detail summary
     :tags tags
     :indent indent
     :action (let ((target slug))
               (lambda (_button)
                 (my/noema-roam--open-slug target))))))

(defun my/noema-roam--show-note-list
    (title entries &optional empty-text refresh-function icon)
  "Show TITLE and note ENTRIES in a special buffer."
  (let* ((refresh
          (or refresh-function
              (let ((page-title title)
                    (page-entries entries)
                    (page-empty empty-text)
                    (page-icon icon))
                (lambda ()
                  (my/noema-roam--show-note-list
                   page-title page-entries page-empty nil page-icon)))))
         (buf (my/noema-roam--prepare-ui-buffer
               "*Noema roam notes*" title (or icon 'note) refresh
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          title
          :icon (or icon 'note)
          :subtitle "Noema Markdown roam notes"
          :stats (list (cons (format "%d notes" (length entries)) 'info))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Notes" (length entries))
         (if (null entries)
             (my/noema-roam-ui-insert-empty
              (or empty-text "No notes."))
           (dolist (entry entries)
             (my/noema-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/noema-roam--show-search-results (query entries)
  "Show note search QUERY and ENTRIES with a live refresh action."
  (my/noema-roam--show-note-list
   (format "Markdown roam search: %s" query)
   entries
   "No matching notes."
   (let ((search-query query))
     (lambda ()
       (my/noema-roam--show-search-results
        search-query
        (my/noema-roam-search-notes search-query))))
   'search))

(defun my/noema-roam--search-parse-term (term)
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

(defun my/noema-roam--search-match-p (entry parsed-term)
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
                       (downcase (my/noema-roam--candidate-haystack entry)))))))

(defun my/noema-roam-search-notes (&optional query)
  "Search notes with optional scoped operators.
Operators: intitle:TEXT, incategory:TAG, tag:TAG, linksto:SLUG, plain text.
Multiple terms are ANDed."
  (interactive)
  (let* ((query (or query (read-string "Search notes (intitle: incategory: linksto:): ")))
         (raw-parts (split-string (downcase (string-trim query)) "\\s-+" t))
         (parsed (mapcar #'my/noema-roam--search-parse-term raw-parts))
         (entries (seq-filter
                   (lambda (entry)
                     (seq-every-p
                      (lambda (term) (my/noema-roam--search-match-p entry term))
                      parsed))
                   (my/noema-roam--all-note-summaries))))
    (if (called-interactively-p 'interactive)
        (if (= (length entries) 1)
            (my/noema-roam--open-slug (plist-get (car entries) :slug))
          (my/noema-roam--show-search-results query entries))
      entries)))

(defun my/noema-roam-recent-notes ()
  "Show recently opened roam notes."
  (interactive)
  (my/noema-roam--show-note-list
   "Recent Markdown roam notes"
   (seq-filter
    #'identity
    (mapcar (lambda (slug)
              (seq-find (lambda (entry)
                          (equal (plist-get entry :slug) slug))
                        (my/noema-roam--all-note-summaries)))
            (seq-filter (lambda (slug)
                          (file-exists-p (my/noema-roam--slug-to-file slug)))
                        my/noema-roam--recent)))
   "No recent notes."
   #'my/noema-roam-recent-notes
   'note))

(defun my/noema-roam-related-notes (&optional target-slug)
  "Show outgoing links and backlinks for the current note."
  (interactive)
  (let* ((slug (or target-slug (my/noema-roam--current-slug)))
         (links (and slug (my/noema-roam--note-links slug)))
         (backlinks (and slug (my/noema-roam--db-backlinks-to slug)))
         (summaries (my/noema-roam--all-note-summaries))
         (by-slug (lambda (target)
                    (seq-find (lambda (entry)
                                (equal (plist-get entry :slug) target))
                              summaries)))
         (refresh (let ((target slug))
                    (lambda () (my/noema-roam-related-notes target))))
         (buf (my/noema-roam--prepare-ui-buffer
               "*Noema roam related*" "Related Notes" 'related refresh
               (format "%d links · %d backlinks"
                       (length links) (length backlinks)))))
    (unless slug (user-error "Not in a roam note"))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Related notes"
          :icon 'related
          :subtitle (my/noema-roam--note-title slug)
          :stats (list (cons (format "%d outgoing" (length links)) 'info)
                       (cons (format "%d backlinks" (length backlinks)) 'muted))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Outgoing links" (length links))
         (if links
             (dolist (target links)
               (when-let* ((entry (or (funcall by-slug target)
                                      (my/noema-roam--summary-entry-for-slug
                                       target summaries))))
                 (my/noema-roam--insert-note-button entry)))
           (my/noema-roam-ui-insert-empty "No outgoing note links."))
         (insert "\n")
         (my/noema-roam-ui-insert-section "Backlinks" (length backlinks))
         (if backlinks
             (dolist (target backlinks)
               (when-let* ((entry (or (funcall by-slug target)
                                      (my/noema-roam--summary-entry-for-slug
                                       target summaries))))
                 (my/noema-roam--insert-note-button entry)))
           (my/noema-roam-ui-insert-empty "No backlinks.")))))
    (display-buffer buf)))



;; ── Roam agenda ─────────────────────────────────────────────────────────────

(defconst my/noema-roam--agenda-date-fields
  '("ddl" "deadline" "due" "scheduled" "start" "when" "date")
  "Todo fields considered date-like in the agenda.")

(defun my/noema-roam--todo-value (entry &rest keys)
  "Return the first non-empty todo ENTRY value for KEYS.
This checks top-level todo fields first, then the nested args object."
  (or (seq-some
       (lambda (key)
         (let ((value (my/noema-roam--todo-field entry key)))
           (and value
                (not (and (stringp value) (string-empty-p value)))
                value)))
       keys)
      (when-let* ((args (my/noema-roam--todo-field entry "args")))
        (seq-some
         (lambda (key)
           (let ((value (my/noema-roam--todo-field args key)))
             (and value
                  (not (and (stringp value) (string-empty-p value)))
                  value)))
         keys))))

(defun my/noema-roam--todo-string-value (entry &rest keys)
  "Return a trimmed string todo ENTRY value for KEYS, or nil."
  (when-let* ((value (apply #'my/noema-roam--todo-value entry keys))
              (string (string-trim (format "%s" value))))
    (unless (string-empty-p string)
      string)))

(defun my/noema-roam--todo-list-value (entry &rest keys)
  "Return a string list todo ENTRY value for KEYS."
  (let ((value (apply #'my/noema-roam--todo-value entry keys)))
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

(defun my/noema-roam--todo-tags (entry)
  "Return file and inline tags inherited by todo ENTRY."
  (delete-dups
   (seq-filter
    (lambda (tag) (not (string-empty-p tag)))
    (append (my/noema-roam--todo-list-value entry "tags")
            (my/noema-roam--todo-list-value entry "inlineTags")))))

(defun my/noema-roam--todo-canon (entry key)
  "Return canonical arg KEY from todo ENTRY's `canon' object, or nil.
`canon' is attached server-side (see `canonicalTodoArgs' in runtime.mjs) and
already resolves every read alias (due/deadline -> ddl, priority -> prio,
scheduled/start -> sche, ...), so callers no longer need to enumerate aliases
themselves."
  (when-let* ((canon (my/noema-roam--todo-field entry "canon"))
              (value (my/noema-roam--todo-field canon key))
              (s (string-trim (format "%s" value))))
    (unless (string-empty-p s) s)))

(defun my/noema-roam--todo-ddl (entry)
  "Return deadline string for todo ENTRY, or nil."
  (or (my/noema-roam--todo-canon entry "ddl")
      (my/noema-roam--todo-string-value entry "ddl" "deadline" "due")))

(defun my/noema-roam--todo-scheduled (entry)
  "Return scheduled date string for todo ENTRY, or nil."
  (or (my/noema-roam--todo-canon entry "sche")
      (my/noema-roam--todo-string-value entry "scheduled" "start" "when")))

(defun my/noema-roam--date-day-string (value)
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

(defun my/noema-roam--todo-agenda-date (entry)
  "Return the main agenda date string for todo ENTRY, or nil."
  (or (my/noema-roam--date-day-string
       (my/noema-roam--todo-ddl entry))
      (my/noema-roam--date-day-string
       (my/noema-roam--todo-scheduled entry))
      (seq-some
       (lambda (key)
         (my/noema-roam--date-day-string
          (my/noema-roam--todo-value entry key)))
       my/noema-roam--agenda-date-fields)))

(defun my/noema-roam-agenda-search (&optional query)
  "Search the roam agenda with QUERY."
  (interactive (list (read-string
                      "Agenda search (status: tag: title: roamid: file: parent: date: from: to:): ")))
  (my/noema-roam-agenda 'search query))

(defun my/noema-roam--current-buffer-todos ()
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
                                             file (my/noema-roam-root)))
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

(defun my/noema-roam--current-file-todos ()
  "Return todo entries for the current file."
  (let* ((file (buffer-file-name))
         (truename (and file (file-truename file)))
         (indexed
          (and truename
               (seq-filter
                (lambda (entry)
                  (let ((todo-file (my/noema-roam--todo-field entry "file")))
                    (and (stringp todo-file)
                         (file-exists-p todo-file)
                         (string= (file-truename todo-file) truename))))
                (or (my/noema-roam--todos) '())))))
    (or indexed
        (and file (my/noema-roam--current-buffer-todos)))))

(defun my/noema-roam-jump-file-todo ()
  "Quickly jump to a todo in the current Markdown roam file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((todos (my/noema-roam--current-file-todos))
         (choices
          (mapcar
           (lambda (entry)
             (let* ((line (or (my/noema-roam--todo-field entry "line") 0))
                    (status (upcase (my/noema-roam--todo-status entry)))
                    (date (my/noema-roam--todo-agenda-date entry))
                    (text (or (my/noema-roam--todo-string-value
                               entry "text" "source" "context")
                              "(empty todo)"))
                    (label (format "%5s  L%-4s  %s%s"
                                   status line text
                                   (if date (format "  <%s>" date) ""))))
               (cons label entry)))
           todos)))
    (unless choices
      (user-error "No todos in current file"))
    (my/noema-roam--visit-todo
     (cdr (assoc (completing-read "File todo: " choices nil t) choices)))))

(defun my/noema-roam--open-web-agenda (&optional view query)
  "Open Noema Web agenda special page with VIEW and optional QUERY."
  (unless (and (fboundp 'my/noema--ensure-server)
               (fboundp 'my/noema--server-url)
               (fboundp 'my/noema--open-url))
    (require 'init-aaronnote))
  (let* ((view-name (pcase view
                      ((or 'calendar 'month) "calendar")
                      ('log "log")
                      ('projects "projects")
                      ('gantt "gantt")
                      ((or 'clock 'clocktable) "clocktable")
                      ('lints "lints")
                      (_ "agenda")))
         (query-string (and query (format "%s" query)))
         (target-window (selected-window)))
    (my/noema--ensure-server
     (lambda ()
       (when (window-live-p target-window)
         (select-window target-window))
       (my/noema--open-url
        (concat (my/noema--server-url "/agenda")
                "?view=" (url-hexify-string view-name)
                (if (and query-string (not (string-empty-p query-string)))
                    (concat "&q=" (url-hexify-string query-string))
                  ""))
        nil
        t)))))

(defun my/noema-roam-agenda (&optional mode query)
  "Open the Noema Web agenda special page.
The native Emacs agenda renderer is no longer the default project-management
surface; Noema Web owns agenda/project/Gantt management."
  (interactive)
  (my/noema-roam--open-web-agenda
   (pcase mode
     ((or 'calendar 'month) 'calendar)
     ('log 'log)
     ('gantt 'gantt)
     ('projects 'projects)
     ('clock 'clocktable)
     ('clocktable 'clocktable)
     ('lints 'lints)
     (_ 'agenda))
   query))

(defun my/noema-roam-agenda-calendar ()
  "Show the agenda month calendar."
  (interactive)
  (my/noema-roam--open-web-agenda 'calendar))

(defun my/noema-roam-agenda-log ()
  "Show the agenda completion log."
  (interactive)
  (my/noema-roam--open-web-agenda 'log))

(defun my/noema-roam-agenda-gantt ()
  "Show the agenda Gantt view."
  (interactive)
  (my/noema-roam--open-web-agenda 'gantt))

(defun my/noema-roam-agenda-projects ()
  "Show the agenda project rollup view."
  (interactive)
  (my/noema-roam--open-web-agenda 'projects))

(defun my/noema-roam-agenda-clock ()
  "Show the agenda clocktable view."
  (interactive)
  (my/noema-roam--open-web-agenda 'clocktable))

(defun my/noema-roam-agenda-lints ()
  "Show agenda lints."
  (interactive)
  (my/noema-roam--open-web-agenda 'lints))

;; ── Roam activity heatmap ────────────────────────────────────────────────────

(defconst my/noema-roam--activity-heatmap-days 70
  "Number of recent days shown in roam activity heatmaps.")

(defun my/noema-roam--activity-date-counts (&optional days)
  "Return an ordered alist of recent note activity counts for DAYS."
  (let* ((days (or days my/noema-roam--activity-heatmap-days))
         (start (time-subtract (current-time) (days-to-time (1- days))))
         (counts (make-hash-table :test 'equal))
         ordered)
    (dotimes (offset days)
      (let ((day (format-time-string
                  "%Y-%m-%d" (time-add start (days-to-time offset)))))
        (push (cons day 0) ordered)
        (puthash day 0 counts)))
    (dolist (record (delete-dups (my/noema-roam--note-records)))
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

(defun my/noema-roam--activity-heatmap-tone (count)
  "Return a display tone for activity COUNT."
  (cond
   ((>= count 5) 'success)
   ((>= count 2) 'warning)
   ((>= count 1) 'info)
   (t 'muted)))

(defun my/noema-roam--activity-heatmap-cell (count)
  "Return a fixed-width heatmap cell label for COUNT."
  (format " %2s "
          (cond
           ((<= count 0) "")
           ((> count 99) "99")
           (t (format "%d" count)))))

(defun my/noema-roam--activity-heatmap-rows (counts)
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
                (cons (my/noema-roam--activity-heatmap-cell count)
                      (my/noema-roam--activity-heatmap-tone count))
                unless (= week (1- weeks))
                collect " "))
              rows))
    (nreverse rows)))

(defun my/noema-roam--activity-heatmap-row-width (row)
  "Return display width for heatmap ROW."
  (apply #'+
         (mapcar
          (lambda (cell)
            (string-width (if (consp cell) (car cell) cell)))
          row)))

(defun my/noema-roam--pixel-width-to-columns (pixels)
  "Return the display-column width represented by PIXELS."
  (ceiling (/ (float pixels) (max 1 (frame-char-width)))))

(defun my/noema-roam--region-align-width (start end fallback-width)
  "Return rendered width from START to END for `:align-to'.
Use FALLBACK-WIDTH when pixel measurement is unavailable."
  (or (and (fboundp 'string-pixel-width)
           (let ((pixel-width
                  (string-pixel-width (buffer-substring start end))))
             (and (> pixel-width 0)
                  (my/noema-roam--pixel-width-to-columns pixel-width))))
      fallback-width))

(defun my/noema-roam--center-inserted-region (start end width)
  "Center text from START to END using display WIDTH."
  (let* ((align-width (my/noema-roam--region-align-width start end width))
         (prefix (propertize
                  " "
                  'display
                  `(space . (:align-to (- center ,(/ (float align-width) 2)))))))
    (add-text-properties start end
                         `(line-prefix ,prefix indent-prefix ,prefix))))

(defun my/noema-roam--insert-centered-heatmap-row (row &optional face-fn)
  "Insert heatmap ROW centered.  FACE-FN maps a tone to a face."
  (let ((start (point))
        (row-width (my/noema-roam--activity-heatmap-row-width row)))
    (dolist (cell row)
      (if (consp cell)
          (insert (propertize (car cell)
                              'face (if face-fn
                                        (funcall face-fn (cdr cell))
                                      'default)))
        (insert cell)))
    (my/noema-roam--center-inserted-region start (point) row-width)
    (insert "\n")))

(defun my/noema-roam--insert-centered-line (text &optional face)
  "Insert TEXT centered in the selected window with optional FACE."
  (let ((start (point)))
    (insert (if face (propertize text 'face face) text))
    (my/noema-roam--center-inserted-region
     start (point) (string-width text))
    (insert "\n")))

(defun my/noema-roam-ui-insert-activity-heatmap (&optional days)
  "Insert a board-style roam activity heatmap for recent DAYS."
  (let* ((counts (my/noema-roam--activity-date-counts days))
         (total (apply #'+ (mapcar #'cdr counts))))
    (my/noema-roam-ui-insert-section
     (format "Roam activity · last %d days" (length counts))
     total
     (if (> total 0) 'success 'muted))
    (dolist (row (my/noema-roam--activity-heatmap-rows counts))
      (my/noema-roam--insert-centered-heatmap-row
       row
       #'my/noema-roam-ui--tone-face))
    (insert "\n")
    (my/noema-roam--insert-centered-line
     "Each square is one day; value is modified note count."
     'my/noema-roam-ui-meta)
    (insert "\n")
    (my/noema-roam--insert-centered-heatmap-row
     '("Legend  " (" 0 " . muted) " " (" 1 " . info) " "
       (" 2+ " . warning) " " (" 5+ " . success))
     #'my/noema-roam-ui--tone-face)
    (insert "\n")))

(defun my/noema-roam-dashboard-insert-heatmap (&optional days)
  "Insert a compact roam activity heatmap into the main dashboard."
  (condition-case nil
      (let* ((counts (my/noema-roam--activity-date-counts
                      (or days my/noema-roam--activity-heatmap-days)))
             (total (apply #'+ (mapcar #'cdr counts))))
        (when counts
          (my/noema-roam--insert-centered-line
           (format "Roam activity · last %d days · %d changes"
                   (length counts) total)
           (if (facep 'dashboard-heading)
               'dashboard-heading
             'bold))
          (insert "\n")
          (dolist (row (my/noema-roam--activity-heatmap-rows counts))
            (my/noema-roam--insert-centered-heatmap-row
             row
             #'my/noema-roam-ui--tone-face))
          (insert "\n\n")))
    (error nil)))

;; ── Canonical Wiki index compatibility ───────────────────────────────────────

(defun my/noema-roam-sync-full ()
  "Compatibility command for an atomic wiki.db rebuild."
  (interactive)
  (unless (fboundp 'my/noema-wiki-rebuild) (require 'init-aaronnote))
  (my/noema-wiki-rebuild))

(defun my/noema-roam-db-status ()
  "Compatibility command for canonical Wiki index status."
  (interactive)
  (unless (fboundp 'my/noema-wiki-index-status) (require 'init-aaronnote))
  (my/noema-wiki-index-status))

(defun my/noema-roam-magit ()
  "Open magit-status in the roam notes root."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "magit is not available — install it first"))
  (magit-status (my/noema-roam-root)))

(defun my/noema-roam-dired ()
  "Open dired at the roam notes root."
  (interactive)
  (dired (my/noema-roam-root)))

;; ── Roam completion-at-point (roam:// and ../ paths) ─────────────────────────

(defun my/noema-roam--todo-ref-completions (prefix)
  "Return dependency-ref completion strings for PREFIX via the runtime.
Queries the same `todo-refs' service the web editor's completion popup
uses (see `todoRefCompletions' in server/lib/runtime.mjs): same-file todos
first, then open statuses before closed ones; a todo with a stable id
completes to `#id', otherwise to the shortest unique text ref."
  (let* ((body (list :prefix prefix :file (or buffer-file-name "")))
         (result (my/noema-roam--runtime-call-interactive
                  "todo-refs" "--json" (json-serialize body)))
         (items (and result (gethash "items" result))))
    (delq nil (mapcar (lambda (item) (gethash "ref" item)) items))))

(defun my/noema-roam-capf ()
  "Completion-at-point for roam:// links and relative paths in Typst/md buffers."
  (let ((roam-prefix "roam://")
        (dotdot-re "\\.\\./"))
    (cond
     ;; after:/blocks:/task: dependency-ref completion on @@todo/@@itodo/
     ;; @@project/@@milestone/@@clock lines — same key set planning-dsl.mjs
     ;; treats as dep-refs.
     ((looking-back
       "\\(?:after\\|blocks\\|task\\)[ \t]*[:=][ \t]*\"?\\([^,;{}\"\n]*\\)"
       (line-beginning-position) t)
      (let* ((start (match-beginning 1))
             (end (point))
             (prefix (match-string-no-properties 1))
             (candidates (my/noema-roam--todo-ref-completions prefix)))
        (when candidates
          (list start end candidates :exclusive 'no))))
     ;; roam://... completion
     ((and (looking-back (concat roam-prefix "[^][\n\t ]*") (line-beginning-position) t)
           (save-excursion
             (re-search-backward (concat roam-prefix "\\([^][\n\t ]*\\)")
                                 (line-beginning-position) t)))
      (let* ((start (match-beginning 0))
             (end   (match-end 0))
             (candidates
              (mapcar (lambda (slug) (concat roam-prefix slug))
                      (my/noema-roam--all-slugs-cached))))
        (when candidates
          (list start end candidates :exclusive 'no))))
     ;; ../  relative path completion
     ((looking-back (concat dotdot-re "[^][\n\t ]*") (line-beginning-position) t)
      (let* ((root (my/noema-roam-root))
             (start (save-excursion
                      (re-search-backward (concat dotdot-re "\\([^][\n\t ]*\\)")
                                          (line-beginning-position) t)
                      (match-beginning 0)))
             (end (point))
             (candidates
              (when (file-directory-p root)
                (let ((rel (buffer-substring-no-properties start end))
                      result)
                  (dolist (f (my/noema-roam--all-files))
                    (let ((r (file-relative-name f (file-name-directory
                                                    (or buffer-file-name root)))))
                      (when (string-prefix-p rel r)
                        (push r result))))
                  result))))
        (when candidates
          (list start end candidates :exclusive 'no))))
     (t nil))))

(defun my/noema-roam--capf-setup ()
  "Register roam capf for this buffer."
  (add-hook 'completion-at-point-functions #'my/noema-roam-capf nil t))

(add-hook 'markdown-mode-hook #'my/noema-roam--capf-setup)

;; ── Interactive Markdown roam link selector ──────────────────────────────────

(defvar-local my/noema-roam-select--origin-marker nil)
(defvar-local my/noema-roam-select--current-note-id nil)
(defvar-local my/noema-roam-select--preferred-kind nil)
(defvar-local my/noema-roam-select--view nil)
(defvar-local my/noema-roam-select--path "")
(defvar-local my/noema-roam-select--query nil)
(defvar-local my/noema-roam-select--target-record nil)
(defvar-local my/noema-roam-select--target-basis 'id)
(defvar-local my/noema-roam-select--toc-parent nil)

(defvar my/noema-roam-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map my/noema-roam-ui-mode-map)
    (define-key map (kbd "RET") #'my/noema-roam-select-activate)
    (define-key map (kbd "i") #'my/noema-roam-select-insert-current)
    (define-key map (kbd "/") #'my/noema-roam-select-search)
    (define-key map (kbd "s") #'my/noema-roam-select-search)
    (define-key map (kbd "g") #'my/noema-roam-select-root)
    (define-key map (kbd ".") #'my/noema-roam-select-context)
    (define-key map (kbd "u") #'my/noema-roam-select-up)
    (define-key map (kbd "^") #'my/noema-roam-select-up)
    (define-key map (kbd "r") #'my/noema-roam-select-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my/noema-roam-select-mode'.")

(define-derived-mode my/noema-roam-select-mode my/noema-roam-ui-mode "Roam-Select"
  "Interactive Markdown roam link selector."
  (setq-local truncate-lines t)
  (setq-local my/noema-roam-ui-refresh-function
              #'my/noema-roam-select-refresh)
  (my/noema-roam-ui-set-header "Roam Selector" 'search "search")
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (markerp my/noema-roam-select--origin-marker)
                (set-marker my/noema-roam-select--origin-marker nil)))
            nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/noema-roam-select-mode 'emacs))

(defun my/noema-roam--record-path-ref (record)
  "Return RECORD's path-like link ref."
  (let* ((note (plist-get record :note))
         (file (plist-get record :file))
         (rel-file (and file
                        (file-name-absolute-p file)
                        (file-relative-name file (my/noema-roam-root)))))
    (or (my/noema-roam--note-field note "path")
        (my/noema-roam--note-field note "link")
        rel-file
        (plist-get record :key)
        (plist-get record :id))))

(defun my/noema-roam--target-suffix (kind target)
  "Return Markdown roam link suffix for KIND and TARGET."
  (pcase kind
    ('tag (concat "#" (my/noema-roam--encode-ref target)))
    ('dom (concat "@" (mapconcat #'my/noema-roam--encode-ref
                                  (my/noema-roam--dom-target-segments target)
                                  "@")))
    (_ "")))

(defun my/noema-roam--link-target-for-record (record basis &optional kind target)
  "Return Markdown roam link target for RECORD using BASIS, KIND, and TARGET."
  (let ((basis (if (stringp basis) (intern basis) basis)))
    (if (eq basis 'path)
        (concat (my/noema-roam--record-path-ref record)
                (my/noema-roam--target-suffix kind target))
      (my/noema-roam--roam-href (plist-get record :id) kind target))))

(defun my/noema-roam--insert-note-link-target (target text &optional marker)
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

(defun my/noema-roam--tag-targets (record)
  "Return tag target plists for RECORD."
  (let* ((file (plist-get record :file))
         (note (plist-get record :note))
         (labels (and file (my/noema-roam--heading-labels file)))
         (inline-tags (my/noema-roam--note-list-field note "inlineTags"))
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

(defun my/noema-roam--tag-target-display (target)
  "Return completion display string for tag TARGET."
  (let ((id (plist-get target :id))
        (label (plist-get target :label)))
    (if (and label (not (equal id label)))
        (format "%s  %s" id label)
      id)))

(defun my/noema-roam--read-tag-target (record)
  "Read a tag target for RECORD."
  (let* ((targets (my/noema-roam--tag-targets record))
         (table (mapcar (lambda (target)
                          (cons (my/noema-roam--tag-target-display target)
                                target))
                        targets))
         (choice (if table
                     (completing-read "Tag: " table nil t)
                   (user-error "No tag ids in this note"))))
    (cdr (assoc choice table))))

(defun my/noema-roam-select--toc-targets (record)
  "Return TOC targets for RECORD."
  (let* ((file (plist-get record :file))
         (note-id (plist-get record :id))
         (seen nil)
         targets)
    (dolist (target (my/noema-roam--dom-targets file note-id))
      (let ((key (string-join (plist-get target :path) "@")))
        (when (and (not (plist-get target :synthetic))
                   (not (string-empty-p key))
                   (not (member key seen)))
          (push key seen)
          (push target targets))))
    (nreverse targets)))

(defun my/noema-roam-select--toc-dom (target)
  "Return DOM target string for TOC TARGET."
  (string-join (plist-get target :path) "@"))

(defun my/noema-roam-select--read-basis ()
  "Read target basis for the selected note."
  (intern
   (completing-read "Target ref: "
                    '("id" "path")
                    nil t nil nil "id")))

(defun my/noema-roam-select--read-kind ()
  "Read exact target kind for the selected note."
  (pcase (completing-read "Target kind: "
                          '("note" "tag" "toc")
                          nil t nil nil "note")
    ("tag" 'tag)
    ("toc" 'toc)
    (_ 'note)))

(defun my/noema-roam-select--default-note-text (record)
  "Return default display text for RECORD."
  (or (plist-get record :title)
      (plist-get record :id)))

(defun my/noema-roam-select--finish-target (record basis kind target default-text)
  "Insert final note-link for RECORD, BASIS, KIND, TARGET, and DEFAULT-TEXT."
  (let* ((href (my/noema-roam--link-target-for-record record basis kind target))
         (text (read-string (format "Display text [%s]: " default-text)
                            nil nil default-text)))
    (my/noema-roam--insert-note-link-target
     href text my/noema-roam-select--origin-marker)
    (when-let* (((derived-mode-p 'my/noema-roam-select-mode))
                (window (get-buffer-window (current-buffer))))
      (quit-window t window))))

(defun my/noema-roam-select--choose-record (record)
  "Choose exact target for note RECORD."
  (let* ((basis (my/noema-roam-select--read-basis))
         (kind (or my/noema-roam-select--preferred-kind
                   (my/noema-roam-select--read-kind))))
    (pcase kind
      ('tag
       (let* ((tag (my/noema-roam--read-tag-target record))
              (id (plist-get tag :id))
              (label (or (plist-get tag :label) id)))
         (my/noema-roam-select--finish-target
          record basis 'tag id label)))
      ('toc
       (setq my/noema-roam-select--target-record record
             my/noema-roam-select--target-basis basis
             my/noema-roam-select--toc-parent nil
             my/noema-roam-select--query nil)
       (my/noema-roam-select--render-toc))
      (_
       (my/noema-roam-select--finish-target
        record basis nil nil
        (my/noema-roam-select--default-note-text record))))))

(defun my/noema-roam-select--record-relative-file (record)
  "Return RECORD's relative file under the roam root, or nil."
  (when-let* ((file (plist-get record :file)))
    (let ((rel (file-relative-name file (my/noema-roam-root))))
      (unless (or (string-prefix-p "../" rel)
                  (string-prefix-p "/.." rel)
                  (string-match-p "\\`_typst/" rel))
        rel))))

(defun my/noema-roam-select--directory-items (dir)
  "Return directory and note items immediately inside DIR."
  (let ((dir (if (string-empty-p (or dir "")) "" dir))
        dirs notes seen-dirs)
    (dolist (record (my/noema-roam--note-records))
      (when-let* ((rel (my/noema-roam-select--record-relative-file record)))
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

(defun my/noema-roam-select--insert-row (label item &optional face)
  "Insert a selectable row LABEL carrying ITEM."
  (let* ((type (plist-get item :type))
         (record (plist-get item :record))
         (target (plist-get item :target))
         (title (pcase type
                  ('dir (format "%s/" (plist-get item :name)))
                  ('note (my/noema-roam-select--default-note-text record))
                  ('toc (my/noema-roam--dom-target-path-label target))
                  (_ label)))
         (meta (pcase type
                 ('dir (plist-get item :path))
                 ('note (my/noema-roam--record-path-ref record))
                 ('toc (if (plist-get item :has-children) "branch" "heading"))
                 (_ nil)))
         (detail (and (eq type 'note) (plist-get record :id)))
         (tags (and (eq type 'note)
                    (my/noema-roam--note-list-field
                     (plist-get record :note) "tags")))
         (id (pcase type
               ('dir (plist-get item :path))
               ('note (plist-get record :id))
               ('toc (my/noema-roam-select--toc-dom target))
               (_ label))))
    (my/noema-roam-ui-insert-row
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
     :title-face (or face 'my/noema-roam-ui-row-title)
     :meta meta
     :detail detail
     :tags tags
     :action
     (lambda (_ignored)
       (my/noema-roam-select-activate))
     :help "RET: open/select, i: insert/select"
     :properties `(my/noema-roam-select-item ,item))))

(defun my/noema-roam-select--note-label (record &optional prefix)
  "Return display label for note RECORD with PREFIX."
  (let* ((title (my/noema-roam-select--default-note-text record))
         (path (my/noema-roam--record-path-ref record))
         (tags (my/noema-roam--note-list-field (plist-get record :note) "tags")))
    (concat (or prefix "")
            (format "%-38s %s" title (plist-get record :id))
            (when path (concat "  " path))
            (when-let* ((strtags (seq-filter #'stringp tags)))
              (concat "  #" (string-join strtags " #"))))))

(defun my/noema-roam-select--render-header (title)
  "Render selector TITLE and help."
  (let ((icon (pcase my/noema-roam-select--view
                ('root 'directory)
                ('context 'related)
                ('toc 'toc)
                (_ 'search))))
    (my/noema-roam-ui-set-header
     "Roam Selector" icon
     (format "%s view" (or my/noema-roam-select--view 'search)))
    (my/noema-roam-ui-insert-page-header
     title
     :icon icon
     :subtitle "Choose a note, tag, or TOC target without leaving the keyboard"
     :stats (list (cons (format "%s view"
                               (or my/noema-roam-select--view 'search))
                       'info))
     :actions
     '((:label "RET Select"
        :command my/noema-roam-select-activate
        :help "Open or select the current item"
        :primary t)
       (:label "/ Search"
        :command my/noema-roam-select-search
        :help "Search notes or TOC headings")
       (:label "g Root"
        :command my/noema-roam-select-root
        :help "Show the roam root")
       (:label "r Refresh"
        :command my/noema-roam-select-refresh
        :help "Refresh the current selector view")
       (:label "q Close"
        :command quit-window
        :help "Close the selector")))))

(defun my/noema-roam-select--render-root (&optional dir)
  "Render roam root tree at DIR."
  (setq my/noema-roam-select--view 'root
        my/noema-roam-select--path (or dir ""))
  (my/noema-roam-ui-render
   (lambda ()
     (my/noema-roam-select--render-header
      (format "Roam root: /%s" my/noema-roam-select--path))
     (let ((items (my/noema-roam-select--directory-items
                   my/noema-roam-select--path)))
       (my/noema-roam-ui-insert-section "Contents" (length items))
       (if items
           (dolist (item items)
             (pcase (plist-get item :type)
               ('dir
                (my/noema-roam-select--insert-row
                 (format "%s/" (plist-get item :name))
                 item 'my/noema-roam-ui-row-title))
               ('note
                (my/noema-roam-select--insert-row
                 (my/noema-roam-select--note-label
                  (plist-get item :record))
                 item))))
         (my/noema-roam-ui-insert-empty "This directory is empty.")))))
  (unless (my/noema-roam-select--item-at-point)
    (my/noema-roam-ui-goto-first-item)))

(defun my/noema-roam-select--render-context ()
  "Render current-note context."
  (setq my/noema-roam-select--view 'context)
  (let ((record (and my/noema-roam-select--current-note-id
                     (my/noema-roam--resolve-note
                      my/noema-roam-select--current-note-id)))
        entries)
    (when record
      (push record entries)
      (dolist (id (append (my/noema-roam--note-links (plist-get record :id))
                          (my/noema-roam--db-backlinks-to
                           (plist-get record :id))))
        (when-let* ((related (my/noema-roam--resolve-note id)))
          (push related entries))))
    (setq entries (delete-dups (nreverse entries)))
    (my/noema-roam-ui-render
     (lambda ()
       (my/noema-roam-select--render-header
        (format "Current roam context: %s"
                (or my/noema-roam-select--current-note-id "(none)")))
       (my/noema-roam-ui-insert-section "Context" (length entries))
       (if entries
           (dolist (entry entries)
             (my/noema-roam-select--insert-row
              (my/noema-roam-select--note-label entry)
              (list :type 'note :record entry)))
         (my/noema-roam-ui-insert-empty
          "Not in a roam note. Press g to browse the root."))))
    (unless (my/noema-roam-select--item-at-point)
      (my/noema-roam-ui-goto-first-item))))

(defun my/noema-roam-select--render-search (query)
  "Render global note search for QUERY."
  (setq my/noema-roam-select--view 'search
        my/noema-roam-select--query query)
  (let ((entries (my/noema-roam-search-notes query)))
    (my/noema-roam-ui-render
     (lambda ()
       (my/noema-roam-select--render-header
        (if (string-empty-p (or query ""))
            "Search all roam notes"
          (format "Roam search: %s" query)))
       (my/noema-roam-ui-insert-section "Matches" (length entries))
       (if entries
           (dolist (entry entries)
             (when-let* ((record (my/noema-roam--resolve-note
                                  (plist-get entry :slug))))
               (my/noema-roam-select--insert-row
                (my/noema-roam-select--note-label record)
                (list :type 'note :record record))))
         (my/noema-roam-ui-insert-empty "No matching notes."))))
    (unless (my/noema-roam-select--item-at-point)
      (my/noema-roam-ui-goto-first-item))))

(defun my/noema-roam-select--toc-children (targets parent)
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

(defun my/noema-roam-select--toc-has-children-p (targets target)
  "Return non-nil if TARGET has child targets in TARGETS."
  (let* ((path (plist-get target :path))
         (len (length path)))
    (seq-some
     (lambda (candidate)
       (let ((candidate-path (plist-get candidate :path)))
         (and (> (length candidate-path) len)
              (equal (seq-take candidate-path len) path))))
     targets)))

(defun my/noema-roam-select--render-toc ()
  "Render TOC selector for `my/noema-roam-select--target-record'."
  (setq my/noema-roam-select--view 'toc)
  (let* ((record my/noema-roam-select--target-record)
         (targets (my/noema-roam-select--toc-targets record))
         (query my/noema-roam-select--query)
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
                    (my/noema-roam-select--toc-children
                     targets my/noema-roam-select--toc-parent))))
    (my/noema-roam-ui-render
     (lambda ()
       (my/noema-roam-select--render-header
        (format "TOC: %s%s"
                (my/noema-roam-select--default-note-text record)
                (if query (format " / search: %s" query) "")))
       (when my/noema-roam-select--toc-parent
         (my/noema-roam-ui-insert-field
          "Path" (string-join my/noema-roam-select--toc-parent " / ")
          'my/noema-roam-ui-path)
         (insert "\n"))
       (my/noema-roam-ui-insert-section "Headings" (length visible))
       (if visible
           (dolist (target visible)
             (let* ((has-children
                     (and (not query)
                          (my/noema-roam-select--toc-has-children-p
                           targets target)))
                    (label (my/noema-roam--dom-target-path-label target)))
               (my/noema-roam-select--insert-row
                label
                (list :type 'toc
                      :target target
                      :has-children has-children
                      :search query))))
         (my/noema-roam-ui-insert-empty "No TOC targets."))))
    (unless (my/noema-roam-select--item-at-point)
      (my/noema-roam-ui-goto-first-item))))

(defun my/noema-roam-select--item-at-point ()
  "Return selector item at point."
  (or (get-text-property (point) 'my/noema-roam-select-item)
      (get-text-property (line-beginning-position) 'my/noema-roam-select-item)
      (get-text-property (max (point-min) (1- (point)))
                         'my/noema-roam-select-item)))

(defun my/noema-roam-select--finish-toc-target (target)
  "Insert the selected TOC TARGET."
  (let* ((record my/noema-roam-select--target-record)
         (dom (my/noema-roam-select--toc-dom target))
         (label (my/noema-roam--dom-target-path-label target)))
    (my/noema-roam-select--finish-target
     record my/noema-roam-select--target-basis 'dom dom label)))

(defun my/noema-roam-select-activate ()
  "Activate the selector item at point."
  (interactive)
  (pcase-let* ((item (my/noema-roam-select--item-at-point))
               (type (plist-get item :type)))
    (pcase type
      ('dir
       (my/noema-roam-select--render-root (plist-get item :path)))
      ('note
       (my/noema-roam-select--choose-record (plist-get item :record)))
      ('toc
       (if (and (plist-get item :has-children)
                (not (plist-get item :search)))
           (progn
             (setq my/noema-roam-select--toc-parent
                   (plist-get (plist-get item :target) :path)
                   my/noema-roam-select--query nil)
             (my/noema-roam-select--render-toc))
         (my/noema-roam-select--finish-toc-target
          (plist-get item :target))))
      (_
       (user-error "No selectable roam item at point")))))

(defun my/noema-roam-select-insert-current ()
  "Insert/select the current selector item without descending."
  (interactive)
  (let ((item (my/noema-roam-select--item-at-point)))
    (pcase (plist-get item :type)
      ('toc
       (my/noema-roam-select--finish-toc-target
        (plist-get item :target)))
      (_
       (my/noema-roam-select-activate)))))

(defun my/noema-roam-select-search ()
  "Search notes globally, or TOC headings inside a TOC view."
  (interactive)
  (let ((query (read-string "Search: ")))
    (if (eq my/noema-roam-select--view 'toc)
        (progn
          (setq my/noema-roam-select--query query
                my/noema-roam-select--toc-parent nil)
          (my/noema-roam-select--render-toc))
      (my/noema-roam-select--render-search query))))

(defun my/noema-roam-select-root ()
  "Render the roam root tree."
  (interactive)
  (my/noema-roam-select--render-root ""))

(defun my/noema-roam-select-context ()
  "Render current-note context."
  (interactive)
  (if my/noema-roam-select--current-note-id
      (my/noema-roam-select--render-context)
    (my/noema-roam-select--render-root "")))

(defun my/noema-roam-select-up ()
  "Move one selector level up."
  (interactive)
  (pcase my/noema-roam-select--view
    ('root
     (let* ((path (string-remove-suffix "/" my/noema-roam-select--path))
            (parent (if (string-match "\\`\\(.*?/\\)?[^/]+\\'" path)
                        (or (match-string 1 path) "")
                      "")))
       (my/noema-roam-select--render-root parent)))
    ('toc
     (setq my/noema-roam-select--query nil
           my/noema-roam-select--toc-parent
           (butlast my/noema-roam-select--toc-parent))
     (my/noema-roam-select--render-toc))
    (_
     (my/noema-roam-select-context))))

(defun my/noema-roam-select-refresh ()
  "Refresh the current selector view."
  (interactive)
  (pcase my/noema-roam-select--view
    ('root (my/noema-roam-select--render-root my/noema-roam-select--path))
    ('search (my/noema-roam-select--render-search my/noema-roam-select--query))
    ('toc (my/noema-roam-select--render-toc))
    (_ (my/noema-roam-select-context))))

(defun my/noema-roam-select--display-buffer (buffer)
  "Display selector BUFFER in a focused bottom side window."
  (let* ((alist `((side . bottom)
                  (slot . 1)
                  (window-height . ,my/noema-roam-select-window-height)))
         (window (or (get-buffer-window buffer)
                     (display-buffer-in-side-window buffer alist))))
    (set-window-buffer window buffer)
    (select-window window)
    window))

(defun my/noema-roam-select-link (&optional preferred-kind)
  "Open an interactive note-link selector.
PREFERRED-KIND may be `tag' or `toc' to skip the target-kind prompt."
  (interactive)
  (let ((origin (copy-marker (point) t))
        (current-note-id (my/noema-roam--current-slug))
        (buf (get-buffer-create "*Noema roam select*")))
    (with-current-buffer buf
      (my/noema-roam-select-mode)
      (setq-local my/noema-roam-select--origin-marker origin
                  my/noema-roam-select--current-note-id current-note-id
                  my/noema-roam-select--preferred-kind preferred-kind
                  my/noema-roam-select--target-record nil
                  my/noema-roam-select--target-basis 'id
                  my/noema-roam-select--toc-parent nil
                  my/noema-roam-select--query nil)
      (my/noema-roam-select--render-search ""))
    (my/noema-roam-select--display-buffer buf)))

(defun my/noema-roam-copy-link-to-here ()
  "Copy a Markdown roam link to the current note or current heading.
When point is on a heading, ensure a Markdown `{#tag-id}' exists and copy a
canonical `roam://note-id#tag' target."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer has no file"))
  (let* ((note-id (my/noema-roam--current-slug))
         (title (my/noema-roam--note-title note-id))
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
              (setq id (my/noema-roam-generate-tag-id heading))
              (end-of-line)
              (insert (format " {#%s}" id)))
            (setq target (my/noema-roam--roam-href note-id 'tag id)
                  text heading))
        (setq target (my/noema-roam--roam-href note-id)
              text title)))
    (let ((link (format "[%s](%s)"
                        (replace-regexp-in-string "\\]" "\\\\]" (or text ""))
                        target)))
      (kill-new link)
      (message "Copied %s" link))))

;; Enhanced find-note with DB annotation
(defun my/noema-roam--note-annotator (slug)
  "Return annotation for SLUG in completing-read."
  (when-let* ((record (my/noema-roam--resolve-note slug))
              (note (plist-get record :note)))
    (let ((tags (my/noema-roam--note-list-field note "tags"))
          (bls  (length (or (my/noema-roam--db-backlinks-to
                             (plist-get record :id))
                            (gethash "backlinks" note)))))
      (concat "  "
              (if tags (string-join (seq-filter #'stringp tags) ",") "")
              (when (> bls 0) (format " ←%d" bls))))))

;; ── Keymaps & menus ───────────────────────────────────────────────────────────

(defvar my/noema-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'my/noema-roam-find-note)
    (define-key map (kbd "o") #'my/noema-roam-follow-link)
    (define-key map (kbd "i") #'my/noema-roam-insert-link)
    (define-key map (kbd "RET") #'my/noema-roam-select-link)
    (define-key map (kbd "I") #'my/noema-roam-insert-tag-id-link)
    (define-key map (kbd "c") #'my/noema-roam-insert-toc-link)
    (define-key map (kbd "y") #'my/noema-roam-copy-link-to-here)
    (define-key map (kbd "n") #'my/noema-roam-new-node)
    (define-key map (kbd "#") #'my/noema-roam-insert-tag-id)
    (define-key map (kbd "g") #'my/noema-roam-generate-tag-id)
    (define-key map (kbd "s") #'my/noema-roam-search-notes)
    (define-key map (kbd "r") #'my/noema-roam-recent-notes)
    (define-key map (kbd "R") #'my/noema-roam-related-notes)
    (define-key map (kbd "G") #'my/noema-roam-graph)
    (define-key map (kbd "M") #'my/noema-wiki-repositories)
    (define-key map (kbd "b") #'my/noema-roam-backlinks)
    (define-key map (kbd "t") #'my/noema-wiki-tags)
    (define-key map (kbd "T") #'my/noema-roam-todos)
    (define-key map (kbd "F") #'my/noema-roam-jump-file-todo)
    (define-key map (kbd "A") #'my/noema-roam-agenda)
    (define-key map (kbd "u") #'my/noema-wiki-refresh)
    (define-key map (kbd "U") #'my/noema-wiki-rebuild)
    (define-key map (kbd "S") #'my/noema-wiki-index-status)
    (define-key map (kbd "V") #'my/noema-roam-magit)
    (define-key map (kbd "D") #'my/noema-roam-dired)
    (define-key map (kbd "Q") #'my/noema-stop)
    (define-key map (kbd "m") #'my/noema-roam-dispatch)
    map)
  "Roam keymap for Markdown buffers. Bound to C-c r.")

(my/leader!
  "r m" '(:def my/noema-roam-dispatch :which-key "md roam")
  "r a" '(:def my/noema-roam-agenda   :which-key "roam agenda")
  "r d" '(:def my/noema-roam-dired    :which-key "roam dired")
  "r v" '(:def my/noema-roam-magit    :which-key "roam magit")
  "r S" '(:def my/noema-wiki-index-status :which-key "Wiki index status")
  "r e" '(:def my/noema-open-markdown-raw :which-key "edit raw md")
  "r o" '(:def my/noema-open-current-note :which-key "open in Noema"))

;; ── xref backend: gd / M-. for note-link ─────────────────────────────────

(defun my/noema-roam--all-slugs-cached ()
  "Return all canonical roam note ids."
  (mapcar (lambda (record) (plist-get record :id))
          (my/noema-roam--note-records)))

(defun my/noema-roam-xref-backend ()
  "Use aaronnote-roam as xref backend when point is on a Markdown roam link."
  (when (my/noema-roam--target-at-point) 'aaronnote-roam))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql aaronnote-roam)))
  (my/noema-roam--target-at-point))

(defun my/noema-roam-goto-definition ()
  "Jump to the note-link target at point, falling back to normal gd."
  (interactive)
  (if (my/noema-roam--target-at-point)
      (progn
        (when (fboundp 'my/navigation--push-jump)
          (my/navigation--push-jump))
        (my/noema-roam-follow-link))
    (if (fboundp 'my/navigation-find-definition)
        (call-interactively #'my/navigation-find-definition)
      (call-interactively #'xref-find-definitions))))

(defun my/noema-roam--xref-location (file parsed)
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
                  (my/noema-roam--find-dom-target
                   (plist-get parsed :dom) file (plist-get parsed :slug))
                  :pos)))))
      (if pos
          (progn
            (my/noema-roam--goto-pos pos)
            (xref-make-file-location file
                                     (line-number-at-pos)
                                     (current-column)))
        (xref-make-file-location file 1 0)))))

(cl-defmethod xref-backend-definitions ((_backend (eql aaronnote-roam)) target)
  (when-let* ((parsed (my/noema-roam--parse-target target))
              (file (plist-get parsed :file))
              ((file-exists-p file)))
    (list (xref-make (concat "note: " target)
                     (my/noema-roam--xref-location file parsed)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql aaronnote-roam)))
  (mapcar #'my/noema-roam--roam-href
          (my/noema-roam--all-slugs-cached)))

(defun my/noema-roam--xref-setup ()
  "Register aaronnote-roam xref backend for this buffer (highest priority)."
  (add-hook 'xref-backend-functions #'my/noema-roam-xref-backend -90 t))

;; ── Preview click → note-link intercept ──────────────────────────────────

;; ── Daily note ────────────────────────────────────────────────────────────

(defun my/noema-roam-daily-note ()
  "Open or create today's daily note at daily/YYYY-MM-DD."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (slug (concat "daily/" date))
         (file (my/noema-roam--slug-to-file slug)))
    (if (file-exists-p file)
        (my/noema-roam--open-slug slug)
      (my/noema-roam-new--create-draft
       (list :node-type "roam"
             :id slug
             :title (format "%s Daily" date)
             :path (concat slug ".md")
             :kind "note"
             :template-key "daily"
             :tags nil)))))

;; ── Wire everything up ────────────────────────────────────────────────────

(define-key my/noema-roam-map (kbd "d") #'my/noema-roam-daily-note)

(defconst my/noema-roam--help-groups
  '(("Navigate"  my/noema-roam-find-note my/noema-roam-follow-link
                 my/noema-roam-select-link my/noema-roam-recent-notes
                 my/noema-roam-related-notes my/noema-roam-daily-note
                 my/noema-roam-jump-file-todo)
    ("Link"      my/noema-roam-insert-link my/noema-roam-insert-tag-id-link
                 my/noema-roam-insert-toc-link my/noema-roam-copy-link-to-here
                 my/noema-roam-insert-tag-id my/noema-roam-generate-tag-id)
    ("Create"    my/noema-roam-new-node)
    ("Search"    my/noema-roam-search-notes my/noema-roam-backlinks
                 my/noema-wiki-tags my/noema-roam-graph)
    ("Tasks"     my/noema-roam-todos my/noema-roam-agenda)
    ("Maintain"  my/noema-wiki-refresh my/noema-wiki-rebuild
                 my/noema-wiki-index-status my/noema-wiki-repositories
                 my/noema-roam-magit my/noema-roam-dired my/noema-stop
                 my/noema-roam-dispatch my/noema-roam-help))
  "Grouping of `my/noema-roam-map' commands for the help board.
Only the grouping lives here; the keys themselves are read back out of the
keymap so this page cannot drift away from the bindings it documents.")

(defun my/noema-roam--help-rows ()
  "Return (GROUP . ((KEY . COMMAND) ...)) rows describing `my/noema-roam-map'.
Commands present in the keymap but absent from `my/noema-roam--help-groups'
are collected under \"Other\" so nothing is silently undocumented."
  (let ((by-command (make-hash-table :test 'eq))
        (grouped '())
        (seen (make-hash-table :test 'eq)))
    (map-keymap
     (lambda (event definition)
       (when (commandp definition)
         (push (key-description (vector event))
               (gethash definition by-command))))
     my/noema-roam-map)
    (dolist (group my/noema-roam--help-groups)
      (let (rows)
        (dolist (command (cdr group))
          (dolist (key (sort (gethash command by-command) #'string<))
            (puthash command t seen)
            (push (cons key command) rows)))
        (when rows (push (cons (car group) (nreverse rows)) grouped))))
    (let (rest)
      (maphash (lambda (command keys)
                 (unless (gethash command seen)
                   (dolist (key (sort keys #'string<))
                     (push (cons key command) rest))))
               by-command)
      (when rest (push (cons "Other" (nreverse rest)) grouped)))
    (nreverse grouped)))

(defun my/noema-roam--help-render ()
  "Render the roam keybinding help board."
  (aaron-ui-board-render
   (lambda ()
     (aaron-ui-board-insert-page-header
      "Roam keys"
      :icon 'note
      :subtitle (format "%s in a note buffer, or C-c r <key>"
                        (if my/noema-roam-help-key
                            (format "%s / C-c r ?" my/noema-roam-help-key)
                          "C-c r ?")))
     (insert "\n")
     (dolist (group (my/noema-roam--help-rows))
       (aaron-ui-board-insert-section (car group) (length (cdr group)))
       (dolist (row (cdr group))
         (aaron-ui-board-insert-field
          (format "C-c r %s" (car row))
          (or (car (split-string
                    (or (documentation (cdr row)) (symbol-name (cdr row)))
                    "\n"))
              (symbol-name (cdr row)))))
       (insert "\n"))
     (aaron-ui-board-insert-section "Leader" 7)
     (dolist (row '(("SPC r m" . "roam / Wiki dispatch")
                    ("SPC r a" . "agenda")
                    ("SPC r d" . "dired in the vault")
                    ("SPC r v" . "magit in the vault")
                    ("SPC r S" . "Wiki index status")
                    ("SPC r e" . "edit this note raw in Emacs")
                    ("SPC r o" . "open this note in Noema")))
       (aaron-ui-board-insert-field (car row) (cdr row)))
     (insert "\n")
     (aaron-ui-board-insert-section "Also" 3)
     (aaron-ui-board-insert-field "gd / M-." "follow the note link at point (xref)")
     (aaron-ui-board-insert-field "H-o" "global Noema dispatch")
     (aaron-ui-board-insert-field "M-x reports"
                                  "orphans, dead ends, hubs, wanted pages")
     (insert "\n")
     (aaron-ui-board-insert-key-hints "g refresh  ·  q close"))))

;;;###autoload
(defun my/noema-roam-help ()
  "Show the Noema roam keybinding help board.
The page is generated from `my/noema-roam-map', so it always matches the
bindings that are actually installed."
  (interactive)
  (let ((buffer (my/noema-roam--prepare-ui-buffer
                 "*roam-help*" "Roam keys" 'note
                 #'my/noema-roam--help-render)))
    (with-current-buffer buffer
      (my/noema-roam--help-render))
    (pop-to-buffer buffer)))

(define-key my/noema-roam-map (kbd "?") #'my/noema-roam-help)

(config-defvar my/noema-roam-help-key nil
  "Evil normal-state key that opens the roam help board in note buffers.

Defaults to `?'.  That key is `evil-search-backward' elsewhere, so it is only
rebound inside vault notes, and only in normal state; set this to nil to keep
`?' as backward search everywhere.  The help board is always reachable through
`C-c r ?' regardless of this setting."
  :type '(choice (const :tag "Disabled" nil) (string :tag "Key"))
  :group 'my/noema-roam)

(defun my/noema-roam--setup-help-key ()
  "Bind `my/noema-roam-help-key' in this note buffer's evil normal state."
  (when (and my/noema-roam-help-key
             (fboundp 'evil-local-set-key)
             (my/noema-roam--note-in-vault-p
              (expand-file-name (or buffer-file-name ""))))
    (evil-local-set-key 'normal (kbd my/noema-roam-help-key)
                        #'my/noema-roam-help)))

(defun my/noema-roam-setup-keys ()
  "Set up Noema roam keys and xref for the current Markdown buffer.
Binds `C-c r' to `my/noema-roam-map' and registers the roam xref
backend.  Does not install completion-at-point functions (those are
added separately by `my/noema-roam--capf-setup')."
  (local-set-key (kbd "C-c r") my/noema-roam-map)
  (my/noema-roam--setup-help-key)
  (my/noema-roam--xref-setup))

(add-hook 'markdown-mode-hook #'my/noema-roam-setup-keys)

;; Update transient with daily + gd hint
(defun my/noema-roam-dispatch ()
  "Open Noema canonical Wiki dispatch."
  (interactive)
  (unless (fboundp 'my/noema-wiki-dispatch) (require 'init-aaronnote))
  (my/noema-wiki-dispatch))

;;; Legacy native report helpers (canonical UI lives in /wiki?view=reports).

(defconst my/noema-roam--report-limit 200
  "Maximum rows shown in a single wiki report.")

(defun my/noema-roam--wiki-stats ()
  "Return a plist of vault-wide wiki statistics from the cached index."
  (let* ((entries (my/noema-roam--all-note-summaries))
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

(defun my/noema-roam-report-orphaned ()
  "Show notes with no backlinks (MediaWiki Special:LonelyPages analog)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e)
                     (and (null (plist-get e :backlinks))
                          (not (string-prefix-p "daily/"
                                                (or (plist-get e :slug) "")))))
                   (my/noema-roam--all-note-summaries)))
         (entries (seq-take entries my/noema-roam--report-limit))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-orphaned*" "Orphaned Pages" 'orphan
               #'my/noema-roam-report-orphaned
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Orphaned pages"
          :icon 'orphan
          :subtitle "Notes no other note links to"
          :stats (list (cons (format "%d notes" (length entries)) 'warning))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Orphaned" (length entries))
         (if (null entries)
             (my/noema-roam-ui-insert-empty "No orphaned notes.")
           (dolist (entry entries)
             (my/noema-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/noema-roam-report-dead-end ()
  "Show notes that link to no other note (MediaWiki Special:DeadendPages)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e) (null (plist-get e :links)))
                   (my/noema-roam--all-note-summaries)))
         (entries (seq-take entries my/noema-roam--report-limit))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-dead-end*" "Dead-end Pages" 'dead-end
               #'my/noema-roam-report-dead-end
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Dead-end pages"
          :icon 'dead-end
          :subtitle "Notes with no outgoing links"
          :stats (list (cons (format "%d notes" (length entries)) 'warning))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Dead-end" (length entries))
         (if (null entries)
             (my/noema-roam-ui-insert-empty "No dead-end notes.")
           (dolist (entry entries)
             (my/noema-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/noema-roam-report-uncategorized ()
  "Show notes with no tags (MediaWiki Special:UncategorizedPages analog)."
  (interactive)
  (let* ((entries (seq-filter
                   (lambda (e) (null (plist-get e :tags)))
                   (my/noema-roam--all-note-summaries)))
         (entries (seq-take entries my/noema-roam--report-limit))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-uncategorized*" "Uncategorized Pages" 'uncategorized
               #'my/noema-roam-report-uncategorized
               (format "%d notes" (length entries)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Uncategorized pages"
          :icon 'uncategorized
          :subtitle "Notes with no tags"
          :stats (list (cons (format "%d notes" (length entries)) 'muted))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Uncategorized" (length entries))
         (if (null entries)
             (my/noema-roam-ui-insert-empty "All notes have tags.")
           (dolist (entry entries)
             (my/noema-roam--insert-note-button entry))))))
    (display-buffer buf)))

(defun my/noema-roam-report-most-linked ()
  "Show the most-linked notes (MediaWiki Special:MostLinkedPages analog)."
  (interactive)
  (let* ((entries (my/noema-roam--all-note-summaries))
         (sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (> (length (plist-get a :backlinks))
                            (length (plist-get b :backlinks))))))
         (top (seq-take sorted my/noema-roam--report-limit))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-most-linked*" "Most-linked Pages" 'hub
               #'my/noema-roam-report-most-linked
               (format "top %d" (length top)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Most-linked pages"
          :icon 'hub
          :subtitle "Hub notes sorted by incoming links"
          :stats (list (cons (format "%d notes" (length top)) 'info))
          :actions (my/noema-roam--ui-actions))
         (my/noema-roam-ui-insert-section "Hubs" (length top))
         (if (null top)
             (my/noema-roam-ui-insert-empty "No linked notes.")
           (dolist (entry top)
             (let ((bls (length (plist-get entry :backlinks))))
               (my/noema-roam-ui-insert-row
                :id (plist-get entry :slug)
                :icon 'note
                :title (or (plist-get entry :title) (plist-get entry :slug))
                :meta (format "%d backlinks" bls)
                :tags (plist-get entry :tags)
                :action (let ((slug (plist-get entry :slug)))
                          (lambda (_b) (my/noema-roam--open-slug slug))))))))))
    (display-buffer buf)))

(defun my/noema-roam--render-wanted-buffer (items)
  "Render wanted-pages ITEMS (a list of alists) into a roam report buffer."
  (let* ((items (seq-take items my/noema-roam--report-limit))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-wanted*" "Wanted Pages" 'wanted
               #'my/noema-roam-report-wanted
               (format "%d targets" (length items)))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Wanted pages"
          :icon 'wanted
          :subtitle "Link targets that have no matching note"
          :stats (list (cons (format "%d targets" (length items)) 'danger))
          :actions (my/noema-roam--ui-actions
                    (list (list :label "Create all wanted"
                                :command #'my/noema-roam-report-wanted
                                :help "Refresh after creating notes"))))
         (my/noema-roam-ui-insert-section "Wanted" (length items))
         (if (null items)
             (my/noema-roam-ui-insert-empty "No wanted pages. All links resolve!")
           (dolist (item items)
             (let* ((target (alist-get 'target item))
                    (by (alist-get 'by item))
                    (by (cond ((vectorp by) (append by nil))
                              ((listp by) by)
                              (t nil)))
                    (by-count (length by)))
               (my/noema-roam-ui-insert-row
                :id target
                :icon 'wanted
                :title (format "%s" target)
                :meta (format "linked from %d note%s" by-count
                              (if (= by-count 1) "" "s"))
                :action (let ((ref target))
                          (lambda (_b)
                            (my/noema-roam--create-linked-node ref))))))))))
    (display-buffer buf)))

(defun my/noema-roam-report-wanted ()
  "Show link targets that have no corresponding note (MediaWiki Special:WantedPages)."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running; start it with H-o o first"))
  (message "Noema: fetching wanted pages...")
  (my/noema--api-call
   "aaronnote:api:notes:wanted" []
   (lambda (result)
     (let ((items (alist-get 'items result)))
       (my/noema-roam--render-wanted-buffer
        (if (vectorp items) (append items nil)
          (or items nil)))))))

(defun my/noema-roam--asset-items (items)
  "Normalize JSON asset ITEMS into a list."
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(defun my/noema-roam--asset-field (asset field)
  "Return ASSET FIELD from an alist or hash table."
  (cond
   ((hash-table-p asset) (gethash (symbol-name field) asset))
   ((listp asset) (alist-get field asset))
   (t nil)))

(defun my/noema-roam--asset-file (asset)
  "Return ASSET absolute file path."
  (format "%s" (or (my/noema-roam--asset-field asset 'file) "")))

(defun my/noema-roam--format-asset-size (asset)
  "Return a human-readable size label for ASSET."
  (let ((size (my/noema-roam--asset-field asset 'size)))
    (if (numberp size)
        (file-size-human-readable size)
      "unknown size")))

(defun my/noema-roam--format-asset-mtime (asset)
  "Return a human-readable modified-time label for ASSET."
  (let ((mtime-ms (my/noema-roam--asset-field asset 'mtimeMs)))
    (if (numberp mtime-ms)
        (format-time-string "%Y-%m-%d %H:%M"
                            (seconds-to-time (/ mtime-ms 1000.0)))
      "unknown mtime")))

(defun my/noema-roam--open-asset (asset)
  "Open ASSET's file in Emacs."
  (let ((file (my/noema-roam--asset-file asset)))
    (if (and (not (string-empty-p file)) (file-exists-p file))
        (find-file file)
      (user-error "Asset file does not exist: %s" file))))

(defun my/noema-roam--trash-orphaned-assets (assets)
  "Move orphaned ASSETS to Trash through the Noema runtime."
  (let* ((assets (my/noema-roam--asset-items assets))
         (files (delq nil
                      (mapcar (lambda (asset)
                                (let ((file (my/noema-roam--asset-file asset)))
                                  (unless (string-empty-p file) file)))
                              assets))))
    (unless files
      (user-error "No orphaned attachments to trash"))
    (when (yes-or-no-p (format "Move %d orphaned attachment%s to Trash? "
                               (length files)
                               (if (= (length files) 1) "" "s")))
      (message "Noema: moving orphaned attachments to Trash...")
      (my/noema--api-call
       "aaronnote:api:assets:trash-orphans" (vector files)
       (lambda (result)
         (let ((next-assets (my/noema-roam--asset-items
                             (alist-get 'assets result)))
               (trashed (my/noema-roam--asset-items
                         (alist-get 'trashed result)))
               (skipped (my/noema-roam--asset-items
                         (alist-get 'skipped result))))
           (message "Noema: trashed %d orphaned attachment%s%s"
                    (length trashed)
                    (if (= (length trashed) 1) "" "s")
                    (if skipped
                        (format ", skipped %d" (length skipped))
                      ""))
           (my/noema-roam--render-orphaned-assets-buffer next-assets)))))))

(defun my/noema-roam--render-orphaned-assets-buffer (assets)
  "Render orphaned attachment ASSETS into a roam report buffer."
  (let* ((items (seq-take (my/noema-roam--asset-items assets)
                          my/noema-roam--report-limit))
         (count (length items))
         (buf (my/noema-roam--prepare-ui-buffer
               "*roam-orphaned-assets*" "Orphaned Attachments" 'attachment
               #'my/noema-roam-report-orphaned-assets
               (format "%d assets" count))))
    (with-current-buffer buf
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-page-header
          "Orphaned attachments"
          :icon 'attachment
          :subtitle "Files in asset folders that no note currently references"
          :stats (list (cons (format "%d assets" count)
                             (if (> count 0) 'warning 'success)))
          :actions (my/noema-roam--ui-actions
                    (when items
                      `((:label "Trash listed"
                         :command ,(let ((orphans items))
                                     (lambda ()
                                       (my/noema-roam--trash-orphaned-assets
                                        orphans)))
                         :help "Move the listed orphaned attachments to Trash"
                         :primary t)))))
         (my/noema-roam-ui-insert-section "Attachments" count)
         (if (null items)
             (my/noema-roam-ui-insert-empty
              "No orphaned attachments. All scanned assets are referenced.")
           (dolist (asset items)
             (let* ((path (format "%s" (or (my/noema-roam--asset-field asset 'path)
                                           (my/noema-roam--asset-file asset))))
                    (type (format "%s" (or (my/noema-roam--asset-field asset 'type)
                                           "asset"))))
               (my/noema-roam-ui-insert-row
                :id (my/noema-roam--asset-file asset)
                :icon (if (my/noema-roam--asset-field asset 'isImage)
                          'image
                        'attachment)
                :badge type
                :badge-tone 'muted
                :title path
                :meta (my/noema-roam--format-asset-size asset)
                :detail (my/noema-roam--format-asset-mtime asset)
                :action (let ((item asset))
                          (lambda (_b)
                            (my/noema-roam--open-asset item))))))))))
    (display-buffer buf)))

(defun my/noema-roam-report-orphaned-assets ()
  "Show unreferenced Noema attachments and generated media assets."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running; start it with H-o o first"))
  (message "Noema: scanning orphaned attachments...")
  (my/noema--api-call
   "aaronnote:api:assets:scan-orphans" []
   (lambda (result)
     (my/noema-roam--render-orphaned-assets-buffer
      (alist-get 'assets result)))))

(with-eval-after-load 'transient
  ;; Named apart from `my/noema-roam-reports', which opens the web Wiki
  ;; reports view.  Both used to be called `my/noema-roam-reports': because
  ;; `transient' is required at the top of this file the `with-eval-after-load'
  ;; body ran immediately, so the plain `defun' 340 lines below always won and
  ;; these six native reports were reachable only through `M-x'.
  (transient-define-prefix my/noema-roam-reports-native ()
    "Native knowledge-health reports rendered in Emacs board buffers."
    [["Special pages"
      ("w" "wanted pages"       my/noema-roam-report-wanted)
      ("o" "orphaned pages"     my/noema-roam-report-orphaned)
      ("a" "orphaned attachments" my/noema-roam-report-orphaned-assets)
      ("d" "dead-end pages"     my/noema-roam-report-dead-end)
      ("u" "uncategorized"      my/noema-roam-report-uncategorized)
      ("h" "most-linked (hubs)" my/noema-roam-report-most-linked)]]))

;;; Wiki category browser (MediaWiki Category: system analog).

(defun my/noema-roam--category-tree (entries)
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

(defun my/noema-roam-categories ()
  "Browse notes hierarchically by category (nested tags, MediaWiki Category: analog).
Select a top-level category to drill down; select a note to open it."
  (interactive)
  (let* ((entries (my/noema-roam--all-note-summaries))
         (tree (my/noema-roam--category-tree entries))
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
                   (my/noema-roam--open-slug val))))))))
      (browse prefix))))

;;; Wiki move-page with automatic link rewrite (MediaWiki "Move page").

(defun my/noema-roam-move-note ()
  "Rename/move a roam note and rewrite all referencing links.
Prompts for the note to move and a new file name, then calls the
backend's fs:rename + roam-tools:rewrite-path-refs pipeline."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running; start it with H-o o first"))
  (let* ((slug (my/noema-roam--read-note-id "Move note: "))
         (file (my/noema-roam--slug-to-file slug))
         (old-name (file-name-nondirectory file))
         (new-name (read-string (format "New file name for '%s': " old-name)
                                old-name))
         (old-rel (file-relative-name file (my/noema-roam-root)))
         (new-rel (concat (file-name-directory old-rel) new-name)))
    (unless (file-exists-p file)
      (user-error "File not found: %s" file))
    (when (string= old-name new-name)
      (user-error "New name is the same as old name"))
    (message "Moving '%s' → '%s' and rewriting links..." old-rel new-rel)
    (my/noema--api-call
     "aaronnote:api:fs:rename"
     (vector (list (cons "file" file) (cons "targetName" new-name)))
     (lambda (result)
       (if (not (alist-get 'ok result))
           (message "Noema move failed: %s" (alist-get 'message result))
         (my/noema--api-call
          "aaronnote:api:roam-tools:rewrite-path-refs"
          (vector (list (cons "oldPath" old-rel) (cons "newPath" new-rel)))
          (lambda (rewrite-result)
            (my/noema-roam--clear-runtime-cache)
            (let ((changed (length (or (alist-get 'changed rewrite-result) []))))
              (message "Moved '%s' → '%s'; rewrote links in %d file%s."
                       old-rel new-rel changed
                       (if (= changed 1) "" "s"))))))))))

;;; Tag management wrappers for the management dashboard.

(defun my/noema-roam-rename-tag ()
  "Rename a tag across all vault notes via the Noema runtime."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running"))
  (let* ((old-tag (read-string "Old tag name: "))
         (new-tag (read-string (format "Rename '%s' to: " old-tag) old-tag)))
    (when (string-empty-p old-tag)
      (user-error "Tag name cannot be empty"))
    (when (string= old-tag new-tag)
      (user-error "New tag is the same as old tag"))
    (message "Renaming tag '%s' → '%s'..." old-tag new-tag)
    (my/noema--api-call
     "aaronnote:api:roam-tools:rename-tag"
     (vector (list (cons "oldTag" old-tag) (cons "newTag" new-tag)))
     (lambda (result)
       (my/noema-roam--clear-runtime-cache)
       (let ((changed (or (alist-get 'changed result) 0)))
         (message "Renamed tag '%s' → '%s' in %d file%s."
                  old-tag new-tag changed (if (= changed 1) "" "s")))))))

(defun my/noema-roam-delete-tag ()
  "Delete a tag from all vault notes via the Noema runtime."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running"))
  (let ((tag (read-string "Delete tag: ")))
    (when (string-empty-p tag)
      (user-error "Tag name cannot be empty"))
    (when (yes-or-no-p (format "Delete tag '%s' from all notes? " tag))
      (message "Deleting tag '%s'..." tag)
      (my/noema--api-call
       "aaronnote:api:roam-tools:delete-tag"
       (vector (list (cons "tag" tag)))
       (lambda (result)
         (my/noema-roam--clear-runtime-cache)
         (let ((changed (or (alist-get 'changed result) 0)))
           (message "Deleted tag '%s' from %d file%s."
                    tag changed (if (= changed 1) "" "s"))))))))

(defun my/noema-roam-tag-overlap ()
  "Show overlapping/redundant tags report via the Noema runtime."
  (interactive)
  (unless (and (boundp 'my/noema--ready) my/noema--ready)
    (user-error "Noema web-host is not running"))
  (message "Analyzing tag overlap...")
  (my/noema--api-call
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

(defconst my/noema-roam--dashboard-tools
  '((:id find-note
     :icon search
     :badge "FIND"
     :badge-tone info
     :title "Find note"
     :meta "findnode"
     :detail "Jump by Noema id, path, or title."
     :command my/noema-roam-find-note
     :help "Find and open a roam note")
    (:id create-node
     :icon new
     :badge "CREATE"
     :badge-tone success
     :title "Create node"
     :meta "Roam Node"
     :detail "Open the single native node creation workbench."
     :command my/noema-roam-new-node
     :help "Create a new roam node")
    (:id search-notes
     :icon search
     :badge "QUERY"
     :title "Search notes"
     :meta "title/tag/linksto"
     :detail "Filter notes with scoped search operators."
     :command my/noema-roam-search-notes
     :help "Search Noema roam notes")
    (:id daily-note
     :icon note
     :badge "TODAY"
     :badge-tone warning
     :title "Daily note"
     :meta "daily/YYYY-MM-DD"
     :detail "Open or create today's daily note."
     :command my/noema-roam-daily-note
     :help "Open today's daily note")
    (:id agenda
     :icon agenda
     :badge "TASKS"
     :title "Agenda"
     :meta "open tasks"
     :detail "Review open todos grouped by due date and status."
     :command my/noema-roam-agenda
     :help "Open the roam agenda")
    (:id todos
     :icon todo
     :badge "TODO"
     :title "Task list"
     :meta "all todos"
     :detail "List every indexed task across the vault."
     :command my/noema-roam-todos
     :help "List all roam tasks")
    (:id categories
     :icon tag
     :badge "TAGS"
     :title "Categories"
     :meta "nested tags"
     :detail "Browse wiki-style categories from nested tags."
     :command my/noema-roam-categories
     :help "Browse nested tag categories")
    (:id tags
     :icon tag
     :badge "TAG"
     :title "Flat tags"
     :meta "tag picker"
     :detail "Pick a tag, then open one of its notes."
     :command my/noema-roam-tags
     :help "Browse notes by tag")
    (:id recent-notes
     :icon clock
     :badge "RECENT"
     :badge-tone muted
     :title "Recent notes"
     :meta "history"
     :detail "Return to recently opened roam notes."
     :command my/noema-roam-recent-notes
     :help "Show recently opened roam notes"))
  "Command rows shown in the Roam management dashboard quick tools section.")

(defun my/noema-roam--dashboard-tool-action (command)
  "Return a board row action that invokes COMMAND interactively."
  (lambda (_button)
    (if (commandp command)
        (call-interactively command)
      (funcall command))))

(defun my/noema-roam--dashboard-insert-tools ()
  "Insert common command shortcuts into the Roam management dashboard."
  (my/noema-roam-ui-insert-section
   "Quick tools" (length my/noema-roam--dashboard-tools) 'info)
  (dolist (tool my/noema-roam--dashboard-tools)
    (my/noema-roam-ui-insert-row
     :id (list 'dashboard-tool (plist-get tool :id))
     :icon (plist-get tool :icon)
     :badge (plist-get tool :badge)
     :badge-tone (plist-get tool :badge-tone)
     :title (plist-get tool :title)
     :meta (plist-get tool :meta)
     :detail (plist-get tool :detail)
     :action (my/noema-roam--dashboard-tool-action
              (plist-get tool :command))
     :help (plist-get tool :help)))
  (insert "\n"))



;;; Public lifecycle API (called from init-aaronnote.el).

(defun my/noema-roam--cancel-sync-timer ()
  "Compatibility no-op; Wiki index maintenance is owned by the web host."
  nil)

(defun my/noema-roam--vault-file (file)
  "Return FILE as an absolute vault path in scanner form, or nil.

Resolves symlinks on both sides before comparing.  `find-file' stores a
truename in `buffer-file-name', while the scanner builds its paths from the
configured root, so a vault reached through a symlink — or anything under
macOS's /var -> /private/var link — compared unequal and every save looked
like it had happened outside the vault.  The returned path uses the root as
configured so it matches the cached records."
  (when (and (stringp file) (not (string-empty-p file)))
    (let* ((root (file-name-as-directory
                  (expand-file-name (my/noema-roam-root))))
           (file-abs (expand-file-name file)))
      (if (string-prefix-p root file-abs)
          file-abs
        (let ((root-true (file-name-as-directory (file-truename root)))
              (file-true (file-truename file-abs)))
          (when (string-prefix-p root-true file-true)
            (expand-file-name (file-relative-name file-true root-true) root)))))))

(defun my/noema-roam--note-in-vault-p (file)
  "Return non-nil when FILE is a Markdown note inside the vault root."
  (and (my/noema-roam--vault-file file)
       (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)))

(defun my/noema-roam--invalidate-note (file)
  "Invalidate cached Emacs-side state after FILE changed on disk.

Cheaper than `my/noema-roam--clear-runtime-cache': a save of a note that
already exists keeps the directory listing and re-reads only that one file.
Backlinks are computed across every record, so the summary cache still has to
go; the runtime payload is owned by the web-host and is stale after any write."
  (setq my/noema-roam--runtime-index-cache nil
        my/noema-roam--runtime-index-cache-key nil
        my/noema-roam--all-note-summaries-cache nil)
  (let ((file (or (my/noema-roam--vault-file file) (expand-file-name file))))
    (if (and my/noema-roam--all-files-cache
             (member file my/noema-roam--all-files-cache)
             (file-exists-p file))
        (when my/noema-roam--scan-cache
          (setq my/noema-roam--scan-cache
                (mapcar (lambda (record)
                          (if (equal (plist-get record :file) file)
                              (my/noema-roam--scan-record file)
                            record))
                        my/noema-roam--scan-cache)))
      ;; Created or deleted: the directory listing itself is out of date.
      (setq my/noema-roam--all-files-cache nil
            my/noema-roam--scan-cache nil))))

(defun my/noema-roam-note-changed (file)
  "Invalidate Emacs-side caches when FILE was saved.
The web-host owns wiki.db maintenance; Emacs must not start another indexer."
  (when (and (stringp file)
             (not (string-empty-p file))
             (my/noema-roam--note-in-vault-p (expand-file-name file)))
    (my/noema-roam--invalidate-note file)))

(defun my/noema-roam--after-save-h ()
  "Invalidate note caches after saving a vault note from Emacs.

The web-host emits its `saved' event only for writes made in the web editor,
so an Emacs-side save used to leave every cache stale: backlinks, search and
`find-note' kept answering from the pre-save index until something else
happened to clear them.  The backend picks the write up through its own file
watcher, so this only has to repair the Emacs side."
  (when (and buffer-file-name
             (my/noema-roam--note-in-vault-p
              (expand-file-name buffer-file-name)))
    (my/noema-roam--invalidate-note buffer-file-name)))

(defun my/noema-roam--setup-save-hook ()
  "Track saves of the current Markdown buffer when it is a vault note."
  (add-hook 'after-save-hook #'my/noema-roam--after-save-h nil t))

(add-hook 'markdown-mode-hook #'my/noema-roam--setup-save-hook)

;; Keep the historical command names used by existing keymaps, while making
;; the web-host Wiki UI the only maintenance/report implementation.
(defun my/noema-roam-management ()
  "Open canonical Wiki repository management."
  (interactive)
  (unless (fboundp 'my/noema-wiki-repositories) (require 'init-aaronnote))
  (my/noema-wiki-repositories))

(defun my/noema-roam-reports ()
  "Open canonical Wiki reports."
  (interactive)
  (unless (fboundp 'my/noema-wiki-reports) (require 'init-aaronnote))
  (my/noema-wiki-reports))

(provide 'init-md-roam)
;;; init-md-roam.el ends here
