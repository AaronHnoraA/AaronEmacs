;;; init-note-code.el --- Tagged source regions in Typst notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the note/source relationship used by `#note-code'.
;; Lean defaults to the matching `.lean' file under the roam root.  Every other
;; language requires an explicit source path.

;;; Code:

(require 'config)

(require 'subr-x)

(declare-function my/aaronnote-roam-root "init-md-roam" ())
(defvar my/aaronnote--notes-root)
(defvar my/aaronnote-roam-root)

(defgroup my/note-code nil
  "Tagged source regions embedded in Typst notes."
  :group 'my/aaronnote-roam)

(config-defvar my/note-code-root nil
  "Root containing notes and source files."
  :type 'directory
  :group 'my/note-code)

(defconst my/note-code--call-regexp
  (rx "#note-code"
      (* space) "("
      (group (*? anything))
      ")" (* space)
      "[" (group (*? (not (any "]")))) "]")
  "Regexp matching one-line `#note-code' calls.")

(defconst my/note-code--marker-regexp
  (rx line-start (* nonl)
      "@" (or "aaronnote" "note-code") (+ space)
      (group (+ (not space)))
      (* space) line-end)
  "Regexp matching Aaronnote and generic source-region markers.")

(defun my/note-code--root ()
  "Return the normalized note root."
  (file-name-as-directory
   (expand-file-name
    (cond
     ((fboundp 'my/aaronnote-roam-root)
      (my/aaronnote-roam-root))
     ((and (boundp 'my/aaronnote--notes-root)
           (stringp my/aaronnote--notes-root))
      my/aaronnote--notes-root)
     ((and (boundp 'my/aaronnote-roam-root)
           (stringp my/aaronnote-roam-root))
      my/aaronnote-roam-root)
     (t my/note-code-root)))))

(defun my/note-code--arg (name args)
  "Read NAME from Typst argument string ARGS."
  (when (string-match
         (format "\\_<%s\\_>[[:space:]]*:[[:space:]]*\\(\"[^\"]*\"\\|[^,[:space:])]+\\)"
                 (regexp-quote name))
         args)
    (string-remove-suffix
     "\""
     (string-remove-prefix "\"" (match-string 1 args)))))

(defun my/note-code-at-point ()
  "Return the `#note-code' call at point as a plist, or nil."
  (save-excursion
    (let ((origin (point))
          found)
      (goto-char (line-beginning-position))
      (while (and (not found)
                  (re-search-forward my/note-code--call-regexp
                                     (line-end-position) t))
        (when (<= (match-beginning 0) origin (match-end 0))
          (let* ((args (match-string-no-properties 1))
                 (tag (string-trim (match-string-no-properties 2)))
                 (begin (match-beginning 0))
                 (end (match-end 0))
                 (lang (or (my/note-code--arg "lang" args) "lean"))
                 (path (my/note-code--arg "path" args)))
            (setq found
                  (list :lang lang :path path :tag tag
                        :begin begin :end end)))))
      found)))

(defun my/note-code--note-relative-path (&optional file)
  "Return FILE relative to the note root."
  (let* ((root (my/note-code--root))
         (file (expand-file-name (or file buffer-file-name "")))
         (relative (file-relative-name file root)))
    (when (string-prefix-p "../" relative)
      (user-error "Note is outside note-code root: %s" root))
    relative))

(defun my/note-code-lean-path-for-note (&optional selector file)
  "Return the Lean path for SELECTOR or FILE under the note root.
SELECTOR is relative to the note root.  Without SELECTOR, derive the path from
FILE, or from the current `buffer-file-name'."
  (let* ((source
          (if (and selector (not (string-empty-p selector)))
              (string-remove-prefix "/" (string-remove-prefix "./" selector))
            (my/note-code--note-relative-path file)))
         (lean-relative
          (concat (file-name-sans-extension source) ".lean")))
    (expand-file-name lean-relative (my/note-code--root))))

(define-obsolete-function-alias
  'my/note-code-lean-mirror-path
  #'my/note-code-lean-path-for-note
  "2026-07-05")

(defun my/note-code-lean-path (selector)
  "Resolve Lean SELECTOR to an absolute path under the note root.
SELECTOR is a path relative to the note root, e.g. \"math/foo.lean\"."
  (expand-file-name
   (my/note-code-lean-path-for-note selector)))

(defun my/note-code-source-path (call)
  "Resolve source path for note-code CALL."
  (let ((lang (downcase (plist-get call :lang)))
        (path (plist-get call :path)))
    (cond
     (path
      (if (member lang '("lean" "lean4"))
          (my/note-code-lean-path path)
        (expand-file-name (string-remove-prefix "/" path)
                          (my/note-code--root))))
     ((member lang '("lean" "lean4"))
      (my/note-code-lean-path-for-note))
     (t
      (user-error "note-code path is required for language `%s'" lang)))))

(defun my/note-code--goto-tag (tag)
  "Move point to the source region named TAG."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (re-search-forward my/note-code--marker-regexp nil t))
      (when (equal (match-string-no-properties 1) tag)
        (setq found t)))
    (unless found
      (user-error "Source tag not found: %s" tag))
    (forward-line 1)
    (back-to-indentation)))

(defun my/note-code-open-at-point ()
  "Open the source region referenced by `#note-code' at point."
  (interactive)
  (let* ((call (or (my/note-code-at-point)
                   (user-error "Point is not on a #note-code call")))
         (file (my/note-code-source-path call))
         (tag (plist-get call :tag)))
    (unless (file-exists-p file)
      (user-error "Source file does not exist: %s" file))
    (find-file file)
    (my/note-code--goto-tag tag)))

(defun my/note-code--ensure-preamble ()
  "Ensure the current Typst note imports and configures note-code."
  (let ((relative (my/note-code--note-relative-path)))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward
               (rx line-start "#import \"/_typst/roam.typ\": *")
               nil t)
        (insert "#import \"/_typst/roam.typ\": *\n"))
      (goto-char (point-min))
      (unless (re-search-forward
               (rx line-start "#let note-code = note-code.with(note-path:")
               nil t)
        (while (looking-at-p (rx "#import " (* nonl) line-end))
          (forward-line 1))
        (insert (format "#let note-code = note-code.with(note-path: %S)\n"
                        relative))))))

(defun my/note-code-insert (lang path tag)
  "Insert a note-code call for LANG, PATH, and TAG."
  (interactive
   (let* ((lang (completing-read "Language: "
                                 '("lean" "python" "rust" "c" "cpp"
                                   "javascript" "typescript" "elisp" "sh")
                                 nil nil nil nil "lean"))
          (lean-p (member (downcase lang) '("lean" "lean4")))
          (path (unless lean-p
                  (read-string "Project-root source path: ")))
          (tag (read-string "Source tag: ")))
     (list lang path tag)))
  (when (and (not (member (downcase lang) '("lean" "lean4")))
             (string-empty-p (or path "")))
    (user-error "Non-Lean note-code requires a source path"))
  (my/note-code--ensure-preamble)
  (insert
   (if (member (downcase lang) '("lean" "lean4"))
       (format "#note-code(lang: lean)[%s]" tag)
     (format "#note-code(lang: %S, path: %S)[%s]" lang path tag))))

(defun my/note-code-ensure-style-link ()
  "Ensure the note root exposes roam's internal Typst support files."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (file-in-directory-p buffer-file-name (my/note-code--root)))
    (let ((directory (expand-file-name "_typst" (my/note-code--root))))
      (dolist (entry '(("notes/note-code.typ" . "note-code.typ")
                       ("notes/aaronnote-html.css" . "roam/note.css")))
        (let ((source (expand-file-name (car entry) user-emacs-directory))
              (link (expand-file-name (cdr entry) directory)))
          (when (file-exists-p source)
            (make-directory (file-name-directory link) t)
            (unless (and (file-symlink-p link)
                         (file-equal-p source (file-truename link)))
              (when (file-exists-p link)
                (delete-file link))
              (make-symbolic-link source link))))))))

;;; ──────────────────────────────────────────────────────────────────
;;; @@note-code reference generation (from source code buffers)
;;; ──────────────────────────────────────────────────────────────────

(defun my/note-code--comment-prefix ()
  "Return the single-line comment prefix for the current major mode.
Uses `comment-start' when available; falls back to \"# \"."
  (let ((cs (or (bound-and-true-p comment-start) "#")))
    (concat (string-trim-right cs) " ")))

(defun my/note-code--ref-path ()
  "Return the path component for an @@note-code(...) reference.
For files inside the roam/notes root the path is root-relative to noteRoot.
For other project files the path is root-relative to the project root.
Signals an error if the buffer is not visiting a file."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((file   (expand-file-name buffer-file-name))
         (roam   (my/note-code--root))
         (proj   (when (and (fboundp 'project-current)
                            (fboundp 'project-root))
                   (when-let* ((p (project-current)))
                     (file-name-as-directory
                      (expand-file-name (project-root p)))))))
    (cond
     ;; A leading slash means "from the active content root" to Aaronnote:
     ;; noteRoot for roam files, project root for standalone project files.
     ((and roam (file-in-directory-p file roam))
      (concat "/" (file-relative-name file roam)))
     ((and proj (file-in-directory-p file proj))
      (concat "/" (file-relative-name file proj)))
     ;; Fallback: absolute path
     (t file))))

(defun my/note-code--marker-on-line ()
  "Return the @aaronnote/@note-code tag already present on the current line.
Returns nil if the line is not a marker line."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward my/note-code--marker-regexp (line-end-position) t)
      (string-trim (match-string-no-properties 1)))))

(defun my/note-code--default-tag ()
  "Return a plausible tag default: function name at point or a timestamp slug."
  (or (and (fboundp 'which-function) (which-function))
      (format-time-string "region-%Y%m%d%H%M%S")))

;;;###autoload
(defun my/note-code-copy-reference ()
  "Copy an @@note-code(path)[id] reference to the kill ring.

With an active region: prompt for a tag (default: function name or
timestamp), insert an @aaronnote marker comment before the first line
of the region, deactivate the mark, and copy @@note-code(PATH)[TAG].

Without a region: if the current line is already a marker comment,
copy its reference.  Otherwise prompt for a tag, insert the marker
before the current line, and copy the reference.

PATH uses a leading slash for files inside the roam vault or current project,
so AaronNote resolves it from the relevant root rather than the note directory."
  (interactive)
  (require 'project nil t)
  (let ((path (my/note-code--ref-path)))
    (if (use-region-p)
        ;; ── region active: insert marker before region, copy ref ──────────
        (let* ((default (my/note-code--default-tag))
               (tag (string-trim
                     (read-string (format "Reference tag [%s]: " default)
                                  nil nil default))))
          (when (string-empty-p tag)
            (user-error "Tag cannot be empty"))
          (save-excursion
            (goto-char (region-beginning))
            (goto-char (line-beginning-position))
            (insert (format "%s@aaronnote %s\n"
                            (my/note-code--comment-prefix) tag)))
          (deactivate-mark)
          (let ((ref (format "@@note-code(%s)[%s]" path tag)))
            (kill-new ref)
            (message "Copied  %s" ref)))
      ;; ── no region ─────────────────────────────────────────────────────
      (let ((existing (my/note-code--marker-on-line)))
        (if existing
            ;; Current line is already a marker — just copy its reference
            (let ((ref (format "@@note-code(%s)[%s]" path existing)))
              (kill-new ref)
              (message "Copied  %s" ref))
          ;; Current line is regular code — insert a new marker and copy ref
          (let* ((default (my/note-code--default-tag))
                 (tag (string-trim
                       (read-string (format "Reference tag [%s]: " default)
                                    nil nil default))))
            (when (string-empty-p tag)
              (user-error "Tag cannot be empty"))
            (save-excursion
              (goto-char (line-beginning-position))
              (insert (format "%s@aaronnote %s\n"
                              (my/note-code--comment-prefix) tag)))
            (let ((ref (format "@@note-code(%s)[%s]" path tag)))
              (kill-new ref)
              (message "Copied  %s" ref))))))))

(provide 'init-note-code)
;;; init-note-code.el ends here
