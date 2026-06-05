;;; init-note-code.el --- Tagged source regions in Typst notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the note/source relationship used by `#note-code'.
;; Lean defaults to the Aaronnote-style `.lean/' mirror.  Every other language
;; requires an explicit source path.

;;; Code:

(require 'subr-x)

(defgroup my/note-code nil
  "Tagged source regions embedded in Typst notes."
  :group 'my/aaronnote-roam)

(defcustom my/note-code-root "/Users/hc/Documents/AaronNote/"
  "Root containing Typst notes, source files, and the `.lean/' mirror."
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
    (if (boundp 'my/aaronnote-roam-root)
        my/aaronnote-roam-root
      my/note-code-root))))

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

(defun my/note-code-lean-path (selector)
  "Resolve @@lean4 SELECTOR to an absolute path under <root>/lean/.
SELECTOR is a path relative to the lean/ directory, e.g. \"math/foo.lean\"."
  (expand-file-name
   (string-remove-prefix "./" selector)
   (expand-file-name "lean" (my/note-code--root))))

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

(provide 'init-note-code)
;;; init-note-code.el ends here
