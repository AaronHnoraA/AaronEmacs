;;; init-aaronnote-jupyter-notebook.el --- Native ipynb source projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Visit an ipynb as an ordinary language buffer without asking Jupytext to
;; round-trip it.  The buffer is a transient percent-style source projection;
;; the file on disk always remains standard ipynb JSON.  Noema sidecars use
;; nbformat 4.5, while an external notebook keeps its existing minor version.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defvar-local my/noema-jupyter-notebook--document nil)
(defvar-local my/noema-jupyter-notebook--projection-p nil)
(defvar-local my/noema-jupyter-notebook--comment-prefix "#")
(defvar-local my/noema-jupyter-notebook--writing nil)
(defvar-local my/noema-jupyter-notebook--editor-mode nil)

(defconst my/noema-jupyter-notebook--transient-id-not-found
  (make-symbol "noema-jupyter-transient-id-not-found"))
(defconst my/noema-jupyter-notebook--transient-id-missing
  (make-symbol "noema-jupyter-transient-id-missing"))
(defvar my/noema-jupyter-notebook--transient-cell-ids
  (make-hash-table :test #'eq :weakness 'key)
  "Original ids for ordinary notebook cells using an internal projection id.")

(defconst my/noema-jupyter-notebook--marker-re
  "^[ \t]*\\(?://\\|--\\|#\\|;\\)[ \t]*%%\\(?:[ \t]+\\[\\(markdown\\|raw\\)\\]\\)?\\(?:[ \t]+id=\\([A-Za-z0-9_-]+\\)\\)?[ \t]*$")

(defun my/noema-jupyter-notebook--get (key object)
  "Return KEY from JSON OBJECT."
  (let ((name (if (symbolp key) (symbol-name key) key)))
    (cond
     ((hash-table-p object) (or (gethash name object) (gethash key object)))
     ((and (listp object) (keywordp (car object)))
      (plist-get object (if (keywordp key) key (intern (concat ":" name)))))
     ((listp object)
      (or (alist-get key object)
          (alist-get name object nil nil #'string=)
          (and (stringp name) (alist-get (intern-soft name) object)))))))

(defun my/noema-jupyter-notebook--hash (&optional value)
  "Return VALUE as a string-keyed hash table."
  (cond
   ((hash-table-p value) value)
   ((listp value)
    (let ((table (make-hash-table :test #'equal)))
      (dolist (entry value table)
        (when (consp entry)
          (puthash (if (symbolp (car entry))
                       (symbol-name (car entry))
                     (format "%s" (car entry)))
                   (cdr entry) table)))))
   (t (make-hash-table :test #'equal))))

(defun my/noema-jupyter-notebook--source (value)
  "Return notebook source VALUE as one string."
  (cond
   ((vectorp value) (mapconcat (lambda (part) (format "%s" part)) value ""))
   ((listp value) (mapconcat (lambda (part) (format "%s" part)) value ""))
   (t (replace-regexp-in-string "\r\n?" "\n" (format "%s" (or value ""))))))

(defun my/noema-jupyter-notebook--valid-id (value &optional fallback)
  "Return nbformat-valid cell id for VALUE, or FALLBACK."
  (let* ((text (replace-regexp-in-string
                "[^A-Za-z0-9_-]+" "-" (string-trim (format "%s" (or value "")))))
         (text (replace-regexp-in-string "^-+\\|-+$" "" text)))
    (if (string-empty-p text)
        (or fallback "cell")
      (substring text 0 (min 64 (length text))))))

(defun my/noema-jupyter-notebook--new-id ()
  "Return a new standard cell id."
  (format "cell-%s"
          (substring (secure-hash 'sha256
                                  (format "%s:%s:%s"
                                          (float-time) (random) (emacs-pid)))
                     0 12)))

(defun my/noema-jupyter-notebook--noema-standard-p (document)
  "Return non-nil when DOCUMENT declares Noema's managed ipynb metadata."
  (let* ((metadata (and (hash-table-p document)
                        (gethash "metadata" document)))
         (noema (my/noema-jupyter-notebook--get 'noema metadata))
         (source (my/noema-jupyter-notebook--get 'source_file noema)))
    (and noema
         ;; A standalone notebook managed through Noema points source_file at
         ;; itself; it remains an external notebook rather than becoming a
         ;; Noema note sidecar merely because UI/session metadata was added.
         (not (and (stringp source)
                   (string-match-p "\\.ipynb\\'" source)))
         (or source
             (my/noema-jupyter-notebook--get 'storage noema)
             (my/noema-jupyter-notebook--get 'session noema)))))

(defun my/noema-jupyter-notebook--valid-id-p (value)
  "Return non-nil when VALUE is already a valid nbformat cell id."
  (and (stringp value)
       (string-match-p "\\`[A-Za-z0-9_-]\\{1,64\\}\\'" value)))

(defun my/noema-jupyter-notebook--transient-id (cell)
  "Return CELL's original transient id marker, or the not-found sentinel."
  (gethash cell my/noema-jupyter-notebook--transient-cell-ids
           my/noema-jupyter-notebook--transient-id-not-found))

(defun my/noema-jupyter-notebook--normalize (document)
  "Normalize DOCUMENT for projection while retaining unknown fields.
Noema notebooks persist valid unique ids and nbformat 4.5.  Ordinary ipynb
files receive only transient in-memory ids when their cells have none, so an
Emacs visit or save does not silently migrate an external notebook."
  (unless (hash-table-p document)
    (error "Jupyter notebook root must be an object"))
  (let* ((metadata (my/noema-jupyter-notebook--hash
                    (gethash "metadata" document)))
         (managed (my/noema-jupyter-notebook--noema-standard-p document))
         (minor (or (gethash "nbformat_minor" document) 0))
         (seen (make-hash-table :test #'equal))
         normalized)
    (cl-loop for raw in (append (or (gethash "cells" document) nil) nil)
             for index from 1
             do
             (let* ((cell (my/noema-jupyter-notebook--hash raw))
                    (absent my/noema-jupyter-notebook--transient-id-not-found)
                    (original (gethash "id" cell absent))
                    (valid (and (my/noema-jupyter-notebook--valid-id-p original)
                                (not (gethash original seen))))
                    (base (my/noema-jupyter-notebook--valid-id
                           (and valid original) (format "cell-%d" index)))
                    (id base)
                    (attempt 0))
               (while (gethash id seen)
                 (setq attempt (1+ attempt))
                 (let ((suffix (format "-%d-%d" index attempt)))
                   (setq id (concat (substring base 0
                                               (min (length base)
                                                    (max 1 (- 64 (length suffix)))))
                                    suffix))))
               (puthash id t seen)
               (unless (or managed valid
                           (not (eq (my/noema-jupyter-notebook--transient-id cell)
                                    my/noema-jupyter-notebook--transient-id-not-found)))
                 (puthash cell
                          (if (eq original absent)
                              my/noema-jupyter-notebook--transient-id-missing
                            (cons 'original original))
                          my/noema-jupyter-notebook--transient-cell-ids))
               (puthash "id" id cell)
               (puthash "metadata"
                        (my/noema-jupyter-notebook--hash
                         (gethash "metadata" cell)) cell)
               (puthash "source"
                        (my/noema-jupyter-notebook--source
                         (gethash "source" cell)) cell)
               (when (equal (gethash "cell_type" cell) "code")
                 (unless (integerp (gethash "execution_count" cell))
                   (puthash "execution_count" nil cell))
                 (puthash "outputs"
                          (vconcat (or (gethash "outputs" cell) nil)) cell))
               (push cell normalized)))
    (puthash "cells" (vconcat (nreverse normalized)) document)
    (puthash "metadata" metadata document)
    (puthash "nbformat" 4 document)
    (puthash "nbformat_minor"
             (if managed (max 5 minor) minor) document)
    document))

(defun my/noema-jupyter-notebook--parse-string (text)
  "Parse notebook JSON TEXT."
  (my/noema-jupyter-notebook--normalize
   (json-parse-string text :object-type 'hash-table :array-type 'array
                      :null-object nil :false-object :json-false)))

(defun my/noema-jupyter-notebook--cell-ids-need-upgrade-p (document)
  "Return non-nil when DOCUMENT lacks unique nbformat 4.5 cell ids."
  (let ((seen (make-hash-table :test #'equal)) upgrade)
    (dolist (cell (append (or (gethash "cells" document) []) nil))
      (let ((id (and (hash-table-p cell) (gethash "id" cell))))
        (if (and (stringp id)
                 (string-match-p "\\`[A-Za-z0-9_-]\\{1,64\\}\\'" id)
                 (not (gethash id seen)))
            (puthash id t seen)
          (setq upgrade t))))
    upgrade))

(defun my/noema-jupyter-notebook--read-raw (file)
  "Read standard notebook FILE without visiting its source projection."
  (with-temp-buffer
    (insert-file-contents file)
    (my/noema-jupyter-notebook--parse-string (buffer-string))))

(defun my/noema-jupyter-notebook--language (document)
  "Return DOCUMENT's language name."
  (let* ((metadata (gethash "metadata" document))
         (language-info (my/noema-jupyter-notebook--get
                         'language_info metadata))
         (kernelspec (my/noema-jupyter-notebook--get 'kernelspec metadata)))
    (let ((language
           (downcase (format "%s" (or (my/noema-jupyter-notebook--get
                                        'name language-info)
                                       (my/noema-jupyter-notebook--get
                                        'language kernelspec)
                                       "python")))))
      (if (member language '("sage" "sagemath" "py" "python3"))
          "python"
        language))))

(defun my/noema-jupyter-notebook--prefix (language)
  "Return source comment prefix for LANGUAGE."
  (cond
   ((member language '("javascript" "typescript" "c" "cpp" "java"
                       "rust" "go" "swift" "kotlin" "csharp")) "//")
   ((member language '("sql" "lean" "lean4" "haskell")) "--")
   ((member language '("elisp" "emacs-lisp" "lisp" "scheme" "clojure")) ";")
   (t "#")))

(defun my/noema-jupyter-notebook--major-mode (language)
  "Return preferred major mode for LANGUAGE."
  (pcase language
    ((or "python" "sage") (if (fboundp 'python-ts-mode) 'python-ts-mode 'python-mode))
    ((or "javascript" "js") (cond ((fboundp 'my/js-auto-mode) 'my/js-auto-mode)
                                    ((fboundp 'js-ts-mode) 'js-ts-mode)
                                    (t 'js-mode)))
    ((or "typescript" "ts") (cond ((fboundp 'typescript-ts-mode) 'typescript-ts-mode)
                                    ((fboundp 'typescript-mode) 'typescript-mode)
                                    (t 'js-mode)))
    ((or "bash" "sh" "shell" "zsh") 'sh-mode)
    ((or "elisp" "emacs-lisp") 'emacs-lisp-mode)
    ((or "r" "R") (if (fboundp 'R-mode) 'R-mode 'text-mode))
    ("julia" (if (fboundp 'julia-mode) 'julia-mode 'text-mode))
    ("rust" (if (fboundp 'rust-mode) 'rust-mode 'text-mode))
    ((or "lean" "lean4") (if (fboundp 'lean4-mode) 'lean4-mode 'text-mode))
    ("sql" 'sql-mode)
    ("c" 'c-mode)
    ("cpp" 'c++-mode)
    (_ 'text-mode)))

(defun my/noema-jupyter-notebook--insert-source (cell prefix)
  "Insert CELL's source using percent projection PREFIX."
  (let* ((type (or (gethash "cell_type" cell) "code"))
         (source (my/noema-jupyter-notebook--source (gethash "source" cell)))
         (tag (pcase type ("markdown" " [markdown]") ("raw" " [raw]") (_ "")))
         (id (gethash "id" cell))
         (transient (my/noema-jupyter-notebook--transient-id cell))
         (marker-beg (point)))
    (insert prefix " %%" tag)
    (when (eq transient my/noema-jupyter-notebook--transient-id-not-found)
      (insert " id=" id))
    (insert "\n")
    ;; A missing on-disk id stays invisible but the transient identity remains
    ;; attached to the marker so UI operations can still address the cell.
    (add-text-properties marker-beg (1- (point))
                         `(my/noema-jupyter-cell-id ,id
                           rear-nonsticky
                           (my/noema-jupyter-cell-id)))
    (if (member type '("markdown" "raw"))
        (let ((lines (split-string source "\n" nil)))
          (cl-loop for tail on lines
                   for line = (car tail)
                   for last = (null (cdr tail))
                   do (insert prefix (unless (string-empty-p line) " ") line)
                   unless last do (insert "\n")))
      (insert source))
    ;; These separators are projection-only.  Their properties let saving
    ;; distinguish them from a newline that really belongs to cell.source.
    (unless (or (string-empty-p source) (string-suffix-p "\n" source))
      (insert (propertize "\n" 'my/noema-jupyter-notebook-separator t)))
    (insert (propertize "\n" 'my/noema-jupyter-notebook-separator t))))

(defun my/noema-jupyter-notebook--render (document)
  "Replace current buffer with DOCUMENT's source projection."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (prefix (my/noema-jupyter-notebook--prefix
                 (my/noema-jupyter-notebook--language document))))
    (erase-buffer)
    (dolist (cell (append (gethash "cells" document) nil))
      (my/noema-jupyter-notebook--insert-source cell prefix))
    (setq-local my/noema-jupyter-notebook--comment-prefix prefix)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun my/noema-jupyter-notebook--body-end (start end)
  "Return source end between START and END, excluding projection separators."
  (let ((position end))
    (while (and (> position start)
                (get-text-property (1- position)
                                   'my/noema-jupyter-notebook-separator))
      (setq position (1- position)))
    position))

(defun my/noema-jupyter-notebook--uncomment (text prefix)
  "Remove projection PREFIX from markdown/raw TEXT."
  (mapconcat
   (lambda (line)
     (replace-regexp-in-string
      (format "\\`[ \t]*%s\\(?: \\)?" (regexp-quote prefix)) "" line))
   (split-string text "\n" nil) "\n"))

(defun my/noema-jupyter-notebook--canonicalize-managed-markers ()
  "Add persisted ids to bare markers in a Noema-owned source projection.
This runs before save so a newly typed or legacy `# %%' marker becomes a real
cell before an execution command asks Noema to address it.  Ordinary external
notebooks keep their intentionally id-less markers."
  (when (and my/noema-jupyter-notebook--projection-p
             (my/noema-jupyter-notebook--noema-standard-p
              my/noema-jupyter-notebook--document))
    (let ((seen (make-hash-table :test #'equal)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward my/noema-jupyter-notebook--marker-re nil t)
            (when-let* ((id (match-string-no-properties 2)))
              (puthash id t seen)))
          (goto-char (point-min))
          (while (re-search-forward my/noema-jupyter-notebook--marker-re nil t)
            (unless (match-string-no-properties 2)
              (let ((id (my/noema-jupyter-notebook--new-id))
                    (marker-beg (line-beginning-position)))
                (while (gethash id seen)
                  (setq id (my/noema-jupyter-notebook--new-id)))
                (puthash id t seen)
                (goto-char (line-end-position))
                (insert " id=" id)
                (add-text-properties
                 marker-beg (line-end-position)
                 `(my/noema-jupyter-cell-id ,id
                   rear-nonsticky (my/noema-jupyter-cell-id)))))))))))

(defun my/noema-jupyter-notebook-projection-cells (&optional document)
  "Return ordered projected cell plists for current buffer.
DOCUMENT supplies the original cell objects retained by id."
  (let* ((document (or document my/noema-jupyter-notebook--document))
        (old-vector (vconcat (or (and document (gethash "cells" document)) [])))
        (old (make-hash-table :test #'equal))
        markers result)
    (dolist (cell (append (and document (gethash "cells" document)) nil))
      (puthash (gethash "id" cell) cell old))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward my/noema-jupyter-notebook--marker-re nil t)
          (let* ((line-beg (line-beginning-position))
                 (explicit-id (match-string-no-properties 2)))
          (push (list :id (or explicit-id
                              (get-text-property
                               line-beg 'my/noema-jupyter-cell-id))
                      :explicit-id explicit-id
                      :type (or (match-string-no-properties 1) "code")
                      :block-beg line-beg
                      :start-beg line-beg
                      :start-end (line-end-position)
                      :body-beg (progn (forward-line 1) (point)))
                markers))))
      (setq markers (nreverse markers))
      (cl-loop for marker on markers
               for entry = (car marker)
               for index from 0
               for next = (cadr marker)
               for raw-end = (or (plist-get next :block-beg) (point-max))
               for body-end = (my/noema-jupyter-notebook--body-end
                               (plist-get entry :body-beg) raw-end)
               for original = (and (< index (length old-vector))
                                   (aref old-vector index))
               for id = (or (plist-get entry :id)
                            (and original (gethash "id" original))
                            (format "cell-%d" (1+ index)))
               for type = (plist-get entry :type)
               for text = (buffer-substring-no-properties
                            (plist-get entry :body-beg) body-end)
               for source = (if (member type '("markdown" "raw"))
                                (my/noema-jupyter-notebook--uncomment
                                 text my/noema-jupyter-notebook--comment-prefix)
                              text)
               for retained = (or (gethash id old)
                                  (and (null (plist-get entry :explicit-id))
                                       original))
               do (progn
                    (when (null (plist-get entry :explicit-id))
                      (add-text-properties
                       (plist-get entry :start-beg)
                       (plist-get entry :start-end)
                       `(my/noema-jupyter-cell-id ,id
                         rear-nonsticky (my/noema-jupyter-cell-id))))
                    (push (append (plist-put entry :id id)
                                (list :body-end body-end
                                      :end-beg body-end :end-end raw-end
                                      :block-end raw-end
                                      :line (line-number-at-pos
                                             (plist-get entry :body-beg))
                                      :code source :source source
                                      :cell retained))
                          result))))
    (nreverse result)))

(defun my/noema-jupyter-notebook--sync-document (&optional base)
  "Merge current source projection into BASE or its notebook object.
Using a freshly read BASE preserves outputs that Noema wrote while this source
projection remained open in Emacs."
  (let ((document (or base my/noema-jupyter-notebook--document))
        cells)
    (dolist (entry (my/noema-jupyter-notebook-projection-cells
                    document))
      (let* ((old (or (plist-get entry :cell)
                      (make-hash-table :test #'equal)))
             (type (plist-get entry :type)))
        (when (and (null (plist-get entry :explicit-id))
                   (not (my/noema-jupyter-notebook--noema-standard-p document))
                   (eq (my/noema-jupyter-notebook--transient-id old)
                       my/noema-jupyter-notebook--transient-id-not-found))
          (puthash old my/noema-jupyter-notebook--transient-id-missing
                   my/noema-jupyter-notebook--transient-cell-ids))
        (puthash "cell_type" type old)
        (puthash "id" (plist-get entry :id) old)
        (puthash "metadata"
                 (my/noema-jupyter-notebook--hash (gethash "metadata" old)) old)
        (puthash "source" (plist-get entry :source) old)
        (if (equal type "code")
            (progn
              (unless (integerp (gethash "execution_count" old))
                (puthash "execution_count" nil old))
              (puthash "outputs" (vconcat (or (gethash "outputs" old) nil)) old)
              (remhash "attachments" old))
          (remhash "execution_count" old)
          (remhash "outputs" old)
          (when (equal type "raw") (remhash "attachments" old)))
        (push old cells)))
    (puthash "cells" (vconcat (nreverse cells)) document)
    (setq-local my/noema-jupyter-notebook--document
                (my/noema-jupyter-notebook--normalize document))))

(defun my/noema-jupyter-notebook--serialize (document)
  "Serialize DOCUMENT while restoring ordinary cells' original id shape."
  (let (restore)
    (unwind-protect
        (progn
          (dolist (cell (append (gethash "cells" document) nil))
            (let ((transient (my/noema-jupyter-notebook--transient-id cell)))
              (unless (eq transient
                          my/noema-jupyter-notebook--transient-id-not-found)
                (push (cons cell (gethash "id" cell)) restore)
                (if (eq transient
                        my/noema-jupyter-notebook--transient-id-missing)
                    (remhash "id" cell)
                  (puthash "id" (cdr transient) cell)))))
          (json-serialize document :null-object nil
                          :false-object :json-false))
      (dolist (entry restore)
        (puthash "id" (cdr entry) (car entry))))))

(defun my/noema-jupyter-notebook--write-raw (file document)
  "Atomically write DOCUMENT as JSON to FILE."
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-nearby-temp-file ".noema-notebook-" nil ".ipynb"))
        (serialized (my/noema-jupyter-notebook--serialize document)))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file temporary
            (insert serialized)
            (insert "\n"))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (ignore-errors (delete-file temporary))))))

(defun my/noema-jupyter-notebook--write-contents ()
  "Save current source projection as its standard notebook."
  (when (and my/noema-jupyter-notebook--projection-p
             (not my/noema-jupyter-notebook--writing))
    (let ((my/noema-jupyter-notebook--writing t))
      (my/noema-jupyter-notebook--canonicalize-managed-markers)
      ;; Noema may have persisted execution results since this projection was
      ;; opened.  Source is authoritative here, but outputs/counts/private
      ;; runtime metadata are authoritative on disk.
      ;;
      ;; A file that is gone has nothing to merge, so the in-memory document
      ;; stands.  A file that exists but cannot be read or parsed is a
      ;; different matter: falling back to the opened-at document would write
      ;; it back out and silently destroy every result Noema has persisted
      ;; since.  Refuse the save instead and let the user decide.
      (my/noema-jupyter-notebook--sync-document
       (if (file-exists-p buffer-file-name)
           (condition-case error
               (my/noema-jupyter-notebook--read-raw buffer-file-name)
             (error
              (user-error
               "Refusing to save: %s exists but cannot be read (%s); saving now would discard results Noema wrote since this buffer was opened"
               (abbreviate-file-name buffer-file-name)
               (error-message-string error))))
         my/noema-jupyter-notebook--document))
      (my/noema-jupyter-notebook--write-raw
       buffer-file-name my/noema-jupyter-notebook--document)
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))
    t))

(defun my/noema-jupyter-notebook--install-projection (document)
  "Install DOCUMENT as current buffer's language projection."
  (let* ((language (my/noema-jupyter-notebook--language document))
         (mode (my/noema-jupyter-notebook--major-mode language)))
    (my/noema-jupyter-notebook--render document)
    (funcall mode)
    (setq-local my/noema-jupyter-notebook--document document)
    (setq-local my/noema-jupyter-notebook--projection-p t)
    (setq-local my/noema-jupyter-notebook--comment-prefix
                (my/noema-jupyter-notebook--prefix language))
    (setq-local my/noema-jupyter-notebook--editor-mode mode)
    (add-hook 'write-contents-functions
              #'my/noema-jupyter-notebook--write-contents nil t)
    (setq-local revert-buffer-function
                #'my/noema-jupyter-notebook--revert-buffer)
    (setq-local buffer-read-only nil)
    (set-buffer-modified-p nil)))

(defun my/noema-jupyter-notebook-switch-editor-mode (mode)
  "Switch the projected notebook to major MODE without changing its kernel.
The source projection, save/revert integration, and Noema UI are restored
after MODE resets buffer-local state.  This is the Emacs-side LSP switch: mode
hooks decide whether Eglot or another language client is enabled."
  (interactive
   (list (intern (completing-read "Notebook editor mode: "
                                  (mapcar
                                   #'symbol-name
                                   (seq-filter
                                    #'fboundp
                                    '(python-ts-mode python-mode sh-mode
                                      js-ts-mode typescript-ts-mode
                                      lean4-mode emacs-lisp-mode text-mode
                                      fundamental-mode)))
                                  nil t))))
  (unless my/noema-jupyter-notebook--projection-p
    (user-error "This buffer is not an ipynb source projection"))
  (unless (fboundp mode)
    (user-error "Mode is unavailable: %s" mode))
  (let ((document my/noema-jupyter-notebook--document)
        (prefix my/noema-jupyter-notebook--comment-prefix)
        (position (point))
        (modified (buffer-modified-p)))
    (when (and (bound-and-true-p my/noema-jupyter-cell-mode)
               (fboundp 'my/noema-jupyter-cell-mode))
      ;; Let the minor mode remove its overlay and buffer-local hooks before
      ;; the major mode resets locals; otherwise the old marker overlay leaks.
      (my/noema-jupyter-cell-mode -1))
    (funcall mode)
    (setq-local my/noema-jupyter-notebook--document document)
    (setq-local my/noema-jupyter-notebook--projection-p t)
    (setq-local my/noema-jupyter-notebook--comment-prefix prefix)
    (setq-local my/noema-jupyter-notebook--editor-mode mode)
    (add-hook 'write-contents-functions
              #'my/noema-jupyter-notebook--write-contents nil t)
    (setq-local revert-buffer-function
                #'my/noema-jupyter-notebook--revert-buffer)
    (setq-local buffer-read-only nil)
    (when (fboundp 'my/noema-jupyter-cell-activate-buffer)
      (my/noema-jupyter-cell-activate-buffer))
    (goto-char (min position (point-max)))
    (set-buffer-modified-p modified)
    mode))

(defun my/noema-jupyter-notebook--revert-buffer (_ignore-auto noconfirm)
  "Reload the backing ipynb JSON and restore its source projection.
NOCONFIRM has the meaning documented by `revert-buffer'.  Reading the file
explicitly avoids asking Emacs' ordinary text-file revert path to understand a
buffer whose visible source intentionally differs from its JSON on disk."
  (when (and (buffer-modified-p)
             (not noconfirm)
             (not (yes-or-no-p "Discard unsaved notebook source changes? ")))
    (user-error "Notebook revert cancelled"))
  (let* ((bounds (and (fboundp 'my/noema-jupyter-cell--bounds-at-point)
                      (my/noema-jupyter-cell--bounds-at-point)))
         (cell-id (plist-get bounds :id))
         (offset (and bounds
                      (- (point) (plist-get bounds :body-beg))))
         (document (my/noema-jupyter-notebook--read-raw buffer-file-name)))
    (run-hooks 'before-revert-hook)
    (my/noema-jupyter-notebook--install-projection document)
    (when (fboundp 'my/noema-jupyter-cell-activate-buffer)
      (my/noema-jupyter-cell-activate-buffer)
      (when (and cell-id
                 (fboundp 'my/noema-jupyter-cell--goto-id)
                 (my/noema-jupyter-cell--goto-id cell-id)
                 offset)
        (goto-char (min (+ (point) (max 0 offset))
                        (or (plist-get
                             (my/noema-jupyter-cell--bounds-at-point)
                             :body-end)
                            (point-max))))))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    (run-hooks 'after-revert-hook)
    t))

;;;###autoload
(defun my/noema-jupyter-notebook-open-mode ()
  "Visit an ipynb as a native source projection.
Noema-owned notebooks also install the lightweight Emacs management UI; that
UI talks only to Noema and never opens a Jupyter protocol connection."
  (interactive)
  (let* ((raw (json-parse-string
               (buffer-string) :object-type 'hash-table :array-type 'array
               :null-object nil :false-object :json-false))
         (managed (my/noema-jupyter-notebook--noema-standard-p raw))
         (upgrade-ids (and managed
                           (my/noema-jupyter-notebook--cell-ids-need-upgrade-p
                            raw)))
         (document (my/noema-jupyter-notebook--normalize raw)))
    (my/noema-jupyter-notebook--install-projection document)
    (when (and upgrade-ids buffer-file-name)
      (condition-case err
          (progn
            (my/noema-jupyter-notebook--write-raw buffer-file-name document)
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (message "Noema Jupyter: added stable cell ids to %s"
                     (file-name-nondirectory buffer-file-name)))
        (file-error
         (set-buffer-modified-p t)
         (message "Noema Jupyter: cell ids need saving: %s"
                  (error-message-string err))))))
  (when (fboundp 'my/noema-jupyter-cell-activate-buffer)
    (my/noema-jupyter-cell-activate-buffer)))

(defun my/noema-jupyter-notebook-read (file)
  "Return FILE's notebook object, including unsaved projected source."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (if my/noema-jupyter-notebook--projection-p
            (my/noema-jupyter-notebook--sync-document)
          (my/noema-jupyter-notebook--read-raw file)))
    (my/noema-jupyter-notebook--read-raw file)))

(defun my/noema-jupyter-notebook-write (file document)
  "Write DOCUMENT to FILE and update an existing projection model."
  (my/noema-jupyter-notebook--normalize document)
  (my/noema-jupyter-notebook--write-raw file document)
  (when-let* ((buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (when my/noema-jupyter-notebook--projection-p
        (setq-local my/noema-jupyter-notebook--document document)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))))
  document)

(defun my/noema-jupyter-notebook-metadata (file-or-document)
  "Return Noema metadata plist from FILE-OR-DOCUMENT."
  (let* ((document (if (hash-table-p file-or-document)
                       file-or-document
                     (my/noema-jupyter-notebook-read file-or-document)))
         (metadata (gethash "metadata" document))
         (noema (my/noema-jupyter-notebook--get 'noema metadata))
         (kernelspec (my/noema-jupyter-notebook--get 'kernelspec metadata))
         (language (my/noema-jupyter-notebook--language document)))
    (list :source (format "%s" (or (my/noema-jupyter-notebook--get
                                     'source_file noema) ""))
          :kernel (format "%s" (or (my/noema-jupyter-notebook--get
                                     'name kernelspec) "python3"))
          :session (format "%s" (or (my/noema-jupyter-notebook--get
                                      'session noema) "default"))
          :language language
          :storage "ipynb")))

(defun my/noema-jupyter-notebook-cells (file)
  "Return ordered projected cells for notebook FILE."
  (let ((document (my/noema-jupyter-notebook-read file)))
    (if-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (my/noema-jupyter-notebook-projection-cells document))
      (with-temp-buffer
        (setq-local my/noema-jupyter-notebook--document document)
        (setq-local my/noema-jupyter-notebook--comment-prefix
                    (my/noema-jupyter-notebook--prefix
                     (my/noema-jupyter-notebook--language document)))
        (my/noema-jupyter-notebook--render document)
        (my/noema-jupyter-notebook-projection-cells document)))))

(defun my/noema-jupyter-notebook-output-mirror (document)
  "Return engine-compatible output projection for DOCUMENT."
  (let (cells)
    (dolist (cell (append (gethash "cells" document) nil))
      (when (equal (gethash "cell_type" cell) "code")
        (let* ((metadata (gethash "metadata" cell))
               (private (my/noema-jupyter-notebook--get 'noema metadata))
               (execution-count (gethash "execution_count" cell))
               (outputs (vconcat (or (gethash "outputs" cell) nil))))
          (when (or private execution-count outputs)
            (let ((saved (copy-hash-table
                          (my/noema-jupyter-notebook--hash private))))
              (puthash "executionCount" execution-count saved)
              (puthash "outputs" outputs saved)
              (push (cons (intern (gethash "id" cell)) saved) cells))))))
    `((version . 2) (cells . ,(nreverse cells)))))

(defun my/noema-jupyter-notebook-apply-output-mirror (document mirror)
  "Apply engine MIRROR to DOCUMENT's standard output fields."
  (let ((saved-cells (my/noema-jupyter-notebook--get 'cells mirror)))
    (dolist (cell (append (gethash "cells" document) nil))
      (when (equal (gethash "cell_type" cell) "code")
        (let* ((id (gethash "id" cell))
               (present (cond
                         ((hash-table-p saved-cells)
                          (or (gethash id saved-cells)
                              (gethash (intern-soft id) saved-cells)))
                         ((listp saved-cells)
                          (seq-find (lambda (entry)
                                      (equal (format "%s" (car entry)) id))
                                    saved-cells))))
               (saved (if (consp present) (cdr present) present))
               (metadata (my/noema-jupyter-notebook--hash
                          (gethash "metadata" cell))))
          (if saved
              (let ((private (my/noema-jupyter-notebook--hash saved)))
                (puthash "execution_count"
                         (my/noema-jupyter-notebook--get 'executionCount saved) cell)
                (puthash "outputs"
                         (vconcat (or (my/noema-jupyter-notebook--get
                                       'outputs saved) nil)) cell)
                (remhash "executionCount" private)
                (remhash "outputs" private)
                (remhash "widgetRuntime" private)
                (puthash "noema" private metadata))
            (puthash "execution_count" nil cell)
            (puthash "outputs" [] cell)
            (remhash "noema" metadata))
          (puthash "metadata" metadata cell))))
  document))

(defun my/noema-jupyter-notebook-set-kernel (file name)
  "Set notebook FILE's kernelspec NAME."
  (let* ((document (my/noema-jupyter-notebook-read file))
         (metadata (gethash "metadata" document))
         (kernelspec (my/noema-jupyter-notebook--hash
                      (gethash "kernelspec" metadata))))
    (puthash "name" name kernelspec)
    (puthash "display_name" name kernelspec)
    (puthash "kernelspec" kernelspec metadata)
    (my/noema-jupyter-notebook-write file document)))

;; This association intentionally takes precedence over code-cells/Jupytext.
(setq auto-mode-alist
      (cons '("\\.ipynb\\'" . my/noema-jupyter-notebook-open-mode)
            (cl-remove-if (lambda (entry)
                            (and (consp entry)
                                 (stringp (car entry))
                                 (string-match-p "ipynb" (car entry))))
                          auto-mode-alist)))

(with-eval-after-load 'code-cells
  (setq auto-mode-alist
        (cons '("\\.ipynb\\'" . my/noema-jupyter-notebook-open-mode)
              (cl-remove-if
               (lambda (entry)
                 (and (consp entry)
                      (stringp (car entry))
                      (string-match-p "ipynb" (car entry))))
               auto-mode-alist))))

(provide 'init-aaronnote-jupyter-notebook)

;;; init-aaronnote-jupyter-notebook.el ends here
