;;; init-aaronnote-jupyter-notebook.el --- Native ipynb source projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Visit an ipynb as an ordinary language buffer without asking Jupytext to
;; round-trip it.  The buffer is a transient percent-style source projection;
;; the file on disk always remains a standard nbformat 4.5 notebook.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defvar-local my/noema-jupyter-notebook--document nil)
(defvar-local my/noema-jupyter-notebook--projection-p nil)
(defvar-local my/noema-jupyter-notebook--comment-prefix "#")
(defvar-local my/noema-jupyter-notebook--writing nil)

(defconst my/noema-jupyter-notebook--marker-re
  "^[ \t]*\\(?://\\|--\\|#\\|;\\)[ \t]*%%\\(?:[ \t]+\\[\\(markdown\\|raw\\)\\]\\)?[ \t]+id=\\([A-Za-z0-9_-]+\\)[ \t]*$")

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

(defun my/noema-jupyter-notebook--normalize (document)
  "Normalize DOCUMENT to nbformat 4.5 while retaining unknown fields."
  (unless (hash-table-p document)
    (error "Jupyter notebook root must be an object"))
  (let* ((metadata (my/noema-jupyter-notebook--hash
                    (gethash "metadata" document)))
         (seen (make-hash-table :test #'equal))
         normalized)
    (cl-loop for raw in (append (or (gethash "cells" document) nil) nil)
             for index from 1
             do
             (let* ((cell (my/noema-jupyter-notebook--hash raw))
                    (base (my/noema-jupyter-notebook--valid-id
                           (gethash "id" cell) (format "cell-%d" index)))
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
             (max 5 (or (gethash "nbformat_minor" document) 0)) document)
    document))

(defun my/noema-jupyter-notebook--parse-string (text)
  "Parse notebook JSON TEXT."
  (my/noema-jupyter-notebook--normalize
   (json-parse-string text :object-type 'hash-table :array-type 'array
                      :null-object nil :false-object :json-false)))

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
    (downcase (format "%s" (or (my/noema-jupyter-notebook--get
                                 'name language-info)
                                (my/noema-jupyter-notebook--get
                                 'language kernelspec)
                                "python")))))

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
         (tag (pcase type ("markdown" " [markdown]") ("raw" " [raw]") (_ ""))))
    (insert prefix " %%" tag " id=" (gethash "id" cell) "\n")
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

(defun my/noema-jupyter-notebook-projection-cells (&optional document)
  "Return ordered projected cell plists for current buffer.
DOCUMENT supplies the original cell objects retained by id."
  (let ((document (or document my/noema-jupyter-notebook--document))
        (old (make-hash-table :test #'equal))
        markers result)
    (dolist (cell (append (and document (gethash "cells" document)) nil))
      (puthash (gethash "id" cell) cell old))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward my/noema-jupyter-notebook--marker-re nil t)
          (push (list :id (match-string-no-properties 2)
                      :type (or (match-string-no-properties 1) "code")
                      :block-beg (line-beginning-position)
                      :start-beg (line-beginning-position)
                      :start-end (line-end-position)
                      :body-beg (progn (forward-line 1) (point)))
                markers)))
      (setq markers (nreverse markers))
      (cl-loop for marker on markers
               for entry = (car marker)
               for next = (cadr marker)
               for raw-end = (or (plist-get next :block-beg) (point-max))
               for body-end = (my/noema-jupyter-notebook--body-end
                               (plist-get entry :body-beg) raw-end)
               for id = (plist-get entry :id)
               for type = (plist-get entry :type)
               for text = (buffer-substring-no-properties
                            (plist-get entry :body-beg) body-end)
               for source = (if (member type '("markdown" "raw"))
                                (my/noema-jupyter-notebook--uncomment
                                 text my/noema-jupyter-notebook--comment-prefix)
                              text)
               do (push (append entry
                                (list :body-end body-end
                                      :end-beg body-end :end-end raw-end
                                      :block-end raw-end
                                      :line (line-number-at-pos
                                             (plist-get entry :body-beg))
                                      :code source :source source
                                      :cell (gethash id old)))
                        result)))
    (nreverse result)))

(defun my/noema-jupyter-notebook--sync-document ()
  "Merge current source projection into its notebook object."
  (let (cells)
    (dolist (entry (my/noema-jupyter-notebook-projection-cells
                    my/noema-jupyter-notebook--document))
      (let* ((old (or (plist-get entry :cell)
                      (make-hash-table :test #'equal)))
             (type (plist-get entry :type)))
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
    (puthash "cells" (vconcat (nreverse cells)) my/noema-jupyter-notebook--document)
    (my/noema-jupyter-notebook--normalize
     my/noema-jupyter-notebook--document)))

(defun my/noema-jupyter-notebook--write-raw (file document)
  "Atomically write DOCUMENT as JSON to FILE."
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-nearby-temp-file ".noema-notebook-" nil ".ipynb")))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file temporary
            (insert (json-serialize document :null-object nil
                                    :false-object :json-false))
            (insert "\n"))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (ignore-errors (delete-file temporary))))))

(defun my/noema-jupyter-notebook--write-contents ()
  "Save current source projection as its standard notebook."
  (when (and my/noema-jupyter-notebook--projection-p
             (not my/noema-jupyter-notebook--writing))
    (let ((my/noema-jupyter-notebook--writing t))
      (my/noema-jupyter-notebook--sync-document)
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
    (add-hook 'write-contents-functions
              #'my/noema-jupyter-notebook--write-contents nil t)
    (add-hook 'after-revert-hook
              #'my/noema-jupyter-notebook--after-revert nil t)
    (setq-local buffer-read-only nil)
    (set-buffer-modified-p nil)))

(defun my/noema-jupyter-notebook--after-revert ()
  "Turn raw JSON inserted by `revert-buffer' back into source."
  (when (and buffer-file-name (string-match-p "\\.ipynb\\'" buffer-file-name))
    (let ((document (my/noema-jupyter-notebook--parse-string (buffer-string))))
      (my/noema-jupyter-notebook--install-projection document))))

;;;###autoload
(defun my/noema-jupyter-notebook-open-mode ()
  "Visit an ipynb as a native source projection."
  (interactive)
  (my/noema-jupyter-notebook--install-projection
   (my/noema-jupyter-notebook--parse-string (buffer-string))))

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
