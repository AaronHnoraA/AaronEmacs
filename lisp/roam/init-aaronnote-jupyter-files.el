;;; init-aaronnote-jupyter-files.el --- Notebook export and comparison -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'init-aaronnote-jupyter-cell)
(require 'init-jupyter-management)
(require 'remote-process)
(require 'url-util)

(declare-function ediff-buffers "ediff" (buffer-a buffer-b &optional startup-hooks job-name))
(declare-function vc-call-backend "vc-hooks" (backend function &rest args))
(declare-function vc-backend "vc-hooks" (file))
(defvar ediff-after-quit-hook-internal)
(defvar-local my/noema-jupyter-export--process nil)

(defun my/noema-jupyter-files--require-notebook ()
  "Require an ordinary notebook source projection."
  (unless (and my/noema-jupyter-notebook--projection-p buffer-file-name
               (string-suffix-p ".ipynb" buffer-file-name)
               (not (string-match-p "\\.noema\\(?:\\.ipynb\\)?\\'" buffer-file-name)))
    (user-error "Open an ordinary Jupyter notebook source buffer first")))

(defun my/noema-jupyter-files--snapshot ()
  "Combine unsaved source with the latest persisted notebook outputs."
  (my/noema-jupyter-files--require-notebook)
  (my/noema-jupyter-notebook--sync-document
   (my/noema-jupyter-notebook--read-raw buffer-file-name)))

(defun my/noema-jupyter-export--assets (document directory)
  "Read relative Markdown image assets through DIRECTORY's owning filesystem."
  (let ((assets (make-hash-table :test #'equal)) missing)
    (dolist (cell (append (gethash "cells" document) nil))
      (when (equal (gethash "cell_type" cell) "markdown")
        (let ((source (my/noema-jupyter-notebook--source (gethash "source" cell)))
              (offset 0))
          (while (string-match "!\\[[^]\n]*\\](\\(<[^>\n]+>\\|[^ )\n]+\\)\\(?:[ \t]+[^)\n]*\\)?)" source offset)
            (let* ((raw (string-trim (match-string 1 source) "<" ">"))
                   (name (decode-coding-string (url-unhex-string raw) 'utf-8)))
              (setq offset (match-end 0))
              (unless (or (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" name)
                          (file-name-absolute-p name) (member ".." (split-string name "/")))
                (condition-case nil
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally (expand-file-name name directory))
                      (puthash name (base64-encode-string (buffer-string) t) assets))
                  (file-error (cl-pushnew name missing :test #'equal)))))))))
    (cons assets missing)))

(defun my/noema-jupyter-export (format destination &optional callback)
  "Export a notebook snapshot as FORMAT to DESTINATION without running cells.
FORMAT is html, pdf or script.  Conversion runs on the Emacs client; Remote
owns reading source assets and writing DESTINATION.  CALLBACK gets an error
string or nil after the asynchronous conversion and write have finished."
  (interactive
   (progn
     (my/noema-jupyter-files--require-notebook)
     (let* ((format (completing-read "Export notebook as: " '("script" "html" "pdf") nil t))
            (extension (pcase format ("html" ".html") ("pdf" ".pdf")
                         (_ (or (my/noema-jupyter-notebook--get
                                 'file_extension (gethash "language_info"
                                                         (gethash "metadata" my/noema-jupyter-notebook--document)))
                                ".py")))))
       (list format (read-file-name "Export to: " (file-name-directory buffer-file-name)
                                    nil nil (concat (file-name-base buffer-file-name) extension))))))
  (my/noema-jupyter-files--require-notebook)
  (unless (member format '("html" "pdf" "script")) (user-error "Unsupported notebook export format"))
  (when (process-live-p my/noema-jupyter-export--process) (user-error "This notebook is already exporting"))
  (setq destination (expand-file-name destination))
  (when (or (equal destination buffer-file-name)
            (ignore-errors (file-equal-p destination buffer-file-name)))
    (user-error "Choose an export file distinct from the notebook"))
  (when (file-exists-p destination)
    (unless (yes-or-no-p (format "Replace export %s? " destination)) (user-error "Export cancelled")))
  (let* ((origin (current-buffer))
         (notebook (my/noema-jupyter-files--snapshot))
         (assets (my/noema-jupyter-export--assets notebook (file-name-directory buffer-file-name)))
         (directory (remote-with-client-environment
                      (let ((default-directory user-emacs-directory)) (make-temp-file "notebook-export-" t))))
         (input (expand-file-name "input.json" directory))
         (output (expand-file-name "output" directory))
         (destination-before (file-attributes destination))
         (name (file-name-base buffer-file-name))
         (python (or my/jupyter-board-python-command (remote-client-executable-find "python3"))))
    (condition-case err
        (progn
          (with-temp-file input
            (insert (json-serialize `((notebook . ,notebook) (name . ,name) (assets . ,(car assets)))
                                    :null-object nil :false-object :json-false)))
          (setq my/noema-jupyter-export--process
                (let ((default-directory user-emacs-directory))
                (remote-exec-async
                 python :args (list (expand-file-name "etc/jupyter/export-notebook.py" user-emacs-directory)
                                    format input output)
                 :context (remote-context user-emacs-directory) :filesystem-effects 'content
                 :name "notebook-export" :coding 'utf-8-unix
                 :callback
                 (lambda (result)
                   (let (failure)
                     (unwind-protect
                         (condition-case write-error
                             (if (zerop (remote-exec-result-status result))
                                 (progn
                                   (unless (equal destination-before (file-attributes destination))
                                     (error "Export destination changed during conversion; choose a new destination"))
                                   (with-temp-buffer
                                     (set-buffer-multibyte nil)
                                     (insert-file-contents-literally output)
                                     (let ((coding-system-for-write 'no-conversion))
                                       (write-region (point-min) (point-max) destination nil 'silent)))
                                   (message "Exported %s%s" destination
                                            (if (cdr assets) (format " (%d linked images missing)" (length (cdr assets))) "")))
                               (setq failure (string-trim (remote-exec-result-stderr result)))
                               (when (string-empty-p failure)
                                 (setq failure (format "Notebook converter exited with status %s"
                                                       (remote-exec-result-status result)))))
                           (error (setq failure (error-message-string write-error))))
                       (delete-directory directory t)
                       (when (buffer-live-p origin)
                         (with-current-buffer origin (setq my/noema-jupyter-export--process nil))))
                     (when failure
                       (with-current-buffer (get-buffer-create "*Notebook Export*")
                         (let ((inhibit-read-only t)) (erase-buffer) (insert failure) (special-mode))
                         (display-buffer (current-buffer)))
                       (message "Notebook export failed; see *Notebook Export*"))
                     (when callback (funcall callback failure)))))))
          (message "Exporting notebook to %s…" format)
          my/noema-jupyter-export--process)
      (error (delete-directory directory t) (signal (car err) (cdr err))))))

(defun my/noema-jupyter-diff--canonical (value)
  "Make JSON VALUE independent of object key order."
  (cond
   ((hash-table-p value)
    (let ((result (make-hash-table :test #'equal)))
      (dolist (key (sort (hash-table-keys value) #'string<) result)
        (puthash key (my/noema-jupyter-diff--canonical (gethash key value)) result))))
   ((vectorp value) (vconcat (mapcar #'my/noema-jupyter-diff--canonical value)))
   (t value)))

(defun my/noema-jupyter-diff--json (value)
  "Return stable pretty JSON for VALUE."
  (with-temp-buffer
    (insert (json-serialize (my/noema-jupyter-diff--canonical value) :null-object nil :false-object :json-false))
    (json-pretty-print-buffer)
    (buffer-string)))

(defun my/noema-jupyter-diff--text (document components)
  "Render DOCUMENT as cell sections with optional COMPONENTS.
COMPONENTS may include outputs and metadata."
  (with-temp-buffer
    (when (member "metadata" components)
      (insert "Notebook metadata\n" (my/noema-jupyter-diff--json (gethash "metadata" document)) "\n\n"))
    (cl-loop for cell across (gethash "cells" document) for index from 1 do
             (insert (format "Cell %s [%s]\n" (or (gethash "id" cell) index) (gethash "cell_type" cell)))
             (insert (my/noema-jupyter-notebook--source (gethash "source" cell)) "\n")
             (when (member "outputs" components)
               (when (equal (gethash "cell_type" cell) "code")
                 (insert "Outputs\n" (my/noema-jupyter-diff--json (or (gethash "outputs" cell) [])) "\n"))
               (when (gethash "attachments" cell)
                 (insert "Attachments\n" (my/noema-jupyter-diff--json (gethash "attachments" cell)) "\n")))
             (when (member "metadata" components)
               (insert "Metadata\n" (my/noema-jupyter-diff--json (gethash "metadata" cell)) "\n"))
             (insert "\n"))
    (buffer-string)))

(defun my/noema-jupyter-compare (baseline &optional components)
  "Compare this notebook with BASELINE: saved, Git revision, or another file.
With a prefix argument, choose whether outputs and metadata are included."
  (interactive
   (list (completing-read "Compare notebook with: " '("Saved notebook" "Another notebook" "Git revision") nil t)
         (when current-prefix-arg
           (completing-read-multiple "Include components (source always included): " '("outputs" "metadata") nil t))))
  (my/noema-jupyter-files--require-notebook)
  (let* ((current (my/noema-jupyter-files--snapshot))
         (before
          (pcase baseline
            ("Saved notebook" (my/noema-jupyter-notebook--read-raw buffer-file-name))
            ("Another notebook" (my/noema-jupyter-notebook--read-raw
                                 (read-file-name "Compare notebook: " (file-name-directory buffer-file-name) nil t)))
            ("Git revision"
             (unless (remote-routes "exec" 'process-sync (remote-context buffer-file-name))
               (user-error "This filesystem has no Git process access; compare with another notebook instead"))
             (require 'vc)
             (let ((file buffer-file-name) (revision (read-string "Git revision: " "HEAD")))
               (with-temp-buffer
                 (vc-call-backend (vc-backend file) 'find-revision file revision (current-buffer))
                 (json-parse-string (buffer-string) :null-object nil :false-object :json-false))))
            (_ (user-error "Unknown notebook comparison"))))
         (buffers (list (generate-new-buffer "*Notebook Before*") (generate-new-buffer "*Notebook Current*"))))
    (cl-mapc (lambda (buffer document)
               (with-current-buffer buffer
                 (setq default-directory user-emacs-directory)
                 (insert (my/noema-jupyter-diff--text document components))
                 (special-mode))) buffers (list before current))
    (require 'ediff)
    (ediff-buffers
     (car buffers) (cadr buffers)
     (list (lambda ()
             (add-hook 'ediff-after-quit-hook-internal
                       (lambda () (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) buffers)) nil t))))))

(keymap-set my/noema-jupyter-cell-mode-map "C-c i e" #'my/noema-jupyter-export)
(keymap-set my/noema-jupyter-cell-mode-map "C-c i =" #'my/noema-jupyter-compare)
(provide 'init-aaronnote-jupyter-files)
;;; init-aaronnote-jupyter-files.el ends here
