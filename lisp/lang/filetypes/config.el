;;; lang/filetypes/config.el --- Common file type routing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep broad file-name associations here.  Language-specific setup still lives
;; in the language modules; this module decides which viewer/editor should own
;; common data, table, database, document, and asset files.

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'subr-x)

(declare-function +clutch/query-sqlite-file "tools/clutch/config" (file))

(defgroup +filetypes nil
  "Common file type associations."
  :group 'files)

(config-defvar +filetypes-sqlite-extensions nil
  "Extensions opened with the database client."
  :type '(repeat string)
  :group '+filetypes)

(defconst +filetypes--sqlite-header "SQLite format 3"
  "SQLite database file header.")

(defun +filetypes--regexp (extensions)
  "Return an `auto-mode-alist' regexp for EXTENSIONS."
  (concat "\\.\\("
          (regexp-opt extensions)
          "\\)\\'"))

(defun +filetypes--treesit-mode-or (mode language fallback)
  "Return tree-sitter MODE for LANGUAGE when ready, otherwise FALLBACK."
  (if (and (fboundp mode)
           (fboundp 'treesit-ready-p)
           (ignore-errors (treesit-ready-p language t)))
      mode
    fallback))

(defun +filetypes-sqlite-file-p (&optional file)
  "Return non-nil when FILE or the current buffer looks like a SQLite DB."
  (let ((file (or file buffer-file-name)))
    (or (and file
             (member (downcase (or (file-name-extension file) ""))
                     +filetypes-sqlite-extensions))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (looking-at-p (regexp-quote +filetypes--sqlite-header)))))))

;;;###autoload
(defun +filetypes/open-sqlite-database ()
  "Open the current file with the configured database client."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((file buffer-file-name)
        (buffer (current-buffer)))
    (fundamental-mode)
    (setq-local buffer-read-only t)
    (set-buffer-modified-p nil)
    (+clutch/query-sqlite-file file)
    (when (and (buffer-live-p buffer)
               (not (buffer-modified-p buffer))
               (file-equal-p (buffer-file-name buffer) file))
      (kill-buffer buffer))))

(defun +filetypes--prefer-mode (patterns mode)
  "Prepend PATTERNS to `auto-mode-alist' with MODE."
  (dolist (pattern (reverse patterns))
    (setq auto-mode-alist
          (cons (cons pattern mode)
                (cl-remove-if
                 (lambda (entry)
                   (and (consp entry)
                        (equal (car entry) pattern)))
                 auto-mode-alist)))))

;; Tables.
(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode)
         ("\\.tab\\'" . csv-mode)
         ("\\.psv\\'" . csv-mode))
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-align-max-width 80)
  (csv-separators '("," "\t" "|" ";")))

;; Databases.  The header entry catches extensionless SQLite files; extension
;; entries make ordinary .db/.sqlite opens land in the DB client immediately.
(add-to-list 'magic-mode-alist
             (cons +filetypes--sqlite-header
                   #'+filetypes/open-sqlite-database))
(+filetypes--prefer-mode
 (list (+filetypes--regexp +filetypes-sqlite-extensions))
 #'+filetypes/open-sqlite-database)

;; Structured data and configuration.
(+filetypes--prefer-mode
 '("\\.json\\'" "\\.jsonc\\'" "\\.jsonl\\'")
 (+filetypes--treesit-mode-or 'json-ts-mode 'json 'js-json-mode))
(+filetypes--prefer-mode
 '("\\.ya?ml\\'")
 (+filetypes--treesit-mode-or 'yaml-ts-mode 'yaml 'yaml-mode))
(+filetypes--prefer-mode
 '("\\.toml\\'" "\\.lock\\'")
 (+filetypes--treesit-mode-or 'toml-ts-mode 'toml 'conf-toml-mode))
(+filetypes--prefer-mode
 '("\\.ini\\'" "\\.editorconfig\\'" "\\.env\\(?:\\..*\\)?\\'"
   "\\.properties\\'" "\\.service\\'" "\\.timer\\'" "\\.desktop\\'")
 #'conf-mode)

;; Web and markup.
(+filetypes--prefer-mode
 '("\\.html?\\'" "\\.xhtml\\'")
 (+filetypes--treesit-mode-or 'html-ts-mode 'html 'html-mode))
(+filetypes--prefer-mode
 '("\\.xml\\'" "\\.xsd\\'" "\\.rss\\'" "\\.atom\\'" "\\.plist\\'"
   "\\.svg\\'")
 #'nxml-mode)
(+filetypes--prefer-mode
 '("\\.css\\'" "\\.scss\\'" "\\.sass\\'")
 (+filetypes--treesit-mode-or 'css-ts-mode 'css 'css-mode))

;; Build, shell-adjacent, and query files that commonly appear outside projects.
(+filetypes--prefer-mode
 '("\\(?:\\`\\|/\\)Dockerfile\\(?:\\..*\\)?\\'" "\\.dockerfile\\'")
 (+filetypes--treesit-mode-or 'dockerfile-ts-mode 'dockerfile 'conf-mode))
(+filetypes--prefer-mode
 '("\\(?:\\`\\|/\\)Makefile\\'" "\\(?:\\`\\|/\\)GNUmakefile\\'"
   "\\(?:\\`\\|/\\)BSDmakefile\\'" "\\.mk\\'")
 #'makefile-mode)
(+filetypes--prefer-mode
 '("\\.sql\\'" "\\.ddl\\'" "\\.dml\\'")
 #'sql-mode)
(+filetypes--prefer-mode
 '("\\.log\\'" "\\.out\\'" "\\.err\\'")
 #'text-mode)

;; Binary and asset viewers.
(+filetypes--prefer-mode
 '("\\.png\\'" "\\.jpe?g\\'" "\\.gif\\'" "\\.webp\\'" "\\.bmp\\'"
   "\\.tiff?\\'" "\\.ico\\'" "\\.heic\\'")
 #'image-mode)
(+filetypes--prefer-mode
 '("\\.zip\\'" "\\.jar\\'" "\\.war\\'" "\\.ear\\'" "\\.apk\\'" "\\.epub\\'"
   "\\.rar\\'" "\\.7z\\'" "\\.tar\\'" "\\.tgz\\'" "\\.tbz2\\'" "\\.txz\\'"
   "\\.gz\\'" "\\.bz2\\'" "\\.xz\\'" "\\.zst\\'")
 #'archive-mode)
(+filetypes--prefer-mode
 '("\\.bin\\'" "\\.dat\\'" "\\.dump\\'")
 #'hexl-mode)

(provide 'lang/filetypes/config)
;;; lang/filetypes/config.el ends here
