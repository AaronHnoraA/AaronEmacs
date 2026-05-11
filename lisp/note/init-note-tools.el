;;; init-note-tools.el --- Note utility commands -*- lexical-binding: t -*-

;;; Commentary:
;; Utility commands shared by the note workflow: Zotero metadata filling and
;; clipboard image insertion.  Keep these separate from the Typst note index.

;;; Code:

(require 'bibtex)
(require 'init-note)
(require 'subr-x)

(defcustom my/note-image-root "img"
  "Directory, relative to the current note file, where pasted images are stored."
  :type 'string
  :group 'my/note)

(defun my/note-tools--buffer-basename ()
  "Return a stable basename for the current note buffer."
  (file-name-base
   (or (buffer-file-name (buffer-base-buffer))
       (buffer-name (buffer-base-buffer)))))

(defun my/note-image-directory ()
  "Return the image directory used for pasted images in the current note."
  (file-name-concat my/note-image-root
                    (my/note-tools--buffer-basename)))

(defun my/note-tools--timestamped-image-file (&optional extension)
  "Return a non-conflicting image file path for the current note."
  (unless buffer-file-name
    (user-error "Save this note before pasting an image"))
  (let* ((directory (expand-file-name
                     (my/note-image-directory)
                     (file-name-directory buffer-file-name)))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (extension (or extension "png"))
         (candidate (expand-file-name
                     (format "%s.%s" timestamp extension)
                     directory))
         (index 2))
    (make-directory directory t)
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name
                       (format "%s-%d.%s" timestamp index extension)
                       directory)
            index (1+ index)))
    candidate))

;;;###autoload
(defun my/note-paste-image-clipboard ()
  "Paste the macOS clipboard image into the current Typst note."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "Clipboard image paste is configured for macOS only"))
  (unless (executable-find "pngpaste")
    (user-error "Install pngpaste first: brew install pngpaste"))
  (unless (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode)
    (user-error "Clipboard image paste supports Typst note buffers"))
  (let* ((file (my/note-tools--timestamped-image-file "png"))
         (status (call-process "pngpaste" nil nil nil file)))
    (unless (and (integerp status)
                 (zerop status)
                 (file-exists-p file)
                 (> (file-attribute-size (file-attributes file)) 0))
      (user-error "No PNG image found in clipboard"))
    (insert (format "#image(%S)"
                    (file-relative-name
                     file
                     (file-name-directory buffer-file-name))))
    (message "Inserted image: %s" file)))

(defun my/note-zotero--org-bibtex-block ()
  "Return the first Org BibTeX source block body, or nil."
  (let ((case-fold-search t)
        start)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+begin_src[ \t]+bibtex\\b.*$" nil t)
        (setq start (line-beginning-position 2))
        (when (re-search-forward "^[ \t]*#\\+end_src\\b" nil t)
          (buffer-substring-no-properties start (match-beginning 0)))))))

(defun my/note-zotero--markdown-bibtex-block ()
  "Return the first Markdown BibTeX code block body, or nil."
  (let ((case-fold-search t)
        start)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*```[ \t]*bibtex[ \t]*$" nil t)
        (setq start (line-beginning-position 2))
        (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
          (buffer-substring-no-properties start (match-beginning 0)))))))

(defun my/note-zotero--bibtex-content ()
  "Return BibTeX content from the current buffer."
  (or (my/note-zotero--org-bibtex-block)
      (my/note-zotero--markdown-bibtex-block)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "@[[:alpha:]]+[[:space:]]*[({]" nil t)
          (buffer-substring-no-properties (match-beginning 0) (point-max))))
      (user-error "No BibTeX entry found")))

(defun my/note-zotero--clean-field (entries field)
  "Return cleaned FIELD from BibTeX ENTRIES."
  (when-let* ((entry (assoc field entries))
              (value (cdr entry)))
    (setq value (string-trim value))
    (when (and (string-prefix-p "{" value)
               (string-suffix-p "}" value))
      (setq value (substring value 1 -1)))
    (when (and (string-prefix-p "\"" value)
               (string-suffix-p "\"" value))
      (setq value (substring value 1 -1)))
    value))

(defun my/note-zotero--metadata ()
  "Return metadata plist parsed from the first BibTeX entry in this buffer."
  (let ((content (my/note-zotero--bibtex-content))
        entries
        key)
    (with-temp-buffer
      (bibtex-mode)
      (insert content)
      (goto-char (point-min))
      (bibtex-skip-to-valid-entry)
      (setq key (bibtex-key-in-head)
            entries (bibtex-parse-entry)))
    (list :title (my/note-zotero--clean-field entries "title")
          :author (my/note-zotero--clean-field entries "author")
          :year (or (my/note-zotero--clean-field entries "year")
                    (my/note-zotero--clean-field entries "date"))
          :citekey key
          :doi (my/note-zotero--clean-field entries "doi"))))

;;;###autoload
(defun my/note-zotero-fill-metadata ()
  "Fill Zotero/BibTeX placeholders in the current note buffer.
The command replaces ${title}, ${author}, ${year}, ${citekey}, and ${doi}
using the first BibTeX entry found in the buffer."
  (interactive)
  (let ((metadata (my/note-zotero--metadata)))
    (save-excursion
      (dolist (field '(:title :author :year :citekey :doi))
        (let ((tag (format "${%s}" (substring (symbol-name field) 1)))
              (value (or (plist-get metadata field) "")))
          (goto-char (point-min))
          (while (search-forward tag nil t)
            (replace-match value t t)))))
    (message "Zotero metadata filled: %s"
             (or (plist-get metadata :title) "untitled"))))

(defun my/note-tools--setup-keys (map)
  "Bind note tool keys in MAP."
  (define-key map (kbd "C-c n y") #'my/note-paste-image-clipboard)
  (define-key map (kbd "C-c n z") #'my/note-zotero-fill-metadata))

(with-eval-after-load 'typst-ts-mode
  (when (boundp 'typst-ts-mode-map)
    (my/note-tools--setup-keys typst-ts-mode-map)))

(with-eval-after-load 'typst-mode
  (when (boundp 'typst-mode-map)
    (my/note-tools--setup-keys typst-mode-map)))

(when (boundp 'my/typst-mode-map)
  (my/note-tools--setup-keys my/typst-mode-map))

(provide 'init-note-tools)
;;; init-note-tools.el ends here
