;;; init-org-utility.el --- Org utility commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Small Org helpers that do not belong to agenda, capture, roam, or export.

;;; Code:

(require 'init-org-core)
(require 'subr-x)

(defgroup my/org-utility nil
  "Personal Org utility helpers."
  :group 'org)

(defcustom my/org-download-image-root "img"
  "Directory, relative to the Org file, where pasted images are stored."
  :type 'string
  :group 'my/org-utility)

(defvar org-download-image-dir)
(defvar org-download-timestamp)

(declare-function org-download-clipboard "org-download")
(declare-function org-download--dir "org-download")
(declare-function org-download-screenshot "org-download")

(defun my/org-download--buffer-basename ()
  "Return a stable basename for the current Org buffer."
  (file-name-base
   (or (buffer-file-name (buffer-base-buffer))
       (buffer-name (buffer-base-buffer)))))

(defun my/org-download-image-dir ()
  "Return the image directory used by `org-download' for this buffer."
  (file-name-concat my/org-download-image-root
                    (my/org-download--buffer-basename)))

(defun my/org-download-setup ()
  "Set up buffer-local `org-download' storage for Org buffers."
  (setq-local org-download-image-dir (my/org-download-image-dir)))

(defun my/org-download-file-format (filename)
  "Return a timestamped, non-conflicting image FILENAME."
  (let* ((base (concat (format-time-string org-download-timestamp) filename))
         (candidate base)
         (stem (file-name-sans-extension base))
         (ext (file-name-extension base t))
         (index 2))
    (while (file-exists-p (expand-file-name candidate (org-download--dir)))
      (setq candidate (format "%s-%d%s" stem index (or ext ""))
            index (1+ index)))
    candidate))

(defun my/org-download-clipboard ()
  "Insert a macOS clipboard image into the current Org buffer."
  (interactive)
  (require 'org-download)
  (unless (eq system-type 'darwin)
    (user-error "my/org-download-clipboard is configured for macOS only"))
  (unless (executable-find "pngpaste")
    (user-error "Install pngpaste first: brew install pngpaste"))
  (let ((org-download-screenshot-method "pngpaste %s"))
    (org-download-screenshot)))

(use-package org-download
  :ensure t
  :after org
  :hook (org-mode . my/org-download-setup)
  :custom
  (org-download-method 'directory)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-file-format-function #'my/org-download-file-format)
  :bind (:map org-mode-map
              ("C-M-y" . my/org-download-clipboard)
              ("C-c i d" . org-download-delete)))

(provide 'init-org-utility)
;;; init-org-utility.el ends here
