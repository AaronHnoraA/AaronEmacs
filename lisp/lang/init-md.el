;;; init-md.el --- Open Markdown files in Aaronnote -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing is owned by Aaronnote.  Emacs only redirects opened
;; Markdown files into the local Aaronnote Web/Appine bridge.

;;; Code:

(require 'cl-lib)
(require 'init-aaronnote)

(defgroup my/markdown nil
  "Markdown file handoff to Aaronnote."
  :group 'my/aaronnote)

(defcustom my/aaronnote-close-emacs-markdown-buffer t
  "When non-nil, close the temporary Emacs buffer after opening Markdown in Aaronnote."
  :type 'boolean
  :group 'my/markdown)

(defconst my/aaronnote-markdown-auto-mode-patterns
  '("README\\(?:\\.md\\)?\\'" "\\.markdown\\'" "\\.md\\'")
  "File patterns redirected from Emacs buffers into Aaronnote.")

(defvar-local my/aaronnote--markdown-redirected nil
  "Non-nil when this buffer has already been handed to Aaronnote.")

(defun my/aaronnote--markdown-auto-mode-entry-p (entry)
  "Return non-nil when ENTRY is a Markdown auto-mode entry."
  (and (consp entry)
       (let ((pattern (car entry)))
         (and (stringp pattern)
              (or (member pattern my/aaronnote-markdown-auto-mode-patterns)
                  (string-match-p
                   "\\(?:markdown\\|md\\)"
                   pattern))))))

(defun my/aaronnote--pin-markdown-redirect-mode ()
  "Keep Markdown file patterns routed to Aaronnote ahead of other modes."
  (setq auto-mode-alist
        (append
         (mapcar (lambda (pattern)
                   (cons pattern #'my/aaronnote-markdown-redirect-mode))
                 my/aaronnote-markdown-auto-mode-patterns)
         (cl-remove-if #'my/aaronnote--markdown-auto-mode-entry-p
                       auto-mode-alist))))

(defun my/aaronnote--ensure-markdown-file (file)
  "Ensure Markdown FILE exists before Aaronnote opens it."
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region "" nil file nil 'silent))
    file))

(defun my/aaronnote--kill-redirected-markdown-buffer (buffer file)
  "Kill redirected Markdown BUFFER when it is still visiting FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-equal-p buffer-file-name file))
        (kill-buffer buffer)))))

(defun my/aaronnote--redirect-current-markdown-buffer ()
  "Open the current Markdown buffer in Aaronnote and optionally close it."
  (let ((file buffer-file-name)
        (buffer (current-buffer)))
    (unless (and file (my/aaronnote--markdown-file-p file))
      (user-error "Current buffer is not visiting a Markdown file"))
    (setq-local my/aaronnote--markdown-redirected t)
    (fundamental-mode)
    (setq-local buffer-read-only t)
    (let ((target (my/aaronnote--ensure-markdown-file file)))
      (my/aaronnote-open-file target)
      (when my/aaronnote-close-emacs-markdown-buffer
        (run-at-time 0 nil
                     #'my/aaronnote--kill-redirected-markdown-buffer
                     buffer target)))))

(defun my/aaronnote-redirect-markdown-file-h ()
  "Fallback Markdown handoff for packages that override `auto-mode-alist'."
  (when (and buffer-file-name
             (my/aaronnote--markdown-file-p buffer-file-name)
             (not my/aaronnote--markdown-redirected))
    (my/aaronnote--redirect-current-markdown-buffer)))

;;;###autoload
(defun my/aaronnote-markdown-redirect-mode ()
  "Major-mode replacement that opens the current Markdown file in Aaronnote."
  (interactive)
  (my/aaronnote--redirect-current-markdown-buffer))

(my/aaronnote--pin-markdown-redirect-mode)

;; `treesit-auto' adds its own Markdown entries during configuration.  Re-pin
;; the Aaronnote handoff after that package loads so no grammar prompt runs.
(with-eval-after-load 'treesit-auto
  (my/aaronnote--pin-markdown-redirect-mode))

(with-eval-after-load 'init-treesit
  (my/aaronnote--pin-markdown-redirect-mode))

(add-hook 'after-init-hook #'my/aaronnote--pin-markdown-redirect-mode 90)
(add-hook 'find-file-hook #'my/aaronnote-redirect-markdown-file-h)

(provide 'init-md)

;;; init-md.el ends here
