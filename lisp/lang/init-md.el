;;; init-md.el --- Open Markdown files in Aaronnote -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing is owned by Aaronnote.  Emacs redirects opened Markdown
;; files into the local Aaronnote Web/Appine bridge for interactive opens, and
;; falls back to a raw text buffer for programmatic callers (magit, grep, xref).

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

(defvar my/aaronnote--inhibit-redirect nil
  "Non-nil inhibits the markdown-to-Aaronnote redirect.
Bind this to t around programmatic find-file calls (e.g. in magit hooks,
test helpers) where you want a raw Emacs buffer instead of the web editor.")

(defconst my/aaronnote--redirect-commands
  '(find-file find-file-other-window find-file-other-frame
    find-file-literally find-alternate-file
    dired-find-file dired-find-file-other-window dired-view-file)
  "Interactive commands from which Markdown opens should redirect to Aaronnote.")

(defun my/aaronnote--should-redirect-p ()
  "Return non-nil when the current open warrants an Aaronnote redirect."
  (and (not noninteractive)
       (not my/aaronnote--inhibit-redirect)
       ;; Block known programmatic callers that are not user-facing file opens.
       (not (memq this-command
                  '(magit-find-file magit-find-file-other-window
                    magit-diff-visit-file magit-diff-visit-worktree-file
                    xref-goto-xref
                    next-error previous-error
                    org-open-at-point)))))

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
             (not my/aaronnote--markdown-redirected)
             (my/aaronnote--should-redirect-p))
    (my/aaronnote--redirect-current-markdown-buffer)))

;;;###autoload
(defun my/aaronnote-markdown-redirect-mode ()
  "Major-mode replacement that opens the current Markdown file in Aaronnote.
For programmatic opens (magit, grep, org-link), falls back to a raw text mode."
  (interactive)
  (if (my/aaronnote--should-redirect-p)
      (my/aaronnote--redirect-current-markdown-buffer)
    ;; Programmatic open: use a raw markdown view without redirecting.
    (if (require 'markdown-mode nil t)
        (gfm-mode)
      (text-mode))))

;;;###autoload
(defun my/aaronnote-open-markdown-raw (&optional file)
  "Open FILE (or the current note) as a raw Markdown buffer in Emacs.
Bypasses the Aaronnote redirect.  Useful for diffing, fixing broken notes,
or editing when the web editor is unavailable."
  (interactive
   (list (read-file-name "Raw Markdown: " nil
                         (or (my/aaronnote-buffer-file) buffer-file-name)
                         t)))
  (let ((my/aaronnote--inhibit-redirect t)
        (target (expand-file-name
                 (or file
                     (my/aaronnote-buffer-file)
                     (and buffer-file-name
                          (my/aaronnote--markdown-file-p buffer-file-name)
                          buffer-file-name)
                     (user-error "No Markdown file to open")))))
    (unless (my/aaronnote--markdown-file-p target)
      (user-error "Not a Markdown file: %s" target))
    (find-file target)))

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
