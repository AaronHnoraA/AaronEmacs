;;; init-md.el --- Open Markdown files in Noema -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing is owned by Noema.  Emacs redirects opened Markdown
;; files into the local Noema Web/Appine bridge for interactive opens, and
;; falls back to a raw text buffer for programmatic callers (magit, grep, xref).

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'init-aaronnote)

(my/package-ensure-vc
 'markdown-mode
 "https://github.com/jrblevin/markdown-mode.git"
 "f441e8bc9951e73b12c61e9198658488dd8e86e1")

(defgroup my/markdown nil
  "Markdown file handoff to Noema."
  :group 'my/noema)

(config-defvar my/noema-close-emacs-markdown-buffer nil
  "When non-nil, close the temporary Emacs buffer after opening Markdown in Noema."
  :type 'boolean
  :group 'my/markdown)

(defconst my/noema-markdown-auto-mode-patterns
  '("README\\(?:\\.md\\)?\\'" "\\.markdown\\'" "\\.md\\'")
  "File patterns redirected from Emacs buffers into Noema.")

(defvar-local my/noema--markdown-redirected nil
  "Non-nil when this buffer has already been handed to Noema.")

(defvar my/noema--inhibit-redirect nil
  "Non-nil inhibits the markdown-to-Noema redirect.
Bind this to t around programmatic find-file calls (e.g. in magit hooks,
test helpers) where you want a raw Emacs buffer instead of the web editor.")

(defconst my/noema--redirect-commands
  '(find-file find-file-other-window find-file-other-frame
    find-file-literally find-alternate-file
    dired-find-file dired-find-file-other-window dired-view-file)
  "Interactive commands from which Markdown opens should redirect to Noema.")

(defun my/noema--should-redirect-p ()
  "Return non-nil when the current open warrants an Noema redirect."
  (and (not noninteractive)
       (not my/noema--inhibit-redirect)
       ;; Block known programmatic callers that are not user-facing file opens.
       (not (memq this-command
                  '(magit-find-file magit-find-file-other-window
                    magit-diff-visit-file magit-diff-visit-worktree-file
                    xref-goto-xref
                    next-error previous-error
                    org-open-at-point)))))

(defun my/noema--markdown-auto-mode-entry-p (entry)
  "Return non-nil when ENTRY is a Markdown auto-mode entry."
  (and (consp entry)
       (let ((pattern (car entry)))
         (and (stringp pattern)
              (or (member pattern my/noema-markdown-auto-mode-patterns)
                  (string-match-p
                   "\\(?:markdown\\|md\\)"
                   pattern))))))

(defun my/noema--pin-markdown-redirect-mode ()
  "Keep Markdown file patterns routed to Noema ahead of other modes."
  (setq auto-mode-alist
        (append
         (mapcar (lambda (pattern)
                   (cons pattern #'my/noema-markdown-redirect-mode))
                 my/noema-markdown-auto-mode-patterns)
         (cl-remove-if #'my/noema--markdown-auto-mode-entry-p
                       auto-mode-alist))))

(defun my/noema--ensure-markdown-file (file)
  "Ensure Markdown FILE exists before Noema opens it."
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region "" nil file nil 'silent))
    file))

(defun my/noema--kill-redirected-markdown-buffer (buffer file)
  "Kill redirected Markdown BUFFER when it is still visiting FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-equal-p buffer-file-name file))
        (kill-buffer buffer)))))

(defun my/noema--redirect-current-markdown-buffer ()
  "Open the current Markdown buffer in Noema and optionally close it."
  (let ((file buffer-file-name)
        (buffer (current-buffer)))
    (unless (and file (my/noema--markdown-file-p file))
      (user-error "Current buffer is not visiting a Markdown file"))
    (setq-local my/noema--markdown-redirected t)
    (fundamental-mode)
    (setq-local buffer-read-only t)
    (let ((target (my/noema--ensure-markdown-file file)))
      (my/noema-open-file target)
      (when my/noema-close-emacs-markdown-buffer
        (run-at-time 0 nil
                     #'my/noema--kill-redirected-markdown-buffer
                     buffer target)))))

(defun my/noema-redirect-markdown-file-h ()
  "Fallback Markdown handoff for packages that override `auto-mode-alist'."
  (when (and buffer-file-name
             (my/noema--markdown-file-p buffer-file-name)
             (not my/noema--markdown-redirected)
             (my/noema--should-redirect-p))
    (my/noema--redirect-current-markdown-buffer)))

;;;###autoload
(defun my/noema-markdown-redirect-mode ()
  "Major-mode replacement that opens the current Markdown file in Noema.
For programmatic opens (magit, grep, org-link), falls back to a raw text mode."
  (interactive)
  (if (my/noema--should-redirect-p)
      (my/noema--redirect-current-markdown-buffer)
    ;; Programmatic open: use a raw markdown view without redirecting.
    (if (require 'markdown-mode nil t)
        (gfm-mode)
      (text-mode))))

;;;###autoload
(defun my/noema-open-markdown-raw (&optional file)
  "Open FILE (or the current note) as a raw Markdown buffer in Emacs.
Bypasses the Noema redirect.  Useful for diffing, fixing broken notes,
or editing when the web editor is unavailable."
  (interactive
   (list (read-file-name "Raw Markdown: " nil
                         (or (my/noema-buffer-file) buffer-file-name)
                         t)))
  (let ((my/noema--inhibit-redirect t)
        (target (expand-file-name
                 (or file
                     (my/noema-buffer-file)
                     (and buffer-file-name
                          (my/noema--markdown-file-p buffer-file-name)
                          buffer-file-name)
                     (user-error "No Markdown file to open")))))
    (unless (my/noema--markdown-file-p target)
      (user-error "Not a Markdown file: %s" target))
    (find-file target)))

(my/noema--pin-markdown-redirect-mode)

;; `treesit-auto' adds its own Markdown entries during configuration.  Re-pin
;; the Noema handoff after that package loads so no grammar prompt runs.
(with-eval-after-load 'treesit-auto
  (my/noema--pin-markdown-redirect-mode))

(with-eval-after-load 'init-treesit
  (my/noema--pin-markdown-redirect-mode))

(add-hook 'after-init-hook #'my/noema--pin-markdown-redirect-mode 90)
(add-hook 'find-file-hook #'my/noema-redirect-markdown-file-h)

(provide 'init-md)

;;; init-md.el ends here
