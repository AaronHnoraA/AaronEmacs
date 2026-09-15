;;; init-file-workbench.el --- File and Dired workbench -*- lexical-binding: t; -*-

;;; Commentary:
;; Context-aware file operations with routed target-side process execution.

;;; Code:

(require 'config)
(require 'dired)
(require 'remote-fs)
(require 'remote-process)
(require 'subr-x)
(require 'transient)

(declare-function proced-toggle-auto-update "proced" (arg))

(config-defvar my/files-create-parent-directories nil
  "Policy for creating missing parent directories of newly visited files."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Always" always)
                 (const :tag "Never" never))
  :group 'files)

(defun my/files-create-parent-directories-h ()
  "Create a new file's missing parent according to the configured policy."
  (when-let* ((file buffer-file-name)
              (parent (file-name-directory file))
              ((not (file-directory-p parent))))
    (pcase my/files-create-parent-directories
      ('always (make-directory parent t))
      ('ask (when (y-or-n-p (format "Create parent directory %s? " parent))
              (make-directory parent t)))
      (_ nil))))

(add-hook 'find-file-not-found-functions #'my/files-create-parent-directories-h)

(defun my/dired--paths-in-region (beg end)
  "Return Dired paths represented by lines between BEG and END."
  (let (paths)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (when-let* ((file (dired-get-filename nil t)))
          (push file paths))
        (forward-line 1)))
    (nreverse (delete-dups paths))))

(defun my/files-selected-paths ()
  "Return file paths selected by the current file-oriented context."
  (cond
   ((derived-mode-p 'dired-mode)
    (cond
     ((use-region-p)
      (my/dired--paths-in-region (region-beginning) (region-end)))
     ((dired-get-filename nil t)
      (dired-get-marked-files))
     (t
      (list default-directory))))
   (buffer-file-name
    (list buffer-file-name))
   (t
    (user-error "No file paths are available here"))))

(defun my/files--parent-path (path)
  "Return the parent directory to copy for PATH."
  (file-name-directory (directory-file-name path)))

(defun my/files-copy-paths (parents-only)
  "Copy selected paths as one newline-separated kill-ring entry.
With PARENTS-ONLY, copy their de-duplicated parent directories."
  (interactive "P")
  (let* ((paths (my/files-selected-paths))
         (paths (if parents-only
                    (delete-dups (mapcar #'my/files--parent-path paths))
                  paths))
         (text (string-join paths "\n")))
    (kill-new text)
    (message "Copied %d %s" (length paths)
             (if parents-only "directories" "paths"))
    text))

(defun my/dired--parse-du-kib (output)
  "Return the total KiB reported by `du -sk' OUTPUT."
  (let ((total 0))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]" line)
        (setq total (+ total (string-to-number (match-string 1 line))))))
    total))

(defun my/dired-marked-size ()
  "Calculate and display the total size of selected Dired entries."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command requires a Dired buffer"))
  (let* ((files (my/files-selected-paths))
         (context (remote-context default-directory))
         (du (or (remote-executable-find "du" context)
                 (user-error "du is unavailable on this target")))
         (args (append '("-sk") (mapcar #'remote-file-local-name files)))
         (result (remote-exec du :args args :context context
                              :trim t :filesystem-effects 'none))
         (status (remote-exec-result-status result)))
    (unless (zerop status)
      (user-error "du failed: %s" (remote-exec-result-stderr result)))
    (let* ((kib (my/dired--parse-du-kib (remote-exec-result-stdout result)))
           (bytes (* kib 1024))
           (human (file-size-human-readable bytes)))
      (message "Selected size: %s (%d entries)" human (length files))
      bytes)))

(defun my/dired-selection ()
  "Open a virtual Dired buffer containing the selected entries only."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command requires a Dired buffer"))
  (let* ((files (my/files-selected-paths))
         (name (generate-new-buffer-name
                (format "*%s selection*"
                        (file-name-nondirectory
                         (directory-file-name default-directory)))))
         (relative (mapcar (lambda (file)
                             (file-relative-name file default-directory))
                           files)))
    (unless files
      (user-error "No Dired entries selected"))
    (dired (cons name relative))))

(defun my/image-crop-dwim ()
  "Invoke the Emacs image crop UI for the current image buffer."
  (interactive)
  (unless (derived-mode-p 'image-mode)
    (user-error "This command requires an image buffer"))
  (call-interactively #'image-crop))

(transient-define-prefix my/files-dispatch ()
  "File and Dired utilities."
  [["Paths"
    ("y" "copy selected paths" my/files-copy-paths)
    ("Y" "copy parent directories" (lambda () (interactive) (my/files-copy-paths t)))]
   ["Dired"
    ("s" "selected size" my/dired-marked-size)
    ("v" "selection buffer" my/dired-selection)]
   ["Image"
    ("c" "crop current image" my/image-crop-dwim)]])

(with-eval-after-load 'proced
  (add-hook 'proced-mode-hook
            (lambda ()
              (setq-local proced-auto-update-flag t)
              (proced-toggle-auto-update 1))))

(provide 'init-file-workbench)
;;; init-file-workbench.el ends here
