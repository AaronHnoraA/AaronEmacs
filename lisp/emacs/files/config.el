;;; emacs/files/config.el --- File and Dired workbench -*- lexical-binding: t; -*-

;;; Commentary:
;; Context-aware file operations with routed target-side process execution.

;;; Code:

(require 'config)
(require 'cl-lib)
(require 'dired)
(require 'remote-fs)
(require 'remote-process)
(require 'subr-x)
(require 'transient)

(declare-function proced-toggle-auto-update "proced" (arg))

(config-defvar +files-create-parent-directories 'ask
  "Policy for creating missing parent directories of newly visited files."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Always" always)
                 (const :tag "Never" never))
  :group 'files)

(defun +files-create-parent-directories-h ()
  "Create a new file's missing parent according to the configured policy."
  (when-let* ((file buffer-file-name)
              (parent (file-name-directory file))
              ((not (file-directory-p parent))))
    (pcase +files-create-parent-directories
      ('always (make-directory parent t))
      ('ask (when (y-or-n-p (format "Create parent directory %s? " parent))
              (make-directory parent t)))
      (_ nil))))

(add-hook 'find-file-not-found-functions #'+files-create-parent-directories-h)

(defun +dired--paths-in-region (beg end)
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

(defun +files-selected-paths ()
  "Return file paths selected by the current file-oriented context."
  (cond
   ((derived-mode-p 'dired-mode)
    (if (use-region-p)
        (+dired--paths-in-region (region-beginning) (region-end))
      (or (dired-get-marked-files nil 'marked)
          (when-let* ((file (dired-get-filename nil t)))
            (list file))
          (list default-directory))))
   (buffer-file-name
    (list buffer-file-name))
   (t
    (user-error "No file paths are available here"))))

(defun +files--parent-path (path)
  "Return the parent directory to copy for PATH."
  (file-name-directory (directory-file-name path)))

(defun +files/copy-paths (parents-only)
  "Copy selected paths as one newline-separated kill-ring entry.
With PARENTS-ONLY, copy their de-duplicated parent directories."
  (interactive "P")
  (let* ((paths (+files-selected-paths))
         (paths (if parents-only
                    (delete-dups (mapcar #'+files--parent-path paths))
                  paths))
         (text (string-join paths "\n")))
    (kill-new text)
    (message "Copied %d %s" (length paths)
             (if parents-only "directories" "paths"))
    text))

(defun +dired--parse-du-kib (output)
  "Return the total KiB reported by `du -sk' OUTPUT."
  (let ((total 0))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]" line)
        (setq total (+ total (string-to-number (match-string 1 line))))))
    total))

(defun +files/dired-marked-size ()
  "Calculate and display the total size of selected Dired entries."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command requires a Dired buffer"))
  (let* ((files (+files-selected-paths))
         (context (remote-context default-directory))
         (du (or (remote-executable-find "du" context)
                 (user-error "du is unavailable on this target")))
         (args (append '("-sk") (mapcar #'remote-file-local-name files)))
         (result (remote-exec du :args args :context context
                              :trim t :filesystem-effects 'none))
         (status (remote-exec-result-status result)))
    (unless (zerop status)
      (user-error "du failed: %s" (remote-exec-result-stderr result)))
    (let* ((kib (+dired--parse-du-kib (remote-exec-result-stdout result)))
           (bytes (* kib 1024))
           (human (file-size-human-readable bytes)))
      (message "Selected size: %s (%d entries)" human (length files))
      bytes)))

(defun +files/dired-selection ()
  "Open a virtual Dired buffer containing the selected entries only."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command requires a Dired buffer"))
  (let* ((files (+files-selected-paths))
         (directory default-directory))
    (unless files
      (user-error "No Dired entries selected"))
    (let* ((name (generate-new-buffer-name
                  (format "*%s selection*"
                          (file-name-nondirectory
                           (directory-file-name directory)))))
           (relative (mapcar (lambda (file)
                               (file-relative-name file directory))
                             files))
           ;; `dired-noselect' otherwise reuses the ordinary Dired buffer for
           ;; DIRECTORY and replaces its full listing with this selection.
           (selection
            (cl-letf (((symbol-function 'dired-find-buffer-nocreate)
                       (lambda (&rest _) nil)))
              (dired-noselect (cons directory relative)))))
      (unless (buffer-live-p selection)
        (user-error "Dired could not open the selected files"))
      (with-current-buffer selection
        (rename-buffer name)
        ;; This listing is generated anew for each selection.  Let Dired's
        ;; native q restore its previous window and release this buffer.
        (setq-local quit-window-kill-buffer t))
      (pop-to-buffer-same-window selection))))

(defalias 'file-copy-paths #'+files/copy-paths
  "Copy selected file paths; with a prefix, copy their parent directories.")
(defun file-copy-parent-paths ()
  "Copy the parent directories of the selected files."
  (interactive)
  (+files/copy-paths t))
(defalias 'dired-selected-size #'+files/dired-marked-size
  "Show the total size of selected Dired entries on their target.")
(defalias 'dired-selection #'+files/dired-selection
  "Show selected Dired entries in a separate temporary listing.")

(transient-define-prefix file-menu ()
  "Open files and use context-aware file and Dired actions."
  [["Open"
    ("o" "open file" file-open)
    ("O" "open in new frame" file-open-new-frame)]
   ["Paths"
    ("y" "copy selected paths" file-copy-paths)
    ("Y" "copy parent directories" file-copy-parent-paths)]
   ["Dired"
    ("s" "selected size" dired-selected-size)
    ("v" "selection buffer" dired-selection)]])

(with-eval-after-load 'proced
  (add-hook 'proced-mode-hook
            (lambda ()
              (setq-local proced-auto-update-flag t)
              (proced-toggle-auto-update 1))))

(provide 'emacs/files/config)
;;; emacs/files/config.el ends here
