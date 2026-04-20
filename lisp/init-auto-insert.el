;;; init-auto-insert.el --- Auto insert file templates -*- lexical-binding: t; -*-

;;; Commentary:
;; - Uses built-in `auto-insert' to insert templates for new files.
;; - Supports quick switching between templates under templates/<kind>/.

;;; Code:

(require 'subr-x)
(require 'seq)

(defgroup my/template nil
  "File templates for new files."
  :group 'convenience)

(defcustom my/template-root
  (expand-file-name "templates" user-emacs-directory)
  "Root directory storing templates."
  :type 'directory
  :group 'my/template)

(defcustom my/template-current
  '((org . "default.org")
    (c . "default.c")
    (cc . "default.cc")
    (js . "default.js")
    (ts . "default.ts")
    (sh . "default.sh")
    (tex . "article.tex")
    (python . "default.py"))
  "Current template filename per template kind.

Each entry is (KIND . FILENAME), where KIND maps to templates/KIND/FILENAME."
  :type '(alist :key-type symbol :value-type string)
  :group 'my/template)

(defconst my/template-kinds
  '(org c cc js ts sh tex python)
  "Template kinds supported by this configuration.")

(defcustom my/template-auto-insert-enabled-kinds
  '(c cc sh python)
  "Template kinds enabled for `auto-insert-mode'.

Only kinds in this list will be auto-inserted for new files.  Other kinds can
still be inserted manually via `my/template-switch'."
  :type '(repeat symbol)
  :group 'my/template)

(defcustom my/template-auto-insert-enabled t
  "Whether `auto-insert-mode' should insert templates."
  :type 'boolean
  :group 'my/template)

(defvar-local my/template-current-override nil
  "Dir-local override of `my/template-current'.

Value is an alist like `((python . \"module.py\"))'.  When non-nil, this takes
precedence over `my/template-current' when choosing the template file.")

(defvar-local my/template--remote-placeholder-created nil
  "Whether this buffer bootstrapped a missing remote file for auto-insert.")

(defun my/template--safe-kinds-p (value)
  (and (listp value)
       (seq-every-p (lambda (x) (and (symbolp x) (memq x my/template-kinds))) value)))

(defun my/template--safe-current-override-p (value)
  (and (listp value)
       (seq-every-p
        (lambda (cell)
          (and (consp cell)
               (symbolp (car cell))
               (memq (car cell) my/template-kinds)
               (stringp (cdr cell))
               (not (string-match-p (rx (or "/" "\\")) (cdr cell)))))
        value)))

(put 'my/template-auto-insert-enabled 'safe-local-variable #'booleanp)
(put 'my/template-auto-insert-enabled-kinds 'safe-local-variable #'my/template--safe-kinds-p)
(put 'my/template-current-override 'safe-local-variable #'my/template--safe-current-override-p)

(defun my/template--bootstrap-remote-file-maybe ()
  "Create a missing remote file before auto-insert when possible.

This mirrors the successful manual workaround of creating the remote file
first and then opening it."
  (when (and buffer-file-name
             (file-remote-p buffer-file-name)
             (not (file-exists-p buffer-file-name)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region "" nil buffer-file-name nil 'silent))
    (setq-local my/template--remote-placeholder-created t)))

(defun my/template--kind-for-extension (file)
  (when-let* ((ext (file-name-extension (or file ""))))
    (pcase (downcase ext)
      ("org" 'org)
      ("py" 'python)
      ((or "js" "jsx" "mjs" "cjs") 'js)
      ((or "ts" "tsx") 'ts)
      ((or "cc" "cpp" "cxx" "hpp" "hh" "hxx") 'cc)
      ("c" 'c)
      ((or "sh" "bash" "zsh") 'sh)
      ("tex" 'tex)
      (_ nil))))

(defun my/template--kind (&optional file)
  (or (cond
       ((derived-mode-p 'org-mode) 'org)
       ((or (derived-mode-p 'python-mode) (eq major-mode 'python-ts-mode)) 'python)
       ((or (derived-mode-p 'js-mode) (eq major-mode 'js-ts-mode)) 'js)
       ((or (derived-mode-p 'typescript-mode) (eq major-mode 'typescript-ts-mode) (eq major-mode 'tsx-ts-mode)) 'ts)
       ((or (derived-mode-p 'c++-mode) (eq major-mode 'c++-ts-mode)) 'cc)
       ((or (derived-mode-p 'c-mode) (eq major-mode 'c-ts-mode)) 'c)
       ((or (derived-mode-p 'sh-mode) (eq major-mode 'bash-ts-mode)) 'sh)
       ((or (derived-mode-p 'tex-mode) (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode) (eq major-mode 'latex-ts-mode)) 'tex)
       (t nil))
      (my/template--kind-for-extension (or file buffer-file-name))))

(defun my/template--kind-dir (kind)
  (expand-file-name (symbol-name kind) my/template-root))

(defun my/template--candidates (kind)
  (let* ((dir (my/template--kind-dir kind)))
    (unless (file-directory-p dir)
      (user-error "Template dir not found: %s" dir))
    (sort (seq-filter
           (lambda (name)
             (file-regular-p (expand-file-name name dir)))
           (directory-files dir nil directory-files-no-dot-files-regexp 'nosort))
          #'string<)))

(defun my/template--current (kind)
  (or (alist-get kind my/template-current-override nil nil #'eq)
      (alist-get kind my/template-current nil nil #'eq)
      (car (my/template--candidates kind))))

(defun my/template--path (kind &optional template)
  (expand-file-name (or template (my/template--current kind))
                    (my/template--kind-dir kind)))

(defun my/template--available-kinds ()
  (seq-filter (lambda (kind)
                (file-directory-p (my/template--kind-dir kind)))
              my/template-kinds))

(defun my/template--read-kind ()
  (intern
   (completing-read
    "Template kind: "
    (mapcar #'symbol-name (my/template--available-kinds))
    nil t)))

(defun my/auto-insert--replace-in-region (pairs start end)
  (let ((region-start (copy-marker start))
        (region-end (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region region-start region-end)
            (dolist (pair pairs)
              (goto-char (point-min))
              (while (search-forward (car pair) nil t)
                (replace-match (cdr pair) t t)))))
      (set-marker region-start nil)
      (set-marker region-end nil))))

(defun my/auto-insert--maybe-read-title (start end)
  (let ((region-start (copy-marker start))
        (region-end (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region region-start region-end)
            (goto-char (point-min))
            (when (search-forward "{{title}}" nil t)
              (read-string "Title: "
                           (or (and buffer-file-name
                                    (file-name-base buffer-file-name))
                               "")))))
      (set-marker region-start nil)
      (set-marker region-end nil))))

(defun my/auto-insert--cursor-pos (start end)
  (let ((cursor-token "{{cursor}}"))
    (save-excursion
      (goto-char start)
      (when (search-forward cursor-token end t)
        (replace-match "" t t)
        (point)))))

(defun my/auto-insert--insert-template-file (file)
  (unless (file-readable-p file)
    (user-error "Template not readable: %s" file))
  (let ((start (point))
        (title nil))
    (insert-file-contents file)
    (let ((end (point))
          (cursor-pos nil))
      (setq title (my/auto-insert--maybe-read-title start end))
      (my/auto-insert--replace-in-region
       (delq nil
             (list (cons "{{date}}" (format-time-string "%Y-%m-%d"))
                   (cons "{{month_year}}" (format-time-string "%B %Y"))
                   (cons "{{year}}" (format-time-string "%Y"))
                   (cons "{{author}}" user-full-name)
                   (cons "{{user}}" (or (getenv "USER") user-login-name))
                   (cons "{{file}}" (or buffer-file-name ""))
                   (and title (cons "{{title}}" title))))
       start end)
      (setq cursor-pos (my/auto-insert--cursor-pos start (point-max)))
      (goto-char (or cursor-pos (point-max))))))

(defun my/template--confirm-replace-buffer ()
  (when buffer-read-only
    (user-error "Current buffer is read-only"))
  (when (and (buffer-modified-p)
             (not (y-or-n-p "Buffer has unsaved changes. Replace contents with template? ")))
    (user-error "Aborted"))
  t)

(defun my/template-apply (&optional kind replace template)
  "Insert the current KIND template into the current buffer.

When REPLACE is non-nil (or when called interactively with a prefix arg),
erase the buffer first.

KIND should be a symbol like `python' and TEMPLATE (when non-nil) should be a
template filename under templates/KIND/."
  (when replace
    (my/template--confirm-replace-buffer)
    (erase-buffer))
  (my/auto-insert--insert-template-file (my/template--path kind template)))

(defun my/template--auto-insert-allowed-p ()
  "Return non-nil when `auto-insert-mode' should insert templates here."
  (and buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (or my/template--remote-placeholder-created
           (not (file-exists-p buffer-file-name)))
       (not (buffer-modified-p))
       (null (buffer-base-buffer))
       (not (bound-and-true-p org-capture-current-plist))
       (not (member (substring (buffer-name) 0 1) '("*" " ")))))

(defun my/template-auto-insert (kind &optional template)
  "Auto-insert template for KIND when enabled.

When TEMPLATE is non-nil, insert that exact template file under templates/KIND/."
  (my/template--bootstrap-remote-file-maybe)
  (when (and (my/template--auto-insert-allowed-p)
             my/template-auto-insert-enabled
             (memq kind my/template-auto-insert-enabled-kinds))
    (my/template-apply kind nil template)))

(defun my/template-debug ()
  "Report the current template status for this buffer."
  (interactive)
  (let* ((kind (my/template--kind))
         (enabled (and my/template-auto-insert-enabled
                       (memq kind my/template-auto-insert-enabled-kinds))))
    (if (not kind)
        (message "Template: none (no kind matched)")
      (message "Template: kind=%s enabled=%s template=%s (override=%s)"
               kind
               (if enabled "yes" "no")
               (my/template--current kind)
               (if (alist-get kind my/template-current-override nil nil #'eq)
                   "yes"
                 "no")))))

(defun my/template-switch (&optional kind template)
  "Switch TEMPLATE for KIND.

When called interactively, KIND is inferred from the current buffer or file
name, falling back to a prompt."
  (interactive
   (let* ((picked-kind (or (my/template--kind) (my/template--read-kind)))
          (cands (my/template--candidates picked-kind))
          (current (my/template--current picked-kind)))
     (list picked-kind
           (completing-read
            (format "Template (%s): " picked-kind)
            cands nil t nil nil current))))
  (setf (alist-get kind my/template-current nil nil #'eq) template)
  (customize-save-variable 'my/template-current my/template-current)
  (my/template-apply kind 'replace)
  (message "Template(%s) set to: %s" kind (alist-get kind my/template-current nil nil #'eq)))

(use-package autoinsert
  :ensure nil
  :hook (after-init . auto-insert-mode)
  :custom
  (auto-insert-query nil)
  :config
  (define-auto-insert (rx ".org" string-end) (lambda () (my/template-auto-insert 'org)))
  (define-auto-insert (rx ".py" string-end) (lambda () (my/template-auto-insert 'python)))
  (define-auto-insert (rx ".js" string-end) (lambda () (my/template-auto-insert 'js)))
  (define-auto-insert (rx ".cjs" string-end) (lambda () (my/template-auto-insert 'js "default.cjs")))
  (define-auto-insert (rx ".mjs" string-end) (lambda () (my/template-auto-insert 'js "default.mjs")))
  (define-auto-insert (rx ".jsx" string-end) (lambda () (my/template-auto-insert 'js "react.jsx")))
  (define-auto-insert (rx ".ts" string-end) (lambda () (my/template-auto-insert 'ts)))
  (define-auto-insert (rx ".tsx" string-end) (lambda () (my/template-auto-insert 'ts "react.tsx")))
  (define-auto-insert (rx ".c" string-end) (lambda () (my/template-auto-insert 'c)))
  (define-auto-insert (rx "." (or "cc" "cpp" "cxx") string-end)
    (lambda () (my/template-auto-insert 'cc)))
  (define-auto-insert (rx ".sh" string-end)
    (lambda () (my/template-auto-insert 'sh)))
  (define-auto-insert (rx ".bash" string-end)
    (lambda () (my/template-auto-insert 'sh "default.bash")))
  (define-auto-insert (rx ".zsh" string-end)
    (lambda () (my/template-auto-insert 'sh "default.zsh")))
  (define-auto-insert (rx ".tex" string-end)
    (lambda () (my/template-auto-insert 'tex))))

(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
