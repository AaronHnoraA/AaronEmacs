;;; editor/file-templates/config.el --- Auto insert file templates -*- lexical-binding: t; -*-

;;; Commentary:
;; - Uses built-in `auto-insert' to insert templates for new files.
;; - Supports quick switching between templates under templates/<kind>/.

;;; Code:

(require 'config)

(require 'subr-x)
(require 'seq)
(require 'project)

(defgroup +template nil
  "File templates for new files."
  :group 'convenience)

(config-defvar +template-root nil
  "Root directory storing templates."
  :type 'directory
  :group '+template)

(config-defvar +template-current nil
  "Current template filename per template kind.

Each entry is (KIND . FILENAME), where KIND maps to templates/KIND/FILENAME."
  :type '(alist :key-type symbol :value-type string)
  :group '+template)

(defconst +template-kinds
  '(org c cc js ts sh tex python nix makefile cmake)
  "Template kinds supported by this configuration.")

(config-defvar +template-auto-insert-enabled-kinds nil
  "Template kinds enabled for `auto-insert-mode'.

Only kinds in this list will be auto-inserted for new files.  Other kinds can
still be inserted manually via `+template/switch'."
  :type '(repeat symbol)
  :group '+template)

(config-defvar +template-auto-insert-enabled nil
  "Whether `auto-insert-mode' should insert templates."
  :type 'boolean
  :group '+template)

(declare-function remote-file-local-name "remote-fs" (file-name))

(defvar-local +template-current-override nil
  "Dir-local override of `+template-current'.

Value is an alist like `((python . \"module.py\"))'.  When non-nil, this takes
precedence over `+template-current' when choosing the template file.")

(defvar-local +template--remote-placeholder-created nil
  "Whether this buffer bootstrapped a missing remote file for auto-insert.")

(defun +template--safe-kinds-p (value)
  (and (listp value)
       (seq-every-p (lambda (x) (and (symbolp x) (memq x +template-kinds))) value)))

(defun +template--safe-current-override-p (value)
  (and (listp value)
       (seq-every-p
        (lambda (cell)
          (and (consp cell)
               (symbolp (car cell))
               (memq (car cell) +template-kinds)
               (stringp (cdr cell))
               (not (string-match-p (rx (or "/" "\\")) (cdr cell)))))
        value)))

(put '+template-auto-insert-enabled 'safe-local-variable #'booleanp)
(put '+template-auto-insert-enabled-kinds 'safe-local-variable #'+template--safe-kinds-p)
(put '+template-current-override 'safe-local-variable #'+template--safe-current-override-p)

(defun +template--bootstrap-remote-file-maybe ()
  "Create a missing remote file before auto-insert when possible.

This mirrors the successful manual workaround of creating the remote file
first and then opening it."
  (when (and buffer-file-name
             (file-remote-p buffer-file-name)
             (not (file-exists-p buffer-file-name)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region "" nil buffer-file-name nil 'silent))
    (setq-local +template--remote-placeholder-created t)))

(defun +template--kind-for-extension (file)
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
      ("nix" 'nix)
      ("mk" 'makefile)
      ("cmake" 'cmake)
      (_ nil))))

(defun +template--kind (&optional file)
  (or (cond
       ((derived-mode-p 'org-mode) 'org)
       ((or (derived-mode-p 'python-mode) (eq major-mode 'python-ts-mode)) 'python)
       ((or (derived-mode-p 'js-mode) (eq major-mode 'js-ts-mode)) 'js)
       ((or (derived-mode-p 'typescript-mode) (eq major-mode 'typescript-ts-mode) (eq major-mode 'tsx-ts-mode)) 'ts)
       ((or (derived-mode-p 'c++-mode) (eq major-mode 'c++-ts-mode)) 'cc)
       ((or (derived-mode-p 'c-mode) (eq major-mode 'c-ts-mode)) 'c)
       ((or (derived-mode-p 'sh-mode) (eq major-mode 'bash-ts-mode)) 'sh)
       ((or (derived-mode-p 'tex-mode) (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode) (eq major-mode 'latex-ts-mode)) 'tex)
       ((or (derived-mode-p 'nix-mode) (eq major-mode 'nix-ts-mode)) 'nix)
       ((derived-mode-p 'makefile-mode) 'makefile)
       ((derived-mode-p 'cmake-mode) 'cmake)
       (t nil))
      (let ((name (file-name-nondirectory (or file buffer-file-name ""))))
        (cond
         ((member name '("Makefile" "makefile" "GNUmakefile")) 'makefile)
         ((string= name "CMakeLists.txt") 'cmake)
         (t (+template--kind-for-extension (or file buffer-file-name)))))))

(defun +template--kind-dir (kind)
  (expand-file-name (symbol-name kind) +template-root))

(defun +template--candidates (kind)
  (let* ((dir (+template--kind-dir kind)))
    (unless (file-directory-p dir)
      (user-error "Template dir not found: %s" dir))
    (sort (seq-filter
           (lambda (name)
             (file-regular-p (expand-file-name name dir)))
           (directory-files dir nil directory-files-no-dot-files-regexp 'nosort))
          #'string<)))

(defun +template--current (kind)
  (or (alist-get kind +template-current-override nil nil #'eq)
      (alist-get kind +template-current nil nil #'eq)
      (car (+template--candidates kind))))

(defun +template--path (kind &optional template)
  (expand-file-name (or template (+template--current kind))
                    (+template--kind-dir kind)))

(defun +template--project-root ()
  "Return the current project root or the current file directory."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((project (condition-case nil
                                 (project-current nil)
                               (file-error nil))))
          (project-root project))
        (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory))))

(defun +template--ensure-symlink (target link)
  "Ensure LINK points to TARGET."
  (let ((target (file-truename target)))
    (cond
     ((file-symlink-p link)
      (unless (file-equal-p (file-truename link) target)
        (delete-file link)
        (make-symbolic-link target link)))
     ((file-exists-p link)
      (delete-file link)
      (make-symbolic-link target link))
     (t
      (make-symbolic-link target link)))))

(defun +template--available-kinds ()
  (seq-filter (lambda (kind)
                (file-directory-p (+template--kind-dir kind)))
              +template-kinds))

(defun +template--read-kind ()
  (intern
   (completing-read
    "Template kind: "
    (mapcar #'symbol-name (+template--available-kinds))
    nil t)))

(defun +auto-insert--replace-in-region (pairs start end)
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

(defun +auto-insert--maybe-read-title (start end)
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

(defun +auto-insert--cursor-pos (start end)
  (let ((cursor-token "{{cursor}}"))
    (save-excursion
      (goto-char start)
      (when (search-forward cursor-token end t)
        (replace-match "" t t)
        (point)))))

(defun +auto-insert--slug (value)
  "Return a conservative filename/id slug for VALUE."
  (let ((slug (downcase (string-trim (or value "")))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    (or (string-trim slug "-+" "-+") "untitled")))

(defun +auto-insert--insert-template-file (file)
  (unless (file-readable-p file)
    (user-error "Template not readable: %s" file))
  (let ((start (point))
        (title nil))
    ;; `insert-file-contents' leaves point before the text; its return value
    ;; carries the inserted length, which bounds the placeholder region.
    (let ((inserted (cadr (insert-file-contents file))))
      (goto-char (+ start inserted)))
    (let ((end (point))
          (cursor-pos nil))
      (setq title (+auto-insert--maybe-read-title start end))
      (+auto-insert--replace-in-region
       (delq nil
             (list (cons "{{date}}" (format-time-string "%Y-%m-%d"))
                   (cons "{{timestamp}}" (format-time-string "%Y%m%dT%H%M%S"))
                   (cons "{{month_year}}" (format-time-string "%B %Y"))
                   (cons "{{year}}" (format-time-string "%Y"))
                   (cons "{{author}}" user-full-name)
                   (cons "{{user}}" (or (getenv "USER") user-login-name))
                   ;; The file's own path on the machine that holds it,
                   ;; never an Emacs-internal /fs: or TRAMP spelling.
                   (cons "{{file}}"
                         (if buffer-file-name
                             (remote-file-local-name buffer-file-name)
                           ""))
                   (and title (cons "{{slug_title}}" (+auto-insert--slug title)))
                   (and title (cons "{{title}}" title))))
       start end)
      (setq cursor-pos (+auto-insert--cursor-pos start (point-max)))
      (goto-char (or cursor-pos (point-max))))))

(defun +template--confirm-replace-buffer ()
  (when buffer-read-only
    (user-error "Current buffer is read-only"))
  (when (and (buffer-modified-p)
             (not (y-or-n-p "Buffer has unsaved changes. Replace contents with template? ")))
    (user-error "Aborted"))
  t)

(defun +template-apply (&optional kind replace template)
  "Insert the current KIND template into the current buffer.

When REPLACE is non-nil (or when called interactively with a prefix arg),
erase the buffer first.

KIND should be a symbol like `python' and TEMPLATE (when non-nil) should be a
template filename under templates/KIND/."
  (when replace
    (+template--confirm-replace-buffer)
    (erase-buffer))
  (let* ((kind (or kind (+template--kind) (+template--read-kind)))
         (template (or template (+template--current kind))))
    (+auto-insert--insert-template-file (+template--path kind template))))

(defun +template--auto-insert-allowed-p ()
  "Return non-nil when `auto-insert-mode' should insert templates here."
  (and buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (or +template--remote-placeholder-created
           (not (file-exists-p buffer-file-name)))
       (not (buffer-modified-p))
       (null (buffer-base-buffer))
       (not (bound-and-true-p org-capture-current-plist))
       (not (member (substring (buffer-name) 0 1) '("*" " ")))))

(defun +template-auto-insert (kind &optional template)
  "Auto-insert template for KIND when enabled.

When TEMPLATE is non-nil, insert that exact template file under templates/KIND/."
  (+template--bootstrap-remote-file-maybe)
  (when (and (+template--auto-insert-allowed-p)
             +template-auto-insert-enabled
             (memq kind +template-auto-insert-enabled-kinds))
    (+template-apply kind nil template)))

(defun +template/debug ()
  "Report the current template status for this buffer."
  (interactive)
  (let* ((kind (+template--kind))
         (enabled (and +template-auto-insert-enabled
                       (memq kind +template-auto-insert-enabled-kinds))))
    (if (not kind)
        (message "Template: none (no kind matched)")
      (message "Template: kind=%s enabled=%s template=%s (override=%s)"
               kind
               (if enabled "yes" "no")
               (+template--current kind)
               (if (alist-get kind +template-current-override nil nil #'eq)
                   "yes"
                 "no")))))

(defun +template/switch (&optional kind template)
  "Switch TEMPLATE for KIND.

When called interactively, KIND is inferred from the current buffer or file
name, falling back to a prompt."
  (interactive
   (let* ((picked-kind (or (+template--kind) (+template--read-kind)))
          (cands (+template--candidates picked-kind))
          (current (+template--current picked-kind)))
     (list picked-kind
           (completing-read
            (format "Template (%s): " picked-kind)
            cands nil t nil nil current))))
  (setf (alist-get kind +template-current nil nil #'eq) template)
  (config-set '+template-current +template-current)
  (+template-apply kind 'replace)
  (message "Template(%s) set to: %s" kind (alist-get kind +template-current nil nil #'eq)))

(use-package autoinsert
  :ensure nil
  :hook (after-init . auto-insert-mode)
  :custom
  (auto-insert-query nil)
  :config
  (define-auto-insert (rx ".org" string-end) (lambda () (+template-auto-insert 'org)))
  (define-auto-insert (rx ".py" string-end) (lambda () (+template-auto-insert 'python)))
  (define-auto-insert (rx ".js" string-end) (lambda () (+template-auto-insert 'js)))
  (define-auto-insert (rx ".cjs" string-end) (lambda () (+template-auto-insert 'js "default.cjs")))
  (define-auto-insert (rx ".mjs" string-end) (lambda () (+template-auto-insert 'js "default.mjs")))
  (define-auto-insert (rx ".jsx" string-end) (lambda () (+template-auto-insert 'js "react.jsx")))
  (define-auto-insert (rx ".ts" string-end) (lambda () (+template-auto-insert 'ts)))
  (define-auto-insert (rx ".tsx" string-end) (lambda () (+template-auto-insert 'ts "react.tsx")))
  (define-auto-insert (rx ".c" string-end) (lambda () (+template-auto-insert 'c)))
  (define-auto-insert (rx "." (or "cc" "cpp" "cxx") string-end)
    (lambda () (+template-auto-insert 'cc)))
  (define-auto-insert (rx ".sh" string-end)
    (lambda () (+template-auto-insert 'sh)))
  (define-auto-insert (rx ".bash" string-end)
    (lambda () (+template-auto-insert 'sh "default.bash")))
  (define-auto-insert (rx ".zsh" string-end)
    (lambda () (+template-auto-insert 'sh "default.zsh")))
  (define-auto-insert (rx ".tex" string-end)
    (lambda () (+template-auto-insert 'tex)))
  (define-auto-insert (rx "/flake.nix" string-end)
    (lambda () (+template-auto-insert 'nix)))
  (define-auto-insert (rx ".nix" string-end)
    (lambda () (+template-auto-insert 'nix))))

(provide 'editor/file-templates/config)
;;; editor/file-templates/config.el ends here
