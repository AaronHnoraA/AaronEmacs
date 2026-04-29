;;; init-org-utility.el --- Org utility commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Small Org helpers that do not belong to agenda, capture, roam, or export.

;;; Code:

(require 'init-org-core)
(require 'org-id)
(require 'subr-x)

(defgroup my/org-utility nil
  "Personal Org utility helpers."
  :group 'org)

(defcustom my/org-download-image-root "img"
  "Directory, relative to the Org file, where pasted images are stored."
  :type 'string
  :group 'my/org-utility)

(defcustom my/org-reference-display-math-search-limit 4000
  "Maximum nearby characters searched when detecting a \\[...\\] formula."
  :type 'integer
  :group 'my/org-utility)

(defvar org-download-image-dir)
(defvar org-download-timestamp)

(declare-function org-download-clipboard "org-download")
(declare-function org-download--dir "org-download")
(declare-function org-download-screenshot "org-download")

(defun my/org-reference--display-math-bounds ()
  "Return bounds of the surrounding Org \\[...\\] formula, or nil."
  (let ((origin (point))
        (limit (max (point-min)
                    (- (point) my/org-reference-display-math-search-limit)))
        begin end)
    (save-excursion
      (when (search-backward "\\[" limit t)
        (setq begin (point))
        (when (search-forward "\\]" nil t)
          (setq end (point)))))
    (when (and begin end
               (<= begin origin)
               (<= origin end))
      (cons begin end))))

(defun my/org-reference--target-after-formula (pos)
  "Return dedicated target immediately after formula ending at POS, or nil."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (when (looking-at "<<\\([^>\n]+\\)>>")
      (match-string-no-properties 1))))

(defun my/org-reference--formula-target-at (bounds)
  "Return dedicated target around formula BOUNDS, or nil.
Targets are stored after the formula closing delimiter."
  (my/org-reference--target-after-formula (cdr bounds)))

(defun my/org-reference--block-bounds ()
  "Return bounds and type of the surrounding Org #+begin block, or nil."
  (let ((origin (point))
        (case-fold-search t)
        begin end type)
    (save-excursion
      ;; Jump to end of current line so re-search-backward includes this line.
      (goto-char (line-end-position))
      (when (re-search-backward "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b" nil t)
        (setq begin (line-beginning-position)
              type (downcase (match-string-no-properties 1)))
        (when (re-search-forward
               (format "^[ \t]*#\\+end_%s\\b" (regexp-quote type)) nil t)
          (setq end (line-end-position)))))
    (when (and begin end
               (<= begin origin)
               (<= origin end))
      (list :begin begin :end end :type type))))

(defun my/org-reference--target-before-block (pos)
  "Return dedicated target on the line immediately before block starting at POS, or nil."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (when (looking-at "[ \t]*<<\\([^>\n]+\\)>>[ \t]*$")
      (match-string-no-properties 1))))

(defun my/org-reference-create-block-target ()
  "Create or return a dedicated target before the current Org #+begin block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((bounds (or (my/org-reference--block-bounds)
                     (user-error "Point is not inside a #+begin block")))
         (begin (plist-get bounds :begin))
         (type (plist-get bounds :type))
         (target (or (my/org-reference--target-before-block begin)
                     (my/org-reference--unique-target type))))
    (unless (my/org-reference--target-before-block begin)
      (save-excursion
        (goto-char begin)
        (insert "<<" target ">>\n")))
    (kill-new target)
    (message "Org block target: %s" target)
    target))

(defun my/org-reference--unique-target (&optional prefix)
  "Return a buffer-unique dedicated target name using PREFIX."
  (let* ((stem (or prefix "eq"))
         (base (format "%s-%s" stem (format-time-string "%Y%m%dT%H%M%S")))
         (candidate base)
         (index 2))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (format "<<%s>>" (regexp-quote candidate)) nil t)
        (setq candidate (format "%s-%d" base index)
              index (1+ index))))
    candidate))

(defun my/org-reference-create-formula-target ()
  "Create or return a dedicated target after the current \\[...\\] formula."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((bounds (or (my/org-reference--display-math-bounds)
                     (user-error "Point is not inside a \\[...\\] formula")))
         (target (or (my/org-reference--formula-target-at bounds)
                     (my/org-reference--unique-target "eq"))))
    (unless (my/org-reference--formula-target-at bounds)
      (save-excursion
        (goto-char (cdr bounds))
        (insert "     <<" target ">>")))
    (kill-new target)
    (message "Org formula target: %s" target)
    target))

(defun my/org-reference-ensure-target-at-point ()
  "Create a stable Org reference target at point.
On a headline, create or return its `ID'.  Inside a \\[...\\] formula, create
or return its dedicated target.  Inside a #+begin block, create or return a
dedicated target after the block."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cond
   ((org-before-first-heading-p)
    (cond
     ((my/org-reference--block-bounds)
      (my/org-reference-create-block-target))
     ((my/org-reference--display-math-bounds)
      (my/org-reference-create-formula-target))
     (t
      (user-error "Move point to a headline, a \\[...\\] formula, or a #+begin block"))))
   ((org-at-heading-p)
    (let ((id (org-id-get-create)))
      (kill-new id)
      (message "Org heading ID: %s" id)
      id))
   ((my/org-reference--block-bounds)
    (my/org-reference-create-block-target))
   ((my/org-reference--display-math-bounds)
    (my/org-reference-create-formula-target))
   (t
    (org-back-to-heading t)
    (let ((id (org-id-get-create)))
      (kill-new id)
      (message "Org heading ID: %s" id)
      id))))

(defun my/org-reference-create-target-dwim ()
  "Create or reuse a target at point, then copy its Org link.
This is the main interactive reference command: use it on a heading or inside
a \\[...\\] formula."
  (interactive)
  (my/org-copy-reference-link-at-point))

(defun my/org-reference--heading-link ()
  "Return an id link to the current heading, creating the ID if needed."
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (format "[[id:%s]]" (org-id-get-create))))

(defun my/org-reference--formula-link ()
  "Return a link to the current \\[...\\] formula target, creating it if needed."
  (format "[[%s]]" (my/org-reference-create-formula-target)))

(defun my/org-reference--block-link ()
  "Return a link to the current #+begin block target, creating it if needed."
  (format "[[%s]]" (my/org-reference-create-block-target)))

(defun my/org-reference-link-at-point ()
  "Return an Org link to the current heading, \\[...\\] formula or block."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cond
   ((my/org-reference--block-bounds)
    (my/org-reference--block-link))
   ((my/org-reference--display-math-bounds)
    (my/org-reference--formula-link))
   ((org-before-first-heading-p)
    (user-error "Move point to a headline, a \\[...\\] formula, or a #+begin block"))
   (t
    (my/org-reference--heading-link))))

(defun my/org-copy-reference-link-at-point ()
  "Copy an open Org link to the current heading, \\[...\\] formula or block.
The copied text is [[target][ — the description and closing ]] are left for
the user to fill in."
  (interactive)
  (let* ((link (my/org-reference-link-at-point))
         (open (concat (substring link 0 -1) "[")))
    (kill-new open)
    (message "Copied Org link: %s" open)))

(defun my/org-insert-id-link ()
  "Insert an Org id link with prompted ID."
  (interactive)
  (let ((id (string-trim (read-string "ID: "))))
    (when (string-empty-p id)
      (user-error "ID is empty"))
    (insert (format "[[id:%s]]" id))))

(defun my/org-insert-target-link ()
  "Insert an Org dedicated-target link with prompted target."
  (interactive)
  (let ((target (string-trim (read-string "Target: "))))
    (when (string-empty-p target)
      (user-error "Target is empty"))
    (insert (format "[[%s]]" target))))

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
              ("C-c i d" . org-download-delete)
              ("C-c n I" . my/org-insert-id-link)
              ("C-c n T" . my/org-insert-target-link)))

(provide 'init-org-utility)
;;; init-org-utility.el ends here
