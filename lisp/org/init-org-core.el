;;; init-org-core.el --- Core Org paths and defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Refactored Org Configuration.
;; 1. Fixed load order: org-modern loads after org.
;; 2. Optimized directory creation and path handling.
;; 3. Enhanced robustness of special block rendering (nested blocks support).
;; 4. Performance tuning for on-demand LaTeX previews.

;;; Code:

(require 'aaron-ui)
(declare-function my/typography-setup-prose-buffer "init-base")
(defvar my/prose-line-spacing)
(require 'init-funcs)
(require 'org)
(require 'org-element)
(require 'subr-x)

;;; ----------------------------------------------------------------------------
;;; 1. Global Variables & Paths (全局路径配置)
;;; ----------------------------------------------------------------------------

(defvar my-org-root (file-truename "~/HC/Org/")
  "Root directory for all Org files.")

(defvar my-org-roam-dir (expand-file-name "roam/" my-org-root))
(defvar my-org-daily-dir (expand-file-name "daily/" my-org-root))
(defvar my-org-notes-file (expand-file-name "notes.org" my-org-root))
(defvar my-org-diary-file (expand-file-name "diary.org" my-org-root))

;; References
(defvar pv/org-refile-file (expand-file-name "refile.org" my-org-root))
(defvar pv/org-bibtex-dir (expand-file-name "references/" my-org-root))
(defvar pv/org-bibtex-files (list (expand-file-name "references.bib" pv/org-bibtex-dir)))

;; Ensure core directories exist (Optimized)
(dolist (dir (list my-org-root my-org-roam-dir my-org-daily-dir pv/org-bibtex-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defgroup my/org-ui nil
  "Org UI and note-taking ergonomics."
  :group 'org)

(defcustom my/org-rich-ui-max-buffer-size (* 512 1024)
  "Compatibility knob kept for older logic.
Rich Org UI is no longer disabled based on buffer size."
  :type 'integer)

(defcustom my/org-pretty-block-max-buffer-size (* 256 1024)
  "Compatibility knob kept for older logic.
Special block overlays are no longer disabled based on buffer size."
  :type 'integer)

(defcustom my/org-toc-depth 3
  "Default headline depth used by `my/org-toc-insert-or-update'."
  :type 'integer)

(defcustom my/org-toc-auto-update-max-buffer-size (* 1024 1024)
  "Maximum Org buffer size eligible for automatic TOC refresh."
  :type 'integer)

(defcustom my/org-toc-auto-update-enabled t
  "Whether Org buffers with an overview TOC refresh automatically."
  :type 'boolean)

(defvar-local my/org-toc--block-begin-marker nil
  "Marker for the beginning of the managed overview TOC block.")

(defvar-local my/org-toc--block-end-marker nil
  "Marker for the end of the managed overview TOC block.")

(defvar-local my/org-toc--initial-refresh-done nil
  "Whether this buffer already ran its one-time TOC refresh after opening.")

(defun my/org-rich-ui-buffer-p ()
  "Return non-nil when the current Org buffer should use rich UI helpers."
  (my/rich-ui-buffer-p nil my/org-rich-ui-max-buffer-size))

(defun my/org-enable-typography-maybe ()
  "Enable shared prose typography for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (my/typography-setup-prose-buffer)))

(defun my/org-setup-buffer-spacing ()
  "Use relaxed line spacing in Org buffers only."
  (setq-local line-spacing (or (and (boundp 'my/prose-line-spacing)
                                    my/prose-line-spacing)
                               0.16)))

(defun my/org-force-indent-mode ()
  "Enable `org-indent-mode' in every Org buffer."
  (when (derived-mode-p 'org-mode)
    (org-indent-mode 1)))

(defun my/org-force-indent-mode-in-existing-buffers ()
  "Enable `org-indent-mode' for Org buffers that already exist."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/org-force-indent-mode))))

(defun my/org-toc--block-regexp ()
  "Return regexp matching the managed overview TOC block."
  "^[ \t]*#\\+begin_overview\\_>.*:toc\\_>")

(defun my/org-toc--find-block ()
  "Return (BEGIN . END) for the managed overview TOC block, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (my/org-toc--block-regexp) nil t)
      (let ((begin (line-beginning-position)))
        (when (re-search-forward "^[ \t]*#\\+end_overview\\_>" nil t)
          (cons begin (min (point-max) (1+ (line-end-position)))))))))

(defun my/org-toc--set-cached-block (block)
  "Cache managed TOC BLOCK bounds using markers."
  (if block
      (progn
        (setq-local my/org-toc--block-begin-marker (copy-marker (car block)))
        (setq-local my/org-toc--block-end-marker (copy-marker (cdr block) t)))
    (when (markerp my/org-toc--block-begin-marker)
      (set-marker my/org-toc--block-begin-marker nil))
    (when (markerp my/org-toc--block-end-marker)
      (set-marker my/org-toc--block-end-marker nil))
    (setq-local my/org-toc--block-begin-marker nil)
    (setq-local my/org-toc--block-end-marker nil)))

(defun my/org-toc--refresh-cached-block ()
  "Refresh cached managed TOC bounds and return them."
  (let ((block (my/org-toc--find-block)))
    (my/org-toc--set-cached-block block)
    block))

(defun my/org-toc--cached-block ()
  "Return cached managed TOC block bounds, or nil."
  (when (and (markerp my/org-toc--block-begin-marker)
             (markerp my/org-toc--block-end-marker)
             (marker-buffer my/org-toc--block-begin-marker)
             (marker-buffer my/org-toc--block-end-marker))
    (cons (marker-position my/org-toc--block-begin-marker)
          (marker-position my/org-toc--block-end-marker))))

(defun my/org-toc--block-depth (block)
  "Return the DEPTH declared by managed TOC BLOCK, or nil."
  (save-excursion
    (goto-char (car block))
    (when (looking-at ".*:depth[ \t]+\\([0-9]+\\)")
      (string-to-number (match-string 1)))))

(defun my/org-toc--skip-blank-lines ()
  "Move point over blank lines."
  (while (and (not (eobp)) (looking-at-p "[ \t]*$"))
    (forward-line 1)))

(defun my/org-toc--metadata-at-point-p ()
  "Return non-nil when point is on leading Org file metadata."
  (or (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
      (looking-at-p "[ \t]*#\\+")))

(defun my/org-toc--forward-metadata ()
  "Move point over the metadata form at point."
  (cond
   ((looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
    (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
        (forward-line 1)
      (goto-char (point-max))))
   ((looking-at-p "[ \t]*#\\+")
    (forward-line 1))))

(defun my/org-toc--insert-position ()
  "Return the tight position after leading Org file metadata."
  (save-excursion
    (goto-char (point-min))
    (my/org-toc--skip-blank-lines)
    (let ((continue t))
      (while (and continue (not (eobp)))
        (cond
         ((my/org-toc--metadata-at-point-p)
          (my/org-toc--forward-metadata)
          (let ((blank-start (point)))
            (my/org-toc--skip-blank-lines)
            (unless (my/org-toc--metadata-at-point-p)
              (goto-char blank-start)
              (setq continue nil))))
         (t
          (setq continue nil)))))
    (point)))

(defun my/org-toc--headline-link (title)
  "Return an Org fuzzy link to headline TITLE."
  (org-link-make-string (concat "*" title) (my/org-toc--display-title title)))

(defun my/org-toc--display-title (title)
  "Return TITLE suitable as readable TOC link text."
  (let ((text (copy-sequence title)))
    (while (string-match "\\[\\[[^]\n]+\\]\\[\\([^]\n]+\\)\\]\\]" text)
      (setq text (replace-match "\\1" t nil text)))
    (while (string-match "\\[\\[\\([^]\n]+\\)\\]\\]" text)
      (setq text (replace-match "\\1" t nil text)))
    text))

(defun my/org-toc--headline-lines (&optional depth)
  "Return formatted TOC lines up to headline DEPTH.
This intentionally uses a cheap headline scan instead of a full
`org-element' parse."
  (let ((max-depth (or depth my/org-toc-depth))
        lines)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((components (org-heading-components))
                 (level (nth 0 components))
                 (title (string-trim (org-no-properties (or (nth 4 components) ""))))
                 (tags (nth 5 components)))
            (when (and level
                       (<= level max-depth)
                       (not (and tags (string-match-p ":no_toc:" tags)))
                       (not (string-empty-p title)))
              (push (format "%s- %s"
                            (make-string (* 2 (1- level)) ?\s)
                            (my/org-toc--headline-link title))
                    lines))))))
    (nreverse lines)))

(defun my/org-toc--contents (&optional depth)
  "Return the managed overview TOC block text for DEPTH."
  (let ((lines (my/org-toc--headline-lines depth)))
    (concat "#+begin_overview :toc t :depth " (number-to-string (or depth my/org-toc-depth)) "\n"
            (if lines
                (mapconcat #'identity lines "\n")
              "- 还没有标题。")
            "\n#+end_overview\n")))

(defun my/org-toc-insert-or-update (&optional depth)
  "Insert or update the managed TOC at the top of the current Org buffer.
The TOC is stored as a styled `overview' special block and follows `*'
headline levels."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((block (my/org-toc--find-block))
         (depth (cond
                 ((numberp depth) depth)
                 ((consp depth) (prefix-numeric-value depth))
                 (block (or (my/org-toc--block-depth block) my/org-toc-depth))
                 (t my/org-toc-depth)))
         (contents (my/org-toc--contents depth))
         (block (or block (my/org-toc--find-block)))
         (target (my/org-toc--insert-position)))
    (save-excursion
      (if block
          (unless (and (= (car block) target)
                       (string= (buffer-substring-no-properties (car block) (cdr block))
                                contents))
            (let ((delete-start (if (and (< target (car block))
                                         (string-blank-p
                                          (buffer-substring-no-properties
                                           target (car block))))
                                    target
                                  (car block))))
              (delete-region delete-start (cdr block)))
            (goto-char (my/org-toc--insert-position))
            (insert contents)
            (unless (looking-at-p "[ \t]*$")
              (insert "\n")))
        (goto-char (my/org-toc--insert-position))
        (unless (bolp)
          (insert "\n"))
        (insert contents "\n")))
    (my/org-toc--refresh-cached-block)))

(defun my/org-toc-update-if-present ()
  "Refresh the managed overview TOC when the current Org buffer has one."
  (when (and my/org-toc-auto-update-enabled
             (derived-mode-p 'org-mode)
             (<= (buffer-size) my/org-toc-auto-update-max-buffer-size)
             (or (my/org-toc--cached-block)
                 (my/org-toc--refresh-cached-block)))
    (my/org-toc-insert-or-update)))

(defun my/org-toc-initial-refresh-if-present ()
  "Run one active managed TOC refresh when an Org buffer is first set up."
  (when (and my/org-toc-auto-update-enabled
             (not my/org-toc--initial-refresh-done)
             (derived-mode-p 'org-mode)
             (<= (buffer-size) my/org-toc-auto-update-max-buffer-size)
             (my/org-toc--refresh-cached-block))
    (setq-local my/org-toc--initial-refresh-done t)
    (let ((inhibit-message t))
      (my/org-toc-update-if-present))))

(defun my/org-setup-toc-auto-update ()
  "Set up one-time managed overview TOC refresh."
  (my/org-toc-initial-refresh-if-present))

;;; ----------------------------------------------------------------------------
;;; 2. Org Core Configuration (核心设置)
;;; ----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)        ; 自动换行
         (org-mode . my/org-force-indent-mode)
         (org-mode . my/org-setup-buffer-spacing)
         (org-mode . my/org-setup-toc-auto-update)
         (org-mode . my/org-enable-typography-maybe)) ; 缩进模式
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c '" . nil)
         ("C-c C-'" . nil)
         ("C-c C-x T" . my/org-toc-insert-or-update)
         ("C-c C-q" . counsel-org-tag))

  :custom
  ;; --- Directories ---
  (org-directory my-org-root)
  (org-default-notes-file my-org-notes-file)
  (org-archive-location "%s_archive::datetree/")

  ;; --- Appearance Basics ---
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-use-sub-superscripts '{})
  (org-ellipsis " ▾")
  (org-image-actual-width nil)
  (org-startup-with-inline-images t)
  (org-display-remote-inline-images t)
  (org-imenu-depth 4)
  
  ;; --- Navigation & Editing ---
  (org-return-follows-link nil)
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; --- Todo Keywords & Faces ---
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "HOLD(h!)" "WIP(i!)" "WAIT(w@/!)" "|" 
               "DONE(d!)" "CANCELLED(c@/!)")))

  (org-todo-keyword-faces
   `(("TODO"      . (:foreground ,(aaron-ui-color 'accent-red) :weight medium))
     ("NEXT"      . (:foreground ,(aaron-ui-color 'accent-green-soft) :weight medium))
     ("HOLD"      . (:foreground ,(aaron-ui-color 'accent-orange) :weight medium))
     ("WIP"       . (:foreground ,(aaron-ui-color 'accent-cyan) :weight medium))
     ("WAIT"      . (:foreground ,(aaron-ui-color 'accent-yellow) :weight medium))
     ("DONE"      . (:foreground ,(aaron-ui-color 'accent-blue) :weight medium :strike-through t))
     ("CANCELLED" . (:foreground ,(aaron-ui-color 'accent-red-soft) :weight medium :strike-through t))))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)
  (org-priority-faces
   `((?A :foreground ,(aaron-ui-color 'accent-red) :weight medium)
     (?B :foreground ,(aaron-ui-color 'accent-orange))
     (?C :foreground ,(aaron-ui-color 'accent-yellow))))

  ;; --- Properties ---
  (org-global-properties
   '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("STYLE_ALL" . "habit")))
  (org-cycle-hide-drawer-startup t) 

  ;; --- Refiling ---
  (org-refile-use-cache nil)
  (org-refile-targets '((nil . (:maxlevel . 9))
                        (org-agenda-files . (:maxlevel . 9))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  
  ;; --- Tags & Search ---
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)

  ;; --- Links ---
  (org-link-abbrev-alist
   '(("GitHub" . "https://github.com/")
     ("Google" . "https://google.com/search?q=")
     ("RFCs"   . "https://tools.ietf.org/html/")))
  
  ;; --- Citations ---
  (org-cite-global-bibliography pv/org-bibtex-files))

(add-hook 'hack-local-variables-hook #'my/org-force-indent-mode)
(my/org-force-indent-mode-in-existing-buffers)

;;; ----------------------------------------------------------------------------
;;; Misc fixes
;;; ----------------------------------------------------------------------------

;; pcomplete/org-mode/drawer calls (substring context 1 nil) without checking
;; whether context is empty, producing args-out-of-range when company triggers
;; completion inside a LaTeX fragment.  Guard against it here.
(with-eval-after-load 'org-pcomplete
  (define-advice pcomplete/org-mode/drawer
      (:around (orig) guard-empty-context)
    (condition-case nil
        (funcall orig)
      (args-out-of-range nil))))

(provide 'init-org-core)
;;; init-org-core.el ends here
