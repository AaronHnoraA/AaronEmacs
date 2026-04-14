;;; init-org-core.el --- Core Org paths and defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Refactored Org Configuration.
;; 1. Fixed load order: org-modern loads after org.
;; 2. Optimized directory creation and path handling.
;; 3. Enhanced robustness of special block rendering (nested blocks support).
;; 4. Performance tuning for on-demand LaTeX previews.

;;; Code:

(declare-function my/typography-setup-prose-buffer "init-base")
(require 'init-funcs)
(require 'org)
(require 'org-element)

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

(defun my/org-rich-ui-buffer-p ()
  "Return non-nil when the current Org buffer should use rich UI helpers."
  (my/rich-ui-buffer-p nil my/org-rich-ui-max-buffer-size))

(defun my/org-enable-typography-maybe ()
  "Enable shared prose typography for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (my/typography-setup-prose-buffer)))

(defun my/org-setup-buffer-spacing ()
  "Use relaxed line spacing in Org buffers only."
  (setq-local line-spacing 0.1))

;;; ----------------------------------------------------------------------------
;;; 2. Org Core Configuration (核心设置)
;;; ----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)        ; 自动换行
         (org-mode . org-indent-mode)
         (org-mode . my/org-setup-buffer-spacing)
         (org-mode . my/org-enable-typography-maybe) ; 缩进模式
         ;; org-appear 隐藏括号后括号内外无法区分，禁用自动括号补全
         (org-mode . (lambda () (electric-pair-local-mode -1))))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c '" . nil)
         ("C-c C-'" . nil)
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
   '(("TODO"      . (:foreground "#ff6c6b" :weight medium))
     ("NEXT"      . (:foreground "#98be65" :weight medium))
     ("HOLD"      . (:foreground "#feb24c" :weight medium))
     ("WIP"       . (:foreground "#0098dd" :weight medium))
     ("WAIT"      . (:foreground "#ecbe7b" :weight medium))
     ("DONE"      . (:foreground "#51afef" :weight medium :strike-through t))
     ("CANCELLED" . (:foreground "#ff6480" :weight medium :strike-through t))))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)
  (org-priority-faces
   '((?A :foreground "red" :weight medium)
     (?B :foreground "orange")
     (?C :foreground "yellow")))

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

(provide 'init-org-core)
;;; init-org-core.el ends here
