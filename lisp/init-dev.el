;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)

(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function org-at-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-fold-folded-p "org-fold" (&optional pos))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-all "org-fold" (&optional state))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-fold-show-subtree "org-fold" ())
(declare-function my/org-flush-visible-ranges "init-org-ui" (&optional buffer))
(declare-function my/org-latex-preview-visible-debounced "init-org-latex" (&optional window))
(declare-function my/org-schedule-pretty-block-refontify "init-org-ui" (&optional force))
(declare-function my/org-schedule-visible-inline-image-refresh "init-org-core" (&optional force refresh))
(declare-function org-overview "org" (&optional arg))
(declare-function hs-block-positions "hideshow" (&optional adjust-beg adjust-end))
(declare-function hs-discard-overlays "hideshow" (beg end))
(declare-function hs-get-near-block "hideshow" (&optional include-comment))
(declare-function hs-indicator-mouse-toggle-hiding "hideshow" (event))
(declare-function hs-overlay-at "hideshow" (position))
(declare-function hs-toggle-hiding "hideshow" (&optional e))
(declare-function treesit-fold-indicators-mode "treesit-fold-indicators" (&optional arg))
(declare-function treesit-fold-close "treesit-fold" ())
(declare-function treesit-fold-close-all "treesit-fold" ())
(declare-function treesit-fold-open-recursively "treesit-fold" ())
(declare-function treesit-fold-mode "treesit-fold" (&optional arg))
(declare-function treesit-fold-open "treesit-fold" ())
(declare-function treesit-fold-open-all "treesit-fold" ())
(declare-function treesit-fold-toggle "treesit-fold" ())

(defvar hs-allow-nesting)
(defvar hs-hide-comments-when-hiding-all)
(defvar hs-indicators-map)
(defvar hs-minor-mode-map)

(defgroup my/fold nil
  "Editor folding helpers."
  :group 'editing)

(defcustom my/fold-state-file
  (expand-file-name "fold-state.el"
                    (expand-file-name "var" user-emacs-directory))
  "Path used to persist fold states."
  :type 'file
  :group 'my/fold)

(defcustom my/fold-prog-startup 'fold-all
  "Default fold action for code buffers without a persisted fold state.

When set to `fold-all', newly opened code buffers enter a compact outline by
default.  This startup fold is intentionally not persisted; explicit fold
commands still save their state."
  :type '(choice (const :tag "Fold all blocks" fold-all)
                 (const :tag "Leave buffer open" nil))
  :group 'my/fold)

(defvar my/fold-state-table nil
  "Alist mapping file names to persisted fold start lines.")

(defvar my/fold-state-loaded nil
  "Whether fold state has been loaded from disk.")

(defvar-local my/fold--buffer-state-dirty nil
  "Non-nil when this buffer's fold state was changed through `my/fold'.")

;; Compilation Mode
(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  ;; Save all buffers on M-x `compile'
  (compilation-ask-about-save nil))

;; The unified debugger
(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

;; GDB specific config
(use-package gdb-mi
  :ensure nil
  :commands gdb
  :custom
  (gdb-show-main t)
  (gdb-display-io-nopopup t)
  (gdb-show-changed-values t)
  (gdb-delete-out-of-scope t)
  (gdb-use-colon-colon-notation t)
  (gdb-debuginfod-enable-setting nil)
  (gdb-restore-window-configuration-after-quit t))

;; #number can be clickable.
(use-package bug-reference
  :ensure nil
  :bind (:map bug-reference-map
         ("C-c C-o" . bug-reference-push-button)))

;; Insert SPDX license header
(use-package spdx
  :ensure t
  :hook (prog-mode . spdx-tempo-setup)
  :custom
  (spdx-ignore-deprecated t))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :defer 2
  :hook ((prog-mode
          text-mode
          conf-mode
          org-mode
          markdown-mode) . hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t i" . hl-todo-insert)
         ("C-c t o" . hl-todo-occur)
         ("C-c t s" . hl-todo-rgrep)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face trailing)))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :ensure t
  :bind ("C-c x" . quickrun)
  :custom
  (quickrun-focus-p nil)
  (quickrun-input-file-extension ".qr"))

;; xref
(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             ((executable-find "ugrep") 'ugrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  ;; Use Consult so workspace/project symbols can preview target locations.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; A fancy ctags frontend
(use-package citre
  :ensure t
  :init
  ;; Load the prelude.
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-query-jump)
         ("C-c c /" . citre-jump-to-reference)
         ("C-c c ?" . citre-query-jump-to-reference)
         ("C-c c u" . citre-update-this-tags-file)
         ("C-c c g" . citre-global-update-database))
  :custom
  (citre-enable-capf-integration nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

;; Browse devdocs.io
(use-package devdocs
  :ensure t
  :bind ("C-c b" . devdocs-lookup)
  :config
  (add-to-list 'completion-category-overrides '(devdocs (styles . (flex))))
  :custom
  (devdocs-window-select t))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(defun my/fold--org-buffer-p ()
  "Return non-nil when the current buffer should use Org folding."
  (derived-mode-p 'org-mode))

(defun my/fold--treesit-buffer-p ()
  "Return non-nil when the current buffer should prefer `treesit-fold'."
  (and (fboundp 'treesit-ready-p)
       (fboundp 'treesit-fold-mode)
       (ignore-errors (treesit-ready-p nil t))
       (string-match-p "-ts-mode\\'" (symbol-name major-mode))))

(defun my/fold--backend ()
  "Return the preferred fold backend for the current buffer."
  (cond
   ((my/fold--org-buffer-p) 'org)
   ((my/fold--treesit-buffer-p) 'treesit)
   ((derived-mode-p 'prog-mode) 'hs)
   (t nil)))

(defun my/fold--ensure-backend ()
  "Enable the best available folding backend for the current buffer."
  (pcase (my/fold--backend)
    ('treesit
     (when (fboundp 'hs-minor-mode)
       (hs-minor-mode 1))
     (when (not (bound-and-true-p treesit-fold-mode))
       (treesit-fold-mode 1))
     (when (fboundp 'treesit-fold-indicators-mode)
       (treesit-fold-indicators-mode 1)))
    ('hs
     (when (fboundp 'hs-minor-mode)
       (hs-minor-mode 1)))))

(defun my/fold--use-treesit-p ()
  "Return non-nil when folding should use tree-sitter."
  (my/fold--ensure-backend)
  (bound-and-true-p treesit-fold-mode))

(defun my/fold--org-back-to-heading ()
  "Move point to the current Org heading or raise a user-facing error."
  (unless (my/fold--org-buffer-p)
    (user-error "Not in an Org buffer"))
  (unless (or (org-at-heading-p)
              (ignore-errors
                (org-back-to-heading t)
                t))
    (user-error "Point is not inside an Org heading")))

(defun my/fold--org-folded-p ()
  "Return non-nil when the current Org heading is folded."
  (save-excursion
    (end-of-line)
    (org-fold-folded-p)))

(defun my/fold--refresh-visible-org-rendering (&rest _)
  "Refresh visible Org rendering that intentionally ignores folded text."
  (when (my/fold--org-buffer-p)
    (when (boundp 'my/org-latex--last-visible-range)
      (setq my/org-latex--last-visible-range nil))
    (when (fboundp 'my/org-flush-visible-ranges)
      (my/org-flush-visible-ranges (current-buffer)))
    (when (fboundp 'my/org-schedule-visible-inline-image-refresh)
      (my/org-schedule-visible-inline-image-refresh t t))
    (when (fboundp 'my/org-schedule-pretty-block-refontify)
      (my/org-schedule-pretty-block-refontify t))
    (when (fboundp 'my/org-latex-preview-visible-debounced)
      (my/org-latex-preview-visible-debounced
       (and (window-live-p (selected-window))
            (selected-window))))))

(defun my/fold--org-toggle ()
  "Toggle the current Org heading subtree."
  (save-excursion
    (my/fold--org-back-to-heading)
    (if (my/fold--org-folded-p)
        (my/fold--org-open)
      (my/fold--org-close))))

(defun my/fold--org-open ()
  "Open the current Org heading one level."
  (save-excursion
    (my/fold--org-back-to-heading)
    (org-fold-show-entry)
    (org-fold-show-children))
  (my/fold--refresh-visible-org-rendering))

(defun my/fold--org-close ()
  "Close the current Org heading subtree."
  (save-excursion
    (my/fold--org-back-to-heading)
    (org-fold-hide-subtree))
  (my/fold--refresh-visible-org-rendering))

(defun my/fold--org-open-zone ()
  "Open the current Org heading subtree recursively."
  (save-excursion
    (my/fold--org-back-to-heading)
    (org-fold-show-subtree))
  (my/fold--refresh-visible-org-rendering))

(defun my/fold--state-key (&optional file)
  "Return canonical persisted key for FILE or current buffer."
  (when-let* ((path (or file buffer-file-name)))
    (expand-file-name path)))

(defun my/fold--load-state-table ()
  "Load persisted fold state from disk once."
  (unless my/fold-state-loaded
    (setq my/fold-state-loaded t)
    (when (file-exists-p my/fold-state-file)
      (with-temp-buffer
        (insert-file-contents my/fold-state-file)
        (goto-char (point-min))
        (setq my/fold-state-table (read (current-buffer)))))))

(defun my/fold--state-entry ()
  "Return persisted fold state entry for the current buffer, if any."
  (when-let* ((key (my/fold--state-key)))
    (assoc key my/fold-state-table)))

(defun my/fold--save-state-table ()
  "Persist fold state to disk."
  (unless noninteractive
    (my/fold--load-state-table)
    (condition-case err
        (progn
          (make-directory (file-name-directory my/fold-state-file) t)
          (with-temp-file my/fold-state-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 my/fold-state-table (current-buffer))
              (insert "\n"))))
      (file-locked
       (message "Skip saving fold state: %s" (error-message-string err))))))

(defun my/fold--current-hidden-start-lines ()
  "Return sorted start lines for currently hidden folds."
  (save-excursion
    (sort
     (delete-dups
      (mapcar
       (lambda (ov)
         (line-number-at-pos (overlay-start ov)))
       (seq-filter
        (lambda (ov)
          (or (eq (overlay-get ov 'invisible) 'treesit-fold)
              (eq (overlay-get ov 'hs) 'code)))
        (overlays-in (point-min) (point-max)))))
     #'<)))

(defun my/fold--apply-close-at-line (line)
  "Close the fold starting at LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (back-to-indentation)
    (condition-case nil
        (if (my/fold--use-treesit-p)
            (treesit-fold-close)
          (hs-hide-block))
      (error nil))))

(defun my/fold--hs-zone-range ()
  "Return the hideshow fold zone around point."
  (save-excursion
    (if-let* ((overlay (hs-overlay-at (line-end-position))))
        (cons (overlay-start overlay) (overlay-end overlay))
      (when (ignore-errors
              (hs-get-near-block hs-hide-comments-when-hiding-all))
        (when-let* ((block (or (ignore-errors
                                  (hs-block-positions :adjust-beg :adjust-end))
                                (ignore-errors (hs-block-positions)))))
          (pcase-let ((`(,beg ,end) block))
            (when (and beg end (< beg end))
              (cons beg end))))))))

(defun my/fold--hs-open-zone ()
  "Open all hideshow folds in the current fold zone."
  (if-let* ((range (my/fold--hs-zone-range)))
      (let (hs-allow-nesting)
        (hs-discard-overlays (car range) (cdr range)))
    (call-interactively #'hs-show-block)))

(defun my/fold-save-buffer-state ()
  "Persist fold state for the current buffer."
  (when buffer-file-name
    (my/fold--load-state-table)
    (let ((entry (my/fold--state-entry)))
      (when (or my/fold--buffer-state-dirty entry)
        (let ((key (my/fold--state-key))
              (lines (my/fold--current-hidden-start-lines)))
          (setq my/fold-state-table
                (assoc-delete-all key my/fold-state-table))
          (when lines
            (push (cons key lines) my/fold-state-table))
          (my/fold--save-state-table))))))

(defun my/fold--mark-buffer-state-dirty-and-save ()
  "Mark the current buffer's fold state as user-managed and persist it."
  (setq my/fold--buffer-state-dirty t)
  (my/fold-save-buffer-state))

(defun my/fold--apply-default-prog-startup ()
  "Apply the default startup fold policy for code buffers."
  (when (and (eq my/fold-prog-startup 'fold-all)
             (derived-mode-p 'prog-mode))
    (my/fold--ensure-backend)
    (pcase (my/fold--backend)
      ('treesit
       (when (bound-and-true-p treesit-fold-mode)
         (ignore-errors (treesit-fold-close-all))))
      ('hs
       (ignore-errors (hs-hide-all))))))

(defun my/fold-restore-buffer-state ()
  "Restore fold state for the current buffer."
  (when buffer-file-name
    (my/fold--load-state-table)
    (if-let* ((entry (my/fold--state-entry)))
        (progn
          (my/fold--ensure-backend)
          (dolist (line (cdr entry))
            (my/fold--apply-close-at-line line)))
      (my/fold--apply-default-prog-startup))))

(defun my/fold-restore-existing-buffer-states ()
  "Apply startup fold restoration for all live file buffers after startup.

This covers buffers opened before `find-file-hook' started restoring folds."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (derived-mode-p 'prog-mode))
        (ignore-errors
          (my/fold-restore-buffer-state))))))

(defun my/fold-restore-buffer-state-on-open ()
  "Restore fold state as soon as the current file finishes opening."
  (when (and buffer-file-name
             (derived-mode-p 'prog-mode))
    (ignore-errors
      (my/fold-restore-buffer-state))))

(defun my/hs-set-up-overlay (overlay)
  "Render a concise folding indicator for hidden OVERLAY."
  (when (eq 'code (overlay-get overlay 'hs))
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (lines (max 1 (count-lines start end)))
           (map (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] #'my/fold-hs-mouse-toggle)
                  map)))
      (overlay-put overlay 'display
                   (propertize
                    (format " ... [%d lines] " lines)
                    'mouse-face 'highlight
                    'keymap map))
      (overlay-put overlay 'help-echo
                   (format "Hidden code block: %d lines; mouse-1 toggles"
                           lines)))))

(defun my/fold-hs-mouse-toggle (event)
  "Toggle a hideshow fold from mouse EVENT and persist the new state."
  (interactive "e")
  (let ((area (and (mouse-event-p event)
                   (posn-area (event-start event)))))
    (if (memq area '(left-fringe left-margin right-fringe right-margin))
        (hs-indicator-mouse-toggle-hiding event)
      (hs-toggle-hiding event)))
  (my/fold--mark-buffer-state-dirty-and-save))

(defun my/fold-toggle ()
  "Toggle the fold at point."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (my/fold--org-toggle))
    ('treesit
     (call-interactively #'treesit-fold-toggle)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-toggle-hiding)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun my/fold-open ()
  "Open the fold at point."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (my/fold--org-open))
    ('treesit
     (call-interactively #'treesit-fold-open)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-show-block)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun my/fold-close ()
  "Close the fold at point."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (my/fold--org-close))
    ('treesit
     (call-interactively #'treesit-fold-close)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-hide-block)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun my/fold-open-all ()
  "Open all folds in the current buffer."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (org-fold-show-all)
     (my/fold--refresh-visible-org-rendering))
    ('treesit
     (call-interactively #'treesit-fold-open-all)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-show-all)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun my/fold-open-zone ()
  "Open all folds inside the current fold zone."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (my/fold--org-open-zone))
    ('treesit
     (call-interactively #'treesit-fold-open-recursively)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (my/fold--hs-open-zone)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun my/fold-close-all ()
  "Close all folds in the current buffer."
  (interactive)
  (pcase (my/fold--backend)
    ('org
     (org-overview)
     (my/fold--refresh-visible-org-rendering))
    ('treesit
     (call-interactively #'treesit-fold-close-all)
     (my/fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-hide-all)
     (my/fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . my/fold--ensure-backend)
  :custom
  (hs-allow-nesting t)
  (hs-show-indicators t)
  (hs-display-lines-hidden t)
  (hs-indicator-type (if (display-graphic-p) 'fringe 'margin))
  (hs-hide-comments-when-hiding-all nil)
  (hs-set-up-overlay #'my/hs-set-up-overlay))

(with-eval-after-load 'hideshow
  (keymap-set hs-indicators-map "<mouse-1>" #'my/fold-hs-mouse-toggle)
  (keymap-set hs-indicators-map "<left-margin> <mouse-1>"
              #'my/fold-hs-mouse-toggle)
  (keymap-set hs-minor-mode-map "<left-fringe> <mouse-1>"
              #'my/fold-hs-mouse-toggle))

(with-eval-after-load 'treesit-fold
  (defun my/fold--treesit-command-save-a (&rest _)
    "Persist tree-sitter fold state after direct mouse/backend commands."
    (when (and buffer-file-name
               (derived-mode-p 'prog-mode)
               (bound-and-true-p treesit-fold-mode))
      (my/fold--mark-buffer-state-dirty-and-save)))

  (dolist (command '(treesit-fold-open
                     treesit-fold-close
                     treesit-fold-toggle))
    (advice-add command :after #'my/fold--treesit-command-save-a)))

(with-eval-after-load 'org
  (add-hook 'org-cycle-hook #'my/fold--refresh-visible-org-rendering))

(my/leader!
  "z"   '(:ignore t :which-key "fold")
  "z a" '(:def my/fold-toggle :which-key "toggle fold")
  "z o" '(:def my/fold-open :which-key "open fold")
  "z O" '(:def my/fold-open-zone :which-key "open fold zone")
  "z c" '(:def my/fold-close :which-key "close fold")
  "z R" '(:def my/fold-open-all :which-key "open all folds")
  "z M" '(:def my/fold-close-all :which-key "close all folds"))

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "za") #'my/fold-toggle)
  (evil-define-key* 'normal 'global (kbd "zo") #'my/fold-open)
  (evil-define-key* 'normal 'global (kbd "zO") #'my/fold-open-zone)
  (evil-define-key* 'normal 'global (kbd "zc") #'my/fold-close)
  (evil-define-key* 'normal 'global (kbd "zR") #'my/fold-open-all)
  (evil-define-key* 'normal 'global (kbd "zM") #'my/fold-close-all))

(general-define-key
 "H-<tab>" #'my/fold-toggle
 "H-TAB" #'my/fold-toggle
 "H-S-<tab>" #'my/fold-open-zone
 "H-<backtab>" #'my/fold-open-zone
 "H-S-TAB" #'my/fold-open-zone)

(add-hook 'find-file-hook #'my/fold-restore-buffer-state-on-open)
(add-hook 'emacs-startup-hook #'my/fold-restore-existing-buffer-states)
(add-hook 'kill-buffer-hook #'my/fold-save-buffer-state)
(add-hook 'kill-emacs-hook #'my/fold--save-state-table)

;; Antlr mode
(use-package antlr-mode
  :ensure nil
  :mode ("\\.g4\\'" . antlr-mode))

;; XML
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; The dot-language
(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :custom
  (graphviz-dot-indent-width 2))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json")))))


(provide 'init-dev)

;;; init-dev.el ends here
