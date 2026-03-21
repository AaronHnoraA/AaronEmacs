;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)

(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function treesit-fold-close "treesit-fold" ())
(declare-function treesit-fold-close-all "treesit-fold" ())
(declare-function treesit-fold-mode "treesit-fold" (&optional arg))
(declare-function treesit-fold-open "treesit-fold" ())
(declare-function treesit-fold-open-all "treesit-fold" ())
(declare-function treesit-fold-toggle "treesit-fold" ())

(defgroup my/fold nil
  "Editor folding helpers."
  :group 'editing)

(defcustom my/fold-state-file
  (expand-file-name "fold-state.el"
                    (expand-file-name "var" user-emacs-directory))
  "Path used to persist fold states."
  :type 'file
  :group 'my/fold)

(defvar my/fold-state-table nil
  "Alist mapping file names to persisted fold start lines.")

(defvar my/fold-state-loaded nil
  "Whether fold state has been loaded from disk.")

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
  :config
  (global-hl-todo-mode 1)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t i" . hl-todo-insert)
         ("C-c t o" . hl-todo-occur)
         ("C-c t s" . hl-todo-rgrep)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
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
(defun my/fold--treesit-buffer-p ()
  "Return non-nil when the current buffer should prefer `treesit-fold'."
  (and (fboundp 'treesit-ready-p)
       (fboundp 'treesit-fold-mode)
       (ignore-errors (treesit-ready-p))
       (string-match-p "-ts-mode\\'" (symbol-name major-mode))))

(defun my/fold--ensure-backend ()
  "Enable the best available folding backend for the current buffer."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (and (my/fold--treesit-buffer-p)
             (not (bound-and-true-p treesit-fold-mode)))
    (treesit-fold-mode 1)))

(defun my/fold--use-treesit-p ()
  "Return non-nil when folding should use tree-sitter."
  (my/fold--ensure-backend)
  (bound-and-true-p treesit-fold-mode))

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

(defun my/fold--save-state-table ()
  "Persist fold state to disk."
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
     (message "Skip saving fold state: %s" (error-message-string err)))))

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

(defun my/fold-save-buffer-state ()
  "Persist fold state for the current buffer."
  (when buffer-file-name
    (my/fold--load-state-table)
    (let ((key (my/fold--state-key))
          (lines (my/fold--current-hidden-start-lines)))
      (setq my/fold-state-table
            (assoc-delete-all key my/fold-state-table))
      (when lines
        (push (cons key lines) my/fold-state-table))
      (my/fold--save-state-table))))

(defun my/fold-restore-buffer-state ()
  "Restore fold state for the current buffer."
  (when buffer-file-name
    (my/fold--load-state-table)
    (when-let* ((entry (assoc (my/fold--state-key) my/fold-state-table)))
      (my/fold--ensure-backend)
      (dolist (line (cdr entry))
        (my/fold--apply-close-at-line line)))))

(defun my/fold-restore-buffer-state-deferred ()
  "Restore fold state after the current file finishes opening."
  (when (and buffer-file-name
             (derived-mode-p 'prog-mode))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       0.15 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (ignore-errors
               (my/fold-restore-buffer-state)))))
       buffer))))

(defun my/hs-set-up-overlay (overlay)
  "Render a concise folding indicator for hidden OVERLAY."
  (when (eq 'code (overlay-get overlay 'hs))
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (lines (max 1 (count-lines start end))))
      (overlay-put overlay 'display
                   (format " ... [%d lines] " lines))
      (overlay-put overlay 'help-echo
                   (format "Hidden code block: %d lines" lines)))))

(defun my/fold-toggle ()
  "Toggle the fold at point."
  (interactive)
  (if (my/fold--use-treesit-p)
      (call-interactively #'treesit-fold-toggle)
    (call-interactively #'hs-toggle-hiding))
  (my/fold-save-buffer-state))

(defun my/fold-open ()
  "Open the fold at point."
  (interactive)
  (if (my/fold--use-treesit-p)
      (call-interactively #'treesit-fold-open)
    (call-interactively #'hs-show-block))
  (my/fold-save-buffer-state))

(defun my/fold-close ()
  "Close the fold at point."
  (interactive)
  (if (my/fold--use-treesit-p)
      (call-interactively #'treesit-fold-close)
    (call-interactively #'hs-hide-block))
  (my/fold-save-buffer-state))

(defun my/fold-open-all ()
  "Open all folds in the current buffer."
  (interactive)
  (if (my/fold--use-treesit-p)
      (call-interactively #'treesit-fold-open-all)
    (call-interactively #'hs-show-all))
  (my/fold-save-buffer-state))

(defun my/fold-close-all ()
  "Close all folds in the current buffer."
  (interactive)
  (if (my/fold--use-treesit-p)
      (call-interactively #'treesit-fold-close-all)
    (call-interactively #'hs-hide-all))
  (my/fold-save-buffer-state))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . my/fold--ensure-backend)
  :custom
  (hs-show-indicators t)
  (hs-display-lines-hidden t)
  (hs-indicator-type (if (display-graphic-p) 'fringe 'margin))
  (hs-hide-comments-when-hiding-all nil)
  (hs-set-up-overlay #'my/hs-set-up-overlay))

(my/leader-key-label "z" "fold")
(my/evil-global-leader-set "z a" #'my/fold-toggle "toggle fold")
(my/evil-global-leader-set "z o" #'my/fold-open "open fold")
(my/evil-global-leader-set "z c" #'my/fold-close "close fold")
(my/evil-global-leader-set "z R" #'my/fold-open-all "open all folds")
(my/evil-global-leader-set "z M" #'my/fold-close-all "close all folds")

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "za") #'my/fold-toggle)
  (evil-define-key* 'normal 'global (kbd "zo") #'my/fold-open)
  (evil-define-key* 'normal 'global (kbd "zc") #'my/fold-close)
  (evil-define-key* 'normal 'global (kbd "zR") #'my/fold-open-all)
  (evil-define-key* 'normal 'global (kbd "zM") #'my/fold-close-all))

(add-hook 'find-file-hook #'my/fold-restore-buffer-state-deferred)
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
