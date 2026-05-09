;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function org-at-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-fold-folded-p "org-fold" (&optional pos))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-fold-region "org-fold" (from to flag &optional spec-or-alias))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-all "org-fold" (&optional state))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-fold-show-subtree "org-fold" ())
(declare-function my/org-clear-visible-ranges-cache "init-org-ui" (&optional buffer))
(declare-function my/org-latex-preview-visible-debounced "init-org-latex" (&optional window))
(declare-function my/org-schedule-pretty-block-refontify "init-org-ui" (&optional force))
(declare-function my/org-schedule-visible-inline-image-refresh "init-org-core" (&optional force refresh))
(declare-function org-overview "org" (&optional arg))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
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
(defvar flymake-mode)
(defvar org-heading-regexp)
(defvar org-mode-map)

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

(defcustom my/fold-diagnostics-enabled t
  "Show Flymake diagnostic summaries on folded regions."
  :type 'boolean
  :group 'my/fold)

(defcustom my/fold-diagnostics-idle-delay 0.12
  "Idle delay before refreshing folded diagnostic summaries."
  :type 'number
  :group 'my/fold)

(defvar my/fold-state-table nil
  "Alist mapping file names to persisted fold start lines.")

(defvar my/fold-state-loaded nil
  "Whether fold state has been loaded from disk.")

(defvar-local my/fold--buffer-state-dirty nil
  "Non-nil when this buffer's fold state was changed through `my/fold'.")

(defvar-local my/fold-diagnostics--timer nil
  "Pending timer for folded diagnostic summary refresh.")

(defvar-local my/fold-diagnostics--overlays nil
  "Overlays showing diagnostics summarized onto folded lines.")

(defvar-local my/fold--org-render-timer nil
  "Pending lightweight Org render refresh after fold changes.")

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

(defun my/fold--org-heading-at-or-above-p ()
  "Move to the current Org heading and return non-nil when one exists."
  (and (my/fold--org-buffer-p)
       (or (org-at-heading-p)
           (ignore-errors
             (org-back-to-heading t)
             t))))

(defun my/fold--range-visible-p (beg end)
  "Return non-nil when any character between BEG and END is visible."
  (let ((pos (max (point-min) (min beg end)))
        (end (min (point-max) (max beg end)))
        visible)
    (while (and (< pos end) (not visible))
      (let ((next (or (next-single-char-property-change
                       pos 'invisible nil end)
                      end)))
        (unless (invisible-p pos)
          (setq visible t))
        (setq pos (if (> next pos) next (1+ pos)))))
    visible))

(defun my/fold--range-nonblank-visible-p (beg end)
  "Return non-nil when any non-blank character between BEG and END is visible."
  (let ((pos (max (point-min) (min beg end)))
        (end (min (point-max) (max beg end)))
        visible)
    (while (and (< pos end) (not visible))
      (when (and (not (invisible-p pos))
                 (not (memq (char-after pos) '(?\s ?\t ?\n ?\r))))
        (setq visible t))
      (setq pos (1+ pos)))
    visible))

(defun my/fold--org-direct-body-range ()
  "Return the direct body range for the current Org heading, or nil.
The direct body is the text after the heading line and before the first child
heading, or before the subtree end when there is no child."
  (save-excursion
    (my/fold--org-back-to-heading)
    (let* ((body-beg (save-excursion
                       (forward-line 1)
                       (point)))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)))
           (body-end
            (save-excursion
              (forward-line 1)
              (if (re-search-forward org-heading-regexp subtree-end t)
                  (line-beginning-position)
                subtree-end))))
      (when (< body-beg body-end)
        (cons body-beg body-end)))))

(defun my/fold--org-direct-body-visible-p ()
  "Return non-nil when the current Org heading's direct body is visible."
  (when-let* ((range (my/fold--org-direct-body-range)))
    (my/fold--range-nonblank-visible-p (car range) (cdr range))))

(defun my/fold--org-show-direct-body ()
  "Explicitly show the current Org heading's direct body."
  (when-let* ((range (my/fold--org-direct-body-range)))
    (org-fold-region (car range) (cdr range) nil)))

(defun my/fold--org-subtree-collapsed-p ()
  "Return non-nil when the current Org heading's direct body is hidden.
Visible child headings do not count as open here.  In Org's contents/overview
states, child headings may be visible while the useful body is still folded."
  (save-excursion
    (my/fold--org-back-to-heading)
    (if-let* ((range (my/fold--org-direct-body-range)))
        (not (my/fold--range-nonblank-visible-p (car range) (cdr range)))
      (let ((content-beg (save-excursion
                           (forward-line 1)
                           (point)))
            (subtree-end (save-excursion
                           (org-end-of-subtree t t))))
        (and (< content-beg subtree-end)
             (not (my/fold--range-nonblank-visible-p
                   content-beg subtree-end)))))))

(defun my/fold--org-heading-body-visible-p ()
  "Return non-nil when the current Org heading has visible body text."
  (my/fold--org-direct-body-visible-p))

(defun my/fold--org-any-heading-body-visible-p ()
  "Return non-nil when any Org heading body in this buffer is visible."
  (catch 'visible
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (goto-char (line-beginning-position))
          (when (my/fold--org-heading-body-visible-p)
            (throw 'visible t))
          (forward-line 1))))
    nil))

(defun my/fold--org-toggle-buffer ()
  "Toggle whole-buffer Org folding when point is outside any heading."
  (if (my/fold--org-any-heading-body-visible-p)
      (org-overview)
    (org-fold-show-all))
  (my/fold--refresh-visible-org-rendering))

(defun my/fold--org-cancel-render-refresh ()
  "Cancel pending Org rendering refresh for fold changes."
  (when (timerp my/fold--org-render-timer)
    (cancel-timer my/fold--org-render-timer))
  (setq-local my/fold--org-render-timer nil))

(defun my/fold--org-render-refresh-now (buffer window)
  "Refresh visible Org renderers for BUFFER after fold changes.
WINDOW is used as a hint for LaTeX preview scheduling."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local my/fold--org-render-timer nil)
      (when (my/fold--org-buffer-p)
        (when (fboundp 'my/org-schedule-visible-inline-image-refresh)
          (my/org-schedule-visible-inline-image-refresh t t))
        (when (fboundp 'my/org-schedule-pretty-block-refontify)
          (my/org-schedule-pretty-block-refontify t))
        (when (fboundp 'my/org-latex-preview-visible-debounced)
          (my/org-latex-preview-visible-debounced
           (and (window-live-p window) window)))))))

(defun my/fold--refresh-visible-org-rendering (&rest _)
  "Refresh visible Org rendering that intentionally ignores folded text."
  (when (my/fold--org-buffer-p)
    (when (boundp 'my/org-latex--last-visible-range)
      (setq my/org-latex--last-visible-range nil))
    (when (fboundp 'my/org-clear-visible-ranges-cache)
      (my/org-clear-visible-ranges-cache (current-buffer)))
    (font-lock-flush (line-beginning-position) (line-end-position))
    (my/fold-diagnostics-clear)
    (my/fold--org-cancel-render-refresh)
    (setq-local my/fold--org-render-timer
                (run-with-idle-timer
                 0.12 nil
                 #'my/fold--org-render-refresh-now
                 (current-buffer)
                 (and (window-live-p (selected-window))
                      (selected-window))))))

(defun my/fold--org-toggle ()
  "Toggle the current Org heading subtree."
  (save-excursion
    (if (my/fold--org-heading-at-or-above-p)
        (if (my/fold--org-subtree-collapsed-p)
            (my/fold--org-open)
          (my/fold--org-close))
      (my/fold--org-toggle-buffer))))

(defun my/fold--org-open ()
  "Open the current Org heading one level."
  (save-excursion
    (my/fold--org-back-to-heading)
    (org-fold-show-entry)
    (my/fold--org-show-direct-body)
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

(defun my/fold-diagnostics-cancel ()
  "Cancel pending folded diagnostic summary refresh."
  (when (timerp my/fold-diagnostics--timer)
    (cancel-timer my/fold-diagnostics--timer))
  (setq-local my/fold-diagnostics--timer nil))

(defun my/fold-diagnostics-clear ()
  "Delete folded diagnostic summary overlays in the current buffer."
  (mapc #'delete-overlay my/fold-diagnostics--overlays)
  (setq-local my/fold-diagnostics--overlays nil))

(defun my/fold-diagnostics-cleanup ()
  "Release folded diagnostic summary state for this buffer."
  (my/fold-diagnostics-cancel)
  (my/fold-diagnostics-clear)
  (remove-hook 'change-major-mode-hook #'my/fold-diagnostics-cleanup t)
  (remove-hook 'kill-buffer-hook #'my/fold-diagnostics-cleanup t))

(defun my/fold-diagnostics--pos (pos)
  "Return POS as an integer buffer position, or nil."
  (cond
   ((integerp pos) pos)
   ((markerp pos) (marker-position pos))
   (t nil)))

(defun my/fold-diagnostics--normalize-range (range)
  "Return RANGE clamped to the current buffer, or nil when empty."
  (when (consp range)
    (let ((beg (my/fold-diagnostics--pos (car range)))
          (end (my/fold-diagnostics--pos (cdr range))))
      (when (and beg end)
        (setq beg (min (point-max) (max (point-min) beg))
              end (min (point-max) (max (point-min) end)))
        (when (< beg end)
          (cons beg end))))))

(defun my/fold-diagnostics--diag-range (diagnostic)
  "Return DIAGNOSTIC as a normalized buffer range, or nil."
  (let* ((beg (my/fold-diagnostics--pos
               (ignore-errors (flymake-diagnostic-beg diagnostic))))
         (end (my/fold-diagnostics--pos
               (ignore-errors (flymake-diagnostic-end diagnostic)))))
    (when beg
      (setq end (or end beg))
      (my/fold-diagnostics--normalize-range
       (cons beg (max end (1+ beg)))))))

(defun my/fold-diagnostics--diag-kind (diagnostic)
  "Return DIAGNOSTIC kind as one of `error', `warning' or `note'."
  (let* ((type (or (ignore-errors (flymake-diagnostic-type diagnostic))
                   :error))
         (category (or (and (symbolp type) (get type 'flymake-category))
                       type)))
    (cond
     ((memq category '(flymake-error :error error)) 'error)
     ((memq category '(flymake-warning :warning warning)) 'warning)
     ((memq category '(flymake-note :note note)) 'note)
     (t 'error))))

(defun my/fold-diagnostics--diag-text (diagnostic)
  "Return a compact one-line message for DIAGNOSTIC."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]\n\r\t]+" " "
    (or (ignore-errors (flymake-diagnostic-text diagnostic)) ""))))

(defun my/fold-diagnostics--collect ()
  "Return normalized Flymake diagnostics for the current buffer."
  (delq
   nil
   (mapcar
    (lambda (diagnostic)
      (when-let* ((range (my/fold-diagnostics--diag-range diagnostic)))
        (list :range range
              :kind (my/fold-diagnostics--diag-kind diagnostic)
              :text (my/fold-diagnostics--diag-text diagnostic))))
    (ignore-errors (flymake-diagnostics)))))

(defun my/fold-diagnostics--overlay-fold-ranges ()
  "Return folded hideshow/tree-sitter ranges as DISPLAY-POS . RANGE pairs."
  (let (ranges)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (or (eq (overlay-get overlay 'invisible) 'treesit-fold)
                (eq (overlay-get overlay 'hs) 'code))
        (when-let* ((range (my/fold-diagnostics--normalize-range
                            (cons (overlay-start overlay)
                                  (overlay-end overlay)))))
          (push (cons (max (point-min) (1- (overlay-start overlay)))
                      range)
                ranges))))
    (sort ranges (lambda (left right) (< (car left) (car right))))))

(defun my/fold-diagnostics--backend-p ()
  "Return non-nil when folded diagnostics should run here."
  (memq (my/fold--backend) '(treesit hs)))

(defun my/fold-diagnostics--range-summary (diagnostics range)
  "Return a folded diagnostic summary for DIAGNOSTICS in RANGE."
  (pcase-let ((`(,beg . ,end) range))
    (let ((errors 0)
          (warnings 0)
          (notes 0)
          samples)
      (dolist (diagnostic diagnostics)
        (pcase-let* ((diag-range (plist-get diagnostic :range))
                     (`(,diag-beg . ,diag-end) diag-range))
          (when (and (< diag-beg end)
                     (> diag-end beg))
            (pcase (plist-get diagnostic :kind)
              ('error (setq errors (1+ errors)))
              ('warning (setq warnings (1+ warnings)))
              ('note (setq notes (1+ notes))))
            (when (< (length samples) 6)
              (push (format "%s: %s"
                            (pcase (plist-get diagnostic :kind)
                              ('error "error")
                              ('warning "warning")
                              (_ "note"))
                            (plist-get diagnostic :text))
                    samples)))))
      (let ((total (+ errors warnings notes)))
        (when (> total 0)
          (list :errors errors
                :warnings warnings
                :notes notes
                :samples (nreverse samples)))))))

(defun my/fold-diagnostics--format-summary (summary)
  "Return a propertized folded diagnostic SUMMARY string."
  (let (parts)
    (when (> (plist-get summary :errors) 0)
      (push (propertize (format "E:%d" (plist-get summary :errors))
                        'face 'flymake-error)
            parts))
    (when (> (plist-get summary :warnings) 0)
      (push (propertize (format "W:%d" (plist-get summary :warnings))
                        'face 'flymake-warning)
            parts))
    (when (> (plist-get summary :notes) 0)
      (push (propertize (format "N:%d" (plist-get summary :notes))
                        'face 'flymake-note)
            parts))
    (concat
     (propertize "  [" 'face 'shadow)
     (string-join (nreverse parts) " ")
     (propertize "]" 'face 'shadow))))

(defun my/fold-diagnostics--help (summary)
  "Return tooltip text for folded diagnostic SUMMARY."
  (concat "Folded diagnostics:\n"
          (string-join (plist-get summary :samples) "\n")))

(defun my/fold-diagnostics--put-summary (display-pos summary)
  "Place folded diagnostic SUMMARY on DISPLAY-POS's line."
  (save-excursion
    (goto-char display-pos)
    (let ((ov (make-overlay (line-end-position)
                            (line-end-position)
                            nil nil t)))
      (overlay-put ov 'my/fold-diagnostics t)
      (overlay-put ov 'after-string
                   (propertize (my/fold-diagnostics--format-summary summary)
                               'help-echo
                               (my/fold-diagnostics--help summary)))
      (overlay-put ov 'help-echo (my/fold-diagnostics--help summary))
      (overlay-put ov 'priority 2000)
      (push ov my/fold-diagnostics--overlays))))

(defun my/fold-refresh-diagnostics (&optional buffer)
  "Refresh folded diagnostic summaries for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (my/fold-diagnostics-cancel)
        (my/fold-diagnostics-clear)
        (when (and my/fold-diagnostics-enabled
                   (bound-and-true-p flymake-mode)
                   (fboundp 'flymake-diagnostics)
                   (my/fold-diagnostics--backend-p))
          (let ((diagnostics (my/fold-diagnostics--collect)))
            (when diagnostics
              (dolist (entry (my/fold-diagnostics--overlay-fold-ranges))
                (when-let* ((summary
                             (my/fold-diagnostics--range-summary
                              diagnostics (cdr entry))))
                  (my/fold-diagnostics--put-summary (car entry) summary))))))))))

(defun my/fold-schedule-diagnostics (&rest _)
  "Schedule folded diagnostic summary refresh for the current buffer."
  (when (and my/fold-diagnostics-enabled
             (my/fold-diagnostics--backend-p))
    (my/fold-diagnostics-cancel)
    (setq-local my/fold-diagnostics--timer
                (run-with-idle-timer
                 my/fold-diagnostics-idle-delay nil
                 #'my/fold-refresh-diagnostics
                 (current-buffer)))))

(defun my/fold-diagnostics--flymake-publish-a (&rest _)
  "Refresh folded diagnostic summaries after Flymake publishes diagnostics."
  (my/fold-schedule-diagnostics))

(defun my/fold-diagnostics-setup ()
  "Enable folded diagnostic summaries in fold-capable buffers."
  (when (and my/fold-diagnostics-enabled
             (my/fold-diagnostics--backend-p))
    (add-hook 'change-major-mode-hook #'my/fold-diagnostics-cleanup nil t)
    (add-hook 'kill-buffer-hook #'my/fold-diagnostics-cleanup nil t)
    (my/fold-schedule-diagnostics)))

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
  (my/fold-save-buffer-state)
  (my/fold-schedule-diagnostics))

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
       (ignore-errors (hs-hide-all))))
    (my/fold-schedule-diagnostics)))

(defun my/fold-restore-buffer-state ()
  "Restore fold state for the current buffer."
  (when buffer-file-name
    (my/fold--load-state-table)
    (if-let* ((entry (my/fold--state-entry)))
        (progn
          (my/fold--ensure-backend)
          (dolist (line (cdr entry))
            (my/fold--apply-close-at-line line))
          (my/fold-schedule-diagnostics))
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
  (add-hook 'org-cycle-hook #'my/fold--refresh-visible-org-rendering)
  (keymap-set org-mode-map "H-<tab>" #'my/fold-toggle)
  (keymap-set org-mode-map "H-TAB" #'my/fold-toggle)
  (keymap-set org-mode-map "H-S-<tab>" #'my/fold-open-zone)
  (keymap-set org-mode-map "H-<backtab>" #'my/fold-open-zone)
  (keymap-set org-mode-map "H-S-TAB" #'my/fold-open-zone))

(with-eval-after-load 'flymake
  (unless (advice-member-p #'my/fold-diagnostics--flymake-publish-a
                           'flymake--publish-diagnostics)
    (advice-add 'flymake--publish-diagnostics
                :after #'my/fold-diagnostics--flymake-publish-a)))

(add-hook 'prog-mode-hook #'my/fold-diagnostics-setup)

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
