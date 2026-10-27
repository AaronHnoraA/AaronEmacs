;;; editor/fold/config.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function remote-require-on-client "remote-core" (feature &optional filename noerror))
(declare-function yaml-indent-line "yaml-mode" ())

(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function citre-executable-find "citre-common-util"
                  (command &optional remote))
(defvar remote-connection-closed-hook)
(defvar remote-config-after-load-hook)
(declare-function citre-register-backend
                  "citre-backend-interface" (name backend))
(declare-function citre-xref-backend-to-citre-backend
                  "citre-xref-adapter" (backend usable-p &rest keys))
(declare-function +language-server/ensure "tools/lsp/config" ())
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
(declare-function +org-clear-visible-ranges-cache "init-org-ui" (&optional buffer))
(declare-function +org-schedule-visible-inline-image-refresh "init-org-core" (&optional force refresh))
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
(defvar lsp-managed-mode)
(defvar org-heading-regexp)
(defvar org-mode-map)

(defgroup +fold nil
  "Editor folding helpers."
  :group 'editing)

(config-defvar +fold-state-file nil
  "Path used to persist fold states."
  :type 'file
  :group '+fold)

(config-defvar +fold-prog-startup nil
  "Default fold action for code buffers without a persisted fold state.

When set to `fold-all', newly opened code buffers enter a compact outline by
default.  This startup fold is intentionally not persisted; explicit fold
commands still save their state."
  :type '(choice (const :tag "Fold all blocks" fold-all) (const :tag "Leave buffer open" nil))
  :group '+fold)

(config-defvar +fold-diagnostics-enabled nil
  "Show Flymake diagnostic summaries on folded regions."
  :type 'boolean
  :group '+fold)

(config-defvar +fold-diagnostics-idle-delay nil
  "Idle delay before refreshing folded diagnostic summaries."
  :type 'number
  :group '+fold)

(config-defvar +fold-deferred-modes nil
  "Programming modes where startup fold restoration waits for idle time."
  :type '(repeat symbol)
  :group '+fold)

(config-defvar +fold-restore-idle-delay nil
  "Idle seconds before restoring folds in `+fold-deferred-modes'."
  :type 'number
  :group '+fold)

(defvar +fold-state-table nil
  "Alist mapping file names to persisted fold start lines.")

(defvar +fold-state-loaded nil
  "Whether fold state has been loaded from disk.")

(defvar-local +fold--buffer-state-dirty nil
  "Non-nil when this buffer's fold state was changed through `+fold'.")

(defvar-local +fold-diagnostics--timer nil
  "Pending timer for folded diagnostic summary refresh.")

(defvar-local +fold-diagnostics--overlays nil
  "Overlays showing diagnostics summarized onto folded lines.")

(defvar-local +fold--org-render-timer nil
  "Pending lightweight Org render refresh after fold changes.")

(defvar-local +fold--restore-timer nil
  "Idle timer used to defer startup fold restoration.")

;; Compilation Mode
(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
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
          org-mode) . hl-todo-mode)
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

;; A jump can land inside a fold restored when the file was opened, where the
;; cursor sits hidden.  Open those folds the way isearch opens a match; both
;; hideshow and treesit-fold give their overlays `isearch-open-invisible'.
;; Consult reveals its own jumps.  Doom does this for Magit's file visits.
(defun +fold-reveal-point-h (&rest _)
  "Open the folds that hide point."
  (dolist (ov (overlays-at (point)))
    (when-let* (((overlay-get ov 'invisible))
                (open (overlay-get ov 'isearch-open-invisible)))
      (funcall open ov))))

(dolist (hook '(xref-after-jump-hook xref-after-return-hook
                imenu-after-jump-hook better-jumper-post-jump-hook
                magit-diff-visit-file-hook))
  (add-hook hook #'+fold-reveal-point-h -50))

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
(defvar +citre-lsp-backend nil
  "Citre adapter for lsp-mode's `xref-lsp' backend.")

(defun +citre-lsp-usable-p ()
  "Return non-nil when lsp-mode can answer Citre navigation requests."
  (bound-and-true-p lsp-managed-mode))

(defun +citre-lsp-after-jump ()
  "Attach lsp-mode after Citre visits a definition buffer.
The post-command pass preserves Citre peek-through's old Eglot-era behavior:
when startup is locally synchronous, the newly visited file is ready before a
second chained lookup is attempted.  Remote/runtime preparation may continue
asynchronously through the normal language-server lifecycle."
  (when (fboundp '+language-server/ensure)
    (+language-server/ensure))
  (run-hooks 'post-command-hook))

(defun +citre-register-lsp-backend ()
  "Replace Citre's bundled Eglot adapter with the active lsp-mode xref route."
  (setq +citre-lsp-backend
        (citre-xref-backend-to-citre-backend
         'xref-lsp #'+citre-lsp-usable-p
         :after-jump-fn #'+citre-lsp-after-jump))
  (citre-register-backend 'lsp-mode +citre-lsp-backend))

(defvar +citre--executable-cache (make-hash-table :test #'equal)
  "Citre helper executables keyed by target, command and search path.")

(defun +fold/citre-forget-executables (&rest _)
  "Drop the cached Citre helper lookups."
  (interactive)
  (clrhash +citre--executable-cache))

(defun +citre--remote-executable-fast (command paths)
  "Return (t . PATH) when one target command safely resolves COMMAND.
PATHS is Citre's actual remote `exec-path'.  Return nil when its semantics
cannot be reproduced, allowing Citre's original search to handle the case."
  (when (and (require 'remote-process nil t)
             (stringp command)
             (not (string-match-p "[/\n\0]" command))
             (equal exec-suffixes '(""))
             (consp paths)
             (cl-every
              (lambda (path)
                (and (stringp path)
                     (file-name-absolute-p path)
                     (not (string-match-p "[:\n\0]" path))))
              paths))
    (let* ((result
            (remote-exec
             "sh"
             :args
             (list
              "-c"
              (concat
               "found=$(command -v \"$1\") || exit 0; "
               "case \"$found\" in /*) ;; *) exit 3;; esac; "
               "[ -f \"$found\" ] && [ -x \"$found\" ] || exit 3; "
               "printf '%s\\n' \"$found\"")
              "citre-lookup" command)
             :context (remote-context default-directory)
             :adapter "exec"
             :environment
             (list (cons "PATH" (mapconcat #'identity paths ":")))
             :filesystem-effects 'none))
           (output (remote-exec-result-stdout result)))
      (when (and (equal (remote-exec-result-status result) 0)
                 (stringp output))
        (cond
         ((string-empty-p output) (cons t nil))
         ((and (string-suffix-p "\n" output)
               (file-name-absolute-p output)
               (not (string-match-p "\n" (substring output 0 -1))))
          (cons t (substring output 0 -1))))))))

(defun +citre-executable-find-a (fn &rest args)
  "Cache Citre's target helper lookup and use one process probe when safe.
The cache follows Citre's actual remote search path, so a PATH change cannot
reuse a result from an earlier environment.  Unsupported path shapes and
future Citre argument changes retain the original lookup."
  (let ((command (car args))
        (remote (cadr args)))
    (if (or (cddr args)
            (not remote)
            (not (file-remote-p default-directory)))
        (apply fn args)
      (let* ((target (file-remote-p default-directory))
             (handler
              (find-file-name-handler default-directory 'exec-path))
             (paths
              (condition-case nil
                  (and handler (funcall handler 'exec-path))
                (error :lookup-error))))
        (if (or (eq paths :lookup-error)
                (not (proper-list-p paths)))
            (apply fn args)
          (let* ((key (list target command
                            (mapcar
                             (lambda (path)
                               (if (stringp path)
                                   (copy-sequence path)
                                 path))
                             paths)
                            (copy-sequence exec-suffixes)))
                 (cached (gethash key +citre--executable-cache 'missing)))
            (if (not (eq cached 'missing))
                cached
              (let* ((fast
                      (condition-case nil
                          (+citre--remote-executable-fast command paths)
                        (error nil)))
                     (found (if fast (cdr fast) (apply fn args))))
                (when (>= (hash-table-count +citre--executable-cache) 256)
                  (clrhash +citre--executable-cache))
                (puthash key found +citre--executable-cache)
                found))))))))

(with-eval-after-load 'citre-common-util
  (unless (advice-member-p #'+citre-executable-find-a 'citre-executable-find)
    (advice-add 'citre-executable-find :around #'+citre-executable-find-a)))

(with-eval-after-load 'remote-connection
  (add-hook 'remote-connection-closed-hook #'+fold/citre-forget-executables))

(with-eval-after-load 'remote-config
  (add-hook 'remote-config-after-load-hook #'+fold/citre-forget-executables))

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
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-find-definition-backends '(lsp-mode tags global))
  (citre-find-reference-backends '(lsp-mode global))
  :config
  (+citre-register-lsp-backend))

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
(defun +fold--org-buffer-p ()
  "Return non-nil when the current buffer should use Org folding."
  (derived-mode-p 'org-mode))

(defun +fold--treesit-buffer-p ()
  "Return non-nil when the current buffer should prefer `treesit-fold'."
  (and (fboundp 'treesit-parser-list)
       (fboundp 'treesit-fold-mode)
       (string-match-p "-ts-mode\\'" (symbol-name major-mode))
       (ignore-errors (treesit-parser-list))))

(defun +fold--skip-auto-restore-p ()
  "Return non-nil when opening this buffer should not restore folds."
  (derived-mode-p 'org-mode))

(defun +fold--backend ()
  "Return the preferred fold backend for the current buffer."
  (cond
   ((+fold--org-buffer-p) 'org)
   ((+fold--treesit-buffer-p) 'treesit)
   ((derived-mode-p 'prog-mode) 'hs)
   (t nil)))

(defun +fold--ensure-backend ()
  "Enable the best available folding backend for the current buffer."
  (pcase (+fold--backend)
    ('treesit
     (when (bound-and-true-p hs-minor-mode)
       (hs-minor-mode -1))
     (when (not (bound-and-true-p treesit-fold-mode))
       (treesit-fold-mode 1))
     (when (fboundp 'treesit-fold-indicators-mode)
       (treesit-fold-indicators-mode 1)))
    ('hs
     (when (bound-and-true-p treesit-fold-indicators-mode)
       (treesit-fold-indicators-mode -1))
     (when (bound-and-true-p treesit-fold-mode)
       (treesit-fold-mode -1))
     (when (fboundp 'hs-minor-mode)
       (hs-minor-mode 1)))))

(defun +fold--use-treesit-p ()
  "Return non-nil when folding should use tree-sitter."
  (+fold--ensure-backend)
  (bound-and-true-p treesit-fold-mode))

(defun +fold--org-back-to-heading ()
  "Move point to the current Org heading or raise a user-facing error."
  (unless (+fold--org-buffer-p)
    (user-error "Not in an Org buffer"))
  (unless (or (org-at-heading-p)
              (ignore-errors
                (org-back-to-heading t)
                t))
    (user-error "Point is not inside an Org heading")))

(defun +fold--org-heading-at-or-above-p ()
  "Move to the current Org heading and return non-nil when one exists."
  (and (+fold--org-buffer-p)
       (or (org-at-heading-p)
           (ignore-errors
             (org-back-to-heading t)
             t))))

(defun +fold--range-visible-p (beg end)
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

(defun +fold--range-nonblank-visible-p (beg end)
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

(defun +fold--org-direct-body-range ()
  "Return the direct body range for the current Org heading, or nil.
The direct body is the text after the heading line and before the first child
heading, or before the subtree end when there is no child."
  (save-excursion
    (+fold--org-back-to-heading)
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

(defun +fold--org-direct-body-visible-p ()
  "Return non-nil when the current Org heading's direct body is visible."
  (when-let* ((range (+fold--org-direct-body-range)))
    (+fold--range-nonblank-visible-p (car range) (cdr range))))

(defun +fold--org-subtree-content-range ()
  "Return the content range after the current Org heading line, or nil."
  (save-excursion
    (+fold--org-back-to-heading)
    (let ((content-beg (save-excursion
                         (forward-line 1)
                         (point)))
          (subtree-end (save-excursion
                         (org-end-of-subtree t t))))
      (when (< content-beg subtree-end)
        (cons content-beg subtree-end)))))

(defun +fold--org-subtree-content-visible-p ()
  "Return non-nil when any content in the current Org subtree is visible."
  (when-let* ((range (+fold--org-subtree-content-range)))
    (+fold--range-nonblank-visible-p (car range) (cdr range))))

(defun +fold--org-show-direct-body ()
  "Explicitly show the current Org heading's direct body."
  (when-let* ((range (+fold--org-direct-body-range)))
    (org-fold-region (car range) (cdr range) nil)))

(defun +fold--org-subtree-collapsed-p ()
  "Return non-nil when the current Org heading subtree is fully collapsed."
  (save-excursion
    (+fold--org-back-to-heading)
    (cond
     ((+fold--org-direct-body-visible-p)
      nil)
     ;; `org-modern' can hide Org keyword text such as #+subtitle: in the
     ;; direct body.  If child headings are still visible, H-TAB should close
     ;; the subtree instead of treating it as already collapsed.
     ((+fold--org-subtree-content-visible-p)
      nil)
     ((+fold--org-subtree-content-range)
      t)
     (t
      nil))))

(defun +fold--org-heading-body-visible-p ()
  "Return non-nil when the current Org heading has visible body text."
  (+fold--org-direct-body-visible-p))

(defun +fold--org-any-heading-body-visible-p ()
  "Return non-nil when any Org heading body in this buffer is visible."
  (catch 'visible
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (goto-char (line-beginning-position))
          (when (+fold--org-heading-body-visible-p)
            (throw 'visible t))
          (forward-line 1))))
    nil))

(defun +fold--org-toggle-buffer ()
  "Toggle whole-buffer Org folding when point is outside any heading."
  (if (+fold--org-any-heading-body-visible-p)
      (org-overview)
    (org-fold-show-all))
  (+fold--refresh-visible-org-rendering))

(defun +fold--org-cancel-render-refresh ()
  "Cancel pending Org rendering refresh for fold changes."
  (when (timerp +fold--org-render-timer)
    (cancel-timer +fold--org-render-timer))
  (setq-local +fold--org-render-timer nil))

(defun +fold--org-render-refresh-now (buffer _window)
  "Refresh visible Org renderers for BUFFER after fold changes.
The second argument is kept for compatibility with queued timers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local +fold--org-render-timer nil)
      (when (+fold--org-buffer-p)
        (when (fboundp '+org-schedule-visible-inline-image-refresh)
          (+org-schedule-visible-inline-image-refresh t t))))))

(defun +fold--refresh-visible-org-rendering (&rest _)
  "Refresh visible Org rendering that intentionally ignores folded text."
  (when (+fold--org-buffer-p)
    (when (fboundp '+org-clear-visible-ranges-cache)
      (+org-clear-visible-ranges-cache (current-buffer)))
    (font-lock-flush (line-beginning-position) (line-end-position))
    (+fold-diagnostics-clear)
    (+fold--org-cancel-render-refresh)
    (setq-local +fold--org-render-timer
                (run-with-idle-timer
                 0.12 nil
                 #'+fold--org-render-refresh-now
                 (current-buffer)
                 (and (window-live-p (selected-window))
                      (selected-window))))))

(defun +fold--org-toggle ()
  "Toggle the current Org heading subtree."
  (save-excursion
    (if (+fold--org-heading-at-or-above-p)
        (if (+fold--org-subtree-collapsed-p)
            (+fold--org-open)
          (+fold--org-close))
      (+fold--org-toggle-buffer))))

(defun +fold--org-open ()
  "Open the current Org heading one level."
  (save-excursion
    (+fold--org-back-to-heading)
    (org-fold-show-entry)
    (+fold--org-show-direct-body)
    (org-fold-show-children))
  (+fold--refresh-visible-org-rendering))

(defun +fold--org-close ()
  "Close the current Org heading subtree."
  (save-excursion
    (+fold--org-back-to-heading)
    (org-fold-hide-subtree))
  (+fold--refresh-visible-org-rendering))

(defun +fold--org-open-zone ()
  "Open the current Org heading subtree recursively."
  (save-excursion
    (+fold--org-back-to-heading)
    (org-fold-show-subtree))
  (+fold--refresh-visible-org-rendering))

(defun +fold--state-key (&optional file)
  "Return canonical persisted key for FILE or current buffer."
  (when-let* ((path (or file buffer-file-name)))
    (expand-file-name path)))

(defun +fold--load-state-table ()
  "Load persisted fold state from disk once."
  (unless +fold-state-loaded
    (setq +fold-state-loaded t)
    (when (file-exists-p +fold-state-file)
      (with-temp-buffer
        (insert-file-contents +fold-state-file)
        (goto-char (point-min))
        (setq +fold-state-table (read (current-buffer)))))))

(defun +fold--state-entry ()
  "Return persisted fold state entry for the current buffer, if any."
  (when-let* ((key (+fold--state-key)))
    (assoc key +fold-state-table)))

(defun +fold--save-state-table ()
  "Persist fold state to disk."
  (unless noninteractive
    (+fold--load-state-table)
    (condition-case err
        (progn
          (make-directory (file-name-directory +fold-state-file) t)
          (with-temp-file +fold-state-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 +fold-state-table (current-buffer))
              (insert "\n"))))
      (file-locked
       (message "Skip saving fold state: %s" (error-message-string err))))))

(defun +fold--current-hidden-start-lines ()
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

(defun +fold--apply-close-at-line (line)
  "Close the fold starting at LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (back-to-indentation)
    (condition-case nil
        (if (+fold--use-treesit-p)
            (treesit-fold-close)
          (hs-hide-block))
      (error nil))))

(defun +fold--hs-zone-range ()
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

(defun +fold--hs-open-zone ()
  "Open all hideshow folds in the current fold zone."
  (if-let* ((range (+fold--hs-zone-range)))
      (let (hs-allow-nesting)
        (hs-discard-overlays (car range) (cdr range)))
    (call-interactively #'hs-show-block)))

(defun +fold-diagnostics-cancel ()
  "Cancel pending folded diagnostic summary refresh."
  (when (timerp +fold-diagnostics--timer)
    (cancel-timer +fold-diagnostics--timer))
  (setq-local +fold-diagnostics--timer nil))

(defun +fold-diagnostics-clear ()
  "Delete folded diagnostic summary overlays in the current buffer."
  (mapc #'delete-overlay +fold-diagnostics--overlays)
  (setq-local +fold-diagnostics--overlays nil))

(defun +fold-diagnostics-cleanup ()
  "Release folded diagnostic summary state for this buffer."
  (+fold-diagnostics-cancel)
  (+fold-diagnostics-clear)
  (remove-hook 'change-major-mode-hook #'+fold-diagnostics-cleanup t)
  (remove-hook 'kill-buffer-hook #'+fold-diagnostics-cleanup t))

(defun +fold-diagnostics--pos (pos)
  "Return POS as an integer buffer position, or nil."
  (cond
   ((integerp pos) pos)
   ((markerp pos) (marker-position pos))
   (t nil)))

(defun +fold-diagnostics--normalize-range (range)
  "Return RANGE clamped to the current buffer, or nil when empty."
  (when (consp range)
    (let ((beg (+fold-diagnostics--pos (car range)))
          (end (+fold-diagnostics--pos (cdr range))))
      (when (and beg end)
        (setq beg (min (point-max) (max (point-min) beg))
              end (min (point-max) (max (point-min) end)))
        (when (< beg end)
          (cons beg end))))))

(defun +fold-diagnostics--diag-range (diagnostic)
  "Return DIAGNOSTIC as a normalized buffer range, or nil."
  (let* ((beg (+fold-diagnostics--pos
               (ignore-errors (flymake-diagnostic-beg diagnostic))))
         (end (+fold-diagnostics--pos
               (ignore-errors (flymake-diagnostic-end diagnostic)))))
    (when beg
      (setq end (or end beg))
      (+fold-diagnostics--normalize-range
       (cons beg (max end (1+ beg)))))))

(defun +fold-diagnostics--diag-kind (diagnostic)
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

(defun +fold-diagnostics--diag-text (diagnostic)
  "Return a compact one-line message for DIAGNOSTIC."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]\n\r\t]+" " "
    (or (ignore-errors (flymake-diagnostic-text diagnostic)) ""))))

(defun +fold-diagnostics--collect ()
  "Return normalized Flymake diagnostics for the current buffer."
  (delq
   nil
   (mapcar
    (lambda (diagnostic)
      (when-let* ((range (+fold-diagnostics--diag-range diagnostic)))
        (list :range range
              :kind (+fold-diagnostics--diag-kind diagnostic)
              :text (+fold-diagnostics--diag-text diagnostic))))
    (ignore-errors (flymake-diagnostics)))))

(defun +fold-diagnostics--overlay-fold-ranges ()
  "Return folded hideshow/tree-sitter ranges as DISPLAY-POS . RANGE pairs."
  (let (ranges)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (or (eq (overlay-get overlay 'invisible) 'treesit-fold)
                (eq (overlay-get overlay 'hs) 'code))
        (when-let* ((range (+fold-diagnostics--normalize-range
                            (cons (overlay-start overlay)
                                  (overlay-end overlay)))))
          (push (cons (max (point-min) (1- (overlay-start overlay)))
                      range)
                ranges))))
    (sort ranges (lambda (left right) (< (car left) (car right))))))

(defun +fold-diagnostics--backend-p ()
  "Return non-nil when folded diagnostics should run here."
  (memq (+fold--backend) '(treesit hs)))

(defun +fold-diagnostics--range-summary (diagnostics range)
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

(defun +fold-diagnostics--format-summary (summary)
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

(defun +fold-diagnostics--help (summary)
  "Return tooltip text for folded diagnostic SUMMARY."
  (concat "Folded diagnostics:\n"
          (string-join (plist-get summary :samples) "\n")))

(defun +fold-diagnostics--put-summary (display-pos summary)
  "Place folded diagnostic SUMMARY on DISPLAY-POS's line."
  (save-excursion
    (goto-char display-pos)
    (let ((ov (make-overlay (line-end-position)
                            (line-end-position)
                            nil nil t)))
      (overlay-put ov '+fold-diagnostics t)
      (overlay-put ov 'after-string
                   (propertize (+fold-diagnostics--format-summary summary)
                               'help-echo
                               (+fold-diagnostics--help summary)))
      (overlay-put ov 'help-echo (+fold-diagnostics--help summary))
      (overlay-put ov 'priority 2000)
      (push ov +fold-diagnostics--overlays))))

(defun +fold-refresh-diagnostics (&optional buffer)
  "Refresh folded diagnostic summaries for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (+fold-diagnostics-cancel)
        (+fold-diagnostics-clear)
        (when (and +fold-diagnostics-enabled
                   (bound-and-true-p flymake-mode)
                   (fboundp 'flymake-diagnostics)
                   (+fold-diagnostics--backend-p))
          (let ((diagnostics (+fold-diagnostics--collect)))
            (when diagnostics
              (dolist (entry (+fold-diagnostics--overlay-fold-ranges))
                (when-let* ((summary
                             (+fold-diagnostics--range-summary
                              diagnostics (cdr entry))))
                  (+fold-diagnostics--put-summary (car entry) summary))))))))))

(defun +fold-schedule-diagnostics (&rest _)
  "Schedule folded diagnostic summary refresh for the current buffer."
  (when (and +fold-diagnostics-enabled
             (+fold-diagnostics--backend-p))
    (+fold-diagnostics-cancel)
    (setq-local +fold-diagnostics--timer
                (run-with-idle-timer
                 +fold-diagnostics-idle-delay nil
                 #'+fold-refresh-diagnostics
                 (current-buffer)))))

(defun +fold-diagnostics--flymake-publish-a (&rest _)
  "Refresh folded diagnostic summaries after Flymake publishes diagnostics."
  (+fold-schedule-diagnostics))

(defun +fold-diagnostics-setup ()
  "Enable folded diagnostic summaries in fold-capable buffers."
  (when (and +fold-diagnostics-enabled
             (+fold-diagnostics--backend-p))
    (add-hook 'change-major-mode-hook #'+fold-diagnostics-cleanup nil t)
    (add-hook 'kill-buffer-hook #'+fold-diagnostics-cleanup nil t)
    (+fold-schedule-diagnostics)))

(defun +fold-save-buffer-state ()
  "Persist fold state for the current buffer."
  (when buffer-file-name
    (+fold--load-state-table)
    (let ((entry (+fold--state-entry)))
      (when (or +fold--buffer-state-dirty entry)
        (let ((key (+fold--state-key))
              (lines (+fold--current-hidden-start-lines)))
          (setq +fold-state-table
                (assoc-delete-all key +fold-state-table))
          (when lines
            (push (cons key lines) +fold-state-table))
          (+fold--save-state-table))))))

(defun +fold--mark-buffer-state-dirty-and-save ()
  "Mark the current buffer's fold state as user-managed and persist it."
  (setq +fold--buffer-state-dirty t)
  (+fold-save-buffer-state)
  (+fold-schedule-diagnostics))

(defun +fold--apply-default-prog-startup ()
  "Apply the default startup fold policy for code buffers."
  (when (and (eq +fold-prog-startup 'fold-all)
             (derived-mode-p 'prog-mode))
    (+fold--ensure-backend)
    (pcase (+fold--backend)
      ('treesit
       (when (bound-and-true-p treesit-fold-mode)
         (ignore-errors (treesit-fold-close-all))))
      ('hs
       (ignore-errors (hs-hide-all))))
    (+fold-schedule-diagnostics)))

(defun +fold-restore-buffer-state ()
  "Restore fold state for the current buffer."
  (when buffer-file-name
    (+fold--load-state-table)
    (if-let* ((entry (+fold--state-entry)))
        (progn
          (+fold--ensure-backend)
          (dolist (line (cdr entry))
            (+fold--apply-close-at-line line))
          (+fold-schedule-diagnostics))
      (+fold--apply-default-prog-startup))))

(defun +fold-restore-existing-buffer-states ()
  "Apply startup fold restoration for all live file buffers after startup.

This covers buffers opened before `find-file-hook' started restoring folds."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (derived-mode-p 'prog-mode)
                 (not (+fold--skip-auto-restore-p)))
        (ignore-errors
          (+fold-restore-buffer-state))))))

(defun +fold-restore-buffer-state-on-open ()
  "Restore fold state as soon as the current file finishes opening."
  (when (and buffer-file-name
             (derived-mode-p 'prog-mode)
             (not (+fold--skip-auto-restore-p)))
    (if (and +fold-deferred-modes
             (apply #'derived-mode-p +fold-deferred-modes))
        (let ((buffer (current-buffer)))
          (when (timerp +fold--restore-timer)
            (cancel-timer +fold--restore-timer))
          (setq +fold--restore-timer
                (run-with-idle-timer
                 +fold-restore-idle-delay nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq +fold--restore-timer nil)
                       (ignore-errors
                         (+fold-restore-buffer-state))))))))
      (ignore-errors
        (+fold-restore-buffer-state)))))

(defvar +fold-hs-display-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'+fold/hs-mouse-toggle)
    map)
  "Keymap used by clickable hideshow placeholders.")

(defun +hs-hidden-line-count (overlay)
  "Return the number of complete body lines hidden by OVERLAY."
  (max 1 (1- (count-lines (overlay-start overlay) (overlay-end overlay)))))

(defun +hs-set-up-overlay (overlay)
  "Keep native hideshow rendering and add persistent clicks to OVERLAY.
On Emacs 30, provide the line-count display that became built in with
hideshow 6.0.  This function deliberately never changes overlay bounds."
  (when (eq 'code (overlay-get overlay 'hs))
    (let* ((lines (+hs-hidden-line-count overlay))
           (help (format "Hidden code block: %d line%s; mouse-1 toggles"
                         lines (if (= lines 1) "" "s")))
           (native-display (and (>= emacs-major-version 31)
                                (overlay-get overlay 'display)))
           (display (if (stringp native-display)
                        (copy-sequence native-display)
                      (format " ... [%d line%s] "
                              lines (if (= lines 1) "" "s")))))
      (add-text-properties
       0 (length display)
       (list 'mouse-face 'highlight
             'keymap +fold-hs-display-map
             'help-echo help)
       display)
      (overlay-put overlay 'display display)
      (overlay-put overlay 'help-echo help))))

(defun +fold/hs-mouse-toggle (event)
  "Toggle a hideshow fold from mouse EVENT and persist the new state."
  (interactive "e")
  (let ((area (and (mouse-event-p event)
                   (posn-area (event-start event)))))
    (if (and (memq area '(left-fringe left-margin right-fringe right-margin))
             (fboundp 'hs-indicator-mouse-toggle-hiding))
        (hs-indicator-mouse-toggle-hiding event)
      (hs-toggle-hiding event)))
  (+fold--mark-buffer-state-dirty-and-save))

(defun +fold/toggle ()
  "Toggle the fold at point."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (+fold--org-toggle))
    ('treesit
     (call-interactively #'treesit-fold-toggle)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-toggle-hiding)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun +fold/open ()
  "Open the fold at point."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (+fold--org-open))
    ('treesit
     (call-interactively #'treesit-fold-open)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-show-block)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun +fold/close ()
  "Close the fold at point."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (+fold--org-close))
    ('treesit
     (call-interactively #'treesit-fold-close)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-hide-block)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun +fold/open-all ()
  "Open all folds in the current buffer."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (org-fold-show-all)
     (+fold--refresh-visible-org-rendering))
    ('treesit
     (call-interactively #'treesit-fold-open-all)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-show-all)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun +fold/open-zone ()
  "Open all folds inside the current fold zone."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (+fold--org-open-zone))
    ('treesit
     (call-interactively #'treesit-fold-open-recursively)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (+fold--hs-open-zone)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

(defun +fold/close-all ()
  "Close all folds in the current buffer."
  (interactive)
  (pcase (+fold--backend)
    ('org
     (org-overview)
     (+fold--refresh-visible-org-rendering))
    ('treesit
     (call-interactively #'treesit-fold-close-all)
     (+fold--mark-buffer-state-dirty-and-save))
    ('hs
     (call-interactively #'hs-hide-all)
     (+fold--mark-buffer-state-dirty-and-save))
    (_
     (user-error "No fold backend for %s" major-mode))))

;; zj and zk, from Doom.  Every backend (hideshow, treesit-fold, Org) hides a
;; closed fold as an invisible stretch that begins on the fold's first line
;; and spans whole lines; hidden link or emphasis markup never holds a newline.
(defun +fold--closed-fold-lines ()
  "Return the start positions of the visible lines that begin a closed fold."
  (let ((pos (point-min))
        lines)
    (while (< pos (point-max))
      (let ((next (next-single-char-property-change pos 'invisible)))
        (when (and (invisible-p pos)
                   (save-excursion
                     (goto-char pos)
                     (search-forward "\n" next t)))
          (let ((line (save-excursion (goto-char pos) (line-beginning-position))))
            (unless (or (invisible-p line) (eq line (car lines)))
              (push line lines))))
        (setq pos next)))
    (nreverse lines)))

(defun +fold/next (count)
  "Move to the first line of the COUNTth next closed fold, as Vim's zj."
  (interactive "p")
  (let* ((here (line-beginning-position))
         (lines (if (> count 0)
                    (seq-filter (lambda (line) (> line here))
                                (+fold--closed-fold-lines))
                  (nreverse (seq-filter (lambda (line) (< line here))
                                        (+fold--closed-fold-lines)))))
         (target (nth (1- (abs count)) lines)))
    (unless target
      (user-error "No more closed folds %s point" (if (> count 0) "after" "before")))
    (goto-char target)
    (back-to-indentation)))

(defun +fold/previous (count)
  "Move to the first line of the COUNTth previous closed fold, as Vim's zk."
  (interactive "p")
  (+fold/next (- count)))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . +fold--ensure-backend)
  :custom
  (hs-allow-nesting t)
  (hs-show-indicators t)
  (hs-display-lines-hidden t)
  (hs-indicator-type 'fringe)
  (hs-hide-comments-when-hiding-all nil)
  (hs-isearch-open t)
  (hs-set-up-overlay #'+hs-set-up-overlay))

(with-eval-after-load 'hideshow
  (when (and (fboundp 'hs-indicator-mouse-toggle-hiding)
             (boundp 'hs-indicators-map))
    (keymap-set hs-indicators-map "<mouse-1>" #'+fold/hs-mouse-toggle)
    (keymap-set hs-indicators-map "<left-margin> <mouse-1>"
                #'+fold/hs-mouse-toggle))
  (when (and (fboundp 'hs-indicator-mouse-toggle-hiding)
             (boundp 'hs-minor-mode-map))
    (keymap-set hs-minor-mode-map "<left-fringe> <mouse-1>"
                #'+fold/hs-mouse-toggle)))

(with-eval-after-load 'treesit-fold
  (defun +fold--treesit-command-save-a (&rest _)
    "Persist tree-sitter fold state after direct mouse/backend commands."
    (when (and buffer-file-name
               (derived-mode-p 'prog-mode)
               (bound-and-true-p treesit-fold-mode))
      (+fold--mark-buffer-state-dirty-and-save)))

  (dolist (command '(treesit-fold-open
                     treesit-fold-close
                     treesit-fold-toggle))
    (advice-add command :after #'+fold--treesit-command-save-a)))

(with-eval-after-load 'org
  (add-hook 'org-cycle-hook #'+fold--refresh-visible-org-rendering)
  (keymap-set org-mode-map "H-<tab>" #'+fold/toggle)
  (keymap-set org-mode-map "H-TAB" #'+fold/toggle)
  (keymap-set org-mode-map "H-S-<tab>" #'+fold/open-zone)
  (keymap-set org-mode-map "H-<backtab>" #'+fold/open-zone)
  (keymap-set org-mode-map "H-S-TAB" #'+fold/open-zone))

(with-eval-after-load 'flymake
  (unless (advice-member-p #'+fold-diagnostics--flymake-publish-a
                           'flymake--publish-diagnostics)
    (advice-add 'flymake--publish-diagnostics
                :after #'+fold-diagnostics--flymake-publish-a)))

(add-hook 'prog-mode-hook #'+fold-diagnostics-setup)

(define-leader-key!
  "z"   '(:ignore t :which-key "fold")
  "z a" '(:def +fold/toggle :which-key "toggle fold")
  "z o" '(:def +fold/open :which-key "open fold")
  "z O" '(:def +fold/open-zone :which-key "open fold zone")
  "z c" '(:def +fold/close :which-key "close fold")
  "z R" '(:def +fold/open-all :which-key "open all folds")
  "z M" '(:def +fold/close-all :which-key "close all folds"))

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "za") #'+fold/toggle)
  (evil-define-key* 'normal 'global (kbd "zo") #'+fold/open)
  (evil-define-key* 'normal 'global (kbd "zO") #'+fold/open-zone)
  (evil-define-key* 'normal 'global (kbd "zc") #'+fold/close)
  (evil-define-key* 'normal 'global (kbd "zR") #'+fold/open-all)
  (evil-define-key* 'normal 'global (kbd "zM") #'+fold/close-all)
  (evil-define-key* 'normal 'global (kbd "zj") #'+fold/next)
  (evil-define-key* 'normal 'global (kbd "zk") #'+fold/previous))

(general-define-key
 "H-<tab>" #'+fold/toggle
 "H-TAB" #'+fold/toggle
 "H-S-<tab>" #'+fold/open-zone
 "H-<backtab>" #'+fold/open-zone
 "H-S-TAB" #'+fold/open-zone)

(add-hook 'find-file-hook #'+fold-restore-buffer-state-on-open)
(add-hook 'emacs-startup-hook #'+fold-restore-existing-buffer-states)
(add-hook 'kill-buffer-hook #'+fold-save-buffer-state)
(add-hook 'kill-emacs-hook #'+fold--save-state-table)

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

;; From Doom: `yaml-ts-mode' (which visits YAML files here) has no indentation
;; rules of its own and falls back to `indent-relative', so RET after "key:"
;; lands in column 0.  Borrow `yaml-mode's indenter.
(defun +yaml-ts-use-yaml-mode-indentation-h ()
  "Indent `yaml-ts-mode' buffers with `yaml-indent-line'."
  (when (remote-require-on-client 'yaml-mode nil t)
    (setq-local indent-line-function #'yaml-indent-line)))

(add-hook 'yaml-ts-mode-hook #'+yaml-ts-use-yaml-mode-indentation-h)

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


(provide 'editor/fold/config)

;;; editor/fold/config.el ends here
