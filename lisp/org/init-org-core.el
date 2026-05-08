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

(defvar company-idle-delay)
(defvar copilot-idle-delay)
(defvar flymake-no-changes-timeout)
(defvar my/flymake-diagnostic-at-point-delay)
(defvar my/flymake-diagnostic-at-point-mode)
(declare-function my/flymake-diagnostic-at-point-mode "init-lsp" (&optional arg))

;;; ----------------------------------------------------------------------------
;;; 1. Global Variables & Paths (全局路径配置)
;;; ----------------------------------------------------------------------------

(defvar my-org-root (file-truename "~/HC/Org/")
  "Root directory for all Org files.")

(defvar my-org-roam-dir (expand-file-name "roam/" my-org-root))
(defvar my-org-daily-dir (expand-file-name "daily/" my-org-roam-dir))
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

(defcustom my/org-feature-detect-margin-lines 160
  "Lines around visible Org windows searched for optional UI features.
Feature discovery for long Org buffers is intentionally anchored to what is
visible, plus nearby context, instead of scanning the whole buffer on open."
  :type 'integer
  :group 'my/org-ui)

(defcustom my/org-toc-auto-update-max-buffer-size (* 1024 1024)
  "Maximum Org buffer size eligible for automatic TOC refresh."
  :type 'integer)

(defcustom my/org-toc-auto-update-enabled t
  "Whether Org buffers with an overview TOC refresh automatically."
  :type 'boolean)

(defcustom my/org-toc-auto-search-limit (* 64 1024)
  "Maximum characters searched for a managed TOC during automatic setup.
Managed overview TOCs are expected near the top of the file; limiting this
scan avoids walking large ordinary Org buffers that do not use the feature."
  :type 'integer)

(defcustom my/org-low-power-profile t
  "Apply conservative power-oriented local defaults in Org buffers.
The profile keeps the visual writing setup intact, but slows background
assistants that otherwise wake up aggressively while typing or navigating."
  :type 'boolean
  :group 'my/org-ui)

(defcustom my/org-display-line-numbers nil
  "Buffer-local line-number style for Org buffers.
Nil disables line numbers in Org buffers, which is cheaper with
`visual-line-mode', variable pitch text and `org-indent-mode'.  Set this to t,
`relative' or `visual' to keep line numbers."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Absolute" t)
                 (const :tag "Relative" relative)
                 (const :tag "Visual" visual))
  :group 'my/org-ui)

(defcustom my/org-inline-image-on-demand t
  "Display Org inline images for visible ranges on demand.
This keeps image display available while avoiding startup-wide image scans in
very large prose buffers."
  :type 'boolean
  :group 'my/org-ui)

(defcustom my/org-inline-image-visible-margin-lines 48
  "Extra lines around visible Org windows considered for inline images."
  :type 'integer
  :group 'my/org-ui)

(defcustom my/org-inline-image-idle-delay 0.20
  "Idle delay before refreshing visible Org inline images."
  :type 'number
  :group 'my/org-ui)

(defcustom my/org-company-idle-delay 0.45
  "Org-local `company-idle-delay'.
This is intentionally slower than programming buffers; Org completion remains
automatic, but stops racing normal prose typing."
  :type 'number
  :group 'my/org-ui)

(defcustom my/org-copilot-idle-delay 0.90
  "Org-local `copilot-idle-delay'."
  :type '(choice (number :tag "Seconds of delay")
                 (const :tag "Idle completion disabled" nil))
  :group 'my/org-ui)

(defcustom my/org-flymake-no-changes-timeout 1.50
  "Org-local `flymake-no-changes-timeout' for Vale/Flymake checks."
  :type 'number
  :group 'my/org-ui)

(defcustom my/org-flymake-diagnostic-at-point nil
  "When non-nil, echo Flymake diagnostics at point in Org buffers.
Diagnostics remain available through Flymake fringe indicators and diagnostics
buffers even when this echo helper is disabled."
  :type 'boolean
  :group 'my/org-ui)

(defcustom my/org-flymake-diagnostic-at-point-delay 0.80
  "Org-local delay before echoing a Flymake diagnostic at point."
  :type 'number
  :group 'my/org-ui)

(defvar-local my/org-toc--block-begin-marker nil
  "Marker for the beginning of the managed overview TOC block.")

(defvar-local my/org-toc--block-end-marker nil
  "Marker for the end of the managed overview TOC block.")

(defvar-local my/org-toc--initial-refresh-done nil
  "Whether this buffer already ran its one-time TOC refresh after opening.")

(defconst my/org-buffer-feature--scan-regexp
  "\\(^[ \t]*[|│┃]\\)\\|\\(^[ \t]*#\\+begin_\\)\\|\\(\\\\[([]\\|^[ \t]*#\\+begin_display_latex\\b\\)"
  "Regexp used for one-pass detection of Org features used by UI helpers.")

(defvar-local my/org-buffer-feature--cache nil
  "Cached Org feature presence for the current `buffer-chars-modified-tick'.")

(defvar-local my/org-buffer-feature--latched nil
  "Sticky plist of feature flags ever observed in this buffer.
Once a feature is detected we stop scanning for it on later modification
ticks: existing on-demand UI helpers (valign, pretty blocks, LaTeX preview)
only enable themselves when a feature appears; they do not tear themselves
down when the last instance is removed, so a sticky flag is consistent with
their actual behavior and cuts per-keystroke scan work to zero once a buffer
has all three features in scope.")

(defvar-local my/org-inline-image--refresh-timer nil
  "Pending visible inline image refresh timer for the current Org buffer.")

(defvar-local my/org-inline-image--last-signature nil
  "Last visible inline image refresh signature in the current Org buffer.")

(defvar-local my/org-inline-image--enabled nil
  "Non-nil when visible inline image refresh hooks are installed.")

(defun my/org--range-with-line-margin (beg end margin-lines)
  "Return BEG END expanded by MARGIN-LINES lines in the current buffer."
  (let* ((margin (max 0 (or margin-lines 0)))
         (range-beg (max (point-min) (min beg end)))
         (range-end (min (point-max) (max beg end)))
         expanded-beg expanded-end)
    (save-excursion
      (goto-char range-beg)
      (forward-line (- margin))
      (setq expanded-beg (line-beginning-position))
      (goto-char range-end)
      (forward-line margin)
      (setq expanded-end (line-end-position)))
    (cons (max (point-min) expanded-beg)
          (min (point-max) expanded-end))))

(defun my/org--merge-ranges (ranges)
  "Return sorted merged RANGES."
  (let ((ranges (sort (delq nil ranges)
                      (lambda (left right) (< (car left) (car right)))))
        merged)
    (dolist (range ranges)
      (pcase-let ((`(,beg . ,end) range))
        (when (< beg end)
          (if (and merged (<= beg (cdar merged)))
              (setcdr (car merged) (max end (cdar merged)))
            (push (cons beg end) merged)))))
    (nreverse merged)))

(defun my/org-buffer-visible-ranges (&optional buffer margin-lines fallback-to-point)
  "Return visible ranges for BUFFER expanded by MARGIN-LINES.
When FALLBACK-TO-POINT is non-nil and BUFFER is not visible, return a bounded
range around point instead of falling back to the whole buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let (ranges)
          (dolist (window (get-buffer-window-list buffer nil t))
            (when (window-live-p window)
              (let ((beg (window-start window))
                    (end (or (window-end window t) (point-max))))
                (push (my/org--range-with-line-margin
                       beg end margin-lines)
                      ranges))))
          (unless (or ranges (not fallback-to-point))
            (push (my/org--range-with-line-margin
                   (point) (point) margin-lines)
                  ranges))
          (my/org--merge-ranges ranges))))))

(defun my/org-buffer-feature--visible-signature ()
  "Return the current visible feature detection signature."
  (my/org-buffer-visible-ranges
   (current-buffer) my/org-feature-detect-margin-lines t))

(defun my/org-buffer-feature--scan ()
  "Return cached visible feature presence for the current Org buffer."
  (let ((tick (buffer-chars-modified-tick))
        (ranges (my/org-buffer-feature--visible-signature)))
    (unless (and (consp my/org-buffer-feature--cache)
                 (eql (plist-get my/org-buffer-feature--cache :tick) tick)
                 (equal (plist-get my/org-buffer-feature--cache :ranges)
                        ranges))
      (let ((table (plist-get my/org-buffer-feature--latched :table))
            (special-block (plist-get my/org-buffer-feature--latched :special-block))
            (latex-candidate (plist-get my/org-buffer-feature--latched
                                        :latex-candidate)))
        (unless (and table special-block latex-candidate)
          (save-excursion
            (save-restriction
              (widen)
              (dolist (range ranges)
                (goto-char (car range))
                (while (and (not (and table special-block latex-candidate))
                            (re-search-forward
                             my/org-buffer-feature--scan-regexp (cdr range) t))
                  (cond
                   ((match-beginning 1)
                    (setq table t))
                   ((match-beginning 2)
                    (setq special-block t))
                   ((match-beginning 3)
                    (setq latex-candidate t)))))))
          (setq-local my/org-buffer-feature--latched
                      (list :table table
                            :special-block special-block
                            :latex-candidate latex-candidate)))
        (setq-local my/org-buffer-feature--cache
                    (list :tick tick
                          :ranges ranges
                          :table table
                          :special-block special-block
                          :latex-candidate latex-candidate))))
    my/org-buffer-feature--cache))

(defun my/org-buffer-feature-present-p (feature)
  "Return non-nil when cached Org FEATURE is present in the buffer."
  (plist-get (my/org-buffer-feature--scan) feature))

(defun my/org-rich-ui-buffer-p ()
  "Return non-nil when the current Org buffer should use rich UI helpers."
  (my/rich-ui-buffer-p nil my/org-rich-ui-max-buffer-size))

(defun my/org-enable-typography-maybe ()
  "Enable shared prose typography for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (my/typography-setup-prose-buffer)))

(defun my/org-setup-buffer-spacing ()
  "Use relaxed line spacing in Org buffers only."
  (setq-local line-spacing my/prose-line-spacing))

(defun my/org-inline-image--signature (&optional buffer)
  "Return visible inline image refresh inputs for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'org-mode)
          (let ((ranges (my/org-buffer-visible-ranges
                         buffer my/org-inline-image-visible-margin-lines t)))
            (when ranges
              (list (buffer-chars-modified-tick) ranges))))))))

(defun my/org-display-inline-images-visible-now (&optional buffer refresh)
  "Display inline images in BUFFER's visible Org ranges.
When REFRESH is non-nil, existing inline image overlays in those ranges are
refreshed."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local my/org-inline-image--refresh-timer nil)
        (when (derived-mode-p 'org-mode)
          (let ((ranges (my/org-buffer-visible-ranges
                         buffer my/org-inline-image-visible-margin-lines t)))
            (dolist (range ranges)
              (if (fboundp 'org-link-preview-region)
                  (org-link-preview-region nil refresh
                                           (car range) (cdr range))
                (with-suppressed-warnings ((obsolete org-display-inline-images))
                  (org-display-inline-images nil refresh
                                             (car range) (cdr range)))))
            (setq-local my/org-inline-image--last-signature
                        (and ranges
                             (list (buffer-chars-modified-tick) ranges)))))))))

(defun my/org-cancel-visible-inline-image-refresh ()
  "Cancel the pending visible inline image refresh timer."
  (when (timerp my/org-inline-image--refresh-timer)
    (cancel-timer my/org-inline-image--refresh-timer))
  (setq-local my/org-inline-image--refresh-timer nil))

(defun my/org-schedule-visible-inline-image-refresh (&optional force refresh)
  "Schedule a coalesced visible inline image refresh for the current buffer."
  (when (and my/org-inline-image-on-demand
             (derived-mode-p 'org-mode))
    (let ((signature (my/org-inline-image--signature)))
      (when (and signature
                 (or force
                     (not (equal signature
                                 my/org-inline-image--last-signature))))
        (my/org-cancel-visible-inline-image-refresh)
        (setq-local my/org-inline-image--refresh-timer
                    (run-with-idle-timer
                     my/org-inline-image-idle-delay nil
                     #'my/org-display-inline-images-visible-now
                     (current-buffer) refresh))))))

(defun my/org-inline-image-window-scroll-h (window _start)
  "Schedule visible inline image refresh after WINDOW scrolls."
  (when (and my/org-inline-image-on-demand
             (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when my/org-inline-image--enabled
        (my/org-schedule-visible-inline-image-refresh)))))

(defun my/org-inline-image-window-size-h (_frame)
  "Schedule visible inline image refresh after a window size change."
  (when my/org-inline-image--enabled
    (my/org-schedule-visible-inline-image-refresh)))

(defun my/org-cleanup-visible-inline-images ()
  "Remove visible inline image refresh hooks from the current Org buffer."
  (my/org-cancel-visible-inline-image-refresh)
  (remove-hook 'window-scroll-functions
               #'my/org-inline-image-window-scroll-h t)
  (remove-hook 'window-size-change-functions
               #'my/org-inline-image-window-size-h t)
  (remove-hook 'change-major-mode-hook
               #'my/org-cleanup-visible-inline-images t)
  (remove-hook 'kill-buffer-hook
               #'my/org-cleanup-visible-inline-images t)
  (setq-local my/org-inline-image--enabled nil)
  (setq-local my/org-inline-image--last-signature nil))

(defun my/org-setup-visible-inline-images ()
  "Enable visible-range inline image refresh in Org buffers."
  (when (and my/org-inline-image-on-demand
             (derived-mode-p 'org-mode)
             (not my/org-inline-image--enabled))
    (setq-local my/org-inline-image--enabled t)
    (add-hook 'window-scroll-functions
              #'my/org-inline-image-window-scroll-h nil t)
    (add-hook 'window-size-change-functions
              #'my/org-inline-image-window-size-h nil t)
    (add-hook 'change-major-mode-hook
              #'my/org-cleanup-visible-inline-images nil t)
    (add-hook 'kill-buffer-hook
              #'my/org-cleanup-visible-inline-images nil t)
    (my/org-schedule-visible-inline-image-refresh t nil)))

(defun my/org-schedule-visible-inline-images-for-windows (&rest _)
  "Schedule inline image refresh for visible Org windows."
  (when my/org-inline-image-on-demand
    (dolist (window (window-list nil 'no-minibuf))
      (when-let* ((buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (and my/org-inline-image--enabled
                     (derived-mode-p 'org-mode))
            (my/org-schedule-visible-inline-image-refresh)))))))

(defun my/org-setup-low-power-profile ()
  "Apply Org-local defaults that lower idle, typing and navigation wakeups."
  (when my/org-low-power-profile
    (if my/org-display-line-numbers
        (setq-local display-line-numbers my/org-display-line-numbers)
      (display-line-numbers-mode -1))
    (setq-local company-idle-delay my/org-company-idle-delay)
    (setq-local copilot-idle-delay my/org-copilot-idle-delay)
    (setq-local flymake-no-changes-timeout my/org-flymake-no-changes-timeout)
    (setq-local my/flymake-diagnostic-at-point-delay
                my/org-flymake-diagnostic-at-point-delay)
    (unless my/org-flymake-diagnostic-at-point
      (when (and (boundp 'my/flymake-diagnostic-at-point-mode)
                 (bound-and-true-p my/flymake-diagnostic-at-point-mode)
                 (fboundp 'my/flymake-diagnostic-at-point-mode))
        (my/flymake-diagnostic-at-point-mode -1)))))

(defun my/org-flymake-diagnostic-at-point-mode-sync-a (orig &rest args)
  "Keep the Flymake echo helper out of Org low-power buffers."
  (if (and my/org-low-power-profile
           (derived-mode-p 'org-mode)
           (not my/org-flymake-diagnostic-at-point))
      (when (and (boundp 'my/flymake-diagnostic-at-point-mode)
                 (bound-and-true-p my/flymake-diagnostic-at-point-mode)
                 (fboundp 'my/flymake-diagnostic-at-point-mode))
        (my/flymake-diagnostic-at-point-mode -1))
    (apply orig args)))

(defun my/org-setup-flymake-diagnostic-advice ()
  "Install Org-specific low-power advice for Flymake point diagnostics."
  (when (and (fboundp 'my/flymake-diagnostic-at-point-mode-sync)
             (not (advice-member-p
                   #'my/org-flymake-diagnostic-at-point-mode-sync-a
                   'my/flymake-diagnostic-at-point-mode-sync)))
    (advice-add 'my/flymake-diagnostic-at-point-mode-sync
                :around #'my/org-flymake-diagnostic-at-point-mode-sync-a)))

(my/org-setup-flymake-diagnostic-advice)
(with-eval-after-load 'init-lsp
  (my/org-setup-flymake-diagnostic-advice))

(defun my/org-force-indent-mode ()
  "Enable `org-indent-mode' in every Org buffer."
  (when (derived-mode-p 'org-mode)
    (org-indent-mode 1)))

(defun my/org-force-indent-mode-in-existing-buffers ()
  "Enable `org-indent-mode' for Org buffers that already exist."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/org-force-indent-mode))))

(defun my/org-setup-indent-after-local-variables ()
  "Keep `org-indent-mode' enabled after file-local variables are applied."
  (add-hook 'hack-local-variables-hook #'my/org-force-indent-mode nil t))

(defun my/org-toc--block-regexp ()
  "Return regexp matching the managed overview TOC block."
  "^[ \t]*#\\+begin_overview\\_>.*:toc\\_>")

(defun my/org-toc--find-block (&optional full-scan)
  "Return (BEGIN . END) for the managed overview TOC block, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((limit (unless full-scan
                   (and (integerp my/org-toc-auto-search-limit)
                        (> my/org-toc-auto-search-limit 0)
                        (min (point-max)
                             (+ (point-min)
                                my/org-toc-auto-search-limit))))))
      (when (re-search-forward (my/org-toc--block-regexp) limit t)
        (let ((begin (line-beginning-position)))
          (when (re-search-forward "^[ \t]*#\\+end_overview\\_>" nil t)
            (cons begin (min (point-max) (1+ (line-end-position))))))))))

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

(defun my/org-toc--refresh-cached-block (&optional full-scan)
  "Refresh cached managed TOC bounds and return them."
  (let ((block (my/org-toc--find-block full-scan)))
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
  (let* ((full-scan (called-interactively-p 'interactive))
         (block (my/org-toc--find-block full-scan))
         (depth (cond
                 ((numberp depth) depth)
                 ((consp depth) (prefix-numeric-value depth))
                 (block (or (my/org-toc--block-depth block) my/org-toc-depth))
                 (t my/org-toc-depth)))
         (contents (my/org-toc--contents depth))
         (block (or block (my/org-toc--find-block full-scan)))
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
    (my/org-toc--refresh-cached-block full-scan)))

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
         (org-mode . my/org-setup-low-power-profile)
         (org-mode . my/org-setup-visible-inline-images)
         (org-mode . my/org-force-indent-mode)
         (org-mode . my/org-setup-indent-after-local-variables)
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

  ;; --- Appearance Basics ---
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-use-sub-superscripts '{})
  (org-ellipsis " ▾")
  (org-image-actual-width nil)
  (org-startup-with-inline-images nil)
  (org-display-remote-inline-images t)
  (org-imenu-depth 4)
  
  ;; --- Navigation & Editing ---
  (org-return-follows-link nil)
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; --- Search ---
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)

  ;; --- Links ---
  (org-link-abbrev-alist
   '(("GitHub" . "https://github.com/")
     ("Google" . "https://google.com/search?q=")
     ("RFCs"   . "https://tools.ietf.org/html/")))
  
  ;; --- Citations ---
  (org-cite-global-bibliography pv/org-bibtex-files)

  ;; --- Performance ---
  ;; Default 0.5 s fires too aggressively during continuous editing.  1.5 s
  ;; batches more changes per sync pass without noticeable latency on navigation.
  (org-element-cache-sync-idle-time 1.5)
  (org-element-cache-sync-duration 0.1))
(my/org-force-indent-mode-in-existing-buffers)

(add-hook 'window-configuration-change-hook
          #'my/org-schedule-visible-inline-images-for-windows)
(when (boundp 'window-buffer-change-functions)
  (add-hook 'window-buffer-change-functions
            #'my/org-schedule-visible-inline-images-for-windows))

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
