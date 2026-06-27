;;; init-lean-eglot.el --- Lean 4 custom eglot notifications -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'seq)
(require 'subr-x)

(declare-function lean-progress-mode-line-refresh "init-lean")
(declare-function lean-dev-log "init-lean" (format-string &rest args))
(declare-function eglot--flymake-diag-type "eglot" (severity))
(declare-function eglot--lsp-position-to-point "eglot" (pos-plist &optional marker))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function flymake-make-diagnostic "flymake"
                  (locus beg end type text &optional data overlay-properties))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defvar eglot--docver)
(defvar flymake-error-bitmap)
(defvar flymake-warning-bitmap)
(defvar flymake-note-bitmap)
(defvar flymake-diagnostic-functions)

(defcustom lean-sideline-enabled nil
  "When non-nil, render Lean diagnostics and progress at line end."
  :type 'boolean
  :group 'lean)

(defcustom lean-sideline-delay 0.06
  "Seconds to debounce Lean sideline overlay refreshes."
  :type 'number
  :group 'lean)

(defcustom lean-sideline-max-message-length 96
  "Maximum characters shown in a Lean sideline message."
  :type 'integer
  :group 'lean)

(defcustom lean-sideline-minimum-severity 'warning
  "Minimum diagnostic severity rendered in Lean sideline overlays.
Use `note' to show all Lean informational output, including `#check' results."
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Warnings and errors" warning)
                 (const :tag "Notes, warnings, and errors" note))
  :group 'lean)

(defcustom lean-progress-fringe-enabled t
  "When non-nil, show Lean file-progress markers in the fringe."
  :type 'boolean
  :group 'lean)

(defcustom lean-declaration-fringe-enabled t
  "When non-nil, show Lean declaration entry markers in the left fringe."
  :type 'boolean
  :group 'lean)

(defcustom lean-notification-debounce-delay 0.10
  "Seconds to coalesce Lean diagnostics and progress UI notifications."
  :type 'number
  :group 'lean)

(defcustom lean-sideline-prefixes
  '((error . "E")
    (warning . "W")
    (note . "N")
    (processing . "~")
    (blocked . "!"))
  "Small prefix set used by Lean sideline overlays."
  :type '(alist :key-type symbol :value-type string)
  :group 'lean)

;; ── Per-buffer progress state ─────────────────────────────────────────────────

(defvar-local lean--file-progress nil
  "Vector of Lean file-progress items for the current buffer.")

(defvar-local lean--flymake-diagnostics nil
  "Current Flymake diagnostics produced from Lean publishDiagnostics.")

(defvar-local lean--flymake-counts '(:error 0 :warning 0 :note 0)
  "Cached diagnostic counts for Lean Flymake diagnostics.")

(defvar-local lean--flymake-report-fn nil
  "Flymake report function for `lean-flymake-backend'.")

(defvar-local lean--raw-diagnostics nil
  "Latest complete raw Lean diagnostics state.")

(defvar-local lean--diagnostics-version nil
  "Document version associated with `lean--raw-diagnostics'.")

(defvar-local lean--notification-timer nil
  "Timer used to coalesce Lean diagnostics and progress rendering.")

(defvar-local lean--fringe-overlays nil
  "Fringe overlays for Lean file-progress and task markers.")

(defvar-local lean--progress-fringe-timer nil
  "Debounce timer for viewport-bounded progress fringe refresh.")

(defvar-local lean--task-overlays nil
  "Fringe overlays for Lean goal status markers.")

(defvar-local lean--declaration-fringe-overlays nil
  "Left-fringe overlays for visible Lean declarations.")

(defvar-local lean--declaration-fringe-timer nil
  "Debounce timer for visible declaration fringe refresh.")

;; ── Lightweight sideline rendering ───────────────────────────────────────────

(defvar-local lean--sideline-overlays nil
  "Line-end overlays for Lean diagnostics and progress.")

(defvar-local lean--sideline-timer nil
  "Debounce timer for Lean sideline rendering.")

(defface lean-sideline-error-face
  '((t :inherit error :weight bold))
  "Face for Lean sideline errors."
  :group 'lean)

(defface lean-sideline-warning-face
  '((t :inherit warning :weight bold))
  "Face for Lean sideline warnings."
  :group 'lean)

(defface lean-sideline-note-face
  '((t :inherit success))
  "Face for Lean sideline notes."
  :group 'lean)

(defface lean-sideline-progress-face
  '((t :inherit shadow))
  "Face for Lean sideline progress."
  :group 'lean)

(defun lean-clear-sideline-overlays ()
  "Delete Lean sideline overlays in the current buffer."
  (mapc #'delete-overlay lean--sideline-overlays)
  (setq lean--sideline-overlays nil))

(defun lean--sideline-cancel ()
  "Cancel a pending Lean sideline refresh."
  (when (timerp lean--sideline-timer)
    (cancel-timer lean--sideline-timer))
  (setq lean--sideline-timer nil))

(defun lean--declaration-fringe-cancel ()
  "Cancel a pending declaration fringe refresh."
  (when (timerp lean--declaration-fringe-timer)
    (cancel-timer lean--declaration-fringe-timer))
  (setq lean--declaration-fringe-timer nil))

(defun lean-sideline-cleanup ()
  "Release Lean sideline hooks, timer, and overlays."
  (lean--sideline-cancel)
  (lean--progress-fringe-cancel)
  (lean--declaration-fringe-cancel)
  (lean-clear-sideline-overlays)
  (lean--clear-declaration-fringe-overlays)
  (remove-hook 'after-change-functions #'lean--schedule-declaration-fringe-refresh t)
  (remove-hook 'window-scroll-functions #'lean-sideline-window-scroll-h t)
  (remove-hook 'window-size-change-functions #'lean-sideline-window-size-h t)
  (remove-hook 'kill-buffer-hook #'lean-sideline-cleanup t))

(defun lean--visible-ranges ()
  "Return visible buffer ranges for sideline rendering."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (if windows
        (mapcar (lambda (window)
                  (cons (window-start window)
                        (window-end window t)))
                windows)
      (list (cons (point-min) (point-max))))))

(defun lean--position-visible-p (pos ranges)
  "Return non-nil when POS is inside one of RANGES."
  (seq-some (lambda (range)
              (and (>= pos (car range))
                   (<= pos (cdr range))))
            ranges))

(defun lean--truncate-message (text)
  "Return TEXT compacted for sideline rendering."
  (let ((msg (string-trim
              (replace-regexp-in-string "[\n\r\t ]+" " " (or text "")))))
    (if (> (length msg) lean-sideline-max-message-length)
        (concat (substring msg 0 (max 0 (- lean-sideline-max-message-length 1)))
                "…")
      msg)))

(defun lean--flymake-kind (diag)
  "Return DIAG kind as `error', `warning', or `note'."
  (let* ((type (flymake-diagnostic-type diag))
         (category (and (symbolp type) (get type 'flymake-category))))
    (cond
     ((memq category '(flymake-error :error error)) 'error)
     ((memq category '(flymake-warning :warning warning)) 'warning)
     ((memq category '(flymake-note :note note)) 'note)
     ((memq type '(eglot-error :error error)) 'error)
     ((memq type '(eglot-warning :warning warning)) 'warning)
     (t 'note))))

(defun lean--sideline-face (kind)
  "Return sideline face for KIND."
  (pcase kind
    ('error 'lean-sideline-error-face)
    ('warning 'lean-sideline-warning-face)
    ('note 'lean-sideline-note-face)
    (_ 'lean-sideline-progress-face)))

(defun lean--sideline-prefix (kind)
  "Return sideline prefix for KIND."
  (or (alist-get kind lean-sideline-prefixes)
      "!"))

(defun lean--sideline-kind-visible-p (kind)
  "Return non-nil when sideline diagnostic KIND should be rendered."
  (pcase lean-sideline-minimum-severity
    ('error (eq kind 'error))
    ('warning (memq kind '(error warning)))
    (_ t)))

(defun lean--sideline-display (kind text)
  "Return an after-string for sideline KIND and TEXT."
  (let* ((body (format " %s %s" (lean--sideline-prefix kind) text))
         (face (lean--sideline-face kind))
         (width (+ 2 (string-width body))))
    (concat
     (propertize " " 'display `(space :align-to (- right ,width)))
     (propertize body
                 'face face
                 'help-echo text
                 'mouse-face 'mode-line-highlight))))

(defun lean--sideline-add (pos kind text)
  "Render a sideline message at line ending for POS."
  (save-excursion
    (goto-char pos)
    (let ((ov (make-overlay (line-end-position) (line-end-position)
                            nil nil t)))
      (overlay-put ov 'lean-sideline t)
      (overlay-put ov 'after-string
                   (lean--sideline-display kind (lean--truncate-message text)))
      (overlay-put ov 'priority 1900)
      (push ov lean--sideline-overlays))))

(defun lean--sideline-diagnostic-entries (ranges)
  "Return visible sideline entries for diagnostics inside RANGES."
  (let (entries)
    (dolist (diag lean--flymake-diagnostics)
      (let ((beg (ignore-errors (flymake-diagnostic-beg diag))))
        (when (and beg (lean--position-visible-p beg ranges))
          (let* ((line (line-number-at-pos beg t))
                 (kind (lean--flymake-kind diag))
                 (text (flymake-diagnostic-text diag))
                 (existing (assoc line entries)))
            (when (lean--sideline-kind-visible-p kind)
              (if existing
                  (setf (plist-get (cdr existing) :extra)
                        (1+ (or (plist-get (cdr existing) :extra) 0)))
                (push (cons line (list :pos beg :kind kind :text text :extra 0))
                      entries)))))))
    entries))

(defun lean--progress-position (item)
  "Return buffer position for Lean file-progress ITEM."
  (when-let* ((range (plist-get item :range))
              (start (plist-get range :start))
              (line (plist-get start :line)))
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (point))))

(defun lean--sideline-progress-entries (ranges occupied-lines)
  "Return visible progress entries not already in OCCUPIED-LINES."
  (let (entries)
    (dolist (item lean--file-progress)
      (when-let* ((pos (lean--progress-position item)))
        (let ((line (line-number-at-pos pos t)))
          (when (and (not (memq line occupied-lines))
                     (lean--position-visible-p pos ranges))
            (push (cons line
                        (list :pos pos
                              :kind (if (eq (plist-get item :kind) 1)
                                        'processing
                                      'blocked)
                              :text (if (eq (plist-get item :kind) 1)
                                        "checking"
                                      "blocked or failed")
                              :extra 0))
                  entries)))))
    entries))

(defun lean--sideline-render-entry (entry)
  "Render one sideline ENTRY."
  (pcase-let* ((`(,_line . ,data) entry)
               (pos (plist-get data :pos))
               (kind (plist-get data :kind))
               (text (plist-get data :text))
               (extra (plist-get data :extra))
               (label (if (> extra 0)
                          (format "%s (+%d)" text extra)
                        text)))
    (lean--sideline-add pos kind label)))

(defun lean-refresh-sideline (&optional buffer)
  "Refresh Lean sideline overlays for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq lean--sideline-timer nil)
        (lean-clear-sideline-overlays)
        (when (and lean-sideline-enabled
                   (derived-mode-p 'lean-mode))
          (let* ((ranges (lean--visible-ranges))
                 (diag-entries (lean--sideline-diagnostic-entries ranges))
                 (occupied (mapcar #'car diag-entries))
                 (progress-entries
                  (lean--sideline-progress-entries ranges occupied)))
            (dolist (entry (sort (append diag-entries progress-entries)
                                  (lambda (a b) (< (car a) (car b)))))
              (lean--sideline-render-entry entry))))))))

(defun lean-schedule-sideline-refresh (&rest _)
  "Schedule a debounced Lean sideline refresh."
  (if lean-sideline-enabled
      (progn
        (lean--sideline-cancel)
        (setq lean--sideline-timer
              (run-at-time lean-sideline-delay nil
                           #'lean-refresh-sideline
                           (current-buffer))))
    (lean--sideline-cancel)
    (lean-clear-sideline-overlays)))

(defun lean-sideline-window-scroll-h (_window _start)
  "Refresh Lean sideline overlays after scrolling."
  (when (derived-mode-p 'lean-mode)
    (lean-schedule-sideline-refresh)
    (lean--schedule-progress-fringe-refresh)
    (lean--schedule-declaration-fringe-refresh)))

(defun lean-sideline-window-size-h (_frame)
  "Refresh Lean sideline overlays after window size changes."
  (when (derived-mode-p 'lean-mode)
    (lean-schedule-sideline-refresh)
    (lean--schedule-progress-fringe-refresh)
    (lean--schedule-declaration-fringe-refresh)))

(defun lean-setup-sideline ()
  "Install Lean sideline rendering hooks in the current buffer."
  (add-hook 'after-change-functions #'lean--schedule-declaration-fringe-refresh nil t)
  (add-hook 'window-scroll-functions #'lean-sideline-window-scroll-h nil t)
  (add-hook 'window-size-change-functions #'lean-sideline-window-size-h nil t)
  (add-hook 'kill-buffer-hook #'lean-sideline-cleanup nil t)
  (lean-schedule-sideline-refresh)
  (lean--schedule-declaration-fringe-refresh))

;; ── publishDiagnostics → Flymake compatibility ───────────────────────────────
;;
;; Lean's publishDiagnostics notifications can carry version 0 even when Eglot's
;; local document version has already advanced.  Stock Eglot treats mismatched
;; versions as stale and drops them before Flymake sees them, which removes
;; underlines, fringe indicators, mode-line counts, and diagnostics panels.

(defun lean--diagnostics-count (diagnostics)
  "Return a display count for LSP DIAGNOSTICS."
  (cond
   ((vectorp diagnostics) (length diagnostics))
   ((listp diagnostics) (length diagnostics))
   (t 0)))

(defun lean--diagnostics-buffer-for-uri (uri)
  "Return the visited buffer for diagnostics URI, if any."
  (when-let* ((path (ignore-errors (eglot-uri-to-path uri))))
    (find-buffer-visiting path)))

(defun lean--diagnostics-list (diagnostics)
  "Return DIAGNOSTICS as a list."
  (cond
   ((vectorp diagnostics) (append diagnostics nil))
   ((listp diagnostics) diagnostics)
   (t nil)))

(defun lean--diagnostic-region (diagnostic)
  "Return the buffer region for Lean LSP DIAGNOSTIC."
  (when-let* ((range (or (plist-get diagnostic :range)
                         (plist-get diagnostic :fullRange)))
              (start (plist-get range :start))
              (end-pos (plist-get range :end)))
    (save-excursion
      (let ((beg (eglot--lsp-position-to-point start))
            (end (eglot--lsp-position-to-point end-pos)))
        (when (= beg end)
          (setq end (min (point-max) (1+ beg))))
        (cons beg end)))))

(defun lean--diagnostic-message (diagnostic)
  "Return a Flymake info payload for Lean LSP DIAGNOSTIC."
  (list (or (plist-get diagnostic :source) "Lean 4")
        (plist-get diagnostic :code)
        (or (plist-get diagnostic :message) "")))

(defun lean--diagnostic-severity-key (diagnostic)
  "Return cached count key for Lean LSP DIAGNOSTIC severity."
  (let ((severity (plist-get diagnostic :severity)))
    (cond
     ((or (null severity) (<= severity 1)) :error)
     ((= severity 2) :warning)
     (t :note))))

(defun lean--count-diagnostics (diagnostics)
  "Return cached E/W/N counts for Lean LSP DIAGNOSTICS."
  (let ((errors 0)
        (warnings 0)
        (notes 0))
    (dolist (diagnostic (lean--diagnostics-list diagnostics))
      (pcase (lean--diagnostic-severity-key diagnostic)
        (:error (setq errors (1+ errors)))
        (:warning (setq warnings (1+ warnings)))
        (_ (setq notes (1+ notes)))))
    (list :error errors :warning warnings :note notes)))

(defun lean--diagnostic-silent-p (diagnostic)
  "Return non-nil when Lean DIAGNOSTIC should not enter Flymake."
  (eq (plist-get diagnostic :isSilent) t))

(defun lean--visible-diagnostics ()
  "Return raw Lean diagnostics that should be visible through Flymake."
  (seq-remove #'lean--diagnostic-silent-p lean--raw-diagnostics))

(defun lean--diagnostic-to-flymake (diagnostic version)
  "Convert Lean LSP DIAGNOSTIC at VERSION to a Flymake diagnostic."
  (when-let* ((region (lean--diagnostic-region diagnostic)))
    (flymake-make-diagnostic
     (current-buffer)
     (car region)
     (cdr region)
     (eglot--flymake-diag-type (plist-get diagnostic :severity))
     (lean--diagnostic-message diagnostic)
     `((eglot-lsp-diag . ,diagnostic)
       (eglot--doc-version . ,version)))))

(defun lean--publish-flymake-diagnostics (diagnostics version)
  "Publish visible Lean LSP DIAGNOSTICS at VERSION through Flymake."
  (setq lean--flymake-counts (lean--count-diagnostics diagnostics))
  (setq lean--flymake-diagnostics
        (delq nil
              (mapcar (lambda (diagnostic)
                        (lean--diagnostic-to-flymake diagnostic version))
                      (lean--diagnostics-list diagnostics))))
  (when lean--flymake-report-fn
    (funcall lean--flymake-report-fn lean--flymake-diagnostics
             :force t :region (cons (point-min) (point-max)))))

(defun lean-flymake-backend (report-fn &rest _args)
  "Flymake backend fed by Lean publishDiagnostics notifications."
  (setq lean--flymake-report-fn report-fn)
  (funcall report-fn lean--flymake-diagnostics
           :force t :region (cons (point-min) (point-max))))

(defun lean-setup-flymake-backend ()
  "Install the Lean publishDiagnostics Flymake backend in the current buffer."
  (when (boundp 'flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions
                (cons #'lean-flymake-backend
                      (remove #'lean-flymake-backend
                              (remove #'eglot-flymake-backend
                                      flymake-diagnostic-functions)))))
  (setq-local flymake-error-bitmap
              '(lean-fringe-blocked-bitmap lean-fringe-error-face)
              flymake-warning-bitmap
              '(lean-fringe-warning-bitmap lean-fringe-warning-face)
              flymake-note-bitmap
              '(lean-fringe-note-bitmap lean-fringe-note-face))
  (when (fboundp 'lean-dev-log)
    (lean-dev-log "flymake backend installed: funcs=%S"
                  (and (boundp 'flymake-diagnostic-functions)
                       flymake-diagnostic-functions))))

(defun lean--schedule-notification-flush ()
  "Schedule one rendering pass for the latest Lean notification state."
  (unless (timerp lean--notification-timer)
    (setq lean--notification-timer
          (run-at-time (max 0 lean-notification-debounce-delay) nil
                       #'lean--flush-notifications (current-buffer)))))

(defun lean--flush-notifications (buffer)
  "Render the latest Lean notification state for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lean--notification-timer nil)
      (lean--publish-flymake-diagnostics
       (lean--visible-diagnostics) lean--diagnostics-version)
      (lean--update-task-overlays lean--raw-diagnostics)
      (if lean-progress-fringe-enabled
          (lean--update-fringe-overlays lean--file-progress)
        (lean--clear-progress-overlays))
      (when (fboundp 'lean-progress-mode-line-refresh)
        (lean-progress-mode-line-refresh))
      (when (fboundp 'lean-schedule-sideline-refresh)
        (lean-schedule-sideline-refresh)))))

(defun lean--record-diagnostics (diagnostics version incremental)
  "Record Lean DIAGNOSTICS at VERSION, appending when INCREMENTAL."
  (let ((incoming (lean--diagnostics-list diagnostics)))
    (setq lean--raw-diagnostics
          (if incremental
              (nconc lean--raw-diagnostics incoming)
            incoming)
          lean--diagnostics-version
          (or (and (boundp 'eglot--docver) eglot--docver) version)))
  (lean--schedule-notification-flush))

(cl-defmethod eglot-handle-notification :around
  (_server (_method (eql textDocument/publishDiagnostics))
          &key uri diagnostics version isIncremental &allow-other-keys)
  "Handle Lean diagnostics with batching, otherwise use Eglot's normal path."
  (let ((buf (lean--diagnostics-buffer-for-uri uri)))
    (if (and (buffer-live-p buf)
             (with-current-buffer buf (derived-mode-p 'lean-mode)))
        (with-current-buffer buf
          (lean--record-diagnostics diagnostics version (eq isIncremental t))
          (when (fboundp 'lean-dev-log)
            (lean-dev-log
             "publishDiagnostics queued: file=%s diagnostics=%d incremental=%S"
             (file-name-nondirectory (or buffer-file-name (buffer-name)))
             (lean--diagnostics-count diagnostics) isIncremental)))
      (cl-call-next-method))))

;; ── fileProgress notification ─────────────────────────────────────────────────
;; Lean sends: {"textDocument": {"uri": "..."}, "processing": [{...}]}

(defun lean--file-progress-list (processing)
  "Return PROCESSING as a list."
  (cond
   ((vectorp processing) (append processing nil))
   ((listp processing) processing)
   (t nil)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql \$/lean/fileProgress))
   &key textDocument processing &allow-other-keys)
  "Handle $/lean/fileProgress from Lean LSP server."
  (when-let* ((uri  (plist-get textDocument :uri))
              (path (ignore-errors (eglot-uri-to-path uri)))
              (buf  (find-buffer-visiting path)))
    (with-current-buffer buf
      (let ((items (lean--file-progress-list processing)))
        (unless (equal items lean--file-progress)
          (setq-local lean--file-progress items)
          (lean--schedule-notification-flush)
          (when (fboundp 'lean-dev-log)
            (lean-dev-log "fileProgress: file=%s items=%d"
                          (file-name-nondirectory path)
                          (length lean--file-progress))))))))

;; ── Fringe overlays ───────────────────────────────────────────────────────────

(defface lean-fringe-processing-face
  '((((background dark)) :foreground "#4AA5FF" :weight bold)
    (((background light)) :foreground "#006EDC" :weight bold))
  "Fringe face for Lean files being elaborated."
  :group 'lean)

(defface lean-fringe-error-face
  '((((background dark)) :foreground "#FF5F5F" :weight bold)
    (((background light)) :foreground "#C00000" :weight bold))
  "Fringe face for Lean files with errors."
  :group 'lean)

(defface lean-fringe-warning-face
  '((((background dark)) :foreground "#FFB454" :weight bold)
    (((background light)) :foreground "#A85B00" :weight bold))
  "Fringe face for Lean warnings."
  :group 'lean)

(defface lean-fringe-note-face
  '((((background dark)) :foreground "#5FD7FF" :weight bold)
    (((background light)) :foreground "#007C91" :weight bold))
  "Fringe face for Lean notes."
  :group 'lean)

(defface lean-fringe-declaration-face
  '((t :inherit shadow :weight bold))
  "Fringe face for visible Lean declaration entries."
  :group 'lean)

(define-fringe-bitmap 'lean-fringe-processing-bitmap
  [#b00000000
   #b00011000
   #b00111100
   #b01111110
   #b00111100
   #b00011000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'lean-fringe-blocked-bitmap
  [#b00011000
   #b00111100
   #b00111100
   #b00011000
   #b00011000
   #b00000000
   #b00011000
   #b00000000])

(define-fringe-bitmap 'lean-fringe-warning-bitmap
  [#b00000000
   #b00011000
   #b00111100
   #b01111110
   #b11111111
   #b00011000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'lean-fringe-note-bitmap
  [#b00000000
   #b00011000
   #b00111100
   #b00111100
   #b00111100
   #b00011000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'lean-fringe-declaration-bitmap
  [#b01111110
   #b01000000
   #b01111000
   #b01000000
   #b01000000
   #b01000000
   #b00000000
   #b00000000])

(defface lean-fringe-success-face
  '((((background dark)) :foreground "#7BD88F" :weight bold)
    (((background light)) :foreground "#137333" :weight bold))
  "Fringe face for accomplished Lean goals."
  :group 'lean)

(define-fringe-bitmap 'lean-fringe-progress-bar-bitmap
  [#b00000110 #b00000110 #b00000110 #b00000110
   #b00000110 #b00000110 #b00000110 #b00000110])

(define-fringe-bitmap 'lean-fringe-success-bitmap
  [#b00000000 #b01000010 #b00100100 #b00011000
   #b01000010 #b00100100 #b00011000 #b00000000])

(define-fringe-bitmap 'lean-fringe-wip-bitmap
  [#b01111110 #b01011010 #b01011010 #b01011010
   #b01011010 #b01111110 #b00000000 #b00000000])

(defun lean--clear-task-overlays ()
  "Remove all Lean task status overlays from current buffer."
  (mapc #'delete-overlay lean--task-overlays)
  (setq lean--task-overlays nil))

(defconst lean--declaration-fringe-regexp
  (rx line-start (* blank)
      (? (seq "private" (+ blank)))
      (? (seq "noncomputable" (+ blank)))
      (? (seq "unsafe" (+ blank)))
      (or "def" "theorem" "lemma" "example" "structure" "class" "inductive"
          "abbrev" "instance" "axiom" "opaque" "constant")
      symbol-end)
  "Regexp matching Lean declaration lines worth marking in the fringe.")

(defun lean--clear-declaration-fringe-overlays ()
  "Remove Lean declaration fringe overlays from current buffer."
  (mapc #'delete-overlay lean--declaration-fringe-overlays)
  (setq lean--declaration-fringe-overlays nil))

(defun lean--declaration-line-help ()
  "Return tooltip text for the declaration marker on the current line."
  (format "Lean declaration: %s"
          (string-trim
           (buffer-substring-no-properties
            (line-beginning-position) (line-end-position)))))

(defun lean--update-declaration-fringe-overlays ()
  "Update left-fringe declaration markers, limited to visible lines."
  (lean--clear-declaration-fringe-overlays)
  (when (and lean-declaration-fringe-enabled
             (derived-mode-p 'lean-mode))
    (save-excursion
      (dolist (range (lean--visible-ranges))
        (goto-char (car range))
        (beginning-of-line)
        (while (re-search-forward lean--declaration-fringe-regexp (cdr range) t)
          (unless (nth 8 (syntax-ppss))
            (setq lean--declaration-fringe-overlays
                  (lean--add-fringe-overlay
                   (line-beginning-position) 'lean-fringe-declaration-bitmap
                   'lean-fringe-declaration-face (lean--declaration-line-help)
                   'lean-declaration-fringe lean--declaration-fringe-overlays
                   'left-fringe)))
          (forward-line 1))))))

(defun lean--refresh-declaration-fringe (&optional buffer)
  "Redraw visible declaration fringe markers for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq lean--declaration-fringe-timer nil)
        (if lean-declaration-fringe-enabled
            (lean--update-declaration-fringe-overlays)
          (lean--clear-declaration-fringe-overlays))))))

(defun lean--schedule-declaration-fringe-refresh (&rest _)
  "Schedule a debounced viewport declaration fringe refresh."
  (lean--declaration-fringe-cancel)
  (setq lean--declaration-fringe-timer
        (run-at-time lean-sideline-delay nil
                     #'lean--refresh-declaration-fringe (current-buffer))))

(defun lean--diagnostic-tags (diagnostic)
  "Return Lean tags from DIAGNOSTIC as a list."
  (lean--diagnostics-list (plist-get diagnostic :leanTags)))

(defun lean--add-fringe-overlay (pos bitmap face help property collection &optional side)
  "Add a fringe marker at POS and return updated COLLECTION.
BITMAP, FACE, HELP, and PROPERTY describe the marker.
SIDE defaults to `right-fringe'."
  (save-excursion
    (goto-char pos)
    (let ((ov (make-overlay (line-beginning-position)
                            (min (point-max) (1+ (line-beginning-position)))
                            nil nil t)))
      (overlay-put ov 'before-string
                   (propertize " " 'display `(,(or side 'right-fringe) ,bitmap ,face)
                               'help-echo help))
      (overlay-put ov 'help-echo help)
      (overlay-put ov property t)
      (overlay-put ov 'priority 2000)
      (cons ov collection))))

(defun lean--update-task-overlays (diagnostics)
  "Update Lean goal status markers from raw DIAGNOSTICS."
  (lean--clear-task-overlays)
  (dolist (diagnostic diagnostics)
    (when-let* ((tags (lean--diagnostic-tags diagnostic))
                (region (lean--diagnostic-region diagnostic)))
      (cond
       ((memq 2 tags)
        (setq lean--task-overlays
              (lean--add-fringe-overlay
               (car region) 'lean-fringe-success-bitmap
               'lean-fringe-success-face "Lean goals accomplished"
               'lean-task-fringe lean--task-overlays)))
       ((memq 1 tags)
        (setq lean--task-overlays
              (lean--add-fringe-overlay
               (car region) 'lean-fringe-wip-bitmap
               'lean-fringe-warning-face "Lean has unsolved goals"
               'lean-task-fringe lean--task-overlays)))))))

(defun lean--progress-help (item)
  "Return tooltip text for Lean file-progress ITEM."
  (let* ((kind (plist-get item :kind))
         (range (plist-get item :range))
         (start (plist-get range :start))
         (line (plist-get start :line))
         (status (if (or (null kind) (eq kind 1))
                     "processing" "blocked/error")))
    (format "Lean %s at line %s" status (and line (1+ line)))))

(defun lean--progress-region (item)
  "Return the buffer region covered by Lean file-progress ITEM."
  (when-let* ((range (plist-get item :range))
              (start (plist-get range :start))
              (end (plist-get range :end)))
    (cons (eglot--lsp-position-to-point start)
          (eglot--lsp-position-to-point end))))

(defun lean--progress-fringe-cancel ()
  "Cancel a pending progress fringe refresh."
  (when (timerp lean--progress-fringe-timer)
    (cancel-timer lean--progress-fringe-timer))
  (setq lean--progress-fringe-timer nil))

(defun lean--refresh-progress-fringe (&optional buffer)
  "Redraw visible progress fringe markers for BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq lean--progress-fringe-timer nil)
        (when (and lean-progress-fringe-enabled
                   (derived-mode-p 'lean-mode))
          (lean--update-fringe-overlays lean--file-progress))))))

(defun lean--schedule-progress-fringe-refresh ()
  "Schedule a debounced viewport progress fringe refresh."
  (lean--progress-fringe-cancel)
  (setq lean--progress-fringe-timer
        (run-at-time lean-sideline-delay nil
                     #'lean--refresh-progress-fringe (current-buffer))))

(defun lean--update-fringe-overlays (items)
  "Update fringe overlays from progress ITEMS, limited to visible lines."
  (lean--clear-progress-overlays)
  (let ((ranges (lean--visible-ranges)))
    (dolist (item items)
      (when-let* ((region (lean--progress-region item)))
        (let* ((kind       (plist-get item :kind))
               (processing (or (null kind) (eq kind 1)))
               (face       (if processing 'lean-fringe-processing-face
                             'lean-fringe-error-face))
               (help       (lean--progress-help item)))
          (dolist (vr ranges)
            (let ((beg (max (car region) (car vr)))
                  (end (min (cdr region) (cdr vr))))
              (when (< beg end)
                (save-excursion
                  (goto-char beg)
                  (let ((done nil))
                    (while (not done)
                      (setq lean--fringe-overlays
                            (lean--add-fringe-overlay
                             (point) 'lean-fringe-progress-bar-bitmap
                             face help 'lean-fringe lean--fringe-overlays))
                      (setq done
                            (or (>= (line-end-position) end)
                                (/= (forward-line 1) 0))))))))))))))

(defun lean--clear-progress-overlays ()
  "Remove Lean file-progress overlays from current buffer."
  (mapc #'delete-overlay lean--fringe-overlays)
  (setq lean--fringe-overlays nil))

(defun lean--clear-fringe-overlays ()
  "Remove all Lean fringe overlays from current buffer."
  (lean--clear-progress-overlays)
  (lean--clear-task-overlays)
  (lean--clear-declaration-fringe-overlays)
  (when (fboundp 'lean-clear-sideline-overlays)
    (lean-clear-sideline-overlays)))

(defun lean-notification-cleanup ()
  "Cancel pending Lean notification work and clear its UI state."
  (when (timerp lean--notification-timer)
    (cancel-timer lean--notification-timer))
  (lean--progress-fringe-cancel)
  (lean--declaration-fringe-cancel)
  (when lean--flymake-report-fn
    (funcall lean--flymake-report-fn nil
             :force t :region (cons (point-min) (point-max))))
  (setq lean--notification-timer nil
        lean--raw-diagnostics nil
        lean--diagnostics-version nil
        lean--flymake-diagnostics nil
        lean--flymake-counts '(:error 0 :warning 0 :note 0)
        lean--file-progress nil)
  (lean--clear-fringe-overlays))

;; ── Refresh file dependencies ──────────────────────────────────────────────────

(defun lean-refresh-file-dependencies ()
  "Force Lean to re-process all imports by bouncing the file in the LSP."
  (interactive)
  (when-let* ((server (ignore-errors (eglot-current-server))))
    (when (fboundp 'lean-dev-log)
      (lean-dev-log "refresh dependencies: buffer=%s" (buffer-name)))
    (let ((id   (eglot--TextDocumentIdentifier))
          (item (eglot--TextDocumentItem)))
      ;; Send outside eglot's tracking: tell the server to forget + reopen.
      ;; This is the same trick lean4-mode uses; harmless because eglot will
      ;; resync on the next edit via its own after-change hooks.
      (jsonrpc-notify server 'textDocument/didClose `(:textDocument ,id))
      (run-at-time 0.15 nil
                   (let ((b (current-buffer)))
                     (lambda ()
                       (when (buffer-live-p b)
                         (with-current-buffer b
                           (when-let* ((srv (ignore-errors (eglot-current-server))))
                             (jsonrpc-notify srv 'textDocument/didOpen
                                             `(:textDocument ,item)))))))))))

;; ── lean/restartFile custom notification (from infoview proxy) ────────────────
;; The lean-proxy.mjs sends a lean/restartFile notification to Eglot when the
;; infoview's "restart file" button is clicked.  Eglot receives it as a
;; server→client notification and dispatches it here.

(cl-defmethod eglot-handle-notification
  (_server (_method (eql lean/restartFile)) &key uri)
  "Handle a lean/restartFile notification from the infoview proxy.
Finds the buffer for URI and calls `lean-refresh-file-dependencies'."
  (when-let* ((path (ignore-errors (eglot-uri-to-path uri)))
              (buf  (find-buffer-visiting path)))
    (with-current-buffer buf
      (when (fboundp 'lean-refresh-file-dependencies)
        (lean-refresh-file-dependencies))
      (when (fboundp 'lean-dev-log)
        (lean-dev-log "lean/restartFile notification: uri=%s" uri)))))

(provide 'init-lean-eglot)
;;; init-lean-eglot.el ends here
