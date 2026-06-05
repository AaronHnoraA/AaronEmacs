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

(defcustom lean-progress-fringe-enabled nil
  "When non-nil, show Lean file-progress markers in the fringe."
  :type 'boolean
  :group 'lean)

(defcustom lean-diagnostic-fringe-enabled t
  "When non-nil, show Lean diagnostic markers in the fringe."
  :type 'boolean
  :group 'lean)

(defcustom lean-diagnostic-fringe-minimum-severity 'note
  "Minimum diagnostic severity rendered as Lean fringe markers."
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Warnings and errors" warning)
                 (const :tag "Notes, warnings, and errors" note))
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

(defvar-local lean--file-progress-signature nil
  "Last compact signature for Lean file-progress overlays.")

(defvar-local lean--flymake-diagnostics nil
  "Current Flymake diagnostics produced from Lean publishDiagnostics.")

(defvar-local lean--flymake-counts '(:error 0 :warning 0 :note 0)
  "Cached diagnostic counts for Lean Flymake diagnostics.")

(defvar-local lean--flymake-report-fn nil
  "Flymake report function for `lean-flymake-backend'.")

(defvar-local lean--fringe-overlays nil
  "Fringe overlays for Lean file-progress markers.")

(defvar-local lean--diagnostic-fringe-overlays nil
  "Fringe overlays for Lean diagnostic markers.")

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

(defun lean-sideline-cleanup ()
  "Release Lean sideline hooks, timer, and overlays."
  (lean--sideline-cancel)
  (lean-clear-sideline-overlays)
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
    (lean-schedule-sideline-refresh)))

(defun lean-sideline-window-size-h (_frame)
  "Refresh Lean sideline overlays after window size changes."
  (when (derived-mode-p 'lean-mode)
    (lean-schedule-sideline-refresh)))

(defun lean-setup-sideline ()
  "Install Lean sideline rendering hooks in the current buffer."
  (add-hook 'window-scroll-functions #'lean-sideline-window-scroll-h nil t)
  (add-hook 'window-size-change-functions #'lean-sideline-window-size-h nil t)
  (add-hook 'kill-buffer-hook #'lean-sideline-cleanup nil t)
  (lean-schedule-sideline-refresh))

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
  "Publish Lean LSP DIAGNOSTICS at VERSION through Flymake."
  (setq lean--flymake-counts (lean--count-diagnostics diagnostics))
  (setq lean--flymake-diagnostics
        (delq nil
              (mapcar (lambda (diagnostic)
                        (lean--diagnostic-to-flymake diagnostic version))
                      (lean--diagnostics-list diagnostics))))
  (when lean--flymake-report-fn
    (funcall lean--flymake-report-fn lean--flymake-diagnostics))
  (when (fboundp 'lean-progress-mode-line-refresh)
    (lean-progress-mode-line-refresh))
  (when (fboundp 'lean-schedule-sideline-refresh)
    (lean-schedule-sideline-refresh))
  (if lean-diagnostic-fringe-enabled
      (lean--update-diagnostic-fringe-overlays lean--flymake-diagnostics)
    (lean--clear-diagnostic-fringe-overlays))
  (force-mode-line-update))

(defun lean-flymake-backend (report-fn &rest _args)
  "Flymake backend fed by Lean publishDiagnostics notifications."
  (setq lean--flymake-report-fn report-fn)
  (funcall report-fn lean--flymake-diagnostics))

(defun lean-setup-flymake-backend ()
  "Install the Lean publishDiagnostics Flymake backend in the current buffer."
  (when (boundp 'flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions
                (cons #'lean-flymake-backend
                      (remove #'lean-flymake-backend
                              (remove #'eglot-flymake-backend
                                      flymake-diagnostic-functions)))))
  (when (fboundp 'lean-dev-log)
    (lean-dev-log "flymake backend installed: funcs=%S"
                  (and (boundp 'flymake-diagnostic-functions)
                       flymake-diagnostic-functions))))

(defun lean--eglot-flymake-handle-push-a
    (fn server uri diagnostics version then)
  "Let Lean publishDiagnostics reach Flymake when Lean reports stale VERSION."
  (let* ((buf (lean--diagnostics-buffer-for-uri uri))
         (lean-buffer-p (and (buffer-live-p buf)
                             (with-current-buffer buf
                               (derived-mode-p 'lean-mode)))))
    (if lean-buffer-p
        (with-current-buffer buf
          (let* ((docver (and (boundp 'eglot--docver) eglot--docver))
                 (compat (and version docver (/= version docver))))
            (lean--publish-flymake-diagnostics diagnostics
                                               (or docver version))
            (when (fboundp 'lean-dev-log)
              (lean-dev-log
               "publishDiagnostics: file=%s diagnostics=%d version=%S docver=%S applied-version-compat=%S"
               (file-name-nondirectory (or buffer-file-name (buffer-name)))
               (lean--diagnostics-count diagnostics)
               version docver compat))
            nil))
      (funcall fn server uri diagnostics version then))))

;; ── fileProgress notification ─────────────────────────────────────────────────
;; Lean sends: {"textDocument": {"uri": "..."}, "processing": [{...}]}

(defun lean--file-progress-list (processing)
  "Return PROCESSING as a list."
  (cond
   ((vectorp processing) (append processing nil))
   ((listp processing) processing)
   (t nil)))

(defun lean--file-progress-make-signature (items)
  "Return a compact signature for file-progress ITEMS."
  (mapcar
   (lambda (item)
     (let* ((range (plist-get item :range))
            (start (plist-get range :start)))
       (list (plist-get item :kind)
             (plist-get start :line)
             (plist-get start :character))))
   items))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql \$/lean/fileProgress))
   &key textDocument processing &allow-other-keys)
  "Handle $/lean/fileProgress from Lean LSP server."
  (when-let* ((uri  (plist-get textDocument :uri))
              (path (ignore-errors (eglot-uri-to-path uri)))
              (buf  (find-buffer-visiting path)))
    (with-current-buffer buf
      (let* ((items (lean--file-progress-list processing))
             (signature (lean--file-progress-make-signature items)))
        (setq-local lean--file-progress items)
        (unless (equal signature lean--file-progress-signature)
          (setq-local lean--file-progress-signature signature)
          (if lean-progress-fringe-enabled
              (lean--update-fringe-overlays lean--file-progress)
            (when lean--fringe-overlays
              (lean--clear-fringe-overlays)))
          (when (fboundp 'lean-dev-log)
            (lean-dev-log "fileProgress: file=%s items=%d"
                          (file-name-nondirectory path)
                          (length lean--file-progress)))
          (when (fboundp 'lean-progress-mode-line-refresh)
            (lean-progress-mode-line-refresh))
          (when (fboundp 'lean-schedule-sideline-refresh)
            (lean-schedule-sideline-refresh)))))))

;; ── Fringe overlays ───────────────────────────────────────────────────────────

(defface lean-fringe-processing-face
  '((t :foreground "chocolate"))
  "Fringe face for Lean files being elaborated."
  :group 'lean)

(defface lean-fringe-error-face
  '((t :foreground "red"))
  "Fringe face for Lean files with errors."
  :group 'lean)

(defface lean-fringe-warning-face
  '((t :foreground "orange"))
  "Fringe face for Lean warnings."
  :group 'lean)

(defface lean-fringe-note-face
  '((t :foreground "cyan"))
  "Fringe face for Lean notes."
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

(defun lean--severity-rank (kind)
  "Return numeric severity rank for KIND."
  (pcase kind
    ('error 0)
    ('warning 1)
    ('note 2)
    (_ 3)))

(defun lean--diagnostic-fringe-kind-visible-p (kind)
  "Return non-nil when KIND should get a fringe marker."
  (<= (lean--severity-rank kind)
      (lean--severity-rank lean-diagnostic-fringe-minimum-severity)))

(defun lean--diagnostic-fringe-face (kind)
  "Return fringe face for diagnostic KIND."
  (pcase kind
    ('error 'lean-fringe-error-face)
    ('warning 'lean-fringe-warning-face)
    (_ 'lean-fringe-note-face)))

(defun lean--diagnostic-fringe-bitmap (_kind)
  "Return fringe bitmap for diagnostic KIND."
  'lean-fringe-blocked-bitmap)

(defun lean--clear-diagnostic-fringe-overlays ()
  "Remove all Lean diagnostic fringe overlays from current buffer."
  (dolist (ov lean--diagnostic-fringe-overlays)
    (delete-overlay ov))
  (setq lean--diagnostic-fringe-overlays nil))

(defun lean--diagnostic-fringe-line-entries (diagnostics)
  "Return one highest-severity diagnostic per line from DIAGNOSTICS."
  (let (entries)
    (dolist (diag diagnostics)
      (when-let* ((beg (ignore-errors (flymake-diagnostic-beg diag))))
        (let* ((line (line-number-at-pos beg t))
               (kind (lean--flymake-kind diag))
               (text (flymake-diagnostic-text diag))
               (existing (assoc line entries)))
          (when (lean--diagnostic-fringe-kind-visible-p kind)
            (unless (and existing
                         (>= (lean--severity-rank kind)
                             (lean--severity-rank
                              (plist-get (cdr existing) :kind))))
              (when existing
                (setq entries (delq existing entries)))
              (push (cons line (list :pos beg :kind kind :text text))
                    entries))))))
    entries))

(defun lean--update-diagnostic-fringe-overlays (diagnostics)
  "Update Lean diagnostic fringe markers from DIAGNOSTICS."
  (lean--clear-diagnostic-fringe-overlays)
  (dolist (entry (lean--diagnostic-fringe-line-entries diagnostics))
    (let* ((data (cdr entry))
           (pos (plist-get data :pos))
           (kind (plist-get data :kind))
           (help (plist-get data :text)))
      (save-excursion
        (goto-char pos)
        (let ((ov (make-overlay (line-beginning-position)
                                (min (point-max) (1+ (line-beginning-position)))
                                nil nil t)))
          (overlay-put ov 'before-string
                       (propertize " " 'display
                                   `(right-fringe
                                     ,(lean--diagnostic-fringe-bitmap kind)
                                     ,(lean--diagnostic-fringe-face kind))
                                   'help-echo help))
          (overlay-put ov 'help-echo help)
          (overlay-put ov 'lean-diagnostic-fringe t)
          (overlay-put ov 'priority 2000)
          (push ov lean--diagnostic-fringe-overlays))))))

(defun lean--progress-help (item)
  "Return tooltip text for Lean file-progress ITEM."
  (let* ((kind (plist-get item :kind))
         (range (plist-get item :range))
         (start (plist-get range :start))
         (line (plist-get start :line))
         (status (if (eq kind 1) "processing" "blocked/error")))
    (format "Lean %s at line %s" status (and line (1+ line)))))

(defun lean--update-fringe-overlays (items)
  "Update fringe overlays from progress ITEMS list."
  (dolist (ov lean--fringe-overlays)
    (delete-overlay ov))
  (setq lean--fringe-overlays nil)
  (dolist (item items)
    (let* ((range      (plist-get item :range))
           (kind       (plist-get item :kind))
           (start      (plist-get range :start))
           (start-line (plist-get start :line))
           (processing (eq kind 1))
           (face       (if processing 'lean-fringe-processing-face
                         'lean-fringe-error-face))
           (bitmap     (if processing 'lean-fringe-processing-bitmap
                         'lean-fringe-blocked-bitmap)))
      (when (and start-line (numberp start-line))
        (save-excursion
          (goto-char (point-min))
          (forward-line start-line)
          (let ((ov (make-overlay (point) (1+ (point)))))
            (overlay-put ov 'before-string
                         (propertize " " 'display
                                     `(left-fringe ,bitmap ,face)
                                     'help-echo (lean--progress-help item)))
            (overlay-put ov 'help-echo (lean--progress-help item))
            (overlay-put ov 'lean-fringe t)
            (push ov lean--fringe-overlays)))))))

(defun lean--clear-fringe-overlays ()
  "Remove all Lean fringe overlays from current buffer."
  (dolist (ov lean--fringe-overlays)
    (delete-overlay ov))
  (setq lean--fringe-overlays nil
        lean--file-progress-signature nil)
  (when (fboundp 'lean--clear-diagnostic-fringe-overlays)
    (lean--clear-diagnostic-fringe-overlays))
  (when (fboundp 'lean-clear-sideline-overlays)
    (lean-clear-sideline-overlays)))

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

(with-eval-after-load 'eglot
  (when (fboundp 'eglot--flymake-handle-push)
    (unless (advice-member-p #'lean--eglot-flymake-handle-push-a
                             'eglot--flymake-handle-push)
      (advice-add 'eglot--flymake-handle-push
                  :around #'lean--eglot-flymake-handle-push-a))))

(provide 'init-lean-eglot)
;;; init-lean-eglot.el ends here
