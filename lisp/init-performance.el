;;; init-performance.el --- Runtime performance watch board -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'seq)
(require 'subr-x)

(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defgroup my/performance nil
  "Runtime performance and power diagnostics."
  :group 'convenience)

(defcustom my/performance-refresh-interval 2.0
  "Seconds between automatic performance board refreshes."
  :type 'number
  :group 'my/performance)

(defcustom my/performance-record-directory
  (expand-file-name "var/performance/" user-emacs-directory)
  "Directory used for saved performance records."
  :type 'directory
  :group 'my/performance)

(defcustom my/performance-watch-hooks
  '(post-command-hook
    pre-command-hook
    after-change-functions
    before-change-functions
    before-save-hook
    after-save-hook
    window-scroll-functions
    window-size-change-functions
    jit-lock-functions
    org-mode-hook
    org-cycle-hook
    org-babel-after-execute-hook
    kill-buffer-hook
    change-major-mode-hook)
  "Hooks shown by `my/performance-watch'."
  :type '(repeat symbol)
  :group 'my/performance)

(defconst my/performance-buffer-name "*Performance Watch*"
  "Buffer name used by `my/performance-watch'.")

(defvar my/performance--frame nil
  "Frame used by `my/performance-watch', if it is still live.")

(defvar-local my/performance--auto-refresh-timer nil
  "Buffer-local automatic refresh timer for the performance board.")

(defvar-local my/performance--inspected-buffer nil
  "Buffer whose local hooks are inspected by the performance board.")

(defvar-local my/performance--last-sample nil
  "Most recent structured performance sample for this board.")

(defvar-local my/performance--recording nil
  "Whether each refresh should append a performance record.")

(defface my/performance-title-face
  '((t :inherit font-lock-function-name-face :weight bold :height 1.25))
  "Face for performance board title."
  :group 'my/performance)

(defface my/performance-section-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for performance board section titles."
  :group 'my/performance)

(defface my/performance-dim-face
  '((t :inherit shadow))
  "Face for secondary performance board text."
  :group 'my/performance)

(defface my/performance-good-face
  '((t :inherit success))
  "Face for healthy performance values."
  :group 'my/performance)

(defface my/performance-warn-face
  '((t :inherit warning))
  "Face for elevated performance values."
  :group 'my/performance)

(defface my/performance-bad-face
  '((t :inherit error :weight bold))
  "Face for high performance values."
  :group 'my/performance)

(define-derived-mode my/performance-watch-mode special-mode "PerfWatch"
  "Major mode for the runtime performance watch board."
  (setq-local cursor-type nil)
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/performance-watch-mode 'emacs))

(defun my/performance--hook-count (value)
  "Return a display count for hook VALUE."
  (cond
   ((null value) 0)
   ((and (listp value) (not (functionp value))) (length value))
   (t 1)))

(defun my/performance--function-name (fn)
  "Return a compact display name for function FN."
  (cond
   ((symbolp fn) (symbol-name fn))
   ((byte-code-function-p fn) "<byte-code>")
   ((functionp fn) "<lambda>")
   (t (format "%S" fn))))

(defun my/performance--timer-function (timer)
  "Return TIMER's function value."
  (when (and (vectorp timer) (> (length timer) 5))
    (aref timer 5)))

(defun my/performance--timer-summary (timers)
  "Return a sorted summary alist for TIMERS grouped by function."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (timer timers)
      (let* ((fn (my/performance--timer-function timer))
             (name (my/performance--function-name fn)))
        (puthash name (1+ (gethash name table 0)) table)))
    (let (rows)
      (maphash (lambda (name count)
                 (push (cons name count) rows))
               table)
      (sort rows (lambda (left right)
                   (> (cdr left) (cdr right)))))))

(defun my/performance--section (title)
  "Insert a styled section TITLE."
  (insert (propertize title 'face 'my/performance-section-face) "\n")
  (insert (propertize (make-string (length title) ?-) 'face 'my/performance-dim-face)
          "\n"))

(defun my/performance--level-face (ratio)
  "Return a face for normalized RATIO."
  (cond
   ((>= ratio 0.85) 'my/performance-bad-face)
   ((>= ratio 0.55) 'my/performance-warn-face)
   (t 'my/performance-good-face)))

(defun my/performance--bar (ratio &optional width)
  "Return a text bar for normalized RATIO."
  (let* ((width (or width 24))
         (ratio (max 0.0 (min 1.0 (or ratio 0.0))))
         (filled (round (* ratio width)))
         (empty (- width filled))
         (face (my/performance--level-face ratio)))
    (concat "["
            (propertize (make-string filled ?#) 'face face)
            (propertize (make-string empty ?-) 'face 'my/performance-dim-face)
            "]")))

(defun my/performance--format-kb (kb)
  "Return KB as a human readable size."
  (if (numberp kb)
      (file-size-human-readable (* kb 1024))
    "-"))

(defun my/performance--insert-timer-summary (title timers)
  "Insert TITLE and a grouped summary for TIMERS."
  (my/performance--section title)
  (insert (format "%-5s %s\n" "Count" "Function"))
  (dolist (row (seq-take (my/performance--timer-summary timers) 24))
    (insert (format "%-5d %s\n" (cdr row) (car row))))
  (insert "\n"))

(defun my/performance--ps-lines (&rest args)
  "Return output lines from `ps' called with ARGS."
  (when-let* ((ps (executable-find "ps")))
    (with-temp-buffer
      (when (zerop (apply #'call-process ps nil (current-buffer) nil args))
        (split-string (string-trim (buffer-string)) "\n" t)))))

(defun my/performance--parse-ps-process-line (line)
  "Parse one ps output LINE into a plist."
  (when (stringp line)
    (let ((fields (split-string (string-trim line) "[[:space:]]+" t)))
      (when (>= (length fields) 7)
        (list :pid (string-to-number (nth 0 fields))
              :ppid (string-to-number (nth 1 fields))
              :cpu (string-to-number (nth 2 fields))
              :mem (string-to-number (nth 3 fields))
              :rss-kb (string-to-number (nth 4 fields))
              :vsz-kb (string-to-number (nth 5 fields))
              :etime (nth 6 fields)
              :command (string-join (nthcdr 7 fields) " "))))))

(defun my/performance--self-process-sample ()
  "Return OS process metrics for this Emacs process."
  (when-let* ((lines (my/performance--ps-lines
                      "-p" (number-to-string (emacs-pid))
                      "-o" "pid,ppid,%cpu,%mem,rss,vsz,etime,command"))
              (line (cadr lines)))
    (my/performance--parse-ps-process-line line)))

(defun my/performance--org-summary-sample ()
  "Return aggregate Org runtime metrics."
  (let ((buffers 0)
        (visible 0)
        (latex-enabled 0)
        (latex-running 0)
        (latex-queued 0)
        (latex-overlays 0)
        (latex-pending 0))
    (dolist (buffer (my/performance--org-buffers))
      (setq buffers (1+ buffers))
      (when (my/performance--buffer-visible-p buffer)
        (setq visible (1+ visible)))
      (with-current-buffer buffer
        (when (bound-and-true-p my/org-latex--scroll-preview-enabled)
          (setq latex-enabled (1+ latex-enabled)))
        (setq latex-running
              (+ latex-running
                 (if (boundp 'my/org-latex--render-running)
                     my/org-latex--render-running
                   0)))
        (setq latex-queued
              (+ latex-queued
                 (if (boundp 'my/org-latex--render-queue)
                     (my/performance--list-length my/org-latex--render-queue)
                   0)))
        (setq latex-overlays
              (+ latex-overlays
                 (if (boundp 'my/org-latex--overlay-table)
                     (my/performance--hash-count my/org-latex--overlay-table)
                   0)))
        (setq latex-pending
              (+ latex-pending
                 (if (boundp 'my/org-latex--pending-renders)
                     (my/performance--hash-count my/org-latex--pending-renders)
                   0)))))
    (list :buffers buffers
          :visible visible
          :latex-enabled latex-enabled
          :latex-running latex-running
          :latex-queued latex-queued
          :latex-overlays latex-overlays
          :latex-pending latex-pending)))

(defun my/performance--sample ()
  "Return a structured performance sample."
  (let ((process (my/performance--self-process-sample))
        (org (my/performance--org-summary-sample)))
    (list :timestamp (format-time-string "%Y-%m-%d %H:%M:%S")
          :unix-time (float-time)
          :pid (emacs-pid)
          :cpu (or (plist-get process :cpu) 0.0)
          :mem (or (plist-get process :mem) 0.0)
          :rss-kb (plist-get process :rss-kb)
          :vsz-kb (plist-get process :vsz-kb)
          :elapsed (plist-get process :etime)
          :buffers (length (buffer-list))
          :processes (length (process-list))
          :timers (length timer-list)
          :idle-timers (length timer-idle-list)
          :gcs gcs-done
          :gc-elapsed gc-elapsed
          :org-buffers (plist-get org :buffers)
          :org-visible (plist-get org :visible)
          :org-latex-enabled (plist-get org :latex-enabled)
          :org-latex-running (plist-get org :latex-running)
          :org-latex-queued (plist-get org :latex-queued)
          :org-latex-overlays (plist-get org :latex-overlays)
          :org-latex-pending (plist-get org :latex-pending))))

(defun my/performance-record-file (&optional time)
  "Return the performance record file for TIME or now."
  (expand-file-name
   (format "performance-%s.tsv"
           (format-time-string "%Y%m%d" time))
   my/performance-record-directory))

(defconst my/performance--record-fields
  '(:timestamp :pid :cpu :mem :rss-kb :vsz-kb :buffers :processes
    :timers :idle-timers :gcs :gc-elapsed :org-buffers :org-visible
    :org-latex-enabled :org-latex-running :org-latex-queued
    :org-latex-overlays :org-latex-pending)
  "Fields saved to performance record files.")

(defun my/performance--record-header ()
  "Return the TSV record header."
  (mapconcat (lambda (field)
               (string-remove-prefix ":" (symbol-name field)))
             my/performance--record-fields
             "\t"))

(defun my/performance--record-line (sample)
  "Return a TSV line for SAMPLE."
  (mapconcat
   (lambda (field)
     (let ((value (plist-get sample field)))
       (format "%s" (or value ""))))
   my/performance--record-fields
   "\t"))

(defun my/performance-save-record (&optional sample)
  "Append SAMPLE or the current sample to `my/performance-record-file'."
  (interactive)
  (let* ((sample (or sample
                     my/performance--last-sample
                     (setq my/performance--last-sample
                           (my/performance--sample))))
         (file (my/performance-record-file))
         (new-file (not (file-exists-p file))))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (when new-file
        (insert (my/performance--record-header) "\n"))
      (insert (my/performance--record-line sample) "\n")
      (append-to-file (point-min) (point-max) file))
    (when (called-interactively-p 'interactive)
      (message "Performance record saved to %s" file))
    file))

(defun my/performance-toggle-recording ()
  "Toggle appending a record on every performance refresh."
  (interactive)
  (unless (derived-mode-p 'my/performance-watch-mode)
    (user-error "Not in a performance watch buffer"))
  (setq-local my/performance--recording (not my/performance--recording))
  (my/performance-watch-refresh)
  (message "Performance recording %s"
           (if my/performance--recording "enabled" "disabled")))

(defun my/performance-open-record-directory ()
  "Open `my/performance-record-directory'."
  (interactive)
  (make-directory my/performance-record-directory t)
  (find-file my/performance-record-directory))

(defun my/performance--insert-os-processes ()
  "Insert OS-level Emacs process metrics from `ps'."
  (let* ((pid (emacs-pid))
         (pid-string (number-to-string pid))
         (self-lines
          (my/performance--ps-lines
           "-p" pid-string
           "-o" "pid,ppid,%cpu,%mem,rss,vsz,etime,command"))
         (all-lines
          (my/performance--ps-lines
           "-axo" "pid,ppid,%cpu,%mem,rss,vsz,etime,command"))
         child-lines)
    (when all-lines
      (setq child-lines
            (cl-loop for line in (cdr all-lines)
                     when (string-match
                           (format "^[[:space:]]*[0-9]+[[:space:]]+%s[[:space:]]"
                                   (regexp-quote pid-string))
                           line)
                     collect line)))
    (my/performance--section "OS Process Metrics")
    (if self-lines
        (dolist (line self-lines)
          (insert line "\n"))
      (insert "ps unavailable\n"))
    (when child-lines
      (insert "\nChild processes\n")
      (dolist (line (cons (car all-lines) child-lines))
        (insert line "\n")))
    (insert "\n")))

(defun my/performance--insert-emacs-summary ()
  "Insert Emacs runtime metrics."
  (my/performance--section "Emacs Runtime")
  (insert (format "%-24s %s\n" "Version" emacs-version))
  (insert (format "%-24s %s\n" "Uptime" (emacs-uptime)))
  (insert (format "%-24s %d\n" "Buffers" (length (buffer-list))))
  (insert (format "%-24s %d\n" "Processes" (length (process-list))))
  (insert (format "%-24s %d / %d\n" "Timers / idle timers"
                  (length timer-list)
                  (length timer-idle-list)))
  (insert (format "%-24s %d\n" "GCs" gcs-done))
  (insert (format "%-24s %.3fs\n" "GC elapsed" gc-elapsed))
  (insert (format "%-24s %s\n" "gc-cons-threshold"
                  (file-size-human-readable gc-cons-threshold)))
  (insert (format "%-24s %s\n" "read-process-output-max"
                  (file-size-human-readable read-process-output-max)))
  (when (fboundp 'memory-info)
    (insert (format "%-24s %S\n" "memory-info" (memory-info))))
  (insert "\n"))

(defun my/performance--insert-emacs-processes ()
  "Insert Emacs subprocesses."
  (my/performance--section "Emacs Process List")
  (insert (format "%-24s %-10s %-8s %-12s %s\n"
                  "Name" "Status" "PID" "Buffer" "Command"))
  (dolist (process (process-list))
    (insert
     (format "%-24s %-10s %-8s %-12s %S\n"
             (process-name process)
             (process-status process)
             (or (ignore-errors (process-id process)) "-")
             (if-let* ((buffer (process-buffer process)))
                 (buffer-name buffer)
               "-")
             (process-command process))))
  (insert "\n"))

(defun my/performance--insert-hook-table (&optional buffer)
  "Insert global and BUFFER-local hook activation counts."
  (let ((buffer (or buffer (current-buffer))))
    (my/performance--section "Hook Table")
    (insert (format "Selected buffer: %s\n\n" (buffer-name buffer)))
    (insert (format "%-34s %-8s %-8s %s\n"
                    "Hook" "Global" "Local" "Local entries"))
    (dolist (row (my/performance-hook-snapshot buffer))
      (insert (format "%-34s %-8d %-8d %s\n"
                      (plist-get row :hook)
                      (plist-get row :global-count)
                      (plist-get row :local-count)
                      (string-join (plist-get row :local-entries) ", "))))
    (insert "\n")))

(defun my/performance--buffer-visible-p (buffer)
  "Return non-nil when BUFFER is visible in any live window."
  (get-buffer-window buffer t))

(defun my/performance--list-length (value)
  "Return list length for VALUE, or 0."
  (if (listp value) (length value) 0))

(defun my/performance--hash-count (value)
  "Return hash table count for VALUE, or 0."
  (if (hash-table-p value) (hash-table-count value) 0))

(defun my/performance--hook-function-list (value)
  "Return VALUE normalized to a list of hook functions."
  (cond
   ((null value) nil)
   ((and (listp value) (not (functionp value))) value)
   (t (list value))))

(defun my/performance-hook-snapshot (&optional buffer hooks)
  "Return structured hook counts for BUFFER and HOOKS.
The return value is a list of plists with `:hook', `:global-count',
`:local-count' and `:local-entries'."
  (let ((buffer (or buffer (current-buffer)))
        (hooks (or hooks my/performance-watch-hooks)))
    (mapcar
     (lambda (hook)
       (let ((global (and (boundp hook) (default-value hook)))
             local local-entries)
         (with-current-buffer buffer
           (when (local-variable-p hook)
             (setq local (symbol-value hook)
                   local-entries
                   (mapcar #'my/performance--function-name
                           (my/performance--hook-function-list local)))))
         (list :hook hook
               :global-count (my/performance--hook-count global)
               :local-count (my/performance--hook-count local)
               :local-entries local-entries)))
     hooks)))

(defun my/performance--org-buffers ()
  "Return live Org buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'org-mode)))
   (buffer-list)))

(defun my/performance-org-buffer-snapshot ()
  "Return structured Org buffer runtime state."
  (mapcar
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((running (if (boundp 'my/org-latex--render-running)
                          my/org-latex--render-running
                        0))
             (queued (if (boundp 'my/org-latex--render-queue)
                         (my/performance--list-length
                          my/org-latex--render-queue)
                       0))
             (overlays (if (boundp 'my/org-latex--overlay-table)
                           (my/performance--hash-count my/org-latex--overlay-table)
                         0))
             (pending (if (boundp 'my/org-latex--pending-renders)
                          (my/performance--hash-count my/org-latex--pending-renders)
                        0)))
         (list :buffer (buffer-name buffer)
               :size (buffer-size)
               :visible (and (my/performance--buffer-visible-p buffer) t)
               :modified (buffer-modified-p)
               :major-mode major-mode
               :post-command-count
               (my/performance--hook-count post-command-hook)
               :after-change-count
               (my/performance--hook-count after-change-functions)
               :jit-lock-count
               (my/performance--hook-count
                (and (boundp 'jit-lock-functions) jit-lock-functions))
               :latex-enabled
               (bound-and-true-p my/org-latex--scroll-preview-enabled)
               :latex-running running
               :latex-queued queued
               :latex-overlays overlays
               :latex-pending pending))))
   (my/performance--org-buffers)))

(defun my/performance--insert-org-buffers ()
  "Insert Org-specific runtime state."
  (my/performance--section "Org Buffers")
  (insert (format "%-28s %8s %-7s %-6s %-5s %-5s %-5s %-5s %-7s %-7s %-8s\n"
                  "Buffer" "Size" "Visible" "Mod" "Post" "AChg" "JIT"
                  "Latex" "Run/Q" "Overlay" "Pending"))
  (dolist (row (my/performance-org-buffer-snapshot))
    (insert
     (format "%-28s %8d %-7s %-6s %-5d %-5d %-5d %-5s %-7s %-7d %-8d\n"
             (truncate-string-to-width (plist-get row :buffer) 28 nil nil t)
             (plist-get row :size)
             (if (plist-get row :visible) "yes" "no")
             (if (plist-get row :modified) "yes" "no")
             (plist-get row :post-command-count)
             (plist-get row :after-change-count)
             (plist-get row :jit-lock-count)
             (if (plist-get row :latex-enabled) "on" "off")
             (format "%s/%s"
                     (plist-get row :latex-running)
                     (plist-get row :latex-queued))
             (plist-get row :latex-overlays)
             (plist-get row :latex-pending))))
  (insert "\n"))

(defun my/performance-buffer-hotspot-snapshot (&optional limit)
  "Return largest buffers with hook counts, capped at LIMIT entries."
  (mapcar
   (lambda (buffer)
     (with-current-buffer buffer
       (list :buffer (buffer-name buffer)
             :size (buffer-size)
             :major-mode major-mode
             :post-command-count
             (my/performance--hook-count post-command-hook)
             :after-change-count
             (my/performance--hook-count after-change-functions)
             :jit-lock-count
             (my/performance--hook-count
              (and (boundp 'jit-lock-functions) jit-lock-functions)))))
   (seq-take
    (sort (copy-sequence (buffer-list))
          (lambda (left right)
            (> (buffer-size left) (buffer-size right))))
    (or limit 20))))

(defun my/performance--insert-buffer-hotspots ()
  "Insert largest buffers and local hook counts."
  (my/performance--section "Largest Buffers")
  (insert (format "%-32s %10s %-22s %-5s %-5s %-5s\n"
                  "Buffer" "Size" "Mode" "Post" "AChg" "JIT"))
  (dolist (row (my/performance-buffer-hotspot-snapshot 20))
    (insert
     (format "%-32s %10d %-22s %-5d %-5d %-5d\n"
             (truncate-string-to-width (plist-get row :buffer) 32 nil nil t)
             (plist-get row :size)
             (plist-get row :major-mode)
             (plist-get row :post-command-count)
             (plist-get row :after-change-count)
             (plist-get row :jit-lock-count))))
  (insert "\n"))

(defun my/performance-snapshot (&optional inspected-buffer)
  "Return a structured performance snapshot for INSPECTED-BUFFER.
This API is intended for quick debugging from `emacsclient --eval' or batch
tools; it samples once and does not start any refresh timers."
  (let ((buffer (or inspected-buffer
                    (and (derived-mode-p 'my/performance-watch-mode)
                         (buffer-live-p my/performance--inspected-buffer)
                         my/performance--inspected-buffer)
                    (current-buffer))))
    (list :sample (my/performance--sample)
          :inspected-buffer (and (buffer-live-p buffer) (buffer-name buffer))
          :hooks (and (buffer-live-p buffer)
                      (my/performance-hook-snapshot buffer))
          :org-buffers (my/performance-org-buffer-snapshot)
          :largest-buffers (my/performance-buffer-hotspot-snapshot 20)
          :timers (my/performance--timer-summary timer-list)
          :idle-timers (my/performance--timer-summary timer-idle-list))))

(defun my/performance--insert-report (sample inspected-buffer)
  "Insert a complete performance report for SAMPLE and INSPECTED-BUFFER."
  (my/performance--insert-header sample inspected-buffer)
  (my/performance--insert-overview sample)
  (my/performance--insert-os-processes)
  (my/performance--insert-emacs-summary)
  (my/performance--insert-org-buffers)
  (my/performance--insert-emacs-processes)
  (my/performance--insert-hook-table inspected-buffer)
  (my/performance--insert-buffer-hotspots)
  (my/performance--insert-timer-summary "Timers" timer-list)
  (my/performance--insert-timer-summary "Idle Timers" timer-idle-list))

(defun my/performance-report-string (&optional inspected-buffer)
  "Return a complete plain-text performance report for INSPECTED-BUFFER.
This samples once and does not mutate or display the performance board."
  (let* ((buffer (or inspected-buffer (current-buffer)))
         (sample (my/performance--sample)))
    (with-temp-buffer
      (my/performance--insert-report sample buffer)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my/performance-watch-refresh ()
  "Refresh the performance watch board."
  (interactive)
  (unless (derived-mode-p 'my/performance-watch-mode)
    (user-error "Not in a performance watch buffer"))
  (let ((inhibit-read-only t)
        (selected-buffer (if (buffer-live-p my/performance--inspected-buffer)
                             my/performance--inspected-buffer
                           (window-buffer (selected-window))))
        (sample (my/performance--sample)))
    (setq-local my/performance--last-sample sample)
    (when my/performance--recording
      (my/performance-save-record sample))
    (erase-buffer)
    (my/performance--insert-report sample selected-buffer)
    (goto-char (point-min))))

(defun my/performance--insert-header (sample inspected-buffer)
  "Insert board usage and metadata for SAMPLE inspecting INSPECTED-BUFFER."
  (insert (propertize "Performance Watch" 'face 'my/performance-title-face) "\n")
  (insert (propertize "=================" 'face 'my/performance-dim-face) "\n\n")
  (insert (propertize "Usage" 'face 'my/performance-section-face) "\n")
  (insert "  g  refresh now        y  copy full page          a  toggle auto refresh\n")
  (insert "  s  save one record    R  toggle recording        o  open record directory\n")
  (insert "  R  toggle recording   o  open record directory   p  start CPU profiler\n")
  (insert "  P  profiler report    q  close performance frame\n\n")
  (insert (format "Updated: %s    Inspecting: %s\n"
                  (plist-get sample :timestamp)
                  (if (buffer-live-p inspected-buffer)
                      (buffer-name inspected-buffer)
                    "-")))
  (insert (format "Auto refresh: %s every %.1fs    Recording: %s    Record file: %s\n\n"
                  (if (timerp my/performance--auto-refresh-timer) "on" "off")
                  my/performance-refresh-interval
                  (if my/performance--recording "on" "off")
                  (abbreviate-file-name (my/performance-record-file)))))

(defun my/performance--insert-metric (label value ratio &optional suffix)
  "Insert one visual metric LABEL VALUE with normalized RATIO and SUFFIX."
  (insert (format "%-18s %8s %-4s %s\n"
                  label
                  value
                  (or suffix "")
                  (my/performance--bar ratio 28))))

(defun my/performance--insert-overview (sample)
  "Insert visual overview for SAMPLE."
  (my/performance--section "Overview")
  (my/performance--insert-metric
   "Emacs CPU"
   (format "%.1f" (plist-get sample :cpu))
   (/ (float (plist-get sample :cpu)) 100.0)
   "%")
  (my/performance--insert-metric
   "Emacs memory"
   (format "%.1f" (plist-get sample :mem))
   (/ (float (plist-get sample :mem)) 20.0)
   "%")
  (my/performance--insert-metric
   "RSS"
   (my/performance--format-kb (plist-get sample :rss-kb))
   (/ (float (or (plist-get sample :rss-kb) 0)) (* 2 1024 1024.0)))
  (my/performance--insert-metric
   "Org buffers"
   (number-to-string (plist-get sample :org-buffers))
   (/ (float (plist-get sample :org-buffers)) 20.0))
  (my/performance--insert-metric
   "LaTeX queue"
   (format "%s/%s"
           (plist-get sample :org-latex-running)
           (plist-get sample :org-latex-queued))
   (/ (float (+ (plist-get sample :org-latex-running)
                (plist-get sample :org-latex-queued)))
      12.0))
  (my/performance--insert-metric
   "Pending renders"
   (number-to-string (plist-get sample :org-latex-pending))
   (/ (float (plist-get sample :org-latex-pending)) 24.0))
  (insert "\n"))

(defun my/performance--auto-refresh-buffer (buffer)
  "Refresh performance watch BUFFER while it is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp my/performance--auto-refresh-timer)
        (my/performance-watch-refresh)))))

(defun my/performance-watch-enable-auto-refresh ()
  "Enable automatic refresh for the current performance watch buffer."
  (unless (timerp my/performance--auto-refresh-timer)
    (setq my/performance--auto-refresh-timer
          (run-with-timer
           my/performance-refresh-interval
           my/performance-refresh-interval
           #'my/performance--auto-refresh-buffer
           (current-buffer)))))

(defun my/performance-watch-toggle-auto-refresh ()
  "Toggle automatic refresh for the current performance watch buffer."
  (interactive)
  (unless (derived-mode-p 'my/performance-watch-mode)
    (user-error "Not in a performance watch buffer"))
  (if (timerp my/performance--auto-refresh-timer)
      (progn
        (cancel-timer my/performance--auto-refresh-timer)
        (setq my/performance--auto-refresh-timer nil)
        (my/performance-watch-refresh)
        (message "Performance watch auto refresh disabled"))
    (my/performance-watch-enable-auto-refresh)
    (my/performance-watch-refresh)
    (message "Performance watch auto refresh enabled")))

(defun my/performance-watch-cleanup ()
  "Cancel automatic refresh owned by the current performance watch buffer."
  (when (timerp my/performance--auto-refresh-timer)
    (cancel-timer my/performance--auto-refresh-timer)
    (setq my/performance--auto-refresh-timer nil)))

(defun my/performance-profiler-start-cpu ()
  "Start Emacs CPU profiler."
  (interactive)
  (profiler-start 'cpu)
  (message "CPU profiler started; run `profiler-report' after reproducing load"))

(defun my/performance-copy-page ()
  "Copy the full current performance page to the kill ring."
  (interactive)
  (let ((text (if (derived-mode-p 'my/performance-watch-mode)
                  (buffer-substring-no-properties (point-min) (point-max))
                (my/performance-report-string (current-buffer)))))
    (kill-new text)
    (message "Copied performance report (%d chars)" (length text))))

(defun my/performance-watch-quit ()
  "Quit the performance watch frame and stop its automatic refresh."
  (interactive)
  (my/performance-watch-cleanup)
  (let ((frame (selected-frame)))
    (cond
     ((and (frame-live-p frame)
           (eq frame my/performance--frame)
           (> (length (frame-list)) 1))
      (setq my/performance--frame nil)
      (delete-frame frame))
     (t
      (quit-window)))))

(defun my/performance-watch-display-buffer (buffer)
  "Display performance watch BUFFER in a dedicated frame."
  (unless (and (framep my/performance--frame)
               (frame-live-p my/performance--frame))
    (setq my/performance--frame
          (make-frame `((name . "Performance Watch")
                        (minibuffer . t)
                        (width . 132)
                        (height . 44)))))
  (select-frame-set-input-focus my/performance--frame)
  (with-selected-frame my/performance--frame
    (delete-other-windows)
    (switch-to-buffer buffer)
    (set-window-dedicated-p (selected-window) t)
    (selected-window)))

(defun my/performance-watch ()
  "Open a runtime performance and power diagnostics board."
  (interactive)
  (let ((inspected-buffer (current-buffer))
        (buffer (get-buffer-create my/performance-buffer-name)))
    (with-current-buffer buffer
      (my/performance-watch-mode)
      (setq-local my/performance--inspected-buffer inspected-buffer)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (local-set-key (kbd "g") #'my/performance-watch-refresh)
        (local-set-key (kbd "y") #'my/performance-copy-page)
        (local-set-key (kbd "a") #'my/performance-watch-toggle-auto-refresh)
        (local-set-key (kbd "s") #'my/performance-save-record)
        (local-set-key (kbd "R") #'my/performance-toggle-recording)
        (local-set-key (kbd "o") #'my/performance-open-record-directory)
        (local-set-key (kbd "p") #'my/performance-profiler-start-cpu)
        (local-set-key (kbd "P") #'profiler-report)
        (local-set-key (kbd "q") #'my/performance-watch-quit))
      (add-hook 'kill-buffer-hook #'my/performance-watch-cleanup nil t)
      (my/performance-watch-refresh))
    (my/performance-watch-display-buffer buffer)))

(my/leader!
  "h p" '(:def my/performance-watch :which-key "performance watch"))

(provide 'init-performance)
;;; init-performance.el ends here
