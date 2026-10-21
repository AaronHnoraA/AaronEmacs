;;; init-performance.el --- Runtime performance watch board -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'init-funcs)
(require 'seq)
(require 'subr-x)

(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function profiler-report "profiler" ())
(declare-function my/open-resolve-file-name "init-open"
                  (file &optional directory))
(declare-function my/open-visit-file "init-open" (file &optional wildcards))
(declare-function aaron-ui-performance-view "aaron-ui-performance"
                  (state selected owner))

(defgroup my/performance nil
  "Runtime performance and power diagnostics."
  :group 'convenience)

(config-defvar my/performance-refresh-interval nil
  "Seconds between automatic performance board refreshes."
  :type 'number
  :group 'my/performance)

(config-defvar my/performance-record-directory nil
  "Directory used for saved performance records."
  :type 'directory
  :group 'my/performance)

(config-defvar my/performance-watch-hooks nil
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

(defvar-local my/performance--view-state nil
  "ETAF ref holding the current Performance Watch snapshot.")

(defvar-local my/performance--tab-state nil
  "ETAF ref holding the selected Performance Watch section.")

(defvar-local my/performance--recording nil
  "Whether each refresh should append a performance record.")

(defvar my/performance--capture-buffer nil
  "Transient buffer used only to capture one ETAF report section.")

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

(define-derived-mode my/performance-watch-mode aaron-ui-board-mode "PerfWatch"
  "Major mode for the runtime performance watch board."
  (setq-local cursor-type nil)
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/performance-watch-mode 'emacs))

(defvar-keymap my/performance-watch-keys-mode-map
  :doc "Shortcuts layered over the ETAF Performance Watch page."
  "g" #'my/performance-watch-refresh
  "G" #'my/performance-garbage-collect
  "y" #'my/performance-copy-page
  "a" #'my/performance-watch-toggle-auto-refresh
  "s" #'my/performance-save-record
  "R" #'my/performance-toggle-recording
  "o" #'my/performance-open-record-directory
  "p" #'my/performance-profiler-start-cpu
  "P" #'profiler-report
  "1" (lambda () (interactive) (my/performance-watch-select-tab 'overview))
  "2" (lambda () (interactive) (my/performance-watch-select-tab 'processes))
  "3" (lambda () (interactive) (my/performance-watch-select-tab 'buffers))
  "4" (lambda () (interactive) (my/performance-watch-select-tab 'hooks))
  "q" #'my/performance-watch-quit)

(define-minor-mode my/performance-watch-keys-mode
  "Keyboard shortcuts for the ETAF Performance Watch page."
  :lighter " PerfWatch"
  :keymap my/performance-watch-keys-mode-map
  (when (and my/performance-watch-keys-mode (fboundp 'evil-local-mode))
    (evil-local-mode -1)))

(defun my/performance-watch-select-tab (tab)
  "Select Performance Watch TAB."
  (interactive
   (list (intern (completing-read "Performance section: "
                                  '("overview" "processes" "buffers" "hooks")
                                  nil t))))
  (unless my/performance--tab-state
    (user-error "Not in a Performance Watch page"))
  (etaf-set-value my/performance--tab-state tab))

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
  (aaron-ui-board-insert-section title))

(defun my/performance--level-face (ratio)
  "Return a face for normalized RATIO."
  (aaron-ui-board--level-face ratio))

(defun my/performance--bar (ratio &optional width)
  "Return a text bar for normalized RATIO."
  (aaron-ui-board-bar ratio width))

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
        (visible 0))
    (dolist (buffer (my/performance--org-buffers))
      (setq buffers (1+ buffers))
      (when (my/performance--buffer-visible-p buffer)
        (setq visible (1+ visible))))
    (list :buffers buffers
          :visible visible)))

(defun my/performance--math-preview-sample ()
  "Return aggregate math-preview (RaTeX) runtime metrics.

Replaces telemetry for an Org LaTeX preview engine that no longer exists:
the old fields probed `my/org-latex--*' variables that nothing defines, so
they reported zero unconditionally."
  (let ((buffers 0)
        (cached 0)
        (inflight 0))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (bound-and-true-p ratex-mode)
            (setq buffers (1+ buffers))
            (setq cached
                  (+ cached (my/performance--hash-count
                             (and (boundp 'ratex--render-cache)
                                  ratex--render-cache))))
            (setq inflight
                  (+ inflight (my/performance--hash-count
                               (and (boundp 'ratex--inflight-requests)
                                    ratex--inflight-requests))))))))
    (list :buffers buffers
          :cached cached
          :inflight inflight
          :backend-live (and (fboundp 'ratex-backend-live-p)
                             (ratex-backend-live-p)
                             t)
          :pending (my/performance--hash-count
                    (and (boundp 'ratex--pending) ratex--pending)))))

(defun my/performance--sample ()
  "Return a structured performance sample."
  (let* ((process (my/performance--self-process-sample))
         (org (my/performance--org-summary-sample))
         (math (my/performance--math-preview-sample))
         (descendants (my/performance--descendant-tree-snapshot)))
    (list :timestamp (format-time-string "%Y-%m-%d %H:%M:%S")
          :unix-time (float-time)
          :pid (emacs-pid)
          :cpu (or (plist-get process :cpu) 0.0)
          :mem (or (plist-get process :mem) 0.0)
          :rss-kb (plist-get process :rss-kb)
          :vsz-kb (plist-get process :vsz-kb)
          :children-rss-kb (plist-get descendants :rss-kb)
          :children-count (length (plist-get descendants :tree))
          :elapsed (plist-get process :etime)
          :buffers (length (buffer-list))
          :processes (length (process-list))
          :timers (length timer-list)
          :idle-timers (length timer-idle-list)
          :gcs gcs-done
          :gc-elapsed gc-elapsed
          :org-buffers (plist-get org :buffers)
          :org-visible (plist-get org :visible)
          :math-preview-buffers (plist-get math :buffers)
          :math-preview-cached (plist-get math :cached)
          :math-preview-inflight (plist-get math :inflight)
          :math-preview-pending (plist-get math :pending)
          :math-preview-backend-live (plist-get math :backend-live))))

(defun my/performance-record-file (&optional time)
  "Return the performance record file for TIME or now."
  (expand-file-name
   (format "performance-%s.tsv"
           (format-time-string "%Y%m%d" time))
   my/performance-record-directory))

(defconst my/performance--record-fields
  '(:timestamp :pid :cpu :mem :rss-kb :vsz-kb :buffers :processes
    :timers :idle-timers :gcs :gc-elapsed :org-buffers :org-visible
    :math-preview-buffers :math-preview-cached :math-preview-inflight
    :math-preview-pending :math-preview-backend-live)
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
  (unless (or (derived-mode-p 'my/performance-watch-mode)
              my/performance--view-state)
    (user-error "Not in a performance watch buffer"))
  (setq-local my/performance--recording (not my/performance--recording))
  (my/performance-watch-refresh)
  (message "Performance recording %s"
           (if my/performance--recording "enabled" "disabled")))

(defun my/performance-open-record-directory ()
  "Open `my/performance-record-directory'."
  (interactive)
  (make-directory my/performance-record-directory t)
  (require 'init-open)
  (my/open-visit-file
   (my/open-resolve-file-name my/performance-record-directory)))

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

(defun my/performance--process-plists (ps-lines)
  "Parse `ps' output PS-LINES (header + rows) into a list of process plists."
  (delq nil (mapcar #'my/performance--parse-ps-process-line (cdr ps-lines))))

(defun my/performance--descendant-tree (pid processes)
  "Return (DEPTH . PROC) pairs for PID's descendants among PROCESSES.
PROCESSES is a list of plists as returned by `my/performance--process-plists'.
Order is depth-first, parent before children, matching indented display."
  (let ((by-ppid (make-hash-table :test 'eql)))
    (dolist (proc processes)
      (push proc (gethash (plist-get proc :ppid) by-ppid)))
    (let (result)
      (cl-labels ((walk (parent-pid depth)
                    (dolist (child (nreverse (copy-sequence (gethash parent-pid by-ppid))))
                      (push (cons depth child) result)
                      (walk (plist-get child :pid) (1+ depth)))))
        (walk pid 0))
      (nreverse result))))

(defun my/performance--descendant-tree-snapshot ()
  "Return (:tree TREE :rss-kb TOTAL) for the current Emacs process's descendants.
TREE is as returned by `my/performance--descendant-tree'; TOTAL is the sum of
their RSS in KB.  Returns nil when `ps' is unavailable."
  (when-let* ((all-lines (my/performance--ps-lines
                          "-axo" "pid,ppid,%cpu,%mem,rss,vsz,etime,command"))
              (processes (my/performance--process-plists all-lines)))
    (let ((tree (my/performance--descendant-tree (emacs-pid) processes)))
      (list :tree tree
            :rss-kb (apply #'+ (mapcar (lambda (entry)
                                         (or (plist-get (cdr entry) :rss-kb) 0))
                                       tree))))))

(defun my/performance--insert-toolchain-memory ()
  "Insert a recursive RSS rollup of the whole Emacs descendant process tree.
Unlike the raw `ps' block above (direct children only), this walks the full
tree — e.g. Noema's web-host node process and anything *it* spawns, like
a Copilot LSP instance or a Jupyter kernel — so the real toolchain memory
footprint (Lean workers, language servers, node backends) is visible in one
place instead of requiring a manual audit."
  (my/performance--section "Toolchain Memory")
  (let* ((snapshot (my/performance--descendant-tree-snapshot))
         (tree (plist-get snapshot :tree))
         (children-rss-kb (or (plist-get snapshot :rss-kb) 0))
         (self (my/performance--self-process-sample))
         (self-rss-kb (or (plist-get self :rss-kb) 0)))
    (if (null snapshot)
        (insert "ps unavailable\n")
      (insert (format "%-8s %10s  %s\n" "PID" "RSS(MB)" "Process"))
      (insert (format "%-8d %10.1f  Emacs (self)\n" (emacs-pid) (/ self-rss-kb 1024.0)))
      (if (null tree)
          (insert "  (no descendant processes)\n")
        (dolist (entry tree)
          (let* ((depth (car entry))
                 (proc (cdr entry)))
            (insert (format "%-8d %10.1f  %s%s\n"
                            (plist-get proc :pid)
                            (/ (or (plist-get proc :rss-kb) 0) 1024.0)
                            (make-string (* 2 (1+ depth)) ?\s)
                            (plist-get proc :command))))))
      (insert (format "\n%-28s %d\n" "Descendant processes" (length tree)))
      (insert (format "%-28s %.1f MB\n" "Descendant RSS total" (/ children-rss-kb 1024.0)))
      (insert (format "%-28s %.1f MB\n" "Emacs + descendants"
                      (/ (+ self-rss-kb children-rss-kb) 1024.0)))))
  (insert "\n"))

(defun my/performance--insert-emacs-summary ()
  "Insert Emacs runtime metrics."
  (my/performance--section "Emacs Runtime")
  (insert (format "%-24s %s\n" "Version" emacs-version))
  (insert (format "%-24s %s\n" "Uptime" (emacs-uptime)))
  (insert (format "%-24s %d\n" "Buffers"
                  (length (delq my/performance--capture-buffer
                                (buffer-list)))))
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
  (when (fboundp 'memory-use-counts)
    (insert (format "%-24s %S\n" "memory-use-counts" (memory-use-counts))))
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
              (and (boundp 'jit-lock-functions) jit-lock-functions)))))
   (my/performance--org-buffers)))

(defun my/performance--insert-org-buffers ()
  "Insert Org-specific runtime state."
  (my/performance--section "Org Buffers")
  (insert (format "%-28s %8s %-7s %-6s %-5s %-5s %-5s\n"
                  "Buffer" "Size" "Visible" "Mod" "Post" "AChg" "JIT"))
  (dolist (row (my/performance-org-buffer-snapshot))
    (insert
     (format "%-28s %8d %-7s %-6s %-5d %-5d %-5d\n"
             (truncate-string-to-width (plist-get row :buffer) 28 nil nil t)
             (plist-get row :size)
             (if (plist-get row :visible) "yes" "no")
             (if (plist-get row :modified) "yes" "no")
             (plist-get row :post-command-count)
             (plist-get row :after-change-count)
             (plist-get row :jit-lock-count))))
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
    (sort (delq my/performance--capture-buffer
                (copy-sequence (buffer-list)))
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
                    (and (or (derived-mode-p 'my/performance-watch-mode)
                             my/performance--view-state)
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
  (my/performance--insert-toolchain-memory)
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
  (unless (or (derived-mode-p 'my/performance-watch-mode)
              my/performance--view-state)
    (user-error "Not in a performance watch buffer"))
  (let* ((inhibit-read-only t)
         (selected-buffer (if (buffer-live-p my/performance--inspected-buffer)
                              my/performance--inspected-buffer
                            (current-buffer)))
         (sample (my/performance--sample)))
    (setq-local my/performance--last-sample sample)
    (when my/performance--recording
      (my/performance-save-record sample))
    (if my/performance--view-state
        (etaf-set-value
         my/performance--view-state
         (my/performance--ui-snapshot sample selected-buffer))
      (erase-buffer)
      (my/performance--insert-report sample selected-buffer)
      (goto-char (point-min)))))

(defun my/performance--section-body (render)
  "Capture the body of a legacy section produced by RENDER."
  (with-temp-buffer
    (let ((my/performance--capture-buffer (current-buffer)))
      (funcall render)
      (goto-char (point-min))
      (forward-line 1)
      (string-trim-right
       (buffer-substring-no-properties (point) (point-max))))))

(defun my/performance--ui-snapshot (sample inspected-buffer)
  "Create the ETAF page snapshot from SAMPLE and INSPECTED-BUFFER."
  (let ((definitions
         (list
          (list 'overview 'overview "Overview"
                (lambda () (my/performance--insert-overview sample)))
          (list 'os 'overview "OS Process Metrics"
                #'my/performance--insert-os-processes)
          (list 'toolchain 'overview "Toolchain Memory"
                #'my/performance--insert-toolchain-memory)
          (list 'runtime 'overview "Emacs Runtime"
                #'my/performance--insert-emacs-summary)
          (list 'emacs-processes 'processes "Emacs Process List"
                #'my/performance--insert-emacs-processes)
          (list 'org 'buffers "Org Buffers"
                #'my/performance--insert-org-buffers)
          (list 'largest 'buffers "Largest Buffers"
                #'my/performance--insert-buffer-hotspots)
          (list 'hooks 'hooks "Hook Table"
                (lambda () (my/performance--insert-hook-table
                            inspected-buffer)))
          (list 'timers 'hooks "Timers"
                (lambda () (my/performance--insert-timer-summary
                            "Timers" timer-list)))
          (list 'idle-timers 'hooks "Idle Timers"
                (lambda () (my/performance--insert-timer-summary
                            "Idle Timers" timer-idle-list))))))
    (list :summary
          (format "Inspecting: %s   Updated: %s   ·   auto %s   rec %s"
                  (if (buffer-live-p inspected-buffer)
                      (buffer-name inspected-buffer) "-")
                  (plist-get sample :timestamp)
                  (if (timerp my/performance--auto-refresh-timer) "on" "off")
                  (if my/performance--recording "on" "off"))
          :auto (timerp my/performance--auto-refresh-timer)
          :recording my/performance--recording
          :sections
          (mapcar
           (lambda (definition)
             (list :key (nth 0 definition)
                   :tab (nth 1 definition)
                   :title (nth 2 definition)
                   :body (my/performance--section-body
                          (nth 3 definition))))
           definitions))))

(defun my/performance--insert-header (sample inspected-buffer)
  "Insert board usage and metadata for SAMPLE inspecting INSPECTED-BUFFER."
  (aaron-ui-board-insert-page-header
   "Performance Watch"
   :icon 'chart
   :subtitle (format "Inspecting: %s   Updated: %s"
                     (if (buffer-live-p inspected-buffer)
                         (buffer-name inspected-buffer)
                       "-")
                     (plist-get sample :timestamp))
   :stats (list
           (cons (format "auto %s" (if (timerp my/performance--auto-refresh-timer) "on" "off"))
                 (if (timerp my/performance--auto-refresh-timer) 'success 'muted))
           (cons (format "rec %s" (if my/performance--recording "on" "off"))
                 (if my/performance--recording 'warning 'muted))))
  (aaron-ui-board-insert-key-hints
   "Keys: g refresh  G collect  y copy page  a auto-refresh  s save record  R recording  o record dir  p profiler  q quit")
  (insert "\n"))

(defun my/performance--insert-metric (label value ratio &optional suffix)
  "Insert one visual metric LABEL VALUE with normalized RATIO and SUFFIX."
  (aaron-ui-board-insert-metric label value ratio suffix))

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
   "Math preview"
   (format "%s buf, backend %s"
           (plist-get sample :math-preview-buffers)
           (if (plist-get sample :math-preview-backend-live) "up" "down"))
   (/ (float (plist-get sample :math-preview-buffers)) 12.0))
  (my/performance--insert-metric
   "Pending renders"
   (format "%s in flight, %s queued"
           (plist-get sample :math-preview-inflight)
           (plist-get sample :math-preview-pending))
   (/ (float (+ (plist-get sample :math-preview-inflight)
                (plist-get sample :math-preview-pending)))
      24.0))
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
           (max 1 (or my/performance-refresh-interval 5))
           (max 1 (or my/performance-refresh-interval 5))
           #'my/performance--auto-refresh-buffer
           (current-buffer)))))

(defun my/performance-watch-toggle-auto-refresh ()
  "Toggle automatic refresh for the current performance watch buffer."
  (interactive)
  (unless (or (derived-mode-p 'my/performance-watch-mode)
              my/performance--view-state)
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

(defun my/performance-garbage-collect ()
  "Run a manual garbage collection and report process memory before and after."
  (interactive)
  (let* ((before (my/performance--self-process-sample))
         (start (float-time))
         (stats (garbage-collect))
         (elapsed (- (float-time) start))
         (after (my/performance--self-process-sample)))
    (when (or (derived-mode-p 'my/performance-watch-mode)
              my/performance--view-state)
      (my/performance-watch-refresh))
    (message "GC %.3fs; RSS %s -> %s; %d statistic rows"
             elapsed
             (my/performance--format-kb (plist-get before :rss-kb))
             (my/performance--format-kb (plist-get after :rss-kb))
             (length stats))))

(defun my/performance-copy-page ()
  "Copy the full current performance page to the kill ring."
  (interactive)
  (let ((text
         (cond
          (my/performance--view-state
           (let ((sample my/performance--last-sample)
                 (inspected my/performance--inspected-buffer)
                 (auto my/performance--auto-refresh-timer)
                 (recording my/performance--recording))
             (with-temp-buffer
               (setq-local my/performance--auto-refresh-timer auto
                           my/performance--recording recording)
               (my/performance--insert-report sample inspected)
               (buffer-substring-no-properties (point-min) (point-max)))))
          ((derived-mode-p 'my/performance-watch-mode)
           (buffer-substring-no-properties (point-min) (point-max)))
          (t (my/performance-report-string (current-buffer))))))
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
    (when (and (fboundp 'etaf-runtime-for-buffer)
               (etaf-runtime-for-buffer buffer))
      (etaf-refit))
    (set-window-dedicated-p (selected-window) t)
    (selected-window)))

(defun my/performance-watch ()
  "Open a runtime performance and power diagnostics board."
  (interactive)
  (require 'aaron-ui-performance)
  (let ((inspected-buffer (current-buffer))
        (buffer (get-buffer-create my/performance-buffer-name)))
    (with-current-buffer buffer
      (unless (etaf-runtime-for-buffer buffer)
        (let ((view-state (etaf-ref nil))
              (tab-state (etaf-ref 'overview)))
          (etaf-mount
           buffer (aaron-ui-performance-view view-state tab-state buffer))
          (setq-local my/performance--view-state view-state
                      my/performance--tab-state tab-state)
          (my/performance-watch-keys-mode 1)
          (add-hook 'kill-buffer-hook #'my/performance-watch-cleanup nil t)))
      (setq-local my/performance--inspected-buffer inspected-buffer)
      (my/performance-watch-refresh))
    (my/performance-watch-display-buffer buffer)))

(defun my/performance--theme-changed (&rest _)
  "Repaint the ETAF Performance Watch after a theme change."
  (when-let* ((buffer (get-buffer my/performance-buffer-name)))
    (with-current-buffer buffer
      (when my/performance--view-state
        (etaf-update-value
         my/performance--view-state
         (lambda (snapshot)
           (plist-put (copy-sequence snapshot) :theme-revision
                      (1+ (or (plist-get snapshot :theme-revision)
                              0)))))))))

(add-hook 'after-load-theme-hook #'my/performance--theme-changed)

(my/leader!
  "h p" '(:def my/performance-watch :which-key "performance watch"))

(provide 'init-performance)
;;; init-performance.el ends here
