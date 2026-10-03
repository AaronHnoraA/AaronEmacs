;;; init-activity.el --- Background activity indicator and board -*- lexical-binding: t -*-

;;; Commentary:
;; One place that answers "is Emacs busy, and with what?".
;;
;; Two surfaces:
;;
;;   - A permanent mode-line segment.  It is always present, so its position
;;     never moves; it rests when nothing runs and animates while work is in
;;     flight.  mouse-1 opens the board.
;;   - `my/activity-board', a read-only page listing everything running.
;;
;; The enumeration root is `process-list', Emacs' own registry of every
;; asynchronous subprocess.  Taking it as the root is what keeps this module
;; cheap to maintain: a new language server, agent tool or helper daemon shows
;; up on the board without this file learning anything about it.
;;
;; But `process-list' is too broad to count.  It holds three different things
;; at once, and only the first is an activity that will end:
;;
;;   task        finite work: compilation, native-comp workers, remote tasks
;;   service     long-lived daemons: language servers, epdfinfo, agent sessions
;;   connection  network/serial endpoints, TRAMP among them
;;
;; A count over all of them would sit in the dozens forever and mean nothing.
;; So the board shows all three grouped, and the indicator counts only `task'.
;;
;; Task identity is not guessed from the process name or command.  It is asked
;; from the registries that already own that work — `compilation-in-progress',
;; `remote-tasks' and `comp-async-compilations' — so a process is a task
;; exactly when some owner says it is.  Adding a new kind of background work
;; costs one line in `my/activity--owned-task-processes', and only if it should
;; be counted; appearing on the board is free.
;;
;; Work without an Emacs process comes from the native-compilation queue,
;; explicit registration (`my/with-activity'), or cached provider snapshots.
;; Providers poll asynchronously at the idle interval, independently of FPS.
;;
;; The mode-line segment runs on every redisplay of every window, so it only
;; reads two cached values and never scans anything.  The scan happens in one
;; self-rescheduling timer, which polls slowly while idle and switches to the
;; frame rate only while something runs.

;;; Code:

(require 'config)
(require 'cl-lib)
(require 'subr-x)
(require 'aaron-ui-board)

(declare-function remote-task-process "remote-task" (task))
(declare-function remote-task-name "remote-task" (task))
(declare-function remote-task-state "remote-task" (task))
(declare-function remote-task-buffer "remote-task" (task))
(declare-function remote-task-started-at "remote-task" (task))
(declare-function remote-task-cancel "remote-task" (task &optional reason))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function my/remote-per-file-subprocess-affordable-p "init-tramp"
                  (&optional path))

(defvar remote-tasks)
(defvar compilation-in-progress)
(defvar comp-async-compilations)
(defvar comp-files-queue)
(defvar spinner-types)
(defvar my/activity-indicator-mode nil)

;;; -----------------------------------------------------------------------
;;; Frames

(defconst my/activity-sprite-directory
  (expand-file-name "assets/activity/" user-emacs-directory)
  "Directory holding the activity indicator's sprite frames.
The frames are XPM, which is plain text, supported by Emacs without any
image library, and editable in place: one character per pixel over the
colour table at the top of each file.")

(defconst my/activity-frame-sets
  '((pikachu
     :rest "ᕕ( ᴗ )ᕗ"
     :frames ["ᕕ( ᐛ )ᕗ" "ᕗ( ᐛ )ᕕ" "ᕕ( ᐖ )ᕗ" "ᕗ( ᐖ )ᕕ"]
     :rest-sprite "pikachu-stand.xpm"
     :sprites ["pikachu-run-1.xpm" "pikachu-run-2.xpm"
               "pikachu-run-3.xpm" "pikachu-run-4.xpm"]))
  "Frame sets available to the activity indicator.
`:rest' is drawn while nothing runs and `:frames' are cycled while
something does.  Every string in one set must occupy the same number of
columns, or the rest of the mode line shifts on each tick.

`:rest-sprite' and `:sprites' name files in `my/activity-sprite-directory'
and are used instead on a graphical display.  They are the real picture;
the strings are the terminal fallback, and a set may provide only them.")

(config-defvar my/activity-indicator-style 'pikachu
  "Frame set of the activity indicator.
A key in `my/activity-frame-sets', or failing that in the `spinner'
package's `spinner-types', whose first frame is then used at rest."
  :type 'symbol :group 'ui)

(config-defvar my/activity-indicator-fps 8
  "Frames per second while the activity indicator is running."
  :type 'number :group 'ui)

(config-defvar my/activity-poll-interval 2.0
  "Seconds between scans for running work while the indicator rests.
This is how long a task may run before it shows up.  It is not the frame
rate: once something is running the timer re-arms at
`my/activity-indicator-fps' instead."
  :type 'number :group 'ui)

(defvar my/activity--sprite-cache (make-hash-table :test #'equal)
  "Cache of sprite file name to image object.
The mode-line segment runs on every redisplay, so images are built once
here rather than per draw.  `my/activity-reload-sprites' empties it.")

(defun my/activity--sprite (file)
  "Return the image for sprite FILE, or nil when it cannot be shown."
  (when (and file (display-graphic-p) (image-type-available-p 'xpm))
    (let ((cached (gethash file my/activity--sprite-cache 'missing)))
      (if (not (eq cached 'missing))
          cached
        (let* ((path (expand-file-name file my/activity-sprite-directory))
               (image (and (file-readable-p path)
                           (ignore-errors
                             (create-image path 'xpm nil :ascent 'center)))))
          (puthash file image my/activity--sprite-cache))))))

(defun my/activity-reload-sprites ()
  "Forget cached sprites so edited frame files are picked up."
  (interactive)
  (clrhash my/activity--sprite-cache)
  (force-mode-line-update t))

(defun my/activity--frame-set ()
  "Return the active frame set as a plist, falling back to `pikachu'."
  (or (alist-get my/activity-indicator-style my/activity-frame-sets)
      (when (require 'spinner nil t)
        (when-let* ((frames (alist-get my/activity-indicator-style spinner-types)))
          (list :rest (aref frames 0) :frames frames)))
      (alist-get 'pikachu my/activity-frame-sets)))

;;; -----------------------------------------------------------------------
;;; Activities without a process

(cl-defstruct (my/activity (:constructor my/activity--create) (:copier nil))
  "An activity that has no process of its own."
  label kind started-at cancel buffer detail visit)

(defvar my/activity--registered nil
  "Live activities registered through `my/activity-register'.")

(defvar my/activity-provider-functions nil
  "Functions returning cached lists of additional `my/activity' objects.
Providers must not perform IO: both counting and board rendering read them.")

(defvar my/activity-poll-functions nil
  "Functions refreshing provider caches at `my/activity-poll-interval'.
IO must be asynchronous; this hook also runs while the animation is active.")

(defvar my/activity--last-poll 0
  "Time the provider caches were last polled.")

(defun my/activity--activities ()
  "Return registered work and the current provider snapshots."
  (append my/activity--registered
          (cl-loop for provider in my/activity-provider-functions
                   append (funcall provider))))

(defun my/activity--poll (&optional force)
  "Refresh provider caches when due, or immediately with FORCE."
  (let ((now (float-time)))
    (when (or force (>= (- now my/activity--last-poll)
                       my/activity-poll-interval))
      (setq my/activity--last-poll now)
      (run-hooks 'my/activity-poll-functions))))

(cl-defun my/activity-register (label &key (kind 'task) cancel)
  "Register running work described by LABEL and return its handle.
KIND is `task', `service' or `connection'.  CANCEL, when given, is called
with no arguments to stop the work from the board.  The caller must pass
the handle to `my/activity-unregister' when the work ends; prefer the
`my/with-activity' macro, which cannot leak it."
  (let ((activity (my/activity--create :label label :kind kind
                                       :started-at (float-time)
                                       :cancel cancel)))
    (push activity my/activity--registered)
    (my/activity--refresh)
    activity))

(defun my/activity-unregister (activity)
  "Remove ACTIVITY from the registry."
  (when activity
    (setq my/activity--registered (delq activity my/activity--registered))
    (my/activity--refresh)))

(defmacro my/with-activity (label &rest body)
  "Run BODY with LABEL registered as a running activity.
The registration is removed when BODY returns, signals, or is quit."
  (declare (indent 1) (debug (form body)))
  (let ((handle (gensym "activity")))
    `(let ((,handle (my/activity-register ,label)))
       (unwind-protect (progn ,@body)
         (my/activity-unregister ,handle)))))

;;; -----------------------------------------------------------------------
;;; Classification

(defun my/activity--owned-task-processes ()
  "Return live processes that some registry claims as finite work.
This is the whole definition of \"task\" in this module.  A process nobody
claims is a service, however busy it looks."
  (let (owned)
    (dolist (process (bound-and-true-p compilation-in-progress))
      (push process owned))
    (when (and (boundp 'comp-async-compilations)
               (hash-table-p comp-async-compilations))
      (dolist (process (hash-table-values comp-async-compilations))
        (push process owned)))
    (when (and (boundp 'remote-tasks) (hash-table-p remote-tasks))
      (dolist (task (hash-table-values remote-tasks))
        (when-let* ((process (ignore-errors (remote-task-process task))))
          (push process owned))))
    (delete-dups (seq-filter #'process-live-p owned))))

(cl-defun my/activity-process-kind
    (process &optional (task-processes (my/activity--owned-task-processes)))
  "Return `task', `service' or `connection' for PROCESS.
TASK-PROCESSES, when given, is a precomputed
`my/activity--owned-task-processes' list, so that classifying a whole
`process-list' stays one scan rather than one per process."
  (cond
   ((memq process task-processes) 'task)
   ((memq (process-type process) '(network serial)) 'connection)
   (t 'service)))

(defun my/activity--native-comp-queue-length ()
  "Return how many files wait for a native-compilation worker.
Queued files have no process yet, so `process-list' cannot see them."
  (if (and (boundp 'comp-files-queue) (listp comp-files-queue))
      (length comp-files-queue)
    0))

;;; -----------------------------------------------------------------------
;;; Count and mode-line segment

(defvar my/activity--count 0
  "Cached number of running tasks, read by the mode-line segment.")

(defvar my/activity--frame-index 0
  "Cached frame counter, read by the mode-line segment.")

(defvar my/activity--timer nil
  "One-shot timer that rescans and advances the frame, or nil.")

(defun my/activity-count ()
  "Return the number of running tasks, scanning now."
  (+ (length (my/activity--owned-task-processes))
     (my/activity--native-comp-queue-length)
     (cl-count 'task (my/activity--activities) :key #'my/activity-kind)))

(defvar-keymap my/activity--mode-line-map
  :doc "Mouse action for the activity indicator."
  "<mode-line> <mouse-1>" #'my/activity-board)

(defun my/activity-mode-line-text ()
  "Return the activity indicator.
Runs on every redisplay of every window, so it only formats two cached
values; the scan behind them happens in `my/activity--tick'."
  (let* ((set (my/activity--frame-set))
         (running (> my/activity--count 0))
         (pick (lambda (frames)
                 (and (> (length frames) 0)
                      (aref frames (mod my/activity--frame-index
                                        (length frames))))))
         (glyph (if running
                    (funcall pick (plist-get set :frames))
                  (plist-get set :rest)))
         (sprite (my/activity--sprite
                  (if running
                      (funcall pick (or (plist-get set :sprites) []))
                    (plist-get set :rest-sprite))))
         (help (if running
                   (format "%d background task(s) running\nmouse-1: Open the activity board"
                           my/activity--count)
                 "Nothing running\nmouse-1: Open the activity board")))
    (concat
     " "
     ;; The sprite is shown through a `display' property over the glyph, so
     ;; the glyph remains the string's own text and a terminal frame, where
     ;; `my/activity--sprite' returns nil, draws that instead.
     (propertize (or glyph "*")
                 'display sprite
                 'face (unless (or running sprite) 'shadow)
                 'help-echo help
                 'mouse-face 'mode-line-highlight
                 'local-map my/activity--mode-line-map)
     ;; The count stays plain text: it must not be swallowed by the image.
     (when (> my/activity--count 1)
       (propertize (format " %d" my/activity--count)
                   'help-echo help
                   'mouse-face 'mode-line-highlight
                   'local-map my/activity--mode-line-map)))))

(defconst my/activity--mode-line-entry
  '(:eval (my/activity-mode-line-text))
  "Mode-line entry for the activity indicator.")

;;; -----------------------------------------------------------------------
;;; Timer

(defun my/activity--schedule ()
  "Arm the next scan, at the frame rate while running and slowly while not."
  (unless my/activity--timer
    (setq my/activity--timer
          (run-with-timer (if (> my/activity--count 0)
                              (/ 1.0 (max 1 my/activity-indicator-fps))
                            my/activity-poll-interval)
                          nil #'my/activity--tick))))

(defun my/activity--tick ()
  "Rescan, advance the frame, and re-arm."
  (setq my/activity--timer nil)
  (my/activity--poll)
  (let ((previous my/activity--count))
    (setq my/activity--count (my/activity-count))
    (if (> my/activity--count 0)
        (setq my/activity--frame-index (1+ my/activity--frame-index))
      (setq my/activity--frame-index 0))
    ;; Redraw while running, and once more on the edge back to rest.
    (when (or (> my/activity--count 0) (/= previous my/activity--count))
      (force-mode-line-update t)))
  (when my/activity-indicator-mode
    (my/activity--schedule)))

(defun my/activity--refresh ()
  "Recount now, without waiting for the next scan.
A pending scan is re-armed whenever the count changed, because the two
rates differ: a timer armed while resting is two seconds away, and work
starting under it would otherwise stand still until it fired."
  (let ((previous my/activity--count))
    (setq my/activity--count (my/activity-count))
    (unless (= previous my/activity--count)
      (when (timerp my/activity--timer)
        (cancel-timer my/activity--timer))
      (setq my/activity--timer nil)
      (when my/activity-indicator-mode
        (my/activity--schedule))
      (force-mode-line-update t))))

;;;###autoload
(define-minor-mode my/activity-indicator-mode
  "Show running background work in the mode line."
  :global t :lighter nil :group 'mode-line
  (if my/activity-indicator-mode
      (progn
        (unless (member my/activity--mode-line-entry
                        (default-value 'mode-line-misc-info))
          (setq-default mode-line-misc-info
                        (append (default-value 'mode-line-misc-info)
                                (list my/activity--mode-line-entry))))
        (my/activity--poll t)
        (my/activity--refresh)
        (my/activity--schedule))
    (when (timerp my/activity--timer)
      (cancel-timer my/activity--timer))
    (setq my/activity--timer nil)
    (setq-default mode-line-misc-info
                  (remove my/activity--mode-line-entry
                          (default-value 'mode-line-misc-info)))
    (force-mode-line-update t)))

;;; -----------------------------------------------------------------------
;;; Board

;; The board is a hub, not a replacement.  Emacs already ships a manager for
;; every kind of background work it knows about, and each one does its own job
;; better than a reimplementation would: `list-processes' for raw process
;; fields, `list-timers' for timers, `proced' for the OS, `memory-report' for
;; allocation.  So the board shows what is running, grouped and actionable,
;; and hands off to the native tool for anything deeper.
;;
;; Performance rules for everything below:
;;
;;   - `process-attributes' is an OS call per process.  It is used only in the
;;     detail view for one process, never while rendering the list.
;;   - Rendering is O(processes + timers) and touches no network or disk.
;;   - Auto refresh is off by default.  When on it is one timer, bounded by
;;     `my/activity-board-refresh-interval' and cancelled with the buffer.

(config-defvar my/activity-board-refresh-interval 2.0
  "Seconds between redraws while the activity board auto-refreshes.
Auto refresh is off until toggled with \\`a' in the board, because each
redraw walks `process-list' and `timer-list'."
  :type 'number :group 'ui)

(defvar my/activity--first-seen
  (make-hash-table :test #'eq :weakness 'key)
  "When each process was first observed, for an elapsed column.
Weakly keyed: a process that dies is dropped with no bookkeeping, since
Emacs itself holds no start time for a process.")

(defvar-local my/activity-board--filter nil
  "Substring the board is currently narrowed to, or nil.")

(defvar-local my/activity-board--show-timers nil
  "Whether the board lists Emacs timers.")

(defvar-local my/activity-board--auto-timer nil
  "Repeating redraw timer for this board buffer, or nil.")

(defun my/activity--elapsed-string (since)
  "Return SINCE formatted as an elapsed time, or a dash when unknown."
  (if (not since)
      "—"
    (let ((seconds (- (float-time) since)))
      (cond ((< seconds 60) (format "%.0fs" seconds))
            ((< seconds 3600) (format "%dm%02ds"
                                      (floor seconds 60)
                                      (floor (mod seconds 60))))
            (t (format "%dh%02dm" (floor seconds 3600)
                       (floor (mod seconds 3600) 60)))))))

(defun my/activity--process-start-time (process)
  "Return when PROCESS started, as a float time.
The OS knows the true start time and Emacs does not, so it is asked once
per process and cached: one system call over a process' whole life, never
one per redraw.  A process the OS will not describe falls back to when it
was first seen here, which is an underestimate but never wrong by more
than the age of this Emacs session."
  (let ((cached (gethash process my/activity--first-seen)))
    (or cached
        (puthash process
                 (let* ((pid (process-id process))
                        (elapsed (and (integerp pid)
                                      (alist-get 'etime
                                                 (ignore-errors
                                                   (process-attributes pid))))))
                   (if elapsed
                       (- (float-time) (float-time elapsed))
                     (float-time)))
                 my/activity--first-seen))))

(defun my/activity--process-elapsed (process)
  "Return how long PROCESS has been running, as a short string."
  (my/activity--elapsed-string (my/activity--process-start-time process)))

(defun my/activity--remote-task-for (process)
  "Return the `remote-task' owning PROCESS, if any."
  (when (and (boundp 'remote-tasks) (hash-table-p remote-tasks))
    (seq-find (lambda (task)
                (eq process (ignore-errors (remote-task-process task))))
              (hash-table-values remote-tasks))))

(defun my/activity--process-label (process)
  "Return a readable label for PROCESS."
  (if-let* ((task (my/activity--remote-task-for process)))
      (format "%s" (remote-task-name task))
    (process-name process)))

(defun my/activity--process-detail (process)
  "Return the command line behind PROCESS, as a single line."
  (let ((command (process-command process)))
    (cond
     ((consp command) (string-join command " "))
     ((stringp command) command)
     (t (format "%s" (process-status process))))))

;;; -----------------------------------------------------------------------
;;; Board: row actions

(defun my/activity-board--process-at-point ()
  "Return the process on the current row, or signal."
  (or (get-text-property (point) 'my/activity-process)
      (user-error "No process on this line")))

(defun my/activity-board-stop ()
  "Stop the task on the current board row.
A task whose owner provides a cancel path uses it.  Killing a process no
registry claims is destructive and asks first."
  (interactive)
  (let ((process (get-text-property (point) 'my/activity-process))
        (activity (get-text-property (point) 'my/activity-handle)))
    (cond
     ((my/activity-p activity)
      (if-let* ((cancel (my/activity-cancel activity)))
          (progn (funcall cancel)
                 (my/activity-unregister activity)
                 (my/activity-board-refresh))
        (user-error "%s cannot be cancelled" (my/activity-label activity))))
     ((not (processp process))
      (user-error "No task on this line"))
     ((my/activity--remote-task-for process)
      (remote-task-cancel (my/activity--remote-task-for process))
      (my/activity-board-refresh))
     ((yes-or-no-p (format "Kill %s, which no task registry owns? "
                           (process-name process)))
      (delete-process process)
      (my/activity-board-refresh)))))

(defun my/activity-board-interrupt ()
  "Send SIGINT to the process on the current row.
This is the polite stop: a compiler or shell gets to clean up after it."
  (interactive)
  (let ((process (my/activity-board--process-at-point)))
    (interrupt-process process)
    (message "Interrupted %s" (process-name process))
    (my/activity-board-refresh)))

(defun my/activity-board-kill ()
  "Send SIGKILL to the process on the current row.
Unconditional and unclean, so it asks first."
  (interactive)
  (let ((process (my/activity-board--process-at-point)))
    (when (yes-or-no-p (format "SIGKILL %s? " (process-name process)))
      (signal-process process 'SIGKILL)
      (my/activity-board-refresh))))

(defun my/activity-board--show-process (process)
  "Show PROCESS' buffer, or say that it has none."
  (let ((buffer (and (processp process) (process-buffer process))))
    (if (buffer-live-p buffer)
        (pop-to-buffer buffer)
      (user-error "%s has no buffer" (process-name process)))))

(defun my/activity-board-visit ()
  "Show the buffer behind the current board row, if it has one."
  (interactive)
  (if-let* ((activity (get-text-property (point) 'my/activity-handle)))
      (cond ((my/activity-visit activity)
             (funcall (my/activity-visit activity)))
            ((buffer-live-p (my/activity-buffer activity))
             (pop-to-buffer (my/activity-buffer activity)))
            (t (user-error "%s has no buffer" (my/activity-label activity))))
    (my/activity-board--show-process
     (get-text-property (point) 'my/activity-process))))

(defun my/activity-board-help ()
  "Say which keys this board takes, instead of a read-only error.
Every board here is read-only, so an unbound letter would otherwise reach
`self-insert-command' and report that the buffer cannot be edited, which
says nothing about what to press instead."
  (interactive)
  (message "%s" (substitute-command-keys "\\<my/activity-board-mode-map>\
j/k move  \\[aaron-ui-board-activate] open  \\[my/activity-board-describe] details  \
\\[my/activity-board-stop] stop  \\[my/activity-board-interrupt] interrupt  \
\\[my/activity-board-kill] kill  \\[aaron-ui-board-refresh] refresh  \
\\[my/activity-board-filter] filter  \\[describe-mode] help")))

(defun my/activity-board-copy-command ()
  "Copy the current row's command line to the kill ring."
  (interactive)
  (let ((command (my/activity--process-detail
                  (my/activity-board--process-at-point))))
    (kill-new command)
    (message "%s" command)))

(defun my/activity-board-describe ()
  "Show everything Emacs and the OS know about the current row's process.
This is the one place `process-attributes' is called: it is a system call
per process and must never run while rendering the list."
  (interactive)
  (let* ((process (my/activity-board--process-at-point))
         (pid (process-id process))
         (attributes (and (integerp pid) (process-attributes pid))))
    (with-current-buffer (get-buffer-create "*Activity Process*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (format "%s\n\n" (process-name process)))
        (pcase-dolist (`(,label . ,value)
                       `(("Status"  . ,(process-status process))
                         ("PID"     . ,(or pid "—"))
                         ("Type"    . ,(process-type process))
                         ("Buffer"  . ,(if (buffer-live-p (process-buffer process))
                                           (buffer-name (process-buffer process))
                                         "—"))
                         ("TTY"     . ,(or (process-tty-name process) "—"))
                         ("Query"   . ,(process-query-on-exit-flag process))
                         ("Command" . ,(my/activity--process-detail process))))
          (insert (format "%-10s %s\n" label value)))
        (when attributes
          (insert "\nOS:\n")
          (pcase-dolist (`(,key . ,value) attributes)
            (insert (format "%-10s %s\n" key value))))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

;;; -----------------------------------------------------------------------
;;; Board: view controls

(defun my/activity-board-filter (pattern)
  "Narrow the board to rows matching PATTERN; empty input clears it."
  (interactive
   (list (read-string "Filter (empty clears): "
                      (or my/activity-board--filter ""))))
  (setq my/activity-board--filter
        (unless (string-empty-p (string-trim pattern)) (string-trim pattern)))
  (my/activity-board-refresh))

(defun my/activity-board-toggle-timers ()
  "Show or hide the Emacs timer section."
  (interactive)
  (setq my/activity-board--show-timers (not my/activity-board--show-timers))
  (my/activity-board-refresh))

(defun my/activity-board-toggle-auto-refresh ()
  "Start or stop redrawing this board on a timer.
Off by default: each redraw walks `process-list' and `timer-list', and a
board left open on a fast interval is a background cost of its own."
  (interactive)
  (if my/activity-board--auto-timer
      (progn
        (cancel-timer my/activity-board--auto-timer)
        (setq my/activity-board--auto-timer nil)
        (message "Activity board auto refresh off"))
    (let ((buffer (current-buffer)))
      (setq my/activity-board--auto-timer
            (run-with-timer
             my/activity-board-refresh-interval
             my/activity-board-refresh-interval
             (lambda ()
               ;; The timer outlives nothing: a dead buffer cancels it, and
               ;; so does `my/activity-board--cleanup' on kill.
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (get-buffer-window buffer t)
                       (my/activity-board-refresh)))
                 (cancel-timer my/activity-board--auto-timer))))))
    (message "Activity board auto refresh every %.1fs"
             my/activity-board-refresh-interval))
  (force-mode-line-update))

(defun my/activity-board--cleanup ()
  "Cancel this board's auto-refresh timer when the buffer goes away."
  (when (timerp my/activity-board--auto-timer)
    (cancel-timer my/activity-board--auto-timer))
  (setq my/activity-board--auto-timer nil))

;;; -----------------------------------------------------------------------
;;; Board: native hand-offs

(defconst my/activity-board-native-links
  '((:label "Processes" :command list-processes
     :help "Built-in raw process list, with every field Emacs tracks")
    (:label "Timers" :command list-timers
     :help "Built-in timer list: what Emacs will run, and when")
    (:label "Threads" :command list-threads
     :help "Built-in thread list")
    (:label "Proced" :command proced
     :help "Built-in OS process manager, beyond this Emacs")
    (:label "Memory" :command memory-report
     :help "Built-in memory report")
    (:label "Messages" :command view-echo-area-messages
     :help "The *Messages* log"))
  "Built-in Emacs managers the board hands off to.
Each one is better at its own job than a reimplementation here would be.")

(defconst my/activity-board-config-links
  '((:label "Performance" :command my/performance-watch
     :help "This configuration's performance watch")
    (:label "Remote" :command remote-board
     :help "Remote targets, routes and forwards")
    (:label "Compile" :command my/compile-board
     :help "Byte and native compilation")
    (:label "Language servers" :command my/language-server-manager
     :help "Language server manager")
    (:label "Config" :command config-board
     :help "The configuration registry"))
  "Boards in this configuration that own a slice of background work.")

(defun my/activity-board--available-links (links)
  "Return LINKS whose commands exist in this session."
  (seq-filter (lambda (link) (fboundp (plist-get link :command))) links))

;;; -----------------------------------------------------------------------
;;; Board: mode

;; `aaron-ui-board-mode-map' already gives every board in this configuration
;; j/n down, k/p up and RET activate.  Nothing here may shadow those: a board
;; where k stops a task instead of moving up is both inconsistent with the
;; others and one keystroke away from killing something by accident.  So the
;; stop keys sit on x/i/X, away from navigation.
(defvar-keymap my/activity-board-mode-map
  :parent aaron-ui-board-mode-map
  :doc "Keymap for `my/activity-board-mode'."
  "x" #'my/activity-board-stop
  "i" #'my/activity-board-interrupt
  "X" #'my/activity-board-kill
  "v" #'my/activity-board-visit
  "w" #'my/activity-board-copy-command
  "d" #'my/activity-board-describe
  "/" #'my/activity-board-filter
  "t" #'my/activity-board-toggle-timers
  "a" #'my/activity-board-toggle-auto-refresh
  "P" #'list-processes
  "T" #'list-timers
  "H" #'list-threads
  "O" #'proced
  "M" #'memory-report
  "<remap> <self-insert-command>" #'my/activity-board-help)

(define-derived-mode my/activity-board-mode aaron-ui-board-mode "Activity"
  "Hub for everything Emacs is running in the background.

Move:   j/n down   k/p up   TAB next button
Rows:   RET open   v buffer   d details   w copy command
Stop:   x stop (asks when nothing owns it)   i interrupt   X SIGKILL
View:   g refresh   a auto refresh   / filter   t timers
Native: P processes  T timers  H threads  O proced  M memory"
  (add-hook 'kill-buffer-hook #'my/activity-board--cleanup nil t))

;;; -----------------------------------------------------------------------
;;; Board: rendering

(defun my/activity-board--matches-p (&rest fields)
  "Return non-nil when the active filter matches any of FIELDS."
  (or (null my/activity-board--filter)
      (let ((needle (downcase my/activity-board--filter)))
        (seq-some (lambda (field)
                    (and field (string-search needle (downcase (format "%s" field)))))
                  fields))))

(defun my/activity--insert-process-row (process kind)
  "Insert one board row for PROCESS classified as KIND."
  (aaron-ui-board-insert-row
   :id (format "%s" process)
   :icon (pcase kind ('task 'compile) ('connection 'server) (_ 'process))
   :badge (format "%s" (process-status process))
   :badge-tone (pcase (process-status process)
                 ((or 'run 'open 'listen 'connect) 'success)
                 ((or 'exit 'signal 'closed 'failed) 'danger)
                 (_ 'muted))
   :title (my/activity--process-label process)
   :meta (my/activity--process-elapsed process)
   :detail (my/activity--process-detail process)
   :action (lambda (_) (my/activity-board--show-process process))
   :help "RET shows this process' buffer"
   :properties (list 'my/activity-process process)))

(defun my/activity--insert-group (title kind processes)
  "Insert the board section TITLE for PROCESSES classified as KIND."
  (aaron-ui-board-insert-section title (length processes))
  (if (null processes)
      (aaron-ui-board-insert-empty (if my/activity-board--filter
                                       "Nothing matches the filter"
                                     "None"))
    (dolist (process processes)
      (my/activity--insert-process-row process kind))))

(defun my/activity--timer-rows ()
  "Return the live timers as (LABEL REPEAT DUE) triples.
Reads `timer-list' and `timer-idle-list' only, the same state
`list-timers' shows; nothing here schedules or cancels anything."
  (let (rows)
    (dolist (entry (append timer-list timer-idle-list))
      (let* ((function (timer--function entry))
             (label (if (symbolp function)
                        (symbol-name function)
                      "<lambda>"))
             (repeat (timer--repeat-delay entry))
             (idle (memq entry timer-idle-list)))
        (push (list label
                    (cond ((and repeat idle) (format "idle, every %ss" repeat))
                          (idle "idle, once")
                          (repeat (format "every %ss" repeat))
                          (t "once"))
                    entry)
              rows)))
    (nreverse rows)))

(defun my/activity--insert-timers ()
  "Insert the Emacs timer section."
  (let ((rows (seq-filter (lambda (row) (my/activity-board--matches-p (car row)))
                          (my/activity--timer-rows))))
    (aaron-ui-board-insert-section "Emacs timers" (length rows))
    (if (null rows)
        (aaron-ui-board-insert-empty "None")
      (pcase-dolist (`(,label ,schedule ,_timer) rows)
        (aaron-ui-board-insert-row
         :id (concat "timer:" label schedule)
         :icon 'clock
         :badge schedule
         :badge-tone (if (string-prefix-p "idle" schedule) 'muted 'info)
         :title label))
      (aaron-ui-board-insert-empty "T opens the built-in timer list"))))

(defun my/activity-board-refresh ()
  "Redraw the activity board."
  (interactive)
  (my/activity--refresh)
  (let* ((inhibit-read-only t)
         (task-processes (my/activity--owned-task-processes))
         (activities (my/activity--activities))
         (groups (list (cons 'task nil) (cons 'service nil)
                       (cons 'connection nil)))
         (queued (my/activity--native-comp-queue-length))
         (total 0))
    (dolist (process (process-list))
      (when (process-live-p process)
        (setq total (1+ total))
        (when (my/activity-board--matches-p (my/activity--process-label process)
                                            (my/activity--process-detail process))
          (let ((cell (assq (my/activity-process-kind process task-processes)
                            groups)))
            (setcdr cell (cons process (cdr cell)))))))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Activity" :icon 'process
        :subtitle (if (> my/activity--count 0)
                      (format "%d task(s) running · %d process(es) total"
                              my/activity--count total)
                    (format "Nothing running · %d process(es) total" total))
        :stats (delq nil
                     (list (cons (format "%d tasks" my/activity--count)
                                 (if (> my/activity--count 0) 'success 'muted))
                           (cons (format "%d services"
                                         (length (alist-get 'service groups)))
                                 'info)
                           (cons (format "%d connections"
                                         (length (alist-get 'connection groups)))
                                 'info)
                           (when my/activity-board--filter
                             (cons (format "filter: %s" my/activity-board--filter)
                                   'warning))
                           (when my/activity-board--auto-timer
                             (cons "auto" 'warning))))
        :actions (append
                  (list (list :label "Refresh" :command #'my/activity-board-refresh
                              :help "Redraw now" :primary t)
                        (list :label (if my/activity-board--auto-timer
                                         "Auto: on" "Auto: off")
                              :command #'my/activity-board-toggle-auto-refresh
                              :help "Toggle periodic redraw")
                        (list :label (if my/activity-board--show-timers
                                         "Timers: shown" "Timers: hidden")
                              :command #'my/activity-board-toggle-timers
                              :help "Show or hide Emacs timers")
                        (list :label "Filter" :command #'my/activity-board-filter
                              :help "Narrow to matching rows"))))
       ;; Work with no process of its own comes first: it is invisible to
       ;; `list-processes', so this board is the only place it shows.
       (when (or activities (> queued 0))
         (aaron-ui-board-insert-section
          "In flight (no process)"
          (+ (length activities) (if (> queued 0) 1 0)))
         (dolist (activity activities)
           (aaron-ui-board-insert-row
            :id (format "%s" activity)
            :icon 'clock
            :badge (format "%s" (my/activity-kind activity))
            :badge-tone 'info
            :title (my/activity-label activity)
            :meta (my/activity--elapsed-string (my/activity-started-at activity))
            :detail (my/activity-detail activity)
            :action (when (or (my/activity-visit activity)
                              (buffer-live-p (my/activity-buffer activity)))
                      (lambda (_) (my/activity-board-visit)))
            :properties (list 'my/activity-handle activity)))
         (when (> queued 0)
           (aaron-ui-board-insert-row
            :id "native-comp-queue"
            :icon 'compile
            :badge "queued" :badge-tone 'warning
            :title "Native compilation queue"
            :meta (format "%d file(s)" queued))))
       (my/activity--insert-group "Tasks" 'task (alist-get 'task groups))
       (my/activity--insert-group "Services" 'service (alist-get 'service groups))
       (my/activity--insert-group "Connections" 'connection
                                  (alist-get 'connection groups))
       (when my/activity-board--show-timers
         (my/activity--insert-timers))
       (aaron-ui-board-insert-section "Built-in managers")
       (insert "   ")
       (aaron-ui-board-insert-actions
        (my/activity-board--available-links my/activity-board-native-links))
       (insert "\n\n")
       (aaron-ui-board-insert-section "This configuration")
       (insert "   ")
       (aaron-ui-board-insert-actions
        (my/activity-board--available-links my/activity-board-config-links))
       (insert "\n\n")
       (aaron-ui-board-insert-key-hints
        (concat "Move: j/k   Rows: RET open  v buffer  d details  w copy\n"
                "Stop: x stop  i interrupt  X kill    "
                "View: g refresh  a auto  / filter  t timers\n"
                "Native: P processes  T timers  H threads  O proced  M memory"))))))

;;;###autoload
(defun my/activity-board ()
  "Show every process Emacs is running, grouped by what it is."
  (interactive)
  (my/activity--poll t)
  (my/activity--refresh)
  (with-current-buffer (get-buffer-create "*Activity*")
    (unless (derived-mode-p 'my/activity-board-mode)
      (my/activity-board-mode))
    (aaron-ui-board-set-header "Activity" 'process)
    (setq-local aaron-ui-board-refresh-function #'my/activity-board-refresh)
    (my/activity-board-refresh))
  (pop-to-buffer "*Activity*"))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/activity-board-mode 'emacs))

;;; -----------------------------------------------------------------------
;;; In-band triggers

;; Visiting a file whose route pays its own shell round trip per operation is
;; the one blocking operation that regularly looks hung.  It has no process to
;; show, so it registers explicitly.  The cost is asked from the selected
;; Remote backend rather than from `file-remote-p', so a batched backend such
;; as tramp-rpc opens without raising the indicator.
;;
;; This is the "during" half of the slow-open feedback.  The "after" half is
;; `my/find-file-feedback-a' in init-tramp.el, which reports the elapsed time
;; once the file is open.  This one only draws, that one only measures.
(defun my/activity-find-file-a (fn filename &rest args)
  "Run FN on FILENAME as a registered activity when the route is slow."
  (if (my/remote-per-file-subprocess-affordable-p filename)
      (apply fn filename args)
    (my/with-activity (format "Opening %s" (file-name-nondirectory filename))
      (apply fn filename args))))

(advice-add 'find-file :around #'my/activity-find-file-a)

(my/activity-indicator-mode 1)

(provide 'init-activity)
;;; init-activity.el ends here
