;;; remote-transfer.el --- Queued file jobs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Bulk transfers between logical directories run through client tools;
;; same-target file operations run through routed target processes.  Both
;; are asynchronous and use one queue and lifecycle.
;;
;; `remote-transfer-start' copies or synchronizes with rsync as an
;; asynchronous job that reports progress; `remote-transfer-list' is the
;; queue.
;;
;; Neither path names a machine or a backend.  A directory the client reads
;; directly (`remote-client-file-name') is passed as a plain path; any other
;; is reached through the backend's `:client-shell'.  A backend that offers
;; neither is refused, never silently served from the client's own disk.
;;
;; A job belongs to its targets: an explicit disconnect ends it, and so does
;; leaving Emacs.  Nothing here mounts a filesystem or needs a system
;; extension; the tools are ordinary user programs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-backend)
(require 'remote-process)

(declare-function evil-set-initial-state "evil-core" (mode state))

(defgroup remote-transfer nil
  "Queued bulk transfers."
  :group 'remote)

(defcustom remote-transfer-history-limit 40
  "Finished transfers kept in the queue before the oldest are dropped."
  :type 'integer
  :group 'remote-transfer)

(defcustom remote-transfer-log-limit 200000
  "Characters of tool output kept per transfer."
  :type 'integer
  :group 'remote-transfer)

(defvar remote-transfer-finished-functions nil
  "Functions called with a transfer job once it has ended.")

(cl-defstruct (remote-transfer-job
               (:constructor remote-transfer-job--create))
  id name kind sources destination target-ids command environment route
  execution overwrite state percent rate eta started finished exit-status process
  transport-process pending
  log-buffer callback kill-timer)

(cl-defstruct (remote-file-operation-plan
               (:constructor remote-file-operation-plan--create))
  method sources destination overwrite command target-id)

(defvar remote-transfer-jobs nil
  "Transfer jobs, newest first.")

(defvar remote-transfer--next-id 0)

(defvar remote-transfer--refresh-timer nil
  "Timer redrawing progress while a transfer runs.")

(defvar-local remote-transfer--log-job nil
  "Transfer whose output the current log buffer shows.")

(defvar-local remote-transfer--queue-owner-frame nil
  "Frame whose transfer queue view this buffer owns.")

(defvar-keymap remote-transfer-log-mode-map
  :parent special-mode-map
  :doc "Keymap for a transfer job's read-only output."
  "q" #'quit-window)

(define-derived-mode remote-transfer-log-mode special-mode "Transfer Log"
  "Read-only output for one transfer job.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'remote-transfer-log-mode 'emacs)
  (evil-set-initial-state 'remote-transfer-list-mode 'emacs))

;;; Endpoints

(defun remote-transfer-endpoint (file-name)
  "Return how a client-side bulk tool addresses FILE-NAME.
The plist has `:target', `:path' and, when the client cannot read the file
directly, `:shell': the backend's `:client-shell' answer plus its `:route'."
  (let* ((canonical (remote-canonicalize-file-name file-name))
         (target-id (remote-file-name-target canonical))
         (client (remote-client-file-name canonical)))
    (if client
        (list :target target-id :path client)
      (let ((shell
             (seq-some
              (lambda (route)
                (when-let* ((shell
                             (condition-case nil
                                 (remote-backend-client-shell route)
                               (remote-backend-unsupported nil))))
                  (append shell (list :route route))))
              (remote-routes "process" 'process-async
                             (remote-context canonical)))))
        (unless shell
          (signal 'remote-backend-unsupported
                  (list (format "Target %s offers no bulk transfer connection"
                                target-id))))
        (list :target target-id
              :path (remote-file-local-name canonical)
              :shell shell)))))

(defun remote-transfer--destination-host (destination)
  "Return DESTINATION spelled for a HOST:PATH argument.
An IPv6 literal is bracketed so its colons are not read as the separator."
  (if (string-match "\\`\\(?:\\(.*\\)@\\)?\\([^@]*:[^@]*\\)\\'" destination)
      (let ((user (match-string 1 destination))
            (host (match-string 2 destination)))
        (concat (and user (concat user "@")) "[" host "]"))
    destination))

(defun remote-transfer--spec (endpoint &optional contents)
  "Return ENDPOINT as an rsync argument.
CONTENTS non-nil names what is inside the directory instead of the directory."
  (let* ((path (plist-get endpoint :path))
         (path (if contents
                   (file-name-as-directory path)
                 (if (equal path "/") path (directory-file-name path)))))
    (if-let* ((shell (plist-get endpoint :shell)))
        (concat (remote-transfer--destination-host
                 (plist-get shell :destination))
                ":" path)
      path)))

(defun remote-transfer--rsh-string (command)
  "Return client argv COMMAND as rsync's remote-shell string.
rsync splits it on blanks and honours quotes but no backslash escapes."
  (mapconcat
   (lambda (argument)
     (cond
      ((string-match-p "'" argument)
       (signal 'remote-backend-unsupported
               (list "Remote shell argument contains a quote" argument)))
      ((string-match-p "[[:space:]\"]" argument)
       (concat "'" argument "'"))
      (t argument)))
   command " "))

(defun remote-transfer--client-tool (name)
  "Return the client program NAME or signal a `user-error'."
  (or (remote-client-executable-find name)
      (user-error "This machine has no %s" name)))

(defvar remote-transfer--rsync-version-cache nil
  "Cons of an rsync program and its version as a list of numbers, or nil.")

(defun remote-transfer--rsync-version (rsync)
  "Return RSYNC's version as (MAJOR MINOR PATCH), or nil for another tool.
macOS ships openrsync under the same name; it reports no such version."
  (unless (equal (car remote-transfer--rsync-version-cache) rsync)
    (setq remote-transfer--rsync-version-cache
          (cons
           rsync
           (with-temp-buffer
             (let ((default-directory temporary-file-directory))
               (ignore-errors (call-process rsync nil t nil "--version")))
             (goto-char (point-min))
             (and (re-search-forward
                   "^rsync +version \\([0-9]+\\)\\.\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?"
                   nil t)
                  (list (string-to-number (match-string 1))
                        (string-to-number (match-string 2))
                        (string-to-number (or (match-string 3) "0"))))))))
  (cdr remote-transfer--rsync-version-cache))

(defun remote-transfer--rsync-options (rsync dry-run)
  "Return the options RSYNC's version allows, for a DRY-RUN or a transfer."
  (let ((version (remote-transfer--rsync-version rsync)))
    (append
     ;; From 3.2.4 the client escapes remote names itself, which every server
     ;; accepts.  Before that only `-s' keeps blanks in a remote name intact,
     ;; and a server that is not rsync proper rejects it.
     (when (and version
                (version-list-<= '(3 0 0) version)
                (version-list-< version '(3 2 4)))
       (list "-s"))
     (cond
      (dry-run (list "-n" "-i"))
      ;; Whole-transfer progress needs 3.1; older tools report per file.
      ((and version (version-list-<= '(3 1 0) version))
       (list "--info=progress2" "--no-inc-recursive"))
      (t (list "--progress"))))))

(cl-defun remote-transfer-command
    (sources destination &key contents delete dry-run)
  "Return (ARGV ENVIRONMENT ROUTE TARGET-IDS) copying SOURCES to DESTINATION.
SOURCES are logical file names on one target and DESTINATION is a logical
directory.  CONTENTS copies what is inside a single source directory rather
than the directory.  DELETE removes what the source no longer has.  DRY-RUN
only lists the changes."
  (let* ((rsync (remote-transfer--client-tool "rsync"))
         (source-endpoints (mapcar #'remote-transfer-endpoint sources))
         (destination-endpoint (remote-transfer-endpoint destination))
         (source-shell (plist-get (car source-endpoints) :shell))
         (destination-shell (plist-get destination-endpoint :shell))
         (shell (or source-shell destination-shell))
         (target-ids
          (delete-dups
           (mapcar (lambda (endpoint) (plist-get endpoint :target))
                   (cons destination-endpoint source-endpoints)))))
    (unless sources
      (user-error "Nothing to transfer"))
    (when (> (length (delete-dups
                      (mapcar (lambda (endpoint) (plist-get endpoint :target))
                              source-endpoints)))
             1)
      (user-error "Transfer sources must be on one target"))
    (when (and source-shell destination-shell)
      (user-error
       "rsync cannot copy between two targets directly; go through a folder on this machine"))
    (list
     (append
      (list rsync "-a" "-h" "--partial")
      (remote-transfer--rsync-options rsync dry-run)
      (when delete (list "--delete"))
      (when shell
        (list "-e" (remote-transfer--rsh-string (plist-get shell :command))))
      (mapcar (lambda (endpoint)
                (remote-transfer--spec endpoint contents))
              source-endpoints)
      (list (remote-transfer--spec destination-endpoint t)))
     (and shell (plist-get shell :environment))
     (and shell (plist-get shell :route))
     target-ids)))

(cl-defun remote-transfer-file-operation-command
    (method sources destination &key overwrite)
  "Return a target-side argv for METHOD on SOURCES into DESTINATION.
METHOD is `copy', `move', `symlink', `relative-symlink' or `hardlink'.
All names must belong to one target.  OVERWRITE is nil, `ask' or t; links
never replace an existing entry.  Preflight completes before any process is
started, so a rejected selection cannot partly execute."
  (unless (memq method '(copy move symlink relative-symlink hardlink))
    (user-error "Unsupported target file operation: %s" method))
  (unless sources
    (user-error "No files selected"))
  (let* ((destination (file-name-as-directory
                       (remote-canonicalize-file-name destination)))
         (sources (mapcar #'remote-canonicalize-file-name sources))
         (target (remote-file-name-target destination))
         (seen (make-hash-table :test #'equal))
         conflicts)
    (unless (cl-every (lambda (source)
                        (equal (remote-file-name-target source) target))
                      sources)
      (user-error "Target file operation requires one source target"))
    (unless (file-directory-p destination)
      (user-error "Destination is not a directory: %s" destination))
    (dolist (source sources)
      (let* ((name (file-name-nondirectory (directory-file-name source)))
             (output (expand-file-name name destination)))
        (unless (or (file-exists-p source) (file-symlink-p source))
          (user-error "Selected file no longer exists: %s" source))
        (when (and (eq method 'hardlink) (file-directory-p source))
          (user-error "Cannot hardlink a directory: %s" source))
        (when (gethash name seen)
          (user-error "Multiple selected sources are named %s" name))
        (puthash name t seen)
        (when (equal source output)
          (user-error "Cannot %s a file into its own path: %s"
                      method source))
        (when (and (memq method '(copy move))
                   (file-directory-p source)
                   (string-prefix-p (file-name-as-directory source)
                                    destination))
          (user-error "Cannot %s a directory into itself: %s"
                      method source))
        (when (or (file-exists-p output) (file-symlink-p output))
          (when (or (memq method '(symlink relative-symlink hardlink))
                    (file-directory-p output))
            (user-error "Destination already exists: %s" output))
          (push output conflicts))))
    (when conflicts
      (pcase overwrite
        ('ask
         (unless (yes-or-no-p
                  (format "Overwrite %d existing file%s on %s? "
                          (length conflicts)
                          (if (= (length conflicts) 1) "" "s") target))
           (user-error "Target file operation cancelled")))
         ('t nil)
         (_ (user-error "Destination exists: %s" (car conflicts)))))
    (let* ((physical-destination (remote-file-local-name destination))
           (physical-sources (mapcar #'remote-file-local-name sources))
           (program (pcase method
                      ('copy "cp") ('move "mv") (_ "ln")))
           (arguments
            (pcase method
              ('copy (append (list "-R" "-p")
                             (list (if conflicts "-f" "-n"))
                             physical-sources
                             (list physical-destination)))
              ('move (append (list (if conflicts "-f" "-n"))
                             physical-sources
                             (list physical-destination)))
              ('relative-symlink
               (append (list "-s")
                       (mapcar (lambda (source)
                                 (file-relative-name
                                  source physical-destination))
                               physical-sources)
                       (list physical-destination)))
              ('symlink
               (append (list "-s") physical-sources
                       (list physical-destination)))
              ('hardlink
               (append physical-sources (list physical-destination))))))
      (cons program arguments))))

;;; Jobs

(defun remote-transfer--running ()
  "Return the transfers that are still running."
  (seq-filter (lambda (job) (eq (remote-transfer-job-state job) 'running))
              remote-transfer-jobs))

(defun remote-transfer-jobs-for-target (target-id &optional running)
  "Return the transfers touching TARGET-ID, only live ones with RUNNING."
  (seq-filter
   (lambda (job)
     (and (member target-id (remote-transfer-job-target-ids job))
          (or (not running)
              (eq (remote-transfer-job-state job) 'running))))
   remote-transfer-jobs))

(defun remote-transfer--log (job text)
  "Append TEXT to JOB's bounded log."
  (let ((buffer (remote-transfer-job-log-buffer job)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert text)
            (when (> (buffer-size) remote-transfer-log-limit)
              (goto-char (point-min))
              (forward-char (/ remote-transfer-log-limit 2))
              (forward-line 1)
              (delete-region (point-min) (point))))
          (when moving (goto-char (point-max))))))))

(defconst remote-transfer--progress-regexp
  "\\([0-9]+\\)%[ \t]+\\([0-9.,]+[[:alpha:]]*B/s\\)[ \t]+\\([0-9:]+\\)"
  "Match the percentage, rate and time of an rsync progress report.")

(defun remote-transfer--filter (job output)
  "Fold tool OUTPUT into JOB's progress and log."
  (let* ((text (concat (remote-transfer-job-pending job) output))
         (lines (split-string text "[\r\n]"))
         (complete (butlast lines)))
    (setf (remote-transfer-job-pending job) (car (last lines)))
    (dolist (line complete)
      (cond
       ((string-match remote-transfer--progress-regexp line)
        (setf (remote-transfer-job-percent job)
              (string-to-number (match-string 1 line))
              (remote-transfer-job-rate job) (match-string 2 line)
              (remote-transfer-job-eta job) (match-string 3 line)))
       ((not (string-blank-p line))
        (remote-transfer--log job (concat line "\n")))))))

(defun remote-transfer--trim-history ()
  "Drop the oldest finished transfers beyond the history limit."
  (let ((finished 0) kept)
    (dolist (job remote-transfer-jobs)
      (if (eq (remote-transfer-job-state job) 'running)
          (push job kept)
        (cl-incf finished)
        ;; Do not close a log while the user is reading it.  The explicit
        ;; clear action can still remove it, using Emacs's window return.
        (if (or (<= finished remote-transfer-history-limit)
                (when-let* ((log (remote-transfer-job-log-buffer job)))
                  (get-buffer-window log t)))
            (push job kept)
          (when (buffer-live-p (remote-transfer-job-log-buffer job))
            (kill-buffer (remote-transfer-job-log-buffer job))))))
    (setq remote-transfer-jobs (nreverse kept))))

(defun remote-transfer--complete (job status outcome)
  "Finish JOB with exit STATUS and OUTCOME, preserving cancellation."
  (unless (remote-transfer-job-finished job)
    (when (timerp (remote-transfer-job-kill-timer job))
      (cancel-timer (remote-transfer-job-kill-timer job))
      (setf (remote-transfer-job-kill-timer job) nil))
    (setf (remote-transfer-job-exit-status job) status
          (remote-transfer-job-finished job) (float-time)
          (remote-transfer-job-process job) nil
          (remote-transfer-job-transport-process job) nil
          (remote-transfer-job-state job)
          (if (eq (remote-transfer-job-state job) 'cancelled)
              'cancelled outcome))
    (when (eq (remote-transfer-job-state job) 'done)
      (setf (remote-transfer-job-percent job) 100))
    (remote-transfer--log
     job (format "\n[%s]\n" (remote-transfer--state-text job)))
    (remote-transfer--trim-history)
    (remote-transfer--refresh)
    (when-let* ((callback (remote-transfer-job-callback job)))
      (condition-case err
          (funcall callback job)
        (error (remote-log 'transfer-callback-error
                           :error (error-message-string err)))))
    (run-hook-with-args 'remote-transfer-finished-functions job)))

(defun remote-transfer--sentinel (job process)
  "Record how JOB's client PROCESS ended and release what it held."
  (unless (process-live-p process)
    (when-let* ((pending (remote-transfer-job-pending job))
                ((not (string-blank-p pending))))
      (remote-transfer--filter job "\n"))
    (let ((status (process-exit-status process)))
      (remote-transfer--complete
       job status
       (cond
        ((not (eq (process-status process) 'exit)) 'cancelled)
        ((zerop status) 'done)
        (t 'failed))))))

(defun remote-transfer--invalidate-target-cache (job route)
  "Forget paths JOB may have changed through target ROUTE."
  (when route
    (condition-case err
        (progn
          (remote-fs-invalidate-mutated-path
           (remote-transfer-job-destination job) route t)
          (pcase (remote-transfer-job-kind job)
            ('move
             (dolist (parent (delete-dups
                              (mapcar #'file-name-directory
                                      (remote-transfer-job-sources job))))
               (remote-fs-invalidate-mutated-path parent route t)))
            ('hardlink
             (dolist (source (remote-transfer-job-sources job))
               (remote-fs-invalidate-mutated-path source route)))))
      (error
       (remote-transfer--log
        job (format "Cache refresh failed: %s\n"
                    (error-message-string err)))))))

(defun remote-transfer--target-finished (job result)
  "Record target-side RESULT for JOB and release its process."
  (unless (remote-transfer-job-finished job)
    (dolist (output (list (remote-exec-result-stdout result)
                          (remote-exec-result-stderr result)))
      (when (and (stringp output) (not (string-empty-p output)))
        (remote-transfer--log
         job (if (string-suffix-p "\n" output) output (concat output "\n")))))
    ;; `make-process' does not flush metadata the way `process-file' does.
    ;; Invalidate before reporting completion, even after partial failure.
    (remote-transfer--invalidate-target-cache
     job (remote-exec-result-route result))
    (let ((status (remote-exec-result-status result)))
      (remote-transfer--complete
       job (if (integerp status) status 1)
       (if (and (integerp status) (zerop status)) 'done 'failed)))))

(defun remote-transfer--state-text (job)
  "Return JOB's state as a short phrase."
  (pcase (remote-transfer-job-state job)
    ('running "running")
    ('done "done")
    ('cancelled "cancelled")
    ('failed (if-let* ((status (remote-transfer-job-exit-status job)))
                 (format "failed (%s)" status)
               "failed (status unknown)"))
    (state (format "%s" state))))

(defun remote-transfer--client-process-environment (environment)
  "Return the client process environment extended by alist ENVIRONMENT."
  (append
   (mapcar (lambda (entry) (concat (car entry) "=" (cdr entry)))
           environment)
   (remote-client-process-environment)))

(defun remote-transfer--launch (job)
  "Start JOB's client or target process and return JOB."
  (let* ((log (generate-new-buffer
               (format " *remote-transfer %d*" (remote-transfer-job-id job))))
         process)
    (with-current-buffer log
      (remote-transfer-log-mode)
      (setq-local remote-transfer--log-job job))
    (setf (remote-transfer-job-log-buffer job) log
          (remote-transfer-job-state job) 'running
          (remote-transfer-job-percent job) nil
          (remote-transfer-job-rate job) nil
          (remote-transfer-job-eta job) nil
          (remote-transfer-job-pending job) nil
          (remote-transfer-job-exit-status job) nil
          (remote-transfer-job-finished job) nil
          (remote-transfer-job-started job) (float-time))
    (remote-transfer--log job (format "%s\n\n" (remote-transfer-job-name job)))
    (condition-case err
        (setq process
              (if (eq (remote-transfer-job-execution job) 'target)
                  (remote-exec-async
                   (car (remote-transfer-job-command job))
                   :args (cdr (remote-transfer-job-command job))
                   :context (remote-transfer-job-destination job)
                   :name (format "remote-file-operation-%d"
                                 (remote-transfer-job-id job))
                   :coding 'utf-8
                   :filesystem-effects 'content
                   :callback (lambda (result)
                               (remote-transfer--target-finished job result)))
                (let ((process-environment
                       (remote-transfer--client-process-environment
                        (remote-transfer-job-environment job)))
                      (exec-path (remote-client-exec-path))
                      (default-directory temporary-file-directory))
                  (make-process
                   :name (format "remote-transfer-%d"
                                 (remote-transfer-job-id job))
                   :buffer nil
                   :command (remote-transfer-job-command job)
                   :connection-type 'pipe
                   :coding 'utf-8
                   :filter (lambda (_process output)
                             (remote-transfer--filter job output))
                   :sentinel (lambda (process _event)
                               (remote-transfer--sentinel job process))))))
      (error
       (kill-buffer log)
       (signal (car err) (cdr err))))
    ;; An explicit disconnect ends every process carrying the target's route.
    (when-let* ((route (remote-transfer-job-route job)))
      (process-put process 'remote-route route))
    (setf (remote-transfer-job-process job) process
          (remote-transfer-job-transport-process job)
          (when (eq (remote-transfer-job-execution job) 'target)
            (ignore-errors
              (remote-fs-route-transport-process
               (remote-transfer-job-destination job)
               (process-get process 'remote-route)))))
    (push job remote-transfer-jobs)
    (remote-transfer--start-refresh)
    job))

(cl-defun remote-transfer-start
    (sources destination &key contents delete dry-run name callback)
  "Start copying SOURCES to DESTINATION without waiting, and return the job.
SOURCES are logical file names on one target; DESTINATION is a logical
directory.  CONTENTS, DELETE and DRY-RUN are as in `remote-transfer-command'.
NAME labels the job in the queue.  CALLBACK receives the job when it ends."
  (pcase-let ((`(,command ,environment ,route ,target-ids)
               (remote-transfer-command
                sources destination
                :contents contents :delete delete :dry-run dry-run)))
    (remote-transfer--launch
     (remote-transfer-job--create
      :id (cl-incf remote-transfer--next-id)
      :name (or name
                (format "%s -> %s"
                        (mapconcat #'file-name-nondirectory
                                   (mapcar #'directory-file-name sources) ", ")
                        destination))
      :kind (cond (dry-run 'preview) (contents 'sync) (t 'copy))
      :sources sources :destination destination
      :target-ids target-ids :command command
      :environment environment :route route :callback callback))))

(cl-defun remote-transfer-plan-file-operation
    (method sources destination &key overwrite)
  "Preflight METHOD on SOURCES into DESTINATION and return a reusable plan.
OVERWRITE is nil, `ask' or t.  No target process starts before this returns."
  (let* ((destination (remote-canonicalize-file-name destination))
         (sources (mapcar #'remote-canonicalize-file-name sources))
         (command (remote-transfer-file-operation-command
                   method sources destination :overwrite overwrite)))
    (remote-file-operation-plan--create
     :method method :sources sources :destination destination
     :overwrite overwrite :command command
     :target-id (remote-file-name-target destination))))

(cl-defun remote-transfer-start-file-operation (plan &key name callback)
  "Launch preflighted target-side file operation PLAN as a queued job.
The same routed process, cancellation, disconnect and log lifecycle applies
to native and remote targets.  CALLBACK receives the finished job."
  (unless (remote-file-operation-plan-p plan)
    (error "Expected a preflighted remote file operation plan"))
  (let ((method (remote-file-operation-plan-method plan))
        (sources (remote-file-operation-plan-sources plan))
        (destination (remote-file-operation-plan-destination plan)))
    (remote-transfer--launch
     (remote-transfer-job--create
      :id (cl-incf remote-transfer--next-id)
      :name (or name
                (format "%s %s -> %s"
                        method
                        (mapconcat
                         (lambda (source)
                           (file-name-nondirectory
                            (directory-file-name source)))
                         sources ", ")
                        destination))
      :kind method :execution 'target
      :overwrite (remote-file-operation-plan-overwrite plan)
      :sources sources :destination destination
      :target-ids (list (remote-file-operation-plan-target-id plan))
      :command (remote-file-operation-plan-command plan)
      :callback callback))))

(defun remote-transfer-cancel (job)
  "Stop JOB if it is running.  A transfer may leave a partial file."
  (when-let* ((process (remote-transfer-job-process job))
              ((process-live-p process)))
    (setf (remote-transfer-job-state job) 'cancelled)
    (interrupt-process process)
    ;; rsync normally exits on the interrupt; do not leave it if it does not.
    (unless (timerp (remote-transfer-job-kill-timer job))
      (setf (remote-transfer-job-kill-timer job)
            (run-at-time 3 nil
                         (lambda ()
                           (setf (remote-transfer-job-kill-timer job) nil)
                           (when (process-live-p process)
                             (delete-process process))))))))

(defun remote-transfer-retry (job)
  "Retry JOB as a new queue entry and return it.
Target-side operations are preflighted again against the current filesystem."
  (when (eq (remote-transfer-job-state job) 'running)
    (user-error "Transfer is still running"))
  (if (eq (remote-transfer-job-execution job) 'target)
      (remote-transfer-start-file-operation
       (remote-transfer-plan-file-operation
        (remote-transfer-job-kind job)
        (remote-transfer-job-sources job)
        (remote-transfer-job-destination job)
        :overwrite (remote-transfer-job-overwrite job))
       :name (remote-transfer-job-name job)
       :callback (remote-transfer-job-callback job))
    (let ((copy (copy-remote-transfer-job job)))
      (setf (remote-transfer-job-id copy) (cl-incf remote-transfer--next-id)
            (remote-transfer-job-kill-timer copy) nil)
      (remote-transfer--launch copy))))

(defun remote-transfer--target-disconnected (target-id _reason)
  "End the transfers that belong to TARGET-ID."
  (dolist (job (remote-transfer-jobs-for-target target-id t))
    (remote-transfer-cancel job)))

(add-hook 'remote-target-disconnect-hook
          #'remote-transfer--target-disconnected)

;;; Progress display

(defun remote-transfer--transport-lost (job)
  "Finish JOB when its target transport vanished before reporting an outcome."
  (unless (remote-transfer-job-finished job)
    (let ((relay (remote-transfer-job-process job)))
      (remote-transfer--log
       job "Target transport lost; remote command outcome is unknown.\n")
      (remote-transfer--invalidate-target-cache
       job (and relay (process-get relay 'remote-route)))
      (remote-transfer--complete job nil 'failed)
      (when (and relay (process-live-p relay))
        (ignore-errors (delete-process relay))))))

(defun remote-transfer--refresh ()
  "Redraw visible frame queue views and stop the timer once nothing runs."
  (dolist (job (remote-transfer--running))
    (when-let* ((transport (remote-transfer-job-transport-process job))
                ((not (process-live-p transport))))
      (remote-transfer--transport-lost job)))
  (dolist (frame (frame-list))
    (when-let* ((buffer (frame-parameter frame
                                         'remote-transfer-queue-buffer))
                ((buffer-live-p buffer))
                ((get-buffer-window buffer frame)))
      (with-current-buffer buffer
        (remote-transfer-list-refresh))))
  (unless (remote-transfer--running)
    (when (timerp remote-transfer--refresh-timer)
      (cancel-timer remote-transfer--refresh-timer))
    (setq remote-transfer--refresh-timer nil)))

(defun remote-transfer--start-refresh ()
  "Redraw progress periodically while a transfer runs."
  (unless (timerp remote-transfer--refresh-timer)
    (setq remote-transfer--refresh-timer
          (run-with-timer 0.5 0.5 #'remote-transfer--refresh)))
  (remote-transfer--refresh))

(defun remote-transfer--bar (percent)
  "Return a text progress bar for PERCENT."
  (if (numberp percent)
      (let ((filled (/ (* (min 100 (max 0 percent)) 16) 100)))
        (format "%s%s %3d%%"
                (make-string filled ?#) (make-string (- 16 filled) ?.)
                percent))
    ""))

(defun remote-transfer--entries ()
  "Return `tabulated-list-entries' for the queue."
  (mapcar
   (lambda (job)
     (list
      job
      (vector
       (propertize
        (remote-transfer--state-text job)
        'face (pcase (remote-transfer-job-state job)
                ('done 'success)
                ('failed 'error)
                ('cancelled 'warning)
                (_ 'default)))
       (remote-transfer--bar (remote-transfer-job-percent job))
       (if (eq (remote-transfer-job-state job) 'running)
           (or (remote-transfer-job-rate job) "")
         "")
       (if (eq (remote-transfer-job-state job) 'running)
           (or (remote-transfer-job-eta job) "")
         (format-seconds
          "%mm%02ss"
          (- (or (remote-transfer-job-finished job) (float-time))
             (remote-transfer-job-started job))))
       (remote-transfer-job-name job))))
   remote-transfer-jobs))

(defun remote-transfer-list-refresh ()
  "Redraw the transfer queue."
  (interactive)
  (when (derived-mode-p 'remote-transfer-list-mode)
    (setq tabulated-list-entries (remote-transfer--entries))
    (tabulated-list-print t)))

(defun remote-transfer--job-at-point ()
  "Return the transfer on the current row."
  (or (tabulated-list-get-id) (user-error "No transfer on this row")))

(defun remote-transfer-list-cancel ()
  "Cancel the transfer at point."
  (interactive)
  (remote-transfer-cancel (remote-transfer--job-at-point))
  (remote-transfer-list-refresh))

(defun remote-transfer-list-retry ()
  "Run the transfer at point again."
  (interactive)
  (remote-transfer-retry (remote-transfer--job-at-point))
  (remote-transfer-list-refresh))

(defun remote-transfer-show-log (job)
  "Show the tool output of JOB."
  (interactive (list (remote-transfer--job-at-point)))
  (let ((buffer (remote-transfer-job-log-buffer job)))
    (unless (buffer-live-p buffer)
      (user-error "This transfer's log is gone"))
    (pop-to-buffer buffer '(nil (reusable-frames . nil)))))

(defun remote-transfer-list-clear ()
  "Remove finished transfers and their logs from the queue."
  (interactive)
  (dolist (job remote-transfer-jobs)
    (unless (eq (remote-transfer-job-state job) 'running)
      (when (buffer-live-p (remote-transfer-job-log-buffer job))
        (quit-windows-on (remote-transfer-job-log-buffer job) t))))
  (setq remote-transfer-jobs (remote-transfer--running))
  (remote-transfer--refresh))

(defvar-keymap remote-transfer-list-mode-map
  :parent tabulated-list-mode-map
  "q" #'quit-window
  "RET" #'remote-transfer-show-log
  "l" #'remote-transfer-show-log
  "k" #'remote-transfer-list-cancel
  "r" #'remote-transfer-list-retry
  "c" #'remote-transfer-list-clear
  "g" #'remote-transfer-list-refresh)

(define-derived-mode remote-transfer-list-mode tabulated-list-mode "Transfers"
  "Mode for the file job queue.
RET shows a transfer's output, k cancels it, r runs it again and c clears
the finished ones."
  (setq tabulated-list-format
        [("State" 12 nil)
         ("Progress" 22 nil)
         ("Rate" 12 nil)
         ("Time" 9 nil)
         ("Operation" 60 nil)])
  (setq tabulated-list-padding 1
        tabulated-list-entries (remote-transfer--entries))
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'remote-transfer--queue-buffer-killed nil t))

(defun remote-transfer--queue-buffer-killed ()
  "Clear this queue buffer's frame reference when it is killed."
  (when (and (frame-live-p remote-transfer--queue-owner-frame)
             (eq (frame-parameter remote-transfer--queue-owner-frame
                                  'remote-transfer-queue-buffer)
                 (current-buffer)))
    (set-frame-parameter remote-transfer--queue-owner-frame
                         'remote-transfer-queue-buffer nil)))

(defun remote-transfer--frame-deleted (frame)
  "Release FRAME's transfer queue view, leaving the jobs running."
  (when-let* ((buffer (frame-parameter frame
                                       'remote-transfer-queue-buffer)))
    (set-frame-parameter frame 'remote-transfer-queue-buffer nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(add-hook 'delete-frame-functions #'remote-transfer--frame-deleted)

(defun remote-transfer-list-buffer (&optional frame)
  "Return FRAME's transfer queue view, creating it when needed."
  (let* ((frame (or frame (selected-frame)))
         (existing (frame-parameter frame 'remote-transfer-queue-buffer))
         (created (not (buffer-live-p existing)))
         (buffer (if created
                     (generate-new-buffer "*Remote Transfers*")
                   existing)))
    (when created
      (set-frame-parameter frame 'remote-transfer-queue-buffer buffer))
    (condition-case error
        (with-current-buffer buffer
          (unless (derived-mode-p 'remote-transfer-list-mode)
            (remote-transfer-list-mode))
          (setq-local remote-transfer--queue-owner-frame frame)
          (remote-transfer-list-refresh))
      (error
       (when created
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when (eq (frame-parameter frame 'remote-transfer-queue-buffer)
                   buffer)
           (set-frame-parameter frame 'remote-transfer-queue-buffer nil)))
       (signal (car error) (cdr error))))
    buffer))

;;;###autoload
(defun remote-transfer-list ()
  "Show the transfer and target file operation queue."
  (interactive)
  (pop-to-buffer (remote-transfer-list-buffer)
                 '(nil (reusable-frames . nil))))

(provide 'remote-transfer)
;;; remote-transfer.el ends here
