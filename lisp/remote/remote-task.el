;;; remote-task.el --- Workspace-owned build and test commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Tasks run on the owning target through the same process route as LSP and
;; terminals.  Output uses Compilation mode so file/line errors remain usable.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-workspace)

(declare-function compilation-filter "compile" (process string))
(declare-function compilation-mode "compile" ())
(defvar compilation-in-progress)
(defvar compilation-mode-line-errors)
(defvar compilation-finish-functions)
(defvar compilation-parse-errors-filename-function)
(defvar next-error-last-buffer)

(cl-defstruct (remote-task (:constructor remote-task-create))
  id name workspace-id context command environment directory buffer process
  state exit-code
  started-at finished-at resource target-pid output-prefix cancel-timer
  cancel-target-allowed target-process-group transport-interrupted
  cancel-signal-timer cancel-signal-process target-cancel-confirmed)

(cl-defstruct (remote-task-profile
               (:constructor remote-task-profile-create))
  id name command environment directory)

(defvar remote-tasks (make-hash-table :test #'equal)
  "Tasks, including completed runs, keyed by unique run ID.")

(defvar remote-task-profiles (make-hash-table :test #'equal)
  "Named task definitions shared by local and remote workspaces.")

(defvar remote-task--counter 0)

(defcustom remote-task-cancel-pid-wait 5
  "Seconds to await the target PID after an immediate task cancellation.
The wait uses a timer and never blocks the command loop."
  :type 'number
  :group 'remote)

(defcustom remote-task-cancel-signal-wait 5
  "Seconds to await target-side TERM confirmation before closing its relay.
The check runs asynchronously on the target, so waiting does not block Emacs."
  :type 'number
  :group 'remote)

(defvar-local remote-task-instance nil
  "Task whose output is displayed in the current compilation buffer.")

(cl-defun remote-task-register
    (id command &key name environment directory)
  "Register a reusable task ID with target COMMAND.
COMMAND is a nonempty argv list; the task wrapper executes it as argv without
shell expansion.  The same definition can run against any workspace target.
DIRECTORY, if supplied,
must be a logical path owned by the selected workspace."
  (unless (and (listp command) command
               (seq-every-p (lambda (item)
                              (and (stringp item)
                                   (not (string-empty-p item))))
                            command))
    (error "Task command must be a nonempty argv list: %S" command))
  (let* ((id (remote-normalize-id id))
         (profile
          (remote-task-profile-create
           :id id :name (or name id)
           :command (copy-sequence command)
           :environment (copy-tree environment)
           :directory directory)))
    (puthash id profile remote-task-profiles)
    profile))

(defun remote-task--workspace (value)
  "Resolve VALUE to an open workspace without changing an existing owner."
  (or (and value (remote-get-workspace value))
      (let ((context
             (or value
                 (and (file-remote-p default-directory)
                      default-directory)
                 remote-current-workspace
                 (remote-make-file-name "local" default-directory))))
        (remote-workspace-open
         context :adapter "process" :capability 'process-async
         :load-environment t))))

(defun remote-task--directory (workspace requested)
  "Return a WORKSPACE-owned directory for REQUESTED."
  (let* ((root (file-name-as-directory
                (remote-workspace-root workspace)))
         (current
          (and (not requested)
               (file-remote-p default-directory)
               (remote-canonicalize-file-name default-directory)))
         (directory
          (file-name-as-directory
           (remote-canonicalize-file-name
            (or requested
                (and current
                     (equal (remote-file-name-target current)
                            (remote-workspace-target-id workspace))
                     (string-prefix-p root
                                      (file-name-as-directory current))
                     current)
                root)))))
    (unless (and (equal (remote-file-name-target directory)
                        (remote-workspace-target-id workspace))
                 (string-prefix-p root directory))
      (error "Task directory %s is outside workspace %s"
             directory root))
    directory))

(defun remote-task--logical-output-file (filename)
  "Map target-absolute FILENAME in this task's output to `/fs:' identity.
Relative names already resolve against the compilation buffer's logical
`default-directory'.  Explicit logical or other remote names stay intact."
  (if (and (stringp filename)
           (file-name-absolute-p filename)
           (not (file-remote-p filename))
           (remote-task-p remote-task-instance))
      (remote-make-file-name
       (remote-file-name-target
        (remote-task-directory remote-task-instance))
       filename)
    filename))

(defun remote-task--filter (task process output)
  "Capture TASK's target PID frame, then stream OUTPUT to Compilation mode."
  (if (remote-task-target-pid task)
      (compilation-filter process output)
    (let* ((pending (concat (or (remote-task-output-prefix task) "") output))
           (marker
            (concat "\036EMACS_REMOTE_TASK_PID:"
                    (regexp-quote (remote-task-id task))
                    ":\\([0-9]+\\):\\([01]\\)\036\n"))
           (match (string-match marker pending)))
      (cond
       (match
        (let ((pid (string-to-number (match-string 1 pending)))
              (group-p (equal (match-string 2 pending) "1"))
              (before (substring pending 0 match))
              (after (substring pending (match-end 0))))
          (setf (remote-task-target-pid task) pid
                (remote-task-target-process-group task) group-p
                (remote-task-output-prefix task) nil)
          (unless (string-empty-p before)
            (compilation-filter process before))
          (unless (string-empty-p after)
            (compilation-filter process after))
          (when (eq (remote-task-state task) 'cancelled)
            (run-at-time 0 nil #'remote-task--finish-cancel task))))
       ((> (length pending) 512)
        ;; A future backend may transform the framing.  Keep the user's
        ;; output visible even if the PID frame cannot be decoded.
        (setf (remote-task-output-prefix task) nil)
        (compilation-filter process pending))
       (t
        (setf (remote-task-output-prefix task) pending))))))

(defun remote-task--close-cancel-relay (task)
  "Finish TASK's cancellation after target confirmation or timeout."
  (when-let* ((timer (remote-task-cancel-signal-timer task)))
    (cancel-timer timer)
    (setf (remote-task-cancel-signal-timer task) nil))
  (setf (remote-task-cancel-signal-process task) nil)
  (when (and (memq (remote-task-state task)
                   '(cancelled cancel-unconfirmed))
             (process-live-p (remote-task-process task)))
    (delete-process (remote-task-process task))))

(defun remote-task--mark-cancel-unconfirmed (task)
  "Show that TASK's target-side cancellation could not be verified."
  (unless (eq (remote-task-state task) 'cancel-unconfirmed)
    (setf (remote-task-state task) 'cancel-unconfirmed)
    (when-let* ((buffer (remote-task-buffer task)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\nCancellation unconfirmed: target may still run.\n")
            (setq mode-line-process
                  '((:propertize ":cancel?"
                                  face compilation-mode-line-fail)
                    compilation-mode-line-errors))
            (force-mode-line-update))))))
  task)

(defun remote-task--target-cancel-complete (task result)
  "Record target-side TERM RESULT before closing TASK's local relay."
  (setf (remote-task-target-cancel-confirmed task)
        (and (eq (remote-task-state task) 'cancelled)
             (zerop (remote-exec-result-status result))))
  (unless (remote-task-target-cancel-confirmed task)
    (remote-task--mark-cancel-unconfirmed task)
    (remote-log
     'task-target-cancel-error
     :task (remote-task-id task)
     :status (remote-exec-result-status result)))
  (remote-task--close-cancel-relay task))

(defun remote-task--cancel-signal-expired (task)
  "Bound a stalled target-side cancellation for TASK."
  (remote-task--mark-cancel-unconfirmed task)
  (remote-log 'task-cancel-unconfirmed :task (remote-task-id task))
  (when-let* ((sidecar (remote-task-cancel-signal-process task)))
    (when (process-live-p sidecar)
      (delete-process sidecar)))
  (remote-task--close-cancel-relay task))

(defun remote-task--terminate-target (task)
  "Asynchronously terminate TASK's target process or group and confirm exit.
Closing a local SSH relay does not reliably stop its remote child.  The
target-side shell first reports its PID, then replaces itself with the argv
command.  Where `setsid -w' is available, the PID also owns a process group and
cancellation signals that group so child processes do not remain behind."
  (when-let* ((pid (remote-task-target-pid task))
              ((remote-task-cancel-target-allowed task)))
    (let* ((command
            (list
             "sh" "-c"
             "if [ \"$2\" = 1 ]; then kill -TERM \"-$1\" 2>/dev/null || kill -TERM \"$1\" 2>/dev/null; else kill -TERM \"$1\" 2>/dev/null; fi; for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do kill -0 \"$1\" 2>/dev/null || exit 0; case \"$(ps -o stat= -p \"$1\" 2>/dev/null)\" in *Z*|'') exit 0;; esac; sleep 0.1; done; exit 23"
             "emacs-remote-task-kill" (number-to-string pid)
             (if (remote-task-target-process-group task) "1" "0")))
           (route (process-get (remote-task-process task) 'remote-route)))
      (condition-case error
          (let ((sidecar
                 (remote-exec-async
                  (car command) :args (cdr command)
                  :adapter "process"
                  :link (and route (remote-route-pipeline-id route))
                  :context (remote-task-context task)
                  :callback
                  (lambda (result)
                    (remote-task--target-cancel-complete task result)))))
            ;; A backend can invoke the callback before returning its process.
            (when (remote-task-cancel-signal-timer task)
              (setf (remote-task-cancel-signal-process task) sidecar)))
      (error
       (remote-log
        'task-target-cancel-error
        :task (remote-task-id task)
        :error (error-message-string error))
       (remote-task--mark-cancel-unconfirmed task)
       (remote-task--close-cancel-relay task))))))

(defun remote-task--finish-cancel (task)
  "Finish a pending cancellation after TASK's PID arrives or its timer ends."
  (when-let* ((timer (remote-task-cancel-timer task)))
    (cancel-timer timer)
    (setf (remote-task-cancel-timer task) nil))
  (when (eq (remote-task-state task) 'cancelled)
    (if (and (remote-task-target-pid task)
             (remote-task-cancel-target-allowed task))
        (unless (remote-task-cancel-signal-timer task)
          (setf (remote-task-cancel-signal-timer task)
                (run-at-time remote-task-cancel-signal-wait nil
                             #'remote-task--cancel-signal-expired task))
          (remote-task--terminate-target task))
      (remote-task--mark-cancel-unconfirmed task)
      (remote-log 'task-cancel-unconfirmed :task (remote-task-id task))
      (remote-task--close-cancel-relay task))))

(defun remote-task--transport-failed (workspace route _error)
  "Record ROUTE loss on running tasks owned by WORKSPACE.
The task process may still be live while its workspace reconnects.  Its
sentinel must not turn a relay's synthetic exit zero into task success."
  (dolist (resource (remote-workspace-resources workspace))
    (when (eq (remote-workspace-resource-kind resource) 'task)
      (let* ((task (remote-workspace-resource-value resource))
             (process (and (remote-task-p task)
                           (remote-task-process task)))
             (task-route (and process
                              (process-get process 'remote-route))))
        (when (and (remote-task-p task)
                   (eq (remote-task-state task) 'running)
                   (remote-route-p task-route)
                   (equal (remote-pipeline-route-key task-route)
                          (remote-pipeline-route-key route)))
          (setf (remote-task-transport-interrupted task) t))))))

(defun remote-task--finished (task process event)
  "Record PROCESS's terminal EVENT and finish TASK's compilation buffer."
  (when (and (remote-task-p task)
             (eq process (remote-task-process task))
             (not (process-get process 'remote-task-sentinel-handled))
             (memq (process-status process) '(exit signal failed closed)))
    (process-put process 'remote-task-sentinel-handled t)
    (when-let* ((timer (remote-task-cancel-timer task)))
      (cancel-timer timer)
      (setf (remote-task-cancel-timer task) nil))
    (when-let* ((pending (remote-task-output-prefix task)))
      (setf (remote-task-output-prefix task) nil)
      (unless (string-empty-p pending)
        (compilation-filter process pending)))
    (unless (remote-task-finished-at task)
      (let* ((workspace
              (remote-get-workspace (remote-task-workspace-id task)))
             (transport-lost
              (or (remote-task-transport-interrupted task)
                  (and workspace
                       (memq (remote-workspace-state workspace)
                             '(disconnected reconnecting failed))))))
        (setf (remote-task-finished-at task) (current-time)
              (remote-task-exit-code task)
              (unless (or transport-lost
                          (memq (remote-task-state task)
                                '(cancelled cancel-unconfirmed)))
                (process-exit-status process)))
        ;; A deleted local relay can report an ordinary exit or signal even
        ;; when the target-side TERM was never confirmed.  Keep the more
        ;; precise cancellation state until the user explicitly reruns it.
        (unless (memq (remote-task-state task)
                      '(cancelled cancel-unconfirmed))
          (setf (remote-task-state task)
                (cond
                 (transport-lost 'interrupted)
                 ((and (eq (process-status process) 'exit)
                       (zerop (process-exit-status process)))
                  'succeeded)
                 (t 'failed))))
        (when (eq (remote-task-state task) 'interrupted)
          (remote-log 'task-interrupted :task (remote-task-id task))))
      (when-let* ((workspace
                   (remote-get-workspace (remote-task-workspace-id task)))
                  (resource (remote-task-resource task)))
        (remote-workspace-forget-resource workspace resource)
        (setf (remote-task-resource task) nil)))
    (when-let* ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (status (remote-task-state task)))
            (goto-char (point-max))
            (insert
             (format "\nTask %s: %s (%.2f s)\n"
                     (remote-task-name task)
                     (pcase status
                       ('failed
                        (format "failed, exit %s"
                                (remote-task-exit-code task)))
                       ('interrupted
                        "interrupted, remote result unknown")
                       ('cancel-unconfirmed
                        "cancellation unconfirmed, target may still run")
                       (_ status))
                     (- (float-time)
                        (float-time (remote-task-started-at task)))))
            (setq mode-line-process
                  (list
                   (propertize
                    (format ":%s" status)
                    'face
                    (if (memq status
                              '(failed interrupted cancel-unconfirmed))
                        'compilation-mode-line-fail
                      'compilation-mode-line-exit))
                   compilation-mode-line-errors))
            (force-mode-line-update)
            (run-hook-with-args
             'compilation-finish-functions buffer event)))))
    (setq compilation-in-progress
          (delq process compilation-in-progress))
    (delete-process process)))

(defun remote-task-cancel (task &optional _reason)
  "Cancel TASK's live process and retain its output for inspection."
  (interactive
   (list
    (or remote-task-instance
        (let* ((running (seq-filter
                         (lambda (item)
                           (eq (remote-task-state item) 'running))
                         (hash-table-values remote-tasks)))
               (name (completing-read
                      "Cancel task: "
                      (mapcar #'remote-task-id running) nil t)))
          (gethash name remote-tasks)))))
  (unless (remote-task-p task)
    (error "No task to cancel"))
  (when (eq (remote-task-state task) 'running)
    (let ((workspace
           (remote-get-workspace (remote-task-workspace-id task))))
      (setf (remote-task-state task) 'cancelled
            (remote-task-finished-at task) (current-time)
            (remote-task-cancel-target-allowed task)
            (and workspace
                 (memq (remote-workspace-state workspace)
                       '(open closing)))))
    (if (or (remote-task-target-pid task)
            (not (process-live-p (remote-task-process task))))
        (remote-task--finish-cancel task)
      (setf (remote-task-cancel-timer task)
            (run-at-time
             remote-task-cancel-pid-wait nil
             #'remote-task--finish-cancel task)))
    (when-let* ((workspace
                 (remote-get-workspace (remote-task-workspace-id task)))
                (resource (remote-task-resource task)))
      (remote-workspace-forget-resource workspace resource)
      (setf (remote-task-resource task) nil)))
  task)

(defun remote-task--buffer-killed ()
  "Stop a running task before its output buffer disappears."
  (when (remote-task-p remote-task-instance)
    (when (eq (remote-task-state remote-task-instance) 'running)
      (remote-task-cancel remote-task-instance 'buffer-killed))
    (remhash (remote-task-id remote-task-instance) remote-tasks)))

(defun remote-task-rerun (&optional task)
  "Run TASK again with its original command, directory, and environment.
Interactively, use the task displayed in the current Compilation buffer.
An interrupted task is never replayed automatically after reconnection."
  (interactive)
  (setq task (or task remote-task-instance))
  (unless (remote-task-p task)
    (user-error "No Remote task in this buffer"))
  (when (eq (remote-task-state task) 'running)
    (user-error "Task is still running; cancel it first"))
  (let ((workspace
         (remote-get-workspace (remote-task-workspace-id task))))
    (unless (and workspace (remote-workspace-live-p workspace))
      (user-error "Task workspace is not connected; reconnect it first"))
    (remote-task-run
     (remote-task-command task)
     :workspace workspace
     :name (remote-task-name task)
     :directory (remote-task-directory task)
     :environment (copy-tree (remote-task-environment task))
     :display t)))

(cl-defun remote-task-run
    (command &key workspace name directory environment display)
  "Run target argv COMMAND asynchronously in WORKSPACE.
DIRECTORY defaults to the current logical directory when it belongs to the
workspace, otherwise the workspace root.  Each invocation has independent
state and a Compilation buffer.  Killing that buffer or closing its workspace
cancels the process.  DISPLAY shows the output buffer."
  (unless (and (listp command) command
               (seq-every-p (lambda (item)
                              (and (stringp item)
                                   (not (string-empty-p item))))
                            command))
    (error "Task command must be a nonempty argv list: %S" command))
  ;; Compilation is a client UI library.  Load it only for a real task and
  ;; outside the target's logical file context.
  (let ((default-directory temporary-file-directory))
    (require 'compile))
  (let* ((workspace (remote-task--workspace workspace))
         (_live (unless (remote-workspace-live-p workspace)
                  (error "Workspace is not ready for tasks: %s"
                         (remote-workspace-id workspace))))
         (directory (remote-task--directory workspace directory))
         (id (format "%s/task-%d" (remote-workspace-id workspace)
                     (cl-incf remote-task--counter)))
         (name (or name (file-name-nondirectory (car command))))
         (buffer (generate-new-buffer (format "*remote-task:%s*" name)))
         (task (remote-task-create
                :id id :name name
                :workspace-id (remote-workspace-id workspace)
                :context (remote-workspace-context workspace)
                :command (copy-sequence command)
                :environment (copy-tree environment)
                :directory directory
                :buffer buffer :state 'starting
                :started-at (current-time)))
         process)
    (condition-case error
        (progn
          (with-current-buffer buffer
            (compilation-mode)
            (local-set-key (kbd "g") #'remote-task-rerun)
            (setq default-directory directory
                  remote-task-instance task)
            (setq-local compilation-parse-errors-filename-function
                        #'remote-task--logical-output-file)
            (add-hook 'kill-buffer-hook #'remote-task--buffer-killed nil t)
            (let ((inhibit-read-only t))
              (insert (format "Workspace: %s\nDirectory: %s\nCommand: %s\n\n"
                              (remote-workspace-id workspace)
                              directory
                              (mapconcat #'shell-quote-argument command " "))))
            (setq process
                  (remote-make-process
                   :name id :buffer buffer
                   :command
                   (append
                    (list "sh" "-c"
                          "if command -v setsid >/dev/null 2>&1 && { setsid -w sh -c 'exit 7' >/dev/null 2>&1; [ \"$?\" -eq 7 ]; }; then exec setsid -w sh -c 'printf \"\\036EMACS_REMOTE_TASK_PID:%s:%s:1\\036\\n\" \"$1\" \"$$\"; shift; exec \"$@\"' emacs-remote-task \"$@\"; else printf '\\036EMACS_REMOTE_TASK_PID:%s:%s:0\\036\\n' \"$1\" \"$$\"; shift; exec \"$@\"; fi"
                          "emacs-remote-task" id)
                    command)
                   :connection-type 'pipe :coding 'utf-8-unix :noquery t
                   :remote-adapter "process"
                   :remote-preferred-route
                   (remote-workspace-primary-route workspace)
                   :remote-context (remote-workspace-context workspace)
                   :remote-directory directory
                   :remote-environment environment
                   :filter (lambda (running output)
                             (remote-task--filter task running output))
                   :sentinel (lambda (finished event)
                               (remote-task--finished task finished event))))
            (set-marker (process-mark process) (point-max) buffer)
            (setq mode-line-process
                  '((:propertize ":run" face compilation-mode-line-run)
                    compilation-mode-line-errors)))
          (setf (remote-task-process task) process
                (remote-task-state task) 'running
                (remote-task-resource task)
                (remote-workspace-register-resource
                 workspace 'task task #'remote-task-cancel
                 '(:recovery manual)))
          (when-let* ((route (process-get process 'remote-route)))
            (remote-workspace-track-live-route workspace route))
          (push process compilation-in-progress)
          (puthash id task remote-tasks)
          (setq next-error-last-buffer buffer)
          ;; A process provider may wait internally and deliver its sentinel
          ;; before `remote-make-process' returns.  Finish that fast process
          ;; after its workspace ownership and buffer state are installed.
          (when (memq (process-status process) '(exit signal failed closed))
            (remote-task--finished task process "finished\n"))
          (when display (display-buffer buffer))
          task)
      (error
       (when (and process (process-live-p process))
         (delete-process process))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (signal (car error) (cdr error))))))

(cl-defun remote-task-run-profile (id &key workspace display)
  "Run registered task ID in WORKSPACE."
  (interactive
   (list (completing-read "Task profile: "
                          (hash-table-keys remote-task-profiles) nil t)
         :workspace nil :display t))
  (let ((profile (gethash (remote-normalize-id id t)
                          remote-task-profiles)))
    (unless profile (error "Unknown task profile: %S" id))
    (remote-task-run
     (remote-task-profile-command profile)
     :workspace workspace
     :name (remote-task-profile-name profile)
     :directory (remote-task-profile-directory profile)
     :environment (remote-task-profile-environment profile)
     :display display)))

(cl-defun remote-task-run-command (&optional workspace)
  "Prompt for one shell command and run it in WORKSPACE.
The command is explicitly supplied by the user and interpreted by the
target's POSIX shell.  Use `remote-task-run' for argv-only execution."
  (interactive)
  (let* ((command (read-shell-command "Task command: "))
         (name (truncate-string-to-width command 40 nil nil "…")))
    (when (string-blank-p command)
      (user-error "Task command is empty"))
    (remote-task-run (list "sh" "-c" command)
                     :workspace workspace :name name :display t)))

(add-hook 'remote-workspace-transport-failure-hook
          #'remote-task--transport-failed)

(provide 'remote-task)
;;; remote-task.el ends here
