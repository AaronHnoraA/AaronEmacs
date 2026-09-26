;;; remote-task-live-smoke.el --- Opt-in real target task check -*- lexical-binding: t; -*-

;;; Commentary:
;; REMOTE_TASK_E2E_TARGET=host make remote-task-live-smoke
;; REMOTE_TASK_E2E_ROOT=/existing/folder/ overrides the default /tmp/ root.
;; REMOTE_TASK_E2E_SOURCE=/existing/file verifies absolute error navigation.
;; The probe runs commands but does not write target files.

;;; Code:

(require 'remote-config)
(require 'remote-framework)

(defun remote-task-live-smoke--wait (task timeout)
  "Wait up to TIMEOUT seconds for TASK to stop running."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (eq (remote-task-state task) 'running)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (eq (remote-task-state task) 'running)
      task)))

(defun remote-task-live-smoke--target-exited (workspace pid)
  "Check that target PID ended or is a zombie; clean up a live survivor."
  (zerop
   (remote-exec-result-status
    (remote-exec
     "sh" :args
     (list "-c"
           (format
            "for i in 1 2 3 4 5 6 7 8 9 10; do kill -0 %s 2>/dev/null || exit 0; case \"$(ps -o stat= -p %s 2>/dev/null)\" in *Z*|'') exit 0;; esac; sleep 0.1; done; kill %s 2>/dev/null; exit 23"
            pid pid pid))
     :context (remote-workspace-context workspace)))))

(defun remote-task-live-smoke--buffer-pid (task label)
  "Read LABEL's decimal PID from TASK's Compilation output."
  (with-current-buffer (remote-task-buffer task)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "%s=\\([0-9]+\\)" label) nil t)
        (match-string-no-properties 1)))))

(defun remote-task-live-smoke-run ()
  "Check task output, failure, cancellation, and workspace ownership."
  (remote-config-load)
  (remote-fs-install)
  (let* ((target (or (getenv "REMOTE_TASK_E2E_TARGET")
                     (error "Set REMOTE_TASK_E2E_TARGET")))
         (root (or (getenv "REMOTE_TASK_E2E_ROOT") "/tmp/"))
         (source (getenv "REMOTE_TASK_E2E_SOURCE"))
         (logical (file-name-as-directory
                   (remote-make-file-name target root)))
         (workspace (remote-workspace-open
                     logical :adapter "process"
                     :capability 'process-async))
         successful failed sleeping immediate children)
    (unwind-protect
        (progn
          (setq successful
                (remote-task-run
                 (list "sh" "-c"
                       "printf 'cwd=%s\\n' \"$PWD\"; printf 'token=%s\\n' \"$REMOTE_TASK_TOKEN\"; if [ -n \"$REMOTE_TASK_SOURCE\" ]; then printf '%s:1:1: error: task link\\n' \"$REMOTE_TASK_SOURCE\"; fi")
                 :workspace workspace
                 :environment
                 (list (cons "REMOTE_TASK_TOKEN" "routed")
                       (cons "REMOTE_TASK_SOURCE" (or source "")))))
          (unless (remote-task-live-smoke--wait successful 25)
            (error "Task success probe timed out"))
          (unless (and (eq (remote-task-state successful) 'succeeded)
                       (equal (remote-route-target-id
                               (process-get (remote-task-process successful)
                                            'remote-route))
                              target)
                       (seq-some
                        (lambda (route)
                          (equal (remote-connection-route-key route)
                                 (remote-connection-route-key
                                  (process-get
                                   (remote-task-process successful)
                                   'remote-route))))
                        (remote-workspace-routes workspace))
                       (with-current-buffer (remote-task-buffer successful)
                         (and (save-excursion
                                (goto-char (point-min))
                                (search-forward "token=routed" nil t))
                              (save-excursion
                                (goto-char (point-min))
                                (search-forward
                                 (format "cwd=%s" (directory-file-name root))
                                 nil t)))))
            (error "Task routed output or cwd was wrong: %S"
                   (remote-task-state successful)))
          (when source
            (with-current-buffer (remote-task-buffer successful)
              (goto-char (point-min))
              (next-error 1 t))
            (let* ((logical-source (remote-make-file-name target source))
                   (visited
                    (seq-find
                     (lambda (buffer)
                       (equal (buffer-file-name buffer) logical-source))
                     (buffer-list))))
              (unless visited
                (error "Task error opened the wrong source file: %s"
                       logical-source))
              (kill-buffer visited)))
          (setq failed
                (remote-task-run (list "sh" "-c" "exit 7")
                                 :workspace workspace))
          (unless (remote-task-live-smoke--wait failed 25)
            (error "Task failure probe timed out"))
          (unless (and (eq (remote-task-state failed) 'failed)
                       (= (remote-task-exit-code failed) 7))
            (error "Task failure status was wrong: %S"
                   (remote-task-state failed)))
          (setq immediate
                (remote-task-run (list "sleep" "30")
                                 :workspace workspace))
          (remote-task-cancel immediate)
          (let ((deadline (+ (float-time) 8)))
            (while (and (process-live-p (remote-task-process immediate))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (unless (and (eq (remote-task-state immediate) 'cancelled)
                       (not (process-live-p (remote-task-process immediate)))
                       (remote-task-target-pid immediate)
                       (remote-task-live-smoke--target-exited
                        workspace (remote-task-target-pid immediate)))
            (error "Immediate cancellation did not end the target task"))
          (setq children
                (remote-task-run
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf 'child_pid=%s\\n' \"$child\"; wait \"$child\"")
                 :workspace workspace))
          (let ((deadline (+ (float-time) 8)))
            (while (and (not (remote-task-live-smoke--buffer-pid
                              children "child_pid"))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (let ((child-pid
                 (or (remote-task-live-smoke--buffer-pid children "child_pid")
                     (error "Task never reported its child PID"))))
            (unless (remote-task-target-process-group children)
              (error "Target has no setsid process group for child cleanup"))
            (remote-task-cancel children)
            (unless (and (remote-task-live-smoke--target-exited
                          workspace (remote-task-target-pid children))
                         (remote-task-live-smoke--target-exited
                          workspace child-pid))
              (error "Cancellation left a target child running: %s" child-pid)))
          (setq sleeping
                (remote-task-run
                 (list "sh" "-c"
                       "printf 'task_pid=%s\\n' \"$$\"; exec sleep 30")
                 :workspace workspace))
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p (remote-task-process sleeping))
                        (< (float-time) deadline)
                        (not (with-current-buffer
                                 (remote-task-buffer sleeping)
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "task_pid=[0-9]+" nil t)))))
              (accept-process-output nil 0.05)))
          (let ((pid
                 (with-current-buffer (remote-task-buffer sleeping)
                   (save-excursion
                     (goto-char (point-min))
                     (unless (re-search-forward "task_pid=\\([0-9]+\\)" nil t)
                       (error "Task never reported its target PID"))
                     (match-string-no-properties 1)))))
            (remote-workspace-close workspace 'task-live-smoke)
            (unless (remote-task-live-smoke--target-exited workspace pid)
              (error "Cancelled task remained alive on target: %s" pid)))
          (let ((deadline (+ (float-time) 8)))
            (while (and (process-live-p (remote-task-process sleeping))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (unless (and (eq (remote-task-state sleeping) 'cancelled)
                       (not (process-live-p (remote-task-process sleeping))))
            (error "Workspace close did not cancel task"))
          (princ
           (format "task target=%s backend=%s success=%s failure=%s cancel=%s\n"
                   target
                   (remote-route-backend-id
                    (process-get (remote-task-process successful)
                                 'remote-route))
                   (remote-task-state successful)
                   (remote-task-exit-code failed)
                   (remote-task-state sleeping))))
      (when (remote-workspace-live-p workspace)
        (remote-workspace-close workspace 'task-live-cleanup))
      (dolist (task (list successful failed sleeping immediate children))
        (when (and task (buffer-live-p (remote-task-buffer task)))
          (kill-buffer (remote-task-buffer task)))))))

(remote-task-live-smoke-run)

(provide 'remote-task-live-smoke)
;;; remote-task-live-smoke.el ends here
