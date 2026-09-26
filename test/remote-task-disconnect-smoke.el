;;; remote-task-disconnect-smoke.el --- Opt-in task transport fault probe -*- lexical-binding: t; -*-

;;; Commentary:
;; REMOTE_TASK_E2E_TARGET=host REMOTE_TASK_E2E_ROOT=/existing/path/ \
;;   emacs --batch ... -l test/remote-task-disconnect-smoke.el
;; Only the RPC transport owned by this batch Emacs is stopped.  The target
;; runs a temporary sleep command and no target files are written.

;;; Code:

(require 'remote-config)
(require 'remote-framework)
(require 'tramp)
(require 'tramp-rpc)

(defun remote-task-disconnect-smoke--wait (predicate timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to become non-nil."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun remote-task-disconnect-smoke-run ()
  "Observe task exit and workspace recovery after this client's RPC dies."
  (remote-config-load)
  (remote-fs-install)
  (let* ((target (or (getenv "REMOTE_TASK_E2E_TARGET")
                     (error "Set REMOTE_TASK_E2E_TARGET")))
         (root (or (getenv "REMOTE_TASK_E2E_ROOT") "/tmp/"))
         (logical (file-name-as-directory
                   (remote-make-file-name target root)))
         (workspace (remote-workspace-open
                     logical :adapter "process" :capability 'process-async))
         (task nil)
         (rerun nil)
         (pid nil)
         (rerun-pid nil)
         (transport nil))
    (unwind-protect
        (progn
          (setq task
                (remote-task-run
                 (list "sh" "-c"
                       "printf 'task_host=%s\\n' \"$(hostname)\"; printf 'task_pid=%s\\n' \"$$\"; exec sleep 30")
                 :workspace workspace))
          (unless
              (remote-task-disconnect-smoke--wait
               (lambda () (remote-task-target-pid task)) 8)
            (error "Task did not report its target PID"))
          (setq pid (remote-task-target-pid task))
          (let* ((physical
                  (remote-project-file-name
                   logical nil 'file-read "emacs-file"))
                 (connection
                  (tramp-rpc--get-connection
                   (tramp-dissect-file-name physical))))
            (setq transport (plist-get connection :process)))
          (unless (and (processp transport)
                       (process-live-p transport)
                       (equal (remote-route-backend-id
                               (process-get (remote-task-process task)
                                            'remote-route))
                              "tramp-rpc"))
            (error "Probe requires a live tramp-rpc process route"))
          (delete-process transport)
          (remote-task-disconnect-smoke--wait
           (lambda () (not (eq (remote-task-state task) 'running))) 12)
          (unless (and (eq (remote-task-state task) 'interrupted)
                       (null (remote-task-exit-code task))
                       (with-current-buffer (remote-task-buffer task)
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward
                            "interrupted, remote result unknown" nil t))))
            (error "Transport loss was reported as %s, exit %s"
                   (remote-task-state task)
                   (remote-task-exit-code task)))
          (let ((task-state (remote-task-state task))
                (task-exit (remote-task-exit-code task))
                (owner-state (remote-workspace-state workspace)))
            (unless
                (remote-task-disconnect-smoke--wait
                 (lambda () (remote-workspace-live-p workspace)) 12)
              (error "Workspace did not recover after task transport loss"))
            (setq rerun (remote-task-rerun task))
            (unless
                (remote-task-disconnect-smoke--wait
                 (lambda () (remote-task-target-pid rerun)) 8)
              (error "Rerun did not start on the recovered target"))
            (setq rerun-pid (remote-task-target-pid rerun))
            (unless (and (eq (remote-task-state rerun) 'running)
                         (equal (remote-task-directory rerun)
                                (remote-task-directory task))
                         (equal (remote-route-target-id
                                 (process-get (remote-task-process rerun)
                                              'remote-route))
                                target))
              (error "Rerun lost its workspace route or directory"))
            (remote-task-cancel rerun)
            (unless
                (remote-task-disconnect-smoke--wait
                 (lambda ()
                   (not (process-live-p (remote-task-process rerun))))
                 8)
              (error "Rerun cancellation did not close its relay"))
            (unless
                (remote-task-disconnect-smoke--wait
                 (lambda ()
                   (remote-task-target-cancel-confirmed rerun))
                 8)
              (error
               "Rerun target cancellation was not confirmed (signal-process=%s)"
               (and (remote-task-cancel-signal-process rerun)
                    (process-status
                     (remote-task-cancel-signal-process rerun)))))
            (let* ((inspection
                    (remote-exec
                     "ps" :args
                     (list "-o" "stat=" "-p"
                           (number-to-string rerun-pid))
                     :context (remote-workspace-context workspace)))
                   (state
                    (string-trim
                     (remote-exec-result-stdout inspection))))
              (unless (or (string-empty-p state)
                          (string-match-p "\\`Z" state))
                (error
                 "Rerun cancellation left target PID %s alive: %s"
                 rerun-pid state)))
            (princ
             (format
              "task=%s exit=%s workspace-at-exit=%s recovered=%s rerun=%s relay=%s pid=%s\n"
              task-state task-exit owner-state
              (remote-workspace-state workspace)
              (remote-task-state rerun)
              (process-status (remote-task-process task)) pid))))
      (dolist (pid (delq nil (list pid rerun-pid)))
        (let ((cleaned nil))
          (condition-case nil
              (progn
                (remote-exec
                 "sh" :args
                 (list "-c"
                       "kill -TERM \"-$1\" 2>/dev/null || kill -TERM \"$1\" 2>/dev/null || true"
                       "emacs-remote-task-cleanup" (number-to-string pid))
                 :context (remote-workspace-context workspace))
                (setq cleaned
                      (let* ((inspection
                              (remote-exec
                               "ps" :args
                               (list "-o" "stat=" "-p"
                                     (number-to-string pid))
                               :context
                               (remote-workspace-context workspace)))
                             (state
                              (string-trim
                               (remote-exec-result-stdout inspection))))
                        (or (string-empty-p state)
                            (string-match-p "\\`Z" state)))))
            (error nil))
          (unless cleaned
            ;; The normal SSH client is independent of this batch RPC relay.
            (let ((remote-command
                   (format
                    "kill -TERM -%d 2>/dev/null || kill -TERM %d 2>/dev/null || true; for i in 1 2 3 4 5 6 7 8 9 10; do kill -0 %d 2>/dev/null || exit 0; case \"$(ps -o stat= -p %d 2>/dev/null)\" in *Z*|'') exit 0;; esac; sleep 0.1; done; exit 23"
                    pid pid pid pid)))
              (unless
                  (eq
                   0
                   (call-process
                    "ssh" nil nil nil "-o" "BatchMode=yes"
                    "-o" "ConnectTimeout=5" target remote-command))
                (error "Could not clean up target task PID %s" pid))))))
      (when (and task (eq (remote-task-state task) 'running))
        (remote-task-cancel task))
      (when (and rerun (eq (remote-task-state rerun) 'running))
        (remote-task-cancel rerun))
      (when (remote-workspace-live-p workspace)
        (remote-workspace-close workspace 'task-disconnect-smoke))
      (when (and task (buffer-live-p (remote-task-buffer task)))
        (kill-buffer (remote-task-buffer task)))
      (when (and rerun (buffer-live-p (remote-task-buffer rerun)))
        (kill-buffer (remote-task-buffer rerun))))))

(remote-task-disconnect-smoke-run)

(provide 'remote-task-disconnect-smoke)
;;; remote-task-disconnect-smoke.el ends here
