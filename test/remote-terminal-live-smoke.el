;;; remote-terminal-live-smoke.el --- Real routed PTY exchange -*- lexical-binding: t; -*-

;; REMOTE_TERMINAL_E2E_TARGET=host make remote-terminal-live-smoke
;; REMOTE_TERMINAL_E2E_ROOT=/existing/directory/ optionally selects the cwd.
;; REMOTE_TERMINAL_E2E_BACKEND=tramp-rpc selects the alternate PTY backend.
;; REMOTE_TERMINAL_E2E_FAULT=rpc kills only this batch client's RPC transport.
;; REMOTE_TERMINAL_E2E_FAULT=pty kills only its terminal SSH process.
;; REMOTE_TERMINAL_E2E_ROUNDS=20 samples repeated warm PTY input/output RTT.
;; REMOTE_TERMINAL_E2E_SECOND=1 measures a second PTY on the same workspace.
;; REMOTE_TERMINAL_E2E_STANDALONE=1 opens a PTY without a prior workspace.
;; This probe creates no target files.

(require 'remote-config)
(require 'remote-framework)

(defun remote-terminal-live-smoke--await (terminal marker seconds)
  "Wait for MARKER in TERMINAL's output for at most SECONDS."
  (let ((deadline (+ (float-time) seconds))
        (process (remote-terminal-process terminal)))
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not (with-current-buffer (remote-terminal-buffer terminal)
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward marker nil t)))))
      (accept-process-output process 0.05))
    (with-current-buffer (remote-terminal-buffer terminal)
      (save-excursion
        (goto-char (point-min))
        (search-forward marker nil t)))))

(defun remote-terminal-live-smoke-run ()
  "Verify a real target PTY routes cwd, environment, input, and output."
  (remote-config-load)
  (remote-fs-install)
  (let* ((target (or (getenv "REMOTE_TERMINAL_E2E_TARGET")
                     (error "Set REMOTE_TERMINAL_E2E_TARGET")))
         (backend (getenv "REMOTE_TERMINAL_E2E_BACKEND"))
         (fault (getenv "REMOTE_TERMINAL_E2E_FAULT"))
         (rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_TERMINAL_E2E_ROUNDS") "1"))))
         (root (or (getenv "REMOTE_TERMINAL_E2E_ROOT") "/tmp/"))
         (logical (file-name-as-directory
                   (remote-make-file-name target root)))
         (_backend-preference
          (when backend
            (let ((selected (remote-get-target target)))
              (unless selected (error "Unknown target %s" target))
              (setf (remote-target-preferences selected)
                    (cons (cons 'pty (list backend))
                          (assq-delete-all
                           'pty (copy-tree
                                 (remote-target-preferences selected))))))))
         (standalone (equal (getenv "REMOTE_TERMINAL_E2E_STANDALONE") "1"))
         (workspace (unless standalone
                      (remote-workspace-open
                       logical :adapter "process"
                       :capability 'process-async :load-environment t)))
         terminal second result latencies host opened-at ready-ms second-ready-ms)
    (unwind-protect
        (progn
          (setq opened-at (float-time))
          (setq terminal
                (remote-terminal-open
                 (or workspace logical) :name "live-pty" :shell "/bin/sh"
                 :arguments
                 (list "-c"
                       "printf 'TERM-READY|%s|%s|%s\\n' \"$PWD\" \"$REMOTE_TERM_TOKEN\" \"$(hostname)\"; while IFS= read -r value; do printf 'TERM-ECHO|%s\\n' \"$value\"; done")
                 :environment '(("REMOTE_TERM_TOKEN" . "routed"))))
          (setq workspace (or workspace (remote-get-workspace logical)))
          (unless (remote-terminal-live-smoke--await
                   terminal
                   (format "TERM-READY|%s|routed"
                           (directory-file-name root))
                   25)
            (error "PTY cwd/environment mismatch: status=%S route=%S output=%S"
                   (process-status (remote-terminal-process terminal))
                   (let ((route (process-get
                                 (remote-terminal-process terminal)
                                 'remote-route)))
                     (and route (remote-route-backend-id route)))
                   (with-current-buffer (remote-terminal-buffer terminal)
                     (buffer-string))))
          (setq ready-ms (* 1000 (- (float-time) opened-at)))
          (when (member fault '("rpc" "pty"))
            (let ((remote-workspace-auto-reconnect nil))
              (if (equal fault "rpc")
                  (let ((transport
                         (process-get
                          (remote-terminal-process terminal)
                          :tramp-rpc-connection-process)))
                    (unless (and (processp transport)
                                 (process-live-p transport))
                      (error "PTY has no live RPC transport to fault"))
                    (delete-process transport))
                (delete-process (remote-terminal-process terminal)))
              (let ((deadline (+ (float-time) 10)))
                (while (and (eq (remote-terminal-state terminal) 'open)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (unless (eq (remote-terminal-state terminal) 'disconnected)
                (error "PTY transport loss left terminal %S and workspace %S"
                       (remote-terminal-state terminal)
                       (remote-workspace-state workspace)))
              (when (equal fault "rpc")
                (remote-workspace-reconnect workspace))
              (setq terminal (remote-terminal-restart terminal))
              (unless (remote-terminal-live-smoke--await
                       terminal
                       (format "TERM-READY|%s|routed"
                               (directory-file-name root))
                       25)
                (error "PTY restart did not restore cwd and environment"))))
          (setq host
                (with-current-buffer (remote-terminal-buffer terminal)
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward
                           "TERM-READY|[^|\r\n]+|routed|\\([^[:space:]]+\\)"
                           nil t)
                      (match-string-no-properties 1)))))
          (dotimes (index rounds)
            (let* ((marker (format "TERM-ECHO|hello-pty-%d" index))
                   (started (float-time)))
              (remote-terminal-send-string
               terminal (format "hello-pty-%d\n" index))
              (unless (remote-terminal-live-smoke--await
                       terminal marker 15)
                (error "PTY input/output round trip %d failed" index))
              (push (* 1000 (- (float-time) started)) latencies)))
          (setq latencies (sort latencies #'<))
          (when (equal (getenv "REMOTE_TERMINAL_E2E_SECOND") "1")
            (let ((started (float-time)))
              (setq second
                    (remote-terminal-open
                     workspace :name "live-pty-second" :shell "/bin/sh"
                     :arguments
                     (list "-c"
                           "printf 'SECOND-READY|%s|%s\\n' \"$PWD\" \"$REMOTE_TERM_TOKEN\"; IFS= read -r value")
                     :environment '(("REMOTE_TERM_TOKEN" . "routed"))))
              (unless (remote-terminal-live-smoke--await
                       second
                       (format "SECOND-READY|%s|routed"
                               (directory-file-name root))
                       25)
                (error "Second PTY did not become ready"))
              (setq second-ready-ms (* 1000 (- (float-time) started)))))
          (setq result
                (list
                 :target target
                 :host host
                 :fault fault
                 :workspace-route
                 (remote-route-backend-id
                  (remote-workspace-primary-route workspace))
                 :route
                 (let ((route (process-get
                               (remote-terminal-process terminal)
                               'remote-route)))
                   (and route (remote-route-backend-id route)))
                 :route-tracked
                 (let ((route (process-get
                               (remote-terminal-process terminal)
                               'remote-route)))
                   (and route
                        (seq-some
                         (lambda (known)
                           (equal (remote-connection-route-key known)
                                  (remote-connection-route-key route)))
                         (remote-workspace-routes workspace))))
                 :state (remote-terminal-state terminal)
                 :round-trip t
                 :ready-ms ready-ms
                 :second-ready-ms second-ready-ms
                 :rounds rounds
                 :median-ms (nth (/ rounds 2) latencies)
                 :p95-ms (nth (min (1- rounds)
                                   (floor (* 0.95 rounds)))
                              latencies)))
          (when (and backend
                     (not (equal (plist-get result :route) backend)))
            (error "PTY used %s instead of requested %s"
                   (plist-get result :route) backend))
          (unless (plist-get result :route-tracked)
            (error "PTY route is not tracked by its workspace"))
          (princ (format "Remote PTY smoke: %S\n" result))
          result)
      (when second (remote-terminal-close second))
      (when terminal (remote-terminal-close terminal))
      (when (remote-workspace-live-p workspace)
        (remote-workspace-close workspace 'terminal-live-smoke)))))

(when noninteractive
  (remote-terminal-live-smoke-run))

(provide 'remote-terminal-live-smoke)
;;; remote-terminal-live-smoke.el ends here
