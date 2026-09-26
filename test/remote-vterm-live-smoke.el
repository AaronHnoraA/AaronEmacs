;;; remote-vterm-live-smoke.el --- Real routed VTerm check -*- lexical-binding: t; -*-

;; REMOTE_VTERM_E2E_TARGET=host make remote-vterm-live-smoke
;; REMOTE_VTERM_E2E_ROOT=/existing/directory/ optionally selects the cwd.
;; REMOTE_VTERM_E2E_PREOPEN=1 measures VTerm in an already open workspace.
;; REMOTE_VTERM_E2E_PRELOAD=1 excludes VTerm package load from that measure.
;; This probe creates no target files.

(require 'init-shell)
(require 'remote-config)
(require 'remote-framework)

(defun remote-vterm-live-smoke-run ()
  "Check that the actual VTerm frontend uses a routed target PTY."
  (remote-config-load)
  (remote-fs-install)
  (when (equal (getenv "REMOTE_VTERM_E2E_PRELOAD") "1")
    (require 'vterm))
  (let* ((target (or (getenv "REMOTE_VTERM_E2E_TARGET")
                     (error "Set REMOTE_VTERM_E2E_TARGET")))
         (root (or (getenv "REMOTE_VTERM_E2E_ROOT") "/tmp/"))
         (default-directory
          (file-name-as-directory (remote-make-file-name target root)))
         (preopened
          (when (equal (getenv "REMOTE_VTERM_E2E_PREOPEN") "1")
            (remote-workspace-open
             default-directory :adapter "process"
             :capability 'process-async :load-environment t)))
         (started-at (float-time))
         (buffer (save-window-excursion
                   (vterm "*remote-vterm-live-smoke*")))
         (spawned-at (float-time))
         (terminal
          (and (buffer-live-p buffer)
               (buffer-local-value 'remote-terminal-instance buffer)))
         (process (and terminal (remote-terminal-process terminal)))
         (route (and process (process-get process 'remote-route)))
         (workspace (and terminal
                         (remote-get-workspace
                          (remote-terminal-workspace-id terminal))))
         command-at)
    (unwind-protect
        (progn
          (unless (and (process-live-p process) route workspace
                       (eq (remote-route-capability route) 'pty)
                       (equal (remote-route-target-id route)
                              (remote-normalize-id target))
                       (seq-some
                        (lambda (known)
                          (equal (remote-connection-route-key known)
                                 (remote-connection-route-key route)))
                        (remote-workspace-routes workspace)))
            (error "VTerm PTY mismatch: process=%S route=%S workspace=%S tracked=%S"
                   (and process (process-status process))
                   (and route
                        (list (remote-route-target-id route)
                              (remote-route-backend-id route)
                              (remote-route-capability route)))
                   (and workspace (remote-workspace-state workspace))
                   (and workspace route
                        (mapcar #'remote-connection-route-key
                                (remote-workspace-routes workspace)))))
          (setq command-at (float-time))
          (with-current-buffer buffer
            (vterm-send-string "printf 'VTERM-ROUTED|%s|%s\\n' \"$PWD\" \"$(hostname)\"")
            (vterm-send-return))
          (let ((deadline (+ (float-time) 20)))
            (while (and (process-live-p process)
                        (< (float-time) deadline)
                        (not (with-current-buffer buffer
                               (save-excursion
                                 (goto-char (point-min))
                                 (search-forward
                                  (format "VTERM-ROUTED|%s|"
                                          (directory-file-name root))
                                  nil t)))))
              (accept-process-output process 0.05)))
          (unless (with-current-buffer buffer
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward
                       (format "VTERM-ROUTED|%s|"
                               (directory-file-name root))
                       nil t)))
            (error "VTerm shell did not execute in target cwd: %S"
                   (with-current-buffer buffer (buffer-string))))
          (princ
           (format "Remote VTerm smoke: target=%s backend=%s cwd=%s preopen=%s spawn-ms=%.1f echo-ms=%.1f ready-ms=%.1f\n"
                   target (remote-route-backend-id route) root
                   (and preopened t)
                   (* 1000 (- spawned-at started-at))
                   (* 1000 (- (float-time) command-at))
                   (* 1000 (- (float-time) started-at)))))
      (when terminal (remote-terminal-close terminal))
      (let ((owner (or workspace preopened)))
        (when (remote-workspace-live-p owner)
          (remote-workspace-close owner 'vterm-live-smoke))))))

(when noninteractive
  (remote-vterm-live-smoke-run))

(provide 'remote-vterm-live-smoke)
;;; remote-vterm-live-smoke.el ends here
