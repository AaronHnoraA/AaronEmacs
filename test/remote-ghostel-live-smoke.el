;;; remote-ghostel-live-smoke.el --- Real routed Ghostel check -*- lexical-binding: t; -*-

;; REMOTE_GHOSTEL_E2E_TARGET=host make remote-ghostel-live-smoke
;; REMOTE_GHOSTEL_E2E_ROOT=/existing/directory/ optionally selects the cwd.
;; REMOTE_GHOSTEL_E2E_PREOPEN=1 measures Ghostel in an already open workspace.
;; REMOTE_GHOSTEL_E2E_PRELOAD=1 excludes Ghostel package load from that measure.
;; REMOTE_GHOSTEL_E2E_FAULT=transport drops the terminal's transport, then
;; checks that the buffer survives and that an explicit restart works.
;; REMOTE_GHOSTEL_E2E_TARGET=local runs the same check on the client machine.
;; This probe creates no target files.

(require 'init-shell)
(require 'remote-config)
(require 'remote-framework)

(defvar ghostel--term)
(declare-function ghostel--copy-all-text "ghostel-module" (term))
(declare-function ghostel-send-string "ghostel" (string))

(defun remote-ghostel-live-smoke--screen (buffer)
  "Return what the terminal in Ghostel BUFFER holds.
The terminal's own text is read as well as the buffer, because a batch Emacs
has no window for Ghostel to render into."
  (with-current-buffer buffer
    (concat (buffer-string)
            (or (and ghostel--term
                     (ignore-errors (ghostel--copy-all-text ghostel--term)))
                ""))))

(defun remote-ghostel-live-smoke--echo (buffer root)
  "Run a command in Ghostel BUFFER and require its output from ROOT."
  (let ((process (get-buffer-process buffer))
        (expected (format "GHOSTEL-ROUTED|%s|" (directory-file-name root)))
        (deadline (+ (float-time) 20))
        found)
    (with-current-buffer buffer
      (ghostel-send-string
       "printf 'GHOSTEL-ROUTED|%s|%s\\n' \"$PWD\" \"$(hostname)\"\n"))
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not (setq found
                           (string-search
                            expected
                            (remote-ghostel-live-smoke--screen buffer)))))
      (accept-process-output process 0.05))
    (unless found
      (error "Ghostel shell did not execute in target cwd: %S"
             (remote-ghostel-live-smoke--screen buffer)))))

(defun remote-ghostel-live-smoke--fault (terminal buffer workspace root)
  "Drop TERMINAL's transport and restart it; return the new buffer.
BUFFER is its Ghostel buffer in WORKSPACE, started in ROOT."
  (let ((name (buffer-name buffer))
        (process (remote-terminal-process terminal))
        replacement)
    (remote-workspace--mark-terminals-disconnected workspace 'injected-loss)
    ;; A reconnect announces the same loss a second time.
    (remote-workspace--mark-terminals-disconnected
     workspace 'workspace-reconnect)
    (accept-process-output nil 0.2)
    (unless (and (buffer-live-p buffer)
                 (not (process-live-p process))
                 (eq (remote-terminal-state terminal) 'disconnected)
                 (memq terminal (remote-terminal-list workspace))
                 (= 1 (with-current-buffer buffer
                        (count-matches "transport disconnected"
                                       (point-min) (point-max)))))
      (error "Disconnected Ghostel was not kept: buffer=%S state=%S text=%S"
             (buffer-live-p buffer) (remote-terminal-state terminal)
             (and (buffer-live-p buffer)
                  (with-current-buffer buffer (buffer-string)))))
    (setq replacement (remote-terminal-restart terminal))
    (unless (and (buffer-live-p replacement)
                 (not (eq replacement buffer))
                 (not (buffer-live-p buffer))
                 (equal (buffer-name replacement) name)
                 (process-live-p (get-buffer-process replacement))
                 (eq (remote-terminal-state terminal) 'closed)
                 (= 1 (length (remote-terminal-list workspace))))
      (error "Ghostel restart mismatch: replacement=%S old=%S terminals=%S"
             replacement (buffer-live-p buffer)
             (length (remote-terminal-list workspace))))
    (remote-ghostel-live-smoke--echo replacement root)
    replacement))

(defun remote-ghostel-live-smoke-run ()
  "Check that the actual Ghostel frontend uses a routed target PTY."
  (remote-config-load)
  (remote-fs-install)
  (when (equal (getenv "REMOTE_GHOSTEL_E2E_PRELOAD") "1")
    (require 'ghostel))
  (let* ((target (or (getenv "REMOTE_GHOSTEL_E2E_TARGET")
                     (error "Set REMOTE_GHOSTEL_E2E_TARGET")))
         (root (or (getenv "REMOTE_GHOSTEL_E2E_ROOT") "/tmp/"))
         (default-directory
          (file-name-as-directory (remote-make-file-name target root)))
         (preopened
          (when (equal (getenv "REMOTE_GHOSTEL_E2E_PREOPEN") "1")
            (remote-workspace-open
             default-directory :adapter "process"
             :capability 'process-async :load-environment t)))
         (started-at (float-time))
         (buffer (my/ghostel-create-hidden "*remote-ghostel-live-smoke*"))
         (spawned-at (float-time))
         (terminal
          (and (buffer-live-p buffer)
               (buffer-local-value 'remote-terminal-instance buffer)))
         (process (and terminal (remote-terminal-process terminal)))
         (route (and process (process-get process 'remote-route)))
         (workspace (and terminal
                         (remote-get-workspace
                          (remote-terminal-workspace-id terminal))))
         ;; Where the target shares the client's filesystem, Ghostel runs
         ;; the shell on its own native PTY and there is no route to check.
         (client-side (remote-client-file-name default-directory))
         (fault (getenv "REMOTE_GHOSTEL_E2E_FAULT"))
         command-at)
    (unwind-protect
        (progn
          (unless (and (process-live-p process) workspace
                       (equal (remote-workspace-target-id workspace)
                              (remote-normalize-id target))
                       (or (and client-side (null route))
                           (and route
                                (eq (remote-route-capability route) 'pty)
                                (equal (remote-route-target-id route)
                                       (remote-normalize-id target))
                                (seq-some
                                 (lambda (known)
                                   (equal
                                    (remote-connection-route-key known)
                                    (remote-connection-route-key route)))
                                 (remote-workspace-routes workspace)))))
            (error "Ghostel PTY mismatch: process=%S route=%S workspace=%S tracked=%S"
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
          (remote-ghostel-live-smoke--echo buffer root)
          (when (equal fault "transport")
            (setq buffer
                  (remote-ghostel-live-smoke--fault
                   terminal buffer workspace root)
                  terminal
                  (buffer-local-value 'remote-terminal-instance buffer)))
          (princ
           (format "Remote Ghostel smoke: target=%s backend=%s cwd=%s preopen=%s fault=%s spawn-ms=%.1f echo-ms=%.1f ready-ms=%.1f\n"
                   target
                   (if route (remote-route-backend-id route) "frontend-pty")
                   root
                   (and preopened t) (or fault "none")
                   (* 1000 (- spawned-at started-at))
                   (* 1000 (- (float-time) command-at))
                   (* 1000 (- (float-time) started-at)))))
      (when terminal (remote-terminal-close terminal))
      (let ((owner (or workspace preopened)))
        (when (remote-workspace-live-p owner)
          (remote-workspace-close owner 'ghostel-live-smoke))))))

(when noninteractive
  (remote-ghostel-live-smoke-run))

(provide 'remote-ghostel-live-smoke)
;;; remote-ghostel-live-smoke.el ends here
