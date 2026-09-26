;;; remote-debug-live-smoke.el --- Opt-in Dape target stdio check -*- lexical-binding: t; -*-

;;; Commentary:
;; REMOTE_DEBUG_E2E_TARGET=target emacs ... -l test/remote-debug-live-smoke.el
;; REMOTE_DEBUG_E2E_MODE=module also exercises the Python module alias.
;; REMOTE_DEBUG_E2E_MODE=attach tests a target-loopback debugpy listener.
;; Creates and removes a tiny Python file below the target's /tmp directory.

;;; Code:

(require 'cl-lib)
(require 'init-debug)
(require 'remote-config)
(require 'remote-framework)
(require 'dape)

(defun remote-debug-live-smoke--wait (test seconds)
  "Wait up to SECONDS until TEST returns non-nil."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall test)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall test)))

(defun remote-debug-live-smoke--target-port (root)
  "Reserve and release an ephemeral loopback port on ROOT's target."
  (let* ((result
          (remote-exec
           "python3" :context root :adapter "process"
           :args
           '("-c" "import socket; s=socket.socket(); s.bind(('127.0.0.1', 0)); print(s.getsockname()[1]); s.close()")))
         (port (string-to-number (remote-exec-result-stdout result))))
    (unless (and (zerop (remote-exec-result-status result))
                 (<= 1024 port 65535))
      (error "Could not choose target debug port: %S" result))
    port))

(defun remote-debug-live-smoke--listener-p (root port)
  "Return non-nil when target ROOT listens on loopback PORT."
  (zerop
   (remote-exec-result-status
    (remote-exec
     "sh" :context root :adapter "process"
     :args
     (list "-c"
           (format "ss -lnt | grep -qE '127[.]0[.]0[.]1:%d[[:space:]]'"
                   port))))))

(defun remote-debug-live-smoke-run ()
  "Verify Python DAP over the target's process stdio route."
  (remote-config-load)
  (remote-fs-install)
  (let* ((target (or (getenv "REMOTE_DEBUG_E2E_TARGET")
                     (error "Set REMOTE_DEBUG_E2E_TARGET")))
         (module-p (equal (getenv "REMOTE_DEBUG_E2E_MODE") "module"))
         (attach-p (equal (getenv "REMOTE_DEBUG_E2E_MODE") "attach"))
         (root (file-name-as-directory
                (remote-make-file-name
                 target (format "/tmp/emacs-dape-smoke-%d" (emacs-pid)))))
         (source (expand-file-name
                  (if module-p "smoke_module.py" "main.py") root))
         (config (unless attach-p
                   (copy-tree
                    (alist-get (if module-p 'python-module 'python-file)
                               dape-configs))))
         buffer connection frame debuggee output-buffer error-buffer port)
    (unless (or config attach-p)
      (error "Dape Python config is unavailable"))
    (unwind-protect
        (progn
          (make-directory root t)
          (with-temp-file source
            (insert (if attach-p "import debugpy\ndebugpy.breakpoint()\n"
                      "value = 42\n"))
            (insert "print(42, flush=True)\n"))
          (setq buffer (find-file-noselect source))
          (when attach-p
            (setq port (remote-debug-live-smoke--target-port root)
                  output-buffer (generate-new-buffer " *remote-debuggee*")
                  error-buffer (generate-new-buffer " *remote-debuggee-error*"))
            (setq debuggee
                  (remote-make-process
                   :name "remote-debuggee"
                   :command
                   (list "python3" "-u" "-m" "debugpy"
                         "--listen" (format "127.0.0.1:%d" port)
                         "--wait-for-client" (remote-file-local-name source))
                   :buffer output-buffer :stderr error-buffer :noquery t
                   :remote-context root :remote-directory root))
            (unless (remote-debug-live-smoke--wait
                     (lambda () (remote-debug-live-smoke--listener-p root port))
                     8)
              (error "Target debuggee never listened on %d" port))
            (setq config (my/debug-python-attach-config port)))
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (when module-p
              (setq config (plist-put config :module "smoke_module")))
            (unless attach-p
              (setq config (plist-put config :stopOnEntry t)))
            (dape (if attach-p config (dape--config-eval-1 config))))
          (setq connection (car dape--connections))
          (unless (remote-debug-live-smoke--wait
                   (lambda () (eq (dape--state connection) 'stopped)) 25)
            (error "Dape did not stop: state=%S config=%S stderr=%S"
                   (dape--state connection) (dape--config connection)
                   (and (buffer-live-p (dape--stderr-buffer connection))
                        (with-current-buffer (dape--stderr-buffer connection)
                          (buffer-string)))))
          (unless (equal (plist-get (dape--config connection) 'prefix-local)
                         (format "/fs:%s:" target))
            (error "Dape did not project source identity: %S"
                   (dape--config connection)))
          (dolist (process
                   (list (jsonrpc--process connection)
                         (if attach-p debuggee
                           (and (buffer-live-p (dape--shell-buffer connection))
                                (get-buffer-process
                                 (dape--shell-buffer connection))))))
            (unless (and (processp process)
                         (remote-route-p (process-get process 'remote-route))
                         (equal target
                                (remote-route-target-id
                                 (process-get process 'remote-route))))
              (error "Dape adapter or integrated terminal left target route: %S"
                     process)))
          (unless (remote-debug-live-smoke--wait
                   (lambda () (setq frame
                                    (dape--current-stack-frame connection))) 8)
            (error "Dape stopped without a stack frame"))
          (let ((mapped
                 (dape--file-name-local
                  connection (plist-get (plist-get frame :source) :path))))
            (unless (equal (file-truename mapped) (file-truename source))
              (error "Dape stack frame did not resolve to source: mapped=%S source=%S frame=%S"
                     mapped source frame)))
          (dape-continue connection)
          (unless (remote-debug-live-smoke--wait
                   (lambda () (memq (dape--state connection)
                                    '(exited terminated))) 15)
            (error "Dape target did not exit: %S" (dape--state connection)))
          (unless (and (buffer-live-p
                        (if attach-p output-buffer
                          (dape--shell-buffer connection)))
                       (with-current-buffer
                           (if attach-p output-buffer
                             (dape--shell-buffer connection))
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "42" nil t))))
            (error "Dape target process did not show output"))
          (princ (format "Remote Dape: target=%s mode=%s state=%S source=%S\n"
                         target (cond (attach-p "attach")
                                      (module-p "module") (t "file"))
                         (dape--state connection) source)))
      (when connection (ignore-errors (dape-kill connection)))
      (when (and (processp debuggee) (process-live-p debuggee))
        (delete-process debuggee))
      (dolist (capture (list output-buffer error-buffer))
        (when (buffer-live-p capture) (kill-buffer capture)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p root) (delete-directory root t)))))

(when noninteractive (remote-debug-live-smoke-run))

(provide 'remote-debug-live-smoke)
;;; remote-debug-live-smoke.el ends here
