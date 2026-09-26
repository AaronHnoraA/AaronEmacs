;;; lsp-existing-file-live-probe.el --- Read-only live Python completion check -*- lexical-binding: t; -*-

;; REMOTE_LSP_EXISTING_FILE=/fs:host:/path/file.py make lsp-existing-file-live-probe
;; REMOTE_LSP_EXISTING_PREWARM=0 measures an unprimed first completion.
;; REMOTE_LSP_EXISTING_STALL=1 drops one completion reply to verify the
;; bounded Company fallback and a later successful retry on the same buffer.
;; REMOTE_LSP_EXISTING_STALL=restart drops both sync and health replies to
;; verify a rate-limited restart of the still-live Python server.
;; REMOTE_LSP_EXISTING_STALL=unowned also removes its resource record first.
;; The source buffer is restored and never saved.  The server may write its
;; normal language-service cache on the target.

(require 'init-lsp)
(require 'remote-config)
(require 'remote-framework)
(require 'company-capf)

(defun my/lsp-existing-file-live-probe--ready-p ()
  "Return the current buffer's initialized Python workspace, if any."
  (seq-find
   (lambda (workspace)
     (and (eq (lsp--workspace-status workspace) 'initialized)
          (eq (my/language-server--lsp-workspace-id workspace)
              'my-python)))
   (lsp-workspaces)))

(defun my/lsp-existing-file-live-probe--restart-test
    (old-workspace &optional unowned)
  "Verify missing completion replies restart OLD-WORKSPACE once.
With UNOWNED, simulate an LSP workspace without its Remote resource record."
  (unless (bound-and-true-p my/lsp-remote-change--eligible)
    (error "Remote completion guard is not active"))
  (when unowned
    (unless
        (seq-some
         (lambda (owner)
           (when-let* ((resource
                        (seq-find
                         (lambda (candidate)
                           (and (eq (remote-workspace-resource-kind candidate)
                                    'lsp)
                                (eq (remote-workspace-resource-value candidate)
                                    old-workspace)))
                         (remote-workspace-resources owner))))
             (remote-workspace-forget-resource owner resource)))
         (hash-table-values remote-workspaces))
      (error "No Remote LSP resource could be removed for fallback test")))
  (let* ((original-async (symbol-function 'lsp-request-async))
         (my/lsp-remote-completion-timeout 0.2)
         (my/lsp-remote-completion-health-timeout 0.45)
         (my/lsp-remote-completion--restart-history
          (make-hash-table :test #'equal))
         (company--capf-cache nil)
         (lsp-completion--cache nil)
         (started (float-time))
         (key (my/lsp-mode--workspace-key old-workspace))
         new-workspace)
    (cl-letf (((symbol-function 'lsp-request-async)
               (lambda (method params callback &rest keys)
                 (unless (equal method "textDocument/completion")
                   (apply original-async method params callback keys)))))
      (unless (null (company-capf 'candidates "prin" ""))
        (error "Persistent stall returned completion candidates"))
      (let ((deadline (+ (float-time) 8)))
        (while (and (< (float-time) deadline)
                    (not (gethash key
                                  my/lsp-remote-completion--restart-history)))
          (accept-process-output nil 0.05)))
      (unless (gethash key my/lsp-remote-completion--restart-history)
        (error "Unresponsive Python LSP was not restarted")))
    (let ((deadline (+ (float-time) 15)))
      (while (and (< (float-time) deadline)
                  (not (and (setq new-workspace
                                  (my/lsp-existing-file-live-probe--ready-p))
                            (not (eq new-workspace old-workspace)))))
        (accept-process-output nil 0.05)))
    (unless (and new-workspace (not (eq new-workspace old-workspace)))
      (error "Python LSP did not reinitialize after health timeout"))
    (let ((deadline (+ (float-time) 8)))
      (while (and (< (float-time) deadline)
                  (not (eq 'ready
                           (plist-get
                            (gethash
                             new-workspace
                             my/lsp-completion--prewarm-state)
                            :state))))
        (accept-process-output nil 0.05)))
    (setq company--capf-cache nil
          lsp-completion--cache nil)
    (unless (member "print" (company-capf 'candidates "prin" ""))
      (error "Completion did not recover after LSP restart"))
    (list :health-restarted t
          :unowned-recovery (and unowned t)
          :restart-recovered-ms (* 1000 (- (float-time) started)))))

(defun my/lsp-existing-file-live-probe-run ()
  "Probe actual Company completion on an existing Python source file."
  (remote-config-load)
  (remote-fs-install)
  (let* ((file (or (getenv "REMOTE_LSP_EXISTING_FILE")
                   (error "Set REMOTE_LSP_EXISTING_FILE")))
         (prewarm (not (equal (getenv "REMOTE_LSP_EXISTING_PREWARM") "0")))
         (early (equal (getenv "REMOTE_LSP_EXISTING_EARLY") "1"))
         (my/lsp-completion-prewarm prewarm)
         (started (float-time))
         (_readable (unless (file-readable-p file)
                      (error "Source is not readable: %s" file)))
         (buffer (find-file-noselect file))
         (visited-at (float-time))
         result)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (unless (derived-mode-p 'python-mode 'python-ts-mode)
              (error "Expected a Python source buffer"))
            (let ((original (buffer-string))
                  (initialized-at nil)
                  (workspace nil))
              (setq-local lsp-auto-guess-root t
                          lsp-guess-root-without-session t
                          my/language-server--manual-start t)
              (my/language-server-ensure)
              (when (and (boundp 'lsp--buffer-deferred)
                         lsp--buffer-deferred
                         (fboundp 'lsp--init-if-visible))
                (lsp--init-if-visible))
              (let ((deadline (+ (float-time) 45)))
                (while (and (< (float-time) deadline)
                            (not (setq workspace
                                       (my/lsp-existing-file-live-probe--ready-p))))
                  (accept-process-output nil 0.05)))
              (unless workspace
                (error "Python LSP did not initialize for %s" file))
              (setq initialized-at (float-time))
              (when-let* ((settle (getenv "REMOTE_LSP_EXISTING_SETTLE")))
                (accept-process-output nil (string-to-number settle)))
              (setq result
                    (list :visit-ms (* 1000 (- visited-at started))
                          :initialize-ms
                          (* 1000 (- initialized-at visited-at))
                          :prewarm-enabled prewarm))
              (when early
                (unless prewarm
                  (error "Early Company probe requires completion prewarm"))
                (save-excursion
                  (goto-char (point-max))
                  (let ((begin (point)))
                    (unwind-protect
                        (progn
                          (insert "\nprin")
                          (let* ((this-command 'self-insert-command)
                                 (guard-at (float-time))
                                 (delay (funcall company-idle-delay)))
                            (setq result
                                  (append result
                                          (list :early-guard-ms
                                                (* 1000 (- (float-time) guard-at))
                                                :early-idle-delay delay)))
                            (when delay
                              (error "Cold Company start was not deferred")))
                          (let ((deadline (+ (float-time) 8)))
                            (while (and (< (float-time) deadline)
                                        (not (member "print" company-candidates)))
                              (accept-process-output nil 0.05)))
                          (setq result
                                (append result
                                        (list :early-popup-print
                                              (and (member "print"
                                                           company-candidates)
                                                   t))))
                          (unless (plist-get result :early-popup-print)
                            (error "Company did not resume after warmup")))
                      (when (fboundp 'company-abort)
                        (company-abort))
                      (delete-region begin (point-max))))))
              (when prewarm
                (let ((deadline
                       (+ (float-time)
                          (if-let* ((wait (getenv
                                           "REMOTE_LSP_EXISTING_PREWARM_WAIT")))
                              (string-to-number wait)
                            8))))
                  (while (and (< (float-time) deadline)
                              (not (memq
                                    (plist-get
                                     (gethash
                                      workspace
                                      my/lsp-completion--prewarm-state)
                                     :state)
                                    '(ready failed unsupported no-buffer))))
                    (accept-process-output nil 0.05)))
                (let ((state
                       (plist-get
                        (gethash
                         workspace
                         my/lsp-completion--prewarm-state)
                        :state)))
                  (setq result
                        (append result
                                (list :prewarm-state state
                                      :prewarm-ms
                                      (* 1000 (- (float-time) initialized-at)))))
                  (unless (eq state 'ready)
                    (error "Python completion prewarm ended in %S" state))))
              (save-excursion
                (goto-char (point-max))
                (let ((begin (point)))
                  (unwind-protect
                      (progn
                        (insert "\nprin")
                        (accept-process-output nil 0.15)
                        (let ((request-at (float-time))
                              prefix candidates)
                          (with-timeout
                              (15 (error "Company completion timed out"))
                            (setq prefix (company-capf 'prefix)
                                  candidates
                                  (company-capf 'candidates "prin" "")))
                          (setq result
                                (append
                                 result
                                 (list :company-ms
                                       (* 1000 (- (float-time) request-at))
                                       :prefix (car-safe prefix)
                                       :candidates (length candidates)
                                       :contains-print
                                       (and (member "print" candidates) t)
                                       :capf
                                        (car-safe
                                        company-capf--current-completion-data))))))
                        (when (equal (getenv "REMOTE_LSP_EXISTING_STALL") "1")
                          (unless (bound-and-true-p
                                   my/lsp-remote-change--eligible)
                            (error "Remote completion guard is not active"))
                          (let* ((original-async
                                  (symbol-function 'lsp-request-async))
                                 (my/lsp-remote-completion-timeout 0.25)
                                 (company--capf-cache nil)
                                 (lsp-completion--cache nil)
                                 (intercepted nil)
                                 (began (float-time))
                                 timed-out-ms backoff-ms)
                            (cl-letf
                                (((symbol-function 'lsp-request-async)
                                  (lambda (method params callback &rest keys)
                                    (if (equal method "textDocument/completion")
                                        (setq intercepted t)
                                      (apply original-async
                                             method params callback keys)))))
                              (unless (null
                                       (company-capf 'candidates "prin" ""))
                                (error "Stalled completion returned candidates")))
                            (setq timed-out-ms
                                  (* 1000 (- (float-time) began)))
                            (unless (and intercepted
                                         (< timed-out-ms 800)
                                         (numberp
                                          my/lsp-remote-completion--retry-at))
                              (error "Completion did not fall back promptly: %S ms"
                                     timed-out-ms))
                            (setq began (float-time))
                            (unless (null (company-capf 'candidates "prin" ""))
                              (error "Completion backoff returned candidates"))
                            (setq backoff-ms (* 1000 (- (float-time) began)))
                            (let ((deadline (+ (float-time) 5)))
                              (while (and (< (float-time) deadline)
                                          (or my/lsp-remote-completion--probe
                                              my/lsp-remote-completion--retry-at))
                                (accept-process-output nil 0.05)))
                            (when (or my/lsp-remote-completion--probe
                                      my/lsp-remote-completion--retry-at)
                              (error "Asynchronous completion health check did not recover"))
                            (setq company--capf-cache nil
                                  lsp-completion--cache nil)
                            (unless (member "print"
                                            (company-capf 'candidates "prin" ""))
                              (error "Completion did not recover after stall"))
                            (setq result
                                  (append result
                                          (list :stall-guard-ms timed-out-ms
                                                :stall-backoff-ms backoff-ms
                                                :health-recovered-ms
                                                (* 1000 (- (float-time) began))
                                                :stall-recovered t)))))
                        (when (member (getenv "REMOTE_LSP_EXISTING_STALL")
                                      '("restart" "unowned"))
                          (setq result
                                (append
                                 result
                                 (my/lsp-existing-file-live-probe--restart-test
                                  workspace
                                  (equal (getenv "REMOTE_LSP_EXISTING_STALL")
                                         "unowned")))))
                    (delete-region begin (point-max)))))
              (unless (equal original (buffer-string))
                (error "Completion probe did not restore the source buffer")))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (dolist (workspace (ignore-errors (lsp-workspaces)))
            (ignore-errors
              (my/lsp-mode-shutdown-workspace
               workspace 'existing-file-live-probe))))
        (kill-buffer buffer)))
    (princ (format "Existing Python LSP: %S\n" result))
    (unless (and (equal (plist-get result :prefix) "prin")
                 (plist-get result :contains-print)
                 (eq (plist-get result :capf) 'lsp-completion-at-point))
      (error "Company did not return `print' through LSP CAPF"))
    result))

(when noninteractive
  (my/lsp-existing-file-live-probe-run))

(provide 'lsp-existing-file-live-probe)
;;; lsp-existing-file-live-probe.el ends here
