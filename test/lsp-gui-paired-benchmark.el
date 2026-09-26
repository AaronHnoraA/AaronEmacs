;;; lsp-gui-paired-benchmark.el --- Matched native/SSH GUI LSP redraw -*- lexical-binding: t; -*-

;; Load after lsp-remote-live-smoke.el in a GUI frame and call RUN.

(require 'remote-config)
(require 'remote-framework)

(defun my/lsp-gui-paired-benchmark-run ()
  "Run Python LSP checks for native and SSH in the same GUI frame."
  (unless (display-graphic-p)
    (error "A GUI frame is required"))
  (remote-config-load)
  (remote-fs-install)
  (set-frame-size
   (selected-frame)
   (max 40 (string-to-number
            (or (getenv "REMOTE_LSP_E2E_FRAME_COLUMNS") "180")))
   (max 20 (string-to-number
            (or (getenv "REMOTE_LSP_E2E_FRAME_ROWS") "55"))))
  (let* ((remote-id (or (getenv "REMOTE_LSP_E2E_TARGET")
                        (error "Set REMOTE_LSP_E2E_TARGET")))
         (local-target (remote-get-target "local"))
         (remote-target (remote-get-target remote-id))
         (spec (assoc 'python my/lsp-remote-live-smoke-specs))
         results)
    (unless (and local-target remote-target spec)
      (error "Python or configured local/SSH target is unavailable"))
    (dolist (entry `((native . ,local-target)
                     (remote . ,remote-target)
                     (remote . ,remote-target)
                     (native . ,local-target)))
      (let ((process-environment (copy-sequence process-environment)))
        (setenv "REMOTE_LSP_E2E_VISIT"
                (if (eq (car entry) 'native) "native" "logical"))
        (push (cons (car entry)
                    (my/lsp-remote-live-smoke--run-one
                     (cdr entry) spec))
              results)))
    (setq results (nreverse results))
    (with-temp-file
        (or (getenv "REMOTE_LSP_E2E_RESULT_FILE")
            (error "Set REMOTE_LSP_E2E_RESULT_FILE"))
      (dolist (entry results)
        (prin1
         (list :route (car entry)
               :ok (plist-get (cdr entry) :ok)
               :typing-probe (plist-get (cdr entry) :typing-probe)
               :completion (plist-get (cdr entry) :company-probe)
               :watch (plist-get (cdr entry) :watch-probe))
         (current-buffer))
        (terpri (current-buffer))))
    (kill-emacs
     (if (seq-every-p (lambda (entry) (plist-get (cdr entry) :ok))
                      results)
         0 1))))

(provide 'lsp-gui-paired-benchmark)
;;; lsp-gui-paired-benchmark.el ends here
