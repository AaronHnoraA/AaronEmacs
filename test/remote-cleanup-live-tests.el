;;; remote-cleanup-live-tests.el --- Opt-in target teardown checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Load through the normal init, with REMOTE_E2E=1 and a selected
;; REMOTE_E2E_TARGET.  The Python peer requires no target-side LSP install.
;; All target writes and spawned processes belong to a fresh /tmp directory.

;;; Code:

(require 'ert)
(require 'init-lsp)
(require 'lsp-mode)
(load (expand-file-name "test/remote-e2e-tests.el" user-emacs-directory) nil t)

(defun remote-cleanup-live--write (file contents)
  "Write CONTENTS to disposable target FILE."
  (with-temp-buffer
    (insert contents)
    (write-region (point-min) (point-max) file nil 'silent)))

(defun remote-cleanup-live--wait (predicate seconds)
  "Wait up to SECONDS for PREDICATE, failing if it never becomes true."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (should (funcall predicate))))

(cl-defun remote-cleanup-live--exercise (target command &optional (isolated t))
  "Exercise initialized LSP, Dired, watches and tasks on TARGET via COMMAND.
ISOLATED also asserts the entire target remains unopened.  Native `local'
shares unrelated client UI jobs in a full init, so its case checks the owned
test resources and deferred source startup instead of global quiescence."
  (let* ((target-id (remote-target-id target))
         (context (remote-context (remote-make-file-name target-id "/tmp/")))
         (directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-remote-cleanup.XXXXXX")
            :context context :check t)))
         (root (file-name-as-directory (remote-make-file-name target-id directory)))
         (peer (concat root "peer.py"))
         (events (concat root "events.log"))
         (source (concat root "clean.c"))
         (edited (concat root "edited.c"))
         (my/language-server-booster-required nil)
         (my/enable-direnv nil)
         buffers source-buffer edited-buffer dired-buffer workspace lsp-workspace
         task watch reconnect-trace queued-idle
         (trace-advice
          (lambda (route &rest _)
            (when (and (equal (remote-route-target-id route) target-id)
                       (not (remote-connection-cached-p route)))
              (let ((print-level 3) (print-length 5))
                (setq reconnect-trace (backtrace-to-string)))))))
    (unwind-protect
        (progn
          (remote-cleanup-live--write
           peer (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "test/fixtures/remote-cleanup-lsp.py"
                                     user-emacs-directory))
                  (buffer-string)))
          (remote-cleanup-live--write source "int clean(void) { return 1; }\n")
          (remote-cleanup-live--write edited "int edited(void) { return 2; }\n")
          (my/register-language-server
           'c-mode (list "python3" "-u" (remote-file-local-name peer)
                         (remote-file-local-name events))
           :server-id 'remote-cleanup-probe :executables '("python3"))
          (setq dired-buffer (remote-open-folder target directory)
                source-buffer (find-file-noselect source)
                edited-buffer (find-file-noselect edited)
                buffers (list source-buffer edited-buffer dired-buffer))
          (dolist (buffer (list source-buffer edited-buffer))
            (switch-to-buffer buffer)
            (with-current-buffer buffer
              (c-mode)
              (setq-local lsp-enabled-clients '(remote-cleanup-probe)
                          lsp-auto-guess-root t lsp-guess-root-without-session t)
              ;; Start at the advised connection boundary.  Initialization
              ;; must succeed; a test without a running server cannot pass.
              (lsp)
              (remote-cleanup-live--wait
               (lambda ()
                 (seq-some (lambda (value)
                             (eq (lsp--workspace-status value) 'initialized))
                           (lsp-workspaces)))
               15)))
          (with-current-buffer source-buffer
            (setq lsp-workspace (car (lsp-workspaces))
                  workspace (remote-get-workspace
                             (my/language-server--lsp-workspace-root
                              lsp-workspace)))
            (should workspace)
            (should (seq-some
                     (lambda (resource)
                       (eq (remote-workspace-resource-kind resource) 'lsp))
                     (remote-workspace-resources workspace)))
            (set-buffer-modified-p nil)
            (let ((remote-file-watch-workspace workspace))
              (setq watch (file-notify-add-watch source '(change) #'ignore))))
          (with-current-buffer edited-buffer
            (goto-char (point-max))
            (insert "/* preserve unsaved edit */\n"))
          (setq task (remote-task-run
                      '("python3" "-u" "-c" "import time; time.sleep(120)")
                      :workspace workspace :name "cleanup-probe"))
          (push (remote-task-buffer task) buffers)
          (message "Cleanup live: %s %s, LSP initialized, %d buffers, watch, task"
                   target-id command (length buffers))
          (setq queued-idle (copy-sequence timer-idle-list))
          (advice-add 'remote-connection-ensure :before trace-advice)
          (pcase command
            ('board (remote-board-disconnect-target target))
            ('connection
             (let* ((session (seq-find
                              (lambda (value)
                                (equal (remote-connection-target-id value) target-id))
                              (hash-table-values remote-connection-pool)))
                    (vec (tramp-dissect-file-name
                          (remote-connection-handle session))))
               (tramp-cleanup-connection vec)))
            ('all (tramp-cleanup-all-connections)))
          (dolist (buffer (delq edited-buffer (copy-sequence buffers)))
            (should-not (buffer-live-p buffer)))
          (should (buffer-live-p edited-buffer))
          (with-current-buffer edited-buffer
            (should (buffer-modified-p))
            (should (string-match-p "preserve unsaved edit" (buffer-string)))
            (should remote-buffer-disconnected-p)
            (should-not (lsp-workspaces))
            ;; Exercise deferred consumer entry points with the retained file.
            (my/language-server-ensure)
            (my/lsp-mode--connect-via-remote-a
             (lambda (&rest _) (ert-fail "Deferred LSP reopened target"))))
          (should-not (process-live-p (remote-task-process task)))
          (should-not (file-notify-valid-p watch))
          (should-not (let ((process (lsp--workspace-proc lsp-workspace)))
                        (and (processp process) (process-live-p process))))
          ;; Real normal timers run here.  Execute queued per-buffer idle
          ;; callbacks explicitly too, since batch Emacs never goes idle.
          (dolist (timer (delete-dups
                         (append queued-idle (copy-sequence timer-idle-list))))
            (when (memq edited-buffer (timer--args timer))
              (apply (timer--function timer) (timer--args timer))))
          (let ((deadline (+ (float-time) 8)))
            (while (< (float-time) deadline) (accept-process-output nil 0.1)))
          (should-not (remote-get-workspace root))
          (when isolated
            (when (and reconnect-trace
                       (remote-connection-target-open-p target-id))
              (message "Unexpected target reopening:\n%s" reconnect-trace))
            (should-not (remote-connection-target-open-p target-id))
            (should-not (file-remote-p source nil t))
            (should-not (remote-background-job-list target-id)))
          (should-not
           (seq-some (lambda (value)
                       (string-prefix-p root (plist-get value :file)))
                     (remote-file-watch-list)))
          (when isolated
            (should-not
             (seq-some
              (lambda (buffer)
                (and (not (eq buffer edited-buffer))
                     (equal (ignore-errors (remote-buffer-target buffer)) target-id)))
              (buffer-list))))
          (when isolated
            (should-not (seq-some
                         (lambda (buffer)
                           (string-match-p "\\`\\*tramp/rpc " (buffer-name buffer)))
                         (buffer-list))))
          ;; Reading the peer's lifecycle log is a deliberate new access,
          ;; after proving cleanup remains closed for eight seconds.
          (with-temp-buffer
            (insert-file-contents events)
            (should (string-match-p "initialize\n" (buffer-string)))
            (should (string-match-p "shutdown\n" (buffer-string))))
          (message "Cleanup live: %s %s passed; only unsaved edited.c retained"
                   target-id command))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))))
      (advice-remove 'remote-connection-ensure trace-advice)
      (remote-workspace-disconnect-target target-id 'live-test-cleanup)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (when (and directory
                 (string-match-p "\\`/tmp/emacs-remote-cleanup\\.[[:alnum:]]+\\'"
                                 directory))
        (remote-exec "rm" :args (list "-rf" directory) :context context :check t))
      (remote-workspace-disconnect-target target-id 'live-test-cleanup))))

(ert-deftest remote-cleanup-live-initialized-lsp-target-teardown ()
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 and REMOTE_E2E_TARGET"))
  (remote-fs-install)
  (let ((target (or (remote-e2e--target) (ert-fail "No selected target"))))
    (remote-cleanup-live--exercise (remote-get-target "local") 'board nil)
    (dolist (command '(connection all board))
      (remote-cleanup-live--exercise target command))))

(provide 'remote-cleanup-live-tests)
;;; remote-cleanup-live-tests.el ends here
