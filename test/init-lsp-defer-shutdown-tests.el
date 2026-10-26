;;; init-lsp-defer-shutdown-tests.el --- G-L1 deferred last-buffer stop -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-lsp-defer-shutdown-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lsp-mode)

(defmacro init-lsp-defer-tests--with-workspace (&rest body)
  "Run BODY with `workspace' bound to a fake lsp-mode workspace.
Its process is a live `cat'; `stops' collects the REASONs passed to
`+lsp-mode-shutdown-workspace', whose real definition is `real-stop'."
  (declare (indent 0))
  `(let* ((process (make-process :name "lsp-defer-test" :command '("cat")
                                 :connection-type 'pipe :noquery t))
          (workspace (make-lsp--workspace :proc process :cmd-proc process))
          (stops nil)
          (real-stop (symbol-function '+lsp-mode-shutdown-workspace))
          (+language-server-defer-shutdown 0.2)
          (lsp-keep-workspace-alive nil))
     (ignore real-stop)
     (unwind-protect
         (cl-letf (((symbol-function '+lsp-mode-shutdown-workspace)
                    (lambda (_workspace &optional reason)
                      (push reason stops)))
                   ((symbol-function 'lsp-workspaces) (lambda () (list workspace))))
           (with-temp-buffer
             ,@body))
       (+lsp-mode--cancel-deferred-shutdown workspace)
       (when (process-live-p process)
         (delete-process process)))))

(defun init-lsp-defer-tests--close (expected-keep)
  "Close the current buffer's document, checking FN gets EXPECTED-KEEP."
  (+lsp-mode--defer-last-buffer-shutdown-a
   (lambda (&optional keep)
     (should (eq (and keep t) expected-keep)))))

(defun init-lsp-defer-tests--wait ()
  "Wait past the 0.2 second deferral."
  (let ((deadline (+ (float-time) 0.5)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(ert-deftest +lsp-defer-closing-last-buffer-stops-the-server-later ()
  "The stop that closing the last buffer starts waits, then runs."
  (should (advice-member-p #'+lsp-mode--defer-last-buffer-shutdown-a
                           'lsp--text-document-did-close))
  (init-lsp-defer-tests--with-workspace
    (init-lsp-defer-tests--close t)
    (should (gethash workspace +lsp-mode--deferred-shutdowns))
    (should-not stops)
    (init-lsp-defer-tests--wait)
    (should (equal stops '(last-buffer-closed)))
    (should-not (gethash workspace +lsp-mode--deferred-shutdowns))))

(ert-deftest +lsp-defer-a-buffer-joining-in-time-keeps-the-server ()
  "A file of the project opened during the delay keeps the server running."
  (init-lsp-defer-tests--with-workspace
    (init-lsp-defer-tests--close t)
    (setf (lsp--workspace-buffers workspace) (list (current-buffer)))
    (init-lsp-defer-tests--wait)
    (should-not stops)))

(ert-deftest +lsp-defer-disconnected-buffers-stop-at-once ()
  "Buffers of an explicitly disconnected target are never deferred."
  (init-lsp-defer-tests--with-workspace
    (setq-local remote-buffer-disconnected-p t)
    (init-lsp-defer-tests--close nil)
    (should-not (gethash workspace +lsp-mode--deferred-shutdowns))))

(ert-deftest +lsp-defer-explicit-stop-cancels-the-pending-one ()
  "An explicit stop during the delay wins, and the timer does not run again."
  (init-lsp-defer-tests--with-workspace
    (init-lsp-defer-tests--close t)
    (cl-letf (((symbol-function 'lsp-workspace-shutdown) #'ignore))
      (funcall real-stop workspace 'user-shutdown))
    (should-not (gethash workspace +lsp-mode--deferred-shutdowns))
    (init-lsp-defer-tests--wait)
    (should-not stops)))

(ert-deftest +lsp-defer-a-server-that-died-meanwhile-is-left-alone ()
  "Failure injection: the process dies during the delay; nothing is stopped."
  (init-lsp-defer-tests--with-workspace
    (init-lsp-defer-tests--close t)
    (delete-process process)
    (init-lsp-defer-tests--wait)
    (should-not stops)))

(ert-deftest +lsp-defer-disabled-or-kept-alive-closes-as-before ()
  "With no delay, or `lsp-keep-workspace-alive', lsp-mode decides as before."
  (init-lsp-defer-tests--with-workspace
    (let ((+language-server-defer-shutdown nil))
      (init-lsp-defer-tests--close nil))
    (let ((lsp-keep-workspace-alive t))
      (init-lsp-defer-tests--close nil))
    (should-not (gethash workspace +lsp-mode--deferred-shutdowns))))

(provide 'init-lsp-defer-shutdown-tests)
;;; init-lsp-defer-shutdown-tests.el ends here
