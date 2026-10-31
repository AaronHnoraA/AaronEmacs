;;; agent-notifications-noema-tests.el --- Research notification boundary -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/notifications)
(require 'noema/bridge/agent-notifications)
(require 'tools/agent/acp)
(require 'noema-agent-worker)

(ert-deftest agent-notify-c9-noema-custom-callback-remains-owned-by-user ()
  (let ((+agent-acp-notify-function #'ignore)
        (noema-agent-worker-notify-function #'ignore))
    (should-not (+agent-notify--runtime-attention (current-buffer) 'permission))
    (should-not (+noema-agent-notifications--worker nil 'completed))))

(ert-deftest agent-notify-c9-noema-disabled-callback-stays-disabled ()
  (let ((+agent-acp-notify-function nil)
        (noema-agent-worker-notify-function nil))
    (cl-letf (((symbol-function '+agent-notify-event)
               (lambda (&rest _) (ert-fail "Disabled callback must stay quiet"))))
      (should (+agent-notify--runtime-attention (current-buffer) 'permission))
      (should (+noema-agent-notifications--worker nil 'completed)))))

(ert-deftest agent-notify-c9-noema-manual-session-emits-structured-event ()
  (with-temp-buffer
    (setq-local +agent-acp-session-name "review")
    (let ((+agent-acp-notify-function #'+agent-acp-notify-default) received)
      (cl-letf (((symbol-function '+agent-notify-event)
                 (lambda (&rest args) (setq received args))))
        (should (+agent-notify--runtime-attention (current-buffer) 'done))
        (should (eq (cadr received) 'completed))
        (should (equal (plist-get (cddr received) :label) "review"))))))

(ert-deftest agent-notify-c9-noema-run-uses-run-identity ()
  (with-temp-buffer
    (let* ((worker (noema-agent-worker--create
                    :run-id "r1" :buffer (current-buffer)))
           (noema-agent-worker-notify-function #'noema-agent-worker-notify-default) received)
      (cl-letf (((symbol-function '+agent-notify-event)
                 (lambda (&rest args) (setq received args)))
                ((symbol-function 'noema-agent-worker--session-name)
                 (lambda (_) "review")))
        (should (+noema-agent-notifications--worker worker 'input))
        (should (eq (cadr received) 'input))
        (should (equal (plist-get (cddr received) :id) "r1"))
        (should (equal (plist-get (cddr received) :source) "Noema Research"))))))

(ert-deftest agent-notify-c9-noema-run-and-ephemeral-events-are-not-doubled ()
  (with-temp-buffer
    (let ((noema-agent-worker--runs (make-hash-table :test #'equal)))
      (puthash "r1" (noema-agent-worker--create :run-id "r1" :buffer (current-buffer))
               noema-agent-worker--runs)
      (should (+noema-agent-notifications--inhibit (current-buffer) 'failed))
      (clrhash noema-agent-worker--runs)
      (setq-local +agent-acp-session-origin 'side)
      (should (+agent-notify--runtime-inhibit (current-buffer) 'completed)))))

(ert-deftest agent-notify-c9-noema-waiting-is-per-worker-and-quiet ()
  (let* ((noema-agent-worker--attention-count 0) events echoed
        (noema-agent-worker-attention-changed-functions nil)
        (noema-agent-worker-notification-functions
         (list (lambda (worker kind) (push (cons worker kind) events) t))))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq echoed t))))
      (let ((one (noema-agent-worker--create :pending-permissions '("p1")))
            (two (noema-agent-worker--create :pending-inputs '("i1"))))
        (noema-agent-worker--attention-note 1 one)
        (noema-agent-worker--attention-note 1 two)
        (should (equal (mapcar #'cdr (reverse events)) '(permission input)))
        (should-not echoed)
        (setf (noema-agent-worker-pending-permissions one) nil)
        (noema-agent-worker--attention-note -1 one)
        (should (eq (cdar events) 'resolved))))))

(ert-deftest agent-notify-c2-noema-completion-waits-for-terminal-ack ()
  (let* ((worker (noema-agent-worker--create :run-id "r1"))
         (noema-agent-worker--runs (make-hash-table :test #'equal)) events
         (noema-agent-worker-notification-functions
          (list (lambda (candidate kind)
                  (should (eq worker candidate))
                  (should (noema-agent-worker-terminal-acked worker))
                  (should-not (gethash "r1" noema-agent-worker--runs))
                  (push kind events) t))))
    (puthash "r1" worker noema-agent-worker--runs)
    (cl-letf (((symbol-function 'noema-agent-worker--stop-renewal) #'ignore)
              ((symbol-function 'noema-agent-worker--best-effort) #'ignore))
      (noema-agent-worker--ack-terminal worker "completed")
      (noema-agent-worker--ack-terminal worker "completed")
      (should (equal events '(completed))))))

(ert-deftest agent-notify-c8-noema-terminal-cleanup-cannot-requeue-approval ()
  (let* ((noema-agent-worker--attention-count 1)
         (worker (noema-agent-worker--create :terminal t :pending-permissions '("p1")))
         events
         (noema-agent-worker-notification-functions
          (list (lambda (_ kind) (push kind events) t))))
    (noema-agent-worker--attention-note -1 worker)
    (should (equal events '(resolved)))))

(ert-deftest agent-notify-c8-noema-cancel-withdraws-before-grace-period ()
  (let* ((worker (noema-agent-worker--create)) events
         (noema-agent-worker-notification-functions
          (list (lambda (_ kind) (push kind events) t))))
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-worker--best-effort) #'ignore))
          (noema-agent-worker--cancel worker)
          (should (equal events '(resolved)))
          (should-not (noema-agent-worker-terminal-acked worker)))
      (when (timerp (noema-agent-worker-cancel-timer worker))
        (cancel-timer (noema-agent-worker-cancel-timer worker))))))

(provide 'agent-notifications-noema-tests)
;;; agent-notifications-noema-tests.el ends here
