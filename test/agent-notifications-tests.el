;;; agent-notifications-tests.el --- Quiet notification contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/notifications)

(defmacro agent-notifications-test--with-policy (here &rest body)
  "Run BODY with isolated notification state and the user HERE."
  (declare (indent 1))
  `(let ((+agent-notifications-mode t)
         (+agent-notify-events '(permission input failed))
         (+agent-notify-delay 1000) (+agent-notify-cooldown 30)
         (+agent-notify--pending (make-hash-table :test #'equal))
         (+agent-notify--timer nil) (+agent-notify--last-delivered 0)
         (+agent-notify-inhibit-functions nil) (+agent-notify-context-functions nil)
         (aaron-notify-history nil) (aaron-notify-unread 0)
         (aaron-notify-unread-functions nil) (noninteractive nil)
         (buffer (generate-new-buffer " *agent-notification-test*")) sent echoes)
     (cl-letf (((symbol-function 'aaron-notify--user-here-p) (lambda () ,here))
               ((symbol-function 'message) (lambda (&rest args) (push args echoes))))
       (let ((aaron-notify-system-function (lambda (entry) (push entry sent))))
         (unwind-protect (progn ,@body)
           (+agent-notify--cancel-timer)
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest agent-notify-c7-completion-only-enters-history ()
  (agent-notifications-test--with-policy nil
    (+agent-notify-event buffer 'completed :label "review")
    (should (= 1 (length aaron-notify-history)))
    (should-not +agent-notify--timer)
    (should-not sent)
    (should-not echoes)
    (should (= 0 aaron-notify-unread))))

(ert-deftest agent-notify-c7-foreground-does-not-overwrite-echo ()
  (agent-notifications-test--with-policy t
    (+agent-notify-event buffer 'permission)
    (+agent-notify--cancel-timer)
    (+agent-notify--flush)
    (should (= 1 (length aaron-notify-history)))
    (should-not sent)
    (should-not echoes)))

(ert-deftest agent-notify-c7-batch-never-sends-to-system ()
  (agent-notifications-test--with-policy nil
    (let ((noninteractive t))
      (+agent-notify-event buffer 'failed)
      (+agent-notify--cancel-timer)
      (+agent-notify--flush)
      (should-not sent)
      (should-not echoes))))

(ert-deftest agent-notify-c8-same-state-is-recorded-once ()
  (agent-notifications-test--with-policy nil
    (+agent-notify-event buffer 'permission)
    (+agent-notify-event buffer 'permission)
    (should (= 1 (length aaron-notify-history)))
    (should (= 1 (hash-table-count +agent-notify--pending)))
    (+agent-notify--cancel-timer)
    (+agent-notify--flush)
    (+agent-notify-event buffer 'permission)
    (should (= 1 (length sent)))
    (should-not +agent-notify--timer)
    (+agent-notify-event buffer 'resolved)
    (+agent-notify-event buffer 'permission)
    (should (= 2 (length aaron-notify-history)))))

(ert-deftest agent-notify-c8-resolved-and-cancelled-withdraw-pending ()
  (dolist (kind '(resolved cancelled started))
    (agent-notifications-test--with-policy nil
      (+agent-notify-event buffer 'input)
      (should (timerp +agent-notify--timer))
      (+agent-notify-event buffer kind)
      (should-not +agent-notify--timer)
      (+agent-notify--flush)
      (should-not sent))))

(ert-deftest agent-notify-c8-killing-actual-agent-withdraws-its-event ()
  (agent-notifications-test--with-policy nil
    ;; The event callback may run in a completely different current buffer.
    (+agent-notify-event buffer 'permission)
    (kill-buffer buffer)
    (should-not +agent-notify--timer)
    (+agent-notify--flush)
    (should-not sent)))

(ert-deftest agent-notify-c8-burst-is-one-actionable-reminder ()
  (agent-notifications-test--with-policy nil
    (let ((other (generate-new-buffer " *other-agent*")))
      (unwind-protect
          (progn
            (+agent-notify-event buffer 'permission :label "one")
            (+agent-notify-event other 'failed :label "two")
            (should-not sent)
            (+agent-notify--cancel-timer)
            (+agent-notify--flush)
            (should (= 1 (length sent)))
            (should (= 1 aaron-notify-unread))
            (should (string-match-p "2 need attention" (aaron-notify-entry-title (car sent))))
            (should (equal (aaron-notify-entry-body (car sent)) "1 awaiting approval · 1 failed"))
            (should (= 3 (length aaron-notify-history))))
        (kill-buffer other)))))

(ert-deftest agent-notify-c8-cooldown-delays-the-next-reminder ()
  (agent-notifications-test--with-policy nil
    (let ((+agent-notify--last-delivered 95) (+agent-notify-delay 2) delay)
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 100))
                ((symbol-function 'run-at-time)
                 (lambda (seconds &rest _) (setq delay seconds) (timer-create))))
        (+agent-notify-event buffer 'failed)
        (should (= 25 delay))))))

(ert-deftest agent-notify-c9-click-is-the-only-action ()
  (agent-notifications-test--with-policy nil
    (let (activated)
      (+agent-notify-event buffer 'permission :action (lambda () (setq activated t)))
      (should-not activated)
      (+agent-notify--cancel-timer)
      (+agent-notify--flush)
      (should-not activated)
      (aaron-notify-activate (aaron-notify-entry-id (car sent)))
      (should activated))))

(ert-deftest agent-notify-c9-native-events-are-structured-and-cancellable ()
  (agent-notifications-test--with-policy nil
    (+agent-notify--native-event buffer '((:event . permission-request)))
    (should (= 1 (hash-table-count +agent-notify--pending)))
    (+agent-notify--native-event buffer '((:event . permission-response)))
    (should (= 0 (hash-table-count +agent-notify--pending)))
    (+agent-notify--native-event buffer '((:event . turn-complete)
                                         (:data . ((:stop-reason . "cancelled")))))
    (should (= 1 (length aaron-notify-history)))
    (let ((+agent-notify-inhibit-functions (list (lambda (&rest _) t))))
      (+agent-notify--native-event buffer '((:event . error)))
      (should (= 1 (length aaron-notify-history))))))

(ert-deftest agent-notify-c9-subscription-restart-ignores-old-events ()
  (agent-notifications-test--with-policy nil
    (let (callbacks)
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest args) (push (plist-get args :on-event) callbacks) 'subscription)))
        (with-current-buffer buffer
          (setq-local agent-shell--state (list :generation 1))
          (+agent-notify--watch-h)
          (+agent-notify--watch-h)
          (should (= 1 (length callbacks)))
          (setq-local agent-shell--state (list :generation 2))
          (+agent-notify--watch-h))
        (should (= 2 (length callbacks)))
        (funcall (cadr callbacks) '((:event . error)))
        (should-not aaron-notify-history)
        (funcall (car callbacks) '((:event . error)))
        (should (= 1 (length aaron-notify-history)))))))

(ert-deftest agent-notify-c8-one-answer-does-not-clear-other-permissions ()
  (agent-notifications-test--with-policy nil
    (+agent-notify--native-event buffer '((:event . permission-request)
                                         (:data . ((:request-id . "p1")))))
    (+agent-notify--native-event buffer '((:event . permission-request)
                                         (:data . ((:request-id . "p2")))))
    (+agent-notify--native-event buffer '((:event . permission-response)
                                         (:data . ((:request-id . "p1")))))
    (should (= 1 (hash-table-count +agent-notify--pending)))
    (+agent-notify--native-event buffer '((:event . permission-response)
                                         (:data . ((:request-id . "p2")))))
    (should (= 0 (hash-table-count +agent-notify--pending)))
    (should-not +agent-notify--timer)))

(ert-deftest agent-notify-c8-disabling-releases-timers-and-subscriptions ()
  (agent-notifications-test--with-policy nil
    (+agent-notify-event buffer 'input)
    (let (unsubscribed)
      (with-current-buffer buffer
        (setq-local agent-shell--state '(state)
                    +agent-notify--subscription-state agent-shell--state
                    +agent-notify--subscription 'token))
      (cl-letf (((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest args) (setq unsubscribed (plist-get args :subscription)))))
        (+agent-notifications-mode -1))
      (should (eq unsubscribed 'token))
      (should-not +agent-notify--timer)
      (should (= 0 (hash-table-count +agent-notify--pending))))))


(ert-deftest agent-notify-c8-one-answer-does-not-clear-other-questions ()
  (agent-notifications-test--with-policy nil
    (dolist (id '("q1" "q2"))
      (+agent-notify--native-event buffer `((:event . elicitation-request) (:data . ((:request-id . ,id))))))
    (+agent-notify--native-event buffer '((:event . elicitation-response) (:data . ((:request-id . "q1")))))
    (should (= 1 (hash-table-count +agent-notify--pending)))
    (+agent-notify--native-event buffer '((:event . elicitation-response) (:data . ((:request-id . "q2")))))
    (should (= 0 (hash-table-count +agent-notify--pending)))
    (should-not +agent-notify--timer)))

(ert-deftest agent-notify-c8-answering-question-retains-pending-approval ()
  (agent-notifications-test--with-policy nil
    (+agent-notify--native-event buffer '((:event . permission-request) (:data . ((:request-id . "p")))))
    (+agent-notify--native-event buffer '((:event . elicitation-request) (:data . ((:request-id . "q")))))
    (+agent-notify--native-event buffer '((:event . elicitation-response) (:data . ((:request-id . "q")))))
    (should (= 1 (hash-table-count +agent-notify--pending)))
    (+agent-notify--native-event buffer '((:event . permission-response) (:data . ((:request-id . "p")))))
    (should (= 0 (hash-table-count +agent-notify--pending)))))

(provide 'agent-notifications-tests)
;;; agent-notifications-tests.el ends here
