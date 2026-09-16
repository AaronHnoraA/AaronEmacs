;;; init-ui-dashboard-tests.el --- Startup dashboard Agenda card tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-ui-dashboard-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-ui)

(ert-deftest my/dashboard-agenda-reports-a-retryable-host-failure ()
  "A failed Agenda answer must replace `loading' with a retryable line."
  (with-temp-buffer
    (my/dashboard--insert-agenda-card-content nil '((message . "host down")))
    (goto-char (point-min))
    (should (search-forward "Agenda unavailable · host down" nil t))
    (should (search-forward "[retry]" nil t))
    (should (button-at (1- (point))))))

(ert-deftest my/dashboard-agenda-insertion-only-reads-the-complete-cache ()
  "Dashboard construction must not start a request or rewrite itself later."
  (require 'noema-agenda)
  (let ((my/dashboard--agenda-snapshot
         '((stats . ((open . 3) (doing . 1) (blocked . 0) (overdue . 2)))
           (todos . []) (days . [])))
        (my/dashboard--agenda-error nil)
        (queries 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
                 (lambda (_callback) (cl-incf queries))))
        (my/dashboard-insert-agenda)
        (should (= queries 0))
        (goto-char (point-min))
        (should (search-forward "3 open   1 doing   0 blocked   2 overdue" nil t))))))

(ert-deftest my/dashboard-agenda-includes-unscheduled-roam-work ()
  "The compact card falls back to open todos outside the date buckets."
  (with-temp-buffer
    (my/dashboard--insert-agenda-card-content
     '((stats . ((open . 1) (doing . 0) (blocked . 0) (overdue . 0)))
       (todos . [((uid . "roam") (text . "Read the Roam task")
                  (status . "todo") (effectiveStatus . "todo"))])
       (days . [((date . "2026-09-16") (entries . []))]))
     nil)
    (goto-char (point-min))
    (should (search-forward "open  TODO" nil t))
    (should (search-forward "Read the Roam task" nil t))))

(ert-deftest my/dashboard-agenda-cache-refresh-coalesces-callers ()
  "One host query publishes a complete snapshot to every waiting caller."
  (require 'noema-agenda)
  (let ((my/dashboard--agenda-snapshot nil)
        (my/dashboard--agenda-error nil)
        (my/dashboard--agenda-request-pending nil)
        (my/dashboard--agenda-waiters nil)
        (queries 0)
        callback
        answers)
    (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
               (lambda (cb) (cl-incf queries) (setq callback cb))))
      (my/dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'first changed) answers)))
      (my/dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'second changed) answers)))
      (should (= queries 1))
      (funcall callback
               '((stats . ((open . 1))) (todos . []) (days . [])) nil)
      (should (equal (mapcar #'car answers) '(second first)))
      (should (cl-every #'cdr answers))
      (should-not my/dashboard--agenda-request-pending)
      (should (= (alist-get 'open
                            (alist-get 'stats my/dashboard--agenda-snapshot))
                 1)))))

(ert-deftest my/dashboard-refresh-restores-chunlian-after-content ()
  "A complete Dashboard refresh clears old overlays and restores them last."
  (let (calls)
    (with-temp-buffer
      (dashboard-mode)
      (setq-local chunlian-mode t)
      (cl-letf (((symbol-function 'chunlian--clear-display)
                 (lambda () (push 'clear calls)))
                ((symbol-function 'dashboard-refresh-buffer)
                 (lambda () (push 'refresh calls)))
                ((symbol-function 'chunlian-mode)
                 (lambda (&optional _arg) (push 'chunlian calls))))
        (my/dashboard--refresh-buffer-completely (current-buffer))
        (should (equal (nreverse calls) '(clear refresh chunlian)))))))

(ert-deftest my/dashboard-agenda-change-does-no-io-while-hidden ()
  "A hidden Dashboard only marks its cache dirty."
  (let ((my/dashboard--agenda-dirty nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer) (lambda (_name) (current-buffer)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'my/dashboard--refresh-agenda-cache)
                 (lambda (&rest _) (ert-fail "hidden Dashboard queried Agenda"))))
        (my/dashboard-agenda-handle-change)
        (should my/dashboard--agenda-dirty)))))

(ert-deftest my/dashboard-agenda-host-stop-releases-request-resources ()
  "Stopping the host cancels the deadline and drops retained callbacks."
  (let ((my/dashboard--agenda-request-generation 2)
        (my/dashboard--agenda-request-pending t)
        (my/dashboard--agenda-dirty nil)
        (my/dashboard--agenda-waiters (list (lambda (_) nil)))
        (my/dashboard--agenda-request-timer (run-at-time 60 nil #'ignore)))
    (my/dashboard-agenda-host-stopped)
    (should (= my/dashboard--agenda-request-generation 3))
    (should-not my/dashboard--agenda-request-pending)
    (should-not my/dashboard--agenda-request-timer)
    (should-not my/dashboard--agenda-waiters)
    (should my/dashboard--agenda-dirty)))

(ert-deftest my/dashboard-agenda-is-the-last-section-before-the-footer ()
  "The Agenda card stays at the bottom of the startup dashboard."
  (let* ((list (remq 'dashboard-insert-newline dashboard-startupify-list))
         (tail (last list 2)))
    (should (equal tail '(my/dashboard-insert-agenda dashboard-insert-footer)))))

(provide 'init-ui-dashboard-tests)
;;; init-ui-dashboard-tests.el ends here
