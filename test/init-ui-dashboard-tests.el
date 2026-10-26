;;; init-ui-dashboard-tests.el --- Startup dashboard Agenda card tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-ui-dashboard-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ui/theme/config)

(defvar +dashboard-test-read-eval nil)

;; Never touch the real session cache from tests.
(setq +dashboard-agenda-cache-file
      (make-temp-file "dashboard-agenda-test-" nil ".eld"))

(ert-deftest +dashboard-agenda-reports-a-retryable-host-failure ()
  "A failed Agenda answer must replace `loading' with a retryable line."
  (with-temp-buffer
    (+dashboard--insert-agenda-card-content nil '((message . "host down")))
    (goto-char (point-min))
    (should (search-forward "Agenda unavailable · host down" nil t))
    (should (search-forward "[retry]" nil t))
    (should (button-at (1- (point))))))

(ert-deftest +dashboard-agenda-insertion-only-reads-the-complete-cache ()
  "Dashboard construction must not start a request or rewrite itself later."
  (require 'noema-agenda)
  (let ((+dashboard--agenda-snapshot
         '((stats . ((open . 3) (doing . 1) (blocked . 0) (overdue . 2)))
           (todos . []) (days . [])))
        (+dashboard--agenda-error nil)
        (queries 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
                 (lambda (_callback) (cl-incf queries))))
        (+dashboard-insert-agenda)
        (should (= queries 0))
        (goto-char (point-min))
        (should (search-forward "3 open   1 doing   0 blocked   2 overdue" nil t))))))

(ert-deftest +dashboard-agenda-includes-unscheduled-roam-work ()
  "The compact card falls back to open todos outside the date buckets."
  (with-temp-buffer
    (+dashboard--insert-agenda-card-content
     '((stats . ((open . 1) (doing . 0) (blocked . 0) (overdue . 0)))
       (todos . [((uid . "roam") (text . "Read the Roam task")
                  (status . "todo") (effectiveStatus . "todo"))])
       (days . [((date . "2026-09-16") (entries . []))]))
     nil)
    (goto-char (point-min))
    (should (search-forward "open  TODO" nil t))
    (should (search-forward "Read the Roam task" nil t))))

(ert-deftest +dashboard-agenda-cache-refresh-coalesces-callers ()
  "One host query publishes a complete snapshot to every waiting caller."
  (require 'noema-agenda)
  (let ((+dashboard--agenda-snapshot nil)
        (+dashboard--agenda-error nil)
        (+dashboard--agenda-request-pending nil)
        (+dashboard--agenda-waiters nil)
        (queries 0)
        callback
        answers)
    (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
               (lambda (cb) (cl-incf queries) (setq callback cb))))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'first changed) answers)))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'second changed) answers)))
      (should (= queries 1))
      (funcall callback
               '((stats . ((open . 1))) (todos . []) (days . [])) nil)
      (should (equal (mapcar #'car answers) '(second first)))
      (should (cl-every #'cdr answers))
      (should-not +dashboard--agenda-request-pending)
      (should (= (alist-get 'open
                            (alist-get 'stats +dashboard--agenda-snapshot))
                 1)))))

(ert-deftest +dashboard-unchanged-agenda-is-not-rendered-twice ()
  "An equal host answer keeps the rendered snapshot, so nothing rebuilds."
  (require 'noema-agenda)
  (let* ((snapshot '((stats . ((open . 1))) (todos . []) (days . [])))
         (+dashboard--agenda-snapshot snapshot)
         (+dashboard--agenda-error nil)
         (+dashboard--agenda-request-pending nil)
         (+dashboard--agenda-waiters nil)
         (+dashboard--agenda-dirty t)
         (+dashboard--agenda-cache-loaded t)
         (+dashboard--rendered-agenda 'unrendered)
         callback changed-answer)
    (with-temp-buffer (+dashboard-insert-agenda))
    (should (+dashboard--rendered-current-p))
    (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
               (lambda (cb) (setq callback cb))))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (setq changed-answer (list changed))))
      (funcall callback (copy-tree snapshot) nil))
    (should (equal changed-answer '(nil)))
    (should (eq +dashboard--agenda-snapshot snapshot))
    (should (+dashboard--rendered-current-p))))

(ert-deftest +dashboard-startup-renders-the-last-session-agenda ()
  "The first render uses the saved snapshot; a changed answer is saved."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-seed-" nil ".eld"))
         (saved '((stats . ((open . 7) (doing . 0) (blocked . 0) (overdue . 0)))
                  (todos . []) (days . [])))
         (+dashboard--agenda-snapshot nil)
         (+dashboard--agenda-error nil)
         (+dashboard--agenda-cache-loaded nil)
         (+dashboard--rendered-agenda 'unrendered))
    (unwind-protect
        (progn
          (+dashboard--save-agenda-cache saved)
          (with-temp-buffer
            (+dashboard-insert-agenda)
            (goto-char (point-min))
            (should (search-forward "7 open" nil t)))
          (should (equal +dashboard--agenda-snapshot saved))
          ;; A different answer replaces the file for the next session.
          (let ((+dashboard--agenda-request-pending nil))
            (+dashboard--finish-agenda-refresh
             '((stats . ((open . 2))) (todos . []) (days . [])) nil))
          (should (= 2 (alist-get 'open
                                  (alist-get 'stats
                                             (with-temp-buffer
                                               (insert-file-contents
                                                +dashboard-agenda-cache-file)
                                               (read (current-buffer))))))))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-agenda-cache-rejects-reader-code-and-trailing-data ()
  "A damaged cache cannot evaluate Lisp or supply a partial snapshot."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-untrusted-" nil ".eld"))
         (+dashboard--agenda-cache-loaded nil)
         (+dashboard--agenda-snapshot nil)
         (+dashboard-test-read-eval nil)
         (read-eval t))
    (unwind-protect
        (progn
          (with-temp-file +dashboard-agenda-cache-file
            (insert "#.(progn (setq +dashboard-test-read-eval t) '((stats . nil)))"))
          (+dashboard--load-agenda-cache)
          (should-not +dashboard-test-read-eval)
          (should-not +dashboard--agenda-snapshot)
          (with-temp-file +dashboard-agenda-cache-file
            (insert "((stats . nil)) ((stats . ((open . 99))))"))
          (setq +dashboard--agenda-cache-loaded nil)
          (+dashboard--load-agenda-cache)
          (should-not +dashboard--agenda-snapshot))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-agenda-cache-rename-failure-preserves-last-good-file ()
  "A failed cache replacement leaves the previous session snapshot intact."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-atomic-" nil ".eld"))
         (old '((stats . ((open . 7))) (todos . []) (days . [])))
         (new '((stats . ((open . 2))) (todos . []) (days . []))))
    (unwind-protect
        (progn
          (+dashboard--save-agenda-cache old)
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (&rest _) (error "simulated rename failure"))))
            (+dashboard--save-agenda-cache new))
          (with-temp-buffer
            (insert-file-contents +dashboard-agenda-cache-file)
            (should (equal (read (current-buffer)) old)))
          (should-not
           (directory-files
            (file-name-directory +dashboard-agenda-cache-file)
            nil
            (concat "^" (regexp-quote
                           (file-name-nondirectory +dashboard-agenda-cache-file))
                    "\\.tmp-"))))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-refresh-restores-chunlian-after-content ()
  "A complete Dashboard refresh clears old overlays and restores them last."
  (let (calls)
    (with-temp-buffer
      (dashboard-mode)
      (setq-local chunlian-mode t)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (cl-letf (((symbol-function 'chunlian--clear-display)
                   (lambda () (push 'clear calls)))
                  ((symbol-function 'dashboard-refresh-buffer)
                   (lambda () (push 'refresh calls)))
                  ((symbol-function 'chunlian-mode)
                   (lambda (&optional _arg) (push 'chunlian calls))))
          (+dashboard--refresh-buffer-completely (current-buffer))
          (should (equal (nreverse calls) '(clear refresh chunlian))))))))

(ert-deftest +dashboard-agenda-change-does-no-io-while-hidden ()
  "A hidden Dashboard only marks its cache dirty."
  (let ((+dashboard--agenda-dirty nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer) (lambda (_name) (current-buffer)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function '+dashboard--refresh-agenda-cache)
                 (lambda (&rest _) (ert-fail "hidden Dashboard queried Agenda"))))
        (+dashboard-agenda-handle-change)
        (should +dashboard--agenda-dirty)))))

(ert-deftest +dashboard-agenda-host-stop-releases-request-resources ()
  "Stopping the host cancels the deadline and drops retained callbacks."
  (let ((+dashboard--agenda-request-generation 2)
        (+dashboard--agenda-request-pending t)
        (+dashboard--agenda-dirty nil)
        (+dashboard--agenda-waiters (list (lambda (_) nil)))
        (+dashboard--agenda-request-timer (run-at-time 60 nil #'ignore)))
    (+dashboard-agenda-host-stopped)
    (should (= +dashboard--agenda-request-generation 3))
    (should-not +dashboard--agenda-request-pending)
    (should-not +dashboard--agenda-request-timer)
    (should-not +dashboard--agenda-waiters)
    (should +dashboard--agenda-dirty)))

(ert-deftest +dashboard-agenda-is-the-last-section-before-the-footer ()
  "The Agenda card stays at the bottom of the startup dashboard."
  (let* ((list (remq 'dashboard-insert-newline dashboard-startupify-list))
         (tail (last list 2)))
    (should (equal tail '(+dashboard-insert-agenda dashboard-insert-footer)))))

(provide 'init-ui-dashboard-tests)
;;; init-ui-dashboard-tests.el ends here
