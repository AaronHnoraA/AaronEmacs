;;; init-background-tests.el --- External action entry tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-background-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-background)

(ert-deftest background-raycast-manages-only-graphical-frames ()
  "A TTY client cannot mask the last GUI frame or appear in Raycast."
  (let ((graphical (selected-frame))
        (terminal 'test-terminal-frame))
    (cl-letf (((symbol-function 'frame-list)
               (lambda () (list graphical terminal)))
              ((symbol-function 'frame-parent) (lambda (_frame) nil))
              ((symbol-function 'frame-live-p) (lambda (_frame) t))
              ((symbol-function 'frame-visible-p) (lambda (_frame) t))
              ((symbol-function 'display-graphic-p)
               (lambda (frame) (eq frame graphical))))
      (should (equal (my/background--editing-frames) (list graphical)))
      (should (my/background--sole-visible-p graphical))
      (should-not (my/background--sole-visible-p terminal))
      (should (equal (my/background-status) "visible"))
      (should-error (my/background--hide-frame terminal)
                    :type 'user-error))))

(ert-deftest background-act-rejects-malformed-action-ids ()
  "An invalid launcher ID must not silently select action zero."
  (let ((my/background-actions '(("Toggle" my/background-toggle)))
        (argument (base64-encode-string "" t)))
    (dolist (id '("bad" "0oops" "-1" "1"))
      (should-error
       (my/background-act (base64-encode-string id t) argument)
       :type 'user-error))))

(ert-deftest background-action-receipt-reports-deferred-outcome ()
  "The launcher can distinguish acceptance, completion and later failure."
  (should (= my/background-action-protocol 1))
  (let ((my/background-actions '(("Probe" ignore)))
        (my/background--requests (make-hash-table :test #'equal))
        deferred fail (shown 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat callback &rest args)
                 (setq deferred (cons callback args))))
              ((symbol-function 'my/background--act)
               (lambda (_action _argument)
                 (when fail (error "deferred probe failure"))))
              ((symbol-function 'my/background-show)
               (lambda () (cl-incf shown))))
      (dolist (failed '(nil t))
        (setq fail failed)
        (let* ((receipt
                (json-parse-string
                 (base64-decode-string
                  (my/background-act (base64-encode-string "0" t)
                                     (base64-encode-string "" t)))
                 :object-type 'plist))
               (id (plist-get receipt :id))
               (encoded (base64-encode-string id t)))
          (should (string-match-p "\\`[0-9a-f]+\\'" id))
          (should (equal (plist-get
                          (json-parse-string
                           (base64-decode-string (my/background-result encoded))
                           :object-type 'plist)
                          :status)
                         "pending"))
          (apply (car deferred) (cdr deferred))
          (let ((result
                 (json-parse-string
                  (base64-decode-string (my/background-result encoded))
                  :object-type 'plist)))
            (should (equal (plist-get result :status)
                           (if failed "error" "ok")))
            (when failed
              (should (string-match-p "deferred probe failure"
                                      (plist-get result :message)))))))
      (should (= shown 1))
      (should (equal
               (plist-get
                (json-parse-string
                 (base64-decode-string
                  (my/background-result (base64-encode-string "unknown" t)))
                 :object-type 'plist)
                :status)
               "unknown")))))

(ert-deftest background-frame-receipt-reports-deferred-failure ()
  "A frame that disappears after validation fails in the queryable result."
  (let ((my/background--requests (make-hash-table :test #'equal))
        deferred)
    (cl-letf (((symbol-function 'my/background--editing-frames)
               (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat callback &rest args)
                 (setq deferred (cons callback args))))
              ((symbol-function 'my/background--frame-act)
               (lambda (_action _id) (user-error "frame vanished")))
              ((symbol-function 'my/background-show) #'ignore))
      (let* ((receipt
              (json-parse-string
               (base64-decode-string
                (my/background-frame-act
                 (base64-encode-string "new" t)
                 (base64-encode-string "" t)))
               :object-type 'plist))
             (id (plist-get receipt :id)))
        (apply (car deferred) (cdr deferred))
        (let ((result
               (json-parse-string
                (base64-decode-string
                 (my/background-result (base64-encode-string id t)))
                :object-type 'plist)))
          (should (equal (plist-get result :status) "error"))
          (should (string-match-p "frame vanished"
                                  (plist-get result :message))))))))

(ert-deftest background-request-store-expires-and-rolls-back-queue-errors ()
  "Old receipts and a failed timer registration leave no pending records."
  (let ((my/background-actions '(("Probe" ignore)))
        (my/background--requests (make-hash-table :test #'equal)))
    (puthash "old" (list :status "ok"
                          :created (- (float-time) 301))
             my/background--requests)
    (should (equal
             (plist-get
              (json-parse-string
               (base64-decode-string
                (my/background-result (base64-encode-string "old" t)))
               :object-type 'plist)
              :status)
             "unknown"))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (error "timer unavailable"))))
      (should-error
       (my/background-act (base64-encode-string "0" t)
                          (base64-encode-string "" t))))
    (should (= (hash-table-count my/background--requests) 0))))

(ert-deftest background-deferred-action-failure-becomes-visible ()
  "An asynchronous launcher failure must reveal Emacs and name the action."
  (let ((shown 0))
    (cl-letf (((symbol-function 'my/background--act)
               (lambda (_action _argument) (error "test action failure")))
              ((symbol-function 'my/background-show)
               (lambda () (cl-incf shown))))
      (my/background--act-safely '("Open File" find-file "Path") "bad")
      (should (= shown 1))
      (with-current-buffer "*Messages*"
        (goto-char (point-min))
        (should (search-forward "Emacs action Open File failed: test action failure"
                                nil t))))))

(ert-deftest background-frame-act-rejects-invalid-requests ()
  "An unknown frame or action fails before a deferred task is scheduled."
  (let ((scheduled 0)
        (empty (base64-encode-string "" t)))
    (cl-letf (((symbol-function 'my/background--editing-frames)
               (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) (cl-incf scheduled))))
      (should-error
       (my/background-frame-act (base64-encode-string "bogus" t) empty)
       :type 'user-error)
      (should-error
       (my/background-frame-act (base64-encode-string "focus" t)
                                (base64-encode-string "missing" t))
       :type 'user-error)
      (should (= scheduled 0))
      (my/background-frame-act (base64-encode-string "new" t) empty)
      (should (= scheduled 1)))))

(ert-deftest background-deferred-frame-action-reports-a-closed-frame ()
  "A frame can vanish between request validation and deferred execution."
  (let ((shown 0))
    (cl-letf (((symbol-function 'my/background--editing-frames)
               (lambda () nil))
              ((symbol-function 'my/background-show)
               (lambda () (cl-incf shown))))
      (my/background--frame-act-safely "focus" "closed-frame")
      (should (= shown 1))
      (with-current-buffer "*Messages*"
        (goto-char (point-min))
        (should (search-forward
                 "Emacs frame action focus failed: Frame closed-frame closed before the action ran"
                 nil t))))))

(ert-deftest background-frame-factory-cleans-up-after-sizing-failure ()
  (let ((source (generate-new-buffer "*frame-factory-source*"))
        created-from deleted)
    (unwind-protect
        (with-current-buffer source
          (cl-letf (((symbol-function 'make-frame)
                     (lambda (&rest _)
                       (setq created-from (current-buffer))
                       (selected-frame)))
                    ((symbol-function 'my/frame-fit-default)
                     (lambda (_frame) (error "Sizing failed")))
                    ((symbol-function 'delete-frame)
                     (lambda (frame &rest _) (setq deleted frame))))
            (should-error (my/background-make-frame))))
      (kill-buffer source))
    (should (memq created-from
                  (list (get-buffer "*dashboard*")
                        (get-buffer "*scratch*"))))
    (should (eq deleted (selected-frame)))))

(ert-deftest background-frame-factory-keeps-successful-frame ()
  (let (deleted)
    (cl-letf (((symbol-function 'make-frame)
               (lambda (&rest _) (selected-frame)))
              ((symbol-function 'my/frame-fit-default)
               (lambda (_frame) nil))
              ((symbol-function 'delete-frame)
               (lambda (frame &rest _) (setq deleted frame))))
      (should (eq (my/background-make-frame) (selected-frame))))
    (should-not deleted)))

(provide 'init-background-tests)
;;; init-background-tests.el ends here
