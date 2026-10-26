;;; init-background-tests.el --- External action entry tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-background-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config/background/config)
(require 'term/portal/config)

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
      (should (equal (+background--editing-frames) (list graphical)))
      (should (+background--sole-visible-p graphical))
      (should-not (+background--sole-visible-p terminal))
      (should (equal (+background-status) "visible"))
      (should-error (+background--hide-frame terminal)
                    :type 'user-error))))

(ert-deftest background-act-rejects-malformed-action-ids ()
  "An invalid launcher ID must not silently select action zero."
  (let ((+background-actions '(("Toggle" +background/toggle)))
        (argument (base64-encode-string "" t)))
    (dolist (id '("bad" "0oops" "-1" "1"))
      (should-error
       (+background-act (base64-encode-string id t) argument)
       :type 'user-error))))

(ert-deftest background-action-receipt-reports-deferred-outcome ()
  "The launcher can distinguish acceptance, completion and later failure."
  (should (= +background-action-protocol 1))
  (let ((+background-actions '(("Probe" ignore)))
        (+background--requests (make-hash-table :test #'equal))
        deferred fail (shown 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat callback &rest args)
                 (setq deferred (cons callback args))))
              ((symbol-function '+background--act)
               (lambda (_action _argument)
                 (when fail (error "deferred probe failure"))))
              ((symbol-function '+background/show)
               (lambda () (cl-incf shown))))
      (dolist (failed '(nil t))
        (setq fail failed)
        (let* ((receipt
                (json-parse-string
                 (base64-decode-string
                  (+background-act (base64-encode-string "0" t)
                                     (base64-encode-string "" t)))
                 :object-type 'plist))
               (id (plist-get receipt :id))
               (encoded (base64-encode-string id t)))
          (should (string-match-p "\\`[0-9a-f]+\\'" id))
          (should (equal (plist-get
                          (json-parse-string
                           (base64-decode-string (+background-result encoded))
                           :object-type 'plist)
                          :status)
                         "pending"))
          (apply (car deferred) (cdr deferred))
          (let ((result
                 (json-parse-string
                  (base64-decode-string (+background-result encoded))
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
                  (+background-result (base64-encode-string "unknown" t)))
                 :object-type 'plist)
                :status)
               "unknown")))))

(ert-deftest background-frame-receipt-reports-deferred-failure ()
  "A frame that disappears after validation fails in the queryable result."
  (let ((+background--requests (make-hash-table :test #'equal))
        deferred)
    (cl-letf (((symbol-function '+background--editing-frames)
               (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat callback &rest args)
                 (setq deferred (cons callback args))))
              ((symbol-function '+background--frame-act)
               (lambda (_action _id) (user-error "frame vanished")))
              ((symbol-function '+background/show) #'ignore))
      (let* ((receipt
              (json-parse-string
               (base64-decode-string
                (+background-frame-act
                 (base64-encode-string "new" t)
                 (base64-encode-string "" t)))
               :object-type 'plist))
             (id (plist-get receipt :id)))
        (apply (car deferred) (cdr deferred))
        (let ((result
               (json-parse-string
                (base64-decode-string
                 (+background-result (base64-encode-string id t)))
                :object-type 'plist)))
          (should (equal (plist-get result :status) "error"))
          (should (string-match-p "frame vanished"
                                  (plist-get result :message))))))))

(ert-deftest background-request-store-expires-and-rolls-back-queue-errors ()
  "Old receipts and a failed timer registration leave no pending records."
  (let ((+background-actions '(("Probe" ignore)))
        (+background--requests (make-hash-table :test #'equal)))
    (puthash "old" (list :status "ok"
                          :created (- (float-time) 301))
             +background--requests)
    (should (equal
             (plist-get
              (json-parse-string
               (base64-decode-string
                (+background-result (base64-encode-string "old" t)))
               :object-type 'plist)
              :status)
             "unknown"))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (error "timer unavailable"))))
      (should-error
       (+background-act (base64-encode-string "0" t)
                          (base64-encode-string "" t))))
    (should (= (hash-table-count +background--requests) 0))))

(ert-deftest background-deferred-action-failure-becomes-visible ()
  "An asynchronous launcher failure must reveal Emacs and name the action."
  (let ((shown 0))
    (cl-letf (((symbol-function '+background--act)
               (lambda (_action _argument) (error "test action failure")))
              ((symbol-function '+background/show)
               (lambda () (cl-incf shown))))
      (+background--act-safely '("Open File" find-file "Path") "bad")
      (should (= shown 1))
      (with-current-buffer "*Messages*"
        (goto-char (point-min))
        (should (search-forward "Emacs action Open File failed: test action failure"
                                nil t))))))

(ert-deftest background-frame-act-rejects-invalid-requests ()
  "An unknown frame or action fails before a deferred task is scheduled."
  (let ((scheduled 0)
        (empty (base64-encode-string "" t)))
    (cl-letf (((symbol-function '+background--editing-frames)
               (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) (cl-incf scheduled))))
      (should-error
       (+background-frame-act (base64-encode-string "bogus" t) empty)
       :type 'user-error)
      (should-error
       (+background-frame-act (base64-encode-string "focus" t)
                                (base64-encode-string "missing" t))
       :type 'user-error)
      (should (= scheduled 0))
      (+background-frame-act (base64-encode-string "new" t) empty)
      (should (= scheduled 1)))))

(ert-deftest background-deferred-frame-action-reports-a-closed-frame ()
  "A frame can vanish between request validation and deferred execution."
  (let ((shown 0))
    (cl-letf (((symbol-function '+background--editing-frames)
               (lambda () nil))
              ((symbol-function '+background/show)
               (lambda () (cl-incf shown))))
      (+background--frame-act-safely "focus" "closed-frame")
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
                    ((symbol-function '+frame-fit-default)
                     (lambda (_frame) (error "Sizing failed")))
                    ((symbol-function 'delete-frame)
                     (lambda (frame &rest _) (setq deleted frame))))
            (should-error (+background-make-frame))))
      (kill-buffer source))
    (should (eq created-from (+window-dashboard-buffer)))
    (should (eq deleted (selected-frame)))))

(ert-deftest background-raw-make-frame-starts-on-dashboard ()
  "The frame hook gives even a plain `make-frame' the Dashboard."
  (let ((source (generate-new-buffer "*frame-dashboard-source*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (should (memq #'+background--new-frame-dashboard
                        after-make-frame-functions))
          (+background--new-frame-dashboard (selected-frame))
          (should (eq (window-buffer) (+window-dashboard-buffer))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest background-child-frame-keeps-its-own-buffer ()
  "Dashboard setup does not replace a child frame's popup content."
  (let ((source (generate-new-buffer "*child-frame-source*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (_frame) 'parent)))
            (+background--new-frame-dashboard (selected-frame)))
          (should (eq (window-buffer) source)))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest background-frame-factory-keeps-successful-frame ()
  (let (deleted)
    (cl-letf (((symbol-function 'make-frame)
               (lambda (&rest _) (selected-frame)))
              ((symbol-function '+frame-fit-default)
               (lambda (_frame) nil))
              ((symbol-function 'delete-frame)
               (lambda (frame &rest _) (setq deleted frame))))
      (should (eq (+background-make-frame) (selected-frame))))
    (should-not deleted)))

(ert-deftest background-new-terminal-uses-one-frame-without-focus-timer ()
  "Opening a terminal must not schedule focus back to an older frame."
  (let ((terminal (generate-new-buffer " *new-terminal-focus-test*"))
        (origin (selected-frame))
        (new-frame 'new-terminal-frame)
        active focused timers parameters)
    (setq active origin)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function '+background-make-frame)
                   (lambda (&optional params)
                     (should-not params) new-frame))
                  ((symbol-function 'selected-frame) (lambda () active))
                  ((symbol-function 'select-frame)
                   (lambda (frame &rest _) (setq active frame)))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (frame) (push frame focused)))
                  ((symbol-function '+portal-create)
                   (lambda (&rest _)
                     (should (eq active new-frame))
                     (should-not focused)
                     terminal))
                  ((symbol-function 'set-frame-parameter)
                   (lambda (_frame key value)
                     (push (cons key value) parameters)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (cl-incf timers))))
          (should (eq (+portal-open-external nil "true") terminal))
          (should-not focused)
          (should-not timers)
          (should (equal parameters `((+portal/terminal . ,terminal)))))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest background-raycast-terminal-focuses-only-latest-frame ()
  "Rapid New Terminal actions must not activate an older terminal frame."
  (let* ((first 'first-raycast-frame)
         (second 'second-raycast-frame)
         (first-buffer (generate-new-buffer " *raycast-first*"))
         (second-buffer (generate-new-buffer " *raycast-second*"))
         (current first)
         (+portal--last-raycast-frame nil)
         timers focused)
    (unwind-protect
        (cl-letf (((symbol-function '+portal-open-external)
                   (lambda () (if (eq current first)
                                  first-buffer second-buffer)))
                  ((symbol-function 'selected-frame)
                   (lambda () current))
                  ((symbol-function 'delete-other-windows) #'ignore)
                  ((symbol-function 'set-frame-size) #'ignore)
                  ((symbol-function 'frame-monitor-workarea)
                   (lambda (&rest _) '(0 0 1000 800)))
                  ((symbol-function 'frame-outer-width) (lambda (&rest _) 400))
                  ((symbol-function 'frame-outer-height) (lambda (&rest _) 300))
                  ((symbol-function 'set-frame-position) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (push (cons callback args) timers)))
                  ((symbol-function 'frame-live-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-selected-window)
                   (lambda (frame) (if (eq frame first) 'first-window
                                     'second-window)))
                  ((symbol-function 'window-buffer)
                   (lambda (window) (if (eq window 'first-window)
                                        first-buffer second-buffer)))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (frame) (push frame focused))))
          (+portal/open-new)
          (setq current second)
          (+portal/open-new)
          (dolist (timer (reverse timers))
            (apply (car timer) (cdr timer)))
          (should (equal focused (list second))))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'init-background-tests)
;;; init-background-tests.el ends here
