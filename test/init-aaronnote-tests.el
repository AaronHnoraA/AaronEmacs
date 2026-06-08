;;; init-aaronnote-tests.el --- Aaronnote bridge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-aaronnote)

(ert-deftest my/aaronnote-run-emacs-key-queues-m-x ()
  (let ((buffer (generate-new-buffer "*aaronnote-test*"))
        (unread-command-events nil)
        (my/aaronnote--app-buffer nil)
        focused-frame
        edit-mode-arg
        scheduled-fn)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq my/aaronnote--app-buffer buffer)
          (cl-letf (((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame) (setq focused-frame frame)))
                    ((symbol-function 'xwidget-webkit-edit-mode)
                     (lambda (arg) (setq edit-mode-arg arg)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest _args)
                       (setq scheduled-fn function)
                       'mock-timer)))
            (my/aaronnote--run-emacs-key "M-x"))
          (should (equal unread-command-events
                         (listify-key-sequence (kbd "M-x"))))
          (should (eq (selected-window) (get-buffer-window buffer 'visible)))
          (should (eq focused-frame (selected-frame)))
          (should (equal edit-mode-arg -1))
          (should (eq scheduled-fn #'my/aaronnote--focus-minibuffer-if-active)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/aaronnote-run-emacs-key-queues-prefix-sequence ()
  (let ((unread-command-events nil)
        (my/aaronnote--app-buffer nil)
        scheduled-fn)
    (cl-letf (((symbol-function 'select-frame-set-input-focus)
               (lambda (_frame) nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest _args)
                 (setq scheduled-fn function)
                 'mock-timer)))
      (my/aaronnote--run-emacs-key "C-x C-f"))
    (should (equal unread-command-events
                   (listify-key-sequence (kbd "C-x C-f"))))
    (should (eq scheduled-fn #'my/aaronnote--focus-minibuffer-if-active))))

(ert-deftest my/aaronnote-process-filter-handles-split-ready-line ()
  (let* ((buffer (generate-new-buffer " *aaronnote-test-process*"))
         (proc (make-process
                :name "aaronnote-test-process"
                :buffer buffer
                :command (list "cat")))
         (my/aaronnote--process proc)
         (my/aaronnote--port nil)
         (my/aaronnote--ready nil)
         (flush-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'my/aaronnote--flush-ready-callbacks)
                   (lambda () (cl-incf flush-count))))
          (my/aaronnote--process-filter proc "aaronote-web-host:ready:")
          (should-not my/aaronnote--ready)
          (should (equal (process-get proc 'aaronnote-pending)
                         "aaronote-web-host:ready:"))
          (my/aaronnote--process-filter proc "50815\n")
          (should my/aaronnote--ready)
          (should (= my/aaronnote--port 50815))
          (should (= flush-count 1))
          (should (equal (process-get proc 'aaronnote-pending) "")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/aaronnote-sentinel-resets-dead-current-process ()
  (let* ((buffer (generate-new-buffer " *aaronnote-sentinel-test*"))
         (proc (make-process
                :name "aaronnote-sentinel-test"
                :buffer buffer
                :command (list "cat")))
         (my/aaronnote--process proc)
         (my/aaronnote--port 50815)
         (my/aaronnote--ready t)
         (my/aaronnote--ready-callbacks (list #'ignore))
         (my/aaronnote--ready-watchdog nil))
    (unwind-protect
        (progn
          (delete-process proc)
          (my/aaronnote--sentinel proc "exited abnormally with code 1\n")
          (should-not my/aaronnote--process)
          (should-not my/aaronnote--port)
          (should-not my/aaronnote--ready)
          (should-not my/aaronnote--ready-callbacks))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/aaronnote-watchdog-clears-stale-ready-callbacks ()
  (let ((my/aaronnote--ready nil)
        (my/aaronnote--ready-watchdog 'mock-timer)
        (my/aaronnote--ready-callbacks (list #'ignore)))
    (my/aaronnote--watchdog-fire)
    (should-not my/aaronnote--ready-watchdog)
    (should-not my/aaronnote--ready-callbacks)))

(ert-deftest my/aaronnote-activity-hooks-install-idempotently ()
  (let ((after-focus-change-function nil)
        (window-buffer-change-functions nil)
        (window-selection-change-functions nil)
        (my/aaronnote--activity-hooks-installed nil)
        (my/aaronnote--activity-timer nil)
        (my/aaronnote--paused t))
    (my/aaronnote--install-activity-hooks)
    (my/aaronnote--install-activity-hooks)
    (should my/aaronnote--activity-hooks-installed)
    (should (equal window-buffer-change-functions
                   (list #'my/aaronnote--update-activity)))
    (should (equal window-selection-change-functions
                   (list #'my/aaronnote--update-activity)))
    (my/aaronnote--remove-activity-hooks)
    (should-not my/aaronnote--activity-hooks-installed)
    (should-not window-buffer-change-functions)
    (should-not window-selection-change-functions)
    (should-not my/aaronnote--paused)))

(ert-deftest my/aaronnote-process-filter-ignores-stale-proc-ready-line ()
  "A dying old process emitting a ready: line must not clobber the new port."
  (let* ((buffer (generate-new-buffer " *aaronnote-stale-proc-test*"))
         (stale-proc (make-process
                      :name "aaronnote-stale-test"
                      :buffer buffer
                      :command (list "cat")))
         ;; Simulate a new process by using a different proc object as current.
         (new-proc (make-process
                    :name "aaronnote-new-test"
                    :buffer buffer
                    :command (list "cat")))
         (my/aaronnote--process new-proc)
         (my/aaronnote--port 60000)
         (my/aaronnote--ready t)
         (flush-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'my/aaronnote--flush-ready-callbacks)
                   (lambda () (cl-incf flush-count))))
          ;; stale-proc (not current) emits a ready: line with a different port.
          (my/aaronnote--process-filter stale-proc "aaronote-web-host:ready:99999\n")
          ;; Port and ready state must be unchanged.
          (should (= my/aaronnote--port 60000))
          (should my/aaronnote--ready)
          (should (= flush-count 0)))
      (dolist (p (list stale-proc new-proc))
        (when (process-live-p p) (delete-process p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'init-aaronnote-tests)
;;; init-aaronnote-tests.el ends here
