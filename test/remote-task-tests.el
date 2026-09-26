;;; remote-task-tests.el --- Routed task lifecycle checks -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'remote-config)
(require 'remote-framework)

(ert-deftest remote-task-unconfirmed-cancel-survives-relay-exit ()
  "A local relay exit cannot establish that target cancellation succeeded."
  (require 'compile)
  (let* ((buffer (generate-new-buffer " *remote-task-cancel-race*"))
         (process
          (make-process
           :name "remote-task-cancel-race" :buffer buffer
           :command '("sh" "-c" "exit 0") :noquery t))
         (task
          (remote-task-create
           :id "cancel-race" :name "cancel-race" :buffer buffer
           :process process :state 'cancel-unconfirmed
           :started-at (current-time))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (compilation-mode))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (remote-task--finished task process "finished\n")
          (should (eq (remote-task-state task) 'cancel-unconfirmed))
          (should-not (remote-task-exit-code task))
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward
                     "cancellation unconfirmed, target may still run" nil t))))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest remote-task-local-process-output-and-workspace-close ()
  "Tasks use the target cwd/env, parse errors, and stop on workspace close."
  (remote-config-load)
  (remote-fs-install)
  (let* ((native (make-temp-file "emacs-remote-task-" t))
         (logical (file-name-as-directory
                   (remote-make-file-name "local" native)))
         (default-directory logical)
         (workspace (remote-workspace-open
                     logical :adapter "process"
                     :capability 'process-async))
         completed rerun sleeping immediate disconnected)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "sample.c" native)
            (insert "one\ntwo\nthree\n"))
          (setq completed
                (remote-task-run
                 (list "sh" "-c"
                       "pwd; printf '%s/sample.c:3:2: error: example\\n' \"$PWD\"; printf 'token=%s\\n' \"$REMOTE_TASK_TOKEN\"")
                 :workspace workspace
                 :environment '(("REMOTE_TASK_TOKEN" . "routed"))))
          (let ((deadline (+ (float-time) 8)))
            (while (and (eq (remote-task-state completed) 'running)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (remote-task-state completed) 'succeeded))
          (should (= (remote-task-exit-code completed) 0))
          (should-not (remote-task-resource completed))
          (with-current-buffer (remote-task-buffer completed)
            (should (derived-mode-p 'compilation-mode))
            (should (eq (local-key-binding (kbd "g"))
                        #'remote-task-rerun))
            (goto-char (point-min))
            (should (search-forward native nil t))
            (should (search-forward "token=routed" nil t))
            (goto-char (point-min))
            (should (search-forward
                     (concat native "/sample.c:3:2: error: example")
                     nil t))
            (should (get-text-property
                     (line-beginning-position) 'compilation-message))
            (goto-char (point-min))
            (next-error 1 t))
          (let ((logical-file
                 (remote-make-file-name
                  "local" (expand-file-name "sample.c" native))))
            (let ((source
                   (seq-find
                    (lambda (buffer)
                      (equal (buffer-file-name buffer) logical-file))
                    (buffer-list))))
              (should source)
              (should-not
               (seq-find
                (lambda (buffer)
                  (equal (buffer-file-name buffer)
                         (expand-file-name "sample.c" native)))
                (buffer-list)))
              (kill-buffer source)))
          (setq rerun (remote-task-rerun completed))
          (let ((deadline (+ (float-time) 8)))
            (while (and (eq (remote-task-state rerun) 'running)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (remote-task-state rerun) 'succeeded))
          (should-not (equal (remote-task-id rerun)
                             (remote-task-id completed)))
          (should (equal (remote-task-directory rerun)
                         (remote-task-directory completed)))
          (should (equal (remote-task-environment rerun)
                         (remote-task-environment completed)))
          (with-current-buffer (remote-task-buffer rerun)
            (goto-char (point-min))
            (should (search-forward "token=routed" nil t)))
          (let ((left (remote-task-run
                       (list "sh" "-c" "sleep 0.5; printf 'left-only\\n'")
                       :workspace workspace))
                (right (remote-task-run
                        (list "sh" "-c" "sleep 0.5; printf 'right-only\\n'")
                        :workspace workspace)))
            (unwind-protect
                (progn
                  (should (and (eq (remote-task-state left) 'running)
                               (eq (remote-task-state right) 'running)))
                  (let ((deadline (+ (float-time) 4)))
                    (while (and (or (eq (remote-task-state left) 'running)
                                    (eq (remote-task-state right) 'running))
                                (< (float-time) deadline))
                      (accept-process-output nil 0.05)))
                  (should (and (eq (remote-task-state left) 'succeeded)
                               (eq (remote-task-state right) 'succeeded)))
                  (with-current-buffer (remote-task-buffer left)
                    (should (save-excursion
                              (goto-char (point-min))
                              (search-forward "left-only" nil t)))
                    (should-not (save-excursion
                                  (goto-char (point-min))
                                  (search-forward "right-only" nil t))))
                  (with-current-buffer (remote-task-buffer right)
                    (should (save-excursion
                              (goto-char (point-min))
                              (search-forward "right-only" nil t)))))
              (kill-buffer (remote-task-buffer left))
              (kill-buffer (remote-task-buffer right))))
          (setq immediate
                (remote-task-run (list "sleep" "30")
                                 :workspace workspace))
          (remote-task-cancel immediate)
          (let ((deadline (+ (float-time) 6)))
            (while (and (process-live-p (remote-task-process immediate))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (remote-task-state immediate) 'cancelled))
          (should-not (process-live-p (remote-task-process immediate)))
          (setq disconnected
                (remote-task-run (list "sleep" "30")
                                 :workspace workspace))
          (setf (remote-workspace-state workspace) 'disconnected)
          (remote-task-cancel disconnected)
          (let ((deadline (+ (float-time) 6)))
            (while (and (process-live-p
                         (remote-task-process disconnected))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (remote-task-state disconnected)
                      'cancel-unconfirmed))
          (with-current-buffer (remote-task-buffer disconnected)
            (goto-char (point-min))
            (should (search-forward
                     "Cancellation unconfirmed: target may still run" nil t)))
          (setf (remote-workspace-state workspace) 'open)
          (setq sleeping
                (remote-task-run (list "sleep" "30")
                                 :workspace workspace))
          (should (eq (remote-task-state sleeping) 'running))
          (should (remote-task-resource sleeping))
          (should-error (remote-task-rerun sleeping) :type 'user-error)
          (remote-workspace-close workspace 'test)
          (let ((deadline (+ (float-time) 6)))
            (while (and (process-live-p (remote-task-process sleeping))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (remote-task-state sleeping) 'cancelled))
          (should-not (process-live-p (remote-task-process sleeping)))
          (should-error (remote-task-rerun completed) :type 'user-error))
      (when (remote-workspace-live-p workspace)
        (remote-workspace-close workspace 'test-cleanup))
      (dolist (task (list completed rerun sleeping immediate disconnected))
        (when (and task (buffer-live-p (remote-task-buffer task)))
          (kill-buffer (remote-task-buffer task))))
      (delete-directory native t))))

(ert-deftest remote-task-transport-interruption-stays-on-target ()
  "A transport fault marks only tasks using that target pipeline."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (remote-workspace-auto-reconnect nil)
         (route-a
          (remote-route-create
           :target-id "host-a" :pipeline-id "ssh"
           :backend-id "tramp-rpc"))
         (route-b
          (remote-route-create
           :target-id "host-b" :pipeline-id "ssh"
           :backend-id "tramp-rpc"))
         (process-a
          (make-process :name "task-fault-a" :command '("sleep" "30")
                        :noquery t))
         (process-b
          (make-process :name "task-fault-b" :command '("sleep" "30")
                        :noquery t))
         (task-a
          (remote-task-create :id "a" :process process-a :state 'running))
         (task-b
          (remote-task-create :id "b" :process process-b :state 'running))
         (workspace-a
          (remote-workspace-create
           :key 'a :target-id "host-a" :state 'open
           :routes (list route-a)
           :resources
           (list (remote-workspace-resource-create
                  :kind 'task :value task-a))))
         (workspace-b
          (remote-workspace-create
           :key 'b :target-id "host-b" :state 'open
           :routes (list route-b)
           :resources
           (list (remote-workspace-resource-create
                  :kind 'task :value task-b)))))
    (unwind-protect
        (progn
          (process-put process-a 'remote-route route-a)
          (process-put process-b 'remote-route route-b)
          (puthash 'a workspace-a remote-workspaces)
          (puthash 'b workspace-b remote-workspaces)
          (remote-workspace-handle-transport-failure
           route-a '(remote-transport-error "injected"))
          (should (remote-task-transport-interrupted task-a))
          (should-not (remote-task-transport-interrupted task-b))
          (should (eq (remote-workspace-state workspace-a)
                      'disconnected))
          (should (eq (remote-workspace-state workspace-b) 'open)))
      (delete-process process-a)
      (delete-process process-b))))

(provide 'remote-task-tests)
;;; remote-task-tests.el ends here
