;;; remote-framework-tests.el --- Framework boundary tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -L lisp/remote \
;;     -l test/remote-framework-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'remote-framework)
(require 'remote-config)
(require 'remote-board)

(defvar tramp-rpc-ssh-args)
(defvar remote--client-process-environment)
(defvar remote--client-exec-path)

(defmacro remote-framework-test-with-registry (&rest body)
  "Evaluate BODY with isolated framework registries."
  (declare (indent 0) (debug t))
  `(let ((remote-targets (make-hash-table :test #'equal))
         (remote-links (make-hash-table :test #'equal))
         (remote-link-plugins (make-hash-table :test #'equal))
         (remote-adapters (make-hash-table :test #'equal))
         (remote-route-health (make-hash-table :test #'equal))
         (remote-connection-pool (make-hash-table :test #'equal))
         (remote-pipeline-runtime-pool
          (make-hash-table :test #'equal))
         (remote-transports (make-hash-table :test #'equal))
         (remote-backends (make-hash-table :test #'equal))
         (remote-backend-contracts (make-hash-table :test #'equal))
         (remote-operation-providers nil)
         (remote-accelerator-probe-cache (make-hash-table :test #'equal))
         (remote-workspaces (make-hash-table :test #'equal))
         (remote-services (make-hash-table :test #'equal))
         (remote-service-instances (make-hash-table :test #'equal))
         (remote-terminals (make-hash-table :test #'equal))
         (remote-channels (make-hash-table :test #'equal))
         (remote-channel-groups (make-hash-table :test #'equal))
         (remote-workspace--resource-counter 0)
         (remote-channel--counter 0)
         (remote-channel-group--counter 0)
         (remote-doctor-check-functions nil)
         (remote-route-log nil)
         (remote-board--opening-folders (make-hash-table :test #'equal))
         (remote-board--opening-targets (make-hash-table :test #'equal))
         (remote-board-connection-progress
          (make-hash-table :test #'equal))
         (remote-board-connection-history
          (make-hash-table :test #'equal))
         (remote-board-ssh-statuses (make-hash-table :test #'equal))
         (remote-board--ssh-probe-processes (make-hash-table :test #'equal))
         (remote-board--ssh-status-generation 0))
     (remote-framework-reset)
     ,@body))

(ert-deftest remote-board-lists-configured-and-recent-folders-without-connect ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (remote-board-recent-folder-limit 2)
           (remote-board-recent-folders nil))
      (setf (remote-target-workspaces target)
            '(((id . "main") (path . "/work/"))))
      (remote-board--remember-folder "/fs:lab:/work/")
      (remote-board--remember-folder "/fs:lab:/other/")
      (remote-board--remember-folder "/fs:lab:/new/")
      (should
       (equal remote-board-recent-folders
              '("/fs:lab:/new/" "/fs:lab:/other/")))
      (cl-letf (((symbol-function 'remote-connection-ensure)
                 (lambda (&rest _)
                   (ert-fail "Board rendering opened a connection"))))
        (let* ((rows (remote-board--folder-rows target nil))
               (paths (mapcar (lambda (row) (nth 2 (car row))) rows)))
          (should (equal paths
                         '("/work/" "/fs:lab:/new/"
                           "/fs:lab:/other/")))
          (should (equal (aref (cadar rows) 2) "configured"))
          (should (equal (aref (cadadr rows) 2) "recent"))))
      (remote-board--remember-folder "/fs:lab:/work/")
      (should (= (length (remote-board--folder-rows target nil)) 2)))))

(ert-deftest remote-board-connection-progress-keeps-newest-attempt ()
  (remote-framework-test-with-registry
    (let* ((route
            (remote-route-create
             :target-id "lab" :pipeline-id "lab/ssh"
             :backend-id "tramp-rpc"))
           (older
            (remote-connection-create
             :target-id "lab" :generation 3 :state 'opening))
           (newer
            (remote-connection-create
             :target-id "lab" :generation 4 :state 'opening))
           (remote-board-connection-history-limit 3))
      (remote-board--connection-progress older route 'transport)
      (should (equal (remote-board--target-state "lab" nil nil)
                     "opening route"))
      (remote-board--connection-progress newer route 'backend)
      (remote-board--connection-progress older route 'ready)
      (should (equal (remote-board--target-state "lab" nil nil)
                     "SSH login"))
      (setf (remote-connection-state newer) 'failed
            (remote-connection-error newer)
            '(error "Permission denied (publickey)."))
      (remote-board--connection-progress newer route 'failed)
      (remote-board--record-connection-failure
       newer route (remote-connection-error newer))
      (should-not (gethash "lab" remote-board-connection-progress))
      (should (= (length (gethash "lab"
                                  remote-board-connection-history))
                 3))
      (should (equal (remote-board--target-state "lab" nil nil)
                     "auth required"))
      (let ((buffer nil))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (value &rest _ignored)
                           (setq buffer value))))
                (remote-board-connection-log "lab"))
              (with-current-buffer buffer
                (should (string-match-p "SSH login" (buffer-string)))
                (should (string-match-p "Permission denied"
                                        (buffer-string)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest remote-board-opens-and-remembers-only-existing-folders ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (let ((remote-board-recent-folders nil)
          opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened path)
                   'opened)))
        (should (eq (remote-open-folder "local" "/tmp/") 'opened)))
      (should (equal opened "/fs:local:/tmp/"))
      (should (equal remote-board-recent-folders
                     '("/fs:local:/tmp/")))
      (let ((workspace (remote-get-workspace "/fs:local:/tmp/")))
        (should (remote-workspace-live-p workspace))
        (should (equal (remote-route-link-plugin-id
                        (remote-workspace-primary-route workspace))
                       "native")))
      (should-not (gethash "local" remote-board--opening-targets))
      (should-error
       (remote-open-folder
        "local" "/tmp/no-such-remote-board-folder-20260925/")
       :type 'user-error)
      (should (equal remote-board-recent-folders
                     '("/fs:local:/tmp/"))))))

(ert-deftest remote-board-folder-open-failure-releases-new-workspace ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (let* ((directory (make-temp-file "remote-board-open-" t))
           (logical (remote-make-file-name "local" directory)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'find-file)
                       (lambda (_path)
                         (should (gethash "local"
                                          remote-board--opening-targets))
                         (should (gethash (file-name-as-directory logical)
                                          remote-board--opening-folders))
                         (should (equal
                                  (remote-board--target-state
                                   "local" nil nil)
                                  "opening folder"))
                         (should (equal
                                  (remote-board--folder-state
                                   (file-name-as-directory logical)
                                   nil 'configured)
                                  "opening"))
                         (error "Injected Dired failure"))))
              (should-error (remote-open-folder "local" directory)))
            (should-not (remote-get-workspace logical))
            (should-not (gethash "local" remote-board--opening-targets))
            (should-not (gethash (file-name-as-directory logical)
                                 remote-board--opening-folders)))
        (delete-directory directory t)))))

(ert-deftest remote-board-close-and-disconnect-release-owned-state ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (let* ((directory (make-temp-file "remote-board-close-" t))
           (logical (remote-make-file-name "local" directory)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'find-file)
                       (lambda (_path) 'opened)))
              (remote-open-folder "local" directory))
            (with-temp-buffer
              (remote-board-mode)
              (cl-letf (((symbol-function 'remote-board-target-at-point)
                         (lambda () (remote-get-target "local")))
                        ((symbol-function 'remote-board--folder-at-point)
                         (lambda () directory)))
                (remote-board-close-workspace)))
            (should-not (remote-get-workspace logical))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (_path) 'opened)))
              (remote-open-folder "local" directory))
            (let ((result (remote-board-disconnect-target "local")))
              (should (= (plist-get result :workspaces) 1))
              (should (= (plist-get result :sessions) 1)))
            (should-not (remote-get-workspace logical))
            (should-not (remote-connection-pool-status)))
        (delete-directory directory t)))))

(ert-deftest remote-disconnect-closes-target-buffers-and-protects-edits ()
  "The same lifecycle closes native and logical buffers, with no file I/O."
  (dolist (target '("local" "box"))
    (remote-framework-test-with-registry
      (let* ((directory (if (equal target "local")
                            "/tmp/" (format "/fs:%s:/work/" target)))
             (file (generate-new-buffer " *disconnect-file*"))
             (dirty (generate-new-buffer " *disconnect-unsaved*"))
             (dired (generate-new-buffer " *disconnect-dired*"))
             (output (generate-new-buffer " *disconnect-output*"))
             (vetoed (generate-new-buffer "disconnect-veto"))
             (other (generate-new-buffer " *disconnect-other*"))
             (scratch (generate-new-buffer " *disconnect-scratch*"))
             (process (make-pipe-process :name "disconnect-owned"
                                         :buffer output :noquery t))
             (context (remote-context-create :target-id target))
             result)
        (unwind-protect
            (progn
              (dolist (buffer (list file dirty))
                (with-current-buffer buffer
                  (setq buffer-file-name (concat directory (buffer-name buffer))
                        default-directory directory)))
              (with-current-buffer dirty
                (insert "keep this edit"))
              (with-current-buffer dired
                (setq default-directory directory major-mode 'dired-mode))
              (with-current-buffer output
                (insert "process output is not an unsaved file"))
              (process-put process 'remote-context context)
              (with-current-buffer vetoed
                (setq-local remote-buffer-target-id target)
                (add-hook 'kill-buffer-query-functions (lambda () nil) nil t))
              (with-current-buffer other
                (setq buffer-file-name "/fs:other:/work/other.c"))
              ;; A native scratch directory has no explicit lifecycle owner.
              (with-current-buffer scratch
                (setq default-directory "/tmp/"))
              (cl-letf (((symbol-function 'remote-connection-ensure)
                         (lambda (&rest _) (ert-fail "Cleanup opened a session"))))
                (setq result (remote-workspace-disconnect-target target)))
              (dolist (buffer (list file dired output))
                (should-not (buffer-live-p buffer)))
              (should-not (process-live-p process))
              (should (>= (plist-get result :buffers) 3))
              (should (equal (sort (plist-get result :kept-buffers) #'string<)
                             (sort (list (buffer-name dirty) (buffer-name vetoed))
                                   #'string<)))
              (should (buffer-local-value 'remote-buffer-disconnected-p dirty))
              (with-current-buffer dirty
                (should (buffer-modified-p))
                (should (equal (buffer-string) "keep this edit")))
              (should (buffer-live-p other))
              (should (buffer-live-p scratch)))
          (when (process-live-p process) (delete-process process))
          (dolist (buffer (list file dirty dired output vetoed other scratch))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (set-buffer-modified-p nil)
                (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))))

(ert-deftest remote-disconnect-consumer-errors-do-not-skip-other-closers ()
  (remote-framework-test-with-registry
    (with-temp-buffer
      (setq-local remote-buffer-target-id "box")
      (insert "protected work")
      (setq buffer-file-name "/fs:box:/work/edited.c")
      (let (buffer-closed target-closed)
        (let ((remote-buffer-disconnect-hook
               (list (lambda () (error "broken buffer consumer"))
                     (lambda () (setq buffer-closed t))))
              (remote-target-disconnect-hook
               (list (lambda (&rest _) (error "broken target consumer"))
                     (lambda (&rest _) (setq target-closed t)))))
          (remote-workspace-disconnect-target "box")
          (should buffer-closed)
          (should target-closed)
          (should (buffer-live-p (current-buffer)))
          (set-buffer-modified-p nil))))))

(ert-deftest remote-disconnect-cancels-only-own-target-background-work ()
  (remote-framework-test-with-registry
    (let ((remote-background-jobs (make-hash-table :test #'equal))
          (remote-background-target-epochs (make-hash-table :test #'equal)))
      (dolist (target '("box" "other"))
        (remote-background-submit
         (list 'probe target)
         (lambda () (ert-fail "Cancelled job ran")) :target-id target))
      (remote-workspace-disconnect-target "box")
      (should-not (remote-background-job-list "box"))
      (should (= (length (remote-background-job-list "other")) 1))
      (remote-background-clear))))

(ert-deftest remote-closing-workspace-does-not-schedule-recovery ()
  (remote-framework-test-with-registry
    (let* ((remote-background-jobs (make-hash-table :test #'equal))
           (route (remote-route-create :target-id "box" :pipeline-id "box/ssh"))
           (workspace (remote-workspace-create :id "closing" :key 'closing
                                               :target-id "box" :state 'closing
                                               :routes (list route))))
      (puthash 'closing workspace remote-workspaces)
      (remote-workspace-handle-transport-failure
       route '(remote-transport-error "intentional shutdown"))
      (should (eq (remote-workspace-state workspace) 'closing))
      (should-not (remote-background-job-list)))))

(ert-deftest remote-tramp-target-disconnect-cleans-unpooled-handles ()
  (remote-framework-test-with-registry
    (require 'tramp-cmds)
    (remote-register-target "box")
    (remote-register-pipeline
     "box" "ssh" '("tramp" "tramp-rpc")
     :config '(:method "ssh" :host "cleanup-box"))
    (let ((tramp-methods (cons '("rpc") tramp-methods)) cleaned)
      (cl-letf (((symbol-function 'tramp-cleanup-connection)
                 (lambda (vec &rest _)
                   (should remote-backend-tramp-explicit-cleanup)
                   (push (tramp-file-name-method vec) cleaned))))
        (remote-workspace-disconnect-target "box"))
      (should (equal (sort cleaned #'string<) '("rpc" "ssh"))))))

(ert-deftest remote-routed-process-retains-stdout-and-stderr-buffer-ownership ()
  "Native stderr directories must not lose their target after process exit."
  (remote-framework-test-with-registry
    (let* ((output (generate-new-buffer " *routed-owned-output*"))
           (stderr (generate-new-buffer " *routed-owned-stderr*"))
           (process (remote-make-process
                     :name "routed-owned-process"
                     :command '("sh" "-c" "printf done; printf err >&2")
                     :remote-context (remote-context "/tmp/")
                     :buffer output :stderr stderr :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p process) (accept-process-output process 0.1))
            (should (equal (remote-buffer-target output) "local"))
            (should (equal (remote-buffer-target stderr) "local"))
            (let ((result (remote-workspace-disconnect-target "local")))
              (should (= (plist-get result :buffers) 2)))
            (should-not (buffer-live-p output))
            (should-not (buffer-live-p stderr)))
        (when (process-live-p process) (delete-process process))
        (dolist (buffer (list output stderr))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest remote-board-folder-prompt-completes-on-selected-target ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (let (completion-directory opened)
      (with-temp-buffer
        (remote-board-mode)
        (cl-letf (((symbol-function 'remote-board-target-at-point)
                   (lambda () (remote-get-target "local")))
                  ((symbol-function 'remote-board--folder-at-point)
                   (lambda () "/tmp/"))
                  ((symbol-function 'read-directory-name)
                   (lambda (_prompt directory &rest _args)
                     (setq completion-directory directory)
                     "/fs:local:/tmp/"))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq opened path))))
          (call-interactively #'remote-open-folder)))
      (should (equal completion-directory "/fs:local:/tmp/"))
      (should (equal opened "/fs:local:/tmp/")))))

(ert-deftest remote-board-views-refresh-independently-by-frame ()
  "A state change redraws every visible view without resetting its sort."
  (remote-framework-test-with-registry
    (let* ((real-frame (selected-frame))
           (other-frame (make-symbol "other-frame"))
           (other-parameters (make-hash-table :test #'eq))
           (original-frame-parameter (symbol-function 'frame-parameter))
           (original-set-frame-parameter (symbol-function 'set-frame-parameter))
           (original-frame-live-p (symbol-function 'frame-live-p))
           (target (remote-register-target "view-probe" :label "Before" :trusted t))
           first second first-history second-history)
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (frame key)
                   (if (eq frame other-frame)
                       (gethash key other-parameters)
                     (funcall original-frame-parameter frame key))))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame key value)
                   (if (eq frame other-frame)
                       (puthash key value other-parameters)
                     (funcall original-set-frame-parameter frame key value))))
                ((symbol-function 'frame-live-p)
                 (lambda (frame)
                   (if (eq frame other-frame) t
                     (funcall original-frame-live-p frame)))))
        (unwind-protect
            (progn
              (setq first (remote-board--buffer real-frame)
                    second (remote-board--buffer other-frame))
              (should-not (eq first second))
              (setq first-history
                    (remote-board--history-buffer 'routes "*Remote Routes*"
                                                  real-frame)
                    second-history
                    (remote-board--history-buffer 'routes "*Remote Routes*"
                                                  other-frame))
              (should-not (eq first-history second-history))
              (with-current-buffer first
                (setq-local tabulated-list-sort-key '("ID" . nil)))
              (with-current-buffer second
                (should-not tabulated-list-sort-key))
              (setf (remote-target-label target) "After")
              (cl-letf (((symbol-function 'frame-list)
                         (lambda () (list real-frame other-frame)))
                        ((symbol-function 'get-buffer-window)
                         (lambda (buffer frame)
                           (and (eq buffer
                                    (frame-parameter frame
                                                     'remote-board-view-buffer))
                                t))))
                (remote-board--refresh-if-open))
              (dolist (buffer (list first second))
                (with-current-buffer buffer
                  (should (string-match-p "After" (buffer-string)))))
              (with-current-buffer first
                (should (equal tabulated-list-sort-key '("ID" . nil))))
              (remote-board--frame-deleted other-frame)
              (should-not (buffer-live-p second))
              (should-not (buffer-live-p second-history))
              (should (buffer-live-p first-history))
              (should (buffer-live-p first)))
          (when (buffer-live-p second-history) (kill-buffer second-history))
          (when (buffer-live-p first-history) (kill-buffer first-history))
          (when (buffer-live-p second) (kill-buffer second))
          (when (buffer-live-p first) (kill-buffer first)))))))

(ert-deftest remote-board-mode-line-is-remote-only-and-cheap ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :label "Lab" :trusted t)
    (with-temp-buffer
      (setq default-directory "/tmp/")
      (should-not (remote-board--mode-line-text))
      (setq default-directory "/fs:local:/tmp/")
      (should-not (remote-board--mode-line-text))
      (setq default-directory "/fs:lab:/work/")
      (should (string-match-p "Remote:Lab"
                              (remote-board--mode-line-text))))))

(ert-deftest remote-board-folder-row-keeps-its-target-identity ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (remote-board-recent-folders '("/fs:lab:/work/")))
      (with-temp-buffer
        (remote-board-mode)
        (setq tabulated-list-entries
              (remote-board--folder-rows target nil))
        (tabulated-list-print)
        (goto-char (point-min))
        (should (eq (remote-board-target-at-point) target))
        (should (equal (remote-board--folder-at-point)
                       "/fs:lab:/work/"))))))

(ert-deftest remote-board-port-row-copies-and-closes-owned-forward ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (context
            (remote-context-create
             :target-id "lab" :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (workspace (remote-workspace-open context :connect nil))
           (route
            (remote-route-create
             :target-id "lab" :link-id "lab/ssh"
             :link-plugin-id "tramp" :capability 'port-forward
             :adapter-id "network"))
           (forward
            (remote-forward-create
             :route route :context context :state 'open
             :local-endpoint '(:host "127.0.0.1" :port 49152)
             :remote-endpoint '(:host "127.0.0.1" :port 8080)))
           (opened 0)
           (closed 0)
           (remote-channel-opened-hook
            (list (lambda (_channel) (cl-incf opened))))
           (remote-channel-closed-hook
            (list (lambda (_channel) (cl-incf closed))))
           (channel
            (remote-channel--adopt
             'forward route context forward)))
      (remote-workspace-register-resource
       workspace 'forward forward
       (lambda (value _reason) (remote-close-channel value)))
      (should (= opened 1))
      (with-temp-buffer
        (remote-board-mode)
        (setq tabulated-list-entries
              (remote-board--forward-rows
               target (remote-channel-list)))
        (tabulated-list-print)
        (goto-char (point-min))
        (should (eq (remote-board-target-at-point) target))
        (should (eq (remote-board--forward-at-point) channel))
        (remote-copy-target-uri)
        (should (equal (current-kill 0) "127.0.0.1:49152"))
        (remote-board-close-forward))
      (should (= closed 1))
      (should (eq (remote-forward-state forward) 'closed))
      (should-not (gethash (remote-channel-id channel) remote-channels))
      (should-not (remote-workspace-resources workspace)))))

(ert-deftest remote-board-failed-forward-closes-new-owner ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (cl-letf (((symbol-function 'remote-port-forward)
               (lambda (&rest _)
                 (error "Injected forward failure"))))
      (should-error (remote-board-forward-port "local" 12345)
                    :type 'error)
      (should (zerop (hash-table-count remote-workspaces))))
    (let ((owner (remote-workspace-open "/fs:local:/tmp/" :connect nil)))
      (cl-letf (((symbol-function 'remote-workspace-for-path)
                 (lambda (_path) owner))
                ((symbol-function 'remote-port-forward)
                 (lambda (&rest _)
                   (error "Injected forward failure"))))
        (should-error (remote-board-forward-port "local" 12345)
                      :type 'error)
        (should (remote-workspace-live-p owner))))))

(ert-deftest remote-board-moves-named-forward-without-dropping-old-on-error ()
  (remote-framework-test-with-registry
    (remote-fs-install)
    (let* ((forward (remote-board-forward-port
                     "local" 12345 "127.0.0.1" 0 "service"))
           (old-channel (remote-channel-of forward))
           (old-port (plist-get (remote-channel-endpoint forward 'local)
                                :port))
           (workspace (car (remote-board--forward-owner forward)))
           replacement blocker)
      (unwind-protect
          (with-temp-buffer
            (remote-board-mode)
            (cl-letf (((symbol-function 'remote-board--forward-at-point)
                       (lambda () old-channel)))
              (remote-board-rename-forward "renamed service")
              (should (equal (plist-get
                              (remote-channel-metadata old-channel) :name)
                             "renamed service"))
              (setq replacement (remote-board-change-local-port 0)))
            (let* ((new-channel (remote-channel-of replacement))
                   (new-port
                    (plist-get (remote-channel-endpoint replacement 'local)
                               :port)))
              (should (integerp new-port))
              (should (/= new-port old-port))
              (should (eq (remote-forward-state forward) 'closed))
              (should (remote-channel-live-p new-channel))
              (should (equal (plist-get
                              (remote-channel-metadata new-channel) :name)
                             "renamed service"))
              (should (= (length (remote-workspace-resources workspace)) 1))
              (setq blocker
                    (make-network-process
                     :name "remote-board-busy-port" :server t
                     :host "127.0.0.1" :service 0 :noquery t))
              (let ((busy-port
                     (plist-get (process-contact blocker t) :service)))
                (cl-letf (((symbol-function 'remote-board--forward-at-point)
                           (lambda () new-channel)))
                  (should-error
                   (remote-board-change-local-port busy-port)))
                (should (remote-channel-live-p new-channel))
                (should (= (length
                            (remote-workspace-resources workspace)) 1)))))
        (when (and blocker (process-live-p blocker))
          (delete-process blocker))
        (when workspace (remote-workspace-close workspace 'test-cleanup))))))

(ert-deftest remote-workspace-background-reconnect-accepts-own-epoch-change ()
  (remote-framework-test-with-registry
    (let* ((remote-background-jobs (make-hash-table :test #'equal))
           (remote-background-target-epochs (make-hash-table :test #'equal))
           (context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0))
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-state workspace) 'disconnected)
      (unwind-protect
          (cl-letf (((symbol-function 'remote-session-invalidate)
                     (lambda (&rest _)
                       (remote-background-invalidate-target "local")))
                    ((symbol-function 'remote-session-acquire)
                     (lambda (&rest _)
                       (cl-incf attempts)
                       'session)))
            (let ((job (remote-workspace--schedule-reconnect workspace)))
              (cancel-timer (remote-background-job-timer job))
              (remote-background--run job)
              (should (= attempts 1))
              (should (eq (remote-background-job-state job) 'complete))
              (should (eq (remote-workspace-state workspace) 'open))))
        (remote-background-clear 'test-cleanup)))))

(ert-deftest remote-workspace-failed-recovery-resumes-when-the-target-returns ()
  "A workspace that gave up during a long outage is not left for dead."
  (remote-framework-test-with-registry
    (let* ((remote-workspace-auto-reconnect t)
           (route (remote-route-create
                   :target-id "lab" :pipeline-id "lab/ssh"
                   :backend-id "tramp" :capability 'process-sync
                   :adapter-id "process"))
           (make
            (lambda (id state error)
              (let ((workspace
                     (remote-workspace-create
                      :id id :key (list "lab" id) :target-id "lab"
                      :root (format "/fs:lab:/%s/" id)
                      :routes (list route) :primary-route route
                      :state state :error error)))
                (puthash (remote-workspace-key workspace) workspace
                         remote-workspaces)
                workspace)))
           (gave-up (funcall make "gave-up" 'failed
                             '(remote-file-error "Connection timed out")))
           (broken (funcall make "broken" 'failed
                            '(error "Permission denied")))
           (healthy (funcall make "healthy" 'open nil))
           scheduled)
      (cl-letf (((symbol-function 'remote-workspace--schedule-reconnect)
                 (lambda (workspace) (push workspace scheduled) 'job))
                ((symbol-function 'remote-workspace--transport-error-p)
                 (lambda (_workspace error)
                   (eq (car-safe error) 'remote-file-error))))
        ;; A session opening for another target says nothing about this one.
        (remote-workspace--revive-after-session-open
         nil (remote-route-create :target-id "other" :pipeline-id "other/ssh"
                                  :backend-id "tramp"))
        (should-not scheduled)
        (remote-workspace--revive-after-session-open nil route)
        (should (equal scheduled (list gave-up)))
        (should (eq (remote-workspace-state gave-up) 'disconnected))
        ;; An error that was not about the transport stays as it is.
        (should (eq (remote-workspace-state broken) 'failed))
        (should (eq (remote-workspace-state healthy) 'open))
        (let ((remote-workspace-auto-reconnect nil))
          (setf (remote-workspace-state gave-up) 'failed)
          (setq scheduled nil)
          (remote-workspace--revive-after-session-open nil route)
          (should-not scheduled))))))

(ert-deftest remote-workspace-announces-each-connection-state-once ()
  "Losing, awaiting and regaining a target is said once per target."
  (let ((remote-workspace--announced-states (make-hash-table :test #'equal))
        (remote-workspace-announce-connection-state t)
        (noninteractive nil)
        said)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (push (apply #'format format-string arguments) said)))
              ((symbol-function 'remote-get-target) (lambda (_id) nil)))
      ;; A connection that was never lost has nothing to report.
      (remote-workspace--announce "lab" 'restored)
      (should-not said)
      ;; Two workspaces of one target fail together.
      (remote-workspace--announce "lab" 'lost)
      (remote-workspace--announce "lab" 'lost)
      (should (= (length said) 1))
      (should (string-match-p "lab: connection lost" (car said)))
      (remote-workspace--announce "lab" 'waiting)
      (remote-workspace--announce "lab" 'waiting)
      (should (= (length said) 2))
      (remote-workspace--announce "lab" 'restored)
      (remote-workspace--announce "lab" 'restored)
      (should (= (length said) 3))
      (should (string-match-p "lab: reconnected" (car said)))
      ;; Switched off, the state is still tracked but nothing is said.
      (let ((remote-workspace-announce-connection-state nil))
        (remote-workspace--announce "lab" 'lost)
        (should (= (length said) 3))))))

(ert-deftest remote-workspace-return-probe-reconnects-without-blocking ()
  "After recovery gives up, an asynchronous probe waits for the target."
  (remote-framework-test-with-registry
    (let* ((remote-workspace-auto-reconnect t)
           (remote-workspace--return-probes (make-hash-table :test #'equal))
           (remote-workspace-return-probe-interval 0.05)
           (remote-workspace-return-probe-limit 50)
           (route (remote-route-create
                   :target-id "lab" :pipeline-id "lab/ssh"
                   :backend-id "tramp" :capability 'process-sync
                   :adapter-id "process"))
           (workspace
            (remote-workspace-create
             :id "gave-up" :key '("lab" "gave-up") :target-id "lab"
             :root "/fs:lab:/srv/" :routes (list route) :primary-route route
             :state 'failed
             :error '(remote-file-error "Connection timed out")))
           (answer "false")
           (probes 0)
           scheduled
           (remote-workspace-reachability-probe-functions
            (list (lambda (_route)
                    (cl-incf probes)
                    (list (executable-find answer))))))
      (puthash (remote-workspace-key workspace) workspace remote-workspaces)
      (unwind-protect
          (cl-letf (((symbol-function 'remote-workspace--schedule-reconnect)
                     (lambda (target) (push target scheduled) 'job))
                    ((symbol-function 'remote-workspace--transport-error-p)
                     (lambda (_workspace error)
                       (eq (car-safe error) 'remote-file-error))))
            (remote-workspace--watch-for-return "lab")
            (should (gethash "lab" remote-workspace--return-probes))
            ;; The target stays away: the probe keeps asking, nothing dials.
            (let ((deadline (+ (float-time) 5)))
              (while (and (< probes 3) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (>= probes 3))
            (should-not scheduled)
            (should (eq (remote-workspace-state workspace) 'failed))
            ;; It answers: recovery resumes and the probe ends.
            (setq answer "true")
            (let ((deadline (+ (float-time) 5)))
              (while (and (not scheduled) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (equal scheduled (list workspace)))
            (should (eq (remote-workspace-state workspace) 'disconnected))
            (should-not (gethash "lab" remote-workspace--return-probes))
            ;; An explicit disconnect ends a probe that is still waiting.
            (setf (remote-workspace-state workspace) 'failed)
            (setq answer "false")
            (remote-workspace--watch-for-return "lab")
            (should (gethash "lab" remote-workspace--return-probes))
            (remote-workspace--stop-return-probe "lab")
            (should-not (gethash "lab" remote-workspace--return-probes))
            (let ((count probes))
              (accept-process-output nil 0.3)
              (should (= probes count))))
        (remote-workspace--stop-return-probe "lab")))))

(ert-deftest remote-workspace-transport-failure-is-isolated-by-pipeline ()
  "A failed SSH transport affects its target pipeline, not other targets."
  (remote-framework-test-with-registry
    (let* ((remote-workspace-auto-reconnect nil)
           (failed-route
            (remote-route-create
             :target-id "host-a" :pipeline-id "ssh"
             :backend-id "tramp-rpc"))
           (other-target-route
            (remote-route-create
             :target-id "host-b" :pipeline-id "ssh"
             :backend-id "tramp-rpc"))
           (other-backend-route
            (remote-route-create
             :target-id "host-a" :pipeline-id "ssh"
             :backend-id "tramp"))
           (affected
            (remote-workspace-create
             :key 'affected :target-id "host-a" :state 'open
             :routes (list failed-route)))
           (other-target
            (remote-workspace-create
             :key 'other-target :target-id "host-b" :state 'open
             :routes (list other-target-route)))
           (other-backend
            (remote-workspace-create
             :key 'other-backend :target-id "host-a" :state 'open
             :routes (list other-backend-route))))
      (puthash 'affected affected remote-workspaces)
      (puthash 'other-target other-target remote-workspaces)
      (puthash 'other-backend other-backend remote-workspaces)
      (remote-workspace-handle-transport-failure
       failed-route '(error "connection lost"))
      (should (eq (remote-workspace-state affected) 'disconnected))
      (should (eq (remote-workspace-state other-target) 'open))
      (should (eq (remote-workspace-state other-backend)
                  'disconnected)))))

(ert-deftest remote-workspace-open-keeps-recoverable-owner ()
  "Reentrant consumers must not replace an owner during recovery."
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (resource
            (remote-workspace-register-resource
             workspace 'lsp 'server)))
      (dolist (state '(disconnected reconnecting failed))
        (setf (remote-workspace-state workspace) state)
        (should (eq (remote-workspace-open context :connect nil)
                    workspace))
        (should (eq (remote-get-workspace context) workspace))
        (should (memq resource (remote-workspace-resources workspace)))
        (should (eq (remote-workspace-state workspace) state))))))

(ert-deftest remote-workspace-existing-owner-loads-requested-environment ()
  "Opening files first must not skip project activation on later kernel use."
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/course/test.ipynb"
             :workspace-root "/fs:local:/tmp/course/"))
           (workspace (remote-workspace-open context :connect nil))
           (loads 0))
      (cl-letf (((symbol-function 'remote-environment-ensure)
                 (lambda (_context) (cl-incf loads) 'project-environment)))
        (should (eq (remote-workspace-open
                     context :connect nil :load-environment t)
                    workspace))
        (should (= loads 1))
        (should (eq (remote-workspace-environment workspace)
                    'project-environment))))))

(ert-deftest remote-workspace-background-reconnect-retries-external-epoch-change ()
  (remote-framework-test-with-registry
    (let* ((remote-background-jobs (make-hash-table :test #'equal))
           (remote-background-target-epochs (make-hash-table :test #'equal))
           (remote-workspace-reconnect-delays '(0))
           (remote-connection-open-timeout 8)
           (remote-workspace-reconnect-first-open-timeout 3)
           (context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0)
           observed-timeouts)
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-state workspace) 'disconnected)
      (unwind-protect
          (cl-letf (((symbol-function 'remote-session-invalidate)
                     (lambda (&rest _)
                       (remote-background-invalidate-target "local")))
                    ((symbol-function 'remote-session-acquire)
                     (lambda (&rest _)
                       (cl-incf attempts)
                       (push remote-connection-open-timeout
                             observed-timeouts)
                       (when (= attempts 1)
                         (remote-background-invalidate-target "local"))
                       'session)))
            (let ((job (remote-workspace--schedule-reconnect workspace)))
              (cancel-timer (remote-background-job-timer job))
              (remote-background--run job)
              (should (= attempts 1))
              (should (eq (remote-background-job-state job) 'waiting))
              (should (eq (remote-workspace-state workspace)
                          'disconnected))
              (cancel-timer (remote-background-job-timer job))
              (remote-background--run job)
              (should (= attempts 2))
              (should (equal (nreverse observed-timeouts) '(3 8)))
              (should (eq (remote-background-job-state job) 'complete))
              (should (eq (remote-workspace-state workspace) 'open))))
        (remote-background-clear 'test-cleanup)))))

(ert-deftest remote-workspace-background-reconnect-cannot-reopen-closed-owner ()
  (remote-framework-test-with-registry
    (let* ((remote-background-jobs (make-hash-table :test #'equal))
           (remote-background-target-epochs (make-hash-table :test #'equal))
           (context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context)))
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-state workspace) 'disconnected)
      (unwind-protect
          (cl-letf (((symbol-function 'remote-session-invalidate)
                     (lambda (&rest _) nil))
                    ((symbol-function 'remote-session-acquire)
                     (lambda (&rest _)
                       (remote-workspace-close workspace 'test-close)
                       'session)))
            (let ((job (remote-workspace--schedule-reconnect workspace)))
              (cancel-timer (remote-background-job-timer job))
              (remote-background--run job)
              (should (eq (remote-workspace-state workspace) 'closed))
              (should-not (remote-get-workspace
                           "/fs:local:/tmp/reconnect/"))))
        (remote-background-clear 'test-cleanup)))))

(ert-deftest remote-workspace-manual-async-reconnect-coalesces-and-reports ()
  (remote-framework-test-with-registry
    (let* ((remote-background-jobs (make-hash-table :test #'equal))
           (remote-background-target-epochs (make-hash-table :test #'equal))
           (context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0)
           callbacks)
      (setf (remote-workspace-routes workspace) (list route))
      (should-error (remote-workspace-reconnect-async workspace)
                    :type 'user-error)
      (unwind-protect
          (cl-letf (((symbol-function 'remote-session-invalidate)
                     (lambda (&rest _)
                       (remote-background-invalidate-target "local")))
                    ((symbol-function 'remote-session-acquire)
                     (lambda (&rest _)
                       (cl-incf attempts)
                       'session)))
            (let ((job
                   (remote-workspace-reconnect-async
                    workspace :force t
                    :callback (lambda (_value) (push 'first callbacks)))))
              (should (eq (remote-background-job-state job) 'waiting))
              (should (eq (remote-workspace-state workspace)
                          'reconnecting))
              (should
               (eq job
                   (remote-workspace-reconnect-async
                    workspace
                    :callback
                    (lambda (_value) (push 'second callbacks)))))
              (should (= attempts 0))
              (cancel-timer (remote-background-job-timer job))
              (remote-background--run job)
              (should (= attempts 1))
              (should (eq (remote-workspace-state workspace) 'open))
              (should (= (length callbacks) 2))
              (should (memq 'first callbacks))
              (should (memq 'second callbacks))))
        (remote-background-clear 'test-cleanup)))))

(ert-deftest remote-framework-loads-public-layers ()
  (dolist (feature
           '(remote-core remote-compat remote-pipeline remote-backend
             remote-session remote-fs remote-accelerator remote-process
             remote-channel remote-environment
             remote-path remote-service remote-workspace remote-terminal
             remote-doctor))
    (should (featurep feature)))
  (dolist (function
           '(remote-register-pipeline
             remote-pipeline-stages
             remote-register-backend
             remote-backend-prepare-execution
             remote-backend-prepare-process
             remote-backend-probe
             remote-backend-contract-list
             remote-backend-stdio-bridge-command
             remote-register-file-operation
             remote-register-operation-provider
             remote-operation-provider-list
             remote-session-acquire
             remote-session-invalidate
             remote-make-network-process
             remote-open-network-stream
             remote-port-forward
             remote-reverse-port-forward
             remote-channel-adopt
             remote-channel-of
             remote-channel-live-p
             remote-channel-endpoint
             remote-channel-list
             remote-channel-clear
             remote-channel-recover
             remote-channel-group-open
             remote-channel-group-endpoints
             remote-channel-group-live-p
             remote-channel-group-recover
             remote-channel-group-close
             remote-get-file-operation
             remote-file-operation-list
             remote-unregister-file-operation
             remote-environment-resolve
             remote-workspace-open
             remote-workspace-reconnect
             remote-workspace-track-route
             remote-workspace-find-resource
             remote-workspace-register-recoverable-resource
             remote-workspace-ensure-recoverable-resource
             remote-workspace-add-file-watch
             remote-register-service
             remote-service-provision-directory
             remote-service-ensure
             remote-service-restart
             remote-terminal-open
             remote-terminal-adopt
             remote-terminal-command
             remote-terminal-put-metadata
             remote-terminal-restart
             remote-doctor-register-check
             remote-doctor-unregister-check
             remote-doctor-report
             remote-workspace-context-id))
    (should (fboundp function))))

(ert-deftest remote-pipeline-preserves-ordered-transport-stages ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "via-edge" "tramp"
             :stages
             '((:id "overlay" :transport "tailscale")
               (:id "gateway" :transport "ssh"
                :config (:host "edge"))
               (:id "tunnel" :transport "frp"))
             :config '(:host "lab")))
           (stages (remote-pipeline-stages pipeline)))
      (should (remote-pipeline-p pipeline))
      (should (equal (remote-pipeline-id pipeline) "lab/via-edge"))
      (should
       (equal (mapcar #'remote-pipeline-stage-id stages)
              '("overlay" "gateway" "tunnel")))
      (should
       (equal (mapcar #'remote-pipeline-stage-transport stages)
              '("tailscale" "ssh" "frp"))))))

(ert-deftest remote-builtin-ssh-pipeline-owns-one-lazy-control-path ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "managed" "tramp"
             :stages
             '((:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'process-async
             :adapter-id "process"))
           first second control)
      (unwind-protect
          (progn
            (setq first (remote-pipeline-acquire route context)
                  second (remote-pipeline-acquire route context)
                  control
                  (remote-stage-runtime-handle
                   (car (remote-pipeline-runtime-stages first))))
            (should (eq first second))
            (should (remote-ssh-control-p control))
            (should
             (equal
              (remote-ssh-control-destination control)
              "dev@lab"))
            (should-not
             (file-exists-p
              (remote-ssh-control-path control)))
            (let ((remote-current-pipeline-runtime first))
              (should
               (equal
                (remote-transport-ssh-control-options)
                (list
                 "ControlMaster=auto"
                 (format
                  "ControlPersist=%d"
                  remote-transport-ssh-control-persist)
                 (format
                  "ControlPath=%s"
                  (remote-ssh-control-path control))))))
            (remote-pipeline-release first)
            (should
             (eq (remote-pipeline-runtime-state first) 'open))
            (remote-pipeline-release second)
            (should
             (eq (remote-pipeline-runtime-state first) 'closed))
            (should
             (eq (remote-ssh-control-state control) 'closed)))
        (when (and first
                   (not
                    (eq (remote-pipeline-runtime-state first) 'closed)))
          (remote-pipeline-release first t 'test-cleanup))))))

(ert-deftest remote-pipeline-compiles-multi-hop-tramp-path ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "nested" "tramp"
             :stages
             '((:id "overlay" :transport "tailscale"
                :config (:host "edge.tailnet"))
               (:id "gateway" :transport "ssh"
                :config (:user "dev"))
               (:id "host" :transport "ssh"
                :config (:host "lab.internal"))
               (:id "container" :transport "docker"
                :config (:host "workspace")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'file-read
             :adapter-id "emacs-file")))
      (should
       (equal
        (remote-project-file-name "/fs:lab:/work/a.el" route)
        (concat
         "/ssh:dev@edge.tailnet|ssh:lab.internal"
         "|docker:workspace:/work/a.el"))))))

(ert-deftest remote-pipeline-runtime-is-shared-and-closes-in-reverse ()
  (remote-framework-test-with-registry
    (let (opened closed)
      (dolist (id '("first" "second"))
        (let ((transport-id id))
          (remote-register-transport
           id
           :prepare #'remote-transport--address-prepare
           :connect
           (lambda (_stage endpoint _runtime)
             (push transport-id opened)
             (remote-transport-result-create
              :endpoint endpoint :handle transport-id))
           :disconnect
           (lambda (stage-runtime _runtime)
             (push
              (remote-stage-runtime-handle stage-runtime)
              closed)))))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "managed" "tramp"
               :stages '("first" "second")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :link-id (remote-pipeline-id pipeline)
               :link-plugin-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file"))
             (first (remote-pipeline-acquire route nil))
             (second (remote-pipeline-acquire route nil)))
        (should (eq first second))
        (should (equal (nreverse opened) '("first" "second")))
        (should (= (remote-pipeline-runtime-use-count first) 2))
        (remote-pipeline-release first)
        (should (eq (remote-pipeline-runtime-state first) 'open))
        (remote-pipeline-release second)
        (should (eq (remote-pipeline-runtime-state first) 'closed))
        (should (equal closed '("first" "second")))))))

(ert-deftest remote-pipeline-open-rolls-back-completed-stages ()
  (remote-framework-test-with-registry
    (let (closed)
      (remote-register-transport
       "managed"
       :prepare #'remote-transport--address-prepare
       :connect
       (lambda (_stage endpoint _runtime)
         (remote-transport-result-create
          :endpoint endpoint :handle 'managed))
       :disconnect
       (lambda (_stage-runtime _runtime)
         (push 'managed closed)))
      (remote-register-transport
       "broken"
       :prepare #'remote-transport--address-prepare
       :connect
       (lambda (_stage _endpoint _runtime)
         (error "stage failed")))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "rollback" "tramp"
               :stages '("managed" "broken")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :link-id (remote-pipeline-id pipeline)
               :link-plugin-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file")))
        (should-error (remote-pipeline-open route nil))
        (should (equal closed '(managed)))))))

(ert-deftest remote-pipeline-reentrant-acquire-keeps-one-runtime ()
  (remote-framework-test-with-registry
    (let (route context nested-error)
      (remote-register-transport
       "reentrant-stage"
       :connect
       (lambda (_stage endpoint _runtime)
         (condition-case error
             (remote-pipeline-acquire route context)
           (error (setq nested-error error)))
         (remote-transport-result-create
          :endpoint endpoint :handle 'stage)))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "reentrant" "tramp"
               :stages '("reentrant-stage")
               :config '(:host "lab")))
             (resolved
              (remote-route-create
               :target-id "lab"
               :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file")))
        (setq
         route resolved
         context
         (remote-context-create
          :target-id "lab" :localname "/work/"
          :workspace-root "/fs:lab:/work/"))
        (let ((runtime (remote-pipeline-acquire route context)))
          (should (eq (car nested-error) 'remote-pipeline-busy))
          (should (= (hash-table-count remote-pipeline-runtime-pool) 1))
          (should (= (remote-pipeline-runtime-use-count runtime) 1))
          (remote-pipeline-release runtime)
          (should
           (zerop (hash-table-count
                   remote-pipeline-runtime-pool))))))))

(ert-deftest remote-pipeline-cancelled-open-rolls-back-late-handle ()
  (remote-framework-test-with-registry
    (let (closed)
      (remote-register-transport
       "cancel-stage"
       :connect
       (lambda (_stage endpoint _runtime)
         (remote-pipeline-runtime-clear 'test-cancel)
         (remote-transport-result-create
          :endpoint endpoint :handle 'late-handle))
       :disconnect
       (lambda (stage _runtime)
         (push (remote-stage-runtime-handle stage) closed)))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "cancelled" "tramp"
               :stages '("cancel-stage")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file"))
             (context
              (remote-context-create
               :target-id "lab" :localname "/work/"
               :workspace-root "/fs:lab:/work/")))
        (should-error
         (remote-pipeline-acquire route context)
         :type 'remote-pipeline-cancelled)
        (should (equal closed '(late-handle)))
        (should
         (zerop (hash-table-count
                 remote-pipeline-runtime-pool)))))))

(ert-deftest remote-config-accepts-new-pipeline-vocabulary ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-link-object
     "lab"
     '((id . "via-edge")
       (backends . ("tramp-rpc" "tramp"))
       (stages
        . (((id . "overlay") (transport . "tailscale"))
           ((id . "gateway") (transport . "ssh"))))
       (config . ((host . "lab")))))
    (let* ((pipeline (remote-get-pipeline "via-edge" "lab"))
           (stages (remote-pipeline-stages pipeline)))
      (should
       (equal (remote-pipeline-backend-ids pipeline)
              '("tramp-rpc" "tramp")))
      (should
       (equal (mapcar #'remote-pipeline-stage-transport stages)
              '("tailscale" "ssh"))))))

(ert-deftest remote-config-merges-compatible-pipeline-backends ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp")
       (config . ((host . "lab") (method . "ssh")))))
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp-rpc")
       (config . ((host . "lab")))))
    (should
     (equal
      (remote-pipeline-backend-ids
       (remote-get-pipeline "ssh" "lab"))
      '("tramp" "tramp-rpc")))))

(ert-deftest remote-config-rejects-conflicting-pipeline-definitions ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp")
       (config . ((host . "lab-a")))))
    (should-error
     (remote-config--register-pipeline-object
      "lab"
      '((id . "ssh") (backend . "tramp-rpc")
        (config . ((host . "lab-b")))))
     :type 'error)))

(ert-deftest remote-config-load-is-transactional-on-registration-error ()
  (remote-framework-test-with-registry
    (remote-register-target "stable" :trusted t)
    (remote-register-pipeline
     "stable" "ssh" "tramp" :config '(:host "stable"))
    (let ((file (make-temp-file "remote-config-invalid-" nil ".json"))
          (generation remote-config-generation))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert
               "{\n"
               "  \"version\": 2,\n"
               "  \"targets\": [{\n"
               "    \"id\": \"broken\",\n"
               "    \"pipelines\": [\n"
               "      {\"id\":\"ssh\",\"backend\":\"tramp\","
               "\"config\":{\"host\":\"one\"}},\n"
               "      {\"id\":\"ssh\",\"backend\":\"tramp-rpc\","
               "\"config\":{\"host\":\"two\"}}\n"
               "    ]\n"
               "  }],\n"
               "  \"imports\": []\n"
               "}\n"))
            (should-error (remote-config-load file))
            (should (= remote-config-generation generation))
            (should (remote-get-target "stable"))
            (should (remote-get-pipeline "ssh" "stable"))
            (should-not (remote-get-target "broken")))
        (delete-file file)))))

(ert-deftest remote-board-add-ssh-host-imports-a-private-config-file ()
  (remote-framework-test-with-registry
    (let* ((root (make-temp-file "remote-add-host-" t))
           (json-file (expand-file-name "etc/remote.json" root))
           (ssh-file (expand-file-name "ssh/config" root))
           (remote-config-file json-file)
           (remote-config-generation 0))
      (unwind-protect
          (progn
            (make-directory (file-name-directory json-file) t)
            (make-directory (file-name-directory ssh-file) t)
            (with-temp-file json-file
              (insert
               "{\"version\":2,\"imports\":[{\"type\":\"ssh-config\","
               "\"files\":[\"../ssh/config\"],"
               "\"include\":[\"new-*\",\"second-*\"],"
               "\"pipelines\":["
               "{\"id\":\"ssh\",\"backend\":\"tramp\","
               "\"config\":{\"method\":\"ssh\"}}]}]}\n"))
            (cl-letf (((symbol-function
                        'remote-board--ssh-client-config-file)
                       (lambda () ssh-file)))
              (let ((default-directory "/fs:local:/tmp/"))
                (should (equal (remote-config-ssh-import-files)
                               (list ssh-file)))
                (let ((target
                       (remote-board-add-ssh-host
                        "new-host" "example.invalid" "alice" 2222
                        "~/.ssh/id_test" ssh-file)))
                  (should (equal (remote-target-id target) "new-host"))
                  (should-not (remote-target-trusted target))
                  (should (equal
                           (plist-get
                            (remote-pipeline-config
                             (remote-get-pipeline "ssh" "new-host"))
                            :host)
                           "new-host"))
                  (should (equal
                           (plist-get
                            (remote-pipeline-config
                             (remote-get-pipeline "ssh" "new-host"))
                            :ssh-config-file)
                           ssh-file)))))
            (should (= (logand (file-modes ssh-file) #o777) #o600))
            (with-temp-buffer
              (insert-file-contents ssh-file)
              (should
               (equal (buffer-string)
                      (concat
                       "Host new-host\n"
                       "    HostName example.invalid\n"
                       "    User alice\n"
                       "    Port 2222\n"
                       "    IdentityFile ~/.ssh/id_test\n"))))
            (when (executable-find "ssh")
              (with-temp-buffer
                (should (= 0 (call-process
                              "ssh" nil t nil "-F" ssh-file
                              "-G" "new-host")))
                (should (string-match-p
                         "^hostname example\\.invalid$" (buffer-string)))
                (should (string-match-p
                         "^port 2222$" (buffer-string)))))
            (cl-letf (((symbol-function
                        'remote-board--ssh-client-config-file)
                       (lambda () ssh-file)))
              (let ((before
                     (with-temp-buffer
                       (insert-file-contents ssh-file)
                       (buffer-string))))
                (should-error
                 (remote-board-add-ssh-host
                  "new-host" "other.invalid" nil nil nil ssh-file)
                 :type 'user-error)
                (should-error
                 (remote-board-add-ssh-host
                  "bad\nHost injected" "other.invalid" nil nil nil
                  ssh-file)
                 :type 'user-error)
                (should-error
                 (remote-board-add-ssh-host
                  "unreachable" "other.invalid" nil nil nil
                  (expand-file-name "other-config" root))
                 :type 'user-error)
                (should-error
                 (remote-board-add-ssh-host
                  "excluded" "other.invalid" nil nil nil ssh-file)
                 :type 'user-error)
                (should
                 (equal before
                        (with-temp-buffer
                          (insert-file-contents ssh-file)
                          (buffer-string)))))
              (remote-board-add-ssh-host
               "second-host" "second.invalid" nil 22 nil ssh-file))
            (should (remote-get-target "new-host"))
            (should (remote-get-target "second-host")))
        (delete-directory root t)))))

(ert-deftest remote-board-add-ssh-command-imports-open-ssh-options ()
  (remote-framework-test-with-registry
    (let* ((root (make-temp-file "remote-command-host-" t))
           (json-file (expand-file-name "etc/remote.json" root))
           (ssh-file (expand-file-name "ssh/config" root))
           (real-file (expand-file-name "ssh/managed-config" root))
           (identity (expand-file-name "ssh/my key" root))
           (remote-config-file json-file)
           (remote-config-generation 0))
      (unwind-protect
          (progn
            (make-directory (file-name-directory json-file) t)
            (make-directory (file-name-directory ssh-file) t)
            (with-temp-file real-file
              (insert "ServerAliveInterval 13\n"
                      "Host other-host\n    HostName other.invalid\n"
                      "    User bob\n"
                      "Host *\n    HostName wildcard.invalid\n"
                      "    User wildcard\n"))
            (set-file-modes real-file #o600)
            (make-symbolic-link real-file ssh-file)
            (with-temp-file json-file
              (insert
               "{\"version\":2,\"imports\":[{\"type\":\"ssh-config\","
               "\"files\":[\"../ssh/config\"],\"include\":[\"cmd-*\"],"
               "\"pipelines\":[{\"id\":\"ssh\",\"backend\":\"tramp\","
               "\"config\":{\"method\":\"ssh\"}}]}]}\n"))
            (let* ((command
                     (format
                     "ssh -F %s -i \"%s\" -i ~/.ssh/id_other -p2222 -J jump.example -o ConnectTimeout=7 alice@example.invalid"
                     ssh-file identity))
                   (target
                    (remote-board-add-ssh-command command "cmd-host")))
              (should (equal (remote-target-id target) "cmd-host"))
              (should-not (remote-target-trusted target))
              (should (equal (file-symlink-p ssh-file) real-file))
              (should (= (logand (file-modes real-file) #o777) #o600))
              (should
               (equal
                (plist-get
                 (remote-pipeline-config
                  (remote-get-pipeline "ssh" "cmd-host"))
                 :ssh-config-file)
                ssh-file))
              (with-temp-buffer
                (insert-file-contents ssh-file)
                (should (string-match-p "Host cmd-host" (buffer-string)))
                (should
                 (< (string-match "Host cmd-host" (buffer-string))
                    (string-match "Host \\*" (buffer-string))))
                (should
                 (string-match-p
                  (regexp-quote (format "IdentityFile \"%s\"" identity))
                  (buffer-string)))
                (should
                 (string-match-p "IdentityFile ~/\\.ssh/id_other"
                                 (buffer-string)))
                (should
                 (string-match-p "ProxyJump jump\\.example"
                                 (buffer-string))))
              (when (executable-find "ssh")
                (with-temp-buffer
                  (should (= 0 (call-process
                                "ssh" nil t nil "-G" "-F" ssh-file
                                "cmd-host")))
                  (dolist (line '("hostname example.invalid"
                                  "user alice" "port 2222"
                                  "proxyjump jump.example"
                                  "connecttimeout 7"))
                    (should (string-match-p
                             (concat "^" (regexp-quote line) "$")
                             (buffer-string)))))
                (with-temp-buffer
                  (should (= 0 (call-process
                                "ssh" nil t nil "-G" "-F" ssh-file
                                "other-host")))
                  (dolist (line '("hostname other.invalid" "user bob"
                                  "serveraliveinterval 13"))
                    (should (string-match-p
                             (concat "^" (regexp-quote line) "$")
                             (buffer-string))))))
              (should
               (remote-board-add-ssh-command
                (format
                 "ssh -F %s -o \"ProxyCommand=ssh -W %%h:%%p jump\" alice@example.invalid"
                 ssh-file)
                "cmd-proxy"))
              (when (executable-find "ssh")
                (with-temp-buffer
                  (should (= 0 (call-process
                                "ssh" nil t nil "-G" "-F" ssh-file
                                "cmd-proxy")))
                  (should (string-match-p
                           "^proxycommand ssh -W %h:%p jump$"
                           (buffer-string)))))
              (let ((before
                     (with-temp-buffer
                       (insert-file-contents ssh-file)
                       (buffer-string))))
                (dolist (bad
                         '("sh -c 'ssh evil'"
                           "ssh example.invalid id"
                           "ssh -L 8000:localhost:80 example.invalid"
                           "ssh -o Host=evil example.invalid"
                           "ssh -p abc example.invalid"))
                  (should-error
                   (remote-board-add-ssh-command bad "cmd-bad" ssh-file)
                   :type 'user-error))
                (when (executable-find "ssh")
                  (should-error
                   (remote-board-add-ssh-command
                    "ssh -o BogusSetting=1 example.invalid"
                    "cmd-bad" ssh-file)
                   :type 'user-error))
                (should-error
                 (remote-board-add-ssh-command
                  "ssh -F /tmp/unimported-ssh-config example.invalid"
                  "cmd-bad")
                 :type 'user-error)
                (should
                 (equal before
                        (with-temp-buffer
                          (insert-file-contents ssh-file)
                          (buffer-string)))))))
        (delete-directory root t)))))

(ert-deftest remote-board-ssh-command-core-options-and-client-path ()
  (let ((parsed
         (remote-board--parse-ssh-command
          (concat "ssh -o HostName=example.invalid -o User=alice "
                  "-o Port=2200 -o IdentityFile=/tmp/id_ed25519 myalias"))))
    (should (equal (plist-get parsed :hostname) "example.invalid"))
    (should (equal (plist-get parsed :suggested-alias) "myalias"))
    (should (equal (plist-get parsed :user) "alice"))
    (should (= (plist-get parsed :port) 2200))
    (should (equal (plist-get parsed :identity-file)
                   "/tmp/id_ed25519")))
  (let ((default-directory "/fs:local:/tmp/"))
    (should (equal
             (remote-board--ssh-command-config-file "ssh/config")
             (expand-file-name "ssh/config" invocation-directory))))
  (should
   (equal
    (remote-board--ssh-option-line
     '("ProxyCommand" . "ssh -W %h:%p jump"))
    "    ProxyCommand ssh -W %h:%p jump\n"))
  (let* ((command
          (mapconcat
           #'shell-quote-argument
           '("ssh" "-o" "HostName=example.invalid"
             "-i" "/tmp/my key" "myalias")
           " "))
         (parsed (remote-board--parse-ssh-command command)))
    (should (equal (plist-get parsed :hostname) "example.invalid"))
    (should (equal (plist-get parsed :identity-file) "/tmp/my key")))
  (should
   (equal
    (plist-get
     (remote-board--parse-ssh-command
      "ssh -i /tmp/first -o IdentityFile=/tmp/second myalias")
     :identity-files)
    '("/tmp/first" "/tmp/second")))
  (should-error
   (remote-board--parse-ssh-command "ssh host; touch /tmp/unsafe")
   :type 'user-error)
  (should
   (equal (plist-get (remote-board--parse-ssh-command
                     "ssh\t-l\talice\texample.invalid")
                     :user)
          "alice")))

(ert-deftest remote-config-validates-schema-version ()
  (should (= (remote-config--schema-version '((version . 1))) 1))
  (should (= (remote-config--schema-version '((version . 2))) 2))
  (should-error
   (remote-config--schema-version '((version . 3)))
   :type 'error))

(ert-deftest remote-route-v2-constraints-are-hard-boundaries ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline
     "lab" "primary" '("tramp-rpc" "tramp")
     :priority 100 :config '(:host "lab"))
    (remote-register-pipeline
     "lab" "secondary" "tramp"
     :priority 1 :config '(:host "lab-backup"))
    (let* ((context
            (remote-context-create
             :target-id "lab" :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (route
            (remote-resolve
             "emacs-file" 'file-read context
             '(:pipeline "secondary" :backend "tramp"))))
      (should (equal (remote-route-pipeline-id route) "lab/secondary"))
      (should (equal (remote-route-backend-id route) "tramp"))
      (should-error
       (remote-resolve
        "emacs-file" 'file-read context
        '(:pipeline "secondary" :backend "tramp-rpc")))
      (should
       (equal
        (remote-route-pipeline-id
         (remote-resolve
          "emacs-file" 'file-read context
          '(:exclude-pipelines ("primary"))))
        "lab/secondary")))))

(ert-deftest remote-file-operation-contract-is-explicit-and-extensible ()
  (let ((remote-file-operations (make-hash-table :test #'eq)))
    (remote-fs-register-standard-operations)
    (let ((write
           (gethash 'write-region remote-file-operations))
          (read
           (gethash 'file-attributes remote-file-operations)))
      (should (equal
               (remote-file-operation-spec-path-arguments write)
               '(2)))
      (should (remote-file-operation-spec-mutating write))
      (should-not (remote-file-operation-spec-retry-safe write))
      (should
       (remote-file-operation-spec-retry-safe read)))
    (remote-register-file-operation
     'framework-test-operation
     :capability 'file-read
     :path-arguments '(1)
     :result-kind 'path)
    (should
     (eq
      (remote-file-operation-spec-result-kind
       (gethash 'framework-test-operation remote-file-operations))
      'path))
    (should
     (eq (remote-get-file-operation 'framework-test-operation)
         (car
          (seq-filter
           (lambda (spec)
             (eq
              (remote-file-operation-spec-operation spec)
              'framework-test-operation))
           (remote-file-operation-list)))))
    (should
     (remote-unregister-file-operation 'framework-test-operation))
    (should-not
     (remote-get-file-operation 'framework-test-operation))
    ;; Emacs 32 can dispatch this primitive directly while recursively
    ;; creating parent directories.
    (should
     (remote-get-file-operation 'make-directory-internal))))

(ert-deftest remote-file-file-equal-internal-spelling-uses-public-primitive ()
  (let (called)
    (cl-letf (((symbol-function 'remote-fs--call-routed)
               (lambda (operation arguments)
                 (setq called (cons operation arguments))
                 t)))
      (should
       (remote-fs-file-name-handler
        'file-file-equal-p
        "/fs:local:/tmp/a" "/fs:local:/tmp/b")))
    (should
     (equal called
            '(file-equal-p
              "/fs:local:/tmp/a" "/fs:local:/tmp/b")))))

(ert-deftest remote-file-operation-unknown-fallback-is-conservative ()
  (let ((remote-file-operations (make-hash-table :test #'eq))
        (remote-fs--unknown-operations (make-hash-table :test #'eq))
        (remote-route-log nil))
    (let ((spec (remote-fs--operation-spec
                 'future-emacs-file-operation)))
      (should
       (eq (remote-file-operation-spec-capability spec)
           'file-write))
      (should (remote-file-operation-spec-mutating spec))
      (should-not
       (remote-file-operation-spec-retry-safe spec)))
    (should (= (hash-table-count remote-fs--unknown-operations) 1))
    (should (eq (plist-get (car remote-route-log) :kind)
                'file-operation-warning))))

(ert-deftest remote-file-operation-cross-target-mutation-policy ()
  (should-not
   (remote-fs--validate-cross-target-operation
    'copy-file
    '("/fs:local:/tmp/a" "/fs:lab:/tmp/a")))
  (dolist (operation
           '(rename-file add-name-to-file make-symbolic-link))
    (should-error
     (remote-fs--validate-cross-target-operation
      operation
      '("/fs:local:/tmp/a" "/fs:lab:/tmp/a"))
     :type 'error)))

(ert-deftest remote-workspace-has-stable-identity-and-owns-resources ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "project"
             :workspace-root "/fs:local:/tmp/project/"))
           (first
            (remote-workspace-open context :connect nil))
           (second
            (remote-workspace-open context :connect nil))
           closed)
      (should (eq first second))
      (should (equal (remote-workspace-id first)
                     "local@project"))
      (remote-workspace-register-resource
       first 'test 'handle
       (lambda (value reason)
         (setq closed (list value reason))))
      (remote-workspace-close first 'test-complete)
      (should (equal closed '(handle test-complete)))
      (should (eq (remote-workspace-state first) 'closed))
      (should-not (remote-get-workspace "local@project")))))

(ert-deftest remote-workspace-recoverable-resource-is-keyed-and-idempotent ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "keyed"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (first
            (remote-workspace-ensure-recoverable-resource
             workspace 'lsp '(eglot root) 'server-1
             :recover (lambda (&rest _arguments) 'server-2)))
           (second
            (remote-workspace-ensure-recoverable-resource
             workspace 'lsp '(eglot root) 'server-current
             :recover (lambda (&rest _arguments) 'server-recovered))))
      (should (eq first second))
      (should (= (length (remote-workspace-resources workspace)) 1))
      (should
       (eq (remote-workspace-resource-value first) 'server-current))
      (remote-workspace-recover-resource workspace first)
      (should
       (eq (remote-workspace-resource-value first) 'server-recovered))
      (should
       (eq
        (remote-workspace-find-resource workspace 'lsp '(eglot root))
        first))
      (remote-workspace-forget-resource workspace first)
      (should-not (remote-workspace-resources workspace)))))

(ert-deftest remote-workspace-file-watch-uses-one-target-neutral-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "watch"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (opened nil)
           (closed nil)
           (next 0))
      (cl-letf
          (((symbol-function 'file-notify-add-watch)
            (lambda (file flags callback)
              (push (list file flags callback) opened)
              (list 'descriptor (cl-incf next))))
           ((symbol-function 'file-notify-rm-watch)
            (lambda (descriptor)
              (push descriptor closed))))
        (let ((resource
               (remote-workspace-add-file-watch
                workspace "src/" '(change attribute-change) #'ignore
                :key 'sources)))
          (should
           (equal
            (caar opened)
            "/fs:local:/tmp/project/src/"))
          (remote-workspace-recover-resource workspace resource)
          (should (= (length opened) 2))
          (should (equal closed '((descriptor 1))))
          (should
           (equal
            (remote-workspace-resource-value resource)
            '(descriptor 2))))))))

(ert-deftest remote-file-watch-startup-allows-a-connection ()
  "A watcher that must open its own connection gets the connection deadline."
  (let ((remote-file-watch-startup-timeout 2.5)
        (remote-connection-open-timeout 8))
    (should (= (remote-fs--watch-startup-timeout) 8))
    (let ((remote-connection-open-timeout 1))
      (should (= (remote-fs--watch-startup-timeout) 2.5)))
    (let ((remote-connection-open-timeout nil))
      (should (= (remote-fs--watch-startup-timeout) 2.5)))
    ;; A disabled wait stays disabled.
    (let ((remote-file-watch-startup-timeout 0))
      (should (= (remote-fs--watch-startup-timeout) 0)))))

(ert-deftest remote-file-watch-recovery-is-not-raced-by-its-own-resync ()
  "The old watcher's death schedules a resync; it must not undo the recovery.
On a slow link the resync timer fires while the recovery is still waiting for
its new watcher, kills that watcher and leaves the watch failed."
  (let* ((remote-file-watches (make-hash-table :test #'equal))
         (remote-background-jobs (make-hash-table :test #'equal))
         (remote-background-target-epochs (make-hash-table :test #'equal))
         (remote-background-retry-jitter 0)
         (watch (remote-file-watch-create
                 :id "watch-race" :generation 0 :state 'disconnected
                 :file "/fs:lab:/srv/" :target-id "lab"
                 :descriptor '(remote-file-watch . "watch-race")))
         (created 0) (removed 0) nested)
    (puthash "watch-race" watch remote-file-watches)
    (cl-letf (((symbol-function 'remote-fs--watch-add-physical)
               (lambda (_watch) (list 'physical (cl-incf created))))
              ((symbol-function 'remote-fs--watch-remove-physical)
               (lambda (target)
                 (when (remote-file-watch-physical-descriptor target)
                   (cl-incf removed))
                 (setf (remote-file-watch-physical-descriptor target) nil)))
              ((symbol-function 'remote-fs--watch-await-ready)
               (lambda (target)
                 ;; The resync timer fires during the wait.
                 (unless nested
                   (setq nested t)
                   (should (eq (remote-file-watch-recover target) target)))
                 target)))
      ;; What the killed watcher's sentinel leaves behind.
      (remote-file-watch-resync watch 'backend-stopped)
      (should (gethash '(file-watch-resync "watch-race")
                       remote-background-jobs))
      (should (eq (remote-file-watch-recover watch) watch))
      (should nested)
      (should (= created 1))
      (should (zerop removed))
      (should (equal (remote-file-watch-physical-descriptor watch)
                     '(physical 1)))
      (should (eq (remote-file-watch-state watch) 'open))
      ;; The recovery did the resync's work; nothing is left to fire later.
      (should-not (gethash '(file-watch-resync "watch-race")
                           remote-background-jobs)))))

(ert-deftest remote-file-watch-recovery-rejects-unready-python-backend ()
  "A reconnect cannot report a watch open before its ready handshake."
  (let* ((watch (remote-file-watch-create
                 :id "watch-unready" :generation 0
                 :state 'disconnected))
         (process (make-pipe-process :name "remote-watch-unready-test"
                                     :noquery t))
         (remote-file-watch-startup-timeout 0))
    (unwind-protect
        (progn
          (process-put process 'remote-file-watch-direct t)
          (process-put process 'remote-file-watch-provider 'python-inotify)
          (cl-letf (((symbol-function 'remote-fs--watch-add-physical)
                     (lambda (_watch) process)))
            (should-error (remote-file-watch-recover watch)
                          :type 'remote-backend-unsupported))
          (should (eq (remote-file-watch-state watch) 'failed))
          (should-not (remote-file-watch-physical-descriptor watch))
          (should-not (process-live-p process)))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest remote-file-watch-reports-change-missed-before-ready ()
  "A change made while no watcher observed the file is delivered once."
  (let* ((events nil)
         (process (make-pipe-process :name "remote-watch-gap-test"
                                     :noquery t))
         (make
          (lambda (id since recursive)
            (remote-file-watch-create
             :id id :generation 1 :state 'open :target-id "devbox"
             :descriptor (cons 'remote-file-watch id)
             :file "/fs:devbox:/srv/note.md" :flags '(change)
             :recursive recursive :sequence 0
             :uncovered-since since
             :callback (lambda (event) (push event events))))))
    (unwind-protect
        (let ((now (float-time)))
          ;; Changed two seconds ago, unobserved for the last five.
          (let ((watch (funcall make "watch-gap" (- now 5) nil)))
            (remote-fs--inotify-filter watch 1 process "READY\0002.000\000")
            (should (process-get process 'remote-file-watch-ready))
            (should-not (remote-file-watch-uncovered-since watch))
            (should (equal (mapcar (lambda (event) (nth 1 event)) events)
                           '(changed)))
            (should (equal (nth 2 (car events)) "/fs:devbox:/srv/note.md"))
            ;; A later READY on a covered watch reports nothing.
            (setq events nil)
            (remote-fs--inotify-filter watch 1 process "READY\0000.100\000")
            (should-not events))
          ;; Unchanged for a minute: nothing was missed.
          (let ((watch (funcall make "watch-quiet" (- now 5) nil)))
            (remote-fs--inotify-filter watch 1 process "READY\00060.000\000")
            (should-not events)
            (should-not (remote-file-watch-uncovered-since watch)))
          ;; A missing path and a tree root say nothing about what changed.
          (let ((watch (funcall make "watch-absent" (- now 5) nil)))
            (remote-fs--inotify-filter watch 1 process "READY\000\000")
            (should-not events))
          (let ((watch (funcall make "watch-tree" (- now 5) t)))
            (remote-fs--inotify-filter watch 1 process "READY\0000.100\000")
            (should-not events)))
      (delete-process process))))

(ert-deftest remote-file-watch-replacement-tree-asks-for-missed-files ()
  "Only a tree watcher that replaces a lost one reports recent files."
  (let ((remote-file-watch-startup-timeout 2.5)
        (remote-connection-open-timeout 10)
        (since (- (float-time) 60)))
    (cl-flet ((window (state recursive since)
                (remote-fs--watch-recent-window
                 (remote-file-watch-create
                  :id "watch-window" :state state :recursive recursive
                  :uncovered-since since))))
      (should-not (window 'opening t since))
      (should-not (window 'stopped nil since))
      (should-not (window 'stopped t nil))
      ;; Sixty seconds lost, plus the time a new watcher may need.
      (should (<= 71 (window 'stopped t since) 73)))))

(ert-deftest remote-file-watch-marks-unobserved-interval ()
  "Losing the backend descriptor starts the interval a watcher reports on."
  (let* ((process (make-pipe-process :name "remote-watch-interval-test"
                                     :noquery t))
         (watch (remote-file-watch-create
                 :id "watch-interval" :generation 1 :state 'open
                 :physical-descriptor process)))
    (unwind-protect
        (progn
          (process-put process 'remote-file-watch-direct t)
          (should-not (remote-file-watch-uncovered-since watch))
          (remote-fs--watch-remove-physical watch)
          (should (numberp (remote-file-watch-uncovered-since watch)))
          (should-not (process-live-p process)))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest remote-workspace-reconnect-retries-transport-and-recovers-resources ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "recovery"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0)
           closed recovered)
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-primary-route workspace) route)
      (let ((resource
             (remote-workspace-register-recoverable-resource
              workspace 'watch 'old-watch
              :close
              (lambda (value reason)
                (setq closed (list value reason)))
              :recover
              (lambda (_resource _workspace)
                (setq recovered t)
                'new-watch))))
        (remote-workspace-register-resource
         workspace 'terminal 'shell)
        (let ((remote-workspace-reconnect-delays '(0 0 0)))
          (cl-letf
              (((symbol-function 'remote-session-invalidate)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'remote-session-acquire)
                (lambda (&rest _arguments)
                  (cl-incf attempts)
                  (when (< attempts 4)
                    (signal 'remote-transport-error
                            '("injected transport loss")))
                  'session)))
            (remote-workspace-reconnect workspace)))
        (should (= attempts 4))
        (should recovered)
        (should (equal closed '(old-watch transport-recovery)))
        (should (eq (remote-workspace-resource-value resource)
                    'new-watch))
        (should (eq (remote-workspace-resource-state resource) 'open))
        (should
         (eq
          (remote-workspace-resource-state
           (seq-find
            (lambda (candidate)
              (eq (remote-workspace-resource-kind candidate)
                  'terminal))
            (remote-workspace-resources workspace)))
          'disconnected))
        (should (eq (remote-workspace-state workspace) 'open))))))

(ert-deftest remote-workspace-first-auto-reconnect-has-a-short-open-deadline ()
  (remote-framework-test-with-registry
    (let* ((remote-connection-open-timeout 8)
           (remote-workspace-reconnect-first-open-timeout 3)
           (context
            (remote-context-create
             :target-id "local" :localname "/tmp/reconnect/a.el"
             :workspace-root "/fs:local:/tmp/reconnect/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           observed)
      (setf (remote-workspace-routes workspace) (list route))
      (cl-letf (((symbol-function 'remote-session-invalidate)
                 (lambda (&rest _) nil))
                ((symbol-function 'remote-session-acquire)
                 (lambda (&rest _)
                   (push remote-connection-open-timeout observed)
                   'session))
                ((symbol-function 'remote-workspace--recover-after-transport)
                 (lambda (_workspace) 'open)))
        (remote-workspace--reconnect-once workspace t)
        (remote-workspace--reconnect-once workspace)
        (let* ((pipeline (remote-route-pipeline route))
               (config (copy-sequence (remote-pipeline-config pipeline))))
          (setf (remote-pipeline-config pipeline)
                (plist-put config :connect-timeout 12))
          (remote-workspace--reconnect-once workspace t))
        (should (equal (nreverse observed) '(3 8 8)))))))

(ert-deftest remote-workspace-reconnect-quiesces-forwards-before-session-reset ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           events)
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-primary-route workspace) route)
      (let ((resource
             (remote-workspace-register-recoverable-resource
              workspace 'forward 'old-forward
              :close (lambda (value reason)
                       (push (list 'close value reason) events))
              :recover (lambda (_resource _owner)
                         (push 'recover events)
                         'new-forward))))
        (cl-letf (((symbol-function 'remote-session-invalidate)
                   (lambda (&rest _args)
                     (push 'invalidate events)))
                  ((symbol-function 'remote-session-acquire)
                   (lambda (&rest _args)
                     (push 'acquire events)
                     'session)))
          (remote-workspace-reconnect workspace))
        (should
         (equal (reverse events)
                '((close old-forward workspace-reconnect)
                  invalidate acquire recover)))
        (should (eq (remote-workspace-resource-value resource)
                    'new-forward))
        (should (eq (remote-workspace-resource-state resource)
                    'open))))))

(ert-deftest remote-workspace-does-not-retry-operation-errors ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "no-retry"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0))
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-primary-route workspace) route)
      (cl-letf
          (((symbol-function 'remote-session-invalidate)
            (lambda (&rest _arguments) nil))
           ((symbol-function 'remote-session-acquire)
            (lambda (&rest _arguments)
              (cl-incf attempts)
              (error "permission denied"))))
        (should-error
         (remote-workspace-reconnect workspace)
         :type 'error))
      (should (= attempts 1))
      (should (eq (remote-workspace-state workspace) 'failed)))))

(ert-deftest remote-native-network-api-keeps-process-compatibility ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (server
            (remote-make-network-process
             :name "remote-channel-test"
             :server t :host "127.0.0.1" :service t
             :noquery t :remote-context context))
           (channel (remote-channel-of server)))
      (unwind-protect
          (progn
            (should (processp server))
            (should (remote-channel-p channel))
            (should (eq (remote-channel-kind channel) 'listener))
            (should (eq (remote-channel-handle channel) server))
            (should
             (equal
              (remote-channel-endpoint server 'remote)
              (list :host (process-contact server :host)
                    :port (process-contact server :service)))))
        (remote-close-channel server)))))

(ert-deftest remote-channel-adopts-third-party-listener-idempotently ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (server
            (make-network-process
             :name "remote-adopt-test"
             :server t :host "127.0.0.1" :service t
             :noquery t))
           (first
            (remote-channel-adopt
             server :kind 'listener :context context
             :metadata '(:application "test")))
           (second
            (remote-channel-adopt
             server :kind 'listener :context context)))
      (unwind-protect
          (progn
            (should (eq first second))
            (should (eq (remote-channel-of server) first))
            (should
             (equal
              (plist-get
               (plist-get (car (remote-channel-list "local"))
                          :metadata)
               :application)
              "test")))
        (remote-close-channel server)))))

(ert-deftest remote-channel-group-is-atomic-recoverable-and-named ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "channel-group"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           destinations group replacement)
      (unwind-protect
          (progn
            (dotimes (index 2)
              (push
               (make-network-process
                :name (format "remote-group-destination-%d" index)
                :server t :host "127.0.0.1" :service t :noquery t)
               destinations))
            (setq group
                  (remote-channel-group-open
                   `((shell . (:host "127.0.0.1"
                              :port ,(process-contact
                                      (nth 0 destinations) :service)))
                     (iopub . (:host "127.0.0.1"
                              :port ,(process-contact
                                      (nth 1 destinations) :service))))
                   :context context :workspace workspace
                   :key 'jupyter-test))
            (should (remote-channel-group-live-p group))
            (should (equal (mapcar #'car
                                   (remote-channel-group-endpoints
                                    group 'local))
                           '(shell iopub)))
            (should
             (remote-workspace-find-resource
              workspace 'channel-group 'jupyter-test))
            (setq replacement (remote-channel-group-recover group))
            (should-not (remote-channel-group-live-p group))
            (should (remote-channel-group-live-p replacement))
            (should
             (= (remote-channel-group-generation replacement) 2))
            (remote-channel-group-close replacement)
            (should-not (remote-channel-group-live-p replacement))
            ;; Closing twice is deliberately harmless.
            (remote-channel-group-close replacement))
        (when (remote-channel-group-p group)
          (ignore-errors (remote-channel-group-close group)))
        (when (remote-channel-group-p replacement)
          (ignore-errors (remote-channel-group-close replacement)))
        (dolist (process destinations)
          (when (process-live-p process)
            (delete-process process)))))))

(ert-deftest remote-channel-group-rolls-back-partial-open ()
  (remote-framework-test-with-registry
    (let ((calls 0)
          closed)
      (cl-letf
          (((symbol-function 'remote-port-forward)
            (lambda (&rest _arguments)
              (cl-incf calls)
              (if (= calls 2)
                  (error "second endpoint failed")
                'first-forward)))
           ((symbol-function 'remote-close-channel)
            (lambda (value) (push value closed))))
        (should-error
         (remote-channel-group-open
          '((first . (:host "127.0.0.1" :port 1))
            (second . (:host "127.0.0.1" :port 2)))
          :context
          (remote-context-create
           :target-id "local" :localname "/tmp/"
           :workspace-root "/fs:local:/tmp/"))
         :type 'error)
        (should (equal closed '(first-forward)))
        (should (zerop (hash-table-count remote-channel-groups)))))))

(ert-deftest remote-channel-group-rolls-back-failed-registration ()
  "A group that fails after opening must leave no registry or recovery owner."
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (register
            (symbol-function 'remote-workspace-ensure-recoverable-resource))
           closed)
      (cl-letf (((symbol-function 'remote-port-forward)
                 (lambda (&rest _) 'forward))
                ((symbol-function 'remote-close-channel)
                 (lambda (value) (push value closed)))
                ((symbol-function 'remote-workspace-ensure-recoverable-resource)
                 (lambda (&rest args)
                   (apply register args)
                   (error "Registration failed"))))
        (should-error
         (remote-channel-group-open
          '((shell . (:host "127.0.0.1" :port 1)))
          :context context :workspace workspace :key 'failed-group))
        (should (equal closed '(forward)))
        (should (zerop (hash-table-count remote-channel-groups)))
        (should-not (remote-workspace-find-resource
                     workspace 'channel-group 'failed-group))))))

(ert-deftest remote-native-reverse-forward-relays-and-cleans-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (destination
            (make-network-process
             :name "remote-forward-echo"
             :server t :host "127.0.0.1" :service t
             :noquery t :coding 'binary
             :log
             (lambda (_server client _message)
               (set-process-filter
                client
                (lambda (process string)
                 (process-send-string process string))))))
           (buffers-before (buffer-list))
           forward client buffer)
      (unwind-protect
          (progn
            (setq forward
                  (remote-reverse-port-forward
                   (list
                    :host "127.0.0.1"
                    :port (process-contact destination :service))
                   :context context :register nil
                   :stable-endpoint t))
            (let* ((endpoint
                    (remote-channel-endpoint forward 'remote))
                   (channel (remote-channel-of forward)))
              (should (remote-channel-p channel))
              (should (remote-channel-live-p forward))
              (should (equal (plist-get endpoint :host)
                             "127.0.0.1"))
              (should (integerp (plist-get endpoint :port)))
              (should (= (length (remote-channel-list "local")) 1))
              (setq buffer (generate-new-buffer
                            " *remote-forward-client*"))
              (setq client
                    (make-network-process
                     :name "remote-forward-client"
                     :buffer buffer
                     :host (plist-get endpoint :host)
                     :service (plist-get endpoint :port)
                     :coding 'binary :noquery t))
              (process-send-string client "roundtrip")
              (let ((deadline (+ (float-time) 2)))
                (while
                    (and
                     (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (not (string-match-p
                             "roundtrip" (buffer-string))))
                     (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should
               (with-current-buffer buffer
                 (string-match-p "roundtrip" (buffer-string))))
              (should-not
               (cl-find-if
                (lambda (candidate)
                  (and
                   (not (memq candidate buffers-before))
                   (string-prefix-p
                    "remote-native-forward-" (buffer-name candidate))))
                (buffer-list)))
              (remote-close-channel forward)
              (should (eq (remote-forward-state forward) 'closed))
              (should (eq (remote-channel-state channel) 'closed))
              (should-not (remote-channel-list "local"))
              (let ((replacement (remote-channel-recover channel)))
                (should (remote-channel-live-p replacement))
                (should
                 (equal
                  (remote-channel-endpoint replacement 'remote)
                  endpoint))
                (should (= (length (remote-channel-list "local")) 1))
                (remote-close-channel replacement)
                (should-not (remote-channel-list "local")))))
        (when (and (processp client) (process-live-p client))
          (delete-process client))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (and (remote-forward-p forward)
                   (not (eq (remote-forward-state forward) 'closed)))
          (remote-close-channel forward))
        (when (process-live-p destination)
          (delete-process destination))))))

(ert-deftest remote-routed-listener-process-contact-exposes-target-port ()
  (let* ((server
          (make-network-process
           :name "remote-listener-contact"
           :server t :host "127.0.0.1" :service t :noquery t))
         (physical-port (process-contact server :service))
         (logical-port (1+ physical-port))
         (was-installed
          remote-channel--process-contact-advice-installed))
    (unwind-protect
        (progn
          (process-put
           server 'remote-listen-endpoint
           (list :host "127.0.0.1" :port logical-port))
          (remote-channel-install-compatibility)
          (should (= (process-contact server :service) logical-port))
          (should
           (equal
            (process-contact server)
            (list "127.0.0.1" logical-port))))
      (delete-process server)
      (unless was-installed
        (remote-channel-uninstall-compatibility)))))

(ert-deftest remote-forward-unexpected-exit-reports-transport-failure ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (forward
            (remote-port-forward
             '(:host "127.0.0.1" :port 9)
             :context context :register nil))
           (channel (remote-channel-of forward))
           reported)
      (unwind-protect
          (cl-letf
              (((symbol-function 'remote-report-route-failure)
                (lambda (route error)
                  (setq reported (list route error))
                  'transport)))
            (delete-process (remote-forward-handle forward))
            (accept-process-output nil 0.01)
            (should (eq (remote-forward-state forward) 'failed))
            (should (eq (remote-channel-state channel) 'failed))
            (should (eq (car reported)
                        (remote-channel-route channel)))
            (should
             (eq (car (cadr reported))
                 'remote-transport-error))
            (should-not (remote-channel-list "local")))
        (remote-close-channel forward)))))

(ert-deftest remote-forward-recovery-keeps-allocated-local-port ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (forward
            (remote-port-forward
             '(:host "127.0.0.1" :port 9)
             :local-endpoint '(:host "127.0.0.1" :port 0)
             :context context :register nil))
           (channel (remote-channel-of forward))
           (port (plist-get (remote-channel-endpoint forward 'local)
                            :port))
           replacement)
      (unwind-protect
          (progn
            (should (and (integerp port) (> port 0)))
            (remote-close-channel forward)
            (setq replacement
                  (funcall (remote-channel-recovery-function channel)))
            (should (not (eq replacement forward)))
            (should
             (= (plist-get (remote-channel-endpoint replacement 'local)
                           :port)
                port)))
        (ignore-errors (remote-close-channel forward))
        (when replacement
          (ignore-errors (remote-close-channel replacement)))))))

(ert-deftest remote-doctor-reports-local-routing-boundaries ()
  (remote-framework-test-with-registry
    (let ((report (remote-doctor-report "local")))
      (should (memq (plist-get report :status) '(ok warning)))
      (dolist (capability '(file-read process-sync network-client))
        (should
         (eq
          (plist-get
           (seq-find
            (lambda (check)
              (eq
               (plist-get check :name)
               (intern (format "route:%s" capability))))
            (plist-get report :checks))
           :status)
          'ok))))))

(ert-deftest remote-doctor-report-views-are-frame-owned ()
  "Running Doctor in one frame leaves another frame's report untouched."
  (remote-framework-test-with-registry
    (let* ((real-frame (selected-frame))
           (other-frame (make-symbol "other-frame"))
           (other-parameters (make-hash-table :test #'eq))
           (original-frame-parameter (symbol-function 'frame-parameter))
           (original-set-frame-parameter (symbol-function 'set-frame-parameter))
           (original-frame-live-p (symbol-function 'frame-live-p))
           first second)
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (frame key)
                   (if (eq frame other-frame)
                       (gethash key other-parameters)
                     (funcall original-frame-parameter frame key))))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame key value)
                   (if (eq frame other-frame)
                       (puthash key value other-parameters)
                     (funcall original-set-frame-parameter frame key value))))
                ((symbol-function 'frame-live-p)
                 (lambda (frame)
                   (if (eq frame other-frame) t
                     (funcall original-frame-live-p frame)))))
        (unwind-protect
            (save-window-excursion
              (setq second (remote-doctor--buffer other-frame))
              (with-current-buffer second
                (let ((inhibit-read-only t))
                  (insert "Other frame report")))
              (remote-doctor "local")
              (setq first (current-buffer))
              (should-not (eq first second))
              (with-current-buffer second
                (should (equal (buffer-string) "Other frame report")))
              (remote-doctor--frame-deleted other-frame)
              (should-not (buffer-live-p second))
              (should (buffer-live-p first)))
          (when (buffer-live-p second) (kill-buffer second))
          (when (buffer-live-p first) (kill-buffer first)))))))

(ert-deftest remote-doctor-probe-reports-signaled-process-output ()
  "A nonnumeric process result remains a useful probe error."
  (remote-framework-test-with-registry
    (cl-letf (((symbol-function 'remote-process-file)
               (lambda (_program _infile destination _display &rest _args)
                 (with-current-buffer destination
                   (insert "partial output"))
                 "terminated by signal")))
      (let ((check (remote-doctor--probe (remote-get-target "local"))))
        (should (eq (plist-get check :status) 'error))
        (should (string-match-p "terminated by signal"
                                (plist-get check :detail)))
        (should (string-match-p "partial output"
                                (plist-get check :detail)))
        (should-not (get-buffer " *remote-doctor-probe*"))))))

(ert-deftest remote-doctor-includes-and-isolates-consumer-checks ()
  (remote-framework-test-with-registry
    (let ((check
           (lambda (target probe)
             (list
              :name 'consumer-test :status 'ok
              :detail
              (format "%s/%s" (remote-target-id target) (if probe 1 0))))))
      (remote-doctor-register-check check)
      (remote-doctor-register-check check)
      (let ((checks (plist-get (remote-doctor-report "local") :checks)))
        (should
         (eq
          (plist-get
           (seq-find
            (lambda (entry)
              (eq (plist-get entry :name) 'consumer-test))
            checks)
           :status)
          'ok)))
      (remote-doctor-unregister-check check)
      (should-not remote-doctor-check-functions))))

(ert-deftest remote-doctor-isolates-malformed-consumer-checks ()
  "A malformed extension result appears as one error, not a broken report."
  (remote-framework-test-with-registry
    (let* ((bad (lambda (_target _probe)
                  (list (list :name 'bad :status "not-a-status"))))
           (good (lambda (_target _probe)
                   (list :name 'good :status 'ok :detail "still here"))))
      (remote-doctor-register-check bad)
      (remote-doctor-register-check good)
      (let* ((report (remote-doctor-report "local"))
             (checks (plist-get report :checks))
             (consumer-error
              (seq-find
               (lambda (check)
                 (string-prefix-p "consumer:"
                                  (format "%s" (plist-get check :name))))
               checks)))
        (should (eq (plist-get consumer-error :status) 'error))
        (should (string-match-p "Invalid Remote Doctor check"
                                (plist-get consumer-error :detail)))
        (should (seq-find
                 (lambda (check)
                   (eq (plist-get check :name) 'good))
                 checks))))))

(ert-deftest remote-service-provisioning-is-trust-gated ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted nil)
    (let* ((context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-id "main"
             :workspace-root "/fs:lab:/work/"))
           (workspace
            (remote-workspace-open context :connect nil))
           installed)
      (remote-register-service
       "agent"
       :capabilities '(files processes channels)
       :probe
       (lambda (_context)
         (and installed
              '(:available t :version "1")))
       :provision
       (lambda (_context _probe)
         (setq installed t)))
      (should-error
       (remote-workspace-ensure-service
        workspace "agent" :provision t)
       :type 'remote-service-untrusted)
      (setf
       (remote-target-trusted
        (remote-get-target "lab"))
       t)
      (let ((instance
             (remote-workspace-ensure-service
              workspace "agent" :provision t)))
        (should installed)
        (should (remote-service-instance-live-p instance))
        (should (equal
                 (remote-service-instance-version instance)
                 "1"))
        (should
         (eq instance
             (remote-workspace-ensure-service
              workspace "agent" :provision t)))
        (should (= (remote-service-instance-use-count instance) 1))
        (should (= (length (remote-workspace-resources workspace)) 1)))
      (remote-workspace-close workspace)
      (should-not (remote-service-list)))))

(ert-deftest remote-service-directory-provisioning-is-trust-gated ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted nil)
    (let ((source (make-temp-file "remote-service-source-" t))
          (context
           (remote-context-create
            :target-id "lab"
            :localname "/work/"
            :workspace-id "main"
            :workspace-root "/fs:lab:/work/")))
      (unwind-protect
          (should-error
           (remote-service-provision-directory
            "test-tool" source "/fs:lab:/cache/test-tool/v1/"
            :context context
            :ready-file "bin/tool"
            :ready-kind 'executable)
           :type 'remote-service-untrusted)
        (delete-directory source t)))))

(ert-deftest remote-service-directory-provisioning-publishes-and-repairs ()
  (remote-framework-test-with-registry
    (let* ((source (make-temp-file "remote-service-source-" t))
           (target-root (make-temp-file "remote-service-target-" t))
           (native-install
            (expand-file-name "cache/test-tool/v1/" target-root))
           (logical-root
            (remote-make-file-name
             "local" (file-name-as-directory target-root)))
           (install
            (remote-make-file-name
             "local" (file-name-as-directory native-install)))
           (context
            (remote-context-create
             :target-id "local"
             :localname target-root
             :workspace-id "test"
             :workspace-root logical-root))
           (prepare-count 0)
           (prepare
            (lambda (_context staging)
              (setq prepare-count (1+ prepare-count))
              (let* ((native-staging (remote-file-local-name staging))
                     (launcher
                      (expand-file-name "bin/tool" native-staging)))
                (make-directory (file-name-directory launcher) t)
                (with-temp-file launcher
                  (insert "#!/bin/sh\nexit 0\n"))
                (set-file-modes launcher #o700)))))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "payload.txt" source)
              (insert "payload"))
            (should
             (equal
              (remote-service-provision-directory
               "test-tool" source install
               :context context
               :adapter "exec"
               :payload-directory "lib"
               :ready-file "bin/tool"
               :ready-kind 'executable
               :prepare prepare)
              (file-name-as-directory install)))
            (should (= prepare-count 1))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "lib/payload.txt" native-install))
                (buffer-string))
              "payload"))
            ;; A ready version is a cache hit and is not repackaged.
            (remote-service-provision-directory
             "test-tool" source install
             :context context :adapter "exec"
             :payload-directory "lib"
             :ready-file "bin/tool" :ready-kind 'executable
             :prepare prepare)
            (should (= prepare-count 1))
            ;; An interrupted/incomplete versioned leaf is replaced in place.
            (delete-file (expand-file-name "bin/tool" native-install))
            (with-temp-file (expand-file-name "stale" native-install)
              (insert "incomplete"))
            (remote-service-provision-directory
             "test-tool" source install
             :context context :adapter "exec"
             :payload-directory "lib"
             :ready-file "bin/tool" :ready-kind 'executable
             :prepare prepare)
            (should (= prepare-count 2))
            (should
             (file-executable-p
              (expand-file-name "bin/tool" native-install)))
            (should-not
             (file-exists-p (expand-file-name "stale" native-install)))
            ;; An executable can still be corrupt.  A consumer validator
            ;; repairs that versioned leaf without accepting stale contents.
            (let* ((launcher (expand-file-name "bin/tool" native-install))
                   (validate
                    (lambda (_context candidate)
                      (with-temp-buffer
                        (insert-file-contents
                         (expand-file-name
                          "bin/tool" (remote-file-local-name candidate)))
                        (equal (buffer-string) "#!/bin/sh\nexit 0\n")))))
              (remote-service-provision-directory
               "test-tool" source install
               :context context :adapter "exec"
               :payload-directory "lib"
               :ready-file "bin/tool" :ready-kind 'executable
               :prepare prepare :validate validate)
              (should (= prepare-count 2))
              (with-temp-file launcher
                (insert "#!/bin/sh\nexit 9\n"))
              (set-file-modes launcher #o700)
              (remote-service-provision-directory
               "test-tool" source install
               :context context :adapter "exec"
               :payload-directory "lib"
               :ready-file "bin/tool" :ready-kind 'executable
               :prepare prepare :validate validate)
              (should (= prepare-count 3))
              (should (funcall validate context install))))
        (delete-directory source t)
        (delete-directory target-root t)))))

(ert-deftest remote-target-service-restarts-in-place-for-every-workspace ()
  "A target-scoped restart must not leave another workspace with a stale object."
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((left-context
            (remote-context-create
             :target-id "lab"
             :localname "/work/left/a.el"
             :workspace-id "left"
             :workspace-root "/fs:lab:/work/left/"))
           (right-context
            (remote-context-create
             :target-id "lab"
             :localname "/work/right/a.el"
             :workspace-id "right"
             :workspace-root "/fs:lab:/work/right/"))
           (left
            (remote-workspace-open left-context :connect nil))
           (right
            (remote-workspace-open right-context :connect nil))
           (starts 0)
           (stops 0))
      (remote-register-service
       "shared-agent"
       :scope 'target
       :capabilities '(processes channels)
       :start
       (lambda (_context _probe)
         (list :handle (format "handle-%d" (cl-incf starts))))
       :stop
       (lambda (_instance _reason)
         (cl-incf stops)))
      (let* ((instance
              (remote-workspace-ensure-service left "shared-agent"))
             (right-instance
              (remote-workspace-ensure-service right "shared-agent")))
        (should (eq instance right-instance))
        (should (= (remote-service-instance-use-count instance) 2))
        (should
         (eq instance
             (remote-workspace-ensure-service
              left "shared-agent" :force t)))
        (should (eq instance
                    (car (remote-workspace-services right))))
        (should (equal (remote-service-instance-handle instance)
                       "handle-2"))
        (should (= (remote-service-instance-use-count instance) 2))
        (should (= starts 2))
        (should (= stops 1))
        ;; Exercise the transport-recovery path, which first releases the
        ;; left workspace's reference and then reacquires with FORCE.
        (let ((resource
               (seq-find
                (lambda (candidate)
                  (eq
                   (remote-workspace-resource-kind candidate)
                   'service))
                (remote-workspace-resources left))))
          (should
           (eq instance
               (remote-workspace-recover-resource left resource)))
          (should
           (eq instance
               (remote-workspace-resource-value resource))))
        (should (eq instance
                    (car (remote-workspace-services right))))
        (should (equal (remote-service-instance-handle instance)
                       "handle-3"))
        (should (= (remote-service-instance-use-count instance) 2))
        (should (= starts 3))
        (should (= stops 2))
        (remote-workspace-close left)
        (should (remote-service-instance-live-p instance))
        (should (= (remote-service-instance-use-count instance) 1))
        (remote-workspace-close right)
        (should-not (remote-service-list))
        (should (= stops 3))))))

(ert-deftest remote-terminal-runs-through-routed-pty-boundary ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "tmp"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (terminal
            (remote-terminal-open
             workspace
             :name "test"
             :shell "/bin/sh"
             :arguments '("-c" "printf terminal-ready"))))
      (unwind-protect
          (progn
            (while
                (process-live-p
                 (remote-terminal-process terminal))
              (accept-process-output
               (remote-terminal-process terminal) 0.1))
           (should
             (string-match-p
              "terminal-ready"
              (with-current-buffer
                  (remote-terminal-buffer terminal)
                (buffer-string))))
            (should
             (eq (plist-get
                  (remote-process-description
                   (remote-terminal-process terminal))
                  :class)
                 'interactive))
            (should
             (eq (remote-route-capability
                  (process-get (remote-terminal-process terminal)
                               'remote-route))
                 'pty)))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-abnormal-exit-keeps-restartable-buffer ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "terminal-exit"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (terminal
            (remote-terminal-open
             workspace :name "abnormal-exit"
             :shell "/bin/sh" :arguments '("-c" "read line; exit 255"))))
      (unwind-protect
          (progn
            (remote-terminal-send-string terminal "finish\n")
            (let ((deadline (+ (float-time) 5)))
              (while (and (eq (remote-terminal-state terminal) 'open)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (remote-terminal-state terminal) 'disconnected))
            (should (buffer-live-p (remote-terminal-buffer terminal)))
            (should (memq terminal (remote-terminal-list workspace))))
        (remote-workspace-close workspace)))))

(ert-deftest remote-path-login-shell-asks-for-the-shell-alone ()
  "Knowing which shell to start must not run the person's shell startup."
  (remote-framework-test-with-registry
    (let* ((remote-path-facts-cache (make-hash-table :test #'equal))
           (target (remote-register-target "lab" :trusted t))
           (context
            (remote-context-create
             :target-id "lab" :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           commands)
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (program &rest arguments)
                   (push (cons program (plist-get arguments :args)) commands)
                   (remote-exec-result-create
                    :status 0
                    :stdout "motd banner\n__EMACS_LOGIN_SHELL__/usr/bin/fish"))))
        (should (equal (remote-path-login-shell context) "/usr/bin/fish"))
        (should (= (length commands) 1))
        ;; A plain `sh -c': no login or interactive startup files.
        (should (equal (caar commands) "sh"))
        (should (equal (nth 1 (car commands)) "-c"))
        ;; The answer is a target fact; asking again costs nothing.
        (should (equal (remote-target-shell target) "/usr/bin/fish"))
        (should (equal (remote-path-login-shell context) "/usr/bin/fish"))
        (should (= (length commands) 1)))
      ;; Facts that are already known answer without touching the target.
      (setf (remote-target-shell target) nil)
      (puthash "lab"
               (remote-path-facts-create :target-id "lab" :shell "/bin/zsh")
               remote-path-facts-cache)
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (&rest _) (error "Must not probe"))))
        (should (equal (remote-path-login-shell context) "/bin/zsh")))
      ;; With NO-CONNECT nothing is asked unless a session is already open.
      (clrhash remote-path-facts-cache)
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (&rest _) (error "Must not connect")))
                ((symbol-function 'remote-path--session-ready-p)
                 (lambda (_context) nil)))
        (should-not (remote-path-login-shell context t)))
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (&rest _)
                   (remote-exec-result-create
                    :status 0 :stdout "__EMACS_LOGIN_SHELL__/bin/dash")))
                ((symbol-function 'remote-path--session-ready-p)
                 (lambda (_context) t)))
        (should (equal (remote-path-login-shell context t) "/bin/dash")))
      (setf (remote-target-shell target) nil)
      ;; A target that reports no shell yields nil, not an empty string.
      (clrhash remote-path-facts-cache)
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (&rest _)
                   (remote-exec-result-create
                    :status 0 :stdout "__EMACS_LOGIN_SHELL__"))))
        (should-not (remote-path-login-shell context))
        (should-not (remote-target-shell target))))))

(ert-deftest remote-terminal-probes-login-shell-and-keeps-fallback ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-id "shell"
             :workspace-root "/fs:lab:/work/"))
           (workspace
            (remote-workspace-create
             :id "lab@shell"
             :target-id "lab"
             :workspace-id "shell"
             :root "/fs:lab:/work/"
             :context context
             :state 'open))
           (deferred
            (append remote-terminal--target-shell-command '("-l"))))
      (let (asked)
        (cl-letf
            (((symbol-function 'remote-path-login-shell)
              (lambda (_context &optional no-connect)
                (setq asked no-connect)
                "/usr/bin/zsh")))
          (should
           (equal
            (remote-terminal-command workspace "default" t)
            '("/usr/bin/zsh" "-l")))
          ;; A terminal never opens a session only to learn its shell.
          (should asked)))
      (setf (remote-target-shell target) nil)
      ;; Asking would have to connect, or failed: the target starts its own
      ;; login shell inside the terminal's session instead.
      (dolist (answer (list (lambda (&rest _) nil)
                            (lambda (&rest _)
                              (error "injected shell probe failure"))))
        (cl-letf (((symbol-function 'remote-path-login-shell) answer)
                  ((symbol-function 'remote-executable-find)
                   (lambda (&rest _) (error "Must not guess a shell"))))
          (should
           (equal (remote-terminal-command workspace "default" t)
                  deferred))))
      ;; Without a probe only known facts are used.
      (cl-letf (((symbol-function 'remote-path-login-shell)
                 (lambda (&rest _) (error "Must not ask"))))
        (should (equal (remote-terminal-command workspace "default")
                       deferred))
        (setf (remote-target-shell target) "/bin/bash")
        (should (equal (remote-terminal-command workspace "default")
                       '("/bin/bash" "-l")))))))

(ert-deftest remote-terminal-deferred-shell-starts-the-account-shell ()
  "The deferred command really execs `$SHELL' with the profile arguments."
  (let ((process-environment
         (cons "SHELL=/bin/echo" process-environment)))
    (with-temp-buffer
      (should
       (zerop
        (apply #'call-process
               (car remote-terminal--target-shell-command) nil t nil
               (append (cdr remote-terminal--target-shell-command)
                       '("-l" "two words")))))
      (should (equal (buffer-string) "-l two words\n")))))

(ert-deftest remote-terminal-transport-loss-requires-explicit-restart ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "terminal-restart"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (terminal
            (remote-terminal-open
             workspace
             :name "restart"
             :shell "/bin/sh"
             :arguments '("-c" "sleep 30")))
           replacement)
      (unwind-protect
          (progn
            (remote-workspace--mark-terminals-disconnected
             workspace 'injected-loss)
            (should
             (eq (remote-terminal-state terminal) 'disconnected))
            (should (memq terminal (remote-terminal-list workspace)))
            (setq replacement (remote-terminal-restart terminal))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should (remote-terminal-p replacement))
            (should (eq (remote-terminal-state replacement) 'open))
            (should-not (eq terminal replacement)))
        (remote-workspace-close workspace)))))

(ert-deftest remote-routed-process-preserves-invocation-directory ()
  (remote-framework-test-with-registry
    (let* ((root (make-temp-file "remote-process-cwd-" t))
           (child (expand-file-name "child/" root))
           (logical-root
            (remote-canonicalize-file-name
             (file-name-as-directory root)))
           (logical-child
            (remote-canonicalize-file-name
             (file-name-as-directory child)))
           (context
            (remote-context-create
             :target-id "local"
             :localname (file-name-as-directory root)
             :workspace-id "cwd-test"
             :workspace-root logical-root))
           (buffer (generate-new-buffer " *remote-process-cwd*"))
           process)
      (unwind-protect
          (progn
            (make-directory child)
            (setq process
                  (remote-make-process
                   :name "remote-process-cwd"
                   :buffer buffer
                   :command '("/bin/pwd")
                   :remote-context context
                   :remote-directory logical-child
                   :noquery t))
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (should
             (equal
              (car
               (split-string
                (with-current-buffer buffer (buffer-string))
                "\n" t))
              (directory-file-name child))))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-directory root t)))))

(ert-deftest remote-terminal-adopts-native-frontend-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "frontend"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (buffer (generate-new-buffer " *remote-terminal-frontend*"))
           (process
            (make-process
             :name "remote-terminal-frontend"
             :buffer buffer
             :command '("/bin/sh" "-c" "sleep 30")
             :noquery t))
           terminal)
      (unwind-protect
          (progn
            (setq terminal
                  (remote-terminal-adopt
                   workspace buffer
                   :process process
                   :name "frontend"
                   :profile "default"
                   :metadata '(:frontend test)))
            (should (eq
                     (buffer-local-value
                      'remote-terminal-instance buffer)
                     terminal))
            (should (eq (process-get process 'remote-terminal)
                        terminal))
            (should (= (length (remote-terminal-list workspace)) 1))
            (should (= (length
                        (remote-workspace-resources workspace))
                       1))
            (kill-buffer buffer)
            (should-not (process-live-p process))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should-not (remote-terminal-list workspace))
            (should-not (remote-workspace-resources workspace)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-explicitly-restarts-native-frontend ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "frontend-restart"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (buffer
            (generate-new-buffer
             " *remote-terminal-frontend-restart*"))
           (process
            (make-process
             :name "remote-terminal-frontend-restart"
             :buffer buffer
             :command '("/bin/sh" "-c" "sleep 30")
             :noquery t))
           restarted-with
           (terminal
            (remote-terminal-adopt
             workspace buffer
             :process process
             :name "frontend-restart"
             :profile "default"
             :metadata
             (list
              :frontend 'test
              :restart-function
              (lambda (old owner)
                (setq restarted-with (list old owner))
                'replacement)))))
      (unwind-protect
          (progn
            (remote-terminal-mark-disconnected
             terminal 'injected-loss)
            (should (eq
                     (remote-terminal-state terminal)
                     'disconnected))
            (should-not (remote-terminal-process terminal))
            (should (buffer-live-p buffer))
            (should
             (eq (remote-terminal-restart terminal)
                 'replacement))
            (should (equal restarted-with
                           (list terminal workspace)))
            (should-not (buffer-live-p buffer))
            (should (eq
                     (remote-terminal-state terminal)
                     'closed)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(defun remote-framework-test--frontend-terminal (workspace name &rest metadata)
  "Adopt a routed process whose frontend kills its buffer on exit.
Return (TERMINAL BUFFER PROCESS).  The sentinel mirrors a native frontend
such as Ghostel: it removes the buffer unless `remote-framework-test--keep'
is set in it."
  (let* ((buffer (generate-new-buffer (format " *%s*" name)))
         (process
          (remote-make-process
           :name name
           :buffer buffer
           :command '("/bin/sh" "-c" "sleep 30")
           :noquery t
           :remote-adapter "process"
           :remote-context (remote-workspace-context workspace)
           :sentinel
           (lambda (finished _event)
             (let ((owner (process-buffer finished)))
               (when (and (buffer-live-p owner)
                          (not (buffer-local-value
                                'remote-framework-test--keep owner)))
                 (kill-buffer owner)))))))
    (list (remote-terminal-adopt
           workspace buffer :process process :name name
           :metadata (append (list :frontend 'test) metadata))
          buffer process)))

(defvar-local remote-framework-test--keep nil)

(ert-deftest remote-terminal-disconnect-preserves-frontend-buffer ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "frontend-loss"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (calls 0)
           (frontend
            (remote-framework-test--frontend-terminal
             workspace "frontend-loss"
             :disconnect-function
             (lambda (_terminal _reason)
               (cl-incf calls)
               (setq-local remote-framework-test--keep t))))
           (terminal (nth 0 frontend))
           (buffer (nth 1 frontend))
           (process (nth 2 frontend)))
      (unwind-protect
          (progn
            (remote-workspace--mark-terminals-disconnected
             workspace 'injected-loss)
            (should-not (process-live-p process))
            (should (buffer-live-p buffer))
            (should (eq (remote-terminal-state terminal) 'disconnected))
            (should (memq terminal (remote-terminal-list workspace)))
            ;; A reconnect marks the same terminal again.
            (remote-workspace--mark-terminals-disconnected
             workspace 'workspace-reconnect)
            (should (= calls 1))
            (should
             (= 1 (with-current-buffer buffer
                    (count-matches "transport disconnected"
                                   (point-min) (point-max))))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-lost-process-preserves-frontend-buffer ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "frontend-exit"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (frontend
            (remote-framework-test--frontend-terminal
             workspace "frontend-exit"
             :disconnect-function
             (lambda (_terminal _reason)
               (setq-local remote-framework-test--keep t))))
           (terminal (nth 0 frontend))
           (buffer (nth 1 frontend))
           (process (nth 2 frontend)))
      (unwind-protect
          (progn
            ;; The process dies underneath the frontend, as a lost SSH PTY does.
            (signal-process process 'SIGKILL)
            (let ((deadline (+ (float-time) 5)))
              (while (and (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (buffer-live-p buffer))
            (should (eq (remote-terminal-state terminal) 'disconnected))
            (should-not (remote-terminal-process terminal))
            (should (memq terminal (remote-terminal-list workspace))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-kill-buffer-is-not-a-disconnect ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "frontend-kill"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (calls 0)
           (frontend
            (remote-framework-test--frontend-terminal
             workspace "frontend-kill"
             :disconnect-function
             (lambda (_terminal _reason) (cl-incf calls))))
           (terminal (nth 0 frontend))
           (buffer (nth 1 frontend))
           (process (nth 2 frontend)))
      (unwind-protect
          (progn
            (kill-buffer buffer)
            (should-not (process-live-p process))
            (should (= calls 0))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should-not (remote-terminal-list workspace))
            (should-not (remote-workspace-resources workspace)))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-unrouted-frontend-exit-is-not-transport-loss ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "frontend-unrouted"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (buffer (generate-new-buffer " *remote-terminal-unrouted*"))
           ;; A frontend-owned handle that the framework did not route, like
           ;; the event pipe of a native PTY.
           (process
            (make-process
             :name "remote-terminal-unrouted" :buffer buffer
             :command '("/bin/sh" "-c" "sleep 30") :noquery t))
           (terminal
            (remote-terminal-adopt
             workspace buffer :process process :name "unrouted"
             :metadata '(:frontend test))))
      (unwind-protect
          (progn
            (signal-process process 'SIGKILL)
            (let ((deadline (+ (float-time) 5)))
              (while (and (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should-not (remote-terminal-list workspace)))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-failed-restart-keeps-the-old-terminal ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local" :localname "/tmp/"
             :workspace-id "frontend-restart-failure"
             :workspace-root "/fs:local:/tmp/"))
           (workspace (remote-workspace-open context :connect nil))
           (fail t)
           seen-name
           (frontend
            (remote-framework-test--frontend-terminal
             workspace "restart-failure"
             :disconnect-function
             (lambda (_terminal _reason)
               (setq-local remote-framework-test--keep t))
             :restart-function
             (lambda (old _owner)
               ;; The frontend recreates its buffer under the original name.
               (setq seen-name (get-buffer (remote-terminal-name old)))
               (if fail
                   (error "Injected restart failure")
                 'replacement))))
           (terminal (nth 0 frontend))
           (buffer (nth 1 frontend))
           (name (buffer-name buffer)))
      (unwind-protect
          (progn
            (setf (remote-terminal-name terminal) name)
            (remote-terminal-mark-disconnected terminal 'injected-loss)
            (should-error (remote-terminal-restart terminal))
            (should-not seen-name)
            (should (buffer-live-p buffer))
            (should (equal (buffer-name buffer) name))
            (should (eq (remote-terminal-state terminal) 'disconnected))
            (should (memq terminal (remote-terminal-list workspace)))
            (setq fail nil)
            (should (eq (remote-terminal-restart terminal) 'replacement))
            (should-not (buffer-live-p buffer))
            (should (eq (remote-terminal-state terminal) 'closed)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-backend-registration-keeps-link-plugin-compatibility ()
  (remote-framework-test-with-registry
    (remote-register-backend
     "memory"
     :capabilities '(file-read)
     :project
     (lambda (file-name _pipeline _route)
       (concat "/projected" (remote-fs-localname file-name))))
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline "lab" "memory" "memory")
    (remote-register-adapter
     "test" :capabilities '(file-read)
     :preferences '((default . ("memory"))))
    (let* ((context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-root "/fs:lab:/work/"))
           (route (remote-resolve "test" 'file-read context)))
      (should (remote-get-backend "memory"))
      (should (remote-get-link-plugin "memory"))
      (should
       (equal
        (remote-project-file-name "/fs:lab:/work/a.el" route)
        "/projected/work/a.el")))))

(ert-deftest remote-backend-prepares-logical-and-physical-execution ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/work/a.el"
             :workspace-root "/fs:local:/tmp/work/"))
           (route (remote-resolve "exec" 'process-sync context))
           (execution
            (remote-backend-prepare-execution
             route context '("git" "status") '(("LANG" . "C")))))
      (should
       (equal (remote-backend-execution-logical-directory execution)
              "/fs:local:/tmp/work/"))
      (should
       (equal (remote-backend-execution-physical-directory execution)
              "/tmp/work/"))
      (should
       (equal (remote-backend-execution-command execution)
              '("git" "status")))
      (should
       (eq (plist-get
            (remote-backend-execution-metadata execution)
            :program-form)
           'search)))))

(ert-deftest remote-stdio-bridge-dispatches-through-the-selected-backend ()
  "The public process layer must not special-case local, SSH, or backend IDs."
  (remote-framework-test-with-registry
    (let (seen)
      (remote-register-backend
       "test-bridge"
       :capabilities '(process-async)
       :project
       (lambda (_file-name _pipeline _route)
         temporary-file-directory)
       :stdio-bridge
       (lambda (execution)
         (setq seen execution)
         (list
          "/test/bridge"
          (remote-route-target-id
           (remote-backend-execution-route execution))
          (car (remote-backend-execution-command execution)))))
      (remote-register-target "lab" :trusted t)
      (remote-register-pipeline
       "lab" "bridge" "test-bridge"
       :config '(:transport "direct"))
      (let* ((context
              (remote-context-create
               :target-id "lab"
               :localname "/work/a.el"
               :workspace-root "/fs:lab:/work/"))
             (command
              (remote-local-bridge-command
               "language-server"
               :context context
               :link "lab/bridge")))
        (should
         (equal command
                '("/test/bridge" "lab" "language-server")))
        (should (remote-backend-execution-p seen))
        (should
         (equal
          (remote-backend-execution-logical-directory seen)
          "/fs:lab:/work/"))))))

(ert-deftest remote-rpc-backend-declares-absolute-spawn-contract ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "ssh" "tramp-rpc"
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp-rpc"
             :capability 'process-async
             :adapter-id "process"))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-root "/fs:lab:/work/"))
           (execution
            (remote-backend-prepare-execution
             route context '("direnv" "export" "json") nil)))
      (should
       (eq (plist-get
            (remote-backend-execution-metadata execution)
            :program-form)
           'absolute))
      (should
       (plist-get
        (remote-backend-execution-metadata execution)
        :require-absolute-program)))))

(ert-deftest remote-tramp-stream-keeps-target-host-above-the-relay ()
  "TLS/SNI sees the target host while only the socket connect sees loopback."
  (remote-framework-test-with-registry
    (let* ((route
            (remote-route-create
             :target-id "lab"
             :pipeline-id "lab/ssh"
             :backend-id "tramp"
             :capability 'network-client
             :adapter-id "network"))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (process
            (make-pipe-process
             :name "remote-stream-return-list" :noquery t))
           (forward
            (remote-forward-create
             :backend-id "tramp"
             :route route :context context
             :local-endpoint
             '(:host "127.0.0.1" :port 49152)
             :remote-endpoint
             '(:host "db.internal" :port 443)
             :state 'open))
           high-level
           low-level)
      (unwind-protect
          (cl-letf
              (((symbol-function 'gnutls-available-p)
                (lambda () t))
               ((symbol-function 'remote-backend-tramp-forward)
                (lambda (&rest _arguments) forward))
               ((symbol-function 'make-network-process)
                (lambda (&rest arguments)
                  (setq low-level arguments)
                  process))
               ((symbol-function 'open-network-stream)
                (lambda (name buffer host service &rest parameters)
                  (setq high-level
                        (list name buffer host service parameters))
                  (list
                   (make-network-process
                    :name name :buffer buffer
                    :host host :service service)
                   :greeting "hello"
                   :type 'tls))))
            (let ((result
                   (remote-backend-tramp-stream
                    route context "db" nil "db.internal" 443
                    '(:type tls :return-list t))))
              (should (eq (car result) process))
              (should (equal (nth 2 high-level) "db.internal"))
              (should (= (nth 3 high-level) 443))
              (should (eq (plist-get (nth 4 high-level) :type) 'tls))
              (should
               (equal (plist-get low-level :host) "127.0.0.1"))
              (should (= (plist-get low-level :service) 49152))
              (should (eq (process-get process 'remote-forward)
                          forward))))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest remote-open-network-stream-preserves-native-return-list ()
  (remote-framework-test-with-registry
    (let ((process
           (make-pipe-process
            :name "remote-stream-list-contract" :noquery t)))
      (unwind-protect
          (progn
            (remote-register-backend
             "stream-list"
             :capabilities '(network-client)
             :open-network-stream
             (lambda (_route _context _name _buffer _host _service parameters)
               (should (plist-get parameters :return-list))
               (list process :greeting "ready" :type 'plain)))
            (remote-register-target "lab" :trusted t)
            (remote-register-pipeline
             "lab" "stream" "stream-list"
             :config '(:transport "direct"))
            (let* ((context
                    (remote-context-create
                     :target-id "lab"
                     :localname "/work/"
                     :workspace-root "/fs:lab:/work/"))
                   (result
                    (remote-open-network-stream
                     "stream" nil "service.internal" 8080
                     :return-list t
                     :remote-context context
                     :remote-pipeline "lab/stream")))
              (should
               (equal (cdr result)
                      '(:greeting "ready" :type plain)))
              (should (eq (car result) process))
              (should (remote-channel-of process))
              (should
               (equal
                (remote-channel-endpoint process 'remote)
                '(:host "service.internal" :port 8080)))))
        (when (process-live-p process)
          (remote-close-channel process))))))

(ert-deftest remote-channel-never-falls-back-to-the-client-machine ()
  (remote-framework-test-with-registry
    (remote-register-backend
     "file-only"
     :capabilities '(file-read)
     :project (lambda (file _pipeline _route) file))
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline
     "lab" "isolated" "file-only")
    (let ((context
           (remote-context-create
            :target-id "lab"
            :localname "/work/"
            :workspace-root "/fs:lab:/work/")))
      (should-error
       (remote-resolve "network" 'network-client context))
      (should-error
       (remote-open-network-stream
        "unsafe-fallback" nil "127.0.0.1" 9
        :remote-context context)))))

(ert-deftest remote-ssh-forward-command-respects-pipeline-hops ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "jumped" "tramp"
             :stages
             '((:id "jump" :transport "ssh"
                :config (:host "edge" :user "ops" :port 2222))
               (:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'port-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote)
                   "/usr/bin/ssh")))
        (should
         (equal
          (remote-backend-tramp--ssh-forward-command
           route "127.0.0.1" 49152 "127.0.0.1" 3000)
          '("/usr/bin/ssh" "-N" "-T" "-S" "none"
            "-o" "ExitOnForwardFailure=yes"
            "-v"
            "-o" "ServerAliveInterval=15"
            "-o" "ServerAliveCountMax=3"
            "-o" "ConnectTimeout=8"
            "-o" "ConnectionAttempts=1"
            "-J" "ops@edge:2222"
            "-L" "127.0.0.1:49152:127.0.0.1:3000"
            "dev@lab")))))))

(ert-deftest remote-ssh-forward-command-uses-target-keepalive-override ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "ssh" "tramp"
             :config '(:host "lab" :ssh-options
                       ("ServerAliveInterval=4" "ServerAliveCountMax=2"))))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'port-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote)
                   "/usr/bin/ssh")))
        (let ((command
               (remote-backend-tramp--ssh-forward-command
                route "127.0.0.1" 49152 "127.0.0.1" 3000)))
          (should (member "ServerAliveInterval=4" command))
          (should (member "ServerAliveCountMax=2" command))
          (should-not (member "ServerAliveInterval=15" command))
          (should-not (member "ServerAliveCountMax=3" command)))))))

(ert-deftest remote-ssh-forward-command-preserves-managed-control-path ()
  "A forward must use the same SSH master as its workspace session."
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "ssh" "tramp" :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'port-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote) "/usr/bin/ssh"))
                ((symbol-function 'remote-transport-ssh-control-options)
                 (lambda (&optional _runtime)
                   '("ControlMaster=auto"
                     "ControlPath=/tmp/noema-managed-control"))))
        (let ((command
               (remote-backend-tramp--ssh-forward-command
                route "127.0.0.1" 49152 "127.0.0.1" 3000)))
          (should (member "ControlPath=/tmp/noema-managed-control"
                          command))
          (should-not (member "-S" command)))))))

(ert-deftest remote-ssh-mux-forward-recognizes-master-session ()
  "OpenSSH mux clients omit the ordinary local-listener debug line."
  (with-temp-buffer
    (insert "debug1: Requesting forwarding of local forward 127.0.0.1:50001 -> 127.0.0.1:3000\r\n")
    (should-not
     (remote-backend-tramp--mux-forward-session-p (current-buffer)))
    (insert "debug1: mux_client_request_session: master session id: 5\r\n")
    (should
     (remote-backend-tramp--mux-forward-session-p (current-buffer)))))

(ert-deftest remote-ssh-custom-config-reaches-every-client-command ()
  (remote-framework-test-with-registry
    (let* ((config-file "/tmp/remote ssh config")
           (_target (remote-register-target "custom" :trusted t))
           (pipeline
            (remote-register-pipeline
             "custom" "ssh" '("tramp-rpc" "tramp")
             :config (list :host "custom" :ssh-config-file config-file)))
           (route
            (remote-route-create
             :target-id "custom"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp-rpc"
             :capability 'process-async
             :adapter-id "process"))
           (execution
            (remote-backend-execution-create
             :route route
             :physical-directory "/rpc:custom:/tmp/"))
           (plan
            (remote-backend-tramp-handler-process-plan
             execution '(:name "custom" :command ("true")) nil))
           copied)
      (cl-labels
          ((has-config (arguments)
             (equal (cadr (member "-F" arguments)) config-file)))
        (should
         (equal (remote-backend-tramp--ssh-config-args pipeline)
                (list "-F" config-file)))
        (should
         (equal (car (remote-backend-tramp--method-login-args
                      "ssh" nil (list "-F" config-file)))
                (list "-F" config-file)))
        (should
         (has-config
          (remote-backend-tramp-direct-async-command
           route '("true") nil "/tmp/")))
        (let ((login
               (remote-backend-tramp-ssh-client-command
                route :tty t)))
          (should (member "-tt" login))
          (should (has-config login)))
        (should
         (has-config
          (remote-backend-tramp--ssh-forward-command
           route "127.0.0.1" 50001 "127.0.0.1" 22)))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program _infile _destination _display
                                    &rest arguments)
                     (setq copied (cons program arguments))
                     0)))
          (should
           (= 0 (remote-backend-tramp-direct-copy-file
                 route "/tmp/client" "/tmp/target")))
          (should (has-config copied)))
        (should
         (has-config
          (remote-transport--ssh-control-command
           (remote-ssh-control-create
            :path "/tmp/control" :destination "custom"
            :config-file config-file)
           "check")))
        (let ((tramp-rpc-ssh-args nil)
              observed)
          (should
           (eq
            (funcall
             (remote-backend-process-plan-around-start plan)
             (lambda ()
               (setq observed tramp-rpc-ssh-args)
               'started))
            'started))
          (should (has-config observed)))))))

(ert-deftest remote-board-ssh-failure-state-clears-after-connection ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "ssh" "tramp" :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'file-read :adapter-id "emacs-file"))
           (connection
            (remote-connection-create
             :target-id "lab" :state 'failed
             :error '(error "Permission denied (publickey)."))))
      (remote-board--record-connection-failure
       connection route (remote-connection-error connection))
      (should
       (equal (remote-board--target-state "lab" nil nil)
              "auth required"))
      (remote-board--clear-connection-failure connection route)
      (should (equal (remote-board--target-state "lab" nil nil)
                     "idle")))))

(ert-deftest remote-board-ssh-diagnostic-captures-authentication-failure ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (name "*Remote SSH lab*")
           process)
      (unwind-protect
          (cl-letf
              (((symbol-function 'remote-board--ssh-client-command)
                (lambda (&rest _arguments)
                  '("sh" "-c"
                    "printf 'Permission denied (publickey).\\n' >&2; exit 255"))))
            (setq process (remote-board-ssh-diagnose target))
            (let ((deadline (+ (float-time) 3)))
              (while (and
                      (eq (plist-get
                           (gethash "lab" remote-board-ssh-statuses)
                           :state)
                          'checking)
                      (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should
             (eq (plist-get (gethash "lab" remote-board-ssh-statuses)
                            :state)
                 'authentication))
            (with-current-buffer name
              (should
               (string-match-p "Permission denied"
                               (buffer-string)))))
        (when (process-live-p process)
          (delete-process process))
        (when-let* ((buffer (get-buffer name)))
          (kill-buffer buffer))))))

(ert-deftest remote-board-ssh-login-uses-client-home-and-argv ()
  (remote-framework-test-with-registry
    (require 'term)
    (let* ((target (remote-register-target "lab" :trusted t))
           (buffer (generate-new-buffer " *remote-test-ssh-login*"))
           captured)
      (unwind-protect
          (cl-letf
              (((symbol-function 'remote-board--ssh-client-command)
                (lambda (&rest _arguments)
                  '("/usr/bin/ssh" "-tt" "-F" "/tmp/ssh config" "lab")))
               ((symbol-function 'remote-client-process-environment)
                (lambda () '("HOME=/Users/client" "PATH=/usr/bin")))
               ((symbol-function 'remote-client-exec-path)
                (lambda () '("/usr/bin")))
               ((symbol-function 'make-term)
                (lambda (&rest arguments)
                  (setq captured
                        (list arguments (getenv "HOME") default-directory))
                  buffer))
               ((symbol-function 'term-char-mode) #'ignore)
               ((symbol-function 'pop-to-buffer) #'ignore))
            (let ((process-environment '("HOME=/home/target")))
              (should (eq (remote-board-ssh-login target) buffer)))
            (should
             (equal (car captured)
                    '("Remote SSH Login lab" "/usr/bin/ssh" nil
                      "-tt" "-F" "/tmp/ssh config" "lab")))
            (should (equal (cadr captured) "/Users/client"))
            (should (equal (nth 2 captured)
                           temporary-file-directory)))
        (kill-buffer buffer)))))

(ert-deftest remote-ssh-custom-config-survives-an-existing-tramp-cache ()
  (let* ((vector (tramp-dissect-file-name
                  "/ssh:remote-config-cache-test:/" nil))
         (login-args
          (remote-backend-tramp--method-login-args
           "ssh" nil '("-F" "/tmp/custom-ssh-config"))))
    (unwind-protect
        (progn
          ;; TRAMP has already initialized this connection's cache before
          ;; the custom config is selected.
          (tramp-get-method-parameter vector 'tramp-login-args)
          (remote-backend-tramp--seed-login-args
           "/ssh:remote-config-cache-test:" login-args)
          (should
           (equal
            (tramp-get-method-parameter vector 'tramp-login-args)
            login-args))
          (let ((key
                 (seq-find
                  (lambda (item) (equal item "login-args"))
                  (hash-table-keys
                   (tramp-get-hash-table
                    (tramp-file-name-unify vector))))))
            (should (get-text-property 0 'tramp-default key))))
      (tramp-flush-connection-properties vector))))

(ert-deftest remote-ssh-client-process-plan-keeps-client-home ()
  (remote-framework-test-with-registry
    (remote-register-target "custom" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "custom" "ssh" "tramp"
             :config '(:host "custom"
                       :ssh-config-file "/tmp/custom-ssh-config")))
           (route
            (remote-route-create
             :target-id "custom"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'process-async
             :adapter-id "process"))
           (execution
            (remote-backend-execution-create
             :route route :command '("sh" "-c" "true")
             :physical-directory "/ssh:custom:/tmp/"
             :context
             (remote-context-create
              :target-id "custom" :localname "/tmp/"))))
      (let* ((remote--client-process-environment
              '("HOME=/client" "PATH=/usr/bin"))
             (remote--client-exec-path '("/usr/bin"))
             (process-environment '("HOME=/target" "PATH=/remote/bin"))
             (plan
              (remote-backend-tramp-prepare-process
               execution
               '(:name "custom" :command ("sh" "-c" "true")
                 :connection-type pipe)
               '(("HOME" . "/target")))))
        (should (member "HOME=/client"
                        (remote-backend-process-plan-process-environment
                         plan)))
        (should-not (member "HOME=/target"
                            (remote-backend-process-plan-process-environment
                             plan)))
        (should (equal
                 (remote-backend-process-plan-exec-path plan)
                 '("/usr/bin")))
        (should
         (string-match-p
          "/target"
          (car (last
                (plist-get
                 (remote-backend-process-plan-arguments plan)
                 :command)))))))))

(ert-deftest remote-ssh-reverse-forward-command-respects-pipeline-hops ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "jumped" "tramp"
             :stages
             '((:id "jump" :transport "ssh"
                :config (:host "edge" :user "ops" :port 2222))
               (:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'reverse-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote)
                   "/usr/bin/ssh")))
        (should
         (equal
          (remote-backend-tramp--ssh-forward-command
           route "127.0.0.1" 3000 "127.0.0.1" 49152
           'reverse)
          '("/usr/bin/ssh" "-N" "-T" "-S" "none"
            "-o" "ExitOnForwardFailure=yes"
            "-v"
            "-o" "ServerAliveInterval=15"
            "-o" "ServerAliveCountMax=3"
            "-o" "ConnectTimeout=8"
            "-o" "ConnectionAttempts=1"
            "-J" "ops@edge:2222"
            "-R" "127.0.0.1:49152:127.0.0.1:3000"
            "dev@lab")))))))

(provide 'remote-framework-tests)
;;; remote-framework-tests.el ends here
