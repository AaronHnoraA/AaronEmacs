;;; terminal-remote-tests.el --- Terminal placement and client boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'term/portal/config)
(unless (featurep 'remote-framework-tests)
  (load (expand-file-name "test/remote-framework-tests.el" user-emacs-directory) nil t))

(defmacro terminal-remote-test-with-registry (&rest body)
  "Isolate framework facts and unrelated workspace consumer hooks for BODY."
  (declare (indent 0))
  `(let ((remote-workspace-open-hook nil) (remote-workspace-close-hook nil))
     (remote-framework-test-with-registry ,@body)))

(defun terminal-remote-test--wait (predicate)
  "Wait briefly for terminal PREDICATE."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(ert-deftest terminal-remote-eshell-explicit-disconnect-drains-retained-queue ()
  (require 'term/shell/eshell)
  (terminal-remote-test-with-registry
    (let* ((native (file-name-as-directory (make-temp-file "eshell-disconnect-" t)))
           (target "eshell-disconnect-test")
           (root native)
           buffer results)
      (remote-register-target target :trusted t)
      (remote-register-pipeline target "native" '("native"))
      (unwind-protect
          (progn
            (setq buffer (+eshell-buffer root "disconnect"))
            (with-current-buffer buffer
              ;; Isolate target teardown to this owned shell; native fixture
              ;; files stay on the client and no fake filesystem is needed.
              (setq-local remote-buffer-target-id target)
              (setq-local kill-buffer-query-functions (list (lambda () nil))))
            (+eshell-run "/bin/echo never-dispatch" :directory root :name "disconnect"
                         :callback (lambda (result) (push result results)))
            (+eshell-run "/bin/echo never-dispatch-either" :directory root :name "disconnect"
                         :callback (lambda (result) (push result results)))
            (remote-workspace-disconnect-target target 'test)
            (should (buffer-live-p buffer))
            (should (buffer-local-value 'remote-buffer-disconnected-p buffer))
            (should (= (length results) 2))
            (should (cl-every (lambda (result) (and (eq (plist-get result :reason) 'disconnected)
                                                   (null (plist-get result :status)))) results))
            (with-current-buffer buffer
              (should-not +eshell--queue)
              (should-not +eshell--current)
              (should-not +eshell--dispatch-timer)
              (should-not +eshell--command-timer))
            (should-error (+eshell-run "/bin/echo still-disconnected" :directory root :name "disconnect")
                          :type 'user-error))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
        (remote-workspace-disconnect-target target 'test-cleanup)
        (delete-directory native t)))))

(ert-deftest terminal-remote-eshell-disconnect-stops-running-work-and-retains-draft ()
  (require 'term/shell/eshell)
  (terminal-remote-test-with-registry
    (let* ((root (file-name-as-directory (make-temp-file "eshell-running-disconnect-" t)))
           (target "eshell-running-disconnect")
           buffer peer process results peer-result)
      (remote-register-target target :trusted t)
      (remote-register-pipeline target "native" '("native"))
      (unwind-protect
          (progn
            (setq buffer (+eshell-buffer root "running-disconnect")
                  peer (+eshell-buffer root "unaffected-peer"))
            (with-current-buffer buffer
              (goto-char (point-max)) (insert "unsent draft")
              (setq-local kill-buffer-query-functions (list (lambda () nil))))
            (+eshell-run "/bin/sleep 30" :directory root :name "running-disconnect"
                         :callback (lambda (result) (push result results)))
            (+eshell-run "/bin/echo never" :directory root :name "running-disconnect"
                         :callback (lambda (result) (push result results)))
            (terminal-remote-test--wait (lambda () (buffer-local-value 'eshell-foreground-command buffer)))
            (setq process (get-buffer-process buffer))
            (should (process-live-p process))
            (with-current-buffer buffer (setq-local remote-buffer-target-id target))
            (remote-workspace-disconnect-target target 'test)
            (should (buffer-live-p buffer))
            (should-not (process-live-p process))
            (should (= (length results) 2))
            (should (cl-every (lambda (result) (eq (plist-get result :reason) 'disconnected)) results))
            (terminal-remote-test--wait
             (lambda () (with-current-buffer buffer
                          (and (null +eshell--dispatch-timer)
                               (null +eshell--command-timer)
                               (equal (+eshell--input) "unsent draft")))))
            (should (= (length results) 2))
            (+eshell-run "/bin/echo peer-alive" :directory root :name "unaffected-peer"
                         :callback (lambda (result) (setq peer-result result)))
            (terminal-remote-test--wait (lambda () peer-result))
            (should (equal (string-trim (plist-get peer-result :output)) "peer-alive"))
            ;; Merely requesting the buffer leaves it disconnected.  A
            ;; manual submission is the explicit start which enables APIs.
            (should (eq buffer (+eshell-buffer root "running-disconnect")))
            (should (buffer-local-value 'remote-buffer-disconnected-p buffer))
            (with-current-buffer buffer
              (delete-region eshell-last-output-end (point-max))
              (insert "*echo manual-restart")
              (eshell-send-input))
            (should-not (buffer-local-value 'remote-buffer-disconnected-p buffer))
            (let ((result (+eshell-run-sync "/bin/echo api-after-manual" :directory root
                                            :name "running-disconnect" :timeout 5)))
              (should (equal (string-trim (plist-get result :output)) "api-after-manual"))))
        (dolist (candidate (list buffer peer))
          (when (buffer-live-p candidate)
            (with-current-buffer candidate
              (let ((kill-buffer-query-functions nil)) (kill-buffer candidate)))))
        (remote-workspace-disconnect-target target 'test-cleanup)
        (delete-directory root t)))))

(ert-deftest terminal-remote-eshell-disconnect-during-cwd-change-cannot-submit-the-popped-item ()
  (require 'term/shell/eshell)
  (terminal-remote-test-with-registry
    (let* ((root (file-name-as-directory (make-temp-file "eshell-cwd-disconnect-" t)))
           (child (concat root "child/"))
           (target "eshell-cwd-disconnect")
           (original (symbol-function '+eshell--sync-directory))
           buffer results)
      (remote-register-target target :trusted t)
      (remote-register-pipeline target "native" '("native"))
      (make-directory child)
      (unwind-protect
          (progn
            (setq buffer (+eshell-buffer root "cwd-disconnect"))
            (with-current-buffer buffer
              (setq-local remote-buffer-target-id target
                          kill-buffer-query-functions (list (lambda () nil))))
            (cl-letf (((symbol-function '+eshell--sync-directory)
                       (lambda (shell directory)
                         (funcall original shell directory)
                         (when (and +eshell--dispatching (equal directory child))
                           (remote-workspace-disconnect-target target 'test)))))
              (dolist (entry (list (cons "*echo first" root) (cons "*echo never-second" child)
                                   (cons "*echo never-third" root)))
                (+eshell-run (car entry) :directory (cdr entry) :name "cwd-disconnect"
                             :callback (lambda (result) (setq results (append results (list result))))))
              (terminal-remote-test--wait (lambda () (= 3 (length results)))))
            (should (= (plist-get (car results) :status) 0))
            (should (cl-every (lambda (result) (eq (plist-get result :reason) 'disconnected)) (cdr results)))
            (with-current-buffer buffer
              (should-not +eshell--current)
              (should-not +eshell--queue)
              (should-not +eshell--dispatch-timer)
              (should-not +eshell--command-timer)
              (should-not (string-match-p "never-second\\|never-third" (buffer-string)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
        (remote-workspace-disconnect-target target 'test-cleanup)
        (delete-directory root t)))))

(ert-deftest terminal-remote-cwd-survives-parent-workspace-and-restart ()
  (terminal-remote-test-with-registry
    (let* ((root (file-name-as-directory (make-temp-file "terminal-workspace-" t)))
           (child (concat root "nested directory/"))
           (workspace (remote-workspace-open (remote-make-file-name "local" root)
                                             :connect nil))
           terminal)
      (unwind-protect
          (progn
            (make-directory child)
            (cl-letf (((symbol-function 'remote-workspace-refresh-environment)
                       (lambda (&rest _) (ert-fail "Terminal built an environment capsule"))))
              (setq terminal
                    (remote-terminal-open workspace :directory child
                                          :shell "/bin/sh"
                                          :arguments '("-c" "printf 'CWD:%s\\n' \"$PWD\"; read value")))
              (terminal-remote-test--wait
               (lambda () (with-current-buffer (remote-terminal-buffer terminal)
                            (string-match-p (regexp-quote (concat "CWD:" (directory-file-name child)))
                                            (buffer-string)))))
              (should (equal (buffer-local-value 'default-directory (remote-terminal-buffer terminal)) child))
              (remote-terminal-mark-disconnected terminal 'test)
              (setq terminal (remote-terminal-restart terminal))
              (terminal-remote-test--wait
               (lambda () (with-current-buffer (remote-terminal-buffer terminal)
                            (string-match-p (regexp-quote (concat "CWD:" (directory-file-name child)))
                                            (buffer-string)))))))
        (remote-workspace-close workspace)
        (delete-directory root t)))))

(ert-deftest terminal-remote-explicit-empty-argv-does-not-add-login-arguments ()
  (terminal-remote-test-with-registry
    (let* ((workspace (remote-workspace-open "/fs:local:/tmp/" :connect nil))
           (terminal (remote-terminal-open workspace :shell "/bin/pwd" :arguments nil)))
      (unwind-protect
          (let ((process (remote-terminal-process terminal)))
            (terminal-remote-test--wait (lambda () (not (process-live-p process))))
            (should (= (process-exit-status process) 0)))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-client-launch-carries-cwd-environment-and-real-pty-route ()
  (terminal-remote-test-with-registry
    (let* ((root (file-name-as-directory (make-temp-file "terminal-native-plan-" t)))
           (child (concat root "child with spaces/"))
           (workspace (remote-workspace-open (remote-make-file-name "local" root) :connect nil))
           (output (generate-new-buffer " *terminal-native-plan*"))
           (remote--client-process-environment '("PATH=/usr/bin:/bin" "PLAN_VALUE=old"))
           (remote--client-exec-path '("/usr/bin" "/bin")) process)
      (unwind-protect
          (progn
            (make-directory child)
            (let ((launch
                   (cl-letf (((symbol-function 'remote-workspace-refresh-environment)
                              (lambda (&rest _) (ert-fail "Native PTY launch built a capsule"))))
                     (remote-terminal-prepare-client-launch
                      workspace :directory child :environment '(("PLAN_VALUE" . "explicit"))
                      :command '("sh" "-c" "printf '%s|%s' \"$PWD\" \"$PLAN_VALUE\"")))))
              (should (eq (remote-route-capability (plist-get launch :route)) 'pty))
              ;; The host has an old, unrelated environment and cwd.  The
              ;; native argv must carry the complete process plan itself.
              (let ((default-directory temporary-file-directory)
                    (process-environment '("PATH=/wrong/bin" "PLAN_VALUE=wrong")))
                (setq process (make-process :name "terminal-native-plan" :buffer output
                                            :command (plist-get launch :command) :noquery t
                                            :sentinel #'ignore)))
              (terminal-remote-test--wait (lambda () (not (process-live-p process))))
              (should (= (process-exit-status process) 0))
              (should (equal (with-current-buffer output (buffer-string))
                             (concat (directory-file-name child) "|explicit")))))
        (when (and process (process-live-p process)) (delete-process process))
        (kill-buffer output)
        (remote-workspace-close workspace)
        (delete-directory root t)))))

(ert-deftest terminal-remote-native-handle-has-one-owner-and-one-close ()
  (terminal-remote-test-with-registry
    (let* ((workspace (remote-workspace-open "/fs:local:/tmp/" :connect nil))
           (buffer (generate-new-buffer " *terminal-native-handle*"))
           (alive t) (closes 0) sent terminal)
      (unwind-protect
          (progn
            (setq terminal
                  (remote-terminal-adopt
                   workspace buffer
                   :metadata (list :frontend 'test-native
                                   :live-function (lambda (_) alive)
                                   :send-function (lambda (_ string) (setq sent string))
                                   :close-function (lambda (_) (cl-incf closes) (setq alive nil)))))
            (should-not (remote-terminal-process terminal))
            (should (remote-terminal-live-p terminal))
            (remote-terminal-send-string terminal "hello")
            (should (equal sent "hello"))
            (remote-workspace-close workspace)
            (should (= closes 1))
            (should-not (buffer-live-p buffer))
            (should-not (remote-terminal-list))
            (remote-terminal-close terminal)
            (should (= closes 1))
            (should-error (remote-terminal-send-string terminal "late")))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-native-program-exit-does-not-invent-transport-loss ()
  (terminal-remote-test-with-registry
    (let* ((remote-terminal-auto-close-on-exit nil)
           (workspace (remote-workspace-open "/fs:local:/tmp/" :connect nil))
           (buffer (generate-new-buffer " *terminal-native-unknown-exit*"))
           (disconnected 0) terminal)
      (unwind-protect
          (progn
            (setq terminal
                  (remote-terminal-adopt
                   workspace buffer
                   :metadata (list :live-function (lambda (_) t)
                                   :disconnect-function (lambda (_ _reason) (cl-incf disconnected)))))
            (remote-terminal-frontend-exited terminal)
            (remote-terminal-frontend-exited terminal)
            (should (= disconnected 0))
            (should (buffer-live-p buffer))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should (plist-member (remote-terminal-metadata terminal) :exit-status))
            (should-not (plist-get (remote-terminal-metadata terminal) :exit-status))
            (should-not (remote-terminal-list workspace))
            (should-not (remote-terminal-live-p terminal)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-rejects-cwd-on-a-different-target-before-spawning ()
  (terminal-remote-test-with-registry
    (let ((workspace (remote-workspace-open "/fs:local:/tmp/" :connect nil)))
      (unwind-protect
          (cl-letf (((symbol-function 'remote-make-process)
                     (lambda (&rest _) (ert-fail "Mismatched target spawned a terminal"))))
            (should-error (remote-terminal-open workspace :directory "/fs:other:/tmp/")
                          :type 'user-error))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-delayed-view-cleanup-retains-disconnected-history ()
  (terminal-remote-test-with-registry
    (let* ((workspace (remote-workspace-open "/fs:local:/tmp/" :connect nil))
           (buffer (generate-new-buffer " *terminal-disconnected-cleanup*"))
           cleanup terminal)
      (unwind-protect
          (with-current-buffer buffer
            (setq-local major-mode 'portal-terminal-mode
                        portal-view--target "terminal:disconnecting")
            (insert "Retained terminal output\n")
            (setq terminal
                  (remote-terminal-adopt workspace buffer
                                         :metadata (list :live-function (lambda (_) t))))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (setq cleanup (cons function args)))))
              (+portal-close-terminal-buffer-a
               (lambda () (setq portal-view--target nil))))
            (should cleanup)
            (remote-terminal-mark-disconnected terminal 'frontend-exit)
            (apply (car cleanup) (cdr cleanup))
            (should (buffer-live-p buffer))
            (should (eq (remote-terminal-state terminal) 'disconnected))
            (should (string-match-p "Retained terminal output" (buffer-string)))
            (should (memq terminal (remote-terminal-list workspace)))
            (setq cleanup nil)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _) (setq cleanup t))))
              (+portal-close-terminal-buffer-a #'ignore))
            (should-not cleanup))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-context-key-uses-target-instead-of-tramp-method ()
  (terminal-remote-test-with-registry
    (remote-register-target "terminal-lab" :trusted t)
    (remote-register-link "terminal-lab" "ssh" "tramp"
                          :config '(:host "terminal-lab" :user "test"))
    (let ((logical "/fs:terminal-lab:/work/project/")
          (physical "/ssh:test@terminal-lab:/work/project/"))
      (should (equal (+terminal-context-key logical) "terminal-lab"))
      (should (equal (+terminal-context-key physical) (+terminal-context-key logical)))
      (should (equal (+eshell-context-buffer-name physical) (+eshell-context-buffer-name logical)))
      (should (equal (+terminal-context-key "/tmp/") (+terminal-context-key "/fs:local:/tmp/"))))))

(ert-deftest terminal-remote-official-process-boundary-retains-dynamic-environment ()
  (let* ((process-environment (cons "TERMINAL_CALLER=first"
                                    (cons "TERMINAL_CALLER=second" process-environment)))
         (tramp-remote-process-environment
          (cons "TERMINAL_TRAMP=dynamic" tramp-remote-process-environment))
         (tramp-remote-path '("/workspace/bin" "/usr/bin"))
         (default-directory "/fs:terminal-boundary:/work/") captured)
    (cl-letf (((symbol-function 'remote-make-process)
               (lambda (&rest args) (setq captured args))))
      (remote-fs-handle-make-process
       :name "environment" :command '("true")
       :remote-environment '(("TERMINAL_CALLER" . "explicit")))
      (let ((environment (remote--merge-environment-overrides nil (plist-get captured :remote-environment))))
        (should (equal (cdr (assoc "TERMINAL_CALLER" environment)) "explicit"))
        (should (equal (cdr (assoc "TERMINAL_TRAMP" environment)) "dynamic"))
        (should (equal (cdr (assoc "PATH" environment)) "/workspace/bin:/usr/bin"))
        (should-not (assoc "HOME" environment)))
      (remote-fs-handle-start-file-process "environment" nil "true")
      (should (equal (cdr (assoc "TERMINAL_CALLER" (plist-get captured :remote-environment))) "first"))
      (remote-fs-handle-make-process
       :name "environment" :command '("true")
       :remote-environment (remote-environment-create :vars '(("TERMINAL_CALLER" . "capsule"))))
      (should (equal (cdr (assoc "TERMINAL_CALLER" (plist-get captured :remote-environment))) "capsule")))))

(ert-deftest terminal-remote-environment-delta-keeps-values-and-explicit-unsets ()
  (should (equal (remote-fs--environment-delta
                  '("KEEP=base" "VALUE=first=\nline" "VALUE=second" "EXPLICIT_UNSET")
                  '("KEEP=base" "REMOVED=base") t)
                 '(("VALUE" . "first=\nline") ("EXPLICIT_UNSET") ("REMOVED")))))

(ert-deftest terminal-remote-portal-subdirectory-uses-existing-workspace-owner ()
  (terminal-remote-test-with-registry
    (let ((workspace (remote-workspace-open "/fs:local:/tmp/portal-owner/" :connect nil)))
      (unwind-protect
          (should (equal (+portal-workspace-id "/fs:local:/tmp/portal-owner/child/")
                         (remote-workspace-id workspace)))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-client-cwd-report-never-changes-the-owning-target ()
  (with-temp-buffer
    (setq-local +portal-target-directory "/fs:shared-client:/project/"
                portal-terminal--directory "/project/sub/"
                portal-terminal--exited nil
                remote-terminal-instance (remote-terminal-create :buffer (current-buffer) :state 'open))
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) "/project/")))
      (+portal--native-terminal-changed-a)
      (should (equal default-directory "/fs:shared-client:/project/sub/")))
    (setq +portal-target-directory "/fs:shared-client:/srv/project/"
          portal-terminal--directory "/client/mount/project/sub/")
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) "/client/mount/project/")))
      (+portal--native-terminal-changed-a)
      (should (equal default-directory "/fs:shared-client:/srv/project/")))))

(ert-deftest terminal-remote-portal-web-keeps-url-and-loader-on-client ()
  (require 'aaron-open)
  (with-temp-buffer
    (setq default-directory "/fs:unreachable-web-target:/project/")
    (setq-local process-environment '("PATH=/target/bin") exec-path '("/target/bin")
                remote--buffer-base-process-environment '("PATH=/client/bin" "WEB_OWNER=client")
                remote--buffer-base-exec-path '("/client/bin"))
    (let ((remote-current-adapter-id "target-adapter") (remote-current-route 'target-route)
          (remote-current-workspace 'target-workspace) loaded opened)
      (cl-letf (((symbol-function 'remote-require-on-client)
                 (lambda (feature &rest _)
                   (should (equal default-directory temporary-file-directory))
                   (should (equal (getenv "WEB_OWNER") "client"))
                   (should (equal exec-path '("/client/bin")))
                   (should-not remote-current-route)
                   (push feature loaded)))
                ((symbol-function '+portal--ensure)
                 (lambda () (should (equal (getenv "WEB_OWNER") "client"))))
                ((symbol-function 'aaron-open--with-browser-window)
                 (lambda (_mode _reuse function) (funcall function)))
                ((symbol-function 'portal-open-url)
                 (lambda (url)
                   (should (equal default-directory temporary-file-directory))
                   (setq opened url)))
                ((symbol-function 'portal-input-focus) #'ignore))
        (aaron/open-portal-url "http://localhost:3000/project"))
      (should (equal opened "http://localhost:3000/project"))
      (should (equal loaded '(portal-web term/portal/config)))
      (should (equal default-directory "/fs:unreachable-web-target:/project/"))
      (should (equal (getenv "PATH") "/target/bin")))))

(ert-deftest terminal-remote-portal-source-url-uses-terminal-owner-in-a-foreign-callback ()
  (require 'aaron-open)
  (terminal-remote-test-with-registry
    (let* ((workspace (remote-workspace-open "/fs:local:/tmp/url-owner/" :connect nil))
           (buffer (generate-new-buffer " *terminal-url-owner*"))
           (terminal (remote-terminal-adopt workspace buffer
                                            :metadata (list :live-function (lambda (_) t))))
           mapped opened)
      (unwind-protect
          (with-temp-buffer
            (setq default-directory "/fs:another-url-target:/unrelated/")
            (cl-letf (((symbol-function 'remote-require-on-client) #'ignore)
                      ((symbol-function '+portal--ensure) #'ignore)
                      ((symbol-function 'remote-url-for-client)
                       (lambda (url &rest args)
                         (should (equal default-directory temporary-file-directory))
                         (should-not remote-current-workspace)
                         (setq mapped (list url (plist-get args :workspace)))
                         "http://127.0.0.1:49999/target?q=%2F#part"))
                      ((symbol-function 'aaron-open--with-browser-window)
                       (lambda (_mode _reuse function) (funcall function)))
                      ((symbol-function 'portal-open-url) (lambda (url) (setq opened url)))
                      ((symbol-function 'portal-input-focus) #'ignore))
              (+portal/open-terminal-url "http://localhost:3000/target?q=%2F#part" terminal)
              (should (equal mapped (list "http://localhost:3000/target?q=%2F#part" workspace)))
              (should (equal opened "http://127.0.0.1:49999/target?q=%2F#part"))))
        (remote-workspace-close workspace)))))

(ert-deftest terminal-remote-eshell-loads-code-on-client-and-initializes-on-target ()
  (remote-require-on-client 'esh-mode)
  (with-temp-buffer
    (setq default-directory "/fs:unreachable-eshell-target:/project/")
    (setq-local process-environment '("PATH=/target/bin") exec-path '("/target/bin")
                remote--buffer-base-process-environment '("PATH=/usr/bin:/bin" "ESH_OWNER=client")
                remote--buffer-base-exec-path '("/usr/bin" "/bin"))
    (let ((eshell-path-env-list '("/target/bin")) loaded initialized)
      (cl-letf (((symbol-function 'load)
                 (lambda (&rest _)
                   (setq loaded t)
                   (should (equal default-directory temporary-file-directory))
                   (should (equal (getenv "ESH_OWNER") "client"))
                   (should-not eshell-path-env-list)
                   t))
                ((symbol-function 'eshell-terminal-remote-fixture-initialize)
                 (lambda ()
                   (setq initialized t)
                   (should (equal default-directory "/fs:unreachable-eshell-target:/project/"))
                   (should (equal eshell-path-env-list '("/target/bin"))))))
        (eshell-load-modules '(eshell-terminal-remote-fixture))
        (eshell-initialize-modules '(eshell-terminal-remote-fixture)))
      (should loaded)
      (should initialized)
      (should (equal (getenv "PATH") "/target/bin")))))

(provide 'terminal-remote-tests)
