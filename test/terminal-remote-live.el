;;; terminal-remote-live.el --- Real Eshell, native Portal and Web placement -*- lexical-binding: t; -*-

;; TERMINAL_REMOTE_TARGET=local|aaron-pc; TERMINAL_REMOTE_BACKEND=tramp|tramp-rpc
;; selects process-async and port-forward only.  Filesystem keeps its default.
;; Use the Portal GUI test runner for terminal-remote-live-native-and-web.
(require 'ert)
(require 'cl-lib)
(require 'term/portal/config)
(require 'term/shell/eshell)
(require 'aaron-open)
(unless (featurep 'remote-live-target)
  (load (expand-file-name "test/remote-live-target.el" user-emacs-directory) nil t))
(defvar eshell-directory-name)
(defvar eshell-history-file-name)
(defvar eshell-last-dir-ring-file-name)

(defun terminal-remote-live--wait (predicate &optional seconds)
  "Wait for PREDICATE, servicing process and native view events."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (when (display-graphic-p) (redisplay t)))
    (should (funcall predicate))))

(defmacro terminal-remote-live--fixture (&rest body)
  "Run BODY in disposable target ROOT/CHILD and client shell history."
  (declare (indent 0))
  `(let* ((target (or (getenv "TERMINAL_REMOTE_TARGET") "local"))
          (backend (getenv "TERMINAL_REMOTE_BACKEND"))
          (token (format "terminal-%s-%x" (emacs-pid) (random most-positive-fixnum)))
          (native (concat (file-name-as-directory (or (getenv "TERMINAL_REMOTE_TMP_ROOT") "/tmp/")) token "/"))
          (root (remote-make-file-name target native))
          (child (concat root "project child/"))
          (history (make-temp-file "terminal-eshell-history-" t))
          (eshell-directory-name history)
          (eshell-history-file-name (expand-file-name "history" history))
          (eshell-last-dir-ring-file-name (expand-file-name "lastdir" history))
          (configured (remote-get-target target))
          (preferences (copy-tree (remote-target-preferences configured)))
          workspace terminal buffers)
     (unwind-protect
         (progn
           (when backend
             (setf (remote-target-preferences configured)
                   (append (list (cons 'process-async (list backend))
                                 (cons 'port-forward (list backend)))
                           (assq-delete-all 'port-forward
                                            (assq-delete-all 'process-async (copy-tree preferences))))))
           (make-directory child t)
           (setq workspace (remote-workspace-open root :connect nil :load-environment nil))
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (dolist (buffer buffers)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))
       (when workspace (remote-workspace-close workspace))
       (condition-case cleanup-error
           (when (file-directory-p root) (delete-directory root t))
         (error (message "Terminal fixture cleanup: %s (%s)" root
                         (error-message-string cleanup-error))))
       (delete-directory history t)
       (setf (remote-target-preferences configured) preferences))))

(ert-deftest terminal-remote-live-comint-keeps-selected-child-and-environment ()
  (terminal-remote-live--fixture
    (setq terminal (remote-terminal-open
                    workspace :directory child :shell "/bin/sh"
                    :arguments '("-c" "printf 'PTY:%s|%s\\n' \"$PWD\" \"$TERMINAL_TEST\"; read value")
                    :environment '(("TERMINAL_TEST" . "target"))))
    (terminal-remote-live--wait
     (lambda () (with-current-buffer (remote-terminal-buffer terminal)
                  (string-match-p (regexp-quote (concat "PTY:" native "project child|target"))
                                  (buffer-string)))))
    (let ((route (process-get (remote-terminal-process terminal) 'remote-route)))
      (should (equal (remote-route-target-id route) target))
      (should (eq (remote-route-capability route) 'pty))
      (message "Terminal live: target=%s backend=%s frontend=comint" target (remote-route-backend-id route)))
    (remote-terminal-mark-disconnected terminal 'test)
    (setq terminal (remote-terminal-restart terminal))
    (terminal-remote-live--wait
     (lambda () (with-current-buffer (remote-terminal-buffer terminal)
                  (string-match-p (regexp-quote (concat "PTY:" native "project child|target"))
                                  (buffer-string)))))))

(ert-deftest terminal-remote-live-eshell-routes-queue-cwd-and-timeout ()
  (terminal-remote-live--fixture
    (let* ((buffer (+eshell-buffer child token))
           (original (symbol-function 'remote-make-process)) routes results)
      (push buffer buffers)
      (with-current-buffer buffer (setenv "TERMINAL_TEST" "target"))
      (cl-letf (((symbol-function 'remote-make-process)
                 (lambda (&rest args)
                   (let ((process (apply original args)))
                     (push (process-get process 'remote-route) routes) process))))
        (let ((result (+eshell-run-sync
                       "/bin/sh -c 'printf \"ESH:%s|%s\\n\" \"$PWD\" \"$TERMINAL_TEST\"'"
                       :directory child :name token :timeout 30)))
          (should (= (plist-get result :status) 0))
          (should (string-match-p (regexp-quote (concat "ESH:" native "project child|target"))
                                  (plist-get result :output))))
        (should routes)
        (dolist (route routes)
          (should (equal (remote-route-target-id route) target))
          (when backend (should (equal (remote-route-backend-id route) backend))))
        (message "Terminal live: target=%s backend=%s frontend=eshell" target
                 (remote-route-backend-id (car routes)))
        (+eshell-run "/bin/sh -c 'sleep 0.2; pwd'" :directory child :name token
                     :callback (lambda (result) (setq results (append results (list result)))))
        (+eshell-run "/bin/pwd" :directory root :name token
                     :callback (lambda (result) (setq results (append results (list result)))))
        (terminal-remote-live--wait (lambda () (= (length results) 2)))
        (should (string-match-p (regexp-quote (concat native "project child")) (plist-get (car results) :output)))
        (should (equal (string-trim (plist-get (cadr results) :output)) (directory-file-name native)))
        (setq results nil)
        (+eshell-run "/bin/sleep 30" :directory child :name token :timeout 0.3
                     :callback (lambda (result) (setq results (append results (list result)))))
        (+eshell-run "/bin/echo after-timeout" :directory root :name token
                     :callback (lambda (result) (setq results (append results (list result)))))
        (terminal-remote-live--wait (lambda () (= (length results) 2)))
        (should (eq (plist-get (car results) :reason) 'timeout))
        (should (= (plist-get (cadr results) :status) 0))
        (should (string-match-p "after-timeout" (plist-get (cadr results) :output)))))))

(ert-deftest terminal-remote-live-eshell-explicit-disconnect-retains-draft-and-resolves-queue ()
  (terminal-remote-live--fixture
    (let* ((buffer (+eshell-buffer child token))
           results process)
      (push buffer buffers)
      (with-current-buffer buffer
        (setq-local kill-buffer-query-functions (list (lambda () nil)))
        (goto-char (point-max)) (insert "retained remote draft"))
      (+eshell-run "/bin/sleep 30" :directory child :name token
                   :callback (lambda (result) (push result results)))
      (+eshell-run "/bin/echo must-not-run-after-disconnect" :directory root :name token
                   :callback (lambda (result) (push result results)))
      (terminal-remote-live--wait (lambda () (buffer-local-value 'eshell-foreground-command buffer)))
      (setq process (get-buffer-process buffer))
      (let ((route (process-get process 'remote-route)))
        (should (equal (remote-route-target-id route) target))
        (when backend (should (equal (remote-route-backend-id route) backend)))
        (message "Eshell explicit disconnect: target=%s backend=%s" target (remote-route-backend-id route)))
      (remote-workspace-disconnect-target target 'terminal-test)
      (should (buffer-live-p buffer))
      (should-not (process-live-p process))
      (should (= 2 (length results)))
      (should (cl-every (lambda (result) (and (eq (plist-get result :reason) 'disconnected)
                                             (null (plist-get result :status)))) results))
      (terminal-remote-live--wait
       (lambda () (with-current-buffer buffer
                    (and (null +eshell--dispatch-timer) (null +eshell--command-timer)
                         (equal (+eshell--input) "retained remote draft")))))
      (should-error (+eshell-run "/bin/echo implicit-restart" :directory child :name token)
                    :type 'user-error)
      (with-current-buffer buffer
        (delete-region eshell-last-output-end (point-max))
        (insert "/bin/echo explicit-restart")
        (eshell-send-input))
      (terminal-remote-live--wait
       (lambda () (with-current-buffer buffer
                    (and (not (+eshell--running-p))
                         (string-match-p "^explicit-restart$" (buffer-string))))))
      (should-not (buffer-local-value 'remote-buffer-disconnected-p buffer))
      (let ((result (+eshell-run-sync "/bin/echo after-explicit-restart" :directory child :name token :timeout 20)))
        (should (= 0 (plist-get result :status)))
        (should (equal (string-trim (plist-get result :output)) "after-explicit-restart")))
      (should (= 2 (length results))))))

(defun terminal-remote-live--fetch-http (url marker)
  "Fetch client URL over a fresh TCP stream and require body MARKER."
  (remote-require-on-client 'url-parse)
  (let* ((parsed (url-generic-parse-url url))
         (buffer (generate-new-buffer " *terminal-url-http-response*"))
         (default-directory temporary-file-directory)
         (remote-channel-native-api-inhibit t)
         (remote-current-route nil)
         (remote-current-workspace nil)
         process)
    (unwind-protect
        (progn
          (setq process (make-network-process :name "terminal-url-http-client" :buffer buffer
                                              :host (url-host parsed) :service (url-port parsed)
                                              :coding 'binary :noquery t :sentinel #'ignore))
          (process-send-string process (concat "GET " (url-filename parsed) " HTTP/1.0\r\nHost: "
                                                (url-host parsed) "\r\nConnection: close\r\n\r\n"))
          (terminal-remote-live--wait
           (lambda () (with-current-buffer buffer (string-match-p (regexp-quote marker) (buffer-string)))) 10)
          (with-current-buffer buffer (should (string-match-p "\\`HTTP/1\\.[01] 200" (buffer-string)))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer buffer))))

(defun terminal-remote-live--url-recovery (&optional browser)
  "Exercise a target HTTP server across its forward's failure, with BROWSER."
  (let ((remote-workspace-open-hook nil) (remote-workspace-close-hook nil)
        (remote-workspace-auto-reconnect t))
    (terminal-remote-live--fixture
      (let* ((script (concat child "url-fixture.py"))
             (before (concat "HTTP-before-" token))
             (after (concat "HTTP-after-" token))
             source group resource web)
        (unwind-protect
            (save-window-excursion
              (copy-file (expand-file-name "test/fixtures/remote-url-http-server.py" user-emacs-directory) script)
              (write-region (concat "<title>" before "</title>") nil (concat child "index.html") nil 'silent)
              (let* ((reply (remote-exec "/usr/bin/python3" :context child :check t :filesystem-effects 'content
                                         :args (append (list (remote-file-local-name script) "start" (remote-file-local-name child))
                                                       ;; The disposable SSH runner can reap
                                                       ;; this detached process after a GUI crash.
                                                       (when-let* ((owner (getenv "TERMINAL_REMOTE_SSH_CONFIG")))
                                                         (list owner)))))
                     (server (json-parse-string (remote-exec-result-stdout reply) :object-type 'plist)))
                (should (integerp (plist-get server :pid)))
                (setq source (format "http://127.0.0.1:%s/index.html?phase=before#fragment" (plist-get server :port))))
              (terminal-remote-live--fetch-http (remote-url-for-client source :workspace workspace) before)
              (setq resource (remote-workspace-find-resource workspace 'channel-group)
                    group (and resource (remote-workspace-resource-value resource)))
              (when group
                (let* ((forward (cdr (assq 'http (remote-channel-group-channels group))))
                       (route (remote-forward-route forward))
                       (process (remote-forward-handle forward)))
                  (should (process-live-p process))
                  (should (equal (remote-route-target-id route) target))
                  (when backend (should (equal (remote-route-backend-id route) backend)))
                  (message "URL recovery: target=%s backend=%s capability=%s" target
                           (remote-route-backend-id route) (remote-route-capability route))
                  (delete-process process)
                  (terminal-remote-live--wait
                   (lambda () (and (remote-workspace-live-p workspace)
                                    (not (eq group (remote-workspace-resource-value resource)))
                                    (remote-channel-group-live-p (remote-workspace-resource-value resource)))) 30)
                  (should-not (remote-channel-group-live-p group))
                  (setq group (remote-workspace-resource-value resource))))
              (write-region (concat "<title>" after "</title>") nil (concat child "index.html") nil 'silent)
              (setq source (replace-regexp-in-string "phase=before" "phase=after" source t t))
              (terminal-remote-live--fetch-http (remote-url-for-client source :workspace workspace) after)
              (when browser
                (with-temp-buffer
                  (setq default-directory "/fs:unrelated-url-recovery-callback:/project/")
                  (setq web (aaron/open-portal-url source nil workspace)))
                (push web buffers)
                (let ((handle (buffer-local-value 'portal-web--target web)))
                  (terminal-remote-live--wait
                   (lambda () (equal (plist-get (portal--inspect handle) :title) after)))))
              (princ (format "Target HTTP after forward recovery: target=%s forwarded=%s browser=%s\n"
                             target (and group t) (and browser t)) #'external-debugging-output)
              (remote-workspace-close workspace)
              (when group (should-not (remote-channel-group-live-p group))))
          (when (file-exists-p script)
            (let ((stopped (remote-exec "/usr/bin/python3" :context child :filesystem-effects 'content
                                        :args (list (remote-file-local-name script) "stop" (remote-file-local-name child)))))
              (should (= (remote-exec-result-status stopped) 0)))))))))

(ert-deftest terminal-remote-live-url-recovers-while-target-http-server-persists ()
  (terminal-remote-live--url-recovery))

(ert-deftest terminal-remote-live-native-source-url-recovers-the-page-after-forward-failure ()
  (skip-unless (display-graphic-p))
  (terminal-remote-live--url-recovery t))

(ert-deftest terminal-remote-live-native-and-web ()
  (skip-unless (display-graphic-p))
  (terminal-remote-live--fixture
    (save-window-excursion
      (delete-other-windows)
      (let* ((command '("/bin/sh" "-c"
                         "printf 'NATIVE:%s|%s\\nNATIVE-PID:%s\\n' \"$PWD\" \"$TERMINAL_TEST\" \"$$\"; while IFS= read -r value; do printf 'ECHO:%s\\n' \"$value\"; done"))
             (buffer (+portal-create :directory child :command command
                                     :environment '(("TERMINAL_TEST" . "target"))))
             (target-handle (buffer-local-value 'portal-view--target buffer)) pid)
        (push buffer buffers)
        (setq terminal (buffer-local-value 'remote-terminal-instance buffer))
        (should (eq (buffer-local-value 'major-mode buffer) 'portal-terminal-mode))
        (should-not (remote-terminal-process terminal))
        (terminal-remote-live--wait
         (lambda () (string-match-p (regexp-quote (concat "NATIVE:" native "project child|target"))
                                    (portal--terminal-text target-handle))))
        (princ "Native terminal ready\n" #'external-debugging-output)
        (let ((text (portal--terminal-text target-handle)))
          (should (string-match "NATIVE-PID:\\([0-9]+\\)" text))
          (setq pid (match-string 1 text)))
        (let ((route (plist-get (remote-terminal-metadata terminal) :route)))
          (should (equal (remote-route-target-id route) target))
          (should (eq (remote-route-capability route) 'pty))
          (message "Terminal live: target=%s backend=%s frontend=portal-native" target
                   (remote-route-backend-id route)))
        (remote-terminal-send-string terminal "hello-from-native\n")
        (terminal-remote-live--wait
         (lambda () (string-match-p "ECHO:hello-from-native" (portal--terminal-text target-handle))))
        (princ "Native input echoed\n" #'external-debugging-output)
        (remote-terminal-mark-disconnected terminal 'test)
        (should (buffer-live-p buffer))
        (should-not (portal--inspect target-handle))
        (should (with-current-buffer buffer (string-match-p "NATIVE:" (buffer-string))))
        (terminal-remote-live--wait
         (lambda () (= (remote-exec-result-status
                         (remote-exec "/bin/sh" :context root :filesystem-effects 'none
                                      :args (list "-c" "kill -0 \"$1\" 2>/dev/null" "terminal-probe" pid))) 1)) 10)
        (setq terminal (remote-terminal-restart terminal)
              buffer (remote-terminal-buffer terminal))
        (push buffer buffers)
        (pop-to-buffer buffer)
        (setq target-handle (buffer-local-value 'portal-view--target buffer))
        (terminal-remote-live--wait
         (lambda () (string-match-p (regexp-quote (concat "NATIVE:" native "project child|target"))
                                    (portal--terminal-text target-handle))))
        (princ "Native terminal restarted\n" #'external-debugging-output)
        (with-temp-buffer
          (setq default-directory child)
          (let ((web (aaron/open-portal-url "data:text/html,<title>Remote Portal Web</title><p>client view</p>")))
            (push web buffers)
            (let ((handle (buffer-local-value 'portal-web--target web)))
              (terminal-remote-live--wait
               (lambda () (equal (plist-get (portal--inspect handle) :title) "Remote Portal Web"))))))
        (remote-workspace-close workspace)
        (should-not (buffer-live-p buffer))
        (should-not (portal--inspect target-handle))))))

(ert-deftest terminal-remote-live-native-source-url-opens-the-target-http-service ()
  (skip-unless (display-graphic-p))
  (terminal-remote-live--fixture
    (let ((output (generate-new-buffer " *terminal-url-http-server*"))
          (title (concat "Target URL " token)) server web group)
      (push output buffers)
      (unwind-protect
          (save-window-excursion
            (write-region (concat "<title>" title "</title><p>served by the source target</p>")
                          nil (concat child "index.html") nil 'silent)
            (setq server
                  (remote-make-process
                   :name "terminal-url-http-server" :buffer output :noquery t
                   :command '("/usr/bin/python3" "-u" "-c"
                              "from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler; s=ThreadingHTTPServer(('127.0.0.1',0),SimpleHTTPRequestHandler); print('URL-SERVER:%s'%s.server_address[1],flush=True); s.serve_forever()")
                   :remote-context (remote-workspace-context workspace)
                   :remote-directory child :connection-type 'pipe :sentinel #'ignore))
            (terminal-remote-live--wait
             (lambda () (with-current-buffer output (string-match-p "URL-SERVER:[0-9]+" (buffer-string)))))
            (let* ((port (with-current-buffer output
                           (string-match "URL-SERVER:\\([0-9]+\\)" (buffer-string))
                           (match-string 1 (buffer-string))))
                   (url (concat "http://127.0.0.1:" port "/index.html?q=%2F#part"))
                   (buffer (+portal-create :directory child :command '("/bin/sh" "-c" "read value"))))
              (push buffer buffers)
              (setq terminal (buffer-local-value 'remote-terminal-instance buffer))
              ;; A callback's selected buffer must not replace the URL's owner.
              (with-temp-buffer
                (setq default-directory "/fs:unreachable-url-callback:/other-project/")
                (setq web (+portal/open-terminal-url url terminal)))
              (push web buffers)
              (let ((handle (buffer-local-value 'portal-web--target web)))
                (terminal-remote-live--wait
                 (lambda () (equal (plist-get (portal--inspect handle) :title) title))))
              (should (process-live-p server))
              (when-let* ((resource (remote-workspace-find-resource workspace 'channel-group)))
                (setq group (remote-workspace-resource-value resource))
                (should (remote-channel-group-live-p group)))
              (let ((again (remote-url-for-client url :workspace workspace)))
                (should (string-suffix-p "/index.html?q=%2F#part" again))
                (should (<= (length (remote-workspace-resources workspace)) 2)))
              (princ (format "Target HTTP served into client Web: target=%s forwarded=%s\n" target (and group t))
                     #'external-debugging-output)
              (remote-workspace-close workspace)
              (when group (should-not (remote-channel-group-live-p group)))
              (should-not (buffer-live-p buffer))
              (should (buffer-live-p web))))
        (when (and server (process-live-p server)) (delete-process server))))))

(ert-deftest terminal-remote-live-native-interactive-cwd-and-peer ()
  (skip-unless (display-graphic-p))
  (terminal-remote-live--fixture
    (save-window-excursion
      (delete-other-windows)
      (let* ((buffer (+portal-create :directory child))
             (handle (buffer-local-value 'portal-view--target buffer))
             (second (concat root "second directory/")) peer peer-handle)
        (push buffer buffers)
        (make-directory second)
        (setq terminal (buffer-local-value 'remote-terminal-instance buffer))
        (portal--terminal-paste handle "printf 'INTERACTIVE-CWD:%s\\n' \"$PWD\"" t)
        (terminal-remote-live--wait
         (lambda () (string-match-p (regexp-quote (concat "INTERACTIVE-CWD:" native "project child"))
                                    (portal--terminal-text handle))))
        (princ "Interactive initial cwd ready\n" #'external-debugging-output)
        (portal--terminal-paste handle (concat "cd -- " (shell-quote-argument (concat native "second directory"))) t)
        (portal--terminal-paste handle "printf 'INTERACTIVE-NEXT:%s\\n' \"$PWD\"" t)
        (terminal-remote-live--wait
         (lambda () (string-match-p (regexp-quote (concat "INTERACTIVE-NEXT:" native "second directory"))
                                    (portal--terminal-text handle))))
        (princ "Interactive cd executed\n" #'external-debugging-output)
        (if (remote-client-file-name second)
            (progn
              (terminal-remote-live--wait
               (lambda () (equal (remote-canonicalize-emacs-file-name
                                 (buffer-local-value 'default-directory buffer)) second)))
              (princ "Interactive client cwd tracked\n" #'external-debugging-output))
          (should (equal (buffer-local-value 'default-directory buffer) child))
          (princ "Target identity retained; native target cwd reporting remains pending\n" #'external-debugging-output))
        (setq peer (+portal-create :directory child))
        (push peer buffers)
        (setq peer-handle (buffer-local-value 'portal-view--target peer))
        (should (equal (remote-terminal-workspace-id terminal)
                       (remote-terminal-workspace-id (buffer-local-value 'remote-terminal-instance peer))))
        (remote-terminal-close terminal)
        (should-not (portal--inspect handle))
        (should (portal--inspect peer-handle))
        (should (buffer-live-p peer))
        (remote-workspace-close workspace)
        (should-not (portal--inspect peer-handle))
        (should-not (buffer-live-p peer))))))

(provide 'terminal-remote-live)
