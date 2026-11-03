;;; agent-lifecycle-remote-live.el --- The lifecycle on a real target -*- lexical-binding: t; -*-

;; Opt-in: AGENT_REMOTE_TARGET names a target with node on PATH.  The ACP fixture
;; runs on that target, under /tmp, and is removed afterwards.  Set
;; AGENT_REMOTE_BACKEND to tramp or tramp-rpc to check process and forward routes.
;; No provider,
;; credential or model request is involved.

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/manager)
(require 'tools/agent/queue)
(unless (featurep 'remote-live-target)
  (load (expand-file-name "test/remote-live-target.el" user-emacs-directory) nil t))
(unless (featurep 'agent-flow-e2e)
  (load (expand-file-name "test/agent-flow-e2e.el" user-emacs-directory) nil t))

(defun agent-remote-live--config (fixture store)
  "Return a fixture agent whose process and files are native target paths."
  (agent-shell-make-agent-config
   :identifier 'flow-fixture :mode-line-name "Fixture"
   :buffer-name "Agent Remote" :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker (lambda (buffer)
                   (agent-shell--make-acp-client
                    :command "node" :command-params (list fixture store)
                    :environment-variables '("AGENT_REMOTE_MARKER=ssh-route"
                                             "AGENT_FLOW_LIST_DELAY_MS=100")
                    :context-buffer buffer))))

(defun agent-remote-live--dispose (&rest buffers)
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(defun agent-remote-live--request-count (file method)
  "Count METHOD frames the real fixture received in its log FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-count method
              (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                      (split-string (buffer-string) "\n" t))
              :key (lambda (row) (alist-get 'method row)) :test #'equal)))

(ert-deftest agent-remote-live-queue-exit-changes-and-restore ()
  (let ((target (getenv "AGENT_REMOTE_TARGET")))
    (unless target (ert-skip "Set AGENT_REMOTE_TARGET to a target with node"))
    (let* ((native (format "/tmp/agent-live-%s/" (format-time-string "%s%N")))
           (root (remote-make-file-name target native))
           (fixture (concat native "fixture.mjs"))
           (store (concat native "sessions.json"))
           (agent-shell-confirm-interrupt nil)
           (agent-shell-busy-submit-default-function #'agent-shell-busy-submit-queue)
           (target-object (remote-get-target target))
           (preferences (copy-tree (remote-target-preferences target-object)))
           (+agent-acp-notify-function nil)
           (asked nil) buffer restored opened editor peer other)
      (make-directory root t)
      (unwind-protect
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt &rest _) (push prompt asked) nil))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _) (caar choices)))
                    ((symbol-function 'find-file) (lambda (file) (setq opened file)))
                    ((symbol-function 'find-file-other-window) (lambda (file) (setq opened file))))
            (when-let* ((backend (getenv "AGENT_REMOTE_BACKEND")))
              (setf (remote-target-preferences target-object)
                    (append (list (cons 'process-async (list backend))
                                  (cons 'port-forward (list backend)))
                            (assq-delete-all 'port-forward
                                             (assq-delete-all 'process-async (copy-tree preferences))))))
            (remote-workspace-open root :connect nil :load-environment nil)
            (copy-file (expand-file-name "test/fixtures/agent-flow-acp.mjs" user-emacs-directory)
                       (concat root "fixture.mjs"))
            ;; The clipboard belongs to the client; only its file is sent
            ;; to the workspace.  Fake the UI source, keep real file IO.
            (let* ((screenshots (concat root "screenshots/"))
                   (image "fixture-image-bytes")
                   (agent-shell-clipboard-image-handlers
                    (list (list (cons :command "sh")
                                (cons :save
                                      (lambda (file)
                                        (should-not (file-remote-p file))
                                        (should-not (file-remote-p default-directory))
                                        (should (zerop (call-process "true" nil nil nil)))
                                        (with-temp-file file (insert image))))))))
              (make-directory screenshots)
              (with-temp-buffer
                (setq default-directory root)
                (let ((file (agent-shell--save-clipboard-image :destination-dir screenshots)))
                  (should (equal (remote-file-name-target file) target))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (should (equal (buffer-string) image))))))
            (setq buffer (+agent-acp-start :config (agent-remote-live--config fixture store)
                                           :directory root :focus t :origin 'manual))
            (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))) 40)
            (agent-flow-e2e--wait
             (lambda () (not (buffer-local-value '+agent-acp--title-query buffer))) 20)
            (let* ((log (concat root "sessions.json.events"))
                   (before (agent-remote-live--request-count log "session/list")))
              (with-current-buffer buffer
                (dotimes (_ 10) (agent-shell--refresh-session-title))
                (should-not (+agent-acp-busy-p buffer)))
              (agent-flow-e2e--wait
               (lambda () (not (buffer-local-value '+agent-acp--title-query buffer))) 20)
              (should (= 1 (- (agent-remote-live--request-count log "session/list") before))))
            (should (equal (plist-get (+agent-session-snapshot buffer) :target) target))
            (let* ((process (+agent-acp-state-value buffer '(:client :process)))
                   (route (process-get process 'remote-route)))
              (should (equal (remote-route-target-id route) target))
              (should (equal (remote-route-adapter-id route) "agent-shell"))
              (should (eq (process-get process 'remote-process-class) 'interactive))
              (when-let* ((backend (getenv "AGENT_REMOTE_BACKEND")))
                (should (equal (remote-route-backend-id route) backend)))
              (message "ACP live route: target=%s backend=%s adapter=%s class=%s"
                       target (remote-route-backend-id route)
                       (remote-route-adapter-id route)
                       (process-get process 'remote-process-class)))
            (agent-flow-e2e--send buffer "runtime")
            (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
            (let ((text (agent-flow-e2e--text buffer)))
              (should (string-match-p "ssh-route" text))
              (should (string-match-p (regexp-quote (directory-file-name native)) text)))
            ;; ACP's editor APIs see unsaved text, then save into the same
            ;; target file and buffer.  They must not silently read disk or
            ;; open a client file with the same bare absolute spelling.
            (let ((file (concat root "editor.txt")))
              (write-region "disk version\n" nil file nil 'silent)
              (setq editor (find-file-noselect file))
              (with-current-buffer editor
                (erase-buffer)
                (insert "unsaved editor version\nsecond editor line\nthird editor line\n"))
              (agent-flow-e2e--send buffer "fs-read")
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
              (should (string-match-p "FS_READ:unsaved editor version"
                                      (agent-flow-e2e--text buffer)))
              (with-temp-buffer
                (insert-file-contents file)
                (should (equal (buffer-string) "disk version\n")))
              (agent-flow-e2e--send buffer "fs-read-range")
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
              (should (string-match-p (regexp-quote "FS_RANGE:\"second editor line\\n\"")
                                      (agent-flow-e2e--text buffer)))
              (agent-flow-e2e--send buffer "fs-missing")
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
              (should (string-match-p "FS_MISSING_CODE:-32002" (agent-flow-e2e--text buffer)))
              (agent-flow-e2e--send buffer "fs-write")
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
              (should (string-match-p "FS_WRITE_OK" (agent-flow-e2e--text buffer)))
              (with-current-buffer editor
                (should (equal (buffer-string) "written by ACP\n"))
                (should-not (buffer-modified-p)))
              (with-temp-buffer
                (insert-file-contents file)
                (should (equal (buffer-string) "written by ACP\n"))))
            ;; Project is the shared pool; session is the physical client.
            ;; A peer reuses transport, not ACP state, and ending it must
            ;; leave both its project peer and another project running.
            (let* ((other-root (concat root "project-other/"))
                   (route (process-get (+agent-acp-state-value buffer '(:client :process))
                                       'remote-route))
                   (session (remote-connection-cached-p route)))
              (make-directory other-root)
              (remote-workspace-open other-root :connect nil :load-environment nil)
              (setq peer (+agent-acp-start
                          :config (agent-remote-live--config fixture (concat native "peer.json"))
                          :directory root :focus nil :origin 'manual)
                    other (+agent-acp-start
                           :config (agent-remote-live--config fixture (concat native "project-other/sessions.json"))
                           :directory other-root :focus nil :origin 'manual))
              (dolist (agent (list peer other))
                (agent-flow-e2e--wait (lambda () (+agent-acp-state-value agent '(:init-finished))) 40))
              (should (= 2 (length (+agent-pool-sessions root target))))
              (should (= 1 (length (+agent-pool-sessions other-root target))))
              (with-current-buffer buffer
                (cl-letf (((symbol-function '+agent-session-snapshot)
                           (lambda (&rest _) (ert-fail "Live tab query built a full snapshot"))))
                  (let ((tabs (+agent-acp--project-buffers)))
                    (should (= 2 (length tabs)))
                    (should (memq buffer tabs))
                    (should (memq peer tabs))
                    (should-not (memq other tabs)))))
              (should (eq session (remote-connection-cached-p route)))
              (should-not (eq (+agent-acp-state-value buffer '(:client :process))
                              (+agent-acp-state-value peer '(:client :process))))
              (agent-flow-e2e--send peer "titled")
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p peer))) 20)
              (should (equal (+agent-acp-state-value peer '(:session :title)) "Fixture title"))
              (should-not (equal (+agent-acp-state-value buffer '(:session :title)) "Fixture title"))
              (+agent-acp-kill peer)
              (should (= 1 (length (+agent-pool-sessions root target))))
              (with-current-buffer buffer
                (should (equal (+agent-acp--project-buffers) (list buffer))))
              (should (process-live-p (+agent-acp-state-value buffer '(:client :process))))
              (should (process-live-p (+agent-acp-state-value other '(:client :process))))
              (should (eq session (remote-connection-cached-p route)))
              (agent-flow-e2e--send buffer "hold")
              (agent-flow-e2e--send other "hold")
              (agent-flow-e2e--wait (lambda () (and (+agent-acp-busy-p buffer) (+agent-acp-busy-p other))) 20)
              ;; Local forward loss belongs to its project resource, even
              ;; while ACP clients on the same transport have pending turns.
              (let* ((owner (remote-workspace-for-path root))
                     (other-owner (remote-workspace-for-path other-root))
                     (clients (mapcar (lambda (agent) (+agent-acp-state-value agent '(:client))) (list buffer other)))
                     (processes (mapcar (lambda (client) (map-elt client :process)) clients))
                     (ids (mapcar (lambda (agent) (+agent-acp-state-value agent '(:session :id))) (list buffer other)))
                     (epoch (remote-background-target-epoch target))
                     (group (remote-channel-group-open
                             '((probe . (:host "127.0.0.1" :port 9)))
                             :context (remote-workspace-context owner) :workspace owner :key 'acp-isolation))
                     (resource (remote-workspace-find-resource owner 'channel-group 'acp-isolation)))
                (should-not (eq owner other-owner))
                (when-let* ((backend (getenv "AGENT_REMOTE_BACKEND")))
                  (should (equal backend (remote-route-backend-id
                                          (remote-forward-route (cdar (remote-channel-group-channels group)))))))
                (delete-process (remote-forward-handle (cdar (remote-channel-group-channels group))))
                (agent-flow-e2e--wait
                 (lambda () (and (not (eq group (remote-workspace-resource-value resource)))
                                  (remote-channel-group-live-p (remote-workspace-resource-value resource))
                                  (not (remote-workspace-resource-recovering-p owner resource)))) 20)
                (should (remote-workspace-live-p owner))
                (should (remote-workspace-live-p other-owner))
                (should (= epoch (remote-background-target-epoch target)))
                (should (eq session (remote-connection-cached-p route)))
                (cl-mapc (lambda (agent client process id)
                           (should (eq client (+agent-acp-state-value agent '(:client))))
                           (should (eq process (+agent-acp-state-value agent '(:client :process))))
                           (should (process-live-p process))
                           (should (equal id (+agent-acp-state-value agent '(:session :id))))
                           (should (+agent-acp-busy-p agent)))
                         (list buffer other) clients processes ids)
                ;; An actual session/list response proves protocol IO after
                ;; recovery; a live OS process alone would not prove this.
                (agent-flow-e2e--wait (lambda () (not (buffer-local-value '+agent-acp--title-query other))) 20)
                (let* ((log (concat other-root "sessions.json.events"))
                       (before (agent-remote-live--request-count log "session/list")))
                  (with-current-buffer other (agent-shell--refresh-session-title))
                  (agent-flow-e2e--wait (lambda () (not (buffer-local-value '+agent-acp--title-query other))) 20)
                  (should (= 1 (- (agent-remote-live--request-count log "session/list") before)))
                  (should (+agent-acp-busy-p other)))
                (message "ACP forward isolation: target=%s backend=%s two project sessions retained; session/list replied"
                         target (remote-route-backend-id route)))
              (should (= 1 (+agent-pool-stop root target)))
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
              (should (+agent-acp-busy-p other))
              (+agent-acp-stop other)
              (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p other))) 20))
            ;; C37/C39: stopping holds the queue; resuming sends it.
            (agent-flow-e2e--send buffer "hold")
            (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)) 20)
            (agent-flow-e2e--send buffer "later")
            (+agent-acp-stop buffer)
            (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
            (should-not asked)
            (should (= 1 (+agent-acp-held-prompt-count buffer)))
            (+agent-queue-resume buffer)
            (agent-flow-e2e--wait (lambda () (and (zerop (+agent-acp-pending-prompt-count buffer))
                                                  (not (+agent-acp-busy-p buffer))))
                                  20)
            (should (string-match-p "Reply: later" (agent-flow-e2e--text buffer)))
            ;; C44: a change the agent reports opens on the session's target.
            (agent-flow-e2e--send buffer "edit")
            (agent-flow-e2e--wait (lambda () (and (not (+agent-acp-busy-p buffer))
                                                  (buffer-local-value '+agent-acp--changes buffer)))
                                  20)
            (should (string-suffix-p "changed.txt"
                                     (caar (buffer-local-value '+agent-acp--changes buffer))))
            (with-temp-buffer (+agent-session-changes buffer))
            (should (equal (remote-file-name-target opened) target))
            ;; C38: the remote process exits and the session says so at once.
            (let ((id (+agent-acp-state-value buffer '(:session :id))))
              (agent-flow-e2e--send buffer "crash-later")
              (agent-flow-e2e--wait
               (lambda ()
                 (let ((snapshot (+agent-session-snapshot buffer)))
                   (and (equal (+agent-sessions--state snapshot) "dead")
                        (plist-get snapshot :last-error))))
               20)
              (let ((snapshot (+agent-session-snapshot buffer)))
                (should (eq (plist-get snapshot :turn-state) 'interrupted))
                (should (plist-get snapshot :last-error))
                (should-not (plist-get snapshot :busy)))
              ;; C42: the saved conversation returns with no live session.
              (+agent-acp-kill buffer)
              (setq restored (+agent-acp-start :config (agent-remote-live--config fixture store)
                                               :directory root :focus nil :origin 'manual
                                               :choose-saved t))
              (agent-flow-e2e--wait
               (lambda () (+agent-acp-state-value restored '(:init-finished))) 40)
              (should (equal id (+agent-acp-state-value restored '(:session :id))))
              ;; An explicit target disconnect fences work before closing
              ;; transport, preserves a retained conversation, and requires
              ;; a fresh explicit reconnect without replaying held prompts.
              (agent-flow-e2e--send restored "hold")
              (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p restored)) 20)
              (agent-flow-e2e--send restored "held across target disconnect")
              (let ((process (+agent-acp-state-value restored '(:client :process)))
                    (prompts (agent-remote-live--request-count (concat root "sessions.json.events") "session/prompt")))
                (with-current-buffer restored
                  (setq-local kill-buffer-query-functions (list (lambda () nil))))
                (remote-workspace-disconnect-target target 'agent-live-test)
                (should (buffer-live-p restored))
                (should-not (process-live-p process))
                (should (= 1 (+agent-acp-held-prompt-count restored)))
                (should-error (+agent-queue-resume restored) :type 'user-error)
                (let ((snapshot (+agent-session-snapshot restored)))
                  (should (eq (plist-get snapshot :process-state) 'disconnected))
                  (should (eq (plist-get snapshot :turn-state) 'interrupted))
                  (should (equal (plist-get snapshot :native-session-id) id))
                  (should-not (plist-get snapshot :last-error)))
                (with-current-buffer restored (setq-local kill-buffer-query-functions nil))
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (setq restored (+agent-acp-restart-native restored)))
                (agent-flow-e2e--wait (lambda () (+agent-acp-state-value restored '(:init-finished))) 40)
                (should (equal id (+agent-acp-state-value restored '(:session :id))))
                (should-not (buffer-local-value 'remote-buffer-disconnected-p restored))
                (should (= 1 (+agent-acp-held-prompt-count restored)))
                (should (= prompts (agent-remote-live--request-count (concat root "sessions.json.events") "session/prompt")))
                (message "ACP explicit disconnect/reconnect: target=%s session retained; held prompt never sent" target)
                (+agent-queue-clear restored))))
        (setf (remote-target-preferences target-object) preferences)
        (agent-remote-live--dispose buffer restored peer other)
        (when (buffer-live-p editor)
          (with-current-buffer editor (set-buffer-modified-p nil))
          (agent-remote-live--dispose editor))
        (ignore-errors (delete-directory root t))
        (ignore-errors (remote-workspace-disconnect-target target 'live-test-cleanup))))))

(provide 'agent-lifecycle-remote-live)
;;; agent-lifecycle-remote-live.el ends here
