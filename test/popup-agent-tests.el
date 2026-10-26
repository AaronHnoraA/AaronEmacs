;;; popup-agent-tests.el --- Shared terminal/ACP popup regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'term/portal/popup)
(require 'noema-agent-acp)
(require 'tools/agent/config)

(ert-deftest agent-shell-stall-warning-clears-on-progress ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((process (make-pipe-process :name "acp-watch-test" :buffer nil))
           (state `((:active-requests . (((:method . "session/prompt"))))
                    (:client . ((:process . ,process)))))
           (agent-shell--state state)
           (+agent-shell--last-activity (float-time))
           warning deleted scheduled)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell--update-fragment)
                     (lambda (&rest args) (setq warning args)))
                    ((symbol-function 'agent-shell--delete-fragment)
                     (lambda (&rest args) (setq deleted args)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args) nil))
                    ((symbol-function 'message) #'ignore))
            (+agent-shell--stall-tick (current-buffer) state)
            (should-not +agent-shell--stall-shown)
            (should-not warning)
            (setq +agent-shell--last-activity
                  (- (float-time) +agent-shell-stall-warning-seconds 5))
            (+agent-shell--stall-tick (current-buffer) state)
            (should +agent-shell--stall-shown)
            (should (equal (plist-get warning :block-id) "acp-stalled"))
            (should (string-match-p "process is running" (plist-get warning :body)))
            (should scheduled)
            (+agent-shell--note-activity '((:event . tool-call-update)))
            (should-not +agent-shell--stall-shown)
            (should (equal (plist-get deleted :block-id) "acp-stalled")))
        (delete-process process)))))

(ert-deftest agent-shell-dead-client-does-not-send-into-uninitialized-process ()
  (let* ((process (make-pipe-process :name "acp-dead-client-test" :buffer nil))
         (state `((:initialized . t)
                  (:client . ((:process . ,process)))))
         sent failure)
    (delete-process process)
    (+agent-shell--reject-dead-client-a
     (lambda (&rest _) (setq sent t))
     :state state
     :on-failure (lambda (error-object _raw) (setq failure error-object)))
    (should-not sent)
    (should (string-match-p "reconnect"
                            (map-elt failure 'message)))))

(ert-deftest agent-shell-dead-client-can-restart-its-initialize-handshake ()
  (let* ((process (make-pipe-process :name "acp-reinit-test" :buffer nil))
         (state `((:client . ((:process . ,process)))))
         sent)
    (delete-process process)
    (+agent-shell--reject-dead-client-a
     (lambda (&rest _) (setq sent t))
     :state state :request '((:method . "initialize")))
    (should sent)))

(ert-deftest agent-shell-submit-reconnects-dead-client-before-sending ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode
          agent-shell--state '((:client . nil)))
    (let (reconnected sent)
      (cl-letf (((symbol-function '+agent-shell--dead-client-p)
                 (lambda (_) t))
                ((symbol-function '+agent-shell/reconnect)
                 (lambda () (setq reconnected t))))
        (+agent-shell--submit-a (lambda (&rest _) (setq sent t)))
        (should reconnected)
        (should-not sent)))))

(ert-deftest agent-shell-reconnect-retains-unsent-input-and-session-id ()
  (let ((old (generate-new-buffer " *acp-reconnect-old*"))
        (new (generate-new-buffer " *acp-reconnect-new*"))
        restarted inserted adopted)
    (unwind-protect
        (with-current-buffer old
          (setq major-mode 'agent-shell-mode
                agent-shell--state
                '((:session . ((:id . "saved-session")))
                  (:active-requests . (old-request))))
          (setq-local noema-agent-acp-session-origin 'manual
                      noema-agent-acp-session-name "manual/claude"
                      noema-agent-acp-session-agent "claude"
                      noema-agent-acp-session-root "/tmp/project/")
          (cl-letf (((symbol-function '+agent-shell--dead-client-p)
                     (lambda (_) t))
                    ((symbol-function 'agent-shell--prompt-input)
                     (lambda () "unsent draft"))
                    ((symbol-function 'agent-shell-restart)
                     (lambda (&rest args)
                       (setq restarted args)
                       (should-not (map-elt agent-shell--state :active-requests))))
                    ((symbol-function 'get-buffer)
                     (lambda (_) new))
                    ((symbol-function 'noema-agent-acp-adopt)
                     (lambda (&rest args) (setq adopted args)))
                    ((symbol-function 'agent-shell-insert)
                     (lambda (&rest args) (setq inserted args)))
                    ((symbol-function 'message) #'ignore))
            (+agent-shell/reconnect)
            (should (equal restarted '(:session-id "saved-session")))
            (should (eq (car adopted) new))
            (should (equal (plist-get (cdr adopted) :name) "manual/claude"))
            (should (equal (plist-get inserted :text) "unsent draft"))
            (should-not (plist-get inserted :submit))))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest agent-shell-refresh-skips-windows-in-hidden-frames ()
  (let ((buffer (generate-new-buffer " *hidden-frame-refresh*"))
        (+auto-revert-recent-buffer-limit 0))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (should (memq buffer (+auto-revert--candidate-buffers)))
          (cl-letf (((symbol-function 'frame-visible-p)
                     (lambda (_frame) nil)))
            (should-not (memq buffer (+auto-revert--candidate-buffers)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-shell-turn-complete-dispatches-buffer-refresh-policy ()
  "ACP completion uses a buffer's registered refresh policy regardless of mode."
  (let* ((buffer (generate-new-buffer " *agent-refresh-policy*"))
         (+auto-revert--agent-turn-check-timer nil)
         callback called provider-called
         (+auto-revert-agent-refresh-functions
          (list (lambda () (setq provider-called t)))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local +auto-revert-agent-refresh-function
                        (lambda (target) (setq called target))))
          (cl-letf (((symbol-function '+auto-revert--candidate-buffers)
                     (lambda () (list buffer)))
                    ((symbol-function '+auto-revert--refresh-buffer-if-safe)
                     (lambda (&rest _) (ert-fail "Ignored registered policy")))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest _args)
                       (setq callback fn) 'scheduled)))
            (+auto-revert-agent-turn-complete-h nil nil)
            (should callback)
            (funcall callback)
            (should (eq called buffer))
            (should provider-called)
            (setq called nil)
            (with-current-buffer buffer
              (setq-local remote-buffer-disconnected-p t))
            (+auto-revert-agent-refresh-buffer buffer)
            (should-not called)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-shell-turn-complete-refreshes-round-trip-remote-file-once ()
  "A remote file is checked at ACP completion without background polling."
  (with-temp-buffer
    (setq-local buffer-file-name "/fs:box:/work/main.py")
    (set-buffer-modified-p nil)
    (let (refreshed (exists-checks 0) (verify-checks 0)
          (operation-cost 'round-trip) unchanged)
      (cl-letf (((symbol-function 'remote-file-operation-cost)
                 (lambda (_file &optional _adapter) operation-cost))
                ((symbol-function 'file-exists-p)
                 (lambda (_file) (cl-incf exists-checks) t))
                ((symbol-function 'verify-visited-file-modtime)
                 (lambda (&rest _) (cl-incf verify-checks) unchanged))
                ((symbol-function 'revert-buffer)
                 (lambda (&rest _) (setq refreshed t))))
        (should-not (+auto-revert--unmodified-stale-file-buffer-p (current-buffer)))
        (should (= exists-checks 0))
        (should (= verify-checks 0))
        (setq unchanged t)
        (+auto-revert-agent-refresh-buffer (current-buffer))
        (should-not refreshed)
        (should (= verify-checks 1))
        (should (= exists-checks 0))
        (setq unchanged nil)
        (+auto-revert-agent-refresh-buffer (current-buffer))
        (should refreshed)
        (should (= verify-checks 2))
        (should (= exists-checks 1))
        (setq operation-cost 'batched)
        (should (+auto-revert--unmodified-stale-file-buffer-p (current-buffer)))
        (should (= exists-checks 2))
        (setq-local remote-buffer-disconnected-p t)
        (setq refreshed nil)
        (+auto-revert-agent-refresh-buffer (current-buffer))
        (should-not refreshed)
        (should (= exists-checks 2))))))

(ert-deftest agent-shell-turn-complete-keeps-unsaved-remote-edit ()
  "The remote ACP check reports a conflict without replacing local text."
  (with-temp-buffer
    (setq-local buffer-file-name "/fs:box:/work/main.py")
    (insert "Unsaved work")
    (let ((+auto-revert--warned-modified-files (make-hash-table :test #'equal))
          notice)
      (cl-letf (((symbol-function 'remote-file-operation-cost)
                 (lambda (_file &optional _adapter) 'round-trip))
                ((symbol-function 'file-exists-p) (lambda (_file) t))
                ((symbol-function 'verify-visited-file-modtime)
                 (lambda (&rest _) nil))
                ((symbol-function 'revert-buffer)
                 (lambda (&rest _) (ert-fail "Discarded unsaved remote edit")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq notice (apply #'format format-string args)))))
        (should-not (+auto-revert--modified-stale-file-buffer-p (current-buffer)))
        (+auto-revert-agent-refresh-buffer (current-buffer))
        (should (buffer-modified-p))
        (should (equal (buffer-string) "Unsaved work"))
        (should (string-match-p "File changed on disk" notice))
        (should (+auto-revert--modified-stale-file-buffer-p (current-buffer) t))))))

(ert-deftest agent-shell-file-refresh-keeps-cheap-local-focus-path ()
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/agent-refresh-local.py")
    (set-buffer-modified-p nil)
    (let ((buffer (current-buffer)) refreshed)
      (cl-letf (((symbol-function 'remote-file-operation-cost)
                 (lambda (_file &optional _adapter) 'batched))
                ((symbol-function 'file-exists-p) (lambda (_file) t))
                ((symbol-function 'verify-visited-file-modtime)
                 (lambda (&rest _) nil))
                ((symbol-function '+auto-revert--candidate-buffers)
                 (lambda () (list buffer)))
                ((symbol-function 'revert-buffer)
                 (lambda (&rest _) (setq refreshed t))))
        (+auto-revert-refresh-visible-stale-buffers-h)
        (should refreshed)))))

(ert-deftest agent-shell-old-remote-buffer-refreshes-when-shown ()
  "Showing a round-trip buffer schedules one check outside the hot path."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *agent-old-remote*"))
          (window (selected-window))
          (timer (timer-create))
          callback
          (schedules 0)
          (refreshes 0))
      (unwind-protect
          (progn
            (let ((window-buffer-change-functions nil))
              (set-window-buffer window buffer))
            (with-current-buffer buffer
              (setq-local buffer-file-name "/tmp/agent-old-remote.py"))
            (cl-letf (((symbol-function 'remote-file-operation-cost)
                       (lambda (_file &optional _adapter) 'round-trip))
                      ((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest _args)
                         (cl-incf schedules)
                         (setq callback fn)
                         timer))
                      ((symbol-function '+auto-revert-agent-refresh-buffer)
                       (lambda (_buffer) (cl-incf refreshes))))
              (+auto-revert-check-shown-round-trip-file-h window)
              (+auto-revert-check-shown-round-trip-file-h window)
              (should (= schedules 1))
              (funcall callback)
              (should (= refreshes 1))
              (with-current-buffer buffer
                (setq-local remote-buffer-disconnected-p t))
              (+auto-revert-check-shown-round-trip-file-h window)
              (should (= schedules 1))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-shell-close-does-not-prompt-for-txt-or-disable-other-queries ()
  (dolist (popup '(nil t))
    (let ((buffer (generate-new-buffer " *popup-transcript-test*"))
          (shell-maker-prompt-before-killing-buffer t)
          (queries 0))
      (unwind-protect
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local shell-maker--config 'test
                        kill-buffer-query-functions
                        (list #'shell-maker-kill-buffer-query
                              (lambda () (cl-incf queries) t)))
            (insert "Agent output and an unsent prompt")
            (run-hooks 'agent-shell-mode-hook)
            (when popup (+portal-popup-apply-ui buffer))
            (should (buffer-modified-p))
            (should (local-variable-p 'shell-maker-prompt-before-killing-buffer))
            (should-not shell-maker-prompt-before-killing-buffer)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Asked to save transcript")))
                      ((symbol-function 'shell-maker-save-session-transcript) (lambda () (ert-fail "Exported transcript"))))
              (should (kill-buffer buffer)))
            (should (= queries 1)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-transcripts-disabled-for-new-and-existing-sessions ()
  (should-not (agent-shell--transcript-file-path))
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    ;; Existing sessions retain their old path until the policy is applied.
    (setq-local agent-shell--transcript-file "/unused/transcript.md")
    (+agent-shell-disable-transcripts)
    (cl-letf (((symbol-function 'write-region)
               (lambda (&rest _) (ert-fail "Wrote a transcript")))
              ((symbol-function 'make-directory)
               (lambda (&rest _) (ert-fail "Created a transcript directory"))))
      (should-not (agent-shell--ensure-transcript-file))
      (agent-shell--append-transcript :text "Agent output"
                                      :file-path agent-shell--transcript-file))))

(ert-deftest agent-shell-busy-submit-steers-supported-agents-and-queues-others ()
  "Busy Enter steers only when the active adapter advertised support."
  (should agent-shell-persistent-prompt-enabled)
  (should (eq agent-shell-busy-submit-default-function
              #'+agent-shell-busy-submit-steer-or-queue))
  (should (eq agent-shell-busy-submit-override-function
              #'agent-shell-busy-submit-queue))
  (with-temp-buffer
    (let (calls)
      (cl-letf (((symbol-function 'agent-shell-busy-submit-steer)
                 (lambda (prompt) (push (list 'steer prompt) calls)))
                ((symbol-function 'agent-shell-busy-submit-queue)
                 (lambda (prompt) (push (list 'queue prompt) calls))))
        (setq-local agent-shell--state '((:supports-steering . t)))
        (funcall agent-shell-busy-submit-default-function "Change direction")
        (setq-local agent-shell--state '((:supports-steering . nil)))
        (funcall agent-shell-busy-submit-default-function "Next turn")
        (funcall agent-shell-busy-submit-override-function "Explicit queue")
        (should (equal (nreverse calls)
                       '((steer "Change direction")
                         (queue "Next turn")
                         (queue "Explicit queue"))))))))

(defun +agent-shell-test--execution (session-id directory &optional agent)
  "Make a fake live ACP frontend for SESSION-ID in DIRECTORY."
  (let ((buffer (generate-new-buffer " *agent-session-test*"))
        (process (make-pipe-process :name "agent-session-test" :noquery t)))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode
            default-directory directory)
      (setq-local agent-shell--state
                  (agent-shell--make-state
                   :agent-config `((:identifier . ,(or agent 'codex)))
                   :buffer buffer))
      (map-put! (map-elt agent-shell--state :session) :id session-id)
      (map-put! agent-shell--state :client
                `((:process . ,process)
                  (:command . "fake-acp")
                  (:instance-count . 0)
                  (:error-handlers . nil)
                  (:notification-handlers . nil)
                  (:request-handlers . nil)
                  (:pending-requests . nil))))
    buffer))

(defun +agent-shell-test--close (buffer)
  "Release fake ACP process and BUFFER."
  (when (buffer-live-p buffer)
    (let ((process (noema-agent-acp-state-value buffer '(:client :process))))
      (when (and (processp process) (process-live-p process))
        (delete-process process)))
    (kill-buffer buffer)))

(ert-deftest agent-shell-resume-reuses-one-local-execution ()
  (let ((default-directory "/tmp/")
        (config '((:identifier . codex)))
        (launches 0) buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () default-directory)))
          (let ((first (+agent-shell--reuse-session-a
                        (lambda (&rest _) (cl-incf launches)
                          (let ((buffer (+agent-shell-test--execution "task-a" "/tmp/")))
                            (push buffer buffers) buffer))
                        :config config :session-id "task-a" :no-focus t)))
            (should (eq first
                        (+agent-shell--reuse-session-a
                         (lambda (&rest _) (ert-fail "Started a duplicate process"))
                         :config config :session-id "task-a" :no-focus t)))
            (should (= launches 1))))
      (mapc #'+agent-shell-test--close buffers))))

(ert-deftest agent-shell-resume-reuses-explicit-upstream-cwd ()
  "The new :cwd argument selects the matching execution even from elsewhere."
  (let* ((config '((:identifier . codex)))
         (buffer (+agent-shell-test--execution "task-a" "/tmp/")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd)
                   (lambda () (ert-fail "Ignored explicit :cwd"))))
          (should (eq buffer
                      (+agent-shell--reuse-session-a
                       (lambda (&rest _) (ert-fail "Started a duplicate process"))
                       :config config :session-id "task-a"
                       :cwd "/tmp/" :no-focus t))))
      (+agent-shell-test--close buffer))))

(ert-deftest agent-shell-codex-reuse-leaves-other-adapters-unchanged ()
  (let* ((default-directory "/tmp/")
         (config '((:identifier . claude)))
         (first (+agent-shell-test--execution "task-a" "/tmp/" 'claude))
         (started nil)
         (second nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () default-directory)))
          (setq second
                (+agent-shell--reuse-session-a
                 (lambda (&rest _)
                   (setq started t)
                   (+agent-shell-test--execution "task-a" "/tmp/" 'claude))
                 :config config :session-id "task-a" :no-focus t))
          (should started)
          (should-not (eq first second)))
      (+agent-shell-test--close first)
      (+agent-shell-test--close second))))

(ert-deftest noema-resume-reuses-codex-without-reinitializing-it ()
  (let* ((config '((:identifier . codex)))
         (buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (starts 0)
         (subscriptions 0))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest _) (cl-incf starts)
                     (ert-fail "Started a second Codex client")))
                  ((symbol-function 'noema-agent-acp-subscribe)
                   (lambda (&rest _) (cl-incf subscriptions))))
          (should (eq buffer (noema-agent-acp-start
                              :config config :directory "/tmp/"
                              :session-id "task-a")))
          (should (zerop starts))
          (should (zerop subscriptions)))
      (+agent-shell-test--close buffer))))

(ert-deftest agent-shell-resume-keeps-tasks-and-hosts-separate ()
  (let ((config '((:identifier . codex)))
        (buffers (list (+agent-shell-test--execution "task-a" "/tmp/")
                       (+agent-shell-test--execution
                        "task-a" "/fs:server-a:/work/")))
        (launches 0))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () default-directory)))
          (dolist (case '(("task-b" "/tmp/")
                          ("task-a" "/fs:server-b:/work/")))
            (let ((default-directory (cadr case)))
              (+agent-shell--reuse-session-a
               (lambda (&rest _)
                 (cl-incf launches)
                 (let ((buffer (+agent-shell-test--execution
                                (car case) (cadr case))))
                   (push buffer buffers) buffer))
               :config config :session-id (car case) :no-focus t)))
          (should (= launches 2))
          (should (eq (+agent-shell--find-execution
                       "task-a" config "/fs:server-a:/work/")
                      (cadr (last buffers 2)))))
      (mapc #'+agent-shell-test--close buffers))))

(ert-deftest agent-shell-resume-ignores-dead-process-and-cleans-on-kill ()
  (let* ((config '((:identifier . codex)))
         (buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (process (noema-agent-acp-state-value buffer '(:client :process))))
    (unwind-protect
        (progn
          (should (eq buffer (+agent-shell--find-execution "task-a" config "/tmp/")))
          (delete-process process)
          (should-not (+agent-shell--find-execution "task-a" config "/tmp/"))
          (with-current-buffer buffer
            (setq-local +agent-shell--requested-session-id "task-a")
            (+agent-shell--forget-session)
            (should-not +agent-shell--requested-session-id)))
      (+agent-shell-test--close buffer))))

(ert-deftest agent-shell-task-busy-suppresses-resume-fallback ()
  (let ((state '((:agent-config . ((:identifier . codex)))))
        (request '((:method . "session/load")
                   (:params . ((sessionId . "task-a")))))
        (fallback nil) wire handled)
    (cl-letf (((symbol-function '+agent-shell--handle-task-busy)
               (lambda (&rest _) (setq handled t))))
      (+agent-shell--busy-request-a
       (lambda (&rest args) (setq wire args))
       :state state :request request :buffer (current-buffer)
       :on-failure (lambda (&rest _) (setq fallback t)))
      (funcall (plist-get wire :on-failure)
               '((code . -32000) (data . ((code . "TASK_BUSY")))) nil)
      (should handled)
      (should-not fallback)
      (should (+agent-shell--task-busy-p
               '((message . "Another Codex session is using this task."))))
      (should-not (+agent-shell--task-busy-p
                   '((message . "Session not found")))))))

(ert-deftest agent-shell-task-busy-shows-retry-state-and-releases-client ()
  (let* ((buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (request '((:method . "session/load")
                    (:params . ((sessionId . "task-a")))))
         fragment)
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'agent-shell--update-fragment)
                     (lambda (&rest args) (setq fragment args)))
                    ((symbol-function 'agent-shell--emit-event) #'ignore)
                    ((symbol-function 'shell-maker-busy) (lambda () nil))
                    ((symbol-function 'agent-shell-heartbeat-stop) #'ignore))
            (+agent-shell--handle-task-busy buffer request))
          (should (eq +agent-shell--task-busy 'external))
          (should (equal +agent-shell--requested-session-id "task-a"))
          (should (equal (plist-get fragment :label-left)
                         "Task already active"))
          (should-not (+agent-shell-execution-live-p buffer)))
      (+agent-shell-test--close buffer))))

(ert-deftest agent-shell-explicit-shutdown-still-terminates-execution ()
  (let* ((buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (process (noema-agent-acp-state-value buffer '(:client :process))))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'agent-shell-heartbeat-stop) #'ignore))
            (agent-shell--shutdown))
          (should-not (process-live-p process))
          (should-not (noema-agent-acp-state-value buffer '(:client))))
      (+agent-shell-test--close buffer))))

(defun +noema-agent-bridge-test--request (request)
  "Send REQUEST through the same JSON boundary as emacsclient."
  (json-parse-string
   (+noema-agent-bridge-request
    (base64-encode-string
     (encode-coding-string (json-serialize request) 'utf-8) t))
   :object-type 'alist :array-type 'list))

(ert-deftest noema-agent-bridge-lists-only-live-target-scoped-sessions ()
  (let* ((+noema-agent-bridge-enabled t)
         (live (+agent-shell-test--execution "task-a" "/tmp/"))
         (dead (+agent-shell-test--execution "task-b" "/tmp/")))
    (unwind-protect
        (progn
          (delete-process (noema-agent-acp-state-value dead '(:client :process)))
          (let* ((response (+noema-agent-bridge-test--request
                            '((action . "list"))))
                 (sessions (alist-get 'sessions response)))
            (should (= (length sessions) 1))
            (should (equal (alist-get 'sessionId (car sessions)) "task-a"))
            (should (equal (alist-get 'workspace (car sessions))
                           (+agent-shell--session-context "/tmp/")))))
      (+agent-shell-test--close live)
      (+agent-shell-test--close dead))))

(ert-deftest noema-agent-bridge-sends-to-exact-session-and-queues-when-busy ()
  (let* ((+noema-agent-bridge-enabled t)
         (buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (request `((action . "send") (sessionId . "task-a")
                    (agent . "codex")
                    (workspace . ,(+agent-shell--session-context "/tmp/"))
                    (message . "Mobile update")))
         queued submitted first queued-response)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-acp-enqueue)
                   (lambda (_buffer text) (setq queued text)))
                  ((symbol-function 'noema-agent-acp-prompt)
                   (lambda (&rest args) (setq submitted args)))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest _) (ert-fail "Started a second process"))))
          (setq first (+noema-agent-bridge-test--request request))
          (should (equal (alist-get 'state first) "submitted"))
          (should (equal (map-elt (car (plist-get submitted :content)) 'text)
                         "Mobile update"))
          (with-current-buffer buffer
            (+noema-agent-bridge--event
             '((:event . agent-message-chunk)
               (:data . ((:text-chunk . "Received"))))))
          (should (equal (alist-get 'text
                                   (+noema-agent-bridge-test--request
                                    `((action . "read") (sessionId . "task-a")
                                      (agent . "codex")
                                      (workspace . ,(+agent-shell--session-context "/tmp/"))
                                      (requestId . ,(alist-get 'requestId first)))))
                         "Received"))
          (funcall (plist-get submitted :on-success) '((stopReason . "end_turn")))
          (should (equal (alist-get 'state
                                   (+noema-agent-bridge-test--request
                                    `((action . "read") (sessionId . "task-a")
                                      (agent . "codex")
                                      (workspace . ,(+agent-shell--session-context "/tmp/"))
                                      (requestId . ,(alist-get 'requestId first)))))
                         "completed"))
          (with-current-buffer buffer (setq-local shell-maker--busy t))
          (setq queued-response (+noema-agent-bridge-test--request request))
          (should (equal (alist-get 'state queued-response) "queued"))
          (should (equal queued "Mobile update"))
          (with-current-buffer buffer
            (+noema-agent-bridge--event
             '((:event . input-submitted)
               (:data . ((:prompt . "Mobile update")))))
            (+noema-agent-bridge--event
             '((:event . agent-message-chunk)
               (:data . ((:text-chunk . "Queued reply")))))
            (+noema-agent-bridge--event '((:event . turn-complete))))
          (let ((reply (+noema-agent-bridge-test--request
                        `((action . "read") (sessionId . "task-a")
                          (agent . "codex")
                          (workspace . ,(+agent-shell--session-context "/tmp/"))
                          (requestId . ,(alist-get 'requestId queued-response))))))
            (should (equal (alist-get 'state reply) "completed"))
            (should (equal (alist-get 'text reply) "Queued reply")))
          (should (eq (alist-get 'ok
                                 (+noema-agent-bridge-test--request
                                  `((action . "send") (sessionId . "task-a")
                                    (agent . "codex")
                                    (workspace . "/fs:other:/tmp")
                                    (message . "wrong host"))))
                      :false)))
      (+agent-shell-test--close buffer))))

(ert-deftest noema-agent-bridge-requires-enabling-and-explicit-interrupt ()
  (let* ((buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (request `((action . "interrupt") (sessionId . "task-a")
                    (agent . "codex")
                    (workspace . ,(+agent-shell--session-context "/tmp/"))))
         stopped)
    (unwind-protect
        (progn
          (should (eq (alist-get 'ok
                                 (+noema-agent-bridge-test--request
                                  '((action . "list"))))
                      :false))
          (let ((+noema-agent-bridge-enabled t))
            (with-current-buffer buffer (setq-local shell-maker--busy t))
            (cl-letf (((symbol-function 'noema-agent-acp-stop)
                       (lambda (target) (setq stopped target))))
              (should (equal (alist-get 'state
                                       (+noema-agent-bridge-test--request request))
                             "interrupt-requested"))
              (should (eq stopped buffer)))))
      (+agent-shell-test--close buffer))))

(ert-deftest noema-agent-bridge-does-not-queue-behind-structured-run ()
  (let* ((+noema-agent-bridge-enabled t)
         (buffer (+agent-shell-test--execution "task-a" "/tmp/"))
         (request `((action . "send") (sessionId . "task-a")
                    (agent . "codex")
                    (workspace . ,(+agent-shell--session-context "/tmp/"))
                    (message . "Wait for the Run")))
         enqueued)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-acp-busy-p) (lambda (_) t))
                  ((symbol-function 'noema-agent-acp-enqueue)
                   (lambda (&rest _) (setq enqueued t))))
          (let ((response (+noema-agent-bridge-test--request request)))
            (should (eq (alist-get 'ok response) :false))
            (should (string-match-p "Run is active" (alist-get 'error response)))
            (should-not enqueued)))
      (+agent-shell-test--close buffer))))

(ert-deftest agent-shell-command-menu-adds-local-resume-to-advertised-commands ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell-completion--shell-buffer (current-buffer)
                agent-shell--state
                '((:available-commands . (((name . "status")
                                           (description . "Show status"))))))
    (insert "/")
    (setq-local company-mode t)
    (let (opened)
      (should (equal (nth 2 (agent-shell--command-completion-at-point))
                     '("resume" "status")))
      (cl-letf (((symbol-function 'company-manual-begin)
                 (lambda () (setq opened t)))
                ((symbol-function 'agent-shell--trigger-completion-at-point)
                 (lambda () (ert-fail "Used fallback completion"))))
        (+agent-shell--show-completion-after-insert)
        (should opened)))))

(ert-deftest agent-shell-local-resume-completes-without-advertised-commands ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell-completion--shell-buffer (current-buffer)
                agent-shell--state '((:available-commands . nil)))
    (insert "/")
    (should (equal (nth 2 (agent-shell--command-completion-at-point))
                   '("resume")))))

(ert-deftest agent-shell-resume-slash-is-not-sent-to-the-agent ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let (resumed submitted)
      (cl-letf (((symbol-function 'agent-shell--prompt-input)
                 (lambda () "/resume"))
                ((symbol-function '+agent-shell/resume)
                 (lambda () (setq resumed t))))
        (+agent-shell--submit-a (lambda (&rest _) (setq submitted t)))
        (should resumed)
        (should-not submitted)))))

(ert-deftest agent-shell-resume-chooses-official-id-in-current-workspace ()
  (let ((source (generate-new-buffer " *resume-source*"))
        (target (generate-new-buffer " *resume-target*"))
        listed-cwd resumed-id binding-name)
    (unwind-protect
        (with-current-buffer source
          (setq major-mode 'agent-shell-mode
                default-directory "/tmp/")
          (setq-local agent-shell--state
                      '((:supports-session-list . t)
                        (:supports-session-resume . t)
                        (:agent-config . ((:identifier . codex)))
                        (:session . ((:id . "current")))))
          (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp/"))
                    ((symbol-function 'agent-shell--resolve-path) #'identity)
                    ((symbol-function 'agent-shell--list-sessions)
                     (lambda (&rest args)
                       (setq listed-cwd (plist-get args :cwd))
                       (funcall (plist-get args :on-success)
                                '(((sessionId . "current")
                                   (title . "Current conversation")
                                   (updatedAt . "2026-10-01T00:00:00Z"))
                                  ((sessionId . "saved-native-id")
                                   (title . "Saved conversation")
                                   (updatedAt . "2026-09-30T00:00:00Z"))))))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _) (caar choices)))
                    ((symbol-function '+agent-shell--find-execution)
                     (lambda (&rest _) nil))
                    ((symbol-function 'noema-sessions-native-binding)
                     (lambda (&rest _) '(:name "research" :session-id "logical-id")))
                    ((symbol-function 'noema-agent-acp-mark-session-buffer)
                     (lambda (buffer name _agent _root)
                       (should (eq buffer target))
                       (setq binding-name name)))
                    ((symbol-function 'agent-shell--prompt-input)
                     (lambda () nil))
                    ((symbol-function 'agent-shell-restart)
                     (lambda (&rest args)
                       (setq resumed-id (plist-get args :session-id))
                       (let ((name (buffer-name)))
                         (kill-buffer (current-buffer))
                         (with-current-buffer target
                           (rename-buffer name t)))))
                    ((symbol-function 'agent-shell--display-buffer)
                     (lambda (&rest _) (ert-fail "Opened another buffer"))))
            (+agent-shell/resume)
            (should (equal listed-cwd "/tmp/"))
            (should (equal resumed-id "saved-native-id"))
            (should (equal binding-name "research"))
            (should (equal (buffer-local-value 'noema-agent-promote--session-id target)
                           "logical-id"))
            (should-not (buffer-live-p source))))
      (mapc #'kill-buffer (list source target)))))

(ert-deftest agent-shell-resume-reuses-live-session-buffer ()
  (let ((source (generate-new-buffer " *resume-source*"))
        (target (generate-new-buffer " *resume-target*"))
        shown)
    (unwind-protect
        (with-current-buffer source
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--state
                      '((:agent-config . ((:identifier . codex)))
                        (:session . ((:id . "current")))))
          (cl-letf (((symbol-function 'agent-shell--sort-sessions-by-recency) #'identity)
                    ((symbol-function 'agent-shell--session-title)
                     (lambda (_) "Saved conversation"))
                    ((symbol-function 'agent-shell--format-session-date)
                     (lambda (_) "today"))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _) (caar choices)))
                    ((symbol-function '+agent-shell--find-execution)
                     (lambda (&rest _) target))
                    ((symbol-function 'agent-shell--prompt-input)
                     (lambda () nil))
                    ((symbol-function 'agent-shell-restart)
                     (lambda (&rest _) (ert-fail "Restarted an already live session")))
                    ((symbol-function 'agent-shell--display-buffer)
                     (lambda (buffer) (setq shown buffer))))
            (+agent-shell--resume-from-list
             source '(((sessionId . "saved-native-id")
                       (updatedAt . "2026-09-30T00:00:00Z"))))
            (should (eq shown target))
            (should (buffer-live-p source))))
      (mapc #'kill-buffer (list source target)))))

(ert-deftest agent-shell-resume-keeps-popup-session-in-popup-pool ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *resume-popup-source*"))
          (target (generate-new-buffer " *resume-popup-target*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil)
          (+portal-popup--displaying t)
          resumed-id)
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq major-mode 'agent-shell-mode
                    default-directory "/tmp/")
              (setq-local +portal-popup-workspace-id "local-work"
                          agent-shell--state
                          '((:agent-config . ((:identifier . claude)))
                            (:session . ((:id . "current"))))))
            (+portal-popup-display-buffer source)
            (cl-letf (((symbol-function 'agent-shell--sort-sessions-by-recency) #'identity)
                      ((symbol-function 'agent-shell--session-title)
                       (lambda (_) "Saved conversation"))
                      ((symbol-function 'agent-shell--format-session-date)
                       (lambda (_) "today"))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices)))
                      ((symbol-function '+agent-shell--find-execution)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell--prompt-input) (lambda () nil))
                      ((symbol-function 'noema-sessions-native-binding)
                       (lambda (&rest _) nil))
                      ((symbol-function 'noema-agent-acp-start)
                       (lambda (&rest args)
                         (setq resumed-id (plist-get args :session-id))
                         (with-current-buffer target
                           (setq major-mode 'agent-shell-mode
                                 default-directory "/tmp/"))
                         target))
                      ((symbol-function 'noema-agent-acp-adopt) #'ignore)
                      ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore))
              (+agent-shell--resume-from-list
               source '(((sessionId . "saved-native-id")
                         (updatedAt . "2026-09-30T00:00:00Z")))))
            (should (equal resumed-id "saved-native-id"))
            (should-not (buffer-live-p source))
            (should (eq (window-buffer (+portal-popup--window)) target))
            (should (equal (+portal-popup--live-buffers) (list target)))
            (should (equal (buffer-local-value '+portal-popup-workspace-id target)
                           "local-work")))
        (+portal/hide-popup)
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list source target))))))

(ert-deftest agent-shell-resume-keeps-manual-session-in-its-window ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *resume-manual-source*"))
          (target (generate-new-buffer " *resume-manual-target*"))
          started)
      (unwind-protect
          (let ((window (selected-window)))
            (set-window-buffer window source)
            (with-current-buffer source
              (setq major-mode 'agent-shell-mode
                    default-directory "/tmp/")
              (setq-local agent-shell--state
                          '((:agent-config . ((:identifier . codex)))
                            (:session . ((:id . "current")))))
              (setq-local noema-agent-acp-display-buffer-function
                          (lambda (buffer) (set-window-buffer window buffer))))
            (cl-letf (((symbol-function 'agent-shell--sort-sessions-by-recency) #'identity)
                      ((symbol-function 'agent-shell--session-title)
                       (lambda (_) "Saved conversation"))
                      ((symbol-function 'agent-shell--format-session-date)
                       (lambda (_) "today"))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices)))
                      ((symbol-function '+agent-shell--find-execution)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell--prompt-input) (lambda () nil))
                      ((symbol-function 'noema-sessions-native-binding)
                       (lambda (&rest _) nil))
                      ((symbol-function 'noema-agent-acp-start)
                       (lambda (&rest args)
                         (setq started args)
                         (with-current-buffer target
                           (setq major-mode 'agent-shell-mode
                                 default-directory "/tmp/")
                           (setq-local noema-agent-acp-display-buffer-function
                                       (plist-get args :display-function)))
                         target))
                      ((symbol-function 'noema-agent-acp-adopt) #'ignore)
                      ((symbol-function 'agent-shell-restart)
                       (lambda (&rest _) (ert-fail "Lost manual window"))))
              (+agent-shell--resume-from-list
               source '(((sessionId . "saved-native-id")
                         (updatedAt . "2026-09-30T00:00:00Z")))))
            (should (equal (plist-get started :session-id) "saved-native-id"))
            (should (eq (plist-get started :origin) 'manual))
            (should (eq (window-buffer window) target))
            (should-not (buffer-live-p source))
            (should (eq (buffer-local-value
                         'noema-agent-acp-display-buffer-function target)
                        (plist-get started :display-function))))
        (dolist (buffer (list source target))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest popup-tabs-and-current-buffer-span-workspaces ()
  (save-window-excursion
    (let ((work-a (generate-new-buffer " *popup-work-a*"))
          (work-b (generate-new-buffer " *popup-work-b*"))
          (term-a (generate-new-buffer " *popup-term-a*"))
          (term-b (generate-new-buffer " *popup-term-b*"))
          (term-new (generate-new-buffer " *popup-term-new*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil)
          (+portal-popup-origin-buffer nil)
          (+portal-popup--displaying t)
          created-in)
      (unwind-protect
          (let ((work-window (selected-window)))
            (with-current-buffer work-a (setq default-directory "/tmp/project-a/"))
            (with-current-buffer work-b (setq default-directory "/fs:box:/work/project-b/"))
            (with-current-buffer term-a
              (setq-local default-directory "/tmp/project-a/"
                          +portal-popup-workspace-id "local-a"))
            (with-current-buffer term-b
              (setq-local default-directory "/fs:box:/work/project-b/"
                          +portal-popup-workspace-id "box@remote-b"))
            (set-window-buffer work-window work-a)
            (+portal-popup-display-buffer term-a)
            (select-window work-window)
            (set-window-buffer work-window work-b)
            (+portal-popup-display-buffer term-b)
            (should (equal (+portal-popup--live-buffers)
                           (list term-a term-b)))
            (should (string-match-p "popup-term-a"
                                    (substring-no-properties
                                     (nth 1 (+portal-popup--tab-line)))))
            (should (string-match-p "popup-term-b"
                                    (substring-no-properties
                                     (nth 2 (+portal-popup--tab-line)))))
            (should (string-match-p "project-b@box"
                                    (substring-no-properties
                                     (nth 2 (+portal-popup--tab-line)))))
            (+portal-popup/cycle nil)
            (should (eq (window-buffer (+portal-popup--window)) term-a))
            (+portal-popup/select-index 2)
            (should (eq (window-buffer (+portal-popup--window)) term-b))
            (cl-letf (((symbol-function '+portal-popup--create-buffer)
                       (lambda (directory &rest _)
                         (setq created-in directory)
                         (with-current-buffer term-new
                           (setq-local +portal-popup-instance-p t))
                         term-new)))
              (+portal-popup/new))
            (should (equal created-in "/fs:box:/work/project-b/")))
        (+portal/hide-popup)
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list work-a work-b term-a term-b term-new))))))

(ert-deftest popup-buffer-can-move-in-and-out-with-its-own-ui ()
  (save-window-excursion
    (let ((work (generate-new-buffer " *popup-move-work*"))
          (term (generate-new-buffer " *popup-move-term*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil))
      (unwind-protect
          (let ((work-window (selected-window)))
            (with-current-buffer work
              (setq-local header-line-format "Work header"
                          tab-line-format "Work tabs"))
            (with-current-buffer term
              (setq-local +portal-popup-fixed t))
            (set-window-buffer work-window work)
            (+portal-popup-display-buffer term)
            (let ((+portal-popup--displaying t))
              (select-window work-window))
            (+portal-popup/move-buffer-in)
            (should (eq (window-buffer (+portal-popup--window)) work))
            (should (memq work (+portal-popup--live-buffers)))
            (with-current-buffer work
              (should (equal header-line-format "Work header"))
              (should-not (equal tab-line-format "Work tabs")))
            (+portal-popup/move-buffer-out)
            (should (eq (window-buffer work-window) work))
            (should (eq (window-buffer (+portal-popup--window)) term))
            (should (equal (+portal-popup--live-buffers) (list term)))
            (with-current-buffer work
              (should-not +portal-popup-instance-p)
              (should (equal header-line-format "Work header"))
              (should (equal tab-line-format "Work tabs"))))
        (+portal/hide-popup)
        (dolist (buffer (list work term))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest popup-terminal-and-agent-use-one-shared-tab-row ()
  "The terminal does not add a header above its popup tabs."
  (let ((terminal (generate-new-buffer " *popup-ui-terminal*"))
        (agent (generate-new-buffer " *popup-ui-agent*")))
    (unwind-protect
        (progn
          (with-current-buffer terminal
            (setq major-mode 'portal-terminal-mode)
            (setq-local tab-line-format "ordinary terminal tabs"
                        header-line-format "ordinary terminal header")
            (+portal-popup--save-ui)
            (+portal-popup-apply-ui terminal)
            (should (equal tab-line-format '(:eval (+portal-popup--tab-line))))
            (should tab-line-exclude)
            (should-not header-line-format)
            (+portal-popup--restore-ui)
            (should (equal tab-line-format "ordinary terminal tabs"))
            (should (equal header-line-format "ordinary terminal header")))
          (with-current-buffer agent
            (setq major-mode 'agent-shell-mode)
            (setq-local header-line-format "Native agent header")
            (+portal-popup-apply-ui agent)
            (should (equal tab-line-format '(:eval (+portal-popup--tab-line))))
            (should (equal header-line-format "Native agent header"))))
      (kill-buffer terminal)
      (kill-buffer agent))))

(ert-deftest popup-agent-move-updates-its-session-display-route ()
  (save-window-excursion
    (let ((agent (generate-new-buffer " *popup-move-agent*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil))
      (unwind-protect
          (let ((work-window (selected-window)))
            (with-current-buffer agent
              (setq major-mode 'agent-shell-mode)
              (setq-local header-line-format "Native agent header"
                          noema-agent-acp-session-agent "codex"
                          noema-agent-acp-display-buffer-function
                          (lambda (buffer)
                            (set-window-buffer work-window buffer))))
            (set-window-buffer work-window agent)
            (+portal-popup/move-buffer-in)
            (should (eq (window-buffer (+portal-popup--window)) agent))
            (with-current-buffer agent
              (should (eq noema-agent-acp-display-buffer-function
                          #'+portal-popup-display-buffer))
              (should (eq +portal-popup-kind 'ai-codex))
              (should (equal header-line-format "Native agent header")))
            (+portal-popup/move-buffer-out)
            (should (eq (window-buffer work-window) agent))
            (with-current-buffer agent
              (should-not +portal-popup-instance-p)
              (should-not +portal-popup-agent-keys-mode)
              (should (equal header-line-format "Native agent header"))
              (should (functionp noema-agent-acp-display-buffer-function))
              (should-not (eq noema-agent-acp-display-buffer-function
                              #'+portal-popup-display-buffer))))
        (+portal/hide-popup)
        (when (buffer-live-p agent) (kill-buffer agent))))))

(ert-deftest popup-buffer-window-picker-excludes-the-popup ()
  (save-window-excursion
    (let ((popup-buffer (generate-new-buffer " *popup-window-picker*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil)
          offered)
      (unwind-protect
          (let* ((first (selected-window))
                 (second (split-window-right)))
            (+portal-popup-display-buffer popup-buffer)
            (require 'ace-window)
            (cl-letf (((symbol-function 'aw-window-list)
                       (lambda () (window-list)))
                      ((symbol-function 'aw-select)
                       (lambda (&rest _)
                         (setq offered (aw-window-list))
                         second)))
              (should (eq (+portal-popup--choose-normal-window "Move")
                          second)))
            (should (memq first offered))
            (should (memq second offered))
            (should-not (memq (+portal-popup--window) offered)))
        (+portal/hide-popup)
        (when (buffer-live-p popup-buffer)
          (kill-buffer popup-buffer))))))

(ert-deftest popup-agent-transcript-policy-does-not-affect-other-buffers ()
  (let ((shell-maker-prompt-before-killing-buffer t))
    (with-temp-buffer
      (+portal-popup-apply-ui (current-buffer))
      (should shell-maker-prompt-before-killing-buffer)
      (should-not (local-variable-p 'shell-maker-prompt-before-killing-buffer)))
    (should shell-maker-prompt-before-killing-buffer)))

(defmacro popup-agent-test-with-placement (client &rest body)
  "Run BODY with every logical directory client-accessible when CLIENT."
  (declare (indent 1))
  `(cl-letf* ((directory-p (symbol-function 'file-directory-p))
              ((symbol-function 'remote-client-file-name)
               (lambda (path)
                 (and ,client (string-prefix-p "/fs:local:" path)
                      (substring path (length "/fs:local:")))))
              ;; Only the tests' fake workspaces; everything else is real.
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (cond ((string-match-p "unreachable" path) nil)
                       ((string-match-p "\\`\\(?:/Users/test/\\|/fs:\\)" path) t)
                       (t (funcall directory-p path))))))
     ,@body))

(ert-deftest agent-shell-opencode-prefers-verified-binary-for-client-agents ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (+agent-shell-opencode-executable "/test/official/opencode")
        (default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (path) (equal path "/test/official/opencode"))))
        (should (equal (+agent-shell-use-official-opencode
                        '(:command "opencode" :command-params ("acp") :context-buffer nil))
                       '(:command "/test/official/opencode" :command-params ("acp")
                                  :context-buffer nil)))
        ;; The global setting itself is never rewritten.
        (should (equal agent-shell-opencode-acp-command '("opencode" "acp")))))))

(ert-deftest agent-shell-opencode-remote-agent-uses-target-path ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (+agent-shell-opencode-executable "/test/official/opencode")
        (default-directory "/fs:server:/home/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t)))
        (should (equal (+agent-shell-use-official-opencode
                        '(:command "opencode" :command-params ("acp")))
                       '(:command "opencode" :command-params ("acp"))))))))

(ert-deftest agent-shell-opencode-preserves-custom-launchers-and-missing-install ()
  (let ((default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (let ((agent-shell-opencode-acp-command '("/custom/opencode" "acp" "--pure")))
        (cl-letf (((symbol-function 'file-executable-p)
                   (lambda (_) (ert-fail "Custom launcher was inspected"))))
          (should (equal (+agent-shell-use-official-opencode
                          '(:command "/custom/opencode" :command-params ("acp" "--pure")))
                         '(:command "/custom/opencode" :command-params ("acp" "--pure"))))))
      (let ((agent-shell-opencode-acp-command '("opencode" "acp")))
        (cl-letf (((symbol-function 'file-executable-p) (lambda (_) nil)))
          (should (equal (+agent-shell-use-official-opencode
                          '(:command "opencode" :command-params ("acp")))
                         '(:command "opencode" :command-params ("acp")))))))))

(ert-deftest agent-shell-opencode-expands-config-store-abbreviated-paths ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (+agent-shell-opencode-executable "~/.config/emacs/var/opencode")
        (default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t)))
        (should (equal (plist-get (+agent-shell-use-official-opencode
                                   '(:command "opencode" :command-params ("acp")))
                                  :command)
                       (expand-file-name +agent-shell-opencode-executable)))))))

(ert-deftest agent-shell-native-entrypoint-projects-logical-cwd ()
  ;; Call upstream's public CWD directly, without the Noema start wrapper.
  (let ((agent-shell-cwd-function (lambda () "/fs:local:/Users/test/project/")))
    (popup-agent-test-with-placement t
      (should (equal (agent-shell-cwd) "/Users/test/project/")))))

(ert-deftest agent-shell-process-directory-is-native-for-local-target ()
  (popup-agent-test-with-placement t
    (should (equal (+agent-shell-process-directory "/Users/test/project")
                   "/Users/test/project/"))
    (should (equal (+agent-shell-process-directory "/fs:local:/Users/test/project/")
                   "/Users/test/project/"))))

(ert-deftest agent-shell-process-directory-routes-remote-target ()
  ;; A target that shares nothing with the client keeps its logical identity,
  ;; so acp.el's `make-process' enters the /fs: handler and runs it there.
  (popup-agent-test-with-placement t
    (should (equal (+agent-shell-process-directory "/fs:server:/home/test/project")
                   "/fs:server:/home/test/project/"))))

(ert-deftest agent-shell-explicit-local-workspace-stays-local-from-remote-buffer ()
  "Agent placement and session keys must not borrow the source target."
  (let ((default-directory "/fs:server:/home/test/project/"))
    (popup-agent-test-with-placement t
      (should (equal (+agent-shell-process-directory "/Users/test/project")
                     "/Users/test/project/"))
      (should (equal (+agent-shell--session-context "/Users/test/project")
                     "/fs:local:/Users/test/project"))
      (should-not (+agent-shell-agent-file-name
                   "/Users/test/project/a.el"
                   (current-buffer))))))

(ert-deftest terminal-explicit-local-directory-stays-local-from-remote-buffer ()
  "Terminal and Portal identities use the requested directory's machine."
  (let ((default-directory "/fs:server:/home/test/project/"))
    (should (equal (aaron-terminal-normalize-directory "/Users/test/project")
                   "/Users/test/project/"))
    (should (equal (+terminal-context-key "/Users/test/project")
                   "local"))
    (should (equal (+terminal-context-key
                    "/fs:local:/Users/test/project")
                   "local"))
    (should (equal (aaron-terminal-home-directory
                    "/fs:local:/Users/test/project")
                   (file-name-as-directory
                    (expand-file-name "~" temporary-file-directory))))
    (should (equal (remote-context-target-id
                    (+portal-workspace-context "/Users/test/project"))
                   "local"))))

(ert-deftest agent-shell-path-resolver-is-identity-on-local-target ()
  (popup-agent-test-with-placement t
    (let ((default-directory "/Users/test/project/"))
      (should (equal (+agent-shell-resolve-path "/Users/test/project/a.el")
                     "/Users/test/project/a.el"))
      (should (equal (+agent-shell-resolve-path "/fs:local:/Users/test/project/a.el")
                     "/Users/test/project/a.el"))
      (should (equal (+agent-shell-resolve-path "relative.el") "relative.el")))))

(ert-deftest agent-shell-path-resolver-maps-remote-session-both-ways ()
  (popup-agent-test-with-placement t
    (let ((default-directory "/fs:server:/home/test/project/"))
      ;; Emacs -> agent: session/new cwd, mentions, diffs.
      (should (equal (+agent-shell-resolve-path "/fs:server:/home/test/project/")
                     "/home/test/project/"))
      ;; Agent -> Emacs: fs/read_text_file and fs/write_text_file.
      (should (equal (+agent-shell-resolve-path "/home/test/project/a.py")
                     "/fs:server:/home/test/project/a.py")))))

(ert-deftest agent-shell-requests-resolve-in-their-own-session ()
  (let ((session (generate-new-buffer " *agent-session-test*"))
        seen)
    (unwind-protect
        (progn
          (with-current-buffer session
            (setq default-directory "/fs:server:/home/test/"))
          (with-temp-buffer
            (+agent-shell-in-session-buffer-a
             (lambda (&rest _) (setq seen default-directory))
             :state (list (cons :buffer session)) :acp-request nil))
          (should (equal seen "/fs:server:/home/test/")))
      (kill-buffer session))))

(ert-deftest agent-shell-context-files-map-into-the-session-machine ()
  (popup-agent-test-with-placement t
    (let ((remote (generate-new-buffer " *remote-session*"))
          (local (generate-new-buffer " *local-session*")))
      (unwind-protect
          (progn
            (with-current-buffer remote
              (setq default-directory "/fs:server:/home/test/project/"))
            (with-current-buffer local
              (setq default-directory "/Users/test/project/"))
            ;; Same machine: the agent gets that machine's native path.
            (should (equal (+agent-shell-agent-file-name
                            "/fs:server:/home/test/project/a.py" remote)
                           "/home/test/project/a.py"))
            (should (equal (+agent-shell-agent-file-name
                            "/Users/test/project/a.py" local)
                           "/Users/test/project/a.py"))
            (should (equal (+agent-shell-agent-file-name
                            "/Users/test/project/a.py" nil)
                           "/Users/test/project/a.py"))
            ;; Different machine: unreachable, never a wrong path.
            (should-not (+agent-shell-agent-file-name
                         "/Users/test/project/a.py" remote))
            (should-not (+agent-shell-agent-file-name
                         "/fs:server:/home/test/project/a.py" local)))
        (kill-buffer remote)
        (kill-buffer local)))))

(ert-deftest popup-agent-accepts-a-remote-workspace ()
  ;; Placement belongs to the agent boundary; the popup does not branch on it.
  (let ((work (generate-new-buffer " *popup-remote-work*")) started)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer work
            (setq default-directory "/fs:server:/home/test/project/src/"))
          (set-window-buffer (selected-window) work)
    (cl-letf (((symbol-function 'noema-agent-acp-config-for) (lambda (_) 'config))
              ((symbol-function 'noema-agent-acp-start)
               (lambda (&rest args) (setq started (plist-get args :directory))
                 (generate-new-buffer " *popup-remote-agent*")))
              ((symbol-function 'noema-agent-acp-adopt) #'ignore)
              ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore)
              ((symbol-function '+portal-popup-display-buffer) #'ignore)
              ((symbol-function '+portal-popup--requested-workspace-id) #'ignore)
              ((symbol-function '+project-current-root)
               (lambda () "/fs:server:/home/test/project/")))
      ;; From a file below the project, the agent starts at the project root.
      (+portal-popup/agent 'codex)
      (should (equal started "/fs:server:/home/test/project/"))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p " *popup-remote-agent*" (buffer-name buffer))
          (kill-buffer buffer)))))
      (kill-buffer work))))

(ert-deftest popup-agent-launches-from-last-focused-work-buffer ()
  (save-window-excursion
    (let ((work (generate-new-buffer " *popup-focused-work*"))
          (term (generate-new-buffer " *popup-focused-term*"))
          (agent (generate-new-buffer " *popup-focused-agent*"))
          (+portal-popup-buffers nil)
          (+portal-popup-current-buffer nil)
          (+portal-popup-origin-buffer nil)
          (+portal-popup--displaying t)
          started)
      (unwind-protect
          (progn
            (with-current-buffer work
              (setq default-directory "/fs:box:/work/project/src/"))
            (with-current-buffer term
              (setq-local +portal-popup-workspace-id "box-work"))
            (set-window-buffer (selected-window) work)
            (+portal-popup-display-buffer term)
            (cl-letf (((symbol-function '+project-current-root)
                       (lambda ()
                         (should (eq (current-buffer) work))
                         "/fs:box:/work/project/"))
                      ((symbol-function '+portal-popup--requested-workspace-id)
                       (lambda (&rest _) "box-work"))
                      ((symbol-function 'noema-agent-acp-config-for)
                       (lambda (_) 'config))
                      ((symbol-function 'noema-agent-acp-start)
                       (lambda (&rest args)
                         (setq started (plist-get args :directory))
                         agent))
                      ((symbol-function 'noema-agent-acp-adopt) #'ignore)
                      ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore))
              (+portal-popup/agent 'claude))
            (should (equal started "/fs:box:/work/project/"))
            (should (eq (window-buffer (+portal-popup--window)) agent)))
        (+portal/hide-popup)
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list work term agent))))))

(ert-deftest popup-agent-uses-noema-project-workspace-and-session-root ()
  (save-window-excursion
    (let ((work (generate-new-buffer " *popup-noema-work*"))
          (agent (generate-new-buffer " *popup-noema-agent*"))
          started adopted-root)
      (unwind-protect
          (progn
            (with-current-buffer work
              (setq default-directory "/tmp/noema-project/notes/"))
            (set-window-buffer (selected-window) work)
            (cl-letf (((symbol-function 'noema-project-root)
                       (lambda (directory)
                         (should (equal directory "/tmp/noema-project/notes/"))
                         "/tmp/noema-project/"))
                      ((symbol-function 'noema-project-workspace)
                       (lambda (root)
                         (should (equal root "/tmp/noema-project/"))
                         "/tmp/code-workspace/"))
                      ((symbol-function '+project-current-root)
                       (lambda () (ert-fail "Ignored Noema workspace")))
                      ((symbol-function '+portal-popup--requested-workspace-id)
                       (lambda (&rest _) "local-code"))
                      ((symbol-function 'noema-agent-acp-config-for)
                       (lambda (_) 'config))
                      ((symbol-function 'noema-agent-acp-start)
                       (lambda (&rest args)
                         (setq started (plist-get args :directory))
                         agent))
                      ((symbol-function 'noema-agent-acp-adopt)
                       (lambda (_buffer &rest args)
                         (setq adopted-root (plist-get args :root))))
                      ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore)
                      ((symbol-function '+portal-popup-display-buffer) #'ignore))
              (+portal-popup/agent 'claude))
            (should (equal started "/tmp/code-workspace/"))
            (should (equal adopted-root "/tmp/noema-project/")))
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list work agent))))))

(ert-deftest agent-shell-process-directory-refuses-an-unreachable-workspace ()
  ;; shell-maker would silently fall back to the local home directory.
  (popup-agent-test-with-placement t
    (should-error (+agent-shell-process-directory "/fs:unreachable:/srv/")
                  :type 'user-error)))

(ert-deftest agent-shell-client-gets-its-workspace-environment ()
  "A natively spawned agent gets its workspace capsule in its buffer.
A routed agent gets it from the process route instead: its buffer keeps this
machine's HOME, because agent-shell's caches expand `~' there."
  (let (ensured +agent-shell--lookup-target)
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'remote-environment-ensure)
                 (lambda (context &rest _) (setq ensured context)))
                ((symbol-function 'remote-context)
                 (lambda (dir)
                   (remote-context-create
                    :target-id (if (string-prefix-p "/fs:server:" dir) "server" "local")
                    :localname dir))))
        (with-temp-buffer
          (setq default-directory "/Users/test/project/")
          (+agent-shell-apply-workspace-environment)
          (should (equal (remote-context-localname ensured) "/Users/test/project/"))
          (should (equal +agent-shell--lookup-target "local"))
          ;; Already projected: not resolved again.
          (setq ensured nil)
          (setq-local remote-buffer-environment 'capsule)
          (+agent-shell-apply-workspace-environment)
          (should-not ensured))
        (with-temp-buffer
          (setq default-directory "/fs:server:/home/test/project/")
          (+agent-shell-apply-workspace-environment)
          (should-not ensured)
          (should (equal +agent-shell--lookup-target "server")))))
      ;; The missing-executable report names that target even after
      ;; agent-shell has killed the shell buffer.
      (should (string-match-p "target `server'"
                              (+agent-shell-missing-executable-a "not found")))))

(ert-deftest popup-agent-outside-a-project-starts-here ()
  (let ((work (generate-new-buffer " *popup-scratch-work*")) started)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer work
            (setq default-directory "/fs:server:/home/test/scratch/"))
          (set-window-buffer (selected-window) work)
    (cl-letf (((symbol-function 'noema-agent-acp-config-for) (lambda (_) 'config))
              ((symbol-function 'noema-agent-acp-start)
               (lambda (&rest args) (setq started (plist-get args :directory))
                 (generate-new-buffer " *popup-remote-agent*")))
              ((symbol-function 'noema-agent-acp-adopt) #'ignore)
              ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore)
              ((symbol-function '+portal-popup-display-buffer) #'ignore)
              ((symbol-function '+portal-popup--requested-workspace-id) #'ignore)
              ((symbol-function '+project-current-root) #'ignore))
      (+portal-popup/agent 'codex)
      (should (equal started "/fs:server:/home/test/scratch/"))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p " *popup-remote-agent*" (buffer-name buffer))
          (kill-buffer buffer)))))
      (kill-buffer work))))

(ert-deftest popup-agent-launchers-are-memory-only ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) (ert-fail "Header checked executables")))
            ((symbol-function 'noema-agent-acp-start) (lambda (&rest _) (ert-fail "Header started agent"))))
    (let* ((term (+portal-popup--new-tab-segment))
           (agent (+portal-popup--agent-tab-segment))
           (term-map (get-text-property 0 'local-map term))
           (agent-map (get-text-property 0 'local-map agent)))
      (should (eq (lookup-key term-map [header-line mouse-3]) #'+portal-popup/menu))
      (should (eq (lookup-key agent-map [tab-line mouse-1]) #'+portal-popup/agent-menu))
      (should (equal (mapcar (lambda (item) (aref item 0)) (+portal-popup--agent-menu-items))
                     '("Claude" "Codex" "OpenCode"))))))

(ert-deftest popup-agent-menu-includes-configured-apps-and-agent-submenu ()
  (let ((+project-popup-portal-apps '(("yazi" . "yazi") ("btop" . "btop"))) items)
    (cl-letf (((symbol-function 'easy-menu-create-menu) (lambda (_title entries) (setq items entries)))
              ((symbol-function 'popup-menu) #'ignore))
      (+portal-popup/menu nil))
    (should (equal (mapcar (lambda (item) (aref item 0)) (cdr (assoc "Applications" items))) '("yazi" "btop")))
    (should (= (length (cdr (assoc "Agent" items))) 3))))

(ert-deftest popup-agent-starts-acp-not-portal-and-shares-the-real-popup ()
  (save-window-excursion
    (let ((+portal-popup-buffers nil) (+portal-popup-current-buffer nil)
          (+portal-popup--displaying t) (+portal-popup-window-height 0.3)
          (agent (generate-new-buffer " *popup-test-agent*"))
          (term (generate-new-buffer " *popup-test-terminal*")) started)
      (unwind-protect
          (cl-letf (((symbol-function '+portal-workspace-id) (lambda (&rest _) "local"))
                    ((symbol-function 'noema-agent-acp-config-for) (lambda (id) (list id)))
                    ((symbol-function 'noema-agent-acp-start)
                     (lambda (&rest args)
                       (setq started args)
                       (with-current-buffer agent
                         (setq major-mode 'agent-shell-mode)
                         (setq-local header-line-format "native ACP header")
                         (noema-agent-acp-tabs-mode 1))
                       agent))
                    ((symbol-function '+portal-popup--create-buffer) (lambda (&rest _) (ert-fail "Agent used portal")))
                    ((symbol-function 'noema-agent-render-flush) #'ignore))
            (+portal-popup-display-buffer term)
            (let ((window (+portal-popup--window)))
              (should (eq (+portal-popup/agent 'codex) agent))
              (should (equal (plist-get started :config) '(codex)))
              (should-not (plist-get started :focus))
              (should (eq (+portal-popup--window) window))
              (should (eq (window-buffer window) agent))
              (with-current-buffer agent
                (should (eq major-mode 'agent-shell-mode))
                (should +portal-popup-instance-p)
                (should-not noema-agent-acp-tabs-mode)
                (should (equal header-line-format "native ACP header"))
                (should (eq (key-binding (kbd "C-c C-e")) #'+portal-popup/toggle))
                (should (eq (key-binding (kbd "C-c E")) #'+portal-popup/cycle))
                (should (eq (key-binding (kbd "C-c M-e")) #'+portal/toggle-fixed)))
              ;; ACP redisplay must not move the popup session to the project window.
              (noema-agent-acp-show-buffer agent)
              (should (eq (+portal-popup--window) window))
              (+portal-popup/cycle nil)
              (should (eq (window-buffer window) term))
              (+portal-popup/cycle nil)
              (should (eq (window-buffer window) agent))
              (+portal-popup/toggle)
              (should-not (+portal-popup--window))
              (should (buffer-live-p agent))
              (+portal-popup/toggle)
              (should (eq (window-buffer (+portal-popup--window)) agent))
              (+portal/toggle-fixed)
              (should (buffer-local-value '+portal-popup-fixed agent))))
        (+portal/hide-popup)
        (kill-buffer agent)
        (kill-buffer term)))))

(ert-deftest popup-agent-entrypoints-select-the-corresponding-adapter ()
  (let (started)
    (cl-letf (((symbol-function '+portal-popup/agent) (lambda (id) (push id started))))
      (+portal-popup/agent-claude)
      (+portal-popup/agent-codex)
      (+portal-popup/agent-opencode))
    (should (equal (nreverse started) '(claude codex opencode)))))

(ert-deftest agent-shell-interactive-entry-uses-managed-window ()
  (let (managed original)
    (cl-letf (((symbol-function '+agent-shell/here)
               (lambda () (interactive) (setq managed t)))
              ((symbol-function 'agent-shell--dwim)
               (lambda (&rest _) (setq original t))))
      (call-interactively #'agent-shell)
      (should managed)
      (should-not original)
      (setq managed nil)
      (agent-shell)
      (should managed)
      (should-not original)
      (setq managed nil)
      (agent-shell '(16))
      (should original)
      (should-not managed))))

(ert-deftest agent-shell-manual-entry-displays-in-invoking-window ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *agent-shell-source*"))
          (agent-buffer (generate-new-buffer " *agent-shell-manual*"))
          started adopted)
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) source)
            (let ((window (selected-window)))
              (cl-letf (((symbol-function '+agent-shell-launch-context)
                         (lambda () (cons "/tmp/project/" "/tmp/project/code/")))
                        ((symbol-function 'noema-agent-acp-config-for)
                         (lambda (agent) (list :identifier agent)))
                        ((symbol-function 'noema-agent-acp-start)
                         (lambda (&rest args) (setq started args) agent-buffer))
                        ((symbol-function 'noema-agent-acp-adopt)
                         (lambda (buffer &rest args)
                           (setq adopted args) buffer)))
                (should (eq (+agent-shell/here 'codex) agent-buffer))
                (should (eq (selected-window) window))
                (should (eq (window-buffer window) agent-buffer))
                (should (equal (plist-get started :directory)
                               "/tmp/project/code/"))
                (should (eq (plist-get started :origin) 'manual))
                (should-not (plist-get started :focus))
                (should (functionp (plist-get started :display-function)))
                (should (equal (plist-get adopted :root) "/tmp/project/")))))
        (when (buffer-live-p source) (kill-buffer source))
        (when (buffer-live-p agent-buffer) (kill-buffer agent-buffer))))))

(ert-deftest agent-shell-adapters-run-the-workspace-cli-without-fallback ()
  "Claude/Codex adapters get the CLI from the workspace PATH, or fail."
  (let ((+agent-shell--lookup-target "local"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &optional _remote)
                 (and (equal program "claude") "/home/me/.local/bin/claude"))))
      (should (equal (plist-get (+agent-shell-use-workspace-cli
                                 '(:command "claude-agent-acp"
                                   :environment-variables ("A=1")))
                                :environment-variables)
                     '("CLAUDE_CODE_EXECUTABLE=/home/me/.local/bin/claude" "A=1")))
      (should-error (+agent-shell-use-workspace-cli '(:command "codex-acp"))
                    :type 'user-error)
      (should (equal (+agent-shell-use-workspace-cli '(:command "pi-acp"))
                     '(:command "pi-acp"))))))

;;; popup-agent-tests.el ends here
