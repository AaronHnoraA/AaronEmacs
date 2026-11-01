;;; +agent-acp-tests.el --- Tests for Noema's ACP boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tools/agent/acp)

(ert-deftest +agent-acp-export-model-does-not-change-interactive-preference ()
  (let ((saved (make-hash-table :test #'equal))
        (original '((:identifier . codex))))
    (puthash "codex" "gpt-6-astra" saved)
    (cl-letf (((symbol-function '+agent-acp--read-models) (lambda () saved))
              ((symbol-function '+agent-acp-available-model-ids)
               (lambda (&optional _) '("gpt-5.6-terra" "gpt-6-astra"))))
      (let* ((export (noema-agent-export-config original "gpt-5.6-terra"))
             (started (plist-get
                       (+agent-acp--model-config (list :config export)) :config)))
        (should (equal (funcall (alist-get :default-model-id started))
                       "gpt-5.6-terra"))
        (should (equal (funcall (alist-get :default-config-options started))
                       '(("reasoning_effort" . "medium"))))
        (should (equal (gethash "codex" saved) "gpt-6-astra"))
        (should-not (alist-get :agent-pinned-model original))))))

(ert-deftest +agent-acp-pinned-model-is-isolated-both-ways ()
  (let ((saved (make-hash-table :test #'equal))
        (original '((:identifier . codex)
                    (:default-config-options . (("reasoning_effort" . "high")
                                                ("mode" . "auto"))))))
    (puthash "codex" "gpt-6-astra" saved)
    (cl-letf (((symbol-function '+agent-acp--read-models) (lambda () saved))
              ((symbol-function '+agent-acp-available-model-ids)
               (lambda (&optional _) '("gpt-6.1-terra" "gpt-6-astra")))
              ((symbol-function '+agent-acp--remember-model)
               (lambda (id model) (puthash (format "%s" id) model saved))))
      (let* ((pinned (+agent-acp-pinned-config original "gpt-6.1-terra" "xhigh"))
             (started (plist-get
                       (+agent-acp--model-config (list :config pinned)) :config)))
        ;; The remembered interactive model does not reach the pinned session.
        (should (equal (funcall (alist-get :default-model-id started)) "gpt-6.1-terra"))
        (should (equal (funcall (alist-get :default-config-options started))
                       '(("reasoning_effort" . "xhigh") ("mode" . "auto"))))
        ;; A model change inside the pinned session is not remembered.
        (cl-letf (((symbol-function 'agent-shell--state)
                   (lambda () `((:agent-config . ,pinned)))))
          (with-temp-buffer
            (+agent-acp--remember-interactive-model 'codex "gpt-6.1-sol")))
        (should (equal (gethash "codex" saved) "gpt-6-astra"))
        (should (equal (alist-get :default-config-options original)
                       '(("reasoning_effort" . "high") ("mode" . "auto"))))))))

(ert-deftest +agent-acp-claude-export-keeps-other-options-at-medium-effort ()
  (let* ((original '((:identifier . claude-code)
                     (:default-config-options . noema-test-claude-options)))
         (copy nil))
    (cl-letf (((symbol-function 'noema-test-claude-options)
               (lambda () '(("mode" . "auto") ("effort" . "high")))))
      (setq copy (noema-agent-export-config original "sonnet"))
      (should (equal (funcall (alist-get :default-config-options copy))
                     '(("effort" . "medium") ("mode" . "auto"))))
      (should (eq (alist-get :default-config-options original)
                  'noema-test-claude-options)))))

(ert-deftest +agent-acp-claude-export-does-not-persist-or-load-global-settings ()
  (let* ((original '((:identifier . claude-code)
                     (:session-meta . ((claudeCode . ((options . ((thinking . ((type . "adaptive")))))))))))
         (copy (noema-agent-export-config original "sonnet"))
         (options (alist-get 'options (alist-get 'claudeCode (alist-get :session-meta copy)))))
    (should (eq (alist-get 'persistSession options) :false))
    (should (equal (alist-get 'settingSources options) []))
    (should (equal (alist-get 'skills options) []))
    (should (equal (alist-get 'tools options) ["Read" "Edit" "Write" "Glob" "Grep"]))
    (should (equal (alist-get 'disallowedTools options) ["Bash" "Task" "Skill" "WebFetch" "WebSearch"]))
    (should (equal (alist-get 'thinking options) '((type . "adaptive"))))
    (should-not (alist-get 'persistSession
                           (alist-get 'options (alist-get 'claudeCode (alist-get :session-meta original)))))))

(ert-deftest +agent-acp-frozen-context-uses-utf8-and-text-fallback ()
  (let* ((item `(("ref" . "skill:test")
                 ("resolvedUri" . "noema://skill/test/1")
                 ("contentBase64" . ,(base64-encode-string
                                       (encode-coding-string "中文" 'utf-8) t))
                 ("mediaType" . "text/plain; charset=utf-8")))
         (buffer (current-buffer)))
    (cl-letf (((symbol-function '+agent-acp-state-value) (lambda (&rest _) t)))
      (let ((blocks (+agent-acp-context-blocks buffer "Prompt" (list item))))
        (should (equal (alist-get 'text (alist-get 'resource (cadr blocks))) "中文"))))
    (cl-letf (((symbol-function '+agent-acp-state-value) (lambda (&rest _) nil)))
      (let ((text (alist-get 'text (car (+agent-acp-context-blocks
                                       buffer "Prompt" (list item))))))
        (should (string-match-p "skill:test" text))
        (should (string-suffix-p "中文" text))))))

(ert-deftest +agent-acp-cleans-only-the-private-codex-export-rollout ()
  (let* ((root (make-temp-file "noema-export-codex-home-" t))
         (process-environment (cons (concat "CODEX_HOME=" root) process-environment))
         (sessions (expand-file-name "sessions/2026/10/21" root))
         (id "98b3f858-4a19-4f24-b3f0-9afbecb0fdcb")
         (target (expand-file-name (concat "rollout-2026-10-21-" id ".jsonl") sessions))
         (other (expand-file-name "rollout-2026-10-21-00000000-0000-0000-0000-000000000000.jsonl" sessions)))
    (unwind-protect
        (progn
          (make-directory sessions t)
          (with-temp-file target (insert "private export"))
          (with-temp-file other (insert "ordinary session"))
          (noema-agent-cleanup-export-history id)
          (should-not (file-exists-p target))
          (should (file-exists-p other)))
      (delete-directory root t))))

(ert-deftest +agent-acp-export-buffer-schedules-only-its-codex-rollout-cleanup ()
  (let ((buffer (generate-new-buffer " *private-codex-export*"))
        (id "98b3f858-4a19-4f24-b3f0-9afbecb0fdcb")
        scheduled)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local +agent-acp-session-origin 'latex-export))
          (cl-letf (((symbol-function '+agent-acp--project-buffers) (lambda (&rest _) nil))
                    ((symbol-function '+agent-acp--buffer-agent-id) (lambda (&rest _) "codex"))
                    ((symbol-function '+agent-acp-state-value) (lambda (&rest _) id))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (setq scheduled (cons function args)))))
            (+agent-acp-kill buffer))
          (should-not (buffer-live-p buffer))
          (should (equal scheduled
                         (cons #'noema-agent-cleanup-export-history
                               (list id)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +agent-acp-export-model-ack-does-not-save-preference ()
  (let (saved)
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () '((:agent-config . ((:identifier . codex)
                                               (:agent-pinned-model . "gpt-5.6-terra"))))))
              ((symbol-function 'agent-shell--current-model-id)
               (lambda (_) "gpt-5.6-terra"))
              ((symbol-function '+agent-acp--remember-model)
               (lambda (&rest args) (setq saved args))))
      (+agent-acp--set-model
       (lambda (&rest args) (funcall (plist-get args :on-success)))
       :model-id "gpt-5.6-terra")
      (should-not saved))))

(ert-deftest +agent-acp-turn-complete-forwards-one-public-event ()
  (let ((buffer (generate-new-buffer " *noema-turn-event*"))
        (subscriptions nil)
        (events nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'agent-shell-mode)
          (let ((+agent-acp-turn-complete-functions
                 (list (lambda (source event)
                         (push (list source event) events)))))
            (cl-letf (((symbol-function '+agent-acp-subscribe)
                       (lambda (&rest args)
                         (push args subscriptions)
                         'mock-subscription))
                      ((symbol-function '+agent-acp--note-attention)
                       (lambda (&rest _) nil)))
              (+agent-acp--subscribe-attention buffer)
              (+agent-acp--subscribe-attention buffer)
              (should (= (length subscriptions) 5))
              (let ((callback (plist-get (car subscriptions) :callback))
                    (event '((:event . turn-complete)
                             (:data . ((:stop-reason . "end_turn"))))))
                (funcall callback event)
                (should (equal events (list (list buffer event))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +agent-acp-turn-complete-watches-bare-claude-shell ()
  "An unadopted Claude agent-shell still subscribes to turn completion."
  (let ((buffer (generate-new-buffer " *bare-claude-shell*"))
        (+agent-acp-adopt-foreign-sessions nil)
        subscriptions)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'agent-shell-mode)
          (should (memq #'+agent-acp--watch-attention-h
                        agent-shell-mode-hook))
          (cl-letf (((symbol-function '+agent-acp-subscribe)
                     (lambda (&rest args)
                       (push (plist-get args :event) subscriptions)
                       'mock-subscription)))
            (+agent-acp--watch-attention-h)
            (should (memq 'permission-request subscriptions))
            (should (memq 'turn-complete subscriptions))
            (+agent-acp--watch-attention-h)
            (should (= (length subscriptions) 5))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +agent-acp-renamed-shell-restores-real-input-and-submits ()
  "Exercise the real shell process, prompt filter, Evil keys and input sender."
  (save-window-excursion
    (let* ((directory (make-temp-file "noema-native-input-" t))
           (shell-maker-root-path directory)
           (agent-shell-mode-hook nil)
           (original-name (generate-new-buffer-name " *native-input-test*"))
           (buffer (shell-maker-start-v2
                    :config (agent-shell--make-shell-maker-config
                             :prompt "OpenCode> " :prompt-regexp "OpenCode> ")
                    :buffer-name original-name :no-focus t :new-session t
                    :alias-commands nil)))
      (unwind-protect
          (with-current-buffer buffer
            ;; This is shell-maker's local placeholder process. Its default
            ;; termination notice cannot be inserted into read-only output.
            (set-process-sentinel (get-buffer-process buffer) #'ignore)
            (setq-local agent-shell--state (agent-shell--make-state :buffer buffer)
                        +agent-render-policy 'always)
            (setf (alist-get :id (alist-get :session agent-shell--state)) "session")
            (+agent-acp-mark-session-buffer buffer "input-test" "opencode" directory)
            (should (eq (shell-maker-buffer shell-maker--config) buffer))
            (should (eq (shell-maker--process) (get-buffer-process buffer)))
            (agent-shell--update-text :state agent-shell--state :block-id "answer"
                                      :text "\nFinished answer.\n" :create-new t)
            ;; agent-shell >= 0.79 inserts output above the prompt, so a
            ;; plain reply leaves the input prompt live.
            (should (+agent-acp--live-input-p))
            ;; Reproduce an already-open session with the old stale mapping
            ;; and busy flag.  Focus input must repair these as well.
            (setq-local shell-maker--buffer-name-override original-name
                        shell-maker--busy t
                        +agent-acp--prompt-owed nil)
            (switch-to-buffer buffer)
            (evil-local-mode 1)
            (evil-normal-state)
            (dolist (state '(normal insert visual motion))
              (evil-change-state state)
              (should (eq (key-binding (kbd "C-c C-e")) #'+agent-acp-focus-input)))
            (evil-normal-state)
            (cl-letf (((symbol-function '+agent-acp-show-buffer)
                       (lambda (target) (switch-to-buffer target))))
              (call-interactively (key-binding (kbd "C-c C-e"))))
            (should (eq evil-state 'insert))
            (should (= (point) (point-max)))
            (should (agent-shell--live-input-prompt-p comint-last-prompt))
            (should-not shell-maker--busy)
            (should-not (get-buffer original-name))
            (insert "Please explain the second point.")
            (let (submitted)
              (cl-letf (((symbol-function 'shell-maker--eval-input-on-buffer-v2)
                         (lambda (&rest args) (setq submitted (plist-get args :input)))))
                (call-interactively (key-binding (kbd "RET"))))
              (should (equal submitted "Please explain the second point.")))
            (shell-maker-finish-output :config shell-maker--config :success t)
            ;; Successful, cancelled and failed ACP replies must all restore
            ;; native input, including when their output was deferred.
            (dolist (outcome '("end_turn" "cancelled" "failed"))
              (setq-local +agent-render-policy 'never)
              (let (wire)
                (cl-letf (((symbol-function 'acp-send-request)
                           (lambda (&rest args) (setq wire args))))
                  (+agent-acp-prompt :buffer buffer :run-id outcome
                                          :content '(((type . "text") (text . "Next request")))))
                (agent-shell--update-text :state agent-shell--state :block-id "reply"
                                          :text "\nNext reply\n" :create-new t)
                (should (+agent-acp-request-active-p buffer))
                (if (equal outcome "failed")
                    (funcall (plist-get wire :on-failure) "offline" '((message . "offline")))
                  (funcall (plist-get wire :on-success) `((stopReason . ,outcome))))
                (should +agent-acp--prompt-owed)
                (should +agent-render--pending)
                (+agent-render-flush buffer)
                (should-not +agent-acp--prompt-owed)
                (should (+agent-acp--live-input-p))
                (save-excursion
                  (goto-char (point-max))
                  (let ((status (pcase outcome
                                  ("failed" "Failed") ("cancelled" "Stopped") (_ "Completed"))))
                    (should (search-backward status nil t))
                    (should (< (point) (marker-position (car comint-last-prompt))))
                    (should (search-backward "Next reply" nil t))))
                (let ((end (point-max)))
                  (+agent-acp-restore-prompt buffer)
                  (should (= end (point-max)))))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((process (get-buffer-process buffer))) (delete-process process))
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
        (when-let* ((orphan (get-buffer original-name))) (kill-buffer orphan))
        (delete-directory directory t)))))

(ert-deftest +agent-acp-submitted-input-is-visible-and-preserves-native-draft ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                +agent-render-policy 'always
                comint-last-output-start (copy-marker 1))
    (insert "OpenCode> ")
    (setq-local comint-last-prompt (cons (copy-marker 1) (copy-marker (point))))
    (insert "unfinished draft")
    (let ((state `((:buffer . ,(current-buffer)) (:request-count . 1))))
      (cl-letf (((symbol-function 'agent-shell--render-markdown) #'ignore))
        (+agent-acp--display-submitted-content
         state '(((type . "text") (text . "Review this proof."))) "first"))
      (should (string-match-p "Review this proof" (buffer-string)))
      (should (equal (buffer-substring-no-properties (cdr comint-last-prompt) (point-max))
                     "unfinished draft"))
      (should (agent-shell--live-input-prompt-p comint-last-prompt)))))

(ert-deftest +agent-acp-mouse-model-selection-is-remembered-after-ack ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                '((:agent-config . ((:identifier . opencode)))))
    (let (request saved)
      (cl-letf (((symbol-function 'agent-shell--current-model-id) (lambda (_) "provider/chosen"))
                ((symbol-function '+agent-acp--remember-model)
                 (lambda (id model) (setq saved (list id model)))))
        (+agent-acp--set-model
         (lambda (&rest args) (setq request args)) :model-id "provider/chosen")
        (should-not saved)
        (with-temp-buffer (funcall (plist-get request :on-success)))
        (should (equal saved '(opencode "provider/chosen")))))))

(ert-deftest +agent-acp-model-change-reaches-acp-and-updates-session ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                agent-shell--state
                `((:buffer . ,(current-buffer))
                  (:agent-config . ((:identifier . opencode)))
                  (:session . ((:id . "s") (:model-id . "opencode/big-pickle")))))
    (let (wire saved)
      (cl-letf (((symbol-function 'acp-send-request) (lambda (&rest args) (setq wire args)))
                ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
                ((symbol-function '+agent-acp--remember-model)
                 (lambda (id model) (setq saved (list id model)))))
        ;; Same upstream setter used by the model menu, including its RPC.
        (agent-shell--config-option-set-model-id :model-id "provider/chosen")
        (should (equal (map-nested-elt (plist-get wire :request) '(:params modelId))
                       "provider/chosen"))
        (should-not saved)
        (funcall (plist-get wire :on-success) nil)
        (should (equal (agent-shell--current-model-id agent-shell--state) "provider/chosen"))
        (should (equal saved '(opencode "provider/chosen")))))))

(ert-deftest +agent-acp-prompt-displays-input-and-sends-exact-blocks-once ()
  (with-temp-buffer
    (setq-local shell-maker--current-request-id 7)
    (setq-local agent-shell--state
                `((:buffer . ,(current-buffer)) (:request-count . 7)
                  (:last-entry-type . "agent_message_chunk")
                  (:session . ((:id . "selected-session") (:model-id . "provider/chosen")))))
    (let ((content '(((type . "text") (text . "Review this proof."))
                     ((type . "resource") (resource . ((uri . "file:///proof") (text . "frozen context"))))))
          displayed requests)
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args)
                   (when (equal (plist-get args :block-id) "submitted-input")
                     (setq displayed args))))
                ((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (should displayed)
                   (push (plist-get args :request) requests))))
        (+agent-acp-prompt :buffer (current-buffer) :run-id "run-1" :content content)
        (should (= 1 (length requests)))
        (should (equal (map-elt (car requests) :method) "session/prompt"))
        (should (equal (map-nested-elt (car requests) '(:params sessionId)) "selected-session"))
        (should (equal (map-nested-elt (car requests) '(:params prompt)) (vconcat content)))
        (should (string-match-p "Review this proof" (plist-get displayed :body)))
        (should (plist-get displayed :above-last-prompt))
        (should (= (map-elt agent-shell--state :request-count) 8))
        (should-not (map-elt agent-shell--state :last-entry-type))
        (should (equal (map-nested-elt agent-shell--state '(:session :model-id)) "provider/chosen"))))))

(ert-deftest +agent-acp-model-memory-is-per-agent-and-validates-availability ()
  (let* ((directory (make-temp-file "noema-models-" t))
         (+agent-acp-model-preferences-file (expand-file-name "models.json" directory)))
    (unwind-protect
        (progn
          (+agent-acp--remember-model 'opencode "provider/fast")
          (+agent-acp--remember-model 'codex "another")
          (should (equal (gethash "opencode" (+agent-acp--read-models)) "provider/fast"))
          (let* ((args (+agent-acp--model-config '(:config ((:identifier . opencode)))))
                 (default (map-elt (plist-get args :config) :default-model-id)))
            (cl-letf (((symbol-function 'agent-shell--state) #'ignore)
                      ((symbol-function 'agent-shell--get-available-models)
                       (lambda (_) '(((:model-id . "provider/fast"))))))
              (should (equal (funcall default) "provider/fast")))
            (cl-letf (((symbol-function 'agent-shell--state) #'ignore)
                      ((symbol-function 'agent-shell--get-available-models) #'ignore))
              (should-not (funcall default)))))
      (delete-directory directory t))))

(ert-deftest +agent-acp-model-memory-records-only-successful-selection ()
  (let (success saved)
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:agent-config . ((:identifier . opencode))))))
              ((symbol-function 'agent-shell--current-model-id) (lambda (_) "fast"))
              ((symbol-function 'noema-agent-worker-buffer-busy-p) #'ignore)
              ((symbol-function '+agent-acp--remember-model) (lambda (id model) (setq saved (list id model)))))
      (+agent-acp--select-model (lambda (callback) (setq success callback)))
      (should-not saved)
      (funcall success)
      (should (equal saved '(opencode "fast"))))))

(ert-deftest +agent-acp-model-switch-rejects-an-active-request ()
  (cl-letf (((symbol-function 'agent-shell--state) (lambda () '((:active-requests . (prompt))))))
    (should-error (+agent-acp--select-model (lambda (&rest _) (ert-fail "changed busy model"))) :type 'user-error)))

(ert-deftest +agent-acp-pool-model-change-validates-and-uses-acp ()
  (with-temp-buffer
    (setq-local agent-shell--state '((:session . ((:id . "session-1")))))
    (let (selected)
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () agent-shell--state))
                ((symbol-function '+agent-acp-available-model-ids)
                 (lambda (&optional _) '("provider/one" "provider/two")))
                ((symbol-function '+agent-acp-current-model-id)
                 (lambda (&optional _) "provider/one"))
                ((symbol-function 'agent-shell--config-option-set-model-id)
                 (lambda (&rest args) (setq selected (plist-get args :model-id)))))
        (should-error (+agent-acp-set-model "provider/unknown") :type 'user-error)
        (should-not selected)
        (+agent-acp-set-model "provider/two")
        (should (equal selected "provider/two"))
        (setq-local agent-shell--state '((:active-requests . (prompt))
                                        (:session . ((:id . "session-1")))))
        (should-error (+agent-acp-set-model "provider/one") :type 'user-error)))))

(ert-deftest +agent-acp-receipt-precedes-consumer-and-is-run-scoped ()
  (with-temp-buffer
    (setq-local agent-shell--state '((:session . ((:id . "s")))))
    (let (success)
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args) (setq success (plist-get args :on-success)))))
        (+agent-acp-prompt :buffer (current-buffer) :run-id "r" :content []
                                :on-success (lambda (_) (error "consumer crashed")))
        (should-error (funcall success '((stopReason . "end_turn"))))
        (should (eq (plist-get (+agent-acp-prompt-receipt (current-buffer) "r") :status) 'completed))
        (should-not (+agent-acp-prompt-receipt (current-buffer) "other-run"))))))

(ert-deftest +agent-acp-projects-logical-local-directory ()
  (let ((root (file-name-as-directory temporary-file-directory)))
    (cl-letf (((symbol-function 'remote-client-file-name)
               (lambda (path) (should (string-prefix-p "/fs:local:" path)) root)))
      (should (equal (+agent-acp--client-directory (concat "/fs:local:" root)) root)))))

(ert-deftest +agent-acp-rejects-unprojectable-directory-before-start ()
  (cl-letf (((symbol-function 'remote-client-file-name) (lambda (_) nil))
            ((symbol-function 'agent-shell--start) (lambda (&rest _) (ert-fail "Started with invalid cwd"))))
    (should-error (+agent-acp-start :directory "/fs:unavailable:/project" :config 'test)
                  :type 'user-error)))

(ert-deftest +agent-acp-pins-native-cwd-through-async-handshake ()
  (let ((root (file-name-as-directory temporary-file-directory))
        (buffer (generate-new-buffer " *noema-native-cwd-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'remote-client-file-name) (lambda (_) root))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest _)
                     (should (equal default-directory root))
                     (should (equal (funcall agent-shell-cwd-function) root))
                     buffer))
                  ((symbol-function '+agent-acp--install-workspace-tabs) #'ignore)
                  ((symbol-function '+agent-acp-subscribe) #'ignore)
                  ((symbol-function '+agent-acp-hide-client-stderr) #'ignore))
          (+agent-acp-start :directory (concat "/fs:local:" root) :config 'test)
          (with-current-buffer buffer
            (should (local-variable-p 'agent-shell-cwd-function))
            (should (equal (funcall agent-shell-cwd-function) root))))
      (kill-buffer buffer))))

(ert-deftest +agent-acp-hides-client-stderr-buffer ()
  "The acp.el stderr buffer is internal even when its agent is user-visible."
  (let* ((agent (generate-new-buffer " *noema-acp-test-agent*"))
         (stderr (generate-new-buffer "acp-client-stderr(noema-test)-1"))
         (process (make-process :name "acp-client(noema-test)-1"
                                :command '("sleep" "5")
                                :stderr stderr
                                :connection-type 'pipe
                                :noquery t)))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                   (lambda (buffer) (eq buffer agent)))
                  ((symbol-function '+agent-acp--state)
                   (lambda (_buffer) `((:client . ((:process . ,process)))))))
          (should (eq (+agent-acp-hide-client-stderr agent) stderr))
          (should (equal (buffer-name stderr) " acp-client-stderr(noema-test)-1"))
          ;; Idempotent: an already hidden buffer is left alone.
          (should-not (+agent-acp-hide-client-stderr agent)))
      (delete-process process)
      (when-let* ((pipe (get-process "acp-client(noema-test)-1 stderr")))
        (delete-process pipe))
      (kill-buffer agent)
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(ert-deftest +agent-acp-retires-transcript-copy-workspace ()
  "Reloading replaces an old copied-transcript workspace with the real session."
  (let* ((root (file-name-as-directory (make-temp-file "noema-acp-retire-" t)))
         (agent (generate-new-buffer " noema-agent-retire"))
         (old (generate-new-buffer "*Noema Agent Workspace · retire*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer agent
            (setq major-mode 'agent-shell-mode)
            (setq-local +agent-acp-session-root root)
            ;; Hooks of the removed workspace point at functions that no
            ;; longer exist; they must not survive into editing or killing.
            (add-hook 'after-change-functions '+agent-acp--workspace-source-changed nil t)
            (add-hook 'kill-buffer-hook '+agent-acp--workspace-source-killed nil t))
          (with-current-buffer old
            (setq-local +agent-acp-workspace-root root)
            (add-hook 'kill-buffer-hook '+agent-acp--workspace-killed nil t))
          (set-window-buffer (selected-window) old)
          (+agent-acp--retire-transcript-workspace)
          (should-not (buffer-live-p old))
          (should (eq agent (window-buffer (selected-window))))
          (with-current-buffer agent
            (should +agent-acp-tabs-mode)
            (should-not (memq '+agent-acp--workspace-source-changed after-change-functions))
            (insert "still editable")))
      (when (buffer-live-p agent) (kill-buffer agent))
      (when (buffer-live-p old) (kill-buffer old))
      (delete-directory root t))))

(ert-deftest +agent-acp-window-keys-and-help ()
  "Management keys are bound and `?' types in the prompt, helps elsewhere."
  (dolist (binding '(("C-c C-e" . +agent-acp-focus-input)
                     ("C-c C-x" . +agent-acp-stop)
                     ("C-c C-r" . +agent-session-restart)
                     ("C-c C-k" . +agent-acp-close)
                     ("C-c C-w" . +agent-session-rename)
                     ("C-c C-d" . +agent-session-archive)
                     ("C-c C-z" . +agent-session-menu)
                     ("?" . +agent-acp-help-or-insert)))
    (should (eq (lookup-key +agent-acp-tabs-mode-map (kbd (car binding)))
                (cdr binding))))
  (with-temp-buffer
    (insert "Codex> ")
    (setq-local comint-last-prompt (cons (copy-marker 1) (copy-marker (point))))
    (let (helped)
      (cl-letf (((symbol-function '+agent-session-menu) (lambda () (interactive) (setq helped t))))
        (let ((last-command-event ??))
          (+agent-acp-help-or-insert))
        (should (equal (buffer-string) "Codex> ?"))
        (should-not helped)
        (goto-char (point-min))
        (+agent-acp-help-or-insert)
        (should helped)))))

(ert-deftest +agent-acp-restores-an-input-prompt-after-a-noema-turn ()
  (let ((buffer (generate-new-buffer " noema-agent-prompt"))
        (live nil)
        finished)
    (unwind-protect
        (with-current-buffer buffer
          (setq major-mode 'agent-shell-mode)
          (setq-local shell-maker--config 'config
                      shell-maker--busy nil
                      +agent-render--pending nil
                      comint-last-prompt (cons (copy-marker 1) (copy-marker 1)))
          (cl-letf (((symbol-function 'shell-maker-finish-output)
                     (lambda (&rest args) (setq finished args live t)))
                    ((symbol-function 'agent-shell--live-input-prompt-p)
                     (lambda (_prompt) live)))
            ;; A live prompt already awaits input: nothing to write.
            (setq live t)
            (should-not (+agent-acp-restore-prompt buffer))
            (should-not finished)
            (should-not +agent-acp--prompt-owed)
            ;; Output followed the prompt: write the next one as agent-shell does.
            (setq live nil)
            (should (+agent-acp-restore-prompt buffer))
            (should (equal finished '(:config config :success t)))
            ;; Hidden buffer: output is deferred, so the prompt waits for it.
            (setq live nil finished nil)
            (setq-local +agent-render--pending '((text)))
            (should-not (+agent-acp-restore-prompt buffer))
            (should +agent-acp--prompt-owed)
            (should-not finished)
            (setq-local +agent-render--pending nil)
            (+agent-acp--settle-after-flush-a buffer)
            (should (equal finished '(:config config :success t)))
            (should-not +agent-acp--prompt-owed)))
      (kill-buffer buffer))))

(ert-deftest +agent-acp-tab-management-stops-closes-and-offers-a-menu ()
  (let* ((root (file-name-as-directory (make-temp-file "noema-acp-manage-" t)))
         (main (generate-new-buffer " noema-agent-main"))
         (other (generate-new-buffer " noema-agent-other"))
         (retired (generate-new-buffer " noema-agent-retired"))
         stopped interrupted)
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (list main other retired))
            (with-current-buffer buffer
              (setq major-mode 'agent-shell-mode)
              (setq-local +agent-acp-session-root root)))
          (with-current-buffer main (setq-local +agent-acp-session-name "main"))
          (with-current-buffer other (setq-local +agent-acp-session-name "other"))
          (cl-letf (((symbol-function '+agent-render-flush) #'ignore)
                    ((symbol-function '+agent-acp-interrupt)
                     (lambda (&rest _) (setq interrupted t))))
            (+agent-acp-show-buffer main)
            (let ((window (+agent-acp--workspace-window))
                  (+agent-acp-stop-functions
                   (list (lambda (buffer) (setq stopped buffer)))))
              ;; A Run owner stops its Run; otherwise the turn is interrupted.
              (+agent-acp-stop main)
              (should (eq stopped main))
              (should-not interrupted)
              (setq +agent-acp-stop-functions nil)
              (+agent-acp-stop main)
              (should interrupted)
              ;; Name actions are unavailable for a retired buffer.
              (let ((active (lambda (buffer label)
                              (let ((item (seq-find (lambda (item)
                                                      (and (vectorp item) (equal (aref item 0) label)))
                                                    (+agent-acp--menu-items buffer))))
                                (plist-get (append (seq-drop item 2) nil) :active)))))
                (should (eq t (funcall active main "Restart session")))
                (should-not (funcall active retired "Restart session")))
              ;; Closing a shown tab keeps the Agent window on the next session.
              (+agent-acp-close main)
              (should-not (buffer-live-p main))
              (should (eq other (window-buffer window)))
              (+agent-acp-close-retired other)
              (should-not (buffer-live-p retired))
              (should (buffer-live-p other)))))
      (dolist (buffer (list main other retired))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory root t))))

;; Adopted from Pisper's resident-runtime protection: queued intent keeps a
;; session alive exactly like a running turn does.
(ert-deftest +agent-acp-queued-prompts-protect-a-session-from-auto-stop ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                shell-maker--busy nil
                +agent-acp-session-origin 'run
                agent-shell--state (list (cons :pending-prompts nil)
                                         (cons :init-finished t)))
    (let ((buffer (current-buffer)))
      (should (= (+agent-acp-pending-prompt-count buffer) 0))
      (should (+agent-acp-auto-stoppable-p buffer))
      (setf (map-elt agent-shell--state :pending-prompts) (list "next" "after"))
      (should (= (+agent-acp-pending-prompt-count buffer) 2))
      (should-not (+agent-acp-auto-stoppable-p buffer)))))

;; Adopted from Pisper's temporary side chat.
(ert-deftest +agent-acp-side-chats-stay-out-of-the-durable-registry ()
  (let (durable)
    (cl-letf (((symbol-function 'noema-agent-register-session)
               (lambda (_buffer name _root) (push name durable))))
      (dolist (origin '(side manual))
        (with-temp-buffer
          (setq-local major-mode 'agent-shell-mode
                      +agent-acp-session-origin origin)
          (+agent-acp-adopt (current-buffer) :agent "codex" :origin origin
                                 :root "/tmp/p/" :name (format "%s-chat" origin)))))
    (should (equal durable '("manual-chat")))
    (should (memq 'side +agent-acp-auto-stop-origins))))

(ert-deftest +agent-acp-conversation-tree-requires-history-capabilities ()
  (require 'agent-shell-fork-tree)
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                agent-shell--state '((:supports-session-list . t)
                                     (:supports-session-load . t)))
    (let ((source (current-buffer)) opened resolved)
      (cl-letf (((symbol-function 'agent-shell-fork-tree)
                 (lambda ()
                   (setq opened (current-buffer)
                         resolved (file-truename "/noema/target-only/workspace")))))
        (+agent-acp-conversation-tree source)
        (should (eq opened source))
        (should (equal resolved "/noema/target-only/workspace")))
      (setf (map-elt agent-shell--state :supports-session-load) nil)
      (should-error (+agent-acp-conversation-tree source) :type 'user-error))))

(ert-deftest +agent-acp-conversation-tree-visit-adopts-new-branch ()
  (save-window-excursion
    (let ((source (generate-new-buffer " *tree-source*"))
          (branch (generate-new-buffer " *tree-branch*"))
          adopted shown)
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq-local +agent-acp-session-root "/tmp/noema-project/"))
            (with-current-buffer branch
              (setq-local major-mode 'agent-shell-mode))
            (set-window-buffer (selected-window) branch)
            (set-window-parameter (selected-window) 'agent-workspace t)
            (with-temp-buffer
              (setq-local agent-shell-fork-tree--source source)
              (cl-letf (((symbol-function '+agent-acp-adopt)
                         (lambda (buffer &rest properties)
                           (setq adopted (cons buffer properties))))
                        ((symbol-function '+agent-acp-show-buffer)
                         (lambda (buffer) (setq shown buffer))))
                (+agent-acp--conversation-tree-visit-a
                 (lambda (&rest _) (selected-window)))))
            (should (eq (car adopted) branch))
            (should (eq (plist-get (cdr adopted) :origin) 'tree))
            (should (equal (plist-get (cdr adopted) :root) "/tmp/noema-project/"))
            (should (eq shown branch)))
        (set-window-parameter (selected-window) 'agent-workspace nil)
        (kill-buffer source)
        (kill-buffer branch)))))

(ert-deftest +agent-acp-conversation-tree-never-reuses-another-workspace ()
  (let ((source (generate-new-buffer " *tree-scope-source*"))
        (same (generate-new-buffer " *tree-scope-same*"))
        (other (generate-new-buffer " *tree-scope-other*"))
        visible)
    (unwind-protect
        (progn
          (dolist (buffer (list source same other))
            (with-current-buffer buffer
              (setq-local agent-shell--state
                          '((:agent-config . ((:identifier . codex)))))))
          (with-current-buffer source (setq default-directory "/tmp/project-a/"))
          (with-current-buffer same (setq default-directory "/tmp/project-a/"))
          (with-current-buffer other (setq default-directory "/tmp/project-b/"))
          (with-temp-buffer
            (setq-local agent-shell-fork-tree--source source)
            (cl-letf (((symbol-function 'agent-shell-buffers)
                       (lambda () (list same other)))
                      ((symbol-function '+agent-acp-agent-buffer-p)
                       (lambda (_buffer) nil)))
              (+agent-acp--conversation-tree-visit-a
               (lambda (&rest _)
                 (setq visible (agent-shell-buffers))
                 nil))))
          (should (equal visible (list same))))
      (dolist (buffer (list source same other))
        (kill-buffer buffer)))))

(provide 'agent-acp-tests)
;;; +agent-acp-tests.el ends here
