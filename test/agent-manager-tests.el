;;; agent-manager-tests.el --- Manager and lifecycle contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/manager)
(require 'term/portal/popup)

(defun agent-manager-tests--dispose (buffer)
  "Discard a test-owned buffer without prompting about its synthetic work."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))

(defun agent-manager-tests--session (name &optional root)
  "A real buffer and local pipe, with no provider or model request."
  (let* ((buffer (generate-new-buffer (concat " *agent-manager-test-" name "*")))
         (process (make-pipe-process :name (buffer-name buffer) :noquery t)))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode)
      (setq-local default-directory (or root temporary-file-directory)
                  +agent-acp-session-root default-directory
                  +agent-acp-session-name name +agent-acp-session-agent "codex"
                  +agent-acp-session-origin 'run
                  agent-shell--state
                  (agent-shell--make-state :buffer buffer :agent-config '((:identifier . codex))))
      (setf (map-elt agent-shell--state :client) (list (cons :process process))
            (map-elt agent-shell--state :init-finished) t
            (map-elt (map-elt agent-shell--state :session) :id) name)
      (+agent-acp--track-activity-h)
      (add-hook 'kill-buffer-hook (lambda () (when (process-live-p process) (delete-process process))) nil t))
    buffer))

(ert-deftest agent-manager-c11-entry-is-a-direct-global-key ()
  (with-temp-buffer
    (should (eq (key-binding (kbd "H-A")) #'+agent/manager))
    (should (eq (key-binding (kbd "C-c A RET")) #'+agent/manager))
    (evil-local-mode 1) (evil-normal-state)
    (should (eq (key-binding (kbd "SPC a a")) #'+agent/manager))))

(ert-deftest agent-manager-c11-groups-include-target-and-workspace ()
  (let* ((a '(:name "a" :root "/work/" :target "one"))
         (b '(:name "a" :root "/work/" :target "two"))
         (sessions (+agent-manager--sessions (list b a) 'all nil "")))
    (should (= (length (seq-group-by #'+agent-manager--group sessions)) 2))
    (should (equal (+agent-manager--sessions sessions 'all nil "one") nil))))

(ert-deftest agent-manager-c11-filters-are-literal-and-keep-request-facts ()
  (let ((sessions '((:name "a[1]" :root "/p/" :attention-kind permission)
                    (:name "other" :root "/q/" :unread-kind done)
                    (:name "dead" :root "/q/" :process-state dead)
                    (:name "idle" :root "/q/"))))
    (should (= 1 (length (+agent-manager--sessions sessions 'all nil "[1]"))))
    (should (= 1 (length (+agent-manager--sessions sessions 'project "/p/" ""))))
    (should (= 3 (length (+agent-manager--sessions sessions 'attention nil ""))))))

(ert-deftest agent-manager-c11-refresh-keeps-the-selected-entity ()
  (let ((a (agent-manager-tests--session "a"))
        (b (agent-manager-tests--session "b"))
        (private (agent-manager-tests--session "private")))
    (unwind-protect
        (with-temp-buffer
          (setq-local +agent-manager--model (etaf-ref nil)
                      +agent-manager--selected b)
          (let ((+agent-session-private-functions
                 (list (lambda (session) (eq (plist-get session :buffer) private)))))
           (cl-letf (((symbol-function '+agent-acp-sessions)
                     (lambda (&rest _) (mapcar #'+agent-session-snapshot (list a b private)))))
            (+agent-manager-refresh)
            (should (= (plist-get (etaf-value +agent-manager--model) :total) 2))
            (should (eq (plist-get (etaf-value +agent-manager--model) :selected) b))
            (with-current-buffer b (setq +agent-acp-attention 'permission))
            (+agent-manager-refresh)
            (should (eq +agent-manager--selected b))
            (should (eq (buffer-local-value '+agent-acp-attention b) 'permission)))))
      (agent-manager-tests--dispose a) (agent-manager-tests--dispose b) (agent-manager-tests--dispose private))))

(ert-deftest agent-manager-c11-real-etaf-controls-render-and-scroll ()
  (let* ((agents (cl-loop for index below 24 collect (agent-manager-tests--session (format "task-%02d" index))))
         (buffer (generate-new-buffer " *agent-manager-view-test*"))
         (model (etaf-ref (list :sessions (mapcar #'+agent-session-snapshot agents)
                                :selected (car agents) :scope 'all :query "" :total 24 :attention 0))))
    (unwind-protect
        (progn
          (etaf-mount buffer (etaf-view (+agent-manager-page :model model :owner buffer))
                      '(:viewport-width 1000 :viewport-height 32))
          (with-current-buffer buffer
            (let ((runtime (etaf-runtime-for-buffer buffer)))
              (should (string-match-p "Sessions" (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "All 24" (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "RET open" (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "q return" (buffer-string)))
              (should (> (etaf-scroll-to runtime 'agent-manager-list 'end) 0))
              (should (= (etaf-scroll-to runtime 'agent-manager-list 0) 0)))))
      (agent-manager-tests--dispose buffer) (mapc #'agent-manager-tests--dispose agents))))

(ert-deftest agent-manager-c11-empty-view-has-a-working-new-entry ()
  (let* ((buffer (generate-new-buffer " *agent-manager-empty-test*"))
         (model (etaf-ref '(:sessions nil :selected nil :scope all :query "" :total 0 :attention 0))))
    (unwind-protect
        (progn
          (etaf-mount buffer (etaf-view (+agent-manager-page :model model :owner buffer))
                      '(:viewport-width 900 :viewport-height 30))
          (with-current-buffer buffer
            (should (string-match-p "No sessions here" (buffer-string)))
            (should (string-match-p "New · n" (buffer-string)))))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c12-hiding-and-killing-ui-never-ends-execution ()
  (let ((agent (agent-manager-tests--session "keep-running"))
        (buffer (get-buffer-create +agent-manager-buffer-name)) timer)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local +agent-manager--watching t
                      +agent-manager--timer (run-at-time 300 nil #'ignore))
          (setq timer +agent-manager--timer)
          (add-hook '+agent-acp-changed-functions #'+agent-manager--changed-h)
          (add-hook 'kill-buffer-hook #'+agent-manager--killed-h nil t)
          (+agent-manager--suspend)
          (should-not (memq timer timer-list))
          (should-not (memq #'+agent-manager--changed-h +agent-acp-changed-functions))
          (kill-buffer buffer)
          (should (process-live-p (+agent-acp-state-value agent '(:client :process)))))
      (when (buffer-live-p buffer) (agent-manager-tests--dispose buffer)) (agent-manager-tests--dispose agent))))

(ert-deftest agent-manager-c13-restart-resumes-native-id-and-retains-unsent-input ()
  (save-window-excursion
    (let* ((old (agent-manager-tests--session "restart"))
           (new (agent-manager-tests--session "replacement"))
           (old-name (buffer-name old)) seen inserted displayed)
      (unwind-protect
          (progn
            (with-current-buffer old
              (setq-local +agent-acp-session-origin 'intervention
                          +agent-acp-display-buffer-function (lambda (b) (setq displayed b)))
              (setf (map-elt agent-shell--state :pending-prompts) '("next" "later")))
            (cl-letf (((symbol-function 'agent-shell--prompt-input) (lambda () "unsent draft"))
                      ((symbol-function 'agent-shell-restart)
                       (lambda (&rest args)
                         (setq seen args)
                         (kill-buffer old)
                         (with-current-buffer new (rename-buffer old-name))))
                      ((symbol-function 'agent-shell-insert) (lambda (&rest args) (setq inserted args))))
              (should (eq (+agent-acp-restart-native old) new)))
            (should (equal seen '(:session-id "restart")))
            (should (equal (plist-get inserted :text) "unsent draft"))
            (should-not (plist-get inserted :submit))
            (should (eq (plist-get inserted :shell-buffer) new))
            (should (equal (+agent-acp-state-value new '(:pending-prompts)) '("next" "later")))
            (should (eq (buffer-local-value '+agent-acp-session-origin new) 'intervention))
            (should (equal (buffer-local-value '+agent-acp-session-name new) "restart"))
            (should (eq displayed new)))
        (when (buffer-live-p old) (agent-manager-tests--dispose old)) (agent-manager-tests--dispose new)))))

(ert-deftest agent-manager-c13-failed-restart-retains-a-recoverable-draft ()
  (let ((old (agent-manager-tests--session "failed-restart")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--prompt-input) (lambda () "must survive"))
                  ((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'agent-shell-restart)
                   (lambda (&rest _) (kill-buffer old) (error "startup failed"))))
          (should-error (+agent-acp-restart-native old))
          (with-current-buffer "*Agent recovery*"
            (should (string-match-p "must survive" (buffer-string)))))
      (when (buffer-live-p old) (agent-manager-tests--dispose old))
      (when-let* ((buffer (get-buffer "*Agent recovery*"))) (agent-manager-tests--dispose buffer)))))

(ert-deftest agent-manager-c13-real-upstream-restart-receives-id-and-workspace-environment ()
  (save-window-excursion
    (let* ((old (agent-manager-tests--session "native-restart"))
           (new (agent-manager-tests--session "native-new")) started)
      (unwind-protect
          (progn
            (with-current-buffer old
              (setq-local process-environment (cons "AGENT_TEST_WORKSPACE=kept" process-environment)))
            (cl-letf (((symbol-function 'agent-shell--start)
                       (lambda (&rest args)
                         (should (equal (getenv "AGENT_TEST_WORKSPACE") "kept"))
                         (setq started args) new))
                      ((symbol-function 'agent-shell--current-shell) (lambda () old))
                      ((symbol-function 'agent-shell--display-buffer) #'ignore)
                      ((symbol-function 'agent-shell--prompt-input) (lambda () "")))
              (+agent-acp-restart-native old))
            (should-not (buffer-live-p old))
            (should (equal (plist-get started :session-id) "native-restart"))
            (should (plist-get started :new-session)))
        (when (buffer-live-p old) (agent-manager-tests--dispose old))
        (when (buffer-live-p new) (agent-manager-tests--dispose new))))))

(ert-deftest agent-manager-c14-auto-stop-protects-every-kind-of-work ()
  (let ((buffer (agent-manager-tests--session "auto-stop")))
    (unwind-protect
        (with-current-buffer buffer
          (should (+agent-acp-auto-stoppable-p buffer))
          (dolist (pair '((:active-requests . (initializing))
                          (:tool-calls . (("tool" . ((:permission-request-id . 1)))))
                          (:elicitations . (("question" . ((:status . pending)))))
                          (:pending-prompts . ("intent"))))
            (setf (map-elt agent-shell--state (car pair)) (cdr pair))
            (should-not (+agent-acp-auto-stoppable-p buffer))
            (setf (map-elt agent-shell--state (car pair)) nil))
          (setf (map-elt agent-shell--state :init-finished) nil)
          (should-not (+agent-acp-auto-stoppable-p buffer))
          (setf (map-elt agent-shell--state :init-finished) t
                (map-elt (map-elt agent-shell--state :client) :pending-requests) '(rpc))
          (should-not (+agent-acp-auto-stoppable-p buffer)))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c15-stop-is-idempotent-and-times-out-without-faking-success ()
  (let ((buffer (agent-manager-tests--session "stopping")) (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setf (map-elt agent-shell--state :active-requests) '(request))
          (cl-letf (((symbol-function '+agent-acp-interrupt) (lambda (&rest _) (cl-incf calls))))
            (+agent-acp-stop buffer) (+agent-acp-stop buffer))
          (should (= calls 1))
          (should (equal (+agent-sessions--state (+agent-session-snapshot buffer)) "stopping"))
          (+agent-acp--stop-expired buffer agent-shell--state +agent-acp--stop-token)
          (should (equal (+agent-sessions--state (+agent-session-snapshot buffer)) "stop-unconfirmed"))
          (should (process-live-p (+agent-acp-state-value buffer '(:client :process))))
          (should (map-elt agent-shell--state :active-requests)))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c15-stale-cancellation-cannot-touch-a-new-turn ()
  (let ((buffer (agent-manager-tests--session "cancel-race")))
    (unwind-protect
        (with-current-buffer buffer
          (setf (map-elt agent-shell--state :active-requests) '(request))
          (cl-letf (((symbol-function '+agent-acp-interrupt) #'ignore)) (+agent-acp-stop buffer))
          (let ((state agent-shell--state) (token +agent-acp--stop-token)
                (timer +agent-acp--stop-timer))
            (+agent-acp--clear-stop)
            (should-not (memq timer timer-list))
            (+agent-acp--stop-expired buffer state token)
            (should-not +agent-acp--stop-unconfirmed)
            (should-not +agent-acp--stop-token)))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c15-subscriptions-are-fenced-to-their-state-and-buffer ()
  (let ((buffer (agent-manager-tests--session "subscription")) callback seen)
    (unwind-protect
        (progn
          (+agent-acp-subscribe :buffer buffer :event 'turn-complete
                               :callback (lambda (_) (setq seen (current-buffer))))
          (setq callback (map-elt (car (+agent-acp-state-value buffer '(:event-subscriptions))) :on-event))
          (with-temp-buffer (funcall callback '((:event . turn-complete))))
          (should (eq seen buffer))
          (setq seen nil)
          (with-current-buffer buffer
            (setq agent-shell--state (agent-shell--make-state :buffer buffer)))
          (funcall callback '((:event . turn-complete)))
          (should-not seen)
          (agent-manager-tests--dispose buffer)
          (funcall callback '((:event . turn-complete)))
          (should-not seen))
      (when (buffer-live-p buffer) (agent-manager-tests--dispose buffer)))))

(ert-deftest agent-manager-c15-native-events-release-stop-observers-once ()
  (let ((buffer (agent-manager-tests--session "native-event")))
    (unwind-protect
        (with-current-buffer buffer
          (+agent-acp--watch-lifecycle-h)
          (+agent-acp--watch-lifecycle-h)
          (should (= 1 (length (map-elt agent-shell--state :event-subscriptions))))
          (setf (map-elt agent-shell--state :active-requests) '(request))
          (cl-letf (((symbol-function '+agent-acp-interrupt) #'ignore)) (+agent-acp-stop buffer))
          (let ((timer +agent-acp--stop-timer))
            (agent-shell--emit-event :event 'input-submitted)
            (should-not +agent-acp--stop-token)
            (should-not (memq timer timer-list))))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c15-raw-new-request-invalidates-old-stop-without-an-event ()
  (let ((buffer (agent-manager-tests--session "raw-request")))
    (unwind-protect
        (with-current-buffer buffer
          (setf (map-elt agent-shell--state :active-requests) '(first))
          (cl-letf (((symbol-function '+agent-acp-interrupt) #'ignore)) (+agent-acp-stop buffer))
          (let ((token +agent-acp--stop-token))
            (setf (map-elt agent-shell--state :request-count) 2
                  (map-elt agent-shell--state :active-requests) '(second))
            (+agent-acp--stop-expired buffer agent-shell--state token)
            (should-not +agent-acp--stop-token)
            (should-not +agent-acp--stop-unconfirmed)
            (should (equal (map-elt agent-shell--state :active-requests) '(second)))))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c14-recycling-requires-positive-resume-capabilities ()
  (require 'noema-agent-worker)
  (let ((buffer (agent-manager-tests--session "capabilities")))
    (unwind-protect
        (with-current-buffer buffer
          (setf (map-elt agent-shell--state :supports-session-list) :false
                (map-elt agent-shell--state :supports-session-load) :false
                (map-elt agent-shell--state :supports-session-resume) :false)
          (should-not (noema-agent-worker--resumable-buffer-p buffer))
          (should-error (+agent-acp-conversation-tree buffer) :type 'user-error)
          (setf (map-elt agent-shell--state :supports-session-resume) t)
          (should (noema-agent-worker--resumable-buffer-p buffer)))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c16-live-input-remains-editable-during-a-turn ()
  (save-window-excursion
    (let ((buffer (agent-manager-tests--session "input")) restored)
      (unwind-protect
          (with-current-buffer buffer
            (setf (map-elt agent-shell--state :active-requests) '(running))
            (setq-local shell-maker--busy t)
            (cl-letf (((symbol-function '+agent-acp-show-buffer) (lambda (b) (switch-to-buffer b)))
                      ((symbol-function '+agent-acp-restore-prompt) (lambda (_) (setq restored t))))
              (should (eq (+agent-acp-focus-input buffer) buffer))
              (should-not restored)
              (insert "still editable")
              (should shell-maker--busy)
              (should (equal (map-elt agent-shell--state :active-requests) '(running)))))
        (agent-manager-tests--dispose buffer)))))

(ert-deftest agent-manager-c15-retirement-rechecks-identity-and-cleans-its-timer ()
  (let ((buffer (agent-manager-tests--session "retire"))
        (unknown (agent-manager-tests--session "history-unknown")))
    (unwind-protect
        (with-current-buffer buffer
          (with-current-buffer unknown
            (setq +agent-acp-session-name nil)
            (+agent-acp--schedule-retired unknown)
            (let ((timer +agent-acp--retire-timer))
              (cancel-timer timer) (funcall (timer--function timer)))
            (should (buffer-live-p unknown))
            (setf (map-elt agent-shell--state :supports-session-load) t)
            (+agent-acp--schedule-retired unknown)
            (let ((timer +agent-acp--retire-timer))
              (cancel-timer timer) (funcall (timer--function timer))))
          (should-not (buffer-live-p unknown))
          (setq +agent-acp-session-name nil)
          (+agent-acp--schedule-retired buffer)
          (let* ((timer +agent-acp--retire-timer) (callback (timer--function timer)))
            (cancel-timer timer)
            (setq +agent-acp-session-name "claimed-again")
            (funcall callback)
            (should (buffer-live-p buffer)))
          (+agent-acp--schedule-retired buffer)
          (let ((timer +agent-acp--retire-timer))
            (agent-manager-tests--dispose buffer)
            (should-not (memq timer timer-list))))
      (when (buffer-live-p buffer) (agent-manager-tests--dispose buffer))
      (when (buffer-live-p unknown) (agent-manager-tests--dispose unknown)))))

(ert-deftest agent-manager-c13-research-restart-uses-shared-operation-after-run-ends ()
  (require 'noema-sessions)
  (let ((buffer (agent-manager-tests--session "research-restart")) called)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-restart-native) (lambda (b) (setq called b))))
          (let ((+agent-acp-run-owned-functions (list (lambda (_) t))))
            (should-error (noema-sessions-agent-restart buffer) :type 'user-error)
            (should-not called))
          (let ((+agent-acp-run-owned-functions nil))
            (noema-sessions-agent-restart buffer)
            (should (eq called buffer))))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c14-hidden-unsent-draft-protects-the-session ()
  (let ((buffer (agent-manager-tests--session "draft")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "Agent> ")
          (setq-local comint-last-prompt (cons (copy-marker 1) (copy-marker (point))))
          (insert "unsent")
          (should-not (+agent-acp-auto-stoppable-p buffer))
          (let ((asked nil))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq asked t) nil)))
              (should-not (+agent-acp-confirm-stop buffer "end")))
            (should asked)))
      (agent-manager-tests--dispose buffer))))

(ert-deftest agent-manager-c16-agent-dock-does-not-split-the-work-pane ()
  (save-window-excursion
    (delete-other-windows)
    (let ((source (selected-window)) (buffer (agent-manager-tests--session "dock")))
      (unwind-protect
          (progn
            (+agent-acp-show-buffer buffer)
            (should (= (length (window-list)) 2))
            (should (window-parameter (selected-window) 'agent-workspace))
            (+agent-acp-hide buffer)
            (should (eq (selected-window) source))
            (should (= (length (window-list)) 1))
            (should (buffer-live-p buffer))
            (should (process-live-p (+agent-acp-state-value buffer '(:client :process)))))
        (agent-manager-tests--dispose buffer)))))

(ert-deftest agent-manager-c16-popup-and-dock-have-the-same-input-hide-keys ()
  (dolist (map (list +agent-acp-tabs-mode-map +portal-popup-agent-keys-mode-map))
    (should (eq (lookup-key map (kbd "C-c C-e")) #'+agent-acp-focus-input))
    (should (eq (lookup-key map (kbd "C-c C-o")) #'+agent-acp-hide))
    (should (eq (lookup-key map (kbd "C-c C-l")) #'+agent/manager))))

(provide 'agent-manager-tests)
;;; agent-manager-tests.el ends here

(ert-deftest agent-manager-c17-sessions-inbox-and-usage-use-one-surface ()
  (let (opened)
    (cl-letf (((symbol-function '+agent-manager-show)
               (lambda (&rest args) (push args opened))))
      (+agent/sessions t) (+agent/inbox) (+agent/usage)
      (should (equal (nreverse opened) '((sessions t) (attention) (usage nil)))))))

(ert-deftest agent-manager-c17-folds-survive-refresh-without-hiding-requests ()
  (let ((a (agent-manager-tests--session "first" "/tmp/one/"))
        (b (agent-manager-tests--session "second" "/tmp/two/")))
    (unwind-protect
        (with-temp-buffer
          (setq +agent-manager--model (etaf-ref nil) +agent-manager--selected a)
          (cl-letf (((symbol-function '+agent-acp-sessions)
                     (lambda (&rest _) (mapcar #'+agent-session-snapshot (list a b)))))
            (+agent-manager-refresh)
            (+agent-manager-toggle-group (+agent-manager--group (+agent-session-snapshot a)))
            (should (eq +agent-manager--selected b))
            (+agent-manager-refresh)
            (should (= 1 (length (+agent-manager--visible-sessions
                                  (plist-get (etaf-value +agent-manager--model) :sessions)))))
            (+agent-manager-search "first")
            (should (eq +agent-manager--selected a))
            (should +agent-manager--collapsed)))
      (agent-manager-tests--dispose a) (agent-manager-tests--dispose b))))

(ert-deftest agent-manager-c18-preview-refits-content-and-hide-detaches-real-buffer ()
  (require 'aaron-ui-float)
  (save-window-excursion
    (let* ((content (generate-new-buffer " *agent-content*"))
           (agent (agent-manager-tests--session "preview"))
           (panel (aaron-ui-float--make :buffer content :frame (selected-frame)
                                       :parent (selected-frame) :owned t))
           (aaron-ui-float--panels (list panel)) fit-width preview)
      (unwind-protect
          (progn
            (switch-to-buffer content)
            (setq-local aaron-ui-float--current panel)
            (cl-letf (((symbol-function 'etaf-runtime-for-buffer) (lambda (&rest _) 'runtime))
                      ((symbol-function 'etaf-runtime-refit) (lambda (_ width &rest _) (setq fit-width width)))
                      ((symbol-function 'posframe-hide) #'ignore)
                      ((symbol-function 'select-frame-set-input-focus) #'ignore))
              (setq preview (aaron-ui-float-preview-buffer panel agent :sidebar-width 28))
              (select-window preview)
              (aaron-ui-float--refit panel)
              (should (= fit-width (window-body-width (get-buffer-window content) t)))
              (should (eq (window-buffer preview) agent))
              (with-current-buffer agent (+agent-acp--attention-shown preview))
              (aaron-ui-float-hide panel)
              (should-not (window-live-p preview))
              (should-not (get-buffer-window agent t))
              (should (process-live-p (+agent-acp-state-value agent '(:client :process))))))
        (agent-manager-tests--dispose content) (agent-manager-tests--dispose agent)))))

(ert-deftest agent-manager-c18-preview-never-clears-or-suppresses-unread ()
  (save-window-excursion
    (let ((agent (agent-manager-tests--session "unread"))
          (+agent-acp-notify-function nil))
      (unwind-protect
          (progn
            (switch-to-buffer agent)
            (set-window-parameter (selected-window) 'aaron-ui-preview t)
            (with-current-buffer agent
              (setq +agent-acp-attention 'done)
              (+agent-acp--attention-shown (selected-window))
              (should (eq +agent-acp-attention 'done))
              (+agent-acp--note-attention agent 'permission)
              (should (eq +agent-acp-attention 'permission))))
        (set-window-parameter (selected-window) 'aaron-ui-preview nil)
        (agent-manager-tests--dispose agent)))))

(ert-deftest agent-manager-c20-research-is-a-shared-read-only-group ()
  (require 'noema-agent-acp)
  (let ((agent (agent-manager-tests--session "research")))
    (unwind-protect
        (with-current-buffer agent
          (setq-local noema-agent-promote--session-id "logical-id")
          (let ((snapshot (+agent-session-snapshot agent)))
            (should (equal (plist-get snapshot :group) '(:key noema :label "Noema Research")))
            (should (equal (plist-get snapshot :session-id) "logical-id"))
            (should-not (eq (plist-get snapshot :native-session-id) "logical-id"))))
      (agent-manager-tests--dispose agent))))

(ert-deftest agent-manager-c21-context-folding-leaves-protocol-content-intact ()
  (let* ((content '(((type . "text") (text . "Review this."))
                    ((type . "resource_link") (name . "Research note") (uri . "noema://memory/private-id"))))
         (before (copy-tree content)) calls)
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (push args calls))))
      (+agent-acp--display-submitted-content '((:request-count . 1)) content "run"))
    (let ((prompt (seq-find (lambda (call) (equal (plist-get call :block-id) "submitted-input")) calls))
          (context (seq-find (lambda (call) (equal (plist-get call :block-id) "submitted-context")) calls)))
      (should (equal (plist-get prompt :body) "Review this."))
      (should-not (plist-get context :expanded))
      (should (equal (plist-get context :label-left) "Context · 1 attachments"))
      (should (string-match-p "noema://memory/private-id" (plist-get context :body)))
      (should (equal before content)))))

(ert-deftest agent-manager-c23-pool-stop-includes-all-origins-and-fences-other-projects ()
  (require 'tools/agent/menu)
  (let ((a (agent-manager-tests--session "group-a"))
        (b (agent-manager-tests--session "group-b"))
        (popup (agent-manager-tests--session "group-popup"))
        (outside (agent-manager-tests--session "outside" "/tmp/another-project/"))
        stopped)
    (unwind-protect
        (progn
          (dolist (buffer (list a b popup outside))
            (with-current-buffer buffer
              (setq +agent-acp-session-origin (if (eq buffer popup) 'popup 'manual))
              (setf (map-elt agent-shell--state :active-requests) '(request))))
          (cl-letf (((symbol-function '+agent-acp-sessions)
                     (lambda (&rest _) (mapcar #'+agent-session-snapshot (list a b popup outside))))
                    ((symbol-function '+agent-acp-stop) (lambda (buffer) (push buffer stopped))))
            (+agent-menu--stop-group a))
          (should (= (length stopped) 3))
          (should (memq a stopped)) (should (memq b stopped))
          (should (memq popup stopped))
          (should-not (memq outside stopped)))
      (mapc #'agent-manager-tests--dispose (list a b popup outside)))))

(ert-deftest agent-manager-c20-retired-work-is-reachable-without-idle-tab-clutter ()
  (let ((agent (agent-manager-tests--session "previous")))
    (unwind-protect
        (with-current-buffer agent
          (setq +agent-acp-session-name nil +agent-acp-retired t)
          (should-not (memq agent (+agent-acp--project-buffers default-directory)))
          (should (eq (plist-get (plist-get (+agent-session-snapshot agent) :group) :key) 'retired))
          (should (seq-find (lambda (s) (eq agent (plist-get s :buffer))) (+agent-acp-sessions)))
          (setf (map-elt agent-shell--state :pending-prompts) '("keep queued work"))
          (should (memq agent (+agent-acp--project-buffers default-directory)))
          (setf (map-elt agent-shell--state :pending-prompts) nil
                (map-elt agent-shell--state :active-requests) '(request))
          (should (memq agent (+agent-acp--project-buffers default-directory))))
      (agent-manager-tests--dispose agent))))

(ert-deftest agent-manager-c22-warm-sweep-keeps-intent-and-requires-restoration ()
  (let ((stale (agent-manager-tests--session "stale"))
        (queued (agent-manager-tests--session "queued"))
        (opaque (agent-manager-tests--session "opaque")))
    (unwind-protect
        (progn
          (dolist (buffer (list stale queued opaque))
            (with-current-buffer buffer
              (setq +agent-acp-last-used-at (- (float-time) 3600))
              (setf (map-elt agent-shell--state :supports-session-load) t)))
          (with-current-buffer queued (setf (map-elt agent-shell--state :pending-prompts) '("next")))
          (with-current-buffer opaque
            (setf (map-elt agent-shell--state :supports-session-load) :false
                  (map-elt agent-shell--state :supports-session-resume) :false))
          (+agent-acp-sweep-warm-buffers 8 60)
          (should-not (buffer-live-p stale))
          (should (buffer-live-p queued)) (should (buffer-live-p opaque))
          (should (= 1 (+agent-acp-pending-prompt-count queued))))
      (dolist (buffer (list stale queued opaque))
        (when (buffer-live-p buffer) (agent-manager-tests--dispose buffer))))))

(ert-deftest agent-manager-c22-last-background-client-releases-the-single-timer ()
  (let ((agent (agent-manager-tests--session "warm-owner")) first)
    (unwind-protect
        (progn
          (with-current-buffer agent (+agent-acp--install-retention-h))
          (+agent-acp--sync-warm-sweep)
          (setq first +agent-acp--warm-sweep-timer)
          (should (timerp first))
          (+agent-acp--sync-warm-sweep)
          (should (eq first +agent-acp--warm-sweep-timer))
          (agent-manager-tests--dispose agent)
          (should-not +agent-acp--warm-sweep-timer)
          (should-not (memq first timer-list)))
      (when (buffer-live-p agent) (agent-manager-tests--dispose agent)))))
