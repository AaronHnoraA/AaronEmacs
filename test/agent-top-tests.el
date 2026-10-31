;;; +agent-top-tests.el --- Tests for the live agent board -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'agent-shell)
(require 'tools/agent/top)

(defmacro +agent-top-tests--with-session (usage &rest body)
  "Run BODY with `session' bound to a fake agent buffer reporting USAGE."
  (declare (indent 1))
  `(let ((session (generate-new-buffer " *abtop-test*")))
     (unwind-protect
         (progn
           (with-current-buffer session
             (setq-local agent-shell--state (agent-shell--make-state :buffer session))
             (setf (alist-get :model-id (alist-get :session agent-shell--state)) "opus")
             (setf (alist-get :request-count agent-shell--state) 3)
             (pcase-dolist (`(,key . ,value) ,usage)
               (setf (alist-get key (alist-get :usage agent-shell--state)) value))
             (setq-local +agent-acp-session-name "research"
                         +agent-acp-session-agent "claude"
                         +agent-acp-session-root "/tmp/proj/"))
           (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                      (lambda (buffer) (eq buffer session)))
                     ((symbol-function '+agent-acp-busy-p) #'ignore))
             ,@body))
       (kill-buffer session))))

(ert-deftest +agent-top-reads-agent-shell-state ()
  "Usage comes from agent-shell's recorded ACP state, normalized."
  (+agent-top-tests--with-session
      '((:context-used . 144000) (:context-size . 200000)
        (:input-tokens . 5200) (:output-tokens . 2800.0)
        (:cached-read-tokens . 320000) (:cost-amount . 0.42))
    (let ((usage (+agent-acp-usage session)))
      (should (equal (plist-get usage :model) "opus"))
      (should (= (plist-get usage :context-used) 144000))
      (should (= (plist-get usage :output) 2800))
      (should (= (plist-get usage :thought) 0))
      (should (= (plist-get usage :turns) 3))
      (should (= (plist-get usage :compactions) 0))
      (should (= (plist-get usage :cost) 0.42)))
    (should-not (+agent-acp-usage (current-buffer)))))

(ert-deftest +agent-top-counts-compactions-and-rate-limits ()
  "Usage reports feed compaction counting and Claude rate limits."
  (let ((+agent-acp--rate-limits (make-hash-table :test #'equal))
        (+agent-acp-rate-limits-file (make-temp-file "abtop-limits" nil ".json"))
        reported)
    (+agent-top-tests--with-session nil
      (let ((+agent-acp-changed-functions
             (list (lambda (buffer) (push buffer reported))))
            (state (buffer-local-value 'agent-shell--state session)))
        (dolist (used '(50000 120000 30000 36000 5000))
          (agent-shell--update-usage-from-notification
           :state state :acp-update `((used . ,used) (size . 200000))))
        ;; 120k -> 30k is a compaction; 36k -> 5k is too.  50k -> 120k is growth.
        (should (= 2 (plist-get (+agent-acp-usage session) :compactions)))
        (should (= 120000 (plist-get (+agent-acp-usage session) :context-peak)))
        (should (= 5 (plist-get (+agent-acp-usage session) :report-seq)))
        (should (= 5 (length reported)))
        (agent-shell--update-usage-from-notification
         :state state
         :acp-update '((used . 6000) (size . 200000)
                       (_meta (_claude/rateLimit (status . "allowed")
                                                 (rateLimitType . "five_hour")
                                                 (utilization . 0.42)
                                                 (resetsAt . 1791090463))))))
      (let ((limit (gethash "claude/five_hour" (+agent-acp-rate-limits))))
        (should (= 0.42 (plist-get limit :utilization)))
        (should (= 1791090463 (plist-get limit :resets-at))))
      ;; The last limits survive a restart.
      (setq +agent-acp--rate-limits nil)
      (should (= 0.42 (plist-get (gethash "claude/five_hour" (+agent-acp-rate-limits))
                                 :utilization))))))

(ert-deftest +agent-top-reads-codex-rate-limits ()
  "The last token_count with limits wins; windows are named by length."
  (let ((limits
         (+agent-top--codex-parse
          (concat
           "{\"timestamp\":\"2026-09-28T05:00:00Z\",\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"rate_limits\":{\"primary\":{\"used_percent\":1.0,\"window_minutes\":300}}}}\n"
           "{\"timestamp\":\"2026-09-28T05:36:54Z\",\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"rate_limits\":{\"limit_id\":\"codex\",\"primary\":{\"used_percent\":7.0,\"window_minutes\":10080,\"resets_at\":1791090463},\"secondary\":null}}}\n"
           "{\"type\":\"response_item\"}\n"))))
    (should (= 1 (length limits)))
    (should (equal (plist-get (car limits) :window) "7d"))
    (should (= 0.07 (plist-get (car limits) :utilization)))
    (should (numberp (plist-get (car limits) :updated-at)))))

(ert-deftest +agent-top-selects-newest-codex-file ()
  "Select by modification time even when the filename order differs."
  (let* ((root (make-temp-file "abtop-codex-" t))
         (day (expand-file-name "sessions/2026/09/30" root))
         (earlier (expand-file-name "rollout-z.jsonl" day))
         (latest (expand-file-name "rollout-a.jsonl" day)))
    (unwind-protect
        (progn
          (make-directory day t)
          (with-temp-file earlier (insert "old\n"))
          (with-temp-file latest (insert "new\n"))
          (set-file-times earlier (seconds-to-time 100))
          (set-file-times latest (seconds-to-time 200))
          (cl-letf (((symbol-function '+agent-top--codex-home)
                     (lambda () root)))
            (should (equal (+agent-top--codex-file) latest))))
      (delete-directory root t))))

(ert-deftest +agent-top-renders-sessions-and-detail ()
  "The board lists the session with a context bar and shows its detail."
  (let ((+agent-acp--rate-limits (make-hash-table :test #'equal))
        (+agent-acp-rate-limits-file (make-temp-file "abtop-limits" nil ".json")))
    (+agent-top-tests--with-session
        '((:context-used . 144000) (:context-size . 200000)
          (:input-tokens . 5200) (:output-tokens . 2800) (:cost-amount . 0.42))
      (cl-letf (((symbol-function '+agent-top--codex-limits) #'ignore)
                ((symbol-function '+agent-top--opencode-providers)
                 (lambda () '("github-copilot" "openai"))))
        (with-temp-buffer
          (+agent-top-mode)
          (+agent-top--render)
          (let ((text (buffer-string)))
            (should (string-match-p "╭─ ▾ quota " text))
            (should (string-match-p "claude +5h +·+ +reported during a turn, or on g" text))
            (should (string-match-p "codex +5h .*\n.* 7d +·+ +absent" text))
            (should (string-match-p "opencode +github-copilot .*GitHub's API" text))
            (should (string-match-p "openai +·+ +ChatGPT login" text))
            (should (string-match-p "○ idle +research +opus" text))
            (should (string-match-p " 72%" text))
            (should (string-match-p "\\$0\\.42" text))
            (should (string-match-p "╭─ ▾ remote control " text))
            (should (string-match-p "╭─ ▾ prompt cache " text))
            (should (string-match-p "╭─ ▾ session research " text))
            (should (string-match-p "144\\.0k / 200\\.0k" text)))
          (should (eq +agent-top--shown-id session))
          (should (eq (get-text-property (point) 'aaron-ui-board--item-id) session))
          (+agent-top--teardown))))))

(ert-deftest +agent-top-cache-reuse-is-a-reported-category-share ()
  "The board compares reads with fresh input and writes across sessions."
  (let ((rows (list (cons nil '(:input 100 :cached-read 300 :cached-write 100))
                    (cons nil '(:input 100 :cached-read 100 :cached-write 0)))))
    (pcase-let ((`(,fresh ,read ,write ,share)
                 (+agent-top--cache-summary rows)))
      (should (= fresh 200))
      (should (= read 400))
      (should (= write 100))
      (should (< (abs (- share (/ 4.0 7))) 0.0001)))
    (should (equal (+agent-top--cache-description
                    (+agent-top--cache-metrics
                     '(:input 100 :cached-read 300 :cached-write 100)))
                   "read 300 · fresh 100 · write 100 · reuse 60% · read/write 3.0x"))))

(ert-deftest +agent-top-claude-remote-requires-connection-confirmation ()
  "A live process is only connecting until Claude prints Connected."
  (let* ((connecting (+agent-top--claude-remote-scan
                      "" "· Connecting · project"))
         (connected (+agent-top--claude-remote-scan
                     (plist-get connecting :tail)
                     (concat "\e[1A· Connected · project\n"
                             "https://claude.ai/code?environment=env_test123")))
         (lost (+agent-top--claude-remote-scan
                (plist-get connected :tail) "\n· Reconnecting · project")))
    (should (eq (plist-get connecting :state) 'connecting))
    (should (eq (plist-get connected :state) 'connected))
    (should (equal (plist-get connected :url)
                   "https://claude.ai/code?environment=env_test123"))
    (should (eq (plist-get lost :state) 'connecting))))

(ert-deftest +agent-top-codex-toggle-preserves-app-server ()
  "Codex Remote actions must not stop the app-server daemon."
  (let (requested)
    (cl-letf (((symbol-function '+agent-top--codex-remote-run)
               (lambda (action) (setq requested action))))
      (dolist (case '((on . disable) (error . retry) (off . enable)))
        (let ((+agent-top--codex-remote-state (car case)))
          (+agent-top-codex-remote-toggle)
          (should (eq requested (cdr case))))))))

(ert-deftest +agent-top-codex-status-distinguishes-enabled-from-connected ()
  "The daemon can remain enabled while the phone cannot reach it."
  (should (eq (+agent-top--codex-remote-classify
               "{\"status\":\"connected\"}" 0) 'on))
  (should (eq (+agent-top--codex-remote-classify
               "Error: Remote control is enabled on Mac but the connection is errored" 1)
              'error))
  (should (eq (+agent-top--codex-remote-classify
               "{\"status\":\"connecting\"}" 0) 'connecting))
  (should (eq (+agent-top--codex-remote-classify
               "Remote control is not enabled" 1) 'off)))

(ert-deftest +agent-top-codex-remembers-enabled-but-errored-baseline ()
  "A connection error still counts as an originally enabled remote."
  (should (eq (+agent-top--codex-remote-enabled-state
               'error "Remote control is enabled on Mac but the connection is errored")
              'enabled))
  (should (eq (+agent-top--codex-remote-enabled-state
               'off "Remote control is not enabled") 'disabled))
  (should (eq (+agent-top--codex-remote-enabled-state
               'error "unrelated CLI failure") 'unknown))
  (let ((+agent-top--codex-remote-baseline 'unknown)
        (+agent-top--codex-remote-state 'error)
        (+agent-top--codex-remote-note "unrelated CLI failure"))
    (should-error (+agent-top--codex-remote-prepare-action 'enable)
                  :type 'user-error)))

(ert-deftest +agent-top-codex-exit-restores-only-abtop-changes ()
  "Exiting restores the original global flag after an abtop toggle."
  (let ((+agent-top--codex-remote-process nil)
        (+agent-top--codex-remote-owned t)
        (+agent-top--codex-remote-baseline 'enabled)
        (+agent-top--codex-remote-last-requested 'disabled)
        requested)
    (cl-letf (((symbol-function '+agent-top--run-before-exit)
               (lambda (command) (setq requested command) t))
              ((symbol-function 'executable-find) (lambda (name) name)))
      (+agent-top--restore-codex-on-exit)
      (should (equal (last requested) '("enable-remote-control")))
      (setq requested nil
            +agent-top--codex-remote-baseline 'disabled
            +agent-top--codex-remote-last-requested 'enabled)
      (+agent-top--restore-codex-on-exit)
      (should (equal (last requested) '("disable-remote-control")))
      (setq requested nil
            +agent-top--codex-remote-last-requested 'disabled)
      (+agent-top--restore-codex-on-exit)
      (should-not requested)
      (setq +agent-top--codex-remote-owned nil
            +agent-top--codex-remote-last-requested 'enabled)
      (+agent-top--restore-codex-on-exit)
      (should-not requested))))

(ert-deftest +agent-top-emacs-exit-stops-owned-claude ()
  "The exit hook stops Claude processes launched by abtop."
  (let* ((+agent-top--claude-remotes (make-hash-table :test #'equal))
         (process (make-process :name "abtop-exit-test"
                                :command (list (expand-file-name invocation-name
                                                                 invocation-directory)
                                               "-Q" "--batch" "--eval" "(sleep-for 30)")
                                :connection-type 'pty :noquery t)))
    (unwind-protect
        (progn
          (puthash "/tmp/project/" process +agent-top--claude-remotes)
          (+agent-top--stop-claude-on-exit)
          (should-not (process-live-p process))
          (should (= (hash-table-count +agent-top--claude-remotes) 0))
          (should (memq #'+agent-top--shutdown kill-emacs-hook)))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest +agent-top-exit-command-waits-for-completion ()
  "Codex restoration uses a bounded command that observes its exit status."
  (should (+agent-top--run-before-exit
           (list (expand-file-name invocation-name invocation-directory)
                 "-Q" "--batch"))))

(ert-deftest +agent-top-remote-panel-shows-confirmed-state-and-web-entry ()
  "The panel separates a live Claude process from a confirmed connection."
  (let* ((process (make-pipe-process :name "abtop-remote-test" :noquery t))
         (+agent-top--claude-remotes (make-hash-table :test #'equal))
         (+agent-top--claude-remote-errors (make-hash-table :test #'equal))
         (+agent-top--codex-remote-state 'error)
         (+agent-top--codex-remote-note "enabled, but connection errored"))
    (unwind-protect
        (progn
          (puthash "/tmp/project/" process +agent-top--claude-remotes)
          (process-put process '+agent-top--state 'connected)
          (process-put process '+agent-top--url
                       "https://claude.ai/code?environment=env_test")
          (with-temp-buffer
            (+agent-top-mode)
            (let ((inhibit-read-only t)) (+agent-top--insert-remote))
            (let ((rendered (buffer-string)))
              (should (string-match-p "codex +enabled · connection error" rendered))
              (should (string-match-p "\\[E retry\\]" rendered))
              (should (string-match-p "state +connected" rendered))
              (should (string-match-p "\\[open Claude\\]" rendered)))))
      (delete-process process))))

(ert-deftest +agent-top-claude-stop-clears-closed-project ()
  "D removes a closed Claude project and its retained console."
  (let ((+agent-top--claude-remotes (make-hash-table :test #'equal))
        (+agent-top--claude-remote-errors (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *abtop-closed-remote*")))
    (puthash "/tmp/project/" (list :reason "exit 1" :buffer buffer)
             +agent-top--claude-remote-errors)
    (+agent-top-claude-remote-stop "/tmp/project/")
    (should-not (gethash "/tmp/project/" +agent-top--claude-remote-errors))
    (should-not (buffer-live-p buffer))))

(ert-deftest +agent-top-claude-exit-shows-real-error ()
  "A failed Remote launch shows the CLI reason on the board."
  (let* ((buffer (generate-new-buffer " *abtop-remote-error-test*"))
         (+agent-top--claude-remotes (make-hash-table :test #'equal))
         (+agent-top--claude-remote-errors (make-hash-table :test #'equal))
         (+agent-top--codex-remote-state 'off))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Error: This folder is already served by a terminal `claude remote-control` on this device. Stop it first.\n"))
          (let ((reason (+agent-top--claude-remote-failure buffer 1)))
            (puthash "/tmp/project/" (list :reason reason :buffer buffer)
                     +agent-top--claude-remote-errors)
            (with-temp-buffer
              (+agent-top-mode)
              (let ((inhibit-read-only t)) (+agent-top--insert-remote))
              (should (string-match-p "already served by a terminal"
                                      (buffer-string))))))
      (kill-buffer buffer))))

(ert-deftest +agent-top-d-clears-only-claude-project ()
  "D can clear the sole project from anywhere on the board."
  (let ((+agent-top--claude-remotes (make-hash-table :test #'equal))
        (+agent-top--claude-remote-errors (make-hash-table :test #'equal)))
    (puthash "/tmp/project/" (list :reason "exit 1")
             +agent-top--claude-remote-errors)
    (with-temp-buffer
      (+agent-top-mode)
      (call-interactively (key-binding (kbd "D"))))
    (should (= (hash-table-count +agent-top--claude-remote-errors) 0))))

(ert-deftest +agent-top-d-uses-selected-claude-project ()
  "D on a project row clears that project when several are listed."
  (let ((+agent-top--claude-remotes (make-hash-table :test #'equal))
        (+agent-top--claude-remote-errors (make-hash-table :test #'equal)))
    (puthash "/tmp/a/" (list :reason "exit 1") +agent-top--claude-remote-errors)
    (puthash "/tmp/b/" (list :reason "exit 1") +agent-top--claude-remote-errors)
    (with-temp-buffer
      (+agent-top-mode)
      (let ((inhibit-read-only t)) (+agent-top--insert-remote))
      (goto-char (point-min))
      (search-forward "/tmp/b/")
      (goto-char (match-beginning 0))
      (call-interactively (key-binding (kbd "D"))))
    (should (gethash "/tmp/a/" +agent-top--claude-remote-errors))
    (should-not (gethash "/tmp/b/" +agent-top--claude-remote-errors))))

(ert-deftest +agent-top-remote-row-opens-console-not-agent ()
  "RET on a Remote row must not fall back to the selected agent session."
  (let* ((directory "/tmp/project/")
         (console (generate-new-buffer " *abtop-remote-console-test*"))
         (board (generate-new-buffer " *abtop-remote-board-test*"))
         (process (make-pipe-process :name "abtop-remote-visit-test"
                                     :buffer console :noquery t))
         (+agent-top--claude-remotes (make-hash-table :test #'equal))
         (+agent-top--claude-remote-errors (make-hash-table :test #'equal)))
    (unwind-protect
        (save-window-excursion
          (puthash directory process +agent-top--claude-remotes)
          (switch-to-buffer board)
          (+agent-top-mode)
          (let ((inhibit-read-only t)) (+agent-top--insert-remote))
          (goto-char (point-min))
          (search-forward "/tmp/project/")
          (goto-char (match-beginning 0))
          (should (eq (key-binding [mouse-1])
                      '+agent-top-mouse-visit))
          (should-error (+agent-top--session-at-point) :type 'user-error)
          (call-interactively (key-binding (kbd "RET")))
          (should (eq (window-buffer (selected-window)) console)))
      (delete-process process)
      (kill-buffer board)
      (kill-buffer console))))

(ert-deftest +agent-top-refresh-keeps-remote-row-selected ()
  "A changing quota panel must not send the cursor back to the agent row."
  (let* ((directory "/tmp/project/")
         (process (make-pipe-process :name "abtop-remote-refresh-test" :noquery t))
         (+agent-top--claude-remotes (make-hash-table :test #'equal))
         (+agent-top--claude-remote-errors (make-hash-table :test #'equal))
         (+agent-top--codex-remote-state 'off)
         (quota-lines 1))
    (unwind-protect
        (progn
          (puthash directory process +agent-top--claude-remotes)
          (with-temp-buffer
            (+agent-top-mode)
            (cl-letf (((symbol-function '+agent-top--sessions) (lambda () nil))
                      ((symbol-function '+agent-top--insert-quota)
                       (lambda () (dotimes (_ quota-lines) (insert "quota\n"))))
                      ((symbol-function '+agent-top--insert-cache) #'ignore)
                      ((symbol-function '+agent-top--insert-sessions) #'ignore)
                      ((symbol-function '+agent-top--insert-detail) #'ignore)
                      ((symbol-function '+agent-top--insert-key-hints) #'ignore))
              (+agent-top--render)
              (goto-char (point-min))
              (search-forward "/tmp/project/")
              (goto-char (match-beginning 0))
              (setq quota-lines 3)
              (+agent-top--render)
              (should (equal (get-text-property
                              (point) '+agent-top--remote-root)
                             directory)))))
      (delete-process process))))

;; A full header update or a kill reports a change; busy animation ticks do not.
(ert-deftest +agent-top-hears-session-changes ()
  (let* ((buffer (generate-new-buffer " *abtop-change*"))
         (reported nil)
         (+agent-acp-changed-functions (list (lambda (b) (push b reported)))))
    (with-current-buffer buffer
      (+agent-acp--header-updated-a :cache-enabled t)
      (should-not reported)
      (+agent-acp--header-updated-a)
      (should (equal reported (list buffer)))
      (+agent-acp--track-activity-h))
    (kill-buffer buffer)
    (should (= 2 (length reported)))))

;; Claude's usage endpoint reports 0-100 and ISO times; the ACP event may
;; carry every window at once in unifiedWindows (fractions, epoch seconds).
(ert-deftest +agent-top-records-claude-quota-sources ()
  (let ((+agent-acp--rate-limits (make-hash-table :test #'equal))
        (+agent-acp-rate-limits-file (make-temp-file "abtop-limits" nil ".json")))
    (+agent-top--claude-record-usage
     '((five_hour (utilization . 42.0) (resets_at . "2026-09-29T02:00:00Z"))
       (seven_day (utilization . 0.5) (resets_at))
       (seven_day_opus)))
    (let ((five (gethash "claude/five_hour" (+agent-acp-rate-limits)))
          (seven (gethash "claude/seven_day" (+agent-acp-rate-limits))))
      (should (= 0.42 (plist-get five :utilization)))
      (should (= (plist-get five :resets-at)
                 (float-time (date-to-time "2026-09-29T02:00:00Z"))))
      (should (= 0.005 (plist-get seven :utilization)))
      (should-not (gethash "claude/seven_day_opus" (+agent-acp-rate-limits))))
    (+agent-acp--note-rate-limit
     "claude" '((status . "allowed")
                (unifiedWindows (five_hour (utilization . 0.6) (resetsAt . 1791090463))
                                (seven_day (utilization . 0.1) (resetsAt . 1791500000)))))
    (should (= 0.6 (plist-get (gethash "claude/five_hour" (+agent-acp-rate-limits))
                              :utilization)))
    (should (= 0.1 (plist-get (gethash "claude/seven_day" (+agent-acp-rate-limits))
                              :utilization)))))

(ert-deftest +agent-top-claude-token-is-never-refreshed ()
  "An expired or missing login is reported, not renewed."
  (should (equal (+agent-top--claude-token
                  (json-serialize `((claudeAiOauth . ((accessToken . "t")
                                                      (expiresAt . ,(* 1000 (+ (float-time) 60))))))))
                 "t"))
  (should-error (+agent-top--claude-token
                 (json-serialize '((claudeAiOauth . ((accessToken . "t") (expiresAt . 1)))))))
  (should-error (+agent-top--claude-token "{}")))

(ert-deftest +agent-top-claude-fetch-sends-token-and-records ()
  "One request with the bearer token; the reply lands in the rate limits."
  (let ((+agent-acp--rate-limits (make-hash-table :test #'equal))
        (+agent-acp-rate-limits-file (make-temp-file "abtop-limits" nil ".json"))
        (+agent-top--claude-fetch nil)
        (+agent-top-claude-usage t)
        headers)
    (cl-letf (((symbol-function '+agent-top--claude-read-token)
               (lambda (callback) (funcall callback "secret-token")))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq headers url-request-extra-headers)
                 (with-current-buffer (generate-new-buffer " *usage-reply*")
                   (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                           "{\"five_hour\":{\"utilization\":12,\"resets_at\":null}}")
                   (funcall callback nil)
                   nil))))
      (+agent-top--claude-fetch)
      (should (equal (cdr (assoc "Authorization" headers)) "Bearer secret-token"))
      (should (equal (cdr (assoc "anthropic-beta" headers)) "oauth-2025-04-20"))
      (should (= 0.12 (plist-get (gethash "claude/five_hour" (+agent-acp-rate-limits))
                                 :utilization)))
      (should-not (plist-get +agent-top--claude-fetch :running))
      (should-not (plist-get +agent-top--claude-fetch :error))
      ;; A second explicit refresh inside the interval does not ask again.
      (setq headers nil)
      (+agent-top--claude-fetch)
      (should-not headers))))

(ert-deftest +agent-top-hidden-board-stops-listening ()
  "A board nobody sees unsubscribes; showing it subscribes again."
  (let ((+agent-acp-changed-functions nil)
        (board (get-buffer-create +agent-top--buffer-name))
        (visible nil))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-top--visible-window)
                   (lambda (_) visible))
                  ((symbol-function '+agent-top--timer-fire) #'ignore))
          (add-hook '+agent-acp-changed-functions #'+agent-top--session-changed)
          (+agent-top--session-changed nil)
          (should-not +agent-acp-changed-functions)
          (should-not +agent-top--refresh-timer)
          (setq visible t)
          (+agent-top--shown nil)
          (should (memq #'+agent-top--session-changed +agent-acp-changed-functions))
          (should (timerp +agent-top--refresh-timer)))
      (+agent-top--teardown)
      (kill-buffer board))))

(provide 'agent-top-tests)
;;; +agent-top-tests.el ends here
