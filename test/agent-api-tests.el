;;; agent-api-tests.el --- Butler access to shared Agent runtime -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/api)
(require 'tools/agent/context)

(defmacro agent-api-tests--session (&rest body)
  "Run BODY with one isolated public session and no process or model."
  (declare (indent 0))
  `(let* ((buffer (generate-new-buffer "*agent API fixture*"))
          (row (list :buffer buffer :name "worker" :agent "codex" :origin 'manual
                     :root "/tmp/" :workspace "/fs:local:/tmp/" :target "local"
                     :native-session-id "native-one" :process-state 'live
                     :turn-state 'idle :queued 0 :queue-held 0
                     :last-prompt "private prompt"))
          (identity `((buffer . ,(buffer-name buffer)) (nativeSessionId . "native-one"))))
     (with-current-buffer buffer
       (setq-local major-mode 'agent-shell-mode default-directory "/tmp/"
                   +agent-acp-session-root "/tmp/" +agent-acp-session-origin 'manual))
     (unwind-protect
         (let ((+agent-session-private-buffer-functions nil)
               (+agent-session-private-functions nil))
           (cl-letf (((symbol-function '+agent-acp-agent-buffer-p) (lambda (candidate) (eq candidate buffer)))
                   ((symbol-function '+agent-session-snapshot) (lambda (_) row)))
             ,@body))
       (kill-buffer buffer))))

(ert-deftest agent-api-c45-list-is-bounded-metadata-and-does-not-start-or-show ()
  (agent-api-tests--session
    (cl-letf (((symbol-function '+agent-acp-start) (lambda (&rest _) (ert-fail "list started a session")))
              ((symbol-function '+agent-acp-show-buffer) (lambda (&rest _) (ert-fail "list selected a window"))))
      (let* ((response (+agent/api '((action . "list") (limit . 1))))
             (session (aref (alist-get 'sessions response) 0)))
        (should (= (alist-get 'total response) 1))
        (should (equal (alist-get 'nativeSessionId session) "native-one"))
        (should-not (string-match-p "private prompt" (prin1-to-string response))))
      (should (= (length (alist-get 'sessions (+agent/api '((offset . 100))))) 0))
      (should (= (alist-get 'total (+agent/api '((target . "other")))) 0))
      (should-error (+agent/api '((limit . 101))) :type 'user-error))))

(ert-deftest agent-api-c45-native-id-and-pool-fences-prevent-stale-actions ()
  (agent-api-tests--session
    (dolist (action '("status" "send" "stop" "read" "show"))
      (should-error (+agent/api `((action . ,action) (buffer . ,(buffer-name buffer))
                                 (nativeSessionId . "old-id"))) :type 'user-error))
    (should-error (+agent/api (append '((action . "status") (target . "other")) identity)) :type 'user-error)
    (should (alist-get 'ok (+agent/api (append '((action . "status") (target . "local")) identity))))))

(ert-deftest agent-api-c45-send-preserves-focus-and-uses-native-queue ()
  (agent-api-tests--session
    (let (sent queued)
      (cl-letf (((symbol-function '+agent-acp-state-value) (lambda (&rest _) t))
                ((symbol-function '+agent-acp-busy-p) (lambda (_) nil))
                ((symbol-function '+agent-acp-pending-prompt-count) (lambda (_) (plist-get row :queued)))
                ((symbol-function '+agent-acp-show-buffer) (lambda (_) (ert-fail "Background send stole focus")))
                ((symbol-function '+agent-acp-prompt) (lambda (&rest args) (setq sent (plist-get args :content))))
                ((symbol-function '+agent-acp-enqueue) (lambda (_ text) (setq queued text))))
        (+agent/api (append '((action . "send") (prompt . "Do the explicit task")) identity))
        (should (equal (map-elt (car sent) 'text) "Do the explicit task"))
        (setq row (plist-put row :queued 1) sent nil)
        (+agent/api (append '((action . "send") (prompt . "After the held task")) identity))
        (should (equal queued "After the held task"))
        (should-not sent)
        (setq row (plist-put row :process-state 'disconnected))
        (should-error (+agent/api (append '((action . "send") (prompt . "Do not reconnect")) identity)) :type 'user-error)))))

(ert-deftest agent-api-c45-create-starts-without-prompt-and-preserves-workspace ()
  (agent-api-tests--session
    (let (start adoption)
      (cl-letf (((symbol-function '+agent-acp-config-for) (lambda (_) 'config))
                ((symbol-function '+agent-workspace) (lambda (_) "/tmp/execute/"))
                ((symbol-function '+agent-acp-start) (lambda (&rest args) (setq start args) buffer))
                ((symbol-function '+agent-acp-adopt) (lambda (_ &rest args) (setq adoption args)))
                ((symbol-function '+agent-acp-prompt) (lambda (&rest _) (ert-fail "create submitted a task"))))
        (+agent/api `((action . "create") (root . ,temporary-file-directory) (agent . "codex")))
        (should (equal (plist-get start :directory) "/tmp/execute/"))
        (should-not (plist-get start :focus))
        (should (eq (plist-get adoption :origin) 'manual))
        (should-error (+agent/api `((action . "create") (root . ,temporary-file-directory)
                                   (agent . "codex") (target . "another-target"))) :type 'user-error)
        ;; A local project may have an explicitly configured remote workspace.
        ;; The execution fence belongs to that workspace, not the notes root.
        (cl-letf (((symbol-function '+agent-workspace) (lambda (_) "/fs:pi-api-remote:/work/")))
          (+agent/api `((action . "create") (root . ,temporary-file-directory)
                        (agent . "codex") (target . "pi-api-remote")))
          (should (equal (plist-get start :directory) "/fs:pi-api-remote:/work/")))))))

(ert-deftest agent-api-c45-read-is-paged-and-excludes-editable-draft ()
  (agent-api-tests--session
    (with-current-buffer buffer
      (insert "result\n")
      (setq-local comint-last-prompt (cons (point-min-marker) (copy-marker (point))))
      (insert "PRIVATE DRAFT"))
    (cl-letf (((symbol-function '+agent-render-flush) #'ignore))
      (let ((page (+agent/api (append '((action . "read") (offset . 2) (limit . 3)) identity))))
        (should (equal (alist-get 'text page) "sul"))
        (should (= (alist-get 'total page) 7))
        (should (= (alist-get 'nextOffset page) 5)))
      (should (equal (alist-get 'text (+agent/api (append '((action . "read") (offset . 5)) identity))) "t\n"))
      (should-error (+agent/api (append '((action . "read") (limit . 32001)) identity)) :type 'user-error))))

(ert-deftest agent-api-c45-stop-and-show-delegate-to-the-owners ()
  (agent-api-tests--session
    (let (stopped shown)
      (cl-letf (((symbol-function '+agent-acp-stop) (lambda (b) (setq stopped b)))
                ((symbol-function '+agent-acp-show-buffer) (lambda (b) (setq shown b))))
        (+agent/api (append '((action . "stop")) identity))
        (should (eq stopped buffer))
        (should-not shown)
        (+agent/api (append '((action . "show")) identity))
        (should (eq shown buffer))))))

(ert-deftest agent-api-c50-status-reports-capabilities-and-keeps-unknown-usage-null ()
  (agent-api-tests--session
    (setq row (append row (list :capabilities (list :fork t :steer nil))))
    (cl-letf (((symbol-function '+agent-acp-usage) (lambda (_) (list :report-seq 0 :context-size 0))))
      (let* ((session (alist-get 'session (+agent/api `((action . "status") ,@identity)))))
        (should (eq (alist-get 'fork (alist-get 'capabilities session)) t))
        (should (eq (alist-get 'steer (alist-get 'capabilities session)) :json-false))
        (should-not (alist-get 'usage session))))
    (cl-letf (((symbol-function '+agent-acp-usage)
               (lambda (_) (list :report-seq 2 :context-used 10 :context-size 100 :cached-read 7))))
      (let ((usage (alist-get 'usage (alist-get 'session (+agent/api `((action . "status") ,@identity))))))
        (should (= (alist-get 'contextUsed usage) 10))
        (should (= (alist-get 'cachedRead usage) 7))))
    (should-not (alist-get 'usage (aref (alist-get 'sessions (+agent/api '((action . "list")))) 0)))))

(ert-deftest agent-api-c50-status-reply-survives-json-encoding ()
  (require 'remote-gateway)
  (agent-api-tests--session
    (cl-letf (((symbol-function '+agent-acp-usage) (lambda (_) (list :report-seq 0))))
      (let* ((reply (+agent/api `((action . "status") ,@identity)))
             (decoded (json-parse-string (remote-gateway--encode reply)
                                         :object-type 'alist :false-object :json-false :null-object nil))
             (session (alist-get 'session decoded)))
        (should-not (alist-get 'usage session))
        (should-not (alist-get 'stopReason session))
        (should (eq (alist-get 'accepted decoded) :json-false))
        (should (eq (alist-get 'busy session) :json-false))
        (should (eq (alist-get 'fork (alist-get 'capabilities session)) :json-false))))))

(ert-deftest agent-api-c50-worktree-target-fence-runs-before-creation ()
  (cl-letf (((symbol-function '+agent-workspace) (lambda (_) "/fs:worker:/work/"))
            ((symbol-function '+agent-worktree--start)
             (lambda (&rest _) (ert-fail "Mismatched target created a worktree"))))
    (should-error (+agent/api `((action . "create") (root . ,temporary-file-directory)
                               (agent . "codex") (worktree . "try") (target . "another")))
                  :type 'user-error)))

(ert-deftest agent-api-c50-create-with-worktree-returns-its-branch-and-path ()
  (agent-api-tests--session
    (let (call)
      (cl-letf (((symbol-function '+agent-worktree--start)
                 (lambda (agent name &rest keys)
                   (setq call (list agent name (plist-get keys :group) default-directory))
                   (list :buffer buffer :branch "agent/try" :path "/tmp/try")))
                ((symbol-function 'remote-require-on-client) #'ignore)
                ((symbol-function 'remote-resolve-emacs-file-name) #'identity))
        (let ((response (+agent/api '((action . "create") (root . "/tmp/") (agent . "codex")
                                      (worktree . "try") (group . "g")))))
          (should (equal call '("codex" "try" "g" "/tmp/")))
          (should (equal (alist-get 'path (alist-get 'worktree response)) "/tmp/try")))))))

(provide 'agent-api-tests)
