;;; agent-history-tests.el --- One history across agents (C63) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/history)

(defmacro agent-history-tests--home (&rest body)
  "Run BODY with `home' bound to a fresh home directory the readers use."
  (declare (indent 0))
  `(let ((home (file-name-as-directory (make-temp-file "agent-history-" t))))
     (unwind-protect
         (cl-letf (((symbol-function '+agent-history--home) (lambda (_) home)))
           ,@body)
       (delete-directory home t))))

(defun agent-history-tests--write (file &rest lines)
  "Write FILE with LINES, each a JSON object given as an alist."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (dolist (line lines) (insert (json-encode line) "\n"))))

(defun agent-history-tests--sqlite (db &rest statements)
  "Run STATEMENTS on a new sqlite database DB."
  (make-directory (file-name-directory db) t)
  (with-temp-buffer
    (should (eq 0 (call-process "sqlite3" nil t nil db (string-join statements ";"))))))

(ert-deftest agent-c63-claude-conversations-by-folder-with-their-title ()
  (agent-history-tests--home
    (let* ((project (expand-file-name "work/proj/" home))
           (folder (+agent-history--claude-folder project))
           (file (expand-file-name (format ".claude/projects/%s/abc.jsonl" folder) home)))
      (agent-history-tests--write
       file
       `((type . "user") (isMeta . t) (sessionId . "abc") (cwd . ,(directory-file-name project))
         (message . ((content . "<local-command-caveat>x</local-command-caveat>"))))
       `((type . "user") (sessionId . "abc") (cwd . ,(directory-file-name project)) (gitBranch . "main")
         (message . ((content . [((type . "text") (text . "Fix the parser"))]))))
       '((type . "assistant") (message . ((content . [((type . "text") (text . "Done."))]))))
       '((type . "ai-title") (aiTitle . "Parser fix") (sessionId . "abc")))
      ;; Another folder's conversation is not this project's.
      (agent-history-tests--write
       (expand-file-name ".claude/projects/-elsewhere/zzz.jsonl" home)
       '((type . "user") (sessionId . "zzz") (cwd . "/elsewhere") (message . ((content . "hi")))))
      (let ((rows (+agent-history--claude home (list project))))
        (should (= (length rows) 1))
        (should (equal (plist-get (car rows) :title) "Parser fix"))
        (should (equal (plist-get (car rows) :prompt) "Fix the parser"))
        (should (equal (plist-get (car rows) :branch) "main"))
        (should (equal (mapcar #'cdr (+agent-history--exchanges (car rows)))
                       '("Fix the parser" "Done.")))
        (should (= (length (+agent-history--claude home nil)) 2))))))

(ert-deftest agent-c63-codex-threads-without-automated-ones ()
  (skip-unless (executable-find "sqlite3"))
  (agent-history-tests--home
    (agent-history-tests--sqlite
     (expand-file-name ".codex/state_5.sqlite" home)
     "CREATE TABLE threads (id TEXT, title TEXT, first_user_message TEXT, cwd TEXT,
        updated_at INTEGER, git_branch TEXT, rollout_path TEXT, archived INTEGER,
        source TEXT)"
     "INSERT INTO threads VALUES ('t1','Plan','plan it','/w/p',200,'dev','/r/1.jsonl',0,'cli')"
     "INSERT INTO threads VALUES ('t2','','only prompt','/w/p/sub',100,NULL,'/r/2.jsonl',0,'vscode')"
     "INSERT INTO threads VALUES ('t3','Polish','x','/w/p',300,NULL,'/r/3.jsonl',0,'exec')"
     "INSERT INTO threads VALUES ('t4','Review','x','/w/p',300,NULL,'/r/4.jsonl',0,'{\"subagent\":\"review\"}')"
     "INSERT INTO threads VALUES ('t5','Old','x','/w/p',300,NULL,'/r/5.jsonl',1,'cli')"
     "INSERT INTO threads VALUES ('t6','Other','x','/w/q',300,NULL,'/r/6.jsonl',0,'cli')")
    (let ((rows (+agent-history--codex home '("/w/p/"))))
      (should (equal (mapcar (lambda (row) (plist-get row :id)) rows) '("t1" "t2")))
      (should (equal (plist-get (nth 1 rows) :title) "only prompt"))
      (should (equal (plist-get (car rows) :branch) "dev"))
      (should (string-suffix-p "/r/1.jsonl" (plist-get (car rows) :file))))))

(ert-deftest agent-c63-opencode-sessions-someone-spoke-in ()
  (skip-unless (executable-find "sqlite3"))
  (agent-history-tests--home
    (agent-history-tests--sqlite
     (+agent-history--opencode-db home)
     "CREATE TABLE session (id TEXT, title TEXT, directory TEXT, time_updated INTEGER,
        parent_id TEXT, time_archived INTEGER)"
     "CREATE TABLE message (id TEXT, session_id TEXT, data TEXT)"
     "CREATE TABLE part (id TEXT, message_id TEXT, session_id TEXT, time_created INTEGER, data TEXT)"
     "INSERT INTO session VALUES ('s1','Refactor','/w/p',5000,NULL,NULL)"
     "INSERT INTO session VALUES ('s2','New session','/w/p',6000,NULL,NULL)"
     "INSERT INTO message VALUES ('m1','s1','{\"role\":\"user\"}')"
     "INSERT INTO message VALUES ('m2','s1','{\"role\":\"assistant\"}')"
     "INSERT INTO part VALUES ('p1','m1','s1',1,'{\"type\":\"text\",\"text\":\"split it\"}')"
     "INSERT INTO part VALUES ('p2','m2','s1',2,'{\"type\":\"text\",\"text\":\"split.\"}')")
    (let ((rows (+agent-history--opencode home '("/w/p/"))))
      (should (equal (mapcar (lambda (row) (plist-get row :id)) rows) '("s1")))
      (should (= (plist-get (car rows) :turns) 1))
      (should (= (plist-get (car rows) :updated) 5.0))
      (should (equal (+agent-history--exchanges (car rows))
                     '(("user" . "split it") ("assistant" . "split.")))))))

(defun agent-history-tests--agy-store (home)
  "Create an Antigravity summary store under HOME and return its path."
  (let ((db (expand-file-name ".gemini/antigravity-cli/conversation_summaries.db" home)))
    (agent-history-tests--sqlite
     db
     "CREATE TABLE conversation_summaries (conversation_id TEXT PRIMARY KEY,
 title TEXT, preview TEXT, step_count INTEGER, last_modified_time TEXT,
 workspace_uris TEXT, parent_conversation_id TEXT DEFAULT '', nesting_depth INTEGER DEFAULT 0)"
     "CREATE TABLE conversations_fts (conversation_id TEXT, chunk_index INTEGER,
 archived INTEGER DEFAULT 0, user_step_byte_offsets BLOB, agent_step_byte_offsets BLOB,
 user_body TEXT, agent_body TEXT)"
     "INSERT INTO conversation_summaries VALUES
 ('a','Résumé','first',14,'1970-01-01 00:00:20+00:00',
  '[\"file:///elsewhere\",\"file:///w/proj%20space/sub\"]','',0),
 ('other','Other','x',2,'1970-01-01 00:00:30+00:00','[\"file:///w/proj%20space-other\"]','',0),
 ('child','Child','x',2,'1970-01-01 00:00:40+00:00','[\"file:///w/proj%20space\"]','a',1),
 ('empty','New','',0,'1970-01-01 00:00:50+00:00','[\"file:///w/proj%20space\"]','',0),
 ('archived','Old','x',2,'1970-01-01 00:01:00+00:00','[\"file:///w/proj%20space\"]','',0),
 ('unindexed','Recent','hi',2,'1970-01-01 00:00:25+00:00','[\"file:///w/proj%20space\"]','',0)"
     "INSERT INTO conversations_fts VALUES
 ('a',-1,0,'','','',''),
 ('a',0,0,CAST('[[2,0],[8,7]]' AS BLOB),CAST('[[3,0],[9,7]]' AS BLOB),'中文
Next
','答复
Done
'),
 ('a',1,0,'[[12,0]]','[[13,0]]','Last
','Final
'),
 ('other',0,0,'[[0,0]]','[[1,0]]','x','y'),
 ('child',0,0,'[[0,0]]','[[1,0]]','x','y'),
 ('archived',0,1,'[[0,0]]','[[1,0]]','x','y')")
    (let ((conversation (expand-file-name ".gemini/antigravity-cli/conversations/a.db" home)))
      (agent-history-tests--sqlite
       conversation
       "PRAGMA journal_mode=WAL"
       "CREATE TABLE steps (idx INTEGER PRIMARY KEY, step_type INTEGER, step_payload BLOB)"
       "INSERT INTO steps VALUES
 (2,14,X'9A010A1A080A06E4B8ADE69687'),
 (3,15,X'A201080A06E7AD94E5A48D'),
 (8,14,X'9A01081A060A044E657874'),
 (9,15,X'A201060A04446F6E65'),
 (12,14,X'9A01081A060A044C617374'),
 (13,15,X'A201070A0546696E616C'),
 (14,15,X'A201081A06736563726574'),
 (15,15,X'FF'),
 (16,6,X'A201060A04746F6F6C')")
      (copy-file conversation
                 (expand-file-name ".gemini/antigravity-cli/conversations/unindexed.db" home))
      conversation)))

(ert-deftest agent-c63-agy-history-scopes-all-workspaces-and-skips-nonconversations ()
  (skip-unless (executable-find "sqlite3"))
  (agent-history-tests--home
    (agent-history-tests--agy-store home)
    (cl-letf (((symbol-function '+agent-history-scope) (lambda (&rest _) '("/w/proj space/"))))
      (let ((rows (+agent-history-list home)))
        (should (equal (mapcar (lambda (row) (plist-get row :id)) rows) '("unindexed" "a")))
        (let ((row (cadr rows)))
          (should (equal (plist-get row :agent) "agy"))
          (should (equal (plist-get row :cwd) "/w/proj space/sub"))
          (should (equal (plist-get row :title) "Résumé"))
          (should (= (plist-get row :turns) 3))
          (should (= (plist-get row :updated) 20)))
        (should (car (+agent-history--page (car rows))))
        (should (equal (mapcar (lambda (row) (plist-get row :id)) (+agent-history-list home t))
                       '("other" "unindexed" "a")))
        (should (equal (mapcar (lambda (row) (plist-get row :id)) (+agent-history--agy home '("/")))
                       '("other" "unindexed" "a")))))))

(ert-deftest agent-c63-agy-preview-reads-native-steps-without-thoughts-or-search-index ()
  (skip-unless (executable-find "sqlite3"))
  (agent-history-tests--home
    (let* ((db (agent-history-tests--agy-store home))
           (row (list :agent "agy" :id "a" :db db))
           (+agent-history--page-messages 2)
           (first (+agent-history--page row)))
      (should (equal (car first) '(("assistant" . "Final") ("user" . "Last"))))
      (let ((second (+agent-history--page row (cdr first))))
        (should (equal (car second) '(("assistant" . "Done") ("user" . "Next"))))
        (let ((third (+agent-history--page row (cdr second))))
          (should (equal (car third) '(("assistant" . "答复") ("user" . "中文"))))
          (should-not (car (+agent-history--page row (cdr third)))))))))

(ert-deftest agent-c63-agy-fallback-points-to-its-read-only-database ()
  (skip-unless (executable-find "sqlite3"))
  (agent-history-tests--home
    (let ((prompt (+agent-history-transcript-prompt
                   (list :agent "agy" :id "a" :cwd "/w" :title "T"
                         :db (agent-history-tests--agy-store home)))))
      (should (string-match-p "sqlite3 -readonly" prompt))
      (should (string-match-p "conversations/a.db" prompt))
      (should-not (string-match-p "opencode" prompt))
      (should (string-match "transcript (SQLite): `\\([^`]+\\)`" prompt))
      (should (string-match-p "13|15|A201070A0546696E616C"
                              (shell-command-to-string (match-string 1 prompt)))))))

(ert-deftest agent-c63-agy-native-restore-recognizes-upstream-identity ()
  (let (restored)
    (with-temp-buffer
      (setq-local +agent-acp-session-agent "antigravity")
      (cl-letf (((symbol-function '+agent-history--live-buffer) (lambda (_) nil))
                ((symbol-function '+agent-history--directory) (lambda (_ fallback) fallback))
                ((symbol-function '+agent-acp-restart-native)
                 (lambda (_ id) (setq restored id)))
                ((symbol-function '+agent-history-continue)
                 (lambda (&rest _) (ert-fail "Same provider must resume natively"))))
        (+agent-history-open '(:agent "agy" :id "saved-agy") "antigravity" (current-buffer))
        (should (equal restored "saved-agy"))
        (should +agent-acp-strict-restore)))))

(ert-deftest agent-c63-history-merges-agents-newest-first-without-scratch ()
  (agent-history-tests--home
    (let ((+agent-history-readers
           `(("a" . ,(lambda (_ _) (list (list :agent "claude" :id "1" :updated 10.0 :cwd "/w")
                                         (list :agent "claude" :id "2" :updated 40.0
                                               :cwd "/private/var/folders/x/T/probe"))))
             ("b" . ,(lambda (_ _) (list (list :agent "codex" :id "3" :updated 20.0 :cwd "/w")))))))
      (cl-letf (((symbol-function '+agent-history-scope) (lambda (&rest _) '("/w/"))))
        (should (equal (mapcar (lambda (row) (plist-get row :id))
                               (+agent-history-list home))
                       '("3" "1")))))))

(ert-deftest agent-c63-transcript-prompt-names-how-to-read-it ()
  (let ((file-prompt (+agent-history-transcript-prompt
                      (list :agent "claude" :id "abc" :cwd "/w" :title "T" :file "/h/.claude/x.jsonl")))
        (db-prompt (+agent-history-transcript-prompt
                    (list :agent "opencode" :id "ses_1" :cwd "/w" :title "T"))))
    (should (string-match-p "claude conversation (abc" file-prompt))
    (should (string-match-p "/h/\\.claude/x\\.jsonl" file-prompt))
    (should (string-match-p "opencode session export ses_1" db-prompt))
    (should-not (string-match-p "{" db-prompt)))
  ;; A long first message as title stays one short line: a link, not content.
  (let ((prompt (+agent-history-transcript-prompt
                 (list :agent "codex" :id "x" :cwd "/w" :file "/r/x.jsonl"
                       :title (concat "task:\n" (make-string 3000 ?y))))))
    (should (< (length prompt) 600))
    (should-not (string-match-p "\n.*\n.*\n.*\n" prompt))))

(ert-deftest agent-c63-open-routes-by-agent ()
  (let (calls)
    (cl-letf (((symbol-function '+agent-history--live-buffer) (lambda (_) nil))
              ((symbol-function '+agent-history--directory) (lambda (_ fallback) fallback))
              ((symbol-function '+agent-history--restore) (lambda (row _) (push (list 'restore (plist-get row :id)) calls)))
              ((symbol-function '+agent-history-continue) (lambda (row agent _) (push (list 'continue (plist-get row :id) agent) calls)))
              ((symbol-function '+agent-acp-restart-native) (lambda (_ id) (push (list 'native id) calls))))
      ;; No session: its own agent restores it.
      (+agent-history-open (list :agent "claude" :id "c1"))
      ;; Another agent continues it from the transcript.
      (+agent-history-open (list :agent "claude" :id "c2") "codex")
      ;; In a session of the same agent it restores there, strictly.
      (with-temp-buffer
        (setq-local +agent-acp-session-agent "claude")
        (+agent-history-open (list :agent "claude" :id "c3") "claude" (current-buffer))
        (should +agent-acp-strict-restore))
      (should (equal (nreverse calls)
                     '((restore "c1") (continue "c2" "codex") (native "c3")))))))

(ert-deftest agent-c63-failed-restore-continues-from-the-transcript ()
  (let ((row (list :agent "codex" :id "x9" :file "/r/x9.jsonl"))
        continued killed)
    (puthash "x9" row +agent-history--rows)
    (with-temp-buffer
      (setq-local +agent-acp-session-agent "codex")
      (let ((failed (current-buffer)))
        (cl-letf (((symbol-function 'run-at-time) (lambda (_ _ fn &rest args) (apply fn args)))
                  ((symbol-function '+agent-history-continue)
                   (lambda (r agent _) (setq continued (list (plist-get r :id) agent)) 'new))
                  ((symbol-function '+agent-acp-kill) (lambda (b) (setq killed b))))
          (should (+agent-history-restore-failed-h failed "x9"))
          (should (equal continued '("x9" "codex")))
          (should (eq killed failed))
          ;; Unknown conversations keep the plain failure.
          (should-not (+agent-history-restore-failed-h failed "unknown")))))))

(ert-deftest agent-c63-label-has-the-columns ()
  (cl-letf (((symbol-function '+agent-acp--session-open-p) (lambda (_) t)))
    (let ((label (substring-no-properties
                  (+agent-history-label (list :agent "codex" :id "abcdef12-x" :updated (float-time)
                                              :turns 3 :branch "main" :cwd "/w/proj"
                                              :title "Fix\nit")))))
      (should (string-match-p "\\`codex +now +3↵ +main +proj +Fix it" label))
      (should (string-match-p "● open" label)))))

(ert-deftest agent-c63-preview-floats-like-an-lsp-hover ()
  (require 'posframe nil t)
  (skip-unless (fboundp 'posframe-show))
  (let (shown closed (reads 0))
    (cl-letf (((symbol-function 'posframe-workable-p) (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (buffer &rest args)
                 (setq shown (list buffer (plist-get args :poshandler) (plist-get args :accept-focus)))))
              ((symbol-function 'posframe-delete-frame) (lambda (buffer) (setq closed buffer)))
              ((symbol-function '+agent-history--page)
               (lambda (_ &optional _next)
                 (cl-incf reads) (list '(("assistant" . "planned") ("user" . "plan it")))))
              ((symbol-function 'display-buffer) (lambda (&rest _) (error "No window preview"))))
      (let ((state (+agent-history--preview-state))
            (row (list :agent "codex" :id "p1" :title "Plan\nwith a newline" :updated (float-time)))
            (layout (current-window-configuration)))
        (funcall state 'preview row)
        (funcall state 'preview (list :agent "codex" :id "p2" :updated (float-time)))
        (funcall state 'preview row)
        ;; A conversation is read once per picker.
        (should (= reads 2))
        (let ((buffer (car shown)))
          (should (equal (buffer-name buffer) +agent-history-preview-buffer))
          (should (eq (cadr shown) #'+agent-history--preview-poshandler))
          (should-not (nth 2 shown))
          (should (string-prefix-p "Plan with a newline\n"
                                   (with-current-buffer buffer (buffer-string))))
          (should (compare-window-configurations layout (current-window-configuration)))
          (funcall state 'exit nil)
          ;; Release the frame; only its empty buffer remains reusable.
          (should (eq closed buffer))
          (should (buffer-live-p buffer))
          (should (= 0 (buffer-size buffer))))))))

(ert-deftest agent-c63-resume-preview-is-destroyed-on-accept-and-cancel ()
  "Each Resume invocation releases its actual child frame on exit."
  (skip-unless (display-graphic-p))
  (require 'posframe)
  (require 'consult)
  (let ((+agent-history-preview-buffer (generate-new-buffer-name " *resume-preview-test*"))
        (row (list :agent "codex" :id "preview-test" :title "Resume preview"
                   :updated (float-time)))
        frames)
    (unwind-protect
        (dolist (action '(accept cancel))
          (let (shown cancelled)
            (cl-letf* ((show (symbol-function '+agent-history--show-preview))
                       ((symbol-function '+agent-history--page)
                        (lambda (&rest _) (list '(("assistant" . "Preview text")))))
                       ((symbol-function '+agent-history--show-preview)
                        (lambda (buffer)
                          (prog1 (funcall show buffer)
                            (setq shown (buffer-local-value 'posframe--frame buffer))
                            (setq unread-command-events
                                  (list (if (eq action 'cancel) ?\C-g ?\r)))))))
              (let ((selected
                     (condition-case nil
                         (with-timeout (5 (error "Resume preview did not exit"))
                           (+agent-history-read (list row)))
                       (quit (setq cancelled t) nil))))
                (if (eq action 'accept)
                    (should (eq selected row))
                  (should cancelled))))
            (should (framep shown))
            (should-not (frame-live-p shown))
            (should-not (memq shown frames))
            (push shown frames)
            (let ((buffer (get-buffer +agent-history-preview-buffer)))
              (should (buffer-live-p buffer))
              (should (zerop (buffer-size buffer))))))
      (posframe-delete +agent-history-preview-buffer))))

(ert-deftest agent-c63-preview-scrolls-without-taking-the-keyboard ()
  (let ((buffer (+agent-history--preview-render
                 (list :agent "claude" :id "s" :title "T" :updated (float-time))
                 (list (cl-loop for i below 40 collect (cons "user" (format "line %d" i)))))))
    (save-window-excursion
      (let ((selected (selected-window))
            (window (split-window)))
        (set-window-buffer window buffer)
        (set-window-start window 1)
        (+agent-history--preview-wheel `(wheel-down (,window 1 (0 . 0) 0)))
        (should (> (window-start window) 1))
        ;; Point follows, so redisplay keeps the scroll.
        (should (= (window-point window) (window-start window)))
        (should (eq (selected-window) selected))
        (+agent-history--preview-wheel `(wheel-up (,window 1 (0 . 0) 0)))
        (should (= (window-start window) 1))
        (should (eq (lookup-key (with-current-buffer buffer (current-local-map)) [mouse-1])
                    #'ignore))))
    (should (eq (lookup-key +agent-history-picker-map (kbd "C-M-v"))
                #'+agent-history/preview-scroll-up))
    ;; The picker sends the wheel over the box to it, and leaves others alone.
    (cl-letf (((symbol-function '+agent-history--preview-frame-p) (lambda (f) (eq f (selected-frame)))))
      (let ((last-input-event `(wheel-down (,(selected-window) 1 (0 . 0) 0))))
        (should (eq (+agent-history--wheel-filter 'cmd) 'cmd)))
      (let ((last-input-event ?a))
        (should-not (+agent-history--wheel-filter 'cmd))))
    ;; macOS sends a frame switch before a wheel event over the box: ignored,
    ;; and only while a picker is open.
    (cl-letf (((symbol-function '+agent-history--preview-frame-p) (lambda (f) (eq f 'box))))
      (should (+agent-history--switch-frame-a '(switch-frame box)))
      (should-not (+agent-history--switch-frame-a '(switch-frame other))))
    (let (during)
      (cl-letf (((symbol-function 'require)
                 (let ((require (symbol-function 'require)))
                   (lambda (feature &rest args)
                     (or (eq feature 'consult) (apply require feature args)))))
                ((symbol-function 'consult--read)
                 (lambda (&rest _)
                   (setq during (advice-member-p #'+agent-history--switch-frame-a
                                                 'handle-switch-frame))
                   nil)))
        (+agent-history-read (list (list :agent "codex" :id "x" :updated 1.0)))
        (should during)
        (should-not (advice-member-p #'+agent-history--switch-frame-a
                                     'handle-switch-frame))))))

(ert-deftest agent-c63-preview-sits-beside-a-tall-list ()
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'frame-char-height) (lambda (&rest _) 20))
            ((symbol-function 'frame-pixel-width) (lambda (&rest _) 3000))
            ((symbol-function 'frame-pixel-height) (lambda (&rest _) 1600))
            ((symbol-function 'window-pixel-edges) (lambda (&rest _) '(0 1300 3000 1600)))
            ((symbol-function 'window-frame) (lambda (&rest _) (selected-frame))))
    (cl-letf (((symbol-function 'window-total-height) (lambda (&rest _) 13)))
      ;; Inside the minibuffer area, right of the candidates, as tall as the list.
      (should (equal (+agent-history--preview-geometry (selected-frame))
                     (list (- 3000 860 20) 1300 84 12))))
    (cl-letf (((symbol-function 'window-total-height) (lambda (&rest _) 1)))
      ;; A one-line minibuffer: just above it.
      (pcase-let ((`(,_ ,y ,_ ,lines) (+agent-history--preview-geometry (selected-frame))))
        (should (= lines 20))
        (should (<= (+ y (* 20 21)) 1300))))))


(ert-deftest agent-c63-long-transcripts-are-read-a-page-at-a-time ()
  (agent-history-tests--home
    (let* ((file (expand-file-name "t.jsonl" home))
           (row (list :agent "claude" :id "t" :file file))
           (+agent-history-page-bytes 4096))
      (apply #'agent-history-tests--write file
             (cl-loop for i below 40
                      append (list `((type . "user") (message . ((content . ,(format "q%d" i)))))
                                   ;; Tool output between turns, larger than a window.
                                   `((type . "user") (message . ((content . [((type . "tool_result") (content . ,(make-string 5000 ?x)))]))))
                                   `((type . "assistant") (message . ((content . [((type . "text") (text . ,(format "a%d" i)))])))))))
      (let ((reads 0) (seen nil) (page (list nil)) next)
        (cl-letf* ((read (symbol-function '+agent-history--read-bytes))
                   ((symbol-function '+agent-history--read-bytes)
                    (lambda (file start end)
                      (cl-incf reads)
                      (should (<= (- end start) 4096))
                      (funcall read file start end))))
          (setq page (+agent-history--page row))
          ;; The newest first, a page's worth, from bounded windows.
          (should (equal (car (car page)) '("assistant" . "a39")))
          (should (<= (length (car page)) +agent-history--page-messages))
          (should (<= reads +agent-history--page-windows))
          (setq next (cdr page))
          (should (integerp next))
          ;; Walking on reaches the first message and stops.
          (setq seen (car page))
          (while next
            (let ((older (+agent-history--page row next)))
              (should (or (null (cdr older)) (< (cdr older) next)))
              (setq seen (append seen (car older)) next (cdr older))))
          (should (equal (car (last seen)) '("user" . "q0")))
          (should (= (length seen) 80)))))))

(ert-deftest agent-c63-preview-loads-older-pages-as-it-scrolls ()
  (let* ((row (list :agent "claude" :id "s" :title "T" :updated (float-time)))
         (pages (cons (cl-loop for i below 6 collect (cons "user" (format "new %d" i))) 100))
         (asked nil))
    (cl-letf (((symbol-function '+agent-history--page)
               (lambda (_ next)
                 (push next asked)
                 (cons (list (cons "assistant" "older")) nil))))
      (with-current-buffer (+agent-history--preview-render row pages)
        (should (string-match-p "earlier messages load" (buffer-string)))
        (should (+agent-history--preview-more))
        (should (equal asked '(100)))
        ;; Appended to the cached pages, footer now the start.
        (should (equal (car (last (car pages))) '("assistant" . "older")))
        (should (null (cdr pages)))
        (should (string-match-p "older\n\n— start of conversation —\\'" (buffer-string)))
        (should-not (+agent-history--preview-more))))))

(ert-deftest agent-c63-view-reads-a-conversation-like-a-file ()
  (agent-history-tests--home
    (let* ((file (expand-file-name "v.jsonl" home))
           (row (list :agent "claude" :id "v" :file file :title "Long one"
                      :updated (float-time)))
           (+agent-history-page-bytes 2048))
      (apply #'agent-history-tests--write file
             (cl-loop for i below 50
                      append (list `((type . "user") (message . ((content . ,(format "q%d" i)))))
                                   `((type . "assistant") (message . ((content . [((type . "text") (text . ,(format "a%d" i)))])))))))
      (let ((buffer (+agent-history-view row)))
        (unwind-protect
            (with-current-buffer buffer
              (should buffer-read-only)
              (should +agent-history-view-mode)
              ;; Newest loaded, oldest not yet; in reading order.
              (should (string-match-p "## You\n\nq49\n\n## Agent\n\na49\n\n\\'" (buffer-string)))
              (should-not (string-match-p "\nq0\n" (buffer-string)))
              (should (string-match-p "earlier messages: press <" (buffer-string)))
              (while +agent-history--view-next
                (+agent-history/view-earlier))
              (should (string-match-p "start of conversation -->\n\n## You\n\nq0\n" (buffer-string)))
              (should (= 1 (agent-history-tests--count "^## You\n\nq25$"))))
          (kill-buffer buffer))))))

(defun agent-history-tests--count (regexp)
  "Count REGEXP in the current buffer."
  (save-excursion (goto-char (point-min)) (how-many regexp)))

(ert-deftest agent-c63-cmd-return-views-instead-of-opening ()
  (let (viewed opened)
    (cl-letf (((symbol-function '+agent-history-list) (lambda (&rest _) (list (list :agent "codex" :id "r"))))
              ((symbol-function '+agent-history-read)
               (lambda (rows &rest _) (setq +agent-history--picker-action 'view) (car rows)))
              ((symbol-function '+agent-history-view) (lambda (row) (setq viewed row)))
              ((symbol-function '+agent-history-open) (lambda (&rest args) (setq opened args))))
      (with-temp-buffer (+agent/history))
      (should (equal (plist-get viewed :id) "r"))
      (should-not opened)
      (should-not +agent-history--picker-action))
    (should (eq (lookup-key +agent-history-picker-map (kbd "M-RET")) #'+agent-history/picker-view))))

;;; agent-history-tests.el ends here
