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
    (should (string-match-p "claude session, ID abc" file-prompt))
    (should (string-match-p "/h/\\.claude/x\\.jsonl" file-prompt))
    (should (string-match-p "opencode session export ses_1" db-prompt))
    (should-not (string-match-p "{" db-prompt))))

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
  (let (shown hidden (reads 0))
    (cl-letf (((symbol-function 'posframe-workable-p) (lambda () t))
              ((symbol-function 'posframe-show)
               (lambda (buffer &rest args)
                 (setq shown (list buffer (plist-get args :poshandler) (plist-get args :accept-focus)))))
              ((symbol-function 'posframe-hide) (lambda (buffer) (setq hidden buffer)))
              ((symbol-function '+agent-history--exchanges)
               (lambda (_) (cl-incf reads) '(("user" . "plan it") ("assistant" . "planned"))))
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
          ;; Hidden for reuse, emptied, never killed.
          (should (eq hidden buffer))
          (should (buffer-live-p buffer))
          (should (= 0 (buffer-size buffer))))))))

(ert-deftest agent-c63-preview-scrolls-without-taking-the-keyboard ()
  (let ((buffer (+agent-history--preview-render
                 (list :agent "claude" :id "s" :title "T" :updated (float-time))
                 (cl-loop for i below 40 collect (cons "user" (format "line %d" i))))))
    (save-window-excursion
      (let ((selected (selected-window))
            (window (split-window)))
        (set-window-buffer window buffer)
        (set-window-start window 1)
        (+agent-history--preview-wheel `(wheel-down (,window 1 (0 . 0) 0)))
        (should (> (window-start window) 1))
        (should (eq (selected-window) selected))
        (+agent-history--preview-wheel `(wheel-up (,window 1 (0 . 0) 0)))
        (should (= (window-start window) 1))
        (should (eq (lookup-key (with-current-buffer buffer (current-local-map)) [mouse-1])
                    #'ignore))))
    (should (eq (lookup-key +agent-history-picker-map (kbd "C-M-v"))
                #'+agent-history/preview-scroll-up))))

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

;;; agent-history-tests.el ends here
