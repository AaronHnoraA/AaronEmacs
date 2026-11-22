;;; agent-collab-tests.el --- Document collaboration: authorship, threads, direct changes -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/acp)
(require 'tools/agent/sessions)
(require 'tools/agent/binding)
(require 'tools/agent/collab)

(defmacro agent-collab-tests--agent-buffer (var &rest body)
  "Bind VAR to a fake agent buffer that `+agent-acp-agent-buffer-p' accepts."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer "*agent collab fixture*")))
     (unwind-protect
         (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                    (lambda (buffer) (eq buffer ,var))))
           (with-current-buffer ,var
             (setq-local default-directory "/tmp/" +agent-acp-session-root "/tmp/"
                         +agent-acp-session-name "claude·SAI" +agent-acp-session-agent "claude"))
           ,@body)
       (kill-buffer ,var))))

;;;; C66: an Agent process knows which session it is.

(ert-deftest agent-collab-c66-client-environment-names-the-session ()
  (agent-collab-tests--agent-buffer buffer
    (let* ((args (+agent-acp--session-environment-a
                  (list :context-buffer buffer :command "claude" :environment-variables '("A=1"))))
           (token (+agent-acp-session-token buffer)))
      (should (stringp token))
      (should (member (concat "NOEMA_AGENT_SESSION=" token) (plist-get args :environment-variables)))
      (should (member "A=1" (plist-get args :environment-variables)))
      ;; A restart builds a new client for the same session: same token.
      (should (equal (+agent-acp-session-token buffer) token))
      (should (equal (+agent-acp-session-buffer-for-token token) buffer))))
  (with-temp-buffer
    (should (equal (+agent-acp--session-environment-a
                    (list :context-buffer (current-buffer) :command "x"))
                   (list :context-buffer (current-buffer) :command "x")))))

(ert-deftest agent-collab-c66-latex-repair-names-the-document-tools ()
  "The export-repair session edits the note through Noema, so its page follows."
  (require 'noema/bridge/config)
  (let ((prompt (+noema--latex-intervention-prompt
                 '((file . "/tmp/n.md") (outputPath . "/tmp/n.tex") (error . "boom")))))
    (should (string-match-p "agent:document:edit" prompt))
    (should (string-match "node \\([^ `]+agent-api\\.mjs\\)" prompt))
    (should (file-exists-p (match-string 1 prompt)))))

;;;; C67: threads anchored in a document.

(defmacro agent-collab-tests--project (root &rest body)
  "Bind ROOT to a temporary project with src/SAI.md and src/main.py, and run BODY."
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory (make-temp-file "agent-collab" t)))
          (+agent-collab-changed-functions nil))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" ,root))
           (with-temp-file (expand-file-name "src/SAI.md" ,root) (insert "# SAI\n\nalpha\nbeta\n"))
           (with-temp-file (expand-file-name "src/main.py" ,root) (insert "x = 1\ny = 2\n"))
           (cl-letf (((symbol-function '+agent-collab-root) (lambda (_file) ,root)))
             ,@body))
       (delete-directory ,root t))))

(ert-deftest agent-collab-c70-a-thread-is-kept-in-the-folder-journal ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let* ((file (expand-file-name "src/SAI.md" root))
             (thread (+agent-collab-thread-create :file file :line 3 :anchor "alpha"
                                                  :session agent :text "Why alpha?")))
        (should (file-exists-p (expand-file-name "src/.noema-comments.jsonl" root)))
        (should (string-match-p "\\`c-[0-9]\\{14\\}-[0-9a-f]\\{6\\}\\'" (alist-get 'id thread)))
        (let ((stored (car (+agent-collab-threads file))))
          (should (equal (alist-get 'id stored) (alist-get 'id thread)))
          (should (equal (alist-get 'state stored) "waiting"))
          (should (equal (alist-get 'line stored) 3))
          (should (equal (alist-get 'quote stored) "alpha"))
          (should (equal (alist-get 'name (alist-get 'assignee stored)) "claude·SAI"))
          (should (equal (mapcar (lambda (m) (alist-get 'name (alist-get 'author m))) (alist-get 'messages stored))
                         '("You"))))
        (should-not (+agent-collab-threads (expand-file-name "src/main.py" root)))))))

(ert-deftest agent-collab-c70-threads-the-host-wrote-are-read ()
  "Emacs folds the same journal the Noema host appends to for page comments."
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/SAI.md" root)))
      (with-temp-file (expand-file-name "src/.noema-comments.jsonl" root)
        (insert "{\"v\":1,\"op\":\"thread\",\"id\":\"c-1111\",\"note\":\"SAI.md\",\"quote\":\"alpha\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"Why?\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"
                "{\"v\":1,\"op\":\"reply\",\"id\":\"c-1111\",\"mid\":\"m-1\",\"author\":{\"name\":\"codex\",\"kind\":\"agent\"},\"text\":\"Because.\",\"at\":\"2026-11-17T08:01:00.000Z\"}\n"
                "{\"v\":1,\"op\":\"thread\",\"id\":\"c-2222\",\"note\":\"other.md\",\"quote\":\"x\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"elsewhere\",\"at\":\"2026-11-17T08:02:00.000Z\"}\n"
                "not json\n"))
      (let ((threads (+agent-collab-threads file)))
        (should (equal (mapcar (lambda (thread) (alist-get 'id thread)) threads) '("c-1111")))
        (should (equal (alist-get 'state (car threads)) "replied")))
      (+agent-collab--set-resolved file "c-1111" t)
      (should (equal (alist-get 'state (+agent-collab-thread file "c-1111")) "resolved")))))

(ert-deftest agent-collab-c70-thread-identity-includes-its-document ()
  "A sibling note must not be able to read, resolve, reassign or replace a task."
  (agent-collab-tests--project root
    (let* ((a (expand-file-name "src/SAI.md" root))
           (b (expand-file-name "src/main.py" root))
           (journal (expand-file-name "src/.noema-comments.jsonl" root)))
      (+agent-collab-thread-create :file a :id "c-owned" :text "on a")
      (let ((before (with-temp-buffer (insert-file-contents journal) (buffer-string))))
        (should-not (+agent-collab-thread b "c-owned"))
        (should-error (+agent-collab--set-resolved b "c-owned" t) :type 'user-error)
        (should-error (+agent-collab-unassign b "c-owned") :type 'user-error)
        (should-error (+agent-collab-thread-create :file b :id "c-owned" :text "collision") :type 'user-error)
        (should (equal before (with-temp-buffer (insert-file-contents journal) (buffer-string)))))
      (should (equal (alist-get 'status (+agent-collab-thread a "c-owned")) "open")))))

(ert-deftest agent-collab-c73-a-dispatched-comment-is-sent-to-its-agent ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let ((file (expand-file-name "src/SAI.md" root)) delivered notified)
        (with-temp-file (expand-file-name "src/SAI.md" root) (insert "# SAI\n\n{==alpha==}{>>#c-1111<<}\n"))
        (with-temp-file (expand-file-name "src/.noema-comments.jsonl" root)
          (insert "{\"v\":1,\"op\":\"thread\",\"id\":\"c-1111\",\"note\":\"SAI.md\",\"quote\":\"alpha\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"Why alpha?\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"))
        (cl-letf (((symbol-function '+agent-collab--live-collaborator) (lambda (_) agent))
                  ((symbol-function '+agent-collab--idle-p) (lambda (_) t))
                  ((symbol-function '+agent-collab--deliver) (lambda (_b prompt &optional _finished) (push prompt delivered)))
                  ((symbol-function '+agent-collab--notify-host) (lambda (f) (push f notified))))
          (+agent-collab-ask-thread file "c-1111"))
        (should (string-match-p "Why alpha?" (car delivered)))
        (should (string-match-p "line 3 about «alpha»" (car delivered)))
        (should (equal (alist-get 'name (alist-get 'assignee (+agent-collab-thread file "c-1111"))) "claude·SAI"))
        (should notified)))))

(ert-deftest agent-collab-c70-a-comment-without-a-collaborator-stays ()
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/SAI.md" root)))
      (with-temp-file (expand-file-name "src/.noema-comments.jsonl" root)
        (insert "{\"v\":1,\"op\":\"thread\",\"id\":\"c-1111\",\"note\":\"SAI.md\",\"quote\":\"alpha\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"Note to self\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"))
      (cl-letf (((symbol-function '+agent-collab--live-collaborator) (lambda (_) nil)))
        (+agent-collab-ask-thread file "c-1111"))
      (should (equal (alist-get 'state (+agent-collab-thread file "c-1111")) "open")))))

(ert-deftest agent-collab-c67-context-sends-default-to-the-document-collaborator ()
  (require 'tools/agent/context)
  (agent-collab-tests--agent-buffer agent
    (let ((document (generate-new-buffer "SAI.md")) offered)
      (unwind-protect
          (with-current-buffer document
            (setq buffer-file-name "/tmp/x/SAI.md")
            (cl-letf (((symbol-function '+agent-binding-sessions)
                       (lambda (&rest _) (list (list :buffer agent))))
                      ((symbol-function '+agent-session-live-p) (lambda (_) t))
                      ((symbol-function '+agent-acp-project-root) (lambda (&rest _) "/tmp/"))
                      ((symbol-function '+agent-session-read)
                       (lambda (&rest args) (setq offered (plist-get args :default)) agent)))
              (let ((+agent-context-always-ask-session t))
                (should (eq (+agent-context--session) agent))
                (should (eq offered agent)))))
        (with-current-buffer document (set-buffer-modified-p nil) (setq buffer-file-name nil))
        (kill-buffer document)))))

;;;; C66/C67: suggestions for files that cannot hold a card.

(defun agent-collab-tests--suggest (root &rest fields)
  "Write one pending suggestion record under ROOT with FIELDS and return its id."
  (let* ((id (or (plist-get fields :id) "s1"))
         (dir (expand-file-name ".agent/collab/suggestions/" root)))
    (make-directory dir t)
    (with-temp-file (expand-file-name (concat id ".json") dir)
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert
               `((id . ,id) (file . ,(expand-file-name "src/main.py" root)) (title . "Bump")
                 (from . ((line . 2) (column . 0))) (to . ((line . 2) (column . 5)))
                 (oldText . ,(or (plist-get fields :old) "y = 2")) (newText . "y = 3")
                 (status . "pending") (comments . []) (author . ((name . "codex·main") (session . "tok"))))))
    id))

(ert-deftest agent-collab-c66-accepting-a-suggestion-applies-it-where-it-still-fits ()
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/main.py" root)))
      (agent-collab-tests--suggest root)
      (should (equal (mapcar (lambda (s) (alist-get 'id s)) (+agent-collab-suggestions file)) '("s1")))
      (+agent-collab-suggestion-accept root "s1")
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "x = 1\ny = 3\n"))
      (should (equal (alist-get 'status (+agent-collab-suggestion root "s1")) "accepted"))
      (should-not (+agent-collab-suggestions file)))))

(ert-deftest agent-collab-c66-a-suggestion-whose-text-moved-is-refused ()
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/main.py" root)))
      (agent-collab-tests--suggest root :old "z = 9")
      (should-error (+agent-collab-suggestion-accept root "s1") :type 'user-error)
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "x = 1\ny = 2\n"))
      (+agent-collab-suggestion-reject root "s1")
      (should (equal (alist-get 'status (+agent-collab-suggestion root "s1")) "rejected")))))

;;;; C68: changes an Agent applied itself.

(ert-deftest agent-collab-c68-direct-changes-are-recorded-from-tool-diffs ()
  (agent-collab-tests--agent-buffer agent
    (with-current-buffer agent
      (setq-local +agent-acp--direct-edits nil +agent-acp--counted-changes nil +agent-acp--changes nil)
      (+agent-acp--note-tool-change "t1" '((:status . "completed")
                                           (:diffs ((:old . "y = 2") (:new . "y = 3") (:file . "/tmp/x/main.py")))))
      (+agent-acp--note-tool-change "t1" '((:status . "completed")
                                           (:diffs ((:old . "y = 2") (:new . "y = 3") (:file . "/tmp/x/main.py"))))))
    (let ((edits (+agent-collab-direct-changes "/tmp/x/main.py")))
      (should (= (length edits) 1))
      (should (equal (plist-get (car edits) :new) "y = 3"))
      (should (eq (plist-get (car edits) :buffer) agent)))))

(ert-deftest agent-collab-c68-revert-puts-back-only-a-change-still-in-place ()
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/main.py" root)))
      (with-temp-file file (insert "x = 1\ny = 3\n"))
      (+agent-collab-revert-direct-change (list :file file :old "y = 2" :new "y = 3"))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "x = 1\ny = 2\n"))
      (should-error (+agent-collab-revert-direct-change (list :file file :old "y = 2" :new "y = 3"))
                    :type 'user-error)
      (with-temp-file file (insert "a\na\n"))
      (should-error (+agent-collab-revert-direct-change (list :file file :old "b" :new "a"))
                    :type 'user-error))))

(ert-deftest agent-collab-c68-a-note-is-reverted-through-noema-so-its-page-follows ()
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/SAI.md" root)) called)
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (channel args callback &optional _timeout)
                   (setq called (list channel args))
                   (funcall callback '((ok . t)) nil))))
        (+agent-collab-revert-direct-change (list :file file :old "ALPHA" :new "alpha"))
        (should (equal (car called) "noema:api:agent:document:edit"))
        (let ((edit (aref (alist-get 'edits (aref (cadr called) 0)) 0)))
          (should (equal (alist-get 'oldText edit) "alpha"))
          (should (equal (alist-get 'newText edit) "ALPHA"))
          (should (equal (alist-get 'from edit) '((line . 3) (column . 0))))
          (should (equal (alist-get 'to edit) '((line . 3) (column . 5)))))
        ;; The note itself was not written behind its page's back.
        (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                       "# SAI\n\nalpha\nbeta\n"))))))

(ert-deftest agent-collab-c67-a-context-send-can-hand-delivery-to-a-thread ()
  (require 'tools/agent/context)
  (agent-collab-tests--agent-buffer agent
    (let (handed)
      (cl-letf (((symbol-function '+agent-context--session) (lambda (&rest _) agent))
                ((symbol-function '+agent-context--deliver) (lambda (&rest _) (error "Delivered directly"))))
        (+agent-context--send :prompt "Why?" :instructions "Selection: 3:0"
                              :references (lambda (_root _session) nil)
                              :deliver (lambda (buffer blocks _instructions question)
                                         (setq handed (list buffer question (length blocks))))))
      (should (equal handed (list agent "Why?" 1))))))

(ert-deftest agent-collab-c76-a-send-about-a-place-becomes-a-task ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let ((file (expand-file-name "src/SAI.md" root)) sent)
        (cl-letf (((symbol-function '+agent-context--deliver)
                   (lambda (buffer blocks _instructions no-focus &rest _)
                     (setq sent (list buffer (alist-get 'text (car blocks)) no-focus)))))
          (funcall (+agent-collab-task-delivery file 3 "alpha")
                   agent '(((type . "text") (text . "Why?")) ((type . "resource_link"))) nil "Why?"))
        (let ((thread (car (+agent-collab-threads file))))
          (should (equal (alist-get 'text (car (alist-get 'messages thread))) "Why?"))
          (should (equal (alist-get 'quote thread) "alpha"))
          (should (equal (alist-get 'session (alist-get 'assignee thread)) (+agent-acp-session-token agent)))
          ;; Sent at once, without taking the keyboard; the turn names the thread.
          (should (eq (nth 0 sent) agent))
          (should (string-prefix-p "Why?\n\n(Task from comment " (nth 1 sent)))
          (should (string-match-p (regexp-quote (alist-get 'id thread)) (nth 1 sent)))
          (should (nth 2 sent)))))))

(ert-deftest agent-collab-c76-sends-from-a-document-carry-a-task-delivery ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let ((file (expand-file-name "src/SAI.md" root)) seen)
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (cl-letf (((symbol-function '+agent-context--send)
                         (lambda (&rest args) (setq seen (plist-get args :deliver)))))
                (+agent-context-send-buffer)
                (should (functionp seen))
                (setq seen nil)
                (goto-char (point-min)) (set-mark (point)) (forward-line 1) (activate-mark)
                (+agent-context-send-region)
                (should (functionp seen)))
            (set-buffer-modified-p nil) (kill-buffer)))
        ;; A chat is not a document: no task.
        (should-not (+agent-context--task-delivery agent))))))

(ert-deftest agent-collab-c67-a-collaborator-chat-means-its-primary-file ()
  (agent-collab-tests--agent-buffer agent
    (with-current-buffer agent
      (setq-local +agent-acp-session-file "/tmp/x/SAI.md")
      (cl-letf (((symbol-function '+agent-collab--client-file) #'identity))
        (should (equal (+agent-collab--document) "/tmp/x/SAI.md"))
        (setq-local +agent-acp-session-file nil)
        (should-error (+agent-collab--document) :type 'user-error)))))

(ert-deftest agent-collab-c71-a-mentioned-collaborator-answers ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let ((file (expand-file-name "src/SAI.md" root)) asked)
        (with-temp-file (expand-file-name "src/.noema-comments.jsonl" root)
          (insert "{\"v\":1,\"op\":\"thread\",\"id\":\"c-1111\",\"note\":\"SAI.md\",\"quote\":\"alpha\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"@claude·SAI why?\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"))
        (cl-letf (((symbol-function '+agent-collab--live-collaborator) (lambda (_) (error "Not this one")))
                  ((symbol-function '+agent-session-snapshot)
                   (lambda (buffer) (and (eq buffer agent) (list :buffer agent :native-session-id "n1"))))
                  ((symbol-function '+agent-collab--idle-p) (lambda (_) t))
                  ((symbol-function '+agent-collab--deliver) (lambda (b _prompt &optional _finished) (push b asked)))
                  ((symbol-function '+agent-collab--notify-host) #'ignore))
          (+agent-collab-ask-thread file "c-1111" `((buffer . ,(buffer-name agent)) (nativeSessionId . "n1")))
          (should (equal asked (list agent)))
          ;; A stale identity is refused rather than sent to another session.
          (setq asked nil)
          (+agent-collab-ask-thread file "c-1111" `((buffer . ,(buffer-name agent)) (nativeSessionId . "old")))
          (should-not asked))))))

(ert-deftest agent-collab-c73-a-task-carries-the-tools-guide ()
  (agent-collab-tests--agent-buffer agent
    (let (delivered)
      (cl-letf (((symbol-function '+agent-context--deliver)
                 (lambda (buffer blocks instructions no-focus _failure &optional _finished)
                   (setq delivered (list buffer (length blocks) instructions no-focus)))))
        (let ((+agent-collab-session-instructions-function (lambda () '(noema-tools . "GUIDE"))))
          (+agent-collab--deliver agent "q"))
        (should (equal delivered (list agent 1 '(noema-tools . "GUIDE") t)))
        (let ((+agent-collab-session-instructions-function nil))
          (+agent-collab--deliver agent "q"))
        (should (equal (nth 2 delivered) nil))))))

(ert-deftest agent-collab-c73-short-quotes-arrive-complete ()
  "A quote short enough to inline remains usable as exact oldText, even in CJK."
  (let* ((quote (make-string 1200 ?字))
         (prompt (+agent-collab--task-prompt
                  `((id . "c-1111") (file . "/tmp/a.md") (line . 1)
                    (quote . ,quote) (messages . (((text . "改成中文"))))))))
    (should (string-match "about «\\([^»]*\\)»" prompt))
    (should (equal (match-string 1 prompt) quote))))

(ert-deftest agent-collab-c73-long-quotes-require-a-read ()
  "A shortened excerpt must never be presented as exact source for editing."
  (let* ((quote (make-string 1201 ?x))
         (prompt (+agent-collab--task-prompt
                  `((id . "c-1111") (file . "/tmp/a.md") (line . 1)
                    (quote . ,quote) (messages . (((text . "精简一下"))))))))
    (should-not (string-match-p (regexp-quote quote) prompt))
    (should (string-match-p "agent:document:read" prompt))))

(ert-deftest agent-collab-c76-context-delivery-keeps-short-quotes ()
  "Sending a selection and dispatching a stored comment use the same quote policy."
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let* ((file (expand-file-name "src/SAI.md" root))
             (quote (concat (make-string 240 ?a) " full ending")) delivered)
        (cl-letf (((symbol-function '+agent-collab--notify-host) #'ignore)
                  ((symbol-function '+agent-context--deliver)
                   (lambda (_session blocks &rest _) (setq delivered blocks))))
          (funcall (+agent-collab-task-delivery file 3 quote)
                   agent '(((type . "text") (text . "Rewrite"))) nil "Rewrite"))
        (should (string-match-p (regexp-quote quote) (alist-get 'text (car delivered))))))))

(ert-deftest agent-collab-c75-resolving-one-thread-leaves-the-others-open ()
  "Each folded thread is its own structure: no state leaks between threads."
  (agent-collab-tests--project root
    (let ((file (expand-file-name "src/SAI.md" root)))
      (with-temp-file (expand-file-name "src/.noema-comments.jsonl" root)
        (insert "{\"v\":1,\"op\":\"thread\",\"id\":\"c-1111\",\"note\":\"SAI.md\",\"quote\":\"a\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"one\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"
                "{\"v\":1,\"op\":\"thread\",\"id\":\"c-2222\",\"note\":\"SAI.md\",\"quote\":\"b\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":\"two\",\"at\":\"2026-11-17T08:01:00.000Z\"}\n"
                "{\"v\":1,\"op\":\"resolve\",\"id\":\"c-1111\",\"at\":\"2026-11-17T09:00:00.000Z\"}\n"
                "{\"v\":1,\"op\":\"edit\",\"id\":\"c-1111\",\"text\":\"ONE\",\"at\":\"2026-11-17T09:01:00.000Z\"}\n"))
      (should (equal (alist-get 'status (+agent-collab-thread file "c-1111")) "resolved"))
      (should (equal (alist-get 'status (+agent-collab-thread file "c-2222")) "open"))
      (should (equal (alist-get 'text (car (alist-get 'messages (+agent-collab-thread file "c-2222")))) "two")))))

(provide 'agent-collab-tests)
;;; agent-collab-tests.el ends here
