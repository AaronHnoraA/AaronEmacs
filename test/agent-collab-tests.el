;;; agent-collab-tests.el --- Document collaboration: authorship, comment tasks, direct changes -*- lexical-binding: t; -*-
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

(ert-deftest agent-collab-c73-a-task-carries-the-tools-guide ()
  (agent-collab-tests--agent-buffer agent
    (let (delivered)
      (cl-letf (((symbol-function '+agent-context--deliver)
                 (lambda (buffer blocks instructions no-focus _failure)
                   (setq delivered (list buffer (length blocks) instructions no-focus)))))
        (let ((+agent-collab-session-instructions-function (lambda () '(noema-tools . "GUIDE"))))
          (+agent-collab--deliver agent "q"))
        (should (equal delivered (list agent 1 '(noema-tools . "GUIDE") t)))
        (let ((+agent-collab-session-instructions-function nil))
          (+agent-collab--deliver agent "q"))
        (should (equal (nth 2 delivered) nil))))))


;;;; C79: comments are comments in the note; tasks are journal lines.

(ert-deftest agent-collab-c79-the-scanner-reads-the-shared-fixtures ()
  (let ((fixtures (with-temp-buffer
                    (insert-file-contents (expand-file-name "site-lisp/noema/shared/comment-fixtures.json" user-emacs-directory))
                    (json-parse-buffer :object-type 'alist :array-type 'list :false-object nil))))
    (dolist (fixture fixtures)
      (should (equal (mapcar (lambda (c) (list (alist-get 'id c) (alist-get 'done c) (alist-get 'text c)
                                               (alist-get 'line c) (alist-get 'quote c)))
                             (+agent-collab-scan (alist-get 'source fixture)))
                     (mapcar (lambda (c) (list (alist-get 'id c) (alist-get 'done c) (alist-get 'text c)
                                               (alist-get 'line c) (alist-get 'quote c)))
                             (alist-get 'comments fixture)))))))

(defun agent-collab-tests--note (root text)
  "Write TEXT as ROOT's src/SAI.md and return its file."
  (let ((file (expand-file-name "src/SAI.md" root)))
    (with-temp-file file (insert text))
    file))

(defun agent-collab-tests--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest agent-collab-c79-handing-over-names-the-comment-and-records-the-agent ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let* ((+noema--ready nil)
             (file (agent-collab-tests--note root "# SAI\n\nThe bound @@comment [Prove [it]] holds.\n"))
             (comment (car (+agent-collab-comments file)))
             delivered)
        (should (equal (alist-get 'id comment) ""))
        (cl-letf (((symbol-function '+agent-collab--deliver) (lambda (_b prompt) (push prompt delivered))))
          (+agent-collab-dispatch comment agent))
        ;; An id-less comment is given one; its text stays as written.
        (should (string-match "The bound @@comment \\[Prove \\[it\\]\\] {id: \\([a-z0-9]\\{6\\}\\)} holds" (agent-collab-tests--disk file)))
        (let ((id (match-string 1 (agent-collab-tests--disk file))))
          (should (string-match-p (format "comment %s on .*SAI.md, line 3, after «The bound»" id) (car delivered)))
          (should (string-match-p "agent:document:comment" (car delivered)))
          (should (equal (alist-get 'assignee (+agent-collab-comment file id))
                         (cons "claude·SAI" (+agent-acp-session-token agent)))))))))

(ert-deftest agent-collab-c79-done-and-reopen-edit-only-the-head ()
  (agent-collab-tests--project root
    (let* ((+noema--ready nil)
           (file (agent-collab-tests--note root "a @@comment [x \\] y] {id: ab12cd; by: You}\nb @@comment [z] {id: cd34ef}\n")))
      (+agent-collab-set-done (+agent-collab-comment file "ab12cd") t)
      (should (equal (agent-collab-tests--disk file) "a @@comment(done) [x \\] y] {id: ab12cd; by: You}\nb @@comment [z] {id: cd34ef}\n"))
      (should (alist-get 'done (+agent-collab-comment file "ab12cd")))
      (should-not (alist-get 'done (+agent-collab-comment file "cd34ef")))
      (+agent-collab-set-done (+agent-collab-comment file "ab12cd") nil)
      (should (string-prefix-p "a @@comment [x \\] y]" (agent-collab-tests--disk file))))))

(ert-deftest agent-collab-c79-a-note-gains-a-side-comment-and-code-keeps-a-journal-task ()
  (agent-collab-tests--project root
    (let* ((+noema--ready nil)
           (note (agent-collab-tests--note root "# SAI\n\nalpha\nbeta\n"))
           (code (expand-file-name "src/main.py" root))
           (begin (string-match "alpha" (agent-collab-tests--disk note)))
           (id (+agent-collab-add note "Why ] alpha?" begin (+ begin 5))))
      (should (equal (agent-collab-tests--disk note) (format "# SAI\n\nalpha @@comment [Why \\] alpha?] {id: %s}\nbeta\n" id)))
      (let ((task (+agent-collab-add code "Rename y" 6 11)))
        (should (equal (agent-collab-tests--disk code) "x = 1\ny = 2\n"))
        (let ((comment (+agent-collab-comment code task)))
          (should (equal (alist-get 'text comment) "Rename y"))
          (should (equal (alist-get 'line comment) 2))
          (should (equal (alist-get 'quote comment) "y = 2")))))))

(ert-deftest agent-collab-c76-a-send-about-a-place-becomes-a-task ()
  (agent-collab-tests--project root
    (agent-collab-tests--agent-buffer agent
      (let* ((+noema--ready nil)
             (file (agent-collab-tests--note root "# SAI\n\nalpha\n"))
             (begin (string-match "alpha" (agent-collab-tests--disk file)))
             sent id)
        (cl-letf (((symbol-function '+agent-context--deliver)
                   (lambda (buffer blocks _instructions no-focus &rest _)
                     (setq sent (list buffer (alist-get 'text (car blocks)) no-focus)))))
          (setq id (funcall (+agent-collab-task-delivery file 3 "alpha" begin (+ begin 5))
                            agent '(((type . "text") (text . "Why?")) ((type . "resource_link"))) nil "Why?")))
        (should (string-match-p (format "alpha @@comment \\[Why\\?\\] {id: %s}" id) (agent-collab-tests--disk file)))
        (should (eq (nth 0 sent) agent))
        (should (string-prefix-p "Why?\n\n(Task from comment " (nth 1 sent)))
        (should (nth 2 sent))
        (should (equal (car (alist-get 'assignee (+agent-collab-comment file id))) "claude·SAI"))))))

(provide 'agent-collab-tests)
;;; agent-collab-tests.el ends here
