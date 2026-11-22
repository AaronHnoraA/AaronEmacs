;;; agent-panel-tests.el --- The collaboration panel (C75) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/panel)

(defun agent-panel-tests--journal (dir lines)
  "Write LINES (thread specs (ID NOTE TEXT STATUS)) as DIR's comment journal."
  (make-directory dir t)
  (with-temp-file (expand-file-name ".noema-comments.jsonl" dir)
    (setq buffer-file-coding-system 'utf-8-unix)
    (pcase-dolist (`(,id ,note ,text ,status) lines)
      (insert (format "{\"v\":1,\"op\":\"thread\",\"id\":%S,\"note\":%S,\"quote\":\"q\",\"author\":{\"name\":\"You\",\"kind\":\"person\"},\"text\":%S,\"at\":\"2026-11-17T08:00:00.000Z\"}\n" id note text))
      (when (equal status "resolved")
        (insert (format "{\"v\":1,\"op\":\"resolve\",\"id\":%S,\"at\":\"2026-11-17T09:00:00.000Z\"}\n" id))))))

(defmacro agent-panel-tests--tree (root &rest body)
  "Bind ROOT to a project tree with comments in several folders, and run BODY."
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory (make-temp-file "agent-panel" t))))
     (unwind-protect
         (progn
           (agent-panel-tests--journal (expand-file-name "notes" ,root)
                                       '(("c-a1" "a.md" "on a" "open") ("c-a2" "a.md" "done on a" "resolved")
                                         ("c-b1" "b.md" "on b" "open")))
           (agent-panel-tests--journal (expand-file-name "notes/deep" ,root) '(("c-d1" "d.md" "deep" "open")))
           (agent-panel-tests--journal (expand-file-name "other" ,root) '(("c-o1" "o.md" "other" "open")))
           (agent-panel-tests--journal (expand-file-name "node_modules/x" ,root) '(("c-n1" "n.md" "skipped" "open")))
           (clrhash +agent-panel--journal-cache)
           (cl-letf (((symbol-function '+agent-panel--project-root) (lambda (_) ,root))
                     ((symbol-function '+agent-collab--client-file) #'identity))
             ,@body))
       (delete-directory ,root t))))

(defun agent-panel-tests--ids (groups)
  (sort (mapcar (lambda (thread) (alist-get 'id thread)) (apply #'append (mapcar #'cdr groups))) #'string<))

(ert-deftest agent-panel-c75-scopes-widen-from-the-file-to-the-project ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'file)) '("c-a1" "c-a2")))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'dir)) '("c-a1" "c-a2" "c-b1")))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'tree)) '("c-a1" "c-a2" "c-b1" "c-d1")))
      ;; The project skips dependency folders.
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'project))
                     '("c-a1" "c-a2" "c-b1" "c-d1" "c-o1")))
      (should (equal (mapcar #'car (+agent-panel-tasks file 'dir))
                     (list (expand-file-name "notes/a.md" root) (expand-file-name "notes/b.md" root)))))))

(ert-deftest agent-panel-c75-a-page-document-lists-its-tasks ()
  "A Noema page names its document /fs:local:...; its tasks are listed all the same."
  (agent-panel-tests--tree root
    (cl-letf (((symbol-function '+agent-collab--client-file)
               (lambda (file) (string-remove-prefix "/fs:local:" file))))
      (with-temp-buffer
        (+agent-panel-mode)
        (setq +agent-panel--document (concat "/fs:local:" (expand-file-name "notes/a.md" root)))
        (let ((inhibit-read-only t)) (+agent-panel--draw-tasks))
        (should (string-match-p "Tasks of this file · 1 open" (buffer-string)))
        (should (string-match-p "▌ on a" (buffer-string)))))))

(ert-deftest agent-panel-c75-a-new-folder-of-comments-is-found ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 5))
      (agent-panel-tests--journal (expand-file-name "fresh" root) '(("c-f1" "f.md" "fresh" "open")))
      ;; Cached until a comment changes in a folder the cache does not know.
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 5))
      (+agent-panel--comments-changed-h (expand-file-name "fresh/f.md" root))
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 6)))))

(defun agent-panel-tests--board (file)
  "Return the board of FILE as drawn."
  (with-temp-buffer
    (+agent-panel-mode)
    (setq +agent-panel--document file)
    (let ((inhibit-read-only t)) (+agent-panel--draw-tasks))
    (buffer-string)))

(ert-deftest agent-panel-c81-cards-sit-in-the-lane-their-state-means ()
  (let* ((agent (generate-new-buffer "*panel agent*"))
         (snapshot nil)
         (lane (lambda (thread) (car (+agent-panel--card-lane thread)))))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-session-buffer-for-token) (lambda (token) (and (equal token "tok") agent)))
                  ((symbol-function '+agent-session-snapshot) (lambda (_) snapshot)))
          (should (eq (funcall lane '((status . "open"))) 'todo))
          (should (eq (funcall lane '((status . "resolved") (assignee (session . "tok")))) 'done))
          (setq snapshot '(:turn-state running))
          (should (eq (funcall lane '((status . "open") (assignee (session . "tok")))) 'doing))
          (setq snapshot '(:turn-state idle :attention-kind permission))
          (should (eq (funcall lane '((status . "open") (assignee (session . "tok")))) 'waiting))
          (setq snapshot '(:process-state disconnected))
          (should (eq (funcall lane '((status . "open") (assignee (session . "tok")))) 'paused))
          (should (eq (funcall lane '((status . "open") (assignee (session . "gone")))) 'paused))
          (should (equal (+agent-panel--agent-state '(:turn-state running)) '("working" . "●"))))
      (kill-buffer agent))))

(ert-deftest agent-panel-c81-the-board-shows-lanes-cards-and-agents ()
  (agent-panel-tests--tree root
    (cl-letf (((symbol-function '+agent-panel--project-sessions)
               (lambda (_) (list (list :name "claude·SAI" :turn-state 'running :buffer 'a)
                                 (list :name "codex·SAI" :attention-kind 'permission :buffer 'b)))))
      (let ((board (agent-panel-tests--board (expand-file-name "notes/a.md" root))))
        (should (string-match-p "Agents  ● claude·SAI   ! codex·SAI" board))
        (should (string-match-p "▾ To do  1\n ▌ on a\n ▌ «q»\n ▌ a.md" board))
        (should (string-match-p "▾ In progress  0\n   —" board))
        ;; Done is folded to its count.
        (should (string-match-p "▸ Done  1" board))
        (should-not (string-match-p "done on a" board))))))

(ert-deftest agent-panel-c81-moving-a-card-does-what-its-lane-means ()
  (agent-panel-tests--tree root
    (let* ((file (expand-file-name "notes/a.md" root))
           (thread (+agent-collab-thread file "c-a1"))
           dispatched)
      (cl-letf (((symbol-function '+agent-panel--choose-agent) (lambda (_) 'agent))
                ((symbol-function '+agent-collab-dispatch) (lambda (f id session) (setq dispatched (list f id session)))))
        ;; To do → In progress hands it over.
        (+agent-panel--move-to thread 'doing)
        (should (equal dispatched (list file "c-a1" 'agent)))
        ;; → Done marks it done; back to To do reopens it.
        (+agent-panel--move-to thread 'done)
        (should (equal (alist-get 'status (+agent-collab-thread file "c-a1")) "resolved"))
        (+agent-panel--move-to (+agent-collab-thread file "c-a1") 'todo)
        (should (equal (alist-get 'status (+agent-collab-thread file "c-a1")) "open"))))))

(ert-deftest agent-panel-c81-clear-finished-archives-only-the-scoped-terminal-tasks ()
  (agent-panel-tests--tree root
    (let* ((file (expand-file-name "notes/a.md" root))
           (sibling (expand-file-name "notes/b.md" root))
           (source "{==anchor==}{>>#c-a2<<}\n@@scomment [margin note]\n@@comment [private note]\n")
           (+agent-panel--document file))
      (with-temp-file file (insert source))
      (+agent-collab-thread-create :file file :id "c-fail" :text "failed task")
      (+agent-collab--append
       file '((op . "assign") (id . "c-fail") (attempt . "try-1")
              (assignee . ((name . "agy") (session . "tok")))))
      (+agent-collab--append file '((op . "fail") (id . "c-fail") (attempt . "try-1") (reason . "refusal")))
      (+agent-collab--append
       file '((op . "thread") (id . "c-question") (note . "a.md")
              (author . ((name . "Agent") (kind . "agent"))) (text . "A question for the author")))
      (+agent-collab--set-resolved sibling "c-b1" t)
      (with-temp-buffer
        (+agent-panel-mode)
        (let ((inhibit-read-only t))
          (+agent-panel--draw-tasks)
          (should (string-match-p "Failed" (buffer-string)))
          (should-not (string-match-p "margin note\\|private note\\|A question for the author" (buffer-string)))
          (goto-char (point-min))
          (search-forward "Clear finished")
          ;; Focus may have moved before the idle redraw replaced this board.
          (let ((+agent-panel--document sibling))
            (button-activate (button-at (1- (point)))))))
      (should (equal (mapcar (lambda (thread) (alist-get 'id thread))
                             (cdar (+agent-panel-tasks file 'file)))
                     '("c-a1")))
      (dolist (id '("c-a2" "c-fail"))
        (should (alist-get 'archived (+agent-collab-thread file id))))
      (should-not (alist-get 'archived (+agent-collab-thread sibling "c-b1")))
      (should (equal source (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (+agent-collab--set-resolved file "c-a2" nil)
      (should (member "c-a2" (mapcar (lambda (thread) (alist-get 'id thread))
                                    (cdar (+agent-panel-tasks file 'file))))))))

(ert-deftest agent-panel-c81-taking-back-unassigns ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (+agent-collab--append file '((op . "assign") (id . "c-a1") (assignee . ((name . "claude·SAI") (session . "tok")))))
      (should (alist-get 'assignee (+agent-collab-thread file "c-a1")))
      (+agent-panel--move-to (+agent-collab-thread file "c-a1") 'todo)
      (should-not (alist-get 'assignee (+agent-collab-thread file "c-a1"))))))

(ert-deftest agent-panel-c75-redraw-keeps-the-selected-task ()
  "A new card before point must not redirect the next keyboard action to it."
  (dolist (between-cards '(nil t))
    (agent-panel-tests--tree root
      (let* ((+agent-panel-buffer-name " *panel selection test*")
             (+agent-panel--document (expand-file-name "notes/a.md" root))
             (buffer (+agent-panel--tasks-buffer)))
        (unwind-protect
            (with-current-buffer buffer
              (cl-letf (((symbol-function '+agent-panel--project-sessions) (lambda (_) nil)))
                (agent-panel-tests--journal
                 (expand-file-name "notes" root)
                 '(("c-tail" "a.md" "last task" "open")
                   ("c-a1" "a.md" "on a" "open")))
                (+agent-panel-redraw)
                (goto-char (point-min))
                (search-forward "on a")
                (when between-cards
                  (goto-char (next-single-property-change (point) '+agent-panel-task))
                  (should-not (get-text-property (point) '+agent-panel-task))
                  (should-not (get-text-property (point) '+agent-panel-lane)))
                (should (equal (cdr (+agent-panel--at '+agent-panel-task)) "c-a1"))
                (agent-panel-tests--journal
                 (expand-file-name "notes" root)
                 '(("c-a1" "a.md" "on a" "open")
                   ("c-new" "a.md" "new first task" "open")))
                (+agent-panel-redraw)
                ;; Verify the fixture actually puts the new card before the selected one.
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "new first task")
                  (should (search-forward "on a" nil t)))
                (should (equal (+agent-panel--at '+agent-panel-task)
                               (cons +agent-panel--document "c-a1")))
                (agent-panel-tests--journal (expand-file-name "notes" root)
                                            '(("c-new" "a.md" "new first task" "open")))
                (+agent-panel-redraw)
                (should-error (+agent-panel--task) :type 'user-error)))
          (kill-buffer buffer))))))

(ert-deftest agent-panel-c81-lane-headers-do-not-select-previous-cards ()
  "A task action at an empty lane must not reach back into a different lane."
  (agent-panel-tests--tree root
    (with-temp-buffer
      (+agent-panel-mode)
      (let ((+agent-panel--document (expand-file-name "notes/a.md" root))
            (inhibit-read-only t))
        (+agent-panel--draw-tasks)
        (goto-char (point-min))
        (search-forward "In progress")
        (should (eq (get-text-property (point) '+agent-panel-lane) 'doing))
        (should-error (+agent-panel--task) :type 'user-error)))))

(ert-deftest agent-panel-c81-waiting-cards-explain-the-request ()
  "The panel can show the exact approval or question from the shared snapshot."
  (with-temp-buffer
    (+agent-panel--insert-card
     '((file . "/tmp/a.md") (id . "c-1111") (line . 1)
       (messages . (((text . "修订证明")))) (assignee . ((name . "codex"))))
     'waiting '(:attention-kind permission :request "Allow editing proof.md?"))
    (should (string-match-p "Allow editing proof.md?" (buffer-string)))))

(ert-deftest agent-panel-c81-o-opens-the-selected-task-chat ()
  (let ((session (generate-new-buffer " *panel chat target*")) shown)
    (unwind-protect
        (with-temp-buffer
          (+agent-panel-mode)
          (should (eq (key-binding (kbd "o")) #'+agent-panel-chat))
          (cl-letf (((symbol-function '+agent-panel--task) (lambda () '((file . "/tmp/task.md"))))
                    ((symbol-function '+agent-collab--thread-session) (lambda (&rest _) session))
                    ((symbol-function '+agent-panel--editing-window) #'selected-window)
                    ((symbol-function '+agent-acp-show-buffer) (lambda (buffer) (setq shown buffer))))
            (call-interactively (key-binding (kbd "o")))
            (should (eq shown session))))
      (kill-buffer session))))

(ert-deftest agent-panel-c81-o-also-opens-the-agent-row ()
  (let ((session (generate-new-buffer " *panel agent row*")) shown)
    (unwind-protect
        (with-temp-buffer
          (insert (propertize "Agent" '+agent-panel-agent session))
          (goto-char (point-min))
          (cl-letf (((symbol-function '+agent-panel--task) (lambda () (ert-fail "Agent row has no task")))
                    ((symbol-function '+agent-panel--editing-window) #'selected-window)
                    ((symbol-function '+agent-acp-show-buffer) (lambda (buffer) (setq shown buffer))))
            (+agent-panel-chat)
            (should (eq shown session))))
      (kill-buffer session))))

(ert-deftest agent-panel-c81-a-gone-assignee-never-opens-another-agents-chat ()
  (let (shown)
    (with-temp-buffer
      (cl-letf (((symbol-function '+agent-panel--task)
                 (lambda () '((file . "/tmp/task.md") (assignee (session . "gone")))))
                ((symbol-function '+agent-acp-session-buffer-for-token) (lambda (_) nil))
                ((symbol-function '+agent-collab--thread-session) (lambda (&rest _) (current-buffer)))
                ((symbol-function '+agent-panel--editing-window) #'selected-window)
                ((symbol-function '+agent-acp-show-buffer) (lambda (_) (setq shown t))))
        (should-error (+agent-panel-chat) :type 'user-error)
        (should-not shown)))))

(ert-deftest agent-panel-c81-source-visits-use-the-shared-note-anchor-navigation ()
  (require 'noema-agenda)
  (let (visited)
    (cl-letf (((symbol-function '+agent-panel--task)
               (lambda () '((file . "/tmp/task.md") (id . "c-1234"))))
              ((symbol-function '+agent-panel--editing-window) #'selected-window)
              ((symbol-function 'noema-agenda-visit-record) (lambda (record) (setq visited record)))
              ((symbol-function '+noema/open-file) (lambda (&rest _) (ert-fail "Bypassed anchor navigation")))
              ((symbol-function 'run-at-time) (lambda (&rest _) (ert-fail "Delayed jump without identity"))))
      (+agent-panel-visit)
      (should (equal (alist-get 'file visited) "/tmp/task.md"))
      (should (equal (alist-get 'id visited) "#c-1234"))
      (should (equal (alist-get 'sourceKind visited) "comment")))))

(ert-deftest agent-panel-c81-native-reference-tasks-keep-their-quote-for-navigation ()
  (require 'noema-agenda)
  (agent-panel-tests--tree root
    (let* ((file (expand-file-name "notes/a.md" root))
           (thread (+agent-collab-thread-create :file file :line 12 :anchor "Unique theorem statement" :text "Prove this"))
           visited)
      (cl-letf (((symbol-function '+agent-panel--task) (lambda () thread))
                ((symbol-function '+agent-panel--editing-window) #'selected-window)
                ((symbol-function 'noema-agenda-visit-record) (lambda (record) (setq visited record))))
        (+agent-panel-visit)
        (should (equal (alist-get 'sourceKind visited) "comment-reference"))
        (should (equal (alist-get 'quote visited) "Unique theorem statement"))))))

(ert-deftest agent-panel-c81-search-and-lane-filters-use-the-full-thread ()
  (should (fboundp '+agent-panel-set-query))
  (agent-panel-tests--tree root
    (let* ((+agent-panel-buffer-name " *panel search test*")
           (+agent-panel--document (expand-file-name "notes/a.md" root))
           (buffer (+agent-panel--tasks-buffer)))
      (unwind-protect
          (with-current-buffer buffer
            (+agent-collab--add-message +agent-panel--document "c-a1" nil "Need Ω convergence, not merely a bound")
            (+agent-panel-set-query "ω CONVERGENCE")
            (should (string-match-p "▌ on a" (buffer-string)))
            (should-not (string-match-p "Done  1" (buffer-string)))
            (+agent-panel-set-filter 'done)
            (should (string-match-p "No tasks match" (buffer-string)))
            (+agent-panel-set-query "")
            (should (string-match-p "done on a" (buffer-string)))
            (+agent-panel-reset-filters)
            (should (string-match-p "To do  1" (buffer-string)))
            (should (string-match-p "Done  1" (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest agent-panel-c81-inline-details-show-the-whole-thread-and-survive-redraw ()
  (should (fboundp '+agent-panel-toggle-details))
  (agent-panel-tests--tree root
    (let* ((+agent-panel-buffer-name " *panel details test*")
           (+agent-panel--document (expand-file-name "notes/a.md" root))
           (buffer (+agent-panel--tasks-buffer)))
      (unwind-protect
          (with-current-buffer buffer
            (+agent-collab--add-message +agent-panel--document "c-a1" nil "First line\nSecond line with full details")
            (+agent-panel-redraw)
            (goto-char (point-min)) (search-forward "▌ on a")
            (+agent-panel-toggle-details)
            (should (string-match-p "Second line with full details" (buffer-string)))
            (search-forward "Second line")
            (should (equal (+agent-panel--at '+agent-panel-task) (cons +agent-panel--document "c-a1")))
            (+agent-panel-redraw)
            (should (string-match-p "Second line with full details" (buffer-string)))
            (should (equal (+agent-panel--at '+agent-panel-task) (cons +agent-panel--document "c-a1")))
            (+agent-panel-toggle-details)
            (should-not (string-match-p "Second line with full details" (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest agent-panel-c75-h-t-shows-a-right-side-window-and-hides-it ()
  (agent-panel-tests--tree root
    (let ((+agent-binding-last-document (expand-file-name "notes/a.md" root))
          (+agent-panel--tab 'tasks))
      (unwind-protect
          (progn
            (+agent/panel)
            (let ((window (+agent-panel--window)))
              (should window)
              (should (eq (window-parameter window 'window-side) 'right))
              (should (eq (window-buffer window) (get-buffer +agent-panel-buffer-name)))
              (should (memq #'+agent-panel--comments-changed-h +agent-collab-changed-functions)))
            (+agent/panel)
            (should-not (+agent-panel--window))
            ;; Hidden, it follows nothing.
            (should-not (memq #'+agent-panel--comments-changed-h +agent-collab-changed-functions))
            (should-not (memq #'+agent-panel--changed-h +agent-acp-changed-functions))
            (should-not (memq #'+agent-panel--window-change-h window-buffer-change-functions)))
        (when-let* ((window (+agent-panel--window))) (delete-window window))
        (when (get-buffer +agent-panel-buffer-name) (kill-buffer +agent-panel-buffer-name))))))

(ert-deftest agent-panel-c78-a-pi-that-cannot-start-says-so ()
  (let ((+agent-panel--tab 'pi))
    (with-current-buffer (+agent-panel--pi-starting "/tmp/")
      (cl-letf (((symbol-function 'noema-pi-project-ensure) (lambda (_) (user-error "Pi is unavailable"))))
        (+agent-panel--start-pi "/tmp/"))
      (should (string-match-p "Pi could not start: Pi is unavailable" (buffer-string)))
      (kill-buffer))))

(ert-deftest agent-panel-c78-pi-normal-state-switches-tabs-with-digits ()
  "Pi's normal-state digits switch the panel; insert and other chats keep theirs."
  (require 'evil)
  (with-temp-buffer
    (agent-shell-mode)
    (evil-local-mode 1)
    (evil-normal-state)
    (let* ((keys '("1" "2" "3"))
           (original (mapcar #'key-binding keys)))
      (+agent-panel-pi-mode 1)
      (should (equal (mapcar #'key-binding keys)
                     '(+agent-panel-tasks-tab +agent-panel-pi-tab +agent-panel-graph-tab)))
      (evil-insert-state)
      (should (equal (mapcar #'key-binding keys)
                     '(self-insert-command self-insert-command self-insert-command)))
      (evil-normal-state)
      (+agent-panel-pi-mode -1)
      (should (equal (mapcar #'key-binding keys) original)))))

(ert-deftest agent-panel-c78-tabs-are-buffers-in-one-side-window ()
  (agent-panel-tests--tree root
    (let ((+agent-binding-last-document (expand-file-name "notes/a.md" root))
          (+agent-panel--tab 'tasks)
          (pi (generate-new-buffer "*pi fixture*")))
      (unwind-protect
          (cl-letf* ((started nil)
                     ((symbol-function 'noema-pi-project-ensure) (lambda (_directory) pi))
                     ((symbol-function 'noema-pi-router-buffer) (lambda (_directory) (and started pi)))
                     ((symbol-function 'run-with-timer) #'ignore)
                     ((symbol-function '+agent-topology--sessions) (lambda () nil)))
            (provide 'noema-pi-router)
            (+agent/panel)
            (let ((window (+agent-panel--window)))
              ;; Clicking a tab works: the tabs are the window's tab line.
              (should (lookup-key (get-text-property 3 'keymap (+agent-panel-header)) [tab-line mouse-1]))
              ;; A Pi still to start: a starting page at once, nothing waits.
              (+agent-panel--show-tab 'pi)
              (should (equal (buffer-name (window-buffer window)) " *Pi starting*"))
              (+agent-panel--start-pi root)
              (setq started t)
              (should (eq (+agent-panel--window) window))
              (should (eq (window-buffer window) pi))
              (should (buffer-local-value '+agent-panel-pi-mode pi))
              (should (string-match-p "2 Pi" (with-current-buffer pi
                                               (eval (cadr (window-parameter window 'tab-line-format)) t))))
              ;; agent-shell rewrites its header line; the tabs are the window's.
              (with-current-buffer pi (setq header-line-format "agent-shell header"))
              (should (window-parameter window 'tab-line-format))
              (+agent-panel--show-tab 'graph)
              (should (eq (window-buffer window) (get-buffer "*Agent Graph*")))
              (+agent-panel--show-tab 'tasks)
              (should (eq (window-buffer window) (get-buffer +agent-panel-buffer-name)))
              ;; A project Pi shown from anywhere lands in the panel.
              (delete-window window)
              (+agent-panel-display-pi pi)
              (should (eq (window-buffer (+agent-panel--window)) pi))
              (should (eq +agent-panel--tab 'pi))))
        (when-let* ((window (+agent-panel--window))) (delete-window window))
        (dolist (name (list +agent-panel-buffer-name "*Agent Graph*"))
          (when (get-buffer name) (kill-buffer name)))
        (kill-buffer pi)))))

(ert-deftest agent-panel-c75-the-last-focused-document-is-remembered ()
  (let ((+agent-binding-last-document nil)
        (document (generate-new-buffer "SAI.md")))
    (unwind-protect
        (with-current-buffer document
          (setq buffer-file-name "/tmp/x/SAI.md")
          (+agent-binding--track-document-h)
          (should (equal +agent-binding-last-document "/tmp/x/SAI.md"))
          (with-temp-buffer (special-mode) (+agent-binding--track-document-h))
          (should (equal +agent-binding-last-document "/tmp/x/SAI.md")))
      (with-current-buffer document (set-buffer-modified-p nil) (setq buffer-file-name nil))
      (kill-buffer document))))

(provide 'agent-panel-tests)
;;; agent-panel-tests.el ends here
