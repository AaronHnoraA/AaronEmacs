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

(ert-deftest agent-panel-c81-taking-back-unassigns ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (+agent-collab--append file '((op . "assign") (id . "c-a1") (assignee . ((name . "claude·SAI") (session . "tok")))))
      (should (alist-get 'assignee (+agent-collab-thread file "c-a1")))
      (+agent-panel--move-to (+agent-collab-thread file "c-a1") 'todo)
      (should-not (alist-get 'assignee (+agent-collab-thread file "c-a1"))))))

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
