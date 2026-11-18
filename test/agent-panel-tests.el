;;; agent-panel-tests.el --- The collaboration panel (C75) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/panel)

(defun agent-panel-tests--note (root path &rest comments)
  "Write note PATH under ROOT holding COMMENTS ((ID TEXT DONE) ...)."
  (let ((file (expand-file-name path root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "# Note\n\n")
      (pcase-dolist (`(,id ,text ,done) comments)
        (insert (format "Some passage @@comment%s [%s] {id: %s}\n" (if done "(done)" "") text id))))
    file))

(defmacro agent-panel-tests--tree (root &rest body)
  "Bind ROOT to a project tree with comment tasks in several folders, and run BODY."
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory (make-temp-file "agent-panel" t))))
     (unwind-protect
         (progn
           (agent-panel-tests--note ,root "notes/a.md" '("ca0001" "on a" nil) '("ca0002" "done on a" t))
           (agent-panel-tests--note ,root "notes/b.md" '("cb0001" "on b" nil))
           (agent-panel-tests--note ,root "notes/deep/d.md" '("cd0001" "deep" nil))
           (agent-panel-tests--note ,root "other/o.md" '("co0001" "other" nil))
           (agent-panel-tests--note ,root "node_modules/x/n.md" '("cn0001" "skipped" nil))
           (agent-panel-tests--note ,root "notes/plain.md")
           ;; A code file keeps its task in the folder's journal.
           (with-temp-file (expand-file-name "other/main.py" ,root) (insert "x = 1\n"))
           (with-temp-file (expand-file-name "other/.noema-tasks.jsonl" ,root)
             (insert "{\"v\":1,\"op\":\"task\",\"id\":\"cm0001\",\"text\":\"rename x\",\"line\":1,\"quote\":\"x\",\"note\":\"main.py\",\"at\":\"2026-11-17T08:00:00.000Z\"}\n"))
           (cl-letf (((symbol-function '+agent-panel--project-root) (lambda (_) ,root))
                     ((symbol-function '+agent-collab--client-file) #'identity))
             ,@body))
       (delete-directory ,root t))))

(defun agent-panel-tests--ids (groups)
  (sort (mapcar (lambda (comment) (alist-get 'id comment)) (apply #'append (mapcar #'cdr groups))) #'string<))

(ert-deftest agent-panel-c79-scopes-widen-from-the-file-to-the-project ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'file)) '("ca0001" "ca0002")))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'dir)) '("ca0001" "ca0002" "cb0001")))
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'tree)) '("ca0001" "ca0002" "cb0001" "cd0001")))
      ;; The project skips dependency folders and includes a code file's task.
      (should (equal (agent-panel-tests--ids (+agent-panel-tasks file 'project))
                     '("ca0001" "ca0002" "cb0001" "cd0001" "cm0001" "co0001")))
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
        (should (string-match-p "this file · 1 open" (buffer-string)))
        (should (string-match-p "● on a" (buffer-string)))))))

(ert-deftest agent-panel-c79-the-task-list-hides-done-until-asked ()
  (agent-panel-tests--tree root
    (with-temp-buffer
      (+agent-panel-mode)
      (setq +agent-panel--document (expand-file-name "notes/a.md" root))
      (let ((inhibit-read-only t)) (+agent-panel--draw-tasks))
      (should (string-match-p "this file · 1 open, 1 done (hidden)" (buffer-string)))
      (should (string-match-p "after «Some passage»" (buffer-string)))
      (should (string-match-p "not handed over · line 3" (buffer-string)))
      (should-not (string-match-p "done on a" (buffer-string)))
      (setq +agent-panel--show-resolved t)
      (let ((inhibit-read-only t)) (erase-buffer) (+agent-panel--draw-tasks))
      (should (string-match-p "✓ done on a" (buffer-string)))
      (goto-char (point-min))
      (re-search-forward "● on a$")
      (should (equal (cdr (get-text-property (point) '+agent-panel-task)) "ca0001")))))

(ert-deftest agent-panel-c79-c-marks-the-comment-done-in-its-note ()
  (agent-panel-tests--tree root
    (let ((+noema--ready nil))
      (with-temp-buffer
        (+agent-panel-mode)
        (setq +agent-panel--document (expand-file-name "notes/a.md" root))
        (let ((inhibit-read-only t)) (+agent-panel--draw-tasks))
        (goto-char (point-min))
        (re-search-forward "● on a$")
        (cl-letf (((symbol-function '+agent-panel-redraw) #'ignore))
          (+agent-panel-resolve))
        (should (string-match-p "@@comment(done) \\[on a\\] {id: ca0001}"
                                (with-temp-buffer (insert-file-contents (expand-file-name "notes/a.md" root)) (buffer-string))))))))

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
