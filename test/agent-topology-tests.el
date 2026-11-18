;;; agent-topology-tests.el --- Collaborator topology and Treemacs presence (C69) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/acp)
(require 'tools/agent/binding)
(require 'tools/agent/collab)
(require 'tools/agent/topology)
(require 'tools/agent/presence)

(defun agent-topology-tests--sessions ()
  "Two projects: one Pi, two collaborators of SAI.md, one project-level agent."
  (list (list :buffer 'pi :name "pi" :agent "pi" :root "/p/" :kind 'pi :turn-state 'idle)
        (list :buffer 'a :name "claude·SAI" :agent "claude" :root "/p/"
              :file "/fs:local:/p/SAI.md" :turn-state 'running :touched '("/fs:local:/p/notes/a.md"))
        (list :buffer 'b :name "codex·SAI" :agent "codex" :root "/p/"
              :file "/fs:local:/p/SAI.md" :turn-state 'idle :attention-kind 'permission)
        (list :buffer 'c :name "codex·q" :agent "codex" :root "/q/" :turn-state 'idle
              :process-state 'disconnected)))

(ert-deftest agent-topology-c69-model-has-pi-projects-files-and-collaborators ()
  (let* ((model (+agent-topology-model (agent-topology-tests--sessions)
                                       (lambda (_file) (list :pending 2 :threads 1))))
         (nodes (plist-get model :nodes))
         (kinds (mapcar (lambda (node) (cons (plist-get node :id) (plist-get node :kind))) nodes)))
    (should (equal (alist-get "project:/p/" kinds nil nil #'equal) 'project))
    (should (equal (alist-get "project:/q/" kinds nil nil #'equal) 'project))
    (should (equal (alist-get "file:/fs:local:/p/SAI.md" kinds nil nil #'equal) 'file))
    (should (equal (alist-get "file:/fs:local:/p/notes/a.md" kinds nil nil #'equal) 'file))
    (should (equal (alist-get "session:a" kinds nil nil #'equal) 'agent))
    (should (equal (alist-get "session:pi" kinds nil nil #'equal) 'pi))
    (let ((edges (plist-get model :edges)))
      (should (member '("project:/p/" "file:/fs:local:/p/SAI.md" binding) edges))
      (should (member '("file:/fs:local:/p/SAI.md" "session:a" binding) edges))
      (should (member '("session:a" "file:/fs:local:/p/notes/a.md" touched) edges))
      (should (member '("session:pi" "project:/p/" manages) edges))
      ;; A collaborator without a primary file hangs from its project.
      (should (member '("project:/q/" "session:c" binding) edges)))
    (let ((label (lambda (id) (plist-get (seq-find (lambda (node) (equal (plist-get node :id) id)) nodes) :label))))
      (should (string-match-p "working" (funcall label "session:a")))
      (should (string-match-p "needs you" (funcall label "session:b")))
      (should (string-match-p "disconnected" (funcall label "session:c")))
      (should (string-match-p "SAI.md" (funcall label "file:/fs:local:/p/SAI.md")))
      (should (string-match-p "✎2" (funcall label "file:/fs:local:/p/SAI.md")))
      (should (string-match-p "1 thread" (funcall label "file:/fs:local:/p/SAI.md"))))))

(ert-deftest agent-topology-c69-dot-source-quotes-labels ()
  (pcase-let ((`(,source . ,names)
               (+agent-topology--dot-source
                (list :nodes (list (list :id "x" :kind 'file :label "a \"b\"\nc"))
                      :edges nil))))
    (should (string-match-p "n0 \\[shape=note, label=\"a \\\\\"b\\\\\"\\\\nc\"\\]" source))
    (should (equal (gethash "x" names) "n0"))))

(ert-deftest agent-topology-c69-layout-with-graphviz ()
  (skip-unless (executable-find "dot"))
  (let* ((model (+agent-topology-model (agent-topology-tests--sessions) (lambda (_) nil)))
         (layout (+agent-topology--layout model)))
    (should (= (length (plist-get layout :nodes)) (length (plist-get model :nodes))))
    (should (> (plist-get layout :width) 0))
    (should (plist-get (car (plist-get layout :edges)) :points))))

(ert-deftest agent-topology-c69-file-counts-come-from-cards-suggestions-and-threads ()
  (let ((file (make-temp-file "topology" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "a\n#+begin ediff t\n-a\n+b\n#+end ediff\n#+begin ediff r\n@@ediff-status rejected\n-c\n+d\n#+end ediff\n"))
          (cl-letf (((symbol-function '+agent-collab-suggestions) (lambda (&rest _) '(s1 s2)))
                    ((symbol-function '+agent-collab-comments)
                     (lambda (&rest _) '(((done)) ((done)) ((done . t))))))
            (let ((counts (+agent-topology-file-counts file)))
              (should (= (plist-get counts :pending) 3))
              (should (= (plist-get counts :threads) 2)))))
      (delete-file file))))

;;;; Treemacs presence

(ert-deftest agent-presence-c69-entries-count-collaborators-and-aggregate-folders ()
  (let ((entries (+agent-presence-entries
                  (agent-topology-tests--sessions)
                  (lambda (file) (if (string-suffix-p "SAI.md" file) (list :pending 2 :threads 1) nil)))))
    (let ((sai (cdr (assoc "/p/SAI.md" entries))))
      (should (= (plist-get sai :collaborators) 2))
      (should (= (plist-get sai :pending) 2))
      (should (plist-get sai :working)))
    (let ((touched (cdr (assoc "/p/notes/a.md" entries))))
      (should (= (plist-get touched :collaborators) 0))
      (should (plist-get touched :touched)))
    (let ((folder (cdr (assoc "/p/notes" entries))))
      (should (plist-get folder :folder))
      (should (plist-get folder :touched)))
    (should-not (assoc "/q/" entries))))

(ert-deftest agent-presence-c69-suffix-reads-briefly ()
  (should (equal (+agent-presence--suffix '(:collaborators 2 :pending 3)) " ◆2 ✎3"))
  (should (equal (+agent-presence--suffix '(:collaborators 1)) " ◆1"))
  (should (equal (+agent-presence--suffix '(:touched t)) " ◇"))
  (should-not (+agent-presence--suffix nil)))

(ert-deftest agent-presence-c69-applies-and-clears-treemacs-annotations ()
  (let (suffixes faces cleared applied)
    (cl-letf (((symbol-function 'treemacs-clear-annotation-suffixes) (lambda (source) (push source cleared)))
              ((symbol-function 'treemacs-clear-annotation-faces) (lambda (source) (push source cleared)))
              ((symbol-function 'treemacs-set-annotation-suffix)
               (lambda (path suffix source) (push (list path suffix source) suffixes)))
              ((symbol-function 'treemacs-set-annotation-face)
               (lambda (path face source) (push (list path face source) faces)))
              ((symbol-function '+agent-presence--apply-visible) (lambda () (setq applied t))))
      (+agent-presence-apply '(("/p/SAI.md" :collaborators 2 :working t) ("/p/x.md" :touched t)))
      (should (equal cleared '(agent-collab agent-collab)))
      (should (member '("/p/SAI.md" " ◆2" agent-collab) suffixes))
      (should (member '("/p/x.md" " ◇" agent-collab) suffixes))
      (should (equal faces '(("/p/SAI.md" +agent-presence-working agent-collab))))
      (should applied))))

(ert-deftest agent-presence-c69-a-folder-counts-each-file-once ()
  (let* ((sessions (list (list :buffer 'a :root "/p/" :file "/p/d/x.md")
                         (list :buffer 'b :root "/p/" :file "/p/d/x.md")
                         (list :buffer 'c :root "/p/" :file "/p/d/y.md")))
         (folder (cdr (assoc "/p/d" (+agent-presence-entries sessions (lambda (_) nil))))))
    (should (= (plist-get folder :collaborators) 3))))

(provide 'agent-topology-tests)
;;; agent-topology-tests.el ends here
