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

(ert-deftest agent-panel-c75-a-new-folder-of-comments-is-found ()
  (agent-panel-tests--tree root
    (let ((file (expand-file-name "notes/a.md" root)))
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 5))
      (agent-panel-tests--journal (expand-file-name "fresh" root) '(("c-f1" "f.md" "fresh" "open")))
      ;; Cached until a comment changes in a folder the cache does not know.
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 5))
      (+agent-panel--comments-changed-h (expand-file-name "fresh/f.md" root))
      (should (equal (length (agent-panel-tests--ids (+agent-panel-tasks file 'project))) 6)))))

(ert-deftest agent-panel-c75-the-task-list-hides-resolved-until-asked ()
  (agent-panel-tests--tree root
    (with-temp-buffer
      (+agent-panel-mode)
      (setq +agent-panel--document (expand-file-name "notes/a.md" root))
      (let ((inhibit-read-only t)) (+agent-panel--draw-tasks))
      (should (string-match-p "this file · 1 open" (buffer-string)))
      (should (string-match-p "on a" (buffer-string)))
      (should-not (string-match-p "done on a" (buffer-string)))
      (setq +agent-panel--show-resolved t)
      (let ((inhibit-read-only t)) (erase-buffer) (+agent-panel--draw-tasks))
      (should (string-match-p "done on a" (buffer-string)))
      (goto-char (point-min))
      (re-search-forward "● on a$")
      (should (equal (cdr (get-text-property (point) '+agent-panel-task)) "c-a1")))))

(ert-deftest agent-panel-c75-h-t-shows-a-right-side-window-and-hides-it ()
  (agent-panel-tests--tree root
    (let ((+agent-binding-last-document (expand-file-name "notes/a.md" root)))
      (unwind-protect
          (progn
            (+agent/panel)
            (let ((window (get-buffer-window +agent-panel-buffer-name)))
              (should window)
              (should (eq (window-parameter window 'window-side) 'right))
              (should (memq #'+agent-panel--comments-changed-h +agent-collab-changed-functions)))
            (+agent/panel)
            (should-not (get-buffer-window +agent-panel-buffer-name))
            ;; Hidden, it follows nothing.
            (should-not (memq #'+agent-panel--comments-changed-h +agent-collab-changed-functions))
            (should-not (memq #'+agent-panel--changed-h +agent-acp-changed-functions))
            (should-not (memq #'+agent-panel--window-change-h window-buffer-change-functions)))
        (when (get-buffer +agent-panel-buffer-name) (kill-buffer +agent-panel-buffer-name))))))

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
