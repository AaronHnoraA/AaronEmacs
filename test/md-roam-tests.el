;;; md-roam-tests.el --- Unit tests for Markdown roam links -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-md-roam)
(require 'noema-agenda)

(ert-deftest noema-roam-opens-local-note-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function 'my/noema-roam--touch-recent) #'ignore)
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (my/noema-roam--open-file-smart
       "/Users/test/notes/page.md" '(:slug "page")))
    (should (equal visited "/Users/test/notes/page.md"))
    (should-not (file-remote-p context))))

(ert-deftest noema-roam-temporary-boards-are-owned-by-frame ()
  (let ((views (make-hash-table :test #'eq))
        (active-frame 'frame-a)
        first second)
    (cl-letf (((symbol-function 'selected-frame)
               (lambda () active-frame))
              ((symbol-function 'frame-live-p)
               (lambda (_frame) t))
              ((symbol-function 'frame-parameter)
               (lambda (frame _key) (gethash frame views)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame _key value) (puthash frame value views))))
      (unwind-protect
          (progn
            (setq first (my/noema-roam--prepare-ui-buffer
                         "*roam-frame-test*" "Report" 'note #'ignore))
            (should (eq first (my/noema-roam--prepare-ui-buffer
                               "*roam-frame-test*" "Report" 'note #'ignore)))
            (setq active-frame 'frame-b
                  second (my/noema-roam--prepare-ui-buffer
                          "*roam-frame-test*" "Report" 'note #'ignore))
            (should-not (eq first second))
            (with-current-buffer first
              (should (eq my/noema-roam--ui-owner-frame 'frame-a))
              (should quit-window-kill-buffer))
            (with-current-buffer second
              (should (eq my/noema-roam--ui-owner-frame 'frame-b)))
            (kill-buffer first)
            (should-not (gethash 'frame-a views))
            (should (buffer-live-p second))
            (my/noema-roam--delete-frame-ui-h 'frame-b)
            (should-not (buffer-live-p second))
            (should-not (gethash 'frame-b views)))
        (dolist (buffer (list first second))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest noema-roam-temporary-board-q-returns-and-releases-buffer ()
  (save-window-excursion
    (let* ((source (generate-new-buffer "*Roam report source*"))
           (view (my/noema-roam--prepare-ui-buffer
                  "*roam-quit-test*" "Report" 'note #'ignore)))
      (unwind-protect
          (progn
            (switch-to-buffer source)
            (select-window
             (display-buffer view '(display-buffer-same-window)))
            (call-interactively (key-binding (kbd "q")))
            (should (eq (window-buffer) source))
            (should-not (buffer-live-p view)))
        (when (buffer-live-p view)
          (kill-buffer view))
        (kill-buffer source)))))

(ert-deftest noema-roam-async-reports-return-to-origin-frame ()
  (let ((origin (generate-new-buffer " *roam-report-origin*"))
        (active-frame 'frame-a)
        (frame-a-live t)
        callback rendered)
    (unwind-protect
        (with-current-buffer origin
          (let ((my/noema--ready t))
            (cl-letf (((symbol-function 'selected-frame)
                       (lambda () active-frame))
                      ((symbol-function 'select-frame)
                       (lambda (frame &optional _norecord)
                         (setq active-frame frame)))
                      ((symbol-function 'frame-live-p)
                       (lambda (frame)
                         (or (eq frame 'frame-b) frame-a-live)))
                      ((symbol-function 'my/noema--api-call)
                       (lambda (_channel _args fn) (setq callback fn)))
                      ((symbol-function 'my/noema-roam--render-wanted-buffer)
                       (lambda (&rest _)
                         (setq rendered (list active-frame (current-buffer)))))
                      ((symbol-function 'my/noema-roam--render-orphaned-assets-buffer)
                       (lambda (&rest _)
                         (setq rendered (list active-frame (current-buffer)))))
                      ((symbol-function 'my/noema-roam--render-tag-overlap-buffer)
                       (lambda (&rest _)
                         (setq rendered (list active-frame (current-buffer))))))
              (my/noema-roam-report-wanted)
              (setq active-frame 'frame-b)
              (funcall callback '((items . [])))
              (should (equal rendered (list 'frame-a origin)))
              (should (eq active-frame 'frame-b))
              (setq active-frame 'frame-a
                    rendered nil)
              (my/noema-roam-report-orphaned-assets)
              (setq active-frame 'frame-b)
              (funcall callback '((assets . [])))
              (should (equal rendered (list 'frame-a origin)))
              (should (eq active-frame 'frame-b))
              (setq active-frame 'frame-a
                    rendered nil)
              (noema-report-tag-overlap)
              (setq active-frame 'frame-b)
              (funcall callback '((duplicateCase . [])
                                  (overlaps . [])
                                  (tagCount . 0)))
              (should (equal rendered (list 'frame-a origin)))
              (should (eq active-frame 'frame-b))
              (setq active-frame 'frame-a
                    frame-a-live nil
                    rendered nil)
              (my/noema-roam-report-wanted)
              (setq active-frame 'frame-b)
              (funcall callback '((items . [])))
              (should-not rendered))))
      (kill-buffer origin))))

(ert-deftest noema-reports-menu-connects-wiki-and-emacs-views ()
  (should (commandp 'noema-reports-menu))
  (should (member "noema-reports-menu"
                  (all-completions "noema-reports-menu" obarray #'commandp)))
  (dolist (entry '(("v" . my/noema-wiki-reports)
                   ("w" . my/noema-roam-report-wanted)
                   ("o" . my/noema-roam-report-orphaned)
                   ("a" . my/noema-roam-report-orphaned-assets)
                   ("d" . my/noema-roam-report-dead-end)
                   ("u" . my/noema-roam-report-uncategorized)
                   ("h" . my/noema-roam-report-most-linked)
                   ("t" . noema-report-tag-overlap)))
    (should (eq (plist-get
                 (cdr (transient-get-suffix 'noema-reports-menu (car entry)))
                 :command)
                (cdr entry))))
  (should (eq (plist-get (cdr (transient-get-suffix 'noema-wiki-menu "!"))
                         :command)
              'my/noema-wiki-reports))
  (should (eq (plist-get (cdr (transient-get-suffix 'noema-wiki-menu "D"))
                         :command)
              'noema-reports-menu))
  (should (eq (plist-get (cdr (transient-get-suffix 'noema-menu "!n"))
                         :command)
              'noema-reports-menu)))

(ert-deftest noema-report-tag-overlap-uses-temporary-report-board ()
  (let (view)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq view buffer))))
          (my/noema-roam--render-tag-overlap-buffer
           '((duplicateCase . [((lower . "research")
                                (variants . ["Research" "research"]))])
             (overlaps . [((a . "research") (b . "math")
                           (aCount . 4) (bCount . 3) (sharedCount . 3)
                           (containment . 1.0) (jaccard . 0.75))])
             (tagCount . 5)))
          (should (buffer-live-p view))
          (with-current-buffer view
            (should (derived-mode-p 'my/noema-roam-ui-mode))
            (should (eq my/noema-roam--ui-owner-frame (selected-frame)))
            (should quit-window-kill-buffer)
            (should (eq my/noema-roam-ui-refresh-function
                        #'noema-report-tag-overlap))
            (should (string-match-p "Research / research" (buffer-string)))
            (should (string-match-p "#research  ↔  #math"
                                    (buffer-string)))
            (should (string-match-p "3 shared · 4/3 notes"
                                    (buffer-string)))
            (should (string-match-p "100% containment · 75% Jaccard"
                                    (buffer-string)))))
      (when (buffer-live-p view)
        (kill-buffer view)))))

(ert-deftest my/noema-roam-new-note-commands-are-interactive ()
  (dolist (command '(roam-new-note markdown-new-document
                              my/noema-roam-new-node))
    (should (commandp command))))

(ert-deftest noema-agenda-entries-are-searchable-and-share-the-menu-route ()
  "Native and hosted Agenda views have distinct searchable commands."
  (dolist (command '(noema-agenda noema-agenda-web
                     noema-agenda-calendar noema-agenda-log
                     noema-agenda-gantt noema-agenda-projects
                     noema-agenda-clocktable noema-agenda-lints
                     noema-agenda-search))
    (should (commandp command))
    (should (member (symbol-name command)
                    (all-completions "noema-agenda" obarray #'commandp))))
  (should (eq (lookup-key my/noema-roam-map (kbd "A"))
              #'noema-agenda)))

(ert-deftest my/noema-roam-new-template-candidates-match-note-type ()
  (with-temp-buffer
    (my/noema-roam-new-mode)
    (setq-local my/noema-roam-new--templates
                '((:key "roam" :name "Linked" :nodeType "roam")
                  (:key "basic" :name "Plain" :nodeType "regular")
                  (:key "shared" :name "Shared"))
                my/noema-roam-new--draft
                '(:node-type "roam" :kind "note" :template-key "roam"))
    (should (equal (mapcar #'cdr (my/noema-roam-new--template-candidates))
                   '("" "roam" "shared")))
    (setq-local my/noema-roam-new--draft
                '(:node-type "regular" :kind "default" :template-key "basic"))
    (should (equal (mapcar #'cdr (my/noema-roam-new--template-candidates))
                   '("" "basic" "shared")))))

(ert-deftest my/noema-roam-new-switch-type-changes-default-template ()
  (with-temp-buffer
    (my/noema-roam-new-mode)
    (setq-local my/noema-roam-new--templates
                '((:key "roam" :name "Linked" :nodeType "roam")
                  (:key "basic" :name "Plain" :nodeType "regular"))
                my/noema-roam-new--draft
                '(:node-type "roam" :title "Test" :path "test.md"
                  :kind "note" :template-key "roam"))
    (my/noema-roam-new-render)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "regular")))
      (my/noema-roam-new-edit-type))
    (should (equal (plist-get my/noema-roam-new--draft :template-key)
                   "basic"))
    (should (equal (plist-get my/noema-roam-new--draft :kind)
                   "default"))))

(defun my/noema-roam-test--write-file (file content)
  "Write CONTENT to FILE, creating parents."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defmacro my/noema-roam-test-with-vault (&rest body)
  "Run BODY with a temporary Markdown roam vault."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "md-roam-test-" t)))
          (my/noema-roam-root root)
          (my/noema--notes-root root)
          (my/noema-roam--db-cache nil)
          (my/noema-roam--db-path-cache nil)
          (my/noema-roam--db-mtime nil)
          (my/noema-roam--runtime-index-cache nil)
          (my/noema-roam--runtime-index-cache-key nil)
          (my/noema-roam--scan-cache nil)
          (note-file (expand-file-name "demo/topology.md" root))
          (other-file (expand-file-name "demo/analysis.md" root)))
     (unwind-protect
         (progn
           (my/noema-roam-test--write-file
            note-file
            "#+begin meta
id: 20260605T120000-topology
title: Topology Note
date: 2026-06-05
kind: default
tags: math, topology
aliases: topological space
refs: roam://20260605T120000-analysis#analysis
source: roam/demo/topology.md
#+end meta

# Main Title {#main}

## Hausdorff Space {#hausdorff}

## Nested

### Child Heading {#child}
")
           (my/noema-roam-test--write-file
            other-file
            "#+begin meta
id: 20260605T120000-analysis
title: Analysis Note
date: 2026-06-05
kind: default
tags: math
refs: demo/topology.md@main-title
source: roam/demo/analysis.md
#+end meta

# Analysis Note {#analysis}

[Topology](demo/topology.md@main-title)
")
           (setq my/noema-roam--db-cache nil
                 my/noema-roam--db-path-cache nil
                 my/noema-roam--db-mtime nil
                 my/noema-roam--runtime-index-cache nil
                 my/noema-roam--runtime-index-cache-key nil
                 my/noema-roam--scan-cache nil)
           ,@body)
	       (delete-directory root t))))

(ert-deftest noema-move-note-uses-host-move-and-retargets-native-buffer ()
  (my/noema-roam-test-with-vault
    (let* ((target-dir (expand-file-name "archive" root))
           (target (expand-file-name "topology.md" target-dir))
           (source (find-file-noselect note-file))
           (my/noema--ready t)
           calls)
      (make-directory target-dir)
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--read-note-id)
                     (lambda (_) "20260605T120000-topology"))
                    ((symbol-function 'my/noema-roam--slug-to-file)
                     (lambda (_) note-file))
                    ((symbol-function 'read-file-name)
                     (lambda (_prompt dir _default _must-match initial &rest _)
                       (should (equal dir (file-name-directory note-file)))
                       (should (equal initial "topology.md"))
                       target))
                    ((symbol-function 'my/noema--api-call)
                     (lambda (channel args callback)
                       (push (cons channel args) calls)
                       (rename-file note-file target)
                       (with-temp-file target (insert "Rewritten after move\n"))
                       (funcall callback
                                `((ok . t) (file . ,target)
                                  (referenceRewrite . ((referenceCount . 3))))))))
            (noema-move-note)
            (should (= (length calls) 1))
            (should (equal (caar calls) "aaronnote:api:fs:move"))
            (should (equal (alist-get "target" (aref (cdar calls) 0)
                                      nil nil #'equal)
                           target))
            (with-current-buffer source
              (should (file-equal-p buffer-file-name target))
              (should (equal (buffer-string) "Rewritten after move\n"))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest noema-move-note-keeps-open-editor-drafts-safe ()
  (my/noema-roam-test-with-vault
    (let ((web (generate-new-buffer " *noema-move-web-pane*"))
          (my/noema--ready t)
          (prompted nil)
          (called nil)
          (destination note-file)
          source occupied)
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--read-note-id)
                     (lambda (_) "20260605T120000-topology"))
                    ((symbol-function 'my/noema-roam--slug-to-file)
                     (lambda (_) note-file))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _) (setq prompted t) destination))
                    ((symbol-function 'my/noema--api-call)
                     (lambda (&rest _) (setq called t))))
            (with-current-buffer web
              (setq-local my/noema-buffer-file-name
                          (file-truename note-file)))
            (should-error (noema-move-note) :type 'user-error)
            (should-not prompted)
            (should-not called)
            (kill-buffer web)
            (setq source (find-file-noselect note-file))
            (with-current-buffer source
              (goto-char (point-max))
              (insert "draft"))
            (should-error (noema-move-note) :type 'user-error)
            (should-not prompted)
            (should-not called)
            (with-current-buffer source (set-buffer-modified-p nil))
            (setq destination (expand-file-name "demo/occupied.md" root)
                  occupied (find-file-noselect destination))
            (should-error (noema-move-note) :type 'user-error)
            (should prompted)
            (should-not called))
        (when (buffer-live-p web) (kill-buffer web))
        (when (buffer-live-p occupied) (kill-buffer occupied))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(ert-deftest noema-move-note-routes-open-page-to-its-client ()
  (my/noema-roam-test-with-vault
    (let* ((web (generate-new-buffer " *noema-move-page*"))
           (target (expand-file-name "demo/moved.md" root))
           (my/noema--ready t)
           routed)
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--read-note-id)
                     (lambda (_) "20260605T120000-topology"))
                    ((symbol-function 'my/noema-roam--slug-to-file)
                     (lambda (_) note-file))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _) target))
                    ((symbol-function 'my/noema-command)
                     (lambda (command detail)
                       (setq routed (list (current-buffer) command detail))))
                    ((symbol-function 'my/noema--api-call)
                     (lambda (&rest _) (ert-fail "Web pane must own the move"))))
            (with-current-buffer web
              (setq-local my/noema-buffer-file-name (file-truename note-file))
              (setq-local my/noema--client-id "test-client"))
            (noema-move-note)
            (should (eq (nth 0 routed) web))
            (should (equal (nth 1 routed) "move-note"))
            (should (equal (alist-get 'file (nth 2 routed))
                           (file-truename note-file)))
            (should (equal (alist-get 'target (nth 2 routed)) target)))
        (when (buffer-live-p web) (kill-buffer web))))))

(ert-deftest noema-move-note-event-retargets-native-buffer ()
  (my/noema-roam-test-with-vault
    (let* ((target (expand-file-name "demo/moved.md" root))
           (source (find-file-noselect note-file)))
      (unwind-protect
          (progn
            (rename-file note-file target)
            (with-temp-file target (insert "Host-rewritten note\n"))
            (my/noema-roam--handle-moved-note
             `((oldFile . ,note-file) (file . ,target)))
            (with-current-buffer source
              (should (file-equal-p buffer-file-name target))
              (should (equal (buffer-string) "Host-rewritten note\n"))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest noema-trash-note-event-closes-clean-native-buffer ()
  (my/noema-roam-test-with-vault
    (let ((source (find-file-noselect note-file)))
      (unwind-protect
          (progn
            (delete-file note-file)
            (my/noema-roam--handle-trashed-note `((file . ,note-file)))
            (should-not (buffer-live-p source)))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest noema-trash-note-event-keeps-edited-native-draft ()
  (my/noema-roam-test-with-vault
    (let ((source (find-file-noselect note-file)))
      (unwind-protect
          (progn
            (with-current-buffer source
              (goto-char (point-max))
              (insert "Unsaved draft\n"))
            (delete-file note-file)
            (my/noema-roam--handle-trashed-note `((file . ,note-file)))
            (with-current-buffer source
              (should-not buffer-file-name)
              (should (buffer-modified-p))
              (should (string-prefix-p "Trashed draft:" (buffer-name)))
              (when (fboundp 'my/tab-line-visible-buffer-p)
                (should (my/tab-line-visible-buffer-p source)))
              (should (string-match-p "Unsaved draft" (buffer-string)))))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(ert-deftest my/noema-roam-offline-scan-requires-explicit-id ()
  (my/noema-roam-test-with-vault
    (let ((plain-file (expand-file-name "demo/ordinary.md" root))
          (my/noema-roam--all-files-cache nil))
      (cl-letf (((symbol-function 'my/noema-roam--runtime-index)
                 (lambda () nil)))
      (my/noema-roam-test--write-file
       plain-file "#+begin meta\ntitle: Ordinary\n#+end meta\n\n# Ordinary\n")
      (should-not (my/noema-roam--scan-record plain-file))
      (should (= 2 (length (my/noema-roam--scanned-note-records))))
      (my/noema-roam-test--write-file
       plain-file "#+begin meta\nid: ordinary-id\ntitle: Ordinary\n#+end meta\n")
      (my/noema-roam--invalidate-note plain-file)
      (should (equal "ordinary-id"
                     (plist-get (my/noema-roam--resolve-note "ordinary-id") :id)))
      (my/noema-roam-test--write-file
       plain-file "#+begin meta\nid: ordinary-id\nroam: off\n#+end meta\n")
      (my/noema-roam--invalidate-note plain-file)
      (should-not (my/noema-roam--resolve-note "ordinary-id"))))))

(ert-deftest my/noema-roam-runtime-records-reject-ordinary-markdown ()
  (let* ((plain (make-hash-table :test 'equal))
         (hidden (make-hash-table :test 'equal))
         (active (make-hash-table :test 'equal))
         (payload (make-hash-table :test 'equal)))
    (puthash "id" "hidden-id" hidden)
    (puthash "roam" "off" hidden)
    (puthash "id" "active-id" active)
    (puthash "roam" t active)
    (puthash "notes" (list plain hidden active) payload)
    (cl-letf (((symbol-function 'my/noema-roam--runtime-index)
               (lambda () payload)))
      (should (equal '("active-id")
                     (mapcar (lambda (record) (plist-get record :id))
                             (my/noema-roam--runtime-note-records)))))))

(defun my/noema-roam-test--agenda-model (todos from days)
  "Return a small server-shaped agenda view model for TODOS."
  (let* ((from (or from (format-time-string "%Y-%m-%d")))
         (days (or days 7))
         (today (format-time-string "%Y-%m-%d"))
         (dates nil)
         (entries-by-date (make-hash-table :test 'equal))
         (open 0)
         (doing 0)
         (done 0)
         (cancelled 0)
         (blocked 0)
         (overdue 0))
    (dotimes (index days)
      (let ((date (my/noema-agenda--date-add from index)))
        (push date dates)
        (puthash date nil entries-by-date)))
    (dolist (todo todos)
      (let* ((id (my/noema-roam--todo-field todo "id"))
             (status (my/noema-roam--todo-status todo))
             (date (my/noema-roam--todo-agenda-date todo)))
        (pcase status
          ("blocked" (setq blocked (1+ blocked)))
          ("doing" (setq doing (1+ doing)))
          ((or "done" "complete" "completed") (setq done (1+ done)))
          ((or "cancelled" "canceled") (setq cancelled (1+ cancelled)))
          (_ (setq open (1+ open))))
        (when (and date (string< date today)
                   (not (my/noema-roam--todo-closed-p todo)))
          (setq overdue (1+ overdue)))
        (when (and id date
                   (not (eq (gethash date entries-by-date 'missing)
                            'missing)))
          (push `(:kind "deadline"
                  :label "Deadline"
                  :todoId ,id
                  :date ,date
                  :dateKey "ddl"
                  :urgency 0)
                (gethash date entries-by-date)))))
    `(:type "agenda"
      :range (:from ,from
              :to ,(my/noema-agenda--date-add from (1- days))
              :today ,today)
      :days ,(mapcar (lambda (date)
                       `(:date ,date
                         :entries ,(nreverse (gethash date entries-by-date))))
                     (nreverse dates))
      :todos ,todos
      :lints nil
      :logByDay nil
      :stats (:open ,open
              :doing ,doing
              :done ,done
              :cancelled ,cancelled
              :blocked ,blocked
              :overdue ,overdue))))

(defun my/noema-roam-test--agenda-runtime (todos)
  "Return a mock `my/noema-roam--runtime-call' for agenda TODOS."
  (lambda (&rest args)
    (pcase (car args)
      ("agenda"
       (let* ((json-str (or (cadr (member "--json" args)) "{}"))
              (body (json-parse-string json-str :object-type 'alist))
              (from (alist-get 'from body))
              (days (alist-get 'days body)))
         (my/noema-roam-test--agenda-model todos from days)))
      (_ nil))))

(ert-deftest my/noema-roam-parse-canonical-targets ()
  (my/noema-roam-test-with-vault
    (let ((plain (my/noema-roam--parse-target
                  "roam://20260605T120000-topology"))
          (tag (my/noema-roam--parse-target
                "roam://20260605T120000-topology#eq-eq%3A1"))
          (dom (my/noema-roam--parse-target
                "roam://20260605T120000-topology@nested@child-heading")))
      (should (equal (plist-get plain :slug) "20260605T120000-topology"))
      (should (equal (plist-get tag :id) "eq-eq:1"))
      (should (equal (plist-get dom :dom) "nested@child-heading"))
      (should (equal (plist-get dom :file) note-file)))
    (with-temp-buffer
      (setq buffer-file-name note-file)
      (let ((local (my/noema-roam--parse-target
                    "@@main-title@nested@child-heading")))
        (should (plist-get local :local))
        (should (equal (plist-get local :dom)
                       "main-title@nested@child-heading"))
        (should (equal (plist-get local :file) note-file))))))

(ert-deftest my/noema-roam-resolves-path-title-alias-and-tag ()
  (my/noema-roam-test-with-vault
    (dolist (ref '("demo/topology.md"
                   "demo/topology"
                   "Topology Note"
                   "topological space"
                   "topology"))
      (should (equal (plist-get (my/noema-roam--resolve-note ref) :id)
                     "20260605T120000-topology")))))

(ert-deftest my/noema-roam-normalizes-db-backlinks-to-note-id ()
  (my/noema-roam-test-with-vault
    (should (member "20260605T120000-analysis"
                    (my/noema-roam--db-backlinks-to
                     "20260605T120000-topology")))))

(ert-deftest my/noema-roam-selector-builds-note-tag-and-toc-targets ()
  (my/noema-roam-test-with-vault
    (let* ((record (my/noema-roam--resolve-note "demo/topology.md"))
           (tags (my/noema-roam--tag-targets record))
           (toc-targets (my/noema-roam-select--toc-targets record))
           (child (seq-find
                   (lambda (target)
                     (equal (plist-get target :path)
                            '("main-title" "nested" "child-heading")))
                   toc-targets)))
      (should (equal (my/noema-roam--link-target-for-record record 'id)
                     "roam://20260605T120000-topology"))
      (should (equal (my/noema-roam--link-target-for-record record 'path)
                     "demo/topology.md"))
      (should (seq-find (lambda (target)
                          (equal (plist-get target :id) "hausdorff"))
                        tags))
      (should (equal (my/noema-roam--link-target-for-record
                      record 'id 'tag "hausdorff")
                     "roam://20260605T120000-topology#hausdorff"))
      (should (equal (my/noema-roam--link-target-for-record
                      record 'path 'tag "hausdorff")
                     "demo/topology.md#hausdorff"))
      (should child)
      (should (equal (my/noema-roam-select--toc-dom child)
                     "main-title@nested@child-heading"))
      (should (equal (my/noema-roam--link-target-for-record
                      record 'id 'dom
                      (my/noema-roam-select--toc-dom child))
                     "roam://20260605T120000-topology@main-title@nested@child-heading"))
      (should (equal (my/noema-roam--link-target-for-record
                      record 'path 'dom
                      (my/noema-roam-select--toc-dom child))
                     "demo/topology.md@main-title@nested@child-heading")))))

(ert-deftest my/noema-roam-selector-browses-root-and-toc-levels ()
  (my/noema-roam-test-with-vault
    (let* ((root-items (my/noema-roam-select--directory-items ""))
           (demo (seq-find (lambda (item)
                             (and (eq (plist-get item :type) 'dir)
                                  (equal (plist-get item :name) "demo")))
                           root-items))
           (demo-items (my/noema-roam-select--directory-items "demo/"))
           (record (my/noema-roam--resolve-note "demo/topology.md"))
           (targets (my/noema-roam-select--toc-targets record))
           (top (seq-find (lambda (target)
                            (equal (plist-get target :path)
                                   '("main-title")))
                          targets))
           (children (my/noema-roam-select--toc-children
                      targets '("main-title"))))
      (should demo)
      (should (seq-find (lambda (item)
                          (and (eq (plist-get item :type) 'note)
                               (equal (plist-get
                                       (plist-get item :record)
                                       :id)
                                      "20260605T120000-topology")))
                        demo-items))
      (should top)
      (should (seq-find (lambda (target)
                          (equal (plist-get target :path)
                                 '("main-title" "hausdorff-space")))
                        children))
      (should (seq-find (lambda (target)
                          (equal (plist-get target :path)
                                 '("main-title" "nested")))
                        children)))))

(ert-deftest my/noema-roam-selector-inserts-at-origin-marker ()
  (my/noema-roam-test-with-vault
    (let* ((record (my/noema-roam--resolve-note "demo/topology.md"))
           source marker)
      (with-temp-buffer
        (setq source (current-buffer)
              marker (copy-marker (point) t))
        (with-temp-buffer
          (my/noema-roam-select-mode)
          (setq-local my/noema-roam-select--origin-marker marker)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "Topology")))
            (my/noema-roam-select--finish-target
             record 'id nil nil "Topology Note")))
        (with-current-buffer source
          (should (equal (buffer-string)
                         "[Topology](roam://20260605T120000-topology)")))))))

(ert-deftest my/noema-roam-selector-opens-bottom-search-view ()
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*Noema roam select*")))
      (kill-buffer buffer))
    (with-temp-buffer
      (let (displayed-buffer display-alist)
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   (lambda (buffer alist)
                     (setq displayed-buffer buffer
                           display-alist alist)
                     (selected-window))))
          (my/noema-roam-select-link))
        (should (eq displayed-buffer (get-buffer "*Noema roam select*")))
        (should (eq (cdr (assq 'side display-alist)) 'bottom))
        (with-current-buffer "*Noema roam select*"
          (should (eq my/noema-roam-select--view 'search))
          (should (string-match-p "Topology Note"
                                  (buffer-string))))))))

(ert-deftest my/noema-roam-setup-does-not-install-capf ()
  (with-temp-buffer
    (my/noema-roam-setup-keys)
    (should-not (fboundp 'my/noema-roam--capf))
    (should-not (memq (intern "my/noema-roam--capf")
                      completion-at-point-functions))))

(ert-deftest my/noema-roam-follow-link-jumps-to-tag-and-dom-target ()
  (my/noema-roam-test-with-vault
    (let (opened)
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert "[Hausdorff](demo/topology.md#hausdorff)")
              (goto-char (point-min))
              (search-forward "hausdorff")
              (my/noema-roam-follow-link)
              (setq opened (current-buffer))
              (should (equal (file-truename buffer-file-name)
                             (file-truename note-file)))
              (should (looking-at "{#hausdorff}")))
            (with-temp-buffer
              (insert "[Child](roam://20260605T120000-topology@nested@child-heading)")
              (goto-char (point-min))
              (search-forward "child-heading")
              (my/noema-roam-follow-link)
              (setq opened (current-buffer))
              (should (equal (file-truename buffer-file-name)
                             (file-truename note-file)))
              (save-excursion
                (beginning-of-line)
                (should (looking-at "### Child Heading"))))
            (with-current-buffer opened
              (goto-char (point-max))
              (insert "\n[Local child](@@main-title@nested@child-heading)\n")
              (search-backward "child-heading")
              (my/noema-roam-follow-link)
              (should (equal (file-truename buffer-file-name)
                             (file-truename note-file)))
              (save-excursion
                (beginning-of-line)
                (should (looking-at "### Child Heading")))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

(ert-deftest my/noema-roam-xref-locates-tag-and-dom-for-id-and-path ()
  (my/noema-roam-test-with-vault
    (dolist (case '(("roam://20260605T120000-topology#hausdorff" 14 19)
                    ("demo/topology.md#hausdorff" 14 19)
                    ("roam://20260605T120000-topology@nested@child-heading" 18 0)
                    ("demo/topology.md@nested@child-heading" 18 0)))
      (pcase-let ((`(,target ,line ,column) case))
        (let* ((defs (xref-backend-definitions 'aaronnote-roam target))
               (loc (xref-item-location (car defs))))
          (should (equal (file-truename (xref-file-location-file loc))
                         (file-truename note-file)))
          (should (= (xref-file-location-line loc) line))
          (should (= (xref-file-location-column loc) column)))))))

(ert-deftest my/noema-roam-gd-jumps-to-tag-and-dom-for-id-and-path ()
  (my/noema-roam-test-with-vault
    (let (opened)
      (unwind-protect
          (dolist (case '(("[Hausdorff](roam://20260605T120000-topology#hausdorff)" 14 "{#hausdorff}")
                          ("[Hausdorff](demo/topology.md#hausdorff)" 14 "{#hausdorff}")
                          ("[Child](roam://20260605T120000-topology@nested@child-heading)" 18 "### Child Heading")
                          ("[Child](demo/topology.md@nested@child-heading)" 18 "### Child Heading")))
            (pcase-let ((`(,link ,line ,pattern) case))
              (with-temp-buffer
                (insert link)
                (goto-char (point-min))
                (search-forward "]")
                (my/noema-roam-goto-definition)
                (setq opened (current-buffer))
                (should (equal (file-truename buffer-file-name)
                               (file-truename note-file)))
                (should (= (line-number-at-pos) line))
                (should (looking-at pattern)))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

(ert-deftest my/noema-roam-ui-row-keeps-face-and-activates ()
  (with-temp-buffer
    (my/noema-roam-ui-mode)
    (let (activated)
      (my/noema-roam-ui-render
       (lambda ()
         (my/noema-roam-ui-insert-row
          :id "modern-row"
          :icon 'note
          :title "Modern row"
          :action (lambda (_ignored) (setq activated t)))))
      (goto-char (point-min))
      (search-forward "Modern row")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'face)
                  'my/noema-roam-ui-row-title))
      (should (equal (get-text-property
                      (point) 'my/noema-roam-ui-item-id)
                     "modern-row"))
      (should-not (button-at (point)))
      (my/noema-roam-ui-activate)
      (should activated))))

(ert-deftest my/noema-roam-ui-render-preserves-current-row ()
  (with-temp-buffer
    (my/noema-roam-ui-mode)
    (let ((renderer
           (lambda ()
             (my/noema-roam-ui-insert-row
              :id "first" :title "First")
             (my/noema-roam-ui-insert-row
              :id "second" :title "Second"))))
      (my/noema-roam-ui-render renderer)
      (goto-char (point-min))
      (search-forward "Second")
      (goto-char (match-beginning 0))
      (my/noema-roam-ui-render renderer)
      (should (equal (get-text-property
                      (point) 'my/noema-roam-ui-item-id)
                     "second")))))

(ert-deftest my/noema-roam-native-views-use-shared-ui-mode ()
  (my/noema-roam-test-with-vault
    (let* ((todo '(:note "20260605T120000-topology"
                   :title "Topology Note"
                   :text "Review compact workbench"
                   :status "doing"))
           (summary '(:slug "20260605T120000-topology"
                      :title "Topology Note"
                      :path "demo/topology.md"
                      :tags ("math" "topology")
                      :summary "A sample note"))
           (buffers '("*roam-todos*"
                      "*Noema roam notes*")))
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--todos)
                     (lambda (callback) (funcall callback (list todo))))
                    ((symbol-function 'my/noema-roam--all-note-summaries)
                     (lambda () (list summary)))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/noema-roam-todos)
            (my/noema-roam--show-note-list "Notes" (list summary))
            (dolist (name buffers)
              (with-current-buffer name
                (should (derived-mode-p 'my/noema-roam-ui-mode))
                (should header-line-format)
                (should (functionp my/noema-roam-ui-refresh-function))
                (goto-char (point-min))
                (forward-button 1)
                (should (button-at (point))))))
        (dolist (name buffers)
          (when-let* ((buffer (get-buffer name)))
            (kill-buffer buffer)))))))

(ert-deftest my/noema-roam-orphaned-assets-report-renders-api-result ()
  (my/noema-roam-test-with-vault
    (let ((my/noema--ready t)
          (asset-file (expand-file-name "attachments/orphan.pdf" root)))
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema--api-call)
                     (lambda (channel args callback)
                       (should (equal channel
                                      "aaronnote:api:assets:scan-orphans"))
                       (should (equal args []))
                       (funcall callback
                                `((type . "unused-assets")
                                  (root . ,root)
                                  (assets . [((file . ,asset-file)
                                              (path . "attachments/orphan.pdf")
                                              (name . "orphan.pdf")
                                              (type . "application/pdf")
                                              (size . 2048)
                                              (mtimeMs . 1800000000000)
                                              (isImage . :json-false))])))))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/noema-roam-report-orphaned-assets)
            (with-current-buffer "*roam-orphaned-assets*"
              (should (derived-mode-p 'my/noema-roam-ui-mode))
              (should (functionp my/noema-roam-ui-refresh-function))
              (should (string-match-p "Orphaned attachments" (buffer-string)))
              (should (string-match-p "attachments/orphan.pdf" (buffer-string)))
              (should (string-match-p "2k" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*roam-orphaned-assets*")))
          (kill-buffer buffer))))))

(ert-deftest my/noema-roam-trash-orphaned-assets-confirms-and-refreshes ()
  (my/noema-roam-test-with-vault
    (let* ((my/noema--ready t)
           (asset-file (expand-file-name "attachments/orphan.pdf" root))
           (asset `((file . ,asset-file)
                    (path . "attachments/orphan.pdf")
                    (type . "application/pdf")
                    (size . 2048)
                    (mtimeMs . 1800000000000)))
           called-channel
           called-args)
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _args) t))
                    ((symbol-function 'my/noema--api-call)
                     (lambda (channel args callback)
                       (setq called-channel channel
                             called-args args)
                       (funcall callback
                                `((type . "unused-assets-trash")
                                  (ok . t)
                                  (trashed . [,asset])
                                  (skipped . [])
                                  (assets . [])))))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/noema-roam--trash-orphaned-assets (list asset))
            (should (equal called-channel
                           "aaronnote:api:assets:trash-orphans"))
            (should (equal (aref called-args 0) (list asset-file)))
            (with-current-buffer "*roam-orphaned-assets*"
              (should (string-match-p "No orphaned attachments"
                                      (buffer-string)))))
        (when-let* ((buffer (get-buffer "*roam-orphaned-assets*")))
          (kill-buffer buffer))))))

	
(ert-deftest my/noema-roam-agenda-routes-native-default-and-specialized-web-views ()
  (let (calls)
    (cl-letf (((symbol-function 'my/noema-roam--open-web-agenda)
               (lambda (&optional view query)
                 (push (list view query) calls)))
              ((symbol-function 'my/noema-agenda)
               (lambda (&optional query) (push (list 'native query) calls))))
      (my/noema-roam-agenda)
      (my/noema-roam-agenda 'calendar "today")
      (my/noema-roam-agenda 'clock nil))
    (should (equal (nreverse calls)
                   '((native nil) (calendar "today") (clocktable nil))))))

(ert-deftest my/noema-roam-agenda-special-pages-use-web-host ()
  (let (views)
    (cl-letf (((symbol-function 'my/noema-roam--open-web-agenda)
               (lambda (&optional view _query) (push view views))))
      (my/noema-roam-agenda-calendar)
      (my/noema-roam-agenda-log)
      (my/noema-roam-agenda-gantt)
      (my/noema-roam-agenda-projects)
      (my/noema-roam-agenda-clock)
      (my/noema-roam-agenda-lints))
    (should (equal (nreverse views)
                   '(calendar log gantt projects clocktable lints)))))

(ert-deftest my/noema-roam-web-agenda-opens-with-explicit-current-scopes ()
  "The Web page receives Roam plus the entered project, never implicit Any."
  (let (opened)
    (let ((my/project-active-root "/work/demo/"))
      (cl-letf (((symbol-function 'noema-agenda-activate-project)
                 (lambda (root &optional callback)
                   (should (equal root "/work/demo/"))
                   (funcall callback '((id . "project:demo")) nil)))
                ((symbol-function 'my/noema--ensure-server)
                 (lambda (&optional callback) (when callback (funcall callback))))
                ((symbol-function 'my/noema--server-url)
                 (lambda (&optional path) (concat "http://127.0.0.1:7777" path)))
                ((symbol-function 'my/noema--open-url)
                 (lambda (url &rest _) (setq opened url))))
        (my/noema-roam--open-web-agenda 'gantt "proof")
        (should (string-match-p "view=gantt" opened))
        (should (string-match-p "scope=knowledge" opened))
        (should (string-match-p "scope=project%3Ademo" opened))
        (should (string-match-p "q=proof" opened))))))

(ert-deftest my/noema-roam-todo-metadata-update-sends-native-priority-only ()
  (let ((todo '(:file "/tmp/task.md" :scopeId "knowledge" :uid "task-1"
               :sourceRef ((revision . "r")) :text "Review"))
        captured)
    (cl-letf (((symbol-function 'noema-agenda--patch)
               (lambda (entry patch _callback) (setq captured (list entry patch)))))
      (my/noema-roam-update-todo-metadata "priority" "B" todo)
      (should (equal captured (list todo '((prio . "B"))))))))

(ert-deftest my/noema-roam-todos-empty-result-does-not-start-a-fallback ()
  (let (operation called)
    (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
               (lambda (op _body callback)
                 (setq operation op)
                 (funcall callback '((todos . [])) nil)))
              ((symbol-function 'my/noema-roam--runtime-call)
               (lambda (&rest _) (ert-fail "CLI fallback")))
              ((symbol-function 'my/noema-roam--note-records)
               (lambda () (ert-fail "vault scan"))))
      (my/noema-roam--todos (lambda (todos) (setq called t) (should-not todos)))
      (should called)
      (should (equal operation "query-active")))))

(ert-deftest my/noema-roam-todos-failure-preserves-the-existing-view ()
  (let (reported)
    (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
               (lambda (_op _body callback) (funcall callback nil '((message . "Disconnected")))))
              ((symbol-function 'noema-agenda--error) (lambda (err) (setq reported err)))
              ((symbol-function 'my/noema-roam--runtime-call)
               (lambda (&rest _) (ert-fail "CLI fallback"))))
      (my/noema-roam--todos (lambda (_todos) (ert-fail "replaced view after failure")))
      (should reported))))

(ert-deftest my/noema-roam-current-file-todos-use-unsaved-snapshot-and-utf16 ()
  (with-temp-buffer
    (setq buffer-file-name "/fs:unentered:/research/tasks.md")
    (insert "😀\n@@todo(doing) [write proof]{ddl=2026-06-07}\n\n@@itodo [review]")
    (let ((source (buffer-string)) (tick (buffer-chars-modified-tick)) captured)
      (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
                 (lambda (op body callback)
                   (should (equal op "document"))
                   (setq captured body)
                   (funcall callback
                            '((todos . [((index . 3) (line . 2) (source . "@@todo(doing) [write proof]{ddl=2026-06-07}")
                                         (text . "write proof") (status . "doing") (canon . ((ddl . "2026-06-07"))))])) nil)))
                ((symbol-function 'my/noema-roam--todos) (lambda (&rest _) (ert-fail "vault query")))
                ((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "source IO"))))
        (narrow-to-region 3 12)
        (my/noema-roam--current-file-todos
         (lambda (todos)
           (should (= (length todos) 1))
           (should (= (my/noema-roam--todo-field (car todos) "bufferPoint") 3))
           (should (equal (my/noema-roam--todo-status (car todos)) "doing"))
           (should (equal (my/noema-roam--todo-agenda-date (car todos)) "2026-06-07"))))
        (should (equal (alist-get 'content captured) source))
        (should (equal (alist-get 'file captured) buffer-file-name))
        (should (= tick (buffer-chars-modified-tick)))
        (should (buffer-narrowed-p))))))

(ert-deftest my/noema-roam-file-todos-ignore-stale-and-superseded-replies ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/unsaved.md")
    (insert "@@todo [Live]")
    (let (callbacks delivered)
      (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
                 (lambda (_op _body callback) (push callback callbacks))))
        (my/noema-roam--current-buffer-todos (lambda (_) (ert-fail "superseded result")))
        (my/noema-roam--current-buffer-todos (lambda (_) (setq delivered t)))
        (funcall (cadr callbacks) '((todos . [])) nil)
        (funcall (car callbacks) '((todos . [])) nil)
        (should delivered)
        (my/noema-roam--current-buffer-todos (lambda (_) (ert-fail "stale source")))
        (insert " edit")
        (funcall (car callbacks) '((todos . [])) nil)))))

(ert-deftest my/noema-roam-file-todos-do-not-touch-a-killed-buffer ()
  (let ((buffer (generate-new-buffer " *file tasks*")) callback)
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
                   (lambda (_op _body cb) (setq callback cb))))
          (with-current-buffer buffer
            (setq buffer-file-name "/tmp/unsaved.md")
            (my/noema-roam--current-buffer-todos (lambda (_) (ert-fail "closed buffer"))))
          (kill-buffer buffer)
          (funcall callback '((todos . [])) nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-roam-file-tasks-reject-work-document-output-scanning ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/work.noema")
    (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
               (lambda (&rest _) (ert-fail "parsed AI outputs as Markdown"))))
      (should-error (my/noema-roam--current-buffer-todos #'ignore) :type 'user-error))))

(ert-deftest my/noema-roam-todo-write-uses-scoped-owner-and-never-falls-back ()
  (with-temp-buffer
    (let ((entry '((uid . "u") (scopeId . "project:p") (sourceRef . ((revision . "r")))))
          captured callback refreshed)
      (cl-letf (((symbol-function 'noema-agenda--patch)
                 (lambda (todo patch cb) (setq captured (list todo patch) callback cb)))
                ((symbol-function 'my/noema-roam--runtime-call)
                 (lambda (&rest _) (ert-fail "unscoped writer")))
                ((symbol-function 'my/noema-roam-ui-refresh) (lambda () (setq refreshed t))))
        (my/noema-roam-update-todo-status "done" entry)
        (should (equal captured (list entry '((op . "complete")))))
        (funcall callback nil '((message . "Source changed")))
        (should-not refreshed)
        (my/noema-roam-update-todo-metadata "due" "2026-09-20" entry)
        (should (equal captured (list entry '((ddl . "2026-09-20")))))
        (funcall callback '((changed . t)) nil)
        (should refreshed)))))

(ert-deftest my/noema-roam-file-task-navigation-keeps-exact-duplicate-position ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/unsaved.md")
    (insert "😀\n@@todo [Same]\n@@todo [Same]")
    (let (entries)
      (cl-letf (((symbol-function 'my/noema-roam--agenda-call)
                 (lambda (_op _body callback)
                   (funcall callback '((todos . [((index . 3) (source . "@@todo [Same]"))
                                                ((index . 17) (source . "@@todo [Same]"))])) nil)))
                ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer)))
                ((symbol-function 'recenter) #'ignore))
        (my/noema-roam--current-buffer-todos (lambda (todos) (setq entries todos)))
        (my/noema-roam--visit-todo (cadr entries))
        (should (= (line-number-at-pos) 3))
        (should (= (point) 17))
        (insert "modified ")
        (should-error (my/noema-roam--visit-todo (car entries)) :type 'user-error)))))

(ert-deftest my/noema-roam-file-task-reply-does-not-steal-another-buffer ()
  (let ((origin (generate-new-buffer " *task origin*")) callback)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'my/noema-roam--current-file-todos)
                     (lambda (cb) (setq callback cb)))
                    ((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "stole focus"))))
            (my/noema-roam-jump-file-todo)
            (switch-to-buffer (get-buffer-create "*scratch*"))
            (funcall callback nil)))
      (kill-buffer origin))))

(ert-deftest my/noema-roam-task-rows-use-native-identities-for-identical-titles ()
  (let (ids)
    (cl-letf (((symbol-function 'my/noema-roam-ui-insert-row)
               (lambda (&rest props) (push (plist-get props :id) ids))))
      (my/noema-roam--insert-todo-row '(:uid "wn-one" :note "work" :text "Same" :status "todo"))
      (my/noema-roam--insert-todo-row '(:uid "wn-two" :note "work" :text "Same" :status "todo"))
      (should (equal (nreverse ids) '("wn-one" "wn-two"))))))

(ert-deftest my/noema-roam-search-view-refreshes-results ()
  (my/noema-roam-test-with-vault
    (let ((first '(:slug "first" :title "First result" :path "first.md"))
          (second '(:slug "second" :title "Second result" :path "second.md"))
          (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer))
                    ((symbol-function 'my/noema-roam-search-notes)
                     (lambda (_query)
                       (setq calls (1+ calls))
                       (list second))))
            (my/noema-roam--show-search-results "math" (list first))
            (with-current-buffer "*Noema roam notes*"
              (should (string-match-p "First result" (buffer-string)))
              (my/noema-roam-ui-refresh)
              (should (= calls 1))
              (should (string-match-p "Second result" (buffer-string)))
              (should-not (string-match-p "First result" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*Noema roam notes*")))
          (kill-buffer buffer))))))

(ert-deftest my/noema-roam-selector-row-uses-shared-action-property ()
  (my/noema-roam-test-with-vault
    (with-temp-buffer
      (my/noema-roam-select-mode)
      (my/noema-roam-select--render-root "")
      (goto-char (point-min))
      (search-forward "demo/")
      (goto-char (match-beginning 0))
      (should (derived-mode-p 'my/noema-roam-ui-mode))
      (should (get-text-property
               (point) 'my/noema-roam-ui-row-action))
      (should-not (button-at (point)))
      (let (opened)
        (cl-letf (((symbol-function 'my/noema-roam-select--render-root)
                   (lambda (path) (setq opened path))))
          (my/noema-roam-ui-activate))
        (should (equal opened "demo/"))))))

(ert-deftest my/noema-roam-new-opens-single-node-draft ()
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (let ((response (make-hash-table :test 'equal))
          (template (make-hash-table :test 'equal))
          displayed)
      (puthash "key" "roam" template)
      (puthash "name" "Roam note" template)
      (puthash "templates" (list template) response)
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                     (lambda (&rest _args) response))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args)
                       (setq displayed buffer)
                       buffer)))
            (my/noema-roam-new "demo")
            (should (eq displayed (get-buffer "*roam-new-node*")))
            (with-current-buffer "*roam-new-node*"
              (should (derived-mode-p 'my/noema-roam-new-mode))
              (should (string-match-p "New Markdown" (buffer-string)))
              (should (equal (plist-get my/noema-roam-new--draft :path)
                             "demo/untitled.md"))
              (should (equal (plist-get
                              my/noema-roam-new--draft :template-key)
                             "roam"))
              (should (assoc 'tags my/noema-roam-new--widgets))
              (should (equal (widget-value
                              (alist-get
                               'tags my/noema-roam-new--widgets))
                             ""))
              (should-not (string-match-p "TAGS[[:space:]\n]+None"
                                           (buffer-string)))
              (dolist (label '("CREATE AS" "TITLE" "SAVE PATH"
                               "RENDER KIND" "TEMPLATE" "TAGS"))
                (should (string-match-p label (buffer-string))))))
        (when-let* ((buffer (get-buffer "*roam-new-node*")))
          (kill-buffer buffer))))))

(ert-deftest my/noema-roam-new-reopen-preserves-active-draft ()
  "Repeating the same entry must focus the draft without clearing widgets."
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (let ((calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                     (lambda (&rest _args)
                       (cl-incf calls)
                       (let ((response (make-hash-table :test 'equal)))
                         (puthash "templates"
                                  '((:key "roam" :nodeType "roam")) response)
                         response)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/noema-roam-new "demo" nil "roam")
            (with-current-buffer "*roam-new-node*"
              (widget-value-set
               (alist-get 'title my/noema-roam-new--widgets)
               "Keep this title"))
            (my/noema-roam-new "demo" nil "roam")
            (with-current-buffer "*roam-new-node*"
              (should (equal (widget-value
                              (alist-get 'title my/noema-roam-new--widgets))
                             "Keep this title"))
              (should (equal my/noema-roam-new--base-directory "demo")))
            (should (= calls 1)))
        (when-let* ((buffer (get-buffer "*roam-new-node*")))
          (kill-buffer buffer))))))

(ert-deftest my/noema-roam-new-keeps-separate-frame-drafts ()
  (my/noema-roam-test-with-vault
    (let ((views (make-hash-table :test #'eq))
          (active-frame 'frame-a)
          (template-loads 0)
          first second)
      (cl-letf (((symbol-function 'selected-frame)
                 (lambda () active-frame))
                ((symbol-function 'frame-live-p)
                 (lambda (_frame) t))
                ((symbol-function 'frame-parameter)
                 (lambda (frame _key) (gethash frame views)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame _key value) (puthash frame value views)))
                ((symbol-function 'my/noema-roam--runtime-call)
                 (lambda (&rest _)
                   (cl-incf template-loads)
                   (let ((response (make-hash-table :test 'equal)))
                     (puthash "templates"
                              '((:key "roam" :nodeType "roam")) response)
                     response)))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _) buffer)))
        (unwind-protect
            (progn
              (setq first (my/noema-roam-new "demo" nil "roam"))
              (with-current-buffer first
                (setq-local my/noema-roam-new--draft
                            (plist-put my/noema-roam-new--draft
                                       :title "Frame A draft")))
              (setq active-frame 'frame-b
                    second (my/noema-roam-new "demo" nil "roam"))
              (should-not (eq first second))
              (with-current-buffer second
                (should-not (equal (plist-get my/noema-roam-new--draft :title)
                                   "Frame A draft")))
              (setq active-frame 'frame-a)
              (should (eq first (my/noema-roam-new "demo" nil "roam")))
              (with-current-buffer first
                (should (equal (plist-get my/noema-roam-new--draft :title)
                               "Frame A draft")))
              (should (= template-loads 2))
              (my/noema-roam--delete-frame-ui-h 'frame-b)
              (should (buffer-live-p first))
              (should-not (buffer-live-p second)))
          (dolist (buffer (list first second))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest my/noema-roam-new-q-restores-origin-and-releases-form ()
  "Cancelling the creation form follows the native board return path."
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (let ((origin (generate-new-buffer " *roam-new-origin*"))
          form)
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer origin)
            (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                       (lambda (&rest _args)
                         (let ((response (make-hash-table :test 'equal)))
                           (puthash "templates"
                                    '((:key "roam" :nodeType "roam")) response)
                           response))))
              (my/noema-roam-new "demo" nil "roam"))
            (setq form (get-buffer "*roam-new-node*"))
            (should (eq (window-buffer) form))
            (with-current-buffer form
              (call-interactively
               (lookup-key my/noema-roam-new-mode-map (kbd "q"))))
            (should (eq (window-buffer) origin))
            (should-not (buffer-live-p form)))
        (when (buffer-live-p form) (kill-buffer form))
        (kill-buffer origin)))))

(ert-deftest my/noema-markdown-new-document-starts-plain-in-current-folder ()
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                   (lambda (&rest _args)
                     (let ((response (make-hash-table :test 'equal)))
                       (puthash "templates"
                                '((:key "roam" :nodeType "roam")
                                  (:key "basic" :nodeType "regular"))
                                response)
                       response)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args) buffer)))
          (let ((buffer-file-name (expand-file-name "demo/source.md" root)))
            (markdown-new-document))
          (with-current-buffer "*roam-new-node*"
            (should (equal (plist-get my/noema-roam-new--draft :node-type)
                           "regular"))
            (should (equal (plist-get my/noema-roam-new--draft :kind)
                           "default"))
            (should (equal (plist-get my/noema-roam-new--draft :template-key)
                           "basic"))
            (should (equal (plist-get my/noema-roam-new--draft :path)
                           "demo/untitled.md"))
            (should (string-match-p "Regular Markdown · plain"
                                    (buffer-string)))))
      (when-let* ((buffer (get-buffer "*roam-new-node*")))
        (kill-buffer buffer)))))

(ert-deftest my/noema-markdown-new-document-reset-keeps-plain-type ()
  "Resetting a regular Markdown draft must not silently make a Roam node."
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                   (lambda (&rest _args)
                     (let ((response (make-hash-table :test 'equal)))
                       (puthash "templates"
                                '((:key "roam" :nodeType "roam")
                                  (:key "basic" :nodeType "regular"))
                                response)
                       response)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args) buffer)))
          (markdown-new-document "demo")
          (with-current-buffer "*roam-new-node*"
            (widget-value-set
             (alist-get 'title my/noema-roam-new--widgets) "Draft title")
            (my/noema-roam-new-reset)
            (should (equal (plist-get my/noema-roam-new--draft :node-type)
                           "regular"))
            (should (equal (plist-get my/noema-roam-new--draft :kind)
                           "default"))
            (should (equal (plist-get my/noema-roam-new--draft :template-key)
                           "basic"))
            (should (equal (plist-get my/noema-roam-new--draft :title)
                           "Untitled"))))
      (when-let* ((buffer (get-buffer "*roam-new-node*")))
        (kill-buffer buffer)))))

(ert-deftest my/noema-roam-new-refresh-keeps-edits-and-validates-template ()
  "Refreshing the catalogue must keep typed fields and reject a stale key."
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (let ((calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                     (lambda (&rest _args)
                       (cl-incf calls)
                       (let ((response (make-hash-table :test 'equal)))
                         (puthash "templates"
                                  (if (= calls 1)
                                      '((:key "basic" :nodeType "regular"))
                                    '((:key "report" :nodeType "regular")))
                                  response)
                         response)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (markdown-new-document "demo")
            (with-current-buffer "*roam-new-node*"
              (widget-value-set
               (alist-get 'title my/noema-roam-new--widgets) "Keep this")
              (my/noema-roam-new-refresh)
              (should (equal (plist-get my/noema-roam-new--draft :title)
                             "Keep this"))
              (should (equal (plist-get my/noema-roam-new--draft :node-type)
                             "regular"))
              (should (equal (plist-get my/noema-roam-new--draft :template-key)
                             "")))
            (should (= calls 2)))
        (when-let* ((buffer (get-buffer "*roam-new-node*")))
          (kill-buffer buffer))))))

(ert-deftest my/noema-roam-new-note-alias-opens-the-workbench ()
  (my/noema-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new-node*")))
      (kill-buffer buffer))
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                   (lambda (&rest _args)
                     (let ((response (make-hash-table :test 'equal)))
                       (puthash "templates"
                                '((:key "roam" :name "Linked" :nodeType "roam"))
                                response)
                       response)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args) buffer)))
          (call-interactively #'roam-new-note)
          (with-current-buffer "*roam-new-node*"
            (should (derived-mode-p 'my/noema-roam-new-mode))
            (should (equal (plist-get my/noema-roam-new--draft :template-key)
                           "roam"))))
      (when-let* ((buffer (get-buffer "*roam-new-node*")))
        (kill-buffer buffer)))))

(ert-deftest my/noema-roam-runtime-index-uses-roam-only-api ()
  (let (called-channel)
    (cl-letf (((symbol-function 'my/noema--api-call-sync)
               (lambda (channel _args &optional _timeout)
                 (setq called-channel channel)
                 (let ((response (make-hash-table :test 'equal)))
                   (puthash "notes" [] response)
                   response))))
      (my/noema-roam--runtime-call-via-api "index" nil))
    (should (equal called-channel "aaronnote:api:notes:roam-index"))))

(ert-deftest my/noema-roam-runtime-create-api-uses-string-keyed-hash-payload ()
  (let ((draft-json
         (json-encode
          '((nodeType . "roam")
            (title . "Group")
            (path . "public/Math/Algebra/group.md")
            (tags . ["algebra" "qc"]))))
        captured)
    (cl-letf (((symbol-function 'my/noema--api-call-sync)
               (lambda (channel args &optional _timeout)
                 (setq captured (list channel args))
                 (let ((response (make-hash-table :test #'equal)))
                   (puthash "file" "/tmp/group.md" response)
                   response))))
      (my/noema-roam--runtime-call-via-api
       "create" (list "--json" draft-json)))
    (should (equal (car captured)
                   "aaronnote:api:notes:create-node"))
    (let ((body (aref (cadr captured) 0)))
      (should (equal (gethash "tags" body) ["algebra" "qc"])))))

(ert-deftest my/noema-roam-new-editable-fields-sync-draft ()
  (with-temp-buffer
    (my/noema-roam-new-mode)
    (setq-local my/noema-roam-new--base-directory "projects"
                my/noema-roam-new--templates
                '((:key "roam" :name "Roam note"))
                my/noema-roam-new--draft
                (my/noema-roam-new--default-draft "projects"))
    (my/noema-roam-new-render)
    (widget-value-set (alist-get 'title my/noema-roam-new--widgets)
                      "Direct Title")
    (widget-value-set (alist-get 'path my/noema-roam-new--widgets)
                      "projects/direct-title.md")
    (widget-value-set (alist-get 'kind my/noema-roam-new--widgets)
                      "theorem")
    (widget-value-set (alist-get 'tags my/noema-roam-new--widgets)
                      "work, math")
    (my/noema-roam-new--sync-draft-from-widgets)
    (should (equal (plist-get my/noema-roam-new--draft :title)
                   "Direct Title"))
    (should (equal (plist-get my/noema-roam-new--draft :path)
                   "projects/direct-title.md"))
    (should (equal (plist-get my/noema-roam-new--draft :kind)
                   "theorem"))
    (should (equal (plist-get my/noema-roam-new--draft :tags)
                   '("math" "work")))))

(ert-deftest my/noema-roam-new-title-updates-default-path ()
  (with-temp-buffer
    (my/noema-roam-new-mode)
    (setq-local my/noema-roam-new--base-directory "projects"
                my/noema-roam-new--templates
                '((:key "roam" :name "Roam note"))
                my/noema-roam-new--draft
                (my/noema-roam-new--default-draft "projects"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "Project Atlas")))
      (my/noema-roam-new-edit-title))
    (should (equal (plist-get my/noema-roam-new--draft :title)
                   "Project Atlas"))
    (should (equal (plist-get my/noema-roam-new--draft :path)
                   "projects/project-atlas.md"))))

(ert-deftest my/noema-roam-new-edit-path-chooses-directory ()
  (my/noema-roam-test-with-vault
    (make-directory (expand-file-name "archive" root) t)
    (with-temp-buffer
      (my/noema-roam-new-mode)
      (setq-local my/noema-roam-new--base-directory "projects"
                  my/noema-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/noema-roam-new--draft
                  (my/noema-roam-new--default-draft "projects"))
      (my/noema-roam-new-render)
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _args) (expand-file-name "archive" root))))
        (my/noema-roam-new-edit-path))
      (should (equal (plist-get my/noema-roam-new--draft :path)
                     "archive/untitled.md"))
      (should (equal (widget-value
                      (alist-get 'path my/noema-roam-new--widgets))
                     "archive/untitled.md")))))

(ert-deftest my/noema-roam-new-edit-tags-updates-field ()
  (my/noema-roam-test-with-vault
    (with-temp-buffer
      (my/noema-roam-new-mode)
      (setq-local my/noema-roam-new--base-directory "projects"
                  my/noema-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/noema-roam-new--draft
                  (my/noema-roam-new--default-draft "projects"))
      (my/noema-roam-new-render)
      (let ((answers '("math" "logic" "")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _args) (pop answers))))
          (my/noema-roam-new-edit-tags)))
      (should (equal (plist-get my/noema-roam-new--draft :tags)
                     '("logic" "math")))
      (should (equal (widget-value
                      (alist-get 'tags my/noema-roam-new--widgets))
                     "#logic #math")))))

(ert-deftest my/noema-roam-new-path-and-tags-labels-run-actions ()
  (my/noema-roam-test-with-vault
    (make-directory (expand-file-name "archive" root) t)
    (with-temp-buffer
      (my/noema-roam-new-mode)
      (setq-local my/noema-roam-new--base-directory "projects"
                  my/noema-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/noema-roam-new--draft
                  (my/noema-roam-new--default-draft "projects"))
      (my/noema-roam-new-render)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (search-forward "SAVE PATH")
        (let ((action (get-text-property
                       (match-beginning 0) 'aaron-ui-board--row-action)))
          (should action)
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _args) (expand-file-name "archive" root))))
            (funcall action nil))))
      (should (equal (plist-get my/noema-roam-new--draft :path)
                     "archive/untitled.md"))
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (search-forward "TAGS")
        (let ((action (get-text-property
                       (match-beginning 0) 'aaron-ui-board--row-action))
              (answers '("math" "")))
          (should action)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args) (pop answers))))
            (funcall action nil))))
      (should (equal (plist-get my/noema-roam-new--draft :tags)
                     '("math")))
      (should (equal (widget-value
                      (alist-get 'tags my/noema-roam-new--widgets))
                     "#math")))))

(ert-deftest my/noema-roam-new-default-draft-avoids-existing-untitled ()
  (my/noema-roam-test-with-vault
    (my/noema-roam-test--write-file
     (expand-file-name "projects/untitled.md" root)
     "# Untitled\n")
    (let ((draft (my/noema-roam-new--default-draft "projects")))
      (should (equal (plist-get draft :title) "Untitled"))
      (should (equal (plist-get draft :path) "projects/untitled-2.md")))))

(ert-deftest my/noema-roam-new-create-refreshes-stale-untitled-path ()
  (my/noema-roam-test-with-vault
    (my/noema-roam-test--write-file
     (expand-file-name "projects/project-atlas.md" root)
     "# Project Atlas\n")
    (let* ((my/noema-roam-new--base-directory "projects")
           (draft (my/noema-roam-new--draft-for-create
                   '(:node-type "roam"
                     :title "Project Atlas"
                     :path "projects/untitled.md"
                     :kind "note"
                     :template-key "roam"
                     :tags nil))))
      (should (equal (plist-get draft :path)
                     "projects/project-atlas-2.md")))))

(ert-deftest my/noema-roam-new-create-defaults-match-aaronnote ()
  (my/noema-roam-test-with-vault
    (let ((my/noema-roam-new--base-directory "projects"))
      (let ((draft
             (my/noema-roam-new--draft-for-create
              '(:node-type "regular"
                :title " "
                :path ""
                :kind ""
                :template-key ""
                :tags (" work " "" "work")))))
        (should (equal (plist-get draft :node-type) "regular"))
        (should (equal (plist-get draft :title) "Untitled"))
        (should (equal (plist-get draft :path) "projects/untitled.md"))
        (should (equal (plist-get draft :kind) "default"))
        (should (equal (plist-get draft :tags) '("work")))))))

(ert-deftest my/noema-roam-new-create-uses-aaronnote-runtime-draft ()
  (my/noema-roam-test-with-vault
    (let* ((draft '(:node-type "roam"
                    :title "Project Atlas"
                    :path "projects/project-atlas.md"
                    :kind "note"
                    :template-key "project"
                    :tags ("work" "planning")))
           (created-file (expand-file-name "projects/project-atlas.md" root))
           captured)
      (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                 (lambda (action &rest args)
                   (setq captured
                         (list action
                               (json-parse-string
                                (cadr args)
                                :object-type 'hash-table
                                :array-type 'list)))
                   (my/noema-roam-test--write-file
                    created-file "# Project Atlas\n")
                   (let ((response (make-hash-table :test 'equal)))
                     (puthash "file" created-file response)
                     response))))
        (should (equal (my/noema-roam-new--create-draft draft)
                       created-file)))
      (should (eq (current-buffer) (find-buffer-visiting created-file)))
      (should (equal (car captured) "create"))
      (let ((payload (cadr captured)))
        (should (equal (gethash "nodeType" payload) "roam"))
        (should (equal (gethash "path" payload)
                       "projects/project-atlas.md"))
        (should (equal (gethash "templateKey" payload) "project"))
        (should (equal (gethash "tags" payload) '("planning" "work"))))
      (kill-buffer (find-buffer-visiting created-file)))))

(ert-deftest my/noema-roam-new-tags-use-runtime-canonical-form ()
  (should
   (equal (my/noema-roam-new--normalize-tags
           ["#Work" "math" "work" "TCS" "tcs"])
          '("math" "tcs" "work")))
  (should
   (equal (my/noema-roam-new--normalize-tags
           "#Work, math  work #TCS")
          '("math" "TCS" "work")))
  (should
   (equal (my/noema-roam-new--tag-display '("work" "math"))
          "#math #work")))

(ert-deftest my/noema-roam-exposes-one-node-creation-entry ()
  (should (commandp #'my/noema-roam-new-node))
  (should-not (commandp #'my/noema-roam-new))
  (should-not (fboundp 'my/noema-roam-new-note))
  (should (eq (lookup-key my/noema-roam-map (kbd "n"))
              #'my/noema-roam-new-node))
  (should-not (lookup-key my/noema-roam-map (kbd "N")))
  (should (= (seq-count
              (lambda (tool) (eq (plist-get tool :id) 'create-node))
              my/noema-roam--dashboard-tools)
             1))
  (should-not
   (seq-find (lambda (tool) (eq (plist-get tool :id) 'create-note))
             my/noema-roam--dashboard-tools)))

(ert-deftest my/noema-roam-new-create-button-syncs-roam-tags-to-runtime ()
  (my/noema-roam-test-with-vault
    (let* ((buffer (generate-new-buffer " *roam-node-click-test*"))
           (created-file (expand-file-name "projects/direct-click.md" root))
           captured)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (my/noema-roam-new-mode)
              (setq-local my/noema-roam-new--base-directory "projects"
                          my/noema-roam-new--templates
                          '((:key "roam" :name "Roam note"))
                          my/noema-roam-new--draft
                          (my/noema-roam-new--default-draft "projects"))
              (my/noema-roam-new-render)
              (widget-value-set (alist-get 'title my/noema-roam-new--widgets)
                                "Direct Click")
              (widget-value-set (alist-get 'path my/noema-roam-new--widgets)
                                "projects/direct-click.md")
              (widget-value-set (alist-get 'tags my/noema-roam-new--widgets)
                                "#Work, math work")
              (goto-char (point-min))
              (search-forward "c Create")
              (goto-char (match-beginning 0))
              (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                         (lambda (action &rest args)
                           (setq captured
                                 (list action
                                       (json-parse-string
                                        (cadr args)
                                        :object-type 'hash-table
                                        :array-type 'list)))
                           (my/noema-roam-test--write-file
                            created-file "# Direct Click\n")
                           (let ((response (make-hash-table :test 'equal)))
                             (puthash "file" created-file response)
                             response))))
                (push-button)))
            (should-not (buffer-live-p buffer))
            (should (buffer-live-p (find-buffer-visiting created-file)))
            (should (equal (car captured) "create"))
            (should (equal (gethash "tags" (cadr captured))
                           '("math" "work"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when-let* ((opened (find-buffer-visiting created-file)))
          (kill-buffer opened))))))

(ert-deftest my/noema-roam-new-open-failure-retries-created-file ()
  "A failed visit must not run create-node a second time."
  (my/noema-roam-test-with-vault
    (let* ((form (generate-new-buffer " *roam-create-retry*"))
           (created-file (expand-file-name "projects/retry.md" root))
           (real-find-file (symbol-function 'find-file))
           (creates 0)
           (visits 0))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer form
              (my/noema-roam-new-mode)
              (setq-local my/noema-roam-new--base-directory "projects"
                          my/noema-roam-new--templates
                          '((:key "roam" :name "Roam note"))
                          my/noema-roam-new--draft
                          '(:node-type "roam" :title "Retry"
                            :path "projects/retry.md" :kind "note"
                            :template-key "roam" :tags nil))
              (my/noema-roam-new-render))
            (switch-to-buffer form)
            (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                       (lambda (&rest _args)
                         (cl-incf creates)
                         (my/noema-roam-test--write-file
                          created-file "# Retry\n")
                         (let ((response (make-hash-table :test 'equal)))
                           (puthash "file" created-file response)
                           response)))
                      ((symbol-function 'find-file)
                       (lambda (file)
                         (cl-incf visits)
                         (if (= visits 1)
                             (error "Visit failed")
                           (funcall real-find-file file)))))
              (should-error (my/noema-roam-new-create))
              (should (eq (window-buffer) form))
              (should (equal (buffer-local-value
                              'my/noema-roam-new--created-file form)
                             created-file))
              (should (string-match-p "c Open file"
                                      (with-current-buffer form
                                        (buffer-string))))
              (with-current-buffer form
                (should-error (my/noema-roam-new-edit-type)
                              :type 'user-error)
                (my/noema-roam-new-create))
              (should-not (buffer-live-p form))
              (should (= creates 1))
              (should (= visits 2))
              (should (eq (window-buffer)
                          (find-buffer-visiting created-file)))))
        (when (buffer-live-p form) (kill-buffer form))
        (when-let* ((opened (find-buffer-visiting created-file)))
          (kill-buffer opened))))))

(ert-deftest my/noema-roam-new-cold-noema-open-keeps-placeholder ()
  "Creating a note must leave the redirected file visible until Noema starts."
  (my/noema-roam-test-with-vault
    (let* ((form (generate-new-buffer " *roam-cold-create*"))
           (file (expand-file-name "projects/cold.md" root))
           (my/noema-markdown-default-surface 'app)
           (my/noema-close-emacs-markdown-buffer nil)
           (my/noema--ready nil)
           (my/noema--port 4242)
           (noninteractive nil)
           callback placeholder result)
      (unwind-protect
          (save-window-excursion
            (with-current-buffer form
              (my/noema-roam-new-mode)
              (setq-local my/noema-roam-new--base-directory "projects"
                          my/noema-roam-new--templates
                          '((:key "roam" :name "Roam note"))
                          my/noema-roam-new--draft
                          '(:node-type "roam" :title "Cold"
                            :path "projects/cold.md" :kind "note"
                            :template-key "roam" :tags nil))
              (my/noema-roam-new-render))
            (switch-to-buffer form)
            (cl-letf (((symbol-function 'my/noema-roam--runtime-call)
                       (lambda (&rest _args)
                         (my/noema-roam-test--write-file file "# Cold\n")
                         (let ((response (make-hash-table :test 'equal)))
                           (puthash "file" file response)
                           response)))
                      ((symbol-function 'my/noema--ensure-server)
                       (lambda (fn) (setq callback fn)))
                      ((symbol-function 'my/noema--open-url)
                       (lambda (&rest _args)
                         (setq result (generate-new-buffer
                                       " *roam-cold-opened*"))
                         (switch-to-buffer result)
                         result)))
              (let ((this-command 'my/noema-roam-new-create))
                (my/noema-roam-new-create))
              (setq placeholder (window-buffer))
              (should-not (buffer-live-p form))
              (should (buffer-local-value
                       'my/noema--markdown-redirected placeholder))
              (with-current-buffer placeholder
                (my/noema--markdown-placeholder-cancel-timer)
                (my/noema--markdown-placeholder-open
                 placeholder my/noema--markdown-open-generation))
              (should (functionp callback))
              (funcall callback)
              (should (eq (window-buffer) result))))
        (dolist (buffer (list form placeholder result))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest my/noema-markdown-warm-host-opens-after-native-visit ()
  "A ready host must not have its page covered by the file placeholder."
  (let* ((file (make-temp-file "noema-warm-markdown" nil ".md"))
         (origin (generate-new-buffer " *noema-warm-origin*"))
         (my/noema-markdown-default-surface 'app)
         (my/noema-close-emacs-markdown-buffer t)
         (my/noema--port 4242)
         (noninteractive nil)
         (opens 0)
         placeholder result)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'my/noema--ensure-server)
                     (lambda (fn) (funcall fn)))
                    ((symbol-function 'my/noema--open-url)
                     (lambda (&rest _args)
                       (cl-incf opens)
                       (setq result (generate-new-buffer
                                     " *noema-warm-result*"))
                       (switch-to-buffer result)
                       result)))
            (let ((this-command 'find-file))
              (find-file file))
            (setq placeholder (window-buffer))
            (should (buffer-local-value
                     'my/noema--markdown-redirected placeholder))
            (with-current-buffer placeholder
              (should (derived-mode-p
                       'my/noema-markdown-placeholder-mode))
              (should (eq (key-binding (kbd "g"))
                          #'my/noema-markdown-placeholder-retry))
              (should (eq (key-binding (kbd "e"))
                          #'markdown-edit-raw))
              (should (eq (key-binding (kbd "q")) #'quit-window)))
            (should (commandp 'markdown-edit-raw))
            (should (= opens 0))
            (with-current-buffer placeholder
              (my/noema--markdown-placeholder-cancel-timer)
              (my/noema--markdown-placeholder-open
               placeholder my/noema--markdown-open-generation))
            (should (= opens 1))
            (should (eq (window-buffer) result))
            (sleep-for 0.05)
            (should-not (buffer-live-p placeholder))))
      (dolist (buffer (list origin placeholder result))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest my/noema-markdown-timeout-retry-and-raw-cancel-stale-open ()
  "A timed-out placeholder can retry; raw editing cancels pending opens."
  (let* ((file (make-temp-file "noema-retry-markdown" nil ".md"))
         (origin (generate-new-buffer " *noema-retry-origin*"))
         (my/noema-markdown-default-surface 'app)
         (my/noema-close-emacs-markdown-buffer nil)
         (my/noema--ready nil)
         (my/noema--port 4242)
         (my/noema--ready-callbacks nil)
         (my/noema--ready-watchdog nil)
         (noninteractive nil)
         callbacks placeholder raw
         (opens 0))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'my/noema--ensure-server)
                     (lambda (fn) (push fn callbacks)))
                    ((symbol-function 'my/noema--open-url)
                     (lambda (&rest _args) (cl-incf opens) nil)))
            (let ((this-command 'find-file))
              (find-file file))
            (setq placeholder (window-buffer))
            (with-current-buffer placeholder
              (my/noema--markdown-placeholder-cancel-timer)
              (my/noema--markdown-placeholder-open
               placeholder my/noema--markdown-open-generation))
            (should (= (length callbacks) 1))
            (my/noema--watchdog-fire)
            (should (eq (buffer-local-value
                         'my/noema--markdown-placeholder-state placeholder)
                        'failed))
            (with-current-buffer placeholder
              (call-interactively
               (lookup-key my/noema-markdown-placeholder-mode-map
                           (kbd "g")))
              (my/noema--markdown-placeholder-cancel-timer)
              (my/noema--markdown-placeholder-open
               placeholder my/noema--markdown-open-generation))
            (should (= (length callbacks) 2))
            (funcall (cadr callbacks))
            (should (= opens 0))
            (with-current-buffer placeholder
              (my/noema-open-markdown-raw file))
            (setq raw (window-buffer))
            (should-not (buffer-live-p placeholder))
            (with-current-buffer raw
              (should (derived-mode-p 'gfm-mode))
              (should-not buffer-read-only))
            (funcall (car callbacks))
            (should (= opens 0))))
      (dolist (buffer (list origin placeholder raw))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-exists-p file) (delete-file file)))))

(provide 'md-roam-tests)
;;; md-roam-tests.el ends here
