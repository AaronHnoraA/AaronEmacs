;;; md-roam-tests.el --- Unit tests for Markdown roam links -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-md-roam)
(require 'noema-agenda)

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

(ert-deftest my/noema-roam-db-status-delegates-to-canonical-wiki-status ()
  (let (called)
    (cl-letf (((symbol-function 'my/noema-wiki-index-status)
               (lambda () (setq called t))))
      (my/noema-roam-db-status))
    (should called)))

(ert-deftest my/noema-roam-management-delegates-to-canonical-wiki-ui ()
  (let (called)
    (cl-letf (((symbol-function 'my/noema-wiki-repositories)
               (lambda () (setq called t))))
      (my/noema-roam-management))
    (should called)))

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
              (should (string-match-p "New node" (buffer-string)))
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
              (dolist (label '("TYPE" "TITLE" "SAVE PATH"
                               "KIND" "TEMPLATE" "TAGS"))
                (should (string-match-p label (buffer-string))))))
        (when-let* ((buffer (get-buffer "*roam-new-node*")))
          (kill-buffer buffer))))))

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
           captured opened)
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
                     response)))
                ((symbol-function 'my/noema-open-file)
                 (lambda (file) (setq opened file))))
        (should (equal (my/noema-roam-new--create-draft draft)
                       created-file)))
      (should (equal opened created-file))
      (should (equal (car captured) "create"))
      (let ((payload (cadr captured)))
        (should (equal (gethash "nodeType" payload) "roam"))
        (should (equal (gethash "path" payload)
                       "projects/project-atlas.md"))
        (should (equal (gethash "templateKey" payload) "project"))
        (should (equal (gethash "tags" payload) '("planning" "work")))))))

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
           captured
           opened)
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
                             response)))
                        ((symbol-function 'my/noema-open-file)
                         (lambda (file) (setq opened file))))
                (push-button)))
            (should-not (buffer-live-p buffer))
            (should (equal opened created-file))
            (should (equal (car captured) "create"))
            (should (equal (gethash "tags" (cadr captured))
                           '("math" "work"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'md-roam-tests)
;;; md-roam-tests.el ends here
