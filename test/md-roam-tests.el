;;; md-roam-tests.el --- Unit tests for Markdown roam links -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-md-roam)

(defun my/aaronnote-roam-test--write-file (file content)
  "Write CONTENT to FILE, creating parents."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defmacro my/aaronnote-roam-test-with-vault (&rest body)
  "Run BODY with a temporary Markdown roam vault."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "md-roam-test-" t)))
          (my/aaronnote-roam-root root)
          (my/aaronnote--notes-root root)
          (my/aaronnote-roam--db-cache nil)
          (my/aaronnote-roam--db-path-cache nil)
          (my/aaronnote-roam--db-mtime nil)
          (my/aaronnote-roam--runtime-index-cache nil)
          (my/aaronnote-roam--runtime-index-cache-key nil)
          (my/aaronnote-roam--scan-cache nil)
          (note-file (expand-file-name "demo/topology.md" root))
          (other-file (expand-file-name "demo/analysis.md" root)))
     (unwind-protect
         (progn
           (my/aaronnote-roam-test--write-file
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
           (my/aaronnote-roam-test--write-file
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
           (setq my/aaronnote-roam--db-cache nil
                 my/aaronnote-roam--db-path-cache nil
                 my/aaronnote-roam--db-mtime nil
                 my/aaronnote-roam--runtime-index-cache nil
                 my/aaronnote-roam--runtime-index-cache-key nil
                 my/aaronnote-roam--scan-cache nil)
           ,@body)
       (delete-directory root t))))

(ert-deftest my/aaronnote-roam-parse-canonical-targets ()
  (my/aaronnote-roam-test-with-vault
    (let ((plain (my/aaronnote-roam--parse-target
                  "roam://20260605T120000-topology"))
          (tag (my/aaronnote-roam--parse-target
                "roam://20260605T120000-topology#eq-eq%3A1"))
          (dom (my/aaronnote-roam--parse-target
                "roam://20260605T120000-topology@nested@child-heading")))
      (should (equal (plist-get plain :slug) "20260605T120000-topology"))
      (should (equal (plist-get tag :id) "eq-eq:1"))
      (should (equal (plist-get dom :dom) "nested@child-heading"))
      (should (equal (plist-get dom :file) note-file)))))

(ert-deftest my/aaronnote-roam-resolves-path-title-alias-and-tag ()
  (my/aaronnote-roam-test-with-vault
    (dolist (ref '("demo/topology.md"
                   "demo/topology"
                   "Topology Note"
                   "topological space"
                   "topology"))
      (should (equal (plist-get (my/aaronnote-roam--resolve-note ref) :id)
                     "20260605T120000-topology")))))

(ert-deftest my/aaronnote-roam-normalizes-db-backlinks-to-note-id ()
  (my/aaronnote-roam-test-with-vault
    (should (member "20260605T120000-analysis"
                    (my/aaronnote-roam--db-backlinks-to
                     "20260605T120000-topology")))))

(ert-deftest my/aaronnote-roam-selector-builds-note-tag-and-toc-targets ()
  (my/aaronnote-roam-test-with-vault
    (let* ((record (my/aaronnote-roam--resolve-note "demo/topology.md"))
           (tags (my/aaronnote-roam--tag-targets record))
           (toc-targets (my/aaronnote-roam-select--toc-targets record))
           (child (seq-find
                   (lambda (target)
                     (equal (plist-get target :path)
                            '("main-title" "nested" "child-heading")))
                   toc-targets)))
      (should (equal (my/aaronnote-roam--link-target-for-record record 'id)
                     "roam://20260605T120000-topology"))
      (should (equal (my/aaronnote-roam--link-target-for-record record 'path)
                     "demo/topology.md"))
      (should (seq-find (lambda (target)
                          (equal (plist-get target :id) "hausdorff"))
                        tags))
      (should (equal (my/aaronnote-roam--link-target-for-record
                      record 'id 'tag "hausdorff")
                     "roam://20260605T120000-topology#hausdorff"))
      (should (equal (my/aaronnote-roam--link-target-for-record
                      record 'path 'tag "hausdorff")
                     "demo/topology.md#hausdorff"))
      (should child)
      (should (equal (my/aaronnote-roam-select--toc-dom child)
                     "main-title@nested@child-heading"))
      (should (equal (my/aaronnote-roam--link-target-for-record
                      record 'id 'dom
                      (my/aaronnote-roam-select--toc-dom child))
                     "roam://20260605T120000-topology@main-title@nested@child-heading"))
      (should (equal (my/aaronnote-roam--link-target-for-record
                      record 'path 'dom
                      (my/aaronnote-roam-select--toc-dom child))
                     "demo/topology.md@main-title@nested@child-heading")))))

(ert-deftest my/aaronnote-roam-selector-browses-root-and-toc-levels ()
  (my/aaronnote-roam-test-with-vault
    (let* ((root-items (my/aaronnote-roam-select--directory-items ""))
           (demo (seq-find (lambda (item)
                             (and (eq (plist-get item :type) 'dir)
                                  (equal (plist-get item :name) "demo")))
                           root-items))
           (demo-items (my/aaronnote-roam-select--directory-items "demo/"))
           (record (my/aaronnote-roam--resolve-note "demo/topology.md"))
           (targets (my/aaronnote-roam-select--toc-targets record))
           (top (seq-find (lambda (target)
                            (equal (plist-get target :path)
                                   '("main-title")))
                          targets))
           (children (my/aaronnote-roam-select--toc-children
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

(ert-deftest my/aaronnote-roam-selector-inserts-at-origin-marker ()
  (my/aaronnote-roam-test-with-vault
    (let* ((record (my/aaronnote-roam--resolve-note "demo/topology.md"))
           source marker)
      (with-temp-buffer
        (setq source (current-buffer)
              marker (copy-marker (point) t))
        (with-temp-buffer
          (my/aaronnote-roam-select-mode)
          (setq-local my/aaronnote-roam-select--origin-marker marker)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "Topology")))
            (my/aaronnote-roam-select--finish-target
             record 'id nil nil "Topology Note")))
        (with-current-buffer source
          (should (equal (buffer-string)
                         "[Topology](roam://20260605T120000-topology)")))))))

(ert-deftest my/aaronnote-roam-selector-opens-bottom-search-view ()
  (my/aaronnote-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*aaronnote-roam-select*")))
      (kill-buffer buffer))
    (with-temp-buffer
      (let (displayed-buffer display-alist)
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   (lambda (buffer alist)
                     (setq displayed-buffer buffer
                           display-alist alist)
                     (selected-window))))
          (my/aaronnote-roam-select-link))
        (should (eq displayed-buffer (get-buffer "*aaronnote-roam-select*")))
        (should (eq (cdr (assq 'side display-alist)) 'bottom))
        (with-current-buffer "*aaronnote-roam-select*"
          (should (eq my/aaronnote-roam-select--view 'search))
          (should (string-match-p "Topology Note"
                                  (buffer-string))))))))

(ert-deftest my/aaronnote-roam-setup-does-not-install-capf ()
  (with-temp-buffer
    (my/aaronnote-roam-setup-keys)
    (should-not (fboundp 'my/aaronnote-roam--capf))
    (should-not (memq (intern "my/aaronnote-roam--capf")
                      completion-at-point-functions))))

(ert-deftest my/aaronnote-roam-follow-link-jumps-to-tag-and-dom-target ()
  (my/aaronnote-roam-test-with-vault
    (let (opened)
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert "[Hausdorff](demo/topology.md#hausdorff)")
              (goto-char (point-min))
              (search-forward "hausdorff")
              (my/aaronnote-roam-follow-link)
              (setq opened (current-buffer))
              (should (equal (file-truename buffer-file-name)
                             (file-truename note-file)))
              (should (looking-at "{#hausdorff}")))
            (with-temp-buffer
              (insert "[Child](roam://20260605T120000-topology@nested@child-heading)")
              (goto-char (point-min))
              (search-forward "child-heading")
              (my/aaronnote-roam-follow-link)
              (setq opened (current-buffer))
              (should (equal (file-truename buffer-file-name)
                             (file-truename note-file)))
              (save-excursion
                (beginning-of-line)
                (should (looking-at "### Child Heading")))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

(ert-deftest my/aaronnote-roam-xref-locates-tag-and-dom-for-id-and-path ()
  (my/aaronnote-roam-test-with-vault
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

(ert-deftest my/aaronnote-roam-gd-jumps-to-tag-and-dom-for-id-and-path ()
  (my/aaronnote-roam-test-with-vault
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
                (my/aaronnote-roam-goto-definition)
                (setq opened (current-buffer))
                (should (equal (file-truename buffer-file-name)
                               (file-truename note-file)))
                (should (= (line-number-at-pos) line))
                (should (looking-at pattern)))))
        (when (buffer-live-p opened)
          (kill-buffer opened))))))

(ert-deftest my/aaronnote-roam-ui-row-keeps-face-and-activates ()
  (with-temp-buffer
    (my/aaronnote-roam-ui-mode)
    (let (activated)
      (my/aaronnote-roam-ui-render
       (lambda ()
         (my/aaronnote-roam-ui-insert-row
          :id "modern-row"
          :icon 'note
          :title "Modern row"
          :action (lambda (_ignored) (setq activated t)))))
      (goto-char (point-min))
      (search-forward "Modern row")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'face)
                  'my/aaronnote-roam-ui-row-title))
      (should (equal (get-text-property
                      (point) 'my/aaronnote-roam-ui-item-id)
                     "modern-row"))
      (should-not (button-at (point)))
      (my/aaronnote-roam-ui-activate)
      (should activated))))

(ert-deftest my/aaronnote-roam-ui-render-preserves-current-row ()
  (with-temp-buffer
    (my/aaronnote-roam-ui-mode)
    (let ((renderer
           (lambda ()
             (my/aaronnote-roam-ui-insert-row
              :id "first" :title "First")
             (my/aaronnote-roam-ui-insert-row
              :id "second" :title "Second"))))
      (my/aaronnote-roam-ui-render renderer)
      (goto-char (point-min))
      (search-forward "Second")
      (goto-char (match-beginning 0))
      (my/aaronnote-roam-ui-render renderer)
      (should (equal (get-text-property
                      (point) 'my/aaronnote-roam-ui-item-id)
                     "second")))))

(ert-deftest my/aaronnote-roam-native-views-use-shared-ui-mode ()
  (my/aaronnote-roam-test-with-vault
    (let* ((todo '(:note "20260605T120000-topology"
                   :title "Topology Note"
                   :text "Review compact workbench"
                   :status "doing"))
           (summary '(:slug "20260605T120000-topology"
                      :title "Topology Note"
                      :path "demo/topology.md"
                      :tags ("math" "topology")
                      :summary "A sample note"))
           (db (make-hash-table :test 'equal))
           (buffers '("*roam-todos*"
                      "*roam-agenda*"
                      "*aaronnote-roam-notes*"
                      "*aaronnote-roam-management*"
                      "*roam-db-status*")))
      (puthash "generated" "2026-06-06T00:00:00Z" db)
      (unwind-protect
          (cl-letf (((symbol-function 'my/aaronnote-roam--todos)
                     (lambda () (list todo)))
                    ((symbol-function 'my/aaronnote-roam--all-note-summaries)
                     (lambda () (list summary)))
                    ((symbol-function 'my/aaronnote-roam--db)
                     (lambda () db))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/aaronnote-roam-todos)
            (my/aaronnote-roam-agenda)
            (my/aaronnote-roam--show-note-list "Notes" (list summary))
            (my/aaronnote-roam-management)
            (my/aaronnote-roam-db-status)
            (dolist (name buffers)
              (with-current-buffer name
                (should (derived-mode-p 'my/aaronnote-roam-ui-mode))
                (should header-line-format)
                (should (functionp my/aaronnote-roam-ui-refresh-function))
                (goto-char (point-min))
                (forward-button 1)
                (should (button-at (point))))))
        (dolist (name buffers)
          (when-let* ((buffer (get-buffer name)))
            (kill-buffer buffer)))))))

(ert-deftest my/aaronnote-roam-agenda-keeps-today-out-of-overdue ()
  (my/aaronnote-roam-test-with-vault
    (let ((todo '(:note "20260605T120000-topology"
                  :title "Topology Note"
                  :text "Review compact workbench"
                  :status "todo"
                  :ddl "2026-06-06")))
      (unwind-protect
          (cl-letf (((symbol-function 'my/aaronnote-roam--todos)
                     (lambda () (list todo)))
                    ((symbol-function 'my/aaronnote-roam--todo-overdue-p)
                     (lambda (_ddl) t))
                    ((symbol-function 'format-time-string)
                     (lambda (&rest _args) "2026-06-06"))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer)))
            (my/aaronnote-roam-agenda)
            (with-current-buffer "*roam-agenda*"
              (should (string-match-p ">  Today" (buffer-string)))
              (should-not (string-match-p ">  Overdue" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*roam-agenda*")))
          (kill-buffer buffer))))))

(ert-deftest my/aaronnote-roam-search-view-refreshes-results ()
  (my/aaronnote-roam-test-with-vault
    (let ((first '(:slug "first" :title "First result" :path "first.md"))
          (second '(:slug "second" :title "Second result" :path "second.md"))
          (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &rest _args) buffer))
                    ((symbol-function 'my/aaronnote-roam-search-notes)
                     (lambda (_query)
                       (setq calls (1+ calls))
                       (list second))))
            (my/aaronnote-roam--show-search-results "math" (list first))
            (with-current-buffer "*aaronnote-roam-notes*"
              (should (string-match-p "First result" (buffer-string)))
              (my/aaronnote-roam-ui-refresh)
              (should (= calls 1))
              (should (string-match-p "Second result" (buffer-string)))
              (should-not (string-match-p "First result" (buffer-string)))))
        (when-let* ((buffer (get-buffer "*aaronnote-roam-notes*")))
          (kill-buffer buffer))))))

(ert-deftest my/aaronnote-roam-selector-row-uses-shared-action-property ()
  (my/aaronnote-roam-test-with-vault
    (with-temp-buffer
      (my/aaronnote-roam-select-mode)
      (my/aaronnote-roam-select--render-root "")
      (goto-char (point-min))
      (search-forward "demo/")
      (goto-char (match-beginning 0))
      (should (derived-mode-p 'my/aaronnote-roam-ui-mode))
      (should (get-text-property
               (point) 'my/aaronnote-roam-ui-row-action))
      (should-not (button-at (point)))
      (let (opened)
        (cl-letf (((symbol-function 'my/aaronnote-roam-select--render-root)
                   (lambda (path) (setq opened path))))
          (my/aaronnote-roam-ui-activate))
        (should (equal opened "demo/"))))))

(ert-deftest my/aaronnote-roam-new-opens-aaronnote-style-native-draft ()
  (my/aaronnote-roam-test-with-vault
    (when-let* ((buffer (get-buffer "*roam-new*")))
      (kill-buffer buffer))
    (let ((response (make-hash-table :test 'equal))
          (template (make-hash-table :test 'equal))
          displayed)
      (puthash "key" "roam" template)
      (puthash "name" "Roam note" template)
      (puthash "templates" (list template) response)
      (unwind-protect
          (cl-letf (((symbol-function 'my/aaronnote-roam--runtime-call)
                     (lambda (&rest _args) response))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args)
                       (setq displayed buffer)
                       buffer)))
            (my/aaronnote-roam-new "demo")
            (should (eq displayed (get-buffer "*roam-new*")))
            (with-current-buffer "*roam-new*"
              (should (derived-mode-p 'my/aaronnote-roam-new-mode))
              (should (equal (plist-get my/aaronnote-roam-new--draft :path)
                             "demo/untitled.md"))
              (should (equal (plist-get
                              my/aaronnote-roam-new--draft :template-key)
                             "roam"))
              (should (assoc 'tags my/aaronnote-roam-new--widgets))
              (should (equal (widget-value
                              (alist-get
                               'tags my/aaronnote-roam-new--widgets))
                             ""))
              (should-not (string-match-p "TAGS[[:space:]\n]+None"
                                           (buffer-string)))
              (dolist (label '("TYPE" "TITLE" "SAVE PATH"
                               "KIND" "TEMPLATE" "TAGS"))
                (should (string-match-p label (buffer-string))))))
        (when-let* ((buffer (get-buffer "*roam-new*")))
          (kill-buffer buffer))))))

(ert-deftest my/aaronnote-roam-new-editable-fields-sync-draft ()
  (with-temp-buffer
    (my/aaronnote-roam-new-mode)
    (setq-local my/aaronnote-roam-new--base-directory "projects"
                my/aaronnote-roam-new--templates
                '((:key "roam" :name "Roam note"))
                my/aaronnote-roam-new--draft
                (my/aaronnote-roam-new--default-draft "projects"))
    (my/aaronnote-roam-new-render)
    (widget-value-set (alist-get 'title my/aaronnote-roam-new--widgets)
                      "Direct Title")
    (widget-value-set (alist-get 'path my/aaronnote-roam-new--widgets)
                      "projects/direct-title.md")
    (widget-value-set (alist-get 'kind my/aaronnote-roam-new--widgets)
                      "theorem")
    (widget-value-set (alist-get 'tags my/aaronnote-roam-new--widgets)
                      "work, math")
    (my/aaronnote-roam-new--sync-draft-from-widgets)
    (should (equal (plist-get my/aaronnote-roam-new--draft :title)
                   "Direct Title"))
    (should (equal (plist-get my/aaronnote-roam-new--draft :path)
                   "projects/direct-title.md"))
    (should (equal (plist-get my/aaronnote-roam-new--draft :kind)
                   "theorem"))
    (should (equal (plist-get my/aaronnote-roam-new--draft :tags)
                   '("work" "math")))))

(ert-deftest my/aaronnote-roam-new-title-updates-default-path ()
  (with-temp-buffer
    (my/aaronnote-roam-new-mode)
    (setq-local my/aaronnote-roam-new--base-directory "projects"
                my/aaronnote-roam-new--templates
                '((:key "roam" :name "Roam note"))
                my/aaronnote-roam-new--draft
                (my/aaronnote-roam-new--default-draft "projects"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "Project Atlas")))
      (my/aaronnote-roam-new-edit-title))
    (should (equal (plist-get my/aaronnote-roam-new--draft :title)
                   "Project Atlas"))
    (should (equal (plist-get my/aaronnote-roam-new--draft :path)
                   "projects/project-atlas.md"))))

(ert-deftest my/aaronnote-roam-new-edit-path-chooses-directory ()
  (my/aaronnote-roam-test-with-vault
    (make-directory (expand-file-name "archive" root) t)
    (with-temp-buffer
      (my/aaronnote-roam-new-mode)
      (setq-local my/aaronnote-roam-new--base-directory "projects"
                  my/aaronnote-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/aaronnote-roam-new--draft
                  (my/aaronnote-roam-new--default-draft "projects"))
      (my/aaronnote-roam-new-render)
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _args) (expand-file-name "archive" root))))
        (my/aaronnote-roam-new-edit-path))
      (should (equal (plist-get my/aaronnote-roam-new--draft :path)
                     "archive/untitled.md"))
      (should (equal (widget-value
                      (alist-get 'path my/aaronnote-roam-new--widgets))
                     "archive/untitled.md")))))

(ert-deftest my/aaronnote-roam-new-edit-tags-updates-field ()
  (my/aaronnote-roam-test-with-vault
    (with-temp-buffer
      (my/aaronnote-roam-new-mode)
      (setq-local my/aaronnote-roam-new--base-directory "projects"
                  my/aaronnote-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/aaronnote-roam-new--draft
                  (my/aaronnote-roam-new--default-draft "projects"))
      (my/aaronnote-roam-new-render)
      (let ((answers '("math" "logic" "")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _args) (pop answers))))
          (my/aaronnote-roam-new-edit-tags)))
      (should (equal (plist-get my/aaronnote-roam-new--draft :tags)
                     '("math" "logic")))
      (should (equal (widget-value
                      (alist-get 'tags my/aaronnote-roam-new--widgets))
                     "math, logic")))))

(ert-deftest my/aaronnote-roam-new-path-and-tags-labels-run-actions ()
  (my/aaronnote-roam-test-with-vault
    (make-directory (expand-file-name "archive" root) t)
    (with-temp-buffer
      (my/aaronnote-roam-new-mode)
      (setq-local my/aaronnote-roam-new--base-directory "projects"
                  my/aaronnote-roam-new--templates
                  '((:key "roam" :name "Roam note"))
                  my/aaronnote-roam-new--draft
                  (my/aaronnote-roam-new--default-draft "projects"))
      (my/aaronnote-roam-new-render)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (search-forward "SAVE PATH")
        (let ((action (get-text-property
                       (match-beginning 0) 'aaron-ui-board--row-action)))
          (should action)
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _args) (expand-file-name "archive" root))))
            (funcall action nil))))
      (should (equal (plist-get my/aaronnote-roam-new--draft :path)
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
      (should (equal (plist-get my/aaronnote-roam-new--draft :tags)
                     '("math")))
      (should (equal (widget-value
                      (alist-get 'tags my/aaronnote-roam-new--widgets))
                     "math")))))

(ert-deftest my/aaronnote-roam-new-default-draft-avoids-existing-untitled ()
  (my/aaronnote-roam-test-with-vault
    (my/aaronnote-roam-test--write-file
     (expand-file-name "projects/untitled.md" root)
     "# Untitled\n")
    (let ((draft (my/aaronnote-roam-new--default-draft "projects")))
      (should (equal (plist-get draft :title) "Untitled"))
      (should (equal (plist-get draft :path) "projects/untitled-2.md")))))

(ert-deftest my/aaronnote-roam-new-create-refreshes-stale-untitled-path ()
  (my/aaronnote-roam-test-with-vault
    (my/aaronnote-roam-test--write-file
     (expand-file-name "projects/project-atlas.md" root)
     "# Project Atlas\n")
    (let* ((my/aaronnote-roam-new--base-directory "projects")
           (draft (my/aaronnote-roam-new--draft-for-create
                   '(:node-type "roam"
                     :title "Project Atlas"
                     :path "projects/untitled.md"
                     :kind "note"
                     :template-key "roam"
                     :tags nil))))
      (should (equal (plist-get draft :path)
                     "projects/project-atlas-2.md")))))

(ert-deftest my/aaronnote-roam-new-create-defaults-match-aaronnote ()
  (my/aaronnote-roam-test-with-vault
    (let ((my/aaronnote-roam-new--base-directory "projects"))
      (let ((draft
             (my/aaronnote-roam-new--draft-for-create
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

(ert-deftest my/aaronnote-roam-new-create-uses-aaronnote-runtime-draft ()
  (my/aaronnote-roam-test-with-vault
    (let* ((draft '(:node-type "roam"
                    :title "Project Atlas"
                    :path "projects/project-atlas.md"
                    :kind "note"
                    :template-key "project"
                    :tags ("work" "planning")))
           (created-file (expand-file-name "projects/project-atlas.md" root))
           captured opened)
      (cl-letf (((symbol-function 'my/aaronnote-roam--runtime-call)
                 (lambda (action &rest args)
                   (setq captured
                         (list action
                               (json-parse-string
                                (cadr args)
                                :object-type 'hash-table
                                :array-type 'list)))
                   (my/aaronnote-roam-test--write-file
                    created-file "# Project Atlas\n")
                   (let ((response (make-hash-table :test 'equal)))
                     (puthash "file" created-file response)
                     response)))
                ((symbol-function 'my/aaronnote-open-file)
                 (lambda (file) (setq opened file))))
        (should (equal (my/aaronnote-roam-new--create-draft draft)
                       created-file)))
      (should (equal opened created-file))
      (should (equal (car captured) "create"))
      (let ((payload (cadr captured)))
        (should (equal (gethash "nodeType" payload) "roam"))
        (should (equal (gethash "path" payload)
                       "projects/project-atlas.md"))
        (should (equal (gethash "templateKey" payload) "project"))
        (should (equal (gethash "tags" payload) '("work" "planning")))))))

(provide 'md-roam-tests)
;;; md-roam-tests.el ends here
