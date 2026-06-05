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

(provide 'md-roam-tests)
;;; md-roam-tests.el ends here
