;;; xenodium-workbench-tests.el --- Focused workbench regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/xenodium-workbench-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'init-bazel)
(require 'init-calendar-tools)
(require 'init-editing-workbench)
(require 'init-file-workbench)
(require 'init-github)
(require 'init-macos-tools)
(require 'init-org-tools)

(defmacro my/workbench-test-with-region (beg end &rest body)
  "Activate BEG through END while evaluating BODY."
  (declare (indent 2))
  `(let ((transient-mark-mode t))
     (goto-char ,beg)
     (push-mark ,end t t)
     ,@body))

(ert-deftest my/transpose-regions-dwim-swaps-disjoint-regions ()
  (with-temp-buffer
    (insert "one middle two")
    (my/workbench-test-with-region 1 4
      (my/transpose-regions-dwim nil))
    (my/workbench-test-with-region 12 15
      (my/transpose-regions-dwim nil))
    (should (equal (buffer-string) "two middle one"))
    (should-not my/transpose-regions--buffer)))

(ert-deftest my/transpose-regions-reset-releases-markers-and-overlay ()
  (with-temp-buffer
    (insert "alpha beta")
    (my/workbench-test-with-region 1 6
      (my/transpose-regions-dwim nil))
    (should (overlayp my/transpose-regions--overlay))
    (my/transpose-regions-reset)
    (should-not my/transpose-regions--buffer)
    (should-not my/transpose-regions--overlay)))

(ert-deftest my/hash-dwim-hashes-the-requested-bounds ()
  (with-temp-buffer
    (insert "abc")
    (should
     (equal (my/hash-dwim 'sha256 (point-min) (point-max))
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (should (equal (current-kill 0)
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))))

(ert-deftest my/copy-as-format-uses-the-whole-buffer-without-a-region ()
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (goto-char (point-min))
    (my/copy-as-format-markdown)
    (should (string-match-p "alpha" (current-kill 0)))
    (should (string-match-p "beta" (current-kill 0)))))

(ert-deftest my/narrow-or-widen-dwim-prefers-the-active-region ()
  (with-temp-buffer
    (insert "before middle after")
    (my/workbench-test-with-region 8 14
      (my/narrow-or-widen-dwim nil))
    (should (buffer-narrowed-p))
    (should (equal (buffer-string) "middle"))
    (my/narrow-or-widen-dwim nil)
    (should-not (buffer-narrowed-p))))

(ert-deftest my/last-change-mode-records-and-releases-its-marker ()
  (let ((buffer (generate-new-buffer " *last-change-test*"))
        (was-enabled my/last-change-mode))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (text-mode)
          (my/last-change-mode 1)
          (insert "changed")
          (should (eq (marker-buffer my/last-change-marker) buffer))
          (my/last-change-mode -1)
          (should-not my/last-change-marker))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when was-enabled
        (my/last-change-mode 1)))))

(ert-deftest my/files-create-parent-directories-honors-always-policy ()
  (let* ((root (make-temp-file "file-workbench-" t))
         (parent (expand-file-name "nested/deeper/" root))
         (file (expand-file-name "file.txt" parent)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (let ((my/files-create-parent-directories 'always))
            (my/files-create-parent-directories-h))
          (should (file-directory-p parent)))
      (delete-directory root t))))

(ert-deftest my/files-copy-paths-joins-one-kill-ring-entry ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.txt")
    (should (equal (my/files-copy-paths nil) "/tmp/example.txt"))
    (should (equal (current-kill 0) "/tmp/example.txt"))))

(ert-deftest my/dired-parse-du-kib-sums-each-entry ()
  (should (= (my/dired--parse-du-kib "4\t/tmp/a\n12\t/tmp/b\n") 16)))

(ert-deftest my/bazel-workspace-root-recognizes-bzlmod ()
  (let* ((root (make-temp-file "bazel-workbench-" t))
         (child (expand-file-name "pkg/nested/" root)))
    (unwind-protect
        (progn
          (make-directory child t)
          (with-temp-file (expand-file-name "MODULE.bazel" root))
          (should (equal (file-name-as-directory root)
                         (my/bazel-workspace-root child))))
      (delete-directory root t))))

(ert-deftest my/bazel-parse-targets-sorts-and-deduplicates ()
  (should (equal (my/bazel--parse-targets "//z:t\n//a:t\n//z:t\n")
                 '("//a:t" "//z:t"))))

(ert-deftest my/bazel-workbench-commands-are-top-level-definitions ()
  (dolist (command '(my/bazel-build my/bazel-test my/bazel-run
                     my/bazel-insert-target my/bazel-jump-to-target))
    (should (fboundp command))))

(ert-deftest my/bazel-target-query-uses-the-logical-target ()
  (let ((cache-root (make-temp-file "bazel-cache-" t))
        (context (remote-context-create
                  :target-id "test-target"
                  :localname "/workspace/"
                  :workspace-root "/workspace/"
                  :source 'test))
        captured
        callback-value)
    (unwind-protect
        (let ((my/bazel-cache-directory cache-root))
          (cl-letf (((symbol-function 'my/bazel-workspace-root)
                     (lambda (&optional _directory) "/workspace/"))
                    ((symbol-function 'my/bazel--context)
                     (lambda (&optional _root) context))
                    ((symbol-function 'my/bazel-executable)
                     (lambda (&optional _context) "/usr/bin/bazelisk"))
                    ((symbol-function 'remote-exec-async)
                     (lambda (program &rest options)
                       (setq captured (cons program options))
                       (funcall
                        (plist-get options :callback)
                        (remote-exec-result-create
                         :status 0 :stdout "//b:t\n//a:t\n" :stderr "")))))
            (my/bazel-with-targets
             (lambda (root context executable targets)
               (setq callback-value (list root context executable targets)))
             t)
            (should (equal (car captured) "/usr/bin/bazelisk"))
            (should (equal (plist-get (cdr captured) :args)
                           '("query" "//..." "--output=label")))
            (should (eq (plist-get (cdr captured) :context) context))
            (should (equal callback-value
                           (list "/workspace/" context "/usr/bin/bazelisk"
                                 '("//a:t" "//b:t"))))))
      (delete-directory cache-root t))))

(ert-deftest my/github-parses-and-labels-topic-json ()
  (let* ((items (my/github--parse-topics
                 "[{\"number\":7,\"title\":\"Fix\",\"author\":{\"login\":\"sam\"},\"isPullRequest\":true,\"url\":\"https://example.test/7\"}]"))
         (item (car items)))
    (should (= (alist-get 'number item) 7))
    (should (string-match-p "PR.*#7.*Fix.*sam"
                            (my/github--topic-candidate item)))))

(ert-deftest my/github-repository-resolution-runs-on-the-logical-target ()
  (let (captured)
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/workspace/"))
              ((symbol-function 'remote-context)
               (lambda (_path) 'test-context))
              ((symbol-function 'remote-executable-find)
               (lambda (program context)
                 (should (equal program "gh"))
                 (should (eq context 'test-context))
                 "/usr/bin/gh"))
              ((symbol-function 'remote-exec)
               (lambda (program &rest options)
                 (setq captured (cons program options))
                 (remote-exec-result-create
                  :status 0 :stdout "owner/repo" :stderr ""))))
      (should (equal (my/github--repo-info)
                     '("/workspace/" test-context "/usr/bin/gh" "owner/repo")))
      (should (equal (car captured) "/usr/bin/gh"))
      (should (equal (plist-get (cdr captured) :args)
                     '("repo" "view" "--json" "nameWithOwner"
                       "--jq" ".nameWithOwner")))
      (should (eq (plist-get (cdr captured) :context) 'test-context)))))

(ert-deftest my/dired-marked-size-routes-du-and-target-native-paths ()
  (with-temp-buffer
    (setq major-mode 'dired-mode
          default-directory "/ssh:test:/workspace/")
    (let (captured)
      (cl-letf (((symbol-function 'my/files-selected-paths)
                 (lambda () '("/ssh:test:/workspace/a"
                              "/ssh:test:/workspace/b")))
                ((symbol-function 'remote-context)
                 (lambda (_path) 'test-context))
                ((symbol-function 'remote-executable-find)
                 (lambda (_program _context) "/usr/bin/du"))
                ((symbol-function 'remote-exec)
                 (lambda (program &rest options)
                   (setq captured (cons program options))
                   (remote-exec-result-create
                    :status 0 :stdout "4\ta\n12\tb\n" :stderr ""))))
        (should (= (my/dired-marked-size) (* 16 1024)))
        (should (equal (car captured) "/usr/bin/du"))
        (should (equal (plist-get (cdr captured) :args)
                       '("-sk" "/workspace/a" "/workspace/b")))
        (should (eq (plist-get (cdr captured) :context) 'test-context))))))

(ert-deftest my/macos-ns-color-conversion-uses-eight-bit-components ()
  (should (equal (my/macos--ns-color-to-hex "65535,32768,0") "#ff8000")))

(ert-deftest my/org-link-dwim-wraps-a-region-with-the-clipboard-url ()
  (with-temp-buffer
    (org-mode)
    (insert "Link title")
    (kill-new "https://example.test/page")
    (my/workbench-test-with-region (point-min) (point-max)
      (my/org-insert-link-dwim))
    (should (equal (buffer-string)
                   "[[https://example.test/page][Link title]]"))))

(ert-deftest my/year-calendar-renders-first-and-last-month ()
  (save-window-excursion
    (let ((buffer (my/calendar-year 2026)))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "2026 January" (buffer-string)))
            (should (string-match-p "2026 December" (buffer-string)))
            (should my/year-calendar-mode))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'xenodium-workbench-tests)
;;; xenodium-workbench-tests.el ends here
