;;; ai-workbench-tests.el --- Tests for ai-workbench -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)
(require 'ai-workbench-tools)
(require 'ai-workbench-status)

(ert-deftest ai-workbench-profile-names-prefers-all-search-paths ()
  "Profile names should be collected across configured directories."
  (let* ((dir-a (make-temp-file "aiw-profiles-a" t))
         (dir-b (make-temp-file "aiw-profiles-b" t))
         (ai-workbench-profile-search-path (list dir-a dir-b)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "default.txt" dir-a)
            (insert "default a"))
          (with-temp-file (expand-file-name "review.txt" dir-b)
            (insert "review b"))
          (should (equal '("default" "review")
                         (sort (ai-workbench-profile-names) #'string<))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest ai-workbench-profile-read-uses-first-readable-match ()
  "Profile contents should come from the earliest readable search path."
  (let* ((dir-a (make-temp-file "aiw-profile-first" t))
         (dir-b (make-temp-file "aiw-profile-second" t))
         (ai-workbench-profile-search-path (list dir-a dir-b)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "review.txt" dir-a)
            (insert "first"))
          (with-temp-file (expand-file-name "review.txt" dir-b)
            (insert "second"))
          (should (string= "first" (ai-workbench-profile-read "review"))))
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest ai-workbench-profile-summary-uses-first-line ()
  "Profile summary should use the first non-empty line."
  (let* ((dir-a (make-temp-file "aiw-profile-summary" t))
         (ai-workbench-profile-search-path (list dir-a)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "debug.txt" dir-a)
            (insert "\nFirst summary line\nSecond line\n"))
          (should (string-match-p "First summary line"
                                  (ai-workbench-profile-summary "debug"))))
      (delete-directory dir-a t))))

(ert-deftest ai-workbench-profile-build-prompt-includes-standard-policies ()
  "Built profile prompts should include Git policy and activation reply protocol."
  (let* ((dir-a (make-temp-file "aiw-profile-prompt" t))
         (project-root "/tmp/ai-workbench-tests/")
         (ai-workbench-profile-search-path (list dir-a)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "default.txt" dir-a)
            (insert "Project-specific behavior."))
          (ai-workbench-session-set-profile "default" project-root)
          (let ((prompt (ai-workbench-profile-build-prompt project-root)))
            (should (string-match-p "Git usage rules:" prompt))
            (should (string-match-p "Write approval rules:" prompt))
            (should (string-match-p "已开启 Emacs 特调模式" prompt))
            (should (string-match-p "Project-specific behavior\\." prompt))))
      (delete-directory dir-a t))))

(ert-deftest ai-workbench-profile-build-prompt-prefers-snippet-files ()
  "Built profile prompts should prefer shared snippet files from search path."
  (let* ((profile-dir (make-temp-file "aiw-profile-dir" t))
         (snippet-dir (make-temp-file "aiw-snippet-dir" t))
         (project-root "/tmp/ai-workbench-tests/")
         (ai-workbench-profile-search-path (list profile-dir))
         (ai-workbench-profile-snippet-search-path (list snippet-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "default.txt" profile-dir)
            (insert "Project-specific behavior."))
          (with-temp-file (expand-file-name "git-policy.txt" snippet-dir)
            (insert "Custom git rule."))
          (with-temp-file (expand-file-name "write-policy.txt" snippet-dir)
            (insert "Custom write rule."))
          (with-temp-file (expand-file-name "activation-policy.txt" snippet-dir)
            (insert "Custom activation rule."))
          (ai-workbench-session-set-profile "default" project-root)
          (let ((prompt (ai-workbench-profile-build-prompt project-root)))
            (should (string-match-p "Custom git rule\\." prompt))
            (should (string-match-p "Custom write rule\\." prompt))
            (should (string-match-p "Custom activation rule\\." prompt))
            (should-not (string-match-p "Git usage rules:" prompt))))
      (delete-directory profile-dir t)
      (delete-directory snippet-dir t))))

(ert-deftest ai-workbench-profile-wrap-user-prompt-adds_profile_context ()
  "Wrapped user prompts should prepend the shared profile context."
  (let* ((profile-dir (make-temp-file "aiw-profile-dir" t))
         (snippet-dir (make-temp-file "aiw-snippet-dir" t))
         (project-root "/tmp/ai-workbench-tests/")
         (ai-workbench-profile-search-path (list profile-dir))
         (ai-workbench-profile-snippet-search-path (list snippet-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "default.txt" profile-dir)
            (insert "Project-specific behavior."))
          (with-temp-file (expand-file-name "activation-policy.txt" snippet-dir)
            (insert "Ack first."))
          (ai-workbench-session-set-profile "default" project-root)
          (let ((prompt (ai-workbench-profile-wrap-user-prompt "Fix bug" project-root)))
            (should (string-match-p "Ack first\\." prompt))
            (should (string-match-p "User task:" prompt))
            (should (string-match-p "Fix bug" prompt))))
      (delete-directory profile-dir t)
      (delete-directory snippet-dir t))))

(ert-deftest ai-workbench-profile-uses-editable-template-files ()
  "Profile wrappers should be controlled by editable template files."
  (let* ((profile-dir (make-temp-file "aiw-profile-dir" t))
         (snippet-dir (make-temp-file "aiw-snippet-dir" t))
         (template-dir (make-temp-file "aiw-template-dir" t))
         (project-root "/tmp/ai-workbench-tests/")
         (ai-workbench-profile-search-path (list profile-dir))
         (ai-workbench-profile-snippet-search-path (list snippet-dir))
         (ai-workbench-profile-template-search-path (list template-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "default.txt" profile-dir)
            (insert "Profile body."))
          (with-temp-file (expand-file-name "git-policy.txt" snippet-dir)
            (insert "Git body."))
          (with-temp-file (expand-file-name "write-policy.txt" snippet-dir)
            (insert "Write body."))
          (with-temp-file (expand-file-name "activation-policy.txt" snippet-dir)
            (insert "Activation body."))
          (with-temp-file (expand-file-name "profile-prompt.txt" template-dir)
            (insert "TPL {{profile}} {{working-directory}} {{git-policy}} {{write-policy}} {{activation-policy}} {{profile-text}}"))
          (with-temp-file (expand-file-name "user-prompt-wrapper.txt" template-dir)
            (insert "WRAP\n{{profile-prompt}}\nTASK\n{{user-prompt}}"))
          (ai-workbench-session-set-profile "default" project-root)
          (let ((prompt (ai-workbench-profile-wrap-user-prompt "Fix bug" project-root)))
            (should (string-match-p "\\`WRAP" prompt))
            (should (string-match-p "TPL default" prompt))
            (should (string-match-p "Git body" prompt))
            (should (string-match-p "TASK\nFix bug" prompt))))
      (delete-directory profile-dir t)
      (delete-directory snippet-dir t)
      (delete-directory template-dir t))))

(ert-deftest ai-workbench-profile-rejects-unsafe-etc-names ()
  "Profile and snippet names should not escape their etc directories."
  (should-error (ai-workbench-profile-file "../escape") :type 'user-error)
  (should-error (ai-workbench-profile-snippet-file "bad/name") :type 'user-error)
  (should-error (ai-workbench-profile-template-file "..") :type 'user-error))

(ert-deftest ai-workbench-tools-relative-path-does-not-match-siblings ()
  "Relative path helpers should not treat sibling prefixes as project files."
  (should-not
   (string= "ile/a.el"
            (ai-workbench-tools--relative-path
             "/tmp/projectile/a.el"
             "/tmp/project/"))))

(ert-deftest ai-workbench-tools-writing-prompt-uses-editable-template ()
  "Writing prompts should be rendered from editable template files."
  (let* ((template-dir (make-temp-file "aiw-writing-template-dir" t))
         (ai-workbench-profile-template-search-path (list template-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "writing-prompt.txt" template-dir)
            (insert "WRITE {{mode}}\n{{task}}\n{{context}}\n{{text}}"))
          (let ((prompt (ai-workbench-tools--writing-prompt
                         "润色" "make it concise" "hello" "source: note.org")))
            (should (string-match-p "\\`WRITE 润色" prompt))
            (should (string-match-p "make it concise" prompt))
            (should (string-match-p "source: note\\.org" prompt))
            (should (string-match-p "hello" prompt))))
      (delete-directory template-dir t))))

(ert-deftest ai-workbench-session-reset-profile-injected-clears-all-backends ()
  "Resetting injected profile state should clear every backend marker."
  (let ((project-root "/tmp/ai-workbench-tests/"))
    (ai-workbench-session-mark-profile-injected 'claude project-root)
    (ai-workbench-session-mark-profile-injected 'codex project-root)
    (ai-workbench-session-reset-profile-injected project-root)
    (should-not (ai-workbench-session-profile-injected-p 'claude project-root))
    (should-not (ai-workbench-session-profile-injected-p 'codex project-root))))

(ert-deftest ai-workbench-status-render-includes-core-fields ()
  "Status rendering should surface the main project fields."
  (let* ((project-root "/tmp/ai-workbench-tests/")
         (ai-workbench-profile-search-path nil))
    (ai-workbench-session-set-backend 'codex project-root)
    (ai-workbench-session-set-profile "default" project-root)
    (ai-workbench-session-set-initialized t project-root)
    (ai-workbench-session-set-last-status "Ready" project-root)
    (ai-workbench-session-set-last-prompt "Fix tests" project-root)
    (let ((text (ai-workbench-status--render project-root)))
      (should (string-match-p "Backend[[:space:]]+codex" text))
      (should (string-match-p "Profile[[:space:]]+default" text))
      (should (string-match-p "Profile summary" text))
      (should (string-match-p "Last status[[:space:]]+Ready" text))
      (should (string-match-p "Fix tests" text)))))

(ert-deftest ai-workbench-tools-project-file-summary-limits-output ()
  "Project file summaries should truncate long file lists."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&rest _) 'dummy-project))
            ((symbol-function 'project-files)
             (lambda (&rest _)
               (mapcar (lambda (name)
                         (expand-file-name name "/tmp/project/"))
                       '("a.el" "b.el" "c.el")))))
    (let ((ai-workbench-tools-project-files-limit 2))
      (should (string-match-p
               "\\.\\.\\. 1 more file"
               (ai-workbench-tools--project-file-summary "/tmp/project/"))))))

(ert-deftest ai-workbench-tools-test-failure-summary-reads-compilation-buffer ()
  "Test failure summaries should extract failure lines from active buffers."
  (let ((buffer (get-buffer-create "*compilation*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "ok\nFAIL test_example\nAssertionError: boom\n")
          (compilation-mode)
          (let ((summary (ai-workbench-tools--test-failure-summary)))
            (should (string-match-p "FAIL test_example" summary))
            (should (string-match-p "AssertionError: boom" summary))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'ai-workbench-tests)
;;; ai-workbench-tests.el ends here
