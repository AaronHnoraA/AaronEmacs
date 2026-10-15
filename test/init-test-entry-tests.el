;;; init-test-entry-tests.el --- Test runner entry regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-test)

(ert-deftest test-commands-are-searchable-without-my-prefix ()
  (dolist (command '(test-nearest test-file test-project test-rerun))
    (should (commandp command))))

(ert-deftest test-runner-uses-current-language-in-mixed-project ()
  "File and project test scopes should follow the current source language."
  (let ((root (make-temp-file "mixed-test-runner-" t)))
    (unwind-protect
        (progn
          (dolist (name '("Cargo.toml" "go.mod" "pyproject.toml"
                          "package.json"))
            (with-temp-file (expand-file-name name root)))
          (cl-letf (((symbol-function 'my/test-project-root)
                     (lambda () root))
                    ((symbol-function 'my/project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/project-local-env)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/test--current-defun)
                     (lambda () "TestThing")))
            (dolist (case '(("main.go" project "go test ./...")
                            ("main.go" file "go test .")
                            ("main.py" project "pytest")
                            ("main.py" file "pytest main.py")
                            ("main.rs" project "cargo test")
                            ("main.js" project "npm test")))
              (with-temp-buffer
                (setq buffer-file-name (expand-file-name (car case) root)
                      default-directory (file-name-as-directory root))
                (should (equal (cadr (my/test--command (cadr case)))
                               (nth 2 case)))))))
      (delete-directory root t))))

(ert-deftest test-go-nearest-quotes-its-name-as-a-regexp ()
  "A Go test name should be one literal -run match in the current package."
  (let ((root (make-temp-file "go-nearest-runner-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "go.mod" root))
          (cl-letf (((symbol-function 'my/test-project-root)
                     (lambda () root))
                    ((symbol-function 'my/project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/project-local-env)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/test--current-defun)
                     (lambda () "TestThing.Sub")))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "main_test.go" root)
                    default-directory (file-name-as-directory root))
              (let ((command (cadr (my/test--command 'nearest))))
                (should (string-prefix-p "go test . -run " command))
                (should (equal
                         (shell-command-to-string
                          (format "set -- %s; printf '%%s' \"$5\"" command))
                         "^TestThing\\.Sub$"))))))
      (delete-directory root t))))

(ert-deftest test-rerun-state-survives-compile-start-failure ()
  "A failed launch must leave the previous rerun target intact."
  (let ((previous (frame-parameter nil 'my/output-last-runs)))
    (unwind-protect
        (progn
          (my/output-record-run 'test "previous command" "/tmp/"
                                '(("OLD" . "1")))
          (cl-letf (((symbol-function 'compile)
                     (lambda (&rest _) (error "Compile could not start"))))
            (should-error (my/test--run "new command" "/tmp/" nil)))
          (should (equal (my/output-last-run 'test)
                         '("previous command" "/tmp/" (("OLD" . "1"))))))
      (set-frame-parameter nil 'my/output-last-runs previous))))

(ert-deftest test-project-local-command-bypasses-runner-detection ()
  "An explicit project command should not probe unrelated runner markers."
  (let ((root (make-temp-file "test-override-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'my/test-project-root)
                   (lambda () root))
                  ((symbol-function 'my/project-local-test-command)
                   (lambda (&rest _) "custom test command"))
                  ((symbol-function 'my/project-local-env)
                   (lambda (&rest _) nil))
                  ((symbol-function 'my/test--runner)
                   (lambda (&rest _) (error "Runner should not be probed"))))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "some.rs" root)
                  default-directory (file-name-as-directory root))
            (should (equal (cadr (my/test--command 'nearest))
                           "custom test command"))))
      (delete-directory root t))))

(ert-deftest test-runner-does-not-use-another-language-for-source-file ()
  (let ((root (make-temp-file "test-runner-mismatch-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "go.mod" root))
          (cl-letf (((symbol-function 'my/test-project-root)
                     (lambda () root))
                    ((symbol-function 'my/project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function 'my/project-local-env)
                     (lambda (&rest _) nil)))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "main.rs" root)
                    default-directory (file-name-as-directory root))
              (should-error (my/test--command 'file) :type 'user-error))))
      (delete-directory root t))))

(provide 'init-test-entry-tests)
;;; init-test-entry-tests.el ends here
