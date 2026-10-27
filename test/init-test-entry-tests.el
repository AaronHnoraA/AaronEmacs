;;; init-test-entry-tests.el --- Test runner entry regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/test/config)

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
          (cl-letf (((symbol-function '+test-project-root)
                     (lambda () root))
                    ((symbol-function '+project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function '+project-local-env)
                     (lambda (&rest _) nil))
                    ((symbol-function '+test--current-defun)
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
                (should (equal (cadr (+test--command (cadr case)))
                               (nth 2 case)))))))
      (delete-directory root t))))

(defun test-entry--go-command (scope source point-after)
  "Return the Go test command for SCOPE in a buffer holding SOURCE.
Point is placed after the text POINT-AFTER, or at the start when nil.  The
`-run' or `-bench' pattern is returned as the shell would pass it."
  (let ((root (make-temp-file "go-runner-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "go.mod" root))
          (cl-letf (((symbol-function '+test-project-root) (lambda () root))
                    ((symbol-function '+project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function '+project-local-env)
                     (lambda (&rest _) nil)))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "main_test.go" root)
                    default-directory (file-name-as-directory root))
              (insert source)
              (goto-char (point-min))
              (when point-after (search-forward point-after))
              (let ((command (cadr (+test--command scope))))
                (string-trim
                 (shell-command-to-string
                  (format "set -- %s; shift 2; printf '%%s ' \"$@\"" command)))))))
      (delete-directory root t))))

(defconst test-entry--go-source
  "package main\n\nfunc helper() {}\n\nfunc TestOne(t *testing.T) {\n\thelper()\n}\n\nfunc TestTwo(t *testing.T) {}\n\nfunc BenchmarkFast(b *testing.B) {\n\tfor range b.N {}\n}\n")

(ert-deftest test-go-nearest-runs-the-test-function-around-point ()
  "Nearest is the Go test around point, even where point is in a call to a helper."
  (should (equal (test-entry--go-command 'nearest test-entry--go-source "\thelp")
                 ". -run ^(TestOne)$"))
  (should (equal (test-entry--go-command 'nearest test-entry--go-source "TestTw")
                 ". -run ^(TestTwo)$")))

(ert-deftest test-go-nearest-benchmark-runs-only-that-benchmark ()
  "A benchmark at point runs with -bench and no tests."
  (should (equal (test-entry--go-command 'nearest test-entry--go-source "b.N")
                 ". -run ^$ -bench ^(BenchmarkFast)$")))

(ert-deftest test-go-file-runs-the-tests-this-file-declares ()
  "File scope runs the file's own tests, not the rest of its package."
  (should (equal (test-entry--go-command 'file test-entry--go-source nil)
                 ". -run ^(TestOne|TestTwo)$"))
  (should (equal (test-entry--go-command 'file "package main\n" nil)
                 ".")))

(ert-deftest test-rerun-state-survives-compile-start-failure ()
  "A failed launch must leave the previous rerun target intact."
  (let ((previous (frame-parameter nil '+output-last-runs)))
    (unwind-protect
        (progn
          (+output-record-run 'test "previous command" "/tmp/"
                                '(("OLD" . "1")))
          (cl-letf (((symbol-function 'compile)
                     (lambda (&rest _) (error "Compile could not start"))))
            (should-error (+test--run "new command" "/tmp/" nil)))
          (should (equal (+output-last-run 'test)
                         '("previous command" "/tmp/" (("OLD" . "1"))))))
      (set-frame-parameter nil '+output-last-runs previous))))

(ert-deftest test-project-local-command-bypasses-runner-detection ()
  "An explicit project command should not probe unrelated runner markers."
  (let ((root (make-temp-file "test-override-" t)))
    (unwind-protect
        (cl-letf (((symbol-function '+test-project-root)
                   (lambda () root))
                  ((symbol-function '+project-local-test-command)
                   (lambda (&rest _) "custom test command"))
                  ((symbol-function '+project-local-env)
                   (lambda (&rest _) nil))
                  ((symbol-function '+test--runner)
                   (lambda (&rest _) (error "Runner should not be probed"))))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "some.rs" root)
                  default-directory (file-name-as-directory root))
            (should (equal (cadr (+test--command 'nearest))
                           "custom test command"))))
      (delete-directory root t))))

(ert-deftest test-runner-does-not-use-another-language-for-source-file ()
  (let ((root (make-temp-file "test-runner-mismatch-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "go.mod" root))
          (cl-letf (((symbol-function '+test-project-root)
                     (lambda () root))
                    ((symbol-function '+project-local-test-command)
                     (lambda (&rest _) nil))
                    ((symbol-function '+project-local-env)
                     (lambda (&rest _) nil)))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "main.rs" root)
                    default-directory (file-name-as-directory root))
              (should-error (+test--command 'file) :type 'user-error))))
      (delete-directory root t))))

(provide 'init-test-entry-tests)
;;; init-test-entry-tests.el ends here
