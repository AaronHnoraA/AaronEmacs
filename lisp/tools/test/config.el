;;; tools/test/config.el --- Project test workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'aaron-funcs)
(require 'tools/output/config)
(require 'tools/project-local/config)
(require 'subr-x)

(declare-function +project-current-root "tools/project/config")

(defgroup +test nil
  "Test helpers."
  :group 'tools)

(defun +test-project-root ()
  "Return the best project root for test commands."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun +test--package-manager (root)
  "Return the preferred Node package manager for ROOT."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun +test--runner (root &optional file)
  "Return a test runner for ROOT, using FILE's language when known."
  (let ((extension (and file (downcase (or (file-name-extension file) "")))))
    (pcase extension
      ("rs" (and (file-exists-p (expand-file-name "Cargo.toml" root))
                 'cargo))
      ("go" (and (file-exists-p (expand-file-name "go.mod" root))
                 'go))
      ((or "py" "pyi")
       (and (cl-some (lambda (name)
                       (file-exists-p (expand-file-name name root)))
                     '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg"))
            'pytest))
      ((or "js" "jsx" "ts" "tsx" "mjs" "cjs")
       (and (file-exists-p (expand-file-name "package.json" root))
            'node))
      (_
       (cond
        ((file-exists-p (expand-file-name "Cargo.toml" root)) 'cargo)
        ((cl-some (lambda (name)
                    (file-exists-p (expand-file-name name root)))
                  '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg"))
         'pytest)
        ((file-exists-p (expand-file-name "go.mod" root)) 'go)
        ((file-exists-p (expand-file-name "package.json" root)) 'node)
        ((file-exists-p (expand-file-name "justfile" root)) 'just)
        ((file-exists-p (expand-file-name "Makefile" root)) 'make))))))

(defun +test--current-defun ()
  "Return the current defun name when possible."
  (or (when (fboundp 'which-function)
        (which-function))
      (ignore-errors
        (add-log-current-defun))))

(defun +test--python-node-id ()
  "Return a pytest node id suffix for point when possible."
  (when-let* ((name (+test--current-defun)))
    (mapconcat #'identity (split-string name "\\." t) "::")))

(defun +test--go-package (root file)
  "Return the Go package path relative to ROOT for FILE."
  (let* ((dir (file-name-directory file))
         (rel (file-relative-name dir root)))
    (if (equal rel "./")
        "."
      (concat "./" (directory-file-name rel)))))

(defconst +test--go-test-regexp
  (concat "^func[ \t]+\\(\\(?:Test\\|Benchmark\\|Example\\|Fuzz\\)"
          "[[:alnum:]_]*\\)[ \t]*[[(]")
  "Regexp matching a Go test, benchmark, example or fuzz function header.")

;; From Doom's lang/go: the nearest Go test is the test function around point,
;; not whatever function `which-function' reports (a helper is no test), and
;; a file's tests are the functions it declares, not its whole package.
(defun +test--go-test-at-point ()
  "Return the name of the Go test function at or above point, or nil."
  (save-excursion
    (end-of-line)
    (when (re-search-backward +test--go-test-regexp nil t)
      (match-string-no-properties 1))))

(defun +test--go-file-tests ()
  "Return the names of the Go tests, examples and fuzz targets in this buffer."
  (save-excursion
    (goto-char (point-min))
    (let (names)
      (while (re-search-forward +test--go-test-regexp nil t)
        (unless (string-prefix-p "Benchmark" (match-string 1))
          (push (match-string-no-properties 1) names)))
      (nreverse names))))

(defun +test--go-command (package names)
  "Return a `go test' command for PACKAGE limited to the functions NAMES."
  (let ((benchmarks (seq-filter (lambda (name) (string-prefix-p "Benchmark" name))
                                names))
        (pattern (lambda (list) (concat "^(" (string-join list "|") ")$"))))
    (cond
     ((null names) (format "go test %s" (shell-quote-argument package)))
     (benchmarks
      (format "go test %s -run '^$' -bench %s"
              (shell-quote-argument package)
              (shell-quote-argument (funcall pattern benchmarks))))
     (t (format "go test %s -run %s"
                (shell-quote-argument package)
                (shell-quote-argument (funcall pattern names)))))))

(defun +test--command (scope)
  "Build a test command for SCOPE."
  (let* ((root (file-name-as-directory (expand-file-name (+test-project-root))))
         (file (and buffer-file-name (expand-file-name buffer-file-name)))
         (relfile (and file (file-relative-name file root)))
         (defun-name (+test--current-defun))
         (override (+project-local-test-command scope root))
         (runner (unless override (+test--runner root file)))
         (pm (when (eq runner 'node) (+test--package-manager root)))
         (env (+project-local-env 'test root)))
    (list
     root
     (or override
         (progn
           (unless runner
             (user-error "No test runner for %s in %s; set a project :test override"
                         (if file (file-name-nondirectory file) "this project")
                         root))
           (pcase (list runner scope)
             (`(cargo nearest)
              (if (and defun-name (not (string-empty-p defun-name)))
                  (format "cargo test %s" (shell-quote-argument defun-name))
                "cargo test"))
             (`(cargo ,_) "cargo test")
             (`(pytest nearest)
              (if-let* ((node-id (and file (+test--python-node-id))))
                  (format "pytest %s::%s"
                          (shell-quote-argument relfile)
                          (shell-quote-argument node-id))
                (format "pytest %s" (shell-quote-argument (or relfile ".")))))
             (`(pytest file)
              (format "pytest %s" (shell-quote-argument (or relfile "."))))
             (`(pytest ,_) "pytest")
             (`(go nearest)
              (if file
                  (+test--go-command (+test--go-package root file)
                                     (ensure-list (+test--go-test-at-point)))
                "go test ./..."))
             (`(go file)
              (if file
                  (+test--go-command (+test--go-package root file)
                                     (+test--go-file-tests))
                "go test ./..."))
             (`(go ,_) "go test ./...")
             (`(node nearest)
              (format "%s test -- %s" pm (shell-quote-argument (or relfile "."))))
             (`(node file)
              (format "%s test -- %s" pm (shell-quote-argument (or relfile "."))))
             (`(node ,_) (format "%s test" pm))
             (`(just ,_) "just test")
             (`(make ,_) "make test")
             (_ (user-error "Unsupported test scope")))))
     env)))

(defun +test--run (command root &optional env)
  "Run test COMMAND from ROOT."
  (let ((default-directory root)
        (process-environment (+project-local-apply-env env)))
    (prog1 (+output-compile command "test" env)
      (+output-record-run 'test command root env))))

(defun +test/nearest ()
  "Run the nearest test to point."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (+test--command 'nearest)))
    (+test--run command root env)))

(defun +test/file ()
  "Run tests for the current file."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (+test--command 'file)))
    (+test--run command root env)))

(defun +test/project ()
  "Run the project test suite."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (+test--command 'project)))
    (+test--run command root env)))

(defun +test/rerun ()
  "Re-run this frame's last test command."
  (interactive)
  (unless (+output-last-run 'test)
    (user-error "No previous test command"))
  (pcase-let ((`(,command ,directory ,env) (+output-last-run 'test)))
    (+test--run command directory env)))

(defalias 'test-nearest #'+test/nearest
  "Run the nearest test to point.")
(defalias 'test-file #'+test/file
  "Run tests for the current file.")
(defalias 'test-project #'+test/project
  "Run the current project's test suite.")
(defalias 'test-rerun #'+test/rerun
  "Re-run the last test command.")

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix test-menu ()
    "Test workflow."
    [["Run"
      ("n" "nearest" +test/nearest)
      ("N" "file" +test/file)
      ("p" "project" +test/project)
      ("T" "rerun last" +test/rerun)]]))

(define-leader-key!
  "r"   '(:ignore t :which-key "run")
  "c t" '(:def test-menu :which-key "tests")
  "c n" '(:def +test/nearest :which-key "nearest test")
  "c N" '(:def +test/file :which-key "file tests")
  "c p" '(:def +test/project :which-key "project tests")
  "c T" '(:def +test/rerun :which-key "rerun test"))

(define-leader-key!
  "r t" '(:def test-menu :which-key "tests")
  "r n" '(:def +test/nearest :which-key "nearest test")
  "r f" '(:def +test/file :which-key "file tests")
  "r p" '(:def +test/project :which-key "project tests")
  "r T" '(:def +test/rerun :which-key "rerun test"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! "\\*test\\*" :select t :align 'below :size +display-rules-bottom-size :regexp t)

(provide 'tools/test/config)
;;; tools/test/config.el ends here
