;;; init-test.el --- Project test workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'init-project-local)
(require 'subr-x)

(declare-function my/project-current-root "init-project")

(defgroup my/test nil
  "Test helpers."
  :group 'tools)

(defvar my/test-last-command nil
  "Last test command executed.")

(defvar my/test-last-directory nil
  "Directory where the last test command ran.")

(defvar my/test-last-environment nil
  "Environment used by the last test command.")

(defun my/test-project-root ()
  "Return the best project root for test commands."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun my/test--package-manager (root)
  "Return the preferred Node package manager for ROOT."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun my/test--runner (root)
  "Return a test runner symbol for ROOT."
  (cond
   ((file-exists-p (expand-file-name "Cargo.toml" root)) 'cargo)
   ((cl-some (lambda (file) (file-exists-p (expand-file-name file root)))
             '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg"))
    'pytest)
   ((file-exists-p (expand-file-name "go.mod" root)) 'go)
   ((file-exists-p (expand-file-name "package.json" root)) 'node)
   ((file-exists-p (expand-file-name "justfile" root)) 'just)
   ((file-exists-p (expand-file-name "Makefile" root)) 'make)
   (t nil)))

(defun my/test--current-defun ()
  "Return the current defun name when possible."
  (or (when (fboundp 'which-function)
        (which-function))
      (ignore-errors
        (add-log-current-defun))))

(defun my/test--python-node-id ()
  "Return a pytest node id suffix for point when possible."
  (when-let* ((name (my/test--current-defun)))
    (mapconcat #'identity (split-string name "\\." t) "::")))

(defun my/test--go-package (root file)
  "Return the Go package path relative to ROOT for FILE."
  (let* ((dir (file-name-directory file))
         (rel (file-relative-name dir root)))
    (if (equal rel "./")
        "./..."
      (concat "./" (directory-file-name rel)))))

(defun my/test--file-relative (root)
  "Return the current buffer file name relative to ROOT."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (file-relative-name buffer-file-name root))

(defun my/test--command (scope)
  "Build a test command for SCOPE."
  (let* ((root (file-name-as-directory (expand-file-name (my/test-project-root))))
         (runner (my/test--runner root))
         (file (and buffer-file-name (expand-file-name buffer-file-name)))
         (relfile (and file (file-relative-name file root)))
         (defun-name (my/test--current-defun))
         (pm (my/test--package-manager root))
         (override (my/project-local-test-command scope root))
         (env (my/project-local-env 'test root)))
    (list
     root
     (or override
         (progn
           (unless runner
             (user-error "No supported test runner detected in %s" root))
           (pcase (list runner scope)
             (`(cargo nearest)
              (if (and defun-name (not (string-empty-p defun-name)))
                  (format "cargo test %s" (shell-quote-argument defun-name))
                "cargo test"))
             (`(cargo ,_) "cargo test")
             (`(pytest nearest)
              (if-let* ((node-id (and file (my/test--python-node-id))))
                  (format "pytest %s::%s"
                          (shell-quote-argument relfile)
                          (shell-quote-argument node-id))
                (format "pytest %s" (shell-quote-argument (or relfile ".")))))
             (`(pytest file)
              (format "pytest %s" (shell-quote-argument (or relfile "."))))
             (`(pytest ,_) "pytest")
             (`(go nearest)
              (if (and defun-name file)
                  (format "go test %s -run '^%s$'"
                          (shell-quote-argument (my/test--go-package root file))
                          (replace-regexp-in-string "'" "'\\''" defun-name))
                (format "go test %s"
                        (shell-quote-argument
                         (if file (my/test--go-package root file) "./...")))))
             (`(go file)
              (format "go test %s"
                      (shell-quote-argument
                       (if file (my/test--go-package root file) "./..."))))
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

(defun my/test--run (command root &optional env)
  "Run test COMMAND from ROOT."
  (setq my/test-last-command command
        my/test-last-directory root
        my/test-last-environment env)
  (let ((default-directory root)
        (process-environment (my/project-local-apply-env env))
        (compilation-buffer-name-function (lambda (_) "*test*")))
    (compile command)))

(defun my/test-nearest ()
  "Run the nearest test to point."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (my/test--command 'nearest)))
    (my/test--run command root env)))

(defun my/test-file ()
  "Run tests for the current file."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (my/test--command 'file)))
    (my/test--run command root env)))

(defun my/test-project ()
  "Run the project test suite."
  (interactive)
  (pcase-let ((`(,root ,command ,env) (my/test--command 'project)))
    (my/test--run command root env)))

(defun my/test-rerun ()
  "Re-run the last test command."
  (interactive)
  (unless (and my/test-last-command my/test-last-directory)
    (user-error "No previous test command"))
  (my/test--run my/test-last-command
                my/test-last-directory
                my/test-last-environment))

(use-package transient
  :ensure nil
  :defer t
  :config
  (transient-define-prefix my/test-dispatch ()
    "Test workflow."
    [["Run"
      ("n" "nearest" my/test-nearest)
      ("N" "file" my/test-file)
      ("p" "project" my/test-project)
      ("T" "rerun last" my/test-rerun)]]))

(my/leader-key-label "r" "run")
(my/evil-global-leader-set "c t" #'my/test-dispatch "tests")
(my/evil-global-leader-set "c n" #'my/test-nearest "nearest test")
(my/evil-global-leader-set "c N" #'my/test-file "file tests")
(my/evil-global-leader-set "c p" #'my/test-project "project tests")
(my/evil-global-leader-set "c T" #'my/test-rerun "rerun test")

(my/evil-global-leader-set "r t" #'my/test-dispatch "tests")
(my/evil-global-leader-set "r n" #'my/test-nearest "nearest test")
(my/evil-global-leader-set "r f" #'my/test-file "file tests")
(my/evil-global-leader-set "r p" #'my/test-project "project tests")
(my/evil-global-leader-set "r T" #'my/test-rerun "rerun test")

(provide 'init-test)
;;; init-test.el ends here
