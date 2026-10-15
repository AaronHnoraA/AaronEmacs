;;; init-project-run-tests.el --- Project run profile integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-project-run)

(defmacro project-run-test-with-root (&rest body)
  "Run BODY in an isolated project directory."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "project-run-test-" t)))
          (default-directory root)
          (my/project-run-profile-alist nil)
          (my/project-local-overrides nil)
          (my/project-local-settings nil))
     (unwind-protect
         (cl-letf (((symbol-function 'my/project-current-root)
                    (lambda () root)))
           ,@body)
       (delete-directory root t))))

(ert-deftest project-run-commands-are-searchable-and-bound-consistently ()
  "Direct actions, menus and shortcuts use searchable commands."
  (dolist (command '(project-run project-rerun project-run-menu
                     task-run task-rerun task-build task-build-rerun task-menu))
    (should (commandp command))
    (should (member (symbol-name command)
                    (all-completions (symbol-name command) obarray #'commandp))))
  (should (eq (key-binding (kbd "<f5>")) #'project-run-menu))
  (should (eq (key-binding (kbd "H-R")) #'project-run-menu)))

(ert-deftest project-run-only-offers-real-scripts-and-file-targets ()
  "A file or a word outside a script definition is not a runnable profile."
  (project-run-test-with-root
    (with-temp-file (expand-file-name "package.json" root)
      (insert "{\"description\":\"dev serve preview\",\"scripts\":{\"start\":\"node app.js\"}}"))
    (with-temp-file (expand-file-name "Makefile" root)
      (insert "all:\n\t@true\n"))
    (with-temp-file (expand-file-name "justfile" root)
      (insert "lint:\n  echo ok\n"))
    (should (equal (my/project-run-profiles)
                   '(("start" . "npm run start"))))
    (should (equal (cdr (assoc "start" (my/task-candidates)))
                   "npm run start"))))

(ert-deftest project-run-discovers-scripts-through-logical-local-target ()
  "Project command discovery accepts the Remote framework's local identity."
  (project-run-test-with-root
    (with-temp-file (expand-file-name "package.json" root)
      (insert "{\"scripts\":{\"dev\":\"node app.js\"}}"))
    (let* ((logical (concat "/fs:local:" root))
           (default-directory logical))
      (cl-letf (((symbol-function 'my/project-current-root)
                 (lambda () logical)))
        (should (equal (my/project-run-profiles)
                       '(("dev" . "npm run dev"))))
        (should (equal (cdr (assoc "dev" (my/task-candidates)))
                       "npm run dev"))))))

(ert-deftest project-run-one-command-per-label-with-explicit-precedence ()
  "Local overrides beat configured profiles, which beat detected commands."
  (project-run-test-with-root
    (with-temp-file (expand-file-name "package.json" root)
      (insert "{\"scripts\":{\"run\":\"node app.js\",\"dev\":\"vite\"}}"))
    (with-temp-file (expand-file-name "Makefile" root)
      (insert "run:\n\t@true\n"))
    (with-temp-file (expand-file-name "Cargo.toml" root)
      (insert "[package]\nname = \"example\"\n"))
    (setq my/project-run-profile-alist
          `((,root . (("run" . "custom run")))))
    (let ((profiles (my/project-run-profiles)))
      (should (equal (cdr (assoc "run" profiles)) "custom run"))
      (should (= 1 (cl-count "run" profiles :key #'car :test #'equal))))
    (setq my/project-local-settings '(:run (("run" . "local run"))))
    (let ((profiles (my/project-run-profiles)))
      (should (equal (cdr (assoc "run" profiles)) "local run"))
      (should (= 1 (cl-count "run" profiles :key #'car :test #'equal))))))

(ert-deftest project-task-mixed-repository-keeps-every-run-target-selectable ()
  "A shared label must name its provider instead of hiding other commands."
  (project-run-test-with-root
    (dolist (file '("Cargo.toml" "go.mod" "pyproject.toml"))
      (with-temp-file (expand-file-name file root)
        (insert "present\n")))
    (with-temp-file (expand-file-name "package.json" root)
      (insert "{\"scripts\":{\"run\":\"node app.js\",\"test\":\"vitest\"}}"))
    (with-temp-file (expand-file-name "Makefile" root)
      (insert "run:\n\t@true\n"))
    (with-temp-file (expand-file-name "justfile" root)
      (insert "run:\n  echo ok\n"))
    (let ((choices (my/task-candidates)))
      (should (equal (cdr (assoc "cargo:run" choices)) "cargo run"))
      (should (equal (cdr (assoc "go:run" choices)) "go run ."))
      (should (equal (cdr (assoc "node:run" choices)) "npm run run"))
      (should (equal (cdr (assoc "python:test" choices)) "pytest"))
      (should (equal (cdr (assoc "just:run" choices)) "just run"))
      (should (equal (cdr (assoc "make:run" choices)) "make run"))
      (should (= (length choices)
                 (length (delete-dups (mapcar #'car choices))))))))

(ert-deftest project-run-uses-transient-origin-and-project-environment ()
  "A menu opened from a source buffer runs in that buffer's project and env."
  (project-run-test-with-root
    (with-temp-file (expand-file-name "package.json" root)
      (insert "{\"scripts\":{\"dev\":\"node app.js\"}}"))
    (let ((source (generate-new-buffer " *project-run-source*"))
          (recorded nil)
          (launched nil)
          (direnv-path nil))
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq default-directory root))
            (cl-letf (((symbol-function 'my/direnv-update-environment-maybe)
                       (lambda (path) (setq direnv-path path)))
                      ((symbol-function 'my/task--resolve-command-executable)
                       #'identity)
                      ((symbol-function 'my/output-compile)
                       (lambda (command kind environment)
                         (setq launched (list command kind default-directory
                                              (getenv "RUN_TOKEN") environment))
                         'started))
                      ((symbol-function 'my/output-record-run)
                       (lambda (&rest args) (setq recorded args)))
                      ((symbol-function 'my/project-local-env)
                       (lambda (&rest _) '(("RUN_TOKEN" . "value")))))
              (let ((transient--original-buffer source))
                (with-temp-buffer
                  (my/project-run "dev"))))
            (should (equal launched
                           (list "npm run dev" "run" root "value"
                                 '(("RUN_TOKEN" . "value")))))
            (should (equal direnv-path root))
            (should (equal recorded
                           (list 'run "npm run dev" root
                                 '(("RUN_TOKEN" . "value"))))))
        (kill-buffer source)))))

(ert-deftest project-run-failed-launch-preserves-this-frames-rerun ()
  "A failed compilation must not replace the frame's previous run profile."
  (project-run-test-with-root
    (let ((previous (frame-parameter nil 'my/output-last-runs)))
      (unwind-protect
          (progn
            (my/output-record-run 'run "previous command" root nil)
            (cl-letf (((symbol-function 'my/project-run-profiles)
                       (lambda () '(("dev" . "new command"))))
                      ((symbol-function 'my/task--resolve-command-executable)
                       #'identity)
                      ((symbol-function 'my/output-compile)
                       (lambda (&rest _) (error "Cannot start process"))))
              (should-error (my/project-run--run "dev"))
              (should (equal (my/output-last-run 'run)
                             (list "previous command" root nil)))))
        (set-frame-parameter nil 'my/output-last-runs previous)))))

(provide 'init-project-run-tests)
;;; init-project-run-tests.el ends here
