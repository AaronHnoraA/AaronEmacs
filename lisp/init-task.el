;;; init-task.el --- Project task runner -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'init-project-local)
(require 'json)

(declare-function my/project-current-root "init-project")

(defgroup my/task nil
  "Project task helpers."
  :group 'tools)

(defvar my/task-last-command nil
  "Last project task command executed.")

(defvar my/task-last-directory nil
  "Directory where the last project task ran.")

(defvar my/task-last-environment nil
  "Environment used by the last project task.")

(defun my/task-project-root ()
  "Return the best project root for task commands."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun my/task--package-manager (root)
  "Return the preferred Node package manager for ROOT."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun my/task--node-scripts (root)
  "Return package.json scripts from ROOT as an alist."
  (let ((file (expand-file-name "package.json" root)))
    (when (file-readable-p file)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (scripts (alist-get 'scripts (json-read-file file))))
        (mapcar (lambda (entry)
                  (cons (symbol-name (car entry))
                        (format "%s run %s"
                                (my/task--package-manager root)
                                (shell-quote-argument (symbol-name (car entry))))))
                scripts)))))

(defun my/task--make-targets (root file)
  "Return simple target candidates from FILE in ROOT."
  (let ((path (expand-file-name file root))
        targets)
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Za-z0-9_.-]+\\):\\(?:\\s-\\|$\\)" nil t)
          (let ((target (match-string 1)))
            (unless (string-prefix-p "." target)
              (push target targets))))))
    (nreverse (delete-dups targets))))

(defun my/task-candidates ()
  "Return available task candidates as an alist of (LABEL . COMMAND)."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (pairs nil))
    (when (file-exists-p (expand-file-name "Cargo.toml" root))
      (setq pairs (append pairs '(("build" . "cargo build")
                                  ("run" . "cargo run")
                                  ("test" . "cargo test")
                                  ("bench" . "cargo bench")
                                  ("clean" . "cargo clean")))))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (setq pairs (append pairs '(("build" . "go build ./...")
                                  ("run" . "go run .")
                                  ("test" . "go test ./...")
                                  ("clean" . "go clean -cache")))))
    (when (cl-some (lambda (file) (file-exists-p (expand-file-name file root)))
                   '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg"))
      (setq pairs (append pairs '(("test" . "pytest")
                                  ("lint" . "ruff check .")
                                  ("format" . "ruff format .")))))
    (dolist (entry (my/task--node-scripts root))
      (push entry pairs))
    (dolist (target (my/task--make-targets root "justfile"))
      (push (cons (format "just:%s" target) (format "just %s" target)) pairs))
    (dolist (target (my/task--make-targets root "Makefile"))
      (push (cons (format "make:%s" target) (format "make %s" target)) pairs))
    (my/project-local-merge-candidates 'task
                                       (delete-dups (nreverse pairs))
                                       root)))

(defun my/task-run (label)
  "Run task LABEL selected from `my/task-candidates'."
  (interactive
   (let* ((candidates (my/task-candidates))
          (labels (mapcar #'car candidates)))
     (unless candidates
       (user-error "No project tasks detected"))
     (list (completing-read "Task: " labels nil t))))
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (command (or (cdr (assoc label (my/task-candidates)))
                      (user-error "Unknown task: %s" label)))
         (env (my/project-local-env 'task root)))
    (setq my/task-last-command command
          my/task-last-directory root
          my/task-last-environment env)
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (compilation-buffer-name-function (lambda (_) "*task*")))
      (compile command))))

(defun my/task-rerun ()
  "Re-run the last task."
  (interactive)
  (unless (and my/task-last-command my/task-last-directory)
    (user-error "No previous task command"))
  (let ((default-directory my/task-last-directory)
        (process-environment (my/project-local-apply-env
                              my/task-last-environment))
        (compilation-buffer-name-function (lambda (_) "*task*")))
    (compile my/task-last-command)))

(use-package transient
  :ensure nil
  :defer t
  :config
  (transient-define-prefix my/task-dispatch ()
    "Project tasks."
    [["Tasks"
      ("t" "choose task" my/task-run)
      ("r" "rerun last" my/task-rerun)]]))

(my/evil-global-leader-set "r k" #'my/task-dispatch "tasks")
(my/evil-global-leader-set "r K" #'my/task-rerun "rerun task")

(provide 'init-task)
;;; init-task.el ends here
