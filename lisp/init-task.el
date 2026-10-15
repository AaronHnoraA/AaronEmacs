;;; init-task.el --- Project task runner -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'init-output)
(require 'init-project-local)
(require 'json)

(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))
(declare-function my/project-current-root "init-project")
(declare-function remote-executable-find "remote-process" (program &optional context))

(defvar my/direnv-subprocess-sync-inhibited)
(defvar transient--original-buffer)

(defgroup my/task nil
  "Project task helpers."
  :group 'tools)

(defun my/task--origin-buffer ()
  "Return the source buffer that initiated the current task command."
  (cond
   ((and (boundp 'transient--original-buffer)
         (buffer-live-p transient--original-buffer))
    transient--original-buffer)
   ((and (active-minibuffer-window)
         (window-live-p (minibuffer-selected-window)))
    (window-buffer (minibuffer-selected-window)))
   (t
    (current-buffer))))

(defun my/task--call-in-origin-buffer (fn)
  "Call FN in the source buffer that initiated the current task command."
  (let ((buffer (my/task--origin-buffer)))
    (if (eq buffer (current-buffer))
        (funcall fn)
      (if-let* ((window (get-buffer-window buffer (selected-frame))))
          (with-selected-window window
            (funcall fn))
        (with-current-buffer buffer
          (funcall fn))))))

(defun my/task--command-path ()
  "Return the best path to derive direnv environment from."
  (or buffer-file-name default-directory))

(defun my/task--command-simple-p (command)
  "Return non-nil when COMMAND is a simple executable invocation."
  (not (string-match-p "[;&|<>`$()\n]" command)))

(defun my/task--resolve-command-executable (command)
  "Resolve COMMAND's executable to an absolute path when possible.
Callers bind `default-directory' to the task's target root before calling
this, so `remote-executable-find' resolves on that target rather than the
client machine and returns a target-native path safe to embed in COMMAND."
  (if (not (my/task--command-simple-p command))
      command
    (pcase-let* ((`(,program . ,_) (split-string-and-unquote command))
                 (executable (and program (remote-executable-find program))))
      (if executable
          (replace-regexp-in-string
           (format "\\`%s" (regexp-quote program))
           (shell-quote-argument executable)
           command
           t t)
        command))))

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

(defun my/task--disambiguate-candidates (entries)
  "Turn (SOURCE LABEL COMMAND) ENTRIES into distinct task choices.
Qualify LABEL with SOURCE only when more than one tool provides that label."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (cl-incf (gethash (cadr entry) counts 0)))
    (mapcar (lambda (entry)
              (pcase-let ((`(,source ,label ,command) entry))
                (cons (if (> (gethash label counts) 1)
                          (format "%s:%s" source label)
                        label)
                      command)))
            entries)))

(defun my/task--build-directories (root)
  "Return likely build directories in ROOT.
The list includes directories that already look configured for CMake or Ninja."
  (let ((candidates '("." "build" "cmake-build-debug" "cmake-build-release"
                      "cmake-build-relwithdebinfo" "out" "out/build"))
        result)
    (dolist (dir candidates (nreverse (delete-dups result)))
      (let ((path (expand-file-name dir root)))
        (when (and (file-directory-p path)
                   (or (file-exists-p (expand-file-name "build.ninja" path))
                       (file-exists-p (expand-file-name "CMakeCache.txt" path))))
          (push dir result))))))

(defun my/task-build-candidates ()
  "Return available build candidates as an alist of (LABEL . COMMAND)."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (makefiles '("Makefile" "makefile" "GNUmakefile"))
         (build-dirs (my/task--build-directories root))
         (pairs nil)
         (has-cmake (file-exists-p (expand-file-name "CMakeLists.txt" root)))
         (has-root-ninja (file-exists-p (expand-file-name "build.ninja" root))))
    (when (cl-some (lambda (file) (file-exists-p (expand-file-name file root)))
                   makefiles)
      (push '("make" . "make") pairs))
    (when has-root-ninja
      (push '("ninja" . "ninja") pairs))
    (dolist (dir build-dirs)
      (unless (equal dir ".")
        (let ((quoted (shell-quote-argument dir))
              (path (expand-file-name dir root)))
          (when (file-exists-p (expand-file-name "build.ninja" path))
            (push (cons (format "ninja:%s" dir)
                        (format "ninja -C %s" quoted))
                  pairs))
          (when (file-exists-p (expand-file-name "CMakeCache.txt" path))
            (push (cons (format "cmake:build:%s" dir)
                        (format "cmake --build %s" quoted))
                  pairs)))))
    (when has-cmake
      (if build-dirs
          (dolist (dir build-dirs)
            (let ((quoted (shell-quote-argument dir)))
              (push (cons (if (equal dir ".")
                              "cmake:build"
                            (format "cmake:build:%s" dir))
                          (format "cmake --build %s" quoted))
                    pairs)))
        ;; Bootstrap the default build dir on first use for simple CMake projects.
        (push '("cmake:build" . "cmake -S . -B build && cmake --build build")
              pairs)))
    (my/project-local-merge-candidates 'build
                                       (delete-dups (nreverse pairs))
                                       root)))

(defun my/task-candidates ()
  "Return available task candidates as an alist of (LABEL . COMMAND)."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (entries nil)
         (pairs nil))
    (when (file-exists-p (expand-file-name "Cargo.toml" root))
      (setq entries
            (append entries
                    '(("cargo" "build" "cargo build")
                      ("cargo" "run" "cargo run")
                      ("cargo" "test" "cargo test")
                      ("cargo" "bench" "cargo bench")
                      ("cargo" "clean" "cargo clean")))))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (setq entries
            (append entries
                    '(("go" "build" "go build ./...")
                      ("go" "run" "go run .")
                      ("go" "test" "go test ./...")
                      ("go" "clean" "go clean -cache")))))
    (when (cl-some (lambda (file) (file-exists-p (expand-file-name file root)))
                   '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg"))
      (setq entries
            (append entries
                    '(("python" "test" "pytest")
                      ("python" "lint" "ruff check .")
                      ("python" "format" "ruff format .")))))
    (dolist (entry (my/task--node-scripts root))
      (setq entries (append entries
                            (list (list "node" (car entry) (cdr entry))))))
    (setq pairs (my/task--disambiguate-candidates entries))
    (dolist (target (my/task--make-targets root "justfile"))
      (setq pairs (append pairs
                          (list (cons (format "just:%s" target)
                                      (format "just %s" target))))))
    (dolist (target (my/task--make-targets root "Makefile"))
      (setq pairs (append pairs
                          (list (cons (format "make:%s" target)
                                      (format "make %s" target))))))
    (my/project-local-merge-candidates 'task
                                       pairs
                                       root)))

(defun my/task--run (label)
  "Run task LABEL selected from `my/task-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (command-path (my/task--command-path))
         (command (or (cdr (assoc label (my/task-candidates)))
                      (user-error "Unknown task: %s" label)))
         (env (my/project-local-env 'task root)))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (prog1 (my/output-compile
              (my/task--resolve-command-executable command) "task" env)
        (my/output-record-run 'task command root env)))))

(defun my/task-run (label)
  "Run task LABEL selected from `my/task-candidates'."
  (interactive
   (my/task--call-in-origin-buffer
    (lambda ()
      (let* ((candidates (my/task-candidates))
             (labels (mapcar #'car candidates)))
        (unless candidates
          (user-error "No project tasks detected"))
        (list (completing-read "Task: " labels nil t))))))
  (my/task--call-in-origin-buffer
   (lambda ()
     (my/task--run label))))

(defun my/task-rerun ()
  "Re-run this frame's last task."
  (interactive)
  (unless (my/output-last-run 'task)
    (user-error "No previous task command"))
  (pcase-let ((`(,command ,directory ,env) (my/output-last-run 'task)))
    (let ((default-directory directory)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (my/output-compile (my/task--resolve-command-executable command)
                         "task" env))))

(defun my/task--build (label)
  "Run build LABEL selected from `my/task-build-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (command-path (my/task--command-path))
         (command (or (cdr (assoc label (my/task-build-candidates)))
                      (user-error "Unknown build: %s" label)))
         (env (my/project-local-env 'build root)))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (prog1 (my/output-compile
              (my/task--resolve-command-executable command) "build" env)
        (my/output-record-run 'build command root env)))))

(defun my/task-build (label)
  "Run build LABEL selected from `my/task-build-candidates'."
  (interactive
   (my/task--call-in-origin-buffer
    (lambda ()
      (let* ((candidates (my/task-build-candidates))
             (labels (mapcar #'car candidates)))
        (unless candidates
          (user-error "No build commands detected"))
        (list (completing-read "Build: " labels nil t))))))
  (my/task--call-in-origin-buffer
   (lambda ()
     (my/task--build label))))

(defun my/task-build-rerun ()
  "Re-run this frame's last build command."
  (interactive)
  (unless (my/output-last-run 'build)
    (user-error "No previous build command"))
  (pcase-let ((`(,command ,directory ,env) (my/output-last-run 'build)))
    (let ((default-directory directory)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (my/output-compile (my/task--resolve-command-executable command)
                         "build" env))))

(defalias 'task-run #'my/task-run
  "Choose and run a task in the current project.")
(defalias 'task-rerun #'my/task-rerun
  "Re-run this frame's last project task.")
(defalias 'task-build #'my/task-build
  "Choose and run a build command in the current project.")
(defalias 'task-build-rerun #'my/task-build-rerun
  "Re-run this frame's last project build.")

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix task-menu ()
    "Project tasks."
    [["Tasks"
      ("t" "choose task" task-run)
      ("r" "rerun last" task-rerun)]
     ["Build"
      ("b" "choose build" task-build)
      ("B" "rerun build" task-build-rerun)]
     ["Bazel"
      ("z" "target workbench" bazel-target-menu)]]))

(my/leader!
  "r k" '(:def task-menu :which-key "tasks")
  "r K" '(:def task-rerun :which-key "rerun task"))

(provide 'init-task)
;;; init-task.el ends here
