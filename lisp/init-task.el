;;; init-task.el --- Project task runner -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'init-project-local)
(require 'json)

(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))
(declare-function my/project-current-root "init-project")

(defvar my/direnv-subprocess-sync-inhibited)
(defvar transient--original-buffer)

(defgroup my/task nil
  "Project task helpers."
  :group 'tools)

(defvar my/task-last-command nil
  "Last project task command executed.")

(defvar my/task-last-directory nil
  "Directory where the last project task ran.")

(defvar my/task-last-environment nil
  "Environment used by the last project task.")
(defvar my/task-build-last-command nil
  "Last build command executed.")

(defvar my/task-build-last-directory nil
  "Directory where the last build command ran.")

(defvar my/task-build-last-environment nil
  "Environment used by the last build command.")

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
      (if-let* ((window (get-buffer-window buffer t)))
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
  "Resolve COMMAND's executable to an absolute path when possible."
  (if (not (my/task--command-simple-p command))
      command
    (pcase-let* ((`(,program . ,_) (split-string-and-unquote command))
                 (executable (and program (executable-find program))))
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

(defun my/task--run (label)
  "Run task LABEL selected from `my/task-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (command-path (my/task--command-path))
         (command (or (cdr (assoc label (my/task-candidates)))
                      (user-error "Unknown task: %s" label)))
         (env (my/project-local-env 'task root)))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe command-path))
    (setq my/task-last-command command
          my/task-last-directory root
          my/task-last-environment env)
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (compilation-buffer-name-function (lambda (_) "*task*"))
          (my/direnv-subprocess-sync-inhibited t))
      (compile (my/task--resolve-command-executable command)))))

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
  "Re-run the last task."
  (interactive)
  (unless (and my/task-last-command my/task-last-directory)
    (user-error "No previous task command"))
  (let ((default-directory my/task-last-directory)
        (process-environment (my/project-local-apply-env
                              my/task-last-environment))
        (compilation-buffer-name-function (lambda (_) "*task*"))
        (my/direnv-subprocess-sync-inhibited t))
    (compile (my/task--resolve-command-executable my/task-last-command))))

(defun my/task--build (label)
  "Run build LABEL selected from `my/task-build-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (my/task-project-root))))
         (command-path (my/task--command-path))
         (command (or (cdr (assoc label (my/task-build-candidates)))
                      (user-error "Unknown build: %s" label)))
         (env (my/project-local-env 'build root)))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe command-path))
    (setq my/task-build-last-command command
          my/task-build-last-directory root
          my/task-build-last-environment env)
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (compilation-buffer-name-function (lambda (_) "*build*"))
          (my/direnv-subprocess-sync-inhibited t))
      (compile (my/task--resolve-command-executable command)))))

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
  "Re-run the last build command."
  (interactive)
  (unless (and my/task-build-last-command my/task-build-last-directory)
    (user-error "No previous build command"))
  (let ((default-directory my/task-build-last-directory)
        (process-environment (my/project-local-apply-env
                              my/task-build-last-environment))
        (compilation-buffer-name-function (lambda (_) "*build*"))
        (my/direnv-subprocess-sync-inhibited t))
    (compile (my/task--resolve-command-executable my/task-build-last-command))))

(use-package transient
  :ensure nil
  :defer t
  :config
  (transient-define-prefix my/task-dispatch ()
    "Project tasks."
    [["Tasks"
      ("t" "choose task" my/task-run)
      ("r" "rerun last" my/task-rerun)]
     ["Build"
      ("b" "choose build" my/task-build)
      ("B" "rerun build" my/task-build-rerun)]]))

(my/leader!
  "r k" '(:def my/task-dispatch :which-key "tasks")
  "r K" '(:def my/task-rerun :which-key "rerun task"))

(provide 'init-task)
;;; init-task.el ends here
