;;; tools/task/config.el --- Project task runner -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'aaron-funcs)
(require 'tools/output/config)
(require 'tools/project-local/config)
(require 'json)

(declare-function +direnv-update-environment-maybe "tools/direnv/config" (&optional path))
(declare-function +project-current-root "tools/project/config")
(declare-function remote-executable-find "remote-process" (program &optional context))

(defvar +direnv-subprocess-sync-inhibited)
(defvar transient--original-buffer)

(defgroup +task nil
  "Project task helpers."
  :group 'tools)

(defun +task--origin-buffer ()
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

(defun +task--call-in-origin-buffer (fn)
  "Call FN in the source buffer that initiated the current task command."
  (let ((buffer (+task--origin-buffer)))
    (if (eq buffer (current-buffer))
        (funcall fn)
      (if-let* ((window (get-buffer-window buffer (selected-frame))))
          (with-selected-window window
            (funcall fn))
        (with-current-buffer buffer
          (funcall fn))))))

(defun +task--command-path ()
  "Return the best path to derive direnv environment from."
  (or buffer-file-name default-directory))

(defun +task--command-simple-p (command)
  "Return non-nil when COMMAND is a simple executable invocation."
  (not (string-match-p "[;&|<>`$()\n]" command)))

(defun +task--resolve-command-executable (command)
  "Resolve COMMAND's executable to an absolute path when possible.
Callers bind `default-directory' to the task's target root before calling
this, so `remote-executable-find' resolves on that target rather than the
client machine and returns a target-native path safe to embed in COMMAND."
  (if (not (+task--command-simple-p command))
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

(defun +task-project-root ()
  "Return the best project root for task commands."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun +task--package-manager (root)
  "Return the preferred Node package manager for ROOT."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun +task--node-scripts (root)
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
                                (+task--package-manager root)
                                (shell-quote-argument (symbol-name (car entry))))))
                scripts)))))

(defun +task--make-targets (root file)
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

(defun +task--disambiguate-candidates (entries)
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

(defun +task--build-directories (root)
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

(defun +task-build-candidates ()
  "Return available build candidates as an alist of (LABEL . COMMAND)."
  (let* ((root (file-name-as-directory (expand-file-name (+task-project-root))))
         (makefiles '("Makefile" "makefile" "GNUmakefile"))
         (build-dirs (+task--build-directories root))
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
    (+project-local-merge-candidates 'build
                                       (delete-dups (nreverse pairs))
                                       root)))

(defun +task-candidates ()
  "Return available task candidates as an alist of (LABEL . COMMAND)."
  (let* ((root (file-name-as-directory (expand-file-name (+task-project-root))))
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
    (dolist (entry (+task--node-scripts root))
      (setq entries (append entries
                            (list (list "node" (car entry) (cdr entry))))))
    (setq pairs (+task--disambiguate-candidates entries))
    (dolist (target (+task--make-targets root "justfile"))
      (setq pairs (append pairs
                          (list (cons (format "just:%s" target)
                                      (format "just %s" target))))))
    (dolist (target (+task--make-targets root "Makefile"))
      (setq pairs (append pairs
                          (list (cons (format "make:%s" target)
                                      (format "make %s" target))))))
    (+project-local-merge-candidates 'task
                                       pairs
                                       root)))

(defun +task--run (label)
  "Run task LABEL selected from `+task-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (+task-project-root))))
         (command-path (+task--command-path))
         (command (or (cdr (assoc label (+task-candidates)))
                      (user-error "Unknown task: %s" label)))
         (env (+project-local-env 'task root)))
    (when (fboundp '+direnv-update-environment-maybe)
      (+direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (prog1 (+output-compile
              (+task--resolve-command-executable command) "task" env)
        (+output-record-run 'task command root env)))))

(defun +task/run (label)
  "Run task LABEL selected from `+task-candidates'."
  (interactive
   (+task--call-in-origin-buffer
    (lambda ()
      (let* ((candidates (+task-candidates))
             (labels (mapcar #'car candidates)))
        (unless candidates
          (user-error "No project tasks detected"))
        (list (completing-read "Task: " labels nil t))))))
  (+task--call-in-origin-buffer
   (lambda ()
     (+task--run label))))

(defun +task/rerun ()
  "Re-run this frame's last task."
  (interactive)
  (unless (+output-last-run 'task)
    (user-error "No previous task command"))
  (pcase-let ((`(,command ,directory ,env) (+output-last-run 'task)))
    (let ((default-directory directory)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (+output-compile (+task--resolve-command-executable command)
                         "task" env))))

(defun +task--build (label)
  "Run build LABEL selected from `+task-build-candidates'."
  (let* ((root (file-name-as-directory (expand-file-name (+task-project-root))))
         (command-path (+task--command-path))
         (command (or (cdr (assoc label (+task-build-candidates)))
                      (user-error "Unknown build: %s" label)))
         (env (+project-local-env 'build root)))
    (when (fboundp '+direnv-update-environment-maybe)
      (+direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (prog1 (+output-compile
              (+task--resolve-command-executable command) "build" env)
        (+output-record-run 'build command root env)))))

(defun +task/build (label)
  "Run build LABEL selected from `+task-build-candidates'."
  (interactive
   (+task--call-in-origin-buffer
    (lambda ()
      (let* ((candidates (+task-build-candidates))
             (labels (mapcar #'car candidates)))
        (unless candidates
          (user-error "No build commands detected"))
        (list (completing-read "Build: " labels nil t))))))
  (+task--call-in-origin-buffer
   (lambda ()
     (+task--build label))))

(defun +task/build-rerun ()
  "Re-run this frame's last build command."
  (interactive)
  (unless (+output-last-run 'build)
    (user-error "No previous build command"))
  (pcase-let ((`(,command ,directory ,env) (+output-last-run 'build)))
    (let ((default-directory directory)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (+output-compile (+task--resolve-command-executable command)
                         "build" env))))

(defalias 'task-run #'+task/run
  "Choose and run a task in the current project.")
(defalias 'task-rerun #'+task/rerun
  "Re-run this frame's last project task.")
(defalias 'task-build #'+task/build
  "Choose and run a build command in the current project.")
(defalias 'task-build-rerun #'+task/build-rerun
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

(define-leader-key!
  "r k" '(:def task-menu :which-key "tasks")
  "r K" '(:def task-rerun :which-key "rerun task"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! "\\*task\\*" :select t :align 'below :size +display-rules-bottom-size :regexp t)

(provide 'tools/task/config)
;;; tools/task/config.el ends here
