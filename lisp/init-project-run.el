;;; init-project-run.el --- Project run profiles -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-project-local)

(declare-function my/project-current-root "init-project")

(defgroup my/project-run nil
  "Project run profile helpers."
  :group 'tools)

(defcustom my/project-run-profile-alist nil
  "Optional custom run profiles.
Each entry is of the form (MATCHER . ((NAME . COMMAND) ...)).
MATCHER can be a directory path or regexp matched against the project root."
  :type '(alist :key-type string :value-type (repeat (cons string string)))
  :group 'my/project-run)

(defvar my/project-run-last-command nil
  "Last run profile command.")

(defvar my/project-run-last-directory nil
  "Directory where the last run profile executed.")

(defvar my/project-run-last-environment nil
  "Environment used by the last run profile.")

(defun my/project-run-root ()
  "Return the current project root."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun my/project-run--package-manager (root)
  "Return the preferred Node package manager for ROOT."
  (cond
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun my/project-run--custom-profiles (root)
  "Return custom run profiles matching ROOT."
  (seq-mapcat
   (lambda (entry)
     (pcase-let ((`(,matcher . ,profiles) entry))
       (when (or (string= (expand-file-name matcher) (expand-file-name root))
                 (string-match-p matcher root))
         profiles)))
   my/project-run-profile-alist))

(defun my/project-run-profiles ()
  "Return run profiles for the current project."
  (let* ((root (file-name-as-directory (expand-file-name (my/project-run-root))))
         (profiles (my/project-run--custom-profiles root)))
    (when (file-exists-p (expand-file-name "Cargo.toml" root))
      (setq profiles (append profiles '(("run" . "cargo run")))))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (setq profiles (append profiles '(("run" . "go run .")))))
    (when (file-exists-p (expand-file-name "manage.py" root))
      (setq profiles (append profiles '(("dev" . "python manage.py runserver")))))
    (when (file-exists-p (expand-file-name "main.py" root))
      (setq profiles (append profiles '(("run" . "python main.py")))))
    (when (file-exists-p (expand-file-name "package.json" root))
      (let ((pm (my/project-run--package-manager root)))
        (dolist (entry '(("dev" . "dev")
                         ("start" . "start")
                         ("serve" . "serve")
                         ("preview" . "preview")))
          (when (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "package.json" root))
                    (search-forward (format "\"%s\"" (car entry)) nil t)))
            (push (cons (car entry)
                        (format "%s run %s" pm (cdr entry)))
                  profiles)))))
    (when (file-exists-p (expand-file-name "justfile" root))
      (dolist (entry '(("dev" . "just dev")
                       ("run" . "just run")
                       ("serve" . "just serve")
                       ("start" . "just start")))
        (push entry profiles)))
    (when (file-exists-p (expand-file-name "Makefile" root))
      (dolist (entry '(("dev" . "make dev")
                       ("run" . "make run")
                       ("serve" . "make serve")
                       ("start" . "make start")))
        (push entry profiles)))
    (my/project-local-merge-candidates 'run
                                       (delete-dups (nreverse profiles))
                                       root)))

(defun my/project-run (name)
  "Run project profile NAME."
  (interactive
   (let ((profiles (my/project-run-profiles)))
     (unless profiles
       (user-error "No run profiles detected"))
     (list (completing-read "Run profile: " (mapcar #'car profiles) nil t))))
  (let* ((root (file-name-as-directory (expand-file-name (my/project-run-root))))
         (command (or (cdr (assoc name (my/project-run-profiles)))
                      (user-error "Unknown run profile: %s" name)))
         (env (my/project-local-env 'run root)))
    (setq my/project-run-last-command command
          my/project-run-last-directory root
          my/project-run-last-environment env)
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (compilation-buffer-name-function (lambda (_) "*run*")))
      (compile command))))

(defun my/project-run-rerun ()
  "Re-run the last project profile."
  (interactive)
  (unless (and my/project-run-last-command my/project-run-last-directory)
    (user-error "No previous run profile"))
  (let ((default-directory my/project-run-last-directory)
        (process-environment (my/project-local-apply-env
                              my/project-run-last-environment))
        (compilation-buffer-name-function (lambda (_) "*run*")))
    (compile my/project-run-last-command)))

(use-package transient
  :ensure nil
  :defer t
  :config
  (transient-define-prefix my/project-run-dispatch ()
    "Project run profiles."
    [["Run"
      ("r" "choose profile" my/project-run)
      ("R" "rerun last" my/project-run-rerun)]]))

(my/evil-global-leader-set "r r" #'my/project-run-dispatch "run profile")
(my/evil-global-leader-set "r R" #'my/project-run-rerun "rerun profile")

(provide 'init-project-run)
;;; init-project-run.el ends here
