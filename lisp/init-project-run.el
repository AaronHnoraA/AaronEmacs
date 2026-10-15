;;; init-project-run.el --- Project run profiles -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'init-funcs)
(require 'init-output)
(require 'init-project-local)
(require 'init-task)
(require 'seq)

(declare-function my/project-current-root "init-project")
(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))

(defvar my/direnv-subprocess-sync-inhibited)

(defgroup my/project-run nil
  "Project run profile helpers."
  :group 'tools)

(config-defvar my/project-run-profile-alist nil
  "Optional custom run profiles.
Each entry is of the form (MATCHER . ((NAME . COMMAND) ...)).
MATCHER can be a directory path or regexp matched against the project root."
  :type '(alist :key-type string :value-type (repeat (cons string string)))
  :group 'my/project-run)

(defun my/project-run-root ()
  "Return the current project root."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun my/project-run--custom-profiles (root)
  "Return custom run profiles matching ROOT."
  (seq-mapcat
   (lambda (entry)
     (pcase-let ((`(,matcher . ,profiles) entry))
       (when (or (string= (expand-file-name matcher) (expand-file-name root))
                 (string-match-p matcher root))
         profiles)))
   my/project-run-profile-alist))

(defconst my/project-run--target-names
  '("dev" "run" "serve" "start" "preview")
  "Conventional task names offered as run profiles.")

(defun my/project-run--append-unique (profiles candidates)
  "Append CANDIDATES to PROFILES, keeping the first command for each label."
  (dolist (entry candidates profiles)
    (unless (assoc (car entry) profiles)
      (setq profiles (append profiles (list entry))))))

(defun my/project-run-profiles ()
  "Return run profiles for the current project."
  (let* ((root (file-name-as-directory (expand-file-name (my/project-run-root))))
         (profiles (my/project-run--append-unique nil
                                                  (my/project-run--custom-profiles root))))
    ;; Task discovery owns parsing of package scripts and file targets.  A
    ;; Makefile or justfile alone is not evidence that a run target exists.
    (setq profiles
          (my/project-run--append-unique
           profiles
           (seq-filter (lambda (entry)
                         (member (car entry) my/project-run--target-names))
                       (my/task--node-scripts root))))
    (dolist (file '("justfile" "Makefile"))
      (setq profiles
            (my/project-run--append-unique
             profiles
             (mapcar (lambda (target)
                       (cons target
                             (format "%s %s"
                                     (if (equal file "justfile") "just" "make")
                                     (shell-quote-argument target))))
                     (seq-filter (lambda (target)
                                   (member target my/project-run--target-names))
                                 (my/task--make-targets root file))))))
    (when (file-exists-p (expand-file-name "Cargo.toml" root))
      (setq profiles (my/project-run--append-unique
                      profiles '(("run" . "cargo run")))))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (setq profiles (my/project-run--append-unique
                      profiles '(("run" . "go run .")))))
    (when (file-exists-p (expand-file-name "manage.py" root))
      (setq profiles (my/project-run--append-unique
                      profiles '(("dev" . "python manage.py runserver")))))
    (when (file-exists-p (expand-file-name "main.py" root))
      (setq profiles (my/project-run--append-unique
                      profiles '(("run" . "python main.py")))))
    (my/project-local-merge-candidates 'run
                                       profiles
                                       root)))

(defun my/project-run--run (name)
  "Run project profile NAME in the source buffer's project."
  (let* ((root (file-name-as-directory (expand-file-name (my/project-run-root))))
         (command-path (my/task--command-path))
         (command (or (cdr (assoc name (my/project-run-profiles)))
                      (user-error "Unknown run profile: %s" name)))
         (env (my/project-local-env 'run root)))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (prog1 (my/output-compile
              (my/task--resolve-command-executable command) "run" env)
        (my/output-record-run 'run command root env)))))

(defun my/project-run (name)
  "Run project profile NAME."
  (interactive
   (my/task--call-in-origin-buffer
    (lambda ()
      (let ((profiles (my/project-run-profiles)))
        (unless profiles
          (user-error "No run profiles detected"))
        (list (completing-read "Run profile: " (mapcar #'car profiles) nil t))))))
  (my/task--call-in-origin-buffer
   (lambda () (my/project-run--run name))))

(defun my/project-run-rerun ()
  "Re-run this frame's last project profile."
  (interactive)
  (unless (my/output-last-run 'run)
    (user-error "No previous run profile"))
  (pcase-let ((`(,command ,directory ,env) (my/output-last-run 'run)))
    (let ((default-directory directory)
          (process-environment (my/project-local-apply-env env))
          (my/direnv-subprocess-sync-inhibited t))
      (my/output-compile (my/task--resolve-command-executable command)
                         "run" env))))

(defalias 'project-run #'my/project-run
  "Choose and run a profile in the current project.")
(defalias 'project-rerun #'my/project-run-rerun
  "Re-run this frame's last project profile.")

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix project-run-menu ()
    "Project run profiles."
    [["Run"
      ("r" "choose profile" project-run)
      ("R" "rerun last" project-rerun)]]))

(my/leader!
  "r r" '(:def project-run-menu :which-key "run profile")
  "r R" '(:def project-rerun :which-key "rerun profile"))

(provide 'init-project-run)
;;; init-project-run.el ends here
