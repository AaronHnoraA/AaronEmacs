;;; tools/run/config.el --- Project run profiles -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)
(require 'tools/output/config)
(require 'tools/project-local/config)
(require 'tools/task/config)
(require 'seq)

(declare-function +project-current-root "tools/project/config")
(declare-function +direnv-update-environment-maybe "tools/direnv/config" (&optional path))

(defvar +direnv-subprocess-sync-inhibited)

(defgroup +project/run nil
  "Project run profile helpers."
  :group 'tools)

(config-defvar +project-run-profile-alist nil
  "Optional custom run profiles.
Each entry is of the form (MATCHER . ((NAME . COMMAND) ...)).
MATCHER can be a directory path or regexp matched against the project root."
  :type '(alist :key-type string :value-type (repeat (cons string string)))
  :group '+project/run)

(defun +project-run-root ()
  "Return the current project root."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      (when-let* ((project (project-current nil default-directory)))
        (project-root project))
      default-directory))

(defun +project-run--custom-profiles (root)
  "Return custom run profiles matching ROOT."
  (seq-mapcat
   (lambda (entry)
     (pcase-let ((`(,matcher . ,profiles) entry))
       (when (or (string= (expand-file-name matcher) (expand-file-name root))
                 (string-match-p matcher root))
         profiles)))
   +project-run-profile-alist))

(defconst +project-run--target-names
  '("dev" "run" "serve" "start" "preview")
  "Conventional task names offered as run profiles.")

(defun +project-run--append-unique (profiles candidates)
  "Append CANDIDATES to PROFILES, keeping the first command for each label."
  (dolist (entry candidates profiles)
    (unless (assoc (car entry) profiles)
      (setq profiles (append profiles (list entry))))))

(defun +project-run-profiles ()
  "Return run profiles for the current project."
  (let* ((root (file-name-as-directory (expand-file-name (+project-run-root))))
         (profiles (+project-run--append-unique nil
                                                  (+project-run--custom-profiles root))))
    ;; Task discovery owns parsing of package scripts and file targets.  A
    ;; Makefile or justfile alone is not evidence that a run target exists.
    (setq profiles
          (+project-run--append-unique
           profiles
           (seq-filter (lambda (entry)
                         (member (car entry) +project-run--target-names))
                       (+task--node-scripts root))))
    (dolist (file '("justfile" "Makefile"))
      (setq profiles
            (+project-run--append-unique
             profiles
             (mapcar (lambda (target)
                       (cons target
                             (format "%s %s"
                                     (if (equal file "justfile") "just" "make")
                                     (shell-quote-argument target))))
                     (seq-filter (lambda (target)
                                   (member target +project-run--target-names))
                                 (+task--make-targets root file))))))
    (when (file-exists-p (expand-file-name "Cargo.toml" root))
      (setq profiles (+project-run--append-unique
                      profiles '(("run" . "cargo run")))))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (setq profiles (+project-run--append-unique
                      profiles '(("run" . "go run .")))))
    (when (file-exists-p (expand-file-name "manage.py" root))
      (setq profiles (+project-run--append-unique
                      profiles '(("dev" . "python manage.py runserver")))))
    (when (file-exists-p (expand-file-name "main.py" root))
      (setq profiles (+project-run--append-unique
                      profiles '(("run" . "python main.py")))))
    (+project-local-merge-candidates 'run
                                       profiles
                                       root)))

(defun +project-run--run (name)
  "Run project profile NAME in the source buffer's project."
  (let* ((root (file-name-as-directory (expand-file-name (+project-run-root))))
         (command-path (+task--command-path))
         (command (or (cdr (assoc name (+project-run-profiles)))
                      (user-error "Unknown run profile: %s" name)))
         (env (+project-local-env 'run root)))
    (when (fboundp '+direnv-update-environment-maybe)
      (+direnv-update-environment-maybe command-path))
    (let ((default-directory root)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (prog1 (+output-compile
              (+task--resolve-command-executable command) "run" env)
        (+output-record-run 'run command root env)))))

(defun +project/run (name)
  "Run project profile NAME."
  (interactive
   (+task--call-in-origin-buffer
    (lambda ()
      (let ((profiles (+project-run-profiles)))
        (unless profiles
          (user-error "No run profiles detected"))
        (list (completing-read "Run profile: " (mapcar #'car profiles) nil t))))))
  (+task--call-in-origin-buffer
   (lambda () (+project-run--run name))))

(defun +project-run/rerun ()
  "Re-run this frame's last project profile."
  (interactive)
  (unless (+output-last-run 'run)
    (user-error "No previous run profile"))
  (pcase-let ((`(,command ,directory ,env) (+output-last-run 'run)))
    (let ((default-directory directory)
          (process-environment (+project-local-apply-env env))
          (+direnv-subprocess-sync-inhibited t))
      (+output-compile (+task--resolve-command-executable command)
                         "run" env))))

(defalias 'project-run #'+project/run
  "Choose and run a profile in the current project.")
(defalias 'project-rerun #'+project-run/rerun
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

(define-leader-key!
  "r r" '(:def project-run-menu :which-key "run profile")
  "r R" '(:def project-rerun :which-key "rerun profile"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! "\\*run\\*" :select t :align 'below :size +display-rules-bottom-size :regexp t)

(provide 'tools/run/config)
;;; tools/run/config.el ends here
