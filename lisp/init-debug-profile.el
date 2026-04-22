;;; init-debug-profile.el --- Project debug profiles for Dape -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-project-local)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function dape "dape" (config &optional skip-compile))
(declare-function my/project-current-root "init-project")

(defvar dape-configs)
(defvar-local dape-command nil)

(defgroup my/debug-profile nil
  "Project debug profile helpers."
  :group 'tools)

(defvar my/debug-profile-last-config nil
  "Last Dape config plist started by `my/debug-profile-run'.")

(defvar my/debug-profile-last-directory nil
  "Directory where the last debug profile started.")

(defun my/debug-profile-root ()
  "Return the current project root for debug workflows."
  (file-name-as-directory
   (expand-file-name
    (or (and (fboundp 'my/project-current-root)
             (my/project-current-root))
        (when-let* ((project (project-current nil default-directory)))
          (project-root project))
        default-directory))))

(defun my/debug-profile--config-name (name)
  "Normalize debug config NAME into a symbol."
  (cond
   ((symbolp name) name)
   ((stringp name) (intern name))
   (t (user-error "Invalid debug configuration name: %S" name))))

(defun my/debug-profile--plist-remove (plist prop)
  "Return PLIST without PROP."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key prop)
          (setq result (plist-put result key value)))))
    result))

(defun my/debug-profile--alist-config (name)
  "Return a copy of dape config NAME."
  (or (copy-tree (alist-get name dape-configs))
      (user-error "Unknown Dape configuration: %s" name)))

(defun my/debug-profile--merge-plists (base overrides)
  "Merge OVERRIDES plist into BASE plist."
  (let ((result (copy-tree base))
        (plist (copy-tree overrides)))
    (while plist
      (setq result (plist-put result (pop plist) (pop plist))))
    result))

(defun my/debug-profile--spec->command (spec &optional root)
  "Convert debug SPEC into a `dape-command'-style list for ROOT."
  (let* ((root (or root (my/debug-profile-root)))
         (spec (my/project-local-resolve spec root)))
    (cond
     ((and (listp spec)
           (or (plist-member spec :base)
               (plist-member spec 'base)))
      (let ((base (or (plist-get spec :base)
                      (plist-get spec 'base)))
            (overrides (my/debug-profile--plist-remove
                        (my/debug-profile--plist-remove spec :base)
                        'base)))
        (cons (my/debug-profile--config-name base) overrides)))
     ((and (consp spec)
           (or (symbolp (car spec))
               (stringp (car spec)))
           (not (keywordp (car spec))))
      (cons (my/debug-profile--config-name (car spec))
            (copy-tree (cdr spec))))
     ((or (symbolp spec)
          (stringp spec))
      (list (my/debug-profile--config-name spec)))
     ((and (listp spec)
           (or (plist-member spec :type)
               (plist-member spec :request)))
      spec)
     (t
      (user-error "Unsupported debug profile spec: %S" spec)))))

(defun my/debug-profile--env-plist (env)
  "Convert ENV alist into a plist accepted by Dape."
  (let (plist)
    (dolist (entry env plist)
      (when (consp entry)
        (setq plist
              (plist-put plist
                         (intern (format ":%s" (car entry)))
                         (cdr entry)))))))

(defun my/debug-profile--config-with-env (config root)
  "Merge project-local debug env for ROOT into CONFIG."
  (let ((env (my/project-local-env 'debug root)))
    (if env
        (let* ((config (copy-tree config))
               (merged (my/debug-profile--merge-plists
                        (plist-get config :env)
                        (my/debug-profile--env-plist env))))
          (plist-put config :env merged))
      config)))

(defun my/debug-profile--spec->config (spec &optional root)
  "Resolve debug SPEC into a Dape config plist for ROOT."
  (let* ((root (or root (my/debug-profile-root)))
         (command (my/debug-profile--spec->command spec root))
         (config (if (and (listp command)
                          (or (plist-member command :type)
                              (plist-member command :request)))
                     (copy-tree command)
                   (let ((base (my/debug-profile--alist-config (car command))))
                     (my/debug-profile--merge-plists base (cdr command))))))
    (my/debug-profile--config-with-env config root)))

(defun my/debug-profile--builtin-candidates ()
  "Return Dape configs matching the current buffer as (LABEL . SPEC)."
  (when (require 'dape nil t)
    (cl-loop for (name . config) in dape-configs
             for modes = (plist-get config 'modes)
             when (or (null modes)
                      (apply #'derived-mode-p modes))
             collect (cons (symbol-name name) name))))

(defun my/debug-profile-candidates ()
  "Return debug profile candidates for the current project."
  (let* ((root (my/debug-profile-root))
         (base (append
                (when dape-command
                  (list (cons "buffer-local dape-command"
                              (copy-tree dape-command))))
                (my/debug-profile--builtin-candidates))))
    (my/project-local-merge-candidates 'debug base root)))

(defun my/debug-profile--default-spec (&optional root)
  "Return the default debug SPEC for ROOT when configured."
  (let* ((root (or root (my/debug-profile-root)))
         (default (my/project-local-value :debug-default root)))
    (cond
     (default
      (if (and (stringp default)
               (assoc default (my/debug-profile-candidates)))
          (cdr (assoc default (my/debug-profile-candidates)))
        default))
     (t
      (let ((local-debug (my/project-local-value :debug root)))
        (when (and (listp local-debug)
                   (= (length local-debug) 1))
          (cdar local-debug)))))))

(defun my/debug-profile-apply-default ()
  "Populate `dape-command' from project-local defaults when helpful."
  (when (and (derived-mode-p 'prog-mode)
             (not (local-variable-p 'dape-command)))
    (when-let* ((spec (my/debug-profile--default-spec))
                (command (my/debug-profile--spec->command spec)))
      (unless (keywordp (car-safe command))
        (setq-local dape-command command)))))

(defun my/debug-profile-run (name)
  "Run debug profile NAME with Dape."
  (interactive
   (let ((candidates (my/debug-profile-candidates)))
     (unless candidates
       (user-error "No debug profiles detected"))
     (list (completing-read "Debug profile: "
                            (mapcar #'car candidates)
                            nil t))))
  (let* ((root (my/debug-profile-root))
         (spec (or (cdr (assoc name (my/debug-profile-candidates)))
                   (user-error "Unknown debug profile: %s" name)))
         (config (my/debug-profile--spec->config spec root)))
    (setq my/debug-profile-last-config config
          my/debug-profile-last-directory root)
    (let ((default-directory root))
      (dape config))))

(defun my/debug-profile-rerun ()
  "Re-run the last debug profile."
  (interactive)
  (unless (and my/debug-profile-last-config my/debug-profile-last-directory)
    (user-error "No previous debug profile"))
  (let ((default-directory my/debug-profile-last-directory))
    (dape (copy-tree my/debug-profile-last-config))))

(transient-define-prefix my/debug-profile-dispatch ()
  "Debug profile workflow."
  [["Debug"
    ("d" "choose profile" my/debug-profile-run)
    ("r" "rerun last" my/debug-profile-rerun)
    ("D" "raw dape" dape)]])

(add-hook 'prog-mode-hook #'my/debug-profile-apply-default)

(my/leader!
  "r d" '(:def my/debug-profile-dispatch :which-key "debug profile")
  "r D" '(:def my/debug-profile-rerun :which-key "rerun debug profile"))

(provide 'init-debug-profile)
;;; init-debug-profile.el ends here
