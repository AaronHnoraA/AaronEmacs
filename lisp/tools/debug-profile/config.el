;;; tools/debug-profile/config.el --- Project debug profiles for Dape -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)
(require 'tools/project-local/config)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function dape "dape" (config &optional skip-compile))
(declare-function +project-current-root "tools/project/config")

(defvar dape-configs)
(defvar-local dape-command nil)

(defgroup +debug-profile nil
  "Project debug profile helpers."
  :group 'tools)

(defconst +debug-profile--frame-history-parameter
  '+debug-profile-last-runs
  "Frame parameter storing debug configs by project root.")

(defun +debug-profile-root ()
  "Return the current project root for debug workflows."
  (file-name-as-directory
   (expand-file-name
    (or (and (fboundp '+project-current-root)
             (+project-current-root))
        (when-let* ((project (project-current nil default-directory)))
          (project-root project))
        default-directory))))

(defun +debug-profile--config-name (name)
  "Normalize debug config NAME into a symbol."
  (cond
   ((symbolp name) name)
   ((stringp name) (intern name))
   (t (user-error "Invalid debug configuration name: %S" name))))

(defun +debug-profile--plist-remove (plist prop)
  "Return PLIST without PROP."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key prop)
          (setq result (plist-put result key value)))))
    result))

(defun +debug-profile--alist-config (name)
  "Return a copy of dape config NAME."
  (or (copy-tree (alist-get name dape-configs))
      (user-error "Unknown Dape configuration: %s" name)))

(defun +debug-profile--merge-plists (base overrides)
  "Merge OVERRIDES plist into BASE plist."
  (let ((result (copy-tree base))
        (plist (copy-tree overrides)))
    (while plist
      (setq result (plist-put result (pop plist) (pop plist))))
    result))

(defun +debug-profile--spec->command (spec &optional root)
  "Convert debug SPEC into a `dape-command'-style list for ROOT."
  (let* ((root (or root (+debug-profile-root)))
         (spec (+project-local-resolve spec root)))
    (cond
     ((and (listp spec)
           (or (plist-member spec :base)
               (plist-member spec 'base)))
      (let ((base (or (plist-get spec :base)
                      (plist-get spec 'base)))
            (overrides (+debug-profile--plist-remove
                        (+debug-profile--plist-remove spec :base)
                        'base)))
        (cons (+debug-profile--config-name base) overrides)))
     ((and (consp spec)
           (or (symbolp (car spec))
               (stringp (car spec)))
           (not (keywordp (car spec))))
      (cons (+debug-profile--config-name (car spec))
            (copy-tree (cdr spec))))
     ((or (symbolp spec)
          (stringp spec))
      (list (+debug-profile--config-name spec)))
     ((and (listp spec)
           (or (plist-member spec :type)
               (plist-member spec :request)))
      spec)
     (t
      (user-error "Unsupported debug profile spec: %S" spec)))))

(defun +debug-profile--env-plist (env)
  "Convert ENV alist into a plist accepted by Dape."
  (let (plist)
    (dolist (entry env plist)
      (when (consp entry)
        (setq plist
              (plist-put plist
                         (intern (format ":%s" (car entry)))
                         (cdr entry)))))))

(defun +debug-profile--config-with-env (config root)
  "Merge project-local debug env for ROOT into CONFIG."
  (let ((env (+project-local-env 'debug root)))
    (if env
        (let* ((config (copy-tree config))
               (merged (+debug-profile--merge-plists
                        (plist-get config :env)
                        (+debug-profile--env-plist env))))
          (plist-put config :env merged))
      config)))

(defun +debug-profile--spec->config (spec &optional root)
  "Resolve debug SPEC into a Dape config plist for ROOT."
  (let* ((root (or root (+debug-profile-root)))
         (command (+debug-profile--spec->command spec root))
         (config (if (and (listp command)
                          (or (plist-member command :type)
                              (plist-member command :request)))
                     (copy-tree command)
                   (let ((base (+debug-profile--alist-config (car command))))
                     (+debug-profile--merge-plists base (cdr command))))))
    (+debug-profile--config-with-env config root)))

(defun +debug-profile--builtin-candidates ()
  "Return Dape configs matching the current buffer as (LABEL . SPEC)."
  (when (require 'dape nil t)
    (cl-loop for (name . config) in dape-configs
             for modes = (plist-get config 'modes)
             when (or (null modes)
                      (apply #'derived-mode-p modes))
             collect (cons (symbol-name name) name))))

(defun +debug-profile-candidates ()
  "Return debug profile candidates for the current project."
  (let* ((root (+debug-profile-root))
         (base (append
                (when dape-command
                  (list (cons "buffer-local dape-command"
                              (copy-tree dape-command))))
                (+debug-profile--builtin-candidates))))
    (+project-local-merge-candidates 'debug base root)))

(defun +debug-profile--default-spec (&optional root)
  "Return the default debug SPEC for ROOT when configured."
  (let* ((root (or root (+debug-profile-root)))
         (default (+project-local-value :debug-default root)))
    (cond
     (default
      (if (and (stringp default)
               (assoc default (+debug-profile-candidates)))
          (cdr (assoc default (+debug-profile-candidates)))
        default))
     (t
      (let ((local-debug (+project-local-value :debug root)))
        (when (and (listp local-debug)
                   (= (length local-debug) 1))
          (cdar local-debug)))))))

(defun +debug-profile-apply-default ()
  "Populate `dape-command' from project-local defaults when helpful."
  (when (and (derived-mode-p 'prog-mode)
             (not (local-variable-p 'dape-command)))
    (when-let* ((spec (+debug-profile--default-spec))
                (command (+debug-profile--spec->command spec)))
      (unless (keywordp (car-safe command))
        (setq-local dape-command command)))))

(defun +debug-profile--record-run (frame root config)
  "Remember CONFIG for ROOT in FRAME after Dape accepts a launch."
  (when (frame-live-p frame)
    (let ((previous (frame-parameter
                     frame +debug-profile--frame-history-parameter)))
      (set-frame-parameter
       frame +debug-profile--frame-history-parameter
       (cons (cons root (copy-tree config))
             (assoc-delete-all root (copy-sequence previous)))))))

(defun +debug-profile--last-run (frame root)
  "Return FRAME's last debug config for ROOT, if any."
  (cdr (assoc root
              (frame-parameter frame +debug-profile--frame-history-parameter))))

(defun +debug-profile/run (name)
  "Run debug profile NAME with Dape."
  (interactive
   (let ((candidates (+debug-profile-candidates)))
     (unless candidates
       (user-error "No debug profiles detected"))
     (list (completing-read "Debug profile: "
                            (mapcar #'car candidates)
                            nil t))))
  (let* ((frame (selected-frame))
         (root (+debug-profile-root))
         (spec (or (cdr (assoc name (+debug-profile-candidates)))
                   (user-error "Unknown debug profile: %s" name)))
         (config (+debug-profile--spec->config spec root)))
    (let ((default-directory root))
      (prog1 (dape (copy-tree config))
        (+debug-profile--record-run frame root config)))))

(defun +debug-profile/rerun ()
  "Re-run this frame's last debug profile for the current project."
  (interactive)
  (let* ((root (+debug-profile-root))
         (config (+debug-profile--last-run (selected-frame) root)))
    (unless config
      (user-error "No previous debug profile for this project in this frame"))
    (let ((default-directory root))
      (dape (copy-tree config)))))

(transient-define-prefix debug-profile-menu ()
  "Debug profile workflow."
  [["Debug"
    ("d" "choose profile" +debug-profile/run)
    ("r" "rerun last" +debug-profile/rerun)
    ("D" "raw dape" dape)]])

(add-hook 'prog-mode-hook #'+debug-profile-apply-default)

(define-leader-key!
  "r d" '(:def debug-profile-menu :which-key "debug profile")
  "r D" '(:def +debug-profile/rerun :which-key "rerun debug profile"))

(provide 'tools/debug-profile/config)
;;; tools/debug-profile/config.el ends here
