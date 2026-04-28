;;; init-project.el --- Project navigation and integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-treemacs-bridge)

(eval-when-compile
  (require 'treemacs-workspaces nil t))

(eval-and-compile
  (my/treemacs-bridge-declarations))

(defvar projectile-known-projects)
(defvar project--list)
(defvar dashboard-projects-backend)
(defvar dashboard-projects-switch-function)
(defvar winner-mode)
(defvar winner-currents)
(defvar winner-pending-undo-ring)
(defvar winner-ring-alist)
(defvar winner-undo-frame)

(declare-function project--ensure-read-project-list "project")
(declare-function project--write-project-list "project")
(declare-function dashboard-projects-backend-load-projects "dashboard-widgets")
(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))
(declare-function get-current-persp "perspective")
(declare-function persp-parameter "perspective" (parameter &optional persp))
(declare-function projectile-find-file-in-directory "projectile")
(declare-function projectile-ignored-project-p "projectile")
(declare-function projectile-known-projects "projectile")
(declare-function projectile-load-known-projects "projectile")
(declare-function projectile-merge-known-projects "projectile")
(declare-function projectile-relevant-known-projects "projectile")
(declare-function projectile-save-known-projects "projectile")
(declare-function set-persp-parameter "perspective" (parameter value &optional persp))

(defgroup my/project nil
  "Project workflow helpers."
  :group 'convenience)

(defcustom my/project-search-paths nil
  "Project discovery roots for Projectile.
Entries follow the same shape as `projectile-project-search-path': either a
directory string or a cons cell of the form (DIRECTORY . DEPTH)."
  :type '(repeat
          (choice
           (directory :tag "Directory")
           (cons :tag "Directory + depth"
                 (directory :tag "Directory")
                 (integer :tag "Depth"))))
  :group 'my/project)

(defcustom my/project-import-project-el-entries nil
  "Whether `project.el' entries may flow back into the manual project list."
  :type 'boolean
  :group 'my/project)

(defconst my/project-state-dir
  (file-name-as-directory
   (expand-file-name "project"
                     (or (and (boundp 'my/state-dir) my/state-dir)
                         (expand-file-name "var" user-emacs-directory)))))

(defconst my/project-ignored-roots-file
  (expand-file-name "ignored-roots.el" my/project-state-dir))

(defconst my/project-list-file
  (expand-file-name "projects.eld" my/project-state-dir))

(make-directory my/project-state-dir t)

(setq project-list-file my/project-list-file)

(defun my/project-normalize-root (project-root)
  "Return PROJECT-ROOT as a normalized directory name."
  (when (stringp project-root)
    (file-name-as-directory (expand-file-name project-root))))

(defun my/project-canonical-path (path)
  "Return PATH in a stable canonical form."
  (when (stringp path)
    (let* ((expanded (expand-file-name path))
           (canonical (if (file-remote-p expanded)
                          expanded
                        (or (ignore-errors (file-truename expanded))
                            expanded))))
      (directory-file-name canonical))))

(defun my/project-root-equal-p (left right)
  "Return non-nil when LEFT and RIGHT refer to the same project root."
  (equal (my/project-canonical-path left)
         (my/project-canonical-path right)))

(defun my/project-hidden-root-p (project-root)
  "Return non-nil when PROJECT-ROOT should stay out of project UIs."
  (when (stringp project-root)
    (let ((project-root (my/project-normalize-root project-root)))
      (and project-root
           (or (my/project-ignored-root-p project-root)
               (and (require 'projectile nil t)
                    (projectile-ignored-project-p project-root)))))))

(defun my/project-load-ignored-roots ()
  "Return project roots suppressed from the custom project workflow."
  (when (file-exists-p my/project-ignored-roots-file)
    (with-temp-buffer
      (insert-file-contents my/project-ignored-roots-file)
      (let ((data (read (current-buffer))))
        (when (proper-list-p data)
          data)))))

(defvar my/project-ignored-roots
  (or (my/project-load-ignored-roots) nil)
  "Project roots hidden from the custom project workflow.")

(defun my/project-save-ignored-roots ()
  "Persist `my/project-ignored-roots' to disk."
  (with-temp-file my/project-ignored-roots-file
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp (mapcar #'my/project-normalize-root my/project-ignored-roots)
          (current-buffer)))))

(defun my/project-ignored-root-p (project-root)
  "Return non-nil when PROJECT-ROOT is suppressed from the workflow."
  (seq-some
   (lambda (ignored-root)
     (my/project-root-equal-p ignored-root project-root))
   my/project-ignored-roots))

(defun my/project-ignore-root (project-root)
  "Suppress PROJECT-ROOT from the custom project workflow."
  (unless (my/project-ignored-root-p project-root)
    (push (my/project-normalize-root project-root) my/project-ignored-roots)
    (my/project-save-ignored-roots)))

(defun my/project-unignore-root (project-root)
  "Re-enable PROJECT-ROOT in the custom project workflow."
  (let ((before (length my/project-ignored-roots)))
    (setq my/project-ignored-roots
          (seq-remove
           (lambda (ignored-root)
             (my/project-root-equal-p ignored-root project-root))
           my/project-ignored-roots))
    (unless (= before (length my/project-ignored-roots))
      (my/project-save-ignored-roots)
      t)))

(defun my/project-project-object-ignored-p (project)
  "Return non-nil when PROJECT should be excluded from `project.el' lists."
  (my/project-hidden-root-p (project-root project)))

(defun my/project-path-inside-root-p (path project-root)
  "Return non-nil when PATH belongs to PROJECT-ROOT."
  (when-let* ((path (my/project-canonical-path path))
              (root (file-name-as-directory
                     (my/project-canonical-path project-root))))
    (or (equal path (directory-file-name root))
        (file-in-directory-p path root))))

(defun my/project-name (project-root)
  "Return a display name for PROJECT-ROOT."
  (let* ((project-root (directory-file-name (my/project-normalize-root project-root)))
         (name (and (boundp 'projectile-project-name-function)
                    (functionp projectile-project-name-function)
                    (ignore-errors
                      (funcall projectile-project-name-function project-root)))))
    (if (and (stringp name) (not (equal name "")))
        name
      (file-name-nondirectory project-root))))

(defun my/project-perspective-name (project-root)
  "Return a perspective-safe name for PROJECT-ROOT.
If multiple known projects share the same basename, add the parent directory to
keep perspective names unique."
  (let* ((project-root (directory-file-name (my/project-normalize-root project-root)))
         (base-name (my/project-name project-root))
         (parent-name (file-name-nondirectory
                       (directory-file-name (file-name-directory project-root))))
         (has-duplicate
          (seq-some
           (lambda (other-root)
             (let ((other-root (directory-file-name other-root)))
               (and (not (equal other-root project-root))
                    (equal (my/project-name other-root) base-name))))
           (my/project-known-projects))))
    (if has-duplicate
        (format "%s@%s" base-name parent-name)
      base-name)))

(defun my/project-current-root ()
  "Return the current project root, or nil when outside any project."
  (let ((root
         (or (ignore-errors
               (when-let* ((projectile-root (projectile-project-root)))
                 (my/project-normalize-root projectile-root)))
             (when (fboundp 'project-current)
               (when-let* ((project (project-current nil default-directory))
                           (project-root (project-root project)))
                 (my/project-normalize-root project-root))))))
    (unless (my/project-hidden-root-p root)
      root)))

(defun my/project-sync-roots-from-project-el ()
  "Sync visible `project.el' roots into Projectile known projects."
  (when (and my/project-import-project-el-entries
             (require 'project nil t)
             (require 'projectile nil t))
    (projectile-known-projects)
    (project--ensure-read-project-list)
    (let ((changed nil))
      (dolist (project-root (project-known-project-roots))
        (let ((project-root (my/project-normalize-root project-root)))
          (unless (or (my/project-hidden-root-p project-root)
                      (seq-some
                       (lambda (known-root)
                         (my/project-root-equal-p known-root project-root))
                       projectile-known-projects))
            (push project-root projectile-known-projects)
            (setq changed t))))
      (when changed
        (setq projectile-known-projects
              (seq-uniq (mapcar #'my/project-normalize-root projectile-known-projects)))
        (projectile-merge-known-projects)))
    projectile-known-projects))

(defun my/project-known-projects ()
  "Return known Projectile projects that still exist."
  (when (require 'projectile nil t)
    (seq-filter
     (lambda (project-root)
       (and (file-directory-p project-root)
            (not (my/project-hidden-root-p project-root))))
     (mapcar #'my/project-normalize-root
             (projectile-relevant-known-projects)))))

(defun my/dashboard-projects-load-projects (orig-fn &rest args)
  "Use the custom Projectile project list for dashboard projects."
  (if (eq dashboard-projects-backend 'projectile)
      (or (my/project-known-projects) nil)
    (apply orig-fn args)))

(defun my/project-read-known-project (&optional prompt)
  "Read one known project root using PROMPT."
  (let ((projects (my/project-known-projects)))
    (unless projects
      (user-error "Projectile does not know any projects yet"))
    (projectile-completing-read (or prompt "Project: ")
                                projects
                                :caller 'projectile-read-project)))

(defun my/project-read-target-root (&optional prompt)
  "Return the current project root, or ask for one using PROMPT.
With a prefix argument, always prompt."
  (or (and (not current-prefix-arg)
           (my/project-current-root))
      (my/project-read-known-project prompt)))

(defun my/project-switch-perspective (project-root)
  "Switch to the perspective associated with PROJECT-ROOT."
  (when (fboundp 'persp-switch)
    (persp-switch (my/project-perspective-name project-root))))

(defun my/project-save-winner-data-h (&rest _)
  "Persist `winner-mode' state into the active perspective."
  (when (and (bound-and-true-p winner-mode)
             (fboundp 'get-current-persp)
             (get-current-persp))
    (set-persp-parameter
     'winner-ring
     (list winner-currents
           winner-ring-alist
           winner-pending-undo-ring))))

(defun my/project-load-winner-data-h (&rest _)
  "Restore `winner-mode' state from the active perspective."
  (when (bound-and-true-p winner-mode)
    (pcase-let ((`(,currents ,alist ,pending)
                 (or (persp-parameter 'winner-ring)
                     (list nil nil nil))))
      (setq winner-undo-frame nil
            winner-currents currents
            winner-ring-alist alist
            winner-pending-undo-ring pending))))

(defmacro my/with-project-root-context (project-root &rest body)
  "Evaluate BODY with PROJECT-ROOT bound as the active Projectile root."
  (declare (indent 1) (debug t))
  `(let* ((project-root (my/project-normalize-root ,project-root))
          (default-directory project-root)
          (projectile-project-root project-root))
     (when (fboundp 'my/direnv-update-environment-maybe)
       (my/direnv-update-environment-maybe project-root))
     ,@body))

(defun my/project-switch (project-root &optional arg)
  "Switch to PROJECT-ROOT, optionally using Projectile commander with ARG."
  (interactive (list (my/project-read-known-project "Switch to project: ")
                     current-prefix-arg))
  (setq project-root (my/project-normalize-root project-root))
  (my/project-switch-perspective project-root)
  (my/with-project-root-context project-root
    (projectile-switch-project-by-name project-root arg)))

(defun my/project-ensure-treemacs ()
  "Load Treemacs integrations needed for project-aware navigation."
  (or (require 'treemacs nil t)
      (user-error "Treemacs is unavailable"))
  (require 'treemacs-projectile nil t)
  (require 'treemacs-evil nil t))

(defun my/project-open-workbench (project-root &optional arg)
  "Open PROJECT-ROOT as a full workbench.
This switches perspective and runs Projectile's switch action.
With ARG, Projectile uses commander mode."
  (interactive (list (my/project-read-known-project "Open project workbench: ")
                     current-prefix-arg))
  (setq project-root (my/project-normalize-root project-root))
  (my/project-switch project-root arg))

(defun my/project-find-file (project-root)
  "Find a file in PROJECT-ROOT.
Without a prefix argument, default to the current project."
  (interactive (list (my/project-read-target-root "Find file in project: ")))
  (my/with-project-root-context project-root
    (projectile-find-file-in-directory project-root)))

(defun my/project-recent-file (project-root)
  "Open a recently visited file in PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "Recent file in project: ")))
  (my/with-project-root-context project-root
    (projectile-recentf)))

(defun my/project-switch-buffer (project-root)
  "Switch to a buffer that belongs to PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "Buffer in project: ")))
  (my/with-project-root-context project-root
    (projectile-switch-to-buffer)))

(defun my/project-ripgrep (project-root)
  "Run `consult-ripgrep' in PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "Ripgrep in project: ")))
  (setq project-root (my/project-normalize-root project-root))
  (when (fboundp 'my/direnv-update-environment-maybe)
    (my/direnv-update-environment-maybe project-root))
  (consult-ripgrep project-root))

(defun my/project-open-root (project-root)
  "Open PROJECT-ROOT in Dired/Dirvish."
  (interactive (list (my/project-read-target-root "Open project root: ")))
  (setq project-root (my/project-normalize-root project-root))
  (when (fboundp 'my/direnv-update-environment-maybe)
    (my/direnv-update-environment-maybe project-root))
  (dired project-root))

(defun my/project-magit-status (project-root)
  "Open Magit status for PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "Magit project: ")))
  (setq project-root (my/project-normalize-root project-root))
  (when (fboundp 'my/direnv-update-environment-maybe)
    (my/direnv-update-environment-maybe project-root))
  (let ((default-directory project-root))
    (magit-status-setup-buffer project-root)))

(defun my/project-vterm (project-root)
  "Open or switch to a dedicated VTerm for PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "VTerm project: ")))
  (let* ((project-root (my/project-normalize-root project-root))
         (buffer-name (format "*vterm:%s*" (my/project-perspective-name project-root)))
         (default-directory project-root))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe project-root))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (vterm buffer-name))))

(defun my/project-kill-buffers (project-root)
  "Kill buffers belonging to PROJECT-ROOT."
  (interactive (list (my/project-read-target-root "Kill buffers in project: ")))
  (let ((default-directory (my/project-normalize-root project-root)))
    (projectile-kill-buffers)))

(defun my/project-add-known-project (project-root)
  "Register PROJECT-ROOT across the project workflow."
  (interactive "DAdd project: ")
  (setq project-root (my/project-register-root project-root))
  (message "Registered project %s in Projectile/project.el"
           (abbreviate-file-name project-root)))

(defun my/project-remove-from-projectile-known-projects (project-root)
  "Remove PROJECT-ROOT from `projectile-known-projects'.
This matches canonically, so symlinked roots are cleaned as well."
  (let ((before (length projectile-known-projects)))
    (setq projectile-known-projects
          (seq-remove
           (lambda (known-root)
             (my/project-root-equal-p known-root project-root))
           projectile-known-projects))
    (unless (= before (length projectile-known-projects))
      (projectile-merge-known-projects)
      t)))

(defun my/project-remove-from-project-list (project-root)
  "Remove PROJECT-ROOT from the built-in `project.el' project list."
  (when (require 'project nil t)
    (project--ensure-read-project-list)
    (let ((before (length project--list)))
      (setq project--list
            (seq-remove
             (lambda (entry)
               (let ((root (cond
                            ((stringp entry) entry)
                            ((stringp (car-safe entry)) (car-safe entry))
                            ((and (listp entry)
                                  (stringp (car-safe (last entry))))
                             (car (last entry)))
                            (t nil))))
                 (and root (my/project-root-equal-p root project-root))))
             project--list))
      (unless (= before (length project--list))
        (project--write-project-list)
        t))))

(defun my/project-treemacs-project-name (project-root &optional workspace)
  "Return a Treemacs-safe display name for PROJECT-ROOT in WORKSPACE."
  (let* ((workspace (or workspace (treemacs-current-workspace)))
         (base-name (my/project-perspective-name project-root))
         (candidate base-name)
         (index 2))
    (while (seq-some
            (lambda (project)
              (and (not (my/project-root-equal-p (treemacs-project->path project)
                                                 project-root))
                   (string= candidate (treemacs-project->name project))))
            (treemacs-workspace->projects workspace))
      (setq candidate (format "%s<%d>" base-name index)
            index (1+ index)))
    candidate))

(defun my/project-add-to-treemacs-workspace (project-root)
  "Add PROJECT-ROOT to the current Treemacs workspace if needed."
  (when (require 'treemacs-workspaces nil t)
    (require 'treemacs-persistence nil t)
    (let* ((project-root (my/project-normalize-root project-root))
           (workspace (treemacs-current-workspace))
           (path (my/project-canonical-path project-root)))
      (if (seq-some
           (lambda (project)
             (my/project-root-equal-p (treemacs-project->path project) path))
           (treemacs-workspace->projects workspace))
          nil
        (pcase (treemacs-do-add-project-to-workspace
                path
                (my/project-treemacs-project-name project-root workspace))
          (`(success ,_project) t)
          (`(duplicate-project ,_project) nil)
          (`(includes-project ,_project) nil)
          (`(duplicate-name ,_project) nil)
          (`(invalid-path ,reason)
           (message "Treemacs did not register %s: %s"
                    (abbreviate-file-name project-root)
                    reason)
           nil)
          (`(invalid-name ,name)
           (message "Treemacs rejected project name %s for %s"
                    name
                    (abbreviate-file-name project-root))
           nil))))))

(defun my/project-register-root (project-root)
  "Register PROJECT-ROOT across project backends."
  (setq project-root (my/project-normalize-root project-root))
  (unless (require 'projectile nil t)
    (user-error "Projectile is unavailable"))
  (unless (file-directory-p project-root)
    (user-error "%s is not a directory" (abbreviate-file-name project-root)))
  (let ((default-directory project-root))
    (unless (projectile-project-p project-root)
      ;; Projectile caches "rootless" directories. If a directory becomes a
      ;; project (e.g. `git init` / `git clone`) while Emacs is running, that
      ;; cached miss will make subsequent checks incorrectly return nil until the
      ;; cache is cleared.
      (when (fboundp 'projectile-invalidate-cache)
        (projectile-invalidate-cache nil))
      (unless (projectile-project-p project-root)
        (user-error "%s is not recognized as a project root (try `M-x projectile-invalidate-cache` or restart Emacs)"
                    (abbreviate-file-name project-root)))))
  (my/project-unignore-root project-root)
  (projectile-add-known-project project-root)
  (when (require 'project nil t)
    (project--remember-dir project-root))
  project-root)

(defun my/project--filter-visible-roots (roots)
  "Return ROOTS without entries that should stay hidden from project UIs."
  (seq-uniq
   (seq-remove #'my/project-hidden-root-p
               (mapcar #'my/project-normalize-root roots))))

(defun my/project-prune-hidden-project-state ()
  "Remove ignored roots such as `~/` from persisted project state."
  (interactive)
  (when (require 'projectile nil t)
    (projectile-load-known-projects)
    (let ((filtered (my/project--filter-visible-roots projectile-known-projects)))
      (unless (equal filtered projectile-known-projects)
        (setq projectile-known-projects filtered)
        (projectile-merge-known-projects))))
  (when (require 'project nil t)
    (project--ensure-read-project-list)
    (let ((filtered
           (seq-remove
            (lambda (entry)
              (let ((root (cond
                           ((stringp entry) entry)
                           ;; Historical format: ("/path/")
                           ((stringp (car-safe entry)) (car-safe entry))
                           ;; Be tolerant of formats like (vc Git "/path/") etc.
                           ((and (listp entry)
                                 (stringp (car-safe (last entry))))
                            (car (last entry)))
                           (t nil))))
                (and root (my/project-hidden-root-p root))))
            project--list)))
      (unless (equal filtered project--list)
        (setq project--list filtered)
        (project--write-project-list)))))

(defun my/project--remember-dir-visible-only (orig-fn root &rest args)
  "Only let ORIG-FN remember ROOT when it is explicitly managed here.

Emacs 31 `project--remember-dir' takes optional arguments (NO-WRITE STABLE)."
  ;; Be defensive: some callers may pass non-string roots (e.g. backend tags).
  (let* ((root (and (stringp root) (my/project-normalize-root root)))
         (managed-p
          (or (null root)
              (my/project-hidden-root-p root)
              (and (require 'projectile nil t)
                   (projectile-known-projects)
                   (seq-some
                    (lambda (known-root)
                      (my/project-root-equal-p known-root root))
                    projectile-known-projects)))))
    (when managed-p
      (apply orig-fn root args))))

(defun my/show-imenu-target-root ()
  "Return the directory root used by `show-imenu'."
  (my/project-normalize-root
   (or (my/project-current-root) default-directory)))

(defun my/show-imenu-enable-treemacs-modes ()
  "Enable Treemacs features used for project and symbol following."
  (my/project-ensure-treemacs)
  (or (require 'treemacs-project-follow-mode nil t)
      (user-error "Treemacs project follow is unavailable"))
  (or (require 'treemacs-tag-follow-mode nil t)
      (user-error "Treemacs tag follow is unavailable"))
  (or (require 'treemacs-follow-mode nil t)
      (user-error "Treemacs follow mode is unavailable"))
  (when (bound-and-true-p treemacs-tag-follow-mode)
    (treemacs-tag-follow-mode -1))
  (when (bound-and-true-p treemacs-follow-mode)
    (treemacs-follow-mode -1))
  (my/treemacs-reset-follow-state)
  (treemacs-project-follow-mode 1)
  (my/treemacs-cursor-follow-mode 1))

(defun my/treemacs-markerize-imenu-position (position)
  "Convert integer imenu POSITION values into markers for Treemacs."
  (cond
   ((markerp position) position)
   ((integerp position)
    (let ((marker (make-marker)))
      (set-marker marker position (current-buffer))
      marker))
   (t position)))

(defun my/treemacs-normalize-flat-imenu-index (flat-index)
  "Normalize FLAT-INDEX so Treemacs always receives marker positions."
  (if (eq flat-index 'unsupported)
      flat-index
    (mapcar
     (lambda (tag-path)
       (let* ((leaf (copy-tree (car tag-path)))
              (position (cdr leaf)))
         (setcdr leaf (my/treemacs-markerize-imenu-position position))
         (cons leaf (cdr tag-path))))
     flat-index)))

(defun my/treemacs-flatten-and-sort-imenu-index ()
  "Flatten the current buffer's imenu index and sort it for Treemacs.
This mirrors Treemacs' implementation, but also normalizes integer positions to
markers before sorting so modes that do not emit markers do not break tag
following."
  (if (eq major-mode 'pdf-view-mode)
      'unsupported
    (let* ((imenu-auto-rescan t)
           (org? (eq major-mode 'org-mode))
           (index (treemacs--get-imenu-index (buffer-file-name)))
           (flat-index (if org?
                           (treemacs--flatten-org-mode-imenu-index index)
                         (treemacs--flatten-imenu-index index)))
           (first (caar flat-index))
           (semantic? (and (consp first) (overlayp (cdr first))))
           (compare-func (if (memq major-mode '(markdown-mode adoc-mode))
                             #'treemacs--compare-markdown-tag-paths
                           #'treemacs--compare-tag-paths)))
      (when semantic?
        (dolist (tag-path flat-index)
          (let ((leaf (car tag-path))
                (marker (make-marker)))
            (setcdr leaf (move-marker marker (overlay-start (cdr leaf)))))))
      (when org?
        (dolist (tag-path flat-index)
          (let ((leaf (car tag-path)))
            (when (stringp leaf)
              (setcar tag-path
                      (cons leaf (get-text-property 0 'org-imenu-marker leaf)))))))
      (dolist (tag-path flat-index)
        (let ((leaf (car tag-path)))
          (when (consp leaf)
            (setcdr leaf (my/treemacs-markerize-imenu-position (cdr leaf))))))
      (sort flat-index compare-func))))

(defvar my/treemacs-cursor-follow-timer nil
  "Idle timer used by `my/treemacs-cursor-follow-mode'.")

(defvar my/treemacs-last-follow-state nil
  "Last source context synchronized to Treemacs.")

(defvar my/treemacs-cursor-follow--hooks-active nil
  "Whether Treemacs cursor-follow high-frequency hooks are installed.")

(defcustom my/treemacs-cursor-follow-delay 0.35
  "Idle delay before Treemacs follows the current file and symbol."
  :type 'number
  :group 'my/project)

(defun my/treemacs-current-follow-state (&optional buffer window)
  "Return the follow state for BUFFER in WINDOW.
The state is used to avoid re-following when the source context did not
actually change."
  (let ((buffer (or buffer (current-buffer)))
        (window (or window (selected-window))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (list :buffer buffer
              :window window
              :point (point)
              :tick (buffer-chars-modified-tick))))))

(defun my/treemacs-reset-follow-state ()
  "Clear Treemacs caches used by file and tag following."
  (when (timerp my/treemacs-cursor-follow-timer)
    (cancel-timer my/treemacs-cursor-follow-timer))
  (setq my/treemacs-cursor-follow-timer nil
        my/treemacs-last-follow-state nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (boundp 'treemacs--imenu-cache)
        (setq-local treemacs--imenu-cache nil))
      (when (boundp 'treemacs--project-of-buffer)
        (setq-local treemacs--project-of-buffer nil))
      (when (boundp 'treemacs--previously-followed-tag-position)
        (setq-local treemacs--previously-followed-tag-position nil)))))

(defun my/treemacs-reset-current-buffer-follow-state ()
  "Clear Treemacs follow cache for the current buffer only."
  (when (timerp my/treemacs-cursor-follow-timer)
    (cancel-timer my/treemacs-cursor-follow-timer))
  (setq my/treemacs-cursor-follow-timer nil
        my/treemacs-last-follow-state nil)
  (when (boundp 'treemacs--imenu-cache)
    (setq-local treemacs--imenu-cache nil))
  (when (boundp 'treemacs--project-of-buffer)
    (setq-local treemacs--project-of-buffer nil))
  (when (boundp 'treemacs--previously-followed-tag-position)
    (setq-local treemacs--previously-followed-tag-position nil)))

(defun my/treemacs-source-buffer-p ()
  "Return non-nil when the current buffer should drive Treemacs following."
  (and (not (minibufferp))
       (not (derived-mode-p 'treemacs-mode))
       (or buffer-file-name
           (eq major-mode 'dired-mode))))

(defun my/treemacs-follow-context-active-p (&optional buffer)
  "Return non-nil when BUFFER is still the active source for Treemacs follow.
When BUFFER is nil, use the current buffer in the selected window."
  (let ((window (selected-window))
        (target (or buffer (current-buffer))))
    (and (window-live-p window)
         (not (eq window (treemacs-get-local-window)))
         (eq (window-buffer window) target)
         (buffer-live-p target)
         (with-current-buffer target
           (my/treemacs-source-buffer-p)))))

(defun my/treemacs-find-current-user-project-a (orig-fn &rest args)
  "Only let source buffers drive Treemacs project following."
  (when (my/treemacs-source-buffer-p)
    (apply orig-fn args)))

(defun my/treemacs-follow-path ()
  "Return the current buffer path Treemacs should follow."
  (cond
   (buffer-file-name
    (my/project-canonical-path buffer-file-name))
   ((derived-mode-p 'dired-mode)
    (my/project-canonical-path default-directory))))

(defun my/treemacs-safe-imenu-index ()
  "Return the current buffer's Treemacs imenu index, or nil on failure."
  (when buffer-file-name
    (condition-case nil
        (let ((inhibit-message t))
          (pcase (treemacs--flatten&sort-imenu-index)
            ('unsupported nil)
            (index index)))
      (error nil))))

(defun my/treemacs-follow-source-silently (&optional prefer-tag)
  "Follow the current source buffer in Treemacs without transient failures.
When PREFER-TAG is non-nil, prefer following the current tag when one exists."
  (let* ((treemacs-window (treemacs-get-local-window))
         (path (my/treemacs-follow-path)))
    (when (and treemacs-window path)
      (setq-local treemacs--project-of-buffer nil)
      (let ((project (treemacs--find-project-for-buffer path)))
        (when project
          (let ((inhibit-message t)
                (treemacs-pulse-on-failure nil)
                (index (and prefer-tag (my/treemacs-safe-imenu-index)))
                (followed nil))
            (setq followed
                  (or (condition-case nil
                          (and index
                               (treemacs--do-follow-tag index treemacs-window path project)
                               t)
                        (error nil))
                      (condition-case nil
                          (with-selected-window treemacs-window
                            (and (treemacs-goto-file-node path project) t))
                        (error nil))))
            (when followed
              (with-selected-window treemacs-window
                (hl-line-highlight)
                (set-window-point treemacs-window (point))
                (force-window-update treemacs-window)))
            followed))))))

(defun my/treemacs-follow-current-buffer (buffer)
  "Update Treemacs to follow BUFFER's current file and symbol."
  (setq my/treemacs-cursor-follow-timer nil)
  (when (and (buffer-live-p buffer)
             (window-live-p (treemacs-get-local-window))
             (my/treemacs-follow-context-active-p buffer))
    (with-current-buffer buffer
      (when (my/treemacs-follow-source-silently t)
        (setq my/treemacs-last-follow-state
              (my/treemacs-current-follow-state buffer (selected-window)))))))

(defun my/treemacs-schedule-follow (&rest _)
  "Schedule a Treemacs follow update for the current source buffer."
  (when (timerp my/treemacs-cursor-follow-timer)
    (cancel-timer my/treemacs-cursor-follow-timer)
    (setq my/treemacs-cursor-follow-timer nil))
  (when (and my/treemacs-cursor-follow-mode
             (window-live-p (treemacs-get-local-window))
             (my/treemacs-follow-context-active-p))
    (let* ((buffer (current-buffer))
           (state (my/treemacs-current-follow-state buffer (selected-window))))
      (unless (equal state my/treemacs-last-follow-state)
        (setq my/treemacs-cursor-follow-timer
              (run-with-idle-timer my/treemacs-cursor-follow-delay nil
                                   #'my/treemacs-follow-current-buffer
                                   buffer))))))

(defun my/treemacs-cursor-follow-window-visible-p ()
  "Return non-nil when a Treemacs window is available for following."
  (and (fboundp 'treemacs-get-local-window)
       (window-live-p (treemacs-get-local-window))))

(defun my/treemacs-cursor-follow-enable-hooks ()
  "Install high-frequency hooks used while Treemacs is visible."
  (unless my/treemacs-cursor-follow--hooks-active
    (setq my/treemacs-cursor-follow--hooks-active t)
    (add-hook 'post-command-hook #'my/treemacs-schedule-follow)
    (add-hook 'first-change-hook #'my/treemacs-reset-current-buffer-follow-state)))

(defun my/treemacs-cursor-follow-disable-hooks ()
  "Remove high-frequency hooks used by Treemacs cursor following."
  (when my/treemacs-cursor-follow--hooks-active
    (setq my/treemacs-cursor-follow--hooks-active nil)
    (remove-hook 'post-command-hook #'my/treemacs-schedule-follow)
    (remove-hook 'first-change-hook #'my/treemacs-reset-current-buffer-follow-state))
  (when (timerp my/treemacs-cursor-follow-timer)
    (cancel-timer my/treemacs-cursor-follow-timer))
  (setq my/treemacs-cursor-follow-timer nil))

(defun my/treemacs-cursor-follow-sync-hooks (&rest _)
  "Keep Treemacs follow hooks installed only while Treemacs is visible."
  (if (and my/treemacs-cursor-follow-mode
           (my/treemacs-cursor-follow-window-visible-p))
      (progn
        (my/treemacs-cursor-follow-enable-hooks)
        (my/treemacs-schedule-follow))
    (my/treemacs-cursor-follow-disable-hooks)))

(define-minor-mode my/treemacs-cursor-follow-mode
  "Follow the current file and symbol in Treemacs."
  :init-value nil
  :global t
  :lighter nil
  (if my/treemacs-cursor-follow-mode
      (progn
        (my/treemacs-reset-follow-state)
        (add-hook 'window-configuration-change-hook
                  #'my/treemacs-cursor-follow-sync-hooks)
        (my/treemacs-cursor-follow-sync-hooks))
    (remove-hook 'window-configuration-change-hook
                 #'my/treemacs-cursor-follow-sync-hooks)
    (my/treemacs-cursor-follow-disable-hooks)
    (setq my/treemacs-cursor-follow-timer nil
          my/treemacs-last-follow-state nil)))

(defun show-imenu ()
  "Toggle a left Treemacs view that follows the current file and tag."
  (interactive)
  (my/project-ensure-treemacs)
  (if-let* ((window (treemacs-get-local-window)))
      (delete-window window)
    (let ((source-buffer (current-buffer))
          (root (my/show-imenu-target-root)))
      (my/show-imenu-enable-treemacs-modes)
      (save-selected-window
        (let ((default-directory root))
          (treemacs-add-and-display-current-project-exclusively)
          (with-current-buffer source-buffer
            (my/treemacs-follow-source-silently t)))))))

(defun my/project-remove-from-treemacs-workspaces (project-root)
  "Remove PROJECT-ROOT from every Treemacs workspace."
  (when (require 'treemacs-workspaces nil t)
    (require 'treemacs-persistence nil t)
    (let ((target (my/project-canonical-path project-root))
          (removed nil))
      (dolist (workspace (append (treemacs-workspaces)
                                 (treemacs-disabled-workspaces)))
        (let* ((projects (treemacs-workspace->projects workspace))
               (filtered
                (seq-remove
                 (lambda (project)
                   (equal (my/project-canonical-path (treemacs-project->path project))
                          target))
                 projects)))
          (unless (= (length filtered) (length projects))
            (setf (treemacs-workspace->projects workspace) filtered)
            (setq removed t))))
      (when removed
        (treemacs--invalidate-buffer-project-cache)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'treemacs-mode)
              (ignore-errors (treemacs--rerender-after-workspace-change)))))
        (treemacs--persist))
      removed)))

(defun my/project-remove-projectile-cache (project-root)
  "Remove Projectile cache data for PROJECT-ROOT."
  (let* ((project-root (my/project-normalize-root project-root))
         (cache-file (projectile-project-cache-file project-root)))
    (dolist (table '(projectile-project-type-cache
                     projectile-projects-cache
                     projectile-projects-cache-time
                     projectile--dirconfig-cache))
      (when (boundp table)
        (maphash
         (lambda (key _value)
           (when (my/project-root-equal-p key project-root)
             (remhash key (symbol-value table))))
         (symbol-value table))))
    (when (and cache-file (file-exists-p cache-file))
      (delete-file cache-file))))

(defun my/project-kill-associated-buffers (project-root &optional project-label)
  "Kill buffers associated with PROJECT-ROOT.
Returns the number of killed buffers."
  (let* ((project-root (my/project-normalize-root project-root))
         (project-label (or project-label
                            (my/project-perspective-name project-root)))
         (buffers (seq-filter
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (or (my/project-path-inside-root-p buffer-file-name project-root)
                           (my/project-path-inside-root-p default-directory project-root)
                           (equal (buffer-name)
                                  (format "*vterm:%s*" project-label)))))
                   (buffer-list)))
         (killed 0))
    (dolist (buffer (append (delq (current-buffer) buffers)
                            (when (memq (current-buffer) buffers)
                              (list (current-buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)
        (setq killed (1+ killed))))
    killed))

(defun my/project-kill-perspective-if-exists (project-name)
  "Kill the perspective named PROJECT-NAME if it exists."
  (when (require 'perspective nil t)
    (when (member project-name (persp-names))
      (persp-kill project-name)
      t)))

(defun my/project-remove-known-project (project-root)
  "Remove PROJECT-ROOT from Emacs project state and clean related buffers."
  (interactive (list (my/project-read-target-root "Remove project: ")))
  (setq project-root (my/project-normalize-root project-root))
  (let ((project-label (my/project-perspective-name project-root)))
    (unless (yes-or-no-p
             (format "Remove project %s from Projectile/project.el/Treemacs/perspective state? "
                     (abbreviate-file-name project-root)))
      (user-error "Project removal cancelled"))
    (my/project-ignore-root project-root)
    (my/project-remove-from-projectile-known-projects project-root)
    (my/project-remove-from-project-list project-root)
    (my/project-remove-projectile-cache project-root)
    (my/project-remove-from-treemacs-workspaces project-root)
    (my/project-kill-perspective-if-exists project-label)
    (my/project-kill-associated-buffers project-root project-label)
    (message "Removed project %s from Emacs project management"
             (abbreviate-file-name project-root))))

(defun my/project-discover-projects-in-directory (directory depth)
  "Discover projects under DIRECTORY up to DEPTH levels deep."
  (interactive
   (list (read-directory-name "Discover projects in: " nil nil t)
         (read-number "Discovery depth: " 2)))
  (let* ((known-before (mapcar #'my/project-normalize-root projectile-known-projects))
         (before (length known-before)))
    (projectile-discover-projects-in-directory directory depth)
    (let* ((known-after (mapcar #'my/project-normalize-root projectile-known-projects))
           (new-projects
            (seq-remove
             (lambda (project-root)
               (seq-some
                (lambda (known-root)
                  (my/project-root-equal-p known-root project-root))
                known-before))
             known-after))
           (synced-count 0))
      (dolist (project-root new-projects)
        (my/project-register-root project-root)
        (setq synced-count (1+ synced-count)))
      (message "Discovered %d project(s) under %s and synced %d into Projectile/project.el"
               (max 0 (- (length known-after) before))
               (abbreviate-file-name directory)
               synced-count))))

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :init
  ;; When reloading init after Emacs has already started, `after-init-hook` has
  ;; run already, so we must enable Projectile immediately to keep `SPC p ...`
  ;; bindings functional.
  (when after-init-time
    (when (require 'projectile nil t)
      (projectile-mode 1)))
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (dolist (dir '("bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (define-key projectile-command-map (kbd ".") #'my/project-dispatch)
  (define-key projectile-command-map (kbd "p") #'my/project-switch)
  (define-key projectile-command-map (kbd "o") #'my/project-open-workbench)
  (define-key projectile-command-map (kbd "f") #'my/project-find-file)
  (define-key projectile-command-map (kbd "r") #'my/project-recent-file)
  (define-key projectile-command-map (kbd "b") #'my/project-switch-buffer)
  (define-key projectile-command-map (kbd "g") #'my/project-ripgrep)
  (define-key projectile-command-map (kbd "d") #'my/project-open-root)
  (define-key projectile-command-map (kbd "m") #'my/project-magit-status)
  (define-key projectile-command-map (kbd "v") #'my/project-vterm)
  (setq projectile-mode-line "Projectile"
        projectile-dynamic-mode-line nil
        projectile-track-known-projects-automatically nil)
  (my/project-prune-hidden-project-state)
  :custom
  (projectile-use-git-grep t)
  (projectile-ignored-project-function #'my/project-ignored-root-p)
  (projectile-indexing-method 'alien)
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-project-search-path my/project-search-paths)
  (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,package-user-dir)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :defer 2
  :config
  (counsel-projectile-mode 1))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix my/project-dispatch ()
    "Project workbench."
    :transient-suffix 'transient--do-stay
    [["Switch"
      ("p" "switch project" my/project-switch
       :transient transient--do-exit)
      ("o" "open workbench" my/project-open-workbench
       :transient transient--do-exit)
      ("f" "find file" my/project-find-file)
      ("r" "recent file" my/project-recent-file)
      ("b" "switch buffer" my/project-switch-buffer)]
     ["Search"
      ("s" "ripgrep" my/project-ripgrep)
      ("a" "all-project files" projectile-find-file-in-known-projects)
      ("g" "magit" my/project-magit-status)]
     ["Open / Shell"
      ("d" "open root" my/project-open-root)
      ("v" "project vterm" my/project-vterm)]
     ["Manage"
      ("A" "add project" my/project-add-known-project)
      ("D" "discover in dir" my/project-discover-projects-in-directory)
      ("x" "remove project" my/project-remove-known-project)
      ("k" "kill project buffers" my/project-kill-buffers)]]))

(use-package treemacs
  :ensure t
  :defer t
  :hook (treemacs-mode . (lambda ()
                           (display-line-numbers-mode -1)))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t"     . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-buffer-name-function #'treemacs-default-buffer-name
        treemacs-buffer-name-prefix " *Treemacs-Buffer-"
        treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 1.0
        treemacs-default-visit-action 'treemacs-visit-node-in-most-recently-used-window
        treemacs-directory-name-transformer #'identity
        treemacs-display-in-side-window t
        treemacs-eldoc-display 'simple
        treemacs-file-event-delay 3000
        treemacs-file-name-transformer #'identity
        treemacs-follow-after-init nil
        treemacs-expand-after-init t
        treemacs-find-workspace-method 'find-for-file-or-pick-first
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-hide-dot-git-directory t
        treemacs-hide-dot-jj-directory t
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-is-never-other-window nil
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        treemacs-move-files-by-mouse-dragging t
        treemacs-move-forward-on-expand nil
        treemacs-no-delete-other-windows t
        treemacs-project-follow-cleanup nil
        treemacs-position 'left
        treemacs-read-string-input 'from-child-frame
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home nil
        treemacs-show-cursor nil
        treemacs-show-hidden-files t
        treemacs-silent-filewatch nil
        treemacs-silent-refresh nil
        treemacs-sorting 'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.5
        treemacs-width 35
        treemacs-width-is-initially-locked nil
        treemacs-file-follow-delay 0.4
        treemacs-workspace-switch-cleanup nil
        imenu-auto-rescan t)
  ;; In practice evil's motion-state mouse bindings can shadow treemacs' own
  ;; handlers, so wire single-click actions explicitly for both vanilla and
  ;; evil treemacs buffers.
  (keymap-set treemacs-mode-map "<mouse-1>" #'treemacs-single-click-expand-action)
  (with-eval-after-load 'treemacs-evil
    (keymap-set evil-treemacs-state-map "<down-mouse-1>" #'treemacs-leftclick-action)
    (keymap-set evil-treemacs-state-map "<mouse-1>" #'treemacs-single-click-expand-action)
    (keymap-set evil-treemacs-state-map "<double-mouse-1>" #'treemacs-doubleclick-action)
    (keymap-set evil-treemacs-state-map "<drag-mouse-1>" #'treemacs-dragleftclick-action)
    (keymap-set evil-treemacs-state-map "<mouse-3>" #'treemacs-rightclick-menu))
  (with-eval-after-load 'treemacs-tag-follow-mode
    (advice-remove 'treemacs--flatten&sort-imenu-index
                   #'my/treemacs-normalize-flat-imenu-index)
    (advice-remove 'treemacs--flatten&sort-imenu-index
                   #'my/treemacs-flatten-and-sort-imenu-index)
    (advice-add 'treemacs--flatten&sort-imenu-index
                :override
                #'my/treemacs-flatten-and-sort-imenu-index))
  (when (bound-and-true-p treemacs-tag-follow-mode)
    (treemacs-tag-follow-mode -1))
  (when (bound-and-true-p treemacs-follow-mode)
    (treemacs-follow-mode -1))
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-filewatch-mode t)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (treemacs-hide-gitignored-files-mode nil)
  (treemacs-project-follow-mode t)
  (my/treemacs-cursor-follow-mode t))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package perspective
  :ensure t
  :defer 2
  :bind
  (("C-x k" . persp-kill-buffer*))
  :init
  (setq persp-mode-prefix-key (kbd "C-x x")
        persp-initial-frame-name "main")
  :config
  (add-to-list 'window-persistent-parameters '(winner-ring . t))
  (persp-mode 1)
  (add-hook 'persp-before-deactivate-functions #'my/project-save-winner-data-h)
  (add-hook 'persp-activated-functions #'my/project-load-winner-data-h)
  (with-eval-after-load 'treemacs
    (when (assoc 'Perspectives treemacs-scope-types)
      (treemacs-set-scope-type 'Perspectives))))

(with-eval-after-load 'project
  (add-to-list 'project-list-exclude #'my/project-project-object-ignored-p))

(with-eval-after-load 'project
  (advice-add 'project--remember-dir :around #'my/project--remember-dir-visible-only)
  (my/project-prune-hidden-project-state))

(with-eval-after-load 'dashboard
  (setq dashboard-projects-backend 'projectile
        dashboard-projects-switch-function #'my/project-open-workbench)

  (unless (advice-member-p #'my/dashboard-projects-load-projects
                           #'dashboard-projects-backend-load-projects)
    (advice-add 'dashboard-projects-backend-load-projects
                :around
                #'my/dashboard-projects-load-projects)))




(with-eval-after-load 'treemacs
  (advice-add 'treemacs--follow :around
              (lambda (fn &rest args)
                (let ((inhibit-message t))
                  (apply fn args)))))

(with-eval-after-load 'treemacs-workspaces
  (advice-remove 'treemacs--find-current-user-project
                 #'my/treemacs-find-current-user-project-a)
  (advice-add 'treemacs--find-current-user-project :around
              #'my/treemacs-find-current-user-project-a))





(provide 'init-project)

;;; init-project.el ends here
