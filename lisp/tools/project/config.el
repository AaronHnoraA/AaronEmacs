;;; tools/project/config.el --- Project navigation and integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'aaron-open)
(require 'ui/treemacs/config)
(require 'remote-fs)
(require 'remote-workspace)
(require 'subr-x)

(eval-when-compile
  (require 'treemacs-workspaces nil t))

(eval-and-compile
  (+treemacs-bridge-declarations))

(defvar projectile-known-projects)
(defvar recentf-list)
(defvar dashboard-projects-backend)
(defvar dashboard-projects-switch-function)
(defvar breadcrumb--ipath-plain-cache)
(defvar breadcrumb--last-update-tick)
(defvar breadcrumb-local-mode)
(defvar bc--ipath-plain-cache)
(defvar imenu-auto-rescan)
(defvar lsp-managed-mode)
(defvar treemacs-indentation)
(defvar treemacs-indentation-string)

(declare-function dashboard-projects-backend-load-projects "dashboard-widgets")
(declare-function breadcrumb-local-mode "breadcrumb" (&optional arg))
(declare-function lsp-headerline-check-breadcrumb "lsp-headerline" (&rest arguments))
(declare-function +breadcrumb-tab-line-sync-h "tools/lsp/config" ())
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(declare-function imenu--subalist-p "imenu" (item))
(declare-function +file-icon-for-file "ui/theme/config" (file &rest args))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(declare-function +direnv-update-environment-maybe "tools/direnv/config" (&optional path))
(declare-function consult-grep "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))
(defvar consult-ripgrep-args)
(declare-function remote-executable-find "remote-process"
                  (program &optional context))
(declare-function remote-read-target "remote-board" (&optional prompt omit-local))
(declare-function get-current-persp "perspective")
(declare-function persp-parameter "perspective" (parameter &optional persp))
(declare-function persp-curr "perspective" (&optional frame))
(declare-function remote-workspace-root "remote-workspace" (workspace))
(defvar remote-current-workspace)
(declare-function projectile-find-file-in-directory "projectile")
(declare-function projectile-ignored-project-p "projectile")
(declare-function projectile-known-projects "projectile")
(declare-function projectile-load-known-projects "projectile")
(declare-function projectile-merge-known-projects "projectile")
(declare-function projectile-relevant-known-projects "projectile")
(declare-function projectile-save-known-projects "projectile")
(declare-function set-persp-parameter "perspective" (parameter value &optional persp))

(defgroup +project nil
  "Project workflow helpers."
  :group 'convenience)

(remote-register-adapter
 "treemacs"
 :capabilities '(file-read directory metadata watch process-sync)
 :preferences '((default . ("native" "tramp" "tramp-rpc")))
 :placement 'target
 :process-class 'background)

(config-defvar +project-search-paths nil
  "Project discovery roots for Projectile.
Entries follow the same shape as `projectile-project-search-path': either a
directory string or a cons cell of the form (DIRECTORY . DEPTH)."
  :type '(repeat (choice (directory :tag "Directory") (cons :tag "Directory + depth" (directory :tag "Directory") (integer :tag "Depth"))))
  :group '+project)

(config-defvar +jupyter-ssh-projects nil
  "SSH Jupyter project shortcuts as plists with :name, :target and :root.
The root is an absolute path on the target.  Notebook, shell and source files
share the ordinary Remote workspace for that folder."
  :type 'sexp
  :group '+project)

(defun +jupyter/ssh-open-project (&optional project)
  "Enter a target-owned Jupyter PROJECT through the normal Remote filesystem.
PROJECT is a configured plist; interactively choose one or an SSH directory."
  (interactive)
  (let* ((current (and (fboundp '+project-current-root)
                       (+project-current-root)))
         (choices
          (append
           (when (and current
                      (not (equal "local"
                                  (remote-context-target-id (remote-context current)))))
             (list (cons (format "Current project · %s" current)
                         (list :root current))))
           (mapcar (lambda (item)
                     (cons (format "%s · %s" (plist-get item :name)
                                   (plist-get item :target)) item))
                   +jupyter-ssh-projects)
           '(("Choose SSH folder…" . choose))))
         (project (or project
                      (cdr (assoc (completing-read "Jupyter SSH project: "
                                                   choices nil t)
                                  choices))))
         (root
          (if (eq project 'choose)
              (let* ((_ (require 'remote-board))
                     (target (remote-read-target "SSH target: " t))
                     (base (remote-make-file-name (remote-target-id target) "/")))
                (read-directory-name "Project folder on target: " base nil t))
            (let ((path (plist-get project :root)))
              (if (remote-fs-file-name-p path)
                  path
                (remote-make-file-name (plist-get project :target) path))))))
    (unless (and root (file-directory-p root))
      (user-error "Jupyter SSH project directory is unavailable: %s" root))
    (remote-workspace-open root :connect t :adapter "emacs-file"
                           :capability 'file-read :load-environment t)
    ;; Remember it like any project; a plain folder without a project marker
    ;; still opens, it just is not listed.
    (condition-case err
        (+project-register-root root)
      (error (message "Project not remembered: %s" (error-message-string err))))
    (+project/switch root)))

(config-defvar +project-import-project-el-entries nil
  "Whether `project.el' entries may flow back into the manual project list."
  :type 'boolean
  :group '+project)

(config-defvar +treemacs-imenu-indent-width 2
  "Columns used for each semantic level in an expanded Treemacs outline.
The outline begins at this shallow fixed indentation instead of inheriting the
source file's filesystem depth."
  :type 'integer
  :group '+project)

(defconst +project-state-dir
  (file-name-as-directory
   (expand-file-name "project"
                     (or (and (boundp 'aaron-state-dir) aaron-state-dir)
                         (expand-file-name "var" user-emacs-directory)))))

(defconst +project-ignored-roots-file
  (expand-file-name "ignored-roots.el" +project-state-dir))

(defconst +project-list-file
  (expand-file-name "projects.eld" +project-state-dir))

(defvar +project-ignored-roots-load-error nil
  "Error from loading the ignored-project list, or nil when it parsed.")

(make-directory +project-state-dir t)

(setq project-list-file +project-list-file)

(defun +project--canonical-file-name (path)
  "Canonicalize PATH without assigning a native absolute path to a target.
Relative paths still belong to the current buffer's directory."
  (remote-canonicalize-emacs-file-name path))

(defun +project-normalize-root (project-root)
  "Return PROJECT-ROOT as a normalized directory name.
A root this machine reaches directly keeps its native name, so ordinary local
projects are unchanged.  Any other root is its logical `/fs:TARGET:/path/'
identity, so a project reached through `/ssh:' or `/fs:' is one project.
This is spelling only: nothing is probed or connected."
  (when (stringp project-root)
    (let* ((logical (ignore-errors
                      (+project--canonical-file-name project-root)))
           (expanded (if logical
                         logical
                       (expand-file-name project-root)))
           (client (and logical (ignore-errors (remote-client-file-name logical)))))
      (file-name-as-directory (or client logical expanded)))))

(defun +project-client-root-p (project-root)
  "Return non-nil when PROJECT-ROOT is reachable without a Remote connection."
  (let ((logical (ignore-errors (+project--canonical-file-name project-root))))
    (or (null logical) (ignore-errors (remote-client-file-name logical)))))

(defun +project-canonical-path (path)
  "Return PATH in a stable canonical form."
  (when (stringp path)
    (let* ((logical (+project--canonical-file-name path))
           (client (ignore-errors (remote-client-file-name logical)))
           (canonical
            (if client
                (+project--canonical-file-name
                 (or (ignore-errors (file-truename client)) client))
              logical)))
      (directory-file-name canonical))))

(defun +treemacs-project-path (path)
  "Return PATH in the filesystem namespace Treemacs can consume.
Treemacs launches native Python/Git helpers for paths it classifies as local,
so a logical `/fs:local:' spelling must never enter its project model.  A
client-accessible target uses its client path; otherwise the `treemacs'
adapter projects the logical path to a standard file-handler path such as
TRAMP.  The source buffer itself keeps its ordinary logical identity."
  (when (stringp path)
    (let* ((logical (+project--canonical-file-name path))
           (client (ignore-errors (remote-client-file-name logical)))
           (physical
            (or
             client
             (remote-project-file-name
              logical nil 'directory "treemacs"))))
      (directory-file-name physical))))

(defun +treemacs-visit-path (path)
  "Return the buffer-facing path for a Treemacs model PATH.
Client-accessible targets retain native Emacs paths.  Target-only physical
paths are restored to `/fs:' so opening a Treemacs node cannot leak a TRAMP
pipeline identity into buffer state."
  (if (not (stringp path))
      path
    (let* ((logical (+project--canonical-file-name path))
           (client (ignore-errors (remote-client-file-name logical))))
      (or client logical))))

(defvar +programmatic-file-visit nil
  "Non-nil while a tool visits files only to read or index them.
File-surface redirects (such as Markdown opening in Noema) must not fire
then: the visit is not the person opening the file.")

(defvar +treemacs-tag-visit-functions nil
  "Abnormal hook run after Treemacs jumps to a tag.
Each function receives the buffer it jumped into, with point at the tag, in
the selected window.  A file shown on another surface (a Noema page) can move
that surface to the tag instead.")

(defun +treemacs-after-tag-visit-a (&rest _)
  "Run `+treemacs-tag-visit-functions' for the buffer a tag jump selected."
  (let ((buffer (window-buffer (selected-window))))
    (when (buffer-local-value 'buffer-file-name buffer)
      (run-hook-with-args '+treemacs-tag-visit-functions buffer))))

(defun +treemacs-tag-visit-programmatic-a (fn &rest args)
  "Run the heading-jump FN with ARGS as a programmatic file visit.
Jumping to a heading reads the file to find it; opening the file node itself
is an ordinary visit and keeps every surface redirect (Markdown→Noema)."
  (let ((+programmatic-file-visit t))
    (apply fn args)))

(defun +treemacs-visit-logical-path-a (fn &rest args)
  "Run Treemacs visit FN with its model paths restored for buffers."
  (let ((original-find-file (symbol-function 'find-file))
        (original-find-file-noselect (symbol-function 'find-file-noselect))
        (original-dired (symbol-function 'dired))
        (original-get-file-buffer (symbol-function 'get-file-buffer)))
    (cl-letf
        (((symbol-function 'find-file)
          (lambda (file &rest arguments)
            (apply original-find-file
                   (+treemacs-visit-path file) arguments)))
         ((symbol-function 'find-file-noselect)
          (lambda (file &rest arguments)
            (apply original-find-file-noselect
                   (+treemacs-visit-path file) arguments)))
         ((symbol-function 'dired)
          (lambda (directory &rest arguments)
            (apply original-dired
                   (+treemacs-visit-path directory) arguments)))
         ((symbol-function 'get-file-buffer)
          (lambda (file)
            ;; `get-file-buffer' is a C primitive, but while resolving file
            ;; identity it may enter the public symbol again.  Restore the
            ;; primitive for the duration of this call so our temporary path
            ;; projector cannot recursively wrap itself.
            (cl-letf
                (((symbol-function 'get-file-buffer)
                  original-get-file-buffer))
              (let ((logical (+treemacs-visit-path file)))
                ;; Emacs' primitive lookup may decline custom logical
                ;; handlers even when `buffer-file-truename' is identical;
                ;; `find-buffer-visiting' performs the handler-aware fallback.
                (or (funcall original-get-file-buffer logical)
                    (find-buffer-visiting logical)))))))
      (apply fn args))))

(defun +treemacs-normalize-persist-lines-a (lines)
  "Project persisted Treemacs path LINES before workspace restoration.
Older revisions persisted logical `/fs:' identities.  Treemacs deliberately
disables file-name handlers for paths it classifies as local, so
`/fs:local:' would otherwise be passed literally to native Python/Git
helpers.  Target-only paths are projected to ordinary TRAMP names, which
Treemacs already understands as remote.

This advice transforms only Treemacs' in-memory parse input.  Its normal
persistence pass writes the migrated model atomically through Treemacs'
existing state owner."
  (mapcar
   (lambda (line)
     (if (and
          (stringp line)
          (string-match
           "\\`\\([[:space:]]*- path ::[[:space:]]*\\)\\(.+\\)\\'"
           line))
         (let ((prefix (match-string 1 line))
               (path (match-string 2 line)))
           (condition-case err
               (concat prefix (+treemacs-project-path path))
             (error
              (remote-log
               'treemacs-persist-path-migration-failed
               :path path
               :error (error-message-string err))
              line)))
       line))
   lines))

(defun +project-root-equal-p (left right)
  "Return non-nil when LEFT and RIGHT refer to the same project root."
  (equal (+project-canonical-path left)
         (+project-canonical-path right)))

(defun +project-root-member-p (root roots)
  "Return non-nil when ROOT matches a member of ROOTS by project identity."
  (seq-some (lambda (known) (+project-root-equal-p known root)) roots))

(defun +project-hidden-root-p (project-root)
  "Return non-nil when PROJECT-ROOT should stay out of project UIs."
  (when (stringp project-root)
    (let ((project-root (+project-normalize-root project-root)))
      (and project-root
           (or (+project-ignored-root-p project-root)
               (and (require 'projectile nil t)
                    (projectile-ignored-project-p project-root)))))))

(defun +project-load-ignored-roots ()
  "Return project roots suppressed from the custom project workflow."
  (setq +project-ignored-roots-load-error nil)
  (condition-case err
      (when (file-exists-p +project-ignored-roots-file)
        (with-temp-buffer
          (insert-file-contents +project-ignored-roots-file)
          (with-syntax-table emacs-lisp-mode-syntax-table
            (let ((data (read (current-buffer))))
              (unless (and (proper-list-p data)
                           (cl-every #'stringp data))
                (user-error "Ignored-project file must contain a list of roots"))
              (forward-comment (point-max))
              (unless (eobp)
                (user-error "Ignored-project file has extra forms"))
              data))))
    (error
     (setq +project-ignored-roots-load-error (error-message-string err))
     (display-warning
      'init-project
      (format "Failed to load ignored projects from %s: %s"
              +project-ignored-roots-file
              +project-ignored-roots-load-error)
      :error)
     nil)))

(defvar +project-ignored-roots
  (or (+project-load-ignored-roots) nil)
  "Project roots hidden from the custom project workflow.")

(defun +project-save-ignored-roots ()
  "Persist `+project-ignored-roots' to disk."
  (when +project-ignored-roots-load-error
    (user-error "Repair %s and run project-reload-ignored-roots before saving: %s"
                +project-ignored-roots-file
                +project-ignored-roots-load-error))
  (config--write-string-atomically
   +project-ignored-roots-file
   (with-temp-buffer
     (insert ";;; -*- lisp-data -*-\n")
     (let ((print-length nil)
           (print-level nil))
       (pp (mapcar #'+project-normalize-root +project-ignored-roots)
           (current-buffer)))
     (buffer-string))))

(defun +project/reload-ignored-roots ()
  "Reload the ignored-project list after editing its state file."
  (interactive)
  (let ((roots (+project-load-ignored-roots)))
    (when +project-ignored-roots-load-error
      (user-error "Ignored-project list is still invalid: %s"
                  +project-ignored-roots-load-error))
    (setq +project-ignored-roots roots)
    (+project/prune-hidden-project-state)
    (message "Reloaded %d ignored project roots" (length roots))))

(defalias 'project-reload-ignored-roots #'+project/reload-ignored-roots
  "Reload the ignored-project list after repairing its state file.")

(defun +project-ignored-root-p (project-root)
  "Return non-nil when PROJECT-ROOT is suppressed from the workflow."
  (seq-some
   (lambda (ignored-root)
     (+project-root-equal-p ignored-root project-root))
   +project-ignored-roots))

(defun +project-ignore-root (project-root)
  "Suppress PROJECT-ROOT from the custom project workflow."
  (unless (+project-ignored-root-p project-root)
    (let ((previous +project-ignored-roots))
      (push (+project-normalize-root project-root) +project-ignored-roots)
      (condition-case err
          (+project-save-ignored-roots)
        (error
         (setq +project-ignored-roots previous)
         (signal (car err) (cdr err)))))))

(defun +project-unignore-root (project-root)
  "Re-enable PROJECT-ROOT in the custom project workflow."
  (let ((previous +project-ignored-roots)
        (before (length +project-ignored-roots)))
    (setq +project-ignored-roots
          (seq-remove
           (lambda (ignored-root)
             (+project-root-equal-p ignored-root project-root))
           +project-ignored-roots))
    (unless (= before (length +project-ignored-roots))
      (condition-case err
          (+project-save-ignored-roots)
        (error
         (setq +project-ignored-roots previous)
         (signal (car err) (cdr err))))
      t)))

(defun +project-project-object-ignored-p (project)
  "Return non-nil when PROJECT should be excluded from `project.el' lists."
  (+project-hidden-root-p (project-root project)))

(defun +project-path-inside-root-p (path project-root)
  "Return non-nil when PATH belongs to PROJECT-ROOT without a target probe."
  (when-let* ((path (+project-canonical-path path))
              (root (file-name-as-directory
                     (+project-canonical-path project-root))))
    (or (equal path (directory-file-name root))
        (string-prefix-p root path))))

(defun +project-name (project-root)
  "Return a display name for PROJECT-ROOT."
  (let* ((project-root (directory-file-name (+project-normalize-root project-root)))
         (name (and (boundp 'projectile-project-name-function)
                    (functionp projectile-project-name-function)
                    (ignore-errors
                      (funcall projectile-project-name-function project-root)))))
    (if (and (stringp name) (not (equal name "")))
        name
      (file-name-nondirectory project-root))))

(defun +project-perspective-name (project-root)
  "Return a perspective-safe name for PROJECT-ROOT.
If multiple known projects share the same basename, add the parent directory to
keep perspective names unique."
  (let* ((project-root (directory-file-name (+project-normalize-root project-root)))
         (base-name (+project-name project-root))
         (parent-name (file-name-nondirectory
                       (directory-file-name (file-name-directory project-root))))
         (has-duplicate
          (seq-some
           (lambda (other-root)
             (let ((other-root (directory-file-name other-root)))
               (and (not (equal other-root project-root))
                    (equal (+project-name other-root) base-name))))
           (+project-known-projects))))
    (if has-duplicate
        (format "%s@%s" base-name parent-name)
      base-name)))

(defun +project-current-root ()
  "Return the current project root, or nil when outside any project."
  (let ((root
         (or (ignore-errors
               (when-let* ((projectile-root (projectile-project-root)))
                 (+project-normalize-root projectile-root)))
             (when (fboundp 'project-current)
               (when-let* ((project (project-current nil default-directory))
                           (project-root (project-root project)))
                 (+project-normalize-root project-root))))))
    (unless (+project-hidden-root-p root)
      root)))

(defun +project-sync-roots-from-project-el ()
  "Sync visible `project.el' roots into Projectile known projects."
  (when (and +project-import-project-el-entries
             (require 'project nil t)
             (require 'projectile nil t))
    (projectile-known-projects)
    (let ((changed nil))
      (dolist (project-root (project-known-project-roots))
        (let ((project-root (+project-normalize-root project-root)))
          (unless (or (+project-hidden-root-p project-root)
                      (seq-some
                       (lambda (known-root)
                         (+project-root-equal-p known-root project-root))
                       projectile-known-projects))
            (push project-root projectile-known-projects)
            (setq changed t))))
      (when changed
        (setq projectile-known-projects
              (seq-uniq (mapcar #'+project-normalize-root projectile-known-projects)))
        (projectile-merge-known-projects)))
    projectile-known-projects))

(defun +project-known-projects ()
  "Return known Projectile projects that still exist.
A remote root is listed without probing it: listing projects must not dial
every target, and an unreachable host must not make its projects vanish.
Opening the project connects and reports a missing directory then."
  (when (require 'projectile nil t)
    (seq-uniq
     (seq-filter
      (lambda (project-root)
        (and (not (+project-hidden-root-p project-root))
             (or (not (+project-client-root-p project-root))
                 (file-directory-p project-root))))
      (mapcar #'+project-normalize-root
              (projectile-relevant-known-projects))))))

(defun +dashboard-projects-load-projects (orig-fn &rest args)
  "Use the custom Projectile project list for dashboard projects."
  (if (eq dashboard-projects-backend 'projectile)
      (or (+project-known-projects) nil)
    (apply orig-fn args)))

(defun +project-read-known-project (&optional prompt)
  "Read one known project root using PROMPT."
  (let ((projects (+project-known-projects)))
    (unless projects
      (user-error "Projectile does not know any projects yet"))
    (projectile-completing-read (or prompt "Project: ")
                                projects
                                :caller 'projectile-read-project)))

(defun +project-read-target-root (&optional prompt)
  "Return the current project root, or ask for one using PROMPT.
With a prefix argument, always prompt."
  (or (and (not current-prefix-arg)
           (+project-current-root))
      (+project-read-known-project prompt)))

(defun +project-switch-perspective (project-root)
  "Switch to the perspective associated with PROJECT-ROOT."
  (when (fboundp 'persp-switch)
    (persp-switch (+project-perspective-name project-root))))

(defmacro +with-project-root-context (project-root &rest body)
  "Evaluate BODY with PROJECT-ROOT bound as the active Projectile root."
  (declare (indent 1) (debug t))
  `(let* ((project-root (+project-normalize-root ,project-root))
          (default-directory project-root)
          (projectile-project-root project-root))
     (when (fboundp '+direnv-update-environment-maybe)
       (+direnv-update-environment-maybe project-root))
     ,@body))

(defvar +project-activated-hook nil
  "Hook called with the canonical active root, or nil on project exit.
Only explicit navigation changes this state; background project queries do not.")

(defvar +project-active-root nil "Canonical root explicitly active for project consumers.")
(defvar +project--switching nil)
(defvar +project--perspective-roots (make-hash-table :test #'eq :weakness 'key)
  "Project associations keyed by live Perspective objects, independent of names.")

(defun +project-activate (root &optional remember)
  "Activate ROOT, or leave the active project when ROOT is nil.
With REMEMBER, associate the current perspective with ROOT.  Identity changes
emit one event without scanning files or opening a Remote connection."
  (setq root (when root (file-name-as-directory
                         (+project--canonical-file-name root))))
  (when (and remember (fboundp 'persp-curr) (persp-curr))
    (if root (puthash (persp-curr) root +project--perspective-roots)
      (remhash (persp-curr) +project--perspective-roots)))
  (unless (equal root +project-active-root)
    (setq +project-active-root root)
    (run-hook-with-args '+project-activated-hook root)))

(defun +project/leave ()
  "Leave the active project's background consumers, preserving its buffers."
  (interactive)
  (+project-activate nil t))

(defun +project-perspective-switched-h ()
  "Follow a recorded Perspective switch, ignoring temporary package contexts."
  (unless +project--switching
    (+project-activate (gethash (persp-curr) +project--perspective-roots))))

(defun +project-perspective-killed-h ()
  "Forget a killed perspective and release its active project."
  (let* ((perspective (persp-curr))
         (root (gethash perspective +project--perspective-roots)))
    (remhash perspective +project--perspective-roots)
    (when (and root (equal root +project-active-root))
      (+project-activate nil))))

(defun +project-workspace-closed-h ()
  "Release project consumers when their Remote workspace closes."
  (let ((root (file-name-as-directory (remote-workspace-root remote-current-workspace)))
        expired)
    (maphash (lambda (perspective value)
               (when (equal root value) (push perspective expired)))
             +project--perspective-roots)
    (dolist (perspective expired) (remhash perspective +project--perspective-roots))
    (when (equal root +project-active-root) (+project-activate nil))))

(with-eval-after-load 'remote-workspace
  (add-hook 'remote-workspace-close-hook #'+project-workspace-closed-h))

(defun +project/switch (project-root &optional arg)
  "Switch to PROJECT-ROOT, keeping an already visible project file.
With ARG, use Projectile's commander action instead."
  (interactive (list (+project-read-known-project "Switch to project: ")
                     current-prefix-arg))
  (setq project-root (+project-normalize-root project-root))
  (let ((canonical-root
         (file-name-as-directory
          (+project--canonical-file-name project-root)))
        opened)
    (unwind-protect
        (progn
          (let ((+project--switching t)) (+project-switch-perspective project-root))
          (+with-project-root-context project-root
            (if arg
                (projectile-switch-project-by-name project-root arg)
              (if-let* ((source-window
                         (seq-find
                          (lambda (window)
                            (and (not (window-parameter window 'window-side))
                                 (when-let* ((file (buffer-file-name
                                                    (window-buffer window))))
                                   (string-prefix-p
                                    canonical-root
                                    (+project--canonical-file-name file)))))
                          (cons (selected-window)
                                (window-list nil nil)))))
                  (select-window source-window)
                (dired project-root))))
          (setq opened t)
          (+project-activate project-root t))
      ;; A cancelled opener can already have switched the perspective.  Follow
      ;; its recorded identity instead of retaining the previous scope there.
      (when (and (not opened) (fboundp 'persp-curr))
        (+project-perspective-switched-h)))))

(defun +project-ensure-treemacs ()
  "Load Treemacs integrations needed for project-aware navigation."
  (or (require 'treemacs nil t)
      (user-error "Treemacs is unavailable"))
  (require 'treemacs-projectile nil t)
  (require 'treemacs-evil nil t))

(defun +project/open-workbench (project-root &optional arg)
  "Switch to PROJECT-ROOT and activate its workbench context.
With ARG, use Projectile's switch action."
  (interactive (list (+project-read-known-project "Open project workbench: ")
                     current-prefix-arg))
  (setq project-root (+project-normalize-root project-root))
  (+project/switch project-root arg))

(defalias 'project-workbench #'+project/open-workbench
  "Open a project as a full workbench.")

(defun +project/find-file (project-root)
  "Find a file in PROJECT-ROOT.
Without a prefix argument, default to the current project."
  (interactive (list (+project-read-target-root "Find file in project: ")))
  (+with-project-root-context project-root
    (projectile-find-file-in-directory project-root)
    (+project-activate project-root t)))

(defun +project-recent-file-choices (project-root)
  "Return recent files in PROJECT-ROOT as (RELATIVE-NAME . VISIT-PATH).
Read Emacs's `recentf-list' directly, so listing remote candidates does not
ask Projectile to resolve the project root with `file-truename'."
  (let ((canonical-root
         (file-name-as-directory (+project-canonical-path project-root)))
        (seen (make-hash-table :test #'equal))
        choices)
    (dolist (file (and (boundp 'recentf-list) recentf-list))
      (when (stringp file)
        (when-let* ((canonical
                     (ignore-errors (+project-canonical-path file)))
                    ((string-prefix-p canonical-root canonical))
                    (relative (substring canonical (length canonical-root))))
          (unless (gethash relative seen)
            (when-let* ((logical
                         (ignore-errors (+project--canonical-file-name file))))
              (puthash relative t seen)
              (push (cons relative
                          (or (ignore-errors
                                (remote-client-file-name logical))
                              logical))
                    choices))))))
    (nreverse choices)))

(defun +project/recent-file (project-root)
  "Open a recently visited file in PROJECT-ROOT."
  (interactive (list (+project-read-target-root "Recent file in project: ")))
  (+with-project-root-context project-root
    (let ((choices (+project-recent-file-choices project-root)))
      (unless choices
        (user-error "No recent files in %s"
                    (abbreviate-file-name project-root)))
      (let* ((selected
              (projectile-completing-read "Recently visited files: "
                                          (mapcar #'car choices)
                                          :caller 'projectile-read-file))
             (file (cdr (assoc selected choices))))
        (unless file
          (user-error "Choose a recent project file"))
        (find-file file)
        (+project-activate project-root t)))))

(defun +project/find-file-in-known-projects ()
  "Find a file across known projects, preserving its own machine identity."
  (interactive)
  (let* ((file (projectile-completing-read
                "Find file in projects: " (projectile-all-project-files)
                :caller 'projectile-read-file))
         (logical (+project--canonical-file-name file))
         (visit (or (remote-client-file-name logical) logical)))
    (aaron-open-visit-file visit)))

(defun +project/switch-buffer (project-root)
  "Switch to a buffer that belongs to PROJECT-ROOT."
  (interactive (list (+project-read-target-root "Buffer in project: ")))
  (+with-project-root-context project-root
    (projectile-switch-to-buffer)
    (+project-activate project-root t)))

(defun +project/ripgrep (project-root &optional initial rg-unavailable)
  "Search PROJECT-ROOT with its available target-side search program.
Use a client path for a shared filesystem so local projects keep native
process and file-visit speed.  GNU grep remains usable when an SSH
target has no ripgrep installation.  INITIAL seeds the search prompt.
RG-UNAVAILABLE records a previous probe by another search entry point."
  (interactive (list (+project-read-target-root "Search in project: ")))
  (require 'remote-process)
  (require 'remote-search)
  (require 'consult)
  (setq project-root (+project-normalize-root project-root))
  (when (fboundp '+direnv-update-environment-maybe)
    (+direnv-update-environment-maybe project-root))
  (let* ((logical (+project--canonical-file-name project-root))
         (context (remote-context logical))
         (search-root (or (remote-client-file-name logical) logical))
         (custom-search-command
          (not (remote-search-consult-ripgrep-args
                consult-ripgrep-args "rg")))
         (rg (and (not custom-search-command)
                  (remote-search-ripgrep context rg-unavailable)))
         (rg-args
          (and rg
               (remote-search-consult-ripgrep-args
                consult-ripgrep-args rg)))
         (remote-current-adapter-id "process"))
    (cond
     (custom-search-command
      (consult-ripgrep search-root initial))
     (rg-args
      (let ((consult-ripgrep-args rg-args))
        (consult-ripgrep search-root initial)))
     ((remote-executable-find "grep" context)
      (consult-grep search-root initial))
     (t
      (user-error "Neither ripgrep nor grep is available on this target")))))

(defun +project/open-root (project-root)
  "Open PROJECT-ROOT in Dired/Dirvish."
  (interactive (list (+project-read-target-root "Open project root: ")))
  (setq project-root (+project-normalize-root project-root))
  (when (fboundp '+direnv-update-environment-maybe)
    (+direnv-update-environment-maybe project-root))
  (let ((default-directory project-root))
    (dired project-root))
  (+project-activate project-root t))

(defun +project/magit-status (project-root)
  "Open Magit status for PROJECT-ROOT."
  (interactive (list (+project-read-target-root "Magit project: ")))
  (setq project-root (+project-normalize-root project-root))
  (when (fboundp '+direnv-update-environment-maybe)
    (+direnv-update-environment-maybe project-root))
  (let* ((logical (+project--canonical-file-name project-root))
         (magit-root (or (remote-client-file-name logical) logical))
         (default-directory magit-root))
    (magit-status-setup-buffer magit-root)
    (+project-activate project-root t)))

(defun +project/portal (project-root)
  "Open or switch to a dedicated Portal terminal for PROJECT-ROOT."
  (interactive (list (+project-read-target-root "Portal project: ")))
  (let* ((project-root (+project-normalize-root project-root))
         (buffer-name (format "*portal:%s*" (+project-perspective-name project-root)))
         (default-directory project-root))
    (when (fboundp '+direnv-update-environment-maybe)
      (+direnv-update-environment-maybe project-root))
    (+portal-open-in-directory buffer-name project-root)
    (+project-activate project-root t)))

(defun +project/kill-buffers (project-root)
  "Close file-visiting buffers in PROJECT-ROOT using Projectile's policy."
  (interactive (list (+project-read-target-root "Close project files: ")))
  (let ((default-directory (+project-normalize-root project-root)))
    (projectile-kill-buffers)))

(defun +project/add-known-project (project-root)
  "Register PROJECT-ROOT across the project workflow."
  (interactive "DAdd project: ")
  (setq project-root (+project-register-root project-root))
  (message "Registered project %s in the project workflow"
           (abbreviate-file-name project-root)))

(defalias 'project-add-to-workbench #'+project/add-known-project
  "Add a project to the Projectile and Perspective workbench.")

(defun +project-remove-from-projectile-known-projects (project-root)
  "Remove PROJECT-ROOT from `projectile-known-projects'.
This matches canonically, so symlinked roots are cleaned as well."
  (let ((before (length projectile-known-projects)))
    (setq projectile-known-projects
          (seq-remove
           (lambda (known-root)
             (+project-root-equal-p known-root project-root))
           projectile-known-projects))
    (unless (= before (length projectile-known-projects))
      (projectile-merge-known-projects)
      t)))

(defun +project-remove-from-project-list (project-root)
  "Remove PROJECT-ROOT from the built-in `project.el' project list."
  (when (require 'project nil t)
    (let ((removed nil))
      (dolist (known-root (project-known-project-roots))
        (when (+project-root-equal-p known-root project-root)
          (let ((inhibit-message t))
            (project-forget-project known-root))
          (setq removed t)))
      removed)))

(defun +project-treemacs-project-name (project-root &optional workspace)
  "Return a Treemacs-safe display name for PROJECT-ROOT in WORKSPACE."
  (let* ((workspace (or workspace (treemacs-current-workspace)))
         (base-name (+project-perspective-name project-root))
         (candidate base-name)
         (index 2))
    (while (seq-some
            (lambda (project)
              (and (not (+project-root-equal-p (treemacs-project->path project)
                                                 project-root))
                   (string= candidate (treemacs-project->name project))))
            (treemacs-workspace->projects workspace))
      (setq candidate (format "%s<%d>" base-name index)
            index (1+ index)))
    candidate))

(defun +project-add-to-treemacs-workspace (project-root)
  "Add PROJECT-ROOT to the current Treemacs workspace if needed."
  (when (require 'treemacs-workspaces nil t)
    (require 'treemacs-persistence nil t)
    (let* ((project-root (+project-normalize-root project-root))
           (workspace (treemacs-current-workspace))
           (path (+treemacs-project-path project-root)))
      (if (seq-some
           (lambda (project)
             (+project-root-equal-p (treemacs-project->path project) path))
           (treemacs-workspace->projects workspace))
          nil
        (pcase (treemacs-do-add-project-to-workspace
                path
                (+project-treemacs-project-name project-root workspace))
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

(defun +project-rollback-registration (project-root ignored-before
                                                     projectile-before native-before)
  "Undo new registrations for PROJECT-ROOT; return any cleanup failures.
BEFORE arguments record whether each owner already knew the project."
  (let (failures)
    (cl-labels ((attempt (label function)
                  (condition-case err
                      (funcall function)
                    (error
                     (push (format "%s: %s" label (error-message-string err))
                           failures)))))
      (unless native-before
        (attempt "project.el"
                 (lambda ()
                   (+project-remove-from-project-list project-root)
                   (when (+project-root-member-p
                          project-root (project-known-project-roots))
                     (error "project.el still remembers the root")))))
      (unless projectile-before
        (attempt "Projectile"
                 (lambda ()
                   (+project-remove-from-projectile-known-projects
                    project-root)
                   (when (+project-root-member-p
                          project-root (projectile-known-projects))
                     (error "Projectile still remembers the root")))))
      (when ignored-before
        (attempt "ignored roots"
                 (lambda ()
                   (+project-ignore-root project-root)
                   (unless (+project-ignored-root-p project-root)
                     (error "Ignored-project list did not restore the root"))))))
    (nreverse failures)))

(defun +project-register-root (project-root)
  "Register PROJECT-ROOT across project backends."
  (setq project-root (+project-normalize-root project-root))
  (unless (require 'projectile nil t)
    (user-error "Projectile is unavailable"))
  (when +project-ignored-roots-load-error
    (user-error "Repair the ignored-project file before adding projects: %s"
                +project-ignored-roots-load-error))
  (let ((existing-workspace
         (unless (+project-client-root-p project-root)
           (remote-get-workspace project-root)))
        (workspace nil)
        (registered nil)
        (registration-started nil)
        (ignored-before nil)
        (projectile-before nil)
        (native-before nil))
    (unwind-protect
        (condition-case err
            (progn
              ;; A remote root is registered through its Remote workspace, the
              ;; same owner its files, processes and environment use.
              (unless (+project-client-root-p project-root)
                (setq workspace
                      (remote-workspace-open
                       project-root :connect t :adapter "emacs-file"
                       :capability 'file-read :load-environment t)))
              (unless (file-directory-p project-root)
                (user-error "%s is not a directory"
                            (abbreviate-file-name project-root)))
              (let ((default-directory project-root))
                (unless (projectile-project-p project-root)
                  ;; Projectile caches "rootless" directories. If a directory
                  ;; becomes a project while Emacs runs, clear that cached miss.
                  (when (fboundp 'projectile-invalidate-cache)
                    (projectile-invalidate-cache nil))
                  (unless (projectile-project-p project-root)
                    (user-error "%s is not recognized as a project root (try `M-x projectile-invalidate-cache` or restart Emacs)"
                                (abbreviate-file-name project-root)))))
              (setq ignored-before (+project-ignored-root-p project-root)
                    projectile-before
                    (+project-root-member-p project-root
                                              (projectile-known-projects))
                    native-before
                    (and (require 'project nil t)
                         (+project-root-member-p
                          project-root (project-known-project-roots))))
              ;; Check Projectile's other exclusions before changing state.
              ;; Temporarily omit our own ignore entry without persisting it.
              (let ((+project-ignored-roots
                     (seq-remove
                      (lambda (root)
                        (+project-root-equal-p root project-root))
                      +project-ignored-roots)))
                (when (projectile-ignored-project-p project-root)
                  (user-error "Projectile excludes %s; adjust its ignored-project settings before adding it"
                              (abbreviate-file-name project-root))))
              (setq registration-started t)
              (+project-unignore-root project-root)
              (unless projectile-before
                (projectile-add-known-project project-root))
              (unless native-before
                (when (require 'project nil t)
                  (project-remember-project (cons 'transient project-root))))
              (unless (+project-root-member-p
                       project-root (projectile-known-projects))
                (user-error "Projectile did not register %s"
                            (abbreviate-file-name project-root)))
              (setq registered t)
              project-root)
          ((error quit)
           (let ((failures
                  (when registration-started
                    (+project-rollback-registration
                     project-root ignored-before projectile-before
                     native-before))))
             (when failures
               (display-warning
                'init-project
                (format "Project registration failed; cleanup incomplete (%s)"
                        (mapconcat #'identity failures "; "))
                :error))
             (signal (car err) (cdr err)))))
      (when (and (not registered)
                 (not existing-workspace)
                 (remote-workspace-p workspace))
        (condition-case err
            (remote-workspace-close workspace 'project-registration-failed)
          (error
           (display-warning
            'init-project
            (format "Could not close workspace after project registration failed: %s"
                    (error-message-string err))
            :error)))))))

(defun +project--filter-visible-roots (roots)
  "Return ROOTS without entries that should stay hidden from project UIs."
  (seq-uniq
   (seq-remove #'+project-hidden-root-p
               (mapcar #'+project-normalize-root roots))))

(defun +project/prune-hidden-project-state ()
  "Remove ignored roots such as `~/` from persisted project state."
  (interactive)
  ;; A damaged ignored-root file may hide additional roots.  Wait for an
  ;; explicit reload before changing either package's persisted project list.
  (unless +project-ignored-roots-load-error
    (when (require 'projectile nil t)
      (projectile-load-known-projects)
      (let ((filtered (+project--filter-visible-roots projectile-known-projects)))
        (unless (equal filtered projectile-known-projects)
          (setq projectile-known-projects filtered)
          (projectile-merge-known-projects))))
    (when (require 'project nil t)
      (dolist (root (project-known-project-roots))
        (when (+project-hidden-root-p root)
          (let ((inhibit-message t))
            (project-forget-project root)))))))

(defun +show-imenu-target-root ()
  "Return the directory root used by `show-imenu'."
  (+project-normalize-root
   (or (+project-current-root) default-directory)))

(defun +show-imenu-enable-treemacs-modes ()
  "Enable Treemacs features used for project and symbol following."
  (+project-ensure-treemacs)
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
  (+treemacs-reset-follow-state)
  (treemacs-project-follow-mode 1)
  (+treemacs-cursor-follow-mode 1))

(defconst +show-imenu-breadcrumb-refresh-delays '(0.05 0.35 1.0)
  "Retry delays used to refresh breadcrumbs after opening Treemacs.")

(defun +show-imenu-refresh-breadcrumb (buffer)
  "Synchronously refresh breadcrumb state in source BUFFER."
  (when (and (buffer-live-p buffer)
             (require 'breadcrumb nil t))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'prog-mode)
                 (fboundp 'breadcrumb-local-mode))
        (when (boundp 'breadcrumb--ipath-plain-cache)
          (setq-local breadcrumb--ipath-plain-cache nil))
        (when (boundp 'breadcrumb--last-update-tick)
          (setq-local breadcrumb--last-update-tick -1))
        (let ((non-essential t)
              (imenu-auto-rescan t))
          (ignore-errors
            (imenu--make-index-alist t)))
        (if (bound-and-true-p lsp-managed-mode)
            ;; LSP owns the shared tab-line lane.  Re-enabling the generic
            ;; provider here used to recreate the old header for each delayed
            ;; Treemacs refresh after entering a file.
            (when (fboundp 'lsp-headerline-check-breadcrumb)
              (ignore-errors (lsp-headerline-check-breadcrumb)))
          (breadcrumb-local-mode -1)
          (breadcrumb-local-mode 1)
          (when (fboundp '+breadcrumb-tab-line-sync-h)
            (+breadcrumb-tab-line-sync-h)))
        (force-mode-line-update)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (force-window-update window))))

(defun +show-imenu-restore-breadcrumb (buffer)
  "Restore breadcrumb header-line state in source BUFFER after Treemacs setup."
  (+show-imenu-refresh-breadcrumb buffer)
  (dolist (delay +show-imenu-breadcrumb-refresh-delays)
    (run-at-time delay nil #'+show-imenu-refresh-breadcrumb buffer)))

(defun +treemacs-markerize-imenu-position (position)
  "Convert integer imenu POSITION values into markers for Treemacs."
  (cond
   ((markerp position) position)
   ((integerp position)
    (let ((marker (make-marker)))
      (set-marker marker position (current-buffer))
      marker))
   (t position)))

(defun +treemacs-normalize-flat-imenu-index (flat-index)
  "Normalize FLAT-INDEX so Treemacs always receives marker positions."
  (if (eq flat-index 'unsupported)
      flat-index
    (mapcar
     (lambda (tag-path)
       (let* ((leaf (copy-tree (car tag-path)))
              (position (cdr leaf)))
         (setcdr leaf (+treemacs-markerize-imenu-position position))
         (cons leaf (cdr tag-path))))
     flat-index)))

(defun +treemacs-org-imenu-entry-label (entry)
  "Return ENTRY's label without text properties."
  (when (consp entry)
    (substring-no-properties (format "%s" (car entry)))))

(defun +treemacs-org-imenu-entry-p (entry)
  "Return non-nil when ENTRY looks like an Org imenu entry."
  (and (consp entry)
       (stringp (car entry))
       (get-text-property 0 'org-imenu-marker (car entry))))

(defun +treemacs-org-imenu-index-p (index)
  "Return non-nil when INDEX contains Org imenu entries."
  (and (listp index)
       (seq-some
        (lambda (entry)
          (or (+treemacs-org-imenu-entry-p entry)
              (and (imenu--subalist-p entry)
                   (+treemacs-org-imenu-index-p (cdr entry)))))
        index)))

(defun +treemacs-org-imenu-duplicate-leaf-p (node leaf)
  "Return non-nil when LEAF duplicates Org parent NODE."
  (and (imenu--subalist-p node)
       (+treemacs-org-imenu-entry-p leaf)
       (not (imenu--subalist-p leaf))
       (string= (+treemacs-org-imenu-entry-label node)
                (+treemacs-org-imenu-entry-label leaf))))

(defun +treemacs-prune-org-imenu-index (index)
  "Remove duplicate parent leaves from an Org imenu INDEX.
`org-imenu-get-tree' emits a parent headline twice when it has children: once
as a subalist and once as a leaf.  Treemacs renders both, which makes Org files
with generated overviews noisy and can leave stale duplicate tag paths."
  (let (result)
    (while index
      (let* ((entry (car index))
             (next (cadr index))
             (entry (if (imenu--subalist-p entry)
                        (cons (car entry)
                              (+treemacs-prune-org-imenu-index (cdr entry)))
                      entry)))
        (push entry result)
        (setq index
              (if (+treemacs-org-imenu-duplicate-leaf-p entry next)
                  (cddr index)
                (cdr index)))))
    (nreverse result)))

(defconst +treemacs-imenu-category-kinds
  '(("Files" . 1)
    ("Modules" . 2)
    ("Namespaces" . 3)
    ("Packages" . 4)
    ("Classes" . 5)
    ("Methods" . 6)
    ("Properties" . 7)
    ("Fields" . 8)
    ("Constructors" . 9)
    ("Enums" . 10)
    ("Interfaces" . 11)
    ("Functions" . 12)
    ("Variables" . 13)
    ("Constants" . 14)
    ("Strings" . 15)
    ("Numbers" . 16)
    ("Booleans" . 17)
    ("Arrays" . 18)
    ("Objects" . 19)
    ("Keys" . 20)
    ("Nulls" . 21)
    ("Enum Members" . 22)
    ("Structs" . 23)
    ("Events" . 24)
    ("Operators" . 25)
    ("Type Parameters" . 26)
    ;; Common non-LSP Imenu headings.
    ("Types" . 23)
    ("Records" . 23))
  "Map Imenu category labels to Language Server Protocol SymbolKinds.")

(defconst +treemacs-symbol-icon-specs
  '((1 "nf-cod-symbol_file" font-lock-doc-face)
    (2 "nf-cod-symbol_namespace" font-lock-constant-face)
    (3 "nf-cod-symbol_namespace" font-lock-constant-face)
    (4 "nf-cod-symbol_namespace" font-lock-constant-face)
    (5 "nf-cod-symbol_class" font-lock-type-face)
    (6 "nf-cod-symbol_method" font-lock-function-name-face)
    (7 "nf-cod-symbol_property" font-lock-variable-name-face)
    (8 "nf-cod-symbol_field" font-lock-variable-name-face)
    (9 "nf-cod-symbol_method" font-lock-function-name-face)
    (10 "nf-cod-symbol_enum" font-lock-type-face)
    (11 "nf-cod-symbol_interface" font-lock-type-face)
    (12 "nf-cod-symbol_method" font-lock-function-name-face)
    (13 "nf-cod-symbol_variable" font-lock-variable-name-face)
    (14 "nf-cod-symbol_constant" font-lock-constant-face)
    (15 "nf-cod-symbol_string" font-lock-string-face)
    (16 "nf-cod-symbol_numeric" font-lock-constant-face)
    (17 "nf-cod-symbol_boolean" font-lock-constant-face)
    (18 "nf-cod-symbol_array" font-lock-type-face)
    (19 "nf-cod-symbol_misc" font-lock-type-face)
    (20 "nf-cod-symbol_key" font-lock-keyword-face)
    (21 "nf-cod-symbol_misc" font-lock-constant-face)
    (22 "nf-cod-symbol_enum_member" font-lock-constant-face)
    (23 "nf-cod-symbol_structure" font-lock-type-face)
    (24 "nf-cod-symbol_event" font-lock-warning-face)
    (25 "nf-cod-symbol_operator" font-lock-builtin-face)
    (26 "nf-cod-symbol_parameter" font-lock-variable-name-face)
    (0 "nf-cod-symbol_misc" shadow))
  "VS Code Codicons and faces used for typed Treemacs Imenu nodes.")

(defun +treemacs-imenu-category-kind (label)
  "Return the LSP SymbolKind represented by Imenu category LABEL."
  (cdr (assoc-string
        (string-trim (substring-no-properties label))
        +treemacs-imenu-category-kinds
        t)))

(defun +treemacs-imenu-inferred-kind (item parent-kind parent-label)
  "Infer ITEM's SymbolKind when LSP metadata is not yet available.
PARENT-KIND and PARENT-LABEL describe its semantic parent."
  (let* ((case-fold-search nil)
         (label (car item))
         (plain (string-trim (substring-no-properties label)))
         (exact (and (> (length label) 0)
                     (get-text-property 0 '+lsp-symbol-kind label)))
         (category (+treemacs-imenu-category-kind label))
         (node-p (imenu--subalist-p item)))
    (or exact
        category
        (and (string-match-p "\\`struct\\b" (downcase plain)) 23)
        (and (string-match-p "\\`interface\\b" (downcase plain)) 11)
        (and (string-match-p "\\`enum\\b" (downcase plain)) 10)
        (and node-p (string-match-p "\\`[[:upper:]]" plain) 5)
        (and (string-match-p "\\`[[:upper:]_][[:upper:][:digit:]_]*\\'" plain)
             14)
        (and (string-match-p "(" plain)
             (if (and (memq parent-kind '(5 11 23))
                      parent-label
                      (string= (car (split-string plain "(")) parent-label))
                 9
               (if (memq parent-kind '(5 11 23)) 6 12)))
        (and (string-match-p "\\`[[:word:]_$]+\\(?:\\.[[:word:]_$]+\\)+\\'" plain)
             3)
        ;; Category inheritance is useful for native Imenu indexes, but a
        ;; semantic class/struct is not a category for all its children.
        (and (not (memq parent-kind '(5 10 11 23))) parent-kind)
        (and node-p 19)
        13)))

(defun +treemacs-symbol-icon (kind)
  "Return a VS Code-style semantic icon for LSP SymbolKind KIND."
  (let* ((spec (or (assq kind +treemacs-symbol-icon-specs)
                   (assq 0 +treemacs-symbol-icon-specs)))
         (name (nth 1 spec))
         (face (nth 2 spec)))
    (if (require 'nerd-icons nil t)
        (nerd-icons-codicon name :face face :v-adjust -0.05)
      (propertize "•" 'face face))))

(defun +treemacs-decorate-imenu-index
    (index &optional parent-kind parent-label)
  "Add semantic icons to Imenu INDEX without changing its target positions.
PARENT-KIND and PARENT-LABEL support native Imenu fallbacks before LSP has
returned DocumentSymbols."
  (mapcar
   (lambda (item)
     (if (not (and (consp item) (stringp (car item))))
         item
       (let* ((label (car item))
              (kind (+treemacs-imenu-inferred-kind
                     item parent-kind parent-label))
              (plain (substring-no-properties label))
              (decorated
               (if (and (> (length label) 0)
                        (get-text-property 0 '+treemacs-symbol-decorated label))
                   label
                 (let ((value (concat (+treemacs-symbol-icon kind) " " label)))
                   (add-text-properties
                    0 (length value)
                    (list '+treemacs-symbol-decorated t
                          '+lsp-symbol-kind kind)
                    value)
                   value)))
              (category-kind (+treemacs-imenu-category-kind label))
              (child-kind (or category-kind kind))
              (child-parent (and (not category-kind) plain)))
         (if (imenu--subalist-p item)
             (cons decorated
                   (+treemacs-decorate-imenu-index
                    (cdr item) child-kind child-parent))
           (cons decorated (cdr item))))))
   index))

(defun +treemacs-imenu-header (file)
  "Return a typed outline header entry for FILE, or nil without a buffer."
  (when-let* ((logical (+treemacs-visit-path file))
              (buffer (or (get-file-buffer logical)
                          (get-file-buffer file)
                          (find-buffer-visiting logical)
                          (find-buffer-visiting file))))
    (with-current-buffer buffer
      (let ((label
             (propertize
              (format "OUTLINE · %s" (file-name-nondirectory logical))
              '+lsp-symbol-kind 1
              '+treemacs-outline-header t)))
        (cons label (copy-marker (point-min)))))))

(defun +treemacs-imenu-indentation-pattern (source-depth)
  "Return indentation components that visually erase SOURCE-DEPTH.
Treemacs keeps the original semantic button depths for DOM operations; only
the strings rendered before outline buttons are made shallow."
  (let* ((width (max 0 +treemacs-imenu-indent-width))
         (unit (make-string width ?\s)))
    (append (make-list (max 0 source-depth) "")
            ;; More than enough for real source-symbol nesting.  `treemacs'
            ;; cycles list indentation, so keep this tail deliberately wide.
            (make-list 128 unit))))

(defun +treemacs-expand-file-outline-shallow-a (fn button &rest arguments)
  "Render BUTTON's outline shallowly while calling FN with ARGUMENTS."
  (let* ((source-depth (or (treemacs-button-get button :depth) 0))
         (treemacs-indentation-string
          (+treemacs-imenu-indentation-pattern source-depth)))
    (apply fn button arguments)))

(defun +treemacs-expand-tag-outline-shallow-a (fn button &rest arguments)
  "Keep nested tag expansion shallow while calling FN for BUTTON."
  (let* ((path (treemacs-button-get button :path))
         (tag-depth (if (listp path) (max 1 (1- (length path))) 1))
         (button-depth (or (treemacs-button-get button :depth) tag-depth))
         (source-depth (max 0 (- button-depth tag-depth)))
         (treemacs-indentation-string
          (+treemacs-imenu-indentation-pattern source-depth)))
    (apply fn button arguments)))

(defun +treemacs-get-imenu-index-a (orig-fn file)
  "Post-process Treemacs imenu index from ORIG-FN for FILE."
  ;; FILE belongs to Treemacs' backend-consumable model and may therefore use
  ;; `/ssh:'.  Imenu, markers, and source buffers belong to Emacs' logical
  ;; buffer namespace.  Fetching tags through the model path would create a
  ;; second buffer for the same file and Treemacs may then kill that temporary
  ;; buffer after indexing.
  ;; Treemacs kills the buffer it indexed unless its `get-file-buffer' check
  ;; found one first.  That primitive misses a buffer visiting another
  ;; spelling of the same file (native vs `/fs:'), while `find-file-noselect'
  ;; still returns it -- so the person's own buffer would be killed.  Ask the
  ;; handler-aware `find-buffer-visiting' instead.
  (let* ((original-get-file-buffer (symbol-function 'get-file-buffer))
         (index (cl-letf (((symbol-function 'get-file-buffer)
                           (lambda (name)
                             (cl-letf (((symbol-function 'get-file-buffer)
                                        original-get-file-buffer))
                               (or (funcall original-get-file-buffer name)
                                   (find-buffer-visiting name))))))
                  ;; Indexing reads the file; it must not open it elsewhere.
                  (let ((+programmatic-file-visit t))
                    (funcall orig-fn (+treemacs-visit-path file)))))
         (index (if (+treemacs-org-imenu-index-p index)
                    (+treemacs-prune-org-imenu-index index)
                  index))
         (header (and index (+treemacs-imenu-header file))))
    (when header
      (setq index (cons header index)))
    (+treemacs-decorate-imenu-index index)))

(defun +treemacs-extract-position (item file)
  "Return buffer and position for Treemacs imenu ITEM in FILE.
This is a defensive variant of `treemacs--extract-position'.  Org imenu leaves
are dotted cons cells like (TITLE . MARKER), so PDF-specific proper-list checks
must not call `cadr' before the Org case has been handled."
  (cond
   ((markerp item)
    (cons (marker-buffer item) (marker-position item)))
   ((overlayp item)
    (cons (overlay-buffer item) (overlay-start item)))
   ((integerp item)
    (cons nil item))
   ((and (consp item)
         (stringp (car item))
         (get-text-property 0 'org-imenu-marker (car item)))
    (let ((org-marker (get-text-property 0 'org-imenu-marker (car item))))
      (cons (marker-buffer org-marker) (marker-position org-marker))))
   ((and (consp item)
         (markerp (cdr item)))
    (cons (marker-buffer (cdr item)) (marker-position (cdr item))))
   ((and (consp item)
         (consp (cdr item))
         (eq 'pdf-outline-imenu-activate-link (cadr item)))
    (with-no-warnings
      (cons (find-buffer-visiting (+treemacs-visit-path file))
            (lambda () (apply #'pdf-outline-imenu-activate-link item)))))))

(defun +breadcrumb-header-line-safely-a (fn &rest args)
  "Render breadcrumb FN without allowing a stale empty crumb to break redisplay.
Remote buffer identity changes can invalidate breadcrumb's cached imenu path
between two redisplay passes.  The package shortens each crumb with
`substring'; an empty stale crumb otherwise raises `args-out-of-range' from
the header-line evaluator."
  (condition-case error
      (apply fn args)
    ((args-out-of-range wrong-type-argument)
     (when (boundp 'breadcrumb--ipath-plain-cache)
       (setq-local breadcrumb--ipath-plain-cache nil))
     (when (boundp 'bc--ipath-plain-cache)
       (setq-local bc--ipath-plain-cache nil))
     (remote-log
      'breadcrumb-redisplay-recovered
      :buffer (buffer-name)
      :file buffer-file-name
      :error (error-message-string error))
     "")))

(defun +treemacs-flatten-and-sort-imenu-index ()
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
           (compare-func (if (eq major-mode 'adoc-mode)
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
            (setcdr leaf (+treemacs-markerize-imenu-position (cdr leaf))))))
      (sort flat-index compare-func))))

(defvar +treemacs-cursor-follow-timer nil
  "Idle timer used by `+treemacs-cursor-follow-mode'.")

(defvar +treemacs-last-follow-state nil
  "Last source context synchronized to Treemacs.")

(defvar +treemacs-cursor-follow--hooks-active nil
  "Whether Treemacs cursor-follow high-frequency hooks are installed.")

(config-defvar +treemacs-cursor-follow-delay nil
  "Idle delay before Treemacs follows the current file and symbol."
  :type 'number
  :group '+project)

(config-defvar +treemacs-tag-follow-excluded-modes nil
  "Major modes where custom Treemacs cursor follow should not enter tag nodes."
  :type '(repeat symbol)
  :group '+project)

(defun +treemacs-current-follow-state (&optional buffer window)
  "Return the follow state for BUFFER in WINDOW.
The state is used to avoid re-following when the source context did not
actually change."
  (let ((buffer (or buffer (current-buffer)))
        (window (or window (selected-window))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (list :buffer buffer
              :window window
              :file (+treemacs-source-file)
              :point (point)
              :tick (buffer-chars-modified-tick))))))

(defun +treemacs-reset-follow-state ()
  "Clear Treemacs caches used by file and tag following."
  (when (timerp +treemacs-cursor-follow-timer)
    (cancel-timer +treemacs-cursor-follow-timer))
  (setq +treemacs-cursor-follow-timer nil
        +treemacs-last-follow-state nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (boundp 'treemacs--imenu-cache)
        (setq-local treemacs--imenu-cache nil))
      (when (boundp 'treemacs--project-of-buffer)
        (setq-local treemacs--project-of-buffer nil))
      (when (boundp 'treemacs--previously-followed-tag-position)
        (setq-local treemacs--previously-followed-tag-position nil)))))

(defun +treemacs-reset-current-buffer-follow-state ()
  "Clear Treemacs follow cache for the current buffer only."
  (when (timerp +treemacs-cursor-follow-timer)
    (cancel-timer +treemacs-cursor-follow-timer))
  (setq +treemacs-cursor-follow-timer nil
        +treemacs-last-follow-state nil)
  (when (boundp 'treemacs--imenu-cache)
    (setq-local treemacs--imenu-cache nil))
  (when (boundp 'treemacs--project-of-buffer)
    (setq-local treemacs--project-of-buffer nil))
  (when (boundp 'treemacs--previously-followed-tag-position)
    (setq-local treemacs--previously-followed-tag-position nil)))

(defun +treemacs-source-buffer-p ()
  "Return non-nil when the current buffer should drive Treemacs following."
  (and (not (minibufferp))
       (not (derived-mode-p 'treemacs-mode))
       (or (+treemacs-source-file)
           (eq major-mode 'dired-mode))))

(defun +treemacs-follow-context-active-p (&optional buffer)
  "Return non-nil when BUFFER is still the active source for Treemacs follow.
When BUFFER is nil, use the current buffer in the selected window."
  (let ((window (selected-window))
        (target (or buffer (current-buffer))))
    (and (window-live-p window)
         (not (eq window (treemacs-get-local-window)))
         (eq (window-buffer window) target)
         (buffer-live-p target)
         (with-current-buffer target
           (+treemacs-source-buffer-p)))))

(defun +treemacs-find-current-user-project-a (orig-fn &rest args)
  "Return the current source project in Treemacs' physical namespace."
  (when (+treemacs-source-buffer-p)
    (when-let* ((root (apply orig-fn args)))
      (+treemacs-project-path root))))

(defun +treemacs-source-file ()
  "Return the real or Noema virtual file for the current buffer."
  (or buffer-file-name
      (and (boundp '+noema-buffer-file-name)
           (stringp +noema-buffer-file-name)
           (not (string-empty-p +noema-buffer-file-name))
           +noema-buffer-file-name)))

(defun +treemacs-follow-path ()
  "Return the current buffer path Treemacs should follow."
  (cond
   ((+treemacs-source-file)
    (+treemacs-project-path (+treemacs-source-file)))
   ((derived-mode-p 'dired-mode)
    (+treemacs-project-path default-directory))))

(defun +treemacs-normalize-workspace-paths ()
  "Migrate every Treemacs project to its backend-consumable path.
Return the number of changed projects.  This repairs persisted `/fs:local:'
entries created by older revisions before Treemacs starts native helpers."
  (when (and (require 'treemacs-workspaces nil t)
             (require 'treemacs-persistence nil t))
    ;; Force lazy persistence loading before traversing the registries.
    (treemacs-current-workspace)
    (let ((changed 0))
      (dolist (workspace
               (append (treemacs-workspaces)
                       (treemacs-disabled-workspaces)))
        (dolist (project (treemacs-workspace->projects workspace))
          (let* ((old (treemacs-project->path project))
                 (new
                  (condition-case err
                      (+treemacs-project-path old)
                    (error
                     (remote-log
                      'treemacs-path-migration-failed
                      :path old
                      :error (error-message-string err))
                     old))))
            (unless (equal old new)
              (let ((type 'treemacs-project))
                (aset
                 project
                 (cl-struct-slot-offset type 'path)
                 new)
                (aset
                 project
                 (cl-struct-slot-offset type 'path-status)
                 (treemacs--get-path-status new)))
              (setq changed (1+ changed))))))
      (when (> changed 0)
        (treemacs--invalidate-buffer-project-cache)
        (treemacs--persist))
      changed)))

(defun +treemacs-safe-imenu-index ()
  "Return the current buffer's Treemacs imenu index, or nil on failure."
  (when (and buffer-file-name
             (not (memq major-mode +treemacs-tag-follow-excluded-modes)))
    (condition-case nil
        (let ((inhibit-message t))
          (pcase (treemacs--flatten&sort-imenu-index)
            ('unsupported nil)
            (index index)))
      (error nil))))

(defun +treemacs-project-for-path (path)
  "Return the active Treemacs project containing PATH."
  (treemacs--find-project-for-buffer path))

(defun +treemacs-follow-source-silently (&optional prefer-tag)
  "Follow the current source buffer in Treemacs without transient failures.
When PREFER-TAG is non-nil, prefer following the current tag when one exists."
  (let* ((treemacs-window (treemacs-get-local-window))
         (path (+treemacs-follow-path)))
    (when (and treemacs-window path)
      (setq-local treemacs--project-of-buffer nil)
      (let ((project (+treemacs-project-for-path path)))
        (when project
          (let ((inhibit-message t)
                (treemacs-pulse-on-failure nil)
                (index (and prefer-tag (+treemacs-safe-imenu-index)))
                (tag-path nil)
                (followed nil))
            (setq tag-path
                  (and index
                       (ignore-errors
                         (treemacs--find-index-pos (point) index))))
            (setq followed
                  (or (condition-case nil
                          (when tag-path
                            ;; `treemacs--do-follow-tag' performs the move but
                            ;; returns nil when recentering is disabled.  The
                            ;; precomputed tag path is the success predicate;
                            ;; otherwise the old code immediately moved the
                            ;; cursor back to the file node.
                            (treemacs--do-follow-tag
                             index treemacs-window path project)
                            'tag)
                        (error nil))
                      (condition-case nil
                          (with-selected-window treemacs-window
                            (and (treemacs-goto-file-node path project) 'file))
                        (error nil))))
            (when followed
              (with-selected-window treemacs-window
                (hl-line-highlight)
                (set-window-point treemacs-window (point))
                (force-window-update treemacs-window)))
            followed))))))

(defun +treemacs-follow-current-buffer (buffer)
  "Update Treemacs to follow BUFFER's current file and symbol."
  (setq +treemacs-cursor-follow-timer nil)
  (when (and (buffer-live-p buffer)
             (window-live-p (treemacs-get-local-window))
             (+treemacs-follow-context-active-p buffer))
    (with-current-buffer buffer
      (when (+treemacs-follow-source-silently t)
        (setq +treemacs-last-follow-state
              (+treemacs-current-follow-state buffer (selected-window)))))))

(defun +treemacs-schedule-follow (&rest _)
  "Schedule a Treemacs follow update for the current source buffer."
  (when (timerp +treemacs-cursor-follow-timer)
    (cancel-timer +treemacs-cursor-follow-timer)
    (setq +treemacs-cursor-follow-timer nil))
  (when (and +treemacs-cursor-follow-mode
             (window-live-p (treemacs-get-local-window))
             (+treemacs-follow-context-active-p))
    (let* ((buffer (current-buffer))
           (state (+treemacs-current-follow-state buffer (selected-window))))
      (unless (equal state +treemacs-last-follow-state)
        (setq +treemacs-cursor-follow-timer
              (run-with-idle-timer +treemacs-cursor-follow-delay nil
                                   #'+treemacs-follow-current-buffer
                                   buffer))))))

(defun +treemacs-cursor-follow-window-visible-p ()
  "Return non-nil when a Treemacs window is available for following."
  (and (fboundp 'treemacs-get-local-window)
       (window-live-p (treemacs-get-local-window))))

(defun +treemacs-cursor-follow-enable-hooks ()
  "Install high-frequency hooks used while Treemacs is visible."
  (unless +treemacs-cursor-follow--hooks-active
    (setq +treemacs-cursor-follow--hooks-active t)
    (add-hook 'post-command-hook #'+treemacs-schedule-follow)
    (add-hook 'first-change-hook #'+treemacs-reset-current-buffer-follow-state)))

(defun +treemacs-cursor-follow-disable-hooks ()
  "Remove high-frequency hooks used by Treemacs cursor following."
  (when +treemacs-cursor-follow--hooks-active
    (setq +treemacs-cursor-follow--hooks-active nil)
    (remove-hook 'post-command-hook #'+treemacs-schedule-follow)
    (remove-hook 'first-change-hook #'+treemacs-reset-current-buffer-follow-state))
  (when (timerp +treemacs-cursor-follow-timer)
    (cancel-timer +treemacs-cursor-follow-timer))
  (setq +treemacs-cursor-follow-timer nil))

(defun +treemacs-cursor-follow-sync-hooks (&rest _)
  "Keep Treemacs follow hooks installed only while Treemacs is visible."
  (if (and +treemacs-cursor-follow-mode
           (+treemacs-cursor-follow-window-visible-p))
      (progn
        (+treemacs-cursor-follow-enable-hooks)
        (+treemacs-schedule-follow))
    (+treemacs-cursor-follow-disable-hooks)))

(defun +treemacs-process-file-events-safely-a (orig-fn &rest args)
  "Ignore Treemacs' harmless stale file-event race.

A file notification can schedule a refresh just before its tree node or buffer
is torn down.  Some refresh paths then try to use nil as a vector; others try
to enumerate a directory after a build tool has removed it.  The next file
event or an explicit refresh will rebuild the affected node, so only suppress
those exact races and preserve every other error for debugging."
  (condition-case err
      (apply orig-fn args)
    (wrong-type-argument
     (unless (equal err '(wrong-type-argument arrayp nil))
       (signal (car err) (cdr err))))
    (file-missing
     (unless (equal (cadr err) "Opening directory")
       (signal (car err) (cdr err))))))

(define-minor-mode +treemacs-cursor-follow-mode
  "Follow the current file and symbol in Treemacs."
  :init-value nil
  :global t
  :lighter nil
  (if +treemacs-cursor-follow-mode
      (progn
        (+treemacs-reset-follow-state)
        (add-hook 'window-configuration-change-hook
                  #'+treemacs-cursor-follow-sync-hooks)
        (+treemacs-cursor-follow-sync-hooks))
    (remove-hook 'window-configuration-change-hook
                 #'+treemacs-cursor-follow-sync-hooks)
    (+treemacs-cursor-follow-disable-hooks)
    (setq +treemacs-cursor-follow-timer nil
          +treemacs-last-follow-state nil)))

(defun show-imenu ()
  "Toggle a left Treemacs view that follows the current file and tag."
  (interactive)
  (+project-ensure-treemacs)
  (if-let* ((window (treemacs-get-local-window)))
      (delete-window window)
    (let ((source-buffer (current-buffer))
          (root (+show-imenu-target-root)))
      (+show-imenu-enable-treemacs-modes)
      (save-selected-window
        (let ((default-directory root))
          (treemacs-add-and-display-current-project-exclusively)
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (+treemacs-follow-source-silently t))
            (+show-imenu-restore-breadcrumb source-buffer)))))))

(defun +treemacs-focus-and-pulse-current-node (window)
  "Select Treemacs WINDOW and pulse its current file or Imenu node."
  (when (window-live-p window)
    (select-window window)
    (set-window-point window (point))
    (hl-line-highlight)
    (treemacs-pulse-on-success)
    window))

(defun +project/show-imenu-open-current-symbol ()
  "Open Treemacs, follow the current source symbol, select it and pulse it.
Unlike `show-imenu', this command is an ensure-open action and never closes an
already visible Treemacs window."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (root (+show-imenu-target-root))
         (treemacs-window nil)
         (opened nil)
         (followed nil))
    (+project-ensure-treemacs)
    (setq treemacs-window (treemacs-get-local-window))
    (unless (window-live-p treemacs-window)
      (setq opened t)
      (+show-imenu-enable-treemacs-modes)
      (save-selected-window
        (let ((default-directory root))
          (treemacs-add-and-display-current-project-exclusively)))
      (setq treemacs-window (treemacs-get-local-window)))
    (unless (window-live-p treemacs-window)
      (user-error "Treemacs did not open for %s" root))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (setq followed (+treemacs-follow-source-silently t)))
      ;; A visible Treemacs may currently show another project exclusively.
      ;; Repoint that same window once, instead of treating it as an open/close
      ;; toggle or silently leaving the click without a target node.
      (when (and (not followed) (not opened))
        (save-selected-window
          (let ((default-directory root))
            (treemacs-add-and-display-current-project-exclusively)))
        (setq opened t
              treemacs-window (treemacs-get-local-window))
        (when (window-live-p treemacs-window)
          (with-current-buffer source-buffer
            (setq followed (+treemacs-follow-source-silently t)))))
      (when opened
        (+show-imenu-restore-breadcrumb source-buffer)))
    (when followed
      (+treemacs-focus-and-pulse-current-node treemacs-window))))

(defun +project/show-imenu-from-breadcrumb (event)
  "Handle breadcrumb mouse EVENT and reveal the current symbol in Treemacs."
  (interactive "e")
  (when-let* ((position (event-start event))
              (window (posn-window position))
              ((window-live-p window)))
    (select-window window))
  (+project/show-imenu-open-current-symbol))

(defun +project-remove-from-treemacs-workspaces (project-root)
  "Remove PROJECT-ROOT from every Treemacs workspace."
  (when (require 'treemacs-workspaces nil t)
    (require 'treemacs-persistence nil t)
    (let ((target (+project-canonical-path project-root))
          (removed nil))
      (dolist (workspace (append (treemacs-workspaces)
                                 (treemacs-disabled-workspaces)))
        (let* ((projects (treemacs-workspace->projects workspace))
               (filtered
                (seq-remove
                 (lambda (project)
                   (equal (+project-canonical-path (treemacs-project->path project))
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

(defun +project-remove-projectile-cache (project-root)
  "Remove Projectile cache data for PROJECT-ROOT."
  (let* ((project-root (+project-normalize-root project-root))
         (cache-file (projectile-project-cache-file project-root)))
    (dolist (table '(projectile-project-type-cache
                     projectile-projects-cache
                     projectile-projects-cache-time
                     projectile--dirconfig-cache))
      (when (boundp table)
        (maphash
         (lambda (key _value)
           (when (+project-root-equal-p key project-root)
             (remhash key (symbol-value table))))
         (symbol-value table))))
    (when (and cache-file (file-exists-p cache-file))
      (delete-file cache-file))))

(defun +project-kill-associated-buffers (project-root &optional project-label)
  "Kill buffers associated with PROJECT-ROOT.
Return a plist with counts of killed and preserved buffers and errors."
  (let* ((project-root (+project-normalize-root project-root))
         (project-label (or project-label
                            (+project-perspective-name project-root)))
         (buffers (seq-filter
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (or (+project-path-inside-root-p buffer-file-name project-root)
                           (+project-path-inside-root-p default-directory project-root)
                           (equal (buffer-name)
                                  (format "*portal:%s*" project-label)))))
                   (buffer-list)))
         (killed 0)
         (preserved 0)
         (errors nil))
    (dolist (buffer (append (delq (current-buffer) buffers)
                            (when (memq (current-buffer) buffers)
                              (list (current-buffer)))))
      (when (buffer-live-p buffer)
        (condition-case err
            (kill-buffer buffer)
          (error
           (push (format "%s: %s" (buffer-name buffer)
                         (error-message-string err))
                 errors)))
        (if (buffer-live-p buffer)
            (setq preserved (1+ preserved))
          (setq killed (1+ killed)))))
    (list :killed killed :preserved preserved :errors (nreverse errors))))

(defun +project-kill-perspective-if-exists (project-name)
  "Kill the perspective named PROJECT-NAME if it exists."
  (when (require 'perspective nil t)
    (when (member project-name (persp-names))
      (persp-kill project-name)
      t)))

(defun +project-read-removable-root ()
  "Read a project root, including previously ignored roots for retry."
  (let* ((roots (delete-dups
                 (append (+project-known-projects)
                         (copy-sequence +project-ignored-roots)
                         (when +project-active-root
                           (list +project-active-root)))))
         (root (completing-read "Remove project: " roots nil nil nil nil
                                (+project-current-root))))
    (when (string-empty-p root)
      (user-error "Choose a project root to remove"))
    (+project-normalize-root root)))

(defun +project/remove-known-project (project-root)
  "Remove PROJECT-ROOT from Emacs project state and clean related buffers."
  (interactive (list (+project-read-removable-root)))
  (setq project-root (+project-normalize-root project-root))
  (let ((project-label (+project-perspective-name project-root))
        (failures nil)
        (buffers nil))
    (unless (yes-or-no-p
             (format "Remove project %s from Projectile/project.el/Treemacs/perspective state? "
                     (abbreviate-file-name project-root)))
      (user-error "Project removal cancelled"))
    (cl-labels
        ((attempt (label function)
           (condition-case err
               (funcall function)
             (error
              (push (format "%s: %s" label (error-message-string err))
                    failures)))))
      (attempt "ignored roots" (lambda () (+project-ignore-root project-root)))
      (attempt "Projectile list"
               (lambda () (+project-remove-from-projectile-known-projects
                           project-root)))
      (attempt "project.el list"
               (lambda () (+project-remove-from-project-list project-root)))
      (attempt "Projectile cache"
               (lambda () (+project-remove-projectile-cache project-root)))
      (attempt "Treemacs"
               (lambda () (+project-remove-from-treemacs-workspaces
                           project-root)))
      (attempt "Perspective"
               (lambda () (+project-kill-perspective-if-exists project-label)))
      (attempt "buffers"
               (lambda ()
                 (setq buffers
                       (+project-kill-associated-buffers
                        project-root project-label))))
      (when (and +project-active-root
                 (remote-file-equal-p +project-active-root project-root))
        (attempt "active project" #'+project/leave)))
    (dolist (error (plist-get buffers :errors))
      (push (concat "buffer " error) failures))
    (if failures
        (user-error "Project %s removal incomplete (%s); retry removal"
                    (abbreviate-file-name project-root)
                    (mapconcat #'identity (nreverse failures) "; "))
      (message "Removed project %s; closed %d buffer(s), preserved %d"
               (abbreviate-file-name project-root)
               (or (plist-get buffers :killed) 0)
               (or (plist-get buffers :preserved) 0)))))

(defalias 'project-remove-from-workbench #'+project/remove-known-project
  "Remove a project from the workbench and clean associated state.")

(defun +project-new-projectile-roots (known-before)
  "Return Projectile roots added since KNOWN-BEFORE, comparing identities."
  (seq-remove
   (lambda (project-root)
     (seq-some
      (lambda (known-root)
        (+project-root-equal-p known-root project-root))
      known-before))
   (mapcar #'+project-normalize-root projectile-known-projects)))

(defun +project-forget-failed-discovery (project-root)
  "Remove newly discovered PROJECT-ROOT from Projectile; return error text."
  (condition-case err
      (progn
        (+project-remove-from-projectile-known-projects project-root)
        (when (+project-root-member-p
               project-root (projectile-known-projects))
          (error "Projectile still remembers the root"))
        nil)
    ((error quit) (error-message-string err))))

(defun +project-clean-up-discovered-roots (roots)
  "Forget ROOTS added by an incomplete scan; return cleanup failures."
  (let (failures)
    (dolist (root roots)
      (when-let* ((cleanup-error (+project-forget-failed-discovery root)))
        (push (format "%s: %s" root cleanup-error) failures)))
    (nreverse failures)))

(defun +project/discover-projects-in-directory (directory depth)
  "Discover projects under DIRECTORY up to DEPTH levels deep."
  (interactive
   (list (read-directory-name "Discover projects in: " nil nil t)
         (read-number "Discovery depth: " 2)))
  (when +project-ignored-roots-load-error
    (user-error "Repair the ignored-project file before discovering projects: %s"
                +project-ignored-roots-load-error))
  (let ((known-before (mapcar #'+project-normalize-root
                              (projectile-known-projects)))
        (failures nil)
        (registered-count 0))
    (condition-case err
        (projectile-discover-projects-in-directory directory depth)
      ((error quit)
       (setq failures
             (+project-clean-up-discovered-roots
              (+project-new-projectile-roots known-before)))
       (when (and failures (eq (car err) 'quit))
         (display-warning
          'init-project
          (format "Project discovery was cancelled; cleanup incomplete (%s)"
                  (mapconcat #'identity failures "; "))
          :error))
       (if (and failures (not (eq (car err) 'quit)))
           (user-error "Project discovery failed: %s; cleanup incomplete (%s)"
                       (error-message-string err)
                       (mapconcat #'identity failures "; "))
         (signal (car err) (cdr err)))))
    (let ((new-projects (+project-new-projectile-roots known-before)))
      (dolist (root new-projects)
        (condition-case err
            (progn
              (+project-register-root root)
              (cl-incf registered-count))
          (error
           (let ((cleanup-error (+project-forget-failed-discovery root)))
             (push (format "%s: %s%s"
                           (abbreviate-file-name root)
                           (error-message-string err)
                           (if cleanup-error
                               (format "; Projectile cleanup: %s" cleanup-error)
                             ""))
                   failures)))
          (quit
           (when-let* ((cleanup-failures
                        (+project-clean-up-discovered-roots
                         (member root new-projects))))
             (display-warning
              'init-project
              (format "Project discovery was cancelled; cleanup incomplete (%s)"
                      (mapconcat #'identity cleanup-failures "; "))
              :error))
           (signal (car err) (cdr err)))))
      (if failures
          (user-error "Discovered %d project(s), registered %d; failed: %s"
                      (length new-projects) registered-count
                      (mapconcat #'identity (nreverse failures) "; "))
        (message "Discovered and registered %d project(s) under %s"
                 registered-count (abbreviate-file-name directory))))))

(defalias 'project-discover-in-directory
  #'+project/discover-projects-in-directory
  "Discover projects under a directory and sync workbench state.")

(declare-function projectile--wildcard-p "projectile" (name))
(declare-function remote-ancestor-directory-names "remote-fs" (directory))
(declare-function projectile-file-exists-p "projectile" (file))
(declare-function projectile-expand-file-name-wildcard "projectile"
                  (name-pattern dir))

(defun +project--directory-marker (directory markers &optional files-only)
  "Return the first of MARKERS present in DIRECTORY, or nil.

This replaces `projectile--directory-marker' so that a wildcard marker
without a path separator is answered from the directory listing the call
already makes, instead of expanding it through its own
`file-expand-wildcards'.  Projectile ships eight of those markers
(`?*.sln', `?*.xcodeproj', ...), so walking four levels up to resolve a
project root costs 32 extra directory listings.  Every one of them is a
separate round trip on a target, which made this the single largest cost of
opening a remote file here.

A marker carrying a path separator still has to be probed directly.  With
FILES-ONLY non-nil a marker naming a directory does not count as present, and
that extra stat is only paid for a marker that actually matched."
  (let ((entries nil)
        (names nil)
        (listed nil))
    (cl-labels
        ((listing ()
           (unless listed
             ;; Where a lookup is a round trip, the framework has already
             ;; listed the ancestors while scanning for markers.
             (setq names (let ((known (remote-ancestor-directory-names
                                       directory)))
                           (if known
                               (cdr known)
                             (ignore-errors
                               (directory-files
                                directory nil
                                directory-files-no-dot-files-regexp t))))
                   entries (let ((set (make-hash-table :test #'equal)))
                             (dolist (name names set)
                               (puthash name t set)))
                   listed t))
           entries)
         (usable-p (name)
           (or (not files-only)
               (not (file-directory-p (expand-file-name name directory))))))
      (seq-find
       (lambda (marker)
         (cond
          ((not (stringp marker)) nil)
          ((string-search "/" marker)
           (let ((expanded
                  (projectile-expand-file-name-wildcard marker directory)))
             (and (projectile-file-exists-p expanded)
                  (or (not files-only)
                      (not (file-directory-p expanded))))))
          ((projectile--wildcard-p marker)
           (let ((regexp (wildcard-to-regexp marker)))
             (listing)
             (seq-find
              (lambda (name)
                (and (string-match-p regexp name) (usable-p name)))
              names)))
          (t
           (and (listing)
                (gethash marker entries)
                (usable-p marker)))))
       markers))))

(defun +project--directory-marker-compatible-p ()
  "Return non-nil when Projectile's marker scan still has its known shape."
  (and (fboundp 'projectile--directory-marker)
       (fboundp 'projectile--wildcard-p)
       (fboundp 'projectile-file-exists-p)
       (fboundp 'projectile-expand-file-name-wildcard)
       (equal (func-arity 'projectile--directory-marker) '(2 . 3))))

(with-eval-after-load 'projectile
  ;; The shape check has to run before the override, because `func-arity'
  ;; reports the advice once one is installed.
  (unless (advice-member-p #'+project--directory-marker
                           'projectile--directory-marker)
    (if (+project--directory-marker-compatible-p)
        (advice-add 'projectile--directory-marker
                    :override #'+project--directory-marker)
      (display-warning
       'init-project
       "projectile--directory-marker changed shape; leaving its wildcard markers unaccelerated"
       :warning))))

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
  (define-key projectile-command-map (kbd ".") #'project-menu)
  (define-key projectile-command-map (kbd "p") #'+project/switch)
  (define-key projectile-command-map (kbd "o") #'+project/open-workbench)
  (define-key projectile-command-map (kbd "f") #'+project/find-file)
  (define-key projectile-command-map (kbd "r") #'+project/recent-file)
  (define-key projectile-command-map (kbd "b") #'+project/switch-buffer)
  (define-key projectile-command-map (kbd "g") #'+project/ripgrep)
  (define-key projectile-command-map (kbd "d") #'+project/open-root)
  (define-key projectile-command-map (kbd "m") #'+project/magit-status)
  (define-key projectile-command-map (kbd "v") #'+project/portal)
  (unless (advice-member-p #'+project/find-file-in-known-projects
                           'projectile-find-file-in-known-projects)
    (advice-add 'projectile-find-file-in-known-projects :override
                #'+project/find-file-in-known-projects))
  (setq projectile-mode-line "Projectile"
        projectile-dynamic-mode-line nil
        projectile-track-known-projects-automatically nil)
  (+project/prune-hidden-project-state)
  :custom
  (projectile-use-git-grep t)
  (projectile-ignored-project-function #'+project-ignored-root-p)
  (projectile-indexing-method 'alien)
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-project-search-path +project-search-paths)
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
  (transient-define-prefix project-menu ()
    "Project workbench."
    :transient-suffix 'transient--do-stay
    [["Switch"
      ("p" "switch project" +project/switch
       :transient transient--do-exit)
      ("f" "find file" +project/find-file)
      ("r" "recent file" +project/recent-file)
      ("b" "switch buffer" +project/switch-buffer)]
     ["Search"
      ("s" "search project" +project/ripgrep)
      ("a" "all-project files" +project/find-file-in-known-projects)
      ("g" "magit" +project/magit-status)]
     ["Open / Shell"
      ("d" "open root" +project/open-root)
      ("v" "project portal" +project/portal)
      ("j" "Jupyter SSH project" +jupyter/ssh-open-project)]
     ["Manage"
      ("l" "leave active project" +project/leave)
      ("A" "add project" project-add-to-workbench)
      ("D" "discover in dir" project-discover-in-directory)
      ("R" "reload ignored roots" project-reload-ignored-roots)
      ("x" "remove project" project-remove-from-workbench)
      ("k" "close project files" +project/kill-buffers)]]))

(use-package treemacs
  :ensure t
  :defer t
  :hook (treemacs-mode . (lambda ()
                           (display-line-numbers-mode -1)
                           (+treemacs-apply-elegant-ui)))
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
  (treemacs-modify-theme "Default"
    :icon-directory (expand-file-name "assets/icons" user-emacs-directory)
    :config
    (let ((lean-icon (+file-icon-for-file
                      "file.lean" :height 1.0 :v-adjust -0.05)))
      (when lean-icon
        (treemacs-create-icon
         :icon lean-icon
         :extensions ("lean")
         :fallback "L "))
      (treemacs-create-icon
       :file "Noema.svg"
       :extensions ("md")
       :fallback "✎ ")
      (treemacs-create-icon
       :file "NoemaWork.svg"
       :extensions ("noema")
       :fallback "◇ ")
      ;; Semantic Codicons live in the Imenu labels.  Keep the disclosure
      ;; column blank for leaves instead of showing a second generic red dot.
      (treemacs-create-icon
       :icon "  "
       :extensions (tag-leaf)
       :fallback 'same-as-icon)))
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
        treemacs-missing-project-action 'keep
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
                   #'+treemacs-normalize-flat-imenu-index)
    (advice-remove 'treemacs--flatten&sort-imenu-index
                   #'+treemacs-flatten-and-sort-imenu-index)
    (advice-add 'treemacs--flatten&sort-imenu-index
                :override
                #'+treemacs-flatten-and-sort-imenu-index))
  (with-eval-after-load 'treemacs-tags
    (unless (advice-member-p
             #'+treemacs-expand-file-outline-shallow-a
             'treemacs--expand-file-node)
      (advice-add
       'treemacs--expand-file-node
       :around #'+treemacs-expand-file-outline-shallow-a))
    (unless (advice-member-p
             #'+treemacs-expand-tag-outline-shallow-a
             'treemacs--expand-tag-node)
      (advice-add
       'treemacs--expand-tag-node
       :around #'+treemacs-expand-tag-outline-shallow-a))
    (advice-remove 'treemacs--get-imenu-index
                   #'+treemacs-get-imenu-index-a)
    (advice-add 'treemacs--get-imenu-index
                :around
                #'+treemacs-get-imenu-index-a)
    (advice-remove 'treemacs--extract-position
                   #'+treemacs-extract-position)
    (advice-add 'treemacs--extract-position
                :override
                #'+treemacs-extract-position)
    (unless (advice-member-p
             #'+treemacs-visit-logical-path-a
             'treemacs--call-imenu-and-goto-tag)
      (advice-add
       'treemacs--call-imenu-and-goto-tag
       :around #'+treemacs-visit-logical-path-a))
    (dolist (command '(treemacs--call-imenu-and-goto-tag treemacs--goto-tag))
      (unless (advice-member-p #'+treemacs-after-tag-visit-a command)
        (advice-add command :after #'+treemacs-after-tag-visit-a)))
    (unless (advice-member-p #'+treemacs-tag-visit-programmatic-a
                             'treemacs--call-imenu-and-goto-tag)
      (advice-add 'treemacs--call-imenu-and-goto-tag
                  :around #'+treemacs-tag-visit-programmatic-a)))
  (with-eval-after-load 'treemacs-mouse-interface
    (dolist (command
             '(treemacs--imenu-tag-noselect
               treemacs-node-buffer-and-position))
      (unless (advice-member-p #'+treemacs-visit-logical-path-a command)
        (advice-add command :around #'+treemacs-visit-logical-path-a))))
  (with-eval-after-load 'treemacs-filewatch-mode
    (advice-remove 'treemacs--process-file-events
                   #'+treemacs-process-file-events-safely-a)
    (advice-add 'treemacs--process-file-events
                :around
                #'+treemacs-process-file-events-safely-a))
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
  ;; Treemacs' ignored-file prefetch can emit invalid Lisp for Git-quoted paths.
  ;; Keep it disabled and prevent accidental toggles from starting that process.
  (put 'treemacs-hide-gitignored-files-mode :prefetch-done t)
  (treemacs-hide-gitignored-files-mode -1)
  (treemacs-project-follow-mode t)
  (+treemacs-cursor-follow-mode t))

(defun +treemacs-apply-elegant-ui ()
  "Apply Elegant spacing without replacing Treemacs theme colors."
  (when (derived-mode-p 'treemacs-mode)
    (setq-local line-spacing 0.08)))

(add-hook 'after-load-theme-hook #'+treemacs-apply-elegant-ui)

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
  (("C-x k" . persp-kill-buffer*)
   ("M-W" . persp-kill-buffer*))
  :init
  (setq persp-mode-prefix-key (kbd "C-x x")
        persp-initial-frame-name "main")
  :config
  (add-to-list 'window-persistent-parameters '(winner-ring . t))
  (persp-mode 1)
  (add-hook 'persp-switch-hook #'+project-perspective-switched-h)
  (add-hook 'persp-killed-hook #'+project-perspective-killed-h)
  (with-eval-after-load 'treemacs
    (when (assoc 'Perspectives treemacs-scope-types)
      (treemacs-set-scope-type 'Perspectives))))

(with-eval-after-load 'project
  (add-to-list 'project-list-exclude #'+project-project-object-ignored-p))

(with-eval-after-load 'project
  ;; `project-list-exclude' handles hidden roots.  Leave Emacs's own project
  ;; remembering intact even when a project is not managed by Projectile.
  (advice-remove 'project--remember-dir #'+project--remember-dir-visible-only)
  (+project/prune-hidden-project-state))

(with-eval-after-load 'dashboard
  (setq dashboard-projects-backend 'projectile
        dashboard-projects-switch-function #'+project/open-workbench)

  (unless (advice-member-p #'+dashboard-projects-load-projects
                           #'dashboard-projects-backend-load-projects)
    (advice-add 'dashboard-projects-backend-load-projects
                :around
                #'+dashboard-projects-load-projects)))




(with-eval-after-load 'treemacs
  (advice-add 'treemacs--follow :around
              (lambda (fn &rest args)
                (let ((inhibit-message t))
                  (apply fn args)))))

(with-eval-after-load 'treemacs-workspaces
  (advice-remove 'treemacs--find-current-user-project
                 #'+treemacs-find-current-user-project-a)
  (advice-add 'treemacs--find-current-user-project :around
              #'+treemacs-find-current-user-project-a))

(with-eval-after-load 'treemacs-persistence
  (unless (advice-member-p
           #'+treemacs-normalize-persist-lines-a
           'treemacs--read-persist-lines)
    (advice-add
     'treemacs--read-persist-lines
     :filter-return #'+treemacs-normalize-persist-lines-a)))

(with-eval-after-load 'treemacs-interface
  (dolist (command
           '(treemacs-visit-node-vertical-split
             treemacs-visit-node-horizontal-split
             treemacs-visit-node-no-split
             treemacs-visit-node-ace
             treemacs-visit-node-in-most-recently-used-window
             treemacs-visit-node-ace-horizontal-split
             treemacs-visit-node-ace-vertical-split))
    (unless (advice-member-p #'+treemacs-visit-logical-path-a command)
      (advice-add command :around #'+treemacs-visit-logical-path-a))))

(with-eval-after-load 'breadcrumb
  (dolist (renderer '(breadcrumb--header-line bc--header-line))
    (when (and
           (fboundp renderer)
           (not
            (advice-member-p
             #'+breadcrumb-header-line-safely-a renderer)))
      (advice-add renderer :around #'+breadcrumb-header-line-safely-a))))




(provide 'tools/project/config)

;;; tools/project/config.el ends here
