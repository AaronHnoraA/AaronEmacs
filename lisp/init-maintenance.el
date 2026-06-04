;;; init-maintenance.el --- Migration and state maintenance helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-package-utils)
(require 'subr-x)

(defgroup my/maintenance nil
  "Migration and state maintenance helpers."
  :group 'convenience)

(defcustom my/maintenance-state-backup-dir
  (expand-file-name "var/backup-snapshots" user-emacs-directory)
  "Directory used to store local state snapshot archives."
  :type 'directory
  :group 'my/maintenance)

(defcustom my/maintenance-state-paths
  '("package-lock.el"
    "etc"
    "var/amx-save.el"
    "var/dape"
    "var/org"
    "var/prescient-save.el"
    "var/project"
    "var/project-list.el"
    "var/projectile"
    "var/recentf-save.el"
    "var/save-place.el"
    "var/savehist.el"
    "var/session"
    "var/transient"
    "var/tramp/persistency.el"
    "var/treemacs")
  "Relative config paths included in state snapshots."
  :type '(repeat string)
  :group 'my/maintenance)

(defun my/maintenance-config-root ()
  "Return the root of the current Emacs config."
  (file-name-as-directory (expand-file-name user-emacs-directory)))

(defun my/maintenance--existing-state-paths ()
  "Return snapshot paths that currently exist."
  (seq-filter
   (lambda (path)
     (file-exists-p (expand-file-name path (my/maintenance-config-root))))
   my/maintenance-state-paths))

(defun my/maintenance--snapshot-file-name ()
  "Return a timestamped archive path for a state snapshot."
  (expand-file-name
   (format "emacs-state-%s.tar.gz"
           (format-time-string "%Y%m%d-%H%M%S"))
   my/maintenance-state-backup-dir))

(defun my/maintenance-state-report ()
  "Return a batch-friendly report for state snapshot coverage."
  (interactive)
  (let* ((existing (my/maintenance--existing-state-paths))
         (missing (seq-remove (lambda (path) (member path existing))
                              my/maintenance-state-paths))
         (report
          (list :backup-dir my/maintenance-state-backup-dir
                :paths existing
                :missing-paths missing
                :tar (executable-find "tar"))))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun my/maintenance-state-snapshot (&optional archive)
  "Create a compressed state snapshot archive at ARCHIVE.
When ARCHIVE is nil, store it under `my/maintenance-state-backup-dir'."
  (interactive)
  (unless (executable-find "tar")
    (user-error "The `tar' executable is required for state snapshots"))
  (let* ((root (my/maintenance-config-root))
         (paths (my/maintenance--existing-state-paths))
         (target (expand-file-name (or archive
                                       (my/maintenance--snapshot-file-name))
                                   root))
         (default-directory root))
    (unless paths
      (user-error "No configured state paths exist yet"))
    (make-directory (file-name-directory target) t)
    (with-temp-buffer
      (let ((status (apply #'call-process
                           "tar"
                           nil
                           (current-buffer)
                           nil
                           "-czf" target
                           paths)))
        (unless (eq status 0)
          (error "State snapshot failed: %s"
                 (string-trim (buffer-string))))))
    (when (called-interactively-p 'interactive)
      (message "State snapshot written to %s" target))
    target))

(defun my/maintenance-state-restore (archive)
  "Restore a state snapshot ARCHIVE into the current config root."
  (interactive "fState snapshot archive: ")
  (unless (executable-find "tar")
    (user-error "The `tar' executable is required for state restore"))
  (let ((root (my/maintenance-config-root))
        (archive (expand-file-name archive)))
    (unless (file-exists-p archive)
      (user-error "Snapshot archive does not exist: %s" archive))
    (with-temp-buffer
      (let ((status (call-process
                     "tar"
                     nil
                     (current-buffer)
                     nil
                     "-xzf" archive
                     "-C" root)))
        (unless (eq status 0)
          (error "State restore failed: %s"
                 (string-trim (buffer-string))))))
    (when (called-interactively-p 'interactive)
      (message "State snapshot restored from %s" archive))
    archive))

(my/leader!
  "r B" '(:def my/maintenance-state-snapshot :which-key "backup state")
  "r R" '(:def my/maintenance-state-restore :which-key "restore state"))

(provide 'init-maintenance)
;;; init-maintenance.el ends here
