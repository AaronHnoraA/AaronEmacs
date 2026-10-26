;;; config/maintenance/config.el --- Migration and state maintenance helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'aaron-funcs)
(require 'aaron-packages)
(require 'seq)
(require 'subr-x)

(defgroup +maintenance nil
  "Migration and state maintenance helpers."
  :group 'convenience)

(config-defvar +maintenance-state-backup-dir nil
  "Directory used to store local state snapshot archives."
  :type 'directory
  :group '+maintenance)

(config-defvar +maintenance-state-paths nil
  "Relative config paths included in state snapshots."
  :type '(repeat string)
  :group '+maintenance)

(config-defvar +maintenance-var-cleanup-enable t
  "Whether to run conservative automatic cleanup for `var/'."
  :type 'boolean
  :group '+maintenance)

(config-defvar +maintenance-var-cleanup-idle-delay 45
  "Idle seconds after startup before the first automatic `var/' cleanup check."
  :type 'number
  :group '+maintenance)

(config-defvar +maintenance-var-cleanup-interval (* 24 60 60)
  "Minimum seconds between automatic `var/' cleanup runs."
  :type 'number
  :group '+maintenance)

(config-defvar +maintenance-var-cleanup-max-bytes (* 2 1024 1024 1024)
  "Soft size cap for `var/'.  Nil disables capacity-triggered cleanup."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group '+maintenance)

(config-defvar +maintenance-var-cleanup-targets
  '((:path "auctex/output" :age-days 7)
    (:path "Noema/tmp" :age-days 2)
    (:path "log" :age-days 30)
    (:path "auto-save/sessions" :age-days 14)
    (:path "backup" :age-days 60)
    (:path "lsp" :age-days 90)
    (:path "lsp-java/workspace" :age-days 90)
    (:path "eln-cache" :age-days nil))
  "Cleanup targets under `var/'.
Each entry is a plist with :path and optional :age-days.  Files older than
:age-days are removed during regular cleanup; all listed targets may also be
used as cold-data candidates when `+maintenance-var-cleanup-max-bytes' is
exceeded."
  :type '(repeat sexp)
  :group '+maintenance)

(defvar +maintenance-var-cleanup--timer nil)

(defvar +maintenance--local-config-root
  (let ((default-directory "/")
        ;; This value belongs to the editor process, not to the selected file
        ;; target.  Capture it before direnv can install a target HOME in a
        ;; buffer and without asking a logical file-name handler to expand it.
        (file-name-handler-alist nil))
    (file-name-as-directory
     (expand-file-name user-emacs-directory)))
  "Native client directory which owns Emacs maintenance state.")

(defvar +maintenance--local-process-environment
  (copy-sequence process-environment)
  "Client process environment captured before buffer-local target capsules.")

(defvar +maintenance--local-exec-path
  (copy-sequence exec-path)
  "Client executable search path used by editor-owned maintenance.")

(defun +maintenance-config-root ()
  "Return the local root of the current Emacs config.
Idle timers inherit the selected buffer's `default-directory'.  Resolve this
editor-owned path from a known local directory so a remote buffer cannot turn
`~/.config/emacs' into a target-side path."
  +maintenance--local-config-root)

(defun +maintenance-var-root ()
  "Return the root of managed Emacs runtime state."
  (file-name-as-directory
   (expand-file-name "var" (+maintenance-config-root))))

(defun +maintenance-var-cleanup--state-file ()
  "Return the persistent state file for automatic `var/' cleanup."
  (expand-file-name "maintenance/var-cleanup-state.el"
                    (+maintenance-var-root)))

(defun +maintenance-var-cleanup--read-state ()
  "Read persisted automatic cleanup state."
  (let ((file (+maintenance-var-cleanup--state-file)))
    (when (file-exists-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))))))

(defun +maintenance-var-cleanup--write-state (state)
  "Persist automatic cleanup STATE."
  (let ((file (+maintenance-var-cleanup--state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (prin1 state (current-buffer))
        (insert "\n")))))

(defun +maintenance-var-cleanup--walk-files (dir)
  "Return regular files recursively under DIR without following symlinks."
  (let (files)
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (cond
         ((file-symlink-p entry) nil)
         ((file-directory-p entry)
          (setq files (nconc files
                             (+maintenance-var-cleanup--walk-files entry))))
         ((file-regular-p entry)
          (push entry files)))))
    files))

(defun +maintenance-var-cleanup--file-entry (file target-root age-days)
  "Return cleanup metadata for FILE below TARGET-ROOT."
  (let* ((attrs (file-attributes file 'integer))
         (mtime (file-attribute-modification-time attrs))
         (size (or (file-attribute-size attrs) 0))
         (age-seconds (float-time (time-subtract (current-time) mtime))))
    (list :file file
          :target target-root
          :size size
          :mtime mtime
          :age-days (/ age-seconds 86400.0)
          :max-age-days age-days)))

(defun +maintenance-var-cleanup--target-files ()
  "Return cleanup candidates from configured target directories."
  (let ((root (+maintenance-var-root))
        entries)
    (dolist (target +maintenance-var-cleanup-targets)
      (let* ((rel (plist-get target :path))
             (age-days (plist-get target :age-days))
             (dir (and rel (expand-file-name rel root))))
        (when (and dir
                   (file-directory-p dir)
                   (file-in-directory-p dir root))
          (dolist (file (+maintenance-var-cleanup--walk-files dir))
            (push (+maintenance-var-cleanup--file-entry file dir age-days)
                  entries)))))
    entries))

(defun +maintenance-var-cleanup--directory-bytes (dir)
  "Return total regular-file bytes below DIR."
  (cl-loop for file in (+maintenance-var-cleanup--walk-files dir)
           for attrs = (ignore-errors (file-attributes file 'integer))
           sum (or (and attrs (file-attribute-size attrs)) 0)))

(defun +maintenance-var-cleanup--delete-file (entry dry-run)
  "Delete cleanup ENTRY unless DRY-RUN is non-nil.
Return the entry size when deletion is successful or simulated."
  (let ((file (plist-get entry :file))
        (size (plist-get entry :size)))
    (if dry-run
        size
      (condition-case nil
          (progn
            (delete-file file)
            size)
        (error 0)))))

(defun +maintenance-var-cleanup--empty-dirs (dir dry-run)
  "Remove empty directories below DIR unless DRY-RUN is non-nil."
  (when (file-directory-p dir)
    (let (dirs)
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (when (and (not (file-symlink-p entry))
                   (file-directory-p entry))
          (setq dirs (nconc dirs
                            (+maintenance-var-cleanup--empty-dirs entry
                                                                     dry-run)))
          (push entry dirs)))
      (dolist (candidate (sort (copy-sequence dirs)
                               (lambda (a b) (> (length a) (length b)))))
        (when (and (file-directory-p candidate)
                   (null (directory-files candidate nil
                                          directory-files-no-dot-files-regexp)))
          (unless dry-run
            (ignore-errors (delete-directory candidate)))))
      dirs)))

(defun +maintenance/var-cleanup (&optional dry-run)
  "Clean cold cache files under `var/'.
With prefix argument DRY-RUN, report what would be deleted without deleting."
  (interactive "P")
  (let* ((root (+maintenance-var-root))
         (before-bytes (+maintenance-var-cleanup--directory-bytes root))
         (entries (+maintenance-var-cleanup--target-files))
         (age-candidates
          (seq-filter
           (lambda (entry)
             (let ((max-age (plist-get entry :max-age-days)))
               (and max-age
                    (> (plist-get entry :age-days) max-age))))
           entries))
         (deleted-files 0)
         (deleted-bytes 0)
         (deleted-set (make-hash-table :test #'equal)))
    (dolist (entry age-candidates)
      (let ((bytes (+maintenance-var-cleanup--delete-file entry dry-run)))
        (when (> bytes 0)
          (puthash (plist-get entry :file) t deleted-set)
          (cl-incf deleted-files)
          (cl-incf deleted-bytes bytes))))
    (when (and +maintenance-var-cleanup-max-bytes
               (> (- before-bytes deleted-bytes)
                  +maintenance-var-cleanup-max-bytes))
      (let ((remaining
             (sort
              (seq-remove
               (lambda (entry) (gethash (plist-get entry :file) deleted-set))
               entries)
              (lambda (a b)
                (time-less-p (plist-get a :mtime)
                             (plist-get b :mtime)))))
            (current (- before-bytes deleted-bytes)))
        (while (and remaining
                    (> current +maintenance-var-cleanup-max-bytes))
          (let* ((entry (pop remaining))
                 (bytes (+maintenance-var-cleanup--delete-file entry dry-run)))
            (when (> bytes 0)
              (puthash (plist-get entry :file) t deleted-set)
              (cl-incf deleted-files)
              (cl-incf deleted-bytes bytes)
              (cl-decf current bytes))))))
    (dolist (target +maintenance-var-cleanup-targets)
      (let ((dir (expand-file-name (plist-get target :path) root)))
        (when (and (file-directory-p dir)
                   (file-in-directory-p dir root))
          (+maintenance-var-cleanup--empty-dirs dir dry-run))))
    (let ((report (list :dry-run (not (null dry-run))
                        :root root
                        :before-bytes before-bytes
                        :deleted-files deleted-files
                        :deleted-bytes deleted-bytes
                        :after-bytes (max 0 (- before-bytes deleted-bytes))
                        :capacity +maintenance-var-cleanup-max-bytes)))
      (unless dry-run
        (+maintenance-var-cleanup--write-state
         (list :last-run (float-time)
               :last-report report)))
      (when (called-interactively-p 'interactive)
        (message "var cleanup: %s" report))
      report)))

(defun +maintenance-var-cleanup-maybe (&optional force)
  "Run automatic `var/' cleanup if enough time has elapsed or FORCE is non-nil."
  (let ((default-directory (+maintenance-config-root))
        ;; A remote direnv capsule is buffer-local and may contain target
        ;; HOME/TMPDIR values.  Editor-owned maintenance must never inherit
        ;; those values merely because an idle timer fired in that buffer.
        (process-environment
         (copy-sequence +maintenance--local-process-environment))
        (exec-path (copy-sequence +maintenance--local-exec-path)))
    (when +maintenance-var-cleanup-enable
      (let* ((state (+maintenance-var-cleanup--read-state))
             (last-run (plist-get state :last-run))
             (elapsed (and last-run (- (float-time) last-run))))
        (when (or force
                  (not elapsed)
                  (> elapsed +maintenance-var-cleanup-interval))
          (+maintenance/var-cleanup))))))

(defun +maintenance-var-cleanup-start-timer ()
  "Start the idle timer for automatic `var/' cleanup."
  (when +maintenance-var-cleanup--timer
    (cancel-timer +maintenance-var-cleanup--timer))
  (when +maintenance-var-cleanup-enable
    (setq +maintenance-var-cleanup--timer
          (run-with-idle-timer
           +maintenance-var-cleanup-idle-delay
           t
           #'+maintenance-var-cleanup-maybe))))

(defun +maintenance--existing-state-paths ()
  "Return snapshot paths that currently exist."
  (seq-filter
   (lambda (path)
     (file-exists-p (expand-file-name path (+maintenance-config-root))))
   +maintenance-state-paths))

(defun +maintenance--snapshot-file-name ()
  "Return a timestamped archive path for a state snapshot."
  (expand-file-name
   (format "emacs-state-%s.tar.gz"
           (format-time-string "%Y%m%d-%H%M%S"))
   +maintenance-state-backup-dir))

(defun +maintenance/state-report ()
  "Return a batch-friendly report for state snapshot coverage."
  (interactive)
  (let* ((existing (+maintenance--existing-state-paths))
         (missing (seq-remove (lambda (path) (member path existing))
                              +maintenance-state-paths))
         (report
          (list :backup-dir +maintenance-state-backup-dir
                :paths existing
                :missing-paths missing
                :tar (executable-find "tar"))))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun +maintenance/state-snapshot (&optional archive)
  "Create a compressed state snapshot archive at ARCHIVE.
When ARCHIVE is nil, store it under `+maintenance-state-backup-dir'."
  (interactive)
  (unless (executable-find "tar")
    (user-error "The `tar' executable is required for state snapshots"))
  (let* ((root (+maintenance-config-root))
         (paths (+maintenance--existing-state-paths))
         (target (expand-file-name (or archive
                                       (+maintenance--snapshot-file-name))
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

(defun +maintenance/state-restore (archive)
  "Restore a state snapshot ARCHIVE into the current config root."
  (interactive "fState snapshot archive: ")
  (unless (executable-find "tar")
    (user-error "The `tar' executable is required for state restore"))
  (let ((root (+maintenance-config-root))
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

(aaron-leader!
  "r B" '(:def +maintenance/state-snapshot :which-key "backup state")
  "r V" '(:def +maintenance/state-restore :which-key "restore state")
  "r C" '(:def +maintenance/var-cleanup :which-key "clean var"))

(add-hook 'emacs-startup-hook #'+maintenance-var-cleanup-start-timer)

(provide 'config/maintenance/config)
;;; config/maintenance/config.el ends here
