;;; core/aaron-packages.el --- Package helper utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'subr-x)

(defvar package-selected-packages)
(defvar package-vc-selected-packages)
(defvar aaron-package-lock-version)
(defvar aaron-package-vc-patches nil
  "Alist of VC package symbols and patch files relative to `user-emacs-directory'.")

(let ((manifest (expand-file-name "config/package-patches.el" user-emacs-directory)))
  (when (file-exists-p manifest)
    (load manifest nil 'nomessage)))

(defgroup aaron-package-utils nil
  "Helpers for installing and loading packages."
  :group 'convenience)

(defvar aaron-package-vc-recipes nil
  "Registered VC package recipes declared by this config.
Each element is a cons cell of the form (PACKAGE . SPEC).")

(defconst aaron-package-lock-file
  (expand-file-name "package-lock.el" user-emacs-directory)
  "Path to the generated package lock file.")

(defconst aaron-package-lock-supported-version 2
  "Expected schema version for `aaron-package-lock-file'.")

(defconst aaron-package-core-packages
  '(use-package)
  "Packages managed specially during bootstrap and omitted from lock export.")

(defconst aaron-package-bootstrap-min-installed-count 3
  "Installed package count threshold that marks an environment as already set up.")

(defvar aaron-package--archives-refreshed nil
  "Whether package archives have already been refreshed in this Emacs session.")

(defun aaron-package-ensure-installed-list (packages)
  "Ensure every package in PACKAGES is installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun aaron-package-refresh-archives-once ()
  "Refresh package archives once per Emacs session when needed."
  (unless (or aaron-package--archives-refreshed
              package-archive-contents)
    (message "Refreshing package archives...")
    (package-refresh-contents)
    (setq aaron-package--archives-refreshed t)))

(defun aaron-package-install-archive (pkg)
  "Install archive package PKG if it is missing."
  (unless (package-installed-p pkg)
    (aaron-package-refresh-archives-once)
    (message "Installing archive package %s..." pkg)
    (package-install pkg)))

(defun aaron-package-register-vc (package spec)
  "Register PACKAGE with VC SPEC for bootstrap export."
  (setq aaron-package-vc-recipes (assq-delete-all package aaron-package-vc-recipes))
  (push (cons package (copy-tree spec)) aaron-package-vc-recipes)
  (when (boundp 'package-vc-selected-packages)
    (setq package-vc-selected-packages
          (assq-delete-all package package-vc-selected-packages))
    (push (cons package (copy-tree spec)) package-vc-selected-packages))
  (cons package spec))

(defun aaron-package-vc-recipes-sorted ()
  "Return a stable copy of `aaron-package-vc-recipes'."
  (sort (copy-tree aaron-package-vc-recipes)
        (lambda (a b)
          (string< (symbol-name (car a))
                   (symbol-name (car b))))))

(defun aaron-package-ensure-vc (package url &optional rev)
  "Install PACKAGE from URL via package-vc when it is missing.
REV defaults to `:last-release'."
  (let ((spec `(:url ,url :rev ,(or rev :last-release))))
    (aaron-package-register-vc package spec)
    (unless (or (package-installed-p package)
                ;; VC packages may already exist as working trees under elpa/
                ;; but still confuse package-vc into prompting for overwrite.
                ;; If the library is already loadable, treat it as installed.
                (locate-library (symbol-name package)))
      (require 'package-vc)
      (package-vc-install (cons package spec) (plist-get spec :rev)))
    (aaron-package-apply-vc-patch package)))

(defun aaron-package-vc-source-directory (package)
  "Return PACKAGE's package-vc checkout directory, or signal a useful error."
  (let* ((desc (aaron-package--package-desc package))
         (dir (and desc (eq (package-desc-kind desc) 'vc)
                   (or (alist-get :vc-dir (package-desc-extras desc))
                       (package-desc-dir desc)))))
    (unless (and (stringp dir) (file-directory-p dir))
      (error "VC checkout for %s is missing; install the package first" package))
    dir))

(defun aaron-package--vc-patch-file (package)
  "Return PACKAGE's registered patch as an absolute path, or nil."
  (when-let* ((name (alist-get package aaron-package-vc-patches)))
    (let ((file (expand-file-name name user-emacs-directory)))
      (unless (file-readable-p file)
        (error "Patch for %s is missing: %s" package file))
      file)))

(defun aaron-package--git-result (directory &rest args)
  "Run Git ARGS in DIRECTORY and return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil "-C" directory args)))
      (cons status (string-trim (buffer-string))))))

(defun aaron-package-apply-vc-patch (package)
  "Apply PACKAGE's registered patch once; fail clearly on upstream conflicts.
Return `applied', `already', or nil when PACKAGE has no patch."
  (when-let* ((patch (aaron-package--vc-patch-file package)))
    (let* ((dir (aaron-package-vc-source-directory package))
           (reverse (aaron-package--git-result dir "apply" "--reverse" "--check" patch)))
      (if (zerop (car reverse))
          'already
        (let ((check (aaron-package--git-result dir "apply" "--check" patch)))
          (unless (zerop (car check))
            (error "Patch for %s conflicts with %s: %s"
                   package dir (cdr check)))
          (let ((result (aaron-package--git-result dir "apply" patch)))
            (unless (zerop (car result))
              (error "Applying patch for %s failed: %s" package (cdr result)))
            'applied))))))

(defun aaron-package-unapply-vc-patch (package)
  "Remove PACKAGE's registered patch before package-vc updates its checkout.
Return `removed', nil if it was already absent, or signal on a conflict."
  (when-let* ((patch (aaron-package--vc-patch-file package)))
    (let* ((dir (aaron-package-vc-source-directory package))
           (reverse (aaron-package--git-result dir "apply" "--reverse" "--check" patch)))
      (if (zerop (car reverse))
          (let ((result (aaron-package--git-result dir "apply" "--reverse" patch)))
            (unless (zerop (car result))
              (error "Removing patch for %s failed: %s" package (cdr result)))
            'removed)
        (let ((check (aaron-package--git-result dir "apply" "--check" patch)))
          (unless (zerop (car check))
            (error "Patch state for %s is neither applied nor clean: %s"
                   package (cdr check)))
          nil)))))

(defun aaron-package-with-vc-patches-suspended (function)
  "Run FUNCTION with registered VC patches removed, then restore them.
This gives package-vc a clean checkout for upgrades.  Other local source
changes stop the upgrade instead of being overwritten or silently retained."
  (unwind-protect
      (progn
        (dolist (entry aaron-package-vc-patches)
          (let ((package (car entry)))
            (when (package-installed-p package)
              (aaron-package-unapply-vc-patch package)
              (let* ((dir (aaron-package-vc-source-directory package))
                     (status (aaron-package--git-result dir "diff" "--quiet" "HEAD" "--")))
                (unless (zerop (car status))
                  (error "VC checkout for %s has other local changes: %s"
                         package dir))))))
        (funcall function))
    (dolist (entry aaron-package-vc-patches)
      (when (package-installed-p (car entry))
        (aaron-package-apply-vc-patch (car entry))))))

(defun aaron-package-read-lock-file ()
  "Load `aaron-package-lock-file' if it exists."
  (when (file-exists-p aaron-package-lock-file)
    (load-file aaron-package-lock-file)
    t))

(defun aaron-package-read-lock-state ()
  "Return package metadata recorded in `aaron-package-lock-file'.
The result is a plist with `:version', `:packages', and `:vc-packages'."
  (when (file-exists-p aaron-package-lock-file)
    (let (package-selected-packages
          package-vc-selected-packages
          aaron-package-lock-version)
      (load-file aaron-package-lock-file)
      (list :version aaron-package-lock-version
            :packages (copy-tree package-selected-packages)
            :vc-packages (copy-tree package-vc-selected-packages)))))

(defun aaron-package--package-desc (pkg)
  "Return the first package descriptor for PKG from `package-alist'."
  (cadr (assq pkg package-alist)))

(defun aaron-package--vc-package-p (pkg)
  "Return non-nil when PKG is installed as a VC package."
  (let ((desc (aaron-package--package-desc pkg)))
    (eq (and desc (package-desc-kind desc)) 'vc)))

(defun aaron-package--installed-third-party-packages ()
  "Return installed third-party packages in stable order."
  (let ((pkgs nil))
    (dolist (entry package-alist)
      (unless (memq (car entry) aaron-package-core-packages)
        (push (car entry) pkgs)))
    (sort pkgs
          (lambda (a b)
            (string< (symbol-name a)
                     (symbol-name b))))))

(defun aaron-package--current-vc-package-list ()
  "Return VC package specs collected from init declarations and package state."
  (let ((merged nil))
    (dolist (entry (append (and (boundp 'aaron-package-vc-recipes)
                                (copy-tree aaron-package-vc-recipes))
                           (and (boundp 'package-vc-selected-packages)
                                (copy-tree package-vc-selected-packages))))
      (pcase-let ((`(,name . ,spec) entry))
        (setq merged (assq-delete-all name merged))
        (push (cons name spec) merged)))
    (sort merged
          (lambda (a b)
            (string< (symbol-name (car a))
                     (symbol-name (car b)))))))

(defun aaron-package--current-package-list ()
  "Return installed non-VC packages in stable order."
  (let ((vc-names (mapcar #'car (aaron-package--current-vc-package-list))))
    (cl-remove-if (lambda (pkg)
                    (or (memq pkg vc-names)
                        (aaron-package--vc-package-p pkg)))
                  (aaron-package--installed-third-party-packages))))

(defun aaron-package--normalize-symbol-list (items)
  "Return ITEMS as a stable symbol list without duplicates."
  (sort (delete-dups (copy-sequence (or items nil)))
        (lambda (a b)
          (string< (symbol-name a)
                   (symbol-name b)))))

(defun aaron-package--normalize-vc-package-list (items)
  "Return ITEMS as a stable VC package alist without duplicate package names."
  (let ((merged nil))
    (dolist (entry (or items nil))
      (pcase-let ((`(,name . ,spec) entry))
        (setq merged (assq-delete-all name merged))
        (push (cons name spec) merged)))
    (sort merged
          (lambda (a b)
            (string< (symbol-name (car a))
                     (symbol-name (car b)))))))

(defun aaron-package--missing-symbols (left right)
  "Return symbols present in LEFT but not in RIGHT."
  (cl-set-difference (aaron-package--normalize-symbol-list left)
                     (aaron-package--normalize-symbol-list right)
                     :test #'eq))

(defun aaron-package--vc-entry-equal-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same VC package entry."
  (and (eq (car left) (car right))
       (equal (cdr left) (cdr right))))

(defun aaron-package--missing-vc-entries (left right)
  "Return VC entries present in LEFT but not in RIGHT."
  (cl-remove-if
   (lambda (entry)
     (seq-some (lambda (other)
                 (aaron-package--vc-entry-equal-p entry other))
               right))
   (aaron-package--normalize-vc-package-list left)))

(defun aaron-package-export-lock-file (&optional file)
  "Write installed archive and VC packages to FILE.
FILE defaults to `aaron-package-lock-file'."
  (let ((target (or file aaron-package-lock-file))
        (pkgs (aaron-package--current-package-list))
        (vc-pkgs (aaron-package--current-vc-package-list)))
    (with-temp-file target
      (insert ";;; package-lock.el --- Generated by bootstrap.el -*- lexical-binding: t; -*-\n\n")
      (insert ";; 自动生成的包锁文件：普通包走 package-install，VC 包走 package-vc-install。\n\n")
      (insert "(setq aaron-package-lock-version 2)\n\n")
      (insert "(setq package-selected-packages\n      '")
      (prin1 pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(setq package-vc-selected-packages\n      '")
      (prin1 vc-pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(provide 'package-lock)\n"))
    target))

(defun aaron/package-lock-audit ()
  "Compare current package state against `aaron-package-lock-file'.
Return a plist suitable for batch checks."
  (interactive)
  (let* ((lock-state (aaron-package-read-lock-state))
         (lock-version (plist-get lock-state :version))
         (locked-packages (plist-get lock-state :packages))
         (locked-vc-packages (plist-get lock-state :vc-packages))
         (current-packages (aaron-package--current-package-list))
         (current-vc-packages (aaron-package--current-vc-package-list))
         (archive-missing-in-lock
          (aaron-package--missing-symbols current-packages locked-packages))
         (archive-extra-in-lock
          (aaron-package--missing-symbols locked-packages current-packages))
         (vc-missing-in-lock
          (aaron-package--missing-vc-entries current-vc-packages locked-vc-packages))
         (vc-extra-in-lock
          (aaron-package--missing-vc-entries locked-vc-packages current-vc-packages))
         (ok (and lock-state
                  (eq lock-version aaron-package-lock-supported-version)
                  (null archive-missing-in-lock)
                  (null archive-extra-in-lock)
                  (null vc-missing-in-lock)
                  (null vc-extra-in-lock)))
         (report
          (list :ok ok
                :lock-file aaron-package-lock-file
                :lock-version lock-version
                :expected-version aaron-package-lock-supported-version
                :archive-missing-in-lock archive-missing-in-lock
                :archive-extra-in-lock archive-extra-in-lock
                :vc-missing-in-lock vc-missing-in-lock
                :vc-extra-in-lock vc-extra-in-lock)))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun aaron-package-install-from-lock ()
  "Install archive and VC packages recorded in `aaron-package-lock-file'."
  (when (aaron-package-read-lock-file)
    (when (boundp 'package-selected-packages)
      (dolist (pkg package-selected-packages)
        (condition-case err
            (aaron-package-install-archive pkg)
          (error
           (display-warning
            'init-package-utils
            (format "Failed to install archive package %s: %s"
                    pkg (error-message-string err))
            :error)))))
    (when (boundp 'package-vc-selected-packages)
      (require 'package-vc)
      (dolist (entry package-vc-selected-packages)
        (pcase-let ((`(,pkg . ,spec) entry))
          (unless (package-installed-p pkg)
            (condition-case err
                (progn
                  (aaron-package-refresh-archives-once)
                  (message "Installing VC package %s..." pkg)
                  (cond
                   ((null spec)
                    (package-vc-install pkg))
                   ((stringp spec)
                    (package-vc-install pkg spec))
                   ((listp spec)
                    (package-vc-install (cons pkg spec)))
                   (t
                    (error "Unsupported VC package spec: %S" spec))))
              (error
               (display-warning
                'init-package-utils
                (format "Failed to install VC package %s: %s"
                        pkg (error-message-string err))
                :error)))))))
    (dolist (entry aaron-package-vc-patches)
      (when (package-installed-p (car entry))
        (aaron-package-apply-vc-patch (car entry))))
    t))

(defun aaron-package--upgrade-archive-packages (query)
  "Upgrade archive packages.
When QUERY is non-nil, allow the underlying package upgrader to prompt."
  (aaron-package-refresh-archives-once)
  (cond
   ((fboundp 'package-upgrade-all)
    (package-upgrade-all query)
    t)
   ((and (fboundp 'package-list-packages)
         (fboundp 'package-menu-mark-upgrades)
         (fboundp 'package-menu-execute))
    (package-list-packages t)
    (with-current-buffer "*Packages*"
      (package-menu-mark-upgrades)
      (package-menu-execute t))
    t)
   (t nil)))

(defun aaron-package--upgrade-vc-packages ()
  "Upgrade installed VC packages."
  (require 'package-vc)
  (let ((upgrade (cond
                  ((fboundp 'package-vc-upgrade-all)
                   #'package-vc-upgrade-all)
                  ((and (fboundp 'package-vc-upgrade)
                        (boundp 'package-vc-selected-packages))
                   (lambda ()
                     (dolist (entry package-vc-selected-packages)
                       (package-vc-upgrade (car entry))))))))
    (when upgrade
      (aaron-package-with-vc-patches-suspended upgrade)
      t)))

(defun aaron-package-bootstrap-from-lock-if-needed ()
  "Install packages from `aaron-package-lock-file' on first startup.
This makes a freshly cloned config survivable even when the user launches
Emacs directly instead of running `bootstrap.el' first."
  (when (< (length (aaron-package--installed-third-party-packages))
           aaron-package-bootstrap-min-installed-count)
    (when (file-exists-p aaron-package-lock-file)
      (message "Detected fresh package state; restoring dependencies from %s..."
               aaron-package-lock-file)
      (aaron-package-install-from-lock))))

(defun aaron/package-upgrade-all (&optional query)
  "Upgrade installed archive and VC packages, then refresh the lock file.
If QUERY is non-nil, archive package upgrades may prompt for confirmation."
  (interactive (list t))
  (unless (booleanp query)
    (setq query nil))
  (let ((archive-upgrade-available nil)
        (vc-upgrade-available nil)
        (upgraded-any nil))
    (condition-case err
        (progn
          (setq archive-upgrade-available
                (aaron-package--upgrade-archive-packages query))
          (setq vc-upgrade-available
                (aaron-package--upgrade-vc-packages))
          (setq upgraded-any
                (or archive-upgrade-available
                    vc-upgrade-available)))
      (error
       (user-error "Package upgrade failed: %s" (error-message-string err))))
    (unless upgraded-any
      (user-error "Package upgrade command is unavailable in this Emacs"))
    (let ((lock-file (aaron-package-export-lock-file)))
      (message "Package upgrade finished; lock file refreshed at %s" lock-file))))

(defun aaron/package-upgrade-all-noninteractive ()
  "Upgrade installed packages without minibuffer prompts.
Intended for dashboard and button-based entry points."
  (interactive)
  (aaron/package-upgrade-all nil))

;; Compatibility shims for newer ELPA Magit running against Emacs 31's
;; built-in `transient'.
(defvar transient-prefer-reading-value nil
  "Whether Magit should prefer reading infix values in the minibuffer.

This variable was provided by older external `transient' releases and is
still referenced by current Magit code.")

(with-eval-after-load 'transient
  ;; Emacs 31's built-in transient dropped this private helper, but the
  ;; current ELPA Magit still calls it when formatting some branch infixes.
  (unless (fboundp 'transient--get-format)
    (defun transient--get-format (obj)
      "Return OBJ's display format for Magit compatibility."
      (slot-value obj 'format))))

(provide 'aaron-packages)
;;; core/aaron-packages.el ends here
