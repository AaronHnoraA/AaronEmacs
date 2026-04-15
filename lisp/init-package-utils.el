;;; init-package-utils.el --- Package helper utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'package)

(defvar package-selected-packages)
(defvar package-vc-selected-packages)
(defvar my/package-lock-version)

(defgroup my/package-utils nil
  "Helpers for installing and loading packages."
  :group 'convenience)

(defvar my/package-vc-recipes nil
  "Registered VC package recipes declared by this config.
Each element is a cons cell of the form (PACKAGE . SPEC).")

(defconst my/package-lock-file
  (expand-file-name "package-lock.el" user-emacs-directory)
  "Path to the generated package lock file.")

(defconst my/package-lock-supported-version 2
  "Expected schema version for `my/package-lock-file'.")

(defconst my/package-core-packages
  '(use-package)
  "Packages managed specially during bootstrap and omitted from lock export.")

(defconst my/package-bootstrap-min-installed-count 3
  "Installed package count threshold that marks an environment as already set up.")

(defvar my/package--archives-refreshed nil
  "Whether package archives have already been refreshed in this Emacs session.")

(defun my/package-ensure-installed-list (packages)
  "Ensure every package in PACKAGES is installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun my/package-refresh-archives-once ()
  "Refresh package archives once per Emacs session when needed."
  (unless (or my/package--archives-refreshed
              package-archive-contents)
    (message "Refreshing package archives...")
    (package-refresh-contents)
    (setq my/package--archives-refreshed t)))

(defun my/package-install-archive (pkg)
  "Install archive package PKG if it is missing."
  (unless (package-installed-p pkg)
    (my/package-refresh-archives-once)
    (message "Installing archive package %s..." pkg)
    (package-install pkg)))

(defun my/package-register-vc (package spec)
  "Register PACKAGE with VC SPEC for bootstrap export."
  (setq my/package-vc-recipes (assq-delete-all package my/package-vc-recipes))
  (push (cons package (copy-tree spec)) my/package-vc-recipes)
  (when (boundp 'package-vc-selected-packages)
    (setq package-vc-selected-packages
          (assq-delete-all package package-vc-selected-packages))
    (push (cons package (copy-tree spec)) package-vc-selected-packages))
  (cons package spec))

(defun my/package-vc-recipes-sorted ()
  "Return a stable copy of `my/package-vc-recipes'."
  (sort (copy-tree my/package-vc-recipes)
        (lambda (a b)
          (string< (symbol-name (car a))
                   (symbol-name (car b))))))

(defun my/package-ensure-vc (package url &optional rev)
  "Install PACKAGE from URL via package-vc when it is missing.
REV defaults to `:last-release'."
  (let ((spec `(:url ,url :rev ,(or rev :last-release))))
    (my/package-register-vc package spec)
    (unless (or (package-installed-p package)
                ;; VC packages may already exist as working trees under elpa/
                ;; but still confuse package-vc into prompting for overwrite.
                ;; If the library is already loadable, treat it as installed.
                (locate-library (symbol-name package)))
      (require 'package-vc)
      (package-vc-install (cons package spec)))))

(defun my/package-read-lock-file ()
  "Load `my/package-lock-file' if it exists."
  (when (file-exists-p my/package-lock-file)
    (load-file my/package-lock-file)
    t))

(defun my/package-read-lock-state ()
  "Return package metadata recorded in `my/package-lock-file'.
The result is a plist with `:version', `:packages', and `:vc-packages'."
  (when (file-exists-p my/package-lock-file)
    (let (package-selected-packages
          package-vc-selected-packages
          my/package-lock-version)
      (load-file my/package-lock-file)
      (list :version my/package-lock-version
            :packages (copy-tree package-selected-packages)
            :vc-packages (copy-tree package-vc-selected-packages)))))

(defun my/package--package-desc (pkg)
  "Return the first package descriptor for PKG from `package-alist'."
  (cadr (assq pkg package-alist)))

(defun my/package--vc-package-p (pkg)
  "Return non-nil when PKG is installed as a VC package."
  (let ((desc (my/package--package-desc pkg)))
    (eq (and desc (package-desc-kind desc)) 'vc)))

(defun my/package--installed-third-party-packages ()
  "Return installed third-party packages in stable order."
  (let ((pkgs nil))
    (dolist (entry package-alist)
      (unless (memq (car entry) my/package-core-packages)
        (push (car entry) pkgs)))
    (sort pkgs
          (lambda (a b)
            (string< (symbol-name a)
                     (symbol-name b))))))

(defun my/package--current-vc-package-list ()
  "Return VC package specs collected from init declarations and package state."
  (let ((merged nil))
    (dolist (entry (append (and (boundp 'my/package-vc-recipes)
                                (copy-tree my/package-vc-recipes))
                           (and (boundp 'package-vc-selected-packages)
                                (copy-tree package-vc-selected-packages))))
      (pcase-let ((`(,name . ,spec) entry))
        (setq merged (assq-delete-all name merged))
        (push (cons name spec) merged)))
    (sort merged
          (lambda (a b)
            (string< (symbol-name (car a))
                     (symbol-name (car b)))))))

(defun my/package--current-package-list ()
  "Return installed non-VC packages in stable order."
  (let ((vc-names (mapcar #'car (my/package--current-vc-package-list))))
    (cl-remove-if (lambda (pkg)
                    (or (memq pkg vc-names)
                        (my/package--vc-package-p pkg)))
                  (my/package--installed-third-party-packages))))

(defun my/package--normalize-symbol-list (items)
  "Return ITEMS as a stable symbol list without duplicates."
  (sort (delete-dups (copy-sequence (or items nil)))
        (lambda (a b)
          (string< (symbol-name a)
                   (symbol-name b)))))

(defun my/package--normalize-vc-package-list (items)
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

(defun my/package--missing-symbols (left right)
  "Return symbols present in LEFT but not in RIGHT."
  (cl-set-difference (my/package--normalize-symbol-list left)
                     (my/package--normalize-symbol-list right)
                     :test #'eq))

(defun my/package--vc-entry-equal-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same VC package entry."
  (and (eq (car left) (car right))
       (equal (cdr left) (cdr right))))

(defun my/package--missing-vc-entries (left right)
  "Return VC entries present in LEFT but not in RIGHT."
  (cl-remove-if
   (lambda (entry)
     (seq-some (lambda (other)
                 (my/package--vc-entry-equal-p entry other))
               right))
   (my/package--normalize-vc-package-list left)))

(defun my/package-export-lock-file (&optional file)
  "Write installed archive and VC packages to FILE.
FILE defaults to `my/package-lock-file'."
  (let ((target (or file my/package-lock-file))
        (pkgs (my/package--current-package-list))
        (vc-pkgs (my/package--current-vc-package-list)))
    (with-temp-file target
      (insert ";;; package-lock.el --- Generated by bootstrap.el -*- lexical-binding: t; -*-\n\n")
      (insert ";; 自动生成的包锁文件：普通包走 package-install，VC 包走 package-vc-install。\n\n")
      (insert "(setq my/package-lock-version 2)\n\n")
      (insert "(setq package-selected-packages\n      '")
      (prin1 pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(setq package-vc-selected-packages\n      '")
      (prin1 vc-pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(provide 'package-lock)\n"))
    target))

(defun my/package-lock-audit ()
  "Compare current package state against `my/package-lock-file'.
Return a plist suitable for batch checks."
  (interactive)
  (let* ((lock-state (my/package-read-lock-state))
         (lock-version (plist-get lock-state :version))
         (locked-packages (plist-get lock-state :packages))
         (locked-vc-packages (plist-get lock-state :vc-packages))
         (current-packages (my/package--current-package-list))
         (current-vc-packages (my/package--current-vc-package-list))
         (archive-missing-in-lock
          (my/package--missing-symbols current-packages locked-packages))
         (archive-extra-in-lock
          (my/package--missing-symbols locked-packages current-packages))
         (vc-missing-in-lock
          (my/package--missing-vc-entries current-vc-packages locked-vc-packages))
         (vc-extra-in-lock
          (my/package--missing-vc-entries locked-vc-packages current-vc-packages))
         (ok (and lock-state
                  (eq lock-version my/package-lock-supported-version)
                  (null archive-missing-in-lock)
                  (null archive-extra-in-lock)
                  (null vc-missing-in-lock)
                  (null vc-extra-in-lock)))
         (report
          (list :ok ok
                :lock-file my/package-lock-file
                :lock-version lock-version
                :expected-version my/package-lock-supported-version
                :archive-missing-in-lock archive-missing-in-lock
                :archive-extra-in-lock archive-extra-in-lock
                :vc-missing-in-lock vc-missing-in-lock
                :vc-extra-in-lock vc-extra-in-lock)))
    (if (called-interactively-p 'interactive)
        (message "%S" report)
      report)))

(defun my/package-install-from-lock ()
  "Install archive and VC packages recorded in `my/package-lock-file'."
  (when (my/package-read-lock-file)
    (when (boundp 'package-selected-packages)
      (dolist (pkg package-selected-packages)
        (condition-case err
            (my/package-install-archive pkg)
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
                  (my/package-refresh-archives-once)
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
    t))

(defun my/package--upgrade-archive-packages (query)
  "Upgrade archive packages.
When QUERY is non-nil, allow the underlying package upgrader to prompt."
  (my/package-refresh-archives-once)
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

(defun my/package--upgrade-vc-packages ()
  "Upgrade installed VC packages."
  (require 'package-vc)
  (cond
   ((fboundp 'package-vc-upgrade-all)
    (package-vc-upgrade-all)
    t)
   ((and (fboundp 'package-vc-upgrade)
         (boundp 'package-vc-selected-packages))
    (dolist (entry package-vc-selected-packages)
      (package-vc-upgrade (car entry)))
    t)
   (t nil)))

(defun my/package-bootstrap-from-lock-if-needed ()
  "Install packages from `my/package-lock-file' on first startup.
This makes a freshly cloned config survivable even when the user launches
Emacs directly instead of running `bootstrap.el' first."
  (when (< (length (my/package--installed-third-party-packages))
           my/package-bootstrap-min-installed-count)
    (when (file-exists-p my/package-lock-file)
      (message "Detected fresh package state; restoring dependencies from %s..."
               my/package-lock-file)
      (my/package-install-from-lock))))

(defun my/package-upgrade-all (&optional query)
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
                (my/package--upgrade-archive-packages query))
          (setq vc-upgrade-available
                (my/package--upgrade-vc-packages))
          (setq upgraded-any
                (or archive-upgrade-available
                    vc-upgrade-available)))
      (error
       (user-error "Package upgrade failed: %s" (error-message-string err))))
    (unless upgraded-any
      (user-error "Package upgrade command is unavailable in this Emacs"))
    (let ((lock-file (my/package-export-lock-file)))
      (message "Package upgrade finished; lock file refreshed at %s" lock-file))))

(defun my/package-upgrade-all-noninteractive ()
  "Upgrade installed packages without minibuffer prompts.
Intended for dashboard and button-based entry points."
  (interactive)
  (my/package-upgrade-all nil))

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

(provide 'init-package-utils)
;;; init-package-utils.el ends here
