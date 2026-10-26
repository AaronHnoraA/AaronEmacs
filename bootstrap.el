;;; bootstrap.el --- Bootstrap packages from lock file -*- lexical-binding: t; -*-

(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; 调整 GC，安装阶段更快
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(require 'cl-lib)
;; Some package autoloads currently emit top-level
;; `define-compilation-mode' forms.  `package-initialize' evaluates those
;; autoload files while activating packages, so make the built-in provider
;; available before activation.
(require 'compile)
(require 'package)

(defvar package-selected-packages)
(defvar package-vc-selected-packages)
(defvar aaron-package-lock-version)

;; 仓库按需改
(setq package-archives
      '(("etaf"   . "https://etaf.top/packages/")
        ("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org"    . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/core" user-emacs-directory))
(require 'aaron-packages)

;; ------------------------------------------------------------
;; 配置：你的 init.el 和 lock 文件路径
;; ------------------------------------------------------------

(defconst aaron-bootstrap-init-file
  (expand-file-name "init.el" user-emacs-directory)
  "Path to the real init.el that defines all use-package config.")

(defconst aaron-bootstrap-lock-file
  (expand-file-name "package-lock.el" user-emacs-directory)
  "Path to the package lock file storing package metadata.")

(defconst aaron-bootstrap-lock-supported-version 2
  "Expected schema version for `aaron-bootstrap-lock-file'.")

(defconst aaron-bootstrap-mode-env "BOOTSTRAP_MODE"
  "Environment variable controlling bootstrap behavior.")

(defconst aaron-bootstrap-flag-var
  '+bootstrap-has-run
  "Feature/variable name used to detect an already bootstrapped environment.")

(defconst aaron-bootstrap-min-old-env-package-count 3
  "Installed package count threshold used to detect an existing environment.")

(defconst aaron-bootstrap-core-packages
  '(use-package)
  "Packages that should not count toward the old-environment heuristic.")

(defvar aaron-bootstrap--archives-refreshed nil
  "Whether package archives have already been refreshed in this run.")

(defvar aaron-bootstrap--resolved-mode nil
  "Resolved bootstrap mode for the current run.")

(defconst aaron-bootstrap-export-trigger-features
  '(org)
  "Features to require before exporting the lock file.
This lets deferred package declarations register their VC recipes.")

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

(defun aaron-bootstrap-refresh-archives-once ()
  "Refresh package archives once per bootstrap run."
  (unless (or aaron-bootstrap--archives-refreshed
              package-archive-contents)
    (message "Refreshing package archives...")
    (package-refresh-contents)
    (setq aaron-bootstrap--archives-refreshed t)))

(defun aaron-bootstrap-ensure-package-installed (pkg)
  "Install archive package PKG if it is missing."
  (unless (package-installed-p pkg)
    (aaron-bootstrap-refresh-archives-once)
    (message "Installing %s..." pkg)
    (package-install pkg)))

(defun aaron-bootstrap--package-desc (pkg)
  "Return the first package descriptor for PKG from `package-alist'."
  (cadr (assq pkg package-alist)))

(defun aaron-bootstrap--vc-package-p (pkg)
  "Return non-nil if PKG is installed as a VC package."
  (let ((desc (aaron-bootstrap--package-desc pkg)))
    (eq (and desc (package-desc-kind desc)) 'vc)))

(defun aaron-bootstrap--archive-package-installed-p (pkg)
  "Return non-nil when PKG is installed as a non-VC package."
  (and (package-installed-p pkg)
       (not (aaron-bootstrap--vc-package-p pkg))))

(defun aaron-bootstrap--vc-package-installed-p (pkg)
  "Return non-nil when PKG is installed as a VC package."
  (and (package-installed-p pkg)
       (aaron-bootstrap--vc-package-p pkg)))

(defun aaron-bootstrap--delete-installed-package (pkg)
  "Delete installed package PKG when present."
  (when-let* ((desc (aaron-bootstrap--package-desc pkg)))
    (message "Removing mismatched package %s (%s)..."
             pkg
             (or (package-desc-kind desc) 'archive))
    (package-delete desc t)))

(defun aaron-bootstrap--installed-third-party-packages ()
  "Return currently installed third-party packages excluding bootstrap essentials."
  (let ((pkgs nil))
    (dolist (entry package-alist)
      (let ((pkg (car entry)))
        (unless (memq pkg aaron-bootstrap-core-packages)
          (push pkg pkgs))))
    (sort pkgs (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun aaron-bootstrap--current-package-list ()
  "Return installed non-VC packages in stable order."
  (let ((vc-names (mapcar #'car (aaron-bootstrap--current-vc-package-list))))
    (cl-remove-if (lambda (pkg)
                    (or (memq pkg vc-names)
                        (aaron-bootstrap--vc-package-p pkg)))
                  (aaron-bootstrap--installed-third-party-packages))))

(defun aaron-bootstrap--current-vc-package-list ()
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

(defun aaron-bootstrap-write-lock-file ()
  "Write installed archive and VC packages to `aaron-bootstrap-lock-file'."
  (let ((pkgs (aaron-bootstrap--current-package-list))
        (vc-pkgs (aaron-bootstrap--current-vc-package-list)))
    (with-temp-file aaron-bootstrap-lock-file
      (insert ";;; package-lock.el --- Generated by bootstrap.el -*- lexical-binding: t; -*-\n\n")
      (insert ";; 自动生成的包锁文件：普通包走 package-install，VC 包走 package-vc-install。\n\n")
      (insert "(setq aaron-package-lock-version 2)\n\n")
      (insert "(setq package-selected-packages\n      '")
      (prin1 pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(setq package-vc-selected-packages\n      '")
      (prin1 vc-pkgs (current-buffer))
      (insert ")\n\n")
      (insert "(provide 'package-lock)\n"))))

(defun aaron-bootstrap-read-lock-file ()
  "Load `aaron-bootstrap-lock-file' if it exists."
  (when (file-exists-p aaron-bootstrap-lock-file)
    (load-file aaron-bootstrap-lock-file)))

(defun aaron-bootstrap-read-lock-state ()
  "Return package metadata recorded in `aaron-bootstrap-lock-file'."
  (when (file-exists-p aaron-bootstrap-lock-file)
    (let (package-selected-packages
          package-vc-selected-packages
          aaron-package-lock-version)
      (load-file aaron-bootstrap-lock-file)
      (list :version aaron-package-lock-version
            :packages (copy-tree package-selected-packages)
            :vc-packages (copy-tree package-vc-selected-packages)))))

(defun aaron-bootstrap--normalize-symbol-list (items)
  "Return ITEMS as a stable symbol list without duplicates."
  (sort (delete-dups (copy-sequence (or items nil)))
        (lambda (a b)
          (string< (symbol-name a)
                   (symbol-name b)))))

(defun aaron-bootstrap--normalize-vc-package-list (items)
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

(defun aaron-bootstrap--missing-symbols (left right)
  "Return symbols present in LEFT but not in RIGHT."
  (cl-set-difference (aaron-bootstrap--normalize-symbol-list left)
                     (aaron-bootstrap--normalize-symbol-list right)
                     :test #'eq))

(defun aaron-bootstrap--vc-entry-equal-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same VC package entry."
  (and (eq (car left) (car right))
       (equal (cdr left) (cdr right))))

(defun aaron-bootstrap--missing-vc-entries (left right)
  "Return VC entries present in LEFT but not in RIGHT."
  (cl-remove-if
   (lambda (entry)
     (seq-some (lambda (other)
                 (aaron-bootstrap--vc-entry-equal-p entry other))
               right))
   (aaron-bootstrap--normalize-vc-package-list left)))

(defun aaron-bootstrap-audit-lock ()
  "Compare installed packages against `aaron-bootstrap-lock-file'."
  (let* ((lock-state (aaron-bootstrap-read-lock-state))
         (lock-version (plist-get lock-state :version))
         (locked-packages (aaron-bootstrap--normalize-symbol-list
                           (plist-get lock-state :packages)))
         (locked-vc-packages (aaron-bootstrap--normalize-vc-package-list
                              (plist-get lock-state :vc-packages)))
         (installed-archive-packages
          (cl-remove-if-not #'aaron-bootstrap--archive-package-installed-p
                            (aaron-bootstrap--installed-third-party-packages)))
         (installed-vc-packages
          (cl-remove-if-not #'aaron-bootstrap--vc-package-installed-p
                            (aaron-bootstrap--installed-third-party-packages)))
         (archive-missing-in-lock
          (cl-remove-if #'aaron-bootstrap--archive-package-installed-p locked-packages))
         (archive-extra-in-lock
          (aaron-bootstrap--missing-symbols installed-archive-packages locked-packages))
         (vc-missing-in-lock
          (cl-remove-if
           (lambda (entry)
             (aaron-bootstrap--vc-package-installed-p (car entry)))
           locked-vc-packages))
         (vc-extra-in-lock
          (cl-remove-if
           (lambda (pkg)
             (assq pkg locked-vc-packages))
           installed-vc-packages)))
    (list :ok (and lock-state
                   (eq lock-version aaron-bootstrap-lock-supported-version)
                   (null archive-missing-in-lock)
                   (null archive-extra-in-lock)
                   (null vc-missing-in-lock)
                   (null vc-extra-in-lock))
          :lock-file aaron-bootstrap-lock-file
          :lock-version lock-version
          :expected-version aaron-bootstrap-lock-supported-version
          :archive-missing-in-lock archive-missing-in-lock
          :archive-extra-in-lock archive-extra-in-lock
          :vc-missing-in-lock vc-missing-in-lock
          :vc-extra-in-lock vc-extra-in-lock)))

(defun aaron-bootstrap--env-mode ()
  "Return bootstrap mode from `aaron-bootstrap-mode-env', if valid."
  (when-let* ((value (getenv aaron-bootstrap-mode-env))
              (mode (intern (downcase (string-trim value)))))
    (pcase mode
      ((or 'auto 'install 'export 'audit) mode)
      (_
       (error "Unsupported %s=%s (expected auto/install/export/audit)"
              aaron-bootstrap-mode-env value)))))

(defun aaron-bootstrap-install-archive-packages-from-lock ()
  "Install archive packages from `package-selected-packages'."
  (when (boundp 'package-selected-packages)
    (dolist (pkg package-selected-packages)
      (unless (aaron-bootstrap--archive-package-installed-p pkg)
        (condition-case err
            (progn
              (when (package-installed-p pkg)
                (aaron-bootstrap--delete-installed-package pkg))
              (aaron-bootstrap-ensure-package-installed pkg))
          (error
           (message "Failed to install archive package %s: %s"
                    pkg (error-message-string err))))))))

(defun aaron-bootstrap-install-vc-packages-from-lock ()
  "Install VC packages from `package-vc-selected-packages'."
  (when (boundp 'package-vc-selected-packages)
    (require 'package-vc)
    (dolist (entry package-vc-selected-packages)
      (pcase-let ((`(,pkg . ,spec) entry))
        (unless (aaron-bootstrap--vc-package-installed-p pkg)
          (condition-case err
              (progn
                (when (package-installed-p pkg)
                  (aaron-bootstrap--delete-installed-package pkg))
                (aaron-bootstrap-refresh-archives-once)
                (message "Installing VC package %s..." pkg)
                (cond
                 ((null spec)
                  (package-vc-install pkg))
                 ((stringp spec)
                  (package-vc-install pkg spec))
                 ((listp spec)
                  (package-vc-install (cons pkg spec) (plist-get spec :rev)))
                 (t
                  (error "Unsupported VC package spec: %S" spec))))
            (error
             (message "Failed to install VC package %s: %s"
                      pkg (error-message-string err)))))))
    (dolist (entry aaron-package-vc-patches)
      (when (package-installed-p (car entry))
        (aaron-package-apply-vc-patch (car entry))))))

;; ------------------------------------------------------------
;; Main flow
;; ------------------------------------------------------------

(defun aaron-bootstrap-old-env-p ()
  "Detect whether this looks like an existing local environment.

The heuristic is intentionally simple: if there are already enough
third-party packages installed locally, treat this machine as the
source environment and export a fresh lock file; otherwise, treat it as
the target environment and install from the checked-in lock file."
  (or (featurep aaron-bootstrap-flag-var)
      (boundp aaron-bootstrap-flag-var)
      (>= (length (aaron-bootstrap--installed-third-party-packages))
          aaron-bootstrap-min-old-env-package-count)))

(defun aaron-bootstrap-resolve-mode ()
  "Resolve bootstrap mode for the current run.

Priority:
1. Explicit `BOOTSTRAP_MODE'
2. If a lock file exists, prefer deterministic restore
3. Fall back to the old-environment heuristic"
  (or aaron-bootstrap--resolved-mode
      (setq aaron-bootstrap--resolved-mode
            (or (aaron-bootstrap--env-mode)
                (if (file-exists-p aaron-bootstrap-lock-file)
                    'install
                  (if (aaron-bootstrap-old-env-p)
                      'export
                    'install))))))

(defun aaron-bootstrap-run ()
  "Main entry for bootstrap."
  (pcase (aaron-bootstrap-resolve-mode)
    ('export
      (progn
        (aaron-bootstrap-ensure-package-installed 'use-package)
        (require 'use-package)
        (message "Bootstrap mode=export. Loading init.el and refreshing package-lock.el...")
        (when (file-exists-p aaron-bootstrap-init-file)
          (load-file aaron-bootstrap-init-file))
        (dolist (feature aaron-bootstrap-export-trigger-features)
          (ignore-errors
            (require feature)))
        (aaron-bootstrap-write-lock-file)
        (message "package-lock.el generated at %s" aaron-bootstrap-lock-file)))
    ('install
     (progn
       (message "Bootstrap mode=install. Restoring dependencies from package-lock.el...")
       (aaron-bootstrap-read-lock-file)
       (if (or (boundp 'package-selected-packages)
               (boundp 'package-vc-selected-packages))
           (progn
             (aaron-bootstrap-install-archive-packages-from-lock)
             (aaron-bootstrap-install-vc-packages-from-lock))
         (message "No package-lock.el found. Only use-package is ensured; rest will be installed lazily."))))
    ('audit
     (progn
       (message "Bootstrap mode=audit. Checking package-lock.el against installed packages...")
       (prin1 (aaron-bootstrap-audit-lock))
       (terpri)))
    (_
     (error "Unsupported bootstrap mode: %S" (aaron-bootstrap-resolve-mode))))

  ;; 恢复 GC 设置 + 退出
  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.1)
  (message "Bootstrap finished. You can now restart Emacs with your normal init.")
  (run-at-time
   "1 sec" nil
   (lambda ()
     (save-some-buffers t)
     (kill-emacs 0))))

(aaron-bootstrap-run)

(provide 'bootstrap)
;;; bootstrap.el ends here
