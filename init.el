;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-

(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(setq load-prefer-newer t)
(require 'compile)
(require 'package)

(defvar exec-path-from-shell-arguments)
(defvar exec-path-from-shell-check-startup-files)
(declare-function exec-path-from-shell-getenv "exec-path-from-shell" (name))

;; 包源要在任何安装逻辑之前设置好
(setq package-archives
      '(("etaf"   . "https://etaf.top/packages/")
        ("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org"    . "https://orgmode.org/elpa/")))

;; =========================
;; 同步 shell PATH 到 Emacs（macOS 必备）
;; =========================
(defun aaron-package-initialize-once ()
  "Initialize package.el if needed."
  (unless package--initialized
    (package-initialize)))

(defun aaron-ensure-use-package-installed ()
  "Install `use-package' when missing."
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(aaron-package-initialize-once)
(aaron-ensure-use-package-installed)
(require 'use-package)

(defconst aaron-shell-environment-variables
  '("PATH" "MANPATH" "INFOPATH"
    "SSH_AUTH_SOCK" "SSH_AGENT_PID"
    "TEXINPUTS" "BIBINPUTS" "BSTINPUTS")
  "Environment variables copied from the login shell into Emacs.")

(defun aaron/refresh-environment-from-shell ()
  "Refresh PATH-like environment variables from the user's login shell."
  (interactive)
  (when (eq system-type 'darwin)
    (cond
     ((featurep 'exec-path-from-shell)
      ;; One login shell for all variables.
      (pcase-dolist (`(,variable . ,value)
                     (ignore-errors
                       (exec-path-from-shell-getenvs
                        aaron-shell-environment-variables)))
        (when (stringp value)
          (setenv variable value)))
      (setq exec-path
            (append (parse-colon-path (or (getenv "PATH") ""))
                    (list exec-directory))))
     (t
      (let* ((shell (or (getenv "SHELL") shell-file-name))
             (path
              (when (and shell (file-executable-p shell))
                (string-trim-right
                 (with-temp-buffer
                   (when (zerop
                          (call-process shell nil t nil "-lc" "printf %s \"$PATH\""))
                     (buffer-string)))))))
        (when (and path (not (string-empty-p path)))
          (setenv "PATH" path)
          (setq exec-path
                (append (parse-colon-path path)
                        (list exec-directory)))))))))



(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin)
           (not noninteractive)
           (or (daemonp) (display-graphic-p)))
  :ensure t
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (aaron/refresh-environment-from-shell))

(add-hook 'server-after-make-frame-hook #'aaron/refresh-environment-from-shell)
;; 关键：以后 use-package 默认都会自动安装缺失包
;(setq use-package-always-ensure t)

;;; Commentary:
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it by the
;; `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Modules are addressed by their path under lisp/ (`tools/lsp/config'), so
;; only lisp/ itself, the core and the Remote library need to be on the path.
(let* ((dir (locate-user-emacs-file "lisp"))
       (general-dir (locate-user-emacs-file "site-lisp/general.el"))
       (aaron-ui-dir (locate-user-emacs-file "site-lisp/aaron-ui"))
       (config-dir (locate-user-emacs-file "site-lisp/config"))
       (vendored-dir (locate-user-emacs-file "site-lisp/vendored")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "core" dir)))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "remote" dir)))
  (add-to-list 'load-path (file-name-as-directory config-dir))
  (add-to-list 'load-path (file-name-as-directory general-dir))
  (add-to-list 'load-path (file-name-as-directory aaron-ui-dir))
  (add-to-list 'load-path (file-name-as-directory vendored-dir)))

(defun aaron-migrate-legacy-noema-state ()
  "Move state from `var/aaronnote/', Noema's former name, into `var/noema/'.
Entries already present under `var/noema/' are left in place.  Nothing
moves while a Noema host started from the old layout is still running."
  (let* ((legacy (locate-user-emacs-file "var/aaronnote/"))
         (current (locate-user-emacs-file "var/noema/"))
         (host (expand-file-name "host.json" legacy))
         (pid (and (file-readable-p host)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents host)
                       (gethash "pid" (json-parse-buffer)))))))
    (when (and (file-directory-p legacy)
               (not (and (natnump pid)
                         (ignore-errors (zerop (signal-process pid 0))))))
      (make-directory current t)
      (dolist (child (directory-files legacy nil
                                      directory-files-no-dot-files-regexp))
        (let ((target (expand-file-name child current)))
          (unless (file-exists-p target)
            (rename-file (expand-file-name child legacy) target))))
      (when (directory-empty-p legacy)
        (delete-directory legacy)))))

(condition-case err
    (aaron-migrate-legacy-noema-state)
  (error
   (display-warning 'init (format "Noema state migration failed: %s"
                                  (error-message-string err)))))

(setq custom-file (locate-user-emacs-file "etc/custom.el"))

(when (file-exists-p custom-file)
  (condition-case err
      (load custom-file nil 'nomessage)
    (error
     (display-warning
      'init
      (format "Failed to load custom file %s: %s"
              custom-file
              (error-message-string err))
      :error))))

(require 'aaron-packages)
(aaron-package-bootstrap-from-lock-if-needed)

(defun aaron/reload-init ()
  "Reload only the top-level Emacs init file.
Already loaded modules are not re-evaluated; restart Emacs for module edits."
  (interactive)
  (let ((init-file user-init-file))
    (unless (and init-file (file-exists-p init-file))
      (user-error "Emacs init file is unavailable"))
    (load-file init-file)
    (message "Reloaded %s; loaded modules were not re-evaluated"
             init-file)))



;; Unified configuration registry: load before the module graph so modules can
;; register while loading.  `config-apply-store' runs on `after-init-hook'.
(require 'config)

(require 'init-modules)



(provide 'init)

;;; init.el ends here
