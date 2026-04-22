;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(setq user-full-name "aaron")
(setq load-prefer-newer t)
(require 'package)

(defvar exec-path-from-shell-arguments)
(defvar exec-path-from-shell-check-startup-files)
(declare-function exec-path-from-shell-getenv "exec-path-from-shell" (name))

;; 包源要在任何安装逻辑之前设置好
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org"    . "https://orgmode.org/elpa/")))
;; =========================
;; 同步 shell PATH 到 Emacs（macOS 必备）
;; =========================
(defun my/package-initialize-once ()
  "Initialize package.el if needed."
  (unless package--initialized
    (package-initialize)))

(defun my/ensure-use-package-installed ()
  "Install `use-package' when missing."
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(my/package-initialize-once)
(my/ensure-use-package-installed)
(require 'use-package)

(defconst my/shell-environment-variables
  '("PATH" "MANPATH" "INFOPATH" "TEXINPUTS" "BIBINPUTS" "BSTINPUTS")
  "Environment variables copied from the login shell into Emacs.")

(defun my/refresh-environment-from-shell ()
  "Refresh PATH-like environment variables from the user's login shell."
  (interactive)
  (when (eq system-type 'darwin)
    (cond
     ((featurep 'exec-path-from-shell)
      (dolist (variable my/shell-environment-variables)
        (let ((value (ignore-errors
                       (exec-path-from-shell-getenv variable))))
          (when (stringp value)
            (setenv variable value))))
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
  (my/refresh-environment-from-shell))

(add-hook 'server-after-make-frame-hook #'my/refresh-environment-from-shell)
;; 关键：以后 use-package 默认都会自动安装缺失包
;(setq use-package-always-ensure t)

;;; Commentary:
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it by the
;; `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' benefits from that.
;;
;; `cat /proc/sys/fs/pipe-max-size` to check the max value.
(setq read-process-output-max (* 4 1024 1024))
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

(let* ((dir (locate-user-emacs-file "lisp"))
       (org-dir (expand-file-name "org" dir))
       (git-dir (expand-file-name "git" dir)))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory org-dir))
  (add-to-list 'load-path (file-name-as-directory git-dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))
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

(require 'init-package-utils)
(my/package-bootstrap-from-lock-if-needed)

(defun my/reload-init ()
  "Reload Emacs init file safely."
  (interactive)
  (let ((init-file user-init-file))
    (when (and init-file (file-exists-p init-file))
      (message "Reloading init file: %s" init-file)
      (condition-case err
          (load-file init-file)
        (error
         (message "Error reloading init file: %s"
                  (error-message-string err)))))))



(require 'init-modules)
(require 'init-compile)



(provide 'init)

;;; init.el ends here
