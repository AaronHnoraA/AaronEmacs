;; =========================
;; 同步 shell PATH 到 Emacs（macOS 必备）
;; =========================
(require 'package)
(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'exec-path-from-shell)
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; 关键：以后 use-package 默认都会自动安装缺失包
(setq use-package-always-ensure t)

;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
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

(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org"    . "https://orgmode.org/elpa/")))

(unless package--initialized
  (package-initialize))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
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

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
(require 'init-evil)
(require 'init-lsp)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-snippets)
(require 'init-treesit)
(require 'init-windows)
(require 'init-project)


;; standalone apps
(require 'init-org)
(require 'init-text)
(require 'init-mail)
(require 'init-shell)
(require 'init-spell)
(require 'init-gpt)
(require 'init-lean)
(require 'init-search)
(require 'init-direnv)
(require 'init-smartparens)
(require 'init-rainbow-delimiters)
(require 'init-indent-guides)
(require 'init-avy)
(require 'init-multiple-cursors)
(require 'init-auctex)
(require 'init-jupyter)
(require 'init-browser)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-macos))

(provide 'init)

;;; init.el ends here
