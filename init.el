(setq user-full-name "aaron")
;; =========================
;; 同步 shell PATH 到 Emacs（macOS 必备）
;; =========================
(require 'package)
(unless package--initialized
  (package-initialize))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)



(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package direnv
  :ensure t
  :after exec-path-from-shell
  :config
  (direnv-mode))
;; 关键：以后 use-package 默认都会自动安装缺失包
;(setq use-package-always-ensure t)

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


(with-eval-after-load 'direnv
  (require 'nix-env) ;; 去掉 nil t，确保找不到文件时直接报错
  (when (fboundp 'nix-env-apply)

    (defun nix-env-apply-if-local ()
      "Apply nix-env only for local buffers (skip TRAMP)."
      (interactive)
      (if (file-remote-p default-directory)
          (message "nix-env: skipped (remote directory)")
        (nix-env-apply)
        (message "nix-env: successfully applied for local directory")))

    ;; 启动时应用
    (nix-env-apply-if-local)

    ;; direnv 更新后应用
    (add-hook 'direnv-after-update-environment-hook #'nix-env-apply-if-local)))


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
(require 'init-org-zotero)
(require 'init-eaf)
(require 'init-text)
(require 'init-mail)
(require 'init-shell)
(require 'init-spell)
(require 'init-gpt)
(require 'init-search)
(require 'init-direnv)
(require 'init-smartparens)
(require 'init-rainbow-delimiters)
(require 'init-avy)
(require 'init-multiple-cursors)
(require 'init-auctex)
(require 'init-jupyter)
(require 'init-browser)
(require 'init-stock)
(require 'init-fzfs)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-macos))

;;; Commentary:
;; Two commands:
;; 1) my/byte-recompile-lisp-dir   : byte-compile ~/.emacs.d/lisp recursively
;; 2) my/native-compile-lisp-dir   : native-compile ~/.emacs.d/lisp recursively (async queue)
;;
;; Paths are resolved via `user-emacs-directory` (no hard-coded absolute paths).
;;
;; Warning policy:
;; - Compile-time: show warnings/errors (do NOT suppress byte-compiler warnings here).
;; - Runtime: optional suppression via `warning-minimum-level` (does not affect byte compile output).

;;; Code:

;;;; ---------------------------------------------------------------------------
;;;; 0) Directory helper (no hard-coded paths)
;;;; ---------------------------------------------------------------------------

(defconst my/lisp-dir
  (file-name-as-directory (expand-file-name "lisp" user-emacs-directory))
  "Directory containing my Emacs Lisp config files.")

(defun my/ensure-lisp-dir ()
  "Ensure `my/lisp-dir` exists."
  (unless (file-directory-p my/lisp-dir)
    (user-error "Directory does not exist: %s" my/lisp-dir))
  my/lisp-dir)

;;;; ---------------------------------------------------------------------------
;;;; 1) BYTE COMPILE (recursive)
;;;; ---------------------------------------------------------------------------

(defun my/byte-recompile-lisp-dir (&optional force)
  "Byte-recompile `my/lisp-dir` recursively.
With prefix arg FORCE, recompile all files; otherwise only outdated ones."
  (interactive "P")
  (require 'bytecomp)
  (let ((base (my/ensure-lisp-dir)))
    ;; byte-recompile-directory: second arg 2 = recurse
    ;; third arg: if non-nil => compile all; if nil => only outdated.
    ;; NOTE: We intentionally DO NOT suppress warnings here.
    (byte-recompile-directory base 2 force)
    (message "Byte-recompile done: %s (force=%s)" base (and force t))))

;;;; ---------------------------------------------------------------------------
;;;; 2) NATIVE COMPILE (recursive, async queue)
;;;; ---------------------------------------------------------------------------

(defun my/native-comp-available-p ()
  "Return non-nil if this Emacs supports native compilation."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)
       (fboundp 'native-compile-async)))

(defun my/native-compile-lisp-dir (&optional force)
  "Native-compile `my/lisp-dir` recursively (enqueue async).
With prefix arg FORCE, delete old .eln files first to force recompilation.

This requires an Emacs build with native-comp support."
  (interactive "P")
  (cond
   ((not (my/native-comp-available-p))
    (message "Native compilation is not available in this Emacs build."))
   (t
    (let ((base (my/ensure-lisp-dir))
          (eln-cache (when (boundp 'native-comp-eln-load-path)
                       (car native-comp-eln-load-path))))
      (when force
        ;; 删除缓存中的所有 .eln 文件以强制重新编译
        (when (and eln-cache (file-directory-p eln-cache))
          (dolist (eln (directory-files-recursively eln-cache "\\.eln\\'"))
            (delete-file eln))
          (message "Deleted old .eln files from %s" eln-cache)))
      ;; 递归入队编译
      (native-compile-async base 'recursively)
      (message "Native compile queued: %s -> %s (force=%s)"
               base (or eln-cache "<eln-cache>") (and force t))))))

;;;; ---------------------------------------------------------------------------
;;;; 3) OPTIONAL: runtime warning suppression (does NOT affect compile-time output)
;;;; ---------------------------------------------------------------------------

(defcustom my/suppress-runtime-warnings t
  "If non-nil, suppress runtime warnings below :error."
  :type 'boolean)

(when my/suppress-runtime-warnings
  ;; Runtime only: keep errors visible, silence warnings/info.
  (setq warning-minimum-level :error))


(provide 'init)

;;; init.el ends here
