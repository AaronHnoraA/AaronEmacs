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
(require 'init-joplin)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-macos))


;;; Commentary:
;; Two commands:
;; 1) my/byte-recompile-lisp-dir   : byte-compile ~/.emacs.d/lisp recursively
;; 2) my/native-compile-lisp-dir   : native-compile ~/.emacs.d/lisp recursively (async queue)
;;
;; Paths are resolved via `user-emacs-directory` (no hard-coded absolute paths).

;;; Code:

(require 'cl-lib)

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
;;;; 0.1) Native-comp log helpers & Policies
;;;; ---------------------------------------------------------------------------

(defcustom my/native-comp-pop-log t
  "If non-nil, display native compilation log buffer when compilation starts."
  :type 'boolean)

(defcustom my/native-comp-async-report-policy 'silent
  "Value assigned to `native-comp-async-report-warnings-errors`.
- nil    : default behavior (Emacs decides)
- 'silent: reduce intrusive popups, keep logs in the log buffer
- t      : be more explicit about warnings/errors"
  :type '(choice (const :tag "Default (nil)" nil)
                 (const :tag "Silent (recommended)" silent)
                 (const :tag "Verbose (t)" t)))

(defcustom my/native-comp-verbose 1
  "Value assigned to `native-comp-verbose`."
  :type 'integer)

(defun my/native-comp--log-buffer ()
  "Return an existing native compilation log buffer if any."
  (or (get-buffer "*Async-native-compile-log*")
      (get-buffer "*Native-compile-Log*")
      (get-buffer "*Native-compile-Log*<2>")))

(defun my/native-comp-open-log ()
  "Open native compilation log buffer if it exists."
  (interactive)
  (let ((buf (my/native-comp--log-buffer)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (message "No native compilation log buffer yet (start native compilation first)."))))

;;;; ---------------------------------------------------------------------------
;;;; 1) BYTE COMPILE (recursive)
;;;; ---------------------------------------------------------------------------

(defun my/byte-recompile-lisp-dir (&optional force)
  "Byte-recompile `my/lisp-dir` recursively.
With prefix arg FORCE, recompile all files; otherwise only outdated ones."
  (interactive "P")
  (require 'bytecomp)
  (let ((base (my/ensure-lisp-dir)))
    (byte-recompile-directory base 2 force)
    (message "Byte-recompile done: %s (force=%s)" base (and force t))))

;;;; ---------------------------------------------------------------------------
;;;; 2) NATIVE COMPILE (recursive, async queue)
;;;; ---------------------------------------------------------------------------

(defvar my/native-comp--progress-timer nil
  "Timer used to report native compilation progress.")

(defvar my/native-comp--progress-start-time nil
  "When progress reporting started.")

(defun my/native-comp-available-p ()
  "Return non-nil if this Emacs supports native compilation."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)
       (fboundp 'native-compile-async)))

(defun my/native-comp--queue-size ()
  "Return total async native compilation queue size (active + pending).
Safely handles both Emacs 28 (lists) and Emacs 29+ (hash-tables)."
  (let ((active (if (boundp 'comp-async-compilations)
                    (if (hash-table-p comp-async-compilations)
                        (hash-table-count comp-async-compilations)
                      (length comp-async-compilations))
                  0))
        (queued (if (boundp 'comp-files-queue)
                    (if (hash-table-p comp-files-queue)
                        (hash-table-count comp-files-queue)
                      (length comp-files-queue))
                  0)))
    (+ active queued)))

(defun my/native-comp--start-progress (base)
  "Start minibuffer progress reporting for native compilation of BASE."
  (setq my/native-comp--progress-start-time (float-time))
  (when (timerp my/native-comp--progress-timer)
    (cancel-timer my/native-comp--progress-timer))
  (setq my/native-comp--progress-timer
        (run-with-timer
         1 1
         (lambda ()
           (let ((n (my/native-comp--queue-size)))
             (cond
              ((<= n 0)
               (when (timerp my/native-comp--progress-timer)
                 (cancel-timer my/native-comp--progress-timer))
               (setq my/native-comp--progress-timer nil)
               (let ((elapsed (and my/native-comp--progress-start-time
                                   (- (float-time) my/native-comp--progress-start-time))))
                 (message "✓ Native compilation completed (%.1fs). See log buffer if needed." (or elapsed 0.0))))
              (t
               (message "Native compilation in progress... remaining jobs: %d  [%s]" n base))))))))

(defun my/native-comp--maybe-show-log ()
  "Show native compilation log buffer a moment later if it exists."
  (when my/native-comp-pop-log
    (run-with-timer
     0.8 nil
     (lambda ()
       (let ((buf (my/native-comp--log-buffer)))
         (when (buffer-live-p buf)
           (display-buffer buf)))))))

(defun my/native-comp--safe-delete-eln (dir)
  "Safely delete ONLY the .eln files corresponding to .el files in DIR.
Works across Emacs 28 and 29+."
  (require 'comp)
  (let ((deleted-count 0))
    (dolist (el-file (directory-files-recursively dir "\\.el\\'"))
      ;; comp-el-to-eln-file is Emacs 29+, comp-el-to-eln-filename is Emacs 28
      (let ((eln-file (ignore-errors 
                        (cond ((fboundp 'comp-el-to-eln-file) (comp-el-to-eln-file el-file))
                              ((fboundp 'comp-el-to-eln-filename) (comp-el-to-eln-filename el-file))))))
        (when (and eln-file (file-exists-p eln-file))
          (delete-file eln-file)
          (cl-incf deleted-count))))
    deleted-count))

(defun my/native-compile-lisp-dir (&optional force)
  "Native-compile `my/lisp-dir` recursively (enqueue async).
With prefix arg FORCE, safely delete old .eln files for this dir to force recompilation."
  (interactive "P")
  (cond
   ((not (my/native-comp-available-p))
    (message "Native compilation is not available in this Emacs build."))
   (t
    (let ((base (my/ensure-lisp-dir)))
      ;; Apply policies
      (when (boundp 'native-comp-verbose)
        (setq native-comp-verbose my/native-comp-verbose))
      (when (boundp 'native-comp-async-report-warnings-errors)
        (setq native-comp-async-report-warnings-errors my/native-comp-async-report-policy))

      ;; Force clear *target specific* cache
      (when force
        (let ((count (my/native-comp--safe-delete-eln base)))
          (message "Deleted %d old .eln cache files for %s" count base)))

      ;; Enqueue compilation
      (native-compile-async base t)

      (my/native-comp--maybe-show-log)
      (my/native-comp--start-progress base)

      (message "Native compile queued: %s (force=%s). Watch log buffer for details."
               base (and force t))))))

;;;; ---------------------------------------------------------------------------
;;;; 3) OPTIONAL: runtime warning suppression
;;;; ---------------------------------------------------------------------------

(defcustom my/suppress-runtime-warnings t
  "If non-nil, suppress runtime warnings below :error."
  :type 'boolean)

(when my/suppress-runtime-warnings
  (setq warning-minimum-level :error))

;;;; ---------------------------------------------------------------------------
;;;; 4) OPTIONAL: Auto-compile on save
;;;; ---------------------------------------------------------------------------

(defun my/auto-compile-lisp-on-save ()
  "Automatically async native-compile the current file if it's inside `my/lisp-dir`."
  (when (and (eq major-mode 'emacs-lisp-mode)
             buffer-file-name
             (string-prefix-p (expand-file-name my/lisp-dir)
                              (expand-file-name buffer-file-name))
             (my/native-comp-available-p))
    ;; We compile just this specific file instead of the whole directory
    (native-compile-async buffer-file-name)
    (message "Auto-queued native compilation for: %s" (file-name-nondirectory buffer-file-name))))

;; Uncomment the following line to enable auto-compilation on save
;; (add-hook 'after-save-hook #'my/auto-compile-lisp-on-save)



(provide 'init)

;;; init.el ends here
