;;; early-init.el --- Early bird -*- lexical-binding: t; -*-

;;; 提前关掉包系统自动初始化，用 use-package 等自己控制
(setq package-enable-at-startup nil)

;; Prefer fresh source over stale bytecode throughout the startup chain.
(setq load-prefer-newer t)

(defun aaron-native-comp-refresh-gcc-library-path ()
  "Find Homebrew's current GCC runtime library before native compilation.
Emacs.app can retain a versioned `LIBRARY_PATH' from its build.  After a GCC
upgrade that path no longer contains libemutls_w, which subr trampolines need."
  (when (eq system-type 'darwin)
    (let* ((library
            (car (append
                  (file-expand-wildcards
                   "/opt/homebrew/lib/gcc/current/gcc/*/*/libemutls_w.a" t)
                  (file-expand-wildcards
                   "/usr/local/lib/gcc/current/gcc/*/*/libemutls_w.a" t))))
           (directory (and library
                           (directory-file-name
                            (file-name-directory library))))
           (existing (getenv "LIBRARY_PATH")))
      (when (and directory
                 (not (equal directory
                             (car (split-string (or existing "")
                                                path-separator t)))))
        (setenv "LIBRARY_PATH"
                (if (and existing (not (string= existing "")))
                    (concat directory path-separator existing)
                  directory))))))

(aaron-native-comp-refresh-gcc-library-path)

(defconst aaron-startup-byte-compiled-files
  '("early-init.el" "init.el" "bootstrap.el")
  "Top-level startup files whose bytecode can break bootstrap.")

(defvar aaron-delete-local-bytecode-on-startup t
  "Whether startup deletes config-owned `.elc' files before loading modules.

Byte compilation remains useful as a health check, but this personal config
changes frequently enough that stale or badly compiled local bytecode is more
expensive than the small startup win it provides.  This only touches files under
the config's own Elisp trees, not installed packages under `elpa/'.")

(defconst aaron-local-bytecode-directories
  '("lisp" "site-lisp")
  "Config-owned directories whose `.elc' files are disposable.")

(defun aaron-delete-stale-startup-bytecode ()
  "Delete top-level startup `.elc' files before normal init loading continues.

The startup entry points are small and order-sensitive.  Loading an older or
badly compiled `init.elc' can bypass source fixes before `load-prefer-newer' has
a chance to affect the rest of the module graph."
  (dolist (file aaron-startup-byte-compiled-files)
    (let* ((source (expand-file-name file user-emacs-directory))
           (bytecode (concat source "c")))
      (when (and (file-exists-p source)
                 (file-exists-p bytecode))
        (condition-case err
            (delete-file bytecode)
          (error
           (display-warning
            'early-init
            (format "Failed to delete stale startup bytecode %s: %s"
                    bytecode
                    (error-message-string err))
            :error)))))))

(defun aaron-delete-local-bytecode-files ()
  "Delete config-owned `.elc' files before loading local modules."
  (when aaron-delete-local-bytecode-on-startup
    (dolist (directory aaron-local-bytecode-directories)
      (let ((root (expand-file-name directory user-emacs-directory)))
        (when (file-directory-p root)
          (dolist (file (directory-files-recursively root "\\.elc\\'"))
            (condition-case err
                (delete-file file)
              (error
               (display-warning
                'early-init
                (format "Failed to delete local bytecode %s: %s"
                        file
                        (error-message-string err))
                :error)))))))))

(aaron-delete-stale-startup-bytecode)
(aaron-delete-local-bytecode-files)

;; Keep native compilation cache local to this config so cleanup stays simple.
(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

;;; 启动性能优化：暂时放宽 GC、禁用 file-name-handler
(defvar aaron-original-gc-cons-threshold gc-cons-threshold)
(defvar aaron-original-gc-cons-percentage gc-cons-percentage)
(defvar aaron-original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      garbage-collection-messages nil
      frame-inhibit-implied-resize t
      file-name-handler-alist nil)

;; Silence non-critical warning popups as early as possible so startup noise
;; stays in `*Warnings*` instead of stealing focus.
(defvar aaron-warning-popup-minimum-level :error
  "Minimum warning severity that may auto-display the warnings buffer.")

(defvar aaron-warning-suppress-elpa-noise t
  "Whether to suppress low-severity startup noise from third-party ELPA packages.")

(defconst aaron-suppressed-warnings-buffer "*Suppressed Warnings*"
  "Buffer that collects low-severity warnings redirected away from *Messages*.
Browse with `aaron/show-suppressed-warnings'.")

(defun aaron-log-to-suppressed-warnings (type message level)
  "Append a suppressed warning to `aaron-suppressed-warnings-buffer'."
  (with-current-buffer (get-buffer-create aaron-suppressed-warnings-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] (%s) %s: %s\n"
                      (format-time-string "%T")
                      (if (consp type)
                          (mapconcat #'symbol-name type ".")
                        (symbol-name type))
                      (upcase (substring (symbol-name level) 1))
                      message)))))

(defun aaron/show-suppressed-warnings ()
  "Pop up the buffer of warnings that were silenced during startup."
  (interactive)
  (if-let* ((buf (get-buffer aaron-suppressed-warnings-buffer)))
      (pop-to-buffer buf)
    (message "No suppressed warnings recorded.")))

(defun aaron-warning-level-rank (level)
  "Return a comparable numeric rank for warning LEVEL."
  (pcase level
    (:debug 0)
    (:warning 1)
    (:error 2)
    (:emergency 3)
    (_ 1)))

(defun aaron-warning-level-at-least-p (level threshold)
  "Return non-nil when warning LEVEL is at least THRESHOLD."
  (>= (aaron-warning-level-rank level)
      (aaron-warning-level-rank threshold)))

(defun aaron-warning-type-list (type)
  "Normalize warning TYPE into the list form used by `warning-suppress-types'."
  (if (consp type) type (list type)))

(defun aaron-warning-should-suppress-p (type message level)
  "Return non-nil when TYPE/MESSAGE/LEVEL should be redirected to the warning log."
  (let ((level (or level :warning))
        (type-list (aaron-warning-type-list type)))
    (and aaron-warning-suppress-elpa-noise
         (not (aaron-warning-level-at-least-p level :error))
         (or
          ;; Byte-compiler and native-compiler noise from ELPA packages.
          (member 'bytecomp     type-list)
          (member 'obsolete     type-list)
          (member 'comp         type-list)
          ;; (require 'cl) deprecation.
          (member 'cl-functions type-list)
          ;; Remaining message-content checks.
          (and (stringp message)
               (or (string-match-p "/elpa/" message)
                   (string-match-p
                    "setting attribute .+:foreground.+'font-lock-variable-name-face'.+nil value is invalid"
                    message)
                   (and (member 'package type-list)
                        (string-match-p "deprecated" message))))))))

(defun aaron-suppress-warning-popups-a (orig-fn type message &optional level buffer-name)
  "Redirect low-severity warnings to `aaron-suppressed-warnings-buffer'."
  (let ((level (or level :warning)))
    (cond
     ((aaron-warning-should-suppress-p type message level)
      ;; Log to the dedicated buffer instead of discarding or showing in *Messages*.
      (aaron-log-to-suppressed-warnings type message level))
     ((aaron-warning-level-at-least-p level aaron-warning-popup-minimum-level)
      (funcall orig-fn type message level buffer-name))
     (t
      (let ((warning-suppress-types
             (cons (aaron-warning-type-list type) warning-suppress-types)))
        (funcall orig-fn type message level buffer-name))))))

(unless (advice-member-p #'aaron-suppress-warning-popups-a 'display-warning)
  (advice-add 'display-warning :around #'aaron-suppress-warning-popups-a))

;; Native compilation warnings can surface before the main init loads.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-warning-on-missing-source nil)

;; Emacs 31 development builds can emit a lot of third-party package
;; compatibility warnings while loading or JIT-compiling ELPA packages.
;; Keep normal startup usable by only surfacing actual errors.
(setq warning-minimum-level :error
      warning-minimum-log-level :error)

;; Stop the byte-compiler from generating obsolete-API warnings in the first
;; place.  ELPA packages targeting pre-31 Emacs use when-let, if-let,
;; defadvice, search, count, etc. that became obsolete in Emacs 28–31.
;; '(not obsolete) means "all default warnings except the obsolete category".
(setq byte-compile-warnings '(not obsolete))


(defvar aaron-gui-undecorated t
  "Whether GUI frames should start without native window decorations.
Keep this disabled by default because some Emacs/macOS builds handle
undecorated frames poorly.")

(defvar aaron-gui-internal-border-width 0
  "Internal border width applied to GUI frames.")

(defvar aaron-gui-startup-fullscreen nil
  "Startup fullscreen state for GUI frames.
Use nil for a normal window, `maximized' for a maximized window, or
`fullboth' for a fullscreen window.")

;; Restore normal GC and file-name dispatch as soon as init has finished.
;;
;; `emacs-startup-hook' is not a reliable restoration boundary for daemon
;; startup: a daemon can begin serving buffers before that hook runs.  Restoring
;; by assignment is also unsafe because packages may have registered additional
;; handlers while init was loading.  Merge the saved entries instead, preserving
;; both the original TRAMP dispatcher and handlers installed during init.
(defun aaron-restore-startup-runtime-settings ()
  "Restore file-name handlers and the normal post-init GC policy.
This function is idempotent so `after-init-hook' and the startup fallback may
both call it."
  (dolist (handler aaron-original-file-name-handler-alist)
    (add-to-list 'file-name-handler-alist handler t))
  ;; Once `gcmh' starts, let it own GC policy. Otherwise, restore to a saner
  ;; interactive threshold than Emacs' tiny default.
  (unless (bound-and-true-p gcmh-mode)
    (setq gc-cons-threshold (max aaron-original-gc-cons-threshold
                                 (* 16 1024 1024))
          gc-cons-percentage aaron-original-gc-cons-percentage)))

(add-hook 'after-init-hook #'aaron-restore-startup-runtime-settings)
(add-hook 'emacs-startup-hook #'aaron-restore-startup-runtime-settings)


(let ((frame-params
       `((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (alpha . 96)
         ,@(when aaron-gui-undecorated
             '((undecorated . t)))
         ,@(when (integerp aaron-gui-internal-border-width)
             `((internal-border-width . ,aaron-gui-internal-border-width)))
         ,@(when aaron-gui-startup-fullscreen
             `((fullscreen . ,aaron-gui-startup-fullscreen))))))
  (setq default-frame-alist
        (append frame-params default-frame-alist)))



;; 对第一个 frame 也应用同样设置（有些版本对 initial-frame-alist 特判）
(setq initial-frame-alist default-frame-alist)

;; The initial GUI frame otherwise exposes *scratch* and partially initialized
;; tab/modelines while Dashboard is still being built.
(defvar aaron-gui-hide-initial-frame-during-startup t
  "Whether to reveal the first GUI frame only after startup content is ready.")

(defun aaron-reveal-startup-frames ()
  "Reveal GUI frames hidden by the early startup boundary.
This is idempotent and deliberately ignores daemon/emacsclient frames."
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'aaron-startup-hidden)
      (set-frame-parameter frame 'aaron-startup-hidden nil)
      (make-frame-visible frame)
      (redraw-frame frame))))

(defun aaron-gui-graphic-frame-exists-p ()
  "Return non-nil when a graphic frame already exists."
  (catch 'found
    (dolist (frame (frame-list))
      (when (display-graphic-p frame)
        (throw 'found t)))))

;; GNU Emacs has no graphic display during early init, so this pre-hide has
;; always been inert there.  Neomacs already owns a live graphic frame at this
;; point, where hiding through `initial-frame-alist' unmaps the real window and
;; the reveal in `emacs-startup-hook' races the render thread, leaving a window
;; that accepts no input.  Only pre-hide a frame that does not exist yet.
(when (and aaron-gui-hide-initial-frame-during-startup
           (not (daemonp))
           (display-graphic-p)
           (not (aaron-gui-graphic-frame-exists-p)))
  (setq initial-frame-alist
        (cons '(visibility . nil)
              (cons '(aaron-startup-hidden . t)
                    (assq-delete-all
                     'visibility
                     (assq-delete-all
                      'aaron-startup-hidden initial-frame-alist))))))

(add-hook 'emacs-startup-hook #'aaron-reveal-startup-frames 100)

;;; 额外的 UI 模式关闭（确保 TTY/GUI 都统一）
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; 放在 GUI 初始化之后
(when (display-graphic-p)
  (select-frame-set-input-focus (selected-frame)))

;; 让启动更“干净”：取消启动画面、startup message 等（可选）
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)



;;; early-init.el ends here
