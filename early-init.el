;;; early-init.el --- Early bird -*- lexical-binding: t; -*-

;;; 提前关掉包系统自动初始化，用 use-package 等自己控制
(setq package-enable-at-startup nil)

;; Prefer fresh source over stale bytecode throughout the startup chain.
(setq load-prefer-newer t)

(defconst my/startup-byte-compiled-files
  '("early-init.el" "init.el" "bootstrap.el")
  "Top-level startup files whose stale bytecode can break bootstrap.")

(defun my/delete-stale-startup-bytecode ()
  "Delete stale top-level `.elc' files before normal init loading continues."
  (dolist (file my/startup-byte-compiled-files)
    (let* ((source (expand-file-name file user-emacs-directory))
           (bytecode (concat source "c")))
      (when (and (file-exists-p source)
                 (file-exists-p bytecode)
                 (file-newer-than-file-p source bytecode))
        (condition-case err
            (delete-file bytecode)
          (error
           (display-warning
            'early-init
            (format "Failed to delete stale startup bytecode %s: %s"
                    bytecode
                    (error-message-string err))
            :error)))))))

(my/delete-stale-startup-bytecode)

;; Keep native compilation cache local to this config so cleanup stays simple.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

;;; 启动性能优化：暂时放宽 GC、禁用 file-name-handler
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(defvar my/original-gc-cons-percentage gc-cons-percentage)
(defvar my/original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      file-name-handler-alist nil)

;; Silence non-critical warning popups as early as possible so startup noise
;; stays in `*Warnings*` instead of stealing focus.
(defvar my/warning-popup-minimum-level :error
  "Minimum warning severity that may auto-display the warnings buffer.")

(defvar my/warning-suppress-elpa-noise t
  "Whether to suppress low-severity startup noise from third-party ELPA packages.")

(defconst my/suppressed-warnings-buffer "*Suppressed Warnings*"
  "Buffer that collects low-severity warnings redirected away from *Messages*.
Browse with `my/show-suppressed-warnings'.")

(defun my/log-to-suppressed-warnings (type message level)
  "Append a suppressed warning to `my/suppressed-warnings-buffer'."
  (with-current-buffer (get-buffer-create my/suppressed-warnings-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] (%s) %s: %s\n"
                      (format-time-string "%T")
                      (if (consp type)
                          (mapconcat #'symbol-name type ".")
                        (symbol-name type))
                      (upcase (substring (symbol-name level) 1))
                      message)))))

(defun my/show-suppressed-warnings ()
  "Pop up the buffer of warnings that were silenced during startup."
  (interactive)
  (if-let* ((buf (get-buffer my/suppressed-warnings-buffer)))
      (pop-to-buffer buf)
    (message "No suppressed warnings recorded.")))

(defun my/warning-level-rank (level)
  "Return a comparable numeric rank for warning LEVEL."
  (pcase level
    (:debug 0)
    (:warning 1)
    (:error 2)
    (:emergency 3)
    (_ 1)))

(defun my/warning-level-at-least-p (level threshold)
  "Return non-nil when warning LEVEL is at least THRESHOLD."
  (>= (my/warning-level-rank level)
      (my/warning-level-rank threshold)))

(defun my/warning-type-list (type)
  "Normalize warning TYPE into the list form used by `warning-suppress-types'."
  (if (consp type) type (list type)))

(defun my/warning-should-suppress-p (type message level)
  "Return non-nil when TYPE/MESSAGE/LEVEL should be redirected to the warning log."
  (let ((level (or level :warning))
        (type-list (my/warning-type-list type)))
    (and my/warning-suppress-elpa-noise
         (not (my/warning-level-at-least-p level :error))
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

(defun my/suppress-warning-popups-a (orig-fn type message &optional level buffer-name)
  "Redirect low-severity warnings to `my/suppressed-warnings-buffer'."
  (let ((level (or level :warning)))
    (cond
     ((my/warning-should-suppress-p type message level)
      ;; Log to the dedicated buffer instead of discarding or showing in *Messages*.
      (my/log-to-suppressed-warnings type message level))
     ((my/warning-level-at-least-p level my/warning-popup-minimum-level)
      (funcall orig-fn type message level buffer-name))
     (t
      (let ((warning-suppress-types
             (cons (my/warning-type-list type) warning-suppress-types)))
        (funcall orig-fn type message level buffer-name))))))

(unless (advice-member-p #'my/suppress-warning-popups-a 'display-warning)
  (advice-add 'display-warning :around #'my/suppress-warning-popups-a))

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


(defvar my/gui-undecorated t
  "Whether GUI frames should start without native window decorations.
Keep this disabled by default because some Emacs/macOS builds handle
undecorated frames poorly.")

(defvar my/gui-internal-border-width 0
  "Internal border width applied to GUI frames.")

(defvar my/gui-startup-fullscreen nil
  "Startup fullscreen state for GUI frames.
Use nil for a normal window, `maximized' for a maximized window, or
`fullboth' for a fullscreen window.")

;; Emacs 启动完成后恢复正常 GC 和 file-name-handler 设置
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/original-file-name-handler-alist)
            ;; Once `gcmh' starts, let it own GC policy. Otherwise, restore to
            ;; a saner interactive threshold than Emacs' tiny default.
            (unless (bound-and-true-p gcmh-mode)
              (setq gc-cons-threshold (max my/original-gc-cons-threshold
                                           (* 16 1024 1024))
                    gc-cons-percentage my/original-gc-cons-percentage))))


(let ((frame-params
       `((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         ,@(when my/gui-undecorated
             '((undecorated . t)))
         ,@(when (integerp my/gui-internal-border-width)
             `((internal-border-width . ,my/gui-internal-border-width)))
         ,@(when my/gui-startup-fullscreen
             `((fullscreen . ,my/gui-startup-fullscreen))))))
  (setq default-frame-alist
        (append frame-params default-frame-alist)))



;; 对第一个 frame 也应用同样设置（有些版本对 initial-frame-alist 特判）
(setq initial-frame-alist default-frame-alist)

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

(add-to-list 'default-frame-alist '(alpha-background . 97))
(set-frame-parameter nil 'alpha-background 97)
(add-to-list 'default-frame-alist '(alpha . 97))

;;; early-init.el ends here
