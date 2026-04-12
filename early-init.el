;;; early-init.el --- Early bird -*- lexical-binding: t; -*-

;;; 提前关掉包系统自动初始化，用 use-package 等自己控制
(setq package-enable-at-startup nil)

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

(defun my/suppress-warning-popups-a (orig-fn type message &optional level buffer-name)
  "Keep lower-severity warnings in the log without auto-popping a window."
  (let ((level (or level :warning)))
    (if (my/warning-level-at-least-p level my/warning-popup-minimum-level)
        (funcall orig-fn type message level buffer-name)
      (let ((warning-suppress-types
             (cons (my/warning-type-list type) warning-suppress-types)))
        (funcall orig-fn type message level buffer-name)))))

(unless (advice-member-p #'my/suppress-warning-popups-a 'display-warning)
  (advice-add 'display-warning :around #'my/suppress-warning-popups-a))

;; Native compilation warnings can surface before the main init loads.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-warning-on-missing-source nil)

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


(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (undecorated . t)
         (internal-border-width . 0)

         ;; 启动时直接进入全屏。
         ;; 如需改成“最大化但不进原生全屏”，可改为 `maximized`。
         ;(fullscreen . fullboth)
         ;(fullscreen . maximized)
        )
       default-frame-alist))



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

;;; early-init.el ends here
