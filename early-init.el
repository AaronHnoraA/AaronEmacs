;;; early-init.el --- Early bird -*- lexical-binding: t; -*-

;;; 提前关掉包系统自动初始化，用 use-package 等自己控制
(setq package-enable-at-startup nil)

;;; 启动性能优化：暂时放宽 GC、禁用 file-name-handler
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(defvar my/original-gc-cons-percentage gc-cons-percentage)
(defvar my/original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Emacs 启动完成后恢复正常 GC 和 file-name-handler 设置
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/original-gc-cons-threshold
                  gc-cons-percentage my/original-gc-cons-percentage
                  file-name-handler-alist my/original-file-name-handler-alist)))


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
