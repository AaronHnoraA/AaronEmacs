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

;;; UI 精简与无边框设置
;; 这些放在 early-init 里，让所有新 frame 默认都极简 UI
(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)        ; 不显示菜单栏
         (tool-bar-lines . 0)        ; 不显示工具栏
         (vertical-scroll-bars . nil); 不显示竖直滚动条
         (horizontal-scroll-bars . nil) ; 不显示水平滚动条
         (undecorated . t)           ; 无窗口边框/标题栏（支持的平台）
         (internal-border-width . 0) ; 去掉内部边框
        )
       default-frame-alist))

;; 对第一个 frame 也应用同样设置（有些版本对 initial-frame-alist 特判）
(setq initial-frame-alist default-frame-alist)

;;; 额外的 UI 模式关闭（确保 TTY/GUI 都统一）
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 让启动更“干净”：取消启动画面、startup message 等（可选）
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;;; early-init.el ends here
