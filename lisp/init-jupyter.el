;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; ----------------------------
;; 安装 emacs-jupyter
;; ----------------------------

(use-package jupyter
  :ensure t
  :config
  ;; 如果需要 org-babel 支持，下面会启用
  (setq jupyter-log-buffer-name "*jupyter-log*")
  ;; 如果你想让 REPL 自动关联到当前 buffer
  (setq jupyter-repl-interaction-mode-enable-prompt-overlay t)
  )

;; ----------------------------
;; Org-mode + emacs-jupyter 设置
;; ----------------------------


;; ----------------------------
;; 快捷键（可选）
;; ----------------------------

;; 常用 jupyter-repl 相关
(global-set-key (kbd "C-c j r") 'jupyter-run-repl)         ;; 启动 REPL
(global-set-key (kbd "C-c j c") 'jupyter-connect-repl)     ;; 连接已有 kernel
(global-set-key (kbd "C-c j i") 'jupyter-inspect-at-point) ;; inspect
(global-set-key (kbd "C-c j e") 'jupyter-eval-line-or-region)

;; ----------------------------
;; 内核 session 提示在状态栏
;; ----------------------------
(setq jupyter-repl-buffer-name-template "*jupyter-repl[%s]*")





(use-package code-cells
  :ensure t)





(provide 'init-jupyter)



;;; init-base.el ends here
