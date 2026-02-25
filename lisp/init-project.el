;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))


(use-package treemacs
  :ensure t
  :bind
  (("C-c t" . treemacs)
   ("M-0"   . treemacs-select-window))
  
  :config
  ;; ==========================================
  ;; 1. 窗口位置：底部弹出 (占据 35% 高度)
  ;; ==========================================
  (setq treemacs-display-in-side-window nil)
  (add-to-list 'display-buffer-alist
               '("\\` \\*Treemacs-.*"
                 (display-buffer-at-bottom)
                 (window-height . 0.35)
                 (window-parameters . ((no-other-window . t)))))

  ;; ==========================================
  ;; 2. 焦点切换自动隐藏
  ;; ==========================================
  (defun my-treemacs-auto-hide (&optional _)
    (let ((tree-win (treemacs-get-local-window)))
      (when (and tree-win
                 (window-live-p tree-win)
                 (not (eq (selected-window) tree-win))
                 (not (window-minibuffer-p (selected-window))))
        (ignore-errors (delete-window tree-win)))))
  (add-hook 'window-selection-change-functions #'my-treemacs-auto-hide)

  ;; ==========================================
  ;; 3. 性能优化与 Imenu 基础设置
  ;; ==========================================
  (setq treemacs-deferred-git-apply-delay 0.5
        treemacs-max-git-entries 3000
        treemacs-file-event-delay 1500
        treemacs-show-cursor t
        treemacs-width-is-initially-locked nil
        ;; 【关键】开启内置 Imenu 的自动刷新，防止按 TAB 展开后看到的是旧的代码结构
        imenu-auto-rescan t) 

  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

;; ==========================================
;; 4. Magit 联动 (仅保留 Git 状态实时刷新)
;; ==========================================
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))





(use-package perspective
  :ensure t
  :bind
  ;; 可选：让 C-x k 在当前工作区里“更符合直觉”地关 buffer
  (("C-x k" . persp-kill-buffer*))
  :init
  ;; Emacs 28+ 必须显式设置前缀键，否则会有警告
  ;; 官方默认是 C-x x；你也可以改成 (kbd "C-c w") 等
  (setq persp-mode-prefix-key (kbd "C-x x"))

  ;; 默认工作区名字（可选）
  (setq persp-initial-frame-name "main")

  ;; 如果你想压掉“没设置 prefix key”的警告，也可以用这行（但你已设置了，就不需要）
  ;; (setq persp-suppress-no-prefix-key-warning t)

  :config
  ;; 启用
  (persp-mode 1))




(provide 'init-project)



;;; init-base.el ends here
