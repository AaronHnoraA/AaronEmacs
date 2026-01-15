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
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))


;;; init-perspective.el --- Workspaces (perspective.el) -*- lexical-binding: t; -*-

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
