;;; init-snippets.el --- yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:
;; - 所有 prog-mode 自动启用 yas-minor-mode
;; - TAB 不给 yas 用（缩进 / company 用）
;; - C-c y 显式展开 snippet
;; - C-c s 打开 snippet 菜单

;;; Code:

(use-package yasnippet
  :ensure t
  :init
  ;; 你自己的 snippets 目录：~/.emacs.d/snippets
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)
  (yas-reload-all)

  (with-eval-after-load 'yasnippet
    ;; snippet 会话中的 field 跳转
    (define-key yas-keymap (kbd "M-]") #'yas-next-field)
    (define-key yas-keymap (kbd "M-[") #'yas-prev-field)
    ;; 不让 yas 抢 TAB
    (define-key yas-keymap (kbd "TAB") nil)
    (define-key yas-keymap (kbd "<tab>") nil)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  ;; 把包自带 snippets 也加入
  (add-to-list 'yas-snippet-dirs
               (expand-file-name
                "snippets"
                (file-name-directory
                 (locate-library "yasnippet-snippets"))))
  (yas-reload-all))

;; 全局手动展开 / 插入 snippet
(global-set-key (kbd "C-c y") #'yas-expand)
(global-set-key (kbd "C-c s") #'yas-insert-snippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
