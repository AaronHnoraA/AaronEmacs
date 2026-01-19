;;; init-snippets.el --- yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:
;; 目标：
;; - Tab 不给 yasnippet 用（Tab 只做缩进 / company）
;; - 仅在 snippet 激活时：
;;     RET        -> 下一个 field
;;     S-<return> -> 上一个 field

;;; Code:

(use-package yasnippet
  :ensure t
  :init
  ;; 用 Emacs 配置根目录，避免写死 ~/.emacs.d
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  :hook
  (prog-mode . yas-minor-mode)
  :config
  ;; 全局启用
  (yas-global-mode 1)
  (yas-reload-all)

  ;; company: 给每个 backend 附加 company-yasnippet
  (defun company-mode/backend-with-yas (backend)
    "Append `company-yasnippet` to BACKEND unless already present."
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

(with-eval-after-load 'yasnippet
  ;; 只在 snippet 活跃时，yas-keymap 才会生效
  ;; 所以把键绑在 yas-keymap 上，就天然满足“仅仅 snippet 展开补全时”
  (define-key yas-keymap (kbd "M-]") #'yas-next-field)
  (define-key yas-keymap (kbd "M-[") #'yas-prev-field)

  ;; 如果你还想更保险：别让 yas 抢 TAB
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") nil))
)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
