;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

;; Emacs Lisp 不使用 lsp-mode：避免弹 “no matched clients”
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (boundp 'lsp-mode)
              (lsp-mode -1))))

;; 文档即时显示（多行）
(setq eldoc-echo-area-use-multiline-p t)

;; 原生补全体验：按 TAB/循环更顺
(setq completion-cycle-threshold 3)

;; Emacs Lisp buffer 里 company 只用 capf（只对当前 buffer 生效）
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends '(company-capf))))

;; 使用 Flymake（Emacs 自带检查；只对 elisp buffer）
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(with-eval-after-load 'lsp-mode
  (defun my/lsp--skip-in-elisp (orig-fn &rest args)
    "Do not run lsp in Emacs Lisp buffers."
    (if (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
        (progn
          ;; 确保万一已经开启也立刻关掉
          (when (bound-and-true-p lsp-mode)
            (lsp-mode -1))
          ;; 不再继续执行 lsp/lsp-deferred
          nil)
      (apply orig-fn args)))

  (advice-add 'lsp :around #'my/lsp--skip-in-elisp)
  (advice-add 'lsp-deferred :around #'my/lsp--skip-in-elisp))
(provide 'init-elisp)

;;; init-elisp.el ends here
