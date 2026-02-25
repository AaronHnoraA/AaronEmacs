(use-package js2-mode
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ---------------------------------------------------------
;; 1. 设置 JavaScript 相关模式的专用缩进变量
;; ---------------------------------------------------------
(setq js-indent-level 4)      ; 适用于内置 js-mode, js-jsx-mode
(setq js2-basic-offset 4)     ; 适用于 js2-mode

;; ---------------------------------------------------------
;; 2. 通过 Hook 确保所有 JS/LSP 相关的 Buffer 都是 2 个空格
;; ---------------------------------------------------------
(defun my-js-indent-setup ()
  "统一设置 JS Buffer 的缩进为 2 个空格."
  (setq indent-tabs-mode nil) ; 禁用 Tab 字符，使用空格
  (setq tab-width 4))         ; 设置 Tab 宽度为 2 (LSP 会读取此变量)

;; 将上述设置挂载到各个 JS 模式的 Hook 上
(add-hook 'js-mode-hook #'my-js-indent-setup)
(add-hook 'js2-mode-hook #'my-js-indent-setup)
(add-hook 'js-jsx-mode-hook #'my-js-indent-setup)

;; 如果你还使用了 typescript-mode 或 web-mode，也可以顺手加上：
(setq typescript-indent-level 4)
(add-hook 'typescript-mode-hook #'my-js-indent-setup)


(provide 'init-js2)
