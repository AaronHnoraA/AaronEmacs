;;; init-js2.el --- JavaScript mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package js2-mode
  :ensure t
  )

;; 修复了正则表达式中过多的转义字符，标准写法为 "\\.js\\'"
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ---------------------------------------------------------
;; 1. 设置 JavaScript 相关模式的专用缩进变量 (4个空格)
;; ---------------------------------------------------------
(setq js-indent-level 4)      ; 适用于内置 js-mode, js-jsx-mode
(setq js2-basic-offset 4)     ; 适用于 js2-mode

;; ---------------------------------------------------------
;; 2. 设置 TypeScript 相关模式的专用缩进变量 (4个空格)
;; ---------------------------------------------------------
(setq typescript-indent-level 4)            ; 适用于传统的 typescript-mode
(setq typescript-ts-mode-indent-offset 4)   ; 适用于 Emacs 29+ 内置的 typescript-ts-mode 和 tsx-ts-mode

;; ---------------------------------------------------------
;; 3. 通过 Hook 确保所有 JS/TS/LSP 相关的 Buffer 都是 4 个空格
;; ---------------------------------------------------------
(defun my-js-ts-indent-setup ()
  "统一设置 JS/TS Buffer 的缩进为 4 个空格."
  (setq indent-tabs-mode nil) ; 禁用 Tab 字符，使用空格
  (setq tab-width 4))         ; 设置 Tab 宽度为 4 (LSP 等工具会读取此变量)

;; 将上述设置挂载到各个 JS 模式的 Hook 上
(add-hook 'js-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js2-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js-jsx-mode-hook #'my-js-ts-indent-setup)

;; 将上述设置挂载到各个 TS 模式的 Hook 上
(add-hook 'typescript-mode-hook #'my-js-ts-indent-setup)
(add-hook 'typescript-ts-mode-hook #'my-js-ts-indent-setup)
(add-hook 'tsx-ts-mode-hook #'my-js-ts-indent-setup) ; 补充处理 TSX 文件的模式

(provide 'init-js2)
