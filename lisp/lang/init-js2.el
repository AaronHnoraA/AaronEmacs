;;; init-js2.el --- JavaScript mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/language-server-executable-available-p "init-lsp" (program))
(declare-function my/eglot-set-workspace-configuration "init-lsp" (configuration))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(defun my/js-ts-mode-available-p ()
  "Return non-nil when `js-ts-mode' can be used safely."
  (and (fboundp 'js-ts-mode)
       (fboundp 'treesit-ready-p)
       (ignore-errors
         (and (treesit-ready-p 'javascript t)
              (treesit-ready-p 'jsdoc t)))))

(defun my/js-auto-mode ()
  "Use tree-sitter JavaScript mode when ready, otherwise fall back to `js2-mode'."
  (interactive)
  (if (my/js-ts-mode-available-p)
      (js-ts-mode)
    (js2-mode)))

(defun my/js-local-node-bin-directory ()
  "Return the nearest project-local node_modules/.bin directory, if present."
  (when-let* ((root (locate-dominating-file
                    default-directory
                    "node_modules/.bin/typescript-language-server"))
              (bin-dir (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin-dir)
      bin-dir)))

(defun my/js-setup-local-node-bin ()
  "Make project-local Node executables visible to JS/TS tooling."
  (when-let* ((bin-dir (my/js-local-node-bin-directory)))
    (setq-local exec-path (cons bin-dir (remove bin-dir exec-path)))
    (let* ((path-separator (if (eq system-type 'windows-nt) ";" ":"))
           (path (or (getenv "PATH") ""))
           (process-environment (copy-sequence process-environment)))
      (unless (member bin-dir (split-string path path-separator t))
        (setenv "PATH" (if (string-empty-p path)
                           bin-dir
                         (concat bin-dir path-separator path)))
        (setq-local process-environment process-environment)))))

(defun my/js-ts-eglot-available-p ()
  "Return non-nil when the TypeScript language server is available."
  (my/language-server-executable-available-p "typescript-language-server"))

(defun my/js-ts-eglot-workspace-configuration ()
  "Return shared JS/TS workspace configuration."
  '(:typescript (:inlayHints (:includeInlayParameterNameHints "literals"
                 :includeInlayParameterNameHintsWhenArgumentMatchesName t
                 :includeInlayFunctionParameterTypeHints t
                 :includeInlayVariableTypeHints nil
                 :includeInlayPropertyDeclarationTypeHints t
                 :includeInlayFunctionLikeReturnTypeHints t
                 :includeInlayEnumMemberValueHints t))
    :javascript (:inlayHints (:includeInlayParameterNameHints "literals"
                 :includeInlayParameterNameHintsWhenArgumentMatchesName t
                 :includeInlayFunctionParameterTypeHints t
                 :includeInlayVariableTypeHints nil
                 :includeInlayPropertyDeclarationTypeHints t
                 :includeInlayFunctionLikeReturnTypeHints t
                 :includeInlayEnumMemberValueHints t))))

(defun my/js-ts-eglot-ensure ()
  "Start Eglot for JS/TS buffers when the server is available."
  (my/js-setup-local-node-bin)
  (when (my/js-ts-eglot-available-p)
    (my/eglot-set-workspace-configuration
     (my/js-ts-eglot-workspace-configuration))
    (my/eglot-ensure-unless-lsp-mode)))

(use-package js2-mode
  :ensure t
  )

(setq auto-mode-alist
      (cl-remove-if
       (lambda (entry)
         (member entry '(("\\.js\\'" . js2-mode)
                         ("\\.js\\'" . js-mode)
                         ("\\.js\\'" . js-ts-mode))))
       auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . my/js-auto-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . my/js-auto-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . my/js-auto-mode))

;; ---------------------------------------------------------
;; 1. 设置 JavaScript 相关模式的专用缩进变量 (2 个空格)
;; ---------------------------------------------------------
(setq js-indent-level 2)      ; 适用于内置 js-mode, js-jsx-mode
(setq js2-basic-offset 2)     ; 适用于 js2-mode

;; ---------------------------------------------------------
;; 2. 设置 TypeScript 相关模式的专用缩进变量 (2 个空格)
;; ---------------------------------------------------------
(setq typescript-indent-level 2)            ; 适用于传统的 typescript-mode
(when (boundp 'typescript-ts-indent-offset)
  (setq typescript-ts-indent-offset 2))     ; 适用于 Emacs 29+ 内置的 typescript-ts-mode 和 tsx-ts-mode

;; ---------------------------------------------------------
;; 3. 通过 Hook 确保所有 JS/TS/LSP 相关的 Buffer 都是 2 个空格
;; ---------------------------------------------------------
(defun my/js-set-local-variable (variable value)
  "Set VARIABLE buffer-locally to VALUE, following obsolete aliases."
  (when-let* ((target (or (car-safe (get variable 'byte-obsolete-variable))
                          variable)))
    (when (boundp target)
      (set (make-local-variable target) value))))

(defun my-js-ts-indent-setup ()
  "统一设置 JS/TS buffer 的缩进为 2 个空格."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (my/js-set-local-variable 'js-indent-level 2)
  (my/js-set-local-variable 'js2-basic-offset 2)
  (my/js-set-local-variable 'typescript-indent-level 2)
  (my/js-set-local-variable 'typescript-ts-indent-offset 2)
  (my/js-set-local-variable 'typescript-ts-mode-indent-offset 2)
  (my/js-set-local-variable 'evil-shift-width 2))

;; 将上述设置挂载到各个 JS 模式的 Hook 上
(add-hook 'js-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js2-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js-jsx-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js-ts-mode-hook #'my-js-ts-indent-setup)
(add-hook 'js-base-mode-hook #'my-js-ts-indent-setup)

;; 将上述设置挂载到各个 TS 模式的 Hook 上
(add-hook 'typescript-mode-hook #'my-js-ts-indent-setup)
(add-hook 'typescript-ts-mode-hook #'my-js-ts-indent-setup)
(add-hook 'tsx-ts-mode-hook #'my-js-ts-indent-setup) ; 补充处理 TSX 文件的模式

(use-package eglot
  :ensure nil
  :hook ((js-mode . my/js-ts-eglot-ensure)
         (js2-mode . my/js-ts-eglot-ensure)
         (js-ts-mode . my/js-ts-eglot-ensure)
         (js-jsx-mode . my/js-ts-eglot-ensure)
         (typescript-mode . my/js-ts-eglot-ensure)
         (typescript-ts-mode . my/js-ts-eglot-ensure)
         (tsx-ts-mode . my/js-ts-eglot-ensure)))

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(js-mode js2-mode js-ts-mode js-jsx-mode
       typescript-mode typescript-ts-mode tsx-ts-mode)
     '("typescript-language-server" "--stdio")
     :label "typescript-language-server"
     :executables '("typescript-language-server")
     :note "JS/TS buffers share the TypeScript language server through Eglot.")))

(provide 'init-js2)
