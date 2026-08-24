;;; init-js2.el --- JavaScript mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'subr-x)

(config-defvar my/js-indent-offset nil
  "Indentation width used by JavaScript and TypeScript modes."
  :type 'integer
  :group 'languages)

(declare-function my/language-server-executable-available-p "init-lsp" (program))
(declare-function my/language-server-set-workspace-configuration
                  "init-lsp" (configuration))
(declare-function my/register-language-server "init-lsp")
(declare-function remote-environment-apply "remote-environment" (environment &optional buffer))
(declare-function remote-environment-derive "remote-environment" (environment id &rest keys))
(declare-function remote-environment-ensure "remote-environment" (&optional context force callback))
(declare-function remote-file-local-name "remote-fs" (file-name))

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

(defun my/js-project-node-bin-directory ()
  "Return the nearest project-local node_modules/.bin directory, if present."
  (when-let* ((root (locate-dominating-file
                    default-directory
                    "node_modules/.bin/typescript-language-server"))
              (bin-dir (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin-dir)
      bin-dir)))

(defun my/js-setup-project-node-bin ()
  "Add the target project's Node executables through its environment capsule."
  (when-let* ((bin-dir (my/js-project-node-bin-directory)))
    (when-let* ((environment (remote-environment-ensure))
                (target-bin (remote-file-local-name bin-dir)))
      (remote-environment-apply
       (remote-environment-derive
        environment "node-project-bin"
        :scope 'toolchain
        :path-prepend (list target-bin)
        :source 'node-modules)))))

(defun my/js-ts-language-server-available-p ()
  "Return non-nil when the TypeScript language server is available."
  (my/language-server-executable-available-p "typescript-language-server"))

(defun my/js-ts-language-server-workspace-configuration ()
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

(defun my/js-ts-language-server-setup-h ()
  "Prepare JS/TS buffers for the language server."
  (my/js-setup-project-node-bin)
  (when (my/js-ts-language-server-available-p)
    (my/language-server-set-workspace-configuration
     (my/js-ts-language-server-workspace-configuration))))

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
;; 3. 通过 Hook 确保所有 JS/TS/LSP 相关的 Buffer 都是 2 个空格
;; ---------------------------------------------------------
(defun my/js-set-local-variable (variable value)
  "Set VARIABLE buffer-locally to VALUE, following obsolete aliases."
  (when-let* ((target (or (car-safe (get variable 'byte-obsolete-variable))
                          variable)))
    (when (boundp target)
      (set (make-local-variable target) value))))

(defun my-js-ts-indent-setup ()
  "统一设置 JS/TS buffer 的缩进."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width my/js-indent-offset)
  (my/js-set-local-variable 'js-indent-level my/js-indent-offset)
  (my/js-set-local-variable 'js2-basic-offset my/js-indent-offset)
  (my/js-set-local-variable 'typescript-indent-level my/js-indent-offset)
  (my/js-set-local-variable 'typescript-ts-indent-offset my/js-indent-offset)
  (my/js-set-local-variable 'typescript-ts-mode-indent-offset my/js-indent-offset)
  (my/js-set-local-variable 'evil-shift-width my/js-indent-offset))

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

(dolist (hook '(js-mode-hook js2-mode-hook js-ts-mode-hook js-jsx-mode-hook
                typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'my/js-ts-language-server-setup-h))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'my/register-language-server)
    (my/register-language-server
     '(js-mode js2-mode js-ts-mode js-jsx-mode
       typescript-mode typescript-ts-mode tsx-ts-mode)
     (lambda ()
       (list (or (my/language-server-executable-find
                  "typescript-language-server")
                 "typescript-language-server")
             "--stdio"))
     :server-id 'my-typescript
     :priority 1
     :label "typescript-language-server"
     :executables '("typescript-language-server")
     :note "JS/TS buffers share the TypeScript language server.")))

(provide 'init-js2)
