;;; lang/javascript/config.el --- JavaScript mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'subr-x)

(config-defvar +js-indent-offset nil
  "Indentation width used by JavaScript and TypeScript modes."
  :type 'integer
  :group 'languages)

(declare-function +language-server-executable-available-p "tools/lsp/config" (program))
(declare-function +language-server-set-workspace-configuration
                  "tools/lsp/config" (configuration))
(declare-function set-language-server! "tools/lsp/config")
(declare-function remote-environment-apply "remote-environment" (environment &optional buffer))
(declare-function remote-environment-derive "remote-environment" (environment id &rest keys))
(declare-function remote-environment-ensure "remote-environment" (&optional context force callback))
(declare-function remote-file-local-name "remote-fs" (file-name))

(defun +js-ts-mode-available-p ()
  "Return non-nil when `js-ts-mode' can be used safely."
  (and (fboundp 'js-ts-mode)
       (fboundp 'treesit-ready-p)
       (ignore-errors
         (and (treesit-ready-p 'javascript t)
              (treesit-ready-p 'jsdoc t)))))

(defun +js-auto-mode ()
  "Use tree-sitter JavaScript mode when ready, otherwise fall back to `js2-mode'."
  (interactive)
  (if (+js-ts-mode-available-p)
      (js-ts-mode)
    (js2-mode)))

(defun +js-project-node-bin-directory ()
  "Return the nearest project-local node_modules/.bin directory, if present."
  (when-let* ((root (locate-dominating-file
                    default-directory
                    "node_modules/.bin/typescript-language-server"))
              (bin-dir (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin-dir)
      bin-dir)))

(defun +js-setup-project-node-bin ()
  "Add the target project's Node executables through its environment capsule."
  (when-let* ((bin-dir (+js-project-node-bin-directory)))
    (when-let* ((environment (remote-environment-ensure))
                (target-bin (remote-file-local-name bin-dir)))
      (remote-environment-apply
       (remote-environment-derive
        environment "node-project-bin"
        :scope 'toolchain
        :path-prepend (list target-bin)
        :source 'node-modules)))))

(defun +js-ts-language-server-available-p ()
  "Return non-nil when the TypeScript language server is available."
  (+language-server-executable-available-p "typescript-language-server"))

(defun +js-ts-language-server-workspace-configuration ()
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

(defun +js-ts-language-server-setup-h ()
  "Prepare JS/TS buffers for the language server."
  (+js-setup-project-node-bin)
  (when (+js-ts-language-server-available-p)
    (+language-server-set-workspace-configuration
     (+js-ts-language-server-workspace-configuration))))

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
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . +js-auto-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . +js-auto-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . +js-auto-mode))

;; ---------------------------------------------------------
;; 3. 通过 Hook 确保所有 JS/TS/LSP 相关的 Buffer 都是 2 个空格
;; ---------------------------------------------------------
(defun +js-set-local-variable (variable value)
  "Set VARIABLE buffer-locally to VALUE, following obsolete aliases."
  (when-let* ((target (or (car-safe (get variable 'byte-obsolete-variable))
                          variable)))
    (when (boundp target)
      (set (make-local-variable target) value))))

(defun +javascript-indent-setup-h ()
  "统一设置 JS/TS buffer 的缩进."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width +js-indent-offset)
  (+js-set-local-variable 'js-indent-level +js-indent-offset)
  (+js-set-local-variable 'js2-basic-offset +js-indent-offset)
  (+js-set-local-variable 'typescript-indent-level +js-indent-offset)
  (+js-set-local-variable 'typescript-ts-indent-offset +js-indent-offset)
  (+js-set-local-variable 'typescript-ts-mode-indent-offset +js-indent-offset)
  (+js-set-local-variable 'evil-shift-width +js-indent-offset))

;; 将上述设置挂载到各个 JS 模式的 Hook 上
(add-hook 'js-mode-hook #'+javascript-indent-setup-h)
(add-hook 'js2-mode-hook #'+javascript-indent-setup-h)
(add-hook 'js-jsx-mode-hook #'+javascript-indent-setup-h)
(add-hook 'js-ts-mode-hook #'+javascript-indent-setup-h)
(add-hook 'js-base-mode-hook #'+javascript-indent-setup-h)

;; 将上述设置挂载到各个 TS 模式的 Hook 上
(add-hook 'typescript-mode-hook #'+javascript-indent-setup-h)
(add-hook 'typescript-ts-mode-hook #'+javascript-indent-setup-h)
(add-hook 'tsx-ts-mode-hook #'+javascript-indent-setup-h) ; 补充处理 TSX 文件的模式

(dolist (hook '(js-mode-hook js2-mode-hook js-ts-mode-hook js-jsx-mode-hook
                typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'+js-ts-language-server-setup-h))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    (set-language-server!
     '(js-mode js2-mode js-ts-mode js-jsx-mode
       typescript-mode typescript-ts-mode tsx-ts-mode)
     (lambda ()
       (list (or (+language-server-executable-find
                  "typescript-language-server")
                 "typescript-language-server")
             "--stdio"))
     :server-id '+typescript
     :priority 1
     :label "typescript-language-server"
     :executables '("typescript-language-server")
     :note "JS/TS buffers share the TypeScript language server.")))

(provide 'lang/javascript/config)
