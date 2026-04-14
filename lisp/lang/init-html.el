;;; init-html.el --- HTML LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; 前置依赖安装 (终端执行):
;; npm install -g vscode-langservers-extracted
;; 此包包含了 vscode-html-language-server

;;; Code:

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

;; 1. 配置 Eglot 及 LSP Server 映射
(use-package eglot
  :ensure t
  :defer t)

(with-eval-after-load 'eglot
  ;; Eglot 默认已支持 html-mode 和 mhtml-mode
  ;; 此处通过 eglot-server-programs 将自定义的 vue-html-mode 绑定到 HTML LSP 服务
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     'html-ts-mode
     '("vscode-html-language-server" "--stdio")
     :label "vscode-html-language-server"
     :executables '("vscode-html-language-server")
     :note "html-ts-mode uses the HTML language server through Eglot.")
    (my/register-eglot-server-program
     'vue-html-mode
     '("vscode-html-language-server" "--stdio")
     :label "vscode-html-language-server"
     :executables '("vscode-html-language-server")
     :note "vue-html-mode shares the HTML language server through Eglot.")))

;; 2. 配置 vue-html-mode 并设置自动启动 Eglot
(use-package vue-html-mode
  :ensure t
  :hook (vue-html-mode . my/eglot-ensure-unless-lsp-mode))

;; 3. (推荐) 为 Emacs 内置的 HTML 模式也自动开启 Eglot
(use-package sgml-mode
  :defer t
  :hook ((html-mode . my/eglot-ensure-unless-lsp-mode)
         (html-ts-mode . my/eglot-ensure-unless-lsp-mode)
         (mhtml-mode . my/eglot-ensure-unless-lsp-mode)))

(provide 'init-html)

;;; init-html.el ends here
