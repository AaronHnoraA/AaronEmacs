;;; init-html.el --- HTML LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; 前置依赖安装 (终端执行):
;; npm install -g vscode-langservers-extracted
;; 此包包含了 vscode-html-language-server

;;; Code:

(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/register-language-server "init-lsp")

;; 1. HTML LSP server 映射
;; lsp-mode 自带 html-mode / mhtml-mode 客户端；这里把 html-ts-mode 和
;; 自定义的 vue-html-mode 也绑定到同一个 HTML language server。
(with-eval-after-load 'lsp-mode
  (when (fboundp 'my/register-language-server)
    (my/register-language-server
     '(html-mode html-ts-mode mhtml-mode vue-html-mode)
     (lambda ()
       (list (or (my/language-server-executable-find
                  "vscode-html-language-server")
                 "vscode-html-language-server")
             "--stdio"))
     :server-id 'my-html
     :priority 1
     :label "vscode-html-language-server"
     :executables '("vscode-html-language-server")
     :note "HTML and vue-html buffers share the HTML language server.")))

;; 2. vue-html-mode
(use-package vue-html-mode
  :ensure t
  :defer t)

(provide 'init-html)

;;; init-html.el ends here
