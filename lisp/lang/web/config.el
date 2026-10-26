;;; lang/web/config.el --- HTML, CSS and Vue -*- lexical-binding: t -*-

;;; Commentary:
;; 前置依赖安装 (终端执行):
;; npm install -g vscode-langservers-extracted
;; 此包包含了 vscode-html-language-server
;; npm install -g @vue/language-server
;; Vue 的 language server；TypeScript 库取 target 上 `tsc' 所在的包

;;; Code:

(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function set-language-server! "tools/lsp/config")
(declare-function remote-expand-file-name "remote-fs" (file-name &optional directory target))
(declare-function remote-file-local-name "remote-fs" (file-name))

(require 'config)

;; 1. HTML LSP server 映射
;; lsp-mode 自带 html-mode / mhtml-mode 客户端；这里把 html-ts-mode 和
;; 自定义的 vue-html-mode 也绑定到同一个 HTML language server。
(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    (set-language-server!
     '(html-mode html-ts-mode mhtml-mode vue-html-mode)
     (lambda ()
       (list (or (+language-server-executable-find
                  "vscode-html-language-server")
                 "vscode-html-language-server")
             "--stdio"))
     :server-id '+html
     :priority 1
     :label "vscode-html-language-server"
     :executables '("vscode-html-language-server")
     :note "HTML and vue-html buffers share the HTML language server.")))

;; 2. vue-html-mode
(use-package vue-html-mode
  :ensure t
  :defer t)

;; 3. CSS
(config-register 'css-indent-offset
                 :type 'integer
                 :group '+web
                 :doc "Indentation of CSS, SCSS and the style block of a Vue file")

;; 4. Vue single-file components: web-mode, with Vue's language server.
(config-defvar +web-indent-offset nil
  "Indentation of the template, style and script blocks of a Vue file."
  :type 'integer
  :group '+web)

(defun +web-vue-buffer-p (&rest _)
  "Return non-nil when the current buffer visits a Vue file."
  (and buffer-file-name (string-suffix-p ".vue" buffer-file-name t)))

(defun +web-vue-setup-h ()
  "Indent a Vue file by `+web-indent-offset' with spaces.
Electric indent re-indents a line on `o' and `O' in web-mode's mixed blocks,
so it is off here."
  (when (+web-vue-buffer-p)
    (setq-local web-mode-markup-indent-offset +web-indent-offset
                web-mode-css-indent-offset +web-indent-offset
                web-mode-code-indent-offset +web-indent-offset
                indent-tabs-mode nil)
    (electric-indent-local-mode -1)))

(use-package web-mode
  :ensure t
  :mode "\\.vue\\'"
  :hook (web-mode . +web-vue-setup-h))

(defun +web-vue-initialization-options ()
  "Point Vue's language server at the target's TypeScript library.
`tsc' is found and followed on the workspace's target; the server gets the
target-native path."
  (when-let* ((tsc (+language-server-executable-find "tsc"))
              (tsc (file-truename (remote-expand-file-name tsc default-directory)))
              (lib (expand-file-name "../lib" (file-name-directory tsc)))
              ((file-directory-p lib)))
    `(:typescript (:tsdk ,(remote-file-local-name lib)))))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    (set-language-server!
     'web-mode
     (lambda ()
       (list (or (+language-server-executable-find "vue-language-server")
                 "vue-language-server")
             "--stdio"))
     :server-id '+vue
     :priority 1
     :activation-fn #'+web-vue-buffer-p
     :initialization-options #'+web-vue-initialization-options
     :label "vue-language-server"
     :executables '("vue-language-server")
     :note "Vue files; install with npm i -g @vue/language-server.")))

(provide 'lang/web/config)

;;; lang/web/config.el ends here
