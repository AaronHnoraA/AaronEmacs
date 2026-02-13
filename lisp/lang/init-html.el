;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; npm install -g vscode-langservers-extracted
;; sp-install-serverREThtml-lsRET

(use-package lsp-mode
  :hook ((html-mode . lsp))
  :commands lsp)


(use-package vue-html-mode
 :ensure t)




(provide 'init-html)

;;; init-bazel.el ends here
