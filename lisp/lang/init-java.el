;;; init-java.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:
;; Keep Java support minimal. Prefer `lsp-mode' for Java buffers and load
;; `lsp-java' only when it is already installed locally.

;;; Code:

(declare-function my/register-lsp-mode-preference "init-lsp" (mode &optional feature source note))
(declare-function my/lsp-mode-ensure "init-lsp" ())

(when (fboundp 'my/register-lsp-mode-preference)
  (my/register-lsp-mode-preference 'java-mode 'lsp-java)
  (my/register-lsp-mode-preference 'java-ts-mode 'lsp-java))

(add-hook 'java-mode-hook #'my/lsp-mode-ensure)
(add-hook 'java-ts-mode-hook #'my/lsp-mode-ensure)

(when (locate-library "lsp-java")
  (use-package lsp-java
    :after lsp-mode
    :defer t))

(provide 'init-java)
;;; init-java.el ends here
