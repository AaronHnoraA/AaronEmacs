;;; init-java.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `lsp-mode' for Java buffers.  `lsp-java' manages the JDT LS
;; integration, including Gradle project import.

;;; Code:

(declare-function my/register-lsp-mode-preference "init-lsp" (mode &optional feature source note))
(declare-function my/lsp-mode-ensure "init-lsp" ())

(when (fboundp 'my/register-lsp-mode-preference)
  (my/register-lsp-mode-preference 'java-mode 'lsp-java)
  (my/register-lsp-mode-preference 'java-ts-mode 'lsp-java))

(add-hook 'java-mode-hook #'my/lsp-mode-ensure)
(add-hook 'java-ts-mode-hook #'my/lsp-mode-ensure)

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :defer t)

(provide 'init-java)
;;; init-java.el ends here
