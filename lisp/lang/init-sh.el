;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/language-server-executable-available-p "init-lsp" (program))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(defun my/sh-eglot-ensure ()
  "Start Eglot for shell buffers when bash-language-server is available."
  (when (my/language-server-executable-available-p "bash-language-server")
    (my/eglot-ensure-unless-lsp-mode)))

;; Edit shell scripts
;;
;; sh-mode provides `sh-while-getopts' to automate getopts.
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'"     . sh-mode)
         ("/PKGBUILD\\'" . sh-mode))
  :hook ((sh-mode . sh-mode-setup)
         (sh-mode . my/sh-eglot-ensure)
         (bash-ts-mode . sh-mode-setup)
         (bash-ts-mode . my/sh-eglot-ensure))
  :config
  (defun sh-mode-setup ()
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t)
    (local-set-key (kbd "C-c C-e") #'sh-execute-region))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

;; Snippets for sh
(use-package tempo
  :ensure nil
  :after sh-script
  :hook ((sh-mode . sh-mode-tempo-setup)
         (bash-ts-mode . sh-mode-tempo-setup))
  :config
  (defvar sh-tempo-tags nil)
  (defun sh-mode-tempo-setup ()
    (tempo-use-tag-list 'sh-tempo-tags))

  (tempo-define-template "sh-shebang"
                         '("#!/bin/bash" > n n
                           "set -euo pipefail -x" > n n
                           )
                         "shebang"
                         "Insert shebang"
                         'sh-tempo-tags))

(use-package eglot
  :ensure nil
  :defer t)

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(sh-mode bash-ts-mode)
     '("bash-language-server" "start")
     :label "bash-language-server"
     :executables '("bash-language-server")
     :note "Shell buffers use bash-language-server through Eglot.")))

(provide 'init-sh)
;;; init-sh.el ends here
