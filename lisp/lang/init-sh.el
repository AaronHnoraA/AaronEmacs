;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/language-server-executable-available-p "init-lsp" (program))
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/register-language-server "init-lsp")

;; Edit shell scripts
;;
;; sh-mode provides `sh-while-getopts' to automate getopts.
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'"     . sh-mode)
         ("/PKGBUILD\\'" . sh-mode))
  :hook ((sh-mode . sh-mode-setup)
         (bash-ts-mode . sh-mode-setup))
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

(with-eval-after-load 'lsp-mode
  (when (fboundp 'my/register-language-server)
    (my/register-language-server
     '(sh-mode bash-ts-mode)
     (lambda ()
       (list (or (my/language-server-executable-find "bash-language-server")
                 "bash-language-server")
             "start"))
     :server-id 'my-bash
     :priority 1
     :label "bash-language-server"
     ;; Shell buffers are everywhere; only claim one when the server is
     ;; actually installed on the workspace target.
     :activation-fn
     (lambda (&rest _)
       (and (derived-mode-p 'sh-mode 'bash-ts-mode)
            (my/language-server-executable-available-p "bash-language-server")))
     :executables '("bash-language-server")
     :note "Shell buffers use bash-language-server through lsp-mode.")))

(provide 'init-sh)
;;; init-sh.el ends here
