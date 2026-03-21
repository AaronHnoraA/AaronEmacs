;;; init-workspace-symbol.el --- Workspace symbol lookup -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/symbols-workspace-dynamic "init-symbols")

(defun my/language-server-workspace-symbol ()
  "Search workspace symbols through the active xref/LSP backend."
  (interactive)
  (unless (and (fboundp 'my/current-language-server-backend)
               (my/current-language-server-backend))
    (user-error "No active language server in the current buffer"))
  (call-interactively #'my/symbols-workspace-dynamic))

(my/evil-global-leader-set "c s" #'my/language-server-workspace-symbol
                           "workspace symbol")

(provide 'init-workspace-symbol)
;;; init-workspace-symbol.el ends here
