;;; tools/workspace-symbol/config.el --- Workspace symbol lookup -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)

(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +symbols/xref-apropos "tools/symbols/config")

(defun +language-server/workspace-symbol ()
  "Search workspace symbols through the active xref/LSP backend."
  (interactive)
  (unless (and (fboundp '+current-language-server-backend)
               (+current-language-server-backend))
    (user-error "No active language server in the current buffer"))
  (call-interactively #'+symbols/xref-apropos))

(aaron-leader!
  "c S" '(:def +language-server/workspace-symbol :which-key "workspace symbol"))

(provide 'tools/workspace-symbol/config)
;;; tools/workspace-symbol/config.el ends here
