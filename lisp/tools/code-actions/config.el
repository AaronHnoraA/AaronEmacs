;;; tools/code-actions/config.el --- Unified code action menu -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)

(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +language-server/code-actions "tools/lsp/config")
(declare-function +language-server/rename "tools/lsp/config")
(declare-function +language-server/format-buffer "tools/lsp/config")
(declare-function +format/toggle-on-save "editor/format/config")
(declare-function +problems/buffer "checkers/problems/config")
(declare-function +problems/project "checkers/problems/config")
(declare-function +language-server/workspace-symbol "tools/workspace-symbol/config")
(declare-function +symbols/buffer "tools/symbols/config")
(declare-function +symbols/project "tools/symbols/config")
(declare-function +task/build "tools/task/config")
(declare-function +task/build-rerun "tools/task/config")
(declare-function lsp-organize-imports "lsp-mode")

(defun +language-server/organize-imports ()
  "Organize imports using the active language server when supported."
  (interactive)
  (unless (and (fboundp '+current-language-server-backend)
               (+current-language-server-backend))
    (user-error "No active language server in current buffer"))
  (if (fboundp 'lsp-organize-imports)
      (lsp-organize-imports)
    (+language-server/code-actions)))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix code-actions-menu ()
    "Open code editing, inspection, and build actions."
    [["Edit"
      ("a" "code actions" +language-server/code-actions)
      ("r" "rename symbol" +language-server/rename)
      ("f" "format buffer" +language-server/format-buffer)
      ("F" "toggle format-on-save" +format/toggle-on-save)
      ("o" "organize imports" +language-server/organize-imports)]
     ["Inspect"
      ("!" "problems" +problems/buffer)
      ("?" "project problems" +problems/project)
      ("s" "workspace symbol" +language-server/workspace-symbol)
      ("i" "buffer symbols" +symbols/buffer)
      ("I" "project symbols" +symbols/project)]
     ["Build"
      ("b" "build" +task/build)
      ("B" "rerun build" +task/build-rerun)]]))

(define-leader-key!
  "c ." '(:def code-actions-menu :which-key "code menu"))

(provide 'tools/code-actions/config)
;;; tools/code-actions/config.el ends here
