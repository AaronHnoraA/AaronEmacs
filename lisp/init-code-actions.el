;;; init-code-actions.el --- Unified code action menu -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/language-server-code-actions "init-lsp")
(declare-function my/language-server-rename "init-lsp")
(declare-function my/language-server-format-buffer "init-lsp")
(declare-function my/format-toggle-on-save "init-format")
(declare-function my/problems-buffer "init-problems")
(declare-function my/problems-project "init-problems")
(declare-function my/language-server-workspace-symbol "init-workspace-symbol")
(declare-function my/symbols-buffer "init-symbols")
(declare-function my/symbols-project "init-symbols")
(declare-function my/task-build "init-task")
(declare-function my/task-build-rerun "init-task")
(declare-function lsp-organize-imports "lsp-mode")

(defun my/language-server-organize-imports ()
  "Organize imports using the active language server when supported."
  (interactive)
  (pcase (and (fboundp 'my/current-language-server-backend)
              (my/current-language-server-backend))
    ('lsp-mode
     (if (fboundp 'lsp-organize-imports)
         (lsp-organize-imports)
       (my/language-server-code-actions)))
    ('eglot
     (my/language-server-code-actions))
    (_
     (user-error "No active language server in current buffer"))))

(use-package transient
  :ensure nil
  :defer t
  :config
  (transient-define-prefix my/code-actions-dispatch ()
    "Unified code action menu."
    [["Edit"
      ("a" "code actions" my/language-server-code-actions)
      ("r" "rename symbol" my/language-server-rename)
      ("f" "format buffer" my/language-server-format-buffer)
      ("F" "toggle format-on-save" my/format-toggle-on-save)
      ("o" "organize imports" my/language-server-organize-imports)]
     ["Inspect"
      ("!" "problems" my/problems-buffer)
      ("?" "project problems" my/problems-project)
      ("s" "workspace symbol" my/language-server-workspace-symbol)
      ("i" "buffer symbols" my/symbols-buffer)
      ("I" "project symbols" my/symbols-project)]
     ["Build"
      ("b" "build" my/task-build)
      ("B" "rerun build" my/task-build-rerun)]]))

(my/evil-global-leader-set "c ." #'my/code-actions-dispatch "code menu")

(provide 'init-code-actions)
;;; init-code-actions.el ends here
