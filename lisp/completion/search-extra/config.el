;;; completion/search-extra/config.el --- Extra search commands -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)

(declare-function +project-current-root "tools/project/config")
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))

(defconst +search-todo-regexp
  "TODO\\|FIXME\\|BUG\\|HACK\\|NOTE\\|WIP"
  "Default regexp used for TODO-style searches.")

(defun +search-project-root ()
  "Return the root directory used by project-aware searches."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      default-directory))

(defun +search/todos ()
  "Search TODO-style markers in the current project."
  (interactive)
  (consult-ripgrep (+search-project-root) +search-todo-regexp))

(defun +search/symbol-at-point ()
  "Search the symbol at point in the current project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (consult-ripgrep (+search-project-root) symbol)))

(defun +search/project-lines ()
  "Search lines across project buffers."
  (interactive)
  (if (fboundp 'consult-line-multi)
      (consult-line-multi nil)
    (user-error "consult-line-multi is unavailable")))

(define-leader-key!
  "s t" '(:def +search/todos :which-key "todos")
  "s P" '(:def +search/symbol-at-point :which-key "grep symbol")
  "s L" '(:def +search/project-lines :which-key "project lines"))

(provide 'completion/search-extra/config)
;;; completion/search-extra/config.el ends here
