;;; init-search-extra.el --- Extra search commands -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/project-current-root "init-project")
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))

(defconst my/search-todo-regexp
  "TODO\\|FIXME\\|BUG\\|HACK\\|NOTE\\|WIP"
  "Default regexp used for TODO-style searches.")

(defun my/search-project-root ()
  "Return the root directory used by project-aware searches."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun my/search-todos ()
  "Search TODO-style markers in the current project."
  (interactive)
  (consult-ripgrep (my/search-project-root) my/search-todo-regexp))

(defun my/search-symbol-at-point ()
  "Search the symbol at point in the current project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (consult-ripgrep (my/search-project-root) symbol)))

(defun my/search-project-lines ()
  "Search lines across project buffers."
  (interactive)
  (if (fboundp 'consult-line-multi)
      (consult-line-multi nil)
    (user-error "consult-line-multi is unavailable")))

(my/leader!
  "s t" '(:def my/search-todos :which-key "todos")
  "s P" '(:def my/search-symbol-at-point :which-key "grep symbol")
  "s L" '(:def my/search-project-lines :which-key "project lines"))

(provide 'init-search-extra)
;;; init-search-extra.el ends here
