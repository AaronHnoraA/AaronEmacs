;;; init-search-extra-tests.el --- Routed project searches -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'consult)
(require 'completion/search-extra/config)

(ert-deftest search-extra-symbol-and-todos-use-target-grep-fallback ()
  (with-temp-buffer
    (insert "needle")
    (goto-char (point-min))
    (let ((consult-ripgrep-args "rg --null --smart-case")
          calls)
      (cl-letf (((symbol-function '+project-current-root)
                 (lambda () "/fs:box:/work/"))
                ((symbol-function '+direnv-update-environment-maybe) #'ignore)
                ((symbol-function 'remote-search-ripgrep)
                 (lambda (&rest _) nil))
                ((symbol-function 'remote-client-file-name)
                 (lambda (&rest _) nil))
                ((symbol-function 'remote-executable-find)
                 (lambda (program _context)
                   (when (equal program "grep") "/usr/bin/grep")))
                ((symbol-function 'consult-ripgrep)
                 (lambda (&rest _)
                   (ert-fail "Remote search bypassed target grep fallback")))
                ((symbol-function 'consult-grep)
                 (lambda (root &optional initial)
                   (push (list root initial) calls))))
        (+search/symbol-at-point)
        (+search/todos))
      (should (equal (nreverse calls)
                     (list '("/fs:box:/work/" "needle")
                           (list "/fs:box:/work/" +search-todo-regexp)))))))

;;; init-search-extra-tests.el ends here
