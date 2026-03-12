;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; ----------------------------
;; 安装 emacs-jupyter
;; ----------------------------

(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-inspect-at-point
             jupyter-eval-line-or-region)
  :bind (("C-c j r" . jupyter-run-repl)
         ("C-c j c" . jupyter-connect-repl)
         ("C-c j i" . jupyter-inspect-at-point)
         ("C-c j e" . jupyter-eval-line-or-region))
  :init
  (setq jupyter-log-buffer-name "*jupyter-log*")
  (setq jupyter-repl-interaction-mode-enable-prompt-overlay t)
  (setq jupyter-repl-buffer-name-template "*jupyter-repl[%s]*"))

(use-package code-cells
  :ensure t
  :commands (code-cells-mode code-cells-eval))





(provide 'init-jupyter)



;;; init-base.el ends here
