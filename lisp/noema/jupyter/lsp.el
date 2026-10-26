;;; noema/jupyter/lsp.el --- Notebook LSP UI integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Notebook LSP uses the ordinary file-owned Remote workspace, direnv and
;; Python toolchain.  A kernel only executes cells; its host and kernelspec do
;; not choose an analyzer or change a notebook's file identity.

;;; Code:

(require 'noema/jupyter/cell)
(require 'noema/jupyter/project)
(require 'tools/lsp/runtime)

;; A live Emacs may reload this module after the old kernel-owned provider was
;; installed.  Retire it without affecting unrelated runtime providers.
(setq +language-server-runtime-providers
      (seq-remove (lambda (provider)
                    (eq (plist-get provider :name) 'noema-jupyter))
                  +language-server-runtime-providers))

(defun +noema-jupyter-cell--lsp-capf-priority-h ()
  "Keep live-kernel completion ahead of static LSP completion."
  (when (bound-and-true-p +noema-jupyter-cell-mode)
    (setq-local
     completion-at-point-functions
     (cons #'+noema-jupyter-cell-capf
           (delq #'+noema-jupyter-cell-capf
                 completion-at-point-functions)))))

(defun +noema-jupyter-cell--lsp-ui-h ()
  "Keep Noema's controls in its header below the shared tab-line."
  (when (bound-and-true-p +noema-jupyter-cell-mode)
    (setq-local header-line-format
                '(:eval (+noema-jupyter-cell--header-line)))
    (+noema-jupyter-cell--lsp-capf-priority-h)
    (force-mode-line-update t)))

(with-eval-after-load 'lsp-mode
  (advice-remove 'lsp--uri-to-path
                 #'+noema-jupyter-cell--lsp-source-path)
  (add-hook 'lsp-managed-mode-hook #'+noema-jupyter-cell--lsp-ui-h))

(with-eval-after-load 'lsp-completion
  (add-hook 'lsp-completion-mode-hook
            #'+noema-jupyter-cell--lsp-capf-priority-h))

(add-hook '+noema-jupyter-cell-mode-hook #'+noema-jupyter-cell--lsp-ui-h)

(provide 'noema/jupyter/lsp)
;;; noema/jupyter/lsp.el ends here
