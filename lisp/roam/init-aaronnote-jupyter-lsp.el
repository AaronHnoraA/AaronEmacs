;;; init-aaronnote-jupyter-lsp.el --- Notebook LSP UI integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Notebook LSP uses the ordinary file-owned Remote workspace, direnv and
;; Python toolchain.  A kernel only executes cells; its host and kernelspec do
;; not choose an analyzer or change a notebook's file identity.

;;; Code:

(require 'init-aaronnote-jupyter-cell)
(require 'init-aaronnote-jupyter-project)
(require 'init-lsp-runtime)

;; A live Emacs may reload this module after the old kernel-owned provider was
;; installed.  Retire it without affecting unrelated runtime providers.
(setq my/language-server-runtime-providers
      (seq-remove (lambda (provider)
                    (eq (plist-get provider :name) 'noema-jupyter))
                  my/language-server-runtime-providers))

(defun my/noema-jupyter-cell--lsp-capf-priority-h ()
  "Keep live-kernel completion ahead of static LSP completion."
  (when (bound-and-true-p my/noema-jupyter-cell-mode)
    (setq-local
     completion-at-point-functions
     (cons #'my/noema-jupyter-cell-capf
           (delq #'my/noema-jupyter-cell-capf
                 completion-at-point-functions)))))

(defun my/noema-jupyter-cell--lsp-ui-h ()
  "Keep Noema's controls in its header below the shared tab-line."
  (when (bound-and-true-p my/noema-jupyter-cell-mode)
    (setq-local header-line-format
                '(:eval (my/noema-jupyter-cell--header-line)))
    (my/noema-jupyter-cell--lsp-capf-priority-h)
    (force-mode-line-update t)))

(with-eval-after-load 'lsp-mode
  (advice-remove 'lsp--uri-to-path
                 #'my/noema-jupyter-cell--lsp-source-path)
  (add-hook 'lsp-managed-mode-hook #'my/noema-jupyter-cell--lsp-ui-h))

(with-eval-after-load 'lsp-completion
  (add-hook 'lsp-completion-mode-hook
            #'my/noema-jupyter-cell--lsp-capf-priority-h))

(add-hook 'my/noema-jupyter-cell-mode-hook #'my/noema-jupyter-cell--lsp-ui-h)

(provide 'init-aaronnote-jupyter-lsp)
;;; init-aaronnote-jupyter-lsp.el ends here
