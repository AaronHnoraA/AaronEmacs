;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/eglot-ensure "init-lsp")
(defvar imenu-create-index-function)
(defvar-local my/python-imenu-backend nil
  "Original Python imenu backend for the current buffer.")

(use-package eglot
  :ensure t
  :hook ((python-mode . my/eglot-ensure)
         (python-ts-mode . my/eglot-ensure)))

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.sage\\'" . python-mode))
  :hook ((python-mode . my/python-setup-imenu)
         (python-ts-mode . my/python-setup-imenu))
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (defun my/python-imenu-create-index ()
    "Return a Python imenu index with a safe fallback.

Prefer the mode-provided backend, but fall back to
`python-imenu-create-index' when the tree-sitter backend is unavailable or
fails.  This keeps `consult-imenu' and Treemacs tags working in
`python-ts-mode' buffers even when the Python grammar is missing."
    (or (and (functionp my/python-imenu-backend)
             (ignore-errors
               (funcall my/python-imenu-backend)))
        (python-imenu-create-index)))

  (defun my/python-setup-imenu ()
    "Ensure Python buffers always expose a usable imenu index."
    (setq-local my/python-imenu-backend
                (let ((backend imenu-create-index-function))
                  (if (and (functionp backend)
                           (not (eq backend #'my/python-imenu-create-index)))
                      backend
                    #'python-imenu-create-index)))
    (setq-local imenu-create-index-function #'my/python-imenu-create-index))

  (defun my/python-refresh-open-buffers ()
    "Reapply Python buffer setup to already-open buffers.

This makes `my/reload-init' immediately fix existing Python buffers instead of
waiting for them to be reopened."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'python-mode 'python-ts-mode)
          (my/python-setup-imenu)))))

  (defun my/python-ensure-imenu-around (orig-fn &optional noerror)
    "Ensure Python buffers expose imenu before calling ORIG-FN.

This catches callers like Treemacs that may build imenu indices from temporary
buffers where normal mode hooks were skipped."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (my/python-setup-imenu))
    (funcall orig-fn noerror))

  (with-eval-after-load 'imenu
    (advice-add 'imenu--make-index-alist :around #'my/python-ensure-imenu-around))

  (my/python-refresh-open-buffers))
(provide 'init-python)
;;; init-python.el ends here
