;;; init-symbols.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function consult-imenu "consult-imenu")
(declare-function consult-imenu-multi "consult-imenu" (&optional query))
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/python-setup-imenu "init-python")
(declare-function xref-find-apropos "xref" (pattern))

(defvar imenu--index-alist)

(defun my/symbols--prepare-buffer-imenu ()
  "Refresh the current buffer's imenu data before symbol search."
  (when (and (fboundp 'my/python-setup-imenu)
             (derived-mode-p 'python-mode 'python-ts-mode))
    (my/python-setup-imenu))
  (when (boundp 'imenu--index-alist)
    (setq imenu--index-alist nil)))

(defun my/symbols-buffer ()
  "Search symbols in the current buffer."
  (interactive)
  (my/symbols--prepare-buffer-imenu)
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (imenu nil)))

(defun my/symbols-project ()
  "Search project symbols.
Prefer workspace symbols from the active language server.  Fall
back to `consult-imenu-multi', which only covers opened buffers."
  (interactive)
  (cond
   ((and (fboundp 'my/current-language-server-backend)
         (my/current-language-server-backend))
    (call-interactively #'xref-find-apropos))
   ((fboundp 'consult-imenu-multi)
    (consult-imenu-multi))
   (t
    (my/symbols-buffer))))

(global-set-key (kbd "M-g i") #'my/symbols-buffer)

(my/evil-global-leader-set "s b" #'my/symbols-buffer "buffer symbols")
(my/evil-global-leader-set "s I" #'my/symbols-project "project symbols")

(provide 'init-symbols)
;;; init-symbols.el ends here
