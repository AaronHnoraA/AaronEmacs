;;; init-beancount.el --- Beancount configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Editing support for Beancount journals, plus the `my/beancount' UI
;; (dedicated frame: NAS Fava on the left, a bean-tool-backed dashboard
;; and entry wizard on the right).  See beancount-tool.el,
;; beancount-dashboard.el, beancount-entry.el and beancount-frame.el.

;;; Code:

(declare-function my/register-lsp-mode-preference "init-lsp" (mode &optional feature source note))
(declare-function my/lsp-mode-ensure "init-lsp")
(defvar lsp-beancount-journal-file)
(defvar lsp-beancount-langserver-executable)
(defvar lsp-auto-guess-root)

(defgroup my/beancount nil
  "Beancount editing support."
  :group 'languages)

(defcustom my/beancount-check-command "bean-check"
  "Program used to validate Beancount files."
  :type 'string
  :group 'my/beancount)

(defcustom my/beancount-language-server-command "beancount-lsp-server"
  "Program used by `lsp-beancount'."
  :type 'string
  :group 'my/beancount)

(defun my/beancount-locate-root-file ()
  "Return the nearest Beancount root journal for the current buffer."
  (or (when-let* ((root (locate-dominating-file default-directory "main.bean")))
        (expand-file-name "main.bean" root))
      (when-let* ((root (locate-dominating-file default-directory "main.beancount")))
        (expand-file-name "main.beancount" root))
      buffer-file-name))

(defun my/beancount-setup ()
  "Apply local defaults for Beancount buffers."
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local compile-command
              (mapconcat #'shell-quote-argument
                         (delq nil
                               (list my/beancount-check-command
                                     (my/beancount-locate-root-file)))
                         " "))
  (setq-local lsp-beancount-journal-file
              (my/beancount-locate-root-file))
  (setq-local lsp-auto-guess-root t)
  (my/lsp-mode-ensure))

(use-package beancount
  :ensure t
  :mode (("\\.bean\\'" . beancount-mode)
         ("\\.beancount\\'" . beancount-mode))
  :hook (beancount-mode . my/beancount-setup))

(when (fboundp 'my/register-lsp-mode-preference)
  (my/register-lsp-mode-preference
   'beancount-mode
   'lsp-beancount
   load-file-name
   "Beancount buffers use beancount-language-server through lsp-mode."))

(with-eval-after-load 'lsp-beancount
  (setq lsp-beancount-langserver-executable
        my/beancount-language-server-command))

(require 'beancount-tool)
(require 'beancount-dashboard)
(require 'beancount-entry)
(require 'beancount-frame)

(with-eval-after-load 'beancount
  (my/local-leader!
    :keymaps 'beancount-mode-map
    "c" 'compile
    "d" 'my/beancount
    "a" 'my/beancount-add-entry
    "s" 'my/beancount-add-split
    "y" 'my/beancount-sync
    "f" 'my/beancount-fava-reload))

(provide 'init-beancount)
;;; init-beancount.el ends here
