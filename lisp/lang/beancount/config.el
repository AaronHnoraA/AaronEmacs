;;; lang/beancount/config.el --- Beancount configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Editing support for Beancount journals, plus the `+beancount/beancount' UI
;; (dedicated frame: NAS Fava on the left, a bean-tool-backed dashboard
;; and entry wizard on the right).  See lang/beancount/tool.el,
;; lang/beancount/dashboard.el, lang/beancount/entry.el and lang/beancount/frame.el.

;;; Code:

(declare-function +lsp/mode-ensure "tools/lsp/config")
(defvar lsp-beancount-journal-file)
(defvar lsp-beancount-langserver-executable)
(defvar lsp-auto-guess-root)

(defgroup +beancount/beancount nil
  "Beancount editing support."
  :group 'languages)

(defcustom +beancount-check-command "bean-check"
  "Program used to validate Beancount files."
  :type 'string
  :group '+beancount/beancount)

(defcustom +beancount-language-server-command "beancount-lsp-server"
  "Program used by `lsp-beancount'."
  :type 'string
  :group '+beancount/beancount)

(defun +beancount-locate-root-file ()
  "Return the nearest Beancount root journal for the current buffer."
  (or (when-let* ((root (locate-dominating-file default-directory "main.bean")))
        (expand-file-name "main.bean" root))
      (when-let* ((root (locate-dominating-file default-directory "main.beancount")))
        (expand-file-name "main.beancount" root))
      buffer-file-name))

(defun +beancount-setup ()
  "Apply local defaults for Beancount buffers."
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local compile-command
              (mapconcat #'shell-quote-argument
                         (delq nil
                               (list +beancount-check-command
                                     (+beancount-locate-root-file)))
                         " "))
  (setq-local lsp-beancount-journal-file
              (+beancount-locate-root-file))
  (setq-local lsp-auto-guess-root t)
  (+lsp/mode-ensure))

(use-package beancount
  :ensure t
  :mode (("\\.bean\\'" . beancount-mode)
         ("\\.beancount\\'" . beancount-mode))
  :hook (beancount-mode . +beancount-setup))

(with-eval-after-load 'lsp-beancount
  (setq lsp-beancount-langserver-executable
        +beancount-language-server-command))

(require 'lang/beancount/tool)
(require 'lang/beancount/dashboard)
(require 'lang/beancount/entry)
(require 'lang/beancount/frame)

(with-eval-after-load 'beancount
  (aaron-local-leader!
    :keymaps 'beancount-mode-map
    "c" 'compile
    "d" '+beancount/beancount
    "a" '+beancount/add-entry
    "s" '+beancount/add-split
    "y" '+beancount/sync
    "f" '+beancount/fava-reload))

(provide 'lang/beancount/config)
;;; lang/beancount/config.el ends here
