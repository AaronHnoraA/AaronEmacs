;;; init-lsp-ops.el --- Language server operations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/current-language-server-backend "init-lsp")
(declare-function eglot-code-action-organize-imports "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-events-buffer "eglot")
(declare-function eglot-list-connections "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-show-workspace-configuration "eglot")
(declare-function eglot-shutdown "eglot" (server))
(declare-function lsp-describe-session "lsp-mode")
(declare-function lsp-disconnect "lsp-mode")
(declare-function lsp-organize-imports "lsp-mode")
(declare-function lsp-restart-workspace "lsp-mode")
(declare-function lsp-workspace-show-log "lsp-mode")

(defun my/language-server--backend ()
  "Return the active language-server backend or signal an error."
  (or (and (fboundp 'my/current-language-server-backend)
           (my/current-language-server-backend))
      (user-error "No active language server in current buffer")))

(defun my/language-server-organize-imports ()
  "Organize imports using the active language server backend."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (call-interactively #'eglot-code-action-organize-imports))
    ('lsp-mode
     (call-interactively #'lsp-organize-imports))))

(defun my/language-server-restart ()
  "Restart the active language server."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (call-interactively #'eglot-reconnect))
    ('lsp-mode
     (call-interactively #'lsp-restart-workspace))))

(defun my/language-server-shutdown ()
  "Shutdown the active language server."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (eglot-shutdown (eglot-current-server)))
    ('lsp-mode
     (call-interactively #'lsp-disconnect))))

(defun my/language-server-open-log ()
  "Open the current language server log buffer."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (call-interactively #'eglot-events-buffer))
    ('lsp-mode
     (call-interactively #'lsp-workspace-show-log))))

(defun my/language-server-describe-session ()
  "Describe or list current language server sessions."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (call-interactively #'eglot-list-connections))
    ('lsp-mode
     (call-interactively #'lsp-describe-session))))

(defun my/language-server-show-workspace-configuration ()
  "Show workspace configuration when supported by the active backend."
  (interactive)
  (pcase (my/language-server--backend)
    ('eglot
     (call-interactively #'eglot-show-workspace-configuration))
    ('lsp-mode
     (call-interactively #'lsp-describe-session))))

(my/leader!
  "c o" '(:def my/language-server-organize-imports :which-key "organize imports")
  "c R" '(:def my/language-server-restart :which-key "restart language server"))

(provide 'init-lsp-ops)
;;; init-lsp-ops.el ends here
