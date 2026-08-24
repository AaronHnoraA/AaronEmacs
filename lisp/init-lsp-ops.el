;;; init-lsp-ops.el --- Language server operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-agnostic lifecycle commands.  `lsp-mode' is the only client, but
;; every command still goes through `my/language-server--backend' so there is
;; exactly one place that decides whether a server is running and one uniform
;; error when none is.

;;; Code:

(require 'init-funcs)

(declare-function my/current-language-server-backend "init-lsp")
(declare-function lsp-describe-session "lsp-mode")
(declare-function lsp-organize-imports "lsp-mode")
(declare-function lsp-restart-workspace "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp-workspace-show-log "lsp-mode")
(declare-function my/lsp-mode-shutdown-workspace
                  "init-lsp" (workspace &optional reason))

(defun my/language-server--backend ()
  "Return the active language-server backend or signal an error."
  (or (and (fboundp 'my/current-language-server-backend)
           (my/current-language-server-backend))
      (user-error "No active language server in current buffer")))

(defun my/language-server-restart ()
  "Restart the active language server."
  (interactive)
  (my/language-server--backend)
  (call-interactively #'lsp-restart-workspace))

(defun my/language-server-shutdown ()
  "Shutdown the active language server."
  (interactive)
  (my/language-server--backend)
  ;; `lsp-disconnect' only detaches the current buffer and intentionally
  ;; keeps the workspace process alive.  The command is named "Shutdown",
  ;; so stop every workspace associated with this buffer through the
  ;; bounded lifecycle owner.
  (dolist (workspace (lsp-workspaces))
    (my/lsp-mode-shutdown-workspace workspace 'user-shutdown)))

(defun my/language-server-open-log ()
  "Open the current language server log buffer."
  (interactive)
  (my/language-server--backend)
  (call-interactively #'lsp-workspace-show-log))

(defun my/language-server-describe-session ()
  "Describe or list current language server sessions."
  (interactive)
  (my/language-server--backend)
  (call-interactively #'lsp-describe-session))

(defun my/language-server-show-workspace-configuration ()
  "Show workspace configuration for the active language server."
  (interactive)
  (my/language-server--backend)
  (call-interactively #'lsp-describe-session))

(my/leader!
  "c o" '(:def my/language-server-organize-imports :which-key "organize imports")
  "c R" '(:def my/language-server-restart :which-key "restart language server"))

(provide 'init-lsp-ops)
;;; init-lsp-ops.el ends here
