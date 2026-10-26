;;; tools/lsp-ops/config.el --- Language server operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-agnostic lifecycle commands.  `lsp-mode' is the only client, but
;; every command still goes through `+language-server--backend' so there is
;; exactly one place that decides whether a server is running and one uniform
;; error when none is.

;;; Code:

(require 'aaron-funcs)

(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function lsp-describe-session "lsp-mode")
(declare-function lsp-organize-imports "lsp-mode")
(declare-function lsp-restart-workspace "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp-workspace-show-log "lsp-mode")
(declare-function +lsp-mode-shutdown-workspace
                  "tools/lsp/config" (workspace &optional reason))

(defun +language-server--backend ()
  "Return the active language-server backend or signal an error."
  (or (and (fboundp '+current-language-server-backend)
           (+current-language-server-backend))
      (user-error "No active language server in current buffer")))

(defun +language-server/restart ()
  "Restart the active language server."
  (interactive)
  (+language-server--backend)
  (call-interactively #'lsp-restart-workspace))

(defun +language-server/shutdown ()
  "Shutdown the active language server."
  (interactive)
  (+language-server--backend)
  ;; `lsp-disconnect' only detaches the current buffer and intentionally
  ;; keeps the workspace process alive.  The command is named "Shutdown",
  ;; so stop every workspace associated with this buffer through the
  ;; bounded lifecycle owner.
  (dolist (workspace (lsp-workspaces))
    (+lsp-mode-shutdown-workspace workspace 'user-shutdown)))

(defun +language-server/open-log ()
  "Open the current language server log buffer."
  (interactive)
  (+language-server--backend)
  (call-interactively #'lsp-workspace-show-log))

(defun +language-server/describe-session ()
  "Describe or list current language server sessions."
  (interactive)
  (+language-server--backend)
  (call-interactively #'lsp-describe-session))

(defun +language-server/show-workspace-configuration ()
  "Show workspace configuration for the active language server."
  (interactive)
  (+language-server--backend)
  (call-interactively #'lsp-describe-session))

(aaron-leader!
  "c o" '(:def +language-server/organize-imports :which-key "organize imports")
  "c R" '(:def +language-server/restart :which-key "restart language server"))

(provide 'tools/lsp-ops/config)
;;; tools/lsp-ops/config.el ends here
