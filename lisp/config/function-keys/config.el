;;; config/function-keys/config.el --- Global function-key shortcuts -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reserve F11 for the window manager / system shortcuts.
;;
;; The rest of the function keys are mapped to one-tap entry points for the
;; current daily workflow: search, project, run/test/debug, terminal and
;; LLM.

;;; Code:

(require 'general)

(declare-function telescope "completion/telescope/config" ())
(declare-function project-menu "tools/project/config" ())
(declare-function +telescope/ripgrep "completion/telescope/config" ())
(declare-function project-run-menu "tools/run/config" ())
(declare-function test-menu "tools/test/config" ())
(declare-function debug-menu "tools/debugger/config" ())
(declare-function +portal-popup/toggle "term/portal/popup" ())
(declare-function noema-agenda "noema/bridge/config" (&optional query))
(autoload 'olivetti-mode "olivetti" nil t)

(general-define-key
 "<f1>" #'help-command
 "<f2>" #'telescope
 "<f3>" #'project-menu
 "<f4>" #'+telescope/ripgrep
 "<f5>" #'project-run-menu
 "<f6>" #'test-menu
 "<f7>" #'debug-menu
 "<f8>" #'olivetti-mode
 "<f9>" #'noema-agenda
 "<f10>" #'+portal-popup/toggle)
;; F11 intentionally left alone.

(provide 'config/function-keys/config)
;;; config/function-keys/config.el ends here
