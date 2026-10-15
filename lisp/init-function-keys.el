;;; init-function-keys.el --- Global function-key shortcuts -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reserve F11 for the window manager / system shortcuts.
;;
;; The rest of the function keys are mapped to one-tap entry points for the
;; current daily workflow: search, project, run/test/debug, terminal and
;; LLM.

;;; Code:

(require 'general)

(declare-function telescope "init-telescope" ())
(declare-function project-menu "init-project" ())
(declare-function my/telescope-ripgrep "init-telescope" ())
(declare-function project-run-menu "init-project-run" ())
(declare-function test-menu "init-test" ())
(declare-function debug-menu "init-debug" ())
(declare-function ghostel-toggle "init-ghostel-popup" ())
(declare-function noema-agenda "init-aaronnote" (&optional query))
(autoload 'olivetti-mode "olivetti" nil t)

(general-define-key
 "<f1>" #'help-command
 "<f2>" #'telescope
 "<f3>" #'project-menu
 "<f4>" #'my/telescope-ripgrep
 "<f5>" #'project-run-menu
 "<f6>" #'test-menu
 "<f7>" #'debug-menu
 "<f8>" #'olivetti-mode
 "<f9>" #'noema-agenda
 "<f10>" #'ghostel-toggle)
;; F11 intentionally left alone.

(provide 'init-function-keys)
;;; init-function-keys.el ends here
