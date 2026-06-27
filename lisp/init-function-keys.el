;;; init-function-keys.el --- Global function-key shortcuts -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reserve F11 for the window manager / system shortcuts.
;;
;; The rest of the function keys are mapped to one-tap entry points for the
;; current daily workflow: search, project, run/test/debug, terminal and
;; LLM.

;;; Code:

(declare-function telescope "init-telescope" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/telescope-ripgrep "init-telescope" ())
(declare-function my/project-run-dispatch "init-project-run" ())
(declare-function my/test-dispatch "init-test" ())
(declare-function my/debug-dispatch "init-debug" ())
(declare-function vterm-toggle "init-vterm-popup" ())
(declare-function claude-code-ide-menu "claude-code-ide" ())
(autoload 'olivetti-mode "olivetti" nil t)

(global-set-key (kbd "<f1>") #'help-command)
(global-set-key (kbd "<f2>") #'telescope)
(global-set-key (kbd "<f3>") #'my/project-dispatch)
(global-set-key (kbd "<f4>") #'my/telescope-ripgrep)
(global-set-key (kbd "<f5>") #'my/project-run-dispatch)
(global-set-key (kbd "<f6>") #'my/test-dispatch)
(global-set-key (kbd "<f7>") #'my/debug-dispatch)
(global-set-key (kbd "<f8>") #'olivetti-mode)
(global-set-key (kbd "<f10>") #'vterm-toggle)
;; F11 intentionally left alone.
(global-set-key (kbd "<f12>") #'claude-code-ide-menu)

(provide 'init-function-keys)
;;; init-function-keys.el ends here
