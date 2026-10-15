;;; init-hyper.el --- Personal Hyper command surfaces -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function my/appine-board "init-appine" ())
(declare-function code-actions-menu "init-code-actions" ())
(declare-function my/compile-board "init-compile" ())
(declare-function config-compile-menu "init-compile" ())
(declare-function debug-menu "init-debug" ())
(declare-function editing-menu "init-editing-workbench" ())
(declare-function file-menu "init-file-workbench" ())
(declare-function diagnostics-menu "init-diagnostics-extra" ())
(declare-function my/git-board "init-git-board" ())
(declare-function git-menu "init-git-core" ())
(declare-function my/health-report "init-health" ())
(declare-function language-server-menu "init-lsp-tools" ())
(declare-function my/language-server-manager "init-lsp-tools" ())
(declare-function my/jupyter-board "init-jupyter-board" ())
(declare-function output-menu "init-output" ())
(declare-function my/problems-project "init-problems" ())
(declare-function project-menu "init-project" ())
(declare-function my/project-local-describe "init-project-local" ())
(declare-function project-run-menu "init-project-run" ())
(declare-function my/remote-connectboard "init-remote-connectboard" (&optional choose-action))
(declare-function merge-conflict-menu "init-smerge" ())
(declare-function task-menu "init-task" ())
(declare-function test-menu "init-test" ())
(declare-function workspace-menu "init-workspaces" ())
(declare-function telescope "init-telescope" ())
(declare-function config-dispatch "config-tools" ())
(declare-function keybinding-guide "init-open" ())
(declare-function mode-keybindings "init-tools" ())

(defun my/hyper-open-remote-connectboard-dispatch ()
  "Open the remote connectboard action picker."
  (interactive)
  (my/remote-connectboard t))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix command-hub ()
    "Open the main Emacs command hub."
    [["Boards"
      ("c" "compile board" my/compile-board :transient transient--do-exit)
      ("g" "git board" my/git-board :transient transient--do-exit)
      ("L" "language server" my/language-server-manager :transient transient--do-exit)
      ("j" "jupyter board" my/jupyter-board :transient transient--do-exit)
      ("a" "appine board" my/appine-board :transient transient--do-exit)
      ("h" "health report" my/health-report :transient transient--do-exit)]
     ["Menus"
      ("C" "settings" config-dispatch :transient transient--do-exit)
      ("p" "project" project-menu :transient transient--do-exit)
      ("w" "workspace" workspace-menu :transient transient--do-exit)
      ("r" "run" project-run-menu :transient transient--do-exit)
      ("k" "task" task-menu :transient transient--do-exit)
      ("t" "test" test-menu :transient transient--do-exit)
      ("o" "output" output-menu :transient transient--do-exit)]
     ["Code / Ops"
      ("." "code actions" code-actions-menu :transient transient--do-exit)
      ("e" "compile menu" config-compile-menu :transient transient--do-exit)
      ("d" "diagnostics" diagnostics-menu :transient transient--do-exit)
      ("D" "debug" debug-menu :transient transient--do-exit)
      ("u" "language server menu" language-server-menu :transient transient--do-exit)
      ("x" "telescope" telescope :transient transient--do-exit)]
     ["More"
      ("K" "mode keys" mode-keybindings :transient transient--do-exit)
      ("G" "key guide" keybinding-guide :transient transient--do-exit)
      ("f" "files" file-menu :transient transient--do-exit)
      ("i" "editing" editing-menu :transient transient--do-exit)
      ("m" "git menu" git-menu :transient transient--do-exit)
      ("s" "merge conflict" merge-conflict-menu :transient transient--do-exit)
      ("n" "problems project" my/problems-project :transient transient--do-exit)
      ("R" "remote board" my/hyper-open-remote-connectboard-dispatch :transient transient--do-exit)
      ("P" "project local" my/project-local-describe :transient transient--do-exit)]]))

(provide 'init-hyper)
;;; init-hyper.el ends here
