;;; config/hyper/config.el --- Personal Hyper command surfaces -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function code-actions-menu "tools/code-actions/config" ())
(declare-function +compile/board "config/compile/config" ())
(declare-function config-compile-menu "config/compile/config" ())
(declare-function debug-menu "tools/debugger/config" ())
(declare-function editing-menu "editor/editing/config" ())
(declare-function file-menu "emacs/files/config" ())
(declare-function diagnostics-menu "checkers/diagnostics-extra/config" ())
(declare-function +git/board "tools/git/board" ())
(declare-function git-menu "tools/git/core" ())
(declare-function +health/report "config/health/config" ())
(declare-function language-server-menu "tools/lsp-hub/config" ())
(declare-function +language-server/manager "tools/lsp-hub/config" ())
(declare-function +jupyter/board "tools/jupyter/config" ())
(declare-function output-menu "tools/output/config" ())
(declare-function +problems/project "checkers/problems/config" ())
(declare-function project-menu "tools/project/config" ())
(declare-function +project-local/describe "tools/project-local/config" ())
(declare-function project-run-menu "tools/run/config" ())
(declare-function +connectboard/remote-connectboard "tools/connectboard/config" (&optional choose-action))
(declare-function merge-conflict-menu "emacs/smerge/config" ())
(declare-function task-menu "tools/task/config" ())
(declare-function test-menu "tools/test/config" ())
(declare-function workspace-menu "ui/workspaces/config" ())
(declare-function telescope "completion/telescope/config" ())
(declare-function config-dispatch "config-tools" ())
(declare-function keybinding-guide "aaron-open" ())
(declare-function mode-keybindings "tools/menus/config" ())

(defun +hyper/hyper-open-remote-connectboard-dispatch ()
  "Open the remote connectboard action picker."
  (interactive)
  (+connectboard/remote-connectboard t))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix command-hub ()
    "Open the main Emacs command hub."
    [["Boards"
      ("c" "compile board" +compile/board :transient transient--do-exit)
      ("g" "git board" +git/board :transient transient--do-exit)
      ("L" "language server" +language-server/manager :transient transient--do-exit)
      ("j" "jupyter board" +jupyter/board :transient transient--do-exit)
      ("h" "health report" +health/report :transient transient--do-exit)]
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
      ("n" "problems project" +problems/project :transient transient--do-exit)
      ("R" "remote board" +hyper/hyper-open-remote-connectboard-dispatch :transient transient--do-exit)
      ("P" "project local" +project-local/describe :transient transient--do-exit)]]))

(provide 'config/hyper/config)
;;; config/hyper/config.el ends here
