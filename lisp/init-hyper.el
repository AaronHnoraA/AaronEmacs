;;; init-hyper.el --- Personal Hyper command surfaces -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function my/appine-board "init-appine" ())
(declare-function my/code-actions-dispatch "init-code-actions" ())
(declare-function my/compile-board "init-compile" ())
(declare-function my/compile-dispatch "init-compile" ())
(declare-function my/debug-profile-dispatch "init-debug-profile" ())
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/git-board "init-git-board" ())
(declare-function my/git-dispatch "init-git-core" ())
(declare-function my/health-report "init-health" ())
(declare-function my/jupyter-dispatch "init-jupyter-tools" ())
(declare-function my/jupyter-manager "init-jupyter-core" ())
(declare-function my/language-server-dispatch "init-lsp-tools" ())
(declare-function my/language-server-manager "init-lsp-tools" ())
(declare-function my/output-dispatch "init-output" ())
(declare-function my/problems-project "init-problems" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/project-local-describe "init-project-local" ())
(declare-function my/project-run-dispatch "init-project-run" ())
(declare-function my/remote-connectboard "init-remote-connectboard" (&optional choose-action))
(declare-function my/smerge-dispatch "init-smerge" ())
(declare-function my/task-dispatch "init-task" ())
(declare-function my/test-dispatch "init-test" ())
(declare-function my/workspace-dispatch "init-workspaces" ())
(declare-function telescope "init-telescope" ())

(defun my/hyper-open-remote-connectboard-dispatch ()
  "Open the remote connectboard action picker."
  (interactive)
  (my/remote-connectboard t))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix my/hyper-dispatch ()
    "Hyper management hub for personal commands."
    [["Boards"
      ("c" "compile board" my/compile-board :transient transient--do-exit)
      ("g" "git board" my/git-board :transient transient--do-exit)
      ("L" "language server" my/language-server-manager :transient transient--do-exit)
      ("j" "jupyter manager" my/jupyter-manager :transient transient--do-exit)
      ("a" "appine board" my/appine-board :transient transient--do-exit)
      ("h" "health report" my/health-report :transient transient--do-exit)]
     ["Menus"
      ("p" "project" my/project-dispatch :transient transient--do-exit)
      ("w" "workspace" my/workspace-dispatch :transient transient--do-exit)
      ("r" "run" my/project-run-dispatch :transient transient--do-exit)
      ("k" "task" my/task-dispatch :transient transient--do-exit)
      ("t" "test" my/test-dispatch :transient transient--do-exit)
      ("o" "output" my/output-dispatch :transient transient--do-exit)]
     ["Code / Ops"
      ("." "code actions" my/code-actions-dispatch :transient transient--do-exit)
      ("e" "compile menu" my/compile-dispatch :transient transient--do-exit)
      ("d" "diagnostics" my/diagnostics-dispatch :transient transient--do-exit)
      ("D" "debug" my/debug-profile-dispatch :transient transient--do-exit)
      ("u" "language server menu" my/language-server-dispatch :transient transient--do-exit)
      ("x" "telescope" telescope :transient transient--do-exit)]
     ["More"
      ("m" "git menu" my/git-dispatch :transient transient--do-exit)
      ("s" "merge conflict" my/smerge-dispatch :transient transient--do-exit)
      ("n" "problems project" my/problems-project :transient transient--do-exit)
      ("J" "jupyter menu" my/jupyter-dispatch :transient transient--do-exit)
      ("R" "remote board" my/hyper-open-remote-connectboard-dispatch :transient transient--do-exit)
      ("P" "project local" my/project-local-describe :transient transient--do-exit)]]))

(provide 'init-hyper)
;;; init-hyper.el ends here
