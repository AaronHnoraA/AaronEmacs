;;; init-hyper.el --- Personal Hyper command surfaces -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'seq)

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

(defvar my/hyper-command-history nil
  "History for `my/hyper-execute-command'.")

(defvar my/hyper-command-active nil
  "Non-nil while `my/hyper-execute-command' is reading a command.")

(defvar my/hyper-command-hidden-regexps
  '("^my/.+--"
    "-mode\\'"
    "-refresh\\'"
    "-quit\\'"
    "^my/appine-\\(?:back\\|clear-\\|close-tab\\|create-plugin\\|delete-plugin\\|find\\|focus-or-open\\|forward\\|import-\\|kill-all\\|new-tab\\|next-tab\\|open-\\(?:cache\\|cookie\\|docs\\|local-storage\\|plugin\\|plugins\\)\\|prev-tab\\|restart\\|run-js-once\\|scroll-\\|to-\\|toggle-org-links\\)"
    "^my/\\(?:apply-font-config\\|compile-apply-runtime-settings\\|confirm-quit-emacs\\|dashboard-upgrade-packages\\|dired-open-dwim\\|escape\\|evil-\\|python-setup-imenu\\|refresh-current-content\\|rust-eglot-setup\\|ui-apply-polish\\)"
    "^my/bookmark-list-"
    "^my/diagnostics-\\(?:buffer-errors\\|buffer-warnings\\|clear\\|project-errors\\|project-warnings\\|rerun\\|show\\)"
    "^my/\\(?:git-board\\|git-tree\\)-"
    "^my/git-\\(?:stage\\|unstage\\)-current-file\\'"
    "^my/\\(?:eglot\\|lsp-mode\\)-ensure"
    "^my/\\(?:jupyter\\|language-server\\)-manager-"
    "^my/jupyter-\\(?:clear-language-connection-file\\|connect-existing-repl\\|connect-repl-dwim\\|edit-\\|install-current-python-kernel\\|open-\\(?:current\\|jupyter\\|kernelspec\\|remote\\|runtime\\)\\|prune-stale-connections\\|refresh-kernelspecs\\|register-language-connection-file\\|remote-ikernel-\\|run-repl-for-language\\|set-default-kernel\\|show-\\|use-connection-file\\)"
    "^my/language-server-\\(?:describe-session\\|ensure\\|open-log\\|ops-dispatch\\|restart\\|set-\\|show-workspace-configuration\\|shutdown\\|toggle-\\)"
    "^my/maintenance-state-"
    "^my/output-\\(?:first\\|next\\|previous\\|stop\\|toggle\\)"
    "^my/project-\\(?:prune-hidden-project-state\\|popup-vterm-app\\)"
    "^my/remote-connectboard-\\(?:copy\\|open\\|vterm\\)"
    "^my/smerge-keep-"
    "^my/treemacs-repair-workspace\\'"
    "^my/workspace-switch-to-[0-9]+\\'")
  "Regexps matching personal commands hidden from `my/hyper-execute-command'.")

(add-to-list 'savehist-additional-variables 'my/hyper-command-history)

(defun my/hyper-hidden-command-p (command)
  "Return non-nil when COMMAND should be hidden from the Hyper picker."
  (let ((name (if (symbolp command) (symbol-name command) command)))
    (and (stringp name)
         (seq-some (lambda (regexp)
                     (string-match-p regexp name))
                   my/hyper-command-hidden-regexps))))

(defun my/hyper-public-command-p (symbol)
  "Return non-nil when SYMBOL should appear in the personal command picker."
  (let ((name (symbol-name symbol)))
    (and (commandp symbol)
         (string-prefix-p "my/" name)
         (not (string-prefix-p "my/hyper-" name))
         (not (my/hyper-hidden-command-p symbol)))))

(defun my/hyper-command-candidates ()
  "Return interactive commands whose names start with \"my/\"."
  (let (commands)
    (mapatoms
     (lambda (symbol)
       (when (my/hyper-public-command-p symbol)
         (push (symbol-name symbol) commands))))
    (sort commands #'string-lessp)))

(defun my/hyper-execute-command ()
  "Read and execute an interactive `my/' command with independent history."
  (interactive)
  (if (and my/hyper-command-active (active-minibuffer-window))
      (select-window (active-minibuffer-window))
    (let ((my/hyper-command-active t))
      (let* ((commands (my/hyper-command-candidates))
             (command-name
              (completing-read "My M-x: " commands nil t "my/ "
                               'my/hyper-command-history))
             (command (intern command-name)))
        (setq prefix-arg current-prefix-arg)
        (command-execute command 'record)))))

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
      ("x" "my M-x" my/hyper-execute-command :transient transient--do-exit)]
     ["More"
      ("m" "git menu" my/git-dispatch :transient transient--do-exit)
      ("s" "merge conflict" my/smerge-dispatch :transient transient--do-exit)
      ("n" "problems project" my/problems-project :transient transient--do-exit)
      ("J" "jupyter menu" my/jupyter-dispatch :transient transient--do-exit)
      ("R" "remote board" my/hyper-open-remote-connectboard-dispatch :transient transient--do-exit)
      ("P" "project local" my/project-local-describe :transient transient--do-exit)]]))

(provide 'init-hyper)
;;; init-hyper.el ends here
