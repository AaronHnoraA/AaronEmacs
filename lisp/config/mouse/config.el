;;; config/mouse/config.el --- Mouse-triggered shortcut layer -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reserve `H-C-M-' combinations for mouse software that can only emit simple
;; shortcuts. These bindings are intentionally kept out of the regular keyboard
;; workflow.

;;; Code:

(require 'easymenu)

(autoload 'ibuffer-jump "ibuffer" nil t)
(autoload 'ace-window "ace-window" nil t)
(autoload 'ibuffer-visit-buffer "ibuffer" nil t)
(autoload 'ibuffer-visit-buffer-other-window "ibuffer" nil t)
(autoload 'ibuffer "ibuffer" nil t)
(autoload 'project-find-regexp "project" nil t)
(autoload 'recentf-open-files "recentf" nil t)

(declare-function browse-at-remote "browse-at-remote" (&optional kill))
(declare-function browse-at-remote-kill "browse-at-remote" ())
(declare-function diff-hl-next-hunk "diff-hl" (&optional backward))
(declare-function diff-hl-previous-hunk "diff-hl" ())
(declare-function diff-hl-revert-hunk "diff-hl" ())
(declare-function diff-hl-show-hunk "diff-hl-show-hunk" ())
(declare-function diff-hl-stage-current-hunk "diff-hl" ())
(declare-function lsp-ui-doc-glance "lsp-ui-doc" ())
(declare-function lsp-describe-thing-at-point "lsp-mode" ())
(declare-function magit-dispatch "magit" ())
(declare-function magit-file-dispatch "magit" ())
(declare-function magit-status "magit" (&optional directory))
(declare-function dape "dape" (config &optional skip-compile))
(declare-function dape-breakpoint-toggle "dape" ())
(declare-function dape-continue "dape" (conn))
(declare-function dape-next "dape" (conn))
(declare-function dape-repl "dape" ())
(declare-function code-actions-menu "tools/code-actions/config" ())
(declare-function +current-language-server-backend "tools/lsp/config" ())
(declare-function debug-menu "tools/debugger/config" ())
(declare-function +diagnostics/buffer-ui "checkers/diagnostics/config" ())
(declare-function diagnostics-menu "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/project-ui "checkers/diagnostics/config" ())
(declare-function +git/board "tools/git/board" ())
(declare-function +git-diff/file-with-branch-base "tools/git/diff" ())
(declare-function +git-diff/file-with-head "tools/git/diff" ())
(declare-function git-menu "tools/git/core" ())
(declare-function +git/file-blame-toggle "tools/git/core" ())
(declare-function +git/file-log "tools/git/core" ())
(declare-function +git/stage-current-file "tools/git/core" ())
(declare-function +git/tree "tools/git/tree" ())
(declare-function +git/unstage-current-file "tools/git/core" ())
(declare-function +windows/kill-buffer-dwim "ui/windows/config" ())
(declare-function +language-server/code-actions "tools/lsp/config" ())
(declare-function +language-server/describe-session "tools/lsp-ops/config" ())
(declare-function +language-server/ensure "tools/lsp/config" ())
(declare-function +language-server/format-buffer "tools/lsp/config" ())
(declare-function +language-server/manager "tools/lsp-hub/config" ())
(declare-function +language-server/open-log "tools/lsp-ops/config" ())
(declare-function +language-server/organize-imports "tools/lsp-ops/config" ())
(declare-function +language-server/rename "tools/lsp/config" ())
(declare-function +language-server/restart "tools/lsp-ops/config" ())
(declare-function +language-server/workspace-symbol "tools/workspace-symbol/config" ())
(declare-function +navigation/back "editor/navigation/config" ())
(declare-function +navigation/beginning-of-defun "editor/navigation/config" ())
(declare-function +navigation/end-of-defun "editor/navigation/config" ())
(declare-function +navigation/find-definition "editor/navigation/config" ())
(declare-function +navigation/find-implementation "editor/navigation/config" ())
(declare-function +navigation/find-references "editor/navigation/config" ())
(declare-function +navigation/find-type-definition "editor/navigation/config" ())
(declare-function +navigation/forward "editor/navigation/config" ())
(declare-function +navigation/next-defun "editor/navigation/config" ())
(declare-function +navigation/peek-definition "editor/navigation/config" ())
(declare-function +navigation/peek-references "editor/navigation/config" ())
(declare-function +navigation/previous-defun "editor/navigation/config" ())
(declare-function structure-up "editor/navigation/config" ())
(declare-function +problems/buffer "checkers/problems/config" ())
(declare-function +problems/project "checkers/problems/config" ())
(declare-function +project-current-root "tools/project/config" ())
(declare-function project-menu "tools/project/config" ())
(declare-function +project/open-root "tools/project/config" (project-root))
(declare-function +symbols/buffer "tools/symbols/config" ())
(declare-function +symbols/project "tools/symbols/config" ())
(declare-function project-vc-dir "project" ())
(declare-function vc-refresh-state "vc-hooks" ())

(defvar vc-mode)
(defvar vc-mode-line-map)

(defvar +mouse-git-mode-line-map nil
  "Mode-line keymap used for the VCS segment.")

(defun +mouse--event-window (event)
  "Return the window associated with mouse EVENT."
  (let* ((posn (and event (event-start event)))
         (window (and posn (posn-window posn))))
    (if (windowp window)
        window
      (selected-window))))

(defun +mouse--select-event-window (event)
  "Select the window associated with mouse EVENT."
  (let ((window (+mouse--event-window event)))
    (when (window-live-p window)
      (select-window window))
    window))

(defun +mouse--set-point-from-event (event)
  "Select EVENT's window and move point to the clicked text position."
  (let* ((window (+mouse--select-event-window event))
         (posn (and event (event-start event)))
         (point (and posn (posn-point posn))))
    (when (and (window-live-p window)
               (integer-or-marker-p point))
      (with-selected-window window
        (goto-char point)))
    window))

(defun +mouse--call-at-event (event command)
  "Move point to EVENT and call COMMAND interactively."
  (+mouse--set-point-from-event event)
  (call-interactively command))

(defun +mouse--command-enabled-p (command &optional extra)
  "Return non-nil when COMMAND exists and EXTRA does not disable it."
  (and (fboundp command) extra))

(defun +mouse--menu-item (label command &optional enabled)
  "Return an easy-menu vector for LABEL and COMMAND."
  (vector label command (+mouse--command-enabled-p command enabled)))

(defun +mouse--source-symbol-p ()
  "Return non-nil when point appears to be on a source symbol."
  (and (thing-at-point 'symbol t) t))

(defun +mouse--language-server-active-p ()
  "Return non-nil when the current buffer has an active language server."
  (and (fboundp '+current-language-server-backend)
       (+current-language-server-backend)
       t))

(defun +mouse--language-server-buffer-p ()
  "Return non-nil when language-server commands make sense here."
  (derived-mode-p 'prog-mode))

(defun +mouse--git-context-p ()
  "Return non-nil when Git commands are likely to work here.
This is only used while building click menus."
  (or (bound-and-true-p vc-mode)
      (and default-directory
           (not (file-remote-p default-directory))
           (locate-dominating-file default-directory ".git"))))

(defun +mouse/ibuffer-visit-buffer-mouse (event)
  "Visit the buffer at the clicked ibuffer row for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer))

(defun +mouse/ibuffer-visit-buffer-other-window-mouse (event)
  "Visit the buffer at the clicked ibuffer row in another window for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer-other-window))

(defun +mouse/recent-files ()
  "Open the built-in clickable recent-files dialog."
  (interactive)
  (recentf-mode 1)
  (recentf-open-files))

(defun +mouse/project-root ()
  "Open the current project root in a clickable directory view."
  (interactive)
  (if-let* ((project-root (and (fboundp '+project-current-root)
                               (+project-current-root))))
      (+project/open-root project-root)
    (call-interactively #'project-menu)))

(defun +mouse/project-search (regexp)
  "Search the current project for REGEXP and show clickable results."
  (interactive
   (list
    (read-regexp
     "Project regexp: "
     (thing-at-point 'symbol t))))
  (project-find-regexp regexp))

(defun +mouse--git-menu-items ()
  "Return Git popup menu items for the current buffer."
  (let ((git-p (+mouse--git-context-p))
        (file-p (and buffer-file-name (+mouse--git-context-p))))
    (list
     (list
      "Repository"
      (+mouse--menu-item "Magit status" 'magit-status git-p)
      (+mouse--menu-item "Git workbench" 'git-menu git-p)
      (+mouse--menu-item "Magit dispatch" 'magit-dispatch git-p)
      (+mouse--menu-item "VC directory" 'project-vc-dir git-p)
      (+mouse--menu-item "Git board" '+git/board git-p)
      (+mouse--menu-item "Git tree" '+git/tree git-p))
     (list
      "Current File"
      (+mouse--menu-item "Open changes vs HEAD" '+git-diff/file-with-head file-p)
      (+mouse--menu-item "Open changes vs branch base" '+git-diff/file-with-branch-base file-p)
      (+mouse--menu-item "File history" '+git/file-log file-p)
      (+mouse--menu-item "Blame toggle" '+git/file-blame-toggle file-p)
      (+mouse--menu-item "Stage file" '+git/stage-current-file file-p)
      (+mouse--menu-item "Unstage file" '+git/unstage-current-file file-p)
      (+mouse--menu-item "Magit file menu" 'magit-file-dispatch file-p))
     (list
      "Hunk"
      (+mouse--menu-item "Show hunk" 'diff-hl-show-hunk file-p)
      (+mouse--menu-item "Previous hunk" 'diff-hl-previous-hunk file-p)
      (+mouse--menu-item "Next hunk" 'diff-hl-next-hunk file-p)
      (+mouse--menu-item "Stage hunk" 'diff-hl-stage-current-hunk file-p)
      (+mouse--menu-item "Revert hunk" 'diff-hl-revert-hunk file-p))
     (list
      "Remote"
      (+mouse--menu-item "Copy remote URL" 'browse-at-remote-kill file-p)
      (+mouse--menu-item "Open remote URL" 'browse-at-remote file-p)))))

(defun +mouse/git-mode-line-menu (event)
  "Show a Git popup menu for the VCS mode-line segment under EVENT."
  (interactive "e")
  (+mouse--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    (format "Git: %s" (or (and (bound-and-true-p vc-mode)
                               (substring-no-properties vc-mode))
                          (buffer-name)))
    (+mouse--git-menu-items))
   event))

(defun +mouse--patch-vc-mode-line-map (&rest _)
  "Patch the current buffer's `vc-mode' string with the local Git mouse map."
  (when (and +mouse-git-mode-line-map
             (stringp vc-mode)
             (> (length vc-mode) 0))
    (add-text-properties
     0 (length vc-mode)
     `(local-map ,+mouse-git-mode-line-map
                 keymap ,+mouse-git-mode-line-map
                 mouse-face mode-line-highlight
                 help-echo "Git status\nmouse-1: Git menu\nmouse-3: Git menu")
     vc-mode)))

(defun +mouse-setup-vc-mode-line-map ()
  "Install a robust Git mouse map for the VCS mode-line segment."
  (setq +mouse-git-mode-line-map
        (let ((map (make-sparse-keymap)))
          (define-key map [mode-line down-mouse-1] #'+mouse/git-mode-line-menu)
          (define-key map [mode-line mouse-1] #'ignore)
          (define-key map [mode-line down-mouse-3] #'+mouse/git-mode-line-menu)
          (define-key map [mode-line mouse-3] #'ignore)
          map))
  (when (boundp 'vc-mode-line-map)
    (setq vc-mode-line-map +mouse-git-mode-line-map))
  (when (and (fboundp 'vc-mode-line)
             (not (advice-member-p #'+mouse--patch-vc-mode-line-map
                                    #'vc-mode-line)))
    (advice-add #'vc-mode-line :after #'+mouse--patch-vc-mode-line-map))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (+mouse--patch-vc-mode-line-map))))

(defun +mouse/find-definition (event)
  "Jump to the definition at the clicked source position."
  (interactive "e")
  (+mouse--call-at-event event #'+navigation/find-definition))

(defun +mouse/find-references (event)
  "Find references at the clicked source position."
  (interactive "e")
  (+mouse--call-at-event event #'+navigation/find-references))

(defun +mouse/code-actions (event)
  "Open code actions at the clicked source position."
  (interactive "e")
  (+mouse--set-point-from-event event)
  (cond
   ((fboundp 'code-actions-menu)
    (call-interactively #'code-actions-menu))
   ((fboundp '+language-server/code-actions)
    (call-interactively #'+language-server/code-actions))
   (t
    (user-error "No code action command is available"))))

(defun +mouse/doc-at-point (event)
  "Show documentation for the clicked source position."
  (interactive "e")
  (+mouse--set-point-from-event event)
  (cond
   ((fboundp 'lsp-ui-doc-glance)
    (call-interactively #'lsp-ui-doc-glance))
   ((fboundp 'lsp-describe-thing-at-point)
    (call-interactively #'lsp-describe-thing-at-point))
   (t
    (user-error "No documentation command is available"))))

(defun +mouse--source-menu-items ()
  "Return source-buffer popup menu items.
The menu is built only on click; there is no per-buffer scanning."
  (let ((symbol-p (+mouse--source-symbol-p))
        (lsp-active-p (+mouse--language-server-active-p))
        (lsp-buffer-p (+mouse--language-server-buffer-p)))
    (list
     (list
      "Navigate"
      (+mouse--menu-item "Definition" '+navigation/find-definition symbol-p)
      (+mouse--menu-item "References" '+navigation/find-references symbol-p)
      (+mouse--menu-item "Implementation" '+navigation/find-implementation lsp-active-p)
      (+mouse--menu-item "Type definition" '+navigation/find-type-definition lsp-active-p)
      (+mouse--menu-item "Peek definition" '+navigation/peek-definition symbol-p)
      (+mouse--menu-item "Peek references" '+navigation/peek-references symbol-p)
      (+mouse--menu-item "Back" '+navigation/back t)
      (+mouse--menu-item "Forward" '+navigation/forward t))
     (list
      "Function"
      (+mouse--menu-item "Beginning of defun" '+navigation/beginning-of-defun t)
      (+mouse--menu-item "End of defun" '+navigation/end-of-defun t)
      (+mouse--menu-item "Previous defun" '+navigation/previous-defun t)
      (+mouse--menu-item "Next defun" '+navigation/next-defun t)
      (+mouse--menu-item "Up structure" 'structure-up t)
      (+mouse--menu-item "Buffer symbols" '+symbols/buffer t)
      (+mouse--menu-item "Project symbols" '+symbols/project t))
     (list
      "Language Server"
      (+mouse--menu-item "Ensure / connect" '+language-server/ensure lsp-buffer-p)
      (+mouse--menu-item "Hub" '+language-server/manager lsp-buffer-p)
      (+mouse--menu-item "Code actions" '+language-server/code-actions lsp-active-p)
      (+mouse--menu-item "Rename symbol" '+language-server/rename lsp-active-p)
      (+mouse--menu-item "Format buffer" '+language-server/format-buffer lsp-active-p)
      (+mouse--menu-item "Organize imports" '+language-server/organize-imports lsp-active-p)
      (+mouse--menu-item "Restart" '+language-server/restart lsp-active-p)
      (+mouse--menu-item "Open log" '+language-server/open-log lsp-active-p)
      (+mouse--menu-item "Describe session" '+language-server/describe-session lsp-active-p))
     (list
      "Debug"
      (+mouse--menu-item "Debug menu" 'debug-menu lsp-buffer-p)
      (+mouse--menu-item "Start / choose config" 'dape lsp-buffer-p)
      (+mouse--menu-item "Toggle breakpoint" 'dape-breakpoint-toggle lsp-buffer-p)
      (+mouse--menu-item "Continue" 'dape-continue lsp-buffer-p)
      (+mouse--menu-item "Step over" 'dape-next lsp-buffer-p)
      (+mouse--menu-item "REPL" 'dape-repl lsp-buffer-p))
     (list
      "Diagnostics"
      (+mouse--menu-item "Buffer problems" '+problems/buffer t)
      (+mouse--menu-item "Project problems" '+problems/project t)
      (+mouse--menu-item "Buffer diagnostics UI" '+diagnostics/buffer-ui t)
      (+mouse--menu-item "Project diagnostics UI" '+diagnostics/project-ui t)
      (+mouse--menu-item "Diagnostics menu" 'diagnostics-menu t))
     (cons "Git" (+mouse--git-menu-items))
     (list
      "Project / Window"
      (+mouse--menu-item "Workspace symbol" '+language-server/workspace-symbol lsp-active-p)
      (+mouse--menu-item "Project board" 'project-menu t)
      (+mouse--menu-item "Project root" '+mouse/project-root t)
      (+mouse--menu-item "Project grep" '+mouse/project-search t)
      (+mouse--menu-item "Ace window" 'ace-window t)))))

(defun +mouse/source-menu (event)
  "Show the source-buffer right-click menu for EVENT."
  (interactive "e")
  (+mouse--set-point-from-event event)
  (popup-menu
   (easy-menu-create-menu
    (format "Source: %s" (buffer-name))
    (+mouse--source-menu-items))
   event))

(defun +mouse/text-menu (event)
  "Show the standard context menu at EVENT in a text buffer."
  (interactive "e")
  (+mouse--set-point-from-event event)
  (popup-menu (context-menu-map event) event))

(defun +mouse/mode-line-primary (event)
  "Select the mode-line window clicked by EVENT.
Dedicated status segments handle their own actions."
  (interactive "e")
  (+mouse--select-event-window event))

(defun +mouse/mode-line-code-menu (event)
  "Open a code-oriented menu from the mode line at EVENT."
  (interactive "e")
  (+mouse--select-event-window event)
  (cond
   ((derived-mode-p 'prog-mode)
    (if (fboundp 'code-actions-menu)
        (call-interactively #'code-actions-menu)
      (call-interactively #'+language-server/manager)))
   (t
    (call-interactively #'mouse-actions-menu))))

(defun +mouse--mode-line-menu-items ()
  "Return mode-line popup menu items for the selected window."
  (let ((prog-p (derived-mode-p 'prog-mode))
        (lsp-active-p (+mouse--language-server-active-p)))
    (list
     (list
      "Buffer"
      (+mouse--menu-item "Ibuffer jump" 'ibuffer-jump t)
      (+mouse--menu-item "Buffer list" 'ibuffer t)
      (+mouse--menu-item "Kill buffer" '+windows/kill-buffer-dwim t)
      (+mouse--menu-item "Recent files" '+mouse/recent-files t))
     (list
      "Language Server"
      (+mouse--menu-item "Hub" '+language-server/manager prog-p)
      (+mouse--menu-item "Ensure / connect" '+language-server/ensure prog-p)
      (+mouse--menu-item "Code menu" 'code-actions-menu prog-p)
      (+mouse--menu-item "Code actions" '+language-server/code-actions lsp-active-p)
      (+mouse--menu-item "Rename symbol" '+language-server/rename lsp-active-p)
      (+mouse--menu-item "Format buffer" '+language-server/format-buffer lsp-active-p)
      (+mouse--menu-item "Restart" '+language-server/restart lsp-active-p)
      (+mouse--menu-item "Log" '+language-server/open-log lsp-active-p))
     (list
      "Debug"
      (+mouse--menu-item "Debug menu" 'debug-menu prog-p)
      (+mouse--menu-item "Start / choose config" 'dape prog-p)
      (+mouse--menu-item "Toggle breakpoint" 'dape-breakpoint-toggle prog-p)
      (+mouse--menu-item "REPL" 'dape-repl prog-p))
     (list
      "Diagnostics / Project"
      (+mouse--menu-item "Buffer problems" '+problems/buffer t)
      (+mouse--menu-item "Project problems" '+problems/project t)
      (+mouse--menu-item "Diagnostics menu" 'diagnostics-menu t)
      (+mouse--menu-item "Project board" 'project-menu t)
      (+mouse--menu-item "Project root" '+mouse/project-root t))
     (cons "Git" (+mouse--git-menu-items))
     (list
      "Jump / Window"
      (+mouse--menu-item "Definition" '+navigation/find-definition prog-p)
      (+mouse--menu-item "References" '+navigation/find-references prog-p)
      (+mouse--menu-item "Back" '+navigation/back t)
      (+mouse--menu-item "Forward" '+navigation/forward t)
      (+mouse--menu-item "Ace window" 'ace-window t)))))

(defun +mouse/mode-line-menu (event)
  "Show a mode-line/header-line right-click menu for EVENT."
  (interactive "e")
  (+mouse--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    (format "Status: %s" (buffer-name))
    (+mouse--mode-line-menu-items))
   event))

(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-name-map "<mouse-1>" #'+mouse/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-name-map "<mouse-2>" #'+mouse/ibuffer-visit-buffer-other-window-mouse)
  (keymap-set ibuffer-mode-map "<mouse-1>" #'+mouse/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-mode-map "<mouse-2>" #'+mouse/ibuffer-visit-buffer-other-window-mouse))

(keymap-set prog-mode-map "C-<mouse-1>" #'+mouse/find-definition)
(keymap-set prog-mode-map "M-<mouse-1>" #'+mouse/code-actions)
(keymap-set prog-mode-map "S-<mouse-1>" #'+mouse/doc-at-point)
(keymap-set prog-mode-map "<mouse-3>" #'+mouse/source-menu)
(keymap-set text-mode-map "<mouse-3>" #'+mouse/text-menu)

(with-eval-after-load 'lsp-mode
  (keymap-set lsp-mode-map "C-<mouse-1>" #'+mouse/find-definition)
  (keymap-set lsp-mode-map "M-<mouse-1>" #'+mouse/code-actions)
  (keymap-set lsp-mode-map "S-<mouse-1>" #'+mouse/doc-at-point)
  (keymap-set lsp-mode-map "<mouse-3>" #'+mouse/source-menu))

(global-set-key [mode-line mouse-1] #'+mouse/mode-line-primary)
(global-set-key [mode-line C-mouse-1] #'+mouse/mode-line-code-menu)
(global-set-key [mode-line mouse-3] #'+mouse/mode-line-menu)
(global-set-key [header-line C-mouse-1] #'+mouse/mode-line-code-menu)
(global-set-key [header-line mouse-3] #'+mouse/mode-line-menu)

(with-eval-after-load 'vc-hooks
  (+mouse-setup-vc-mode-line-map))

(when (featurep 'vc-hooks)
  (+mouse-setup-vc-mode-line-map))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix mouse-actions-menu ()
    "Mouse-friendly command board. Every suffix can be triggered by click."
    [["Buffers"
      ("a" "ibuffer jump" ibuffer-jump :transient transient--do-exit)
      ("b" "buffer list" ibuffer :transient transient--do-exit)
      ("k" "kill buffer" +windows/kill-buffer-dwim :transient transient--do-exit)
      ("r" "recent files" +mouse/recent-files :transient transient--do-exit)]
     ["Project / Search"
      ("f" "project root" +mouse/project-root :transient transient--do-exit)
      ("g" "project grep" +mouse/project-search :transient transient--do-exit)
      ("p" "project board" project-menu :transient transient--do-exit)]
     ["Code / Status"
      ("x" "code menu" code-actions-menu :transient transient--do-exit)
      ("l" "language server" +language-server/manager :transient transient--do-exit)
      ("!" "diagnostics" diagnostics-menu :transient transient--do-exit)
      ("." "git workbench" git-menu :transient transient--do-exit)
      ("v" "magit status" magit-status :transient transient--do-exit)]
     ["Jump / Window"
      ("d" "definition" +navigation/find-definition :transient transient--do-exit)
      ("z" "jump back" +navigation/back :transient transient--do-exit)
      ("w" "ace window" ace-window :transient transient--do-exit)]]))

;; Mouse-friendly direct actions. Logitech MX Master style buttons work best
;; when one press maps to one immediate command.
(global-set-key (kbd "H-C-M-a") #'ibuffer-jump)
(global-set-key (kbd "H-C-M-b") #'ibuffer)
(global-set-key (kbd "H-C-M-d") #'+navigation/find-definition)
(global-set-key (kbd "H-C-M-f") #'+mouse/project-root)
(global-set-key (kbd "H-C-M-g") #'+mouse/project-search)
(global-set-key (kbd "H-C-M-k") #'+windows/kill-buffer-dwim)
(global-set-key (kbd "H-C-M-m") #'mouse-actions-menu)
(global-set-key (kbd "H-C-M-p") #'project-menu)
(global-set-key (kbd "H-C-M-r") #'+mouse/recent-files)
(global-set-key (kbd "H-C-M-w") #'ace-window)
(global-set-key (kbd "H-C-M-z") #'+navigation/back)

(provide 'config/mouse/config)
;;; config/mouse/config.el ends here
