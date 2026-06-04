;;; init-mouse.el --- Mouse-triggered shortcut layer -*- lexical-binding: t; -*-

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
(declare-function eldoc-doc-buffer "eldoc" ())
(declare-function eldoc-box-help-at-point "eldoc-box" ())
(declare-function magit-dispatch "magit" ())
(declare-function magit-file-dispatch "magit" ())
(declare-function magit-status "magit" (&optional directory))
(declare-function my/code-actions-dispatch "init-code-actions" ())
(declare-function my/current-language-server-backend "init-lsp" ())
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui" ())
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/diagnostics-project-ui "init-diagnostics-ui" ())
(declare-function my/git-board "init-git-board" ())
(declare-function my/git-diff-file-with-branch-base "init-git-diff" ())
(declare-function my/git-diff-file-with-head "init-git-diff" ())
(declare-function my/git-dispatch "init-git-core" ())
(declare-function my/git-file-blame-toggle "init-git-core" ())
(declare-function my/git-file-log "init-git-core" ())
(declare-function my/git-stage-current-file "init-git-core" ())
(declare-function my/git-tree "init-git-tree" ())
(declare-function my/git-unstage-current-file "init-git-core" ())
(declare-function my/kill-buffer-dwim "init-windows" ())
(declare-function my/language-server-code-actions "init-lsp" ())
(declare-function my/language-server-describe-session "init-lsp-ops" ())
(declare-function my/language-server-ensure "init-lsp" ())
(declare-function my/language-server-format-buffer "init-lsp" ())
(declare-function my/language-server-manager "init-lsp-tools" ())
(declare-function my/language-server-open-log "init-lsp-ops" ())
(declare-function my/language-server-organize-imports "init-lsp-ops" ())
(declare-function my/language-server-rename "init-lsp" ())
(declare-function my/language-server-restart "init-lsp-ops" ())
(declare-function my/language-server-workspace-symbol "init-workspace-symbol" ())
(declare-function my/navigation-back "init-navigation" ())
(declare-function my/navigation-beginning-of-defun "init-navigation" ())
(declare-function my/navigation-end-of-defun "init-navigation" ())
(declare-function my/navigation-find-definition "init-navigation" ())
(declare-function my/navigation-find-implementation "init-navigation" ())
(declare-function my/navigation-find-references "init-navigation" ())
(declare-function my/navigation-find-type-definition "init-navigation" ())
(declare-function my/navigation-forward "init-navigation" ())
(declare-function my/navigation-next-defun "init-navigation" ())
(declare-function my/navigation-peek-definition "init-navigation" ())
(declare-function my/navigation-peek-references "init-navigation" ())
(declare-function my/navigation-previous-defun "init-navigation" ())
(declare-function my/navigation-up-structure "init-navigation" ())
(declare-function my/problems-buffer "init-problems" ())
(declare-function my/problems-project "init-problems" ())
(declare-function my/project-current-root "init-project" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/project-open-root "init-project" (project-root))
(declare-function my/symbols-buffer "init-symbols" ())
(declare-function my/symbols-project "init-symbols" ())
(declare-function project-vc-dir "project" ())
(declare-function vc-refresh-state "vc-hooks" ())

(defvar vc-mode)
(defvar vc-mode-line-map)

(defvar my/mouse--git-mode-line-menu-last-time 0
  "Timestamp of the last Git mode-line popup.
Used to suppress duplicate down/up events from the same click.")

(defvar my/mouse-git-mode-line-map nil
  "Mode-line keymap used for the VCS segment.")

(defun my/mouse--event-window (event)
  "Return the window associated with mouse EVENT."
  (let* ((posn (and event (event-start event)))
         (window (and posn (posn-window posn))))
    (if (windowp window)
        window
      (selected-window))))

(defun my/mouse--select-event-window (event)
  "Select the window associated with mouse EVENT."
  (let ((window (my/mouse--event-window event)))
    (when (window-live-p window)
      (select-window window))
    window))

(defun my/mouse--set-point-from-event (event)
  "Select EVENT's window and move point to the clicked text position."
  (let* ((window (my/mouse--select-event-window event))
         (posn (and event (event-start event)))
         (point (and posn (posn-point posn))))
    (when (and (window-live-p window)
               (integer-or-marker-p point))
      (with-selected-window window
        (goto-char point)))
    window))

(defun my/mouse--call-at-event (event command)
  "Move point to EVENT and call COMMAND interactively."
  (my/mouse--set-point-from-event event)
  (call-interactively command))

(defun my/mouse--command-enabled-p (command &optional extra)
  "Return non-nil when COMMAND exists and EXTRA does not disable it."
  (and (fboundp command) extra))

(defun my/mouse--menu-item (label command &optional enabled)
  "Return an easy-menu vector for LABEL and COMMAND."
  (vector label command (my/mouse--command-enabled-p command enabled)))

(defun my/mouse--source-symbol-p ()
  "Return non-nil when point appears to be on a source symbol."
  (and (thing-at-point 'symbol t) t))

(defun my/mouse--language-server-active-p ()
  "Return non-nil when the current buffer has an active language server."
  (and (fboundp 'my/current-language-server-backend)
       (my/current-language-server-backend)
       t))

(defun my/mouse--language-server-buffer-p ()
  "Return non-nil when language-server commands make sense here."
  (derived-mode-p 'prog-mode))

(defun my/mouse--git-context-p ()
  "Return non-nil when Git commands are likely to work here.
This is only used while building click menus."
  (or (bound-and-true-p vc-mode)
      (and default-directory
           (not (file-remote-p default-directory))
           (locate-dominating-file default-directory ".git"))))

(defun my/ibuffer-visit-buffer-mouse (event)
  "Visit the buffer at the clicked ibuffer row for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer))

(defun my/ibuffer-visit-buffer-other-window-mouse (event)
  "Visit the buffer at the clicked ibuffer row in another window for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer-other-window))

(defun my/mouse-recent-files ()
  "Open the built-in clickable recent-files dialog."
  (interactive)
  (recentf-mode 1)
  (recentf-open-files))

(defun my/mouse-project-root ()
  "Open the current project root in a clickable directory view."
  (interactive)
  (if-let* ((project-root (and (fboundp 'my/project-current-root)
                               (my/project-current-root))))
      (my/project-open-root project-root)
    (call-interactively #'my/project-dispatch)))

(defun my/mouse-project-search (regexp)
  "Search the current project for REGEXP and show clickable results."
  (interactive
   (list
    (read-regexp
     "Project regexp: "
     (thing-at-point 'symbol t))))
  (project-find-regexp regexp))

(defun my/mouse--git-menu-items ()
  "Return Git popup menu items for the current buffer."
  (let ((git-p (my/mouse--git-context-p))
        (file-p (and buffer-file-name (my/mouse--git-context-p))))
    (list
     (list
      "Repository"
      (my/mouse--menu-item "Magit status" 'magit-status git-p)
      (my/mouse--menu-item "Git workbench" 'my/git-dispatch git-p)
      (my/mouse--menu-item "Magit dispatch" 'magit-dispatch git-p)
      (my/mouse--menu-item "VC directory" 'project-vc-dir git-p)
      (my/mouse--menu-item "Git board" 'my/git-board git-p)
      (my/mouse--menu-item "Git tree" 'my/git-tree git-p))
     (list
      "Current File"
      (my/mouse--menu-item "Open changes vs HEAD" 'my/git-diff-file-with-head file-p)
      (my/mouse--menu-item "Open changes vs branch base" 'my/git-diff-file-with-branch-base file-p)
      (my/mouse--menu-item "File history" 'my/git-file-log file-p)
      (my/mouse--menu-item "Blame toggle" 'my/git-file-blame-toggle file-p)
      (my/mouse--menu-item "Stage file" 'my/git-stage-current-file file-p)
      (my/mouse--menu-item "Unstage file" 'my/git-unstage-current-file file-p)
      (my/mouse--menu-item "Magit file menu" 'magit-file-dispatch file-p))
     (list
      "Hunk"
      (my/mouse--menu-item "Show hunk" 'diff-hl-show-hunk file-p)
      (my/mouse--menu-item "Previous hunk" 'diff-hl-previous-hunk file-p)
      (my/mouse--menu-item "Next hunk" 'diff-hl-next-hunk file-p)
      (my/mouse--menu-item "Stage hunk" 'diff-hl-stage-current-hunk file-p)
      (my/mouse--menu-item "Revert hunk" 'diff-hl-revert-hunk file-p))
     (list
      "Remote"
      (my/mouse--menu-item "Copy remote URL" 'browse-at-remote-kill file-p)
      (my/mouse--menu-item "Open remote URL" 'browse-at-remote file-p)))))

(defun my/mouse-git-mode-line-menu (event)
  "Show a Git popup menu for the VCS mode-line segment under EVENT."
  (interactive "e")
  (let ((now (float-time)))
    (when (> (- now my/mouse--git-mode-line-menu-last-time) 0.35)
      (setq my/mouse--git-mode-line-menu-last-time now)
      (my/mouse--select-event-window event)
      (popup-menu
       (easy-menu-create-menu
        (format "Git: %s" (or (and (bound-and-true-p vc-mode)
                                   (substring-no-properties vc-mode))
                              (buffer-name)))
        (my/mouse--git-menu-items))
       event))))

(defun my/mouse--patch-vc-mode-line-map (&rest _)
  "Patch the current buffer's `vc-mode' string with the local Git mouse map."
  (when (and my/mouse-git-mode-line-map
             (stringp vc-mode)
             (> (length vc-mode) 0))
    (add-text-properties
     0 (length vc-mode)
     `(local-map ,my/mouse-git-mode-line-map
                 keymap ,my/mouse-git-mode-line-map
                 mouse-face mode-line-highlight
                 help-echo "Git status\nmouse-1: Git menu\nmouse-3: Git menu")
     vc-mode)))

(defun my/mouse-setup-vc-mode-line-map ()
  "Install a robust Git mouse map for the VCS mode-line segment."
  (setq my/mouse-git-mode-line-map
        (let ((map (make-sparse-keymap)))
          (define-key map [mode-line down-mouse-1] #'my/mouse-git-mode-line-menu)
          (define-key map [mode-line mouse-1] #'my/mouse-git-mode-line-menu)
          (define-key map [mode-line down-mouse-3] #'my/mouse-git-mode-line-menu)
          (define-key map [mode-line mouse-3] #'my/mouse-git-mode-line-menu)
          map))
  (when (boundp 'vc-mode-line-map)
    (setq vc-mode-line-map my/mouse-git-mode-line-map))
  (when (and (fboundp 'vc-mode-line)
             (not (advice-member-p #'my/mouse--patch-vc-mode-line-map
                                    #'vc-mode-line)))
    (advice-add #'vc-mode-line :after #'my/mouse--patch-vc-mode-line-map))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/mouse--patch-vc-mode-line-map))))

(defun my/mouse-find-definition (event)
  "Jump to the definition at the clicked source position."
  (interactive "e")
  (my/mouse--call-at-event event #'my/navigation-find-definition))

(defun my/mouse-find-references (event)
  "Find references at the clicked source position."
  (interactive "e")
  (my/mouse--call-at-event event #'my/navigation-find-references))

(defun my/mouse-code-actions (event)
  "Open code actions at the clicked source position."
  (interactive "e")
  (my/mouse--set-point-from-event event)
  (cond
   ((fboundp 'my/code-actions-dispatch)
    (call-interactively #'my/code-actions-dispatch))
   ((fboundp 'my/language-server-code-actions)
    (call-interactively #'my/language-server-code-actions))
   (t
    (user-error "No code action command is available"))))

(defun my/mouse-doc-at-point (event)
  "Show documentation for the clicked source position."
  (interactive "e")
  (my/mouse--set-point-from-event event)
  (cond
   ((fboundp 'eldoc-box-help-at-point)
    (call-interactively #'eldoc-box-help-at-point))
   ((fboundp 'eldoc-doc-buffer)
    (call-interactively #'eldoc-doc-buffer))
   (t
    (user-error "No Eldoc command is available"))))

(defun my/mouse--source-menu-items ()
  "Return source-buffer popup menu items.
The menu is built only on click; there is no per-buffer scanning."
  (let ((symbol-p (my/mouse--source-symbol-p))
        (lsp-active-p (my/mouse--language-server-active-p))
        (lsp-buffer-p (my/mouse--language-server-buffer-p)))
    (list
     (list
      "Navigate"
      (my/mouse--menu-item "Definition" 'my/navigation-find-definition symbol-p)
      (my/mouse--menu-item "References" 'my/navigation-find-references symbol-p)
      (my/mouse--menu-item "Implementation" 'my/navigation-find-implementation lsp-active-p)
      (my/mouse--menu-item "Type definition" 'my/navigation-find-type-definition lsp-active-p)
      (my/mouse--menu-item "Peek definition" 'my/navigation-peek-definition symbol-p)
      (my/mouse--menu-item "Peek references" 'my/navigation-peek-references symbol-p)
      (my/mouse--menu-item "Back" 'my/navigation-back t)
      (my/mouse--menu-item "Forward" 'my/navigation-forward t))
     (list
      "Function"
      (my/mouse--menu-item "Beginning of defun" 'my/navigation-beginning-of-defun t)
      (my/mouse--menu-item "End of defun" 'my/navigation-end-of-defun t)
      (my/mouse--menu-item "Previous defun" 'my/navigation-previous-defun t)
      (my/mouse--menu-item "Next defun" 'my/navigation-next-defun t)
      (my/mouse--menu-item "Up structure" 'my/navigation-up-structure t)
      (my/mouse--menu-item "Buffer symbols" 'my/symbols-buffer t)
      (my/mouse--menu-item "Project symbols" 'my/symbols-project t))
     (list
      "Language Server"
      (my/mouse--menu-item "Ensure / connect" 'my/language-server-ensure lsp-buffer-p)
      (my/mouse--menu-item "Hub" 'my/language-server-manager lsp-buffer-p)
      (my/mouse--menu-item "Code actions" 'my/language-server-code-actions lsp-active-p)
      (my/mouse--menu-item "Rename symbol" 'my/language-server-rename lsp-active-p)
      (my/mouse--menu-item "Format buffer" 'my/language-server-format-buffer lsp-active-p)
      (my/mouse--menu-item "Organize imports" 'my/language-server-organize-imports lsp-active-p)
      (my/mouse--menu-item "Restart" 'my/language-server-restart lsp-active-p)
      (my/mouse--menu-item "Open log" 'my/language-server-open-log lsp-active-p)
      (my/mouse--menu-item "Describe session" 'my/language-server-describe-session lsp-active-p))
     (list
      "Diagnostics"
      (my/mouse--menu-item "Buffer problems" 'my/problems-buffer t)
      (my/mouse--menu-item "Project problems" 'my/problems-project t)
      (my/mouse--menu-item "Buffer diagnostics UI" 'my/diagnostics-buffer-ui t)
      (my/mouse--menu-item "Project diagnostics UI" 'my/diagnostics-project-ui t)
      (my/mouse--menu-item "Diagnostics menu" 'my/diagnostics-dispatch t))
     (cons "Git" (my/mouse--git-menu-items))
     (list
      "Project / Window"
      (my/mouse--menu-item "Workspace symbol" 'my/language-server-workspace-symbol lsp-active-p)
      (my/mouse--menu-item "Project board" 'my/project-dispatch t)
      (my/mouse--menu-item "Project root" 'my/mouse-project-root t)
      (my/mouse--menu-item "Project grep" 'my/mouse-project-search t)
      (my/mouse--menu-item "Ace window" 'ace-window t)))))

(defun my/mouse-source-menu (event)
  "Show the source-buffer right-click menu for EVENT."
  (interactive "e")
  (my/mouse--set-point-from-event event)
  (popup-menu
   (easy-menu-create-menu
    (format "Source: %s" (buffer-name))
    (my/mouse--source-menu-items))
   event))

(defun my/mouse-mode-line-primary (event)
  "Select the mode-line window clicked by EVENT.
Dedicated status segments handle their own actions."
  (interactive "e")
  (my/mouse--select-event-window event))

(defun my/mouse-mode-line-code-menu (event)
  "Open a code-oriented menu from the mode line at EVENT."
  (interactive "e")
  (my/mouse--select-event-window event)
  (cond
   ((derived-mode-p 'prog-mode)
    (if (fboundp 'my/code-actions-dispatch)
        (call-interactively #'my/code-actions-dispatch)
      (call-interactively #'my/language-server-manager)))
   (t
    (call-interactively #'my/mouse-dispatch))))

(defun my/mouse--mode-line-menu-items ()
  "Return mode-line popup menu items for the selected window."
  (let ((prog-p (derived-mode-p 'prog-mode))
        (lsp-active-p (my/mouse--language-server-active-p)))
    (list
     (list
      "Buffer"
      (my/mouse--menu-item "Ibuffer jump" 'ibuffer-jump t)
      (my/mouse--menu-item "Buffer list" 'ibuffer t)
      (my/mouse--menu-item "Kill buffer" 'my/kill-buffer-dwim t)
      (my/mouse--menu-item "Recent files" 'my/mouse-recent-files t))
     (list
      "Language Server"
      (my/mouse--menu-item "Hub" 'my/language-server-manager prog-p)
      (my/mouse--menu-item "Ensure / connect" 'my/language-server-ensure prog-p)
      (my/mouse--menu-item "Code menu" 'my/code-actions-dispatch prog-p)
      (my/mouse--menu-item "Code actions" 'my/language-server-code-actions lsp-active-p)
      (my/mouse--menu-item "Rename symbol" 'my/language-server-rename lsp-active-p)
      (my/mouse--menu-item "Format buffer" 'my/language-server-format-buffer lsp-active-p)
      (my/mouse--menu-item "Restart" 'my/language-server-restart lsp-active-p)
      (my/mouse--menu-item "Log" 'my/language-server-open-log lsp-active-p))
     (list
      "Diagnostics / Project"
      (my/mouse--menu-item "Buffer problems" 'my/problems-buffer t)
      (my/mouse--menu-item "Project problems" 'my/problems-project t)
      (my/mouse--menu-item "Diagnostics menu" 'my/diagnostics-dispatch t)
      (my/mouse--menu-item "Project board" 'my/project-dispatch t)
      (my/mouse--menu-item "Project root" 'my/mouse-project-root t))
     (cons "Git" (my/mouse--git-menu-items))
     (list
      "Jump / Window"
      (my/mouse--menu-item "Definition" 'my/navigation-find-definition prog-p)
      (my/mouse--menu-item "References" 'my/navigation-find-references prog-p)
      (my/mouse--menu-item "Back" 'my/navigation-back t)
      (my/mouse--menu-item "Forward" 'my/navigation-forward t)
      (my/mouse--menu-item "Ace window" 'ace-window t)))))

(defun my/mouse-mode-line-menu (event)
  "Show a mode-line/header-line right-click menu for EVENT."
  (interactive "e")
  (my/mouse--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    (format "Status: %s" (buffer-name))
    (my/mouse--mode-line-menu-items))
   event))

(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-name-map "<mouse-1>" #'my/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-name-map "<mouse-2>" #'my/ibuffer-visit-buffer-other-window-mouse)
  (keymap-set ibuffer-mode-map "<mouse-1>" #'my/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-mode-map "<mouse-2>" #'my/ibuffer-visit-buffer-other-window-mouse))

(keymap-set prog-mode-map "C-<mouse-1>" #'my/mouse-find-definition)
(keymap-set prog-mode-map "M-<mouse-1>" #'my/mouse-code-actions)
(keymap-set prog-mode-map "S-<mouse-1>" #'my/mouse-doc-at-point)
(keymap-set prog-mode-map "<mouse-3>" #'my/mouse-source-menu)

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "C-<mouse-1>" #'my/mouse-find-definition)
  (keymap-set eglot-mode-map "M-<mouse-1>" #'my/mouse-code-actions)
  (keymap-set eglot-mode-map "S-<mouse-1>" #'my/mouse-doc-at-point)
  (keymap-set eglot-mode-map "<mouse-3>" #'my/mouse-source-menu))

(with-eval-after-load 'lsp-mode
  (keymap-set lsp-mode-map "C-<mouse-1>" #'my/mouse-find-definition)
  (keymap-set lsp-mode-map "M-<mouse-1>" #'my/mouse-code-actions)
  (keymap-set lsp-mode-map "S-<mouse-1>" #'my/mouse-doc-at-point)
  (keymap-set lsp-mode-map "<mouse-3>" #'my/mouse-source-menu))

(global-set-key [mode-line mouse-1] #'my/mouse-mode-line-primary)
(global-set-key [mode-line C-mouse-1] #'my/mouse-mode-line-code-menu)
(global-set-key [mode-line mouse-3] #'my/mouse-mode-line-menu)
(global-set-key [header-line C-mouse-1] #'my/mouse-mode-line-code-menu)
(global-set-key [header-line mouse-3] #'my/mouse-mode-line-menu)

(with-eval-after-load 'vc-hooks
  (my/mouse-setup-vc-mode-line-map))

(when (featurep 'vc-hooks)
  (my/mouse-setup-vc-mode-line-map))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix my/mouse-dispatch ()
    "Mouse-friendly command board. Every suffix can be triggered by click."
    [["Buffers"
      ("a" "ibuffer jump" ibuffer-jump :transient transient--do-exit)
      ("b" "buffer list" ibuffer :transient transient--do-exit)
      ("k" "kill buffer" my/kill-buffer-dwim :transient transient--do-exit)
      ("r" "recent files" my/mouse-recent-files :transient transient--do-exit)]
     ["Project / Search"
      ("f" "project root" my/mouse-project-root :transient transient--do-exit)
      ("g" "project grep" my/mouse-project-search :transient transient--do-exit)
      ("p" "project board" my/project-dispatch :transient transient--do-exit)]
     ["Code / Status"
      ("x" "code menu" my/code-actions-dispatch :transient transient--do-exit)
      ("l" "language server" my/language-server-manager :transient transient--do-exit)
      ("!" "diagnostics" my/diagnostics-dispatch :transient transient--do-exit)
      ("." "git workbench" my/git-dispatch :transient transient--do-exit)
      ("v" "magit status" magit-status :transient transient--do-exit)]
     ["Jump / Window"
      ("d" "definition" my/navigation-find-definition :transient transient--do-exit)
      ("z" "jump back" my/navigation-back :transient transient--do-exit)
      ("w" "ace window" ace-window :transient transient--do-exit)]]))

;; Mouse-friendly direct actions. Logitech MX Master style buttons work best
;; when one press maps to one immediate command.
(global-set-key (kbd "H-C-M-a") #'ibuffer-jump)
(global-set-key (kbd "H-C-M-b") #'ibuffer)
(global-set-key (kbd "H-C-M-d") #'my/navigation-find-definition)
(global-set-key (kbd "H-C-M-f") #'my/mouse-project-root)
(global-set-key (kbd "H-C-M-g") #'my/mouse-project-search)
(global-set-key (kbd "H-C-M-k") #'my/kill-buffer-dwim)
(global-set-key (kbd "H-C-M-m") #'my/mouse-dispatch)
(global-set-key (kbd "H-C-M-p") #'my/project-dispatch)
(global-set-key (kbd "H-C-M-r") #'my/mouse-recent-files)
(global-set-key (kbd "H-C-M-w") #'ace-window)
(global-set-key (kbd "H-C-M-z") #'my/navigation-back)

(provide 'init-mouse)
;;; init-mouse.el ends here
