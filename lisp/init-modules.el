;;; init-modules.el --- Load init modules in order -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; The module system lives in lisp/core/aaron-modules.el.  This file is the
;; configuration's `aaron!' declaration: every module, its category and its
;; load position.  Categories follow Doom's, plus :remote (the foundation
;; layer, loaded right after :core and :os) and :noema.

(require 'aaron-lib)
(require 'aaron-emacs)
(require 'aaron-modules)
(autoload 'aaron/modules "aaron-modules-board" nil t)
(autoload 'aaron/describe-module "aaron-modules-board" nil t)
(autoload 'aaron/doctor "aaron-modules-board" nil t)
(autoload 'module-board "aaron-modules-board" nil t)
(autoload 'module-doctor "aaron-modules-board" nil t)

(aaron!
 :core   init-utils init-base init-open patches/init-patches
 :os     (:when (eq system-type 'darwin) (init-macos :optional t))
         ;; Must follow `init-macos': it restores the Option/Command layout
         ;; that the `H-' bindings assume when the host build is Neomacs.
         (init-neomacs :optional t)
 :remote (init-remote :doctor remote-doctor) init-emacs-gateway
 :ui     init-ui
         ;; Must follow `init-ui': the activity indicator appends itself to
         ;; the `mode-line-misc-info' that init-ui has finished setting up.
         init-activity
 :noema  init-activity-noema
 :ui     init-tabbar init-display-rules
 :tools  init-tools
 :editor init-evil
 :tools  init-lsp
 :editor init-format
 :checkers init-problems
 :tools  init-symbols init-workspace-symbol init-lsp-ops
         (init-lsp-tools :doctor my/language-server-doctor)
         init-code-actions init-git
 :emacs  init-smerge
 :tools  init-dev init-clutch
 :lang   init-filetypes
 :editor init-navigation init-navigation-extra
 :emacs  init-dired
 :editor init-editing-workbench
 :emacs  init-file-workbench
 :app    (init-calendar-tools :autoload my/calendar-year)
 :os     (:when (eq system-type 'darwin)
           (init-macos-tools :autoload macos-tools-menu))
 :lang   (init-org-tools :after org)
 :tools  (init-zoxide :optional t) (init-ai-ide :optional t) init-copilot
 :completion init-minibuffer init-consult-project
 :editor init-snippets init-auto-insert
 :config init-doom-extra
 :tools  init-treesit
 :ui     init-windows
 :emacs  init-dirvish-keys
 :editor init-expand-region
 :ui     init-treemacs-bridge
 :tools  init-project
 :ui     init-workspaces
 :emacs  init-scratch
 :tools  init-project-local
 :emacs  init-dir-locals init-rename
 :editor init-harpoon
 :ui     init-scrollview
 :tools  init-output init-test init-task init-project-run (init-debug :doctor my/debug-adapter-doctor)
         init-debug-profile
 :ui     init-session
 :config init-maintenance config-tools init-performance
 :checkers init-diagnostics-ui init-diagnostics-extra
 :config (init-health :doctor config-health) init-server init-background
 :lang   init-md
 :noema  init-noema init-noema-search init-md-roam
 :term   (:when (eq system-type 'darwin) (init-portal :optional t))
 :editor init-text
 :term   init-shell
 :tools  init-remote-connectboard
         (init-jupyter-board :doctor my/jupyter-board-doctor)
 :checkers (init-spell :optional t)
 :completion init-search init-search-extra init-telescope
 :tools  init-direnv
 :editor init-smartparens
 :ui     init-rainbow-delimiters
 :editor init-avy init-multiple-cursors
 :lang   (init-auctex :after (tex tex-site pdf-tools pdf-view)
                      :autoload (latex-preview-menu
                                 my/latex-preview-open-pdf
                                 my/latex-sync-forward
                                 my/latex-sync-forward-mouse
                                 my/latex-preview-compile-and-view
                                 my/latex-preview-view-pdf))
         ;; Math-fragment preview is RaTeX, configured in init-latex.el; its
         ;; doctor is the entry point when a preview misbehaves.
         (init-latex :autoload (my/latex-preview-doctor latex-preview-doctor)
                     :doctor my/latex-preview-doctor)
 :app    (init-browser :optional t)
 :completion (init-fzfs :optional t)
 :config init-function-keys init-hyper init-mouse init-ignored)

(aaron-modules-load)

(my/package-register-vc 'joplin-mode
                        '(:url "https://github.com/cinsk/joplin-mode.git"
                          :rev :last-release))

(provide 'init-modules)
;;; init-modules.el ends here
