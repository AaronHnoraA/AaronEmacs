;;; init-modules.el --- The configuration's module declaration -*- lexical-binding: t; -*-

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
 :core   ;; The config registry first: every later module registers its
         ;; settings while it loads.
         (config :feature config)
         (general :feature general) (keybinds :feature aaron-keybinds)
         (utils :feature aaron-utils) (base :feature aaron-base)
         (open :feature aaron-open) (patches :feature aaron-patches)
 :os     (:when (eq system-type 'darwin) (macos :optional t))
         ;; Must follow `os/macos': it restores the Option/Command layout
         ;; that the `H-' bindings assume when the host build is Neomacs.
         (neomacs :optional t)
 :remote (framework :doctor remote-doctor) gateway
 :ui     theme
         ;; Must follow `ui/theme': the activity indicator appends itself to
         ;; the `mode-line-misc-info' that the theme has finished setting up.
         activity
 :noema  activity
 :ui     tabbar popup
 :tools  menus
 :editor evil
 :tools  lsp
         ;; tools/lsp requires these at the end of its body; declared here,
         ;; where they actually load, they are registered without moving.
 :lang   cpp rust ocaml bazel haskell python elisp
 :checkers grammar
 :lang   beancount sh java lean
 :noema  jupyter
 :lang   markdown nix web javascript
         ;; Math-fragment preview is RaTeX, configured in lang/latex; its
         ;; doctor is the entry point when a preview misbehaves.
         (latex :autoload (+latex/preview-doctor latex-preview-doctor)
                :doctor +latex/preview-doctor)
 :editor format
 :checkers problems
 :tools  symbols workspace-symbol lsp-ops
         (lsp-hub :doctor +language-server/doctor) code-actions git
 :emacs  smerge
 :editor fold
 :tools  clutch
 :lang   filetypes
 :editor navigation navigation-extra
 :emacs  dired
 :editor editing
 :emacs  files
 :app    (calendar :autoload +calendar/calendar-year)
 :os     (:when (eq system-type 'darwin)
           (macos-tools :autoload macos-tools-menu))
 :lang   (org :after org)
 :tools  (zoxide :optional t) (agent :optional t) copilot
 :completion minibuffer consult-project
 :editor snippets file-templates
 :config default
 :tools  tree-sitter
 :ui     windows
 :emacs  dirvish
 :editor expand-region
 :ui     treemacs
 :tools  project
 :ui     workspaces
 :emacs  scratch
 :tools  project-local
 :emacs  dir-locals rename
 :editor harpoon
 :ui     scrollview
 :tools  output test task run (debugger :doctor +debug/adapter-doctor)
         debug-profile
 :ui     session
 :config maintenance (config-tools :feature config-tools) performance
 :checkers diagnostics diagnostics-extra
 :config (health :doctor config-health) server background
 :noema  bridge search roam
 :term   (:when (eq system-type 'darwin) (portal :optional t))
 :editor text
 :term   shell
 :tools  connectboard (jupyter :doctor +jupyter-board/doctor)
 :checkers (spell :optional t)
 :completion search search-extra telescope
 :tools  direnv
 :editor smartparens
 :ui     rainbow-delimiters
 :editor avy multiple-cursors
 :lang   (auctex :after (tex tex-site pdf-tools pdf-view)
                 :autoload (latex-preview-menu
                            +latex/preview-open-pdf
                            +latex/sync-forward
                            +latex/sync-forward-mouse
                            +latex/preview-compile-and-view
                            +latex/preview-view-pdf))
 :app    (browser :optional t)
 :completion (fzf :optional t)
 :config function-keys hyper mouse compile)

(aaron-modules-load)

(aaron-package-register-vc 'joplin-mode
                        '(:url "https://github.com/cinsk/joplin-mode.git"
                          :rev :last-release))

(provide 'init-modules)
;;; init-modules.el ends here
