;;; init-modules.el --- Load init modules in order -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-base)
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
(require 'init-evil)
(require 'init-lsp)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-snippets)
(require 'init-treesit)
(require 'init-windows)
(require 'init-project)
(require 'init-server)

;; standalone apps
(require 'init-org)
(require 'init-org-zotero)
(require 'init-eaf)
(require 'init-text)
(require 'init-shell)
(require 'init-spell)
(require 'init-gpt)
(require 'init-search)
(require 'init-direnv)
(require 'init-smartparens)
(require 'init-rainbow-delimiters)
(require 'init-avy)
(require 'init-multiple-cursors)
(require 'init-auctex)
(require 'init-jupyter)
(require 'init-browser)
(require 'init-fzfs)
(my/package-register-vc 'joplin-mode
                        '(:url "https://github.com/cinsk/joplin-mode.git"
                          :rev :last-release))
(with-eval-after-load 'markdown-mode
  (require 'init-joplin))
(require 'init-ignored)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-macos))

(provide 'init-modules)
;;; init-modules.el ends here
