;;; init-modules.el --- Load init modules in order -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my/require-module-after-any-feature (module &rest features)
  "Require MODULE after any of FEATURES loads.
If one of FEATURES is already available, require MODULE immediately."
  (if (catch 'loaded
        (dolist (feature features)
          (when (featurep feature)
            (throw 'loaded t))))
      (require module)
    (dolist (feature features)
      (with-eval-after-load feature
        (require module)))))

(require 'init-base)
(require 'init-utils)
(require 'init-ui)
(require 'init-display-rules)
(require 'init-tools)
(require 'init-evil)
(require 'init-lsp)
(require 'init-format)
(require 'init-problems)
(require 'init-symbols)
(require 'init-workspace-symbol)
(require 'init-lsp-ops)
(require 'init-code-actions)
(require 'init-git)
(require 'init-smerge)
(require 'init-dev)
(require 'init-navigation)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-consult-project)
(require 'init-snippets)
(require 'init-treesit)
(require 'init-windows)
(require 'init-expand-region)
(require 'init-project)
(require 'init-rename)
(require 'init-harpoon)
(require 'init-test)
(require 'init-task)
(require 'init-project-run)
(require 'init-session)
(require 'init-diagnostics-ui)
(require 'init-server)

;; standalone apps
(my/require-module-after-any-feature 'init-org 'org)
(my/require-module-after-any-feature 'init-org-zotero 'org)
(require 'init-eaf)
(require 'init-text)
(require 'init-shell)
(require 'init-spell)
(require 'init-gpt)
(require 'init-search)
(require 'init-search-extra)
(require 'init-direnv)
(require 'init-smartparens)
(require 'init-rainbow-delimiters)
(require 'init-avy)
(require 'init-multiple-cursors)
(my/require-module-after-any-feature 'init-auctex 'tex 'tex-site 'pdf-tools 'pdf-view)
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
