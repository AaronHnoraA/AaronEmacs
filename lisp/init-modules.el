;;; init-modules.el --- Load init modules in order -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my/require-module-safely (module)
  "Require MODULE and report errors without aborting the rest of init."
  (condition-case err
      (require module)
    (error
     (display-warning
      'init-modules
      (format "Failed to load %s: %s" module (error-message-string err))
      :error)
     nil)))

(defun my/require-module-after-any-feature (module &rest features)
  "Require MODULE after any of FEATURES loads.
If one of FEATURES is already available, require MODULE immediately."
  (if (catch 'loaded
        (dolist (feature features)
          (when (featurep feature)
            (throw 'loaded t))))
      (my/require-module-safely module)
    (dolist (feature features)
      (with-eval-after-load feature
        (my/require-module-safely module)))))

(require 'init-utils)
(require 'init-base)
(require 'init-tramp)
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
(require 'init-lsp-tools)
(require 'init-code-actions)
(require 'init-git)
(require 'init-smerge)
(require 'init-dev)
(require 'init-navigation)
(require 'init-navigation-extra)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-consult-project)
(require 'init-snippets)
(require 'init-auto-insert)
(require 'init-doom-extra)
(require 'init-treesit)
(require 'init-windows)
(require 'init-expand-region)
(require 'init-treemacs-bridge)
(require 'init-project)
(require 'init-workspaces)
(require 'init-scratch)
(require 'init-project-local)
(require 'init-rename)
(require 'init-harpoon)
(require 'init-test)
(require 'init-task)
(require 'init-project-run)
(require 'init-debug-profile)
(require 'init-output)
(require 'init-session)
(require 'init-maintenance)
(require 'init-diagnostics-ui)
(require 'init-diagnostics-extra)
(require 'init-health)
(require 'init-server)

;; standalone apps
(my/require-module-after-any-feature 'init-org 'org)
(my/require-module-after-any-feature 'init-org-mmdc 'org)
(my/require-module-after-any-feature 'init-org-zotero 'org)
(my/require-module-safely 'init-eaf)
(require 'init-text)
(require 'init-shell)
(require 'init-remote-connectboard)
(my/require-module-safely 'init-spell)
(my/require-module-safely 'init-gpt)
(require 'init-search)
(require 'init-search-extra)
(require 'init-telescope)
(require 'init-direnv)
(require 'init-smartparens)
(require 'init-rainbow-delimiters)
(require 'init-avy)
(require 'init-multiple-cursors)
(my/require-module-after-any-feature 'init-auctex 'tex 'tex-site 'pdf-tools 'pdf-view)
(my/require-module-safely 'init-jupyter)
(my/require-module-safely 'init-browser)
(my/require-module-safely 'init-fzfs)
(require 'init-function-keys)
(my/package-register-vc 'joplin-mode
                        '(:url "https://github.com/cinsk/joplin-mode.git"
                          :rev :last-release))
(with-eval-after-load 'markdown-mode
  (my/require-module-safely 'init-joplin))
(require 'init-ignored)


;; MacOS specific
(when (eq system-type 'darwin)
  (my/require-module-safely 'init-macos))

(provide 'init-modules)
;;; init-modules.el ends here
