;;; completion/telescope/config.el --- Centralized Telescope-style picker UI -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)

(declare-function bookmark-bmenu-list "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function consult-buffer "consult" ())
(declare-function consult-complex-command "consult" ())
(declare-function consult-find "consult" (&optional dir initial))
(declare-function consult-git-grep "consult" (&optional dir initial))
(declare-function consult-line "consult" (&optional initial start))
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-recent-file "consult" ())
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function consult-yank-pop "consult" ())
(declare-function +diagnostics/buffer-errors "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/buffer-notes "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/buffer-ui "checkers/diagnostics/config" ())
(declare-function +diagnostics/buffer-warnings "checkers/diagnostics-extra/config" ())
(declare-function diagnostics-menu "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/project-errors "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/project-notes "checkers/diagnostics-extra/config" ())
(declare-function +diagnostics/project-ui "checkers/diagnostics/config" ())
(declare-function +diagnostics/project-warnings "checkers/diagnostics-extra/config" ())
(declare-function +project-current-root "tools/project/config")
(declare-function project-menu "tools/project/config")
(declare-function +project/open-root "tools/project/config")
(declare-function +project/ripgrep "tools/project/config"
                  (project-root &optional initial))
(declare-function +project/switch "tools/project/config")
(declare-function +project/portal "tools/project/config")
(declare-function consult-bookmark "consult" (name))
(declare-function +problems/buffer "checkers/problems/config" ())
(declare-function +problems/buffer-errors "checkers/problems/config" ())
(declare-function +problems/buffer-notes "checkers/problems/config" ())
(declare-function +problems/buffer-warnings "checkers/problems/config" ())
(declare-function +problems/project "checkers/problems/config" ())
(declare-function +problems/project-errors "checkers/problems/config" ())
(declare-function +problems/project-notes "checkers/problems/config" ())
(declare-function +problems/project-warnings "checkers/problems/config" ())
(declare-function +symbols/buffer "tools/symbols/config")
(declare-function +symbols/project "tools/symbols/config")

(defgroup +telescope nil
  "Centralized Telescope-style picker workflow."
  :group 'convenience)

(config-defvar +telescope-preview-key nil
  "Default preview strategy used by `telescope'."
  :type 'sexp
  :group '+telescope)

(config-defvar +telescope-file-preview-key nil
  "Preview strategy for file-oriented Telescope pickers."
  :type 'sexp
  :group '+telescope)

(config-defvar +telescope-grep-preview-key nil
  "Preview strategy for grep-oriented Telescope pickers."
  :type 'sexp
  :group '+telescope)

(defun +telescope--project-root ()
  "Return the active project root, or `default-directory' as fallback."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      default-directory))

(defun +telescope/project-files ()
  "Find files from the active project root with live preview."
  (interactive)
  (consult-find (+telescope--project-root)))

(defun +telescope/files-here ()
  "Find files from the current directory with live preview."
  (interactive)
  (consult-find default-directory))

(defun +telescope/buffers ()
  "Switch buffers from a centralized Telescope picker."
  (interactive)
  (consult-buffer))

(defun +telescope/recent-files ()
  "Open recent files from a centralized Telescope picker."
  (interactive)
  (consult-recent-file))

(defun +telescope/ripgrep ()
  "Search the active project with an available target-side program."
  (interactive)
  (require 'tools/project/config)
  (+project/ripgrep (+telescope--project-root)))

(defun +telescope/git-grep ()
  "Search the active project with git-grep and live preview."
  (interactive)
  (consult-git-grep (+telescope--project-root)))

(defun +telescope/lines ()
  "Search lines in the current buffer with live preview."
  (interactive)
  (consult-line))

(defun +telescope/project-lines ()
  "Search lines across project buffers with live preview."
  (interactive)
  (consult-line-multi nil))

(defun +telescope/symbol-at-point ()
  "Search the symbol at point in the active project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (require 'tools/project/config)
    (+project/ripgrep (+telescope--project-root) symbol)))

(defun +telescope/buffer-symbols ()
  "Search symbols in the current buffer with live preview."
  (interactive)
  (+symbols/buffer))

(defun +telescope/project-symbols ()
  "Search symbols across project buffers with live preview."
  (interactive)
  (+symbols/project))

(defun +telescope/bookmarks ()
  "Browse bookmarks through Consult's native bookmark source."
  (interactive)
  (call-interactively #'consult-bookmark))

(defun +telescope/yank-ring ()
  "Browse the kill ring from a centralized Telescope picker."
  (interactive)
  (consult-yank-pop))

(defun +telescope/command-history ()
  "Replay commands from the command history with preview."
  (interactive)
  (consult-complex-command))

(with-eval-after-load 'consult
  (eval
   `(consult-customize
     +telescope/project-files
     +telescope/files-here
     :preview-key ',+telescope-file-preview-key))
  (eval
   `(consult-customize
     +telescope/buffers
     +telescope/recent-files
     +telescope/lines
     +telescope/project-lines
     +telescope/buffer-symbols
     +telescope/project-symbols
     +telescope/bookmarks
     +telescope/yank-ring
     +telescope/command-history
     :preview-key ',+telescope-preview-key))
  (eval
   `(consult-customize
     +telescope/ripgrep
     +telescope/git-grep
     +telescope/symbol-at-point
     :preview-key ',+telescope-grep-preview-key)))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix telescope ()
    "Centralized Telescope-style picker hub."
    [["Find"
      ("f" "project files" +telescope/project-files
       :transient transient--do-exit)
      ("F" "cwd files" +telescope/files-here
       :transient transient--do-exit)
      ("b" "buffers" +telescope/buffers
       :transient transient--do-exit)
      ("r" "recent files" +telescope/recent-files
       :transient transient--do-exit)]
     ["Search"
      ("g" "project search" +telescope/ripgrep
       :transient transient--do-exit)
      ("G" "git grep" +telescope/git-grep
       :transient transient--do-exit)
      ("l" "buffer lines" +telescope/lines
       :transient transient--do-exit)
      ("L" "project lines" +telescope/project-lines
       :transient transient--do-exit)
      ("s" "symbol at point" +telescope/symbol-at-point
       :transient transient--do-exit)]
     ["Symbols"
      ("i" "buffer symbols" +telescope/buffer-symbols
       :transient transient--do-exit)
      ("I" "project symbols" +telescope/project-symbols
       :transient transient--do-exit)
      ("m" "bookmarks" +telescope/bookmarks
       :transient transient--do-exit)
      ("y" "yank ring" +telescope/yank-ring
       :transient transient--do-exit)]]
    [["Project"
      ("p" "switch project" +project/switch
       :transient transient--do-exit)
      ("o" "open root" +project/open-root
       :transient transient--do-exit)
      ("v" "project portal" +project/portal
       :transient transient--do-exit)
      ("P" "project workbench" project-menu
       :transient transient--do-exit)]
     ["Diagnostics"
      ("!" "buffer picker" +problems/buffer
       :transient transient--do-exit)
      ("?" "project picker" +problems/project
       :transient transient--do-exit)
      ("d" "diagnostics menu" diagnostics-menu
       :transient transient--do-exit)
      ("D" "Flymake project list" +diagnostics/project-ui
       :transient transient--do-exit)]
     ["Buffer Diags"
      ("e" "errors" +problems/buffer-errors
       :transient transient--do-exit)
      ("w" "warnings" +problems/buffer-warnings
       :transient transient--do-exit)
      ("n" "notes" +problems/buffer-notes
       :transient transient--do-exit)
      ("B" "board" +diagnostics/buffer-ui
       :transient transient--do-exit)]
     ["Project Diags"
      ("E" "errors" +problems/project-errors
       :transient transient--do-exit)
      ("W" "warnings" +problems/project-warnings
       :transient transient--do-exit)
      ("N" "notes" +problems/project-notes
       :transient transient--do-exit)]
     ["Other"
      ("c" "command history" +telescope/command-history
       :transient transient--do-exit)]])
  (global-set-key (kbd "C-c p t") #'telescope))

;; Bind physical `SPC SPC` under the Evil leader map. Using `<leader><leader>`
;; here does not resolve to the same key sequence as the literal leader key.
(aaron-leader!
  "SPC" '(:def telescope :which-key "telescope"))

(provide 'completion/telescope/config)
;;; completion/telescope/config.el ends here
