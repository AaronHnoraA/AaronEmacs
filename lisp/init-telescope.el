;;; init-telescope.el --- Centralized Telescope-style picker UI -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function bookmark-bmenu-list "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function consult-bookmark "consult" ())
(declare-function consult-buffer "consult" ())
(declare-function consult-complex-command "consult" ())
(declare-function consult-find "consult" (&optional dir initial))
(declare-function consult-git-grep "consult" (&optional dir initial))
(declare-function consult-line "consult" (&optional initial start))
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-recent-file "consult" ())
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function consult-yank-pop "consult" ())
(declare-function my/project-current-root "init-project")
(declare-function my/project-dispatch "init-project")
(declare-function my/project-open-root "init-project")
(declare-function my/project-switch "init-project")
(declare-function my/project-vterm "init-project")
(declare-function my/symbols-buffer "init-symbols")
(declare-function my/symbols-project "init-symbols")

(defgroup my/telescope nil
  "Centralized Telescope-style picker workflow."
  :group 'convenience)

(defcustom my/telescope-preview-key '(:debounce 0.2 any)
  "Default preview strategy used by `telescope'."
  :type 'sexp
  :group 'my/telescope)

(defcustom my/telescope-file-preview-key '(:debounce 0.15 any)
  "Preview strategy for file-oriented Telescope pickers."
  :type 'sexp
  :group 'my/telescope)

(defcustom my/telescope-grep-preview-key '(:debounce 0.35 any)
  "Preview strategy for grep-oriented Telescope pickers."
  :type 'sexp
  :group 'my/telescope)

(defun my/telescope--project-root ()
  "Return the active project root, or `default-directory' as fallback."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun my/telescope-project-files ()
  "Find files from the active project root with live preview."
  (interactive)
  (consult-find (my/telescope--project-root)))

(defun my/telescope-files-here ()
  "Find files from the current directory with live preview."
  (interactive)
  (consult-find default-directory))

(defun my/telescope-buffers ()
  "Switch buffers from a centralized Telescope picker."
  (interactive)
  (consult-buffer))

(defun my/telescope-recent-files ()
  "Open recent files from a centralized Telescope picker."
  (interactive)
  (consult-recent-file))

(defun my/telescope-ripgrep ()
  "Search the active project with ripgrep and live preview."
  (interactive)
  (consult-ripgrep (my/telescope--project-root)))

(defun my/telescope-git-grep ()
  "Search the active project with git-grep and live preview."
  (interactive)
  (consult-git-grep (my/telescope--project-root)))

(defun my/telescope-lines ()
  "Search lines in the current buffer with live preview."
  (interactive)
  (consult-line))

(defun my/telescope-project-lines ()
  "Search lines across project buffers with live preview."
  (interactive)
  (consult-line-multi nil))

(defun my/telescope-symbol-at-point ()
  "Search the symbol at point in the active project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (consult-ripgrep (my/telescope--project-root) symbol)))

(defun my/telescope-buffer-symbols ()
  "Search symbols in the current buffer with live preview."
  (interactive)
  (my/symbols-buffer))

(defun my/telescope-project-symbols ()
  "Search symbols across project buffers with live preview."
  (interactive)
  (my/symbols-project))

(defun my/telescope-bookmarks ()
  "Browse bookmarks from a centralized Telescope entry.
Load bookmark data first; if no bookmark exists yet, fall back to
the built-in bookmark list buffer."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (if bookmark-alist
      (consult-bookmark)
    (bookmark-bmenu-list)))

(defun my/telescope-yank-ring ()
  "Browse the kill ring from a centralized Telescope picker."
  (interactive)
  (consult-yank-pop))

(defun my/telescope-command-history ()
  "Replay commands from the command history with preview."
  (interactive)
  (consult-complex-command))

(with-eval-after-load 'consult
  (eval
   `(consult-customize
     my/telescope-project-files
     my/telescope-files-here
     :preview-key ',my/telescope-file-preview-key))
  (eval
   `(consult-customize
     my/telescope-buffers
     my/telescope-recent-files
     my/telescope-lines
     my/telescope-project-lines
     my/telescope-buffer-symbols
     my/telescope-project-symbols
     my/telescope-bookmarks
     my/telescope-yank-ring
     my/telescope-command-history
     :preview-key ',my/telescope-preview-key))
  (eval
   `(consult-customize
     my/telescope-ripgrep
     my/telescope-git-grep
     my/telescope-symbol-at-point
     :preview-key ',my/telescope-grep-preview-key)))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix telescope ()
    "Centralized Telescope-style picker hub."
    [["Find"
      ("f" "project files" my/telescope-project-files
       :transient transient--do-exit)
      ("F" "cwd files" my/telescope-files-here
       :transient transient--do-exit)
      ("b" "buffers" my/telescope-buffers
       :transient transient--do-exit)
      ("r" "recent files" my/telescope-recent-files
       :transient transient--do-exit)]
     ["Search"
      ("g" "project ripgrep" my/telescope-ripgrep
       :transient transient--do-exit)
      ("G" "git grep" my/telescope-git-grep
       :transient transient--do-exit)
      ("l" "buffer lines" my/telescope-lines
       :transient transient--do-exit)
      ("L" "project lines" my/telescope-project-lines
       :transient transient--do-exit)
      ("s" "symbol at point" my/telescope-symbol-at-point
       :transient transient--do-exit)]
     ["Symbols"
      ("i" "buffer symbols" my/telescope-buffer-symbols
       :transient transient--do-exit)
      ("I" "project symbols" my/telescope-project-symbols
       :transient transient--do-exit)
      ("m" "bookmarks" my/telescope-bookmarks
       :transient transient--do-exit)
      ("y" "yank ring" my/telescope-yank-ring
       :transient transient--do-exit)]]
    [["Project"
      ("p" "switch project" my/project-switch
       :transient transient--do-exit)
      ("o" "open root" my/project-open-root
       :transient transient--do-exit)
      ("v" "project vterm" my/project-vterm
       :transient transient--do-exit)
      ("w" "project workbench" my/project-dispatch
       :transient transient--do-exit)]
     ["Other"
      ("c" "command history" my/telescope-command-history
       :transient transient--do-exit)]])
  (global-set-key (kbd "C-c p t") #'telescope))

;; Bind physical `SPC SPC` under the Evil leader map. Using `<leader><leader>`
;; here does not resolve to the same key sequence as the literal leader key.
(my/evil-global-leader-set "SPC" #'telescope "telescope")

(provide 'init-telescope)
;;; init-telescope.el ends here
