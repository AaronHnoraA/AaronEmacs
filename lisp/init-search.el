;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; =========================
;; Ivy
;; =========================
(use-package ivy
  :ensure t
  :defer t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  )

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-alt-done))

(use-package counsel
  :ensure t
  :after ivy)

(defconst my/search-ignore-exact-names
  '(".DS_Store" ".localized" "Icon\r"
    ".use-package-keywords.md" ".projectile" ".dir-locals-2.el" ".cache"
    "__pycache__" ".mypy_cache" ".pytest_cache" ".ruff_cache"
    ".coverage" "coverage"
    "node_modules" "dist" "build" "out" ".parcel-cache" ".turbo"
    "target" ".gradle" ".idea" ".vscode" ".settings" ".classpath" ".project"
    ".ccls-cache" ".clangd" ".cache-clangd"
    ".venv" "venv" ".envrc"
    ".git" ".hg" ".svn")
  "Exact file or directory names treated as junk in search-related UIs.")

(defconst my/search-ignore-patterns-regexp
  (concat
   "\\(?:"
   ".*~\\'"
   "\\|#.*#\\'"
   "\\|\\.#[^/]+\\'"
   "\\|.*\\.~undo-tree~\\'"
   "\\|.*\\(?:\\.swp\\|\\.swo\\|\\.swn\\|\\.swx\\)\\'"
   "\\|.*\\.un~\\'"
   "\\|.*\\(?:\\.bak\\|\\.tmp\\|\\.temp\\)\\'"
   "\\|\\.Trash\\'"
   "\\|\\.DocumentRevisions-V100\\'"
   "\\|\\.TemporaryItems\\'"
   "\\|\\.fseventsd\\'"
   "\\|\\.Spotlight-V100\\'"
   "\\)")
  "Regexp matching junk file patterns to exclude from recent-file UIs.")

(defun my/search-apply-recentf-filter ()
  "Exclude junk entries from `recentf'."
  (with-eval-after-load 'recentf
    (dolist (name my/search-ignore-exact-names)
      (add-to-list 'recentf-exclude
                   (concat "/"
                           (regexp-quote name)
                           "\\(?:/\\|\\'\\)")))
    (add-to-list 'recentf-exclude my/search-ignore-patterns-regexp)))

(my/search-apply-recentf-filter)

(declare-function consult-line "consult" (&optional initial start))
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-recent-file "consult" ())

(defun my/search-line-forward ()
  "Search the current buffer with `consult-line'."
  (interactive)
  (call-interactively #'consult-line))

(defun my/search-line-backward ()
  "Keep classic backward incremental search on `C-r'."
  (interactive)
  (call-interactively #'isearch-backward))

(defun my/search-open-recent-file ()
  "Open a recent file with the Consult UI."
  (interactive)
  (call-interactively #'consult-recent-file))

(defun my/search-project-buffers ()
  "Search across opened project buffers."
  (interactive)
  (if (fboundp 'consult-line-multi)
      (call-interactively #'consult-line-multi)
    (call-interactively #'consult-line)))

(global-set-key (kbd "C-s") #'my/search-line-forward)
(global-set-key (kbd "C-r") #'my/search-line-backward)
(global-set-key (kbd "C-x C-r") #'my/search-open-recent-file)

(provide 'init-search)

;;; init-base.el ends here
