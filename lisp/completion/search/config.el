;;; editor/snippets/config.el --- The necessary settings -*- lexical-binding: t -*-

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

(defconst +search-ignore-exact-names
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

(defconst +search-ignore-patterns-regexp
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

(defun +search-apply-recentf-filter ()
  "Exclude junk entries from `recentf'."
  (with-eval-after-load 'recentf
    (dolist (name +search-ignore-exact-names)
      (add-to-list 'recentf-exclude
                   (concat "/"
                           (regexp-quote name)
                           "\\(?:/\\|\\'\\)")))
    (add-to-list 'recentf-exclude +search-ignore-patterns-regexp)))

(+search-apply-recentf-filter)

(declare-function consult-line "consult" (&optional initial start))
(declare-function consult-line-multi "consult" (query &optional initial))
(declare-function consult-recent-file "consult" ())
(defvar evil-visual-beginning)
(defvar evil-visual-end)

(defun +search/line-forward ()
  "Search the current buffer with `consult-line'.
From Doom's `+default/search-buffer': a selection within one line is the
initial input, matched literally; a selection over several lines limits
the search to those lines."
  (interactive)
  (if (not (use-region-p))
      (call-interactively #'consult-line)
    ;; Evil's Visual selection includes the character under the cursor,
    ;; which the Emacs region it maintains does not.
    (let ((beg (if (bound-and-true-p evil-visual-state-minor-mode)
                   evil-visual-beginning
                 (region-beginning)))
          (end (if (bound-and-true-p evil-visual-state-minor-mode)
                   evil-visual-end
                 (region-end))))
      (deactivate-mark)
      (if (= (line-number-at-pos beg) (line-number-at-pos end))
          (consult-line (replace-regexp-in-string
                         " " "\\\\ "
                         (regexp-quote (buffer-substring-no-properties beg end))))
        (save-restriction
          (narrow-to-region beg end)
          (consult-line))))))

(defun +search/line-backward ()
  "Keep classic backward incremental search on `C-r'."
  (interactive)
  (call-interactively #'isearch-backward))

(defun +search/open-recent-file ()
  "Open a recent file with the Consult UI."
  (interactive)
  (call-interactively #'consult-recent-file))

(defun +search/project-buffers ()
  "Search across opened project buffers."
  (interactive)
  (if (fboundp 'consult-line-multi)
      (call-interactively #'consult-line-multi)
    (call-interactively #'consult-line)))

(global-set-key (kbd "C-s") #'+search/line-forward)
(global-set-key (kbd "C-r") #'+search/line-backward)
(global-set-key (kbd "C-x C-r") #'+search/open-recent-file)

(provide 'completion/search/config)

;;; core/aaron-base.el ends here
