;;; init-git-core.el --- Core Git UI and workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'transient)

(declare-function browse-at-remote "browse-at-remote" (&optional kill))
(declare-function browse-at-remote-kill "browse-at-remote" ())
(declare-function diff-hl-next-hunk "diff-hl" (&optional backward))
(declare-function diff-hl-previous-hunk "diff-hl" ())
(declare-function diff-hl-revert-hunk "diff-hl" ())
(declare-function diff-hl-show-hunk "diff-hl-show-hunk" ())
(declare-function diff-hl-stage-current-hunk "diff-hl" ())
(declare-function magit-blame-addition "magit-blame" ())
(declare-function magit-blame-quit "magit-blame" ())
(declare-function magit-dispatch "magit" ())
(declare-function magit-file-dispatch "magit" ())
(declare-function magit-log-buffer-file "magit-log" (&optional follow beg end))
(declare-function magit-stage-files "magit-apply" (files &optional force))
(declare-function magit-status "magit" (&optional directory))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-unstage-files "magit-apply" (files))
(declare-function git-commit-setup-flyspell "git-commit" ())
(declare-function git-commit-turn-on-flyspell "git-commit" ())
(declare-function my/git-board "init-git-board" ())
(declare-function my/git-diff-file-with-branch-base "init-git-diff" ())
(declare-function my/git-diff-file-with-head "init-git-diff" ())
(declare-function my/git-diff-file-with-revision "init-git-diff" (revision))
(declare-function my/git-tree "init-git-tree" ())
(declare-function my/smerge-dispatch "init-smerge" ())

(defvar magit-blame-mode)

(defvar-local my/git-tool-origin-window-configuration nil
  "Window configuration captured before opening the current Git tool buffer.")

(defvar-local my/git-tool-cleanup-functions nil
  "Cleanup functions to run when the current Git tool buffer is killed.")

(defun my/git-tool-register-cleanup (fn)
  "Register cleanup function FN for the current Git tool buffer."
  (add-hook 'kill-buffer-hook fn nil t)
  (push fn my/git-tool-cleanup-functions))

(defun my/git-tool-prepare-buffer ()
  "Capture the current window configuration for a Git tool buffer."
  (setq-local my/git-tool-origin-window-configuration
              (current-window-configuration)))

(defun my/git-tool-quit-buffer (origin-buffer)
  "Quit the current Git tool buffer and restore ORIGIN-BUFFER when possible."
  (let ((tool-buffer (current-buffer))
        (window-config my/git-tool-origin-window-configuration))
    (cond
     ((and (window-configuration-p window-config)
           (buffer-live-p origin-buffer))
      (set-window-configuration window-config)
      (when (eq (current-buffer) tool-buffer)
        (switch-to-buffer origin-buffer))
      (kill-buffer tool-buffer))
     ((buffer-live-p origin-buffer)
      (switch-to-buffer origin-buffer)
      (kill-buffer tool-buffer))
     (t
      (quit-window 'kill)))))

(defun my/git-commit-setup-flyspell-h ()
  "Enable Flyspell in commit buffers across Magit versions."
  (if (fboundp 'git-commit-setup-flyspell)
      (git-commit-setup-flyspell)
    (git-commit-turn-on-flyspell)))

(defun my/git--current-file-context ()
  "Return current file Git context as (REPO-ROOT RELATIVE-PATH).

Signal a user error if the current buffer is not visiting a tracked file inside
a Git repository."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((file (file-truename buffer-file-name))
         (repo-root (magit-toplevel (file-name-directory file))))
    (unless repo-root
      (user-error "Current file is not inside a Git repository"))
    (unless (file-in-directory-p file repo-root)
      (user-error "Current file is outside the repository root"))
    (list repo-root (file-relative-name file repo-root))))

(defun my/git-file-log ()
  "Show the Git log for the current file."
  (interactive)
  (require 'magit-log)
  (magit-log-buffer-file))

(defun my/git-file-blame-toggle ()
  "Toggle blame annotations for the current file."
  (interactive)
  (require 'magit-blame)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
    (magit-blame-addition)))

(defun my/git-stage-current-file ()
  "Stage the current file."
  (interactive)
  (pcase-let ((`(,repo-root ,relative-path) (my/git--current-file-context)))
    (require 'magit-apply)
    (let ((default-directory repo-root))
      (magit-stage-files (list relative-path)))
    (message "Staged %s" relative-path)))

(defun my/git-unstage-current-file ()
  "Unstage the current file."
  (interactive)
  (pcase-let ((`(,repo-root ,relative-path) (my/git--current-file-context)))
    (require 'magit-apply)
    (let ((default-directory repo-root))
      (magit-unstage-files (list relative-path)))
    (message "Unstaged %s" relative-path)))

(transient-define-prefix my/git-dispatch ()
  "Git workbench for repository and current-file operations."
  [["Repository"
    ("g" "status" magit-status :transient transient--do-exit)
    ("w" "git board" my/git-board :transient transient--do-exit)
    ("t" "git tree" my/git-tree :transient transient--do-exit)
    ("." "magit dispatch" magit-dispatch :transient transient--do-exit)
    ("m" "merge conflict" my/smerge-dispatch :transient transient--do-exit)]
   ["Current file"
    ("d" "diff vs revision" my/git-diff-file-with-revision :transient transient--do-exit)
    ("=" "diff vs HEAD" my/git-diff-file-with-head :transient transient--do-exit)
    ("b" "diff vs branch base" my/git-diff-file-with-branch-base :transient transient--do-exit)
    ("l" "file log" my/git-file-log :transient transient--do-exit)
    ("B" "blame toggle" my/git-file-blame-toggle :transient transient--do-exit)
    ("s" "stage file" my/git-stage-current-file :transient transient--do-exit)
    ("u" "unstage file" my/git-unstage-current-file :transient transient--do-exit)]
   ["Changes"
    ("[" "prev hunk" diff-hl-previous-hunk :transient transient--do-exit)
    ("]" "next hunk" diff-hl-next-hunk :transient transient--do-exit)
    ("h" "show hunk" diff-hl-show-hunk :transient transient--do-exit)
    ("r" "revert hunk" diff-hl-revert-hunk :transient transient--do-exit)
    ("S" "stage hunk" diff-hl-stage-current-hunk :transient transient--do-exit)]
   ["Remote"
    ("y" "copy remote url" browse-at-remote-kill :transient transient--do-exit)
    ("Y" "browse remote url" browse-at-remote :transient transient--do-exit)
    ("f" "file dispatch" magit-file-dispatch :transient transient--do-exit)]])

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :ensure t
  :hook (git-commit-setup . my/git-commit-setup-flyspell-h)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-paint-whitespace nil)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-section-visibility-indicator '("▸" . "▾")))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-allow-async-diff t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure t
  :defer 2
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (when (fboundp 'diff-hl-show-hunk-mouse-mode)
    (diff-hl-show-hunk-mouse-mode 1))
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  :custom
  (diff-hl-update-async t))

(defvar my/git-ui--face-theme-signature nil
  "Last theme signature applied by `my/git-apply-ui'.")

(defun my/git-apply-ui ()
  "Apply local diff indicator faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/git-ui--face-theme-signature)
        (setq my/git-ui--face-theme-signature signature)
        (when (facep 'magit-section-heading)
          (set-face-attribute 'magit-section-heading nil
                              :foreground "#a9bed3"
                              :weight 'medium))
        (when (facep 'magit-section-highlight)
          (set-face-attribute 'magit-section-highlight nil
                              :background "#2c3342"
                              :extend t))
        (when (facep 'magit-diff-context)
          (set-face-attribute 'magit-diff-context nil
                              :foreground "#8b90a8"
                              :background "#24232f"))
        (when (facep 'magit-diff-context-highlight)
          (set-face-attribute 'magit-diff-context-highlight nil
                              :foreground "#d8dee9"
                              :background "#2a2f3d"))
        (when (facep 'magit-diff-added)
          (set-face-attribute 'magit-diff-added nil
                              :foreground "#8fbf8f"
                              :background "#243026"))
        (when (facep 'magit-diff-added-highlight)
          (set-face-attribute 'magit-diff-added-highlight nil
                              :foreground "#a7d7a7"
                              :background "#2a3a2e"))
        (when (facep 'magit-diff-removed)
          (set-face-attribute 'magit-diff-removed nil
                              :foreground "#d79a9a"
                              :background "#332628"))
        (when (facep 'magit-diff-removed-highlight)
          (set-face-attribute 'magit-diff-removed-highlight nil
                              :foreground "#e4b3b3"
                              :background "#3a2d30"))
        (when (facep 'magit-branch-local)
          (set-face-attribute 'magit-branch-local nil
                              :foreground "#8aa6c1"
                              :weight 'medium))
        (when (facep 'magit-branch-remote)
          (set-face-attribute 'magit-branch-remote nil
                              :foreground "#8fbf8f"
                              :weight 'medium))
        (when (facep 'magit-hash)
          (set-face-attribute 'magit-hash nil
                              :foreground "#6f748b"))
        (when (facep 'diff-hl-insert)
          (set-face-attribute 'diff-hl-insert nil
                              :foreground "#7fa86f"
                              :background "#7fa86f"))
        (when (facep 'diff-hl-change)
          (set-face-attribute 'diff-hl-change nil
                              :foreground "#8aa6c1"
                              :background "#8aa6c1"))
        (when (facep 'diff-hl-delete)
          (set-face-attribute 'diff-hl-delete nil
                              :foreground "#bf7f7f"
                              :background "#bf7f7f"))))))

(add-hook 'after-init-hook #'my/git-apply-ui)
(add-hook 'server-after-make-frame-hook #'my/git-apply-ui)
(add-hook 'after-load-theme-hook #'my/git-apply-ui)

;; Visual diff interface
(use-package ediff
  :ensure nil
  ;; Restore window config after quitting ediff
  :hook ((ediff-before-setup . ediff-save-window-conf)
         (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Setup gitignore mode
(use-package conf-mode
  :ensure nil
  :mode (("\\.gitignore\\'"     . conf-unix-mode)
         ("\\.gitconfig\\'"     . conf-unix-mode)
         ("\\.gitattributes\\'" . conf-unix-mode)))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

(use-package browse-at-remote
  :ensure t
  :commands (browse-at-remote
             browse-at-remote-kill))

(my/leader!
  "g"   '(:ignore t :which-key "git")
  "g ." '(:def my/git-dispatch :which-key "git hub")
  "g g" '(:def magit-status :which-key "status")
  "g w" '(:def my/git-board :which-key "git board")
  "g t" '(:def my/git-tree :which-key "git tree")
  "g l" '(:def my/git-file-log :which-key "file log")
  "g B" '(:def my/git-file-blame-toggle :which-key "blame toggle")
  "g S" '(:def my/git-stage-current-file :which-key "stage file")
  "g U" '(:def my/git-unstage-current-file :which-key "unstage file")
  "g ]" '(:def diff-hl-next-hunk :which-key "next hunk")
  "g [" '(:def diff-hl-previous-hunk :which-key "previous hunk")
  "g r" '(:def diff-hl-revert-hunk :which-key "revert hunk")
  "g s" '(:def diff-hl-stage-current-hunk :which-key "stage hunk")
  "g h" '(:def diff-hl-show-hunk :which-key "show hunk")
  "g y" '(:def browse-at-remote-kill :which-key "copy remote url")
  "g Y" '(:def browse-at-remote :which-key "browse remote url"))

(provide 'init-git-core)
;;; init-git-core.el ends here
