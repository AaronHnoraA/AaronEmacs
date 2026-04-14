;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;
;; git-messenger has been superseded by {C-x v h} (`vc-region-history')

;;; Code:

(require 'init-funcs)

(declare-function browse-at-remote "browse-at-remote" (&optional kill))
(declare-function browse-at-remote-kill "browse-at-remote" ())
(declare-function diff-hl-next-hunk "diff-hl" (&optional backward))
(declare-function diff-hl-previous-hunk "diff-hl" ())
(declare-function diff-hl-revert-hunk "diff-hl" ())
(declare-function diff-hl-show-hunk "diff-hl-show-hunk" ())
(declare-function diff-hl-stage-current-hunk "diff-hl" ())
(declare-function git-commit-setup-flyspell "git-commit" ())
(declare-function git-commit-turn-on-flyspell "git-commit" ())

(defun my/git-commit-setup-flyspell-h ()
  "Enable Flyspell in commit buffers across Magit versions."
  (if (fboundp 'git-commit-setup-flyspell)
      (git-commit-setup-flyspell)
    (git-commit-turn-on-flyspell)))

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

(defun my/git-apply-ui ()
  "Apply local diff indicator faces."
  (when (display-graphic-p)
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
      (set-face-attribute 'diff-hl-insert nil :foreground "#7fa86f" :background "#7fa86f"))
    (when (facep 'diff-hl-change)
      (set-face-attribute 'diff-hl-change nil :foreground "#8aa6c1" :background "#8aa6c1"))
    (when (facep 'diff-hl-delete)
      (set-face-attribute 'diff-hl-delete nil :foreground "#bf7f7f" :background "#bf7f7f"))))

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

(my/leader-key-label "g" "git")
(my/evil-global-leader-set "g g" #'magit-status "status")
(my/evil-global-leader-set "g ]" #'diff-hl-next-hunk "next hunk")
(my/evil-global-leader-set "g [" #'diff-hl-previous-hunk "previous hunk")
(my/evil-global-leader-set "g r" #'diff-hl-revert-hunk "revert hunk")
(my/evil-global-leader-set "g s" #'diff-hl-stage-current-hunk "stage hunk")
(my/evil-global-leader-set "g h" #'diff-hl-show-hunk "show hunk")
(my/evil-global-leader-set "g y" #'browse-at-remote-kill "copy remote url")
(my/evil-global-leader-set "g Y" #'browse-at-remote "browse remote url")

(provide 'init-git)

;;; init-git.el ends here
