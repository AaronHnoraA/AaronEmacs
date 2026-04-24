;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dired-narrow is superseded by `consult-focus-lines'.

;;; Code:

(require 'aaron-ui)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))

(defun my/dired-open-dwim ()
  "Open the Dired entry at point."
  (interactive)
  (call-interactively #'dired-find-file))

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("RET"       . my/dired-open-dwim)
         ("<return>"  . my/dired-open-dwim)
         ;; consistent with ivy
         ("C-c C-e"   . wdired-change-to-wdired-mode)
         ("H"         . dired-dotfiles-toggle))
  :config
  (with-eval-after-load 'evil
    (evil-define-key* 'normal dired-mode-map (kbd "RET") #'my/dired-open-dwim)
    (evil-define-key* 'normal dired-mode-map (kbd "<return>") #'my/dired-open-dwim))
  :custom
  (dired-dwim-target t)
  (dired-bind-vm nil)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-AFhlv"))

(use-package dired-aux
  :ensure nil
  :after dired
  :config
  (with-no-warnings
    (defvar dired-dotfiles-show t)
    (defun dired-dotfiles-toggle (&rest _)
      "Show/hide dotfiles."
      (interactive)
      (if (not dired-dotfiles-show)
          (revert-buffer)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
      (setq-local dired-dotfiles-show (not dired-dotfiles-show))))
  :custom
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

;; Make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(defun my/dired-apply-ui ()
  "Apply local UI styling to Dired."
  (when (display-graphic-p)
    (when (facep 'diredfl-dir-name)
      (aaron-ui-set-face 'diredfl-dir-name
                         :foreground 'fg-dim
                         :weight 'medium))
    (when (facep 'diredfl-file-name)
      (aaron-ui-set-face 'diredfl-file-name
                         :foreground 'fg-soft))
    (when (facep 'diredfl-exec-priv)
      (aaron-ui-set-face 'diredfl-exec-priv
                         :foreground 'accent-green-soft))
    (when (facep 'diredfl-no-priv)
      (aaron-ui-set-face 'diredfl-no-priv
                         :foreground 'fg-faint))
    (when (facep 'diredfl-date-time)
      (aaron-ui-set-face 'diredfl-date-time
                         :foreground 'line-number))
    (when (facep 'diredfl-number)
      (aaron-ui-set-face 'diredfl-number
                         :foreground 'accent-cyan))
    (when (facep 'dired-header)
      (aaron-ui-set-face 'dired-header
                         :foreground 'fg-dim
                         :weight 'medium))
    (setq-local hl-line-face 'hl-line)))

(add-hook 'dired-mode-hook #'my/dired-apply-ui)

(let ((gls (executable-find "gls")))
  (setq insert-directory-program (or gls insert-directory-program)
        dired-use-ls-dired (and gls t)
        dired-listing-switches
        (if gls
            "-alh --group-directories-first --time-style=long-iso"
          "-alh")))


(provide 'init-dired)
;;; init-dired.el ends here
