;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dired-narrow is superseded by `consult-focus-lines'.

;;; Code:

(require 'aaron-ui)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function material-icon-dired-icons-mode "material-icon-dired" (&optional arg))
(declare-function material-icon-create-icon-image "material-icon-utils" (icon-path))
(declare-function material-icon-get-icon-for-dir "material-icon-utils" (dirname))
(declare-function material-icon-get-icon-for-file "material-icon-utils" (filename &optional dir-p))
(declare-function material-icon-set-icon-size "material-icon-utils" (size))

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

(defun my/dired-material-icons-setup ()
  "Enable Material SVG icons when the selected frame can render them."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (require 'material-icon-dired nil t))
    (material-icon-set-icon-size (max 16 (frame-char-height)))
    (material-icon-dired-icons-mode 1)))

(defun my/dired-material-icons-add-icons ()
  "Apply Material SVG icons to every visible Dired/Dirvish entry.
Unlike the package default, directory entries named `.' and `..' receive
the normal folder icon as well.  Repainting the whole buffer also covers
lines inserted later by `dirvish-subtree'."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename nil)
          (when-let* ((file (dired-get-filename 'relative 'noerror)))
            (let* ((absolute (dired-get-filename nil 'noerror))
                   (dir-p (or (member file '("." ".."))
                              (and absolute (file-directory-p absolute))
                              (string-suffix-p "/" file)))
                   (icon-path
                    (if dir-p
                        (material-icon-get-icon-for-dir file)
                      (material-icon-get-icon-for-file file)))
                   (icon (material-icon-create-icon-image icon-path)))
              (when icon
                (put-text-property (1- (point)) (point) 'display icon)))))
        (forward-line 1)))))

(defun my/dired-material-icons-after-subtree (&rest _)
  "Refresh Material icons after Dirvish inserts a subtree."
  (when (bound-and-true-p material-icon-dired-icons-mode)
    (my/dired-material-icons-add-icons)))

(add-hook 'dired-mode-hook #'my/dired-material-icons-setup)

(with-eval-after-load 'material-icon-dired
  (advice-remove 'material-icon-dired-add-icons
                 #'my/dired-material-icons-add-icons)
  (advice-add 'material-icon-dired-add-icons
              :override #'my/dired-material-icons-add-icons))

(with-eval-after-load 'dirvish-subtree
  (advice-remove 'dirvish-subtree--insert
                 #'my/dired-material-icons-after-subtree)
  (advice-add 'dirvish-subtree--insert
              :after #'my/dired-material-icons-after-subtree))

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
