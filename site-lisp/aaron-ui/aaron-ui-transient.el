;;; aaron-ui-transient.el --- Theme transient and which-key from the aaron-ui palette -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Applies aaron-ui palette faces to every transient dispatch menu and to
;; which-key.  No per-menu edits needed — theming transient's shared faces
;; restyles all 23+ menus automatically.
;;
;; Load order: require after aaron-ui is on load-path.
;; `init-tools.el' requires this module.

;;; Code:

(require 'aaron-ui)
(require 'transient)

(defvar aaron-ui-transient--theme-signature nil
  "Signature of the last applied transient theme; skip reapply when unchanged.")

(defun aaron-ui-transient-apply-faces ()
  "Theme transient and which-key faces from the aaron-ui palette."
  (let ((sig (list custom-enabled-themes
                   (face-attribute 'default :background nil t)
                   (face-attribute 'default :foreground nil t))))
    (unless (equal sig aaron-ui-transient--theme-signature)
      (setq aaron-ui-transient--theme-signature sig)
      (aaron-ui-set-face 'transient-heading          :foreground 'accent-cyan :weight 'bold)
      (aaron-ui-set-face 'transient-key              :foreground 'accent-cyan :weight 'medium)
      (aaron-ui-set-face 'transient-key-stay         :foreground 'accent-green)
      (aaron-ui-set-face 'transient-key-exit         :foreground 'accent-mauve)
      (aaron-ui-set-face 'transient-key-return       :foreground 'accent-yellow)
      (aaron-ui-set-face 'transient-key-noop         :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-argument         :foreground 'accent-yellow :weight 'bold)
      (aaron-ui-set-face 'transient-value            :foreground 'accent-green  :weight 'bold)
      (aaron-ui-set-face 'transient-inactive-argument :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-inactive-value   :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-enabled-suffix   :foreground 'accent-green
                         :background 'bg-success)
      (aaron-ui-set-face 'transient-disabled-suffix  :foreground 'fg-faint
                         :background 'bg-elevated)
      (aaron-ui-set-face 'transient-active-infix     :background 'bg-panel :extend t)
      (aaron-ui-set-face 'transient-delimiter        :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-unreachable      :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-unreachable-key  :foreground 'fg-faint)
      (aaron-ui-set-face 'transient-nonstandard-key  :foreground 'accent-yellow)
      (aaron-ui-set-face 'transient-mismatched-key   :foreground 'accent-red-strong)
      (aaron-ui-set-face 'transient-inapt-suffix     :foreground 'fg-faint :slant 'italic)))
  (aaron-ui-transient--apply-which-key-faces))

(defun aaron-ui-transient--apply-which-key-faces ()
  "Theme which-key faces from the aaron-ui palette."
  (aaron-ui-set-face 'which-key-key-face                   :foreground 'accent-cyan :weight 'medium)
  (aaron-ui-set-face 'which-key-group-description-face     :foreground 'accent-mauve)
  (aaron-ui-set-face 'which-key-command-description-face   :foreground 'fg-main)
  (aaron-ui-set-face 'which-key-separator-face             :foreground 'fg-faint)
  (aaron-ui-set-face 'which-key-note-face                  :foreground 'fg-faint)
  (aaron-ui-set-face 'which-key-local-map-description-face :foreground 'fg-strong)
  (aaron-ui-set-face 'which-key-highlighted-command-face   :foreground 'accent-yellow)
  (aaron-ui-set-face 'which-key-docstring-face             :foreground 'fg-dim))

(add-hook 'after-init-hook              #'aaron-ui-transient-apply-faces)
(add-hook 'server-after-make-frame-hook #'aaron-ui-transient-apply-faces)
(add-hook 'after-load-theme-hook        #'aaron-ui-transient-apply-faces)
(with-eval-after-load 'which-key (aaron-ui-transient--apply-which-key-faces))
(aaron-ui-transient-apply-faces)

(provide 'aaron-ui-transient)
;;; aaron-ui-transient.el ends here
