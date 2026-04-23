;;; aaron-ui.el --- Local vendored UI theme entrypoint -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keep the Kanagawa base theme in-repo so bootstrapping a fresh Emacs does not
;; need to fetch it from ELPA.  This file is the single local entrypoint for
;; loading the vendored theme files under `site-lisp/aaron-ui/vendor/`.

;;; Code:

(defconst aaron-ui--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the local `aaron-ui' package.")

(defconst aaron-ui-theme-directory
  (expand-file-name "vendor/kanagawa-themes" aaron-ui--root)
  "Directory containing vendored Kanagawa theme files.")

(add-to-list 'load-path aaron-ui-theme-directory)
(add-to-list 'custom-theme-load-path aaron-ui-theme-directory)

(require 'kanagawa-themes)

(defconst aaron-ui-wave-custom-colors
  '((fg "#EEF3FF")
    (fg-dim "#E0E8FF")
    (bg-dim "#252C3E")
    (bg-gutter "#151927")
    (bg-m3 "#121622")
    (bg-m2 "#151927")
    (bg-m1 "#1E2434")
    (bg "#171B28")
    (bg-p1 "#1E2536")
    (bg-p2 "#2A3247")
    (bg-visual "#323A54")
    (bg-search "#2B3A55")
    (pmenu-bg "#1A2234")
    (pmenu-bg-sel "#26334C")
    (pmenu-bg-thumb "#30405C")
    (float-bg "#171B28")
    (float-bg-border "#121622")
    (special "#D8B8FF")
    (nontext "#7A84A3")
    (syn-comment "#B2BDD9")
    (syn-string "#BBF7B8")
    (syn-number "#FFC69C")
    (syn-constant "#FFC69C")
    (syn-identifier "#8FDAFF")
    (syn-parameter "#F4F7FF")
    (syn-fun "#A9CBFF")
    (syn-statement "#D8B8FF")
    (syn-keyword "#D8B8FF")
    (syn-operator "#9DE7FF")
    (syn-preproc "#FF9DB7")
    (syn-type "#A8F0E3")
    (syn-variable "#F4F7FF")
    (syn-regex "#FFD2F2")
    (syn-punct "#C7D0EA")
    (diag-ok "#BBF7B8")
    (diag-error "#FF96B4")
    (diag-warning "#FFE7AA")
    (diag-info "#A9CBFF")
    (diag-hint "#A8F0E3")
    (red "#F38BA8")
    (green "#A6E3A1")
    (yellow "#F9E2AF")
    (blue "#89B4FA")
    (magenta "#CBA6F7")
    (cyan "#74C7EC")
    (bright-red "#FF9DB7")
    (bright-green "#BBF7B8")
    (bright-yellow "#FFE7AA")
    (bright-blue "#A9CBFF")
    (bright-magenta "#D8B8FF")
    (bright-cyan "#A8F0E3")
    (bright-white "#FFF4E8")
    (extend-color-1 "#FFC69C")
    (extend-color-2 "#FFD2F2"))
  "Aaron's high-contrast Wave palette, tuned to match Kitty transparency.")

(defcustom aaron-ui-default-variant 'wave
  "Default Kanagawa variant loaded by `aaron-ui-load-theme'."
  :type '(choice (const :tag "Wave" wave)
                 (const :tag "Dragon" dragon)
                 (const :tag "Lotus" lotus))
  :group 'faces)

(defun aaron-ui--custom-colors-for-variant (variant)
  "Return custom color overrides for VARIANT."
  (pcase variant
    ('wave aaron-ui-wave-custom-colors)
    (_ nil)))

(defun aaron-ui-load-theme (&optional variant)
  "Load local Kanagawa VARIANT from `site-lisp/aaron-ui'."
  (interactive)
  (let* ((variant (or variant aaron-ui-default-variant))
         (theme (intern (format "kanagawa-%s" variant))))
    (setq kanagawa-themes-comment-italic nil)
    (setq kanagawa-themes-keyword-italic nil)
    (setq kanagawa-themes-custom-colors
          (aaron-ui--custom-colors-for-variant variant))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(provide 'aaron-ui)

;;; aaron-ui.el ends here
