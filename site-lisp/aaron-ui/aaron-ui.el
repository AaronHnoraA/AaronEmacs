;;; aaron-ui.el --- Local vendored UI theme entrypoint -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keep the Kanagawa base theme in-repo so bootstrapping a fresh Emacs does not
;; need to fetch it from ELPA.  This file also exposes a small semantic palette
;; API so local UI modules can consume named colors instead of scattering raw
;; hex values everywhere.

;;; Code:

(require 'cl-lib)

(defgroup aaron-ui nil
  "Local theme helpers and semantic palette."
  :group 'faces)

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

(defconst aaron-ui-wave-semantic-colors
  '((fg-strong . fg)
    (fg-main . "#E4ECFF")
    (fg-soft . "#D8DEE9")
    (fg-dim . "#A9BED3")
    (fg-overlay . "#90A0C0")
    (fg-muted . "#8B90A8")
    (fg-subtle . "#9BA8C7")
    (fg-faint . "#6F748B")
    (fg-tab . "#8F96AD")
    (fg-tab-separator . "#5F6578")
    (fg-tab-overflow . "#6D7488")
    (accent-blue . "#A9CBFF")
    (accent-cyan . "#9DE7FF")
    (accent-sand . "#E7D6A5")
    (accent-yellow . "#F9E2AF")
    (accent-yellow-soft . "#D8B27F")
    (accent-orange . "#D0A86E")
    (accent-green . "#BBF7B8")
    (accent-green-soft . "#8FBF8F")
    (accent-green-strong . "#A7D7A7")
    (accent-red . "#D79A9A")
    (accent-red-soft . "#BF7F7F")
    (accent-red-strong . "#E4B3B3")
    (accent-lavender . "#B4BEFE")
    (accent-mauve . "#CBA6F7")
    (accent-teal . "#A8F0E3")
    (accent-rose . "#F5E0DC")
    (bg-base . bg)
    (bg-code . "#1E2638")
    (bg-elevated . "#232B3D")
    (bg-elevated-strong . "#2B3549")
    (bg-panel . "#30394E")
    (bg-panel-strong . "#38445C")
    (bg-overlay . "#2B3448")
    (bg-preview . "#293447")
    (bg-popup-separator . "#3D465C")
    (bg-ratex . "#2B3140")
    (bg-success . "#243026")
    (bg-success-strong . "#2A3A2E")
    (bg-danger . "#332628")
    (bg-danger-strong . "#3A2D30")
    (bg-meta . "#253C43")
    (bg-meta-strong . "#2F4C54")
    (bg-surface . "#293448")
    (bg-surface-strong . "#35435C")
    (bg-surface-stronger . "#485875")
    (border-subtle . "#414B61")
    (border-muted . "#56627B")
    (border-popup-separator . "#58627A")
    (border-ratex . "#5F6F8F")
    (guide-line . "#5E81AC")
    (line-number . "#7F849C")
    (line-number-current . fg-strong)
    (line-number-current-bg . bg-p2)
    (line-number-major . accent-yellow-soft))
  "Semantic UI color tokens for the Wave variant.")

(defconst aaron-ui-wave-role-colors
  '((role-critical . accent-red-strong)
    (role-popout . accent-cyan)
    (role-strong . fg-strong)
    (role-salient . accent-blue)
    (role-faded . fg-dim)
    (role-subtle . border-subtle)
    (surface-base . bg-base)
    (surface-raised . bg-elevated)
    (surface-overlay . bg-overlay)
    (surface-panel . bg-panel)
    (surface-selected . bg-visual)
    (state-focus . accent-cyan)
    (state-hover . bg-elevated)
    (state-active . bg-surface-strong)
    (state-disabled . fg-faint)
    (status-success . accent-green)
    (status-warning . accent-yellow)
    (status-error . accent-red-strong)
    (status-info . accent-blue))
  "Elegant semantic roles mapped onto Aaron's Wave palette.")

(defconst aaron-ui-layout-tokens
  '((space-1 . "4px")
    (space-2 . "8px")
    (space-3 . "12px")
    (space-4 . "16px")
    (space-5 . "24px")
    (space-6 . "32px")
    (space-8 . "48px")
    (radius-1 . "2px")
    (radius-2 . "4px")
    (stroke-thin . "1px")
    (motion-fast . "120ms")
    (motion-normal . "180ms")
    (shadow-float . "0 12px 32px rgb(0 0 0 / 28%)"))
  "Cross-host layout tokens used by Emacs and Noema.")

(defconst aaron-ui-css-token-map
  '(("--aaron-role-critical" . role-critical)
    ("--aaron-role-popout" . role-popout)
    ("--aaron-role-strong" . role-strong)
    ("--aaron-role-salient" . role-salient)
    ("--aaron-role-faded" . role-faded)
    ("--aaron-role-subtle" . role-subtle)
    ("--aaron-surface-base" . surface-base)
    ("--aaron-surface-raised" . surface-raised)
    ("--aaron-surface-overlay" . surface-overlay)
    ("--aaron-surface-panel" . surface-panel)
    ("--aaron-surface-selected" . surface-selected)
    ("--aaron-state-focus" . state-focus)
    ("--aaron-state-hover" . state-hover)
    ("--aaron-state-active" . state-active)
    ("--aaron-state-disabled" . state-disabled)
    ("--aaron-status-success" . status-success)
    ("--aaron-status-warning" . status-warning)
    ("--aaron-status-error" . status-error)
    ("--aaron-status-info" . status-info)
    ("--aaron-space-1" . space-1)
    ("--aaron-space-2" . space-2)
    ("--aaron-space-3" . space-3)
    ("--aaron-space-4" . space-4)
    ("--aaron-space-5" . space-5)
    ("--aaron-space-6" . space-6)
    ("--aaron-space-8" . space-8)
    ("--aaron-radius-1" . radius-1)
    ("--aaron-radius-2" . radius-2)
    ("--aaron-stroke-thin" . stroke-thin)
    ("--aaron-motion-fast" . motion-fast)
    ("--aaron-motion-normal" . motion-normal)
    ("--aaron-shadow-float" . shadow-float))
  "CSS custom properties exported for Noema.")

(defcustom aaron-ui-default-variant 'wave
  "Default Kanagawa variant loaded by `aaron-ui-load-theme'."
  :type '(choice (const :tag "Wave" wave)
                 (const :tag "Dragon" dragon)
                 (const :tag "Lotus" lotus))
  :group 'aaron-ui)

(defvar aaron-ui-current-variant aaron-ui-default-variant
  "Variant currently loaded by `aaron-ui-load-theme'.")

(defun aaron-ui--custom-colors-for-variant (variant)
  "Return custom color overrides for VARIANT."
  (pcase variant
    ('wave aaron-ui-wave-custom-colors)
    (_ nil)))

(defun aaron-ui--semantic-colors-for-variant (variant)
  "Return semantic UI colors for VARIANT."
  (pcase variant
    ('wave aaron-ui-wave-semantic-colors)
    (_ aaron-ui-wave-semantic-colors)))

(defun aaron-ui--role-colors-for-variant (variant)
  "Return Elegant semantic role colors for VARIANT."
  (pcase variant
    ('wave aaron-ui-wave-role-colors)
    (_ aaron-ui-wave-role-colors)))

(defun aaron-ui--palette-for-variant (&optional variant)
  "Return the merged palette for VARIANT."
  (let ((variant (or variant aaron-ui-current-variant aaron-ui-default-variant)))
    (append (aaron-ui--role-colors-for-variant variant)
            (aaron-ui--semantic-colors-for-variant variant)
            (aaron-ui--custom-colors-for-variant variant))))

(defun aaron-ui--tokens-for-variant (&optional variant)
  "Return all color and layout tokens for VARIANT."
  (append (aaron-ui--palette-for-variant variant)
          aaron-ui-layout-tokens))

(defun aaron-ui-has-color-p (token &optional variant)
  "Return non-nil when TOKEN is a known palette color for VARIANT."
  (and (symbolp token)
       (assq token (aaron-ui--palette-for-variant variant))))

(defun aaron-ui--resolve-color (token palette seen)
  "Resolve TOKEN against PALETTE, tracking SEEN aliases."
  (cond
   ((stringp token) token)
   ((not (symbolp token)) nil)
   ((memq token seen)
    (error "Circular aaron-ui color alias: %S" (reverse (cons token seen))))
   (t
    (let ((value (alist-get token palette nil nil #'eq)))
      (cond
       ((null value) nil)
       ((stringp value) value)
       ((and (consp value)
             (null (cdr value))
             (stringp (car value)))
        (car value))
       ((symbolp value) (aaron-ui--resolve-color value palette (cons token seen)))
       ((and (consp value)
             (null (cdr value))
             (symbolp (car value)))
        (aaron-ui--resolve-color (car value) palette (cons token seen)))
       (t value))))))

(defun aaron-ui-color (token &optional fallback variant)
  "Return resolved color string for TOKEN in VARIANT.
Return FALLBACK when TOKEN is unknown.  String TOKEN values are returned as-is."
  (or (aaron-ui--resolve-color token (aaron-ui--palette-for-variant variant) nil)
      fallback
      (and (stringp token) token)
      (error "Unknown aaron-ui color token: %S" token)))

(defun aaron-ui-token (token &optional fallback variant)
  "Return resolved design TOKEN for VARIANT, or FALLBACK when unknown."
  (or (aaron-ui--resolve-color
       token (aaron-ui--tokens-for-variant variant) nil)
      fallback
      (and (stringp token) token)
      (error "Unknown aaron-ui design token: %S" token)))

(defun aaron-ui-css-tokens (&optional variant)
  "Return deterministic CSS custom properties for VARIANT."
  (concat
   "/* Generated by aaron-ui-export-css-tokens.  Do not edit by hand. */\n"
   ":root {\n"
   "  color-scheme: dark;\n"
   (mapconcat
    (lambda (entry)
      (format "  %s: %s;"
              (car entry)
              (aaron-ui-token (cdr entry) nil variant)))
    aaron-ui-css-token-map
    "\n")
   "\n}\n"))

(defun aaron-ui-export-css-tokens (file &optional variant)
  "Export Aaron UI CSS tokens for VARIANT to FILE."
  (interactive "FExport Aaron UI CSS tokens to: ")
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file file
    (insert (aaron-ui-css-tokens variant)))
  file)

(defun aaron-ui--resolve-colorish (value)
  "Resolve VALUE if it looks like an `aaron-ui' color token."
  (cond
   ((and (symbolp value) (aaron-ui-has-color-p value))
    (aaron-ui-color value))
   ((plistp value)
    (let ((copy (copy-tree value)))
      (dolist (key '(:color :foreground :background))
        (when (plist-member copy key)
          (setq copy
                (plist-put copy key
                           (aaron-ui--resolve-colorish (plist-get copy key))))))
      copy))
   (t value)))

(defun aaron-ui--resolve-face-attrs (attrs)
  "Resolve palette-backed colors in face attribute list ATTRS."
  (let (resolved)
    (while attrs
      (let ((key (pop attrs))
            (value (pop attrs)))
        (push key resolved)
        (push
         (pcase key
           ((or :foreground :background :distant-foreground)
            (aaron-ui--resolve-colorish value))
           ((or :underline :overline :box)
            (aaron-ui--resolve-colorish value))
           (_ value))
         resolved)))
    (nreverse resolved)))

(defun aaron-ui-set-face (face &rest attrs)
  "Apply ATTRS to FACE, resolving known `aaron-ui' color tokens automatically."
  (when (facep face)
    (apply #'set-face-attribute face nil (aaron-ui--resolve-face-attrs attrs))))

(defun aaron-ui-load-theme (&optional variant)
  "Load local Kanagawa VARIANT from `site-lisp/aaron-ui'."
  (interactive)
  (let* ((variant (or variant aaron-ui-default-variant))
         (theme (intern (format "kanagawa-%s" variant))))
    (setq aaron-ui-current-variant variant)
    (setq kanagawa-themes-comment-italic nil)
    (setq kanagawa-themes-keyword-italic nil)
    (setq kanagawa-themes-custom-colors
          (aaron-ui--custom-colors-for-variant variant))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(provide 'aaron-ui)

;;; aaron-ui.el ends here
