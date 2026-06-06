;;; init-md-roam-ui.el --- Native UI primitives for Markdown roam -*- lexical-binding: t -*-

;;; Commentary:
;; Shared, theme-aware rendering helpers for Aaronnote's native Emacs views.

;;; Code:

(require 'aaron-ui)
(require 'button)
(require 'cl-lib)
(require 'subr-x)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

(defgroup my/aaronnote-roam-ui nil
  "Native Emacs UI for Aaronnote Markdown roam tools."
  :group 'my/aaronnote-roam)

(defface my/aaronnote-roam-ui-title
  '((t (:inherit variable-pitch :weight bold :height 1.35)))
  "Face for Aaronnote roam view titles."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-subtitle
  '((t (:inherit shadow :height 0.92)))
  "Face for Aaronnote roam view subtitles."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-section
  '((t (:inherit bold :height 1.02)))
  "Face for Aaronnote roam section headings."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-row-title
  '((t (:inherit default :weight medium)))
  "Face for primary row text."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-meta
  '((t (:inherit shadow :height 0.9)))
  "Face for compact row metadata."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-detail
  '((t (:inherit shadow :height 0.92)))
  "Face for secondary row details."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-path
  '((t (:inherit fixed-pitch :height 0.88)))
  "Face for paths and identifiers."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-icon
  '((t (:inherit default :weight bold)))
  "Face for view and row icons."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-separator
  '((t (:inherit shadow :height 0.45)))
  "Face for lightweight separators."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge
  '((t (:inherit default :weight medium :height 0.86)))
  "Base face for compact status badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge-info
  '((t (:inherit my/aaronnote-roam-ui-badge)))
  "Face for informational badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge-success
  '((t (:inherit my/aaronnote-roam-ui-badge)))
  "Face for success badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge-warning
  '((t (:inherit my/aaronnote-roam-ui-badge)))
  "Face for warning badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge-danger
  '((t (:inherit my/aaronnote-roam-ui-badge)))
  "Face for danger badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-badge-muted
  '((t (:inherit my/aaronnote-roam-ui-badge)))
  "Face for muted badges."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-action
  '((t (:inherit button :weight medium :height 0.9)))
  "Face for action buttons."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-action-primary
  '((t (:inherit my/aaronnote-roam-ui-action :weight bold)))
  "Face for primary action buttons."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-empty
  '((t (:inherit shadow :slant italic)))
  "Face for empty-state messages."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-row-highlight
  '((t (:inherit highlight :extend t)))
  "Face used when hovering or selecting a row."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-header-line
  '((t (:inherit header-line :weight medium)))
  "Face for the Aaronnote roam header line."
  :group 'my/aaronnote-roam-ui)

(defface my/aaronnote-roam-ui-header-status
  '((t (:inherit header-line :weight regular :height 0.9)))
  "Face for secondary header-line status."
  :group 'my/aaronnote-roam-ui)

(defvar my/aaronnote-roam-ui--theme-signature nil
  "Last theme signature applied to Aaronnote roam UI faces.")

(defvar-local my/aaronnote-roam-ui-header-title "Aaronnote Roam"
  "Title shown in the current native roam view header.")

(defvar-local my/aaronnote-roam-ui-header-icon 'note
  "Icon kind shown in the current native roam view header.")

(defvar-local my/aaronnote-roam-ui-header-status nil
  "Optional status shown at the right side of the native roam view header.")

(defvar-local my/aaronnote-roam-ui-refresh-function nil
  "No-argument function used to refresh the current native roam view.")

(defun my/aaronnote-roam-ui-apply-faces ()
  "Apply the active `aaron-ui' palette to native roam UI faces."
  (let ((signature (list custom-enabled-themes
                         (face-attribute 'default :background nil t)
                         (face-attribute 'default :foreground nil t))))
    (unless (equal signature my/aaronnote-roam-ui--theme-signature)
      (setq my/aaronnote-roam-ui--theme-signature signature)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-title
                         :foreground 'fg-strong)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-subtitle
                         :foreground 'fg-muted)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-section
                         :foreground 'accent-cyan
                         :overline 'border-subtle)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-row-title
                         :foreground 'fg-main)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-meta
                         :foreground 'fg-muted)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-detail
                         :foreground 'fg-dim)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-path
                         :foreground 'fg-faint)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-icon
                         :foreground 'accent-cyan)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-separator
                         :foreground 'border-subtle)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge
                         :foreground 'fg-dim
                         :background 'bg-elevated
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge-info
                         :foreground 'accent-blue
                         :background 'bg-surface
                         :box '(:line-width (1 . -1) :color border-muted))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge-success
                         :foreground 'accent-green
                         :background 'bg-success
                         :box '(:line-width (1 . -1) :color accent-green-soft))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge-warning
                         :foreground 'accent-yellow
                         :background 'bg-ratex
                         :box '(:line-width (1 . -1) :color accent-yellow-soft))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge-danger
                         :foreground 'accent-red-strong
                         :background 'bg-danger
                         :box '(:line-width (1 . -1) :color accent-red-soft))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-badge-muted
                         :foreground 'fg-faint
                         :background 'bg-elevated
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-action
                         :foreground 'fg-dim
                         :background 'bg-elevated
                         :underline nil
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-action-primary
                         :foreground 'accent-cyan
                         :background 'bg-surface
                         :underline nil
                         :box '(:line-width (1 . -1) :color border-muted))
      (aaron-ui-set-face 'my/aaronnote-roam-ui-empty
                         :foreground 'fg-faint)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-row-highlight
                         :foreground 'fg-strong
                         :background 'bg-panel
                         :extend t)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-header-line
                         :foreground 'fg-strong
                         :background 'bg-elevated
                         :box nil
                         :overline 'border-subtle
                         :underline 'border-subtle)
      (aaron-ui-set-face 'my/aaronnote-roam-ui-header-status
                         :foreground 'fg-muted
                         :background 'bg-elevated))))

(add-hook 'after-init-hook #'my/aaronnote-roam-ui-apply-faces)
(add-hook 'server-after-make-frame-hook #'my/aaronnote-roam-ui-apply-faces)
(add-hook 'after-load-theme-hook #'my/aaronnote-roam-ui-apply-faces)

(defconst my/aaronnote-roam-ui--icons
  '((agenda "nf-md-calendar_check_outline" "A")
    (backlink "nf-md-link_variant" "<")
    (database "nf-md-database_outline" "D")
    (directory "nf-md-folder_outline" "/")
    (management "nf-md-cog_outline" "M")
    (new "nf-md-note_plus_outline" "+")
    (note "nf-md-note_text_outline" "N")
    (path "nf-md-file_outline" "P")
    (related "nf-md-vector_link" "R")
    (search "nf-md-magnify" "?")
    (section "nf-md-chevron_right" ">")
    (status "nf-md-information_outline" "i")
    (tag "nf-md-tag_outline" "#")
    (template "nf-md-file_document_edit_outline" "T")
    (toc "nf-md-file_tree_outline" "T")
    (todo "nf-md-checkbox_blank_circle_outline" "o")
    (warning "nf-md-alert_circle_outline" "!"))
  "Nerd icon names and text fallbacks used by native roam views.")

(defun my/aaronnote-roam-ui-icon (kind)
  "Return a display icon for KIND with a text fallback."
  (let* ((spec (alist-get kind my/aaronnote-roam-ui--icons))
         (nerd-name (car spec))
         (fallback (or (cadr spec) "*")))
    (or (when (and nerd-name
                   (display-graphic-p)
                   (fboundp 'nerd-icons-mdicon))
          (condition-case nil
              (nerd-icons-mdicon nerd-name :height 0.9 :v-adjust 0.0)
            (error nil)))
        fallback)))

(defun my/aaronnote-roam-ui--header-line ()
  "Return the formatted header line for the current native roam view."
  (let* ((left (concat
                " "
                (propertize (my/aaronnote-roam-ui-icon
                             my/aaronnote-roam-ui-header-icon)
                            'face 'my/aaronnote-roam-ui-icon)
                "  "
                (propertize my/aaronnote-roam-ui-header-title
                            'face 'my/aaronnote-roam-ui-header-line)))
         (status (and my/aaronnote-roam-ui-header-status
                      (propertize
                       (format "%s  " my/aaronnote-roam-ui-header-status)
                       'face 'my/aaronnote-roam-ui-header-status))))
    (if status
        (list left
              (propertize
               " "
               'display `(space :align-to
                                (- right ,(+ 2 (string-width
                                                my/aaronnote-roam-ui-header-status)))))
              status)
      left)))

(defvar my/aaronnote-roam-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'my/aaronnote-roam-ui-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'my/aaronnote-roam-ui-activate)
    (define-key map (kbd "<return>") #'my/aaronnote-roam-ui-activate)
    (define-key map (kbd "TAB") #'my/aaronnote-roam-ui-next-button)
    (define-key map (kbd "<backtab>") #'my/aaronnote-roam-ui-previous-button)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `my/aaronnote-roam-ui-mode'.")

(defvar my/aaronnote-roam-ui-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'my/aaronnote-roam-ui-mouse-activate)
    map)
  "Mouse keymap attached to actionable native roam rows.")

(define-derived-mode my/aaronnote-roam-ui-mode special-mode "Roam-UI"
  "Base mode for Aaronnote's native Emacs workbench views."
  (setq-local truncate-lines t)
  (setq-local header-line-format '(:eval (my/aaronnote-roam-ui--header-line)))
  (setq-local cursor-type 'box)
  (setq-local hl-line-face 'my/aaronnote-roam-ui-row-highlight)
  (hl-line-mode 1))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/aaronnote-roam-ui-mode 'emacs))

(defun my/aaronnote-roam-ui-set-header (title icon &optional status)
  "Set the current native roam view header TITLE, ICON, and STATUS."
  (setq-local my/aaronnote-roam-ui-header-title title
              my/aaronnote-roam-ui-header-icon icon
              my/aaronnote-roam-ui-header-status status)
  (force-mode-line-update t))

(defun my/aaronnote-roam-ui-refresh ()
  "Refresh the current native roam view."
  (interactive)
  (if (functionp my/aaronnote-roam-ui-refresh-function)
      (funcall my/aaronnote-roam-ui-refresh-function)
    (user-error "This Aaronnote roam view has no refresh action")))

(defun my/aaronnote-roam-ui-activate ()
  "Activate the row or button at point in the current native roam view."
  (interactive)
  (let ((action (or (get-text-property
                     (point) 'my/aaronnote-roam-ui-row-action)
                    (get-text-property
                     (line-beginning-position)
                     'my/aaronnote-roam-ui-row-action)
                    (and (> (point) (point-min))
                         (get-text-property
                          (1- (point)) 'my/aaronnote-roam-ui-row-action)))))
    (cond
     (action (funcall action nil))
     ((button-at (point)) (push-button))
     (t (user-error "No Aaronnote roam item at point")))))

(defun my/aaronnote-roam-ui-mouse-activate (event)
  "Activate the native roam row clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (my/aaronnote-roam-ui-activate))

(defun my/aaronnote-roam-ui-next-button ()
  "Move to the next actionable item, wrapping at the end."
  (interactive)
  (forward-button 1 t t))

(defun my/aaronnote-roam-ui-previous-button ()
  "Move to the previous actionable item, wrapping at the beginning."
  (interactive)
  (backward-button 1 t t))

(defun my/aaronnote-roam-ui--tone-face (tone)
  "Return the badge face for TONE."
  (pcase tone
    ('danger 'my/aaronnote-roam-ui-badge-danger)
    ('warning 'my/aaronnote-roam-ui-badge-warning)
    ('success 'my/aaronnote-roam-ui-badge-success)
    ('muted 'my/aaronnote-roam-ui-badge-muted)
    (_ 'my/aaronnote-roam-ui-badge-info)))

(defun my/aaronnote-roam-ui-insert-badge (label &optional tone)
  "Insert compact badge LABEL using TONE."
  (insert (propertize (format " %s " label)
                      'face (my/aaronnote-roam-ui--tone-face tone))))

(defun my/aaronnote-roam-ui-insert-action (label command help &optional primary)
  "Insert an action button with LABEL, COMMAND, HELP, and PRIMARY styling."
  (insert-text-button
   (format " %s " label)
   'action (lambda (_button)
             (if (commandp command)
                 (call-interactively command)
               (funcall command)))
   'follow-link t
   'help-echo help
   'face (if primary
             'my/aaronnote-roam-ui-action-primary
           'my/aaronnote-roam-ui-action)))

(defun my/aaronnote-roam-ui-insert-actions (actions)
  "Insert ACTIONS as a compact toolbar.
Each entry is a plist with :label, :command, :help, and optional :primary."
  (let ((first t))
    (dolist (action actions)
      (unless first (insert " "))
      (setq first nil)
      (my/aaronnote-roam-ui-insert-action
       (plist-get action :label)
       (plist-get action :command)
       (or (plist-get action :help) (plist-get action :label))
       (plist-get action :primary)))))

(cl-defun my/aaronnote-roam-ui-insert-page-header
    (title &key icon subtitle stats actions)
  "Insert a native workbench header for TITLE.
ICON is a semantic icon kind.  SUBTITLE is optional secondary text.
STATS is a list of (LABEL . TONE) pairs.  ACTIONS is a toolbar action list."
  (insert (propertize (my/aaronnote-roam-ui-icon (or icon 'note))
                      'face 'my/aaronnote-roam-ui-icon)
          "  "
          (propertize title 'face 'my/aaronnote-roam-ui-title)
          "\n")
  (when subtitle
    (insert "   " (propertize subtitle 'face 'my/aaronnote-roam-ui-subtitle) "\n"))
  (when stats
    (insert "   ")
    (let ((first t))
      (dolist (stat stats)
        (unless first (insert " "))
        (setq first nil)
        (my/aaronnote-roam-ui-insert-badge (car stat) (cdr stat))))
    (insert "\n"))
  (when actions
    (insert "   ")
    (my/aaronnote-roam-ui-insert-actions actions)
    (insert "\n"))
  (insert (propertize " " 'face 'my/aaronnote-roam-ui-separator
                      'display '(space :align-to right)))
  (insert "\n\n"))

(defun my/aaronnote-roam-ui-insert-section (title &optional count tone)
  "Insert section TITLE with optional COUNT badge and TONE."
  (insert (propertize (format "%s  %s"
                              (my/aaronnote-roam-ui-icon 'section)
                              title)
                      'face 'my/aaronnote-roam-ui-section))
  (when count
    (insert "  ")
    (my/aaronnote-roam-ui-insert-badge (format "%s" count) tone))
  (insert "\n"))

(defun my/aaronnote-roam-ui-insert-empty (text)
  "Insert an empty-state message TEXT."
  (insert "   " (propertize text 'face 'my/aaronnote-roam-ui-empty) "\n\n"))

(defun my/aaronnote-roam-ui-insert-field (label value &optional face)
  "Insert a compact LABEL and VALUE field."
  (insert "   "
          (propertize (format "%-16s" label) 'face 'my/aaronnote-roam-ui-meta)
          (propertize (format "%s" (or value "-"))
                      'face (or face 'my/aaronnote-roam-ui-row-title))
          "\n"))

(cl-defun my/aaronnote-roam-ui-insert-row
    (&key id icon badge badge-tone title title-face meta detail tags indent
          action help properties)
  "Insert one compact actionable row and return its buffer range.
ID identifies the row across refreshes.  ACTION is called with one nil argument.
PROPERTIES is an additional text-property plist applied to the whole row."
  (let* ((start (point))
         (indent-string (make-string (* 2 (or indent 0)) ?\s)))
    (insert "   " indent-string
            (propertize (my/aaronnote-roam-ui-icon (or icon 'note))
                        'face 'my/aaronnote-roam-ui-icon)
            "  ")
    (when badge
      (my/aaronnote-roam-ui-insert-badge badge badge-tone)
      (insert "  "))
    (insert (propertize (or title "(untitled)")
                        'face (or title-face
                                  'my/aaronnote-roam-ui-row-title)))
    (when (and meta (not (string-empty-p (format "%s" meta))))
      (insert "  "
              (propertize (format "%s" meta)
                          'face 'my/aaronnote-roam-ui-meta)))
    (insert "\n")
    (when (and detail (not (string-empty-p (format "%s" detail))))
      (insert "      " indent-string
              (propertize (format "%s" detail)
                          'face 'my/aaronnote-roam-ui-detail)
              "\n"))
    (when tags
      (insert "      " indent-string)
      (let ((first t))
        (dolist (tag tags)
          (unless first (insert " "))
          (setq first nil)
          (my/aaronnote-roam-ui-insert-badge (format "#%s" tag) 'muted)))
      (insert "\n"))
    (let ((end (point)))
      (add-text-properties
       start end
       (append `(my/aaronnote-roam-ui-item-id ,id
                 my/aaronnote-roam-ui-row-action ,action
                 mouse-face my/aaronnote-roam-ui-row-highlight
                 help-echo ,(or help "RET/mouse-1: open")
                 keymap ,my/aaronnote-roam-ui-row-map
                 local-map ,my/aaronnote-roam-ui-row-map)
               properties))
      (cons start end))))

(defun my/aaronnote-roam-ui--goto-item-id (id)
  "Move point to the row identified by ID, returning non-nil on success."
  (when id
    (let ((position (point-min))
          found)
      (while (and (< position (point-max)) (not found))
        (if (equal (get-text-property
                    position 'my/aaronnote-roam-ui-item-id)
                   id)
            (setq found position)
          (setq position
                (next-single-property-change
                 position 'my/aaronnote-roam-ui-item-id nil (point-max)))))
      (when found
        (goto-char found)
        t))))

(defun my/aaronnote-roam-ui-goto-first-item ()
  "Move point to the first actionable native roam row."
  (goto-char (point-min))
  (when-let* ((position (text-property-not-all
                         (point-min) (point-max)
                         'my/aaronnote-roam-ui-item-id nil)))
    (goto-char position)))

(defun my/aaronnote-roam-ui-render (renderer)
  "Replace the current buffer by calling RENDERER while preserving position."
  (let* ((item-id (get-text-property (point) 'my/aaronnote-roam-ui-item-id))
         (line (line-number-at-pos))
         (column (current-column))
         (window (get-buffer-window (current-buffer)))
         (window-start-position (and window (window-start window)))
         (inhibit-read-only t))
    (erase-buffer)
    (funcall renderer)
    (unless (my/aaronnote-roam-ui--goto-item-id item-id)
      (goto-char (point-min))
      (forward-line (1- (max 1 line)))
      (move-to-column column))
    (when (and window window-start-position)
      (set-window-start window
                        (min window-start-position (point-max))
                        t))))

(my/aaronnote-roam-ui-apply-faces)

(provide 'init-md-roam-ui)
;;; init-md-roam-ui.el ends here
