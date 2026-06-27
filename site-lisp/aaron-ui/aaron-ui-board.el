;;; aaron-ui-board.el --- Reusable board/dashboard UI toolkit -*- lexical-binding: t -*-

;;; Commentary:
;; A shared, theme-aware rendering toolkit for read-only dashboard/report/hub
;; buffers.  Built on the `aaron-ui' semantic palette.
;;
;; All public symbols live under `aaron-ui-board-*'.  Consumers:
;;   (require 'aaron-ui-board)
;;
;; Available primitives (see each docstring for full args):
;;   Faces: aaron-ui-board-{title,subtitle,section,row-title,meta,detail,path,
;;          icon,separator,badge,badge-{info,success,warning,danger,muted},
;;          action,action-primary,empty,row-highlight,header-line,header-status,
;;          good,warn,bad}
;;   Mode:  aaron-ui-board-mode  (base for all boards)
;;   Header: aaron-ui-board-set-header TITLE ICON &optional STATUS
;;   Render: aaron-ui-board-render RENDERER  (position-preserving refresh)
;;   Insert: aaron-ui-board-insert-page-header  :icon :subtitle :stats :actions
;;           aaron-ui-board-insert-section TITLE &optional COUNT TONE
;;           aaron-ui-board-insert-empty TEXT
;;           aaron-ui-board-insert-field LABEL VALUE &optional FACE
;;           aaron-ui-board-insert-row  :id :icon :badge :title :meta ... :action
;;           aaron-ui-board-insert-badge LABEL &optional TONE
;;           aaron-ui-board-insert-action LABEL COMMAND HELP &optional PRIMARY
;;           aaron-ui-board-insert-actions ACTIONS
;;           aaron-ui-board-insert-metric LABEL VALUE &optional RATIO SUFFIX
;;           aaron-ui-board-insert-openable-path PATH &optional LABEL
;;           aaron-ui-board-insert-key-hints TEXT
;;   Bar:   aaron-ui-board-bar RATIO &optional WIDTH
;;   Level: aaron-ui-board--level-face RATIO
;;   Icons: aaron-ui-board-icon KIND

;;; Code:

(require 'aaron-ui)
(require 'button)
(require 'cl-lib)
(require 'subr-x)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

;;; -----------------------------------------------------------------------
;;; Group

(defgroup aaron-ui-board nil
  "Shared board/dashboard rendering toolkit built on `aaron-ui'."
  :group 'aaron-ui)

;;; -----------------------------------------------------------------------
;;; Faces

(defface aaron-ui-board-title
  '((t (:inherit variable-pitch :weight bold :height 1.35)))
  "Face for board page titles."
  :group 'aaron-ui-board)

(defface aaron-ui-board-subtitle
  '((t (:inherit shadow :height 0.92)))
  "Face for board page subtitles."
  :group 'aaron-ui-board)

(defface aaron-ui-board-section
  '((t (:inherit bold :height 1.02)))
  "Face for board section headings."
  :group 'aaron-ui-board)

(defface aaron-ui-board-row-title
  '((t (:inherit default :weight medium)))
  "Face for primary row text."
  :group 'aaron-ui-board)

(defface aaron-ui-board-meta
  '((t (:inherit shadow :height 0.9)))
  "Face for compact row metadata."
  :group 'aaron-ui-board)

(defface aaron-ui-board-detail
  '((t (:inherit shadow :height 0.92)))
  "Face for secondary row detail text."
  :group 'aaron-ui-board)

(defface aaron-ui-board-path
  '((t (:inherit fixed-pitch :height 0.88)))
  "Face for file paths and identifiers."
  :group 'aaron-ui-board)

(defface aaron-ui-board-icon
  '((t (:inherit default :weight bold)))
  "Face for view and row icons."
  :group 'aaron-ui-board)

(defface aaron-ui-board-separator
  '((t (:inherit shadow :height 0.45)))
  "Face for lightweight separator lines."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge
  '((t (:inherit default :weight medium :height 0.86)))
  "Base face for compact status badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge-info
  '((t (:inherit aaron-ui-board-badge)))
  "Face for informational badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge-success
  '((t (:inherit aaron-ui-board-badge)))
  "Face for success badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge-warning
  '((t (:inherit aaron-ui-board-badge)))
  "Face for warning badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge-danger
  '((t (:inherit aaron-ui-board-badge)))
  "Face for danger badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-badge-muted
  '((t (:inherit aaron-ui-board-badge)))
  "Face for muted badges."
  :group 'aaron-ui-board)

(defface aaron-ui-board-action
  '((t (:inherit button :weight medium :height 0.9)))
  "Face for action toolbar buttons."
  :group 'aaron-ui-board)

(defface aaron-ui-board-action-primary
  '((t (:inherit aaron-ui-board-action :weight bold)))
  "Face for primary action toolbar buttons."
  :group 'aaron-ui-board)

(defface aaron-ui-board-empty
  '((t (:inherit shadow :slant italic)))
  "Face for empty-state placeholder messages."
  :group 'aaron-ui-board)

(defface aaron-ui-board-row-highlight
  '((t (:inherit highlight :extend t)))
  "Face used when hovering or selecting a row."
  :group 'aaron-ui-board)

(defface aaron-ui-board-header-line
  '((t (:inherit header-line :weight medium)))
  "Face for the board header line title."
  :group 'aaron-ui-board)

(defface aaron-ui-board-header-status
  '((t (:inherit header-line :weight regular :height 0.9)))
  "Face for secondary header-line status text."
  :group 'aaron-ui-board)

;; --- metric level faces ---

(defface aaron-ui-board-good
  '((t (:inherit success)))
  "Face for healthy/passing metric values."
  :group 'aaron-ui-board)

(defface aaron-ui-board-warn
  '((t (:inherit warning)))
  "Face for elevated/caution metric values."
  :group 'aaron-ui-board)

(defface aaron-ui-board-bad
  '((t (:inherit error :weight bold)))
  "Face for critical/failing metric values."
  :group 'aaron-ui-board)

;;; -----------------------------------------------------------------------
;;; Theme reapply

(defvar aaron-ui-board--theme-signature nil
  "Last theme signature applied to `aaron-ui-board' faces.")

(defun aaron-ui-board-apply-faces ()
  "Apply the active `aaron-ui' palette to all board faces."
  (let ((signature (list custom-enabled-themes
                         (face-attribute 'default :background nil t)
                         (face-attribute 'default :foreground nil t))))
    (unless (equal signature aaron-ui-board--theme-signature)
      (setq aaron-ui-board--theme-signature signature)
      (aaron-ui-set-face 'aaron-ui-board-title
                         :foreground 'fg-strong)
      (aaron-ui-set-face 'aaron-ui-board-subtitle
                         :foreground 'fg-muted)
      (aaron-ui-set-face 'aaron-ui-board-section
                         :foreground 'accent-cyan
                         :overline 'border-subtle)
      (aaron-ui-set-face 'aaron-ui-board-row-title
                         :foreground 'fg-main)
      (aaron-ui-set-face 'aaron-ui-board-meta
                         :foreground 'fg-muted)
      (aaron-ui-set-face 'aaron-ui-board-detail
                         :foreground 'fg-dim)
      (aaron-ui-set-face 'aaron-ui-board-path
                         :foreground 'fg-faint)
      (aaron-ui-set-face 'aaron-ui-board-icon
                         :foreground 'accent-cyan)
      (aaron-ui-set-face 'aaron-ui-board-separator
                         :foreground 'border-subtle)
      (aaron-ui-set-face 'aaron-ui-board-badge
                         :foreground 'fg-dim
                         :background 'bg-elevated
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'aaron-ui-board-badge-info
                         :foreground 'accent-blue
                         :background 'bg-surface
                         :box '(:line-width (1 . -1) :color border-muted))
      (aaron-ui-set-face 'aaron-ui-board-badge-success
                         :foreground 'accent-green
                         :background 'bg-success
                         :box '(:line-width (1 . -1) :color accent-green-soft))
      (aaron-ui-set-face 'aaron-ui-board-badge-warning
                         :foreground 'accent-yellow
                         :background 'bg-ratex
                         :box '(:line-width (1 . -1) :color accent-yellow-soft))
      (aaron-ui-set-face 'aaron-ui-board-badge-danger
                         :foreground 'accent-red-strong
                         :background 'bg-danger
                         :box '(:line-width (1 . -1) :color accent-red-soft))
      (aaron-ui-set-face 'aaron-ui-board-badge-muted
                         :foreground 'fg-faint
                         :background 'bg-elevated
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'aaron-ui-board-action
                         :foreground 'fg-dim
                         :background 'bg-elevated
                         :underline nil
                         :box '(:line-width (1 . -1) :color border-subtle))
      (aaron-ui-set-face 'aaron-ui-board-action-primary
                         :foreground 'accent-cyan
                         :background 'bg-surface
                         :underline nil
                         :box '(:line-width (1 . -1) :color border-muted))
      (aaron-ui-set-face 'aaron-ui-board-empty
                         :foreground 'fg-faint)
      (aaron-ui-set-face 'aaron-ui-board-row-highlight
                         :foreground 'fg-strong
                         :background 'bg-panel
                         :extend t)
      (aaron-ui-set-face 'aaron-ui-board-header-line
                         :foreground 'fg-strong
                         :background 'bg-elevated
                         :box nil
                         :overline 'border-subtle
                         :underline 'border-subtle)
      (aaron-ui-set-face 'aaron-ui-board-header-status
                         :foreground 'fg-muted
                         :background 'bg-elevated)
      (aaron-ui-set-face 'aaron-ui-board-good
                         :foreground 'accent-green)
      (aaron-ui-set-face 'aaron-ui-board-warn
                         :foreground 'accent-yellow)
      (aaron-ui-set-face 'aaron-ui-board-bad
                         :foreground 'accent-red-strong))))

(add-hook 'after-init-hook #'aaron-ui-board-apply-faces)
(add-hook 'server-after-make-frame-hook #'aaron-ui-board-apply-faces)
(add-hook 'after-load-theme-hook #'aaron-ui-board-apply-faces)

;;; -----------------------------------------------------------------------
;;; Icons

(defconst aaron-ui-board--icons
  '((agenda      "nf-md-calendar_check_outline"       "A")
    (attachment  "nf-md-paperclip"                    "@")
    (backlink    "nf-md-link_variant"                 "<")
    (chart       "nf-md-chart_bar"                    "~")
    (check       "nf-md-check_circle_outline"         "v")
    (clock       "nf-md-clock_outline"                "t")
    (compile     "nf-md-hammer_wrench"                "B")
    (cross       "nf-md-close_circle_outline"         "x")
    (database    "nf-md-database_outline"             "D")
    (diagnostics "nf-md-stethoscope"                  "?")
    (directory   "nf-md-folder_outline"               "/")
    (gear        "nf-md-cog_outline"                  "G")
    (health      "nf-md-heart_pulse"                  "H")
    (image       "nf-md-image_outline"                "I")
    (jupyter     "nf-md-language_python"              "J")
    (kernel      "nf-md-chip"                         "K")
    (lock        "nf-md-lock_outline"                 "L")
    (management  "nf-md-cog_outline"                  "M")
    (new         "nf-md-note_plus_outline"            "+")
    (note        "nf-md-note_text_outline"            "N")
    (package     "nf-md-package_variant_closed"       "P")
    (path        "nf-md-file_outline"                 "f")
    (process     "nf-md-pulse"                        "~")
    (related     "nf-md-vector_link"                  "R")
    (search      "nf-md-magnify"                      "?")
    (section     "nf-md-chevron_right"                ">")
    (server      "nf-md-server_outline"               "S")
    (status      "nf-md-information_outline"          "i")
    (tag         "nf-md-tag_outline"                  "#")
    (template    "nf-md-file_document_edit_outline"   "T")
    (terminal    "nf-md-console"                      "$")
    (toc         "nf-md-file_tree_outline"            "T")
    (todo        "nf-md-checkbox_blank_circle_outline" "o")
    (warning     "nf-md-alert_circle_outline"         "!"))
  "Nerd icon names and text fallbacks for `aaron-ui-board-icon'.")

(defun aaron-ui-board-icon (kind)
  "Return a display icon for KIND with a plain-text fallback."
  (let* ((spec (alist-get kind aaron-ui-board--icons))
         (nerd-name (car spec))
         (fallback (or (cadr spec) "*")))
    (or (when (and nerd-name
                   (display-graphic-p)
                   (fboundp 'nerd-icons-mdicon))
          (condition-case nil
              (nerd-icons-mdicon nerd-name :height 0.9 :v-adjust 0.0)
            (error nil)))
        fallback)))

;;; -----------------------------------------------------------------------
;;; Buffer-local header state

(defvar-local aaron-ui-board-header-title "Board"
  "Title shown in the current board's header line.")

(defvar-local aaron-ui-board-header-icon 'note
  "Icon kind shown in the current board's header line.")

(defvar-local aaron-ui-board-header-status nil
  "Optional status string shown at the right of the board header line.")

(defvar-local aaron-ui-board-refresh-function nil
  "No-argument function used to refresh the current board.")

(defun aaron-ui-board--header-line ()
  "Return the formatted header-line value for the current board."
  (let* ((left (concat
                " "
                (propertize (aaron-ui-board-icon aaron-ui-board-header-icon)
                            'face 'aaron-ui-board-icon)
                "  "
                (propertize aaron-ui-board-header-title
                            'face 'aaron-ui-board-header-line)))
         (status (and aaron-ui-board-header-status
                      (propertize
                       (format "%s  " aaron-ui-board-header-status)
                       'face 'aaron-ui-board-header-status))))
    (if status
        (list left
              (propertize
               " "
               'display `(space :align-to
                                (- right ,(+ 2 (string-width
                                                aaron-ui-board-header-status)))))
              status)
      left)))

(defun aaron-ui-board-set-header (title icon &optional status)
  "Set the current board's header TITLE, ICON kind, and optional STATUS."
  (setq-local aaron-ui-board-header-title title
              aaron-ui-board-header-icon icon
              aaron-ui-board-header-status status)
  (force-mode-line-update t))

;;; -----------------------------------------------------------------------
;;; Base mode and keymaps

(defun aaron-ui-board-ignore-horizontal-wheel (_event)
  "Ignore horizontal wheel EVENT in board buffers."
  (interactive "e"))

(defun aaron-ui-board--bind-horizontal-wheel (map)
  "Bind horizontal wheel events in MAP to stay vertical-only."
  (dolist (event '([wheel-left]
                   [wheel-right]
                   [double-wheel-left]
                   [double-wheel-right]
                   [triple-wheel-left]
                   [triple-wheel-right]))
    (define-key map event #'aaron-ui-board-ignore-horizontal-wheel)))

(defvar aaron-ui-board-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g")         #'aaron-ui-board-refresh)
    (define-key map (kbd "q")         #'quit-window)
    (define-key map (kbd "RET")       #'aaron-ui-board-activate)
    (define-key map (kbd "<return>")  #'aaron-ui-board-activate)
    (define-key map (kbd "TAB")       #'aaron-ui-board-next-button)
    (define-key map (kbd "<backtab>") #'aaron-ui-board-previous-button)
    (define-key map (kbd "j")         #'next-line)
    (define-key map (kbd "n")         #'next-line)
    (define-key map (kbd "k")         #'previous-line)
    (define-key map (kbd "p")         #'previous-line)
    (aaron-ui-board--bind-horizontal-wheel map)
    map)
  "Keymap for `aaron-ui-board-mode'.")

(defvar aaron-ui-board-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]         #'aaron-ui-board-mouse-activate)
    (define-key map (kbd "RET")       #'aaron-ui-board-activate)
    (define-key map (kbd "<return>")  #'aaron-ui-board-activate)
    map)
  "Keymap attached to actionable board rows via the `local-map' text property.")

(define-derived-mode aaron-ui-board-mode special-mode "Board"
  "Base major mode for `aaron-ui' read-only dashboard/hub buffers."
  (setq-local truncate-lines nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local header-line-format '(:eval (aaron-ui-board--header-line)))
  (setq-local cursor-type 'box)
  (setq-local hl-line-face 'aaron-ui-board-row-highlight)
  (hl-line-mode 1))

(with-eval-after-load 'evil
  (evil-set-initial-state 'aaron-ui-board-mode 'emacs))

;;; -----------------------------------------------------------------------
;;; Navigation + activation

(defun aaron-ui-board-refresh ()
  "Refresh the current board by calling `aaron-ui-board-refresh-function'."
  (interactive)
  (if (functionp aaron-ui-board-refresh-function)
      (funcall aaron-ui-board-refresh-function)
    (user-error "This board has no refresh action")))

(defun aaron-ui-board-activate ()
  "Activate the board row or button at point."
  (interactive)
  (let ((action (or (get-text-property (point) 'aaron-ui-board--row-action)
                    (get-text-property (line-beginning-position)
                                       'aaron-ui-board--row-action)
                    (and (> (point) (point-min))
                         (get-text-property (1- (point))
                                            'aaron-ui-board--row-action)))))
    (cond
     (action (funcall action nil))
     ((button-at (point)) (push-button))
     (t (user-error "No board item at point")))))

(defun aaron-ui-board-mouse-activate (event)
  "Activate the board row clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (aaron-ui-board-activate))

(defun aaron-ui-board-next-button ()
  "Move to the next actionable item in the board, wrapping at the end."
  (interactive)
  (forward-button 1 t t))

(defun aaron-ui-board-previous-button ()
  "Move to the previous actionable item in the board, wrapping at the start."
  (interactive)
  (backward-button 1 t t))

;;; -----------------------------------------------------------------------
;;; Position-preserving render

(defun aaron-ui-board--goto-item-id (id)
  "Move point to the row identified by ID; return non-nil on success."
  (when id
    (let ((pos (point-min))
          found)
      (while (and (< pos (point-max)) (not found))
        (if (equal (get-text-property pos 'aaron-ui-board--item-id) id)
            (setq found pos)
          (setq pos (next-single-property-change
                     pos 'aaron-ui-board--item-id nil (point-max)))))
      (when found
        (goto-char found)
        t))))

(defun aaron-ui-board-goto-first-item ()
  "Move point to the first actionable row in the board."
  (goto-char (point-min))
  (when-let* ((pos (text-property-not-all
                    (point-min) (point-max)
                    'aaron-ui-board--item-id nil)))
    (goto-char pos)))

(defun aaron-ui-board-render (renderer)
  "Replace buffer content by calling RENDERER, preserving position."
  (let* ((item-id (get-text-property (point) 'aaron-ui-board--item-id))
         (line (line-number-at-pos))
         (column (current-column))
         (window (get-buffer-window (current-buffer)))
         (win-start (and window (window-start window)))
         (inhibit-read-only t))
    (erase-buffer)
    (funcall renderer)
    (unless (aaron-ui-board--goto-item-id item-id)
      (goto-char (point-min))
      (forward-line (1- (max 1 line)))
      (move-to-column column))
    (when (and window win-start)
      (set-window-start window (min win-start (point-max)) t))))

;;; -----------------------------------------------------------------------
;;; Badge and tone helpers

(defun aaron-ui-board--tone-face (tone)
  "Return the badge face for TONE symbol."
  (pcase tone
    ('danger  'aaron-ui-board-badge-danger)
    ('warning 'aaron-ui-board-badge-warning)
    ('success 'aaron-ui-board-badge-success)
    ('muted   'aaron-ui-board-badge-muted)
    (_        'aaron-ui-board-badge-info)))

(defun aaron-ui-board-insert-badge (label &optional tone)
  "Insert a compact badge with LABEL styled for TONE."
  (insert (propertize (format " %s " label)
                      'face (aaron-ui-board--tone-face tone))))

;;; -----------------------------------------------------------------------
;;; Action toolbar

(defun aaron-ui-board-insert-action (label command help &optional primary)
  "Insert a toolbar button with LABEL that invokes COMMAND.
HELP is the tooltip.  When PRIMARY is non-nil use the primary button face."
  (insert-text-button
   (format " %s " label)
   'action (lambda (_button)
             (if (commandp command)
                 (call-interactively command)
               (funcall command)))
   'follow-link t
   'help-echo help
   'face (if primary
             'aaron-ui-board-action-primary
           'aaron-ui-board-action)))

(defun aaron-ui-board-insert-actions (actions)
  "Insert ACTIONS as a compact inline toolbar.
Each entry is a plist: :label, :command, :help, and optional :primary."
  (let ((first t))
    (dolist (action actions)
      (unless first (insert " "))
      (setq first nil)
      (aaron-ui-board-insert-action
       (plist-get action :label)
       (plist-get action :command)
       (or (plist-get action :help) (plist-get action :label))
       (plist-get action :primary)))))

;;; -----------------------------------------------------------------------
;;; Page-level header

(cl-defun aaron-ui-board-insert-page-header (title &key icon subtitle stats actions)
  "Insert the large page header for TITLE.
ICON is a semantic icon kind (default `note').
SUBTITLE is an optional dim line below the title.
STATS is a list of (LABEL . TONE) pairs rendered as inline badges.
ACTIONS is a list of action plists for the toolbar."
  (insert (propertize (aaron-ui-board-icon (or icon 'note))
                      'face 'aaron-ui-board-icon)
          "  "
          (propertize title 'face 'aaron-ui-board-title)
          "\n")
  (when subtitle
    (insert "   " (propertize subtitle 'face 'aaron-ui-board-subtitle) "\n"))
  (when stats
    (insert "   ")
    (let ((first t))
      (dolist (stat stats)
        (unless first (insert " "))
        (setq first nil)
        (aaron-ui-board-insert-badge (car stat) (cdr stat))))
    (insert "\n"))
  (when actions
    (insert "   ")
    (aaron-ui-board-insert-actions actions)
    (insert "\n"))
  (insert (propertize " " 'face 'aaron-ui-board-separator
                      'display '(space :align-to right)))
  (insert "\n\n"))

;;; -----------------------------------------------------------------------
;;; Section and row primitives

(defun aaron-ui-board-insert-section (title &optional count tone)
  "Insert a section heading for TITLE with optional COUNT badge styled TONE."
  (insert (propertize (format "%s  %s"
                              (aaron-ui-board-icon 'section)
                              title)
                      'face 'aaron-ui-board-section))
  (when count
    (insert "  ")
    (aaron-ui-board-insert-badge (format "%s" count) tone))
  (insert "\n"))

(defun aaron-ui-board-insert-empty (text)
  "Insert an empty-state message TEXT."
  (insert "   " (propertize text 'face 'aaron-ui-board-empty) "\n\n"))

(defun aaron-ui-board-insert-field (label value &optional face)
  "Insert a left-aligned LABEL / VALUE field row.
FACE overrides `aaron-ui-board-row-title' for the value."
  (insert "   "
          (propertize (format "%-16s" label) 'face 'aaron-ui-board-meta)
          (propertize (format "%s" (or value "-"))
                      'face (or face 'aaron-ui-board-row-title))
          "\n"))

(cl-defun aaron-ui-board-insert-row
    (&key id icon badge badge-tone title title-face meta detail tags indent
          action help properties)
  "Insert one compact actionable row and return its buffer range as (START . END).
ID identifies the row across refreshes (used by `aaron-ui-board-render').
ACTION is called with one nil argument when the row is activated.
PROPERTIES is an extra text-property plist applied to the whole row."
  (let* ((start (point))
         (pad (make-string (* 2 (or indent 0)) ?\s)))
    (insert "   " pad
            (propertize (aaron-ui-board-icon (or icon 'note))
                        'face 'aaron-ui-board-icon)
            "  ")
    (when badge
      (aaron-ui-board-insert-badge badge badge-tone)
      (insert "  "))
    (insert (propertize (or title "(untitled)")
                        'face (or title-face 'aaron-ui-board-row-title)))
    (when (and meta (not (string-empty-p (format "%s" meta))))
      (insert "  "
              (propertize (format "%s" meta)
                          'face 'aaron-ui-board-meta)))
    (insert "\n")
    (when (and detail (not (string-empty-p (format "%s" detail))))
      (insert "      " pad
              (propertize (format "%s" detail)
                          'face 'aaron-ui-board-detail)
              "\n"))
    (when tags
      (insert "      " pad)
      (let ((first t))
        (dolist (tag tags)
          (unless first (insert " "))
          (setq first nil)
          (aaron-ui-board-insert-badge (format "#%s" tag) 'muted)))
      (insert "\n"))
    (let ((end (point)))
      (add-text-properties
       start end
       (append `(aaron-ui-board--item-id      ,id
                 aaron-ui-board--row-action   ,action
                 mouse-face                   aaron-ui-board-row-highlight
                 help-echo                    ,(or help "RET/mouse-1: open")
                 keymap                       ,aaron-ui-board-row-map
                 local-map                    ,aaron-ui-board-row-map)
               properties))
      (cons start end))))

;;; -----------------------------------------------------------------------
;;; Metric / bar / level primitives (for performance + status boards)

(defun aaron-ui-board--level-face (ratio)
  "Return the good/warn/bad face for normalized RATIO (0.0–1.0)."
  (cond
   ((>= ratio 0.85) 'aaron-ui-board-bad)
   ((>= ratio 0.55) 'aaron-ui-board-warn)
   (t               'aaron-ui-board-good)))

(defun aaron-ui-board-bar (ratio &optional width)
  "Return a propertized progress bar string for normalized RATIO.
WIDTH defaults to 24 columns."
  (let* ((width (or width 24))
         (ratio (max 0.0 (min 1.0 (or ratio 0.0))))
         (filled (round (* ratio width)))
         (empty (- width filled))
         (face (aaron-ui-board--level-face ratio)))
    (concat "["
            (propertize (make-string filled ?#) 'face face)
            (propertize (make-string empty ?-) 'face 'aaron-ui-board-meta)
            "]")))

(defun aaron-ui-board-insert-metric (label value &optional ratio suffix)
  "Insert a metric row: LABEL VALUE [bar] [SUFFIX].
RATIO (0.0–1.0) enables a progress bar and level colouring.
SUFFIX is appended after the value (e.g. \"MB\" or \"%\")."
  (insert "   "
          (propertize (format "%-20s" label) 'face 'aaron-ui-board-meta))
  (if ratio
      (let ((face (aaron-ui-board--level-face ratio)))
        (insert (propertize (format "%s%s" value (or suffix ""))
                            'face face)
                "  "
                (aaron-ui-board-bar ratio)))
    (insert (propertize (format "%s%s" value (or suffix ""))
                        'face 'aaron-ui-board-row-title)))
  (insert "\n"))

;;; -----------------------------------------------------------------------
;;; Openable-path button

(defun aaron-ui-board-insert-openable-path (path &optional label)
  "Insert PATH as a clickable button that opens the file or directory.
LABEL overrides the displayed text (defaults to the abbreviated path)."
  (let ((display (or label (abbreviate-file-name path))))
    (insert-text-button
     display
     'action (lambda (_b)
               (if (file-directory-p path)
                   (dired path)
                 (find-file path)))
     'follow-link t
     'help-echo path
     'face 'aaron-ui-board-path)))

;;; -----------------------------------------------------------------------
;;; Key-hints line

(defun aaron-ui-board-insert-key-hints (text)
  "Insert a dim key-hints line containing TEXT."
  (insert "   "
          (propertize text 'face 'aaron-ui-board-meta)
          "\n"))

;;; -----------------------------------------------------------------------
;;; Initial face application

(aaron-ui-board-apply-faces)

(provide 'aaron-ui-board)
;;; aaron-ui-board.el ends here
