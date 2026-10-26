;;; noema/roam/ui.el --- Noema roam UI (shim onto aaron-ui-board) -*- lexical-binding: t -*-

;;; Commentary:
;; Backward-compatibility shim.  All public symbols delegate to `aaron-ui-board'.
;; `noema/roam/config.el' and its ~120 call sites are unchanged.

;;; Code:

(require 'aaron-ui-board)

(declare-function +noema-roam/todo-done "noema/roam/config" ())
(declare-function +noema-roam/update-todo-status "noema/roam/config" (status &optional entry))
(declare-function +noema-roam/set-todo-priority "noema/roam/config" (&optional priority entry))
(declare-function +noema-roam/set-todo-due "noema/roam/config" (&optional due entry))
(declare-function +noema-roam/set-todo-scheduled "noema/roam/config" (&optional scheduled entry))
(declare-function +noema-roam/set-todo-repeat "noema/roam/config" (&optional repeat entry))

;;; --- face aliases ---

(put '+noema-roam-ui-title          'face-alias 'aaron-ui-board-title)
(put '+noema-roam-ui-subtitle       'face-alias 'aaron-ui-board-subtitle)
(put '+noema-roam-ui-section        'face-alias 'aaron-ui-board-section)
(put '+noema-roam-ui-row-title      'face-alias 'aaron-ui-board-row-title)
(put '+noema-roam-ui-meta           'face-alias 'aaron-ui-board-meta)
(put '+noema-roam-ui-detail         'face-alias 'aaron-ui-board-detail)
(put '+noema-roam-ui-path           'face-alias 'aaron-ui-board-path)
(put '+noema-roam-ui-icon           'face-alias 'aaron-ui-board-icon)
(put '+noema-roam-ui-separator      'face-alias 'aaron-ui-board-separator)
(put '+noema-roam-ui-badge          'face-alias 'aaron-ui-board-badge)
(put '+noema-roam-ui-badge-info     'face-alias 'aaron-ui-board-badge-info)
(put '+noema-roam-ui-badge-success  'face-alias 'aaron-ui-board-badge-success)
(put '+noema-roam-ui-badge-warning  'face-alias 'aaron-ui-board-badge-warning)
(put '+noema-roam-ui-badge-danger   'face-alias 'aaron-ui-board-badge-danger)
(put '+noema-roam-ui-badge-muted    'face-alias 'aaron-ui-board-badge-muted)
(put '+noema-roam-ui-action         'face-alias 'aaron-ui-board-action)
(put '+noema-roam-ui-action-primary 'face-alias 'aaron-ui-board-action-primary)
(put '+noema-roam-ui-empty          'face-alias 'aaron-ui-board-empty)
(put '+noema-roam-ui-row-highlight  'face-alias 'aaron-ui-board-row-highlight)
(put '+noema-roam-ui-header-line    'face-alias 'aaron-ui-board-header-line)
(put '+noema-roam-ui-header-status  'face-alias 'aaron-ui-board-header-status)

;;; --- variable aliases ---

(defvaralias '+noema-roam-ui--theme-signature  'aaron-ui-board--theme-signature)
(defvaralias '+noema-roam-ui--icons            'aaron-ui-board--icons)

(defvaralias '+noema-roam-ui-row-map           'aaron-ui-board-row-map)

;;; --- buffer-local variable aliases ---
;; defvaralias works for buffer-local vars: the alias cell points at the
;; canonical symbol, so set/read via either name operates on the same slot.

(defvaralias '+noema-roam-ui-header-title    'aaron-ui-board-header-title)
(defvaralias '+noema-roam-ui-header-icon     'aaron-ui-board-header-icon)
(defvaralias '+noema-roam-ui-header-status   'aaron-ui-board-header-status)
(defvaralias '+noema-roam-ui-refresh-function 'aaron-ui-board-refresh-function)

;;; --- compatibility mode ---

(define-derived-mode +noema-roam-ui-mode aaron-ui-board-mode "Roam-UI"
  "Backward-compatible mode name for Noema roam board buffers.")

;; These todo keys belong to roam boards only.  `+noema-roam-ui-mode-map'
;; used to be a `defvaralias' onto `aaron-ui-board-mode-map', which made
;; `define-derived-mode' a no-op for the map and installed them into the
;; keymap shared by every board in the config — compile, git, jupyter,
;; health, diagnostics, performance, lsp.  `p' even shadowed `previous-line'
;; there.  The derived mode now owns a real child map.
(define-key +noema-roam-ui-mode-map (kbd "d") #'+noema-roam/todo-done)
(define-key +noema-roam-ui-mode-map (kbd "s") #'+noema-roam/update-todo-status)
(define-key +noema-roam-ui-mode-map (kbd "p") #'+noema-roam/set-todo-priority)
(define-key +noema-roam-ui-mode-map (kbd "D") #'+noema-roam/set-todo-due)
(define-key +noema-roam-ui-mode-map (kbd "S") #'+noema-roam/set-todo-scheduled)
(define-key +noema-roam-ui-mode-map (kbd "r") #'+noema-roam/set-todo-repeat)

;; Matches the convention every other board in this config follows: jupyter
;; (`tools/jupyter/config.el'), compile, lsp-tools and diagnostics all answer
;; `?' with their own help/dispatch surface.
(declare-function +noema-roam/help "noema/roam/config" ())
(define-key +noema-roam-ui-mode-map (kbd "?") #'+noema-roam/help)

;;; --- function aliases ---

(defalias '+noema-roam-ui-apply-faces       #'aaron-ui-board-apply-faces)
(defalias '+noema-roam-ui-icon              #'aaron-ui-board-icon)
(defalias '+noema-roam-ui--header-line      #'aaron-ui-board--header-line)
(defalias '+noema-roam-ui-set-header        #'aaron-ui-board-set-header)
(defalias '+noema-roam/ui-refresh           #'aaron-ui-board-refresh)
(defalias '+noema-roam/ui-activate          #'aaron-ui-board-activate)
(defalias '+noema-roam/ui-mouse-activate    #'aaron-ui-board-mouse-activate)
(defalias '+noema-roam/ui-next-button       #'aaron-ui-board-next-button)
(defalias '+noema-roam/ui-previous-button   #'aaron-ui-board-previous-button)
(defalias '+noema-roam-ui--tone-face        #'aaron-ui-board--tone-face)
(defalias '+noema-roam-ui-insert-badge      #'aaron-ui-board-insert-badge)
(defalias '+noema-roam-ui-insert-action     #'aaron-ui-board-insert-action)
(defalias '+noema-roam-ui-insert-actions    #'aaron-ui-board-insert-actions)
(defalias '+noema-roam-ui-insert-page-header #'aaron-ui-board-insert-page-header)
(defalias '+noema-roam-ui-insert-section    #'aaron-ui-board-insert-section)
(defalias '+noema-roam-ui-insert-empty      #'aaron-ui-board-insert-empty)
(defalias '+noema-roam-ui-insert-field      #'aaron-ui-board-insert-field)
(defun +noema-roam-ui-insert-row (&rest args)
  "Insert a roam UI row while preserving legacy text properties."
  (let* ((id (plist-get args :id))
         (title-face (plist-get args :title-face))
         (action (plist-get args :action))
         (properties (plist-get args :properties))
         (legacy-properties
          (append `(+noema-roam-ui-item-id    ,id
                    +noema-roam-ui-row-action ,action)
                  properties))
         (args (plist-put (copy-sequence args)
                          :properties legacy-properties)))
    (unless title-face
      (setq args (plist-put args :title-face
                            '+noema-roam-ui-row-title)))
    (apply #'aaron-ui-board-insert-row args)))
(defalias '+noema-roam-ui--goto-item-id     #'aaron-ui-board--goto-item-id)
(defalias '+noema-roam-ui-goto-first-item   #'aaron-ui-board-goto-first-item)
(defalias '+noema-roam-ui-render            #'aaron-ui-board-render)

(provide 'noema/roam/ui)
;;; noema/roam/ui.el ends here
