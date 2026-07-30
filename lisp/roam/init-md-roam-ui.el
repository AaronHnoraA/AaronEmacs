;;; init-md-roam-ui.el --- Noema roam UI (shim onto aaron-ui-board) -*- lexical-binding: t -*-

;;; Commentary:
;; Backward-compatibility shim.  All public symbols delegate to `aaron-ui-board'.
;; `init-md-roam.el' and its ~120 call sites are unchanged.

;;; Code:

(require 'aaron-ui-board)

(declare-function my/noema-roam-todo-done "init-md-roam" ())
(declare-function my/noema-roam-update-todo-status "init-md-roam" (status &optional entry))
(declare-function my/noema-roam-set-todo-priority "init-md-roam" (&optional priority entry))
(declare-function my/noema-roam-set-todo-due "init-md-roam" (&optional due entry))
(declare-function my/noema-roam-set-todo-scheduled "init-md-roam" (&optional scheduled entry))
(declare-function my/noema-roam-set-todo-repeat "init-md-roam" (&optional repeat entry))

;;; --- face aliases ---

(put 'my/noema-roam-ui-title          'face-alias 'aaron-ui-board-title)
(put 'my/noema-roam-ui-subtitle       'face-alias 'aaron-ui-board-subtitle)
(put 'my/noema-roam-ui-section        'face-alias 'aaron-ui-board-section)
(put 'my/noema-roam-ui-row-title      'face-alias 'aaron-ui-board-row-title)
(put 'my/noema-roam-ui-meta           'face-alias 'aaron-ui-board-meta)
(put 'my/noema-roam-ui-detail         'face-alias 'aaron-ui-board-detail)
(put 'my/noema-roam-ui-path           'face-alias 'aaron-ui-board-path)
(put 'my/noema-roam-ui-icon           'face-alias 'aaron-ui-board-icon)
(put 'my/noema-roam-ui-separator      'face-alias 'aaron-ui-board-separator)
(put 'my/noema-roam-ui-badge          'face-alias 'aaron-ui-board-badge)
(put 'my/noema-roam-ui-badge-info     'face-alias 'aaron-ui-board-badge-info)
(put 'my/noema-roam-ui-badge-success  'face-alias 'aaron-ui-board-badge-success)
(put 'my/noema-roam-ui-badge-warning  'face-alias 'aaron-ui-board-badge-warning)
(put 'my/noema-roam-ui-badge-danger   'face-alias 'aaron-ui-board-badge-danger)
(put 'my/noema-roam-ui-badge-muted    'face-alias 'aaron-ui-board-badge-muted)
(put 'my/noema-roam-ui-action         'face-alias 'aaron-ui-board-action)
(put 'my/noema-roam-ui-action-primary 'face-alias 'aaron-ui-board-action-primary)
(put 'my/noema-roam-ui-empty          'face-alias 'aaron-ui-board-empty)
(put 'my/noema-roam-ui-row-highlight  'face-alias 'aaron-ui-board-row-highlight)
(put 'my/noema-roam-ui-header-line    'face-alias 'aaron-ui-board-header-line)
(put 'my/noema-roam-ui-header-status  'face-alias 'aaron-ui-board-header-status)

;;; --- variable aliases ---

(defvaralias 'my/noema-roam-ui--theme-signature  'aaron-ui-board--theme-signature)
(defvaralias 'my/noema-roam-ui--icons            'aaron-ui-board--icons)
(defvaralias 'my/noema-roam-ui-mode-map          'aaron-ui-board-mode-map)
(defvaralias 'my/noema-roam-ui-row-map           'aaron-ui-board-row-map)

;;; --- buffer-local variable aliases ---
;; defvaralias works for buffer-local vars: the alias cell points at the
;; canonical symbol, so set/read via either name operates on the same slot.

(defvaralias 'my/noema-roam-ui-header-title    'aaron-ui-board-header-title)
(defvaralias 'my/noema-roam-ui-header-icon     'aaron-ui-board-header-icon)
(defvaralias 'my/noema-roam-ui-header-status   'aaron-ui-board-header-status)
(defvaralias 'my/noema-roam-ui-refresh-function 'aaron-ui-board-refresh-function)

;;; --- compatibility mode ---

(define-derived-mode my/noema-roam-ui-mode aaron-ui-board-mode "Roam-UI"
  "Backward-compatible mode name for Noema roam board buffers.")

(define-key my/noema-roam-ui-mode-map (kbd "d") #'my/noema-roam-todo-done)
(define-key my/noema-roam-ui-mode-map (kbd "s") #'my/noema-roam-update-todo-status)
(define-key my/noema-roam-ui-mode-map (kbd "p") #'my/noema-roam-set-todo-priority)
(define-key my/noema-roam-ui-mode-map (kbd "D") #'my/noema-roam-set-todo-due)
(define-key my/noema-roam-ui-mode-map (kbd "S") #'my/noema-roam-set-todo-scheduled)
(define-key my/noema-roam-ui-mode-map (kbd "r") #'my/noema-roam-set-todo-repeat)

;;; --- function aliases ---

(defalias 'my/noema-roam-ui-apply-faces       #'aaron-ui-board-apply-faces)
(defalias 'my/noema-roam-ui-icon              #'aaron-ui-board-icon)
(defalias 'my/noema-roam-ui--header-line      #'aaron-ui-board--header-line)
(defalias 'my/noema-roam-ui-set-header        #'aaron-ui-board-set-header)
(defalias 'my/noema-roam-ui-refresh           #'aaron-ui-board-refresh)
(defalias 'my/noema-roam-ui-activate          #'aaron-ui-board-activate)
(defalias 'my/noema-roam-ui-mouse-activate    #'aaron-ui-board-mouse-activate)
(defalias 'my/noema-roam-ui-next-button       #'aaron-ui-board-next-button)
(defalias 'my/noema-roam-ui-previous-button   #'aaron-ui-board-previous-button)
(defalias 'my/noema-roam-ui--tone-face        #'aaron-ui-board--tone-face)
(defalias 'my/noema-roam-ui-insert-badge      #'aaron-ui-board-insert-badge)
(defalias 'my/noema-roam-ui-insert-action     #'aaron-ui-board-insert-action)
(defalias 'my/noema-roam-ui-insert-actions    #'aaron-ui-board-insert-actions)
(defalias 'my/noema-roam-ui-insert-page-header #'aaron-ui-board-insert-page-header)
(defalias 'my/noema-roam-ui-insert-section    #'aaron-ui-board-insert-section)
(defalias 'my/noema-roam-ui-insert-empty      #'aaron-ui-board-insert-empty)
(defalias 'my/noema-roam-ui-insert-field      #'aaron-ui-board-insert-field)
(defun my/noema-roam-ui-insert-row (&rest args)
  "Insert a roam UI row while preserving legacy text properties."
  (let* ((id (plist-get args :id))
         (title-face (plist-get args :title-face))
         (action (plist-get args :action))
         (properties (plist-get args :properties))
         (legacy-properties
          (append `(my/noema-roam-ui-item-id    ,id
                    my/noema-roam-ui-row-action ,action)
                  properties))
         (args (plist-put (copy-sequence args)
                          :properties legacy-properties)))
    (unless title-face
      (setq args (plist-put args :title-face
                            'my/noema-roam-ui-row-title)))
    (apply #'aaron-ui-board-insert-row args)))
(defalias 'my/noema-roam-ui--goto-item-id     #'aaron-ui-board--goto-item-id)
(defalias 'my/noema-roam-ui-goto-first-item   #'aaron-ui-board-goto-first-item)
(defalias 'my/noema-roam-ui-render            #'aaron-ui-board-render)

(provide 'init-md-roam-ui)
;;; init-md-roam-ui.el ends here
