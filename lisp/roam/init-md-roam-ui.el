;;; init-md-roam-ui.el --- Aaronnote roam UI (shim onto aaron-ui-board) -*- lexical-binding: t -*-

;;; Commentary:
;; Backward-compatibility shim.  All public symbols delegate to `aaron-ui-board'.
;; `init-md-roam.el' and its ~120 call sites are unchanged.

;;; Code:

(require 'aaron-ui-board)

(declare-function my/aaronnote-roam-todo-done "init-md-roam" ())
(declare-function my/aaronnote-roam-update-todo-status "init-md-roam" (status &optional entry))
(declare-function my/aaronnote-roam-set-todo-priority "init-md-roam" (&optional priority entry))
(declare-function my/aaronnote-roam-set-todo-due "init-md-roam" (&optional due entry))
(declare-function my/aaronnote-roam-set-todo-scheduled "init-md-roam" (&optional scheduled entry))
(declare-function my/aaronnote-roam-set-todo-repeat "init-md-roam" (&optional repeat entry))

;;; --- face aliases ---

(put 'my/aaronnote-roam-ui-title          'face-alias 'aaron-ui-board-title)
(put 'my/aaronnote-roam-ui-subtitle       'face-alias 'aaron-ui-board-subtitle)
(put 'my/aaronnote-roam-ui-section        'face-alias 'aaron-ui-board-section)
(put 'my/aaronnote-roam-ui-row-title      'face-alias 'aaron-ui-board-row-title)
(put 'my/aaronnote-roam-ui-meta           'face-alias 'aaron-ui-board-meta)
(put 'my/aaronnote-roam-ui-detail         'face-alias 'aaron-ui-board-detail)
(put 'my/aaronnote-roam-ui-path           'face-alias 'aaron-ui-board-path)
(put 'my/aaronnote-roam-ui-icon           'face-alias 'aaron-ui-board-icon)
(put 'my/aaronnote-roam-ui-separator      'face-alias 'aaron-ui-board-separator)
(put 'my/aaronnote-roam-ui-badge          'face-alias 'aaron-ui-board-badge)
(put 'my/aaronnote-roam-ui-badge-info     'face-alias 'aaron-ui-board-badge-info)
(put 'my/aaronnote-roam-ui-badge-success  'face-alias 'aaron-ui-board-badge-success)
(put 'my/aaronnote-roam-ui-badge-warning  'face-alias 'aaron-ui-board-badge-warning)
(put 'my/aaronnote-roam-ui-badge-danger   'face-alias 'aaron-ui-board-badge-danger)
(put 'my/aaronnote-roam-ui-badge-muted    'face-alias 'aaron-ui-board-badge-muted)
(put 'my/aaronnote-roam-ui-action         'face-alias 'aaron-ui-board-action)
(put 'my/aaronnote-roam-ui-action-primary 'face-alias 'aaron-ui-board-action-primary)
(put 'my/aaronnote-roam-ui-empty          'face-alias 'aaron-ui-board-empty)
(put 'my/aaronnote-roam-ui-row-highlight  'face-alias 'aaron-ui-board-row-highlight)
(put 'my/aaronnote-roam-ui-header-line    'face-alias 'aaron-ui-board-header-line)
(put 'my/aaronnote-roam-ui-header-status  'face-alias 'aaron-ui-board-header-status)

;;; --- variable aliases ---

(defvaralias 'my/aaronnote-roam-ui--theme-signature  'aaron-ui-board--theme-signature)
(defvaralias 'my/aaronnote-roam-ui--icons            'aaron-ui-board--icons)
(defvaralias 'my/aaronnote-roam-ui-mode-map          'aaron-ui-board-mode-map)
(defvaralias 'my/aaronnote-roam-ui-row-map           'aaron-ui-board-row-map)

;;; --- buffer-local variable aliases ---
;; defvaralias works for buffer-local vars: the alias cell points at the
;; canonical symbol, so set/read via either name operates on the same slot.

(defvaralias 'my/aaronnote-roam-ui-header-title    'aaron-ui-board-header-title)
(defvaralias 'my/aaronnote-roam-ui-header-icon     'aaron-ui-board-header-icon)
(defvaralias 'my/aaronnote-roam-ui-header-status   'aaron-ui-board-header-status)
(defvaralias 'my/aaronnote-roam-ui-refresh-function 'aaron-ui-board-refresh-function)

;;; --- compatibility mode ---

(define-derived-mode my/aaronnote-roam-ui-mode aaron-ui-board-mode "Roam-UI"
  "Backward-compatible mode name for Aaronnote roam board buffers.")

(define-key my/aaronnote-roam-ui-mode-map (kbd "d") #'my/aaronnote-roam-todo-done)
(define-key my/aaronnote-roam-ui-mode-map (kbd "s") #'my/aaronnote-roam-update-todo-status)
(define-key my/aaronnote-roam-ui-mode-map (kbd "p") #'my/aaronnote-roam-set-todo-priority)
(define-key my/aaronnote-roam-ui-mode-map (kbd "D") #'my/aaronnote-roam-set-todo-due)
(define-key my/aaronnote-roam-ui-mode-map (kbd "S") #'my/aaronnote-roam-set-todo-scheduled)
(define-key my/aaronnote-roam-ui-mode-map (kbd "r") #'my/aaronnote-roam-set-todo-repeat)

;;; --- function aliases ---

(defalias 'my/aaronnote-roam-ui-apply-faces       #'aaron-ui-board-apply-faces)
(defalias 'my/aaronnote-roam-ui-icon              #'aaron-ui-board-icon)
(defalias 'my/aaronnote-roam-ui--header-line      #'aaron-ui-board--header-line)
(defalias 'my/aaronnote-roam-ui-set-header        #'aaron-ui-board-set-header)
(defalias 'my/aaronnote-roam-ui-refresh           #'aaron-ui-board-refresh)
(defalias 'my/aaronnote-roam-ui-activate          #'aaron-ui-board-activate)
(defalias 'my/aaronnote-roam-ui-mouse-activate    #'aaron-ui-board-mouse-activate)
(defalias 'my/aaronnote-roam-ui-next-button       #'aaron-ui-board-next-button)
(defalias 'my/aaronnote-roam-ui-previous-button   #'aaron-ui-board-previous-button)
(defalias 'my/aaronnote-roam-ui--tone-face        #'aaron-ui-board--tone-face)
(defalias 'my/aaronnote-roam-ui-insert-badge      #'aaron-ui-board-insert-badge)
(defalias 'my/aaronnote-roam-ui-insert-action     #'aaron-ui-board-insert-action)
(defalias 'my/aaronnote-roam-ui-insert-actions    #'aaron-ui-board-insert-actions)
(defalias 'my/aaronnote-roam-ui-insert-page-header #'aaron-ui-board-insert-page-header)
(defalias 'my/aaronnote-roam-ui-insert-section    #'aaron-ui-board-insert-section)
(defalias 'my/aaronnote-roam-ui-insert-empty      #'aaron-ui-board-insert-empty)
(defalias 'my/aaronnote-roam-ui-insert-field      #'aaron-ui-board-insert-field)
(defun my/aaronnote-roam-ui-insert-row (&rest args)
  "Insert a roam UI row while preserving legacy text properties."
  (let* ((id (plist-get args :id))
         (title-face (plist-get args :title-face))
         (action (plist-get args :action))
         (properties (plist-get args :properties))
         (legacy-properties
          (append `(my/aaronnote-roam-ui-item-id    ,id
                    my/aaronnote-roam-ui-row-action ,action)
                  properties))
         (args (plist-put (copy-sequence args)
                          :properties legacy-properties)))
    (unless title-face
      (setq args (plist-put args :title-face
                            'my/aaronnote-roam-ui-row-title)))
    (apply #'aaron-ui-board-insert-row args)))
(defalias 'my/aaronnote-roam-ui--goto-item-id     #'aaron-ui-board--goto-item-id)
(defalias 'my/aaronnote-roam-ui-goto-first-item   #'aaron-ui-board-goto-first-item)
(defalias 'my/aaronnote-roam-ui-render            #'aaron-ui-board-render)

(provide 'init-md-roam-ui)
;;; init-md-roam-ui.el ends here
