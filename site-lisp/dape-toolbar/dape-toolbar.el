;;; dape-toolbar.el --- Debug toolbar for dape  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zsxh

;; Author: zsxh <bnbvbchen@gmail.com>
;; Maintainer: zsxh <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/dape-toolbar
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.2") (compat "30.1.0.1") (dape "0.26.0") (nerd-icons "0.1.0"))
;; Keywords: convenience tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Toolbar for Dape
;;
;; Basic usage:
;;
;;   (with-eval-after-load 'dape
;;     (dape-toolbar-mode))
;;
;; Customize buttons:
;;
;;   ;; Modify a specific button
;;   (setf (alist-get 'continue dape-toolbar-buttons)
;;         '("nf-cod-debug_continue" dape-continue "Continue" nerd-icons-green))
;;
;;   ;; Add a new button
;;   (push '(my-action . ("nf-cod-run_all" my-command "Run" nerd-icons-green))
;;         dape-toolbar-buttons)
;;
;;   ;; Add a button with visibility predicate
;;   (push '(conditional-btn . ("nf-cod-check" my-command "Check" nerd-icons-blue
;;                             (lambda () (boundp 'some-var))))
;;         dape-toolbar-buttons)
;;
;;   ;; Remove a button
;;   (setq dape-toolbar-buttons
;;         (assq-delete-all 'quit dape-toolbar-buttons))
;;
;; Available button keys: continue, step-over, step-in, step-out, restart, quit, disconnect
;;

;;; Code:

(require 'dape)
(require 'nerd-icons)
(require 'button)


(defgroup dape-toolbar nil
  "Debug toolbar for dape."
  :group 'dape
  :prefix "dape-toolbar-")

;;; Customization

(defcustom dape-toolbar-button-height 1.3
  "Height of toolbar buttons."
  :type 'number
  :group 'dape-toolbar)

(defcustom dape-toolbar-buttons
  '((continue . ("nf-cod-debug_continue" dape-continue "Continue" nerd-icons-blue))
    (step-over . ("nf-cod-debug_step_over" dape-next "Step Over" nerd-icons-blue))
    (step-in . ("nf-cod-debug_step_into" dape-step-in "Step Into" nerd-icons-blue))
    (step-out . ("nf-cod-debug_step_out" dape-step-out "Step Out" nerd-icons-blue))
    (restart . ("nf-cod-debug_restart" dape-restart "Restart" nerd-icons-green))
    (quit . ("nf-cod-debug_stop" dape-quit "Quit" nerd-icons-red))
    (disconnect . ("nf-cod-debug_disconnect" dape-disconnect-quit "Disconnect" nerd-icons-orange)))
  "Alist of toolbar buttons.
Each entry is (KEY . (ICON COMMAND HELP-STRING FACE [PREDICATE])).
PREDICATE is an optional function that returns non-nil to show the button."
  :type
  '(alist :key-type (symbol :tag "Button ID")
          :value-type
          (list :tag "Button Properties"
            (string :tag "Icon")
            (function :tag "Command")
            (string :tag "Help Tooltip")
            (face :tag "Icon Face")
            (choice :tag "Visibility Predicate"
                    :value nil
                    (const :tag "Always Show" nil)
                    (function :tag "Predicate Function"))))
  :group 'dape-toolbar)

;;; Functions

(defun dape-toolbar--show-help (_window _pos action)
  "Show help echo for cursor sensor.

WINDOW, POS, and ACTION are arguments for cursor sensor function."
  (when (eq action 'entered)
    (let ((help (get-text-property (point) 'help-echo)))
      (when help
        (message "%s" help)))))

(defun dape-toolbar--insert-buttons ()
  "Insert toolbar buttons in current buffer."
  (let ((first t))
    (dolist (spec dape-toolbar-buttons)
      (pcase-let ((`(,icon ,command ,help ,face ,predicate) (cdr spec)))
        (when (or (not predicate)
                  (and (functionp predicate) (funcall predicate)))
          (unless first
            (insert "  "))
          (setq first nil)
          (let ((btn-text (condition-case nil
                              (nerd-icons-codicon icon)
                            (error (propertize "?" 'face 'warning)))))
            (insert-text-button
             btn-text
             'action (lambda (_button) (call-interactively command))
             'help-echo help
             'cursor-sensor-functions `(dape-toolbar--show-help)
             'face `(:inherit ,face :height ,dape-toolbar-button-height)
             'mouse-face 'highlight)))))))

(defvar dape-toolbar-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'dape-continue)
    (define-key map "n" #'dape-next)
    (define-key map "s" #'dape-step-in)
    (define-key map "o" #'dape-step-out)
    (define-key map "r" #'dape-restart)
    (define-key map "q" #'dape-quit)
    (define-key map "D" #'dape-disconnect-quit)
    map)
  "Keymap for `dape-toolbar-info-mode'.")

(define-derived-mode dape-toolbar-info-mode dape-info-parent-mode "Toolbar"
  "Major mode for dape debug toolbar."
  :interactive nil
  (cursor-sensor-mode 1)
  (setq-local revert-buffer-function #'ignore)
  (let ((inhibit-read-only t))
    (dape-toolbar--insert-buttons))
  ;; Fit window to content (delay to ensure window is ready)
  (run-at-time 0 nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (let ((win (get-buffer-window buf t)))
                     (when (window-live-p win)
                       (ignore-errors
                         (fit-window-to-buffer win))))))
               (current-buffer)))


;;; Integration

;;;###autoload
(define-minor-mode dape-toolbar-mode
  "Toggle dape toolbar.
When enabled, adds toolbar to `dape-info-buffer-window-groups'."
  :global t
  :group 'dape-toolbar
  (if dape-toolbar-mode
      (progn
        (add-to-list 'dape--info-buffer-name-alist
                     '(dape-toolbar-info-mode . "Toolbar"))
        (add-to-list 'dape-info-buffer-window-groups '(dape-toolbar-info-mode)))
    (setq dape--info-buffer-name-alist
          (assq-delete-all 'dape-toolbar-info-mode dape--info-buffer-name-alist))
    (setq dape-info-buffer-window-groups
          (remove '(dape-toolbar-info-mode) dape-info-buffer-window-groups))))


(provide 'dape-toolbar)
;;; dape-toolbar.el ends here
