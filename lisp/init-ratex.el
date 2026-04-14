;;; init-ratex.el --- RaTeX popup preview integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use ratex.el as an edit-time popup preview backend for Org math fragments.
;; Keep the existing Org/XeLaTeX preview pipeline intact and only add RaTeX's
;; edit-time popup, so Org overlays and RaTeX can coexist.

;;; Code:

(require 'cl-lib)

(declare-function ratex-mode "ratex" (&optional arg))
(declare-function ratex--active-fragment-at-point "ratex-render")
(declare-function ratex-handle-buffer-switch "ratex-render")
(declare-function ratex-handle-post-command "ratex-render")
(declare-function ratex--hide-edit-preview "ratex-render")
(declare-function evil-insert-state-p "evil")

(defvar my/org-latex--allow-native-preview)
(defvar ratex-auto-download-backend)
(defvar ratex-backend-root)
(defvar ratex-edit-preview)
(defvar ratex-debug)
(defvar ratex-font-size)
(defvar ratex-font-dir)
(defvar ratex-hide-source-while-preview)
(defvar ratex-inline-preview)
(defvar ratex-posframe-background-color)
(defvar ratex-posframe-border-color)
(defvar ratex-render-cache-limit)
(defvar ratex-render-cache-ttl)
(defvar ratex-render-color)
(defvar-local ratex-mode)
(defvar-local ratex--active-fragment)
(defvar-local my/ratex-preview-timer nil)

(defcustom my/ratex-evil-insert-only t
  "When non-nil, only show RaTeX edit previews in Evil insert state."
  :type 'boolean)

(defcustom my/ratex-preview-style 'posframe
  "Preview UI used for RaTeX edit previews in conservative mode."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Minibuffer" minibuffer)
                 (const :tag "Posframe" posframe)))

(defcustom my/ratex-preview-idle-delay 0.15
  "Idle delay before running RaTeX preview updates."
  :type 'number)

(defconst my/ratex-root
  (expand-file-name "site-lisp/ratex.el" user-emacs-directory)
  "Local checkout root for ratex.el.")

(defconst my/ratex-font-dir
  (expand-file-name "vendor/ratex-core/fonts" my/ratex-root)
  "Font directory used by the RaTeX backend.")

(defun my/org-ratex-available-p ()
  "Return non-nil when the local ratex.el checkout is available."
  (file-directory-p (expand-file-name "lisp" my/ratex-root)))

(defun my/org-ratex-preview-active-p ()
  "Return non-nil when RaTeX edit preview should be active now."
  (or (not my/ratex-evil-insert-only)
      (not (bound-and-true-p evil-local-mode))
      (evil-insert-state-p)))

(defun my/ratex--debounced-post-command (orig-fn &rest args)
  "Run `ratex-handle-post-command' through a short idle debounce."
  (my/ratex-cancel-pending-preview)
  (if (or (not (derived-mode-p 'org-mode))
          (not (bound-and-true-p ratex-mode))
          (not (my/org-ratex-preview-active-p))
          (null (ignore-errors (ratex--active-fragment-at-point))))
      (my/ratex-hide-preview-now)
    (setq-local
     my/ratex-preview-timer
     (run-with-idle-timer
      my/ratex-preview-idle-delay nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq my/ratex-preview-timer nil)
            (when (and (derived-mode-p 'org-mode)
                       (bound-and-true-p ratex-mode)
                       (my/org-ratex-preview-active-p))
              (condition-case nil
                  (apply orig-fn args)
                (error
                 (my/ratex-hide-preview-now)))))))
      (current-buffer)))))

(defun my/ratex-cancel-pending-preview ()
  "Cancel any queued RaTeX preview timer in the current buffer."
  (when (timerp my/ratex-preview-timer)
    (cancel-timer my/ratex-preview-timer)
    (setq-local my/ratex-preview-timer nil)))

(defun my/ratex-hide-preview-now ()
  "Cancel pending preview work and hide any visible RaTeX preview."
  (my/ratex-cancel-pending-preview)
  (setq-local ratex--active-fragment nil)
  (when (fboundp 'ratex--hide-edit-preview)
    (ratex--hide-edit-preview)))

(defun my/ratex-handle-buffer-switch (orig-fn &rest args)
  "Clear pending/visible RaTeX previews before delegating to ORIG-FN."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (bound-and-true-p ratex-mode)
          (my/ratex-hide-preview-now)))))
  (apply orig-fn args))

(defun my/org-ratex-sync-evil-state ()
  "Enable RaTeX edit preview only when the current Evil state should show it."
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p ratex-mode))
    (if (my/org-ratex-preview-active-p)
        (progn
          (setq-local ratex-edit-preview my/ratex-preview-style)
          (when (fboundp 'ratex-handle-post-command)
            (ratex-handle-post-command)))
      (setq-local ratex-edit-preview nil)
      (my/ratex-hide-preview-now))))

(defun my/org-ratex-enable ()
  "Enable popup-only RaTeX previews alongside Org previews."
  (when (and (my/org-ratex-available-p)
             (display-graphic-p)
             (derived-mode-p 'org-mode))
    ;; Keep Org's async preview stack active; RaTeX only adds the edit popup.
    (setq-local my/org-latex--allow-native-preview nil)
    (ratex-mode 1)
    (my/org-ratex-sync-evil-state)))

(use-package ratex
  :if (my/org-ratex-available-p)
  :load-path "~/.emacs.d/site-lisp/ratex.el/lisp"
  :commands (ratex-mode
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend)
  :init
  (setq ratex-backend-root my/ratex-root
        ratex-font-dir my/ratex-font-dir
        ratex-font-size 24.0
        ratex-edit-preview my/ratex-preview-style
        ratex-debug nil
        ratex-hide-source-while-preview nil
        ratex-inline-preview nil
        ratex-render-cache-limit 12
        ratex-render-cache-ttl 30
        ratex-auto-download-backend t
        ratex-render-color "#d8dee9"
        ratex-posframe-background-color "#2b3140"
        ratex-posframe-border-color "#5f6f8f"
        ratex-posframe-poshandler
        #'ratex-posframe-poshandler-point-top-left-corner-offset)
  :hook (org-mode . my/org-ratex-enable))

(with-eval-after-load 'ratex-render
  (advice-add 'ratex-handle-buffer-switch :around #'my/ratex-handle-buffer-switch)
  (advice-add 'ratex-handle-post-command :around #'my/ratex--debounced-post-command))

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my/org-ratex-sync-evil-state)
  (add-hook 'evil-insert-state-exit-hook #'my/org-ratex-sync-evil-state))

(provide 'init-ratex)
;;; init-ratex.el ends here
