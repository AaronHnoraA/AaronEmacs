;;; init-ratex.el --- RaTeX popup preview integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use ratex.el as an edit-time popup preview backend for Org math fragments.
;; Keep the existing Org/XeLaTeX preview pipeline intact and only add RaTeX's
;; edit-time popup, so Org overlays and RaTeX can coexist.

;;; Code:

(declare-function ratex-mode "ratex" (&optional arg))

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

(defconst my/ratex-root
  (expand-file-name "site-lisp/ratex.el" user-emacs-directory)
  "Local checkout root for ratex.el.")

(defconst my/ratex-font-dir
  (expand-file-name "vendor/ratex-core/fonts" my/ratex-root)
  "Font directory used by the RaTeX backend.")

(defun my/org-ratex-available-p ()
  "Return non-nil when the local ratex.el checkout is available."
  (file-directory-p (expand-file-name "lisp" my/ratex-root)))

(defun my/org-ratex-enable ()
  "Enable popup-only RaTeX previews alongside Org previews."
  (when (and (my/org-ratex-available-p)
             (display-graphic-p)
             (derived-mode-p 'org-mode))
    ;; Keep Org's async preview stack active; RaTeX only adds the edit popup.
    (setq-local my/org-latex--allow-native-preview nil)
    (ratex-mode 1)))

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
        ratex-edit-preview 'posframe
        ratex-debug nil
        ratex-hide-source-while-preview nil
        ratex-inline-preview nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-auto-download-backend t
        ratex-render-color "#d8dee9"
        ratex-posframe-background-color "#2b3140"
        ratex-posframe-border-color "#5f6f8f"
        ratex-posframe-poshandler
        #'ratex-posframe-poshandler-point-top-left-corner-offset)
  :hook (org-mode . my/org-ratex-enable))

(provide 'init-ratex)
;;; init-ratex.el ends here
