;;; init-ratex.el --- RaTeX popup preview integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use ratex.el as an edit-time popup preview backend for Org math fragments.
;; Keep the existing XeLaTeX export pipeline intact, but disable the in-buffer
;; Org preview overlays in buffers where RaTeX takes over.

;;; Code:

(declare-function org-clear-latex-preview "org" (&optional beg end))
(declare-function org-fragtog-mode "org-fragtog" (&optional arg))
(declare-function ratex-mode "ratex" (&optional arg))

(defvar my/org-latex--allow-native-preview)
(defvar ratex-auto-download-backend)
(defvar ratex-backend-root)
(defvar ratex-edit-preview)
(defvar ratex-font-dir)
(defvar ratex-inline-preview)
(defvar ratex-posframe-background-color)
(defvar ratex-posframe-border-color)
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

(defun my/org-ratex-disable-other-preview-systems ()
  "Disable Org math preview systems that would conflict with RaTeX."
  (setq-local my/org-latex--allow-native-preview t)
  (when (fboundp 'org-fragtog-mode)
    (org-fragtog-mode -1))
  (when (fboundp 'org-clear-latex-preview)
    (org-clear-latex-preview (point-min) (point-max))))

(defun my/org-ratex-enable ()
  "Enable popup-only RaTeX previews in the current Org buffer."
  (when (and (my/org-ratex-available-p)
             (display-graphic-p)
             (derived-mode-p 'org-mode))
    (my/org-ratex-disable-other-preview-systems)
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
        ratex-edit-preview 'posframe
        ratex-inline-preview nil
        ratex-auto-download-backend t
        ratex-render-color "#d8dee9"
        ratex-posframe-background-color "#16181d"
        ratex-posframe-border-color "#3b4252")
  :hook (org-mode . my/org-ratex-enable))

(provide 'init-ratex)
;;; init-ratex.el ends here
