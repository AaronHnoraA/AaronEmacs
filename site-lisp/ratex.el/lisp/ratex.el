;;; ratex.el --- Inline LaTeX previews via RaTeX -*- lexical-binding: t; -*-

;; Author: ratex.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, math, tools

;;; Commentary:

;; Minimal async inline math preview minor mode backed by RaTeX.

;;; Code:
(require 'ratex-core)
(require 'ratex-overlays)
(require 'ratex-render)

(declare-function evil-insert-state-p "evil")

(defvar my/org-latex--allow-native-preview)
(defvar my/org-latex--external-preview-active)
(defvar-local ratex--saved-org-native-preview nil)
(defvar-local ratex--saved-org-external-preview nil)
(defvar-local ratex--saved-org-scroll-preview nil)
(defvar-local ratex--saved-org-fragtog-mode nil)

(defun ratex--supported-buffer-p ()
  "Return non-nil when the current buffer should enable RaTeX."
  (derived-mode-p 'org-mode
                  'latex-mode
                  'LaTeX-mode
                  'tex-mode
                  'TeX-mode
                  'plain-tex-mode
                  'plain-TeX-mode
                  'docTeX-mode
                  'markdown-mode))

(defun ratex--prepare-org-preview-integration ()
  "Adjust Org preview integration for the current buffer."
  (when (derived-mode-p 'org-mode)
    (when (and ratex-org-disable-native-preview
               (boundp 'my/org-latex--allow-native-preview))
      (setq-local ratex--saved-org-native-preview my/org-latex--allow-native-preview)
      (setq-local my/org-latex--allow-native-preview nil))
    (when (boundp 'my/org-latex--external-preview-active)
      (setq-local ratex--saved-org-external-preview my/org-latex--external-preview-active)
      (setq-local my/org-latex--external-preview-active t))
    (when (boundp 'my/org-latex--scroll-preview-enabled)
      (setq-local ratex--saved-org-scroll-preview my/org-latex--scroll-preview-enabled))
    (when (fboundp 'my/org-latex-cleanup-scroll-preview)
      (my/org-latex-cleanup-scroll-preview))
    (when (bound-and-true-p org-fragtog-mode)
      (setq-local ratex--saved-org-fragtog-mode t)
      (org-fragtog-mode -1))))

(defun ratex--restore-org-preview-integration ()
  "Restore Org preview integration state for the current buffer."
  (when (derived-mode-p 'org-mode)
    (when (boundp 'my/org-latex--allow-native-preview)
      (setq-local my/org-latex--allow-native-preview ratex--saved-org-native-preview))
    (when (boundp 'my/org-latex--external-preview-active)
      (setq-local my/org-latex--external-preview-active
                  ratex--saved-org-external-preview))
    (when (and ratex--saved-org-scroll-preview
               (fboundp 'my/org-latex-enable-scroll-preview))
      (my/org-latex-enable-scroll-preview))
    (when (and ratex--saved-org-fragtog-mode
               (fboundp 'org-fragtog-mode))
      (org-fragtog-mode 1))
    (kill-local-variable 'ratex--saved-org-native-preview)
    (kill-local-variable 'ratex--saved-org-external-preview)
    (kill-local-variable 'ratex--saved-org-scroll-preview)
    (kill-local-variable 'ratex--saved-org-fragtog-mode)))

(defun ratex-sync-evil-state ()
  "Refresh edit preview visibility after an Evil state transition."
  (when ratex-mode
    (if (ratex--preview-active-p)
        (ratex-refresh-post-command-soon)
      (ratex--hide-edit-preview)
      (let ((fragment (ratex--active-fragment-at-point)))
        (when fragment
          (ratex--ensure-fragment-preview fragment))))))

;;;###autoload
(define-minor-mode ratex-mode
  "Minor mode for inline math previews powered by RaTeX."
  :lighter " RaTeX"
  (if ratex-mode
      (progn
        (ratex--prepare-org-preview-integration)
        (ratex-reset-buffer-state)
        (add-hook 'post-command-hook #'ratex-post-command nil t)
        (add-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch)
        (ratex-start-backend)
        (ratex-initialize-previews)
        (ratex-sync-evil-state))
    (remove-hook 'post-command-hook #'ratex-post-command t)
    (remove-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch)
    (ratex-handle-buffer-switch)
    (ratex-clear-overlays)
    (ratex-reset-buffer-state)
    (ratex--restore-org-preview-integration)))


;;;###autoload
;;;###autoload
(defun ratex-toggle-preview-command ()
  "Toggle RaTeX preview at point."
  (interactive)
  (ratex-toggle-preview-at-point))

;;;###autoload
(defun ratex-setup ()
  "Enable `ratex-mode' in common text/math buffers."
  (interactive)
  (dolist (hook '(org-mode-hook
                  latex-mode-hook
                  LaTeX-mode-hook
                  tex-mode-hook
                  TeX-mode-hook
                  plain-tex-mode-hook
                  plain-TeX-mode-hook
                  docTeX-mode-hook
                  markdown-mode-hook))
    (add-hook hook #'ratex-turn-on)))

;;;###autoload
(defun ratex-turn-on ()
  "Enable `ratex-mode' in buffers that RaTeX supports."
  (when (and (display-graphic-p)
             (ratex--supported-buffer-p))
    (ratex-mode 1)))

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'ratex-sync-evil-state)
  (add-hook 'evil-insert-state-exit-hook #'ratex-sync-evil-state))

(provide 'ratex)

;;; ratex.el ends here
