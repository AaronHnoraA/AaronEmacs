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

(defvar ratex-mode)
(declare-function evil-insert-state-p "evil")

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

(defun ratex--disable-current-buffer ()
  "Tear down RaTeX state in the current buffer without changing `ratex-mode'."
  (remove-hook 'post-command-hook #'ratex-post-command t)
  (remove-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch)
  (ratex-handle-buffer-switch)
  (ratex--preserving-window-start #'ratex-clear-overlays)
  (ratex-reset-buffer-state))

(defun ratex-sync-evil-state ()
  "Refresh edit preview visibility after an Evil state transition."
  (when ratex-mode
    (if (ratex--preview-active-p)
        (ratex-refresh-post-command-soon)
      (let ((fragment (or ratex--active-fragment
                          (ratex--active-fragment-at-point))))
        (ratex--hide-edit-preview)
        (when fragment
          (ratex--refresh-inline-fragment-now fragment))))))

;;;###autoload
(define-minor-mode ratex-mode
  "Minor mode for inline math previews powered by RaTeX."
  :lighter " RaTeX"
  (if ratex-mode
      (condition-case err
          (progn
            (ratex-reset-buffer-state)
            (add-hook 'post-command-hook #'ratex-post-command nil t)
            (add-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch)
            (ratex-start-backend)
            (ratex-initialize-previews)
            (ratex-sync-evil-state))
        (error
         (ratex--disable-current-buffer)
         (setq ratex-mode nil)
         (display-warning
          'ratex
          (format "RaTeX failed to enable in %s: %s"
                  (buffer-name)
                  (error-message-string err))
          :warning)))
    (ratex--disable-current-buffer)))


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
             (ratex--supported-buffer-p)
             (not (file-remote-p default-directory)))
    (ratex-mode 1)))

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'ratex-sync-evil-state)
  (add-hook 'evil-insert-state-exit-hook #'ratex-sync-evil-state))

(provide 'ratex)

;;; ratex.el ends here
