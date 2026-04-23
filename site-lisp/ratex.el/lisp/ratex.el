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

(defun ratex--any-active-buffer-p (&optional ignored-buffer)
  "Return non-nil when any live buffer except IGNORED-BUFFER has `ratex-mode'."
  (catch 'active
    (dolist (buffer (buffer-list))
      (unless (eq buffer ignored-buffer)
        (with-current-buffer buffer
          (when (bound-and-true-p ratex-mode)
            (throw 'active t)))))
    nil))

(defun ratex--ensure-global-hooks ()
  "Install global hooks needed while at least one RaTeX buffer is active."
  (add-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch))

(defun ratex--maybe-remove-global-hooks (&optional ignored-buffer)
  "Remove global RaTeX hooks when no active buffers remain.
IGNORED-BUFFER is treated as already inactive, which is useful while running
cleanup from `kill-buffer-hook' or `change-major-mode-hook'."
  (unless (ratex--any-active-buffer-p ignored-buffer)
    (remove-hook 'buffer-list-update-hook #'ratex-handle-buffer-switch)
    (ratex--cancel-cache-gc-timer)))

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
  (remove-hook 'kill-buffer-hook #'ratex--cleanup-current-buffer-h t)
  (remove-hook 'change-major-mode-hook #'ratex--cleanup-current-buffer-h t)
  (ratex-handle-buffer-switch)
  (ratex--preserving-window-start #'ratex-clear-overlays)
  (ratex-reset-buffer-state)
  (ratex--maybe-remove-global-hooks (current-buffer)))

(defun ratex--cleanup-current-buffer-h ()
  "Clean RaTeX state before the current buffer is killed or changes mode."
  (setq ratex-mode nil)
  (ratex--disable-current-buffer))

;;;###autoload
(define-minor-mode ratex-mode
  "Minor mode for inline math previews powered by RaTeX."
  :lighter " RaTeX"
  (if ratex-mode
      (condition-case err
          (progn
            (ratex-reset-buffer-state)
            (ratex--ensure-cache-gc-timer)
            (add-hook 'post-command-hook #'ratex-post-command nil t)
            (add-hook 'kill-buffer-hook #'ratex--cleanup-current-buffer-h nil t)
            (add-hook 'change-major-mode-hook #'ratex--cleanup-current-buffer-h nil t)
            (ratex--ensure-global-hooks)
            (ratex-start-backend)
            (ratex-initialize-previews)
            (when (ratex--preview-style)
              (ratex--refresh-preview-now)))
        (error
         (ratex--disable-current-buffer)
         (setq ratex-mode nil)
         (ratex--maybe-remove-global-hooks (current-buffer))
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

(provide 'ratex)

;;; ratex.el ends here
