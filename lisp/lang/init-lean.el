;;; init-lean.el --- Lean4 config with upstream lsp-mode integration -*- lexical-binding: t -*-

;;; Commentary:
;; Lean 4 works best with the upstream `lean4-mode' + `lsp-mode' integration.

;;; Code:

(my/package-ensure-installed-list '(dash magit-section lsp-mode))
(my/package-ensure-vc 'lean4-mode "https://github.com/leanprover-community/lean4-mode.git")

(with-eval-after-load 'magit-mode
  ;; `magit-region-highlight-hook' may reference
  ;; `magit-diff-update-hunk-region' before `magit-diff' is loaded.
  (require 'magit-diff nil t))

(defvar magit-region-highlight-hook)

(defun my/lean4-info-buffer-setup (&rest _)
  "Adjust the Lean goal buffer after it is created or shown."
  (let ((buffer (get-buffer "*Lean Goal*")))
    (when buffer
      (with-current-buffer buffer
        ;; Lean's info buffer uses `magit-section-mode' for rendering, but it
        ;; is not a diff buffer and does not need Magit's diff-specific region
        ;; highlighting hook.
        (setq-local magit-region-highlight-hook nil)))))

(with-eval-after-load 'lean4-info
  (advice-add 'lean4-ensure-info-buffer :after #'my/lean4-info-buffer-setup)
  (advice-add 'lean4-toggle-info-buffer :after #'my/lean4-info-buffer-setup))

(use-package lean4-mode
  :init
  (my/register-lsp-mode-preference 'lean4-mode)
  :mode ("\\.lean\\'" . lean4-mode))

(provide 'init-lean)

;;; init-lean.el ends here
