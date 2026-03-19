;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot integration for Rust using rust-analyzer.

;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . my/rust-eglot-setup)
         (rust-ts-mode . my/rust-eglot-setup))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t)
  :config
  (defun my/rust-eglot-setup ()
    "Apply Rust-specific Eglot settings."
    ;; 通过 eglot-workspace-configuration 向 rust-analyzer 传递专属参数
    ;; 这等效于 lsp-mode 中的 lsp-rust-analyzer-diagnostics-disabled
    (setq-local eglot-workspace-configuration
                '((:rust-analyzer .
                   (:diagnostics (:disabled ["unresolved-extern-crate"])))))))

(provide 'init-rust)
;;; init-rust.el ends here



;;; init-rust.el ends here
