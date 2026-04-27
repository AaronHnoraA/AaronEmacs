;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot integration for Rust using rust-analyzer.

;;; Code:

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/eglot-set-workspace-configuration "init-lsp" (configuration))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . my/rust-eglot-setup)
         (rust-mode . my/eglot-ensure-unless-lsp-mode)
         (rust-ts-mode . my/rust-eglot-setup)
         (rust-ts-mode . my/eglot-ensure-unless-lsp-mode))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t)
  :config
  (defun my/rust-eglot-setup ()
    "Apply Rust-specific Eglot settings."
    (my/eglot-set-workspace-configuration
     '((:rust-analyzer
        :diagnostics (:disabled ["unresolved-extern-crate"])
        :cargo (:allFeatures t)
        :checkOnSave (:command "clippy")
        :lens (:references (:adt (:enable t)
                            :enumVariant (:enable t)
                            :method (:enable t)
                            :trait (:enable t))))))))

(use-package eglot
  :ensure nil
  :defer t)

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(rust-mode rust-ts-mode)
     '("rust-analyzer")
     :label "rust-analyzer"
     :executables '("rust-analyzer")
     :note "Rust buffers use rust-analyzer through Eglot.")))

(provide 'init-rust)
;;; init-rust.el ends here
