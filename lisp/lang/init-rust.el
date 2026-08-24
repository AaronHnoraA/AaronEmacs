;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:
;; lsp-mode integration for Rust using rust-analyzer.

;;; Code:

(declare-function my/language-server-set-workspace-configuration
                  "init-lsp" (configuration))
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/register-language-server "init-lsp")

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . my/rust-language-server-setup-h)
         (rust-ts-mode . my/rust-language-server-setup-h))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t)
  :config
  (defun my/rust-language-server-setup-h ()
    "Apply Rust-specific language-server settings."
    (my/language-server-set-workspace-configuration
     '((:rust-analyzer
        :diagnostics (:disabled ["unresolved-extern-crate"])
        :cargo (:allFeatures t)
        :checkOnSave (:command "clippy")
        :lens (:references (:adt (:enable t)
                            :enumVariant (:enable t)
                            :method (:enable t)
                            :trait (:enable t))))))))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'my/register-language-server)
    (my/register-language-server
     '(rust-mode rust-ts-mode)
     (lambda ()
       (list (or (my/language-server-executable-find "rust-analyzer")
                 "rust-analyzer")))
     :server-id 'my-rust-analyzer
     :priority 1
     :label "rust-analyzer"
     :executables '("rust-analyzer")
     :note "Rust buffers use rust-analyzer through lsp-mode.")))

(provide 'init-rust)
;;; init-rust.el ends here
