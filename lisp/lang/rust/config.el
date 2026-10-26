;;; lang/rust/config.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:
;; lsp-mode integration for Rust using rust-analyzer.

;;; Code:

(declare-function +language-server-set-workspace-configuration
                  "tools/lsp/config" (configuration))
(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function set-language-server! "tools/lsp/config")

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . +rust/rust-language-server-setup-h)
         (rust-ts-mode . +rust/rust-language-server-setup-h))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t)
  :config
  (defun +rust/rust-language-server-setup-h ()
    "Apply Rust-specific language-server settings."
    (+language-server-set-workspace-configuration
     '((:rust-analyzer
        :diagnostics (:disabled ["unresolved-extern-crate"])
        :cargo (:allFeatures t)
        :checkOnSave (:command "clippy")
        :lens (:references (:adt (:enable t)
                            :enumVariant (:enable t)
                            :method (:enable t)
                            :trait (:enable t))))))))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    (set-language-server!
     '(rust-mode rust-ts-mode)
     (lambda ()
       (list (or (+language-server-executable-find "rust-analyzer")
                 "rust-analyzer")))
     :server-id 'my-rust-analyzer
     :priority 1
     :label "rust-analyzer"
     :executables '("rust-analyzer")
     :note "Rust buffers use rust-analyzer through lsp-mode.")))

(provide 'lang/rust/config)
;;; lang/rust/config.el ends here
