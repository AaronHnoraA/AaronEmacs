;;; init-lean.el --- Lean4 config with eglot -*- lexical-binding: t -*-

;;; Commentary:
;; Lean 4 support using eglot instead of lsp-mode.

;;; Code:

(require 'package)
(require 'use-package)

;; Dependencies sometimes needed by lean4-mode
(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'magit-section)
(add-to-list 'package-selected-packages 'eglot)

;; Install lean4-mode from source repository if missing
(unless (package-installed-p 'lean4-mode)
  (package-vc-install
   '(lean4-mode
     :url "https://github.com/leanprover-community/lean4-mode.git"
     :rev :last-release)))

(use-package lean4-mode
  :mode ("\\.lean\\'" . lean4-mode)
  :hook
  (lean4-mode . eglot-ensure)
  :init
  ;; Optional: if your Emacs sometimes maps .lean strangely, this keeps it explicit.
  (add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))
  :config
  ;; Tell eglot how to start Lean's language server.
  ;;
  ;; Usually `lean` is provided by elan:
  ;;   curl https://elan.lean-lang.org -sSf | sh
  ;;
  ;; Check with:
  ;;   lean --version
  ;;
  ;; If `lean` is on PATH, this is enough.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(lean4-mode . ("lean" "--server")))))

(provide 'init-lean)

;;; init-lean.el ends here
