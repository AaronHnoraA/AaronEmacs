;;; init-lean.el --- Lean4 config with eglot -*- lexical-binding: t -*-

;;; Commentary:
;; Lean 4 support using eglot instead of lsp-mode.

;;; Code:

(my/package-ensure-installed-list '(dash magit-section eglot))
(my/package-ensure-vc 'lean4-mode "https://github.com/leanprover-community/lean4-mode.git")

(use-package lean4-mode
  :mode ("\\.lean\\'" . lean4-mode)
  :hook (lean4-mode . eglot-ensure)
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
