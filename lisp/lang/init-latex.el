;;; init-latex.el --- LaTeX/BibTeX LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `texlab' when installed, otherwise fall back to `digestif'.
;; This keeps LaTeX/BibTeX buffers on the same Eglot-based workflow as the
;; rest of the configuration while preserving the existing latexmk/XeLaTeX
;; build setup from AUCTeX.

;;; Code:

(declare-function my/executable-or-name "init-utils")
(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(defun my/latex-eglot-available-p ()
  "Return non-nil when a LaTeX language server is available."
  (or (executable-find "texlab")
      (executable-find "digestif")))

(defun my/latex-eglot-workspace-configuration ()
  "Return workspace settings for LaTeX language servers."
  `(:texlab
    (:build (:executable ,(my/executable-or-name "latexmk")
             :args ["-xelatex"
                    "-interaction=nonstopmode"
                    "-synctex=1"
                    "-file-line-error"
                    "-outdir=%OUTDIR%"
                    "%f"]
             :onSave nil
             :forwardSearchAfter nil)
     :chktex (:onOpenAndSave t
              :onEdit nil)
     :diagnosticsDelay 300)))

(defun my/latex-eglot-ensure ()
  "Start Eglot for LaTeX-related buffers when a server is available."
  (when (my/latex-eglot-available-p)
    (setq-local eglot-workspace-configuration
                (my/latex-eglot-workspace-configuration))
    (my/eglot-ensure-unless-lsp-mode)))

(use-package eglot
  :ensure nil
  :defer t
  :hook ((latex-mode . my/latex-eglot-ensure)
         (LaTeX-mode . my/latex-eglot-ensure)
         (tex-mode . my/latex-eglot-ensure)
         (TeX-mode . my/latex-eglot-ensure)
         (plain-tex-mode . my/latex-eglot-ensure)
         (plain-TeX-mode . my/latex-eglot-ensure)
         (docTeX-mode . my/latex-eglot-ensure)
         (bibtex-mode . my/latex-eglot-ensure))
  :config
  (my/register-eglot-server-program
   '(latex-mode LaTeX-mode
     tex-mode TeX-mode
     plain-tex-mode plain-TeX-mode
     docTeX-mode
     bibtex-mode)
   (eglot-alternatives
    '(("texlab")
      ("digestif")))
   :label "texlab/digestif"
   :executables '("texlab" "digestif")
   :note "LaTeX and BibTeX buffers prefer texlab, then fall back to digestif."))

(provide 'init-latex)
;;; init-latex.el ends here
