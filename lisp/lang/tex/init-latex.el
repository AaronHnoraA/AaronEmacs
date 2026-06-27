;;; init-latex.el --- LaTeX/BibTeX LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `texlab' when installed, otherwise fall back to `digestif'.
;; This keeps LaTeX/BibTeX buffers on the same Eglot-based workflow as the
;; rest of the configuration while preserving the existing latexmk/XeLaTeX
;; build setup from AUCTeX.

;;; Code:

(require 'aaron-ui nil t)

(declare-function aaron-ui-color "aaron-ui" (token &optional fallback variant))
(declare-function my/executable-or-name "init-utils")
(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(defun my/latex-ratex-color (token fallback)
  "Return Aaron UI color TOKEN, or FALLBACK when the theme helper is absent."
  (if (fboundp 'aaron-ui-color)
      (aaron-ui-color token fallback)
    fallback))

(add-to-list 'load-path
             (expand-file-name "site-lisp/ratex.el/lisp" user-emacs-directory))

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
         (bibtex-mode . my/latex-eglot-ensure)))

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
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
     :note "LaTeX and BibTeX buffers prefer texlab, then fall back to digestif.")))

(use-package ratex
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend
             ratex-toggle-preview-command)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.30
        ratex-edit-preview-max-staleness 1.0
        ratex-edit-preview-scan-lines 2
        ratex-font-size 32.0
        ratex-inline-preview nil
        ratex-initial-render-scope 'visible
        ratex-visible-region-margin 1
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-render-color (my/latex-ratex-color 'fg-soft "#D8DEE9")
        ratex-posframe-background-color (my/latex-ratex-color 'bg-ratex "#2B3140")
        ratex-posframe-border-color (my/latex-ratex-color 'border-ratex "#5F6F8F"))
  :hook ((latex-mode . ratex-turn-on)
         (LaTeX-mode . ratex-turn-on)
         (tex-mode . ratex-turn-on)
         (TeX-mode . ratex-turn-on)
         (plain-tex-mode . ratex-turn-on)
         (plain-TeX-mode . ratex-turn-on)
         (docTeX-mode . ratex-turn-on)))

(provide 'init-latex)
;;; init-latex.el ends here
