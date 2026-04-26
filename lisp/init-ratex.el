;;; init-ratex.el --- Local RaTeX package wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'aaron-ui)

(defconst my/org-ratex--fragment-start-regexp
  "\\\\(\\|\\\\\\[\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}\\|\\$"
  "Regexp matching likely LaTeX fragment starters in Org buffers.")

(defvar-local my/org-ratex--syntax-watch-installed nil
  "Whether this Org buffer is waiting to enable `ratex-mode' on demand.")

(defun my/org-ratex-buffer-has-fragment-syntax-p ()
  "Return non-nil when the current Org buffer likely contains LaTeX fragments."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward my/org-ratex--fragment-start-regexp nil t))))

(defun my/org-ratex-cleanup-syntax-watch ()
  "Remove the Org-local watcher that enables `ratex-mode' on demand."
  (remove-hook 'after-change-functions #'my/org-ratex-enable-on-syntax-insert t)
  (remove-hook 'change-major-mode-hook #'my/org-ratex-cleanup-syntax-watch t)
  (remove-hook 'kill-buffer-hook #'my/org-ratex-cleanup-syntax-watch t)
  (setq-local my/org-ratex--syntax-watch-installed nil))

(defun my/org-ratex-enable-on-syntax-insert (beg end _len)
  "Enable `ratex-mode' when an edit between BEG and END inserts LaTeX syntax."
  (when (and (derived-mode-p 'org-mode)
             (not (bound-and-true-p ratex-mode))
             (save-excursion
               (goto-char beg)
               (re-search-forward my/org-ratex--fragment-start-regexp end t)))
    (my/org-ratex-cleanup-syntax-watch)
    (ratex-turn-on)))

(defun my/org-ratex-install-syntax-watch ()
  "Enable `ratex-mode' lazily for Org buffers without LaTeX syntax yet."
  (unless my/org-ratex--syntax-watch-installed
    (setq-local my/org-ratex--syntax-watch-installed t)
    (add-hook 'after-change-functions #'my/org-ratex-enable-on-syntax-insert nil t)
    (add-hook 'change-major-mode-hook #'my/org-ratex-cleanup-syntax-watch nil t)
    (add-hook 'kill-buffer-hook #'my/org-ratex-cleanup-syntax-watch nil t)))

(defun my/org-ratex-turn-on-maybe ()
  "Enable `ratex-mode' immediately or lazily in Org buffers."
  (if (not (derived-mode-p 'org-mode))
      (ratex-turn-on)
    (if (my/org-ratex-buffer-has-fragment-syntax-p)
        (ratex-turn-on)
      (my/org-ratex-install-syntax-watch))))

(use-package ratex
  :load-path "~/.emacs.d/site-lisp/ratex.el/lisp"
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.30
        ratex-edit-preview-scan-lines 1
        ratex-font-size 32.0
        ratex-inline-preview nil
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-render-color (aaron-ui-color 'fg-soft)
        ratex-posframe-background-color (aaron-ui-color 'bg-ratex)
        ratex-posframe-border-color (aaron-ui-color 'border-ratex))
  :hook ((org-mode . my/org-ratex-turn-on-maybe)
         (latex-mode . ratex-turn-on)
         (LaTeX-mode . ratex-turn-on)
         (tex-mode . ratex-turn-on)
         (TeX-mode . ratex-turn-on)
         (markdown-mode . ratex-turn-on)
         (plain-tex-mode . ratex-turn-on)
         (plain-TeX-mode . ratex-turn-on)
         (docTeX-mode . ratex-turn-on)))

(provide 'init-ratex)
;;; init-ratex.el ends here
