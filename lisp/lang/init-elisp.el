;;; init-elisp.el --- Emacs Lisp config -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp uses built-in tooling instead of lsp/eglot:
;; - eldoc
;; - completion-at-point
;; - flymake

;;; Code:

(require 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp does not use eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/elisp--flymake-safe-buffer-p ()
  "Return non-nil when byte-compile Flymake is safe for this buffer."
  (or buffer-file-name
      (not (fboundp 'trusted-content-p))
      (funcall #'trusted-content-p)))

(defun my/elisp-mode-setup ()
  "Setup for Emacs Lisp buffers."
  ;; Multi-line eldoc in echo area
  (setq-local eldoc-echo-area-use-multiline-p t)

  ;; Better native completion experience
  (setq-local completion-cycle-threshold 3)

  ;; `prog-mode' enables Flymake broadly; turn it back off for untrusted
  ;; scratch-like buffers so Emacs 31 does not emit the byte-compile warning.
  (if (my/elisp--flymake-safe-buffer-p)
      (flymake-mode 1)
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook #'my/elisp-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevent eglot from starting in Emacs Lisp buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'eglot
  (defun my/eglot--skip-in-elisp (orig-fn &rest args)
    "Do not run eglot in Emacs Lisp buffers."
    (if (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
        (progn
          ;; If eglot somehow already started, shut it down locally.
          (when (and (fboundp 'eglot-managed-p)
                     (eglot-managed-p))
            (ignore-errors (eglot-shutdown (eglot-current-server))))
          nil)
      (apply orig-fn args)))

  (advice-add 'eglot :around #'my/eglot--skip-in-elisp)
  (advice-add 'eglot-ensure :around #'my/eglot--skip-in-elisp))


(provide 'init-elisp)

;;; init-elisp.el ends here
