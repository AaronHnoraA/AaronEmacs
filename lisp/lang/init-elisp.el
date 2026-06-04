;;; init-elisp.el --- Emacs Lisp config -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp keeps the built-in editing experience by default, but can opt
;; into `lsp-mode' via Elsa in file buffers when `elsa-lsp' is installed.

;;; Code:

(require 'init-funcs)
(require 'init-package-utils)
(require 'subr-x)

(declare-function my/lsp-mode-ensure "init-lsp")
(declare-function my/language-server-stop-eglot "init-lsp")
(declare-function my/register-lsp-mode-preference "init-lsp" (mode &optional feature source note))
(declare-function flymake-start "flymake" (&optional report-fn))
(declare-function elisp-flymake-byte-compile "elisp-mode" (report-fn &rest args))
(declare-function elisp-flymake-checkdoc "elisp-mode" (report-fn &rest args))
(declare-function lsp-activate-on "lsp-mode" (&rest languages))
(declare-function lsp-deferred "lsp-mode")
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function lsp-stdio-connection "lsp-mode" (command &optional test-command))
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))
(declare-function make-lsp-client "lsp-mode" (&rest args))

(defvar flymake-diagnostic-functions)
(defvar lsp-language-id-configuration)
(defvar lsp-inlay-hint-enable)
(defvar lsp-auto-guess-root)
(defvar lsp-guess-root-without-session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/elisp--flymake-safe-buffer-p ()
  "Return non-nil when byte-compile Flymake is safe for this buffer."
  (or buffer-file-name
      (not (fboundp 'trusted-content-p))
      (funcall #'trusted-content-p)))

(defun my/elisp-flymake-backends ()
  "Return the Flymake backends suitable for the current Emacs Lisp buffer."
  (delq nil
        (list (when (my/elisp--flymake-safe-buffer-p)
                #'elisp-flymake-byte-compile)
              #'elisp-flymake-checkdoc)))

(defun my/elisp-mode-setup ()
  "Setup for Emacs Lisp buffers."
  ;; Multi-line eldoc in echo area
  (setq-local eldoc-echo-area-use-multiline-p t)

  ;; Better native completion experience
  (setq-local completion-cycle-threshold 3)

  ;; Elsa does not implement the inlay hint request.
  (setq-local lsp-inlay-hint-enable nil)
  (when (and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp-inlay-hints-mode))
    (ignore-errors (lsp-inlay-hints-mode -1)))

  ;; Keep startup buffers such as `*scratch*' out of Flymake when Emacs marks
  ;; them as untrusted.  Otherwise the built-in byte-compile backend emits a
  ;; warning before our local backend override can take effect.
  (if (my/elisp--flymake-safe-buffer-p)
      (progn
        (setq-local flymake-diagnostic-functions (my/elisp-flymake-backends))
        (flymake-mode 1)
        (flymake-start))
    (setq-local flymake-diagnostic-functions nil)
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook #'my/elisp-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional Elsa LSP support (same explicit lsp-mode route pattern as Lean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(my/package-install-archive 'elsa)

(when (fboundp 'my/register-lsp-mode-preference)
  (my/register-lsp-mode-preference
   'emacs-lisp-mode
   'elsa-lsp
   nil
   "Emacs Lisp buffers prefer Elsa through lsp-mode when Elsa is installed."))

(defun my/elisp-elsa-lsp-command ()
  "Return a direct Elsa LSP command using the current Emacs binary.

This avoids the upstream Eask/Cask wrapper requirement and starts Elsa from
the already-installed ELPA package."
  (let ((emacs-bin (expand-file-name invocation-name invocation-directory)))
    (list emacs-bin
          "--batch"
          "-Q"
          "--eval"
          (mapconcat
           #'identity
           '("(progn"
             "  (require 'package)"
             "  (package-initialize)"
             "  (require 'cl-lib)"
             "  (let ((warning-minimum-level :emergency)"
             "        (message-log-max nil)"
             "        (inhibit-message t))"
             "    (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil))"
             "              ((symbol-function 'display-warning) (lambda (&rest _args) nil)))"
             "      (require 'elsa-lsp)))"
             "  (elsa-lsp-stdin-loop))")
           "\n"))))

(defun my/elisp-register-elsa-lsp-client ()
  "Register the Elsa LSP client without loading Elsa into the main Emacs."
  (when (locate-library "elsa-lsp")
    (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "emacs-lisp"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection #'my/elisp-elsa-lsp-command)
      :activation-fn (lsp-activate-on "emacs-lisp")
      :major-modes '(emacs-lisp-mode)
      :priority 2
      :server-id 'elsa-lsp))))

(defun my/elisp-lsp-ensure ()
  "Start Elsa LSP for the current Emacs Lisp file buffer."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (not (member (file-name-nondirectory buffer-file-name)
                          '(".dir-locals.el" ".dir-locals-2.el")))
             (or (not (fboundp 'trusted-content-p))
                 (funcall #'trusted-content-p))
             (locate-library "elsa-lsp")
             (not (bound-and-true-p lsp-managed-mode)))
    (setq-local lsp-inlay-hint-enable nil)
    (setq-local lsp-auto-guess-root t)
    (setq-local lsp-guess-root-without-session t)
    (when (fboundp 'my/language-server-stop-eglot)
      (my/language-server-stop-eglot))
    (lsp-deferred)))

(defun my/elisp-lsp-ensure-deferred ()
  "Start Elsa LSP after the current Emacs Lisp file buffer settles."
  (when (and buffer-file-name
             (fboundp 'my/elisp-lsp-ensure)
             (locate-library "elsa-lsp"))
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (my/elisp-lsp-ensure))))
       buffer))))

(with-eval-after-load 'lsp-mode
  (my/elisp-register-elsa-lsp-client))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-lsp-ensure-deferred)



(provide 'init-elisp)

;;; init-elisp.el ends here
