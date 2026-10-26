;;; lang/elisp/config.el --- Emacs Lisp config -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp keeps the built-in editing experience by default, but can opt
;; into `lsp-mode' via Elsa in file buffers when `elsa-lsp' is installed.

;;; Code:

(require 'config)
(require 'aaron-funcs)
(require 'aaron-packages)
(require 'subr-x)

(declare-function +lsp/mode-ensure "tools/lsp/config")
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

(defun +elisp--flymake-safe-buffer-p ()
  "Return non-nil when byte-compile Flymake is safe for this buffer."
  (or buffer-file-name
      (not (fboundp 'trusted-content-p))
      (funcall #'trusted-content-p)))

(defun +elisp-flymake-backends ()
  "Return the Flymake backends suitable for the current Emacs Lisp buffer."
  (delq nil
        (list (when (+elisp--flymake-safe-buffer-p)
                #'elisp-flymake-byte-compile)
              #'elisp-flymake-checkdoc)))

(defun +elisp-mode-setup ()
  "Setup for Emacs Lisp buffers."
  ;; Multi-line eldoc in echo area
  (setq-local eldoc-echo-area-use-multiline-p t)

  ;; From Doom: show a variable's current value beside its documentation.
  ;; Emacs ships this as `elisp-eldoc-var-docstring-with-value'.
  (when (memq #'elisp-eldoc-var-docstring eldoc-documentation-functions)
    (remove-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring t)
    (add-hook 'eldoc-documentation-functions
              #'elisp-eldoc-var-docstring-with-value nil t))

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
  (if (+elisp--flymake-safe-buffer-p)
      (progn
        (setq-local flymake-diagnostic-functions (+elisp-flymake-backends))
        (flymake-mode 1)
        (flymake-start))
    (setq-local flymake-diagnostic-functions nil)
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))))

(add-hook 'emacs-lisp-mode-hook #'+elisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook #'+elisp-mode-setup)

;; From Doom: the byte-compile check runs in a child Emacs that only sees
;; `elisp-flymake-byte-compile-load-path' ("./"), so in this configuration
;; every file stopped at its first `require' ("Cannot open load file") and
;; reported nothing else.  Give the child this session's `load-path', ahead of
;; "./": in a directory module "./config.el" is the module itself and would
;; shadow the core `config' feature.
(defvar elisp-flymake-byte-compile-load-path)

(defadvice! +elisp--flymake-session-load-path-a (fn &rest args)
  "Byte-compile for Flymake with the running session's `load-path'."
  :around #'elisp-flymake-byte-compile
  (let ((elisp-flymake-byte-compile-load-path
         (append load-path elisp-flymake-byte-compile-load-path)))
    (apply fn args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional Elsa LSP support (same explicit lsp-mode route pattern as Lean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(aaron-package-install-archive 'elsa)

(defgroup +elisp nil
  "Emacs Lisp editing and analysis."
  :group 'languages)

(config-defvar +elisp-elsa-worker-limit nil
  "Maximum number of parallel analysis workers the Elsa LSP server may start.
Elsa fans out over `num-processors' with no upper bound, and every worker is
a fresh Emacs that loads Elsa from scratch.  On a natively compiled build the
startup cost is small enough not to matter; on a build without native
compilation - Neomacs today - twelve interpreted workers saturate the machine
and stall the editor while a file is being analysed.  nil means automatic:
leave Elsa alone when native compilation is available, otherwise cap at
`+elisp-elsa-worker-fallback-limit'.  An integer forces that limit."
  :type '(choice (const :tag "Automatic" nil) integer)
  :group '+elisp)

(defconst +elisp-elsa-worker-fallback-limit 4
  "Worker cap applied by `+elisp-elsa-worker-limit' in automatic mode.")

(defun +elisp-elsa-effective-worker-limit ()
  "Return the worker cap for the Elsa LSP server, or nil for no cap."
  (cond
   ((integerp +elisp-elsa-worker-limit)
    (max 1 +elisp-elsa-worker-limit))
   (+elisp-elsa-worker-limit nil)
   ((native-comp-available-p) nil)
   (t (min +elisp-elsa-worker-fallback-limit (num-processors)))))

(defun +elisp-elsa-lsp-command ()
  "Return a direct Elsa LSP command using the current Emacs binary.

This avoids the upstream Eask/Cask wrapper requirement and starts Elsa from
the already-installed ELPA package."
  (let ((emacs-bin (expand-file-name invocation-name invocation-directory))
        (worker-limit (+elisp-elsa-effective-worker-limit)))
    (list emacs-bin
          "--batch"
          "-Q"
          "--eval"
          (mapconcat
           #'identity
           `("(progn"
             "  (require 'package)"
             "  (package-initialize)"
             "  (require 'cl-lib)"
             ,@(when worker-limit
                 (list (format "  (advice-add 'num-processors :override (lambda (&rest _) %d))"
                               worker-limit)))
             "  (let ((warning-minimum-level :emergency)"
             "        (message-log-max nil)"
             "        (inhibit-message t))"
             "    (cl-letf"
             "        (((symbol-function 'message) (lambda (&rest _args) nil))"
             "         ((symbol-function 'display-warning) (lambda (&rest _args) nil)))"
             "      (require 'elsa-lsp)"
             "      ;; Elsa 20250717 redundantly lists elsa-type together with"
             "      ;; its child elsa-type-symbol.  Its byte-compiled defclass"
             "      ;; bypasses function rebinding, so repair the class after"
             "      ;; loading and before the server starts handling requests."
             "      (when (memq 'elsa-type"
             "                  (mapcar #'eieio-class-name"
             "                          (eieio-class-parents"
             "                           'elsa-type-bool)))"
             "        (eval"
             "         '(defclass elsa-type-bool (elsa-type-symbol)"
             "            ((predicates"
             "              :initarg :predicates"
             "              :documentation \"Type narrowing predicates.\")))))))"
             "  (elsa-lsp-stdin-loop))")
           "\n"))))

(defun +elisp-register-elsa-lsp-client ()
  "Register the Elsa LSP client without loading Elsa into the main Emacs."
  (when (locate-library "elsa-lsp")
    (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "emacs-lisp"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection #'+elisp-elsa-lsp-command)
      :activation-fn (lsp-activate-on "emacs-lisp")
      :major-modes '(emacs-lisp-mode)
      :priority 2
      :server-id 'elsa-lsp))))

(defun +elisp-lsp-ensure ()
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
    (lsp-deferred)))

(defun +elisp-lsp-ensure-deferred ()
  "Start Elsa LSP after the current Emacs Lisp file buffer settles."
  (when (and buffer-file-name
             (fboundp '+elisp-lsp-ensure)
             (locate-library "elsa-lsp"))
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (+elisp-lsp-ensure))))
       buffer))))

(with-eval-after-load 'lsp-mode
  (+elisp-register-elsa-lsp-client))

(add-hook 'emacs-lisp-mode-hook #'+elisp-lsp-ensure-deferred)



;; From Doom's `doom/help-search-load-path': search the source of everything
;; Emacs can load, the config's own modules and the installed packages alike.
(declare-function consult-ripgrep "consult" (&optional dir initial))

(defun +elisp/search-load-path (&optional initial)
  "Search every directory on `load-path' with ripgrep.
INITIAL, the symbol at point when called interactively, seeds the input."
  (interactive (list (thing-at-point 'symbol t)))
  (require 'consult)
  (consult-ripgrep (seq-filter #'file-directory-p
                               (delete-dups (copy-sequence load-path)))
                   initial))

(defalias 'elisp-search-load-path #'+elisp/search-load-path
  "Search the source of every directory Emacs loads from.")

;; From Doom's `doom/sandbox': run a snippet in a fresh `emacs -Q' that still
;; sees every installed package, to tell a package's behaviour from this
;; configuration's.  Only the vanilla run is offered: a second instance loading
;; the full configuration would compete with this one for its server and
;; background state.
(defvar +elisp-sandbox-buffer-name "*elisp-sandbox*"
  "Name of the Emacs Lisp sandbox buffer.")

(defun +elisp-sandbox--command (file)
  "Return the command that runs FILE in a vanilla Emacs with this `load-path'."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q"
        "--eval" (prin1-to-string `(setq load-path ',load-path))
        "-l" file))

(defun +elisp-sandbox/run ()
  "Run the sandbox buffer's code in a new vanilla Emacs."
  (interactive)
  (let* ((dir (make-temp-file "elisp-sandbox-" t))
         (file (expand-file-name "sandbox.el" dir))
         (code (buffer-substring-no-properties (point-min) (point-max)))
         ;; A client-side helper: never route it through a remote buffer.
         (default-directory temporary-file-directory))
    (with-temp-file file
      (insert ";; -*- lexical-binding: t; -*-\n" code))
    (make-process :name "elisp-sandbox"
                  :command (+elisp-sandbox--command file)
                  :noquery t
                  :sentinel (lambda (process _event)
                              (unless (process-live-p process)
                                (delete-directory dir t))))
    (message "Sandbox started in a vanilla Emacs")))

(defvar-keymap +elisp-sandbox-mode-map
  :parent emacs-lisp-mode-map
  "C-c C-c" #'+elisp-sandbox/run)

(define-derived-mode +elisp-sandbox-mode emacs-lisp-mode "Sandbox"
  "Emacs Lisp to run in a vanilla Emacs with \\[+elisp-sandbox/run].")

(defun +elisp/sandbox ()
  "Open the Emacs Lisp sandbox."
  (interactive)
  (pop-to-buffer
   (with-current-buffer (get-buffer-create +elisp-sandbox-buffer-name)
     (unless (derived-mode-p '+elisp-sandbox-mode)
       (+elisp-sandbox-mode)
       (insert (substitute-command-keys
                ";; Code here runs in `emacs -Q' with every installed package on
;; `load-path' but none of this configuration.  \\[+elisp-sandbox/run] runs it.\n\n")))
     (current-buffer))))

(defalias 'elisp-sandbox #'+elisp/sandbox
  "Try Emacs Lisp in a vanilla Emacs that sees the installed packages.")

(provide 'lang/elisp/config)

;;; lang/elisp/config.el ends here
