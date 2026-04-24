;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; Restore the original Eglot + Flymake workflow, while keeping a small
;; compatibility layer so explicitly registered modes can still opt into
;; `lsp-mode' when needed.  The maintenance/dashboard layer lives in
;; `init-lsp-tools.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(eval-when-compile
  (ignore-errors
    (require 'hydra)))

(defgroup my/language-server nil
  "Routing and observability helpers for the language server stack."
  :group 'tools
  :prefix "my/language-server-")

(defvar my/lsp-mode-preferred-modes nil
  "Major modes that should use `lsp-mode' instead of `eglot'.")

(defvar my/lsp-mode-required-features nil
  "Alist mapping major modes to extra `lsp-mode' support features.")

(defvar my/lsp-mode-preference-metadata nil
  "Metadata for explicit `lsp-mode' routing entries.

Each entry is a plist with at least `:mode', `:feature', `:source', and
optional `:note' keys.")

(defvar my/eglot-custom-server-program-metadata nil
  "Metadata for locally registered `eglot-server-programs' entries.

Each entry is a plist with keys such as `:modes', `:program',
`:executables', `:label', `:source', and `:note'.")

(defcustom my/language-server-performance-read-process-output-max (* 1024 1024)
  "Minimum `read-process-output-max' while any language server is active."
  :type 'integer
  :group 'my/language-server)

(defcustom my/language-server-performance-gcmh-factor 2
  "Multiplier applied to `gcmh-high-cons-threshold' while LSP is active."
  :type 'integer
  :group 'my/language-server)

(defcustom my/language-server-defer-shutdown 3
  "Seconds to defer Eglot shutdown after the last managed buffer closes."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'my/language-server)

(defcustom my/language-server-disable-file-watchers-on-remote t
  "Disable `lsp-mode' file watchers in remote buffers."
  :type 'boolean
  :group 'my/language-server)

(defvar tramp-login-shell)
(defvar eglot-events-buffer-config)
(defvar eglot-stay-out-of)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(defvar gcmh-high-cons-threshold)
(defvar my/dape-state-dir)
(defvar company-dabbrev-ignore-case)
(defvar company-dabbrev-downcase)
(defvar company-dabbrev-code-ignore-case)
(defvar company-dabbrev-code-everywhere)
(defvar company-files-exclusions)
(defvar lsp-managed-mode)
(defvar lsp-completion-provider)
(defvar lsp-diagnostics-provider)
(defvar lsp-enable-file-watchers)
(defvar lsp-file-watch-threshold)
(defvar read-process-output-max)

(defvar my/language-server--managed-buffer-count 0
  "Number of buffers currently counted for LSP performance tuning.")

(defvar my/language-server--default-read-process-output-max nil
  "Original `read-process-output-max' before LSP performance tuning.")

(defvar my/language-server--default-gcmh-high-cons-threshold nil
  "Original `gcmh-high-cons-threshold' before LSP performance tuning.")

(defvar-local my/language-server--performance-buffer-p nil
  "Whether the current buffer is counted for LSP performance tuning.")

(defvar-local my/flymake-diagnostic-at-point-timer nil
  "Idle timer used by `my/flymake-diagnostic-at-point-mode'.")

(defvar-local my/flymake-diagnostic-at-point-last-point nil
  "Last point position shown by `my/flymake-diagnostic-at-point-mode'.")

(defvar-local my/flymake-diagnostic-at-point-last-text nil
  "Last diagnostic text shown by `my/flymake-diagnostic-at-point-mode'.")

(defcustom my/flymake-diagnostic-at-point-delay 0.2
  "Seconds to wait before echoing the Flymake diagnostic at point."
  :type 'number
  :group 'my/language-server)

(declare-function lsp-feature? "lsp-mode" (method))
(declare-function lsp--update-inlay-hints "lsp-mode" ())
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function eglot-current-server "eglot")
(declare-function eglot-code-actions "eglot" ())
(declare-function eglot-find-implementation "eglot" ())
(declare-function eglot-find-typeDefinition "eglot" ())
(declare-function eglot-format-buffer "eglot" ())
(declare-function eglot-rename "eglot" ())
(declare-function eglot--lookup-mode "eglot" (mode))
(declare-function eglot--managed-buffers "eglot" (server))
(declare-function eglot--managed-mode@my/defer-eglot-shutdown nil (&optional server))
(declare-function eglot-shutdown "eglot" (server))
(declare-function eldoc-box-quit-frame "eldoc-box" ())
(declare-function gcmh-set-high-threshold "gcmh" ())
(declare-function hydra--call-interactively-remap-maybe "hydra" (cmd &optional keys))
(declare-function hydra-default-pre "hydra" ())
(declare-function hydra-keyboard-quit "hydra" ())
(declare-function hydra-set-transient-map "hydra" (keymap &optional keep-pred on-exit message timeout))
(declare-function hydra-show-hint "hydra" (&rest args))
(declare-function lsp--on-request@my/handle-inlay-hint-refresh nil (workspace request))
(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))
(declare-function my/project-local-apply-env "init-project-local" (env &optional base))
(declare-function my/project-local-env "init-project-local" (kind &optional root))
(declare-function my/project-local-value "init-project-local" (key &optional root))
(declare-function my/problems-buffer "init-problems" ())
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui")
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/diagnostics-project-ui "init-diagnostics-ui")
(declare-function prescient-persist-mode "prescient" (&optional arg))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-start "flymake" (&optional report-fn))
(declare-function dape--live-connection "dape" (&optional kind noerror))

(defun my/language-server--resolve-source (source)
  "Return SOURCE as an absolute file name when available."
  (when-let* ((path (or source load-file-name buffer-file-name)))
    (expand-file-name path)))

(defun my/language-server-executable-find (program)
  "Return PROGRAM path for the current local or remote buffer."
  (executable-find program t))

(defun my/language-server-executable-available-p (program)
  "Return non-nil when PROGRAM is available locally or on the remote host."
  (and (my/language-server-executable-find program) t))

(defun my/prog-flymake-setup ()
  "Enable Flymake for programming buffers with mode-specific exceptions.

Untrusted Emacs Lisp buffers manage Flymake locally so the built-in
byte-compile backend does not emit noisy warnings on startup."
  (unless (and (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
               (fboundp 'trusted-content-p)
               (not (trusted-content-p)))
    (flymake-mode 1)))

(defun my/lsp-managed-mode-setup ()
  "Apply per-buffer `lsp-mode' UI tweaks after a client attaches."
  (when (and (bound-and-true-p lsp-inlay-hint-enable)
             (fboundp 'lsp-feature?)
             (fboundp 'lsp-inlay-hints-mode)
             (ignore-errors (lsp-feature? "textDocument/inlayHint")))
    (lsp-inlay-hints-mode 1)))

(defun my/flymake-diagnostic-at-point-text ()
  "Return the first Flymake diagnostic text covering point."
  (when (bound-and-true-p flymake-mode)
    (when-let* ((diag (seq-find
                       (lambda (it)
                         (let ((beg (flymake-diagnostic-beg it))
                               (end (flymake-diagnostic-end it)))
                           (<= beg (point) (max beg end))))
                       (flymake-diagnostics))))
      (flymake-diagnostic-text diag))))

(defun my/flymake-diagnostic-at-point-cancel ()
  "Cancel the current buffer's Flymake point-diagnostic timer."
  (when (timerp my/flymake-diagnostic-at-point-timer)
    (cancel-timer my/flymake-diagnostic-at-point-timer)
    (setq my/flymake-diagnostic-at-point-timer nil)))

(defun my/flymake-diagnostic-at-point-display (buffer)
  "Echo the Flymake diagnostic for BUFFER when point stays idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/flymake-diagnostic-at-point-timer nil)
      (when (and (bound-and-true-p my/flymake-diagnostic-at-point-mode)
                 (bound-and-true-p flymake-mode)
                 (frame-focus-state)
                 (eq (current-buffer) (window-buffer (selected-window))))
        (let ((point-now (point))
              (text (my/flymake-diagnostic-at-point-text)))
          (unless (and (equal point-now my/flymake-diagnostic-at-point-last-point)
                       (equal text my/flymake-diagnostic-at-point-last-text))
            (setq my/flymake-diagnostic-at-point-last-point point-now
                  my/flymake-diagnostic-at-point-last-text text)
            (when text
              (message "➤ %s" text))))))))

(defun my/flymake-diagnostic-at-point-schedule (&rest _)
  "Refresh the idle timer for `my/flymake-diagnostic-at-point-mode'."
  (my/flymake-diagnostic-at-point-cancel)
  (when (and (bound-and-true-p my/flymake-diagnostic-at-point-mode)
             (not (minibufferp))
             (get-buffer-window (current-buffer) t))
    (setq my/flymake-diagnostic-at-point-timer
          (run-with-idle-timer
           my/flymake-diagnostic-at-point-delay
           nil
           #'my/flymake-diagnostic-at-point-display
           (current-buffer)))))

(define-minor-mode my/flymake-diagnostic-at-point-mode
  "Display Flymake diagnostics for point in the echo area."
  :lighter nil
  (if my/flymake-diagnostic-at-point-mode
      (progn
        (add-hook 'post-command-hook #'my/flymake-diagnostic-at-point-schedule nil t)
        (add-hook 'pre-command-hook #'my/flymake-diagnostic-at-point-cancel nil t)
        (add-hook 'change-major-mode-hook #'my/flymake-diagnostic-at-point-cleanup nil t)
        (add-hook 'kill-buffer-hook #'my/flymake-diagnostic-at-point-cleanup nil t))
    (my/flymake-diagnostic-at-point-cleanup)))

(defun my/flymake-diagnostic-at-point-cleanup ()
  "Release buffer-local point-diagnostic hooks and timer."
  (remove-hook 'post-command-hook #'my/flymake-diagnostic-at-point-schedule t)
  (remove-hook 'pre-command-hook #'my/flymake-diagnostic-at-point-cancel t)
  (remove-hook 'change-major-mode-hook #'my/flymake-diagnostic-at-point-cleanup t)
  (remove-hook 'kill-buffer-hook #'my/flymake-diagnostic-at-point-cleanup t)
  (my/flymake-diagnostic-at-point-cancel)
  (setq my/flymake-diagnostic-at-point-last-point nil
        my/flymake-diagnostic-at-point-last-text nil))

(defun my/flymake-diagnostic-at-point-mode-sync ()
  "Keep `my/flymake-diagnostic-at-point-mode' aligned with `flymake-mode'."
  (unless (bound-and-true-p flymake-mode)
    (setq my/flymake-diagnostic-at-point-last-point nil
          my/flymake-diagnostic-at-point-last-text nil))
  (my/flymake-diagnostic-at-point-mode
   (if (bound-and-true-p flymake-mode) 1 -1)))

(defun my/lsp-handle-inlay-hint-refresh (workspace)
  "Handle an unsupported inlay-hint refresh request for WORKSPACE."
  (dolist (buffer (ignore-errors (lsp--workspace-buffers workspace)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p lsp-managed-mode)
                   (bound-and-true-p lsp-inlay-hints-mode)
                   (fboundp 'lsp--update-inlay-hints)
                   (get-buffer-window buffer t))
          (ignore-errors
            (lsp--update-inlay-hints))))))
  nil)

(defun my/language-server--plist-like-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (listp value)
       (or (null value)
           (let ((rest value)
                 (ok t))
             (while (and rest ok)
               (setq ok (and (keywordp (car rest))
                             (consp (cdr rest))))
               (setq rest (cddr rest)))
             ok))))

(defun my/language-server--alist-like-p (value)
  "Return non-nil when VALUE looks like an alist."
  (and (listp value)
       (or (null value)
           (consp (car value)))))

(defun my/language-server--merge-values (base override)
  "Deep-merge OVERRIDE into BASE for keyed plist/alist structures."
  (cond
   ((null override) (copy-tree base))
   ((null base) (copy-tree override))
   ((and (my/language-server--plist-like-p base)
         (my/language-server--plist-like-p override))
    (let ((result (copy-tree base))
          (plist (copy-tree override)))
      (while plist
        (let* ((key (pop plist))
               (value (pop plist))
               (current (plist-get result key)))
          (setq result
                (plist-put result key
                           (my/language-server--merge-values current value)))))
      result))
   ((and (my/language-server--alist-like-p base)
         (my/language-server--alist-like-p override))
   (let ((result (copy-tree base)))
      (dolist (entry (copy-tree override) result)
        (when (consp entry)
          (let* ((key (car entry))
                 (current (assoc key result))
                 (value (if current
                            (my/language-server--merge-values (cdr current) (cdr entry))
                          (copy-tree (cdr entry)))))
            (setq result (assq-delete-all key result))
            (setq result (append result (list (cons key value)))))))))
   (t (copy-tree override))))

(defun my/language-server-project-backend-override ()
  "Return the project-local backend override for the current buffer."
  (when (fboundp 'my/project-local-value)
    (pcase (my/project-local-value :language-server)
      ((or 'lsp 'lsp-mode) 'lsp-mode)
      ('eglot 'eglot)
      ('disabled 'disabled)
      (_ nil))))

(defun my/language-server-preferred-backend ()
  "Return the preferred backend for the current buffer."
  (or (my/language-server-project-backend-override)
      (if (and my/lsp-mode-preferred-modes
               (apply #'derived-mode-p my/lsp-mode-preferred-modes))
          'lsp-mode
        'eglot)))

(defun my/language-server-project-environment ()
  "Return the merged project-local environment for language servers."
  (when (fboundp 'my/project-local-env)
    (my/project-local-env 'lsp)))

(defun my/language-server-process-environment ()
  "Return the process environment for launching language servers."
  (let ((env (my/language-server-project-environment)))
    (if (and env (fboundp 'my/project-local-apply-env))
        (my/project-local-apply-env env process-environment)
      process-environment)))

(defun my/language-server-apply-process-environment ()
  "Install the language-server process environment in the current buffer."
  (setq-local process-environment
              (my/language-server-process-environment)))

(defun my/language-server-project-workspace-configuration ()
  "Return project-local Eglot workspace configuration overrides."
  (when (fboundp 'my/project-local-value)
    (my/project-local-value :eglot-workspace)))

(defun my/eglot-set-workspace-configuration (configuration)
  "Merge CONFIGURATION into the current buffer's Eglot workspace settings."
  (setq-local eglot-workspace-configuration
              (my/language-server--merge-values
               (and (boundp 'eglot-workspace-configuration)
                    eglot-workspace-configuration)
               configuration)))

(defun my/language-server-apply-eglot-local-settings ()
  "Apply project-local Eglot settings before startup."
  (when-let* ((configuration (my/language-server-project-workspace-configuration)))
    (my/eglot-set-workspace-configuration configuration)))

(defun my/language-server-apply-lsp-local-settings ()
  "Apply local `lsp-mode' settings before startup."
  (when (and my/language-server-disable-file-watchers-on-remote
             (file-remote-p default-directory))
    (setq-local lsp-enable-file-watchers nil
                lsp-file-watch-threshold 0)))

(defun my/eglot-contact-available-p ()
  "Return non-nil when Eglot has a server mapping for this buffer."
  (and (require 'eglot nil t)
       (fboundp 'eglot--lookup-mode)
       (ignore-errors (eglot--lookup-mode major-mode))))

(defun my/language-server-lsp-mode-preference-entries ()
  "Return explicit `lsp-mode' routing entries in registration order."
  (nreverse (copy-sequence my/lsp-mode-preference-metadata)))

(defun my/language-server-eglot-program-entries ()
  "Return locally registered Eglot server-program entries."
  (nreverse (copy-sequence my/eglot-custom-server-program-metadata)))

(defun my/language-server-prepare-remote-eglot-environment ()
  "Prepare shell settings for remote `eglot' buffers."
  (when (file-remote-p default-directory)
    (let ((remote-shell (or (and (boundp 'tramp-login-shell)
                                 tramp-login-shell)
                            "sh")))
      (setq-local shell-file-name remote-shell)
      (setq-local explicit-shell-file-name remote-shell)
      (setq-local shell-command-switch "-c"))))

(defun my/language-server-managed-p ()
  "Return non-nil when the current buffer is managed by Eglot or lsp-mode."
  (or (bound-and-true-p lsp-managed-mode)
      (bound-and-true-p eglot-managed-mode)
      (bound-and-true-p eglot--managed-mode)))

(defun my/language-server-performance--enable ()
  "Apply Doom-style IPC and GC tuning while language servers are active."
  (when (= my/language-server--managed-buffer-count 1)
    (setq my/language-server--default-read-process-output-max
          (default-value 'read-process-output-max))
    (setq-default read-process-output-max
                  (max (default-value 'read-process-output-max)
                       my/language-server-performance-read-process-output-max))
    (when (boundp 'gcmh-high-cons-threshold)
      (setq my/language-server--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      (setq-default gcmh-high-cons-threshold
                    (* my/language-server-performance-gcmh-factor
                       my/language-server--default-gcmh-high-cons-threshold))
      (when (fboundp 'gcmh-set-high-threshold)
        (gcmh-set-high-threshold)))))

(defun my/language-server-performance--disable ()
  "Restore pre-LSP IPC and GC settings when no language servers remain."
  (when (= my/language-server--managed-buffer-count 0)
    (when my/language-server--default-read-process-output-max
      (setq-default read-process-output-max
                    my/language-server--default-read-process-output-max))
    (when (and (boundp 'gcmh-high-cons-threshold)
               my/language-server--default-gcmh-high-cons-threshold)
      (setq-default gcmh-high-cons-threshold
                    my/language-server--default-gcmh-high-cons-threshold)
      (when (fboundp 'gcmh-set-high-threshold)
        (gcmh-set-high-threshold)))))

(defun my/language-server-performance--leave-buffer ()
  "Remove the current buffer from LSP performance accounting."
  (when my/language-server--performance-buffer-p
    (setq my/language-server--performance-buffer-p nil
          my/language-server--managed-buffer-count
          (max 0 (1- my/language-server--managed-buffer-count)))
    (my/language-server-performance--disable)))

(defun my/language-server-performance-sync-h ()
  "Synchronize LSP performance tuning with the current buffer state."
  (if (my/language-server-managed-p)
      (unless my/language-server--performance-buffer-p
        (setq my/language-server--performance-buffer-p t)
        (setq my/language-server--managed-buffer-count
              (1+ my/language-server--managed-buffer-count))
        (my/language-server-performance--enable))
    (my/language-server-performance--leave-buffer)))

(add-hook 'eglot-managed-mode-hook #'my/language-server-performance-sync-h)
(add-hook 'lsp-managed-mode-hook #'my/language-server-performance-sync-h)
(add-hook 'kill-buffer-hook #'my/language-server-performance--leave-buffer)
(add-hook 'change-major-mode-hook #'my/language-server-performance--leave-buffer)

(defun my/register-lsp-mode-preference (mode &optional feature source note)
  "Prefer `lsp-mode' over `eglot' for MODE.
When FEATURE is non-nil, require it before starting `lsp-mode'.
SOURCE and NOTE are recorded for maintenance tooling."
  (add-to-list 'my/lsp-mode-preferred-modes mode)
  (when feature
    (setf (alist-get mode my/lsp-mode-required-features nil nil #'eq)
          feature))
  (setq my/lsp-mode-preference-metadata
        (cons (list :mode mode
                    :feature feature
                    :source (my/language-server--resolve-source source)
                    :note note)
              (cl-remove-if
               (lambda (entry)
                 (eq (plist-get entry :mode) mode))
               my/lsp-mode-preference-metadata))))

(defun my/register-eglot-server-program (modes program &rest props)
  "Register PROGRAM for MODES and record metadata for maintenance tools.

PROPS accepts `:executables', `:label', `:source', and `:note'."
  (add-to-list 'eglot-server-programs (cons modes program))
  (setq my/eglot-custom-server-program-metadata
        (cons (list :modes modes
                    :program program
                    :executables (plist-get props :executables)
                    :label (plist-get props :label)
                    :source (my/language-server--resolve-source
                             (plist-get props :source))
                    :note (plist-get props :note))
              (cl-remove-if
               (lambda (entry)
                 (equal (plist-get entry :modes) modes))
               my/eglot-custom-server-program-metadata))))

(defun my/lsp-mode-preferred-p ()
  "Return non-nil when current buffer should use `lsp-mode'."
  (eq (my/language-server-preferred-backend) 'lsp-mode))

(defun my/lsp-mode-required-feature ()
  "Return the extra `lsp-mode' feature required for the current buffer."
  (catch 'feature
    (dolist (entry my/lsp-mode-required-features)
      (when (derived-mode-p (car entry))
        (throw 'feature (cdr entry))))
    nil))

(defun my/lsp-mode-supported-p ()
  "Return non-nil when `lsp-mode' can start for the current buffer."
  (let ((feature (my/lsp-mode-required-feature)))
    (if feature
        (or (featurep feature)
            (require feature nil t))
      t)))

(defun my/current-language-server-backend ()
  "Return the active language server backend for the current buffer."
  (cond
   ((and (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    'eglot)
   ((bound-and-true-p lsp-managed-mode)
    'lsp-mode)
   (t nil)))

(defun my/language-server-stop-eglot ()
  "Shut down the current `eglot' session in this buffer, if any."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (ignore-errors
      (eglot-shutdown (eglot-current-server)))))

(defun my/lsp-mode-ensure ()
  "Start `lsp-mode' for explicitly registered major modes."
  (interactive)
  (when (eq (my/language-server-preferred-backend) 'lsp-mode)
    (unless (bound-and-true-p lsp-managed-mode)
      (my/language-server-stop-eglot)
      (if (my/lsp-mode-supported-p)
          (progn
            (my/language-server-apply-process-environment)
            (my/language-server-apply-lsp-local-settings)
            (lsp-deferred))
        (let ((feature (my/lsp-mode-required-feature)))
          (message "Skip lsp-mode in %s: missing `%s'" major-mode feature))))))

(defun my/eglot-ensure-unless-lsp-mode ()
  "Start `eglot' in the current buffer unless `lsp-mode' is preferred."
  (interactive)
  (when (eq (my/language-server-preferred-backend) 'eglot)
    (unless (or (bound-and-true-p lsp-managed-mode)
                (and (fboundp 'eglot-managed-p)
                     (eglot-managed-p)))
      (when (my/eglot-contact-available-p)
        (when (fboundp 'my/direnv-update-environment-maybe)
          (my/direnv-update-environment-maybe))
        (my/language-server-prepare-remote-eglot-environment)
        (my/language-server-apply-process-environment)
        (my/language-server-apply-eglot-local-settings)
        (eglot-ensure)))))

(defun my/eglot-ensure ()
  "Start `eglot' in programming buffers that do not opt into `lsp-mode'."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (my/eglot-ensure-unless-lsp-mode)))

(defun my/eglot-ensure-deferred ()
  "Start `eglot' after the current buffer has finished opening."
  (let ((buffer (current-buffer)))
    (run-at-time
     0 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (my/eglot-ensure))))
     buffer)))

(defun my/language-server-ensure ()
  "Start the preferred language server backend for the current buffer."
  (interactive)
  (pcase (my/language-server-preferred-backend)
    ('lsp-mode (my/lsp-mode-ensure))
    ('eglot (my/eglot-ensure))
    ('disabled (message "Language server disabled for this project"))))

(defun my/language-server-call (eglot-fn lsp-fn)
  "Call EGLOT-FN or LSP-FN for the active language server backend."
  (pcase (my/current-language-server-backend)
    ('eglot
     (call-interactively eglot-fn))
    ('lsp-mode
     (call-interactively lsp-fn))
    (_
     (user-error "No active language server in current buffer"))))

(defun my/language-server-code-actions ()
  "Run a code action using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-code-actions #'lsp-execute-code-action))

(defun my/language-server-format-buffer ()
  "Format the current buffer using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-format-buffer #'lsp-format-buffer))

(defun my/language-server-rename ()
  "Rename the symbol at point using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-rename #'lsp-rename))

(defun my/language-server-find-implementation ()
  "Find implementation using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-find-implementation #'lsp-find-implementation))

(defun my/language-server-find-type-definition ()
  "Find type definition using the active language server backend."
  (interactive)
  (my/language-server-call #'eglot-find-typeDefinition #'lsp-find-type-definition))

;; -------------------------
;; 1. Company Mode (Completion)
;; -------------------------
;; [https://company-mode.github.io/manual/](https://company-mode.github.io/manual/)

(defconst my/company-lsp-backends
  '((company-capf
     company-files
     :with company-tempo
     company-yasnippet))
  "LSP-first company backends for code buffers.")

(defconst my/company-text-backends
  '((company-capf
     company-files
     :with company-yasnippet
     company-dabbrev))
  "Company backends for prose and document buffers.")

(defconst my/company-shell-backends
  '((company-capf
     company-files
     :with company-dabbrev-code
     company-dabbrev))
  "Company backends for interactive shell buffers.")

(defun my/company-setup-text-backends ()
  "Use company popup completion in document buffers instead of `*Completions*'."
  (setq-local company-backends my/company-text-backends))

(defun my/company-setup-org-backends ()
  "Use Org-specific company backends while keeping CAPF/LSP completions."
  (setq-local company-backends
              '((company-capf
                 company-files
                 company-yasnippet)
                company-dabbrev)))

(defun my/company-setup-shell-backends ()
  "Enable popup completion for Eshell with CAPF/pcomplete."
  (company-mode 1)
  (setq-local company-backends my/company-shell-backends)
  (setq-local company-idle-delay 0.08)
  (setq-local company-minimum-prefix-length 1))

(use-package company
  :ensure t
  :demand t
  :hook ((eglot-managed-mode . company-mode)
         (lsp-managed-mode . company-mode)
         ;; org-mode derives from text-mode, so text-mode-hook already covers it.
         (text-mode . company-mode)
         (text-mode . my/company-setup-text-backends))
  :init
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-selection))
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  (setq company-idle-delay 0.12
        company-minimum-prefix-length 1
        company-show-quick-access t
        company-require-match nil
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t
        company-format-margin-function nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-code-everywhere t
        company-files-exclusions '(".git/" ".DS_Store")
        company-backends my/company-lsp-backends)
  (when (boundp 'company-show-numbers)
    (setq company-show-numbers t))
  (setq-default company-backends my/company-lsp-backends))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'my/company-setup-shell-backends))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company-backends))

(with-eval-after-load 'company-yasnippet
  (define-advice company-yasnippet (:around (fn command &optional arg &rest args)
                                            my/guard-doc-buffer)
    "Ignore snippet preview errors from asynchronous company doc timers."
    (if (eq command 'doc-buffer)
        (condition-case-unless-debug nil
            (apply fn command arg args)
          (error nil))
      (apply fn command arg args))))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-delay 0.2)
  (company-box-scrollbar nil)
  :config
  (define-advice company-box--handle-scroll-parent
      (:around (fn win new-start) my/company-box-guard-scroll-parent)
    "Ignore transient scroll events after the popup/window state is stale.
Guards both the nil new-start case and a potentially-throwing company-box--get-frame."
    (condition-case nil
        (when (and (window-live-p win)
                   (number-or-marker-p new-start)
                   (ignore-errors (frame-live-p (company-box--get-frame))))
          (funcall fn win new-start))
      (error nil))))

(use-package company-prescient
  :ensure t
  :after company
  :defer 2
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))


;; -------------------------
;; 2. Aggressive Indent
;; -------------------------
(use-package aggressive-indent
  :ensure t
  :hook ((elisp-mode . aggressive-indent-mode)
         (python-mode  . aggressive-indent-mode)
         (c++-mode     . aggressive-indent-mode)
         (c-mode       . aggressive-indent-mode)))


;; -------------------------
;; 3. Flymake (Diagnostics)
;; -------------------------
;; Eglot / lsp-mode 均统一走 Flymake 诊断
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . my/prog-flymake-setup)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c !" . my/problems-buffer)
         ("C-c ?" . my/diagnostics-dispatch))
  :custom
  (flymake-no-changes-timeout 0.2) ; 输入停顿 0.2s 后自动检查
  (flymake-indicator-type 'fringes))

;; 光标停在报错位置时，在 minibuffer 显示诊断
(add-hook 'flymake-mode-hook #'my/flymake-diagnostic-at-point-mode-sync)


;; -------------------------
;; 4. lsp-mode (for explicit opt-in languages)
;; -------------------------
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp
             lsp-deferred
             lsp-execute-code-action
             lsp-find-implementation
             lsp-find-type-definition
             lsp-format-buffer
             lsp-inlay-hints-mode
             lsp-rename)
  :hook (lsp-managed-mode . my/lsp-managed-mode-setup)
  :init
  (setq lsp-completion-provider :capf
        lsp-diagnostics-provider :flymake
        lsp-headerline-breadcrumb-enable nil
        lsp-inlay-hint-enable t
        lsp-log-io nil)
  :config
  (define-advice lsp--on-request (:around (fn workspace request) my/handle-inlay-hint-refresh)
    "Handle standard refresh requests that this `lsp-mode' release lacks."
    (let ((method (plist-get request :method)))
      (if (equal method "workspace/inlayHint/refresh")
          (my/lsp-handle-inlay-hint-refresh workspace)
        (funcall fn workspace request))))
  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c d") #'eldoc-doc-buffer)
  (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
  (define-key lsp-mode-map (kbd "C-h e") #'xref-find-definitions)
  (define-key lsp-mode-map (kbd "C-h r") #'xref-find-references)
  (define-key lsp-mode-map (kbd "C-h i") #'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-h t") #'lsp-find-type-definition))


;; -------------------------
;; 5. Eglot (LSP Client)
;; -------------------------
(use-package eglot
  :ensure nil ; Built-in since Emacs 29
  :hook ((prog-mode . my/eglot-ensure-deferred)
         (eglot-managed-mode . (lambda ()
                                 (when (fboundp 'eglot-inlay-hints-mode)
                                   (eglot-inlay-hints-mode 1)))))
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil)
  (eglot-code-action-indications nil)
  (eglot-send-changes-idle-time 0.2)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (read-process-output-max (* 1024 1024)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c d") #'eldoc-doc-buffer)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-h e") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-h r") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-h i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-h t") #'eglot-find-typeDefinition)
  (when (boundp 'eglot-events-buffer-config)
    (cl-callf plist-put eglot-events-buffer-config :size 0))
  (define-advice eglot--managed-mode (:around (fn &optional server) my/defer-eglot-shutdown)
    "Defer Eglot shutdown briefly to avoid restart churn while switching files."
    (let ((orig-shutdown (symbol-function 'eglot-shutdown)))
      (cl-letf (((symbol-function 'eglot-shutdown)
                 (lambda (srv)
                   (if (or (null my/language-server-defer-shutdown)
                           (eq my/language-server-defer-shutdown 0))
                       (funcall orig-shutdown srv)
                     (run-at-time
                      (if (numberp my/language-server-defer-shutdown)
                          my/language-server-defer-shutdown
                        3)
                      nil
                      (lambda (deferred-server)
                        (unless (eglot--managed-buffers deferred-server)
                          (funcall orig-shutdown deferred-server)))
                      srv)))))
        (funcall fn server)))))


;; -------------------------
;; 6. UI Emulation (Doc Box & Breadcrumb)
;; -------------------------

;; 替代 lsp-ui-doc：提供光标处悬浮文档框
(use-package eldoc-box
  :ensure t
  :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode)
         (lsp-managed-mode . eldoc-box-hover-at-point-mode)
         (emacs-lisp-mode . eldoc-box-hover-at-point-mode)
         (lisp-interaction-mode . eldoc-box-hover-at-point-mode))
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-clear-with-C-g t)
  :config
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-h d") #'eldoc-box-help-at-point)
    (define-key eglot-mode-map (kbd "C-h c") #'eldoc-box-quit-frame))
  (with-eval-after-load 'elisp-mode
    (define-key emacs-lisp-mode-map (kbd "C-h d") #'eldoc-box-help-at-point)
    (define-key emacs-lisp-mode-map (kbd "C-h c") #'eldoc-box-quit-frame)
    (define-key lisp-interaction-mode-map (kbd "C-h d") #'eldoc-box-help-at-point)
    (define-key lisp-interaction-mode-map (kbd "C-h c") #'eldoc-box-quit-frame))
  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map (kbd "C-h d") #'eldoc-box-help-at-point)
    (define-key lsp-mode-map (kbd "C-h c") #'eldoc-box-quit-frame)))

;; 替代 lsp-headerline-breadcrumb：Eglot 作者出品的面包屑
(use-package breadcrumb
  :ensure t
  :hook ((prog-mode . breadcrumb-local-mode)
         (org-src-mode . breadcrumb-local-mode)))

;; -------------------------
;; 7. Dape (Debugging)
;; -------------------------
;;
;; dape 是更适合 Eglot / 原生 Emacs 工作流的 DAP 客户端。
;; 入口命令是 `M-x dape`。
;; 推荐打开 `repeat-mode`，这样单步调试体验会更顺。
;;
;; 说明：
;; - 不再使用 dap-ui-mode / dap-auto-configure-mode
;; - 不再依赖 lsp-mode
;; - 部分 dap-mode 命令在 dape 中没有 1:1 同名接口，
;;   这里改成 dape 当前公开可用的命令体系
;;
(defvar dape-default-breakpoints-file)

(use-package dape
  :ensure t
  :after hydra
  :commands (dape
             dape-next
             dape-step-in
             dape-step-out
             dape-continue
             dape-pause
             dape-restart
             dape-quit
             dape-breakpoint-toggle
             dape-breakpoint-log
             dape-breakpoint-expression
             dape-breakpoint-hits
             dape-breakpoint-remove-at-point
             dape-evaluate-expression
             dape-watch-dwim
             dape-repl
             dape-repl-threads
             dape-repl-stack
             dape-repl-breakpoints
             dape-repl-scope
             dape-repl-watch)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  (dape-default-breakpoints-file
   (expand-file-name "breakpoints.eld" my/dape-state-dir))
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode 1)

  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t)))

  (with-suppressed-warnings ((docstrings) (callargs))
    (defhydra hydra-dape-mode
      (:color pink :hint nil :foreign-keys run)
      "
^Stepping^          ^Switch/View^             ^Breakpoints^         ^Debug^                     ^Eval / Watch^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_n_: Next           _ss_: Sessions(REPL)      _bb_: Toggle          _dd_: Debug (dape)          _ee_: Eval
_i_: Step in        _st_: Threads             _bd_: Delete here     _dr_: Restart               _er_: Eval region
_o_: Step out       _sf_: Stack               _ba_: Log message     _dq_: Quit                  _es_: Eval thing
_c_: Continue       _sl_: Locals(scope)       _bc_: Condition       _dR_: REPL                  _ea_: Add watch
_p_: Pause          _sb_: Breakpoints         _bh_: Hit count
"
      ("n" dape-next)
      ("i" dape-step-in)
      ("o" dape-step-out)
      ("c" dape-continue)
      ("p" dape-pause)

      ("ss" dape-repl)
      ("st" dape-repl-threads)
      ("sf" dape-repl-stack)
      ("sl" dape-repl-scope)
      ("sb" dape-repl-breakpoints)

      ("bb" dape-breakpoint-toggle)
      ("ba" dape-breakpoint-log)
      ("bd" dape-breakpoint-remove-at-point)
      ("bc" dape-breakpoint-expression)
      ("bh" dape-breakpoint-hits)

      ("dd" dape)
      ("dr" dape-restart)
      ("dR" dape-repl)
      ("dq" dape-quit :color blue)

      ("ee" dape-evaluate-expression)
      ("ea" dape-watch-dwim)
      ("er" (if (use-region-p)
                (dape-evaluate-expression
                 (or (ignore-errors (dape--live-connection 'stopped t))
                     (ignore-errors (dape--live-connection 'last)))
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))
              (user-error "No active region")))
      ("es" (let ((sym (thing-at-point 'symbol t)))
              (if sym
                  (dape-evaluate-expression
                   (or (ignore-errors (dape--live-connection 'stopped t))
                       (ignore-errors (dape--live-connection 'last)))
                   sym)
                (user-error "No symbol at point"))))

      ("q" nil "quit" :color blue))))


;; -------------------------
;; 8. Misc & Language Init
;; -------------------------

(setq tab-always-indent t)

;; Org-mode specific company setup
(add-hook 'org-mode-hook #'my/company-setup-org-backends)

;; Load other language specific configurations
(require 'init-cpp)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-bazel)
(require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-vale)
(require 'init-sh)
(require 'init-java)
(require 'init-lean)
(require 'init-md)
(require 'init-nix)
(require 'init-html)
(require 'init-js2)
(require 'init-latex)

;; eglot：永不自动重连（需要你手动 M-x eglot 重新连）
(setq-default eglot-autoreconnect nil)

(provide 'init-lsp)
;;; init-lsp.el ends here
