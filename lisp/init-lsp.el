;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; Restore the original Eglot + Flymake workflow, while keeping a small
;; compatibility layer so explicitly registered modes can still opt into
;; `lsp-mode' when needed.  The maintenance/dashboard layer lives in
;; `init-lsp-tools.el'.

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'init-lsp-toolchain)
(require 'init-lsp-runtime)
(require 'project)
(require 'remote-channel)
(require 'remote-process)
(require 'remote-environment)
(require 'remote-workspace)
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

(defvar my/language-server-lsp-local-settings-hook nil
  "Hook run after environment resolution and before lsp-mode starts.")

(defvar my/language-server-disabled-modes nil
  "Major modes that should never auto-start a language server.")

(defvar my/eglot-custom-server-program-metadata nil
  "Metadata for locally registered `eglot-server-programs' entries.

Each entry is a plist with keys such as `:modes', `:program',
`:executables', `:placement', `:label', `:source', and `:note'.")

(config-defvar my/language-server-performance-read-process-output-max nil
  "Minimum `read-process-output-max' while any language server is active."
  :type 'integer
  :group 'my/language-server)

(config-defvar my/language-server-performance-gcmh-factor nil
  "Multiplier applied to `gcmh-high-cons-threshold' while LSP is active."
  :type 'integer
  :group 'my/language-server)

(config-defvar my/language-server-defer-shutdown nil
  "Seconds to defer Eglot shutdown after the last managed buffer closes."
  :type '(choice (const :tag "Disabled" nil) (integer :tag "Seconds"))
  :group 'my/language-server)

(config-defvar my/language-server-file-watch-policy 'auto
  "Policy for language-server dynamic file-watch registrations.

`auto' preserves Eglot and lsp-mode's native watcher behavior when the project
has a path directly accessible to the Emacs client.  For a target-only
filesystem it declines dynamic `workspace/didChangeWatchedFiles'
registration.  Accepting it would otherwise create one SSH-backed watcher per
project directory and can starve or kill the language-server transport.

`native' always accepts the registration, and is intended only for backends
whose watcher implementation is known to scale.  `disabled' declines all
dynamic file-watch registrations."
  :type '(choice
          (const :tag "Automatic placement-aware policy" auto)
          (const :tag "Always use Eglot watchers" native)
          (const :tag "Disable dynamic watchers" disabled))
  :group 'my/language-server)

(config-register
 'eglot-autoreconnect
 :type '(choice (const :tag "Disabled" nil)
                (const :tag "Immediate" t)
                integer)
 :group 'my/language-server
 :doc "Eglot crash-loop guard before automatic reconnect is allowed.")

(config-register
 'lsp-restart
 :type '(choice (const interactive)
                (const auto-restart)
                (const ignore))
 :group 'my/language-server
 :doc "lsp-mode policy after a language-server process exits.")

(defvar eglot--cached-server)
(defvar eglot-events-buffer-config)
(defvar eglot-stay-out-of)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(defvar gcmh-high-cons-threshold)
(defvar company-dabbrev-ignore-case)
(defvar company-dabbrev-downcase)
(defvar company-dabbrev-code-ignore-case)
(defvar company-dabbrev-code-everywhere)
(defvar company-files-exclusions)
(defvar lsp-managed-mode)
(defvar lsp--cur-workspace)
(defvar lsp-enable-file-watchers)
(defvar lsp-completion-provider)
(defvar lsp-diagnostics-provider)
(defvar lsp-restart)
(defvar read-process-output-max)

(dolist (adapter '("language-server" "eglot" "lsp-mode"))
  (remote-register-adapter
   adapter
   :capabilities '(process-sync process-async watch lsp environment
                   network-client)
   :preferences '((default . ("tramp-rpc" "tramp" "native")))
   :placement 'target
   :process-class 'interactive))

(defvar my/language-server--managed-buffer-count 0
  "Number of buffers currently counted for LSP performance tuning.")

(defvar my/language-server--default-read-process-output-max nil
  "Original `read-process-output-max' before LSP performance tuning.")

(defvar my/language-server--default-gcmh-high-cons-threshold nil
  "Original `gcmh-high-cons-threshold' before LSP performance tuning.")

(defvar-local my/language-server--performance-buffer-p nil
  "Whether the current buffer is counted for LSP performance tuning.")

(defvar-local my/eglot--waiting-for-direnv nil
  "Non-nil while Eglot startup waits for an asynchronous direnv export.")

(defvar-local my/lsp-mode--waiting-for-direnv nil
  "Non-nil while lsp-mode startup waits for an asynchronous direnv export.")

(defvar-local my/language-server--waiting-for-runtime nil
  "Non-nil while language-server startup waits for a runtime provider.")

(defvar-local my/flymake-diagnostic-at-point-timer nil
  "Idle timer used by `my/flymake-diagnostic-at-point-mode'.")

(defvar-local my/flymake-diagnostic-at-point-last-point nil
  "Last point position shown by `my/flymake-diagnostic-at-point-mode'.")

(defvar-local my/flymake-diagnostic-at-point-last-text nil
  "Last diagnostic text shown by `my/flymake-diagnostic-at-point-mode'.")

(config-defvar my/flymake-diagnostic-at-point-delay nil
  "Seconds to wait before echoing the Flymake diagnostic at point."
  :type 'number
  :group 'my/language-server)

(declare-function lsp-feature? "lsp-mode" (method))
(declare-function lsp--update-inlay-hints "lsp-mode" ())
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp--workspace-client "lsp-mode" (workspace))
(declare-function lsp--workspace-cmd-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-root "lsp-mode" (workspace))
(declare-function lsp--workspace-status "lsp-mode" (workspace))
(declare-function lsp--workspace-shutdown-action "lsp-mode" (workspace))
(declare-function lsp--client-server-id "lsp-mode" (client))
(declare-function lsp--session-workspaces "lsp-mode" (session))
(declare-function lsp-get "lsp-protocol" (hash-table key))
(declare-function lsp-process-kill "lsp-mode" (process))
(declare-function lsp-session "lsp-mode" ())
(declare-function lsp-workspace-shutdown "lsp-mode" (workspace))
(declare-function lsp-workspace-restart "lsp-mode" (workspace))
(declare-function eglot-current-server "eglot")
(declare-function eglot--project "eglot" (server))
(declare-function eglot-reconnect "eglot" (server &optional interactive))
(declare-function eglot-code-actions "eglot" ())
(declare-function eglot-find-implementation "eglot" ())
(declare-function eglot-find-typeDefinition "eglot" ())
(declare-function eglot-format-buffer "eglot" ())
(declare-function eglot-rename "eglot" ())
(declare-function eglot--lookup-mode "eglot" (mode))
(declare-function eglot--managed-buffers "eglot" (server))
(declare-function eglot--managed-mode@my/defer-eglot-shutdown nil (&optional server))
(declare-function eglot-shutdown
                  "eglot" (server &optional interactive timeout preserve-buffers))
(declare-function jsonrpc-running-p "jsonrpc" (connection))

(defcustom my/lsp-mode-startup-timeout 60
  "Seconds an lsp-mode workspace may remain in `starting' state.
After the deadline the workspace is stopped through the bounded shutdown
path.  Nil or a non-positive value disables the watchdog."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'my/language-server)

(defcustom my/lsp-mode-shutdown-timeout 1
  "Maximum seconds allowed for one graceful lsp-mode workspace shutdown.
The process is force-stopped after this deadline so quitting Emacs never
depends on a responsive language server."
  :type 'number
  :group 'my/language-server)

(defcustom my/lsp-mode-restart-limit 1
  "Maximum automatic lsp-mode restarts within the restart window."
  :type 'integer
  :group 'my/language-server)

(defcustom my/lsp-mode-restart-window 60
  "Seconds used by the lsp-mode crash-loop circuit breaker."
  :type 'number
  :group 'my/language-server)

(defvar my/lsp-mode--startup-timers (make-hash-table :test #'eq)
  "Startup watchdog timers keyed by lsp-mode workspace.")

(defvar my/lsp-mode--restart-history (make-hash-table :test #'equal)
  "Recent automatic restart timestamps keyed by server and logical root.")

(defun my/language-server--set-struct-slot (object type slot value)
  "Set OBJECT's cl-struct TYPE SLOT to VALUE.
Keeping TYPE and SLOT as runtime arguments avoids requiring private lsp-mode
struct definitions while this configuration file is compiled."
  (aset object (cl-struct-slot-offset type slot) value))
(declare-function eldoc-box-quit-frame "eldoc-box" ())
(declare-function gcmh-set-high-threshold "gcmh" ())
(declare-function hydra--call-interactively-remap-maybe "hydra" (cmd &optional keys))
(declare-function hydra-default-pre "hydra" ())
(declare-function hydra-keyboard-quit "hydra" ())
(declare-function hydra-set-transient-map "hydra" (keymap &optional keep-pred on-exit message timeout))
(declare-function hydra-show-hint "hydra" (&rest args))
(declare-function lsp--on-request@my/handle-inlay-hint-refresh nil (workspace request))
(declare-function my/direnv-update-environment-maybe
                  "init-direnv" (&optional path callback))
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

(defun my/language-server--resolve-source (source)
  "Return SOURCE as an absolute file name when available."
  (when-let* ((path (or source load-file-name buffer-file-name)))
    (expand-file-name path)))

(defun my/language-server-executable-find (program)
  "Return PROGRAM path in the current language-server tooling environment.
Runtime contexts intentionally do not make an analyzer installed inside a
kernel environment replace the stable target/workspace analyzer."
  (let* ((runtime (and (boundp 'my/language-server-runtime-current)
                       my/language-server-runtime-current))
         (tool-environment
          (and (my/language-server-runtime-p runtime)
               (my/language-server-runtime-tool-environment runtime)))
         (remote-buffer-environment
          (or tool-environment
              (and (boundp 'remote-buffer-environment)
                   remote-buffer-environment)))
         (remote-current-adapter-id "language-server"))
    (remote-executable-find program)))

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
  (if (and my/language-server-disabled-modes
           (apply #'derived-mode-p my/language-server-disabled-modes))
      'disabled
    (or (my/language-server-project-backend-override)
        (if (and my/lsp-mode-preferred-modes
                 (apply #'derived-mode-p my/lsp-mode-preferred-modes))
            'lsp-mode
          'eglot))))

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
  (remote-environment-ensure)
  (let ((environment (my/language-server-project-environment)))
    (if (and environment remote-buffer-environment)
        (remote-environment-apply
         (remote-environment-derive
          remote-buffer-environment "project-lsp"
          :scope 'workspace
          :vars environment
          :source 'project-local))
      (setq-local process-environment
                  (my/language-server-process-environment))))
  (when (fboundp 'my/language-server-toolchain-apply-environment)
    (my/language-server-toolchain-apply-environment)))

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
    (my/eglot-set-workspace-configuration configuration))
  (when (fboundp 'my/language-server-toolchain-apply-eglot-settings)
    (my/language-server-toolchain-apply-eglot-settings)))

(defun my/language-server-apply-lsp-local-settings ()
  "Apply project-local `lsp-mode' settings before startup."
  ;; Advertise watcher support only when the selected placement can implement
  ;; it without turning each directory into a remote process/connection.
  (setq-local
   lsp-enable-file-watchers
   (not
    (my/language-server--skip-file-watch-p
     (my/language-server--project-root-for-buffer))))
  (run-hooks 'my/language-server-lsp-local-settings-hook))

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

(defun my/language-server-prepare-eglot-execution-environment ()
  "Prepare the shared Eglot target execution boundary.
Shell selection is backend-owned; stdio language servers receive their
original argv without a client shell wrapper."
  nil)

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
  ;; REMOVE non-nil also clears a feature left by an earlier registration
  ;; when a reloaded configuration now passes nil.
  (setf (alist-get mode my/lsp-mode-required-features nil t #'eq)
        feature)
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

PROPS accepts `:executables', `:placement', `:label', `:source', and `:note'.
PLACEMENT defaults to `target': the command is resolved from the active
target/workspace environment and launched through the official process API.
Use `client' only for an explicitly client-side UI helper."
  (add-to-list 'eglot-server-programs (cons modes program))
  (setq my/eglot-custom-server-program-metadata
        (cons (list :modes modes
                    :program program
                    :executables (plist-get props :executables)
                    :placement (or (plist-get props :placement) 'target)
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
    (when-let* ((server (eglot-current-server)))
      (ignore-errors
        (eglot-shutdown server)))))

(defun my/lsp-mode-start-now ()
  "Start lsp-mode after the target environment is ready."
  (if (my/lsp-mode-supported-p)
      (progn
        (my/language-server-apply-process-environment)
        (my/language-server-apply-lsp-local-settings)
        (lsp-deferred))
    (let ((feature (my/lsp-mode-required-feature)))
      (message "Skip lsp-mode in %s: missing `%s'" major-mode feature))))

(defun my/lsp-mode--direnv-ready (_environment error)
  "Resume deferred lsp-mode startup, falling back after direnv ERROR."
  (setq my/lsp-mode--waiting-for-direnv nil)
  (when error
    (message
     "lsp-mode: direnv failed (%s); continuing with the target base environment"
     (error-message-string error)))
  (when (eq (my/language-server-preferred-backend) 'lsp-mode)
    (my/lsp-mode-start-now)))

(defun my/lsp-mode-ensure ()
  "Start `lsp-mode' for explicitly registered major modes."
  (interactive)
  (when (eq (my/language-server-preferred-backend) 'lsp-mode)
    (unless (or my/lsp-mode--waiting-for-direnv
                (bound-and-true-p lsp-managed-mode))
      (my/language-server-stop-eglot)
      (let ((state
             (and
              (fboundp 'my/direnv-update-environment-maybe)
              (my/direnv-update-environment-maybe
               nil #'my/lsp-mode--direnv-ready))))
        (if (eq state 'pending)
            (setq my/lsp-mode--waiting-for-direnv t)
          (my/lsp-mode-start-now))))))

(defun my/eglot-start-now (&optional interactive)
  "Start Eglot after the target environment is ready.
With INTERACTIVE, invoke `eglot' interactively so a language-specific project
finder can participate exactly as it does for `M-x eglot'."
  (my/language-server-prepare-eglot-execution-environment)
  (my/language-server-apply-process-environment)
  (my/language-server-apply-eglot-local-settings)
  (my/language-server-runtime-register-eglot-configuration)
  (when (my/eglot-contact-available-p)
    (if (or interactive
            (my/language-server-runtime-p
             my/language-server-runtime-current))
        (call-interactively #'eglot)
      (eglot-ensure))))

(defalias 'my/eglot--start-now #'my/eglot-start-now)

(defun my/eglot--direnv-ready (_environment error)
  "Resume deferred Eglot startup, falling back after direnv ERROR."
  (setq my/eglot--waiting-for-direnv nil)
  (when error
    (message
     "Eglot: direnv failed (%s); continuing with the target base environment"
     (error-message-string error)))
  (when (eq (my/language-server-preferred-backend) 'eglot)
    (my/eglot-start-now)))

(defun my/eglot-ensure-unless-lsp-mode ()
  "Start `eglot' unless another backend is active.
A slow direnv/Nix export runs asynchronously; Eglot resumes only after the
target/workspace environment has been applied to this buffer."
  (interactive)
  (when (eq (my/language-server-preferred-backend) 'eglot)
    (unless (or my/eglot--waiting-for-direnv
                (bound-and-true-p lsp-managed-mode)
                (and (fboundp 'eglot-managed-p)
                     (eglot-managed-p)))
      (let ((state
             (and
              (fboundp 'my/direnv-update-environment-maybe)
              (my/direnv-update-environment-maybe
               nil #'my/eglot--direnv-ready))))
        (if (eq state 'pending)
            (setq my/eglot--waiting-for-direnv t)
          (my/eglot-start-now))))))

(defun my/language-server--project-root-for-buffer ()
  "Return the current buffer's logical project root."
  (my/language-server--canonical-root
   (or
    (when-let* ((project
                 (ignore-errors
                   (project-current nil default-directory))))
      (project-root project))
    default-directory)))

(defun my/language-server--connect-workspace (root)
  "Open and track the language-server workspace for ROOT."
  (when root
    (my/language-server--resource-owner root)))

(defun my/eglot--connect-via-remote-a (fn &rest args)
  "Route Eglot connection startup through one owning Remote workspace."
  (let* ((project (nth 1 args))
         (root
          (my/language-server--canonical-root
           (and project (ignore-errors (project-root project)))))
         (workspace
          (my/language-server--connect-workspace root))
         (remote-current-adapter-id "language-server")
         (remote-current-workspace workspace))
    (remote-environment-ensure
     (and workspace (remote-workspace-context workspace)))
    (apply fn args)))

(defun my/lsp-mode--connect-via-remote-a (fn &rest args)
  "Route lsp-mode startup through one owning Remote workspace."
  (let* ((root (my/language-server--project-root-for-buffer))
         (workspace
          (my/language-server--connect-workspace root))
         (remote-current-adapter-id "language-server")
         (remote-current-workspace workspace))
    (remote-environment-ensure
     (and workspace (remote-workspace-context workspace)))
    (apply fn args)))

(defun my/lsp-mode--resolve-logical-command-a (fn command &optional test)
  "Resolve lsp-mode COMMAND without its TRAMP tty shell wrapper.
The `/fs:' process boundary already selects a pipe and projects the command
through the chosen backend.  Calling FN in test mode performs the same command
normalization while deliberately skipping `stty raw' and `shell-file-name'."
  (funcall
   fn command
   (or test (remote-fs-file-name-p default-directory))))

(defun my/eglot--target-command-a (fn contact)
  "Keep Eglot CONTACT as argv inside the logical `/fs:' boundary."
  (if (remote-fs-file-name-p default-directory)
      contact
    (funcall fn contact)))

(defun my/language-server--route-native-network-p ()
  "Return non-nil when a native network call belongs to LSP placement."
  (and
   (not remote-channel-native-api-inhibit)
   (equal remote-current-adapter-id "language-server")))

(defun my/language-server--open-network-stream-a
    (fn name buffer host service &rest parameters)
  "Route an LSP `open-network-stream' call through Remote channels."
  (if (my/language-server--route-native-network-p)
      (apply
       #'remote-open-network-stream
       name buffer host service
       (plist-put
        (plist-put
         (copy-sequence parameters)
         :remote-context (remote-context default-directory))
        :remote-adapter "language-server"))
    (apply fn name buffer host service parameters)))

(defun my/language-server--make-network-process-a (fn &rest plist)
  "Route an LSP `make-network-process' call through Remote channels."
  (if (my/language-server--route-native-network-p)
      (apply
       #'remote-make-network-process
       (plist-put
        (plist-put
         (copy-sequence plist)
         :remote-context (remote-context default-directory))
        :remote-adapter "language-server"))
    (apply fn plist)))

(unless (advice-member-p
         #'my/language-server--open-network-stream-a
         'open-network-stream)
  (advice-add
   'open-network-stream :around
   #'my/language-server--open-network-stream-a))

(unless (advice-member-p
         #'my/language-server--make-network-process-a
         'make-network-process)
  (advice-add
   'make-network-process :around
   #'my/language-server--make-network-process-a))

(defun my/lsp-mode--find-logical-workspace-a
    (fn server-id &optional file-name)
  "Resolve lsp-mode's generated SERVER-ID-tramp alias when necessary.
`lsp-auto-register-remote-clients' deliberately renames remote clients, while
language extensions commonly continue to call `lsp-find-workspace' with the
base ID.  Trying the generated alias preserves those extension APIs."
  (or (funcall fn server-id file-name)
      (and
       (not (string-suffix-p "-tramp" (symbol-name server-id)))
       (funcall
        fn
        (intern (format "%s-tramp" server-id))
        file-name))))

(defun my/language-server--canonical-root (root)
  "Return ROOT as a canonical logical directory, or nil."
  (when (stringp root)
    (ignore-errors
      (file-name-as-directory
       (remote-canonicalize-file-name root)))))

(defun my/language-server--path-on-root (path root)
  "Return server PATH in the logical namespace selected by ROOT.
Local and remote targets use the same projection.  Physical backend names are
canonicalized directly; a target-native absolute path is attached to ROOT's
target identity."
  (if (and (stringp path)
           (or (file-name-absolute-p path)
               (remote-fs-file-name-p path)))
      (cond
       ((or (remote-fs-file-name-p path)
            (file-remote-p path))
        (remote-canonicalize-file-name path))
       ((my/language-server--canonical-root root)
        (remote-expand-file-name
         path nil (remote-file-name-target root)))
       (t
        (remote-canonicalize-file-name path)))
    path))

(defun my/eglot--logical-root ()
  "Return the logical project root belonging to Eglot's active server."
  (or
   (when-let* ((server
                (or
                 (and (boundp 'eglot--cached-server)
                      eglot--cached-server)
                 (and (fboundp 'eglot-current-server)
                      (ignore-errors (eglot-current-server)))))
               ((fboundp 'eglot--project))
               (project (ignore-errors (eglot--project server)))
               (root (ignore-errors (project-root project))))
     (my/language-server--canonical-root root))
   (my/language-server--canonical-root default-directory)))

(defun my/eglot--uri-to-logical-a (fn uri)
  "Canonicalize Eglot URI using its server's project target."
  (my/language-server--path-on-root
   (funcall fn uri)
   (my/eglot--logical-root)))

(defun my/lsp-mode--path-to-target-uri-a (fn path)
  "Pass target-native PATH to lsp-mode's ordinary URI converter.
Logical `/fs:' identity stays in Emacs; servers receive the native path they
understand.  This also handles `/fs:local:', which is intentionally not
classified as remote by `file-remote-p'."
  (funcall
   fn
   (if (and (stringp path)
            (remote-fs-file-name-p path))
       (remote-file-local-name path)
     path)))

(defun my/lsp-mode--logical-workspace-roots ()
  "Return logical roots associated with the active lsp-mode operation.
The dynamically bound current workspace comes first.  Remaining workspace
roots are retained for callbacks which are not run in a source buffer."
  (let* ((current
          (and (boundp 'lsp--cur-workspace)
               lsp--cur-workspace))
         (workspaces
          (delete-dups
           (delq
            nil
            (append
             (and current (list current))
             (and (fboundp 'lsp-workspaces)
                  (ignore-errors (lsp-workspaces)))))))
         roots)
    (when (fboundp 'lsp--workspace-root)
      (dolist (workspace workspaces)
        (when-let* ((root
                     (ignore-errors
                       (lsp--workspace-root workspace)))
                    (canonical
                     (my/language-server--canonical-root root)))
          (push canonical roots))))
    (nreverse (delete-dups roots))))

(defun my/lsp-mode--logical-root-for-path (path)
  "Return the lsp-mode workspace root which owns target-native PATH."
  (let ((roots (my/lsp-mode--logical-workspace-roots)))
    (or
     ;; The first root belongs to dynamically bound `lsp--cur-workspace'.
     ;; It is authoritative even when two targets expose the same localname.
     (and (boundp 'lsp--cur-workspace)
          lsp--cur-workspace
          (car roots))
     (car
      (sort
       (seq-filter
        (lambda (root)
          (and
           (stringp path)
           (let ((native-root
                  (file-name-as-directory
                   (remote-file-local-name root))))
             (or (equal path (directory-file-name native-root))
                 (string-prefix-p native-root path)))))
        roots)
       (lambda (left right)
         (> (length (remote-file-local-name left))
            (length (remote-file-local-name right))))))
     (car roots)
     (my/language-server--canonical-root default-directory))))

(defun my/lsp-mode--uri-to-logical-a (fn uri)
  "Restore lsp-mode URI to its workspace's logical target namespace."
  (let ((path (funcall fn uri)))
    (my/language-server--path-on-root
     path
     (my/lsp-mode--logical-root-for-path path))))

(defvar my/language-server--recovering-resource-p nil
  "Non-nil while workspace recovery deliberately restarts an LSP handle.")

(defun my/language-server--resource-owner (root)
  "Return the Remote workspace which owns logical ROOT."
  (when-let* ((root (my/language-server--canonical-root root))
              (workspace
               (remote-workspace-open root :connect nil)))
    ;; The LSP process already acquired this route.  Tracking it here gives
    ;; transport failure matching and reconnect a stable workspace boundary
    ;; without claiming another session reference.
    (remote-workspace-track-route
     workspace "language-server" 'process-async)
    workspace))

(defun my/language-server--forget-resource-value (value)
  "Forget every LSP resource whose current handle is VALUE."
  (dolist (workspace (hash-table-values remote-workspaces))
    (dolist (resource
             (copy-sequence
              (remote-workspace-resources workspace)))
      (when (and
             (eq (remote-workspace-resource-kind resource) 'lsp)
             (eq (remote-workspace-resource-value resource) value))
        (remote-workspace-forget-resource workspace resource)))))

(defun my/language-server--eglot-root (server)
  "Return SERVER's canonical logical project root."
  (when-let* ((project (ignore-errors (eglot--project server)))
              (root (ignore-errors (project-root project))))
    (my/language-server--canonical-root root)))

(defun my/language-server--skip-file-watch-p (root)
  "Return non-nil when an LSP client must decline dynamic watches for ROOT.
The decision is based on client filesystem placement, not on a local/remote
target branch.  A mounted or otherwise client-accessible target therefore
keeps exactly the same watcher path as an ordinary local project."
  (pcase my/language-server-file-watch-policy
    ('disabled t)
    ('native nil)
    ('auto
     (and
      root
      (not (ignore-errors (remote-client-file-name root)))))
    (_
     (error
      "Invalid `my/language-server-file-watch-policy': %S"
      my/language-server-file-watch-policy))))

(defalias 'my/language-server--eglot-skip-file-watch-p
  #'my/language-server--skip-file-watch-p)

(defun my/eglot-register-capability-via-remote-a
    (fn server method id &rest params)
  "Run Eglot capability registration in SERVER's Remote workspace.
For target-only roots, acknowledge but decline dynamic file-watch
registrations.  Eglot advertises those registrations as unsupported on remote
projects; accepting a server which ignores that advertisement makes upstream
Eglot recursively install one process-backed watch per directory."
  (let* ((root (my/language-server--eglot-root server))
         (workspace
          (and root (my/language-server--resource-owner root)))
         (remote-current-adapter-id "language-server")
         (remote-current-workspace workspace))
    (if (and
         (eq method 'workspace/didChangeWatchedFiles)
         (my/language-server--skip-file-watch-p root))
        (progn
          (remote-log
           'lsp-watch-registration-declined
           :backend 'eglot
           :root root
           :registration-id id
           :watcher-count
           (length (or (plist-get params :watchers) []))
           :policy my/language-server-file-watch-policy)
          nil)
      (apply fn server method id params))))

(defun my/lsp-mode--register-capability-via-remote-a
    (fn registration)
  "Register lsp-mode REGISTRATION under the shared watcher policy.
The registration is still retained by lsp-mode, but its recursive client-side
watch creation is disabled for target-only filesystems."
  (let* ((method (lsp-get registration :method))
         (root
          (and lsp--cur-workspace
               (my/language-server--lsp-workspace-root lsp--cur-workspace)))
         (skip
          (and
           (equal method "workspace/didChangeWatchedFiles")
           (my/language-server--skip-file-watch-p root)))
         (lsp-enable-file-watchers
          (and lsp-enable-file-watchers (not skip))))
    (when skip
      (remote-log
       'lsp-watch-registration-declined
       :backend 'lsp-mode
       :root root
       :policy my/language-server-file-watch-policy))
    (funcall fn registration)))

(defun my/language-server-register-eglot-resource (server)
  "Register initialized Eglot SERVER with its Remote workspace owner."
  (when-let* ((project (ignore-errors (eglot--project server)))
              (root (my/language-server--eglot-root server))
              (workspace (my/language-server--resource-owner root)))
    (let ((runtime-id
           (and (eq (car-safe project) 'my/language-server-runtime-project)
                (nth 2 project)))
          (buffer
           (seq-find
            #'buffer-live-p
            (ignore-errors (eglot--managed-buffers server)))))
      (remote-workspace-ensure-recoverable-resource
       workspace 'lsp (append (list 'eglot root)
                              (and runtime-id (list runtime-id))) server
       :close
       (lambda (value reason)
         (unless (eq reason 'transport-recovery)
           (ignore-errors
             (eglot-shutdown value nil 1 t))))
       :recover
       (lambda (resource _owner)
         (let* ((old (remote-workspace-resource-value resource))
                (metadata
                 (remote-workspace-resource-metadata resource))
                (owner-buffer (plist-get metadata :buffer))
                (my/language-server--recovering-resource-p t))
           (eglot-reconnect old)
           (or
            (and
             (buffer-live-p owner-buffer)
             (with-current-buffer owner-buffer
               (ignore-errors (eglot-current-server))))
            old)))
       :metadata
       (list :backend 'eglot :root root :runtime-id runtime-id
             :buffer buffer)))))

(defun my/language-server-eglot-shutdown-resource-a (server)
  "Forget the workspace resource for an Eglot SERVER which already stopped."
  (unless my/language-server--recovering-resource-p
    (my/language-server--forget-resource-value server)))

(defun my/language-server--lsp-workspace-root (workspace)
  "Return lsp-mode WORKSPACE's canonical logical root."
  (my/language-server--canonical-root
   (ignore-errors (lsp--workspace-root workspace))))

(defun my/language-server--lsp-workspace-id (workspace)
  "Return a stable server ID for lsp-mode WORKSPACE."
  (or
   (ignore-errors
     (lsp--client-server-id
      (lsp--workspace-client workspace)))
   'unknown))

(defun my/lsp-mode--workspace-key (workspace)
  "Return a stable crash-accounting key for WORKSPACE."
  (list
   (my/language-server--lsp-workspace-id workspace)
   (my/language-server--lsp-workspace-root workspace)))

(defun my/lsp-mode--cancel-startup-watchdog (workspace)
  "Cancel WORKSPACE's startup watchdog, if any."
  (when-let* ((timer (gethash workspace my/lsp-mode--startup-timers)))
    (cancel-timer timer)
    (remhash workspace my/lsp-mode--startup-timers)))

(defun my/lsp-mode-shutdown-workspace (workspace &optional reason)
  "Stop lsp-mode WORKSPACE without allowing shutdown to block indefinitely.
REASON is recorded for diagnostics.  The shutdown action is marked before any
RPC is sent, closing the race in which a dying process could auto-restart
while an explicit shutdown was still waiting for its response.  Repeated calls
only enforce process termination; they never send a second shutdown RPC."
  (when workspace
    (my/lsp-mode--cancel-startup-watchdog workspace)
    (let ((already-stopping
           (eq
            (ignore-errors (lsp--workspace-shutdown-action workspace))
            'shutdown)))
      (ignore-errors
        (my/language-server--set-struct-slot
         workspace 'lsp--workspace 'shutdown-action 'shutdown))
      (unless already-stopping
        (condition-case error
            (if (and (numberp my/lsp-mode-shutdown-timeout)
                     (> my/lsp-mode-shutdown-timeout 0))
                (with-timeout
                    (my/lsp-mode-shutdown-timeout
                     (signal
                      'timeout
                      (list "lsp-mode workspace shutdown timed out")))
                  (lsp-workspace-shutdown workspace))
              (lsp-workspace-shutdown workspace))
          (error
           (remote-log
            'lsp-shutdown-error
            :backend 'lsp-mode
            :root (my/language-server--lsp-workspace-root workspace)
            :reason reason
            :error (error-message-string error))))))
    ;; Graceful shutdown normally kills CMD-PROC.  Force both handles as a
    ;; final idempotent boundary for dead transports and half-started servers.
    (dolist (process
             (delete-dups
              (delq
               nil
               (list
                (ignore-errors (lsp--workspace-proc workspace))
                (ignore-errors (lsp--workspace-cmd-proc workspace))))))
      (ignore-errors (lsp-process-kill process)))
    t))

(defun my/lsp-mode-shutdown-all ()
  "Boundedly stop every active lsp-mode workspace."
  (dolist (workspace
           (delete-dups
            (delq
             nil
             (ignore-errors
               (lsp--session-workspaces (lsp-session))))))
    (my/lsp-mode-shutdown-workspace workspace 'emacs-exit)))

(defun my/lsp-mode--arm-startup-watchdog ()
  "Arm a bounded startup watchdog for `lsp--cur-workspace'."
  (when (and
         lsp--cur-workspace
         (numberp my/lsp-mode-startup-timeout)
         (> my/lsp-mode-startup-timeout 0))
    (my/lsp-mode--cancel-startup-watchdog lsp--cur-workspace)
    (let ((workspace lsp--cur-workspace))
      (puthash
       workspace
       (run-at-time
        my/lsp-mode-startup-timeout nil
        (lambda (value)
          (remhash value my/lsp-mode--startup-timers)
          (when
              (eq
               (ignore-errors (lsp--workspace-status value))
               'starting)
            (remote-log
             'lsp-startup-timeout
             :backend 'lsp-mode
             :server (my/language-server--lsp-workspace-id value)
             :root (my/language-server--lsp-workspace-root value)
             :timeout my/lsp-mode-startup-timeout)
            (message
             "LSP startup timed out after %.1fs: %s"
             my/lsp-mode-startup-timeout
             (or (my/language-server--lsp-workspace-root value)
                 "unknown workspace"))
            (my/lsp-mode-shutdown-workspace value 'startup-timeout)))
        workspace)
       my/lsp-mode--startup-timers))))

(defun my/lsp-mode--workspace-initialized-h ()
  "Clear startup and crash-loop state for the initialized workspace."
  (when lsp--cur-workspace
    (my/lsp-mode--cancel-startup-watchdog lsp--cur-workspace)))

(defun my/lsp-mode--workspace-uninitialized-h (workspace)
  "Clear the startup watchdog belonging to uninitialized WORKSPACE."
  (my/lsp-mode--cancel-startup-watchdog workspace))

(defun my/lsp-mode--restart-with-circuit-breaker-a (fn workspace)
  "Call lsp-mode restart FN for WORKSPACE unless it is crash-looping."
  (let* ((key (my/lsp-mode--workspace-key workspace))
         (now (float-time))
         (history
          (seq-filter
           (lambda (timestamp)
             (< (- now timestamp) my/lsp-mode-restart-window))
           (gethash key my/lsp-mode--restart-history))))
    (if (>= (length history) (max 0 my/lsp-mode-restart-limit))
        (progn
          (ignore-errors
            (my/language-server--set-struct-slot
             workspace 'lsp--workspace 'shutdown-action 'shutdown))
          (remote-log
           'lsp-restart-circuit-open
           :backend 'lsp-mode
           :server (car key)
           :root (cadr key)
           :attempts (length history)
           :window my/lsp-mode-restart-window)
          (message
           "LSP restart stopped after %d failure(s) in %.0fs: %s"
           (length history)
           my/lsp-mode-restart-window
           (or (cadr key) (car key))))
      (puthash key (cons now history) my/lsp-mode--restart-history)
      (funcall fn workspace))))

(defun my/lsp-mode--read-state-safely-a (fn file)
  "Read lsp-mode state FILE with FN, treating truncated state as empty."
  (condition-case error
      (funcall fn file)
    ((end-of-file invalid-read-syntax)
     (remote-log
      'lsp-session-read-failed
      :file file
      :error (error-message-string error))
     nil)))

(defun my/lsp-mode--persist-atomically-a (_fn file object)
  "Persist lsp-mode OBJECT to FILE with an atomic same-directory rename."
  (let* ((directory (file-name-directory file))
         (print-length nil)
         (print-level nil)
         temporary)
    (make-directory directory t)
    (setq temporary
          (make-temp-file
           (expand-file-name ".lsp-session-" directory)))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix)
              (inhibit-message t))
          (write-region
           (prin1-to-string object) nil temporary nil 'silent)
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun my/language-server-register-lsp-resource ()
  "Register the dynamically active lsp-mode workspace for recovery."
  (when-let* ((lsp-workspace lsp--cur-workspace)
              (root
               (my/language-server--lsp-workspace-root lsp-workspace))
              (workspace (my/language-server--resource-owner root)))
    (remote-workspace-ensure-recoverable-resource
     workspace 'lsp
     (list 'lsp-mode
           (my/language-server--lsp-workspace-id lsp-workspace)
           root)
     lsp-workspace
     :close
     (lambda (value reason)
       (unless (eq reason 'transport-recovery)
         (my/lsp-mode-shutdown-workspace value reason)))
     :recover
     (lambda (resource _owner)
       (let ((value (remote-workspace-resource-value resource))
             (my/language-server--recovering-resource-p t))
         (lsp-workspace-restart value)
         value))
     :metadata
     (list
      :backend 'lsp-mode
      :root root
      :buffer
      (seq-find
       #'buffer-live-p
       (ignore-errors
         (lsp--workspace-buffers lsp-workspace)))))))

(defun my/language-server-unregister-lsp-resource (workspace)
  "Forget an lsp-mode WORKSPACE after lsp-mode has already closed it."
  (my/lsp-mode--workspace-uninitialized-h workspace)
  (unless my/language-server--recovering-resource-p
    (my/language-server--forget-resource-value workspace)))

(defun my/eglot-ensure ()
  "Start `eglot' in programming buffers that do not opt into `lsp-mode'."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (my/eglot-ensure-unless-lsp-mode)))

(defun my/language-server-ensure-deferred ()
  "Start the selected language-server client after the buffer opens."
  (unless (derived-mode-p 'lean-mode)
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (my/language-server-ensure))))
       buffer))))

(defalias 'my/eglot-ensure-deferred
  #'my/language-server-ensure-deferred)

(defun my/language-server--ensure-after-runtime ()
  "Start the preferred backend after runtime preparation has completed."
  (interactive)
  (pcase (my/language-server-preferred-backend)
    ('lsp-mode (my/lsp-mode-ensure))
    ('eglot (my/eglot-ensure))
    ('disabled (message "Language server disabled for this project"))))

(defun my/language-server--runtime-ready (_runtime error)
  "Resume startup after resolving a runtime, reporting fallback ERROR."
  (setq my/language-server--waiting-for-runtime nil)
  (when error
    (my/language-server-runtime-report-fallback error))
  (my/language-server--ensure-after-runtime))

(defun my/language-server-ensure ()
  "Prepare the effective runtime, then start the preferred language server."
  (interactive)
  (unless my/language-server--waiting-for-runtime
    (let ((state
           (my/language-server-runtime-prepare
            #'my/language-server--runtime-ready)))
      (when (eq state 'pending)
        (setq my/language-server--waiting-for-runtime t)))))

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
     company-yasnippet
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
  (setq-local company-idle-delay 0.25)
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
  (setq company-idle-delay 0.28
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
    ;; `company-show-numbers' is the old UI for candidate shortcuts.  Keep the
    ;; newer quick-access hints above as the single numbering surface.
    (setq company-show-numbers nil))
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

(defconst my/company-tooltip-frontends
  '(company-pseudo-tooltip-frontend
    company-pseudo-tooltip-unless-just-one-frontend
    company-pseudo-tooltip-unless-just-one-frontend-with-delay
    company-childframe-frontend
    company-childframe-unless-just-one-frontend
    company-box-frontend)
  "Company frontends that each own the completion candidate popup.")

(defun my/company-box-normalize-frontends ()
  "Keep Company-box as the only candidate-popup frontend in this buffer.
Company 1.1 added its own child-frame frontend.  Older Company-box releases
remove only Company's pseudo-tooltip frontends, leaving both child frames to
render the same CAPF/Eglot candidates on top of each other."
  (when (bound-and-true-p company-box-mode)
    (setq-local
     company-frontends
     (cons
      'company-box-frontend
      (seq-remove
       (lambda (frontend)
         (memq frontend my/company-tooltip-frontends))
       company-frontends)))
    ;; A completion may already have made Company's built-in child frame
    ;; visible before Company-box finished enabling.
    (when (fboundp 'company-childframe-hide)
      (company-childframe-hide))))

(use-package company-box
  :ensure t
  :if window-system
  :hook ((company-mode . company-box-mode)
         (company-box-mode . my/company-box-normalize-frontends))
  :custom
  (company-box-doc-delay 0.45)
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
      (error nil)))
  ;; Also repair buffers that survived a configuration reload.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p company-box-mode)
        (my/company-box-normalize-frontends)))))

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
  (flymake-no-changes-timeout 1.0) ; 输入停顿后再自动检查，减少每次按键后的后台唤醒。
  (flymake-indicator-type 'fringes)
  (flymake-fringe-indicator-position 'left-fringe))

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
  (add-hook
   'lsp-after-initialize-hook
   #'my/language-server-register-lsp-resource)
  (add-hook
   'lsp-after-uninitialized-functions
   #'my/language-server-unregister-lsp-resource)
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

(with-eval-after-load 'lsp-mode
  (add-hook
   'lsp-before-initialize-hook
   #'my/lsp-mode--arm-startup-watchdog)
  (add-hook
   'lsp-after-initialize-hook
   #'my/lsp-mode--workspace-initialized-h)
  (unless (advice-member-p #'my/lsp-mode--connect-via-remote-a 'lsp)
    (advice-add 'lsp :around #'my/lsp-mode--connect-via-remote-a))
  (unless (advice-member-p
           #'my/lsp-mode--resolve-logical-command-a
           'lsp-resolve-final-command)
    (advice-add
     'lsp-resolve-final-command
     :around #'my/lsp-mode--resolve-logical-command-a))
  (unless (advice-member-p
           #'my/lsp-mode--find-logical-workspace-a
           'lsp-find-workspace)
    (advice-add
     'lsp-find-workspace
     :around #'my/lsp-mode--find-logical-workspace-a))
  (unless (advice-member-p
           #'my/lsp-mode--path-to-target-uri-a
           'lsp--path-to-uri)
    (advice-add
     'lsp--path-to-uri
     :around #'my/lsp-mode--path-to-target-uri-a))
  (unless (advice-member-p
           #'my/lsp-mode--uri-to-logical-a
           'lsp--uri-to-path)
    (advice-add
     'lsp--uri-to-path
     :around #'my/lsp-mode--uri-to-logical-a))
  (unless (advice-member-p
           #'my/lsp-mode--register-capability-via-remote-a
           'lsp--server-register-capability)
    (advice-add
     'lsp--server-register-capability
     :around #'my/lsp-mode--register-capability-via-remote-a))
  (unless (advice-member-p
           #'my/lsp-mode--restart-with-circuit-breaker-a
           'lsp--restart-if-needed)
    (advice-add
     'lsp--restart-if-needed
     :around #'my/lsp-mode--restart-with-circuit-breaker-a))
  (unless (advice-member-p
           #'my/lsp-mode--read-state-safely-a
           'lsp--read-from-file)
    (advice-add
     'lsp--read-from-file
     :around #'my/lsp-mode--read-state-safely-a))
  (unless (advice-member-p
           #'my/lsp-mode--persist-atomically-a
           'lsp--persist)
    (advice-add
     'lsp--persist
     :around #'my/lsp-mode--persist-atomically-a))
  (unless (advice-member-p
           #'my/lsp-mode-shutdown-all
           'lsp--global-teardown)
    (advice-add
     'lsp--global-teardown
     :override #'my/lsp-mode-shutdown-all)))


;; -------------------------
;; 5. Eglot (LSP Client)
;; -------------------------
(use-package eglot
  :ensure nil ; Built-in since Emacs 29
  :hook ((prog-mode . my/language-server-ensure-deferred)
         (eglot-managed-mode . (lambda ()
                                 (when (fboundp 'eglot-inlay-hints-mode)
                                   (eglot-inlay-hints-mode 1)))))
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil)
  (eglot-code-action-indications nil)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (read-process-output-max (* 1024 1024)))

(with-eval-after-load 'eglot
  (add-hook
   'eglot-connect-hook
   #'my/language-server-register-eglot-resource)
  (unless (advice-member-p #'my/eglot--connect-via-remote-a
                           'eglot--connect)
    (advice-add 'eglot--connect :around
                #'my/eglot--connect-via-remote-a))
  (unless (advice-member-p #'my/eglot--target-command-a
                           'eglot--cmd)
    (advice-add 'eglot--cmd :around
                #'my/eglot--target-command-a))
  (unless (advice-member-p #'my/eglot--uri-to-logical-a
                           'eglot-uri-to-path)
    (advice-add 'eglot-uri-to-path :around
                #'my/eglot--uri-to-logical-a))
  (unless (advice-member-p
           #'my/eglot-register-capability-via-remote-a
           'eglot-register-capability)
    (advice-add
     'eglot-register-capability
     :around
     #'my/eglot-register-capability-via-remote-a))
  (unless (advice-member-p
           #'my/language-server-eglot-shutdown-resource-a
           'eglot--on-shutdown)
    (advice-add
     'eglot--on-shutdown :after
     #'my/language-server-eglot-shutdown-resource-a))
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
                        (when (and deferred-server
                                   (ignore-errors
                                     (jsonrpc-running-p deferred-server))
                                   (null (ignore-errors
                                           (eglot--managed-buffers
                                            deferred-server))))
                          (ignore-errors
                            (funcall orig-shutdown deferred-server))))
                      srv)))))
        (funcall fn server))))

  (defun my/eglot-shutdown-all-on-exit-h ()
    "Cleanly shut down all Eglot servers before Emacs exits.
Lean's LSP watchdog (and similar servers) fork worker subprocesses that
only get reaped on a clean LSP shutdown/exit; killing Emacs without this
leaves those workers orphaned and running."
    (when (fboundp 'eglot-shutdown-all)
      (with-demoted-errors "eglot-shutdown-all-on-exit: %S"
        (eglot-shutdown-all))))
  (add-hook 'kill-emacs-hook #'my/eglot-shutdown-all-on-exit-h))


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
;; 7. Misc & Language Init
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
(require 'init-languagetool)
(require 'init-beancount)
(require 'init-sh)
(require 'init-java)
(require 'init-lean)
(require 'init-md)
(require 'init-nix)
(require 'init-html)
(require 'init-js2)
(require 'init-latex)

;;; ── CodeLens ──────────────────────────────────────────────────────────────
;; Off by default. Toggle with SPC c L.
;; When enabled, automatically re-initialises after eglot reconnects.
(add-to-list
 'load-path
 (expand-file-name
  "../site-lisp/codelens"
  (file-name-directory
   (or load-file-name
       (locate-library "init-lsp")
       default-directory))))

(use-package eglot-codelens
  :after eglot
  :commands eglot-codelens-mode
  :custom
  (eglot-codelens-update-delay 0.5)
  (eglot-codelens-visible-refresh-delay 0.5)
  :config
  (defun my/eglot-codelens-managed-mode-h ()
    "Sync CodeLens with eglot lifecycle — re-fetch on connect, clean up on disconnect."
    (when eglot-codelens-mode
      (if (and (eglot-managed-p)
               (eglot-current-server)
               (eglot-server-capable :codeLensProvider))
          (progn
            (eglot-codelens--setup-buffer)
            (eglot-codelens--fetch-codelens))
        (eglot-codelens--cleanup-buffer))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-codelens-managed-mode-h))

(provide 'init-lsp)
;;; init-lsp.el ends here
