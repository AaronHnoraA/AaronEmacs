;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; `lsp-mode' is the only language-server client.  Every remote concern —
;; `/fs:' identity, URI anchoring, command projection, capability
;; registration, workspace resource ownership and reconnect — is expressed
;; once here against the public `remote-*' API, with target `local' as an
;; ordinary target rather than a second code path.  Diagnostics go to
;; Flymake; the maintenance/dashboard layer lives in `init-lsp-tools.el'.

;;; Code:

(require 'config)

(require 'aaron-ui)
(require 'cl-lib)
(require 'init-lsp-toolchain)
(require 'init-lsp-runtime)
(require 'project)
(require 'remote-channel)
(require 'remote-doctor)
(require 'remote-process)
(require 'remote-environment)
(require 'remote-workspace)
(require 'subr-x)

(declare-function my/tab-line-refresh "init-tabbar" (&rest arguments))
(declare-function my/show-imenu-from-breadcrumb "init-project" (event))
(declare-function breadcrumb--header-line "breadcrumb" ())
(declare-function bc--header-line "breadcrumb" ())
(defvar my/tab-line-leading-segment-functions)

(eval-when-compile
  (ignore-errors
    (require 'hydra)))

(defgroup my/language-server nil
  "Routing and observability helpers for the language server stack."
  :group 'tools
  :prefix "my/language-server-")

(defvar my/lsp-mode-required-features nil
  "Alist mapping major modes to extra `lsp-mode' support features.")

(defvar my/language-server-lsp-local-settings-hook nil
  "Hook run after environment resolution and before lsp-mode starts.")

(defvar my/language-server-disabled-modes nil
  "Major modes that should never auto-start a language server.")

(defvar my/language-server-program-metadata nil
  "Metadata for language servers registered by `my/register-language-server'.

Each entry is a plist with keys such as `:modes', `:program', `:server-id',
`:feature', `:executables', `:placement', `:label', `:source', and `:note'.
The Hub and Doctor read it to list routes and jump back to the declaring
file; it is the only registry of locally declared servers.")

(defvar my/language-server--resolving-executable-p nil
  "Non-nil inside the Remote executable lookup implementation.")

(config-defvar my/language-server-performance-read-process-output-max nil
  "Minimum `read-process-output-max' while any language server is active."
  :type 'integer
  :group 'my/language-server)

(config-defvar my/language-server-performance-gcmh-factor nil
  "Multiplier applied to `gcmh-high-cons-threshold' while LSP is active."
  :type 'integer
  :group 'my/language-server)

(config-defvar my/language-server-defer-shutdown nil
  "Seconds to keep a language-server workspace alive after its last buffer."
  :type '(choice (const :tag "Disabled" nil) (integer :tag "Seconds"))
  :group 'my/language-server)

(config-defvar my/language-server-file-watch-policy 'auto
  "Policy for language-server dynamic file-watch registrations.

`auto' preserves lsp-mode's native watcher behavior when the project has a
path directly accessible to the Emacs client, and otherwise uses the Remote
backend's `watch' capability.  Remote watches retain logical `/fs:' identity,
workspace ownership and recovery instead of becoming an unrelated set of
raw TRAMP descriptors.

`native' always accepts the registration, and is intended only for backends
whose watcher implementation is known to scale.  `disabled' declines all
dynamic file-watch registrations."
  :type '(choice
          (const :tag "Automatic placement-aware policy" auto)
          (const :tag "Always use native watchers" native)
          (const :tag "Disable dynamic watchers" disabled))
  :group 'my/language-server)

(config-defvar my/language-server-booster-required nil
  "Whether `emacs-lsp-booster' is mandatory for every language server.

The booster pre-parses server JSON into Emacs bytecode and is resolved per
target through `remote-executable-find', so a remote server never runs the
client's copy.  The default is nil: missing booster support falls back to the
ordinary server command and Doctor reports the lost optimisation."
  :type 'boolean
  :group 'my/language-server)

(config-defvar my/language-server-visible-render-margin 8
  "Extra lines rendered above and below each visible LSP window.
CodeLens and inlay hints keep this small warm margin so scrolling remains
smooth without creating overlays throughout the buffer."
  :type 'integer
  :group 'my/language-server)

(config-register
 'lsp-restart
 :type '(choice (const interactive)
                (const auto-restart)
                (const ignore))
 :group 'my/language-server
 :doc "lsp-mode policy after a language-server process exits.")

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
(defvar lsp-response-timeout)
(defvar lsp-imenu-detailed-outline)
(defvar lsp-imenu-sort-methods)
(defvar lsp--line-col-to-point-hash-table)
(defvar lsp-use-workspace-root-for-server-default-directory)
(defvar read-process-output-max)
(defvar remote-file-watch-workspace)
(defvar remote-file-watch-metadata)

(dolist (adapter '("language-server" "lsp-mode"))
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

(defvar-local my/lsp-mode--waiting-for-direnv nil
  "Non-nil while lsp-mode startup waits for an asynchronous direnv export.")

(defvar-local my/language-server--waiting-for-runtime nil
  "Non-nil while language-server startup waits for a runtime provider.")

(defvar-local my/language-server--manual-start nil
  "Non-nil when the pending language-server start was explicitly requested.")

(defvar-local my/language-server--workspace-configuration nil
  "Buffer-local workspace-configuration override for the language server.

Merged from the project-local `:lsp-workspace' value and the active
toolchain profile.  It layers above `lsp-mode''s global
`lsp-client-settings' rather than replacing it.")

(defvar-local my/lsp-document-color-last-visible-region nil
  "Last visible region requested from `textDocument/documentColor'.")

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
(declare-function lsp--document-color "lsp-mode" ())
(declare-function lsp--range-to-region "lsp-mode" (range))
(declare-function lsp--semantic-tokens-request "lsp-semantic-tokens"
                  (region fontify-immediately))
(declare-function lsp--update-inlay-hints "lsp-mode" ())
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp--workspace-client "lsp-mode" (workspace))
(declare-function lsp--workspace-cmd-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-root "lsp-mode" (workspace))
(declare-function lsp--workspace-status "lsp-mode" (workspace))
(declare-function lsp--workspace-shutdown-action "lsp-mode" (workspace))
(declare-function lsp--client-server-id "lsp-mode" (client))
(declare-function lsp--client-remote? "lsp-mode" (client))
(declare-function lsp--session-workspaces "lsp-mode" (session))
(declare-function lsp-get "lsp-protocol" (hash-table key))
(declare-function lsp-process-kill "lsp-mode" (process))
(declare-function lsp-session "lsp-mode" ())
(declare-function lsp-workspace-shutdown "lsp-mode" (workspace))
(declare-function lsp-workspace-restart "lsp-mode" (workspace))
(declare-function jsonrpc-running-p "jsonrpc" (connection))
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function make-lsp-client "lsp-mode" (&rest args))
(declare-function lsp-stdio-connection "lsp-mode" (command &optional test-command))
(declare-function lsp--filter-clients "lsp-mode" (pred))
(declare-function lsp--supports-buffer? "lsp-mode" (client))
(declare-function lsp--server-binary-present? "lsp-mode" (client))
(declare-function lsp--set-configuration "lsp-mode" (settings))
(declare-function lsp--position-to-point "lsp-mode" (position))
(declare-function lsp--collect-lines-and-cols "lsp-mode" (symbols))
(declare-function lsp--convert-line-col-to-points-batch "lsp-mode" (line-col-list))
(declare-function lsp--get-line-and-col "lsp-mode" (symbol))
(declare-function lsp--imenu-filter-symbols "lsp-mode" (symbols))
(declare-function lsp--imenu-hierarchical-p "lsp-mode" (symbols))
(declare-function lsp--imenu-symbol-lessp "lsp-mode" (left right))
(declare-function lsp-document-symbol? "lsp-protocol" (value))
(declare-function lsp-imenu-create-categorized-index "lsp-mode" (symbols))
(declare-function lsp-render-symbol "lsp-mode" (symbol detailed-p))
(declare-function lsp-ui-doc-glance "lsp-ui-doc" ())
(declare-function lsp-ui-doc-show "lsp-ui-doc" ())
(declare-function lsp-ui-doc-hide "lsp-ui-doc" ())
(declare-function breadcrumb-local-mode "breadcrumb" (&optional arg))

(defcustom my/lsp-mode-startup-timeout 60
  "Seconds an lsp-mode workspace may remain in `starting' state.
After the deadline the workspace is stopped through the bounded shutdown
path.  Nil or a non-positive value disables the watchdog.  See
`my/lsp-mode-startup-timeout-overrides' for slower servers."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'my/language-server)

(defcustom my/lsp-mode-startup-timeout-overrides
  '((jdtls . 180) (jdtls-tramp . 180))
  "Per-server-id overrides for `my/lsp-mode-startup-timeout'.
JDTLS routinely exceeds the shared default on a first Gradle/Maven
import combined with target provisioning; raising the default itself
would blunt the crash-loop watchdog for every other, normally
fast-starting lsp-mode server."
  :type '(alist :key-type symbol :value-type number)
  :group 'my/language-server)

(defcustom my/lsp-mode-shutdown-timeout 3
  "Maximum seconds allowed for one graceful lsp-mode workspace shutdown.
The process is force-stopped after this deadline so quitting Emacs never
depends on a responsive language server."
  :type 'number
  :group 'my/language-server)

(defcustom my/lsp-mode-remote-shutdown-response-timeout 2
  "Seconds allowed for a remote server's `shutdown' response.
lsp-mode normally hard-codes half a second during teardown, which is below a
normal SSH round trip plus analyzer cleanup on many targets.  This bound stays
below `my/lsp-mode-shutdown-timeout', after which the process is force-stopped."
  :type 'number
  :group 'my/language-server)

(defcustom my/lsp-mode-restart-limit 1
  "Maximum automatic lsp-mode restarts within the restart window.
See `my/lsp-mode-restart-limit-overrides' for slower servers."
  :type 'integer
  :group 'my/language-server)

(defcustom my/lsp-mode-restart-limit-overrides
  '((jdtls . 3) (jdtls-tramp . 3))
  "Per-server-id overrides for `my/lsp-mode-restart-limit'.
JDTLS's first-import failure modes (a still-provisioning target, a
half-imported Gradle workspace) can legitimately need more than one
automatic retry; other servers keep the shared, stricter default."
  :type '(alist :key-type symbol :value-type integer)
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
    (let ((my/language-server--resolving-executable-p t))
      (remote-executable-find program))))

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
    (lsp-inlay-hints-mode 1))
  ;; Document colors are requested on edits upstream.  Track viewport changes
  ;; on the existing LSP idle cycle as well, without issuing another request
  ;; merely because point moved within the same window.
  (if (and (bound-and-true-p lsp-managed-mode)
           (bound-and-true-p lsp-enable-text-document-color)
           (ignore-errors (lsp-feature? "textDocument/documentColor")))
      (add-hook 'lsp-on-idle-hook
                #'my/lsp-document-color-refresh-visible nil t)
    (remove-hook 'lsp-on-idle-hook
                 #'my/lsp-document-color-refresh-visible t)
    (setq my/lsp-document-color-last-visible-region nil)))

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

(defun my/language-server--mapping-p (value)
  "Return non-nil when VALUE is a non-nil keyed configuration mapping."
  (or (hash-table-p value)
      (and (consp value)
           (or (my/language-server--plist-like-p value)
               (my/language-server--alist-like-p value)))))

(defun my/language-server--copy-value (value)
  "Return a recursive copy of configuration VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (maphash
       (lambda (key item)
         (puthash key (my/language-server--copy-value item) copy))
       value)
      copy))
   ((vectorp value)
    (vconcat (mapcar #'my/language-server--copy-value value)))
   ((consp value) (copy-tree value))
   (t value)))

(defun my/language-server--key-candidates (key)
  "Return equivalent plist, alist and JSON spellings for KEY."
  (delete-dups
   (delq
    nil
    (cond
     ((keywordp key)
      (let ((name (substring (symbol-name key) 1)))
        (list key name (intern name))))
     ((stringp key)
      (list key (intern key) (intern (concat ":" key))))
     ((symbolp key)
      (let ((name (symbol-name key)))
        (list key name
              (and (not (string-prefix-p ":" name))
                   (intern (concat ":" name))))))
     (t (list key))))))

(defun my/language-server--mapping-ref (mapping key missing)
  "Return MAPPING's KEY value, or MISSING when the key is absent."
  (catch 'found
    (dolist (candidate (my/language-server--key-candidates key))
      (cond
       ((hash-table-p mapping)
        (let ((value (gethash candidate mapping missing)))
          (unless (eq value missing)
            (throw 'found value))))
       ((my/language-server--plist-like-p mapping)
        (when-let* ((tail (plist-member mapping candidate)))
          (throw 'found (cadr tail))))
       ((my/language-server--alist-like-p mapping)
        (when-let* ((entry (assoc candidate mapping)))
          (throw 'found (cdr entry))))))
    missing))

(defun my/language-server--mapping-key (mapping key)
  "Return MAPPING's existing spelling for KEY, or its preferred new spelling."
  (or
   (catch 'found
     (dolist (candidate (my/language-server--key-candidates key))
       (when
           (cond
            ((hash-table-p mapping)
             (not (eq (gethash candidate mapping 'my/missing) 'my/missing)))
            ((my/language-server--plist-like-p mapping)
             (plist-member mapping candidate))
            ((my/language-server--alist-like-p mapping)
             (assoc candidate mapping)))
         (throw 'found candidate))))
   (cond
    ((hash-table-p mapping)
     (if-let* ((sample (car (hash-table-keys mapping))))
         (cond
          ((stringp sample) (format "%s" (if (keywordp key)
                                                (substring (symbol-name key) 1)
                                              key)))
          ((keywordp sample)
           (intern (concat ":" (string-remove-prefix ":" (format "%s" key)))))
          (t key))
       (if (keywordp key) (substring (symbol-name key) 1) key)))
    ((my/language-server--plist-like-p mapping)
     (if (keywordp key)
         key
       (intern (concat ":" (string-remove-prefix ":" (format "%s" key))))))
    (t key))))

(defun my/language-server--mapping-put (mapping key value)
  "Return MAPPING with KEY set to VALUE, preserving its container kind."
  (let ((key (my/language-server--mapping-key mapping key)))
    (cond
     ((hash-table-p mapping)
      (puthash key value mapping)
      mapping)
     ((my/language-server--plist-like-p mapping)
      (plist-put mapping key value))
     (t
      (append (assoc-delete-all key mapping) (list (cons key value)))))))

(defun my/language-server--mapping-entries (mapping)
  "Return MAPPING as a list of key/value cons cells."
  (cond
   ((hash-table-p mapping)
    (let (entries)
      (maphash (lambda (key value) (push (cons key value) entries)) mapping)
      (nreverse entries)))
   ((my/language-server--plist-like-p mapping)
    (let ((rest mapping)
          entries)
      (while rest
        (push (cons (pop rest) (pop rest)) entries))
      (nreverse entries)))
   (t mapping)))

(defun my/language-server--merge-values (base override)
  "Deep-merge configuration OVERRIDE into BASE.
Plists, alists and hash tables may be mixed.  An explicitly present nil or
`:json-false' in OVERRIDE replaces BASE rather than being treated as absent."
  (cond
   ((and (my/language-server--mapping-p base)
         (my/language-server--mapping-p override))
    (let ((result (my/language-server--copy-value base))
          (missing (make-symbol "missing")))
      (dolist (entry (my/language-server--mapping-entries override) result)
        (let* ((key (car entry))
               (replacement (cdr entry))
               (current (my/language-server--mapping-ref result key missing)))
          (setq result
                (my/language-server--mapping-put
                 result key
                 (if (eq current missing)
                     (my/language-server--copy-value replacement)
                   (my/language-server--merge-values current replacement))))))))
   (t (my/language-server--copy-value override))))

(defun my/language-server--configuration-section (configuration section)
  "Return (PRESENT . VALUE) for dotted SECTION in CONFIGURATION."
  (let ((value configuration)
        (missing (make-symbol "missing"))
        (present t))
    (dolist (key (split-string section "\\." t))
      (let ((next (and present
                       (my/language-server--mapping-ref value key missing))))
        (if (eq next missing)
            (setq present nil)
          (setq value next))))
    (cons present value)))

(defun my/language-server-project-backend-override ()
  "Return the project-local backend override for the current buffer.

`lsp-mode' is the only client, so the sole meaningful override is
`disabled'.  The historical `lsp' / `lsp-mode' spellings stay accepted so
existing project configuration keeps loading without a warning."
  (when (fboundp 'my/project-local-value)
    (pcase (my/project-local-value :language-server)
      ((or 'lsp 'lsp-mode) 'lsp-mode)
      ('disabled 'disabled)
      (_ nil))))

(defun my/language-server-preferred-backend ()
  "Return the preferred backend for the current buffer."
  (if (and my/language-server-disabled-modes
           (apply #'derived-mode-p my/language-server-disabled-modes))
      'disabled
    (or (my/language-server-project-backend-override)
        'lsp-mode)))

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
  "Return project-local workspace configuration overrides."
  (when (fboundp 'my/project-local-value)
    (my/project-local-value :lsp-workspace)))

(defun my/language-server-set-workspace-configuration (configuration)
  "Merge CONFIGURATION into this buffer's language-server workspace settings.

The merged plist is stored buffer-locally and pushed to the server by
`my/language-server--push-workspace-configuration-h', so a project-local
override and a toolchain profile compose instead of overwriting one
another.  `lsp-mode' keeps its own registered settings in the global
`lsp-client-settings'; this is the per-buffer layer above it."
  (setq-local my/language-server--workspace-configuration
              (my/language-server--merge-values
               my/language-server--workspace-configuration
               configuration)))

(defun my/language-server--push-workspace-configuration-h ()
  "Send this buffer's workspace-configuration override to its server."
  (when-let* ((configuration my/language-server--workspace-configuration))
    (when (fboundp 'lsp--set-configuration)
      (lsp--set-configuration configuration))))

(defun my/language-server--workspace-configuration-override (workspace)
  "Return the workspace-configuration override owned by WORKSPACE.
The value is read from a live buffer that WORKSPACE manages, never from
whichever buffer happens to be current when the server asks.  A warm runtime
workspace may temporarily have no buffers, so its runtime-keyed registry is
the authoritative fallback in that interval."
  (or
   (when (fboundp 'lsp--workspace-buffers)
     (catch 'found
       (dolist (buffer (lsp--workspace-buffers workspace))
         (when (buffer-live-p buffer)
           (when-let* ((configuration
                        (buffer-local-value
                         'my/language-server--workspace-configuration buffer)))
             (throw 'found configuration))))))
   (and (fboundp 'my/language-server-runtime-configuration-for-workspace)
        (my/language-server-runtime-configuration-for-workspace workspace))))

(defun my/language-server-apply-lsp-local-settings ()
  "Apply project-local `lsp-mode' settings before startup."
  (when-let* ((configuration (my/language-server-project-workspace-configuration)))
    (my/language-server-set-workspace-configuration configuration))
  (when (fboundp 'my/language-server-toolchain-apply-lsp-settings)
    (my/language-server-toolchain-apply-lsp-settings))
  ;; Advertise watcher support only when the selected placement can implement
  ;; it without turning each directory into a remote process/connection.
  (setq-local
   lsp-enable-file-watchers
   (not
    (my/language-server--skip-file-watch-p
     (my/language-server--project-root-for-buffer))))
  (run-hooks 'my/language-server-lsp-local-settings-hook))

(defun my/language-server-contact-available-p ()
  "Return non-nil when this buffer has a usable lsp-mode client.
The executable probe runs inside the same Remote workspace/adapter extent as
the eventual process start, so a target binary is never confused with a
client-side installation."
  (and (require 'lsp-mode nil t)
       (fboundp 'lsp--filter-clients)
       (let* ((root (my/language-server--project-root-for-buffer))
              (workspace (my/language-server--connect-workspace root))
              (remote-current-adapter-id "language-server")
              (remote-current-workspace workspace))
         (remote-environment-ensure
          (and workspace (remote-workspace-context workspace)))
         (ignore-errors
           (lsp--filter-clients
            (lambda (client)
              (and (lsp--supports-buffer? client)
                   (lsp--server-binary-present? client))))))))

(defun my/language-server-program-entries ()
  "Return locally registered language servers in registration order."
  (nreverse (copy-sequence my/language-server-program-metadata)))

(defun my/language-server-managed-p ()
  "Return non-nil when the current buffer is managed by a language server."
  (bound-and-true-p lsp-managed-mode))

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

(add-hook 'lsp-managed-mode-hook #'my/language-server-performance-sync-h)
(add-hook 'kill-buffer-hook #'my/language-server-performance--leave-buffer)
(add-hook 'change-major-mode-hook #'my/language-server-performance--leave-buffer)

(cl-defun my/register-language-server
    (modes program &key server-id feature executables placement label source
           note priority multi-root activation-fn initialization-options
           notification-handlers request-handlers server-id-suffix)
  "Register a language server for MODES and record maintenance metadata.

MODES is a major mode or list of major modes.  PROGRAM is the server
argv, a function returning one, or an `lsp-mode' connection object.

PLACEMENT defaults to `target\=': the command is resolved from the active
target/workspace environment and launched through the official process
API.  Use `client\=' only for an explicitly client-side UI helper.

FEATURE, when non-nil, must be loadable before the server may start.
EXECUTABLES, LABEL, SOURCE, and NOTE feed the Hub and Doctor.  The
remaining keys are passed through to `make-lsp-client\='.

This is the only supported way to declare a server; never call
`lsp-register-client\=' or push onto client lists directly."
  (let* ((modes (if (listp modes) modes (list modes)))
         (server-id (or server-id
                        (intern (format "my-%s" (car modes)))))
         (activation-fn
          (and activation-fn
               (lambda (&rest arguments)
                 ;; In lsp-mode an activation function replaces the ordinary
                 ;; major-mode check.  Keep the declared MODES authoritative
                 ;; so availability predicates cannot activate this client in
                 ;; every programming buffer.
                 (and (apply #'derived-mode-p modes)
                      (apply activation-fn arguments)))))
         (connection
          (cond
           ((functionp program) (lsp-stdio-connection program))
           ((and (listp program) (stringp (car program)))
            (lsp-stdio-connection program))
           (t program))))
    (dolist (mode modes)
      (when feature
        (setf (alist-get mode my/lsp-mode-required-features nil t #'eq)
              feature)))
    (apply
     #'lsp-register-client
     (list
      (apply
       #'make-lsp-client
       (append
        (list :new-connection connection
              :major-modes modes
              :server-id server-id
              :priority (or priority 0))
        (when multi-root (list :multi-root multi-root))
        (when activation-fn (list :activation-fn activation-fn))
        (when initialization-options
          (list :initialization-options initialization-options))
        (when notification-handlers
          (list :notification-handlers notification-handlers))
        (when request-handlers (list :request-handlers request-handlers))))))
    (ignore server-id-suffix)
    (setq my/language-server-program-metadata
          (cons (list :modes modes
                      :program program
                      :server-id server-id
                      :feature feature
                      :executables executables
                      :placement (or placement 'target)
                      :label label
                      :source (my/language-server--resolve-source source)
                      :note note)
                (cl-remove-if
                 (lambda (entry)
                   (eq (plist-get entry :server-id) server-id))
                 my/language-server-program-metadata)))
    server-id))

(cl-defun my/register-language-server-feature
    (modes feature &key executables placement label source note)
  "Record a language server that an external package registers for MODES.

Some servers are registered by their own package (`lsp-java', `lean4-mode')
rather than by `my/register-language-server'.  This declares the same
maintenance metadata for them and, more importantly, records FEATURE as a
hard prerequisite so `my/lsp-mode-supported-p' refuses to start a server
whose support library is missing instead of failing inside lsp-mode."
  (let ((modes (if (listp modes) modes (list modes))))
    (dolist (mode modes)
      (setf (alist-get mode my/lsp-mode-required-features nil t #'eq) feature))
    (setq my/language-server-program-metadata
          (cons (list :modes modes
                      :program feature
                      :server-id feature
                      :feature feature
                      :executables executables
                      :placement (or placement 'target)
                      :label (or label (format "%s" feature))
                      :source (my/language-server--resolve-source source)
                      :note note)
                (cl-remove-if
                 (lambda (entry)
                   (eq (plist-get entry :server-id) feature))
                 my/language-server-program-metadata)))
    feature))

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
  "Return the active language server backend for the current buffer.
`lsp-mode' is the only client; the value stays a symbol so consumer
modules can keep dispatching on it."
  (and (bound-and-true-p lsp-managed-mode) 'lsp-mode))

(defun my/lsp-mode-start-now ()
  "Start lsp-mode after the target environment is ready."
  (let ((report-missing my/language-server--manual-start))
    (setq my/language-server--manual-start nil)
    (if (my/lsp-mode-supported-p)
        (progn
          (my/language-server-apply-process-environment)
          (my/language-server-apply-lsp-local-settings)
          (my/language-server-runtime-register-lsp-configuration)
          (if (my/language-server-contact-available-p)
              (lsp-deferred)
            (when report-missing
              (message
               "No installed language server supports %s on this target; run %s"
               major-mode "M-x my/language-server-doctor"))))
      (let ((feature (my/lsp-mode-required-feature)))
        (when report-missing
          (message "Skip lsp-mode in %s: missing `%s'" major-mode feature))))))

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
      (let ((state
             (and
              (fboundp 'my/direnv-update-environment-maybe)
              (my/direnv-update-environment-maybe
               nil #'my/lsp-mode--direnv-ready))))
        (if (eq state 'pending)
            (setq my/lsp-mode--waiting-for-direnv t)
          (my/lsp-mode-start-now))))))

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

(defun my/language-server--booster-command (command)
  "Return COMMAND wrapped in `emacs-lsp-booster' for the active target.

The booster is looked up with `my/language-server-executable-find', so the
path returned is target-native and a remote server can never end up running
the client's copy.  A target without the binary fails loudly while
`my/language-server-booster-required' is non-nil rather than silently
dropping the optimisation on that one machine."
  (let ((booster (my/language-server-executable-find "emacs-lsp-booster")))
    (cond
     (booster
      (append (list booster "--json-false-value" ":json-false" "--") command))
     (my/language-server-booster-required
      (error
       "emacs-lsp-booster not found on target %s; install it there or set %s"
       (or (ignore-errors (remote-context)) "local")
       "my/language-server-booster-required to nil"))
     (t command))))

(defun my/lsp-mode--resolve-logical-command-a (fn command &optional test)
  "Resolve lsp-mode COMMAND without its TRAMP tty shell wrapper.
The `/fs:' process boundary already selects a pipe and projects the command
through the chosen backend.  Calling FN in test mode performs the same command
normalization while deliberately skipping `stty raw' and `shell-file-name'.
The resolved argv is then wrapped in `emacs-lsp-booster' on the same target
that will run the server."
  (let ((resolved
         (funcall
          fn command
          (or test (remote-fs-file-name-p default-directory)))))
    (if (and (listp resolved) (stringp (car-safe resolved)))
        (my/language-server--booster-command resolved)
      resolved)))

(defun my/lsp-mode--supports-logical-buffer-a (fn client)
  "Let one CLIENT definition support native, TRAMP and logical buffers.
lsp-mode normally requires a separate `-tramp' clone whose `remote?' slot is
true.  The Remote framework already owns placement, command resolution and
process launch, so this compatibility boundary temporarily reflects either a
physical TRAMP buffer or a logical `/fs:' buffer's remote truth value while FN
checks support.  The original client slot is always restored."
  (let* ((path (or (buffer-file-name) default-directory))
         (routed
          (and (stringp path)
               (or (remote-fs-file-name-p path)
                   (file-remote-p path))))
         (original (lsp--client-remote? client)))
    (if (not routed)
        (funcall fn client)
      (unwind-protect
          (progn
            (my/language-server--set-struct-slot
             client 'lsp--client 'remote? (and (file-remote-p path) t))
            (funcall fn client))
        (my/language-server--set-struct-slot
         client 'lsp--client 'remote? original)))))

(defun my/lsp-mode--stdio-connect-via-remote-a (connection)
  "Route lsp-mode stdio CONNECTION through its owning Remote workspace.
This is the lsp-mode equivalent of the former Eglot Remote contact boundary.
It deliberately handles ordinary `/ssh:' and `/rpc:' visiting buffers as well
as logical `/fs:' buffers: lsp-mode may keep the ordinary visiting-file
spelling, while process placement, cwd, environment and executable lookup use
the canonical logical workspace root.

The original lsp-mode connect function still owns JSON-RPC filters, sentinels,
stderr buffers and process bookkeeping.  Binding its default directory to the
logical root makes its official `make-process :file-handler t' call enter
`remote-make-process', so no private lsp-mode process implementation is
duplicated here."
  (if-let* ((connect (plist-get connection :connect)))
      (plist-put
       connection :connect
       (lambda (filter sentinel name environment-fn lsp-workspace)
         (let* ((root
                 (my/language-server--canonical-root
                  (or (ignore-errors
                        (lsp--workspace-root lsp-workspace))
                      default-directory)))
                (owner (my/language-server--connect-workspace root))
                (remote-current-adapter-id "language-server")
                (remote-current-workspace owner)
                ;; lsp-mode otherwise replaces this logical directory with
                ;; its physical TRAMP session-folder spelling immediately
                ;; before make-process, bypassing the Remote file handler.
                (lsp-use-workspace-root-for-server-default-directory nil)
                (default-directory (or root default-directory)))
           (remote-environment-ensure
            (and owner (remote-workspace-context owner)))
           (funcall connect filter sentinel name environment-fn lsp-workspace))))
    connection))

(defun my/language-server--booster-json-parse-a (fn &rest args)
  "Read an `emacs-lsp-booster' bytecode payload in place of JSON.
The booster emits pre-parsed Emacs bytecode; everything else keeps taking
FN's ordinary JSON path with ARGS."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply fn args)))

(defun my/language-server--executable-find-a (fn command &optional remote)
  "Resolve COMMAND on the language server's target rather than the client.

Most stock `lsp-clients-*' definitions probe for their binary with a bare
`executable-find', which answers for the machine running Emacs.  While a
language-server route is being resolved, delegate to the target-aware
lookup so those ~100 built-in clients become remote-correct without each
having to be re-registered.  Outside that dynamic extent FN keeps its
ordinary meaning."
  (if (and (not my/language-server--resolving-executable-p)
           (equal remote-current-adapter-id "language-server")
           (stringp command))
      (let ((my/language-server--resolving-executable-p t))
        (ignore-errors (remote-executable-find command)))
    (funcall fn command remote)))

(defun my/language-server--install-server-a (fn client &rest args)
  "Refuse a client-side server install for a target-placed CLIENT.

`lsp-download-install' unpacks into `lsp-server-install-dir' on the machine
running Emacs.  For a workspace whose filesystem the client cannot reach,
that installs the server on the wrong side and the resulting path is
meaningless to the target.  Provisioning for those targets goes through the
workspace service instead, so fail loudly rather than silently installing
here.  ARGS are passed through untouched when the install is legitimate."
  (let ((root (my/language-server--project-root-for-buffer)))
    (if (and root (not (ignore-errors (remote-client-file-name root))))
        (user-error
         "Refusing to install a language server on the client for %s; %s"
         root "provision it on the target instead")
      (apply fn client args))))

(defun my/language-server--tcp-connection-a (fn &rest args)
  "Route an lsp-mode TCP connection through the Remote channel layer.
Binding the adapter activates the `open-network-stream' and
`make-network-process' advices below, so a target-owned server is reached
through a workspace-owned forward instead of a socket opened on this
machine.  A target that cannot provide a channel raises rather than
silently connecting to whatever listens on that port here."
  (let ((remote-current-adapter-id "language-server"))
    (apply fn args)))

(defun my/language-server--workspace-configuration-response-a (fn params)
  "Answer a `workspace/configuration' request from the owning workspace.
The per-buffer override is read from a buffer that `lsp--cur-workspace'
actually manages, so a callback running in an unrelated buffer cannot
change which target's configuration the server receives."
  (let ((response (funcall fn params)))
    (if-let* ((workspace lsp--cur-workspace)
              (override
               (my/language-server--workspace-configuration-override workspace)))
        (let* ((items (append (lsp-get params :items) nil))
               (merged (copy-sequence response))
               (limit (min (length items) (length merged))))
          (dotimes (index limit merged)
            (let* ((section (lsp-get (nth index items) :section))
                   (selection
                    (if (and (stringp section) (not (string-empty-p section)))
                        (my/language-server--configuration-section
                         override section)
                      (cons t override))))
              (when (car selection)
                (aset merged index
                      (my/language-server--merge-values
                       (aref response index) (cdr selection)))))))
      response)))

(defun my/language-server--hide-eglot-commands ()
  "Keep every Eglot command out of `M-x'.
Eglot ships with Emacs and cannot be uninstalled, but it is no longer a
supported route here; surfacing its commands only invites starting a second
client that the Remote contract does not manage."
  (mapatoms
   (lambda (symbol)
     (when (and (commandp symbol)
                (string-prefix-p "eglot" (symbol-name symbol)))
       (put symbol 'completion-predicate #'ignore)))))

(my/language-server--hide-eglot-commands)
(with-eval-after-load 'eglot (my/language-server--hide-eglot-commands))

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


(defun my/lsp-mode--fix-path-casing-a (fn path)
  "Canonicalize PATH into the shared `/fs:' diagnostic-key namespace.
`lsp--fix-path-casing' is the single choke point every lsp-mode
diagnostics store and lookup passes through (`lsp-mode.el' and
`lsp-diagnostics.el').  The stored key already carries a logical `/fs:'
identity via the advised `lsp--uri-to-path'; the lookup side calls this
function with the buffer's native `buffer-file-name'.  Without
normalizing both sides here they never match, and Flymake silently
reports zero diagnostics for locally opened buffers."
  (let ((path (funcall fn path)))
    (if (stringp path)
        (or (ignore-errors (remote-canonicalize-file-name path)) path)
      path)))

(defun my/language-server--doctor-check (_target _probe)
  "Return Remote Doctor diagnostics for the language-server identity fixes.
lsp-mode diagnostics reach a buffer only if that buffer's
`buffer-file-name' resolves to the same key the server's diagnostics were
stored under; `my/lsp-mode--fix-path-casing-a' is what bridges the `/fs:'
logical identity and the buffer's native spelling.  If it is not installed
\(for example after an lsp-mode update renames the function it targets\),
Flymake silently stops showing diagnostics for normal local buffers with no
other symptom, which is exactly the defect this check exists to catch early.

The booster check is here for the same reason: a target missing
`emacs-lsp-booster' is a startup failure that is far cheaper to see in
Doctor than in a server log."
  (list
   (list :name 'language-server-lsp-mode-diagnostic-key-advice
         :status
         (cond
          ((not (fboundp 'lsp--fix-path-casing)) 'ok)
          ((advice-member-p #'my/lsp-mode--fix-path-casing-a
                            'lsp--fix-path-casing)
           'ok)
          (t 'warning))
         :detail
         (if (fboundp 'lsp--fix-path-casing)
             "lsp-mode diagnostics key normalization to /fs:"
           "lsp-mode not loaded yet")
         :remedy
         "Re-check my/lsp-mode--fix-path-casing-a against lsp--fix-path-casing")
   (list :name 'language-server-booster
         :status
         (cond
          ((my/language-server-executable-find "emacs-lsp-booster") 'ok)
          (my/language-server-booster-required 'error)
          (t 'warning))
         :detail "emacs-lsp-booster on the language-server target"
         :remedy
         "Install emacs-lsp-booster on this target, or unset my/language-server-booster-required")))

(remote-doctor-register-check #'my/language-server--doctor-check)

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

(defun my/language-server--skip-file-watch-p (root)
  "Return non-nil when an LSP client must decline dynamic watches for ROOT.
An `auto' watcher is accepted when either the client can access ROOT directly
or the selected Remote route advertises `watch'.  This keeps feature parity
without bypassing the Remote workspace/resource lifecycle."
  (pcase my/language-server-file-watch-policy
    ('disabled t)
    ('native nil)
    ('auto
     (and
      root
      (not
       (or
        (ignore-errors (remote-client-file-name root))
        (ignore-errors
          (remote-routes
           "language-server" 'watch (remote-context root) nil))))))
    (_
     (error
      "Invalid `my/language-server-file-watch-policy': %S"
      my/language-server-file-watch-policy))))

(defun my/lsp-mode--watch-root-via-remote-a
    (fn directory callback ignored-files ignored-directories
        &optional watch warn-big-repo-p)
  "Create lsp-mode watches for DIRECTORY through Remote when target-owned.
The lsp-mode session may retain an ordinary `/ssh:' workspace spelling, while
the watch itself uses canonical `/fs:' paths.  Its public descriptor is then
owned by the same recoverable Remote workspace as the language server."
  (let* ((logical (my/language-server--canonical-root directory))
         (target (and logical (remote-file-name-target logical))))
    (if (or (null logical) (equal target "local"))
        (funcall fn directory callback ignored-files ignored-directories
                 watch warn-big-repo-p)
      (let* ((owner (my/language-server--connect-workspace logical))
             (remote-current-adapter-id "language-server")
             (remote-current-workspace owner)
             (remote-file-watch-workspace owner)
             (remote-file-watch-metadata
              (list :owner 'lsp-mode :root logical)))
        (funcall fn logical callback ignored-files ignored-directories
                 watch warn-big-repo-p)))))

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

(defun my/lsp-mode--remote-shutdown-timeout-a
    (function method params &rest keys)
  "Allow target-owned shutdown METHOD enough time across the Remote route."
  (let* ((root
          (and lsp--cur-workspace
               (my/language-server--lsp-workspace-root lsp--cur-workspace)))
         (target (and root (remote-file-name-target root))))
    (if (and (equal method "shutdown")
             target
             (not (equal target "local")))
        (let ((lsp-response-timeout
               (max (or lsp-response-timeout 0)
                    my/lsp-mode-remote-shutdown-response-timeout)))
          (apply function method params keys))
      (apply function method params keys))))

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

(defun my/lsp-mode--effective-startup-timeout (workspace)
  "Return the startup timeout to apply to WORKSPACE."
  (or (alist-get (my/language-server--lsp-workspace-id workspace)
                 my/lsp-mode-startup-timeout-overrides)
      my/lsp-mode-startup-timeout))

(defun my/lsp-mode--arm-startup-watchdog ()
  "Arm a bounded startup watchdog for `lsp--cur-workspace'."
  (when lsp--cur-workspace
    (my/lsp-mode--cancel-startup-watchdog lsp--cur-workspace)
    (let* ((workspace lsp--cur-workspace)
           (timeout (my/lsp-mode--effective-startup-timeout workspace)))
      (when (and (numberp timeout) (> timeout 0))
        (puthash
         workspace
         (run-at-time
          timeout nil
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
               :timeout timeout)
              (message
               "LSP startup timed out after %.1fs: %s"
               timeout
               (or (my/language-server--lsp-workspace-root value)
                   "unknown workspace"))
              (my/lsp-mode-shutdown-workspace value 'startup-timeout)))
          workspace)
         my/lsp-mode--startup-timers)))))

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
         (limit
          (or (alist-get (car key) my/lsp-mode-restart-limit-overrides)
              my/lsp-mode-restart-limit))
         (now (float-time))
         (history
          (seq-filter
           (lambda (timestamp)
             (< (- now timestamp) my/lsp-mode-restart-window))
           (gethash key my/lsp-mode--restart-history))))
    (if (>= (length history) (max 0 limit))
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
     (append
      (list 'lsp-mode
            (my/language-server--lsp-workspace-id lsp-workspace)
            root)
      ;; Keep the legacy three-part identity for ordinary workspaces.  A
      ;; runtime ID is the client-side fourth component which lets Remote own
      ;; several kernel-specific server processes without changing the one
      ;; real logical root shared by process placement and protocol URIs.
      (when-let* ((runtime-id
                   (and
                    (fboundp 'my/language-server-runtime-workspace-id)
                    (my/language-server-runtime-workspace-id lsp-workspace))))
        (list runtime-id)))
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

(defun my/language-server-ensure-deferred ()
  "Start the selected language-server client after the buffer opens."
  (when (and buffer-file-name
             (not (derived-mode-p 'lean-mode)))
    (setq-local my/language-server--manual-start nil)
    (let ((buffer (current-buffer)))
      (run-at-time
       0 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (my/language-server-ensure))))
       buffer))))

(add-hook 'prog-mode-hook #'my/language-server-ensure-deferred)

(defun my/language-server--ensure-after-runtime ()
  "Start the preferred backend after runtime preparation has completed."
  (interactive)
  (pcase (my/language-server-preferred-backend)
    ('lsp-mode (my/lsp-mode-ensure))
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
  (when (called-interactively-p 'interactive)
    (setq my/language-server--manual-start t))
  (unless my/language-server--waiting-for-runtime
    (let ((state
           (my/language-server-runtime-prepare
            #'my/language-server--runtime-ready)))
      (when (eq state 'pending)
        (setq my/language-server--waiting-for-runtime t)))))

(defun my/language-server-call (command)
  "Call COMMAND when a language server manages the current buffer.
The indirection is kept so every consumer keeps one stable entry point and
one uniform error when nothing is running."
  (if (my/current-language-server-backend)
      (call-interactively command)
    (user-error "No active language server in current buffer")))

(defun my/language-server-code-actions ()
  "Run a code action using the active language server."
  (interactive)
  (my/language-server-call #'lsp-execute-code-action))

(defun my/language-server-format-buffer ()
  "Format the current buffer using the active language server."
  (interactive)
  (my/language-server-call #'lsp-format-buffer))

(defun my/language-server-rename ()
  "Rename the symbol at point using the active language server."
  (interactive)
  (my/language-server-call #'lsp-rename))

(defun my/language-server-find-implementation ()
  "Find implementation using the active language server."
  (interactive)
  (my/language-server-call #'lsp-find-implementation))

(defun my/language-server-find-type-definition ()
  "Find type definition using the active language server."
  (interactive)
  (my/language-server-call #'lsp-find-type-definition))

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
  :hook ((lsp-managed-mode . company-mode)
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
render the same CAPF/lsp-mode candidates on top of each other."
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
;; lsp-mode 诊断统一走 Flymake（`lsp-diagnostics-provider' :flymake）
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . my/prog-flymake-setup)
  :custom
  ;; Emacs renders these summaries for every diagnostic line in the visible
  ;; window.  Unlike the old sideline backend, display is not tied to point.
  (flymake-show-diagnostics-at-end-of-line 'short)
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
(defun my/lsp-imenu--symbol-label (symbol)
  "Return SYMBOL's Imenu label while retaining its LSP SymbolKind."
  (let* ((kind (lsp-get symbol :kind))
         (label (copy-sequence
                 (lsp-render-symbol symbol lsp-imenu-detailed-outline))))
    (when (> (length label) 0)
      (add-text-properties
       0 (length label)
       (list 'my/lsp-symbol-kind kind)
       label))
    label))

(defun my/lsp-imenu--document-symbol-entry (symbol)
  "Convert one LSP document SYMBOL to a typed hierarchical Imenu entry."
  (let* ((children
          (lsp--imenu-filter-symbols (or (lsp-get symbol :children) nil)))
         (children (seq-sort #'lsp--imenu-symbol-lessp children))
         (label (my/lsp-imenu--symbol-label symbol)))
    (if children
        (cons label
              (mapcar #'my/lsp-imenu--document-symbol-entry children))
      (cons label
            (gethash (lsp--get-line-and-col symbol)
                     lsp--line-col-to-point-hash-table)))))

(defconst my/lsp-tab-line-header-entry
  '(t (:eval (window-parameter nil 'lsp-headerline--string)))
  "lsp-mode breadcrumb renderer relocated from header-line to tab-line.")

(defconst my/breadcrumb-header-line-entries
  '((:eval (breadcrumb--header-line)) (:eval (bc--header-line)))
  "Known breadcrumb package renderers relocated out of header-line.")

(defun my/breadcrumb-view-only-string (breadcrumb)
  "Return BREADCRUMB without mouse, link or keymap behavior."
  (when (and (stringp breadcrumb) (not (string-empty-p breadcrumb)))
    (let ((result (copy-sequence breadcrumb)))
      (remove-list-of-text-properties
       0 (length result)
       '(local-map keymap mouse-face help-echo follow-link pointer)
       result)
      result)))

(defun my/breadcrumb-tab-line-action-string (breadcrumb)
  "Return sanitized BREADCRUMB with the one repository-owned click action."
  (when-let* ((result (my/breadcrumb-view-only-string breadcrumb)))
    (add-text-properties
     0 (length result)
     '(my/tab-line-context-action my/show-imenu-from-breadcrumb
       my/tab-line-context-help
       "mouse-1: show current symbol in Treemacs")
     result)
    result))

(defun my/lsp-tab-line-breadcrumb ()
  "Return the current window's sanitized lsp-mode breadcrumb action."
  (my/breadcrumb-tab-line-action-string
   (window-parameter nil 'lsp-headerline--string)))

(defun my/breadcrumb-tab-line-content ()
  "Return breadcrumb.el's sanitized project/imenu action for the tab line."
  (when (bound-and-true-p breadcrumb-local-mode)
    (my/breadcrumb-tab-line-action-string
     (cond
      ((fboundp 'breadcrumb--header-line)
       (breadcrumb--header-line))
      ((fboundp 'bc--header-line)
       (bc--header-line))))))

(defun my/breadcrumb-tab-line-sync-h ()
  "Keep breadcrumb.el in the tab-line from its very first buffer frame."
  (when (listp header-line-format)
    (dolist (entry my/breadcrumb-header-line-entries)
      (setq-local header-line-format (delete entry header-line-format))))
  (if (bound-and-true-p breadcrumb-local-mode)
      (add-hook 'my/tab-line-leading-segment-functions
                #'my/breadcrumb-tab-line-content nil t)
    (remove-hook 'my/tab-line-leading-segment-functions
                 #'my/breadcrumb-tab-line-content t))
  (if (fboundp 'my/tab-line-refresh)
      (my/tab-line-refresh)
    (force-mode-line-update t)))

(defun my/breadcrumb-local-mode-a (fn &rest arguments)
  "Route breadcrumb.el through tab-line before calling FN.
The package's header mutation is confined to a throwaway dynamic binding, so
opening a file can never expose one frame in the former location."
  (let (result)
    (let ((header-line-format
           (cond
            ((listp header-line-format)
             (seq-remove
              (lambda (entry)
                (member entry my/breadcrumb-header-line-entries))
              header-line-format))
            ((null header-line-format) nil)
            (t (list header-line-format)))))
      (setq result (apply fn arguments)))
    (my/breadcrumb-tab-line-sync-h)
    result))

(defun my/lsp-headerline-breadcrumb-mode-a (fn &rest arguments)
  "Route lsp-headerline through the tab-line before calling FN.
Upstream mutates `header-line-format' inside its breadcrumb minor mode.  Run
that bookkeeping against a dynamic throwaway value, then synchronize the
real tab-line provider before redisplay can expose the upstream location."
  (let (result)
    (let ((header-line-format
           (if (listp header-line-format)
               (remove my/lsp-tab-line-header-entry header-line-format)
             header-line-format)))
      (setq result (apply fn arguments)))
    (my/lsp-tab-line-sync-h)
    result))

(defun my/lsp-tab-line-sync-h ()
  "Give LSP the non-tab contextual underlay in managed buffers."
  ;; This also cleans buffers configured before the relocation advice became
  ;; active, and makes disable/unconfigure idempotent.
  (when (listp header-line-format)
    (setq-local header-line-format
                (remove my/lsp-tab-line-header-entry header-line-format)))
  (if (bound-and-true-p lsp-managed-mode)
      ;; Keep lsp-headerline's mode and idle refresh alive; only its renderer
      ;; moves and loses interaction properties.  Application headers
      ;; (notably Noema's) remain the sole owners of their separate line.
      (add-hook 'my/tab-line-leading-segment-functions
                #'my/lsp-tab-line-breadcrumb nil t)
    (remove-hook 'my/tab-line-leading-segment-functions
                 #'my/lsp-tab-line-breadcrumb t))
  (if (fboundp 'my/tab-line-refresh)
      (my/tab-line-refresh)
    (force-mode-line-update t)))

(defun my/lsp-tab-line-apply-ui ()
  "Apply high-contrast repository colors to lsp-mode breadcrumbs."
  (aaron-ui-set-face 'lsp-headerline-breadcrumb-separator-face
                     :inherit nil :foreground 'fg-muted :height 0.9)
  (aaron-ui-set-face 'lsp-headerline-breadcrumb-project-prefix-face
                     :inherit nil :foreground 'fg-soft :weight 'semibold)
  (aaron-ui-set-face 'lsp-headerline-breadcrumb-unknown-project-prefix-face
                     :inherit nil :foreground 'fg-muted :weight 'semibold)
  (aaron-ui-set-face 'lsp-headerline-breadcrumb-path-face
                     :inherit nil :foreground 'fg-dim)
  (aaron-ui-set-face 'lsp-headerline-breadcrumb-symbols-face
                     :inherit nil :foreground 'fg-soft :weight 'semibold))

(defun my/breadcrumb-tab-line-apply-ui ()
  "Apply the same readable palette to the pre-LSP breadcrumb provider."
  (aaron-ui-set-face 'breadcrumb-face
                     :inherit nil :foreground 'fg-muted)
  (aaron-ui-set-face 'breadcrumb-project-crumbs-face
                     :inherit nil :foreground 'fg-dim)
  (aaron-ui-set-face 'breadcrumb-project-base-face
                     :inherit nil :foreground 'fg-soft :weight 'semibold)
  (aaron-ui-set-face 'breadcrumb-project-leaf-face
                     :inherit nil :foreground 'fg-dim)
  (aaron-ui-set-face 'breadcrumb-imenu-crumbs-face
                     :inherit nil :foreground 'fg-dim)
  (aaron-ui-set-face 'breadcrumb-imenu-leaf-face
                     :inherit nil :foreground 'fg-soft :weight 'semibold))

(defun my/lsp-imenu-create-vscode-index (symbols)
  "Build a VS Code-style typed and hierarchically sorted Imenu index.
DocumentSymbol servers retain their exact SymbolKind as a text property for
Treemacs and other visual consumers.  Older flat SymbolInformation servers use
lsp-mode's categorized index, whose category names provide the same fallback."
  (if (and symbols (lsp--imenu-hierarchical-p symbols))
      (let* ((symbols (seq-sort #'lsp--imenu-symbol-lessp symbols))
             (lsp--line-col-to-point-hash-table
              (lsp--convert-line-col-to-points-batch
               (lsp--collect-lines-and-cols symbols))))
        (mapcar #'my/lsp-imenu--document-symbol-entry symbols))
    (lsp-imenu-create-categorized-index symbols)))

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
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(project file symbols)
        lsp-inlay-hint-enable t
        ;; The normal idle hook refreshes the visible range after 350ms.  Do
        ;; not also send an immediate request for every intermediate scroll.
        lsp-update-inlay-hints-on-scroll nil
        ;; lsp-mode owns CodeLens directly.  It is enabled by default and can
        ;; be toggled without opening the maintenance dispatch via SPC c L.
        lsp-lens-enable t
        lsp-idle-delay 0.35
        lsp-lens-debounce-interval 0.35
        lsp-lens-place-position 'end-of-line
        ;; Match VS Code Outline's type-first view, but keep members of the
        ;; same kind in source order so the tree remains predictable while
        ;; editing.  `my/lsp-imenu-create-vscode-index' retains SymbolKind for
        ;; semantic icons in Treemacs.
        lsp-imenu-sort-methods '(kind position name)
        lsp-imenu-index-function #'my/lsp-imenu-create-vscode-index
        lsp-log-io nil
        lsp-keep-workspace-alive nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-render-documentation nil
        lsp-eldoc-enable-hover nil
        lsp-enable-suggest-server-download nil
        ;; The `/fs:' handler plus `remote-make-process' is the only process
        ;; path.  lsp-mode's own `-tramp' client clones would add a second,
        ;; parallel remote implementation whose command, environment and
        ;; workspace identity are outside the Remote contract.
        lsp-auto-register-remote-clients nil)
  :config
  (add-hook
   'lsp-after-initialize-hook
   #'my/language-server-register-lsp-resource)
  (add-hook 'lsp-configure-hook
            #'my/language-server--push-workspace-configuration-h)
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
  (define-key lsp-mode-map (kbd "C-c d") #'lsp-ui-doc-glance)
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
           #'my/lsp-mode--supports-logical-buffer-a
           'lsp--supports-buffer?)
    (advice-add
     'lsp--supports-buffer?
     :around #'my/lsp-mode--supports-logical-buffer-a))
  (unless (advice-member-p
           #'my/lsp-mode--stdio-connect-via-remote-a
           'lsp-stdio-connection)
    (advice-add
     'lsp-stdio-connection
     :filter-return #'my/lsp-mode--stdio-connect-via-remote-a))
  (unless (advice-member-p
           #'my/lsp-mode--resolve-logical-command-a
           'lsp-resolve-final-command)
    (advice-add
     'lsp-resolve-final-command
     :around #'my/lsp-mode--resolve-logical-command-a))
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
           #'my/lsp-mode--fix-path-casing-a
           'lsp--fix-path-casing)
    (advice-add
     'lsp--fix-path-casing
     :around #'my/lsp-mode--fix-path-casing-a))
  (unless (advice-member-p
           #'my/lsp-mode--register-capability-via-remote-a
           'lsp--server-register-capability)
    (advice-add
     'lsp--server-register-capability
     :around #'my/lsp-mode--register-capability-via-remote-a))
  (unless (advice-member-p
           #'my/lsp-mode--watch-root-via-remote-a
           'lsp-watch-root-folder)
    (advice-add
     'lsp-watch-root-folder
     :around #'my/lsp-mode--watch-root-via-remote-a))
  (unless (advice-member-p
           #'my/lsp-mode--restart-with-circuit-breaker-a
           'lsp--restart-if-needed)
    (advice-add
     'lsp--restart-if-needed
     :around #'my/lsp-mode--restart-with-circuit-breaker-a))
  (unless (advice-member-p
           #'my/lsp-mode--remote-shutdown-timeout-a
           'lsp-request)
    (advice-add
     'lsp-request :around #'my/lsp-mode--remote-shutdown-timeout-a))
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
     :override #'my/lsp-mode-shutdown-all))
  (unless (advice-member-p
           #'my/language-server--executable-find-a
           'executable-find)
    (advice-add
     'executable-find
     :around #'my/language-server--executable-find-a))
  (when (fboundp 'lsp--install-server-internal)
    (unless (advice-member-p
             #'my/language-server--install-server-a
             'lsp--install-server-internal)
      (advice-add
       'lsp--install-server-internal
       :around #'my/language-server--install-server-a)))
  (dolist (connector '(lsp-tcp-connection lsp-tcp-server-connection))
    (when (and (fboundp connector)
               (not (advice-member-p
                     #'my/language-server--tcp-connection-a connector)))
      (advice-add connector :around #'my/language-server--tcp-connection-a)))
  (unless (advice-member-p
           #'my/language-server--workspace-configuration-response-a
           'lsp--build-workspace-configuration-response)
    (advice-add
     'lsp--build-workspace-configuration-response
     :around #'my/language-server--workspace-configuration-response-a))
  ;; emacs-lsp-booster answers with pre-parsed bytecode instead of JSON.
  (let ((reader (if (fboundp 'json-parse-buffer) 'json-parse-buffer 'json-read)))
    (unless (advice-member-p #'my/language-server--booster-json-parse-a reader)
      (advice-add reader :around #'my/language-server--booster-json-parse-a))))


;; -------------------------
;; 6. UI Emulation (Doc Box & Breadcrumb)
;; -------------------------

;; lsp-ui：悬浮文档框、sideline code action、peek 跳转
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; `lsp-ui-doc-show-with-cursor' reproduces the old
  ;; hover-at-point feel: the box follows point instead of
  ;; waiting for an explicit command.
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.4)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  ;; `lsp-ui-sideline' reads diagnostics only from flycheck
  ;; (`lsp-ui-sideline--diagnostics' is guarded on `flycheck-mode'), and this
  ;; configuration is Flymake-based.  The `sideline' package below owns that
  ;; column instead, so lsp-ui contributes doc, peek and imenu only.
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  :config
  (define-key lsp-ui-mode-map (kbd "C-h d") #'lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-h c") #'lsp-ui-doc-hide))

;; Code actions stay contextual to the current line.  Flymake itself owns
;; persistent end-of-line diagnostics for every visible diagnostic line.
(use-package sideline
  :ensure t
  :hook (lsp-managed-mode . sideline-mode)
  :custom
  (sideline-backends-right '((sideline-lsp . up)))
  (sideline-backends-left nil)
  (sideline-delay 0.35)
  (sideline-order-right 'up)
  (sideline-priority 100)
  (sideline-display-backend-name nil))

(use-package sideline-lsp
  :ensure t
  :after (sideline lsp-mode)
  :custom
  (sideline-lsp-update-mode 'line)
  (sideline-lsp-ignore-duplicate t)
  (sideline-lsp-code-actions-prefix "⚑ "))

;; Both renderers use the same non-tab contextual underlay.  `breadcrumb'
;; covers the period before LSP is ready and non-LSP buffers; managed buffers
;; replace it with lsp-mode's richer symbol path without changing position.
(use-package breadcrumb
  :ensure t
  :hook ((prog-mode . breadcrumb-local-mode)
         (org-src-mode . breadcrumb-local-mode)))

(defun my/language-server--disable-breadcrumb-h ()
  "Yield the shared tab-line breadcrumb underlay to lsp-mode."
  (when (bound-and-true-p breadcrumb-local-mode)
    (breadcrumb-local-mode -1)))

(add-hook 'lsp-managed-mode-hook #'my/language-server--disable-breadcrumb-h)
(add-hook 'lsp-managed-mode-hook #'my/lsp-tab-line-sync-h)
(add-hook 'lsp-configure-hook #'my/lsp-tab-line-sync-h t)

(with-eval-after-load 'breadcrumb
  (unless (advice-member-p #'my/breadcrumb-local-mode-a
                           'breadcrumb-local-mode)
    (advice-add 'breadcrumb-local-mode :around
                #'my/breadcrumb-local-mode-a))
  (add-hook 'after-load-theme-hook #'my/breadcrumb-tab-line-apply-ui)
  (add-hook 'server-after-make-frame-hook #'my/breadcrumb-tab-line-apply-ui)
  (my/breadcrumb-tab-line-apply-ui))

(with-eval-after-load 'lsp-headerline
  (unless (advice-member-p #'my/lsp-headerline-breadcrumb-mode-a
                           'lsp-headerline-breadcrumb-mode)
    (advice-add 'lsp-headerline-breadcrumb-mode :around
                #'my/lsp-headerline-breadcrumb-mode-a))
  (add-hook 'after-load-theme-hook #'my/lsp-tab-line-apply-ui)
  (add-hook 'server-after-make-frame-hook #'my/lsp-tab-line-apply-ui)
  (my/lsp-tab-line-apply-ui))

(defun my/language-server--visible-region (&optional window)
  "Return WINDOW's visible buffer region plus the configured line margin."
  (when-let* ((window (or window (get-buffer-window (current-buffer) t))))
    (let ((margin (max 0 my/language-server-visible-render-margin))
          start end)
      (save-excursion
        (goto-char (window-start window))
        (forward-line (- margin))
        (setq start (point))
        (goto-char (window-end window t))
        (forward-line margin)
        (setq end (point)))
      (cons start end))))

(defun my/language-server--point-visible-with-margin-p (point)
  "Return non-nil when POINT is near a visible window for this buffer."
  (seq-some
   (lambda (window)
     (when-let* ((region (my/language-server--visible-region window)))
       (and (<= (car region) point) (<= point (cdr region)))))
   (get-buffer-window-list (current-buffer) nil t)))

(defun my/language-server--range-visible-with-margin-p (range)
  "Return non-nil when LSP RANGE overlaps a visible warm region.
The server response may remain buffer-global as protocol/cache state; only
the decoration objects that reach lsp-mode's renderer are filtered here."
  (when-let* ((region (ignore-errors (lsp--range-to-region range))))
    (let ((beg (car region))
          (end (cdr region)))
      (seq-some
       (lambda (window)
         (when-let* ((visible (my/language-server--visible-region window)))
           (and (<= beg (cdr visible))
                (<= (car visible) end))))
       (get-buffer-window-list (current-buffer) nil t)))))

(defconst my/lsp-visible-ranged-decoration-methods
  '("textDocument/documentColor" "textDocument/documentLink")
  "LSP responses whose overlay rendering is bounded to visible windows.")

(defun my/lsp-request-async-visible-decorations-a
    (fn method params callback &rest keys)
  "Filter ranged decoration responses before calling CALLBACK.
METHOD requests and their cache/protocol lifetime are unchanged.  Color and
link overlays are the only part restricted to the visible region plus the
configured warm margin."
  (if (member method my/lsp-visible-ranged-decoration-methods)
      (let ((source-buffer (current-buffer)))
        (apply
         fn method params
         (lambda (result)
           (when (buffer-live-p source-buffer)
             (with-current-buffer source-buffer
               (funcall
                callback
                (seq-filter
                 (lambda (item)
                   (when-let* ((range (lsp-get item :range)))
                     (my/language-server--range-visible-with-margin-p range)))
                 result)))))
         keys))
    (apply fn method params callback keys)))

(defun my/lsp-document-color-refresh-visible ()
  "Refresh document-color overlays after the visible region changes."
  (when-let* ((region (my/language-server--visible-region)))
    (unless (equal region my/lsp-document-color-last-visible-region)
      (setq my/lsp-document-color-last-visible-region region)
      (lsp--document-color))))

(defun my/lsp-lens--display-visible-a (fn lenses)
  "Render only LENSES whose start position is visible in this window.
lsp-lens already notices page changes from its idle hook.  Filtering at the
display boundary keeps overlays bounded to the visible region while retaining
the upstream cache and its 350ms scroll/typing debounce."
  (if (get-buffer-window (current-buffer) t)
      (funcall
       fn
       (seq-filter
        (lambda (lens)
          (when-let* ((range (lsp-get lens :range))
                      (position (lsp-get range :start))
                      (point (ignore-errors
                               (lsp--position-to-point position))))
            (my/language-server--point-visible-with-margin-p point)))
        lenses))
    (funcall fn nil)))

(defun my/lsp-update-inlay-hints-visible-a (fn _start _end)
  "Request inlay hints only for the visible region plus its warm margin."
  (when-let* ((region (my/language-server--visible-region)))
    (funcall fn (car region) (cdr region))))

(defun my/lsp-semantic-tokens-request-visible-a (_fn)
  "Request semantic tokens for the same bounded warm region as other UI.
Servers without range support may still return a full token cache; Emacs'
jit-lock renderer continues to materialize faces only around visible text."
  (when (lsp-feature? "textDocument/semanticTokensFull")
    (when-let* ((region (my/language-server--visible-region)))
      (lsp--semantic-tokens-request region t))))

(with-eval-after-load 'lsp-lens
  (unless (advice-member-p
           #'my/lsp-lens--display-visible-a 'lsp-lens--display)
    (advice-add
     'lsp-lens--display :around #'my/lsp-lens--display-visible-a)))

(with-eval-after-load 'lsp-mode
  (unless (advice-member-p
           #'my/lsp-request-async-visible-decorations-a 'lsp-request-async)
    (advice-add
     'lsp-request-async
     :around #'my/lsp-request-async-visible-decorations-a))
  (unless (advice-member-p
           #'my/lsp-update-inlay-hints-visible-a 'lsp-update-inlay-hints)
    (advice-add
     'lsp-update-inlay-hints
     :around #'my/lsp-update-inlay-hints-visible-a)))

(with-eval-after-load 'lsp-semantic-tokens
  (unless (advice-member-p
           #'my/lsp-semantic-tokens-request-visible-a
           'lsp-semantic-tokens--request-update)
    (advice-add
     'lsp-semantic-tokens--request-update
     :around #'my/lsp-semantic-tokens-request-visible-a)))

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

(provide 'init-lsp)
;;; init-lsp.el ends here
