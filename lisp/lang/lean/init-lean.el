;;; init-lean.el --- Lean 4 on lsp-mode -*- lexical-binding: t -*-

;;; Commentary:
;; Defines lean-mode derived from prog-mode.  Reuses lean4-mode's proven
;; syntax/indent/unicode-input building blocks without pulling in
;; lean4-mode.el itself, whose own client registration would compete with the
;; one declared here.  LSP is driven by lsp-mode through
;; `my/register-language-server\='; the Lean-specific notifications
;; ($/lean/fileProgress, incremental publishDiagnostics, lean/restartFile)
;; live in init-lean-lsp.el.  The xwidget infoview is in
;; init-lean-infoview.el.

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'project)
(require 'remote-gateway)
(require 'seq)
(require 'subr-x)

;;; ── Package: lean4-mode kept for its sub-files only ─────────────────────────

;; We require only the sub-files (lean4-syntax, lean4-eri, lean4-input) and
;; the data/abbreviations.json.  lean4-mode.el itself is NEVER loaded; doing so
;; would trigger its lsp-mode client registration.
(my/package-ensure-vc 'lean4-mode
                      "https://github.com/leanprover-community/lean4-mode.git")

;; Try to load lean4 sub-files eagerly; silently skipped during batch-compile
;; when elpa is not on the path — they are always available at runtime.
(require 'lean4-syntax nil t)
(require 'lean4-eri    nil t)
(require 'lean4-input  nil t)

;;; ── Forward declarations ─────────────────────────────────────────────────────

(declare-function lean4-eri-indent "lean4-eri")
(declare-function lean4-eri-indent-reverse "lean4-eri")
(declare-function quail-show-key "quail")
(declare-function my/package-ensure-vc "init-package-vc")
(declare-function my/register-language-server "init-lsp")
(declare-function my/lsp-mode-start-now "init-lsp")
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/project-current-root "init-project")
(declare-function my/symbols-make-file-line-candidate "init-symbols")
(declare-function my/symbols-read-file-line-candidates "init-symbols")
(declare-function my/symbols-register-project-fallback "init-symbols")
(declare-function company-mode "company" (&optional arg))
(declare-function lsp-ui-doc-mode "lsp-ui-doc" (&optional arg))
(declare-function eldoc-doc-buffer "eldoc" ())
(declare-function eldoc-mode "eldoc" (&optional arg))
(declare-function lsp "lsp-mode" (&optional arg))
(declare-function lsp-execute-code-action "lsp-mode" (action))
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))
(declare-function lsp-semantic-tokens-mode "lsp-mode" (&optional arg))
(declare-function lsp-workspace-restart "lsp-mode" (workspace))
(declare-function lsp-workspaces "lsp-mode" ())
(declare-function lsp-stdio-connection "lsp-mode" (command &optional test-command))
(declare-function lsp-get "lsp-protocol" (from key))
(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-start "flymake" (&optional deferred force))
(declare-function lean-setup-flymake-backend "init-lean-lsp" ())
(declare-function lean-setup-sideline "init-lean-lsp" ())
(declare-function lean-notification-cleanup "init-lean-lsp" ())
(declare-function lean-refresh-file-dependencies "init-lean-lsp")
(declare-function lean--clear-fringe-overlays "init-lean-lsp")
(defvar lean-lsp-notification-handlers)
(declare-function lean-iv-sync-cursor-h "init-lean-infoview")
(declare-function lean-iv-setup-buffer-sync "init-lean-infoview")
(declare-function lean-iv-teardown-h "init-lean-infoview")
(declare-function lean-iv-server-restarted-h "init-lean-infoview")
(declare-function lean-iv-toggle "init-lean-infoview")
(declare-function lean-iv-restart "init-lean-infoview")
(declare-function lean-iv-node-p "init-lean-infoview")
(defvar lean--iv--script-dir
  (expand-file-name
   "lean4-infoview-bridge"
   (file-name-directory
    (or load-file-name
        buffer-file-name
        (locate-library "init-lean")
        user-emacs-directory)))
  "Directory containing the Lean Node proxy and infoview bundle.")
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/flymake-diagnostic-at-point-mode "init-lsp" (&optional arg))
(declare-function my/problems-buffer "init-problems" ())
(declare-function my/direnv-update-environment-maybe
                  "init-direnv" (&optional path callback))
(declare-function remote-expand-file-name
                  "remote-fs" (file-name &optional directory target))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-client-file-name "remote-fs"
                  (file-name &optional adapter))
(declare-function remote-local-bridge-command
                  "remote-process" (program &rest keys))
(declare-function remote-make-client-process
                  "remote-process" (&rest plist))
(declare-function remote-get-target "remote-core" (id))
(declare-function remote-target-trusted "remote-core" (target))
(defvar remote--buffer-base-process-environment)
(defvar remote--buffer-base-exec-path)

;; Forward defvar declarations for variables defined in sibling modules
(defvar lsp-ui-doc-mode)
(defvar eldoc-mode)
(defvar lean--file-progress)
(defvar lean--flymake-counts)
(defvar lean--fringe-overlays)
(defvar lean--resolving-lsp-root nil
  "Non-nil while the Lean project finder should claim the LSP root.")
(defvar flymake-fringe-indicator-position)
(defvar flymake-mode)
(defvar project-find-functions)
(defvar direnv--active-root)
;; From lean4-syntax (loaded above; defvar silences byte-compile for callers)
(defvar lean4-syntax-table)
(defvar lean4-font-lock-defaults)

;;; ── Defcustoms ───────────────────────────────────────────────────────────────

(defgroup lean nil
  "Lean 4 editing support."
  :group 'languages)

(config-defvar lean-lsp-connect-timeout nil
  "Seconds before timing out Lean language-server initialization.
Mathlib projects can legitimately take far longer than the shared default
while Lake warms the environment.  Set this to nil to never time out."
  :type '(choice (const :tag "Never time out" nil) (integer :tag "Seconds"))
  :group 'lean)

(config-defvar lean-lsp-start-delay nil
  "Seconds to wait before automatically starting the Lean language server.
This keeps freshly opened Mathlib buffers responsive while still starting the
language server without user action."
  :type 'number
  :group 'lean)

(config-defvar lean-info-window-width nil
  "Width for Lean xwidget infoview side windows."
  :type 'integer
  :group 'lean)

(config-defvar lean-dev-log-enabled nil
  "When non-nil, write Lean integration events to `lean-dev-log-buffer-name'."
  :type 'boolean
  :group 'lean)

(config-defvar lean-dev-log-buffer-name nil
  "Buffer name for Lean integration diagnostics."
  :type 'string
  :group 'lean)

;;; ── Development log ─────────────────────────────────────────────────────────

(define-derived-mode lean-dev-log-mode special-mode "Lean Dev Log"
  "Mode for Lean integration diagnostics.")

(defun lean-dev-log-buffer ()
  "Return the Lean development log buffer."
  (let ((buf (get-buffer-create lean-dev-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lean-dev-log-mode)
        (lean-dev-log-mode)))
    buf))

(defun lean-dev-log (format-string &rest args)
  "Append FORMAT-STRING with ARGS to `lean-dev-log-buffer-name'."
  (when lean-dev-log-enabled
    (let ((buf (lean-dev-log-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (follow (= (point) (point-max))))
          (goto-char (point-max))
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert (condition-case err
                      (apply #'format format-string args)
                    (error
                     (format "log formatting error: %S; format=%S args=%S"
                             err format-string args))))
          (insert "\n")
          (when follow
            (goto-char (point-max))))))))

(defun lean-dev-log-open ()
  "Open the Lean development log buffer."
  (interactive)
  (pop-to-buffer (lean-dev-log-buffer)))

;;; ── Project root helpers ─────────────────────────────────────────────────────

(defun lean-root-dir-p (dir)
  "Return non-nil when DIR looks like a Lean project root."
  (or (file-exists-p (expand-file-name "lakefile.lean" dir))
      (file-exists-p (expand-file-name "lakefile.toml" dir))
      (file-exists-p (expand-file-name "lean-toolchain" dir))))

(defun lean-project-root ()
  "Return the Lean project root for the current buffer."
  (let ((root
         (or (when-let* ((f (or buffer-file-name default-directory)))
               (locate-dominating-file f #'lean-root-dir-p))
             (and (fboundp 'my/project-current-root)
                  (my/project-current-root))
             default-directory)))
    (if (fboundp 'remote-canonicalize-file-name)
        (remote-canonicalize-file-name root)
      root)))

(defun lean--project-try-lsp (dir)
  "Return a Lean project for DIR while the language server needs an LSP root.
Scoped to `lean--resolving-lsp-root\=' so `project-current\=', Projectile and
ordinary navigation keep seeing the real project."
  (when (and lean--resolving-lsp-root
             (derived-mode-p 'lean-mode))
    (when-let* ((root (locate-dominating-file dir #'lean-root-dir-p)))
      (cons 'lean-lsp-project
            (file-name-as-directory (expand-file-name root))))))

(cl-defmethod project-root ((project (head lean-lsp-project)))
  "Return the root directory for a Lean LSP PROJECT."
  (cdr project))

(defun lean--install-project-finder ()
  "Install Lean's LSP project finder before broader project backends."
  (setq project-find-functions
        (cons #'lean--project-try-lsp
              (remove #'lean--project-try-lsp project-find-functions))))

(with-eval-after-load 'project
  (lean--install-project-finder))

(defun lean--lsp-project-root-candidate ()
  "Return the project root the language server should use for this buffer."
  (let ((lean--resolving-lsp-root t))
    (when-let* ((project (project-current nil)))
      (project-root project))))

;;; ── Language server contact ─────────────────────────────────────────────────────

(config-defvar lean-infoview-proxy-enabled nil
  "When non-nil, route the language server through lean-proxy.mjs.
The proxy is a transparent JSON-RPC passthrough to lake serve; it also
serves the official @leanprover/infoview over HTTP+SSE so a single Lean
LSP session drives both editing and the xwidget infoview.
Set to nil for a direct, no-proxy lake serve connection."
  :type 'boolean
  :group 'lean)

(config-defvar lean-infoview-remote-proxy-auto-deploy t
  "Compatibility switch for the retired remote proxy deployment path.
The active integration always runs the Node/HTTP infoview proxy on the Emacs
client.  Only its stdio LSP peer runs on the selected target."
  :type 'boolean
  :group 'lean)

(defvar lean--proxy-gateway-bindings (make-hash-table :test #'equal)
  "Current gateway registration data for each canonical Lean project root.")

(defun lean--proxy-script ()
  "Return the absolute path to lean-proxy.mjs, or nil if not found."
  (let ((script (expand-file-name "lean-proxy.mjs" lean--iv--script-dir)))
    (when (file-exists-p script) script)))

(defun lean--proxy-bundle-files ()
  "Return the local files needed by the Lean Node proxy."
  (let ((script (lean--proxy-script))
        (dist (expand-file-name "dist" lean--iv--script-dir)))
    (when (and script (file-directory-p dist))
      (cons script
            (sort (directory-files-recursively dist ".")
                  #'string<)))))

(defun lean--proxy-bundle-fingerprint ()
  "Return a stable fingerprint for the bundled Lean proxy files."
  (when-let* ((files (lean--proxy-bundle-files)))
    (secure-hash
     'sha256
     (mapconcat
      (lambda (file)
        (let ((attributes (file-attributes file 'string)))
          (format "%s:%s:%s"
                  (file-relative-name file lean--iv--script-dir)
                  (file-attribute-size attributes)
                  (file-attribute-modification-time attributes))))
      files "\n"))))

(defun lean--remote-proxy-directory (root)
  "Return the content-versioned target cache directory for ROOT."
  (when-let* ((fingerprint (lean--proxy-bundle-fingerprint)))
    (file-name-as-directory
     ;; `expand-file-name' expands a leading tilde before consulting the file
     ;; name handler.  In a `/fs:' buffer that substitutes the target's HOME
     ;; (for example `/home/hc') but drops the logical target identity, after
     ;; which `make-directory' tries to create that path on the client.  Home
     ;; expansion is a routed metadata operation and must remain in `/fs:'.
     (remote-expand-file-name
      (format "~/.cache/emacs/lean-infoview/%s/"
              (substring fingerprint 0 16))
      nil
      (remote-file-name-target root)))))

(defun lean--remote-proxy-trusted-p (root)
  "Return non-nil when ROOT belongs to a trusted remote target."
  (when-let* ((target-id (remote-file-name-target root))
              (target (remote-get-target target-id)))
    (remote-target-trusted target)))

(defun lean--copy-proxy-bundle-to-target (root)
  "Ensure the Lean proxy bundle is staged on ROOT's target.
Return the logical remote script name, or nil when provisioning is disabled."
  (when (and lean-infoview-remote-proxy-auto-deploy
             (lean--remote-proxy-trusted-p root))
    (let* ((destination (lean--remote-proxy-directory root))
           (marker (and destination
                        (expand-file-name ".complete" destination)))
           (fingerprint (lean--proxy-bundle-fingerprint))
           (remote-script
            (and destination
                 (expand-file-name "lean-proxy.mjs" destination))))
      (unless
          (and marker
               (file-readable-p marker)
               (equal
                (string-trim
                 (with-temp-buffer
                   (insert-file-contents marker)
                   (buffer-string)))
                fingerprint))
        (make-directory (expand-file-name "dist" destination) t)
        (dolist (source (lean--proxy-bundle-files))
          (let* ((relative
                  (file-relative-name source lean--iv--script-dir))
                 (target (expand-file-name relative destination)))
            (make-directory (file-name-directory target) t)
            (copy-file source target t t nil)))
        (with-temp-file marker
          (insert fingerprint "\n")))
      remote-script)))

(defun lean--proxy-script-for-root (root)
  "Return the client-local proxy script for project ROOT."
  (ignore root)
  (lean--proxy-script))

(defun lean--proxy-available-p ()
  "Return non-nil if the infoview proxy can be used for the current buffer."
  (and lean-infoview-proxy-enabled
       (lean--proxy-bundle-files)
       (lean--proxy-node-command default-directory)))

(defun lean--proxy-node-command (root)
  "Return a client-local Node command prefix for Lean project ROOT."
  (ignore root)
  (let ((process-environment
         (copy-sequence
          (or remote--buffer-base-process-environment
              (default-value 'process-environment))))
        (exec-path
         (copy-sequence
          (or remote--buffer-base-exec-path
              (default-value 'exec-path)))))
    (when-let* ((node (executable-find "node")))
      (list node))))

(defun lean--proxy-root-key (root)
  "Return the canonical hash key for Lean project ROOT."
  (file-name-as-directory (expand-file-name root)))

(defun lean--proxy-client-id (root)
  "Return the stable gateway client ID for Lean project ROOT."
  (format "lean-infoview-%s"
          (substring (secure-hash 'sha1 (lean--proxy-root-key root)) 0 12)))

(defun lean--proxy-allocate-gateway-binding (root)
  "Allocate and remember a gateway registration for ROOT."
  (let* ((key (lean--proxy-root-key root))
         (binding
          (remote-gateway-prepare-client
           (lean--proxy-client-id root) (remote-context root)
           :placement 'client
           :provides '("lean.cursor"))))
    (puthash key binding lean--proxy-gateway-bindings)
    binding))

(defun lean--proxy-gateway-binding (root)
  "Return ROOT's current gateway registration data."
  (gethash (lean--proxy-root-key root) lean--proxy-gateway-bindings))

(defun lean--proxy-gateway-client (root)
  "Return ROOT's connected Lean gateway peer, or nil."
  (when-let* ((binding (lean--proxy-gateway-binding root)))
    (remote-gateway-find-client
     (plist-get binding :client-id)
     (remote-context root))))

(defun lean--proxy-endpoint (root)
  "Return ROOT's live infoview HTTP endpoint object, or nil."
  (when-let* ((client (lean--proxy-gateway-client root)))
    (remote-gateway-client-endpoint client)))

(defun lean--proxy-forget-gateway-binding (root)
  "Forget ROOT's current gateway registration."
  (let ((key (lean--proxy-root-key root)))
    (when-let* ((binding (gethash key lean--proxy-gateway-bindings)))
      (remote-gateway-release-binding binding t))
    (remhash key lean--proxy-gateway-bindings)))

(defun lean--direct-server-command ()
  "Return the plain Lean server argv for the current buffer."
  (if (locate-dominating-file
       (or buffer-file-name default-directory ".") #'lean-root-dir-p)
      '("lake" "serve")
    '("lean" "--server")))

(defun lean--proxy-process-command ()
  "Return the client-side Node proxy argv, or nil when the proxy is unusable.

The proxy is a transparent JSON-RPC passthrough that additionally serves the
infoview.  It runs on the *client* (it talks to an xwidget here), while its
downstream argv bridges stdio to `lake serve\=' on the target, so the two
halves keep their correct placement."
  (let* ((root    (file-name-as-directory (lean-project-root)))
         (direct  (lean--direct-server-command)))
    (if-let* (((lean--proxy-available-p))
              (script (lean--proxy-script-for-root root))
              (node-command (lean--proxy-node-command root)))
        (let ((gateway (lean--proxy-allocate-gateway-binding root))
              (downstream
               (remote-local-bridge-command
                (car direct)
                :args (cdr direct)
                :context root
                :adapter "language-server"
                :directory root))
              (proxy-root
               (or
                (remote-client-file-name root)
                temporary-file-directory)))
          (lean-dev-log "server-contact: proxy root=%s gateway=%s downstream=%S"
                        root (plist-get gateway :websocket-url) downstream)
          (let ((command
                 (append
                  node-command
                  (list (remote-file-local-name script)
                        "--root" proxy-root
                        "--gateway-url" (plist-get gateway :websocket-url)
                        "--gateway-binding" (plist-get gateway :binding-id)
                        "--gateway-client-id" (plist-get gateway :client-id)
                        "--")
                  downstream))
                (name
                 (format "Lean proxy (%s)"
                         (file-name-nondirectory
                          (directory-file-name root)))))
            (cons name command)))
      (lean-dev-log "server-contact: direct contact=%S" direct)
      nil)))

(defun lean--lsp-connection ()
  "Return the lsp-mode connection plist for Lean.

When the infoview proxy is usable the transport is an explicit *client*
process created with `remote-make-client-process\=', because the Node proxy
renders into an xwidget on this machine.  Otherwise the plain `lake serve\='
argv is handed to `lsp-stdio-connection\=', which projects it onto the
workspace target through the `/fs:\=' process boundary like every other
server."
  (list
   :connect
   (lambda (filter sentinel name environment-fn workspace)
     (if-let* ((proxy (lean--proxy-process-command)))
         (let* ((process-name (generate-new-buffer-name (car proxy)))
                (stderr (get-buffer-create
                         (format "*%s::stderr*" process-name)))
                (proc (remote-make-client-process
                       :name process-name
                       :command (cdr proxy)
                       :connection-type 'pipe
                       :coding 'no-conversion
                       :buffer (format "*%s*" process-name)
                       :filter filter
                       :sentinel sentinel
                       :stderr stderr
                       :noquery t)))
           (set-process-query-on-exit-flag proc nil)
           (when-let* ((stderr-proc (get-buffer-process stderr)))
             (set-process-query-on-exit-flag stderr-proc nil))
           (lean-dev-log "lsp connect: proxy process=%s" process-name)
           (cons proc proc))
       (lean-dev-log "lsp connect: direct")
       (funcall
        (plist-get (lsp-stdio-connection #'lean--direct-server-command)
                   :connect)
        filter sentinel name environment-fn workspace)))
   :test?
   (lambda ()
     (or (lean--proxy-available-p)
         (and (fboundp 'my/language-server-executable-find)
              (my/language-server-executable-find
               (car (lean--direct-server-command))))))))

;; Older revisions intercepted every `make-process' call and tried to recognize
;; this command after the client had already wrapped it in a remote shell.  Remove
;; that hot-reload residue; the explicit process factory above is deterministic.
(when (fboundp 'lean--make-local-proxy-process-a)
  (advice-remove 'make-process #'lean--make-local-proxy-process-a))

;;; ── Mode-line progress ───────────────────────────────────────────────────────

(defvar-local lean--progress-mode-line-string nil)

(defun lean-progress-kind (item)
  "Return the :kind field from a fileProgress ITEM.
ITEM comes straight from the server, so it is read with `lsp-get\=': lsp-mode
delivers payloads as hash tables unless it was built with LSP_USE_PLISTS."
  (or (lsp-get item :kind) 1))

(defun lean--flymake-count (kind)
  "Return cached Lean Flymake diagnostic count for KIND."
  (or (and (boundp 'lean--flymake-counts)
           (plist-get lean--flymake-counts kind))
      0))

(defun lean--status-segment (text face help)
  "Return a propertized mode-line TEXT with FACE and HELP."
  (propertize text
              'face face
              'help-echo help
              'mouse-face 'mode-line-highlight))

(defun lean--progress-mode-line-text ()
  "Compact mode-line string reflecting Lean progress and diagnostics."
  (let* ((items     (and (boundp 'lean--file-progress) lean--file-progress))
         (total     (length items))
         (running   (seq-count (lambda (i) (eq (lean-progress-kind i) 1)) items))
         (blocked   (- total running))
         (errors    (lean--flymake-count :error))
         (warnings  (lean--flymake-count :warning))
         (notes     (lean--flymake-count :note))
         (help      (format "Lean: processing=%d, blocked=%d, errors=%d, warnings=%d, notes=%d"
                            running blocked errors warnings notes))
         parts)
    (when (> running 0)
      (push (lean--status-segment (format "…%d" running) 'warning help) parts))
    (when (> blocked 0)
      (push (lean--status-segment (format "!%d" blocked) 'error help) parts))
    (when (> errors 0)
      (push (lean--status-segment (format "E%d" errors) 'error help) parts))
    (when (> warnings 0)
      (push (lean--status-segment (format "W%d" warnings) 'warning help) parts))
    (when (> notes 0)
      (push (lean--status-segment (format "N%d" notes) 'success help) parts))
    (if parts
        (concat " λ[" (string-join (nreverse parts) " ") "]")
      (lean--status-segment " λ✓" 'success help))))

(defun lean-progress-mode-line-refresh ()
  "Update mode-line Lean progress indicator."
  (setq lean--progress-mode-line-string (lean--progress-mode-line-text))
  (force-mode-line-update t))

(define-minor-mode lean-progress-mode-line-mode
  "Show Lean elaboration state in the mode line."
  :lighter (:eval lean--progress-mode-line-string)
  (if lean-progress-mode-line-mode
      (lean-progress-mode-line-refresh)
    (setq lean--progress-mode-line-string nil)))

;;; ── Noema LSP UI integration ────────────────────────────────────────────

(defvar-local lean--lsp-start-timer nil
  "Timer used to start the language server for the current Lean buffer.")

(defvar-local lean--lsp-waiting-for-environment nil
  "Non-nil while Lean language-server startup waits for direnv.")

(config-defvar lean-lsp-deferred-ui-delays nil
  "Idle delays before enabling expensive Lean LSP UI features."
  :type '(alist :key-type symbol :value-type number)
  :group 'lean)

(defvar-local lean--lsp-ui-timers nil
  "Idle timers used to stage Lean LSP UI activation.")

(defun lean--cancel-lsp-start-timer ()
  "Cancel a pending Lean language-server startup timer."
  (when (timerp lean--lsp-start-timer)
    (cancel-timer lean--lsp-start-timer))
  (setq lean--lsp-start-timer nil))

(defun lean--cancel-lsp-ui-timers ()
  "Cancel pending deferred Lean LSP UI activation."
  (mapc (lambda (timer)
          (when (timerp timer)
            (cancel-timer timer)))
        lean--lsp-ui-timers)
  (setq lean--lsp-ui-timers nil))

(defun lean--enable-lsp-ui-feature (buffer feature)
  "Enable deferred LSP UI FEATURE in Lean BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lean-mode)
                 (bound-and-true-p lsp-managed-mode))
        (pcase feature
          ('company
           (when (fboundp 'company-mode)
             (company-mode 1)))
          ('doc
           (when (fboundp 'lsp-ui-doc-mode)
             (lsp-ui-doc-mode 1)))
          ('inlay-hints
           (when (fboundp 'lsp-inlay-hints-mode)
             (lsp-inlay-hints-mode 1)))
          ('semantic-tokens
           (when (fboundp 'lsp-semantic-tokens-mode)
             (lsp-semantic-tokens-mode 1))))))))

(defun lean--schedule-lsp-ui ()
  "Enable expensive Lean LSP UI features in separate idle slices."
  (lean--cancel-lsp-ui-timers)
  (dolist (entry lean-lsp-deferred-ui-delays)
    (push (run-with-idle-timer
           (max 0 (cdr entry)) nil
           #'lean--enable-lsp-ui-feature (current-buffer) (car entry))
          lean--lsp-ui-timers)))

(defun lean--lsp-managed-mode-a (fn &rest args)
  "Activate Lean LSP protocol support before its expensive UI features.

A Lean file elaborates for seconds after the server attaches; bringing up
completion, hover frames, inlay hints and semantic tokens in the same
command makes that wait feel like a freeze.  They are suppressed here and
re-enabled on separate idle slices by `lean--schedule-lsp-ui\='."
  (if (and (derived-mode-p 'lean-mode)
           (not (bound-and-true-p lsp-managed-mode)))
      (cl-letf (((symbol-function 'company-mode) (lambda (&optional _arg) nil))
                ((symbol-function 'lsp-ui-doc-mode)
                 (lambda (&optional _arg) nil))
                ((symbol-function 'lsp-inlay-hints-mode)
                 (lambda (&optional _arg) nil))
                ((symbol-function 'lsp-semantic-tokens-mode)
                 (lambda (&optional _arg) nil)))
        (prog1 (apply fn args)
          (when (bound-and-true-p lsp-managed-mode)
            (lean--schedule-lsp-ui))))
    (apply fn args)))

(defun lean--setup-diagnostics-ui ()
  "Enable Noema diagnostics UI pieces for the current Lean buffer."
  (when (boundp 'flymake-fringe-indicator-position)
    (setq-local flymake-fringe-indicator-position 'right-fringe))
  (when (fboundp 'flymake-mode)
    (flymake-mode 1))
  (when (fboundp 'my/flymake-diagnostic-at-point-mode)
    (my/flymake-diagnostic-at-point-mode -1)))

(defun lean--setup-managed-ui ()
  "Enable Lean-specific UI after lsp-mode starts managing this buffer.
Completion uses the global corfu+capf surface — no company-mode override."
  (when (fboundp 'lean-setup-sideline)
    (lean-setup-sideline))
  ;; A restart brings up a new proxy on a new port; an open infoview must
  ;; follow it instead of showing the dead one.
  (when (fboundp 'lean-iv-server-restarted-h)
    (lean-iv-server-restarted-h))
  (lean-dev-log "lsp UI active: flymake=%S eldoc=%S doc-frame=%S fringe=%S"
                (bound-and-true-p flymake-mode)
                (bound-and-true-p eldoc-mode)
                (bound-and-true-p lsp-ui-doc-mode)
                (and (boundp 'flymake-fringe-indicator-position)
                     flymake-fringe-indicator-position)))

;;; ── Project symbol search (ripgrep) ──────────────────────────────────────────

(defvar lean--project-symbol-history nil)

(defconst lean--rg-regexp
  (concat
   "^[[:space:]]*"
   "(?:@[[:alnum:]_.]+[[:space:]]+)*"
   "(?:(?:private|protected|noncomputable|unsafe|partial|scoped|local)"
   "[[:space:]]+)*"
   "(?:class[[:space:]]+inductive|inductive|instance|structure|class|theorem"
   "|axiom|lemma|definition|def|constant|abbrev|opaque)\\b")
  "Ripgrep pattern for top-level Lean 4 declarations.")

(defun lean--declaration-kind-and-name (text)
  "Extract (kind . name) from a Lean declaration line TEXT."
  (when (string-match
         (rx string-start
             (* (any " \t"))
             (* (seq "@" (+ (not (any " \t\n\r"))) (+ blank)))
             (* (seq (or "private" "protected" "noncomputable"
                         "unsafe" "partial" "scoped" "local")
                     (+ blank)))
             (group (or (seq "class" (+ blank) "inductive")
                        "inductive" "instance" "structure" "class"
                        "theorem" "axiom" "lemma" "definition" "def"
                        "constant" "abbrev" "opaque"))
             (+ blank)
             (group (+ (not (any " \t\n\r:={([,")))))
         text)
    (cons (match-string 1 text) (match-string 2 text))))

(defun lean--project-symbol-candidates ()
  "Return file-line candidates for Lean declarations in the current project."
  (when-let* ((rg   (if (fboundp 'my/language-server-executable-find)
                        (my/language-server-executable-find "rg")
                      (executable-find "rg")))
              (root (file-name-as-directory (expand-file-name (lean-project-root)))))
    (cl-loop for hit in
             (let ((default-directory root))
               (condition-case nil
                   (process-lines rg "--color" "never" "--line-number"
                                  "--no-heading" "--glob" "*.lean"
                                  "-e" lean--rg-regexp ".")
                 (error nil)))
             when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\(.*\\)\\'" hit)
             for file    = (match-string 1 hit)
             for line    = (string-to-number (match-string 2 hit))
             for text    = (string-trim (match-string 3 hit))
             for kn      = (lean--declaration-kind-and-name text)
             for summary = (if kn (format "%s %s" (car kn) (cdr kn)) text)
             collect (my/symbols-make-file-line-candidate
                      root (expand-file-name file root) line summary))))

(defun lean-project-symbols-fallback ()
  "Fallback project symbol search for Lean files (used by my/symbols)."
  (my/symbols-read-file-line-candidates
   (lean--project-symbol-candidates)
   "Lean symbol: "
   'lean--project-symbol-history
   'lean-project-symbol))

;;; ── Keybindings ──────────────────────────────────────────────────────────────

(defun lean--setup-keys ()
  "Install Lean-specific keys in the current buffer."
  ;; Official Lean infoview rendered in xwidget-webkit.
  (local-set-key (kbd "C-c C-i") #'lean-iv-toggle)
  (local-set-key (kbd "C-c i r") #'lean-iv-restart)
  ;; LSP management
  (local-set-key (kbd "C-c C-r") #'my/language-server-restart)
  (local-set-key (kbd "C-c C-d") #'lean-refresh-file-dependencies)
  (local-set-key (kbd "C-c C-a") #'my/language-server-code-actions)
  (local-set-key (kbd "C-c C-e") #'lsp-ui-doc-glance)
  (local-set-key (kbd "C-c C-l") #'lean-dev-log-open)
  (local-set-key (kbd "C-c !")   #'my/problems-buffer)
  (local-set-key (kbd "C-c ?")   #'my/diagnostics-dispatch)
  ;; Unicode / input
  (local-set-key (kbd "C-c C-k") #'quail-show-key)
  ;; Indentation
  (local-set-key (kbd "TAB")       #'lean4-eri-indent)
  (local-set-key (kbd "<backtab>") #'lean4-eri-indent-reverse))

;;; ── Remote tuning ────────────────────────────────────────────────────────────

(defun lean--tune-remote ()
  "Apply Lean editing tweaks for TRAMP buffers."
  nil)

(defun lean--apply-lsp-settings ()
  "Install Lean-specific language-server settings in the current buffer."
  ;; Lake can take minutes to warm Mathlib; the shared startup watchdog would
  ;; otherwise stop the workspace before it ever initializes.
  (when lean-lsp-connect-timeout
    (setq-local my/lsp-mode-startup-timeout lean-lsp-connect-timeout))
  (lean-dev-log "lean-mode setup: file=%s root=%s proxy-enabled=%S connect-timeout=%S"
                (or buffer-file-name "<no file>")
                (file-name-as-directory (expand-file-name (lean-project-root)))
                lean-infoview-proxy-enabled
                lean-lsp-connect-timeout))

;;; ── Post-command hook: infoview cursor sync ──────────────────────────────────

(defun lean--post-command-h ()
  "On cursor movement in a Lean buffer, sync the xwidget infoview."
  (when (derived-mode-p 'lean-mode)
    (when (fboundp 'lean-iv-sync-cursor-h)
      (lean-iv-sync-cursor-h))))

;;; ── Major mode ───────────────────────────────────────────────────────────────

;;;###autoload
(define-derived-mode lean-mode prog-mode "Lean"
  "Major mode for Lean 4 source files.

Reuses lean4-mode's syntax table, font-lock keywords, indentation
engine (lean4-eri), and the \\='Lean\\=' Quail unicode-input method.
lean4-mode.el itself is never loaded: its own client registration would
compete with the one this module declares."
  :group 'lean
  ;; Ensure lean4 sub-files are loaded (idempotent — already loaded if elpa is ready)
  (require 'lean4-syntax nil t)
  (require 'lean4-eri    nil t)
  (require 'lean4-input  nil t)
  ;; Syntax table: copy lean4's if available, else inherit prog-mode's
  (when (and (boundp 'lean4-syntax-table) lean4-syntax-table)
    (set-syntax-table lean4-syntax-table))
  ;; Comments
  (setq-local comment-start       "--")
  (setq-local comment-start-skip  "[-/]-[ \t]*")
  (setq-local comment-end         "")
  (setq-local comment-end-skip    "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding     1)
  (setq-local comment-use-syntax  t)
  ;; Font-lock / indentation
  (setq-local font-lock-defaults  lean4-font-lock-defaults)
  (setq-local indent-tabs-mode    nil)
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  ;; Unicode input (lean4-input defines the "Lean" Quail package)
  (set-input-method "Lean")
  ;; No electric indent — Lean's structural indentation is managed by lean4-eri
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))
  ;; Trim trailing whitespace on save
  (add-hook 'before-save-hook #'whitespace-cleanup nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean-mode))

;;; ── Mode hook ────────────────────────────────────────────────────────────────

(defun lean--mode-hook ()
  "Full lean-mode setup installed via lean-mode-hook."
  ;; Sibling modules — loaded here so they are ready before the server connects.
  (require 'init-lean-lsp      nil t)
  (require 'init-lean-infoview nil t)
  (require 'init-lean-jump     nil t)
  ;; Projectile may install its project backend after init-lean loads.  Re-pin
  ;; the Lean finder before lsp-mode asks project.el for an LSP root.
  (lean--install-project-finder)
  ;; Keys
  (lean--setup-keys)
  ;; Language-server startup policy
  (lean--apply-lsp-settings)
  ;; Lean owns diagnostics so it can preserve Lean-specific tags
  ;; (`fullRange', `isSilent') and coalesce the large initial
  ;; publishDiagnostics burst, so lsp-mode's own diagnostics stay out.
  (setq-local lsp-diagnostics-provider :none)
  (when (fboundp 'lean-setup-flymake-backend)
    (lean-setup-flymake-backend))
  ;; Mode-line progress indicator
  (lean-progress-mode-line-mode 1)
  ;; Noema Flymake diagnostics UI; managed-mode hook completes the rest.
  (lean--setup-diagnostics-ui)
  ;; `prog-mode' hooks may have enabled Flymake before the Lean backend was
  ;; installed.  Start it once here so the backend receives its report function.
  (when (and (fboundp 'flymake-start)
             (bound-and-true-p flymake-mode))
    (flymake-start nil t))
  ;; Remote tuning
  (lean--tune-remote)
  ;; Keep the infoview bridge's shadow LSP document in sync while visible.
  (when (fboundp 'lean-iv-setup-buffer-sync)
    (lean-iv-setup-buffer-sync))
  ;; Post-command: goal refresh + infoview cursor sync
  (add-hook 'post-command-hook #'lean--post-command-h nil t)
  ;; Buffer teardown
  (add-hook 'kill-buffer-hook  #'lean--buffer-teardown-h nil t)
  ;; Start the language server shortly after the mode hook returns.  This
  ;; keeps the initial file visit responsive but still starts `lake serve'
  ;; automatically.
  (lean--schedule-lsp-start))

(defun lean--schedule-lsp-start ()
  "Schedule automatic language-server startup for the current Lean buffer."
  (lean--cancel-lsp-start-timer)
  (let ((buf (current-buffer))
        (delay (max 0 (or lean-lsp-start-delay 0))))
    (lean-dev-log "lsp scheduled: delay=%s buffer=%s"
                  delay (buffer-name buf))
    (setq lean--lsp-start-timer
          (run-at-time
           delay nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq lean--lsp-start-timer nil)
                 (when (derived-mode-p 'lean-mode)
                   (lean--ensure-lsp)))))
           buf))))

(defun lean--lsp-direnv-ready (_environment error)
  "Resume Lean language-server startup after direnv, falling back after ERROR."
  (setq lean--lsp-waiting-for-environment nil)
  (when error
    (lean-dev-log "lsp direnv failed; using target base environment: %s"
                  (error-message-string error))
    (message
     "Lean LSP: direnv failed (%s); continuing with target base environment"
     (error-message-string error)))
  ;; The asynchronous contract has already applied a successful capsule to
  ;; this buffer.  Start directly instead of re-entering discovery and risking
  ;; another pending latch.  On failure this follows the shared policy:
  ;; availability is more important than an optional environment layer.
  (lean--start-lsp-now))

(defun lean--start-lsp-now ()
  "Start the language server for this Lean buffer through the shared path."
  (if (fboundp 'my/lsp-mode-start-now)
      (my/lsp-mode-start-now)
    (call-interactively #'lsp)))

(defun lean--ensure-lsp ()
  "Start the language server for this lean-mode buffer if not already managed."
  ;; A manual infoview open owns startup once it reaches this boundary.  Do not
  ;; let the mode hook's delayed automatic start allocate a second proxy
  ;; instance while the first connection is still being established.
  (lean--cancel-lsp-start-timer)
  (cond
   ((not buffer-file-name)
    (lean-dev-log "lsp skipped: buffer has no file"))
   ((not (fboundp 'lsp))
    (lean-dev-log "lsp skipped: `lsp-mode' is not available"))
   ((bound-and-true-p lsp-managed-mode)
    (lean-dev-log "lsp already managing buffer: %s" (buffer-name)))
   ((and lean--lsp-waiting-for-environment
         (bound-and-true-p direnv--active-root))
    ;; A pre-fix busy retry could apply the environment but lose the callback
    ;; promised to this buffer.  The active direnv layer is authoritative:
    ;; clear the stale latch and continue instead of waiting forever.
    (setq lean--lsp-waiting-for-environment nil)
    (lean-dev-log "lsp recovered a completed direnv wait: %s"
                  direnv--active-root)
    (lean--ensure-lsp))
   (lean--lsp-waiting-for-environment
    (lean-dev-log "lsp waiting for target environment: %s"
                  (buffer-name)))
   ((and
     (fboundp 'my/direnv-update-environment-maybe)
     (eq
      (my/direnv-update-environment-maybe
       nil #'lean--lsp-direnv-ready)
      'pending))
    (setq lean--lsp-waiting-for-environment t)
    (lean-dev-log "lsp deferred until direnv is ready: %s"
                  (buffer-name)))
   (t
    (lean-dev-log "lsp starting: buffer-dir=%s lsp-root=%s proxy-enabled=%S timeout=%S project-finders=%S"
                  default-directory
                  (or (lean--lsp-project-root-candidate) "<none>")
                  lean-infoview-proxy-enabled
                  lean-lsp-connect-timeout
                  project-find-functions)
    (when (numberp lean-lsp-connect-timeout)
      (let ((buf (current-buffer))
            (timeout lean-lsp-connect-timeout))
        (run-at-time
         (1+ timeout) nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (and (derived-mode-p 'lean-mode)
                          (not (bound-and-true-p lsp-managed-mode)))
                 (lean-dev-log
                  "lsp not connected after %s seconds; check the server stderr and Messages buffers"
                  timeout))))))))
    (condition-case err
        (lean--start-lsp-now)
      (error
       (lean-dev-log "lsp start error: %s" (error-message-string err))
       (signal (car err) (cdr err)))))))

(add-hook 'lean-mode-hook #'lean--mode-hook)

(defun lean--buffer-teardown-h ()
  "Cancel timers, clear overlays, and close the infoview on buffer kill."
  (lean--cancel-lsp-start-timer)
  (lean--cancel-lsp-ui-timers)
  (when (fboundp 'lean-notification-cleanup)
    (lean-notification-cleanup))
  (when (fboundp 'lean--clear-fringe-overlays)
    (lean--clear-fringe-overlays))
  (when (fboundp 'lean-iv-teardown-h)
    (lean-iv-teardown-h)))

;;; ── Language server registration ─────────────────────────────────────────────

(defun lean--lsp-managed-mode-log-h ()
  "Log and finish setup when lsp-mode activates support in a Lean buffer."
  (when (derived-mode-p 'lean-mode)
    (if (bound-and-true-p lsp-managed-mode)
        (progn
          (lean--setup-managed-ui)
          (lean-dev-log "lsp managed mode active: buffer=%s lsp-root=%s flymake=%S eldoc=%S capf=%S"
                        (buffer-name)
                        (or (lean--lsp-project-root-candidate) "<none>")
                        (bound-and-true-p flymake-mode)
                        (bound-and-true-p eldoc-mode)
                        completion-at-point-functions))
      (when (fboundp 'lean-notification-cleanup)
        (lean-notification-cleanup))
      (lean--cancel-lsp-ui-timers)
      (lean-dev-log "lsp managed mode inactive: buffer=%s" (buffer-name)))))

(with-eval-after-load 'lsp-mode
  ;; The client declares Lean's notification handlers, so they must exist
  ;; before it is registered — not only once a Lean buffer opens.
  (require 'init-lean-lsp nil t)
  (unless (advice-member-p #'lean--lsp-managed-mode-a 'lsp-managed-mode)
    (advice-add 'lsp-managed-mode :around #'lean--lsp-managed-mode-a))
  (add-hook 'lsp-managed-mode-hook #'lean--lsp-managed-mode-log-h)
  (add-to-list 'lsp-language-id-configuration '(lean-mode . "lean4"))
  (when (fboundp 'my/register-language-server)
    (my/register-language-server
     'lean-mode
     (lean--lsp-connection)
     :server-id 'lean4
     :priority 1
     :notification-handlers (bound-and-true-p lean-lsp-notification-handlers)
     :executables '("lean" "lake" "node")
     :placement 'hybrid
     :label "Lean Language Server"
     :source "lean4-mode (sub-files only)"
     :note "Runs the Node/HTTP proxy on the client and bridges stdio to target lake serve")))

;;; ── Orphan worker sweep ──────────────────────────────────────────────────────

(defun my/lean-sweep-orphan-workers ()
  "Terminate orphaned `lean --worker' processes left behind by a dead watchdog.
The Lean LSP server (`lean --server' / `lake serve') forks a `lean --worker'
child per open file, and that child holds the (often multi-GB) elaboration
heap.  If the server is killed without a clean LSP shutdown, the worker
survives, reparented to init (ppid 1).  Only ppid-1 processes whose command
line targets a `.cell/' mirror file are touched, so workers still owned by
a live watchdog (any other Emacs/editor session) are never affected."
  (interactive)
  (let ((killed 0))
    (dolist (pid (list-system-processes))
      (let* ((attrs (process-attributes pid))
             (ppid  (cdr (assq 'ppid attrs)))
             (args  (cdr (assq 'args attrs))))
        (when (and args
                   (eql ppid 1)
                   (string-match-p "/lean --worker " args)
                   (string-match-p "/\\.cell/" args))
          (lean-dev-log "sweep-orphan-workers: terminating pid=%s args=%s" pid args)
          (ignore-errors (signal-process pid 'SIGTERM))
          (cl-incf killed))))
    (when (called-interactively-p 'any)
      (message "Lean orphan sweep: terminated %d worker process(es)" killed))
    killed))

(run-with-idle-timer 5 nil #'my/lean-sweep-orphan-workers)

;;; ── Project symbol fallback ──────────────────────────────────────────────────

(with-eval-after-load 'init-symbols
  (my/symbols-register-project-fallback 'lean-mode
                                        #'lean-project-symbols-fallback))

(provide 'init-lean)
;;; init-lean.el ends here
