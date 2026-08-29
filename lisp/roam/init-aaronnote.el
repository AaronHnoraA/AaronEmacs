;;; init-aaronnote.el --- Noema Web/Appine bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Noema web host and opens it in Appine/xwidget.
;; The editable document state lives in Noema's CodeMirror app; Emacs does
;; not mirror buffer edits into the browser.

;;; Code:

(require 'config)

(require 'json)
(require 'remote-gateway)
(require 'remote-process)
(require 'remote-workspace)
(require 'subr-x)
(require 'cl-lib)
(require 'url)
(require 'url-util)
(require 'init-aaronnote-jupyter-cell)
(require 'init-aaronnote-jupyter-runtime)
(require 'init-aaronnote-jupyter-server)
(require 'init-aaronnote-jupyter-lsp)

(declare-function my/xwidget-open-url "init-browser" (url &rest args))
(declare-function my/xwidget-current-url "init-browser" (&optional buffer))
(declare-function my/xwidget-session-buffer "init-browser" (id))
(declare-function my/xwidget-focus "init-browser" (&optional buffer))
(declare-function my/noema--focus-xwidget-buffer "noema-xwidget-keys" (buffer))
(declare-function my/noema--harden-xwidget-placeholder "noema-xwidget-keys" (&optional buffer))
(declare-function my/noema--sync-xwidget-recovery-mode "noema-xwidget-keys" (&rest args))
(declare-function my/xwidget-undo "init-browser" ())
(declare-function my/xwidget-redo "init-browser" ())
(declare-function my/xwidget-setup-control-line "init-browser" ())
(declare-function my/appine-open-url "init-appine" (url))
(declare-function my/appine-open-url-fresh "init-appine" (url))
(declare-function my/appine-kill-all "init-appine" ())
(declare-function my/appine--tab-forget "init-appine" (url))
(declare-function my/appine--tab-reset "init-appine" ())
(declare-function my/appine--switch-to-tab-index "init-appine" (target-index))
(declare-function appine-focus "appine" ())
(declare-function my/open-system-target "init-open" (target))
(declare-function my/noema-roam-note-changed "init-md-roam" (file))
(declare-function my/noema-roam--clear-runtime-cache "init-md-roam" ())
(declare-function my/noema-roam--cancel-sync-timer "init-md-roam" ())
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-edit-mode "xwidget" (&optional arg))
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script &optional callback))
(declare-function xwidget-webkit-pass-command-event "xwidget" (event))
(declare-function my/zotero-open-reference "init-latex" (payload))
(declare-function my/zotero-import-bibtex "init-latex" (payload))
(declare-function remote-expand-file-name
                  "remote-fs" (file-name &optional directory target))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-canonicalize-file-name
                  "remote-fs" (file-name &optional directory))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-file-local-name "remote-fs" (file-name))
(defvar remote-mode nil)
(defvar my/appine-tab-list)
(defvar my/xwidget--session-id)
(defvar my/xwidget-suppress-auto-focus)

;; Publish module — lazy, loaded only when a publish command is first invoked.
(autoload 'my/noema-publish              "init-aaronnote-publish" nil t)
(autoload 'my/noema-publish-build        "init-aaronnote-publish" nil t)
(autoload 'my/noema-publish-deploy       "init-aaronnote-publish" nil t)
(autoload 'my/noema-publish-clean        "init-aaronnote-publish" nil t)

(defgroup my/noema nil
  "Noema Markdown web editor integration."
  :group 'applications)

(defvar my/noema--web-host-script
  (expand-file-name "lisp/roam/Noema/web-host.mjs" user-emacs-directory)
  "Path to the Noema web host script.")

(defvar my/noema--web-dir
  (expand-file-name "lisp/roam/Noema/dist/aaronnote" user-emacs-directory)
  "Path to the built Noema web app.")

(defvar my/noema--runtime-root
  (expand-file-name "lisp/roam/Noema" user-emacs-directory)
  "Path to the vendored Noema runtime.")

(defvar my/noema--state-root
  (expand-file-name "var/aaronnote" user-emacs-directory)
  "Path to Noema state files under the Emacs config.")

(defvar my/noema--tmp-root
  (expand-file-name "tmp" my/noema--state-root)
  "Path to Noema runtime temporary files under the Emacs config.")

(defconst my/noema-jupyter-output-buffer-name "*Noema Jupyter*")
(defconst my/noema-jupyter-output-client-id "aaronnote-jupyter")

(defvar my/noema--snippets-root
  (expand-file-name "snippets" user-emacs-directory)
  "Path to Noema snippets shared with Emacs.")

(defvar my/noema--templates-root
  (expand-file-name "templates/noema" user-emacs-directory)
  "Path to Markdown templates shared by Emacs and Noema.")

(defvar my/noema--latex-templates-root
  (expand-file-name "templates" user-emacs-directory)
  "Path to LaTeX templates shared by Emacs and Noema.")

(defun my/noema--app-config-file ()
  "Return the canonical Noema application configuration file."
  (expand-file-name
   "config.json"
   (or (getenv "NOEMA_CONFIG_DIR")
       (expand-file-name "~/.config/noema"))))

(defun my/noema--workspace-config ()
  "Return Noema's configured workspace object, or nil."
  (let ((file (my/noema--app-config-file)))
    (when (file-readable-p file)
      (condition-case err
          (gethash "workspace"
                   (json-parse-string
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))
                    :object-type 'hash-table))
        (error
         (message "Noema: ignoring invalid app config %s: %s" file err)
         nil)))))

(defun my/noema-workspace-root ()
  "Return the canonical Noema workspace root shared by both hosts."
  (let* ((workspace (my/noema--workspace-config))
         (configured (and workspace (gethash "root" workspace)))
         (root (or (getenv "NOEMA_ROOT")
                   (getenv "AARONNOTE_ROOT")
                   (and (stringp configured) (not (string-empty-p configured)) configured)
                   "~/Documents/Noema")))
    (file-name-as-directory (expand-file-name root))))

(defun my/noema-workspace-layout ()
  "Return the canonical Noema workspace layout name."
  (let* ((workspace (my/noema--workspace-config))
         (configured (and workspace (gethash "layout" workspace)))
         (layout (or (getenv "NOEMA_WORKSPACE_LAYOUT") configured "legacy")))
    (if (equal (downcase (format "%s" layout)) "wiki") "wiki" "legacy")))

(defvar my/noema--notes-root (my/noema-workspace-root)
  "Path to the canonical Noema workspace.")

(defun my/noema--project-settings ()
  "Read Noema's project settings from the note root without evaluation."
  (let ((file (expand-file-name ".dir-locals.el" my/noema--notes-root)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (forward-comment (point-max))
            (let* ((dir-locals (read (current-buffer)))
                   (global-variables (cdr (assq nil dir-locals))))
              (cdr (assq 'my/project-local-settings global-variables))))
        (error nil)))))

(defun my/noema--jupyter-defaults ()
  "Return project-configured Jupyter defaults for new Noema cells."
  (let* ((settings (my/noema--project-settings))
         (configured (plist-get settings :aaronnote-jupyter)))
    (list :language (format "%s" (or (plist-get configured :language) "python"))
          :kernel (format "%s" (or (plist-get configured :kernel) "python3"))
          :session (format "%s" (or (plist-get configured :session) "default")))))

(defun my/noema--jupyter-default-environment ()
  "Return web-host environment entries for project Jupyter defaults."
  (let ((defaults (my/noema--jupyter-defaults)))
    (list
     (format "AARONNOTE_JUPYTER_DEFAULT_LANGUAGE=%s" (plist-get defaults :language))
     (format "AARONNOTE_JUPYTER_DEFAULT_KERNEL=%s" (plist-get defaults :kernel))
     (format "AARONNOTE_JUPYTER_DEFAULT_SESSION=%s" (plist-get defaults :session)))))

(defvar my/noema--katex-macros-dir
  (expand-file-name "etc/katex-macros" user-emacs-directory)
  "Folder of .tex files defining the global KaTeX macro environment.")

(config-defvar my/noema-backend nil
  "Backend used to display Noema."
  :type '(choice (const :tag "xwidget-webkit" xwidget) (const :tag "Appine" appine))
  :group 'my/noema)

(config-defvar my/noema-web-port nil
  "Fixed port for the Noema web host.
Set to 0 to let the OS pick a random port."
  :type 'integer
  :group 'my/noema)

(config-defvar my/noema-web-host-max-heap-mb nil
  "V8 heap cap (MB) for the Noema web-host node process, or nil for no cap.
Passed as a `--max-old-space-size' command-line flag rather than
`NODE_OPTIONS' in the environment, because web-host's `process.env' is also
handed to the codex/claude/opencode CLIs it shells out to for LaTeX export —
an env-based cap would leak onto those unrelated node processes too."
  :type '(choice (integer :tag "Heap cap in MB") (const :tag "No cap" nil))
  :group 'my/noema)

(config-defvar my/noema-echo-severity 'error
  "Warning/error policy for Noema messages copied to the Emacs echo area.
Important command responses are always echoed after browser-side deduplication
and rate limiting.  `error' additionally echoes errors, `warning' echoes
warnings and errors, and nil suppresses both severity classes."
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Warnings and errors" warning)
                 (const :tag "Never" nil))
  :group 'my/noema)

(config-defvar my/noema-latex-export-engine "codex"
  "Engine for the Noema CMD+P LaTeX export.
\"codex\" compile-verifies a deterministic mechanical draft first, then allows
one fidelity-gated polish attempt.  A verified draft is never retried after an
agent timeout or gate rejection; multiple repairs require a concrete mechanical
compile failure.  \"mechanical\" never invokes an agent.  See
`docs/latex-export-style.md' in the Noema app."
  :type '(choice (const "codex") (const "mechanical"))
  :group 'my/noema)

(config-defvar my/noema-latex-export-max-attempts 3
  "Maximum feedback-driven agent repairs after mechanical verification fails.
A fidelity/review rejection falls back immediately instead of retrying."
  :type 'integer
  :group 'my/noema)

(config-defvar my/noema-latex-export-agent-idle-timeout 180
  "Seconds without agent output before Noema performs a liveness check.
A live process is kept running; this is not a kill timeout."
  :type 'integer
  :group 'my/noema)

(config-defvar my/noema-latex-export-agent-hard-timeout 900
  "Absolute seconds allowed for one LaTeX export agent attempt.
At this limit Noema requests graceful termination before using a hard kill."
  :type 'integer
  :group 'my/noema)

(config-defvar my/noema-latex-export-agent "codex"
  "AI backend for the Noema LaTeX export repair step.
One of \"codex\", \"claude\", or \"opencode\".  All run non-interactively in the
single-export staging directory with external-directory writes blocked and
network access available.  Clean mechanically verified exports do not launch
the backend.  The backend is chosen here, not per export."
  :type '(choice (const "codex") (const "claude") (const "opencode"))
  :group 'my/noema)

(config-defvar my/noema-codex-model ""
  "Optional model id for codex during LaTeX export polish (empty = codex default)."
  :type 'string
  :group 'my/noema)

(config-defvar my/noema-latex-export-model ""
  "Optional model id passed to the active LaTeX export backend (empty = default)."
  :type 'string
  :group 'my/noema)

(config-defvar my/noema-opencode-executable "opencode"
  "Executable used when the LaTeX export backend is opencode."
  :type 'string
  :group 'my/noema)

(defvar my/noema--last-sync-stats nil
  "String summary from the last successful Wiki index refresh, or nil.")

(defvar my/noema--process nil
  "Running Noema web-host child process, or nil.")
(defvar my/noema--last-interrupt-snapshot nil
  "Emacs/xwidget state captured when C-g interrupts a Noema adapter action.")
(defvar my/noema--process-log-queue nil
  "Newest-first diagnostic output waiting to be written outside its filter.")
(defvar my/noema--process-log-bytes 0
  "Approximate byte size of `my/noema--process-log-queue'.")
(defvar my/noema--process-log-timer nil
  "One-shot timer flushing deferred web-host diagnostics.")
(defconst my/noema--process-log-queue-limit (* 256 1024)
  "Maximum diagnostic output retained between deferred log flushes.")
(defvar my/noema--gateway-binding nil
  "Registration data for the current Noema web-host process.")
(defvar my/noema--external-file-watches (make-hash-table :test #'equal)
  "Remote-backed file watches owned by the Noema runtime session.")
(defvar my/noema--external-file-watch-timers
  (make-hash-table :test #'equal)
  "Debounce timers for remote Noema file changes.")
(defvar my/noema--external-file-watch-suppressed
  (make-hash-table :test #'equal)
  "Times before which self-write watch events should be ignored.")
(defvar my/noema--port nil
  "HTTP port of the running Noema web-host.")
(defvar my/noema--last-port nil
  "Last ready web-host port, retained so a crashed core can reclaim its URL.")
(defvar my/noema--ready nil
  "Non-nil once the web-host has announced its port.")
(defvar my/noema--ready-callbacks nil
  "Callbacks waiting for the web-host to become ready.")
(defvar my/noema--app-buffer nil
  "Buffer hosting the Appine/xwidget Noema page.")
(defvar my/noema--ready-watchdog nil
  "Watchdog timer cancelled when the web-host becomes ready.")
(defvar my/noema--goto-timer nil
  "Debounce timer for coalescing goto events from the web-host.")
(defvar my/noema--goto-last nil
  "Last applied goto key (truename-file line col), for dedup.")
(defvar my/noema--file-buffers (make-hash-table :test #'equal)
  "Canonical Noema file path to browser buffer map.")
(defvar my/noema--client-buffers (make-hash-table :test #'equal)
  "Noema browser client id to browser buffer map.")

(defun my/noema--record-interrupted-operation (operation)
  "Record adapter OPERATION and xwidget state after it receives `quit'."
  (let* ((buffer (current-buffer))
         (window-buffer (and (window-live-p (selected-window))
                             (window-buffer (selected-window))))
         (xwidget-buffer
          (and (buffer-live-p buffer)
               (fboundp 'my/noema--xwidget-buffer-p)
               (my/noema--xwidget-buffer-p buffer))))
    (setq my/noema--last-interrupt-snapshot
          `((time . ,(format-time-string "%FT%T%z"))
            (operation . ,operation)
            (buffer . ,(and (buffer-live-p buffer) (buffer-name buffer)))
            (selectedWindowBuffer
             . ,(and (buffer-live-p window-buffer)
                     (buffer-name window-buffer)))
            (majorMode . ,major-mode)
            (xwidgetBuffer . ,(and xwidget-buffer t))
            (xwidgetEditMode . ,(and (boundp 'xwidget-webkit-edit-mode)
                                    xwidget-webkit-edit-mode t))
            (recoveryMode . ,(and (boundp 'my/noema-xwidget-recovery-mode)
                                  my/noema-xwidget-recovery-mode t))
            (client . ,(and (boundp 'my/noema--client-id)
                            my/noema--client-id))
            (hostReady . ,(and my/noema--ready t))
            (hostProcessLive . ,(and (processp my/noema--process)
                                     (process-live-p my/noema--process) t))
            (outboundQueue . ,(length my/noema--post-queue))
            (inboundQueue . ,(length my/noema--host-event-queue))
            (thisCommand . ,this-command)
            (lastCommand . ,last-command)))
    (message "Noema: C-g interrupted %s; state saved in M-x my/noema-interrupt-status"
             operation)))

(defun my/noema-interrupt-status ()
  "Display the most recent C-g snapshot from the Noema Emacs adapter."
  (interactive)
  (if (null my/noema--last-interrupt-snapshot)
      (message "Noema: no adapter operation has been interrupted by C-g")
    (with-current-buffer (get-buffer-create "*Noema interrupted operation*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry my/noema--last-interrupt-snapshot)
          (insert (format "%-22s %S\n" (car entry) (cdr entry))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defvar my/noema--build-process nil
  "Current Noema web build process, or nil.")

(defvar my/noema--split-counter 0
  "Counter for fresh Noema xwidget split sessions.")

(defvar my/noema--split-direction nil
  "Preferred direction for the next explicit Noema split.")

(defvar-local my/noema-buffer-file-name nil
  "Current note file represented by an Noema Appine/xwidget buffer.")

(put 'my/noema-buffer-file-name 'permanent-local t)

(defvar-local my/noema--client-id nil
  "Client id for this Noema browser buffer.")

(put 'my/noema--client-id 'permanent-local t)

(defvar-local my/noema--registered-file nil
  "File path currently registered for this Noema browser buffer.")

(put 'my/noema--registered-file 'permanent-local t)

(defvar-local my/noema--xwidget-forced-name nil
  "Non-nil display name marker for Noema xwidget buffers.")

(put 'my/noema--xwidget-forced-name 'permanent-local t)

(defvar-local my/noema--xwidget-pending-file nil
  "File to POST to Noema once the page has finished loading, or nil.")

(put 'my/noema--xwidget-pending-file 'permanent-local t)

;; Keep the Markdown/xwidget input bridge in a dedicated module.  Its command
;; names and wire protocol remain unchanged for browser and Emacs callers.
(add-to-list 'load-path
             (expand-file-name "lisp/roam/Noema/emacs" user-emacs-directory))
(require 'noema-xwidget-keys)

(defvar-keymap my/noema-keys-mode-map
  "M-z" #'my/noema-undo
  "M-Z" #'my/noema-redo
  "M-S-z" #'my/noema-redo
  "M-C" #'my/noema-prose-check)

(define-minor-mode my/noema-keys-mode
  "Buffer-local keys for an Noema browser surface."
  :init-value nil
  :lighter nil
  :keymap my/noema-keys-mode-map)

(defconst my/noema--xwidget-focus-script
  "(() => {
  const focusEditor = () => {
    try {
      window.dispatchEvent(new CustomEvent('aaronnote:command', {
        detail: { command: 'focus' }
      }));
    } catch (_) {}
    const target = document.querySelector(
      '.cm-content, .cm-editor [contenteditable=\"true\"], [data-editor] [contenteditable=\"true\"]'
    );
    if (!target || typeof target.focus !== 'function') return false;
    try {
      target.focus({ preventScroll: true });
    } catch (_) {
      target.focus();
    }
    return true;
  };
  focusEditor();
  requestAnimationFrame(focusEditor);
  setTimeout(focusEditor, 50);
  return true;
})()"
  "JavaScript used to move focus into the Noema editor inside xwidget.")

(defun my/noema--server-url (&optional path)
  "Return the local Noema URL for PATH."
  (format "http://127.0.0.1:%d%s" my/noema--port (or path "/")))

(defun my/noema--canonical-file (file)
  "Return canonical absolute FILE for Noema bookkeeping, or nil."
  (and (stringp file)
       (not (string-empty-p file))
       (cond
        ((and (bound-and-true-p remote-mode)
              (fboundp 'remote-expand-file-name))
         ;; Noema's web host runs locally.  Raw host paths (including
         ;; `~/...') therefore belong to the local target; an already logical
         ;; or TRAMP path retains its encoded target and is rejected later by
         ;; `my/noema--host-file' when the local host cannot serve it.
         (let ((expanded
                (remote-expand-file-name
                 file nil
                 (unless
                     (or (and (fboundp 'remote-fs-file-name-p)
                              (remote-fs-file-name-p file))
                         (string-match-p "\\`fs://" file)
                         (file-remote-p file))
                   "local"))))
           ;; Remote mode represents native paths as /fs:local:.  Preserve
           ;; that logical identity while resolving native directory aliases.
           (if (and (fboundp 'remote-file-name-target)
                    (fboundp 'remote-file-local-name)
                    (fboundp 'remote-canonicalize-file-name)
                    (equal (remote-file-name-target expanded) "local"))
               (remote-canonicalize-file-name
                (file-truename (remote-file-local-name expanded)))
             expanded)))
        ((and (bound-and-true-p remote-mode)
              (fboundp 'remote-canonicalize-file-name))
         (remote-canonicalize-file-name file))
        (t
         ;; A client id is a protocol identity, not just a display path.  On
         ;; macOS /tmp is an alias of /private/tmp; keeping only the expanded
         ;; spelling creates two xwidget clients for the same note and lets a
         ;; retained alias pane participate in lifecycle routing.  Resolve
         ;; local directory symlinks before deriving registries or client ids.
         (file-truename (expand-file-name file))))))

(defun my/noema--host-file (file)
  "Return the path Noema should use for logical FILE.
Local files are projected to native host paths.  Remote files retain their
`/fs:' identity and are served through the Remote-backed gateway provider."
  (when-let* ((file (my/noema--canonical-file file)))
    (if (and (bound-and-true-p remote-mode)
             (fboundp 'remote-file-name-target)
             (fboundp 'remote-file-local-name))
        (if (equal (remote-file-name-target file) "local")
            (remote-file-local-name file)
          file)
      (expand-file-name file))))

(defun my/noema--xwidget-session-id (&optional file)
  "Return the stable xwidget session/client id for FILE."
  (if-let* ((file (my/noema--canonical-file file)))
      (format "aaronnote:%s" file)
    "aaronnote"))

(defun my/noema--split-client-p (client)
  "Return non-nil when CLIENT identifies a split pane."
  (and (stringp client)
       (string-prefix-p "aaronnote-split:" client)))

(defun my/noema--app-url (&optional file client extra-params)
  "Return the Noema app URL, optionally opening FILE for CLIENT."
  (let ((base (my/noema--server-url "/"))
        params)
    (when-let* ((file (my/noema--canonical-file file)))
      (push (cons "file" (my/noema--host-file file)) params))
    (when (and (stringp client) (not (string-empty-p client)))
      (push (cons "client" client) params))
    (dolist (param extra-params)
      (when (and (consp param) (car param) (cdr param))
        (push (cons (format "%s" (car param)) (format "%s" (cdr param))) params)))
    (if params
        (concat base "?"
                (mapconcat
                 (lambda (param)
                   (format "%s=%s"
                           (url-hexify-string (car param))
                           (url-hexify-string (cdr param))))
                 (nreverse params)
                 "&"))
      base)))

(defun my/noema-jupyter--output-url (payload)
  "Return the singleton Jupyter output page URL for PAYLOAD."
  (let ((base (my/noema--server-url "/jupyter.html")) params)
    (dolist (entry payload)
      (when (and (cdr entry) (not (string-empty-p (format "%s" (cdr entry)))))
        (push (cons (format "%s" (car entry)) (format "%s" (cdr entry))) params)))
    (push (cons "client" my/noema-jupyter-output-client-id) params)
    (concat base "?"
            (mapconcat
             (lambda (entry)
               (format "%s=%s"
                       (url-hexify-string (car entry))
                       (url-hexify-string (cdr entry))))
             (nreverse params) "&"))))

(defun my/noema-jupyter--output-dispatch (buffer payload)
  "Ask an existing Jupyter output BUFFER to open PAYLOAD as a tab."
  (when (and (buffer-live-p buffer)
             (fboundp 'xwidget-webkit-current-session)
             (fboundp 'xwidget-webkit-execute-script))
    (with-current-buffer buffer
      (when-let* ((session (xwidget-webkit-current-session)))
        (xwidget-webkit-execute-script
         session
         (format
          "(() => { const p = %s; if (typeof window.noemaJupyterOpenDocument === 'function') { window.noemaJupyterOpenDocument(p); if (p.view && typeof window.noemaJupyterOpenView === 'function') window.noemaJupyterOpenView(p.view); return true; } return false; })()"
          (json-serialize payload :null-object nil :false-object :json-false)))))))

;;;###autoload
(defun my/noema-jupyter-output-open (&optional cell-id focus view)
  "Open the singleton Noema Jupyter output page for CELL-ID.
The page is displayed below the current script buffer.  With FOCUS non-nil,
move keyboard focus to the page; otherwise preserve source-buffer focus."
  (interactive (list nil t))
  (unless (and buffer-file-name
               (bound-and-true-p my/noema-jupyter-cell-mode))
    (user-error "Current buffer is not a Noema Jupyter Cell script"))
  (my/noema-jupyter-cell--update-highlight)
  (let* ((source-buffer (current-buffer))
         (source-window (selected-window))
         (payload
          `((scriptFile . ,(my/noema--host-file buffer-file-name))
            (sourceFile . ,(my/noema--host-file
                            my/noema-jupyter-cell-source-file))
            (cellId . ,(or cell-id my/noema-jupyter-cell-current-id ""))
            (language . ,(or my/noema-jupyter-cell-language "python"))
            (kernel . ,(or my/noema-jupyter-cell-kernel "python3"))
            (session . ,(or my/noema-jupyter-cell-session "default"))
            ,@(when view `((view . ,view)))))
         (url (my/noema-jupyter--output-url payload)))
    (my/noema--ensure-server
     (lambda ()
       (when (and (buffer-live-p source-buffer)
                  (window-live-p source-window))
         (with-selected-window source-window
           (let* ((existing
                   (and (fboundp 'my/xwidget-session-buffer)
                        (my/xwidget-session-buffer
                         my/noema-jupyter-output-client-id)))
                  (target-window
                   (or (and existing (get-buffer-window existing 'visible))
                       (split-window source-window nil 'below)))
                  buffer)
             (if (buffer-live-p existing)
                 (progn
                   (setq buffer existing)
                   (set-window-buffer target-window buffer)
                   (my/noema-jupyter--output-dispatch buffer payload))
               (unless (fboundp 'my/xwidget-open-url) (require 'init-browser))
               (with-selected-window target-window
                 (setq buffer
                       (my/xwidget-open-url
                        url :id my/noema-jupyter-output-client-id
                        :display 'current :reuse-selected t))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq-local my/xwidget-suppress-auto-focus (not focus))
                 (setq-local my/noema--client-id
                             my/noema-jupyter-output-client-id)
                 (setq-local my/noema--xwidget-forced-name
                             my/noema-jupyter-output-buffer-name)
                 (unless (equal (buffer-name) my/noema-jupyter-output-buffer-name)
                   (rename-buffer my/noema-jupyter-output-buffer-name t))))
             (if focus
                 (progn
                   (select-window target-window)
                   (run-at-time 0.2 nil #'my/xwidget-focus buffer))
               (select-window source-window)))))))
    (get-buffer my/noema-jupyter-output-buffer-name)))

(defun my/noema--markdown-file-p (file)
  "Return non-nil when FILE is a Markdown file."
  (and file
       (or (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
           (string-equal (file-name-nondirectory file) "README"))))

(defun my/noema--web-host-log-tail (&optional lines)
  "Return the last LINES (default 12) lines of the Noema web-host log
buffer, or nil when the buffer does not exist or has no output yet."
  (when-let* ((buf (get-buffer " *Noema web host*")))
    (with-current-buffer buf
      (when (> (point-max) (point-min))
        (let* ((n (or lines 12))
               (end (point-max))
               (start (save-excursion
                        (goto-char end)
                        (forward-line (- n))
                        (point)))
               (text (string-trim (buffer-substring-no-properties start end))))
          (unless (string-empty-p text) text))))))

(defun my/noema--watchdog-fire ()
  "Called when the web-host fails to become ready within the timeout.
Previously this only dropped the queued ready-callbacks silently with a
one-line `message'; the caller who pressed e.g. `A' for the agenda had no
visible sign that nothing was going to happen. Now it also surfaces the
process state and a tail of the log buffer, and pops that buffer so the
failure is diagnosable without hunting for it."
  (setq my/noema--ready-watchdog nil)
  (unless my/noema--ready
    (let* ((dropped (length my/noema--ready-callbacks))
           (alive (and my/noema--process (process-live-p my/noema--process)))
           (tail (my/noema--web-host-log-tail))
           (log-buf (get-buffer " *Noema web host*")))
      (setq my/noema--ready-callbacks nil)
      (when log-buf (display-buffer log-buf))
      (message "%s"
               (concat
                (format "Noema: web-host not ready after 10s (%d pending action%s dropped)."
                        dropped (if (= dropped 1) "" "s"))
                (if alive "" " Process exited — check node/port.")
                (if tail (format " Last log: %s" tail) " No log output yet — see *Noema web host*."))))))

(defun my/noema--ensure-server (&optional callback)
  "Start the web-host if needed, then call CALLBACK."
  (if (and my/noema--process
           (process-live-p my/noema--process)
           my/noema--ready)
      (when callback (funcall callback))
    (when callback
      (push callback my/noema--ready-callbacks))
    (unless (and my/noema--process
                 (process-live-p my/noema--process))
      (when (fboundp 'my/appine-kill-all)
        (ignore-errors (my/appine-kill-all)))
      (when (fboundp 'my/appine--tab-reset)
        (my/appine--tab-reset))
      (my/noema--start-server)
      (when my/noema--ready-watchdog
        (cancel-timer my/noema--ready-watchdog))
      (setq my/noema--ready-watchdog
            (run-at-time 10 nil #'my/noema--watchdog-fire)))))

(defun my/noema--start-server (&optional reconnect-port)
  "Spawn the vendored Noema web-host.
When RECONNECT-PORT is non-nil, reclaim that port so live browser pages can
reconnect without a reload and without losing their in-memory editor state."
  (my/noema--clear-process-log-queue)
  (setq my/noema--notes-root (my/noema-workspace-root))
  (make-directory my/noema--notes-root t)
  (unless (executable-find "node")
    (user-error "Noema: `node' not found in exec-path; install Node.js"))
  (unless (file-directory-p my/noema--web-dir)
    (user-error "Noema: built web app not found at %s; run `npm run build' in %s"
                my/noema--web-dir my/noema--runtime-root))
  (let ((old-proc my/noema--process))
    (when (and old-proc (process-live-p old-proc))
      (ignore-errors (signal-process old-proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p old-proc)
            (delete-process old-proc))))))
  (setq my/noema--process nil
        my/noema--port nil
        my/noema--ready nil)
  (let* ((log-buf (get-buffer-create " *Noema web host*"))
         (_copilot-gateway-method
          (require 'init-copilot nil t))
         (gateway
          (remote-gateway-prepare-client
           "aaronnote" (remote-context my/noema--notes-root)
           :placement 'client
           :provides '("aaronnote.command" "aaronnote.api")))
         (copilot-server
          (when (and (not _copilot-gateway-method)
                     (require 'copilot nil t))
            (ignore-errors (copilot-server-executable))))
         (process-environment
          (append
           (delq nil
            (append
             (list
            "AARONNOTE_HOST_MODE=emacs"
            (format "NOEMA_ROOT=%s" (expand-file-name my/noema--notes-root))
            (format "AARONNOTE_ROOT=%s" (expand-file-name my/noema--notes-root))
            (format "NOEMA_WORKSPACE_LAYOUT=%s" (my/noema-workspace-layout))
            (format "AARONNOTE_WEB_DIR=%s" (expand-file-name my/noema--web-dir))
            (format "AARONNOTE_RUNTIME_ROOT=%s" (expand-file-name my/noema--runtime-root))
            (format "AARONNOTE_WORKSPACE_ROOT=%s" (expand-file-name my/noema--notes-root))
            (format "AARONNOTE_LANGUAGETOOL_LANGUAGE=%s"
                    (or (bound-and-true-p my/languagetool-language) "en-US"))
            (format "AARONNOTE_LANGUAGETOOL_URL=%s"
                    (or (bound-and-true-p my/languagetool-server-url)
                        "http://10.243.90.222:8765"))
            (format "AARONNOTE_PROSE_WORDS=%s"
                    (expand-file-name "etc/prose-accepted-words.txt"
                                      user-emacs-directory))
            (format "AARONNOTE_PUBLISH_JS_DIR=%s"
                    (expand-file-name "js" my/noema--runtime-root))
            (format "AARONNOTE_STATE_DIR=%s" (expand-file-name my/noema--state-root))
            (format "AARONNOTE_TMP_DIR=%s" (expand-file-name my/noema--tmp-root))
            (format "AARONNOTE_SNIPPETS_ROOT=%s" (expand-file-name my/noema--snippets-root))
            (format "AARONNOTE_TEMPLATES_ROOT=%s" (expand-file-name my/noema--templates-root))
            (format "AARONNOTE_LATEX_TEMPLATES_ROOT=%s"
                    (expand-file-name my/noema--latex-templates-root))
            (format "AARONNOTE_KATEX_MACROS_DIR=%s" (expand-file-name my/noema--katex-macros-dir)))
            (my/noema--jupyter-default-environment)
            (list
            (format "AARONNOTE_LATEX_EXPORT_ENGINE=%s"
                    (or my/noema-latex-export-engine "codex"))
            (format "AARONNOTE_LATEX_EXPORT_AGENT=%s"
                    (or (bound-and-true-p my/noema-latex-export-agent) "codex"))
            (format "AARONNOTE_LATEX_EXPORT_MAX_ATTEMPTS=%d"
                    (or my/noema-latex-export-max-attempts 3))
            (format "AARONNOTE_LATEX_EXPORT_AGENT_IDLE_TIMEOUT_MS=%d"
                    (* 1000 (max 10 (or my/noema-latex-export-agent-idle-timeout 180))))
            (format "AARONNOTE_LATEX_EXPORT_AGENT_HARD_TIMEOUT_MS=%d"
                    (* 1000 (max (or my/noema-latex-export-agent-idle-timeout 180)
                                 (or my/noema-latex-export-agent-hard-timeout 900))))
            (format "AARONNOTE_CODEX_BIN=%s"
                    (or (bound-and-true-p codex-cli-executable) "codex"))
            (format "AARONNOTE_CLAUDE_BIN=%s"
                    (or (bound-and-true-p claude-code-ide-cli-path) "claude"))
            (format "AARONNOTE_OPENCODE_BIN=%s"
                    (or (bound-and-true-p my/noema-opencode-executable) "opencode"))
            (when (and (boundp 'my/noema-codex-model)
                       (stringp my/noema-codex-model)
                       (not (string-empty-p my/noema-codex-model)))
              (format "AARONNOTE_CODEX_MODEL=%s" my/noema-codex-model))
            (when (and (boundp 'my/noema-latex-export-model)
                       (stringp my/noema-latex-export-model)
                       (not (string-empty-p my/noema-latex-export-model)))
              (format "AARONNOTE_LATEX_EXPORT_MODEL=%s" my/noema-latex-export-model))
            (format "AARONNOTE_WEB_PORT=%d"
                    (or reconnect-port my/noema-web-port 0))
            ;; Emacs-started Noema should share Emacs' existing Copilot LS
            ;; through the gateway, not spawn a second memory-heavy copy.
            "AARONNOTE_COPILOT_DISABLE_LOCAL=1"
            (format "AARONNOTE_EMACS_GATEWAY_URL=%s"
                    (plist-get gateway :websocket-url))
            (format "AARONNOTE_EMACS_GATEWAY_BINDING=%s"
                    (plist-get gateway :binding-id))
            (format "AARONNOTE_EMACS_GATEWAY_CLIENT_ID=%s"
                    (plist-get gateway :client-id))
            (when copilot-server
              (format "AARONNOTE_COPILOT_LANGUAGE_SERVER=%s"
                      (expand-file-name copilot-server)))
            (when (and (not _copilot-gateway-method)
                       (bound-and-true-p my/copilot-server-max-heap-mb))
              (format "AARONNOTE_COPILOT_MAX_HEAP_MB=%d"
                      my/copilot-server-max-heap-mb)))))
           process-environment))
         (proc
          (remote-make-client-process
             :name "aaronnote-web-host"
             :buffer log-buf
             :command
             (append
              (list "node")
              (when my/noema-web-host-max-heap-mb
                (list
                 (format "--max-old-space-size=%d"
                         my/noema-web-host-max-heap-mb)))
              (list (my/noema--host-file
                     my/noema--web-host-script)))
             :noquery t
             :sentinel #'my/noema--sentinel
             :filter #'my/noema--process-filter
             :remote-client-directory user-emacs-directory
             :remote-client-environment process-environment)))
    (with-current-buffer log-buf (erase-buffer))
    (setq my/noema--process proc
          my/noema--gateway-binding gateway)
    proc))

(defun my/noema--flush-ready-callbacks ()
  "Run callbacks waiting for the server to become ready."
  (when my/noema--ready-watchdog
    (cancel-timer my/noema--ready-watchdog)
    (setq my/noema--ready-watchdog nil))
  (let ((callbacks (nreverse my/noema--ready-callbacks)))
    (setq my/noema--ready-callbacks nil)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback)))
  (my/noema--install-activity-hooks)
  ;; Do an initial activity check after the page has had time to load.
  (run-at-time 0.2 nil #'my/noema--update-activity))

(defun my/noema--run-zotero-event (payload import-p)
  "Handle Zotero PAYLOAD in Emacs; IMPORT-P starts the BibTeX picker."
  (let* ((client (alist-get 'client payload))
         (source-buffer (my/noema--key-source-buffer client)))
    (my/noema--release-xwidget-input-buffer source-buffer)
    (my/noema--select-emacs-window)
    (condition-case err
        (progn
          (unless (if import-p
                      (fboundp 'my/zotero-import-bibtex)
                    (fboundp 'my/zotero-open-reference))
            (require 'init-latex))
          (if import-p
              (my/zotero-import-bibtex payload)
            (my/zotero-open-reference payload)))
      (error
       (message "Noema Zotero %s failed: %s"
                (if import-p "import" "open")
                (error-message-string err))))))

(defun my/noema--handle-input-focus (payload)
  "Record that the Noema renderer named by PAYLOAD owns keyboard input.

This adapter derives each pane\'s foreground state from Emacs window and frame
selection.  On the macOS xwidget port a click inside the WebKit view can make
that view first responder without Emacs selecting the surrounding window, so
the pane keeps being reported as background and re-paused under the user\'s
cursor.  A renderer sends this only after a trusted input event it received
while paused, so it is authoritative: select that pane\'s window and recompute
the activity snapshot, which resumes it and pauses its siblings.

The renderer has already dropped its own host pause by the time this arrives,
but the Node host still retains the old pause for reconnect replay. Mark local
state unknown so the recomputed snapshot sends an explicit resume and updates
both sides of the protocol before later background transitions are deduped."
  (let* ((client (alist-get 'client payload))
         (buffer (my/noema--buffer-for-client client)))
    (when (buffer-live-p buffer)
      (setq my/noema--app-buffer buffer)
      (with-current-buffer buffer
        (setq-local my/noema--activity-paused :unknown))
      (when-let* ((window (get-buffer-window buffer 'visible)))
        (unless (eq window (selected-window))
          (my/noema--select-emacs-window window)))
      (setq my/noema--last-activity-signature :unknown)
      (my/noema--update-activity))))

(defun my/noema--handle-ui-state-payload (payload)
  "Echo a structured Noema UI-state PAYLOAD when policy permits.
Gateway payloads are handled directly so status strings are never serialized
to JSON a second time."
  (let* ((status (alist-get 'status payload))
         (severity-name (alist-get 'severity payload))
         (severity
          (and (stringp severity-name)
               (intern-soft severity-name)))
         (echo-p
          (or
           (eq severity 'info)
           (pcase my/noema-echo-severity
             ('warning (memq severity '(warning error)))
             ('error (eq severity 'error))
             (_ nil)))))
    (when (and echo-p
               (stringp status)
               (not (string-empty-p status)))
      (message "Noema %s: %s" severity status))))

(defun my/noema--handle-process-line (line)
  "Handle one legacy Noema event encoded as LINE."
  (let ((ready-prefix "aaronote-web-host:ready:")
        (goto-prefix "aaronote-event:goto:")
	(open-prefix "aaronote-event:open:")
        (system-open-prefix "aaronote-event:system-open:")
	(zotero-prefix "aaronote-event:zotero:")
	(zotero-import-prefix "aaronote-event:zotero-import:")
	(current-file-prefix "aaronote-event:current-file:")
	(ui-state-prefix "aaronote-event:ui-state:")
        (saved-prefix "aaronote-event:saved:")
        (key-prefix "aaronote-event:key:"))
    (cond
     ((string-prefix-p ready-prefix line)
      (let ((port (string-to-number (substring line (length ready-prefix)))))
        (when (> port 0)
          (setq my/noema--port port
                my/noema--last-port port
                my/noema--ready t)
          (my/noema--flush-ready-callbacks)
          ;; Gateway registration/re-registration is the reconnect event for
          ;; Emacs.  Reconcile open notebooks once here; notebook buffers do
          ;; not run polling timers.
          (when (fboundp 'my/noema-jupyter-cell-refresh-open-buffers)
            (my/noema-jupyter-cell-refresh-open-buffers)))))
     ((string-prefix-p goto-prefix line)
      (let* ((payload (substring line (length goto-prefix)))
             (parts (split-string payload ":" nil))
             (line-number (string-to-number (or (car parts) "0")))
             (column (string-to-number (or (cadr parts) "0"))))
        (when (> line-number 0)
          ;; Coalesce burst goto events: cancel any pending jump and schedule
          ;; a fresh one.  Normal (not idle) timer so jumps are not deferred
          ;; indefinitely during continuous Emacs activity.
          (when my/noema--goto-timer
            (cancel-timer my/noema--goto-timer))
          (setq my/noema--goto-timer
                (run-at-time
                 0.05 nil
                 (let ((ln line-number) (col column))
                   (lambda ()
                     (setq my/noema--goto-timer nil)
                     (my/noema--goto-location nil ln col))))))))
     ((string-prefix-p open-prefix line)
	      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length open-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload))
                 (line-number (or (alist-get 'line payload) 1))
                 (column (or (alist-get 'col payload) 0))
                 (tag (alist-get 'tag payload)))
            (if (and (my/noema--markdown-file-p file)
                     (or (null tag) (string-empty-p (or tag ""))))
                ;; Markdown note (e.g. graph double-click): open in Noema.
                (my/noema-open-file file)
              ;; Source region (lean, etc.) or explicit tag: open in Emacs.
              (my/noema--goto-location file line-number column)
              (when (and (stringp file)
                         (string-match-p "\\.ipynb\\'" file)
                         (require 'init-aaronnote-jupyter-cell nil t))
                (ignore-errors
                  (my/noema-jupyter-cell-activate-buffer payload)))
              (when (and tag (not (string-empty-p (or tag ""))))
                (when (require 'init-note-code nil t)
                  (ignore-errors (my/note-code--goto-tag tag))))))
	        (error
	         (message "Noema event parse failed: %s" (error-message-string err)))))
     ((string-prefix-p zotero-import-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-import-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'my/noema--run-zotero-event payload t))
        (error
         (message "Noema Zotero import event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p zotero-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'my/noema--run-zotero-event payload nil))
        (error
         (message "Noema Zotero event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p system-open-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length system-open-prefix))
                           :object-type 'alist))
                 (target (alist-get 'target payload)))
            (when (and (stringp target) (not (string-empty-p target)))
              (cond
               ;; Absolute file/dir path: use smart routing (dired, find-file,
               ;; pdf->system, etc.) instead of delegating to macOS `open'.
               ((file-name-absolute-p target)
                (require 'init-open)
                (my/open-file target))
               ;; URL schemes (http, zotero, …): system open as before.
               (t
                (require 'init-open)
                (my/open-system-target target)))))
        (error
         (message "Noema system-open event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p current-file-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length current-file-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload))
                 (client (alist-get 'client payload)))
            (my/noema--sync-app-buffer-file file client))
        (error
         (message "Noema current-file parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p ui-state-prefix line)
      (condition-case err
          (my/noema--handle-ui-state-payload
           (json-parse-string
            (substring line (length ui-state-prefix))
            :object-type 'alist))
        (error
         (message "Noema UI-state parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p saved-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length saved-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload)))
            (when (and (stringp file) (not (string-empty-p file)))
              (when (fboundp 'my/noema-roam-note-changed)
                (my/noema-roam-note-changed file))))
        (error
         (message "Noema saved-event parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p key-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length key-prefix))
                           :object-type 'alist))
                 (key (alist-get 'key payload))
                 (client (alist-get 'client payload)))
            (when (stringp key)
              (my/noema--run-emacs-key key client)))
        (error
         (message "Noema key-event parse failed: %s"
                  (error-message-string err))))))))

(defun my/noema--external-file (file)
  "Return a canonical Markdown FILE accepted by the gateway provider."
  (let ((file (my/noema--canonical-file file)))
    (unless (and file (my/noema--markdown-file-p file))
      (error "Noema external provider requires a Markdown file: %s"
             file))
    file))

(defun my/noema--external-file-metadata (file)
  "Return JSON-ready metadata for FILE, or signal when it is unavailable."
  (let ((attributes (file-attributes file 'string)))
    (unless (and attributes (file-regular-p file))
      (error "Remote Markdown file is unavailable: %s" file))
    `((mtimeMs
       . ,(* 1000.0
             (float-time
              (file-attribute-modification-time attributes))))
      (size . ,(or (file-attribute-size attributes) 0)))))

(defun my/noema--external-file-notify-change (file)
  "Notify the Noema peer that logical FILE changed externally."
  (remhash file my/noema--external-file-watch-timers)
  (when-let* ((client (remote-gateway-find-client "aaronnote")))
    (let* ((metadata
            (condition-case nil
                (my/noema--external-file-metadata file)
              (error '((mtimeMs . 0) (size . 0)))))
           (mtime (alist-get 'mtimeMs metadata)))
      (remote-gateway-notify
       client "aaronnote.command"
       `((type . "command")
         (command . "note-saved")
         (file . ,file)
         (mtimeMs . ,mtime)
         (clientId . "remote-external"))))))

(defun my/noema--external-file-watch-event (file event)
  "Debounce Remote file watch EVENT for logical FILE."
  (unless (or (eq (nth 1 event) 'stopped)
              (< (float-time)
                 (or
                  (gethash
                   file my/noema--external-file-watch-suppressed)
                  0)))
    (when-let* ((timer
                 (gethash
                  file my/noema--external-file-watch-timers)))
      (cancel-timer timer))
    (puthash
     file
     (run-at-time
      0.25 nil #'my/noema--external-file-notify-change file)
     my/noema--external-file-watch-timers)))

(defun my/noema--ensure-external-file-watch (file)
  "Ensure one recoverable Remote watch exists for logical FILE."
  (when (and (bound-and-true-p remote-mode)
             ;; The web-host runs on the client, so it already sees any file
             ;; the client can open natively and watches it itself.  A routed
             ;; watch is needed exactly when it cannot -- which is a backend
             ;; capability, not a target name.
             (not (remote-client-file-name file))
             (not (gethash file my/noema--external-file-watches)))
    (condition-case error
        (let* ((context (remote-context (file-name-directory file)))
               (workspace (remote-workspace-open context :connect nil))
               (resource
                (remote-workspace-add-file-watch
                 workspace file '(change attribute-change)
                 (lambda (event)
                   (my/noema--external-file-watch-event file event))
                 :key (list 'aaronnote-external-file file)
                 :metadata
                 (list :application "aaronnote" :file file))))
          (puthash
           file (cons workspace resource)
           my/noema--external-file-watches))
      (error
       ;; Editing still works on backends without watch capability; refresh is
       ;; then explicit and saves continue to use mtime conflict detection.
       (message "Noema remote watch unavailable for %s: %s"
                file (error-message-string error))))))

(defun my/noema--clear-external-file-watches ()
  "Close Noema's Remote watches and debounce timers."
  (maphash
   (lambda (_file timer)
     (when (timerp timer)
       (cancel-timer timer)))
   my/noema--external-file-watch-timers)
  (maphash
   (lambda (_file owner)
     (ignore-errors
       (remote-workspace-close-resource
        (car owner) (cdr owner) 'aaronnote-stop)))
   my/noema--external-file-watches)
  (clrhash my/noema--external-file-watch-timers)
  (clrhash my/noema--external-file-watches)
  (clrhash my/noema--external-file-watch-suppressed))

(defun my/noema--external-file-read (params _client)
  "Read the logical Markdown file named by gateway PARAMS through Remote."
  (let* ((file
          (my/noema--external-file
           (alist-get 'file params)))
         (content
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-substring-no-properties
             (point-min) (point-max)))))
    (my/noema--ensure-external-file-watch file)
    (append
     `((file . ,file) (content . ,content))
     (my/noema--external-file-metadata file))))

(defun my/noema--external-file-write (params _client)
  "Atomically write a logical Markdown file described by gateway PARAMS."
  (let* ((file
          (my/noema--external-file
           (alist-get 'file params)))
         (content (format "%s" (or (alist-get 'content params) "")))
         (force (eq (alist-get 'force params) t))
         (base-mtime (alist-get 'baseMtimeMs params))
         (metadata (my/noema--external-file-metadata file))
         (mtime (alist-get 'mtimeMs metadata))
         (size (alist-get 'size metadata)))
    (cond
     ((and (not force)
           (numberp base-mtime)
           (> base-mtime 0)
           (> (abs (- mtime base-mtime)) 1))
      `((ok . :json-false) (conflict . t)
        (file . ,file)
        (message . "File changed on the remote target. Review before overwriting.")
        (mtimeMs . ,mtime) (size . ,size)))
     ((and
       (not force)
       (string-empty-p (string-trim content))
       (with-temp-buffer
         (insert-file-contents file)
         (not
          (string-empty-p
           (string-trim
            (buffer-substring-no-properties
             (point-min) (point-max)))))))
      `((ok . :json-false) (conflict . :json-false)
        (file . ,file)
        (message
         . "Refusing to save empty content over a non-empty remote file.")
        (mtimeMs . ,mtime) (size . ,size)))
     (t
      (let* ((default-directory
              (file-name-as-directory (file-name-directory file)))
             (modes (ignore-errors (file-modes file)))
             (temporary (make-nearby-temp-file ".aaronnote-save-")))
        (puthash
         file (+ (float-time) 30)
         my/noema--external-file-watch-suppressed)
        (unwind-protect
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region content nil temporary nil 'silent)
              (rename-file temporary file t)
              (when modes
                (set-file-modes file modes)))
          (when (file-exists-p temporary)
            (ignore-errors (delete-file temporary))))
        (puthash
         file (+ (float-time) 2)
         my/noema--external-file-watch-suppressed)
        (let ((written
               (my/noema--external-file-metadata file)))
          `((ok . t) (conflict . :json-false)
            (file . ,file)
            (mtimeMs . ,(alist-get 'mtimeMs written))
            (size . ,(alist-get 'size written)))))))))

(defvar my/noema--host-event-queue nil
  "FIFO of Noema events acknowledged but not yet applied by Emacs.")

(defvar my/noema--host-event-timer nil
  "One-shot timer draining `my/noema--host-event-queue'.")

(defun my/noema--drain-host-events ()
  "Apply a bounded FIFO slice after the gateway process filter has returned."
  (setq my/noema--host-event-timer nil)
  (let ((remaining 32))
    (while (and my/noema--host-event-queue (> remaining 0))
      (pcase-let ((`(,function . ,args) (pop my/noema--host-event-queue)))
        (condition-case error
            (apply function args)
          (quit
           (my/noema--record-interrupted-operation
            (format "inbound event %S" function))
           (setq remaining 1))
          (error
           (message "Noema deferred host event failed: %s"
                    (error-message-string error)))))
      (setq remaining (1- remaining))))
  (when my/noema--host-event-queue
    (setq my/noema--host-event-timer
          (run-at-time 0 nil #'my/noema--drain-host-events))))

(defun my/noema--defer-host-event (function &rest args)
  "Queue host event FUNCTION with ARGS and acknowledge its gateway call now."
  (setq my/noema--host-event-queue
        (nconc my/noema--host-event-queue
               (list (cons function (copy-tree args)))))
  (unless (timerp my/noema--host-event-timer)
    (setq my/noema--host-event-timer
          (run-at-time 0 nil #'my/noema--drain-host-events))))

(defun my/noema--gateway-event (params _client)
  "Acknowledge Noema event PARAMS and dispatch it outside the process filter."
  (let* ((type (format "%s" (or (alist-get 'type params) "")))
         (payload (or (alist-get 'payload params) '()))
         (line
          (pcase type
            ("ui-state"
             (my/noema--defer-host-event
              #'my/noema--handle-ui-state-payload payload)
             nil)
            ("ready"
             (format "aaronote-web-host:ready:%s"
                     (or (alist-get 'port payload) 0)))
            ("goto"
             (format "aaronote-event:goto:%s:%s"
                     (or (alist-get 'line payload) 0)
                     (or (alist-get 'col payload) 0)))
            ;; Key events are already structured gateway data.  Dispatch them
            ;; directly instead of serializing and parsing them again: that
            ;; round trip can reject modifier payloads containing non-text
            ;; sentinel values as invalid UTF-8.
            ("key"
             (let ((key (alist-get 'key payload))
                   (client (alist-get 'client payload)))
               (when (stringp key)
                 (my/noema--defer-host-event
                  #'my/noema--run-emacs-key key client)))
             nil)
            ("input-focus"
             (my/noema--defer-host-event
              #'my/noema--handle-input-focus payload)
             nil)
            ("jupyter-session"
             (my/noema--defer-host-event
              #'my/noema-jupyter-cell-handle-session-event payload)
             nil)
            ((or "open" "system-open" "zotero" "zotero-import"
                 "current-file" "saved")
             (format "aaronote-event:%s:%s"
                     type (json-serialize payload)))
            (_ nil))))
    (when line
      (my/noema--defer-host-event #'my/noema--handle-process-line line))
    '((ok . t))))

(remote-gateway-register-method
 "aaronnote.event" #'my/noema--gateway-event)
(remote-gateway-register-method
 "aaronnote.file.read" #'my/noema--external-file-read)
(remote-gateway-register-method
 "aaronnote.file.write" #'my/noema--external-file-write)

(defun my/noema--clear-process-log-queue ()
  "Cancel and discard deferred web-host diagnostic output."
  (when (timerp my/noema--process-log-timer)
    (cancel-timer my/noema--process-log-timer))
  (setq my/noema--process-log-timer nil
        my/noema--process-log-queue nil
        my/noema--process-log-bytes 0))

(defun my/noema--flush-process-log ()
  "Append queued diagnostics after the child-process filter has returned."
  (setq my/noema--process-log-timer nil)
  (let ((entries (nreverse my/noema--process-log-queue))
        touched)
    (setq my/noema--process-log-queue nil
          my/noema--process-log-bytes 0)
    (dolist (entry entries)
      (let* ((proc (car entry))
             (buffer (and (processp proc) (process-buffer proc))))
        (when (buffer-live-p buffer)
          (condition-case nil
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert (cdr entry)))
            (quit
             (my/noema--record-interrupted-operation
              "deferred diagnostic log flush")))
          (cl-pushnew buffer touched))))
    (dolist (buffer touched)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Keep only a recent diagnostic tail.  Trimming once per deferred
          ;; batch avoids doing buffer edits and line scans in a process
          ;; filter while the user is typing in an xwidget.
          (when (> (point-max) 204800)
            (goto-char (- (point-max) 102400))
            (forward-line 1)
            (delete-region (point-min) (point))))))))

(defun my/noema--process-filter (proc output)
  "Queue diagnostic web-host OUTPUT without editing buffers in the filter."
  (when (and (stringp output)
             (> (length output) 0)
             (buffer-live-p (process-buffer proc)))
    (let ((bytes (string-bytes output)))
      ;; Diagnostics must never form unbounded backpressure.  If the child
      ;; produces more than one deferred batch can retain, discard the older
      ;; diagnostic-only chunks and preserve a visible overflow marker plus
      ;; the newest output.  Runtime messages travel over the gateway, not
      ;; this log stream.
      (when (> (+ my/noema--process-log-bytes bytes)
               my/noema--process-log-queue-limit)
        (setq my/noema--process-log-queue
              (list (cons proc "\n[Noema diagnostic burst truncated]\n"))
              my/noema--process-log-bytes 36))
      (when (> bytes my/noema--process-log-queue-limit)
        (setq output
              (substring output
                         (max 0 (- (length output)
                                   (/ my/noema--process-log-queue-limit 2)))))
        (setq bytes (string-bytes output)))
      (push (cons proc output) my/noema--process-log-queue)
      (cl-incf my/noema--process-log-bytes bytes))
    (unless (timerp my/noema--process-log-timer)
      (setq my/noema--process-log-timer
            (run-at-time 0 nil #'my/noema--flush-process-log))))
  nil)

(defun my/noema--sentinel (proc event)
  "Handle web-host PROC state change EVENT."
  (when (and (eq proc my/noema--process)
             (not (process-live-p proc)))
    (when my/noema--ready-watchdog
      (cancel-timer my/noema--ready-watchdog)
      (setq my/noema--ready-watchdog nil))
    (when my/noema--goto-timer
      (cancel-timer my/noema--goto-timer)
      (setq my/noema--goto-timer nil))
    (let ((dropped (length my/noema--ready-callbacks)))
      (setq my/noema--goto-last nil
            my/noema--process nil
            my/noema--port nil
            my/noema--ready nil
            my/noema--ready-callbacks nil)
      ;; Anything queued behind `my/noema--ensure-server' -- a run, an
      ;; interrupt, a kernel restart -- dies with the host.  Dropping those
      ;; silently is what makes the UI look like it simply ignored the key.
      (unless (zerop dropped)
        (message "Noema web-host stopped with %d pending request%s; they were dropped"
                 dropped (if (= dropped 1) "" "s")))
      ;; Its gateway binding outlives the process otherwise, so a restart
      ;; cannot reclaim it.
      (when my/noema--gateway-binding
        (ignore-errors
          (remote-gateway-release-binding my/noema--gateway-binding t))
        (setq my/noema--gateway-binding nil))
      (unless (string-match-p "^finished" event)
        (message "Noema web-host: %s" (string-trim event))))))

(defun my/noema-buffer-file (&optional buffer)
  "Return the Noema note file represented by BUFFER.
When BUFFER is nil, inspect the current buffer."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (and (stringp my/noema-buffer-file-name)
           (not (string-empty-p my/noema-buffer-file-name))
           my/noema-buffer-file-name))))

(defun my/noema--buffer-display-name (&optional file)
  "Return the preferred Noema buffer display name for FILE."
  (if-let* ((file (my/noema--canonical-file file)))
      (format "*Noema: %s*" (file-name-nondirectory file))
    "*Noema*"))

(defun my/noema--split-buffer-display-name (file ordinal)
  "Return an ibuffer-friendly name for FILE's split ORDINAL."
  (format "*Noema split %d: %s*"
          ordinal
          (if-let* ((file (my/noema--canonical-file file)))
              (file-name-nondirectory file)
            "Noema")))

(defun my/noema--split-client-ordinal (client)
  "Return split ordinal encoded in CLIENT, or nil."
  (when (my/noema--split-client-p client)
    (let ((value (car (last (split-string client ":" t)))))
      (when (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)))))

(defun my/noema--rename-live-buffers ()
  "Refresh user-visible names of live Noema xwidget buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (eq major-mode 'xwidget-webkit-mode)
                   (or my/noema-buffer-file-name my/noema--client-id))
          (let ((name
                 (if (my/noema--split-client-p my/noema--client-id)
                     (my/noema--split-buffer-display-name
                      my/noema-buffer-file-name
                      (or (my/noema--split-client-ordinal
                           my/noema--client-id)
                          0))
                   (my/noema--buffer-display-name
                    my/noema-buffer-file-name))))
            (setq-local my/noema--xwidget-forced-name name)
            (unless (equal (buffer-name) name)
              (rename-buffer name t))))))))

(defun my/noema--notify-client-closed (&optional client file)
  "Notify the Noema core that CLIENT no longer has a live view."
  (when (and (stringp client) (not (string-empty-p client)))
    (condition-case nil
        (my/noema--post
         `((type . "client-close")
           (client . ,client)
           ,@(when (and (stringp file) (not (string-empty-p file)))
               `((file . ,(my/noema--host-file file))))))
      (error nil))))

(defun my/noema--cleanup-buffer ()
  "Remove the current buffer from Noema identity registries."
  (my/noema--notify-client-closed
   my/noema--client-id
   my/noema-buffer-file-name)
  (when (and (stringp my/noema--registered-file)
             (eq (gethash my/noema--registered-file my/noema--file-buffers)
                 (current-buffer)))
    (remhash my/noema--registered-file my/noema--file-buffers))
  (when (and (stringp my/noema--client-id)
             (eq (gethash my/noema--client-id my/noema--client-buffers)
                 (current-buffer)))
    (remhash my/noema--client-id my/noema--client-buffers))
  (when (eq my/noema--app-buffer (current-buffer))
    (setq my/noema--app-buffer nil)))

(defun my/noema--refresh-visible-ibuffers ()
  "Refresh visible ibuffer buffers after Noema identity changes."
  (when (fboundp 'ibuffer-update)
    (dolist (buffer (buffer-list))
      (when (get-buffer-window buffer 'visible)
        (with-current-buffer buffer
          (when (derived-mode-p 'ibuffer-mode)
            (let ((inhibit-message t))
              (revert-buffer nil t))))))))

(defun my/noema--buffer-for-client (client)
  "Return the live Noema buffer for CLIENT, or nil."
  (when (and (stringp client) (not (string-empty-p client)))
    (let ((buffer (gethash client my/noema--client-buffers)))
      (unless (or (null buffer) (buffer-live-p buffer))
        (remhash client my/noema--client-buffers)
        (setq buffer nil))
      (or buffer
          (cl-find-if
           (lambda (buf)
             (and (buffer-live-p buf)
                  (with-current-buffer buf
                    (and (eq major-mode 'xwidget-webkit-mode)
                         (stringp my/noema--client-id)
                         (string-equal my/noema--client-id client)))))
           (buffer-list))))))

(defun my/noema--register-buffer (buffer file &optional client rename)
  "Register BUFFER as the Noema browser for FILE and CLIENT.
When RENAME is non-nil, rename xwidget buffers to a note-specific name."
  (when (buffer-live-p buffer)
    (let* ((file (my/noema--canonical-file file))
           (client (and (stringp client)
                        (not (string-empty-p client))
                        client))
           (split-client (my/noema--split-client-p client))
           changed)
      (with-current-buffer buffer
        (let ((old-file my/noema--registered-file)
              (old-client my/noema--client-id))
          (when (and (stringp old-file)
                     (not (equal old-file file))
                     (eq (gethash old-file my/noema--file-buffers) buffer))
            (remhash old-file my/noema--file-buffers))
          (when (and (stringp old-client)
                     (not (equal old-client client))
                     (eq (gethash old-client my/noema--client-buffers) buffer))
            (remhash old-client my/noema--client-buffers))
          (setq changed (or (not (equal my/noema-buffer-file-name file))
                            (not (equal my/noema--client-id client)))))
        (setq-local my/noema-buffer-file-name file)
        (setq-local my/noema--registered-file file)
        (setq-local my/noema--client-id client)
        (setq-local my/noema--xwidget-forced-name
                    (if split-client
                        (my/noema--split-buffer-display-name
                         file
                         (or (my/noema--split-client-ordinal client) 0))
                      (my/noema--buffer-display-name file)))
        (my/noema--harden-xwidget-placeholder)
        (my/noema-keys-mode 1)
        (my/noema--sync-xwidget-recovery-mode)
        (when file
          (setq-local default-directory
                      (file-name-as-directory (file-name-directory file)))
          ;; Xwidget buffers acquire their project directory after their
          ;; major-mode hooks have run.  Notify the environment integration at
          ;; the point where the directory actually becomes authoritative.
          (when (fboundp 'my/direnv-schedule-current-buffer)
            (my/direnv-schedule-current-buffer)))
        (add-hook 'kill-buffer-hook #'my/noema--cleanup-buffer nil t)
        (when (and rename
                   (eq major-mode 'xwidget-webkit-mode)
                   (not (equal (buffer-name)
                               my/noema--xwidget-forced-name)))
          (rename-buffer my/noema--xwidget-forced-name t)
          (setq changed t))
        (when changed
          (force-mode-line-update)
          (force-window-update (current-buffer))))
      (when (and file (not (my/noema--split-client-p client)))
        (puthash file buffer my/noema--file-buffers))
      (when client
        (puthash client buffer my/noema--client-buffers))
      (when (and (eq major-mode 'xwidget-webkit-mode)
                 (fboundp 'my/noema--setup-native-chrome))
        (my/noema--setup-native-chrome))
      (when changed
        (my/noema--refresh-visible-ibuffers))
      buffer)))

(defun my/noema--sync-app-buffer-file (file &optional client)
  "Record FILE as the current note in the matching Noema buffer.
CLIENT, when present, identifies the exact xwidget page that reported the
file switch."
  (let* ((file (my/noema--canonical-file file))
         (target (or (my/noema--buffer-for-client client)
                     (and file (my/noema--buffer-for-file file))
                     my/noema--app-buffer)))
    (when (buffer-live-p target)
      (my/noema--register-buffer target file client t)
      (with-current-buffer target
        (setq-local my/noema--activity-paused :unknown))
      (when file
        (setq my/noema--app-buffer target))
      (when my/noema--activity-hooks-installed
        (setq my/noema--last-activity-signature :unknown)
        (my/noema--update-activity)))))

(defun my/noema--track-app-buffer (buffer &optional file client)
  "Record BUFFER as the active Noema browser buffer.
When FILE is non-nil, set buffer-local file tracking directly."
  (setq my/noema--app-buffer buffer)
  (when (buffer-live-p buffer)
    (my/noema--register-buffer buffer file client t)))

(defun my/noema--buffer-for-file (file)
  "Return a live Noema buffer tracking FILE, or nil."
  (when-let* ((abs (my/noema--canonical-file file)))
    (let ((registered (gethash abs my/noema--file-buffers)))
      (cond
       ((buffer-live-p registered) registered)
       (registered
        (remhash abs my/noema--file-buffers)
        nil)
       (t
        (when-let* ((found
                     (cl-find-if
                      (lambda (buf)
                        (and (buffer-live-p buf)
                             (with-current-buffer buf
                               (and (stringp my/noema-buffer-file-name)
                                    (not (my/noema--split-client-p
                                          my/noema--client-id))
                                    (string-equal
                                     (expand-file-name my/noema-buffer-file-name)
                                     abs)))))
                      (buffer-list))))
          (puthash abs found my/noema--file-buffers)
          found))))))

(defun my/noema-canonical-buffer (&optional buffer)
  "Return the canonical Noema buffer for BUFFER's file, or BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (or (when-let* ((file (my/noema-buffer-file buffer)))
            (my/noema--buffer-for-file file))
          buffer))))

(defun my/noema--open-xwidget (url &optional file)
  "Open Noema in a per-file xwidget session.
Each Markdown FILE gets its own dedicated xwidget session and buffer.
Switching to an already-open file reuses the existing buffer without
reloading.  Non-file opens (roam graph, etc.) share the singleton
\"aaronnote\" session."
  (unless (fboundp 'my/xwidget-open-url)
    (require 'init-browser))
  (let* ((file (my/noema--canonical-file file))
         (id (my/noema--xwidget-session-id file))
         (url (if file
                  (my/noema--app-url file id)
                url))
         (existing (or (and file (my/noema--buffer-for-file file))
                       (and (fboundp 'my/xwidget-session-buffer)
                            (my/xwidget-session-buffer id)))))
    (if existing
        ;; Session already alive for this file: switch to it without reloading.
        (progn
          (switch-to-buffer existing)
          (with-current-buffer existing
            (when (fboundp 'my/xwidget-setup-control-line)
              (my/xwidget-setup-control-line)))
          (run-at-time 0.3 nil #'my/noema--focus-xwidget-buffer existing)
          (my/noema--track-app-buffer existing file id)
          existing)
      ;; New session: open directly at the target URL.
      (let ((buffer (my/xwidget-open-url url
                                         :id id
                                         :display 'current
                                         :reuse-selected t)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'my/xwidget-setup-control-line)
              (my/xwidget-setup-control-line))))
        (my/noema--track-app-buffer buffer file id)
        buffer))))

(defun my/noema--open-appine (url &optional file force-new)
  "Open Noema URL in Appine, one Appine tab per md file.
If a tab with URL already exists, switch to it; otherwise open a new tab.

With FORCE-NEW non-nil, always open a fresh native tab.  Singleton pages
like the roam graph use this: their native tab may have been closed via the
Appine toolbar (which bypasses `appine-close-tab' and leaves the Emacs-side
tab registry stale), so trusting a remembered index would silently no-op."
  (unless (fboundp 'my/appine-open-url)
    (require 'init-appine))
  (let* ((norm-url (and (fboundp 'my/appine--normalize-url)
                        (my/appine--normalize-url url)))
         (existing-idx (and (not force-new) norm-url
                            (cl-position norm-url my/appine-tab-list :test #'equal)))
         (buffer (get-buffer-create "*Appine Window*")))
    (my/noema--track-app-buffer buffer file)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local header-line-format '(:eval (my/noema--header-line)))
      (setq-local cursor-type nil)
      (setq buffer-read-only t))
    (set-window-buffer (selected-window) buffer)
    ;; When forcing a fresh tab, drop any stale registry entry for this URL so
    ;; repeated opens (after a native toolbar close) do not accumulate.
    (when (and force-new norm-url (fboundp 'my/appine--tab-forget))
      (my/appine--tab-forget norm-url))
    (let ((is-new (not existing-idx)))
      (if existing-idx
          (when (fboundp 'my/appine--switch-to-tab-index)
            (my/appine--switch-to-tab-index existing-idx))
        (with-current-buffer buffer
          (if (and force-new (fboundp 'my/appine-open-url-fresh))
              (my/appine-open-url-fresh url)
            (my/appine-open-url url))))
      (when (fboundp 'appine-focus)
        (run-at-time (if is-new 0.4 0.05) nil
                     (lambda ()
                       (when (get-buffer-window buffer 'visible)
                         (ignore-errors (appine-focus)))))))))

(defun my/noema--appine-available-p ()
  "Return non-nil when Noema can dispatch opens through Appine."
  (condition-case err
      (progn
        (unless (fboundp 'my/appine-open-url)
          (require 'init-appine))
        (fboundp 'my/appine-open-url))
    (error
     (message "Noema: Appine unavailable (%s)"
              (error-message-string err))
     nil)))

(defun my/noema--open-url (url &optional file force-new)
  "Open Noema URL using `my/noema-backend'.
FORCE-NEW, when non-nil, asks the Appine backend for a fresh tab instead of
reusing a remembered one."
  (pcase my/noema-backend
    ('appine
     (if (my/noema--appine-available-p)
         (my/noema--open-appine url file force-new)
       (message "Noema: using xwidget because Appine is unavailable")
       (my/noema--open-xwidget url file)))
    ('xwidget (my/noema--open-xwidget url file))
    (_ (user-error "Unsupported Noema backend: %S" my/noema-backend))))

(defvar my/noema--post-queue nil
  "FIFO of notification payloads waiting to leave the Emacs command loop.")

(defvar my/noema--post-timer nil
  "One-shot timer draining `my/noema--post-queue'.")

(defvar my/noema--post-awaiting-ready nil
  "Non-nil while the notification queue is waiting for web-host readiness.")

(defconst my/noema--post-queue-limit 512
  "Maximum pending Noema notifications retained while its host is unavailable.")

(defun my/noema--post-now (payload)
  "Write one small control PAYLOAD to an already-ready Noema web-host."
  (when-let* ((client
               (and my/noema--ready
                    (remote-gateway-find-client "aaronnote"))))
    (remote-gateway-notify client "aaronnote.command" payload)
    t))

(defun my/noema--schedule-post-drain ()
  "Schedule one notification-queue drain without stacking timers."
  (unless (timerp my/noema--post-timer)
    (setq my/noema--post-timer
          (run-at-time 0 nil #'my/noema--drain-post-queue))))

(defun my/noema--clear-post-queue ()
  "Cancel and forget every pending Noema notification or inbound event."
  (when (timerp my/noema--post-timer)
    (cancel-timer my/noema--post-timer))
  (when (timerp my/noema--host-event-timer)
    (cancel-timer my/noema--host-event-timer))
  (setq my/noema--post-timer nil
        my/noema--post-queue nil
        my/noema--post-awaiting-ready nil
        my/noema--host-event-timer nil
        my/noema--host-event-queue nil))

(defun my/noema--drain-post-queue ()
  "Drain a bounded slice of queued Noema notifications."
  (setq my/noema--post-timer nil)
  (cond
   ((null my/noema--post-queue)
    (setq my/noema--post-awaiting-ready nil))
   ((not (and my/noema--ready
              (remote-gateway-find-client "aaronnote")))
    (unless my/noema--post-awaiting-ready
      (setq my/noema--post-awaiting-ready t)
      (my/noema--ensure-server
       (lambda ()
         (setq my/noema--post-awaiting-ready nil)
         (my/noema--schedule-post-drain)))))
   (t
    ;; Keep a burst of shell activity/focus notifications from monopolizing
    ;; one Emacs command-loop turn. A local gateway write is notification-only;
    ;; no response is registered or awaited.
    (let ((remaining 32))
      (while (and my/noema--post-queue (> remaining 0))
        (let ((payload (pop my/noema--post-queue)))
          (condition-case error
              (my/noema--post-now payload)
            (quit
             (my/noema--record-interrupted-operation
              (format "outbound notification %s"
                      (or (alist-get 'command payload)
                          (alist-get 'type payload)
                          "unknown")))
             (setq remaining 1))
            (error
             (message "Noema notification failed: %s"
                      (error-message-string error)))))
        (setq remaining (1- remaining))))
    (when my/noema--post-queue
      (my/noema--schedule-post-drain)))))

(defun my/noema--post (payload)
  "Queue control PAYLOAD without doing socket I/O in the caller's stack."
  (let* ((command (alist-get 'command payload))
         (client (or (alist-get 'client payload) ""))
         (replaceable
          (cond
           ((member command '("pause" "resume")) (list client 'activity))
           ((equal command "focus") (list client 'focus)))))
    (when replaceable
      (setq my/noema--post-queue
            (cl-delete-if
             (lambda (queued)
               (let ((queued-command (alist-get 'command queued))
                     (queued-client (or (alist-get 'client queued) "")))
                 (equal replaceable
                        (cond
                         ((member queued-command '("pause" "resume"))
                          (list queued-client 'activity))
                         ((equal queued-command "focus")
                          (list queued-client 'focus))))))
             my/noema--post-queue))))
  (setq my/noema--post-queue
        (nconc my/noema--post-queue (list (copy-tree payload))))
  (when (> (length my/noema--post-queue) my/noema--post-queue-limit)
    (setq my/noema--post-queue
          (last my/noema--post-queue my/noema--post-queue-limit)))
  (my/noema--schedule-post-drain)
  t)

(defun my/noema--open-file-in-web (file)
  "Ask the already open Noema page to open FILE."
  (my/noema--sync-app-buffer-file file)
  (my/noema--post
   `((type . "open") (file . ,(my/noema--host-file file)))))

(defun my/noema--send-command (command &optional detail)
  "Dispatch Noema COMMAND with optional DETAIL."
  (let ((client (and (boundp 'my/noema--client-id)
                     (stringp my/noema--client-id)
                     (not (string-empty-p my/noema--client-id))
                     my/noema--client-id)))
    (my/noema--post
     `((type . "command")
       (command . ,command)
       ,@(when client `((client . ,client)))
       ,@(when detail `((detail . ,detail)))))))

(defun my/noema--goto-location (file line col)
  "Open FILE in Emacs and move to one-based LINE and zero-based COL.
When FILE is nil, use the current buffer."
  (let* ((abs (and (stringp file)
                   (not (string-empty-p file))
                   (ignore-errors
                     (my/noema--canonical-file
                      (file-truename
                       (my/noema--canonical-file file))))))
         (key (list abs (truncate (or line 1)) (truncate (or col 0)))))
    (let ((same-location (equal key my/noema--goto-last))
          (buffer (if abs
                      (or (find-buffer-visiting abs)
                          (find-file-noselect abs))
                    (current-buffer))))
      (setq my/noema--goto-last key)
      (when (buffer-live-p buffer)
        (let ((window (or (get-buffer-window buffer t)
                          (display-buffer buffer))))
          (when (window-live-p window)
            (select-window window)))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (max 0 (1- (truncate (or line 1)))))
            (forward-char (min (max 0 (truncate (or col 0)))
                               (- (line-end-position) (point)))))
          (unless same-location
            (when (require 'pulse nil t)
              (pulse-momentary-highlight-one-line (point)))))))))

;;;###autoload
(defun my/noema-open-file (file)
  "Open Markdown FILE in Noema Web/Appine."
  (interactive "fMarkdown file: ")
  (unless (my/noema--markdown-file-p file)
    (user-error "Noema opens Markdown files, not %s" file))
  (let ((file (my/noema--canonical-file file))
        (target-window (selected-window)))
    (my/noema--ensure-server
     (lambda ()
      (when (window-live-p target-window)
        (select-window target-window))
      (my/noema--open-url
       (my/noema--app-url file (my/noema--xwidget-session-id file))
       file
       nil)))))

;;;###autoload
(defun my/noema-open-current-note ()
  "Open the current Markdown note in Noema Web/Appine."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (my/noema-open-file buffer-file-name))

(defun my/noema--current-note-file ()
  "Return the Markdown note represented by the current context."
  (or (my/noema-buffer-file)
      (and buffer-file-name
           (my/noema--markdown-file-p buffer-file-name)
           buffer-file-name)))

(defun my/noema--split-window (&optional direction)
  "Create and select the window for an Noema split."
  (let* ((direction (or direction my/noema--split-direction
                        (if (>= (window-total-width) 120) 'right 'below)))
         (window (if (eq direction 'below)
                     (split-window-below)
                   (split-window-right))))
    (select-window window)
    window))

;;;###autoload
(defun my/noema-open-current-note-split ()
  "Open the current Markdown note in a fresh editable Noema xwidget split.

This intentionally does not reuse the canonical Noema xwidget for the
file.  Multiple xwidget windows for the same live session have rendering
issues, so this command creates an isolated editable client while keeping the
normal file/session reuse map owned by the canonical pane."
  (interactive)
  (let ((file (my/noema--current-note-file)))
    (unless (and file (my/noema--markdown-file-p file))
      (user-error "No current Markdown note for Noema"))
    (let ((file (my/noema--canonical-file file))
          (source-window (selected-window)))
      (my/noema--ensure-server
       (lambda ()
         (when (window-live-p source-window)
           (select-window source-window))
         (unless (fboundp 'my/xwidget-open-url)
           (require 'init-browser))
         (let* ((ordinal (cl-incf my/noema--split-counter))
                (client (format "aaronnote-split:%s:%d"
                                (file-truename file)
                                ordinal))
                (url (my/noema--app-url file client))
                (target-window (my/noema--split-window))
                (buffer (my/xwidget-open-url
                         url
                         :id client
                         :display 'current
                         :force-new t
                         :reuse-selected t)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq-local my/noema-buffer-file-name file)
               (setq-local my/noema--client-id client)
               (setq-local my/noema--registered-file nil)
               (setq-local my/noema--xwidget-forced-name
                           (my/noema--split-buffer-display-name
                            file ordinal))
               (puthash client (current-buffer) my/noema--client-buffers)
               (add-hook 'kill-buffer-hook #'my/noema--cleanup-buffer nil t)
               (when (fboundp 'my/xwidget-setup-control-line)
                 (my/xwidget-setup-control-line))
               ;; `xwidget-webkit-browse-url' may return before its buffer has
               ;; finished switching to `xwidget-webkit-mode'.  Naming does
               ;; not depend on the major mode, and delaying it leaves the
               ;; buffer permanently named *xwidget* because the title
               ;; callback correctly avoids overriding Noema-owned names.
               (rename-buffer my/noema--xwidget-forced-name t)
               (when file
                 (setq-local default-directory
                             (file-name-as-directory (file-name-directory file)))
                 (when (fboundp 'my/direnv-schedule-current-buffer)
                   (my/direnv-schedule-current-buffer)))
               (my/noema--harden-xwidget-placeholder)
               (my/noema-keys-mode 1)
               (my/noema--sync-xwidget-recovery-mode)))
           (my/noema--refresh-visible-ibuffers)
           (when (window-live-p target-window)
             (select-window target-window))))))))

;;;###autoload
(defun my/noema-open-current-note-split-right ()
  "Open the current note in a fresh Noema pane to the right."
  (interactive)
  (let ((my/noema--split-direction 'right))
    (my/noema-open-current-note-split)))

;;;###autoload
(defun my/noema-open-current-note-split-below ()
  "Open the current note in a fresh Noema pane below."
  (interactive)
  (let ((my/noema--split-direction 'below))
    (my/noema-open-current-note-split)))

;;;###autoload
(defun my/noema-preview ()
  "Compatibility alias: open the current note in Noema."
  (interactive)
  (my/noema-open-current-note))

;;;###autoload
(defun my/noema-sync-cursor ()
  "Open the current note in Noema.
Cursor-level sync is intentionally no longer a per-keystroke preview channel."
  (interactive)
  (my/noema-open-current-note))

;;;###autoload
(defun my/noema-refresh ()
  "Refresh the current Noema note while preserving page cursor state."
  (interactive)
  (if (and my/noema--ready
           (or (and (boundp 'my/noema--client-id)
                    (stringp my/noema--client-id)
                    (not (string-empty-p my/noema--client-id)))
               (buffer-live-p my/noema--app-buffer)))
      (progn
        (my/noema-command "refresh"))
    (my/noema-open-current-note)))

;;;###autoload
(defun my/noema-command (command &optional detail)
  "Queue COMMAND with optional DETAIL for the open Noema page.
This function never starts the host, writes a socket, evaluates JavaScript or
waits for a response in the invoking Emacs command."
  (interactive "sAaronnote command: ")
  ;; `my/noema--send-command' captures the current pane id into the queued
  ;; payload now; the later timer therefore cannot turn it into a broadcast.
  (my/noema--send-command command detail))

;;;###autoload
(defun my/noema-escape ()
  "Tell Noema to handle Escape."
  (interactive)
  (my/noema-command "escape"))

;;;###autoload
(defun my/noema-save ()
  "Tell Noema to save the current note."
  (interactive)
  (my/noema-command "save"))

;;;###autoload
(defun my/noema-focus ()
  "Tell Noema to focus its editor."
  (interactive)
  (my/noema-command "focus"))

(defun my/noema-open-wiki-view (&optional view query)
  "Open canonical Wiki VIEW with optional additional QUERY parameters."
  (my/noema--ensure-server
   (lambda ()
     (let ((path (concat "/wiki"
                         (when (or view query)
                           (concat "?"
                                   (mapconcat
                                    #'identity
                                    (delq nil
                                          (list (and view (format "view=%s" view))
                                                query))
                                    "&"))))))
       (my/noema--open-url (my/noema--server-url path) nil nil)))))

(defmacro my/noema--def-wiki-view (name view doc)
  "Define NAME to open canonical Wiki VIEW with DOC."
  `(defun ,name ()
     ,doc
     (interactive)
     (my/noema-open-wiki-view ,view)))

(my/noema--def-wiki-view my/noema-wiki-home nil "Open the Noema Wiki home.")
(my/noema--def-wiki-view my/noema-wiki-pages "pages" "Open the canonical Wiki page browser.")
(my/noema--def-wiki-view my/noema-wiki-recent "recent" "Open recently changed Wiki pages.")
(my/noema--def-wiki-view my/noema-wiki-tags "tags" "Open canonical Wiki tag management.")
(my/noema--def-wiki-view my/noema-wiki-namespaces "namespaces" "Open canonical Wiki namespaces.")
(my/noema--def-wiki-view my/noema-wiki-repositories "repositories" "Open canonical Wiki repository management.")
(my/noema--def-wiki-view my/noema-wiki-sync "sync" "Open canonical Wiki synchronization.")
(my/noema--def-wiki-view my/noema-wiki-reports "reports" "Open canonical Wiki reports.")
(my/noema--def-wiki-view my/noema-wiki-wanted "wanted" "Open canonical Wiki wanted-pages report.")

(defun my/noema-wiki-new-page ()
  "Open Noema's canonical new-page flow."
  (interactive)
  (my/noema-open-wiki-view nil "new=1"))

;;;###autoload
(defun my/noema-workspace-graph ()
  "Open the shared interactive workspace graph."
  (interactive)
  (my/noema-open-wiki-view "graph"))

;;;###autoload
(defun my/noema-roam-graph ()
  "Open Noema's canonical workspace graph."
  (interactive)
  (my/noema-workspace-graph))

;;; Pause/resume — report host activity to the shared renderer gate.

(defvar my/noema--paused nil
  "Non-nil when the browser page has been sent a pause command.")
(defvar my/noema--manual-paused nil
  "Non-nil when Noema was paused explicitly by the user.")
(defvar my/noema--activity-timer nil
  "Debounce timer for `my/noema--update-activity'.")
(defvar my/noema--activity-hooks-installed nil
  "Non-nil when Noema pause/resume activity hooks are installed.")
(defvar my/noema--last-activity-signature :unknown
  "Last per-client activity snapshot scheduled by Noema.")
(defvar-local my/noema--activity-paused :unknown
  "Last pause state sent to this Noema renderer client.")
(put 'my/noema--activity-paused 'permanent-local t)

(defconst my/noema--core-ready-script
  "(() => {
  const connection = window.aaronnoteApi && window.aaronnoteApi.connection;
  if (!connection || typeof connection.reconnect !== 'function') return false;
  connection.reconnect('host-ready');
  return true;
})()"
  "JavaScript used to reconnect a retained xwidget page after core restarts.")

(defun my/noema--browser-buffers ()
  "Return registered live Noema browser buffers without scanning all buffers."
  (let (buffers)
    (when (buffer-live-p my/noema--app-buffer)
      (push my/noema--app-buffer buffers))
    (maphash
     (lambda (_client buffer)
       (when (buffer-live-p buffer) (push buffer buffers)))
     my/noema--client-buffers)
    (maphash
     (lambda (_file buffer)
       (when (buffer-live-p buffer) (push buffer buffers)))
     my/noema--file-buffers)
    (delete-dups buffers)))

(defun my/noema--buffer-active-p (buffer)
  "Return non-nil when BUFFER owns the selected window of a focused frame.
A visible but unselected Noema split remains painted and readable, but its
shared renderer activity gate may sleep until that pane is selected again."
  (when (buffer-live-p buffer)
    (cl-some
     (lambda (window)
       (let ((frame (window-frame window)))
         (and (eq (frame-visible-p frame) t)
              (frame-focus-state frame)
              (eq window (frame-selected-window frame)))))
     (get-buffer-window-list buffer nil 'visible))))

(defun my/noema--app-buffer-visible-p ()
  "Return non-nil when the current Noema buffer is the active host pane."
  (my/noema--buffer-active-p my/noema--app-buffer))

(defun my/noema--activity-snapshot ()
  "Return a stable list of each live renderer buffer and its host activity."
  (sort
   (mapcar (lambda (buffer)
             (cons buffer (and (my/noema--buffer-active-p buffer) t)))
           (my/noema--browser-buffers))
   (lambda (left right)
     (string-lessp (buffer-name (car left)) (buffer-name (car right))))))

(defun my/noema--notify-xwidgets-core-ready ()
  "Reconnect retained xwidgets and replay host lifecycle after core restart."
  (when (and (fboundp 'xwidget-webkit-current-session)
             (fboundp 'xwidget-webkit-execute-script))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (my/noema--xwidget-buffer-p buffer))
        (with-current-buffer buffer
          ;; The replacement Node host has no memory of commands accepted by
          ;; its predecessor. Force the next complete activity snapshot to
          ;; replay even when the Emacs-side foreground state did not change.
          (setq-local my/noema--activity-paused :unknown)
          (when-let* ((session (ignore-errors
                                 (xwidget-webkit-current-session))))
            (condition-case nil
                (xwidget-webkit-execute-script
                 session my/noema--core-ready-script)
              (quit
               (my/noema--record-interrupted-operation
                "retained xwidget core reconnect"))
              (error nil))))))
    (when my/noema--activity-hooks-installed
      (setq my/noema--last-activity-signature :unknown)
      (my/noema--update-activity))))

(defun my/noema--maybe-reconnect-core-on-activity ()
  "Restart a disconnected core only while an Noema browser is active.
This is intentionally called from focus/window activity, never from an idle
timer or retry loop. The old port is reclaimed so the browser page and its
unsaved CodeMirror state remain intact."
  (when (and (buffer-live-p my/noema--app-buffer)
             (my/noema--app-buffer-visible-p)
             (not my/noema--ready)
             (not (and (processp my/noema--process)
                       (process-live-p my/noema--process)))
             (integerp my/noema--last-port)
             (> my/noema--last-port 0))
    (unless (memq #'my/noema--notify-xwidgets-core-ready
                  my/noema--ready-callbacks)
      (push #'my/noema--notify-xwidgets-core-ready
            my/noema--ready-callbacks))
    (condition-case err
        (progn
          (my/noema--start-server my/noema--last-port)
          ;; Diagnostic deadline only; it never performs another reconnect.
          (when my/noema--ready-watchdog
            (cancel-timer my/noema--ready-watchdog))
          (setq my/noema--ready-watchdog
                (run-at-time 10 nil #'my/noema--watchdog-fire)))
      (error
       (setq my/noema--ready-callbacks
             (delq #'my/noema--notify-xwidgets-core-ready
                   my/noema--ready-callbacks))
       (message "Noema: active core reconnect failed: %s"
                (error-message-string err))))))

(defun my/noema--apply-activity-snapshot (snapshot)
  "Apply per-client pause states from SNAPSHOT through shared host commands."
  (let ((all-paused (and snapshot t)))
    (dolist (entry snapshot)
      (pcase-let ((`(,buffer . ,active) entry))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let* ((effective-active
                    (and active (not my/noema--manual-paused)))
                   (next-paused (not effective-active)))
              (unless (eq next-paused my/noema--activity-paused)
                (setq-local my/noema--activity-paused next-paused)
                ;; The renderer owns the shared pause state machine.  This
                ;; adapter contributes only its host-window activity fact.
                (my/noema--send-command
                 (if effective-active "resume" "pause")))
              (unless next-paused
                (setq all-paused nil)))))))
    (setq my/noema--paused all-paused)))

(defun my/noema--apply-activity (active)
  "Apply ACTIVE to the current Noema renderer client.
This narrow wrapper is retained for interactive callers and tests; automatic
activity always applies a complete per-client snapshot."
  (let ((buffer (if (memq (current-buffer) (my/noema--browser-buffers))
                    (current-buffer)
                  my/noema--app-buffer)))
    (when (buffer-live-p buffer)
      (my/noema--apply-activity-snapshot (list (cons buffer active))))))

;;;###autoload
(defun my/noema-pause ()
  "Pause shared Noema renderer work until explicitly resumed."
  (interactive)
  (setq my/noema--manual-paused t)
  (my/noema--apply-activity-snapshot (my/noema--activity-snapshot)))

;;;###autoload
(defun my/noema-resume ()
  "Resume only the Noema renderer client in the active host pane."
  (interactive)
  (setq my/noema--manual-paused nil)
  (my/noema--apply-activity-snapshot (my/noema--activity-snapshot)))

;;;###autoload
(defun my/noema-toggle-pause ()
  "Toggle manual pause for Noema's shared renderer activity gate."
  (interactive)
  (if my/noema--manual-paused
      (my/noema-resume)
    (my/noema-pause)))

(defun my/noema--update-activity (&rest _)
  "Coalesce host focus/window changes into per-client pause commands.
No polling timer is installed: Emacs reports only shell-level activity facts,
and every renderer uses the same `runHostCommand' pause implementation."
  (let ((selected (and (window-live-p (selected-window))
                       (window-buffer (selected-window)))))
    (when (memq selected (my/noema--browser-buffers))
      (setq my/noema--app-buffer selected)))
  (my/noema--maybe-reconnect-core-on-activity)
  (let ((snapshot (my/noema--activity-snapshot)))
    (unless (equal snapshot my/noema--last-activity-signature)
      (setq my/noema--last-activity-signature snapshot)
      (when my/noema--activity-timer
        (cancel-timer my/noema--activity-timer))
      ;; Pause quickly when a pane leaves the foreground; allow the focus
      ;; hand-off to settle briefly before resuming WebKit work.
      (setq my/noema--activity-timer
            (run-at-time
             (if (seq-some #'cdr snapshot) 0.12 0.03) nil
             (lambda ()
               (setq my/noema--activity-timer nil)
               (if my/noema--ready
                   (let ((current (my/noema--activity-snapshot)))
                     (setq my/noema--last-activity-signature current)
                     (my/noema--apply-activity-snapshot current))
                 ;; A ready transition must retry even if focus did not change.
                 (setq my/noema--last-activity-signature :unknown))))))))

(defun my/noema--install-activity-hooks ()
  "Install event-driven host activity hooks without a background poller."
  (unless my/noema--activity-hooks-installed
    (add-function :after after-focus-change-function
                  #'my/noema--update-activity)
    (add-hook 'focus-in-hook #'my/noema--update-activity)
    (add-hook 'focus-out-hook #'my/noema--update-activity)
    (add-hook 'window-buffer-change-functions #'my/noema--update-activity)
    (add-hook 'window-selection-change-functions #'my/noema--update-activity)
    (when (boundp 'window-state-change-functions)
      (add-hook 'window-state-change-functions #'my/noema--update-activity))
    (when (boundp 'delete-frame-functions)
      (add-hook 'delete-frame-functions #'my/noema--update-activity))
    (setq my/noema--activity-hooks-installed t)))

(defun my/noema--remove-activity-hooks ()
  "Remove activity hooks and cancel the one pending debounce transition."
  (remove-function after-focus-change-function #'my/noema--update-activity)
  (remove-hook 'focus-in-hook #'my/noema--update-activity)
  (remove-hook 'focus-out-hook #'my/noema--update-activity)
  (remove-hook 'window-buffer-change-functions #'my/noema--update-activity)
  (remove-hook 'window-selection-change-functions #'my/noema--update-activity)
  (when (boundp 'window-state-change-functions)
    (remove-hook 'window-state-change-functions #'my/noema--update-activity))
  (when (boundp 'delete-frame-functions)
    (remove-hook 'delete-frame-functions #'my/noema--update-activity))
  (when my/noema--activity-timer
    (cancel-timer my/noema--activity-timer)
    (setq my/noema--activity-timer nil))
  (dolist (buffer (my/noema--browser-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local my/noema--activity-paused :unknown))))
  (setq my/noema--paused nil
        my/noema--manual-paused nil
        my/noema--last-activity-signature :unknown
        my/noema--activity-hooks-installed nil))

;;;###autoload
(defun my/noema-stop ()
  "Kill the Noema web-host process and reset Appine tab state.
The web-host (Node) is the backend; once it is gone, any Appine tabs showing
its pages are dead, so the Emacs-side tab registry is cleared too."
  (interactive)
  (my/noema--clear-post-queue)
  (my/noema--clear-process-log-queue)
  (my/noema--remove-activity-hooks)
  (when (fboundp 'my/noema-roam--cancel-sync-timer)
    (my/noema-roam--cancel-sync-timer))
  (when my/noema--ready-watchdog
    (cancel-timer my/noema--ready-watchdog)
    (setq my/noema--ready-watchdog nil))
  (when my/noema--goto-timer
    (cancel-timer my/noema--goto-timer)
    (setq my/noema--goto-timer nil
          my/noema--goto-last nil))
  (my/noema--clear-external-file-watches)
  (when my/noema--gateway-binding
    (remote-gateway-release-binding
     my/noema--gateway-binding t)
    (setq my/noema--gateway-binding nil))
  (let ((proc my/noema--process))
    (setq my/noema--process nil
          my/noema--port nil
          my/noema--ready nil
          my/noema--ready-callbacks nil)
    (when (and proc (process-live-p proc))
      (ignore-errors (signal-process proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p proc)
            (delete-process proc))))))
  (when (fboundp 'my/appine--tab-reset)
    (my/appine--tab-reset))
  (message "Noema web-host stopped."))

(defun my/noema--kill-browser-buffers ()
  "Kill Emacs buffers that host Noema browser pages."
  (mapc
   (lambda (buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (or my/noema-buffer-file-name
                   my/noema--client-id
                   (and (derived-mode-p 'xwidget-webkit-mode)
                        (string-prefix-p "*Noema" (buffer-name buffer))))
           (kill-buffer buffer)))))
   (buffer-list))
  (setq my/noema--app-buffer nil)
  (clrhash my/noema--file-buffers)
  (clrhash my/noema--client-buffers))

;;;###autoload
(defun my/noema-close ()
  "Completely close Noema browser surfaces and stop the web-host."
  (interactive)
  (when (fboundp 'my/appine-kill-all)
    (ignore-errors (my/appine-kill-all)))
  (my/noema--kill-browser-buffers)
  (my/noema-stop))

;;;###autoload
(defun my/noema-build-and-reopen ()
  "Build Noema web assets, restart the runtime, and reopen the current note."
  (interactive)
  (when (and my/noema--build-process
             (process-live-p my/noema--build-process))
    (user-error "Noema build is already running"))
  (let* ((file (my/noema--current-note-file))
         (buffer (get-buffer-create "*Noema build*"))
         (default-directory user-emacs-directory))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Noema: building web assets...")
    (setq my/noema--build-process
          (make-process
           :name "aaronnote-build"
           :buffer buffer
           :command '("make" "aaronnote-build")
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((ok (= (process-exit-status proc) 0)))
                 (setq my/noema--build-process nil)
                 (if ok
                     (progn
                       (my/noema-close)
                       (message "Noema: build finished; reopening...")
                       (if (and file (my/noema--markdown-file-p file))
                           (my/noema-open-file file)
                         (my/noema--ensure-server
                          (lambda ()
                            (my/noema--open-url
                             (my/noema--app-url nil "aaronnote") nil t)))))
                   (display-buffer buffer)
                   (message "Noema: build failed; see %s" (buffer-name buffer))))))))
    (display-buffer buffer)))

(add-hook 'kill-emacs-hook #'my/noema-stop)

;;; API call — request the web-host over the shared gateway.

(defun my/noema--gateway-hash-value (value)
  "Convert decoded gateway VALUE into hash-table object representation."
  (cond
   ((and (listp value)
         value
         (cl-every
          (lambda (item)
            (and (consp item) (symbolp (car item))))
          value))
    (let ((table (make-hash-table :test #'equal)))
      (dolist (item value table)
        (puthash
         (symbol-name (car item))
         (my/noema--gateway-hash-value (cdr item))
         table))))
   ((listp value)
    (mapcar #'my/noema--gateway-hash-value value))
   ((vectorp value)
    (vconcat
     (mapcar #'my/noema--gateway-hash-value value)))
   (t value)))

(defcustom my/noema-api-call-timeout 10
  "Seconds to wait for an ordinary asynchronous Noema API reply."
  :type 'number
  :group 'my/noema)

(defun my/noema-api-call (channel args callback &optional timeout)
  "Call CHANNEL with ARGS and invoke CALLBACK with (RESULT ERROR).
Unlike the compatibility wrapper `my/noema--api-call', CALLBACK is also
invoked when Noema is offline or the gateway request fails.

TIMEOUT overrides `my/noema-api-call-timeout'.  Callers whose channel runs
user code — Noema executes a whole run-all before it answers — must pass one,
because the default deadline would report a perfectly healthy run as a
failure while it is still going."
  (if-let* ((client
             (and my/noema--ready
                  (remote-gateway-find-client "aaronnote"))))
      (remote-gateway-request-async
       client "aaronnote.api"
       `((channel . ,channel) (args . ,args))
       (lambda (result error-object)
         (if error-object
             (funcall callback nil error-object)
           (funcall callback result nil)))
       (or timeout my/noema-api-call-timeout))
    (run-at-time
     0 nil callback nil
     '((code . "offline") (message . "Noema web-host is not ready")))))

(defun my/noema--api-call (channel args callback)
  "Compatibility wrapper that invokes CALLBACK only after a successful call."
  (my/noema-api-call
   channel args
   (lambda (result error-object)
     (if error-object
         (message
          "Noema API error %s: %s"
          (or (my/noema-jupyter--get 'code error-object) "unknown")
          (or (my/noema-jupyter--get 'message error-object) "request failed"))
       (funcall callback result)))))

(defun my/noema--activity-client-status ()
  "Return per-renderer host activity diagnostics for runtime status."
  (vconcat
   (mapcar
    (lambda (entry)
      (pcase-let ((`(,buffer . ,active) entry))
        (with-current-buffer buffer
          (let ((status (make-hash-table :test 'equal)))
            (puthash "buffer" (buffer-name buffer) status)
            (puthash "client" my/noema--client-id status)
            (puthash "file" my/noema-buffer-file-name status)
            (puthash "active" (if active t :false) status)
            (puthash "rendererPaused"
                     (cond
                      ((eq my/noema--activity-paused :unknown) "unknown")
                      (my/noema--activity-paused t)
                      (t :false))
                     status)
            status))))
    (my/noema--activity-snapshot))))

(defun my/noema-runtime-status ()
  "Display the Noema runtime debug snapshot."
  (interactive)
  (unless my/noema--ready
    (user-error "Noema web-host is not ready"))
  (message "Noema: loading runtime status…")
  (my/noema-api-call
   "aaronnote:api:runtime:debug" []
   (lambda (result error-object)
     (if error-object
         (message "Noema runtime status unavailable: %s"
                  (or (my/noema-jupyter--get 'message error-object)
                      "request failed"))
       (let ((payload (my/noema--gateway-hash-value result)))
         (puthash "emacsActivity"
                  (let ((activity (make-hash-table :test 'equal)))
                    (puthash "paused" (if my/noema--paused t :false) activity)
                    (puthash "manualPaused" (if my/noema--manual-paused t :false) activity)
                    (puthash "bufferVisible" (if (my/noema--app-buffer-visible-p) t :false) activity)
                    (puthash "clients" (my/noema--activity-client-status) activity)
                    activity)
                  payload)
         (with-current-buffer (get-buffer-create "*Noema runtime status*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (json-serialize payload
                                     :false-object :false
                                     :null-object nil))
             (goto-char (point-min))
             (special-mode))
           (display-buffer (current-buffer))))))))

(defun my/noema-wiki-refresh (&optional full)
  "Refresh wiki.db incrementally, or perform a FULL atomic rebuild."
  (interactive)
  (unless my/noema--ready
    (user-error "Noema: server not running"))
  (let ((mode (if full "full" "incremental")))
    (message "Noema: %s Wiki index..." (if full "rebuilding" "refreshing"))
    (my/noema--api-call
     "aaronnote:api:wiki:refresh"
     (vector (list (cons 'mode mode)))
     (lambda (result)
       (let* ((maintenance (alist-get 'maintenance result))
              (actual-mode (or (alist-get 'mode maintenance) mode))
              (notes (length (or (alist-get 'notes result) nil)))
              (generation (or (alist-get 'generation result) "")))
         (setq my/noema--last-sync-stats
               (format "wiki %s · %d pages · %s"
                       actual-mode notes
                       (if (> (length generation) 8) (substring generation 0 8) generation)))
         (when (fboundp 'my/noema-roam--clear-runtime-cache)
           (my/noema-roam--clear-runtime-cache))
         (message "Noema Wiki index: %s" my/noema--last-sync-stats))))))

(defun my/noema-wiki-rebuild ()
  "Atomically rebuild the canonical wiki.db from all repositories."
  (interactive)
  (my/noema-wiki-refresh t))

(defun my/noema-wiki-index-status ()
  "Display canonical wiki.db maintenance status."
  (interactive)
  (unless my/noema--ready
    (user-error "Noema: server not running"))
  (message "Noema: loading Wiki index status…")
  (my/noema-api-call
   "aaronnote:api:wiki:index-status" []
   (lambda (result error-object)
     (if error-object
         (message "Noema Wiki index status unavailable: %s"
                  (or (my/noema-jupyter--get 'message error-object)
                      "request failed"))
       (let ((payload (my/noema--gateway-hash-value result)))
         (setq my/noema--last-sync-stats
               (format "wiki %s · %s"
                       (or (gethash "lastMode" payload) "not built")
                       (let ((generation (or (gethash "generation" payload) "")))
                         (if (> (length generation) 8)
                             (substring generation 0 8)
                           generation))))
         (with-current-buffer (get-buffer-create "*Noema Wiki index status*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (json-encode payload))
             (json-pretty-print-buffer)
             (goto-char (point-min))
             (special-mode))
           (display-buffer (current-buffer))))))))

;; The old command symbol remains for external keymaps; it no longer has any
;; Roam database implementation behind it.
(defalias 'my/noema-roam-sync #'my/noema-wiki-refresh)

;;; Header-line for the Noema app buffer.

(defun my/noema-editor-menu (event)
  "Open Noema editor actions from the native pencil button at EVENT."
  (interactive "e")
  (my/xwidget--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    "Noema"
    (list
     ["Focus editor" my/noema-focus t]
     ["Task manager" my/xwidget-open-task-manager t]
     "---"
     ["Page outline" my/noema-toggle-page t]
     ["Agenda" my/noema-toggle-agenda t]
     ["Local graph" my/noema-toggle-graph t]
     ["Workspace graph" my/noema-workspace-graph t]
     ["Wiki home" my/noema-wiki-home t]
     ["Tools" my/noema-toggle-tools t]
     ["Jupyter cells" my/noema-toggle-jupyter t]
     "---"
     ["Toggle source" my/noema-toggle-source t]
     ["Run prose check" my/noema-prose-check t]
     ["Export LaTeX…" my/noema-export-latex t]
     ["Settings…" my/noema-settings t]
     ["Move document to Trash" my/noema-trash-current-note t]
     ["Save" my/noema-save t]))
   event))

(dolist (entry '((my/noema-toggle-page . "toggle-toc")
                 (my/noema-toggle-agenda . "toggle-agenda")
                 (my/noema-toggle-graph . "toggle-graph")
                 (my/noema-toggle-tools . "toggle-tools")
                 (my/noema-toggle-jupyter . "jupyter-panel")))
  (let ((fn (car entry)) (command (cdr entry)))
    (fset fn (lambda () (interactive) (my/noema-command command)))))

(defun my/noema--header-browser-buttons ()
  "Return native xwidget controls with Noema actions under the pencil."
  (list
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-arrow_left" "back")
    #'my/xwidget-back "Back [b]" 'header-line)
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-arrow_right" "fwd")
    #'my/xwidget-forward "Forward [f]" 'header-line)
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-refresh" "reload")
    #'my/xwidget-reload "Reload [g]" 'header-line)
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-edit" "edit")
    #'my/noema-editor-menu "Noema actions" 'header-line)
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-layout" "win")
    #'my/xwidget-window-menu "Window menu" 'header-line)))

(defun my/noema--header-line ()
  "Return native browser and editor controls for an Noema buffer."
  (let* ((file (my/noema-buffer-file (current-buffer)))
         (name (if file (file-name-nondirectory file) "Noema")))
    (append
     (list " ")
     (my/noema--header-browser-buttons)
     (list "  " (propertize name 'face 'mode-line-buffer-id)))))

(defun my/noema--setup-native-chrome ()
  "Install Noema-only Emacs chrome in the current xwidget buffer."
  (setq-local header-line-format '(:eval (my/noema--header-line)))
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update t))

(defun my/noema--restore-native-chrome-h ()
  "Restore Noema chrome after generic xwidget mode initialization."
  (when (my/noema--xwidget-buffer-p)
    (my/noema--setup-native-chrome)))

(defun my/noema--restore-native-chrome-later-h ()
  "Restore Noema chrome after all xwidget mode hooks have settled."
  (let ((buffer (current-buffer)))
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (my/noema--restore-native-chrome-h)))))))

(with-eval-after-load 'init-browser
  (advice-add 'my/xwidget-setup-control-line :after
              #'my/noema--restore-native-chrome-h)
  (with-eval-after-load 'xwidget
    (add-hook 'xwidget-webkit-mode-hook
              #'my/noema--restore-native-chrome-later-h 90)))

;;;###autoload
(defun my/noema-pop ()
  "Open the Noema command pop."
  (interactive)
  (require 'transient)
  (call-interactively #'my/noema-dispatch))

(defun my/noema--xwidget-menu-section ()
  "Return Noema actions for the xwidget top-bar popup."
  (when (or my/noema-buffer-file-name
            my/noema--client-id)
    (list
     "---"
     ["Noema: Refresh current pane" my/noema-refresh t]
     ["Noema: Open editable split" my/noema-open-current-note-split t]
     ["Noema: Split right" my/noema-open-current-note-split-right t]
     ["Noema: Split below" my/noema-open-current-note-split-below t]
     ["Noema: Wiki home" my/noema-wiki-home t]
     ["Noema: Workspace graph" my/noema-workspace-graph t]
     ["Noema: Focus editor" my/noema-focus t]
     ["Noema: Pop" my/noema-pop t]
     (list
      "Noema lifecycle"
      ["Build + reopen" my/noema-build-and-reopen t]
      ["Close all Noema" my/noema-close t]))))

(with-eval-after-load 'init-browser
  (add-to-list 'my/xwidget-window-menu-extra-sections
               #'my/noema--xwidget-menu-section))

;;; Web-editor command wrappers.
;; These generate named interactive commands for every web-host editor command
;; so each entry in the dispatch hub is `commandp', appears in M-x, and can
;; be verified with `commandp' in batch tests.

(defmacro my/noema--def-editor-cmd (suffix command &optional doc)
  "Define `my/noema-SUFFIX' that sends editor COMMAND to the web page."
  `(defun ,(intern (format "my/noema-%s" suffix)) ()
     ,(or doc (format "Send the Noema `%s' editor command." command))
     (interactive)
     (my/noema-command ,command)))

(my/noema--def-editor-cmd "toggle-source"   "toggle-source"   "Toggle source / rendered view.")
(my/noema--def-editor-cmd "undo"            "undo"            "Undo last edit in Noema.")
(my/noema--def-editor-cmd "redo"            "redo"            "Redo last undone edit in Noema.")
(my/noema--def-editor-cmd "paste"           "paste"           "Paste through Noema's editor pipeline.")
(my/noema--def-editor-cmd "bold"            "bold"            "Toggle bold at point.")
(my/noema--def-editor-cmd "italic"          "italic"          "Toggle italic at point.")
(my/noema--def-editor-cmd "code-inline"     "code"            "Toggle inline code at point.")
(my/noema--def-editor-cmd "highlight"       "highlight"       "Toggle highlight at point.")
(my/noema--def-editor-cmd "strike"          "strike"          "Toggle strikethrough at point.")
(my/noema--def-editor-cmd "superscript"     "superscript"     "Wrap the selection as Markdown superscript.")
(my/noema--def-editor-cmd "subscript"       "subscript"       "Wrap the selection as Markdown subscript.")
(my/noema--def-editor-cmd "insert-footnote" "insert-footnote" "Insert a numbered Markdown footnote.")
(my/noema--def-editor-cmd "insert-revision" "insert-revision" "Insert an Noema revision suggestion.")
(my/noema--def-editor-cmd "edit-properties" "edit-properties" "Open the native org-meta properties panel.")
(my/noema--def-editor-cmd "move-block-up"   "move-block-up"   "Move the current Markdown block upward.")
(my/noema--def-editor-cmd "move-block-down" "move-block-down" "Move the current Markdown block downward.")
(my/noema--def-editor-cmd "blockquote"      "blockquote"      "Toggle blockquote on paragraph.")
(my/noema--def-editor-cmd "bullet-list"     "bullet-list"     "Toggle bullet list.")
(my/noema--def-editor-cmd "ordered-list"    "ordered-list"    "Toggle ordered list.")
(my/noema--def-editor-cmd "task-list"       "task-list"       "Toggle task/checkbox list.")
(my/noema--def-editor-cmd "code-block"      "code-block"      "Insert/toggle fenced code block.")
(my/noema--def-editor-cmd "paragraph-menu"  "paragraph-menu"  "Open heading/paragraph type menu.")
(my/noema--def-editor-cmd "insert-table"    "insert-table"    "Insert a Markdown table.")
(my/noema--def-editor-cmd "insert-math"     "insert-math-block" "Insert a math block.")
(my/noema--def-editor-cmd "insert-toc"      "insert-toc"      "Insert a table of contents.")
(my/noema--def-editor-cmd "prose-check"     "prose-check"     "Run a bounded LanguageTool check in Noema.")
(my/noema--def-editor-cmd "knowledge-search" "knowledge-search" "Search notes through Noema's unified knowledge index.")
(my/noema--def-editor-cmd "export-latex"    "export-latex"    "Export the current scope to LaTeX.")
(my/noema--def-editor-cmd "settings"        "settings"        "Open Noema settings.")
(my/noema--def-editor-cmd "trash-current-note" "trash-current-note" "Move the current document to recoverable Trash.")

;;; Dispatch transient.

(defun my/noema--dispatch-header ()
  "Header string for the Noema dispatch transient."
  (let ((status (cond
                 ((not my/noema--ready)
                  (propertize "offline" 'face 'error))
                 (t (propertize (format "port %d" my/noema--port)
                                'face 'success))))
        (sync (or my/noema--last-sync-stats "index status unknown")))
    (format "Noema  [%s]  %s" status sync)))

(with-eval-after-load 'transient
  (transient-define-prefix my/noema-dispatch ()
    "Noema editor and canonical Wiki hub.  H-o from anywhere."
    [:description my/noema--dispatch-header
     ;; Row 1 ─────────────────────────────────────────────────────────────────
     ["Note (web)"
      ("o" "open current"     my/noema-open-current-note)
      ("O" "open file…"       my/noema-open-file)
      ("s" "save"             my/noema-save)
      ("r" "refresh"          my/noema-refresh)
      ("f" "focus editor"     my/noema-focus)
      ("e" "escape/normal"    my/noema-escape)
      ("v" "toggle source"    my/noema-toggle-source)
      ("W" "editable split"   my/noema-open-current-note-split)
      ("B" "build + reopen"   my/noema-build-and-reopen)
      ("Q" "close all"        my/noema-close)
      ("R" "raw edit in Emacs" my/noema-open-markdown-raw)
      ("J" "toggle open surface" my/noema-toggle-markdown-surface)
      ("?" "roam key reference" my/noema-roam-help)]
     ["Find / Browse"
      ("j" "find note"        my/noema-roam-find-note)
      ("/" "knowledge search" my/noema-knowledge-search)
      ("l" "recent pages"     my/noema-wiki-recent)
      ("." "follow link"      my/noema-roam-follow-link)
      ("b" "backlinks"        my/noema-roam-backlinks)
      ("x" "related notes"    my/noema-roam-related-notes)
      ("G" "goto definition"  my/noema-roam-goto-definition)]
     ["Insert"
      ("i" "roam link"        my/noema-roam-insert-link)
      ("I" "TOC link"         my/noema-roam-insert-toc-link)
      ("t" "tag id"           my/noema-roam-insert-tag-id)
      ("T" "tag-id link"      my/noema-roam-insert-tag-id-link)
      ("w" "copy link here"   my/noema-roam-copy-link-to-here)
      ("c" "note-code"        my/note-code-insert)]
     ;; Row 2 ─────────────────────────────────────────────────────────────────
     ["Knowledge"
      ("h" "Wiki home"        my/noema-wiki-home)
      ("n" "new page"         my/noema-wiki-new-page)
      ("d" "daily note"       my/noema-roam-daily-note)
      ("a" "tags"             my/noema-wiki-tags)
      ("C" "namespaces"       my/noema-wiki-namespaces)
      ("g" "workspace graph"  my/noema-workspace-graph)
      ("k" "tasks"            my/noema-roam-todos)
      ("A" "agenda"           my/noema-roam-agenda)
      ("L" "agenda log"       my/noema-roam-agenda-log)
      ("F" "file todos"       my/noema-roam-jump-file-todo)
      ("M" "repositories"     my/noema-wiki-repositories)]
     ["Wiki pages"
      ("!r" "reports"         my/noema-wiki-reports)
      ("!w" "wanted pages"    my/noema-wiki-wanted)
      ("!p" "all pages"       my/noema-wiki-pages)
      ("!s" "sync"            my/noema-wiki-sync)]
     ["Index / Host"
      ("y" "incremental refresh" my/noema-wiki-refresh)
      ("Z" "atomic full rebuild" my/noema-wiki-rebuild)
      ("S" "Wiki index status" my/noema-wiki-index-status)
      ("P" "pause/resume"     my/noema-toggle-pause)
      ("H" "runtime status"   my/noema-runtime-status)
      ("D" "dired"            my/noema-roam-dired)
      ("m" "move note"        my/noema-roam-move-note)
      ("X" "magit"            my/noema-roam-magit)
      ("q" "stop server"      my/noema-stop)]
     ["Format (web)"
      ("1" "bold"             my/noema-bold)
      ("2" "italic"           my/noema-italic)
      ("3" "code inline"      my/noema-code-inline)
      ("4" "highlight"        my/noema-highlight)
      ("5" "strike"           my/noema-strike)
      ("^" "superscript"      my/noema-superscript)
      ("_" "subscript"        my/noema-subscript)
      ("N" "footnote"         my/noema-insert-footnote)
      ("K" "revision"         my/noema-insert-revision)
      ("@" "properties"       my/noema-edit-properties)
      ("[" "move block up"    my/noema-move-block-up)
      ("]" "move block down"  my/noema-move-block-down)
      ("6" "blockquote"       my/noema-blockquote)
      ("7" "bullet list"      my/noema-bullet-list)
      ("8" "ordered list"     my/noema-ordered-list)
      ("9" "task list"        my/noema-task-list)
      ("0" "code block"       my/noema-code-block)
      ("p" "heading menu"     my/noema-paragraph-menu)
      ("z" "insert table"     my/noema-insert-table)
      ("E" "math block"       my/noema-insert-math)
      ("u" "insert TOC"       my/noema-insert-toc)
      ("U" "undo"             my/noema-undo)
      ("Y" "redo"             my/noema-redo)
      ("V" "paste"            my/noema-paste)]]))

(with-eval-after-load 'transient
  (transient-define-prefix my/noema-wiki-dispatch ()
    "Canonical Wiki navigation and index maintenance."
    [:description my/noema--dispatch-header
     ["Browse"
      ("h" "home" my/noema-wiki-home)
      ("p" "pages" my/noema-wiki-pages)
      ("r" "recent" my/noema-wiki-recent)
      ("n" "new page" my/noema-wiki-new-page)
      ("g" "graph" my/noema-workspace-graph)]
     ["Organize"
      ("t" "tags" my/noema-wiki-tags)
      ("N" "namespaces" my/noema-wiki-namespaces)
      ("R" "repositories" my/noema-wiki-repositories)
      ("s" "sync" my/noema-wiki-sync)
      ("!" "reports" my/noema-wiki-reports)]
     ["Index"
      ("i" "incremental refresh" my/noema-wiki-refresh)
      ("F" "atomic full rebuild" my/noema-wiki-rebuild)
      ("S" "status" my/noema-wiki-index-status)]]))

;;; Keybindings.

;; Global: H-o opens the Noema dispatch panel.
(general-define-key "H-o" #'my/noema-dispatch)
(general-define-key "C-H-o" #'my/noema-dispatch)

;; Appine buffer direct keys — override global H- bindings that are irrelevant
;; when focused in the Noema pane.
(with-eval-after-load 'appine
  (when (boundp 'appine-active-map)
    (define-key appine-active-map (kbd "H-o") #'my/noema-dispatch)
    (define-key appine-active-map (kbd "C-H-o") #'my/noema-dispatch)
    (define-key appine-active-map (kbd "M-z") #'my/noema-undo)
    (define-key appine-active-map (kbd "M-Z") #'my/noema-redo)
    (define-key appine-active-map (kbd "M-S-z") #'my/noema-redo)
    (define-key appine-active-map (kbd "H-s") #'my/noema-save)
    (define-key appine-active-map (kbd "H-r") #'my/noema-refresh)
    (define-key appine-active-map (kbd "H-B") #'my/noema-build-and-reopen)
    (define-key appine-active-map (kbd "H-q") #'my/noema-close)
    (define-key appine-active-map (kbd "H-y") #'my/noema-wiki-refresh)
    (define-key appine-active-map (kbd "H-g") #'my/noema-roam-graph)))

(my/noema--rename-live-buffers)

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
