;;; init-aaronnote.el --- Aaronnote Web/Appine bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Aaronnote web host and opens it in Appine/xwidget.
;; The editable document state lives in Aaronnote's CodeMirror app; Emacs does
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

(declare-function my/xwidget-open-url "init-browser" (url &rest args))
(declare-function my/xwidget-current-url "init-browser" (&optional buffer))
(declare-function my/xwidget-session-buffer "init-browser" (id))
(declare-function my/xwidget-focus "init-browser" (&optional buffer))
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
(declare-function my/aaronnote-roam-note-changed "init-md-roam" (file))
(declare-function my/aaronnote-roam--clear-runtime-cache "init-md-roam" ())
(declare-function my/aaronnote-roam--cancel-sync-timer "init-md-roam" ())
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
(declare-function remote-file-local-name "remote-fs" (file-name))
(defvar remote-mode nil)
(defvar my/appine-tab-list)
(defvar my/xwidget--session-id)

;; Publish module — lazy, loaded only when a publish command is first invoked.
(autoload 'my/aaronnote-publish              "init-aaronnote-publish" nil t)
(autoload 'my/aaronnote-publish-build        "init-aaronnote-publish" nil t)
(autoload 'my/aaronnote-publish-deploy       "init-aaronnote-publish" nil t)
(autoload 'my/aaronnote-publish-clean        "init-aaronnote-publish" nil t)

(defgroup my/aaronnote nil
  "Aaronnote Markdown web editor integration."
  :group 'applications)

(defvar my/aaronnote--web-host-script
  (expand-file-name "lisp/roam/aaronnote/web-host.mjs" user-emacs-directory)
  "Path to the Aaronnote web host script.")

(defvar my/aaronnote--web-dir
  (expand-file-name "lisp/roam/aaronnote/dist/aaronnote" user-emacs-directory)
  "Path to the built Aaronnote web app.")

(defvar my/aaronnote--runtime-root
  (expand-file-name "lisp/roam/aaronnote" user-emacs-directory)
  "Path to the vendored Aaronnote runtime.")

(defvar my/aaronnote--state-root
  (expand-file-name "var/aaronnote" user-emacs-directory)
  "Path to Aaronnote state files under the Emacs config.")

(defvar my/aaronnote--tmp-root
  (expand-file-name "tmp" my/aaronnote--state-root)
  "Path to Aaronnote runtime temporary files under the Emacs config.")

(defvar my/aaronnote--snippets-root
  (expand-file-name "snippets" user-emacs-directory)
  "Path to Aaronnote snippets shared with Emacs.")

(defvar my/aaronnote--templates-root
  (expand-file-name "templates/aaronnote" user-emacs-directory)
  "Path to Markdown templates shared by Emacs and Aaronnote.")

(defvar my/aaronnote--notes-root
  (expand-file-name ".roam" user-emacs-directory)
  "Path to the Markdown notes directory.")

(defun my/aaronnote--project-settings ()
  "Read Aaronnote's project settings from the note root without evaluation."
  (let ((file (expand-file-name ".dir-locals.el" my/aaronnote--notes-root)))
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

(defun my/aaronnote--jupyter-defaults ()
  "Return project-configured Jupyter defaults for new Aaronnote cells."
  (let* ((settings (my/aaronnote--project-settings))
         (configured (plist-get settings :aaronnote-jupyter)))
    (list :language (format "%s" (or (plist-get configured :language) "python"))
          :kernel (format "%s" (or (plist-get configured :kernel) "python3"))
          :session (format "%s" (or (plist-get configured :session) "default")))))

(defun my/aaronnote--jupyter-default-environment ()
  "Return web-host environment entries for project Jupyter defaults."
  (let ((defaults (my/aaronnote--jupyter-defaults)))
    (list
     (format "AARONNOTE_JUPYTER_DEFAULT_LANGUAGE=%s" (plist-get defaults :language))
     (format "AARONNOTE_JUPYTER_DEFAULT_KERNEL=%s" (plist-get defaults :kernel))
     (format "AARONNOTE_JUPYTER_DEFAULT_SESSION=%s" (plist-get defaults :session)))))

(defvar my/aaronnote--katex-macros-dir
  (expand-file-name "etc/katex-macros" user-emacs-directory)
  "Folder of .tex files defining the global KaTeX macro environment.")

(config-defvar my/aaronnote-backend nil
  "Backend used to display Aaronnote."
  :type '(choice (const :tag "xwidget-webkit" xwidget) (const :tag "Appine" appine))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-web-port nil
  "Fixed port for the Aaronnote web host.
Set to 0 to let the OS pick a random port."
  :type 'integer
  :group 'my/aaronnote)

(config-defvar my/aaronnote-web-host-max-heap-mb nil
  "V8 heap cap (MB) for the Aaronnote web-host node process, or nil for no cap.
Passed as a `--max-old-space-size' command-line flag rather than
`NODE_OPTIONS' in the environment, because web-host's `process.env' is also
handed to the codex/claude/opencode CLIs it shells out to for LaTeX export —
an env-based cap would leak onto those unrelated node processes too."
  :type '(choice (integer :tag "Heap cap in MB") (const :tag "No cap" nil))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-echo-severity 'error
  "Warning/error policy for Aaronnote messages copied to the Emacs echo area.
Important command responses are always echoed after browser-side deduplication
and rate limiting.  `error' additionally echoes errors, `warning' echoes
warnings and errors, and nil suppresses both severity classes."
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Warnings and errors" warning)
                 (const :tag "Never" nil))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-engine "codex"
  "Engine for the Aaronnote CMD+P LaTeX export.
\"codex\" compile-verifies a deterministic mechanical draft first, then allows
one fidelity-gated polish attempt.  A verified draft is never retried after an
agent timeout or gate rejection; multiple repairs require a concrete mechanical
compile failure.  \"mechanical\" never invokes an agent.  See
`docs/latex-export-style.md' in the Aaronnote app."
  :type '(choice (const "codex") (const "mechanical"))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-max-attempts 3
  "Maximum feedback-driven agent repairs after mechanical verification fails.
A fidelity/review rejection falls back immediately instead of retrying."
  :type 'integer
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-agent-idle-timeout 180
  "Seconds without agent output before Aaronnote performs a liveness check.
A live process is kept running; this is not a kill timeout."
  :type 'integer
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-agent-hard-timeout 900
  "Absolute seconds allowed for one LaTeX export agent attempt.
At this limit Aaronnote requests graceful termination before using a hard kill."
  :type 'integer
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-agent "codex"
  "AI backend for the Aaronnote LaTeX export repair step.
One of \"codex\", \"claude\", or \"opencode\".  All run non-interactively in the
single-export staging directory with external-directory writes blocked and
network access available.  Clean mechanically verified exports do not launch
the backend.  The backend is chosen here, not per export."
  :type '(choice (const "codex") (const "claude") (const "opencode"))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-codex-model ""
  "Optional model id for codex during LaTeX export polish (empty = codex default)."
  :type 'string
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-model ""
  "Optional model id passed to the active LaTeX export backend (empty = default)."
  :type 'string
  :group 'my/aaronnote)

(config-defvar my/aaronnote-opencode-executable "opencode"
  "Executable used when the LaTeX export backend is opencode."
  :type 'string
  :group 'my/aaronnote)

(defvar my/aaronnote--last-sync-stats nil
  "String summary from the last successful Roam DB sync, or nil.")

(defvar my/aaronnote--process nil
  "Running Aaronnote web-host child process, or nil.")
(defvar my/aaronnote--gateway-binding nil
  "Registration data for the current AaronNote web-host process.")
(defvar my/aaronnote--external-file-watches (make-hash-table :test #'equal)
  "Remote-backed file watches owned by the AaronNote runtime session.")
(defvar my/aaronnote--external-file-watch-timers
  (make-hash-table :test #'equal)
  "Debounce timers for remote AaronNote file changes.")
(defvar my/aaronnote--external-file-watch-suppressed
  (make-hash-table :test #'equal)
  "Times before which self-write watch events should be ignored.")
(defvar my/aaronnote--port nil
  "HTTP port of the running Aaronnote web-host.")
(defvar my/aaronnote--last-port nil
  "Last ready web-host port, retained so a crashed core can reclaim its URL.")
(defvar my/aaronnote--ready nil
  "Non-nil once the web-host has announced its port.")
(defvar my/aaronnote--ready-callbacks nil
  "Callbacks waiting for the web-host to become ready.")
(defvar my/aaronnote--app-buffer nil
  "Buffer hosting the Appine/xwidget Aaronnote page.")
(defvar my/aaronnote--ready-watchdog nil
  "Watchdog timer cancelled when the web-host becomes ready.")
(defvar my/aaronnote--goto-timer nil
  "Debounce timer for coalescing goto events from the web-host.")
(defvar my/aaronnote--goto-last nil
  "Last applied goto key (truename-file line col), for dedup.")
(defvar my/aaronnote--file-buffers (make-hash-table :test #'equal)
  "Canonical Aaronnote file path to browser buffer map.")
(defvar my/aaronnote--client-buffers (make-hash-table :test #'equal)
  "Aaronnote browser client id to browser buffer map.")

(defvar my/aaronnote--build-process nil
  "Current Aaronnote web build process, or nil.")

(defvar my/aaronnote--split-counter 0
  "Counter for fresh Aaronnote xwidget split sessions.")

(defvar-local my/aaronnote-buffer-file-name nil
  "Current note file represented by an Aaronnote Appine/xwidget buffer.")

(put 'my/aaronnote-buffer-file-name 'permanent-local t)

(defvar-local my/aaronnote--client-id nil
  "Client id for this Aaronnote browser buffer.")

(put 'my/aaronnote--client-id 'permanent-local t)

(defvar-local my/aaronnote--registered-file nil
  "File path currently registered for this Aaronnote browser buffer.")

(put 'my/aaronnote--registered-file 'permanent-local t)

(defvar-local my/aaronnote--xwidget-forced-name nil
  "Non-nil display name marker for Aaronnote xwidget buffers.")

(put 'my/aaronnote--xwidget-forced-name 'permanent-local t)

(defvar-local my/aaronnote--xwidget-pending-file nil
  "File to POST to Aaronnote once the page has finished loading, or nil.")

(put 'my/aaronnote--xwidget-pending-file 'permanent-local t)

;; Keep the Markdown/xwidget input bridge in a dedicated module.  Its command
;; names and wire protocol remain unchanged for browser and Emacs callers.
(add-to-list 'load-path
             (expand-file-name "lisp/roam/aaronnote/emacs" user-emacs-directory))
(require 'aaronnote-xwidget-keys)

(defvar-keymap my/aaronnote-keys-mode-map
  "M-z" #'my/aaronnote-undo
  "M-Z" #'my/aaronnote-redo
  "M-S-z" #'my/aaronnote-redo
  "M-C" #'my/aaronnote-prose-check)

(define-minor-mode my/aaronnote-keys-mode
  "Buffer-local keys for an Aaronnote browser surface."
  :init-value nil
  :lighter nil
  :keymap my/aaronnote-keys-mode-map)

(defconst my/aaronnote--xwidget-focus-script
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
  "JavaScript used to move focus into the Aaronnote editor inside xwidget.")

(defun my/aaronnote--server-url (&optional path)
  "Return the local Aaronnote URL for PATH."
  (format "http://127.0.0.1:%d%s" my/aaronnote--port (or path "/")))

(defun my/aaronnote--canonical-file (file)
  "Return canonical absolute FILE for Aaronnote bookkeeping, or nil."
  (and (stringp file)
       (not (string-empty-p file))
       (cond
        ((and (bound-and-true-p remote-mode)
              (fboundp 'remote-expand-file-name))
         ;; Aaronnote's web host runs locally.  Raw host paths (including
         ;; `~/...') therefore belong to the local target; an already logical
         ;; or TRAMP path retains its encoded target and is rejected later by
         ;; `my/aaronnote--host-file' when the local host cannot serve it.
         (remote-expand-file-name
          file nil
          (unless
              (or (and (fboundp 'remote-fs-file-name-p)
                       (remote-fs-file-name-p file))
                  (string-match-p "\\`fs://" file)
                  (file-remote-p file))
            "local")))
        ((and (bound-and-true-p remote-mode)
              (fboundp 'remote-canonicalize-file-name))
         (remote-canonicalize-file-name file))
        (t
         (expand-file-name file)))))

(defun my/aaronnote--host-file (file)
  "Return the path AaronNote should use for logical FILE.
Local files are projected to native host paths.  Remote files retain their
`/fs:' identity and are served through the Remote-backed gateway provider."
  (when-let* ((file (my/aaronnote--canonical-file file)))
    (if (and (bound-and-true-p remote-mode)
             (fboundp 'remote-file-name-target)
             (fboundp 'remote-file-local-name))
        (if (equal (remote-file-name-target file) "local")
            (remote-file-local-name file)
          file)
      (expand-file-name file))))

(defun my/aaronnote--xwidget-session-id (&optional file)
  "Return the stable xwidget session/client id for FILE."
  (if-let* ((file (my/aaronnote--canonical-file file)))
      (format "aaronnote:%s" file)
    "aaronnote"))

(defun my/aaronnote--split-client-p (client)
  "Return non-nil when CLIENT identifies a split pane."
  (and (stringp client)
       (string-prefix-p "aaronnote-split:" client)))

(defun my/aaronnote--app-url (&optional file client extra-params)
  "Return the Aaronnote app URL, optionally opening FILE for CLIENT."
  (let ((base (my/aaronnote--server-url "/"))
        params)
    (when-let* ((file (my/aaronnote--canonical-file file)))
      (push (cons "file" (my/aaronnote--host-file file)) params))
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

(defun my/aaronnote--markdown-file-p (file)
  "Return non-nil when FILE is a Markdown file."
  (and file
       (or (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
           (string-equal (file-name-nondirectory file) "README"))))

(defun my/aaronnote--web-host-log-tail (&optional lines)
  "Return the last LINES (default 12) lines of the Aaronnote web-host log
buffer, or nil when the buffer does not exist or has no output yet."
  (when-let* ((buf (get-buffer " *aaronnote-web-host*")))
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

(defun my/aaronnote--watchdog-fire ()
  "Called when the web-host fails to become ready within the timeout.
Previously this only dropped the queued ready-callbacks silently with a
one-line `message'; the caller who pressed e.g. `A' for the agenda had no
visible sign that nothing was going to happen. Now it also surfaces the
process state and a tail of the log buffer, and pops that buffer so the
failure is diagnosable without hunting for it."
  (setq my/aaronnote--ready-watchdog nil)
  (unless my/aaronnote--ready
    (let* ((dropped (length my/aaronnote--ready-callbacks))
           (alive (and my/aaronnote--process (process-live-p my/aaronnote--process)))
           (tail (my/aaronnote--web-host-log-tail))
           (log-buf (get-buffer " *aaronnote-web-host*")))
      (setq my/aaronnote--ready-callbacks nil)
      (when log-buf (display-buffer log-buf))
      (message "%s"
               (concat
                (format "Aaronnote: web-host not ready after 10s (%d pending action%s dropped)."
                        dropped (if (= dropped 1) "" "s"))
                (if alive "" " Process exited — check node/port.")
                (if tail (format " Last log: %s" tail) " No log output yet — see *aaronnote-web-host*."))))))

(defun my/aaronnote--ensure-server (&optional callback)
  "Start the web-host if needed, then call CALLBACK."
  (if (and my/aaronnote--process
           (process-live-p my/aaronnote--process)
           my/aaronnote--ready)
      (when callback (funcall callback))
    (when callback
      (push callback my/aaronnote--ready-callbacks))
    (unless (and my/aaronnote--process
                 (process-live-p my/aaronnote--process))
      (when (fboundp 'my/appine-kill-all)
        (ignore-errors (my/appine-kill-all)))
      (when (fboundp 'my/appine--tab-reset)
        (my/appine--tab-reset))
      (my/aaronnote--start-server)
      (when my/aaronnote--ready-watchdog
        (cancel-timer my/aaronnote--ready-watchdog))
      (setq my/aaronnote--ready-watchdog
            (run-at-time 10 nil #'my/aaronnote--watchdog-fire)))))

(defun my/aaronnote--start-server (&optional reconnect-port)
  "Spawn the vendored Aaronnote web-host.
When RECONNECT-PORT is non-nil, reclaim that port so live browser pages can
reconnect without a reload and without losing their in-memory editor state."
  (unless (executable-find "node")
    (user-error "Aaronnote: `node' not found in exec-path; install Node.js"))
  (unless (file-directory-p my/aaronnote--web-dir)
    (user-error "Aaronnote: built web app not found at %s; run `npm run build' in %s"
                my/aaronnote--web-dir my/aaronnote--runtime-root))
  (let ((old-proc my/aaronnote--process))
    (when (and old-proc (process-live-p old-proc))
      (ignore-errors (signal-process old-proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p old-proc)
            (delete-process old-proc))))))
  (setq my/aaronnote--process nil
        my/aaronnote--port nil
        my/aaronnote--ready nil)
  (let* ((log-buf (get-buffer-create " *aaronnote-web-host*"))
         (_copilot-gateway-method
          (require 'init-copilot nil t))
         (gateway
          (remote-gateway-prepare-client
           "aaronnote" (remote-context my/aaronnote--notes-root)
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
            (format "AARONNOTE_ROOT=%s" (expand-file-name my/aaronnote--notes-root))
            (format "AARONNOTE_WEB_DIR=%s" (expand-file-name my/aaronnote--web-dir))
            (format "AARONNOTE_RUNTIME_ROOT=%s" (expand-file-name my/aaronnote--runtime-root))
            (format "AARONNOTE_WORKSPACE_ROOT=%s" (expand-file-name user-emacs-directory))
            (format "AARONNOTE_LANGUAGETOOL_LANGUAGE=%s"
                    (or (bound-and-true-p my/languagetool-language) "en-US"))
            (format "AARONNOTE_LANGUAGETOOL_URL=%s"
                    (or (bound-and-true-p my/languagetool-server-url)
                        "http://10.243.90.222:8765"))
            (format "AARONNOTE_PROSE_WORDS=%s"
                    (expand-file-name "etc/prose-accepted-words.txt"
                                      user-emacs-directory))
            (format "AARONNOTE_PUBLISH_JS_DIR=%s"
                    (expand-file-name "js" my/aaronnote--runtime-root))
            (format "AARONNOTE_STATE_DIR=%s" (expand-file-name my/aaronnote--state-root))
            (format "AARONNOTE_TMP_DIR=%s" (expand-file-name my/aaronnote--tmp-root))
            (format "AARONNOTE_SNIPPETS_ROOT=%s" (expand-file-name my/aaronnote--snippets-root))
            (format "AARONNOTE_TEMPLATES_ROOT=%s" (expand-file-name my/aaronnote--templates-root))
            (format "AARONNOTE_KATEX_MACROS_DIR=%s" (expand-file-name my/aaronnote--katex-macros-dir)))
            (my/aaronnote--jupyter-default-environment)
            (list
            (format "AARONNOTE_LATEX_EXPORT_ENGINE=%s"
                    (or my/aaronnote-latex-export-engine "codex"))
            (format "AARONNOTE_LATEX_EXPORT_AGENT=%s"
                    (or (bound-and-true-p my/aaronnote-latex-export-agent) "codex"))
            (format "AARONNOTE_LATEX_EXPORT_MAX_ATTEMPTS=%d"
                    (or my/aaronnote-latex-export-max-attempts 3))
            (format "AARONNOTE_LATEX_EXPORT_AGENT_IDLE_TIMEOUT_MS=%d"
                    (* 1000 (max 10 (or my/aaronnote-latex-export-agent-idle-timeout 180))))
            (format "AARONNOTE_LATEX_EXPORT_AGENT_HARD_TIMEOUT_MS=%d"
                    (* 1000 (max (or my/aaronnote-latex-export-agent-idle-timeout 180)
                                 (or my/aaronnote-latex-export-agent-hard-timeout 900))))
            (format "AARONNOTE_CODEX_BIN=%s"
                    (or (bound-and-true-p codex-cli-executable) "codex"))
            (format "AARONNOTE_CLAUDE_BIN=%s"
                    (or (bound-and-true-p claude-code-ide-cli-path) "claude"))
            (format "AARONNOTE_OPENCODE_BIN=%s"
                    (or (bound-and-true-p my/aaronnote-opencode-executable) "opencode"))
            (when (and (boundp 'my/aaronnote-codex-model)
                       (stringp my/aaronnote-codex-model)
                       (not (string-empty-p my/aaronnote-codex-model)))
              (format "AARONNOTE_CODEX_MODEL=%s" my/aaronnote-codex-model))
            (when (and (boundp 'my/aaronnote-latex-export-model)
                       (stringp my/aaronnote-latex-export-model)
                       (not (string-empty-p my/aaronnote-latex-export-model)))
              (format "AARONNOTE_LATEX_EXPORT_MODEL=%s" my/aaronnote-latex-export-model))
            (format "AARONNOTE_WEB_PORT=%d"
                    (or reconnect-port my/aaronnote-web-port 0))
            ;; Emacs-started AaronNote should share Emacs' existing Copilot LS
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
              (when my/aaronnote-web-host-max-heap-mb
                (list
                 (format "--max-old-space-size=%d"
                         my/aaronnote-web-host-max-heap-mb)))
              (list (my/aaronnote--host-file
                     my/aaronnote--web-host-script)))
             :noquery t
             :sentinel #'my/aaronnote--sentinel
             :filter #'my/aaronnote--process-filter
             :remote-client-directory user-emacs-directory
             :remote-client-environment process-environment)))
    (with-current-buffer log-buf (erase-buffer))
    (setq my/aaronnote--process proc
          my/aaronnote--gateway-binding gateway)
    proc))

(defun my/aaronnote--flush-ready-callbacks ()
  "Run callbacks waiting for the server to become ready."
  (when my/aaronnote--ready-watchdog
    (cancel-timer my/aaronnote--ready-watchdog)
    (setq my/aaronnote--ready-watchdog nil))
  (let ((callbacks (nreverse my/aaronnote--ready-callbacks)))
    (setq my/aaronnote--ready-callbacks nil)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback)))
  (my/aaronnote--install-activity-hooks)
  ;; Do an initial activity check after the page has had time to load.
  (run-with-idle-timer 2 nil #'my/aaronnote--update-activity))

(defun my/aaronnote--run-zotero-event (payload import-p)
  "Handle Zotero PAYLOAD in Emacs; IMPORT-P starts the BibTeX picker."
  (let* ((client (alist-get 'client payload))
         (source-buffer (my/aaronnote--key-source-buffer client)))
    (my/aaronnote--release-xwidget-input-buffer source-buffer)
    (my/aaronnote--select-emacs-window)
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
       (message "Aaronnote Zotero %s failed: %s"
                (if import-p "import" "open")
                (error-message-string err))))))

(defun my/aaronnote--handle-ui-state-payload (payload)
  "Echo a structured AaronNote UI-state PAYLOAD when policy permits.
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
           (pcase my/aaronnote-echo-severity
             ('warning (memq severity '(warning error)))
             ('error (eq severity 'error))
             (_ nil)))))
    (when (and echo-p
               (stringp status)
               (not (string-empty-p status)))
      (message "AaronNote %s: %s" severity status))))

(defun my/aaronnote--handle-process-line (line)
  "Handle one legacy AaronNote event encoded as LINE."
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
          (setq my/aaronnote--port port
                my/aaronnote--last-port port
                my/aaronnote--ready t)
          (my/aaronnote--flush-ready-callbacks))))
     ((string-prefix-p goto-prefix line)
      (let* ((payload (substring line (length goto-prefix)))
             (parts (split-string payload ":" nil))
             (line-number (string-to-number (or (car parts) "0")))
             (column (string-to-number (or (cadr parts) "0"))))
        (when (> line-number 0)
          ;; Coalesce burst goto events: cancel any pending jump and schedule
          ;; a fresh one.  Normal (not idle) timer so jumps are not deferred
          ;; indefinitely during continuous Emacs activity.
          (when my/aaronnote--goto-timer
            (cancel-timer my/aaronnote--goto-timer))
          (setq my/aaronnote--goto-timer
                (run-at-time
                 0.05 nil
                 (let ((ln line-number) (col column))
                   (lambda ()
                     (setq my/aaronnote--goto-timer nil)
                     (my/aaronnote--goto-location nil ln col))))))))
     ((string-prefix-p open-prefix line)
	      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length open-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload))
                 (line-number (or (alist-get 'line payload) 1))
                 (column (or (alist-get 'col payload) 0))
                 (tag (alist-get 'tag payload)))
            (if (and (my/aaronnote--markdown-file-p file)
                     (or (null tag) (string-empty-p (or tag ""))))
                ;; Markdown note (e.g. graph double-click): open in Aaronnote.
                (my/aaronnote-open-file file)
              ;; Source region (lean, etc.) or explicit tag: open in Emacs.
              (my/aaronnote--goto-location file line-number column)
              (when (and (stringp file)
                         (string-match-p (concat "\\(?:\\`\\|/\\)\\.cell/[^/]+\\'")
                                         file)
                         (require 'init-aaronnote-jupyter-cell nil t))
                (ignore-errors
                  (my/aaronnote-jupyter-cell-activate-buffer)))
              (when (and tag (not (string-empty-p (or tag ""))))
                (when (require 'init-note-code nil t)
                  (ignore-errors (my/note-code--goto-tag tag))))))
	        (error
	         (message "Aaronnote event parse failed: %s" (error-message-string err)))))
     ((string-prefix-p zotero-import-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-import-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'my/aaronnote--run-zotero-event payload t))
        (error
         (message "Aaronnote Zotero import event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p zotero-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'my/aaronnote--run-zotero-event payload nil))
        (error
         (message "Aaronnote Zotero event failed: %s"
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
         (message "Aaronnote system-open event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p current-file-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length current-file-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload))
                 (client (alist-get 'client payload)))
            (my/aaronnote--sync-app-buffer-file file client))
        (error
         (message "Aaronnote current-file parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p ui-state-prefix line)
      (condition-case err
          (my/aaronnote--handle-ui-state-payload
           (json-parse-string
            (substring line (length ui-state-prefix))
            :object-type 'alist))
        (error
         (message "Aaronnote UI-state parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p saved-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length saved-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload)))
            (when (and (stringp file) (not (string-empty-p file)))
              (when (fboundp 'my/aaronnote-roam-note-changed)
                (my/aaronnote-roam-note-changed file))))
        (error
         (message "Aaronnote saved-event parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p key-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length key-prefix))
                           :object-type 'alist))
                 (key (alist-get 'key payload))
                 (client (alist-get 'client payload)))
            (when (stringp key)
              (my/aaronnote--run-emacs-key key client)))
        (error
         (message "Aaronnote key-event parse failed: %s"
                  (error-message-string err))))))))

(defun my/aaronnote--external-file (file)
  "Return a canonical Markdown FILE accepted by the gateway provider."
  (let ((file (my/aaronnote--canonical-file file)))
    (unless (and file (my/aaronnote--markdown-file-p file))
      (error "AaronNote external provider requires a Markdown file: %s"
             file))
    file))

(defun my/aaronnote--external-file-metadata (file)
  "Return JSON-ready metadata for FILE, or signal when it is unavailable."
  (let ((attributes (file-attributes file 'string)))
    (unless (and attributes (file-regular-p file))
      (error "Remote Markdown file is unavailable: %s" file))
    `((mtimeMs
       . ,(* 1000.0
             (float-time
              (file-attribute-modification-time attributes))))
      (size . ,(or (file-attribute-size attributes) 0)))))

(defun my/aaronnote--external-file-notify-change (file)
  "Notify the AaronNote peer that logical FILE changed externally."
  (remhash file my/aaronnote--external-file-watch-timers)
  (when-let* ((client (remote-gateway-find-client "aaronnote")))
    (let* ((metadata
            (condition-case nil
                (my/aaronnote--external-file-metadata file)
              (error '((mtimeMs . 0) (size . 0)))))
           (mtime (alist-get 'mtimeMs metadata)))
      (remote-gateway-notify
       client "aaronnote.command"
       `((type . "command")
         (command . "note-saved")
         (file . ,file)
         (mtimeMs . ,mtime)
         (clientId . "remote-external"))))))

(defun my/aaronnote--external-file-watch-event (file event)
  "Debounce Remote file watch EVENT for logical FILE."
  (unless (or (eq (nth 1 event) 'stopped)
              (< (float-time)
                 (or
                  (gethash
                   file my/aaronnote--external-file-watch-suppressed)
                  0)))
    (when-let* ((timer
                 (gethash
                  file my/aaronnote--external-file-watch-timers)))
      (cancel-timer timer))
    (puthash
     file
     (run-at-time
      0.25 nil #'my/aaronnote--external-file-notify-change file)
     my/aaronnote--external-file-watch-timers)))

(defun my/aaronnote--ensure-external-file-watch (file)
  "Ensure one recoverable Remote watch exists for logical FILE."
  (when (and (bound-and-true-p remote-mode)
             (not (equal (remote-file-name-target file) "local"))
             (not (gethash file my/aaronnote--external-file-watches)))
    (condition-case error
        (let* ((context (remote-context (file-name-directory file)))
               (workspace (remote-workspace-open context :connect nil))
               (resource
                (remote-workspace-add-file-watch
                 workspace file '(change attribute-change)
                 (lambda (event)
                   (my/aaronnote--external-file-watch-event file event))
                 :key (list 'aaronnote-external-file file)
                 :metadata
                 (list :application "aaronnote" :file file))))
          (puthash
           file (cons workspace resource)
           my/aaronnote--external-file-watches))
      (error
       ;; Editing still works on backends without watch capability; refresh is
       ;; then explicit and saves continue to use mtime conflict detection.
       (message "AaronNote remote watch unavailable for %s: %s"
                file (error-message-string error))))))

(defun my/aaronnote--clear-external-file-watches ()
  "Close AaronNote's Remote watches and debounce timers."
  (maphash
   (lambda (_file timer)
     (when (timerp timer)
       (cancel-timer timer)))
   my/aaronnote--external-file-watch-timers)
  (maphash
   (lambda (_file owner)
     (ignore-errors
       (remote-workspace-close-resource
        (car owner) (cdr owner) 'aaronnote-stop)))
   my/aaronnote--external-file-watches)
  (clrhash my/aaronnote--external-file-watch-timers)
  (clrhash my/aaronnote--external-file-watches)
  (clrhash my/aaronnote--external-file-watch-suppressed))

(defun my/aaronnote--external-file-read (params _client)
  "Read the logical Markdown file named by gateway PARAMS through Remote."
  (let* ((file
          (my/aaronnote--external-file
           (alist-get 'file params)))
         (content
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-substring-no-properties
             (point-min) (point-max)))))
    (my/aaronnote--ensure-external-file-watch file)
    (append
     `((file . ,file) (content . ,content))
     (my/aaronnote--external-file-metadata file))))

(defun my/aaronnote--external-file-write (params _client)
  "Atomically write a logical Markdown file described by gateway PARAMS."
  (let* ((file
          (my/aaronnote--external-file
           (alist-get 'file params)))
         (content (format "%s" (or (alist-get 'content params) "")))
         (force (eq (alist-get 'force params) t))
         (base-mtime (alist-get 'baseMtimeMs params))
         (metadata (my/aaronnote--external-file-metadata file))
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
         my/aaronnote--external-file-watch-suppressed)
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
         my/aaronnote--external-file-watch-suppressed)
        (let ((written
               (my/aaronnote--external-file-metadata file)))
          `((ok . t) (conflict . :json-false)
            (file . ,file)
            (mtimeMs . ,(alist-get 'mtimeMs written))
            (size . ,(alist-get 'size written)))))))))

(defun my/aaronnote--gateway-event (params _client)
  "Dispatch AaronNote event PARAMS received through the shared gateway."
  (let* ((type (format "%s" (or (alist-get 'type params) "")))
         (payload (or (alist-get 'payload params) '()))
         (line
          (pcase type
            ("ui-state"
             (my/aaronnote--handle-ui-state-payload payload)
             nil)
            ("ready"
             (format "aaronote-web-host:ready:%s"
                     (or (alist-get 'port payload) 0)))
            ("goto"
             (format "aaronote-event:goto:%s:%s"
                     (or (alist-get 'line payload) 0)
                     (or (alist-get 'col payload) 0)))
            ((or "open" "system-open" "zotero" "zotero-import"
                 "current-file" "saved" "key")
             (format "aaronote-event:%s:%s"
                     type (json-serialize payload)))
            (_ nil))))
    (when line
      (my/aaronnote--handle-process-line line))
    '((ok . t))))

(remote-gateway-register-method
 "aaronnote.event" #'my/aaronnote--gateway-event)
(remote-gateway-register-method
 "aaronnote.file.read" #'my/aaronnote--external-file-read)
(remote-gateway-register-method
 "aaronnote.file.write" #'my/aaronnote--external-file-write)

(defun my/aaronnote--process-filter (proc output)
  "Append diagnostic web-host OUTPUT from PROC to its bounded log."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert output)
      ;; Bound log growth: keep only the most recent ~200 KB, trimming at a
      ;; complete line boundary so no partial lines are left behind.
      ;; The parser accumulator lives in the process property, not this buffer.
      (when (> (point-max) 204800)
        (goto-char (- (point-max) 102400))
        (forward-line 1)
        (delete-region (point-min) (point)))))
  nil)

(defun my/aaronnote--sentinel (proc event)
  "Handle web-host PROC state change EVENT."
  (when (and (eq proc my/aaronnote--process)
             (not (process-live-p proc)))
    (when my/aaronnote--ready-watchdog
      (cancel-timer my/aaronnote--ready-watchdog)
      (setq my/aaronnote--ready-watchdog nil))
    (when my/aaronnote--goto-timer
      (cancel-timer my/aaronnote--goto-timer)
      (setq my/aaronnote--goto-timer nil))
    (setq my/aaronnote--goto-last nil
          my/aaronnote--process nil
          my/aaronnote--port nil
          my/aaronnote--ready nil
          my/aaronnote--ready-callbacks nil)
    (unless (string-match-p "^finished" event)
      (message "Aaronnote web-host: %s" (string-trim event)))))

(defun my/aaronnote-buffer-file (&optional buffer)
  "Return the Aaronnote note file represented by BUFFER.
When BUFFER is nil, inspect the current buffer."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (and (stringp my/aaronnote-buffer-file-name)
           (not (string-empty-p my/aaronnote-buffer-file-name))
           my/aaronnote-buffer-file-name))))

(defun my/aaronnote--buffer-display-name (&optional file)
  "Return the preferred Aaronnote buffer display name for FILE."
  (if-let* ((file (my/aaronnote--canonical-file file)))
      (format "*aaronnote: %s*" (file-name-nondirectory file))
    "*aaronnote*"))

(defun my/aaronnote--split-buffer-display-name (file ordinal)
  "Return an ibuffer-friendly name for FILE's split ORDINAL."
  (format "*aaronnote split %d: %s*"
          ordinal
          (if-let* ((file (my/aaronnote--canonical-file file)))
              (file-name-nondirectory file)
            "Aaronnote")))

(defun my/aaronnote--split-client-ordinal (client)
  "Return split ordinal encoded in CLIENT, or nil."
  (when (my/aaronnote--split-client-p client)
    (let ((value (car (last (split-string client ":" t)))))
      (when (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)))))

(defun my/aaronnote--notify-client-closed (&optional client file)
  "Notify the Aaronnote core that CLIENT no longer has a live view."
  (when (and (stringp client) (not (string-empty-p client)))
    (condition-case nil
        (my/aaronnote--post
         `((type . "client-close")
           (client . ,client)
           ,@(when (and (stringp file) (not (string-empty-p file)))
               `((file . ,(my/aaronnote--host-file file))))))
      (error nil))))

(defun my/aaronnote--cleanup-buffer ()
  "Remove the current buffer from Aaronnote identity registries."
  (my/aaronnote--notify-client-closed
   my/aaronnote--client-id
   my/aaronnote-buffer-file-name)
  (when (and (stringp my/aaronnote--registered-file)
             (eq (gethash my/aaronnote--registered-file my/aaronnote--file-buffers)
                 (current-buffer)))
    (remhash my/aaronnote--registered-file my/aaronnote--file-buffers))
  (when (and (stringp my/aaronnote--client-id)
             (eq (gethash my/aaronnote--client-id my/aaronnote--client-buffers)
                 (current-buffer)))
    (remhash my/aaronnote--client-id my/aaronnote--client-buffers))
  (when (eq my/aaronnote--app-buffer (current-buffer))
    (setq my/aaronnote--app-buffer nil)))

(defun my/aaronnote--refresh-visible-ibuffers ()
  "Refresh visible ibuffer buffers after Aaronnote identity changes."
  (when (fboundp 'ibuffer-update)
    (dolist (buffer (buffer-list))
      (when (get-buffer-window buffer 'visible)
        (with-current-buffer buffer
          (when (derived-mode-p 'ibuffer-mode)
            (let ((inhibit-message t))
              (revert-buffer nil t))))))))

(defun my/aaronnote--buffer-for-client (client)
  "Return the live Aaronnote buffer for CLIENT, or nil."
  (when (and (stringp client) (not (string-empty-p client)))
    (let ((buffer (gethash client my/aaronnote--client-buffers)))
      (unless (or (null buffer) (buffer-live-p buffer))
        (remhash client my/aaronnote--client-buffers)
        (setq buffer nil))
      (or buffer
          (cl-find-if
           (lambda (buf)
             (and (buffer-live-p buf)
                  (with-current-buffer buf
                    (and (eq major-mode 'xwidget-webkit-mode)
                         (stringp my/aaronnote--client-id)
                         (string-equal my/aaronnote--client-id client)))))
           (buffer-list))))))

(defun my/aaronnote--register-buffer (buffer file &optional client rename)
  "Register BUFFER as the Aaronnote browser for FILE and CLIENT.
When RENAME is non-nil, rename xwidget buffers to a note-specific name."
  (when (buffer-live-p buffer)
    (let* ((file (my/aaronnote--canonical-file file))
           (client (and (stringp client)
                        (not (string-empty-p client))
                        client))
           (split-client (my/aaronnote--split-client-p client))
           changed)
      (with-current-buffer buffer
        (let ((old-file my/aaronnote--registered-file)
              (old-client my/aaronnote--client-id))
          (when (and (stringp old-file)
                     (not (equal old-file file))
                     (eq (gethash old-file my/aaronnote--file-buffers) buffer))
            (remhash old-file my/aaronnote--file-buffers))
          (when (and (stringp old-client)
                     (not (equal old-client client))
                     (eq (gethash old-client my/aaronnote--client-buffers) buffer))
            (remhash old-client my/aaronnote--client-buffers))
          (setq changed (or (not (equal my/aaronnote-buffer-file-name file))
                            (not (equal my/aaronnote--client-id client)))))
        (setq-local my/aaronnote-buffer-file-name file)
        (setq-local my/aaronnote--registered-file file)
        (setq-local my/aaronnote--client-id client)
        (setq-local my/aaronnote--xwidget-forced-name
                    (if split-client
                        (my/aaronnote--split-buffer-display-name
                         file
                         (or (my/aaronnote--split-client-ordinal client) 0))
                      (my/aaronnote--buffer-display-name file)))
        (my/aaronnote-keys-mode 1)
        (when file
          (setq-local default-directory
                      (file-name-as-directory (file-name-directory file)))
          ;; Xwidget buffers acquire their project directory after their
          ;; major-mode hooks have run.  Notify the environment integration at
          ;; the point where the directory actually becomes authoritative.
          (when (fboundp 'my/direnv-schedule-current-buffer)
            (my/direnv-schedule-current-buffer)))
        (add-hook 'kill-buffer-hook #'my/aaronnote--cleanup-buffer nil t)
        (when (and rename
                   (eq major-mode 'xwidget-webkit-mode)
                   (not (equal (buffer-name)
                               my/aaronnote--xwidget-forced-name)))
          (rename-buffer my/aaronnote--xwidget-forced-name t)
          (setq changed t))
        (when changed
          (force-mode-line-update)
          (force-window-update (current-buffer))))
      (when (and file (not (my/aaronnote--split-client-p client)))
        (puthash file buffer my/aaronnote--file-buffers))
      (when client
        (puthash client buffer my/aaronnote--client-buffers))
      (when (and (eq major-mode 'xwidget-webkit-mode)
                 (fboundp 'my/aaronnote--setup-native-chrome))
        (my/aaronnote--setup-native-chrome))
      (when changed
        (my/aaronnote--refresh-visible-ibuffers))
      buffer)))

(defun my/aaronnote--sync-app-buffer-file (file &optional client)
  "Record FILE as the current note in the matching Aaronnote buffer.
CLIENT, when present, identifies the exact xwidget page that reported the
file switch."
  (let* ((file (my/aaronnote--canonical-file file))
         (target (or (my/aaronnote--buffer-for-client client)
                     (and file (my/aaronnote--buffer-for-file file))
                     my/aaronnote--app-buffer)))
    (when (buffer-live-p target)
      (my/aaronnote--register-buffer target file client t)
      (when file
        (setq my/aaronnote--app-buffer target)))))

(defun my/aaronnote--track-app-buffer (buffer &optional file client)
  "Record BUFFER as the active Aaronnote browser buffer.
When FILE is non-nil, set buffer-local file tracking directly."
  (setq my/aaronnote--app-buffer buffer)
  (when (buffer-live-p buffer)
    (my/aaronnote--register-buffer buffer file client t)))

(defun my/aaronnote--buffer-for-file (file)
  "Return a live Aaronnote buffer tracking FILE, or nil."
  (when-let* ((abs (my/aaronnote--canonical-file file)))
    (let ((registered (gethash abs my/aaronnote--file-buffers)))
      (cond
       ((buffer-live-p registered) registered)
       (registered
        (remhash abs my/aaronnote--file-buffers)
        nil)
       (t
        (when-let* ((found
                     (cl-find-if
                      (lambda (buf)
                        (and (buffer-live-p buf)
                             (with-current-buffer buf
                               (and (stringp my/aaronnote-buffer-file-name)
                                    (not (my/aaronnote--split-client-p
                                          my/aaronnote--client-id))
                                    (string-equal
                                     (expand-file-name my/aaronnote-buffer-file-name)
                                     abs)))))
                      (buffer-list))))
          (puthash abs found my/aaronnote--file-buffers)
          found))))))

(defun my/aaronnote-canonical-buffer (&optional buffer)
  "Return the canonical Aaronnote buffer for BUFFER's file, or BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (or (when-let* ((file (my/aaronnote-buffer-file buffer)))
            (my/aaronnote--buffer-for-file file))
          buffer))))

(defun my/aaronnote--open-xwidget (url &optional file)
  "Open Aaronnote in a per-file xwidget session.
Each Markdown FILE gets its own dedicated xwidget session and buffer.
Switching to an already-open file reuses the existing buffer without
reloading.  Non-file opens (roam graph, etc.) share the singleton
\"aaronnote\" session."
  (unless (fboundp 'my/xwidget-open-url)
    (require 'init-browser))
  (let* ((file (my/aaronnote--canonical-file file))
         (id (my/aaronnote--xwidget-session-id file))
         (url (if file
                  (my/aaronnote--app-url file id)
                url))
         (existing (or (and file (my/aaronnote--buffer-for-file file))
                       (and (fboundp 'my/xwidget-session-buffer)
                            (my/xwidget-session-buffer id)))))
    (if existing
        ;; Session already alive for this file: switch to it without reloading.
        (progn
          (switch-to-buffer existing)
          (with-current-buffer existing
            (when (fboundp 'my/xwidget-setup-control-line)
              (my/xwidget-setup-control-line)))
          (run-at-time 0.3 nil #'my/xwidget-focus existing)
          (my/aaronnote--track-app-buffer existing file id)
          existing)
      ;; New session: open directly at the target URL.
      (let ((buffer (my/xwidget-open-url url
                                         :id id
                                         :display 'current
                                         :reuse-selected t)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local my/xwidget-focus-script my/aaronnote--xwidget-focus-script)
            (when (fboundp 'my/xwidget-setup-control-line)
              (my/xwidget-setup-control-line))))
        (my/aaronnote--track-app-buffer buffer file id)
        buffer))))

(defun my/aaronnote--open-appine (url &optional file force-new)
  "Open Aaronnote URL in Appine, one Appine tab per md file.
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
    (my/aaronnote--track-app-buffer buffer file)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local header-line-format '(:eval (my/aaronnote--header-line)))
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

(defun my/aaronnote--appine-available-p ()
  "Return non-nil when Aaronnote can dispatch opens through Appine."
  (condition-case err
      (progn
        (unless (fboundp 'my/appine-open-url)
          (require 'init-appine))
        (fboundp 'my/appine-open-url))
    (error
     (message "Aaronnote: Appine unavailable (%s)"
              (error-message-string err))
     nil)))

(defun my/aaronnote--open-url (url &optional file force-new)
  "Open Aaronnote URL using `my/aaronnote-backend'.
FORCE-NEW, when non-nil, asks the Appine backend for a fresh tab instead of
reusing a remembered one."
  (pcase my/aaronnote-backend
    ('appine
     (if (my/aaronnote--appine-available-p)
         (my/aaronnote--open-appine url file force-new)
       (message "Aaronnote: using xwidget because Appine is unavailable")
       (my/aaronnote--open-xwidget url file)))
    ('xwidget (my/aaronnote--open-xwidget url file))
    (_ (user-error "Unsupported Aaronnote backend: %S" my/aaronnote-backend))))

(defun my/aaronnote--post (payload)
  "Send small control PAYLOAD to the Aaronnote web-host."
  (when-let* ((client
               (and my/aaronnote--ready
                    (remote-gateway-find-client "aaronnote"))))
    (remote-gateway-notify client "aaronnote.command" payload)))

(defun my/aaronnote--open-file-in-web (file)
  "Ask the already open Aaronnote page to open FILE."
  (my/aaronnote--sync-app-buffer-file file)
  (my/aaronnote--post
   `((type . "open") (file . ,(my/aaronnote--host-file file)))))

(defun my/aaronnote--send-command (command &optional detail)
  "Dispatch Aaronnote COMMAND with optional DETAIL."
  (let ((client (and (boundp 'my/aaronnote--client-id)
                     (stringp my/aaronnote--client-id)
                     (not (string-empty-p my/aaronnote--client-id))
                     my/aaronnote--client-id)))
    (my/aaronnote--post
     `((type . "command")
       (command . ,command)
       ,@(when client `((client . ,client)))
       ,@(when detail `((detail . ,detail)))))))

(defun my/aaronnote--goto-location (file line col)
  "Open FILE in Emacs and move to one-based LINE and zero-based COL.
When FILE is nil, use the current buffer."
  (let* ((abs (and (stringp file)
                   (not (string-empty-p file))
                   (ignore-errors
                     (my/aaronnote--canonical-file
                      (file-truename
                       (my/aaronnote--canonical-file file))))))
         (key (list abs (truncate (or line 1)) (truncate (or col 0)))))
    (let ((same-location (equal key my/aaronnote--goto-last))
          (buffer (if abs
                      (or (find-buffer-visiting abs)
                          (find-file-noselect abs))
                    (current-buffer))))
      (setq my/aaronnote--goto-last key)
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
(defun my/aaronnote-open-file (file)
  "Open Markdown FILE in Aaronnote Web/Appine."
  (interactive "fMarkdown file: ")
  (unless (my/aaronnote--markdown-file-p file)
    (user-error "Aaronnote opens Markdown files, not %s" file))
  (let ((file (my/aaronnote--canonical-file file))
        (target-window (selected-window)))
    (my/aaronnote--ensure-server
     (lambda ()
      (when (window-live-p target-window)
        (select-window target-window))
      (my/aaronnote--open-url
       (my/aaronnote--app-url file (my/aaronnote--xwidget-session-id file))
       file
       t)))))

;;;###autoload
(defun my/aaronnote-open-current-note ()
  "Open the current Markdown note in Aaronnote Web/Appine."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (my/aaronnote-open-file buffer-file-name))

(defun my/aaronnote--current-note-file ()
  "Return the Markdown note represented by the current context."
  (or (my/aaronnote-buffer-file)
      (and buffer-file-name
           (my/aaronnote--markdown-file-p buffer-file-name)
           buffer-file-name)))

(defun my/aaronnote--split-window ()
  "Create and select the window for an Aaronnote split."
  (let ((window (if (>= (window-total-width) 120)
                    (split-window-right)
                  (split-window-below))))
    (select-window window)
    window))

;;;###autoload
(defun my/aaronnote-open-current-note-split ()
  "Open the current Markdown note in a fresh editable Aaronnote xwidget split.

This intentionally does not reuse the canonical Aaronnote xwidget for the
file.  Multiple xwidget windows for the same live session have rendering
issues, so this command creates an isolated editable client while keeping the
normal file/session reuse map owned by the canonical pane."
  (interactive)
  (let ((file (my/aaronnote--current-note-file)))
    (unless (and file (my/aaronnote--markdown-file-p file))
      (user-error "No current Markdown note for Aaronnote"))
    (let ((file (my/aaronnote--canonical-file file))
          (source-window (selected-window)))
      (my/aaronnote--ensure-server
       (lambda ()
         (when (window-live-p source-window)
           (select-window source-window))
         (unless (fboundp 'my/xwidget-open-url)
           (require 'init-browser))
         (let* ((ordinal (cl-incf my/aaronnote--split-counter))
                (client (format "aaronnote-split:%s:%d"
                                (file-truename file)
                                ordinal))
                (url (my/aaronnote--app-url file client))
                (target-window (my/aaronnote--split-window))
                (buffer (my/xwidget-open-url
                         url
                         :id client
                         :display 'current
                         :force-new t
                         :reuse-selected t)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq-local my/aaronnote-buffer-file-name file)
               (setq-local my/aaronnote--client-id client)
               (setq-local my/aaronnote--registered-file nil)
               (setq-local my/aaronnote--xwidget-forced-name
                           (my/aaronnote--split-buffer-display-name
                            file ordinal))
               (setq-local my/xwidget-focus-script my/aaronnote--xwidget-focus-script)
               (puthash client (current-buffer) my/aaronnote--client-buffers)
               (add-hook 'kill-buffer-hook #'my/aaronnote--cleanup-buffer nil t)
               (when (fboundp 'my/xwidget-setup-control-line)
                 (my/xwidget-setup-control-line))
               ;; `xwidget-webkit-browse-url' may return before its buffer has
               ;; finished switching to `xwidget-webkit-mode'.  Naming does
               ;; not depend on the major mode, and delaying it leaves the
               ;; buffer permanently named *xwidget* because the title
               ;; callback correctly avoids overriding Aaronnote-owned names.
               (rename-buffer my/aaronnote--xwidget-forced-name t)
               (when file
                 (setq-local default-directory
                             (file-name-as-directory (file-name-directory file)))
                 (when (fboundp 'my/direnv-schedule-current-buffer)
                   (my/direnv-schedule-current-buffer)))
               (my/aaronnote-keys-mode 1)))
           (my/aaronnote--refresh-visible-ibuffers)
           (when (window-live-p target-window)
             (select-window target-window))))))))

;;;###autoload
(defun my/aaronnote-preview ()
  "Compatibility alias: open the current note in Aaronnote."
  (interactive)
  (my/aaronnote-open-current-note))

;;;###autoload
(defun my/aaronnote-sync-cursor ()
  "Open the current note in Aaronnote.
Cursor-level sync is intentionally no longer a per-keystroke preview channel."
  (interactive)
  (my/aaronnote-open-current-note))

;;;###autoload
(defun my/aaronnote-refresh ()
  "Refresh the current Aaronnote note while preserving page cursor state."
  (interactive)
  (if (and my/aaronnote--ready
           (or (and (boundp 'my/aaronnote--client-id)
                    (stringp my/aaronnote--client-id)
                    (not (string-empty-p my/aaronnote--client-id)))
               (buffer-live-p my/aaronnote--app-buffer)))
      (progn
        (my/aaronnote-command "refresh"))
    (my/aaronnote-open-current-note)))

;;;###autoload
(defun my/aaronnote-command (command &optional detail)
  "Send COMMAND with optional DETAIL to the open Aaronnote page."
  (interactive "sAaronnote command: ")
  (my/aaronnote--ensure-server
   (lambda ()
     (my/aaronnote--send-command command detail))))

;;;###autoload
(defun my/aaronnote-escape ()
  "Tell Aaronnote to handle Escape."
  (interactive)
  (my/aaronnote-command "escape"))

;;;###autoload
(defun my/aaronnote-save ()
  "Tell Aaronnote to save the current note."
  (interactive)
  (my/aaronnote-command "save"))

;;;###autoload
(defun my/aaronnote-focus ()
  "Tell Aaronnote to focus its editor."
  (interactive)
  (my/aaronnote-command "focus"))

;;;###autoload
(defun my/aaronnote-roam-graph ()
  "Open the standalone roam graph view in Aaronnote.
Always opens a fresh tab so it reliably reappears even after the previous
graph tab was closed via the Appine toolbar."
  (interactive)
  (my/aaronnote--ensure-server
   (lambda ()
     (my/aaronnote--open-url (my/aaronnote--server-url "/graph") nil t))))

;;; Pause/resume — freeze WebKit animations when Aaronnote is not visible.

(defvar my/aaronnote--paused nil
  "Non-nil when the browser page has been sent a pause command.")
(defvar my/aaronnote--manual-paused nil
  "Non-nil when Aaronnote was paused explicitly by the user.")
(defvar my/aaronnote--activity-timer nil
  "Debounce timer for `my/aaronnote--update-activity'.")
(defvar my/aaronnote--activity-hooks-installed nil
  "Non-nil when Aaronnote pause/resume activity hooks are installed.")
(defvar my/aaronnote--last-activity-active :unknown
  "Last active-state scheduled by `my/aaronnote--update-activity'.")

(defconst my/aaronnote--core-ready-script
  "(() => {
  const connection = window.aaronnoteApi && window.aaronnoteApi.connection;
  if (!connection || typeof connection.reconnect !== 'function') return false;
  connection.reconnect('host-ready');
  return true;
})()"
  "JavaScript used to reconnect a retained xwidget page after core restarts.")

(defun my/aaronnote--app-buffer-visible-p ()
  "Return non-nil when the Aaronnote buffer is visible in a focused frame."
  (when (buffer-live-p my/aaronnote--app-buffer)
    (let ((win (get-buffer-window my/aaronnote--app-buffer 'visible)))
      (and win
           (frame-focus-state (window-frame win))))))

(defun my/aaronnote--notify-xwidgets-core-ready ()
  "Reconnect retained Aaronnote xwidgets after an active core restart."
  (when (and (fboundp 'xwidget-webkit-current-session)
             (fboundp 'xwidget-webkit-execute-script))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (my/aaronnote--xwidget-buffer-p buffer))
        (with-current-buffer buffer
          (when-let* ((session (ignore-errors
                                 (xwidget-webkit-current-session))))
            (ignore-errors
              (xwidget-webkit-execute-script
               session my/aaronnote--core-ready-script))))))))

(defun my/aaronnote--maybe-reconnect-core-on-activity ()
  "Restart a disconnected core only while an Aaronnote browser is active.
This is intentionally called from focus/window activity, never from an idle
timer or retry loop. The old port is reclaimed so the browser page and its
unsaved CodeMirror state remain intact."
  (when (and (buffer-live-p my/aaronnote--app-buffer)
             (my/aaronnote--app-buffer-visible-p)
             (not my/aaronnote--ready)
             (not (and (processp my/aaronnote--process)
                       (process-live-p my/aaronnote--process)))
             (integerp my/aaronnote--last-port)
             (> my/aaronnote--last-port 0))
    (unless (memq #'my/aaronnote--notify-xwidgets-core-ready
                  my/aaronnote--ready-callbacks)
      (push #'my/aaronnote--notify-xwidgets-core-ready
            my/aaronnote--ready-callbacks))
    (condition-case err
        (progn
          (my/aaronnote--start-server my/aaronnote--last-port)
          ;; Diagnostic deadline only; it never performs another reconnect.
          (when my/aaronnote--ready-watchdog
            (cancel-timer my/aaronnote--ready-watchdog))
          (setq my/aaronnote--ready-watchdog
                (run-at-time 10 nil #'my/aaronnote--watchdog-fire)))
      (error
       (setq my/aaronnote--ready-callbacks
             (delq #'my/aaronnote--notify-xwidgets-core-ready
                   my/aaronnote--ready-callbacks))
       (message "Aaronnote: active core reconnect failed: %s"
                (error-message-string err))))))

(defun my/aaronnote--apply-activity (active)
  "Send pause or resume to the browser when the active state changes."
  (let ((effective-active (and active (not my/aaronnote--manual-paused))))
    (unless (eq (not effective-active) my/aaronnote--paused)
      (setq my/aaronnote--paused (not effective-active))
      (my/aaronnote--send-command (if effective-active "resume" "pause")))))

;;;###autoload
(defun my/aaronnote-pause ()
  "Pause Aaronnote assist rendering until explicitly resumed."
  (interactive)
  (setq my/aaronnote--manual-paused t)
  (my/aaronnote--apply-activity nil))

;;;###autoload
(defun my/aaronnote-resume ()
  "Resume Aaronnote assist rendering when the app buffer is visible."
  (interactive)
  (setq my/aaronnote--manual-paused nil)
  (my/aaronnote--apply-activity (my/aaronnote--app-buffer-visible-p)))

;;;###autoload
(defun my/aaronnote-toggle-pause ()
  "Toggle manual pause for Aaronnote assist rendering."
  (interactive)
  (if my/aaronnote--manual-paused
      (my/aaronnote-resume)
    (my/aaronnote-pause)))

(defun my/aaronnote--update-activity (&rest _)
  "Debounced check: pause or resume the browser based on buffer visibility.
Also tracks which Aaronnote buffer is currently focused so key forwarding
routes to the right session when multiple files are open."
  ;; Update the active buffer pointer immediately on window-selection changes.
  (let ((cur (current-buffer)))
    (when (my/aaronnote--xwidget-buffer-p cur)
      (setq my/aaronnote--app-buffer cur)))
  (my/aaronnote--maybe-reconnect-core-on-activity)
  (let ((active (my/aaronnote--app-buffer-visible-p)))
    (unless (eq active my/aaronnote--last-activity-active)
      (setq my/aaronnote--last-activity-active active)
      (when my/aaronnote--activity-timer
        (cancel-timer my/aaronnote--activity-timer))
      (setq my/aaronnote--activity-timer
            (if active
                (run-with-idle-timer
                 0.3 nil
                 (lambda ()
                   (setq my/aaronnote--activity-timer nil)
                   (when my/aaronnote--ready
                     (my/aaronnote--apply-activity
                      (my/aaronnote--app-buffer-visible-p)))))
              (run-at-time
               0.05 nil
               (lambda ()
                 (setq my/aaronnote--activity-timer nil)
                 (when my/aaronnote--ready
                   (my/aaronnote--apply-activity
                    (my/aaronnote--app-buffer-visible-p))))))))))

(defun my/aaronnote--install-activity-hooks ()
  "Add hooks that trigger the pause/resume check."
  (unless my/aaronnote--activity-hooks-installed
    (add-function :after after-focus-change-function
                  #'my/aaronnote--update-activity)
    (add-hook 'window-buffer-change-functions #'my/aaronnote--update-activity)
    (add-hook 'window-selection-change-functions #'my/aaronnote--update-activity)
    (setq my/aaronnote--activity-hooks-installed t)))

(defun my/aaronnote--remove-activity-hooks ()
  "Remove pause/resume hooks and cancel any pending debounce timer."
  (remove-function after-focus-change-function #'my/aaronnote--update-activity)
  (remove-hook 'window-buffer-change-functions #'my/aaronnote--update-activity)
  (remove-hook 'window-selection-change-functions #'my/aaronnote--update-activity)
  (when my/aaronnote--activity-timer
    (cancel-timer my/aaronnote--activity-timer)
    (setq my/aaronnote--activity-timer nil))
  (setq my/aaronnote--paused nil
        my/aaronnote--manual-paused nil
        my/aaronnote--last-activity-active :unknown
        my/aaronnote--activity-hooks-installed nil))

;;;###autoload
(defun my/aaronnote-stop ()
  "Kill the Aaronnote web-host process and reset Appine tab state.
The web-host (Node) is the backend; once it is gone, any Appine tabs showing
its pages are dead, so the Emacs-side tab registry is cleared too."
  (interactive)
  (my/aaronnote--remove-activity-hooks)
  (when (fboundp 'my/aaronnote-roam--cancel-sync-timer)
    (my/aaronnote-roam--cancel-sync-timer))
  (when my/aaronnote--ready-watchdog
    (cancel-timer my/aaronnote--ready-watchdog)
    (setq my/aaronnote--ready-watchdog nil))
  (when my/aaronnote--goto-timer
    (cancel-timer my/aaronnote--goto-timer)
    (setq my/aaronnote--goto-timer nil
          my/aaronnote--goto-last nil))
  (my/aaronnote--clear-external-file-watches)
  (when my/aaronnote--gateway-binding
    (remote-gateway-release-binding
     my/aaronnote--gateway-binding t)
    (setq my/aaronnote--gateway-binding nil))
  (let ((proc my/aaronnote--process))
    (setq my/aaronnote--process nil
          my/aaronnote--port nil
          my/aaronnote--ready nil
          my/aaronnote--ready-callbacks nil)
    (when (and proc (process-live-p proc))
      (ignore-errors (signal-process proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p proc)
            (delete-process proc))))))
  (when (fboundp 'my/appine--tab-reset)
    (my/appine--tab-reset))
  (message "Aaronnote web-host stopped."))

(defun my/aaronnote--kill-browser-buffers ()
  "Kill Emacs buffers that host Aaronnote browser pages."
  (mapc
   (lambda (buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (or my/aaronnote-buffer-file-name
                   my/aaronnote--client-id
                   (and (derived-mode-p 'xwidget-webkit-mode)
                        (string-prefix-p "*aaronnote" (buffer-name buffer))))
           (kill-buffer buffer)))))
   (buffer-list))
  (setq my/aaronnote--app-buffer nil)
  (clrhash my/aaronnote--file-buffers)
  (clrhash my/aaronnote--client-buffers))

;;;###autoload
(defun my/aaronnote-close ()
  "Completely close Aaronnote browser surfaces and stop the web-host."
  (interactive)
  (when (fboundp 'my/appine-kill-all)
    (ignore-errors (my/appine-kill-all)))
  (my/aaronnote--kill-browser-buffers)
  (my/aaronnote-stop))

;;;###autoload
(defun my/aaronnote-build-and-reopen ()
  "Build Aaronnote web assets, restart the runtime, and reopen the current note."
  (interactive)
  (when (and my/aaronnote--build-process
             (process-live-p my/aaronnote--build-process))
    (user-error "Aaronnote build is already running"))
  (let* ((file (my/aaronnote--current-note-file))
         (buffer (get-buffer-create "*aaronnote-build*"))
         (default-directory user-emacs-directory))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Aaronnote: building web assets...")
    (setq my/aaronnote--build-process
          (make-process
           :name "aaronnote-build"
           :buffer buffer
           :command '("make" "aaronnote-build")
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((ok (= (process-exit-status proc) 0)))
                 (setq my/aaronnote--build-process nil)
                 (if ok
                     (progn
                       (my/aaronnote-close)
                       (message "Aaronnote: build finished; reopening...")
                       (if (and file (my/aaronnote--markdown-file-p file))
                           (my/aaronnote-open-file file)
                         (my/aaronnote--ensure-server
                          (lambda ()
                            (my/aaronnote--open-url
                             (my/aaronnote--app-url nil "aaronnote") nil t)))))
                   (display-buffer buffer)
                   (message "Aaronnote: build failed; see %s" (buffer-name buffer))))))))
    (display-buffer buffer)))

(add-hook 'kill-emacs-hook #'my/aaronnote-stop)

;;; API call — request the web-host over the shared gateway.

(defun my/aaronnote--gateway-hash-value (value)
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
         (my/aaronnote--gateway-hash-value (cdr item))
         table))))
   ((listp value)
    (mapcar #'my/aaronnote--gateway-hash-value value))
   ((vectorp value)
    (vconcat
     (mapcar #'my/aaronnote--gateway-hash-value value)))
   (t value)))

(defun my/aaronnote--api-call-sync (channel args)
  "Call CHANNEL with ARGS synchronously; return parsed JSON or nil.
Only usable when the web-host is running (`my/aaronnote--ready' is non-nil).
Blocks the caller until the response arrives (or 8 s timeout)."
  (when-let* ((client
               (and my/aaronnote--ready
                    (remote-gateway-find-client "aaronnote")))
              (result
               (remote-gateway-request-sync
                client "aaronnote.api"
                `((channel . ,channel) (args . ,args))
                8)))
    (my/aaronnote--gateway-hash-value result)))

(defun my/aaronnote--api-call (channel args callback)
  "Call CHANNEL with ARGS and asynchronously invoke CALLBACK."
  (when-let* ((client
               (and my/aaronnote--ready
                    (remote-gateway-find-client "aaronnote"))))
    (remote-gateway-request-async
     client "aaronnote.api"
     `((channel . ,channel) (args . ,args))
     (lambda (result error-object)
       (if error-object
           (message
            "Aaronnote API error %s: %s"
            (or (alist-get "code" error-object nil nil #'string=)
                "unknown")
            (or (alist-get "message" error-object nil nil #'string=)
                "request failed"))
         (funcall callback result)))
     10)))

(defun my/aaronnote-runtime-status ()
  "Display the Aaronnote runtime debug snapshot."
  (interactive)
  (unless my/aaronnote--ready
    (user-error "Aaronnote web-host is not ready"))
  (let ((payload (my/aaronnote--api-call-sync
                  "aaronnote:api:runtime:debug" [])))
    (unless payload
      (user-error "Aaronnote runtime status unavailable"))
    (puthash "emacsActivity"
             (let ((activity (make-hash-table :test 'equal)))
               (puthash "paused" (if my/aaronnote--paused t :false) activity)
               (puthash "manualPaused" (if my/aaronnote--manual-paused t :false) activity)
               (puthash "bufferVisible" (if (my/aaronnote--app-buffer-visible-p) t :false) activity)
               activity)
             payload)
    (with-current-buffer (get-buffer-create "*aaronnote-runtime-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (json-serialize payload
                                :false-object :false
                                :null-object nil))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun my/aaronnote-roam-sync ()
  "Sync the Roam DB and show statistics in the minibuffer."
  (interactive)
  (unless my/aaronnote--ready
    (user-error "Aaronnote: server not running"))
  (message "Aaronnote: syncing Roam DB...")
  (my/aaronnote--api-call
   "aaronnote:api:notes:roam-sync" [t]
   (lambda (result)
     (let* ((stats (alist-get 'stats result))
            (notes (or (alist-get 'noteCount stats) 0))
            (links (or (alist-get 'linkCount stats) 0))
            (tags  (or (alist-get 'tagCount stats) 0))
            (dirs  (or (alist-get 'dirCount stats) 0)))
       (setq my/aaronnote--last-sync-stats
             (format "%d notes · %d links · %d tags · %d dirs"
                     notes links tags dirs))
       (when (fboundp 'my/aaronnote-roam--clear-runtime-cache)
         (my/aaronnote-roam--clear-runtime-cache))
       (message "Roam synced: %s" my/aaronnote--last-sync-stats)))))

;;; Header-line for the Aaronnote app buffer.

(defun my/aaronnote-editor-menu (event)
  "Open Aaronnote editor actions from the native pencil button at EVENT."
  (interactive "e")
  (my/xwidget--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    "AaronNote"
    (list
     ["Focus editor" my/aaronnote-focus t]
     ["Task manager" my/xwidget-open-task-manager t]
     "---"
     ["Page outline" my/aaronnote-toggle-page t]
     ["Agenda" my/aaronnote-toggle-agenda t]
     ["Local graph" my/aaronnote-toggle-graph t]
     ["Tools" my/aaronnote-toggle-tools t]
     ["Jupyter cells" my/aaronnote-toggle-jupyter t]
     "---"
     ["Toggle source" my/aaronnote-toggle-source t]
     ["Save" my/aaronnote-save t]))
   event))

(dolist (entry '((my/aaronnote-toggle-page . "toggle-toc")
                 (my/aaronnote-toggle-agenda . "toggle-agenda")
                 (my/aaronnote-toggle-graph . "toggle-graph")
                 (my/aaronnote-toggle-tools . "toggle-tools")
                 (my/aaronnote-toggle-jupyter . "jupyter-panel")))
  (let ((fn (car entry)) (command (cdr entry)))
    (fset fn (lambda () (interactive) (my/aaronnote-command command)))))

(defun my/aaronnote--header-browser-buttons ()
  "Return native xwidget controls with Aaronnote actions under the pencil."
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
    #'my/aaronnote-editor-menu "AaronNote actions" 'header-line)
   (my/xwidget--nav-button
    (my/xwidget--mode-line-icon 'codicon "nf-cod-layout" "win")
    #'my/xwidget-window-menu "Window menu" 'header-line)))

(defun my/aaronnote--header-line ()
  "Return native browser and editor controls for an Aaronnote buffer."
  (let* ((file (my/aaronnote-buffer-file (current-buffer)))
         (name (if file (file-name-nondirectory file) "Aaronnote")))
    (append
     (list " ")
     (my/aaronnote--header-browser-buttons)
     (list "  " (propertize name 'face 'mode-line-buffer-id)))))

(defun my/aaronnote--setup-native-chrome ()
  "Install Aaronnote-only Emacs chrome in the current xwidget buffer."
  (setq-local header-line-format '(:eval (my/aaronnote--header-line)))
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update t))

(defun my/aaronnote--restore-native-chrome-h ()
  "Restore Aaronnote chrome after generic xwidget mode initialization."
  (when (my/aaronnote--xwidget-buffer-p)
    (my/aaronnote--setup-native-chrome)))

(defun my/aaronnote--restore-native-chrome-later-h ()
  "Restore Aaronnote chrome after all xwidget mode hooks have settled."
  (let ((buffer (current-buffer)))
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (my/aaronnote--restore-native-chrome-h)))))))

(with-eval-after-load 'init-browser
  (advice-add 'my/xwidget-setup-control-line :after
              #'my/aaronnote--restore-native-chrome-h)
  (with-eval-after-load 'xwidget
    (add-hook 'xwidget-webkit-mode-hook
              #'my/aaronnote--restore-native-chrome-later-h 90)))

;;;###autoload
(defun my/aaronnote-pop ()
  "Open the Aaronnote command pop."
  (interactive)
  (require 'transient)
  (call-interactively #'my/aaronnote-dispatch))

(defun my/aaronnote--xwidget-menu-section ()
  "Return Aaronnote actions for the xwidget top-bar popup."
  (when (or my/aaronnote-buffer-file-name
            my/aaronnote--client-id)
    (list
     "---"
     ["Aaronnote: Refresh current pane" my/aaronnote-refresh t]
     ["Aaronnote: Open editable split" my/aaronnote-open-current-note-split t]
     ["Aaronnote: Focus editor" my/aaronnote-focus t]
     ["Aaronnote: Pop" my/aaronnote-pop t]
     (list
      "Aaronnote lifecycle"
      ["Build + reopen" my/aaronnote-build-and-reopen t]
      ["Close all Aaronnote" my/aaronnote-close t]))))

(with-eval-after-load 'init-browser
  (add-to-list 'my/xwidget-window-menu-extra-sections
               #'my/aaronnote--xwidget-menu-section))

;;; Web-editor command wrappers.
;; These generate named interactive commands for every web-host editor command
;; so each entry in the dispatch hub is `commandp', appears in M-x, and can
;; be verified with `commandp' in batch tests.

(defmacro my/aaronnote--def-editor-cmd (suffix command &optional doc)
  "Define `my/aaronnote-SUFFIX' that sends editor COMMAND to the web page."
  `(defun ,(intern (format "my/aaronnote-%s" suffix)) ()
     ,(or doc (format "Send the Aaronnote `%s' editor command." command))
     (interactive)
     (my/aaronnote-command ,command)))

(my/aaronnote--def-editor-cmd "toggle-source"   "toggle-source"   "Toggle source / rendered view.")
(my/aaronnote--def-editor-cmd "undo"            "undo"            "Undo last edit in Aaronnote.")
(my/aaronnote--def-editor-cmd "redo"            "redo"            "Redo last undone edit in Aaronnote.")
(my/aaronnote--def-editor-cmd "paste"           "paste"           "Paste through Aaronnote's editor pipeline.")
(my/aaronnote--def-editor-cmd "bold"            "bold"            "Toggle bold at point.")
(my/aaronnote--def-editor-cmd "italic"          "italic"          "Toggle italic at point.")
(my/aaronnote--def-editor-cmd "code-inline"     "code"            "Toggle inline code at point.")
(my/aaronnote--def-editor-cmd "highlight"       "highlight"       "Toggle highlight at point.")
(my/aaronnote--def-editor-cmd "strike"          "strike"          "Toggle strikethrough at point.")
(my/aaronnote--def-editor-cmd "superscript"     "superscript"     "Wrap the selection as Markdown superscript.")
(my/aaronnote--def-editor-cmd "subscript"       "subscript"       "Wrap the selection as Markdown subscript.")
(my/aaronnote--def-editor-cmd "insert-footnote" "insert-footnote" "Insert a numbered Markdown footnote.")
(my/aaronnote--def-editor-cmd "insert-revision" "insert-revision" "Insert an Aaronnote revision suggestion.")
(my/aaronnote--def-editor-cmd "edit-properties" "edit-properties" "Open the native org-meta properties panel.")
(my/aaronnote--def-editor-cmd "move-block-up"   "move-block-up"   "Move the current Markdown block upward.")
(my/aaronnote--def-editor-cmd "move-block-down" "move-block-down" "Move the current Markdown block downward.")
(my/aaronnote--def-editor-cmd "blockquote"      "blockquote"      "Toggle blockquote on paragraph.")
(my/aaronnote--def-editor-cmd "bullet-list"     "bullet-list"     "Toggle bullet list.")
(my/aaronnote--def-editor-cmd "ordered-list"    "ordered-list"    "Toggle ordered list.")
(my/aaronnote--def-editor-cmd "task-list"       "task-list"       "Toggle task/checkbox list.")
(my/aaronnote--def-editor-cmd "code-block"      "code-block"      "Insert/toggle fenced code block.")
(my/aaronnote--def-editor-cmd "paragraph-menu"  "paragraph-menu"  "Open heading/paragraph type menu.")
(my/aaronnote--def-editor-cmd "insert-table"    "insert-table"    "Insert a Markdown table.")
(my/aaronnote--def-editor-cmd "insert-math"     "insert-math-block" "Insert a math block.")
(my/aaronnote--def-editor-cmd "insert-toc"      "insert-toc"      "Insert a table of contents.")
(my/aaronnote--def-editor-cmd "prose-check"     "prose-check"     "Run a bounded LanguageTool check in Aaronnote.")

;;; Dispatch transient.

(defun my/aaronnote--dispatch-header ()
  "Header string for the Aaronnote dispatch transient."
  (let ((status (cond
                 ((not my/aaronnote--ready)
                  (propertize "offline" 'face 'error))
                 (t (propertize (format "port %d" my/aaronnote--port)
                                'face 'success))))
        (sync (or my/aaronnote--last-sync-stats "not synced")))
    (format "Aaronnote  [%s]  %s" status sync)))

(with-eval-after-load 'transient
  (transient-define-prefix my/aaronnote-dispatch ()
    "Aaronnote note-editor and roam hub.  H-o from anywhere."
    [:description my/aaronnote--dispatch-header
     ;; Row 1 ─────────────────────────────────────────────────────────────────
     ["Note (web)"
      ("o" "open current"     my/aaronnote-open-current-note)
      ("O" "open file…"       my/aaronnote-open-file)
      ("s" "save"             my/aaronnote-save)
      ("r" "refresh"          my/aaronnote-refresh)
      ("f" "focus editor"     my/aaronnote-focus)
      ("e" "escape/normal"    my/aaronnote-escape)
      ("v" "toggle source"    my/aaronnote-toggle-source)
      ("W" "editable split"   my/aaronnote-open-current-note-split)
      ("B" "build + reopen"   my/aaronnote-build-and-reopen)
      ("Q" "close all"        my/aaronnote-close)
      ("R" "raw edit in Emacs" my/aaronnote-open-markdown-raw)]
     ["Find / Browse"
      ("j" "find note"        my/aaronnote-roam-find-note)
      ("/" "search…"          my/aaronnote-roam-search-notes)
      ("l" "recent notes"     my/aaronnote-roam-recent-notes)
      ("." "follow link"      my/aaronnote-roam-follow-link)
      ("b" "backlinks"        my/aaronnote-roam-backlinks)
      ("x" "related notes"    my/aaronnote-roam-related-notes)
      ("G" "goto definition"  my/aaronnote-roam-goto-definition)]
     ["Insert"
      ("i" "roam link"        my/aaronnote-roam-insert-link)
      ("I" "TOC link"         my/aaronnote-roam-insert-toc-link)
      ("t" "tag id"           my/aaronnote-roam-insert-tag-id)
      ("T" "tag-id link"      my/aaronnote-roam-insert-tag-id-link)
      ("w" "copy link here"   my/aaronnote-roam-copy-link-to-here)
      ("c" "note-code"        my/note-code-insert)]
     ;; Row 2 ─────────────────────────────────────────────────────────────────
     ["Knowledge"
      ("n" "new note"         my/aaronnote-roam-new-node)
      ("d" "daily note"       my/aaronnote-roam-daily-note)
      ("a" "browse tags"      my/aaronnote-roam-tags)
      ("C" "categories"       my/aaronnote-roam-categories)
      ("g" "roam graph"       my/aaronnote-roam-graph)
      ("k" "tasks"            my/aaronnote-roam-todos)
      ("A" "agenda"           my/aaronnote-roam-agenda)
      ("L" "agenda log"       my/aaronnote-roam-agenda-log)
      ("F" "file todos"       my/aaronnote-roam-jump-file-todo)
      ("M" "management"       my/aaronnote-roam-management)]
     ["Special pages (wiki)"
      ("!" "reports hub"      my/aaronnote-roam-reports)
      ("!w" "wanted pages"    my/aaronnote-roam-report-wanted)
      ("!o" "orphaned"        my/aaronnote-roam-report-orphaned)
      ("!d" "dead-end"        my/aaronnote-roam-report-dead-end)
      ("!u" "uncategorized"   my/aaronnote-roam-report-uncategorized)
      ("!h" "most-linked"     my/aaronnote-roam-report-most-linked)]
     ["Index / Files"
      ("y" "sync DB"          my/aaronnote-roam-sync)
      ("u" "update index"     my/aaronnote-roam-update-db)
      ("Z" "full rebuild"     my/aaronnote-roam-sync-full)
      ("S" "DB status"        my/aaronnote-roam-db-status)
      ("P" "pause/resume"     my/aaronnote-toggle-pause)
      ("R" "runtime status"   my/aaronnote-runtime-status)
      ("D" "dired"            my/aaronnote-roam-dired)
      ("m" "move note"        my/aaronnote-roam-move-note)
      ("V" "magit"            my/aaronnote-roam-magit)
      ("q" "stop server"      my/aaronnote-stop)]
     ["Publish"
      ("X"  "build + deploy"  my/aaronnote-publish)
      ("xb" "build only"      my/aaronnote-publish-build)
      ("xd" "deploy only"     my/aaronnote-publish-deploy)
      ("xc" "clean cache"     my/aaronnote-publish-clean)]
     ["Format (web)"
      ("1" "bold"             my/aaronnote-bold)
      ("2" "italic"           my/aaronnote-italic)
      ("3" "code inline"      my/aaronnote-code-inline)
      ("4" "highlight"        my/aaronnote-highlight)
      ("5" "strike"           my/aaronnote-strike)
      ("^" "superscript"      my/aaronnote-superscript)
      ("_" "subscript"        my/aaronnote-subscript)
      ("N" "footnote"         my/aaronnote-insert-footnote)
      ("K" "revision"         my/aaronnote-insert-revision)
      ("@" "properties"       my/aaronnote-edit-properties)
      ("[" "move block up"    my/aaronnote-move-block-up)
      ("]" "move block down"  my/aaronnote-move-block-down)
      ("6" "blockquote"       my/aaronnote-blockquote)
      ("7" "bullet list"      my/aaronnote-bullet-list)
      ("8" "ordered list"     my/aaronnote-ordered-list)
      ("9" "task list"        my/aaronnote-task-list)
      ("0" "code block"       my/aaronnote-code-block)
      ("p" "heading menu"     my/aaronnote-paragraph-menu)
      ("z" "insert table"     my/aaronnote-insert-table)
      ("E" "math block"       my/aaronnote-insert-math)
      ("C" "insert TOC"       my/aaronnote-insert-toc)
      ("U" "undo"             my/aaronnote-undo)
      ("Y" "redo"             my/aaronnote-redo)
      ("V" "paste"            my/aaronnote-paste)]]))

;;; Keybindings.

;; Global: H-o opens the Aaronnote dispatch panel.
(general-define-key "H-o" #'my/aaronnote-dispatch)
(general-define-key "C-H-o" #'my/aaronnote-dispatch)

;; Appine buffer direct keys — override global H- bindings that are irrelevant
;; when focused in the Aaronnote pane.
(with-eval-after-load 'appine
  (when (boundp 'appine-active-map)
    (define-key appine-active-map (kbd "H-o") #'my/aaronnote-dispatch)
    (define-key appine-active-map (kbd "C-H-o") #'my/aaronnote-dispatch)
    (define-key appine-active-map (kbd "M-z") #'my/aaronnote-undo)
    (define-key appine-active-map (kbd "M-Z") #'my/aaronnote-redo)
    (define-key appine-active-map (kbd "M-S-z") #'my/aaronnote-redo)
    (define-key appine-active-map (kbd "H-s") #'my/aaronnote-save)
    (define-key appine-active-map (kbd "H-r") #'my/aaronnote-refresh)
    (define-key appine-active-map (kbd "H-B") #'my/aaronnote-build-and-reopen)
    (define-key appine-active-map (kbd "H-q") #'my/aaronnote-close)
    (define-key appine-active-map (kbd "H-y") #'my/aaronnote-roam-sync)
    (define-key appine-active-map (kbd "H-g") #'my/aaronnote-roam-graph)))

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
