;;; noema/bridge/config.el --- Noema Web page bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Noema web host and opens it in Portal or xwidget.
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
(require 'transient)
(require 'url)
(require 'url-util)
(require 'noema/jupyter/cell)
(require 'noema/jupyter/runtime)
(require 'noema/jupyter/server)
(require 'noema/jupyter/lsp)
(require 'noema/jupyter/debug)
(require 'noema/jupyter/files)
(require 'aaron-open)

(declare-function +background-make-frame "config/background/config" (&optional parameters))
(declare-function +xwidget/open-url "app/browser/config" (url &rest args))
(declare-function +noema--apple-gateway "noema/bridge/agenda-apple" (body client))
(declare-function +noema--apple-start "noema/bridge/agenda-apple" ())
(declare-function +noema--apple-stop "noema/bridge/agenda-apple" ())
(declare-function +noema--apple-request "noema/bridge/agenda-apple" (body callback &optional timeout))
(declare-function +project-activate "tools/project/config" (root &optional remember))
(declare-function noema-agenda-activate-project "noema-agenda" (root &optional callback))
(declare-function noema-agenda-attention "noema-agenda-attention" ())
(declare-function noema-agenda-attention-handle-change "noema-agenda-attention" (payload))
(declare-function +xwidget-current-url "app/browser/config" (&optional buffer))
(declare-function +xwidget-session-buffer "app/browser/config" (id))
(declare-function +xwidget/focus "app/browser/config" (&optional buffer))
(declare-function noema--focus-xwidget-buffer "noema-xwidget-keys" (buffer))
(declare-function noema--harden-xwidget-placeholder "noema-xwidget-keys" (&optional buffer))
(declare-function noema--sync-xwidget-recovery-mode "noema-xwidget-keys" (&rest args))
(declare-function +xwidget/undo "app/browser/config" ())
(declare-function +xwidget/redo "app/browser/config" ())
(declare-function +xwidget-setup-control-line "app/browser/config" ())
(declare-function aaron/open-system-target "aaron-open" (target))
(declare-function +noema-roam-note-changed "noema/roam/config" (file))
(declare-function +noema-roam--clear-runtime-cache "noema/roam/config" ())
(declare-function +noema-roam--handle-moved-note "noema/roam/config" (event))
(declare-function +noema-roam--handle-trashed-note "noema/roam/config" (event))
(declare-function +noema-roam--cancel-sync-timer "noema/roam/config" ())
(declare-function +noema--markdown-placeholder-set-state "lang/markdown/config" (state))
(declare-function +noema--kill-redirected-markdown-buffer "lang/markdown/config" (buffer file))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-edit-mode "xwidget" (&optional arg))
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script &optional callback))
(declare-function xwidget-webkit-pass-command-event "xwidget" (event))
(declare-function +zotero-open-reference "lang/latex/config" (payload))
(declare-function +zotero-import-bibtex "lang/latex/config" (payload))
(declare-function remote-expand-file-name
                  "remote-fs" (file-name &optional directory target))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-canonicalize-file-name
                  "remote-fs" (file-name &optional directory))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-file-local-name "remote-fs" (file-name))
(defvar remote-mode nil)
(defvar +xwidget--session-id)
(defvar +xwidget-suppress-auto-focus)

;; Publish module — lazy, loaded only when a publish command is first invoked.
(autoload '+noema/publish              "noema/bridge/publish" nil t)
(autoload '+noema/publish-build        "noema/bridge/publish" nil t)
(autoload '+noema/publish-deploy       "noema/bridge/publish" nil t)
(autoload '+noema/publish-clean        "noema/bridge/publish" nil t)
(autoload 'noema-project-overview "noema-project-overview" nil t)
(autoload 'noema-research-workflow-preview "noema-research-workflow" nil t)
(autoload 'noema-history-search "noema-history-search" nil t)
(autoload 'noema-findings "noema-findings" nil t)

(defgroup +noema nil
  "Noema Markdown web editor integration."
  :group 'applications)

(config-defvar +noema-agenda-exclude-patterns nil
  "Additional Agenda exclusions, matched relative to each active scope root.
Examples: archive/**, **/*.generated.md, or scratch.md. Hidden paths and
standard dependency/build directories are always excluded. Restart the Noema
host after changing this list. It applies to discovery and file events."
  :type '(repeat string) :group '+noema)

(config-defvar +noema-agenda-capture-templates nil
  "Declarative capture profiles shared by native and hosted Web Agenda.
Each profile has id, key, name, relative Markdown file, scope (selected or
knowledge), fields and optional required/defaults.  Nil uses the built-in
Task, Deadline and Appointment profiles.  File names allow %Y/%m/%d tokens.
Restart the Noema host after changing this setting."
  :type '(repeat alist) :group '+noema)

(defvar +noema--web-host-script
  (expand-file-name "site-lisp/noema/web-host.mjs" user-emacs-directory)
  "Path to the Noema web host script.")

(defvar +noema--web-dir
  (expand-file-name "site-lisp/noema/dist/pages" user-emacs-directory)
  "Path to the built Noema web app.")

(defvar +noema--runtime-root
  (expand-file-name "site-lisp/noema" user-emacs-directory)
  "Path to the vendored Noema runtime.")

(defvar +noema--state-root
  (expand-file-name "var/noema" user-emacs-directory)
  "Path to Noema state files under the Emacs config.")

(defvar +noema--tmp-root
  (expand-file-name "tmp" +noema--state-root)
  "Path to Noema runtime temporary files under the Emacs config.")

(defconst +noema-jupyter-output-buffer-name "*Noema Jupyter*")
(defconst +noema-jupyter-output-client-id "noema-jupyter")

(defvar +noema--snippets-root
  (expand-file-name "snippets" user-emacs-directory)
  "Path to Noema snippets shared with Emacs.")

(defvar +noema--templates-root
  (expand-file-name "templates/noema" user-emacs-directory)
  "Path to Markdown templates shared by Emacs and Noema.")

(defvar +noema--latex-templates-root
  (expand-file-name "templates" user-emacs-directory)
  "Path to LaTeX templates shared by Emacs and Noema.")

(defun +noema--app-config-file ()
  "Return the canonical Noema application configuration file."
  (expand-file-name
   "config.json"
   (or (getenv "NOEMA_CONFIG_DIR")
       (expand-file-name "~/.config/noema"))))

(defun +noema--workspace-config ()
  "Return Noema's configured workspace object, or nil."
  (let ((file (+noema--app-config-file)))
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

(defun +noema-workspace-root ()
  "Return the canonical Noema workspace root shared by both hosts."
  (let* ((workspace (+noema--workspace-config))
         (configured (and workspace (gethash "root" workspace)))
         (root (or (getenv "NOEMA_ROOT")
                   (and (stringp configured) (not (string-empty-p configured)) configured)
                   "~/Documents/Noema")))
    (file-name-as-directory (expand-file-name root))))

(defun +noema-workspace-layout ()
  "Return the canonical Noema workspace layout name."
  (let* ((workspace (+noema--workspace-config))
         (configured (and workspace (gethash "layout" workspace)))
         (layout (or (getenv "NOEMA_WORKSPACE_LAYOUT") configured "legacy")))
    (if (equal (downcase (format "%s" layout)) "wiki") "wiki" "legacy")))

(defvar +noema--notes-root (+noema-workspace-root)
  "Path to the canonical Noema workspace.")

(defun +noema-skills-directory ()
  "Return the vault directory the growable Skill library lives in.
Skills are knowledge, not product code: keeping them beside the notes means
they are versioned by the same git, reached by the same search, and can be
added to without a Noema release.  They sit inside the README namespace
because that is the repository covering them; a sibling of it would be
tracked by nothing.  The library is laid out as a Portable Agent Plugin, so
the Skills themselves sit under its `skills/\=' directory beside the plugin
manifests.  The Skills that describe Noema\='s own mechanisms stay in the
product tree instead, where they track the code."
  (expand-file-name "public/README/Skills/skills" +noema--notes-root))

(defun +noema--project-settings ()
  "Read Noema's project settings from the note root without evaluation."
  (let ((file (expand-file-name ".dir-locals.el" +noema--notes-root)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (forward-comment (point-max))
            (let* ((dir-locals (read (current-buffer)))
                   (global-variables (cdr (assq nil dir-locals))))
              (cdr (assq '+project-local-settings global-variables))))
        (error nil)))))

(defun +noema--jupyter-defaults ()
  "Return project-configured Jupyter defaults for new Noema cells."
  (let* ((settings (+noema--project-settings))
         (configured (plist-get settings :noema-jupyter)))
    (list :language (format "%s" (or (plist-get configured :language) "python"))
          :kernel (format "%s" (or (plist-get configured :kernel) "python3"))
          :session (format "%s" (or (plist-get configured :session) "default")))))

(defun +noema--jupyter-default-environment ()
  "Return web-host environment entries for project Jupyter defaults."
  (let ((defaults (+noema--jupyter-defaults)))
    (list
     (format "NOEMA_JUPYTER_DEFAULT_LANGUAGE=%s" (plist-get defaults :language))
     (format "NOEMA_JUPYTER_DEFAULT_KERNEL=%s" (plist-get defaults :kernel))
     (format "NOEMA_JUPYTER_DEFAULT_SESSION=%s" (plist-get defaults :session)))))

(defvar +noema--katex-macros-dir
  (expand-file-name "etc/katex-macros" user-emacs-directory)
  "Folder of .tex files defining the global KaTeX macro environment.")

(config-defvar +noema-backend nil
  "Backend used to display Noema."
  :type '(choice (const :tag "Portal Web" portal)
                 (const :tag "xwidget-webkit" xwidget))
  :group '+noema)

(config-defvar +noema-web-port nil
  "Fixed port for the Noema web host.
Set to 0 to let the OS pick a random port."
  :type 'integer
  :group '+noema)

(config-defvar +noema-web-host-max-heap-mb nil
  "V8 heap cap (MB) for the Noema web-host node process, or nil for no cap.
Passed as a `--max-old-space-size' command-line flag rather than
`NODE_OPTIONS' in the environment, because web-host's `process.env' is also
handed to the codex/claude/opencode CLIs it shells out to for LaTeX export —
an env-based cap would leak onto those unrelated node processes too."
  :type '(choice (integer :tag "Heap cap in MB") (const :tag "No cap" nil))
  :group '+noema)

(config-defvar +noema-echo-severity 'error
  "Warning/error policy for Noema messages copied to the Emacs echo area.
Important command responses are always echoed after browser-side deduplication
and rate limiting.  `error' additionally echoes errors, `warning' echoes
warnings and errors, and nil suppresses both severity classes."
  :type '(choice (const :tag "Errors only" error)
                 (const :tag "Warnings and errors" warning)
                 (const :tag "Never" nil))
  :group '+noema)

(config-defvar +noema-latex-export-engine "codex"
  "Engine for the Noema CMD+P LaTeX export.
\"codex\" compile-verifies a deterministic mechanical draft first, then allows
one fidelity-gated polish attempt.  A verified draft is never retried after an
agent timeout or gate rejection; multiple repairs require a concrete mechanical
compile failure.  \"mechanical\" never invokes an agent.  See
`docs/latex-export-style.md' in the Noema app."
  :type '(choice (const "codex") (const "mechanical"))
  :group '+noema)

(config-defvar +noema-latex-export-max-attempts 3
  "Maximum feedback-driven agent repairs after mechanical verification fails.
A fidelity/review rejection falls back immediately instead of retrying."
  :type 'integer
  :group '+noema)

(config-defvar +noema-latex-export-agent-idle-timeout 180
  "Seconds without agent output before Noema performs a liveness check.
A live process is kept running; this is not a kill timeout."
  :type 'integer
  :group '+noema)

(config-defvar +noema-latex-export-agent-hard-timeout 900
  "Absolute seconds allowed for one LaTeX export agent attempt.
At this limit Noema requests graceful termination before using a hard kill."
  :type 'integer
  :group '+noema)

(config-defvar +noema-latex-export-agent "codex"
  "AI backend for the Noema LaTeX export repair step.
One of \"codex\", \"claude\", or \"opencode\".  All run non-interactively in the
single-export staging directory with external-directory writes blocked and
network access available.  Clean mechanically verified exports do not launch
the backend.  The backend is chosen here, not per export."
  :type '(choice (const "codex") (const "claude") (const "opencode"))
  :group '+noema)

(config-defvar +noema-codex-model ""
  "Optional Codex model id for LaTeX export polish (empty = gpt-5.6-terra)."
  :type 'string
  :group '+noema)

(config-defvar +noema-latex-export-model ""
  "Optional model id for LaTeX export polish.
Empty selects gpt-5.6-terra for Codex, Sonnet for Claude, or the OpenCode
default.  This setting does not change interactive agent sessions."
  :type 'string
  :group '+noema)

(config-defvar +noema-opencode-executable "opencode"
  "Executable used when the LaTeX export backend is opencode."
  :type 'string
  :group '+noema)

(defvar +noema--last-sync-stats nil
  "String summary from the last successful Wiki index refresh, or nil.")

(defvar +noema--process nil
  "Running Noema web-host child process, or nil.")
(defvar +noema--last-interrupt-snapshot nil
  "Emacs/xwidget state captured when C-g interrupts a Noema adapter action.")
(defvar +noema--process-log-queue nil
  "Newest-first diagnostic output waiting to be written outside its filter.")
(defvar +noema--process-log-bytes 0
  "Approximate byte size of `+noema--process-log-queue'.")
(defvar +noema--process-log-timer nil
  "One-shot timer flushing deferred web-host diagnostics.")
(defconst +noema--process-log-queue-limit (* 256 1024)
  "Maximum diagnostic output retained between deferred log flushes.")
(defvar +noema--gateway-binding nil
  "Registration data for the current Noema web-host process.")
(defvar +noema--external-file-watches (make-hash-table :test #'equal)
  "Remote-backed file watches owned by the Noema runtime session.")
(defvar +noema--external-file-watch-timers
  (make-hash-table :test #'equal)
  "Debounce timers for remote Noema file changes.")
(defvar +noema--external-file-watch-suppressed
  (make-hash-table :test #'equal)
  "Pending writes or recent self-write signatures for file watch filtering.")
(defvar +noema--port nil
  "HTTP port of the running Noema web-host.")
(defvar +noema--last-port nil
  "Last ready web-host port, retained so a crashed core can reclaim its URL.")
(defvar +noema--ready nil
  "Non-nil once the web-host has announced its port.")
(defvar +noema--ready-callbacks nil
  "Callbacks waiting for the web-host to become ready.")
(defvar +noema--app-buffer nil
  "Buffer hosting the xwidget Noema page.")
(defvar +noema--ready-watchdog nil
  "Watchdog timer cancelled when the web-host becomes ready.")
(defvar +noema--goto-timer nil
  "Debounce timer for coalescing goto events from the web-host.")
(defvar +noema--goto-last nil
  "Last applied goto key (truename-file line col), for dedup.")
(defvar +noema--file-buffers (make-hash-table :test #'equal)
  "Canonical Noema file path to browser buffer map.")
(defvar +noema--client-buffers (make-hash-table :test #'equal)
  "Noema browser client id to browser buffer map.")

(defun +noema--record-interrupted-operation (operation)
  "Record adapter OPERATION and xwidget state after it receives `quit'."
  (let* ((buffer (current-buffer))
         (window-buffer (and (window-live-p (selected-window))
                             (window-buffer (selected-window))))
         (xwidget-buffer
          (and (buffer-live-p buffer)
               (fboundp 'noema--xwidget-buffer-p)
               (noema--xwidget-buffer-p buffer))))
    (setq +noema--last-interrupt-snapshot
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
            (recoveryMode . ,(and (boundp 'noema-xwidget-recovery-mode)
                                  noema-xwidget-recovery-mode t))
            (client . ,(and (boundp '+noema--client-id)
                            +noema--client-id))
            (hostReady . ,(and +noema--ready t))
            (hostProcessLive . ,(and (processp +noema--process)
                                     (process-live-p +noema--process) t))
            (outboundQueue . ,(length +noema--post-queue))
            (inboundQueue . ,(length +noema--host-event-queue))
            (thisCommand . ,this-command)
            (lastCommand . ,last-command)))
    (message "Noema: C-g interrupted %s; state saved in M-x +noema/interrupt-status"
             operation)))

(defun +noema/interrupt-status ()
  "Display the most recent C-g snapshot from the Noema Emacs adapter."
  (interactive)
  (if (null +noema--last-interrupt-snapshot)
      (message "Noema: no adapter operation has been interrupted by C-g")
    (with-current-buffer (get-buffer-create "*Noema interrupted operation*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry +noema--last-interrupt-snapshot)
          (insert (format "%-22s %S\n" (car entry) (cdr entry))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defvar +noema--build-process nil
  "Current Noema web build process, or nil.")

(defvar +noema--split-counter 0
  "Counter for fresh Noema xwidget split sessions.")

(defvar +noema--split-direction nil
  "Preferred direction for the next explicit Noema split.")

(defvar-local +noema-buffer-file-name nil
  "Current note file represented by a Noema xwidget buffer.")

(put '+noema-buffer-file-name 'permanent-local t)

(defvar-local +noema--client-id nil
  "Client id for this Noema browser buffer.")

(put '+noema--client-id 'permanent-local t)

(defvar-local +noema--registered-file nil
  "File path currently registered for this Noema browser buffer.")

(put '+noema--registered-file 'permanent-local t)

(defvar-local +noema--xwidget-forced-name nil
  "Non-nil display name marker for Noema xwidget buffers.")

(put '+noema--xwidget-forced-name 'permanent-local t)

(defvar-local +noema--xwidget-pending-file nil
  "File to POST to Noema once the page has finished loading, or nil.")

(put '+noema--xwidget-pending-file 'permanent-local t)

;; Keep the Markdown/xwidget input bridge in a dedicated module.  Its command
;; names and wire protocol remain unchanged for browser and Emacs callers.
(add-to-list 'load-path
             (expand-file-name "site-lisp/noema/lisp" user-emacs-directory))
(require 'noema-xwidget-keys)
(require 'noema/bridge/portal)

(defvar-keymap +noema-keys-mode-map
  "b" #'+noema/back
  "f" #'+noema/forward
  "M-z" #'+noema/undo
  "M-Z" #'+noema/redo
  "M-S-z" #'+noema/redo
  "M-C" #'+noema/prose-check)

;; The pane buffer is an inert placeholder: the page owns the text.  Global
;; Emacs text commands reached through forwarded keys (H-s, H-c, C-x C-s,
;; C-x h, ...) are remapped onto the page's own equivalents so they act on the
;; note instead of silently doing nothing.
(pcase-dolist (`(,command . ,pane-command)
               '((imenu . noema-goto-heading)
                 (consult-imenu . noema-goto-heading)
                 (consult-outline . noema-goto-heading)
                 (+search/line-forward . noema-find)
                 (consult-line . noema-find)
                 (isearch-forward . noema-find)
                 (isearch-backward . noema-find)
                 (clipboard-kill-ring-save . noema-pane-copy)
                 (kill-ring-save . noema-pane-copy)
                 (clipboard-kill-region . noema-pane-cut)
                 (kill-region . noema-pane-cut)
                 (clipboard-yank . noema-pane-paste)
                 (yank . noema-pane-paste)
                 (mark-whole-buffer . noema-select-all)
                 (save-buffer . +noema/save)
                 (+navigation/back . +noema/back)
                 (+navigation/forward . +noema/forward)
                 (undo . +noema/undo)
                 (undo-redo . +noema/redo)
                 (revert-buffer . noema-refresh-file)
                 (revert-buffer-quick . noema-refresh-file)))
  (define-key +noema-keys-mode-map (vector 'remap command) pane-command))

(defvar +treemacs-tag-visit-functions)

(defun +noema-treemacs-tag-visit-h (buffer)
  "Move the Noema page, not a raw buffer, to a Treemacs heading in BUFFER.
`show-imenu' (H-i) in a Noema pane opens the same Treemacs outline as any
file.  Its heading jump visits the Markdown source; when a Noema pane shows
that note, the pane goes to the heading instead and the throwaway raw buffer
is dropped.  A raw buffer you are editing elsewhere keeps the jump."
  (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
              ((+noema--markdown-file-p file))
              ((not (with-current-buffer buffer
                      (bound-and-true-p noema-md-bridge-source-mode))))
              ((<= (length (get-buffer-window-list buffer nil t)) 1))
              (pane (+noema--buffer-for-file file)))
    (let ((line (with-current-buffer buffer (line-number-at-pos nil t)))
          (window (selected-window))
          (pane-window (get-buffer-window pane 'visible)))
      (if (and (window-live-p pane-window) (not (eq pane-window window)))
          (progn (switch-to-prev-buffer window t)
                 (select-window pane-window))
        (set-window-buffer window pane)
        (setq pane-window window))
      (with-current-buffer pane
        (+noema/command "goto-line" `((value . ,line))))
      (noema--focus-xwidget-window pane-window)
      ;; A hidden bridge copy may carry gptel context; keep it.
      (unless (or (buffer-modified-p buffer) (get-buffer-window buffer t)
                  (with-current-buffer buffer
                    (bound-and-true-p noema-md-bridge--hidden)))
        (kill-buffer buffer)))))

(add-hook '+treemacs-tag-visit-functions #'+noema-treemacs-tag-visit-h)

(define-minor-mode +noema-keys-mode
  "Buffer-local keys for an Noema browser surface."
  :init-value nil
  :lighter nil
  :keymap +noema-keys-mode-map)

(defconst +noema--xwidget-focus-script
  "(() => {
  const focusEditor = () => {
    try {
      window.dispatchEvent(new CustomEvent('noema:command', {
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

(defun +noema--server-url (&optional path)
  "Return the local Noema URL for PATH."
  (format "http://127.0.0.1:%d%s" +noema--port (or path "/")))

(defun +noema--canonical-file (file)
  "Return canonical absolute FILE for Noema bookkeeping, or nil."
  (and (stringp file)
       (not (string-empty-p file))
       (cond
        ((and (bound-and-true-p remote-mode)
              (fboundp 'remote-expand-file-name))
         ;; Noema's web host runs locally.  Raw host paths (including
         ;; `~/...') therefore belong to the local target; an already logical
         ;; or TRAMP path retains its encoded target and is rejected later by
         ;; `+noema--host-file' when the local host cannot serve it.
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
               ;; `file-truename' and the final canonicalization inherit the
               ;; current buffer's directory.  A remote notebook must not
               ;; turn a client-local host script into its SSH target path.
               (let ((default-directory user-emacs-directory))
                 (remote-canonicalize-file-name
                  (file-truename (remote-file-local-name expanded))))
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

(defun +noema--host-file (file)
  "Return the path Noema should use for logical FILE.
Local files are projected to native host paths.  Remote files retain their
`/fs:' identity and are served through the Remote-backed gateway provider."
  (when-let* ((file (+noema--canonical-file file)))
    (if (and (bound-and-true-p remote-mode)
             (fboundp 'remote-file-name-target)
             (fboundp 'remote-file-local-name))
        ;; The web host runs on this machine: it gets a native path for a
        ;; file it can open itself, whatever the target is called.
        (or (ignore-errors (remote-client-file-name file))
            file)
      (expand-file-name file))))

(defun +noema--xwidget-session-id (&optional file)
  "Return the stable xwidget session/client id for FILE."
  (if-let* ((file (+noema--canonical-file file)))
      (format "noema:%s" file)
    "noema"))

(defun +noema--split-client-p (client)
  "Return non-nil when CLIENT identifies a split pane."
  (and (stringp client)
       (string-prefix-p "noema-split:" client)))

(defun +noema--app-url (&optional file client extra-params)
  "Return the Noema app URL, optionally opening FILE for CLIENT."
  (let ((base (+noema--server-url "/"))
        params)
    (when-let* ((file (+noema--canonical-file file)))
      (push (cons "file" (+noema--host-file file)) params))
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

(defun +noema-jupyter--output-url (payload)
  "Return the singleton Jupyter output page URL for PAYLOAD."
  (let ((base (+noema--server-url "/jupyter.html")) params)
    (dolist (entry payload)
      (when (and (cdr entry) (not (string-empty-p (format "%s" (cdr entry)))))
        (push (cons (format "%s" (car entry)) (format "%s" (cdr entry))) params)))
    (push (cons "client" +noema-jupyter-output-client-id) params)
    (concat base "?"
            (mapconcat
             (lambda (entry)
               (format "%s=%s"
                       (url-hexify-string (car entry))
                       (url-hexify-string (cdr entry))))
             (nreverse params) "&"))))

(defun +noema-jupyter--output-dispatch (buffer payload)
  "Ask an existing Jupyter output BUFFER to open PAYLOAD as a tab."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (eq major-mode 'portal-web-mode)
          (+noema-portal-send buffer
                                (list :type "jupyter-open" :payload payload))
        (when (and (fboundp 'xwidget-webkit-current-session)
                   (fboundp 'xwidget-webkit-execute-script))
          (when-let* ((session (xwidget-webkit-current-session)))
            (xwidget-webkit-execute-script
             session
             (format
              "(() => { const p = %s; if (typeof window.noemaJupyterOpenDocument === 'function') { window.noemaJupyterOpenDocument(p); if (p.view && typeof window.noemaJupyterOpenView === 'function') window.noemaJupyterOpenView(p.view); return true; } return false; })()"
              (json-serialize payload :null-object nil :false-object :json-false)))))))))

(defun +noema-jupyter--output-split (source-window)
  "Create the right-side output window for SOURCE-WINDOW.
When SOURCE-WINDOW is stacked with the docked Noema DAG (JuText above, DAG
below), split their whole column instead, so the output spans the column's
height and the DAG stays in the bottom-left."
  (let* ((parent (window-parent source-window))
         (graph (get-buffer "*Noema DAG*"))
         (graph-window (and graph (get-buffer-window graph))))
    (if (and parent graph-window
             (window-combined-p source-window)
             (eq (window-parent graph-window) parent))
        (split-window parent nil 'right)
      (split-window source-window nil 'right))))

(defun +noema-jupyter--focus-if-selected (buffer window)
  "Focus output BUFFER only if WINDOW still owns the keyboard."
  (when (and (window-live-p window)
             (eq window (selected-window))
             (eq buffer (window-buffer window)))
    (if (+noema-portal-buffer-p buffer)
        (+noema-portal-focus buffer)
      (+xwidget/focus buffer))))

(defun +noema-jupyter-output-open-document (payload &optional focus)
  "Open Noema's singleton rich-output renderer for document PAYLOAD.
PAYLOAD names the Emacs-owned work document and optional cell.  The renderer
uses the existing JupyterLab rendermime stack, but it is only a right-side
view: Emacs remains the document editor, workspace compositor, and control
surface.  With FOCUS non-nil, move keyboard focus to the renderer."
  (unless (and (alist-get 'scriptFile payload)
               (not (string-empty-p (format "%s" (alist-get 'scriptFile payload)))))
    (user-error "Missing Noema/Jupyter work document"))
  (let ((source-buffer (current-buffer))
        (source-window (selected-window)))
    (+noema--ensure-server
     (lambda ()
       ;; The port is assigned by `+noema--ensure-server'.  Build the URL
       ;; only after its ready callback runs so a cold Emacs session can open
       ;; already-persisted `.noema' output before doing any other Noema work.
       (let ((url (+noema-jupyter--output-url payload)))
         (when (and (buffer-live-p source-buffer)
                    (window-live-p source-window)
                    (eq (window-buffer source-window) source-buffer))
           (let ((activate (and focus (eq (selected-window) source-window)))
                 target-window buffer)
             (with-selected-window source-window
               (let* ((existing
                       (pcase +noema-backend
                         ('portal
                          (+noema-portal--session-buffer
                           +noema-jupyter-output-client-id))
                         ('xwidget
                          (and (fboundp '+xwidget-session-buffer)
                               (+xwidget-session-buffer
                                +noema-jupyter-output-client-id)))))
                      (visible (and existing
                                    (get-buffer-window existing 'visible)))
                      (created (not visible))
                      (success nil))
                 (setq target-window
                       (or visible (+noema-jupyter--output-split source-window)))
                 (unwind-protect
                     (progn
                       (if (buffer-live-p existing)
                           (progn
                             (setq buffer existing)
                             (set-window-buffer target-window buffer)
                             (+noema-jupyter--output-dispatch buffer payload))
                         (with-selected-window target-window
                           (setq buffer
                                 (if (eq +noema-backend 'portal)
                                     (+noema-portal-open-client
                                      url +noema-jupyter-output-client-id)
                                   (unless (fboundp '+xwidget/open-url)
                                     (require 'app/browser/config))
                                   (+xwidget/open-url
                                    url :id +noema-jupyter-output-client-id
                                    :display 'current :reuse-selected t)))))
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when (eq major-mode 'xwidget-webkit-mode)
                             (setq-local +xwidget-suppress-auto-focus (not focus)))
                           (setq-local +noema--client-id
                                       +noema-jupyter-output-client-id)
                           (setq-local +noema--xwidget-forced-name
                                       +noema-jupyter-output-buffer-name)
                           (unless (equal (buffer-name)
                                          +noema-jupyter-output-buffer-name)
                             (rename-buffer +noema-jupyter-output-buffer-name t)))
                         (setq success t)))
                   (when (and created (not success)
                              (window-live-p target-window))
                     (delete-window target-window)))))
             (when (and activate (window-live-p target-window)
                        (buffer-live-p buffer))
               (select-window target-window)
               (run-at-time 0.2 nil #'+noema-jupyter--focus-if-selected
                            buffer target-window)))))))
    (get-buffer +noema-jupyter-output-buffer-name)))

;;;###autoload
(defun +noema-jupyter/output-open (&optional cell-id focus view)
  "Open the right-side Jupyter rich-output view for CELL-ID.
This compatibility entry point serves native ipynb source projections.  A
`.noema' work document calls `+noema-jupyter-output-open-document' directly
with its own canonical file and cell identity."
  (interactive (list nil t))
  (unless (and buffer-file-name
               (bound-and-true-p +noema-jupyter-cell-mode))
    (user-error "Current buffer is not a Noema Jupyter Cell script"))
  (+noema-jupyter-cell--update-highlight)
  (+noema-jupyter-output-open-document
   `((scriptFile . ,(+noema--host-file buffer-file-name))
     (sourceFile . ,(+noema--host-file +noema-jupyter-cell-source-file))
     (cellId . ,(or cell-id +noema-jupyter-cell-current-id ""))
     (language . ,(or +noema-jupyter-cell-language "python"))
     (kernel . ,(or +noema-jupyter-cell-kernel "python3"))
     (session . ,(or +noema-jupyter-cell-session "default"))
     ,@(when view `((view . ,view))))
   focus))

(defun +noema--markdown-file-p (file)
  "Return non-nil when FILE is a Markdown file."
  (and file
       (or (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
           (string-equal (file-name-nondirectory file) "README"))))

(defun +noema--web-host-log-tail (&optional lines)
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

(defun +noema--watchdog-fire ()
  "Called when the web-host fails to become ready within the timeout.
Previously this only dropped the queued ready-callbacks silently with a
one-line `message'; the caller who pressed e.g. `A' for the agenda had no
visible sign that nothing was going to happen. Now it also surfaces the
process state and a tail of the log buffer, and pops that buffer so the
failure is diagnosable without hunting for it."
  (setq +noema--ready-watchdog nil)
  (unless +noema--ready
    (let* ((dropped (length +noema--ready-callbacks))
           (alive (and +noema--process (process-live-p +noema--process)))
           (tail (+noema--web-host-log-tail))
           (log-buf (get-buffer " *Noema web host*")))
      (setq +noema--ready-callbacks nil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p +noema--markdown-redirected)
                     (eq (bound-and-true-p
                          +noema--markdown-placeholder-state)
                         'starting)
                     (fboundp '+noema--markdown-placeholder-set-state))
            (+noema--markdown-placeholder-set-state 'failed))))
      (when log-buf (display-buffer log-buf))
      (message "%s"
               (concat
                (format "Noema: web-host not ready after 10s (%d pending action%s dropped)."
                        dropped (if (= dropped 1) "" "s"))
                (if alive "" " Process exited — check node/port.")
                (if tail (format " Last log: %s" tail) " No log output yet — see *Noema web host*."))))))

(defun +noema--ensure-server (&optional callback)
  "Start the web-host if needed, then call CALLBACK."
  (if (and +noema--process
           (process-live-p +noema--process)
           +noema--ready)
      (when callback (funcall callback))
    (when callback
      (push callback +noema--ready-callbacks))
    (unless (and +noema--process
                 (process-live-p +noema--process))
      (condition-case err
          (+noema--start-server)
        ((error quit)
         (setq +noema--ready-callbacks
               (delq callback +noema--ready-callbacks))
         (signal (car err) (cdr err)))))
    ;; A retry after the watchdog fired may still find a live but unready
    ;; process.  Bound that wait too; otherwise the new callback never ends.
    (unless (or +noema--ready +noema--ready-watchdog)
      (setq +noema--ready-watchdog
            (run-at-time 10 nil #'+noema--watchdog-fire)))))

(defun +noema--start-server (&optional reconnect-port)
  "Spawn the vendored Noema web-host.
When RECONNECT-PORT is non-nil, reclaim that port so live browser pages can
reconnect without a reload and without losing their in-memory editor state."
  (+noema--clear-process-log-queue)
  (setq +noema--notes-root (+noema-workspace-root))
  (make-directory +noema--notes-root t)
  ;; The host resolves Skills from here on every Run; a missing directory
  ;; would silently resolve to an empty library.
  (make-directory (+noema-skills-directory) t)
  (unless (executable-find "node")
    (user-error "Noema: `node' not found in exec-path; install Node.js"))
  (unless (file-directory-p +noema--web-dir)
    (user-error "Noema: built web app not found at %s; run `npm run build' in %s"
                +noema--web-dir +noema--runtime-root))
  (let ((old-proc +noema--process))
    (when (and old-proc (process-live-p old-proc))
      (ignore-errors (signal-process old-proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p old-proc)
            (delete-process old-proc))))))
  (setq +noema--process nil
        +noema--port nil
        +noema--ready nil)
  (let* ((log-buf (get-buffer-create " *Noema web host*"))
         (_copilot-gateway-method
          (require 'tools/copilot/config nil t))
         (gateway
          (remote-gateway-prepare-client
           "noema" (remote-context +noema--notes-root)
           :placement 'client
           :provides '("noema.command" "noema.api")))
         (copilot-server
          (when (and (not _copilot-gateway-method)
                     (require 'copilot nil t))
            (ignore-errors (copilot-server-executable))))
         (process-environment
          (append
           (delq nil
            (append
             (list
            "NOEMA_HOST_MODE=emacs"
            ;; The host watches this pid and exits with it, so a crashed or
            ;; force-quit Emacs does not leave the host and its kernels behind.
            (format "NOEMA_HOST_OWNER_PID=%d" (emacs-pid))
            (format "NOEMA_AGENDA_CAPTURE_TEMPLATES=%s"
                    (json-encode (vconcat +noema-agenda-capture-templates)))
            (format "NOEMA_AGENDA_EXCLUDE=%s"
                    (json-encode (vconcat +noema-agenda-exclude-patterns)))
            (format "NOEMA_ROOT=%s" (expand-file-name +noema--notes-root))
            (format "NOEMA_GLOBAL_CAPABILITIES=%s"
                    (or (getenv "NOEMA_GLOBAL_CAPABILITIES")
                        (expand-file-name "etc/noema/capabilities.json" user-emacs-directory)))
            ;; Skills live in the vault, beside the notes, so the library grows
            ;; with the person's knowledge and is versioned by the same git as
            ;; the notes.  Only the skills that describe Noema's own mechanisms
            ;; stay in the product tree, where they track the code's version.
            (format "NOEMA_GLOBAL_SKILLS=%s"
                    (or (getenv "NOEMA_GLOBAL_SKILLS")
                        (+noema-skills-directory)))
            (format "NOEMA_ROOT=%s" (expand-file-name +noema--notes-root))
            (format "NOEMA_WORKSPACE_LAYOUT=%s" (+noema-workspace-layout))
            (format "NOEMA_WEB_DIR=%s" (expand-file-name +noema--web-dir))
            (format "NOEMA_RUNTIME_ROOT=%s" (expand-file-name +noema--runtime-root))
            (format "NOEMA_WORKSPACE_ROOT=%s" (expand-file-name +noema--notes-root))
            (format "NOEMA_LANGUAGETOOL_LANGUAGE=%s"
                    (or (bound-and-true-p +languagetool-language) "en-US"))
            (format "NOEMA_LANGUAGETOOL_URL=%s"
                    (or (bound-and-true-p +languagetool-server-url)
                        "http://10.243.90.222:8765"))
            (format "NOEMA_PROSE_WORDS=%s"
                    (expand-file-name "etc/prose-accepted-words.txt"
                                      user-emacs-directory))
            (format "NOEMA_PROSE_DISABLED_RULES=%s"
                    (expand-file-name "etc/prose-disabled-rules.txt"
                                      user-emacs-directory))
            (format "NOEMA_PUBLISH_JS_DIR=%s"
                    (expand-file-name "js" +noema--runtime-root))
            (format "NOEMA_STATE_DIR=%s" (expand-file-name +noema--state-root))
            (format "NOEMA_TMP_DIR=%s" (expand-file-name +noema--tmp-root))
            (format "NOEMA_SNIPPETS_ROOT=%s" (expand-file-name +noema--snippets-root))
            (format "NOEMA_TEMPLATES_ROOT=%s" (expand-file-name +noema--templates-root))
            (format "NOEMA_LATEX_TEMPLATES_ROOT=%s"
                    (expand-file-name +noema--latex-templates-root))
            (format "NOEMA_KATEX_MACROS_DIR=%s" (expand-file-name +noema--katex-macros-dir)))
            (+noema--jupyter-default-environment)
            (list
            (format "NOEMA_LATEX_EXPORT_ENGINE=%s"
                    (or +noema-latex-export-engine "codex"))
            (format "NOEMA_LATEX_EXPORT_AGENT=%s"
                    (or (bound-and-true-p +noema-latex-export-agent) "codex"))
            (format "NOEMA_LATEX_EXPORT_MAX_ATTEMPTS=%d"
                    (or +noema-latex-export-max-attempts 3))
            (format "NOEMA_LATEX_EXPORT_AGENT_IDLE_TIMEOUT_MS=%d"
                    (* 1000 (max 10 (or +noema-latex-export-agent-idle-timeout 180))))
            (format "NOEMA_LATEX_EXPORT_AGENT_HARD_TIMEOUT_MS=%d"
                    (* 1000 (max (or +noema-latex-export-agent-idle-timeout 180)
                                 (or +noema-latex-export-agent-hard-timeout 900))))
            (format "NOEMA_CODEX_BIN=%s"
                    (or (bound-and-true-p codex-cli-executable) "codex"))
            (format "NOEMA_CLAUDE_BIN=%s"
                    (or (bound-and-true-p claude-code-ide-cli-path) "claude"))
            (format "NOEMA_OPENCODE_BIN=%s"
                    (or (bound-and-true-p +noema-opencode-executable) "opencode"))
            (when (and (boundp '+noema-codex-model)
                       (stringp +noema-codex-model)
                       (not (string-empty-p +noema-codex-model)))
              (format "NOEMA_CODEX_MODEL=%s" +noema-codex-model))
            (when (and (boundp '+noema-latex-export-model)
                       (stringp +noema-latex-export-model)
                       (not (string-empty-p +noema-latex-export-model)))
              (format "NOEMA_LATEX_EXPORT_MODEL=%s" +noema-latex-export-model))
            (format "NOEMA_WEB_PORT=%d"
                    (or reconnect-port +noema-web-port 0))
            ;; Emacs-started Noema should share Emacs' existing Copilot LS
            ;; through the gateway, not spawn a second memory-heavy copy.
            "NOEMA_COPILOT_DISABLE_LOCAL=1"
            (format "NOEMA_EMACS_GATEWAY_URL=%s"
                    (plist-get gateway :websocket-url))
            (format "NOEMA_EMACS_GATEWAY_BINDING=%s"
                    (plist-get gateway :binding-id))
            (format "NOEMA_EMACS_GATEWAY_CLIENT_ID=%s"
                    (plist-get gateway :client-id))
            (when copilot-server
              (format "NOEMA_COPILOT_LANGUAGE_SERVER=%s"
                      (expand-file-name copilot-server)))
            (when (and (not _copilot-gateway-method)
                       (bound-and-true-p +copilot-server-max-heap-mb))
              (format "NOEMA_COPILOT_MAX_HEAP_MB=%d"
                      +copilot-server-max-heap-mb)))))
           process-environment))
         (proc
          (remote-make-client-process
             :name "noema-web-host"
             :buffer log-buf
             :command
             (append
              (list "node")
              (when +noema-web-host-max-heap-mb
                (list
                 (format "--max-old-space-size=%d"
                         +noema-web-host-max-heap-mb)))
              (list (+noema--host-file
                     +noema--web-host-script)))
             :noquery t
             :sentinel #'+noema--sentinel
             :filter #'+noema--process-filter
             :remote-client-directory user-emacs-directory
             :remote-client-environment process-environment)))
    (with-current-buffer log-buf (erase-buffer))
    (setq +noema--process proc
          +noema--gateway-binding gateway)
    proc))

(defvar +noema-host-ready-functions nil
  "Functions run with no arguments after the web-host reports ready.

Unlike `+noema--ready-callbacks', which is a one-shot queue the ready
watchdog may drop when the host is slow to boot, this hook is a durable
notification: consumers that want to recover from a dropped or timed-out
request re-issue it from here.")

(defun +noema--flush-ready-callbacks ()
  "Run callbacks waiting for the server to become ready."
  (when +noema--ready-watchdog
    (cancel-timer +noema--ready-watchdog)
    (setq +noema--ready-watchdog nil))
  (let ((callbacks (nreverse +noema--ready-callbacks)))
    (setq +noema--ready-callbacks nil)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback)))
  (run-hook-with-args '+noema-host-ready-functions)
  (+noema--install-activity-hooks)
  ;; Do an initial activity check after the page has had time to load.
  (run-at-time 0.2 nil #'+noema--update-activity))

(defun +noema--run-zotero-event (payload import-p)
  "Handle Zotero PAYLOAD in Emacs; IMPORT-P starts the BibTeX picker."
  (let* ((client (alist-get 'client payload))
         (source-buffer (noema--key-source-buffer client)))
    (noema--release-xwidget-input-buffer source-buffer)
    (noema--select-emacs-window)
    (condition-case err
        (progn
          (unless (if import-p
                      (fboundp '+zotero-import-bibtex)
                    (fboundp '+zotero-open-reference))
            (require 'lang/latex/config))
          (if import-p
              (+zotero-import-bibtex payload)
            (+zotero-open-reference payload)))
      (error
       (message "Noema Zotero %s failed: %s"
                (if import-p "import" "open")
                (error-message-string err))))))

(defun +noema--handle-input-focus (payload)
  "Record that the Noema renderer named by PAYLOAD owns keyboard input.

This adapter derives each pane\'s foreground state from Emacs window and frame
selection.  On the macOS xwidget port a click inside the WebKit view can make
that view first responder without Emacs selecting the surrounding window, so
the pane keeps being reported as background and re-paused under the user\'s
cursor.  A renderer sends this only after a trusted input event it received
while paused.  Record that pane as the input owner without selecting its Emacs
window, which would take first responder away from WebKit.

The renderer has already dropped its own host pause by the time this arrives,
but the Node host still retains the old pause for reconnect replay. Mark local
state unknown so the recomputed snapshot sends an explicit resume and updates
both sides of the protocol before later background transitions are deduped."
  (let* ((client (alist-get 'client payload))
         (buffer (+noema--buffer-for-client client)))
    (when (buffer-live-p buffer)
      (setq +noema--app-buffer buffer)
      (with-current-buffer buffer
        (setq-local +noema--activity-paused :unknown))
      (when-let* ((window (get-buffer-window buffer 'visible)))
        (if (eq window (selected-window))
            (+noema--forget-input-focus)
          (setq +noema--input-focused-window window
                +noema--input-focus-host-window (selected-window))
          (add-hook 'pre-command-hook #'+noema--forget-input-focus-on-command)))
      (setq +noema--last-activity-signature :unknown)
      (+noema--update-activity))))

(defun +noema--handle-ui-state-payload (payload)
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
           (pcase +noema-echo-severity
             ('warning (memq severity '(warning error)))
             ('error (eq severity 'error))
             (_ nil)))))
    (when (and echo-p
               (stringp status)
               (not (string-empty-p status)))
      (message "Noema %s: %s" severity status))))

(defun +noema--handle-process-line (line)
  "Handle one legacy Noema event encoded as LINE."
  ;; `json-serialize' emits unibyte UTF-8.  Splicing it after a multibyte
  ;; piece (an event type decoded from JSON-RPC, say) promotes those bytes to
  ;; raw-byte characters, which `json-parse-string' then rejects as invalid
  ;; UTF-8 on any non-ASCII note path.  Normalize back to bytes first.
  (when (multibyte-string-p line)
    (setq line (encode-coding-string line 'utf-8)))
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
          (setq +noema--port port
                +noema--last-port port
                +noema--ready t)
          (+noema--flush-ready-callbacks)
          (when (fboundp 'noema-agenda-host-ready)
            (noema-agenda-host-ready))
          ;; Gateway registration/re-registration is the reconnect event for
          ;; Emacs.  Reconcile open notebooks once here; notebook buffers do
          ;; not run polling timers.
          (when (fboundp '+noema-jupyter-cell-refresh-open-buffers)
            (+noema-jupyter-cell-refresh-open-buffers)))))
     ((string-prefix-p goto-prefix line)
      (let* ((payload (substring line (length goto-prefix)))
             (parts (split-string payload ":" nil))
             (line-number (string-to-number (or (car parts) "0")))
             (column (string-to-number (or (cadr parts) "0"))))
        (when (> line-number 0)
          ;; Coalesce burst goto events: cancel any pending jump and schedule
          ;; a fresh one.  Normal (not idle) timer so jumps are not deferred
          ;; indefinitely during continuous Emacs activity.
          (when +noema--goto-timer
            (cancel-timer +noema--goto-timer))
          (setq +noema--goto-timer
                (run-at-time
                 0.05 nil
                 (let ((ln line-number) (col column))
                   (lambda ()
                     (setq +noema--goto-timer nil)
                     (+noema--goto-location nil ln col))))))))
     ((string-prefix-p open-prefix line)
	      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length open-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload))
                 (line-number (or (alist-get 'line payload) 1))
                 (column (or (alist-get 'col payload) 0))
                 (tag (alist-get 'tag payload))
                 (new-window (eq (alist-get 'newWindow payload) t))
                 (client (alist-get 'client payload))
                 (source-window
                  (if (and (stringp client) (not (string-empty-p client)))
                      (when-let* ((buffer (+noema--buffer-for-client client)))
                        (get-buffer-window buffer 'visible))
                    (selected-window))))
            (when (window-live-p source-window)
              (if (and new-window (+noema--markdown-file-p file)
                       (or (null tag) (string-empty-p (or tag ""))))
                  (+noema--defer-host-event
                   #'+noema/open-current-note-split file source-window
                   `(("hash" . ,(alist-get 'hash payload))
                     ("dom" . ,(alist-get 'dom payload))))
                (if (and (+noema--markdown-file-p file)
                         (or (null tag) (string-empty-p (or tag ""))))
                    ;; Markdown note (e.g. graph double-click): open in Noema.
                    (+noema/open-file file source-window)
                  ;; Source region (lean, etc.) or explicit tag: open in Emacs.
                  (+noema--goto-location file line-number column)
                  (when (and (stringp file)
                             (string-match-p "\\.ipynb\\'" file)
                             (require 'noema/jupyter/cell nil t))
                    (ignore-errors
                      (+noema-jupyter/cell-activate-buffer payload)))
                  (when (and tag (not (string-empty-p (or tag ""))))
                    (when (require 'noema/bridge/note-code nil t)
                      (ignore-errors (+note-code--goto-tag tag))))))))
	        (error
	         (message "Noema event parse failed: %s" (error-message-string err)))))
     ((string-prefix-p zotero-import-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-import-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'+noema--run-zotero-event payload t))
        (error
         (message "Noema Zotero import event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p zotero-prefix line)
      (condition-case err
          (let ((payload (json-parse-string
                          (substring line (length zotero-prefix))
                          :object-type 'alist)))
            (run-at-time 0 nil #'+noema--run-zotero-event payload nil))
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
                (require 'aaron-open)
                (aaron/open-file target))
               ;; URL schemes (http, zotero, …): system open as before.
               (t
                (require 'aaron-open)
                (aaron/open-system-target target)))))
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
            (+noema--sync-app-buffer-file file client))
        (error
         (message "Noema current-file parse failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p ui-state-prefix line)
      (condition-case err
          (+noema--handle-ui-state-payload
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
              (when (fboundp '+noema-roam-note-changed)
                (+noema-roam-note-changed file))))
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
              (noema--run-emacs-key key client)))
        (error
         (message "Noema key-event parse failed: %s"
                  (error-message-string err))))))))

(defun +noema--external-file (file)
  "Return a canonical Markdown FILE accepted by the gateway provider."
  (let ((file (+noema--canonical-file file)))
    (unless (and file (+noema--markdown-file-p file))
      (error "Noema external provider requires a Markdown file: %s"
             file))
    file))

(defun +noema--external-file-metadata (file)
  "Return JSON-ready metadata for FILE, or signal when it is unavailable."
  (let* ((attributes (file-attributes file 'string))
         (type (and attributes (file-attribute-type attributes))))
    ;; An ordinary file needs only one metadata operation.  For a symlink,
    ;; inspect the target: the link's own mtime would miss agent edits.
    (when (stringp type)
      (setq attributes (file-attributes (file-truename file) 'string)
            type (and attributes (file-attribute-type attributes))))
    (unless (and attributes (null type))
      (error "Remote Markdown file is unavailable: %s" file))
    `((mtimeMs
       . ,(* 1000.0
             (float-time
              (file-attribute-modification-time attributes))))
      (size . ,(or (file-attribute-size attributes) 0)))))

(defun +noema--external-file-notify-change (file &optional client-id)
  "Notify Noema about logical FILE using optional CLIENT-ID as the source."
  (let ((client (remote-gateway-find-client "noema")))
    (when (and client client-id)
      (when-let* ((timer (gethash file +noema--external-file-watch-timers)))
        (cancel-timer timer)))
    (remhash file +noema--external-file-watch-timers)
    (when client
      (let* ((metadata
              (condition-case nil
                  (+noema--external-file-metadata file)
                (error '((mtimeMs . 0) (size . 0)))))
             (mtime (alist-get 'mtimeMs metadata)))
        (remote-gateway-notify
         client "noema.command"
         `((type . "command")
           (command . "note-saved")
           (detail . ((file . ,(+noema--host-file file))
                      (mtimeMs . ,mtime)
                      (clientId . ,(or client-id "remote-external"))))))))))

(defun +noema--check-open-notes-after-agent-turn ()
  "Send disk metadata for open Markdown pages after one ACP turn."
  (when +noema--ready
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (buffer (buffer-list))
        (when (and (local-variable-p '+noema-buffer-file-name buffer)
                   (not (with-current-buffer buffer
                          (bound-and-true-p remote-buffer-disconnected-p))))
          (when-let* ((file (buffer-local-value '+noema-buffer-file-name
                                                  buffer)))
            (when (and (+noema--markdown-file-p file)
                       (not (gethash file seen)))
              (puthash file t seen)
              (+noema--external-file-notify-change
               file "agent-turn"))))))))

(add-hook '+auto-revert-agent-refresh-functions
          #'+noema--check-open-notes-after-agent-turn)

(defun +noema--external-file-signature (file)
  "Return FILE's identity and high-resolution change metadata, or nil."
  (when-let* ((attributes (ignore-errors (file-attributes file))))
    (list (file-attribute-inode-number attributes)
          (file-attribute-size attributes)
          (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes))))

(defun +noema--external-file-self-write-p (file)
  "Whether FILE still matches Noema's own recent write."
  (when-let* ((record (gethash file +noema--external-file-watch-suppressed)))
    (let ((until (if (numberp record) record (car record))))
      (cond
       ((>= (float-time) until)
        (remhash file +noema--external-file-watch-suppressed)
        nil)
       ((numberp record) t) ; An atomic write is in progress.
       ((null (cdr record)) nil)
       (t (equal (cdr record) (+noema--external-file-signature file)))))))

(defun +noema--external-file-watch-event (file event)
  "Debounce Remote file watch EVENT for logical FILE.
A file is watched through its directory, so events for its siblings arrive
here as well; only FILE's own are a change of the note."
  (unless (or (eq (nth 1 event) 'stopped)
              (not (seq-some
                    (lambda (name)
                      (and (stringp name)
                           (equal (file-name-nondirectory name)
                                  (file-name-nondirectory file))
                           (equal (file-name-directory
                                   (remote-canonicalize-file-name name))
                                  (file-name-directory file))))
                    (list (nth 2 event) (nth 3 event))))
              (+noema--external-file-self-write-p file))
    (when-let* ((timer
                 (gethash
                  file +noema--external-file-watch-timers)))
      (cancel-timer timer))
    (puthash
     file
     (run-at-time
      0.25 nil #'+noema--external-file-notify-change file)
     +noema--external-file-watch-timers)))

(defun +noema--external-file-watch-live-p (file)
  "Whether the watch recorded for logical FILE still exists.
Closing its workspace or disconnecting its target releases the watch without
telling Noema.  A record whose owner is gone is dropped, so the next read of
FILE watches it again instead of trusting a watch that no longer reports."
  (when-let* ((owner (gethash file +noema--external-file-watches)))
    (or (remote-workspace-resource-live-p (car owner) (cdr owner))
        (progn
          (remhash file +noema--external-file-watches)
          nil))))

(defun +noema--ensure-external-file-watch (file)
  "Ensure one recoverable Remote watch exists for logical FILE."
  (when (and (bound-and-true-p remote-mode)
             ;; The web-host runs on the client, so it already sees any file
             ;; the client can open natively and watches it itself.  A routed
             ;; watch is needed exactly when it cannot -- which is a backend
             ;; capability, not a target name.
             (not (remote-client-file-name file))
             (not (+noema--external-file-watch-live-p file)))
    (condition-case error
        (let* ((context (remote-context (file-name-directory file)))
               (workspace (remote-workspace-open context :connect nil))
               (resource
                (remote-workspace-add-file-watch
                 workspace file '(change attribute-change)
                 (lambda (event)
                   (+noema--external-file-watch-event file event))
                 :key (list 'noema-external-file file)
                 :metadata
                 (list :application "noema" :file file))))
          (puthash
           file (cons workspace resource)
           +noema--external-file-watches))
      (error
       ;; Editing still works on backends without watch capability; refresh is
       ;; then explicit and saves continue to use mtime conflict detection.
       (message "Noema remote watch unavailable for %s: %s"
                file (error-message-string error))))))

(defun +noema--clear-external-file-watches ()
  "Close Noema's Remote watches and debounce timers."
  (maphash
   (lambda (_file timer)
     (when (timerp timer)
       (cancel-timer timer)))
   +noema--external-file-watch-timers)
  (maphash
   (lambda (_file owner)
     (ignore-errors
       (remote-workspace-close-resource
        (car owner) (cdr owner) 'noema-stop)))
   +noema--external-file-watches)
  (clrhash +noema--external-file-watch-timers)
  (clrhash +noema--external-file-watches)
  (clrhash +noema--external-file-watch-suppressed))

(defun +noema--external-file-read (params _client)
  "Read the logical Markdown file named by gateway PARAMS through Remote."
  (let* ((file
          (+noema--external-file
           (alist-get 'file params)))
         (content
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-substring-no-properties
             (point-min) (point-max)))))
    (+noema--ensure-external-file-watch file)
    (append
     `((file . ,file) (content . ,content))
     (+noema--external-file-metadata file))))

(defun +noema--external-file-write (params _client)
  "Atomically write a logical Markdown file described by gateway PARAMS."
  (let* ((file
          (+noema--external-file
           (alist-get 'file params)))
         (content (format "%s" (or (alist-get 'content params) "")))
         (force (eq (alist-get 'force params) t))
         (base-mtime (alist-get 'baseMtimeMs params))
         (metadata (+noema--external-file-metadata file))
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
             ;; On a logical SSH file `make-nearby-temp-file' may choose the
             ;; target's /tmp, which can be a different filesystem from the
             ;; project and makes the final rename fail with EXDEV.
             (temporary
              (make-temp-file
               (expand-file-name ".noema-save-" default-directory))))
        (puthash
         file (+ (float-time) 30)
         +noema--external-file-watch-suppressed)
        (unwind-protect
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region content nil temporary nil 'silent)
              (rename-file temporary file t)
              (when modes
                (set-file-modes file modes)))
          (when (file-exists-p temporary)
            (ignore-errors (delete-file temporary))))
        (puthash
         file (cons (+ (float-time) 2)
                    (+noema--external-file-signature file))
         +noema--external-file-watch-suppressed)
        (let ((written
               (+noema--external-file-metadata file)))
          `((ok . t) (conflict . :json-false)
            (file . ,file)
            (mtimeMs . ,(alist-get 'mtimeMs written))
            (size . ,(alist-get 'size written)))))))))

(defvar +noema--host-event-queue nil
  "FIFO of Noema events acknowledged but not yet applied by Emacs.")

(defvar +noema--host-event-timer nil
  "One-shot timer draining `+noema--host-event-queue'.")

(defun +noema--drain-host-events ()
  "Apply a bounded FIFO slice after the gateway process filter has returned."
  (setq +noema--host-event-timer nil)
  (let ((remaining 32))
    (while (and +noema--host-event-queue (> remaining 0))
      (pcase-let ((`(,function . ,args) (pop +noema--host-event-queue)))
        (condition-case error
            (apply function args)
          (quit
           (+noema--record-interrupted-operation
            (format "inbound event %S" function))
           (setq remaining 1))
          (error
           (message "Noema deferred host event failed: %s"
                    (error-message-string error)))))
      (setq remaining (1- remaining))))
  (when +noema--host-event-queue
    (setq +noema--host-event-timer
          (run-at-time 0 nil #'+noema--drain-host-events))))

(defun +noema--defer-host-event (function &rest args)
  "Queue host event FUNCTION with ARGS and acknowledge its gateway call now."
  (setq +noema--host-event-queue
        (nconc +noema--host-event-queue
               (list (cons function (copy-tree args)))))
  (unless (timerp +noema--host-event-timer)
    (setq +noema--host-event-timer
          (run-at-time 0 nil #'+noema--drain-host-events))))

(defun +noema--event-line (type payload)
  "Return the unibyte `aaronote-event:' line for TYPE and PAYLOAD.
`json-serialize' returns unibyte UTF-8 bytes.  Concatenating them with a
multibyte TYPE (JSON-RPC decoding hands back multibyte strings) would turn
each payload byte into a raw-byte character, so keep every piece unibyte."
  (let ((json (json-serialize payload)))
    (concat (encode-coding-string (format "aaronote-event:%s:" type) 'utf-8)
            (if (multibyte-string-p json)
                (encode-coding-string json 'utf-8)
              json))))

(defun +noema--gateway-event (params _client)
  "Acknowledge Noema event PARAMS and dispatch it outside the process filter."
  (let* ((type (format "%s" (or (alist-get 'type params) "")))
         (payload (or (alist-get 'payload params) '()))
         (line
          (pcase type
            ("ui-state"
             (+noema--defer-host-event
              #'+noema--handle-ui-state-payload payload)
             nil)
            ("agenda-visit"
             (+noema--defer-host-event
              (lambda (record) (require 'noema-agenda) (noema-agenda-visit-record record)) payload)
             nil)
            ("agenda-changed"
             (when (fboundp 'noema-agenda-handle-change)
               (+noema--defer-host-event #'noema-agenda-handle-change payload))
             (when (fboundp '+dashboard-agenda-handle-change)
               (+noema--defer-host-event #'+dashboard-agenda-handle-change payload))
             nil)
            ("notes-mutated"
             (when (fboundp '+noema-roam--clear-runtime-cache)
               (+noema--defer-host-event
                #'+noema-roam--clear-runtime-cache))
             nil)
            ("note-moved"
             (when (fboundp '+noema-roam--handle-moved-note)
               (+noema--defer-host-event
                #'+noema-roam--handle-moved-note payload))
             nil)
            ("note-trashed"
             (when (fboundp '+noema-roam--handle-trashed-note)
               (+noema--defer-host-event
                #'+noema-roam--handle-trashed-note payload))
             nil)
            ("agenda-attention-changed"
             (+noema--defer-host-event
              (lambda (event)
                (when (fboundp 'noema-agenda--reconcile-files)
                  (noema-agenda--reconcile-files (append (alist-get 'files event) nil)))
                (when (fboundp 'noema-agenda-attention-handle-change)
                  (noema-agenda-attention-handle-change event))) payload)
             nil)
            ("agenda-attention-visit"
             (+noema--defer-host-event #'+noema--agenda-attention-visit payload)
             nil)
            ("latex-export-failed"
             (+noema--defer-host-event #'+noema--latex-export-failed payload)
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
                   (text (alist-get 'text payload))
                   (client (alist-get 'client payload))
                   (host-owned (let ((value (alist-get 'hostOwned payload)))
                                 (and value (not (memq value '(:false :json-false)))))))
               (cond
                ;; Characters typed while Emacs owns the keyboard.
                ((and host-owned (stringp text) (not (string-empty-p text)))
                 (+noema--defer-host-event #'noema--run-emacs-text text))
                ((and (stringp key) (not (string-empty-p key)))
                 (+noema--defer-host-event
                  #'noema--run-emacs-key key client host-owned))))
             nil)
            ("selection-to-agent"
             ;; A pane saved and reported a range for an Emacs action (agent,
             ;; gptel context/rewrite/compose, source buffer); see
             ;; `noema-md-bridge'.  Payloads without an action are agent sends.
             (+noema--defer-host-event
              (lambda (event)
                (require 'noema-md-bridge)
                ;; A refusal (nothing selected, stale pane) is an answer the
                ;; person asked for: show it plainly.
                (condition-case failure
                    (noema-md-bridge-handle-selection event)
                  (user-error (message "%s" (error-message-string failure)))))
              payload)
             nil)
            ("input-focus"
             (+noema--defer-host-event
              #'+noema--handle-input-focus payload)
             nil)
            ("jupyter-session"
             (+noema--defer-host-event
              #'+noema-jupyter-cell-handle-session-event payload)
             nil)
            ("jupyter-debug-ended"
             (+noema--defer-host-event
              #'+noema-jupyter-debug-handle-ended payload)
             nil)
            ("jupyter-cell-select"
             (when (fboundp '+noema-jupyter-cell-select-source)
               (+noema--defer-host-event
                #'+noema-jupyter-cell-select-source payload))
             nil)
            ("research-agent-buffer"
             (when (fboundp 'noema-agent-worker-open-session)
               (+noema--defer-host-event
                #'noema-agent-worker-open-session payload))
             nil)
            ("surface"
             (+noema--defer-host-event
              #'+noema--open-hosted-surface payload)
             nil)
            ("research-worker"
             ;; The document-execution worker owns ACP processes in Emacs.
             ;; Keep gateway dispatch non-blocking and optional so ordinary
             ;; Noema use never depends on the research package being loaded.
             (when (fboundp 'noema-agent-worker-apply-command)
               (+noema--defer-host-event
                #'noema-agent-worker-apply-command payload))
             nil)
            ((or "open" "system-open" "zotero" "zotero-import"
                 "current-file" "saved")
             (+noema--event-line type payload))
            (_ nil))))
    (when line
      (+noema--defer-host-event #'+noema--handle-process-line line))
    '((ok . t))))

(defun +noema--agenda-protected-sources (params _client)
  "Report modified Emacs buffers among requested source files in PARAMS.
This is a memory-only check; it never opens an inactive project or reads disk."
  (let ((files (append (alist-get 'files params) nil)))
    `((files . ,(vconcat
                 (seq-filter
                  (lambda (file)
                    (seq-some (lambda (buffer)
                                (with-current-buffer buffer
                                  (and (buffer-modified-p)
                                       (or (equal file buffer-file-name)
                                           (equal file buffer-file-truename)
                                           (and buffer-file-name
                                                (remote-file-equal-p file buffer-file-name))))))
                              (buffer-list))) files))))))

(remote-gateway-register-method
 "noema.agenda.protected-sources" #'+noema--agenda-protected-sources)
(remote-gateway-register-method
 "noema.agenda.apple"
 (lambda (params client)
   (require 'noema/bridge/agenda-apple)
   (+noema--apple-gateway params client)))
(remote-gateway-register-method
 "noema.agenda.source"
 (lambda (params client)
   (require 'noema/bridge/agenda-source)
   (+noema--agenda-source-request params client)))
(remote-gateway-register-method
 "noema.event" #'+noema--gateway-event)
(remote-gateway-register-method
 "noema.file.read" #'+noema--external-file-read)
(remote-gateway-register-method
 "noema.file.write" #'+noema--external-file-write)

(defvar +noema--latex-agent-requests (make-hash-table :test #'equal)
  "Active LaTeX export polish turns keyed by the web host request id.")

(defun +noema--latex-agent-request-set (request-id key value)
  "Store VALUE under KEY in the active export REQUEST-ID."
  (when-let* ((request (gethash request-id +noema--latex-agent-requests)))
    (puthash request-id (plist-put request key value)
             +noema--latex-agent-requests)
    value))

(defvar +noema--latex-intervention-buffers (make-hash-table :test #'equal)
  "Manual ACP buffers opened for failed LaTeX export task ids.")

(defvar +noema--latex-failure-notified (make-hash-table :test #'equal)
  "Failed LaTeX task ids for which Emacs has already asked about intervention.")

(defun +noema--latex-intervention-display (buffer &optional title)
  "Show export agent BUFFER in its own frame, outside Agent Workspace tabs."
  (let ((window (get-buffer-window buffer t)))
    (unless (window-live-p window)
      (setq window
            (if (display-graphic-p)
                (let ((frame (make-frame `((name . ,(or title "Noema LaTeX intervention"))
                                           (width . 110) (height . 36)))))
                  (set-frame-parameter frame 'noema-latex-intervention-buffer buffer)
                  (set-frame-parameter frame 'noema-agent-standalone t)
                  (frame-root-window frame))
              (display-buffer-pop-up-window buffer '((window-height . 0.4))))))
    (unless (window-live-p window)
      (user-error "Unable to open a separate LaTeX intervention window"))
    (set-window-buffer window buffer)
    (select-frame-set-input-focus (window-frame window))
    (select-window window)
    window))

(defun +noema--latex-export-display (buffer)
  "Show a live export BUFFER outside the ordinary Agent Workspace tabs."
  (+noema--latex-intervention-display buffer "Noema LaTeX export agent"))

(defun +noema--latex-intervention-prompt (params)
  "Draft a reviewable manual repair request from failed export PARAMS."
  (format (concat "Investigate this failed Noema LaTeX export.\n"
                  "Source note: %s\nOutput target: %s\n\n"
                  "Error:\n%s\n\n"
                  "Inspect the source and make only the needed edits. "
                  "Tell me what changed; I will rerun the export from Task Manager.")
          (or (alist-get 'file params) "")
          (or (alist-get 'outputPath params) "")
          (or (alist-get 'error params) "LaTeX export failed")))

(defun +noema--latex-intervention-open (params)
  "Open or focus an independent manual ACP session for failed export PARAMS."
  (require 'noema-agent-acp)
  (let* ((task-id (alist-get 'taskId params))
         (file (alist-get 'file params))
         (output-path (alist-get 'outputPath params))
         (preferred-backend (or (alist-get 'backend params) "codex"))
         (backend (seq-find #'noema-agent-acp-config-for
                            (delete-dups
                             (append (list preferred-backend)
                                     '("codex" "claude" "opencode")))))
         (directory (cond ((and (stringp file) (not (string-empty-p file))
                                (file-directory-p
                                 (file-name-directory (expand-file-name file))))
                           (file-name-directory (expand-file-name file)))
                          ((and (stringp output-path)
                                (not (string-empty-p output-path))
                                (file-directory-p
                                 (file-name-directory (expand-file-name output-path))))
                           (file-name-directory (expand-file-name output-path)))
                          (t default-directory)))
         (existing (gethash task-id +noema--latex-intervention-buffers))
         (config (and backend (noema-agent-acp-config-for backend)))
         (buffer (and (buffer-live-p existing) existing)))
    (unless (and (stringp task-id) (not (string-empty-p task-id)))
      (user-error "Missing failed LaTeX task id"))
    (unless buffer
      (unless config (user-error "No Emacs ACP agent is available for LaTeX intervention"))
      (setq buffer (noema-agent-acp-start
                    :config config :directory directory :origin 'manual
                    :display-function #'+noema--latex-intervention-display))
      (puthash task-id buffer +noema--latex-intervention-buffers)
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (remhash task-id +noema--latex-intervention-buffers)
                    (dolist (frame (frame-list))
                      (when (and (> (length (frame-list)) 1)
                                 (eq (frame-parameter frame
                                                      'noema-latex-intervention-buffer)
                                     (current-buffer)))
                        (delete-frame frame t)))) nil t))
      (let ((draft (+noema--latex-intervention-prompt params)))
        (if (noema-agent-acp-state-value buffer '(:session :id))
            (noema-agent-acp-draft buffer draft)
          (let (subscription)
            (setq subscription
                  (noema-agent-acp-subscribe
                   :buffer buffer :event 'init-finished
                   :callback (lambda (_event)
                               (noema-agent-acp-unsubscribe
                                :buffer buffer :subscription subscription)
                               (when (buffer-live-p buffer)
                                 (noema-agent-acp-draft buffer draft)))))))))
    (noema-agent-acp-show-buffer buffer)
    (message "LaTeX intervention with %s: edit the draft prompt and send it when ready"
             backend)
    buffer))

(defun +noema--latex-export-failed (params)
  "Pop up export diagnostics and ask whether to open manual ACP help."
  (let ((task-id (alist-get 'taskId params)))
    (when (and (stringp task-id)
               (not (gethash task-id +noema--latex-failure-notified)))
      (if (active-minibuffer-window)
          (run-at-time 0.5 nil #'+noema--latex-export-failed params)
        (puthash task-id t +noema--latex-failure-notified)
        (display-warning
         'noema-latex
         (format "LaTeX export failed for %s:\n%s"
                 (file-name-nondirectory (or (alist-get 'file params) "document"))
                 (or (alist-get 'error params) "Unknown error"))
         :error)
        (when (y-or-n-p "LaTeX export failed. Intervene with an agent? ")
          (condition-case err
              (+noema--latex-intervention-open params)
            (error (message "LaTeX intervention failed: %s"
                            (error-message-string err)))))))))

(defun +noema--latex-intervene (params _client)
  "Open manual ACP intervention from a failed Task Manager card."
  (let ((deferred (remote-gateway-defer 15)))
    (+noema--defer-host-event
     (lambda (payload)
       (condition-case err
           (progn
             (+noema--latex-intervention-open payload)
             (remote-gateway-resolve deferred '((ok . t))))
         (error
          (remote-gateway-resolve
           deferred `((ok . :json-false)
                      (message . ,(error-message-string err)))))))
     params)
    deferred))

(defun +noema--latex-agent-open (params _client)
  "Focus the live export ACP session named by PARAMS."
  (let ((deferred (remote-gateway-defer 15)))
    (+noema--defer-host-event
     (lambda (payload)
       (condition-case err
           (let* ((request-id (alist-get 'requestId payload))
                  (request (and (stringp request-id)
                                (gethash request-id +noema--latex-agent-requests)))
                  (buffer (plist-get request :buffer)))
             (unless (buffer-live-p buffer)
               (user-error "This LaTeX export agent session is no longer active"))
             (with-current-buffer buffer
               (setq-local noema-agent-acp-display-buffer-function
                           #'+noema--latex-export-display)
               (unless (bound-and-true-p +noema--latex-export-shown)
                 (setq-local +noema--latex-export-shown t)
                 (add-hook 'kill-buffer-hook
                           (lambda ()
                             (dolist (frame (frame-list))
                               (when (and (> (length (frame-list)) 1)
                                          (eq (frame-parameter frame
                                                               'noema-latex-intervention-buffer)
                                              (current-buffer)))
                                 (delete-frame frame t)))) nil t)))
             (noema-agent-acp-show-buffer buffer)
             (remote-gateway-resolve deferred '((ok . t))))
         (error
          (remote-gateway-resolve
           deferred `((ok . :json-false)
                      (message . ,(error-message-string err)))))))
     params)
    deferred))

(defun +noema--latex-agent-status (_params _client)
  "Report the ACP agents configured in this Emacs for LaTeX export."
  (require 'noema-agent-acp)
  `((agents . ,(vconcat
                (mapcar (lambda (id)
                          `((id . ,id)
                            (available . ,(if (noema-agent-acp-config-for id)
                                              t :json-false))))
                        '("codex" "claude" "opencode"))))))

(defun +noema--latex-agent-retire-buffer (buffer)
  "Close export ACP BUFFER after the current ACP callback returns."
  (when (buffer-live-p buffer)
    (run-at-time
     0 nil
     (lambda (agent-buffer)
       (when (buffer-live-p agent-buffer)
         (unless (ignore-errors (noema-agent-acp-kill agent-buffer))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer agent-buffer)))))
     buffer)))

(defun +noema--latex-agent-retire-workdir (workdir)
  "Close export ACP sessions still attached to WORKDIR."
  (when (and (stringp workdir) (not (string-empty-p workdir)))
    (require 'noema-agent-acp)
    (let ((directory (directory-file-name (expand-file-name workdir))))
      (dolist (session (noema-agent-acp-sessions))
        (when (and (noema-agent-acp-export-session-p session)
                   (equal (directory-file-name (or (plist-get session :root) ""))
                          directory))
          (+noema--latex-agent-retire-buffer (plist-get session :buffer)))))))

(defun +noema--latex-agent-finish (request-id ok &optional message)
  "Finish LaTeX polish REQUEST-ID and retire its private ACP session."
  (when-let* ((request (gethash request-id +noema--latex-agent-requests)))
    (remhash request-id +noema--latex-agent-requests)
    (let ((buffer (plist-get request :buffer))
          (subscription (plist-get request :subscription))
          (progress-subscription (plist-get request :progress-subscription))
          (timer (plist-get request :timer))
          (deferred (plist-get request :deferred)))
      (when (timerp timer) (cancel-timer timer))
      (when (and buffer subscription)
        (ignore-errors
          (noema-agent-acp-unsubscribe :buffer buffer :subscription subscription)))
      (when (and buffer progress-subscription)
        (ignore-errors
          (noema-agent-acp-unsubscribe :buffer buffer
                                       :subscription progress-subscription)))
      (unwind-protect
          (ignore-errors
            (remote-gateway-resolve
             deferred
             `((ok . ,(if ok t :json-false))
               (message . ,(or message "")))))
        ;; ACP may still be dispatching its completion callback. Retire the
        ;; session on the next event turn, even if the gateway is gone.
        (+noema--latex-agent-retire-buffer buffer)))))

(defun +noema--latex-agent-cancel-all ()
  "Retire every LaTeX ACP turn when its web host goes away."
  (let (request-ids)
    (maphash (lambda (request-id _request)
               (push request-id request-ids))
             +noema--latex-agent-requests)
    (dolist (request-id request-ids)
      (+noema--latex-agent-finish request-id nil "Noema host stopped"))
    (when (featurep 'noema-agent-acp)
      (dolist (session (noema-agent-acp-sessions))
        (when (noema-agent-acp-export-session-p session)
          (+noema--latex-agent-retire-buffer (plist-get session :buffer)))))))

(defun +noema--latex-agent-progress (request-id backend event)
  "Report bounded ACP EVENT activity for export REQUEST-ID to Noema."
  (when-let* ((request (gethash request-id +noema--latex-agent-requests))
              (client (remote-gateway-find-client "noema")))
    (let* ((kind (map-elt event :event))
           (now (float-time))
           (last (or (plist-get request :last-progress) 0))
           (text (pcase kind
                   ('tool-call-update (format "%s is using a tool…" backend))
                   ('agent-message-chunk (format "%s is responding…" backend))
                   ('permission-request (format "%s is waiting for permission…" backend))
                   (_ nil))))
      (when (and text
                 (or (eq kind 'permission-request) (>= (- now last) 15)))
        (+noema--latex-agent-request-set request-id :last-progress now)
        (remote-gateway-notify
         client "noema.latex.agent-progress"
         `((requestId . ,request-id) (text . ,text)))))))

(defun +noema--latex-agent-run (params _client)
  "Run a LaTeX polish turn through Noema's Emacs ACP boundary."
  (require 'noema-agent-acp)
  (let* ((request-id (alist-get 'requestId params))
         (backend (alist-get 'backend params))
         (workdir (alist-get 'workdir params))
         (prompt (alist-get 'prompt params))
         (context-items (alist-get 'contextItems params))
         (model (alist-get 'model params))
         (base-config (and (member backend '("codex" "claude" "opencode"))
                           (noema-agent-acp-config-for backend)))
         (config (and base-config
                      (noema-agent-acp-export-config base-config model))))
    (unless (and (stringp request-id) (not (string-empty-p request-id))
                 (not (gethash request-id +noema--latex-agent-requests))
                 config (stringp workdir) (file-directory-p workdir)
                 (file-exists-p (expand-file-name "draft.tex" workdir))
                 (stringp prompt) (not (string-empty-p prompt)))
      (error "Invalid LaTeX ACP export request or unavailable agent"))
    (let ((deferred (remote-gateway-defer 960))
          buffer subscription progress-subscription)
      (puthash request-id
               (list :deferred deferred
                     :timer (run-at-time
                             930 nil
                             (lambda ()
                               (+noema--latex-agent-finish
                                request-id nil "Emacs ACP polish timed out"))))
               +noema--latex-agent-requests)
      (condition-case err
          (progn
            (setq buffer
                  (noema-agent-acp-start :config config :directory workdir
                                         :origin 'latex-export
                                         :display-function
                                         #'+noema--latex-export-display))
            (+noema--latex-agent-request-set request-id :buffer buffer)
            (setq progress-subscription
                  (noema-agent-acp-subscribe
                   :buffer buffer
                   :callback (lambda (event)
                               (+noema--latex-agent-progress
                                request-id backend event))))
            (+noema--latex-agent-request-set
             request-id :progress-subscription progress-subscription)
            (setq subscription
                  (noema-agent-acp-subscribe
                   :buffer buffer :event 'init-finished
                   :callback
                   (lambda (_event)
                     (noema-agent-acp-unsubscribe
                      :buffer buffer :subscription subscription)
                     (when (gethash request-id +noema--latex-agent-requests)
                       (condition-case prompt-error
                           (progn
                             (when (and (stringp model) (not (string-empty-p model)))
                               (let ((selected (noema-agent-acp-export-model-id model buffer))
                                     (current (noema-agent-acp-current-model-id buffer)))
                                 (unless (and selected (equal current selected))
                                   (error "Export model %s unavailable or not selected (current: %s)"
                                          model (or current "unknown")))))
                             (noema-agent-acp-prompt
                            :buffer buffer
                            :content (noema-agent-acp-context-blocks
                                      buffer prompt context-items)
                            :on-success
                            (lambda (response)
                              (+noema--latex-agent-finish
                               request-id
                               (not (equal (alist-get 'stopReason response)
                                           "cancelled"))
                               (or (alist-get 'stopReason response) "")))
                            :on-failure
                            (lambda (error-object _raw)
                              (+noema--latex-agent-finish
                               request-id nil (format "%s" error-object)))))
                         (error
                          (+noema--latex-agent-finish
                           request-id nil (error-message-string prompt-error))))))))
            (+noema--latex-agent-request-set
             request-id :subscription subscription))
        (error
         (+noema--latex-agent-finish
          request-id nil (error-message-string err))))
      deferred)))

(defun +noema--latex-agent-cancel (params _client)
  "Cancel a LaTeX export's ACP turn after task abort or timeout."
  (let* ((request-id (alist-get 'requestId params))
         (workdir (alist-get 'workdir params))
         (request (gethash request-id +noema--latex-agent-requests))
         (buffer (plist-get request :buffer)))
    (when request
      (when (buffer-live-p buffer)
        (ignore-errors (noema-agent-acp-interrupt buffer t)))
      (+noema--latex-agent-finish request-id nil "aborted"))
    ;; Export sends this again on completion. It also catches a buffer whose
    ;; ACP callback finished but whose first retirement attempt did not.
    (+noema--latex-agent-retire-workdir workdir))
  `((ok . t)))

(remote-gateway-register-method
 "noema.latex.agent-status" #'+noema--latex-agent-status)
(remote-gateway-register-method
 "noema.latex.agent-run" #'+noema--latex-agent-run)
(remote-gateway-register-method
 "noema.latex.agent-cancel" #'+noema--latex-agent-cancel)
(remote-gateway-register-method
 "noema.latex.agent-open" #'+noema--latex-agent-open)
(remote-gateway-register-method
 "noema.latex.intervene" #'+noema--latex-intervene)

(defun +noema--clear-process-log-queue ()
  "Cancel and discard deferred web-host diagnostic output."
  (when (timerp +noema--process-log-timer)
    (cancel-timer +noema--process-log-timer))
  (setq +noema--process-log-timer nil
        +noema--process-log-queue nil
        +noema--process-log-bytes 0))

(defun +noema--flush-process-log ()
  "Append queued diagnostics after the child-process filter has returned."
  (setq +noema--process-log-timer nil)
  (let ((entries (nreverse +noema--process-log-queue))
        touched)
    (setq +noema--process-log-queue nil
          +noema--process-log-bytes 0)
    (dolist (entry entries)
      (let* ((proc (car entry))
             (buffer (and (processp proc) (process-buffer proc))))
        (when (buffer-live-p buffer)
          (condition-case nil
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert (cdr entry)))
            (quit
             (+noema--record-interrupted-operation
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

(defun +noema--process-filter (proc output)
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
      (when (> (+ +noema--process-log-bytes bytes)
               +noema--process-log-queue-limit)
        (setq +noema--process-log-queue
              (list (cons proc "\n[Noema diagnostic burst truncated]\n"))
              +noema--process-log-bytes 36))
      (when (> bytes +noema--process-log-queue-limit)
        (setq output
              (substring output
                         (max 0 (- (length output)
                                   (/ +noema--process-log-queue-limit 2)))))
        (setq bytes (string-bytes output)))
      (push (cons proc output) +noema--process-log-queue)
      (cl-incf +noema--process-log-bytes bytes))
    (unless (timerp +noema--process-log-timer)
      (setq +noema--process-log-timer
            (run-at-time 0 nil #'+noema--flush-process-log))))
  nil)

(defun +noema--sentinel (proc event)
  "Handle web-host PROC state change EVENT."
  (when (and (eq proc +noema--process)
             (not (process-live-p proc)))
    (+noema--latex-agent-cancel-all)
    (when (fboundp 'noema-agenda-host-stopped)
      (noema-agenda-host-stopped))
    (when +noema--ready-watchdog
      (cancel-timer +noema--ready-watchdog)
      (setq +noema--ready-watchdog nil))
    (when +noema--goto-timer
      (cancel-timer +noema--goto-timer)
      (setq +noema--goto-timer nil))
    (let ((dropped (length +noema--ready-callbacks)))
      (setq +noema--goto-last nil
            +noema--process nil
            +noema--port nil
            +noema--ready nil
            +noema--ready-callbacks nil)
      ;; Anything queued behind `+noema--ensure-server' -- a run, an
      ;; interrupt, a kernel restart -- dies with the host.  Dropping those
      ;; silently is what makes the UI look like it simply ignored the key.
      (unless (zerop dropped)
        (message "Noema web-host stopped with %d pending request%s; they were dropped"
                 dropped (if (= dropped 1) "" "s")))
      ;; Its gateway binding outlives the process otherwise, so a restart
      ;; cannot reclaim it.
      (when +noema--gateway-binding
        (ignore-errors
          (remote-gateway-release-binding +noema--gateway-binding t))
        (setq +noema--gateway-binding nil))
      (unless (string-match-p "^finished" event)
        (message "Noema web-host: %s" (string-trim event))))))

(defun +noema-buffer-file (&optional buffer)
  "Return the Noema note file represented by BUFFER.
When BUFFER is nil, inspect the current buffer."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (and (stringp +noema-buffer-file-name)
           (not (string-empty-p +noema-buffer-file-name))
           +noema-buffer-file-name))))

(defun +noema--buffer-display-name (&optional file)
  "Return the preferred Noema buffer display name for FILE."
  (if-let* ((file (+noema--canonical-file file)))
      (format "*Noema: %s*" (file-name-nondirectory file))
    "*Noema*"))

(defun +noema--split-buffer-display-name (file ordinal)
  "Return an ibuffer-friendly name for FILE's split ORDINAL."
  (format "*Noema split %d: %s*"
          ordinal
          (if-let* ((file (+noema--canonical-file file)))
              (file-name-nondirectory file)
            "Noema")))

(defun +noema--split-client-ordinal (client)
  "Return split ordinal encoded in CLIENT, or nil."
  (when (+noema--split-client-p client)
    (let ((value (car (last (split-string client ":" t)))))
      (when (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)))))

(defun +noema--rename-live-buffers ()
  "Refresh user-visible names of live Noema xwidget buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (memq major-mode '(xwidget-webkit-mode portal-web-mode))
                   (or +noema-buffer-file-name +noema--client-id))
          (let ((name
                 (if (+noema--split-client-p +noema--client-id)
                     (+noema--split-buffer-display-name
                      +noema-buffer-file-name
                      (or (+noema--split-client-ordinal
                           +noema--client-id)
                          0))
                   (+noema--buffer-display-name
                    +noema-buffer-file-name))))
            (setq-local +noema--xwidget-forced-name name)
            (unless (equal (buffer-name) name)
              (rename-buffer name t))))))))

(defun +noema--notify-client-closed (&optional client file)
  "Notify the Noema core that CLIENT no longer has a live view."
  (when (and (stringp client) (not (string-empty-p client)))
    (condition-case nil
        (+noema--post
         `((type . "client-close")
           (client . ,client)
           ,@(when (and (stringp file) (not (string-empty-p file)))
               `((file . ,(+noema--host-file file))))))
      (error nil))))

(defun +noema--cleanup-buffer ()
  "Remove the current buffer from Noema identity registries."
  (+noema--notify-client-closed
   +noema--client-id
   +noema-buffer-file-name)
  (when (and (stringp +noema--registered-file)
             (eq (gethash +noema--registered-file +noema--file-buffers)
                 (current-buffer)))
    (remhash +noema--registered-file +noema--file-buffers))
  (when (and (stringp +noema--client-id)
             (eq (gethash +noema--client-id +noema--client-buffers)
                 (current-buffer)))
    (remhash +noema--client-id +noema--client-buffers))
  (when (eq +noema--app-buffer (current-buffer))
    (setq +noema--app-buffer nil)))

(defun +noema--refresh-visible-ibuffers ()
  "Refresh visible ibuffer buffers after Noema identity changes."
  (when (fboundp 'ibuffer-update)
    (dolist (buffer (buffer-list))
      (when (get-buffer-window buffer 'visible)
        (with-current-buffer buffer
          (when (derived-mode-p 'ibuffer-mode)
            (let ((inhibit-message t))
              (revert-buffer nil t))))))))

(defun +noema--buffer-for-client (client)
  "Return the live Noema buffer for CLIENT, or nil."
  (when (and (stringp client) (not (string-empty-p client)))
    (let ((buffer (gethash client +noema--client-buffers)))
      (unless (or (null buffer)
                  (and (buffer-live-p buffer)
                       (equal (buffer-local-value '+noema--client-id buffer)
                              client)))
        (remhash client +noema--client-buffers)
        (setq buffer nil))
      (or buffer
          (cl-find-if
           (lambda (buf)
             (and (buffer-live-p buf)
                  (with-current-buffer buf
                    (and (memq major-mode '(xwidget-webkit-mode portal-web-mode))
                         (stringp +noema--client-id)
                         (string-equal +noema--client-id client)))))
           (buffer-list))))))

(defun +noema--register-buffer (buffer file &optional client rename)
  "Register BUFFER as the Noema browser for FILE and CLIENT.
When RENAME is non-nil, rename xwidget buffers to a note-specific name."
  (when (buffer-live-p buffer)
    (let* ((file (+noema--canonical-file file))
           (client (and (stringp client)
                        (not (string-empty-p client))
                        client))
           (split-client (+noema--split-client-p client))
           changed)
      (with-current-buffer buffer
        (let ((old-file +noema--registered-file)
              (old-client +noema--client-id))
          (when (and (stringp old-file)
                     (not (equal old-file file))
                     (eq (gethash old-file +noema--file-buffers) buffer))
            (remhash old-file +noema--file-buffers))
          (when (and (stringp old-client)
                     (not (equal old-client client))
                     (eq (gethash old-client +noema--client-buffers) buffer))
            (remhash old-client +noema--client-buffers))
          (setq changed (or (not (equal +noema-buffer-file-name file))
                            (not (equal +noema--client-id client)))))
        (setq-local +noema-buffer-file-name file)
        (setq-local +noema--registered-file file)
        (setq-local +noema--client-id client)
        (setq-local +noema--xwidget-forced-name
                    (if split-client
                        (+noema--split-buffer-display-name
                         file
                         (or (+noema--split-client-ordinal client) 0))
                      (+noema--buffer-display-name file)))
        (when (eq major-mode 'xwidget-webkit-mode)
          (noema--harden-xwidget-placeholder))
        (+noema-keys-mode 1)
        (when (eq major-mode 'xwidget-webkit-mode)
          (noema--sync-xwidget-recovery-mode))
        (when file
          (setq-local default-directory
                      (file-name-as-directory (file-name-directory file)))
          ;; Xwidget buffers acquire their project directory after their
          ;; major-mode hooks have run.  Notify the environment integration at
          ;; the point where the directory actually becomes authoritative.
          (when (fboundp '+direnv-schedule-current-buffer)
            (+direnv-schedule-current-buffer)))
        (add-hook 'kill-buffer-hook #'+noema--cleanup-buffer nil t)
        (when (and rename
                   (memq major-mode '(xwidget-webkit-mode portal-web-mode))
                   (not (equal (buffer-name)
                               +noema--xwidget-forced-name)))
          (rename-buffer +noema--xwidget-forced-name t)
          (setq changed t))
        (when changed
          (force-mode-line-update)
          (force-window-update (current-buffer))))
      (when (and file (not (+noema--split-client-p client)))
        (puthash file buffer +noema--file-buffers))
      (when client
        (puthash client buffer +noema--client-buffers))
      (when (and (eq major-mode 'xwidget-webkit-mode)
                 (fboundp '+noema--setup-native-chrome))
        (+noema--setup-native-chrome))
      (when (eq major-mode 'portal-web-mode)
        (+noema--setup-portal-chrome))
      (when changed
        (+noema--refresh-visible-ibuffers)
        (when (fboundp '+tab-line-refresh)
          (+tab-line-refresh)))
      buffer)))

(defun +noema--sync-app-buffer-file (file &optional client)
  "Record FILE as the current note in the matching Noema buffer.
CLIENT, when present, identifies the exact xwidget page that reported the
file switch."
  (let* ((file (+noema--canonical-file file))
         ;; A client-bearing event belongs to exactly one page.  A late event
         ;; from a closed page must not relabel whichever pane is active now.
         (target (if (and (stringp client) (not (string-empty-p client)))
                     (+noema--buffer-for-client client)
                   (or (and file (+noema--buffer-for-file file))
                       +noema--app-buffer))))
    (when (buffer-live-p target)
      (+noema--register-buffer target file client t)
      (with-current-buffer target
        (setq-local +noema--activity-paused :unknown))
      (when file
        (setq +noema--app-buffer target))
      (when +noema--activity-hooks-installed
        (setq +noema--last-activity-signature :unknown)
        (+noema--update-activity)))))

(defun +noema--track-app-buffer (buffer &optional file client)
  "Record BUFFER as the active Noema browser buffer.
When FILE is non-nil, set buffer-local file tracking directly."
  (setq +noema--app-buffer buffer)
  (when (buffer-live-p buffer)
    (+noema--register-buffer buffer file client t)))

(defun +noema--buffer-for-file (file)
  "Return a live Noema buffer tracking FILE, or nil."
  (when-let* ((abs (+noema--canonical-file file)))
    (let ((registered (gethash abs +noema--file-buffers)))
      (unless (or (null registered)
                  (and (buffer-live-p registered)
                       (equal (+noema-buffer-file registered) abs)
                       (not (+noema--split-client-p
                             (buffer-local-value '+noema--client-id
                                                 registered)))))
        (remhash abs +noema--file-buffers)
        (setq registered nil))
      (or registered
          (when-let* ((found
                       (cl-find-if
                        (lambda (buf)
                          (and (buffer-live-p buf)
                               (with-current-buffer buf
                                 (and (stringp +noema-buffer-file-name)
                                      (not (+noema--split-client-p
                                            +noema--client-id))
                                      (string-equal
                                       (expand-file-name +noema-buffer-file-name)
                                       abs)))))
                        (buffer-list))))
            (puthash abs found +noema--file-buffers)
            found)))))

(defun +noema-canonical-buffer (&optional buffer)
  "Return the canonical Noema buffer for BUFFER's file, or BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (or (when-let* ((file (+noema-buffer-file buffer)))
            (+noema--buffer-for-file file))
          buffer))))

(defun +noema--url-with-client (url client)
  "Return URL with its `client' query parameter set to CLIENT.
The URL and xwidget session registry must name the same page client."
  (if (or (not (stringp client)) (string-empty-p client))
      url
    (let ((encoded (url-hexify-string client)))
      (cond
       ((string-match "\\([?&]client=\\)[^&#]*" url)
        (replace-match (concat (match-string 1 url) encoded) t t url))
       ((string-match "#" url)
        (let* ((base (substring url 0 (match-beginning 0)))
               (fragment (substring url (match-beginning 0))))
          (concat base (if (string-match-p "?" base) "&" "?")
                  "client=" encoded fragment)))
       (t
        (concat url (if (string-match-p "?" url) "&" "?")
                "client=" encoded))))))

(defun +noema--open-xwidget (url &optional file)
  "Open Noema in an xwidget session showing FILE.
Switching to an already-open file reuses its buffer without reloading.
An in-page link can change the file shown by a live client; that client's id
stays fixed while the file registry follows the page.  Hosted surfaces share
the \"noema:surface\" session."
  (unless (fboundp '+xwidget/open-url)
    (require 'app/browser/config))
  (let* ((file (+noema--canonical-file file))
         ;; Hosted Wiki/Agenda/Config is a separate page family from the
         ;; editor.  Give it one stable client even if the editor's legacy
         ;; singleton id is already displaying a note.
         (nominal-id (if file (+noema--xwidget-session-id file)
                       "noema:surface"))
         (nominal-buffer (and (fboundp '+xwidget-session-buffer)
                              (+xwidget-session-buffer nominal-id)))
         (existing (or (and file (+noema--buffer-for-file file))
                       (and (buffer-live-p nominal-buffer)
                            (equal (+noema-buffer-file nominal-buffer) file)
                            nominal-buffer)))
         ;; An in-page link may move the nominal client from A to B.  Keep
         ;; that client's identity and give a later independent open of A a
         ;; fresh id instead of reusing B's live page under A's old id.
         (id (if (and (not existing) (buffer-live-p nominal-buffer))
                 (format "%s:visit:%d" nominal-id
                         (cl-incf +noema--split-counter))
               nominal-id))
         (url (if file
                  (+noema--app-url file id)
                ;; Wiki, Agenda, Config and Graph pages learn their client from
                ;; the URL too; otherwise commands addressed to this buffer
                ;; (focus, host-owns-keyboard) never reach them.
                (+noema--url-with-client url id))))
    (if existing
        ;; Session already alive for this file: switch to it without reloading.
        (progn
          ;; One WebKit session rendered in two Emacs windows has unreliable
          ;; drawing and input.  Visit its current window instead of showing
          ;; the same xwidget buffer a second time.
          (if-let* ((window (get-buffer-window existing 'visible)))
              (select-window window)
            (switch-to-buffer existing))
          (with-current-buffer existing
            (when (fboundp '+xwidget-setup-control-line)
              (+xwidget-setup-control-line)))
          ;; Singleton hosted surfaces have distinct routes.  Reusing their
          ;; buffer must visit the requested route, not just display whichever
          ;; Wiki/Graph/Agenda page happened to be there last.
          (when (and (null file)
                     (fboundp '+xwidget-current-url)
                     (not (equal (+xwidget-current-url existing) url)))
            (+xwidget/open-url url :id id :display 'current
                                 :reuse-selected t))
          (run-at-time 0.3 nil #'noema--focus-xwidget-buffer existing)
          (+noema--track-app-buffer
           existing file
           (or (with-current-buffer existing +noema--client-id) id))
          existing)
      ;; New session: open directly at the target URL.
      (let ((buffer (+xwidget/open-url url
                                         :id id
                                         :display 'current
                                         :reuse-selected t)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp '+xwidget-setup-control-line)
              (+xwidget-setup-control-line))))
        (+noema--track-app-buffer buffer file id)
        buffer))))

(defun +noema--open-url (url &optional file _force-new)
  "Open Noema URL using `+noema-backend'."
  (pcase +noema-backend
    ('portal (+noema-portal-open url file))
    ('xwidget (+noema--open-xwidget url file))
    (_ (user-error "Unsupported Noema backend: %S" +noema-backend))))

(defun +noema--open-hosted-surface (payload)
  "Open the safe Noema web surface named by PAYLOAD inside Emacs.
The three surfaces share one stable host buffer.  Keep the source editor
visible beside it, and reuse an already visible surface window."
  (let* ((path (and (listp payload) (alist-get 'path payload)))
         (client (and (listp payload) (alist-get 'client payload)))
         (source-window
          (if (and (stringp client) (not (string-empty-p client)))
              (when-let* ((buffer (+noema--buffer-for-client client)))
                (get-buffer-window buffer 'visible))
            (selected-window)))
         (source-buffer (and (window-live-p source-window)
                             (window-buffer source-window))))
    (if (not (and (stringp path)
                  (string-match-p
                   "\\`/\\(?:config\\|wiki\\|agenda\\)\\(?:\\?[^#\n\r]*\\)?\\'"
                   path)))
        (message "Noema ignored unsafe hosted surface: %S" path)
      (when (window-live-p source-window)
        (+noema--ensure-server
         (lambda ()
           (when (and (window-live-p source-window)
                      (eq (window-buffer source-window) source-buffer))
             (let* ((activate (eq (selected-window) source-window))
                    (existing (pcase +noema-backend
                                ('portal
                                 (+noema-portal--session-buffer
                                  "noema:surface"))
                                ('xwidget
                                 (and (fboundp '+xwidget-session-buffer)
                                      (+xwidget-session-buffer
                                       "noema:surface")))))
                    (visible (and existing
                                  (get-buffer-window existing 'visible)))
                    (created (and (not visible)
                                  (not (eq source-buffer existing))))
                    (target-window
                     (or visible
                         (if created
                             (with-selected-window source-window
                               (+noema--split-window))
                           source-window)))
                    (success nil))
               (unwind-protect
                   (with-selected-window target-window
                     (+noema--open-url
                      (+noema--server-url path) nil nil)
                     (setq success t))
                 (when (and created (not success)
                            (window-live-p target-window))
                   (delete-window target-window)))
               (when (and activate (window-live-p target-window))
                 (select-window target-window))))))))))

(defvar +noema--post-queue nil
  "FIFO of notification payloads waiting to leave the Emacs command loop.")

(defvar +noema--post-timer nil
  "One-shot timer draining `+noema--post-queue'.")

(defvar +noema--post-awaiting-ready nil
  "Non-nil while the notification queue is waiting for web-host readiness.")

(defconst +noema--post-queue-limit 512
  "Maximum pending Noema notifications retained while its host is unavailable.")

(defun +noema--post-now (payload)
  "Write one small control PAYLOAD to an already-ready Noema web-host."
  (when-let* ((client
               (and +noema--ready
                    (remote-gateway-find-client "noema"))))
    (remote-gateway-notify client "noema.command" payload)
    t))

(defun +noema--schedule-post-drain ()
  "Schedule one notification-queue drain without stacking timers."
  (unless (timerp +noema--post-timer)
    (setq +noema--post-timer
          (run-at-time 0 nil #'+noema--drain-post-queue))))

(defun +noema--clear-post-queue ()
  "Cancel and forget every pending Noema notification or inbound event."
  (when (timerp +noema--post-timer)
    (cancel-timer +noema--post-timer))
  (when (timerp +noema--host-event-timer)
    (cancel-timer +noema--host-event-timer))
  (setq +noema--post-timer nil
        +noema--post-queue nil
        +noema--post-awaiting-ready nil
        +noema--host-event-timer nil
        +noema--host-event-queue nil))

(defun +noema--drain-post-queue ()
  "Drain a bounded slice of queued Noema notifications."
  (setq +noema--post-timer nil)
  (cond
   ((null +noema--post-queue)
    (setq +noema--post-awaiting-ready nil))
   ((not (and +noema--ready
              (remote-gateway-find-client "noema")))
    (unless +noema--post-awaiting-ready
      (setq +noema--post-awaiting-ready t)
      (+noema--ensure-server
       (lambda ()
         (setq +noema--post-awaiting-ready nil)
         (+noema--schedule-post-drain)))))
   (t
    ;; Keep a burst of shell activity/focus notifications from monopolizing
    ;; one Emacs command-loop turn. A local gateway write is notification-only;
    ;; no response is registered or awaited.
    (let ((remaining 32))
      (while (and +noema--post-queue (> remaining 0))
        (let ((payload (pop +noema--post-queue)))
          (condition-case error
              (+noema--post-now payload)
            (quit
             (+noema--record-interrupted-operation
              (format "outbound notification %s"
                      (or (alist-get 'command payload)
                          (alist-get 'type payload)
                          "unknown")))
             (setq remaining 1))
            (error
             (message "Noema notification failed: %s"
                      (error-message-string error)))))
        (setq remaining (1- remaining))))
    (when +noema--post-queue
      (+noema--schedule-post-drain)))))

(defun +noema--post (payload)
  "Queue control PAYLOAD without doing socket I/O in the caller's stack."
  (let* ((command (alist-get 'command payload))
         (client (or (alist-get 'client payload) ""))
         (replaceable
          (cond
           ((member command '("pause" "resume")) (list client 'activity))
           ((equal command "focus") (list client 'focus)))))
    (when replaceable
      (setq +noema--post-queue
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
             +noema--post-queue))))
  (setq +noema--post-queue
        (nconc +noema--post-queue (list (copy-tree payload))))
  (when (> (length +noema--post-queue) +noema--post-queue-limit)
    (setq +noema--post-queue
          (last +noema--post-queue +noema--post-queue-limit)))
  (+noema--schedule-post-drain)
  t)

(defun +noema--open-file-in-web (file)
  "Ask the already open Noema page to open FILE."
  (+noema--sync-app-buffer-file file)
  (+noema--post
   `((type . "open") (file . ,(+noema--host-file file)))))

(defun +noema--send-command (command &optional detail)
  "Dispatch Noema COMMAND with optional DETAIL."
  (let ((client (and (boundp '+noema--client-id)
                     (stringp +noema--client-id)
                     (not (string-empty-p +noema--client-id))
                     +noema--client-id)))
    (+noema--post
     `((type . "command")
       (command . ,command)
       ,@(when client `((client . ,client)))
       ,@(when detail `((detail . ,detail)))))))

(defun +noema--goto-location (file line col)
  "Open FILE in Emacs and move to one-based LINE and zero-based COL.
When FILE is nil, use the current buffer."
  (let* ((abs (and (stringp file)
                   (not (string-empty-p file))
                   (ignore-errors
                     (+noema--canonical-file
                      (file-truename
                       (+noema--canonical-file file))))))
         (key (list abs (truncate (or line 1)) (truncate (or col 0)))))
    (let ((same-location (equal key +noema--goto-last))
          (buffer (if abs
                      (or (find-buffer-visiting abs)
                          (aaron-open-visit-file-noselect
                           (aaron-open-resolve-file-name abs)))
                    (current-buffer))))
      (setq +noema--goto-last key)
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

(defvar +noema--markdown-redirected)
(defvar +noema--markdown-placeholder-state)
(defvar +noema--markdown-open-generation)
(defvar +noema-close-emacs-markdown-buffer)

(defun +noema--redirect-placeholder-window (file)
  "Return a visible window showing the redirected Emacs buffer of FILE.
While the web-host boots, the Markdown redirect leaves that buffer on screen
as the placeholder its Noema pane will replace."
  (get-window-with-predicate
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (and (bound-and-true-p +noema--markdown-redirected)
            buffer-file-name
            (equal (+noema--canonical-file buffer-file-name) file))))
   'no-minibuffer 'visible))

;;;###autoload
(defun +noema/open-file (file &optional origin-window)
  "Open Markdown FILE in Noema Web from ORIGIN-WINDOW."
  (interactive "fMarkdown file: ")
  (unless (+noema--markdown-file-p file)
    (user-error "Noema opens Markdown files, not %s" file))
  (let* ((file (+noema--canonical-file file))
         (origin (if (window-live-p origin-window)
                     origin-window (selected-window)))
         (source-buffer (window-buffer origin))
         (placeholder (and (bound-and-true-p +noema--markdown-redirected)
                           (current-buffer)))
         (generation (and placeholder +noema--markdown-open-generation)))
    (+noema--ensure-server
     (lambda ()
       ;; A cold host answers after the command that asked has returned.  By
       ;; then `find-file' has put the redirected buffer in a window, which
       ;; need not be ORIGIN; the pane belongs wherever that placeholder is.
       (when (or (null placeholder)
                 (and (buffer-live-p placeholder)
                      (with-current-buffer placeholder
                        (and +noema--markdown-redirected
                             (= generation +noema--markdown-open-generation)))))
         (if-let* ((target-window
                    (or (and placeholder
                             (get-buffer-window placeholder 'visible))
                        (+noema--redirect-placeholder-window file)
                        (and (window-live-p origin)
                             (eq (window-buffer origin) source-buffer)
                             origin))))
             (let ((activate (eq (selected-window) target-window))
                   result)
               (condition-case err
                   (with-selected-window target-window
                     (setq result
                           (+noema--open-url
                            (+noema--app-url
                             file (+noema--xwidget-session-id file))
                            file nil)))
                 ((error quit)
                  (if placeholder
                      (progn
                        (when (buffer-live-p placeholder)
                          (with-current-buffer placeholder
                            (+noema--markdown-placeholder-set-state 'failed)))
                        (message "Noema could not open %s: %s"
                                 (abbreviate-file-name file)
                                 (error-message-string err)))
                    (signal (car err) (cdr err)))))
               (when (buffer-live-p placeholder)
                 (with-current-buffer placeholder
                   (+noema--markdown-placeholder-set-state
                    (if (buffer-live-p result) 'opened 'failed)))
                 (when (and (buffer-live-p result)
                            +noema-close-emacs-markdown-buffer)
                   (run-at-time
                    0 nil #'+noema--kill-redirected-markdown-buffer
                    placeholder file)))
               (when (and activate (buffer-live-p result))
                 (when-let* ((window (get-buffer-window result 'visible)))
                   (select-window window))))
           (when placeholder
             (with-current-buffer placeholder
               (+noema--markdown-placeholder-set-state 'waiting)))))))))

;;;###autoload
(defun +noema/open-current-note ()
  "Open the current Markdown note in Noema Web."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (+noema/open-file buffer-file-name))

(defun +noema--current-note-file ()
  "Return the Markdown note represented by the current context."
  (or (+noema-buffer-file)
      (and buffer-file-name
           (+noema--markdown-file-p buffer-file-name)
           buffer-file-name)))

(defun +noema--split-window (&optional direction)
  "Create and select the window for an Noema split.
DIRECTION `frame' puts the split in a frame of its own.  That frame starts
on a neutral buffer: created from the page, it would show and resize the
page's own xwidget."
  (let* ((direction (or direction +noema--split-direction
                        (if (>= (window-total-width) 120) 'right 'below)))
         (window (pcase direction
                   ('below (split-window-below))
                   ('frame
                    (let ((frame (if (fboundp '+background-make-frame)
                                     (+background-make-frame)
                                   (with-current-buffer
                                       (get-scratch-buffer-create)
                                     (make-frame)))))
                      (select-frame-set-input-focus frame)
                      (frame-selected-window frame)))
                   (_ (split-window-right)))))
    (select-window window)
    window))

(defun +noema--discard-split-window (window)
  "Remove WINDOW, a split that failed to open, with its frame if it has one."
  (if (eq window (frame-root-window (window-frame window)))
      (delete-frame (window-frame window) t)
    (delete-window window)))

;;;###autoload
(defun +noema/open-current-note-split (&optional target-file origin-window extra-params)
  "Open the current Markdown note in a fresh editable Noema xwidget split.

This intentionally does not reuse the canonical Noema xwidget for the
file.  Multiple xwidget windows for the same live session have rendering
issues, so this command creates an isolated editable client while keeping the
normal file/session reuse map owned by the canonical pane.  TARGET-FILE and
ORIGIN-WINDOW let a link open another note beside its source; EXTRA-PARAMS
carry optional initial anchor coordinates."
  (interactive)
  (let ((file (or target-file (+noema--current-note-file))))
    (unless (and file (+noema--markdown-file-p file))
      (user-error "No current Markdown note for Noema"))
    (let* ((file (+noema--canonical-file file))
           (source-window (if (window-live-p origin-window)
                              origin-window (selected-window)))
           (source-buffer (window-buffer source-window))
           ;; Captured: the server callback may run after the caller's
           ;; binding is gone.
           (direction +noema--split-direction))
      (+noema--ensure-server
       (lambda ()
         (when (and (window-live-p source-window)
                    (eq (window-buffer source-window) source-buffer))
           (let ((activate (eq (selected-window) source-window))
                 target-window)
             (with-selected-window source-window
               (when (eq +noema-backend 'xwidget)
                 (unless (fboundp '+xwidget/open-url)
                   (require 'app/browser/config)))
               (let* ((ordinal (cl-incf +noema--split-counter))
                      (client (format "noema-split:%s:%d"
                                      (file-truename file) ordinal))
                      (url (+noema--app-url file client extra-params))
                      (success nil))
                 (setq target-window (+noema--split-window direction))
                 (unwind-protect
                     (let ((buffer (if (eq +noema-backend 'portal)
                                       (+noema-portal-open-client url client file)
                                     (+xwidget/open-url
                                      url :id client :display 'current
                                      :force-new t :reuse-selected t))))
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when (eq +noema-backend 'xwidget)
                             (setq-local +noema-buffer-file-name file)
                             (setq-local +noema--client-id client)
                             (setq-local +noema--registered-file nil)
                             (setq-local +noema--xwidget-forced-name
                                         (+noema--split-buffer-display-name
                                          file ordinal))
                             (puthash client (current-buffer)
                                      +noema--client-buffers)
                             (add-hook 'kill-buffer-hook
                                       #'+noema--cleanup-buffer nil t)
                             (rename-buffer +noema--xwidget-forced-name t)
                             (setq-local default-directory
                                         (file-name-as-directory
                                          (file-name-directory file)))
                             (when (fboundp '+direnv-schedule-current-buffer)
                               (+direnv-schedule-current-buffer))
                             (when (fboundp '+xwidget-setup-control-line)
                               (+xwidget-setup-control-line))
                             (noema--harden-xwidget-placeholder)
                             (+noema-keys-mode 1)
                             (noema--sync-xwidget-recovery-mode)))
                         (setq success t)
                         (+noema--refresh-visible-ibuffers)))
                   (unless success
                     (when (and (window-live-p target-window)
                                (not (eq target-window source-window)))
                       (+noema--discard-split-window target-window))))))
             (when (and activate (window-live-p target-window))
               (select-window target-window)))))))))

;;;###autoload
(defun +noema/open-current-note-split-right ()
  "Open the current note in a fresh Noema pane to the right."
  (interactive)
  (let ((+noema--split-direction 'right))
    (+noema/open-current-note-split)))

;;;###autoload
(defun +noema/open-current-note-split-below ()
  "Open the current note in a fresh Noema pane below."
  (interactive)
  (let ((+noema--split-direction 'below))
    (+noema/open-current-note-split)))

;;;###autoload
(defun +noema-open-file-split-frame (file)
  "Open Noema note FILE in a fresh editable pane in a new frame."
  (let ((+noema--split-direction 'frame))
    (+noema/open-current-note-split file)))

;;;###autoload
(defun +noema/open-current-note-split-frame ()
  "Open the current note in a fresh Noema pane in a new frame."
  (interactive)
  (+noema-open-file-split-frame (+noema--current-note-file)))

;;;###autoload
(defun +noema/preview ()
  "Compatibility alias: open the current note in Noema."
  (interactive)
  (+noema/open-current-note))

;;;###autoload
(defun +noema/sync-cursor ()
  "Open the current note in Noema.
Cursor-level sync is intentionally no longer a per-keystroke preview channel."
  (interactive)
  (+noema/open-current-note))

;;;###autoload
(defun +noema/refresh ()
  "Refresh the current Noema note while preserving page cursor state."
  (interactive)
  (if (and +noema--ready
           (or (and (boundp '+noema--client-id)
                    (stringp +noema--client-id)
                    (not (string-empty-p +noema--client-id)))
               (buffer-live-p +noema--app-buffer)))
      (progn
        (+noema/command "refresh"))
    (+noema/open-current-note)))

;;;###autoload
(defun +noema/command (command &optional detail)
  "Queue COMMAND with optional DETAIL for the open Noema page.
This function never starts the host, writes a socket, evaluates JavaScript or
waits for a response in the invoking Emacs command."
  (interactive "sNoema command: ")
  ;; `+noema--send-command' captures the current pane id into the queued
  ;; payload now; the later timer therefore cannot turn it into a broadcast.
  (+noema--send-command command detail))

;;;###autoload
(defun +noema--standalone-slide-file ()
  "Return the Markdown file shown by the current standalone Slide view."
  (when (or (eq major-mode 'portal-web-mode)
            (and (eq major-mode 'xwidget-webkit-mode)
                 (fboundp '+xwidget-current-url)))
    (when-let* ((url (if (eq major-mode 'portal-web-mode)
                         portal-web--url
                       (+xwidget-current-url)))
                (path (url-filename (url-generic-parse-url url))))
      (when (and (string-match-p "\\`/slides\\(?:[?#]\\|\\'\\)" path)
                 (string-match "[?&]file=\\([^&#]+\\)" path))
        (let ((file (url-unhex-string (match-string 1 path))))
          (and (+noema--markdown-file-p file) file))))))

;;;###autoload
(defun +noema/back ()
  "Return to the previous location in this Noema pane."
  (interactive)
  ;; A standalone /slides page starts in a fresh WebKit window and has no
  ;; editor entry in browser or Noema history.  Its header is Emacs chrome,
  ;; so return to the source editor here instead of broadcasting a page event.
  (if-let* ((file (+noema--standalone-slide-file)))
      (+noema/open-file file)
    (+noema/command "back")))

;;;###autoload
(defun +noema/forward ()
  "Move to the next location in this Noema pane."
  (interactive)
  (+noema/command "forward"))

;;;###autoload
(defun +noema/escape ()
  "Tell Noema to handle Escape."
  (interactive)
  (+noema/command "escape"))

;;;###autoload
(defun +noema/save ()
  "Tell Noema to save the current note."
  (interactive)
  (+noema/command "save"))

;;;###autoload
(defun +noema/focus ()
  "Tell Noema to focus its editor."
  (interactive)
  (+noema/command "focus"))

;;;###autoload
(defun +noema/task-manager ()
  "Open the current Noema page's task manager on either web backend."
  (interactive)
  (+noema/command "task-manager"))

(defun +noema-open-wiki-view (&optional view query)
  "Open the Emacs-hosted Wiki VIEW with optional QUERY parameters."
  (let ((path (concat "/wiki"
                      (when (or view query)
                        (concat "?"
                                (mapconcat
                                 #'identity
                                 (delq nil
                                       (list (and view (format "view=%s" view))
                                             query))
                                 "&"))))))
    (if (or (noema--xwidget-buffer-p (current-buffer))
            (+noema-portal-buffer-p (current-buffer)))
        (+noema--open-hosted-surface
         `((path . ,path) (client . ,+noema--client-id)))
      (+noema--ensure-server
       (lambda ()
         (+noema--open-url (+noema--server-url path) nil nil))))))

(defmacro +noema--def-wiki-view (name view doc)
  "Define NAME to open the Emacs-hosted Wiki VIEW with DOC."
  `(defun ,name ()
     ,doc
     (interactive)
     (+noema-open-wiki-view ,view)))

(+noema--def-wiki-view +noema/wiki-home nil "Open the Noema Wiki home.")
(defalias 'noema-wiki-home #'+noema/wiki-home
  "Open the Noema Wiki home.")
(+noema--def-wiki-view +noema/wiki-pages "pages" "Open the Wiki page browser.")
(+noema--def-wiki-view +noema/wiki-recent "recent" "Open recently changed Wiki pages.")
(+noema--def-wiki-view +noema/wiki-tags "tags" "Open Wiki tag management.")
(+noema--def-wiki-view +noema/wiki-namespaces "namespaces" "Open Wiki namespaces.")
(+noema--def-wiki-view +noema/wiki-repositories "repositories" "Open Wiki repository management.")
(+noema--def-wiki-view +noema/wiki-sync "sync" "Open Wiki synchronization.")
(+noema--def-wiki-view +noema/wiki-reports "reports" "Open Wiki reports.")
(+noema--def-wiki-view +noema/wiki-wanted "wanted" "Open Wiki wanted-pages report.")

(defun +noema/wiki-new-page ()
  "Open Noema's Wiki new-page flow inside Emacs."
  (interactive)
  (+noema-open-wiki-view nil "new=1"))

;;;###autoload
(defun +noema/workspace-graph ()
  "Open the Emacs-hosted interactive workspace graph."
  (interactive)
  (+noema-open-wiki-view "graph"))

;;;###autoload
(defun +noema-roam/graph ()
  "Open Noema's Emacs-hosted workspace graph."
  (interactive)
  (+noema/workspace-graph))

;;; Pause/resume — report host activity to the shared renderer gate.

(defvar +noema--paused nil
  "Non-nil when the browser page has been sent a pause command.")
(defvar +noema--manual-paused nil
  "Non-nil when Noema was paused explicitly by the user.")
(defvar +noema--activity-timer nil
  "Debounce timer for `+noema--update-activity'.")
(defvar +noema--activity-hooks-installed nil
  "Non-nil when Noema pause/resume activity hooks are installed.")
(defvar +noema--last-activity-signature :unknown
  "Last per-client activity snapshot scheduled by Noema.")
(defvar +noema--input-focused-window nil
  "Noema pane that proved WebKit owns input without Emacs selecting it.")
(defvar +noema--input-focus-host-window nil
  "Emacs selected window when `+noema--input-focused-window' took input.")
(defvar-local +noema--activity-paused :unknown
  "Last pause state sent to this Noema renderer client.")
(put '+noema--activity-paused 'permanent-local t)

(defconst +noema--core-ready-script
  "(() => {
  const connection = window.noemaApi && window.noemaApi.connection;
  if (!connection || typeof connection.reconnect !== 'function') return false;
  connection.reconnect('host-ready');
  return true;
})()"
  "JavaScript used to reconnect a retained xwidget page after core restarts.")

(defun +noema--browser-buffers ()
  "Return registered live Noema browser buffers without scanning all buffers."
  (let (buffers)
    (when (buffer-live-p +noema--app-buffer)
      (push +noema--app-buffer buffers))
    (maphash
     (lambda (_client buffer)
       (when (buffer-live-p buffer) (push buffer buffers)))
     +noema--client-buffers)
    (maphash
     (lambda (_file buffer)
       (when (buffer-live-p buffer) (push buffer buffers)))
     +noema--file-buffers)
    (delete-dups buffers)))

(defun +noema--buffer-active-p (buffer)
  "Return non-nil when BUFFER owns the input window of a focused frame.
A visible but unselected Noema split remains painted and readable, but its
shared renderer activity gate may sleep until that pane receives input."
  (when (buffer-live-p buffer)
    (cl-some
     (lambda (window)
       (let ((frame (window-frame window)))
         (and (eq (frame-visible-p frame) t)
              (frame-focus-state frame)
              (eq window (if (window-live-p +noema--input-focused-window)
                             +noema--input-focused-window
                           (frame-selected-window frame))))))
     (get-buffer-window-list buffer nil 'visible))))

(defun +noema--forget-input-focus ()
  "Drop the WebKit input fact when Emacs handles a command or changes panes."
  (setq +noema--input-focused-window nil
        +noema--input-focus-host-window nil)
  (remove-hook 'pre-command-hook #'+noema--forget-input-focus-on-command))

(defun +noema--forget-input-focus-on-command ()
  "Return Noema activity ownership to Emacs at the next host command."
  (+noema--forget-input-focus)
  (setq +noema--last-activity-signature :unknown)
  (+noema--update-activity))

(defun +noema--app-buffer-visible-p ()
  "Return non-nil when the current Noema buffer is the active host pane."
  (+noema--buffer-active-p +noema--app-buffer))

(defun +noema--activity-snapshot ()
  "Return a stable list of each live renderer buffer and its host activity."
  (sort
   (mapcar (lambda (buffer)
             (cons buffer (and (+noema--buffer-active-p buffer) t)))
           (+noema--browser-buffers))
   (lambda (left right)
     (string-lessp (buffer-name (car left)) (buffer-name (car right))))))

(defun +noema--notify-xwidgets-core-ready ()
  "Reconnect retained Noema pages and replay host lifecycle after restart."
  (dolist (buffer (+noema--browser-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local +noema--activity-paused :unknown)
        (condition-case nil
            (cond
             ((+noema-portal-buffer-p buffer)
              (+noema-portal-send buffer '(:type "core-ready")))
             ((and (noema--xwidget-buffer-p buffer)
                   (fboundp 'xwidget-webkit-current-session)
                   (fboundp 'xwidget-webkit-execute-script))
              (when-let* ((session (xwidget-webkit-current-session)))
                (xwidget-webkit-execute-script
                 session +noema--core-ready-script))))
          (quit
           (+noema--record-interrupted-operation "retained Noema core reconnect"))
          (error nil)))))
  (when +noema--activity-hooks-installed
    (setq +noema--last-activity-signature :unknown)
    (+noema--update-activity)))

(defun +noema--maybe-reconnect-core-on-activity ()
  "Restart a disconnected core only while an Noema browser is active.
This is intentionally called from focus/window activity, never from an idle
timer or retry loop. The old port is reclaimed so the browser page and its
unsaved CodeMirror state remain intact."
  (when (and (buffer-live-p +noema--app-buffer)
             (+noema--app-buffer-visible-p)
             (not +noema--ready)
             (not (and (processp +noema--process)
                       (process-live-p +noema--process)))
             (integerp +noema--last-port)
             (> +noema--last-port 0))
    (unless (memq #'+noema--notify-xwidgets-core-ready
                  +noema--ready-callbacks)
      (push #'+noema--notify-xwidgets-core-ready
            +noema--ready-callbacks))
    (condition-case err
        (progn
          (+noema--start-server +noema--last-port)
          ;; Diagnostic deadline only; it never performs another reconnect.
          (when +noema--ready-watchdog
            (cancel-timer +noema--ready-watchdog))
          (setq +noema--ready-watchdog
                (run-at-time 10 nil #'+noema--watchdog-fire)))
      (error
       (setq +noema--ready-callbacks
             (delq #'+noema--notify-xwidgets-core-ready
                   +noema--ready-callbacks))
       (message "Noema: active core reconnect failed: %s"
                (error-message-string err))))))

(defun +noema--apply-activity-snapshot (snapshot)
  "Apply per-client pause states from SNAPSHOT through shared host commands."
  (let ((all-paused (and snapshot t)))
    (dolist (entry snapshot)
      (pcase-let ((`(,buffer . ,active) entry))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let* ((effective-active
                    (and active (not +noema--manual-paused)))
                   (next-paused (not effective-active)))
              (unless (eq next-paused +noema--activity-paused)
                (setq-local +noema--activity-paused next-paused)
                ;; The renderer owns the shared pause state machine.  This
                ;; adapter contributes only its host-window activity fact.
                (+noema--send-command
                 (if effective-active "resume" "pause")))
              (unless next-paused
                (setq all-paused nil)))))))
    (setq +noema--paused all-paused)))

(defun +noema--apply-activity (active)
  "Apply ACTIVE to the current Noema renderer client.
This narrow wrapper is retained for interactive callers and tests; automatic
activity always applies a complete per-client snapshot."
  (let ((buffer (if (memq (current-buffer) (+noema--browser-buffers))
                    (current-buffer)
                  +noema--app-buffer)))
    (when (buffer-live-p buffer)
      (+noema--apply-activity-snapshot (list (cons buffer active))))))

;;;###autoload
(defun +noema/pause ()
  "Pause shared Noema renderer work until explicitly resumed."
  (interactive)
  (setq +noema--manual-paused t)
  (+noema--apply-activity-snapshot (+noema--activity-snapshot)))

;;;###autoload
(defun +noema/resume ()
  "Resume only the Noema renderer client in the active host pane."
  (interactive)
  (setq +noema--manual-paused nil)
  (+noema--apply-activity-snapshot (+noema--activity-snapshot)))

;;;###autoload
(defun +noema/toggle-pause ()
  "Toggle manual pause for Noema's shared renderer activity gate."
  (interactive)
  (if +noema--manual-paused
      (+noema/resume)
    (+noema/pause)))

;;; Undisplayed pages (docs/design/window-lifecycle.md, Q3)

(defvar +noema-detached-page-limit 8
  "How many Noema pages may stay alive while shown in no window.
Closing a page's view keeps it, with its editing state, for switching
back; beyond this many the least recently shown are ended.  Pages save
themselves as they are edited, so ending one loses no text.  nil keeps
every page.")

(config-register '+noema-detached-page-limit
  :type '(choice (const :tag "Keep all" nil) natnum)
  :group 'noema
  :doc "Undisplayed Noema pages kept alive before the oldest are ended.")

(defun +noema--detached-pages ()
  "Return live Noema pages shown in no window, most recently used first.
The shared app buffer is never included."
  (let ((pages (+noema--browser-buffers)))
    (seq-filter (lambda (buffer)
                  (and (memq buffer pages)
                       (not (eq buffer +noema--app-buffer))
                       (not (get-buffer-window buffer t))))
                (buffer-list))))

(defun +noema--trim-detached-pages ()
  "End the undisplayed pages beyond `+noema-detached-page-limit'."
  (when (natnump +noema-detached-page-limit)
    (let ((kill-buffer-query-functions nil))
      (dolist (buffer (nthcdr +noema-detached-page-limit
                              (+noema--detached-pages)))
        (kill-buffer buffer)))))

(defun +noema--update-activity (&rest _)
  "Coalesce host focus/window changes into per-client pause commands.
No polling timer is installed: Emacs reports only shell-level activity facts,
and every renderer uses the same `runHostCommand' pause implementation."
  (when (and +noema--input-focused-window
             (or (not (window-live-p +noema--input-focused-window))
                 (not (eq (selected-window) +noema--input-focus-host-window))))
    (+noema--forget-input-focus))
  (let ((selected (and (window-live-p (selected-window))
                       (window-buffer (selected-window)))))
    (when (memq selected (+noema--browser-buffers))
      (setq +noema--app-buffer selected)))
  (+noema--maybe-reconnect-core-on-activity)
  (let ((snapshot (+noema--activity-snapshot)))
    (unless (equal snapshot +noema--last-activity-signature)
      (setq +noema--last-activity-signature snapshot)
      (when +noema--activity-timer
        (cancel-timer +noema--activity-timer))
      ;; Pause quickly when a pane leaves the foreground; allow the focus
      ;; hand-off to settle briefly before resuming WebKit work.
      (setq +noema--activity-timer
            (run-at-time
             (if (seq-some #'cdr snapshot) 0.12 0.03) nil
             (lambda ()
               (setq +noema--activity-timer nil)
               (if +noema--ready
                   (let ((current (+noema--activity-snapshot)))
                     (setq +noema--last-activity-signature current)
                     (+noema--apply-activity-snapshot current)
                     (+noema--trim-detached-pages))
                 ;; A ready transition must retry even if focus did not change.
                 (setq +noema--last-activity-signature :unknown))))))))

(defun +noema--install-activity-hooks ()
  "Install event-driven host activity hooks without a background poller."
  (unless +noema--activity-hooks-installed
    (add-function :after after-focus-change-function
                  #'+noema--update-activity)
    (add-hook 'focus-in-hook #'+noema--update-activity)
    (add-hook 'focus-out-hook #'+noema--update-activity)
    (add-hook 'window-buffer-change-functions #'+noema--update-activity)
    (add-hook 'window-selection-change-functions #'+noema--update-activity)
    (when (boundp 'window-state-change-functions)
      (add-hook 'window-state-change-functions #'+noema--update-activity))
    (when (boundp 'delete-frame-functions)
      (add-hook 'delete-frame-functions #'+noema--update-activity))
    (setq +noema--activity-hooks-installed t)))

(defun +noema--remove-activity-hooks ()
  "Remove activity hooks and cancel the one pending debounce transition."
  (+noema--forget-input-focus)
  (remove-function after-focus-change-function #'+noema--update-activity)
  (remove-hook 'focus-in-hook #'+noema--update-activity)
  (remove-hook 'focus-out-hook #'+noema--update-activity)
  (remove-hook 'window-buffer-change-functions #'+noema--update-activity)
  (remove-hook 'window-selection-change-functions #'+noema--update-activity)
  (when (boundp 'window-state-change-functions)
    (remove-hook 'window-state-change-functions #'+noema--update-activity))
  (when (boundp 'delete-frame-functions)
    (remove-hook 'delete-frame-functions #'+noema--update-activity))
  (when +noema--activity-timer
    (cancel-timer +noema--activity-timer)
    (setq +noema--activity-timer nil))
  (dolist (buffer (+noema--browser-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local +noema--activity-paused :unknown))))
  (setq +noema--paused nil
        +noema--manual-paused nil
        +noema--last-activity-signature :unknown
        +noema--activity-hooks-installed nil))

;;;###autoload
(defun +noema/agenda-apple-enable (kind)
  "Enable the client EventKit helper and explicitly request access for KIND."
  (interactive (list (completing-read "Enable Apple: " '("reminder" "event") nil t)))
  (unless (member kind '("reminder" "event")) (user-error "Choose reminder or event"))
  (require 'noema/bridge/agenda-apple)
  (+noema--apple-start)
  (+noema--apple-request
   `((op . "authorize") (kind . ,kind))
   (lambda (result error-object)
     (if error-object (message "Noema Apple: %s" (alist-get 'message error-object))
       (message "Noema Apple %s access: %s" kind
                (if (eq (alist-get 'granted result) t) "enabled" "not granted")))) 180))

(defun +noema/agenda-apple-disable ()
  "Stop the client EventKit helper without changing Apple or source items."
  (interactive)
  (when (fboundp '+noema--apple-stop) (+noema--apple-stop))
  (message "Noema Apple integration disabled"))

(defvar +noema--attention-visit-request 0)
(defun +noema--agenda-attention-visit (reference)
  "Explicitly activate and open the source of a global attention REFERENCE."
  (require 'tools/project/config)
  (require 'noema-agenda)
  (let ((request (cl-incf +noema--attention-visit-request)))
    (+project-activate (alist-get 'root reference) t)
    (noema-agenda-activate-project
     +project-active-root
     (lambda (scope problem)
       (when (= request +noema--attention-visit-request)
         (if problem (noema-agenda--error problem)
           (let ((scope-id (noema-agenda--get scope 'id))
                 (project-generation noema-agenda--project-request))
             (noema-agenda--call
              "query" `((scopes . ,(vector scope-id)))
              (lambda (snapshot error-object)
                (when (and (= request +noema--attention-visit-request)
                           (= project-generation noema-agenda--project-request)
                           (equal scope-id noema-agenda--project-scope))
                  (if error-object (noema-agenda--error error-object)
                    (let ((matches
                           (seq-filter
                            (lambda (todo)
                              (and (equal (noema-agenda--get todo 'uid) (alist-get 'uid reference))
                                   (equal (noema-agenda--get todo 'file) (alist-get 'file reference))))
                            (noema-agenda--list (noema-agenda--get snapshot 'todos)))))
                      (if (= (length matches) 1)
                          (noema-agenda-visit-record (car matches))
                        (noema-agenda--error "Bound task moved, disappeared or became ambiguous; refresh its source"))))))))))))))

(defun +noema/agenda-attention ()
  "Open the native global attention view over explicitly promoted bindings."
  (interactive)
  (require 'noema-agenda-attention)
  (noema-agenda-attention))

;;;###autoload
(defun +noema/stop ()
  "Kill the Noema web-host process."
  (interactive)
  (+noema--latex-agent-cancel-all)
  (when (fboundp '+dashboard-agenda-host-stopped)
    (+dashboard-agenda-host-stopped))
  (when (fboundp 'noema-agenda-host-stopped)
    (noema-agenda-host-stopped))
  (+noema--clear-post-queue)
  (+noema--clear-process-log-queue)
  (+noema--remove-activity-hooks)
  (when (fboundp '+noema-roam--cancel-sync-timer)
    (+noema-roam--cancel-sync-timer))
  (when +noema--ready-watchdog
    (cancel-timer +noema--ready-watchdog)
    (setq +noema--ready-watchdog nil))
  (when +noema--goto-timer
    (cancel-timer +noema--goto-timer)
    (setq +noema--goto-timer nil
          +noema--goto-last nil))
  (+noema--clear-external-file-watches)
  (when +noema--gateway-binding
    (remote-gateway-release-binding
     +noema--gateway-binding t)
    (setq +noema--gateway-binding nil))
  (let ((proc +noema--process))
    (setq +noema--process nil
          +noema--port nil
          +noema--ready nil
          +noema--ready-callbacks nil)
    (when (and proc (process-live-p proc))
      (ignore-errors (signal-process proc 'SIGTERM))
      (run-at-time 1.5 nil
        (lambda ()
          (when (process-live-p proc)
            (delete-process proc))))))
  (message "Noema web-host stopped."))

(defun +noema--kill-browser-buffers ()
  "Kill Emacs buffers that host Noema browser pages."
  (mapc
   (lambda (buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (or +noema-buffer-file-name
                   +noema--client-id
                   (and (memq major-mode '(xwidget-webkit-mode portal-web-mode))
                        (string-prefix-p "*Noema" (buffer-name buffer))))
           (kill-buffer buffer)))))
   (buffer-list))
  (setq +noema--app-buffer nil)
  (clrhash +noema--file-buffers)
  (clrhash +noema--client-buffers))

;;;###autoload
(defun +noema/close ()
  "Completely close Noema browser surfaces and stop the web-host."
  (interactive)
  (+noema--kill-browser-buffers)
  (+noema/stop))

;;;###autoload
(defun +noema/build-and-reopen ()
  "Build Noema web assets, restart the runtime, and reopen the current note."
  (interactive)
  (when (and +noema--build-process
             (process-live-p +noema--build-process))
    (user-error "Noema build is already running"))
  (let* ((file (+noema--current-note-file))
         (buffer (get-buffer-create "*Noema build*"))
         (default-directory user-emacs-directory))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Noema: building web assets...")
    (setq +noema--build-process
          (make-process
           :name "noema-web-build"
           :buffer buffer
           :command '("make" "noema-web-build")
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((ok (= (process-exit-status proc) 0)))
                 (setq +noema--build-process nil)
                 (if ok
                     (progn
                       (+noema/close)
                       (message "Noema: build finished; reopening...")
                       (if (and file (+noema--markdown-file-p file))
                           (+noema/open-file file)
                         (+noema--ensure-server
                          (lambda ()
                            (+noema--open-url
                             (+noema--app-url nil "noema:surface") nil t)))))
                   (display-buffer buffer)
                   (message "Noema: build failed; see %s" (buffer-name buffer))))))))
    (display-buffer buffer)))

(add-hook 'kill-emacs-hook #'+noema/stop)

;;; API call — request the web-host over the shared gateway.

(defun +noema--agenda-host-project-root (root)
  "Resolve ROOT for the client-hosted Agenda through Remote placement."
  (require 'remote-fs)
  (let ((logical (remote-canonicalize-emacs-file-name root)))
    (or (remote-client-file-name logical) logical)))

(with-eval-after-load 'noema-agenda
  (setq noema-agenda-project-root-function #'+noema--agenda-host-project-root))

(defun +noema/agenda (&optional query)
  "Open native Noema Agenda for the knowledge vault and current project."
  (interactive)
  (require 'noema-agenda)
  (unless (derived-mode-p 'noema-agenda-mode)
    (let ((root (when (fboundp '+project-current-root) (+project-current-root))))
      (when (fboundp '+project-activate) (+project-activate root t))
      ;; The module can be loaded after the project's original entry event.
      (noema-agenda-activate-project
       (if (boundp '+project-active-root) +project-active-root root))))
  (noema-agenda-open query))

(defalias 'noema-agenda #'+noema/agenda
  "Open the native Noema Agenda for the knowledge vault and current project.")

(defun +noema/agenda-plan-node (scheduled deadline)
  "Include the current DAG node in Agenda with SCHEDULED and DEADLINE dates."
  (interactive (list (read-string "Schedule (YYYY-MM-DD [HH:MM], optional): ")
                     (read-string "Deadline (YYYY-MM-DD [HH:MM], optional): ")))
  (require 'noema-api)
  (let ((patch (make-hash-table :test #'equal)))
    (unless (string-empty-p scheduled) (puthash "sche" scheduled patch))
    (unless (string-empty-p deadline) (puthash "ddl" deadline patch))
    (noema-set-node-agenda (noema-current-node) patch)))

(defun +noema--gateway-hash-value (value)
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
         (+noema--gateway-hash-value (cdr item))
         table))))
   ((listp value)
    (mapcar #'+noema--gateway-hash-value value))
   ((vectorp value)
    (vconcat
     (mapcar #'+noema--gateway-hash-value value)))
   (t value)))

(defun +noema--api-call-sync (channel args &optional timeout)
  "Call CHANNEL with ARGS synchronously; return parsed JSON or nil.
Only usable when the web-host is running (`+noema--ready' is non-nil).
Blocks the caller until the response arrives or TIMEOUT seconds elapse
\(8 by default).  Interactive callers that run on a keystroke — completion
at point, for instance — should pass a much shorter TIMEOUT so a busy
kernel cannot freeze the editor."
  (when-let* ((client
               (and +noema--ready
                    (remote-gateway-find-client "noema")))
              (result
               (remote-gateway-request-sync
                client "noema.api"
                `((channel . ,channel) (args . ,args))
                (or timeout 8))))
    (+noema--gateway-hash-value result)))

(defcustom +noema-api-call-timeout 10
  "Seconds to wait for an ordinary asynchronous Noema API reply."
  :type 'number
  :group '+noema)

(defun +noema-api-call (channel args callback &optional timeout)
  "Call CHANNEL with ARGS and invoke CALLBACK with (RESULT ERROR).
Unlike the compatibility wrapper `+noema--api-call', CALLBACK is also
invoked when Noema is offline or the gateway request fails.

TIMEOUT overrides `+noema-api-call-timeout'.  Callers whose channel runs
user code — Noema executes a whole run-all before it answers — must pass one,
because the default deadline would report a perfectly healthy run as a
failure while it is still going."
  (if-let* ((client
             (and +noema--ready
                  (remote-gateway-find-client "noema"))))
      (remote-gateway-request-async
       client "noema.api"
       `((channel . ,channel) (args . ,args))
       (lambda (result error-object)
         (if error-object
             (funcall callback nil error-object)
           (funcall callback result nil)))
       (or timeout +noema-api-call-timeout))
    (run-at-time
     0 nil callback nil
     '((code . "offline") (message . "Noema web-host is not ready")))))

(defun +noema--api-call (channel args callback)
  "Compatibility wrapper that invokes CALLBACK only after a successful call."
  (+noema-api-call
   channel args
   (lambda (result error-object)
     (if error-object
         (message
          "Noema API error %s: %s"
          (or (+noema-jupyter--get 'code error-object) "unknown")
          (or (+noema-jupyter--get 'message error-object) "request failed"))
       (funcall callback result)))))

(defun +noema--activity-client-status ()
  "Return per-renderer host activity diagnostics for runtime status."
  (vconcat
   (mapcar
    (lambda (entry)
      (pcase-let ((`(,buffer . ,active) entry))
        (with-current-buffer buffer
          (let ((status (make-hash-table :test 'equal)))
            (puthash "buffer" (buffer-name buffer) status)
            (puthash "client" +noema--client-id status)
            (puthash "file" +noema-buffer-file-name status)
            (puthash "active" (if active t :false) status)
            (puthash "rendererPaused"
                     (cond
                      ((eq +noema--activity-paused :unknown) "unknown")
                      (+noema--activity-paused t)
                      (t :false))
                     status)
            status))))
    (+noema--activity-snapshot))))

(defun +noema/runtime-status ()
  "Display the Noema runtime debug snapshot."
  (interactive)
  (unless +noema--ready
    (user-error "Noema web-host is not ready"))
  (message "Noema: loading runtime status…")
  (+noema-api-call
   "noema:api:runtime:debug" []
   (lambda (result error-object)
     (if error-object
         (message "Noema runtime status unavailable: %s"
                  (or (+noema-jupyter--get 'message error-object)
                      "request failed"))
       (let ((payload (+noema--gateway-hash-value result)))
         (puthash "emacsActivity"
                  (let ((activity (make-hash-table :test 'equal)))
                    (puthash "paused" (if +noema--paused t :false) activity)
                    (puthash "manualPaused" (if +noema--manual-paused t :false) activity)
                    (puthash "bufferVisible" (if (+noema--app-buffer-visible-p) t :false) activity)
                    (puthash "clients" (+noema--activity-client-status) activity)
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

(defun +noema/wiki-refresh (&optional full)
  "Refresh wiki.db incrementally, or perform a FULL atomic rebuild."
  (interactive)
  (unless +noema--ready
    (user-error "Noema: server not running"))
  (let ((mode (if full "full" "incremental")))
    (message "Noema: %s Wiki index..." (if full "rebuilding" "refreshing"))
    (+noema--api-call
     "noema:api:wiki:refresh"
     (vector (list (cons 'mode mode)))
     (lambda (result)
       (let* ((maintenance (alist-get 'maintenance result))
              (actual-mode (or (alist-get 'mode maintenance) mode))
              (notes (length (or (alist-get 'notes result) nil)))
              (generation (or (alist-get 'generation result) "")))
         (setq +noema--last-sync-stats
               (format "wiki %s · %d pages · %s"
                       actual-mode notes
                       (if (> (length generation) 8) (substring generation 0 8) generation)))
         (when (fboundp '+noema-roam--clear-runtime-cache)
           (+noema-roam--clear-runtime-cache))
         (message "Noema Wiki index: %s" +noema--last-sync-stats))))))

(defun +noema/wiki-rebuild ()
  "Atomically rebuild the canonical wiki.db from all repositories."
  (interactive)
  (+noema/wiki-refresh t))

(defun +noema/wiki-index-status ()
  "Display canonical wiki.db maintenance status."
  (interactive)
  (unless +noema--ready
    (user-error "Noema: server not running"))
  (message "Noema: loading Wiki index status…")
  (+noema-api-call
   "noema:api:wiki:index-status" []
   (lambda (result error-object)
     (if error-object
         (message "Noema Wiki index status unavailable: %s"
                  (or (+noema-jupyter--get 'message error-object)
                      "request failed"))
       (let ((payload (+noema--gateway-hash-value result)))
         (setq +noema--last-sync-stats
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
(defalias '+noema-roam/sync #'+noema/wiki-refresh)

;;; Header-line for the Noema app buffer.

(defun +noema--undisplayed-buffers ()
  "Return live Noema page buffers absent from every Emacs window."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (or (and (eq major-mode 'xwidget-webkit-mode)
                     (noema--xwidget-buffer-p buffer))
                (+noema-portal-buffer-p buffer)))
          (not (get-buffer-window buffer t))))
   (buffer-list)))

(defun +noema--switch-to-buffer (buffer)
  "Show undisplayed Noema BUFFER in the selected window."
  (when (and (buffer-live-p buffer)
             (not (get-buffer-window buffer t)))
    (switch-to-buffer buffer)
    (setq +noema--app-buffer buffer)
    (if (+noema-portal-buffer-p buffer)
        (+noema-portal-focus buffer)
      (run-at-time 0.05 nil #'noema--focus-xwidget-buffer buffer))))

(defun +noema/switch-buffer-menu (event)
  "Choose an undisplayed Noema buffer from a header-line button."
  (interactive "e")
  (when event (+xwidget--select-event-window event))
  (let ((buffers (+noema--undisplayed-buffers)))
    (if buffers
        (popup-menu
         (easy-menu-create-menu
          "Noema buffers"
          (mapcar
           (lambda (buffer)
             (vector (buffer-name buffer)
                     (lambda ()
                       (interactive)
                       (+noema--switch-to-buffer buffer))
                     t))
           buffers))
         event)
      (message "No undisplayed Noema buffers"))))

(defun +noema/editor-menu (event)
  "Open Noema editor actions from the native pencil button at EVENT."
  (interactive "e")
  (+xwidget--select-event-window event)
  (popup-menu
   (easy-menu-create-menu
    "Noema"
    (list
     ["Focus editor" +noema/focus t]
     ["Task manager" +noema/task-manager t]
     "---"
     ["Page outline" +noema/toggle-page t]
     ["Agenda" +noema/toggle-agenda t]
     ["Local graph" +noema/toggle-graph t]
     ["Workspace graph" +noema/workspace-graph t]
     ["Wiki home" +noema/wiki-home t]
     ["Tools" +noema/toggle-tools t]
     ["Jupyter cells" +noema/toggle-jupyter t]
     "---"
     ["Toggle source" +noema/toggle-source t]
     ["Run prose check" +noema/prose-check t]
     ["Export LaTeX…" +noema/export-latex t]
     ["Settings…" +noema/settings t]
     ["Move document to Trash" noema-trash-note t]
     ["Save" +noema/save t]))
   event))

(dolist (entry '((+noema/toggle-page . "toggle-toc")
                 (+noema/toggle-agenda . "toggle-agenda")
                 (+noema/toggle-graph . "toggle-graph")
                 (+noema/toggle-tools . "toggle-tools")
                 (+noema/toggle-jupyter . "jupyter-panel")))
  (let ((fn (car entry)) (command (cdr entry)))
    (fset fn (lambda () (interactive) (+noema/command command)))))

(defun +noema--header-browser-buttons (&optional reload window-menu)
  "Return shared browser controls for Noema's xwidget or Portal view.
RELOAD and WINDOW-MENU select the backend's implementations."
  (append
   (+xwidget--header-history-and-popup
    #'+noema/back #'+noema/forward #'+noema/switch-buffer-menu)
   (list
    (+xwidget--nav-button
     (+xwidget--mode-line-icon 'codicon "nf-cod-refresh" "reload")
     (or reload #'+xwidget/reload)
     (if reload "Refresh note from disk" "Reload page [g]") 'header-line)
    (+xwidget--nav-button
     (+xwidget--mode-line-icon 'codicon "nf-cod-edit" "edit")
     #'+noema/editor-menu "Noema actions" 'header-line)
    (+xwidget--nav-button
     (+xwidget--mode-line-icon 'codicon "nf-cod-layout" "win")
     (or window-menu #'+xwidget/window-menu)
     "Window menu" 'header-line))))

(defun +noema--header-line ()
  "Return native browser and editor controls for an Noema buffer."
  (let* ((file (+noema-buffer-file (current-buffer)))
         (name (if file (file-name-nondirectory file) "Noema")))
    (append
     (list " ")
     (+noema--header-browser-buttons)
     (list "  " (propertize name 'face 'mode-line-buffer-id)))))

(defun +noema--setup-native-chrome ()
  "Install Noema-only Emacs chrome in the current xwidget buffer."
  (setq-local header-line-format '(:eval (+noema--header-line)))
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update t))

(defun +noema/portal-menu ()
  "Show the Noema page actions in a Portal pane."
  (interactive)
  (popup-menu
   (easy-menu-create-menu
    "Noema"
    (list
     ["Focus editor" +noema/focus t]
     ["Save" +noema/save t]
     ["Back" +noema/back t]
     ["Forward" +noema/forward t]
     ["Refresh pane" +noema/refresh t]
     ["Task manager" +noema/task-manager t]
     "---"
     ["Editable split right" +noema/open-current-note-split-right t]
     ["Editable split below" +noema/open-current-note-split-below t]
     ["Editable split in new frame" +noema/open-current-note-split-frame t]
     ["Page outline" +noema/toggle-page t]
     ["Agenda" +noema/toggle-agenda t]
     ["Local graph" +noema/toggle-graph t]
     ["Workspace graph" +noema/workspace-graph t]
     ["Wiki home" +noema/wiki-home t]
     ["Tools" +noema/toggle-tools t]
     ["Jupyter cells" +noema/toggle-jupyter t]
     "---"
     ["Toggle source" +noema/toggle-source t]
     ["Run prose check" +noema/prose-check t]
     ["Export LaTeX…" +noema/export-latex t]
     ["Settings" +noema/settings t]
     ["Move document to Trash" noema-trash-note t]
     ["Close Noema" +noema/close t]))))

(defun +noema--portal-header-line ()
  "Return the shared Noema browser controls for a Portal page."
  (let ((file (+noema-buffer-file)))
    (append
     (list " ")
     (+noema--header-browser-buttons
      #'+noema/refresh #'+noema/portal-menu)
     (list "  "
           (propertize (if file (file-name-nondirectory file) "Noema")
                       'face 'mode-line-buffer-id)))))

(defun +noema--setup-portal-chrome ()
  "Install native Noema controls in the current Portal page."
  (setq-local header-line-format '(:eval (+noema--portal-header-line)))
  (force-mode-line-update t))

(defun +noema--restore-native-chrome-h ()
  "Restore Noema chrome after generic xwidget mode initialization."
  (when (noema--xwidget-buffer-p)
    (+noema--setup-native-chrome)))

(defun +noema--restore-native-chrome-later-h ()
  "Restore Noema chrome after all xwidget mode hooks have settled."
  (let ((buffer (current-buffer)))
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (+noema--restore-native-chrome-h)))))))

(with-eval-after-load 'app/browser/config
  (advice-add '+xwidget-setup-control-line :after
              #'+noema--restore-native-chrome-h)
  (with-eval-after-load 'xwidget
    (add-hook 'xwidget-webkit-mode-hook
              #'+noema--restore-native-chrome-later-h 90)))

(defun +noema--xwidget-menu-section ()
  "Return Noema actions for the xwidget top-bar popup."
  (when (or +noema-buffer-file-name
            +noema--client-id)
    (list
     "---"
     ["Noema: Back" +noema/back t]
     ["Noema: Forward" +noema/forward t]
     ["Noema: Refresh current pane" +noema/refresh t]
     ["Noema: Open editable split" +noema/open-current-note-split t]
     ["Noema: Split right" +noema/open-current-note-split-right t]
     ["Noema: Split below" +noema/open-current-note-split-below t]
     ["Noema: Split to new frame" +noema/open-current-note-split-frame t]
     ["Noema: Wiki home" +noema/wiki-home t]
     ["Noema: Workspace graph" +noema/workspace-graph t]
     ["Noema: Focus editor" +noema/focus t]
     ["Noema: Pop" noema-menu t]
     (list
      "Noema lifecycle"
      ["Build + reopen" +noema/build-and-reopen t]
      ["Close all Noema" +noema/close t]))))

(with-eval-after-load 'app/browser/config
  (add-to-list '+xwidget-window-menu-extra-sections
               #'+noema--xwidget-menu-section))

;;; Web-editor command wrappers.
;; These generate named interactive commands for every web-host editor command
;; so each entry in the dispatch hub is `commandp', appears in M-x, and can
;; be verified with `commandp' in batch tests.

(defmacro +noema--def-editor-cmd (suffix command &optional doc)
  "Define `+noema/SUFFIX' that sends editor COMMAND to the web page."
  `(defun ,(intern (format "+noema/%s" suffix)) ()
     ,(or doc (format "Send the Noema `%s' editor command." command))
     (interactive)
     (+noema/command ,command)))

(+noema--def-editor-cmd "toggle-source"   "toggle-source"   "Toggle source / rendered view.")
(+noema--def-editor-cmd "undo"            "undo"            "Undo last edit in Noema.")
(+noema--def-editor-cmd "redo"            "redo"            "Redo last undone edit in Noema.")
(+noema--def-editor-cmd "paste"           "paste"           "Paste through Noema's editor pipeline.")
(+noema--def-editor-cmd "bold"            "bold"            "Toggle bold at point.")
(+noema--def-editor-cmd "italic"          "italic"          "Toggle italic at point.")
(+noema--def-editor-cmd "code-inline"     "code"            "Toggle inline code at point.")
(+noema--def-editor-cmd "highlight"       "highlight"       "Toggle highlight at point.")
(+noema--def-editor-cmd "strike"          "strike"          "Toggle strikethrough at point.")
(+noema--def-editor-cmd "superscript"     "superscript"     "Wrap the selection as Markdown superscript.")
(+noema--def-editor-cmd "subscript"       "subscript"       "Wrap the selection as Markdown subscript.")
(+noema--def-editor-cmd "insert-footnote" "insert-footnote" "Insert a numbered Markdown footnote.")
(+noema--def-editor-cmd "insert-revision" "insert-revision" "Insert an Noema revision suggestion.")
(+noema--def-editor-cmd "edit-properties" "edit-properties" "Open the native org-meta properties panel.")
(+noema--def-editor-cmd "move-block-up"   "move-block-up"   "Move the current Markdown block upward.")
(+noema--def-editor-cmd "move-block-down" "move-block-down" "Move the current Markdown block downward.")
(+noema--def-editor-cmd "blockquote"      "blockquote"      "Toggle blockquote on paragraph.")
(+noema--def-editor-cmd "bullet-list"     "bullet-list"     "Toggle bullet list.")
(+noema--def-editor-cmd "ordered-list"    "ordered-list"    "Toggle ordered list.")
(+noema--def-editor-cmd "task-list"       "task-list"       "Toggle task/checkbox list.")
(+noema--def-editor-cmd "toggle-task"     "toggle-task"     "Check or uncheck the selected task items.")
(+noema--def-editor-cmd "inline-math"     "inline-math"     "Wrap the selection in inline math, or unwrap it.")
(+noema--def-editor-cmd "code-block"      "code-block"      "Insert/toggle fenced code block.")
(+noema--def-editor-cmd "paragraph-menu"  "paragraph-menu"  "Open heading/paragraph type menu.")
(+noema--def-editor-cmd "insert-table"    "insert-table"    "Insert a Markdown table.")
(+noema--def-editor-cmd "insert-math"     "insert-math-block" "Insert a math block.")
(+noema--def-editor-cmd "insert-toc"      "insert-toc"      "Insert a table of contents.")
(+noema--def-editor-cmd "prose-check"     "prose-check"     "Run a bounded LanguageTool check in Noema.")
(+noema--def-editor-cmd "knowledge-search" "knowledge-search" "Search notes through Noema's unified knowledge index.")
(+noema--def-editor-cmd "export-latex"    "export-latex"    "Export the current scope to LaTeX.")
(+noema--def-editor-cmd "settings"        "settings"        "Open Noema settings.")

;; Layout syntax and editor preferences are owned by Noema. Emacs sends the
;; same source-backed commands as the web editor and keeps no block defaults.
(defun +noema/insert-layout-grid ()
  "Wrap the selection in a two-column Noema layout grid."
  (interactive)
  (+noema/command "insert-layout" '((value . "grid"))))

(defun +noema/insert-layout-three-columns ()
  "Wrap the selection in a three-column Noema layout grid."
  (interactive)
  (+noema/command "insert-layout" '((value . "grid:3"))))

(defun +noema/insert-layout-flow ()
  "Wrap the selection in a flowing two-column Noema layout."
  (interactive)
  (+noema/command "insert-layout" '((value . "flow"))))

;;;###autoload
(defun noema-trash-note ()
  "Save and move the current Noema document to recoverable Trash."
  (interactive)
  (unless (and (boundp '+noema--client-id)
               (stringp +noema--client-id)
               (not (string-empty-p +noema--client-id)))
    (user-error "Select the Noema editor page to move its note to Trash"))
  (+noema/command "trash-current-note"))

;;; Dispatch transient.

(defun +noema--dispatch-header ()
  "Header string for the Noema dispatch transient."
  (let ((status (cond
                 ((not +noema--ready)
                  (propertize "offline" 'face 'error))
                 (t (propertize (format "port %d" +noema--port)
                                'face 'success))))
        (sync (or +noema--last-sync-stats "index status unknown")))
    (format "Noema  [%s]  %s" status sync)))

(with-eval-after-load 'transient
  (transient-define-prefix noema-menu ()
    "Noema Emacs workspace and CM6 knowledge-component commands."
    [:description +noema--dispatch-header
     ;; Row 1 ─────────────────────────────────────────────────────────────────
     ["Note (web)"
      ("o" "open current"     +noema/open-current-note)
      ("O" "open file…"       +noema/open-file)
      ("s" "save"             +noema/save)
      ("!d" "move to Trash"  noema-trash-note)
      ("r" "refresh"          +noema/refresh)
      ("!f" "reload from disk" noema-refresh-file)
      ("%" "AI / agent…"      noema-ai-menu)
      ("f" "focus editor"     +noema/focus)
      ("e" "escape/normal"    +noema/escape)
      ("v" "toggle source"    +noema/toggle-source)
      ("W" "editable split"   +noema/open-current-note-split)
      ("B" "build + reopen"   +noema/build-and-reopen)
      ("Q" "close all"        +noema/close)
      ("R" "raw edit in Emacs" +noema/open-markdown-raw)
      ("J" "toggle open surface" +noema/toggle-markdown-surface)
      ("?" "roam key reference" +noema-roam/help)]
     ["Find / Browse"
      ("j" "find note"        +noema-roam/find-note)
      ("/" "knowledge search" +noema/knowledge-search)
      ("l" "recent pages"     +noema/wiki-recent)
      ("." "follow link"      +noema-roam/follow-link)
      ("b" "backlinks"        +noema-roam/backlinks)
      ("x" "related notes"    +noema-roam/related-notes)
      ("G" "goto definition"  +noema-roam/goto-definition)]
     ["Insert"
      ("i" "roam link"        +noema-roam/insert-link)
      ("I" "TOC link"         +noema-roam/insert-toc-link)
      ("t" "tag id"           +noema-roam/insert-tag-id)
      ("T" "tag-id link"      +noema-roam/insert-tag-id-link)
      ("w" "copy link here"   +noema-roam/copy-link-to-here)
      ("c" "note-code"        +note-code/insert)]
     ;; Row 2 ─────────────────────────────────────────────────────────────────
     ["Knowledge"
      ("h" "Wiki home"        +noema/wiki-home)
      ("n" "new page"         +noema/wiki-new-page)
      ("d" "daily note"       +noema-roam/daily-note)
      ("a" "tags"             +noema/wiki-tags)
      ("C" "namespaces"       +noema/wiki-namespaces)
      ("g" "workspace graph"  +noema/workspace-graph)
      ("k" "tasks"            +noema-roam/todos)
      ("A" "agenda"           noema-agenda)
      ("L" "agenda log"       noema-agenda-log)
      ("F" "file todos"       +noema-roam/jump-file-todo)
      ("M" "repositories"     +noema/wiki-repositories)]
     ["Wiki pages"
      ("!r" "reports"         +noema/wiki-reports)
      ("!n" "focused reports" noema-reports-menu)
      ("!w" "wanted pages"    +noema/wiki-wanted)
      ("!p" "all pages"       +noema/wiki-pages)
      ("!s" "sync"            +noema/wiki-sync)]
     ["Index / Host"
      ("y" "incremental refresh" +noema/wiki-refresh)
      ("Z" "atomic full rebuild" +noema/wiki-rebuild)
      ("S" "Wiki index status" +noema/wiki-index-status)
      ("P" "pause/resume"     +noema/toggle-pause)
      ("H" "runtime status"   +noema/runtime-status)
      ("D" "dired"            +noema-roam/dired)
      ("m" "move note"        noema-move-note)
      ("X" "magit"            +noema-roam/magit)
      ("q" "stop server"      +noema/stop)]
     ["Format (web)"
      ("1" "bold"             +noema/bold)
      ("2" "italic"           +noema/italic)
      ("3" "code inline"      +noema/code-inline)
      ("4" "highlight"        +noema/highlight)
      ("5" "strike"           +noema/strike)
      ("^" "superscript"      +noema/superscript)
      ("_" "subscript"        +noema/subscript)
      ("$" "inline math"      +noema/inline-math)
      ("N" "footnote"         +noema/insert-footnote)
      ("K" "revision"         +noema/insert-revision)
      ("@" "properties"       +noema/edit-properties)
      ("[" "move block up"    +noema/move-block-up)
      ("]" "move block down"  +noema/move-block-down)
      ("6" "blockquote"       +noema/blockquote)
      ("7" "bullet list"      +noema/bullet-list)
      ("8" "ordered list"     +noema/ordered-list)
      ("9" "task list"        +noema/task-list)
      ("=" "check task"       +noema/toggle-task)
      ("0" "code block"       +noema/code-block)
      ("p" "heading menu"     +noema/paragraph-menu)
      ("z" "insert table"     +noema/insert-table)
      ("E" "math block"       +noema/insert-math)
      ("u" "insert TOC"       +noema/insert-toc)
      ("U" "undo"             +noema/undo)
      ("Y" "redo"             +noema/redo)
      ("V" "paste"            +noema/paste)]
     ["Layout (web)"
      ("!g" "grid"            +noema/insert-layout-grid)
      ("!3" "three columns"   +noema/insert-layout-three-columns)
      ("!l" "flowing columns" +noema/insert-layout-flow)
      ("!e" "editor settings" +noema/settings)]]))

(with-eval-after-load 'transient
  (transient-define-prefix noema-ai-menu ()
    "AI, context and handwriting tasks for Noema.
Selection commands ask the page to save and report its range;
`noema-md-bridge' then runs the UI on the note's Emacs source buffer."
    ["Selection → agent session (last choice preselected)"
     ("a" "send selection"       noema-context-send-region)
     ("l" "send cursor line"     noema-context-send-at-point)
     ("n" "send whole note"      noema-context-send-buffer)]
    ["gptel"
     ("r" "rewrite (diff review)" noema-compose-rewrite)
     ("." "add to context"       noema-compose-add-context)
     ("c" "compose with context" noema-compose)
     ("m" "gptel menu"           noema-compose-menu)]
    ["Context / sessions"
     ("M" "MarginNote digitization" noema-marginnote)
     ("x" "send shared context"  noema-context-send)
     ("," "inspect context"      noema-context-inspect)
     ("e" "edit in Emacs source" noema-md-bridge-edit-source)
     ("S" "sessions"             noema-sessions)]))

(with-eval-after-load 'transient
  (transient-define-prefix noema-wiki-menu ()
    "Emacs-hosted Wiki navigation and index maintenance."
    [:description +noema--dispatch-header
     ["Browse"
      ("h" "home" +noema/wiki-home)
      ("p" "pages" +noema/wiki-pages)
      ("r" "recent" +noema/wiki-recent)
      ("n" "new page" +noema/wiki-new-page)
      ("g" "graph" +noema/workspace-graph)]
     ["Organize"
      ("t" "tags" +noema/wiki-tags)
      ("N" "namespaces" +noema/wiki-namespaces)
      ("R" "repositories" +noema/wiki-repositories)
      ("s" "sync" +noema/wiki-sync)
      ("!" "reports" +noema/wiki-reports)
      ("D" "focused reports" noema-reports-menu)]
     ["Index"
      ("i" "incremental refresh" +noema/wiki-refresh)
      ("F" "atomic full rebuild" +noema/wiki-rebuild)
      ("S" "status" +noema/wiki-index-status)]]))

;;; Keybindings.

;; Global: H-o opens the Noema dispatch panel.
(general-define-key "H-o" #'noema-menu)
(general-define-key "C-H-o" #'noema-menu)

(+noema--rename-live-buffers)

(provide 'noema/bridge/config)
;;; noema/bridge/config.el ends here
