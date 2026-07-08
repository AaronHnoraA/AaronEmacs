;;; init-aaronnote.el --- Aaronnote Web/Appine bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Aaronnote web host and opens it in Appine/xwidget.
;; The editable document state lives in Aaronnote's CodeMirror app; Emacs does
;; not mirror buffer edits into the browser.

;;; Code:

(require 'config)

(require 'json)
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
(declare-function my/aaronnote-jupyter-open-path "init-aaronnote-jupyter" (abs-path &optional selector))
(declare-function my/aaronnote-jupyter-open-root "init-aaronnote-jupyter" ())
(declare-function my/aaronnote-jupyter-open-target "init-aaronnote-jupyter" (target))
(declare-function my/aaronnote-jupyter-open-url "init-aaronnote-jupyter" (url))
(declare-function my/aaronnote-jupyter-url-p "init-aaronnote-jupyter" (url))
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

(config-defvar my/aaronnote-latex-export-engine "codex"
  "Engine for the Aaronnote CMD+P LaTeX export.
\"codex\" builds a deterministic mechanical draft and then lets codex polish it,
gated on the document compiling (with fallback to the draft).  \"mechanical\"
uses only the deterministic converter.  See `docs/latex-export-style.md' in the
Aaronnote app."
  :type '(choice (const "codex") (const "mechanical"))
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-max-attempts 3
  "Maximum codex polish/compile retries before falling back to the LaTeX draft."
  :type 'integer
  :group 'my/aaronnote)

(config-defvar my/aaronnote-latex-export-agent "codex"
  "AI backend for the Aaronnote LaTeX export polish step.
One of \"codex\", \"claude\", or \"opencode\".  All run non-interactively with
permission prompts disabled.  The backend is chosen here, not per export."
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
(defvar my/aaronnote--port nil
  "HTTP port of the running Aaronnote web-host.")
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

(defvar my/aaronnote--readonly-split-counter 0
  "Counter for fresh read-only Aaronnote xwidget split sessions.")

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
       (expand-file-name file)))

(defun my/aaronnote--xwidget-session-id (&optional file)
  "Return the stable xwidget session/client id for FILE."
  (if-let* ((file (my/aaronnote--canonical-file file)))
      (format "aaronnote:%s" file)
    "aaronnote"))

(defun my/aaronnote--readonly-client-p (client)
  "Return non-nil when CLIENT identifies a read-only split pane."
  (and (stringp client)
       (string-prefix-p "aaronnote-readonly:" client)))

(defun my/aaronnote--app-url (&optional file client extra-params)
  "Return the Aaronnote app URL, optionally opening FILE for CLIENT."
  (let ((base (my/aaronnote--server-url "/"))
        params)
    (when-let* ((file (my/aaronnote--canonical-file file)))
      (push (cons "file" file) params))
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

(defun my/aaronnote--start-server ()
  "Spawn the vendored Aaronnote web-host."
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
         (copilot-server
          (when (require 'copilot nil t)
            (ignore-errors (copilot-server-executable))))
         (process-environment
          (append
           (delq nil
            (list
            (format "AARONNOTE_ROOT=%s" (expand-file-name my/aaronnote--notes-root))
            (format "AARONNOTE_WEB_DIR=%s" (expand-file-name my/aaronnote--web-dir))
            (format "AARONNOTE_RUNTIME_ROOT=%s" (expand-file-name my/aaronnote--runtime-root))
            (format "AARONNOTE_WORKSPACE_ROOT=%s" (expand-file-name user-emacs-directory))
            (format "AARONNOTE_VALE_CONFIG=%s"
                    (expand-file-name "vale-styles/.vale.ini" user-emacs-directory))
            (format "AARONNOTE_VALE_WORDS=%s"
                    (expand-file-name "vale-styles/config/vocabularies/Notes/accept.txt"
                                      user-emacs-directory))
            (format "AARONNOTE_PUBLISH_JS_DIR=%s"
                    (expand-file-name "js" my/aaronnote--runtime-root))
            (format "AARONNOTE_STATE_DIR=%s" (expand-file-name my/aaronnote--state-root))
            (format "AARONNOTE_TMP_DIR=%s" (expand-file-name my/aaronnote--tmp-root))
            (format "AARONNOTE_SNIPPETS_ROOT=%s" (expand-file-name my/aaronnote--snippets-root))
            (format "AARONNOTE_TEMPLATES_ROOT=%s" (expand-file-name my/aaronnote--templates-root))
            (format "AARONNOTE_KATEX_MACROS_DIR=%s" (expand-file-name my/aaronnote--katex-macros-dir))
            (format "AARONNOTE_LATEX_EXPORT_ENGINE=%s"
                    (or my/aaronnote-latex-export-engine "codex"))
            (format "AARONNOTE_LATEX_EXPORT_AGENT=%s"
                    (or (bound-and-true-p my/aaronnote-latex-export-agent) "codex"))
            (format "AARONNOTE_LATEX_EXPORT_MAX_ATTEMPTS=%d"
                    (or my/aaronnote-latex-export-max-attempts 3))
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
            (format "AARONNOTE_WEB_PORT=%d" my/aaronnote-web-port)
            (when copilot-server
              (format "AARONNOTE_COPILOT_LANGUAGE_SERVER=%s"
                      (expand-file-name copilot-server)))
            (when (bound-and-true-p my/copilot-server-max-heap-mb)
              (format "AARONNOTE_COPILOT_MAX_HEAP_MB=%d"
                      my/copilot-server-max-heap-mb))))
           process-environment))
         (proc (make-process
                :name "aaronnote-web-host"
                :buffer log-buf
                :command (append (list "node")
                                  (when my/aaronnote-web-host-max-heap-mb
                                    (list (format "--max-old-space-size=%d"
                                                  my/aaronnote-web-host-max-heap-mb)))
                                  (list my/aaronnote--web-host-script))
                :noquery t
                :sentinel #'my/aaronnote--sentinel
                :filter #'my/aaronnote--process-filter)))
    (with-current-buffer log-buf (erase-buffer))
    (setq my/aaronnote--process proc)
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

(defun my/aaronnote--select-emacs-window (&optional window)
  "Select WINDOW and ask the window system to focus its frame."
  (let ((window (or window (selected-window))))
    (when (window-live-p window)
      (select-window window)
      (when (fboundp 'select-frame-set-input-focus)
        (ignore-errors
          (select-frame-set-input-focus (window-frame window)))))))

(defun my/aaronnote--focus-minibuffer-if-active ()
  "Move focus to the active minibuffer after a forwarded Aaronnote key."
  (when-let* ((window (active-minibuffer-window)))
    (my/aaronnote--select-emacs-window window)))

(defun my/aaronnote--release-xwidget-input-buffer (&optional buffer)
  "Exit xwidget edit mode in BUFFER when it is an Aaronnote xwidget."
  (let ((buffer (or buffer my/aaronnote--app-buffer)))
    (when (and (buffer-live-p buffer)
               (fboundp 'xwidget-webkit-edit-mode))
      (with-current-buffer buffer
        (when (eq major-mode 'xwidget-webkit-mode)
          (ignore-errors (xwidget-webkit-edit-mode -1)))))))

(defun my/aaronnote--focus-xwidget-window (window)
  "Focus Aaronnote xwidget WINDOW like a direct window click."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (when (my/aaronnote--xwidget-buffer-p buffer)
        (select-window window)
        (my/aaronnote--select-emacs-window window)
        (setq my/aaronnote--app-buffer buffer)
        (when (fboundp 'my/xwidget-focus)
          (my/xwidget-focus buffer))))))

(defun my/aaronnote--focus-xwidget-window-if-still-selected (window buffer)
  "Focus WINDOW's xwidget if WINDOW still displays BUFFER and is selected."
  (when (and (window-live-p window)
             (eq window (selected-window))
             (eq (window-buffer window) buffer))
    (my/aaronnote--focus-xwidget-window window)))

(defun my/aaronnote--focus-selected-window-after-move (&optional source-window)
  "Restore input focus after selection moves away from SOURCE-WINDOW.
If the target is Aaronnote, enter xwidget edit focus.  If the source was
Aaronnote and the target is a normal Emacs window, restore Emacs frame focus."
  (let ((target-window (selected-window)))
    (when (and (window-live-p target-window)
               (or (not (window-live-p source-window))
                   (not (eq target-window source-window))))
      (let ((target-buffer (window-buffer target-window))
            (source-buffer (and (window-live-p source-window)
                                (window-buffer source-window))))
        (when (and source-buffer
                   (my/aaronnote--xwidget-buffer-p source-buffer))
          (my/aaronnote--release-xwidget-input-buffer source-buffer))
        (cond
         ((my/aaronnote--xwidget-buffer-p target-buffer)
          (my/aaronnote--focus-xwidget-window target-window)
          (run-at-time 0.05 nil
                       #'my/aaronnote--focus-xwidget-window-if-still-selected
                       target-window target-buffer))
         ((and source-buffer
               (my/aaronnote--xwidget-buffer-p source-buffer))
          (my/aaronnote--select-emacs-window target-window)))))))

(defun my/aaronnote--focus-forwarded-key-target (&optional source-window)
  "Restore input focus after an Aaronnote-forwarded key leaves SOURCE-WINDOW."
  (if (active-minibuffer-window)
      (my/aaronnote--focus-minibuffer-if-active)
    (my/aaronnote--focus-selected-window-after-move source-window)))

(defun my/aaronnote--windmove-focus-advice (orig-fun &rest args)
  "Around advice for windmove commands to focus Aaronnote targets correctly."
  (let ((source-window (selected-window)))
    (prog1 (apply orig-fun args)
      (my/aaronnote--focus-selected-window-after-move source-window))))

(defun my/aaronnote--release-xwidget-input ()
  "Exit Aaronnote xwidget edit mode before Emacs handles forwarded keys."
  (my/aaronnote--release-xwidget-input-buffer my/aaronnote--app-buffer))

(defun my/aaronnote--queue-emacs-key (keys key-string)
  "Queue KEYS forwarded from Aaronnote for Emacs' normal command loop.
KEY-STRING is used only for diagnostics."
  (let ((binding (key-binding keys)))
    (cond
     ((or (commandp binding) (keymapp binding))
      (setq unread-command-events
            (nconc (listify-key-sequence keys)
                   unread-command-events))
      (run-at-time 0.05 nil #'my/aaronnote--focus-forwarded-key-target
                   (selected-window)))
     (t
      (message "Aaronnote: no binding for %s" key-string)))))

(defun my/aaronnote--key-source-buffer (&optional client)
  "Return the Aaronnote buffer that forwarded a key for CLIENT."
  (or (my/aaronnote--buffer-for-client client)
      (let ((selected-buffer (window-buffer (selected-window))))
        (and (my/aaronnote--xwidget-buffer-p selected-buffer)
             selected-buffer))
      (and (buffer-live-p my/aaronnote--app-buffer)
           my/aaronnote--app-buffer)))

(defun my/aaronnote--key-source-window (&optional client)
  "Return the visible window that forwarded a key for CLIENT."
  (let ((source-buffer (my/aaronnote--key-source-buffer client)))
    (or (and (buffer-live-p source-buffer)
             (get-buffer-window source-buffer 'visible))
        (let ((window (selected-window)))
          (and (window-live-p window)
               (my/aaronnote--xwidget-buffer-p (window-buffer window))
               window)))))

(defun my/aaronnote--run-emacs-key (key-string &optional client)
  "Execute Emacs key KEY-STRING forwarded from the Aaronnote browser.
CLIENT, when non-nil, identifies the Aaronnote xwidget that sent the key."
  (condition-case err
      (let ((keys (ignore-errors (kbd key-string))))
        (when (and keys (> (length keys) 0))
          (let ((source-buffer (my/aaronnote--key-source-buffer client))
                (win (my/aaronnote--key-source-window client)))
            (my/aaronnote--release-xwidget-input-buffer source-buffer)
            (if (window-live-p win)
                (progn
                  (my/aaronnote--select-emacs-window win)
                  (my/aaronnote--queue-emacs-key keys key-string))
              (my/aaronnote--select-emacs-window)
              (my/aaronnote--queue-emacs-key keys key-string)))))
    (error
     (message "Aaronnote key forward failed (%s): %s"
              key-string (error-message-string err)))))

(defun my/aaronnote--handle-process-line (line)
  "Handle one web-host stdout LINE."
  (let ((ready-prefix "aaronote-web-host:ready:")
        (goto-prefix "aaronote-event:goto:")
	(open-prefix "aaronote-event:open:")
        (system-open-prefix "aaronote-event:system-open:")
	(current-file-prefix "aaronote-event:current-file:")
        (saved-prefix "aaronote-event:saved:")
        (key-prefix "aaronote-event:key:"))
    (cond
     ((string-prefix-p ready-prefix line)
      (let ((port (string-to-number (substring line (length ready-prefix)))))
        (when (> port 0)
          (setq my/aaronnote--port port
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
     ((string-prefix-p system-open-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length system-open-prefix))
                           :object-type 'alist))
                 (target (alist-get 'target payload)))
            (when (and (stringp target) (not (string-empty-p target)))
              (cond
               ((and (string-match-p "\\.ipynb\\(?:@\\|#\\|\\'\\)" target)
                     (progn
                       (unless (fboundp 'my/aaronnote-jupyter-open-target)
                         (require 'init-aaronnote-jupyter))
                       (my/aaronnote-jupyter-open-target target))))
               ((and (progn
                       (unless (fboundp 'my/aaronnote-jupyter-url-p)
                         (require 'init-aaronnote-jupyter nil t))
                       (fboundp 'my/aaronnote-jupyter-url-p))
                     (my/aaronnote-jupyter-url-p target))
                (my/aaronnote-jupyter-open-url target))
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
            (unless (my/aaronnote--readonly-client-p client)
              (my/aaronnote--sync-app-buffer-file file client)))
        (error
         (message "Aaronnote current-file parse failed: %s"
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

(defun my/aaronnote--process-filter (proc output)
  "Process web-host OUTPUT from PROC."
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
  (let ((pending (or (process-get proc 'aaronnote-pending) "")))
    (setq pending (concat pending output))
    ;; Safety cap: a pathological unterminated line must not grow without bound.
    (when (> (length pending) 262144)
      (setq pending ""))
    (let (newline)
      (while (setq newline (string-match "\n" pending))
        ;; Only mutate shared bridge state for the current process; a dying old
        ;; process emitting a trailing ready: line must not clobber the new port.
        (when (eq proc my/aaronnote--process)
          (my/aaronnote--handle-process-line
           (string-trim-right (substring pending 0 newline) "\r")))
        (setq pending (substring pending (1+ newline)))))
    (process-put proc 'aaronnote-pending pending)))

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

(defun my/aaronnote--readonly-buffer-display-name (file ordinal)
  "Return an ibuffer-friendly name for FILE's read-only split ORDINAL."
  (format "*aaronnote split %d: %s*"
          ordinal
          (file-name-nondirectory (my/aaronnote--canonical-file file))))

(defun my/aaronnote--cleanup-buffer ()
  "Remove the current buffer from Aaronnote identity registries."
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
                    (my/aaronnote--buffer-display-name file))
        (my/aaronnote-keys-mode 1)
        (when file
          (setq-local default-directory
                      (file-name-as-directory (file-name-directory file))))
        (add-hook 'kill-buffer-hook #'my/aaronnote--cleanup-buffer nil t)
        (when (and rename
                   (eq major-mode 'xwidget-webkit-mode)
                   (not (equal (buffer-name)
                               (my/aaronnote--buffer-display-name file))))
          (rename-buffer (my/aaronnote--buffer-display-name file) t)
          (setq changed t))
        (when changed
          (force-mode-line-update)
          (force-window-update (current-buffer))))
      (when file
        (puthash file buffer my/aaronnote--file-buffers))
      (when client
        (puthash client buffer my/aaronnote--client-buffers))
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

(defun my/aaronnote--xwidget-buffer-p (&optional buffer)
  "Return non-nil when BUFFER hosts the local Aaronnote xwidget page."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (or (eq buffer my/aaronnote--app-buffer)
             (with-current-buffer buffer
               (and (eq major-mode 'xwidget-webkit-mode)
                    (or
                     my/aaronnote--client-id
                     my/aaronnote-buffer-file-name
                     my/aaronnote--xwidget-forced-name
                     (and (integerp my/aaronnote--port)
                          (fboundp 'my/xwidget-current-url)
                          (when-let* ((url (my/xwidget-current-url buffer)))
                            (string-prefix-p
                             (format "http://127.0.0.1:%d/" my/aaronnote--port)
                             url))))))))))

(defun my/aaronnote--jupyter-xwidget-buffer-p (&optional buffer)
  "Return non-nil when BUFFER hosts the Aaronnote-owned Jupyter xwidget page."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq major-mode 'xwidget-webkit-mode)
                (or (equal (and (boundp 'my/xwidget--session-id)
                                my/xwidget--session-id)
                           "aaronnote-jupyter")
                    (and (progn
                           (unless (fboundp 'my/aaronnote-jupyter-url-p)
                             (require 'init-aaronnote-jupyter nil t))
                           (fboundp 'my/aaronnote-jupyter-url-p))
                         (fboundp 'my/xwidget-current-url)
                         (when-let* ((url (my/xwidget-current-url buffer)))
                           (my/aaronnote-jupyter-url-p url)))))))))

(defun my/aaronnote--pass-xwidget-command-event (event)
  "Pass EVENT through to xwidget when the current buffer is not Aaronnote."
  (if (fboundp 'xwidget-webkit-pass-command-event)
      (xwidget-webkit-pass-command-event event)
    (setq unread-command-events
          (nconc (list event) unread-command-events))))

(defun my/aaronnote--jupyter-xwidget-command (event command)
  "Route xwidget EVENT/COMMAND to Jupyter, or pass EVENT through."
  (pcase command
    ("undo"
     (if (fboundp 'my/xwidget-undo)
         (my/xwidget-undo)
       (my/aaronnote--pass-xwidget-command-event event)))
    ("redo"
     (if (fboundp 'my/xwidget-redo)
         (my/xwidget-redo)
       (my/aaronnote--pass-xwidget-command-event event)))
    (_
     (my/aaronnote--pass-xwidget-command-event event))))

(defun my/aaronnote--xwidget-editor-command (event command &optional detail)
  "Route xwidget EVENT to Aaronnote COMMAND, or pass it through otherwise."
  (cond
   ((my/aaronnote--xwidget-buffer-p)
    (my/aaronnote-command command detail))
   ((my/aaronnote--jupyter-xwidget-buffer-p)
    (my/aaronnote--jupyter-xwidget-command event command))
   (t
    (my/aaronnote--pass-xwidget-command-event event))))

(defun my/aaronnote-xwidget-undo (event)
  "Route Command-z / Meta-z from Aaronnote xwidget to web undo."
  (interactive "e")
  (my/aaronnote--xwidget-editor-command event "undo"))

(defun my/aaronnote-xwidget-redo (event)
  "Route Command-Shift-z / Meta-Shift-z from Aaronnote xwidget to web redo."
  (interactive "e")
  (my/aaronnote--xwidget-editor-command event "redo"))

(defun my/aaronnote-xwidget-shift-tab (event)
  "Route Shift-Tab to Aaronnote in xwidget without losing the Shift modifier."
  (interactive "e")
  (my/aaronnote--xwidget-editor-command
   event
   "key"
   '((key . "Tab")
     (shiftKey . t))))

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
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
           (buf (url-retrieve (my/aaronnote--server-url "/emacs/command")
                              (lambda (_status)
                                (unwind-protect nil
                                  (when (buffer-live-p (current-buffer))
                                    (kill-buffer (current-buffer)))))
                              nil t t)))
      ;; Fallback: kill response buffer if server never replies within 5 s.
      (when (buffer-live-p buf)
        (run-at-time 5 nil (lambda () (when (buffer-live-p buf) (kill-buffer buf))))))))

(defun my/aaronnote--open-file-in-web (file)
  "Ask the already open Aaronnote page to open FILE."
  (my/aaronnote--sync-app-buffer-file file)
  (my/aaronnote--post `((type . "open") (file . ,(expand-file-name file)))))

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
                   (ignore-errors (file-truename (expand-file-name file)))))
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
  (let ((file (expand-file-name file))
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

(defun my/aaronnote--readonly-split-window ()
  "Create and select the window for a read-only Aaronnote split."
  (let ((window (if (>= (window-total-width) 120)
                    (split-window-right)
                  (split-window-below))))
    (select-window window)
    window))

;;;###autoload
(defun my/aaronnote-open-current-note-readonly-split ()
  "Open the current Markdown note in a fresh read-only Aaronnote xwidget split.

This intentionally does not reuse or register the canonical editable
Aaronnote xwidget for the file.  Multiple xwidget windows for the same live
session have rendering issues, so this command creates an isolated read-only
client and keeps it out of the normal file/session sync maps."
  (interactive)
  (let ((file (my/aaronnote--current-note-file)))
    (unless (and file (my/aaronnote--markdown-file-p file))
      (user-error "No current Markdown note for Aaronnote"))
    (let ((file (expand-file-name file))
          (source-window (selected-window)))
      (my/aaronnote--ensure-server
       (lambda ()
         (when (window-live-p source-window)
           (select-window source-window))
         (unless (fboundp 'my/xwidget-open-url)
           (require 'init-browser))
         (let* ((ordinal (cl-incf my/aaronnote--readonly-split-counter))
                (client (format "aaronnote-readonly:%s:%d"
                                (file-truename file)
                                ordinal))
                (url (my/aaronnote--app-url
                      file client
                      '((readonly . "1"))))
                (target-window (my/aaronnote--readonly-split-window))
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
                           (my/aaronnote--readonly-buffer-display-name
                            file ordinal))
               (setq-local my/xwidget-focus-script nil)
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
                             (file-name-as-directory (file-name-directory file))))
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
        (my/aaronnote-command "refresh")
        (my/aaronnote-focus))
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

(defun my/aaronnote--app-buffer-visible-p ()
  "Return non-nil when the Aaronnote buffer is visible in a focused frame."
  (when (buffer-live-p my/aaronnote--app-buffer)
    (let ((win (get-buffer-window my/aaronnote--app-buffer 'visible)))
      (and win
           (frame-focus-state (window-frame win))))))

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

;;; API call — POST to /api and parse JSON response.

(defun my/aaronnote--api-call-sync (channel args)
  "POST CHANNEL with ARGS to /api synchronously; return parsed JSON or nil.
Only usable when the web-host is running (`my/aaronnote--ready' is non-nil).
Blocks the caller until the response arrives (or 8 s timeout)."
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data
            (encode-coding-string
             (json-encode `((channel . ,channel) (args . ,args)))
             'utf-8))
           (buf (url-retrieve-synchronously
                 (my/aaronnote--server-url "/api")
                 t nil 8)))
      (when (buffer-live-p buf)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (when (re-search-forward "^\r?\n" nil t)
                (condition-case err
                    (json-parse-string
                     (buffer-substring (point) (point-max))
                     :object-type 'hash-table
                     :array-type 'list)
                  (error
                   (message "Aaronnote API parse error: %s"
                            (error-message-string err))
                   nil))))
          (kill-buffer buf))))))

(defun my/aaronnote--api-call (channel args callback)
  "POST CHANNEL with ARGS to /api; parse JSON response and call CALLBACK."
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data
            (encode-coding-string
             (json-encode `((channel . ,channel) (args . ,args)))
             'utf-8))
           (buf (url-retrieve
                 (my/aaronnote--server-url "/api")
                 (lambda (status)
                   (unwind-protect
                       (unless (plist-get status :error)
                         (goto-char (point-min))
                         (when (re-search-forward "^\r?\n" nil t)
                           (condition-case err
                               (funcall callback
                                        (json-parse-string
                                         (buffer-substring (point) (point-max))
                                         :object-type 'alist))
                             (error
                              (message "Aaronnote API parse error: %s"
                                       (error-message-string err))))))
                     (when (buffer-live-p (current-buffer))
                       (kill-buffer (current-buffer)))))
                 nil t t)))
      ;; Fallback: kill response buffer if server never replies within 10 s.
      (when (buffer-live-p buf)
        (run-at-time 10 nil (lambda () (when (buffer-live-p buf) (kill-buffer buf))))))))

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

(defun my/aaronnote--header-line ()
  "Return the header-line string for the Aaronnote app buffer."
  (let* ((file (and (buffer-live-p my/aaronnote--app-buffer)
                    (my/aaronnote-buffer-file my/aaronnote--app-buffer)))
         (name (if file (file-name-nondirectory file) "Aaronnote"))
         (status (cond
                  ((not my/aaronnote--ready) " ○ offline")
                  (my/aaronnote--last-sync-stats
                   (format " ◉ %s" my/aaronnote--last-sync-stats))
                  (t " ● ready"))))
    (concat "  " (propertize name 'face 'mode-line-buffer-id) status)))

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
     ["Aaronnote: Open read-only split" my/aaronnote-open-current-note-readonly-split t]
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
(my/aaronnote--def-editor-cmd "blockquote"      "blockquote"      "Toggle blockquote on paragraph.")
(my/aaronnote--def-editor-cmd "bullet-list"     "bullet-list"     "Toggle bullet list.")
(my/aaronnote--def-editor-cmd "ordered-list"    "ordered-list"    "Toggle ordered list.")
(my/aaronnote--def-editor-cmd "task-list"       "task-list"       "Toggle task/checkbox list.")
(my/aaronnote--def-editor-cmd "code-block"      "code-block"      "Insert/toggle fenced code block.")
(my/aaronnote--def-editor-cmd "paragraph-menu"  "paragraph-menu"  "Open heading/paragraph type menu.")
(my/aaronnote--def-editor-cmd "insert-table"    "insert-table"    "Insert a Markdown table.")
(my/aaronnote--def-editor-cmd "insert-math"     "insert-math-block" "Insert a math block.")
(my/aaronnote--def-editor-cmd "insert-toc"      "insert-toc"      "Insert a table of contents.")
(my/aaronnote--def-editor-cmd "prose-check"     "prose-check"     "Run bounded Vale/CSpell checks in Aaronnote.")

;;; Jupyter integration.

(defun my/aaronnote--infer-notebook ()
  "Return the .ipynb file co-located with the current Aaronnote note, or nil."
  (when-let* ((file (my/aaronnote-buffer-file)))
    (let ((nb (concat (file-name-sans-extension file) ".ipynb")))
      (when (file-exists-p nb) nb))))

(defun my/aaronnote-jupyter-open ()
  "Open the notebook associated with the current note in xwidget.
Falls back to JupyterLab root when no matching .ipynb exists."
  (interactive)
  (unless (fboundp 'my/aaronnote-jupyter-open-path)
    (require 'init-aaronnote-jupyter))
  (if-let* ((nb (my/aaronnote--infer-notebook)))
      (my/aaronnote-jupyter-open-path nb)
    (my/aaronnote-jupyter-open-root)))

(defun my/aaronnote-jupyter-open-at-toc ()
  "Pick a heading from the current note, then open its notebook at that section."
  (interactive)
  (let ((file (my/aaronnote-buffer-file)))
    (unless file (user-error "No current Aaronnote note"))
    (unless (fboundp 'my/aaronnote-roam--dom-targets)
      (require 'init-md-roam))
    (let* ((targets (nreverse (my/aaronnote-roam--dom-targets file)))
           (choices  (mapcar
                      (lambda (tgt)
                        (cons (string-join (plist-get tgt :label-path) " / ")
                              (car (last (plist-get tgt :path)))))
                      targets))
           (choice   (completing-read "Jump to heading: "
                                      (mapcar #'car choices) nil t))
           (slug     (cdr (assoc choice choices))))
      (unless (fboundp 'my/aaronnote-jupyter-open-path)
        (require 'init-aaronnote-jupyter))
      (if-let* ((nb (my/aaronnote--infer-notebook)))
          (my/aaronnote-jupyter-open-path nb slug)
        (my/aaronnote-jupyter-open-root)))))

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
      ("W" "readonly split"   my/aaronnote-open-current-note-readonly-split)
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
     ["Jupyter"
      ("J" "open notebook"    my/aaronnote-jupyter-open)
      ("H" "open at heading"  my/aaronnote-jupyter-open-at-toc)]
     ["Format (web)"
      ("1" "bold"             my/aaronnote-bold)
      ("2" "italic"           my/aaronnote-italic)
      ("3" "code inline"      my/aaronnote-code-inline)
      ("4" "highlight"        my/aaronnote-highlight)
      ("5" "strike"           my/aaronnote-strike)
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

;; On xwidget load-finished, send any pending file open command.
(defun my/aaronnote--xwidget-callback-advice (_xwidget _event-type)
  "After xwidget callback: fire pending file POST on load-finished."
  (when (and (eq _event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (let ((buf (and (fboundp 'xwidget-buffer)
                    (xwidget-buffer _xwidget))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when my/aaronnote--xwidget-pending-file
            (let ((file my/aaronnote--xwidget-pending-file)
                  (pending-buf (current-buffer)))
              (setq-local my/aaronnote--xwidget-pending-file nil)
              ;; Short pause so page JS finishes before the POST arrives.
              ;; Guard: the xwidget buffer may have been killed in that window.
              (run-at-time 0.3 nil
                           (lambda ()
                             (when (buffer-live-p pending-buf)
                               (my/aaronnote--open-file-in-web file)))))))))))

(defvar my/aaronnote--xwidget-advice-installed nil
  "Non-nil when `my/aaronnote--xwidget-callback-advice' has been added.")

(with-eval-after-load 'xwidget
  (unless my/aaronnote--xwidget-advice-installed
    (advice-add 'xwidget-webkit-callback :after
                #'my/aaronnote--xwidget-callback-advice)
    (setq my/aaronnote--xwidget-advice-installed t)))

(with-eval-after-load 'xwidget
  (dolist (map (list xwidget-webkit-mode-map xwidget-webkit-edit-mode-map))
    (dolist (key '("M-z"))
      (define-key map (kbd key) #'my/aaronnote-xwidget-undo))
    (dolist (key '("M-Z" "M-S-z"))
      (define-key map (kbd key) #'my/aaronnote-xwidget-redo))
    (dolist (key '("<backtab>" "<iso-lefttab>" "S-TAB" "S-<tab>"))
      (define-key map (kbd key) #'my/aaronnote-xwidget-shift-tab))))

(defvar my/aaronnote--windmove-focus-advice-installed nil
  "Non-nil when Aaronnote windmove focus advice has been installed.")

(defun my/aaronnote--install-windmove-focus-advice ()
  "Install focus repair advice for windmove transitions involving Aaronnote."
  (unless my/aaronnote--windmove-focus-advice-installed
    (dolist (command '(windmove-left windmove-right windmove-up windmove-down))
      (when (fboundp command)
        (advice-add command :around #'my/aaronnote--windmove-focus-advice)))
    (setq my/aaronnote--windmove-focus-advice-installed t)))

(with-eval-after-load 'windmove
  (my/aaronnote--install-windmove-focus-advice))

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
