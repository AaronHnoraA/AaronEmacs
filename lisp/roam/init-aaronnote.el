;;; init-aaronnote.el --- Aaronnote Web/Appine bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Aaronnote web host and opens it in Appine/xwidget.
;; The editable document state lives in Aaronnote's CodeMirror app; Emacs does
;; not mirror buffer edits into the browser.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'url)
(require 'url-util)

(declare-function my/xwidget-open-url "init-browser" (url &rest args))
(declare-function my/xwidget-current-url "init-browser" (&optional buffer))
(declare-function my/xwidget-session-buffer "init-browser" (id))
(declare-function my/xwidget-focus "init-browser" (&optional buffer))
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
(defvar my/appine-tab-list)

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

(defcustom my/aaronnote-backend 'xwidget
  "Backend used to display Aaronnote."
  :type '(choice (const :tag "xwidget-webkit" xwidget)
                 (const :tag "Appine" appine))
  :group 'my/aaronnote)

(defcustom my/aaronnote-web-port 50815
  "Fixed port for the Aaronnote web host.
Set to 0 to let the OS pick a random port."
  :type 'integer
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

(defvar-local my/aaronnote-buffer-file-name nil
  "Current note file represented by an Aaronnote Appine/xwidget buffer.")

(put 'my/aaronnote-buffer-file-name 'permanent-local t)

(defvar-local my/aaronnote--xwidget-pending-file nil
  "File to POST to Aaronnote once the page has finished loading, or nil.")

(put 'my/aaronnote--xwidget-pending-file 'permanent-local t)

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

(defun my/aaronnote--app-url (&optional file)
  "Return the Aaronnote app URL, optionally opening FILE."
  (let ((base (my/aaronnote--server-url "/")))
    (if (and file (not (string-empty-p file)))
        (concat base "?file=" (url-hexify-string (expand-file-name file)))
      base)))

(defun my/aaronnote--markdown-file-p (file)
  "Return non-nil when FILE is a Markdown file."
  (and file
       (or (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
           (string-equal (file-name-nondirectory file) "README"))))

(defun my/aaronnote--watchdog-fire ()
  "Called when the web-host fails to become ready within the timeout."
  (setq my/aaronnote--ready-watchdog nil)
  (unless my/aaronnote--ready
    (setq my/aaronnote--ready-callbacks nil)
    (message "Aaronnote: web-host did not become ready (check *aaronnote-web-host* for errors)")))

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
            (format "AARONNOTE_PUBLISH_JS_DIR=%s"
                    (expand-file-name "js" my/aaronnote--runtime-root))
            (format "AARONNOTE_STATE_DIR=%s" (expand-file-name my/aaronnote--state-root))
            (format "AARONNOTE_TMP_DIR=%s" (expand-file-name my/aaronnote--tmp-root))
            (format "AARONNOTE_SNIPPETS_ROOT=%s" (expand-file-name my/aaronnote--snippets-root))
            (format "AARONNOTE_TEMPLATES_ROOT=%s" (expand-file-name my/aaronnote--templates-root))
            (format "AARONNOTE_WEB_PORT=%d" my/aaronnote-web-port)
            (when copilot-server
              (format "AARONNOTE_COPILOT_LANGUAGE_SERVER=%s"
                      (expand-file-name copilot-server)))))
           process-environment))
         (proc (make-process
                :name "aaronnote-web-host"
                :buffer log-buf
                :command (list "node" my/aaronnote--web-host-script)
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

(defun my/aaronnote--release-xwidget-input ()
  "Exit Aaronnote xwidget edit mode before Emacs handles forwarded keys."
  (when (and (buffer-live-p my/aaronnote--app-buffer)
             (fboundp 'xwidget-webkit-edit-mode))
    (with-current-buffer my/aaronnote--app-buffer
      (when (eq major-mode 'xwidget-webkit-mode)
        (ignore-errors (xwidget-webkit-edit-mode -1))))))

(defun my/aaronnote--queue-emacs-key (keys key-string)
  "Queue KEYS forwarded from Aaronnote for Emacs' normal command loop.
KEY-STRING is used only for diagnostics."
  (let ((binding (key-binding keys)))
    (cond
     ((or (commandp binding) (keymapp binding))
      (setq unread-command-events
            (nconc (listify-key-sequence keys)
                   unread-command-events))
      (run-at-time 0.05 nil #'my/aaronnote--focus-minibuffer-if-active))
     (t
      (message "Aaronnote: no binding for %s" key-string)))))

(defun my/aaronnote--run-emacs-key (key-string)
  "Execute Emacs key KEY-STRING forwarded from the Aaronnote browser."
  (condition-case err
      (let ((keys (ignore-errors (kbd key-string))))
        (when (and keys (> (length keys) 0))
          (my/aaronnote--release-xwidget-input)
          (let ((win (and (buffer-live-p my/aaronnote--app-buffer)
                          (get-buffer-window my/aaronnote--app-buffer 'visible))))
            (if (window-live-p win)
                (with-selected-window win
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
          (my/aaronnote--goto-location nil line-number column))))
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
              (my/aaronnote--sync-app-buffer-file file)
              (my/aaronnote--goto-location file line-number column)
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
              (if (and (fboundp 'my/jupyter-lab-url-p)
                       (my/jupyter-lab-url-p target))
                  (progn
                    (unless (fboundp 'my/xwidget-open-url)
                      (require 'init-browser))
                    (my/xwidget-open-url target :id "jupyter-lab" :display 'side))
                (unless (fboundp 'my/open-system-target)
                  (require 'init-open))
                (my/open-system-target target))))
        (error
         (message "Aaronnote system-open event failed: %s"
                  (error-message-string err)))))
     ((string-prefix-p current-file-prefix line)
      (condition-case err
          (let* ((payload (json-parse-string
                           (substring line (length current-file-prefix))
                           :object-type 'alist))
                 (file (alist-get 'file payload)))
            (my/aaronnote--sync-app-buffer-file file))
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
                 (key (alist-get 'key payload)))
            (when (stringp key)
              (my/aaronnote--run-emacs-key key)))
        (error
         (message "Aaronnote key-event parse failed: %s"
                  (error-message-string err))))))))

(defun my/aaronnote--process-filter (proc output)
  "Process web-host OUTPUT from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert output)))
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
    (setq my/aaronnote--process nil
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

(defun my/aaronnote--sync-app-buffer-file (file)
  "Record FILE as the current note in the matching Aaronnote buffer.
Finds the buffer already tracking FILE, or falls back to the most recently
focused Aaronnote buffer."
  (let ((file (and (stringp file)
                   (not (string-empty-p file))
                   (expand-file-name file))))
    (let ((target (or (and file (my/aaronnote--buffer-for-file file))
                      my/aaronnote--app-buffer)))
      (when (buffer-live-p target)
        (with-current-buffer target
          (let ((changed (not (equal my/aaronnote-buffer-file-name file))))
            (setq-local my/aaronnote-buffer-file-name file)
            (when file
              (setq-local default-directory
                          (file-name-as-directory (file-name-directory file))))
            ;; Skip the redisplay pass when the file pointer hasn't changed;
            ;; current-file events arrive on every navigation even without a switch.
            (when changed
              (force-mode-line-update)
              (force-window-update (current-buffer)))))
        (when file
          (setq my/aaronnote--app-buffer target))))))

(defun my/aaronnote--track-app-buffer (buffer &optional file)
  "Record BUFFER as the active Aaronnote browser buffer.
When FILE is non-nil, set buffer-local file tracking directly."
  (setq my/aaronnote--app-buffer buffer)
  (when (and file (buffer-live-p buffer))
    (with-current-buffer buffer
      (setq-local my/aaronnote-buffer-file-name (expand-file-name file))
      (setq-local default-directory
                  (file-name-as-directory (file-name-directory file))))))

(defun my/aaronnote--xwidget-buffer-p (&optional buffer)
  "Return non-nil when BUFFER hosts the local Aaronnote xwidget page."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (or (eq buffer my/aaronnote--app-buffer)
             (with-current-buffer buffer
               (and (eq major-mode 'xwidget-webkit-mode)
                    (integerp my/aaronnote--port)
                    (fboundp 'my/xwidget-current-url)
                    (when-let* ((url (my/xwidget-current-url buffer)))
                      (string-prefix-p
                       (format "http://127.0.0.1:%d/" my/aaronnote--port)
                       url))))))))

(defun my/aaronnote-xwidget-shift-tab (event)
  "Route Shift-Tab to Aaronnote in xwidget without losing the Shift modifier."
  (interactive "e")
  (if (my/aaronnote--xwidget-buffer-p)
      (my/aaronnote-command
       "key"
       '((key . "Tab")
         (shiftKey . t)))
    (if (fboundp 'xwidget-webkit-pass-command-event)
        (xwidget-webkit-pass-command-event event)
      (setq unread-command-events
            (nconc (list event) unread-command-events)))))

(defun my/aaronnote--buffer-for-file (file)
  "Return a live Aaronnote buffer tracking FILE, or nil."
  (when (and (stringp file) (not (string-empty-p file)))
    (let ((abs (expand-file-name file)))
      (cl-find-if
       (lambda (buf)
         (and (buffer-live-p buf)
              (with-current-buffer buf
                (and (stringp my/aaronnote-buffer-file-name)
                     (string-equal (expand-file-name my/aaronnote-buffer-file-name)
                                   abs)))))
       (buffer-list)))))

(defun my/aaronnote--open-xwidget (url &optional file)
  "Open Aaronnote in a per-file xwidget session.
Each Markdown FILE gets its own dedicated xwidget session and buffer.
Switching to an already-open file reuses the existing buffer without
reloading.  Non-file opens (roam graph, etc.) share the singleton
\"aaronnote\" session."
  (unless (fboundp 'my/xwidget-open-url)
    (require 'init-browser))
  (let* ((id (if file
                 (format "aaronnote:%s" (expand-file-name file))
               "aaronnote"))
         (existing (and (fboundp 'my/xwidget-session-buffer)
                        (my/xwidget-session-buffer id))))
    (if existing
        ;; Session already alive for this file: switch to it without reloading.
        (progn
          (switch-to-buffer existing)
          (run-at-time 0.3 nil #'my/xwidget-focus existing)
          (my/aaronnote--track-app-buffer existing file)
          existing)
      ;; New session: open directly at the target URL.
      (let ((buffer (my/xwidget-open-url url
                                         :id id
                                         :display 'current
                                         :reuse-selected t)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local my/xwidget-focus-script my/aaronnote--xwidget-focus-script)))
        (my/aaronnote--track-app-buffer buffer file)
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
                                (when (buffer-live-p (current-buffer))
                                  (kill-buffer (current-buffer))))
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
  (my/aaronnote--post
   `((type . "command")
     (command . ,command)
     ,@(when detail `((detail . ,detail))))))

(defun my/aaronnote--goto-location (file line col)
  "Open FILE in Emacs and move to one-based LINE and zero-based COL.
When FILE is nil, use the current buffer."
  (let ((buffer (if (and file (not (string-empty-p file)))
                    (find-file-noselect file)
                  (current-buffer))))
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
        (when (require 'pulse nil t)
          (pulse-momentary-highlight-one-line (point)))))))

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
       (my/aaronnote--open-url (my/aaronnote--app-url file) file t)))))

;;;###autoload
(defun my/aaronnote-open-current-note ()
  "Open the current Markdown note in Aaronnote Web/Appine."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (my/aaronnote-open-file buffer-file-name))

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
  "Reopen the current Markdown note in Aaronnote."
  (interactive)
  (my/aaronnote-open-current-note))

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
(defvar my/aaronnote--activity-timer nil
  "Debounce timer for `my/aaronnote--update-activity'.")
(defvar my/aaronnote--activity-hooks-installed nil
  "Non-nil when Aaronnote pause/resume activity hooks are installed.")

(defun my/aaronnote--app-buffer-visible-p ()
  "Return non-nil when the Aaronnote buffer is visible in a focused frame."
  (when (buffer-live-p my/aaronnote--app-buffer)
    (let ((win (get-buffer-window my/aaronnote--app-buffer 'visible)))
      (and win
           (frame-focus-state (window-frame win))))))

(defun my/aaronnote--apply-activity (active)
  "Send pause or resume to the browser when the active state changes."
  (unless (eq (not active) (not my/aaronnote--paused))
    (setq my/aaronnote--paused (not active))
    (my/aaronnote--send-command (if active "resume" "pause"))))

(defun my/aaronnote--update-activity (&rest _)
  "Debounced check: pause or resume the browser based on buffer visibility.
Also tracks which Aaronnote buffer is currently focused so key forwarding
routes to the right session when multiple files are open."
  ;; Update the active buffer pointer immediately on window-selection changes.
  (let ((cur (current-buffer)))
    (when (my/aaronnote--xwidget-buffer-p cur)
      (setq my/aaronnote--app-buffer cur)))
  (when my/aaronnote--activity-timer
    (cancel-timer my/aaronnote--activity-timer))
  (setq my/aaronnote--activity-timer
        (run-with-idle-timer
         0.3 nil
         (lambda ()
           (setq my/aaronnote--activity-timer nil)
           (when my/aaronnote--ready
             (my/aaronnote--apply-activity
              (my/aaronnote--app-buffer-visible-p)))))))

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

;;; Jupyter integration.

(declare-function my/jupyter-lab-url-p    "init-jupyter-lab" (url))
(declare-function my/jupyter-lab-open     "init-jupyter-lab" ())
(declare-function my/jupyter-lab-open-path "init-jupyter-lab" (abs-path &optional selector))

(defun my/aaronnote--infer-notebook ()
  "Return the .ipynb file co-located with the current Aaronnote note, or nil."
  (when-let* ((file (my/aaronnote-buffer-file)))
    (let ((nb (concat (file-name-sans-extension file) ".ipynb")))
      (when (file-exists-p nb) nb))))

(defun my/aaronnote-jupyter-open ()
  "Open the notebook associated with the current note in xwidget.
Falls back to JupyterLab root when no matching .ipynb exists."
  (interactive)
  (unless (fboundp 'my/jupyter-lab-open) (require 'init-jupyter-lab))
  (if-let* ((nb (my/aaronnote--infer-notebook)))
      (my/jupyter-lab-open-path nb)
    (my/jupyter-lab-open)))

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
      (unless (fboundp 'my/jupyter-lab-open) (require 'init-jupyter-lab))
      (if-let* ((nb (my/aaronnote--infer-notebook)))
          (my/jupyter-lab-open-path nb slug)
        (my/jupyter-lab-open)))))

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
      ("g" "roam graph"       my/aaronnote-roam-graph)
      ("k" "tasks"            my/aaronnote-roam-todos)
      ("A" "agenda"           my/aaronnote-roam-agenda)
      ("M" "management"       my/aaronnote-roam-management)]
     ["Index / Files"
      ("y" "sync DB"          my/aaronnote-roam-sync)
      ("u" "update index"     my/aaronnote-roam-update-db)
      ("F" "full rebuild"     my/aaronnote-roam-sync-full)
      ("S" "DB status"        my/aaronnote-roam-db-status)
      ("D" "dired"            my/aaronnote-roam-dired)
      ("m" "magit"            my/aaronnote-roam-magit)
      ("q" "stop server"      my/aaronnote-stop)]
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

;; Global: H-o opens the Aaronnote dispatch panel (o for "org notes").
(general-define-key "H-o" #'my/aaronnote-dispatch)

;; Appine buffer direct keys — override global H- bindings that are irrelevant
;; when focused in the Aaronnote pane.
(with-eval-after-load 'appine
  (when (boundp 'appine-active-map)
    (define-key appine-active-map (kbd "H-o") #'my/aaronnote-dispatch)
    (define-key appine-active-map (kbd "H-s") #'my/aaronnote-save)
    (define-key appine-active-map (kbd "H-r") #'my/aaronnote-refresh)
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
    (dolist (key '("<backtab>" "<iso-lefttab>" "S-TAB" "S-<tab>"))
      (define-key map (kbd key) #'my/aaronnote-xwidget-shift-tab))))

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
