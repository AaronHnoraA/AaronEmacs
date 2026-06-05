;;; init-aaronnote.el --- Aaronnote Web/Appine bridge -*- lexical-binding: t; -*-
;;
;; Emacs starts the local Aaronnote web host and opens it in Appine/xwidget.
;; The editable document state lives in Aaronnote's CodeMirror app; Emacs does
;; not mirror buffer edits into the browser.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-util)

(declare-function my/open-xwidget-url "init-browser" (url &optional reuse-selected))
(declare-function my/appine-open-url "init-appine" (url))
(declare-function appine-focus "appine" ())

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

(defvar my/aaronnote--snippets-root
  (expand-file-name "snippets" user-emacs-directory)
  "Path to Aaronnote snippets shared with Emacs.")

(defvar my/aaronnote--templates-root
  (expand-file-name "lisp/roam/aaronnote/templates" user-emacs-directory)
  "Path to templates owned by the Aaronnote project.")

(defvar my/aaronnote--notes-root
  (expand-file-name ".roam" user-emacs-directory)
  "Path to the Markdown notes directory.")

(defcustom my/aaronnote-backend 'appine
  "Backend used to display Aaronnote."
  :type '(choice (const :tag "Appine" appine)
                 (const :tag "xwidget-webkit" xwidget))
  :group 'my/aaronnote)

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
      (my/aaronnote--start-server))))

(defun my/aaronnote--start-server ()
  "Spawn the vendored Aaronnote web-host."
  (when (and my/aaronnote--process (process-live-p my/aaronnote--process))
    (delete-process my/aaronnote--process))
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
            (format "AARONNOTE_SNIPPETS_ROOT=%s" (expand-file-name my/aaronnote--snippets-root))
            (format "AARONNOTE_TEMPLATES_ROOT=%s" (expand-file-name my/aaronnote--templates-root))
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
  (let ((callbacks (nreverse my/aaronnote--ready-callbacks)))
    (setq my/aaronnote--ready-callbacks nil)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback))))

(defun my/aaronnote--handle-process-line (line)
  "Handle one web-host stdout LINE."
  (let ((ready-prefix "aaronote-web-host:ready:")
        (goto-prefix "aaronote-event:goto:")
        (open-prefix "aaronote-event:open:"))
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
                 (column (or (alist-get 'col payload) 0)))
            (my/aaronnote--goto-location file line-number column))
        (error
         (message "Aaronnote event parse failed: %s" (error-message-string err))))))))

(defun my/aaronnote--process-filter (proc output)
  "Process web-host OUTPUT from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert output)))
  (let ((pending (or (process-get proc 'aaronnote-pending) "")))
    (setq pending (concat pending output))
    (let (newline)
      (while (setq newline (string-match "\n" pending))
        (my/aaronnote--handle-process-line
         (string-trim-right (substring pending 0 newline) "\r"))
        (setq pending (substring pending (1+ newline)))))
    (process-put proc 'aaronnote-pending pending)))

(defun my/aaronnote--sentinel (proc event)
  "Handle web-host PROC state change EVENT."
  (when (and (eq proc my/aaronnote--process)
             (not (process-live-p proc)))
    (setq my/aaronnote--process nil
          my/aaronnote--port nil
          my/aaronnote--ready nil
          my/aaronnote--ready-callbacks nil)
    (unless (string-match-p "^finished" event)
      (message "Aaronnote web-host: %s" (string-trim event)))))

(defun my/aaronnote--track-app-buffer (buffer)
  "Track BUFFER as the embedded Aaronnote browser buffer."
  (setq my/aaronnote--app-buffer buffer))

(defun my/aaronnote--open-xwidget (url)
  "Open Aaronnote URL in xwidget in the selected window."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (require 'xwidget))
  (my/open-xwidget-url url t)
  (my/aaronnote--track-app-buffer (current-buffer)))

(defun my/aaronnote--open-appine (url)
  "Open Aaronnote URL in Appine in the selected window."
  (unless (fboundp 'my/appine-open-url)
    (require 'init-appine))
  (let ((buffer (get-buffer-create "*Appine Window*")))
    (my/aaronnote--track-app-buffer buffer)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local cursor-type nil)
      (setq buffer-read-only t))
    (set-window-buffer (selected-window) buffer)
    (with-current-buffer buffer
      (my/appine-open-url url))
    (when (fboundp 'appine-focus)
      (run-at-time 0.05 nil
                   (lambda ()
                     (when (get-buffer-window buffer 'visible)
                       (ignore-errors (appine-focus))))))))

(defun my/aaronnote--open-url (url)
  "Open Aaronnote URL using `my/aaronnote-backend'."
  (pcase my/aaronnote-backend
    ('appine
     (condition-case err
         (my/aaronnote--open-appine url)
       (error
        (message "Aaronnote: Appine unavailable, using xwidget (%s)"
                 (error-message-string err))
        (my/aaronnote--open-xwidget url))))
    ('xwidget (my/aaronnote--open-xwidget url))
    (_ (user-error "Unsupported Aaronnote backend: %S" my/aaronnote-backend))))

(defun my/aaronnote--post (payload)
  "Send small control PAYLOAD to the Aaronnote web-host."
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
      (url-retrieve (my/aaronnote--server-url "/emacs/command")
                    (lambda (_status)
                      (when (buffer-live-p (current-buffer))
                        (kill-buffer (current-buffer))))
                    nil t t))))

(defun my/aaronnote--open-file-in-web (file)
  "Ask the already open Aaronnote page to open FILE."
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
       (my/aaronnote--open-url (my/aaronnote--app-url file))
       (run-at-time 0.3 nil #'my/aaronnote--open-file-in-web file)))))

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
  "Open the standalone roam graph view in a browser window."
  (interactive)
  (my/aaronnote--ensure-server
   (lambda ()
     (my/aaronnote--open-url (my/aaronnote--server-url "/graph")))))

;;;###autoload
(defun my/aaronnote-stop ()
  "Kill the Aaronnote web-host process."
  (interactive)
  (when (and my/aaronnote--process (process-live-p my/aaronnote--process))
    (delete-process my/aaronnote--process))
  (setq my/aaronnote--process nil
        my/aaronnote--port nil
        my/aaronnote--ready nil
        my/aaronnote--ready-callbacks nil)
  (message "Aaronnote web-host stopped."))

(add-hook 'kill-emacs-hook #'my/aaronnote-stop)

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
