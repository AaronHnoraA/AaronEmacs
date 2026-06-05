;;; init-aaronnote.el --- Aaronote app embedded in xwidget-webkit -*- lexical-binding: t; -*-
;;
;; Embeds the full Aaronote note app inside Emacs via xwidget-webkit.
;; The Node web-host (lisp/roam/aaronnote-web-host.mjs) bridges the app's
;; Electron-only aaronnoteApi to a plain HTTP+SSE server that xwidget can
;; load.  Emacs drives the app over a simple HTTP control channel:
;;   POST /emacs/command {type:"preview", content}  – live markdown push
;;   POST /emacs/command {type:"command", command}  – UI commands (graph, etc.)

;;; Code:

(require 'json)
(require 'url)

(declare-function my/open-xwidget-url "init-browser" (url &optional reuse-selected))
(declare-function my/open--side-browser-window "init-open" (mode &optional reuse-selected))

;; ── Customisable paths ──────────────────────────────────────────────────────

(defvar my/aaronnote--web-host-script
  (expand-file-name "lisp/roam/aaronnote-web-host.mjs" user-emacs-directory)
  "Path to the aaronnote-web-host.mjs Node script.")

(defvar my/aaronnote--notes-root
  (expand-file-name ".roam" user-emacs-directory)
  "Path to the Markdown notes directory (the .roam symlink).")

(defcustom my/aaronnote-preview-delay 0.8
  "Idle seconds before pushing buffer content to the preview."
  :type 'number
  :group 'my/aaronnote)

;; ── Server state ────────────────────────────────────────────────────────────

(defvar my/aaronnote--process nil   "Running web-host child process, or nil.")
(defvar my/aaronnote--port    nil   "HTTP port of the running web-host.")
(defvar my/aaronnote--ready   nil   "Non-nil once the port has been announced.")

;; ── Preview debounce state ──────────────────────────────────────────────────

(defvar my/aaronnote--preview-timer nil   "Per-buffer idle timer for content push.")
(make-variable-buffer-local 'my/aaronnote--preview-timer)

(defvar my/aaronnote--last-content "" "Last content sent, for dedup.")
(make-variable-buffer-local 'my/aaronnote--last-content)

;; ── Server lifecycle ────────────────────────────────────────────────────────

(defun my/aaronnote--server-url (path)
  "Return http://127.0.0.1:<port>/<PATH>."
  (format "http://127.0.0.1:%d%s" my/aaronnote--port path))

(defun my/aaronnote--ensure-server (&optional callback)
  "Start the web-host if not running, then call CALLBACK with no args."
  (if (and my/aaronnote--process
           (process-live-p my/aaronnote--process)
           my/aaronnote--ready)
      (when callback (funcall callback))
    (my/aaronnote--start-server callback)))

(defun my/aaronnote--start-server (&optional ready-callback)
  "Spawn the Node web-host; call READY-CALLBACK once the port is known."
  (when (and my/aaronnote--process (process-live-p my/aaronnote--process))
    (delete-process my/aaronnote--process))
  (setq my/aaronnote--port  nil
        my/aaronnote--ready nil)
  (let* ((log-buf (get-buffer-create " *aaronnote-web-host*"))
         (process-environment
          (cons (format "AARONNOTE_ROOT=%s"
                        (expand-file-name my/aaronnote--notes-root))
                process-environment))
         (proc (make-process
                :name    "aaronnote-web-host"
                :buffer  log-buf
                :command (list "node" my/aaronnote--web-host-script)
                :noquery t
                :sentinel #'my/aaronnote--sentinel
                :filter   (my/aaronnote--make-filter ready-callback))))
    (setq my/aaronnote--process proc)
    (with-current-buffer log-buf (erase-buffer))
    proc))

(defun my/aaronnote--make-filter (ready-callback)
  "Return a process filter that extracts the port from stdout."
  (lambda (proc output)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (insert output)))
    (when (and (not my/aaronnote--ready)
               (string-match "aaronote-web-host:ready:\\([0-9]+\\)" output))
      (setq my/aaronnote--port  (string-to-number (match-string 1 output))
            my/aaronnote--ready t)
      (when ready-callback
        (run-at-time 0.1 nil ready-callback)))))

(defun my/aaronnote--sentinel (proc event)
  "Handle web-host process state change."
  (when (and (eq proc my/aaronnote--process)
             (not (process-live-p proc)))
    (setq my/aaronnote--process nil
          my/aaronnote--port    nil
          my/aaronnote--ready   nil)
    (unless (string-match-p "^finished" event)
      (message "aaronnote-web-host: %s" (string-trim event)))))

(defun my/aaronnote--stop-server ()
  "Kill the web-host process."
  (when (and my/aaronnote--process (process-live-p my/aaronnote--process))
    (delete-process my/aaronnote--process))
  (setq my/aaronnote--process nil
        my/aaronnote--port    nil
        my/aaronnote--ready   nil))

(add-hook 'kill-emacs-hook #'my/aaronnote--stop-server)

;; ── HTTP control channel ────────────────────────────────────────────────────

(defun my/aaronnote--post (endpoint payload)
  "POST JSON PAYLOAD to ENDPOINT on the web-host (fire-and-forget)."
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
      (url-retrieve (my/aaronnote--server-url endpoint)
                    (lambda (_status) nil) nil t t))))

(defun my/aaronnote--send-preview (content)
  "Push markdown CONTENT to the active Aaronote page."
  (my/aaronnote--post "/emacs/command"
                      `((type . "preview") (content . ,content))))

(defun my/aaronnote--send-command (command &optional detail)
  "Dispatch COMMAND to the Aaronote UI (open-roam-graph, etc.)."
  (my/aaronnote--post "/emacs/command"
                      `((type . "command") (command . ,command)
                        ,@(when detail `((detail . ,detail))))))

;; ── Live preview hooks ──────────────────────────────────────────────────────

(defun my/aaronnote--after-change (&rest _)
  "Schedule a content push after edits in the current buffer."
  (when (timerp my/aaronnote--preview-timer)
    (cancel-timer my/aaronnote--preview-timer))
  (setq my/aaronnote--preview-timer
        (run-with-idle-timer
         my/aaronnote-preview-delay nil
         (let ((buf (current-buffer)))
           (lambda ()
             (setq my/aaronnote--preview-timer nil)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (my/aaronnote--push-current-buffer))))))))

(defun my/aaronnote--push-current-buffer ()
  "Push the current buffer's content to the preview if it changed."
  (when my/aaronnote--ready
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (unless (string= content my/aaronnote--last-content)
        (setq my/aaronnote--last-content content)
        (my/aaronnote--send-preview content)))))

(defun my/aaronnote--install-buffer-hooks ()
  "Install per-buffer live-preview hooks."
  (add-hook 'after-change-functions #'my/aaronnote--after-change nil t)
  (add-hook 'after-save-hook        #'my/aaronnote--push-current-buffer nil t)
  (add-hook 'kill-buffer-hook       #'my/aaronnote--cleanup-buffer-hooks nil t))

(defun my/aaronnote--cleanup-buffer-hooks ()
  "Remove live-preview hooks and cancel any pending timer."
  (when (timerp my/aaronnote--preview-timer)
    (cancel-timer my/aaronnote--preview-timer))
  (setq my/aaronnote--preview-timer nil
        my/aaronnote--last-content  "")
  (remove-hook 'after-change-functions #'my/aaronnote--after-change t)
  (remove-hook 'after-save-hook        #'my/aaronnote--push-current-buffer t)
  (remove-hook 'kill-buffer-hook       #'my/aaronnote--cleanup-buffer-hooks t))

;; ── xwidget window ──────────────────────────────────────────────────────────

(defun my/aaronnote--open-xwidget ()
  "Open the Aaronote app URL in xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (require 'xwidget))
  (my/open-xwidget-url (my/aaronnote--server-url "/")))

;; ── Public commands ─────────────────────────────────────────────────────────

;;;###autoload
(defun my/aaronnote-preview ()
  "Open the Aaronote preview for the current markdown buffer."
  (interactive)
  (my/aaronnote--ensure-server
   (let ((buf (current-buffer)))
     (lambda ()
       (with-current-buffer buf
         (my/aaronnote--install-buffer-hooks)
         (my/aaronnote--open-xwidget)
         ;; Give xwidget time to load, then push initial content.
         (run-at-time 2.0 nil
                      (let ((b buf))
                        (lambda ()
                          (when (buffer-live-p b)
                            (with-current-buffer b
                              (my/aaronnote--push-current-buffer)))))))))))

;;;###autoload
(defun my/aaronnote-refresh ()
  "Force-push the current buffer's content to the preview."
  (interactive)
  (setq my/aaronnote--last-content "")   ; reset dedup so content is always sent
  (my/aaronnote--push-current-buffer))

;;;###autoload
(defun my/aaronnote-roam-graph ()
  "Ask Aaronote to show the roam graph."
  (interactive)
  (my/aaronnote--ensure-server
   (lambda () (my/aaronnote--send-command "open-roam-graph"))))

;;;###autoload
(defun my/aaronnote-stop ()
  "Kill the Aaronote web-host process."
  (interactive)
  (my/aaronnote--stop-server)
  (message "Aaronote web-host stopped."))

(provide 'init-aaronnote)
;;; init-aaronnote.el ends here
