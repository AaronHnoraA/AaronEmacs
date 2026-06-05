;;; init-aaronnote.el --- Aaronote Markdown preview for Emacs -*- lexical-binding: t; -*-
;;
;; Embeds the read-only Aaronote Markdown preview inside Emacs.
;; The bundled Node web-host serves the self-contained renderer over HTTP+SSE
;; so Appine or xwidget can load it.  Emacs drives the preview over a simple
;; HTTP control channel:
;;   POST /emacs/command {type:"preview", content}  – live markdown push
;;   POST /emacs/command {type:"reveal", line, col} – reveal source position

;;; Code:

(require 'json)
(require 'url)

(declare-function my/open-xwidget-url "init-browser" (url &optional reuse-selected))
(declare-function my/appine-open-url "init-appine" (url))
(defvar my/open-browser-window-size 0.38)

(defgroup my/aaronnote nil
  "Read-only Aaronote Markdown preview."
  :group 'applications)

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

(defcustom my/aaronnote-preview-backend 'appine
  "Backend used to display the Aaronote preview."
  :type '(choice (const :tag "Appine" appine)
                 (const :tag "xwidget-webkit" xwidget))
  :group 'my/aaronnote)

;; ── Server state ────────────────────────────────────────────────────────────

(defvar my/aaronnote--process nil   "Running web-host child process, or nil.")
(defvar my/aaronnote--port    nil   "HTTP port of the running web-host.")
(defvar my/aaronnote--ready   nil   "Non-nil once the port has been announced.")
(defvar my/aaronnote--source-buffer nil
  "Markdown source buffer currently driving the preview.")
(defvar my/aaronnote--preview-buffer nil
  "Buffer hosting the currently visible embedded preview.")
(defvar my/aaronnote--preview-close-timer nil
  "Timer used to stop the web-host after its preview window closes.")

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
    (let ((old-process my/aaronnote--process))
      (setq my/aaronnote--process nil)
      (delete-process old-process)))
  (when (buffer-live-p my/aaronnote--source-buffer)
    (with-current-buffer my/aaronnote--source-buffer
      (setq my/aaronnote--last-content "")))
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

(defun my/aaronnote--handle-process-line (line ready-callback)
  "Handle one web-host stdout LINE and optional READY-CALLBACK."
  (let ((ready-prefix "aaronote-web-host:ready:")
        (goto-prefix "aaronote-event:goto:"))
    (cond
     ((and (>= (length line) (length ready-prefix))
           (equal (substring line 0 (length ready-prefix)) ready-prefix))
      (let ((port (string-to-number (substring line (length ready-prefix)))))
        (when (> port 0)
          (setq my/aaronnote--port port
                my/aaronnote--ready t)
          (when ready-callback
            (run-at-time 0.1 nil ready-callback)))))
     ((and (>= (length line) (length goto-prefix))
           (equal (substring line 0 (length goto-prefix)) goto-prefix))
      (let* ((payload (substring line (length goto-prefix)))
             (parts (split-string payload ":" nil))
             (line-number (string-to-number (or (car parts) "0")))
             (column (string-to-number (or (cadr parts) "0"))))
        (when (and (<= 1 (length parts) 2)
                   (> line-number 0)
                   (>= column 0))
          (my/aaronnote--goto-source line-number column)))))))

(defun my/aaronnote--make-filter (ready-callback)
  "Return a process filter that handles line-oriented web-host stdout."
  (let ((pending ""))
    (lambda (proc output)
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (insert output)))
      (setq pending (concat pending output))
      (let (newline)
        (while (setq newline (string-match "\n" pending))
          (my/aaronnote--handle-process-line
           (string-trim-right (substring pending 0 newline) "\r")
           ready-callback)
          (setq pending (substring pending (1+ newline))))))))

(defun my/aaronnote--sentinel (proc event)
  "Handle web-host process state change."
  (when (and (eq proc my/aaronnote--process)
             (not (process-live-p proc)))
    (my/aaronnote--uninstall-buffer-hooks my/aaronnote--source-buffer)
    (setq my/aaronnote--process nil
          my/aaronnote--port    nil
          my/aaronnote--ready   nil
          my/aaronnote--source-buffer nil
          my/aaronnote--preview-buffer nil)
    (unless (string-match-p "^finished" event)
      (message "aaronnote-web-host: %s" (string-trim event)))))

(defun my/aaronnote--stop-server ()
  "Kill the web-host process."
  (let ((process my/aaronnote--process))
    (my/aaronnote--uninstall-buffer-hooks my/aaronnote--source-buffer)
    (setq my/aaronnote--process nil
          my/aaronnote--port nil
          my/aaronnote--ready nil
          my/aaronnote--source-buffer nil
          my/aaronnote--preview-buffer nil)
    (when (timerp my/aaronnote--preview-close-timer)
      (cancel-timer my/aaronnote--preview-close-timer))
    (setq my/aaronnote--preview-close-timer nil)
    (when (and process (process-live-p process))
      (delete-process process))))

(add-hook 'kill-emacs-hook #'my/aaronnote--stop-server)

(defun my/aaronnote--preview-buffer-killed ()
  "Stop the web-host when its embedded preview buffer is killed."
  (when (eq (current-buffer) my/aaronnote--preview-buffer)
    (my/aaronnote--stop-server)))

(defun my/aaronnote--track-preview-buffer (buffer)
  "Record BUFFER as the embedded preview host."
  (setq my/aaronnote--preview-buffer buffer)
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook #'my/aaronnote--preview-buffer-killed nil t)))

(defun my/aaronnote--preview-window-state-changed (&rest _)
  "Stop the web-host shortly after its embedded preview window disappears."
  (when (timerp my/aaronnote--preview-close-timer)
    (cancel-timer my/aaronnote--preview-close-timer))
  (setq my/aaronnote--preview-close-timer nil)
  (when (and my/aaronnote--ready
             my/aaronnote--preview-buffer
             (or (not (buffer-live-p my/aaronnote--preview-buffer))
                 (not (get-buffer-window my/aaronnote--preview-buffer t))))
    (setq my/aaronnote--preview-close-timer
          (run-at-time
           0.5 nil
           (lambda ()
             (setq my/aaronnote--preview-close-timer nil)
             (when (and my/aaronnote--ready
                        my/aaronnote--preview-buffer
                        (or (not (buffer-live-p my/aaronnote--preview-buffer))
                            (not (get-buffer-window
                                  my/aaronnote--preview-buffer t))))
               (my/aaronnote--stop-server)))))))

(add-hook 'window-state-change-functions
          #'my/aaronnote--preview-window-state-changed)

;; ── HTTP control channel ────────────────────────────────────────────────────

(defun my/aaronnote--post (endpoint payload)
  "POST JSON PAYLOAD to ENDPOINT on the web-host (fire-and-forget)."
  (when my/aaronnote--ready
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
      (url-retrieve (my/aaronnote--server-url endpoint)
                    (lambda (_status)
                      (kill-buffer (current-buffer)))
                    nil t t))))

(defun my/aaronnote--send-preview (content)
  "Push markdown CONTENT to the active Aaronote page."
  (my/aaronnote--post "/emacs/command"
                      `((type . "preview")
                        (content . ,content)
                        (file . ,(or buffer-file-name "")))))

(defun my/aaronnote--send-command (command &optional detail)
  "Dispatch COMMAND to the Aaronote UI (open-roam-graph, etc.)."
  (my/aaronnote--post "/emacs/command"
                      `((type . "command") (command . ,command)
                        ,@(when detail `((detail . ,detail))))))

(defun my/aaronnote--send-reveal (line col)
  "Reveal one-based LINE and zero-based COL in the Aaronote preview."
  (my/aaronnote--post "/emacs/command"
                      `((type . "reveal") (line . ,line) (col . ,col))))

(defun my/aaronnote--utf16-column-at-point ()
  "Return point's zero-based UTF-16 column on the current line."
  (/ (string-bytes
      (encode-coding-string
       (buffer-substring-no-properties (line-beginning-position) (point))
       'utf-16le))
     2))

(defun my/aaronnote--forward-utf16-column (col)
  "Move forward by at most COL UTF-16 code units on the current line."
  (let ((remaining (max 0 col))
        (end (line-end-position)))
    (while (and (> remaining 0) (< (point) end))
      (let ((width (if (> (char-after) #xffff) 2 1)))
        (if (< remaining width)
            (setq remaining 0)
          (forward-char 1)
          (setq remaining (- remaining width)))))))

(defun my/aaronnote--goto-source (line col)
  "Select the preview source buffer and move to one-based LINE and COL."
  (when (buffer-live-p my/aaronnote--source-buffer)
    (let* ((buffer my/aaronnote--source-buffer)
           (window (or (get-buffer-window buffer t)
                       (display-buffer buffer))))
      (when (window-live-p window)
        (select-window window))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (max 0 (1- line)))
          (my/aaronnote--forward-utf16-column col))
        (when (require 'pulse nil t)
          (pulse-momentary-highlight-one-line (point)))))))

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

(defun my/aaronnote--uninstall-buffer-hooks (buffer)
  "Remove live-preview hooks and timers from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp my/aaronnote--preview-timer)
        (cancel-timer my/aaronnote--preview-timer))
      (setq my/aaronnote--preview-timer nil
            my/aaronnote--last-content "")
      (remove-hook 'after-change-functions #'my/aaronnote--after-change t)
      (remove-hook 'after-save-hook #'my/aaronnote--push-current-buffer t)
      (remove-hook 'kill-buffer-hook #'my/aaronnote--cleanup-buffer-hooks t))))

(defun my/aaronnote--cleanup-buffer-hooks ()
  "Remove live-preview hooks and cancel any pending timer."
  (let ((source-p (eq (current-buffer) my/aaronnote--source-buffer)))
    (my/aaronnote--uninstall-buffer-hooks (current-buffer))
    (when source-p
      (my/aaronnote--stop-server))))

;; ── xwidget window ──────────────────────────────────────────────────────────

(defun my/aaronnote--open-xwidget ()
  "Open the Aaronote app URL in xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (require 'xwidget))
  (let ((editing-window (selected-window)))
    (unwind-protect
        (progn
          (my/open-xwidget-url (my/aaronnote--server-url "/"))
          (my/aaronnote--track-preview-buffer (current-buffer)))
      (when (window-live-p editing-window)
        (select-window editing-window)))))

(defun my/aaronnote--open-appine ()
  "Open the Aaronote app URL in a left-side Appine window."
  (unless (fboundp 'my/appine-open-url)
    (require 'init-appine))
  (let* ((editing-window (selected-window))
         (total-width (window-total-width editing-window))
         (buffer (get-buffer-create "*Appine Window*"))
         (preview-window (get-buffer-window buffer t)))
    (my/aaronnote--track-preview-buffer buffer)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local cursor-type nil)
      (setq buffer-read-only t))
    (unless (window-live-p preview-window)
      (setq preview-window (split-window editing-window nil 'left))
      (set-window-buffer preview-window buffer)
      (set-window-dedicated-p preview-window nil)
      (let ((target (max 20
                         (floor (* total-width my/open-browser-window-size)))))
        (ignore-errors
          (window-resize
           preview-window (- target (window-total-width preview-window)) t))))
    (unwind-protect
        (progn
          (select-window preview-window)
          (my/appine-open-url (my/aaronnote--server-url "/")))
      (when (window-live-p editing-window)
        (select-window editing-window)))))

(defun my/aaronnote--open-preview ()
  "Open the Aaronote preview using `my/aaronnote-preview-backend'."
  (pcase my/aaronnote-preview-backend
    ('appine
     (condition-case err
         (my/aaronnote--open-appine)
       (error
        (message "Aaronote: Appine unavailable, using xwidget (%s)"
                 (error-message-string err))
        (my/aaronnote--open-xwidget))))
    ('xwidget (my/aaronnote--open-xwidget))
    (_ (user-error "Unsupported Aaronote preview backend: %S"
                   my/aaronnote-preview-backend))))

;; ── Public commands ─────────────────────────────────────────────────────────

(defun my/aaronnote--activate-preview (buffer &optional after-open)
  "Show BUFFER in the preview, then call AFTER-OPEN with no arguments."
  (unless (eq buffer my/aaronnote--source-buffer)
    (my/aaronnote--uninstall-buffer-hooks my/aaronnote--source-buffer))
  (setq my/aaronnote--source-buffer buffer)
  (my/aaronnote--ensure-server
   (lambda ()
     (when (buffer-live-p buffer)
       (let ((source-window (or (get-buffer-window buffer t)
                                (display-buffer buffer))))
         (when (window-live-p source-window)
           (select-window source-window)))
       (with-current-buffer buffer
         (my/aaronnote--install-buffer-hooks)
         ;; The host retains this message and replays it to a newly loaded page.
         (my/aaronnote--push-current-buffer)
         (my/aaronnote--open-preview)
         (when after-open
           (funcall after-open)))))))

;;;###autoload
(defun my/aaronnote-preview ()
  "Open the Aaronote preview for the current markdown buffer."
  (interactive)
  (my/aaronnote--activate-preview (current-buffer)))

;;;###autoload
(defun my/aaronnote-sync-cursor ()
  "Reveal the current source position in the Aaronote preview."
  (interactive)
  (let ((buffer (current-buffer))
        (line (line-number-at-pos (point) t))
        (col (my/aaronnote--utf16-column-at-point)))
    (if (and my/aaronnote--ready
             (eq buffer my/aaronnote--source-buffer)
             (buffer-live-p my/aaronnote--preview-buffer)
             (get-buffer-window my/aaronnote--preview-buffer t))
        (my/aaronnote--send-reveal line col)
      (my/aaronnote--activate-preview
       buffer
       (lambda ()
         (my/aaronnote--send-reveal line col))))))

;;;###autoload
(defun my/aaronnote-refresh ()
  "Force-push the current buffer's content to the preview."
  (interactive)
  (if (and my/aaronnote--ready
           (eq (current-buffer) my/aaronnote--source-buffer))
      (progn
        (setq my/aaronnote--last-content "")
        (my/aaronnote--push-current-buffer))
    (my/aaronnote--activate-preview (current-buffer))))

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
