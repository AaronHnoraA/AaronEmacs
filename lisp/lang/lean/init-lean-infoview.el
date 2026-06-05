;;; init-lean-infoview.el --- Lean 4 xwidget infoview via bridge server -*- lexical-binding: t -*-

;;; Commentary:
;; Serves the official @leanprover/infoview React component via a local Node.js
;; bridge (lean4-infoview-bridge/server.mjs) and displays it in a RIGHT-SIDE
;; side window using xwidget-webkit.  Does NOT disrupt the existing buffer layout.
;; Decoupled from lsp-mode: uses eglot-managed-p and eglot--pos-to-lsp-position.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(declare-function lean-dev-log "init-lean" (format-string &rest args))
(declare-function lean--ensure-eglot "init-lean")
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-buffer "xwidget" (xwidget))
(defvar xwidget-list)
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-path-to-uri "eglot" (path))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eglot--pos-to-lsp-position "eglot" (&optional pos))

(defvar lean-info-window-width)

(defvar lean--iv--servers (make-hash-table :test #'equal)
  "Hash table: project-root → (process . port).")

(defvar lean--iv--xwidget-buffers (make-hash-table :test #'equal)
  "Hash table: project-root → visible infoview xwidget buffer.")

(defvar-local lean--iv--xwidget-buf nil
  "The xwidget buffer associated with this lean-mode source buffer.")

(defvar-local lean--iv--sync-timer nil
  "Debounce timer for syncing buffer text to the infoview bridge.")

(defvar-local lean--iv--cursor-timer nil
  "Debounce timer for syncing cursor position to the infoview bridge.")

(defvar-local lean--iv--last-cursor nil
  "Last cursor signature sent to the infoview.")

(defconst lean--iv--script-dir
  (expand-file-name "lean4-infoview-bridge"
                    (file-name-directory
                     (or load-file-name
                         buffer-file-name
                         (locate-library "init-lean-infoview")
                         user-emacs-directory)))
  "Directory containing server.mjs and the Vite dist/.")

(defcustom lean-iv-document-sync-delay 0.35
  "Seconds to debounce document sync to the infoview bridge."
  :type 'number
  :group 'lean)

(defcustom lean-iv-cursor-sync-delay 0.035
  "Seconds to debounce HTTP cursor sync to the infoview bridge.
The xwidget script path is still attempted immediately; this delay controls the
HTTP/SSE fallback used when WebKit script injection is late or unreliable."
  :type 'number
  :group 'lean)

(defun lean--iv-log (format-string &rest args)
  "Write an infoview FORMAT-STRING with ARGS to the Lean development log."
  (when (fboundp 'lean-dev-log)
    (apply #'lean-dev-log (concat "infoview: " format-string) args)))

;; ── Availability checks ───────────────────────────────────────────────────────

(defun lean-iv-node-p ()
  "Return non-nil if node and the dist bundle are present."
  (and (executable-find "node")
       (file-exists-p (expand-file-name "dist/index.html"
                                        lean--iv--script-dir))))

(defun lean-iv-xwidget-p ()
  "Return non-nil if xwidget-webkit is available in this build."
  (fboundp 'xwidget-webkit-browse-url))

(defun lean-iv-available-p ()
  "Return non-nil if the xwidget infoview can run in the current buffer."
  (and (lean-iv-xwidget-p)
       (lean-iv-node-p)
       (not (file-remote-p default-directory))))

(defun lean--iv-project-root ()
  "Return the current Lean infoview project root."
  (file-name-as-directory
   (expand-file-name
    (or (locate-dominating-file default-directory "lakefile.lean")
        (locate-dominating-file default-directory "lakefile.toml")
        (locate-dominating-file default-directory "lean-toolchain")
        default-directory))))

(defun lean--iv-source-uri ()
  "Return the file URI for the current source buffer."
  (when buffer-file-name
    (if (fboundp 'eglot-path-to-uri)
        (eglot-path-to-uri (expand-file-name buffer-file-name))
      (concat "file://" (expand-file-name buffer-file-name)))))

;; ── Bridge server management ──────────────────────────────────────────────────

(defun lean--iv-server-port (root)
  "Return port for ROOT's bridge server if the process is live, else nil."
  (when-let* ((entry (gethash root lean--iv--servers))
              (proc  (car entry))
              ((process-live-p proc)))
    (cdr entry)))

(defun lean-iv-start-server (root callback)
  "Start bridge server for ROOT; invoke CALLBACK with port when ready."
  (let* ((root    (file-name-as-directory (expand-file-name root)))
         (script  (expand-file-name "server.mjs" lean--iv--script-dir))
         (log-buf (get-buffer-create
                   (format "*Lean Infoview Log<%s>*"
                           (file-name-nondirectory (directory-file-name root)))))
         (proc (make-process
                :name    "lean-iv-server"
                :buffer  log-buf
                :stderr  log-buf
                :command (list "node" script "0" root)
                :noquery t
                :filter
                (lambda (proc string)
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-max))
                      (insert string)))
                  (lean--iv-log "bridge output: %s" (string-trim-right string))
                  ;; Port detection: accumulate until port line found
                  (unless (process-get proc 'port)
                    (let ((output (concat (or (process-get proc 'pending-output) "")
                                          string)))
                      (process-put proc 'pending-output
                                   (if (> (length output) 512)
                                       (substring output -512)
                                     output))
                      (when (string-match "LEAN_INFOVIEW_PORT=\\([0-9]+\\)" output)
                        (let ((port (string-to-number (match-string 1 output))))
                          (process-put proc 'port port)
                          (puthash root (cons proc port) lean--iv--servers)
                          (lean--iv-log
                           "bridge listening: root=%s port=%d log-buffer=%s"
                           root port (buffer-name (process-buffer proc)))
                          (funcall callback port)))))
                  ;; EMACS_CMD reverse channel: scan for complete lines
                  (let* ((prev     (or (process-get proc 'cmd-buf) ""))
                         (combined (concat prev string))
                         (lines    (split-string combined "\n"))
                         (complete (butlast lines))
                         (last-part (car (last lines))))
                    (process-put proc 'cmd-buf last-part)
                    (dolist (line complete)
                      (when (string-match "\\`EMACS_CMD=\\({.*}\\)\\'" line)
                        (let ((json-str (match-string 1 line)))
                          (condition-case err
                              (let ((cmd (json-parse-string
                                          json-str
                                          :object-type 'plist
                                          :array-type  'vector
                                          :null-object  nil
                                          :false-object nil)))
                                (lean--iv-dispatch-emacs-cmd proc cmd))
                            (error
                             (lean--iv-log "EMACS_CMD parse error: %S input=%s"
                                           err json-str))))))))
                :sentinel
                (lambda (proc event)
                  (lean--iv-log "bridge sentinel: event=%s status=%S"
                                (string-trim event)
                                (process-status proc))
                  (unless (process-live-p proc)
                    (remhash root lean--iv--servers))))))
    (lean--iv-log "starting bridge: command=%S" (process-command proc))
    (process-put proc 'root root)
    proc))

(defun lean-iv-ensure-server (root callback)
  "Ensure bridge server is running for ROOT; call CALLBACK with port."
  (if-let* ((port (lean--iv-server-port root)))
      (funcall callback port)
    (lean-iv-start-server root callback)))

(defun lean-iv-stop-server (root)
  "Kill the bridge server for ROOT."
  (lean--iv-log "stopping bridge: root=%s" root)
  (when-let* ((entry (gethash root lean--iv--servers))
              (proc  (car entry))
              ((process-live-p proc)))
    (delete-process proc))
  (remhash root lean--iv--servers)
  (remhash root lean--iv--xwidget-buffers))

(defun lean--iv-server-proc ()
  "Return the live bridge process for the current Lean buffer's root, or nil."
  (when-let* ((root  (lean--iv-project-root))
              (entry (gethash root lean--iv--servers))
              (proc  (car entry))
              ((process-live-p proc)))
    proc))

;; ── Xwidget buffer creation ───────────────────────────────────────────────────

(defun lean--iv-buf-name (source)
  "Return the xwidget buffer name for SOURCE."
  (format "*Lean Infoview<%s>*" (buffer-name source)))

(defun lean--iv-xwidget-of (buf)
  "Return the xwidget object inside BUF, or nil."
  (when (buffer-live-p buf)
    (or (with-current-buffer buf
          (when (fboundp 'xwidget-webkit-current-session)
            (ignore-errors (xwidget-webkit-current-session))))
        (with-current-buffer buf
          (when (fboundp 'xwidget-at)
            (ignore-errors
              (or (xwidget-at (point-min))
                  (xwidget-at (point-max))))))
        (when (and (boundp 'xwidget-list)
                   (fboundp 'xwidget-buffer))
          (seq-find (lambda (xw)
                      (eq (ignore-errors (xwidget-buffer xw)) buf))
                    xwidget-list)))))

(defun lean--iv-project-xwidget-buf (&optional root)
  "Return the live project infoview xwidget buffer for ROOT, or nil."
  (let* ((root (or root (lean--iv-project-root)))
         (buf  (and root (gethash root lean--iv--xwidget-buffers))))
    (when (and (buffer-live-p buf)
               (lean--iv-xwidget-of buf))
      buf)))

(defun lean--iv-make-xwidget-buf (url name)
  "Browse URL in a new xwidget-webkit session; rename buffer to NAME and return it.
`xwidget-webkit-browse-url' creates its own buffer (e.g. `*xwidget*') via
`switch-to-buffer', so we capture the selected window's buffer INSIDE
`save-window-excursion' (before windows are restored) then rename it."
  (let (xbuf)
    (save-window-excursion
      (xwidget-webkit-browse-url url t)
      ;; After browse-url, the selected window shows the new xwidget buffer.
      (setq xbuf (window-buffer (selected-window))))
    (when (buffer-live-p xbuf)
      (with-current-buffer xbuf
        (rename-buffer name t)))
    xbuf))

(defun lean-iv-open-infoview (source port)
  "Display the infoview xwidget for SOURCE buffer at PORT in a side window."
  (let* ((root  (with-current-buffer source
                  (lean--iv-project-root)))
         (url   (format "http://127.0.0.1:%d/" port))
         (name  (format "*Lean Infoview<%s>*"
                        (file-name-nondirectory
                         (directory-file-name root))))
         (xbuf  (or (when-let* ((existing (get-buffer name))
                                ((lean--iv-xwidget-of existing)))
                      existing)
                    (lean--iv-project-xwidget-buf root)
                    (lean--iv-make-xwidget-buf url name))))
    (with-current-buffer source
      (setq lean--iv--xwidget-buf xbuf))
    (puthash root xbuf lean--iv--xwidget-buffers)
    ;; Always display via side window — never replaces an existing editing window.
    (display-buffer-in-side-window
     xbuf
     `((side . right)
       (slot . 1)
       (window-width . ,(if (boundp 'lean-info-window-width)
                            lean-info-window-width
                          84))
       (dedicated . t)
       (window-parameters . ((no-other-window . t)))))))

;; ── Cursor sync ───────────────────────────────────────────────────────────────

(defun lean--iv-live-xw ()
  "Return the live xwidget for the current source buffer's infoview, or nil."
  (when-let* ((buf (or (and (boundp 'lean--iv--xwidget-buf)
                            lean--iv--xwidget-buf)
                       (lean--iv-project-xwidget-buf)))
              ((buffer-live-p buf))
              ((get-buffer-window buf t)))
    (lean--iv-xwidget-of buf)))

(defun lean--iv-active-p ()
  "Return non-nil when the current buffer has an active infoview xwidget."
  (let ((buf (or (and (boundp 'lean--iv--xwidget-buf)
                      lean--iv--xwidget-buf)
                 (lean--iv-project-xwidget-buf))))
    (and (buffer-live-p buf)
         (get-buffer-window buf t))))

(defun lean--iv-current-lsp-position ()
  "Return (line . char) at point as LSP 0-based integers, or nil."
  (or (when (fboundp 'eglot--pos-to-lsp-position)
        (ignore-errors
          (when-let* ((pos (eglot--pos-to-lsp-position)))
            (cons (plist-get pos :line) (plist-get pos :character)))))
      (cons (1- (line-number-at-pos)) (current-column))))

(defun lean--iv-cancel-cursor-timer ()
  "Cancel pending cursor sync for the current buffer."
  (when (timerp lean--iv--cursor-timer)
    (cancel-timer lean--iv--cursor-timer))
  (setq lean--iv--cursor-timer nil))

(defun lean--iv-schedule-cursor-post (port uri line character)
  "Debounce a cursor POST to PORT for URI at LINE and CHARACTER."
  (lean--iv-cancel-cursor-timer)
  (let ((buf (current-buffer)))
    (setq lean--iv--cursor-timer
          (run-at-time
           lean-iv-cursor-sync-delay nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq lean--iv--cursor-timer nil)
                 (when (lean--iv-active-p)
                   (lean--iv-post-cursor port uri nil line character)))))))))

(defun lean-iv-sync-cursor-h ()
  "Push cursor position to the infoview xwidget (post-command hook).
Also updates the bridge process's `active-buf' property so the reverse
channel knows which buffer to target for insertText / applyEdit.
Works regardless of whether eglot is currently managing the buffer,
so the infoview follows the cursor as soon as it is visible."
  (when (and (derived-mode-p 'lean-mode)
             (buffer-file-name))
    ;; Track active buffer for the reverse channel
    (when-let* ((proc (lean--iv-server-proc)))
      (process-put proc 'active-buf (current-buffer)))
    ;; Push cursor position into the xwidget infoview.  The WebKit script path
    ;; is low latency; the HTTP/SSE path is a reliable fallback and also works
    ;; when the page has not yet created window.updateCursor.
    (when-let* ((uri  (lean--iv-source-uri))
                (lc   (lean--iv-current-lsp-position)))
      (let ((sig (list uri (car lc) (cdr lc))))
        (unless (equal sig lean--iv--last-cursor)
          (setq lean--iv--last-cursor sig)
          (lean--iv-log "cursor sync: uri=%s line=%d char=%d"
                        uri (car lc) (cdr lc))
          (when-let* ((xw (lean--iv-live-xw)))
            (condition-case err
                (xwidget-webkit-execute-script
                 xw
                 (format "window.updateCursor && window.updateCursor(%s,%d,%d);"
                         (json-encode uri) (car lc) (cdr lc)))
              (error
               (lean--iv-log "cursor sync script error: %S" err))))
          (when-let* ((port (lean--iv-server-port (lean--iv-project-root))))
            (lean--iv-schedule-cursor-post port uri (car lc) (cdr lc))))))))

;; ── HTTP cursor POST (fire-and-forget, opens doc in bridge if needed) ─────────

(defun lean--iv-post-cursor (port uri &optional text line character)
  "POST /cursor to bridge at PORT for URI.
When TEXT is a string, sync it as the full document contents.  When LINE and
CHARACTER are numbers, also update the frontend cursor location."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode
           (append `(:uri ,uri)
                   (when (stringp text)
                     `(:text ,text))
                   (when (numberp line)
                     `(:line ,line))
                   (when (numberp character)
                     `(:character ,character))))
          'utf-8)))
    (lean--iv-log "cursor/document POST: port=%d uri=%s line=%S char=%S text-bytes=%s"
                  port uri line character (and (stringp text) (string-bytes text)))
    (url-retrieve (format "http://127.0.0.1:%d/cursor" port)
                  (lambda (status)
                    (when-let* ((err (plist-get status :error)))
                      (lean--iv-log "cursor/document POST error: %S" err))
                    (when (buffer-live-p (current-buffer))
                      (kill-buffer (current-buffer))))
                  nil t t)))

(defun lean-iv-sync-document-now ()
  "Sync the current source buffer text to the infoview bridge."
  (when-let* ((port (lean--iv-server-port (lean--iv-project-root)))
              (uri  (lean--iv-source-uri)))
    (let ((lc (lean--iv-current-lsp-position)))
      (lean--iv-post-cursor
       port uri
       (buffer-substring-no-properties (point-min) (point-max))
       (and lc (car lc))
       (and lc (cdr lc))))))

(defun lean-iv-schedule-document-sync (&rest _)
  "Debounce document sync to the infoview bridge after edits."
  (when (and (derived-mode-p 'lean-mode)
             (lean--iv-active-p))
    (when (timerp lean--iv--sync-timer)
      (cancel-timer lean--iv--sync-timer))
    (let ((buf (current-buffer)))
      (setq lean--iv--sync-timer
            (run-at-time
             lean-iv-document-sync-delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq lean--iv--sync-timer nil)
                   (lean-iv-sync-document-now)))))))))

(defun lean-iv-setup-buffer-sync ()
  "Install buffer-local document sync hooks for the Lean infoview bridge."
  (add-hook 'after-change-functions #'lean-iv-schedule-document-sync nil t))

(defun lean--iv-open-current-buffer ()
  "Start the bridge if needed and open the infoview for the current buffer.
The xwidget is opened immediately once the bridge port is known; the
infoview React component waits for the `lsp:ready' SSE event internally."
  (let* ((source (current-buffer))
         (root   (lean--iv-project-root)))
    (message "Opening Lean infoview…")
    (lean--iv-log "open requested: buffer=%s root=%s" (buffer-name source) root)
    (lean-iv-ensure-server
     root
     (lambda (port)
       (when (buffer-live-p source)
         (with-current-buffer source
           (when-let* ((uri (lean--iv-source-uri)))
             (lean--iv-post-cursor
              port uri (buffer-substring-no-properties (point-min) (point-max))))
           (setq lean--iv--last-cursor nil)
           (lean-iv-open-infoview source port)
           (lean--iv-log "xwidget opened: buffer=%s port=%d"
                         (buffer-name source) port)
           (message "Lean infoview ready (port %d)" port)
           (ignore-errors (lean-iv-sync-cursor-h))
           (dolist (delay '(0.5 1.5 3.0))
             (run-at-time
              delay nil
              (let ((buf source)
                    (d delay))
                (lambda ()
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (lean--iv-log
                       "sync cursor after xwidget open (delay %s)" d)
                      (ignore-errors (lean-iv-sync-cursor-h))))))))))))))

;; ── Toggle ────────────────────────────────────────────────────────────────────

(defun lean-iv-toggle ()
  "Open or close the Lean xwidget infoview for the current buffer.
This is the single Lean infoview entry point bound to \\[lean-iv-toggle]."
  (interactive)
  (unless (derived-mode-p 'lean-mode)
    (user-error "Must be called from a Lean source buffer"))
  (unless (lean-iv-node-p)
    (user-error "lean4-infoview-bridge/dist/ not built — run `npm run build` there"))
  (unless (lean-iv-xwidget-p)
    (user-error "Emacs was built without xwidget-webkit support"))
  (let ((xbuf (and (boundp 'lean--iv--xwidget-buf)
                   lean--iv--xwidget-buf)))
    (cond
     ((and (buffer-live-p xbuf) (get-buffer-window xbuf))
      (lean--iv-log "closing visible infoview: buffer=%s" (buffer-name xbuf))
      (delete-windows-on xbuf))
     (t
      (when (fboundp 'lean--ensure-eglot)
        (lean--ensure-eglot))
      (lean--iv-open-current-buffer)))))

;; ── Editor reverse channel (infoview → Emacs) ────────────────────────────────
;; The bridge server emits `EMACS_CMD={...}\n' lines to stdout.
;; The process filter in `lean-iv-start-server' scans for them and dispatches
;; here.  Each handler runs in the Emacs event loop (no concurrency issues).

(defun lean--iv-uri-to-path (uri)
  "Convert a file:// URI to an absolute local path, or nil."
  (condition-case nil
      (when (fboundp 'eglot-uri-to-path)
        (eglot-uri-to-path uri))
    (error
     (when (string-prefix-p "file://" uri)
       (decode-coding-string
        (url-unhex-string (substring uri 7)) 'utf-8)))))

(defun lean--iv-show-document (cmd)
  "Open the document specified by CMD in a non-side editing window.
CMD is a plist with :uri and optionally :selection ({:start {:line :character}})."
  (when-let* ((uri  (plist-get cmd :uri))
              (path (lean--iv-uri-to-path uri)))
    (let* ((sel   (plist-get cmd :selection))
           (start (and sel (plist-get sel :start)))
           (line  (and start (plist-get start :line)))
           (char  (and start (plist-get start :character)))
           (buf   (find-file-noselect path)))
      (pop-to-buffer buf '((display-buffer-reuse-window
                            display-buffer-use-some-window)
                           (inhibit-same-window . nil)))
      (with-current-buffer buf
        (when (and line (numberp line))
          (goto-char (point-min))
          (forward-line line)
          (when (and char (numberp char))
            (forward-char (min char (- (line-end-position) (point)))))
          (recenter nil t)))
      (lean--iv-log "show-document: uri=%s line=%S char=%S" uri line char))))

(defun lean--iv-insert-text (proc cmd)
  "Insert text from CMD into the active source buffer for PROC.
CMD is a plist with :text and optionally :pos ({:line :character})."
  (when-let* ((text (plist-get cmd :text))
              (buf  (process-get proc 'active-buf))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (let* ((pos  (plist-get cmd :pos))
             (line (and pos (plist-get pos :line)))
             (char (and pos (plist-get pos :character))))
        (if (and line (numberp line))
            (save-excursion
              (goto-char (point-min))
              (forward-line line)
              (when (and char (numberp char))
                (forward-char (min char (- (line-end-position) (point)))))
              (insert text))
          (insert text))))
    (lean--iv-log "insert-text: kind=%S bytes=%d" (plist-get cmd :kind) (string-bytes text))))

(defun lean--iv-apply-edit (proc cmd)
  "Apply text edits from CMD to the active source buffer for PROC.
CMD is a plist with :edits (vector of {range:{start,end}, newText})."
  (when-let* ((edits (plist-get cmd :edits))
              (buf   (process-get proc 'active-buf))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      ;; Convert vector to list; sort bottom-to-top so earlier offsets stay valid
      (let* ((elist  (if (vectorp edits) (append edits nil) edits))
             (sorted (sort elist
                           (lambda (a b)
                             (let ((la (or (plist-get (plist-get (plist-get a :range) :start) :line) 0))
                                   (lb (or (plist-get (plist-get (plist-get b :range) :start) :line) 0)))
                               (> la lb))))))
        (dolist (edit sorted)
          (when-let* ((range  (plist-get edit :range))
                      (rstart (plist-get range :start))
                      (rend   (plist-get range :end)))
            (let* ((new-text (or (plist-get edit :newText) ""))
                   (s-line (or (plist-get rstart :line) 0))
                   (s-char (or (plist-get rstart :character) 0))
                   (e-line (or (plist-get rend :line) 0))
                   (e-char (or (plist-get rend :character) 0)))
              (save-excursion
                (goto-char (point-min))
                (forward-line s-line)
                (let ((s-pos (+ (point) s-char)))
                  (goto-char (point-min))
                  (forward-line e-line)
                  (let ((e-pos (+ (point) e-char)))
                    (delete-region s-pos e-pos)
                    (goto-char s-pos)
                    (insert new-text)))))))))
    (lean--iv-log "apply-edit: %d edits" (if (vectorp edits) (length edits) (length edits)))))

(defun lean--iv-restart-file (cmd)
  "Restart Lean's processing of the file specified in CMD."
  (when-let* ((uri  (plist-get cmd :uri))
              (path (lean--iv-uri-to-path uri))
              (buf  (find-buffer-visiting path)))
    (with-current-buffer buf
      (when (fboundp 'lean-refresh-file-dependencies)
        (lean-refresh-file-dependencies)))
    (lean--iv-log "restart-file: uri=%s" uri)))

(defun lean--iv-dispatch-emacs-cmd (proc cmd)
  "Dispatch an infoview reverse-channel command CMD from bridge process PROC.
CMD is a plist parsed from a `EMACS_CMD={...}' stdout line."
  (let ((command (plist-get cmd :cmd)))
    (lean--iv-log "emacs-cmd dispatch: cmd=%s" command)
    (cond
     ((equal command "show-document") (lean--iv-show-document cmd))
     ((equal command "insert-text")   (lean--iv-insert-text proc cmd))
     ((equal command "apply-edit")    (lean--iv-apply-edit proc cmd))
     ((equal command "restart-file")  (lean--iv-restart-file cmd))
     (t (lean--iv-log "emacs-cmd unknown: %s" command)))))

;; ── Teardown ──────────────────────────────────────────────────────────────────

(defun lean-iv-teardown-h ()
  "Kill the xwidget infoview when a lean-mode buffer is killed."
  (when (timerp lean--iv--sync-timer)
    (cancel-timer lean--iv--sync-timer)
    (setq lean--iv--sync-timer nil))
  (lean--iv-cancel-cursor-timer)
  (remove-hook 'after-change-functions #'lean-iv-schedule-document-sync t)
  (when (derived-mode-p 'lean-mode)
    (when-let* ((xbuf (and (boundp 'lean--iv--xwidget-buf)
                           lean--iv--xwidget-buf))
                ((buffer-live-p xbuf)))
      (let ((root (lean--iv-project-root)))
        (setq lean--iv--xwidget-buf nil)
        ;; Only kill if no other live Lean buffer from this project can keep
        ;; using the project-level infoview.
        (unless (seq-find (lambda (b)
                            (and (not (eq b (current-buffer)))
                                 (buffer-live-p b)
                                 (with-current-buffer b
                                   (and (derived-mode-p 'lean-mode)
                                        (equal (lean--iv-project-root) root)))))
                          (buffer-list))
          (when (eq (gethash root lean--iv--xwidget-buffers) xbuf)
            (remhash root lean--iv--xwidget-buffers))
          (kill-buffer xbuf))))))

(provide 'init-lean-infoview)
;;; init-lean-infoview.el ends here
