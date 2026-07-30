;;; init-lean-infoview.el --- Lean 4 xwidget infoview via transparent LSP proxy -*- lexical-binding: t -*-

;;; Commentary:
;; Displays the official @leanprover/infoview React component in a RIGHT-SIDE
;; side window using xwidget-webkit.  The infoview runs inside lean-proxy.mjs,
;; which is Eglot's server contact — a transparent JSON-RPC passthrough to
;; `lake serve' that also serves the infoview over HTTP+SSE.  Emacs only sends
;; cursor position; document sync is handled entirely by Eglot through the proxy.
;;
;; Reverse channel (infoview → Emacs) uses standard LSP:
;;   window/showDocument   → handled natively by Eglot
;;   workspace/applyEdit   → handled natively by Eglot
;;   lean/restartFile      → custom notification handled in init-lean-eglot.el

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)

(declare-function lean-dev-log "init-lean" (format-string &rest args))
(declare-function lean-project-root "init-lean" ())
(declare-function lean--ensure-eglot "init-lean")
(declare-function lean--proxy-gateway-binding "init-lean" (root))
(declare-function lean--proxy-gateway-client "init-lean" (root))
(declare-function lean--proxy-endpoint "init-lean" (root))
(declare-function lean--proxy-forget-gateway-binding "init-lean" (root))
(declare-function lean--proxy-available-p "init-lean")
(declare-function lean--proxy-node-command "init-lean" (root))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script))
(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-buffer "xwidget" (xwidget))
(defvar xwidget-list)
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-path-to-uri "eglot" (path))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eglot--pos-to-lsp-position "eglot" (&optional pos))
(declare-function eglot-reconnect "eglot" (server &optional interactive))
(declare-function remote-close-channel "remote-channel" (channel))
(declare-function remote-port-forward "remote-channel" (remote-endpoint &rest keys))
(declare-function remote-forward-handle "remote-backend-core" (forward))
(declare-function remote-forward-local-endpoint "remote-backend-core" (forward))
(declare-function remote-forward-remote-endpoint "remote-backend-core" (forward))
(declare-function remote-forward-state "remote-backend-core" (forward))

(defvar lean-info-window-width)

(defconst lean--iv--script-dir
  (expand-file-name "lean4-infoview-bridge"
                    (file-name-directory
                     (or load-file-name
                         buffer-file-name
                         (locate-library "init-lean-infoview")
                         user-emacs-directory)))
  "Directory containing lean-proxy.mjs and the Vite dist/.")

(defvar lean--iv--xwidget-buffers (make-hash-table :test #'equal)
  "Hash table: project-root → live infoview xwidget buffer.")

(defvar lean--iv--remote-forwards (make-hash-table :test #'equal)
  "Hash table: remote project root → live proxy port forward.")

(defvar-local lean--iv--xwidget-buf nil
  "The xwidget buffer associated with this lean-mode source buffer.")

(defvar-local lean--iv--cursor-timer nil
  "Debounce timer for syncing cursor position to the infoview.")

(defvar-local lean--iv--port-wait-timer nil
  "Current timer waiting for this buffer's Eglot proxy endpoint.")

(defvar-local lean--iv--last-cursor nil
  "Last cursor signature sent to the infoview.")

(config-defvar lean-iv-cursor-sync-delay nil
  "Seconds to debounce HTTP cursor sync to the infoview proxy."
  :type 'number
  :group 'lean)

(config-defvar lean-iv-port-wait-timeout nil
  "Seconds to wait for the proxy to register with the Emacs gateway."
  :type 'integer
  :group 'lean)

(config-defvar lean-iv-font-size nil
  "Base font size in pixels for the Lean infoview."
  :type 'integer
  :group 'lean)

(defun lean--iv-log (format-string &rest args)
  "Write an infoview FORMAT-STRING with ARGS to the Lean development log."
  (when (fboundp 'lean-dev-log)
    (apply #'lean-dev-log (concat "infoview: " format-string) args)))

;; ── Emacs theme bridge ───────────────────────────────────────────────────────

(defun lean--iv-face-color (face attribute fallback)
  "Return FACE ATTRIBUTE as a CSS color, or FALLBACK."
  (let ((value (and (facep face)
                    (face-attribute face attribute nil t))))
    (if (and (stringp value)
             (not (string-empty-p value))
             (not (string-prefix-p "unspecified" value)))
        value
      fallback)))

(defun lean--iv-font-family ()
  "Return the current Emacs code font family for CSS."
  (let ((family (face-attribute 'default :family nil t)))
    (if (and (stringp family)
             (not (string-empty-p family))
             (not (member family '("default" "unspecified"))))
        family
      "monospace")))

(defun lean--iv-theme-data ()
  "Return current Emacs typography and face colors for the infoview."
  `(("mode" . ,(if (eq (frame-parameter nil 'background-mode) 'light)
                   "light"
                 "dark"))
    ("fontFamily" . ,(lean--iv-font-family))
    ("fontSize" . ,(number-to-string lean-iv-font-size))
    ("bg" . ,(lean--iv-face-color 'default :background "#1f1f28"))
    ("fg" . ,(lean--iv-face-color 'default :foreground "#dcd7ba"))
    ("surface" . ,(lean--iv-face-color 'mode-line-inactive :background "#16161d"))
    ("surfaceRaised" . ,(lean--iv-face-color 'mode-line :background "#2a2a37"))
    ("border" . ,(lean--iv-face-color 'vertical-border :foreground "#363646"))
    ("muted" . ,(lean--iv-face-color 'font-lock-comment-face :foreground "#727169"))
    ("accent" . ,(lean--iv-face-color 'font-lock-function-name-face :foreground "#7e9cd8"))
    ("cyan" . ,(lean--iv-face-color 'font-lock-type-face :foreground "#7aa89f"))
    ("green" . ,(lean--iv-face-color 'success :foreground "#98bb6c"))
    ("yellow" . ,(lean--iv-face-color 'warning :foreground "#e6c384"))
    ("red" . ,(lean--iv-face-color 'error :foreground "#e46876"))
    ("selection" . ,(lean--iv-face-color 'region :background "#2d4f67"))))

(defun lean--iv-theme-query ()
  "Return current infoview theme as a URL query string."
  (mapconcat
   (lambda (entry)
     (format "%s=%s"
             (url-hexify-string (car entry))
             (url-hexify-string (cdr entry))))
   (lean--iv-theme-data)
   "&"))

(defun lean--iv-theme-script ()
  "Return JavaScript that applies the current Emacs theme."
  (format "window.applyEmacsTheme && window.applyEmacsTheme(%s);"
          (json-encode (lean--iv-theme-data))))

(defun lean-iv-sync-theme-h ()
  "Apply the current Emacs theme to every live Lean infoview."
  (maphash
   (lambda (_root buf)
     (when-let* (((buffer-live-p buf))
                 (xw (lean--iv-xwidget-of buf)))
       (ignore-errors
         (xwidget-webkit-execute-script xw (lean--iv-theme-script)))))
   lean--iv--xwidget-buffers))

(add-hook 'after-load-theme-hook #'lean-iv-sync-theme-h)

;; ── Availability checks ───────────────────────────────────────────────────────

(defun lean-iv-node-p ()
  "Return non-nil if the proxy bundle and required Node runtime are present."
  (and (file-exists-p
        (expand-file-name "dist/index.html" lean--iv--script-dir))
       (lean--proxy-node-command default-directory)))

(defun lean-iv-xwidget-p ()
  "Return non-nil if this frame can display a native xwidget-webkit."
  (and (display-graphic-p)
       (require 'xwidget nil t)
       (featurep 'xwidget-internal)
       (fboundp 'xwidget-webkit-browse-url)))

(defun lean-iv-available-p ()
  "Return non-nil if the xwidget infoview can run in the current buffer."
  (and (lean-iv-xwidget-p)
       (lean-iv-node-p)))

(defun lean--iv-project-root ()
  "Return the current Lean infoview project root."
  ;; Eglot's contact and the infoview must key proxy state by exactly the same
  ;; logical identity.  Recomputing the root here used to turn a local
  ;; `/fs:local:' root back into a native path, so the proxy wrote one
  ;; instance port file while `C-c C-i' waited on another pending file.
  (file-name-as-directory (lean-project-root)))

(defun lean--iv-source-uri ()
  "Return the file URI for the current source buffer."
  (when buffer-file-name
    (if (fboundp 'eglot-path-to-uri)
        (eglot-path-to-uri (expand-file-name buffer-file-name))
      (concat "file://" (expand-file-name buffer-file-name)))))

;; ── Gateway endpoint discovery ───────────────────────────────────────────────

(defun lean--iv-close-remote-forward (root)
  "Close and forget ROOT's remote infoview forward."
  (when-let* ((forward (gethash root lean--iv--remote-forwards)))
    (remhash root lean--iv--remote-forwards)
    (ignore-errors (remote-close-channel forward))))

(defun lean--iv-remote-forward-live-p (forward remote-port)
  "Return non-nil when FORWARD still reaches REMOTE-PORT."
  (and forward
       (eq (remote-forward-state forward) 'open)
       (equal
        (plist-get (remote-forward-remote-endpoint forward) :port)
        remote-port)
       (when-let* ((process (remote-forward-handle forward)))
         (process-live-p process))))

(defun lean--iv-proxy-port (root)
  "Return ROOT's live client-local infoview HTTP port."
  (when-let* ((endpoint (lean--proxy-endpoint root))
              (port
               (if (hash-table-p endpoint)
                   (gethash "port" endpoint)
                 (alist-get "port" endpoint nil nil #'string=)))
              ((integerp port))
              ((> port 0)))
    port))

(defun lean--iv-cancel-port-wait ()
  "Cancel the current buffer's pending Infoview endpoint wait."
  (when (timerp lean--iv--port-wait-timer)
    (cancel-timer lean--iv--port-wait-timer))
  (setq lean--iv--port-wait-timer nil))

(defun lean--iv-wait-status ()
  "Return a short user-facing status for Infoview proxy startup."
  (cond
   ((bound-and-true-p lean--eglot-waiting-for-environment)
    "waiting for remote direnv")
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
    "Eglot ready; waiting for local Node proxy")
   (t "starting target Lean LSP")))

(defun lean--iv-wait-for-port (root callback)
  "Wait for ROOT's gateway peer and call CALLBACK with its HTTP port."
  (lean--iv-cancel-port-wait)
  (let ((deadline (+ (float-time) lean-iv-port-wait-timeout))
        (source-buf (current-buffer))
        (poll-count 0)
        timer)
    (cl-labels
        ((finish ()
           (when (timerp timer)
             (cancel-timer timer)
             (setq timer nil))
           (when (buffer-live-p source-buf)
             (with-current-buffer source-buf
               (setq lean--iv--port-wait-timer nil))))
         (poll ()
           (cl-incf poll-count)
           (let ((port (lean--iv-proxy-port root)))
             (cond
              ((not (buffer-live-p source-buf))
               (finish)
               t)
              (port
               (finish)
               (with-current-buffer source-buf
                 (condition-case error
                     (funcall callback port)
                   (error
                    (lean--iv-log "proxy endpoint setup failed: %s"
                                  (error-message-string error))
                    (message "Lean infoview: proxy endpoint failed: %s"
                             (error-message-string error)))))
               t)
              ((> (float-time) deadline)
               (finish)
               (lean--iv-log
                "timed out waiting for gateway peer: root=%s status=%s"
                root (with-current-buffer source-buf (lean--iv-wait-status)))
               (message
                "Lean infoview: timed out (%s); see *Lean Dev Log* and Eglot stderr"
                (with-current-buffer source-buf (lean--iv-wait-status)))
               t)
              (t
               (when (or (= poll-count 1)
                         (zerop (% poll-count 10)))
                 (with-current-buffer source-buf
                   (message "Lean infoview: %s…"
                            (lean--iv-wait-status))))
               nil)))))
      (unless (poll)
        (lean--iv-log "waiting for Lean gateway peer (root=%s)" root)
        (setq timer (run-at-time 0.5 0.5 #'poll))
        (setq lean--iv--port-wait-timer timer)))))

;; ── Xwidget buffer management ─────────────────────────────────────────────────

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
  "Browse URL in a new xwidget-webkit session; rename buffer to NAME."
  (let (xbuf)
    (save-window-excursion
      (xwidget-webkit-browse-url url t)
      (setq xbuf (window-buffer (selected-window))))
    (when (buffer-live-p xbuf)
      (with-current-buffer xbuf
        (rename-buffer name t)))
    xbuf))

(defun lean-iv-open-infoview (source port)
  "Display the infoview xwidget for SOURCE buffer at PORT in a side window."
  (let* ((root  (with-current-buffer source
                  (lean--iv-project-root)))
         (url   (format "http://127.0.0.1:%d/?%s" port (lean--iv-theme-query)))
         (name  (format "*Lean Infoview<%s>*"
                        (file-name-nondirectory
                         (directory-file-name root))))
         (existing (or (when-let* ((buf (get-buffer name))
                                   ((lean--iv-xwidget-of buf)))
                         buf)
                       (lean--iv-project-xwidget-buf root)))
         (xbuf  (or existing
                    (lean--iv-make-xwidget-buf url name))))
    (with-current-buffer source
      (setq lean--iv--xwidget-buf xbuf))
    (puthash root xbuf lean--iv--xwidget-buffers)
    (when-let* ((xw (lean--iv-xwidget-of xbuf)))
      (ignore-errors
        (if existing
            (xwidget-webkit-goto-uri xw url)
          (xwidget-webkit-execute-script xw (lean--iv-theme-script)))))
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
  "Return non-nil when the current buffer has a visible infoview xwidget."
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

(defun lean--iv-send-cursor (root uri line character)
  "Send ROOT's cursor position through the shared gateway."
  (when-let* ((client (lean--proxy-gateway-client root)))
    (condition-case error
        (remote-gateway-notify
         client "lean.cursor"
         `(("uri" . ,uri)
           ("line" . ,line)
           ("character" . ,character)))
      (error
       (lean--iv-log "gateway cursor error: %s"
                     (error-message-string error))))))

(defun lean--iv-schedule-cursor-post (root uri line character)
  "Debounce a gateway cursor notification for ROOT."
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
                   (lean--iv-send-cursor root uri line character)))))))))

(defun lean-iv-sync-cursor-h ()
  "Push cursor position to the infoview xwidget (post-command hook).
Uses a fast xwidget-webkit-execute-script path and an HTTP debounce fallback."
  (when (and (derived-mode-p 'lean-mode)
             (buffer-file-name))
    (when-let* ((uri  (lean--iv-source-uri))
                (lc   (lean--iv-current-lsp-position)))
      (let ((sig (list uri (car lc) (cdr lc))))
        (unless (equal sig lean--iv--last-cursor)
          (setq lean--iv--last-cursor sig)
          (lean--iv-log "cursor sync: uri=%s line=%d char=%d"
                        uri (car lc) (cdr lc))
          ;; Fast path: direct JS injection into the xwidget
          (when-let* ((xw (lean--iv-live-xw)))
            (condition-case err
                (xwidget-webkit-execute-script
                 xw
                 (format "window.updateCursor && window.updateCursor(%s,%d,%d);"
                         (json-encode uri) (car lc) (cdr lc)))
              (error
               (lean--iv-log "cursor sync script error: %S" err))))
          ;; Gateway fallback (debounced).
          (when-let* ((root (lean--iv-project-root))
                      ((lean--proxy-gateway-client root)))
            (lean--iv-schedule-cursor-post
             root uri (car lc) (cdr lc))))))))

;; ── Open infoview ─────────────────────────────────────────────────────────────

(defun lean--iv-open-current-buffer ()
  "Wait for the proxy port then open the infoview for the current buffer."
  (let* ((source (current-buffer))
         (root   (lean--iv-project-root)))
    (message "Opening Lean infoview…")
    (lean--iv-log "open requested: buffer=%s root=%s" (buffer-name source) root)
    (lean--iv-wait-for-port
     root
     (lambda (port)
       (when (buffer-live-p source)
         (with-current-buffer source
           (setq lean--iv--last-cursor nil)
           (lean-iv-open-infoview source port)
           (lean--iv-log "xwidget opened: buffer=%s port=%d"
                         (buffer-name source) port)
           (message "Lean infoview ready (port %d)" port)
           (ignore-errors (lean-iv-sync-cursor-h))
           (dolist (delay '(0.5 1.5 3.0))
             (run-at-time
              delay nil
              (let ((buf source) (d delay))
                (lambda ()
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (lean--iv-log "sync cursor after open (delay %s)" d)
                      (ignore-errors (lean-iv-sync-cursor-h))))))))))))))

;; ── Toggle / restart ──────────────────────────────────────────────────────────

(defun lean--iv-reconnect-eglot ()
  "Reconnect the current Eglot server using its interactive server lookup."
  (call-interactively #'eglot-reconnect))

(defun lean-iv-toggle ()
  "Open or close the Lean xwidget infoview for the current buffer."
  (interactive)
  ;; This command may need direnv, Eglot and Lean initialization.  Make the
  ;; key binding observably responsive before any of those asynchronous
  ;; boundaries run.
  (message "Lean infoview: checking local proxy and target LSP…")
  (redisplay)
  (unless (derived-mode-p 'lean-mode)
    (user-error "Must be called from a Lean source buffer"))
  (unless (lean-iv-node-p)
    (user-error "lean4-infoview-bridge/dist/ not built — run `npm run build` there"))
  (unless (lean-iv-xwidget-p)
    (user-error
     "Lean infoview needs a graphical frame with xwidget-webkit support"))
  (unless (lean--proxy-available-p)
    (user-error "Lean infoview proxy not available (check lean-infoview-proxy-enabled)"))
  (let* ((root (lean--iv-project-root))
         (proxy-port (lean--iv-proxy-port root))
         (xbuf (and (boundp 'lean--iv--xwidget-buf)
                    lean--iv--xwidget-buf))
         (visible (and (buffer-live-p xbuf)
                       (get-buffer-window xbuf))))
    (cond
     ((and visible proxy-port)
      (lean--iv-log "closing visible infoview: buffer=%s" (buffer-name xbuf))
      (delete-windows-on xbuf)
     (message "Lean infoview hidden"))
     (t
      ;; A second key press replaces the old wait instead of leaving another
      ;; timer which may later create a duplicate xwidget.
      (lean--iv-cancel-port-wait)
      ;; A visible page with no live owner is a failed/stale xwidget.  Do not
      ;; preserve it and make the next toggle merely reveal the same white page.
      (when visible
        (delete-windows-on xbuf)
        (kill-buffer xbuf)
        (setq lean--iv--xwidget-buf nil)
        (remhash root lean--iv--xwidget-buffers)
        (lean--iv-close-remote-forward root))
      (if (and (fboundp 'eglot-managed-p)
               (eglot-managed-p)
               (or (not (lean--proxy-gateway-binding root))
                   (not proxy-port)))
          (progn
            (lean--iv-log "managed Eglot has no live proxy endpoint; reconnecting")
            (message "Lean infoview: reconnecting Eglot proxy…")
            (lean--proxy-forget-gateway-binding root)
            (lean--iv-reconnect-eglot))
        ;; Ensure Eglot is running; its contact allocates and starts the proxy.
        (when (fboundp 'lean--ensure-eglot)
          (lean--ensure-eglot)))
      (lean--iv-open-current-buffer)))))

(defun lean-iv-restart ()
  "Restart the Lean infoview for the current buffer's project.
Reconnects Eglot (which restarts the proxy and lake serve) and reopens
the infoview xwidget page."
  (interactive)
  (unless (derived-mode-p 'lean-mode)
    (user-error "Must be called from a Lean source buffer"))
  (let* ((root (lean--iv-project-root))
         (xbuf (lean--iv-project-xwidget-buf root)))
    (lean--iv-cancel-port-wait)
    (lean--iv-log "restart requested: root=%s" root)
    (when (buffer-live-p xbuf)
      (delete-windows-on xbuf)
      (kill-buffer xbuf))
    (setq lean--iv--xwidget-buf nil)
    (remhash root lean--iv--xwidget-buffers)
    (lean--iv-close-remote-forward root)
    (lean--proxy-forget-gateway-binding root)
    (setq lean--iv--last-cursor nil)
    (when (fboundp 'eglot-reconnect)
      (lean--iv-reconnect-eglot))
    (lean--iv-open-current-buffer)))

;; ── Buffer sync hook (no-op — Eglot owns doc sync through proxy) ──────────────

(defun lean-iv-setup-buffer-sync ()
  "No-op: document sync is handled by Eglot through the proxy."
  nil)

;; ── Teardown ──────────────────────────────────────────────────────────────────

(defun lean-iv-teardown-h ()
  "Cancel timers and close the infoview when a lean-mode buffer is killed."
  (lean--iv-cancel-cursor-timer)
  (lean--iv-cancel-port-wait)
  (when (derived-mode-p 'lean-mode)
    (when-let* ((xbuf (and (boundp 'lean--iv--xwidget-buf)
                           lean--iv--xwidget-buf))
                ((buffer-live-p xbuf)))
      (let ((root (lean--iv-project-root)))
        (setq lean--iv--xwidget-buf nil)
        (unless (seq-find (lambda (b)
                            (and (not (eq b (current-buffer)))
                                 (buffer-live-p b)
                                 (with-current-buffer b
                                   (and (derived-mode-p 'lean-mode)
                                        (equal (lean--iv-project-root) root)))))
                          (buffer-list))
          (when (eq (gethash root lean--iv--xwidget-buffers) xbuf)
            (remhash root lean--iv--xwidget-buffers))
          (lean--iv-close-remote-forward root)
          (kill-buffer xbuf))))))

(provide 'init-lean-infoview)
;;; init-lean-infoview.el ends here
