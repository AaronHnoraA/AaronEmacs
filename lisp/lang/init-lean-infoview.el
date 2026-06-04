;;; init-lean-infoview.el --- Lean4 xwidget infoview via bridge server -*- lexical-binding: t -*-

;;; Commentary:
;; Serves the official @leanprover/infoview React component via a local Node.js
;; bridge (lean4-infoview-bridge/server.mjs), displays it in an xwidget-webkit
;; buffer, and syncs cursor position via xwidget-webkit-execute-script.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function lsp--cur-position "lsp-mode" ())
(declare-function lsp--text-document-identifier "lsp-mode" ())
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script))
(declare-function xwidget-at "xwidget" (pos))

(defvar my/lean4-iv--servers (make-hash-table :test #'equal)
  "Hash of project-root → (process . port).")

(defvar-local my/lean4-iv--xwidget-buf nil
  "The xwidget buffer for this lean4 source buffer.")

(defconst my/lean4-iv--script-dir
  (expand-file-name "lean4-infoview-bridge" user-emacs-directory)
  "Directory containing server.mjs and dist/.")

(defun my/lean4-iv-node-p ()
  "Return non-nil if node and the dist bundle exist."
  (and (executable-find "node")
       (file-exists-p (expand-file-name "dist/index.html"
                                        my/lean4-iv--script-dir))))

(defun my/lean4-iv-xwidget-p ()
  "Return non-nil if xwidget-webkit is available."
  (fboundp 'xwidget-webkit-browse-url))

(defun my/lean4-iv-available-p ()
  "Return non-nil if the xwidget infoview can be used."
  (and (my/lean4-iv-xwidget-p)
       (my/lean4-iv-node-p)
       (not (file-remote-p default-directory))))

;; ── Server management ────────────────────────────────────────────────────────

(defun my/lean4-iv--server-entry (root)
  "Return (process . port) for ROOT, or nil."
  (gethash root my/lean4-iv--servers))

(defun my/lean4-iv--server-port (root)
  "Return port for ROOT if live, else nil."
  (when-let* ((entry (my/lean4-iv--server-entry root))
              (proc  (car entry))
              ((process-live-p proc)))
    (cdr entry)))

(defun my/lean4-iv-start-server (root callback)
  "Start bridge server for ROOT; call CALLBACK with port when ready."
  (let* ((server-script (expand-file-name "server.mjs" my/lean4-iv--script-dir))
         (log-buf (get-buffer-create (format " *lean4-iv-server[%s]*" (file-name-nondirectory root))))
         (proc (make-process
                :name    "lean4-iv-server"
                :buffer  log-buf
                :command (list "node" server-script "0" root)
                :noquery t
                :filter
                (lambda (proc string)
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-max))
                      (insert string)))
                  ;; Parse the port announcement once
                  (unless (process-get proc 'port)
                    (when (string-match "LEAN_INFOVIEW_PORT=\\([0-9]+\\)" string)
                      (let ((port (string-to-number (match-string 1 string))))
                        (process-put proc 'port port)
                        (puthash root (cons proc port) my/lean4-iv--servers)
                        (funcall callback port)))))
                :sentinel
                (lambda (proc _event)
                  (unless (process-live-p proc)
                    (remhash root my/lean4-iv--servers))))))
    (process-put proc 'root root)
    proc))

(defun my/lean4-iv-ensure-server (root callback)
  "Ensure bridge server running for ROOT; call CALLBACK with port."
  (if-let* ((port (my/lean4-iv--server-port root)))
      (funcall callback port)
    (my/lean4-iv-start-server root callback)))

(defun my/lean4-iv-stop-server (root)
  "Kill bridge server for ROOT."
  (when-let* ((entry (my/lean4-iv--server-entry root))
              (proc  (car entry))
              ((process-live-p proc)))
    (delete-process proc))
  (remhash root my/lean4-iv--servers))

;; ── Xwidget buffer ───────────────────────────────────────────────────────────

(defun my/lean4-iv--xwidget-buf-name (source)
  "Return buffer name for the infoview xwidget of SOURCE."
  (format "*Lean Infoview<%s>*" (buffer-name source)))

(defun my/lean4-iv--xwidget-of-buf (buf)
  "Return the xwidget widget inside BUF, or nil."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (fboundp 'xwidget-at)
        (xwidget-at (point-min))))))

(defun my/lean4-iv-open-infoview (source port)
  "Show the infoview xwidget for SOURCE buffer at PORT."
  (let* ((url (format "http://127.0.0.1:%d/" port))
         (bname (my/lean4-iv--xwidget-buf-name source))
         (existing (get-buffer bname)))
    (with-current-buffer source
      (setq my/lean4-iv--xwidget-buf
            (if (and (buffer-live-p existing)
                     (my/lean4-iv--xwidget-of-buf existing))
                existing
              ;; Create fresh xwidget buffer
              (let ((xbuf (generate-new-buffer bname)))
                (with-current-buffer xbuf
                  (xwidget-webkit-browse-url url t))
                xbuf))))
    (display-buffer-in-side-window
     (with-current-buffer source my/lean4-iv--xwidget-buf)
     `((side . right) (slot . 1) (window-width . 80) (dedicated . t)))))

;; ── Cursor sync ──────────────────────────────────────────────────────────────

(defun my/lean4-iv--xw ()
  "Return live xwidget for current source buffer's infoview, or nil."
  (when-let* ((buf (and (boundp 'my/lean4-iv--xwidget-buf)
                        my/lean4-iv--xwidget-buf))
              ((buffer-live-p buf)))
    (my/lean4-iv--xwidget-of-buf buf)))

(defun my/lean4-iv-sync-cursor-h ()
  "Push current cursor position to the infoview xwidget."
  (when (and (derived-mode-p 'lean4-mode)
             (buffer-file-name)
             (bound-and-true-p lsp-managed-mode))
    (when-let* ((xw (my/lean4-iv--xw))
                (uri (format "file://%s" (buffer-file-name)))
                (pos (lsp--cur-position))
                (line (plist-get pos :line))
                (char (plist-get pos :character)))
      (xwidget-webkit-execute-script
       xw
       (format "window.updateCursor && window.updateCursor(%s,%d,%d);"
               (json-encode uri) line char)))))

;; ── Also sync doc content via HTTP (fire-and-forget) ─────────────────────────

(defun my/lean4-iv--post-cursor (port uri)
  "POST /cursor to bridge server at PORT with URI (opens doc if needed)."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string (json-encode `(:uri ,uri)) 'utf-8)))
    (url-retrieve (format "http://127.0.0.1:%d/cursor" port)
                  #'ignore nil t t)))

;; ── Public entry points ───────────────────────────────────────────────────────

(defun my/lean4-iv-toggle ()
  "Open or close the Lean xwidget infoview for the current buffer."
  (interactive)
  (unless (derived-mode-p 'lean4-mode)
    (user-error "Must be called from a Lean source buffer"))
  (unless (my/lean4-iv-node-p)
    (user-error "Node.js and lean4-infoview-bridge/dist/ are required"))
  (unless (my/lean4-iv-xwidget-p)
    (user-error "Emacs xwidget-webkit support is required"))
  (if (and (boundp 'my/lean4-iv--xwidget-buf)
           (buffer-live-p my/lean4-iv--xwidget-buf)
           (get-buffer-window my/lean4-iv--xwidget-buf))
      ;; Already visible — close it
      (delete-windows-on my/lean4-iv--xwidget-buf)
    ;; Start server and open
    (let* ((source (current-buffer))
           (root   (or (locate-dominating-file default-directory "lakefile.lean")
                       (locate-dominating-file default-directory "lakefile.toml")
                       default-directory)))
      (message "Starting Lean infoview server…")
      (my/lean4-iv-ensure-server
       root
       (lambda (port)
         (with-current-buffer source
           ;; Ensure document is open in bridge server
           (when (buffer-file-name)
             (my/lean4-iv--post-cursor port (format "file://%s" (buffer-file-name))))
           (run-at-time
            1.5 nil  ; brief delay for LSP to initialize
            (lambda ()
              (when (buffer-live-p source)
                (with-current-buffer source
                  (my/lean4-iv-open-infoview source port)
                  (message "Lean infoview ready at http://127.0.0.1:%d" port)))))))))))

(defun my/lean4-iv-teardown-h ()
  "Clean up infoview state when lean4 buffer is killed."
  (when (and (derived-mode-p 'lean4-mode)
             (buffer-live-p (and (boundp 'my/lean4-iv--xwidget-buf)
                                 my/lean4-iv--xwidget-buf)))
    ;; Kill the xwidget buffer if no other lean buffers use it
    (let ((xbuf my/lean4-iv--xwidget-buf))
      (setq my/lean4-iv--xwidget-buf nil)
      (unless (seq-find (lambda (b)
                          (and (not (eq b (current-buffer)))
                               (with-current-buffer b
                                 (and (derived-mode-p 'lean4-mode)
                                      (eq (and (boundp 'my/lean4-iv--xwidget-buf)
                                               my/lean4-iv--xwidget-buf)
                                          xbuf)))))
                        (buffer-list))
        (when (buffer-live-p xbuf)
          (kill-buffer xbuf))))))

(provide 'init-lean-infoview)
;;; init-lean-infoview.el ends here
