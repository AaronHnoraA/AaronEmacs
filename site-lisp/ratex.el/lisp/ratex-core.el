;;; ratex-core.el --- Process management for ratex.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: ratex.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jsonrpc "1.0.24"))
;; Keywords: tex, math, tools

;;; Commentary:

;; Core backend process management for ratex.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(require 'url)

(defgroup ratex nil
  "Inline math rendering with RaTeX."
  :group 'tex)

(defcustom ratex-backend-binary
  (concat "backend/target/release/ratex-editor-backend"
          (if (eq system-type 'windows-nt) ".exe" ""))
  "Path to the backend binary managed by ratex.el.

If relative, it is resolved from `ratex-backend-root' when set, otherwise from
the discovered ratex.el installation root."
  :type 'string)

(defcustom ratex-backend-root nil
  "Absolute path to the ratex.el repository root.

Set this when auto-detection cannot reliably find the backend location."
  :type '(choice (const :tag "Auto detect" nil)
                 directory))

(defcustom ratex-auto-download-backend t
  "When non-nil, download the backend automatically if needed before startup."
  :type 'boolean)

(defcustom ratex-backend-release-repo "gongshangzheng/ratex.el"
  "GitHub repository that hosts backend release binaries."
  :type 'string)

(defcustom ratex-font-dir nil
  "Directory containing KaTeX .ttf font files.

When nil, the backend searches relative to its working directory.
Set this explicitly if the backend cannot locate fonts automatically."
  :type '(choice (const :tag "Auto detect" nil)
                 directory))

(defcustom ratex-edit-preview-idle-delay 0.18
  "Seconds of idle time before updating the active edit preview."
  :type 'number)

(defcustom ratex-edit-preview-scan-lines 4
  "Lines above and below point to scan for math delimiters before previewing.

This cheap regexp scan avoids running full fragment detection when point is
clearly far away from any math block."
  :type 'natnum)

(defcustom ratex-edit-preview-evil-insert-only t
  "When non-nil, only show floating edit previews in Evil insert state."
  :type 'boolean)

(defcustom ratex-org-disable-native-preview t
  "When non-nil, disable the local Org native preview pipeline while `ratex-mode' is active."
  :type 'boolean)

(defcustom ratex-font-size 16.0
  "Default backend SVG font size."
  :type 'number)

(defcustom ratex-svg-padding 2.0
  "Default SVG padding sent to the backend."
  :type 'number)

(defcustom ratex-edit-preview nil
  "Preview style used while editing formulas.

Set to nil to disable edit previews, or `posframe' to show a floating preview."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Posframe" posframe)))

(defcustom ratex-inline-preview t
  "Whether RaTeX should keep rendered inline overlays in the buffer.

When nil, RaTeX only shows the edit preview UI selected by
`ratex-edit-preview' and does not place persistent inline overlays."
  :type 'boolean)

(defcustom ratex-render-color nil
  "Default formula color sent to backend rendering.

Use nil to keep backend defaults."
  :type '(choice (const :tag "Backend default" nil)
                 string))

(defcustom ratex-posframe-background-color "white"
  "Background color for RaTeX posframe preview."
  :type 'string)

(defcustom ratex-posframe-border-color "gray70"
  "Border color for RaTeX posframe preview."
  :type 'string)

(defcustom ratex-posframe-poshandler
  'ratex-posframe-poshandler-point-top-left-corner-offset
  "Poshandler function used to place the RaTeX posframe preview.

The default prefers showing the preview above point and falls back below
when there isn't enough space, similar to eldoc-box's at-point behavior."
  :type 'function)

(defcustom ratex-posframe-max-pixel-width 720
  "Maximum width of the RaTeX posframe in pixels."
  :type 'integer)

(defcustom ratex-posframe-max-pixel-height 420
  "Maximum height of the RaTeX posframe in pixels."
  :type 'integer)

(defcustom ratex-hide-source-while-preview nil
  "Whether to hide the source text while the edit preview is visible."
  :type 'boolean)

(defcustom ratex-debug nil
  "When non-nil, append runtime diagnostics to the RaTeX debug buffer."
  :type 'boolean)

(defcustom ratex-render-cache-limit 32
  "Maximum number of in-memory render results kept per buffer."
  :type 'integer)

(defcustom ratex-render-cache-ttl 90
  "Seconds an unused in-memory render result may live before cleanup."
  :type 'integer)

(defcustom ratex-request-timeout 8.0
  "Seconds to wait before treating a backend request as timed out."
  :type 'number)

(defcustom ratex-backend-restart-cooldown 5.0
  "Cooldown in seconds after repeated backend startup failures."
  :type 'number)

(defcustom ratex-backend-max-start-failures 3
  "Suspend backend startup temporarily after this many consecutive failures."
  :type 'integer)

(defvar ratex--process nil)
(defvar ratex--process-buffer " *ratex-backend*")
(defvar ratex--pending (make-hash-table :test #'eql))
(defvar ratex--next-id 0)
(defvar ratex--read-buffer "")
(defvar ratex--read-chunks nil
  "Pending output chunks accumulated since the last newline, in reverse order.")
(defvar ratex--startup-warned nil)
(defvar ratex--startup-callbacks nil)
(defvar ratex--download-in-progress nil)
(defvar ratex--pending-timers (make-hash-table :test #'eql))
(defvar ratex--backend-start-failures 0)
(defvar ratex--backend-suspended-until nil)
(defconst ratex--debug-buffer-name "*RaTeX Debug*")

(defun ratex-debug-log (format-string &rest args)
  "Append a formatted debug line using FORMAT-STRING and ARGS."
  (when ratex-debug
    (with-current-buffer (get-buffer-create ratex--debug-buffer-name)
      (goto-char (point-max))
      (insert
       (format-time-string "[%H:%M:%S.%3N] ")
       (apply #'format format-string args)
       "\n"))))

(defun ratex-debug-open-buffer ()
  "Open the RaTeX debug buffer."
  (interactive)
  (display-buffer (get-buffer-create ratex--debug-buffer-name)))

(defun ratex-debug-close-buffer ()
  "Close the RaTeX debug buffer if it exists."
  (interactive)
  (when-let* ((buffer (get-buffer ratex--debug-buffer-name)))
    (kill-buffer buffer)))

(defun ratex-root ()
  "Return the installed root directory of ratex.el."
  (or (and ratex-backend-root
           (file-name-as-directory (expand-file-name ratex-backend-root)))
      (ratex--discover-root)
      (error "Could not determine ratex.el root; set `ratex-backend-root'")))

(defun ratex-default-font-dir ()
  "Return the bundled KaTeX font directory when it exists, else nil."
  (let ((dir (expand-file-name "vendor/ratex-core/fonts" (ratex-root))))
    (when (file-directory-p dir)
      dir)))

(defun ratex-backend-live-p ()
  "Return non-nil when the backend process is alive."
  (and ratex--process (process-live-p ratex--process)))

(defun ratex--backend-suspended-p ()
  "Return non-nil when backend startup is in cooldown."
  (and ratex--backend-suspended-until
       (> ratex--backend-suspended-until (float-time))))

(defun ratex--record-backend-start-success ()
  "Reset backend startup failure state after a successful launch."
  (setq ratex--backend-start-failures 0
        ratex--backend-suspended-until nil
        ratex--startup-warned nil))

(defun ratex--record-backend-start-failure (reason)
  "Track a backend startup failure for REASON."
  (setq ratex--backend-start-failures (1+ ratex--backend-start-failures))
  (ratex-debug-log "backend start failure #%s: %s"
                   ratex--backend-start-failures reason)
  (when (>= ratex--backend-start-failures ratex-backend-max-start-failures)
    (setq ratex--backend-suspended-until
          (+ (float-time) ratex-backend-restart-cooldown))
    (ratex--warn
     (format "RaTeX backend suspended for %.1fs after repeated launch failures."
             ratex-backend-restart-cooldown))))

(defun ratex--cancel-pending-timeout (id)
  "Cancel and forget the timeout timer associated with request ID."
  (when-let* ((timer (gethash id ratex--pending-timers)))
    (cancel-timer timer)
    (remhash id ratex--pending-timers)))

(defun ratex--resolve-pending-request (id response)
  "Resolve pending request ID with RESPONSE."
  (let ((callback (gethash id ratex--pending)))
    (when callback
      (ratex--cancel-pending-timeout id)
      (remhash id ratex--pending)
      (funcall callback response))))

(defun ratex-start-backend (&optional callback)
  "Start the backend process if needed.

When CALLBACK is non-nil, invoke it with the live process once startup succeeds."
  (cond
   ((ratex-backend-live-p)
    (ratex-debug-log "backend already live")
    (when callback
      (funcall callback ratex--process))
    ratex--process)
   (ratex--download-in-progress
    (when callback
      (push callback ratex--startup-callbacks))
    nil)
   ((ratex--backend-suspended-p)
    (ratex-debug-log "backend startup skipped during cooldown")
    (when callback
      (funcall callback nil))
    nil)
   ((ratex--backend-ready-p)
    (condition-case err
        (progn
          (ratex-debug-log "launch backend: %s" (ratex--backend-binary-path))
          (ratex--launch-backend)
          (ratex--record-backend-start-success)
          (when callback
            (funcall callback ratex--process))
          ratex--process)
      (error
       (ratex--record-backend-start-failure (error-message-string err))
       (if ratex-auto-download-backend
           (progn
             (ratex-debug-log "backend launch failed; redownload: %s"
                              (error-message-string err))
             (ratex--delete-backend-binary)
             (when callback
               (push callback ratex--startup-callbacks))
             (ratex--download-backend-async))
         (ratex--warn
          (format "RaTeX backend launch failed: %s"
                  (error-message-string err)))
         (when callback (funcall callback nil))
         nil))))
   (ratex-auto-download-backend
    (ratex-debug-log "backend missing; start download")
    (when callback
      (push callback ratex--startup-callbacks))
    (ratex--download-backend-async)
    nil)
   (t
    (ratex--warn "RaTeX backend binary is missing. Run `M-x ratex-download-backend`.")
    (when callback (funcall callback nil))
    nil)))

(defun ratex-stop-backend ()
  "Stop the backend process."
  (interactive)
  (when (ratex-backend-live-p)
    (ratex-debug-log "stop backend")
    (delete-process ratex--process))
  (setq ratex--process nil))

(defun ratex-download-backend ()
  "Download the backend binary from GitHub Releases asynchronously."
  (interactive)
  (if ratex--download-in-progress
      (message "RaTeX backend download already in progress.")
    (ratex--download-backend-async)))

(defun ratex--download-backend-async ()
  "Start an asynchronous download of the backend binary."
  (let* ((binary (ratex--backend-binary-path))
         (directory (file-name-directory binary))
         (url (ratex--backend-download-url)))
    (setq ratex--startup-warned nil)
    (setq ratex--download-in-progress t)
    (make-directory directory t)
    (message "Downloading RaTeX backend from %s..." url)
    (url-retrieve
     url
     (lambda (status)
       (let ((callbacks (nreverse ratex--startup-callbacks))
             (temp-file (make-temp-file
                         "ratex-backend-"
                         nil
                         (if (eq system-type 'windows-nt) ".exe" ""))))
         (setq ratex--startup-callbacks nil)
         (setq ratex--download-in-progress nil)
         (unwind-protect
             (if (or (not status)
                     (plist-get status :error))
                 (let ((err (plist-get status :error)))
                   (ratex--warn
                    (format "RaTeX backend download failed: %s"
                            (if err (error-message-string err) "unknown error")))
                   (dolist (cb callbacks)
                     (funcall cb nil)))
               (goto-char (point-min))
               (re-search-forward "\r?\n\r?\n" nil t)
               (let ((body-start (point)))
                 (write-region body-start (point-max) temp-file nil 'silent))
               (ratex--validate-backend-file temp-file url)
               (rename-file temp-file binary t)
               (unless (eq system-type 'windows-nt)
                 (set-file-modes binary #o755))
               (message "RaTeX backend downloaded to %s" binary)
               (ratex--launch-backend)
               (dolist (cb callbacks)
                 (funcall cb ratex--process)))
           (when (file-exists-p temp-file)
             (delete-file temp-file)))))
     nil t)))

(defcustom ratex-backend-build-command
  '("cargo" "build" "--release" "--manifest-path" "backend/Cargo.toml")
  "Command used to build the backend binary locally.

The default matches the GitHub Actions release build."
  :type '(repeat string))

(defvar ratex--build-process nil)
(defvar ratex--build-buffer " *ratex-backend-build*")

(defun ratex-build-backend ()
  "Build the backend binary locally using cargo.
This is intended for developers who want to compile from source
instead of downloading a pre-built binary."
  (interactive)
  (when (ratex--build-in-progress-p)
    (error "A backend build is already in progress"))
  (let* ((root (ratex-root))
         (default-directory root)
         (program (car ratex-backend-build-command))
         (args (cdr ratex-backend-build-command)))
    (message "Building RaTeX backend...")
    (setq ratex--build-process
          (make-process
           :name "ratex-backend-build"
           :buffer ratex--build-buffer
           :command (cons program args)
           :coding 'utf-8-unix
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (_proc event)
             (unless (process-live-p ratex--build-process)
               (let ((success (= (process-exit-status ratex--build-process) 0)))
                 (setq ratex--build-process nil)
                 (if success
                     (message "RaTeX backend build finished.")
                   (ratex--warn "RaTeX backend build failed.")))))))))

(defun ratex--build-in-progress-p ()
  "Return non-nil if a backend build is in progress."
  (and ratex--build-process (process-live-p ratex--build-process)))

(defun ratex-request (payload callback)
  "Send PAYLOAD to backend and invoke CALLBACK with parsed response."
  (let ((id (cl-incf ratex--next-id))
        (origin-buffer (current-buffer))
        (data nil))
    (setq data (append (list (cons 'id id)) payload))
    (ratex-debug-log "request #%s buffer=%s payload=%S"
                     id (buffer-name origin-buffer) data)
    (puthash
     id
     (lambda (response)
       (ratex-debug-log "callback #%s buffer-live=%s response=%S"
                        id (buffer-live-p origin-buffer) response)
       (when (buffer-live-p origin-buffer)
         (with-current-buffer origin-buffer
           (funcall callback response))))
     ratex--pending)
    (when (and (numberp ratex-request-timeout)
               (> ratex-request-timeout 0))
      (puthash
       id
       (run-with-timer
        ratex-request-timeout nil
        (lambda (request-id)
          (ratex-debug-log "request timeout #%s" request-id)
          (ratex--resolve-pending-request
           request-id
           '((ok . :false) (error . "backend request timed out"))))
        id)
       ratex--pending-timers))
    (ratex-start-backend
     (lambda (proc)
       (if (and proc (process-live-p proc))
           (process-send-string proc (concat (json-encode data) "\n"))
         ;; Backend unavailable — resolve pending entry immediately so it
         ;; does not accumulate forever in ratex--pending.
         (ratex--resolve-pending-request
          id
          '((ok . :false) (error . "backend unavailable"))))))
    id))

(defun ratex-ping (callback)
  "Ping the backend and invoke CALLBACK with the response."
  (ratex-request '((type . "ping")) callback))

(defun ratex--process-filter (_proc chunk)
  "Process backend output CHUNK.

Chunks are accumulated in a list and joined only when a newline
is seen, avoiding the O(n²) cost of repeated string concatenation
for large SVG payloads."
  (ratex-debug-log "backend chunk=%S" chunk)
  (push chunk ratex--read-chunks)
  (when (string-search "\n" chunk)
    (let* ((data (apply #'concat (nreverse ratex--read-chunks)))
           (text (if (string-empty-p ratex--read-buffer)
                     data
                   (concat ratex--read-buffer data))))
      (setq ratex--read-chunks nil
            ratex--read-buffer "")
      (let ((start 0))
        (while-let ((pos (string-search "\n" text start)))
          (let ((line (substring text start pos)))
            (unless (string-empty-p line)
              (ratex--dispatch-line line)))
          (setq start (1+ pos)))
        (unless (= start (length text))
          (setq ratex--read-buffer (substring text start)))))))

(defun ratex--dispatch-line (line)
  "Dispatch one backend output LINE."
  (ratex-debug-log "dispatch line=%s" line)
  (let* ((json-object-type 'alist)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (json-false :false)
        (data (ignore-errors (json-read-from-string line))))
    (when data
      (let* ((id (alist-get 'id data))
             (_callback (gethash id ratex--pending)))
        (when _callback
          (ratex--resolve-pending-request id data))))))

(defun ratex--process-sentinel (proc event)
  "Handle backend PROC EVENT."
  (ratex-debug-log "process sentinel live=%s event=%s" (process-live-p proc) (string-trim event))
  (unless (process-live-p proc)
    (maphash
     (lambda (id _callback)
       (ratex--resolve-pending-request
        id
        `((ok . :false) (error . ,(string-trim event)))))
     ratex--pending)
    (clrhash ratex--pending)
    (clrhash ratex--pending-timers)
    (setq ratex--read-chunks nil
          ratex--read-buffer ""
          ratex--process nil)))

(defun ratex--launch-backend ()
  "Launch the backend binary."
  (let ((binary (ratex--backend-binary-path))
        (default-directory (ratex-root)))
    (unless (file-exists-p binary)
      (error "RaTeX backend binary does not exist: %s" binary))
    (ratex--validate-backend-file binary)
    (setq ratex--read-buffer "")
    (ratex-debug-log "make-process backend binary=%s cwd=%s" binary default-directory)
    (setq ratex--process
          (make-process
           :name "ratex-backend"
           :buffer ratex--process-buffer
           :command (list binary)
           :coding 'utf-8-unix
           :connection-type 'pipe
           :noquery t
           :filter #'ratex--process-filter
           :sentinel #'ratex--process-sentinel))))

(defun ratex--backend-ready-p ()
  "Return non-nil if the backend binary exists and looks executable."
  (let ((binary (ratex--backend-binary-path)))
    (and (file-exists-p binary)
         (ratex--backend-file-valid-p binary))))

(defun ratex--delete-backend-binary ()
  "Delete the current backend binary when it exists."
  (let ((binary (ratex--backend-binary-path)))
    (when (file-exists-p binary)
      (delete-file binary))))

(defun ratex--warn (message-text)
  "Show MESSAGE-TEXT once per startup failure burst."
  (unless ratex--startup-warned
    (setq ratex--startup-warned t)
    (display-warning 'ratex message-text :warning)))

(defun ratex-diagnose-backend ()
  "Show the current backend resolution state."
  (interactive)
  (let* ((root (condition-case err
                   (ratex-root)
                 (error (format "ERROR: %s" (error-message-string err)))))
         (binary (condition-case err
                     (ratex--backend-binary-path)
                   (error (format "ERROR: %s" (error-message-string err)))))
         (download-url (condition-case err
                           (ratex--backend-download-url)
                         (error (format "ERROR: %s" (error-message-string err)))))
         (valid (condition-case err
                    (ratex--backend-file-valid-p binary)
                  (error (format "ERROR: %s" (error-message-string err)))))
         (message-text
          (format
           (concat "ratex root: %s\n"
                   "backend binary: %s\n"
                   "binary exists: %s\n"
                   "binary valid: %s\n"
                   "auto download: %s\n"
                   "release repo: %s\n"
                   "download url: %s")
           root
           binary
           (and (stringp binary) (file-exists-p binary))
           valid
           ratex-auto-download-backend
           ratex-backend-release-repo
           download-url)))
    (if (called-interactively-p 'interactive)
        (message "%s" message-text)
      message-text)))

(defun ratex--backend-binary-path ()
  "Return the absolute path of the backend binary."
  (let ((binary ratex-backend-binary))
    (if (file-name-absolute-p binary)
        binary
      (expand-file-name binary (ratex-root)))))

(defun ratex--backend-download-url ()
  "Return the GitHub Release URL for the current platform backend."
  (format "%s/%s"
          (format "https://github.com/%s/releases/latest/download"
                  ratex-backend-release-repo)
          (ratex--backend-asset-name)))

(defun ratex--backend-asset-name ()
  "Return the release asset name for the current platform."
  (cond
   ((eq system-type 'gnu/linux) "ratex-editor-backend-linux")
   ((eq system-type 'darwin) "ratex-editor-backend-macos")
   ((eq system-type 'windows-nt) "ratex-editor-backend-windows.exe")
   (t
    (error "Unsupported system type for RaTeX backend: %S" system-type))))

(defun ratex--backend-file-valid-p (path)
  "Return non-nil when PATH looks like a valid executable for this platform."
  (and (stringp path)
       (file-exists-p path)
       (not (file-directory-p path))
       (> (file-attribute-size (file-attributes path)) 2)
       (let ((coding-system-for-read 'no-conversion))
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally path nil 0 4)
           (cond
            ((eq system-type 'windows-nt)
             (string-prefix-p "MZ" (buffer-string)))
            ((eq system-type 'gnu/linux)
             (equal (buffer-string) "\177ELF"))
            ((eq system-type 'darwin)
             (member (buffer-string)
                     '("\317\372\355\376"
                       "\316\372\355\376"
                       "\376\355\372\317"
                       "\376\355\372\316"
                       "\312\376\272\276")))
            (t nil))))))

(defun ratex--validate-backend-file (path &optional source-url)
  "Signal an error when PATH is not a valid backend executable.

When SOURCE-URL is non-nil, include it in the error message."
  (unless (ratex--backend-file-valid-p path)
    (let ((details (condition-case nil
                       (let ((coding-system-for-read 'utf-8-unix))
                         (with-temp-buffer
                           (insert-file-contents path nil 0 120)
                           (string-trim (buffer-string))))
                     (error nil))))
      (error
       (concat
        "Downloaded backend is not a valid "
        (pcase system-type
          ('windows-nt "Windows executable")
          ('gnu/linux "ELF executable")
          ('darwin "macOS executable")
          (_ "executable"))
        (when source-url
          (format " from %s" source-url))
        (when (and details (not (string-empty-p details)))
          (format "; file starts with: %S" details)))))))

(defun ratex--discover-root ()
  "Discover the ratex.el root directory."
  (let ((candidates
         (delq nil
               (mapcar
                #'ratex--candidate-root
                (list load-file-name
                      (locate-library "ratex-core.el")
                      (locate-library "ratex.el")
                      (buffer-file-name))))))
    (seq-find #'ratex--valid-root-p candidates)))

(defun ratex--candidate-root (path)
  "Return a possible ratex root for PATH."
  (when path
    (let* ((full (expand-file-name path))
           (dir (file-name-directory full)))
      (cond
       ((not dir) nil)
       ((ratex--valid-root-p (ratex--straight-repo-root dir))
        (ratex--straight-repo-root dir))
       ((string-match-p "/lisp/?\\'" dir)
        (file-name-directory (directory-file-name dir)))
       ((file-directory-p full)
        full)
       (t
        (or (locate-dominating-file dir "lisp/ratex.el")
            (locate-dominating-file dir "backend/Cargo.toml")))))))

(defun ratex--straight-repo-root (path)
  "Return a straight.el repo root mapped from PATH, or nil."
  (when (string-match "\\(.*/straight\\)/build/\\([^/]+\\)/" path)
    (let* ((base (match-string 1 path))
           (pkg (match-string 2 path))
           (repos (expand-file-name "repos" base))
           (candidate-a (expand-file-name pkg repos))
           (candidate-b (expand-file-name (concat pkg ".el") repos)))
      (cond
       ((ratex--valid-root-p candidate-a) candidate-a)
       ((ratex--valid-root-p candidate-b) candidate-b)
       (t nil)))))

(defun ratex--valid-root-p (path)
  "Return non-nil when PATH looks like a valid ratex.el root."
  (and path
       (file-exists-p (expand-file-name "lisp/ratex.el" path))
       (file-directory-p (expand-file-name "lisp" path))))

(provide 'ratex-core)

;;; ratex-core.el ends here
