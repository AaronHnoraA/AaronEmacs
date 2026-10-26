;;; tools/copilot/config.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)
(require 'cl-lib)
(require 'json)
(require 'remote-gateway)
(require 'remote-process)
(require 'subr-x)

;;; ── GitHub Copilot ────────────────────────────────────────────────────────
(declare-function copilot-server-executable "copilot" ())
(declare-function copilot-mode "copilot" (&optional arg))
(declare-function copilot--command "copilot" ())
(declare-function copilot--handle-notification "copilot" (connection method params))
(declare-function copilot--handle-request "copilot" (connection method params))
(declare-function copilot--connection-alivep "copilot" ())
(declare-function copilot--path-to-uri "copilot" (path))
(declare-function copilot--start-server "copilot" ())
(declare-function copilot--overlay-visible "copilot" ())
(declare-function copilot-current-completion "copilot" ())
(declare-function copilot--get-overlay "copilot" ())
(declare-function copilot--set-overlay-text "copilot" (overlay completion))
(declare-function copilot-accept-completion "copilot" (&optional transform-fn))
(declare-function copilot-accept-completion-by-word "copilot" (&optional n))
(declare-function copilot-accept-completion-to-char "copilot" (char &optional count))
(declare-function jsonrpc-notify "jsonrpc" (connection method params))
(declare-function jsonrpc-request "jsonrpc" (connection method params &rest args))
(declare-function aaron-snippet-active-p "aaron-funcs" ())
(declare-function aaron/snippet-next-field-dwim "aaron-funcs" ())
(declare-function aaron/forward-delimiter-dwim "aaron-funcs" ())
(declare-function aaron/jump-forward-dwim "aaron-funcs" ())
(declare-function aaron/backward-delimiter-or-snippet-dwim "aaron-funcs" ())
(defvar copilot--connection nil)
(defvar copilot--overlay nil)
(defvar copilot--quota nil)
(defvar copilot--status nil)
(defvar copilot-log-max)

(defgroup +copilot nil
  "Copilot integration defaults."
  :group 'tools)

(defface +copilot-jump-label-face
  '((t (:foreground "#111827" :background "#f7d774" :weight bold)))
  "Face for temporary s-jump labels inside a Copilot ghost completion."
  :group '+copilot)

(config-defvar +copilot-idle-delay nil
  "Idle seconds before Copilot asks for inline completions."
  :type '(choice (number :tag "Seconds of delay") (const :tag "Inline completion disabled" nil))
  :group '+copilot)

(config-defvar +copilot-large-buffer-threshold nil
  "Maximum buffer size where Copilot is auto-enabled.
Large generated files can make inline completion unnecessarily expensive."
  :type 'integer
  :group '+copilot)

(config-defvar +copilot-disable-on-remote nil
  "Whether to skip automatic Copilot startup in remote buffers."
  :type 'boolean
  :group '+copilot)

(config-defvar +copilot-server-max-heap-mb nil
  "V8 heap cap (MB) for the Copilot language server, or nil for no cap.
Applied via `NODE_OPTIONS=--max-old-space-size' on the process this Emacs
spawns.  Emacs-launched Noema now uses
the shared Emacs gateway to reuse that process instead of starting a second
language server; this cap is only mirrored to Noema's local fallback LSP
in standalone runs."
  :type '(choice (integer :tag "Heap cap in MB") (const :tag "No cap" nil))
  :group '+copilot)

(config-defvar +copilot-deferred-modes nil
  "Major modes where automatic Copilot startup waits for editor idle time."
  :type '(repeat symbol)
  :group '+copilot)

(config-defvar +copilot-defer-on-remote t
  "Start Copilot after editor idle time in remote editing buffers.
This lets the file visit and language-server startup finish before the
client-side Copilot server is loaded."
  :type 'boolean
  :group '+copilot)

(config-defvar +copilot-deferred-idle-delay nil
  "Idle seconds before automatically enabling Copilot in deferred modes."
  :type 'number
  :group '+copilot)

(config-defvar +copilot-cold-local-idle-delay 0.75
  "Idle seconds before the first automatic Copilot load in a local buffer.
Set to nil to load Copilot synchronously on the first local source visit.
Remote and explicitly deferred modes use `+copilot-deferred-idle-delay'."
  :type '(choice (const :tag "Load synchronously" nil)
                 (number :tag "Idle seconds"))
  :group '+copilot)

(defvar +copilot-noema-bridge--documents (make-hash-table :test #'equal)
  "Synthetic Noema document state known to the shared Copilot LS.")

(defvar +copilot-noema-bridge--clients (make-hash-table :test #'equal)
  "Noema Copilot client state keyed by pane/client id.")

(defvar +copilot-noema-bridge--focused-uri nil
  "Synthetic Noema document URI last focused in the shared Copilot LS.")

(defvar +copilot-noema-bridge--focused-client nil
  "Noema pane/client id that most recently held Copilot focus.")

(defvar +copilot-noema-bridge--notified-focused-uri nil
  "Synthetic Noema document URI whose focus was last notified to Copilot.")

(defvar +copilot-noema-bridge--log nil
  "Bounded recent log entries for Noema Copilot bridge diagnostics.")

(defvar +copilot-noema-bridge--log-recording nil
  "Non-nil when the Noema Copilot bridge records detailed events.")

(defvar-local +copilot--auto-enable-timer nil
  "Idle timer used to defer automatic Copilot startup.")

(defun +copilot--client-process-environment ()
  "Return the client environment used by the local Copilot binary."
  (let ((process-environment (remote-client-process-environment)))
    (if +copilot-server-max-heap-mb
        (let* ((flag
                (format
                 "--max-old-space-size=%d"
                 +copilot-server-max-heap-mb))
               (existing (getenv "NODE_OPTIONS")))
          (cons
           (format
            "NODE_OPTIONS=%s"
            (if (and existing (not (string-empty-p existing)))
                (concat existing " " flag)
              flag))
           process-environment))
      process-environment)))

(defun +copilot--client-server-executable-a (fn &rest args)
  "Resolve the Copilot server through the Remote client boundary.
FN and ARGS name the original `copilot-server-executable' invocation."
  (let ((default-directory temporary-file-directory)
        (process-environment (remote-client-process-environment))
        (exec-path (remote-client-exec-path))
        (inhibit-file-name-operation 'file-exists-p)
        (inhibit-file-name-handlers
         (cons #'remote-file-name-handler
               (cons #'tramp-file-name-handler
                     inhibit-file-name-handlers))))
    (apply fn args)))

(defun +copilot--make-client-process ()
  "Create the Copilot language server at explicit client placement."
  (let* ((client-environment (+copilot--client-process-environment))
         (client-exec-path (remote-client-exec-path))
         (default-directory temporary-file-directory)
         (process-environment client-environment)
         (exec-path client-exec-path))
    (remote-make-client-process
     :name "copilot server"
     :command (copilot--command)
     :coding 'utf-8-emacs-unix
     :connection-type 'pipe
     :stderr (get-buffer-create "*copilot stderr*")
     :noquery t
     :remote-client-directory temporary-file-directory
     :remote-client-environment client-environment
     :remote-client-exec-path client-exec-path
     :remote-adapter "process")))

(defun +copilot--make-client-connection-a (_fn)
  "Create Copilot's JSON-RPC connection around ignored original _FN.
Only the process placement differs from `copilot--make-connection': the
language-server binary always runs beside Emacs."
  (let ((make-fn
         (apply-partially
          #'make-instance
          'jsonrpc-process-connection
          :name "copilot"
          :request-dispatcher #'copilot--handle-request
          :notification-dispatcher #'copilot--handle-notification
          :process (+copilot--make-client-process))))
    (condition-case nil
        (funcall
         make-fn
         :events-buffer-config `(:size ,copilot-log-max))
      (invalid-slot-name
       (funcall
        make-fn
        :events-buffer-scrollback-size copilot-log-max)))))

(defun +copilot--nonblocking-exit-a (original &rest arguments)
  "Avoid synchronous Copilot RPC while Emacs exits its own noquery child.
The Copilot exit hook normally waits for a JSON-RPC shutdown reply.  If the
agent closes during that wait, an asynchronous callback can unwind the whole
`kill-emacs' call, leaving Emacs alive.  A client-owned noquery process is
closed by Emacs on exit, so waiting for its reply is unnecessary.  Unknown
connection types keep the package's original shutdown behavior."
  (let ((process
         (and (boundp 'copilot--connection)
              copilot--connection
              (fboundp 'jsonrpc--process)
              (ignore-errors (jsonrpc--process copilot--connection)))))
    (unless (and (processp process)
                 (not (process-query-on-exit-flag process)))
      (apply original arguments))))

(defun +copilot-noema-bridge--log (event &optional detail)
  "Record Noema bridge EVENT with DETAIL when bridge logging is enabled."
  (when +copilot-noema-bridge--log-recording
    (push `((at . ,(format-time-string "%FT%T%z"))
            (event . ,event)
            ,@(when detail `((detail . ,detail))))
          +copilot-noema-bridge--log)
    (when (> (length +copilot-noema-bridge--log) 200)
      (setcdr (nthcdr 199 +copilot-noema-bridge--log) nil))))

(defun +copilot-noema-bridge--server-live-p ()
  "Return non-nil when the shared Emacs gateway is live."
  (remote-gateway-live-p))

(defun +copilot-noema-bridge--empty-object ()
  "Return a JSON object that serializes as `{}`."
  (make-hash-table :test #'equal))

(defun +copilot-noema-bridge--server-bootstrap-buffer ()
  "Return a buffer whose file context gives Copilot a stable workspace root."
  (let ((buffer (get-buffer-create " *Noema copilot bridge*")))
    (with-current-buffer buffer
      (setq-local buffer-file-name
                  (expand-file-name "var/noema/copilot-bridge/bridge.md"
                                    user-emacs-directory))
      (setq-local default-directory
                  (file-name-as-directory user-emacs-directory)))
    buffer))

(defun +copilot-noema-bridge--ensure-copilot ()
  "Ensure `copilot.el' is loaded and the shared LS connection is alive."
  (unless (require 'copilot nil t)
    (error "copilot.el is unavailable"))
  (unless (copilot--connection-alivep)
    (clrhash +copilot-noema-bridge--documents)
    (clrhash +copilot-noema-bridge--clients)
    (setq +copilot-noema-bridge--focused-uri nil
          +copilot-noema-bridge--focused-client nil
          +copilot-noema-bridge--notified-focused-uri nil)
    (with-current-buffer (+copilot-noema-bridge--server-bootstrap-buffer)
      (copilot--start-server))))

(defun +copilot-noema-bridge--request (method &optional params &rest args)
  "Send METHOD with PARAMS to the shared Copilot LS and return its result."
  (+copilot-noema-bridge--ensure-copilot)
  (apply #'jsonrpc-request
         copilot--connection
         method
         (or params (+copilot-noema-bridge--empty-object))
         args))

(defun +copilot-noema-bridge--superseded-error-p (err)
  "Return non-nil when ERR is Copilot's stale inline request cancellation."
  (let ((text (format "%S" err)))
    (or (string-match-p "jsonrpc-error-code[[:space:]\n]*\\.[[:space:]\n]*-32802" text)
        (string-match-p "Request was superseded by a new request" text))))

(defun +copilot-noema-bridge--notify (method params)
  "Send METHOD notification with PARAMS to the shared Copilot LS."
  (+copilot-noema-bridge--ensure-copilot)
  (jsonrpc-notify copilot--connection method params))

(defun +copilot-noema-bridge--language-id (file)
  "Return a Copilot language id for FILE."
  (pcase (downcase (or (file-name-extension (or file "") t) ""))
    ((or ".md" ".markdown") "markdown")
    (".typ" "typst")
    (".ts" "typescript")
    ((or ".js" ".mjs" ".cjs") "javascript")
    (".json" "json")
    (".tex" "latex")
    (".lean" "lean")
    (_ "plaintext")))

(defun +copilot-noema-bridge--uri-for-file (file)
  "Return a synthetic Noema Copilot URI for FILE.
The URI intentionally does not equal the real file URI, so Noema document
sync never collides with a normal Emacs buffer already opened in `copilot.el'."
  (unless (require 'copilot nil t)
    (error "copilot.el is unavailable"))
  (let* ((raw (if (and (stringp file) (not (string-empty-p file)))
                  (expand-file-name file)
                "noema-copilot.md"))
         (ext (or (file-name-extension raw t) ".md"))
         (base (file-name-nondirectory raw))
         (hash (secure-hash 'sha1 raw))
         (synthetic (expand-file-name
                     (format "var/noema/copilot-bridge/%s-%s%s"
                             hash
                             (file-name-base base)
                             ext)
                     user-emacs-directory)))
    (copilot--path-to-uri synthetic)))

(defun +copilot-noema-bridge--client-id (body)
  "Return Noema client id from BODY, or an empty string."
  (let ((value (or (plist-get body :clientId)
                   (plist-get body :client)
                   "")))
    (if (stringp value) (string-trim value) "")))

(defun +copilot-noema-bridge--body-active-p (body)
  "Return non-nil when BODY represents an active/focused pane."
  (and (not (eq (plist-get body :active) :json-false))
       (not (eq (plist-get body :focused) :json-false))))

(defun +copilot-noema-bridge--copilot-live-p ()
  "Return non-nil when the shared Copilot LS connection is alive."
  (and (require 'copilot nil t)
       (copilot--connection-alivep)))

(defun +copilot-noema-bridge--notify-if-live (method params)
  "Send METHOD notification with PARAMS without starting Copilot."
  (when (+copilot-noema-bridge--copilot-live-p)
    (jsonrpc-notify copilot--connection method params)))

(defun +copilot-noema-bridge--document-client-count (uri)
  "Return number of Noema clients still attached to URI."
  (let ((count 0))
    (maphash
     (lambda (_client state)
       (when (equal (plist-get state :uri) uri)
         (setq count (1+ count))))
     +copilot-noema-bridge--clients)
    count))

(defun +copilot-noema-bridge--close-document (uri &optional reason)
  "Close URI in Copilot when Noema has no remaining clients."
  (when (gethash uri +copilot-noema-bridge--documents)
    (remhash uri +copilot-noema-bridge--documents)
    (when (equal +copilot-noema-bridge--focused-uri uri)
      (setq +copilot-noema-bridge--focused-uri nil))
    (when (equal +copilot-noema-bridge--notified-focused-uri uri)
      (setq +copilot-noema-bridge--notified-focused-uri nil))
    (+copilot-noema-bridge--notify-if-live
     'textDocument/didClose
     (list :textDocument (list :uri uri)))
    (+copilot-noema-bridge--log
     "document-close"
     `((uri . ,uri) (reason . ,(or reason ""))))))

(defun +copilot-noema-bridge--detach-client (client-id &optional reason)
  "Detach CLIENT-ID from its Noema Copilot document."
  (unless (string-empty-p client-id)
    (let* ((state (gethash client-id +copilot-noema-bridge--clients))
           (uri (plist-get state :uri)))
      (when state
        (remhash client-id +copilot-noema-bridge--clients)
        (when (equal +copilot-noema-bridge--focused-client client-id)
          (setq +copilot-noema-bridge--focused-client nil)
          (when (equal +copilot-noema-bridge--focused-uri uri)
            (setq +copilot-noema-bridge--focused-uri nil)))
        (when (and uri
                   (= (+copilot-noema-bridge--document-client-count uri) 0)
                   (not (equal +copilot-noema-bridge--focused-uri uri)))
          (+copilot-noema-bridge--close-document uri reason)))
      state)))

(defun +copilot-noema-bridge--attach-client (client-id uri file state)
  "Attach CLIENT-ID to URI/FILE with lifecycle STATE."
  (unless (or (string-empty-p client-id)
              (string-empty-p uri))
    (let ((previous (gethash client-id +copilot-noema-bridge--clients)))
      (when (and previous
                 (not (equal (plist-get previous :uri) uri)))
        (+copilot-noema-bridge--detach-client client-id "switch-file")))
    (puthash client-id
             (list :uri uri
                   :file file
                   :state state
                   :updated-at (float-time))
             +copilot-noema-bridge--clients)))

(defun +copilot-noema-bridge--focus-document (uri client-id file notify)
  "Mark URI as focused by CLIENT-ID and optionally notify Copilot."
  (unless (string-empty-p client-id)
    (+copilot-noema-bridge--attach-client client-id uri file "focused"))
  (setq +copilot-noema-bridge--focused-uri uri
        +copilot-noema-bridge--focused-client
        (unless (string-empty-p client-id) client-id))
  (when (and notify
             (gethash uri +copilot-noema-bridge--documents)
             (not (equal +copilot-noema-bridge--notified-focused-uri uri)))
    (+copilot-noema-bridge--notify
     'textDocument/didFocus
     (list :textDocument (list :uri uri)))
    (setq +copilot-noema-bridge--notified-focused-uri uri)
    (+copilot-noema-bridge--log
     "document-focus"
     `((uri . ,uri) (file . ,file) (clientId . ,client-id)))))

(defun +copilot-noema-bridge--client-may-request-p (body uri)
  "Return non-nil when BODY's client may request inline completion for URI."
  (let* ((client-id (+copilot-noema-bridge--client-id body))
         (state (and (not (string-empty-p client-id))
                     (gethash client-id +copilot-noema-bridge--clients)))
         (focused-client +copilot-noema-bridge--focused-client)
         (focused-uri +copilot-noema-bridge--focused-uri))
    (and (+copilot-noema-bridge--body-active-p body)
         (or (string-empty-p client-id)
             (not (and (stringp focused-client)
                       (not (string-empty-p focused-client))))
             (equal focused-client client-id)
             (equal (plist-get state :state) "focused"))
         (or (not (and (stringp focused-uri)
                       (not (string-empty-p focused-uri))))
             (equal focused-uri uri)
             (equal (plist-get state :state) "focused")))))

(defun +copilot-noema-bridge--utf16-units (char)
  "Return the UTF-16 code-unit width of CHAR."
  (if (> char #xffff) 2 1))

(defun +copilot-noema-bridge--utf16-length (text)
  "Return the UTF-16 code-unit length of TEXT."
  (let ((units 0))
    (dotimes (i (length text) units)
      (setq units (+ units
                     (+copilot-noema-bridge--utf16-units
                      (aref text i)))))))

(defun +copilot-noema-bridge--json-value (value)
  "Return VALUE with nested JSON arrays represented as vectors."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'+copilot-noema-bridge--json-value
                     (append value nil))))
   ((and (listp value) (keywordp (car value)))
    (cl-loop for (key item) on value by #'cddr
             append (list key
                          (+copilot-noema-bridge--json-value item))))
   ((consp value)
    (vconcat (mapcar #'+copilot-noema-bridge--json-value value)))
   (t value)))

(defun +copilot-noema-bridge--json-array (value)
  "Return VALUE normalized as a JSON array."
  (cond
   ((null value) [])
   ((vectorp value)
    (+copilot-noema-bridge--json-value value))
   ((listp value)
    (vconcat (mapcar #'+copilot-noema-bridge--json-value value)))
   (t (vector (+copilot-noema-bridge--json-value value)))))

(defun +copilot-noema-bridge--position-for-offset (text offset)
  "Return LSP position in TEXT for JavaScript UTF-16 OFFSET."
  (let ((limit (max 0 (or offset 0)))
        (units 0)
        (line 0)
        (character 0)
        (i 0)
        (len (length text)))
    (while (and (< i len) (< units limit))
      (let* ((char (aref text i))
             (width (+copilot-noema-bridge--utf16-units char)))
        (if (= char ?\n)
            (setq line (1+ line)
                  character 0)
          (setq character (+ character width)))
        (setq units (+ units width)
              i (1+ i))))
    (list :line line :character character)))

(defun +copilot-noema-bridge--offset-for-position (text position)
  "Return JavaScript UTF-16 offset in TEXT for LSP POSITION."
  (let ((target-line (max 0 (or (plist-get position :line) 0)))
        (target-char (max 0 (or (plist-get position :character) 0)))
        (line 0)
        (character 0)
        (offset 0)
        (i 0)
        (len (length text)))
    (catch 'done
      (while (< i len)
        (let* ((char (aref text i))
               (width (+copilot-noema-bridge--utf16-units char)))
          (cond
           ((= line target-line)
            (when (= char ?\n)
              (throw 'done offset))
            (if (>= (+ character width) target-char)
                (throw 'done (+ offset (min width (max 0 (- target-char character)))))
              (setq character (+ character width)
                    offset (+ offset width))))
           (t
            (setq offset (+ offset width))
            (when (= char ?\n)
              (setq line (1+ line)
                    character 0)))))
        (setq i (1+ i)))
      offset)))

(defun +copilot-noema-bridge--full-range-end (text)
  "Return LSP position at the end of TEXT."
  (+copilot-noema-bridge--position-for-offset
   text
   (+copilot-noema-bridge--utf16-length text)))

(defun +copilot-noema-bridge--status ()
  "Return a JSON-serializable status plist for Noema."
  (or copilot--status
      (list :message (if (and (require 'copilot nil t)
                              (copilot--connection-alivep))
                         "Ready"
                       "Not started")
            :kind (if (and (featurep 'copilot)
                           (copilot--connection-alivep))
                      "Normal"
                    "Inactive")
            :busy :json-false)))

(defun +copilot-noema-bridge--sync-document (uri file content &optional client-id)
  "Synchronize Noema CONTENT for URI/FILE into the shared Copilot LS."
  (unless (and (require 'copilot nil t)
               (copilot--connection-alivep))
    (clrhash +copilot-noema-bridge--documents)
    (setq +copilot-noema-bridge--notified-focused-uri nil))
  (let* ((language-id (+copilot-noema-bridge--language-id file))
         (current (gethash uri +copilot-noema-bridge--documents))
         (old-content (plist-get current :content)))
    (unless (or (null client-id) (string-empty-p client-id))
      (+copilot-noema-bridge--attach-client client-id uri file "focused"))
    (cond
     ((null current)
      (puthash uri (list :version 1 :content content :language-id language-id)
               +copilot-noema-bridge--documents)
      (+copilot-noema-bridge--notify
       'textDocument/didOpen
       (list :textDocument (list :uri uri
                                 :languageId language-id
                                 :version 1
                                 :text content)))
      (list :version 1 :language-id language-id))
     ((not (string-equal old-content content))
      (let ((version (1+ (or (plist-get current :version) 1))))
        (puthash uri (list :version version :content content :language-id language-id)
                 +copilot-noema-bridge--documents)
        (+copilot-noema-bridge--notify
         'textDocument/didChange
         (list :textDocument (list :uri uri :version version)
               :contentChanges
               (vector
                (list :range (list :start (list :line 0 :character 0)
                                   :end (+copilot-noema-bridge--full-range-end
                                         old-content))
                      :rangeLength (+copilot-noema-bridge--utf16-length
                                    old-content)
                      :text content))))
        (list :version version :language-id language-id)))
     (t
      (list :version (plist-get current :version)
            :language-id (or (plist-get current :language-id) language-id))))))

(defun +copilot-noema-bridge--inline (body)
  "Handle Noema Copilot inline completion BODY."
  (let* ((content (or (plist-get body :content) ""))
         (file (or (plist-get body :file) ""))
         (client-id (+copilot-noema-bridge--client-id body))
         (offset (min (max 0 (or (plist-get body :offset) 0))
                      (+copilot-noema-bridge--utf16-length content)))
         (uri (+copilot-noema-bridge--uri-for-file file))
         (doc (when (+copilot-noema-bridge--client-may-request-p body uri)
                (+copilot-noema-bridge--sync-document
                 uri file content client-id)))
         (version (plist-get doc :version))
         (result nil))
    (if (not doc)
        (progn
          (+copilot-noema-bridge--log
           "inline-skipped"
           `((file . ,file) (uri . ,uri) (clientId . ,client-id)
             (reason . "inactive-client")))
          (list :type "copilot-inline"
                :items []
                :status (+copilot-noema-bridge--status)))
      (+copilot-noema-bridge--focus-document uri client-id file t)
      (setq result
            (condition-case err
                (+copilot-noema-bridge--request
                 'textDocument/inlineCompletion
                 (list :textDocument (list :uri uri :version version)
                       :position (+copilot-noema-bridge--position-for-offset
                                  content offset)
                       :context (list :triggerKind 2)
                       :formattingOptions (list :tabSize 2 :insertSpaces t))
                 :timeout 30)
              (error
               (if (+copilot-noema-bridge--superseded-error-p err)
                   (progn
                     (+copilot-noema-bridge--log
                      "inline-superseded"
                      `((file . ,file) (uri . ,uri) (clientId . ,client-id)))
                     (list :items []))
                 (signal (car err) (cdr err))))))
      (let* ((items (plist-get result :items))
             (item (cl-find-if (lambda (candidate)
                                 (stringp (plist-get candidate :insertText)))
                               (append (or items []) nil))))
        (+copilot-noema-bridge--log
         "inline"
         `((file . ,file)
           (offset . ,offset)
           (items . ,(length (append (or items []) nil)))))
        (if (not item)
            (list :type "copilot-inline"
                  :items []
                  :status (+copilot-noema-bridge--status))
          (let* ((range (plist-get item :range))
                 (start (plist-get range :start))
                 (end (plist-get range :end))
                 (from (if start
                           (+copilot-noema-bridge--offset-for-position
                            content start)
                         offset))
                 (to (if end
                         (+copilot-noema-bridge--offset-for-position
                          content end)
                       offset)))
            (list :type "copilot-inline"
                  :items (vector
                          (list :insertText (plist-get item :insertText)
                                :range (list :from from :to to)
                                :item (+copilot-noema-bridge--json-value
                                       item)))
                  :status (+copilot-noema-bridge--status))))))))

(defun +copilot-noema-bridge--shown (body)
  "Notify Copilot that Noema showed a completion from BODY."
  (when-let* ((item (plist-get body :item)))
    (+copilot-noema-bridge--notify
     'textDocument/didShowCompletion
     (list :item item)))
  (list :ok t))

(defun +copilot-noema-bridge--accept (body)
  "Notify Copilot that Noema accepted a completion from BODY."
  (let* ((item (plist-get body :item))
         (accepted-length (plist-get body :acceptedLength)))
    (cond
     ((not item)
      (list :ok :json-false))
     ((and (numberp accepted-length)
           (>= accepted-length 0)
           (< accepted-length
              (length (or (plist-get item :insertText) ""))))
      (+copilot-noema-bridge--notify
       'textDocument/didPartiallyAcceptCompletion
       (list :item item :acceptedLength accepted-length))
      (list :ok t :partial t))
     (t
      (when-let* ((command (plist-get item :command))
                  (command-name (plist-get command :command)))
        (+copilot-noema-bridge--request
         'workspace/executeCommand
         (list :command command-name
               :arguments (+copilot-noema-bridge--json-array
                           (or (plist-get command :arguments) nil)))
         :timeout 30))
	      (list :ok t)))))

(defun +copilot-noema-bridge--focus (body)
  "Record Noema Copilot focus from BODY without starting Copilot."
  (let* ((file (or (plist-get body :file) ""))
         (uri (+copilot-noema-bridge--uri-for-file file))
         (client-id (+copilot-noema-bridge--client-id body)))
    (+copilot-noema-bridge--focus-document uri client-id file nil)
    (+copilot-noema-bridge--log
     "client-focus"
     `((file . ,file) (uri . ,uri) (clientId . ,client-id)))
    (list :ok t :focused client-id :uri uri)))

(defun +copilot-noema-bridge--blur (body)
  "Record Noema Copilot blur from BODY."
  (let* ((client-id (+copilot-noema-bridge--client-id body))
         (state (and (not (string-empty-p client-id))
                     (gethash client-id +copilot-noema-bridge--clients)))
         (uri (plist-get state :uri)))
    (when state
      (puthash client-id
               (plist-put (copy-sequence state) :state "blurred")
               +copilot-noema-bridge--clients))
    (when (equal +copilot-noema-bridge--focused-client client-id)
      (setq +copilot-noema-bridge--focused-client nil)
      (when (equal +copilot-noema-bridge--focused-uri uri)
        (setq +copilot-noema-bridge--focused-uri nil)))
    (+copilot-noema-bridge--log
     "client-blur"
     `((file . ,(or (plist-get body :file) (plist-get state :file) ""))
       (uri . ,(or uri ""))
       (clientId . ,client-id)))
    (list :ok t :focused (or +copilot-noema-bridge--focused-client ""))))

(defun +copilot-noema-bridge--close (body)
  "Detach an Noema Copilot client from BODY."
  (let* ((client-id (+copilot-noema-bridge--client-id body))
         (state (+copilot-noema-bridge--detach-client client-id "client-close")))
    (+copilot-noema-bridge--log
     "client-close"
     `((file . ,(or (plist-get body :file) (plist-get state :file) ""))
       (uri . ,(or (plist-get state :uri) ""))
       (clientId . ,client-id)
       (clients . ,(hash-table-count +copilot-noema-bridge--clients))))
    (list :ok t
          :closed (if state t :json-false)
          :clients (hash-table-count +copilot-noema-bridge--clients))))

(defun +copilot-noema-bridge--find-string-by-key (value keys)
  "Return first string found in VALUE under one of KEYS."
  (cond
   ((not value) nil)
   ((vectorp value)
    (cl-loop for item across value
             thereis (+copilot-noema-bridge--find-string-by-key
                      item keys)))
   ((and (listp value) (keywordp (car value)))
    (cl-loop for (key item) on value by #'cddr
             thereis (if (and (memq key keys) (stringp item)
                              (not (string-empty-p item)))
                         item
                       (+copilot-noema-bridge--find-string-by-key
                        item keys))))
   (t nil)))

(defun +copilot-noema-bridge--sign-in ()
  "Start a Copilot sign-in flow for Noema."
  (let* ((result (condition-case _err
                     (+copilot-noema-bridge--request 'signIn nil :timeout 30)
                   (error
                    (+copilot-noema-bridge--request
                     'signInInitiate nil :timeout 30))))
         (uri (or (+copilot-noema-bridge--find-string-by-key
                   result '(:verificationUri :verification_uri
                             :verificationUriComplete
                             :verification_uri_complete :uri :url))
                  ""))
         (code (or (+copilot-noema-bridge--find-string-by-key
                    result '(:userCode :user_code :code))
                   "")))
    (when (and (stringp uri)
               (string-match-p "\\`https?://" uri))
      (browse-url uri))
    (append (list :type "copilot-sign-in"
                  :openedUri uri
                  :userCode code
                  :message (cond
                            ((string-empty-p code) "Copilot login started")
                            (t (format "Opened GitHub login; code %s" code)))
                  :status (+copilot-noema-bridge--status))
            (and (listp result) result))))

(defun +copilot-noema-bridge--diagnostics ()
  "Return bridge diagnostics for Noema."
  (list :type "copilot-log"
        :bridge "emacs"
        :port (or (plist-get
                   (remote-gateway-connection-info)
                   :port)
                  0)
        :serverLive (if (+copilot-noema-bridge--server-live-p) t :json-false)
        :copilotLive (if (and (require 'copilot nil t)
                              (copilot--connection-alivep))
                         t :json-false)
        :status (+copilot-noema-bridge--status)
        :documents (hash-table-count +copilot-noema-bridge--documents)
        :clients (hash-table-count +copilot-noema-bridge--clients)
        :focusedUri (or +copilot-noema-bridge--focused-uri "")
        :focusedClient (or +copilot-noema-bridge--focused-client "")
        :notifiedFocusedUri (or +copilot-noema-bridge--notified-focused-uri "")
        :logRecording (if +copilot-noema-bridge--log-recording t :json-false)
        :log (vconcat (reverse +copilot-noema-bridge--log))))

(defun +copilot-noema-bridge--dispatch (action body)
  "Dispatch Noema Copilot ACTION with BODY."
  (pcase action
    ("inline" (+copilot-noema-bridge--inline body))
    ("shown" (+copilot-noema-bridge--shown body))
    ("accept" (+copilot-noema-bridge--accept body))
    ("focus" (+copilot-noema-bridge--focus body))
    ("blur" (+copilot-noema-bridge--blur body))
    ("close" (+copilot-noema-bridge--close body))
    ("sign-in" (+copilot-noema-bridge--sign-in))
    ("sign-out"
     (+copilot-noema-bridge--request 'signOut nil :timeout 30)
     (list :ok t :status (+copilot-noema-bridge--status)))
    ("quota"
     (list :type "copilot-quota"
           :result (condition-case err
                       (+copilot-noema-bridge--request
                        'checkQuota nil :timeout 30)
                     (error (list :error (error-message-string err))))))
    ("status"
     (list :type "copilot-status"
           :result (condition-case _err
                       (+copilot-noema-bridge--request
                        'checkStatus nil :timeout 30)
                     (error nil))
           :status (+copilot-noema-bridge--status)))
    ("log"
     (cond
      ((eq (plist-get body :record) t)
       (when (not (eq (plist-get body :clear) :json-false))
         (setq +copilot-noema-bridge--log nil))
       (setq +copilot-noema-bridge--log-recording t)
       (+copilot-noema-bridge--log "recording-started")
       (append (+copilot-noema-bridge--diagnostics)
               (list :message "Copilot bridge log recording started")))
      ((eq (plist-get body :record) :json-false)
       (+copilot-noema-bridge--log "recording-stopped")
       (setq +copilot-noema-bridge--log-recording nil)
       (append (+copilot-noema-bridge--diagnostics)
               (list :message "Copilot bridge logs recorded")))
      (t (+copilot-noema-bridge--diagnostics))))
    (_ (list :ok :json-false
             :message (format "Unknown Copilot bridge action: %s" action)))))

(defun +copilot-noema-bridge--gateway-plist (value)
  "Convert decoded gateway VALUE into the plist shape used by this bridge."
  (cond
   ((vectorp value)
    (vconcat
     (mapcar #'+copilot-noema-bridge--gateway-plist value)))
   ((and (listp value)
         (cl-every
          (lambda (item)
            (and (consp item)
                 (or (stringp (car item))
                     (symbolp (car item)))))
          value))
    (cl-loop
     for (key . item) in value
     append
     (list (intern (concat ":" (format "%s" key)))
           (+copilot-noema-bridge--gateway-plist item))))
   ((listp value)
    (mapcar #'+copilot-noema-bridge--gateway-plist value))
   (t value)))

(defun +copilot-noema-bridge--gateway-request (params _client)
  "Handle Copilot gateway PARAMS from Noema."
  (let ((action (alist-get "action" params "" nil #'string=))
        (body (alist-get "body" params nil nil #'string=)))
    (+copilot-noema-bridge--dispatch
     action
     (+copilot-noema-bridge--gateway-plist body))))

(remote-gateway-register-method
 "copilot.request" #'+copilot-noema-bridge--gateway-request)

(defun +copilot-buffer-eligible-p ()
  "Return non-nil when the current buffer is cheap enough for Copilot."
  (and (not buffer-read-only)
       (not remote-buffer-disconnected-p)
       (not (minibufferp))
       (or (not +copilot-disable-on-remote)
           (not (file-remote-p default-directory)))
       (or (null +copilot-large-buffer-threshold)
           (<= (buffer-size) +copilot-large-buffer-threshold))))

(defun +copilot--call-on-client (function)
  "Call FUNCTION with the client-local directory and process environment."
  (let ((default-directory temporary-file-directory)
        (process-environment (remote-client-process-environment))
        (exec-path (remote-client-exec-path))
        (remote-current-adapter-id nil)
        (remote-current-route nil)
        (remote-current-workspace nil))
    (funcall function)))

(defun +copilot-available-p ()
  "Return non-nil when Copilot can start in the current environment."
  (and (+copilot-buffer-eligible-p)
       ;; `use-package' only installs the hooks here; the library itself may
       ;; still be unloaded when the first editable buffer opens.
       (or (featurep 'copilot)
           (+copilot--call-on-client
            (lambda () (require 'copilot nil t))))
       (ignore-errors
         (when-let* ((server (copilot-server-executable)))
           (file-exists-p server)))))

(defun +copilot--cancel-auto-enable ()
  "Cancel deferred Copilot startup in the current buffer."
  (when (timerp +copilot--auto-enable-timer)
    (cancel-timer +copilot--auto-enable-timer))
  (setq +copilot--auto-enable-timer nil))

(defun +copilot--disconnect-buffer-h ()
  "Stop this buffer's Copilot subscription without stopping the shared server."
  (+copilot--cancel-auto-enable)
  (when (bound-and-true-p copilot-mode)
    (copilot-mode -1)))

(add-hook 'remote-buffer-disconnect-hook #'+copilot--disconnect-buffer-h)

(defun +copilot--enable-buffer (buffer)
  "Enable Copilot in BUFFER when it is still eligible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq +copilot--auto-enable-timer nil)
      (when (and (not remote-buffer-disconnected-p)
                 (+copilot-available-p))
        (copilot-mode 1)))))

(defun +copilot-auto-enable-h ()
  "Auto-enable `copilot-mode' in supported editing buffers.
Remote, configured modes, and a cold local library start after idle."
  (+copilot--cancel-auto-enable)
  (unless remote-buffer-disconnected-p
    (let* ((remote-p (file-remote-p default-directory))
           (configured-mode-p
            (and +copilot-deferred-modes
                 (apply #'derived-mode-p +copilot-deferred-modes)))
           (delay
            (cond
             ((or (and +copilot-defer-on-remote remote-p)
                  configured-mode-p)
              (or +copilot-deferred-idle-delay 1.5))
             ((and (not remote-p)
                   (not (featurep 'copilot)))
              +copilot-cold-local-idle-delay))))
      (if delay
          (progn
            (setq +copilot--auto-enable-timer
                  (run-with-idle-timer delay nil
                                       #'+copilot--enable-buffer
                                       (current-buffer)))
            (add-hook 'kill-buffer-hook #'+copilot--cancel-auto-enable nil t)
            (add-hook 'change-major-mode-hook #'+copilot--cancel-auto-enable nil t))
        (when (+copilot-available-p)
          (copilot-mode 1))))))

(defun +copilot-completion-visible-p ()
  "Return non-nil when Copilot currently shows a completion overlay."
  (and (bound-and-true-p copilot-mode)
       (fboundp 'copilot--overlay-visible)
       (copilot--overlay-visible)))

(defconst +copilot-jump-alphabet "asdfghjklqweruiop"
  "Prefix-free label alphabet shared with Noema's s-jump UI.")

(defun +copilot--jump-label-less-p (left right alphabet)
  "Return non-nil when jump label LEFT sorts before RIGHT in ALPHABET."
  (if (/= (length left) (length right))
      (< (length left) (length right))
    (let ((index 0)
          (result nil)
          (done nil))
      (while (and (< index (length left)) (not done))
        (let ((delta (- (cl-position (aref left index) alphabet)
                        (cl-position (aref right index) alphabet))))
          (unless (= delta 0)
            (setq result (< delta 0)
                  done t)))
        (setq index (1+ index)))
      result)))

(defun +copilot--jump-labels (count &optional alphabet)
  "Build COUNT prefix-free, nearest-first s-jump labels from ALPHABET."
  (let* ((alphabet (or alphabet +copilot-jump-alphabet))
         (keys (delete-dups (mapcar #'char-to-string (string-to-list alphabet))))
         (target (max 0 count))
         (leaves (copy-sequence keys)))
    (when (and (= (length keys) 1) (> target 1))
      (user-error "Copilot jump labels need at least two keys"))
    (while (< (length leaves) target)
      (let ((expand-at 0))
        (cl-loop for label in leaves
                 for index from 0
                 when (<= (length label) (length (nth expand-at leaves)))
                 do (setq expand-at index))
        (let ((prefix (nth expand-at leaves)))
          (setq leaves
                (append (cl-subseq leaves 0 expand-at)
                        (mapcar (lambda (key) (concat prefix key)) keys)
                        (nthcdr (1+ expand-at) leaves))))))
    (cl-subseq
     (sort leaves (lambda (left right)
                    (+copilot--jump-label-less-p left right alphabet)))
     0 (min target (length leaves)))))

(defun +copilot--jump-candidates (completion)
  "Return bounded (LABEL . LENGTH) targets for COMPLETION's first line."
  (let* ((line-end (or (string-match "\n" completion) (length completion)))
         (count (min 256 line-end))
         (lengths (number-sequence 1 count))
         (labels (+copilot--jump-labels count)))
    (cl-mapcar #'cons labels lengths)))

(defun +copilot--jump-render (completion candidates prefix)
  "Render COMPLETION with CANDIDATES narrowed by PREFIX."
  (let ((from 0)
        pieces)
    (dolist (candidate candidates)
      (let ((to (cdr candidate))
            (label (car candidate)))
        (when (string-prefix-p prefix label)
          (push (propertize (substring label (length prefix))
                            'face '+copilot-jump-label-face)
                pieces))
        (push (substring completion from to) pieces)
        (setq from to)))
    (push (substring completion from) pieces)
    (apply #'concat (nreverse pieces))))

(defun +copilot/accept-completion-jump ()
  "Use temporary s-jump labels to accept an exact Copilot prefix."
  (interactive)
  (let* ((completion (and (fboundp 'copilot-current-completion)
                          (copilot-current-completion)))
         (candidates (and completion (+copilot--jump-candidates completion)))
         (prefix "")
         (finished nil)
         (accepted nil))
    (unless candidates
      (user-error "No visible Copilot text to jump within"))
    (unwind-protect
        (while (not finished)
          (copilot--set-overlay-text
           (copilot--get-overlay)
           (+copilot--jump-render completion candidates prefix))
          (let* ((event (read-event "Copilot jump: label (Esc cancels)"))
                 (basic (event-basic-type event)))
            (cond
             ((memq basic '(escape 27 7))
              (setq finished t))
             ((characterp basic)
              (let* ((key (char-to-string (downcase basic)))
                     (next-prefix (concat prefix key))
                     (matches (cl-remove-if-not
                               (lambda (candidate)
                                 (string-prefix-p next-prefix (car candidate)))
                               candidates))
                     (exact (assoc next-prefix matches)))
                (if (null matches)
                    (setq finished t)
                  (setq prefix next-prefix)
                  (when exact
                    ;; Restore the real completion before invoking the package's
                    ;; normal partial-accept path; the labelled text is UI only.
                    (copilot--set-overlay-text (copilot--get-overlay) completion)
                    (copilot-accept-completion
                     (lambda (_ignored) (substring completion 0 (cdr exact))))
                    (setq accepted t
                          finished t))))))))
      (when (and (not accepted)
                 (fboundp 'copilot--overlay-visible)
                 (copilot--overlay-visible))
        (copilot--set-overlay-text (copilot--get-overlay) completion)))
    accepted))

(defun +copilot/forward-delimiter-or-copilot-dwim ()
  "Prefer active snippet/Copilot actions, then language-specific or delimiter jump."
  (interactive)
  (cond
   ((aaron-snippet-active-p)
    (aaron/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion)
         (+copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (aaron/jump-forward-dwim))))

(defun +copilot/forward-delimiter-or-copilot-by-word-dwim ()
  "Prefer snippet field advance, then Copilot accept-by-word, then language-specific or delimiter jump."
  (interactive)
  (cond
   ((aaron-snippet-active-p)
    (aaron/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion-by-word)
         (+copilot-completion-visible-p))
    (copilot-accept-completion-by-word))
   ((and (fboundp 'copilot-accept-completion)
         (+copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (aaron/jump-forward-dwim))))

(defun +copilot/forward-delimiter-or-copilot-to-char-dwim ()
  "S-jump within visible Copilot text, else advance a snippet or delimiter."
  (interactive)
  (cond
   ((+copilot-completion-visible-p)
    (+copilot/accept-completion-jump))
   ((aaron-snippet-active-p)
    (aaron/snippet-next-field-dwim))
   (t
    (aaron/jump-forward-dwim))))

(defconst +copilot-forward-delimiter-keys
  '("s-]" "s-】" "s-］" "s-」" "s-〕"
    "M-]" "M-】" "M-］" "M-」" "M-〕")
  "Keys that accept Copilot or move forward by delimiter.
Super-] is the primary binding (Command-] on macOS).  The Meta aliases retain
the same workflow when a Mac build maps Command to Meta instead.  Chinese
punctuation variants keep the binding usable while an input method is active.")

(defconst +copilot-backward-delimiter-keys '("M-[" "M-【" "M-［" "M-「" "M-〔")
  "Keys that move backward by delimiter or snippet field.")

(defconst +copilot-by-word-keys '("M-\\" "M-、" "M-＼")
  "Keys that accept Copilot by word or move forward by delimiter.")

(defconst +copilot-to-char-keys
  '("M-}" "M-｝" "M-〗" "M-』")
  "Keys that s-jump within Copilot text or move forward by delimiter.")

(defun +copilot-define-keys (keymap keys command)
  "Bind each key in KEYS to COMMAND in KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) command)))

(defun +copilot-setup-dwim-keys (keymap)
  "Install shared DWIM navigation/accept keys into KEYMAP.
Copilot must not own TAB: while Company is active TAB selects its candidate;
otherwise it falls through to the editor's snippet/indent behavior, matching
the completion routing used before the lsp-mode migration."
  (+copilot-define-keys keymap
                          +copilot-forward-delimiter-keys
                          #'+copilot/forward-delimiter-or-copilot-dwim)
  (+copilot-define-keys keymap
                          +copilot-backward-delimiter-keys
                          #'aaron/backward-delimiter-or-snippet-dwim)
  (+copilot-define-keys keymap
                          +copilot-by-word-keys
                          #'+copilot/forward-delimiter-or-copilot-by-word-dwim)
  (+copilot-define-keys keymap
                          +copilot-to-char-keys
                          #'+copilot/forward-delimiter-or-copilot-to-char-dwim)
  (define-key keymap (kbd "TAB") nil)
  (define-key keymap (kbd "<tab>") nil)
  (define-key keymap (kbd "M-(") nil)
  (define-key keymap (kbd "M-)") nil))

(+copilot-define-keys global-map
                        +copilot-forward-delimiter-keys
                        #'+copilot/forward-delimiter-or-copilot-dwim)
(+copilot-define-keys global-map
                        +copilot-backward-delimiter-keys
                        #'aaron/backward-delimiter-or-snippet-dwim)
(+copilot-define-keys global-map
                        +copilot-by-word-keys
                        #'+copilot/forward-delimiter-or-copilot-by-word-dwim)
(+copilot-define-keys global-map
                        +copilot-to-char-keys
                        #'+copilot/forward-delimiter-or-copilot-to-char-dwim)

(use-package copilot
  :ensure t
  :hook ((prog-mode . +copilot-auto-enable-h)
         (org-mode . +copilot-auto-enable-h)
         (org-src-mode . +copilot-auto-enable-h))
  :custom
  (copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory))
  (copilot-idle-delay +copilot-idle-delay)
  (copilot-indent-offset-warning-disable t)
  (copilot-lsp-settings '(:github (:copilot ())))
  :config
  (+copilot-setup-dwim-keys copilot-mode-map)
  (+copilot-setup-dwim-keys copilot-completion-map)
  (when
      (and
       (fboundp
        'copilot--make-connection@+copilot-cap-server-heap)
       (advice-member-p
        'copilot--make-connection@+copilot-cap-server-heap
        'copilot--make-connection))
    (advice-remove
     'copilot--make-connection
     'copilot--make-connection@+copilot-cap-server-heap))
  (unless
      (advice-member-p
       #'+copilot--client-server-executable-a
       'copilot-server-executable)
    (advice-add
     'copilot-server-executable :around
     #'+copilot--client-server-executable-a))
  (unless
      (advice-member-p
       #'+copilot--make-client-connection-a
       'copilot--make-connection)
    (advice-add
     'copilot--make-connection :around
     #'+copilot--make-client-connection-a))
  (when (and (fboundp 'copilot--shutdown-server-at-exit)
             (not (advice-member-p
                   #'+copilot--nonblocking-exit-a
                   'copilot--shutdown-server-at-exit)))
    (advice-add 'copilot--shutdown-server-at-exit :around
                #'+copilot--nonblocking-exit-a))
  (defun +copilot-check-status ()
    "Report current `copilot.el' authentication status.

Compatibility wrapper for old `lsp-copilot-check-status' workflows."
    (interactive)
    (let* ((response (copilot--request 'checkStatus nil))
           (status (plist-get response :status))
           (user (plist-get response :user)))
      (message "%s"
               (cond
                ((and (stringp user) (not (string-empty-p user)))
                 (format "Copilot is signed in as %s%s"
                         user
                         (if (and (stringp status) (not (string-empty-p status)))
                             (format " [%s]" status)
                           "")))
                ((and (stringp status) (not (string-empty-p status)))
                 (format "Copilot status: %s" status))
                (t
                 (format "Copilot status response: %S" response))))))
  (defun +copilot-check-quota ()
    "Report quota or entitlement information from the Copilot server."
    (interactive)
    (message "Copilot quota: %S" (copilot--request 'checkQuota nil)))
  (when (and (fboundp '+copilot--suppress-cancelled-errors)
             (advice-member-p #'+copilot--suppress-cancelled-errors 'copilot--log))
    (advice-remove 'copilot--log #'+copilot--suppress-cancelled-errors))
  (defalias 'lsp-copilot-check-status #'+copilot-check-status)
  (defalias 'lsp-copilot-login #'copilot-login)
  (defalias 'lsp-copilot-logout #'copilot-logout))





(provide 'tools/copilot/config)
;;; tools/copilot/config.el ends here
