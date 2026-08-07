;;; init-copilot.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

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
(declare-function my/snippet-active-p "init-funcs" ())
(declare-function my/snippet-next-field-dwim "init-funcs" ())
(declare-function my/forward-delimiter-dwim "init-funcs" ())
(declare-function my/jump-forward-dwim "init-funcs" ())
(declare-function my/backward-delimiter-or-snippet-dwim "init-funcs" ())
(defvar copilot--connection nil)
(defvar copilot--overlay nil)
(defvar copilot--quota nil)
(defvar copilot--status nil)
(defvar copilot-log-max)

(defgroup my/copilot nil
  "Copilot integration defaults."
  :group 'tools)

(defface my/copilot-jump-label-face
  '((t (:foreground "#111827" :background "#f7d774" :weight bold)))
  "Face for temporary s-jump labels inside a Copilot ghost completion."
  :group 'my/copilot)

(config-defvar my/copilot-idle-delay nil
  "Idle seconds before Copilot asks for inline completions."
  :type '(choice (number :tag "Seconds of delay") (const :tag "Inline completion disabled" nil))
  :group 'my/copilot)

(config-defvar my/copilot-large-buffer-threshold nil
  "Maximum buffer size where Copilot is auto-enabled.
Large generated files can make inline completion unnecessarily expensive."
  :type 'integer
  :group 'my/copilot)

(config-defvar my/copilot-disable-on-remote nil
  "Whether to skip automatic Copilot startup in remote buffers."
  :type 'boolean
  :group 'my/copilot)

(config-defvar my/copilot-server-max-heap-mb nil
  "V8 heap cap (MB) for the Copilot language server, or nil for no cap.
Applied via `NODE_OPTIONS=--max-old-space-size' on the process this Emacs
spawns.  Emacs-launched Noema now uses
the shared Emacs gateway to reuse that process instead of starting a second
language server; this cap is only mirrored to Noema's local fallback LSP
in standalone runs."
  :type '(choice (integer :tag "Heap cap in MB") (const :tag "No cap" nil))
  :group 'my/copilot)

(config-defvar my/copilot-deferred-modes nil
  "Major modes where automatic Copilot startup waits for editor idle time."
  :type '(repeat symbol)
  :group 'my/copilot)

(config-defvar my/copilot-deferred-idle-delay nil
  "Idle seconds before automatically enabling Copilot in deferred modes."
  :type 'number
  :group 'my/copilot)

(defvar my/copilot-noema-bridge--documents (make-hash-table :test #'equal)
  "Synthetic Noema document state known to the shared Copilot LS.")

(defvar my/copilot-noema-bridge--clients (make-hash-table :test #'equal)
  "Noema Copilot client state keyed by pane/client id.")

(defvar my/copilot-noema-bridge--focused-uri nil
  "Synthetic Noema document URI last focused in the shared Copilot LS.")

(defvar my/copilot-noema-bridge--focused-client nil
  "Noema pane/client id that most recently held Copilot focus.")

(defvar my/copilot-noema-bridge--notified-focused-uri nil
  "Synthetic Noema document URI whose focus was last notified to Copilot.")

(defvar my/copilot-noema-bridge--log nil
  "Bounded recent log entries for Noema Copilot bridge diagnostics.")

(defvar my/copilot-noema-bridge--log-recording nil
  "Non-nil when the Noema Copilot bridge records detailed events.")

(defvar-local my/copilot--auto-enable-timer nil
  "Idle timer used to defer automatic Copilot startup.")

(defun my/copilot--client-process-environment ()
  "Return the client environment used by the local Copilot binary."
  (let ((process-environment (remote-client-process-environment)))
    (if my/copilot-server-max-heap-mb
        (let* ((flag
                (format
                 "--max-old-space-size=%d"
                 my/copilot-server-max-heap-mb))
               (existing (getenv "NODE_OPTIONS")))
          (cons
           (format
            "NODE_OPTIONS=%s"
            (if (and existing (not (string-empty-p existing)))
                (concat existing " " flag)
              flag))
           process-environment))
      process-environment)))

(defun my/copilot--client-server-executable-a (fn &rest args)
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

(defun my/copilot--make-client-process ()
  "Create the Copilot language server at explicit client placement."
  (let* ((client-environment (my/copilot--client-process-environment))
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

(defun my/copilot--make-client-connection-a (_fn)
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
          :process (my/copilot--make-client-process))))
    (condition-case nil
        (funcall
         make-fn
         :events-buffer-config `(:size ,copilot-log-max))
      (invalid-slot-name
       (funcall
        make-fn
        :events-buffer-scrollback-size copilot-log-max)))))

(defun my/copilot-noema-bridge--log (event &optional detail)
  "Record Noema bridge EVENT with DETAIL when bridge logging is enabled."
  (when my/copilot-noema-bridge--log-recording
    (push `((at . ,(format-time-string "%FT%T%z"))
            (event . ,event)
            ,@(when detail `((detail . ,detail))))
          my/copilot-noema-bridge--log)
    (when (> (length my/copilot-noema-bridge--log) 200)
      (setcdr (nthcdr 199 my/copilot-noema-bridge--log) nil))))

(defun my/copilot-noema-bridge--server-live-p ()
  "Return non-nil when the shared Emacs gateway is live."
  (remote-gateway-live-p))

(defun my/copilot-noema-bridge--empty-object ()
  "Return a JSON object that serializes as `{}`."
  (make-hash-table :test #'equal))

(defun my/copilot-noema-bridge--server-bootstrap-buffer ()
  "Return a buffer whose file context gives Copilot a stable workspace root."
  (let ((buffer (get-buffer-create " *Noema copilot bridge*")))
    (with-current-buffer buffer
      (setq-local buffer-file-name
                  (expand-file-name "var/aaronnote/copilot-bridge/bridge.md"
                                    user-emacs-directory))
      (setq-local default-directory
                  (file-name-as-directory user-emacs-directory)))
    buffer))

(defun my/copilot-noema-bridge--ensure-copilot ()
  "Ensure `copilot.el' is loaded and the shared LS connection is alive."
  (unless (require 'copilot nil t)
    (error "copilot.el is unavailable"))
  (unless (copilot--connection-alivep)
    (clrhash my/copilot-noema-bridge--documents)
    (clrhash my/copilot-noema-bridge--clients)
    (setq my/copilot-noema-bridge--focused-uri nil
          my/copilot-noema-bridge--focused-client nil
          my/copilot-noema-bridge--notified-focused-uri nil)
    (with-current-buffer (my/copilot-noema-bridge--server-bootstrap-buffer)
      (copilot--start-server))))

(defun my/copilot-noema-bridge--request (method &optional params &rest args)
  "Send METHOD with PARAMS to the shared Copilot LS and return its result."
  (my/copilot-noema-bridge--ensure-copilot)
  (apply #'jsonrpc-request
         copilot--connection
         method
         (or params (my/copilot-noema-bridge--empty-object))
         args))

(defun my/copilot-noema-bridge--superseded-error-p (err)
  "Return non-nil when ERR is Copilot's stale inline request cancellation."
  (let ((text (format "%S" err)))
    (or (string-match-p "jsonrpc-error-code[[:space:]\n]*\\.[[:space:]\n]*-32802" text)
        (string-match-p "Request was superseded by a new request" text))))

(defun my/copilot-noema-bridge--notify (method params)
  "Send METHOD notification with PARAMS to the shared Copilot LS."
  (my/copilot-noema-bridge--ensure-copilot)
  (jsonrpc-notify copilot--connection method params))

(defun my/copilot-noema-bridge--language-id (file)
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

(defun my/copilot-noema-bridge--uri-for-file (file)
  "Return a synthetic Noema Copilot URI for FILE.
The URI intentionally does not equal the real file URI, so Noema document
sync never collides with a normal Emacs buffer already opened in `copilot.el'."
  (unless (require 'copilot nil t)
    (error "copilot.el is unavailable"))
  (let* ((raw (if (and (stringp file) (not (string-empty-p file)))
                  (expand-file-name file)
                "aaronnote-copilot.md"))
         (ext (or (file-name-extension raw t) ".md"))
         (base (file-name-nondirectory raw))
         (hash (secure-hash 'sha1 raw))
         (synthetic (expand-file-name
                     (format "var/aaronnote/copilot-bridge/%s-%s%s"
                             hash
                             (file-name-base base)
                             ext)
                     user-emacs-directory)))
    (copilot--path-to-uri synthetic)))

(defun my/copilot-noema-bridge--client-id (body)
  "Return Noema client id from BODY, or an empty string."
  (let ((value (or (plist-get body :clientId)
                   (plist-get body :client)
                   "")))
    (if (stringp value) (string-trim value) "")))

(defun my/copilot-noema-bridge--body-active-p (body)
  "Return non-nil when BODY represents an active/focused pane."
  (and (not (eq (plist-get body :active) :json-false))
       (not (eq (plist-get body :focused) :json-false))))

(defun my/copilot-noema-bridge--copilot-live-p ()
  "Return non-nil when the shared Copilot LS connection is alive."
  (and (require 'copilot nil t)
       (copilot--connection-alivep)))

(defun my/copilot-noema-bridge--notify-if-live (method params)
  "Send METHOD notification with PARAMS without starting Copilot."
  (when (my/copilot-noema-bridge--copilot-live-p)
    (jsonrpc-notify copilot--connection method params)))

(defun my/copilot-noema-bridge--document-client-count (uri)
  "Return number of Noema clients still attached to URI."
  (let ((count 0))
    (maphash
     (lambda (_client state)
       (when (equal (plist-get state :uri) uri)
         (setq count (1+ count))))
     my/copilot-noema-bridge--clients)
    count))

(defun my/copilot-noema-bridge--close-document (uri &optional reason)
  "Close URI in Copilot when Noema has no remaining clients."
  (when (gethash uri my/copilot-noema-bridge--documents)
    (remhash uri my/copilot-noema-bridge--documents)
    (when (equal my/copilot-noema-bridge--focused-uri uri)
      (setq my/copilot-noema-bridge--focused-uri nil))
    (when (equal my/copilot-noema-bridge--notified-focused-uri uri)
      (setq my/copilot-noema-bridge--notified-focused-uri nil))
    (my/copilot-noema-bridge--notify-if-live
     'textDocument/didClose
     (list :textDocument (list :uri uri)))
    (my/copilot-noema-bridge--log
     "document-close"
     `((uri . ,uri) (reason . ,(or reason ""))))))

(defun my/copilot-noema-bridge--detach-client (client-id &optional reason)
  "Detach CLIENT-ID from its Noema Copilot document."
  (unless (string-empty-p client-id)
    (let* ((state (gethash client-id my/copilot-noema-bridge--clients))
           (uri (plist-get state :uri)))
      (when state
        (remhash client-id my/copilot-noema-bridge--clients)
        (when (equal my/copilot-noema-bridge--focused-client client-id)
          (setq my/copilot-noema-bridge--focused-client nil)
          (when (equal my/copilot-noema-bridge--focused-uri uri)
            (setq my/copilot-noema-bridge--focused-uri nil)))
        (when (and uri
                   (= (my/copilot-noema-bridge--document-client-count uri) 0)
                   (not (equal my/copilot-noema-bridge--focused-uri uri)))
          (my/copilot-noema-bridge--close-document uri reason)))
      state)))

(defun my/copilot-noema-bridge--attach-client (client-id uri file state)
  "Attach CLIENT-ID to URI/FILE with lifecycle STATE."
  (unless (or (string-empty-p client-id)
              (string-empty-p uri))
    (let ((previous (gethash client-id my/copilot-noema-bridge--clients)))
      (when (and previous
                 (not (equal (plist-get previous :uri) uri)))
        (my/copilot-noema-bridge--detach-client client-id "switch-file")))
    (puthash client-id
             (list :uri uri
                   :file file
                   :state state
                   :updated-at (float-time))
             my/copilot-noema-bridge--clients)))

(defun my/copilot-noema-bridge--focus-document (uri client-id file notify)
  "Mark URI as focused by CLIENT-ID and optionally notify Copilot."
  (unless (string-empty-p client-id)
    (my/copilot-noema-bridge--attach-client client-id uri file "focused"))
  (setq my/copilot-noema-bridge--focused-uri uri
        my/copilot-noema-bridge--focused-client
        (unless (string-empty-p client-id) client-id))
  (when (and notify
             (gethash uri my/copilot-noema-bridge--documents)
             (not (equal my/copilot-noema-bridge--notified-focused-uri uri)))
    (my/copilot-noema-bridge--notify
     'textDocument/didFocus
     (list :textDocument (list :uri uri)))
    (setq my/copilot-noema-bridge--notified-focused-uri uri)
    (my/copilot-noema-bridge--log
     "document-focus"
     `((uri . ,uri) (file . ,file) (clientId . ,client-id)))))

(defun my/copilot-noema-bridge--client-may-request-p (body uri)
  "Return non-nil when BODY's client may request inline completion for URI."
  (let* ((client-id (my/copilot-noema-bridge--client-id body))
         (state (and (not (string-empty-p client-id))
                     (gethash client-id my/copilot-noema-bridge--clients)))
         (focused-client my/copilot-noema-bridge--focused-client)
         (focused-uri my/copilot-noema-bridge--focused-uri))
    (and (my/copilot-noema-bridge--body-active-p body)
         (or (string-empty-p client-id)
             (not (and (stringp focused-client)
                       (not (string-empty-p focused-client))))
             (equal focused-client client-id)
             (equal (plist-get state :state) "focused"))
         (or (not (and (stringp focused-uri)
                       (not (string-empty-p focused-uri))))
             (equal focused-uri uri)
             (equal (plist-get state :state) "focused")))))

(defun my/copilot-noema-bridge--utf16-units (char)
  "Return the UTF-16 code-unit width of CHAR."
  (if (> char #xffff) 2 1))

(defun my/copilot-noema-bridge--utf16-length (text)
  "Return the UTF-16 code-unit length of TEXT."
  (let ((units 0))
    (dotimes (i (length text) units)
      (setq units (+ units
                     (my/copilot-noema-bridge--utf16-units
                      (aref text i)))))))

(defun my/copilot-noema-bridge--json-value (value)
  "Return VALUE with nested JSON arrays represented as vectors."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'my/copilot-noema-bridge--json-value
                     (append value nil))))
   ((and (listp value) (keywordp (car value)))
    (cl-loop for (key item) on value by #'cddr
             append (list key
                          (my/copilot-noema-bridge--json-value item))))
   ((consp value)
    (vconcat (mapcar #'my/copilot-noema-bridge--json-value value)))
   (t value)))

(defun my/copilot-noema-bridge--json-array (value)
  "Return VALUE normalized as a JSON array."
  (cond
   ((null value) [])
   ((vectorp value)
    (my/copilot-noema-bridge--json-value value))
   ((listp value)
    (vconcat (mapcar #'my/copilot-noema-bridge--json-value value)))
   (t (vector (my/copilot-noema-bridge--json-value value)))))

(defun my/copilot-noema-bridge--position-for-offset (text offset)
  "Return LSP position in TEXT for JavaScript UTF-16 OFFSET."
  (let ((limit (max 0 (or offset 0)))
        (units 0)
        (line 0)
        (character 0)
        (i 0)
        (len (length text)))
    (while (and (< i len) (< units limit))
      (let* ((char (aref text i))
             (width (my/copilot-noema-bridge--utf16-units char)))
        (if (= char ?\n)
            (setq line (1+ line)
                  character 0)
          (setq character (+ character width)))
        (setq units (+ units width)
              i (1+ i))))
    (list :line line :character character)))

(defun my/copilot-noema-bridge--offset-for-position (text position)
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
               (width (my/copilot-noema-bridge--utf16-units char)))
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

(defun my/copilot-noema-bridge--full-range-end (text)
  "Return LSP position at the end of TEXT."
  (my/copilot-noema-bridge--position-for-offset
   text
   (my/copilot-noema-bridge--utf16-length text)))

(defun my/copilot-noema-bridge--status ()
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

(defun my/copilot-noema-bridge--sync-document (uri file content &optional client-id)
  "Synchronize Noema CONTENT for URI/FILE into the shared Copilot LS."
  (unless (and (require 'copilot nil t)
               (copilot--connection-alivep))
    (clrhash my/copilot-noema-bridge--documents)
    (setq my/copilot-noema-bridge--notified-focused-uri nil))
  (let* ((language-id (my/copilot-noema-bridge--language-id file))
         (current (gethash uri my/copilot-noema-bridge--documents))
         (old-content (plist-get current :content)))
    (unless (or (null client-id) (string-empty-p client-id))
      (my/copilot-noema-bridge--attach-client client-id uri file "focused"))
    (cond
     ((null current)
      (puthash uri (list :version 1 :content content :language-id language-id)
               my/copilot-noema-bridge--documents)
      (my/copilot-noema-bridge--notify
       'textDocument/didOpen
       (list :textDocument (list :uri uri
                                 :languageId language-id
                                 :version 1
                                 :text content)))
      (list :version 1 :language-id language-id))
     ((not (string-equal old-content content))
      (let ((version (1+ (or (plist-get current :version) 1))))
        (puthash uri (list :version version :content content :language-id language-id)
                 my/copilot-noema-bridge--documents)
        (my/copilot-noema-bridge--notify
         'textDocument/didChange
         (list :textDocument (list :uri uri :version version)
               :contentChanges
               (vector
                (list :range (list :start (list :line 0 :character 0)
                                   :end (my/copilot-noema-bridge--full-range-end
                                         old-content))
                      :rangeLength (my/copilot-noema-bridge--utf16-length
                                    old-content)
                      :text content))))
        (list :version version :language-id language-id)))
     (t
      (list :version (plist-get current :version)
            :language-id (or (plist-get current :language-id) language-id))))))

(defun my/copilot-noema-bridge--inline (body)
  "Handle Noema Copilot inline completion BODY."
  (let* ((content (or (plist-get body :content) ""))
         (file (or (plist-get body :file) ""))
         (client-id (my/copilot-noema-bridge--client-id body))
         (offset (min (max 0 (or (plist-get body :offset) 0))
                      (my/copilot-noema-bridge--utf16-length content)))
         (uri (my/copilot-noema-bridge--uri-for-file file))
         (doc (when (my/copilot-noema-bridge--client-may-request-p body uri)
                (my/copilot-noema-bridge--sync-document
                 uri file content client-id)))
         (version (plist-get doc :version))
         (result nil))
    (if (not doc)
        (progn
          (my/copilot-noema-bridge--log
           "inline-skipped"
           `((file . ,file) (uri . ,uri) (clientId . ,client-id)
             (reason . "inactive-client")))
          (list :type "copilot-inline"
                :items []
                :status (my/copilot-noema-bridge--status)))
      (my/copilot-noema-bridge--focus-document uri client-id file t)
      (setq result
            (condition-case err
                (my/copilot-noema-bridge--request
                 'textDocument/inlineCompletion
                 (list :textDocument (list :uri uri :version version)
                       :position (my/copilot-noema-bridge--position-for-offset
                                  content offset)
                       :context (list :triggerKind 2)
                       :formattingOptions (list :tabSize 2 :insertSpaces t))
                 :timeout 30)
              (error
               (if (my/copilot-noema-bridge--superseded-error-p err)
                   (progn
                     (my/copilot-noema-bridge--log
                      "inline-superseded"
                      `((file . ,file) (uri . ,uri) (clientId . ,client-id)))
                     (list :items []))
                 (signal (car err) (cdr err))))))
      (let* ((items (plist-get result :items))
             (item (cl-find-if (lambda (candidate)
                                 (stringp (plist-get candidate :insertText)))
                               (append (or items []) nil))))
        (my/copilot-noema-bridge--log
         "inline"
         `((file . ,file)
           (offset . ,offset)
           (items . ,(length (append (or items []) nil)))))
        (if (not item)
            (list :type "copilot-inline"
                  :items []
                  :status (my/copilot-noema-bridge--status))
          (let* ((range (plist-get item :range))
                 (start (plist-get range :start))
                 (end (plist-get range :end))
                 (from (if start
                           (my/copilot-noema-bridge--offset-for-position
                            content start)
                         offset))
                 (to (if end
                         (my/copilot-noema-bridge--offset-for-position
                          content end)
                       offset)))
            (list :type "copilot-inline"
                  :items (vector
                          (list :insertText (plist-get item :insertText)
                                :range (list :from from :to to)
                                :item (my/copilot-noema-bridge--json-value
                                       item)))
                  :status (my/copilot-noema-bridge--status))))))))

(defun my/copilot-noema-bridge--shown (body)
  "Notify Copilot that Noema showed a completion from BODY."
  (when-let* ((item (plist-get body :item)))
    (my/copilot-noema-bridge--notify
     'textDocument/didShowCompletion
     (list :item item)))
  (list :ok t))

(defun my/copilot-noema-bridge--accept (body)
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
      (my/copilot-noema-bridge--notify
       'textDocument/didPartiallyAcceptCompletion
       (list :item item :acceptedLength accepted-length))
      (list :ok t :partial t))
     (t
      (when-let* ((command (plist-get item :command))
                  (command-name (plist-get command :command)))
        (my/copilot-noema-bridge--request
         'workspace/executeCommand
         (list :command command-name
               :arguments (my/copilot-noema-bridge--json-array
                           (or (plist-get command :arguments) nil)))
         :timeout 30))
	      (list :ok t)))))

(defun my/copilot-noema-bridge--focus (body)
  "Record Noema Copilot focus from BODY without starting Copilot."
  (let* ((file (or (plist-get body :file) ""))
         (uri (my/copilot-noema-bridge--uri-for-file file))
         (client-id (my/copilot-noema-bridge--client-id body)))
    (my/copilot-noema-bridge--focus-document uri client-id file nil)
    (my/copilot-noema-bridge--log
     "client-focus"
     `((file . ,file) (uri . ,uri) (clientId . ,client-id)))
    (list :ok t :focused client-id :uri uri)))

(defun my/copilot-noema-bridge--blur (body)
  "Record Noema Copilot blur from BODY."
  (let* ((client-id (my/copilot-noema-bridge--client-id body))
         (state (and (not (string-empty-p client-id))
                     (gethash client-id my/copilot-noema-bridge--clients)))
         (uri (plist-get state :uri)))
    (when state
      (puthash client-id
               (plist-put (copy-sequence state) :state "blurred")
               my/copilot-noema-bridge--clients))
    (when (equal my/copilot-noema-bridge--focused-client client-id)
      (setq my/copilot-noema-bridge--focused-client nil)
      (when (equal my/copilot-noema-bridge--focused-uri uri)
        (setq my/copilot-noema-bridge--focused-uri nil)))
    (my/copilot-noema-bridge--log
     "client-blur"
     `((file . ,(or (plist-get body :file) (plist-get state :file) ""))
       (uri . ,(or uri ""))
       (clientId . ,client-id)))
    (list :ok t :focused (or my/copilot-noema-bridge--focused-client ""))))

(defun my/copilot-noema-bridge--close (body)
  "Detach an Noema Copilot client from BODY."
  (let* ((client-id (my/copilot-noema-bridge--client-id body))
         (state (my/copilot-noema-bridge--detach-client client-id "client-close")))
    (my/copilot-noema-bridge--log
     "client-close"
     `((file . ,(or (plist-get body :file) (plist-get state :file) ""))
       (uri . ,(or (plist-get state :uri) ""))
       (clientId . ,client-id)
       (clients . ,(hash-table-count my/copilot-noema-bridge--clients))))
    (list :ok t
          :closed (if state t :json-false)
          :clients (hash-table-count my/copilot-noema-bridge--clients))))

(defun my/copilot-noema-bridge--find-string-by-key (value keys)
  "Return first string found in VALUE under one of KEYS."
  (cond
   ((not value) nil)
   ((vectorp value)
    (cl-loop for item across value
             thereis (my/copilot-noema-bridge--find-string-by-key
                      item keys)))
   ((and (listp value) (keywordp (car value)))
    (cl-loop for (key item) on value by #'cddr
             thereis (if (and (memq key keys) (stringp item)
                              (not (string-empty-p item)))
                         item
                       (my/copilot-noema-bridge--find-string-by-key
                        item keys))))
   (t nil)))

(defun my/copilot-noema-bridge--sign-in ()
  "Start a Copilot sign-in flow for Noema."
  (let* ((result (condition-case _err
                     (my/copilot-noema-bridge--request 'signIn nil :timeout 30)
                   (error
                    (my/copilot-noema-bridge--request
                     'signInInitiate nil :timeout 30))))
         (uri (or (my/copilot-noema-bridge--find-string-by-key
                   result '(:verificationUri :verification_uri
                             :verificationUriComplete
                             :verification_uri_complete :uri :url))
                  ""))
         (code (or (my/copilot-noema-bridge--find-string-by-key
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
                  :status (my/copilot-noema-bridge--status))
            (and (listp result) result))))

(defun my/copilot-noema-bridge--diagnostics ()
  "Return bridge diagnostics for Noema."
  (list :type "copilot-log"
        :bridge "emacs"
        :port (or (plist-get
                   (remote-gateway-connection-info)
                   :port)
                  0)
        :serverLive (if (my/copilot-noema-bridge--server-live-p) t :json-false)
        :copilotLive (if (and (require 'copilot nil t)
                              (copilot--connection-alivep))
                         t :json-false)
        :status (my/copilot-noema-bridge--status)
        :documents (hash-table-count my/copilot-noema-bridge--documents)
        :clients (hash-table-count my/copilot-noema-bridge--clients)
        :focusedUri (or my/copilot-noema-bridge--focused-uri "")
        :focusedClient (or my/copilot-noema-bridge--focused-client "")
        :notifiedFocusedUri (or my/copilot-noema-bridge--notified-focused-uri "")
        :logRecording (if my/copilot-noema-bridge--log-recording t :json-false)
        :log (vconcat (reverse my/copilot-noema-bridge--log))))

(defun my/copilot-noema-bridge--dispatch (action body)
  "Dispatch Noema Copilot ACTION with BODY."
  (pcase action
    ("inline" (my/copilot-noema-bridge--inline body))
    ("shown" (my/copilot-noema-bridge--shown body))
    ("accept" (my/copilot-noema-bridge--accept body))
    ("focus" (my/copilot-noema-bridge--focus body))
    ("blur" (my/copilot-noema-bridge--blur body))
    ("close" (my/copilot-noema-bridge--close body))
    ("sign-in" (my/copilot-noema-bridge--sign-in))
    ("sign-out"
     (my/copilot-noema-bridge--request 'signOut nil :timeout 30)
     (list :ok t :status (my/copilot-noema-bridge--status)))
    ("quota"
     (list :type "copilot-quota"
           :result (condition-case err
                       (my/copilot-noema-bridge--request
                        'checkQuota nil :timeout 30)
                     (error (list :error (error-message-string err))))))
    ("status"
     (list :type "copilot-status"
           :result (condition-case _err
                       (my/copilot-noema-bridge--request
                        'checkStatus nil :timeout 30)
                     (error nil))
           :status (my/copilot-noema-bridge--status)))
    ("log"
     (cond
      ((eq (plist-get body :record) t)
       (when (not (eq (plist-get body :clear) :json-false))
         (setq my/copilot-noema-bridge--log nil))
       (setq my/copilot-noema-bridge--log-recording t)
       (my/copilot-noema-bridge--log "recording-started")
       (append (my/copilot-noema-bridge--diagnostics)
               (list :message "Copilot bridge log recording started")))
      ((eq (plist-get body :record) :json-false)
       (my/copilot-noema-bridge--log "recording-stopped")
       (setq my/copilot-noema-bridge--log-recording nil)
       (append (my/copilot-noema-bridge--diagnostics)
               (list :message "Copilot bridge logs recorded")))
      (t (my/copilot-noema-bridge--diagnostics))))
    (_ (list :ok :json-false
             :message (format "Unknown Copilot bridge action: %s" action)))))

(defun my/copilot-noema-bridge--gateway-plist (value)
  "Convert decoded gateway VALUE into the plist shape used by this bridge."
  (cond
   ((vectorp value)
    (vconcat
     (mapcar #'my/copilot-noema-bridge--gateway-plist value)))
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
           (my/copilot-noema-bridge--gateway-plist item))))
   ((listp value)
    (mapcar #'my/copilot-noema-bridge--gateway-plist value))
   (t value)))

(defun my/copilot-noema-bridge--gateway-request (params _client)
  "Handle Copilot gateway PARAMS from Noema."
  (let ((action (alist-get "action" params "" nil #'string=))
        (body (alist-get "body" params nil nil #'string=)))
    (my/copilot-noema-bridge--dispatch
     action
     (my/copilot-noema-bridge--gateway-plist body))))

(remote-gateway-register-method
 "copilot.request" #'my/copilot-noema-bridge--gateway-request)

(defun my/copilot-buffer-eligible-p ()
  "Return non-nil when the current buffer is cheap enough for Copilot."
  (and (not buffer-read-only)
       (not (minibufferp))
       (or (not my/copilot-disable-on-remote)
           (not (file-remote-p default-directory)))
       (or (null my/copilot-large-buffer-threshold)
           (<= (buffer-size) my/copilot-large-buffer-threshold))))

(defun my/copilot-available-p ()
  "Return non-nil when Copilot can start in the current environment."
  (and (my/copilot-buffer-eligible-p)
       ;; `use-package' only installs the hooks here; the library itself may
       ;; still be unloaded when the first editable buffer opens.
       (or (featurep 'copilot)
           (require 'copilot nil t))
       (ignore-errors
         (when-let* ((server (copilot-server-executable)))
           (file-exists-p server)))))

(defun my/copilot--cancel-auto-enable ()
  "Cancel deferred Copilot startup in the current buffer."
  (when (timerp my/copilot--auto-enable-timer)
    (cancel-timer my/copilot--auto-enable-timer))
  (setq my/copilot--auto-enable-timer nil))

(defun my/copilot--enable-buffer (buffer)
  "Enable Copilot in BUFFER when it is still eligible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/copilot--auto-enable-timer nil)
      (when (my/copilot-available-p)
        (copilot-mode 1)))))

(defun my/copilot-auto-enable-h ()
  "Auto-enable `copilot-mode' in supported editing buffers.
Modes in `my/copilot-deferred-modes' start only after editor idle time."
  (my/copilot--cancel-auto-enable)
  (if (and my/copilot-deferred-modes
           (apply #'derived-mode-p my/copilot-deferred-modes))
      (progn
        (setq my/copilot--auto-enable-timer
              (run-with-idle-timer my/copilot-deferred-idle-delay nil
                                   #'my/copilot--enable-buffer
                                   (current-buffer)))
        (add-hook 'kill-buffer-hook #'my/copilot--cancel-auto-enable nil t)
        (add-hook 'change-major-mode-hook #'my/copilot--cancel-auto-enable nil t))
    (when (my/copilot-available-p)
      (copilot-mode 1))))

(defun my/copilot-completion-visible-p ()
  "Return non-nil when Copilot currently shows a completion overlay."
  (and (bound-and-true-p copilot-mode)
       (fboundp 'copilot--overlay-visible)
       (copilot--overlay-visible)))

(defconst my/copilot-jump-alphabet "asdfghjklqweruiop"
  "Prefix-free label alphabet shared with Noema's s-jump UI.")

(defun my/copilot--jump-label-less-p (left right alphabet)
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

(defun my/copilot--jump-labels (count &optional alphabet)
  "Build COUNT prefix-free, nearest-first s-jump labels from ALPHABET."
  (let* ((alphabet (or alphabet my/copilot-jump-alphabet))
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
                    (my/copilot--jump-label-less-p left right alphabet)))
     0 (min target (length leaves)))))

(defun my/copilot--jump-candidates (completion)
  "Return bounded (LABEL . LENGTH) targets for COMPLETION's first line."
  (let* ((line-end (or (string-match "\n" completion) (length completion)))
         (count (min 256 line-end))
         (lengths (number-sequence 1 count))
         (labels (my/copilot--jump-labels count)))
    (cl-mapcar #'cons labels lengths)))

(defun my/copilot--jump-render (completion candidates prefix)
  "Render COMPLETION with CANDIDATES narrowed by PREFIX."
  (let ((from 0)
        pieces)
    (dolist (candidate candidates)
      (let ((to (cdr candidate))
            (label (car candidate)))
        (when (string-prefix-p prefix label)
          (push (propertize (substring label (length prefix))
                            'face 'my/copilot-jump-label-face)
                pieces))
        (push (substring completion from to) pieces)
        (setq from to)))
    (push (substring completion from) pieces)
    (apply #'concat (nreverse pieces))))

(defun my/copilot-accept-completion-jump ()
  "Use temporary s-jump labels to accept an exact Copilot prefix."
  (interactive)
  (let* ((completion (and (fboundp 'copilot-current-completion)
                          (copilot-current-completion)))
         (candidates (and completion (my/copilot--jump-candidates completion)))
         (prefix "")
         (finished nil)
         (accepted nil))
    (unless candidates
      (user-error "No visible Copilot text to jump within"))
    (unwind-protect
        (while (not finished)
          (copilot--set-overlay-text
           (copilot--get-overlay)
           (my/copilot--jump-render completion candidates prefix))
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

(defun my/forward-delimiter-or-copilot-dwim ()
  "Prefer active snippet/Copilot actions, then language-specific or delimiter jump."
  (interactive)
  (cond
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (my/jump-forward-dwim))))

(defun my/forward-delimiter-or-copilot-by-word-dwim ()
  "Prefer snippet field advance, then Copilot accept-by-word, then language-specific or delimiter jump."
  (interactive)
  (cond
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   ((and (fboundp 'copilot-accept-completion-by-word)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion-by-word))
   ((and (fboundp 'copilot-accept-completion)
         (my/copilot-completion-visible-p))
    (copilot-accept-completion))
   (t
    (my/jump-forward-dwim))))

(defun my/forward-delimiter-or-copilot-to-char-dwim ()
  "S-jump within visible Copilot text, else advance a snippet or delimiter."
  (interactive)
  (cond
   ((my/copilot-completion-visible-p)
    (my/copilot-accept-completion-jump))
   ((my/snippet-active-p)
    (my/snippet-next-field-dwim))
   (t
    (my/jump-forward-dwim))))

(defconst my/copilot-forward-delimiter-keys '("M-]" "M-】" "M-］" "M-」" "M-〕")
  "Keys that accept Copilot or move forward by delimiter.
The Chinese punctuation variants keep the binding usable when a Chinese input
method maps the bracket key to full-width punctuation.")

(defconst my/copilot-backward-delimiter-keys '("M-[" "M-【" "M-［" "M-「" "M-〔")
  "Keys that move backward by delimiter or snippet field.")

(defconst my/copilot-by-word-keys '("M-\\" "M-、" "M-＼")
  "Keys that accept Copilot by word or move forward by delimiter.")

(defconst my/copilot-to-char-keys
  '("M-}" "M-｝" "M-〗" "M-』")
  "Keys that s-jump within Copilot text or move forward by delimiter.")

(defun my/copilot-define-keys (keymap keys command)
  "Bind each key in KEYS to COMMAND in KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) command)))

(defun my/copilot-setup-dwim-keys (keymap)
  "Install shared DWIM navigation/accept keys into KEYMAP."
  (my/copilot-define-keys keymap
                          my/copilot-forward-delimiter-keys
                          #'my/forward-delimiter-or-copilot-dwim)
  (my/copilot-define-keys keymap
                          my/copilot-backward-delimiter-keys
                          #'my/backward-delimiter-or-snippet-dwim)
  (my/copilot-define-keys keymap
                          my/copilot-by-word-keys
                          #'my/forward-delimiter-or-copilot-by-word-dwim)
  (my/copilot-define-keys keymap
                          my/copilot-to-char-keys
                          #'my/forward-delimiter-or-copilot-to-char-dwim)
  (define-key keymap (kbd "M-(") nil)
  (define-key keymap (kbd "M-)") nil))

(my/copilot-define-keys global-map
                        my/copilot-forward-delimiter-keys
                        #'my/forward-delimiter-or-copilot-dwim)
(my/copilot-define-keys global-map
                        my/copilot-backward-delimiter-keys
                        #'my/backward-delimiter-or-snippet-dwim)
(my/copilot-define-keys global-map
                        my/copilot-by-word-keys
                        #'my/forward-delimiter-or-copilot-by-word-dwim)
(my/copilot-define-keys global-map
                        my/copilot-to-char-keys
                        #'my/forward-delimiter-or-copilot-to-char-dwim)

(use-package copilot
  :ensure t
  :hook ((prog-mode . my/copilot-auto-enable-h)
         (org-mode . my/copilot-auto-enable-h)
         (org-src-mode . my/copilot-auto-enable-h))
  :custom
  (copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory))
  (copilot-idle-delay my/copilot-idle-delay)
  (copilot-indent-offset-warning-disable t)
  (copilot-lsp-settings '(:github (:copilot ())))
  :config
  (my/copilot-setup-dwim-keys copilot-mode-map)
  (my/copilot-setup-dwim-keys copilot-completion-map)
  (when
      (and
       (fboundp
        'copilot--make-connection@my/copilot-cap-server-heap)
       (advice-member-p
        'copilot--make-connection@my/copilot-cap-server-heap
        'copilot--make-connection))
    (advice-remove
     'copilot--make-connection
     'copilot--make-connection@my/copilot-cap-server-heap))
  (unless
      (advice-member-p
       #'my/copilot--client-server-executable-a
       'copilot-server-executable)
    (advice-add
     'copilot-server-executable :around
     #'my/copilot--client-server-executable-a))
  (unless
      (advice-member-p
       #'my/copilot--make-client-connection-a
       'copilot--make-connection)
    (advice-add
     'copilot--make-connection :around
     #'my/copilot--make-client-connection-a))
  (defun my/copilot-check-status ()
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
  (defun my/copilot-check-quota ()
    "Report quota or entitlement information from the Copilot server."
    (interactive)
    (message "Copilot quota: %S" (copilot--request 'checkQuota nil)))
  (when (and (fboundp 'my/copilot--suppress-cancelled-errors)
             (advice-member-p #'my/copilot--suppress-cancelled-errors 'copilot--log))
    (advice-remove 'copilot--log #'my/copilot--suppress-cancelled-errors))
  (defalias 'lsp-copilot-check-status #'my/copilot-check-status)
  (defalias 'lsp-copilot-login #'copilot-login)
  (defalias 'lsp-copilot-logout #'copilot-logout))





(provide 'init-copilot)
;;; init-copilot.el ends here
