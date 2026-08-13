;;; remote-gateway.el --- Unified external JSON-RPC gateway -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One loopback listener is the control-plane boundary between Emacs and
;; helpers such as Lean and Noema.  HTTP and WebSocket carry the same
;; JSON-RPC 2.0 messages.  Remote targets reach the listener only through a
;; workspace-owned Remote reverse forward.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'web-server)
(require 'remote-channel)
(require 'remote-workspace)

(defgroup remote-gateway nil
  "Unified external communication gateway."
  :group 'remote)

(defcustom remote-gateway-host "127.0.0.1"
  "Loopback address used by the gateway listener."
  :type 'string
  :group 'remote-gateway)

(defcustom remote-gateway-request-timeout 10
  "Default number of seconds to wait for a peer response."
  :type 'number
  :group 'remote-gateway)

(defcustom remote-gateway-discovery-directory
  (locate-user-emacs-file "var/emacs-gateway/")
  "Directory containing per-Emacs gateway discovery records."
  :type 'directory
  :group 'remote-gateway)

(cl-defstruct (remote-gateway-client
               (:constructor remote-gateway-client-create))
  key client-id instance-id target-id workspace-id process channel
  endpoint provides connected-at last-seen metadata)

(cl-defstruct (remote-gateway-binding
               (:constructor remote-gateway-binding-create))
  id client-id target-id workspace-id context endpoint provides created-at)

(cl-defstruct (remote-gateway-pending
               (:constructor remote-gateway-pending-create))
  id client state callback timer)

(cl-defstruct (remote-gateway-deferred
               (:constructor remote-gateway-deferred-create))
  id method process client state timer created-at)

(defvar remote-gateway--server nil)
(defvar remote-gateway--listener-channel nil)
(defvar remote-gateway--instance-id nil)
(defvar remote-gateway--methods (make-hash-table :test #'equal))
(defvar remote-gateway--clients (make-hash-table :test #'equal))
(defvar remote-gateway--process-clients (make-hash-table :test #'eq))
(defvar remote-gateway--bindings (make-hash-table :test #'equal))
(defvar remote-gateway--binding-keys (make-hash-table :test #'equal))
(defvar remote-gateway--pending (make-hash-table :test #'equal))
(defvar remote-gateway--inbound-pending (make-hash-table :test #'equal))
(defvar remote-gateway--forwards (make-hash-table :test #'equal))
(defvar remote-gateway--request-sequence 0)
(defvar remote-gateway--current-process nil
  "WebSocket process currently dispatching an inbound message.")
(defvar remote-gateway--current-request-id nil
  "JSON-RPC ID currently dispatched from a WebSocket peer.")
(defvar remote-gateway--current-request-method nil
  "JSON-RPC method currently dispatched from a WebSocket peer.")

(define-error 'remote-gateway-rpc-error "Gateway JSON-RPC error")

(defun remote-gateway-live-p ()
  "Return non-nil when the gateway listener is live."
  (and remote-gateway--server
       (process-live-p (ws-process remote-gateway--server))))

(defun remote-gateway--local-context ()
  "Return the fixed client-local context that owns the gateway listener."
  (remote-context-create
   :target-id "local"
   :localname (expand-file-name user-emacs-directory)
   :workspace-root (expand-file-name user-emacs-directory)
   :source 'emacs-gateway))

(defun remote-gateway--random-id (&optional prefix)
  "Return a process-local opaque identifier with PREFIX."
  (format "%s%s"
          (or prefix "")
          (substring
           (secure-hash
            'sha256
            (format "%s:%s:%s:%s"
                    (emacs-pid) (float-time) (random) (garbage-collect)))
           0 24)))

(defun remote-gateway--json-value (value &optional depth)
  "Convert arbitrary Lisp VALUE to a JSON-serializable value."
  (let ((depth (or depth 0)))
    (cond
     ((> depth 20) (format "%S" value))
     ((or (null value) (eq value t) (eq value :json-false)
          (stringp value) (numberp value))
      value)
     ((symbolp value) (symbol-name value))
     ((hash-table-p value)
      ;; `json-serialize' accepts string keys in hash tables, but not in
      ;; alists.  Keep this branch as a hash table instead of accidentally
      ;; turning decoded JSON such as {"tags": ...} into a string-keyed alist
      ;; that later fails with `wrong-type-argument symbolp'.
      (let ((object (make-hash-table :test #'equal)))
        (maphash
         (lambda (key item)
           (puthash (format "%s" key)
                    (remote-gateway--json-value item (1+ depth))
                    object))
         value)
        object))
     ((vectorp value)
      (vconcat
       (mapcar
        (lambda (item)
          (remote-gateway--json-value item (1+ depth)))
        value)))
     ((and (proper-list-p value)
           value
           (zerop (% (length value) 2))
           (cl-loop
            for (key _item) on value by #'cddr
            always (keywordp key)))
      (cl-loop
       for (key item) on value by #'cddr
       collect
       (cons
        (intern (substring (symbol-name key) 1))
        (remote-gateway--json-value item (1+ depth)))))
     ((and (proper-list-p value)
           value
           (cl-every
            (lambda (item)
              (and (consp item)
                   (or (stringp (car item))
                       (symbolp (car item)))))
            value))
      (mapcar
       (lambda (item)
         (cons
          (intern (format "%s" (car item)))
          (remote-gateway--json-value (cdr item) (1+ depth))))
       value))
     ((proper-list-p value)
      (vconcat
       (mapcar
        (lambda (item)
          (remote-gateway--json-value item (1+ depth)))
        value)))
     (t (format "%S" value)))))

(defun remote-gateway--encode (value)
  "Encode VALUE as compact UTF-8 JSON."
  (json-serialize
   (remote-gateway--json-value value)
   :null-object nil :false-object :json-false))

(defun remote-gateway--decode (string)
  "Decode JSON STRING into string-keyed alists."
  (json-parse-string
   string :object-type 'alist :array-type 'list
   :null-object nil :false-object :json-false))

(defun remote-gateway--get (key alist)
  "Return string KEY from decoded ALIST."
  (alist-get key alist nil nil #'string=))

(defun remote-gateway--rpc-error (code message &optional data)
  "Signal JSON-RPC CODE and MESSAGE with optional DATA."
  (signal 'remote-gateway-rpc-error (list code message data)))

(defun remote-gateway-register-method (method function)
  "Register JSON-RPC METHOD to call FUNCTION.
FUNCTION receives PARAMS and the connected client, which is nil for HTTP."
  (unless (and (stringp method) (functionp function))
    (error "Invalid gateway method registration: %S %S" method function))
  (puthash method function remote-gateway--methods)
  method)

(defun remote-gateway-unregister-method (method)
  "Remove JSON-RPC METHOD."
  (remhash method remote-gateway--methods))

(defun remote-gateway--success (id result)
  "Build a JSON-RPC success response for ID and RESULT."
  `(("jsonrpc" . "2.0") ("id" . ,id) ("result" . ,result)))

(defun remote-gateway--failure (id code message &optional data)
  "Build a JSON-RPC failure response."
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("error" .
     (("code" . ,code) ("message" . ,message)
      ,@(when data `(("data" . ,data)))))))

(defun remote-gateway--deferred-key (deferred)
  "Return the registry key for DEFERRED."
  (cons
   (remote-gateway-deferred-process deferred)
   (remote-gateway-deferred-id deferred)))

(defun remote-gateway--finish-deferred (deferred response &optional send)
  "Finish DEFERRED exactly once with RESPONSE.
When SEND is non-nil, send RESPONSE to the originating WebSocket."
  (when (and (remote-gateway-deferred-p deferred)
             (eq (remote-gateway-deferred-state deferred) 'pending))
    (let ((timer (remote-gateway-deferred-timer deferred))
          (process (remote-gateway-deferred-process deferred)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (remote-gateway-deferred-timer deferred) nil
            (remote-gateway-deferred-state deferred) 'done)
      (remhash (remote-gateway--deferred-key deferred)
               remote-gateway--inbound-pending)
      (when (and send (process-live-p process))
        (remote-gateway--send-websocket process response))
      t)))

(defun remote-gateway-defer (&optional timeout)
  "Defer the current inbound WebSocket request.
Return a descriptor for `remote-gateway-resolve' or
`remote-gateway-reject'.  Deferred HTTP requests and JSON-RPC notifications
are rejected explicitly.  TIMEOUT defaults to
`remote-gateway-request-timeout'."
  (unless (and remote-gateway--current-process
               remote-gateway--current-request-id)
    (remote-gateway--rpc-error
     -32001 "Deferred responses require an identified WebSocket request"))
  (let* ((deferred
          (remote-gateway-deferred-create
           :id remote-gateway--current-request-id
           :method remote-gateway--current-request-method
           :process remote-gateway--current-process
           :client
           (gethash remote-gateway--current-process
                    remote-gateway--process-clients)
           :state 'pending :created-at (current-time)))
         (key (remote-gateway--deferred-key deferred)))
    (when (gethash key remote-gateway--inbound-pending)
      (remote-gateway--rpc-error
       -32600 "Duplicate deferred request identifier"))
    (puthash key deferred remote-gateway--inbound-pending)
    (setf
     (remote-gateway-deferred-timer deferred)
     (run-at-time
      (or timeout remote-gateway-request-timeout) nil
      (lambda ()
        (remote-gateway--finish-deferred
         deferred
         (remote-gateway--failure
          (remote-gateway-deferred-id deferred)
          -32000
          (format "Gateway inbound request timed out: %s"
                  (remote-gateway-deferred-method deferred)))
         t))))
    deferred))

(defun remote-gateway-resolve (deferred result)
  "Resolve DEFERRED with RESULT.
Return non-nil only for the first settlement."
  (remote-gateway--finish-deferred
   deferred
   (remote-gateway--success
    (remote-gateway-deferred-id deferred) result)
   t))

(defun remote-gateway-reject (deferred code message &optional data)
  "Reject DEFERRED with JSON-RPC CODE, MESSAGE and optional DATA.
Return non-nil only for the first settlement."
  (remote-gateway--finish-deferred
   deferred
   (remote-gateway--failure
    (remote-gateway-deferred-id deferred) code message data)
   t))

(defun remote-gateway--fail-deferred-for-process (process message)
  "Cancel inbound deferred requests owned by PROCESS with MESSAGE."
  (let (pending)
    (maphash
     (lambda (_key deferred)
       (when (eq process (remote-gateway-deferred-process deferred))
         (push deferred pending)))
     remote-gateway--inbound-pending)
    (dolist (deferred pending)
      ;; The peer has gone away, so only settle and release local resources.
      (remote-gateway--finish-deferred
       deferred
       (remote-gateway--failure
        (remote-gateway-deferred-id deferred) -32000 message)
       nil))))

(defun remote-gateway--dispatch-request (message client)
  "Dispatch decoded JSON-RPC request MESSAGE from CLIENT."
  (let* ((version (remote-gateway--get "jsonrpc" message))
         (method (remote-gateway--get "method" message))
         (params (remote-gateway--get "params" message))
         (id-present (assoc-string "id" message))
         (id (remote-gateway--get "id" message)))
    (cond
     ((not (equal version "2.0"))
      (and id-present
           (remote-gateway--failure id -32600 "Invalid Request")))
     ((not (stringp method))
      (and id-present
           (remote-gateway--failure id -32600 "Invalid Request")))
     ((not (gethash method remote-gateway--methods))
      (and id-present
           (remote-gateway--failure id -32601 "Method not found" method)))
     (t
     (condition-case error
          (let* ((remote-gateway--current-request-id
                  (and id-present id))
                 (remote-gateway--current-request-method method)
                 (result
                  (funcall
                   (gethash method remote-gateway--methods)
                   params client)))
            (cond
             ((remote-gateway-deferred-p result) nil)
             (id-present (remote-gateway--success id result))))
        (remote-gateway-rpc-error
         (and id-present
              (remote-gateway--failure
               id (nth 0 (cdr error)) (nth 1 (cdr error))
               (nth 2 (cdr error)))))
        (error
         (and id-present
              (remote-gateway--failure
               id -32603 (error-message-string error)))))))))

(defun remote-gateway--invoke-callback (callback response)
  "Invoke CALLBACK asynchronously with decoded RESPONSE."
  (when callback
    (let ((result (remote-gateway--get "result" response))
          (error-object (remote-gateway--get "error" response)))
      (run-at-time
       0 nil
       (lambda ()
         (condition-case error
             (funcall callback result error-object)
           (error
            (message "Gateway callback failed: %s"
                     (error-message-string error)))))))))

(defun remote-gateway--finish-pending (pending response)
  "Finish PENDING with JSON-RPC RESPONSE and release its resources."
  (when (remote-gateway-pending-p pending)
    (let ((id (remote-gateway-pending-id pending))
          (timer (remote-gateway-pending-timer pending))
          (state (remote-gateway-pending-state pending)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (remote-gateway-pending-timer pending) nil)
      (remhash id remote-gateway--pending)
      (setcar state 'done)
      (setcdr state response)
      (remote-gateway--invoke-callback
       (remote-gateway-pending-callback pending) response)
      t)))

(defun remote-gateway--fail-pending-for-client (client message)
  "Fail all pending requests owned by CLIENT with MESSAGE."
  (let (pending)
    (maphash
     (lambda (_id request)
       (when (eq client (remote-gateway-pending-client request))
         (push request pending)))
     remote-gateway--pending)
    (dolist (request pending)
      (remote-gateway--finish-pending
       request
       (remote-gateway--failure
        (remote-gateway-pending-id request)
        -32000 message)))))

(defun remote-gateway--settle-response (message)
  "Settle a pending request from decoded response MESSAGE."
  (when-let* ((id (remote-gateway--get "id" message))
              (pending (gethash id remote-gateway--pending)))
    (remote-gateway--finish-pending pending message)))

(defun remote-gateway--handle-message (message client)
  "Handle decoded JSON-RPC MESSAGE associated with CLIENT."
  (if (remote-gateway--get "method" message)
      (remote-gateway--dispatch-request message client)
    (remote-gateway--settle-response message)
    nil))

(defun remote-gateway--send-websocket (process value)
  "Send VALUE as a WebSocket JSON message to PROCESS."
  (process-send-string
   process
   (ws-web-socket-frame (encode-coding-string
                         (remote-gateway--encode value) 'utf-8))))

(defun remote-gateway--websocket-message (process string)
  "Handle WebSocket STRING from PROCESS."
  (condition-case error
      (let* ((remote-gateway--current-process process)
             (message
              (remote-gateway--decode
               (decode-coding-string string 'utf-8)))
             (client (gethash process remote-gateway--process-clients))
             (response (remote-gateway--handle-message message client)))
        (when client
          (setf (remote-gateway-client-last-seen client) (current-time)))
        (when response
          (remote-gateway--send-websocket process response)))
    (error
     (remote-gateway--send-websocket
      process
      (remote-gateway--failure
     nil -32700 "Parse error" (error-message-string error))))))

(defun remote-gateway--websocket-control-frame (opcode payload)
  "Return an unmasked control frame with OPCODE and PAYLOAD."
  (let ((payload
         (encode-coding-string (or payload "") 'binary)))
    (concat
     (unibyte-string
      (logior #x80 opcode)
      (string-bytes payload))
     payload)))

(defun remote-gateway--websocket-filter (process chunk)
  "Incrementally parse masked WebSocket frames from PROCESS and CHUNK."
  (let ((buffer
         (concat
          (or (process-get process 'remote-gateway-ws-buffer) "")
          (encode-coding-string chunk 'binary)))
        (continue t))
    (while (and continue (>= (length buffer) 2))
      (let* ((frame buffer)
             (first (aref frame 0))
             (second (aref frame 1))
             (fin (not (zerop (logand first #x80))))
             (opcode (logand first #x0f))
             (masked (not (zerop (logand second #x80))))
             (short-length (logand second #x7f))
             (length-bytes
              (cond ((= short-length 126) 2)
                    ((= short-length 127) 8)
                    (t 0)))
             (base-header (+ 2 length-bytes))
             (header-length (+ base-header (if masked 4 0))))
        (if (< (length frame) header-length)
            (setq continue nil)
          (let ((payload-length
                 (if (zerop length-bytes)
                     short-length
                   (let ((value 0)
                         (offset 2)
                         (limit (+ 2 length-bytes)))
                     (while (< offset limit)
                       (setq value
                             (+ (ash value 8)
                                (aref frame offset))
                             offset (1+ offset)))
                     value)))
                payload)
            (if (< (length frame) (+ header-length payload-length))
                (setq continue nil)
              (setq payload
                    (substring
                     frame header-length
                     (+ header-length payload-length))
                    buffer
                    (substring frame
                               (+ header-length payload-length)))
              (when masked
                (let ((mask
                       (substring frame base-header header-length)))
                  (dotimes (index payload-length)
                    (aset payload index
                          (logxor
                           (aref payload index)
                           (aref mask (% index 4)))))))
              (pcase opcode
                (8
                 (process-send-string
                  process
                  (remote-gateway--websocket-control-frame 8 ""))
                 (delete-process process)
                 (setq continue nil buffer ""))
                (9
                 (process-send-string
                  process
                  (remote-gateway--websocket-control-frame 10 payload)))
                (10 nil)
                ((or 0 1 2)
                 (if (not masked)
                     (progn
                       (delete-process process)
                       (setq continue nil buffer ""))
                   (let* ((fragment
                           (concat
                            (or (process-get process
                                             'remote-gateway-ws-fragment)
                                "")
                            payload)))
                     (if fin
                         (progn
                           (process-put process
                                        'remote-gateway-ws-fragment nil)
                           (remote-gateway--websocket-message
                            process fragment))
                       (process-put process
                                    'remote-gateway-ws-fragment
                                    fragment)))))))))))
    (process-put process 'remote-gateway-ws-buffer buffer)))

(defun remote-gateway--client-disconnected (process _event)
  "Forget the gateway client owned by PROCESS."
  (remote-gateway--fail-deferred-for-process
   process "Gateway client disconnected")
  (when-let* ((client (gethash process remote-gateway--process-clients)))
    (remhash process remote-gateway--process-clients)
    (remote-gateway--fail-pending-for-client
     client "Gateway client disconnected")
    (when (eq client (gethash
                      (remote-gateway-client-key client)
                      remote-gateway--clients))
      (remhash (remote-gateway-client-key client)
               remote-gateway--clients))))

(defun remote-gateway--websocket-handler (request)
  "Upgrade WebSocket REQUEST and adopt its process."
  (let ((process
         (ws-web-socket-connect
          request #'remote-gateway--websocket-message)))
    (if (not process)
        (remote-gateway--http-json
         (ws-process request) 400
         (remote-gateway--failure nil -32600 "WebSocket upgrade required"))
      (set-process-filter process #'remote-gateway--websocket-filter)
      (process-put process 'remote-gateway-ws-buffer "")
      (remote-channel-adopt
       process
       :kind 'stream :context (remote-gateway--local-context)
       :metadata '(:application "emacs-gateway" :role "peer"))
      (let ((sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (finished event)
           (remote-gateway--client-disconnected finished event)
           (when sentinel
             (funcall sentinel finished event)))))
      (throw 'close-connection :keep-alive))))

(defun remote-gateway--http-json (process status value)
  "Send JSON VALUE with HTTP STATUS to PROCESS."
  (let ((body (remote-gateway--encode value)))
    (ws-response-header
     process status
     '("Content-Type" . "application/json; charset=utf-8")
     (cons "Content-Length"
           (number-to-string (string-bytes body)))
     '("Cache-Control" . "no-store"))
    (process-send-string process body)))

(defun remote-gateway--rpc-handler (request)
  "Handle one HTTP JSON-RPC REQUEST."
  (condition-case error
      (let* ((message
              (remote-gateway--decode
               (decode-coding-string (or (ws-body request) "") 'utf-8)))
             (response (remote-gateway--handle-message message nil)))
        (remote-gateway--http-json
         (ws-process request) 200
         (or response
             '(("jsonrpc" . "2.0") ("result" . nil)))))
    (error
     (remote-gateway--http-json
      (ws-process request) 400
      (remote-gateway--failure
       nil -32700 "Parse error" (error-message-string error))))))

(defun remote-gateway--health-handler (request)
  "Return gateway health for HTTP REQUEST."
  (remote-gateway--http-json
   (ws-process request) 200
   `(("status" . "ok")
     ("instanceId" . ,remote-gateway--instance-id)
     ("clients" . ,(hash-table-count remote-gateway--clients)))))

(defun remote-gateway--discovery-file ()
  "Return this process's discovery record path."
  (expand-file-name
   (format "%s.json" (emacs-pid))
   remote-gateway-discovery-directory))

(defun remote-gateway--write-discovery ()
  "Write the live gateway discovery record."
  (make-directory remote-gateway-discovery-directory t)
  (dolist (file
           (directory-files
            remote-gateway-discovery-directory t "\\`[0-9]+\\.json\\'"))
    (when (string-match "\\([0-9]+\\)\\.json\\'" file)
      (let ((pid (string-to-number (match-string 1 file))))
        (unless (process-attributes pid)
          (ignore-errors (delete-file file))))))
  (let ((info (remote-gateway-connection-info)))
    (with-temp-file (remote-gateway--discovery-file)
      (insert
       (remote-gateway--encode
        `(("pid" . ,(emacs-pid))
          ("instanceId" . ,remote-gateway--instance-id)
          ("httpUrl" . ,(plist-get info :http-url))
          ("websocketUrl" . ,(plist-get info :websocket-url))))))))

(defun remote-gateway--delete-discovery ()
  "Delete this process's discovery record."
  (ignore-errors (delete-file (remote-gateway--discovery-file))))

(defun remote-gateway-start ()
  "Start and return the unified loopback gateway."
  (interactive)
  (unless (remote-gateway-live-p)
    (setq remote-gateway--instance-id
          (remote-gateway--random-id "emacs-"))
    (setq remote-gateway--server
          (ws-start
           `(((:POST . "\\`/rpc\\'") .
              ,#'remote-gateway--rpc-handler)
             ((:GET . "\\`/ws\\'") .
              ,#'remote-gateway--websocket-handler)
             ((:GET . "\\`/health\\'") .
              ,#'remote-gateway--health-handler))
           0 nil :host remote-gateway-host))
    (let ((listener (ws-process remote-gateway--server)))
      (process-put
       listener 'remote-listen-endpoint
       (list :host remote-gateway-host
             :port (process-contact listener :service)))
      (setq remote-gateway--listener-channel
            (remote-channel-adopt
             listener
             :kind 'listener :context (remote-gateway--local-context)
             :metadata
             '(:application "emacs-gateway" :role "listener"))))
    (remote-gateway--write-discovery))
  remote-gateway--server)

(defun remote-gateway-stop ()
  "Stop the gateway, its peer channels and reverse forwards."
  (interactive)
  (let (pending)
    (maphash
     (lambda (_id request) (push request pending))
     remote-gateway--pending)
    (dolist (request pending)
      (remote-gateway--finish-pending
       request
       (remote-gateway--failure
        (remote-gateway-pending-id request)
        -32000 "Emacs gateway stopped"))))
  (let (inbound)
    (maphash
     (lambda (_key deferred) (push deferred inbound))
     remote-gateway--inbound-pending)
    (dolist (deferred inbound)
      (remote-gateway--finish-deferred
       deferred
       (remote-gateway--failure
        (remote-gateway-deferred-id deferred)
        -32000 "Emacs gateway stopped")
       t)))
  (when remote-gateway--server
    (ws-stop remote-gateway--server))
  (maphash
   (lambda (_key forward)
     (when (remote-channel-live-p forward)
       (remote-close-channel forward)))
   remote-gateway--forwards)
  (remote-gateway--delete-discovery)
  (setq remote-gateway--server nil
        remote-gateway--listener-channel nil
        remote-gateway--instance-id nil)
  (clrhash remote-gateway--clients)
  (clrhash remote-gateway--process-clients)
  (clrhash remote-gateway--bindings)
  (clrhash remote-gateway--binding-keys)
  (clrhash remote-gateway--pending)
  (clrhash remote-gateway--inbound-pending)
  (clrhash remote-gateway--forwards))

(defun remote-gateway--listener-endpoint ()
  "Return the local gateway listener endpoint."
  (remote-gateway-start)
  (list :host remote-gateway-host
        :port (process-contact
               (ws-process remote-gateway--server) :service)))

(cl-defun remote-gateway-connection-info
    (&optional context &key (placement 'client))
  "Return gateway connection information for CONTEXT and PLACEMENT.
`client' placement connects directly to Emacs loopback.  `target' placement
uses a stable, workspace-owned reverse forward."
  (remote-gateway-start)
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((remote-workspace-p context)
            (remote-workspace-context context))
           (t (remote-context context))))
         (target-id (remote-context-target-id context))
         (local-endpoint (remote-gateway--listener-endpoint))
         (endpoint
          (if (or (eq placement 'client)
                  (equal target-id "local"))
              local-endpoint
            (let* ((workspace
                    (remote-workspace-open context :connect nil))
                   (key (remote-workspace-id workspace))
                   (known (gethash key remote-gateway--forwards))
                   (forward
                    (if (remote-channel-live-p known)
                        known
                      (remote-reverse-port-forward
                       local-endpoint
                       :context context :workspace workspace
                       :stable-endpoint t
                       :metadata
                       (list :application "emacs-gateway"
                             :workspace key)))))
              (puthash key forward remote-gateway--forwards)
              (remote-channel-endpoint forward 'remote)))))
    (let ((host (plist-get endpoint :host))
          (port (plist-get endpoint :port)))
      (list
       :instance-id remote-gateway--instance-id
       :host host :port port
       :http-url (format "http://%s:%s/rpc" host port)
       :websocket-url (format "ws://%s:%s/ws" host port)
       :health-url (format "http://%s:%s/health" host port)
       :target-id target-id
       :workspace-id (remote-context-workspace-id context)
       :placement placement))))

(defun remote-gateway--binding-key (client-id target-id workspace-id)
  "Return the logical registry key for one client binding."
  (list client-id target-id workspace-id))

(defun remote-gateway--binding-plist (binding info)
  "Return public connection INFO for BINDING."
  (append
   info
   (list :binding-id (remote-gateway-binding-id binding)
         :client-id (remote-gateway-binding-client-id binding)
         :endpoint (remote-gateway-binding-endpoint binding)
         :provides (remote-gateway-binding-provides binding))))

(defun remote-gateway-release-binding (binding &optional disconnect)
  "Release BINDING and optionally DISCONNECT its registered peer.
BINDING may be a binding object, binding ID, or connection plist returned by
`remote-gateway-prepare-client'."
  (let* ((id
          (cond
           ((remote-gateway-binding-p binding)
            (remote-gateway-binding-id binding))
           ((stringp binding) binding)
           ((listp binding) (plist-get binding :binding-id))))
         (binding
          (if (remote-gateway-binding-p binding)
              binding
            (and id (gethash id remote-gateway--bindings)))))
    (when binding
      (let* ((key
              (remote-gateway--binding-key
               (remote-gateway-binding-client-id binding)
               (remote-gateway-binding-target-id binding)
               (remote-gateway-binding-workspace-id binding)))
             (client (gethash key remote-gateway--clients)))
        (remhash (remote-gateway-binding-id binding)
                 remote-gateway--bindings)
        (when (equal (gethash key remote-gateway--binding-keys)
                     (remote-gateway-binding-id binding))
          (remhash key remote-gateway--binding-keys))
        (when (and disconnect client)
          (let ((process (remote-gateway-client-process client)))
            (if (process-live-p process)
                (delete-process process)
              (remote-gateway--client-disconnected process "released"))))
        t))))

(cl-defun remote-gateway-prepare-client
    (client-id &optional context &key (placement 'client) endpoint provides)
  "Create bounded registration state and connection data for CLIENT-ID.
An already connected logical client reuses its binding.  A disconnected
client's previous binding is revoked before its replacement is installed."
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((remote-workspace-p context)
            (remote-workspace-context context))
           (t (remote-context context))))
         (info (remote-gateway-connection-info
                context :placement placement))
         (target-id (remote-context-target-id context))
         (workspace-id
          (or (remote-context-workspace-id context)
              (remote-workspace-context-id context)))
         (key
          (remote-gateway--binding-key
           client-id target-id workspace-id))
         (old-id (gethash key remote-gateway--binding-keys))
         (old-binding
          (and old-id (gethash old-id remote-gateway--bindings)))
         (client (gethash key remote-gateway--clients)))
    (if (and old-binding client
             (process-live-p
              (remote-gateway-client-process client)))
        (progn
          (setf (remote-gateway-binding-context old-binding) context
                (remote-gateway-binding-endpoint old-binding) endpoint
                (remote-gateway-binding-provides old-binding) provides)
          (remote-gateway--binding-plist old-binding info))
      (when old-binding
        (remote-gateway-release-binding old-binding))
      (let* ((binding-id (remote-gateway--random-id "binding-"))
             (binding
              (remote-gateway-binding-create
               :id binding-id :client-id client-id
               :target-id target-id :workspace-id workspace-id
               :context context :endpoint endpoint :provides provides
               :created-at (current-time))))
        (puthash binding-id binding remote-gateway--bindings)
        (puthash key binding-id remote-gateway--binding-keys)
        (remote-gateway--binding-plist binding info)))))

(defun remote-gateway--register-client (params _client)
  "Register a WebSocket peer using PARAMS."
  (let* ((binding-id (remote-gateway--get "bindingId" params))
         (binding (and binding-id
                       (gethash binding-id remote-gateway--bindings)))
         (process
          (or remote-gateway--current-process
              (remote-gateway--rpc-error
               -32602 "gateway.register requires WebSocket"))))
    (unless binding
      (remote-gateway--rpc-error -32602 "Unknown gateway binding"))
    (let* ((client-id (remote-gateway-binding-client-id binding))
           (instance-id
            (or (remote-gateway--get "instanceId" params)
                (remote-gateway--random-id "peer-")))
           (key
            (list client-id
                  (remote-gateway-binding-target-id binding)
                  (remote-gateway-binding-workspace-id binding)))
           (old (gethash key remote-gateway--clients))
           (channel (remote-channel-of process))
           (client
            (remote-gateway-client-create
             :key key :client-id client-id :instance-id instance-id
             :target-id (remote-gateway-binding-target-id binding)
             :workspace-id (remote-gateway-binding-workspace-id binding)
             :process process :channel channel
             :endpoint
             (or (remote-gateway--get "endpoint" params)
                 (remote-gateway-binding-endpoint binding))
             :provides
             (or (remote-gateway--get "provides" params)
                 (remote-gateway-binding-provides binding))
             :connected-at (current-time) :last-seen (current-time)
             :metadata params)))
      (when (and old
                 (not (eq process
                          (remote-gateway-client-process old)))
                 (process-live-p (remote-gateway-client-process old)))
        (delete-process (remote-gateway-client-process old)))
      (puthash key client remote-gateway--clients)
      (puthash process client remote-gateway--process-clients)
      `(("clientId" . ,client-id)
        ("instanceId" . ,instance-id)
        ("targetId" . ,(remote-gateway-client-target-id client))
        ("workspaceId" . ,(remote-gateway-client-workspace-id client))))))

(defun remote-gateway--eval (params _client)
  "Evaluate Emacs Lisp supplied in PARAMS."
  (let ((source (or (remote-gateway--get "source" params)
                    (remote-gateway--get "script" params))))
    (unless (stringp source)
      (remote-gateway--rpc-error
       -32602 "emacs.eval requires string params.source"))
    (let ((position 0)
          value form)
      (condition-case error
          (while (< position (length source))
            (let ((read-result (read-from-string source position)))
              (setq form (car read-result)
                    position (cdr read-result)
                    value (eval form t))))
        (end-of-file nil)
        (error (signal (car error) (cdr error))))
      `(("value" . ,(remote-gateway--json-value value))
        ("printed" . ,(prin1-to-string value))))))

(defun remote-gateway--status (_params _client)
  "Return a JSON-safe gateway status."
  `(("instanceId" . ,remote-gateway--instance-id)
    ("clients" . ,(remote-gateway-client-list))
    ("pending" . ,(hash-table-count remote-gateway--pending))))

(defun remote-gateway-client-list ()
  "Return JSON-safe summaries of connected gateway peers."
  (let (result)
    (maphash
     (lambda (_key client)
       (push
        `(("clientId" . ,(remote-gateway-client-client-id client))
          ("instanceId" . ,(remote-gateway-client-instance-id client))
          ("targetId" . ,(remote-gateway-client-target-id client))
          ("workspaceId" . ,(remote-gateway-client-workspace-id client))
          ("endpoint" . ,(remote-gateway-client-endpoint client))
          ("provides" . ,(remote-gateway-client-provides client))
          ("connectedAt" .
           ,(float-time (remote-gateway-client-connected-at client))))
        result))
     remote-gateway--clients)
    result))

(defun remote-gateway-find-client (client-id &optional context)
  "Return connected CLIENT-ID, optionally scoped to CONTEXT."
  (let ((target-id
         (and context
              (remote-context-target-id
               (if (remote-context-p context)
                   context
                 (remote-context context)))))
        found)
    (maphash
     (lambda (_key client)
       (when (and (not found)
                  (equal client-id
                         (remote-gateway-client-client-id client))
                  (or (not target-id)
                      (equal target-id
                             (remote-gateway-client-target-id client))))
         (setq found client)))
     remote-gateway--clients)
    found))

(defun remote-gateway--resolve-client (client)
  "Resolve CLIENT object or logical client ID."
  (if (remote-gateway-client-p client)
      client
    (or (remote-gateway-find-client client)
        (error "Gateway client is not connected: %s" client))))

(defun remote-gateway--begin-request
    (client method params callback timeout)
  "Begin a request and return its managed pending descriptor."
  (let* ((client (remote-gateway--resolve-client client))
         (process (remote-gateway-client-process client))
         (id (format "emacs-%d" (cl-incf remote-gateway--request-sequence)))
         (state (cons 'pending nil))
         (pending
          (remote-gateway-pending-create
           :id id :client client :state state :callback callback)))
    (unless (process-live-p process)
      (error "Gateway client process is not live: %s"
             (remote-gateway-client-client-id client)))
    (puthash id pending remote-gateway--pending)
    (setf
     (remote-gateway-pending-timer pending)
     (run-at-time
      (or timeout remote-gateway-request-timeout) nil
      (lambda ()
        (when (eq pending (gethash id remote-gateway--pending))
          (remote-gateway--finish-pending
           pending
           (remote-gateway--failure
            id -32000 (format "Gateway request timed out: %s" method)))))))
    (condition-case error
        (remote-gateway--send-websocket
         process
         `(("jsonrpc" . "2.0") ("id" . ,id)
           ("method" . ,method) ("params" . ,params)))
      (error
       (remote-gateway--finish-pending
        pending
        (remote-gateway--failure
         id -32000 (error-message-string error)))
       (signal (car error) (cdr error))))
    pending))

(defun remote-gateway-request (client method &optional params)
  "Send METHOD request with PARAMS to CLIENT and return request state.
Unconsumed requests are automatically timed out and removed."
  (remote-gateway-pending-state
   (remote-gateway--begin-request
    client method params nil remote-gateway-request-timeout)))

(defun remote-gateway-request-async
    (client method params callback &optional timeout)
  "Request METHOD from CLIENT without blocking Emacs.
CALLBACK is invoked as (CALLBACK RESULT ERROR), where ERROR is a decoded
JSON-RPC error object or nil.  Return a state cons for optional inspection."
  (unless (functionp callback)
    (error "Gateway async callback is not callable: %S" callback))
  (remote-gateway-pending-state
   (remote-gateway--begin-request
    client method params callback
    (or timeout remote-gateway-request-timeout))))

(defun remote-gateway-request-sync
    (client method &optional params timeout)
  "Synchronously request METHOD from CLIENT and return its result."
  (let* ((timeout (or timeout remote-gateway-request-timeout))
         (pending
          (remote-gateway--begin-request
           client method params nil timeout))
         (state (remote-gateway-pending-state pending))
         (deadline (+ (float-time)
                      timeout)))
    (while (and (eq (car state) 'pending)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (when (eq (car state) 'pending)
      (remote-gateway--finish-pending
       pending
       (remote-gateway--failure
        (remote-gateway-pending-id pending)
        -32000 (format "Gateway request timed out: %s" method))))
    (let* ((response (cdr state))
           (error-object (remote-gateway--get "error" response)))
      (when error-object
        (error "Gateway peer error %s: %s"
               (remote-gateway--get "code" error-object)
               (remote-gateway--get "message" error-object)))
      (remote-gateway--get "result" response))))

(defun remote-gateway-notify (client method &optional params)
  "Send METHOD notification with PARAMS to CLIENT."
  (let ((client (remote-gateway--resolve-client client)))
    (remote-gateway--send-websocket
     (remote-gateway-client-process client)
     `(("jsonrpc" . "2.0") ("method" . ,method)
       ("params" . ,params)))))

(remote-gateway-register-method
 "gateway.register" #'remote-gateway--register-client)
(remote-gateway-register-method
 "gateway.ping" (lambda (_params _client) "pong"))
(remote-gateway-register-method
 "gateway.status" #'remote-gateway--status)
(remote-gateway-register-method
 "emacs.eval" #'remote-gateway--eval)

(add-hook 'kill-emacs-hook #'remote-gateway-stop)

(provide 'remote-gateway)
;;; remote-gateway.el ends here
