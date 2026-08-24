;;; remote-backend-native.el --- Native target backend -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'network-stream)
(require 'remote-backend-core)
(require 'remote-fs)

(defun remote-backend-native-available-p (link context)
  "Return non-nil when LINK and CONTEXT describe the client target.
The native backend is a placement implementation, not a fallback transport
for an arbitrary target.  Rejecting an accidental non-local pipeline here
keeps route resolution from selecting a backend which can only fail later
during path projection."
  (equal
   (or
    (and (remote-context-p context)
         (remote-context-target-id context))
    (and (remote-link-p link)
         (remote-link-target-id link)))
   "local"))

(defun remote-backend-native-project (file-name _link _route)
  "Project logical FILE-NAME onto the native file system."
  (let ((target (remote-fs-target-id file-name)))
    (unless (equal target "local")
      (error "Native backend cannot access target %s" target))
    (remote-fs-localname file-name)))

(defun remote-backend-native-client-file-name (file-name route)
  "Return native FILE-NAME as a path accessible to client-side tools."
  (remote-backend-native-project
   file-name (remote-route-link route) route))

(defun remote-backend-native-expand-localname
    (name directory _link _route)
  "Resolve NAME against native DIRECTORY on the local target."
  (let ((inhibit-file-name-handlers
         (cons #'tramp-file-name-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'expand-file-name))
    (expand-file-name name (or directory "/"))))

(defun remote-backend-native-connect (route _context)
  "Return the native root represented by ROUTE."
  (remote-backend-native-project
   (remote-make-file-name (remote-route-target-id route) "/")
   (remote-route-link route) route))

(defun remote-backend-native-live-p
    (_connection _route _context)
  "Return non-nil because the native backend has no remote session."
  t)

(defun remote-backend-native-network (_route _context arguments)
  "Call `make-network-process' with native ARGUMENTS."
  (apply #'make-network-process arguments))

(defun remote-backend-native-stream
    (_route _context name buffer host service parameters)
  "Call `open-network-stream' with native arguments."
  (apply #'open-network-stream
         name buffer host service parameters))

(defun remote-backend-native--valid-environment-name-p (name)
  "Return non-nil when NAME is safe in an `env' assignment."
  (and (stringp name)
       (string-match-p
        "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)))

(defun remote-backend-native-stdio-bridge (execution)
  "Return client argv which executes native target EXECUTION over stdio."
  (let* ((directory
          (remote-backend-execution-physical-directory execution))
         (command (remote-backend-execution-command execution))
         (assignments
          (cl-loop
           for entry in (remote-backend-execution-environment execution)
           for name = (format "%s" (car-safe entry))
           when (and
                 (consp entry)
                 (cdr entry)
                 (remote-backend-native--valid-environment-name-p name))
           collect
           (concat name "=" (format "%s" (cdr entry)))))
         (shell-command
          (concat
           (when directory
             (format "cd -- %s && "
                     (shell-quote-argument directory)))
           "exec "
           (mapconcat
            #'shell-quote-argument
            (append (list "env") assignments command)
            " "))))
    (list "/bin/sh" "-c" shell-command)))

(defun remote-backend-native--endpoint-value (endpoint key default)
  "Return ENDPOINT's KEY value, falling back to DEFAULT."
  (or
   (and (listp endpoint)
        (or
         (plist-get endpoint key)
         (alist-get
          (intern (substring (symbol-name key) 1))
          endpoint)))
   default))

(defun remote-backend-native--proxy-forget-peer (process)
  "Remove PROCESS from its native forward server's peer registry."
  (when-let* ((server (process-get process 'remote-forward-server)))
    (process-put
     server 'remote-forward-peers
     (delq process (process-get server 'remote-forward-peers)))))

(defun remote-backend-native--proxy-close-pair (process event)
  "Close PROCESS and its paired native forwarding connection."
  ;; Network sentinels also receive an `open' notification.  Treating every
  ;; event as teardown closes the outbound half before the first byte can be
  ;; relayed.
  (when (or (memq (process-status process)
                  '(closed exit signal failed))
            ;; Relay code passes the structured error here to abort an
            ;; otherwise still-open pair after a failed write.
            (consp event))
    (let ((peer (process-get process 'remote-forward-peer)))
      (remote-backend-native--proxy-forget-peer process)
      (unless (process-get process 'remote-forward-closing)
        (process-put process 'remote-forward-closing t)
        (when (processp peer)
          (process-put peer 'remote-forward-closing t)
          (remote-backend-native--proxy-forget-peer peer)
          (when (process-live-p peer)
            (delete-process peer)))
        (when (and (consp event) (process-live-p process))
          (delete-process process))))))

(defun remote-backend-native--proxy-filter (process string)
  "Relay STRING from PROCESS to its paired forwarding process."
  (when-let* ((peer (process-get process 'remote-forward-peer))
              ((process-live-p peer)))
    (condition-case error
        (process-send-string peer string)
      (error
       (remote-log
        'native-forward-relay-error
        :process (process-name process)
        :error (error-message-string error))
       (remote-backend-native--proxy-close-pair process error)))))

(defun remote-backend-native--proxy-buffer-filter (process string)
  "Buffer STRING on PROCESS until its forwarding peer is ready."
  (process-put
   process 'remote-forward-pending
   (concat (process-get process 'remote-forward-pending) string)))

(defun remote-backend-native--proxy-pair (client outbound)
  "Pair CLIENT with OUTBOUND and flush independently buffered data."
  (let ((client-pending
         (process-get client 'remote-forward-pending))
        (outbound-pending
         (process-get outbound 'remote-forward-pending)))
    ;; Make both directions live before flushing either side.  Activating one
    ;; side at a time can feed an eager server greeting into the other side's
    ;; pending request buffer and send it back to the server.
    (process-put client 'remote-forward-peer outbound)
    (process-put outbound 'remote-forward-peer client)
    (process-put client 'remote-forward-pending nil)
    (process-put outbound 'remote-forward-pending nil)
    (set-process-filter client #'remote-backend-native--proxy-filter)
    (set-process-filter outbound #'remote-backend-native--proxy-filter)
    (condition-case error
        (progn
          (when client-pending
            (process-send-string outbound client-pending))
          (when outbound-pending
            (process-send-string client outbound-pending)))
      (error
       (remote-log
        'native-forward-relay-error
        :process (process-name client)
        :error (error-message-string error))
       (remote-backend-native--proxy-close-pair client error)))))

(defun remote-backend-native--proxy-accept (server client _message)
  "Connect accepted CLIENT on SERVER to the configured destination."
  ;; Emacs allocates a fresh buffer for every connection accepted by a
  ;; server process, even when the listener itself has no process buffer.
  ;; The proxy relays bytes exclusively through process filters, so retaining
  ;; that empty buffer only leaks one Fundamental-mode buffer per reconnect.
  ;; Install the buffering filter before killing the buffer because buffer
  ;; teardown can run hooks which re-enter the process event loop.
  (set-process-filter
   client #'remote-backend-native--proxy-buffer-filter)
  (when-let* ((buffer (process-buffer client)))
    (set-process-buffer client nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (let* ((destination
          (process-get server 'remote-forward-destination))
         (host (plist-get destination :host))
         (port (plist-get destination :port))
         outbound)
    ;; A client may send its first bytes while the outbound connection is
    ;; still being created.  The buffering filter installed above preserves
    ;; them until both forwarding peers are ready.
    (set-process-query-on-exit-flag client nil)
    (process-put client 'remote-forward-server server)
    (set-process-sentinel
     client #'remote-backend-native--proxy-close-pair)
    (process-put
     server 'remote-forward-peers
     (cons client (process-get server 'remote-forward-peers)))
    (condition-case error
        (progn
          (setq outbound
                (make-network-process
                 :name
                 (generate-new-buffer-name
                  (format "remote-native-forward-%s" port))
                 :host host :service port
                 :coding 'binary :noquery t
                 :filter #'remote-backend-native--proxy-buffer-filter
                 :sentinel #'remote-backend-native--proxy-close-pair))
          (process-put outbound 'remote-forward-server server)
          (process-put
           server 'remote-forward-peers
           (cons outbound (process-get server 'remote-forward-peers)))
          (remote-backend-native--proxy-pair client outbound))
      (error
       (when (process-live-p client)
         (delete-process client))
       (when (and (processp outbound) (process-live-p outbound))
         (delete-process outbound))
       (remote-log
        'native-forward-connect-error
        :host host :port port
        :error (error-message-string error))))))

(defun remote-backend-native--close-forward (forward)
  "Close native proxy FORWARD and all accepted peer processes."
  (when-let* ((server (remote-forward-handle forward)))
    (dolist (peer (copy-sequence
                   (process-get server 'remote-forward-peers)))
      (when (process-live-p peer)
        (process-put peer 'remote-forward-closing t)
        (delete-process peer)))
    (process-put server 'remote-forward-peers nil)
    (when (process-live-p server)
      (delete-process server))))

(defun remote-backend-native-forward
    (route context local-endpoint remote-endpoint metadata)
  "Open a native TCP forward for ROUTE.
With `:direction reverse' in METADATA, listen on REMOTE-ENDPOINT and connect
accepted clients to LOCAL-ENDPOINT.  Otherwise use the ordinary local-forward
direction."
  (let* ((direction (or (plist-get metadata :direction) 'local))
         (listener
          (if (eq direction 'reverse)
              remote-endpoint
            local-endpoint))
         (destination
          (if (eq direction 'reverse)
              local-endpoint
            remote-endpoint))
         (listen-host
          (remote-backend-native--endpoint-value
           listener :host "127.0.0.1"))
         (listen-port
          (remote-backend-native--endpoint-value listener :port 0))
         (destination-host
          (remote-backend-native--endpoint-value
           destination :host "127.0.0.1"))
         (destination-port
          (remote-backend-native--endpoint-value destination :port nil))
         (_
          (unless destination-port
            (error "Forward destination has no port: %S" destination)))
         (server
          (make-network-process
           :name
           (generate-new-buffer-name
            (format "remote-native-forward-%s" destination-port))
           :server t :host listen-host
           :service
           (if (or (eq listen-port t)
                   (not (and (integerp listen-port)
                             (> listen-port 0))))
               0
             listen-port)
           :coding 'binary :noquery t
           :log #'remote-backend-native--proxy-accept))
         (actual-listener
          (list :host listen-host
                :port (process-contact server :service)))
         (local
          (if (eq direction 'reverse)
              (list :host destination-host :port destination-port)
            actual-listener))
         (remote
          (if (eq direction 'reverse)
              actual-listener
            (list :host destination-host :port destination-port)))
         (forward
          (remote-forward-create
           :backend-id (remote-route-link-plugin-id route)
           :route route :context context :handle server
           :close-function #'remote-backend-native--close-forward
           :local-endpoint local :remote-endpoint remote
           :state 'open
           :metadata
           (plist-put (copy-sequence metadata)
                      :direction direction))))
    (process-put
     server 'remote-forward-destination
     (list :host destination-host :port destination-port))
    (process-put server 'remote-forward forward)
    forward))

(defun remote-backend-native-register ()
  "Register the built-in native backend."
  (remote-register-backend
   "native"
   :capabilities remote-native-capabilities
   :available #'remote-backend-native-available-p
   :project #'remote-backend-native-project
   :client-file-name #'remote-backend-native-client-file-name
   :expand-localname #'remote-backend-native-expand-localname
   :connect #'remote-backend-native-connect
   :live #'remote-backend-native-live-p
   :stdio-bridge #'remote-backend-native-stdio-bridge
   :copy-file-to-target
   (lambda (_route local-file target-file overwrite)
     (copy-file local-file target-file overwrite))
   :make-network-process #'remote-backend-native-network
   :open-network-stream #'remote-backend-native-stream
   :port-forward #'remote-backend-native-forward
   :program-form 'search
   :describe
   (lambda ()
     '(:kind native :session-owner emacs))))

(provide 'remote-backend-native)
;;; remote-backend-native.el ends here
