;;; remote-backend-tramp.el --- Standard TRAMP backend -*- lexical-binding: t; -*-

;;; Code:

(require 'tramp)
(require 'network-stream)
(require 'remote-backend-core)
(require 'remote-connection)
(require 'remote-fs)
(require 'remote-transport)

(defvar tramp-connection-properties)
(defvar tramp-methods)
(defvar tramp-rpc-ssh-args)
(defvar tramp-rpc-ssh-options)

(defconst remote-backend-tramp-capabilities
  '(file-read file-write directory metadata
    process-sync process-async pty watch lsp environment
    network-client network-server port-forward reverse-forward)
  "Capabilities implemented through standard TRAMP.")

(defcustom remote-backend-ssh-forward-timeout 5
  "Seconds to wait for an SSH local forward to start listening."
  :type 'number
  :group 'remote)

(defun remote-backend-tramp--ssh-options (link)
  "Return bounded SSH option strings for LINK.
Pipeline config may set `:ssh-options', `:connect-timeout', and
`:connection-attempts'.  The framework connection deadline supplies the
default TCP connect timeout."
  (let* ((config (remote-pipeline-effective-config link))
         (configured (plist-get config :ssh-options))
         (timeout
          (or (plist-get config :connect-timeout)
              (and (boundp 'remote-connection-open-timeout)
                   remote-connection-open-timeout)))
         (attempts (or (plist-get config :connection-attempts) 1)))
    (delete-dups
     (append
      (cond
       ((null configured) nil)
       ((stringp configured) (list configured))
       ((listp configured)
        (mapcar (lambda (option) (format "%s" option)) configured)))
      (when (and (numberp timeout) (> timeout 0))
        (list (format "ConnectTimeout=%d" (ceiling timeout))))
      (when (and (numberp attempts) (> attempts 0))
        (list (format "ConnectionAttempts=%d" (ceiling attempts))))))))

(defun remote-backend-tramp--ssh-raw-args (options)
  "Convert SSH OPTIONS to a flat raw argument list."
  (cl-loop for option in options
           append (list "-o" option)))

(defun remote-backend-tramp--method-login-args (method options)
  "Return METHOD login arguments decorated with SSH OPTIONS."
  (when-let* ((entry (assoc method tramp-methods))
              (base (cadr (assq 'tramp-login-args (cdr entry)))))
    (append
     (mapcar (lambda (option) (list "-o" option)) options)
     (copy-tree base))))

(defun remote-backend-tramp-file-name (localname link method)
  "Return a TRAMP file name for LOCALNAME through LINK using METHOD."
  (let* ((config (remote-pipeline-effective-config link))
         (host (or (plist-get config :host)
                   (error "Pipeline %s has no host"
                          (remote-link-id link))))
         (user (plist-get config :user))
         (port (plist-get config :port))
         (hops (plist-get config :hops)))
    (if hops
        (format
         "/%s:%s"
         (mapconcat
          #'identity
          (cl-loop
           for hop in hops
           for tail on hops
           collect
           (let ((hop-method
                  (if (cdr tail)
                      (or (remote-endpoint-method hop) "ssh")
                    method))
                 (hop-user (remote-endpoint-user hop))
                 (hop-host (remote-endpoint-host hop))
                 (hop-port (remote-endpoint-port hop)))
             (format
              "%s:%s%s%s"
              hop-method
              (if (and hop-user
                       (not (string-empty-p hop-user)))
                  (concat hop-user "@")
                "")
              hop-host
              (if hop-port (format "#%s" hop-port) ""))))
          "|")
         localname)
      (format "/%s:%s%s%s:%s"
              method
              (if (and user (not (string-empty-p user)))
                  (concat user "@")
                "")
              host
              (if port (format "#%s" port) "")
              localname))))

(defun remote-backend-tramp-project (file-name link _route)
  "Project logical FILE-NAME through standard TRAMP LINK."
  (remote-backend-tramp-file-name
   (remote-fs-localname file-name)
   link
   (or (plist-get (remote-pipeline-effective-config link) :method)
       "ssh")))

(defun remote-backend-tramp-expand-localname-with-method
    (name directory link method)
  "Resolve target-native NAME against DIRECTORY through LINK using METHOD."
  (let* ((target-name
          (if (string-prefix-p "~" name)
              name
            (let ((inhibit-file-name-handlers
                   (cons #'tramp-file-name-handler
                         inhibit-file-name-handlers))
                  (inhibit-file-name-operation 'expand-file-name))
              (expand-file-name name (or directory "/")))))
         ;; Prefix the target-relative spelling before asking Emacs to
         ;; expand it.  Passing bare `~/' with a TRAMP default directory
         ;; would be handled as a client-local absolute name.
         (physical
          (remote-backend-tramp-file-name target-name link method))
         ;; This function is commonly entered from the logical `/fs:' file
         ;; handler.  Emacs inhibits TRAMP's handler while that handler is
         ;; active, but PHYSICAL is a new `/ssh:' or `/rpc:' name and must be
         ;; handed back to TRAMP so target `~' expands against the target HOME.
         (expanded
          (let ((inhibit-file-name-handlers
                 (delq #'tramp-file-name-handler
                       (copy-sequence inhibit-file-name-handlers)))
                (inhibit-file-name-operation nil))
            (expand-file-name physical)))
         (vector (tramp-dissect-file-name expanded nil))
         (localname (tramp-file-name-localname vector)))
    (unless (and (stringp localname)
                 (file-name-absolute-p localname)
                 (not (string-prefix-p "~" localname)))
      (error "TRAMP did not resolve target path %S" physical))
    localname))

(defun remote-backend-tramp-expand-localname
    (name directory link _route)
  "Resolve target-native NAME against DIRECTORY through standard TRAMP LINK."
  (remote-backend-tramp-expand-localname-with-method
   name directory link
   (or (plist-get (remote-pipeline-effective-config link) :method)
       "ssh")))

(defun remote-backend-tramp-connect (route _context)
  "Establish ROUTE using its official TRAMP file-name handler."
  (let* ((link (remote-route-pipeline route))
         (config (remote-pipeline-effective-config link))
         (method
          (if (equal (remote-route-link-plugin-id route) "tramp-rpc")
              "rpc"
            (or (plist-get config :method) "ssh")))
         (options (remote-backend-tramp--ssh-options link))
         (raw-options (remote-backend-tramp--ssh-raw-args options))
         (physical
          (remote-backend-project-file-name
           route
           (remote-make-file-name
            (remote-route-target-id route) "/")))
         (prefix (file-remote-p physical))
         (login-args
          (remote-backend-tramp--method-login-args method options))
         ;; These are official, connection-scoped TRAMP overrides.  Dynamic
         ;; binding keeps one pipeline's SSH policy out of every other target.
         (tramp-connection-properties
          (if (and prefix login-args)
              (cons
               (list
                (concat "\\`" (regexp-quote prefix))
                "login-args" login-args)
               tramp-connection-properties)
            tramp-connection-properties))
         ;; tramp-rpc's ControlMaster bootstrap reads raw args, whereas its
         ;; server process reads both raw args and `tramp-rpc-ssh-options'.
         (tramp-rpc-ssh-args
          ;; Do not deduplicate this flat argv: every option has its own `-o'
          ;; token, and deleting repeated `-o' changes argument boundaries.
          (append raw-options
                  (and (boundp 'tramp-rpc-ssh-args)
                       tramp-rpc-ssh-args)))
         (tramp-rpc-ssh-options
          (delete-dups
           (append options
                   (and (boundp 'tramp-rpc-ssh-options)
                        tramp-rpc-ssh-options)))))
    (unless (file-remote-p physical nil 'connected)
      (file-attributes physical))
    physical))

(defun remote-backend-tramp-live-p
    (connection _route _context)
  "Return whether CONNECTION's TRAMP session remains connected."
  (let ((physical (remote-connection-handle connection)))
    (and (stringp physical)
         (file-remote-p physical nil 'connected)
         t)))

(defun remote-backend-tramp-disconnect (connection _route)
  "Close CONNECTION through TRAMP's public cleanup command."
  (let ((physical (remote-connection-handle connection)))
    (when (and (stringp physical)
               (tramp-tramp-file-p physical))
      (require 'tramp-cmds)
      (tramp-cleanup-connection
       (tramp-dissect-file-name physical nil)))))

(defun remote-backend-tramp--channel-endpoint-value
    (endpoint key &optional default)
  "Return KEY from channel ENDPOINT, falling back to DEFAULT."
  (or
   (cond
    ((and (listp endpoint) (keywordp (car endpoint)))
     (or (plist-get endpoint key)
         (and (eq key :port)
              (plist-get endpoint :service))))
    ((listp endpoint)
     (or
      (alist-get (intern (substring (symbol-name key) 1)) endpoint)
      (alist-get key endpoint)))
    ((and (eq key :port) (integerp endpoint)) endpoint))
   default))

(defun remote-backend-tramp--ssh-destination (endpoint)
  "Return an SSH destination string for transport ENDPOINT."
  (let ((host (remote-endpoint-host endpoint))
        (user (remote-endpoint-user endpoint)))
    (unless (and (stringp host) (not (string-empty-p host)))
      (signal 'remote-backend-unsupported
              (list "SSH pipeline has no destination host")))
    (if (and user (not (string-empty-p user)))
        (format "%s@%s" user host)
      host)))

(defun remote-backend-tramp--ssh-forward-host (host)
  "Return HOST escaped for an OpenSSH forwarding specification."
  (let ((host (format "%s" host)))
    (if (and (string-match-p ":" host)
             (not (and (string-prefix-p "[" host)
                       (string-suffix-p "]" host))))
        (format "[%s]" host)
      host)))

(defun remote-backend-tramp--pipeline-ssh-parts (route)
  "Return `(DESTINATION JUMPS CONFIG)' for ROUTE's SSH pipeline."
  (let* ((pipeline (remote-route-pipeline route))
         (config (remote-pipeline-effective-config pipeline))
         (hops
          (or
           (plist-get config :hops)
           (list
            (remote-endpoint-create
             :target-id (remote-route-target-id route)
             :host (plist-get config :host)
             :port (plist-get config :port)
             :user (plist-get config :user)
             :method (or (plist-get config :method) "ssh")))))
         (unsupported
          (seq-find
           (lambda (hop)
             (not
              (member (or (remote-endpoint-method hop) "ssh")
                      '("ssh" "sshx" "scp"))))
           hops)))
    (when unsupported
      (signal
       'remote-backend-unsupported
       (list
        (format "Direct SSH cannot cross %s stage"
                (remote-endpoint-method unsupported)))))
    (list (car (last hops)) (butlast hops) config)))

(defun remote-backend-tramp--ssh-jump-argument (hops)
  "Return the ProxyJump spelling for HOPS."
  (mapconcat
   (lambda (hop)
     (concat
      (remote-backend-tramp--ssh-destination hop)
      (if-let* ((port (remote-endpoint-port hop)))
          (format ":%s" port)
        "")))
   hops ","))

(defun remote-backend-tramp--valid-environment-name-p (name)
  "Return non-nil when NAME is safe as an `env' assignment."
  (and (stringp name)
       (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)))

(defun remote-backend-tramp-direct-async-command
    (route command environment directory)
  "Return a local SSH argv for target COMMAND on ROUTE.
ENVIRONMENT is an alist of target overrides.  DIRECTORY is a target-native
absolute working directory.  The resulting local process exposes SSH's real
stdout and stderr pipes, avoiding TRAMP's remote FIFO implementation."
  (pcase-let* ((`(,destination ,jumps ,_config)
                 (remote-backend-tramp--pipeline-ssh-parts route))
                (ssh
                 (or (executable-find "ssh")
                     (signal
                      'remote-backend-unsupported
                      '("Local ssh executable is unavailable"))))
                (options
                 (remote-backend-tramp--ssh-options
                  (remote-route-pipeline route)))
                (assignments
                 (cl-loop
                  for entry in environment
                  for name = (format "%s" (car-safe entry))
                  when (and (consp entry)
                            (cdr entry)
                            (remote-backend-tramp--valid-environment-name-p
                             name))
                  collect (concat name "=" (format "%s" (cdr entry)))))
                (remote-shell
                 (concat
                  (when (and directory
                             (file-name-absolute-p directory))
                    (format "cd -- %s && "
                            (shell-quote-argument directory)))
                  "exec "
                  (mapconcat
                   #'shell-quote-argument
                   (append (list "env") assignments command)
                   " ")))
                (arguments (list ssh "-T")))
    (dolist (option options)
      (setq arguments (append arguments (list "-o" option))))
    (when jumps
      (setq arguments
            (append arguments
                    (list "-J"
                          (remote-backend-tramp--ssh-jump-argument jumps)))))
    (when-let* ((port (remote-endpoint-port destination)))
      (setq arguments
            (append arguments (list "-p" (format "%s" port)))))
    (append arguments
            (list (remote-backend-tramp--ssh-destination destination)
                  remote-shell))))

(defun remote-backend-tramp-stdio-bridge (execution)
  "Return client SSH argv exposing target EXECUTION over stdio."
  (let* ((context (remote-backend-execution-context execution))
         (logical-directory
          (remote-backend-execution-logical-directory execution))
         (directory
          (if logical-directory
              (remote-file-local-name logical-directory)
            (remote-context-localname context))))
    (remote-backend-tramp-direct-async-command
     (remote-backend-execution-route execution)
     (remote-backend-execution-command execution)
     (remote-backend-execution-environment execution)
     directory)))

(defun remote-backend-tramp-prepare-process
    (execution arguments environment)
  "Return an async process plan for standard TRAMP EXECUTION.
Pipe processes use a client SSH stdio bridge.  PTY and other process forms
remain on Emacs' ordinary file-handler path."
  (if (eq (or (plist-get arguments :connection-type) 'pipe)
          'pipe)
      (remote-backend-process-plan-create
       :arguments
       (plist-put
        (plist-put
         (copy-sequence arguments)
         :command
         (remote-backend-tramp-direct-async-command
          (remote-backend-execution-route execution)
          (remote-backend-execution-command execution)
          environment
          (if-let* ((directory
                     (remote-backend-execution-logical-directory
                      execution)))
              (remote-file-local-name directory)
            (remote-context-localname
             (remote-backend-execution-context execution)))))
        :file-handler nil)
       :default-directory temporary-file-directory
       :stderr-mode 'native
       :process-properties '((remote-direct-ssh . t))
       :metadata '(:placement client :stdio ssh))
    (remote-backend-tramp-handler-process-plan
     execution arguments environment)))

(defun remote-backend-tramp-handler-process-plan
    (execution arguments _environment)
  "Return a projected file-handler process plan for EXECUTION.
This is shared by standard TRAMP's non-pipe path and tramp-rpc."
  (remote-backend-process-plan-create
   :arguments
   (if (plist-member arguments :file-handler)
       (copy-sequence arguments)
     (plist-put (copy-sequence arguments) :file-handler t))
   :default-directory
   (remote-backend-execution-physical-directory execution)
   :stderr-mode 'framed
   :metadata '(:placement target :stdio file-handler)))

(defun remote-backend-tramp-direct-copy-file
    (route local-file target-file)
  "Copy LOCAL-FILE to target-native TARGET-FILE over ROUTE's SCP channel.
Return the local scp exit status.  This is intended for tool provisioning and
other bulk pipeline transfers; ordinary editor file operations remain owned by
the file-name handler."
  (pcase-let* ((`(,destination ,jumps ,_config)
                 (remote-backend-tramp--pipeline-ssh-parts route))
                (scp
                 (or (executable-find "scp")
                     (signal
                      'remote-backend-unsupported
                      '("Local scp executable is unavailable"))))
                (options
                 (remote-backend-tramp--ssh-options
                  (remote-route-pipeline route)))
                (arguments nil))
    (dolist (option options)
      (setq arguments (append arguments (list "-o" option))))
    (when jumps
      (setq arguments
            (append
             arguments
             (list "-J"
                   (remote-backend-tramp--ssh-jump-argument jumps)))))
    (when-let* ((port (remote-endpoint-port destination)))
      (setq arguments
            (append arguments (list "-P" (format "%s" port)))))
    (let ((default-directory temporary-file-directory))
      (apply
       #'call-process scp nil nil nil
       (append
        arguments
        (list
         local-file
         (format "%s:%s"
                 (remote-backend-tramp--ssh-destination destination)
                 target-file)))))))

(defun remote-backend-tramp--ssh-forward-command
    (route local-host local-port remote-host remote-port
           &optional direction)
  "Build an SSH forward command for ROUTE and the supplied endpoints.
DIRECTION defaults to `local'.  `reverse' creates an SSH `-R' listener at
REMOTE-HOST and REMOTE-PORT which connects to LOCAL-HOST and LOCAL-PORT."
  (let* ((pipeline (remote-route-pipeline route))
         (config (remote-pipeline-effective-config pipeline))
         (hops (or (plist-get config :hops)
                   (list
                    (remote-endpoint-create
                     :target-id (remote-route-target-id route)
                     :host (plist-get config :host)
                     :port (plist-get config :port)
                     :user (plist-get config :user)
                     :method
                     (or (plist-get config :method) "ssh")))))
         (unsupported
          (seq-find
           (lambda (hop)
             (not
              (member (or (remote-endpoint-method hop) "ssh")
                      '("ssh" "sshx" "scp"))))
           hops))
         (destination (car (last hops)))
         (jumps (butlast hops))
         (ssh (or (executable-find "ssh")
                  (signal 'remote-backend-unsupported
                          (list "Local ssh executable is unavailable"))))
         (command
          (list
           ssh "-N" "-T"
           "-o" "ExitOnForwardFailure=yes"
           "-o" "ServerAliveInterval=30"
           "-o" "ServerAliveCountMax=3")))
    (when (eq direction 'reverse)
      ;; OpenSSH reports reverse-forward allocation and confirmation at the
      ;; debug1 level.  The bounded diagnostic buffer is also used to discover
      ;; the target port selected for `-R ...:0:...'.
      (setq command (append command (list "-v"))))
    (when unsupported
      (signal
       'remote-backend-unsupported
       (list
        (format
         "SSH forwarding cannot cross final %s stage"
         (remote-endpoint-method unsupported)))))
    (when jumps
      (setq command
            (append
             command
             (list
              "-J"
              (mapconcat
               (lambda (hop)
                 (concat
                  (remote-backend-tramp--ssh-destination hop)
                  (if-let* ((port (remote-endpoint-port hop)))
                      (format ":%s" port)
                    "")))
               jumps ",")))))
    (when-let* ((port (remote-endpoint-port destination)))
      (setq command
            (append command (list "-p" (format "%s" port)))))
    (append
     command
     (list
      (if (eq direction 'reverse) "-R" "-L")
      (if (eq direction 'reverse)
          (format "%s:%s:%s:%s"
                  (remote-backend-tramp--ssh-forward-host remote-host)
                  remote-port
                  (remote-backend-tramp--ssh-forward-host local-host)
                  local-port)
        (format "%s:%s:%s:%s"
                (remote-backend-tramp--ssh-forward-host local-host)
                local-port
                (remote-backend-tramp--ssh-forward-host remote-host)
                remote-port))
      (remote-backend-tramp--ssh-destination destination)))))

(defun remote-backend-tramp--reserve-local-port (host)
  "Return a currently unused TCP port on HOST."
  (let ((server
         (make-network-process
          :name "remote-forward-reservation"
          :server t
          :host host
          :service 0
          :noquery t)))
    (unwind-protect
        (process-contact server :service)
      (delete-process server))))

(defun remote-backend-tramp--local-port-open-p (host port)
  "Return non-nil when HOST PORT accepts a TCP connection."
  (condition-case nil
      (let ((probe
             (make-network-process
              :name "remote-forward-probe"
              :host host
              :service port
              :coding 'binary
              :noquery t
              :nowait nil)))
        (delete-process probe)
        t)
    (file-error nil)))

(defun remote-backend-tramp--reverse-forward-port
    (buffer requested-port)
  "Return confirmed reverse-forward port parsed from BUFFER.
REQUESTED-PORT is returned after OpenSSH confirms a fixed listener.  For a
dynamic listener, parse the allocated target port from OpenSSH diagnostics."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (cond
         ((re-search-forward
           "Allocated port \\([0-9]+\\) for remote forward" nil t)
          (string-to-number (match-string 1)))
         ((and (integerp requested-port)
               (> requested-port 0)
               (re-search-forward
                "remote forward success for: listen " nil t))
          requested-port))))))

(defun remote-backend-tramp--close-forward (forward)
  "Close SSH FORWARD and its diagnostic buffer."
  (when-let* ((process (remote-forward-handle forward)))
    (when (process-live-p process)
      (delete-process process)))
  (when-let* ((buffer
               (plist-get
                (remote-forward-metadata forward)
                :diagnostic-buffer)))
    (when (buffer-live-p buffer)
      (remote--kill-internal-buffer buffer))))

(defun remote-backend-tramp-forward
    (route context local-endpoint remote-endpoint metadata)
  "Open an SSH local or reverse forward for ROUTE."
  (let* ((direction (or (plist-get metadata :direction) 'local))
         (reverse (eq direction 'reverse))
         (local-host
          (remote-backend-tramp--channel-endpoint-value
           local-endpoint :host "127.0.0.1"))
         (local-port
          (remote-backend-tramp--channel-endpoint-value
           local-endpoint :port))
         (local-port
          (if reverse
              (or local-port
                  (error "Reverse-forward local endpoint has no port: %S"
                         local-endpoint))
            (if (and (integerp local-port) (> local-port 0))
                local-port
              (remote-backend-tramp--reserve-local-port local-host))))
         (remote-host
          (remote-backend-tramp--channel-endpoint-value
           remote-endpoint :host "127.0.0.1"))
         (remote-port
          (remote-backend-tramp--channel-endpoint-value
           remote-endpoint :port))
         (remote-port
          (if (and reverse
                   (or (eq remote-port t)
                       (null remote-port)))
              0
            remote-port))
         (_
          (unless (and (or (integerp remote-port)
                           (stringp remote-port))
                       (not (equal remote-port "")))
            (error "Remote endpoint has no port: %S" remote-endpoint)))
         (command
          (remote-backend-tramp--ssh-forward-command
           route local-host local-port remote-host remote-port direction))
         (buffer
          (generate-new-buffer
           (format " *remote-forward-%s*"
                   (remote-route-target-id route))))
         (default-directory temporary-file-directory)
         process forward)
    (condition-case error
        (progn
          (setq process
                (make-process
                 :name
                 (format "remote-forward-%s-%s"
                         (remote-route-target-id route)
                         remote-port)
                 :buffer buffer
                 :command command
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t))
          (setq forward
                (remote-forward-create
                 :backend-id (remote-route-link-plugin-id route)
                 :route route
                 :context context
                 :handle process
                 :close-function
                 #'remote-backend-tramp--close-forward
                 :local-endpoint
                 (list :host local-host :port local-port)
                 :remote-endpoint
                 (list :host remote-host :port remote-port)
                 :state 'opening
                 :metadata
                 (append
                  metadata
                  (list
                   :direction direction
                   :command command
                   :diagnostic-buffer buffer))))
          (set-process-sentinel
           process
           (lambda (finished event)
             (when (memq (process-status finished)
                         '(exit signal failed))
               (unless (eq (remote-forward-state forward) 'closed)
                 (setf
                  (remote-forward-state forward) 'failed
                  (remote-forward-metadata forward)
                  (plist-put
                   (remote-forward-metadata forward)
                   :failure event))
                 (when (buffer-live-p buffer)
                   (setf
                    (remote-forward-metadata forward)
                    (plist-put
                     (remote-forward-metadata forward)
                     :diagnostic
                     (with-current-buffer buffer
                       (buffer-string))))
                   (remote--kill-internal-buffer buffer))))))
          (let ((deadline
                 (+ (float-time)
                    remote-backend-ssh-forward-timeout))
                confirmed-port)
            (while
                (and
                 (process-live-p process)
                 (not
                  (setq
                   confirmed-port
                   (if reverse
                       (remote-backend-tramp--reverse-forward-port
                        buffer remote-port)
                     (and
                      (remote-backend-tramp--local-port-open-p
                       local-host local-port)
                      local-port))))
                 (< (float-time) deadline))
              (accept-process-output process 0.05))
            (when (and reverse confirmed-port)
              (setq remote-port confirmed-port)
              (setf
               (remote-forward-remote-endpoint forward)
               (list :host remote-host :port confirmed-port)))
            (unless (and (process-live-p process) confirmed-port)
              (let ((diagnostic
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (string-trim (buffer-string))))))
                (remote-backend-tramp--close-forward forward)
                (error "SSH forward failed: %s"
                       (or diagnostic "listener did not start")))))
          (setf (remote-forward-state forward) 'open)
          forward)
      (error
       (when (processp process)
         (when (process-live-p process)
           (delete-process process)))
       (when (buffer-live-p buffer)
         (remote--kill-internal-buffer buffer))
       (signal (car error) (cdr error))))))

(defun remote-backend-tramp--attach-forward (process forward)
  "Attach FORWARD lifecycle to client PROCESS."
  (process-put process 'remote-forward forward)
  (let ((sentinel (process-sentinel process)))
    (set-process-sentinel
     process
     (lambda (finished event)
       (unwind-protect
           (when sentinel
             (funcall sentinel finished event))
         (when (memq (process-status finished)
                     '(exit signal closed failed))
           (remote-backend-tramp--close-forward forward)
           (setf (remote-forward-state forward) 'closed))))))
  process)

(defun remote-backend-tramp-network (route context arguments)
  "Create a network process through an SSH local or reverse forward."
  (when (memq (plist-get arguments :type) '(datagram seqpacket))
    (signal
     'remote-backend-unsupported
     (list "SSH routed network processes support TCP streams only")))
  (when (memq (plist-get arguments :family) '(local local-socket))
    (signal
     'remote-backend-unsupported
     (list "SSH routed network processes do not expose local sockets")))
  (if (plist-get arguments :server)
      (let* ((remote-host (or (plist-get arguments :host) "127.0.0.1"))
             (remote-port (plist-get arguments :service))
             (remote-port
              (if (eq remote-port t) 0 remote-port))
             (_
              (unless (integerp remote-port)
                (error
                 "Remote SSH listeners require a numeric service: %S"
                 (plist-get arguments :service))))
             (local-arguments (copy-sequence arguments))
             (local-arguments
              (plist-put local-arguments :host "127.0.0.1"))
             (local-arguments
              (plist-put local-arguments :service 0))
             server forward)
        (condition-case error
            (let ((default-directory temporary-file-directory))
              (setq server
                    (apply #'make-network-process local-arguments))
              (setq forward
                    (remote-backend-tramp-forward
                     route context
                     (list
                      :host "127.0.0.1"
                      :port (process-contact server :service))
                     (list :host remote-host :port remote-port)
                     '(:owner network-server :direction reverse)))
              (process-put
               server 'remote-listen-endpoint
               (remote-forward-remote-endpoint forward))
              (remote-backend-tramp--attach-forward server forward))
          (error
           (when (and (processp server) (process-live-p server))
             (delete-process server))
           (when (remote-forward-p forward)
             (remote-backend-tramp--close-forward forward)
             (setf (remote-forward-state forward) 'closed))
           (signal (car error) (cdr error)))))
    (let* ((host (plist-get arguments :host))
           (service (plist-get arguments :service))
           (forward
            (remote-backend-tramp-forward
             route context nil
             (list :host host :port service)
             '(:owner network-process)))
           (local (remote-forward-local-endpoint forward))
           (arguments
            (plist-put
             (plist-put
              (copy-sequence arguments)
              :host (plist-get local :host))
             :service (plist-get local :port)))
           process)
      (condition-case error
          (let ((default-directory temporary-file-directory))
            (setq process (apply #'make-network-process arguments))
            (remote-backend-tramp--attach-forward process forward))
        (error
         (remote-backend-tramp--close-forward forward)
         (setf (remote-forward-state forward) 'closed)
         (signal (car error) (cdr error)))))))

(defun remote-backend-tramp-stream
    (route context name buffer host service parameters)
  "Open a network stream through an SSH forward.
The protocol layer retains HOST and SERVICE so TLS SNI, certificate
verification, auth-source, greetings, and returned properties keep their
native meaning.  Only the lowest `make-network-process' connection endpoint is
rewritten to the client-side relay."
  (when
      (and
       (not (gnutls-available-p))
       (or
        (memq (plist-get parameters :type) '(tls ssl starttls))
        (and (memq (plist-get parameters :type) '(nil network))
             (plist-get parameters :success)
             (plist-get parameters :capability-command))))
    ;; External TLS helpers open their own socket and would bypass the local
    ;; relay.  Failing explicitly is safer than silently connecting from the
    ;; client or validating the relay host.
    (signal
     'remote-backend-unsupported
     '("Routed TLS/STARTTLS requires Emacs built-in GnuTLS")))
  (let* ((forward
          (remote-backend-tramp-forward
           route context nil
           (list :host host :port service)
           '(:owner network-stream)))
         (local (remote-forward-local-endpoint forward))
         (local-host (plist-get local :host))
         (local-service (plist-get local :port))
         result)
    (condition-case error
        (let ((default-directory temporary-file-directory))
          (let ((native-make-network-process
                 (symbol-function 'make-network-process)))
            (cl-letf
                (((symbol-function 'make-network-process)
                  (lambda (&rest arguments)
                    (setq arguments
                          (plist-put arguments :host local-host)
                          arguments
                          (plist-put
                           arguments :service local-service))
                    (apply native-make-network-process arguments))))
              (setq result
                    (apply
                     #'open-network-stream
                     name buffer host service parameters))))
          (if-let* ((process
                     (cond
                      ((processp result) result)
                      ((and (consp result)
                            (processp (car result)))
                       (car result)))))
              (remote-backend-tramp--attach-forward process forward)
            ;; Some native stream modes report negotiation failure in a
            ;; return-list instead of signaling.  Preserve that result while
            ;; releasing the no-longer-useful relay.
            (remote-backend-tramp--close-forward forward)
            (setf (remote-forward-state forward) 'closed))
          result)
      (error
       (remote-backend-tramp--close-forward forward)
       (setf (remote-forward-state forward) 'closed)
       (signal (car error) (cdr error))))))

(defun remote-backend-tramp-register ()
  "Register the standard TRAMP backend."
  (remote-register-backend
   "tramp"
   :capabilities remote-backend-tramp-capabilities
   :project #'remote-backend-tramp-project
   :expand-localname #'remote-backend-tramp-expand-localname
   :connect #'remote-backend-tramp-connect
   :live #'remote-backend-tramp-live-p
   :disconnect #'remote-backend-tramp-disconnect
   :prepare-process #'remote-backend-tramp-prepare-process
   :stdio-bridge #'remote-backend-tramp-stdio-bridge
   :make-network-process #'remote-backend-tramp-network
   :open-network-stream #'remote-backend-tramp-stream
   :port-forward #'remote-backend-tramp-forward
   :program-form 'search
   :describe
   (lambda ()
     '(:kind tramp :session-owner tramp))))

(provide 'remote-backend-tramp)
;;; remote-backend-tramp.el ends here
