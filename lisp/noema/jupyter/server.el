;;; noema/jupyter/server.el --- Remote Jupyter servers for Noema -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Kernels normally come from `init-noema-jupyter-runtime': the broker
;; launches a process on the note's Target and forwards its five ZMQ channels.
;; This module covers the other case — a Jupyter server that already exists and
;; is spoken to over HTTP(S): a lab server on a cluster login node, a
;; JupyterHub, or a kernel gateway.
;;
;; Emacs exposes a configured endpoint/credential catalogue to Noema; Noema
;; still owns the Jupyter server sessions and kernel registry.  Secrets come
;; from `auth-source', so no token is ever written into the repository.  And
;; reachability is a Remote question: a server bound to a
;; login node's loopback interface does not exist from the client machine, so
;; `.resolve' opens a `remote-port-forward' inside the owning Target's
;; workspace and hands Noema a client-side URL.  A Target that cannot provide a
;; channel is an error — never a silent fall back to opening a socket here,
;; which would either fail confusingly or, worse, reach a *different* server
;; that happens to answer on that port locally.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'config)
(require 'remote-channel)
(require 'remote-doctor)
(require 'remote-gateway)
(require 'remote-workspace)
(require 'remote-backend)
(require 'remote-backend-jupyter)

(declare-function +noema-jupyter--defer "noema/jupyter/runtime" (function))
(declare-function +noema-jupyter--get "noema/jupyter/runtime" (key alist))

(defvar +noema-jupyter-servers nil
  "Remote Jupyter servers Noema may connect to.

Each entry is a plist:

  :id                stable identifier used in kernel names
  :name              display name; defaults to `:id'
  :url               server URL, for example \"https://lab.example.org/\"
  :kind              `server' (default) or `gateway'
  :auth              `token' (default), `password', `hub', or `none'
  :user              JupyterHub user whose single-user server to use
  :target            owning Remote target id; defaults to \"local\"
  :insecure          accept a TLS certificate this client cannot verify
  :server-name       TLS SNI / Host name, when `:url' is a forward

Secrets are never stored here.  `:auth token' and `:auth password' look the
secret up with `auth-source-search' against the server host, so it lives in
your authinfo/GPG store like any other credential.

Set this through the config board or `etc/config-store.el', not with `setq'.")

(config-register '+noema-jupyter-servers
  :type 'sexp
  :group 'noema
  :on-change #'remote-jupyter-register
  :doc "Remote Jupyter servers (list of plists; see the variable docstring).")

(defvar +noema-jupyter-server--forwards (make-hash-table :test #'equal)
  "Open client forwards to target-side Jupyter servers, keyed by server id.")

(defvar +noema-jupyter-server--credentials (make-hash-table :test #'equal)
  "Session-only credentials keyed by server id and connection identity.
Pasted URL tokens never enter the persistent config or public catalog.")

(defun +noema-jupyter-server--parse-url (value)
  "Return sanitized base URL and optional token from VALUE as a plist."
  (let* ((url (url-generic-parse-url (string-trim value)))
         (filename (or (url-filename url) ""))
         (parts (split-string filename "?"))
         (query (cadr parts))
         (path (car parts)))
    (unless (and (member (url-type url) '("http" "https"))
                 (url-host url) (not (string-empty-p (url-host url)))
                 (not (url-user url)) (not (url-password url))
                 (let ((port (url-port url))) (and (> port 0) (< port 65536))))
      (user-error "Use an HTTP(S) Jupyter URL without username/password"))
    (setq path (replace-regexp-in-string
                "/\\(?:lab\\|tree\\|notebooks\\|doc\\)\\(?:/.*\\)?\\'" "/" path))
    (unless (string-suffix-p "/" path) (setq path (concat path "/")))
    (setf (url-filename url) path (url-target url) nil)
    (list :url (url-recreate-url url)
          :token (cadr (assoc "token" (and query (url-parse-query-string query)))))))

(defun +noema-jupyter-server--public-url (entry)
  "Return ENTRY's credential-free URL for presentation."
  (condition-case nil
      (plist-get (+noema-jupyter-server--parse-url (plist-get entry :url)) :url)
    (error "Invalid server URL")))

(defun +noema-jupyter-server--credential-key (entry)
  "Return the identity a session credential for ENTRY is allowed to serve."
  (list (plist-get entry :id) (+noema-jupyter-server--public-url entry)
        (+noema-jupyter-server--target entry)
        (or (plist-get entry :auth) 'token) (plist-get entry :user)))

(defun +noema-jupyter-server-save (entry &optional credential)
  "Persist ENTRY without URL credentials; keep CREDENTIAL for this session.
An existing id updates that profile.  Connection identity changes release the
old route, so the next resolve cannot reuse a listener for another server."
  (let* ((entry (copy-sequence entry))
         (id (plist-get entry :id))
         (parsed (+noema-jupyter-server--parse-url (plist-get entry :url)))
         (old (+noema-jupyter-server--entry id)))
    (unless (and (stringp id) (string-match-p "\\`[[:alnum:]_-]+\\'" id))
      (user-error "Server id must contain only letters, digits, underscore or dash"))
    (unless (remote-get-target (+noema-jupyter-server--target entry))
      (user-error "Select a configured Remote target"))
    (unless (and (memq (or (plist-get entry :kind) 'server) '(server gateway))
                 (memq (or (plist-get entry :auth) 'token) '(token password hub none)))
      (user-error "Invalid Jupyter service or authentication type"))
    (when (and (eq (plist-get entry :auth) 'hub)
               (string-empty-p (or (plist-get entry :user) "")))
      (user-error "JupyterHub requires a user name"))
    (when (and (plist-get parsed :token)
               (memq (plist-get entry :auth) '(password none)))
      (user-error "A token URL requires token or hub authentication"))
    (setq entry (plist-put entry :url (plist-get parsed :url)))
    (config-set '+noema-jupyter-servers
                (append (seq-remove (lambda (item) (equal (plist-get item :id) id))
                                    +noema-jupyter-servers)
                        (list entry)) nil t)
    (when (and old (not (equal old entry)))
      (+noema-jupyter-server--close-resource old id 'configuration-changed)
      (unless (equal (+noema-jupyter-server--credential-key old)
                     (+noema-jupyter-server--credential-key entry))
        (remhash (+noema-jupyter-server--credential-key old)
                 +noema-jupyter-server--credentials)))
    (when-let* ((secret (or (and credential (not (string-empty-p credential)) credential)
                           (plist-get parsed :token)))
                (_ (not (string-empty-p secret))))
      (puthash (+noema-jupyter-server--credential-key entry) secret
               +noema-jupyter-server--credentials))
    entry))

(defun +noema-jupyter-server-remove (id)
  "Forget profile ID and release its route, without shutting down kernels."
  (when-let* ((entry (+noema-jupyter-server--entry id)))
    (config-set '+noema-jupyter-servers
                (seq-remove (lambda (item) (equal (plist-get item :id) id))
                            +noema-jupyter-servers) nil t)
    (remhash (+noema-jupyter-server--credential-key entry)
             +noema-jupyter-server--credentials)
    (+noema-jupyter-server--close-resource entry id 'configuration-removed)))

(defvar +noema-jupyter-server--workspaces (make-hash-table :test #'equal)
  "Owning workspaces for Jupyter server forwards, keyed by server id.
This is retained independently of configuration so removing a server entry
cannot orphan the already registered recoverable resource.")

(defun +noema-jupyter-server--workspace (entry)
  "Return the Remote workspace which owns ENTRY's forward."
  (let* ((target (+noema-jupyter-server--target entry))
         (context (remote-context (remote-make-file-name target "/"))))
    (remote-workspace-open context)))

(defun +noema-jupyter-server--close-resource (entry id &optional reason)
  "Close ENTRY's keyed forward resource ID, if one exists.
REASON is passed to the workspace resource close contract."
  (let* ((workspace
          (or (gethash id +noema-jupyter-server--workspaces)
              (and entry (+noema-jupyter-server--workspace entry))))
         (resource
          (and workspace
               (remote-workspace-find-resource workspace 'jupyter-server id))))
    (when resource
      (remote-workspace-close-resource workspace resource reason))))

(defun +noema-jupyter-server--entry (id)
  "Return the configured server plist for ID, or nil."
  (seq-find
   (lambda (entry)
     (equal (format "%s" (plist-get entry :id)) (format "%s" id)))
   +noema-jupyter-servers))

(defun +noema-jupyter-server--target (entry)
  "Return the Remote target id owning ENTRY."
  (or (plist-get entry :target) "local"))

(defun +noema-jupyter-server--secret (entry)
  "Return ENTRY's secret from `auth-source', or nil.
The host is taken from `:url' so one authinfo line serves every server on
that host, and the user from `:user' when the entry names one."
  (or (gethash (+noema-jupyter-server--credential-key entry)
               +noema-jupyter-server--credentials)
      (let* ((url (url-generic-parse-url (format "%s" (plist-get entry :url))))
         (host (url-host url))
         (port (url-port url))
         (found
          (car
           (apply #'auth-source-search
                  :max 1 :host host
                  (append
                   (when port (list :port (number-to-string port)))
                   (when-let* ((user (plist-get entry :user)))
                     (list :user user)))))))
    (when-let* ((secret (plist-get found :secret)))
      (if (functionp secret) (funcall secret) secret)))))

(defun +noema-jupyter-server--endpoint (entry)
  "Return (HOST . PORT) for ENTRY's URL, applying the scheme's default port."
  (let* ((url (url-generic-parse-url (format "%s" (plist-get entry :url))))
         (host (url-host url))
         (scheme (url-type url)))
    (unless (and host (not (string-empty-p host)))
      (error "Jupyter server %s has no host in its URL" (plist-get entry :id)))
    (cons host
          (or (url-portspec url)
              (if (equal scheme "https") 443 80)))))

(defun +noema-jupyter-server--client-url (entry port)
  "Return ENTRY's URL rewritten to reach a client-side forward on PORT."
  (let ((url (url-generic-parse-url (format "%s" (plist-get entry :url)))))
    (setf (url-host url) "127.0.0.1"
          (url-portspec url) port)
    (url-recreate-url url)))

(defun +noema-jupyter-server--forward (entry)
  "Open (or reuse) a client forward to ENTRY's target-side server.

The forward is registered against the target's workspace as a recoverable
resource, so a transport drop is repaired the same way a kernel's channel
group is rather than leaving Noema pointed at a dead local port."
  (let* ((id (format "%s" (plist-get entry :id)))
         (existing (gethash id +noema-jupyter-server--forwards)))
    (if (and existing (remote-channel-live-p existing))
        existing
      ;; A dead forward still owns a client-side listener and a workspace
      ;; resource entry.  Remove that keyed owner before making a replacement;
      ;; otherwise a later workspace recovery can resurrect the old handle.
      (when existing
        (unless (ignore-errors
                  (+noema-jupyter-server--close-resource
                   entry id 'stale-forward))
          (remhash id +noema-jupyter-server--forwards)
          (ignore-errors (remote-close-channel existing))))
      (let* ((target (+noema-jupyter-server--target entry))
             (context (remote-context (remote-make-file-name target "/")))
             (workspace (remote-workspace-open context))
             (endpoint (+noema-jupyter-server--endpoint entry))
             (forward
              (remote-port-forward
               (list :host (car endpoint) :port (cdr endpoint))
               :context context :workspace workspace
               :metadata (list :application "noema-jupyter-server"
                               :server id)
               ;; The keyed `jupyter-server' resource below is the one owner.
               ;; Registering the generic forward too creates two independent
               ;; recovery loops for one listener.
               :register nil)))
        (condition-case error
            (progn
              (puthash id workspace +noema-jupyter-server--workspaces)
              (puthash id forward +noema-jupyter-server--forwards)
              (remote-workspace-ensure-recoverable-resource
               workspace 'jupyter-server id forward
               :close
               (lambda (value reason)
                 (ignore-errors (remote-close-channel value))
                 (unless (eq reason 'transport-recovery)
                   (remhash id +noema-jupyter-server--forwards)
                   (remhash id +noema-jupyter-server--workspaces)))
               :recover
               (lambda (resource _owner)
                 ;; Preserve the client endpoint.  Noema's cached HTTP and WebSocket
                 ;; settings remain valid while the target-side route is replaced.
                 ;; If rebinding this exact listener is impossible, recovery fails
                 ;; atomically; the next resolve discards the failed resource and
                 ;; negotiates a new endpoint instead of silently using a stale URL.
                 (let* ((old (remote-workspace-resource-value resource))
                        (local (remote-channel-endpoint old 'local))
                        (replacement
                         (remote-port-forward
                          (list :host (car endpoint) :port (cdr endpoint))
                          :local-endpoint local
                          :context context :workspace workspace
                          :metadata (list :application "noema-jupyter-server"
                                          :server id)
                          :register nil)))
                   (puthash id replacement +noema-jupyter-server--forwards)
                   replacement))
               :recovery 'auto
               :metadata (list :application "noema-jupyter-server" :server id))
              forward)
          (error
           ;; A forward without its keyed workspace resource has no recovery
           ;; owner and must never escape from a partially completed open.
           (remhash id +noema-jupyter-server--forwards)
           (remhash id +noema-jupyter-server--workspaces)
           (ignore-errors (remote-close-channel forward))
           (signal (car error) (cdr error))))))))

(defun +noema-jupyter-server--resolve-entry (entry)
  "Return the Noema-facing connection descriptor for ENTRY."
  (let* ((id (format "%s" (plist-get entry :id)))
         (target (+noema-jupyter-server--target entry))
         (auth (or (plist-get entry :auth) 'token))
         (local-p (equal target "local"))
         (url (format "%s" (plist-get entry :url)))
         (server-name (plist-get entry :server-name))
         secret)
    ;; Resolve credentials before opening a routed listener.  A missing
    ;; password is a configuration error and must not leave a forward behind
    ;; that no Noema connection can ever own or release.
    (when (memq auth '(token password hub))
      (setq secret (+noema-jupyter-server--secret entry))
      (when (and (null secret) (memq auth '(password)))
        (error "No auth-source secret found for Jupyter server %s" id)))
    (unless local-p
      ;; The server lives on a Target.  Reaching it means a routed channel;
      ;; there is deliberately no fallback that would open the socket here.
      (let* ((forward (+noema-jupyter-server--forward entry))
             (local (remote-channel-endpoint forward 'local))
             (port (plist-get local :port)))
        (unless port
          (error "Remote channel for Jupyter server %s exposed no client port" id))
        (setq url (+noema-jupyter-server--client-url entry port))
        ;; The URL now says 127.0.0.1, so certificate verification and any
        ;; virtual-host routing need the real name carried alongside it.
        (setq server-name
              (or server-name (car (+noema-jupyter-server--endpoint entry))))))
    (append
     `((id . ,id)
       (url . ,url)
       (kind . ,(format "%s" (or (plist-get entry :kind) "server")))
       (auth . ,(format "%s" auth))
       (target . ,target))
     (when secret
       (if (eq auth 'password) `((password . ,secret)) `((token . ,secret))))
     (when-let* ((user (plist-get entry :user))) `((user . ,user)))
     (when server-name `((serverName . ,server-name)))
     (when (plist-get entry :insecure) '((allowUnauthorized . t))))))

(defun +noema-jupyter-server--list (_params _client)
  "List configured Jupyter servers, without secrets."
  (+noema-jupyter--defer
   (lambda ()
     `((ok . t)
       (servers .
                ,(mapcar
                  (lambda (entry)
                    `((id . ,(format "%s" (plist-get entry :id)))
                      (displayName .
                                   ,(format "%s" (or (plist-get entry :name)
                                                     (plist-get entry :id))))
                      (url . ,(+noema-jupyter-server--public-url entry))
                      (kind . ,(format "%s" (or (plist-get entry :kind) "server")))
                      (target . ,(+noema-jupyter-server--target entry))))
                  +noema-jupyter-servers))))))

(defun +noema-jupyter-server--resolve (params _client)
  "Resolve one server for Noema, opening a channel first when it is remote."
  (+noema-jupyter--defer
   (lambda ()
     (let* ((id (format "%s" (+noema-jupyter--get 'serverId params)))
            (entry (+noema-jupyter-server--entry id)))
       (unless entry
         (error "Unknown Jupyter server: %s" id))
       (+noema-jupyter-server--resolve-entry entry)))))

(defun +noema-jupyter-server--release (params _client)
  "Close any client forward held open for PARAMS server."
  (+noema-jupyter--defer
   (lambda ()
     (let* ((id (format "%s" (+noema-jupyter--get 'serverId params)))
            (entry (+noema-jupyter-server--entry id))
            (forward (gethash id +noema-jupyter-server--forwards)))
       (unless (ignore-errors
                 (+noema-jupyter-server--close-resource
                  entry id 'consumer-release))
         (remhash id +noema-jupyter-server--forwards)
         (remhash id +noema-jupyter-server--workspaces)
         (when forward
           (ignore-errors (remote-close-channel forward))))
       '((ok . t))))))

(defun +noema-jupyter-server--doctor (target probe)
  "Report configured Jupyter servers owned by TARGET.
With PROBE, resolve each one and fetch `/api/status' through the resolved
client URL, so a broken forward is reported as a broken server rather than
surfacing later as an unexplained kernel failure."
  (let* ((target-id (remote-target-id target))
         (entries
          (seq-filter
           (lambda (entry)
             (equal target-id (+noema-jupyter-server--target entry)))
           +noema-jupyter-servers))
         checks)
    (when entries
      (push (list :name 'noema-jupyter-servers :status 'ok
                  :detail (format "%d configured server(s)" (length entries)))
            checks))
    (when probe
      (dolist (entry entries)
        (let ((id (format "%s" (plist-get entry :id))))
          (push
           (condition-case error
               (let* ((resolved (+noema-jupyter-server--resolve-entry entry))
                      (url (alist-get 'url resolved))
                      (status (+noema-jupyter-server--probe resolved url)))
                 (list :name (intern (format "noema:jupyter-server:%s" id))
                       :status (if status 'ok 'error)
                       :detail (or status "no response from /api/status")
                       :remedy
                       (unless status
                         (format "Check that %s is running and reachable from target %s"
                                 (plist-get entry :url)
                                 (+noema-jupyter-server--target entry)))))
             (error
              (list :name (intern (format "noema:jupyter-server:%s" id))
                    :status 'error :detail (error-message-string error)
                    :remedy "Check :url, :target, and the auth-source entry")))
           checks))))
    (nreverse checks)))

(defun +noema-jupyter-server--probe (resolved url)
  "Fetch `/api/status' for RESOLVED at URL; return a summary string or nil."
  (require 'url)
  (let* ((token (alist-get 'token resolved))
         (password (alist-get 'password resolved))
         (url-request-extra-headers
          (cond
           (token (list (cons "Authorization" (format "token %s" token))))
           ;; A password-authenticated server still answers /api/status for a
           ;; valid token-style credential; sending nothing made every such
           ;; server look unreachable.
           (password (list (cons "Authorization" (format "token %s" password))))))
         ;; A forwarded server is addressed as 127.0.0.1, so its certificate
         ;; never matches the URL.  Doctor was reporting perfectly healthy
         ;; remote servers as broken purely because of that.
         (gnutls-verify-error
          (if (or (alist-get 'allowUnauthorized resolved)
                  (alist-get 'serverName resolved))
              nil
            (bound-and-true-p gnutls-verify-error)))
         (endpoint (concat (string-remove-suffix "/" url) "/api/status"))
         (buffer (ignore-errors (url-retrieve-synchronously endpoint t t 10))))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (let ((code (match-string 1)))
                (if (equal code "200")
                    (format "reachable (%s)" endpoint)
                  (format "HTTP %s from %s" code endpoint)))))
        (kill-buffer buffer)))))

(dolist (entry
         `(("noema.jupyter.server.list" . ,#'+noema-jupyter-server--list)
           ("noema.jupyter.server.resolve" . ,#'+noema-jupyter-server--resolve)
           ("noema.jupyter.server.release" . ,#'+noema-jupyter-server--release)))
  (remote-gateway-register-method (car entry) (cdr entry)))

(remote-doctor-register-check #'+noema-jupyter-server--doctor)

(add-hook 'remote-config-after-load-hook #'remote-jupyter-register)
(remote-jupyter-register)

(defun +noema-jupyter-server-file-name (id &optional path)
  "Return ID's logical Contents file name for PATH, relative to server root."
  (remote-jupyter-register)
  (let ((entry (+noema-jupyter-server--entry id)))
    (unless entry (user-error "Unknown Jupyter server: %s" id))
    (when (eq (plist-get entry :kind) 'gateway)
      (user-error "Kernel gateways do not expose a Contents filesystem"))
    (remote-make-file-name (remote-jupyter-target-id id)
                          (concat "/" (string-remove-prefix "/" (or path ""))))))

(defun +noema-jupyter/server-browse (id)
  "Browse server ID's Contents root in Dired, independently of local projects."
  (interactive (list (completing-read "Jupyter server: "
                                     (mapcar (lambda (entry) (plist-get entry :id))
                                             +noema-jupyter-servers) nil t)))
  (let ((root (+noema-jupyter-server-file-name id)))
    (+noema--ensure-server
     (lambda ()
       (let ((dired-listing-switches "-al"))
         (dired root))))))

(provide 'noema/jupyter/server)
;;; noema/jupyter/server.el ends here
