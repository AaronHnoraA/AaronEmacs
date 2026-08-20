;;; init-aaronnote-jupyter-server.el --- Remote Jupyter servers for Noema -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Kernels normally come from `init-aaronnote-jupyter-runtime': the broker
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
(require 'config)
(require 'remote-channel)
(require 'remote-doctor)
(require 'remote-gateway)
(require 'remote-workspace)

(declare-function my/noema-jupyter--defer "init-aaronnote-jupyter-runtime" (function))
(declare-function my/noema-jupyter--get "init-aaronnote-jupyter-runtime" (key alist))

(defvar my/noema-jupyter-servers nil
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

(config-register 'my/noema-jupyter-servers
  :type 'sexp
  :group 'noema
  :doc "Remote Jupyter servers (list of plists; see the variable docstring).")

(defvar my/noema-jupyter-server--forwards (make-hash-table :test #'equal)
  "Open client forwards to target-side Jupyter servers, keyed by server id.")

(defun my/noema-jupyter-server--entry (id)
  "Return the configured server plist for ID, or nil."
  (seq-find
   (lambda (entry)
     (equal (format "%s" (plist-get entry :id)) (format "%s" id)))
   my/noema-jupyter-servers))

(defun my/noema-jupyter-server--target (entry)
  "Return the Remote target id owning ENTRY."
  (or (plist-get entry :target) "local"))

(defun my/noema-jupyter-server--secret (entry)
  "Return ENTRY's secret from `auth-source', or nil.
The host is taken from `:url' so one authinfo line serves every server on
that host, and the user from `:user' when the entry names one."
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
      (if (functionp secret) (funcall secret) secret))))

(defun my/noema-jupyter-server--endpoint (entry)
  "Return (HOST . PORT) for ENTRY's URL, applying the scheme's default port."
  (let* ((url (url-generic-parse-url (format "%s" (plist-get entry :url))))
         (host (url-host url))
         (scheme (url-type url)))
    (unless (and host (not (string-empty-p host)))
      (error "Jupyter server %s has no host in its URL" (plist-get entry :id)))
    (cons host
          (or (url-portspec url)
              (if (equal scheme "https") 443 80)))))

(defun my/noema-jupyter-server--client-url (entry port)
  "Return ENTRY's URL rewritten to reach a client-side forward on PORT."
  (let ((url (url-generic-parse-url (format "%s" (plist-get entry :url)))))
    (setf (url-host url) "127.0.0.1"
          (url-portspec url) port)
    (url-recreate-url url)))

(defun my/noema-jupyter-server--forward (entry)
  "Open (or reuse) a client forward to ENTRY's target-side server.

The forward is registered against the target's workspace as a recoverable
resource, so a transport drop is repaired the same way a kernel's channel
group is rather than leaving Noema pointed at a dead local port."
  (let* ((id (format "%s" (plist-get entry :id)))
         (existing (gethash id my/noema-jupyter-server--forwards)))
    (if (and existing (remote-channel-live-p existing))
        existing
      ;; A dead forward still owns a client-side listener and a workspace
      ;; resource entry.  `remote-workspace-ensure-recoverable-resource'
      ;; replaces the handle in place below, so releasing it here is the only
      ;; chance its :close ever runs.
      (when existing
        (remhash id my/noema-jupyter-server--forwards)
        (ignore-errors (remote-close-channel existing)))
      (let* ((target (my/noema-jupyter-server--target entry))
             (context (remote-context (remote-make-file-name target "/")))
             (workspace (remote-workspace-open context))
             (endpoint (my/noema-jupyter-server--endpoint entry))
             (forward
              (remote-port-forward
               (list :host (car endpoint) :port (cdr endpoint))
               :context context :workspace workspace
               :metadata (list :application "noema-jupyter-server"
                               :server id))))
        (puthash id forward my/noema-jupyter-server--forwards)
        (remote-workspace-ensure-recoverable-resource
         workspace 'jupyter-server id forward
         :close
         (lambda (value reason)
           (unless (eq reason 'transport-recovery)
             (remhash id my/noema-jupyter-server--forwards)
             (ignore-errors (remote-close-channel value))))
         :recover
         (lambda (resource _owner)
           (let ((replacement (remote-channel-recover resource)))
             (puthash id replacement my/noema-jupyter-server--forwards)
             replacement))
         :recovery 'auto
         :metadata (list :application "noema-jupyter-server" :server id))
        forward))))

(defun my/noema-jupyter-server--resolve-entry (entry)
  "Return the Noema-facing connection descriptor for ENTRY."
  (let* ((id (format "%s" (plist-get entry :id)))
         (target (my/noema-jupyter-server--target entry))
         (auth (or (plist-get entry :auth) 'token))
         (local-p (equal target "local"))
         (url (format "%s" (plist-get entry :url)))
         (server-name (plist-get entry :server-name))
         secret)
    (unless local-p
      ;; The server lives on a Target.  Reaching it means a routed channel;
      ;; there is deliberately no fallback that would open the socket here.
      (let* ((forward (my/noema-jupyter-server--forward entry))
             (local (remote-channel-endpoint forward 'local))
             (port (plist-get local :port)))
        (unless port
          (error "Remote channel for Jupyter server %s exposed no client port" id))
        (setq url (my/noema-jupyter-server--client-url entry port))
        ;; The URL now says 127.0.0.1, so certificate verification and any
        ;; virtual-host routing need the real name carried alongside it.
        (setq server-name
              (or server-name (car (my/noema-jupyter-server--endpoint entry))))))
    (when (memq auth '(token password hub))
      (setq secret (my/noema-jupyter-server--secret entry))
      (when (and (null secret) (memq auth '(password)))
        (error "No auth-source secret found for Jupyter server %s" id)))
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

(defun my/noema-jupyter-server--list (_params _client)
  "List configured Jupyter servers, without secrets."
  (my/noema-jupyter--defer
   (lambda ()
     `((ok . t)
       (servers .
                ,(mapcar
                  (lambda (entry)
                    `((id . ,(format "%s" (plist-get entry :id)))
                      (displayName .
                                   ,(format "%s" (or (plist-get entry :name)
                                                     (plist-get entry :id))))
                      (url . ,(format "%s" (plist-get entry :url)))
                      (kind . ,(format "%s" (or (plist-get entry :kind) "server")))
                      (target . ,(my/noema-jupyter-server--target entry))))
                  my/noema-jupyter-servers))))))

(defun my/noema-jupyter-server--resolve (params _client)
  "Resolve one server for Noema, opening a channel first when it is remote."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((id (format "%s" (my/noema-jupyter--get 'serverId params)))
            (entry (my/noema-jupyter-server--entry id)))
       (unless entry
         (error "Unknown Jupyter server: %s" id))
       (my/noema-jupyter-server--resolve-entry entry)))))

(defun my/noema-jupyter-server--release (params _client)
  "Close any client forward held open for PARAMS server."
  (my/noema-jupyter--defer
   (lambda ()
     (let* ((id (format "%s" (my/noema-jupyter--get 'serverId params)))
            (forward (gethash id my/noema-jupyter-server--forwards)))
       (when forward
         (remhash id my/noema-jupyter-server--forwards)
         (ignore-errors (remote-close-channel forward)))
       '((ok . t))))))

(defun my/noema-jupyter-server--doctor (target probe)
  "Report configured Jupyter servers owned by TARGET.
With PROBE, resolve each one and fetch `/api/status' through the resolved
client URL, so a broken forward is reported as a broken server rather than
surfacing later as an unexplained kernel failure."
  (let* ((target-id (remote-target-id target))
         (entries
          (seq-filter
           (lambda (entry)
             (equal target-id (my/noema-jupyter-server--target entry)))
           my/noema-jupyter-servers))
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
               (let* ((resolved (my/noema-jupyter-server--resolve-entry entry))
                      (url (alist-get 'url resolved))
                      (status (my/noema-jupyter-server--probe resolved url)))
                 (list :name (intern (format "noema:jupyter-server:%s" id))
                       :status (if status 'ok 'error)
                       :detail (or status "no response from /api/status")
                       :remedy
                       (unless status
                         (format "Check that %s is running and reachable from target %s"
                                 (plist-get entry :url)
                                 (my/noema-jupyter-server--target entry)))))
             (error
              (list :name (intern (format "noema:jupyter-server:%s" id))
                    :status 'error :detail (error-message-string error)
                    :remedy "Check :url, :target, and the auth-source entry")))
           checks))))
    (nreverse checks)))

(defun my/noema-jupyter-server--probe (resolved url)
  "Fetch `/api/status' for RESOLVED at URL; return a summary string or nil."
  (require 'url)
  (let* ((token (alist-get 'token resolved))
         (url-request-extra-headers
          (when token (list (cons "Authorization" (format "token %s" token)))))
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
         `(("aaronnote.jupyter.server.list" . ,#'my/noema-jupyter-server--list)
           ("aaronnote.jupyter.server.resolve" . ,#'my/noema-jupyter-server--resolve)
           ("aaronnote.jupyter.server.release" . ,#'my/noema-jupyter-server--release)))
  (remote-gateway-register-method (car entry) (cdr entry)))

(remote-doctor-register-check #'my/noema-jupyter-server--doctor)

(provide 'init-aaronnote-jupyter-server)
;;; init-aaronnote-jupyter-server.el ends here
