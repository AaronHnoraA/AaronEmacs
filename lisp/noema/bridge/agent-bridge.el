;;; agent-bridge.el --- Local transport for the shared Emacs Agent API -*- lexical-binding: t; -*-

;; Legacy CLI transport only.  Session/queue/receipt ownership is tools/agent.
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'server)
(declare-function +agent/api "tools/agent/api" (body))
(declare-function +agent-requests-get "tools/agent/requests" (id))

(defvar +noema-agent-bridge-enabled nil
  "Non-nil when the local Emacs socket accepts Agent bridge requests.")

(defun +noema/agent-bridge-start ()
  "Enable local-socket access to Emacs-managed Agent sessions."
  (interactive)
  (when server-use-tcp (user-error "Agent bridge requires a local Emacs server socket"))
  (unless (server-running-p) (server-start))
  (setq +noema-agent-bridge-enabled t)
  (message "Agent bridge enabled on the local Emacs server socket"))

(defun +noema/agent-bridge-stop ()
  "Disable this transport without stopping any Agent or Emacs server."
  (interactive)
  (setq +noema-agent-bridge-enabled nil))

(defun +noema-agent-bridge--sessions ()
  "Expose public initialized ACP sessions, regardless of provider."
  (require 'tools/agent/api)
  (vconcat
   (delq nil
         (mapcar (lambda (buffer)
                   (let ((s (+agent-session-snapshot buffer)))
                     (when (and (eq (plist-get s :process-state) 'live) (plist-get s :native-session-id))
                       `((sessionId . ,(plist-get s :native-session-id))
                         (buffer . ,(buffer-name buffer))
                         (agent . ,(format "%s" (or (plist-get s :agent) (plist-get s :provider))))
                         (workspace . ,(directory-file-name (plist-get s :workspace))) (name . ,(plist-get s :name))
                         (busy . ,(if (plist-get s :busy) t :json-false))
                         (queued . ,(plist-get s :queued))))))
                 (+agent-pool-buffers nil nil 'all)))))

(defun +noema-agent-bridge--identity (request)
  "Resolve the legacy identity triple to the public API's exact buffer/ID."
  (let* ((workspace (directory-file-name
                     (remote-canonicalize-emacs-file-name (alist-get 'workspace request))))
         (row (seq-find
               (lambda (s) (and (equal (alist-get 'sessionId request) (alist-get 'sessionId s))
                                (equal (alist-get 'agent request) (alist-get 'agent s))
                                (equal workspace (alist-get 'workspace s))))
               (+noema-agent-bridge--sessions))))
    (unless row (user-error "No live matching ACP session; list sessions again"))
    `((buffer . ,(alist-get 'buffer row)) (nativeSessionId . ,(alist-get 'sessionId row)))))

(defun +noema-agent-bridge--dispatch (request)
  "Translate REQUEST; all execution uses the shared Emacs API."
  (require 'tools/agent/api)
  (pcase (alist-get 'action request)
    ("list" `((ok . t) (sessions . ,(+noema-agent-bridge--sessions))))
    ("api" (+agent/api (alist-get 'body request)))
    ((or "send" "read" "interrupt")
     (let ((identity (+noema-agent-bridge--identity request)))
       (pcase (alist-get 'action request)
         ("send"
          (let ((buffer (get-buffer (alist-get 'buffer identity))))
            (when (and (+agent-acp-busy-p buffer)
                       (not (buffer-local-value 'shell-maker--busy buffer))
                       (not (buffer-local-value '+agent-requests--current buffer)))
              (user-error "Noema Run is active; retry when it finishes"))
            (let ((reply (+agent/api `((action . "send") (prompt . ,(alist-get 'message request)) ,@identity))))
              (when (equal (alist-get 'state reply) "running") (setf (alist-get 'state reply) "submitted"))
              reply)))
         ("interrupt" (append '((state . "interrupt-requested")) (+agent/api `((action . "stop") ,@identity))))
         ("read"
          (let ((record (+agent-requests-get (alist-get 'requestId request))))
            (unless (eq (plist-get record :buffer) (get-buffer (alist-get 'buffer identity)))
              (user-error "Request belongs to another Agent session"))
            ;; The legacy CLI has no pagination arguments: retain its full,
            ;; bounded receipt while the shared API continues to page results.
            (let* ((body `((action . "result") (requestId . ,(alist-get 'requestId request)) (limit . 32000)))
                   (reply (+agent/api body)) (page reply) (chunks (list (alist-get 'text reply))))
              (while (alist-get 'nextOffset page)
                (setq page (+agent/api `((offset . ,(alist-get 'nextOffset page)) ,@body)))
                (push (alist-get 'text page) chunks))
              (setf (alist-get 'text reply) (apply #'concat (nreverse chunks))
                    (alist-get 'nextOffset reply) nil)
              reply))))))
    (_ (user-error "Unknown Agent bridge action"))))

(defun +noema-agent-bridge-request (encoded-json)
  "Decode data and route it to the one Emacs Agent runtime."
  (condition-case err
      (progn
        (unless +noema-agent-bridge-enabled
          (user-error "Agent bridge is disabled; use the Noema API or enable agent-bridge-start"))
        (require 'remote-gateway)
        (remote-gateway--encode
         (+noema-agent-bridge--dispatch
          (json-parse-string (decode-coding-string (base64-decode-string encoded-json) 'utf-8)
                             :object-type 'alist :array-type 'list :false-object :json-false :null-object nil))))
    (error (json-serialize `((ok . :false) (error . ,(error-message-string err)))))))

(provide 'noema/bridge/agent-bridge)
;;; agent-bridge.el ends here
