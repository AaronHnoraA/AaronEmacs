;;; noema-agent-bridge.el --- Local Codex Remote to Noema ACP bridge -*- lexical-binding: t; -*-

;; A separate Codex Remote task may ask this Emacs instance to send a turn to
;; one of its existing ACP clients.  The bridge never resumes a Codex task or
;; starts an ACP process.  Emacs's own local server socket is the transport.

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'server)
(require 'subr-x)

(defvar agent-shell--state)
(defvar noema-agent-acp-session-name)
(defvar shell-maker--busy)

(declare-function my/agent-shell--session-context "init-ai-ide" (directory))
(declare-function my/agent-shell--find-execution "init-ai-ide"
                  (session-id config directory &optional exclude))
(declare-function my/agent-shell-execution-live-p "init-ai-ide" (buffer))
(declare-function noema-agent-acp-busy-p "noema-agent-acp" (buffer))
(declare-function noema-agent-acp-pending-prompt-count "noema-agent-acp" (buffer))
(declare-function noema-agent-acp-enqueue "noema-agent-acp" (buffer text))
(declare-function noema-agent-acp-prompt "noema-agent-acp" (&rest args))
(declare-function noema-agent-acp-stop "noema-agent-acp" (&optional buffer))
(declare-function agent-shell-subscribe-to "agent-shell" (&rest args))

(defvar my/noema-agent-bridge-enabled nil
  "Non-nil when the local Emacs server may route explicit bridge requests.")

(defvar my/noema-agent-bridge--serial 0)
(defvar-local my/noema-agent-bridge--subscription nil)
(defvar-local my/noema-agent-bridge--records nil)
(defvar-local my/noema-agent-bridge--current nil)

(defun my/noema-agent-bridge--finish (record status)
  "Mark RECORD with STATUS and stop assigning further output to it."
  (when record
    (setf (plist-get record :status) status)
    (when (eq my/noema-agent-bridge--current record)
      (setq my/noema-agent-bridge--current nil))))

(defun my/noema-agent-bridge--event (event)
  "Collect ACP reply EVENT before Noema's optional hidden rendering."
  (pcase (map-elt event :event)
    ('input-submitted
     (let* ((prompt (map-elt (map-elt event :data) :prompt))
            (record (seq-find
                     (lambda (item)
                       (and (eq (plist-get item :status) 'queued)
                            (equal (plist-get item :message) prompt)))
                     (reverse my/noema-agent-bridge--records))))
       (when record
         (setf (plist-get record :status) 'running)
         (setq my/noema-agent-bridge--current record))))
    ('agent-message-chunk
     (when-let* ((record my/noema-agent-bridge--current)
                 (chunk (map-elt (map-elt event :data) :text-chunk)))
       (let ((text (concat (plist-get record :text) chunk)))
         (when (> (length text) 100000)
           (setq text (substring text (- (length text) 100000)))
           (setf (plist-get record :truncated) t))
         (setf (plist-get record :text) text))))
    ('turn-complete
     (my/noema-agent-bridge--finish my/noema-agent-bridge--current 'completed))
    ('error
     (my/noema-agent-bridge--finish my/noema-agent-bridge--current 'failed))
    ('clean-up
     (dolist (record my/noema-agent-bridge--records)
       (when (memq (plist-get record :status) '(queued running))
         (setf (plist-get record :status) 'failed)))
     (setq my/noema-agent-bridge--current nil))))

(defun my/noema-agent-bridge--watch (buffer)
  "Subscribe once to BUFFER's ACP events for bridge reply collection."
  (with-current-buffer buffer
    (unless my/noema-agent-bridge--subscription
      (setq-local my/noema-agent-bridge--subscription
                  (agent-shell-subscribe-to
                   :shell-buffer buffer :on-event #'my/noema-agent-bridge--event)))))

(defun my/noema-agent-bridge-start ()
  "Enable local-socket access to live Noema ACP sessions for Codex Remote."
  (interactive)
  (when server-use-tcp
    (user-error "Noema agent bridge requires a local Emacs server socket"))
  (unless (server-running-p)
    (server-start))
  (setq my/noema-agent-bridge-enabled t)
  (message "Noema agent bridge enabled on the local Emacs server socket"))

(defun my/noema-agent-bridge-stop ()
  "Disable bridge requests without stopping another Emacs server user."
  (interactive)
  (setq my/noema-agent-bridge-enabled nil)
  (message "Noema agent bridge disabled"))

(defun my/noema-agent-bridge--sessions ()
  "Return live initialized ACP executions in this Emacs as JSON records."
  (vconcat
   (seq-keep
    (lambda (buffer)
      (when (my/agent-shell-execution-live-p buffer)
        (with-current-buffer buffer
          (when-let* (((eq (map-nested-elt agent-shell--state
                                          '(:agent-config :identifier))
                           'codex))
                      (session-id (map-nested-elt agent-shell--state
                                                  '(:session :id)))
                      (agent (map-nested-elt agent-shell--state
                                             '(:agent-config :identifier))))
            `((sessionId . ,session-id)
              (agent . ,(symbol-name agent))
              (workspace . ,(my/agent-shell--session-context default-directory))
              (name . ,(or (and (boundp 'noema-agent-acp-session-name)
                                noema-agent-acp-session-name)
                           :null))
              (busy . ,(if (noema-agent-acp-busy-p buffer) t :false))
              (queued . ,(noema-agent-acp-pending-prompt-count buffer)))))))
    (buffer-list))))

(defun my/noema-agent-bridge--target (request)
  "Resolve REQUEST to one live ACP execution, without starting a process."
  (let* ((session-id (alist-get 'sessionId request))
         (agent-name (alist-get 'agent request))
         (workspace (alist-get 'workspace request))
         (agent (and (stringp agent-name) (intern-soft agent-name))))
    (unless (and (stringp session-id) (not (string-empty-p session-id))
                 (stringp workspace) (not (string-empty-p workspace))
                 (eq agent 'codex))
      (user-error "A Codex sessionId, agent and workspace are required"))
    (or (my/agent-shell--find-execution
         session-id `((:identifier . ,agent)) workspace)
        (user-error "No live matching ACP session; list sessions again"))))

(defun my/noema-agent-bridge--send (request)
  "Deliver REQUEST as the next turn to its exact live ACP target."
  (let* ((buffer (my/noema-agent-bridge--target request))
         (message-text (alist-get 'message request)))
    (unless (and (stringp message-text)
                 (not (string-empty-p (string-trim message-text))))
      (user-error "A nonempty message is required"))
    (unless (my/agent-shell-execution-live-p buffer)
      (user-error "ACP session exited before the message was sent"))
    (my/noema-agent-bridge--watch buffer)
    (with-current-buffer buffer
      ;; Noema's structured Run prompts do not use shell-maker's turn-end
      ;; queue drain.  Do not promise automatic delivery behind one.
      (when (and (noema-agent-acp-busy-p buffer)
                 (not (bound-and-true-p shell-maker--busy)))
        (user-error "Noema Run is active; retry when it finishes"))
      (when (and (map-elt agent-shell--state :active-requests)
                 (not (noema-agent-acp-busy-p buffer)))
        (user-error "ACP control request in progress; retry after it settles"))
      (when (>= (length my/noema-agent-bridge--records) 100)
        (if-let* ((oldest-finished
                   (seq-find (lambda (item)
                               (memq (plist-get item :status) '(completed failed)))
                             (reverse my/noema-agent-bridge--records))))
            (setq my/noema-agent-bridge--records
                  (delq oldest-finished my/noema-agent-bridge--records))
          (user-error "Bridge has 100 unfinished requests; wait for one to finish")))
      (let* ((queued (or (noema-agent-acp-busy-p buffer)
                         (> (noema-agent-acp-pending-prompt-count buffer) 0)))
             (record (list :id (format "bridge-%d" (cl-incf my/noema-agent-bridge--serial))
                           :message message-text :status (if queued 'queued 'running)
                           :text "" :truncated nil)))
        (push record my/noema-agent-bridge--records)
        (condition-case err
            (if queued
                (noema-agent-acp-enqueue buffer message-text)
              (setq my/noema-agent-bridge--current record)
              (noema-agent-acp-prompt
               :buffer buffer
               :content (list `((type . "text") (text . ,message-text)))
               :on-success (lambda (_response)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (my/noema-agent-bridge--finish record 'completed))))
               :on-failure (lambda (_error _raw)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (my/noema-agent-bridge--finish record 'failed))))))
          (error
           (setq my/noema-agent-bridge--records
                 (delq record my/noema-agent-bridge--records))
           (my/noema-agent-bridge--finish record 'failed)
           (signal (car err) (cdr err))))
        `((ok . t) (requestId . ,(plist-get record :id))
          (state . ,(if queued "queued" "submitted")))))))

(defun my/noema-agent-bridge--read (request)
  "Read only the ACP reply associated with REQUEST's bridge request ID."
  (let* ((buffer (my/noema-agent-bridge--target request))
         (request-id (alist-get 'requestId request))
         (record (and (stringp request-id)
                      (with-current-buffer buffer
                        (seq-find (lambda (item)
                                    (equal (plist-get item :id) request-id))
                                  my/noema-agent-bridge--records)))))
    (unless record
      (user-error "Unknown bridge request ID in this ACP session"))
    `((ok . t)
      (requestId . ,request-id)
      (state . ,(symbol-name (plist-get record :status)))
      (text . ,(plist-get record :text))
      (truncated . ,(if (plist-get record :truncated) t :false)))))

(defun my/noema-agent-bridge--interrupt (request)
  "Explicitly interrupt REQUEST's active turn without terminating its client."
  (let ((buffer (my/noema-agent-bridge--target request)))
    (unless (noema-agent-acp-busy-p buffer)
      (user-error "The selected ACP session is idle"))
    (noema-agent-acp-stop buffer)
    '((ok . t) (state . "interrupt-requested"))))

(defun my/noema-agent-bridge-request (encoded-json)
  "Handle one base64 ENCODED-JSON request from a local emacsclient.
  Return a JSON string.  Only list, send, read, and interrupt are exposed."
  (condition-case err
      (progn
        (unless my/noema-agent-bridge-enabled
          (user-error "Noema agent bridge is disabled; run M-x my/noema-agent-bridge-start"))
        (require 'noema-agent-acp)
        (let* ((request (json-parse-string
                         (decode-coding-string
                          (base64-decode-string encoded-json) 'utf-8)
                         :object-type 'alist))
               (action (alist-get 'action request)))
          (json-serialize
           (pcase action
             ("list" `((ok . t) (sessions . ,(my/noema-agent-bridge--sessions))))
             ("send" (my/noema-agent-bridge--send request))
             ("read" (my/noema-agent-bridge--read request))
             ("interrupt" (my/noema-agent-bridge--interrupt request))
             (_ (user-error "Unknown Noema bridge action"))))))
    (error
     (json-serialize `((ok . :false)
                       (error . ,(error-message-string err)))))))

(provide 'noema-agent-bridge)
;;; noema-agent-bridge.el ends here
