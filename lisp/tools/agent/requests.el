;;; requests.el --- Receipts for Emacs-owned ACP tasks -*- lexical-binding: t; -*-

;; Only bounded task receipts live here.  ACP owns processes and the one native
;; queue.  Request identity is a queue object's identity, never its prompt text.
(require 'cl-lib)
(require 'seq)
(require 'tools/agent/acp)
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))

(defvar +agent-requests--serial 0)
(defvar +agent-requests--records nil)
(defconst +agent-requests--capacity 128)
(defconst +agent-requests--text-limit 100000)
(defconst +agent-requests--message-file
  (expand-file-name "task-result.txt" (file-name-directory (or load-file-name buffer-file-name))))
(defvar-local +agent-requests--current nil)
(defvar-local +agent-requests--watch-state nil)

(defun +agent-requests--terminal-p (record)
  "Whether RECORD has ended, successfully or otherwise."
  (memq (plist-get record :status) '(completed cancelled failed interrupted untracked)))

(defun +agent-requests--own (buffer)
  "Return pending receipts belonging to physical BUFFER."
  (seq-filter (lambda (r) (and (eq buffer (plist-get r :buffer))
                              (not (+agent-requests--terminal-p r)))) +agent-requests--records))

(defun +agent-requests--queued-p (buffer prompt)
  "Does PROMPT identify a tracked queue item in BUFFER?"
  (seq-find (lambda (r) (and (memq (plist-get r :status) '(queued held))
                            (eq prompt (plist-get r :prompt)))) (+agent-requests--own buffer)))

(defun +agent-requests--notify (record)
  "Deliver RECORD's terminal result once to its exact requested parent."
  (setf (plist-get record :timer) nil)
  (let* ((parent (plist-get record :notify))
         (buffer (plist-get parent :buffer)))
    (condition-case err
        (progn
          (unless (and (buffer-live-p buffer)
                       (eq (plist-get parent :client) (+agent-acp-state-value buffer '(:client)))
                       (equal (plist-get parent :native) (+agent-acp-state-value buffer '(:session :id)))
                       (not (+agent-acp-client-exited-p buffer))
                       (not (buffer-local-value 'remote-buffer-disconnected-p buffer)))
            (user-error "Parent session is no longer attached; result remains readable"))
          (require 'tools/agent/context)
          (+agent-context--deliver
           buffer
           (list `((type . "text")
                   (text . ,(format (with-temp-buffer
                                      (insert-file-contents +agent-requests--message-file) (buffer-string))
                                    (plist-get record :id) (plist-get record :status)
                                    (or (plist-get record :reason) "")
                                    (if (or (plist-get record :truncated) (> (length (plist-get record :text)) 6000)) "true" "false")
                                    (substring (plist-get record :text) 0 (min 6000 (length (plist-get record :text)))))))) nil t)
          (setf (plist-get record :notification) "submitted"))
      (error (setf (plist-get record :notification) "failed"
                   (plist-get record :notification-error) (error-message-string err))))
    ;; The receipt must not retain the parent's client and transcript.
    (setf (plist-get record :notify) nil)))

(defun +agent-requests--finish (record status &optional reason)
  "Settle RECORD once.  Notifications do not run inside ACP callbacks."
  (unless (+agent-requests--terminal-p record)
    (setf (plist-get record :status) status (plist-get record :reason) reason
          (plist-get record :finished-at) (float-time)
          (plist-get record :state) nil (plist-get record :content) nil
          (plist-get record :prompt) nil)
    (when (buffer-live-p (plist-get record :buffer))
      (with-current-buffer (plist-get record :buffer)
        (when (eq record +agent-requests--current) (setq +agent-requests--current nil))))
    (when (plist-get record :notify)
      (setf (plist-get record :notification) "pending"
            (plist-get record :timer) (run-at-time 0 nil #'+agent-requests--notify record)))))

(defun +agent-requests--complete (record reason)
  "Interpret the provider's stop REASON for RECORD."
  (+agent-requests--finish record
                          (cond ((equal reason "end_turn") 'completed)
                                ((equal reason "cancelled") 'cancelled)
                                (t 'failed)) reason))

(defun +agent-requests--activate (record)
  "Bind RECORD to the current native turn before streaming begins."
  (setq +agent-requests--current record)
  (setf (plist-get record :status) 'running
        (plist-get record :native) (+agent-acp-state-value (current-buffer) '(:session :id))))

(defun +agent-requests--start (record)
  "Send RECORD's full content with callbacks tied to this request."
  (let ((buffer (plist-get record :buffer)))
    (with-current-buffer buffer
      (+agent-requests--activate record)
      (+agent-acp-prompt
       :buffer buffer :content (plist-get record :content) :run-id (plist-get record :id) :process-queue t
       :on-success (lambda (response) (+agent-requests--complete record (map-elt response 'stopReason)))
       :on-failure
       (lambda (error _raw)
         (let ((process (and (buffer-live-p buffer)
                             (+agent-acp-state-value buffer '(:client :process)))))
           (+agent-requests--finish record
                                   (if (and (processp process) (process-live-p process)) 'failed 'interrupted)
                                   (format "%s" error))))))))

(defun +agent-requests--dispatch-head (buffer text)
  "Take tracked TEXT from the native queue and send its original content."
  (when-let* ((record (+agent-requests--queued-p buffer text)))
    (+agent-acp-pop-prompt buffer text)
    (condition-case err (+agent-requests--start record)
      (error (+agent-requests--finish record 'failed (error-message-string err))
             (with-current-buffer buffer (+agent-acp--hold-queue))))
    t))

(defun +agent-requests--dispatch (record)
  "Submit RECORD once initialization has finished, using the native queue."
  (let ((buffer (plist-get record :buffer)))
    (with-current-buffer buffer
      (if (or (+agent-acp-busy-p buffer) (> (+agent-acp-pending-prompt-count buffer) 0))
          (progn
            (setf (plist-get record :status) 'queued)
            (+agent-acp-enqueue buffer (plist-get record :prompt)))
        (+agent-requests--start record)))))

(defun +agent-requests--event (event)
  "Observe only the current physical ACP state's EVENT."
  (pcase (map-elt event :event)
    ('init-finished
     (dolist (r (reverse (+agent-requests--own (current-buffer))))
       (when (eq (plist-get r :status) 'starting)
         (condition-case err (+agent-requests--dispatch r)
           (error (+agent-requests--finish r 'failed (error-message-string err)))))))
    ('agent-message-chunk
     (when-let* ((record +agent-requests--current)
                 (chunk (map-nested-elt event '(:data :text-chunk))))
       (let* ((text (plist-get record :text))
              (room (- +agent-requests--text-limit (length text))))
         (setf (plist-get record :text) (concat text (substring chunk 0 (min room (length chunk)))))
         (when (> (length chunk) room) (setf (plist-get record :truncated) t)))))
    ('error
     ;; Before a session exists, this is a failed handshake.  During a task,
     ;; only its own prompt callback may fail it: other RPCs can also fail.
     (unless (eq (+agent-acp-state-value (current-buffer) '(:init-finished)) t)
       (dolist (r (+agent-requests--own (current-buffer)))
         (when (eq (plist-get r :status) 'starting)
           (+agent-requests--finish r 'failed (map-nested-elt event '(:data :message)))))))
    ('clean-up (+agent-requests--ended (current-buffer)))))

(defun +agent-requests--ended (buffer)
  "Keep interrupted receipts readable after BUFFER's client ends."
  (dolist (r (+agent-requests--own buffer))
    (+agent-requests--finish r 'interrupted "client_ended")))

(defun +agent-requests--changed (buffer)
  "Reconcile withdrawn native queue items without requiring caller polling."
  (dolist (r (+agent-requests--own buffer))
    (when (memq (plist-get r :status) '(queued held))
      (+agent-requests-get (plist-get r :id)))))

(defun +agent-requests--killed-h ()
  "Settle outstanding work when its physical buffer is killed."
  (+agent-requests--ended (current-buffer)))

(defun +agent-requests--watch (buffer)
  "Observe BUFFER once per physical state, before sending any prompt."
  (with-current-buffer buffer
    (unless (eq +agent-requests--watch-state (+agent-acp-state-value buffer nil))
      (setq +agent-requests--watch-state (+agent-acp-state-value buffer nil))
      (+agent-acp-subscribe :buffer buffer :callback #'+agent-requests--event)
      (add-hook 'kill-buffer-hook #'+agent-requests--killed-h nil t)
      (add-hook 'change-major-mode-hook #'+agent-requests--killed-h nil t))))

(defun +agent-requests-check-capacity ()
  "Make room for a receipt, refusing before a new client is created."
  (when (>= (length +agent-requests--records) +agent-requests--capacity)
    (let ((oldest (seq-find (lambda (r) (and (+agent-requests--terminal-p r)
                                            (not (plist-get r :timer))))
                            (reverse +agent-requests--records))))
      (unless oldest (user-error "Too many unfinished Agent requests"))
      (setq +agent-requests--records (delq oldest +agent-requests--records)))))

(defun +agent-requests-submit (buffer content &optional notify)
  "Track CONTENT in BUFFER, optionally notifying an exact parent identity."
  (+agent-requests-check-capacity)
  (let* ((prompt (copy-sequence (map-elt (car content) 'text)))
         (snapshot (+agent-session-snapshot buffer))
         (scope (cl-loop for key in '(:buffer :name :agent :origin :root :target :workspace)
                         append (list key (plist-get snapshot key))))
         (record (list :id (format "agent-%s-%d" (emacs-pid) (cl-incf +agent-requests--serial))
                       :buffer buffer :state (+agent-acp-state-value buffer nil)
                       :scope scope :buffer-name (buffer-name buffer)
                       :created-at (float-time) :finished-at nil
                       :status 'starting :text "" :prompt prompt :content content :notify notify
                       :native (plist-get snapshot :native-session-id)
                       :reason nil :timer nil :notification nil :notification-error nil
                       :truncated nil)))
    (+agent-requests--watch buffer)
    (push record +agent-requests--records)
    (condition-case err
        (when (eq (+agent-acp-state-value buffer '(:init-finished)) t)
          (+agent-requests--dispatch record))
      (error (+agent-requests--finish record 'failed (error-message-string err))))
    record))

(defun +agent-requests-get (id)
  "Find ID and reconcile queue withdrawal or a replaced physical state."
  (let* ((r (or (seq-find (lambda (r) (equal id (plist-get r :id))) +agent-requests--records)
                (user-error "Unknown or expired Agent request: %s" id)))
         (buffer (plist-get r :buffer)))
    (unless (+agent-requests--terminal-p r)
      (cond
       ((or (not (buffer-live-p buffer))
            (not (eq (plist-get r :state) (+agent-acp-state-value buffer nil))))
        (+agent-requests--finish r 'interrupted "client_replaced"))
       ((memq (plist-get r :status) '(queued held))
        (let ((prompt (plist-get r :prompt)))
          (cond ((memq prompt (buffer-local-value '+agent-acp--held-prompts buffer))
                 (setf (plist-get r :status) 'held))
                ((memq prompt (+agent-acp-state-value buffer '(:pending-prompts)))
                 (setf (plist-get r :status) 'queued))
                (t (+agent-requests--finish r 'untracked "queue_changed")))))))
    r))

(defun +agent-requests-summary (record)
  "Return task identity and lifecycle facts, with no prompt or reply text."
  (let* ((r (+agent-requests-get (plist-get record :id)))
         (buffer (plist-get r :buffer))
         (name (if (buffer-live-p buffer) (buffer-name buffer) (plist-get r :buffer-name))))
    `((requestId . ,(plist-get r :id)) (state . ,(symbol-name (plist-get r :status)))
      (terminal . ,(if (+agent-requests--terminal-p r) t :json-false))
      (createdAt . ,(plist-get r :created-at)) (finishedAt . ,(plist-get r :finished-at))
      (notification . ,(plist-get r :notification))
      (session . ((buffer . ,name) (nativeSessionId . ,(plist-get r :native))
                  ,@(mapcar (lambda (field)
                              (let ((value (plist-get (plist-get r :scope) (cdr field))))
                                (cons (car field) (if (and value (symbolp value)) (symbol-name value) value))))
                            '((name . :name) (agent . :agent) (root . :root)
                              (target . :target) (workspace . :workspace))))))))

(defun +agent-requests-result (id offset limit)
  "Return the bounded reply for ID, excluding other tasks and user drafts."
  (unless (and (natnump offset) (integerp limit) (<= 1 limit 32000))
    (user-error "offset must be >= 0 and limit must be 1..32000"))
  (let* ((r (+agent-requests-get id)) (text (plist-get r :text))
         (total (length text)) (end (min total (+ offset limit)))
         (buffer (plist-get r :buffer)))
    `((ok . t) ,@(+agent-requests-summary r)
      (buffer . ,(and (buffer-live-p buffer) (buffer-name buffer)))
      (nativeSessionId . ,(plist-get r :native)) (stopReason . ,(plist-get r :reason))
      (text . ,(substring text (min offset total) end)) (total . ,total)
      (nextOffset . ,(when (< end total) end))
      (truncated . ,(if (plist-get r :truncated) t :json-false))
      (notificationError . ,(plist-get r :notification-error)))))

(add-hook '+agent-acp-separate-prompt-functions #'+agent-requests--queued-p)
(add-hook '+agent-acp-dispatch-prompt-functions #'+agent-requests--dispatch-head)
(add-hook '+agent-acp-client-ended-functions #'+agent-requests--ended)
(add-hook '+agent-acp-changed-functions #'+agent-requests--changed)
(provide 'tools/agent/requests)
;;; requests.el ends here
