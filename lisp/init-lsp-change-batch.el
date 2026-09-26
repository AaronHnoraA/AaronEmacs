;;; init-lsp-change-batch.el --- Bound remote incremental LSP writes -*- lexical-binding: t; -*-

;;; Commentary:
;; lsp-mode builds incremental didChange events on each edit.  Sending each
;; event through tramp-rpc's reliable process queue adds roughly 0.4 ms to
;; every remote keypress.  Combine adjacent events for the same document and
;; send them after a short timer or before the next outgoing LSP message.
;; Each event retains its original range and order; only the notification's
;; final document version is replaced.  The queue never crosses a process
;; generation, and unknown lsp-mode message shapes use the ordinary path.

;;; Code:

(require 'cl-lib)
(require 'remote-fs)

(declare-function lsp--workspace-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-sync-method "lsp-mode" (workspace))
(declare-function lsp-notify "lsp-mode" (method params))
(declare-function lsp-workspaces "lsp-mode" ())
(defvar lsp--cur-workspace)
(defvar lsp-managed-mode)

(defcustom my/lsp-remote-change-batch t
  "Batch remote incremental LSP changes for a short bounded interval."
  :type 'boolean
  :group 'my/language-server)

(defcustom my/lsp-remote-change-batch-delay 0.03
  "Maximum seconds to hold the first remote incremental change."
  :type 'number
  :group 'my/language-server)

(defcustom my/lsp-remote-change-batch-max-events 64
  "Maximum queued change events before an immediate flush."
  :type 'integer
  :group 'my/language-server)

(defvar my/lsp-remote-change--queues
  (make-hash-table :test #'eq :weakness 'key)
  "Pending didChange notifications keyed by lsp-mode workspace.")

(defvar my/lsp-remote-change--flushing nil
  "Non-nil while replaying queued changes through lsp-mode.")

(defvar-local my/lsp-remote-change--eligible nil
  "Non-nil if this managed source buffer belongs to a nonlocal target.")

(defun my/lsp-remote-change--managed-h ()
  "Update remote-change eligibility when lsp-mode manages this buffer."
  (setq my/lsp-remote-change--eligible
        (and (bound-and-true-p lsp-managed-mode)
             buffer-file-name
             (let ((context (ignore-errors (remote-context buffer-file-name))))
               (and (remote-context-p context)
                    (not (equal (remote-context-target-id context)
                                "local")))))))

(defun my/lsp-remote-change--clear (workspace)
  "Cancel and forget WORKSPACE's queued changes."
  (when-let* ((state (gethash workspace my/lsp-remote-change--queues)))
    (when-let* ((timer (plist-get state :timer)))
      (cancel-timer timer))
    (remhash workspace my/lsp-remote-change--queues)))

(defun my/lsp-remote-change--flush (workspace)
  "Send WORKSPACE's queued changes in document order."
  (when-let* ((state (gethash workspace my/lsp-remote-change--queues)))
    (my/lsp-remote-change--clear workspace)
    (when (and (eq (plist-get state :process)
                   (ignore-errors (lsp--workspace-proc workspace)))
               (process-live-p (plist-get state :process)))
      (let ((lsp--cur-workspace workspace)
            (my/lsp-remote-change--flushing t)
            (records (reverse (plist-get state :records))))
        (while records
          (let* ((record (car records))
                 (params (copy-sequence (plist-get record :params)))
                 (source (plist-get record :buffer)))
            (setq params
                  (plist-put params :contentChanges
                             (vconcat (reverse (plist-get record :events)))))
            (condition-case error-data
                (progn
                  (if (buffer-live-p source)
                      (with-current-buffer source
                        (lsp-notify "textDocument/didChange" params))
                    (lsp-notify "textDocument/didChange" params))
                  (setq records (cdr records)))
              (error
               ;; A synchronous transport refusal did not accept this record.
               ;; Keep it and all later records so the next request cannot
               ;; overtake them.  A new process generation drops this queue.
               (let ((new (gethash workspace my/lsp-remote-change--queues)))
                 (unless (and new
                              (not (eq (plist-get new :process)
                                       (plist-get state :process))))
                   (puthash
                    workspace
                    (list :process (plist-get state :process)
                          :records
                          (append (and new (plist-get new :records))
                                  (reverse records))
                          :count
                          (+ (or (and new (plist-get new :count)) 0)
                             (cl-loop for remaining in records
                                      sum (length
                                           (plist-get remaining :events))))
                          :timer (and new (plist-get new :timer)))
                    my/lsp-remote-change--queues)))
               (signal (car error-data) (cdr error-data))))))))))

(defun my/lsp-remote-change--flush-timer (workspace)
  "Flush WORKSPACE from a timer without interrupting the command loop."
  (condition-case error-data
      (my/lsp-remote-change--flush workspace)
    (error
     (remote-log 'lsp-change-batch-flush-error
                 :error (error-message-string error-data)))))

(defun my/lsp-remote-change--queue (workspace process params)
  "Queue incremental PARAMS for WORKSPACE and PROCESS."
  (let ((current (gethash workspace my/lsp-remote-change--queues)))
    (when (and current
               (not (eq process (plist-get current :process))))
      (my/lsp-remote-change--clear workspace)))
  (let* ((state (or (gethash workspace my/lsp-remote-change--queues)
                    (list :process process :records nil :count 0)))
         (document (plist-get params :textDocument))
         (uri (plist-get document :uri))
         (events (append (plist-get params :contentChanges) nil))
         (records (plist-get state :records))
         (latest (car records)))
    (if (and latest (equal uri (plist-get latest :uri)))
        (progn
          (setf (plist-get latest :params) params)
          (setf (plist-get latest :events)
                (nconc (nreverse events) (plist-get latest :events))))
      (push (list :uri uri :params params :events (nreverse events)
                  :buffer (current-buffer))
            records))
    (setq state (plist-put state :records records))
    (setq state (plist-put state :count
                           (+ (plist-get state :count)
                              (length (plist-get params :contentChanges)))))
    (unless (plist-get state :timer)
      (setq state
            (plist-put
             state :timer
             (run-at-time
              (if (and (numberp my/lsp-remote-change-batch-delay)
                       (> my/lsp-remote-change-batch-delay 0))
                  my/lsp-remote-change-batch-delay
                0.03)
              nil #'my/lsp-remote-change--flush-timer workspace))))
    (puthash workspace state my/lsp-remote-change--queues)
    (when (>= (plist-get state :count)
              (if (and (integerp my/lsp-remote-change-batch-max-events)
                       (> my/lsp-remote-change-batch-max-events 0))
                  my/lsp-remote-change-batch-max-events
                64))
      (my/lsp-remote-change--flush workspace))))

(defun my/lsp-remote-change--flush-matching (workspace process)
  "Flush queued changes owned by WORKSPACE or PROCESS."
  (when (> (hash-table-count my/lsp-remote-change--queues) 0)
    (let (workspaces)
      (maphash
       (lambda (candidate state)
         (when (or (and workspace (eq candidate workspace))
                   (and process (eq process (plist-get state :process))))
           (push candidate workspaces)))
       my/lsp-remote-change--queues)
      (dolist (candidate workspaces)
        (my/lsp-remote-change--flush candidate)))))

(defun my/lsp-remote-change--flush-current-buffer ()
  "Flush queued changes for the current buffer's LSP workspaces."
  (when (and (> (hash-table-count my/lsp-remote-change--queues) 0)
             (fboundp 'lsp-workspaces))
    (dolist (workspace (ignore-errors (lsp-workspaces)))
      (my/lsp-remote-change--flush workspace))))

(defun my/lsp-remote-change--notify-a (original method params)
  "Queue eligible didChange METHOD and PARAMS; otherwise call ORIGINAL."
  (if (and my/lsp-remote-change-batch
           my/lsp-remote-change--eligible
           (not my/lsp-remote-change--flushing)
           (equal method "textDocument/didChange"))
      (let* ((workspace (and (boundp 'lsp--cur-workspace)
                             lsp--cur-workspace))
             (document (and (listp params)
                            (plist-get params :textDocument)))
             (changes (and (listp params)
                           (plist-get params :contentChanges)))
             (process (and workspace
                           (ignore-errors (lsp--workspace-proc workspace)))))
        (if (and (listp document)
                 (stringp (plist-get document :uri))
                 (integerp (plist-get document :version))
                 (vectorp changes)
                 (> (length changes) 0)
                 (processp process)
                 (process-live-p process)
                 (eql (ignore-errors (lsp--workspace-sync-method workspace))
                      2))
            (my/lsp-remote-change--queue workspace process params)
          (my/lsp-remote-change--flush-matching workspace process)
          (funcall original method params)))
    (unless my/lsp-remote-change--flushing
      (my/lsp-remote-change--flush-matching
       (and (boundp 'lsp--cur-workspace) lsp--cur-workspace) nil))
    (funcall original method params)))

(defun my/lsp-remote-change--request-a (original &rest arguments)
  "Flush changes before ORIGINAL sends a request with ARGUMENTS."
  (unless my/lsp-remote-change--flushing
    (my/lsp-remote-change--flush-current-buffer))
  (apply original arguments))

(defun my/lsp-remote-change--send-a (original &rest arguments)
  "Flush pending changes before forwarding ARGUMENTS to ORIGINAL."
  (unless my/lsp-remote-change--flushing
    (my/lsp-remote-change--flush-matching
     (and (boundp 'lsp--cur-workspace) lsp--cur-workspace)
     (cadr arguments)))
  (apply original arguments))

(with-eval-after-load 'lsp-mode
  (when (and (fboundp 'lsp-notify)
             (fboundp 'lsp--send-no-wait)
             (fboundp 'lsp--workspace-proc)
             (fboundp 'lsp--workspace-sync-method))
    (advice-add 'lsp-notify :around #'my/lsp-remote-change--notify-a)
    (advice-add 'lsp--send-no-wait :around #'my/lsp-remote-change--send-a)
    (when (fboundp 'lsp-request)
      (advice-add 'lsp-request :around
                  #'my/lsp-remote-change--request-a))
    (when (fboundp 'lsp-request-async)
      (advice-add 'lsp-request-async :around
                  #'my/lsp-remote-change--request-a))))

(add-hook 'lsp-managed-mode-hook #'my/lsp-remote-change--managed-h)
(add-hook 'lsp-after-uninitialized-functions #'my/lsp-remote-change--clear)

(provide 'init-lsp-change-batch)
;;; init-lsp-change-batch.el ends here
