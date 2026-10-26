;;; tools/lsp/change-batch.el --- Bound remote incremental LSP writes -*- lexical-binding: t; -*-

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

(defcustom +lsp-remote-change-batch t
  "Batch remote incremental LSP changes for a short bounded interval."
  :type 'boolean
  :group '+language-server)

(defcustom +lsp-remote-change-batch-delay 0.03
  "Maximum seconds to hold the first remote incremental change."
  :type 'number
  :group '+language-server)

(defcustom +lsp-remote-change-batch-max-events 64
  "Maximum queued change events before an immediate flush."
  :type 'integer
  :group '+language-server)

(defvar +lsp-remote-change--queues
  (make-hash-table :test #'eq :weakness 'key)
  "Pending didChange notifications keyed by lsp-mode workspace.")

(defvar +lsp-remote-change--flushing nil
  "Non-nil while replaying queued changes through lsp-mode.")

(defvar-local +lsp-remote-change--eligible nil
  "Non-nil if writes to this managed buffer's server are relayed.")

(defun +lsp-remote-change--managed-h ()
  "Update remote-change eligibility when lsp-mode manages this buffer."
  (setq +lsp-remote-change--eligible
        (and (bound-and-true-p lsp-managed-mode)
             buffer-file-name
             (eq (ignore-errors
                   (remote-process-write-cost buffer-file-name))
                 'queued))))

(defun +lsp-remote-change--clear (workspace)
  "Cancel and forget WORKSPACE's queued changes."
  (when-let* ((state (gethash workspace +lsp-remote-change--queues)))
    (when-let* ((timer (plist-get state :timer)))
      (cancel-timer timer))
    (remhash workspace +lsp-remote-change--queues)))

(defun +lsp-remote-change--flush (workspace)
  "Send WORKSPACE's queued changes in document order."
  (when-let* ((state (gethash workspace +lsp-remote-change--queues)))
    (+lsp-remote-change--clear workspace)
    (when (and (eq (plist-get state :process)
                   (ignore-errors (lsp--workspace-proc workspace)))
               (process-live-p (plist-get state :process)))
      (let ((lsp--cur-workspace workspace)
            (+lsp-remote-change--flushing t)
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
               (let ((new (gethash workspace +lsp-remote-change--queues)))
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
                    +lsp-remote-change--queues)))
               (signal (car error-data) (cdr error-data))))))))))

(defun +lsp-remote-change--flush-timer (workspace)
  "Flush WORKSPACE from a timer without interrupting the command loop."
  (condition-case error-data
      (+lsp-remote-change--flush workspace)
    (error
     (remote-log 'lsp-change-batch-flush-error
                 :error (error-message-string error-data)))))

(defun +lsp-remote-change--queue (workspace process params)
  "Queue incremental PARAMS for WORKSPACE and PROCESS."
  (let ((current (gethash workspace +lsp-remote-change--queues)))
    (when (and current
               (not (eq process (plist-get current :process))))
      (+lsp-remote-change--clear workspace)))
  (let* ((state (or (gethash workspace +lsp-remote-change--queues)
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
              (if (and (numberp +lsp-remote-change-batch-delay)
                       (> +lsp-remote-change-batch-delay 0))
                  +lsp-remote-change-batch-delay
                0.03)
              nil #'+lsp-remote-change--flush-timer workspace))))
    (puthash workspace state +lsp-remote-change--queues)
    (when (>= (plist-get state :count)
              (if (and (integerp +lsp-remote-change-batch-max-events)
                       (> +lsp-remote-change-batch-max-events 0))
                  +lsp-remote-change-batch-max-events
                64))
      (+lsp-remote-change--flush workspace))))

(defun +lsp-remote-change--flush-matching (workspace process)
  "Flush queued changes owned by WORKSPACE or PROCESS."
  (when (> (hash-table-count +lsp-remote-change--queues) 0)
    (let (workspaces)
      (maphash
       (lambda (candidate state)
         (when (or (and workspace (eq candidate workspace))
                   (and process (eq process (plist-get state :process))))
           (push candidate workspaces)))
       +lsp-remote-change--queues)
      (dolist (candidate workspaces)
        (+lsp-remote-change--flush candidate)))))

(defun +lsp-remote-change--flush-current-buffer ()
  "Flush queued changes for the current buffer's LSP workspaces."
  (when (and (> (hash-table-count +lsp-remote-change--queues) 0)
             (fboundp 'lsp-workspaces))
    (dolist (workspace (ignore-errors (lsp-workspaces)))
      (+lsp-remote-change--flush workspace))))

(defun +lsp-remote-change--notify-a (original method params)
  "Queue eligible didChange METHOD and PARAMS; otherwise call ORIGINAL."
  (if (and +lsp-remote-change-batch
           +lsp-remote-change--eligible
           (not +lsp-remote-change--flushing)
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
            (+lsp-remote-change--queue workspace process params)
          (+lsp-remote-change--flush-matching workspace process)
          (funcall original method params)))
    (unless +lsp-remote-change--flushing
      (+lsp-remote-change--flush-matching
       (and (boundp 'lsp--cur-workspace) lsp--cur-workspace) nil))
    (funcall original method params)))

(defun +lsp-remote-change--request-a (original &rest arguments)
  "Flush changes before ORIGINAL sends a request with ARGUMENTS."
  (unless +lsp-remote-change--flushing
    (+lsp-remote-change--flush-current-buffer))
  (apply original arguments))

(defun +lsp-remote-change--send-a (original &rest arguments)
  "Flush pending changes before forwarding ARGUMENTS to ORIGINAL."
  (unless +lsp-remote-change--flushing
    (+lsp-remote-change--flush-matching
     (and (boundp 'lsp--cur-workspace) lsp--cur-workspace)
     (cadr arguments)))
  (apply original arguments))

(with-eval-after-load 'lsp-mode
  (when (and (fboundp 'lsp-notify)
             (fboundp 'lsp--send-no-wait)
             (fboundp 'lsp--workspace-proc)
             (fboundp 'lsp--workspace-sync-method))
    (advice-add 'lsp-notify :around #'+lsp-remote-change--notify-a)
    (advice-add 'lsp--send-no-wait :around #'+lsp-remote-change--send-a)
    (when (fboundp 'lsp-request)
      (advice-add 'lsp-request :around
                  #'+lsp-remote-change--request-a))
    (when (fboundp 'lsp-request-async)
      (advice-add 'lsp-request-async :around
                  #'+lsp-remote-change--request-a))))

(add-hook 'lsp-managed-mode-hook #'+lsp-remote-change--managed-h)
(add-hook 'lsp-after-uninitialized-functions #'+lsp-remote-change--clear)

(provide 'tools/lsp/change-batch)
;;; tools/lsp/change-batch.el ends here
