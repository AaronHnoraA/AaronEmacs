;;; remote-background.el --- Reentrant-safe remote background work -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Timers may run while Tramp is waiting for process output.  This module
;; gives background discovery and recovery one coalesced, generation-aware
;; boundary instead of letting each consumer invent an unsafe timer retry.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)
(require 'remote-compat)

(cl-defstruct (remote-background-job
               (:constructor remote-background-job-create))
  id key target-id epoch function callback error-callback
  delays attempts timer state owner-buffer non-essential waiters)

(defcustom remote-background-retry-delays '(0.1 0.3 0.8 1.5)
  "Default bounded delays for reentrant remote background operations."
  :type '(repeat number)
  :group 'remote)

(defcustom remote-background-retry-jitter 0.15
  "Fractional random jitter applied to background retry delays."
  :type 'number
  :group 'remote)

(defvar remote-background-jobs (make-hash-table :test #'equal)
  "Pending background jobs keyed by logical owner and purpose.")

(defvar remote-background-target-epochs (make-hash-table :test #'equal)
  "Monotonic invalidation epoch for each target.")

(defvar remote-background--counter 0)

(defvar remote-background-defer-commit nil
  "Non-nil while background work must return observations without caching.")

(defun remote-background-target-epoch (target-id)
  "Return the current background observation epoch for TARGET-ID."
  (gethash (remote-normalize-id target-id)
           remote-background-target-epochs 0))

(defun remote-background-invalidate-target (target-id)
  "Advance TARGET-ID's observation epoch and return it."
  (let* ((target-id (remote-normalize-id target-id))
         (epoch (1+ (remote-background-target-epoch target-id))))
    (puthash target-id epoch remote-background-target-epochs)
    epoch))

(defun remote-background--transient-error-p (error)
  "Return non-nil when ERROR means background work should yield and retry."
  (seq-some
   (lambda (type)
     (remote-compat-error-has-type-p error type))
   '(remote-file-error remote-connection-busy
     remote-connection-cancelled remote-pipeline-cancelled
     remote-transport-error remote-connection-timeout)))

(defun remote-background--delay (delay)
  "Return DELAY with bounded random jitter."
  (let* ((fraction (max 0 remote-background-retry-jitter))
         (span (* delay fraction))
         (offset (if (zerop span)
                     0
                   (- (* 2 span (/ (float (random 1000000)) 1000000.0))
                      span))))
    (max 0 (+ delay offset))))

(defun remote-background--finish (job state &optional value)
  "Finish JOB in STATE and deliver VALUE to its owner."
  (setf (remote-background-job-state job) state
        (remote-background-job-timer job) nil)
  (when (eq (gethash (remote-background-job-key job)
                     remote-background-jobs)
            job)
    (remhash (remote-background-job-key job) remote-background-jobs))
  (remote-background--deliver job state value)
  job)

(defun remote-background--deliver (job state value)
  "Deliver JOB VALUE for terminal STATE while containing callback errors."
  (dolist (waiter (remote-background-job-waiters job))
    (pcase-let ((`(,buffer ,success ,failure) waiter))
      (when (or (null buffer) (buffer-live-p buffer))
        (condition-case error
            (let ((callback (if (eq state 'complete) success failure)))
              (when callback
                (if buffer
                    (with-current-buffer buffer
                      (funcall callback value))
                  (funcall callback value))))
          (error
           (remote-log
            'background-callback-error
            :key (remote-background-job-key job)
            :target (remote-background-job-target-id job)
            :error (error-message-string error))))))))

(defun remote-background--schedule (job delay)
  "Schedule JOB after DELAY seconds."
  (setf (remote-background-job-state job) 'waiting
        (remote-background-job-timer job)
        (run-at-time
         (remote-background--delay delay) nil
         #'remote-background--run job))
  job)

(defun remote-background--retry (job error)
  "Retry JOB after transient ERROR, or finish it as failed."
  (if-let* ((delays (remote-background-job-delays job)))
      (progn
        (setf (remote-background-job-delays job) (cdr delays)
              (remote-background-job-attempts job)
              (1+ (remote-background-job-attempts job))
              (remote-background-job-epoch job)
              (remote-background-target-epoch
               (remote-background-job-target-id job)))
        (remote-log
         'background-retry
         :key (remote-background-job-key job)
         :target (remote-background-job-target-id job)
         :attempt (remote-background-job-attempts job)
         :error (error-message-string error))
        (remote-background--schedule job (car delays)))
    (remote-background--finish job 'failed error)))

(defun remote-background--run (job)
  "Execute one attempt of JOB."
  (when (and (remote-background-job-p job)
             (eq (gethash (remote-background-job-key job)
                          remote-background-jobs)
                 job)
             (memq (remote-background-job-state job) '(waiting running)))
    (setf (remote-background-job-state job) 'running
          (remote-background-job-timer job) nil)
    (condition-case error
        (let* ((non-essential
                (remote-background-job-non-essential job))
               (remote-background-defer-commit t)
               (value (funcall (remote-background-job-function job)))
               (current-epoch
                (remote-background-target-epoch
                 (remote-background-job-target-id job))))
          (cond
           ((not (eq (gethash (remote-background-job-key job)
                              remote-background-jobs)
                     job))
            job)
           ((= current-epoch (remote-background-job-epoch job))
            (remote-background--finish job 'complete value))
           (t
            ;; A session was invalidated while the function yielded.  Never
            ;; publish that observation; repeat it against the new epoch.
            (remote-background--retry
             job '(remote-connection-cancelled
                   "Target generation changed during background work")))))
      (quit
       (remote-background-cancel
        (remote-background-job-key job) 'quit))
      (error
       (if (not (eq (gethash (remote-background-job-key job)
                             remote-background-jobs)
                    job))
           job
         (if (remote-background--transient-error-p error)
           (remote-background--retry job error)
           (remote-background--finish job 'failed error)))))))

(cl-defun remote-background-submit
    (key function &key target-id callback error-callback delays
         replace ((:non-essential avoid-connection) t)
         (owner-buffer (current-buffer)))
  "Submit coalesced background FUNCTION under KEY.
TARGET-ID supplies the invalidation epoch.  CALLBACK receives the result;
ERROR-CALLBACK receives the final error after bounded retries.  Unless REPLACE
is non-nil, an existing pending job for KEY is returned."
  (unless (functionp function)
    (error "Remote background function is not callable: %S" function))
  (let* ((target-id (remote-normalize-id target-id))
         (existing (gethash key remote-background-jobs)))
    (if (and existing (not replace)
             (memq (remote-background-job-state existing)
                   '(waiting running)))
        (progn
          (when (or callback error-callback)
            (setf (remote-background-job-waiters existing)
                  (append
                   (remote-background-job-waiters existing)
                   (list (list owner-buffer callback error-callback)))))
          existing)
      (when existing
        (remote-background-cancel key 'replaced))
      (let ((job
             (remote-background-job-create
              :id (format "background-%d"
                          (cl-incf remote-background--counter))
              :key key :target-id target-id
              :epoch (remote-background-target-epoch target-id)
              :function function :callback callback
              :error-callback error-callback
              :delays (copy-sequence
                       (or delays remote-background-retry-delays))
              :attempts 0 :state 'waiting
              :owner-buffer owner-buffer
              :non-essential avoid-connection
              :waiters
              (and (or callback error-callback)
                   (list (list owner-buffer callback error-callback))))))
        (puthash key job remote-background-jobs)
        (remote-background--schedule job 0)))))

(defun remote-background-cancel (key &optional reason)
  "Cancel pending background job KEY because of REASON."
  (when-let* ((job (gethash key remote-background-jobs)))
    (when-let* ((timer (remote-background-job-timer job)))
      (when (timerp timer)
        (cancel-timer timer)))
    (remhash key remote-background-jobs)
    (setf (remote-background-job-state job) 'cancelled
          (remote-background-job-timer job) nil)
    (remote-log 'background-cancel :key key :reason reason)
    job))

(defun remote-background-job-list (&optional target-id)
  "Return stable summaries of pending jobs, optionally for TARGET-ID."
  (let (result)
    (maphash
     (lambda (_key job)
       (when (or (null target-id)
                 (equal (remote-background-job-target-id job) target-id))
         (push
          (list :id (remote-background-job-id job)
                :key (remote-background-job-key job)
                :target (remote-background-job-target-id job)
                :epoch (remote-background-job-epoch job)
                :state (remote-background-job-state job)
                :attempts (remote-background-job-attempts job))
          result)))
     remote-background-jobs)
    (sort result
          (lambda (left right)
            (string-lessp (plist-get left :id)
                          (plist-get right :id))))))

(defun remote-background-clear (&optional reason)
  "Cancel all pending remote background jobs because of REASON."
  (let ((keys (hash-table-keys remote-background-jobs)))
    (dolist (key keys)
      (remote-background-cancel key (or reason 'clear)))
    (length keys)))

(provide 'remote-background)
;;; remote-background.el ends here
