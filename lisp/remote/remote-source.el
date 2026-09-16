;;; remote-source.el --- Workspace-owned asynchronous source IO -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'remote-workspace)
(require 'remote-process)

(cl-defstruct (remote-source (:constructor remote-source--create))
  workspace resource root process pending partial sequence generation state options callback event)

(defconst remote-source--program
  (with-temp-buffer
    (insert-file-contents (expand-file-name "source-agent.cjs" (file-name-directory (or load-file-name buffer-file-name))))
    (concat (buffer-string) "\nserve();\n")))

(remote-register-adapter "source-files"
                         :capabilities '(process-async process-sync)
                         :placement 'target :process-class 'background)

(defun remote-source--error (message &optional code)
  `((message . ,message) (code . ,(or code "EIO"))))

(defun remote-source--settle (source id result error-object)
  (when-let* ((pending (gethash id (remote-source-pending source))))
    (remhash id (remote-source-pending source))
    (when (timerp (cdr pending)) (cancel-timer (cdr pending)))
    (funcall (car pending) result error-object)))

(defun remote-source--fail (source message)
  "Reject every pending request without modifying the table during iteration."
  (let (ids)
    (maphash (lambda (id _) (push id ids)) (remote-source-pending source))
    (dolist (id ids)
      (remote-source--settle source id nil (remote-source--error message "ECLOSED")))))

(defun remote-source--stop (source &optional _reason)
  (setf (remote-source-state source) 'closed)
  (cl-incf (remote-source-generation source))
  (when (process-live-p (remote-source-process source))
    (delete-process (remote-source-process source)))
  (setf (remote-source-process source) nil (remote-source-partial source) "")
  (remote-source--fail source "Source lease closed"))

(defun remote-source-close (source)
  "Release SOURCE's owned process and watches.  Repeated calls are harmless."
  (if (remote-source-resource source)
      (remote-workspace-close-resource (remote-source-workspace source)
                                       (remote-source-resource source) 'source-exit)
    (remote-source--stop source)))

(defun remote-source-request (source body callback &optional timeout)
  "Asynchronously execute BODY on SOURCE and call CALLBACK with result/error.
BODY uses relative paths; no request may acquire an inactive source implicitly."
  (if (not (and (memq (remote-source-state source) '(opening open))
                (process-live-p (remote-source-process source))))
      (funcall callback nil (remote-source--error "Source process is unavailable; re-enter or recover the workspace" "ECLOSED"))
    (let* ((id (cl-incf (remote-source-sequence source)))
           (timer (run-at-time (or timeout 30) nil
                              (lambda () (remote-source--settle source id nil
                                           (remote-source--error "Source request timed out" "ETIMEDOUT"))))))
      (puthash id (cons callback timer) (remote-source-pending source))
      (condition-case error-object
          (process-send-string (remote-source-process source)
                               (concat (json-encode (cons (cons 'id id) body)) "\n"))
        (error (remote-source--settle source id nil
                                     (remote-source--error (error-message-string error-object))))))))

(defun remote-source--filter (source generation _process chunk)
  (when (= generation (remote-source-generation source))
    (setf (remote-source-partial source) (concat (remote-source-partial source) chunk))
    (condition-case error-object
        (progn
          (when (> (string-bytes (remote-source-partial source)) (* 64 1024 1024))
            (error "Source response exceeds frame limit"))
          (let ((text (remote-source-partial source)) (from 0))
            (while (string-match "\n" text from)
              (let* ((to (match-beginning 0))
                     (message (json-parse-string (substring text from to)
                                                :object-type 'alist :array-type 'list
                                                :null-object nil :false-object :json-false)))
                (setq from (1+ to))
                (if (alist-get 'event message)
                    (when (remote-source-event source) (funcall (remote-source-event source) message))
                  (remote-source--settle source (alist-get 'id message)
                                         (alist-get 'result message) (alist-get 'error message)))))
            (setf (remote-source-partial source) (substring text from))))
      (error
       (remote-source--fail source (error-message-string error-object))
       (remote-source--stop source)))))

(defun remote-source--start (source)
  (let* ((generation (cl-incf (remote-source-generation source)))
         (workspace (remote-source-workspace source))
         (context (remote-workspace-context workspace)))
    (when (process-live-p (remote-source-process source))
      (delete-process (remote-source-process source)))
    (remote-source--fail source "Source process replaced during workspace recovery")
    (setf (remote-source-state source) 'opening (remote-source-partial source) "")
    (setf (remote-source-process source)
          (remote-make-process
           :name "remote-source" :buffer nil :connection-type 'pipe :noquery t
           :coding 'utf-8-unix :remote-context context :remote-adapter "source-files"
           :command (list "node" "-e" remote-source--program)
           :filter (lambda (process chunk) (remote-source--filter source generation process chunk))
           :sentinel
           (lambda (process event)
             (when (and (= generation (remote-source-generation source))
                        (not (process-live-p process)))
               (setf (remote-source-state source) 'disconnected)
               (remote-source--fail source (format "Source process stopped: %s" event))
               (when (remote-source-event source)
                 (funcall (remote-source-event source) '((event . "disconnected"))))))))
    (remote-source-request
     source (append `((op . "open") (root . ,(remote-file-local-name (remote-source-root source))))
                    (remote-source-options source))
     (lambda (result error-object)
       (when (= generation (remote-source-generation source))
         (setf (remote-source-state source) (if error-object 'failed 'open))
         (when (remote-source-callback source)
           (let ((callback (remote-source-callback source)))
             (setf (remote-source-callback source) nil)
             (funcall callback result error-object)))
         (if error-object (remote-source-close source)
           (when (remote-source-event source)
             (funcall (remote-source-event source) '((event . "ready"))))))))
  source))

(cl-defun remote-source-open (workspace root &key options callback event)
  "Open ROOT's source process under WORKSPACE for any target.
CALLBACK receives the asynchronous handshake result/error. EVENT receives file
changes or lifecycle notifications. OPTIONS contains extensions/exclude/hidden/
watch/maxBytes.  Recovery uses the owning workspace, never a polling loop."
  (setq workspace (remote-get-workspace workspace))
  (unless (remote-workspace-live-p workspace) (error "Source workspace is not open"))
  (setq root (file-name-as-directory (remote-canonicalize-file-name root)))
  (unless (string-prefix-p (file-name-as-directory (remote-workspace-root workspace)) root)
    (error "Source root must belong to its owning workspace"))
  (let ((source (remote-source--create
                 :workspace workspace :root (remote-canonicalize-file-name root)
                 :options options :callback callback :event event
                 :pending (make-hash-table :test #'eql) :partial "" :sequence 0 :generation 0)))
    (condition-case error-object
        (progn
          (remote-source--start source)
          (setf (remote-source-resource source)
                (remote-workspace-register-recoverable-resource
                 workspace 'files source :close #'remote-source--stop
                 :recover (lambda (_resource _owner) (remote-source--start source))
                 :metadata (list :application "source-files" :root (remote-source-root source))))
          source)
      (error (remote-source--stop source) (signal (car error-object) (cdr error-object))))))

(provide 'remote-source)
;;; remote-source.el ends here
