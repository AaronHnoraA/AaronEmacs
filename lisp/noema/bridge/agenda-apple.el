;;; noema/bridge/agenda-apple.el --- Client EventKit boundary -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'remote-framework)
(require 'remote-gateway)

(defvar +noema--apple-process nil)
(defvar +noema--apple-workspace nil)
(defvar +noema--apple-resource nil)
(defvar +noema--apple-generation 0)
(defvar +noema--apple-sequence 0)
(defvar +noema--apple-partial "")
(defvar +noema--apple-pending (make-hash-table :test #'eql))
(defvar +noema--apple-clients (make-hash-table :test #'eq))

(remote-register-adapter "noema-apple" :placement 'client
                         :capabilities '(process-async) :process-class 'background)

(defun +noema--apple-settle (id value error-object)
  (when-let* ((pending (gethash id +noema--apple-pending)))
    (remhash id +noema--apple-pending)
    (cancel-timer (cdr pending))
    (funcall (car pending) value error-object)))

(defun +noema--apple-fail-pending (message)
  (let (ids)
    (maphash (lambda (id _) (push id ids)) +noema--apple-pending)
    (dolist (id ids)
      (+noema--apple-settle id nil `((code . "ECLOSED") (message . ,message))))))

(defun +noema--apple-notify (event)
  (maphash (lambda (client _)
             (ignore-errors
               (remote-gateway-notify client "noema.agenda.apple-event" event)))
           +noema--apple-clients))

(defun +noema--apple-close-process (&optional _value _reason)
  (cl-incf +noema--apple-generation)
  (when (process-live-p +noema--apple-process)
    (delete-process +noema--apple-process))
  (setq +noema--apple-process nil +noema--apple-partial "")
  (+noema--apple-fail-pending "Apple helper stopped")
  (+noema--apple-notify '((event . "disconnected"))))

(defun +noema--apple-stop ()
  (when +noema--apple-workspace
    (remote-workspace-close +noema--apple-workspace))
  (when (or +noema--apple-process (> (hash-table-count +noema--apple-pending) 0))
    (+noema--apple-close-process))
  ;; Subscriptions belong to the gateway client, so enable after disable can
  ;; still deliver ready without a timer or a reconnecting host.
  (setq +noema--apple-workspace nil +noema--apple-resource nil))

(defun +noema--apple-filter (generation _process chunk)
  (when (= generation +noema--apple-generation)
    (setq +noema--apple-partial (concat +noema--apple-partial chunk))
    (condition-case error-object
        (let ((text +noema--apple-partial) (from 0))
          (when (> (string-bytes text) (* 8 1024 1024)) (error "Apple response too large"))
          (while (string-match "\n" text from)
            (let* ((end (match-beginning 0))
                   (message (json-parse-string (substring text from end)
                                              :object-type 'alist :array-type 'array
                                              :null-object nil :false-object :json-false)))
              (setq from (1+ end))
              (if (alist-get 'event message) (+noema--apple-notify message)
                (+noema--apple-settle (alist-get 'id message)
                                       (alist-get 'result message) (alist-get 'error message)))))
          (when (= generation +noema--apple-generation)
            (setq +noema--apple-partial (substring text from))))
      (error
       (+noema--apple-fail-pending (error-message-string error-object))
       (+noema--apple-close-process)))))

(defun +noema--apple-spawn ()
  (let* ((binary (expand-file-name "site-lisp/noema/build/apple/noema-agenda-eventkit" user-emacs-directory))
         (generation (cl-incf +noema--apple-generation)))
    (unless (file-executable-p binary)
      (user-error "Build the macOS helper first: make -C site-lisp/noema agenda-apple-build"))
    (setq +noema--apple-process
          (remote-make-client-process
           :name "noema-eventkit" :buffer nil :noquery t :connection-type 'pipe
           :coding 'utf-8-unix :remote-adapter "noema-apple"
           :remote-client-directory user-emacs-directory :command (list binary)
           :filter (lambda (process chunk) (+noema--apple-filter generation process chunk))
           :sentinel (lambda (process event)
                       (when (and (= generation +noema--apple-generation)
                                  (not (process-live-p process)))
                         (setq +noema--apple-process nil +noema--apple-partial "")
                         (+noema--apple-fail-pending (concat "Apple helper exited: " event))
                         (+noema--apple-notify '((event . "disconnected")))))))
    +noema--apple-process))

(defun +noema--apple-start ()
  "Start only after an explicit enable command; never infer from a project."
  (unless (process-live-p +noema--apple-process)
    (+noema--apple-stop)
    ;; This private client workspace is independent of every source project.
    (let ((root (expand-file-name "var/noema/apple/" user-emacs-directory)))
      (make-directory root t)
      (setq +noema--apple-workspace (remote-workspace-open root :connect nil))
      (condition-case error-object
          (let ((process (+noema--apple-spawn)))
            (setq +noema--apple-resource
                  (remote-workspace-register-recoverable-resource
                   +noema--apple-workspace 'service process
                   :close #'+noema--apple-close-process
                   :recover (lambda (&rest _) (+noema--apple-spawn))
                   :metadata '(:application "noema-apple"))))
        (error (+noema--apple-stop) (signal (car error-object) (cdr error-object))))))
  +noema--apple-process)

(defun +noema--apple-request (body callback &optional timeout)
  "Send BODY to the enabled client helper; CALLBACK receives result/error."
  (if (not (process-live-p +noema--apple-process))
      (funcall callback nil '((code . "EDISABLED") (message . "Enable Noema Apple integration explicitly first")))
    (let* ((id (cl-incf +noema--apple-sequence))
           (timer (run-at-time (or timeout 30) nil
                              (lambda () (+noema--apple-settle id nil
                                           '((code . "ETIMEDOUT") (message . "Apple request timed out")))))))
      (puthash id (cons callback timer) +noema--apple-pending)
      (condition-case error-object
          (process-send-string +noema--apple-process
                               (concat (json-encode (cons (cons 'id id) body)) "\n"))
        (error (+noema--apple-settle id nil `((code . "EIO") (message . ,(error-message-string error-object)))))))))

(defun +noema--apple-gateway (body client)
  (unless client (error "Apple bridge requires a connected client"))
  (when (equal (alist-get 'op body) "authorize")
    (error "Use the explicit Emacs Apple enable command to request access"))
  (puthash client t +noema--apple-clients)
  (let ((deferred (remote-gateway-defer 35)))
    (+noema--apple-request body
                            (lambda (value error-object)
                              (remote-gateway-resolve deferred (if error-object `((error . ,error-object)) value))))
    deferred))

(defun +noema--apple-client-left (client)
  (remhash client +noema--apple-clients))
(add-hook 'remote-gateway-client-disconnected-hook #'+noema--apple-client-left)
(add-hook 'kill-emacs-hook #'+noema--apple-stop)
(provide 'noema/bridge/agenda-apple)
