;;; init-aaronnote-agenda-apple.el --- Client EventKit boundary -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'remote-framework)
(require 'remote-gateway)

(defvar my/noema--apple-process nil)
(defvar my/noema--apple-workspace nil)
(defvar my/noema--apple-resource nil)
(defvar my/noema--apple-generation 0)
(defvar my/noema--apple-sequence 0)
(defvar my/noema--apple-partial "")
(defvar my/noema--apple-pending (make-hash-table :test #'eql))
(defvar my/noema--apple-clients (make-hash-table :test #'eq))

(remote-register-adapter "noema-apple" :placement 'client
                         :capabilities '(process-async) :process-class 'background)

(defun my/noema--apple-settle (id value error-object)
  (when-let* ((pending (gethash id my/noema--apple-pending)))
    (remhash id my/noema--apple-pending)
    (cancel-timer (cdr pending))
    (funcall (car pending) value error-object)))

(defun my/noema--apple-fail-pending (message)
  (let (ids)
    (maphash (lambda (id _) (push id ids)) my/noema--apple-pending)
    (dolist (id ids)
      (my/noema--apple-settle id nil `((code . "ECLOSED") (message . ,message))))))

(defun my/noema--apple-notify (event)
  (maphash (lambda (client _)
             (ignore-errors
               (remote-gateway-notify client "aaronnote.agenda.apple-event" event)))
           my/noema--apple-clients))

(defun my/noema--apple-close-process (&optional _value _reason)
  (cl-incf my/noema--apple-generation)
  (when (process-live-p my/noema--apple-process)
    (delete-process my/noema--apple-process))
  (setq my/noema--apple-process nil my/noema--apple-partial "")
  (my/noema--apple-fail-pending "Apple helper stopped")
  (my/noema--apple-notify '((event . "disconnected"))))

(defun my/noema--apple-stop ()
  (when my/noema--apple-workspace
    (remote-workspace-close my/noema--apple-workspace))
  (when (or my/noema--apple-process (> (hash-table-count my/noema--apple-pending) 0))
    (my/noema--apple-close-process))
  ;; Subscriptions belong to the gateway client, so enable after disable can
  ;; still deliver ready without a timer or a reconnecting host.
  (setq my/noema--apple-workspace nil my/noema--apple-resource nil))

(defun my/noema--apple-filter (generation _process chunk)
  (when (= generation my/noema--apple-generation)
    (setq my/noema--apple-partial (concat my/noema--apple-partial chunk))
    (condition-case error-object
        (let ((text my/noema--apple-partial) (from 0))
          (when (> (string-bytes text) (* 8 1024 1024)) (error "Apple response too large"))
          (while (string-match "\n" text from)
            (let* ((end (match-beginning 0))
                   (message (json-parse-string (substring text from end)
                                              :object-type 'alist :array-type 'array
                                              :null-object nil :false-object :json-false)))
              (setq from (1+ end))
              (if (alist-get 'event message) (my/noema--apple-notify message)
                (my/noema--apple-settle (alist-get 'id message)
                                       (alist-get 'result message) (alist-get 'error message)))))
          (when (= generation my/noema--apple-generation)
            (setq my/noema--apple-partial (substring text from))))
      (error
       (my/noema--apple-fail-pending (error-message-string error-object))
       (my/noema--apple-close-process)))))

(defun my/noema--apple-spawn ()
  (let* ((binary (expand-file-name "site-lisp/noema/build/apple/noema-agenda-eventkit" user-emacs-directory))
         (generation (cl-incf my/noema--apple-generation)))
    (unless (file-executable-p binary)
      (user-error "Build the macOS helper first: make -C site-lisp/noema agenda-apple-build"))
    (setq my/noema--apple-process
          (remote-make-client-process
           :name "noema-eventkit" :buffer nil :noquery t :connection-type 'pipe
           :coding 'utf-8-unix :remote-adapter "noema-apple"
           :remote-client-directory user-emacs-directory :command (list binary)
           :filter (lambda (process chunk) (my/noema--apple-filter generation process chunk))
           :sentinel (lambda (process event)
                       (when (and (= generation my/noema--apple-generation)
                                  (not (process-live-p process)))
                         (setq my/noema--apple-process nil my/noema--apple-partial "")
                         (my/noema--apple-fail-pending (concat "Apple helper exited: " event))
                         (my/noema--apple-notify '((event . "disconnected")))))))
    my/noema--apple-process))

(defun my/noema--apple-start ()
  "Start only after an explicit enable command; never infer from a project."
  (unless (process-live-p my/noema--apple-process)
    (my/noema--apple-stop)
    ;; This private client workspace is independent of every source project.
    (let ((root (expand-file-name "var/noema/apple/" user-emacs-directory)))
      (make-directory root t)
      (setq my/noema--apple-workspace (remote-workspace-open root :connect nil))
      (condition-case error-object
          (let ((process (my/noema--apple-spawn)))
            (setq my/noema--apple-resource
                  (remote-workspace-register-recoverable-resource
                   my/noema--apple-workspace 'service process
                   :close #'my/noema--apple-close-process
                   :recover (lambda (&rest _) (my/noema--apple-spawn))
                   :metadata '(:application "noema-apple"))))
        (error (my/noema--apple-stop) (signal (car error-object) (cdr error-object))))))
  my/noema--apple-process)

(defun my/noema--apple-request (body callback &optional timeout)
  "Send BODY to the enabled client helper; CALLBACK receives result/error."
  (if (not (process-live-p my/noema--apple-process))
      (funcall callback nil '((code . "EDISABLED") (message . "Enable Noema Apple integration explicitly first")))
    (let* ((id (cl-incf my/noema--apple-sequence))
           (timer (run-at-time (or timeout 30) nil
                              (lambda () (my/noema--apple-settle id nil
                                           '((code . "ETIMEDOUT") (message . "Apple request timed out")))))))
      (puthash id (cons callback timer) my/noema--apple-pending)
      (condition-case error-object
          (process-send-string my/noema--apple-process
                               (concat (json-encode (cons (cons 'id id) body)) "\n"))
        (error (my/noema--apple-settle id nil `((code . "EIO") (message . ,(error-message-string error-object)))))))))

(defun my/noema--apple-gateway (body client)
  (unless client (error "Apple bridge requires a connected client"))
  (when (equal (alist-get 'op body) "authorize")
    (error "Use the explicit Emacs Apple enable command to request access"))
  (puthash client t my/noema--apple-clients)
  (let ((deferred (remote-gateway-defer 35)))
    (my/noema--apple-request body
                            (lambda (value error-object)
                              (remote-gateway-resolve deferred (if error-object `((error . ,error-object)) value))))
    deferred))

(defun my/noema--apple-client-left (client)
  (remhash client my/noema--apple-clients))
(add-hook 'remote-gateway-client-disconnected-hook #'my/noema--apple-client-left)
(add-hook 'kill-emacs-hook #'my/noema--apple-stop)
(provide 'init-aaronnote-agenda-apple)
