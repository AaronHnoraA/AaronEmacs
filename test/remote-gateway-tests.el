;;; remote-gateway-tests.el --- Unified gateway integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'url)
(require 'websocket)
(require 'remote-gateway)

(defun remote-gateway-test--wait (predicate &optional timeout)
  "Wait until PREDICATE succeeds or TIMEOUT expires."
  (let ((deadline (+ (float-time) (or timeout 3))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(ert-deftest remote-gateway-encodes-string-keyed-hash-payloads ()
  (let ((draft (make-hash-table :test #'equal)))
    (puthash "title" "Group" draft)
    (puthash "tags" ["algebra" "qc"] draft)
    (let* ((encoded
            (remote-gateway--encode
             `((channel . "noema:api:notes:create-node")
               (args . [,draft]))))
           (decoded
            (json-parse-string encoded
                               :object-type 'hash-table
                               :array-type 'list))
           (body (car (gethash "args" decoded))))
      (should (equal (gethash "channel" decoded)
                     "noema:api:notes:create-node"))
      (should (equal (gethash "title" body) "Group"))
      (should (equal (gethash "tags" body) '("algebra" "qc"))))))

(ert-deftest remote-gateway-rejects-unmasked-client-control-frames ()
  (dolist (opcode '(8 9 10))
    (let (closed)
      (cl-letf (((symbol-function 'process-get) (lambda (&rest _) nil))
                ((symbol-function 'process-put) #'ignore)
                ((symbol-function 'delete-process) (lambda (_) (setq closed t)))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (ert-fail "Must not answer an unmasked client frame"))))
        (remote-gateway--websocket-filter 'test-peer (unibyte-string (+ #x80 opcode) 0))
        (should closed)))))

(ert-deftest remote-gateway-acknowledges-masked-client-close ()
  (let (closed reply)
    (cl-letf (((symbol-function 'process-get) (lambda (&rest _) nil))
              ((symbol-function 'process-put) #'ignore)
              ((symbol-function 'delete-process) (lambda (_) (setq closed t)))
              ((symbol-function 'process-send-string)
               (lambda (_ bytes) (setq reply bytes))))
      (remote-gateway--websocket-filter 'test-peer (unibyte-string #x88 #x80 1 2 3 4))
      (should (equal reply (unibyte-string #x88 0)))
      (should closed))))

(ert-deftest remote-gateway-http-evaluates-multiple-elisp-forms ()
  (let* ((info (remote-gateway-connection-info))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-serialize
           '((jsonrpc . "2.0")
             (id . 1)
             (method . "emacs.eval")
             (params .
                     ((source .
                              "(setq remote-gateway-test-value 40) (+ remote-gateway-test-value 2)"))))))
         (buffer
          (url-retrieve-synchronously
           (plist-get info :http-url) t t 3)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (let* ((response
                  (json-parse-string
                   (buffer-substring-no-properties (point) (point-max))
                   :object-type 'alist))
                 (result (alist-get 'result response)))
            (should (= (alist-get 'value result) 42))
            (should (equal (alist-get 'printed result) "42"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest remote-gateway-refuses-requests-from-web-pages ()
  (should (remote-gateway--foreign-request-p
           (make-instance 'ws-request
                          :headers '((:HOST . "127.0.0.1:1")
                                     (:ORIGIN . "https://evil.example")))))
  (should-not (remote-gateway--foreign-request-p
               (make-instance 'ws-request
                              :headers '((:HOST . "127.0.0.1:1"))))))

(ert-deftest remote-gateway-websocket-registers-and-serves-bidirectional-rpc ()
  (let* ((binding
          (remote-gateway-prepare-client
           "gateway-test" (remote-context)
           :provides '("test.echo")))
         (messages nil)
         async-result
         async-error
         disconnected-error
         disconnected-clients
         (remote-gateway-client-disconnected-hook
          (list (lambda (client) (push client disconnected-clients))))
         socket)
    (unwind-protect
        (progn
          (setq
           socket
           (websocket-open
            (plist-get binding :websocket-url)
            :on-open
            (lambda (websocket)
              (websocket-send-text
               websocket
               (json-serialize
                `((jsonrpc . "2.0")
                  (id . "register")
                  (method . "gateway.register")
                  (params .
                          ((bindingId .
                                      ,(plist-get binding :binding-id))
                           (instanceId . "gateway-test-instance")
                           (endpoint . ((port . 43123)))))))))
            :on-message
            (lambda (websocket frame)
              (let ((message
                     (json-parse-string
                      (websocket-frame-text frame)
                      :object-type 'alist)))
                (push message messages)
                (when (equal (alist-get 'method message) "test.echo")
                  (websocket-send-text
                   websocket
                   (json-serialize
                    `((jsonrpc . "2.0")
                      (id . ,(alist-get 'id message))
                      (result . ,(alist-get 'params message))))))))))
          (should
           (remote-gateway-test--wait
            (lambda ()
              (remote-gateway-find-client "gateway-test"))))
          (let* ((large-value (make-string 200000 ?x))
                 (client
                  (remote-gateway-find-client "gateway-test"))
                 (result
                  (remote-gateway-request-sync
                   client "test.echo"
                   `((answer . 42) (large . ,large-value))
                   3)))
            (should (= (alist-get 'answer result) 42))
            (should (equal (alist-get 'large result) large-value))
            (should
             (equal
              (alist-get 'port
                         (remote-gateway-client-endpoint client))
              43123))
            (should
             (equal
              (plist-get
               (remote-channel-metadata
                (remote-gateway-client-channel client))
               :application)
              "emacs-gateway"))
            (remote-gateway-request-async
             client "test.echo" '((async . 42))
             (lambda (result error)
               (setq async-result result
                     async-error error))
             3)
            (should
             (remote-gateway-test--wait
              (lambda () async-result)))
            (should-not async-error)
            (should (= (alist-get 'async async-result) 42))
            (remote-gateway-request-async
             client "test.never" nil
             (lambda (_result error)
               (setq disconnected-error error))
             3)
            (websocket-close socket)
            (setq socket nil)
            (should
             (remote-gateway-test--wait
              (lambda () disconnected-error)))
            (should
             (equal
              (alist-get "message" disconnected-error nil nil #'string=)
              "Gateway client disconnected"))
            (should
             (zerop (hash-table-count remote-gateway--pending)))
            (should (equal disconnected-clients (list client)))
            ;; A duplicate sentinel must not release the client's resources twice.
            (remote-gateway--client-disconnected (remote-gateway-client-process client) "duplicate")
            (should (equal disconnected-clients (list client)))))
      (when socket
        (ignore-errors (websocket-close socket))))))

(ert-deftest remote-gateway-replaces-and-releases-disconnected-bindings ()
  (let* ((client-id
          (format "binding-lifecycle-%s-%s"
                  (emacs-pid) (float-time)))
         (before (hash-table-count remote-gateway--bindings))
         first second)
    (unwind-protect
        (progn
          (setq first
                (remote-gateway-prepare-client
                 client-id (remote-context)))
          (should
           (= (hash-table-count remote-gateway--bindings)
              (1+ before)))
          (setq second
                (remote-gateway-prepare-client
                 client-id (remote-context)))
          (should-not
           (equal (plist-get first :binding-id)
                  (plist-get second :binding-id)))
          (should-not
           (gethash (plist-get first :binding-id)
                    remote-gateway--bindings))
          (should
           (= (hash-table-count remote-gateway--bindings)
              (1+ before))))
      (when second
        (remote-gateway-release-binding second)))
    (should (= (hash-table-count remote-gateway--bindings) before))))

(ert-deftest remote-gateway-websocket-supports-deferred-inbound-responses ()
  (let* ((binding
          (remote-gateway-prepare-client
           "gateway-deferred-test" (remote-context)))
         response socket)
    (remote-gateway-register-method
     "test.deferred"
     (lambda (params _client)
       (let ((deferred (remote-gateway-defer 2)))
         (run-at-time
          0.01 nil
          (lambda ()
            (remote-gateway-resolve deferred params)))
         deferred)))
    (unwind-protect
        (progn
          (setq
           socket
           (websocket-open
            (plist-get binding :websocket-url)
            :on-open
            (lambda (websocket)
              (websocket-send-text
               websocket
               (json-serialize
                `((jsonrpc . "2.0")
                  (id . "deferred")
                  (method . "test.deferred")
                  (params . ((answer . 42)))))))
            :on-message
            (lambda (_websocket frame)
              (let ((message
                     (json-parse-string
                      (websocket-frame-text frame)
                      :object-type 'alist)))
                (when (equal (alist-get 'id message) "deferred")
                  (setq response message))))))
          (should
           (remote-gateway-test--wait (lambda () response)))
          (should (= (alist-get 'answer (alist-get 'result response)) 42))
          (should
           (zerop (hash-table-count remote-gateway--inbound-pending))))
      (remote-gateway-unregister-method "test.deferred")
      (when socket
        (ignore-errors (websocket-close socket)))
      (remote-gateway-release-binding binding))))

(ert-deftest remote-gateway-deferred-http-request-fails-explicitly ()
  (remote-gateway-register-method
   "test.http-deferred"
   (lambda (_params _client)
     (remote-gateway-defer)))
  (unwind-protect
      (let* ((response
              (remote-gateway--dispatch-request
               '(("jsonrpc" . "2.0")
                 ("id" . "http")
                 ("method" . "test.http-deferred"))
               nil))
             (error-object
              (alist-get "error" response nil nil #'string=)))
        (should (= (alist-get "code" error-object nil nil #'string=)
                   -32001)))
    (remote-gateway-unregister-method "test.http-deferred")))

(ert-deftest remote-gateway-target-forward-follows-workspace-recovery ()
  "A recovered workspace forward is reused instead of joined by another."
  (let ((remote-workspaces (make-hash-table :test #'equal))
        (remote-gateway--forwards (make-hash-table :test #'equal))
        (context
         (remote-context-create
          :target-id "lab" :localname "/work/"
          :workspace-id "gateway" :workspace-root "/fs:lab:/work/"))
        (opened 0)
        dead)
    (unwind-protect
        (cl-letf
            (((symbol-function 'remote-reverse-port-forward)
              ;; What the channel layer does at the workspace boundary: one
              ;; recoverable resource whose handle is replaced on recovery.
              (lambda (_local &rest arguments)
                (let ((forward (list 'forward (cl-incf opened))))
                  (remote-workspace-register-recoverable-resource
                   (plist-get arguments :workspace) 'forward forward
                   :close (lambda (value _reason) (push value dead))
                   :recover (lambda (_resource _owner)
                              (list 'forward (cl-incf opened)))
                   :metadata (plist-get arguments :metadata))
                  forward)))
             ((symbol-function 'remote-channel-live-p)
              (lambda (value) (and value (not (memq value dead)))))
             ((symbol-function 'remote-close-channel)
              (lambda (value) (push value dead)))
             ((symbol-function 'remote-channel-endpoint)
              (lambda (forward &optional _side)
                (list :host "127.0.0.1" :port (+ 40000 (nth 1 forward))))))
          (let* ((first (remote-gateway-connection-info
                         context :placement 'target))
                 (workspace (remote-get-workspace context))
                 (resource (car (remote-workspace-resources workspace))))
            (should (= (plist-get first :port) 40001))
            (should (= (plist-get
                        (remote-gateway-connection-info
                         context :placement 'target)
                        :port)
                       40001))
            (should (= opened 1))
            ;; Transport recovery closes the old handle and installs a new one.
            (remote-workspace-recover-resource workspace resource)
            (should (= opened 2))
            (should (= (plist-get
                        (remote-gateway-connection-info
                         context :placement 'target)
                        :port)
                       40002))
            (should (= opened 2))
            (should (= (length (remote-workspace-resources workspace)) 1))
            ;; A forward whose recovery failed is replaced, not accumulated.
            (setf (remote-workspace-resource-state resource) 'failed)
            (push (remote-workspace-resource-value resource) dead)
            (should (= (plist-get
                        (remote-gateway-connection-info
                         context :placement 'target)
                        :port)
                       40003))
            (should (= (length (remote-workspace-resources workspace)) 1))
            ;; Stopping the gateway releases the forward the workspace holds
            ;; now, not the handle the gateway saw first.
            (remote-gateway-stop)
            (should-not (remote-workspace-resources workspace))
            (should (member '(forward 3) dead))))
      (remote-gateway-stop))))

(ert-deftest remote-gateway-writes-and-cleans-its-discovery-record ()
  (remote-gateway-start)
  (let ((file (remote-gateway--discovery-file)))
    (should (file-readable-p file))
    (let ((record
           (json-parse-string
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))
            :object-type 'alist)))
      (should (= (alist-get 'pid record) (emacs-pid)))
      (should (string-suffix-p "/rpc" (alist-get 'httpUrl record))))))

(provide 'remote-gateway-tests)
;;; remote-gateway-tests.el ends here
