;;; aaron-neopyter-rpc.el --- WebSocket server + msgpack-rpc transport -*- lexical-binding: t -*-

;;; Commentary:
;; Direct-mode WebSocket server that speaks msgpack-rpc with the Neopyter JupyterLab
;; extension.  Wire protocol:
;;   - Transport   : WebSocket (Emacs is server, extension is client)
;;   - Encoding    : msgpack-rpc over base64-encoded text frames
;;   - Request     : [0, msgid, method, params]
;;   - Response    : [1, msgid, error, result]
;;   - Notification: [2, method, params]
;;
;; The extension calls RPC methods on its dispatcher; Emacs sends requests and
;; receives responses.  Emacs also receives inbound notifications (Phase 5 ready).

;;; Code:

(require 'cl-lib)
(require 'websocket)
(require 'msgpack)

(defconst aaron-neopyter-rpc--type-request      0)
(defconst aaron-neopyter-rpc--type-response     1)
(defconst aaron-neopyter-rpc--type-notification 2)
(defconst aaron-neopyter-rpc--default-timeout  10.0)

;;; Connection struct

(cl-defstruct (aaron-neopyter--conn
               (:constructor aaron-neopyter--conn-create)
               (:copier nil))
  "State for one Neopyter direct-mode WebSocket server."
  server       ; network-process: the websocket server
  websocket    ; websocket object: the active extension connection, or nil
  host         ; string: bind host
  port         ; integer: bind port
  (status 'disconnected) ; 'disconnected | 'connected
  (pending (make-hash-table :test #'eql)) ; msgid -> (callback . timeout-timer)
  (next-id 1)  ; next request id
  notification-handlers  ; alist: method-string -> function(params)
  on-connect   ; function() called when extension connects
  on-disconnect) ; function() called when extension disconnects

;;; Codec — base64 ↔ msgpack ↔ lisp

(defun aaron-neopyter-rpc--encode (obj)
  "Encode OBJ as msgpack and return a base64 string (no line-breaks)."
  (base64-encode-string (msgpack-encode obj) t))

(defun aaron-neopyter-rpc--b64-to-unibyte (b64)
  "Decode base64 string B64 to a raw unibyte byte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert b64)
    (base64-decode-region (point-min) (point-max))
    (buffer-string)))

(defun aaron-neopyter-rpc--decode (b64-text)
  "Decode base64 text frame B64-TEXT to a Lisp value via msgpack."
  (condition-case err
      (msgpack-read-from-string (aaron-neopyter-rpc--b64-to-unibyte b64-text))
    (error
     (message "[neopyter-rpc] decode error: %s" (error-message-string err))
     nil)))

;;; Message builders

(defun aaron-neopyter-rpc--request-msg (msgid method params)
  "Build a msgpack-rpc request vector [0 MSGID METHOD PARAMS]."
  (vector aaron-neopyter-rpc--type-request msgid method params))

(defun aaron-neopyter-rpc--response-msg (msgid error result)
  "Build a msgpack-rpc response vector [1 MSGID ERROR RESULT]."
  (vector aaron-neopyter-rpc--type-response msgid error result))

(defun aaron-neopyter-rpc--notify-msg (method params)
  "Build a msgpack-rpc notification vector [2 METHOD PARAMS]."
  (vector aaron-neopyter-rpc--type-notification method params))

;;; Low-level send

(defun aaron-neopyter-rpc--send (conn obj)
  "Send OBJ (a Lisp value) to the extension via CONN."
  (let ((ws (aaron-neopyter--conn-websocket conn)))
    (when (and ws (websocket-openp ws))
      (condition-case err
          (websocket-send-text ws (aaron-neopyter-rpc--encode obj))
        (error
         (message "[neopyter-rpc] send error: %s" (error-message-string err)))))))

;;; Public API

(defun aaron-neopyter-rpc-connected-p (conn)
  "Return non-nil if CONN has an active websocket connection."
  (and conn
       (eq (aaron-neopyter--conn-status conn) 'connected)
       (let ((ws (aaron-neopyter--conn-websocket conn)))
         (and ws (websocket-openp ws)))))

(defun aaron-neopyter-rpc-request (conn method params callback &optional timeout-secs)
  "Send METHOD with PARAMS over CONN; call CALLBACK with (result error).
TIMEOUT-SECS defaults to `aaron-neopyter-rpc--default-timeout'."
  (unless (aaron-neopyter-rpc-connected-p conn)
    (funcall callback nil "not connected")
    (cl-return-from aaron-neopyter-rpc-request nil))
  (let* ((msgid (prog1 (aaron-neopyter--conn-next-id conn)
                  (let ((next (1+ (aaron-neopyter--conn-next-id conn))))
                    (setf (aaron-neopyter--conn-next-id conn)
                          (if (> next #x7fffffff) 1 next)))))
         (timeout (or timeout-secs aaron-neopyter-rpc--default-timeout))
         (pending (aaron-neopyter--conn-pending conn))
         (timer (run-with-timer
                 timeout nil
                 (lambda ()
                   (when (gethash msgid pending)
                     (remhash msgid pending)
                     (funcall callback nil
                              (format "RPC timeout for %s (id=%d)" method msgid)))))))
    (puthash msgid (cons callback timer) pending)
    (aaron-neopyter-rpc--send conn (aaron-neopyter-rpc--request-msg msgid method params))))

(defun aaron-neopyter-rpc-notify (conn method params)
  "Send a notification (no response) for METHOD with PARAMS via CONN."
  (when (aaron-neopyter-rpc-connected-p conn)
    (aaron-neopyter-rpc--send conn (aaron-neopyter-rpc--notify-msg method params))))

(defun aaron-neopyter-rpc-register-handler (conn method handler)
  "Register HANDLER for inbound notification METHOD on CONN.
HANDLER is called with (params)."
  (when conn
    (let ((alist (aaron-neopyter--conn-notification-handlers conn)))
      (setf (aaron-neopyter--conn-notification-handlers conn)
            (cons (cons method handler)
                  (assoc-delete-all method alist))))))

;;; Inbound message dispatch

(defun aaron-neopyter-rpc--dispatch (conn msg)
  "Dispatch a decoded msgpack-rpc MSG on CONN."
  (when (consp msg)
    (let ((type (car msg)))
      (cond
       ((= type aaron-neopyter-rpc--type-response)
        (let* ((msgid (nth 1 msg))
               (err   (nth 2 msg))
               (result (nth 3 msg))
               (pending (aaron-neopyter--conn-pending conn))
               (entry (gethash msgid pending)))
          (when entry
            (remhash msgid pending)
            (cancel-timer (cdr entry))
            (funcall (car entry) result (and (not (null err)) err)))))
       ((= type aaron-neopyter-rpc--type-notification)
        (let* ((method (nth 1 msg))
               (params (nth 2 msg))
               (handler (alist-get method
                                   (aaron-neopyter--conn-notification-handlers conn)
                                   nil nil #'string=)))
          (when handler
            (condition-case err
                (funcall handler params)
              (error
               (message "[neopyter-rpc] notification handler error (%s): %s"
                        method (error-message-string err)))))))
       (t
        (message "[neopyter-rpc] unexpected message type %s" type))))))

;;; WebSocket server callbacks

(defun aaron-neopyter-rpc--on-open (conn ws)
  "Handle new WebSocket connection WS on CONN."
  ;; Keep only the most recent connection
  (let ((old (aaron-neopyter--conn-websocket conn)))
    (when (and old (websocket-openp old) (not (eq old ws)))
      (ignore-errors (websocket-close old))))
  (setf (aaron-neopyter--conn-websocket conn) ws)
  (setf (aaron-neopyter--conn-status conn) 'connected)
  (message "[neopyter-rpc] extension connected")
  (when (functionp (aaron-neopyter--conn-on-connect conn))
    (funcall (aaron-neopyter--conn-on-connect conn))))

(defun aaron-neopyter-rpc--on-message (conn _ws frame)
  "Handle incoming WebSocket FRAME on CONN."
  (when (websocket-frame-completep frame)
    (let* ((opcode (websocket-frame-opcode frame)))
      (when (eq opcode 'text)
        (let* ((text (websocket-frame-text frame))
               (msg  (aaron-neopyter-rpc--decode text)))
          (when msg
            (aaron-neopyter-rpc--dispatch conn msg)))))))

(defun aaron-neopyter-rpc--on-close (conn ws)
  "Handle WebSocket WS close on CONN."
  (when (eq ws (aaron-neopyter--conn-websocket conn))
    (setf (aaron-neopyter--conn-websocket conn) nil)
    (setf (aaron-neopyter--conn-status conn) 'disconnected)
    ;; Cancel all pending requests
    (maphash (lambda (msgid entry)
               (ignore-errors (cancel-timer (cdr entry)))
               (funcall (car entry) nil "connection closed"))
             (aaron-neopyter--conn-pending conn))
    (clrhash (aaron-neopyter--conn-pending conn))
    (message "[neopyter-rpc] extension disconnected")
    (when (functionp (aaron-neopyter--conn-on-disconnect conn))
      (funcall (aaron-neopyter--conn-on-disconnect conn)))))

(defun aaron-neopyter-rpc--on-error (_ws type err)
  "Log WebSocket error TYPE ERR."
  (message "[neopyter-rpc] websocket error (%s): %s" type (error-message-string err)))

;;; Server lifecycle

(defun aaron-neopyter-rpc-start-server (host port &optional on-connect on-disconnect)
  "Start a WebSocket server on HOST:PORT.
Return a new `aaron-neopyter--conn' or signal on error."
  (let* ((conn (aaron-neopyter--conn-create
                :host host
                :port port
                :on-connect on-connect
                :on-disconnect on-disconnect))
         (server (websocket-server
                  port
                  :host host
                  :on-open    (lambda (ws)    (aaron-neopyter-rpc--on-open conn ws))
                  :on-message (lambda (ws fr) (aaron-neopyter-rpc--on-message conn ws fr))
                  :on-close   (lambda (ws)    (aaron-neopyter-rpc--on-close conn ws))
                  :on-error   (lambda (ws tp er) (aaron-neopyter-rpc--on-error ws tp er)))))
    (setf (aaron-neopyter--conn-server conn) server)
    (message "[neopyter-rpc] server started at %s:%d" host port)
    conn))

(defun aaron-neopyter-rpc-stop-server (conn)
  "Stop the WebSocket server for CONN and clean up."
  (when conn
    ;; Close active websocket
    (let ((ws (aaron-neopyter--conn-websocket conn)))
      (when ws (ignore-errors (websocket-close ws))))
    (setf (aaron-neopyter--conn-websocket conn) nil)
    ;; Cancel pending requests
    (maphash (lambda (_msgid entry)
               (ignore-errors (cancel-timer (cdr entry)))
               (funcall (car entry) nil "server stopped"))
             (aaron-neopyter--conn-pending conn))
    (clrhash (aaron-neopyter--conn-pending conn))
    ;; Stop server process
    (let ((server (aaron-neopyter--conn-server conn)))
      (when server
        (ignore-errors (websocket-server-close server))))
    (setf (aaron-neopyter--conn-server conn) nil)
    (setf (aaron-neopyter--conn-status conn) 'disconnected)
    (message "[neopyter-rpc] server stopped")))

(provide 'aaron-neopyter-rpc)
;;; aaron-neopyter-rpc.el ends here
