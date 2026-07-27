;;; remote-session.el --- Public routed session lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A session is a cached backend attachment to one target pipeline.
;; `remote-connection' remains a compatibility vocabulary implemented by the
;; real `remote-session' type in `remote-connection.el'.

;;; Code:

(require 'remote-connection)

(defalias 'remote-session-acquire #'remote-connection-ensure)
(defalias 'remote-session-warm #'remote-connection-warm)
(defalias 'remote-session-invalidate #'remote-connection-invalidate)
(defalias 'remote-session-list #'remote-connection-pool-status)
(defalias 'remote-session-clear #'remote-connection-pool-clear)

(defun remote-session-invalidate-pipeline
    (pipeline &optional disconnect reason)
  "Invalidate sessions for PIPELINE.
PIPELINE is a pipeline object or its canonical ID.  With DISCONNECT, ask each
backend to close its retained handle.  REASON is recorded for observability."
  (remote-connection-invalidate-link
   (if (remote-link-p pipeline)
       (remote-link-id pipeline)
     (format "%s" pipeline))
   disconnect reason))

(provide 'remote-session)
;;; remote-session.el ends here
