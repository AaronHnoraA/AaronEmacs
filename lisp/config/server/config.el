;;; config/server/config.el --- Emacs server & browser editing -*- lexical-binding: t -*-

;;; Commentary:
;; Enable Emacs server and edit-server for browser integration.
;; Works with browser extensions like:
;;  - Edit with Emacs
;;  - Atomic Chrome
;;  - GhostText

;;; Code:

;; ------------------------------------------------------------
;; Start Emacs server
;; ------------------------------------------------------------

(require 'server)

;; Ghostty/Yazi opens files through a no-wait client.  Keep the visited
;; buffer after that short-lived client disconnects so it remains visible.
(setq server-kill-new-buffers nil)

(when (not (eq system-type 'darwin))
  (setq server-use-tcp t
        server-host "0.0.0.0"
        server-port 54321))

;; Launchers bound every emacsclient call by a timeout (bin/emacs-app,
;; Raycast), so a client may be gone when its answer is ready.  Writing to
;; its socket then signals "Process server <N> no longer connected to pipe;
;; closed it", which reaches the echo area as if something had failed.  The
;; client left on purpose: log it and drop the connection quietly.
(defadvice! +server-send-to-gone-client-a (fn proc string)
  "Call FN with PROC and STRING, quietly dropping a client that has left."
  :around #'server-send-string
  (if (not (process-live-p proc))
      (server-log "Dropped a reply to a client that has left" proc)
    (condition-case err
        (funcall fn proc string)
      (error
       (if (process-live-p proc)
           (signal (car err) (cdr err))
         (server-log (format "Dropped a reply: %s" (error-message-string err)) proc)
         (ignore-errors (server-delete-client proc t)))))))

;; Keep a local socket available for Emacs client integrations on macOS.
(unless (or noninteractive (server-running-p))
  (server-start))


(provide 'config/server/config)

;;; config/server/config.el ends here
