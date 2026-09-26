;;; remote-backend-ssh-pty.el --- Direct SSH PTY backend -*- lexical-binding: t; -*-

;;; Commentary:
;; An interactive terminal needs a target PTY, not a TRAMP file session.
;; Reuse the pipeline's SSH control connection and the standard backend's
;; client-side argv builder, while keeping this process-only route separate
;; from the TRAMP session pool used by file operations.

;;; Code:

(require 'remote-backend-tramp)

(defun remote-backend-ssh-pty-connect (route _context)
  "Prepare ROUTE for a direct SSH PTY without opening a TRAMP file session."
  (remote-backend-tramp--pipeline-ssh-parts route)
  (unless (remote-client-executable-find "ssh")
    (signal 'remote-backend-unsupported
            '("Local ssh executable is unavailable")))
  'ssh-pty)

(defun remote-backend-ssh-pty-live-p (connection _route _context)
  "Return whether CONNECTION is a direct SSH PTY attachment.
The shared pipeline checks transport liveness separately."
  (eq (remote-connection-handle connection) 'ssh-pty))

(defun remote-backend-ssh-pty-register ()
  "Register a PTY route that bypasses TRAMP file-session startup."
  (remote-register-backend
   "ssh-pty"
   :capabilities '(pty)
   :project #'remote-backend-tramp-project
   :expand-localname #'remote-backend-tramp-expand-localname
   :connect #'remote-backend-ssh-pty-connect
   :live #'remote-backend-ssh-pty-live-p
   :prepare-process #'remote-backend-tramp-prepare-process
   :describe
   (lambda () '(:kind ssh-pty :session-owner pipeline :file-operation-cost none))))

(provide 'remote-backend-ssh-pty)
;;; remote-backend-ssh-pty.el ends here
