;;; aaron-neopyter-ui.el --- Modeline indicator and log buffer -*- lexical-binding: t -*-

;;; Commentary:
;; UI surfaces: mode-line status segment and an optional debug log buffer.
;; Log output is gated behind `aaron-neopyter-debug'.

;;; Code:

(require 'cl-lib)

(defconst aaron-neopyter-ui--log-buffer "*aaron-neopyter-log*"
  "Buffer name for the Neopyter debug log.")

;;; Modeline

(defun aaron-neopyter-ui-status-string ()
  "Return a short status string for the mode-line.
Reads the global connection via `aaron-neopyter--connection'."
  (if (boundp 'aaron-neopyter--connection)
      (let ((conn aaron-neopyter--connection))
        (cond
         ((null conn)                        " [N:off]")
         ((aaron-neopyter-rpc-connected-p conn) " [N:⚡]")
         (t                                  " [N:…]")))
    " [N:off]"))

(defun aaron-neopyter-ui-lighter ()
  "Construct the minor-mode lighter string."
  (concat "Neopyter" (aaron-neopyter-ui-status-string)))

;;; Logging

(defun aaron-neopyter-ui-log (format-string &rest args)
  "Log a message to the Neopyter log buffer when debug mode is active.
FORMAT-STRING and ARGS are passed to `format'."
  (when (and (boundp 'aaron-neopyter-debug) aaron-neopyter-debug)
    (let ((msg (apply #'format format-string args)))
      (with-current-buffer (get-buffer-create aaron-neopyter-ui--log-buffer)
        (goto-char (point-max))
        (insert (format-time-string "[%T] ") msg "\n")))))

(defun aaron-neopyter-ui-show-log ()
  "Display the Neopyter debug log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create aaron-neopyter-ui--log-buffer)))

;;; Status display

(defun aaron-neopyter-ui-show-status ()
  "Display connection status in the echo area."
  (interactive)
  (if (and (boundp 'aaron-neopyter--connection)
           aaron-neopyter--connection)
      (let* ((conn aaron-neopyter--connection)
             (status (aaron-neopyter--conn-status conn))
             (host   (aaron-neopyter--conn-host conn))
             (port   (aaron-neopyter--conn-port conn)))
        (message "Neopyter: %s at %s:%d  (pending: %d)"
                 status host port
                 (hash-table-count (aaron-neopyter--conn-pending conn))))
    (message "Neopyter: not started")))

;;; Force mode-line update

(defun aaron-neopyter-ui-refresh ()
  "Force a mode-line refresh in all buffers with `aaron-neopyter-mode' active."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'aaron-neopyter-mode buf)
      (with-current-buffer buf
        (force-mode-line-update)))))

(provide 'aaron-neopyter-ui)
;;; aaron-neopyter-ui.el ends here
