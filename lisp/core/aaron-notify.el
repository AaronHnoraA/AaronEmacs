;;; aaron-notify.el --- One way to tell the user something -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `aaron-notify' says something once.  It lands where the user is: in the echo
;; area while Emacs has the keyboard, otherwise as a system notification, and
;; always in the history (`aaron/notifications').  Contracts:
;; docs/design/macos-boundary.md.

;;; Code:

(require 'cl-lib)

(cl-defstruct (aaron-notify-entry (:constructor aaron-notify-entry--create))
  id title body level source action time)

(defvar aaron-notify-history nil
  "Notifications, newest first.")

(defconst aaron-notify-history-limit 200
  "How many notifications the history keeps.")

(defvar aaron-notify-unread 0
  "Notifications sent to the system since Emacs last had the keyboard.")

(defvar aaron-notify-system-function nil
  "Function that shows an `aaron-notify-entry' outside Emacs, or nil.")

(defvar aaron-notify-unread-functions nil
  "Functions called with the new `aaron-notify-unread'.")

(defvar aaron-notify--next-id 0
  "The last generated notification id.")

(defun aaron-notify--user-here-p ()
  "Return non-nil when a frame of this Emacs has the keyboard.
A batch Emacs has no user to reach, whatever it is doing."
  (or noninteractive
      (seq-some (lambda (frame) (eq (frame-focus-state frame) t)) (frame-list))))

(defun aaron-notify--set-unread (count)
  "Make COUNT the number of unread notifications."
  (unless (= count aaron-notify-unread)
    (setq aaron-notify-unread count)
    (run-hook-with-args 'aaron-notify-unread-functions count)))

(defun aaron-notify--record (entry)
  "Put ENTRY in the history, replacing an older one with its id."
  (setq aaron-notify-history
        (take aaron-notify-history-limit
              (cons entry
                    (cl-remove (aaron-notify-entry-id entry)
                               aaron-notify-history
                               :key #'aaron-notify-entry-id :test #'equal)))))

(cl-defun aaron-notify (title &key body (level 'info) source id action)
  "Tell the user TITLE, with optional BODY.
LEVEL is `info', `success', `warning' or `error'.  SOURCE names who is
speaking.  A later notification with the same ID replaces this one.  ACTION
is called with no arguments when the user clicks it, in the history or on the
system notification; without one a click opens the history.  Return the id."
  (let ((entry (aaron-notify-entry--create
                :id (or id (format "emacs-%d" (cl-incf aaron-notify--next-id)))
                :title title :body body :level level :source source
                :action action :time (current-time))))
    (aaron-notify--record entry)
    (if (or (aaron-notify--user-here-p) (not aaron-notify-system-function))
        (message "%s%s" title (if body (concat ": " body) ""))
      (funcall aaron-notify-system-function entry)
      (aaron-notify--set-unread (1+ aaron-notify-unread)))
    (aaron-notify-entry-id entry)))

(defun notify-send (&rest params)
  "Notify with the :title and :body of PARAMS.
The older interface; :urgency `critical' is an error and :action is passed on."
  (aaron-notify (or (plist-get params :title) "Emacs")
                :body (plist-get params :body)
                :level (if (eq (plist-get params :urgency) 'critical) 'error 'info)
                :action (plist-get params :action)))

(defun aaron-notify-activate (id)
  "Do what clicking the notification ID means."
  (let ((entry (cl-find id aaron-notify-history
                        :key #'aaron-notify-entry-id :test #'equal)))
    (if-let* ((action (and entry (aaron-notify-entry-action entry))))
        (funcall action)
      (aaron/notifications))))

(defun aaron-notify--focus-changed-h ()
  "Everything sent away has been seen once Emacs has the keyboard."
  (when (aaron-notify--user-here-p)
    (aaron-notify--set-unread 0)))

(add-function :after after-focus-change-function #'aaron-notify--focus-changed-h)

(defun aaron-notify--insert (entry)
  "Insert one line for ENTRY in the history buffer."
  (let ((title (aaron-notify-entry-title entry))
        (body (aaron-notify-entry-body entry)))
    (insert (format-time-string "%m-%d %H:%M  " (aaron-notify-entry-time entry))
            (format "%-8s" (aaron-notify-entry-level entry)))
    (insert-text-button title
                        'action (lambda (_)
                                  (aaron-notify-activate
                                   (aaron-notify-entry-id entry)))
                        'follow-link t)
    (insert (if body (concat "  " (string-replace "\n" " " body)) "") "\n")))

(defun aaron/notifications ()
  "Show the notification history, newest first."
  (interactive)
  (with-current-buffer (get-buffer-create "*Notifications*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mapc #'aaron-notify--insert aaron-notify-history)
      (goto-char (point-min)))
    (special-mode)
    (pop-to-buffer (current-buffer)))
  (aaron-notify--set-unread 0))

(defalias 'notification-history #'aaron/notifications
  "Show what Emacs has told you.")

(provide 'aaron-notify)
;;; aaron-notify.el ends here
