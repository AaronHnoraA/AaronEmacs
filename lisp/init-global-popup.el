;;; init-global-popup.el --- Minibuffer frames summoned from other apps -*- lexical-binding: t -*-

;;; Commentary:
;; A standalone minibuffer-only frame that a system launcher (Raycast, skhd)
;; raises through `emacsclient', so M-x works while another application has
;; the keyboard.  The frame shares this session's buffers, history and
;; completion; it is created on demand and deleted when the read ends.
;;
;; The frame title is a contract with the window manager: yabai floats
;; windows whose title is `my/global-popup-frame-name'.

;;; Code:

(defconst my/global-popup-frame-name "emacs-popup"
  "Title of the global popup frame; the yabai float rule matches it.")

(defvar my/global-popup-width 90
  "Width of the global popup frame in columns.")

(defvar my/global-popup-top-ratio 0.22
  "Distance from the monitor's top edge to the popup, as a height fraction.")

(defun my/global-popup--frame ()
  "Return the live global popup frame, or nil."
  (seq-find (lambda (frame)
              (equal (frame-parameter frame 'name) my/global-popup-frame-name))
            (frame-list)))

(defun my/global-popup--workarea ()
  "Return the workarea (X Y WIDTH HEIGHT) of the monitor under the pointer."
  (let* ((pointer (mouse-absolute-pixel-position))
         (px (car pointer))
         (py (cdr pointer)))
    (or (seq-some
         (lambda (monitor)
           (pcase-let ((`(,x ,y ,w ,h) (alist-get 'geometry monitor)))
             (and (<= x px) (< px (+ x w)) (<= y py) (< py (+ y h))
                  (alist-get 'workarea monitor))))
         (display-monitor-attributes-list))
        (frame-monitor-workarea))))

(defun my/global-popup--make-frame ()
  "Create the popup frame on the pointer's monitor and give it the keyboard."
  (pcase-let* ((`(,x ,y ,w ,h) (my/global-popup--workarea))
               (frame (make-frame `((name . ,my/global-popup-frame-name)
                                    (minibuffer . only)
                                    (undecorated . t)
                                    (visibility . nil)
                                    (alpha . 100)
                                    (internal-border-width . 12)
                                    (width . ,my/global-popup-width)
                                    (height . 1)))))
    (set-frame-position frame
                        (+ x (max 0 (/ (- w (frame-pixel-width frame)) 2)))
                        (+ y (round (* h my/global-popup-top-ratio))))
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)
    frame))

(defun my/global-popup--return (app)
  "Hand the keyboard back to APP, a bundle identifier, when it is non-empty."
  (when (and (stringp app) (not (string-empty-p app)))
    (let ((default-directory temporary-file-directory))
      (call-process "open" nil 0 nil "-b" app))))

(defun my/global-popup--mx (return-app)
  "Read a command in the popup frame and run it in the frame that was selected.
A cancelled read returns the keyboard to RETURN-APP."
  (let ((origin (selected-frame))
        (frame (my/global-popup--make-frame))
        command)
    (unwind-protect
        (setq command (condition-case nil
                          (intern-soft (read-extended-command))
                        (quit nil)))
      (when (frame-live-p frame)
        (delete-frame frame)))
    (if (not (commandp command))
        (my/global-popup--return return-app)
      (when (frame-live-p origin)
        (select-frame-set-input-focus origin))
      (setq this-command command
            real-this-command command)
      (command-execute command 'record))))

;;;###autoload
(defun my/global-mx (&optional return-app)
  "Show M-x in a standalone frame, usable while another application is active.
RETURN-APP is the bundle identifier of the application to reactivate when the
read is cancelled.  Meant for `emacsclient --eval': the read is deferred so
the client returns at once."
  (interactive)
  (let ((popup (my/global-popup--frame))
        (minibuffer (active-minibuffer-window)))
    (cond
     (popup (select-frame-set-input-focus popup))
     (minibuffer (select-frame-set-input-focus (window-frame minibuffer)))
     (t (run-at-time 0 nil #'my/global-popup--mx return-app))))
  nil)

(provide 'init-global-popup)

;;; init-global-popup.el ends here
