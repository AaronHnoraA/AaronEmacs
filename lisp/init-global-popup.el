;;; init-global-popup.el --- Minibuffer frames summoned from other apps -*- lexical-binding: t -*-

;;; Commentary:
;; M-x in a standalone minibuffer-only frame.  It is the only M-x: the key
;; inside Emacs and a system launcher (Raycast, skhd) calling through
;; `emacsclient' both raise it, so it works while another application has the
;; keyboard.  The frame shares this session's buffers, history and
;; completion; it is created on demand and deleted when the read ends.
;;
;; yabai leaves every Emacs window unmanaged, so the frame places itself.

;;; Code:

(require 'general)

(declare-function my/background-show "init-background" ())

(defconst my/global-popup-frame-name "emacs-popup"
  "Title of the global popup frame.")

(defvar my/global-popup-width 90
  "Width of the global popup frame in columns.")

(defvar my/global-popup-lines 11
  "Lines the popup is expected to grow to; it is centred at that height.")

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
    ;; The frame parameters alone do not survive this configuration's frame
    ;; setup, which leaves a minibuffer-only frame ten columns wide.
    (set-frame-size frame my/global-popup-width 1)
    (set-frame-position frame
                        (+ x (max 0 (/ (- w (frame-pixel-width frame)) 2)))
                        (+ y (max 0 (/ (- h (* my/global-popup-lines
                                               (frame-char-height frame)))
                                       2))))
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)
    frame))

(defun my/global-popup--fit (frame)
  "Fit FRAME to its minibuffer vertically, keeping the centred width."
  (fit-frame-to-buffer frame nil nil nil nil 'vertically))

(defun my/global-popup--return (app)
  "Hand the keyboard back to APP, a bundle identifier, when it is non-empty."
  (when (and (stringp app) (not (string-empty-p app)))
    (let ((default-directory temporary-file-directory))
      (call-process "open" nil 0 nil "-b" app))))

(defun my/global-popup--reveal (origin)
  "Bring ORIGIN, a frame kept in the background, to the foreground.
Unconditional: a minibuffer read shows its frame by itself, and Emacs must
still become a regular application again."
  (when (frame-live-p origin)
    (if (fboundp 'my/background-show)
        (my/background-show)
      (select-frame-set-input-focus origin))))

(defun my/global-popup--run (command origin return-app &optional prefix)
  "Run COMMAND in ORIGIN without disturbing a background Emacs.
PREFIX is the prefix argument M-x was given.
A hidden ORIGIN comes forward only when COMMAND turns out to need it: it
reads from the minibuffer or changes what ORIGIN shows.  A command that
leaves nothing visible returns the keyboard to RETURN-APP."
  (let* ((buffer (window-buffer (frame-selected-window origin)))
         (hidden (not (frame-visible-p origin)))
         (reveal (lambda () (when hidden (my/global-popup--reveal origin)))))
    (if hidden
        (select-frame origin)
      (select-frame-set-input-focus origin))
    (unwind-protect
        (progn
          (add-hook 'minibuffer-setup-hook reveal)
          (setq this-command command
                real-this-command command
                prefix-arg prefix)
          (command-execute command 'record))
      (remove-hook 'minibuffer-setup-hook reveal))
    (when (and (frame-live-p origin)
               (not (eq buffer (window-buffer (frame-selected-window origin)))))
      (funcall reveal))
    (unless (seq-some #'frame-visible-p (frame-list))
      (my/global-popup--return return-app))))

(defun my/global-popup--mx (return-app &optional prefix)
  "Read a command in the popup frame and run it in the frame that was selected.
A cancelled read returns the keyboard to RETURN-APP.  PREFIX is passed on to
the command."
  (let ((origin (selected-frame))
        (frame (my/global-popup--make-frame))
        command)
    (unwind-protect
        (setq command (condition-case nil
                          (let ((resize-mini-frames #'my/global-popup--fit))
                            (intern-soft (read-extended-command)))
                        (quit nil)))
      ;; Forced: in the background the popup is the only visible frame.
      (when (frame-live-p frame)
        (delete-frame frame t)))
    (cond
     ((not (commandp command)) (my/global-popup--return return-app))
     ((frame-live-p origin)
      (my/global-popup--run command origin return-app prefix)))))

;;;###autoload
(defun my/global-mx (&optional return-app)
  "Show M-x in a standalone frame, usable while another application is active.
This is the only M-x: `execute-extended-command' is remapped to it.  Calling
it again while the popup is open closes the popup.

RETURN-APP is the bundle identifier of the application to reactivate when the
read is cancelled.  Meant for `emacsclient --eval' as well as keys: the read
is deferred so the client returns at once."
  (interactive)
  (let ((popup (my/global-popup--frame))
        (minibuffer (active-minibuffer-window)))
    (cond
     ((not (display-graphic-p))
      (call-interactively #'execute-extended-command))
     ;; Deferred like the read: this may run inside a server request.
     ((and popup minibuffer (eq (window-frame minibuffer) popup))
      (run-at-time 0 nil #'abort-recursive-edit))
     (minibuffer (select-frame-set-input-focus (window-frame minibuffer)))
     (t
      ;; A popup with no read in progress is a leftover; start over.
      (when popup
        (delete-frame popup t))
      (run-at-time 0 nil #'my/global-popup--mx return-app current-prefix-arg))))
  nil)

;; amx remaps `execute-extended-command' in its own minor-mode map, which
;; would win over the global remap, so both point at the popup.
(general-define-key [remap execute-extended-command] #'my/global-mx)
(with-eval-after-load 'amx
  (general-define-key :keymaps 'amx-mode-map
                      [remap execute-extended-command] #'my/global-mx))

(provide 'init-global-popup)

;;; init-global-popup.el ends here
