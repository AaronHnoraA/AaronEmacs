;;; init-background.el --- Emacs as a background application -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs keeps running with no visible frame and comes forward on demand.
;; `bin/emacs-app' and the Raycast extension drive these commands through
;; `emacsclient'.  Hiding is frame invisibility, not
;; application hiding, so one new frame can appear without the others.
;;
;; The launcher offers a short whitelist, `my/background-actions', and can
;; run nothing else.  M-x itself stays inside Emacs.
;;
;; A background Emacs also leaves the Dock and Cmd-Tab; it returns to both
;; when shown.  Undecorated frames get a drag strip along their top edge.
;; Both need the `macos-window' module built by `bin/emacs-app build';
;; without it the Dock icon stays and frames have no drag strip.

;;; Code:

(declare-function macos-window-set-accessory "ext:macos-window" (accessory))
(declare-function macos-window-install-drag-handles "ext:macos-window" (height))
(declare-function ns-hide-emacs "nsfns.m" (on))

(defvar my/background-hide-dock-icon t
  "Whether a background Emacs leaves the Dock, given the window module.")

(defvar my/background-window-module
  (expand-file-name "var/macos-background/macos-window.dylib"
                    user-emacs-directory)
  "The built `macos-window' module.")

(defvar my/frame-drag-handle-height 12
  "Height in pixels of the drag strip along a frame's top edge.
The strip shows a grabber while the pointer is over it.  Nil leaves frames
without one.")

(defvar my/background--frames nil
  "Frames made invisible by `my/background-hide', most recently used first.")

(defun my/background--editing-frames ()
  "Return the top-level frames."
  (seq-remove #'frame-parent (frame-list)))

(defvar my/frame-default-size '(0.66 . 0.86)
  "Width and height of a new frame, as fractions of its monitor's workarea.
Nil leaves the size to Emacs, which is 80 columns by about 36 lines.")

(defun my/frame-fit-default (frame)
  "Give FRAME the default size, centred on the monitor under the pointer.
Nothing manages Emacs frames from outside, so a frame has to pick a usable
size and place itself.  The size is set on the live frame, in pixels: as a
frame parameter it is converted to columns before the frame has its font,
and the frame then grows past the screen."
  (when (and my/frame-default-size
             (display-graphic-p frame)
             (not (frame-parameter frame 'fullscreen)))
    (pcase-let* ((`(,x ,y ,w ,h) (my/frame-pointer-workarea))
                 (width (round (* w (car my/frame-default-size))))
                 (height (round (* h (cdr my/frame-default-size)))))
      (set-frame-size frame width height t)
      (set-frame-position frame
                          (+ x (max 0 (/ (- w (frame-outer-width frame)) 2)))
                          (+ y (max 0 (/ (- h (frame-outer-height frame)) 2)))))))

(defun my/background-make-frame (&optional parameters)
  "Create a frame with PARAMETERS that starts on a neutral buffer.
`make-frame' shows the current buffer in the new frame.  When that buffer is
an xwidget page, the page is resized to the new frame's window and the frame
it was in is left showing it at the wrong size.  Without a size in
PARAMETERS the frame gets the default one."
  (let ((frame (with-current-buffer (or (get-buffer "*dashboard*")
                                        (get-scratch-buffer-create))
                 (make-frame parameters))))
    (unless (or (assq 'width parameters) (assq 'height parameters))
      (my/frame-fit-default frame))
    frame))

(defun my/frame-pointer-workarea ()
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

(defun my/background-status ()
  "Return \"visible\" when an editing frame is shown, else \"background\"."
  (if (seq-some #'frame-visible-p (my/background--editing-frames))
      "visible"
    "background"))

(defun my/background-hide (&optional all)
  "Send Emacs to the background: no visible frame, process still running.
Non-nil ALL hides every top-level frame whatever it reports; a frame that was
only just shown does not report as visible yet."
  (interactive)
  (let ((frames (if all
                    (my/background--editing-frames)
                  (seq-filter #'frame-visible-p
                              (my/background--editing-frames)))))
    ;; Frames hidden earlier stay remembered when only a later one was shown.
    (setq my/background--frames
          (seq-uniq (append frames
                            (seq-filter #'frame-live-p my/background--frames))))
    (dolist (frame frames)
      (make-frame-invisible frame t))
    (my/background-apply-dock-policy (and all 'background))
    ;; Give the keyboard to the next application.
    (when (fboundp 'ns-hide-emacs)
      (ns-hide-emacs t)))
  nil)

(defun my/background--hide-frame (frame)
  "Make FRAME invisible, leaving the other frames as they are.
The only visible frame takes Emacs to the background with it."
  (cond
   ((my/background--sole-visible-p frame)
    (my/background-hide))
   ((frame-visible-p frame)
    (setq my/background--frames
          (cons frame (delq frame my/background--frames)))
    (make-frame-invisible frame t))))

(defun my/frame-hide (&optional frame)
  "Hide FRAME, by default the selected one, without deleting it.
`my/background-show' and the launcher's frame list bring it back."
  (interactive)
  (my/background--hide-frame (or frame (selected-frame)))
  nil)

(defvar my/background--startup-done nil
  "Non-nil once startup has shown the initial frame.")

(defun my/background--startup-done-h ()
  "Record that the initial frame is on screen."
  (setq my/background--startup-done t))

(add-hook 'window-setup-hook #'my/background--startup-done-h)

(defun my/background--hide-all-h ()
  "Hide every frame once startup has put the initial frame on screen."
  (my/background-hide t))

(defun my/background-start-hidden ()
  "Start in the background: asked for by a launcher right after launch.
The server answers long before the initial frame is shown; hiding at once
finds nothing to hide and the frame appears afterwards.  So the frame that
startup has not revealed yet is kept from being revealed, and everything is
hidden again once startup has finished."
  (if my/background--startup-done
      (my/background-hide t)
    ;; `my/reveal-startup-frames' shows only frames that carry this mark.
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'my-startup-hidden nil))
    (add-hook 'window-setup-hook #'my/background--hide-all-h 90))
  nil)

(defun my/background-show ()
  "Bring Emacs to the foreground, restoring the frames that were hidden."
  (interactive)
  (let ((frames (or (seq-filter #'frame-live-p my/background--frames)
                    (my/background--editing-frames))))
    (setq my/background--frames nil)
    (dolist (frame (reverse frames))
      (make-frame-visible frame))
    ;; Stated outright: visibility is not always reported yet at this point.
    (my/background-apply-dock-policy (and frames t))
    ;; Activation is ignored while the policy change is still settling.
    (when frames
      (run-at-time 0.15 nil #'my/background--focus (car frames))))
  nil)

(defun my/background--focus (frame)
  "Give FRAME the keyboard when it is still live."
  (when (frame-live-p frame)
    (select-frame-set-input-focus frame)))

(defun my/background--sole-visible-p (frame)
  "Return non-nil when FRAME is the only visible top-level frame."
  (and (frame-live-p frame)
       (frame-visible-p frame)
       (not (frame-parent frame))
       (not (seq-some (lambda (other)
                        (and (not (eq other frame))
                             (frame-visible-p other)
                             (not (frame-parent other))))
                      (frame-list)))))

;; Emacs refuses to delete its only visible frame, and closing that frame's
;; window asks to quit Emacs.  A background application hides instead.  The
;; advice replaces only those two outcomes; a forced deletion is untouched.
(defun my/background--delete-frame-a (orig-fn &optional frame force)
  "Hide Emacs instead of failing to delete its only visible FRAME."
  (if (and (not force)
           (my/background--sole-visible-p (or frame (selected-frame))))
      (my/background-hide)
    (funcall orig-fn frame force)))

(defun my/background--handle-delete-frame-a (orig-fn event)
  "Hide Emacs instead of quitting when its only visible frame is closed."
  (if (my/background--sole-visible-p (posn-window (event-start event)))
      (my/background-hide)
    (funcall orig-fn event)))

(advice-add 'delete-frame :around #'my/background--delete-frame-a)
(advice-add 'handle-delete-frame :around #'my/background--handle-delete-frame-a)

(defun my/background-toggle ()
  "Show Emacs when it is in the background, otherwise hide it."
  (interactive)
  (if (equal (my/background-status) "visible")
      (my/background-hide)
    (my/background-show)))

(defun my/background-quit ()
  "Bring Emacs forward and quit it, so its questions are visible."
  (interactive)
  (my/background-show)
  (run-at-time 0 nil #'save-buffers-kill-emacs)
  nil)

(defun my/background--window-module-p ()
  "Return non-nil when the `macos-window' module is loaded or loadable."
  (and (display-graphic-p)
       (file-exists-p my/background-window-module)
       (or (featurep 'macos-window)
           (ignore-errors (module-load my/background-window-module) t))))

(defun my/background-apply-dock-policy (&optional state)
  "Keep Emacs out of the Dock while it is in the background.
A foreground Emacs is a regular application again, with its Dock icon,
Cmd-Tab entry and main menu; macOS ties those three together.  STATE asserts
what Emacs is becoming, since frames report a change of visibility late: the
symbol `background', or any other non-nil value for the foreground."
  (when (my/background--window-module-p)
    (macos-window-set-accessory
     (and my/background-hide-dock-icon
          (or (eq state 'background)
              (and (not state)
                   (equal (my/background-status) "background")))))))

(defun my/frame-install-drag-handles ()
  "Give every top-level frame its drag strip."
  (when (and my/frame-drag-handle-height (my/background--window-module-p))
    (macos-window-install-drag-handles my/frame-drag-handle-height)))

;;; Actions from outside Emacs

(defvar my/background-actions
  '(("Toggle Emacs" my/background-toggle)
    ("New Terminal" my/ghostel-open-new)
    ("New Frame" my/background-new-frame)
    ("Hide Frame" my/frame-hide)
    ("Open File" find-file "Path")
    ("Wiki Home" my/noema-wiki-home)
    ("Quit Emacs" my/background-quit))
  "The Emacs actions offered outside Emacs, in the order shown.
Each entry is (TITLE COMMAND) or (TITLE COMMAND PROMPT).  With PROMPT the
launcher asks for one string and COMMAND is called with it; otherwise
COMMAND runs as an interactive command.  Nothing outside this list can be
run from the launcher.")

(defun my/background--argument (encoded)
  "Decode ENCODED, a base64 UTF-8 string passed by `bin/emacs-app'."
  (decode-coding-string (base64-decode-string encoded) 'utf-8))

(defun my/background--json (value)
  "Return VALUE as base64 JSON; a printed Lisp string is not a safe transport."
  (base64-encode-string (json-serialize value) t))

(defun my/background--action-list ()
  "Return `my/background-actions' as a vector of alists.
Each entry has `id', `title' and `prompt' (empty when it takes no argument)."
  (let ((index -1))
    (vconcat
     (mapcar (lambda (action)
               `((id . ,(number-to-string (setq index (1+ index))))
                 (title . ,(car action))
                 (prompt . ,(or (nth 2 action) ""))))
             my/background-actions))))

(defun my/background-actions ()
  "Return base64 JSON for `my/background-actions'."
  (my/background--json (my/background--action-list)))

(defun my/background-new-frame ()
  "Open a new frame and give it the keyboard."
  (interactive)
  (my/background--focus (my/background-make-frame)))

(defun my/background--act (action argument)
  "Run ACTION, an entry of `my/background-actions', with string ARGUMENT.
Emacs comes forward only when the action turns out to need it: it reads from
the minibuffer or changes what the selected frame shows.  An action that
opens its own frame shows just that frame."
  (let* ((command (nth 1 action))
         (origin (selected-frame))
         (buffer (window-buffer (frame-selected-window origin)))
         (reveal #'my/background-show))
    (unwind-protect
        (progn
          (add-hook 'minibuffer-setup-hook reveal)
          (if (nth 2 action)
              (funcall command argument)
            (setq this-command command
                  real-this-command command)
            (command-execute command 'record)))
      (remove-hook 'minibuffer-setup-hook reveal))
    (when (and (frame-live-p origin)
               (not (eq buffer (window-buffer (frame-selected-window origin)))))
      (funcall reveal))))

(defun my/background-act (id argument)
  "Run the action numbered by the base64 ID with the base64 ARGUMENT.
Meant for `emacsclient --eval': the action is deferred so the client returns
at once."
  (let ((action (nth (string-to-number (my/background--argument id))
                     my/background-actions)))
    (unless action
      (error "No such action"))
    (run-at-time 0 nil #'my/background--act action
                 (my/background--argument argument)))
  nil)

;;; Frames from outside Emacs

(defun my/background--frame-id (frame)
  "Return the string that names FRAME to `bin/emacs-app'."
  (format "%s" (frame-parameter frame 'window-id)))

(defun my/background--frame-list ()
  "Return the top-level frames as a vector of alists, selected one first.
Each entry has `id', `title', `buffers' and `visible'."
  (vconcat
   (mapcar
    (lambda (frame)
      (let ((buffers (mapcar (lambda (window)
                               (buffer-name (window-buffer window)))
                             (window-list frame 'never))))
        `((id . ,(my/background--frame-id frame))
          (title . ,(buffer-name
                     (window-buffer (frame-selected-window frame))))
          (buffers . ,(string-join (seq-uniq buffers) ", "))
          (visible . ,(if (frame-visible-p frame) t :false)))))
    (my/background--editing-frames))))

(defun my/background-frames ()
  "Return base64 JSON describing the top-level frames."
  (my/background--json (my/background--frame-list)))

(defun my/background-state ()
  "Return base64 JSON for everything the launcher shows, in one round trip.
It has `status', `uptime', `memory' (resident megabytes), `buffers',
`processes' (live subprocesses), `actions' and `frames'."
  (my/background--json
   `((status . ,(my/background-status))
     (uptime . ,(emacs-uptime "%dd %hh %z%mm"))
     (memory . ,(/ (or (alist-get 'rss (remote-client-process-attributes (emacs-pid))) 0)
                   1024))
     (buffers . ,(length (buffer-list)))
     (processes . ,(length (seq-filter #'process-id (process-list))))
     (actions . ,(my/background--action-list))
     (frames . ,(my/background--frame-list)))))

(defun my/background--show-frame (frame)
  "Make FRAME visible and give it the keyboard."
  (setq my/background--frames (delq frame my/background--frames))
  (make-frame-visible frame)
  ;; Stated outright: visibility is not always reported yet at this point.
  (my/background-apply-dock-policy t)
  ;; Activation is ignored while the policy change is still settling.
  (run-at-time 0.15 nil #'my/background--focus frame))

(defun my/background--frame-act (action id)
  "Apply ACTION, a string, to the frame named ID.
The actions new, show-all and hide-all take no frame."
  (let* ((frames (my/background--editing-frames))
         (frame (seq-find (lambda (frame)
                            (equal id (my/background--frame-id frame)))
                          frames))
         (others (remq frame frames)))
    (pcase action
      ("new" (my/background-new-frame))
      ("show-all"
       (setq my/background--frames nil)
       (dolist (other (cdr frames))
         (make-frame-visible other))
       (when frames
         (my/background--show-frame (car frames))))
      ("hide-all" (my/background-hide))
      ((guard (not frame)) nil)
      ("focus" (my/background--show-frame frame))
      ("hide" (my/background--hide-frame frame))
      ("hide-others"
       (my/background--show-frame frame)
       ;; Not `my/background--hide-frame': FRAME may not report as visible
       ;; yet, and the last of the others would then hide Emacs itself.
       (dolist (other (seq-filter #'frame-visible-p others))
         (setq my/background--frames
               (cons other (delq other my/background--frames)))
         (make-frame-invisible other t)))
      ("close"
       ;; The last frame cannot go; closing it sends Emacs to the background.
       (if others
           (delete-frame frame t)
         (my/background-hide)))
      ("close-others"
       (my/background--show-frame frame)
       (dolist (other others)
         (delete-frame other t))))))

(defun my/background-frame-act (action id)
  "Apply the base64 ACTION to the frame with the base64 ID.
ACTION is one of those in `my/background--frame-act'.  Deferred, so the
client returns at once."
  (run-at-time 0 nil #'my/background--frame-act
               (my/background--argument action)
               (my/background--argument id))
  nil)

;; `make-frame' puts Emacs back in the Dock, and a deleted frame may have been
;; the last visible one, so the window state is applied again on both.
(defun my/background--refresh-window-state-h (&rest _)
  "Reapply the Dock policy and drag strips after the frame list changes."
  (setq my/background--frames
        (seq-filter #'frame-live-p my/background--frames))
  (my/background-apply-dock-policy)
  (my/frame-install-drag-handles))

;; The initial frame exists by now and already has its font.
(unless after-init-time
  (my/frame-fit-default (selected-frame)))

(add-hook 'emacs-startup-hook #'my/background--refresh-window-state-h)
(add-hook 'after-make-frame-functions #'my/background--refresh-window-state-h)
(add-hook 'after-delete-frame-functions #'my/background--refresh-window-state-h)

(provide 'init-background)

;;; init-background.el ends here
