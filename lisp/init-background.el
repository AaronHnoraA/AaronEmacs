;;; init-background.el --- Emacs as a background application -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs keeps running with no visible frame and comes forward on demand.
;; `bin/emacs-app' and the menu bar item drive these commands through
;; `emacsclient'.  Hiding is frame invisibility, not application hiding, so
;; that `my/global-mx' can raise its popup without the editing frames.
;;
;; A background Emacs also leaves the Dock and Cmd-Tab; it returns to both
;; when shown.  Undecorated frames get a drag strip along their top edge.
;; Both need the `macos-window' module built by `bin/emacs-app build';
;; without it the Dock icon stays and frames have no drag strip.

;;; Code:

(declare-function macos-window-set-accessory "ext:macos-window" (accessory))
(declare-function macos-window-install-drag-handles "ext:macos-window" (height))
(declare-function ns-hide-emacs "nsfns.m" (on))
(defvar my/global-popup-frame-name)

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
  "Return top-level frames other than the global popup."
  (seq-remove (lambda (frame)
                (or (frame-parent frame)
                    (equal (frame-parameter frame 'name)
                           (bound-and-true-p my/global-popup-frame-name))))
              (frame-list)))

(defun my/background-status ()
  "Return \"visible\" when an editing frame is shown, else \"background\"."
  (if (seq-some #'frame-visible-p (my/background--editing-frames))
      "visible"
    "background"))

(defun my/background-hide ()
  "Send Emacs to the background: no visible frame, process still running."
  (interactive)
  (let ((frames (seq-filter #'frame-visible-p (my/background--editing-frames))))
    ;; Frames hidden earlier stay remembered when only a later one was shown.
    (setq my/background--frames
          (seq-uniq (append frames
                            (seq-filter #'frame-live-p my/background--frames))))
    (dolist (frame frames)
      (make-frame-invisible frame t))
    (my/background-apply-dock-policy)
    ;; Give the keyboard to the next application.
    (when (fboundp 'ns-hide-emacs)
      (ns-hide-emacs t)))
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

(defun my/background-apply-dock-policy (&optional foreground)
  "Keep Emacs out of the Dock while it is in the background.
A foreground Emacs is a regular application again, with its Dock icon,
Cmd-Tab entry and main menu; macOS ties those three together.  Non-nil
FOREGROUND asserts that Emacs is being shown."
  (when (my/background--window-module-p)
    (macos-window-set-accessory
     (and my/background-hide-dock-icon
          (not foreground)
          (equal (my/background-status) "background")))))

(defun my/frame-install-drag-handles ()
  "Give every top-level frame its drag strip."
  (when (and my/frame-drag-handle-height (my/background--window-module-p))
    (macos-window-install-drag-handles my/frame-drag-handle-height)))

;; `make-frame' puts Emacs back in the Dock, and a deleted frame may have been
;; the last visible one, so the window state is applied again on both.
(defun my/background--refresh-window-state-h (&rest _)
  "Reapply the Dock policy and drag strips after the frame list changes."
  (my/background-apply-dock-policy)
  (my/frame-install-drag-handles))

(add-hook 'emacs-startup-hook #'my/background--refresh-window-state-h)
(add-hook 'after-make-frame-functions #'my/background--refresh-window-state-h)
(add-hook 'after-delete-frame-functions #'my/background--refresh-window-state-h)

(provide 'init-background)

;;; init-background.el ends here
