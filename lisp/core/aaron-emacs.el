;;; aaron-emacs.el --- Session lifecycle hooks and incremental loading -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Named points in a session's life that modules attach to instead of
;; inventing their own first-use checks:
;;
;;   `aaron-first-input-hook'   before the first command after startup
;;   `aaron-first-file-hook'    before the first visited file or Dired buffer
;;   `aaron-first-buffer-hook'  the first time a real buffer is shown
;;   `aaron-init-ui-hook'       once the initial frame exists (`window-setup-hook')
;;   `aaron-switch-buffer-hook' / `aaron-switch-window-hook' / `aaron-switch-frame-hook'
;;   `MODE-local-vars-hook'  after file and directory locals are applied
;;
;; plus `aaron-load-packages-incrementally', which loads heavy features in
;; small steps while Emacs is idle.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   core/lisp/doom-emacs.el and core/lisp/doom.el @ 59cdaa3

;;; Code:

(require 'aaron-lib)

(defgroup aaron nil
  "Session lifecycle hooks and incremental loading."
  :group 'initialization)

;;; Hooks

(defcustom aaron-first-input-hook nil
  "Transient hook run before the first command after startup."
  :type 'hook
  :group 'aaron)

(defcustom aaron-first-file-hook nil
  "Transient hook run before the first visited file or Dired buffer."
  :type 'hook
  :group 'aaron)

(defcustom aaron-first-buffer-hook nil
  "Transient hook run the first time a real buffer is shown."
  :type 'hook
  :group 'aaron)

(defcustom aaron-init-ui-hook nil
  "Hook run once the initial frame exists."
  :type 'hook
  :group 'aaron)

(defcustom aaron-switch-buffer-hook nil
  "Hook run after a window shows a different buffer than before."
  :type 'hook
  :group 'aaron)

(defcustom aaron-switch-window-hook nil
  "Hook run after another window of the same frame is selected."
  :type 'hook
  :group 'aaron)

(defcustom aaron-switch-frame-hook nil
  "Hook run after frame focus changes, debounced per frame.
See `aaron-switch-frame-hook-debounce-delay'."
  :type 'hook
  :group 'aaron)

(defvar aaron-switch-frame-hook-debounce-delay 2.0
  "Seconds during which `aaron-switch-frame-hook' will not run again for a frame.")

;;; MODE-local-vars-hook

;; File and directory locals are applied after the major mode and its hooks
;; have run.  A hook function that must see them goes on
;; MODE-local-vars-hook instead.

(defvar-local aaron-inhibit-local-var-hooks nil
  "Non-nil once this buffer's MODE-local-vars-hook has run.")

(defun aaron-run-local-var-hooks-h ()
  "Run MAJOR-MODE-local-vars-hook after local variables are applied."
  (unless (or aaron-inhibit-local-var-hooks
              delay-mode-hooks
              ;; Internal buffers (leading space) never get these hooks.
              (string-prefix-p
               " " (buffer-name (or (buffer-base-buffer) (current-buffer)))))
    (setq-local aaron-inhibit-local-var-hooks t)
    (let ((hook-var (intern (format "%s-local-vars-hook" major-mode))))
      (unless (boundp hook-var)
        (set hook-var nil))
      (unless (get hook-var 'variable-documentation)
        (put hook-var 'variable-documentation
             (format "Hook run after file and directory locals are applied in `%s', after `%s-hook'.\nRun by `aaron-run-local-var-hooks-h'."
                     major-mode major-mode)))
      (aaron-run-hooks hook-var))))

(defun aaron-run-local-var-hooks-maybe-h ()
  "Run MODE-local-vars-hook when `enable-local-variables' is off.
Then `hack-local-variables-hook' never runs."
  (unless enable-local-variables
    (aaron-run-local-var-hooks-h)))

;;; Incremental loading

(defvar aaron-incremental-packages nil
  "Features to load one at a time, while idle, after startup.
Break a large package into its parts so no single step pauses Emacs.")

(defvar aaron-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "Idle seconds before incremental loading starts.
Nil disables it; 0 loads everything at startup instead.")

(defvar aaron-incremental-idle-timer 0.75
  "Idle seconds between two incremental loading steps.")

(defvar aaron--incremental-timer nil
  "Pending incremental loading timer.")

(defun aaron--incremental-require (feature)
  "Require FEATURE from this machine, quietly.
The client directory and the reduced `file-name-handler-alist' keep a
remote current buffer from sending the load to its target."
  (let* ((file-name-handler-alist
          (list (rassq 'jka-compr-handler file-name-handler-alist)))
         ;; Bound after the handler list so expanding it cannot reach a
         ;; remote `default-directory'.
         (default-directory (expand-file-name user-emacs-directory "/"))
         (inhibit-message t))
    (require feature nil t)
    t))

(defun aaron-load-packages-incrementally (packages &optional now)
  "Register PACKAGES for incremental loading.
With NOW, start loading them: one per idle step, stopping at user input
and resuming at the next idle period."
  (if (not now)
      (setq aaron-incremental-packages (append aaron-incremental-packages packages))
    (setq aaron--incremental-timer nil)
    (with-delayed-gc!
      (let ((first-idle (or aaron-incremental-first-idle-timer
                            aaron-incremental-idle-timer)))
        (while packages
          (let ((req (pop packages))
                idle-time)
            (if (featurep req)
                (aaron-log 3 "iloader: already loaded %s (%d left)" req (length packages))
              (condition-case-unless-debug e
                  (and (or (null (setq idle-time (current-idle-time)))
                           (< (float-time idle-time) first-idle)
                           (not (while-no-input
                                  (aaron-log 3 "iloader: loading %s (%d left)" req (length packages))
                                  (aaron--incremental-require req))))
                       (push req packages))
                (error
                 (message "Error: failed to incrementally load %S because: %s" req e)
                 (setq packages nil)))
              (if (null packages)
                  (aaron-log 3 "iloader: finished")
                (setq aaron--incremental-timer
                      (run-at-time (if idle-time aaron-incremental-idle-timer first-idle)
                                   nil #'aaron-load-packages-incrementally
                                   packages t))
                (setq packages nil)))))))))

(defun aaron-load-packages-incrementally-h ()
  "Start loading `aaron-incremental-packages'."
  (when (numberp aaron-incremental-first-idle-timer)
    (if (zerop aaron-incremental-first-idle-timer)
        (mapc #'aaron--incremental-require aaron-incremental-packages)
      (setq aaron--incremental-timer
            (run-with-idle-timer aaron-incremental-first-idle-timer nil
                                 #'aaron-load-packages-incrementally
                                 aaron-incremental-packages t)))))

(defun aaron/cancel-incremental-loading ()
  "Stop incremental loading."
  (interactive)
  (when (timerp aaron--incremental-timer)
    (cancel-timer aaron--incremental-timer))
  (setq aaron--incremental-timer nil))

;;; Switch hooks

(defun aaron-run-switch-buffer-hooks-h (&optional _)
  "Run `aaron-switch-buffer-hook' when the selected window shows a new buffer."
  (unless (or (minibufferp)
              (eq (window-old-buffer) (current-buffer)))
    (with-delayed-gc! (aaron-run-hooks 'aaron-switch-buffer-hook))))

(defun aaron-run-switch-window-hooks-h (&optional _)
  "Run `aaron-switch-window-hook' when another window of this frame is selected."
  (unless (or (minibufferp)
              (not (eq (old-selected-frame) (selected-frame)))
              (eq (old-selected-window) (minibuffer-window)))
    (with-delayed-gc! (aaron-run-hooks 'aaron-switch-window-hook))))

(defun aaron--run-switch-frame-hooks-fn (_)
  "Run `aaron-switch-frame-hook' in each newly focused frame, debounced."
  (remove-hook 'pre-redisplay-functions #'aaron--run-switch-frame-hooks-fn)
  (with-delayed-gc!
    (dolist (fr (visible-frame-list))
      (let ((state (frame-focus-state fr)))
        (when (and state (not (eq state 'unknown)))
          (let ((last-update (frame-parameter fr '+last-focus)))
            (when (or (null last-update)
                      (> (float-time (time-subtract (current-time) last-update))
                         aaron-switch-frame-hook-debounce-delay))
              (with-selected-frame fr
                (unwind-protect
                    (let ((inhibit-redisplay t))
                      (aaron-run-hooks 'aaron-switch-frame-hook))
                  (set-frame-parameter fr '+last-focus (current-time)))))))))))

(defvar aaron--last-focus-state nil
  "Focus states of all frames when they were last compared.")

(defun aaron-run-switch-frame-hooks-fn ()
  "Schedule `aaron-switch-frame-hook' once per change of frame focus."
  (unless (equal aaron--last-focus-state
                 (setq aaron--last-focus-state
                       (mapcar #'frame-focus-state (frame-list))))
    (add-hook 'pre-redisplay-functions #'aaron--run-switch-frame-hooks-fn)))

;;; Wiring

(defun aaron--first-buffer-candidate-p ()
  "Return non-nil when the current buffer counts for `aaron-first-buffer-hook'."
  (not (or (minibufferp)
           (string-prefix-p " " (buffer-name))
           (member (buffer-name) '("*scratch*" "*Messages*")))))

(defun aaron-init-ui-h ()
  "Run `aaron-init-ui-hook' and install the switch hooks."
  (aaron-run-hooks 'aaron-init-ui-hook)
  (add-function :after after-focus-change-function #'aaron-run-switch-frame-hooks-fn)
  (add-hook 'window-selection-change-functions #'aaron-run-switch-window-hooks-h)
  (add-hook 'window-buffer-change-functions #'aaron-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' misses files visited through the server.
  (add-hook 'server-switch-hook #'aaron-run-switch-buffer-hooks-h))

(defun aaron-finalize-h ()
  "Finish startup: catch up on files opened from the command line.
Then start incremental loading."
  (when (cl-some #'buffer-file-name (buffer-list))
    (aaron-run-hooks 'aaron-first-file-hook 'aaron-first-buffer-hook)
    (setq aaron-first-file-hook nil
          aaron-first-buffer-hook nil))
  (aaron-load-packages-incrementally-h))

(unless noninteractive
  (aaron-run-hook-on 'aaron-first-file-hook '(find-file-hook dired-initial-position-hook))
  (aaron-run-hook-on 'aaron-first-input-hook '(pre-command-hook))
  (aaron-run-hook-on 'aaron-first-buffer-hook '(find-file-hook aaron-switch-buffer-hook)
                  #'aaron--first-buffer-candidate-p)
  (add-hook 'after-change-major-mode-hook #'aaron-run-local-var-hooks-maybe-h 100)
  (add-hook 'hack-local-variables-hook #'aaron-run-local-var-hooks-h)
  (add-hook 'window-setup-hook #'aaron-init-ui-h -100)
  (add-hook 'emacs-startup-hook #'aaron-finalize-h 100))

(provide 'aaron-emacs)
;;; aaron-emacs.el ends here
