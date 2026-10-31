;;; +agent-render.el --- Bounded hidden agent rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional Emacs integration, not a patch to agent-shell.  The only upstream
;; contracts are the :state keyword on --update-fragment/--update-text and the
;; state's :buffer.  Keep the contract tests when upgrading package-vc.
;; Transport, events, permissions and process lifetime are never intercepted.

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'agent-shell)

(defgroup +agent-render nil "Agent display performance." :group 'applications)
(defcustom +agent-render-policy 'always
  "Render output always, only when visible, or never (headless).
Managed sessions use a buffer-local `visible' policy."
  :type '(choice (const always) (const visible) (const never)) :group '+agent-render)
(defcustom +agent-render-cache-max-bytes (* 4 1024 1024)
  "Maximum approximate deferred bytes per buffer; discard oldest first."
  :type 'natnum :group '+agent-render)
(defvar-local +agent-render--pending nil "FIFO of pending render operations.")
(defvar-local +agent-render--tail nil)
(defvar-local +agent-render--bytes 0)
(defvar +agent-render--replaying nil)

(defun +agent-render--dispatch (original arguments)
  "Render ORIGINAL with ARGUMENTS, or enqueue it in bounded O(1) FIFO storage."
  (let ((buffer (map-elt (plist-get arguments :state) :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (or +agent-render--replaying
                (eq +agent-render-policy 'always)
                (and (eq +agent-render-policy 'visible) (get-buffer-window buffer t)))
            (apply original arguments)
          (let* (;; STATE remains live while hidden.  Pin the turn namespace
                 ;; now so later submissions cannot move earlier output.
                 (arguments (if (plist-get arguments :namespace-id) arguments
                              (plist-put (copy-sequence arguments) :namespace-id
                                         (map-elt (plist-get arguments :state) :request-count))))
                 (bytes (+ 64 (cl-loop for value in arguments when (stringp value)
                                     sum (string-bytes value))))
                 (cell (list (list bytes original arguments))))
            (if +agent-render--tail (setcdr +agent-render--tail cell)
              (setq +agent-render--pending cell))
            (setq +agent-render--tail cell)
            (cl-incf +agent-render--bytes bytes)
            (while (> +agent-render--bytes (max 0 +agent-render-cache-max-bytes))
              (cl-decf +agent-render--bytes (caar +agent-render--pending))
              (setq +agent-render--pending (cdr +agent-render--pending)))
            (unless +agent-render--pending (setq +agent-render--tail nil))
            nil))))))

(defun +agent-render--fragment (original &rest arguments)
  "Defer expensive fragment rendering when appropriate."
  (+agent-render--dispatch original arguments))
(defun +agent-render--text (original &rest arguments)
  "Defer text rendering without deferring ACP notifications."
  (+agent-render--dispatch original arguments))

(defun +agent-render-flush (&optional buffer)
  "Replay BUFFER's bounded pending output in arrival order."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((operations +agent-render--pending)
          (+agent-render--replaying t))
      (setq +agent-render--pending nil +agent-render--tail nil
            +agent-render--bytes 0)
      (dolist (operation operations)
        (condition-case err
            (apply (nth 1 operation) (nth 2 operation))
          (error (display-warning '+agent-render (error-message-string err) :warning)))))))

(defun +agent-render--visible (frame)
  "Flush the agent buffers FRAME's windows now show.
The global value of `window-buffer-change-functions' receives the frame; only
a buffer-local entry is passed a window."
  (dolist (window (window-list frame 'nomini))
    (with-current-buffer (window-buffer window)
      (when (and +agent-render--pending (eq +agent-render-policy 'visible))
        (+agent-render-flush)))))

(define-minor-mode +agent-render-mode
  "Enable the optional hidden rendering adapter globally.
Disabling flushes queued output and leaves normal upstream behavior."
  :global t :group '+agent-render
  (if +agent-render-mode
      (progn
        (advice-add 'agent-shell--update-fragment :around #'+agent-render--fragment)
        (advice-add 'agent-shell--update-text :around #'+agent-render--text)
        (add-hook 'window-buffer-change-functions #'+agent-render--visible))
    (advice-remove 'agent-shell--update-fragment #'+agent-render--fragment)
    (advice-remove 'agent-shell--update-text #'+agent-render--text)
    (remove-hook 'window-buffer-change-functions #'+agent-render--visible)
    (dolist (buffer (buffer-list))
      (when (buffer-local-value '+agent-render--pending buffer) (+agent-render-flush buffer)))))

(provide 'tools/agent/render)
;;; +agent-render.el ends here
