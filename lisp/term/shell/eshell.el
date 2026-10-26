;;; term/shell/eshell.el --- Eshell as the command shell -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The configuration's command shell: run a command in a directory's eshell
;; and get its status and output back (`+eshell-run', `+eshell-run-sync'), or
;; pop up that eshell by hand (`+eshell/toggle').  One eshell buffer per target
;; context, shared with `M-x eshell'; commands sent while it is busy queue.
;; Contracts: docs/design/command-shell.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'config)
(require 'aaron-lib)

;; Eshell itself loads on first use: the entry points below require it.
(defvar eshell-foreground-command)
(defvar eshell-last-command-status)
(defvar eshell-last-input-end)
(defvar eshell-last-output-end)
(declare-function eshell "eshell" (&optional arg))
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-interrupt-process "esh-proc" ())

(declare-function +eshell-context-buffer-name "term/shell/config" (&optional directory))
(declare-function +eshell--sync-directory "term/shell/config" (buffer directory))
(declare-function aaron-terminal-normalize-directory "aaron-funcs" (directory))
(declare-function +project-current-root "tools/project/config")

(config-defvar +eshell-sync-timeout 600
  "Seconds `+eshell-run-sync' waits for a command before interrupting it."
  :type 'number
  :group 'eshell)

(define-error '+eshell-timeout "Eshell command timed out")

(defvar-local +eshell--queue nil
  "Commands waiting to run in this buffer, oldest first.
Each is a plist with :command, :directory and :callback.")

(defvar-local +eshell--current nil
  "The API command running in this buffer, as in `+eshell--queue'.
It also holds :start, a marker at the start of its output, and :pending, the
prompt input that was set aside for it.")

(defvar-local +eshell--dispatch-timer nil
  "Timer that starts the next queued command, or nil.")

(defvar-local +eshell--pending-cr ""
  "A carriage return held back from the end of the previous output chunk.")


;;; Output

(defun +eshell-normalize-output (string)
  "Rewrite terminal redraw sequences in STRING so eshell overwrites in place.
A cursor move to a column becomes a carriage return; other cursor, erase and
mode sequences are dropped; colour sequences stay.  A trailing carriage
return is held for the next chunk, since eshell only erases a line when
text follows the carriage return."
  (if (or (null string) (string-empty-p string))
      string
    (unless (string-empty-p +eshell--pending-cr)
      (setq string (concat +eshell--pending-cr string)
            +eshell--pending-cr ""))
    (setq string (replace-regexp-in-string "\e\\[[0-9]*G" "\r" string t t))
    (setq string (replace-regexp-in-string "\e\\[[0-9;?]*[ABCDEFHIJKSTfhl]" ""
                                           string t t))
    (when (string-suffix-p "\r" string)
      (setq +eshell--pending-cr "\r"
            string (substring string 0 -1)))
    string))

(defun +eshell--reset-output-h ()
  "Forget a held carriage return when a new command starts."
  (setq +eshell--pending-cr ""))


;;; Buffers

(defun +eshell--directory (directory)
  "Return DIRECTORY, or `default-directory', normalized for a terminal."
  (aaron-terminal-normalize-directory (or directory default-directory)))

(defun +eshell-buffer (&optional directory)
  "Return the eshell buffer of DIRECTORY's target context.
The buffer is created when missing, switched to DIRECTORY, and not shown."
  (require 'eshell)
  (let* ((directory (+eshell--directory directory))
         (buffer (let ((default-directory directory))
                   (save-window-excursion (eshell)))))
    (+eshell--sync-directory buffer directory)
    buffer))

(defun +eshell-busy-p (&optional buffer)
  "Return non-nil when BUFFER is running a command or has commands queued."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'eshell-mode)
         (or (+eshell--running-p) +eshell--queue)
         t)))

(defun +eshell--running-p ()
  "Return non-nil when the current eshell buffer is running a command."
  (or +eshell--current eshell-foreground-command))


;;; Running

(defun +eshell--result (item &rest props)
  "Call ITEM's callback with PROPS added to its description."
  (when-let* ((callback (plist-get item :callback)))
    (funcall callback (append props
                              (list :command (plist-get item :command)
                                    :directory (plist-get item :directory)
                                    :buffer (current-buffer))))))

(defun +eshell--input ()
  "Return the text typed at the prompt and not yet sent."
  (buffer-substring-no-properties eshell-last-output-end (point-max)))

(defun +eshell--start (item)
  "Run ITEM, a queued command, in the current eshell buffer."
  (let ((pending (+eshell--input)))
    (delete-region eshell-last-output-end (point-max))
    (+eshell--sync-directory (current-buffer) (plist-get item :directory))
    ;; `eshell-reset' in the directory change redraws the prompt.
    (delete-region eshell-last-output-end (point-max))
    (goto-char (point-max))
    ;; Set before sending: an internal command finishes inside
    ;; `eshell-send-input' itself.  The output starts on the line after it.
    (setq +eshell--current (append (list :pending pending
                                         :start (copy-marker (point)))
                                   item))
    (insert (plist-get item :command))
    (eshell-send-input)))

(defun +eshell--finished-h ()
  "Deliver the result of the API command that just finished, if any."
  (when-let* ((item +eshell--current))
    (let* ((start (plist-get item :start))
           (from (save-excursion (goto-char start) (forward-line 1) (point)))
           (output (buffer-substring-no-properties
                    (min from eshell-last-output-end) eshell-last-output-end)))
      (set-marker start nil)
      (setq +eshell--current nil)
      (+eshell--result item :status eshell-last-command-status :output output)
      (+eshell--schedule-dispatch (plist-get item :pending)))))

(defun +eshell--schedule-dispatch (&optional pending)
  "Restore PENDING input and start the next queued command, after this command."
  (when (timerp +eshell--dispatch-timer)
    (cancel-timer +eshell--dispatch-timer))
  (setq +eshell--dispatch-timer
        (run-at-time 0 nil #'+eshell--dispatch (current-buffer) pending)))

(defun +eshell--dispatch (buffer &optional pending)
  "In BUFFER, put PENDING input back and start the next queued command."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq +eshell--dispatch-timer nil)
      (cond
       ((+eshell--running-p))
       (+eshell--queue
        (let ((item (pop +eshell--queue)))
          ;; Pending input waits until the queue is empty.
          (when (and pending (not (string-empty-p pending)))
            (goto-char (point-max))
            (insert pending))
          (+eshell--start item)))
       ((and pending (not (string-empty-p pending)))
        (goto-char (point-max))
        (insert pending))))))

(defun +eshell-run (command &rest keys)
  "Run COMMAND in an eshell and return its buffer.
KEYS: :directory (default `default-directory') picks the target context and
the working directory; :callback is called with a result plist when the
command ends (see docs/design/command-shell.md); :display non-nil shows the
buffer as `+eshell/toggle' does.  A busy buffer queues the command."
  (let* ((directory (+eshell--directory (plist-get keys :directory)))
         (buffer (+eshell-buffer directory))
         (item (list :command command :directory directory
                     :callback (plist-get keys :callback))))
    (with-current-buffer buffer
      (setq +eshell--queue (append +eshell--queue (list item)))
      (unless (or (+eshell--running-p) (timerp +eshell--dispatch-timer))
        (+eshell--dispatch buffer)))
    (when (plist-get keys :display)
      (+eshell--display buffer))
    buffer))

(defun +eshell-run-sync (command &rest keys)
  "Run COMMAND in an eshell and return its result plist when it ends.
KEYS: :directory as in `+eshell-run'; :timeout in seconds, default
`+eshell-sync-timeout'.  On timeout the command is interrupted and
`+eshell-timeout' is signalled."
  (let* ((timeout (or (plist-get keys :timeout) +eshell-sync-timeout))
         (deadline (+ (float-time) timeout))
         result
         (buffer (+eshell-run command
                              :directory (plist-get keys :directory)
                              :callback (lambda (r) (setq result r)))))
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (or result
        (progn
          (+eshell-cancel buffer)
          (signal '+eshell-timeout (list command timeout))))))

(defun +eshell-interrupt (&optional buffer)
  "Interrupt the command running in BUFFER; queued commands stay."
  (with-current-buffer (or buffer (current-buffer))
    (when eshell-foreground-command
      (eshell-interrupt-process))))

(defun +eshell-cancel (&optional buffer)
  "Drop BUFFER's queued commands, then interrupt the running one.
Each dropped command's callback gets :reason `cancelled'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((queue +eshell--queue))
      (setq +eshell--queue nil)
      (dolist (item queue)
        (+eshell--result item :status nil :reason 'cancelled)))
    (when-let* ((item +eshell--current))
      (if eshell-foreground-command
          (eshell-interrupt-process)
        (setq +eshell--current nil)
        (+eshell--result item :status nil :reason 'cancelled)))))

(defun +eshell--kill-buffer-h ()
  "Resolve every outstanding command of a buffer being killed."
  (when (timerp +eshell--dispatch-timer)
    (cancel-timer +eshell--dispatch-timer)
    (setq +eshell--dispatch-timer nil))
  (let ((items (append (and +eshell--current (list +eshell--current))
                       +eshell--queue)))
    (setq +eshell--current nil
          +eshell--queue nil)
    (dolist (item items)
      (when-let* ((start (plist-get item :start)))
        (set-marker start nil))
      (+eshell--result item :status nil :reason 'killed))))

(defun +eshell-setup-h ()
  "Install the command-shell hooks in an eshell buffer."
  (add-hook 'eshell-pre-command-hook #'+eshell--reset-output-h nil t)
  ;; Before the prompt is drawn, so the output ends where the prompt begins.
  (add-hook 'eshell-post-command-hook #'+eshell--finished-h -90 t)
  (add-hook 'kill-buffer-hook #'+eshell--kill-buffer-h nil t))

(after! eshell
  ;; Eshell walks this list itself rather than through `run-hooks', so a
  ;; buffer-local value's `t' would be called; the filter keeps its state per
  ;; buffer anyway.
  (add-hook 'eshell-preoutput-filter-functions #'+eshell-normalize-output)
  (add-hook 'eshell-mode-hook #'+eshell-setup-h))


;;; Interactive

(defun +eshell--display (buffer)
  "Show BUFFER at the bottom of the frame and select it."
  (select-window
   (or (get-buffer-window buffer)
       (display-buffer buffer '((display-buffer-at-bottom)
                                (window-height . 0.3))))))

(defun +eshell/toggle ()
  "Toggle the eshell of the current directory's target context.
Shown but not selected: select it.  Selected: close its window; the buffer
stays."
  (interactive)
  (let* ((directory (+eshell--directory nil))
         (name (+eshell-context-buffer-name directory))
         (window (get-buffer-window name)))
    (cond
     ((and window (eq window (selected-window)))
      (ignore-errors (delete-window window)))
     (window
      (unless (+eshell-busy-p (window-buffer window))
        (+eshell--sync-directory (window-buffer window) directory))
      (select-window window))
     (t
      (let ((buffer (+eshell-buffer directory)))
        (+eshell--display buffer))))))

(defvar +eshell-command-history nil
  "Commands read by `+eshell/run-command'.")

(defun +eshell/run-command (command)
  "Run COMMAND in the eshell of the project root, or the current directory."
  (interactive (list (read-shell-command "Run in eshell: " nil
                                         '+eshell-command-history)))
  (+eshell-run command
               :directory (or (and (fboundp '+project-current-root)
                                   (+project-current-root))
                              default-directory)
               :display t))

(defalias 'command-shell #'+eshell/toggle
  "Toggle the command shell; see `+eshell/toggle'.")
(defalias 'command-shell-run #'+eshell/run-command
  "Run a command in the command shell; see `+eshell/run-command'.")

(provide 'term/shell/eshell)

;;; term/shell/eshell.el ends here
