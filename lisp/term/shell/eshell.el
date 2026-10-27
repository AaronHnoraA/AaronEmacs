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
(require 'seq)
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
(declare-function eshell-kill-process "esh-proc" ())
(declare-function eshell-truncate-buffer "esh-mode" ())

(declare-function +eshell-context-buffer-name "term/shell/config"
                  (&optional directory task-name))
(declare-function +eshell--make-buffer "term/shell/config"
                  (name directory &optional force-new))
(declare-function +eshell--sync-directory "term/shell/config" (buffer directory))
(declare-function aaron-terminal-normalize-directory "aaron-funcs" (directory))
(declare-function +project-current-root "tools/project/config")
(declare-function +buffer-single-instance-p "ui/windows/config" (buffer))
(declare-function +windows/close-window "ui/windows/config" ())
(declare-function remote-require-on-client "remote-core"
                  (feature &optional filename noerror))
(declare-function aaron-ui-float-show "aaron-ui-float" (buffer &rest options))
(declare-function aaron-ui-float-hide "aaron-ui-float" (&optional panel))
(declare-function aaron-ui-float-panel-for-buffer "aaron-ui-float" (buffer))
(declare-function aaron-ui-float-panel-frame "aaron-ui-float" (panel))
(declare-function aaron-ui-float-visible-p "aaron-ui-float" (panel))
(defvar +eshell--routing nil
  "Non-nil while the generic buffer opener is routing Eshell to a float.")

(config-defvar +eshell-sync-timeout 600
  "Seconds `+eshell-run-sync' waits for a command before interrupting it."
  :type 'number
  :group 'eshell)

(config-defvar +eshell-command-timeout 600
  "Default seconds an asynchronous API command may run after it starts."
  :type 'number
  :group 'eshell)

(config-defvar +eshell-max-queued-commands 64
  "Maximum waiting API commands in one Eshell buffer."
  :type 'integer
  :group 'eshell)

(config-defvar +eshell-max-output-chars 262144
  "Maximum output characters retained for one API command."
  :type 'integer
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

(defvar-local +eshell--command-timer nil
  "Timer that bounds the current API command, or nil.")

(defvar-local +eshell--pending-restore nil
  "Prompt input to restore after an API command is aborted.")

(defvar-local +eshell--tearing-down nil
  "Non-nil while this Eshell buffer is being killed.")

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
    (when (and +eshell--current
               (> (length string) (max 1 +eshell-max-output-chars)))
      (setq +eshell--current (plist-put +eshell--current :truncated t)
            string (substring string (- (length string)
                                        (max 1 +eshell-max-output-chars)))))
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

(defun +eshell-buffer (&optional directory task-name)
  "Return the eshell buffer of DIRECTORY's target context.
The buffer is created when missing and not shown.  An idle buffer switches
to DIRECTORY; a busy buffer keeps its directory until queued work starts.
TASK-NAME gives an automation task its own named buffer and queue."
  (remote-require-on-client 'eshell)
  (let* ((directory (+eshell--directory directory))
         (name (+eshell-context-buffer-name directory task-name))
         (existing (get-buffer name))
         (buffer (if (and existing
                          (with-current-buffer existing
                            (derived-mode-p 'eshell-mode)))
                     existing
                   (+eshell--make-buffer name directory))))
    (unless (+eshell-busy-p buffer)
      (+eshell--sync-directory buffer directory))
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
    (condition-case err
        (funcall callback (append props
                                  (when (plist-get item :truncated)
                                    (list :truncated t))
                                  (list :command (plist-get item :command)
                                        :name (plist-get item :name)
                                        :directory (plist-get item :directory)
                                        :buffer (current-buffer))))
      ((error quit)
       (message "Eshell command callback failed: %S" err)))))

(defun +eshell--output-start (item)
  "Return the first output position of ITEM in this buffer."
  (save-excursion
    (goto-char (plist-get item :start))
    (forward-line 1)
    (point)))

(defun +eshell--output (item)
  "Return ITEM's currently visible output."
  (let ((from (+eshell--output-start item)))
    (buffer-substring-no-properties
     (min from eshell-last-output-end) eshell-last-output-end)))

(defun +eshell--bound-output-h ()
  "Keep the current API command's displayed output within its character cap."
  (when-let* ((item +eshell--current)
              (start (plist-get item :start)))
    (let* ((from (+eshell--output-start item))
           (limit (max 1 +eshell-max-output-chars))
           (drop (- eshell-last-output-end from limit)))
      (when (> drop 0)
        (setq +eshell--current (plist-put +eshell--current :truncated t))
        (let ((inhibit-read-only t))
          (delete-region from (+ from drop)))))))

(defun +eshell--truncate-history-h ()
  "Bound ordinary Eshell scrollback between API commands."
  (unless +eshell--current
    (eshell-truncate-buffer)))

(defun +eshell--cancel-command-timer ()
  "Cancel the active command's timeout timer."
  (when (timerp +eshell--command-timer)
    (cancel-timer +eshell--command-timer)
    (setq +eshell--command-timer nil)))

(defun +eshell--input ()
  "Return the text typed at the prompt and not yet sent."
  (buffer-substring-no-properties eshell-last-output-end (point-max)))

(defun +eshell--start (item)
  "Run ITEM, a queued command, in the current eshell buffer."
  (let ((pending (+eshell--input))
        (cleared nil)
        start)
    (condition-case err
        (progn
          (delete-region eshell-last-output-end (point-max))
          (setq cleared t)
          (+eshell--sync-directory (current-buffer) (plist-get item :directory))
          ;; `eshell-reset' in the directory change redraws the prompt.
          (delete-region eshell-last-output-end (point-max))
          (goto-char (point-max))
          ;; Set before sending: an internal command finishes inside
          ;; `eshell-send-input' itself.  The output starts on the line after it.
          (setq start (copy-marker (point))
                +eshell--current (append (list :pending pending :start start)
                                          item)
                +eshell--command-timer
                (run-at-time (plist-get item :timeout) nil #'+eshell--timeout
                             (current-buffer) +eshell--current))
          (insert (plist-get item :command))
          (eshell-send-input))
      ((error quit)
       (unless (and start (null (marker-buffer start)))
         (+eshell--cancel-command-timer)
         (when start (set-marker start nil))
         (setq +eshell--current nil)
         (let ((foreground eshell-foreground-command))
           (when cleared
             (setq +eshell--pending-restore pending))
           (when foreground
             (condition-case stop-error
                 (eshell-kill-process)
               (error (message "Eshell process stop failed: %S" stop-error))))
           (unless foreground
             (setq +eshell--pending-restore nil)
             (+eshell--schedule-dispatch (and cleared pending))))
         (+eshell--result item :status nil :reason 'error
                          :error (error-message-string err)))))))

(defun +eshell--finished-h ()
  "Deliver the result of the API command that just finished, if any."
  (if-let* ((item +eshell--current))
      (let ((output (+eshell--output item)))
        (+eshell--cancel-command-timer)
        (set-marker (plist-get item :start) nil)
        (setq +eshell--current nil)
        (+eshell--result item :status eshell-last-command-status :output output)
        (eshell-truncate-buffer)
        (+eshell--schedule-dispatch (plist-get item :pending)))
    (when (or +eshell--pending-restore +eshell--queue)
      (let ((pending +eshell--pending-restore))
        (setq +eshell--pending-restore nil)
        (+eshell--schedule-dispatch pending)))))

(defun +eshell--abort-current (reason)
  "Resolve the current API command with REASON and stop its process."
  (when-let* ((item +eshell--current))
    (let ((output (+eshell--output item))
          (foreground eshell-foreground-command))
      (+eshell--cancel-command-timer)
      (set-marker (plist-get item :start) nil)
      (setq +eshell--current nil
            +eshell--pending-restore (plist-get item :pending))
      (if foreground
          (condition-case err
              (if (eq reason 'timeout)
                  (eshell-kill-process)
                (eshell-interrupt-process))
            (error (message "Eshell process stop failed: %S" err)))
        (let ((pending +eshell--pending-restore))
          (setq +eshell--pending-restore nil)
          (+eshell--schedule-dispatch pending)))
      (+eshell--result item :status nil :reason reason :output output))))

(defun +eshell--timeout (buffer item)
  "Time out ITEM in BUFFER if it is still the active command."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq +eshell--current item)
        (+eshell--abort-current 'timeout)))))

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
buffer as `+eshell/toggle' does; :name gives a task its own named buffer and
queue; :timeout bounds execution time.  A busy
buffer queues the command.  This function returns before dispatching it."
  (unless (and (stringp command) (not (string-empty-p command)))
    (user-error "Eshell command must be a nonempty string"))
  (let* ((timeout (or (plist-get keys :timeout) +eshell-command-timeout))
         (_ (unless (and (numberp timeout) (> timeout 0))
              (user-error "Eshell timeout must be positive")))
         (directory (+eshell--directory (plist-get keys :directory)))
         (task-name (plist-get keys :name))
         (buffer (+eshell-buffer directory task-name))
         (item (list :command command :directory directory :name task-name
                     :callback (plist-get keys :callback)
                     :token (plist-get keys :token)
                     :timeout timeout)))
    (with-current-buffer buffer
      (when +eshell--tearing-down
        (user-error "Eshell buffer is closing"))
      (when (>= (length +eshell--queue)
                (max 1 +eshell-max-queued-commands))
        (user-error "Eshell command queue is full"))
      (setq +eshell--queue (append +eshell--queue (list item)))
      (unless (or (+eshell--running-p) (timerp +eshell--dispatch-timer))
        (+eshell--schedule-dispatch)))
    (when (plist-get keys :display)
      (+eshell--display buffer))
    buffer))

(defun +eshell-build-task-name (directory)
  "Return a readable, stable task label for DIRECTORY's build session.
The path digest prevents projects with the same basename from sharing a queue."
  (let* ((root (directory-file-name (+eshell--directory directory)))
         (label (file-name-nondirectory root)))
    (format "build-%s-%s"
            (if (string-empty-p label) "root" label)
            (substring (secure-hash 'sha1 root) 0 8))))

(defun +eshell-build-root (source-directory markers)
  "Find the nearest directory with one of MARKERS inside the source project.
When there is no marker, use SOURCE-DIRECTORY, so a single-file build writes
its output beside the source without searching unrelated ancestors."
  (require 'project)
  (let* ((source-directory (file-name-as-directory
                            (expand-file-name source-directory)))
         (project (project-current nil source-directory))
         (project-root (if project
                           (file-name-as-directory (project-root project))
                         source-directory))
         (cursor source-directory)
         found)
    (while (and cursor (not found)
                (string-prefix-p project-root cursor))
      (when (cl-some (lambda (marker)
                       (file-regular-p (expand-file-name marker cursor)))
                     markers)
        (setq found cursor))
      (unless found
        (setq cursor (unless (equal cursor project-root)
                       (file-name-directory (directory-file-name cursor))))))
    (or found source-directory)))

(defun +eshell-build-output-file (source)
  "Return SOURCE's simple, local executable name ending in .out."
  (concat (file-name-base source) ".out"))

(defun +eshell-run-build (build-command &rest keys)
  "Queue BUILD-COMMAND in a project Eshell, optionally followed by :run.
KEYS also accept :directory, :name, :timeout, :display and :callback as in
`+eshell-run'.  :run executes only after a successful build.  The task name
defaults to one stable build session per project directory.  Display defaults
to t because this is an explicit user build action."
  (unless (and (stringp build-command)
               (not (string-empty-p (string-trim build-command))))
    (user-error "Eshell build command must be a nonempty string"))
  (let* ((directory (+eshell--directory (plist-get keys :directory)))
         (run (plist-get keys :run))
         (name (or (plist-get keys :name)
                   (+eshell-build-task-name directory)))
         (display (if (plist-member keys :display)
                      (plist-get keys :display)
                    t)))
    (when (and run (or (not (stringp run))
                       (string-empty-p (string-trim run))))
      (user-error "Eshell run command must be a nonempty string"))
    (+eshell-run (if run (format "%s && %s" build-command run)
                   build-command)
                 :directory directory :name name :display display
                 :timeout (plist-get keys :timeout)
                 :callback (plist-get keys :callback))))

(defun +eshell-run-sync (command &rest keys)
  "Run COMMAND in an eshell and return its result plist when it ends.
KEYS: :directory and :name as in `+eshell-run'; :timeout in seconds, default
`+eshell-sync-timeout'.  On timeout the command is interrupted and
`+eshell-timeout' is signalled.  This legacy wrapper is batch-only."
  (unless noninteractive
    (user-error "Use +eshell-run in interactive Emacs; sync would block the UI"))
  (let* ((timeout (or (plist-get keys :timeout) +eshell-sync-timeout))
         (token (make-symbol "eshell-sync"))
         (deadline (+ (float-time) timeout))
         result
         (buffer (+eshell-run command
                              :directory (plist-get keys :directory)
                              :name (plist-get keys :name)
                              :timeout timeout
                              :token token
                              :callback (lambda (r) (setq result r)))))
    (while (and (not result) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond
     ((eq (plist-get result :reason) 'timeout)
      (signal '+eshell-timeout (list command timeout)))
     (result result)
     (t
      (+eshell--cancel-token buffer token 'timeout)
      (signal '+eshell-timeout (list command timeout))))))

(defun +eshell--cancel-token (buffer token reason)
  "Resolve only TOKEN's command in BUFFER with REASON."
  (with-current-buffer buffer
    (let (remaining)
      (dolist (item +eshell--queue)
        (if (eq (plist-get item :token) token)
            (+eshell--result item :status nil :reason reason)
          (push item remaining)))
      (setq +eshell--queue (nreverse remaining)))
    (when (and +eshell--current
               (eq (plist-get +eshell--current :token) token))
      (+eshell--abort-current reason))))

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
    (+eshell--abort-current 'cancelled)))

(defun +eshell--kill-buffer-h ()
  "Resolve every outstanding command of a buffer being killed."
  (setq +eshell--tearing-down t)
  (when (timerp +eshell--dispatch-timer)
    (cancel-timer +eshell--dispatch-timer)
    (setq +eshell--dispatch-timer nil))
  (+eshell--cancel-command-timer)
  (let ((items (append (and +eshell--current (list +eshell--current))
                       +eshell--queue)))
    (setq +eshell--current nil
          +eshell--queue nil
          +eshell--pending-restore nil)
    (dolist (item items)
      (when-let* ((start (plist-get item :start)))
        (set-marker start nil))
      (+eshell--result item :status nil :reason 'killed))))

(defun +eshell--disable-undo-h ()
  "Keep a command's output, and the scrollback it trims, out of undo.
From Doom: otherwise every output chunk and every truncated line stays in
the undo history of a buffer that lives for weeks."
  (buffer-disable-undo))

(defun +eshell--enable-undo-h ()
  "Record undo again, from an empty history, for editing the next input."
  (buffer-enable-undo))

(defun +eshell-setup-h ()
  "Install the command-shell hooks in an eshell buffer."
  (add-hook 'eshell-pre-command-hook #'+eshell--reset-output-h nil t)
  (add-hook 'eshell-pre-command-hook #'+eshell--disable-undo-h nil t)
  (setq-local eshell-buffer-maximum-lines 5000)
  (add-hook 'eshell-output-filter-functions #'+eshell--bound-output-h 90 t)
  (add-hook 'eshell-output-filter-functions #'+eshell--truncate-history-h 95 t)
  ;; Before the prompt is drawn, so the output ends where the prompt begins.
  (add-hook 'eshell-post-command-hook #'+eshell--finished-h -90 t)
  ;; After the final trim above, which would otherwise be recorded.
  (add-hook 'eshell-post-command-hook #'+eshell--enable-undo-h -80 t)
  (add-hook 'kill-buffer-hook #'+eshell--kill-buffer-h nil t)
  ;; Perspective protects a workspace's last buffer.  An Eshell float can be
  ;; that buffer, which silently makes `exit' and the red close button no-ops.
  ;; Keep the process and other kill queries, but let Eshell sessions end.
  (setq-local kill-buffer-query-functions
              (remq 'persp-maybe-kill-buffer
                    (copy-sequence kill-buffer-query-functions)))
  ;; Company-box's own child frame can fail while Eshell already lives in a
  ;; child frame.  Keep completion, but use Company's in-buffer popup here.
  (add-hook 'company-mode-hook #'+eshell--disable-company-box-h 100 t)
  (+eshell--disable-company-box-h)
  (local-set-key (kbd "M-w") #'+eshell/hide-or-close-window))

(defun +eshell--disable-company-box-h ()
  "Keep Company completion usable inside the Eshell child frame."
  (when (derived-mode-p 'eshell-mode)
    (when (bound-and-true-p company-box-mode)
      (company-box-mode -1))
    (when (boundp 'company-frontends)
      (setq-local company-frontends
                  (cons 'company-pseudo-tooltip-unless-just-one-frontend
                        (cl-remove-if
                         (lambda (frontend)
                           (memq frontend
                                 '(company-box-frontend
                                   company-childframe-frontend
                                   company-childframe-unless-just-one-frontend
                                   company-pseudo-tooltip-frontend
                                   company-pseudo-tooltip-unless-just-one-frontend)))
                         company-frontends))))))

(after! eshell
  ;; Eshell walks this list itself rather than through `run-hooks', so a
  ;; buffer-local value's `t' would be called; the filter keeps its state per
  ;; buffer anyway.
  (add-hook 'eshell-preoutput-filter-functions #'+eshell-normalize-output)
  (add-hook 'eshell-mode-hook #'+eshell-setup-h))


;;; Interactive

(defun +eshell--visible-floating-frame (buffer)
  "Return BUFFER's visible Eshell child frame, or nil."
  (when (and (buffer-live-p buffer) (featurep 'aaron-ui-float))
    (when-let* ((panel (aaron-ui-float-panel-for-buffer buffer))
                ((aaron-ui-float-visible-p panel)))
      (aaron-ui-float-panel-frame panel))))

(defun +eshell--hide-floating (buffer)
  "Hide BUFFER's Eshell child frame and return focus to its parent."
  (when-let* ((panel (aaron-ui-float-panel-for-buffer buffer)))
    (aaron-ui-float-hide panel)))

(defun +eshell/hide-or-close-window ()
  "Hide the selected Eshell float for Cmd-W; otherwise close its window."
  (interactive)
  (if (eq (selected-frame)
          (+eshell--visible-floating-frame (current-buffer)))
      (+eshell--hide-floating (current-buffer))
    (+windows/close-window)))

(defun +eshell--vacate-ordinary-windows (buffer)
  "Remove BUFFER from ordinary windows after its float is shown.
This repairs sessions displayed by older Eshell startup paths without
deleting or splitting any parent window."
  (dolist (window (get-buffer-window-list buffer nil t))
    (when (and (window-live-p window)
               (not (frame-parent (window-frame window)))
               (not (window-dedicated-p window)))
      (let ((replacement
             (seq-find
              (lambda (candidate)
                (and (buffer-live-p candidate)
                     (not (eq candidate buffer))
                     (not (and (fboundp '+buffer-single-instance-p)
                               (+buffer-single-instance-p candidate)))
                     (with-current-buffer candidate
                       (not (derived-mode-p 'eshell-mode)))))
              (mapcar #'car (window-prev-buffers window)))))
        (set-window-buffer window
                           (or replacement (get-buffer-create "*scratch*")))))))

(defun +eshell--display (buffer)
  "Show BUFFER in a focused child frame, or a bottom window on TTY."
  (let ((floating
         (when (and (display-graphic-p)
                    (remote-require-on-client 'aaron-ui-float nil t))
           (condition-case err
               (let* ((panel
                       (aaron-ui-float-show
                        buffer :key buffer :title (buffer-name buffer)
                        :width 100 :height 26 :kill-on-close t))
                      (frame (aaron-ui-float-panel-frame panel)))
                 (when (frame-live-p frame)
                   (+eshell--vacate-ordinary-windows buffer))
                 frame)
             (error
              (message "Eshell floating display failed: %S" err)
              nil)))))
    (if floating
        (progn
          (select-frame floating)
          (select-frame-set-input-focus floating)
          (frame-selected-window floating))
      (select-window
       (or (get-buffer-window buffer)
           (let ((+eshell--routing t))
             (display-buffer buffer '((display-buffer-at-bottom)
                                      (window-height . 0.3)))))))))

(defun +eshell--route-buffer-p (buffer-or-name &optional _action)
  "Return non-nil when BUFFER-OR-NAME should open as an Eshell float."
  (when-let* ((buffer (and (or (bufferp buffer-or-name)
                               (stringp buffer-or-name))
                           (get-buffer buffer-or-name))))
    (and (display-graphic-p)
         (not +eshell--routing)
         (with-current-buffer buffer (derived-mode-p 'eshell-mode)))))

(defun +eshell--switch-to-buffer-a (original buffer-or-name &rest args)
  "Route Eshell BUFFER-OR-NAME to its float before calling ORIGINAL."
  (if (+eshell--route-buffer-p buffer-or-name)
      (let ((buffer (get-buffer buffer-or-name))
            (+eshell--routing t))
        (+eshell--display buffer)
        buffer)
    (apply original buffer-or-name args)))

(defun +eshell--display-buffer-action (buffer _alist)
  "Show Eshell BUFFER as the selected floating display action."
  (let ((+eshell--routing t))
    (+eshell--display buffer)))

(defun +eshell--ibuffer-visit-a (original &rest args)
  "Open Eshell rows as floats, leaving Ibuffer's windows intact."
  (let ((buffer (ignore-errors (ibuffer-current-buffer t))))
    (if (+eshell--route-buffer-p buffer)
        (let ((+eshell--routing t))
          (+eshell--display buffer)
          buffer)
      (apply original args))))

(dolist (function '(switch-to-buffer switch-to-buffer-other-window
                    switch-to-buffer-other-frame))
  (advice-add function :around #'+eshell--switch-to-buffer-a))

(add-to-list 'display-buffer-alist
             '(+eshell--route-buffer-p +eshell--display-buffer-action))

(with-eval-after-load 'ibuffer
  (dolist (function '(ibuffer-visit-buffer
                      ibuffer-visit-buffer-other-window
                      ibuffer-visit-buffer-other-frame))
    (advice-add function :around #'+eshell--ibuffer-visit-a)))

(defun +eshell/toggle-current ()
  "Toggle the eshell of the current directory's target context.
Shown but not selected: select it.  Selected: hide its view; the buffer stays."
  (interactive)
  (let* ((directory (+eshell--directory nil))
         (name (+eshell-context-buffer-name directory))
         (buffer (get-buffer name))
         (floating (and buffer (+eshell--visible-floating-frame buffer)))
         (window (get-buffer-window name)))
    (cond
     ((and floating (eq floating (selected-frame)))
      (+eshell--hide-floating buffer))
     (floating
      (unless (+eshell-busy-p buffer)
        (+eshell--sync-directory buffer directory))
      (select-frame floating)
      (select-frame-set-input-focus floating))
     ((and window (eq window (selected-window)))
      (ignore-errors (delete-window window)))
     (window
      (unless (+eshell-busy-p (window-buffer window))
        (+eshell--sync-directory (window-buffer window) directory))
      (select-window window))
     (t
      (let ((buffer (+eshell-buffer directory)))
        (+eshell--display buffer))))))

(defun +eshell--selector-candidates (&optional exclude)
  "Return searchable Eshell choices, omitting EXCLUDE when given."
  (append
   (mapcar (lambda (buffer)
             (cons (format "Eshell · %s · %s"
                           (buffer-name buffer)
                           (abbreviate-file-name
                            (buffer-local-value 'default-directory buffer)))
                   buffer))
           (seq-filter (lambda (buffer)
                         (and (not (eq buffer exclude))
                              (with-current-buffer buffer
                                (derived-mode-p 'eshell-mode))))
                       (buffer-list)))
   (when exclude (list (cons "－ 隐藏当前 Eshell" :hide)))
   (list (cons "＋ 新建 Eshell" :new))))

(defun +eshell--next-session-name (directory)
  "Return an unused readable session name for DIRECTORY's target."
  (let ((index 1))
    (while (get-buffer (+eshell-context-buffer-name
                        directory (format "session-%d" index)))
      (cl-incf index))
    (format "session-%d" index)))

(defun +eshell/new (&optional directory name)
  "Create a new Eshell session in DIRECTORY and display it.
NAME labels this session; without one, use the next available session number."
  (interactive
   (list nil (when current-prefix-arg (read-string "Eshell 会话名: "))))
  (let* ((directory (+eshell--directory directory))
         (name (or name (+eshell--next-session-name directory)))
         (buffer (+eshell-buffer directory name)))
    (+eshell--display buffer)
    buffer))

(defun +eshell/toggle ()
  "Search existing Eshell sessions, create one, or hide the current float.
The picker follows Cmd-T's switch/new pattern without changing Portal's
terminal selector.  Every selected Eshell buffer opens in its float."
  (interactive)
  (let* ((directory default-directory)
         (current (and (derived-mode-p 'eshell-mode)
                       (+eshell--visible-floating-frame (current-buffer))
                       (eq (selected-frame)
                           (+eshell--visible-floating-frame (current-buffer)))
                       (current-buffer)))
         (candidates (+eshell--selector-candidates current))
         (vertico-sort-override-function #'identity)
         (choice (completing-read "Eshell (选择或新建): " candidates nil t))
         (selected (cdr (assoc choice candidates))))
    (pcase selected
      (:new (+eshell/new directory))
      (:hide (+eshell--hide-floating current))
      ((pred bufferp) (+eshell--display selected))
      (_ (user-error "No Eshell session selected")))))

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
  "Choose or create a command shell; see `+eshell/toggle'.")
(defalias 'command-shell-run #'+eshell/run-command
  "Run a command in the command shell; see `+eshell/run-command'.")

(provide 'term/shell/eshell)

;;; term/shell/eshell.el ends here
