;;; document-run.el --- Compile actions in Noema notes -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'subr-x)

(declare-function +output-compile "tools/output/config" (command kind &optional environment))
(declare-function remote-gateway--random-id "remote-gateway" (&optional prefix))
(declare-function remote-task-state "remote-task" (task))
(declare-function remote-task-exit-code "remote-task" (task))
(declare-function remote-task-cancel "remote-task" (task &optional reason))
(defvar remote-task-instance)

(cl-defstruct (+noema--document-run (:constructor +noema--document-run-create))
  id note command directory frame buffer process task status exit-code error stop-requested)

(defvar +noema--document-runs nil)

(defun +noema--document-run-state (run)
  "Read RUN's current status from its output owner."
  (let ((task (+noema--document-run-task run))
        (process (+noema--document-run-process run)))
    (when (or task (eq (+noema--document-run-status run) 'running))
      (cond
       (task
        (setf (+noema--document-run-status run) (remote-task-state task)
              (+noema--document-run-exit-code run) (remote-task-exit-code task)))
       ((and process (not (process-live-p process)))
        (let ((exit (process-exit-status process)))
          (setf (+noema--document-run-exit-code run) exit
                (+noema--document-run-status run)
                (cond ((and (eq (process-status process) 'exit) (zerop exit)) 'succeeded)
                      ((+noema--document-run-stop-requested run) 'cancelled)
                      (t 'failed)))))
       ((not process)
        (setf (+noema--document-run-status run) 'failed
              (+noema--document-run-error run) "The command process was unavailable.")))))
  (+noema--document-run-status run))

(defun +noema--document-run-output (run)
  "Return the last 60 lines and at most 12,000 characters of RUN's buffer."
  (let ((buffer (+noema--document-run-buffer run)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-max))
            (forward-line -60)
            (let ((end (point-max))
                  (start (max (point) (- (point-max) 12000))))
              (mapconcat
               #'identity
               (cl-loop for from = start then to
                        while (< from end)
                        for to = (next-single-property-change from 'compilation-annotation nil end)
                        unless (get-text-property from 'compilation-annotation)
                        collect (buffer-substring-no-properties from to))
               ""))))))))

(defun +noema--document-run-snapshot (run)
  "Return the current RUN state to the document."
  (let* ((state (+noema--document-run-state run))
         (buffer (+noema--document-run-buffer run))
         (exit (+noema--document-run-exit-code run))
         (output (+noema--document-run-output run)))
    (append
     (list '(ok . t)
           (cons 'id (+noema--document-run-id run))
           (cons 'status (symbol-name state))
           (cons 'hasOutput (if (buffer-live-p buffer) t :json-false))
           (cons 'stopRequested (if (+noema--document-run-stop-requested run) t :json-false)))
     (and exit (list (cons 'exitCode exit)))
     (and output (list (cons 'output output)))
     (when-let* ((message (+noema--document-run-error run)))
       (list (cons 'message message))))))

(defun +noema--document-run-launch (run)
  "Read the native compile command and start RUN in its originating frame."
  (condition-case error
      (with-selected-frame (+noema--document-run-frame run)
        (let* ((default-directory (+noema--document-run-directory run))
               (command (compilation-read-command (+noema--document-run-command run))))
          (if (string-blank-p command)
              (setf (+noema--document-run-status run) 'cancelled)
            (require 'tools/output/config)
            (let* ((kind (format "Document run %s" (+noema--document-run-id run)))
                   (buffer (+output-compile command kind))
                   (task (buffer-local-value 'remote-task-instance buffer)))
              (setf (+noema--document-run-buffer run) buffer
                    (+noema--document-run-process run) (get-buffer-process buffer)
                    (+noema--document-run-task run) task
                    (+noema--document-run-status run) 'running)))))
    (quit (setf (+noema--document-run-status run) 'cancelled))
    (error (setf (+noema--document-run-status run) 'failed
                 (+noema--document-run-error run) (error-message-string error)))))

(defun +noema--document-run-start (params)
  "Reserve a run and queue a native compile prompt for PARAMS."
  (let ((command (alist-get 'command params))
        (directory (alist-get 'directory params))
        (note (alist-get 'note params))
        (frame (selected-frame)))
    (unless (and (stringp command) (not (string-blank-p command))
                 (<= (length command) 4096) (not (string-search (string 0) command))
                 (stringp note) (file-name-absolute-p note)
                 (not (file-remote-p note)) (file-regular-p note)
                 (stringp directory) (file-name-absolute-p directory)
                 (not (file-remote-p directory)) (file-directory-p directory))
      (user-error "A command and local working directory are required"))
    (when (or (active-minibuffer-window)
              (seq-some (lambda (run) (eq (+noema--document-run-status run) 'awaiting))
                        +noema--document-runs))
      (user-error "Finish the current Emacs prompt before starting a run"))
    (let* ((groups (seq-group-by
                    (lambda (run) (eq (+noema--document-run-state run) 'running))
                    +noema--document-runs))
           (active (alist-get t groups))
           (completed (alist-get nil groups)))
      (when (>= (length active) 24) (user-error "Too many document runs are active"))
      (let ((run (+noema--document-run-create
                  :id (remote-gateway--random-id "run-")
                  :note note
                  :command command :directory (file-name-as-directory directory)
                  :frame frame :status 'awaiting)))
        (setq +noema--document-runs (cons run (append active (seq-take completed 24))))
        (run-at-time 0 nil #'+noema--document-run-launch run)
        (+noema--document-run-snapshot run)))))

(defun +noema--document-run-api (params _client)
  "Route a document's explicit run action in PARAMS."
  (let ((action (alist-get 'action params))
        (id (alist-get 'id params))
        (note (alist-get 'note params)))
    (if (equal action "start")
        (+noema--document-run-start params)
      (let ((run (seq-find (lambda (candidate)
                            (and (equal (+noema--document-run-id candidate) id)
                                 (equal (+noema--document-run-note candidate) note)))
                           +noema--document-runs)))
        (cond
         ((and (not run) (equal action "status"))
          (list '(ok . t) (cons 'id id) '(status . "unavailable")
                '(hasOutput . :json-false) '(stopRequested . :json-false)))
         ((not run) (user-error "This document run is no longer available"))
         (t
          (pcase action
            ("status")
            ("open"
             (let ((buffer (+noema--document-run-buffer run)))
               (unless (buffer-live-p buffer) (user-error "The output buffer is closed"))
               (pop-to-buffer buffer)))
            ("stop"
             (when (eq (+noema--document-run-state run) 'running)
               (if-let* ((task (+noema--document-run-task run)))
                   (remote-task-cancel task)
                 (interrupt-process (+noema--document-run-process run)))
               (setf (+noema--document-run-stop-requested run) t)))
            (_ (user-error "Unknown document run action")))
          (+noema--document-run-snapshot run)))))))

(provide 'noema/bridge/document-run)
;;; document-run.el ends here
