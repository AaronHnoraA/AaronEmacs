;;; tools/output/config.el --- Unified output buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)
(require 'aaron-funcs)
(require 'ui/tabbar/config)
(require 'remote-framework)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function compilation-find-buffer "compile")
(declare-function compilation-next-error "compile" (n &optional reset))
(declare-function compilation-previous-error "compile" (n &optional reset))
(declare-function first-error "compile")
(declare-function kill-compilation "compile")
(declare-function recompile "compile" (&optional edit-command))

(defvar compilation-mode-map)
(defvar compilation-scroll-output)

(defgroup +output nil
  "Unified output helpers for compilation-like buffers."
  :group 'tools)

(defvar +output-compilation-context nil
  "Dynamically bound (FRAME . KIND) while a workflow starts compilation.")

(defvar-local +output-owner-frame nil
  "Frame that owns this workflow output buffer.")
(put '+output-owner-frame 'permanent-local t)

(defvar-local +output-workflow-kind nil
  "Kind of workflow that created this output buffer.")
(put '+output-workflow-kind 'permanent-local t)

(config-defvar +output-interesting-buffer-regexp nil
  "Regexp matching output buffers managed by `+output'."
  :type 'regexp
  :group 'output)

(defun +output-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a managed output buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'compilation-mode)
         (or +output-workflow-kind
             (and (stringp +output-interesting-buffer-regexp)
                  (string-match-p +output-interesting-buffer-regexp
                                  (buffer-name)))))))

(defun +output--adopt-task (task frame kind)
  "Track TASK's output as KIND in FRAME and return its buffer."
  (let ((buffer (remote-task-buffer task)))
    (with-current-buffer buffer
      (setq-local +output-owner-frame frame
                  +output-workflow-kind kind)
      ;; Preserve `remote-task-run's own `g' binding and error parser.
      (+output--setup-buffer))
    (when (frame-live-p frame)
      (set-frame-parameter frame '+output-last-buffer buffer))
    buffer))

(defun +output-compile (command kind &optional environment)
  "Run shell COMMAND in KIND's output buffer for this frame.
Use the owning Remote workspace's task lifecycle when one applies;
otherwise keep Emacs' native `compile' workflow.  ENVIRONMENT is an
alist of project overrides for the workspace task."
  (let* ((frame (selected-frame))
         (+output-compilation-context (cons frame kind))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s F%d*" kind (+frame-short-id frame))))
         (existing-workspace (remote-workspace-for-path default-directory))
         (new-workspace
          (and (not existing-workspace)
               (remote-fs-file-name-p default-directory)
               (remote-workspace-open
                default-directory :adapter "process" :capability 'process-async
                :load-environment t)))
         (workspace (or existing-workspace new-workspace)))
    (if workspace
        (condition-case error
            (+output--adopt-task
             (remote-task-run
              (list "sh" "-c" command)
              :workspace workspace
              :name (format "%s F%d" kind (+frame-short-id frame))
              :directory default-directory
              :environment environment
              :display t)
             frame kind)
          (error
           (when new-workspace
             (remote-workspace-close new-workspace 'task-start-failed))
           (signal (car error) (cdr error))))
      (let ((buffer (compile command)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local +output-owner-frame frame
                        +output-workflow-kind kind))
          (set-frame-parameter frame '+output-last-buffer buffer))
        buffer))))

(defun +output-last-run (kind)
  "Return this frame's last (COMMAND DIRECTORY ENV) for workflow KIND."
  (alist-get kind (frame-parameter nil '+output-last-runs)))

(defun +output-record-run (kind command directory env)
  "Remember a successful KIND launch in this frame."
  (set-frame-parameter
   nil '+output-last-runs
   (cons (cons kind (list command directory env))
         (assq-delete-all kind
                          (copy-sequence
                           (frame-parameter nil '+output-last-runs))))))

(defun +output--buffer ()
  "Return the active output buffer or signal an error."
  (let* ((frame (selected-frame))
         (last (frame-parameter frame '+output-last-buffer))
         (buffer
          (cond
           ((+output-buffer-p (current-buffer)) (current-buffer))
           ((buffer-live-p last) last)
           (t
            (seq-find
             (lambda (candidate)
               (and (+output-buffer-p candidate)
                    (eq (buffer-local-value '+output-owner-frame candidate)
                        frame)))
             (+frame-buffer-list frame t))))))
    (unless buffer
      (user-error "No output buffer is available"))
    (when (and (not (buffer-live-p last))
               (eq (buffer-local-value '+output-owner-frame buffer) frame))
      (set-frame-parameter frame '+output-last-buffer buffer))
    buffer))

(defun +output--kind (&optional buffer)
  "Return a short kind label for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     (+output-workflow-kind +output-workflow-kind)
     ((string-match-p "\\*test\\*" (buffer-name)) "test")
     ((string-match-p "\\*task\\*" (buffer-name)) "task")
     ((string-match-p "\\*build\\*" (buffer-name)) "build")
     ((string-match-p "\\*run\\*" (buffer-name)) "run")
     (t "compile"))))

(defun +output--status (&optional buffer)
  "Return a readable process status string for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if (remote-task-p remote-task-instance)
        (symbol-name (remote-task-state remote-task-instance))
      (let ((proc (get-buffer-process (current-buffer))))
        (if (and proc (process-live-p proc)) "running" "idle")))))

(defun +output--header-line ()
  "Return the header line for the current output buffer."
  (format " %s [%s]  %s  |  R rerun  K stop  s auto-scroll  n/p next-prev error"
          (capitalize (+output--kind))
          (+output--status)
          (abbreviate-file-name default-directory)))

(defun +output--setup-buffer ()
  "Track and decorate the current compilation buffer."
  (when +output-compilation-context
    (setq-local +output-owner-frame (car +output-compilation-context)
                +output-workflow-kind (cdr +output-compilation-context)))
  (when (+output-buffer-p)
    (setq-local header-line-format '(:eval (+output--header-line)))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "R") #'+output/rerun)
    (local-set-key (kbd "K") #'+output/stop)
    (local-set-key (kbd "s") #'+output/toggle-scroll)
    (local-set-key (kbd "n") #'+output/next-error)
    (local-set-key (kbd "p") #'+output/previous-error)))

(defun +output--track-started (_process)
  "Remember a successfully started compilation in its owning frame."
  (when (+output-buffer-p)
    (let ((frame (if (frame-live-p +output-owner-frame)
                     +output-owner-frame
                   (selected-frame))))
      (set-frame-parameter frame '+output-last-buffer (current-buffer)))))

(defun +output--forget-frame (frame)
  "Release output buffers' reference to deleted FRAME.
The workspace, rather than the frame, owns a running Remote task."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value '+output-owner-frame buffer) frame)
      (with-current-buffer buffer
        (setq-local +output-owner-frame nil)))))

(defun +output/switch ()
  "Switch to the most recent output buffer."
  (interactive)
  (pop-to-buffer (+output--buffer)
                 '(nil (reusable-frames . nil))))

(defun +output/rerun ()
  "Re-run the current or most recent output command."
  (interactive)
  (with-current-buffer (+output--buffer)
    (if (remote-task-p remote-task-instance)
        (+output--adopt-task
         (remote-task-rerun remote-task-instance)
         (if (frame-live-p +output-owner-frame)
             +output-owner-frame
           (selected-frame))
         +output-workflow-kind)
      (recompile))))

(defun +output/stop ()
  "Stop the current or most recent output process."
  (interactive)
  (with-current-buffer (+output--buffer)
    (if (remote-task-p remote-task-instance)
        (if (eq (remote-task-state remote-task-instance) 'running)
            (remote-task-cancel remote-task-instance)
          (user-error "No active output process"))
      (if-let* ((proc (get-buffer-process (current-buffer)))
                ((process-live-p proc)))
          (kill-compilation)
        (user-error "No active output process")))))

(defun +output/next-error ()
  "Jump to the next error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (+output--buffer)
    (compilation-next-error 1)))

(defun +output/previous-error ()
  "Jump to the previous error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (+output--buffer)
    (compilation-previous-error 1)))

(defun +output/first-error ()
  "Jump to the first error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (+output--buffer)
    (first-error)))

(defun +output/toggle-scroll ()
  "Toggle `compilation-scroll-output' for the active output buffer."
  (interactive)
  (with-current-buffer (+output--buffer)
    (setq-local compilation-scroll-output
                (not (if (local-variable-p 'compilation-scroll-output)
                         compilation-scroll-output
                       nil)))
    (message "Output auto-scroll %s"
             (if compilation-scroll-output "enabled" "disabled"))))

(transient-define-prefix output-menu ()
  "Open output controls for the current Emacs frame."
  [["Output"
    ("o" "latest output" +output/switch)
    ("r" "rerun" +output/rerun)
    ("k" "stop" +output/stop)
    ("n" "next error" +output/next-error)
    ("p" "previous error" +output/previous-error)
    ("f" "first error" +output/first-error)
    ("s" "toggle auto scroll" +output/toggle-scroll)]])

(defalias 'output-switch #'+output/switch
  "Show this frame's latest available output buffer.")

(add-hook 'compilation-mode-hook #'+output--setup-buffer)
(add-hook 'compilation-start-hook #'+output--track-started)
(add-hook 'delete-frame-functions #'+output--forget-frame)

(define-leader-key!
  "r o" '(:def output-menu :which-key "output")
  "r O" '(:def output-switch :which-key "latest output"))

(provide 'tools/output/config)
;;; tools/output/config.el ends here
