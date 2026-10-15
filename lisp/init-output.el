;;; init-output.el --- Unified output buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)
(require 'init-funcs)
(require 'init-tabbar)
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

(defgroup my/output nil
  "Unified output helpers for compilation-like buffers."
  :group 'tools)

(defvar my/output-compilation-context nil
  "Dynamically bound (FRAME . KIND) while a workflow starts compilation.")

(defvar-local my/output-owner-frame nil
  "Frame that owns this workflow output buffer.")
(put 'my/output-owner-frame 'permanent-local t)

(defvar-local my/output-workflow-kind nil
  "Kind of workflow that created this output buffer.")
(put 'my/output-workflow-kind 'permanent-local t)

(config-defvar my/output-interesting-buffer-regexp nil
  "Regexp matching output buffers managed by `my/output'."
  :type 'regexp
  :group 'output)

(defun my/output-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a managed output buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'compilation-mode)
         (or my/output-workflow-kind
             (and (stringp my/output-interesting-buffer-regexp)
                  (string-match-p my/output-interesting-buffer-regexp
                                  (buffer-name)))))))

(defun my/output--adopt-task (task frame kind)
  "Track TASK's output as KIND in FRAME and return its buffer."
  (let ((buffer (remote-task-buffer task)))
    (with-current-buffer buffer
      (setq-local my/output-owner-frame frame
                  my/output-workflow-kind kind)
      ;; Preserve `remote-task-run's own `g' binding and error parser.
      (my/output--setup-buffer))
    (when (frame-live-p frame)
      (set-frame-parameter frame 'my/output-last-buffer buffer))
    buffer))

(defun my/output-compile (command kind &optional environment)
  "Run shell COMMAND in KIND's output buffer for this frame.
Use the owning Remote workspace's task lifecycle when one applies;
otherwise keep Emacs' native `compile' workflow.  ENVIRONMENT is an
alist of project overrides for the workspace task."
  (let* ((frame (selected-frame))
         (my/output-compilation-context (cons frame kind))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s F%d*" kind (my/frame-short-id frame))))
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
            (my/output--adopt-task
             (remote-task-run
              (list "sh" "-c" command)
              :workspace workspace
              :name (format "%s F%d" kind (my/frame-short-id frame))
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
            (setq-local my/output-owner-frame frame
                        my/output-workflow-kind kind))
          (set-frame-parameter frame 'my/output-last-buffer buffer))
        buffer))))

(defun my/output-last-run (kind)
  "Return this frame's last (COMMAND DIRECTORY ENV) for workflow KIND."
  (alist-get kind (frame-parameter nil 'my/output-last-runs)))

(defun my/output-record-run (kind command directory env)
  "Remember a successful KIND launch in this frame."
  (set-frame-parameter
   nil 'my/output-last-runs
   (cons (cons kind (list command directory env))
         (assq-delete-all kind
                          (copy-sequence
                           (frame-parameter nil 'my/output-last-runs))))))

(defun my/output--buffer ()
  "Return the active output buffer or signal an error."
  (let* ((frame (selected-frame))
         (last (frame-parameter frame 'my/output-last-buffer))
         (buffer
          (cond
           ((my/output-buffer-p (current-buffer)) (current-buffer))
           ((buffer-live-p last) last)
           (t
            (seq-find
             (lambda (candidate)
               (and (my/output-buffer-p candidate)
                    (eq (buffer-local-value 'my/output-owner-frame candidate)
                        frame)))
             (my/frame-buffer-list frame t))))))
    (unless buffer
      (user-error "No output buffer is available"))
    (when (and (not (buffer-live-p last))
               (eq (buffer-local-value 'my/output-owner-frame buffer) frame))
      (set-frame-parameter frame 'my/output-last-buffer buffer))
    buffer))

(defun my/output--kind (&optional buffer)
  "Return a short kind label for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     (my/output-workflow-kind my/output-workflow-kind)
     ((string-match-p "\\*test\\*" (buffer-name)) "test")
     ((string-match-p "\\*task\\*" (buffer-name)) "task")
     ((string-match-p "\\*build\\*" (buffer-name)) "build")
     ((string-match-p "\\*run\\*" (buffer-name)) "run")
     (t "compile"))))

(defun my/output--status (&optional buffer)
  "Return a readable process status string for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if (remote-task-p remote-task-instance)
        (symbol-name (remote-task-state remote-task-instance))
      (let ((proc (get-buffer-process (current-buffer))))
        (if (and proc (process-live-p proc)) "running" "idle")))))

(defun my/output--header-line ()
  "Return the header line for the current output buffer."
  (format " %s [%s]  %s  |  R rerun  K stop  s auto-scroll  n/p next-prev error"
          (capitalize (my/output--kind))
          (my/output--status)
          (abbreviate-file-name default-directory)))

(defun my/output--setup-buffer ()
  "Track and decorate the current compilation buffer."
  (when my/output-compilation-context
    (setq-local my/output-owner-frame (car my/output-compilation-context)
                my/output-workflow-kind (cdr my/output-compilation-context)))
  (when (my/output-buffer-p)
    (setq-local header-line-format '(:eval (my/output--header-line)))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "R") #'my/output-rerun)
    (local-set-key (kbd "K") #'my/output-stop)
    (local-set-key (kbd "s") #'my/output-toggle-scroll)
    (local-set-key (kbd "n") #'my/output-next-error)
    (local-set-key (kbd "p") #'my/output-previous-error)))

(defun my/output--track-started (_process)
  "Remember a successfully started compilation in its owning frame."
  (when (my/output-buffer-p)
    (let ((frame (if (frame-live-p my/output-owner-frame)
                     my/output-owner-frame
                   (selected-frame))))
      (set-frame-parameter frame 'my/output-last-buffer (current-buffer)))))

(defun my/output--forget-frame (frame)
  "Release output buffers' reference to deleted FRAME.
The workspace, rather than the frame, owns a running Remote task."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'my/output-owner-frame buffer) frame)
      (with-current-buffer buffer
        (setq-local my/output-owner-frame nil)))))

(defun my/output-switch ()
  "Switch to the most recent output buffer."
  (interactive)
  (pop-to-buffer (my/output--buffer)
                 '(nil (reusable-frames . nil))))

(defun my/output-rerun ()
  "Re-run the current or most recent output command."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (if (remote-task-p remote-task-instance)
        (my/output--adopt-task
         (remote-task-rerun remote-task-instance)
         (if (frame-live-p my/output-owner-frame)
             my/output-owner-frame
           (selected-frame))
         my/output-workflow-kind)
      (recompile))))

(defun my/output-stop ()
  "Stop the current or most recent output process."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (if (remote-task-p remote-task-instance)
        (if (eq (remote-task-state remote-task-instance) 'running)
            (remote-task-cancel remote-task-instance)
          (user-error "No active output process"))
      (if-let* ((proc (get-buffer-process (current-buffer)))
                ((process-live-p proc)))
          (kill-compilation)
        (user-error "No active output process")))))

(defun my/output-next-error ()
  "Jump to the next error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (compilation-next-error 1)))

(defun my/output-previous-error ()
  "Jump to the previous error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (compilation-previous-error 1)))

(defun my/output-first-error ()
  "Jump to the first error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (first-error)))

(defun my/output-toggle-scroll ()
  "Toggle `compilation-scroll-output' for the active output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (setq-local compilation-scroll-output
                (not (if (local-variable-p 'compilation-scroll-output)
                         compilation-scroll-output
                       nil)))
    (message "Output auto-scroll %s"
             (if compilation-scroll-output "enabled" "disabled"))))

(transient-define-prefix output-menu ()
  "Open output controls for the current Emacs frame."
  [["Output"
    ("o" "latest output" my/output-switch)
    ("r" "rerun" my/output-rerun)
    ("k" "stop" my/output-stop)
    ("n" "next error" my/output-next-error)
    ("p" "previous error" my/output-previous-error)
    ("f" "first error" my/output-first-error)
    ("s" "toggle auto scroll" my/output-toggle-scroll)]])

(defalias 'output-switch #'my/output-switch
  "Show this frame's latest available output buffer.")

(add-hook 'compilation-mode-hook #'my/output--setup-buffer)
(add-hook 'compilation-start-hook #'my/output--track-started)
(add-hook 'delete-frame-functions #'my/output--forget-frame)

(my/leader!
  "r o" '(:def output-menu :which-key "output")
  "r O" '(:def output-switch :which-key "latest output"))

(provide 'init-output)
;;; init-output.el ends here
