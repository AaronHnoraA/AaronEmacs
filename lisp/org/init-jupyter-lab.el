;;; init-jupyter-lab.el --- JupyterLab process management -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Managed local JupyterLab launcher and status section for the Jupyter Hub.

;;; Code:

(require 'browse-url)
(require 'project)
(require 'subr-x)

(require 'init-funcs)

(defgroup my/jupyter nil
  "Jupyter REPL and connection-file helpers."
  :group 'tools
  :prefix "my/jupyter-")

(defcustom my/jupyter-lab-command "/opt/homebrew/anaconda3/bin/jupyter"
  "Absolute path to the Jupyter executable used for local JupyterLab."
  :type 'string
  :group 'my/jupyter)

(defcustom my/jupyter-lab-host "127.0.0.1"
  "Host used for the managed local JupyterLab server."
  :type 'string
  :group 'my/jupyter)

(defcustom my/jupyter-lab-port 8888
  "Port used for the managed local JupyterLab server."
  :type 'integer
  :group 'my/jupyter)

(defcustom my/jupyter-lab-default-directory nil
  "Default working directory for the managed local JupyterLab server.

When nil, prefer the current project root, then `default-directory'."
  :type '(choice (const :tag "Dynamic" nil) directory)
  :group 'my/jupyter)

(defcustom my/jupyter-lab-log-buffer-name "*jupyter-lab*"
  "Buffer used to capture the managed local JupyterLab output."
  :type 'string
  :group 'my/jupyter)

(defvar my/jupyter-lab-process nil
  "Managed local JupyterLab process.")

(defvar my/jupyter-lab-last-directory nil
  "Last working directory used to start managed local JupyterLab.")

(defvar my/jupyter-lab--restart-open nil
  "Non-nil means the next restart should open JupyterLab in the browser.")

(declare-function my/jupyter-manager-refresh "init-jupyter-core")
(declare-function my/jupyter-manager--insert-button "init-jupyter-core"
                  (label action help))

(defun my/jupyter-lab--project-root ()
  "Return the current project root, or nil."
  (when-let* ((project (project-current nil)))
    (expand-file-name (project-root project))))

(defun my/jupyter-lab--default-directory ()
  "Return the working directory for managed local JupyterLab."
  (expand-file-name
   (or my/jupyter-lab-default-directory
       my/jupyter-lab-last-directory
       (my/jupyter-lab--project-root)
       default-directory
       "~")))

(defun my/jupyter-lab--command ()
  "Return the executable used to launch local JupyterLab."
  (unless (and (stringp my/jupyter-lab-command)
               (file-executable-p my/jupyter-lab-command))
    (user-error "Cannot execute %s" my/jupyter-lab-command))
  my/jupyter-lab-command)

(defun my/jupyter-lab--argv ()
  "Return argv for the managed local JupyterLab process."
  (let ((command (my/jupyter-lab--command)))
    (append (list command)
            (when (string= (file-name-nondirectory command) "jupyter")
              '("lab"))
            (list "--no-browser"
                  (format "--ServerApp.ip=%s" my/jupyter-lab-host)
                  (format "--ServerApp.port=%d" my/jupyter-lab-port)
                  "--ServerApp.port_retries=0"))))

(defun my/jupyter-lab-url ()
  "Return the URL for the managed local JupyterLab server."
  (format "http://%s:%d/lab" my/jupyter-lab-host my/jupyter-lab-port))

(defun my/jupyter-lab-running-p ()
  "Return non-nil when the managed local JupyterLab process is alive."
  (and (processp my/jupyter-lab-process)
       (process-live-p my/jupyter-lab-process)))

(defun my/jupyter-lab--refresh-manager-maybe ()
  "Refresh the Jupyter manager when it is visible."
  (when (fboundp 'my/jupyter-manager-refresh)
    (when-let* ((buffer (get-buffer my/jupyter-manager-buffer-name)))
      (with-current-buffer buffer
        (when (derived-mode-p 'my/jupyter-manager-mode)
          (my/jupyter-manager-refresh))))))

(defun my/jupyter-lab--cleanup (&optional keep-log-buffer)
  "Clear managed JupyterLab state.

When KEEP-LOG-BUFFER is non-nil, do not kill the log buffer."
  (setq my/jupyter-lab-process nil
        my/jupyter-lab--restart-open nil)
  (unless keep-log-buffer
    (when-let* ((buffer (get-buffer my/jupyter-lab-log-buffer-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my/jupyter-lab--stop-process (process)
  "Request PROCESS to stop, then force-kill it if it stays alive."
  (when (process-live-p process)
    (set-process-query-on-exit-flag process nil)
    (condition-case err
        (interrupt-process process)
      (error
       (message "JupyterLab interrupt failed: %s" (error-message-string err))
       (ignore-errors (signal-process process 2))))
    ;; Jupyter's shutdown prompt expects an explicit yes/no answer.
    (run-at-time
     0.3 nil
     (lambda (proc)
       (when (process-live-p proc)
         (ignore-errors (process-send-string proc "y\n"))))
     process)
    (run-at-time
     30 nil
     (lambda (proc)
       (when (process-live-p proc)
         (ignore-errors (delete-process proc))))
     process)))

(defun my/jupyter-lab--sentinel (process event)
  "Track local JupyterLab PROCESS state changes described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (when (eq process my/jupyter-lab-process)
      (let ((restart-open my/jupyter-lab--restart-open))
        (my/jupyter-lab--cleanup)
        (when restart-open
          (setq my/jupyter-lab--restart-open nil)
          (my/jupyter-lab-start t))))
    (message "JupyterLab %s" (string-trim event))
    (my/jupyter-lab--refresh-manager-maybe)))

(defun my/jupyter-lab-open ()
  "Open managed local JupyterLab in the default browser."
  (interactive)
  (browse-url-default-browser (my/jupyter-lab-url)))

(defun my/jupyter-lab-start (&optional open)
  "Start managed local JupyterLab in the background.

When OPEN is non-nil, open the JupyterLab page in the browser after launch."
  (interactive "P")
  (if (my/jupyter-lab-running-p)
      (progn
        (message "JupyterLab is already running at %s" (my/jupyter-lab-url))
        (when open
          (my/jupyter-lab-open)))
    (let* ((default-directory (my/jupyter-lab--default-directory))
           (buffer (get-buffer-create my/jupyter-lab-log-buffer-name))
           (process
            (make-process
             :name "jupyter-lab"
             :buffer buffer
             :command (my/jupyter-lab--argv)
             :coding 'utf-8-unix
             :connection-type 'pty
             :noquery t
             :sentinel #'my/jupyter-lab--sentinel)))
      (set-process-query-on-exit-flag process nil)
      (setq my/jupyter-lab-last-directory default-directory
            my/jupyter-lab-process process)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (goto-char (point-max))
          (insert (format "[%s] cwd=%s\n"
                          (format-time-string "%F %T")
                          (abbreviate-file-name default-directory)))))
      (message "Starting JupyterLab at %s" (my/jupyter-lab-url))
      (my/jupyter-lab--refresh-manager-maybe)
      (when open
        (run-at-time "1 sec" nil #'my/jupyter-lab-open)))))

(defun my/jupyter-lab-start-and-open ()
  "Start managed local JupyterLab and open it in the browser."
  (interactive)
  (my/jupyter-lab-start t))

(defun my/jupyter-lab-stop ()
  "Stop the managed local JupyterLab process."
  (interactive)
  (unless (my/jupyter-lab-running-p)
    (my/jupyter-lab--cleanup)
    (user-error "JupyterLab is not running"))
  (let ((process my/jupyter-lab-process))
    (setq my/jupyter-lab--restart-open nil)
    (my/jupyter-lab--stop-process process)
    (message "Stopping JupyterLab...")
    (my/jupyter-lab--refresh-manager-maybe)))

(defun my/jupyter-lab-restart (&optional open)
  "Restart the managed local JupyterLab process.

When OPEN is non-nil, open the JupyterLab page afterwards."
  (interactive "P")
  (if (my/jupyter-lab-running-p)
      (let ((process my/jupyter-lab-process))
        (setq my/jupyter-lab--restart-open open)
        (my/jupyter-lab--stop-process process)
        (message "Restarting JupyterLab..."))
    (my/jupyter-lab-start open)))

(defun my/jupyter-lab-open-log ()
  "Open the managed local JupyterLab log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create my/jupyter-lab-log-buffer-name)))

(defun my/jupyter-manager--insert-lab-section ()
  "Insert the local JupyterLab service section."
  (insert "Local JupyterLab\n")
  (insert "----------------\n")
  (let ((running (my/jupyter-lab-running-p)))
    (insert (format "status: %s\n" (if running "running" "stopped")))
    (insert (format "url:    %s\n" (my/jupyter-lab-url)))
    (insert (format "cwd:    %s\n"
                    (abbreviate-file-name (my/jupyter-lab--default-directory))))
    (insert "actions: ")
    (my/jupyter-manager--insert-button
     "[start]"
     (lambda (_button) (my/jupyter-lab-start))
     "Start local JupyterLab in the background")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[open]"
     (lambda (_button) (my/jupyter-lab-open))
     "Open local JupyterLab in the browser")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[restart]"
     (lambda (_button) (my/jupyter-lab-restart))
     "Restart local JupyterLab")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[stop]"
     (lambda (_button) (my/jupyter-lab-stop))
     "Stop local JupyterLab")
    (insert " ")
    (my/jupyter-manager--insert-button
     "[log]"
     (lambda (_button) (my/jupyter-lab-open-log))
     "Open the local JupyterLab log buffer")
    (insert "\n\n")))

(provide 'init-jupyter-lab)
;;; init-jupyter-lab.el ends here
