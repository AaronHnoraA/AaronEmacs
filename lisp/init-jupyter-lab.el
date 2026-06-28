;;; init-jupyter-lab.el --- JupyterLab process management -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Managed local JupyterLab launcher and status section for the Jupyter Hub.

;;; Code:

(require 'config)

(require 'browse-url)
(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url)

(require 'init-funcs)

(defgroup my/jupyter nil
  "Jupyter REPL and connection-file helpers."
  :group 'tools
  :prefix "my/jupyter-")

(defconst my/jupyter-lab--config-directory
  (file-name-as-directory
   (file-name-directory
    (directory-file-name
     (or (file-name-directory (or load-file-name buffer-file-name))
         user-emacs-directory))))
  "Directory containing this Emacs configuration.")

(config-defvar my/jupyter-lab-command nil
  "Absolute path to the Jupyter executable used for local JupyterLab."
  :type 'string
  :group 'my/jupyter)

(config-defvar my/jupyter-lab-host nil
  "Host used for the managed local JupyterLab server."
  :type 'string
  :group 'my/jupyter)

(config-defvar my/jupyter-lab-port nil
  "Port used for the managed local JupyterLab server."
  :type 'integer
  :group 'my/jupyter)

(config-defvar my/jupyter-lab-default-directory nil
  "Default working directory for the managed local JupyterLab server.

When nil, prefer the roam notes root for roam files, then the current
project root, then `default-directory'."
  :type '(choice (const :tag "Dynamic" nil) directory)
  :group 'my/jupyter)

(config-defvar my/jupyter-lab-log-buffer-name nil
  "Buffer used to capture the managed local JupyterLab output."
  :type 'string
  :group 'my/jupyter)

(config-defvar my/jupyter-lab-jupyter-path nil
  "Jupyter data directory prepended when starting managed local JupyterLab."
  :type 'directory
  :group 'my/jupyter)

(defvar my/jupyter-lab-process nil
  "Managed local JupyterLab process.")

(defvar my/jupyter-lab-last-directory nil
  "Last working directory used to start managed local JupyterLab.")

(defvar my/jupyter-lab--restart-open nil
  "Non-nil means the next restart should open JupyterLab in the browser.

When this is a string, open that URL after restart.")

(defvar my/jupyter-lab--restart-directory nil
  "Working directory to use for a pending JupyterLab restart.")

(defvar my/jupyter-lab--shutdown-response-timer nil
  "Timer used to answer JupyterLab's shutdown prompt.")

(defvar my/jupyter-lab--force-kill-timer nil
  "Timer used to force-kill a JupyterLab process that ignores shutdown.")

(defvar my/jupyter-lab--open-timer nil
  "Timer used to open JupyterLab after startup.")

(defvar jupytext-mode nil)
(defvar-local my/jupytext-notebook-file nil)

(declare-function my/jupyter-manager-refresh "init-jupyter-core")
(declare-function my/jupyter-manager--insert-button "init-jupyter-core"
                  (label action help))
(declare-function my/aaronnote-roam-root "init-md-roam")
(declare-function my/jupytext--ensure-pair "init-jupyter-core")
(declare-function my/jupytext--sync "init-jupyter-core" (&optional announce))

(defun my/jupyter-lab--env-get (env name)
  "Return NAME's value in ENV."
  (when-let* ((entry (cl-find-if
                      (lambda (item)
                        (string-prefix-p (concat name "=") item))
                      env)))
    (substring entry (1+ (length name)))))

(defun my/jupyter-lab--env-set (env name value)
  "Return ENV with NAME set to VALUE."
  (cons (format "%s=%s" name value)
        (cl-remove-if
         (lambda (item)
           (string-prefix-p (concat name "=") item))
         env)))

(defun my/jupyter-lab--prepend-path (value directory)
  "Prepend DIRECTORY to path-like VALUE."
  (let ((directory (directory-file-name (expand-file-name directory))))
    (string-join
     (delete-dups
      (delq nil
            (cons directory
                  (and value
                       (not (string-empty-p value))
                       (split-string value path-separator t)))))
     path-separator)))

(defun my/jupyter-lab--jupyter-path (env)
  "Return the Jupyter data path for ENV."
  (my/jupyter-lab--prepend-path
   (my/jupyter-lab--env-get env "JUPYTER_PATH")
   my/jupyter-lab-jupyter-path))

(defun my/jupyter-lab--process-environment ()
  "Return a process environment suitable for local Jupyter and kernels."
  (let* ((home (expand-file-name "~"))
         (sage-root "/var/tmp/sage-10.9-current")
         (sage-local (expand-file-name "local" sage-root))
         (base-path
          (string-join
           (delete-dups
            (append
             (list (expand-file-name "bin" sage-local)
                   "/opt/homebrew/bin"
                   "/opt/homebrew/sbin"
                   "/usr/local/bin"
                   "/usr/bin"
                   "/bin"
                   "/usr/sbin"
                   "/sbin")
             (split-string (or (getenv "PATH") "") path-separator t)))
           path-separator))
         (env process-environment))
    (dolist (entry `(("HOME" . ,home)
                     ("USER" . ,(user-login-name))
                     ("LOGNAME" . ,(user-login-name))
                     ("SHELL" . "/bin/zsh")
                     ("DOT_SAGE" . ,(expand-file-name ".sage" home))
                     ("IPYTHONDIR" . ,(expand-file-name ".ipython" home))
                     ("TMPDIR" . ,temporary-file-directory)
                     ("SAGE_ROOT" . ,sage-root)
                     ("SAGE_LOCAL" . ,sage-local)
                     ("PATH" . ,base-path)))
      (setq env (my/jupyter-lab--env-set env (car entry) (cdr entry))))
    (my/jupyter-lab--env-set env "JUPYTER_PATH"
                             (my/jupyter-lab--jupyter-path env))))

(setenv "JUPYTER_PATH"
        (my/jupyter-lab--jupyter-path process-environment))

(defun my/jupyter-lab--context-buffer ()
  "Return the buffer whose file should drive Jupyter cwd decisions."
  (if (and (boundp 'my/jupyter-manager-source-buffer)
           (buffer-live-p my/jupyter-manager-source-buffer))
      my/jupyter-manager-source-buffer
    (current-buffer)))

(defun my/jupyter-lab--context-file (&optional buffer)
  "Return BUFFER's file-like path, if any."
  (with-current-buffer (or buffer (my/jupyter-lab--context-buffer))
    (expand-file-name
     (or buffer-file-name
         (and (boundp 'my/aaronnote-buffer-file-name)
              my/aaronnote-buffer-file-name)
         default-directory))))

(defun my/jupyter-lab--roam-root-for-file (file)
  "Return the roam notes root when FILE is inside it."
  (when (stringp file)
    (when-let* ((root-source
                 (cond
                  ((fboundp 'my/aaronnote-roam-root)
                   (my/aaronnote-roam-root))
                  ((and (boundp 'my/aaronnote-roam-root)
                        (stringp my/aaronnote-roam-root))
                   my/aaronnote-roam-root)))
                (root (file-name-as-directory (expand-file-name root-source))))
      (when (file-in-directory-p (expand-file-name file) root)
        root))))

(defun my/jupyter-lab--project-root (&optional file)
  "Return the project root for FILE or the current context, if any."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (or (and file
                        (if (file-directory-p file)
                            file
                          (file-name-directory file)))
                   default-directory)))))
    (when-let* ((project (project-current nil dir)))
      (expand-file-name (project-root project)))))

(defun my/jupyter-lab--root-for-file (file)
  "Return the preferred Jupyter working root for FILE."
  (file-name-as-directory
   (expand-file-name
    (or (my/jupyter-lab--roam-root-for-file file)
        (my/jupyter-lab--project-root file)
        (and file
             (if (file-directory-p file)
                 file
               (file-name-directory file)))
        default-directory
        "~"))))

(defun my/jupyter-lab--default-directory ()
  "Return the working directory for managed local JupyterLab."
  (expand-file-name
   (or my/jupyter-lab-default-directory
       (my/jupyter-lab--root-for-file (my/jupyter-lab--context-file)))))

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

(defun my/jupyter-lab--status-url ()
  "Return the local JupyterLab status API URL."
  (format "http://%s:%d/api/status" my/jupyter-lab-host my/jupyter-lab-port))

(defun my/jupyter-lab--ready-p ()
  "Return non-nil when the managed JupyterLab HTTP server is ready."
  (let ((url-show-status nil))
    (when-let* ((buffer (ignore-errors
                          (url-retrieve-synchronously
                           (my/jupyter-lab--status-url)
                           t t 1.0))))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (looking-at-p "HTTP/[0-9.]+ 200\\b"))
        (kill-buffer buffer)))))

(defun my/jupyter-lab--schedule-open-when-ready (open &optional attempt)
  "Open JupyterLab target OPEN after the HTTP server becomes ready."
  (my/jupyter-lab--cancel-open-timer)
  (let ((attempt (or attempt 0)))
    (setq my/jupyter-lab--open-timer
          (run-at-time
           (if (zerop attempt) 0.2 0.5) nil
           (lambda (open attempt)
             (setq my/jupyter-lab--open-timer nil)
             (cond
              ((my/jupyter-lab--ready-p)
               (my/jupyter-lab--open-url open t))
              ((and (my/jupyter-lab-running-p) (< attempt 80))
               (my/jupyter-lab--schedule-open-when-ready open (1+ attempt)))
              (t
               (message "JupyterLab did not become ready at %s"
                        (my/jupyter-lab-url)))))
           open attempt))))

(defun my/jupyter-lab--open-url (open &optional ready)
  "Open JupyterLab target described by OPEN."
  (if (or ready (my/jupyter-lab--ready-p))
      (progn
        (unless (fboundp 'my/xwidget-open-url) (require 'init-browser))
        (my/xwidget-open-url
         (if (stringp open) open (my/jupyter-lab-url))
         :id "jupyter-lab"
         :display 'side
         :force-new t))
    (my/jupyter-lab--schedule-open-when-ready open)))

(defun my/jupyter-lab-url-p (url)
  "Return non-nil when URL targets the managed local JupyterLab server."
  (and (stringp url)
       (string-prefix-p
        (format "http://%s:%d/" my/jupyter-lab-host my/jupyter-lab-port)
        url)))

(defun my/jupyter-lab--same-root-p (left right)
  "Return non-nil when LEFT and RIGHT name the same Jupyter root."
  (and (stringp left)
       (stringp right)
       (equal (file-truename (file-name-as-directory (expand-file-name left)))
              (file-truename (file-name-as-directory (expand-file-name right))))))

(defun my/jupyter-lab--ensure-root (root &optional open)
  "Ensure managed JupyterLab is running with ROOT as cwd.

OPEN is passed to `my/jupyter-lab-start' or remembered across a restart.
Return non-nil when JupyterLab is already running at ROOT."
  (if (my/jupyter-lab-running-p)
      (if (my/jupyter-lab--same-root-p my/jupyter-lab-last-directory root)
          t
        (let ((process my/jupyter-lab-process))
          (setq my/jupyter-lab--restart-open open)
          (setq my/jupyter-lab--restart-directory root)
          (my/jupyter-lab--stop-process process)
          (message "Restarting JupyterLab from %s..."
                   (abbreviate-file-name root))
          nil))
    (let ((my/jupyter-lab-default-directory root))
      (my/jupyter-lab-start open))
    nil))

(defun my/jupyter-lab--current-notebook-file ()
  "Return the notebook associated with the current buffer, if any."
  (cond
   ((and buffer-file-name
         (string-suffix-p ".ipynb" buffer-file-name t))
    (expand-file-name buffer-file-name))
   ((and (bound-and-true-p jupytext-mode)
         (fboundp 'my/jupytext--ensure-pair))
    (my/jupytext--ensure-pair)
    (when (and (not (file-exists-p my/jupytext-notebook-file))
               (fboundp 'my/jupytext--sync))
      (my/jupytext--sync t))
    (and my/jupytext-notebook-file
         (expand-file-name my/jupytext-notebook-file)))))

(defun my/jupyter-lab--split-notebook-target (target)
  "Return (NOTEBOOK . SELECTOR) parsed from local notebook TARGET."
  (let* ((raw (url-unhex-string (string-remove-prefix "file:" (or target ""))))
         (hash-pos (string-match-p "#" raw))
         (hash-selector (and hash-pos (substring raw (1+ hash-pos))))
         (raw (if hash-pos (substring raw 0 hash-pos) raw))
         (at-match (string-match "\\(.+?\\.ipynb\\)@\\(.+\\)\\'" raw)))
    (if at-match
        (cons (match-string 1 raw) (match-string 2 raw))
      (cons raw hash-selector))))

(defun my/jupyter-lab--jupytext-script-candidates (notebook)
  "Return likely Jupytext script paths for NOTEBOOK."
  (let* ((base (file-name-sans-extension (expand-file-name notebook)))
         (dir (file-name-directory base))
         (stem (file-name-nondirectory base))
         (exts '(".ju.py" ".py" ".md" ".qmd" ".Rmd" ".rmd" ".jl")))
    (append
     (delq nil
           (mapcar (lambda (buffer)
                     (with-current-buffer buffer
                       (when (and buffer-file-name
                                  (bound-and-true-p jupytext-mode)
                                  (stringp my/jupytext-notebook-file)
                                  (file-equal-p
                                   (expand-file-name my/jupytext-notebook-file)
                                   (expand-file-name notebook)))
                         buffer-file-name)))
                   (buffer-list)))
     (mapcar (lambda (ext) (concat dir stem ext)) exts))))

(defun my/jupyter-lab-jupytext-script-for-notebook (notebook)
  "Return an existing Jupytext script paired with NOTEBOOK, or nil."
  (seq-find #'file-exists-p
            (delete-dups
             (my/jupyter-lab--jupytext-script-candidates notebook))))

(defun my/jupyter-lab--selector-slug (value)
  "Return a loose slug for SELECTOR heading matching."
  (let ((text (downcase (url-unhex-string (or value "")))))
    (replace-regexp-in-string
     "-+" "-"
     (replace-regexp-in-string "[^[:alnum:]]+" "-" (string-trim text)))))

(defun my/jupyter-lab--goto-selector (selector)
  "Move point to SELECTOR in the current Jupytext script when possible."
  (when (and selector (not (string-empty-p selector)))
    (let* ((decoded (url-unhex-string selector))
           (slug (my/jupyter-lab--selector-slug decoded))
           (found nil))
      (goto-char (point-min))
      (setq found
            (or (re-search-forward
                 (format "^\\s-*#\\{1,6\\}\\s-+.*%s"
                         (regexp-quote decoded))
                 nil t)
                (catch 'match
                  (while (re-search-forward "^\\s-*#\\{1,6\\}\\s-+\\(.+\\)$" nil t)
                    (when (string= (my/jupyter-lab--selector-slug (match-string 1))
                                   slug)
                      (throw 'match t))))
                (search-forward decoded nil t)))
      (if found
          (progn
            (beginning-of-line)
            (recenter))
        (message "Jupytext selector not found: %s" decoded)))))

(defun my/jupyter-lab-open-jupytext-target (target)
  "Open TARGET's paired Jupytext script in Emacs.
Return non-nil when TARGET was handled."
  (pcase-let* ((`(,notebook . ,selector)
                (my/jupyter-lab--split-notebook-target target)))
    (when (string-suffix-p ".ipynb" notebook t)
      (when-let* ((script (my/jupyter-lab-jupytext-script-for-notebook notebook)))
        (find-file script)
        (my/jupyter-lab--goto-selector selector)
        t))))

(defun my/jupyter-lab-open-path (abs-path &optional selector)
  "Open notebook ABS-PATH in xwidget, jumping to SELECTOR heading slug if given."
  (let* ((root (file-name-as-directory
                (expand-file-name (my/jupyter-lab--root-for-file abs-path))))
         (rel  (file-relative-name (expand-file-name abs-path) root))
         (frag (if (and selector (not (string-empty-p selector)))
                   (concat "#" (url-hexify-string selector)) ""))
         (url  (format "http://%s:%d/lab/tree/%s%s"
                       my/jupyter-lab-host my/jupyter-lab-port
                       (url-hexify-string rel) frag)))
    (when (my/jupyter-lab--ensure-root root url)
      (my/jupyter-lab--open-url url))))

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

(defun my/jupyter-lab--cancel-timer (symbol)
  "Cancel the timer stored in SYMBOL and set SYMBOL to nil."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun my/jupyter-lab--cancel-stop-timers ()
  "Cancel delayed shutdown timers for managed JupyterLab."
  (my/jupyter-lab--cancel-timer 'my/jupyter-lab--shutdown-response-timer)
  (my/jupyter-lab--cancel-timer 'my/jupyter-lab--force-kill-timer))

(defun my/jupyter-lab--cancel-open-timer ()
  "Cancel the delayed browser-open timer for managed JupyterLab."
  (my/jupyter-lab--cancel-timer 'my/jupyter-lab--open-timer))

(defun my/jupyter-lab--cleanup (&optional keep-log-buffer)
  "Clear managed JupyterLab state.

When KEEP-LOG-BUFFER is non-nil, do not kill the log buffer."
  (my/jupyter-lab--cancel-stop-timers)
  (my/jupyter-lab--cancel-open-timer)
  (setq my/jupyter-lab-process nil
        my/jupyter-lab--restart-open nil
        my/jupyter-lab--restart-directory nil)
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
    (my/jupyter-lab--cancel-stop-timers)
    (let (timer)
      (setq timer
            (run-at-time
             0.3 nil
             (lambda (proc)
               (when (eq my/jupyter-lab--shutdown-response-timer timer)
                 (setq my/jupyter-lab--shutdown-response-timer nil))
               (when (process-live-p proc)
                 (ignore-errors (process-send-string proc "y\n"))))
             process))
      (setq my/jupyter-lab--shutdown-response-timer timer))
    (let (timer)
      (setq timer
            (run-at-time
             30 nil
             (lambda (proc)
               (when (eq my/jupyter-lab--force-kill-timer timer)
                 (setq my/jupyter-lab--force-kill-timer nil))
               (when (process-live-p proc)
                 (ignore-errors (delete-process proc))))
             process))
      (setq my/jupyter-lab--force-kill-timer timer))))

(defun my/jupyter-lab--sentinel (process event)
  "Track local JupyterLab PROCESS state changes described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (when (eq process my/jupyter-lab-process)
      (let ((restart-open my/jupyter-lab--restart-open)
            (restart-directory my/jupyter-lab--restart-directory))
        (my/jupyter-lab--cleanup)
        (when restart-open
          (let ((my/jupyter-lab-default-directory restart-directory))
            (my/jupyter-lab-start restart-open)))))
    (message "JupyterLab %s" (string-trim event))
    (my/jupyter-lab--refresh-manager-maybe)))

(defun my/jupyter-lab-open (&optional root-only)
  "Open managed local JupyterLab in xwidget.

When the current buffer is a notebook or Jupytext script, open that notebook.
With prefix argument ROOT-ONLY, open the Lab root instead."
  (interactive "P")
  (if-let* ((notebook (and (not root-only)
                           (my/jupyter-lab--current-notebook-file))))
      (my/jupyter-lab-open-path notebook)
    (let ((root (file-name-as-directory
                 (expand-file-name (my/jupyter-lab--default-directory)))))
      (when (my/jupyter-lab--ensure-root root t)
        (my/jupyter-lab--open-url t)))))

(defun my/jupyter-lab-start (&optional open)
  "Start managed local JupyterLab in the background.

When OPEN is non-nil, open the JupyterLab page in the browser after launch."
  (interactive "P")
  (if (my/jupyter-lab-running-p)
      (progn
        (message "JupyterLab is already running at %s" (my/jupyter-lab-url))
        (when open
          (my/jupyter-lab--open-url open)))
    (let* ((default-directory (my/jupyter-lab--default-directory))
           (process-environment (my/jupyter-lab--process-environment))
           (argv (my/jupyter-lab--argv))
           (buffer (get-buffer-create my/jupyter-lab-log-buffer-name))
           (process
            (make-process
             :name "jupyter-lab"
             :buffer buffer
             :command argv
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
                          (abbreviate-file-name default-directory)))
          (insert (format "cmd=%s\n"
                          (string-join
                           (mapcar #'shell-quote-argument argv)
                           " ")))
          (insert (format "jupyter_path=%s\n"
                          (my/jupyter-lab--env-get
                           process-environment
                           "JUPYTER_PATH")))))
      (message "Starting JupyterLab at %s" (my/jupyter-lab-url))
      (my/jupyter-lab--refresh-manager-maybe)
      (when open
        (my/jupyter-lab--schedule-open-when-ready open)))))

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
