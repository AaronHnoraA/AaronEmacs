;;; init-aaronnote-jupyter.el --- Isolated Aaronnote JupyterLab launcher -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Starts the JupyterLab runtime vendored under lisp/roam/aaronnote/jupyter.
;; This intentionally does not use the top-level Emacs jupyter/ data directory
;; or the global JupyterLab executable.

;;; Code:

(require 'config)

(require 'browse-url)
(require 'cl-lib)
(require 'compile)
(require 'project)
(require 'subr-x)
(require 'url)
(require 'url-util)

(declare-function my/xwidget-open-url "init-browser" (url &rest args))
(declare-function my/xwidget-current-url "init-browser" (&optional buffer))
(declare-function my/xwidget-session-buffer "init-browser" (id))
(declare-function my/aaronnote-roam-root "init-md-roam" ())

(defgroup my/aaronnote-jupyter nil
  "Aaronnote-owned JupyterLab runtime."
  :group 'applications
  :prefix "my/aaronnote-jupyter-")

(defconst my/aaronnote-jupyter-root
  (expand-file-name "lisp/roam/aaronnote/jupyter" user-emacs-directory)
  "Root of the Aaronnote-owned Jupyter project.")

(defconst my/aaronnote-jupyter--run-script
  (expand-file-name "scripts/run-jupyter-lab.sh" my/aaronnote-jupyter-root)
  "Script that starts the Aaronnote-owned JupyterLab.")

(defconst my/aaronnote-jupyter--bootstrap-script
  (expand-file-name "scripts/bootstrap-jupyter.sh" my/aaronnote-jupyter-root)
  "Script that bootstraps the Aaronnote-owned JupyterLab.")

(defconst my/aaronnote-jupyter--doctor-script
  (expand-file-name "scripts/doctor-jupyter.sh" my/aaronnote-jupyter-root)
  "Script that diagnoses the Aaronnote-owned JupyterLab.")

(config-defvar my/aaronnote-jupyter-host "127.0.0.1"
  "Host used for the Aaronnote-owned JupyterLab server."
  :type 'string
  :group 'my/aaronnote-jupyter)

(config-defvar my/aaronnote-jupyter-port 8890
  "Port used for the Aaronnote-owned JupyterLab server."
  :type 'integer
  :group 'my/aaronnote-jupyter)

(config-defvar my/aaronnote-jupyter-default-directory "/"
  "Stable root directory for Aaronnote-owned JupyterLab.
The filesystem root keeps URLs valid when notes from roam, projects, home, and
mounted volumes are open in the same Lab session.  A custom directory may be
used when access should intentionally be restricted."
  :type '(choice (const :tag "Dynamic" nil) directory)
  :group 'my/aaronnote-jupyter)

(config-defvar my/aaronnote-jupyter-log-buffer-name "*aaronnote-jupyter*"
  "Buffer used to capture the Aaronnote-owned JupyterLab output."
  :type 'string
  :group 'my/aaronnote-jupyter)

(config-defvar my/aaronnote-jupyter-markdown-factory "Editor"
  "JupyterLab document factory used when opening Markdown files.
The plain text editor is more reliable inside xwidget than the default Markdown
viewer."
  :type 'string
  :group 'my/aaronnote-jupyter)

(defvar my/aaronnote-jupyter-process nil
  "Managed Aaronnote-owned JupyterLab process.")

(defvar my/aaronnote-jupyter-last-directory nil
  "Last root directory used to start Aaronnote-owned JupyterLab.")

(defvar my/aaronnote-jupyter--open-timer nil
  "Timer used to open JupyterLab after startup.")

(defvar my/aaronnote-jupyter--shutdown-response-timer nil
  "Timer used to answer JupyterLab's shutdown prompt.")

(defvar my/aaronnote-jupyter--force-kill-timer nil
  "Timer used to force-kill a JupyterLab process that ignores shutdown.")

(defvar my/aaronnote-jupyter--restart-open nil
  "Non-nil means the next restart should open JupyterLab.
When this is a string, open that URL after restart.")

(defvar my/aaronnote-jupyter--restart-directory nil
  "Root directory to use for a pending JupyterLab restart.")

(defconst my/aaronnote-jupyter--xwidget-session-id "aaronnote-jupyter"
  "Stable xwidget session id for the Aaronnote-owned JupyterLab surface.")

(defun my/aaronnote-jupyter--env-get (env name)
  "Return NAME's value in ENV."
  (when-let* ((entry (cl-find-if
                      (lambda (item)
                        (string-prefix-p (concat name "=") item))
                      env)))
    (substring entry (1+ (length name)))))

(defun my/aaronnote-jupyter--env-set (env name value)
  "Return ENV with NAME set to VALUE."
  (cons (format "%s=%s" name value)
        (cl-remove-if
         (lambda (item)
           (string-prefix-p (concat name "=") item))
         env)))

(defun my/aaronnote-jupyter--process-environment ()
  "Return a process environment isolated to the Aaronnote Jupyter project."
  (let* ((venv-bin (expand-file-name ".venv/bin" my/aaronnote-jupyter-root))
         (env process-environment))
    (dolist (entry `(("AARONNOTE_JUPYTER_HOST" . ,my/aaronnote-jupyter-host)
                     ("AARONNOTE_JUPYTER_PORT" . ,(number-to-string my/aaronnote-jupyter-port))
                     ("JUPYTER_CONFIG_DIR" . ,(expand-file-name ".jupyter/config" my/aaronnote-jupyter-root))
                     ("JUPYTER_DATA_DIR" . ,(expand-file-name ".jupyter/data" my/aaronnote-jupyter-root))
                     ("JUPYTER_RUNTIME_DIR" . ,(expand-file-name ".jupyter/runtime" my/aaronnote-jupyter-root))
                     ("JUPYTER_PATH" . ,(expand-file-name ".jupyter/data" my/aaronnote-jupyter-root))
                     ("IPYTHONDIR" . ,(expand-file-name ".jupyter/ipython" my/aaronnote-jupyter-root))
                     ("PYTHONNOUSERSITE" . "1")
                     ("PATH" . ,(string-join
                                  (delete-dups
                                   (cons venv-bin
                                         (split-string (or (getenv "PATH") "")
                                                       path-separator t)))
                                  path-separator))))
      (setq env (my/aaronnote-jupyter--env-set env (car entry) (cdr entry))))
    env))

(defun my/aaronnote-jupyter--ready-p ()
  "Return non-nil when the Aaronnote-owned JupyterLab server is ready."
  (let ((url-show-status nil))
    (when-let* ((buffer (ignore-errors
                          (url-retrieve-synchronously
                           (my/aaronnote-jupyter-status-url)
                           t t 1.0))))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (looking-at-p "HTTP/[0-9.]+ 200\\b"))
        (kill-buffer buffer)))))

(defun my/aaronnote-jupyter-url ()
  "Return the base URL for the Aaronnote-owned JupyterLab server."
  (format "http://%s:%d/lab"
          my/aaronnote-jupyter-host
          my/aaronnote-jupyter-port))

(defun my/aaronnote-jupyter-status-url ()
  "Return the local JupyterLab status API URL."
  (format "http://%s:%d/api/status"
          my/aaronnote-jupyter-host
          my/aaronnote-jupyter-port))

(defun my/aaronnote-jupyter-url-p (url)
  "Return non-nil when URL targets the Aaronnote-owned JupyterLab server."
  (and (stringp url)
       (string-prefix-p
        (format "http://%s:%d/" my/aaronnote-jupyter-host my/aaronnote-jupyter-port)
        url)))

(defun my/aaronnote-jupyter--cancel-timer (symbol)
  "Cancel the timer stored in SYMBOL and set SYMBOL to nil."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun my/aaronnote-jupyter--cancel-stop-timers ()
  "Cancel delayed shutdown timers for Aaronnote-owned JupyterLab."
  (my/aaronnote-jupyter--cancel-timer 'my/aaronnote-jupyter--shutdown-response-timer)
  (my/aaronnote-jupyter--cancel-timer 'my/aaronnote-jupyter--force-kill-timer))

(defun my/aaronnote-jupyter--cancel-open-timer ()
  "Cancel the delayed browser-open timer."
  (my/aaronnote-jupyter--cancel-timer 'my/aaronnote-jupyter--open-timer))

(defun my/aaronnote-jupyter--cleanup ()
  "Clear managed Aaronnote Jupyter process state."
  (my/aaronnote-jupyter--cancel-stop-timers)
  (my/aaronnote-jupyter--cancel-open-timer)
  (setq my/aaronnote-jupyter-process nil
        my/aaronnote-jupyter--restart-open nil
        my/aaronnote-jupyter--restart-directory nil))

(defun my/aaronnote-jupyter-running-p ()
  "Return non-nil when the managed Aaronnote JupyterLab process is alive."
  (and (processp my/aaronnote-jupyter-process)
       (process-live-p my/aaronnote-jupyter-process)))

(defun my/aaronnote-jupyter--same-root-p (left right)
  "Return non-nil when LEFT and RIGHT name the same Jupyter root."
  (and (stringp left)
       (stringp right)
       (equal (file-truename (file-name-as-directory (expand-file-name left)))
              (file-truename (file-name-as-directory (expand-file-name right))))))

(defun my/aaronnote-jupyter--roam-root-for-file (file)
  "Return the roam notes root when FILE is inside it."
  (let ((roots (delete-dups
                (delq nil
                      (list
                       (when (fboundp 'my/aaronnote-roam-root)
                         (ignore-errors (my/aaronnote-roam-root)))
                       (when (and (boundp 'my/aaronnote-roam-root)
                                  (stringp my/aaronnote-roam-root))
                         my/aaronnote-roam-root)
                       (expand-file-name ".roam" user-emacs-directory))))))
    (catch 'root
      (dolist (root-source roots)
        (let ((root (file-name-as-directory (expand-file-name root-source))))
          (when (and (file-directory-p root)
                     (stringp file)
                     (or (file-in-directory-p (expand-file-name file) root)
                         (file-in-directory-p (file-truename file)
                                              (file-truename root))))
            (throw 'root root)))))))

(defun my/aaronnote-jupyter--markdown-file-p (file)
  "Return non-nil when FILE is a Markdown file."
  (and (stringp file)
       (or (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
           (string-equal (file-name-nondirectory file) "README"))))

(defun my/aaronnote-jupyter--project-root (&optional file)
  "Return the project root for FILE or current `default-directory'."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (or (and file
                        (if (file-directory-p file)
                            file
                          (file-name-directory file)))
                   default-directory)))))
    (when-let* ((project (project-current nil dir)))
      (expand-file-name (project-root project)))))

(defun my/aaronnote-jupyter--root-for-file (file)
  "Return the preferred Jupyter root for FILE."
  (ignore file)
  (file-name-as-directory
   (expand-file-name
    (or my/aaronnote-jupyter-default-directory "/"))))

(defun my/aaronnote-jupyter--default-directory ()
  "Return the default root for managed Aaronnote JupyterLab."
  (my/aaronnote-jupyter--root-for-file
   (or buffer-file-name default-directory)))

(defun my/aaronnote-jupyter--encode-path (path)
  "URL-encode PATH while preserving path separators."
  (mapconcat #'url-hexify-string
             (split-string (replace-regexp-in-string "\\`/+" "" path) "/" t)
             "/"))

(defun my/aaronnote-jupyter--url-for-file (file root &optional selector)
  "Return a JupyterLab URL for FILE under ROOT, optionally with SELECTOR."
  (let* ((expanded-file (expand-file-name file))
         (expanded-root (file-name-as-directory (expand-file-name root)))
         (rel (file-relative-name
               (file-truename expanded-file)
               (file-name-as-directory (file-truename expanded-root))))
         (query (if (and (my/aaronnote-jupyter--markdown-file-p file)
                         (stringp my/aaronnote-jupyter-markdown-factory)
                         (not (string-empty-p my/aaronnote-jupyter-markdown-factory)))
                    (concat "?factory="
                            (url-hexify-string my/aaronnote-jupyter-markdown-factory))
                  ""))
         (frag (if (and selector (not (string-empty-p selector)))
                   (concat "#" (url-hexify-string selector))
                 "")))
    (format "http://%s:%d/lab/tree/%s%s%s"
            my/aaronnote-jupyter-host
            my/aaronnote-jupyter-port
            (my/aaronnote-jupyter--encode-path rel)
            query
            frag)))

(defun my/aaronnote-jupyter--split-notebook-target (target)
  "Return (NOTEBOOK . SELECTOR) parsed from local notebook TARGET."
  (let* ((raw (url-unhex-string (string-remove-prefix "file:" (or target ""))))
         (hash-pos (string-match-p "#" raw))
         (hash-selector (and hash-pos (substring raw (1+ hash-pos))))
         (raw (if hash-pos (substring raw 0 hash-pos) raw))
         (at-match (string-match "\\(.+?\\.ipynb\\)@\\(.+\\)\\'" raw)))
    (if at-match
        (cons (match-string 1 raw) (match-string 2 raw))
      (cons raw hash-selector))))

(defun my/aaronnote-jupyter--open-xwidget-url (url)
  "Open URL in the stable Aaronnote Jupyter xwidget buffer."
  (unless (fboundp 'my/xwidget-open-url)
    (require 'init-browser))
  (let ((existing (and (fboundp 'my/xwidget-session-buffer)
                       (my/xwidget-session-buffer
                        my/aaronnote-jupyter--xwidget-session-id))))
    (if (and (buffer-live-p existing)
             (fboundp 'my/xwidget-current-url)
             (equal (my/xwidget-current-url existing) url))
        (progn
          (switch-to-buffer existing)
          existing)
      (my/xwidget-open-url
       url
       :id my/aaronnote-jupyter--xwidget-session-id
       :display 'current
       :reuse-selected t))))

;;;###autoload
(defun my/aaronnote-jupyter-open-url (url)
  "Open URL in the stable Aaronnote Jupyter xwidget buffer."
  (interactive "sAaronnote Jupyter URL: ")
  (my/aaronnote-jupyter--open-xwidget-url url))

(defun my/aaronnote-jupyter--open-url (open &optional ready)
  "Open JupyterLab target described by OPEN."
  (if (or ready (my/aaronnote-jupyter--ready-p))
      (let ((url (if (stringp open) open (my/aaronnote-jupyter-url))))
        (if (or (fboundp 'my/xwidget-open-url)
                (require 'init-browser nil t))
            (my/aaronnote-jupyter--open-xwidget-url url)
          (browse-url url)))
    (my/aaronnote-jupyter--schedule-open-when-ready open)))

(defun my/aaronnote-jupyter--schedule-open-when-ready (open &optional attempt)
  "Open JupyterLab target OPEN after the HTTP server becomes ready."
  (my/aaronnote-jupyter--cancel-open-timer)
  (let ((attempt (or attempt 0)))
    (setq my/aaronnote-jupyter--open-timer
          (run-at-time
           (if (zerop attempt) 0.2 0.5) nil
           (lambda (open attempt)
             (setq my/aaronnote-jupyter--open-timer nil)
             (cond
              ((my/aaronnote-jupyter--ready-p)
               (my/aaronnote-jupyter--open-url open t))
              ((and (my/aaronnote-jupyter-running-p) (< attempt 80))
               (my/aaronnote-jupyter--schedule-open-when-ready open (1+ attempt)))
              (t
               (message "Aaronnote Jupyter did not become ready at %s"
                        (my/aaronnote-jupyter-url)))))
           open attempt))))

(defun my/aaronnote-jupyter--stop-process (process)
  "Request PROCESS to stop, then force-kill it if it stays alive."
  (when (process-live-p process)
    (set-process-query-on-exit-flag process nil)
    (condition-case err
        (interrupt-process process)
      (error
       (message "Aaronnote Jupyter interrupt failed: %s" (error-message-string err))
       (ignore-errors (signal-process process 2))))
    (my/aaronnote-jupyter--cancel-stop-timers)
    (let (timer)
      (setq timer
            (run-at-time
             0.3 nil
             (lambda (proc)
               (when (eq my/aaronnote-jupyter--shutdown-response-timer timer)
                 (setq my/aaronnote-jupyter--shutdown-response-timer nil))
               (when (process-live-p proc)
                 (ignore-errors (process-send-string proc "y\n"))))
             process))
      (setq my/aaronnote-jupyter--shutdown-response-timer timer))
    (let (timer)
      (setq timer
            (run-at-time
             30 nil
             (lambda (proc)
               (when (eq my/aaronnote-jupyter--force-kill-timer timer)
                 (setq my/aaronnote-jupyter--force-kill-timer nil))
               (when (process-live-p proc)
                 (ignore-errors (delete-process proc))))
             process))
      (setq my/aaronnote-jupyter--force-kill-timer timer))))

(defun my/aaronnote-jupyter--sentinel (process event)
  "Track Aaronnote Jupyter PROCESS state changes described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (when (eq process my/aaronnote-jupyter-process)
      (let ((restart-open my/aaronnote-jupyter--restart-open)
            (restart-directory my/aaronnote-jupyter--restart-directory))
        (my/aaronnote-jupyter--cleanup)
        (when restart-open
          (my/aaronnote-jupyter-start restart-open restart-directory))))
    (message "Aaronnote Jupyter %s" (string-trim event))))

(defun my/aaronnote-jupyter--ensure-root (root &optional open)
  "Ensure managed JupyterLab is running with ROOT as cwd.
OPEN is passed to `my/aaronnote-jupyter-start' or remembered across a restart.
Return non-nil when JupyterLab is already running at ROOT."
  (if (my/aaronnote-jupyter-running-p)
      (if (my/aaronnote-jupyter--same-root-p
           my/aaronnote-jupyter-last-directory root)
          t
        (let ((process my/aaronnote-jupyter-process))
          (setq my/aaronnote-jupyter--restart-open open
                my/aaronnote-jupyter--restart-directory root)
          (my/aaronnote-jupyter--stop-process process)
          (message "Restarting Aaronnote Jupyter from %s..."
                   (abbreviate-file-name root))
          nil))
    (my/aaronnote-jupyter-start open root)
    nil))

;;;###autoload
(defun my/aaronnote-jupyter-start (&optional open root)
  "Start managed Aaronnote JupyterLab.
When OPEN is non-nil, open JupyterLab after launch. ROOT overrides the dynamic
root directory."
  (interactive "P")
  (if (my/aaronnote-jupyter-running-p)
      (progn
        (message "Aaronnote Jupyter is already running at %s"
                 (my/aaronnote-jupyter-url))
        (when open
          (my/aaronnote-jupyter--open-url open)))
    (unless (file-executable-p my/aaronnote-jupyter--run-script)
      (user-error "Aaronnote Jupyter run script is not executable: %s"
                  my/aaronnote-jupyter--run-script))
    (unless (file-executable-p
             (expand-file-name ".venv/bin/jupyter-lab" my/aaronnote-jupyter-root))
      (user-error "Aaronnote Jupyter is not bootstrapped; run `npm run jupyter:bootstrap' in lisp/roam/aaronnote"))
    (let* ((root (file-name-as-directory
                  (expand-file-name (or root (my/aaronnote-jupyter--default-directory)))))
           (default-directory root)
           (process-environment (my/aaronnote-jupyter--process-environment))
           (argv (list my/aaronnote-jupyter--run-script root))
           (buffer (get-buffer-create my/aaronnote-jupyter-log-buffer-name))
           (process
            (make-process
             :name "aaronnote-jupyter"
             :buffer buffer
             :command argv
             :coding 'utf-8-unix
             :connection-type 'pty
             :noquery t
             :sentinel #'my/aaronnote-jupyter--sentinel)))
      (set-process-query-on-exit-flag process nil)
      (setq my/aaronnote-jupyter-last-directory root
            my/aaronnote-jupyter-process process)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "[%s] cwd=%s\n"
                          (format-time-string "%F %T")
                          (abbreviate-file-name root)))
          (insert (format "cmd=%s\n"
                          (string-join
                           (mapcar #'shell-quote-argument argv)
                           " ")))
          (dolist (var '("JUPYTER_CONFIG_DIR" "JUPYTER_DATA_DIR"
                         "JUPYTER_RUNTIME_DIR" "PYTHONNOUSERSITE"))
            (insert (format "%s=%s\n"
                            var
                            (my/aaronnote-jupyter--env-get
                             process-environment var))))))
      (message "Starting Aaronnote Jupyter at %s" (my/aaronnote-jupyter-url))
      (when open
        (my/aaronnote-jupyter--schedule-open-when-ready open)))))

;;;###autoload
(defun my/aaronnote-jupyter-start-and-open ()
  "Start managed Aaronnote JupyterLab and open it."
  (interactive)
  (my/aaronnote-jupyter-start t))

;;;###autoload
(defun my/aaronnote-jupyter-stop ()
  "Stop the managed Aaronnote JupyterLab process."
  (interactive)
  (unless (my/aaronnote-jupyter-running-p)
    (my/aaronnote-jupyter--cleanup)
    (user-error "Aaronnote Jupyter is not running"))
  (let ((process my/aaronnote-jupyter-process))
    (setq my/aaronnote-jupyter--restart-open nil)
    (my/aaronnote-jupyter--stop-process process)
    (message "Stopping Aaronnote Jupyter...")))

;;;###autoload
(defun my/aaronnote-jupyter-restart (&optional open)
  "Restart the managed Aaronnote JupyterLab process.
When OPEN is non-nil, open the JupyterLab page afterwards."
  (interactive "P")
  (if (my/aaronnote-jupyter-running-p)
      (let ((process my/aaronnote-jupyter-process))
        (setq my/aaronnote-jupyter--restart-open open
              my/aaronnote-jupyter--restart-directory
              (or my/aaronnote-jupyter-last-directory
                  (my/aaronnote-jupyter--default-directory)))
        (my/aaronnote-jupyter--stop-process process)
        (message "Restarting Aaronnote Jupyter..."))
    (my/aaronnote-jupyter-start open)))

;;;###autoload
(defun my/aaronnote-jupyter-open-log ()
  "Open the managed Aaronnote JupyterLab log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create my/aaronnote-jupyter-log-buffer-name)))

;;;###autoload
(defun my/aaronnote-jupyter-open-path (abs-path &optional selector)
  "Open ABS-PATH in the Aaronnote-owned JupyterLab.
When SELECTOR is non-empty, append it as a URL fragment."
  (interactive "fOpen in Aaronnote Jupyter: ")
  (let* ((file (expand-file-name abs-path))
         (root (my/aaronnote-jupyter--root-for-file file))
         (url (my/aaronnote-jupyter--url-for-file file root selector)))
    (when (my/aaronnote-jupyter--ensure-root root url)
      (my/aaronnote-jupyter--open-url url))))

;;;###autoload
(defun my/aaronnote-jupyter-open-target (target)
  "Open a local notebook TARGET in the Aaronnote-owned JupyterLab.
TARGET may be a path, file: URL, or a path with @/# selector syntax.
Return non-nil when TARGET names a notebook."
  (interactive "sOpen notebook target in Aaronnote Jupyter: ")
  (pcase-let* ((`(,notebook . ,selector)
                (my/aaronnote-jupyter--split-notebook-target target)))
    (when (string-suffix-p ".ipynb" notebook t)
      (my/aaronnote-jupyter-open-path notebook selector)
      t)))

;;;###autoload
(defun my/aaronnote-jupyter-open-root ()
  "Open the Aaronnote-owned JupyterLab root."
  (interactive)
  (let ((root (my/aaronnote-jupyter--default-directory)))
    (when (my/aaronnote-jupyter--ensure-root root t)
      (my/aaronnote-jupyter--open-url t))))

;;;###autoload
(defun my/aaronnote-jupyter-open ()
  "Open managed Aaronnote JupyterLab, preferring the current file."
  (interactive)
  (if buffer-file-name
      (my/aaronnote-jupyter-open-path buffer-file-name)
    (my/aaronnote-jupyter-open-root)))

;;;###autoload
(defun my/aaronnote-jupyter-bootstrap ()
  "Bootstrap the Aaronnote-owned JupyterLab runtime."
  (interactive)
  (let ((default-directory (file-name-directory my/aaronnote-jupyter-root)))
    (compilation-start
     (shell-quote-argument my/aaronnote-jupyter--bootstrap-script)
     nil
     (lambda (_mode) "*aaronnote-jupyter-bootstrap*"))))

;;;###autoload
(defun my/aaronnote-jupyter-doctor ()
  "Run diagnostics for the Aaronnote-owned JupyterLab runtime."
  (interactive)
  (let ((default-directory (file-name-directory my/aaronnote-jupyter-root)))
    (compilation-start
     (shell-quote-argument my/aaronnote-jupyter--doctor-script)
     nil
     (lambda (_mode) "*aaronnote-jupyter-doctor*"))))

(provide 'init-aaronnote-jupyter)
;;; init-aaronnote-jupyter.el ends here
