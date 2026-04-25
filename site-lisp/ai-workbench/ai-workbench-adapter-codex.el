;;; ai-workbench-adapter-codex.el --- Codex adapter for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module integrates Codex interactive terminal workflows into
;; ai-workbench using the same style as the Claude adapter: one process and
;; one terminal buffer per project, popup display delegated to
;; `init-vterm-popup.el', and prompt injection done directly through the
;; terminal backend.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)

(defvar my/terminal-startup-cd-inhibited)
(defvar vterm-shell)
(defvar vterm-environment)
(defvar eat-terminal)
(defvar eat-term-name)

(declare-function my/vterm-popup-display-buffer "init-vterm-popup" (buffer))
(declare-function turn-off-evil-mode "evil" ())
(declare-function evil-emacs-state "evil" ())
(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-return "vterm" ())
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function eat-term-send-string "eat" (terminal string))

(defgroup ai-workbench-codex nil
  "Codex terminal integration for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-codex-")

(defcustom ai-workbench-codex-executable "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-workbench-codex)

(defcustom ai-workbench-codex-extra-args nil
  "Additional command line arguments passed to Codex."
  :type '(repeat string)
  :group 'ai-workbench-codex)

(defcustom ai-workbench-codex-terminal-backend 'vterm
  "Terminal backend used for Codex sessions."
  :type '(choice (const vterm) (const eat))
  :group 'ai-workbench-codex)

(defcustom ai-workbench-codex-buffer-name-function
  #'ai-workbench-codex--default-buffer-name
  "Function used to compute Codex buffer names."
  :type 'function
  :group 'ai-workbench-codex)

(defvar ai-workbench-codex-use-exec nil
  "Deprecated toggle kept for compatibility. Codex defaults to terminal mode.")

(defvar ai-workbench-codex--processes (make-hash-table :test 'equal)
  "Hashtable mapping project roots to active Codex processes.")

(define-minor-mode ai-workbench-codex-mode
  "Minor mode marker for ai-workbench Codex terminal buffers."
  :init-value nil
  :lighter " AI-Codex")

(defun ai-workbench-codex--configure-buffer (buffer project-root)
  "Apply ai-workbench Codex local UI and state to BUFFER for PROJECT-ROOT."
  (with-current-buffer buffer
    (setq default-directory project-root)
    (setq-local my/vterm-popup-kind 'ai-codex)
    (setq-local my/vterm-popup-title
                (format "Codex  %s"
                        (abbreviate-file-name project-root)))
    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))
    (when (bound-and-true-p evil-local-mode)
      (turn-off-evil-mode))
    (ai-workbench-codex-mode 1)))

(defun ai-workbench-codex--default-buffer-name (directory)
  "Return the default Codex buffer name for DIRECTORY."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun ai-workbench-codex--working-directory (&optional directory)
  "Return DIRECTORY or infer the current project directory."
  (or directory
      (if-let* ((project (project-current nil default-directory)))
          (expand-file-name (project-root project))
        (expand-file-name default-directory))))

(defun ai-workbench-codex-buffer-name (&optional directory)
  "Return the Codex buffer name for DIRECTORY."
  (funcall ai-workbench-codex-buffer-name-function
           (ai-workbench-codex--working-directory directory)))

(defun ai-workbench-codex-buffer (&optional project-root)
  "Return the Codex session buffer for PROJECT-ROOT."
  (get-buffer (ai-workbench-codex-buffer-name project-root)))

(defun ai-workbench-codex--get-process (&optional project-root)
  "Return the tracked Codex process for PROJECT-ROOT."
  (gethash (ai-workbench-codex--working-directory project-root)
           ai-workbench-codex--processes))

(defun ai-workbench-codex--set-process (process &optional project-root)
  "Track PROCESS for PROJECT-ROOT."
  (puthash (ai-workbench-codex--working-directory project-root)
           process
           ai-workbench-codex--processes))

(defun ai-workbench-codex--cleanup-dead-processes ()
  "Remove dead Codex process table entries."
  (maphash
   (lambda (directory process)
     (unless (process-live-p process)
       (remhash directory ai-workbench-codex--processes)))
   ai-workbench-codex--processes))

(defun ai-workbench-codex--cleanup-on-exit (project-root)
  "Clean up Codex state for PROJECT-ROOT."
  (remhash project-root ai-workbench-codex--processes)
  (ai-workbench-session-clear-profile-injected 'codex project-root)
  (let ((buffer (get-buffer (ai-workbench-codex-buffer-name project-root))))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (maphash
             (lambda (_directory process)
               (when (process-live-p process)
                 (delete-process process)))
             ai-workbench-codex--processes)))

(defun ai-workbench-codex--terminal-ensure-backend ()
  "Ensure the configured Codex terminal backend is available."
  (pcase ai-workbench-codex-terminal-backend
    ('vterm
     (unless (featurep 'vterm)
       (require 'vterm nil t))
     (unless (featurep 'vterm)
       (user-error "The package vterm is not installed")))
    ('eat
     (unless (featurep 'eat)
       (require 'eat nil t))
     (unless (featurep 'eat)
       (user-error "The package eat is not installed")))
    (_
     (user-error "Unsupported Codex terminal backend: %s"
                 ai-workbench-codex-terminal-backend))))

(defun ai-workbench-codex--terminal-send-string (string)
  "Send STRING to the current Codex terminal buffer."
  (pcase ai-workbench-codex-terminal-backend
    ('vterm
     (vterm-send-string string))
    ('eat
     (when eat-terminal
       (eat-term-send-string eat-terminal string)))
    (_
     (error "Unsupported Codex terminal backend: %s"
            ai-workbench-codex-terminal-backend))))

(defun ai-workbench-codex--terminal-send-return ()
  "Send return to the current Codex terminal buffer."
  (pcase ai-workbench-codex-terminal-backend
    ('vterm
     (vterm-send-return))
    ('eat
     (when eat-terminal
       (eat-term-send-string eat-terminal "\r")))
    (_
     (error "Unsupported Codex terminal backend: %s"
            ai-workbench-codex-terminal-backend))))

(defun ai-workbench-codex--build-command ()
  "Return the Codex command string."
  (string-join
   (cons ai-workbench-codex-executable
         (mapcar #'shell-quote-argument ai-workbench-codex-extra-args))
   " "))

(defun ai-workbench-codex--create-terminal-session (buffer-name project-root)
  "Create a Codex terminal session in BUFFER-NAME for PROJECT-ROOT."
  (ai-workbench-codex--terminal-ensure-backend)
  (let* ((command-string (ai-workbench-codex--build-command))
         (default-directory project-root)
         (env-vars (list "TERM_PROGRAM=emacs"
                         "FORCE_CODE_TERMINAL=true")))
    (pcase ai-workbench-codex-terminal-backend
      ('vterm
       (let* ((vterm-buffer-name buffer-name)
              (vterm-shell command-string)
              (vterm-environment (append env-vars vterm-environment))
              (buffer (let ((my/terminal-startup-cd-inhibited t))
                        (save-window-excursion
                          (vterm vterm-buffer-name)))))
         (unless buffer
           (error "Failed to create Codex vterm buffer"))
         (ai-workbench-codex--configure-buffer buffer project-root)
         (let ((process (get-buffer-process buffer)))
           (unless process
             (error "Failed to get Codex vterm process"))
           (cons buffer process))))
      ('eat
       (let* ((buffer (get-buffer-create buffer-name))
              (eat-term-name "xterm-256color")
              (parts (split-string-shell-command command-string))
              (program (car parts))
              (args (cdr parts)))
         (with-current-buffer buffer
           (unless (eq major-mode 'eat-mode)
             (eat-mode))
            (setq-local process-environment (append env-vars process-environment))
           (let ((my/terminal-startup-cd-inhibited t))
             (apply #'eat-exec buffer buffer-name program nil args))
           (ai-workbench-codex--configure-buffer buffer project-root)
           (let ((process (get-buffer-process buffer)))
             (unless process
               (error "Failed to create Codex eat process"))
             (cons buffer process)))))
      (_
       (error "Unsupported Codex terminal backend: %s"
              ai-workbench-codex-terminal-backend)))))

(defun ai-workbench-codex-available-p ()
  "Return non-nil when the Codex executable is available."
  (or (file-executable-p ai-workbench-codex-executable)
      (executable-find ai-workbench-codex-executable)))

(defun ai-workbench-codex-load ()
  "Validate the Codex executable and terminal backend."
  (unless (ai-workbench-codex-available-p)
    (error "Codex executable not found: %s" ai-workbench-codex-executable))
  (ai-workbench-codex--terminal-ensure-backend))

(defun ai-workbench-codex-open-buffer ()
  "Open the current project's Codex session buffer via popup window logic."
  (interactive)
  (if-let* ((buffer (ai-workbench-codex-buffer (ai-workbench-project-root))))
      (my/vterm-popup-display-buffer buffer)
    (user-error "No Codex session for this project")))

(defun ai-workbench-codex-session-live-p (&optional project-root)
  "Return non-nil when the Codex session for PROJECT-ROOT is live."
  (when-let* ((process (ai-workbench-codex--get-process project-root)))
    (process-live-p process)))

(defun ai-workbench-codex-open-active-buffer ()
  "Open the active Codex buffer."
  (interactive)
  (ai-workbench-codex-open-buffer))

(defun ai-workbench-codex-ensure-session (&optional project-root)
  "Ensure a live Codex session exists for PROJECT-ROOT."
  (let* ((root (ai-workbench-codex--working-directory project-root))
         (buffer-name (ai-workbench-codex-buffer-name root)))
    (ai-workbench-codex-load)
    (ai-workbench-codex--cleanup-dead-processes)
    (unless (ai-workbench-codex-session-live-p root)
      (let* ((buffer-and-process
              (ai-workbench-codex--create-terminal-session buffer-name root))
             (buffer (car buffer-and-process))
             (process (cdr buffer-and-process)))
        (ai-workbench-codex--set-process process root)
        (set-process-sentinel
         process
         (lambda (_process event)
           (when (or (string-match "finished" event)
                     (string-match "exited" event)
                     (string-match "killed" event)
                     (string-match "terminated" event))
             (ai-workbench-codex--cleanup-on-exit root))))
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (ai-workbench-codex--cleanup-on-exit root))
                    nil t))))
    (ai-workbench-codex-buffer root)))

(defun ai-workbench-codex--profile-prompt (&optional project-root)
  "Return the Codex profile prompt for PROJECT-ROOT."
  (ai-workbench-profile-build-prompt project-root))

(defun ai-workbench-codex-prime-session (&optional project-root)
  "Inject workdir/profile context into Codex once for PROJECT-ROOT."
  (let ((root (or project-root default-directory)))
    (unless (ai-workbench-session-profile-injected-p 'codex root)
      (ai-workbench-codex-send-prompt
       (ai-workbench-codex--profile-prompt root)
       root)
      (ai-workbench-session-mark-profile-injected 'codex root)
      (ai-workbench-session-set-last-status "Codex profile injected" root))))

(defun ai-workbench-codex--send-prompt-retry (prompt project-root attempts)
  "Send PROMPT for PROJECT-ROOT, retrying up to ATTEMPTS times."
  (if-let* ((buffer (ai-workbench-codex-buffer project-root))
            (process (get-buffer-process buffer))
            ((process-live-p process)))
      (with-current-buffer buffer
        (ai-workbench-codex--terminal-send-string prompt)
        (sit-for 0.1)
        (ai-workbench-codex--terminal-send-return))
    (if (> attempts 0)
        (run-with-timer 0.3 nil
                        #'ai-workbench-codex--send-prompt-retry
                        prompt project-root (1- attempts))
      (error "Codex session did not become ready"))))

(defun ai-workbench-codex-send-prompt (prompt &optional project-root)
  "Send PROMPT to Codex, starting a session for PROJECT-ROOT when needed."
  (let ((root (ai-workbench-codex--working-directory project-root)))
    (ai-workbench-codex-ensure-session root)
    (run-with-timer 0.3 nil
                    #'ai-workbench-codex--send-prompt-retry
                    prompt root 8)))

(defun ai-workbench-codex--draft-prompt-retry (prompt project-root attempts)
  "Insert PROMPT into Codex for PROJECT-ROOT without pressing return."
  (if-let* ((buffer (ai-workbench-codex-buffer project-root))
            (process (get-buffer-process buffer))
            ((process-live-p process)))
      (with-current-buffer buffer
        (ai-workbench-codex--terminal-send-string prompt))
    (if (> attempts 0)
        (run-with-timer 0.3 nil
                        #'ai-workbench-codex--draft-prompt-retry
                        prompt project-root (1- attempts))
      (error "Codex session did not become ready"))))

(defun ai-workbench-codex-draft-prompt (prompt &optional project-root)
  "Insert PROMPT into Codex, starting a session for PROJECT-ROOT when needed.
The prompt is inserted but not submitted."
  (let ((root (ai-workbench-codex--working-directory project-root)))
    (ai-workbench-codex-ensure-session root)
    (run-with-timer 0.3 nil
                    #'ai-workbench-codex--draft-prompt-retry
                    prompt root 8)))

(defun ai-workbench-codex-stop (&optional project-root)
  "Stop the active Codex run for PROJECT-ROOT."
  (interactive)
  (let ((root (ai-workbench-codex--working-directory project-root)))
    (when-let* ((process (ai-workbench-codex--get-process root)))
      (when (process-live-p process)
        (delete-process process)))
    (ai-workbench-codex--cleanup-on-exit root)
    (ai-workbench-session-set-last-status "Stopped active Codex run" root)
    (message "ai-workbench stopped Codex run")))

(defun ai-workbench-codex-execution-mode ()
  "Return the current ai-workbench Codex execution mode."
  'terminal)

(defun ai-workbench-codex-toggle-execution-mode ()
  "Keep compatibility while no longer defaulting to exec mode."
  (interactive)
  (setq ai-workbench-codex-use-exec nil)
  (message "ai-workbench Codex mode: terminal"))

(provide 'ai-workbench-adapter-codex)
;;; ai-workbench-adapter-codex.el ends here
