;;; ai-workbench-cli.el --- Shared CLI-session core for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified CLI-session core for ai-workbench terminal backends.
;; Provides parametric terminal session management (interactive vterm/eat)
;; and headless exec (one-shot process) for CLI AI tools.
;;
;; Tools are registered via `ai-workbench-cli-register-tool'.
;; Adapters (codex, opencode, etc.) register their spec here and expose
;; thin public wrappers that delegate to the generic functions.
;;
;; Session principle:
;;   Interactive vterm sessions: CLI keeps its own session (process + buffer).
;;   Headless exec path: one-shot process, no persistent session needed.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)

(defvar my/terminal-startup-cd-inhibited)
(defvar my/vterm-popup-kind nil)
(defvar my/vterm-popup-title nil)
(defvar vterm-shell)
(defvar vterm-environment)
(defvar eat-terminal)
(defvar eat-term-name)

(declare-function my/vterm-popup-display-buffer "init-vterm-popup" (buffer))
(declare-function turn-off-evil-mode "evil" ())
(declare-function evil-emacs-state "evil" ())
(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm" ())
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function eat-term-send-string "eat" (terminal string))

;; ── Tool registry ─────────────────────────────────────────────────────────────

(defvar ai-workbench-cli--tools nil
  "Alist mapping tool-id symbols to their spec plists.
Each entry is (ID . SPEC) where SPEC is a plist with:
  :name                Display name string
  :executable-var      Symbol of defcustom holding the executable path
  :extra-args-var      Symbol of defcustom holding extra arg list (or nil)
  :terminal-backend-var Symbol of defcustom holding terminal backend (vterm/eat)
  :env-vars            List of environment variable strings for the session
  :buffer-prefix       String prefix for buffer names (e.g. \"codex\")
  :popup-kind          Symbol for the vterm popup kind
  :minor-mode          Minor mode function symbol to activate in terminal buffers
  :exec-args-fn        Function (prompt output-file root) → command string list
  :exec-output         Symbol: \\='file or \\='stdout")

(defun ai-workbench-cli-register-tool (id &rest spec)
  "Register a CLI tool with ID and SPEC plist.
See `ai-workbench-cli--tools' for the expected plist keys."
  (setf (alist-get id ai-workbench-cli--tools) spec))

(defun ai-workbench-cli--spec (id key)
  "Return the KEY value from the registered spec for tool ID."
  (plist-get (alist-get id ai-workbench-cli--tools) key))

(defun ai-workbench-cli--executable (id)
  "Return the resolved executable path for tool ID."
  (let ((var (ai-workbench-cli--spec id :executable-var)))
    (or (and var (boundp var) (stringp (symbol-value var))
             (not (string-empty-p (symbol-value var)))
             (symbol-value var))
        (symbol-name id))))

(defun ai-workbench-cli--extra-args (id)
  "Return the extra arg list for tool ID, or nil."
  (let ((var (ai-workbench-cli--spec id :extra-args-var)))
    (and var (boundp var) (symbol-value var))))

(defun ai-workbench-cli--terminal-backend (id)
  "Return the terminal backend symbol (vterm or eat) for tool ID."
  (let ((var (ai-workbench-cli--spec id :terminal-backend-var)))
    (or (and var (boundp var) (symbol-value var)) 'vterm)))

;; ── Process registry ──────────────────────────────────────────────────────────

(defvar ai-workbench-cli--processes (make-hash-table :test 'equal)
  "Hash-table mapping (ID . ROOT) cons cells to live process objects.")

(defun ai-workbench-cli--proc-key (id root)
  "Return the hash-table key for tool ID and ROOT."
  (cons id root))

(defun ai-workbench-cli--get-process (id root)
  "Return the tracked process for tool ID and ROOT, or nil."
  (gethash (ai-workbench-cli--proc-key id root) ai-workbench-cli--processes))

(defun ai-workbench-cli--set-process (id root process)
  "Track PROCESS for tool ID and ROOT."
  (puthash (ai-workbench-cli--proc-key id root) process ai-workbench-cli--processes))

(defun ai-workbench-cli--remove-process (id root)
  "Remove process tracking for tool ID and ROOT."
  (remhash (ai-workbench-cli--proc-key id root) ai-workbench-cli--processes))

(defun ai-workbench-cli--cleanup-dead-processes (id)
  "Remove dead process entries for tool ID from the registry."
  (maphash
   (lambda (key process)
     (when (and (equal (car key) id)
                (not (process-live-p process)))
       (remhash key ai-workbench-cli--processes)))
   ai-workbench-cli--processes))

(add-hook 'kill-emacs-hook
          (lambda ()
            (maphash
             (lambda (_key process)
               (when (process-live-p process)
                 (delete-process process)))
             ai-workbench-cli--processes)))

;; ── Working directory ─────────────────────────────────────────────────────────

(defun ai-workbench-cli--working-directory (&optional directory)
  "Return DIRECTORY, or infer the current project root."
  (or directory
      (if-let* ((project (project-current nil default-directory)))
          (expand-file-name (project-root project))
        (expand-file-name default-directory))))

;; ── Buffer names ──────────────────────────────────────────────────────────────

(defun ai-workbench-cli--buffer-name (id root)
  "Return the session buffer name for tool ID and ROOT."
  (let ((prefix (or (ai-workbench-cli--spec id :buffer-prefix) (symbol-name id))))
    (format "*%s[%s]*" prefix (file-name-nondirectory (directory-file-name root)))))

(defun ai-workbench-cli-buffer (id &optional project-root)
  "Return the live session buffer for tool ID and PROJECT-ROOT, or nil."
  (let ((root (ai-workbench-cli--working-directory project-root)))
    (get-buffer (ai-workbench-cli--buffer-name id root))))

;; ── Terminal helpers ──────────────────────────────────────────────────────────

(defun ai-workbench-cli--ensure-terminal-backend (id)
  "Ensure the configured terminal backend for tool ID is available."
  (pcase (ai-workbench-cli--terminal-backend id)
    ('vterm
     (unless (featurep 'vterm) (require 'vterm nil t))
     (unless (featurep 'vterm)
       (user-error "The package vterm is not installed")))
    ('eat
     (unless (featurep 'eat) (require 'eat nil t))
     (unless (featurep 'eat)
       (user-error "The package eat is not installed")))
    (tb (user-error "Unsupported terminal backend for %s: %s" id tb))))

(defun ai-workbench-cli--terminal-paste-string (id string)
  "Send STRING using bracketed paste to the current buffer for tool ID."
  (pcase (ai-workbench-cli--terminal-backend id)
    ('vterm (vterm-send-string string t))
    ('eat
     (when eat-terminal
       (eat-term-send-string eat-terminal "\e[200~")
       (eat-term-send-string eat-terminal string)
       (eat-term-send-string eat-terminal "\e[201~")))
    (tb (error "Unsupported terminal backend for %s: %s" id tb))))

(defun ai-workbench-cli--terminal-send-return (id)
  "Send return to the current buffer for tool ID."
  (pcase (ai-workbench-cli--terminal-backend id)
    ('vterm (vterm-send-return))
    ('eat (when eat-terminal (eat-term-send-string eat-terminal "\r")))
    (tb (error "Unsupported terminal backend for %s: %s" id tb))))

;; ── Buffer configuration ──────────────────────────────────────────────────────

(defun ai-workbench-cli--configure-buffer (id buffer project-root)
  "Apply ai-workbench local UI and session state to BUFFER for tool ID."
  (with-current-buffer buffer
    (setq default-directory project-root)
    (let ((kind (ai-workbench-cli--spec id :popup-kind)))
      (when kind (setq-local my/vterm-popup-kind kind)))
    (setq-local my/vterm-popup-title
                (format "%s  %s"
                        (or (ai-workbench-cli--spec id :name) (symbol-name id))
                        (abbreviate-file-name project-root)))
    (when (fboundp 'evil-emacs-state) (evil-emacs-state))
    (when (bound-and-true-p evil-local-mode) (turn-off-evil-mode))
    (let ((mode (ai-workbench-cli--spec id :minor-mode)))
      (when (and mode (fboundp mode)) (funcall mode 1)))))

;; ── Session creation ──────────────────────────────────────────────────────────

(defun ai-workbench-cli--build-command (id)
  "Return the shell command string used to launch tool ID interactively."
  (string-join
   (cons (shell-quote-argument (ai-workbench-cli--executable id))
         (mapcar #'shell-quote-argument (or (ai-workbench-cli--extra-args id) nil)))
   " "))

(defun ai-workbench-cli--create-terminal-session (id buffer-name project-root)
  "Create a terminal session in BUFFER-NAME for tool ID and PROJECT-ROOT.
Returns a (BUFFER . PROCESS) cons cell."
  (ai-workbench-cli--ensure-terminal-backend id)
  (let* ((command-string (ai-workbench-cli--build-command id))
         (default-directory project-root)
         (env-vars (or (ai-workbench-cli--spec id :env-vars)
                       (list "TERM_PROGRAM=emacs"))))
    (pcase (ai-workbench-cli--terminal-backend id)
      ('vterm
       (let* ((vterm-buffer-name buffer-name)
              (vterm-shell command-string)
              (vterm-environment (append env-vars vterm-environment))
              (buffer (let ((my/terminal-startup-cd-inhibited t))
                        (save-window-excursion
                          (vterm vterm-buffer-name)))))
         (unless buffer (error "Failed to create %s vterm buffer" id))
         (ai-workbench-cli--configure-buffer id buffer project-root)
         (let ((process (get-buffer-process buffer)))
           (unless process (error "Failed to get %s vterm process" id))
           (cons buffer process))))
      ('eat
       (let* ((buffer (get-buffer-create buffer-name))
              (eat-term-name "xterm-256color")
              (parts (split-string-shell-command command-string))
              (program (car parts))
              (args (cdr parts)))
         (with-current-buffer buffer
           (unless (eq major-mode 'eat-mode) (eat-mode))
           (setq-local process-environment (append env-vars process-environment))
           (let ((my/terminal-startup-cd-inhibited t))
             (apply #'eat-exec buffer buffer-name program nil args))
           (ai-workbench-cli--configure-buffer id buffer project-root)
           (let ((process (get-buffer-process buffer)))
             (unless process (error "Failed to create %s eat process" id))
             (cons buffer process)))))
      (tb (error "Unsupported terminal backend for %s: %s" id tb)))))

;; ── Session cleanup ───────────────────────────────────────────────────────────

(defun ai-workbench-cli--cleanup-on-exit (id root)
  "Clean up session state for tool ID and ROOT."
  (ai-workbench-cli--remove-process id root)
  (ai-workbench-session-clear-profile-injected id root)
  (let ((buffer (get-buffer (ai-workbench-cli--buffer-name id root))))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

;; ── Public: session lifecycle ─────────────────────────────────────────────────

(defun ai-workbench-cli-available-p (id)
  "Return non-nil when the executable for tool ID is findable."
  (let ((exe (ai-workbench-cli--executable id)))
    (or (file-executable-p exe) (not (null (executable-find exe))))))

(defun ai-workbench-cli-session-live-p (id &optional project-root)
  "Return non-nil when a live session exists for tool ID and PROJECT-ROOT."
  (let* ((root (ai-workbench-cli--working-directory project-root))
         (tracked (ai-workbench-cli--get-process id root))
         (buf-proc (when-let* ((buf (ai-workbench-cli-buffer id root)))
                     (get-buffer-process buf)))
         (live (cond
                ((and tracked (process-live-p tracked)) tracked)
                ((and buf-proc (process-live-p buf-proc)) buf-proc))))
    (when live
      (unless (eq live tracked)
        (ai-workbench-cli--set-process id root live))
      t)))

(defun ai-workbench-cli-ensure-session (id &optional project-root)
  "Ensure a live terminal session exists for tool ID and PROJECT-ROOT.
Returns the session buffer."
  (unless (ai-workbench-cli-available-p id)
    (error "%s executable not found: %s" id (ai-workbench-cli--executable id)))
  (ai-workbench-cli--ensure-terminal-backend id)
  (let* ((root (ai-workbench-cli--working-directory project-root))
         (buffer-name (ai-workbench-cli--buffer-name id root)))
    (ai-workbench-cli--cleanup-dead-processes id)
    (unless (ai-workbench-cli-session-live-p id root)
      (let* ((buf-and-proc
              (ai-workbench-cli--create-terminal-session id buffer-name root))
             (buffer (car buf-and-proc))
             (process (cdr buf-and-proc)))
        (ai-workbench-cli--set-process id root process)
        (set-process-sentinel
         process
         (lambda (_proc event)
           (when (string-match-p "\\(finished\\|exited\\|killed\\|terminated\\)" event)
             (ai-workbench-cli--cleanup-on-exit id root))))
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook
                    (lambda () (ai-workbench-cli--cleanup-on-exit id root))
                    nil t))))
    (ai-workbench-cli-buffer id root)))

(defun ai-workbench-cli-open-buffer (id &optional project-root)
  "Open the terminal buffer for tool ID via the popup window system."
  (if-let* ((buf (ai-workbench-cli-buffer
                  id (ai-workbench-cli--working-directory project-root))))
      (my/vterm-popup-display-buffer buf)
    (user-error "No %s session for this project" id)))

(defun ai-workbench-cli-stop (id &optional project-root)
  "Stop the terminal session for tool ID and PROJECT-ROOT."
  (let ((root (ai-workbench-cli--working-directory project-root)))
    (when-let* ((process (ai-workbench-cli--get-process id root)))
      (when (process-live-p process)
        (delete-process process)))
    (ai-workbench-cli--cleanup-on-exit id root)
    (ai-workbench-session-set-last-status (format "Stopped %s session" id) root)
    (message "ai-workbench stopped %s session" id)))

;; ── Public: prompt dispatch ───────────────────────────────────────────────────

(defun ai-workbench-cli--send-prompt-retry (id prompt root attempts)
  "Send PROMPT to tool ID session in ROOT, retrying up to ATTEMPTS times."
  (if-let* ((buffer (ai-workbench-cli-buffer id root))
            (process (get-buffer-process buffer))
            ((process-live-p process)))
      (with-current-buffer buffer
        (ai-workbench-cli--terminal-paste-string id prompt)
        (sit-for 0.1)
        (ai-workbench-cli--terminal-send-return id))
    (if (> attempts 0)
        (run-with-timer 0.3 nil #'ai-workbench-cli--send-prompt-retry
                        id prompt root (1- attempts))
      (error "%s session did not become ready" id))))

(defun ai-workbench-cli-send-prompt (id prompt &optional project-root)
  "Send PROMPT to the terminal session for tool ID and PROJECT-ROOT."
  (let ((root (ai-workbench-cli--working-directory project-root)))
    (ai-workbench-cli-ensure-session id root)
    (run-with-timer 0.3 nil #'ai-workbench-cli--send-prompt-retry id prompt root 8)))

(defun ai-workbench-cli--draft-prompt-retry (id prompt root attempts)
  "Insert PROMPT into the tool ID session in ROOT without submitting."
  (if-let* ((buffer (ai-workbench-cli-buffer id root))
            (process (get-buffer-process buffer))
            ((process-live-p process)))
      (with-current-buffer buffer
        (ai-workbench-cli--terminal-paste-string id prompt))
    (if (> attempts 0)
        (run-with-timer 0.3 nil #'ai-workbench-cli--draft-prompt-retry
                        id prompt root (1- attempts))
      (error "%s session did not become ready" id))))

(defun ai-workbench-cli-draft-prompt (id prompt &optional project-root)
  "Insert PROMPT into the terminal session for tool ID without submitting."
  (let ((root (ai-workbench-cli--working-directory project-root)))
    (ai-workbench-cli-ensure-session id root)
    (run-with-timer 0.3 nil #'ai-workbench-cli--draft-prompt-retry id prompt root 8)))

;; ── Public: profile bootstrap ─────────────────────────────────────────────────

(defun ai-workbench-cli--cd-prompt (root)
  "Return the cd line prepended to the profile bootstrap for ROOT."
  (format "cd %s"
          (shell-quote-argument (directory-file-name (expand-file-name root)))))

(defun ai-workbench-cli-prime-session (id &optional project-root)
  "Inject the working directory and profile into the tool ID session.
Sends a combined cd+profile bootstrap prompt via bracketed paste so
embedded newlines are preserved and the tool does not round-trip the
cd line before the profile body arrives."
  (let ((root (or project-root default-directory)))
    (unless (ai-workbench-session-profile-injected-p id root)
      (let ((bootstrap (concat (ai-workbench-cli--cd-prompt root)
                               "\n\n"
                               (ai-workbench-profile-build-prompt root))))
        (ai-workbench-cli-send-prompt id bootstrap root)
        (ai-workbench-session-mark-profile-bootstrap-sent id root)
        (ai-workbench-session-mark-profile-injected id root)
        (ai-workbench-session-set-last-status
         (format "%s profile injected" id) root)))))

;; ── Public: headless exec ─────────────────────────────────────────────────────

(defun ai-workbench-cli-exec (id prompt &rest opts)
  "Run a one-shot headless request for tool ID with PROMPT.
OPTS is a plist:
  :root      Working directory (defaults to current project root)
  :callback  Function (result-string) called on success
  :on-error  Function (event-string details-string) called on failure/timeout
  :timeout   Seconds before aborting (default: 180)
Returns the process object so callers can check liveness."
  (let* ((root (or (plist-get opts :root) (ai-workbench-cli--working-directory)))
         (callback (plist-get opts :callback))
         (on-error (plist-get opts :on-error))
         (timeout (or (plist-get opts :timeout) 180))
         (exec-args-fn (ai-workbench-cli--spec id :exec-args-fn))
         (exec-output (or (ai-workbench-cli--spec id :exec-output) 'stdout))
         (output-file (when (eq exec-output 'file)
                        (make-temp-file "ai-workbench-cli-" nil ".txt")))
         (log-buf (generate-new-buffer (format " *ai-workbench-cli-%s*" id)))
         (default-directory root))
    (unless exec-args-fn
      (error "No :exec-args-fn registered for tool %s" id))
    (let* ((command (funcall exec-args-fn prompt output-file root))
           (timer nil)
           (process
            (make-process
             :name (format "ai-workbench-cli-%s" id)
             :buffer log-buf
             :command command
             :coding 'utf-8
             :noquery t
             :sentinel
             (lambda (proc event)
               (when (memq (process-status proc) '(exit signal))
                 (when (timerp timer) (cancel-timer timer))
                 (unwind-protect
                     (if (and (eq (process-status proc) 'exit)
                              (zerop (process-exit-status proc)))
                         (let ((result
                                (ai-workbench-cli--strip-ansi
                                 (pcase exec-output
                                   ('file
                                    (if (and output-file (file-exists-p output-file))
                                        (with-temp-buffer
                                          (insert-file-contents output-file)
                                          (string-trim (buffer-string)))
                                      (error "No output file produced by %s" id)))
                                   ('stdout
                                    (if (buffer-live-p log-buf)
                                        (with-current-buffer log-buf
                                          (string-trim (buffer-string)))
                                      ""))))))
                           (when callback (funcall callback result)))
                       (let ((details (ai-workbench-cli--strip-ansi
                                       (if (buffer-live-p log-buf)
                                           (with-current-buffer log-buf
                                             (string-trim (buffer-string)))
                                         ""))))
                         (when on-error (funcall on-error event details))))
                   (when (and output-file (file-exists-p output-file))
                     (ignore-errors (delete-file output-file)))
                   (when (buffer-live-p log-buf)
                     (kill-buffer log-buf))))))))
      (setq timer
            (run-at-time timeout nil
                         (lambda ()
                           (when (process-live-p process)
                             (delete-process process))
                           (when on-error
                             (funcall on-error
                                      (format "timed out after %ss" timeout)
                                      ""))
                           (when (and output-file (file-exists-p output-file))
                             (ignore-errors (delete-file output-file)))
                           (when (buffer-live-p log-buf)
                             (kill-buffer log-buf)))))
      process)))

;; ── ANSI stripping ────────────────────────────────────────────────────────────

(defun ai-workbench-cli--strip-ansi (str)
  "Remove ANSI/VT100 escape sequences from STR.
CLI tools emit terminal control codes that must be stripped before
the output is shown in an Emacs buffer or passed to the engine."
  (when (stringp str)
    (let ((s str))
      (setq s (replace-regexp-in-string "\033\\][^\007]*\007" "" s))
      (setq s (replace-regexp-in-string "\033\\][^\033]*\033\\\\" "" s))
      (setq s (replace-regexp-in-string "\033\\[[?!>]?[0-9;]*[A-Za-z]" "" s))
      (setq s (replace-regexp-in-string "\033O[A-Za-z]" "" s))
      (setq s (replace-regexp-in-string "\033." "" s))
      s)))

(provide 'ai-workbench-cli)
;;; ai-workbench-cli.el ends here
