;;; tools/agent/interaction-cli.el --- Shared CLI-session core for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified CLI-session core for +agent-interaction terminal backends.
;; Provides parametric terminal session management (interactive Portal/eat)
;; and headless exec (one-shot process) for CLI AI tools.
;;
;; Tools are registered via `+agent-interaction-cli-register-tool'.
;; Adapters (codex, opencode, etc.) register their spec here and expose
;; thin public wrappers that delegate to the generic functions.
;;
;; Session principle:
;;   Interactive Portal sessions: CLI keeps its own session (process + buffer).
;;   Headless exec path: one-shot process, no persistent session needed.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'tools/agent/interaction-session)
(require 'tools/agent/interaction-profile)
(require 'tools/agent/interaction-backend)

(defvar +terminal-startup-cd-inhibited)
(defvar +portal-popup-kind nil)
(defvar +portal-popup-title nil)
(defvar portal-view--target)
(defvar eat-terminal)
(defvar eat-term-name)

(declare-function +portal-popup-display-buffer "term/portal/popup" (buffer))
(declare-function turn-off-evil-mode "evil" ())
(declare-function evil-emacs-state "evil" ())
(declare-function +portal-create "term/portal/config" (&rest args))
(declare-function portal--terminal-paste "portal-terminal" (target text &optional enter))
(declare-function portal-terminal-metadata "portal-terminal" (&optional buffer))
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function eat-term-send-string "eat" (terminal string))

;; ── Tool registry ─────────────────────────────────────────────────────────────

(defvar +agent-interaction-cli--tools nil
  "Alist mapping tool-id symbols to their spec plists.
Each entry is (ID . SPEC) where SPEC is a plist with:
  :name                Display name string
  :executable-var      Symbol of defcustom holding the executable path
  :extra-args-var      Symbol of defcustom holding extra arg list (or nil)
  :terminal-backend-var Symbol of defcustom holding terminal backend (portal/eat)
  :env-vars            List of environment variable strings for the session
  :buffer-prefix       String prefix for buffer names (e.g. \"codex\")
  :popup-kind          Symbol for the Portal popup kind
  :minor-mode          Minor mode function symbol to activate in terminal buffers
  :exec-args-fn        Function (prompt output-file root) → command string list
  :exec-output         Symbol: \\='file or \\='stdout")

(defun +agent-interaction-cli-register-tool (id &rest spec)
  "Register a CLI tool with ID and SPEC plist.
See `+agent-interaction-cli--tools' for the expected plist keys."
  (setf (alist-get id +agent-interaction-cli--tools) spec)
  (+agent-interaction-register-backend
   id
   :label (or (plist-get spec :name) (symbol-name id))
   :generation (gensym (format "%s-" id))
   :capabilities '(:session :send :draft :stop :cancel :headless)
   :authority '(:kind host-cli :sandboxed nil)
   :operations
   (list
    :available-p (lambda () (+agent-interaction-cli-available-p id))
    :live-p (lambda (root) (+agent-interaction-cli-session-live-p id root))
    :ensure (lambda (root) (+agent-interaction-cli-ensure-session id root))
    :open (lambda (root) (+agent-interaction-cli-open-buffer id root))
    :send (lambda (prompt root on-success on-error)
            (+agent-interaction-cli-send-prompt id prompt root on-success on-error))
    :draft (lambda (prompt root on-success on-error)
             (+agent-interaction-cli-draft-prompt id prompt root on-success on-error))
    :stop (lambda (root) (+agent-interaction-cli-stop id root))
    :cancel (lambda (root)
              (if (eq (+agent-interaction-cli--terminal-backend id) 'portal)
                  (+agent-interaction-cli-stop id root)
                (when-let* ((buf (+agent-interaction-cli-buffer id root))
                            (proc (get-buffer-process buf)))
                  (interrupt-process proc))))))
  id)

(defun +agent-interaction-cli--spec (id key)
  "Return the KEY value from the registered spec for tool ID."
  (plist-get (alist-get id +agent-interaction-cli--tools) key))

(defun +agent-interaction-cli--executable (id)
  "Return the resolved executable path for tool ID."
  (let ((var (+agent-interaction-cli--spec id :executable-var)))
    (or (and var (boundp var) (stringp (symbol-value var))
             (not (string-empty-p (symbol-value var)))
             (symbol-value var))
        (symbol-name id))))

(defun +agent-interaction-cli--extra-args (id)
  "Return the extra arg list for tool ID, or nil."
  (let ((var (+agent-interaction-cli--spec id :extra-args-var)))
    (and var (boundp var) (symbol-value var))))

(defun +agent-interaction-cli--terminal-backend (id)
  "Return the terminal backend symbol (portal or eat) for tool ID."
  (let ((var (+agent-interaction-cli--spec id :terminal-backend-var)))
    (or (and var (boundp var) (symbol-value var)) 'portal)))

;; ── Process registry ──────────────────────────────────────────────────────────

(defvar +agent-interaction-cli--processes (make-hash-table :test 'equal)
  "Hash-table mapping (ID . ROOT) cons cells to live process objects.")

(defun +agent-interaction-cli--proc-key (id root)
  "Return the hash-table key for tool ID and ROOT."
  (cons id root))

(defun +agent-interaction-cli--get-process (id root)
  "Return the tracked process for tool ID and ROOT, or nil."
  (gethash (+agent-interaction-cli--proc-key id root) +agent-interaction-cli--processes))

(defun +agent-interaction-cli--set-process (id root process)
  "Track PROCESS for tool ID and ROOT."
  (puthash (+agent-interaction-cli--proc-key id root) process +agent-interaction-cli--processes))

(defun +agent-interaction-cli--remove-process (id root)
  "Remove process tracking for tool ID and ROOT."
  (remhash (+agent-interaction-cli--proc-key id root) +agent-interaction-cli--processes))

(defun +agent-interaction-cli--cleanup-dead-processes (id)
  "Remove dead process entries for tool ID from the registry."
  (maphash
   (lambda (key process)
     (when (and (equal (car key) id)
                (not (process-live-p process)))
       (remhash key +agent-interaction-cli--processes)))
   +agent-interaction-cli--processes))

(add-hook 'kill-emacs-hook
          (lambda ()
            (maphash
             (lambda (_key process)
               (when (and (processp process) (process-live-p process))
                 (delete-process process)))
             +agent-interaction-cli--processes)))

;; ── Working directory ─────────────────────────────────────────────────────────

(defun +agent-interaction-cli--working-directory (&optional directory)
  "Return DIRECTORY, or infer the current project root."
  (or directory
      (if-let* ((project (project-current nil default-directory)))
          (expand-file-name (project-root project))
        (expand-file-name default-directory))))

;; ── Buffer names ──────────────────────────────────────────────────────────────

(defun +agent-interaction-cli--buffer-name (id root)
  "Return the session buffer name for tool ID and ROOT."
  (let ((prefix (or (+agent-interaction-cli--spec id :buffer-prefix) (symbol-name id))))
    (format "*%s[%s]*" prefix (file-name-nondirectory (directory-file-name root)))))

(defun +agent-interaction-cli-buffer (id &optional project-root)
  "Return the live session buffer for tool ID and PROJECT-ROOT, or nil."
  (let ((root (+agent-interaction-cli--working-directory project-root)))
    (get-buffer (+agent-interaction-cli--buffer-name id root))))

;; ── Terminal helpers ──────────────────────────────────────────────────────────

(defun +agent-interaction-cli--ensure-terminal-backend (id)
  "Ensure the configured terminal backend for tool ID is available."
  (pcase (+agent-interaction-cli--terminal-backend id)
    ('portal
     (require 'term/portal/config))
    ('eat
     (unless (featurep 'eat) (require 'eat nil t))
     (unless (featurep 'eat)
       (user-error "The package eat is not installed")))
    (tb (user-error "Unsupported terminal backend for %s: %s" id tb))))

(defun +agent-interaction-cli--terminal-paste-string (id string)
  "Send STRING using bracketed paste to the current buffer for tool ID."
  (pcase (+agent-interaction-cli--terminal-backend id)
    ('portal (portal--terminal-paste portal-view--target string))
    ('eat
     (when eat-terminal
       (eat-term-send-string eat-terminal "\e[200~")
       (eat-term-send-string eat-terminal string)
       (eat-term-send-string eat-terminal "\e[201~")))
    (tb (error "Unsupported terminal backend for %s: %s" id tb))))

(defun +agent-interaction-cli--terminal-send-return (id)
  "Send return to the current buffer for tool ID."
  (pcase (+agent-interaction-cli--terminal-backend id)
    ('portal (portal--terminal-paste portal-view--target "" t))
    ('eat (when eat-terminal (eat-term-send-string eat-terminal "\r")))
    (tb (error "Unsupported terminal backend for %s: %s" id tb))))

;; ── Buffer configuration ──────────────────────────────────────────────────────

(defun +agent-interaction-cli--configure-buffer (id buffer project-root)
  "Apply +agent-interaction local UI and session state to BUFFER for tool ID."
  (with-current-buffer buffer
    (setq default-directory project-root)
    (let ((kind (+agent-interaction-cli--spec id :popup-kind)))
      (when kind (setq-local +portal-popup-kind kind)))
    (setq-local +portal-popup-title
                (format "%s  %s"
                        (or (+agent-interaction-cli--spec id :name) (symbol-name id))
                        (abbreviate-file-name project-root)))
    (unless (derived-mode-p 'portal-terminal-mode)
      (when (fboundp 'evil-emacs-state) (evil-emacs-state))
      (when (bound-and-true-p evil-local-mode) (turn-off-evil-mode)))
    (let ((mode (+agent-interaction-cli--spec id :minor-mode)))
      (when (and mode (fboundp mode)) (funcall mode 1)))))

;; ── Session creation ──────────────────────────────────────────────────────────

(defun +agent-interaction-cli--build-command (id)
  "Return the shell command string used to launch tool ID interactively."
  (string-join
   (cons (shell-quote-argument (+agent-interaction-cli--executable id))
         (mapcar #'shell-quote-argument (or (+agent-interaction-cli--extra-args id) nil)))
   " "))

(defun +agent-interaction-cli--create-terminal-session (id buffer-name project-root)
  "Create a terminal session in BUFFER-NAME for tool ID and PROJECT-ROOT.
Returns a (BUFFER . PROCESS) cons cell."
  (+agent-interaction-cli--ensure-terminal-backend id)
  (let* ((command-string (+agent-interaction-cli--build-command id))
         (default-directory project-root)
         (env-vars (or (+agent-interaction-cli--spec id :env-vars)
                       (list "TERM_PROGRAM=emacs"))))
    (pcase (+agent-interaction-cli--terminal-backend id)
      ('portal
       (let* ((parts (split-string-shell-command command-string))
              (buffer (+portal-create
                       :directory project-root :name buffer-name
                       :command (append (list "env") env-vars parts)
                       :display nil)))
         (+agent-interaction-cli--configure-buffer id buffer project-root)
         (cons buffer nil)))
      ('eat
       (let* ((buffer (get-buffer-create buffer-name))
              (eat-term-name "xterm-256color")
              (parts (split-string-shell-command command-string))
              (program (car parts))
              (args (cdr parts)))
         (with-current-buffer buffer
           (unless (eq major-mode 'eat-mode) (eat-mode))
           (setq-local process-environment (append env-vars process-environment))
           (let ((+terminal-startup-cd-inhibited t))
             (apply #'eat-exec buffer buffer-name program nil args))
           (+agent-interaction-cli--configure-buffer id buffer project-root)
           (let ((process (get-buffer-process buffer)))
             (unless process (error "Failed to create %s eat process" id))
             (cons buffer process)))))
      (tb (error "Unsupported terminal backend for %s: %s" id tb)))))

;; ── Session cleanup ───────────────────────────────────────────────────────────

(defun +agent-interaction-cli--cleanup-on-exit (id root)
  "Clean up session state for tool ID and ROOT."
  (+agent-interaction-cli--remove-process id root)
  (+agent-interaction-session-clear-profile-injected id root)
  (let ((buffer (get-buffer (+agent-interaction-cli--buffer-name id root))))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

;; ── Public: session lifecycle ─────────────────────────────────────────────────

(defun +agent-interaction-cli-available-p (id)
  "Return non-nil when the executable for tool ID is findable."
  (let ((exe (+agent-interaction-cli--executable id)))
    (or (file-executable-p exe) (not (null (executable-find exe))))))

(defun +agent-interaction-cli-session-live-p (id &optional project-root)
  "Return non-nil when a live session exists for tool ID and PROJECT-ROOT."
  (let* ((root (+agent-interaction-cli--working-directory project-root))
         (buffer (+agent-interaction-cli-buffer id root))
         (tracked (+agent-interaction-cli--get-process id root))
         (buf-proc (and buffer (get-buffer-process buffer)))
         (live (cond
                ((eq (+agent-interaction-cli--terminal-backend id) 'portal)
                 (and buffer (portal-terminal-metadata buffer)
                      (not (plist-get (portal-terminal-metadata buffer) :exited))))
                ((and tracked (process-live-p tracked)) tracked)
                ((and buf-proc (process-live-p buf-proc)) buf-proc))))
    (when live
      (when (and (processp live) (not (eq live tracked)))
        (+agent-interaction-cli--set-process id root live))
      t)))

(defun +agent-interaction-cli-ensure-session (id &optional project-root)
  "Ensure a live terminal session exists for tool ID and PROJECT-ROOT.
Returns the session buffer."
  (unless (+agent-interaction-cli-available-p id)
    (error "%s executable not found: %s" id (+agent-interaction-cli--executable id)))
  (+agent-interaction-cli--ensure-terminal-backend id)
  (let* ((root (+agent-interaction-cli--working-directory project-root))
         (buffer-name (+agent-interaction-cli--buffer-name id root)))
    (+agent-interaction-cli--cleanup-dead-processes id)
    (unless (+agent-interaction-cli-session-live-p id root)
      (let* ((buf-and-proc
              (+agent-interaction-cli--create-terminal-session id buffer-name root))
             (buffer (car buf-and-proc))
             (process (cdr buf-and-proc)))
        (when (processp process)
          (+agent-interaction-cli--set-process id root process))
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (+agent-interaction-cli--remove-process id root)
                      (+agent-interaction-session-clear-profile-injected id root))
                    nil t))))
    (+agent-interaction-cli-buffer id root)))

(defun +agent-interaction-cli-open-buffer (id &optional project-root)
  "Open the terminal buffer for tool ID via the popup window system."
  (if-let* ((buf (+agent-interaction-cli-buffer
                  id (+agent-interaction-cli--working-directory project-root))))
      (+portal-popup-display-buffer buf)
    (user-error "No %s session for this project" id)))

(defun +agent-interaction-cli-stop (id &optional project-root)
  "Stop the terminal session for tool ID and PROJECT-ROOT."
  (let ((root (+agent-interaction-cli--working-directory project-root)))
    (when (eq (+agent-interaction-cli--terminal-backend id) 'portal)
      (when-let* ((buffer (+agent-interaction-cli-buffer id root)))
        (kill-buffer buffer)))
    (when-let* ((process (+agent-interaction-cli--get-process id root)))
      (when (process-live-p process)
        (delete-process process)))
    (+agent-interaction-cli--cleanup-on-exit id root)
    (+agent-interaction-session-set-last-status (format "Stopped %s session" id) root)
    (message "+agent-interaction stopped %s session" id)))

;; ── Public: prompt dispatch ───────────────────────────────────────────────────

(defun +agent-interaction-cli--send-prompt-retry (id prompt root attempts on-success on-error)
  "Send PROMPT to tool ID session in ROOT, retrying up to ATTEMPTS times."
  (if-let* ((buffer (+agent-interaction-cli-buffer id root))
            ((+agent-interaction-cli-session-live-p id root)))
      (condition-case err
          (with-current-buffer buffer
            (+agent-interaction-cli--terminal-paste-string id prompt)
            (sit-for 0.1)
            (+agent-interaction-cli--terminal-send-return id)
            (when on-success (funcall on-success)))
        (error
         (when on-error (funcall on-error (error-message-string err)))
         (signal (car err) (cdr err))))
    (if (> attempts 0)
        (run-with-timer 0.3 nil #'+agent-interaction-cli--send-prompt-retry
                        id prompt root (1- attempts) on-success on-error)
      (let ((message (format "%s session did not become ready" id)))
        (when on-error (funcall on-error message))
        (error "%s" message)))))

(defun +agent-interaction-cli-send-prompt (id prompt &optional project-root on-success on-error)
  "Send PROMPT to the terminal session for tool ID and PROJECT-ROOT."
  (let ((root (+agent-interaction-cli--working-directory project-root)))
    (+agent-interaction-cli-ensure-session id root)
    (run-with-timer 0.3 nil #'+agent-interaction-cli--send-prompt-retry
                    id prompt root 8 on-success on-error)))

(defun +agent-interaction-cli--draft-prompt-retry (id prompt root attempts on-success on-error)
  "Insert PROMPT into the tool ID session in ROOT without submitting."
  (if-let* ((buffer (+agent-interaction-cli-buffer id root))
            ((+agent-interaction-cli-session-live-p id root)))
      (condition-case err
          (with-current-buffer buffer
            (+agent-interaction-cli--terminal-paste-string id prompt)
            (when on-success (funcall on-success)))
        (error
         (when on-error (funcall on-error (error-message-string err)))
         (signal (car err) (cdr err))))
    (if (> attempts 0)
        (run-with-timer 0.3 nil #'+agent-interaction-cli--draft-prompt-retry
                        id prompt root (1- attempts) on-success on-error)
      (let ((message (format "%s session did not become ready" id)))
        (when on-error (funcall on-error message))
        (error "%s" message)))))

(defun +agent-interaction-cli-draft-prompt (id prompt &optional project-root on-success on-error)
  "Insert PROMPT into the terminal session for tool ID without submitting."
  (let ((root (+agent-interaction-cli--working-directory project-root)))
    (+agent-interaction-cli-ensure-session id root)
    (run-with-timer 0.3 nil #'+agent-interaction-cli--draft-prompt-retry
                    id prompt root 8 on-success on-error)))

;; ── Public: profile bootstrap ─────────────────────────────────────────────────

(defun +agent-interaction-cli--cd-prompt (root)
  "Return the cd line prepended to the profile bootstrap for ROOT."
  (format "cd %s"
          (shell-quote-argument (directory-file-name (expand-file-name root)))))

(defun +agent-interaction-cli-prime-session (id &optional project-root)
  "Inject the working directory and profile into the tool ID session.
Sends a combined cd+profile bootstrap prompt via bracketed paste so
embedded newlines are preserved and the tool does not round-trip the
cd line before the profile body arrives."
  (let ((root (or project-root default-directory)))
    (unless (+agent-interaction-session-profile-injected-p id root)
      (let ((bootstrap (concat (+agent-interaction-cli--cd-prompt root)
                               "\n\n"
                               (+agent-interaction-profile-build-prompt root))))
        (+agent-interaction-cli-send-prompt
         id bootstrap root
         (lambda ()
           (+agent-interaction-session-mark-profile-bootstrap-sent id root)
           (+agent-interaction-session-mark-profile-injected id root)
           (+agent-interaction-session-set-last-status
            (format "%s profile injected" id) root)))))))

;; ── Public: headless exec ─────────────────────────────────────────────────────

(defun +agent-interaction-cli-exec (id prompt &rest opts)
  "Run a one-shot headless request for tool ID with PROMPT.
OPTS is a plist:
  :root      Working directory (defaults to current project root)
  :callback  Function (result-string) called on success
  :on-error  Function (event-string details-string) called on failure/timeout
  :timeout   Seconds before aborting (default: 180)
Returns the process object so callers can check liveness."
  (let* ((root (or (plist-get opts :root) (+agent-interaction-cli--working-directory)))
         (callback (plist-get opts :callback))
         (on-error (plist-get opts :on-error))
         (timeout (or (plist-get opts :timeout) 180))
         (exec-args-fn (+agent-interaction-cli--spec id :exec-args-fn))
         (exec-output (or (+agent-interaction-cli--spec id :exec-output) 'stdout))
         (output-file (when (eq exec-output 'file)
                        (make-temp-file "+agent-interaction-cli-" nil ".txt")))
         (log-buf (generate-new-buffer (format " *+agent-interaction-cli-%s*" id)))
         (default-directory root))
    (unless exec-args-fn
      (error "No :exec-args-fn registered for tool %s" id))
    (let* ((command (funcall exec-args-fn prompt output-file root))
           (timer nil)
           (process
            (make-process
             :name (format "+agent-interaction-cli-%s" id)
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
                                (+agent-interaction-cli--strip-ansi
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
                       (let ((details (+agent-interaction-cli--strip-ansi
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

(defun +agent-interaction-cli--strip-ansi (str)
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

(provide 'tools/agent/interaction-cli)
;;; tools/agent/interaction-cli.el ends here
