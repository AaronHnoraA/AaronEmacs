;;; init-ai-ide.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Noema is the single Emacs-native AI/research entry layer:
;;
;;   gptel source      arbitrary-buffer compose/context/preset/rewrite UI
;;   agent-shell/acp   structured external-agent sessions and permissions
;;   Magent            queue, ledger, tools and optional gptel-backed agent
;;
;; agent-shell/acp/shell-maker are pristine package-vc dependencies, pinned as
;; one audited group. gptel and Magent remain embedded. C-c A is the prefix.
;;
;; Every agent session -- a `.noema' Run's, the popup pool's, one started here
;; and a bare `M-x agent-shell' -- is registered per project by
;; `noema-agent-acp-adopt', so one list manages them and any of them can be
;; given editor context by name.  `noema-context' hands the shared gptel
;; selection to a chosen session as references, never as copied text.

;;; Code:

(require 'config)
(require 'init-package-utils)
(require 'seq)

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/noema/lisp")))
(require 'noema-upstream)

;; Install missing dependencies only; normal startup does no network access.
;; Migration does not upgrade versions or touch currently running sessions.
(my/package-ensure-vc 'acp "https://github.com/xenodium/acp.el"
                      "242cef63d76cc1073485847f67a21f6d8406d158")
(my/package-ensure-vc 'shell-maker "https://github.com/xenodium/shell-maker"
                      "f448a74a8eded23aa42f8d60a41c5d8d3a183d07")
(my/package-ensure-vc 'agent-shell "https://github.com/xenodium/agent-shell"
                      "55d7148505da2433a30b1228092e17b0775ffa25")

;;; ── Agent placement: every agent-shell session, local or remote ─────────
;;
;; An ACP agent runs where its workspace lives.  This is the one host boundary
;; for all agent-shell entry points (Noema Runs, the popup pool, a bare
;; `M-x agent-shell'), and it treats local as target `local':
;;
;;   process   acp.el starts the agent with `:file-handler' from
;;             `default-directory'.  A client-accessible directory spawns it
;;             here; a logical /fs:TARGET: directory routes `make-process'
;;             through the Remote framework to TARGET, and agent-shell's
;;             `executable-find' check resolves the agent on TARGET's PATH.
;;   paths     ACP carries target-native paths.  The resolver below maps
;;             Emacs names to them and back into the session's target.

(declare-function remote-canonicalize-file-name "remote-fs" (file-name &optional directory))
(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-make-file-name "remote-fs" (target-id localname))
(defvar agent-shell-path-resolver-function)
(defvar noema-agent-acp-process-directory-function)
(defvar noema-agent-acp-agent-file-function)

(defun my/agent-shell-emacs-file-name (logical)
  "Return the Emacs spelling of LOGICAL: native when this machine shares it."
  (or (remote-client-file-name logical) logical))

(defun my/agent-shell-process-directory (directory)
  "Return the directory an agent process for DIRECTORY starts in.
The workspace keeps one logical identity; the Remote framework decides whether
the process runs here or on the directory's target.  An unreachable directory
is an error: shell-maker would otherwise quietly start the agent in the
local home directory, on the wrong machine."
  (let ((process (file-name-as-directory
                  (my/agent-shell-emacs-file-name
                   (remote-canonicalize-file-name (expand-file-name directory))))))
    (unless (condition-case nil (file-directory-p process) (error nil))
      (user-error "Agent workspace is not reachable: %s" process))
    process))

(defun my/agent-shell-resolve-path (path)
  "Map PATH across the ACP boundary in either direction.
An Emacs name becomes the target-native path the agent sees.  A native path
from the agent is placed on the session's target (the current buffer's), and
comes back in its Emacs spelling.  On target `local' both are the identity."
  (if (not (and (stringp path) (file-name-absolute-p path)))
      path
    (let* ((logical (remote-canonicalize-file-name path))
           (native (remote-file-local-name logical)))
      (if (not (equal native path))
          native
        (my/agent-shell-emacs-file-name
         (remote-make-file-name
          (remote-file-name-target default-directory) path))))))

(defun my/agent-shell-agent-file-name (file session)
  "Return FILE as SESSION's agent opens it, or nil when it cannot.
SESSION nil is an agent on this machine.  The agent reaches FILE only when
both live on the same target; the path is then FILE's native name there."
  (let* ((logical (remote-canonicalize-file-name (expand-file-name file)))
         (agent-target
          (remote-file-name-target
           (if session
               (buffer-local-value 'default-directory session)
             temporary-file-directory))))
    (when (equal (remote-file-name-target logical) agent-target)
      (remote-file-local-name logical))))

(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-environment-ensure "remote-environment"
                  (&optional context force callback))
(declare-function remote-context-target-id "remote-core" (context))
(defvar remote-buffer-environment)

(defvar my/agent-shell--lookup-target nil
  "Target whose environment the most recent agent client was resolved in.
agent-shell kills the shell buffer before reporting a missing executable, so
the report cannot ask that buffer.")

(defun my/agent-shell-apply-workspace-environment (&rest _)
  "Give the agent its workspace environment before the client starts.
The capsule is the one a source buffer of the same workspace gets: host path
profiles, the target environment and project providers such as direnv.  The
agent executable is looked up, and its process started, with that PATH on
target `local' exactly as on a remote target; Emacs's global PATH is never
the source.  Resolution failures are reported, not fatal."
  (setq my/agent-shell--lookup-target
        (ignore-errors
          (remote-context-target-id (remote-context default-directory))))
  ;; Only an agent this machine spawns natively reads its environment from
  ;; this buffer.  A routed agent gets the target capsule from the process
  ;; route and `exec-path' handler, while the buffer keeps doing client work
  ;; -- agent-shell's caches and history expand `~' -- so projecting the
  ;; target's HOME here would point those at the target's home on this Mac.
  (unless (or (bound-and-true-p remote-buffer-environment)
              (not (remote-client-file-name
                    (remote-canonicalize-file-name default-directory))))
    (condition-case error
        (remote-environment-ensure (remote-context default-directory))
      (error
       (message "Agent environment for %s unavailable: %s"
                (abbreviate-file-name default-directory)
                (error-message-string error))))))

(defun my/agent-shell-missing-executable-a (message)
  "Name the target whose environment MESSAGE's executable was looked up in."
  (let ((target my/agent-shell--lookup-target))
    (if target
        (format "%s\n[Remote] Looked up on target `%s' with its workspace environment (PATH, direnv); install it there or add it to that environment."
                message target)
      message)))

(defun my/agent-shell-in-session-buffer-a (orig &rest args)
  "Run agent-shell's ACP request handler ORIG with ARGS in its own session.
acp.el dispatches from a timer, so the current buffer is arbitrary; paths the
agent sends must resolve against the session's target."
  (let ((buffer (map-elt (plist-get args :state) :buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer (apply orig args))
      (apply orig args))))

(defvar agent-shell--transcript-file)
(defvar agent-shell--state)
(defvar agent-shell-transcript-file-path-function)
(defvar shell-maker-prompt-before-killing-buffer)
(defvar shell-maker--config)
(defvar company-backends)
(defvar company-idle-delay)
(defvar company-minimum-prefix-length)
(declare-function noema-agent-acp-start "noema-agent-acp" (&rest args))
(declare-function noema-agent-acp-mark-session-buffer "noema-agent-acp" (buffer name agent directory))
(declare-function agent-shell-cwd "agent-shell" ())
(declare-function agent-shell--display-buffer "agent-shell" (buffer))
(declare-function agent-shell--shutdown "agent-shell" ())
(declare-function agent-shell--update-fragment "agent-shell" (&rest args))
(declare-function agent-shell--emit-event "agent-shell" (&rest args))
(declare-function agent-shell--finish-output "agent-shell" (&rest args))
(declare-function agent-shell--start "agent-shell" (&rest args))
(declare-function agent-shell-restart "agent-shell" (&rest args))
(declare-function agent-shell--command-completion-at-point "agent-shell-completion" ())
(declare-function agent-shell--trigger-completion-at-point "agent-shell-completion" ())
(declare-function shell-maker-busy "shell-maker" ())
(defvar-local my/agent-shell--requested-session-id nil
  "ACP session ID requested by this frontend before session/load completes.")
(defvar-local my/agent-shell--task-busy nil
  "Non-nil when this frontend could not acquire its requested Codex task.")

(defun my/agent-shell--session-context (directory)
  "Return DIRECTORY's workspace and execution target as one logical identity.
The Remote framework maps native, TRAMP and /fs: names to this identity."
  (directory-file-name
   (remote-canonicalize-file-name (expand-file-name directory))))

(defun my/agent-shell-execution-live-p (buffer)
  "Return non-nil when BUFFER still owns a live ACP execution process."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'agent-shell-mode)
              (let ((process (map-nested-elt agent-shell--state
                                             '(:client :process))))
                (and (processp process) (process-live-p process)))))))

(defun my/agent-shell--find-execution (session-id config directory &optional exclude)
  "Find a live frontend for SESSION-ID, CONFIG and DIRECTORY, except EXCLUDE.
The ACP session ID is distinct from the Emacs buffer and OS process IDs."
  (let ((context (my/agent-shell--session-context directory))
        (agent (map-elt config :identifier)))
    (seq-find
     (lambda (buffer)
       (and (not (eq buffer exclude))
            (my/agent-shell-execution-live-p buffer)
            (with-current-buffer buffer
              (and (equal agent (map-nested-elt agent-shell--state
                                                '(:agent-config :identifier)))
                   (equal context (my/agent-shell--session-context default-directory))
                   (equal session-id
                          (or (map-nested-elt agent-shell--state '(:session :id))
                              my/agent-shell--requested-session-id))))))
     (buffer-list))))

(defun my/agent-shell--reuse-session-a (original &rest args)
  "Reuse a live matching ACP execution before ORIGINAL starts another process."
  (let* ((session-id (plist-get args :session-id))
         (existing (and (eq (map-elt (plist-get args :config) :identifier) 'codex)
                        (stringp session-id)
                        (my/agent-shell--find-execution
                         session-id (plist-get args :config) (agent-shell-cwd)))))
    (if existing
        (progn
          (unless (plist-get args :no-focus)
            (agent-shell--display-buffer existing))
          existing)
      (let ((buffer (apply original args)))
        (when (and (buffer-live-p buffer) (stringp session-id))
          (with-current-buffer buffer
            (setq-local my/agent-shell--requested-session-id session-id)
            (add-hook 'kill-buffer-hook #'my/agent-shell--forget-session nil t)))
        buffer))))

(defun my/agent-shell--forget-session ()
  "Forget the pending ownership claim when its frontend is destroyed."
  (setq my/agent-shell--requested-session-id nil
        my/agent-shell--task-busy nil))

(defun my/agent-shell--task-busy-p (acp-error)
  "Recognize a Codex task ownership conflict in ACP-ERROR.
ACP has no standardized TASK_BUSY code.  Prefer a structured provider code
when present; the exact Codex message is the isolated compatibility fallback."
  (let* ((data (map-elt acp-error 'data))
         (code (or (and (or (listp data) (hash-table-p data))
                        (map-elt data 'code))
                   (and (or (listp data) (hash-table-p data))
                        (map-elt data 'errorCode))
                   (map-elt acp-error 'code)))
         (message (or (map-elt acp-error 'message)
                      (and (or (listp data) (hash-table-p data))
                           (map-elt data 'message))
                      (and (stringp data) data))))
    (or (member code '("TASK_BUSY" "task_busy" TASK_BUSY task_busy))
        (and (stringp message)
             (string-match-p
              "Another Codex session is using this task\\.?" message)))))

(defun my/agent-shell--handle-task-busy (buffer request)
  "Stop BUFFER's failed acquisition of REQUEST and show a recoverable state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((session-id (or (map-nested-elt request '(:params sessionId))
                             my/agent-shell--requested-session-id
                             (map-nested-elt agent-shell--state '(:session :id))))
             (existing (and session-id
                            (my/agent-shell--find-execution
                             session-id (map-elt agent-shell--state :agent-config)
                             default-directory buffer))))
        (if existing
            (progn
              (agent-shell--display-buffer existing)
              (kill-buffer buffer)
              (message "Opened the existing Codex session in this Emacs"))
          (setq-local my/agent-shell--task-busy 'external
                      my/agent-shell--requested-session-id session-id)
          ;; A failed client must not retain an idle ACP process or retry via
          ;; agent-shell's generic resume-failure fallback to session/new.
          (agent-shell--shutdown)
          (agent-shell--update-fragment
           :state agent-shell--state :block-id "task-busy"
           :label-left "Task already active"
           :body (format
                  "Codex reports that another client owns this task.\nHost/workspace: %s\nSession: %s\nClose the owning client, then use M-x my/agent-shell-retry-task. This Emacs cannot attach to that client's process."
                  (my/agent-shell--session-context default-directory)
                  (or session-id "unknown"))
           :create-new t :navigation 'never)
          (agent-shell--emit-event
           :event 'error :data '((:code . task-busy)
                                  (:message . "Task active in another Codex client")))
          (when (shell-maker-busy)
            (agent-shell--finish-output :config shell-maker--config :success nil))
          (message "Codex task is active in another client; see this buffer for retry"))))))

(defun my/agent-shell--busy-request-a (original &rest args)
  "Handle Codex TASK_BUSY without agent-shell's create-new fallback."
  (let* ((request (plist-get args :request))
         (method (map-elt request :method))
         (state (plist-get args :state))
         (buffer (or (plist-get args :buffer) (map-elt state :buffer)))
         (on-failure (plist-get args :on-failure)))
    (when (and (member method '("session/load" "session/resume" "session/prompt"))
               (eq (map-nested-elt state '(:agent-config :identifier)) 'codex)
               on-failure)
      (setq args
            (plist-put args :on-failure
                       (lambda (acp-error raw-message)
                         (if (my/agent-shell--task-busy-p acp-error)
                             (progn
                               ;; Noema's structured prompt owns a durable Run
                               ;; receipt.  Let it finish that receipt, but do
                               ;; not run agent-shell's resume/new fallback.
                               (when (and (equal method "session/prompt")
                                          (buffer-live-p buffer)
                                          (with-current-buffer buffer
                                            (and (boundp 'noema-agent-acp--prompt-receipt)
                                                 (eq (plist-get
                                                      noema-agent-acp--prompt-receipt
                                                      :status)
                                                     'pending))))
                                 (funcall on-failure
                                          '((code . "TASK_BUSY")
                                            (message . "Task active in another client"))
                                          raw-message))
                               (my/agent-shell--handle-task-busy buffer request))
                           (funcall on-failure acp-error raw-message))))))
    (apply original args)))

(defun my/agent-shell-retry-task ()
  "Retry the Codex task after its other execution session has closed."
  (interactive)
  (unless (and (derived-mode-p 'agent-shell-mode)
               (eq my/agent-shell--task-busy 'external)
               my/agent-shell--requested-session-id)
    (user-error "This buffer has no Codex task waiting for retry"))
  (let ((session-id my/agent-shell--requested-session-id)
        (config (map-elt agent-shell--state :agent-config))
        (directory default-directory)
        (old (current-buffer))
        (origin (and (boundp 'noema-agent-acp-session-origin)
                     noema-agent-acp-session-origin))
        (name (and (boundp 'noema-agent-acp-session-name)
                   noema-agent-acp-session-name))
        (root (and (boundp 'noema-agent-acp-session-root)
                   noema-agent-acp-session-root))
        (logical-id (and (boundp 'noema-agent-promote--session-id)
                         noema-agent-promote--session-id)))
    (let* ((default-directory directory)
           (new (if (and origin (fboundp 'noema-agent-acp-start))
                    (noema-agent-acp-start :config config :directory directory
                                           :session-id session-id :focus t
                                           :origin origin)
                  (agent-shell--start :config config :session-id session-id
                                      :new-session t))))
      (when (and name root (buffer-live-p new)
                 (fboundp 'noema-agent-acp-mark-session-buffer))
        (noema-agent-acp-mark-session-buffer
         new name (format "%s" (map-elt config :identifier)) root)
        (with-current-buffer new
          (setq-local noema-agent-promote--session-id logical-id)))
      (when (buffer-live-p old)
        (kill-buffer old))
      new)))

(defun my/agent-shell--show-completion-after-insert ()
  "Show advertised ACP slash commands after /, preserving @ completion."
  (if (and (eq (char-before) ?/)
           (bound-and-true-p company-mode)
           (agent-shell--command-completion-at-point))
      (company-manual-begin)
    (agent-shell--trigger-completion-at-point)))

(defun my/agent-shell--resume-command-completion-a (original &rest args)
  "Add the local /resume command to ORIGINAL's ACP slash completion."
  (let* ((capf (apply original args))
         (bounds (and (derived-mode-p 'agent-shell-mode)
                      (agent-shell--completion-bounds "[:alnum:]_-" ?/)))
         (start (and bounds (map-elt bounds :start))))
    (if (and start (agent-shell-completion--command-start-p (1- start)))
        (if capf
            (append (list (nth 0 capf) (nth 1 capf)
                          (cons "resume" (remove "resume" (nth 2 capf))))
                    (nthcdr 3 capf))
          (list start (map-elt bounds :end) '("resume")
                :exclusive t
                :annotation-function
                (lambda (_) "  Resume a saved session")
                :exit-function #'agent-shell--capf-exit-with-space))
      capf)))

(defun my/agent-shell--resume-from-list (source sessions)
  "Pick one native conversation from SESSIONS and replace SOURCE with it."
  (when (buffer-live-p source)
    (let* ((state (buffer-local-value 'agent-shell--state source))
           (current-id (map-nested-elt state '(:session :id)))
           (sessions (agent-shell--sort-sessions-by-recency sessions))
           (choices (mapcar
                     (lambda (session)
                       (let ((id (map-elt session 'sessionId)))
                         (cons (format "%s  %s · %s"
                                       (agent-shell--session-title session)
                                       (agent-shell--format-session-date
                                        (or (map-elt session 'updatedAt)
                                            (map-elt session 'createdAt) ""))
                                       id)
                               id)))
                     (seq-filter (lambda (session)
                                   (let ((id (map-elt session 'sessionId)))
                                     (and (stringp id)
                                          (not (equal id current-id)))))
                                 sessions)))
           (config (map-elt state :agent-config))
           (directory (buffer-local-value 'default-directory source))
           (root (or (and (boundp 'noema-agent-acp-session-root)
                          (buffer-local-value 'noema-agent-acp-session-root source))
                     directory)))
      (unless choices
        (user-error "This agent has no other saved sessions in the current workspace"))
      (let* ((selected (completing-read "Resume session: " choices nil t nil nil
                                        (caar choices)))
             (id (cdr (assoc selected choices)))
             (live (my/agent-shell--find-execution id config directory)))
        (with-current-buffer source
          (when (equal (agent-shell--prompt-input) "/resume")
            (agent-shell--clear-prompt-input)))
        (if live
            (progn (agent-shell--display-buffer live) live)
          (let* ((old-name (buffer-name source))
                 (binding (and (fboundp 'noema-sessions-native-binding)
                               (noema-sessions-native-binding id root))))
            ;; Upstream restart replaces the shell in its existing windows and
            ;; resumes by native ID; it does not add another Agent tab.
            (with-current-buffer source
              (agent-shell-restart :session-id id))
            (let ((target (or (get-buffer old-name)
                              (user-error "Agent shell did not restart"))))
              (when (and binding (fboundp 'noema-agent-acp-mark-session-buffer))
                (noema-agent-acp-mark-session-buffer
                 target (plist-get binding :name)
                 (format "%s" (map-elt config :identifier)) root)
                (with-current-buffer target
                  (setq-local noema-agent-promote--session-id
                              (plist-get binding :session-id))))
              target)))))))

(defun my/agent-shell-resume ()
  "Choose an official ACP session of this agent and workspace to resume."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Open an agent-shell buffer before using /resume"))
  (unless (map-elt agent-shell--state :supports-session-list)
    (user-error "This agent does not support listing saved sessions"))
  (let ((source (current-buffer)))
    (agent-shell--list-sessions
     :state agent-shell--state
     :cwd (agent-shell--resolve-path (agent-shell-cwd))
     :buffer source
     :on-success (lambda (sessions)
                   (my/agent-shell--resume-from-list source sessions))
     :on-failure (lambda (error-object _raw)
                   (message "Could not list saved sessions: %s"
                            (or (and (listp error-object)
                                     (map-elt error-object 'message))
                                error-object))))))

(defun my/agent-shell--submit-a (original &rest args)
  "Run local /resume without submitting it as an agent prompt."
  (if (and (derived-mode-p 'agent-shell-mode)
           (equal (agent-shell--prompt-input) "/resume"))
      (my/agent-shell-resume)
    (apply original args)))

(defun my/agent-shell-enable-command-completion ()
  "Show ACP command suggestions in an agent-shell buffer after typing /."
  (when (and (bound-and-true-p agent-shell-completion-mode)
             (require 'company nil t))
    ;; Only explicit @ and / triggers should open the menu in agent-shell.
    (company-mode 1)
    (setq-local company-backends '(company-capf)
                company-idle-delay nil
                company-minimum-prefix-length 0)
    (remove-hook 'post-self-insert-hook
                 #'agent-shell--trigger-completion-at-point t)
    (add-hook 'post-self-insert-hook
              #'my/agent-shell--show-completion-after-insert nil t)))

(defun my/agent-shell-disable-transcripts ()
  "Disable automatic transcripts and save-on-close prompts for this agent."
  ;; Clear the cached path too, so reloading also updates existing sessions.
  (setq-local agent-shell--transcript-file nil
              shell-maker-prompt-before-killing-buffer nil))

(with-eval-after-load 'agent-shell
  ;; Apply to every entry point, including plain M-x agent-shell.
  (setq agent-shell-transcript-file-path-function nil)
  (add-hook 'agent-shell-mode-hook #'my/agent-shell-disable-transcripts)
  (add-hook 'agent-shell-mode-hook #'my/agent-shell-enable-command-completion)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (my/agent-shell-disable-transcripts))))
  ;; CWD is used for both process creation and the asynchronous session/new
  ;; request; the resolver turns it into the agent's native path.
  (advice-add 'agent-shell-cwd :filter-return #'my/agent-shell-process-directory)
  (setq agent-shell-path-resolver-function #'my/agent-shell-resolve-path)
  (advice-add 'agent-shell--on-request :around #'my/agent-shell-in-session-buffer-a)
  (advice-add 'agent-shell--start :around #'my/agent-shell--reuse-session-a)
  (advice-add 'agent-shell--send-request :around #'my/agent-shell--busy-request-a)
  (advice-add 'agent-shell-submit :around #'my/agent-shell--submit-a)
  (advice-add 'agent-shell--command-completion-at-point :around
              #'my/agent-shell--resume-command-completion-a)
  ;; Every client (and agent-shell's early executable check) is made in the
  ;; agent's own buffer; give that buffer its workspace environment first.
  (advice-add 'agent-shell--make-acp-client :before
              #'my/agent-shell-apply-workspace-environment)
  (advice-add 'agent-shell--make-missing-executable-error :filter-return
              #'my/agent-shell-missing-executable-a))

(with-eval-after-load 'noema-agent-acp
  (setq noema-agent-acp-process-directory-function
        #'my/agent-shell-process-directory
        noema-agent-acp-agent-file-function
        #'my/agent-shell-agent-file-name))

(defvar agent-shell-opencode-acp-command)

(config-defvar my/agent-shell-opencode-executable
  (locate-user-emacs-file "var/noema/tools/opencode/1.18.30-official/opencode")
  "Validated official OpenCode executable used instead of the Homebrew build.
An explicitly customized `agent-shell-opencode-acp-command' takes precedence.
See docs/opencode-acp-recovery.md for provenance and the offline smoke test."
  :type 'file
  :group 'ai)

(defun my/agent-shell-use-official-opencode (arguments)
  "Prefer the validated OpenCode executable for a client-side agent.
ARGUMENTS are `agent-shell--make-acp-client' keywords.  The pinned binary is a
file on this machine, so it replaces the default command only when the agent
runs here; elsewhere the target's own `opencode' is resolved from its PATH.
Provider settings, authentication, PATH and a custom command stay untouched."
  (if (and (equal (plist-get arguments :command) "opencode")
           (equal (plist-get arguments :command-params) '("acp"))
           (equal (bound-and-true-p agent-shell-opencode-acp-command) '("opencode" "acp"))
           (stringp my/agent-shell-opencode-executable)
           (remote-client-file-name (remote-canonicalize-file-name default-directory))
           (file-executable-p my/agent-shell-opencode-executable))
      (plist-put (copy-sequence arguments) :command
                 (expand-file-name my/agent-shell-opencode-executable))
    arguments))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--make-acp-client :filter-args
              (lambda (args) (my/agent-shell-use-official-opencode args))
              '((name . my/agent-shell-use-official-opencode))))

;; The Claude and Codex ACP adapters each bundle a copy of their CLI and run
;; it unless told otherwise.  Those copies are removed from this machine; the
;; only CLI is the one the person keeps updated.  Point each adapter at the
;; CLI its workspace environment finds -- the same PATH lookup, on the same
;; target, that found the adapter itself -- so a local session uses the
;; shell's `claude'/`codex' exactly as a remote one uses the target's.  There
;; is no fallback: a missing CLI is an error naming the target.  Pi and
;; OpenCode have no bundled CLI.

(defconst my/agent-shell-adapter-clis
  '(("claude-agent-acp" "CLAUDE_CODE_EXECUTABLE" "claude")
    ("codex-acp" "CODEX_PATH" "codex"))
  "ACP adapters that bundle a CLI: (ADAPTER ENVIRONMENT-VARIABLE CLI).
The adapter runs CLI from ENVIRONMENT-VARIABLE when it is set.")

(defun my/agent-shell-use-workspace-cli (arguments)
  "Make a CLI-bundling adapter in ARGUMENTS run the workspace's own CLI.
ARGUMENTS are `agent-shell--make-acp-client' keywords.  The CLI is looked up
with the agent's workspace environment on its target and passed as the
target-native path.  A CLI that cannot be found is an error."
  (let* ((command (plist-get arguments :command))
         (entry (assoc (and (stringp command) (file-name-nondirectory command))
                       my/agent-shell-adapter-clis)))
    (if (null entry)
        arguments
      (pcase-let ((`(,_ ,variable ,cli) entry))
        (plist-put (copy-sequence arguments) :environment-variables
                   (cons (format "%s=%s" variable
                                 (remote-file-local-name
                                  (or (executable-find cli t)
                                      (user-error "%s: no `%s' on the PATH of target `%s'; install it there or add it to that environment"
                                                  command cli
                                                  (or my/agent-shell--lookup-target "local")))))
                         (plist-get arguments :environment-variables)))))))

(with-eval-after-load 'agent-shell
  ;; Innermost, so the workspace environment advice above has run first.
  (advice-add 'agent-shell--make-acp-client :filter-args
              (lambda (args) (my/agent-shell-use-workspace-cli args))
              '((name . my/agent-shell-use-workspace-cli) (depth . 100))))

(autoload 'noema "noema" nil t)
(autoload 'noema-compose "noema-compose" nil t)
(autoload 'noema-compose-send "noema-compose" nil t)
(autoload 'noema-compose-menu "noema-compose" nil t)
(autoload 'noema-compose-add-context "noema-compose" nil t)
(autoload 'noema-compose-rewrite "noema-compose" nil t)
(autoload 'noema-agent-start "noema-agent-acp" nil t)
(autoload 'noema-agent-promote-current-session "noema-agent-promote" nil t)
(autoload 'noema-research-history-index "noema-agent-promote" nil t)
(autoload 'noema-agent-worker-run-work-cell "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-run-prompt-file "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-decide-permission "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-cancel-queued "noema-agent-worker" nil t)
(autoload 'noema-agent-takeover-session "noema-agent-takeover" nil t)
(autoload 'noema-agent-handback-session "noema-agent-takeover" nil t)
(autoload 'noema-research-attention "noema-research-inspector" nil t)
(autoload 'noema-research-propose-with-magent "noema-research-synthesis" nil t)
(autoload 'noema-project-enable "noema-research" nil t)
(autoload 'noema-pi-router-open "noema-pi-router" nil t)
(autoload 'noema-pi-doctor "noema-pi-router" nil t)
(autoload 'noema-orchestration "noema-orchestration" nil t)
(autoload 'noema-sessions "noema-sessions" nil t)
(autoload 'noema-sessions-switch "noema-sessions" nil t)
(autoload 'noema-sessions-read "noema-sessions")
(autoload 'noema-sessions-native-binding "noema-sessions")
(autoload 'noema-agent-inbox "noema-agent-inbox" nil t)
(autoload 'noema-agent-abtop "noema-agent-abtop" nil t)
(autoload 'noema-context-send "noema-context" nil t)
(autoload 'noema-context-draft "noema-context" nil t)
(autoload 'noema-context-send-region "noema-context" nil t)
(autoload 'noema-context-send-buffer "noema-context" nil t)
(autoload 'noema-context-send-file "noema-context" nil t)
(autoload 'noema-context-send-at-point "noema-context" nil t)
(autoload 'noema-context-inspect "noema-context" nil t)
(autoload 'noema-md-bridge-edit-source "noema-md-bridge" nil t)
(autoload 'noema-md-bridge-rewrite "noema-md-bridge" nil t)
(autoload 'noema-md-bridge-add-context "noema-md-bridge" nil t)
(autoload 'noema-md-bridge-compose "noema-md-bridge" nil t)
(autoload 'noema-capability-manager "noema-capability-ui" nil t)
(autoload 'noema-skill-manager "noema-capability-ui" nil t)
(autoload 'noema-mcp-manager "noema-capability-ui" nil t)
(autoload 'noema-skill-upstream "noema-skill-upstream-ui" nil t)
(autoload 'noema-capability-lookup "noema-capability-actions" nil t)

;; Standard compatibility infrastructure is also package-managed.
(use-package compat :ensure t :defer t)

(config-defvar magent-session-directory
  (locate-user-emacs-file "var/noema/magent/sessions/")
  "Directory for Magent sessions and their audit trail."
  :type 'directory
  :group 'ai)

(config-defvar magent-request-timeout 180
  "Inactivity timeout for API and structured CLI sampling."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-json-line-bytes (* 1024 1024)
  "Maximum buffered bytes for one coding-agent JSON event."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-answer-bytes (* 8 1024 1024)
  "Maximum assistant bytes retained from one coding-agent turn."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-diagnostic-bytes (* 256 1024)
  "Maximum diagnostic bytes retained from one coding-agent turn."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-prompt-bytes (* 4 1024 1024)
  "Maximum combined Magent context and user prompt sent to a CLI."
  :type 'integer
  :group 'ai)

(declare-function noema-interaction-engine-cli-register
                  "noema-interaction-engine-cli" ())

(defvar-keymap my/noema-prefix-map
  :doc "Prefix map for Noema research and agent commands."
  "a" #'noema-agent-start
  "W" #'noema
  "c" #'noema-compose
  "s" #'noema-compose-send
  "m" #'noema-compose-menu
  "." #'noema-compose-add-context
  "r" #'noema-compose-rewrite
  "p" #'noema-agent-promote-current-session
  "I" #'noema-research-attention
  "P" #'noema-pi-router-open
  "D" #'noema-pi-doctor
  "S" #'noema-sessions
  "G" #'noema-agent-inbox
  "U" #'noema-agent-abtop
  "O" #'noema-orchestration
  "b" #'noema-sessions-switch
  "i" #'noema-agent-acp-focus-input
  ;; Editor context for a chosen session, sent as references.
  "x" #'noema-context-send
  "v" #'noema-context-send-region
  "B" #'noema-context-send-buffer
  "f" #'noema-context-send-file
  "@" #'noema-context-send-at-point
  "," #'noema-context-inspect
  "d" #'noema-context-draft
  ;; Noema Markdown pane: open its note in Emacs at the selection.
  "e" #'noema-md-bridge-edit-source)

(global-set-key (kbd "C-c M-a") #'noema-compose-add-context)
(global-set-key (kbd "C-c A") my/noema-prefix-map)

;;; ── Noema settings in the config registry (D-035) ─────────────────────────
;;
;; Noema keeps its tunables as ordinary defcustoms so its repository stays
;; independent.  This glue registers them with `config' once their library
;; loads, so they appear on the config board and persist to
;; `etc/config-noema.el' like every other setting.  Noema's own settings
;; center saves through `customize-save-variable', which `config-custom'
;; routes into the same store.

(defvar config--registry)
(declare-function noema-pi-project-root "noema-pi-router" (&optional directory))
(declare-function noema-pi-close-project "noema-pi-router" (&optional directory interrupt))
(declare-function persp-current-buffers "perspective" ())

(defconst my/noema-config-groups
  '(noema-research noema-research-graph noema-agent-worker noema-pi-router
    noema-agent-session noema-context noema-md-bridge noema-orchestration)
  "Custom groups whose options Noema exposes through `config'.")

(defun my/noema-config--type-arguments (symbol)
  "Return `config-register' type arguments for Custom option SYMBOL."
  (pcase (get symbol 'custom-type)
    ('boolean '(:type boolean))
    ((or 'integer 'natnum) '(:type integer))
    ('number '(:type number))
    ((or 'string 'file 'directory) '(:type string))
    (`(choice . ,options)
     (if (seq-every-p (lambda (option) (eq (car-safe option) 'const)) options)
         `(:type sexp :choices ,(mapcar (lambda (option) (car (last option))) options))
       '(:type sexp)))
    (_ '(:type sexp))))

(defun my/noema-config-register-groups (&rest _)
  "Register every loaded, not yet registered Noema option with `config'."
  (dolist (group my/noema-config-groups)
    (dolist (member (get group 'custom-group))
      (pcase-let ((`(,symbol ,kind) member))
        (when (and (eq kind 'custom-variable)
                   (boundp symbol)
                   (not (gethash symbol config--registry)))
          (apply #'config-register symbol
                 :group 'noema
                 :store-file "etc/config-noema.el"
                 :doc (car (split-string (or (documentation-property
                                              symbol 'variable-documentation t)
                                             "")
                                         "\n"))
                 (my/noema-config--type-arguments symbol)))))))

(dolist (feature '(noema-research-mode noema-research-graph noema-research-settings
                   noema-agent-worker noema-pi-router noema-agent-acp noema-context
                   noema-md-bridge noema-orchestration))
  (eval-after-load feature #'my/noema-config-register-groups))

(defun my/noema-close-projects-of-killed-perspective ()
  "Close Noema projects whose documents live only in the dying perspective.
`persp-killed-hook' runs inside that perspective, before its buffers go.  The
project's Pi and idle agents stop; a running Run finishes first."
  (when (and (featurep 'noema-pi-router) (fboundp 'persp-current-buffers))
    (let* ((inside (seq-filter #'buffer-live-p (persp-current-buffers)))
           (root-of (lambda (buffer)
                      (with-current-buffer buffer
                        (and buffer-file-name
                             (derived-mode-p 'noema-research-mode)
                             (noema-pi-project-root (file-name-directory buffer-file-name))))))
           (roots (delete-dups (delq nil (mapcar root-of inside)))))
      (dolist (root roots)
        (unless (seq-some (lambda (buffer)
                            (and (not (memq buffer inside))
                                 (equal (funcall root-of buffer) root)))
                          (buffer-list))
          (noema-pi-close-project root))))))

(add-hook 'persp-killed-hook #'my/noema-close-projects-of-killed-perspective)

;;; ── Claude Code ────────────────────────────────────────────────────────────

(config-defvar claude-code-ide-cli-path nil
  "Path to the Claude Code CLI binary."
  :type 'file
  :group 'ai)

(config-defvar claude-code-ide-window-side nil
  "Side used for the Claude Code IDE window."
  :type 'symbol
  :group 'ai)

(config-defvar claude-code-ide-window-width nil
  "Width used for the Claude Code IDE window."
  :type 'integer
  :group 'ai)

;; Claude Code compatibility source remains vendored for ACP adapter work, but
;; its standalone IDE UI is deliberately not autoloaded or globally bound.

;;; ── Codex CLI ──────────────────────────────────────────────────────────────

(config-defvar codex-cli-executable nil
  "Executable used by codex-cli."
  :type 'string
  :group 'ai)

(config-defvar codex-cli-terminal-backend nil
  "Terminal backend used by codex-cli."
  :type 'symbol
  :group 'ai)

(config-defvar codex-cli-side nil
  "Side used for the codex-cli window."
  :type 'symbol
  :group 'ai)

(config-defvar codex-cli-width nil
  "Width used for the codex-cli window."
  :type 'integer
  :group 'ai)

;; Codex CLI compatibility source likewise stays vendored without a separate
;; terminal UI or global key family.  Structured agents enter through Noema.

;; ── Native CLI fallback inside embedded gptel ──────────────────────────────

;; The normal agent path is ACP/agent-shell.  Keep the migrated one-shot CLI
;; sampler as a fallback backend without retaining the old gptel fork.
(with-eval-after-load 'gptel
  (require 'noema-interaction-engine-cli)
  (noema-interaction-engine-cli-register))

(require 'noema-agent-bridge)

(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
