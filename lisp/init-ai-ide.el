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

(defvar agent-shell--transcript-file)(defvar agent-shell--transcript-file)
(defvar shell-maker-prompt-before-killing-buffer)

(defun my/agent-shell-disable-transcripts ()
  "Disable automatic transcripts and save-on-close prompts for this agent."
  ;; Clear the cached path too, so reloading also updates existing sessions.
  (setq-local agent-shell--transcript-file nil
              shell-maker-prompt-before-killing-buffer nil))

(with-eval-after-load 'agent-shell
  ;; Apply to every entry point, including plain M-x agent-shell.
  (setq agent-shell-transcript-file-path-function nil)
  (add-hook 'agent-shell-mode-hook #'my/agent-shell-disable-transcripts)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (my/agent-shell-disable-transcripts))))
  ;; CWD is used for both process creation and the asynchronous session/new
  ;; request; the resolver turns it into the agent's native path.
  (advice-add 'agent-shell-cwd :filter-return #'my/agent-shell-process-directory)
  (setq agent-shell-path-resolver-function #'my/agent-shell-resolve-path)
  (advice-add 'agent-shell--on-request :around #'my/agent-shell-in-session-buffer-a)
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
;; it unless told otherwise.  That copy is only as new as the adapter, so the
;; models and features on offer lag behind the CLI the person keeps updated.
;; Point each adapter at the CLI its workspace environment finds -- the same
;; PATH lookup, on the same target, that found the adapter itself -- so a
;; local session uses the shell's `claude'/`codex' exactly as a remote one
;; uses the target's.  Pi and OpenCode have no bundled CLI.

(defconst my/agent-shell-adapter-clis
  '(("claude-agent-acp" "CLAUDE_CODE_EXECUTABLE" "claude")
    ("codex-acp" "CODEX_PATH" "codex"))
  "ACP adapters that bundle a CLI: (ADAPTER ENVIRONMENT-VARIABLE CLI).
The adapter runs CLI from ENVIRONMENT-VARIABLE when it is set.")

(defun my/agent-shell-use-workspace-cli (arguments)
  "Make a CLI-bundling adapter in ARGUMENTS run the workspace's own CLI.
ARGUMENTS are `agent-shell--make-acp-client' keywords.  The CLI is looked up
with the agent's workspace environment on its target and passed as the
target-native path.  An explicit setting of the variable, in the command's
environment or Emacs's, wins; a CLI that cannot be found leaves the adapter
on its bundled copy and says so."
  (let* ((command (plist-get arguments :command))
         (entry (assoc (and (stringp command) (file-name-nondirectory command))
                       my/agent-shell-adapter-clis))
         (variable (nth 1 entry))
         (environment (plist-get arguments :environment-variables)))
    (if (or (null entry)
            (seq-some (lambda (setting) (string-prefix-p (concat variable "=") setting))
                      environment)
            (getenv variable))
        arguments
      (if-let* ((found (executable-find (nth 2 entry) t)))
          (plist-put (copy-sequence arguments) :environment-variables
                     (cons (format "%s=%s" variable (remote-file-local-name found))
                           environment))
        (message "%s: no `%s' on this workspace's PATH; using the adapter's bundled copy"
                 command (nth 2 entry))
        arguments))))

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
(autoload 'noema-context-send "noema-context" nil t)
(autoload 'noema-context-draft "noema-context" nil t)
(autoload 'noema-context-send-region "noema-context" nil t)
(autoload 'noema-context-send-buffer "noema-context" nil t)
(autoload 'noema-context-send-file "noema-context" nil t)
(autoload 'noema-context-send-at-point "noema-context" nil t)
(autoload 'noema-context-inspect "noema-context" nil t)
(autoload 'noema-capability-manager "noema-capability-ui" nil t)
(autoload 'noema-skill-manager "noema-capability-ui" nil t)
(autoload 'noema-mcp-manager "noema-capability-ui" nil t)
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
  "d" #'noema-context-draft)

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
    noema-agent-session noema-context noema-orchestration)
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
                   noema-orchestration))
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

(provide 'init-ai-ide)
;;; init-ai-ide.el ends here
