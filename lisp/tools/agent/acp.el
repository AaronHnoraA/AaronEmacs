;;; tools/agent/acp.el --- Shared boundary for package-managed agent-shell/ACP -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The unmodified package-managed agent-shell and acp.el implementations own physical
;; agent sessions.  Callers own durable business identity.  All coupling to
;; agent-shell implementation details is isolated here so document workers do
;; not serialize agent-shell buffers or private state as canonical data.

;;; Code:

(require 'map)
(require 'json)
(require 'project)
(require 'ring)
(require 'seq)
(require 'simple)
(require 'tab-line)
(require 'aaron-lib)
(require 'agent-shell)
(require 'acp)
(require 'tools/agent/actions)
(require 'tools/agent/render)
(require 'tools/agent/views)
(+agent-render-mode 1)

(defvar +agent-project-root-functions nil "Queries receiving a directory and returning an optional domain project root.")
(defvar +agent-workspace-functions nil "Queries receiving a root and returning its execution workspace.")
(defvar +agent-session-adopt-functions nil "Called with BUFFER, NAME and ROOT after local session registration.")
(defvar +agent-session-forget-functions nil "Called with BUFFER when it gives up a local session name.")
(defvar +agent-session-id-functions nil "Queries receiving BUFFER and returning an optional domain session id.")
(defvar +agent-session-group-functions nil
  "Read-only queries receiving BUFFER and returning (:key ID :label LABEL).
Adapters use already available associations; they must not start a host or
query disk/network while the shared registry renders.")
(defvar +agent-session-close-functions nil "Called with BUFFER before its process and frontend are ended.")
(defvar +agent-session-operation-functions nil "Predicates receiving OPERATION and BUFFER; non-nil reports a domain capability.")
(defvar +agent-session-action-functions nil "Functions receiving OPERATION, BUFFER and arguments; non-nil handles it.")
(defvar +agent-session-attention-functions nil "Queries receiving BUFFER and returning its domain attention reason.")
(defvar +agent-session-private-functions nil "Predicates receiving a snapshot; non-nil excludes it from ordinary context selection.")
(defvar +agent-acp-previewing nil "Non-nil while a reversible session preview is active.")
(defvar +buffer-end-function)
(defvar +buffer-hide-function)
(defvar +window-buffer-replacement)
(defun +agent-workspace (root)
  "Return ROOT's execution workspace without enabling a domain project."
  (or (run-hook-with-args-until-success '+agent-workspace-functions root) root))
(autoload '+agent-session-restart "tools/agent/sessions")
(autoload '+agent-session-rename "tools/agent/sessions")
(autoload '+agent-session-fork "tools/agent/sessions")
(autoload '+agent-session-side-chat "tools/agent/sessions")
(autoload '+agent-session-jump "tools/agent/sessions")
(autoload '+agent-session-archive "tools/agent/sessions")
(autoload '+agent/pool-switch "tools/agent/sessions" nil t)
(autoload '+agent-session-list "tools/agent/sessions")
(autoload '+agent/changes "tools/agent/sessions" nil t)
(autoload '+agent/queue "tools/agent/queue" nil t)
(autoload '+agent-queue-resume "tools/agent/queue" nil t)


(declare-function magent-start "magent-agent-shell" ())
(declare-function remote-client-file-name "remote" (file &optional context))
(declare-function agent-shell-insert "agent-shell" (&key text submit no-focus shell-buffer))
(declare-function shell-maker-finish-output "shell-maker" (&key config success on-output))
(declare-function evil-define-minor-mode-key "evil-core" (state mode key def &rest bindings))
(declare-function evil-insert-state "evil-states" (&optional arg))
;; Session registry commands for the Agent window live in `tools/agent/sessions',
;; which depends on this boundary.
(declare-function +agent-session-restart "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-rename "tools/agent/sessions" (&optional buffer new-name))
(declare-function +agent-session-fork "tools/agent/sessions" (&optional buffer child))
(declare-function +agent-session-side-chat "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-archive "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-jump "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-list "tools/agent/sessions" (&optional buffer))
(defvar shell-maker--busy)
(defvar agent-shell-confirm-interrupt)
(defvar shell-maker--config)
(defvar comint-last-prompt)
(defvar +agent-render--pending)
(declare-function agent-shell--live-input-prompt-p "agent-shell" (prompt))

(defun +agent-acp--state (&optional buffer)
  "Return embedded agent-shell state for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    agent-shell--state))

(defun +agent-acp-state-value (buffer path &optional default)
  "Read PATH from BUFFER's agent-shell state, returning DEFAULT if absent."
  (or (map-nested-elt (+agent-acp--state buffer) path) default))

(defvar-local +agent-acp-strict-restore nil
  "When non-nil, a failed native restore must not create another conversation.")
(put '+agent-acp-strict-restore 'permanent-local t)

(defun +agent-acp--restore-failed (buffer session-id)
  "Keep failed SESSION-ID visible without manufacturing a new conversation."
  (with-current-buffer buffer
    (setq-local +agent-acp-last-error (format "Could not restore native session %s" session-id)
                +agent-acp-attention 'failed)
    (agent-shell--update-bootstrapping-fragment
     :state (agent-shell--state) :block-id "restore_failed"
     :body (concat +agent-acp-last-error ". The reference and history were preserved. Choose an existing session or explicitly create a new one."))
    (+agent-acp--changed)))

(defun +agent-acp--strict-restore-arguments (arguments)
  "Use upstream's failure callback instead of its new-session fallback."
  (let ((buffer (or (plist-get arguments :shell-buffer) (current-buffer))))
    (if (and (buffer-live-p buffer)
             (buffer-local-value '+agent-acp-strict-restore buffer))
        (let ((id (plist-get arguments :session-id)))
          (plist-put (copy-sequence arguments) :on-failure
                     (lambda () (+agent-acp--restore-failed buffer id))))
      arguments)))
(advice-add 'agent-shell--initiate-session-resume-by-id :filter-args
            #'+agent-acp--strict-restore-arguments)

(defvar-local +agent-acp--choose-saved nil
  "Non-nil makes this starting client ask which saved conversation to restore.")

(defun +agent-acp-saved-session-choices (sessions &optional exclude)
  "Return (LABEL . ID) for provider-listed SESSIONS, newest first, except EXCLUDE."
  (mapcar (lambda (session)
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
                          (and (stringp id) (not (equal id exclude)))))
                      (agent-shell--sort-sessions-by-recency sessions))))

(defun +agent-acp--abandon-saved (buffer reason)
  "End client BUFFER, which never opened a conversation, reporting REASON."
  (when (+agent-acp-agent-buffer-p buffer) (+agent-acp-kill buffer))
  (message "%s" reason))

(defun +agent-acp--live-session (id directory)
  "Return the buffer already serving native session ID in DIRECTORY, or nil."
  (seq-find (lambda (buffer)
              (and (+agent-acp-agent-buffer-p buffer)
                   (equal id (+agent-acp-state-value buffer '(:session :id)))
                   (equal directory (buffer-local-value 'default-directory buffer))
                   (not (+agent-acp-client-exited-p buffer))))
            (buffer-list)))

(defun +agent-acp--pick-saved (buffer sessions continue)
  "Ask which of SESSIONS BUFFER restores, then call CONTINUE with its identity."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer
      (let* ((choices (+agent-acp-saved-session-choices sessions))
             (id (condition-case nil
                     (and choices
                          (cdr (assoc (completing-read "Resume conversation: " choices nil t
                                                       nil nil (caar choices))
                                      choices)))
                   (quit nil)))
             (live (and id (+agent-acp--live-session id default-directory))))
        (cond ((null choices)
               (+agent-acp--abandon-saved buffer "This agent has no saved conversations here"))
              ((null id) (+agent-acp--abandon-saved buffer "Resume cancelled"))
              (live (+agent-acp--abandon-saved buffer "That conversation is already open")
                    (+agent-acp-show-buffer live))
              (t (map-put! agent-shell--state :resume-session-id id)
                 (funcall continue)))))))

(defun +agent-acp--choose-saved-session (buffer state continue)
  "List BUFFER's saved conversations, leaving the choice to a clean command loop.
The list arrives in a process callback, which must not read the minibuffer."
  (with-current-buffer buffer (setq +agent-acp--choose-saved nil))
  (if (not (and (map-elt state :supports-session-list)
                (or (map-elt state :supports-session-load)
                    (map-elt state :supports-session-resume))))
      (+agent-acp--abandon-saved buffer "This agent does not list saved conversations")
    (with-current-buffer buffer
      (agent-shell--list-sessions
       :state state :cwd (agent-shell--resolve-path default-directory) :buffer buffer
       :on-success (lambda (sessions)
                     (run-at-time 0 nil #'+agent-acp--pick-saved buffer sessions continue))
       :on-failure (lambda (_error _raw)
                     (run-at-time 0 nil #'+agent-acp--abandon-saved buffer
                                  "Could not list saved conversations"))))))

(defun +agent-acp--strict-initiate-session (original &rest arguments)
  "Reject an unsupported requested restore before upstream can create anew."
  (let* ((buffer (or (plist-get arguments :shell-buffer) (current-buffer)))
         (state (+agent-acp--state buffer))
         (id (map-elt state :resume-session-id)))
    (cond
     ((buffer-local-value '+agent-acp--choose-saved buffer)
      (+agent-acp--choose-saved-session
       buffer state (lambda () (apply #'+agent-acp--strict-initiate-session
                                      original arguments))))
     ((and id (buffer-local-value '+agent-acp-strict-restore buffer)
           (not (or (map-elt state :supports-session-load)
                    (map-elt state :supports-session-resume))))
      (+agent-acp--restore-failed buffer id))
     (t (apply original arguments)))))
(advice-add 'agent-shell--initiate-session :around #'+agent-acp--strict-initiate-session)

(defun +agent-acp-list-native-sessions (buffer callback &optional page-limit)
  "Read provider-owned native history, then call CALLBACK (ROWS ERROR).
This is a control request, not a model prompt. The adapter owns pagination."
  (let ((state (+agent-acp--state buffer)))
    (if (not (map-elt state :supports-session-list))
        (funcall callback nil "Native session listing is unavailable")
      (with-current-buffer buffer
        (agent-shell--list-sessions
         :state state :cwd (agent-shell--resolve-path default-directory)
         :buffer buffer :page-limit (or page-limit agent-shell-session-list-page-limit)
         :on-success (lambda (rows)
                       (when (and (buffer-live-p buffer) (eq state (+agent-acp--state buffer)))
                         (funcall callback rows nil)))
         :on-failure (lambda (error-object _raw)
                       (when (and (buffer-live-p buffer) (eq state (+agent-acp--state buffer)))
                         (funcall callback nil error-object))))))))

(defun +agent-acp--context-field (item key &optional default)
  "Read KEY from a frozen Noema context ITEM."
  (or (map-elt item key) (map-elt item (intern key)) default))

(defun +agent-acp-context-blocks (buffer prompt items)
  "Build ACP content for PROMPT and frozen ITEMS in BUFFER.
Use the same resource block and text fallback contract for research Runs
and private LaTeX exports."
  (let ((embedded (+agent-acp-state-value buffer
                                                '(:prompt-capabilities :embedded-context))))
    (if (and embedded (not (memq embedded '(:false :null))))
        (append
         (list `((type . "text") (text . ,prompt)))
         (mapcar (lambda (item)
                   `((type . "resource")
                     (resource . ((uri . ,(+agent-acp--context-field item "resolvedUri"))
                                  (text . ,(decode-coding-string
                                            (base64-decode-string
                                             (+agent-acp--context-field item "contentBase64"))
                                            'utf-8))
                                  (mimeType . ,(+agent-acp--context-field
                                                item "mediaType" "text/plain; charset=utf-8"))))))
                 items))
      (let ((fallback prompt))
        (dolist (item items)
          (setq fallback
                (concat fallback "\n\n[Noema context: "
                        (+agent-acp--context-field item "ref") "]\n"
                        (decode-coding-string
                         (base64-decode-string
                          (+agent-acp--context-field item "contentBase64"))
                         'utf-8))))
        (list `((type . "text") (text . ,fallback)))))))

(defvar +agent-acp-model-preferences-file
  (locate-user-emacs-file "var/agent/models.json")
  "Local, non-project storage for the last successfully selected model per agent.")

(defun +agent-acp--read-models ()
  "Read model preferences as data, never as executable Lisp."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents +agent-acp-model-preferences-file)
        (let ((models (json-parse-buffer :object-type 'hash-table)))
          (if (hash-table-p models) models (make-hash-table :test #'equal))))
    (error (make-hash-table :test #'equal))))

(defun +agent-acp--remember-model (identifier model)
  "Remember a successfully selected MODEL for agent IDENTIFIER."
  (when (and identifier (stringp model) (not (string-empty-p model)))
    (condition-case err
        (let ((models (+agent-acp--read-models)))
          (puthash (format "%s" identifier) model models)
          (make-directory (file-name-directory +agent-acp-model-preferences-file) t)
          (with-temp-file +agent-acp-model-preferences-file
            (insert (json-serialize models))))
      (error (message "Could not save agent model preference: %s" (error-message-string err))))))

(defun +agent-acp--remember-interactive-model (identifier model)
  "Remember MODEL for IDENTIFIER only outside private export sessions."
  (unless (or (eq (bound-and-true-p +agent-acp-session-origin)
                  'latex-export)
              (map-nested-elt (agent-shell--state)
                              '(:agent-config :agent-pinned-model)))
    (+agent-acp--remember-model identifier model)))

(defun +agent-acp--model-config (args)
  "Apply remembered defaults through upstream's normal startup pipeline in ARGS."
  (let* ((config (copy-tree (plist-get args :config)))
         (identifier (map-elt config :identifier))
         (saved (gethash (format "%s" identifier) (+agent-acp--read-models)))
         (fallback (map-elt config :default-model-id)))
    (when (and config (stringp saved)
               (not (map-elt config :agent-pinned-model)))
      (setf (alist-get :default-model-id config)
            (lambda ()
              ;; Providers may remove a model. Do not block startup on a stale ID.
              (if (seq-some (lambda (model) (equal (map-elt model :model-id) saved))
                            (agent-shell--get-available-models (agent-shell--state)))
                  saved
                (when (functionp fallback) (funcall fallback)))))
      (setq args (plist-put (copy-sequence args) :config config)))
    args))

(defun +agent-acp--select-model (original &optional on-success)
  "Remember interactive model selection after ORIGINAL succeeds; reject busy edits."
  (when (or (map-elt (agent-shell--state) :active-requests)
            (run-hook-with-args-until-success '+agent-acp-busy-functions (current-buffer)))
    (user-error "Agent is starting or running; finish/cancel the current turn before changing model"))
  (let ((buffer (current-buffer))
        (identifier (map-nested-elt (agent-shell--state) '(:agent-config :identifier))))
    (funcall original
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (+agent-acp--remember-interactive-model identifier (agent-shell--current-model-id (agent-shell--state)))))
               (when on-success (funcall on-success))))))

(advice-add 'agent-shell--start :filter-args #'+agent-acp--model-config)
(advice-add 'agent-shell-set-session-model :around #'+agent-acp--select-model)

(defun +agent-acp--set-model (original &rest args)
  "Remember successful model changes from upstream's keyboard and mouse UI."
  (let ((buffer (current-buffer))
        (identifier (map-nested-elt (agent-shell--state) '(:agent-config :identifier)))
        (success (plist-get args :on-success)))
    (apply original
           (plist-put (copy-sequence args) :on-success
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (+agent-acp--remember-interactive-model
                             identifier (agent-shell--current-model-id (agent-shell--state)))))
                        (when success (funcall success)))))))

(advice-add 'agent-shell--config-option-set-model-id :around #'+agent-acp--set-model)

(defun +agent-acp-available-model-ids (&optional buffer)
  "Return model IDs advertised by the ACP session in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (mapcar (lambda (model) (map-elt model :model-id))
            (agent-shell--get-available-models (agent-shell--state)))))

(defun +agent-acp-current-model-id (&optional buffer)
  "Return the selected ACP model ID in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (agent-shell--current-model-id (agent-shell--state))))

(defun +agent-acp-export-model-id (model-id &optional buffer)
  "Resolve export MODEL-ID against models advertised in BUFFER.
Claude's Sonnet alias may be advertised with a versioned model ID."
  (let ((available (+agent-acp-available-model-ids buffer)))
    (or (and (member model-id available) model-id)
        (and (equal model-id "sonnet")
             (seq-find (lambda (id)
                         (and (stringp id)
                              (string-match-p "sonnet" (downcase id))))
                       available)))))

(defun +agent-acp-pinned-config (config model-id effort)
  "Return a copy of CONFIG pinned to MODEL-ID at reasoning EFFORT.
A task that owns its session (LaTeX export, MarginNote sync) chooses its own
model; the pin keeps that choice out of the remembered interactive model."
  (let* ((copy (copy-tree config))
         (effort-key (pcase (map-elt copy :identifier)
                       ('codex "reasoning_effort")
                       ('claude-code "effort")))
         (default-options (map-elt copy :default-config-options)))
    (when (and (stringp model-id) (not (string-empty-p model-id)))
      (setf (alist-get :agent-pinned-model copy) model-id
            (alist-get :default-model-id copy)
            (lambda () (+agent-acp-export-model-id model-id))))
    (when (and effort-key (stringp effort) (not (string-empty-p effort)))
      (setf (alist-get :default-config-options copy)
            (lambda ()
              (let ((options (if (functionp default-options)
                                 (funcall default-options)
                               default-options)))
                (cons (cons effort-key effort)
                      (seq-remove (lambda (entry)
                                    (equal (car-safe entry) effort-key))
                                  options))))))
    copy))



(defun +agent-acp-set-model (model-id &optional buffer)
  "Select MODEL-ID in BUFFER through ACP after checking session state."
  (with-current-buffer (or buffer (current-buffer))
    (unless (map-nested-elt (agent-shell--state) '(:session :id))
      (user-error "No active agent session"))
    (when (or (map-elt (agent-shell--state) :active-requests)
              (run-hook-with-args-until-success '+agent-acp-busy-functions (current-buffer)))
      (user-error "Agent is starting or running; finish/cancel the current turn before changing model"))
    (unless (member model-id (+agent-acp-available-model-ids))
      (user-error "Model is not available in this session: %s" model-id))
    (unless (equal model-id (+agent-acp-current-model-id))
      (agent-shell--config-option-set-model-id :model-id model-id))))

(defun +agent-acp-agent-buffer-p (buffer)
  "Return non-nil when BUFFER is an embedded agent-shell session."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (derived-mode-p 'agent-shell-mode))))

(defvar-local +agent-acp-session-name nil
  "D-031 session name served by this agent-shell buffer, or nil.")
(put '+agent-acp-session-name 'permanent-local t)

(defvar-local +agent-acp-retired nil
  "Non-nil after a newer physical generation takes this session's name.")
(put '+agent-acp-retired 'permanent-local t)

(defvar-local +agent-acp-session-agent nil
  "Agent id of the session served by this agent-shell buffer.")
(put '+agent-acp-session-agent 'permanent-local t)

(defvar-local +agent-acp-session-root nil
  "Project root of the session served by this agent-shell buffer.")
(put '+agent-acp-session-root 'permanent-local t)

(defvar-local +agent-acp-session-origin nil
  "How this agent-shell buffer was started.
One of `run', `popup', `manual', `foreign', `pi', `takeover', `probe',
`marginnote' (a MarginNote sync task), `latex-export' or `side' (an
ephemeral side chat, see `+agent-acp-ephemeral-origins').
Every entry point records it through `+agent-acp-adopt' so one project
view can list and manage sessions whatever opened them.")
(put '+agent-acp-session-origin 'permanent-local t)

(defvar-local +agent-acp-display-buffer-function nil
  "Optional explicit domain display or preferred popup surface.
The function receives the live agent buffer.  Conventional placement and
return belong to windows; obsolete manual/popup window closures are ignored.")
(put '+agent-acp-display-buffer-function 'permanent-local t)

(defvar-local +agent-acp--origin-window nil
  "Obsolete buffer-level source, retained for compatibility.
New views store their return point on the window through tools/agent/views.")
(put '+agent-acp--origin-window 'permanent-local t)

(defvar-local +agent-acp-last-used-at nil
  "Time this Noema-owned physical Session was last used or displayed.")
(put '+agent-acp-last-used-at 'permanent-local t)

(defvar-local +agent-acp-context-warning-level 0
  "Highest Noema context pressure warning emitted for this physical Session.")
(put '+agent-acp-context-warning-level 'permanent-local t)

;; D-033: every physical agent-shell buffer stays out of the global tab-line
;; and tab-bar.  A project's sessions are tabs of one bottom-right Agent
;; window; each tab is the session's own interactive agent-shell buffer.

(autoload '+agent-session-menu "tools/agent/menu" nil t)

(defvar +agent-prefix-map)
(defvar +agent-acp-tabs-mode-map (make-sparse-keymap)
  "Keymap for managing sessions inside the Agent window.")

;; Bound at top level, not in the `defvar' initial value: reloading this file
;; must update the keymap that live Agent buffers already use.  Only what moves
;; between views lives here; agent-shell keeps its own keys, and every other
;; action is in `+agent-actions' (`?', `C-c C-z', `C-c A', or a slash command).
(dolist (binding '(("C-c C-n" . +agent-acp-next-session)
                   ("C-c C-p" . +agent-acp-previous-session)
                   ("C-c C-e" . +agent-acp-focus-input)
                   ("C-c C-o" . +agent-acp-hide)
                   ("C-c C-l" . +agent-acp-session-manager)
                   ("C-c C-z" . +agent-session-menu)
                   ([remap agent-shell-interrupt] . +agent-acp-stop)
                   ("?" . +agent-acp-help-or-insert)))
  (define-key +agent-acp-tabs-mode-map
              (if (stringp (car binding)) (kbd (car binding)) (car binding))
              (cdr binding)))
;; A reload must also drop keys an older load bound.
(dolist (key '("C-c C-a" "C-c C-v" "C-c C-x" "C-c C-r" "C-c C-k" "C-c M-k" "C-c C-w"
               "C-c C-f" "C-c C-t" "C-c C-q" "C-c C-d" "C-c C-j" "C-c ?"))
  (define-key +agent-acp-tabs-mode-map (kbd key) nil t))

(define-minor-mode +agent-acp-tabs-mode
  "Show this agent-shell buffer as one tab of its project Agent window.

The buffer remains the real, interactive agent-shell session.  Its tab line
lists every live session of the same project; choosing a tab shows that
session's own buffer in the Agent window instead of copying a transcript.
Right-click a tab for its actions;
`?' outside the input opens every action.

\\{+agent-acp-tabs-mode-map}"
  :lighter nil
  :keymap +agent-acp-tabs-mode-map
  (if +agent-acp-tabs-mode
      (setq-local tab-line-format '(:eval (+agent-acp--tab-line)))
    (kill-local-variable 'tab-line-format)))

(with-eval-after-load 'evil
  ;; Normal state owns `?' for search; in an Agent window it is help.
  (when (fboundp 'evil-define-minor-mode-key)
    (evil-define-minor-mode-key 'normal '+agent-acp-tabs-mode
      (kbd "?") #'+agent-acp-help-or-insert)
    (dolist (state '(normal insert visual motion))
      (evil-define-minor-mode-key state '+agent-acp-tabs-mode
        (kbd "C-c C-e") #'+agent-acp-focus-input))))

(defun +agent-acp--sync-shell-buffer-name (buffer)
  "Keep shell-maker's native input/output target attached to BUFFER.
The shared registry gives sessions stable display names.  Shell-maker resolves its
process through this buffer-local name, so it must follow every rename."
  (with-current-buffer buffer
    (setq-local shell-maker--buffer-name-override (buffer-name))))

(defun +agent-acp--make-buffer-visible (buffer)
  "Migrate managed agent BUFFER out of Emacs's internal-buffer namespace."
  (when (and (+agent-acp-agent-buffer-p buffer)
             (string-prefix-p " *Agent · " (buffer-name buffer)))
    (with-current-buffer buffer
      (rename-buffer (substring (buffer-name) 1) t)
      (+agent-acp--sync-shell-buffer-name buffer))))

(defun +agent-acp--workspace-root (root)
  "Normalize ROOT with Remote's client projection when available."
  (let ((root (file-name-as-directory (expand-file-name (or root default-directory)))))
    (if (and (fboundp 'remote-canonicalize-emacs-file-name)
             (fboundp 'remote-client-file-name))
        (let ((logical (remote-canonicalize-emacs-file-name root)))
          (or (remote-client-file-name logical) logical))
      root)))

(defun +agent-acp--tab-key (buffer)
  "Return agent BUFFER's stable tab key: named, then retired."
  (let ((name (buffer-local-value '+agent-acp-session-name buffer)))
    (cons (if name 0 1)
          (or name (buffer-name buffer)))))

(defun +agent-session-kind (session)
  "Return the management kind of a SESSION snapshot or physical buffer.
Pi's managed roles are independent of the provider used by ordinary agents."
  (let ((origin (if (bufferp session)
                    (and (buffer-live-p session) (buffer-local-value '+agent-acp-session-origin session))
                  (or (plist-get session :origin)
                      (when-let* ((buffer (plist-get session :buffer)) ((buffer-live-p buffer)))
                        (buffer-local-value '+agent-acp-session-origin buffer))))))
    (if (memq origin '(pi pi-emacs pi-project)) 'pi 'agent)))

(defun +agent-acp--management-domain (buffer)
  "Read BUFFER's logical management role, independently of its provider."
  (if (eq (+agent-session-kind buffer) 'agent) 'agent
    (if (eq (buffer-local-value '+agent-acp-session-origin buffer) 'pi-emacs) 'pi-emacs 'pi-project)))

(defun +agent-acp-session-manager ()
  "Open the current chat's management owner."
  (interactive)
  (unless (run-hook-with-args-until-success '+agent-session-action-functions 'manage (current-buffer))
    (if (eq (+agent-session-kind (current-buffer)) 'pi)
        (user-error "Pi's session owner is not attached")
      (call-interactively #'+agent/manager))))

(defun +agent-pool-key (session)
  "Stable project-pool identity of shared SESSION; no host discovery or I/O."
  (list (or (plist-get session :target) "local")
        (+agent-acp--workspace-root (or (plist-get session :root)
                                       (plist-get session :workspace)))))

(defun +agent-acp--buffer-pool-key (buffer &optional root)
  "Return BUFFER's pool identity using already registered state."
  (with-current-buffer buffer
    (+agent-pool-key (list :root (or root +agent-acp-session-root)
                           :target (and (fboundp 'remote-file-name-target)
                                        (remote-file-name-target default-directory))))))

(defun +agent-acp--tab-sessions (&optional root)
  "Query the current chat's management domain, including retired generations."
  (let* ((kind (+agent-session-kind (current-buffer)))
         (main (eq +agent-acp-session-origin 'pi-emacs))
         (sessions (+agent-pool-sessions (or root +agent-acp-session-root) nil kind)))
    (if (eq kind 'agent) sessions
      (seq-filter (lambda (session) (eq main (eq (plist-get session :origin) 'pi-emacs))) sessions))))

(defun +agent-acp--project-buffers (&optional root)
  "Return the current management domain's user-facing tabs in order."
  (let ((sessions (+agent-acp--tab-sessions root)))
    (sort
     (mapcar (lambda (session) (plist-get session :buffer))
             (seq-filter (lambda (session)
                           (let ((buffer (plist-get session :buffer)))
                             (or (not (+agent-acp-retired-p buffer))
                                 (+agent-acp-busy-p buffer)
                                 (> (+agent-acp-pending-prompt-count buffer) 0)
                                 (+agent-acp--unsent-input-p buffer)))) sessions))
     (lambda (left right)
       (let ((left (+agent-acp--tab-key left)) (right (+agent-acp--tab-key right)))
         (if (= (car left) (car right)) (string-lessp (cdr left) (cdr right))
           (< (car left) (car right))))))))

(defun +agent-acp--tab-label (buffer)
  "Return the tab label of agent BUFFER."
  (with-current-buffer buffer
    (cond
     (+agent-acp-session-name
      (format "%s · %s" +agent-acp-session-name
              (or +agent-acp-session-agent "agent")))
     ;; A retired buffer gave its name to a newer physical session.
     ((string-match "\\*\\(?:Noema \\)?Agent · \\(.+\\) @ [^*]*\\*" (buffer-name))
      (format "%s (retired)" (match-string 1 (buffer-name))))
     (t (string-trim (buffer-name))))))

(defun +agent-acp-retired-p (buffer)
  "Whether BUFFER relinquished its name to a new physical generation."
  (and (buffer-live-p buffer)
       (or (buffer-local-value '+agent-acp-retired buffer)
           (string-match-p " (retired)\\(?:<[0-9]+>\\)?\\'" (buffer-name buffer)))
       (not (buffer-local-value '+agent-acp-session-name buffer))))

(defun +agent-session-group (buffer)
  "Return BUFFER's source group without imposing a second execution owner."
  (or (and (+agent-acp-retired-p buffer) '(:key retired :label "Previous generations"))
      (run-hook-with-args-until-success '+agent-session-group-functions buffer)
      (pcase (buffer-local-value '+agent-acp-session-origin buffer)
        ('popup '(:key popup :label "Portal"))
        ('side '(:key side :label "Side conversations"))
        ('worktree '(:key worktree :label "Worktrees"))
        (_ '(:key interactive :label "Interactive")))))

(defun +agent-acp--tab-line ()
  "Return the session tab line of the current agent buffer's project."
  (let ((current (current-buffer))
        (manager-map (make-sparse-keymap)))
    (define-key manager-map [tab-line mouse-1]
                (lambda (event) (interactive "e")
                  (select-window (posn-window (event-start event)))
                  (call-interactively #'+agent-acp-session-manager)))
    (concat
     (propertize (if (eq (+agent-session-kind current) 'pi) " Pi  " " Agents  ") 'face 'tab-line-tab-inactive
                 'keymap manager-map 'mouse-face 'tab-line-highlight
                 'help-echo (if (eq (+agent-session-kind current) 'pi)
                                "Pi Sessions · C-c C-l" "Agent Manager · H-A / C-c C-l"))
     (mapconcat
     (lambda (buffer)
       (let ((selected (eq buffer current))
             (map (make-sparse-keymap)))
         (define-key map [tab-line mouse-1]
                     (lambda (event)
                       (interactive "e")
                       (select-window (posn-window (event-start event)))
                       (+agent-acp-show-buffer buffer)))
         ;; tab-line binds the press globally to its own tab menu, so the
         ;; session menu must own the press and swallow the release.
         (define-key map [tab-line down-mouse-3]
                     (lambda (event)
                       (interactive "e")
                       (select-window (posn-window (event-start event)))
                       (+agent-acp-show-buffer buffer)
                       (call-interactively #'+agent-session-menu)))
         (define-key map [tab-line mouse-3] #'ignore)
         (propertize (concat (if (+agent-acp-busy-p buffer) " ● " "  ")
                             (or (buffer-local-value '+agent-acp-session-name buffer)
                                 (+agent-acp--tab-label buffer))
                             (let ((queued (+agent-acp-pending-prompt-count buffer)))
                               (if (> queued 0) (format " +%d" queued) ""))
                             " ")
                     'face (if selected 'tab-line-tab-current 'tab-line-tab-inactive)
                     'mouse-face 'tab-line-highlight
                     'help-echo (if selected
                                    "mouse-3: manage this agent session"
                                  "mouse-1: show this agent session\nmouse-3: manage it")
                     'keymap map)))
     (+agent-acp--project-buffers +agent-acp-session-root)
     " "))))

(defvar +agent-acp--tabs-timer nil)

(defun +agent-acp--refresh-tabs (&rest _)
  "Redraw Agent window tab lines after a project's session set changed."
  (unless (timerp +agent-acp--tabs-timer)
    (setq +agent-acp--tabs-timer
          (run-at-time 0 nil #'+agent-acp--flush-tabs))))

(defun +agent-acp--flush-tabs ()
  (setq +agent-acp--tabs-timer nil)
  (tab-line-force-update t))

(defun +agent-acp--cancel-tabs-h ()
  (when (timerp +agent-acp--tabs-timer) (cancel-timer +agent-acp--tabs-timer))
  (setq +agent-acp--tabs-timer nil))

(add-hook 'kill-emacs-hook #'+agent-acp--cancel-tabs-h)

(defun +agent-acp--install-workspace-tabs (buffer)
  "Make agent BUFFER a tab of its project Agent window.
The buffer stays out of the global tab-line and tab-bar (D-033) and is still
the real agent-shell buffer, so it accepts input and interrupts directly."
  (+agent-acp--make-buffer-visible buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (+agent-acp-agent-buffer-p buffer)
        (setq-local +buffer-end-function #'+agent-acp-close
                    +buffer-hide-function #'+agent-acp-hide)
        (+agent-acp--install-close-hooks-h))
      (+agent-acp--sync-shell-buffer-name buffer)
      (setq-local tab-line-exclude t)
      ;; Hooks installed by the transcript-copy workspace of an older load.
      (remove-hook 'after-change-functions '+agent-acp--workspace-source-changed t)
      (remove-hook 'kill-buffer-hook '+agent-acp--workspace-source-killed t)
      (unless +agent-acp-tabs-mode
        (+agent-acp-tabs-mode 1))
      (add-hook 'kill-buffer-hook #'+agent-acp--refresh-tabs nil t))
    (+agent-acp--refresh-tabs)))

(defun +agent-acp-switch-session ()
  "Show another live agent session of this project in the Agent window."
  (interactive)
  (call-interactively (if (eq (+agent-session-kind (current-buffer)) 'pi)
                         #'+agent-acp-session-manager #'+agent/pool-switch)))

(defun +agent-acp-next-session (&optional count)
  "Show the COUNTth next session tab of this project in the Agent window."
  (interactive "p")
  (let* ((buffers (+agent-acp--project-buffers +agent-acp-session-root))
         (index (seq-position buffers (current-buffer))))
    (unless buffers
      (user-error "No live agent session in this project"))
    (+agent-acp-show-buffer
     (nth (mod (+ (or index 0) (or count 1)) (length buffers)) buffers))))

(defun +agent-acp-previous-session (&optional count)
  "Show the COUNTth previous session tab of this project in the Agent window."
  (interactive "p")
  (+agent-acp-next-session (- (or count 1))))

(defvar +agent-acp-busy-functions nil
  "Abnormal hook called with an agent buffer; non-nil while a Run uses it.")

(defvar +agent-acp-stop-functions nil
  "Abnormal hook called with an agent buffer; non-nil after one stopped its Run.")

(defvar-local +agent-acp--prompt-owed nil
  "Non-nil when a Noema-driven turn ended without a live input prompt yet.")

(defun +agent-acp--default-buffer ()
  "Return the agent buffer a command should act on from the current context."
  (or (and (+agent-acp-agent-buffer-p (current-buffer)) (current-buffer))
      (when-let* ((window (+agent-acp--workspace-window))
                  ((+agent-acp-agent-buffer-p (window-buffer window))))
        (window-buffer window))
      (let ((directory (expand-file-name default-directory)))
        (seq-find (lambda (buffer)
                    (and (+agent-acp-agent-buffer-p buffer)
                         (when-let* ((root (buffer-local-value '+agent-acp-session-root buffer)))
                           (string-prefix-p root directory))))
                  (buffer-list)))))

(defun +agent-acp-command-buffer (&optional buffer)
  "Return BUFFER or the agent buffer of the current context, or signal."
  (let ((candidate (or buffer (+agent-acp--default-buffer))))
    (unless (+agent-acp-agent-buffer-p candidate)
      (user-error "No agent session here"))
    candidate))

(declare-function agent-shell-fork-tree "agent-shell-fork-tree" ())
(defvar agent-shell-fork-tree--source)

(defun +agent-acp-conversation-tree (&optional buffer)
  "Browse BUFFER's native ACP conversation tree in the Noema workflow.
The tree is an in-memory view of native sessions in the agent's workspace;
Noema's named Sessions list and WorkNode DAG retain their own meanings."
  (interactive)
  (let ((source (+agent-acp-command-buffer buffer)))
    (unless (and (+agent-session-capability-p source :list)
                 (+agent-session-capability-p source :load))
      (user-error "This agent needs ACP session/list and session/load for a conversation tree"))
    (unless (require 'agent-shell-fork-tree nil t)
      (user-error "agent-shell-fork-tree is not installed; restore packages from package-lock.el"))
    (with-current-buffer source
      ;; Upstream resolves the ACP cwd to a target-native path, then calls
      ;; `file-truename' on it.  On a remote target that would inspect this
      ;; machine's filesystem.  The Noema process directory is already the
      ;; canonical ACP cwd, so keep that native path verbatim for tree lookup.
      (cl-letf (((symbol-function 'file-truename) #'identity))
        (agent-shell-fork-tree)))))

(defun +agent-acp--conversation-tree-visit-a (original &rest args)
  "Bring an upstream tree continuation or fork into Noema's Agent workspace."
  (let* ((source agent-shell-fork-tree--source)
         (root (and (buffer-live-p source)
                    (buffer-local-value '+agent-acp-session-root source)))
         (agent (and (buffer-live-p source)
                     (+agent-acp-state-value source '(:agent-config :identifier))))
         (directory (and (buffer-live-p source)
                         (expand-file-name (buffer-local-value 'default-directory source))))
         (all-shells (symbol-function 'agent-shell-buffers))
         ;; Upstream's endpoint reuse checks the native ID alone.  IDs are
         ;; scoped to an agent and execution directory in Noema, including
         ;; remote targets, so narrow that lookup during this action.
         (window (cl-letf (((symbol-function 'agent-shell-buffers)
                            (lambda ()
                              (seq-filter
                               (lambda (buffer)
                                 (and (equal agent (+agent-acp-state-value
                                                    buffer '(:agent-config :identifier)))
                                      (equal directory
                                             (expand-file-name
                                              (buffer-local-value 'default-directory buffer)))))
                               (funcall all-shells)))))
                   (apply original args)))
         (target (and (window-live-p window) (window-buffer window))))
    (when (+agent-acp-agent-buffer-p target)
      ;; The upstream tree starts a native ACP session.  Give a newly created
      ;; branch a Noema name, so Sessions and context handoff can find it.
      (unless (buffer-local-value '+agent-acp-session-name target)
        (+agent-acp-adopt target :origin 'tree :root root))
      (+agent-acp-show-buffer target)
      (when (and (window-live-p window)
                 (not (window-parameter window 'agent-workspace))
                 (eq (window-buffer window) target))
        (quit-window nil window)))
    window))

(with-eval-after-load 'agent-shell-fork-tree
  (advice-add 'agent-shell-fork-tree--visit :around
              #'+agent-acp--conversation-tree-visit-a))

(defun +agent-acp-client-exited-p (buffer)
  "Whether BUFFER's ACP client process existed and has ended.
Nothing that process owed can still arrive, so its requests are history."
  (let ((process (+agent-acp-state-value buffer '(:client :process))))
    (and (processp process) (not (process-live-p process)))))

(defun +agent-acp--permission-request (state)
  "Return the tool call in STATE awaiting a permission answer, or nil."
  (cdr (seq-find (lambda (entry) (map-elt (cdr entry) :permission-request-id))
                 (map-elt state :tool-calls))))

(defun +agent-acp--pending-elicitation (state)
  "Return the form in STATE awaiting an answer, or nil."
  (cdr (seq-find (lambda (entry) (eq (map-elt (cdr entry) :status) 'pending))
                 (map-elt state :elicitations))))

(defun +agent-acp--working-requests-p (state)
  "Whether STATE has work beyond a read-only native-history query.
Unknown requests remain protected; pending RPCs still exclude auto-retention."
  (cl-some (lambda (request)
             (not (and (listp request) (equal (map-elt request :method) "session/list"))))
           (map-elt state :active-requests)))

(defun +agent-acp-request-active-p (buffer)
  "Whether protocol requests or a domain execution still own BUFFER."
  (when (+agent-acp-agent-buffer-p buffer)
    (or (and (not (+agent-acp-client-exited-p buffer))
             (let ((state (+agent-acp--state buffer)))
               (or (+agent-acp--working-requests-p state)
                   (+agent-acp--permission-request state)
                   (+agent-acp--pending-elicitation state))))
        (run-hook-with-args-until-success '+agent-acp-busy-functions buffer))))

(defun +agent-acp-busy-p (buffer)
  "Return non-nil while agent BUFFER answers a prompt or serves a Noema Run."
  (and (+agent-acp-agent-buffer-p buffer)
       (or (and (buffer-local-value 'shell-maker--busy buffer)
                (not (+agent-acp-client-exited-p buffer)))
           (+agent-acp-request-active-p buffer))))

(defvar-local +agent-acp--held-prompts nil
  "Queued prompts a stop or failure took out of automatic delivery.
agent-shell sends `:pending-prompts' after any later turn that ends normally,
so a queue the person stopped must not stay there.  See C37.")
(put '+agent-acp--held-prompts 'permanent-local t)

(defun +agent-acp-held-prompt-count (buffer)
  "Return how many of BUFFER's queued prompts wait for an explicit resume."
  (if (buffer-live-p buffer)
      (length (buffer-local-value '+agent-acp--held-prompts buffer))
    0))

(defun +agent-acp-pending-prompt-count (buffer)
  "Return how many prompts the person queued in agent BUFFER, held ones included.
agent-shell owns delivery of its native queue; `tools/agent/queue' edits both
parts, and lists read this count to show queued intent."
  (if (buffer-live-p buffer)
      (+ (+agent-acp-held-prompt-count buffer)
         (length (ignore-errors
                   (map-elt (buffer-local-value 'agent-shell--state buffer) :pending-prompts))))
    0))

(defun +agent-acp--hold-queue ()
  "Move the current buffer's native queue out of automatic delivery.
Upstream asks with `y-or-n-p' from the ACP response callback when a cancelled
turn leaves prompts queued.  Emptying the queue from the turn event, which runs
just before that question, keeps every prompt and asks nothing."
  (when-let* ((pending (map-elt agent-shell--state :pending-prompts)))
    (setq +agent-acp--held-prompts (append +agent-acp--held-prompts pending))
    (map-put! agent-shell--state :pending-prompts nil)
    (condition-case nil
        (agent-shell--update-fragment
         :state agent-shell--state
         :block-id (format "%s-queue-held" (map-elt agent-shell--state :request-count))
         :label-left "Queue held"
         :body (format "%d queued prompt%s held. Review, edit or resume them with C-c A q."
                       (length +agent-acp--held-prompts)
                       (if (cdr +agent-acp--held-prompts) "s" ""))
         :create-new t :navigation 'never
         :above-last-prompt (not (shell-maker-busy)))
      (error nil))
    t))

(declare-function agent-shell-get-model-name "agent-shell" (state))

(defvar-local +agent-acp--context-last 0
  "Context tokens this session's agent reported last.")
(put '+agent-acp--context-last 'permanent-local t)

(defvar-local +agent-acp--usage-report-seq 0
  "Count token or context usage reports received for this ACP buffer.")
(put '+agent-acp--usage-report-seq 'permanent-local t)

(defvar-local +agent-acp--context-peak 0
  "Most context tokens this session's agent has reported.")
(put '+agent-acp--context-peak 'permanent-local t)

(defvar-local +agent-acp--compactions 0
  "Context compactions seen in this session, see `+agent-acp--note-context'.")
(put '+agent-acp--compactions 'permanent-local t)

(defconst +agent-acp--compaction-ratio 0.7
  "A context report below this share of the previous one counts as a compaction.")

(defconst +agent-acp--compaction-floor 10000
  "Context below this many tokens is too small for a drop to mean compaction.")

(defvar +agent-acp-rate-limits-file
  (locate-user-emacs-file "var/agent/rate-limits.json")
  "Last subscription rate limits the agents reported, kept across restarts.")

(defvar +agent-acp--rate-limits nil
  "Table \"AGENT/WINDOW\" -> plist, loaded from the file on first use.")

(defun +agent-acp-rate-limits ()
  "Return the latest subscription rate limit per agent and window.
A table from \"AGENT/WINDOW\" to a plist with :agent, :window, :utilization
\(0.0-1.0 or nil), :status, :resets-at (epoch seconds or nil) and
:updated-at.  Filled from the `_claude/rateLimit' metadata claude-agent-acp
attaches to `usage_update', and kept in `+agent-acp-rate-limits-file'
so the last value survives a restart; :updated-at shows its age."
  (or +agent-acp--rate-limits
      (setq +agent-acp--rate-limits
            (let ((table (make-hash-table :test #'equal)))
              (dolist (limit (condition-case nil
                                 (with-temp-buffer
                                   (insert-file-contents +agent-acp-rate-limits-file)
                                   (json-parse-buffer :object-type 'plist :array-type 'list
                                                      :null-object nil))
                               (error nil)))
                (when (and (plist-get limit :agent) (plist-get limit :window))
                  (puthash (format "%s/%s" (plist-get limit :agent) (plist-get limit :window))
                           limit table)))
              table))))

(defun +agent-acp--save-rate-limits ()
  "Write the rate-limit table as data."
  (condition-case err
      (let (limits)
        (maphash (lambda (_key limit)
                   (push (cl-loop for (key value) on limit by #'cddr
                                  when value append (list key value))
                         limits))
                 (+agent-acp-rate-limits))
        (make-directory (file-name-directory +agent-acp-rate-limits-file) t)
        (with-temp-file +agent-acp-rate-limits-file
          (insert (json-serialize (vconcat limits)))))
    (error (message "Could not save agent rate limits: %s" (error-message-string err)))))

(defvar +agent-acp-changed-functions nil
  "Abnormal hook run with an agent buffer after its visible state changed.
It runs after agent-shell stores reported usage, after every full header
update (session start, model or mode change, turn start and end) -- not
after the busy animation's cached ticks -- and when the buffer is killed.
Listeners should coalesce; several changes arrive together.")

(defun +agent-acp--changed ()
  "Tell `+agent-acp-changed-functions' the current agent buffer changed."
  (run-hook-with-args '+agent-acp-changed-functions (current-buffer)))

(defun +agent-acp--header-updated-a (&rest args)
  "Report a full agent-shell header update; ARGS are its keyword arguments."
  (unless (plist-get args :cache-enabled)
    (+agent-acp--changed)))

;;;; ── Attention of sessions no Run owns ────────────────────────────────────
;;
;; A Run's permission requests and results are recorded by the host, which
;; derives the Session's attention and the worker notifies.  A session the
;; person started by hand -- `C-c A n', a worktree session, the popup pool, a
;; resumed conversation -- has no Run, so it would finish or stop on a
;; permission request without telling anyone.  This is its Emacs-side
;; counterpart: it lives only in this Emacs and is cleared by looking.
;; Adopted from Agent Fleet's blocked/done attention loop.

(defcustom +agent-acp-notify-function #'+agent-acp-notify-default
  "Function called with TITLE and BODY when a session without a Run needs you.
It fires only while no Emacs frame has focus, when such a session asks for a
permission or finishes a turn.  nil turns these notifications off.  Runs
notify through `noema-agent-worker-notify-function' instead."
  :type '(choice (const :tag "Off" nil) function)
  :group '+agent-session)

(defvar +agent-acp-run-owned-functions nil
  "Abnormal hook run with an agent buffer; non-nil means a Run owns it now.
While one does, its attention comes from the host, so the Emacs-side
attention below stands aside rather than report the same event twice.")

(defvar-local +agent-acp-last-error nil
  "Last reported execution error; viewing clears unread attention, not this fact.")

(defvar-local +agent-acp-attention nil
  "Why this session needs the person: `permission', `done' or nil.
Set when the agent asks for a permission or finishes a turn while its buffer
is not on screen, and cleared when the buffer is shown.")

(defvar-local +agent-acp--attention-watched nil
  "Non-nil once this buffer's permission and turn events are watched.")

(defvar +agent-acp-turn-complete-functions nil
  "Abnormal hook called with agent BUFFER and EVENT after an ACP turn ends.
Consumers should defer filesystem work outside the event callback.")
(defvar +agent-acp-client-ended-functions nil
  "Functions called with BUFFER when its physical client ends, including shutdown.
Release owned callbacks/timers here; native conversation and view identity remain.")
(defvar-local +agent-acp--reported-turn nil)

(defun +agent-acp--report-turn-complete (buffer event)
  "Publish one terminal fact per exact physical native turn."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((key (list (+agent-acp-state-value buffer '(:client))
                       (+agent-acp-state-value buffer '(:session :id))
                       (+agent-acp-state-value buffer '(:request-count)))))
        (unless (equal key +agent-acp--reported-turn)
          (setq +agent-acp--reported-turn key)
          (run-hook-wrapped
           '+agent-acp-turn-complete-functions
           (lambda (function source payload)
             (condition-case err (funcall function source payload)
               (error (display-warning '+agent-acp (error-message-string err) :warning)))
             nil)
           buffer event))))))

(defvar +agent-acp-notification-functions nil
  "Functions called with BUFFER and attention REASON, even in the foreground.
A non-nil result owns delivery and suppresses the legacy notification.  The
Emacs host uses this to apply shared notification policy outside Noema.")

(defun +agent-acp-notify-default (title body)
  "Send TITLE and BODY through the host's `notify-send', when it has one."
  (when (fboundp 'notify-send)
    (funcall 'notify-send :title title :body body)))

(defun +agent-acp-attention-mark (buffer)
  "Return BUFFER's Emacs-side attention as a list mark, or nil."
  (when (buffer-live-p buffer)
    (pcase (buffer-local-value '+agent-acp-attention buffer)
      ('permission "!approve")
      ('input "!input")
      ('failed "!failed")
      ('done "done"))))

(defun +agent-acp-clear-attention (buffer)
  "Forget BUFFER's Emacs-side attention, telling listeners when it had one."
  (when (and (buffer-live-p buffer)
             (buffer-local-value '+agent-acp-attention buffer))
    (with-current-buffer buffer
      (setq-local +agent-acp-attention nil)
      (+agent-acp--changed))))

(defun +agent-acp--note-attention (buffer reason)
  "Record that BUFFER's agent needs the person for REASON."
  (when (and (buffer-live-p buffer)
             (not (memq (buffer-local-value '+agent-acp-session-origin buffer)
                        (cons 'probe +agent-acp-ephemeral-origins)))
             (not (run-hook-with-args-until-success
                   '+agent-acp-run-owned-functions buffer)))
    (let ((focused (seq-some (lambda (frame) (eq (frame-focus-state frame) t))
                             (frame-list))))
      ;; A buffer on screen is seen once Emacs has focus again; a mark on it
      ;; would never be cleared by showing it.
      (unless (seq-some (lambda (window)
                          (not (window-parameter window 'aaron-ui-preview)))
                        (get-buffer-window-list buffer nil 'visible))
        (with-current-buffer buffer
          (setq-local +agent-acp-attention reason)
          (+agent-acp--changed)))
      (when (and (not (condition-case nil
                          (run-hook-with-args-until-success
                           '+agent-acp-notification-functions buffer reason)
                        (error nil)))
                 +agent-acp-notify-function (not focused))
        (condition-case nil
            (funcall +agent-acp-notify-function "Agent"
                     (format "%s %s"
                             (or (buffer-local-value '+agent-acp-session-name buffer)
                                 (buffer-name buffer))
                             (pcase reason
                               ('permission "is waiting for your permission")
                               ('input "is waiting for your answer")
                               ('failed "failed")
                               (_ "finished its turn"))))
          (error nil))))))

(defun +agent-acp--attention-shown (window)
  "Clear the current agent buffer's attention now that a window shows it."
  (unless (or +agent-acp-previewing (window-parameter window 'aaron-ui-preview))
    (+agent-acp-clear-attention (current-buffer))))

(defcustom +agent-acp-changes-limit 200
  "Most distinct files one session remembers having changed."
  :type 'natnum :group '+agent-session)

(defvar-local +agent-acp--changes nil
  "Files this client reported changing, newest first, as (FILE . PLIST).
PLIST holds :count and :at.  It is an event record of this client, not Git
state, and it ends with the client.  See C44.")

(defvar-local +agent-acp--counted-changes nil
  "Tool call and file pairs already counted in the running turn.")

(defun +agent-acp--note-change (file)
  "Record that the current session changed FILE, as the agent named it."
  (when (and (stringp file) (not (string-empty-p file)))
    (let ((count (or (plist-get (cdr (assoc file +agent-acp--changes)) :count) 0)))
      (setq +agent-acp--changes
            (seq-take (cons (cons file (list :count (1+ count) :at (float-time)))
                            (assoc-delete-all file +agent-acp--changes))
                      +agent-acp-changes-limit)))))

(defun +agent-acp--note-tool-change (id tool-call)
  "Record the diffs of TOOL-CALL with ID once, when it has completed.
A tool call is reported many times; only a finished one changed anything."
  (when (equal (map-elt tool-call :status) "completed")
    (dolist (diff (map-elt tool-call :diffs))
      (let* ((file (map-elt diff :file)) (key (cons id file)))
        (unless (member key +agent-acp--counted-changes)
          (push key +agent-acp--counted-changes)
          (+agent-acp--note-change file))))))

(defvar-local +agent-acp--stop-reason nil
  "How the last turn ended, as the agent reported it; nil while one runs.")

(defvar-local +agent-acp--turn-started-at nil
  "When the running turn began, or nil.")

(defvar-local +agent-acp--exit-interrupted nil
  "Non-nil when the client exited with a turn open, so its outcome is unknown.")

(defun +agent-acp--turn-began ()
  "Record that a turn is running in the current buffer."
  (unless +agent-acp--turn-started-at
    (setq +agent-acp--turn-started-at (float-time)))
  (setq +agent-acp--stop-reason nil))

(defun +agent-acp--turn-ended (reason)
  "Record that the current buffer's turn ended with stop REASON."
  (setq +agent-acp--turn-started-at nil
        +agent-acp--stop-reason reason
        +agent-acp--counted-changes nil)
  (unless (member reason '("end_turn" ""))
    (+agent-acp--hold-queue))
  (unless (member reason '("end_turn" "cancelled" ""))
    (setq-local +agent-acp-last-error (format "Turn ended: %s" reason))))

(defun +agent-acp--subscribe-attention (buffer)
  "Watch BUFFER's permission requests and finished turns, once."
  (unless (buffer-local-value '+agent-acp--attention-watched buffer)
    (with-current-buffer buffer
      (setq-local +agent-acp--attention-watched t)
      (add-hook 'window-buffer-change-functions
                #'+agent-acp--attention-shown nil t))
    (+agent-acp-subscribe
     :buffer buffer :event 'permission-request
     :callback (lambda (_event) (+agent-acp--note-attention buffer 'permission)))
    (+agent-acp-subscribe
     :buffer buffer :event 'elicitation-request
     :callback (lambda (_event) (+agent-acp--note-attention buffer 'input)))
    (+agent-acp-subscribe
     :buffer buffer :event 'error
     :callback (lambda (event)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq +agent-acp-last-error (or (map-elt event :data) t)
                           +agent-acp--turn-started-at nil)
                     (+agent-acp--hold-queue))
                   (+agent-acp--note-attention buffer 'failed))))
    (+agent-acp-subscribe
     :buffer buffer :event 'input-submitted
     :callback (lambda (_event)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (+agent-acp--clear-stop)
                     (+agent-acp--turn-began)
                     (setq +agent-acp-last-error nil))
                   (+agent-acp-clear-attention buffer))))
    (+agent-acp-subscribe
     :buffer buffer :event 'tool-call-update
     :callback (lambda (event)
                 (let ((data (map-elt event :data)))
                   (+agent-acp--note-tool-change (map-elt data :tool-call-id)
                                                 (map-elt data :tool-call)))))
    (+agent-acp-subscribe
     :buffer buffer :event 'file-write
     :callback (lambda (event)
                 (+agent-acp--note-change (map-elt (map-elt event :data) :path))))
    (+agent-acp-subscribe
     :buffer buffer :event 'turn-complete
     :callback (lambda (event)
                 (let ((reason (format "%s" (or (map-elt (map-elt event :data)
                                                         :stop-reason)
                                                ""))))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (+agent-acp--clear-stop)
                       (+agent-acp--turn-ended reason)
                       (+agent-acp--changed)))
                   (+agent-acp--report-turn-complete buffer event)
                   (pcase reason
                     ;; An interrupt is the person's own act and is not news.
                     ("cancelled" nil)
                     ((or "end_turn" "") (+agent-acp--note-attention buffer 'done))
                     (_ (+agent-acp--note-attention buffer 'failed))))))))

(defun +agent-acp--watch-attention (buffer)
  "Watch agent-shell BUFFER for attention; other buffers have no events."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
    (+agent-acp--subscribe-attention buffer)))

(defun +agent-acp--watch-attention-h ()
  "Subscribe the current agent-shell buffer to ACP attention events."
  (+agent-acp--watch-attention (current-buffer)))

;; The turn-complete hook must also reach bare agent-shell sessions (including
;; Claude) when session adoption is disabled or has not finished yet.
(add-hook 'agent-shell-mode-hook #'+agent-acp--watch-attention-h)
(dolist (buffer (buffer-list))
  (+agent-acp--watch-attention buffer))

(advice-add 'agent-shell--update-header-and-mode-line :after
            #'+agent-acp--header-updated-a)

(defun +agent-acp--note-context (used)
  "Record the context size USED just reported by the current agent buffer.
The agent computes USED itself, so a sharp drop from a sizeable context is a
compaction or a cleared conversation rather than cache variance (abtop
additionally checks cache reads because it derives context from them)."
  (when (and (numberp used) (> used 0))
    (when (and (>= +agent-acp--context-last +agent-acp--compaction-floor)
               (< used (* +agent-acp--compaction-ratio +agent-acp--context-last)))
      (cl-incf +agent-acp--compactions))
    (setq +agent-acp--context-last used
          +agent-acp--context-peak (max +agent-acp--context-peak used))))

(defvar +agent-acp--rate-limits-saved-at 0
  "When the rate-limit file was last written.")

(defconst +agent-acp--rate-limits-save-interval 300
  "Seconds after which an unchanged rate limit is rewritten, to keep its age.")

(defun +agent-acp-put-rate-limit (agent window utilization &rest properties)
  "Record that AGENT's WINDOW is UTILIZATION used, a fraction 0.0-1.0 or nil.
PROPERTIES may give :status and :resets-at (epoch seconds).  The file is
written only when a value changed, or after
`+agent-acp--rate-limits-save-interval' to keep its age true."
  (let* ((key (format "%s/%s" agent window))
         (utilization (and (numberp utilization) utilization))
         (limit (list :agent agent :window window :utilization utilization
                      :status (plist-get properties :status)
                      :resets-at (let ((value (plist-get properties :resets-at)))
                                   (and (numberp value) value))
                      :updated-at (float-time)))
         (old (gethash key (+agent-acp-rate-limits))))
    (puthash key limit (+agent-acp-rate-limits))
    (when (or (not (equal (plist-get old :utilization) utilization))
              (not (equal (plist-get old :resets-at) (plist-get limit :resets-at)))
              (not (equal (plist-get old :status) (plist-get limit :status)))
              (> (- (float-time) +agent-acp--rate-limits-saved-at)
                 +agent-acp--rate-limits-save-interval))
      (setq +agent-acp--rate-limits-saved-at (float-time))
      (+agent-acp--save-rate-limits))))

(defun +agent-acp--note-rate-limit (agent info)
  "Record claude-agent-acp rate-limit INFO reported by AGENT.
INFO names the window it concerns; newer Claude Code versions also attach
`unifiedWindows' with every window's utilization at once."
  (when-let* ((window (map-elt info 'rateLimitType)))
    (+agent-acp-put-rate-limit agent window (map-elt info 'utilization)
                                    :status (map-elt info 'status)
                                    :resets-at (map-elt info 'resetsAt)))
  (pcase-dolist (`(,window . ,value) (map-elt info 'unifiedWindows))
    (when (consp value)
      (+agent-acp-put-rate-limit agent (symbol-name window) (map-elt value 'utilization)
                                      :resets-at (map-elt value 'resetsAt)))))

(defun +agent-acp--usage-updated-a (&rest args)
  "After agent-shell stores reported usage, record what Noema tracks with it.
ARGS are agent-shell's keyword arguments; the notification form carries the
context size and, from claude-agent-acp, rate-limit metadata."
  (let* ((state (plist-get args :state))
         (buffer (map-elt state :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (or (seq-some (lambda (key) (map-elt (plist-get args :acp-update) key))
                            '(used size))
                  (seq-some (lambda (key) (map-elt (plist-get args :acp-usage) key))
                            '(totalTokens inputTokens outputTokens thoughtTokens
                              cachedReadTokens cachedWriteTokens)))
          (setq-local +agent-acp--usage-report-seq
                      (1+ +agent-acp--usage-report-seq)))
        (when-let* ((update (plist-get args :acp-update)))
          (+agent-acp--note-context (map-elt update 'used))
          (when-let* ((info (map-nested-elt update '(_meta _claude/rateLimit))))
            (+agent-acp--note-rate-limit
             (or +agent-acp-session-agent "agent") info))))
      (with-current-buffer buffer (+agent-acp--changed)))))

(advice-add 'agent-shell--update-usage-from-notification :after
            #'+agent-acp--usage-updated-a)
(advice-add 'agent-shell--save-usage :after #'+agent-acp--usage-updated-a)

(defun +agent-acp-usage (buffer)
  "Return what agent BUFFER's ACP connection reported about its usage.
A plist with :model, :model-id, :context-used, :context-size, :context-peak,
:compactions, :input, :output, :thought, :cached-read, :cached-write, :total,
:cost, :currency, :turns and :report-seq. Counts are non-negative integers, zero when
never reported; :cost is nil until the agent reports one.

agent-shell already records these from `usage_update' notifications and
PromptResponse usage, so this only reads that state: no process scan, no
request, no timer.  It is the one place Noema reads agent-shell's usage."
  (when (+agent-acp-agent-buffer-p buffer)
    (let* ((state (buffer-local-value 'agent-shell--state buffer))
           (usage (map-elt state :usage))
           (count (lambda (key)
                    (let ((value (map-elt usage key)))
                      (if (numberp value) (max 0 (truncate value)) 0)))))
      (list :model (ignore-errors (agent-shell-get-model-name state))
            :model-id (map-nested-elt state '(:session :model-id))
			:report-seq (buffer-local-value '+agent-acp--usage-report-seq buffer)
            :context-used (funcall count :context-used)
            :context-size (funcall count :context-size)
            :context-peak (buffer-local-value '+agent-acp--context-peak buffer)
            :compactions (buffer-local-value '+agent-acp--compactions buffer)
            :input (funcall count :input-tokens)
            :output (funcall count :output-tokens)
            :thought (funcall count :thought-tokens)
            :cached-read (funcall count :cached-read-tokens)
            :cached-write (funcall count :cached-write-tokens)
            :total (funcall count :total-tokens)
            ;; agent-shell starts every session at 0, so zero means unreported.
            :cost (let ((amount (map-elt usage :cost-amount)))
                    (and (numberp amount) (> amount 0) amount))
            :currency (map-elt usage :cost-currency)
            :turns (let ((turns (map-elt state :request-count)))
                     (if (natnump turns) turns 0))))))

(defcustom +agent-acp-ephemeral-origins '(side latex-export)
  "Session origins that never enter a project's durable session registry.
A side chat asks a quick question beside a session without becoming one of
the project's conversations; it is listed while it lives and then goes.
Adopted from Pisper's temporary side chat."
  :type '(repeat symbol)
  :group '+agent-session)

(defvar-local +agent-acp-side-parent nil
  "Name of the session this ephemeral side chat was opened beside.")
(put '+agent-acp-side-parent 'permanent-local t)

(defcustom +agent-acp-auto-stop-origins '(run probe pi side latex-export)
  "Session origins whose idle, hidden agents may be stopped automatically.
Automatic stops are the periodic warm-buffer sweep and closing a project when
its last `.noema' document goes.  Sessions a person opened -- `manual',
`takeover', `foreign' (a bare `M-x agent-shell') -- are never stopped behind
their back; close them with \\[+agent-acp-close] instead."
  :type '(repeat symbol)
  :group '+agent-session)



(defun +agent-acp-note-activity (&rest _)
  "Record that the current agent buffer changed: input, output or a prompt.
Idle time is measured from real activity, not from when the session was
last shown, so a conversation typed directly into the buffer stays warm."
  (setq +agent-acp-last-used-at (float-time)))

(defun +agent-acp--track-activity-h ()
  "Measure this agent buffer's idle time and report when it goes."
  (+agent-acp--install-close-hooks-h)
  (add-hook 'after-change-functions #'+agent-acp-note-activity nil t)
  (add-hook 'kill-buffer-hook #'+agent-acp--changed nil t)
  (add-hook 'kill-buffer-hook #'+agent-acp--clear-stop nil t)
  (add-hook 'kill-buffer-hook #'+agent-acp--clear-deferred-h nil t)
  (unless +agent-acp-last-used-at
    (+agent-acp-note-activity)))

(add-hook 'agent-shell-mode-hook #'+agent-acp--track-activity-h)
(dolist (buffer (buffer-list))
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer (+agent-acp--track-activity-h))))

(defun +agent-acp-auto-stoppable-p (buffer)
  "Return non-nil when an automatic lifecycle may stop agent BUFFER.
It must be idle, hold no queued prompt, shown in no window of any frame, and
started by an origin in
`+agent-acp-auto-stop-origins'.  Every automatic stop path asks this."
  (and (+agent-acp-agent-buffer-p buffer)
       (+agent-acp--retention-origin-p buffer)
       ;; Handshake/RPC work can precede shell-maker's turn flag.
       (eq (+agent-acp-state-value buffer '(:init-finished)) t)
       (not (+agent-acp-state-value buffer '(:client :pending-requests)))
       (not (+agent-acp-busy-p buffer))
       ;; Queued prompts are the person's intent still waiting to be sent.
       (zerop (+agent-acp-pending-prompt-count buffer))
       (not (+agent-acp--unsent-input-p buffer))
       (not (get-buffer-window buffer t))))

(defun +agent-acp--unsent-input-p (buffer)
  "Whether BUFFER contains a nonempty draft after its native input prompt."
  (when-let* ((start (+agent-acp--input-start buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (re-search-forward "[^ \t\n\r]" nil t)))))

(defun +agent-acp-confirm-stop (buffer action)
  "Return non-nil when ACTION may stop agent BUFFER, asking while it is busy."
  (or (not (or (+agent-acp-busy-p buffer)
               (> (+agent-acp-pending-prompt-count buffer) 0)
               (+agent-acp--unsent-input-p buffer)))
      (yes-or-no-p (format "%s has work or unsent input; %s it anyway? "
                           (+agent-acp--tab-label buffer) action))))

(defcustom +agent-acp-stop-timeout 5
  "Seconds to await cancellation before reporting an unconfirmed stop.
A timeout never declares success, replays a request, or kills another turn."
  :type 'number :group '+agent-session)

(defvar-local +agent-acp--stop-token nil)
(defvar-local +agent-acp--stop-timer nil)
(defvar-local +agent-acp--stop-unconfirmed nil)

(defun +agent-acp--clear-stop ()
  "Release this buffer's cancellation observer."
  (when (timerp +agent-acp--stop-timer) (cancel-timer +agent-acp--stop-timer))
  (setq +agent-acp--stop-timer nil +agent-acp--stop-token nil
        +agent-acp--stop-unconfirmed nil))

(defun +agent-acp--stop-expired (buffer state token)
  "Report missing acknowledgement only for the exact stopped execution."
  (when (and (+agent-acp-agent-buffer-p buffer)
             (eq state (+agent-acp--state buffer))
             (eq token (buffer-local-value '+agent-acp--stop-token buffer)))
    (with-current-buffer buffer
      (when (timerp +agent-acp--stop-timer) (cancel-timer +agent-acp--stop-timer))
      (setq +agent-acp--stop-timer nil)
      (cond
       ((not (and (eq (nth 0 token) (map-elt state :client))
                  (eq (nth 1 token) (map-nested-elt state '(:client :process)))
                  (equal (nth 2 token) (map-elt state :request-count))
                  (equal (nth 3 token) (map-nested-elt state '(:session :id)))))
        (+agent-acp--clear-stop))
       ((+agent-acp-busy-p buffer) (setq +agent-acp--stop-unconfirmed t))
       (t (+agent-acp--clear-stop)))
      (+agent-acp--changed))))

(defun +agent-acp-stop (&optional buffer)
  "Stop agent BUFFER's work: cancel its Noema Run, else interrupt the turn."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (unless (run-hook-with-args-until-success '+agent-acp-stop-functions buffer)
      (when (+agent-acp-client-exited-p buffer)
        (user-error "The agent process has ended; nothing is running to stop"))
      (unless (+agent-acp-busy-p buffer)
        (user-error "This session has no running turn"))
      (with-current-buffer buffer
        (unless +agent-acp--stop-token
          (let* ((state agent-shell--state)
                 (token (list (map-elt state :client)
                              (map-nested-elt state '(:client :process))
                              (map-elt state :request-count)
                              (map-nested-elt state '(:session :id)))))
            (setq +agent-acp--stop-token token)
            (condition-case err
                (progn
                  (+agent-acp-interrupt buffer t)
                  (when (eq +agent-acp--stop-token token)
                    (setq +agent-acp--stop-timer
                          (run-at-time +agent-acp-stop-timeout nil
                                       #'+agent-acp--stop-expired buffer state token)))
                  (+agent-acp--changed))
              (error (+agent-acp--clear-stop) (signal (car err) (cdr err))))))))))



(defun +agent-acp--query-kill-h ()
  "Protect work through every native buffer-ending entry point."
  (or (eq (current-buffer) +window-buffer-replacement)
      (+agent-acp-confirm-stop (current-buffer) "close")))

(defun +agent-acp--install-close-hooks-h ()
  "Keep native kill queries and cleanup consistent with the Agent menu."
  ;; Our queries also protect the native queue, plus active work and drafts.
  (remove-hook 'kill-buffer-query-functions
               #'agent-shell--prompt-queue-confirm-kill-buffer t)
  (remove-hook 'kill-emacs-query-functions
               #'agent-shell--prompt-queue-confirm-kill-emacs)
  (add-hook 'kill-emacs-query-functions #'+agent-acp--query-kill-emacs-h)
  (add-hook 'kill-buffer-query-functions #'+agent-acp--query-kill-h nil t)
  (add-hook 'kill-buffer-hook #'+agent-acp--before-kill-h -50 t))

(defun +agent-acp--unfinished-sessions ()
  "Return buffers of sessions that exiting Emacs would cost something.
Temporary sessions keep no conversation worth a question."
  (seq-filter
   (lambda (buffer)
     (and (+agent-acp-agent-buffer-p buffer)
          (not (memq (buffer-local-value '+agent-acp-session-origin buffer)
                     (cons 'probe +agent-acp-ephemeral-origins)))
          (or (+agent-acp-busy-p buffer)
              (> (+agent-acp-pending-prompt-count buffer) 0)
              (+agent-acp--unsent-input-p buffer))))
   (buffer-list)))

(defun +agent-acp--query-kill-emacs-h ()
  "Ask once before exiting Emacs ends sessions that still hold work."
  (let ((sessions (and (not noninteractive) (+agent-acp--unfinished-sessions))))
    (or (null sessions)
        (y-or-n-p (format "%d agent session%s still working or holding unsent input (%s); quit anyway? "
                          (length sessions) (if (cdr sessions) "s" "")
                          (mapconcat #'+agent-acp--tab-label sessions ", "))))))

(defun +agent-acp--before-kill-h ()
  "Notify owners and settle views after all queries accepted this kill.
The package's own cleanup still owns the physical client and protocol."
  (unless (eq (current-buffer) +window-buffer-replacement)
    (let* ((buffer (current-buffer))
           (next (car (delq buffer (+agent-acp--project-buffers +agent-acp-session-root)))))
      (+agent-acp--clear-stop)
      (dolist (function +agent-session-close-functions)
	(condition-case err (funcall function buffer)
          (error (message "Agent close observer: %s" (error-message-string err)))))
      (dolist (window (get-buffer-window-list buffer nil t))
	(unless (window-parameter window 'aaron-ui-preview)
          (if next
              (let ((record (window-parameter window '+agent-view)))
		(set-window-buffer window next)
		(when record
                  (set-window-parameter window '+agent-view
					(plist-put (copy-sequence record) :buffer next))))
            ;; Each view restores its own editor; never fall through native
            ;; buffer history to another agent.  Closing from a manager or a
            ;; background frame must not move the person's keyboard focus.
            (save-selected-window
              (with-selected-window window (+agent-view-hide buffer)))))))))

(defun +agent-acp-kill (buffer)
  "End BUFFER after its caller has authorized teardown, retaining history."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (+agent-acp--install-close-hooks-h)
      (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))

(defun +agent-acp-close (&optional buffer)
  "Close agent BUFFER's tab.  Its process stops; its name and history remain."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (with-current-buffer buffer
      (+agent-acp--install-close-hooks-h)
      (kill-buffer buffer))))

(defun +agent-acp--close-where (buffer predicate)
  "Close the tabs of BUFFER's project whose buffer satisfies PREDICATE."
  (dolist (other (with-current-buffer buffer
                  (mapcar (lambda (session) (plist-get session :buffer))
                          (+agent-acp--tab-sessions +agent-acp-session-root))))
    (when (and (funcall predicate other)
               (+agent-acp-confirm-stop other "close"))
      (+agent-acp-kill other))))

(defun +agent-acp-close-others (&optional buffer)
  "Close every other session tab of agent BUFFER's project."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (+agent-acp--close-where buffer (lambda (other) (not (eq other buffer))))
    (+agent-acp-show-buffer buffer)))

(defun +agent-acp-close-retired (&optional buffer)
  "Close the retired session tabs of agent BUFFER's project."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (+agent-acp--close-where
     buffer (lambda (other)
              (null (buffer-local-value '+agent-acp-session-name other))))))

(defun +agent-acp--input-start (buffer)
  "Return where the input after BUFFER's last prompt begins, or nil."
  (with-current-buffer buffer
    (when-let* ((prompt (bound-and-true-p comint-last-prompt))
                ((markerp (cdr prompt))))
      (marker-position (cdr prompt)))))

(defun +agent-acp-read-transcript (buffer offset limit)
  "Read at most LIMIT characters from BUFFER's output at OFFSET.
Flush pending rendering, exclude the editable draft and leave focus and unread
state alone.  Offsets address the currently available native transcript."
  (unless (and (integerp offset) (>= offset 0)
               (integerp limit) (<= 1 limit 32000))
    (user-error "offset must be >= 0 and limit must be 1..32000"))
  (+agent-render-flush buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let* ((end (or (+agent-acp--input-start buffer) (point-min)))
             (total (- end (point-min)))
             (start (+ (point-min) (min offset total)))
             (finish (min end (+ start limit))))
        `((text . ,(buffer-substring-no-properties start finish))
          (total . ,total)
          (nextOffset . ,(when (< finish end) (- finish (point-min)))))))))

(defun +agent-acp--live-input-p ()
  "Return non-nil when the last native prompt still owns editable input.
Plain agent-shell replies carry `agent-shell-ui-state' and `read-only',
but can lack `field=output' after a programmatic Noema submission."
  (and comint-last-prompt
       (agent-shell--live-input-prompt-p comint-last-prompt)
       (not (text-property-not-all
             (cdr comint-last-prompt) (point-max) 'agent-shell-ui-state nil))))

(defun +agent-acp--settle-prompt (buffer)
  "Write BUFFER's owed input prompt once its turn output is on screen.
Return non-nil when a prompt was written."
  (with-current-buffer buffer
    (+agent-acp--sync-shell-buffer-name buffer)
    (when (and +agent-acp--prompt-owed
               ;; A hidden buffer defers its output (render policy `visible');
               ;; the prompt must follow that output, not precede it.
               (not +agent-render--pending)
               (not (+agent-acp--working-requests-p agent-shell--state))
               (bound-and-true-p shell-maker--config))
      (agent-shell-heartbeat-stop :heartbeat (map-elt agent-shell--state :heartbeat))
      (agent-shell--collapse-expanded-activity-group agent-shell--state)
      (let ((written
             (unless (+agent-acp--live-input-p)
               (shell-maker-finish-output :config shell-maker--config :success t)
               t)))
        ;; Keep the retry flag if native prompt creation failed.  Never leave
        ;; a stale busy flag after a confirmed terminal response.
        (setq shell-maker--busy nil
              +agent-acp--prompt-owed nil)
        written))))

(defun +agent-acp-restore-prompt (buffer)
  "Give agent BUFFER a live input prompt after a Noema-driven turn.
Noema prompts through the ACP request API, bypassing shell-maker's input
flow, so nothing writes the next prompt when that turn ends.  Write it the
way agent-shell ends its own turns.  When the turn's output is still deferred
because the buffer is hidden, the prompt is written right after that output
renders.  Return non-nil when a prompt was written now."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer
      (unless (+agent-acp--working-requests-p agent-shell--state)
        (agent-shell-heartbeat-stop :heartbeat (map-elt agent-shell--state :heartbeat))
        (setq shell-maker--busy nil))
      (setq-local +agent-acp--prompt-owed t))
    (+agent-acp--settle-prompt buffer)))

(defun +agent-acp--settle-after-flush-a (&optional buffer)
  "Write the prompt owed by BUFFER after its deferred output rendered."
  (let ((buffer (or buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (buffer-local-value '+agent-acp--prompt-owed buffer))
      (+agent-acp--settle-prompt buffer))))

(unless (advice-member-p #'+agent-acp--settle-after-flush-a
                         '+agent-render-flush)
  (advice-add '+agent-render-flush :after
              #'+agent-acp--settle-after-flush-a))

;;;###autoload
(defun +agent-acp-focus-input (&optional buffer)
  "Show agent BUFFER in the Agent window with point at its input prompt."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    ;; Showing renders deferred output first; the owed prompt follows it.
    (+agent-acp-show-buffer buffer)
    (with-current-buffer buffer
      ;; Also repair sessions that completed before this code was loaded.
      ;; The native persistent prompt supports input during generation.
      (unless (+agent-acp-request-active-p buffer) (+agent-acp-restore-prompt buffer)))
    (when-let* ((window (get-buffer-window buffer (selected-frame))))
      (select-window window)
      (goto-char (point-max))
      (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-insert-state))
        (evil-insert-state))
      (set-window-point window (point))
      (recenter -3))
    buffer))

(defun +agent-acp-help ()
  "Describe how to work in the Agent window."
  (interactive)
  (with-help-window "*Agent Help*"
    (princ
     (substitute-command-keys
      (concat
       "Agent window

Each tab is one live agent session of this project.  The buffer is the real
agent-shell: type at its prompt and press RET to send, also during a turn.

M-x agent-shell\topen a new session in this window
  with C-u C-u\tpick a session, a new one or a saved conversation
C-c a\trecall this project's latest session, or return from it

\\<+agent-acp-tabs-mode-map>In the chat
  \\[+agent-acp-focus-input]\tjump to the input
  \\[+agent-acp-hide]\thide this view; the agent keeps working
  \\[+agent-acp-next-session] / \\[+agent-acp-previous-session]\tnext / previous tab
  \\[agent-shell-interrupt]\tstop the current turn or Run; queued prompts are held
  \\[+agent/manager]\tAgent Manager
  \\[+agent-session-menu], or ? outside the input\tevery action

At the input, type
"
       (mapconcat (lambda (entry) (format "  /%s\t%s\n" (car entry) (cddr entry)))
                  (+agent-actions-slash-commands) "")
       "
C-c A plus a letter runs the same action from anywhere; c leads to sending
context.  Mouse: mouse-1 shows a tab, mouse-3 opens its actions.")))))

(defun +agent-acp-help-or-insert ()
  "Insert `?' in the prompt input; elsewhere open session actions."
  (interactive)
  (let ((start (+agent-acp--input-start (current-buffer))))
    (if (and start (>= (point) start))
        (call-interactively #'self-insert-command)
      (call-interactively #'+agent-session-menu))))

(defun +agent-acp--workspace-window (&optional frame)
  "Return FRAME's one Agent window, if it is live."
  (seq-find (lambda (window)
              (and (window-parameter window 'agent-workspace)
                   (+agent-acp-agent-buffer-p (window-buffer window))))
            (window-list (or frame (selected-frame)) 'nomini)))

(defun +agent-acp--display-workspace-buffer (buffer)
  "Display BUFFER through the window-owned Agent view controller."
  (+agent-view-show buffer))

(defun +agent-acp--retire-transcript-workspace ()
  "Replace transcript-copy Agent Workspace buffers left by an older load.
Every live agent buffer becomes a tab, and each window that showed an old
workspace shows a real agent buffer of the same project instead."
  (dolist (buffer (buffer-list))
    (when (+agent-acp-agent-buffer-p buffer)
      (+agent-acp--install-workspace-tabs buffer)))
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*Noema Agent Workspace · " (buffer-name buffer))
      (let* ((root (ignore-errors
                     (buffer-local-value '+agent-acp-workspace-root buffer)))
             (timer (ignore-errors
                      (buffer-local-value '+agent-acp-workspace-refresh-timer buffer)))
             (agent (car (+agent-acp--project-buffers root))))
        (when (timerp timer)
          (cancel-timer timer))
        (with-current-buffer buffer
          (remove-hook 'kill-buffer-hook '+agent-acp--workspace-killed t))
        (dolist (window (get-buffer-window-list buffer nil t))
          (if agent
              (set-window-buffer window agent)
            (ignore-errors (delete-window window))))
        (kill-buffer buffer)))))

(defun +agent-acp-touch (buffer)
  "Record recent use of Noema agent BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local +agent-acp-last-used-at (float-time)))))

(defun +agent-acp-trim-buffer (buffer max-bytes)
  "Trim durable old output in BUFFER to approximately MAX-BYTES.

Call this only after the corresponding Run output is durable.  The current
input prompt and its editable tail are retained, and point stays with its
text, so a hidden session is not reopened at the top of what remains."
  (when (and (buffer-live-p buffer) (> max-bytes 0))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (let ((excess (- (position-bytes (point-max)) (position-bytes (point-min))
                         max-bytes)))
          (when (> excess 0)
            (save-excursion
              (goto-char (byte-to-position (+ (position-bytes (point-min)) excess)))
              (forward-line 1)
              (let ((inhibit-read-only t)
                    (buffer-undo-list t)
                    (prompt (car-safe (bound-and-true-p comint-last-prompt))))
                (delete-region (point-min)
                               (if (markerp prompt)
                                   (min (point) (marker-position prompt))
                                 (point)))))))))))

(defun +agent-acp-mark-session-buffer (buffer name agent directory)
  "Register the original agent-shell BUFFER as NAME in DIRECTORY.
The manager, dock and domain callers all reuse this same frontend and native
transcript.  A previous physical generation relinquishes NAME so lookup stays
unambiguous; its work, draft and queued prompts retain their own lifetime."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (+agent-acp--install-workspace-tabs buffer)
      (+agent-acp-touch buffer)
      (+agent-acp-hide-client-stderr buffer)
      (+agent-acp--watch-attention buffer)
      (when directory
        (setq-local +agent-acp-session-root
                    (file-name-as-directory (expand-file-name directory))))
      (when (and (stringp name) (not (string-empty-p name)))
        (let ((key (+agent-acp--buffer-pool-key buffer)))
          (dolist (other (buffer-list))
            (when (and (not (eq other buffer))
                       (+agent-acp-agent-buffer-p other)
                       (eq (+agent-acp--management-domain buffer) (+agent-acp--management-domain other))
                       (equal (buffer-local-value '+agent-acp-session-name other) name)
                       (equal (+agent-acp--buffer-pool-key other) key))
              (with-current-buffer other
                (setq-local +agent-acp-session-name nil +agent-acp-retired t)
                (run-hook-with-args '+agent-session-forget-functions other)
                (unless (string-suffix-p " (retired)" (buffer-name))
                  (rename-buffer (concat (buffer-name) " (retired)") t))
                (+agent-acp--sync-shell-buffer-name other))
              ;; An idle retired buffer only duplicates a tab; its conversation
              ;; stays in the Run history.  A busy one stays until it finishes.
              (+agent-acp--schedule-retired other))))
        (setq-local +agent-acp-session-name name +agent-acp-retired nil
                    +agent-acp-session-agent agent)
        (let ((wanted (format "*Agent · %s · %s @ %s*"
                              name (or agent "agent")
                              (file-name-nondirectory
                               (directory-file-name (or +agent-acp-session-root
                                                        default-directory))))))
          (unless (string-prefix-p wanted (buffer-name))
            (rename-buffer wanted t)))
        (+agent-acp--sync-shell-buffer-name buffer)))
    (+agent-acp--refresh-tabs)
    buffer))

(defun +agent-acp-session-buffer (name root &optional context)
  "Find execution session NAME in ROOT, or CONTEXT's explicit management domain."
  (let ((key (and root (+agent-acp--buffer-pool-key (or context (current-buffer)) root))))
    (seq-find (lambda (buffer)
                (and (+agent-acp-agent-buffer-p buffer)
                     (eq (+agent-acp--management-domain buffer)
                         (if context (+agent-acp--management-domain context) 'agent))
                     (equal (buffer-local-value '+agent-acp-session-name buffer) name)
                     (or (null key) (equal (+agent-acp--buffer-pool-key buffer) key))))
              (buffer-list))))


;;; ── One project-wide registry of live agent sessions ──────────────────────
;;
;; A `.noema' Run, the popup pool, `+agent/start' and a bare
;; `M-x agent-shell' all produce the same kind of resource, so they are all
;; registered the same way: one project root, one session name, one origin.
;; Buffer-local identity stays the source of truth — this adds the single
;; entry point and the merged view over it.  Registration only ever reads the
;; project: opening an agent never creates a `noema.toml'.

(defgroup +agent-session nil
  "Shared project-wide registry of live agent sessions."
  :group 'applications)

(defcustom +agent-acp-adopt-foreign-sessions t
  "Whether agent-shell buffers Noema did not start join its session registry.
When non-nil, a bare `M-x agent-shell' session is given a project root and a
session name like every other session, so one view lists them all.  Set this
to nil to leave such buffers exactly as upstream created them."
  :type 'boolean
  :group '+agent-session)

(declare-function projectile-project-root "projectile" (&optional directory))


(defun +agent-acp-project-root (&optional directory)
  "Query the registered project scope of DIRECTORY, without creating one."
  (let ((directory (file-name-as-directory (expand-file-name (or directory default-directory)))))
    (+agent-acp--workspace-root
     (or (run-hook-with-args-until-success '+agent-project-root-functions directory)
         (and (fboundp 'projectile-project-root) (ignore-errors (projectile-project-root directory)))
         (when-let* ((project (project-current nil directory))) (project-root project))
         directory))))

(defun +agent-acp--claimed-names (root)
  "Return the session names live agent buffers of ROOT already hold."
  (delq nil (mapcar (lambda (buffer)
                      (and (+agent-acp-agent-buffer-p buffer)
                           (equal (buffer-local-value '+agent-acp-session-root buffer)
                                  root)
                           (eq (+agent-session-kind buffer) 'agent)
                           (buffer-local-value '+agent-acp-session-name buffer)))
                    (buffer-list))))

(defun +agent-acp--unique-name (base root)
  "Return BASE, or BASE plus a counter, unclaimed by an execution session."
  (let ((taken (+agent-acp--claimed-names root))
        (candidate base)
        (index 1))
    (while (member candidate taken)
      (setq index (1+ index)
            candidate (format "%s-%d" base index)))
    candidate))

(defun +agent-acp--buffer-agent-id (buffer)
  "Return the agent id agent-shell configured BUFFER with, as a string."
  (let ((identifier (+agent-acp-state-value buffer '(:agent-config :identifier))))
    (cond ((stringp identifier) identifier)
          ((and identifier (symbolp identifier)) (symbol-name identifier)))))



(cl-defun +agent-acp-adopt (buffer &key agent origin name root)
  "Register agent BUFFER as a named session of its project, and return it.
ORIGIN records which entry point started it.  NAME defaults to a unique local
name built from ORIGIN and the agent id; ROOT defaults to
`+agent-acp-project-root' of the buffer's own directory.

Optional domain adapters associate their business identity after registration;
the runtime does not start a document host or create a business project."
  (when (+agent-acp-agent-buffer-p buffer)
    (let* ((origin (or origin
                       (buffer-local-value '+agent-acp-session-origin buffer)
                       'manual))
           (root (+agent-acp--workspace-root
                  (or root
                      (+agent-acp-project-root
                       (or (buffer-local-value '+agent-acp-session-root buffer)
                           (buffer-local-value 'default-directory buffer))))))
           (agent (or (and agent (format "%s" agent))
                      (buffer-local-value '+agent-acp-session-agent buffer)
                      (+agent-acp--buffer-agent-id buffer)
                      "agent"))
           (name (or name
                     (buffer-local-value '+agent-acp-session-name buffer)
                     (+agent-acp--unique-name (format "%s/%s" origin agent) root))))
      (with-current-buffer buffer
        (setq-local +agent-acp-session-origin origin))
      (+agent-acp-mark-session-buffer buffer name agent root)
      (unless (memq origin +agent-acp-ephemeral-origins)
        (run-hook-with-args '+agent-session-adopt-functions buffer name root))
      buffer)))

(defun +agent-acp-sessions (&optional root)
  "Return the shared snapshots of every agent buffer, optionally under ROOT."
  (let ((root (and root (+agent-acp--workspace-root root))))
    (delq nil
          (mapcar (lambda (buffer)
                    (when (and (+agent-acp-agent-buffer-p buffer)
                               (or (not root) (equal root (buffer-local-value '+agent-acp-session-root buffer))))
                      (+agent-session-snapshot buffer)))
                  (buffer-list)))))

(defun +agent-pool-sessions (&optional root target kind)
  "Read ROOT's public execution pool, or every execution pool when ROOT is nil.
KIND defaults to agent; pi queries managed Pi roles, all includes both.
TARGET defaults to the invoking execution context.  This queries one registry."
  (let* ((target (or target (and (fboundp 'remote-file-name-target)
                                (remote-file-name-target default-directory))))
         (key (and root (+agent-pool-key (list :root root :target target)))))
    (seq-filter
     (lambda (session)
       (and (or (not key) (equal key (+agent-pool-key session)))
            (or (eq kind 'all) (eq (or kind 'agent) (+agent-session-kind session)))
            (not (run-hook-with-args-until-success '+agent-session-private-functions session))))
     (+agent-acp-sessions))))

(defun +agent-acp--last-prompt ()
  "Return the first line of the current buffer's latest submitted prompt."
  (when (and (bound-and-true-p comint-input-ring)
             (not (ring-empty-p comint-input-ring)))
    (car (split-string (substring-no-properties (ring-ref comint-input-ring 0))
                       "\n" t "[ \t]+"))))

(defun +agent-session-snapshot (buffer)
  "Describe one real agent BUFFER without a domain service or disk scan."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer
      (let* ((state (+agent-acp--state buffer))
             (process (map-nested-elt state '(:client :process)))
             (directory default-directory)
             (live (not (+agent-acp-client-exited-p buffer)))
             (permission (+agent-acp--permission-request state))
             (elicitation (+agent-acp--pending-elicitation state))
             (attention (or (run-hook-with-args-until-success '+agent-session-attention-functions buffer)
                            (and live permission 'permission)
                            (and live elicitation 'input)
                            (and +agent-acp-last-error 'failed))))
        (list :buffer buffer :name +agent-acp-session-name :agent +agent-acp-session-agent
              :provider (map-nested-elt state '(:agent-config :identifier))
              :origin +agent-acp-session-origin :root +agent-acp-session-root
              :group (+agent-session-group buffer) :retired (+agent-acp-retired-p buffer)
              :target (and (fboundp 'remote-file-name-target) (remote-file-name-target directory))
              :workspace (if (fboundp 'remote-canonicalize-emacs-file-name)
                             (remote-canonicalize-emacs-file-name directory) directory)
              :native-session-id (map-nested-elt state '(:session :id))
              :session-id (run-hook-with-args-until-success '+agent-session-id-functions buffer)
              :process-state (cond ((and (not process) (map-nested-elt state '(:session :id))) 'disconnected)
                                   ((not process) 'starting)
                                   ((and (processp process) (process-live-p process)) 'live)
                                   (t 'dead))
              :turn-state (cond ((not live) (if +agent-acp--exit-interrupted 'interrupted 'idle))
                                (+agent-acp--stop-unconfirmed 'stop-unconfirmed)
                                (+agent-acp--stop-token 'stopping)
                                ((not (eq (map-elt state :init-finished) t)) 'starting)
                                ((+agent-acp-busy-p buffer) 'running)
                                (t 'idle))
              :busy (and (+agent-acp-busy-p buffer) t)
              :attention-kind attention :unread-kind +agent-acp-attention :last-error +agent-acp-last-error :usage (+agent-acp-usage buffer)
              :title (map-nested-elt state '(:session :title))
              :last-prompt (+agent-acp--last-prompt)
              :request (and live (or (map-elt permission :title)
                                     (map-elt elicitation :message)))
              :turn-started-at +agent-acp--turn-started-at
              :stop-reason +agent-acp--stop-reason
              :queued (+agent-acp-pending-prompt-count buffer)
              :queue-held (+agent-acp-held-prompt-count buffer)
              :changes (length +agent-acp--changes)
              :capabilities (list :list (map-elt state :supports-session-list)
                                  :load (map-elt state :supports-session-load)
                                  :resume (map-elt state :supports-session-resume)
                                  :fork (map-elt state :supports-session-fork)
                                  :steer (map-elt state :supports-steering))
              :last-used +agent-acp-last-used-at)))))

(defun +agent-session-capability-p (buffer capability)
  "Whether BUFFER positively reports CAPABILITY in its snapshot."
  (let ((value (plist-get (plist-get (+agent-session-snapshot buffer) :capabilities) capability)))
    (and (not (memq value '(nil :false :null))) t)))

(defun +agent-session-operation-supported-p (operation buffer)
  "Whether native or domain capabilities explicitly support OPERATION."
  (or (and (eq operation 'fork) (+agent-session-capability-p buffer :fork))
      (run-hook-with-args-until-success '+agent-session-operation-functions operation buffer)))

(defun +agent-acp--adopt-foreign-h ()
  "Register an agent-shell session Noema itself did not start.
Every Noema entry point sets `+agent-acp-session-origin' as it starts the
buffer, so a buffer without one came from upstream.  The work waits one
command, because upstream returns the buffer before its caller can name it."
  (when +agent-acp-adopt-foreign-sessions
    (let ((buffer (current-buffer)))
      (when (timerp +agent-acp--adopt-timer) (cancel-timer +agent-acp--adopt-timer))
      (setq +agent-acp--adopt-timer
       (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (setq +agent-acp--adopt-timer nil)))
         (when (and (buffer-live-p buffer)
                    (+agent-acp-agent-buffer-p buffer)
                    (not (buffer-local-value '+agent-acp-session-origin buffer))
                    (not (buffer-local-value '+agent-acp-session-name buffer)))
           (condition-case error-object
               (+agent-acp-adopt buffer :origin 'foreign)
             (error (message "Noema could not register this agent session: %s"
                             (error-message-string error-object)))))))))))

(defvar-local +agent-acp--adopt-timer nil)
(defvar-local +agent-acp--retire-timer nil)

(defun +agent-acp--clear-deferred-h ()
  "Release this physical buffer's registration and retirement callbacks."
  (dolist (timer (list +agent-acp--adopt-timer +agent-acp--retire-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq +agent-acp--adopt-timer nil +agent-acp--retire-timer nil))

(defun +agent-acp--schedule-retired (buffer)
  "Recheck retirement when BUFFER is still idle, hidden and unnamed."
  (with-current-buffer buffer
    (when (timerp +agent-acp--retire-timer) (cancel-timer +agent-acp--retire-timer))
    (setq +agent-acp--retire-timer
          (run-at-time
           0 nil
           (lambda ()
             (when (+agent-acp-agent-buffer-p buffer)
               (with-current-buffer buffer
                 (setq +agent-acp--retire-timer nil)
                 (when (and (not +agent-acp-session-name)
                            (eq (+agent-acp-state-value buffer '(:init-finished)) t)
                            (+agent-acp-resumable-p buffer)
                            (not (+agent-acp-busy-p buffer))
                            (zerop (+agent-acp-pending-prompt-count buffer))
                            (not (+agent-acp--unsent-input-p buffer))
                            (not (get-buffer-window buffer t)))
                   (+agent-acp-kill buffer)))))))))

(defcustom +agent-acp-warm-buffer-limit 8
  "Maximum idle resumable background clients retained per target/workspace."
  :type 'natnum :group '+agent-session)

(defcustom +agent-acp-warm-idle-seconds (* 30 60)
  "Seconds an unused resumable background client remains warm."
  :type 'natnum :group '+agent-session)

(defvar +agent-acp--warm-sweep-timer nil)

(defun +agent-acp-resumable-p (buffer)
  "Whether BUFFER has a native identity and positively reports restoration."
  (let ((id (+agent-acp-state-value buffer '(:session :id))))
    (and (stringp id) (not (string-empty-p id))
         (cl-some (lambda (path)
                    (not (memq (+agent-acp-state-value buffer path) '(nil :null :false))))
                  '((:supports-session-load) (:supports-session-resume))))))

(defun +agent-acp--retention-origin-p (buffer)
  "Whether BUFFER's origin permits automatic warm retention."
  (and (buffer-live-p buffer)
       (let ((origin (buffer-local-value '+agent-acp-session-origin buffer)))
         (and (not (memq origin '(pi pi-emacs pi-project)))
              (memq origin +agent-acp-auto-stop-origins)))))

(defun +agent-acp-sweep-warm-buffers (&optional limit idle-seconds)
  "Hibernate unused recoverable background clients through their shared owner.
+agent-acp-auto-stoppable-p protects work, drafts, queues and visible clients.
LIMIT and IDLE-SECONDS default to the shared settings.  No history is removed."
  (interactive)
  (let ((groups (make-hash-table :test #'equal)) (now (float-time)))
    (dolist (buffer (buffer-list))
      (when (and (+agent-acp-agent-buffer-p buffer)
                 (not (eq (buffer-local-value '+agent-acp-session-origin buffer) 'pi))
                 (+agent-acp-resumable-p buffer) (+agent-acp-auto-stoppable-p buffer))
        (let ((snapshot (+agent-session-snapshot buffer)))
          (push buffer (gethash (list (plist-get snapshot :target) (plist-get snapshot :root)) groups)))))
    (maphash
     (lambda (_group buffers)
       (let ((ordered (sort buffers (lambda (a b)
                                     (> (or (buffer-local-value '+agent-acp-last-used-at a) 0)
                                        (or (buffer-local-value '+agent-acp-last-used-at b) 0))))))
         (cl-loop for buffer in ordered for index from 0
                  for last = (or (buffer-local-value '+agent-acp-last-used-at buffer) 0)
                  when (and (or (>= index (or limit +agent-acp-warm-buffer-limit))
                                (> (- now last) (or idle-seconds +agent-acp-warm-idle-seconds)))
                            ;; A prior shutdown may run native event callbacks.
                            (+agent-acp-auto-stoppable-p buffer) (+agent-acp-resumable-p buffer))
                  do (+agent-acp-kill buffer))))
     groups)))

(defun +agent-acp--cancel-warm-sweep ()
  (when (timerp +agent-acp--warm-sweep-timer) (cancel-timer +agent-acp--warm-sweep-timer))
  (setq +agent-acp--warm-sweep-timer nil))

(defun +agent-acp--sync-warm-sweep (&optional source exclude)
  "Schedule one sweep only while a background physical client exists."
  (cond
   ;; A Pi or personal-session update cannot create or remove an eligible
   ;; client.  In particular, never scan all editor buffers for those events.
   ((and source (not exclude) (not (+agent-acp--retention-origin-p source))) nil)
   ((or (and source (not exclude)
             (let ((process (+agent-acp-state-value source '(:client :process))))
               (and (processp process) (process-live-p process))))
        (seq-some
            (lambda (buffer)
              (and (not (eq buffer exclude)) (+agent-acp--retention-origin-p buffer)
                   (+agent-acp-agent-buffer-p buffer)
                   (let ((process (+agent-acp-state-value buffer '(:client :process))))
                     (and (processp process) (process-live-p process)))))
            (buffer-list)))
    (unless (timerp +agent-acp--warm-sweep-timer)
      (setq +agent-acp--warm-sweep-timer
            (run-at-time 300 nil #'+agent-acp--warm-sweep-tick))))
   (t (+agent-acp--cancel-warm-sweep))))

(defun +agent-acp--warm-sweep-tick ()
  (setq +agent-acp--warm-sweep-timer nil)
  (unwind-protect (+agent-acp-sweep-warm-buffers) (+agent-acp--sync-warm-sweep)))

(defun +agent-acp--retention-killed-h ()
  (+agent-acp--sync-warm-sweep nil (current-buffer)))

(defun +agent-acp--install-retention-h ()
  (add-hook 'kill-buffer-hook #'+agent-acp--retention-killed-h 100 t))

(add-hook '+agent-acp-changed-functions #'+agent-acp--sync-warm-sweep)
(add-hook 'agent-shell-mode-hook #'+agent-acp--install-retention-h)
(add-hook 'kill-emacs-hook #'+agent-acp--cancel-warm-sweep)
(dolist (buffer (buffer-list))
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer (+agent-acp--install-retention-h))))
(+agent-acp--sync-warm-sweep)

(add-hook 'agent-shell-mode-hook #'+agent-acp--adopt-foreign-h)

(defun +agent-acp-resolve-config (identifier)
  "Resolve embedded agent-shell configuration IDENTIFIER."
  (copy-tree (agent-shell--resolve-config-designator identifier)))

(defun +agent-acp-config-for (agent)
  "Resolve Noema AGENT id to an agent-shell configuration, or nil."
  (let ((identifier (pcase (downcase (format "%s" (or agent "codex")))
                      ((or "claude" "claude-code") 'claude-code)
                      ((or "open-code" "opencode") 'opencode)
                      (name (intern name)))))
    (ignore-errors (+agent-acp-resolve-config identifier))))

(defvar agent-shell-pi-environment)
(declare-function agent-shell-pi-make-client "agent-shell-pi" (&rest args))

(defun +agent-acp-pi-config (environment)
  "Return a Pi agent-shell configuration whose client runs with ENVIRONMENT.
ENVIRONMENT is a list of \"NAME=VALUE\" strings.  They are added every time
agent-shell builds the client, so a restart or resume keeps them (D-035).
Return nil when Pi is not available."
  (require 'agent-shell-pi nil t)
  (when-let* ((config (+agent-acp-config-for "pi"))
              ((fboundp 'agent-shell-pi-make-client)))
    (setf (alist-get :client-maker config)
          (lambda (buffer)
            (let ((agent-shell-pi-environment
                   (append environment (bound-and-true-p agent-shell-pi-environment))))
              (agent-shell-pi-make-client :buffer buffer))))
    config))

(defun +agent-acp-known-agents ()
  "Return agent ids for work-directive completion.
Resolution remains inside this adapter so JuText never depends on
agent-shell's private configuration representation."
  (let ((candidates '(("codex" . codex)
                      ("claude" . claude-code)
                      ("opencode" . open-code)
                      ("pi" . pi)))
        available)
    (dolist (candidate candidates (or (nreverse available)
                                      (mapcar #'car candidates)))
      (when (ignore-errors
              (+agent-acp-resolve-config (cdr candidate)))
        (push (car candidate) available)))))

(defun +agent-acp-hide-client-stderr (buffer)
  "Hide the stderr buffer of agent BUFFER's ACP client process.
acp.el names it `acp-client-stderr(...)' without a leading space, so it
would appear in buffer lists beside the hidden D-033 agent buffers.  Emacs
attaches that buffer to a pipe process named after the client process.
Renaming keeps acp.el's cleanup working, since it holds the buffer object.
Return the stderr buffer when it was renamed."
  (when-let* (((+agent-acp-agent-buffer-p buffer))
              (process (map-elt (map-elt (+agent-acp--state buffer) :client) :process))
              ((processp process))
              (pipe (get-process (concat (process-name process) " stderr")))
              (stderr (process-buffer pipe))
              ((buffer-live-p stderr))
              ((not (string-prefix-p " " (buffer-name stderr)))))
    (with-current-buffer stderr
      (rename-buffer (concat " " (buffer-name)) t))
    stderr))

(defvar +agent-acp-process-directory-function nil
  "Function mapping a session directory to its agent process directory.
It receives an expanded directory and returns the directory the agent process
starts in, or nil when no process can run there.  A host with its own file
routing, such as a logical /fs: layer, sets this so Noema and a bare
`agent-shell' agree on placement.  nil uses Emacs's own file handlers.")

(defun +agent-acp--default-process-directory (directory)
  "Return DIRECTORY for an agent process through Emacs file handlers.
A logical /fs: name becomes its client-accessible spelling when there is one;
a remote name stays, and `make-process' routes the agent to that host."
  (let ((client (and (string-prefix-p "/fs:" directory)
                     (fboundp 'remote-client-file-name)
                     (remote-client-file-name directory))))
    (cond (client client)
          ((file-remote-p directory) directory) ; aaron-allow: file-remote-p — standalone Emacs handler fallback
          ((not (string-prefix-p "/fs:" directory)) directory))))

(defvar +agent-acp-agent-file-function nil
  "Function mapping (FILE SESSION) to the path the SESSION's agent opens.
FILE is an expanded Emacs file name; SESSION is an agent buffer or nil for an
agent on this machine.  Return nil when that agent's machine cannot reach
FILE.  A host with its own file routing sets this alongside
`+agent-acp-process-directory-function'.  nil uses Emacs's own handlers.")

(defun +agent-acp--default-agent-file (file session)
  "Return FILE as SESSION's agent sees it, through Emacs file handlers."
  (let ((agent-host (and session
                         (file-remote-p ; aaron-allow: file-remote-p — standalone Emacs handler fallback
                          (buffer-local-value 'default-directory session))))
        (client (if (string-prefix-p "/fs:" file)
                    (and (fboundp 'remote-client-file-name)
                         (remote-client-file-name file))
                  file)))
    (cond ((and agent-host (equal agent-host (file-remote-p file))) ; aaron-allow: file-remote-p — standalone Emacs handler fallback
           (file-local-name file))
          ((and (not agent-host) client (not (file-remote-p client)) ; aaron-allow: file-remote-p — standalone Emacs handler fallback
                (not (string-prefix-p "/fs:" client)))
           client))))

(defun +agent-acp-agent-file (file &optional session)
  "Return FILE as the path the agent of SESSION opens, or nil.
SESSION nil means an agent running on this machine.  nil means the agent's
machine cannot reach FILE, such as a local file for an agent on a server."
  (funcall (or +agent-acp-agent-file-function
               #'+agent-acp--default-agent-file)
           (expand-file-name file) session))

(defun +agent-acp--client-directory (directory)
  "Return the directory an agent process for DIRECTORY starts in.
Never hand an unresolved identity to a process: signal when neither this
machine nor a routed host can run the agent there."
  (let* ((expanded (expand-file-name directory))
         (process (funcall (or +agent-acp-process-directory-function
                               #'+agent-acp--default-process-directory)
                           expanded)))
    (unless process
      (user-error "No machine can run an agent in: %s" directory))
    (unless (condition-case error-object
                (file-directory-p process)
              (error (user-error "Agent directory is unreachable: %s (%s)"
                                 process (error-message-string error-object))))
      (user-error "Agent directory does not exist: %s" process))
    (file-name-as-directory process)))

(defun +agent-acp-restart-native (buffer &optional requested-session-id)
  "Restart BUFFER's client while retaining conversation and unsent intent.
Use upstream restart and its native kill queries.  Active requests are never
replayed.  A failed start retains unsent text in an explicit recovery buffer."
  (+agent-view-exit-preview)
  (let* ((+window-buffer-replacement buffer)
         (+agent-view-replacement-windows
          (seq-remove (lambda (window) (window-parameter window 'aaron-ui-preview))
                      (get-buffer-window-list buffer nil t)))
         ;; Native kill normally restores a popup by deleting its side window.
         ;; A physical replacement owns that window through the whole swap.
         (kill-buffer-quit-windows nil)
         (window-deletable-functions (cons #'+agent-view--replacement-deletable-p
                                            window-deletable-functions))
         (+agent-view-request (+agent-view-capture buffer))
         (views (mapcar (lambda (window)
                          (with-selected-window window (+agent-view-capture buffer)))
                        +agent-view-replacement-windows))
         (name (buffer-name buffer))
         (session-id (or requested-session-id (+agent-acp-state-value buffer '(:session :id))))
         (origin (buffer-local-value '+agent-acp-session-origin buffer))
         (session-name (buffer-local-value '+agent-acp-session-name buffer))
         (agent (buffer-local-value '+agent-acp-session-agent buffer))
         (root (buffer-local-value '+agent-acp-session-root buffer))
         (display (buffer-local-value '+agent-acp-display-buffer-function buffer))
         (popup-properties
          (when (and (boundp '+portal-popup-instance-p)
                     (buffer-local-value '+portal-popup-instance-p buffer))
            (mapcar (lambda (variable) (cons variable (buffer-local-value variable buffer)))
                    '(+portal-popup-workspace-id +portal-popup-title +portal-popup-kind +portal-popup-fixed))))
         (origin-window (buffer-local-value '+agent-acp--origin-window buffer))
         (policy (buffer-local-value '+agent-render-policy buffer))
         (strict-restore (buffer-local-value '+agent-acp-strict-restore buffer))
         (process-environment (copy-sequence (buffer-local-value 'process-environment buffer)))
         (exec-path (copy-sequence (buffer-local-value 'exec-path buffer)))
         (draft (with-current-buffer buffer (ignore-errors (agent-shell--prompt-input))))
         ;; A restart is not a go-ahead: everything queued waits for one (C37).
         (pending (append (buffer-local-value '+agent-acp--held-prompts buffer)
                          (copy-tree (+agent-acp-state-value buffer '(:pending-prompts))))))
    (condition-case err
        (progn
          (with-current-buffer buffer
            ;; Exited clients may retain a request which never received a
            ;; response.  Preserve its unknown outcome; do not replay it.
            (let ((process (+agent-acp-state-value buffer '(:client :process))))
              (unless (and (processp process) (process-live-p process))
                (setf (map-elt agent-shell--state :active-requests) nil)))
            (agent-shell-restart :session-id session-id))
          (when-let* ((replacement (get-buffer name)))
            (with-current-buffer replacement
              (dolist (property popup-properties)
                (set (make-local-variable (car property)) (cdr property)))
              (setq-local +agent-acp-session-origin origin
                          +agent-acp-display-buffer-function display
                          +agent-acp--origin-window origin-window
                          +agent-render-policy policy
                          +agent-acp-strict-restore strict-restore)
              ;; No active request is copied; the queue resumes explicitly.
              (setq +agent-acp--held-prompts pending))
            (when session-name
              (+agent-acp-adopt replacement :origin origin :name session-name :agent agent :root root))
            (when (and (stringp draft) (not (string-empty-p draft)))
              (agent-shell-insert :text draft :submit nil :no-focus t :shell-buffer replacement))
            ;; Upstream replaces surviving windows directly.  Restore their
            ;; return records too, and recreate a popup deleted by its native
            ;; kill hook.  Presentation requests expire with this command.
            (dolist (view views)
              (when (frame-live-p (plist-get view :frame))
                (let ((+agent-view-request view))
                  (save-selected-window (+agent-acp-show-buffer replacement)))))
            (+agent-acp-show-buffer replacement)
            (with-current-buffer replacement (+agent-acp--changed))
            replacement))
      (error
       (unless (buffer-live-p buffer)
         (dolist (view views) (+agent-view-abandon view buffer))
         (when-let* ((origin (plist-get +agent-view-request :origin))
                     ((window-live-p origin)))
           (select-window origin))
         (when (or (and (stringp draft) (not (string-empty-p draft))) pending)
           (with-current-buffer (get-buffer-create "*Agent recovery*")
             (goto-char (point-max))
             (insert (format "\n%s · %s\nUnsent draft:\n%s\nQueued prompts:\n%s\n"
                             (or session-name name) (or root "") (or draft "")
                             (mapconcat (lambda (prompt) (format "%s" prompt)) pending "\n\n")))
             (display-buffer (current-buffer)))))
       (signal (car err) (cdr err))))))

(cl-defun +agent-acp-start (&key config directory session-id fork-session-id focus
                                      (origin 'manual) (render-policy 'visible)
                                      display-function strict-restore choose-saved)
  "Start CONFIG in DIRECTORY, optionally resuming or forking a native session.
The buffer is displayed only when FOCUS is non-nil (D-033): a document Run
never pops its agent buffer; the person opens it on purpose.

ORIGIN records which entry point owns the session, so the project-wide
registry can list it and `+agent-acp--adopt-foreign-h' leaves it alone.
CHOOSE-SAVED asks which provider-listed conversation to restore instead of
opening a new one; cancelling ends the client.

Noema disables agent-shell's duplicate Markdown transcript and defaults to a
visible-only render policy.  Neither setting affects ACP transport or events."
  (let* ((directory (+agent-acp--client-directory directory))
        (default-directory directory)
        ;; Project discovery can return a logical /fs: root again. Pin the
        ;; actual process directory through the asynchronous ACP handshake.
        (agent-shell-cwd-function (lambda () directory))
        (agent-shell-transcript-file-path-function nil)
        (+agent-render-policy render-policy))
    ;; An ACP session already executing in this Emacs is an attachment, not
    ;; another client to initialize or subscribe.  The shared lookup includes
    ;; both the agent and execution target/workspace.
    (if-let* ((existing (and session-id
                             (eq (map-elt config :identifier) 'codex)
                             (fboundp '+agent-shell--find-execution)
                             (+agent-shell--find-execution
                              session-id config directory))))
        (progn
          (when focus (+agent-acp-show-buffer existing))
          existing)
      (let ((buffer
             (agent-shell--start :config config :no-focus t :new-session t
                                 :session-strategy 'new :session-id session-id
                                 :fork-session-id fork-session-id)))
        (with-current-buffer buffer
          (setq-local agent-shell-transcript-file-path-function nil
                      agent-shell-cwd-function (lambda () directory)
                      agent-shell--transcript-file nil
                      ;; Interrupting a Noema session stops at once; the Run
                      ;; record and the conversation both survive it.
                      agent-shell-confirm-interrupt nil
                      +agent-render-policy render-policy
                      +agent-acp-strict-restore (or strict-restore choose-saved)
                      +agent-acp--choose-saved choose-saved
                      +agent-acp-session-origin origin
                      +agent-acp-display-buffer-function display-function
                      +agent-acp-session-root
                      (file-name-as-directory (expand-file-name directory)))
          (unless display-function
            (+agent-acp--install-workspace-tabs buffer)))
        ;; acp.el starts the client process for the asynchronous handshake.
        (+agent-acp-subscribe
         :buffer buffer :event 'init-handshake
         :callback (lambda (_event) (+agent-acp-hide-client-stderr buffer)))
        (+agent-acp-hide-client-stderr buffer)
        (when focus
          (+agent-acp-show-buffer buffer))
        buffer))))

(cl-defun +agent-acp-subscribe (&key buffer event callback)
  "Subscribe CALLBACK to EVENT for this exact physical BUFFER state."
  (let ((state (+agent-acp--state buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event event
     :on-event (lambda (payload)
                 (when (and (buffer-live-p buffer) (eq state (+agent-acp--state buffer)))
                   (with-current-buffer buffer (funcall callback payload)))))))

(cl-defun +agent-acp-unsubscribe (&key buffer subscription)
  "Remove SUBSCRIPTION from agent-shell BUFFER when both are still live."
  (when (and subscription (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell-unsubscribe :subscription subscription)))))

(defvar +agent-acp--shutting-down nil
  "Non-nil while this Emacs itself shuts a client down; that is no failure.")

(defun +agent-acp--shutdown-a (original &rest args)
  "Mark ORIGINAL's process exit as deliberate.
`delete-process' can run the sentinel before upstream forgets the client."
  (let ((+agent-acp--shutting-down t) (buffer (current-buffer)))
    (unwind-protect (apply original args)
      (when (buffer-live-p buffer)
        (run-hook-with-args '+agent-acp-client-ended-functions buffer)
        (+agent-acp--sync-warm-sweep buffer)))))
(advice-add 'agent-shell--shutdown :around #'+agent-acp--shutdown-a)

(defun +agent-acp--client-exited (buffer state process event interrupted)
  "Report that PROCESS, the client of STATE in BUFFER, ended with EVENT.
Nothing else tells a person: a hidden, idle session otherwise stays listed as
ready until the next prompt fails.  See C38."
  (when (and (not +agent-acp--shutting-down)
             (not (process-live-p process))
             (+agent-acp-agent-buffer-p buffer)
             (eq state (+agent-acp--state buffer))
             (eq process (map-nested-elt state '(:client :process)))
             (not (and (boundp 'remote-buffer-disconnected-p)
                       (buffer-local-value 'remote-buffer-disconnected-p buffer))))
    (with-current-buffer buffer
      (setq-local +agent-acp-last-error
                  (format "Agent process ended: %s" (string-trim (or event "")))
                  +agent-acp--exit-interrupted interrupted
                  +agent-acp--turn-started-at nil)
      (+agent-acp--clear-stop)
      (+agent-acp--hold-queue)
      (run-hook-with-args '+agent-acp-client-ended-functions buffer)
      (condition-case nil
          (agent-shell--update-fragment
           :state state :block-id "client-exited"
           :label-left "Agent process ended"
           :body (concat (if interrupted
                             "A turn was running, so its result is unknown and it will not be replayed. "
                           "")
                         "Submit input or use C-c C-r to reconnect this conversation.")
           :create-new t :navigation 'never
           :above-last-prompt (not (shell-maker-busy)))
        (error nil))
      (+agent-acp--changed))
    (+agent-acp--note-attention buffer 'failed)))

(defvar-local +agent-acp--watched-process nil)

(defun +agent-acp--watch-process ()
  "Watch the current buffer's ACP client process for exit, once per process.
acp.el starts it at the first request, so every session event checks."
  (let ((process (map-nested-elt agent-shell--state '(:client :process))))
    (when (and (processp process)
               (not (eq process +agent-acp--watched-process))
               (process-live-p process))
      (setq +agent-acp--watched-process process)
      (let ((buffer (current-buffer)) (state agent-shell--state))
        (add-function
         :around (process-sentinel process)
         (lambda (original exited event)
           ;; acp.el fails pending requests here; read what was open first.
           (let ((interrupted (and (buffer-live-p buffer)
                                   (or (map-elt state :active-requests)
                                       (buffer-local-value 'shell-maker--busy buffer)))))
             (funcall original exited event)
             (+agent-acp--client-exited buffer state exited event
                                        (and interrupted t)))))))))

(defvar-local +agent-acp--lifecycle-state nil)
(defvar-local +agent-acp--lifecycle-subscription nil)

(defun +agent-acp--watch-lifecycle-h ()
  "Keep one state-scoped lifecycle subscription, including after a reload."
  (unless (eq +agent-acp--lifecycle-state agent-shell--state)
    (when (and +agent-acp--lifecycle-state +agent-acp--lifecycle-subscription)
      (setf (map-elt +agent-acp--lifecycle-state :event-subscriptions)
            (seq-remove
             (lambda (sub) (equal (map-elt sub :token) +agent-acp--lifecycle-subscription))
             (map-elt +agent-acp--lifecycle-state :event-subscriptions))))
    (setq +agent-acp--lifecycle-state agent-shell--state
          +agent-acp--lifecycle-subscription
          (+agent-acp-subscribe
           :buffer (current-buffer)
           :callback
           (lambda (event)
             (+agent-acp--watch-process)
             (when (memq (map-elt event :event) '(input-submitted turn-complete clean-up))
               (+agent-acp--clear-stop)
               (+agent-acp--changed)))))))

(add-hook 'agent-shell-mode-hook #'+agent-acp--watch-lifecycle-h)
(dolist (buffer (buffer-list))
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer (+agent-acp--watch-lifecycle-h))))

(defun +agent-acp-show-buffer (buffer)
  "Show agent BUFFER in its project Agent window and hydrate deferred output."
  (unless (+agent-acp-agent-buffer-p buffer)
    (user-error "No live agent buffer"))
  (+agent-acp--install-workspace-tabs buffer)
  (+agent-view-show buffer)
  (+agent-acp-touch buffer)
  (+agent-acp-clear-attention buffer)
  (+agent-render-flush buffer)
  buffer)

(defun +agent-acp-hide (&optional buffer)
  "Hide the current agent view and return through its window-owned source."
  (interactive)
  (+agent-view-hide (+agent-acp-command-buffer buffer)))

(defun +agent-acp-review-request (&optional buffer)
  "Open BUFFER at its native approval controls or unanswered form."
  (interactive)
  (let* ((buffer (+agent-acp-command-buffer buffer))
         (attention (plist-get (+agent-session-snapshot buffer) :attention-kind)))
    (unless (memq attention '(permission input))
      (user-error "This session has no pending approval or question"))
    (+agent-acp-show-buffer buffer)
    (with-current-buffer buffer
      (pcase attention
        ('permission (agent-shell-jump-to-latest-permission-button-row))
        ('input (agent-shell-elicitation-jump-to-latest-form))))))

(defun +agent-acp--display-buffer-a (orig buffer &rest args)
  "Route every interactive agent-shell display through the shared workspace.

The Noema worker path already calls `+agent-acp-show-buffer', but direct
Codex/Claude/OpenCode/Pi and Magent entry points call upstream
`agent-shell--display-buffer' themselves.  Without this narrow advice those
entry points recreate the per-agent visible buffers that the project
workspace is meant to replace."
  (if (+agent-acp-agent-buffer-p buffer)
      (progn
        (with-current-buffer buffer
          (unless +agent-acp-session-root
            (setq-local +agent-acp-session-root
                        (file-name-as-directory (expand-file-name default-directory))))
          (unless +agent-acp-session-agent
            (setq-local +agent-acp-session-agent
                        (format "%s"
                                (or (map-nested-elt agent-shell--state
                                                      '(:agent-config :identifier))
                                    "agent"))))
          (unless +agent-acp-display-buffer-function
            (+agent-acp--install-workspace-tabs buffer)))
        (+agent-acp-show-buffer buffer))
    (apply orig buffer args)))

(unless (advice-member-p #'+agent-acp--display-buffer-a
                         'agent-shell--display-buffer)
  (advice-add 'agent-shell--display-buffer :around
              #'+agent-acp--display-buffer-a))

(defun +agent-acp-set-permission-responder (buffer responder)
  "Install RESPONDER for structured permission requests in BUFFER."
  (with-current-buffer buffer
    (setq-local agent-shell-permission-responder-function responder)))

(defun +agent-acp-draft (buffer text)
  "Put TEXT in BUFFER's native input for review; do not submit it."
  (agent-shell-insert :text text :submit nil :shell-buffer buffer))

(defvar-local +agent-acp--prompt-receipt nil
  "Completion fact for the exact Noema prompt, independent of UI callbacks.")

(defun +agent-acp-prompt-receipt (buffer run-id)
  "Return BUFFER's completion receipt only when it belongs to RUN-ID.
No buffer text, elapsed time or absence of activity is evidence of success."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (equal (plist-get +agent-acp--prompt-receipt :run-id) run-id)
        +agent-acp--prompt-receipt))))

(defun +agent-acp--display-submitted-content (state content run-id)
  "Show Noema's submitted CONTENT using the upstream conversation renderer.
ACP servers need not echo live user messages.  This display is separate
from transport and preserves any draft at the native input prompt."
  (let ((namespace (format "noema-run-%s" (or run-id (map-elt state :request-count))))
        (texts (seq-filter (lambda (block) (equal (map-elt block 'type) "text")) content))
        (contexts (seq-remove (lambda (block) (equal (map-elt block 'type) "text")) content)))
    (when texts
      (agent-shell--update-fragment
       :state state :namespace-id namespace :block-id "submitted-input" :label-left "You"
       :body (mapconcat (lambda (block) (or (map-elt block 'text) "")) texts "\n\n")
       :expanded t :above-last-prompt t))
    (when contexts
      (agent-shell--update-fragment
       :state state :namespace-id namespace :block-id "submitted-context"
       :label-left (format "Context · %d attachments" (length contexts))
       :body (mapconcat
              (lambda (block)
                (format "%s%s"
                        (or (map-elt block 'name) (map-elt block 'type) "Context")
                        (if-let* ((uri (or (map-nested-elt block '(resource uri))
                                           (map-elt block 'uri))))
                            (concat "\n" uri) "")))
              contexts "\n\n")
       :expanded nil :navigation 'never :above-last-prompt t))))

(defun +agent-acp-structured-turn-p (buffer)
  "Whether a structured prompt is running in BUFFER.
shell-maker does not know about it, so the native input would otherwise send a
second, concurrent prompt into the same session."
  (eq (plist-get (buffer-local-value '+agent-acp--prompt-receipt buffer) :status)
      'pending))

(defun +agent-acp--finish-prompt (buffer receipt &optional process-queue)
  "Restore native input for RECEIPT, unless BUFFER has begun another turn."
  (when (and (+agent-acp-agent-buffer-p buffer)
             (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer)))
    (with-current-buffer buffer
      (unwind-protect
          (agent-shell--update-fragment
           :state agent-shell--state
           :namespace-id (format "noema-run-%s" (or (plist-get receipt :run-id)
                                                   (map-elt agent-shell--state :request-count)))
           :block-id "turn-status"
           :label-left (cond ((eq (plist-get receipt :status) 'failed) "Failed")
                             ((equal (map-elt (plist-get receipt :response) 'stopReason) "cancelled")
                              "Stopped")
                             ((not (member (map-elt (plist-get receipt :response) 'stopReason)
                                           '(nil "" "end_turn"))) "Failed")
                             (t "Completed"))
           :above-last-prompt (and (not +agent-render--pending)
                                   (+agent-acp--live-input-p)))
        (+agent-acp-restore-prompt buffer)
        ;; A structured turn bypasses the native flow that sends the queue, and
        ;; the Run's owner may start the next one: leave it for the person.
        (unless process-queue (+agent-acp--hold-queue))
        (+agent-acp--changed)))))

(cl-defun +agent-acp-prompt (&key buffer content run-id on-success on-failure process-queue)
  "Send CONTENT through BUFFER's initialized ACP session.
PROCESS-QUEUE continues native pending prompts after a normal completed turn.
Research callers retain their explicit queue policy by leaving it nil."
  (with-current-buffer buffer
    (+agent-acp--clear-stop)
    (+agent-acp--turn-began)
    (setq-local +agent-acp-last-error nil)
    ;; A new turn is running; do not write a prompt into its output.
    (setq-local +agent-acp--prompt-owed nil)
    (let ((state agent-shell--state)
          (receipt (list :run-id run-id :status 'pending :response nil :error nil)))
      (setq-local +agent-acp--prompt-receipt receipt)
      ;; `agent-shell--handle' normally advances the turn before dispatch.
      ;; Frozen structured prompts bypass that input parser, not turn identity.
      (shell-maker--increment-request-id)
      (setf (alist-get :request-count state) (shell-maker--current-request-id)
            (alist-get :last-entry-type state) nil
            (alist-get :last-activity-time state) (current-time))
      (setq agent-shell--state state)
      ;; Keep this before transport so even immediately streamed replies follow
      ;; their input.  The hidden renderer queues both in the same FIFO.
      (condition-case err
          (+agent-acp--display-submitted-content state content run-id)
        (error (message "Noema input display: %s" (error-message-string err))))
      (agent-shell--send-request
       :state state :client (map-elt state :client)
       :request (acp-make-session-prompt-request
                 :session-id (map-nested-elt state '(:session :id))
                 :prompt content)
       :buffer buffer
       :on-success (lambda (response)
                     ;; Record the protocol fact before any consumer/UI runs.
                     (setf (plist-get receipt :status) 'completed
                           (plist-get receipt :response) response)
                     (when (and (buffer-live-p buffer)
                                (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer)))
                       (with-current-buffer buffer
                         (+agent-acp--clear-stop)
                         (+agent-acp--turn-ended
                          (format "%s" (or (map-elt response 'stopReason) "")))))
                     (unwind-protect
                         (when on-success (funcall on-success response))
                       (+agent-acp--finish-prompt buffer receipt process-queue)
                       (when (and (buffer-live-p buffer)
                                  (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer)))
                         (+agent-acp--report-turn-complete
                          buffer `((:event . turn-complete)
                                   (:data . ((:stop-reason . ,(map-elt response 'stopReason)))))))
                       (when (and process-queue (buffer-live-p buffer)
                                  (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer))
                                  (equal (map-elt response 'stopReason) "end_turn")
                                  (zerop (+agent-acp-held-prompt-count buffer))
                                  (> (+agent-acp-pending-prompt-count buffer) 0)
                                  (not (+agent-acp-busy-p buffer)))
                         ;; Submitting the next native turn needs an actual input
                         ;; prompt, even when this buffer's output was deferred.
                         (+agent-render-flush buffer)
                         (with-current-buffer buffer (agent-shell--prompt-queue-process-next)))))
       :on-failure (lambda (error raw)
                     (setf (plist-get receipt :status) 'failed
                           (plist-get receipt :error) raw)
                     (when (and (buffer-live-p buffer)
                                (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer)))
                       (with-current-buffer buffer
                         (+agent-acp--clear-stop)
                         (setq +agent-acp--turn-started-at nil
                               +agent-acp-last-error (or error "ACP request failed"))
                         (+agent-acp--hold-queue)))
                     (unwind-protect
                         (when on-failure (funcall on-failure error raw))
                       (+agent-acp--finish-prompt buffer receipt process-queue)
                       (when (and (buffer-live-p buffer)
                                  (eq receipt (buffer-local-value '+agent-acp--prompt-receipt buffer)))
                         (+agent-acp--report-turn-complete
                          buffer '((:event . turn-complete) (:data . ((:stop-reason . "error"))))))))))))

(defun +agent-acp-file-uri (file &optional session)
  "Return FILE as the `file://' uri agent-shell would put in a content block.
With SESSION, the path is the one that session's agent opens (see
`+agent-acp-agent-file'), so a remote agent gets its own machine's path.
Otherwise path resolution honours `agent-shell-path-resolver-function'."
  (concat "file://"
          (or (and session (+agent-acp-agent-file file session))
              (agent-shell--resolve-path (expand-file-name file)))))

(defun +agent-acp-file-metadata (file)
  "Return (:mime-type MIME :size BYTES) for FILE, or nil when unreadable.
Only metadata is read; the file contents are never loaded."
  (when-let* ((file (expand-file-name file))
              ((file-readable-p file))
              (meta (ignore-errors
                      (agent-shell--read-file-content :file-path file :shallow t))))
    (list :mime-type (map-elt meta :mime-type)
          :size (map-elt meta :size))))

(defun +agent-acp-enqueue (buffer text)
  "Queue TEXT as BUFFER's next turn through agent-shell's pending queue.
Used when a session is mid-turn: upstream sends the queued prompt itself once
the running turn finishes."
  (with-current-buffer buffer
    (agent-shell--prompt-queue-enqueue :prompt text)))

(defun +agent-acp--queue-changed-a (&rest _)
  "Report a queue length change, which agent-shell keeps to itself."
  (when (derived-mode-p 'agent-shell-mode) (+agent-acp--changed)))
(advice-add 'agent-shell--prompt-queue-enqueue :after #'+agent-acp--queue-changed-a)
(advice-add 'agent-shell--prompt-queue-process-next :after #'+agent-acp--queue-changed-a)

(defun +agent-acp-interrupt (buffer &optional force)
  "Interrupt the active request in BUFFER; FORCE skips confirmation."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer (agent-shell-interrupt force))))

(defun +agent-acp-shutdown (buffer)
  "Shut down BUFFER's physical ACP client."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer (agent-shell--shutdown))))

(defun +agent-acp-available-modes (buffer)
  "Return BUFFER's native ACP session modes."
  (with-current-buffer buffer
    (when (fboundp 'agent-shell--get-available-modes)
      (agent-shell--get-available-modes agent-shell--state))))

(cl-defun +agent-acp-set-mode (buffer mode-id on-success on-failure)
  "Set BUFFER to native MODE-ID and invoke completion callbacks."
  (with-current-buffer buffer
    (agent-shell--config-option-set-mode-id
     :mode-id mode-id :on-success on-success :on-failure on-failure)))

;;;###autoload
(defun +agent/resume-saved (&optional agent)
  "Choose a saved conversation of AGENT in this workspace and restore it.
This needs no live session: a new client lists the provider's own record.
Cancelling, or finding nothing to restore, ends that client."
  (interactive
   (list (intern (completing-read "Resume with agent: " '("codex" "claude" "opencode")
                                  nil t nil nil "codex"))))
  (let* ((agent (or agent 'codex))
         (source (+agent-acp-agent-buffer-p (current-buffer)))
         (root (or (and source +agent-acp-session-root) (+agent-acp-project-root)))
         (config (or (+agent-acp-config-for agent)
                     (user-error "No agent configuration for %s" agent)))
         (buffer (+agent-acp-start :config config :focus t :origin 'manual :choose-saved t
                                   :directory (if source default-directory (+agent-workspace root)))))
    (+agent-acp-adopt buffer :agent agent :origin 'manual :root root)
    buffer))

;;;###autoload
(defun +agent/start (&optional agent)
  "Start a reusable agent session in the current workspace."
  (interactive
   (list (intern (completing-read "Agent: " '("codex" "claude" "opencode" "pi" "magent") nil t nil nil "codex"))))
  (let* ((agent (or agent 'codex))
         (source (+agent-acp-agent-buffer-p (current-buffer)))
         (root (or (and source +agent-acp-session-root) (+agent-acp-project-root)))
         (directory (if source default-directory (+agent-workspace root)))
         (buffer
          (pcase agent
            ('magent (require 'magent-agent-shell) (magent-start))
            ('pi (require 'agent-shell-pi) (agent-shell-pi-start-agent))
            (_ (let ((config (+agent-acp-config-for agent)))
                 (unless config (user-error "No agent configuration for %s" agent))
                 (+agent-acp-start :config config :directory directory :origin 'manual :focus t))))))
    (when (+agent-acp-agent-buffer-p buffer)
      (+agent-acp-adopt buffer :agent agent :origin 'manual :root root))
    buffer))

(+agent-acp--retire-transcript-workspace)


(provide 'tools/agent/acp)
;;; tools/agent/acp.el ends here
