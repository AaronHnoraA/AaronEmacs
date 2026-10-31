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
(require 'seq)
(require 'simple)
(require 'tab-line)
(require 'aaron-lib)
(require 'agent-shell)
(require 'acp)
(require 'tools/agent/render)
(+agent-render-mode 1)

(defvar +agent-project-root-functions nil "Queries receiving a directory and returning an optional domain project root.")
(defvar +agent-workspace-functions nil "Queries receiving a root and returning its execution workspace.")
(defvar +agent-session-adopt-functions nil "Called with BUFFER, NAME and ROOT after local session registration.")
(defvar +agent-session-forget-functions nil "Called with BUFFER when it gives up a local session name.")
(defvar +agent-session-id-functions nil "Queries receiving BUFFER and returning an optional domain session id.")
(defvar +agent-session-close-functions nil "Called with BUFFER before its process and frontend are ended.")
(defvar +agent-session-menu-functions nil "Functions receiving BUFFER and returning domain menu items.")
(defvar +agent-session-operation-functions nil "Predicates receiving OPERATION and BUFFER; non-nil reports a domain capability.")
(defvar +agent-session-action-functions nil "Functions receiving OPERATION, BUFFER and arguments; non-nil handles it.")
(defvar +agent-session-attention-functions nil "Queries receiving BUFFER and returning its domain attention reason.")
(defvar +agent-session-private-functions nil "Predicates receiving a snapshot; non-nil excludes it from ordinary context selection.")
(defvar +agent-acp-previewing nil "Non-nil while a reversible session preview is active.")
(defun +agent-workspace (root)
  "Return ROOT's execution workspace without enabling a domain project."
  (or (run-hook-with-args-until-success '+agent-workspace-functions root) root))
(autoload '+agent-session-restart "tools/agent/sessions")
(autoload '+agent-session-rename "tools/agent/sessions")
(autoload '+agent-session-fork "tools/agent/sessions")
(autoload '+agent-session-side-chat "tools/agent/sessions")
(autoload '+agent-session-jump "tools/agent/sessions")
(autoload '+agent-session-archive "tools/agent/sessions")
(autoload '+agent-session-list "tools/agent/sessions")


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
  "Optional Emacs-owned display function for this agent buffer.
The popup pool uses this boundary instead of changing agent-shell internals.
The function receives the live agent buffer and owns only its presentation.")
(put '+agent-acp-display-buffer-function 'permanent-local t)

(defvar-local +agent-acp-last-used-at nil
  "Time this Noema-owned physical Session was last used or displayed.")
(put '+agent-acp-last-used-at 'permanent-local t)

(defvar-local +agent-acp-context-warning-level 0
  "Highest Noema context pressure warning emitted for this physical Session.")
(put '+agent-acp-context-warning-level 'permanent-local t)

;; D-033: every physical agent-shell buffer stays out of the global tab-line
;; and tab-bar.  A project's sessions are tabs of one bottom-right Agent
;; window; each tab is the session's own interactive agent-shell buffer.

(defvar +agent-acp-tabs-mode-map (make-sparse-keymap)
  "Keymap for managing sessions inside the Agent window.")

;; Bound at top level, not in the `defvar' initial value: reloading this file
;; must update the keymap that live Agent buffers already use.
(dolist (binding '(("C-c C-a" . +agent-acp-switch-session)
                   ("C-c C-n" . +agent-acp-next-session)
                   ("C-c C-p" . +agent-acp-previous-session)
                   ("C-c C-e" . +agent-acp-focus-input)
                   ("C-c C-v" . agent-shell-set-session-model)
                   ("C-c C-x" . +agent-acp-stop)
                   ("C-c C-r" . +agent-session-restart)
                   ("C-c C-k" . +agent-acp-close)
                   ("C-c M-k" . +agent-acp-close-others)
                   ("C-c C-w" . +agent-session-rename)
                   ("C-c C-f" . +agent-session-fork)
                   ("C-c C-t" . +agent-acp-conversation-tree)
                   ("C-c C-q" . +agent-session-side-chat)
                   ("C-c C-d" . +agent-session-archive)
                   ("C-c C-j" . +agent-session-jump)
                   ("C-c C-l" . +agent-session-list)
                   ("C-c C-z" . +agent-acp-menu)
                   ("C-c ?" . +agent-acp-help)
                   ("?" . +agent-acp-help-or-insert)))
  (define-key +agent-acp-tabs-mode-map (kbd (car binding)) (cdr binding)))

(define-minor-mode +agent-acp-tabs-mode
  "Show this agent-shell buffer as one tab of its project Agent window.

The buffer remains the real, interactive agent-shell session.  Its tab line
lists every live session of the same project; choosing a tab shows that
session's own buffer in the Agent window instead of copying a transcript.
Right-click a tab, or press \\[+agent-acp-menu], for session management;
\\[+agent-acp-help] describes every key.

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
      (kbd "?") #'+agent-acp-help)
    (dolist (state '(normal insert visual motion))
      (evil-define-minor-mode-key state '+agent-acp-tabs-mode
        (kbd "C-c C-e") #'+agent-acp-focus-input
        (kbd "C-c C-x") #'+agent-acp-stop
        (kbd "C-c C-v") #'agent-shell-set-session-model))))

(defun +agent-acp--sync-shell-buffer-name (buffer)
  "Keep shell-maker's native input/output target attached to BUFFER.
The shared registry gives sessions stable display names.  Shell-maker resolves its
process through this buffer-local name, so it must follow every rename."
  (with-current-buffer buffer
    (setq-local shell-maker--buffer-name-override (buffer-name))))

(defun +agent-acp--workspace-root (root)
  "Normalize ROOT with Remote's client projection when available."
  (let ((root (file-name-as-directory (expand-file-name (or root default-directory)))))
    (if (and (fboundp 'remote-canonicalize-emacs-file-name)
             (fboundp 'remote-client-file-name))
        (let ((logical (remote-canonicalize-emacs-file-name root)))
          (or (remote-client-file-name logical) logical))
      root)))

(defun +agent-acp--tab-key (buffer)
  "Return agent BUFFER's stable tab key: coordinator, named, then retired."
  (let ((name (buffer-local-value '+agent-acp-session-name buffer)))
    (cons (cond ((equal name "pi") 0) (name 1) (t 2))
          (or name (buffer-name buffer)))))

(defun +agent-acp--project-buffers (&optional root)
  "Return live agent buffers of project ROOT in stable tab order.
The coordinator comes first and sessions follow by name, so selecting a tab
never moves the others."
  (let ((root (+agent-acp--workspace-root
               (or root +agent-acp-session-root))))
    (sort
     (seq-filter
      (lambda (buffer)
        (and (+agent-acp-agent-buffer-p buffer)
             (not (buffer-local-value '+agent-acp-display-buffer-function buffer))
             (equal (buffer-local-value '+agent-acp-session-root buffer) root)))
      (buffer-list))
     (lambda (left right)
       (let ((left (+agent-acp--tab-key left))
             (right (+agent-acp--tab-key right)))
         (if (= (car left) (car right))
             (string-lessp (cdr left) (cdr right))
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

(defun +agent-acp--tab-line ()
  "Return the session tab line of the current agent buffer's project."
  (let ((current (current-buffer)))
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
                       (+agent-acp-menu buffer event)))
         (define-key map [tab-line mouse-3] #'ignore)
         (propertize (concat " " (+agent-acp--tab-label buffer) " ")
                     'face (if selected 'tab-line-tab-current 'tab-line-tab-inactive)
                     'mouse-face 'tab-line-highlight
                     'help-echo (if selected
                                    "mouse-3: manage this agent session"
                                  "mouse-1: show this agent session\nmouse-3: manage it")
                     'keymap map)))
     (+agent-acp--project-buffers +agent-acp-session-root)
     " ")))

(defun +agent-acp--refresh-tabs (&rest _)
  "Redraw Agent window tab lines after a project's session set changed."
  (run-at-time 0 nil #'tab-line-force-update t))

(defun +agent-acp--install-workspace-tabs (buffer)
  "Make agent BUFFER a tab of its project Agent window.
The buffer stays out of the global tab-line and tab-bar (D-033) and is still
the real agent-shell buffer, so it accepts input and interrupts directly."
  (when (and (buffer-live-p buffer)
             (not (buffer-local-value '+agent-acp-display-buffer-function buffer)))
    (with-current-buffer buffer
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
  (let ((choices (mapcar (lambda (buffer)
                           (cons (+agent-acp--tab-label buffer) buffer))
                         (+agent-acp--project-buffers +agent-acp-session-root))))
    (unless choices
      (user-error "No live agent session in this project"))
    (+agent-acp-show-buffer
     (cdr (assoc (completing-read "Agent session: " choices nil t) choices)))))

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
    (unless (and (+agent-acp-state-value source '(:supports-session-list))
                 (+agent-acp-state-value source '(:supports-session-load)))
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

(defun +agent-acp-busy-p (buffer)
  "Return non-nil while agent BUFFER answers a prompt or serves a Noema Run."
  (or (buffer-local-value 'shell-maker--busy buffer)
      (run-hook-with-args-until-success '+agent-acp-busy-functions buffer)))

(defun +agent-acp-pending-prompt-count (buffer)
  "Return how many prompts the person queued in agent BUFFER.
agent-shell owns the queue (`agent-shell-prompt-queue', steering and
removal); Noema only reads its length so lists can show queued intent."
  (if (buffer-live-p buffer)
      (length (ignore-errors
                (map-elt (buffer-local-value 'agent-shell--state buffer) :pending-prompts)))
    0))

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
;; person started by hand -- `C-c A a', a worktree session, the popup pool, a
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
      (unless (get-buffer-window buffer 'visible)
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

(defun +agent-acp--attention-shown (_window)
  "Clear the current agent buffer's attention now that a window shows it."
  (unless +agent-acp-previewing (+agent-acp-clear-attention (current-buffer))))

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
                     (setq +agent-acp-last-error (or (map-elt event :data) t)))
                   (+agent-acp--note-attention buffer 'failed))))
    (+agent-acp-subscribe
     :buffer buffer :event 'input-submitted
     :callback (lambda (_event)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer (setq +agent-acp-last-error nil))
                   (+agent-acp-clear-attention buffer))))
    (+agent-acp-subscribe
     :buffer buffer :event 'turn-complete
     :callback (lambda (event)
                 (run-hook-with-args
                  '+agent-acp-turn-complete-functions buffer event)
                 ;; An interrupt is the person's own act and is not news.
                 (unless (equal (format "%s" (or (map-elt (map-elt event :data)
                                                          :stop-reason)
                                                 ""))
                                "cancelled")
                   (+agent-acp--note-attention buffer 'done))))))

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
  (add-hook 'after-change-functions #'+agent-acp-note-activity nil t)
  (add-hook 'kill-buffer-hook #'+agent-acp--changed nil t)
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
       (memq (buffer-local-value '+agent-acp-session-origin buffer)
             +agent-acp-auto-stop-origins)
       (not (+agent-acp-busy-p buffer))
       ;; Queued prompts are the person's intent still waiting to be sent.
       (zerop (+agent-acp-pending-prompt-count buffer))
       (not (get-buffer-window buffer t))))

(defun +agent-acp-confirm-stop (buffer action)
  "Return non-nil when ACTION may stop agent BUFFER, asking while it is busy."
  (or (not (+agent-acp-busy-p buffer))
      (yes-or-no-p (format "%s is still working; %s it anyway? "
                           (+agent-acp--tab-label buffer) action))))

(defun +agent-acp-stop (&optional buffer)
  "Stop agent BUFFER's work: cancel its Noema Run, else interrupt the turn."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (unless (run-hook-with-args-until-success '+agent-acp-stop-functions buffer)
      (+agent-acp-interrupt buffer t))))



(defun +agent-acp-kill (buffer)
  "End BUFFER's physical session, leaving provider history intact."
  (let* ((root (buffer-local-value '+agent-acp-session-root buffer))
         (next (car (delq buffer (+agent-acp--project-buffers root)))))
    (run-hook-with-args '+agent-session-close-functions buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (cond (next (set-window-buffer window next))
            ((window-parameter window 'agent-workspace) (ignore-errors (delete-window window)))))
    (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))

(defun +agent-acp-close (&optional buffer)
  "Close agent BUFFER's tab.  Its process stops; its name and history remain."
  (interactive)
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (when (+agent-acp-confirm-stop buffer "close")
      (+agent-acp-kill buffer))))

(defun +agent-acp--close-where (buffer predicate)
  "Close the tabs of BUFFER's project whose buffer satisfies PREDICATE."
  (dolist (other (+agent-acp--project-buffers
                  (buffer-local-value '+agent-acp-session-root buffer)))
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
               (not (map-elt agent-shell--state :active-requests))
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
      (unless (map-elt agent-shell--state :active-requests)
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
    (with-current-buffer buffer
      (when (or (map-elt agent-shell--state :active-requests)
                (run-hook-with-args-until-success '+agent-acp-busy-functions buffer))
        (user-error "Agent is still working; stop it with C-c C-x before editing")))
    ;; Showing renders deferred output first; the owed prompt follows it.
    (+agent-acp-show-buffer buffer)
    (with-current-buffer buffer
      ;; Also repair sessions that completed before this code was loaded.
      (+agent-acp-restore-prompt buffer))
    (when-let* ((window (get-buffer-window buffer (selected-frame))))
      (select-window window)
      (goto-char (point-max))
      (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-insert-state))
        (evil-insert-state))
      (set-window-point window (point))
      (recenter -3))
    buffer))

(defun +agent-acp--menu-items (buffer)
  "Return generic actions plus explicitly supplied domain actions for BUFFER."
  (let ((named (and (buffer-local-value '+agent-acp-session-name buffer) t)))
   (append
   `(["Show" (+agent-acp-show-buffer ,buffer)]
     ["Focus input" (+agent-acp-focus-input ,buffer)]
     ["Choose model..." (with-current-buffer ,buffer (call-interactively #'agent-shell-set-session-model))]
     ["Stop current work" (+agent-acp-stop ,buffer)] "--"
     ["Restart session" (+agent-session-restart ,buffer) :active ,named]
     ["Rename session..." (+agent-session-rename ,buffer)]
     ["Fork session..." (+agent-session-fork ,buffer)
      (+agent-session-operation-supported-p 'fork ,buffer)]
     ["Conversation tree" (+agent-acp-conversation-tree ,buffer)]
     ["Side chat beside it" (+agent-session-side-chat ,buffer)])
   (mapcan (lambda (fn) (funcall fn buffer)) +agent-session-menu-functions)
   `("--" ["Close tab" (+agent-acp-close ,buffer)]
     ["Close other tabs" (+agent-acp-close-others ,buffer)]
     ["Close retired tabs" (+agent-acp-close-retired ,buffer)]
     "--" ["Session list" (+agent-session-list ,buffer)] ["Help" +agent-acp-help]))))

(defun +agent-acp-menu (&optional buffer event)
  "Pop up the session management menu of agent BUFFER at EVENT or point."
  (interactive (list nil last-nonmenu-event))
  (let ((buffer (+agent-acp-command-buffer buffer)))
    (popup-menu (easy-menu-create-menu (+agent-acp--tab-label buffer)
                                       (+agent-acp--menu-items buffer))
                (if (mouse-event-p event) event (posn-at-point)))))

(defun +agent-acp-help ()
  "Describe the Agent window, its session tabs and their keys."
  (interactive)
  (with-help-window "*Agent Help*"
    (princ
     (substitute-command-keys
      "Agent window

Each tab is one live agent session of this project.  The buffer is the real
agent-shell: type at its prompt and press RET to send.

Mouse
  mouse-1 on a tab\tshow that session
  mouse-3 on a tab\tsession menu

\\<+agent-acp-tabs-mode-map>Sessions
  \\[+agent-acp-focus-input]\tjump to the input prompt
  \\[+agent-acp-switch-session]\tpick a session
  \\[+agent-acp-next-session] / \\[+agent-acp-previous-session]\tnext / previous tab
  \\[+agent-acp-stop]\tstop the current Run or turn
  \\[+agent-session-restart]\trestart the session and resume its conversation
  \\[+agent-acp-close]\tclose this tab (name and history remain)
  \\[+agent-acp-close-others]\tclose the other tabs
  \\[+agent-session-rename]\trename the session
  \\[+agent-session-fork]\tfork the session
  \\[+agent-acp-conversation-tree]\tbrowse this conversation's branches
  \\[+agent-session-side-chat]\tside chat: ask beside it without interrupting
  \\[+agent-session-archive]\tarchive the session and close its tab
  \\[+agent-session-jump]\tjump to its latest work block
  \\[+agent-session-list]\tsession list
  \\[+agent-acp-menu]\tsession menu
  ?\tthis help, outside the prompt input

\\<agent-shell-mode-map>Agent shell
  \\[agent-shell-interrupt]\tinterrupt the current turn
  \\[agent-shell-set-session-mode]\tsession mode
  \\[agent-shell-set-session-model]\tmodel
"))))

(defun +agent-acp-help-or-insert ()
  "Insert `?' in the prompt input; elsewhere describe the Agent window."
  (interactive)
  (let ((start (+agent-acp--input-start (current-buffer))))
    (if (and start (>= (point) start))
        (call-interactively #'self-insert-command)
      (+agent-acp-help))))

(defun +agent-acp--workspace-window (&optional frame)
  "Return FRAME's one Agent window, if it is live."
  (seq-find (lambda (window)
              (window-parameter window 'agent-workspace))
            (window-list (or frame (selected-frame)) 'nomini)))

(defun +agent-acp--right-pane-p (window)
  "Return non-nil when WINDOW already occupies a right-hand pane."
  (let ((edges (window-edges window)))
    (>= (car edges) (/ (frame-width (window-frame window)) 2))))

(defun +agent-acp--bottom-right-window (buffer frame)
  "Return a new bottom-right window in FRAME for agent BUFFER."
  (let* ((windows (seq-remove #'window-minibuffer-p (window-list frame 'nomini)))
         (right-pane (seq-find #'+agent-acp--right-pane-p
                               (sort (copy-sequence windows)
                                     (lambda (left right)
                                       (> (car (window-edges left))
                                          (car (window-edges right)))))))
         (base (or right-pane (frame-selected-window frame)))
         (bottom (condition-case nil
                     (if (+agent-acp--right-pane-p base)
                         (split-window base nil 'below)
                       (let ((right (split-window base nil 'right)))
                         (split-window right nil 'below)))
                   (error nil))))
    (or bottom
        (display-buffer-in-side-window
         buffer
         `((side . right)
           (slot . 0)
           (window-width . 0.38)
           (window-parameters . ((agent-workspace . t))))))))

(defun +agent-acp--display-workspace-buffer (buffer)
  "Show agent BUFFER itself in its frame's one bottom-right Agent window."
  (let* ((frame (if (frame-parameter (selected-frame) 'agent-standalone)
                    (or (seq-find (lambda (candidate)
                                    (not (frame-parameter candidate 'agent-standalone)))
                                  (frame-list))
                        (selected-frame))
                  (selected-frame)))
         (root (buffer-local-value '+agent-acp-session-root buffer))
         (existing (+agent-acp--workspace-window frame)))
    ;; A previous version placed the window across the whole bottom edge.
    ;; Do not reuse that stale geometry; a reload repairs the layout.
    (when (and (window-live-p existing)
               (not (+agent-acp--right-pane-p existing)))
      (ignore-errors (delete-window existing))
      (setq existing nil))
    (let ((window (or existing
                      (+agent-acp--bottom-right-window buffer frame))))
      (unless (window-live-p window)
        (user-error "Unable to create the Agent window"))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-parameter window 'agent-workspace t)
      (set-window-parameter window 'agent-workspace-root root)
      (window-preserve-size window nil t)
      ;; Sessions are tabs of this one window; retire any other window that
      ;; still shows an agent buffer.
      (dolist (other (window-list frame 'nomini))
        (when (and (not (eq other window))
                   (+agent-acp-agent-buffer-p (window-buffer other)))
          (ignore-errors (quit-window nil other))))
      (select-window window)
      (select-frame-set-input-focus frame)
      window)))

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
  "Mark BUFFER as the private ACP transport for session NAME in DIRECTORY.
D-033: physical ACP buffers are hidden implementation details rendered into
one project Agent Workspace.  Stable local identity lets the session manager
and OutputArea find the exact transport without creating another UI buffer.
D-035: another live buffer still holding NAME in the same project served an
older physical session; it gives the name up so lookups stay unambiguous."
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
        (let ((root +agent-acp-session-root))
          (dolist (other (buffer-list))
            (when (and (not (eq other buffer))
                       (+agent-acp-agent-buffer-p other)
                       (equal (buffer-local-value '+agent-acp-session-name other) name)
                       (equal (buffer-local-value '+agent-acp-session-root other) root))
              (with-current-buffer other
                (setq-local +agent-acp-session-name nil)
                (run-hook-with-args '+agent-session-forget-functions other)
                (unless (string-suffix-p " (retired)" (buffer-name))
                  (rename-buffer (concat (buffer-name) " (retired)") t))
                (+agent-acp--sync-shell-buffer-name other))
              ;; An idle retired buffer only duplicates a tab; its conversation
              ;; stays in the Run history.  A busy one stays until it finishes.
              (let ((retired other))
                (run-at-time 0 nil
                             (lambda ()
                               (when (and (buffer-live-p retired)
                                          (not (+agent-acp-busy-p retired)))
                                 (+agent-acp-kill retired))))))))
        (setq-local +agent-acp-session-name name
                    +agent-acp-session-agent agent)
        (let ((wanted (format " *Agent · %s · %s @ %s*"
                              name (or agent "agent")
                              (file-name-nondirectory
                               (directory-file-name (or +agent-acp-session-root
                                                        default-directory))))))
          (unless (string-prefix-p wanted (buffer-name))
            (rename-buffer wanted t)))
        (+agent-acp--sync-shell-buffer-name buffer)))
    (+agent-acp--refresh-tabs)
    buffer))

(defun +agent-acp-session-buffer (name root)
  "Return the live agent buffer serving session NAME in project ROOT."
  (let ((root (and root (file-name-as-directory (expand-file-name root)))))
    (seq-find (lambda (buffer)
                (and (+agent-acp-agent-buffer-p buffer)
                     (equal (buffer-local-value '+agent-acp-session-name buffer) name)
                     (or (null root)
                         (equal (buffer-local-value '+agent-acp-session-root buffer) root))))
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
                           (buffer-local-value '+agent-acp-session-name buffer)))
                    (buffer-list))))

(defun +agent-acp--unique-name (base root)
  "Return BASE, or BASE plus a counter, unclaimed by a live session of ROOT.
The coordinator name \"pi\" is reserved and is never generated here."
  (let ((taken (cons "pi" (+agent-acp--claimed-names root)))
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

(defun +agent-session-snapshot (buffer)
  "Describe one real agent BUFFER without a domain service or disk scan."
  (when (+agent-acp-agent-buffer-p buffer)
    (with-current-buffer buffer
      (let* ((state (+agent-acp--state buffer))
             (process (map-nested-elt state '(:client :process)))
             (directory default-directory)
             (attention (or (run-hook-with-args-until-success '+agent-session-attention-functions buffer)
                            (and (seq-some (lambda (entry) (map-elt (cdr entry) :permission-request-id))
                                           (map-elt state :tool-calls)) 'permission)
                            (and (seq-some (lambda (entry) (eq (map-elt (cdr entry) :status) 'pending))
                                           (map-elt state :elicitations)) 'input)
                            (and +agent-acp-last-error 'failed))))
        (list :buffer buffer :name +agent-acp-session-name :agent +agent-acp-session-agent
              :provider (map-nested-elt state '(:agent-config :identifier))
              :origin +agent-acp-session-origin :root +agent-acp-session-root
              :target (and (fboundp 'remote-file-name-target) (remote-file-name-target directory))
              :workspace (if (fboundp 'remote-canonicalize-emacs-file-name)
                             (remote-canonicalize-emacs-file-name directory) directory)
              :native-session-id (map-nested-elt state '(:session :id))
              :session-id (run-hook-with-args-until-success '+agent-session-id-functions buffer)
              :process-state (cond ((not process) 'starting)
                                   ((and (processp process) (process-live-p process)) 'live)
                                   (t 'dead))
              :busy (and (+agent-acp-busy-p buffer) t)
              :attention-kind attention :unread-kind +agent-acp-attention :last-error +agent-acp-last-error :usage (+agent-acp-usage buffer)
              :capabilities (list :list (map-elt state :supports-session-list)
                                  :load (map-elt state :supports-session-load)
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
      (run-at-time
       0 nil
       (lambda ()
         (when (and (buffer-live-p buffer)
                    (+agent-acp-agent-buffer-p buffer)
                    (not (buffer-local-value '+agent-acp-session-origin buffer))
                    (not (buffer-local-value '+agent-acp-session-name buffer)))
           (condition-case error-object
               (+agent-acp-adopt buffer :origin 'foreign)
             (error (message "Noema could not register this agent session: %s"
                             (error-message-string error-object))))))))))

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

(cl-defun +agent-acp-start (&key config directory session-id fork-session-id focus
                                      (origin 'manual) (render-policy 'visible)
                                      display-function)
  "Start CONFIG in DIRECTORY, optionally resuming or forking a native session.
The buffer is displayed only when FOCUS is non-nil (D-033): a document Run
never pops its agent buffer; the person opens it on purpose.

ORIGIN records which entry point owns the session, so the project-wide
registry can list it and `+agent-acp--adopt-foreign-h' leaves it alone.

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
  "Subscribe CALLBACK to EVENT in agent-shell BUFFER."
  (agent-shell-subscribe-to :shell-buffer buffer :event event :on-event callback))

(cl-defun +agent-acp-unsubscribe (&key buffer subscription)
  "Remove SUBSCRIPTION from agent-shell BUFFER when both are still live."
  (when (and subscription (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell-unsubscribe :subscription subscription)))))

(defun +agent-acp-show-buffer (buffer)
  "Show agent BUFFER in its project Agent window and hydrate deferred output."
  (unless (+agent-acp-agent-buffer-p buffer)
    (user-error "No live agent buffer"))
  (if-let* ((display (buffer-local-value '+agent-acp-display-buffer-function buffer)))
      (funcall display buffer)
    (+agent-acp--install-workspace-tabs buffer)
    (+agent-acp--display-workspace-buffer buffer))
  (+agent-acp-touch buffer)
  (+agent-acp-clear-attention buffer)
  (+agent-render-flush buffer)
  buffer)

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
  (when (seq-some (lambda (block) (equal (map-elt block 'type) "text")) content)
    (agent-shell--update-fragment
     :state state :namespace-id (format "noema-run-%s" (or run-id (map-elt state :request-count)))
     :block-id "submitted-input" :label-left "You"
     :body (mapconcat
            (lambda (block)
              (if (equal (map-elt block 'type) "text")
                  (or (map-elt block 'text) "")
                (format "[Context: %s]"
                        (or (map-nested-elt block '(resource uri))
                            ;; A `resource_link' carries its uri at top level.
                            (map-elt block 'uri)
                            (map-elt block 'type)))))
            content "\n\n")
     :expanded t :above-last-prompt t)))

(defun +agent-acp--finish-prompt (buffer receipt)
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
                             (t "Completed"))
           :above-last-prompt (and (not +agent-render--pending)
                                   (+agent-acp--live-input-p)))
        (+agent-acp-restore-prompt buffer)))))

(cl-defun +agent-acp-prompt (&key buffer content run-id on-success on-failure)
  "Send structured CONTENT through BUFFER's initialized ACP session."
  (with-current-buffer buffer
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
                     (unwind-protect
                         (when on-success (funcall on-success response))
                       (+agent-acp--finish-prompt buffer receipt)))
       :on-failure (lambda (error raw)
                     (setf (plist-get receipt :status) 'failed
                           (plist-get receipt :error) raw)
                     (unwind-protect
                         (when on-failure (funcall on-failure error raw))
                       (+agent-acp--finish-prompt buffer receipt)))))))

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
(defun +agent/start (&optional agent)
  "Start a reusable agent session in the current workspace."
  (interactive
   (list (intern (completing-read "Agent: " '("codex" "claude" "opencode" "pi" "magent") nil t nil nil "codex"))))
  (let* ((agent (or agent 'codex)) (root (+agent-acp-project-root))
         (directory (+agent-workspace root))
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
