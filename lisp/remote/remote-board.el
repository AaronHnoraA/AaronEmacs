;;; remote-board.el --- Target, route, and health observability -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The board displays logical targets.  Link implementations are shown as
;; route state, never as duplicate target entries.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-config)
(require 'remote-doctor)
(require 'remote-session)
(require 'remote-workspace)
(require 'remote-channel)
(require 'remote-task)

(declare-function remote-backend-tramp-ssh-client-command
                  "remote-backend-tramp" (route &rest options))
(declare-function make-term "term" (name program &optional startfile &rest switches))
(declare-function term-char-mode "term" ())

(defvar remote-target-history nil)
(defvar savehist-additional-variables)

(defcustom remote-board-recent-folder-limit 40
  "Maximum number of explicitly opened Remote folders to remember."
  :type 'integer
  :group 'remote)

(defvar remote-board-recent-folders nil
  "Most recently opened canonical `/fs:' folders, newest first.
Entries are recorded only after an explicit Remote folder open succeeds.
The board never probes these paths while rendering; a missing folder is
reported when the user tries to open it again.")

(defvar remote-board--refresh-timer nil
  "Pending idle refresh of the visible Remote board, if any.")

(defvar remote-board--opening-folders (make-hash-table :test #'equal)
  "In-flight folder opens by logical file name; values are nesting counts.")

(defvar remote-board--opening-targets (make-hash-table :test #'equal)
  "In-flight folder opens by target ID; values are nesting counts.")

(defvar remote-board-connection-progress (make-hash-table :test #'equal)
  "Current connection phase by target ID, without target-side polling.")

(defvar remote-board-connection-history (make-hash-table :test #'equal)
  "Recent connection phases by target ID for the board's output view.")

(defcustom remote-board-connection-history-limit 32
  "Maximum recent connection phase entries retained per target."
  :type 'integer
  :group 'remote)

(defvar remote-board-ssh-statuses (make-hash-table :test #'equal)
  "Last SSH diagnostic state by target ID; board rendering never probes it.")

(defvar remote-board--ssh-probe-processes (make-hash-table :test #'equal)
  "Running client SSH diagnostic processes by target ID.")

(defvar remote-board--ssh-status-generation 0
  "Incremented when configuration changes to reject old probe results.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables
               'remote-board-recent-folders))

(defun remote-board--remember-folder (folder)
  "Remember canonical FOLDER without contacting its target."
  (when (and (stringp folder)
             (string-match remote-fs-canonical-regexp folder))
    (let ((canonical
           (file-name-as-directory
            (remote-make-file-name
             (match-string 1 folder) (match-string 2 folder)))))
      (setq remote-board-recent-folders
            (cons canonical (delete canonical remote-board-recent-folders)))
      (if (<= remote-board-recent-folder-limit 0)
          (setq remote-board-recent-folders nil)
        (when (> (length remote-board-recent-folders)
                 remote-board-recent-folder-limit)
          (setcdr (nthcdr (1- remote-board-recent-folder-limit)
                          remote-board-recent-folders)
                  nil)))
      canonical)))

(defun remote-board--folder-name (path)
  "Return a short, nonempty display name for target-native PATH."
  (let ((name (file-name-nondirectory (directory-file-name path))))
    (if (string-empty-p name) "/" name)))

(defun remote-target-list ()
  "Return registered targets sorted by label."
  (let (targets)
    (maphash (lambda (_id target) (push target targets)) remote-targets)
    (sort targets
          (lambda (left right)
            (string-lessp
             (remote-target-label left)
             (remote-target-label right))))))

(defun remote-read-target (&optional prompt omit-local)
  "Read and return a target.
PROMPT customizes the minibuffer prompt.  OMIT-LOCAL excludes `local'."
  (let (table)
    (dolist (target (remote-target-list))
      (unless (and omit-local
                   (equal (remote-target-id target) "local"))
        (push (cons
               (format "%-24s %s"
                       (remote-target-label target)
                       (remote-target-id target))
               target)
              table)))
    (cdr
     (assoc
      (completing-read
       (or prompt "Target: ") table nil t nil 'remote-target-history)
      table))))

(defun remote--route-label (target capability adapter)
  "Return compact route label for TARGET, CAPABILITY, and ADAPTER."
  (let* ((file-name
          (remote-make-file-name (remote-target-id target) "/"))
         (context (remote-context file-name))
         (route (car (remote-routes adapter capability context))))
    (if route
        (let* ((link (remote-route-link route))
               (health
                (or (remote-route-backend-health route)
                    (remote-link-health link capability))))
          (format "%s:%s%s"
                  (remote-route-link-plugin-id route)
                  (remote-link-short-id link)
                  (if (eq (plist-get health :status) 'failed)
                      " !"
                    "")))
      "unavailable")))

(defun remote-board--ssh-status-label (state)
  "Return a compact board label for SSH diagnostic STATE."
  (pcase state
    ('checking "checking SSH")
    ('ready "SSH ready")
    ('authentication "auth required")
    ('host-key "host key")
    ('network "network error")
    ('name "unknown host")
    (_ "SSH error")))

(defun remote-board--ssh-classify-result (status output)
  "Classify OpenSSH exit STATUS and OUTPUT for the Remote board."
  (let ((case-fold-search t))
    (cond
     ((zerop status) 'ready)
     ((string-match-p
       "host key verification failed\\|remote host identification has changed\\|possible dns spoofing"
       output)
      'host-key)
     ((string-match-p
       "permission denied\\|authentication failed\\|too many authentication failures\\|no more authentication methods"
       output)
      'authentication)
     ((string-match-p
       "could not resolve hostname\\|name or service not known\\|nodename nor servname provided"
       output)
      'name)
     ((string-match-p
       "connection timed out\\|operation timed out\\|connection refused\\|no route to host\\|network is unreachable"
       output)
      'network)
     (t 'failed))))

(defun remote-board--ssh-status-set (target-id state detail)
  "Cache TARGET-ID's SSH STATE and DETAIL without network I/O."
  (puthash target-id
           (list :state state :detail detail :time (current-time))
           remote-board-ssh-statuses)
  (remote-board--schedule-status-refresh))

(defun remote-board--record-connection-failure (connection route reason)
  "Remember a failed SSH CONNECTION on ROUTE with its REASON."
  (when (and (remote-route-p route)
             (member (remote-route-link-plugin-id route)
                     '("tramp" "tramp-rpc"))
             (eq (remote-connection-state connection) 'failed))
    (let* ((error-data (or (remote-connection-error connection) reason))
           (detail
            (condition-case nil
                (error-message-string error-data)
              (error (format "%s" error-data)))))
      (remote-board--ssh-status-set
       (remote-route-target-id route)
       (remote-board--ssh-classify-result 255 detail)
       detail))))

(defun remote-board--clear-connection-failure (_connection route)
  "Clear stale SSH errors after ROUTE connects successfully."
  (when (and (remote-route-p route)
             (member (remote-route-link-plugin-id route)
                     '("tramp" "tramp-rpc")))
    (remhash (remote-route-target-id route) remote-board-ssh-statuses)
    (remote-board--schedule-status-refresh)))

(defun remote-board--reset-ssh-status (&rest _ignored)
  "Discard SSH probe results after a configuration reload."
  (cl-incf remote-board--ssh-status-generation)
  (let ((processes (hash-table-values remote-board--ssh-probe-processes)))
    (clrhash remote-board--ssh-probe-processes)
    (dolist (process processes)
      (when (process-live-p process)
        (delete-process process))))
  (clrhash remote-board-ssh-statuses)
  (clrhash remote-board-connection-progress)
  (remote-board--schedule-status-refresh))

(defun remote-board--connection-phase-label (phase backend)
  "Return a short board label for connection PHASE and BACKEND."
  (pcase phase
    ('transport "opening route")
    ('backend
     (if (member backend '("tramp" "tramp-rpc"))
         "SSH login"
       "starting backend"))
    ('probe "checking server")
    ('ready "connected")
    ('cancelled "cancelled")
    ('failed "failed")
    (_ "connecting")))

(defun remote-board--connection-progress (connection route phase)
  "Record CONNECTION's PHASE on ROUTE for board state and output."
  (let* ((target-id (remote-route-target-id route))
         (generation (remote-connection-generation connection))
         (current (gethash target-id remote-board-connection-progress)))
    (unless (equal target-id "local")
      (let* ((entry
              (list :time (current-time)
                    :generation generation
                    :phase phase
                    :pipeline (remote-route-link-id route)
                    :backend (remote-route-link-plugin-id route)
                    :error (and (eq phase 'failed)
                                (remote-connection-error connection))))
             (history (cons entry
                            (gethash target-id
                                     remote-board-connection-history))))
        (puthash
         target-id
         (if (> remote-board-connection-history-limit 0)
             (seq-take history remote-board-connection-history-limit)
           nil)
         remote-board-connection-history)
        (cond
         ((memq phase '(ready failed cancelled))
          (when (and current
                     (equal (plist-get current :generation) generation))
            (remhash target-id remote-board-connection-progress)))
         ((or (null current)
              (>= generation (plist-get current :generation)))
          (puthash target-id entry remote-board-connection-progress)))
        (remote-board--opening-refresh)))))

(add-hook 'remote-connection-closed-hook
          #'remote-board--record-connection-failure)
(add-hook 'remote-connection-opened-hook
          #'remote-board--clear-connection-failure)
(add-hook 'remote-connection-progress-hook
          #'remote-board--connection-progress)
(add-hook 'remote-config-after-load-hook
          #'remote-board--reset-ssh-status)

(defun remote-board--target-state (target-id sessions workspaces)
  "Summarize cached SESSIONS and WORKSPACES for TARGET-ID without I/O."
  (let ((session-states
         (mapcar
          (lambda (session) (plist-get session :state))
          (seq-filter
           (lambda (session)
             (equal (plist-get session :target) target-id))
           sessions)))
        (workspace-states
         (mapcar
          (lambda (workspace) (plist-get workspace :state))
          (seq-filter
           (lambda (workspace)
             (equal (plist-get workspace :target) target-id))
           workspaces)))
        (ssh-status (gethash target-id remote-board-ssh-statuses))
        (progress (gethash target-id remote-board-connection-progress)))
    (cond
     (progress
      (remote-board--connection-phase-label
       (plist-get progress :phase)
       (plist-get progress :backend)))
     ((gethash target-id remote-board--opening-targets) "opening folder")
     ((memq 'reconnecting workspace-states) "reconnecting")
     ((memq 'open workspace-states) "workspace open")
     ((memq 'opening session-states) "connecting")
     ((seq-some (lambda (state)
                  (memq state '(failed degraded disconnected)))
                workspace-states)
      (if (and ssh-status
               (memq (plist-get ssh-status :state)
                     '(authentication host-key network name failed)))
          (remote-board--ssh-status-label (plist-get ssh-status :state))
        "attention"))
     ((memq 'open session-states) "session open")
     (ssh-status
      (remote-board--ssh-status-label (plist-get ssh-status :state)))
     ((equal target-id "local") "local")
     (t "idle"))))

(defun remote-board--state-cell (target-id sessions workspaces)
  "Return TARGET-ID's State cell with cached SSH detail on hover."
  (let* ((state (remote-board--target-state
                 target-id sessions workspaces))
         (progress (gethash target-id remote-board-connection-progress))
         (detail
          (if progress
              (format "Connecting via %s/%s; L: connection log"
                      (plist-get progress :pipeline)
                      (plist-get progress :backend))
            (plist-get (gethash target-id remote-board-ssh-statuses)
                       :detail))))
    (if (and (stringp detail) (not (string-empty-p detail)))
        (propertize
         state 'help-echo
         (concat
          (truncate-string-to-width
           (replace-regexp-in-string "[\r\n]+" " " detail)
           220 nil nil "…")
          "\nD: diagnose SSH  T: open SSH login terminal"))
      state)))

(defun remote-board--folder-state (logical workspaces origin)
  "Return the cached state for LOGICAL folder, or its ORIGIN label."
  (or (and logical
           (gethash logical remote-board--opening-folders)
           "opening")
      (when logical
        (when-let* ((workspace
                     (seq-find
                      (lambda (item)
                        (equal (plist-get item :root)
                               (file-name-as-directory logical)))
                      workspaces)))
          (symbol-name (plist-get workspace :state))))
      (symbol-name origin)))

(defun remote-board--folder-row
    (target path logical label origin workspaces)
  "Build one folder row for TARGET and PATH without probing it.
LOGICAL is its known canonical name, LABEL is the displayed folder name, and
ORIGIN is `configured', `active', or `recent'."
  (let ((target-id (remote-target-id target)))
    (list
     (list 'folder target-id path)
     (vector
      (concat "  ↳ " label)
      ""
      (remote-board--folder-state logical workspaces origin)
      (if logical (remote-fs-localname logical) path)
      "" "" "" ""))))

(defun remote-board--folder-rows (target workspaces)
  "Return configured, active, and remembered folder rows for TARGET.
All data comes from local registries and savehist.  No target filesystem or
connection is consulted while the board is drawn."
  (let ((target-id (remote-target-id target))
        (seen (make-hash-table :test #'equal))
        rows)
    (dolist (workspace (remote-target-workspaces target))
      (when-let* ((path (remote-fs--workspace-property workspace 'path))
                  ((stringp path)))
        (let* ((logical
                (when (and (file-name-absolute-p path)
                           (not (string-prefix-p "~" path)))
                  (file-name-as-directory
                   (remote-make-file-name target-id path))))
               (label
                (or (remote-fs--workspace-property workspace 'id)
                    (remote-board--folder-name path)
                    path)))
          (unless (gethash (or logical path) seen)
            (puthash (or logical path) t seen)
            (push (remote-board--folder-row
                   target path logical (format "%s" label)
                   'configured workspaces)
                  rows)))))
    (dolist (workspace workspaces)
      (when (equal (plist-get workspace :target) target-id)
        (let* ((logical (plist-get workspace :root))
               (path (and logical (remote-fs-localname logical))))
          (when (and path (not (gethash logical seen)))
            (puthash logical t seen)
            (push (remote-board--folder-row
                   target logical logical
                   (remote-board--folder-name path)
                   'active workspaces)
                  rows)))))
    (dolist (logical remote-board-recent-folders)
      (when (and (stringp logical)
                 (string-match remote-fs-canonical-regexp logical)
                 (equal (match-string 1 logical) target-id)
                 (not (gethash logical seen)))
        (puthash logical t seen)
        (let ((path (remote-fs-localname logical)))
          (push (remote-board--folder-row
                 target logical logical
                 (remote-board--folder-name path)
                 'recent workspaces)
                rows))))
    (nreverse rows)))

(defun remote-board--endpoint-text (endpoint)
  "Return a readable host and port for ENDPOINT without probing it."
  (if (and (listp endpoint) (plist-member endpoint :port))
      (format "%s:%s"
              (or (plist-get endpoint :host) "127.0.0.1")
              (plist-get endpoint :port))
    (format "%s" (or endpoint "?"))))

(defun remote-board--forward-rows (target channels)
  "Return live forward rows for TARGET from cached CHANNELS."
  (let ((target-id (remote-target-id target))
        rows)
    (dolist (channel channels)
      (when (and (equal (plist-get channel :target) target-id)
                 (eq (plist-get channel :kind) 'forward))
        (let* ((local (plist-get channel :local-endpoint))
               (remote (plist-get channel :remote-endpoint))
               (name (plist-get (plist-get channel :metadata) :name))
               (direction (plist-get (plist-get channel :metadata)
                                     :direction)))
          (push
           (list
            (list 'forward target-id (plist-get channel :id))
            (vector
             (format "  ⇢ %s"
                     (if (and (stringp name) (not (string-empty-p name)))
                         (format "%s (%s)" name
                                 (or (plist-get remote :port) "?"))
                       (format "port %s" (or (plist-get remote :port) "?"))))
             ""
             (symbol-name (or (plist-get channel :state) 'unknown))
             (format "%s %s → %s"
                     (if (eq direction 'reverse) "reverse" "forward")
                     (remote-board--endpoint-text remote)
                     (remote-board--endpoint-text local))
             "" "" "" ""))
           rows))))
    (nreverse rows)))

(defun remote-board--entries ()
  "Return `tabulated-list-entries' for the current registry."
  (let ((sessions (remote-session-list))
        (workspaces (remote-workspace-list))
        (channels (remote-channel-list))
        entries)
    (dolist (target (remote-target-list))
      (let* ((id (remote-target-id target))
             (workspace-count
              (seq-count
               (lambda (workspace)
                 (equal (plist-get workspace :target) id))
               workspaces)))
        (push
         (list
          id
          (vector
           (remote-target-label target)
           id
           (remote-board--state-cell id sessions workspaces)
           ""
           (number-to-string workspace-count)
           (remote--route-label target 'file-read "emacs-file")
           (remote--route-label target 'process-async "process")
           (if (remote-target-trusted target) "trusted" "untrusted")))
         entries)
        (dolist (row (remote-board--folder-rows target workspaces))
          (push row entries))
        (dolist (row (remote-board--forward-rows target channels))
          (push row entries))))
    (nreverse entries)))

(defun remote-board-refresh-status ()
  "Refresh routes and cached lifecycle state without reloading configuration."
  (interactive)
  (when (derived-mode-p 'remote-board-mode)
    (setq tabulated-list-entries (remote-board--entries))
    (tabulated-list-print t)))

(defun remote-board--refresh-if-open ()
  "Refresh the board after lifecycle hooks have finished mutating state."
  (setq remote-board--refresh-timer nil)
  (when-let* ((board (get-buffer "*Remote*")))
    (when (get-buffer-window board t)
      (with-current-buffer board
        (when (derived-mode-p 'remote-board-mode)
          (remote-board-refresh-status))))))

(defun remote-board--schedule-status-refresh (&rest _ignored)
  "Coalesce local board refreshes after connection or workspace changes."
  (when (and (get-buffer-window "*Remote*" t)
             (not remote-board--refresh-timer))
    (setq remote-board--refresh-timer
          (run-with-idle-timer 0.1 nil
                               #'remote-board--refresh-if-open))))

(add-hook 'remote-connection-opened-hook
          #'remote-board--schedule-status-refresh)
(add-hook 'remote-connection-closed-hook
          #'remote-board--schedule-status-refresh)
(add-hook 'remote-workspace-open-hook
          #'remote-board--schedule-status-refresh)
(add-hook 'remote-workspace-close-hook
          #'remote-board--schedule-status-refresh)
(add-hook 'remote-channel-opened-hook
          #'remote-board--schedule-status-refresh)
(add-hook 'remote-channel-closed-hook
          #'remote-board--schedule-status-refresh)

(defun remote-board-refresh ()
  "Reload configuration and refresh the board."
  (interactive)
  (remote-config-reload)
  (remote-board-refresh-status))

(defun remote-board-target-at-point ()
  "Return target represented by the current row."
  (let ((id (tabulated-list-get-id)))
    (or (remote-get-target
         (if (consp id)
             (nth 1 id)
           id))
        (user-error "No target on this row"))))

(defun remote-board--folder-at-point ()
  "Return the selected board row's folder spelling, or nil."
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'folder))
      (nth 2 id))))

(defun remote-board--forward-at-point ()
  "Return the selected live forward channel, or nil."
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'forward))
      (gethash (nth 2 id) remote-channels))))

(defun remote-board--forward-address (channel)
  "Return CHANNEL's client-side host and port as a string."
  (remote-board--endpoint-text
   (remote-channel-endpoint channel 'local)))

(defun remote-board-forward-port (target port &optional host local-port name)
  "Forward PORT on TARGET's HOST to client loopback LOCAL-PORT.
The forward belongs to an open workspace so it can recover after transport
loss.  Zero or nil LOCAL-PORT allocates a free port.  A prefix argument
prompts for HOST; two prefix arguments also prompt for LOCAL-PORT."
  (interactive
   (let ((target
          (if (derived-mode-p 'remote-board-mode)
              (remote-board-target-at-point)
            (remote-read-target "Forward port on target: "))))
     (list target
           (read-number "Target port: ")
           (if current-prefix-arg
               (read-string "Target host: " "127.0.0.1")
             "127.0.0.1")
           (if (and current-prefix-arg
                    (>= (prefix-numeric-value current-prefix-arg) 16))
               (read-number "Client port (0 = automatic): " 0)
             0))))
  (unless (and (integerp port) (<= 1 port 65535))
    (user-error "Target port must be between 1 and 65535"))
  (unless (or (null local-port)
              (and (integerp local-port) (<= 0 local-port 65535)))
    (user-error "Client port must be between 0 and 65535"))
  (unless (or (null name) (stringp name))
    (user-error "Forward name must be a string"))
  (let* ((host (if (and (stringp host) (not (string-empty-p host)))
                   host
                 "127.0.0.1"))
         (target (if (remote-target-p target)
                     target
                   (or (remote-get-target target)
                       (error "Unknown target: %S" target))))
         (folder (and (derived-mode-p 'remote-board-mode)
                      (remote-board--folder-at-point)))
         (logical (if folder
                      (remote-board--folder-file-name target folder)
                    (remote-make-file-name
                     (remote-target-id target) "/")))
         (existing
          (or (remote-workspace-for-path logical)
              (and (null folder)
                   (seq-find
                    (lambda (workspace)
                      (and (equal (remote-workspace-target-id workspace)
                                  (remote-target-id target))
                           (remote-workspace-live-p workspace)))
                    (hash-table-values remote-workspaces)))))
         (workspace
          (or existing (remote-workspace-open logical :connect t)))
         (forward
          (condition-case error
              (remote-port-forward
               (list :host host :port port)
               :local-endpoint
               (list :host "127.0.0.1" :port (or local-port 0))
               :context (remote-workspace-context workspace)
               :workspace workspace
               :metadata (list :source 'remote-board :name name))
            (error
             (unless existing
               (remote-workspace-close workspace 'forward-failed))
             (signal (car error) (cdr error))))))
    (remote-board-refresh-status)
    (message "Forwarded %s:%d to %s"
             host port
             (remote-board--forward-address forward))
    forward))

(defun remote-board--forward-owner (forward)
  "Return (WORKSPACE . RESOURCE) for workspace-owned FORWARD, or nil."
  (catch 'found
    (maphash
     (lambda (_key workspace)
       (when-let* ((resource
                    (seq-find
                     (lambda (item)
                       (and (eq (remote-workspace-resource-kind item)
                                'forward)
                            (eq (remote-workspace-resource-value item)
                                forward)))
                     (remote-workspace-resources workspace))))
         (throw 'found (cons workspace resource))))
     remote-workspaces)
    nil))

(defun remote-board--close-forward (channel reason)
  "Close CHANNEL and its owned resource for REASON."
  (let* ((forward (remote-channel-handle channel))
         (owner (remote-board--forward-owner forward)))
    (if owner
        (remote-workspace-close-resource (car owner) (cdr owner) reason)
      (remote-close-channel channel))))

(defun remote-board-close-forward ()
  "Close the selected forward and remove its workspace recovery resource."
  (interactive)
  (let ((channel
         (or (remote-board--forward-at-point)
             (user-error "Select a forwarded port row"))))
    (remote-board--close-forward channel 'user-close)
    (remote-board-refresh-status)
    (message "Closed forwarded port")))

(defun remote-board-rename-forward (name)
  "Give the selected board-owned forward a display NAME.
The name is retained when the workspace recovers its channel."
  (interactive
   (let ((channel
          (or (remote-board--forward-at-point)
              (user-error "Select a forwarded port row"))))
     (list (read-string
            "Forward name (empty clears): "
            (or (plist-get (remote-channel-metadata channel) :name) "")))))
  (let ((channel
         (or (remote-board--forward-at-point)
             (user-error "Select a forwarded port row"))))
    (unless (stringp name)
      (user-error "Forward name must be a string"))
    (unless (eq (plist-get (remote-channel-metadata channel) :source)
                'remote-board)
      (user-error "This forward is managed outside the Remote board"))
    ;; Mutate the shared metadata list, including channels opened before
    ;; this command existed.  The recovery closure retains the same head.
    (let* ((metadata (remote-channel-metadata channel))
           (name-cell (memq :name metadata))
           (value (and (not (string-empty-p name)) name)))
      (if name-cell
          (setcar (cdr name-cell) value)
        (setcdr (last metadata) (list :name value))))
    (remote-board-refresh-status)
    (message "Forward name %s" (or (plist-get
                                     (remote-channel-metadata channel) :name)
                                    "cleared"))))

(defun remote-board-change-local-port (port)
  "Move the selected board forward to client loopback PORT.
The old listener remains open if the requested PORT cannot be allocated.
PORT zero chooses a new free port."
  (interactive
   (let ((channel
          (or (remote-board--forward-at-point)
              (user-error "Select a forwarded port row"))))
     (list
      (read-number
       "New client port (0 = automatic): "
       (plist-get (remote-channel-endpoint channel 'local) :port)))))
  (unless (and (integerp port) (<= 0 port 65535))
    (user-error "Client port must be between 0 and 65535"))
  (let* ((channel
          (or (remote-board--forward-at-point)
              (user-error "Select a forwarded port row")))
         (metadata (remote-channel-metadata channel))
         (route (remote-channel-route channel))
         (old-local (remote-channel-endpoint channel 'local))
         (remote-endpoint (remote-channel-endpoint channel 'remote))
         (owner (remote-board--forward-owner
                 (remote-channel-handle channel))))
    (unless (and (eq (plist-get metadata :source) 'remote-board)
                 (eq (plist-get metadata :direction) 'local)
                 (remote-channel-live-p channel))
      (user-error "Select a live board-owned forward port"))
    (when (and (> port 0) (= port (plist-get old-local :port)))
      (user-error "Forward already uses client port %d" port))
    (let* ((replacement
            (remote-port-forward
             remote-endpoint
             :local-endpoint
             (plist-put (copy-sequence old-local) :port port)
             :context (remote-channel-context channel)
             :adapter (remote-route-adapter-id route)
             :pipeline (remote-route-pipeline-id route)
             :metadata (copy-sequence metadata)
             :workspace (car owner)))
           (new-channel (remote-channel-of replacement))
           (actual (plist-get (remote-channel-endpoint replacement 'local)
                              :port)))
      (unless (and (integerp actual)
                   (or (zerop port) (= actual port)))
        (remote-board--close-forward new-channel 'port-mismatch)
        (error "Backend did not bind the requested client port %d" port))
      (remote-board--close-forward channel 'port-changed)
      (remote-board-refresh-status)
      (message "Forward now listens on %s"
               (remote-board--forward-address new-channel))
      replacement)))

(defun remote-board--folder-file-name (target folder)
  "Return FOLDER's logical identity on TARGET."
  (if (and (stringp folder)
           (string-match remote-fs-canonical-regexp folder))
      (if (equal (match-string 1 folder) (remote-target-id target))
          folder
        (error "Folder belongs to another target: %s" folder))
    (remote-target-file-name target folder)))

(defun remote-board--opening-refresh ()
  "Redraw a visible board after a local folder-open state change."
  (when-let* ((board (get-buffer "*Remote*"))
              ((get-buffer-window board t)))
    (with-current-buffer board
      (remote-board-refresh-status))
    (unless noninteractive
      (redisplay))))

(defun remote-board--opening-update (target-id logical delta)
  "Adjust TARGET-ID and LOGICAL opening counts by DELTA."
  (dolist (entry `((,remote-board--opening-targets . ,target-id)
                   (,remote-board--opening-folders . ,logical)))
    (let* ((table (car entry))
           (key (cdr entry))
           (count (+ delta (gethash key table 0))))
      (if (> count 0)
          (puthash key count table)
        (remhash key table))))
  ;; Rendering is only progress UI.  A transient window/Dirvish error must
  ;; neither abort a file open nor strand its in-flight status.
  (condition-case nil
      (remote-board--opening-refresh)
    ((error quit) nil)))

(defun remote-open-folder (target folder)
  "Open existing FOLDER on TARGET as a managed Remote workspace."
  (interactive
   (let* ((target
           (if (derived-mode-p 'remote-board-mode)
               (remote-board-target-at-point)
             (remote-read-target "Open folder on target: ")))
          (initial
           (or (and (derived-mode-p 'remote-board-mode)
                    (remote-board--folder-at-point))
               (remote-target-default-localname target)))
          (initial-logical
           (remote-board--folder-file-name target initial)))
     (list target
           (read-directory-name
            (format "%s folder: " (remote-target-label target))
            initial-logical initial-logical nil initial-logical))))
  (let* ((target (if (remote-target-p target)
                     target
                   (or (remote-get-target target)
                       (error "Unknown target: %S" target))))
         (logical
          (file-name-as-directory
           (remote-board--folder-file-name target folder))))
    (remote-board--opening-update (remote-target-id target) logical 1)
    (unwind-protect
        (progn
          (unless (file-directory-p logical)
            (user-error "Folder is unavailable on %s: %s"
                        (remote-target-label target)
                        (remote-fs-localname logical)))
          (let* ((existing (remote-get-workspace logical))
                 (workspace
                  (remote-workspace-open
                   logical :connect t :adapter "emacs-file"
                   :capability 'file-read))
                 opened)
            (unwind-protect
                (prog1 (find-file logical)
                  (setq opened t)
                  (remote-board--remember-folder logical))
              (unless (or opened existing)
                (remote-workspace-close workspace 'folder-open-failed)))))
      (remote-board--opening-update (remote-target-id target) logical -1))))

(defun remote-open-target (&optional target)
  "Open TARGET's folder, or copy a selected forward's local address."
  (interactive
   (list (unless (derived-mode-p 'remote-board-mode)
           (remote-read-target "Open target: "))))
  (if (and (derived-mode-p 'remote-board-mode)
           (remote-board--forward-at-point))
      (remote-copy-target-uri target)
    (let* ((target (or target (remote-board-target-at-point)))
           (target (if (remote-target-p target)
                       target
                     (or (remote-get-target target)
                         (error "Unknown target: %S" target)))))
      (remote-open-folder
       target
       (or (and (derived-mode-p 'remote-board-mode)
                (remote-board--folder-at-point))
           (remote-target-default-localname target))))))

(defun remote-copy-target-uri (&optional target)
  "Copy TARGET's folder URI or selected forward's local address."
  (interactive
   (list (unless (derived-mode-p 'remote-board-mode)
           (remote-read-target "Copy target URI: "))))
  (let ((uri
         (if-let* ((forward
                    (and (derived-mode-p 'remote-board-mode)
                         (remote-board--forward-at-point))))
             (remote-board--forward-address forward)
           (remote-file-name-to-uri
            (let* ((target (or target (remote-board-target-at-point)))
                   (target (if (remote-target-p target)
                               target
                             (or (remote-get-target target)
                                 (error "Unknown target: %S" target)))))
              (remote-board--folder-file-name
               target
               (or (and (derived-mode-p 'remote-board-mode)
                        (remote-board--folder-at-point))
                   (remote-target-default-localname target))))))))
    (kill-new uri)
    (message "Copied %s" uri)))

(defun remote-board-reconnect-workspace ()
  "Schedule recovery for the selected workspace.
An open workspace requires a prefix argument to force a new transport."
  (interactive)
  (unless (derived-mode-p 'remote-board-mode)
    (user-error "Open the Remote board first"))
  (let* ((target (remote-board-target-at-point))
         (folder (or (remote-board--folder-at-point)
                     (user-error "Select a workspace folder row")))
         (logical (remote-board--folder-file-name target folder))
         (workspace
          (or (remote-get-workspace logical)
              (remote-workspace-for-path logical)
              (user-error "No open workspace for %s" logical)))
         (board (current-buffer))
         (name (remote-workspace-id workspace)))
    (remote-workspace-reconnect-async
     workspace
     :force current-prefix-arg
     :callback
     (lambda (_reopened)
       (when (buffer-live-p board)
         (with-current-buffer board
           (remote-board-refresh-status)))
       (message "Workspace %s recovery: %s"
                name (remote-workspace-state workspace)))
     :error-callback
     (lambda (error)
       (when (buffer-live-p board)
         (with-current-buffer board
           (remote-board-refresh-status)))
       (message "Reconnect failed for %s: %s"
                name (error-message-string error))))
    (remote-board-refresh-status)
    (message "Reconnecting %s..." name)))

(defun remote-board-close-workspace ()
  "Close the workspace on the selected folder row and its owned resources."
  (interactive)
  (unless (derived-mode-p 'remote-board-mode)
    (user-error "Open the Remote board first"))
  (let* ((target (remote-board-target-at-point))
         (folder (or (remote-board--folder-at-point)
                     (user-error "Select a workspace folder row")))
         (logical (remote-board--folder-file-name target folder))
         (workspace
          (or (remote-get-workspace logical)
              (remote-workspace-for-path logical)
              (user-error "No open workspace for %s" logical))))
    (remote-workspace-close workspace 'user)
    (remote-board-refresh-status)
    (message "Closed workspace %s" (remote-workspace-id workspace))
    workspace))

(defun remote-board-run-command ()
  "Run a build or test command in the selected workspace folder."
  (interactive)
  (unless (derived-mode-p 'remote-board-mode)
    (user-error "Open the Remote board first"))
  (let* ((target (remote-board-target-at-point))
         (folder (or (remote-board--folder-at-point)
                     (user-error "Select a workspace folder row")))
         (logical (remote-board--folder-file-name target folder)))
    (remote-task-run-command logical)))

(defun remote-board-disconnect-target (&optional target)
  "Close TARGET's resources, sessions and buffers, preserving unsaved work."
  (interactive
   (list (if (derived-mode-p 'remote-board-mode)
             (remote-board-target-at-point)
           (remote-read-target "Disconnect target: "))))
  (let* ((target (if (remote-target-p target)
                     target
                   (or (remote-get-target target)
                       (error "Unknown target: %S" target))))
         (target-id (remote-target-id target))
         (result (remote-workspace-disconnect-target
                  target-id 'target-disconnect))
         (workspaces (plist-get result :workspaces))
         (sessions (plist-get result :sessions)))
    (remhash target-id remote-board-ssh-statuses)
    (remote-board-refresh-status)
    (message "Disconnected %s (%d workspace%s, %d session%s, %d buffers closed)%s"
             (remote-target-label target)
             workspaces (if (= workspaces 1) "" "s")
             sessions (if (= sessions 1) "" "s")
             (plist-get result :buffers)
             (if-let* ((kept (plist-get result :kept-buffers)))
                 (format "; kept unsaved/vetoed: %s" (string-join kept ", "))
               ""))
    result))

(defun remote-edit-config ()
  "Visit `remote-config-file'."
  (interactive)
  (find-file remote-config-file))

(defun remote-board--ssh-host-field (label value &optional alias)
  "Validate SSH config VALUE for LABEL without allowing a second directive.
ALIAS requires the concrete Host spelling accepted by Remote's SSH importer."
  (unless (and (stringp value)
               (not (string-empty-p value))
               (if alias
                   (string-match-p
                    "\\`[[:alnum:]][[:alnum:]._-]*\\'" value)
                 (not (string-match-p "[[:space:]#\"'\\\\]" value))))
    (user-error "Invalid SSH %s: %S" label value))
  value)

(defun remote-board--ssh-config-value (label value)
  "Return VALUE quoted for one OpenSSH config field named LABEL.
Shell syntax in a pasted SSH command is parsed as data, never executed."
  (unless (and (stringp value)
               (not (string-empty-p value))
               (not (string-match-p "[[:cntrl:]]" value)))
    (user-error "Invalid SSH %s: %S" label value))
  (if (string-match-p "[[:space:]#\"\\\\]" value)
      (concat "\""
              (string-replace "\"" "\\\""
                              (string-replace "\\" "\\\\" value))
              "\"")
    value))

(defun remote-board--ssh-option-line (option)
  "Return one validated OpenSSH config line for OPTION."
  (let ((name (car-safe option))
        (value (cdr-safe option)))
    (unless (and (stringp name)
                 (string-match-p "\\`[[:alpha:]][[:alnum:]]*\\'" name)
                 (not (member (downcase name)
                              '("host" "match" "include" "hostname"
                                "user" "port"))))
      (user-error "Unsupported SSH option: %S" name))
    (format
     "    %s %s\n" name
     (if (member (downcase name)
                 '("proxycommand" "localcommand" "remotecommand"))
         ;; OpenSSH treats these as a command tail; quoting the whole tail
         ;; would make the spaces part of the program name.
         (progn
           (remote-board--ssh-config-value name value)
           value)
       (remote-board--ssh-config-value name value)))))

(defun remote-board--write-ssh-host (file entry)
  "Add ENTRY before existing rules in local SSH config FILE atomically.
OpenSSH uses the first value for most options.  A new explicit Host must
precede an existing `Host *' or Include so its connection fields win."
  (when (file-remote-p file)
    (user-error "SSH config must be on this machine: %s" file))
  (unless (file-directory-p (file-name-directory file))
    (user-error "SSH config directory does not exist: %s"
                (file-name-directory file)))
  (let* ((existing (file-exists-p file))
         (destination (if existing (file-truename file) file))
         (mode (if existing (file-modes destination) #o600))
         (temporary nil))
    (when (and existing
               (not (and (file-regular-p destination)
                         (file-readable-p destination)
                         (file-writable-p destination))))
      (user-error "SSH config must be a readable, writable file: %s" file))
    (unwind-protect
        (progn
          (setq temporary
                (make-temp-file
                 (expand-file-name ".remote-ssh-host-"
                                   (file-name-directory destination))))
          (with-temp-buffer
            (insert entry)
            (when existing
              ;; A leading Host changes the scope of formerly global options
              ;; in the old file.  Host * restores their all-host scope while
              ;; keeping the new host's first-value precedence.
              (insert "\nHost *\n")
              (insert-file-contents destination))
            (write-region (point-min) (point-max) temporary nil 'silent))
          (set-file-modes temporary mode)
          (rename-file temporary destination existing)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun remote-board--ssh-client-config-file ()
  "Return the user SSH config read by the default OpenSSH client."
  (expand-file-name "~/.ssh/config"))

(defun remote-board--ssh-command-config-file (name)
  "Resolve SSH -F NAME against Emacs' client-side invocation directory."
  (expand-file-name name
                    (or invocation-directory
                        (file-name-directory remote-config-file))))

(defun remote-board--validate-ssh-host-entry (alias entry)
  "Check generated Host ENTRY for ALIAS with local OpenSSH, when available.
The isolated config contains no existing Match or Include rules, so this
syntax check cannot run commands from the user's existing SSH config."
  (when-let* ((ssh (remote-client-executable-find "ssh")))
    (let ((temporary (make-temp-file "remote-ssh-check-")))
      (unwind-protect
          (progn
            (write-region entry nil temporary nil 'silent)
            (with-temp-buffer
              (let ((default-directory temporary-file-directory)
                    (process-environment
                     (remote-client-process-environment))
                    (exec-path (remote-client-exec-path)))
                (unless (equal 0 (call-process
                                  ssh nil t nil "-G" "-F" temporary alias))
                  (user-error "OpenSSH rejected host entry: %s"
                              (string-trim (buffer-string)))))))
        (delete-file temporary)))))

(defun remote-board-add-ssh-host
    (alias hostname &optional user port identity-file config-file ssh-options)
  "Add ALIAS for HOSTNAME to imported SSH CONFIG-FILE and reload targets.
USER, PORT, IDENTITY-FILE, and SSH-OPTIONS are optional SSH client settings.
The alias is initially untrusted unless its Remote import grants trust."
  (interactive
   (let* ((alias (read-string "SSH alias: "))
          (hostname (read-string "Host name or IP: " alias))
          (user (read-string "SSH user (blank for default): "))
          (port (read-number "SSH port: " 22))
          (identity-file
           (read-string "Identity file (blank for SSH default): "))
          (files (remote-config-ssh-import-files))
          (preferred (remote-board--ssh-client-config-file))
          (config-file
           (progn
             (unless files
               (user-error "No SSH config import in %s" remote-config-file))
             (if (= (length files) 1)
                 (car files)
               (completing-read
                "SSH config file: " files nil t nil nil
                (if (member preferred files) preferred (car files)))))))
     (list alias hostname user port identity-file config-file)))
  (let* ((files (remote-config-ssh-import-files))
         (preferred (remote-board--ssh-client-config-file))
         (file
          (expand-file-name
           (or config-file
               (and (member preferred files) preferred)
               (car files)
               (user-error "No SSH config import in %s"
                           remote-config-file))))
         (alias (remote-board--ssh-host-field "alias" alias t))
         (hostname (remote-board--ssh-host-field "host name" hostname))
         (user (and user (not (string-empty-p user))
                    (remote-board--ssh-host-field "user" user)))
         (identity-file
          (and identity-file (not (string-empty-p identity-file))
               (remote-board--ssh-config-value
                "identity file" identity-file)))
         (option-lines (mapconcat #'remote-board--ssh-option-line
                                  ssh-options ""))
         (id (remote-fs--slug alias)))
    (unless (member file files)
      (user-error "SSH config is not imported by %s: %s"
                  remote-config-file file))
    (unless (remote-config-ssh-host-importable-p alias file)
      (user-error "SSH alias %s is excluded or has no enabled import route"
                  alias))
    (unless (or (null port)
                (and (integerp port) (<= 1 port) (<= port 65535)))
      (user-error "SSH port must be between 1 and 65535: %S" port))
    (when (or (member alias (remote-config--ssh-hosts file))
              (remote-get-target id))
      (user-error "SSH alias or Remote target already exists: %s" alias))
    (let ((entry
           (concat
            (format "Host %s\n    HostName %s\n" alias hostname)
            (when user (format "    User %s\n" user))
            (when (and port (/= port 22))
              (format "    Port %d\n" port))
            (when identity-file
              (format "    IdentityFile %s\n" identity-file))
            option-lines)))
      (remote-board--validate-ssh-host-entry alias entry)
      (remote-board--write-ssh-host file entry))
    (remote-config-reload)
    (when (derived-mode-p 'remote-board-mode)
      (remote-board-refresh-status))
    (let ((target (remote-get-target id)))
      (unless target
        (error "SSH host was saved but Remote did not import %s" alias))
      (message "Added SSH host %s in %s" alias file)
      target)))

(defun remote-board--shell-words (command)
  "Split shell-like COMMAND into words without evaluating any shell syntax.
Single/double quotes and POSIX backslash escapes are enough for an SSH argv.
Operators outside quotes are rejected rather than interpreted."
  (let ((index 0)
        (length (length command))
        (state 'plain)
        (started nil)
        (letters nil)
        (words nil))
    (when (cl-loop for char across command
                   thereis (and (or (< char 32) (= char 127))
                                (not (eq char ?\t))))
      (user-error "SSH command contains a control character"))
    (while (< index length)
      (let ((char (aref command index)))
        (pcase state
          ('plain
           (cond
            ((memq char '(?\s ?\t))
             (when started
               (push (apply #'string (nreverse letters)) words)
               (setq letters nil started nil)))
            ((memq char '(?\; ?\| ?\& ?\< ?\>))
             (user-error "SSH command contains a shell operator"))
            ((eq char ?\\) (setq state 'plain-escape started t))
            ((eq char ?\') (setq state 'single started t))
            ((eq char ?\") (setq state 'double started t))
            (t (push char letters) (setq started t))))
          ('single
           (if (eq char ?\')
               (setq state 'plain)
             (push char letters)))
          ('double
           (cond
            ((eq char ?\") (setq state 'plain))
            ((eq char ?\\) (setq state 'double-escape))
            (t (push char letters))))
          ('plain-escape
           (push char letters)
           (setq state 'plain))
          ('double-escape
           (unless (memq char '(?$ ?` ?\" ?\\))
             (push ?\\ letters))
           (push char letters)
           (setq state 'double))))
      (setq index (1+ index)))
    (unless (eq state 'plain)
      (user-error "Malformed SSH command quoting"))
    (when started
      (push (apply #'string (nreverse letters)) words))
    (nreverse words)))

(defun remote-board--parse-ssh-command (command)
  "Parse an SSH connection COMMAND into config fields without running it.
Only connection options before one destination are accepted.  A remote
command, shell prefix, or unsupported SSH flag is rejected."
  (let* ((words (remote-board--shell-words command))
         (program (pop words))
         user port identity-files config-file hostname-override options)
    (unless (and program
                 (equal (file-name-nondirectory program) "ssh"))
      (user-error "Expected an ssh command"))
    (while (and words (string-prefix-p "-" (car words)))
      (let* ((flag (pop words))
             (kind (and (>= (length flag) 2) (substring flag 0 2)))
             (inline (and kind (substring flag 2)))
             (value
              (if (and inline (not (string-empty-p inline)))
                  inline
                (or (pop words)
                    (user-error "SSH option %s needs a value" flag)))))
        (pcase kind
          ("-i" (push value identity-files))
          ("-p"
           (unless (string-match-p "\\`[[:digit:]]+\\'" value)
             (user-error "Invalid SSH port: %S" value))
           (setq port (string-to-number value)))
          ("-l" (setq user value))
          ("-F" (setq config-file value))
          ("-J" (push (cons "ProxyJump" value) options))
          ("-o"
           (unless (string-match
                    "\\`\\([[:alpha:]][[:alnum:]]*\\)=\\(.+\\)\\'"
                    value)
             (user-error "Use -o Name=Value: %S" value))
           (let ((name (match-string 1 value))
                 (argument (match-string 2 value)))
             (pcase (downcase name)
               ("hostname" (setq hostname-override argument))
               ("user" (setq user argument))
               ("port"
                (unless (string-match-p "\\`[[:digit:]]+\\'" argument)
                  (user-error "Invalid SSH port: %S" argument))
                (setq port (string-to-number argument)))
               ("identityfile" (push argument identity-files))
               (_ (push (cons name argument) options)))))
          (_ (user-error "Unsupported SSH option: %s" flag)))))
    (unless (= (length words) 1)
      (user-error "SSH command needs exactly one host and no remote command"))
    (let* ((destination (car words))
           (at (string-match "@" destination))
           (destination-host
            (if at (substring destination (1+ at)) destination))
           (hostname (or hostname-override destination-host)))
      (when at
        (unless (and (> at 0) (not (string-empty-p destination-host)))
          (user-error "Invalid SSH destination: %S" destination))
        (unless user (setq user (substring destination 0 at))))
      (when (or (string-empty-p destination-host)
                (string-match-p "[@/`$();]" destination-host))
        (user-error "Invalid SSH destination: %S" destination))
      (remote-board--ssh-host-field "destination" destination-host)
      (remote-board--ssh-host-field "host name" hostname)
      (when user (remote-board--ssh-host-field "user" user))
      (dolist (identity-file identity-files)
        (remote-board--ssh-config-value "identity file" identity-file))
      (when config-file
        (remote-board--ssh-config-value "config file" config-file))
      (dolist (option options)
        (remote-board--ssh-option-line option))
      (setq identity-files (nreverse identity-files))
      (list :hostname hostname
            :suggested-alias (remote-fs--slug destination-host)
            :user user :port port :identity-file (car identity-files)
            :identity-files identity-files
            :config-file config-file :options (nreverse options)))))

(defun remote-board-add-ssh-command (command &optional alias config-file)
  "Add the host in SSH COMMAND to an imported config as ALIAS.
CONFIG-FILE overrides the interactive choice unless COMMAND has `-F'."
  (interactive
   (let* ((command (read-string "SSH command: " "ssh "))
          (parsed (remote-board--parse-ssh-command command))
          (alias (read-string "Save as SSH host: "
                              (plist-get parsed :suggested-alias)))
          (files (remote-config-ssh-import-files))
          (from-command (plist-get parsed :config-file))
          (preferred (remote-board--ssh-client-config-file))
          (file
           (cond
            (from-command nil)
            ((null files)
             (user-error "No SSH config import in %s" remote-config-file))
            ((= (length files) 1) (car files))
            (t (completing-read
                "SSH config file: " files nil t nil nil
                (if (member preferred files) preferred (car files)))))))
     (list command alias file)))
  (let* ((parsed (remote-board--parse-ssh-command command))
         (command-file (plist-get parsed :config-file))
         (identity-files (plist-get parsed :identity-files)))
    (when (and command-file config-file
               (not (equal (remote-board--ssh-command-config-file
                            command-file)
                           (expand-file-name config-file))))
      (user-error "SSH -F conflicts with selected config file"))
    (remote-board-add-ssh-host
     (or alias (plist-get parsed :suggested-alias))
     (plist-get parsed :hostname)
     (plist-get parsed :user)
     (plist-get parsed :port)
     (car identity-files)
     (or (and command-file
              (remote-board--ssh-command-config-file command-file))
         config-file)
     (append
      (mapcar (lambda (file) (cons "IdentityFile" file))
              (cdr identity-files))
      (plist-get parsed :options)))))

(defun remote-route-log-buffer ()
  "Display recent route decisions and failures."
  (interactive)
  (let ((buffer (get-buffer-create "*Remote Routes*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (dolist (entry remote-route-log)
          (insert
           (format-time-string
            "%Y-%m-%d %H:%M:%S "
            (plist-get entry :time)))
          (insert (format "%S\n" (cdr (cdr entry)))))))
    (pop-to-buffer buffer)))

(defun remote-board-connection-log (&optional target)
  "Display recent connection phases for TARGET without contacting it."
  (interactive
   (list (if (derived-mode-p 'remote-board-mode)
             (remote-board-target-at-point)
           (remote-read-target "Connection log for target: "))))
  (let* ((target-id
          (if (remote-target-p target)
              (remote-target-id target)
            (or target
                (user-error "Select a target"))))
         (buffer
          (get-buffer-create
           (format "*Remote Connect %s*" target-id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (format "Connection history: %s\n\n" target-id))
        (dolist (entry
                 (reverse
                  (gethash target-id remote-board-connection-history)))
          (insert
           (format "%s  #%s  %s/%s  %s\n"
                   (format-time-string
                    "%Y-%m-%d %H:%M:%S"
                    (plist-get entry :time))
                   (plist-get entry :generation)
                   (plist-get entry :pipeline)
                   (plist-get entry :backend)
                   (remote-board--connection-phase-label
                    (plist-get entry :phase)
                    (plist-get entry :backend))))
          (when-let* ((error-data (plist-get entry :error)))
            (insert
             (format "    %s\n"
                     (condition-case nil
                         (error-message-string error-data)
                       (error (format "%S" error-data)))))))))
    (pop-to-buffer buffer)))

(defun remote-board--ssh-client-command
    (target &optional tty diagnostic)
  "Return an OpenSSH command for TARGET without opening a framework session.
TTY requests an interactive shell.  DIAGNOSTIC requests bounded verbose
output and disables authentication prompts."
  (require 'remote-backend-tramp)
  (let* ((context
          (remote-context
           (remote-make-file-name (remote-target-id target) "/")))
         (routes (remote-routes "process" 'process-async context))
         (options
          (when diagnostic
            '("BatchMode=yes" "ConnectTimeout=5"
              "ConnectionAttempts=1"))))
    (or
     (seq-some
      (lambda (route)
        (when (member (remote-route-link-plugin-id route)
                      '("tramp" "tramp-rpc"))
          (condition-case nil
              (remote-backend-tramp-ssh-client-command
               route :tty tty :verbose diagnostic
               :extra-options options)
            (remote-backend-unsupported nil))))
      routes)
     (user-error "No usable SSH route for %s"
                 (remote-target-label target)))))

(defun remote-board--ssh-probe-finish
    (target-id generation process stdout-buffer stderr-buffer log-buffer)
  "Finish TARGET-ID's SSH diagnostic after PROCESS and its streams close."
  (let* ((status (process-exit-status process))
         (stdout
          (when (buffer-live-p stdout-buffer)
            (with-current-buffer stdout-buffer (buffer-string))))
         (stderr
          (when (buffer-live-p stderr-buffer)
            (with-current-buffer stderr-buffer (buffer-string))))
         (output (concat (or stderr "") (or stdout "")))
         (state (remote-board--ssh-classify-result status output))
         (label (remote-board--ssh-status-label state))
         (detail
          (if (eq state 'ready)
              "SSH authentication and command execution succeeded"
            (let ((lines
                   (reverse (split-string (string-trim output) "\n" t))))
              (or
               (seq-find
                (lambda (line)
                  (eq (remote-board--ssh-classify-result 255 line)
                      state))
                lines)
               (car lines)
               "OpenSSH exited without diagnostic output")))))
    (when (and (buffer-live-p log-buffer)
               (eq (gethash target-id remote-board--ssh-probe-processes)
                   process))
      (with-current-buffer log-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert
           (if (string-empty-p output)
               "No SSH output.\n"
             (substring output (max 0 (- (length output) 65536)))))
          (unless (bolp) (insert "\n"))
          (insert (format "\nResult: %s (exit %d)\n" label status))
          (unless (eq state 'ready)
            (insert "Use T on the Remote board for interactive SSH login.\n")))))
    (when (eq (gethash target-id remote-board--ssh-probe-processes)
              process)
      (remhash target-id remote-board--ssh-probe-processes)
      (when (= generation remote-board--ssh-status-generation)
        (remote-board--ssh-status-set target-id state detail)
        (message "SSH diagnostic for %s: %s" target-id label)))
    (when (buffer-live-p stdout-buffer)
      (kill-buffer stdout-buffer))
    (when (buffer-live-p stderr-buffer)
      (kill-buffer stderr-buffer))))

(defun remote-board-ssh-diagnose (&optional target)
  "Check TARGET with client OpenSSH and show its detailed output.
The check runs asynchronously with password prompts disabled.  It never
opens a Remote workspace or starts an LSP server."
  (interactive
   (list
    (if (derived-mode-p 'remote-board-mode)
        (remote-board-target-at-point)
      (remote-read-target "Diagnose SSH target: " t))))
  (let* ((target
          (if (remote-target-p target)
              target
            (or (remote-get-target target)
                (user-error "Unknown Remote target: %S" target))))
         (target-id (remote-target-id target))
         (existing
          (gethash target-id remote-board--ssh-probe-processes)))
    (if (and existing (process-live-p existing))
        (progn
          (pop-to-buffer
           (get-buffer-create (format "*Remote SSH %s*" target-id)))
          existing)
      (let* ((command (append
                       (remote-board--ssh-client-command target nil t)
                       '("true")))
             (log-buffer
              (get-buffer-create (format "*Remote SSH %s*" target-id)))
             (stdout-buffer (generate-new-buffer " *Remote SSH stdout*"))
             (stderr-buffer (generate-new-buffer " *Remote SSH stderr*"))
             (generation remote-board--ssh-status-generation)
             (process-environment (remote-client-process-environment))
             (exec-path (remote-client-exec-path))
             (default-directory temporary-file-directory)
             stderr-process process finished)
        (setenv "LC_ALL" "C")
        (with-current-buffer log-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Checking SSH for %s...\n\n"
                            (remote-target-label target)))
            (special-mode)))
        (setq stderr-process
              (make-pipe-process
               :name (format "remote-ssh-stderr-%s" target-id)
               :buffer stderr-buffer :noquery t))
        (cl-labels
            ((finish (&optional force)
               (when (and (not finished)
                          process
                          (memq (process-status process) '(exit signal))
                          (or force
                              (not (process-live-p stderr-process))))
                 (setq finished t)
                 (when (process-live-p stderr-process)
                   (delete-process stderr-process))
                 (remote-board--ssh-probe-finish
                  target-id generation process stdout-buffer
                  stderr-buffer log-buffer))))
          (set-process-sentinel
           stderr-process
           (lambda (_process _event) (finish)))
          (condition-case error
              (setq process
                    (make-process
                     :name (format "remote-ssh-probe-%s" target-id)
                     :command command
                     :buffer stdout-buffer
                     :stderr stderr-process
                     :connection-type 'pipe
                     :noquery t
                     :sentinel
                     (lambda (_process _event)
                       (finish)
                       (run-at-time 0.2 nil (lambda () (finish t))))))
            (error
             (delete-process stderr-process)
             (kill-buffer stdout-buffer)
             (kill-buffer stderr-buffer)
             (signal (car error) (cdr error)))))
        (puthash target-id process remote-board--ssh-probe-processes)
        (remote-board--ssh-status-set target-id 'checking nil)
        (pop-to-buffer log-buffer)
        process))))

(defun remote-board-ssh-login (&optional target)
  "Open an interactive client SSH terminal for TARGET.
This uses the target's configured SSH pipeline and does not change the
framework connection until a Remote folder is opened or reconnected."
  (interactive
   (list
    (if (derived-mode-p 'remote-board-mode)
        (remote-board-target-at-point)
      (remote-read-target "SSH login to target: " t))))
  (let* ((target
          (if (remote-target-p target)
              target
            (or (remote-get-target target)
                (user-error "Unknown Remote target: %S" target))))
         (command (remote-board--ssh-client-command target t))
         (process-environment (remote-client-process-environment))
         (exec-path (remote-client-exec-path))
         (default-directory temporary-file-directory)
         (name (format "Remote SSH Login %s" (remote-target-id target))))
    (require 'term)
    (let ((buffer (apply #'make-term name (car command) nil (cdr command))))
      (with-current-buffer buffer (term-char-mode))
      (pop-to-buffer buffer)
      buffer)))

(defun remote-board-doctor (&optional target)
  "Run `remote-doctor' for TARGET or the target at point."
  (interactive)
  (remote-doctor
   (or target
       (and (derived-mode-p 'remote-board-mode)
            (remote-board-target-at-point)))
   current-prefix-arg))

(defvar remote-board--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'remote-board)
    map)
  "Mouse action for the current target's mode-line indicator.")

(defun remote-board--mode-line-text ()
  "Return a cheap current-target indicator for canonical Remote buffers."
  (let ((path (or buffer-file-name default-directory)))
    (when (and (stringp path)
               (string-prefix-p "/fs:" path))
      (when-let* ((target-id (remote-fs-target-id path))
                  ((not (equal target-id "local"))))
        (let* ((target (remote-get-target target-id))
               (label (or (and target (remote-target-label target))
                          target-id)))
          (propertize
           (format " Remote:%s"
                   (truncate-string-to-width label 20 nil nil "…"))
           'help-echo "mouse-1: Open Remote targets and folders"
           'mouse-face 'mode-line-highlight
           'local-map remote-board--mode-line-map))))))

(defconst remote-board--mode-line-entry
  '(:eval (remote-board--mode-line-text))
  "Mode-line entry for canonical Remote buffers.")

(unless (member remote-board--mode-line-entry
                (default-value 'mode-line-misc-info))
  (setq-default mode-line-misc-info
                (append (default-value 'mode-line-misc-info)
                        (list remote-board--mode-line-entry))))

(defvar-keymap remote-board-mode-map
  :parent tabulated-list-mode-map
  "RET" #'remote-open-target
  "o" #'remote-open-target
  "f" #'remote-open-folder
  "!" #'remote-board-run-command
  "p" #'remote-board-forward-port
  "k" #'remote-board-close-forward
  "n" #'remote-board-rename-forward
  "P" #'remote-board-change-local-port
  "r" #'remote-board-reconnect-workspace
  "c" #'remote-board-close-workspace
  "C" #'remote-board-disconnect-target
  "w" #'remote-copy-target-uri
  "a" #'remote-board-add-ssh-host
  "A" #'remote-board-add-ssh-command
  "e" #'remote-edit-config
  "l" #'remote-route-log-buffer
  "L" #'remote-board-connection-log
  "D" #'remote-board-ssh-diagnose
  "T" #'remote-board-ssh-login
  "d" #'remote-board-doctor
  "s" #'remote-board-refresh-status
  "g" #'remote-board-refresh)

(define-derived-mode remote-board-mode tabulated-list-mode "Remote"
  "Mode for logical targets and resolved route health.
Open targets with RET, close a folder with c or disconnect a target with C.
Add SSH hosts with a or paste an SSH command with A.
Inspect SSH with D, open an SSH login terminal with T, inspect connection
phases with L, and run the full
Doctor with d.  See `remote-board-mode-map' for more actions."
  (setq tabulated-list-format
        [("Target" 22 t)
         ("ID" 18 t)
         ("State" 17 t)
         ("Folder / Port" 48 nil)
         ("Workspaces" 11 nil)
         ("Files" 24 nil)
         ("Processes" 24 nil)
         ("Trust" 10 nil)])
  (setq tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries (remote-board--entries))
  (tabulated-list-init-header))

(defun remote-board ()
  "Open the logical target and route board."
  (interactive)
  (let ((buffer (get-buffer-create "*Remote*")))
    (with-current-buffer buffer
      (remote-board-mode)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(provide 'remote-board)
;;; remote-board.el ends here
