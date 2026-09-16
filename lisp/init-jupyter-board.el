;;; init-jupyter-board.el --- Jupyter kernelspec management board -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual management for local kernelspecs and the vendored remote_ikernel CLI.

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'init-jupyter-management)
(require 'json)
(require 'subr-x)
(require 'transient)

(declare-function my/noema-api-call "init-aaronnote" (channel args callback))
(declare-function my/noema-jupyter-runtime-control
                  "init-aaronnote-jupyter-runtime" (runtime-id action callback))
(declare-function my/noema-jupyter-runtime-snapshot
                  "init-aaronnote-jupyter-runtime" (&optional target-id))

(defgroup my/jupyter-board nil
  "Jupyter kernelspec and remote kernel management."
  :group 'tools)

(defcustom my/jupyter-board-show-advanced-default nil
  "Whether a newly opened Jupyter Board shows technical resources.
The default view keeps remote profiles and active sessions prominent."
  :type 'boolean
  :group 'my/jupyter-board)

(defconst my/jupyter-board-buffer-name "*Jupyter Board*")
(defconst my/jupyter-board-log-buffer-name "*Jupyter Board Log*")
(defconst my/jupyter-board-detail-buffer-name "*Jupyter Kernel Detail*")

(defvar-local my/jupyter-board--entries nil)
(defvar-local my/jupyter-board--runtimes nil)
(defvar-local my/jupyter-board--connections nil)
(defvar-local my/jupyter-board--errors nil)
(defvar-local my/jupyter-board--loading nil)
(defvar-local my/jupyter-board--target nil)
(defvar-local my/jupyter-board--source-buffer nil)
(defvar-local my/jupyter-board--refresh-generation 0)
(defvar-local my/jupyter-board--show-advanced nil)
(defvar-local my/jupyter-board--show-stale-connections nil)
(defvar my/jupyter-board--edit-origin nil)
(defvar my/jupyter-board--edit-target nil)
(defvar my/jupyter-board-remote-host-history nil)
(defvar my/jupyter-board-remote-name-history nil)
(defvar my/jupyter-board-remote-workdir-history nil)
(defvar my/jupyter-board-remote-command-history nil)

(defconst my/jupyter-board-course-pytorch-profile
  '(:name "Python 3.13 PyTorch CUDA"
    :workdir "/home/hc/Desktop/9444"
    :interpreter "/home/hc/Desktop/9444/.conda/bin/python"
    :kernel-command "/home/hc/Desktop/9444/.conda/bin/python -m ipykernel_launcher -f {connection_file}"
    :language "python"
    :group "core")
  "The known-good remote profile for the 9444 CUDA/conda environment.")

(define-derived-mode my/jupyter-board-mode aaron-ui-board-mode "Jupyter-Board"
  "Major mode for the Jupyter management board.")

(defun my/jupyter-board--alist-get (key object)
  "Return KEY from JSON alist OBJECT."
  (my/jupyter-management-get key object))

(defun my/jupyter-board--capture (program &rest args)
  "Run PROGRAM with ARGS and return stdout, signaling on failure."
  (unless (and program (file-executable-p program))
    (error "Executable is unavailable: %s" (or program "<unset>")))
  (let ((stderr-file (make-temp-file "jupyter-board-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file program nil
                               (list (current-buffer) stderr-file) nil args)))
            (if (zerop status)
                (buffer-string)
              (let ((stdout (buffer-string)))
                (error "%s"
                       (string-trim
                        (concat
                         stdout "\n"
                         (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string)))))))))
      (delete-file stderr-file))))

(defun my/jupyter-board--json-file (file)
  "Read FILE as a JSON alist, returning nil when unreadable."
  (when (file-readable-p file)
    (condition-case nil
        (json-parse-string
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))
         :object-type 'alist :array-type 'list
         :null-object nil :false-object nil)
      (error nil))))

(defun my/jupyter-board--remote-p (name argv raw)
  "Return non-nil when NAME, ARGV, or RAW identifies remote_ikernel."
  (or (string-prefix-p "rik_" name)
      (cl-some (lambda (arg)
                 (string-match-p "remote_ikernel" (format "%s" arg)))
               argv)
      (my/jupyter-board--alist-get 'remote_ikernel_argv raw)))

(defun my/jupyter-board--remote-metadata (spec)
  "Return Aaron remote-kernel metadata from SPEC."
  (let* ((metadata (my/jupyter-board--alist-get 'metadata spec))
         (aaron (my/jupyter-board--alist-get 'aaron metadata)))
    (my/jupyter-board--alist-get 'remote_kernel aaron)))

(defun my/jupyter-board--argv-option (argv option)
  "Return OPTION's value from ARGV, supporting split and equals forms."
  (my/jupyter-management-argv-option argv option))

(defun my/jupyter-board--entry (name object)
  "Normalize kernelspec NAME and JSON OBJECT into a plist."
  (my/jupyter-management-normalize-spec
   (or (and (boundp 'my/jupyter-board--target) my/jupyter-board--target)
       (remote-get-target "local"))
   name object))

(defun my/jupyter-board--load-entries ()
  "Return normalized entries from the configured Jupyter command."
  (let* ((output (my/jupyter-board--capture
                  my/jupyter-board-jupyter-command "kernelspec" "list" "--json"))
         (data (json-parse-string output :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil))
         (kernelspecs (my/jupyter-board--alist-get 'kernelspecs data)))
    (sort
     (mapcar (lambda (cell)
               (my/jupyter-board--entry (symbol-name (car cell)) (cdr cell)))
             kernelspecs)
     (lambda (a b) (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun my/jupyter-board--current-entry ()
  "Return the kernel entry at point."
  (or (get-text-property (point) 'my/jupyter-board-entry)
      (get-text-property (line-beginning-position) 'my/jupyter-board-entry)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'my/jupyter-board-entry))))

(defun my/jupyter-board--require-entry (&optional remote-only)
  "Return current entry, requiring a remote entry when REMOTE-ONLY."
  (let ((entry (my/jupyter-board--current-entry)))
    (unless entry (user-error "No kernel entry at point"))
    (when (and remote-only (not (plist-get entry :remote)))
      (user-error "This action requires a remote kernel"))
    entry))

(defun my/jupyter-board--command-version (program &rest args)
  "Return one-line PROGRAM version using ARGS."
  (condition-case nil
      (let ((output (string-trim (apply #'my/jupyter-board--capture program args))))
        (if (string-match "version[[:space:]]+\\([[:alnum:]+._-]+\\)" output)
            (match-string 1 output)
          (car (split-string output "\n" t))))
    (error "unavailable")))

(defun my/jupyter-board--module-source ()
  "Return the installed remote_ikernel module source path."
  (condition-case nil
      (string-trim
       (my/jupyter-board--capture
        my/jupyter-board-python-command "-c"
        "import remote_ikernel; print(remote_ikernel.__file__)"))
    (error "unavailable")))

(defun my/jupyter-board--insert-context ()
  "Insert selected-target and passive tool health information."
  (let* ((target my/jupyter-board--target)
         (local (my/jupyter-management-local-target-p target))
         (jupyter (my/jupyter-management-command target 'jupyter))
         (remote-ikernel (my/jupyter-management-command target 'remote-ikernel)))
    (aaron-ui-board-insert-section "Context / Health")
    (aaron-ui-board-insert-field
     "Target" (format "%s (%s)"
                      (my/jupyter-management-target-label target)
                      (my/jupyter-management-target-id target)))
    (aaron-ui-board-insert-field
     "Noema core"
     (if (and (boundp 'my/noema--ready) my/noema--ready) "online" "offline")
     (if (and (boundp 'my/noema--ready) my/noema--ready)
         'aaron-ui-board-good 'aaron-ui-board-dim))
    (aaron-ui-board-insert-field
     "Jupyter" (or jupyter "unconfigured")
     (if (or (not local) (and jupyter (file-executable-p jupyter)))
         'aaron-ui-board-good 'aaron-ui-board-bad))
    (aaron-ui-board-insert-field
     "remote_ikernel" (or remote-ikernel "unconfigured")
     (if (or (not local) (and remote-ikernel (file-executable-p remote-ikernel)))
         'aaron-ui-board-good 'aaron-ui-board-dim))
    (when local
      (aaron-ui-board-insert-field "Python" (or my/jupyter-board-python-command "unconfigured"))
      (aaron-ui-board-insert-field "vendored source"
                                   my/jupyter-remote-ikernel-source-directory))
    (insert "   ")
    (aaron-ui-board-insert-actions
     `((:label "Target" :command my/jupyter-board-select-target :primary t
               :help "Select local or Remote target")
       (:label "Refresh" :command my/jupyter-board-refresh :help "Refresh snapshots")
       (:label "Doctor" :command my/jupyter-board-doctor :help "Open target doctor")
       ,@(when local
           '((:label "Reinstall tool" :command my/jupyter-board-reinstall
                     :help "Install the vendored remote_ikernel into the configured Python")))
       (:label "Log" :command my/jupyter-board-open-log :help "Open command log")))
    (insert "\n\n")))

(defun my/jupyter-board--entry-detail (entry)
  "Return a compact, user-facing detail string for ENTRY."
  (if (plist-get entry :remote)
      (string-join
       (delq nil
             (list (and (plist-get entry :workdir)
                        (format "Folder: %s" (plist-get entry :workdir)))
                   (and (plist-get entry :kernel-command)
                        (format "Starts: %s" (plist-get entry :kernel-command)))
                   (when-let* ((health (plist-get entry :health))
                               (status (plist-get health :status))
                               ((eq status 'error)))
                     (format "Needs repair: %s" (plist-get health :detail)))))
       "  ·  ")
    (string-join
     (delq nil
           (list (format "%s" (plist-get entry :resource-dir))
                 (when-let* ((health (plist-get entry :health)))
                   (format "%s: %s" (plist-get health :status)
                           (plist-get health :detail)))))
     "  ")))

(defun my/jupyter-board--remote-interface-label (entry)
  "Return a readable interface label for remote kernelspec ENTRY."
  (let ((interface (plist-get entry :interface)))
    (pcase interface
      ("ssh" "SSH")
      ("pbs" "PBS")
      ("sge" "Grid Engine")
      ("sge_qrsh" "Grid Engine / qrsh")
      ("slurm" "Slurm")
      ("lsf" "LSF")
      ("local" "Local launcher")
      (_ (if (and interface (not (string-empty-p interface)))
             interface
           "Remote")))))

(defun my/jupyter-board--insert-entry-actions (entry actions)
  "Insert ACTIONS for ENTRY and make the buttons entry-aware."
  (let ((start (point)))
    (insert "      ")
    (aaron-ui-board-insert-actions actions)
    (add-text-properties
     start (point)
     (list 'my/jupyter-board-entry entry
           'aaron-ui-board--item-id
           (or (plist-get entry :id) (plist-get entry :name))))
    (insert "\n\n")))

(defun my/jupyter-board--insert-entry (entry)
  "Insert one kernelspec ENTRY."
  (let* ((remote (plist-get entry :remote))
         (health (plist-get (plist-get entry :health) :status))
         (host (plist-get entry :host))
         (meta (if remote
                   (string-join
                    (delq nil
                          (list (my/jupyter-board--remote-interface-label entry)
                                host
                                (unless (string-empty-p
                                         (or (plist-get entry :language) ""))
                                  (plist-get entry :language))
                                (when (eq health 'error) "launcher unavailable")))
                    "  ·  ")
                 (format "%s  %s" (plist-get entry :name)
                         (or (plist-get entry :language) "")))))
    (aaron-ui-board-insert-row
     :id (or (plist-get entry :id) (plist-get entry :name))
     :icon (if remote 'server 'jupyter)
     :badge (if remote
                (if (equal (plist-get entry :group) "core") "SAVED" "TEMP")
              (upcase (or (plist-get entry :language) "LOCAL")))
     :badge-tone (if remote
                     (if (equal (plist-get entry :group) "core") 'success 'warning)
                   'muted)
     :title (plist-get entry :display-name)
     :meta meta
     :detail (my/jupyter-board--entry-detail entry)
     :action (lambda (_button) (my/jupyter-board-describe))
     :help (if remote
               "RET: details; use the buttons below to connect or manage"
             "RET: details; r: open REPL; d: delete")
     :properties (list 'my/jupyter-board-entry entry))
    (when remote
      (my/jupyter-board--insert-entry-actions
       entry
       `((:label "Open REPL" :command my/jupyter-board-repl :primary t
                 :help "Start this remote kernel and open its Emacs REPL")
         (:label "Edit" :command my/jupyter-remote-edit-guided
                 :help "Edit the common settings with a guided form")
         (:label ,(if (equal (plist-get entry :group) "core")
                      "Make temporary" "Keep profile")
                 :command my/jupyter-board-toggle-group
                 :help "Toggle whether bulk cleanup may remove this profile")
         (:label "Details" :command my/jupyter-board-describe
                 :help "Show the complete kernelspec")
         (:label "Delete" :command my/jupyter-board-delete
                 :help "Delete this profile after confirmation"))))))

(defun my/jupyter-board--insert-runtime (entry)
  "Insert one live runtime ENTRY."
  (let ((status (format "%s" (or (plist-get entry :status) "unknown"))))
    (aaron-ui-board-insert-row
     :id (plist-get entry :id)
     :icon 'jupyter
     :badge (upcase status)
     :badge-tone (pcase status
                   ((or "dead" "error") 'danger)
                   ("running" 'warning)
                   (_ 'success))
     :title (or (plist-get entry :title)
                (plist-get entry :kernel) "Jupyter runtime")
     :meta (format "%s · %s · %s"
                   (plist-get entry :provider)
                   (or (plist-get entry :target-id) "local")
                   (or (plist-get entry :session) "default"))
     :detail
     (string-join
      (delq nil
            (list (plist-get entry :source-file)
                  (when-let* ((pid (plist-get entry :pid))) (format "pid=%s" pid))
                  (when (plist-get entry :state-lost) "state lost")))
      "  ")
     :action (lambda (_button) (my/jupyter-board-describe))
     :help "RET: details; use the buttons below to control this session"
     :properties (list 'my/jupyter-board-entry entry))
    (my/jupyter-board--insert-entry-actions
     entry
     `((:label "Open" :command my/jupyter-board-repl :primary t
               :help "Open the REPL or reconnect through its connection file")
       (:label "Interrupt" :command my/jupyter-board-interrupt
               :help "Interrupt the current execution")
       ,@(unless (plist-get entry :attached)
           '((:label "Restart" :command my/jupyter-board-restart
                     :help "Restart this runtime after confirmation")))
       (:label ,(if (plist-get entry :attached) "Disconnect" "Shut down")
               :command my/jupyter-board-shutdown
               :help "End or disconnect this runtime after confirmation")))))

(defun my/jupyter-board--insert-connection (entry)
  "Insert one local connection-file ENTRY."
  (let ((valid (plist-get entry :valid)) (stale (plist-get entry :stale)))
    (aaron-ui-board-insert-row
     :id (plist-get entry :id)
     :icon 'jupyter
     :badge (cond ((not valid) "INVALID") (stale "STALE") (t "READY"))
     :badge-tone (cond ((not valid) 'danger) (stale 'warning) (t 'success))
     :title (file-name-nondirectory (plist-get entry :file))
     :meta (or (plist-get entry :kernel) "connection")
     :detail (format "%s%s"
                     (plist-get entry :file)
                     (if (plist-get entry :mtime)
                         (format "  %s" (format-time-string
                                         "%Y-%m-%d %H:%M"
                                         (plist-get entry :mtime))) ""))
     :action (lambda (_button) (my/jupyter-board-describe))
     :help "RET: details; use the buttons below to connect or inspect"
     :properties (list 'my/jupyter-board-entry entry))
    (my/jupyter-board--insert-entry-actions
     entry
     '((:label "Connect" :command my/jupyter-board-repl :primary t
               :help "Connect an Emacs REPL to this kernel")
       (:label "Open JSON" :command my/jupyter-board-open-resource
               :help "Open the connection file")
       (:label "Delete" :command my/jupyter-board-delete
               :help "Delete the connection file after confirmation")))))

(defun my/jupyter-board--insert-group (title entries &optional tone)
  "Insert TITLE section containing ENTRIES, with optional badge TONE."
  (aaron-ui-board-insert-section title (length entries) tone)
  (if entries
      (progn
        (mapc #'my/jupyter-board--insert-entry entries)
        (unless (plist-get (car entries) :remote) (insert "\n")))
    (aaron-ui-board-insert-empty "No kernels in this group.")))

(defun my/jupyter-board--broker-by-host-id (brokers host-id)
  "Return the broker entry in BROKERS matching HOST-ID."
  (and host-id
       (cl-find host-id brokers :key (lambda (entry)
                                       (plist-get entry :host-runtime-id))
                :test #'equal)))

(defun my/jupyter-board--normalize-noema-runtimes (payload brokers target-id)
  "Normalize Noema task PAYLOAD and merge BROKERS for TARGET-ID."
  (let ((tasks (or (my/jupyter-board--alist-get 'kernels payload) nil))
        matched result)
    (dolist (task tasks)
      (let* ((host-id (my/jupyter-board--alist-get 'hostRuntimeId task))
             (broker (my/jupyter-board--broker-by-host-id brokers host-id))
             (owner (or (plist-get broker :target-id) "local")))
        (when broker (push broker matched))
        (when (equal owner target-id)
          (push
           (list :id (format "runtime:noema:%s"
                             (or (my/jupyter-board--alist-get 'id task)
                                 (my/jupyter-board--alist-get 'key task)))
                 :kind 'runtime :provider 'noema :noema-task t
                 :target-id owner
                 :key (my/jupyter-board--alist-get 'key task)
                 :runtime-id (my/jupyter-board--alist-get 'id task)
                 :host-runtime-id host-id
                 :title (or (my/jupyter-board--alist-get 'kernel task) "Noema kernel")
                 :kernel (my/jupyter-board--alist-get 'kernel task)
                 :session (my/jupyter-board--alist-get 'session task)
                 :language (my/jupyter-board--alist-get 'language task)
                 :source-file (my/jupyter-board--alist-get 'sourceFile task)
                 :script-file (my/jupyter-board--alist-get 'file task)
                 :status (my/jupyter-board--alist-get 'status task)
                 :running (my/jupyter-board--alist-get 'running task)
                 :attached (eq t (my/jupyter-board--alist-get 'attached task))
                 :generation (or (my/jupyter-board--alist-get 'generation task)
                                 (plist-get broker :generation))
                 :state-lost (or (eq t (my/jupyter-board--alist-get 'stateLost task))
                                 (plist-get broker :state-lost))
                 :pid (plist-get broker :pid)
                 :log-file (plist-get broker :log-file)
                 :connection-file (plist-get broker :connection-file))
           result))))
    (dolist (broker brokers)
      (unless (memq broker matched) (push broker result)))
    (nreverse result)))

(defun my/jupyter-board--set-provider-result
    (buffer generation provider value error)
  "Apply async PROVIDER VALUE or ERROR to BUFFER for GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'my/jupyter-board-mode)
                 (= generation my/jupyter-board--refresh-generation))
        (setq my/jupyter-board--loading (delq provider my/jupyter-board--loading))
        (setf (alist-get provider my/jupyter-board--errors) error)
        (pcase provider
          ('specs (unless error (setq my/jupyter-board--entries value)))
          ('connections (unless error (setq my/jupyter-board--connections value)))
          ('noema
           (let* ((target-id (my/jupyter-management-target-id
                              my/jupyter-board--target))
                  (brokers (when (fboundp 'my/noema-jupyter-runtime-snapshot)
                             (my/noema-jupyter-runtime-snapshot target-id)))
                  (emacs (when (equal target-id "local")
                           (my/jupyter-management-emacs-clients))))
             (setq my/jupyter-board--runtimes
                   (append emacs
                           (if value
                               (my/jupyter-board--normalize-noema-runtimes
                                value brokers target-id)
                             brokers))))))
        (my/jupyter-board--render)))))

(defun my/jupyter-board--insert-provider-errors ()
  "Render loading and provider error state."
  (when (or my/jupyter-board--loading
            (cl-some #'cdr my/jupyter-board--errors))
    (aaron-ui-board-insert-section "Loading & Problems")
    (dolist (provider my/jupyter-board--loading)
      (aaron-ui-board-insert-field
       (pcase provider
         ('specs "Kernel profiles")
         ('connections "Connections")
         ('noema "Live sessions")
         (_ (format "%s" provider)))
       "loading…"
                                   'aaron-ui-board-dim))
    (dolist (entry my/jupyter-board--errors)
      (when (cdr entry)
        (aaron-ui-board-insert-field
         (pcase (car entry)
           ('specs "Kernel profiles")
           ('connections "Connections")
           ('noema "Live sessions")
           (_ (format "%s" (car entry))))
         (cdr entry) 'aaron-ui-board-bad)))
    (insert "   ")
    (aaron-ui-board-insert-actions
     '((:label "Diagnostics" :command my/jupyter-board-doctor
               :help "Inspect the selected target")
       (:label "Command log" :command my/jupyter-board-open-log
               :help "Open output from management commands")))
    (insert "\n")))

(defun my/jupyter-board--insert-quick-start (remote-count)
  "Insert the short usage path for REMOTE-COUNT configured profiles."
  (let* ((local (my/jupyter-management-local-target-p my/jupyter-board--target))
         (program (my/jupyter-management-command
                   my/jupyter-board--target 'remote-ikernel))
         (ready (or (not local) (and program (file-executable-p program)))))
    (aaron-ui-board-insert-section "Start Here")
    (if (> remote-count 0)
        (progn
          (aaron-ui-board-insert-field
           "1 · Connect" "Choose Open REPL on a remote profile below.")
          (aaron-ui-board-insert-field
           "2 · Work" "Use the new REPL from your source buffer or notebook.")
          (aaron-ui-board-insert-field
           "Profiles stored"
           (format "%s — this is where profiles are stored, not necessarily the compute host."
                   (my/jupyter-management-target-label my/jupyter-board--target))))
      (aaron-ui-board-insert-field
       "1 · Add" "Create an SSH profile for the machine that will run Python.")
      (aaron-ui-board-insert-field
       "2 · Connect" "Open its REPL; this starts the remote kernel and SSH tunnels.")
      (aaron-ui-board-insert-field
       "3 · Work" "Evaluate code in the REPL or from an associated source buffer."))
    (aaron-ui-board-insert-field
     "Launcher"
     (cond (ready (if local "Ready" "Checked on the selected target when used"))
           (t "Setup needed — remote_ikernel is not installed"))
     (if ready 'aaron-ui-board-good 'aaron-ui-board-bad))
    (insert "   ")
    (aaron-ui-board-insert-actions
     `((:label "Quick Add SSH" :command my/jupyter-remote-quick-add :primary t
               :help "Create a normal SSH/Python profile with a guided form")
       (:label "Usage Guide" :command my/jupyter-board-help
               :help "Explain setup, targets, profiles, and daily use")
       (:label "Advanced Add" :command my/jupyter-remote-add
               :help "Configure schedulers, jump hosts, and custom launch commands")
       ,@(when (and local (not ready))
           '((:label "Install remote_ikernel" :command my/jupyter-board-reinstall
                     :help "Install the vendored launcher into the configured Python")))))
    (insert "\n\n")))

(defun my/jupyter-board--insert-remote-profiles (entries)
  "Insert the primary remote-kernel management section for ENTRIES."
  (aaron-ui-board-insert-section "Remote Kernels" (length entries) 'success)
  (insert "   ")
  (aaron-ui-board-insert-actions
   `((:label "Add SSH Profile" :command my/jupyter-remote-quick-add :primary t
             :help "Add a normal SSH/Python remote kernel")
     (:label "Add Course PyTorch" :command my/jupyter-board-add-course-pytorch
             :help "Add the /home/hc/Desktop/9444 Python 3.13 CUDA environment")
     (:label "Advanced Add" :command my/jupyter-remote-add
             :help "Add a scheduler, jump-host, or custom launcher profile")
     (:label "Refresh Profiles" :command my/jupyter-board-refresh
             :help "Reload kernelspec profiles from the selected target")
     (:label "Agent / CLI Guide" :command my/jupyter-board-open-agent-guide
             :help "Open the non-interactive management API documentation")
     ,@(when (cl-some (lambda (entry)
                        (equal (plist-get entry :group) "temporary"))
                      entries)
         '((:label "Clean TEMP" :command my/jupyter-board-clean-temporary
                   :help "Delete all temporary profiles after confirmation")))))
  (insert "\n\n")
  (if entries
      (progn
        (insert "   "
                (propertize
                 "SAVED profiles are protected; TEMP profiles are eligible for bulk cleanup."
                 'face 'aaron-ui-board-meta)
                "\n\n")
        (mapc #'my/jupyter-board--insert-entry entries))
    (aaron-ui-board-insert-empty
     "No remote profile is configured yet. Choose Add SSH Profile above.")))

(defun my/jupyter-board--insert-advanced
    (connections project other)
  "Insert technical CONNECTIONS, PROJECT, and OTHER kernel resources."
  (my/jupyter-board--insert-context)
  (when (my/jupyter-management-local-target-p my/jupyter-board--target)
    (let* ((stale (cl-remove-if-not
                   (lambda (entry) (plist-get entry :stale)) connections))
           (current (cl-remove-if
                     (lambda (entry) (plist-get entry :stale)) connections))
           (visible (if my/jupyter-board--show-stale-connections
                        connections current)))
      (aaron-ui-board-insert-section "Connection Files"
                                     (length connections) 'warning)
      (when stale
        (insert "   ")
        (aaron-ui-board-insert-actions
         `((:label ,(if my/jupyter-board--show-stale-connections
                        "Hide old files"
                      (format "Show %d old files" (length stale)))
                   :command my/jupyter-board-toggle-stale-connections
                   :help "Old connection files are hidden to keep this page manageable")))
        (insert "\n\n"))
      (if visible
          (mapc #'my/jupyter-board--insert-connection visible)
        (aaron-ui-board-insert-empty
         (if stale
             (format "No current connection files; %d old files are hidden."
                     (length stale))
           "No local connection files.")))))
  (my/jupyter-board--insert-group "Noema Project Kernels" project 'success)
  (my/jupyter-board--insert-group "Local / Target Kernels" other 'muted))

(defun my/jupyter-board--render ()
  "Render the current passive Jupyter snapshots."
  (let* ((remote (cl-remove-if-not (lambda (entry) (plist-get entry :remote))
                                   my/jupyter-board--entries))
         (project (cl-remove-if-not
                   (lambda (entry) (eq (plist-get entry :origin) 'noema-project))
                   my/jupyter-board--entries))
         (other (cl-remove-if (lambda (entry)
                                (or (plist-get entry :remote)
                                    (eq (plist-get entry :origin) 'noema-project)))
                              my/jupyter-board--entries)))
    (let ((inhibit-read-only t))
      (aaron-ui-board-set-header
       "Remote Kernels" 'server
       (format "Profiles on %s"
               (my/jupyter-management-target-label my/jupyter-board--target)))
      (aaron-ui-board-render
       (lambda ()
         (aaron-ui-board-insert-page-header
          "Remote Kernel Manager" :icon 'server
          :subtitle (format "Run Jupyter code on another machine · profiles stored on %s"
                            (my/jupyter-management-target-label
                             my/jupyter-board--target))
          :stats `((,(format "%d remote profile%s" (length remote)
                             (if (= (length remote) 1) "" "s")) . info)
                   (,(format "%d active session%s" (length my/jupyter-board--runtimes)
                             (if (= (length my/jupyter-board--runtimes) 1) "" "s"))
                    . success))
          :actions `((:label "Quick Add SSH" :command my/jupyter-remote-quick-add
                             :primary t :help "Create an SSH remote kernel profile")
                     (:label "Target" :command my/jupyter-board-select-target
                             :help "Choose where kernelspec profiles are stored")
                     (:label "Refresh" :command my/jupyter-board-refresh
                             :help "Refresh profiles and live sessions")
                     (:label "Guide" :command my/jupyter-board-help
                             :help "Open the usage guide")
                     (:label ,(if my/jupyter-board--show-advanced
                                  "Hide technical" "Show technical")
                             :command my/jupyter-board-toggle-advanced
                             :help "Toggle local kernels, connection files, and diagnostics")))
         (my/jupyter-board--insert-provider-errors)
         (my/jupyter-board--insert-quick-start (length remote))
         (my/jupyter-board--insert-remote-profiles remote)
         (aaron-ui-board-insert-section "Active Sessions"
                                        (length my/jupyter-board--runtimes) 'success)
         (if my/jupyter-board--runtimes
             (mapc #'my/jupyter-board--insert-runtime my/jupyter-board--runtimes)
           (aaron-ui-board-insert-empty
            "No session is running. Open a remote profile when you are ready."))
         (if my/jupyter-board--show-advanced
             (my/jupyter-board--insert-advanced
              my/jupyter-board--connections project other)
           (progn
             (aaron-ui-board-insert-section "Technical Resources")
             (insert "   ")
             (aaron-ui-board-insert-actions
              '((:label "Show kernels, connections & diagnostics"
                        :command my/jupyter-board-toggle-advanced
                        :help "Expand the technical management sections")
                (:label "Commands" :command my/jupyter-board-dispatch
                        :help "Open every Jupyter Board command")))
             (insert "\n\n")))
         (aaron-ui-board-insert-key-hints
          "Keys: a quick add  e edit  r open REPL  g refresh  T target  v technical view  ? guide  M all commands"))))))

(defun my/jupyter-board-refresh ()
  "Refresh all Jupyter providers without starting services or kernels."
  (interactive)
  (unless (derived-mode-p 'my/jupyter-board-mode)
    (user-error "Not in a Jupyter Board"))
  (unless my/jupyter-board--target
    (setq my/jupyter-board--target (remote-get-target "local")))
  (cl-incf my/jupyter-board--refresh-generation)
  (let ((buffer (current-buffer))
        (generation my/jupyter-board--refresh-generation)
        (target my/jupyter-board--target))
    (setq my/jupyter-board--loading '(specs connections noema)
          my/jupyter-board--errors nil
          my/jupyter-board--runtimes
          (append
           (when (my/jupyter-management-local-target-p target)
             (my/jupyter-management-emacs-clients))
           (when (fboundp 'my/noema-jupyter-runtime-snapshot)
             (my/noema-jupyter-runtime-snapshot
              (my/jupyter-management-target-id target)))))
    (my/jupyter-board--render)
    (my/jupyter-management-discover-specs
     target (lambda (value error)
              (my/jupyter-board--set-provider-result
               buffer generation 'specs value error)))
    (my/jupyter-management-discover-connections
     target (lambda (value error)
              (my/jupyter-board--set-provider-result
               buffer generation 'connections value error)))
    (if (fboundp 'my/noema-api-call)
        (my/noema-api-call
         "aaronnote:api:jupyter-cell:tasks" []
         (lambda (value error)
           (my/jupyter-board--set-provider-result
            buffer generation 'noema value
            (when error
              (or (my/jupyter-board--alist-get 'message error)
                  (format "%s" error))))))
      (my/jupyter-board--set-provider-result
       buffer generation 'noema nil "Noema integration is unavailable"))))

(defun my/jupyter-board ()
  "Open the Jupyter management board."
  (interactive)
  (let ((source (current-buffer))
        (buffer (get-buffer-create my/jupyter-board-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/jupyter-board-mode)
        (my/jupyter-board-mode)
        (setq-local my/jupyter-board--source-buffer source
                    my/jupyter-board--target (remote-get-target "local")
                    my/jupyter-board--show-advanced
                    my/jupyter-board-show-advanced-default
                    aaron-ui-board-refresh-function #'my/jupyter-board-refresh)
        (aaron-ui-board-set-header "Remote Kernels" 'server)
        (use-local-map (copy-keymap aaron-ui-board-mode-map))
        (my/jupyter-board--setup-keys))
      (unless (eq source buffer)
        (setq my/jupyter-board--source-buffer source))
      (my/jupyter-board-refresh))
    (pop-to-buffer buffer)))

(defun my/jupyter-board-toggle-advanced ()
  "Toggle technical Jupyter resources in the current board."
  (interactive)
  (unless (derived-mode-p 'my/jupyter-board-mode)
    (user-error "Not in the Remote Kernel Manager"))
  (setq my/jupyter-board--show-advanced
        (not my/jupyter-board--show-advanced))
  (my/jupyter-board--render))

(defun my/jupyter-board-toggle-stale-connections ()
  "Toggle visibility of old local Jupyter connection files."
  (interactive)
  (unless (derived-mode-p 'my/jupyter-board-mode)
    (user-error "Not in the Remote Kernel Manager"))
  (setq my/jupyter-board--show-stale-connections
        (not my/jupyter-board--show-stale-connections))
  (my/jupyter-board--render))

(defun my/jupyter-board-help ()
  "Show a practical guide to remote kernels in Emacs."
  (interactive)
  (with-help-window "*Remote Kernel Guide*"
    (princ "Remote Kernel Manager\n\n")
    (princ "最常用的 SSH 工作流\n\n")
    (princ "1. 通常保持 Target 为 Local。Target 表示 kernelspec 配置保存在哪里；")
    (princ "真正运行计算的服务器由 SSH host 决定。\n")
    (princ "2. 选择 Quick Add SSH，填写 user@host（非标准端口可写 user@host:2222）。\n")
    (princ "3. 远端需要能运行 Python 和 ipykernel。默认启动命令适用于常见 Python 3 环境。\n")
    (princ "4. 在对应 profile 下选择 Open REPL。Emacs 会启动远端 kernel、建立五个 Jupyter 端口的 SSH tunnel，并打开 REPL。\n")
    (princ "5. 从 Python 源码 buffer 打开 Board 后，Open REPL 会自动关联该 buffer。\n")
    (princ "   C-c C-c 运行当前行/选区，C-c C-b 运行整个 buffer，C-c C-z 跳到 REPL。\n\n")
    (princ "首次使用前\n\n")
    (princ "• 建议先在终端确认 ssh user@host 能登录，并配置 SSH key；交互式密码在后台启动时不够稳定。\n")
    (princ "• 在服务器确认 python3 -c \"import ipykernel\" 成功。\n")
    (princ "• 如果本地 remote_ikernel 未安装，展开 Technical Resources 后使用 Reinstall tool。\n\n")
    (princ "Profile 类型\n\n")
    (princ "• SAVED：长期保留，批量清理不会删除，适合整学期使用。\n")
    (princ "• TEMP：试验配置，可从页面一次清理。\n")
    (princ "• Slurm/PBS/SGE、jump hosts 和自定义启动命令请使用 Advanced Add。\n\n")
    (princ "排错\n\n")
    (princ "先查看 Diagnostics 和 Command log。连接失败不会静默改用本地 kernel；")
    (princ "修好 SSH、远端 Python 或工作目录后，再次 Open REPL 即可。\n")))

(defun my/jupyter-board-open-agent-guide ()
  "Show the non-interactive API intended for an agent or background task."
  (interactive)
  (with-help-window "*Remote Kernel Agent Guide*"
    (princ "Remote Kernel Manager · Agent API\n\n")
    (princ "不要模拟点击按钮，也不要调用 transient。后台任务直接调用：\n\n")
    (princ "  (my/jupyter-board-add-course-pytorch\n")
    (princ "   :host \"Aaron-WSL2\"\n")
    (princ "   :callback (lambda (output error) ...))\n\n")
    (princ "这个函数不会读取 minibuffer，也不会弹出确认框。它会在当前 target（默认 Local）\n")
    (princ "上创建/替换一个 SAVED profile，并把命令输出写入 *Jupyter Board Log*。\n\n")
    (princ "预设值\n\n")
    (princ "  name        Python 3.13 PyTorch CUDA\n")
    (princ "  workdir     /home/hc/Desktop/9444\n")
    (princ "  interpreter /home/hc/Desktop/9444/.conda/bin/python\n")
    (princ "  kernel_cmd  <interpreter> -m ipykernel_launcher -f {connection_file}\n")
    (princ "  group       core / SAVED\n\n")
    (princ "host 必须是 SSH config 中的别名，或 user@host[:port]。\n")
    (princ "成功后刷新 Board，在 profile 上点击 Open REPL 即可启动远端 CUDA kernel。\n")))

(defun my/jupyter-board-open-log ()
  "Open the Jupyter Board command log."
  (interactive)
  (pop-to-buffer (get-buffer-create my/jupyter-board-log-buffer-name)))

(defun my/jupyter-board-open-source ()
  "Open the vendored remote_ikernel source directory."
  (interactive)
  (dired my/jupyter-remote-ikernel-source-directory))

(defun my/jupyter-board-open-resource ()
  "Open the most useful resource for the entry at point."
  (interactive)
  (let ((entry (my/jupyter-board--require-entry)))
    (pcase (plist-get entry :kind)
      ('kernelspec (dired (plist-get entry :resource-dir)))
      ('connection (find-file (plist-get entry :file)))
      ('runtime
       (cond
        ((buffer-live-p (plist-get entry :buffer))
         (pop-to-buffer (plist-get entry :buffer)))
        ((plist-get entry :source-file)
         (find-file (plist-get entry :source-file)))
        ((plist-get entry :log-file)
         (find-file (plist-get entry :log-file)))
        (t (user-error "This runtime has no openable resource"))))
      (_ (user-error "No openable resource at point")))))

(defun my/jupyter-board--log-command (program args)
  "Append PROGRAM and ARGS to the command log and return its buffer."
  (let ((buffer (get-buffer-create my/jupyter-board-log-buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format-time-string "\n[%Y-%m-%d %H:%M:%S] "))
      (insert (mapconcat #'shell-quote-argument (cons program args) " ") "\n"))
    buffer))

(defun my/jupyter-board--start-command (program args &optional callback)
  "Run PROGRAM ARGS asynchronously, then invoke CALLBACK with output."
  (unless (and program (file-executable-p program))
    (user-error "Executable is unavailable: %s" (or program "<unset>")))
  (let* ((buffer (my/jupyter-board--log-command program args))
         (process
          (make-process
           :name "jupyter-board-command"
           :buffer buffer
           :stderr buffer
           :command (cons program args)
           :noquery t
           :filter
           (lambda (process chunk)
             (process-put process 'my/output
                          (concat (or (process-get process 'my/output) "") chunk))
             (when-let* ((log (process-buffer process)))
               (with-current-buffer log
                 (goto-char (point-max))
                 (insert chunk))))
           :sentinel
           (lambda (process _event)
             (when (and (memq (process-status process) '(exit signal))
                        (not (process-get process 'my/finalize-scheduled)))
               ;; A process sentinel can run before Emacs delivers its final
               ;; filter chunk.  Finalize on the next event-loop turn.
               (process-put process 'my/finalize-scheduled t)
               (run-at-time
                0 nil
                (lambda (finished done-callback)
                  (unless (process-get finished 'my/callback-done)
                    (process-put finished 'my/callback-done t)
                    (let ((ok (zerop (process-exit-status finished)))
                          (output (or (process-get finished 'my/output) "")))
                      (if ok
                          (progn
                            (when done-callback (funcall done-callback output))
                            (when-let* ((board (get-buffer my/jupyter-board-buffer-name)))
                              (with-current-buffer board
                                (when (derived-mode-p 'my/jupyter-board-mode)
                                  (my/jupyter-board-refresh))))
                            (message "Jupyter command completed"))
                        (display-buffer (process-buffer finished))
                        (message "Jupyter command failed; see %s"
                                 my/jupyter-board-log-buffer-name)))))
                process callback))))))
    process))

(defun my/jupyter-board--start-target-command (target kind args &optional callback)
  "Run command KIND with ARGS on TARGET, then invoke CALLBACK on success."
  (if (my/jupyter-management-local-target-p target)
      (my/jupyter-board--start-command
       (my/jupyter-management-command target kind) args callback)
    (let* ((program (my/jupyter-management-command target kind))
           (log (my/jupyter-board--log-command program args)))
      (remote-exec-async
       program :args args :context (my/jupyter-management-context target)
       :filesystem-effects 'metadata
       :name (format "jupyter-action-%s"
                     (my/jupyter-management-target-id target))
       :callback
       (lambda (result)
         (with-current-buffer log
           (goto-char (point-max))
           (insert (remote-exec-result-stdout result))
           (unless (string-empty-p (remote-exec-result-stderr result))
             (insert "\n" (remote-exec-result-stderr result))))
         (if (zerop (remote-exec-result-status result))
             (progn
               (when callback (funcall callback (remote-exec-result-stdout result)))
               (when-let* ((board (get-buffer my/jupyter-board-buffer-name)))
                 (with-current-buffer board (my/jupyter-board-refresh)))
               (message "Remote Jupyter command completed"))
           (display-buffer log)
           (message "Remote Jupyter command failed; see %s"
                    my/jupyter-board-log-buffer-name)))))))

(defun my/jupyter-board-select-target ()
  "Select the local or configured Remote target."
  (interactive)
  (let ((target (remote-read-target "Jupyter target: ")))
    (when target
      (setq my/jupyter-board--target target
            my/jupyter-board--entries nil
            my/jupyter-board--connections nil
            my/jupyter-board--runtimes nil)
      (my/jupyter-board-refresh))))

(defun my/jupyter-board-doctor ()
  "Open Remote Doctor for the selected target without starting anything."
  (interactive)
  (remote-board-doctor my/jupyter-board--target))

(defun my/jupyter-board-reinstall ()
  "Reinstall vendored remote_ikernel into the configured Python."
  (interactive)
  (unless (my/jupyter-management-local-target-p my/jupyter-board--target)
    (user-error "Vendored remote_ikernel reinstall is local-only"))
  (my/jupyter-board--start-command
   my/jupyter-remote-ikernel-install-script '("install")))

(defun my/jupyter-board-describe ()
  "Describe the kernelspec, runtime, or connection at point."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry))
         (buffer (get-buffer-create my/jupyter-board-detail-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase (plist-get entry :kind)
          ('kernelspec
           (insert (format "%s\n\n" (plist-get entry :display-name)))
           (insert (format "Target: %s\nName: %s\nLanguage: %s\nResource: %s\n"
                           (plist-get entry :target-id) (plist-get entry :name)
                           (plist-get entry :language) (plist-get entry :resource-dir)))
           (insert (format "Health: %s — %s\n"
                           (plist-get (plist-get entry :health) :status)
                           (plist-get (plist-get entry :health) :detail)))
           (when (plist-get entry :remote)
             (insert (format "Group: %s\nInterface: %s\nHost: %s\n"
                             (plist-get entry :group)
                             (or (plist-get entry :interface) "-")
                             (or (plist-get entry :host) "-"))))
           (insert "\nkernel.json\n-----------\n")
           (let ((json-encoding-pretty-print t))
             (insert (json-encode (or (plist-get entry :raw)
                                      (plist-get entry :spec))))))
          ('connection
           (insert (format "%s\n\nValid: %s\nStale: %s\n\n"
                           (plist-get entry :file) (plist-get entry :valid)
                           (plist-get entry :stale)))
           (let ((json-encoding-pretty-print t))
             (insert (json-encode (or (plist-get entry :payload) '())))))
          ('runtime
           (insert (format "%s\n\n" (or (plist-get entry :title) "Jupyter runtime")))
           (pp entry (current-buffer)))
          (_ (pp entry (current-buffer))))
        (insert "\n")
        (special-mode)))
    (pop-to-buffer buffer)))

(defun my/jupyter-board-repl ()
  "Run, connect, or open a REPL for the entry at point."
  (interactive)
  (let ((entry (my/jupyter-board--require-entry)))
    (pcase (plist-get entry :kind)
      ('kernelspec
       (my/jupyter-management-run-repl entry my/jupyter-board--source-buffer))
      ('connection
       (my/jupyter-management-connect-repl entry my/jupyter-board--source-buffer))
      ('runtime
       (cond
        ((buffer-live-p (plist-get entry :buffer))
         (pop-to-buffer (plist-get entry :buffer)))
        ((and (plist-get entry :connection-file)
              (file-readable-p (plist-get entry :connection-file)))
         (my/jupyter-management-connect-repl
          (list :valid t :file (plist-get entry :connection-file))
          my/jupyter-board--source-buffer))
        (t (user-error "This runtime has no client-accessible REPL endpoint"))))
      (_ (user-error "No REPL action for this entry")))))

(defun my/jupyter-board--noema-action (entry action)
  "Apply Noema ACTION to runtime ENTRY."
  (if (plist-get entry :noema-task)
      (let ((channel (format "aaronnote:api:jupyter-cell:%s" action))
            (body `((key . ,(plist-get entry :key))
                    (id . ,(plist-get entry :runtime-id))
                    (file . ,(or (plist-get entry :source-file) ""))
                    (kernel . ,(or (plist-get entry :kernel) ""))
                    (session . ,(or (plist-get entry :session) "default")))))
        (my/noema-api-call
         channel (vector body)
         (lambda (_result error)
           (if error
               (message "Noema runtime action failed: %s"
                        (or (my/jupyter-board--alist-get 'message error) error))
             (message "Noema runtime %s completed" action)
             (when-let* ((board (get-buffer my/jupyter-board-buffer-name)))
               (with-current-buffer board (my/jupyter-board-refresh)))))))
    (my/noema-jupyter-runtime-control
     (plist-get entry :runtime-id) action
     (lambda (_result error)
       (if error (message "Broker runtime action failed: %s" error)
         (when-let* ((board (get-buffer my/jupyter-board-buffer-name)))
           (with-current-buffer board (my/jupyter-board-refresh))))))))

(defun my/jupyter-board--runtime-action (action)
  "Apply lifecycle ACTION to the runtime at point."
  (let ((entry (my/jupyter-board--require-entry)))
    (unless (eq (plist-get entry :kind) 'runtime)
      (user-error "This action requires a live runtime"))
    (when (and (eq action 'restart) (plist-get entry :attached))
      (user-error "Attached runtimes cannot be restarted"))
    (when (and (memq action '(restart shutdown))
               (not (yes-or-no-p
                     (format "%s runtime %s? State may be lost. "
                             (capitalize (symbol-name action))
                             (or (plist-get entry :title) (plist-get entry :id))))))
      (user-error "Cancelled"))
    (pcase (plist-get entry :provider)
      ('emacs-repl
       (my/jupyter-management-client-action entry action)
       (my/jupyter-board-refresh))
      ((or 'noema 'noema-broker) (my/jupyter-board--noema-action entry action))
      (_ (user-error "Unsupported runtime provider")))))

(defun my/jupyter-board-interrupt ()
  "Interrupt the runtime at point."
  (interactive)
  (my/jupyter-board--runtime-action 'interrupt))

(defun my/jupyter-board-restart ()
  "Restart the runtime at point after confirmation."
  (interactive)
  (my/jupyter-board--runtime-action 'restart))

(defun my/jupyter-board-shutdown ()
  "Shutdown or disconnect the runtime at point after confirmation."
  (interactive)
  (my/jupyter-board--runtime-action 'shutdown))

(defun my/jupyter-board-delete ()
  "Delete the kernelspec or connection file at point with confirmation."
  (interactive)
  (let ((entry (my/jupyter-board--require-entry)))
    (pcase (plist-get entry :kind)
      ('connection
       (let ((file (plist-get entry :file)))
         (when (yes-or-no-p (format "Delete connection file %s? " file))
           (delete-file file)
           (my/jupyter-board-refresh))))
      ('kernelspec
       (when (eq (plist-get entry :origin) 'noema-project)
         (user-error "Project kernels are managed by Refresh Project Specs"))
       (let* ((name (plist-get entry :name))
              (remote (plist-get entry :remote))
              (core (equal (plist-get entry :group) "core"))
              (prompt (if core
                          (format "Delete protected Core kernel %s? " name)
                        (format "Delete kernel %s? " name))))
         (when (if core (yes-or-no-p prompt) (y-or-n-p prompt))
           (my/jupyter-board--start-target-command
            (plist-get entry :target)
            (if remote 'remote-ikernel 'jupyter)
            (if remote
                (list "manage" "--delete" name)
              (list "kernelspec" "remove" "-f" name))))))
      (_ (user-error "Delete is only available for kernelspecs and connection files")))))

(defun my/jupyter-board-set-group ()
  "Set the current remote kernel's Core/Temporary group."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry t))
         (current (plist-get entry :group))
         (group (completing-read "Remote kernel group: " '("core" "temporary")
                                 nil t nil nil current)))
    (unless (equal current group)
      (my/jupyter-board--start-target-command
       (plist-get entry :target) 'remote-ikernel
       (list "manage" "--set-group" (plist-get entry :name) group)))))

(defun my/jupyter-board-toggle-group ()
  "Toggle whether the current remote profile is saved or temporary."
  (interactive)
  (let* ((entry (my/jupyter-board--require-entry t))
         (current (plist-get entry :group))
         (group (if (equal current "core") "temporary" "core")))
    (when (yes-or-no-p
           (if (equal group "core")
               "Keep this profile and protect it from bulk cleanup? "
             "Make this profile temporary and eligible for bulk cleanup? "))
      (my/jupyter-board--start-target-command
       (plist-get entry :target) 'remote-ikernel
       (list "manage" "--set-group" (plist-get entry :name) group)))))

(defun my/jupyter-board-clean-temporary ()
  "Delete all Temporary remote kernels after confirmation."
  (interactive)
  (let ((names (mapcar (lambda (entry) (plist-get entry :name))
                       (cl-remove-if-not
                        (lambda (entry)
                          (and (plist-get entry :remote)
                               (equal (plist-get entry :group) "temporary")))
                        my/jupyter-board--entries))))
    (if (null names)
        (message "No Temporary remote kernels")
      (when (yes-or-no-p (format "Delete %d Temporary remote kernels? " (length names)))
        (my/jupyter-board--start-target-command
         my/jupyter-board--target 'remote-ikernel
         (append '("manage" "--delete") names))))))

(defun my/jupyter-board-install-python-kernel ()
  "Install the selected target's Python environment as a user kernelspec."
  (interactive)
  (let* ((target my/jupyter-board--target)
         (name (read-string "Kernelspec name: " "python3"))
         (display (read-string "Display name: "
                               (format "Python (%s)"
                                       (my/jupyter-management-target-label target)))))
    (unless (string-match-p "\\`[[:alnum:]_.-]+\\'" name)
      (user-error "Invalid kernelspec name"))
    (when (yes-or-no-p
           (format "Install Python environment on target %s as %s? "
                   (my/jupyter-management-target-id target) name))
      (my/jupyter-board--start-target-command
       target 'python
       (list "-m" "ipykernel" "install" "--user"
             "--name" name "--display-name" display)))))

(defun my/jupyter-board-refresh-project-specs ()
  "Explicitly regenerate Noema project kernelspecs on the local machine."
  (interactive)
  (unless (my/jupyter-management-local-target-p my/jupyter-board--target)
    (user-error "Noema project kernelspec repair is local-only"))
  (let ((script (expand-file-name
                 "site-lisp/noema/jupyter/scripts/install-kernelspecs.sh"
                 user-emacs-directory)))
    (unless (file-executable-p script)
      (user-error "Noema kernelspec installer is unavailable: %s" script))
    (when (yes-or-no-p "Regenerate Noema project kernelspecs? ")
      (my/jupyter-board--start-command script nil))))

(defun my/jupyter-board-clean-idle-runtimes ()
  "Ask Noema to clean runtimes eligible under its idle policy."
  (interactive)
  (unless (fboundp 'my/noema-api-call)
    (user-error "Noema integration is unavailable"))
  (let ((candidates
         (cl-remove-if
          (lambda (entry)
            (or (not (memq (plist-get entry :provider) '(noema noema-broker)))
                (plist-get entry :attached)
                (> (or (plist-get entry :running) 0) 0)))
          my/jupyter-board--runtimes)))
    (if (null candidates)
        (message "No idle Noema runtime candidates")
      (when (yes-or-no-p
             (format "Ask Noema to clean eligible idle/dead runtimes (%d visible)? "
                     (length candidates)))
        (my/noema-api-call
         "aaronnote:api:jupyter-cell:cleanup" [((force . :json-false))]
         (lambda (_result error)
           (if error
               (message "Noema cleanup failed: %s"
                        (or (my/jupyter-board--alist-get 'message error) error))
             (when-let* ((board (get-buffer my/jupyter-board-buffer-name)))
               (with-current-buffer board (my/jupyter-board-refresh))))))))))

(defun my/jupyter-board--transient-config-args (entry)
  "Return transient arguments that reproduce remote kernelspec ENTRY."
  (let* ((meta (plist-get entry :remote-meta))
         (config (my/jupyter-board--alist-get 'config meta))
         (argv (plist-get entry :argv))
         (value (lambda (key option)
                  (or (my/jupyter-board--alist-get key config)
                      (my/jupyter-board--argv-option argv option))))
         (interface (funcall value 'interface "--interface"))
         (host (funcall value 'host "--host"))
         (kernel-command (funcall value 'kernel_cmd "--kernel_cmd"))
         (name (or (my/jupyter-board--alist-get 'name config)
                   (let ((display (plist-get entry :display-name)))
                     (if (and host (string-prefix-p (format "SSH %s " host) display))
                         (substring display (length (format "SSH %s " host)))
                       display))))
         args)
    (dolist (pair `(("--interface=" . ,interface)
                    ("--name=" . ,name)
                    ("--kernel_cmd=" . ,kernel-command)
                    ("--group=" . ,(plist-get entry :group))
                    ("--host=" . ,host)
                    ("--language=" . ,(or (funcall value 'language "--language")
                                            (plist-get entry :language)))
                    ("--workdir=" . ,(funcall value 'workdir "--workdir"))
                    ("--cpus=" . ,(funcall value 'cpus "--cpus"))
                    ("--pe=" . ,(funcall value 'pe "--pe"))
                    ("--remote-precmd=" . ,(funcall value 'remote_precmd "--remote-precmd"))
                    ("--launch-cmd=" . ,(funcall value 'launch_cmd "--launch-cmd"))
                    ("--remote-launch-args=" . ,(funcall value 'remote_launch_args "--remote-launch-args"))))
      (when (and (cdr pair) (not (equal (cdr pair) "")))
        (push (concat (car pair) (format "%s" (cdr pair))) args)))
    (when (my/jupyter-board--alist-get 'verbose config) (push "--verbose" args))
    (when (my/jupyter-board--alist-get 'system config) (push "--system" args))
    (let ((tunnels (my/jupyter-board--alist-get 'tunnel_hosts config)))
      (when tunnels (push (concat "--tunnel-hosts=" (string-join tunnels ",")) args)))
    (nreverse args)))

(defun my/jupyter-board--remote-args-normalize (args)
  "Convert transient ARGS to remote_ikernel CLI arguments."
  (apply #'append
         (mapcar
          (lambda (arg)
            (if (string-prefix-p "--tunnel-hosts=" arg)
                (cons "--tunnel-hosts"
                      (split-string (substring arg (length "--tunnel-hosts=")) "," t "[[:space:]]*"))
              (list arg)))
          args)))

(defun my/jupyter-board--arg-value (args prefix)
  "Return the value of PREFIX from transient ARGS."
  (when-let* ((arg (cl-find-if (lambda (item) (string-prefix-p prefix item)) args)))
    (substring arg (length prefix))))

(defun my/jupyter-board--replace-arg (args prefix value)
  "In ARGS, replace PREFIX with VALUE or remove it when VALUE is empty."
  (let ((rest (cl-remove-if (lambda (arg) (string-prefix-p prefix arg)) args)))
    (if (and value (not (string-empty-p value)))
        (append rest (list (concat prefix value)))
      rest)))

(defun my/jupyter-board--read-required (prompt default history)
  "Read a non-empty value using PROMPT, DEFAULT, and HISTORY."
  (let ((value (string-trim (read-string prompt default history))))
    (when (string-empty-p value)
      (user-error "This value is required"))
    value))

(defun my/jupyter-board--validate-remote-args (args)
  "Validate remote_ikernel management ARGS before changing a profile."
  (let ((interface (my/jupyter-board--arg-value args "--interface="))
        (name (my/jupyter-board--arg-value args "--name="))
        (command (my/jupyter-board--arg-value args "--kernel_cmd="))
        (host (my/jupyter-board--arg-value args "--host="))
        (cpus (my/jupyter-board--arg-value args "--cpus=")))
    (unless (member interface '("ssh" "local" "pbs" "sge" "sge_qrsh" "slurm" "lsf"))
      (user-error "Choose a supported launch interface"))
    (unless (and name (not (string-empty-p (string-trim name))))
      (user-error "Profile name is required"))
    (when (string-match-p "[\n\r]" name)
      (user-error "Profile name cannot contain a newline"))
    (unless (and command (not (string-empty-p (string-trim command))))
      (user-error "Remote kernel command is required"))
    (unless (string-match-p
             "{\\(?:host_\\)?connection_file}" command)
      (user-error
       "Kernel command must contain {connection_file} so Jupyter can connect"))
    (when (equal interface "ssh")
      (unless (and host (not (string-empty-p (string-trim host))))
        (user-error "SSH host is required"))
      (when (string-match-p "[[:space:]\n\r]" host)
        (user-error "SSH host cannot contain whitespace; use user@host:port")))
    (when (and cpus
               (not (and (string-match-p "\\`[0-9]+\\'" cpus)
                         (> (string-to-number cpus) 0))))
      (user-error "CPU count must be a positive integer"))
    t))

(defun my/jupyter-board--save-remote-profile (target args &optional origin)
  "Save remote profile ARGS on TARGET, replacing ORIGIN when renamed."
  (my/jupyter-board--validate-remote-args args)
  (my/jupyter-board--start-target-command
   target 'remote-ikernel
   (append '("manage" "--add")
           (my/jupyter-board--remote-args-normalize args))
   (lambda (output)
     (when (and origin
                (string-match "Added kernel \\['\\([^']+\\)'\\]" output))
       (let ((created (match-string 1 output)))
         (when (and (not (equal created origin))
                    (yes-or-no-p
                     (format "Profile is now %s. Delete its old copy %s? "
                             created origin)))
           (my/jupyter-board--start-target-command
            target 'remote-ikernel
            (list "manage" "--delete" origin))))))))

(defun my/jupyter-board--remote-wizard (&optional entry)
  "Create an SSH profile, or edit SSH profile ENTRY, with guided prompts."
  (let* ((editing (and entry t))
         (target (or (plist-get entry :target)
                     (and (boundp 'my/jupyter-board--target)
                          my/jupyter-board--target)
                     (remote-get-target "local")))
         (base (if entry
                   (my/jupyter-board--transient-config-args entry)
                 '("--interface=ssh" "--group=core" "--language=python")))
         (interface (or (my/jupyter-board--arg-value base "--interface=") "ssh")))
    (when (my/jupyter-management-local-target-p target)
      (let ((program (my/jupyter-management-command target 'remote-ikernel)))
        (unless (and program (file-executable-p program))
          (user-error
           "remote_ikernel is not installed; show Technical Resources and choose Reinstall tool"))))
    (if (and editing (not (equal interface "ssh")))
        (progn
          (message "Scheduler profiles use the advanced editor")
          (my/jupyter-remote-edit))
      (let* ((host (my/jupyter-board--read-required
                    "SSH host (user@host or user@host:port): "
                    (my/jupyter-board--arg-value base "--host=")
                    'my/jupyter-board-remote-host-history))
             (name (my/jupyter-board--read-required
                    "Profile name shown in the kernel picker: "
                    (or (my/jupyter-board--arg-value base "--name=") "Python")
                    'my/jupyter-board-remote-name-history))
             (workdir (string-trim
                       (read-string
                        "Remote working directory (optional): "
                        (my/jupyter-board--arg-value base "--workdir=")
                        'my/jupyter-board-remote-workdir-history)))
             (command
              (my/jupyter-board--read-required
               "Remote kernel command: "
               (or (my/jupyter-board--arg-value base "--kernel_cmd=")
                   "python3 -m ipykernel_launcher -f {connection_file}")
               'my/jupyter-board-remote-command-history))
             (language (or (my/jupyter-board--arg-value base "--language=")
                           (plist-get entry :language)
                           "python"))
             (current-group (or (my/jupyter-board--arg-value base "--group=")
                                "core"))
             (lifetime
              (completing-read
               "Profile lifetime: " '("Saved (protected)" "Temporary") nil t nil nil
               (if (equal current-group "temporary")
                   "Temporary" "Saved (protected)")))
             (group (if (string-prefix-p "Temporary" lifetime)
                        "temporary" "core"))
             (args base))
        (dolist (pair `(("--interface=" . "ssh")
                        ("--host=" . ,host)
                        ("--name=" . ,name)
                        ("--language=" . ,language)
                        ("--group=" . ,group)
                        ("--workdir=" . ,workdir)
                        ("--kernel_cmd=" . ,command)))
          (setq args (my/jupyter-board--replace-arg args (car pair) (cdr pair))))
        (my/jupyter-board--validate-remote-args args)
        (when (yes-or-no-p
               (format "%s '%s' on %s%s? "
                       (if editing "Update" "Create") name host
                       (if (string-empty-p workdir) ""
                         (format " in %s" workdir))))
          (my/jupyter-board--save-remote-profile
           target args (plist-get entry :name)))))))

(defun my/jupyter-remote-quick-add ()
  "Create a normal SSH remote-kernel profile with guided prompts."
  (interactive)
  (my/jupyter-board--remote-wizard))

(defun my/jupyter-remote-edit-guided ()
  "Edit the current SSH profile with guided prompts."
  (interactive)
  (my/jupyter-board--remote-wizard (my/jupyter-board--require-entry t)))

(cl-defun my/jupyter-board-add-course-pytorch
    (&key host target callback)
  "Add the 9444 Python 3.13/PyTorch CUDA profile without prompting.
HOST is an SSH alias or user@host[:port].  TARGET is where the kernelspec
is stored and defaults to the current Board target or Local.  CALLBACK,
when supplied, receives OUTPUT and nil after the asynchronous command
completes successfully.  This entry point is intended for agents and
background tasks; it never reads the minibuffer or asks for confirmation."
  (interactive
   (list :host
         (my/jupyter-board--read-required
          "Course PyTorch SSH host (alias or user@host[:port]): "
          "Aaron-WSL2" 'my/jupyter-board-remote-host-history)
         :target my/jupyter-board--target))
  (let* ((target (or target my/jupyter-board--target (remote-get-target "local")))
         (profile my/jupyter-board-course-pytorch-profile)
         (host (and host (string-trim host)))
         (args (list
                "--interface=ssh"
                (concat "--host=" (or host ""))
                (concat "--name=" (plist-get profile :name))
                (concat "--language=" (plist-get profile :language))
                (concat "--group=" (plist-get profile :group))
                (concat "--workdir=" (plist-get profile :workdir))
                (concat "--kernel_cmd=" (plist-get profile :kernel-command)))))
    (unless (and host (not (string-empty-p host)))
      (user-error "Course PyTorch profile requires an SSH host"))
    (my/jupyter-board--validate-remote-args args)
    (my/jupyter-board--start-target-command
     target 'remote-ikernel
     (append '("manage" "--add")
             (my/jupyter-board--remote-args-normalize args))
     (lambda (output)
       (when callback (funcall callback output nil))))))

(defun my/jupyter-remote-add-run ()
  "Create or replace a remote kernelspec from the active transient."
  (interactive)
  (let* ((args (transient-args 'my/jupyter-remote-add-dispatch))
         (origin my/jupyter-board--edit-origin)
         (target (or my/jupyter-board--edit-target
                     (remote-get-target "local"))))
    (my/jupyter-board--validate-remote-args args)
    (setq my/jupyter-board--edit-origin nil
          my/jupyter-board--edit-target nil)
    (my/jupyter-board--save-remote-profile target args origin)))

(defun my/jupyter-remote-add ()
  "Open the advanced remote kernel creation transient."
  (interactive)
  (setq my/jupyter-board--edit-origin nil
        my/jupyter-board--edit-target my/jupyter-board--target)
  (transient-setup 'my/jupyter-remote-add-dispatch nil nil
                   :value '("--interface=ssh" "--group=temporary")))

(defun my/jupyter-remote-edit ()
  "Edit the current remote kernel through the creation transient."
  (interactive)
  (let ((entry (my/jupyter-board--require-entry t)))
    (setq my/jupyter-board--edit-origin (plist-get entry :name)
          my/jupyter-board--edit-target (plist-get entry :target))
    (transient-setup 'my/jupyter-remote-add-dispatch nil nil
                     :value (my/jupyter-board--transient-config-args entry))))

(transient-define-prefix my/jupyter-remote-add-dispatch ()
  "Create or edit a remote_ikernel kernelspec."
  [["Identity"
    ("-i" "Interface" "--interface=" :choices ("ssh" "local" "pbs" "sge" "sge_qrsh" "slurm" "lsf"))
    ("-n" "Name" "--name=")
    ("-k" "Kernel command" "--kernel_cmd=")
    ("-g" "Group" "--group=" :choices ("core" "temporary"))]
   ["Remote"
    ("-x" "Host" "--host=")
    ("-l" "Language" "--language=")
    ("-w" "Workdir" "--workdir=")
    ("-t" "Tunnel hosts CSV" "--tunnel-hosts=")]
   ["Scheduler"
    ("-c" "CPUs" "--cpus=")
    ("-p" "Parallel env" "--pe=")
    ("-P" "Remote pre-command" "--remote-precmd=")
    ("-L" "Launch command" "--launch-cmd=")
    ("-A" "Remote launch args" "--remote-launch-args=")]
   ["Flags"
    ("-v" "Verbose" "--verbose")
    ("-s" "System kernelspec" "--system")]
   ["Apply"
    ("a" "Add / replace" my/jupyter-remote-add-run)]])

(transient-define-prefix my/jupyter-board-dispatch ()
  "Jupyter Board command surface."
  [["Board"
    ("j" "Open board" my/jupyter-board)
    ("T" "Select target" my/jupyter-board-select-target)
    ("g" "Refresh" my/jupyter-board-refresh)
    ("v" "Technical view" my/jupyter-board-toggle-advanced)
    ("?" "Usage guide" my/jupyter-board-help)
    ("RET" "Describe" my/jupyter-board-describe)
    ("o" "Open resource" my/jupyter-board-open-resource)
    ("r" "REPL / connect" my/jupyter-board-repl)]
   ["Runtime"
    ("i" "Interrupt" my/jupyter-board-interrupt)
    ("R" "Restart" my/jupyter-board-restart)
    ("k" "Shutdown" my/jupyter-board-shutdown)
    ("K" "Clean idle" my/jupyter-board-clean-idle-runtimes)]
   ["Remote"
    ("a" "Quick add SSH" my/jupyter-remote-quick-add)
    ("A" "Advanced add" my/jupyter-remote-add)
    ("e" "Guided edit" my/jupyter-remote-edit-guided)
    ("E" "Advanced edit" my/jupyter-remote-edit)
    ("m" "Set group" my/jupyter-board-set-group)
    ("C" "Clean temporary" my/jupyter-board-clean-temporary)]
   ["Maintenance"
    ("d" "Delete" my/jupyter-board-delete)
    ("p" "Install Python" my/jupyter-board-install-python-kernel)
    ("P" "Refresh project specs" my/jupyter-board-refresh-project-specs)
    ("I" "Reinstall tool" my/jupyter-board-reinstall)
    ("D" "Target doctor" my/jupyter-board-doctor)
    ("S" "Open source" my/jupyter-board-open-source)
    ("l" "Open log" my/jupyter-board-open-log)]])

(defun my/jupyter-board--setup-keys ()
  "Install local keybindings for the Jupyter Board."
  (local-set-key (kbd "g") #'my/jupyter-board-refresh)
  (local-set-key (kbd "T") #'my/jupyter-board-select-target)
  (local-set-key (kbd "a") #'my/jupyter-remote-quick-add)
  (local-set-key (kbd "A") #'my/jupyter-remote-add)
  (local-set-key (kbd "RET") #'my/jupyter-board-describe)
  (local-set-key (kbd "r") #'my/jupyter-board-repl)
  (local-set-key (kbd "i") #'my/jupyter-board-interrupt)
  (local-set-key (kbd "R") #'my/jupyter-board-restart)
  (local-set-key (kbd "k") #'my/jupyter-board-shutdown)
  (local-set-key (kbd "e") #'my/jupyter-remote-edit-guided)
  (local-set-key (kbd "E") #'my/jupyter-remote-edit)
  (local-set-key (kbd "m") #'my/jupyter-board-set-group)
  (local-set-key (kbd "d") #'my/jupyter-board-delete)
  (local-set-key (kbd "C") #'my/jupyter-board-clean-temporary)
  (local-set-key (kbd "o") #'my/jupyter-board-open-resource)
  (local-set-key (kbd "p") #'my/jupyter-board-install-python-kernel)
  (local-set-key (kbd "P") #'my/jupyter-board-refresh-project-specs)
  (local-set-key (kbd "D") #'my/jupyter-board-doctor)
  (local-set-key (kbd "v") #'my/jupyter-board-toggle-advanced)
  (local-set-key (kbd "?") #'my/jupyter-board-help)
  (local-set-key (kbd "M") #'my/jupyter-board-dispatch))

(provide 'init-jupyter-board)
;;; init-jupyter-board.el ends here
