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
(defvar my/jupyter-board--edit-origin nil)
(defvar my/jupyter-board--edit-target nil)

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
  (get-text-property (point) 'my/jupyter-board-entry))

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
     '((:label "Target" :command my/jupyter-board-select-target :primary t
               :help "Select local or Remote target")
       (:label "Refresh" :command my/jupyter-board-refresh :help "Refresh snapshots")
       (:label "Doctor" :command my/jupyter-board-doctor :help "Open target doctor")
       (:label "Log" :command my/jupyter-board-open-log :help "Open command log")))
    (insert "\n\n")))

(defun my/jupyter-board--entry-detail (entry)
  "Return a compact detail string for ENTRY."
  (if (plist-get entry :remote)
      (string-join
       (delq nil
             (list (and (plist-get entry :interface)
                        (format "interface=%s" (plist-get entry :interface)))
                   (and (plist-get entry :host)
                        (format "host=%s" (plist-get entry :host)))
                   (and (plist-get entry :workdir)
                        (format "workdir=%s" (plist-get entry :workdir)))
                   (and (plist-get entry :kernel-command)
                        (format "command=%s" (plist-get entry :kernel-command)))))
       "  ")
    (string-join
     (delq nil
           (list (format "%s" (plist-get entry :resource-dir))
                 (when-let* ((health (plist-get entry :health)))
                   (format "%s: %s" (plist-get health :status)
                           (plist-get health :detail)))))
     "  ")))

(defun my/jupyter-board--insert-entry (entry)
  "Insert one kernelspec ENTRY."
  (aaron-ui-board-insert-row
   :id (or (plist-get entry :id) (plist-get entry :name))
   :icon (if (plist-get entry :remote) 'remote 'jupyter)
   :badge (if (plist-get entry :remote)
              (upcase (plist-get entry :group))
            (or (plist-get entry :language) "LOCAL"))
   :badge-tone (pcase (plist-get entry :group)
                 ("core" 'success)
                 ("temporary" 'warning)
                 (_ 'muted))
   :title (plist-get entry :display-name)
   :meta (format "%s  %s" (plist-get entry :name)
                 (or (plist-get entry :language) ""))
   :detail (my/jupyter-board--entry-detail entry)
   :action (lambda (_button) (my/jupyter-board-describe))
   :help "RET details; r REPL; e edit; d delete"
   :properties (list 'my/jupyter-board-entry entry)))

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
     :help "RET details; i interrupt; R restart; k shutdown; o open"
     :properties (list 'my/jupyter-board-entry entry))))

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
     :help "r connect REPL; o open JSON; d delete"
     :properties (list 'my/jupyter-board-entry entry))))

(defun my/jupyter-board--insert-group (title entries &optional tone)
  "Insert TITLE section containing ENTRIES, with optional badge TONE."
  (aaron-ui-board-insert-section title (length entries) tone)
  (if entries
      (progn
        (mapc #'my/jupyter-board--insert-entry entries)
        (insert "\n"))
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
    (aaron-ui-board-insert-section "Discovery")
    (dolist (provider my/jupyter-board--loading)
      (aaron-ui-board-insert-field (format "%s" provider) "loading…"
                                   'aaron-ui-board-dim))
    (dolist (entry my/jupyter-board--errors)
      (when (cdr entry)
        (aaron-ui-board-insert-field (format "%s" (car entry)) (cdr entry)
                                     'aaron-ui-board-bad)))
    (insert "\n")))

(defun my/jupyter-board--render ()
  "Render the current passive Jupyter snapshots."
  (let* ((remote (cl-remove-if-not (lambda (entry) (plist-get entry :remote))
                                   my/jupyter-board--entries))
         (project (cl-remove-if-not
                   (lambda (entry) (eq (plist-get entry :origin) 'noema-project))
                   my/jupyter-board--entries))
         (core (cl-remove-if-not (lambda (entry)
                                   (equal (plist-get entry :group) "core")) remote))
         (temporary (cl-remove-if-not (lambda (entry)
                                        (equal (plist-get entry :group) "temporary")) remote))
         (other (cl-remove-if (lambda (entry)
                                (or (plist-get entry :remote)
                                    (eq (plist-get entry :origin) 'noema-project)))
                              my/jupyter-board--entries)))
    (let ((inhibit-read-only t))
      (aaron-ui-board-render
       (lambda ()
         (aaron-ui-board-insert-page-header
          "Jupyter Board" :icon 'jupyter
          :subtitle "Passive kernelspec, runtime, connection and target management"
          :stats `((,(format "%d specs" (length my/jupyter-board--entries)) . info)
                   (,(format "%d runtimes" (length my/jupyter-board--runtimes)) . success)
                   (,(format "%d connections" (length my/jupyter-board--connections)) . warning))
          :actions '((:label "Target" :command my/jupyter-board-select-target :primary t)
                     (:label "Refresh" :command my/jupyter-board-refresh)
                     (:label "Add Remote" :command my/jupyter-remote-add)
                     (:label "Clean Idle" :command my/jupyter-board-clean-idle-runtimes)))
         (my/jupyter-board--insert-context)
         (my/jupyter-board--insert-provider-errors)
         (aaron-ui-board-insert-section "Live Runtimes"
                                        (length my/jupyter-board--runtimes) 'success)
         (if my/jupyter-board--runtimes
             (progn (mapc #'my/jupyter-board--insert-runtime
                          my/jupyter-board--runtimes) (insert "\n"))
           (aaron-ui-board-insert-empty "No active runtime snapshots."))
         (when (my/jupyter-management-local-target-p my/jupyter-board--target)
           (aaron-ui-board-insert-section "Connection Files"
                                          (length my/jupyter-board--connections) 'warning)
           (if my/jupyter-board--connections
               (progn (mapc #'my/jupyter-board--insert-connection
                            my/jupyter-board--connections) (insert "\n"))
             (aaron-ui-board-insert-empty "No local connection files.")))
         (my/jupyter-board--insert-group "Noema Project Kernels" project 'success)
         (my/jupyter-board--insert-group "Core Remote Kernels" core 'success)
         (my/jupyter-board--insert-group "Temporary Remote Kernels" temporary 'warning)
         (my/jupyter-board--insert-group "Local / Target Kernels" other 'muted)
         (aaron-ui-board-insert-key-hints
          "Keys: T target  g refresh  RET detail  r REPL/connect  i interrupt  R restart  k shutdown  a/e/m remote  p install Python  d delete  o open  D doctor  ? menu"))))))

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
                    aaron-ui-board-refresh-function #'my/jupyter-board-refresh)
        (aaron-ui-board-set-header "Jupyter Board" 'jupyter)
        (use-local-map (copy-keymap special-mode-map))
        (my/jupyter-board--setup-keys))
      (unless (eq source buffer)
        (setq my/jupyter-board--source-buffer source))
      (my/jupyter-board-refresh))
    (pop-to-buffer buffer)))

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
                 "lisp/roam/Noema/jupyter/scripts/install-kernelspecs.sh"
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

(defun my/jupyter-remote-add-run ()
  "Create or replace a remote kernelspec from the active transient."
  (interactive)
  (let* ((args (transient-args 'my/jupyter-remote-add-dispatch))
         (interface (my/jupyter-board--arg-value args "--interface="))
         (name (my/jupyter-board--arg-value args "--name="))
         (kernel-command (my/jupyter-board--arg-value args "--kernel_cmd="))
         (host (my/jupyter-board--arg-value args "--host="))
         (origin my/jupyter-board--edit-origin)
         (target (or my/jupyter-board--edit-target
                     (remote-get-target "local"))))
    (unless (and interface (not (string-empty-p interface)))
      (user-error "--interface is required"))
    (unless (and name (not (string-empty-p name)))
      (user-error "--name is required"))
    (unless (and kernel-command (not (string-empty-p kernel-command)))
      (user-error "--kernel_cmd is required"))
    (when (and (equal interface "ssh") (or (null host) (string-empty-p host)))
      (user-error "--host is required for SSH kernels"))
    (setq my/jupyter-board--edit-origin nil
          my/jupyter-board--edit-target nil)
    (my/jupyter-board--start-target-command
     target 'remote-ikernel
     (append '("manage" "--add") (my/jupyter-board--remote-args-normalize args))
     (lambda (output)
       (when (and origin
                  (string-match "Added kernel \\['\\([^']+\\)'\\]" output))
         (let ((created (match-string 1 output)))
           (when (and (not (equal created origin))
                      (yes-or-no-p (format "New kernel %s created; delete old %s? " created origin)))
             (my/jupyter-board--start-target-command
              target 'remote-ikernel
              (list "manage" "--delete" origin)))))))))

(defun my/jupyter-remote-add ()
  "Open the remote kernel creation transient."
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
    ("RET" "Describe" my/jupyter-board-describe)
    ("o" "Open resource" my/jupyter-board-open-resource)
    ("r" "REPL / connect" my/jupyter-board-repl)]
   ["Runtime"
    ("i" "Interrupt" my/jupyter-board-interrupt)
    ("R" "Restart" my/jupyter-board-restart)
    ("k" "Shutdown" my/jupyter-board-shutdown)
    ("K" "Clean idle" my/jupyter-board-clean-idle-runtimes)]
   ["Remote"
    ("a" "Add" my/jupyter-remote-add)
    ("e" "Edit" my/jupyter-remote-edit)
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
  (local-set-key (kbd "a") #'my/jupyter-remote-add)
  (local-set-key (kbd "RET") #'my/jupyter-board-describe)
  (local-set-key (kbd "r") #'my/jupyter-board-repl)
  (local-set-key (kbd "i") #'my/jupyter-board-interrupt)
  (local-set-key (kbd "R") #'my/jupyter-board-restart)
  (local-set-key (kbd "k") #'my/jupyter-board-shutdown)
  (local-set-key (kbd "e") #'my/jupyter-remote-edit)
  (local-set-key (kbd "m") #'my/jupyter-board-set-group)
  (local-set-key (kbd "d") #'my/jupyter-board-delete)
  (local-set-key (kbd "C") #'my/jupyter-board-clean-temporary)
  (local-set-key (kbd "o") #'my/jupyter-board-open-resource)
  (local-set-key (kbd "p") #'my/jupyter-board-install-python-kernel)
  (local-set-key (kbd "P") #'my/jupyter-board-refresh-project-specs)
  (local-set-key (kbd "D") #'my/jupyter-board-doctor)
  (local-set-key (kbd "?") #'my/jupyter-board-dispatch))

(provide 'init-jupyter-board)
;;; init-jupyter-board.el ends here
