;;; jupyter-linux-kernel-live-smoke.el --- Real Linux broker probe -*- lexical-binding: t; -*-
;; JUPYTER_LINUX_TARGET=<configured SSH target> make jupyter-linux-kernel-live-smoke
;; JUPYTER_LINUX_TARGET=<configured SSH target> make jupyter-linux-disconnect-live-smoke
;; JUPYTER_LINUX_CONFIG=<temporary Remote JSON> can pin a physical test host.
;; Requires python3, ipykernel, jupyter_client, and jupyter_core on the target.

(require 'init-aaronnote)
(require 'init-aaronnote-jupyter-runtime)
(require 'remote-config)
(require 'remote-framework)

(defun my/jupyter-linux-live--client
    (runtime code label &optional mode on-busy expected)
  "Run CODE through RUNTIME with the Noema Node protocol client.
MODE selects a client probe.  ON-BUSY runs after the target reported that
an execution started, allowing a test-owned transport cut at that point."
  (let* ((buffer (generate-new-buffer " *jupyter-linux-live-client*"))
         (default-directory user-emacs-directory)
         (process-environment (remote-client-process-environment))
         (exec-path (remote-client-exec-path))
         (node (or (getenv "JUPYTER_LINUX_NODE") (executable-find "node")
                   (error "Set JUPYTER_LINUX_NODE to a client-side Node executable")))
         (process
          (make-process
           :name "jupyter-linux-live-client" :buffer buffer :connection-type 'pipe
           :command (list node (expand-file-name
                                "test/jupyter-remote-live-client.mjs"
                                user-emacs-directory))
           :noquery t))
         (deadline (+ (float-time) 80))
         notified)
    (unwind-protect
        (progn
          (process-send-string
           process
           (json-serialize
            `((mode . ,(or mode "raw")) (label . ,label)
              (connectionInfo . ,(my/noema-jupyter-runtime-client-connection runtime))
              (code . ,code) (expected . ,(or expected "42")))))
          (process-send-eof process)
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output nil 0.05)
            (when (and on-busy (not notified)
                       (with-current-buffer buffer
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "INFLIGHT_BUSY" nil t))))
              (setq notified t)
              (funcall on-busy)))
          (when (and on-busy (not notified))
            (error "Linux Jupyter execution never reported busy"))
          (unless (and (not (process-live-p process))
                       (zerop (process-exit-status process)))
            (let* ((log-file (my/noema-jupyter-runtime-log-file runtime))
                   (kernel-log
                    (condition-case nil
                        (with-temp-buffer
                          (insert-file-contents log-file)
                          (let ((text (buffer-string)))
                            (substring text (max 0 (- (length text) 1800)))))
                      (error "<unavailable>"))))
              (error "Linux Jupyter client failed (alive=%S, log=%s): %s"
                     (my/noema-jupyter--alive-p runtime) kernel-log
                     (with-current-buffer buffer (buffer-string)))))
          (princ (with-current-buffer buffer (buffer-string))))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(defun my/jupyter-linux-live--cut-forwards (runtime)
  "Terminate RUNTIME's five test-owned SSH forward processes.
One multiplexed SSH master may exit with all of its forward clients, so each
process must be live before the cut, but need not require a separate kill."
  (let* ((group (my/noema-jupyter-runtime-channel-group runtime))
         (processes
          (mapcar
           (lambda (entry)
             (let* ((forward (cdr entry))
                    (process (and (remote-forward-p forward)
                                  (remote-forward-handle forward))))
               (unless (processp process)
                 (error "Channel %s has no owned forward process"
                        (car entry)))
               process))
           (remote-channel-group-channels group))))
    (unless (and (= (length processes) 5)
                 (cl-every #'process-live-p processes))
      (error "Expected five live SSH forwards before the cut"))
    (dolist (process processes)
      (when (process-live-p process)
        (delete-process process)))
    (unless (cl-every (lambda (process) (not (process-live-p process)))
                      processes)
      (error "SSH forward cut left a test-owned process alive"))
    (princ "CUT five test-owned SSH forwards during execution\n")))

(defun my/jupyter-linux-live--check-forward-route (runtime)
  "Check that all five SSH forwards follow one workspace control socket."
  (let ((paths
         (mapcar
          (lambda (entry)
            (let* ((process
                    (remote-forward-handle (cdr entry)))
                   (command (and (processp process)
                                 (process-command process))))
              (when (member "-S" command)
                (error "Jupyter forward %s disables SSH multiplexing"
                       (car entry)))
              (or (seq-find
                   (lambda (arg)
                     (string-prefix-p "ControlPath=" arg))
                   command)
                  (error "Jupyter forward %s lacks a managed ControlPath"
                         (car entry)))))
          (remote-channel-group-channels
           (my/noema-jupyter-runtime-channel-group runtime)))))
    (unless (and (= (length paths) 5)
                 (= (length (delete-dups paths)) 1))
      (error "Jupyter forwards use different SSH control sockets: %S"
             paths))
    (princ "PASS five forwards share the workspace SSH ControlPath\n")))

(defun my/jupyter-linux-live--signal-audit (original runtime signal)
  "Record broker signals sent during the opt-in fault probe."
  (princ (format "BROKER_SIGNAL %s PID=%s\n"
                 signal (my/noema-jupyter-runtime-pid runtime)))
  (funcall original runtime signal))

(defun my/jupyter-linux-live--exited-p (runtime)
  "Return non-nil only when the target confirms RUNTIME exited."
  (let ((deadline (+ (float-time) 6))
        (status (my/noema-jupyter--alive-p runtime)))
    (while (and (eq status t) (< (float-time) deadline))
      (sleep-for 0.1)
      (setq status (my/noema-jupyter--alive-p runtime)))
    (null status)))

(defun my/jupyter-linux-live-run ()
  "Launch, execute, recover channels, and close a real Linux ipykernel."
  (let* ((override (getenv "JUPYTER_LINUX_SSH"))
         (remote-client-ssh-program
          (or override remote-client-ssh-program)))
    (unless (remote-client-executable-find "ssh")
      (error "Client SSH is unavailable: %S" remote-client-ssh-program))
    (when override
      (my/tramp-configure-ssh-program))
    (remote-config-load (getenv "JUPYTER_LINUX_CONFIG"))
    (remote-fs-install)
    (let* ((target (or (getenv "JUPYTER_LINUX_TARGET")
                     (error "Set JUPYTER_LINUX_TARGET")))
         (_configured (or (remote-get-target target)
                          (error "Unknown Remote target: %s" target)))
         (native-root
          (format "/tmp/noema-jupyter-linux-%s-%s"
                  (emacs-pid)
                  (substring (secure-hash 'sha256
                                          (format "%s-%s" (float-time) (random)))
                             0 10)))
         (root (file-name-as-directory
                (remote-make-file-name target native-root)))
         (file (expand-file-name "probe.ipynb" root))
         (local-state (make-temp-file "noema-jupyter-linux-client-" t))
         (my/noema-jupyter-pending-directory
          (expand-file-name "pending/" local-state))
         (my/noema-jupyter-runtimes (make-hash-table :test #'equal))
         runtime workspace pre-cut-group)
    (unwind-protect
        (progn
          (make-directory root t)
          (with-temp-file file (insert "{}\n"))
          (cl-letf (((symbol-function 'my/noema-jupyter--runtime-directory)
                     (lambda (_workspace) root)))
            (setq runtime
                  (my/noema-jupyter--launch-runtime
                   `((kernelId . "linux-live-probe")
                     (sourceFile . ,file) (kernelName . "python3")))
                  workspace (my/noema-jupyter-runtime-workspace runtime)))
          (unless (file-exists-p
                   (my/noema-jupyter--pending-file
                    (my/noema-jupyter-runtime-id runtime)))
            (error "Launch did not persist broker identity"))
          (princ (format "LINUX_BROKER_TARGET=%s PID=%s ALIVE=%S\n"
                         (my/noema-jupyter-runtime-host-name runtime)
                         (my/noema-jupyter-runtime-pid runtime)
                         (my/noema-jupyter--alive-p runtime)))
          (my/jupyter-linux-live--check-forward-route runtime)
          (my/jupyter-linux-live--client
           runtime
           "noema_audit_state = 41; print(noema_audit_state + 1)"
           "Linux broker launch, execution and completion")
          (when (getenv "JUPYTER_LINUX_FAULT_INJECTION")
            (setq pre-cut-group
                  (my/noema-jupyter-runtime-channel-group runtime))
            (my/jupyter-linux-live--client
             runtime
             "import time; time.sleep(4); noema_outage_state = 84; print(noema_outage_state)"
             "Linux in-flight transport outcome is bounded"
             "raw-inflight-disconnect"
             (lambda () (my/jupyter-linux-live--cut-forwards runtime)))
            (unless (file-exists-p
                     (my/noema-jupyter--pending-file
                      (my/noema-jupyter-runtime-id runtime)))
              (error "Transport loss discarded the broker identity")))
          (if (getenv "JUPYTER_LINUX_FAULT_INJECTION")
              (let ((deadline (+ (float-time) 45))
                    (expected-host (my/noema-jupyter-runtime-host-name runtime)))
                (while (and (< (float-time) deadline)
                            (or (eq pre-cut-group
                                    (my/noema-jupyter-runtime-channel-group runtime))
                                (not (remote-channel-group-live-p
                                      (my/noema-jupyter-runtime-channel-group runtime)))))
                  (accept-process-output nil 0.1))
                (let ((current-host
                       (condition-case nil
                           (my/noema-jupyter--current-host
                            (my/noema-jupyter-runtime-context runtime))
                         (error "<unreachable>"))))
                  (princ (format "AFTER_CUT host=%s expected=%s workspace=%S runtime=%S alive=%S generation=%S\n"
                               current-host expected-host
                               (remote-workspace-state workspace)
                               (my/noema-jupyter-runtime-state runtime)
                               (my/noema-jupyter--alive-p runtime)
                               (remote-channel-group-generation
                                (my/noema-jupyter-runtime-channel-group runtime))))
                  (unless (equal current-host expected-host)
                    (error "Remote target moved from %s to %s; kernel record retained"
                           expected-host current-host)))
                (dolist (resource (remote-workspace-resources workspace))
                  (when (eq (remote-workspace-resource-state resource) 'failed)
                    (princ
                     (format "RECOVERY_FAILED kind=%S error=%s\n"
                             (remote-workspace-resource-kind resource)
                             (if-let* ((failure
                                        (remote-workspace-resource-error
                                         resource)))
                                 (error-message-string failure)
                               "<none>")))))
                (unless (and (not (eq pre-cut-group
                                      (my/noema-jupyter-runtime-channel-group runtime)))
                             (remote-channel-group-live-p
                              (my/noema-jupyter-runtime-channel-group runtime)))
                  (error "Workspace did not automatically recover all five forwards")))
            (let* ((old (my/noema-jupyter-runtime-channel-group runtime))
                   (ports (remote-channel-group-endpoints old 'local))
                   (replacement (remote-channel-group-recover old)))
              (my/noema-jupyter--update-group runtime old replacement)
              (unless (equal ports (remote-channel-group-endpoints replacement 'local))
                (error "Recovery changed client endpoints"))))
          (pcase (my/noema-jupyter--alive-p runtime)
            ('t
             (my/jupyter-linux-live--client
              runtime
              "print(noema_audit_state + 1)"
              "Linux five-channel recovery retains state")
             (when (getenv "JUPYTER_LINUX_FAULT_INJECTION")
               (my/jupyter-linux-live--client
                runtime
                "print(noema_outage_state - 42)"
                "Linux in-flight code completed on the original kernel")))
            ('nil
             (unless (getenv "JUPYTER_LINUX_FAULT_INJECTION")
               (error "Kernel died during an ordinary channel recovery"))
             (unless (and (eq (my/noema-jupyter-runtime-state runtime) 'dead)
                          (eq (my/noema-jupyter--get
                               'alive
                               (my/noema-jupyter--runtime-status-payload runtime))
                              :json-false)
                          (file-exists-p
                           (my/noema-jupyter--pending-file
                            (my/noema-jupyter-runtime-id runtime))))
               (error "Confirmed kernel death lost state or durable identity"))
             (princ "PASS confirmed kernel death is reported without losing its record\n")
             (let ((old-id (my/noema-jupyter-runtime-id runtime)))
               (my/noema-jupyter--shutdown-runtime runtime)
               (cl-letf (((symbol-function
                            'my/noema-jupyter--runtime-directory)
                          (lambda (_workspace) root)))
                 (setq runtime
                       (my/noema-jupyter--launch-runtime
                        `((kernelId . "linux-live-probe")
                          (sourceFile . ,file) (kernelName . "python3")))))
               (when (equal old-id (my/noema-jupyter-runtime-id runtime))
                 (error "Relaunch reused the dead runtime identity"))
               (my/jupyter-linux-live--client
                runtime
                "print('noema_audit_state' not in globals()); noema_audit_state = 41; print(noema_audit_state + 1)"
                "Linux relaunch starts with fresh state"
                nil nil "True")))
            (_ (error "Kernel liveness stayed unknown after workspace recovery")))
          (my/noema-jupyter--shutdown-runtime runtime)
          (unless (my/jupyter-linux-live--exited-p runtime)
            (error "Broker target process group did not exit"))
          (unless (and (eq (my/noema-jupyter-runtime-state runtime) 'closed)
                       (not (file-exists-p
                             (my/noema-jupyter--pending-file
                              (my/noema-jupyter-runtime-id runtime))))
                       (not (file-exists-p
                             (my/noema-jupyter-runtime-connection-file runtime))))
            (error "Broker shutdown left state or connection files"))
          (princ "PASS real Linux Remote / ipykernel broker lifecycle\n"))
      (when runtime (ignore-errors (my/noema-jupyter--shutdown-runtime runtime)))
      (when workspace (ignore-errors (remote-workspace-close workspace 'test-cleanup)))
      (if (and runtime (not (my/jupyter-linux-live--exited-p runtime)))
          (progn
            ;; Keep the connection identity and local journal if target-side
            ;; cleanup cannot be confirmed.  Removing either would turn a
            ;; test failure into an unidentifiable detached process.
            (my/noema-jupyter--record-pending-shutdown runtime)
            (display-warning
             'noema-jupyter
             (format "Unconfirmed live probe retained: target=%s root=%s journal=%s"
                     target root my/noema-jupyter-pending-directory)
             :error))
        (when (file-directory-p root) (delete-directory root t))
        (delete-directory local-state t))))))

(if (getenv "JUPYTER_LINUX_FAULT_INJECTION")
    (unwind-protect
        (progn
          (advice-add 'my/noema-jupyter--signal :around
                      #'my/jupyter-linux-live--signal-audit)
          (my/jupyter-linux-live-run))
      (advice-remove 'my/noema-jupyter--signal
                     #'my/jupyter-linux-live--signal-audit))
  (my/jupyter-linux-live-run))
;;; jupyter-linux-kernel-live-smoke.el ends here
