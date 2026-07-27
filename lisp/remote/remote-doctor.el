;;; remote-doctor.el --- Remote framework diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Doctor is intentionally target- and pipeline-oriented.  It reports the
;; logical boundary, configured transports, execution backends, live sessions,
;; workspaces, and recoverable resources without exposing physical file names.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)
(require 'remote-pipeline)
(require 'remote-transport)
(require 'remote-backend)
(require 'remote-session)
(require 'remote-fs)
(require 'remote-workspace)

(declare-function remote-process-file "remote-process"
                  (program &optional infile destination display &rest args))

(defun remote-doctor--target (target)
  "Resolve TARGET to a registered target object."
  (cond
   ((remote-target-p target) target)
   ((null target)
    (remote-get-target
     (remote-context-target-id (remote-context))))
   (t
    (or (remote-get-target target)
        (error "Unknown remote target: %S" target)))))

(defun remote-doctor--check (name status &optional detail remedy)
  "Build one diagnostic check."
  (list :name name :status status :detail detail :remedy remedy))

(defun remote-doctor--pipeline-checks (target)
  "Return pipeline and backend checks for TARGET."
  (let ((pipelines
         (remote-pipelines-for-target (remote-target-id target)))
        checks)
    (push
     (remote-doctor--check
      'pipelines
      (if pipelines 'ok 'error)
      (if pipelines
          (format "%d configured" (length pipelines))
        "No reachability pipeline")
      (unless pipelines "Add a pipeline to etc/remote.json"))
     checks)
    (dolist (pipeline pipelines)
      (let* ((pipeline-id (remote-pipeline-id pipeline))
             (missing-backends
              (seq-remove
               #'remote-get-backend
               (remote-pipeline-backend-ids pipeline)))
             (missing-transports
              (seq-remove
               #'remote-get-transport
               (mapcar
                #'remote-pipeline-stage-transport
                (remote-pipeline-stages pipeline)))))
        (push
         (remote-doctor--check
          (intern (format "pipeline:%s" pipeline-id))
          (if (remote-pipeline-enabled pipeline) 'ok 'warning)
          (format "backends=%S stages=%S priority=%s"
                  (remote-pipeline-backend-ids pipeline)
                  (mapcar
                   #'remote-pipeline-stage-transport
                   (remote-pipeline-stages pipeline))
                  (remote-pipeline-priority pipeline))
          (unless (remote-pipeline-enabled pipeline)
            "Enable the pipeline or remove it"))
         checks)
        (when missing-backends
          (push
           (remote-doctor--check
            (intern (format "backends:%s" pipeline-id))
            'error
            (format "Missing %S" missing-backends)
            "Load or register the required execution backend")
           checks))
        (when missing-transports
          (push
           (remote-doctor--check
            (intern (format "transports:%s" pipeline-id))
            'error
            (format "Missing %S" missing-transports)
            "Load or register the required transport stage")
           checks))))
    (nreverse checks)))

(defun remote-doctor--route-check (target adapter capability)
  "Check CAPABILITY routing for ADAPTER on TARGET."
  (let* ((path
          (remote-make-file-name (remote-target-id target) "/"))
         (context (remote-context path))
         (routes
          (condition-case error
              (remote-routes adapter capability context)
            (error
             (list :error error)))))
    (if (eq (car-safe routes) :error)
        (remote-doctor--check
         (intern (format "route:%s" capability))
         'error
         (error-message-string (plist-get routes :error))
         "Check pipeline capabilities and backend availability")
      (remote-doctor--check
       (intern (format "route:%s" capability))
       (if routes 'ok 'error)
       (if-let* ((route (car routes)))
           (format "%s via %s"
                   (remote-route-backend-id route)
                   (remote-route-pipeline-id route))
         "No route")
       (unless routes
         "Register a backend that advertises this capability")))))

(defun remote-doctor--runtime-checks (target)
  "Return live session and workspace checks for TARGET."
  (let ((target-id (remote-target-id target))
        checks)
    (dolist (session (remote-session-list))
      (when (equal (plist-get session :target) target-id)
        (push
         (remote-doctor--check
          'session
          (if (eq (plist-get session :state) 'open) 'ok 'warning)
          (format "%s/%s state=%s uses=%s"
                  (plist-get session :link)
                  (plist-get session :plugin)
                  (plist-get session :state)
                  (plist-get session :uses)))
         checks)))
    (dolist (summary (remote-workspace-list))
      (when (equal (plist-get summary :target) target-id)
        (push
         (remote-doctor--check
          (intern (format "workspace:%s" (plist-get summary :id)))
          (pcase (plist-get summary :state)
            ('open 'ok)
            ('degraded 'warning)
            (_ 'error))
          (format "state=%s resources=%s services=%S"
                  (plist-get summary :state)
                  (plist-get summary :resources)
                  (plist-get summary :services))
          (when (memq (plist-get summary :state)
                      '(disconnected failed degraded))
            "Run remote-workspace-reconnect and inspect resource errors"))
         checks)))
    (nreverse checks)))

(defun remote-doctor--probe (target)
  "Run a small routed process probe for TARGET."
  (let* ((context
          (remote-context
           (remote-make-file-name (remote-target-id target) "/")))
         (default-directory
          (remote-context-workspace-root context))
         (buffer (generate-new-buffer " *remote-doctor-probe*"))
         status output)
    (unwind-protect
        (condition-case error
            (progn
              (setq status
                    (let ((remote-current-adapter-id "process"))
                      (remote-process-file
                       "uname" nil buffer nil "-s")))
              (setq output
                    (with-current-buffer buffer
                      (string-trim (buffer-string))))
              (remote-doctor--check
               'probe
               (if (zerop status) 'ok 'error)
               (format "uname exit=%s output=%s" status output)
               (unless (zerop status)
                 "Inspect the route log and target shell environment")))
          (error
           (remote-doctor--check
            'probe 'error (error-message-string error)
            "Inspect transport, authentication, and remote executable PATH")))
      (remote--kill-internal-buffer buffer))))

(defun remote-doctor-report (&optional target probe)
  "Return a structured diagnostic report for TARGET.
When PROBE is non-nil, establish a session and execute `uname -s'."
  (let* ((target (remote-doctor--target target))
         (checks
          (append
           (list
            (remote-doctor--check
             'config-version
             (if (and (boundp 'remote-config-version)
                      (memq remote-config-version '(1 2)))
                 'ok
               'warning)
             (if (boundp 'remote-config-version)
                 remote-config-version
               "configuration not loaded"))
            (remote-doctor--check
             'logical-root 'ok
             (remote-make-file-name (remote-target-id target) "/")))
           (remote-doctor--pipeline-checks target)
           (mapcar
            (lambda (request)
              (apply #'remote-doctor--route-check target request))
            '(("emacs-file" file-read)
              ("process" process-sync)
              ("network" network-client)))
           (remote-doctor--runtime-checks target)
           (and probe (list (remote-doctor--probe target))))))
    (list
     :target (remote-target-id target)
     :label (remote-target-label target)
     :trusted (remote-target-trusted target)
     :status
     (cond
      ((seq-some
        (lambda (check) (eq (plist-get check :status) 'error))
        checks)
       'error)
      ((seq-some
        (lambda (check) (eq (plist-get check :status) 'warning))
        checks)
       'warning)
      (t 'ok))
     :checks checks)))

(defun remote-doctor--insert-report (report)
  "Insert human-readable REPORT in the current buffer."
  (insert
   (format "Remote Doctor — %s (%s)\n\n"
           (plist-get report :label)
           (plist-get report :target)))
  (insert
   (format "Overall: %s    Trust: %s\n\n"
           (upcase (symbol-name (plist-get report :status)))
           (if (plist-get report :trusted) "trusted" "untrusted")))
  (dolist (check (plist-get report :checks))
    (insert
     (format "%-8s %-34s %s\n"
             (upcase (symbol-name (plist-get check :status)))
             (plist-get check :name)
             (or (plist-get check :detail) "")))
    (when-let* ((remedy (plist-get check :remedy)))
      (insert (format "         → %s\n" remedy)))))

(defun remote-doctor (&optional target probe)
  "Display diagnostics for TARGET.
With a prefix argument PROBE, also connect and run a target-side probe."
  (interactive
   (list
    (when (fboundp 'remote-read-target)
      (remote-read-target "Doctor target: "))
    current-prefix-arg))
  (let* ((report (remote-doctor-report target probe))
         (buffer (get-buffer-create "*Remote Doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remote-doctor--insert-report report)
        (special-mode)))
    (pop-to-buffer buffer)
    report))

(provide 'remote-doctor)
;;; remote-doctor.el ends here
