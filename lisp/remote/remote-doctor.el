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
(require 'remote-compat)
(require 'remote-pipeline)
(require 'remote-transport)
(require 'remote-backend)
(require 'remote-session)
(require 'remote-fs)
(require 'remote-channel)
(require 'remote-workspace)
(require 'remote-accelerator)
(require 'remote-background)

(declare-function remote-process-file "remote-process"
                  (program &optional infile destination display &rest args))

(defvar remote-doctor-check-functions nil
  "Registered consumer diagnostic functions.
Each function receives TARGET and PROBE and returns a check or list of checks
using the same plist format as `remote-doctor--check'.")

(defun remote-doctor-register-check (function)
  "Register consumer diagnostic FUNCTION idempotently."
  (unless (functionp function)
    (error "Remote Doctor check is not callable: %S" function))
  (cl-pushnew function remote-doctor-check-functions :test #'equal)
  function)

(defun remote-doctor-unregister-check (function)
  "Unregister consumer diagnostic FUNCTION."
  (setq remote-doctor-check-functions
        (delete function remote-doctor-check-functions)))

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
          (format "%s/%s generation=%s state=%s uses=%s"
                  (plist-get session :link)
                  (plist-get session :plugin)
                  (plist-get session :generation)
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
    (dolist (watch (remote-file-watch-list))
      (when (equal (plist-get watch :target) target-id)
        (push
         (remote-doctor--check
          (intern (format "watch:%s" (plist-get watch :id)))
          (if (eq (plist-get watch :state) 'open) 'ok 'warning)
          (format "state=%s sequence=%s file=%s"
                  (plist-get watch :state)
                  (plist-get watch :sequence)
                  (plist-get watch :file))
          (unless (eq (plist-get watch :state) 'open)
            "Wait for resync or recreate the logical watch"))
         checks)))
    (dolist (job (remote-background-job-list target-id))
      (push
       (remote-doctor--check
        (intern (format "background:%s" (plist-get job :id)))
        (if (> (plist-get job :attempts) 0) 'warning 'ok)
        (format "state=%s epoch=%s attempts=%s key=%S"
                (plist-get job :state)
                (plist-get job :epoch)
                (plist-get job :attempts)
                (plist-get job :key))
        (when (> (plist-get job :attempts) 0)
          "Tramp was busy or the target generation changed; retry is bounded"))
       checks))
    (dolist (channel (remote-channel-list target-id))
      (push
       (remote-doctor--check
        (intern (format "channel:%s" (plist-get channel :id)))
        (if (eq (plist-get channel :state) 'open) 'ok 'warning)
        (format "%s via %s/%s local=%S remote=%S"
                (plist-get channel :kind)
                (plist-get channel :pipeline)
                (plist-get channel :backend)
                (plist-get channel :local-endpoint)
                (plist-get channel :remote-endpoint))
        (unless (eq (plist-get channel :state) 'open)
          "Close or recreate the failed channel"))
       checks))
    (dolist (group (remote-channel-group-list target-id))
      (push
       (remote-doctor--check
        (intern (format "channel-group:%s" (plist-get group :id)))
        (if (eq (plist-get group :state) 'open) 'ok 'warning)
        (format "generation=%s endpoints=%S"
                (plist-get group :generation)
                (plist-get group :endpoints))
        (unless (eq (plist-get group :state) 'open)
          "Recover or close the failed channel group"))
       checks))
    (nreverse checks)))

(defun remote-doctor--consumer-checks (target probe)
  "Return checks contributed by consumers for TARGET and PROBE."
  (let (checks)
    (dolist (function remote-doctor-check-functions)
      (condition-case error
          (let ((value (funcall function target probe)))
            (setq checks
                  (append
                   checks
                   (cond
                    ((null value) nil)
                    ((keywordp (car-safe value)) (list value))
                    (t value)))))
        (error
         (push
          (remote-doctor--check
           (intern (format "consumer:%s" function))
           'error (error-message-string error)
           "Inspect or unregister the failing consumer Doctor check")
          checks))))
    checks))

(defun remote-doctor--compatibility-checks (target)
  "Return upstream, backend-contract, and accelerator checks for TARGET."
  (let* ((compat (remote-compat-report))
         (target-id (remote-target-id target))
         (contracts
          (seq-filter
           (lambda (entry) (equal (caar entry) target-id))
           (remote-backend-contract-list)))
         (providers (remote-operation-provider-list))
         (operation-coverage (remote-file-operation-coverage-report))
         (backend-coverage (remote-backend-coverage-report))
         (adapter-coverage (remote-adapter-coverage-report))
         checks)
    (push
     (remote-doctor--check
      'upstream-contract
      (if (plist-get compat :foreign-handler-registration) 'ok 'warning)
      (format "Emacs=%s Tramp=%s external-operations=%s structured-errors=%s"
              (plist-get compat :emacs-version)
              (plist-get compat :tramp-version)
              (plist-get compat :external-operations)
              (plist-get compat :structured-errors))
      (unless (plist-get compat :external-operations)
        "Install a current GNU ELPA Tramp to enable high-level operations"))
     checks)
    (push
     (remote-doctor--check
      'upstream-operation-coverage
      (if (plist-get operation-coverage :missing) 'error 'ok)
      (format "%d upstream, %d registered, missing=%S"
              (length (plist-get operation-coverage :upstream))
              (length (plist-get operation-coverage :registered))
              (plist-get operation-coverage :missing))
      (when (plist-get operation-coverage :missing)
        "Add explicit routing contracts for the new Tramp operations"))
     checks)
    (dolist (coverage backend-coverage)
      (push
       (remote-doctor--check
        (intern (format "backend-surface:%s"
                        (plist-get coverage :backend)))
        (if (plist-get coverage :problems) 'error 'ok)
        (format "capabilities=%S problems=%S"
                (plist-get coverage :capabilities)
                (plist-get coverage :problems))
        (when (plist-get coverage :problems)
          "Implement the missing backend callback or remove the capability"))
       checks))
    (dolist (coverage adapter-coverage)
      (push
       (remote-doctor--check
        (intern (format "adapter-surface:%s"
                        (plist-get coverage :adapter)))
        (if (plist-get coverage :problems) 'error 'ok)
        (format "capabilities=%S problems=%S"
                (plist-get coverage :capabilities)
                (plist-get coverage :problems))
        (when (plist-get coverage :problems)
          "Declare the prerequisite capabilities for this adapter"))
       checks))
    (push
     (remote-doctor--check
      'operation-providers
      (if (seq-some (lambda (item) (plist-get item :available)) providers)
          'ok 'warning)
      (format "%S" providers)
      (unless (seq-some (lambda (item) (plist-get item :available)) providers)
        "Install tramp-hlo; ordinary Tramp remains the semantic fallback"))
     checks)
    (dolist (entry contracts)
      (let ((contract (cdr entry)))
        (push
         (remote-doctor--check
          (intern (format "backend-contract:%s"
                          (plist-get contract :backend)))
          (pcase (plist-get contract :status)
            ('ok 'ok)
            ('degraded 'warning)
            (_ 'error))
          (format "version=%s protocol=%s server=%s capabilities=%S"
                  (or (plist-get contract :implementation-version) "unknown")
                  (or (plist-get contract :protocol-version) "n/a")
                  (or (plist-get contract :server-version) "n/a")
                  (plist-get contract :capabilities))
          (plist-get contract :detail))
         checks)))
    (when (fboundp 'remote-backend-tramp-rpc-release-contract)
      (let ((release (remote-backend-tramp-rpc-release-contract)))
        (push
         (remote-doctor--check
          'tramp-rpc-release
          (if (plist-get release :release-checkout) 'ok 'warning)
          (format "client=%s tag=%s revision=%s dirty=%s"
                  (or (plist-get release :client-version) "unknown")
                  (or (plist-get release :tag) "not-exact")
                  (or (plist-get release :revision) "unknown")
                  (plist-get release :dirty))
          (unless (plist-get release :release-checkout)
            (concat "The checkout is not an exact release; keep deploy policy "
                    "at auto so a source-keyed server is used")))
         checks)))
    (when (and (locate-library "tramp-rpc")
               (fboundp 'remote-backend-tramp-rpc-compat-report))
      (require 'tramp-rpc nil t)
      (require 'tramp-rpc-deploy nil t)
      (let* ((report (remote-backend-tramp-rpc-compat-report))
             (interfaces (plist-get report :private-interfaces))
             (broken
              (seq-remove
               (lambda (entry) (plist-get entry :compatible))
               interfaces)))
        (push
         (remote-doctor--check
          'tramp-rpc-private-boundary
          (if broken 'warning 'ok)
          (format "%S" interfaces)
          (when broken
            (concat "Private tramp-rpc signatures changed; their optional "
                    "accelerations are disabled until the adapter is updated")))
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
             (remote-make-file-name (remote-target-id target) "/"))
            (remote-doctor--check
             'file-operation-contract
             (if (zerop
                  (hash-table-count remote-fs--unknown-operations))
                 'ok
               'warning)
             (format "%d registered, %d unknown observed, %d unknown effects"
                     (length (remote-file-operation-list))
                     (hash-table-count remote-fs--unknown-operations)
                     (seq-count
                      (lambda (spec)
                        (eq
                         (remote-file-operation-spec-filesystem-effects spec)
                         'unknown))
                      (remote-file-operation-list)))
             (unless
                 (zerop
                  (hash-table-count remote-fs--unknown-operations))
               "Register explicit contracts for operations in the route log")))
           (remote-doctor--pipeline-checks target)
           (remote-doctor--compatibility-checks target)
           (mapcar
            (lambda (request)
              (apply #'remote-doctor--route-check target request))
            '(("emacs-file" file-read)
              ("process" process-sync)
              ("network" network-client)))
           (remote-doctor--runtime-checks target)
           (and probe (list (remote-doctor--probe target)))
           (remote-doctor--consumer-checks target probe))))
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
