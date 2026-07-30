;;; remote-framework-tests.el --- Framework boundary tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -L lisp/remote \
;;     -l test/remote-framework-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'remote-framework)
(require 'remote-config)

(defmacro remote-framework-test-with-registry (&rest body)
  "Evaluate BODY with isolated framework registries."
  (declare (indent 0) (debug t))
  `(let ((remote-targets (make-hash-table :test #'equal))
         (remote-links (make-hash-table :test #'equal))
         (remote-link-plugins (make-hash-table :test #'equal))
         (remote-adapters (make-hash-table :test #'equal))
         (remote-route-health (make-hash-table :test #'equal))
         (remote-connection-pool (make-hash-table :test #'equal))
         (remote-pipeline-runtime-pool
          (make-hash-table :test #'equal))
         (remote-transports (make-hash-table :test #'equal))
         (remote-backends (make-hash-table :test #'equal))
         (remote-workspaces (make-hash-table :test #'equal))
         (remote-services (make-hash-table :test #'equal))
         (remote-service-instances (make-hash-table :test #'equal))
         (remote-terminals (make-hash-table :test #'equal))
         (remote-channels (make-hash-table :test #'equal))
         (remote-channel-groups (make-hash-table :test #'equal))
         (remote-workspace--resource-counter 0)
         (remote-channel--counter 0)
         (remote-channel-group--counter 0)
         (remote-doctor-check-functions nil)
         (remote-route-log nil))
     (remote-framework-reset)
     ,@body))

(ert-deftest remote-framework-loads-public-layers ()
  (dolist (feature
           '(remote-core remote-pipeline remote-backend remote-session
             remote-fs remote-process remote-channel remote-environment
             remote-path remote-service remote-workspace remote-terminal
             remote-doctor))
    (should (featurep feature)))
  (dolist (function
           '(remote-register-pipeline
             remote-pipeline-stages
             remote-register-backend
             remote-backend-prepare-execution
             remote-backend-prepare-process
             remote-backend-stdio-bridge-command
             remote-register-file-operation
             remote-session-acquire
             remote-session-invalidate
             remote-make-network-process
             remote-open-network-stream
             remote-port-forward
             remote-reverse-port-forward
             remote-channel-adopt
             remote-channel-of
             remote-channel-live-p
             remote-channel-endpoint
             remote-channel-list
             remote-channel-clear
             remote-channel-recover
             remote-channel-group-open
             remote-channel-group-endpoints
             remote-channel-group-live-p
             remote-channel-group-recover
             remote-channel-group-close
             remote-get-file-operation
             remote-file-operation-list
             remote-unregister-file-operation
             remote-environment-resolve
             remote-workspace-open
             remote-workspace-reconnect
             remote-workspace-track-route
             remote-workspace-find-resource
             remote-workspace-register-recoverable-resource
             remote-workspace-ensure-recoverable-resource
             remote-workspace-add-file-watch
             remote-register-service
             remote-service-ensure
             remote-service-restart
             remote-terminal-open
             remote-terminal-adopt
             remote-terminal-command
             remote-terminal-put-metadata
             remote-terminal-restart
             remote-doctor-register-check
             remote-doctor-unregister-check
             remote-doctor-report
             remote-workspace-context-id))
    (should (fboundp function))))

(ert-deftest remote-pipeline-preserves-ordered-transport-stages ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "via-edge" "tramp"
             :stages
             '((:id "overlay" :transport "tailscale")
               (:id "gateway" :transport "ssh"
                :config (:host "edge"))
               (:id "tunnel" :transport "frp"))
             :config '(:host "lab")))
           (stages (remote-pipeline-stages pipeline)))
      (should (remote-pipeline-p pipeline))
      (should (equal (remote-pipeline-id pipeline) "lab/via-edge"))
      (should
       (equal (mapcar #'remote-pipeline-stage-id stages)
              '("overlay" "gateway" "tunnel")))
      (should
       (equal (mapcar #'remote-pipeline-stage-transport stages)
              '("tailscale" "ssh" "frp"))))))

(ert-deftest remote-builtin-ssh-pipeline-owns-one-lazy-control-path ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "managed" "tramp"
             :stages
             '((:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'process-async
             :adapter-id "process"))
           first second control)
      (unwind-protect
          (progn
            (setq first (remote-pipeline-acquire route context)
                  second (remote-pipeline-acquire route context)
                  control
                  (remote-stage-runtime-handle
                   (car (remote-pipeline-runtime-stages first))))
            (should (eq first second))
            (should (remote-ssh-control-p control))
            (should
             (equal
              (remote-ssh-control-destination control)
              "dev@lab"))
            (should-not
             (file-exists-p
              (remote-ssh-control-path control)))
            (let ((remote-current-pipeline-runtime first))
              (should
               (equal
                (remote-transport-ssh-control-options)
                (list
                 "ControlMaster=auto"
                 (format
                  "ControlPersist=%d"
                  remote-transport-ssh-control-persist)
                 (format
                  "ControlPath=%s"
                  (remote-ssh-control-path control))))))
            (remote-pipeline-release first)
            (should
             (eq (remote-pipeline-runtime-state first) 'open))
            (remote-pipeline-release second)
            (should
             (eq (remote-pipeline-runtime-state first) 'closed))
            (should
             (eq (remote-ssh-control-state control) 'closed)))
        (when (and first
                   (not
                    (eq (remote-pipeline-runtime-state first) 'closed)))
          (remote-pipeline-release first t 'test-cleanup))))))

(ert-deftest remote-pipeline-compiles-multi-hop-tramp-path ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "nested" "tramp"
             :stages
             '((:id "overlay" :transport "tailscale"
                :config (:host "edge.tailnet"))
               (:id "gateway" :transport "ssh"
                :config (:user "dev"))
               (:id "host" :transport "ssh"
                :config (:host "lab.internal"))
               (:id "container" :transport "docker"
                :config (:host "workspace")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'file-read
             :adapter-id "emacs-file")))
      (should
       (equal
        (remote-project-file-name "/fs:lab:/work/a.el" route)
        (concat
         "/ssh:dev@edge.tailnet|ssh:lab.internal"
         "|docker:workspace:/work/a.el"))))))

(ert-deftest remote-pipeline-runtime-is-shared-and-closes-in-reverse ()
  (remote-framework-test-with-registry
    (let (opened closed)
      (dolist (id '("first" "second"))
        (let ((transport-id id))
          (remote-register-transport
           id
           :prepare #'remote-transport--address-prepare
           :connect
           (lambda (_stage endpoint _runtime)
             (push transport-id opened)
             (remote-transport-result-create
              :endpoint endpoint :handle transport-id))
           :disconnect
           (lambda (stage-runtime _runtime)
             (push
              (remote-stage-runtime-handle stage-runtime)
              closed)))))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "managed" "tramp"
               :stages '("first" "second")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :link-id (remote-pipeline-id pipeline)
               :link-plugin-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file"))
             (first (remote-pipeline-acquire route nil))
             (second (remote-pipeline-acquire route nil)))
        (should (eq first second))
        (should (equal (nreverse opened) '("first" "second")))
        (should (= (remote-pipeline-runtime-use-count first) 2))
        (remote-pipeline-release first)
        (should (eq (remote-pipeline-runtime-state first) 'open))
        (remote-pipeline-release second)
        (should (eq (remote-pipeline-runtime-state first) 'closed))
        (should (equal closed '("first" "second")))))))

(ert-deftest remote-pipeline-open-rolls-back-completed-stages ()
  (remote-framework-test-with-registry
    (let (closed)
      (remote-register-transport
       "managed"
       :prepare #'remote-transport--address-prepare
       :connect
       (lambda (_stage endpoint _runtime)
         (remote-transport-result-create
          :endpoint endpoint :handle 'managed))
       :disconnect
       (lambda (_stage-runtime _runtime)
         (push 'managed closed)))
      (remote-register-transport
       "broken"
       :prepare #'remote-transport--address-prepare
       :connect
       (lambda (_stage _endpoint _runtime)
         (error "stage failed")))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "rollback" "tramp"
               :stages '("managed" "broken")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :link-id (remote-pipeline-id pipeline)
               :link-plugin-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file")))
        (should-error (remote-pipeline-open route nil))
        (should (equal closed '(managed)))))))

(ert-deftest remote-pipeline-reentrant-acquire-keeps-one-runtime ()
  (remote-framework-test-with-registry
    (let (route context nested-error)
      (remote-register-transport
       "reentrant-stage"
       :connect
       (lambda (_stage endpoint _runtime)
         (condition-case error
             (remote-pipeline-acquire route context)
           (error (setq nested-error error)))
         (remote-transport-result-create
          :endpoint endpoint :handle 'stage)))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "reentrant" "tramp"
               :stages '("reentrant-stage")
               :config '(:host "lab")))
             (resolved
              (remote-route-create
               :target-id "lab"
               :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file")))
        (setq
         route resolved
         context
         (remote-context-create
          :target-id "lab" :localname "/work/"
          :workspace-root "/fs:lab:/work/"))
        (let ((runtime (remote-pipeline-acquire route context)))
          (should (eq (car nested-error) 'remote-pipeline-busy))
          (should (= (hash-table-count remote-pipeline-runtime-pool) 1))
          (should (= (remote-pipeline-runtime-use-count runtime) 1))
          (remote-pipeline-release runtime)
          (should
           (zerop (hash-table-count
                   remote-pipeline-runtime-pool))))))))

(ert-deftest remote-pipeline-cancelled-open-rolls-back-late-handle ()
  (remote-framework-test-with-registry
    (let (closed)
      (remote-register-transport
       "cancel-stage"
       :connect
       (lambda (_stage endpoint _runtime)
         (remote-pipeline-runtime-clear 'test-cancel)
         (remote-transport-result-create
          :endpoint endpoint :handle 'late-handle))
       :disconnect
       (lambda (stage _runtime)
         (push (remote-stage-runtime-handle stage) closed)))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "cancelled" "tramp"
               :stages '("cancel-stage")
               :config '(:host "lab")))
             (route
              (remote-route-create
               :target-id "lab"
               :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "tramp"
               :capability 'file-read
               :adapter-id "emacs-file"))
             (context
              (remote-context-create
               :target-id "lab" :localname "/work/"
               :workspace-root "/fs:lab:/work/")))
        (should-error
         (remote-pipeline-acquire route context)
         :type 'remote-pipeline-cancelled)
        (should (equal closed '(late-handle)))
        (should
         (zerop (hash-table-count
                 remote-pipeline-runtime-pool)))))))

(ert-deftest remote-config-accepts-new-pipeline-vocabulary ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-link-object
     "lab"
     '((id . "via-edge")
       (backends . ("tramp-rpc" "tramp"))
       (stages
        . (((id . "overlay") (transport . "tailscale"))
           ((id . "gateway") (transport . "ssh"))))
       (config . ((host . "lab")))))
    (let* ((pipeline (remote-get-pipeline "via-edge" "lab"))
           (stages (remote-pipeline-stages pipeline)))
      (should
       (equal (remote-pipeline-backend-ids pipeline)
              '("tramp-rpc" "tramp")))
      (should
       (equal (mapcar #'remote-pipeline-stage-transport stages)
              '("tailscale" "ssh"))))))

(ert-deftest remote-config-merges-compatible-pipeline-backends ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp")
       (config . ((host . "lab") (method . "ssh")))))
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp-rpc")
       (config . ((host . "lab")))))
    (should
     (equal
      (remote-pipeline-backend-ids
       (remote-get-pipeline "ssh" "lab"))
      '("tramp" "tramp-rpc")))))

(ert-deftest remote-config-rejects-conflicting-pipeline-definitions ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-config--register-pipeline-object
     "lab"
     '((id . "ssh") (backend . "tramp")
       (config . ((host . "lab-a")))))
    (should-error
     (remote-config--register-pipeline-object
      "lab"
      '((id . "ssh") (backend . "tramp-rpc")
        (config . ((host . "lab-b")))))
     :type 'error)))

(ert-deftest remote-config-load-is-transactional-on-registration-error ()
  (remote-framework-test-with-registry
    (remote-register-target "stable" :trusted t)
    (remote-register-pipeline
     "stable" "ssh" "tramp" :config '(:host "stable"))
    (let ((file (make-temp-file "remote-config-invalid-" nil ".json"))
          (generation remote-config-generation))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert
               "{\n"
               "  \"version\": 2,\n"
               "  \"targets\": [{\n"
               "    \"id\": \"broken\",\n"
               "    \"pipelines\": [\n"
               "      {\"id\":\"ssh\",\"backend\":\"tramp\","
               "\"config\":{\"host\":\"one\"}},\n"
               "      {\"id\":\"ssh\",\"backend\":\"tramp-rpc\","
               "\"config\":{\"host\":\"two\"}}\n"
               "    ]\n"
               "  }],\n"
               "  \"imports\": []\n"
               "}\n"))
            (should-error (remote-config-load file))
            (should (= remote-config-generation generation))
            (should (remote-get-target "stable"))
            (should (remote-get-pipeline "ssh" "stable"))
            (should-not (remote-get-target "broken")))
        (delete-file file)))))

(ert-deftest remote-config-validates-schema-version ()
  (should (= (remote-config--schema-version '((version . 1))) 1))
  (should (= (remote-config--schema-version '((version . 2))) 2))
  (should-error
   (remote-config--schema-version '((version . 3)))
   :type 'error))

(ert-deftest remote-route-v2-constraints-are-hard-boundaries ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline
     "lab" "primary" '("tramp-rpc" "tramp")
     :priority 100 :config '(:host "lab"))
    (remote-register-pipeline
     "lab" "secondary" "tramp"
     :priority 1 :config '(:host "lab-backup"))
    (let* ((context
            (remote-context-create
             :target-id "lab" :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (route
            (remote-resolve
             "emacs-file" 'file-read context
             '(:pipeline "secondary" :backend "tramp"))))
      (should (equal (remote-route-pipeline-id route) "lab/secondary"))
      (should (equal (remote-route-backend-id route) "tramp"))
      (should-error
       (remote-resolve
        "emacs-file" 'file-read context
        '(:pipeline "secondary" :backend "tramp-rpc")))
      (should
       (equal
        (remote-route-pipeline-id
         (remote-resolve
          "emacs-file" 'file-read context
          '(:exclude-pipelines ("primary"))))
        "lab/secondary")))))

(ert-deftest remote-file-operation-contract-is-explicit-and-extensible ()
  (let ((remote-file-operations (make-hash-table :test #'eq)))
    (remote-fs-register-standard-operations)
    (let ((write
           (gethash 'write-region remote-file-operations))
          (read
           (gethash 'file-attributes remote-file-operations)))
      (should (equal
               (remote-file-operation-spec-path-arguments write)
               '(2)))
      (should (remote-file-operation-spec-mutating write))
      (should-not (remote-file-operation-spec-retry-safe write))
      (should
       (remote-file-operation-spec-retry-safe read)))
    (remote-register-file-operation
     'framework-test-operation
     :capability 'file-read
     :path-arguments '(1)
     :result-kind 'path)
    (should
     (eq
      (remote-file-operation-spec-result-kind
       (gethash 'framework-test-operation remote-file-operations))
      'path))
    (should
     (eq (remote-get-file-operation 'framework-test-operation)
         (car
          (seq-filter
           (lambda (spec)
             (eq
              (remote-file-operation-spec-operation spec)
              'framework-test-operation))
           (remote-file-operation-list)))))
    (should
     (remote-unregister-file-operation 'framework-test-operation))
    (should-not
     (remote-get-file-operation 'framework-test-operation))
    ;; Emacs 32 can dispatch this primitive directly while recursively
    ;; creating parent directories.
    (should
     (remote-get-file-operation 'make-directory-internal))))

(ert-deftest remote-file-file-equal-internal-spelling-uses-public-primitive ()
  (let (called)
    (cl-letf (((symbol-function 'remote-fs--call-routed)
               (lambda (operation arguments)
                 (setq called (cons operation arguments))
                 t)))
      (should
       (remote-fs-file-name-handler
        'file-file-equal-p
        "/fs:local:/tmp/a" "/fs:local:/tmp/b")))
    (should
     (equal called
            '(file-equal-p
              "/fs:local:/tmp/a" "/fs:local:/tmp/b")))))

(ert-deftest remote-file-operation-unknown-fallback-is-conservative ()
  (let ((remote-file-operations (make-hash-table :test #'eq))
        (remote-fs--unknown-operations (make-hash-table :test #'eq))
        (remote-route-log nil))
    (let ((spec (remote-fs--operation-spec
                 'future-emacs-file-operation)))
      (should
       (eq (remote-file-operation-spec-capability spec)
           'file-write))
      (should (remote-file-operation-spec-mutating spec))
      (should-not
       (remote-file-operation-spec-retry-safe spec)))
    (should (= (hash-table-count remote-fs--unknown-operations) 1))
    (should (eq (plist-get (car remote-route-log) :kind)
                'file-operation-warning))))

(ert-deftest remote-file-operation-cross-target-mutation-policy ()
  (should-not
   (remote-fs--validate-cross-target-operation
    'copy-file
    '("/fs:local:/tmp/a" "/fs:lab:/tmp/a")))
  (dolist (operation
           '(rename-file add-name-to-file make-symbolic-link))
    (should-error
     (remote-fs--validate-cross-target-operation
      operation
      '("/fs:local:/tmp/a" "/fs:lab:/tmp/a"))
     :type 'error)))

(ert-deftest remote-workspace-has-stable-identity-and-owns-resources ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "project"
             :workspace-root "/fs:local:/tmp/project/"))
           (first
            (remote-workspace-open context :connect nil))
           (second
            (remote-workspace-open context :connect nil))
           closed)
      (should (eq first second))
      (should (equal (remote-workspace-id first)
                     "local@project"))
      (remote-workspace-register-resource
       first 'test 'handle
       (lambda (value reason)
         (setq closed (list value reason))))
      (remote-workspace-close first 'test-complete)
      (should (equal closed '(handle test-complete)))
      (should (eq (remote-workspace-state first) 'closed))
      (should-not (remote-get-workspace "local@project")))))

(ert-deftest remote-workspace-recoverable-resource-is-keyed-and-idempotent ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "keyed"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (first
            (remote-workspace-ensure-recoverable-resource
             workspace 'lsp '(eglot root) 'server-1
             :recover (lambda (&rest _arguments) 'server-2)))
           (second
            (remote-workspace-ensure-recoverable-resource
             workspace 'lsp '(eglot root) 'server-current
             :recover (lambda (&rest _arguments) 'server-recovered))))
      (should (eq first second))
      (should (= (length (remote-workspace-resources workspace)) 1))
      (should
       (eq (remote-workspace-resource-value first) 'server-current))
      (remote-workspace-recover-resource workspace first)
      (should
       (eq (remote-workspace-resource-value first) 'server-recovered))
      (should
       (eq
        (remote-workspace-find-resource workspace 'lsp '(eglot root))
        first))
      (remote-workspace-forget-resource workspace first)
      (should-not (remote-workspace-resources workspace)))))

(ert-deftest remote-workspace-file-watch-uses-one-target-neutral-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "watch"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (opened nil)
           (closed nil)
           (next 0))
      (cl-letf
          (((symbol-function 'file-notify-add-watch)
            (lambda (file flags callback)
              (push (list file flags callback) opened)
              (list 'descriptor (cl-incf next))))
           ((symbol-function 'file-notify-rm-watch)
            (lambda (descriptor)
              (push descriptor closed))))
        (let ((resource
               (remote-workspace-add-file-watch
                workspace "src/" '(change attribute-change) #'ignore
                :key 'sources)))
          (should
           (equal
            (caar opened)
            "/fs:local:/tmp/project/src/"))
          (remote-workspace-recover-resource workspace resource)
          (should (= (length opened) 2))
          (should (equal closed '((descriptor 1))))
          (should
           (equal
            (remote-workspace-resource-value resource)
            '(descriptor 2))))))))

(ert-deftest remote-workspace-reconnect-retries-transport-and-recovers-resources ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "recovery"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0)
           closed recovered)
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-primary-route workspace) route)
      (let ((resource
             (remote-workspace-register-recoverable-resource
              workspace 'watch 'old-watch
              :close
              (lambda (value reason)
                (setq closed (list value reason)))
              :recover
              (lambda (_resource _workspace)
                (setq recovered t)
                'new-watch))))
        (remote-workspace-register-resource
         workspace 'terminal 'shell)
        (let ((remote-workspace-reconnect-delays '(0 0 0)))
          (cl-letf
              (((symbol-function 'remote-session-invalidate)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'remote-session-acquire)
                (lambda (&rest _arguments)
                  (cl-incf attempts)
                  (when (< attempts 4)
                    (signal 'remote-transport-error
                            '("injected transport loss")))
                  'session)))
            (remote-workspace-reconnect workspace)))
        (should (= attempts 4))
        (should recovered)
        (should (equal closed '(old-watch transport-recovery)))
        (should (eq (remote-workspace-resource-value resource)
                    'new-watch))
        (should (eq (remote-workspace-resource-state resource) 'open))
        (should
         (eq
          (remote-workspace-resource-state
           (seq-find
            (lambda (candidate)
              (eq (remote-workspace-resource-kind candidate)
                  'terminal))
            (remote-workspace-resources workspace)))
          'disconnected))
        (should (eq (remote-workspace-state workspace) 'open))))))

(ert-deftest remote-workspace-does-not-retry-operation-errors ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "no-retry"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           (route (remote-resolve "process" 'process-sync context))
           (attempts 0))
      (setf (remote-workspace-routes workspace) (list route)
            (remote-workspace-primary-route workspace) route)
      (cl-letf
          (((symbol-function 'remote-session-invalidate)
            (lambda (&rest _arguments) nil))
           ((symbol-function 'remote-session-acquire)
            (lambda (&rest _arguments)
              (cl-incf attempts)
              (error "permission denied"))))
        (should-error
         (remote-workspace-reconnect workspace)
         :type 'error))
      (should (= attempts 1))
      (should (eq (remote-workspace-state workspace) 'failed)))))

(ert-deftest remote-native-network-api-keeps-process-compatibility ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (server
            (remote-make-network-process
             :name "remote-channel-test"
             :server t :host "127.0.0.1" :service t
             :noquery t :remote-context context))
           (channel (remote-channel-of server)))
      (unwind-protect
          (progn
            (should (processp server))
            (should (remote-channel-p channel))
            (should (eq (remote-channel-kind channel) 'listener))
            (should (eq (remote-channel-handle channel) server))
            (should
             (equal
              (remote-channel-endpoint server 'remote)
              (list :host (process-contact server :host)
                    :port (process-contact server :service)))))
        (remote-close-channel server)))))

(ert-deftest remote-channel-adopts-third-party-listener-idempotently ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (server
            (make-network-process
             :name "remote-adopt-test"
             :server t :host "127.0.0.1" :service t
             :noquery t))
           (first
            (remote-channel-adopt
             server :kind 'listener :context context
             :metadata '(:application "test")))
           (second
            (remote-channel-adopt
             server :kind 'listener :context context)))
      (unwind-protect
          (progn
            (should (eq first second))
            (should (eq (remote-channel-of server) first))
            (should
             (equal
              (plist-get
               (plist-get (car (remote-channel-list "local"))
                          :metadata)
               :application)
              "test")))
        (remote-close-channel server)))))

(ert-deftest remote-channel-group-is-atomic-recoverable-and-named ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/project/a.el"
             :workspace-id "channel-group"
             :workspace-root "/fs:local:/tmp/project/"))
           (workspace (remote-workspace-open context :connect nil))
           destinations group replacement)
      (unwind-protect
          (progn
            (dotimes (index 2)
              (push
               (make-network-process
                :name (format "remote-group-destination-%d" index)
                :server t :host "127.0.0.1" :service t :noquery t)
               destinations))
            (setq group
                  (remote-channel-group-open
                   `((shell . (:host "127.0.0.1"
                              :port ,(process-contact
                                      (nth 0 destinations) :service)))
                     (iopub . (:host "127.0.0.1"
                              :port ,(process-contact
                                      (nth 1 destinations) :service))))
                   :context context :workspace workspace
                   :key 'jupyter-test))
            (should (remote-channel-group-live-p group))
            (should (equal (mapcar #'car
                                   (remote-channel-group-endpoints
                                    group 'local))
                           '(shell iopub)))
            (should
             (remote-workspace-find-resource
              workspace 'channel-group 'jupyter-test))
            (setq replacement (remote-channel-group-recover group))
            (should-not (remote-channel-group-live-p group))
            (should (remote-channel-group-live-p replacement))
            (should
             (= (remote-channel-group-generation replacement) 2))
            (remote-channel-group-close replacement)
            (should-not (remote-channel-group-live-p replacement))
            ;; Closing twice is deliberately harmless.
            (remote-channel-group-close replacement))
        (when (remote-channel-group-p group)
          (ignore-errors (remote-channel-group-close group)))
        (when (remote-channel-group-p replacement)
          (ignore-errors (remote-channel-group-close replacement)))
        (dolist (process destinations)
          (when (process-live-p process)
            (delete-process process)))))))

(ert-deftest remote-channel-group-rolls-back-partial-open ()
  (remote-framework-test-with-registry
    (let ((calls 0)
          closed)
      (cl-letf
          (((symbol-function 'remote-port-forward)
            (lambda (&rest _arguments)
              (cl-incf calls)
              (if (= calls 2)
                  (error "second endpoint failed")
                'first-forward)))
           ((symbol-function 'remote-close-channel)
            (lambda (value) (push value closed))))
        (should-error
         (remote-channel-group-open
          '((first . (:host "127.0.0.1" :port 1))
            (second . (:host "127.0.0.1" :port 2)))
          :context
          (remote-context-create
           :target-id "local" :localname "/tmp/"
           :workspace-root "/fs:local:/tmp/"))
         :type 'error)
        (should (equal closed '(first-forward)))
        (should (zerop (hash-table-count remote-channel-groups)))))))

(ert-deftest remote-native-reverse-forward-relays-and-cleans-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (destination
            (make-network-process
             :name "remote-forward-echo"
             :server t :host "127.0.0.1" :service t
             :noquery t :coding 'binary
             :log
             (lambda (_server client _message)
               (set-process-filter
                client
                (lambda (process string)
                  (process-send-string process string))))))
           forward client buffer)
      (unwind-protect
          (progn
            (setq forward
                  (remote-reverse-port-forward
                   (list
                    :host "127.0.0.1"
                    :port (process-contact destination :service))
                   :context context :register nil
                   :stable-endpoint t))
            (let* ((endpoint
                    (remote-channel-endpoint forward 'remote))
                   (channel (remote-channel-of forward)))
              (should (remote-channel-p channel))
              (should (remote-channel-live-p forward))
              (should (equal (plist-get endpoint :host)
                             "127.0.0.1"))
              (should (integerp (plist-get endpoint :port)))
              (should (= (length (remote-channel-list "local")) 1))
              (setq buffer (generate-new-buffer
                            " *remote-forward-client*"))
              (setq client
                    (make-network-process
                     :name "remote-forward-client"
                     :buffer buffer
                     :host (plist-get endpoint :host)
                     :service (plist-get endpoint :port)
                     :coding 'binary :noquery t))
              (process-send-string client "roundtrip")
              (let ((deadline (+ (float-time) 2)))
                (while
                    (and
                     (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (not (string-match-p
                             "roundtrip" (buffer-string))))
                     (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should
               (with-current-buffer buffer
                 (string-match-p "roundtrip" (buffer-string))))
              (remote-close-channel forward)
              (should (eq (remote-forward-state forward) 'closed))
              (should (eq (remote-channel-state channel) 'closed))
              (should-not (remote-channel-list "local"))
              (let ((replacement (remote-channel-recover channel)))
                (should (remote-channel-live-p replacement))
                (should
                 (equal
                  (remote-channel-endpoint replacement 'remote)
                  endpoint))
                (should (= (length (remote-channel-list "local")) 1))
                (remote-close-channel replacement)
                (should-not (remote-channel-list "local")))))
        (when (and (processp client) (process-live-p client))
          (delete-process client))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (and (remote-forward-p forward)
                   (not (eq (remote-forward-state forward) 'closed)))
          (remote-close-channel forward))
        (when (process-live-p destination)
          (delete-process destination))))))

(ert-deftest remote-routed-listener-process-contact-exposes-target-port ()
  (let* ((server
          (make-network-process
           :name "remote-listener-contact"
           :server t :host "127.0.0.1" :service t :noquery t))
         (physical-port (process-contact server :service))
         (logical-port (1+ physical-port))
         (was-installed
          remote-channel--process-contact-advice-installed))
    (unwind-protect
        (progn
          (process-put
           server 'remote-listen-endpoint
           (list :host "127.0.0.1" :port logical-port))
          (remote-channel-install-compatibility)
          (should (= (process-contact server :service) logical-port))
          (should
           (equal
            (process-contact server)
            (list "127.0.0.1" logical-port))))
      (delete-process server)
      (unless was-installed
        (remote-channel-uninstall-compatibility)))))

(ert-deftest remote-forward-unexpected-exit-reports-transport-failure ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-root "/fs:local:/tmp/"))
           (forward
            (remote-port-forward
             '(:host "127.0.0.1" :port 9)
             :context context :register nil))
           (channel (remote-channel-of forward))
           reported)
      (unwind-protect
          (cl-letf
              (((symbol-function 'remote-report-route-failure)
                (lambda (route error)
                  (setq reported (list route error))
                  'transport)))
            (delete-process (remote-forward-handle forward))
            (accept-process-output nil 0.01)
            (should (eq (remote-forward-state forward) 'failed))
            (should (eq (remote-channel-state channel) 'failed))
            (should (eq (car reported)
                        (remote-channel-route channel)))
            (should
             (eq (car (cadr reported))
                 'remote-transport-error))
            (should-not (remote-channel-list "local")))
        (remote-close-channel forward)))))

(ert-deftest remote-doctor-reports-local-routing-boundaries ()
  (remote-framework-test-with-registry
    (let ((report (remote-doctor-report "local")))
      (should (memq (plist-get report :status) '(ok warning)))
      (dolist (capability '(file-read process-sync network-client))
        (should
         (eq
          (plist-get
           (seq-find
            (lambda (check)
              (eq
               (plist-get check :name)
               (intern (format "route:%s" capability))))
            (plist-get report :checks))
           :status)
          'ok))))))

(ert-deftest remote-doctor-includes-and-isolates-consumer-checks ()
  (remote-framework-test-with-registry
    (let ((check
           (lambda (target probe)
             (list
              :name 'consumer-test :status 'ok
              :detail
              (format "%s/%s" (remote-target-id target) (if probe 1 0))))))
      (remote-doctor-register-check check)
      (remote-doctor-register-check check)
      (let ((checks (plist-get (remote-doctor-report "local") :checks)))
        (should
         (eq
          (plist-get
           (seq-find
            (lambda (entry)
              (eq (plist-get entry :name) 'consumer-test))
            checks)
           :status)
          'ok)))
      (remote-doctor-unregister-check check)
      (should-not remote-doctor-check-functions))))

(ert-deftest remote-service-provisioning-is-trust-gated ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted nil)
    (let* ((context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-id "main"
             :workspace-root "/fs:lab:/work/"))
           (workspace
            (remote-workspace-open context :connect nil))
           installed)
      (remote-register-service
       "agent"
       :capabilities '(files processes channels)
       :probe
       (lambda (_context)
         (and installed
              '(:available t :version "1")))
       :provision
       (lambda (_context _probe)
         (setq installed t)))
      (should-error
       (remote-workspace-ensure-service
        workspace "agent" :provision t)
       :type 'remote-service-untrusted)
      (setf
       (remote-target-trusted
        (remote-get-target "lab"))
       t)
      (let ((instance
             (remote-workspace-ensure-service
              workspace "agent" :provision t)))
        (should installed)
        (should (remote-service-instance-live-p instance))
        (should (equal
                 (remote-service-instance-version instance)
                 "1"))
        (should
         (eq instance
             (remote-workspace-ensure-service
              workspace "agent" :provision t)))
        (should (= (remote-service-instance-use-count instance) 1))
        (should (= (length (remote-workspace-resources workspace)) 1)))
      (remote-workspace-close workspace)
      (should-not (remote-service-list)))))

(ert-deftest remote-target-service-restarts-in-place-for-every-workspace ()
  "A target-scoped restart must not leave another workspace with a stale object."
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((left-context
            (remote-context-create
             :target-id "lab"
             :localname "/work/left/a.el"
             :workspace-id "left"
             :workspace-root "/fs:lab:/work/left/"))
           (right-context
            (remote-context-create
             :target-id "lab"
             :localname "/work/right/a.el"
             :workspace-id "right"
             :workspace-root "/fs:lab:/work/right/"))
           (left
            (remote-workspace-open left-context :connect nil))
           (right
            (remote-workspace-open right-context :connect nil))
           (starts 0)
           (stops 0))
      (remote-register-service
       "shared-agent"
       :scope 'target
       :capabilities '(processes channels)
       :start
       (lambda (_context _probe)
         (list :handle (format "handle-%d" (cl-incf starts))))
       :stop
       (lambda (_instance _reason)
         (cl-incf stops)))
      (let* ((instance
              (remote-workspace-ensure-service left "shared-agent"))
             (right-instance
              (remote-workspace-ensure-service right "shared-agent")))
        (should (eq instance right-instance))
        (should (= (remote-service-instance-use-count instance) 2))
        (should
         (eq instance
             (remote-workspace-ensure-service
              left "shared-agent" :force t)))
        (should (eq instance
                    (car (remote-workspace-services right))))
        (should (equal (remote-service-instance-handle instance)
                       "handle-2"))
        (should (= (remote-service-instance-use-count instance) 2))
        (should (= starts 2))
        (should (= stops 1))
        ;; Exercise the transport-recovery path, which first releases the
        ;; left workspace's reference and then reacquires with FORCE.
        (let ((resource
               (seq-find
                (lambda (candidate)
                  (eq
                   (remote-workspace-resource-kind candidate)
                   'service))
                (remote-workspace-resources left))))
          (should
           (eq instance
               (remote-workspace-recover-resource left resource)))
          (should
           (eq instance
               (remote-workspace-resource-value resource))))
        (should (eq instance
                    (car (remote-workspace-services right))))
        (should (equal (remote-service-instance-handle instance)
                       "handle-3"))
        (should (= (remote-service-instance-use-count instance) 2))
        (should (= starts 3))
        (should (= stops 2))
        (remote-workspace-close left)
        (should (remote-service-instance-live-p instance))
        (should (= (remote-service-instance-use-count instance) 1))
        (remote-workspace-close right)
        (should-not (remote-service-list))
        (should (= stops 3))))))

(ert-deftest remote-terminal-runs-through-routed-pty-boundary ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "tmp"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (terminal
            (remote-terminal-open
             workspace
             :name "test"
             :shell "/bin/sh"
             :arguments '("-c" "printf terminal-ready"))))
      (unwind-protect
          (progn
            (while
                (process-live-p
                 (remote-terminal-process terminal))
              (accept-process-output
               (remote-terminal-process terminal) 0.1))
            (should
             (string-match-p
              "terminal-ready"
              (with-current-buffer
                  (remote-terminal-buffer terminal)
                (buffer-string)))))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-probes-login-shell-and-keeps-fallback ()
  (remote-framework-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-id "shell"
             :workspace-root "/fs:lab:/work/"))
           (workspace
            (remote-workspace-create
             :id "lab@shell"
             :target-id "lab"
             :workspace-id "shell"
             :root "/fs:lab:/work/"
             :context context
             :state 'open)))
      (cl-letf
          (((symbol-function 'remote-path-probe)
            (lambda (&rest _arguments)
              (remote-path-facts-create
               :target-id "lab"
               :shell "/usr/bin/zsh"))))
        (should
         (equal
          (remote-terminal-command workspace "default" t)
          '("/usr/bin/zsh" "-l"))))
      (setf (remote-target-shell target) nil)
      (let (attempts)
        (cl-letf
            (((symbol-function 'remote-path-probe)
              (lambda (&rest _arguments)
                (error "injected shell probe failure")))
             ((symbol-function 'remote-executable-find)
              (lambda (program &rest _arguments)
                (push program attempts)
                (and (equal program "bash") "/bin/bash"))))
          (should
           (equal
            (remote-terminal-command workspace "default" t)
            '("/bin/bash" "-l")))
          (should (equal (nreverse attempts) '("zsh" "bash")))))
      (let (attempts)
        (cl-letf
            (((symbol-function 'remote-path-probe)
              (lambda (&rest _arguments)
                (error "injected shell probe failure")))
             ((symbol-function 'remote-executable-find)
              (lambda (program &rest _arguments)
                (push program attempts)
                nil)))
          (should
           (equal
            (remote-terminal-command workspace "default" t)
            '("/bin/sh" "-l")))
          (should
           (equal (nreverse attempts) '("zsh" "bash" "sh"))))))))

(ert-deftest remote-terminal-transport-loss-requires-explicit-restart ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "terminal-restart"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (terminal
            (remote-terminal-open
             workspace
             :name "restart"
             :shell "/bin/sh"
             :arguments '("-c" "sleep 30")))
           replacement)
      (unwind-protect
          (progn
            (remote-workspace--mark-terminals-disconnected
             workspace 'injected-loss)
            (should
             (eq (remote-terminal-state terminal) 'disconnected))
            (should (memq terminal (remote-terminal-list workspace)))
            (setq replacement (remote-terminal-restart terminal))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should (remote-terminal-p replacement))
            (should (eq (remote-terminal-state replacement) 'open))
            (should-not (eq terminal replacement)))
        (remote-workspace-close workspace)))))

(ert-deftest remote-routed-process-preserves-invocation-directory ()
  (remote-framework-test-with-registry
    (let* ((root (make-temp-file "remote-process-cwd-" t))
           (child (expand-file-name "child/" root))
           (logical-root
            (remote-canonicalize-file-name
             (file-name-as-directory root)))
           (logical-child
            (remote-canonicalize-file-name
             (file-name-as-directory child)))
           (context
            (remote-context-create
             :target-id "local"
             :localname (file-name-as-directory root)
             :workspace-id "cwd-test"
             :workspace-root logical-root))
           (buffer (generate-new-buffer " *remote-process-cwd*"))
           process)
      (unwind-protect
          (progn
            (make-directory child)
            (setq process
                  (remote-make-process
                   :name "remote-process-cwd"
                   :buffer buffer
                   :command '("/bin/pwd")
                   :remote-context context
                   :remote-directory logical-child
                   :noquery t))
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (should
             (equal
              (car
               (split-string
                (with-current-buffer buffer (buffer-string))
                "\n" t))
              (directory-file-name child))))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-directory root t)))))

(ert-deftest remote-terminal-adopts-native-frontend-lifecycle ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "frontend"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (buffer (generate-new-buffer " *remote-terminal-frontend*"))
           (process
            (make-process
             :name "remote-terminal-frontend"
             :buffer buffer
             :command '("/bin/sh" "-c" "sleep 30")
             :noquery t))
           terminal)
      (unwind-protect
          (progn
            (setq terminal
                  (remote-terminal-adopt
                   workspace buffer
                   :process process
                   :name "frontend"
                   :profile "default"
                   :metadata '(:frontend test)))
            (should (eq
                     (buffer-local-value
                      'remote-terminal-instance buffer)
                     terminal))
            (should (eq (process-get process 'remote-terminal)
                        terminal))
            (should (= (length (remote-terminal-list workspace)) 1))
            (should (= (length
                        (remote-workspace-resources workspace))
                       1))
            (kill-buffer buffer)
            (should-not (process-live-p process))
            (should (eq (remote-terminal-state terminal) 'closed))
            (should-not (remote-terminal-list workspace))
            (should-not (remote-workspace-resources workspace)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-terminal-explicitly-restarts-native-frontend ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/"
             :workspace-id "frontend-restart"
             :workspace-root "/fs:local:/tmp/"))
           (workspace
            (remote-workspace-open context :connect nil))
           (buffer
            (generate-new-buffer
             " *remote-terminal-frontend-restart*"))
           (process
            (make-process
             :name "remote-terminal-frontend-restart"
             :buffer buffer
             :command '("/bin/sh" "-c" "sleep 30")
             :noquery t))
           restarted-with
           (terminal
            (remote-terminal-adopt
             workspace buffer
             :process process
             :name "frontend-restart"
             :profile "default"
             :metadata
             (list
              :frontend 'test
              :restart-function
              (lambda (old owner)
                (setq restarted-with (list old owner))
                'replacement)))))
      (unwind-protect
          (progn
            (remote-terminal-mark-disconnected
             terminal 'injected-loss)
            (should (eq
                     (remote-terminal-state terminal)
                     'disconnected))
            (should-not (remote-terminal-process terminal))
            (should (buffer-live-p buffer))
            (should
             (eq (remote-terminal-restart terminal)
                 'replacement))
            (should (equal restarted-with
                           (list terminal workspace)))
            (should-not (buffer-live-p buffer))
            (should (eq
                     (remote-terminal-state terminal)
                     'closed)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (remote-workspace-close workspace)))))

(ert-deftest remote-backend-registration-keeps-link-plugin-compatibility ()
  (remote-framework-test-with-registry
    (remote-register-backend
     "memory"
     :capabilities '(file-read)
     :project
     (lambda (file-name _pipeline _route)
       (concat "/projected" (remote-fs-localname file-name))))
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline "lab" "memory" "memory")
    (remote-register-adapter
     "test" :capabilities '(file-read)
     :preferences '((default . ("memory"))))
    (let* ((context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-root "/fs:lab:/work/"))
           (route (remote-resolve "test" 'file-read context)))
      (should (remote-get-backend "memory"))
      (should (remote-get-link-plugin "memory"))
      (should
       (equal
        (remote-project-file-name "/fs:lab:/work/a.el" route)
        "/projected/work/a.el")))))

(ert-deftest remote-backend-prepares-logical-and-physical-execution ()
  (remote-framework-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname "/tmp/work/a.el"
             :workspace-root "/fs:local:/tmp/work/"))
           (route (remote-resolve "exec" 'process-sync context))
           (execution
            (remote-backend-prepare-execution
             route context '("git" "status") '(("LANG" . "C")))))
      (should
       (equal (remote-backend-execution-logical-directory execution)
              "/fs:local:/tmp/work/"))
      (should
       (equal (remote-backend-execution-physical-directory execution)
              "/tmp/work/"))
      (should
       (equal (remote-backend-execution-command execution)
              '("git" "status")))
      (should
       (eq (plist-get
            (remote-backend-execution-metadata execution)
            :program-form)
           'search)))))

(ert-deftest remote-stdio-bridge-dispatches-through-the-selected-backend ()
  "The public process layer must not special-case local, SSH, or backend IDs."
  (remote-framework-test-with-registry
    (let (seen)
      (remote-register-backend
       "test-bridge"
       :capabilities '(process-async)
       :project
       (lambda (_file-name _pipeline _route)
         temporary-file-directory)
       :stdio-bridge
       (lambda (execution)
         (setq seen execution)
         (list
          "/test/bridge"
          (remote-route-target-id
           (remote-backend-execution-route execution))
          (car (remote-backend-execution-command execution)))))
      (remote-register-target "lab" :trusted t)
      (remote-register-pipeline
       "lab" "bridge" "test-bridge"
       :config '(:transport "direct"))
      (let* ((context
              (remote-context-create
               :target-id "lab"
               :localname "/work/a.el"
               :workspace-root "/fs:lab:/work/"))
             (command
              (remote-local-bridge-command
               "language-server"
               :context context
               :link "lab/bridge")))
        (should
         (equal command
                '("/test/bridge" "lab" "language-server")))
        (should (remote-backend-execution-p seen))
        (should
         (equal
          (remote-backend-execution-logical-directory seen)
          "/fs:lab:/work/"))))))

(ert-deftest remote-rpc-backend-declares-absolute-spawn-contract ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "ssh" "tramp-rpc"
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp-rpc"
             :capability 'process-async
             :adapter-id "process"))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/a.el"
             :workspace-root "/fs:lab:/work/"))
           (execution
            (remote-backend-prepare-execution
             route context '("direnv" "export" "json") nil)))
      (should
       (eq (plist-get
            (remote-backend-execution-metadata execution)
            :program-form)
           'absolute))
      (should
       (plist-get
        (remote-backend-execution-metadata execution)
        :require-absolute-program)))))

(ert-deftest remote-tramp-stream-keeps-target-host-above-the-relay ()
  "TLS/SNI sees the target host while only the socket connect sees loopback."
  (remote-framework-test-with-registry
    (let* ((route
            (remote-route-create
             :target-id "lab"
             :pipeline-id "lab/ssh"
             :backend-id "tramp"
             :capability 'network-client
             :adapter-id "network"))
           (context
            (remote-context-create
             :target-id "lab"
             :localname "/work/"
             :workspace-root "/fs:lab:/work/"))
           (process
            (make-pipe-process
             :name "remote-stream-return-list" :noquery t))
           (forward
            (remote-forward-create
             :backend-id "tramp"
             :route route :context context
             :local-endpoint
             '(:host "127.0.0.1" :port 49152)
             :remote-endpoint
             '(:host "db.internal" :port 443)
             :state 'open))
           high-level
           low-level)
      (unwind-protect
          (cl-letf
              (((symbol-function 'gnutls-available-p)
                (lambda () t))
               ((symbol-function 'remote-backend-tramp-forward)
                (lambda (&rest _arguments) forward))
               ((symbol-function 'make-network-process)
                (lambda (&rest arguments)
                  (setq low-level arguments)
                  process))
               ((symbol-function 'open-network-stream)
                (lambda (name buffer host service &rest parameters)
                  (setq high-level
                        (list name buffer host service parameters))
                  (list
                   (make-network-process
                    :name name :buffer buffer
                    :host host :service service)
                   :greeting "hello"
                   :type 'tls))))
            (let ((result
                   (remote-backend-tramp-stream
                    route context "db" nil "db.internal" 443
                    '(:type tls :return-list t))))
              (should (eq (car result) process))
              (should (equal (nth 2 high-level) "db.internal"))
              (should (= (nth 3 high-level) 443))
              (should (eq (plist-get (nth 4 high-level) :type) 'tls))
              (should
               (equal (plist-get low-level :host) "127.0.0.1"))
              (should (= (plist-get low-level :service) 49152))
              (should (eq (process-get process 'remote-forward)
                          forward))))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest remote-open-network-stream-preserves-native-return-list ()
  (remote-framework-test-with-registry
    (let ((process
           (make-pipe-process
            :name "remote-stream-list-contract" :noquery t)))
      (unwind-protect
          (progn
            (remote-register-backend
             "stream-list"
             :capabilities '(network-client)
             :open-network-stream
             (lambda (_route _context _name _buffer _host _service parameters)
               (should (plist-get parameters :return-list))
               (list process :greeting "ready" :type 'plain)))
            (remote-register-target "lab" :trusted t)
            (remote-register-pipeline
             "lab" "stream" "stream-list"
             :config '(:transport "direct"))
            (let* ((context
                    (remote-context-create
                     :target-id "lab"
                     :localname "/work/"
                     :workspace-root "/fs:lab:/work/"))
                   (result
                    (remote-open-network-stream
                     "stream" nil "service.internal" 8080
                     :return-list t
                     :remote-context context
                     :remote-pipeline "lab/stream")))
              (should
               (equal (cdr result)
                      '(:greeting "ready" :type plain)))
              (should (eq (car result) process))
              (should (remote-channel-of process))
              (should
               (equal
                (remote-channel-endpoint process 'remote)
                '(:host "service.internal" :port 8080)))))
        (when (process-live-p process)
          (remote-close-channel process))))))

(ert-deftest remote-channel-never-falls-back-to-the-client-machine ()
  (remote-framework-test-with-registry
    (remote-register-backend
     "file-only"
     :capabilities '(file-read)
     :project (lambda (file _pipeline _route) file))
    (remote-register-target "lab" :trusted t)
    (remote-register-pipeline
     "lab" "isolated" "file-only")
    (let ((context
           (remote-context-create
            :target-id "lab"
            :localname "/work/"
            :workspace-root "/fs:lab:/work/")))
      (should-error
       (remote-resolve "network" 'network-client context))
      (should-error
       (remote-open-network-stream
        "unsafe-fallback" nil "127.0.0.1" 9
        :remote-context context)))))

(ert-deftest remote-ssh-forward-command-respects-pipeline-hops ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "jumped" "tramp"
             :stages
             '((:id "jump" :transport "ssh"
                :config (:host "edge" :user "ops" :port 2222))
               (:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :link-id (remote-pipeline-id pipeline)
             :link-plugin-id "tramp"
             :capability 'port-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote)
                   "/usr/bin/ssh")))
        (should
         (equal
          (remote-backend-tramp--ssh-forward-command
           route "127.0.0.1" 49152 "127.0.0.1" 3000)
          '("/usr/bin/ssh" "-N" "-T"
            "-o" "ExitOnForwardFailure=yes"
            "-o" "ServerAliveInterval=30"
            "-o" "ServerAliveCountMax=3"
            "-o" "ConnectTimeout=8"
            "-o" "ConnectionAttempts=1"
            "-J" "ops@edge:2222"
            "-L" "127.0.0.1:49152:127.0.0.1:3000"
            "dev@lab")))))))

(ert-deftest remote-ssh-reverse-forward-command-respects-pipeline-hops ()
  (remote-framework-test-with-registry
    (remote-register-target "lab" :trusted t)
    (let* ((pipeline
            (remote-register-pipeline
             "lab" "jumped" "tramp"
             :stages
             '((:id "jump" :transport "ssh"
                :config (:host "edge" :user "ops" :port 2222))
               (:id "target" :transport "ssh"
                :config (:host "lab" :user "dev")))
             :config '(:host "lab")))
           (route
            (remote-route-create
             :target-id "lab"
             :pipeline-id (remote-pipeline-id pipeline)
             :backend-id "tramp"
             :capability 'reverse-forward
             :adapter-id "network")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &optional _remote)
                   "/usr/bin/ssh")))
        (should
         (equal
          (remote-backend-tramp--ssh-forward-command
           route "127.0.0.1" 3000 "127.0.0.1" 49152
           'reverse)
          '("/usr/bin/ssh" "-N" "-T"
            "-o" "ExitOnForwardFailure=yes"
            "-o" "ServerAliveInterval=30"
            "-o" "ServerAliveCountMax=3"
            "-v"
            "-o" "ConnectTimeout=8"
            "-o" "ConnectionAttempts=1"
            "-J" "ops@edge:2222"
            "-R" "127.0.0.1:49152:127.0.0.1:3000"
            "dev@lab")))))))

(provide 'remote-framework-tests)
;;; remote-framework-tests.el ends here
