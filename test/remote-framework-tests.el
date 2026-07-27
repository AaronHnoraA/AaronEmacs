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
         (remote-workspace--resource-counter 0)
         (remote-channel--counter 0)
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
             remote-register-file-operation
             remote-session-acquire
             remote-session-invalidate
             remote-make-network-process
             remote-open-network-stream
             remote-port-forward
             remote-channel-of
             remote-environment-resolve
             remote-workspace-open
             remote-workspace-reconnect
             remote-workspace-register-recoverable-resource
             remote-register-service
             remote-service-ensure
             remote-terminal-open
             remote-terminal-adopt
             remote-terminal-command
             remote-terminal-put-metadata
             remote-terminal-restart
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
      'path))))

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
            (should (eq (remote-channel-handle channel) server)))
        (remote-close-channel server)))))

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
                 "1")))
      (remote-workspace-close workspace)
      (should-not (remote-service-list)))))

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
            "-J" "ops@edge:2222"
            "-L" "127.0.0.1:49152:127.0.0.1:3000"
            "dev@lab")))))))

(provide 'remote-framework-tests)
;;; remote-framework-tests.el ends here
