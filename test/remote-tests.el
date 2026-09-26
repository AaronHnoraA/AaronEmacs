;;; remote-tests.el --- Logical target and routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -L lisp/remote -L lisp/remote/backend \
;;     -l test/remote-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-connection)
(require 'remote-environment)
(require 'remote-path)
(require 'remote-backend-tramp-rpc)
(require 'remote-framework)
(require 'direnv)

(defvar tramp-rpc-ssh-args)
(defvar tramp-rpc-ssh-options)
(defvar tramp-rpc-use-controlmaster)

;; Emacs 32 may native-compile a trampoline whenever `cl-letf' temporarily
;; replaces a C subr.  These isolated unit tests deliberately replace file
;; primitives and must not depend on an external assembler being installed.
(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))

(remote-fs-install)

(defmacro remote-test-with-registry (&rest body)
  "Evaluate BODY with an isolated remote registry."
  (declare (indent 0) (debug t))
  `(let ((remote-targets (make-hash-table :test #'equal))
         (remote-links (make-hash-table :test #'equal))
         (remote-link-plugins (make-hash-table :test #'equal))
         (remote-adapters (make-hash-table :test #'equal))
         (remote-route-health (make-hash-table :test #'equal))
         (remote-route-log nil)
         (remote-connection-pool (make-hash-table :test #'equal))
         (remote-pipeline-runtime-pool (make-hash-table :test #'equal))
         (remote-environment-cache (make-hash-table :test #'equal))
         (remote-environments-by-id (make-hash-table :test #'equal))
         (remote-environment-providers nil)
         (remote-fs-preferred-route-cache (make-hash-table :test #'equal))
         (remote-backend-tramp--prefix-cache
          (make-hash-table :test #'equal))
         (remote-backend-contracts (make-hash-table :test #'equal)))
     (remote-reset-registries)
     (remote-fs-register-link-plugins)
     (remote-register-adapter
      "process"
      :capabilities '(process-sync process-async environment)
      :preferences '((default . ("native" "tramp-rpc" "tramp"))))
     (remote-register-adapter
      "exec"
      :capabilities '(process-sync process-async environment)
      :preferences '((default . ("tramp-rpc" "tramp" "native"))))
     (remote-register-adapter
      "environment"
      :capabilities '(process-sync environment)
      :preferences '((default . ("tramp-rpc" "tramp" "native"))))
     ,@body))

(ert-deftest remote-public-api-is-present ()
  (dolist (function
           '(remote-canonicalize-file-name
             remote-expand-file-name
             remote-file-name-to-uri
             remote-uri-to-file-name
             remote-file-name-target
             remote-file-local-name
             remote-client-file-name
             remote-project-file-name
             remote-file-equal-p
             remote-register-link-plugin
             remote-register-adapter
             remote-register-pipeline
             remote-get-pipeline
             remote-pipeline-resolve
             remote-register-backend
             remote-get-backend
             remote-backend-prepare-execution
             remote-connection-ensure
             remote-connection-warm
             remote-connection-pool-status
             remote-session-acquire
             remote-session-warm
             remote-session-list
             remote-context
             remote-resolve
             remote-make-process
             remote-make-client-process
             remote-client-process-environment
             remote-client-exec-path
             remote-client-executable-find
             remote-process-description
             remote-process-file
             remote-exec
             remote-exec-async
             remote-executable-find
             remote-copy-file-to-target
             remote-service-provision-directory
             remote-local-bridge-command
             remote-environment-ensure
             remote-environment-derive
             remote-register-environment-maintainer
             remote-get-environment
             remote-path-decorate
             remote-register-path-profile
             remote-path-candidates
             remote-path-probe
             remote-make-network-process
             remote-open-network-stream
             remote-port-forward
             remote-file-watch-list
             remote-get-file-watch
             remote-file-watch-recover))
    (should (fboundp function)))
  (should (macrop 'remote-with-route)))

(ert-deftest remote-local-target-exposes-native-unhandled-directory ()
  (let* ((native (file-name-as-directory temporary-file-directory))
         (logical (remote-make-file-name "local" native)))
    (should
     (equal (unhandled-file-name-directory logical) native))
    (with-temp-buffer
      (let ((default-directory logical))
        (should (zerop (call-process "pwd" nil t nil)))
        (should
         (equal
          (file-name-as-directory (string-trim (buffer-string)))
          native))))))

(ert-deftest remote-client-file-name-is-a-backend-placement-query ()
  (remote-test-with-registry
    (remote-register-adapter
     "emacs-file" :capabilities '(metadata)
     :preferences '((default . ("native" "isolated-files"))))
    (let* ((native (file-name-as-directory temporary-file-directory))
           (logical (remote-make-file-name "local" native)))
      (should (equal (remote-client-file-name logical) native)))
    (remote-register-backend
     "isolated-files"
     :capabilities '(metadata)
     :project (lambda (_file _pipeline _route) "/isolated/"))
    (remote-register-target "isolated" :trusted t)
    (remote-register-pipeline
     "isolated" "only" "isolated-files"
     :config '(:transport "direct"))
    (should-not
     (remote-client-file-name "/fs:isolated:/work/a.el"))))

(ert-deftest remote-client-executable-find-stays-on-this-machine ()
  "An adapter may answer `executable-find' for its target.
A helper this machine has to execute itself must still be resolved here."
  (let ((client-program
         (expand-file-name "remote-client-probe"
                           temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file client-program (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes client-program #o755)
          (cl-letf* ((original (symbol-function 'executable-find))
                     ((symbol-function 'executable-find)
                      (lambda (command &optional remote)
                        (if (equal remote-current-adapter-id "target-lookup")
                            "/target-only/bin/remote-client-probe"
                          (funcall original command remote)))))
            (let ((exec-path (cons temporary-file-directory exec-path))
                  (remote-current-adapter-id "target-lookup"))
              (should
               (equal (executable-find "remote-client-probe")
                      "/target-only/bin/remote-client-probe"))
              (should
               (equal (remote-client-executable-find "remote-client-probe")
                      client-program)))))
      (when (file-exists-p client-program)
        (delete-file client-program)))))

(ert-deftest remote-client-environment-survives-a-target-projection ()
  "Projecting a target environment rebinds the default `exec-path'.
The client boundary must keep answering with this machine's values."
  (let ((remote--client-exec-path nil)
        (remote--client-process-environment nil)
        (client-path (copy-sequence exec-path))
        (client-environment (copy-sequence process-environment)))
    (remote-with-client-environment
      (let ((exec-path '("/target-only/bin"))
            (process-environment '("PATH=/target-only/bin")))
        (should (equal (remote-client-exec-path) client-path))
        (should
         (equal (remote-client-process-environment) client-environment))))))

(ert-deftest remote-client-environment-ignores-a-consumer-projection ()
  "A consumer may bind a target projection before entering the framework.
`python-shell-with-environment' does this around `run-python'; tramp-rpc
then built the client's SSH ControlPath under the target's HOME.  Neither
the unpinned fallback nor a pin taken inside that binding may see it."
  (with-temp-buffer
    (let ((remote--client-exec-path nil)
          (remote--client-process-environment nil)
          (remote--buffer-base-process-environment nil)
          (remote--buffer-base-exec-path nil)
          (client-home (getenv "HOME"))
          (client-path (remote-client-exec-path)))
      (let ((process-environment
             (cons "HOME=/home/target" (copy-sequence process-environment)))
            (exec-path client-path))
        (should (equal (getenv "HOME") "/home/target"))
        (let ((process-environment (remote-client-process-environment)))
          (should (equal (getenv "HOME") client-home)))
        (remote-with-client-environment
          (let ((process-environment
                 (cons "HOME=/home/deeper-target" process-environment)))
            (let ((process-environment (remote-client-process-environment)))
              (should (equal (getenv "HOME") client-home))))
          (should (equal (remote-client-exec-path) client-path)))))))

(ert-deftest remote-fs-exec-path-answers-from-the-workspace-capsule ()
  "Stock `executable-find' with REMOTE must see project tools such as direnv's.
The backend alone only knows the target's login PATH."
  (let ((default-directory "/fs:box:/work/project/"))
    (cl-letf (((symbol-function 'remote-fs--routes) (lambda (&rest _) '(route)))
              ((symbol-function 'remote-environment-resolve)
               (lambda (context &rest _)
                 (should (equal (remote-context-target-id context) "box"))
                 (remote-environment-create
                  :vars '(("PATH" . "/work/project/bin:/usr/bin")))))
              ((symbol-function 'remote-fs--call-routed)
               (lambda (&rest _) '("/usr/bin" "/bin" "/work/project/"))))
      ;; Capsule first; backend-only entries keep their place after it.
      (should (equal (remote-fs-handle-exec-path)
                     '("/work/project/bin" "/usr/bin" "/bin" "/work/project/"))))
    ;; Without a capsule PATH the backend still answers.
    (cl-letf (((symbol-function 'remote-fs--routes) (lambda (&rest _) '(route)))
              ((symbol-function 'remote-environment-resolve)
               (lambda (&rest _) (remote-environment-create :vars nil)))
              ((symbol-function 'remote-fs--call-routed)
               (lambda (operation _args) (list operation))))
      (should (equal (remote-fs-handle-exec-path) '(exec-path))))))

(ert-deftest remote-client-exec-path-drops-foreign-directories ()
  "A consumer may rebind `exec-path' to target directories around a call
that re-enters the framework.  The client boundary must not inherit it."
  (let ((remote--client-exec-path nil)
        (remote--buffer-base-exec-path nil)
        (remote--client-exec-path-snapshot nil)
        (client (list temporary-file-directory "/usr/bin")))
    (let ((exec-path client))
      (should (equal (remote-client-exec-path) client)))
    ;; Every entry belongs to another filesystem: the last usable client path
    ;; answers instead of an empty search path.
    (let ((exec-path '("/fs:elsewhere:/bin" "/fs:elsewhere:/usr/bin")))
      (should (equal (remote-client-exec-path) client)))
    ;; A partially foreign list keeps only what this machine can execute.
    (let ((exec-path (cons "/fs:elsewhere:/bin" client)))
      (should (equal (remote-client-exec-path) client)))))

(ert-deftest remote-ssh-control-check-is-not-forked-per-operation ()
  "Validating the pipeline must not cost a local fork per file operation.
Every routed operation validates before reusing the pooled connection, so an
unthrottled `ssh -O check' would dominate the remote call it guards."
  (let* ((control
          (remote-ssh-control-create
           :path (expand-file-name "remote-control-probe"
                                   temporary-file-directory)
           :destination "example"
           :state 'lazy))
         (remote-transport-ssh-control-check-interval 60)
         (calls 0))
    (cl-letf (((symbol-function 'remote-transport--ssh-control-command)
               (lambda (&rest _) '("true")))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq calls (1+ calls)) 0)))
      (should (remote-transport--ssh-control-check control))
      (should (= calls 1))
      ;; A cached answer needs the stage to have marked the control open,
      ;; which `remote-transport--ssh-live-p' does from the same result.
      (should (remote-transport--ssh-control-check control))
      (should (= calls 2))
      (setf (remote-ssh-control-state control) 'open)
      (dotimes (_ 20) (should (remote-transport--ssh-control-check control)))
      (should (= calls 2))
      ;; A caller that has to know right now still asks.
      (should (remote-transport--ssh-control-check control 'force))
      (should (= calls 3)))
    ;; A negative answer is never remembered as positive.
    (cl-letf (((symbol-function 'remote-transport--ssh-control-command)
               (lambda (&rest _) '("false")))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq calls (1+ calls)) 1)))
      (should-not (remote-transport--ssh-control-check control 'force))
      (should-not (remote-ssh-control-checked-at control))
      ;; A negative answer leaves nothing to reuse, so the next caller asks.
      (should-not (remote-transport--ssh-control-check control))
      (should (= calls 5)))))

(ert-deftest remote-file-operation-cost-reports-the-selected-backend ()
  "Consumers ask what an operation costs, never which backend is selected."
  (remote-test-with-registry
    (remote-register-adapter
     "emacs-file" :capabilities '(metadata)
     :preferences '((default . ("batched-files"))))
    (remote-register-backend
     "batched-files"
     :capabilities '(metadata)
     :project (lambda (_file _pipeline _route) "/batched/")
     :describe (lambda () '(:file-operation-cost batched)))
    (remote-register-backend
     "shell-files"
     :capabilities '(metadata)
     :project (lambda (_file _pipeline _route) "/shell/")
     :describe (lambda () '(:kind shell)))
    (remote-register-target "fast" :trusted t)
    (remote-register-pipeline
     "fast" "only" "batched-files" :config '(:transport "direct"))
    (remote-register-target "slow" :trusted t)
    (remote-register-pipeline
     "slow" "only" "shell-files" :config '(:transport "direct"))
    (should
     (eq (remote-file-operation-cost "/fs:fast:/work/a.el") 'batched))
    ;; A backend that declares nothing is assumed to pay a round trip.
    (should
     (eq (remote-file-operation-cost "/fs:slow:/work/a.el") 'round-trip))))

(ert-deftest remote-tramp-rpc-unbuildable-server-is-an-incompatibility ()
  "A server binary this client can never produce must not be retried.
An ordinary retryable failure would repeat the bootstrap connection and the
refused build on every remote operation."
  (let ((unbuildable
         (list 'remote-file-error
               (concat "Failed to obtain tramp-rpc-server for x86_64-linux.\n"
                       "Errors:\n"
                       "  build: Cannot cross-compile for x86_64-linux"
                       " on aarch64-darwin")))
        (transient
         (list 'remote-file-error
               "rpc process exited before answering method=system.info")))
    (should
     (equal
      (plist-get
       (remote-backend-tramp-rpc-classify-error unbuildable 'connect)
       :status)
      'incompatible))
    (should-not
     (plist-get
      (remote-backend-tramp-rpc-classify-error unbuildable 'connect)
      :retryable))
    (should
     (equal
      (plist-get
       (remote-backend-tramp-rpc-classify-error transient 'connect)
       :status)
      'failed))
    (should
     (plist-get
      (remote-backend-tramp-rpc-classify-error transient 'connect)
      :retryable))))

(ert-deftest remote-environment-overrides-augment-resolved-capsule ()
  (should
   (equal
    (remote--merge-environment-overrides
     '(("BASE" . "yes") ("PATH" . "/resolved/bin"))
     '(("PATH" . "/explicit/bin") ("EXTRA" . "ok")))
    '(("BASE" . "yes")
      ("PATH" . "/explicit/bin")
      ("EXTRA" . "ok")))))

(ert-deftest remote-local-bridge-command-keeps-local-as-a-target ()
  (remote-test-with-registry
    (let* ((context
            (remote-context-create
             :target-id "local"
             :localname temporary-file-directory
             :workspace-root
             (remote-make-file-name
              "local" temporary-file-directory)))
           (command
            (remote-local-bridge-command
             "sh" :args '("-c" "printf ok")
             :context context
             :environment '(("BRIDGE_TEST" . "yes")))))
      (should (equal (seq-take command 2) '("/bin/sh" "-c")))
      (should
       (string-match-p
        (regexp-quote
         (concat "cd -- "
                 (shell-quote-argument temporary-file-directory)))
        (nth 2 command)))
      (should
       (string-match-p
        (regexp-quote (shell-quote-argument "BRIDGE_TEST=yes"))
        (nth 2 command)))
      (should (string-match-p "sh -c" (nth 2 command)))
      (should
       (string-match-p
        (regexp-quote (shell-quote-argument "printf ok"))
        (nth 2 command))))))

(ert-deftest remote-make-client-process-escapes-a-logical-target-buffer ()
  (let* ((buffer (generate-new-buffer " *remote-client-process*"))
         (default-directory
          (remote-make-file-name "local" temporary-file-directory))
         (remote--buffer-base-process-environment
          (cons "REMOTE_CLIENT_TEST=client"
                (default-value 'process-environment)))
         (remote--buffer-base-exec-path (default-value 'exec-path))
         process)
    (unwind-protect
        (progn
          (setq process
                (remote-make-client-process
                 :name "remote-client-process"
                 :buffer buffer
                 :command
                 '("/bin/sh" "-c"
                   "printf '%s|%s' \"$REMOTE_CLIENT_TEST\" \"$PWD\"")
                 :noquery t))
          (while (process-live-p process)
            (accept-process-output process 0.1))
          (should (= (process-exit-status process) 0))
          (should
           (string-prefix-p
            (concat "client|"
                    (directory-file-name temporary-file-directory))
            (with-current-buffer buffer (buffer-string))))
          (should-not (process-get process 'remote-route)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest remote-interactive-process-class-applies-latency-policy ()
  (remote-test-with-registry
    (remote-register-adapter
     "language-server"
     :capabilities '(process-async)
     :preferences '((default . ("native")))
     :process-class 'interactive)
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (buffer (generate-new-buffer " *remote-interactive-process*"))
           (process
            (remote-make-process
             :name "remote-interactive-process"
             :buffer buffer
             :command '("sh" "-c" "printf ready")
             :remote-adapter "language-server"
             :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (should
             (equal
              (remote-process-description process)
              '(:class interactive
                :priority 100
                :adaptive-read-buffering nil)))
            (should
             (equal
              (plist-get
               (remote-backend-execution-metadata
                (process-get process 'remote-backend-execution))
               :process-class)
              'interactive)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest remote-tramp-rpc-connect-keeps-client-home-and-one-control-owner ()
  (remote-test-with-registry
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "ssh" "tramp-rpc" :config '(:host "box"))
    (let* ((route
            (remote-route-create
             :target-id "box"
             :pipeline-id "box/ssh"
             :backend-id "tramp-rpc"
             :capability 'process-sync
             :adapter-id "language-server"))
           (context
            (remote-context-create
             :target-id "box"
             :localname "/work/a.c"
             :workspace-root "/fs:box:/work/"))
           (remote--buffer-base-process-environment
            '("HOME=/Users/client" "PATH=/client/bin"))
           (remote--buffer-base-exec-path '("/client/bin"))
           (process-environment
            '("HOME=/home/target" "PATH=/home/target/bin"))
           (exec-path '("/home/target/bin"))
           (tramp-rpc-use-controlmaster t)
           (tramp-rpc-ssh-args nil)
           (tramp-rpc-ssh-options nil)
           observed)
      (cl-letf
          (((symbol-function 'remote-transport-ssh-control-options)
            (lambda (&optional _runtime)
              '("ControlMaster=auto"
                "ControlPersist=600"
                "ControlPath=/tmp/framework-control")))
           ((symbol-function 'remote-backend-project-file-name)
            (lambda (_route _file) "/rpc:box:/"))
           ((symbol-function 'remote-backend-tramp--method-login-args)
            (lambda (&rest _arguments) nil))
           ;; Answer only for the projected name.  A blanket stub would also
           ;; claim the client's own directories, and the client boundary
           ;; legitimately drops directories that belong to another
           ;; filesystem.
           ((symbol-function 'file-remote-p)
            (lambda (file &optional _identification connected)
              (and (not connected)
                   (stringp file)
                   (string-prefix-p "/rpc:" file)
                   "/rpc:box:")))
           ((symbol-function 'file-attributes)
            (lambda (_file &optional _id-format)
              (setq observed
                    (list
                     (getenv "HOME")
                     (copy-sequence exec-path)
                     tramp-rpc-use-controlmaster
                     (copy-sequence tramp-rpc-ssh-args)
                     (copy-sequence tramp-rpc-ssh-options)))
              '(directory))))
        (should
         (equal
          (remote-backend-tramp-connect route context)
          "/rpc:box:/")))
      (should
       (equal
        observed
        '("/Users/client"
          ("/client/bin")
          nil
          nil
          ("ControlMaster=auto"
           "ControlPersist=600"
           "ControlPath=/tmp/framework-control"
           "ServerAliveInterval=15"
           "ServerAliveCountMax=3"
           "ConnectTimeout=8"
           "ConnectionAttempts=1")))))))

(ert-deftest remote-emacs-file-adapter-supports-environment-operations ()
  "File-name handlers may receive `exec-path' while visiting a file.
Citre and similar packages use this operation from `find-file-hook', so
the default file adapter must route it without aborting later hooks."
  (remote-test-with-registry
    (should
     (memq 'environment
           (remote-adapter-capabilities
            (remote-get-adapter "emacs-file"))))))

(ert-deftest remote-backup-results-preserve-client-placement ()
  "A local backup cache path must not be projected into the target namespace."
  (let ((spec (remote-get-file-operation 'find-backup-file-name)))
    (should
     (eq (remote-file-operation-spec-result-kind spec)
         'placement-path-alist))
    (should
     (equal
      (remote-fs--transform-result
       spec '("/Users/client/.emacs-backups/a.c~") "box")
      '("/Users/client/.emacs-backups/a.c~")))
    (should
     (equal
      (remote-fs--transform-result
       spec '("/ssh:box:/var/backups/a.c~") "box")
      '("/fs:box:/var/backups/a.c~")))))

(ert-deftest remote-file-operations-do-not-inherit-process-only-adapters ()
  "Nested native file APIs keep the standard file caller contract."
  (remote-test-with-registry
    (remote-register-adapter
     "language-server"
     :capabilities '(process-sync process-async lsp environment))
    (let ((remote-current-adapter-id "language-server"))
      (should
       (equal
        (remote-fs--adapter-for-capability 'metadata)
        "emacs-file"))
      (should
       (equal
        (remote-fs--adapter-for-capability 'environment)
        "language-server")))))

(ert-deftest remote-canonicalize-tilde-default-does-not-reenter-target-inference ()
  (remote-test-with-registry
    (let* ((default-directory "~/.config/emacs/")
           (canonical
            (remote-canonicalize-file-name default-directory)))
      (should (equal (remote-fs-target-id canonical) "local"))
      (should
       (equal (remote-fs-localname canonical)
              (file-name-as-directory
               (expand-file-name "~/.config/emacs/")))))))

(ert-deftest remote-connection-pool-opens-once-and-reuses ()
  (remote-test-with-registry
    (let ((opens 0))
      (remote-register-link-plugin
       "pooled"
       :capabilities '(process-sync)
       :project-file-name (lambda (_file _link _route) "/pooled/")
       :connect
       (lambda (_route _context)
         (cl-incf opens)
         'handle)
       :connection-live-p
       (lambda (_connection _route _context) t))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "pooled")
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("pooled"))))
      (let* ((context
              (remote-context-create
               :target-id "lab" :localname "/work/a"
               :workspace-root "/fs:lab:/work/"))
             (route (remote-resolve "test" 'process-sync context))
             (first (remote-connection-ensure route context))
             (second (remote-connection-ensure route context)))
        (should (eq first second))
        (should (= opens 1))
        (should (= (remote-connection-use-count second) 2))
        (should (= (length (remote-connection-pool-status)) 1))))))

(ert-deftest remote-connection-read-lease-rechecks-when-disabled ()
  (remote-test-with-registry
    (let ((remote-connection--last-live-check
           (make-hash-table :test #'eq))
          (remote-connection-read-liveness-interval 10)
          (opens 0)
          (checks 0)
          (live t))
      (remote-register-link-plugin
       "leased"
       :capabilities '(metadata)
       :project-file-name (lambda (_file _link _route) "/leased/")
       :connect (lambda (_route _context)
                  (cl-incf opens)
                  'handle)
       :connection-live-p
       (lambda (_connection _route _context)
         (cl-incf checks)
         live))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "leased")
      (remote-register-adapter
       "test" :capabilities '(metadata)
       :preferences '((default . ("leased"))))
      (let* ((context
              (remote-context-create
               :target-id "lab" :localname "/work/a"
               :workspace-root "/fs:lab:/work/"))
             (route (remote-resolve "test" 'metadata context))
             (session (remote-connection-ensure route context)))
        (let ((remote-connection-liveness-lease-eligible t))
          (should (eq session (remote-connection-ensure route context)))
          (should (= checks 1))
          (setq live nil)
          (should (eq session (remote-connection-ensure route context)))
          (should (= checks 1))
          (puthash session (- (float-time) 20)
                   remote-connection--last-live-check)
          (should-not (eq session (remote-connection-ensure route context)))
          (should (= checks 2))
          (should (= opens 2)))
        ;; A caller outside the read-only file-query boundary still performs
        ;; a full check, even if the new session has a fresh lease.
        (let ((replacement (gethash (remote-connection-route-key route)
                                    remote-connection-pool)))
          (setq live t)
          (let ((remote-connection-liveness-lease-eligible t))
            (should (eq replacement
                        (remote-connection-ensure route context))))
          (setq live nil)
          (should-not (eq replacement
                          (remote-connection-ensure route context)))
          (should (= opens 3)))))))

(ert-deftest remote-connection-read-lease-does-not-hide-operation-failure ()
  (remote-test-with-registry
    (let ((remote-connection--last-live-check
           (make-hash-table :test #'eq))
          (remote-connection-read-liveness-interval 10))
      (remote-register-link-plugin
       "leased"
       :capabilities '(metadata)
       :project-file-name (lambda (_file _link _route) "/tmp/")
       :connect (lambda (_route _context) 'handle)
       :connection-live-p (lambda (_connection _route _context) t))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "leased")
      (let* ((logical "/fs:lab:/tmp/")
             (context (remote-context logical))
             (route (remote-resolve "emacs-file" 'metadata context))
             (session (remote-connection-ensure route context)))
        (let ((remote-connection-liveness-lease-eligible t))
          (should (eq session (remote-connection-ensure route context))))
        (cl-letf (((symbol-function 'remote-fs--call-underlying)
                   (lambda (&rest _)
                     (signal 'remote-transport-error
                             '("Injected transport loss")))))
          (should-error
           (remote-fs--call-routed 'file-attributes (list logical))
           :type 'remote-transport-error))
        (should-not (remote-connection-cached-p route))))))

(ert-deftest remote-read-query-successes-do-not-hide-failures-in-route-log ()
  (remote-test-with-registry
    (let ((remote-log-read-query-successes nil))
      (remote-register-link-plugin
       "read-log-test"
       :capabilities '(metadata)
       :project-file-name (lambda (_file _link _route) "/tmp/")
       :connect (lambda (_route _context) 'handle)
       :connection-live-p (lambda (_connection _route _context) t))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "read-log-test")
      (let ((logical "/fs:lab:/tmp/"))
        (should (remote-fs--call-routed 'file-exists-p (list logical)))
        (setq remote-route-log nil)
        (should (remote-fs--call-routed 'file-exists-p (list logical)))
        (should-not
         (seq-some
          (lambda (event)
            (memq (plist-get event :kind) '(route connection-reuse)))
          remote-route-log))
        (let ((remote-log-read-query-successes t))
          (should (remote-fs--call-routed 'file-exists-p (list logical))))
        (should
         (seq-some
          (lambda (event)
            (eq (plist-get event :kind) 'route))
          remote-route-log))
        (should
         (seq-some
          (lambda (event)
            (eq (plist-get event :kind) 'connection-reuse))
          remote-route-log))
        (setq remote-route-log nil)
        (cl-letf (((symbol-function 'remote-fs--call-underlying)
                   (lambda (&rest _)
                     (signal 'remote-transport-error
                             '("Injected transport loss")))))
          (should-error
           (remote-fs--call-routed 'file-exists-p (list logical))
           :type 'remote-transport-error))
        (should
         (seq-some
          (lambda (event)
            (eq (plist-get event :kind) 'failure))
          remote-route-log))))))

(ert-deftest remote-connection-reentrant-open-keeps-one-session-reference ()
  (remote-test-with-registry
    (let (context route nested-error reentered)
      (let ((attempts 0)
            (disconnects 0))
        (remote-register-link-plugin
         "reentrant"
         :capabilities '(process-sync)
         :project-file-name (lambda (_file _link _route) "/reentrant/")
         :connect
         (lambda (_route _context)
           (cl-incf attempts)
           (unless reentered
             (setq reentered t)
             (condition-case error
                 (remote-connection-ensure route context)
               (error (setq nested-error error))))
           'handle)
         :connection-live-p
         (lambda (_connection _route _context) t)
         :disconnect
         (lambda (_connection _route)
           (cl-incf disconnects)))
        (remote-register-target "lab" :trusted t)
        (remote-register-link "lab" "ssh" "reentrant")
        (remote-register-adapter
         "test" :capabilities '(process-sync)
         :preferences '((default . ("reentrant"))))
        (setq
         context
         (remote-context-create
          :target-id "lab" :localname "/work/a"
          :workspace-root "/fs:lab:/work/")
         route (remote-resolve "test" 'process-sync context))
        (let* ((session (remote-connection-ensure route context))
               (runtime
                (remote-connection-pipeline-runtime session)))
          (should (= attempts 1))
          (should (eq (car nested-error) 'remote-connection-busy))
          (should (= (hash-table-count remote-connection-pool) 1))
          (should (= (remote-pipeline-runtime-use-count runtime) 1))
          (remote-connection-pool-clear t)
          (should (= disconnects 1))
          (should (zerop (hash-table-count remote-connection-pool)))
          (should
           (zerop (hash-table-count remote-pipeline-runtime-pool))))))))

(ert-deftest remote-connection-cancelled-during-open-does-not-resurrect ()
  (remote-test-with-registry
    (let (context route disconnected-handles)
      (remote-register-link-plugin
       "cancelled"
       :capabilities '(process-sync)
       :project-file-name (lambda (_file _link _route) "/cancelled/")
       :connect
       (lambda (_route _context)
         (remote-connection-invalidate route t 'test-cancel)
         'opened-handle)
       :disconnect
       (lambda (connection _route)
         (push (remote-connection-handle connection)
               disconnected-handles)))
      (remote-register-target "lab" :trusted t)
      (remote-register-link "lab" "ssh" "cancelled")
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("cancelled"))))
      (setq
       context
       (remote-context-create
        :target-id "lab" :localname "/work/a"
        :workspace-root "/fs:lab:/work/")
       route (remote-resolve "test" 'process-sync context))
      (should-error
       (remote-connection-ensure route context)
       :type 'remote-connection-cancelled)
      (should (equal disconnected-handles '(opened-handle)))
      (should (zerop (hash-table-count remote-connection-pool)))
      (should (zerop (hash-table-count remote-pipeline-runtime-pool))))))

(ert-deftest remote-connection-cancelled-during-transport-open-releases-runtime ()
  "Cancellation before backend startup must release the acquired pipeline.
The session cannot publish its runtime until a yielding transport CONNECT
returns, so this covers a different ownership window from backend cancellation."
  (remote-test-with-registry
    (let (context route backend-opened)
      (remote-register-transport
       "cancel-session-transport"
       :connect
       (lambda (_stage _endpoint _runtime)
         (remote-connection-invalidate route t 'transport-cancel)
         'transport-handle)
       :disconnect (lambda (_stage-runtime _runtime) nil))
      (remote-register-link-plugin
       "transport-cancelled"
       :capabilities '(process-sync)
       :project-file-name
       (lambda (_file _link _route) "/transport-cancelled/")
       :connect
       (lambda (_route _context)
         (setq backend-opened t)
         'backend-handle))
      (remote-register-target "lab" :trusted t)
      (remote-register-pipeline
       "lab" "cancelled" "transport-cancelled"
       :stages
       '((:id "yielding"
          :transport "cancel-session-transport"
          :config (:host "lab"))))
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("transport-cancelled"))))
      (setq
       context
       (remote-context-create
        :target-id "lab" :localname "/work/a"
        :workspace-root "/fs:lab:/work/")
       route (remote-resolve "test" 'process-sync context))
      (should-error
       (remote-connection-ensure route context)
       :type 'remote-connection-cancelled)
      (should-not backend-opened)
      (should (zerop (hash-table-count remote-connection-pool)))
      (should (zerop (hash-table-count remote-pipeline-runtime-pool))))))

(ert-deftest remote-connection-progress-only-reports-new-session-phases ()
  (remote-test-with-registry
    (let (phases)
      (remote-register-link-plugin
       "progress-test"
       :capabilities '(process-sync)
       :project-file-name (lambda (_file _link _route) "/progress/")
       :connect (lambda (_route _context) 'handle)
       :connection-live-p (lambda (_connection _route _context) t))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline "lab" "ssh" "progress-test"))
             (route
              (remote-route-create
               :target-id "lab" :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "progress-test" :capability 'process-sync
               :adapter-id "process"))
             (context
              (remote-context-create
               :target-id "lab" :localname "/work/")))
        (let ((remote-connection-progress-hook
               (list (lambda (_connection _route phase)
                       (push phase phases)))))
          (should (remote-connection-ensure route context))
          (should (remote-connection-ensure route context)))
        (should (equal (nreverse phases)
                       '(transport backend probe ready)))
        (should (= (hash-table-count remote-connection-pool) 1))
        (remote-connection-invalidate route t 'test)
        (let ((remote-connection-progress-hook
               (list (lambda (&rest _arguments)
                       (error "Broken progress observer")))))
          (should (remote-connection-ensure route context)))
        (remote-connection-invalidate route t 'test)
        (let ((remote-connection-progress-hook
               (list (lambda (&rest _arguments)
                       (signal 'quit nil))))
              caught)
          (condition-case nil
              (remote-connection-ensure route context)
            (quit (setq caught t)))
          (should caught)
          (should (zerop (hash-table-count remote-connection-pool)))
          (should (zerop
                   (hash-table-count remote-pipeline-runtime-pool))))))))

(ert-deftest remote-connection-quit-cleans-partial-transport-and-progress ()
  (remote-test-with-registry
    (let (closed phases backend-started)
      (remote-register-transport
       "progress-first"
       :connect (lambda (_stage endpoint _runtime)
                  (remote-transport-result-create
                   :endpoint endpoint :handle 'first-handle))
       :disconnect (lambda (stage _runtime)
                     (push (remote-stage-runtime-handle stage) closed)))
      (remote-register-transport
       "progress-quit"
       :connect (lambda (_stage _endpoint _runtime)
                  (signal 'quit nil)))
      (remote-register-link-plugin
       "quit-test"
       :capabilities '(process-sync)
       :project-file-name (lambda (_file _link _route) "/quit/")
       :connect (lambda (_route _context)
                  (setq backend-started t)))
      (remote-register-target "lab" :trusted t)
      (let* ((pipeline
              (remote-register-pipeline
               "lab" "ssh" "quit-test"
               :stages '("progress-first" "progress-quit")))
             (route
              (remote-route-create
               :target-id "lab" :pipeline-id (remote-pipeline-id pipeline)
               :backend-id "quit-test" :capability 'process-sync
               :adapter-id "process"))
             (context
              (remote-context-create
               :target-id "lab" :localname "/work/"))
             caught)
        (let ((remote-connection-progress-hook
               (list (lambda (_connection _route phase)
                       (push phase phases)))))
          (condition-case nil
              (remote-connection-ensure route context)
            (quit (setq caught t))))
        (should caught)
        (should-not backend-started)
        (should (equal closed '(first-handle)))
        (should (equal (nreverse phases) '(transport cancelled)))
        (should (zerop (hash-table-count remote-connection-pool)))
        (should (zerop
                 (hash-table-count remote-pipeline-runtime-pool)))))))

(ert-deftest remote-connection-open-has-framework-deadline ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "blocking"
     :capabilities '(process-sync)
     :project-file-name (lambda (_file _link _route) "/blocking/")
     :connect
     (lambda (_route _context)
       (while t
         (accept-process-output nil 0.005))))
    (remote-register-target "offline" :trusted t)
    (remote-register-link "offline" "ssh" "blocking")
    (remote-register-adapter
     "test" :capabilities '(process-sync)
     :preferences '((default . ("blocking"))))
    (let* ((context
            (remote-context-create
             :target-id "offline" :localname "/work/a"
             :workspace-root "/fs:offline:/work/"))
           (route (remote-resolve "test" 'process-sync context))
           (remote-connection-open-timeout 0.03)
           (started (float-time)))
      (should-error
       (remote-connection-ensure route context)
       :type 'remote-connection-timeout)
      (should (< (- (float-time) started) 0.5))
      (should (zerop (hash-table-count remote-connection-pool)))
      (should-not (remote-pipeline-runtime-list)))))

(ert-deftest remote-path-and-uri-round-trip ()
  (remote-test-with-registry
    (should
     (equal (remote-canonicalize-file-name "/tmp/a b")
            "/fs:local:/tmp/a b"))
    (should
     (equal (remote-file-name-to-uri "/tmp/a b")
            "fs://local/tmp/a%20b"))
    (should
     (equal (remote-uri-to-file-name "fs://local/tmp/a%20b")
            "/fs:local:/tmp/a b"))
    (should (equal (remote-make-file-name "box" "")
                   "/fs:box:/"))
    (should (equal (remote-canonicalize-file-name "/ssh:box:")
                   "/fs:box:/"))
    (should (equal (remote-file-name-target "/tmp/a") "local"))
    (should (equal (remote-file-local-name "/fs:local:/tmp/a")
                   "/tmp/a"))
    (should (remote-file-equal-p
             "/tmp/../tmp/a" "fs://local/tmp/a"))))

(ert-deftest remote-canonicalize-tramp-home-on-the-target ()
  (remote-test-with-registry
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "ssh" '("tramp" "tramp-rpc")
     :config '(:host "box" :method "ssh"))
    (let ((original (symbol-function 'expand-file-name)))
      (cl-letf
          (((symbol-function 'expand-file-name)
            (lambda (name &optional directory)
              (if (equal name "/ssh:box:~/work/")
                  "/ssh:box:/home/me/work/"
                (funcall original name directory)))))
        (should
         (equal
          (remote-canonicalize-file-name "/ssh:box:~/work/")
          "/fs:box:/home/me/work/"))))
    (should-error
     (remote-make-file-name "box" "~/work/")
     :type 'error)))

(ert-deftest remote-expand-local-home-is-target-aware ()
  (remote-test-with-registry
    (let* ((spelling "~/Documents/Noema/")
           (expected
            (file-name-as-directory
             (expand-file-name spelling))))
      (should
       (equal
        (remote-expand-file-name spelling nil "local")
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (remote-canonicalize-file-name spelling)
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (abbreviate-file-name
         (remote-make-file-name "local" expected))
        (remote-make-file-name "local" expected)))
      (should
       (equal
        (remote-target-file-name
         (remote-get-target "local") spelling)
        (remote-make-file-name "local" expected))))))

(ert-deftest remote-result-projection-expands-abbreviated-local-home ()
  "Backend path results may use `~' but logical results must be absolute."
  (remote-test-with-registry
    (let* ((spec (remote-get-file-operation 'locate-dominating-file))
           (spelling "~/Documents/Noema/")
           (expected
            (remote-make-file-name
             "local" (file-name-as-directory (expand-file-name spelling)))))
      (should
       (equal (remote-fs--transform-result spec spelling "local")
              expected)))))

(ert-deftest remote-expand-home-is-owned-by-selected-backend ()
  (remote-test-with-registry
    (remote-register-backend
     "target-home"
     :capabilities '(metadata)
     :project (lambda (file _link _route) file)
     :expand-localname
     (lambda (name _directory _link _route)
       (if (string-prefix-p "~/" name)
           (concat "/home/remote/" (string-remove-prefix "~/" name))
         name)))
    (remote-register-target
     "box" :trusted t
     :workspaces
     '(((id . "main") (path . "~/work/"))))
    (remote-register-link "box" "home" "target-home")
    (should
     (equal
      (remote-expand-file-name "~/work/" nil "box")
      "/fs:box:/home/remote/work/"))
    (should
     (equal
      (remote-canonicalize-file-name "~/work/" "/fs:box:/")
      "/fs:box:/home/remote/work/"))
    (should
     (equal
      (gethash
       '("box" "~/work/") remote-fs-path-expansion-cache)
      "/home/remote/work/"))
    (let* ((logical
            (remote-target-file-name (remote-get-target "box")))
           (context (remote-context logical)))
      (should (equal logical "/fs:box:/home/remote/work/"))
      (should (equal (remote-context-workspace-id context) "main"))
      (should
       (equal
        (remote-context-workspace-root context)
        "/fs:box:/home/remote/work/")))))

(ert-deftest remote-context-selects-deepest-current-workspace ()
  (remote-test-with-registry
    (let* ((parent '((id . "parent") (path . "/srv/")))
           (nested '((id . "nested") (path . "/srv/project/")))
           (target (remote-register-target
                    "lab" :trusted t
                    :workspaces (list parent nested)))
           (path "/fs:lab:/srv/project/main.c"))
      (should (equal (remote-context-workspace-id (remote-context path))
                     "nested"))
      ;; A direct edit to the registered workspace must be visible on the
      ;; next file query; nonlocal context selection has no stale TTL.
      (setcdr (assq 'path nested) "/opt/project/")
      (should (eq (remote-fs--workspace-for target
                                            "/srv/project/main.c")
                  parent))
      (should (equal (remote-context-workspace-id (remote-context path))
                     "parent")))))

(ert-deftest remote-symlink-api-preserves-native-target-spelling ()
  (remote-test-with-registry
    (let* ((root (make-temp-file "remote-symlink-" t))
           (target (expand-file-name "target.txt" root))
           (relative-link (expand-file-name "relative-link" root))
           (absolute-link (expand-file-name "absolute-link" root))
           (logical-target (remote-canonicalize-file-name target))
           (logical-truename
            (remote-canonicalize-file-name (file-truename target)))
           (logical-relative
            (remote-canonicalize-file-name relative-link))
           (logical-absolute
            (remote-canonicalize-file-name absolute-link)))
      (unwind-protect
          (progn
            (with-temp-file target (insert "target"))
            (make-symbolic-link "target.txt" logical-relative)
            (should
             (equal (file-symlink-p logical-relative) "target.txt"))
            (should
             (equal (file-truename logical-relative) logical-truename))
            (should (file-equal-p logical-relative logical-target))
            (make-symbolic-link logical-target logical-absolute)
            (should
             (equal (file-symlink-p logical-absolute) target))
            (should
             (equal (file-truename logical-absolute) logical-truename))
            (delete-file target)
            (should
             (equal (file-symlink-p logical-relative) "target.txt")))
        (ignore-errors (delete-file relative-link))
        (ignore-errors (delete-file absolute-link))
        (ignore-errors (delete-file target))
        (ignore-errors (delete-directory root))))))

(ert-deftest remote-symlink-logical-target-never-becomes-tramp-text ()
  (remote-test-with-registry
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "ssh" "tramp"
     :config '(:host "box" :method "ssh"))
    (let* ((context
            (remote-context-create
             :target-id "box"
             :localname "/home/me/link"
             :workspace-root "/fs:box:/home/me/"))
           (route
            (remote-resolve "emacs-file" 'file-write context)))
      (should
       (equal
        (remote-fs--translate-args
         'make-symbolic-link
         '("/fs:box:/home/me/target"
           "/fs:box:/home/me/link")
         route)
        '("/home/me/target"
          "/ssh:box:/home/me/link")))
      (should-error
       (remote-fs--translate-args
        'make-symbolic-link
        '("/fs:local:/tmp/target"
          "/fs:box:/home/me/link")
        route)))))

(ert-deftest remote-direnv-skips-tramp-connection-buffer-prefix ()
  (remote-test-with-registry
    (let ((default-directory "/ssh:box:")
          buffer-file-name)
      (should (direnv--transport-connection-path-p default-directory))
      (should-not (direnv--directory))
      (should-not (direnv--envrc-root)))
    (with-temp-buffer
      (rename-buffer "*tramp/ssh box*" t)
      (setq default-directory "/ssh:box:/home/me/")
      (should (direnv--transport-connection-path-p default-directory))
      (should-not (direnv--directory)))))

(ert-deftest remote-direnv-root-discovery-coalesces-and-invalidates ()
  "Repeated root discovery must not cause repeated target RPCs."
  (let ((direnv--envrc-root-cache (make-hash-table :test #'equal))
        (direnv-envrc-root-cache-timeout 1.0)
        (directory-calls 0)
        (locate-calls 0)
        (clock 100.0)
        root)
    (cl-letf (((symbol-function 'direnv--directory)
               (lambda (&optional _path)
                 (cl-incf directory-calls)
                 "/fs:local:/tmp/project/"))
              ((symbol-function 'float-time)
               (lambda (&optional _value) clock))
              ((symbol-function 'locate-dominating-file)
               (lambda (_directory _name)
                 (cl-incf locate-calls)
                 root))
              ((symbol-function 'remote-canonicalize-file-name)
               #'identity))
      (should-not (direnv--envrc-root "/fs:local:/tmp/project/main.c"))
      (should-not (direnv--envrc-root "/fs:local:/tmp/project/main.c"))
      (should (= directory-calls 1))
      (should (= locate-calls 1))
      (setq root "/fs:local:/tmp/project/")
      (setq clock 102.0)
      (should (equal (direnv--envrc-root
                      "/fs:local:/tmp/project/main.c")
                     root))
      (should (= directory-calls 2))
      (should (= locate-calls 2))
      (setq root nil)
      (direnv-invalidate-root-cache)
      (should-not (direnv--envrc-root
                   "/fs:local:/tmp/project/main.c"))
      (should (= locate-calls 3)))))

(ert-deftest remote-direnv-visiting-file-directory-is-lexical ()
  "A visiting file has a known directory and needs no metadata probe."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/project/main.c")
    (cl-letf (((symbol-function 'direnv--transport-connection-path-p)
               (lambda (_path) nil))
              ((symbol-function 'remote-canonicalize-file-name)
               #'identity)
              ((symbol-function 'file-directory-p)
               (lambda (_path)
                 (ert-fail "Visiting-file root must not stat the file"))))
      (should (equal (direnv--directory) "/tmp/project/")))))

(ert-deftest remote-direnv-defers-all-discovery-while-tramp-is-busy ()
  (remote-test-with-registry
    (with-temp-buffer
      (let ((tramp-current-connection '(busy))
            scheduled)
        (cl-letf
            (((symbol-function 'direnv--envrc-root)
              (lambda (&rest _)
                (ert-fail "Busy refresh must not inspect .envrc")))
             ((symbol-function 'direnv--schedule-buffer-refresh)
              (lambda (buffer delay)
                (setq scheduled (list buffer delay)))))
          (direnv--refresh-buffer (current-buffer))
          (should (equal scheduled
                         (list (current-buffer)
                               direnv-transport-busy-retry-delay))))))))

(ert-deftest remote-direnv-busy-retry-completes-a-pending-ready-request ()
  "A busy retry which hits the cache must fulfill the original callback."
  (with-temp-buffer
    (let ((busy t)
          scheduled
          delivered
          (environment 'cached-environment)
          (context
           (remote-context-create
            :target-id "local"
            :localname "/tmp/project/"
            :workspace-root "/fs:local:/tmp/project/")))
      (cl-letf
          (((symbol-function 'direnv--transport-busy-p)
            (lambda () busy))
           ((symbol-function 'run-at-time)
            (lambda (_delay _repeat function &rest arguments)
              (setq scheduled
                    (lambda () (apply function arguments)))
              'mock-timer))
           ((symbol-function 'direnv--envrc-root)
            (lambda (&optional _path) "/fs:local:/tmp/project/"))
           ((symbol-function 'remote-context)
            (lambda (&optional _path) context))
           ((symbol-function 'direnv--fingerprint)
            (lambda (_context) '(fingerprint)))
           ((symbol-function 'direnv--cached-export)
            (lambda (_root _fingerprint) 'cached))
           ((symbol-function 'remote-environment-ensure)
            (lambda (&optional _context _force)
              (setq remote-buffer-environment environment)
              environment)))
        (should
         (eq
          (direnv-environment-ensure-async
           nil
           (lambda (result error)
             (setq delivered (list result error))))
          'pending))
        (should scheduled)
        (setq busy nil)
        (funcall scheduled)
        (should (equal delivered (list environment nil)))))))

(ert-deftest remote-direnv-contains-discovery-errors-inside-timer ()
  (remote-test-with-registry
    (with-temp-buffer
      (let (logged)
        (cl-letf
            (((symbol-function 'direnv--envrc-root)
              (lambda (&rest _)
                (signal 'remote-file-error '("stale connection"))))
             ((symbol-function 'remote-log)
              (lambda (&rest event) (setq logged event))))
          (direnv--refresh-buffer (current-buffer))
          (should (eq (car logged) 'direnv-error))
          (should (string-match-p
                   "stale connection"
                   (plist-get (cdr logged) :error))))))))

(ert-deftest remote-route-prefers-plugin-and-falls-back-on-health ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "slow" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-link-plugin
     "fast" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-target
     "lab" :preferences '((default . ("fast" "slow"))) :trusted t)
    (remote-register-link "lab" "primary" "fast" :priority 1)
    (remote-register-link "lab" "fallback" "slow" :priority 100)
    (remote-register-adapter "test" :capabilities '(file-read))
    (let* ((context (remote-context-create
                     :target-id "lab" :localname "/work/a"
                     :workspace-root "/fs:lab:/work/"))
           (route (remote-resolve "test" 'file-read context)))
      (should (equal (remote-route-link-plugin-id route) "fast"))
      (remote-report-route-failure
       route '(file-error "Connection refused"))
      (should
       (equal
        (remote-route-link-plugin-id
         (remote-resolve "test" 'file-read context))
        "slow")))))

(ert-deftest remote-one-link-can-offer-multiple-backend-plugins ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "slow" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-link-plugin
     "fast" :capabilities '(file-read)
     :project-file-name (lambda (file _link _route) file))
    (remote-register-target "lab" :trusted t)
    (remote-register-link "lab" "ssh" "slow" :priority 10)
    (remote-register-link "lab" "ssh" "fast" :priority 10)
    (remote-register-adapter
     "test" :capabilities '(file-read)
     :preferences '((default . ("fast" "slow"))))
    (let* ((context
            (remote-context-create
             :target-id "lab" :localname "/work/a"
             :workspace-root "/fs:lab:/work/"))
           (routes (remote-routes "test" 'file-read context)))
      (should (= (length (remote-links-for-target "lab")) 1))
      (should (= (length routes) 2))
      (should
       (equal (mapcar #'remote-route-link-id routes)
              '("lab/ssh" "lab/ssh")))
      (should
       (equal (remote-route-link-plugin-id (car routes)) "fast")))))

(ert-deftest remote-backend-incompatibility-falls-back-on-the-same-link ()
  (remote-test-with-registry
    (dolist (plugin '("tramp-rpc" "tramp"))
      (remote-register-link-plugin
       plugin
       :capabilities '(process-sync)
       :project-file-name
       (lambda (_file _link _route) temporary-file-directory)))
    (remote-register-target "pi" :trusted t)
    (remote-register-link
     "pi" "ssh" '("tramp-rpc" "tramp") :priority 10)
    (remote-register-adapter
     "test" :capabilities '(process-sync)
     :preferences '((default . ("tramp-rpc" "tramp"))))
    (let* ((context
            (remote-context-create
             :target-id "pi" :localname "/home/hc/a"
             :workspace-root "/fs:pi:/home/hc/"))
           (remote-environment-inhibit t)
           attempts result)
      (setq result
            (remote--call-with-process-route
             "test" 'process-sync context nil
             (lambda (route _directory _environment)
               (push (remote-route-link-plugin-id route) attempts)
               (if (equal (remote-route-link-plugin-id route)
                          "tramp-rpc")
                   (signal
                    'remote-backend-incompatible
                    '("Unknown architecture armv7l-linux"
                      (:architecture "armv7l-linux")))
                 'fallback-ok))))
      (should (eq result 'fallback-ok))
      (should (equal (nreverse attempts) '("tramp-rpc" "tramp")))
      (let* ((route
              (car (remote-routes "test" 'process-sync context)))
             (backend-health
              (remote-route-backend-health
               (remote-route-create
                :target-id "pi"
                :link-id "pi/ssh"
                :link-plugin-id "tramp-rpc"
                :capability 'process-sync
                :adapter-id "test"))))
        (should (equal (remote-route-link-plugin-id route) "tramp"))
        (should
         (eq (plist-get backend-health :status) 'incompatible))))))

(ert-deftest remote-workspace-process-prefers-owner-route-with-failover ()
  "A terminal or task tries its workspace backend before another one."
  (remote-test-with-registry
    (dolist (plugin '("tramp-rpc" "tramp"))
      (remote-register-link-plugin
       plugin :capabilities '(process-sync)
       :project-file-name
       (lambda (_file _link _route) temporary-file-directory)))
    (remote-register-target "box" :trusted t)
    (remote-register-link "box" "ssh" '("tramp-rpc" "tramp"))
    (remote-register-adapter
     "test" :capabilities '(process-sync)
     :preferences '((default . ("tramp-rpc" "tramp"))))
    (let* ((context
            (remote-context-create
             :target-id "box" :localname "/work/"
             :workspace-root "/fs:box:/work/"))
           (remote-environment-inhibit t)
           (remote-process-preferred-route
            (seq-find
             (lambda (route)
               (equal (remote-route-backend-id route) "tramp"))
             (remote-routes "test" 'process-sync context)))
           attempts)
      (should remote-process-preferred-route)
      (should
       (equal
        (remote--call-with-process-route
         "test" 'process-sync context nil
         (lambda (route _directory _environment)
           (push (remote-route-backend-id route) attempts)
           (if (equal (remote-route-backend-id route) "tramp")
               (signal 'remote-backend-incompatible
                       '("injected backend failure"))
             (remote-route-backend-id route))))
        "tramp-rpc"))
      (should (equal (nreverse attempts) '("tramp" "tramp-rpc"))))))

(ert-deftest remote-connection-failure-cools-the-whole-pipeline ()
  (remote-test-with-registry
    (let ((attempts 0))
      (dolist (plugin '("one" "two"))
        (remote-register-link-plugin
         plugin
         :capabilities '(process-sync)
         :project-file-name
         (lambda (_file _link _route) "/unreachable/")
         :connect
         (lambda (_route _context)
           (cl-incf attempts)
           (signal 'file-error '("Connection refused")))))
      (remote-register-target "offline" :trusted t)
      (remote-register-link
       "offline" "ssh" '("one" "two"))
      (remote-register-adapter
       "test" :capabilities '(process-sync)
       :preferences '((default . ("one" "two"))))
      (let ((context
             (remote-context-create
              :target-id "offline" :localname "/work/a"
              :workspace-root "/fs:offline:/work/"))
            (remote-environment-inhibit t))
        (should-error
         (remote--call-with-process-route
          "test" 'process-sync context nil
          (lambda (&rest _)
            (ert-fail "A failed connection must not run the operation")))
         :type 'file-error)
        ;; Both backends use the same physical pipeline.  A transport failure
        ;; must not retry that endpoint under another backend name.
        (should (= attempts 1))
        (should
         (eq
          (plist-get
           (remote-link-health
            (remote-get-link "offline/ssh") 'process-sync)
           :status)
          'failed))))))

(ert-deftest remote-tramp-rpc-maps-published-armv7-release ()
  (should
   (equal
    (remote-backend-tramp-rpc--arch-to-rust-target-a
     #'identity "armv7l-linux")
    "armv7-unknown-linux-musleabihf")))

(ert-deftest remote-tramp-ssh-options-preserve-argv-boundaries ()
  (should
   (equal
    (remote-backend-tramp--ssh-raw-args
     '("ConnectTimeout=8" "ConnectionAttempts=1"))
    '("-o" "ConnectTimeout=8" "-o" "ConnectionAttempts=1"))))

(ert-deftest remote-tramp-ssh-server-alive-defaults-respect-target-options ()
  "Keepalives cover managed SSH unless the target explicitly overrides them."
  (let ((remote-backend-tramp-ssh-server-alive-interval 15)
        (remote-backend-tramp-ssh-server-alive-count-max 3)
        (config '(:host "box")))
    (cl-letf (((symbol-function 'remote-pipeline-effective-config)
               (lambda (_pipeline) config))
              ((symbol-function 'remote-transport-ssh-control-options)
               (lambda (&optional _runtime) nil)))
      (let ((options (remote-backend-tramp--ssh-options 'pipeline)))
        (should (member "ServerAliveInterval=15" options))
        (should (member "ServerAliveCountMax=3" options)))
      (setq config
            '(:host "box" :ssh-options
                    ("serveraliveinterval=7" "ServerAliveCountMax=1")))
      (let ((options (remote-backend-tramp--ssh-options 'pipeline)))
        (should (member "serveraliveinterval=7" options))
        (should (member "ServerAliveCountMax=1" options))
        (should-not (member "ServerAliveInterval=15" options))
        (should-not (member "ServerAliveCountMax=3" options)))
      (setq config '(:host "box"))
      (let ((remote-backend-tramp-ssh-server-alive-interval 0)
            (remote-backend-tramp-ssh-server-alive-count-max nil))
        (let ((options (remote-backend-tramp--ssh-options 'pipeline)))
          (should (member "ServerAliveInterval=0" options))
          (should-not (seq-some
                       (lambda (option)
                         (string-prefix-p "ServerAliveCountMax=" option))
                       options)))))))

(ert-deftest remote-tramp-rpc-local-relays-use-a-local-directory ()
  (let ((default-directory "/rpc:box:/work/")
        seen-directory)
    (cl-letf
        (((symbol-function 'start-process)
          (lambda (&rest _arguments)
            (setq seen-directory default-directory)
            'relay)))
      (should
       (eq
        (remote-backend-tramp-rpc--local-relay-cwd-a
         (lambda () (start-process "relay" nil "cat")))
        'relay)))
    (should (equal seen-directory temporary-file-directory))))

(ert-deftest remote-tramp-rpc-controlmaster-path-stays-on-client ()
  "The PTY socket path uses client HOME despite a target buffer context."
  (let ((default-directory "/rpc:box:/work/")
        (remote--buffer-base-process-environment
         '("HOME=/Users/client" "PATH=/client/bin"))
        (remote--buffer-base-exec-path '("/client/bin"))
        (process-environment '("HOME=/home/target" "PATH=/target/bin"))
        (exec-path '("/target/bin"))
        observed)
    (remote-backend-tramp-rpc--local-controlmaster-path-a
     (lambda ()
       (setq observed
             (list (expand-file-name "~/.ssh/tramp-rpc/socket")
                   default-directory (getenv "HOME") exec-path))))
    (should
     (equal observed
            (list "/Users/client/.ssh/tramp-rpc/socket"
                  temporary-file-directory "/Users/client"
                  '("/client/bin"))))))

(ert-deftest remote-tramp-rpc-encodes-large-process-environments ()
  ;; The -Q contract suite does not call `package-initialize'.  Load the
  ;; installed msgpack source explicitly so this compatibility check runs
  ;; when the dependency is present instead of silently skipping it.
  (let ((load-path
         (append
          (cl-remove-if-not
           #'file-directory-p
           (file-expand-wildcards
            (expand-file-name "elpa/msgpack-*" user-emacs-directory)
            t))
          load-path)))
    (skip-unless (require 'msgpack nil t))
    (remote-backend-tramp-rpc-install)
    (let* ((environment
            (cl-loop for index below 78
                     collect (cons (format "REMOTE_TEST_%02d" index)
                                   (format "value-%02d" index))))
           (encoded (msgpack-encode environment))
           (msgpack-map-type 'alist)
           (msgpack-key-type 'string)
           (decoded (msgpack-read-from-string encoded)))
      (should (= (length decoded) 78))
      (should (equal (cdr (assoc "REMOTE_TEST_00" decoded)) "value-00"))
      (should (equal (cdr (assoc "REMOTE_TEST_77" decoded)) "value-77")))))

(ert-deftest remote-project-file-name-accepts-explicit-link ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "prefix"
     :capabilities '(file-read)
     :project-file-name
     (lambda (file link _route)
       (concat (plist-get (remote-link-config link) :prefix)
               (remote-file-local-name file))))
    (remote-register-target "box" :trusted t)
    (remote-register-link
     "box" "one" "prefix" :config '(:prefix "/transport"))
    (should
     (equal
      (remote-project-file-name "/fs:box:/work/a" "one")
      "/transport/work/a"))))

(ert-deftest remote-local-visit-keeps-logical-buffer-identity ()
  (remote-test-with-registry
    (let* ((native (make-temp-file "remote-visit-" nil ".txt" "hello"))
           (logical (remote-canonicalize-file-name native))
           buffer)
      (unwind-protect
          (progn
            (setq buffer (find-file-noselect logical))
            (with-current-buffer buffer
              (should (equal buffer-file-name logical))
              (should (remote-fs-file-name-p buffer-file-truename))
              (should (equal (buffer-string) "hello"))
              (should (file-exists-p buffer-file-name))
              (should (verify-visited-file-modtime buffer))
              (set-visited-file-modtime)
              (should (verify-visited-file-modtime buffer))
              (should (stringp (make-auto-save-file-name)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (file-exists-p native) (delete-file native))))))

(ert-deftest remote-ordinary-local-visit-keeps-native-buffer-identity ()
  (let ((file (make-temp-file "remote-native-visit-" nil ".el"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should-not (remote-fs-file-name-p buffer-file-name))
            (should-not (file-remote-p buffer-file-name))
            (should
             (equal
              (expand-file-name buffer-file-name)
              (expand-file-name file)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest remote-native-process-status-does-not-reenter-fs-handler ()
  (remote-test-with-registry
    (let* ((buffer (generate-new-buffer " *remote-native-process*"))
           (process (make-pipe-process
                     :name "remote-native-process"
                     :buffer buffer
                     :noquery t)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory
                    (remote-canonicalize-file-name
                     temporary-file-directory)))
            (let ((tramp-file-name-for-operation-external
                   (cons '(process-status . process)
                         tramp-file-name-for-operation-external)))
              (should
               (eq (remote-fs-file-name-handler
                    'process-status process)
                   'open))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest remote-fs-supports-tramp-without-external-operation-table ()
  (should (boundp 'tramp-file-name-for-operation-external))
  (let ((tramp-file-name-for-operation-external nil)
        routed-operation)
    (cl-letf (((symbol-function 'remote-fs--call-routed)
               (lambda (operation _args)
                 (setq routed-operation operation)
                 'routed)))
      (should
       (eq (remote-fs-file-name-handler
            'file-readable-p "/fs:local:/tmp/example")
           'routed))
      (should (eq routed-operation 'file-readable-p)))))

(ert-deftest remote-mock-target-routes-file-operations ()
  (remote-test-with-registry
    (let* ((root (make-temp-file "remote-target-" t))
           (native (expand-file-name "note.txt" root))
           (logical "/fs:mock:/workspace/note.txt"))
      (unwind-protect
          (progn
            (with-temp-file native (insert "routed"))
            (remote-register-link-plugin
             "mock"
             :capabilities remote-capabilities
             :project-file-name
             (lambda (file _link _route)
               (expand-file-name
                (string-remove-prefix
                 "/workspace/" (remote-file-local-name file))
                root)))
            (remote-register-target "mock" :trusted t)
            (remote-register-link "mock" "only" "mock")
            (should (file-exists-p logical))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents logical)
                (buffer-string))
              "routed"))
            (should (equal (file-remote-p logical 'host) "mock"))
            (should (equal (file-local-name logical)
                           "/workspace/note.txt")))
        (when (file-exists-p native) (delete-file native))
        (when (file-directory-p root) (delete-directory root))))))

(ert-deftest remote-directory-enumeration-preserves-dot-entry-contract ()
  "FULL directory results must retain literal `.' and `..' entries.
Normalizing those suffixes turns them into visible parent/current directory
names in tree consumers such as Treemacs."
  (remote-test-with-registry
    (let* ((root (make-temp-file "remote-directory-" t))
           (child (expand-file-name "child" root))
           (logical
            (file-name-as-directory
             (remote-canonicalize-file-name root)))
           (logical-dot (concat logical "."))
           (logical-dot-dot (concat logical "..")))
      (unwind-protect
          (progn
            (make-directory child)
            (let ((entries (directory-files logical t nil t)))
              (should (member logical-dot entries))
              (should (member logical-dot-dot entries))
              (should (equal (directory-file-name logical-dot)
                             logical-dot))
              (should (equal (directory-file-name logical-dot-dot)
                             logical-dot-dot))
              (should (equal (file-name-nondirectory logical-dot) "."))
              (should (equal (file-name-nondirectory logical-dot-dot) ".."))
              (should
               (equal
                (sort (mapcar #'file-name-nondirectory entries)
                      #'string-lessp)
                '("." ".." "child"))))
            (let ((entries
                   (directory-files-and-attributes logical t nil t)))
              (should (assoc logical-dot entries))
              (should (assoc logical-dot-dot entries))
              (should
               (equal
                (sort
                 (mapcar
                  (lambda (entry)
                    (file-name-nondirectory (car entry)))
                  entries)
                 #'string-lessp)
                '("." ".." "child")))))
        (when (file-directory-p child)
          (delete-directory child))
        (when (file-directory-p root)
          (delete-directory root))))))

(ert-deftest remote-fs-install-restores-fast-direct-dispatch ()
  (remote-test-with-registry
    ;; Reproduce daemon/reload startup with TRAMP already loaded but its
    ;; top-level file-name handler temporarily removed.
    (let ((file-name-handler-alist nil))
      (remote-fs-install)
      (should
       (eq (find-file-name-handler
            "/fs:local:/tmp/" 'file-directory-p)
           #'remote-fs--direct-file-name-handler))
      (should
       (eq (find-file-name-handler
            "/fs:local:/tmp/" 'expand-file-name)
           #'remote-fs--direct-file-name-handler))
      (should
       (eq (find-file-name-handler
            "/fs:local:/tmp/" 'unhandled-file-name-directory)
           #'tramp-file-name-handler))
      (remote-fs-install)
      (should
       (= 1 (seq-count
             (lambda (item)
               (eq (cdr item) #'remote-fs--direct-file-name-handler))
             file-name-handler-alist)))
      (should (file-directory-p "/fs:local:/tmp/")))))

(ert-deftest remote-fs-direct-expansion-preserves-logical-paths ()
  (remote-test-with-registry
    (remote-fs-install)
    (should (equal (expand-file-name "/fs:local:/tmp/a/../b")
                   "/fs:local:/tmp/b"))
    (should (equal (expand-file-name "/fs:local:/tmp//.git/")
                   "/fs:local:/tmp/.git/"))
    (let ((default-directory "/fs:local:/tmp/work/"))
      (should (equal (expand-file-name "child")
                     "/fs:local:/tmp/work/child"))
      (should (equal (expand-file-name "../other")
                     "/fs:local:/tmp/other"))
      (should (equal (expand-file-name "/tmp/native.txt")
                     "/tmp/native.txt")))))

(ert-deftest remote-fs-direct-dispatch-defers-unknown-operations-to-tramp ()
  (let (seen)
    (cl-letf (((symbol-function 'tramp-file-name-handler)
               (lambda (operation &rest args)
                 (setq seen (cons operation args))
                 'upstream)))
      (should
       (eq (remote-fs--direct-file-name-handler
            'future-emacs-operation "/fs:local:/tmp/")
           'upstream)))
    (should (equal seen '(future-emacs-operation "/fs:local:/tmp/")))))

(ert-deftest remote-fs-direct-dispatch-routes-remote-metadata ()
  (let (seen)
    (cl-letf (((symbol-function 'remote-fs-file-name-handler)
               (lambda (operation &rest args)
                 (setq seen (cons operation args))
                 'routed))
              ((symbol-function 'tramp-file-name-handler)
               (lambda (&rest _args)
                 (ert-fail "Remote metadata reentered TRAMP parsing"))))
      (should
       (eq (remote-fs--direct-file-name-handler
            'file-exists-p "/fs:lab:/tmp/")
           'routed)))
    (should (equal seen '(file-exists-p "/fs:lab:/tmp/")))))

(ert-deftest remote-fs-preferred-route-cache-observes-live-changes ()
  (remote-test-with-registry
    (let* ((target
            (remote-register-target
             "lab" :trusted t
             :preferences '((default . ("fast" "slow")))))
           (_fast
            (remote-register-link-plugin
             "fast" :capabilities '(metadata)
             :available-p (lambda (_link _context) t)))
           (slow-available t)
           (_slow
            (remote-register-link-plugin
             "slow" :capabilities '(metadata)
             :available-p (lambda (_link _context) slow-available)))
           (link (remote-register-link "lab" "ssh" '("fast" "slow")))
           (context
            (remote-context-create
             :target-id "lab" :localname "/tmp/"))
           (original-routes (symbol-function 'remote-routes))
           (calls 0))
      (cl-letf (((symbol-function 'remote-routes)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply original-routes args))))
        (should
         (equal (mapcar #'remote-route-link-plugin-id
                        (remote-fs--routes "emacs-file" 'metadata context))
                '("fast" "slow")))
        (should (= calls 1))
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 1))
        ;; Editing an existing preference list must invalidate its snapshot.
        (setcar (cdr (assq 'default (remote-target-preferences target)))
                "slow")
        (should
         (equal (remote-route-link-plugin-id
                 (car (remote-fs--routes
                       "emacs-file" 'metadata context)))
                "slow"))
        (should (= calls 2))
        (setcar (cdr (assq 'default (remote-target-preferences target)))
                "fast")
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 3))
        (puthash
         (remote--backend-health-key link "fast" 'metadata)
         (list :status 'failed :failed-at (float-time))
         remote-route-health)
        (should
         (equal (remote-route-link-plugin-id
                 (car (remote-fs--routes
                       "emacs-file" 'metadata context)))
                "slow"))
        (should (= calls 4))
        (remhash (remote--backend-health-key link "fast" 'metadata)
                 remote-route-health)
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 5))
        ;; Availability may change inside a registered plugin closure.
        (setq slow-available nil)
        (should
         (equal (mapcar #'remote-route-link-plugin-id
                        (remote-fs--routes
                         "emacs-file" 'metadata context))
                '("fast")))
        (should (= calls 6))
        (setq slow-available t)
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 7))
        (remote-register-link-plugin
         "fast" :capabilities '(metadata)
         :available-p (lambda (_link _context) nil))
        (should
         (equal (mapcar #'remote-route-link-plugin-id
                        (remote-fs--routes
                         "emacs-file" 'metadata context))
                '("slow")))
        (should (= calls 8))))))

(ert-deftest remote-fs-preferred-route-cache-observes-adapter-and-context-edits ()
  "Snapshot checks must notice in-place preference edits on every owner."
  (remote-test-with-registry
    (let* ((_target (remote-register-target "lab" :trusted t))
           (adapter
            (remote-register-adapter
             "emacs-file" :capabilities '(metadata)
             :preferences '((default . ("fast" "slow")))))
           (_fast
            (remote-register-link-plugin
             "fast" :capabilities '(metadata)
             :available-p (lambda (_link _context) t)))
           (_slow
            (remote-register-link-plugin
             "slow" :capabilities '(metadata)
             :available-p (lambda (_link _context) t)))
           (_link (remote-register-link "lab" "ssh" '("fast" "slow")))
           (context (remote-context-create
                     :target-id "lab" :localname "/tmp/"))
           (original-routes (symbol-function 'remote-routes))
           (calls 0))
      (cl-letf (((symbol-function 'remote-routes)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply original-routes args))))
        (should (equal (remote-route-link-plugin-id
                        (car (remote-fs--routes
                              "emacs-file" 'metadata context)))
                       "fast"))
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 1))
        (setcar (cdr (assq 'default (remote-adapter-preferences adapter)))
                "slow")
        (should (equal (remote-route-link-plugin-id
                        (car (remote-fs--routes
                              "emacs-file" 'metadata context)))
                       "slow"))
        (should (= calls 2))
        (setcar (cdr (assq 'default (remote-adapter-preferences adapter)))
                "fast")
        (remote-fs--routes "emacs-file" 'metadata context)
        (should (= calls 3))
        (setf (remote-context-source context)
              '((preferences . ((default . ("slow" "fast"))))))
        (should (equal (remote-route-link-plugin-id
                        (car (remote-fs--routes
                              "emacs-file" 'metadata context)))
                       "slow"))
        (should (= calls 4))
        (setcar
         (cdr (assq 'default
                    (alist-get 'preferences (remote-context-source context))))
         "fast")
        (should (equal (remote-route-link-plugin-id
                        (car (remote-fs--routes
                              "emacs-file" 'metadata context)))
                       "fast"))
        (should (= calls 5))))))

(ert-deftest remote-fs-context-cache-observes-new-remote-workspace ()
  (remote-test-with-registry
    (let* ((target (remote-register-target "lab" :trusted t))
           (path "/fs:lab:/work/a.el")
           (first (remote-fs--context-for-file path)))
      (should-not (remote-context-workspace-id first))
      (should (gethash path remote-fs-context-cache))
      (setf (remote-target-workspaces target)
            '(((id . "main") (path . "/work/"))))
      (let ((updated (remote-fs--context-for-file path)))
        (should (equal (remote-context-workspace-id updated) "main"))
        (should (equal (remote-context-workspace-root updated)
                       "/fs:lab:/work/"))))))

(ert-deftest remote-backend-tramp-prefix-follows-active-endpoint-and-hops ()
  (remote-test-with-registry
    (let* ((link (remote-link-create
                  :id "lab/ssh" :target-id "lab"
                  :config '(:host "fallback")))
           (endpoint (remote-endpoint-create :host "first"))
           (remote-current-pipeline-runtime
            (remote-pipeline-runtime-create
             :pipeline-id "lab/ssh" :endpoint endpoint))
           (effective-config (symbol-function 'remote-pipeline-effective-config))
           (config-calls 0))
      (cl-letf (((symbol-function 'remote-pipeline-effective-config)
                 (lambda (&rest args)
                   (cl-incf config-calls)
                   (apply effective-config args))))
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/one" link "rpc")
                       "/rpc:first:/tmp/one"))
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/two" link "rpc")
                       "/rpc:first:/tmp/two"))
        (should (= config-calls 1))
        (setf (remote-endpoint-host endpoint) "second")
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/three" link "rpc")
                       "/rpc:second:/tmp/three"))
        (should (= config-calls 2))
        (aset (remote-endpoint-host endpoint) 0 ?n)
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/mutated" link "rpc")
                       "/rpc:necond:/tmp/mutated"))
        (should (= config-calls 3))
        (setf (remote-endpoint-host endpoint) nil)
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/fallback" link "rpc")
                       "/rpc:fallback:/tmp/fallback"))
        (should (= config-calls 4))
        (aset (plist-get (remote-link-config link) :host) 0 ?t)
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/config-mutated" link "rpc")
                       "/rpc:tallback:/tmp/config-mutated"))
        (should (= config-calls 5))
        (setf (remote-link-config link)
              (list :host "fallback"
                    :hops (list (remote-endpoint-create :host "jump")
                                (remote-endpoint-create :host "second"))))
        (should (equal (remote-backend-tramp-file-name
                        "/tmp/four" link "rpc")
                       "/ssh:jump|rpc:second:/tmp/four"))
        (should (= config-calls 6))))))

(ert-deftest remote-process-and-executable-use-logical-context ()
  (remote-test-with-registry
    (let ((default-directory
           (remote-canonicalize-file-name temporary-file-directory)))
      (with-temp-buffer
        (should
         (zerop
          (remote-process-file
           "sh" nil t nil "-c" "printf routed")))
        (should (equal (buffer-string) "routed")))
      (should (file-name-absolute-p
               (remote-executable-find "sh"))))))

(ert-deftest remote-executable-find-accepts-target-native-absolute-path ()
  "An argv resolved on the target remains valid during lsp-mode's recheck."
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (shell (remote-executable-find "sh")))
      (should (file-name-absolute-p shell))
      (should (equal (remote-executable-find shell) shell)))))

(ert-deftest remote-rpc-executable-probe-preserves-path-and-missing-result ()
  "The target shell result is framed, and unsupported output falls back."
  (let ((calls 0))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _destination _display
                        &rest _args)
                 (cl-incf calls)
                 (insert "/target/my tools/server\0")
                 0)))
      (should (equal (remote--rpc-executable-find "server")
                     '(t . "/target/my tools/server")))
      (should-not (remote--rpc-executable-find "relative/server"))
      (should (= calls 1))))
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) 1)))
    (should (equal (remote--rpc-executable-find "missing") '(t))))
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert "unexpected shell output")
               0)))
    (should-not (remote--rpc-executable-find "server")))
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) (error "no POSIX shell"))))
    (should-not (remote--rpc-executable-find "server"))))

(ert-deftest remote-process-file-projects-logical-file-arguments ()
  "INFILE and stderr paths follow the route just like default-directory."
  (remote-test-with-registry
    (remote-register-adapter
     "emacs-file"
     :capabilities '(metadata)
     :preferences '((default . ("native"))))
    (let* ((input (make-temp-file "remote-process-input-"))
           (stderr (make-temp-file "remote-process-stderr-"))
           (logical-input (remote-canonicalize-file-name input))
           (logical-stderr (remote-canonicalize-file-name stderr))
           (default-directory
            (remote-canonicalize-file-name temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file input (insert "logical-input"))
            (with-temp-buffer
              (should
               (zerop
                (remote-process-file
                 "cat" logical-input t nil)))
              (should (equal (buffer-string) "logical-input")))
            (should
             (zerop
              (remote-process-file
               "sh" nil (list nil logical-stderr) nil
               "-c" "printf logical-stderr >&2")))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents stderr)
                (buffer-string))
              "logical-stderr")))
        (dolist (file (list input stderr))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest remote-exec-returns-structured-route-result ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (result
            (remote-exec
             "sh" :args '("-c" "printf stdout; printf stderr >&2")
             :check t)))
      (should (zerop (remote-exec-result-status result)))
      (should (equal (remote-exec-result-stdout result) "stdout"))
      (should (equal (remote-exec-result-stderr result) "stderr"))
      (should
       (equal (remote-route-link-plugin-id
               (remote-exec-result-route result))
              "native")))))

(ert-deftest remote-exec-async-uses-routed-make-process ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           result
           (process
            (remote-exec-async
             "sh"
             :args '("-c" "printf async; printf problem >&2")
             :callback (lambda (value) (setq result value)))))
      (while (and (process-live-p process) (not result))
        (accept-process-output process 0.1))
      (unless result
        (accept-process-output process 0.1))
      (should (remote-exec-result-p result))
      (should (zerop (remote-exec-result-status result)))
      (should (equal (remote-exec-result-stdout result) "async"))
      (should (equal (remote-exec-result-stderr result) "problem"))
      (should
       (equal
        (remote-route-link-plugin-id
        (remote-exec-result-route result))
        "native")))))

(ert-deftest remote-tramp-stderr-frame-roundtrips-streams ()
  (let* ((token "frame-token")
         (combined (concat "stdout\036" token "\037stderr"))
         (streams (remote--split-stderr-frame combined token)))
    (should (equal (car streams) "stdout"))
    (should (equal (cdr streams) "stderr"))
    (should
     (equal (remote--split-stderr-frame "partial output" token)
            '("partial output" . "")))))

(ert-deftest remote-standard-tramp-process-uses-direct-ssh-stderr ()
  (let* ((route
          (remote-route-create
           :target-id "box"
           :pipeline-id "box/ssh"
           :backend-id "tramp"
           :capability 'process-async
           :adapter-id "exec"))
         (context
          (remote-context-create
           :target-id "box"
           :localname "/tmp/"
           :workspace-root "/fs:box:/tmp/"))
         (process
          (make-pipe-process
           :name "remote-test-tramp-stderr" :noquery t))
         (stdout (generate-new-buffer " *remote-test-stdout*"))
         (stderr (generate-new-buffer " *remote-test-stderr*"))
         captured)
    (unwind-protect
        (cl-letf
            (((symbol-function 'remote--call-with-process-route)
              (lambda (_adapter _capability _context _constraints function)
                (funcall function route temporary-file-directory nil)))
             ((symbol-function 'remote--prepare-backend-execution)
             (lambda (_route _context command _environment directory
                              &optional _logical-directory)
                (remote-backend-execution-create
                 :route route
                 :context context
                 :logical-directory "/fs:box:/tmp/"
                 :physical-directory directory
                 :command command)))
             ((symbol-function
              'remote-backend-tramp-direct-async-command)
              (lambda (_route command _environment directory &optional _tty)
                (should (equal directory "/tmp/"))
                (append '("/usr/bin/ssh" "-T" "box") command)))
             ((symbol-function 'make-process)
              (lambda (&rest arguments)
                (setq captured arguments)
                process)))
          (remote-make-process
           :name "remote-test-tramp-stderr"
           :buffer stdout
           :stderr stderr
           :command '("sh" "-c" "printf output")
           :remote-context context
           :remote-stderr-token "token")
          (should (eq (plist-get captured :stderr) stderr))
          (should
           (equal (car (plist-get captured :command)) "/usr/bin/ssh"))
          (should-not (process-get process 'remote-stderr-token))
          (should (process-get process 'remote-direct-ssh)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(ert-deftest remote-standard-tramp-pty-spawns-client-ssh ()
  "A target PTY must use direct SSH without a TRAMP process handler."
  (let* ((route
          (remote-route-create
           :target-id "box" :pipeline-id "box/ssh"
           :backend-id "tramp" :capability 'pty :adapter-id "process"))
         (context
          (remote-context-create
           :target-id "box" :localname "/tmp/"
           :workspace-root "/fs:box:/tmp/"))
         (execution
          (remote-backend-execution-create
           :route route :context context
           :command '("/bin/sh" "-l")))
         seen)
    (cl-letf
        (((symbol-function 'remote-backend-tramp-direct-async-command)
          (lambda (_route _command _environment directory tty)
            (setq seen (list directory tty))
            '("/usr/bin/ssh" "-tt" "box" "exec /bin/sh -l"))))
      (let* ((plan
              (remote-backend-tramp-prepare-process
               execution '(:command ("/bin/sh" "-l")
                           :connection-type pty)
               '(("TERM" . "xterm-256color"))))
             (arguments (remote-backend-process-plan-arguments plan)))
        (should (equal seen '("/tmp/" t)))
        (should (equal (plist-get arguments :command)
                       '("/usr/bin/ssh" "-tt" "box"
                         "exec /bin/sh -l")))
        (should (eq (plist-get arguments :file-handler) nil))
        (should (equal (remote-backend-process-plan-default-directory plan)
                       temporary-file-directory))
        (should (plist-get (remote-backend-process-plan-metadata plan)
                           :pty))))))

(ert-deftest remote-ssh-pty-connect-skips-tramp-file-session ()
  "A terminal-only backend must not warm TRAMP's file connection."
  (let ((route
         (remote-route-create
          :target-id "box" :pipeline-id "box/ssh"
          :backend-id "ssh-pty" :capability 'pty :adapter-id "process")))
    (should (equal (remote-backend-capabilities
                    (remote-get-backend "ssh-pty"))
                   '(pty)))
    (cl-letf (((symbol-function 'remote-backend-tramp--pipeline-ssh-parts)
               (lambda (_route) '(destination nil nil)))
              ((symbol-function 'remote-client-executable-find)
               (lambda (_name) "/usr/bin/ssh"))
              ((symbol-function 'file-attributes)
               (lambda (&rest _arguments)
                 (ert-fail "Direct PTY connection opened a TRAMP file"))))
      (should (eq (remote-backend-ssh-pty-connect route nil) 'ssh-pty)))))

(ert-deftest remote-tramp-rpc-process-frames-a-process-stderr-destination ()
  "tramp-rpc must not silently merge native `:stderr PROCESS' into stdout."
  (let* ((route
          (remote-route-create
           :target-id "box"
           :pipeline-id "box/ssh"
           :backend-id "tramp-rpc"
           :capability 'process-async
           :adapter-id "exec"))
         (context
          (remote-context-create
           :target-id "box"
           :localname "/tmp/"
           :workspace-root "/fs:box:/tmp/"))
         (process
          (make-pipe-process
           :name "remote-test-rpc-stderr" :noquery t))
         (stderr-process
          (make-pipe-process
           :name "remote-test-rpc-stderr-destination" :noquery t))
         captured)
    (unwind-protect
        (cl-letf
            (((symbol-function 'remote--call-with-process-route)
              (lambda (_adapter _capability _context _constraints function)
                (funcall function route temporary-file-directory nil)))
             ((symbol-function 'remote--prepare-backend-execution)
              (lambda (_route _context command _environment directory
                               &optional logical-directory)
                (remote-backend-execution-create
                 :logical-directory logical-directory
                 :physical-directory directory
                 :command command)))
             ((symbol-function 'executable-find)
              (lambda (_program &optional _remote) "/bin/direnv"))
             ((symbol-function 'make-process)
              (lambda (&rest arguments)
                (setq captured arguments)
                process)))
          (remote-make-process
           :name "remote-test-rpc-stderr"
           :stderr stderr-process
           :command '("direnv" "export" "json")
           :remote-context context
           :remote-stderr-token "rpc-token")
          (should-not (plist-member captured :stderr))
          (should
           (equal (seq-take (plist-get captured :command) 2)
                  '("/bin/sh" "-c")))
          (should (equal (process-get process 'remote-stderr-token)
                         "rpc-token"))
          (should-not (process-get process 'remote-direct-ssh)))
      (dolist (candidate (list process stderr-process))
        (when (process-live-p candidate)
          (delete-process candidate))))))

(ert-deftest remote-direnv-starting-sentinel-coalesces-nested-refresh ()
  (let ((direnv--export-processes (make-hash-table :test #'equal))
        (direnv--export-waiters (make-hash-table :test #'equal))
        (root "/fs:local:/tmp/project/")
        (context
         (remote-context-create
          :target-id "local"
          :localname "/tmp/project/"
          :workspace-root "/fs:local:/tmp/project/"))
        (calls 0))
    (cl-letf (((symbol-function 'remote-exec-async)
               (lambda (_program &rest _options)
                 (cl-incf calls)
                 (direnv--start-export
                  context root '(fingerprint) (current-buffer))
                 'mock-process)))
      (direnv--start-export
       context root '(fingerprint) (current-buffer))
      (should (= calls 1))
      (should (eq (gethash root direnv--export-processes)
                  'mock-process)))))

(ert-deftest remote-direnv-backs-off-unchanged-export-failures ()
  (let* ((direnv--export-failures (make-hash-table :test #'equal))
         (direnv-export-failure-retry-delay 60)
         (root "/fs:local:/tmp/project/")
         (fingerprint '(fingerprint))
         (failure '(error "broken export"))
         (context
          (remote-context-create
           :target-id "local"
           :localname "/tmp/project/"
           :workspace-root root))
         (starts 0))
    (direnv--record-export-failure root fingerprint failure)
    (cl-letf (((symbol-function 'direnv--transport-busy-p) (lambda () nil))
              ((symbol-function 'direnv--envrc-root)
               (lambda (&optional _path) root))
              ((symbol-function 'remote-context)
               (lambda (&optional _path) context))
              ((symbol-function 'direnv--fingerprint)
               (lambda (_context) fingerprint))
              ((symbol-function 'direnv--cached-export)
               (lambda (_root _fingerprint) nil))
              ((symbol-function 'direnv--start-export)
               (lambda (&rest _arguments) (cl-incf starts))))
      (with-temp-buffer
        (should
         (eq (direnv-environment-ensure-async root) 'pending))
        (should (equal direnv--last-error failure))
        (should (zerop starts))))))

(ert-deftest remote-direnv-direct-refresh-honors-export-failure-backoff ()
  "Automatic buffer refreshes must not bypass the export retry guard."
  (let* ((direnv--export-failures (make-hash-table :test #'equal))
         (direnv--export-processes (make-hash-table :test #'equal))
         (direnv--export-waiters (make-hash-table :test #'equal))
         (direnv-export-failure-retry-delay 60)
         (root "/fs:local:/tmp/project/")
         (fingerprint '(fingerprint))
         (failure '(error "broken export"))
         (context
          (remote-context-create
           :target-id "local"
           :localname "/tmp/project/"
           :workspace-root root))
         (starts 0)
         delivered)
    (direnv--record-export-failure root fingerprint failure)
    (cl-letf (((symbol-function 'remote-exec-async)
               (lambda (&rest _arguments)
                 (cl-incf starts)))
              ((symbol-function 'direnv--finish-export-waiters)
               (lambda (finished-root finished-context &optional error)
                 (setq delivered
                       (list finished-root finished-context error)))))
      (with-temp-buffer
        (should-not
         (direnv--start-export
          context root fingerprint (current-buffer)))
        (should (zerop starts))
        (should (equal delivered (list root context failure)))
        (should (equal direnv--last-error failure))))))

(ert-deftest remote-direnv-detects-locked-tramp-connection ()
  (let ((process
         (make-pipe-process
          :name "remote-test-locked-tramp" :noquery t)))
    (unwind-protect
        (progn
          (process-put process 'tramp-vector 'mock-vector)
          (cl-letf (((symbol-function 'tramp-get-connection-property)
                     (lambda (candidate property &optional _default)
                       (and (eq candidate process)
                            (equal property "locked")))))
            (should (direnv--transport-busy-p))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest remote-exec-async-keeps-framework-sentinel-text-out-of-stderr ()
  (remote-test-with-registry
    (let (result)
      (let ((process
             (remote-exec-async
              "sh"
              :args '("-c" "printf output; printf error >&2")
              :context
              (remote-context
               (remote-canonicalize-file-name temporary-file-directory))
              :callback (lambda (value) (setq result value)))))
        (while (process-live-p process)
          (accept-process-output process 0.1))
        (while (null result)
          (accept-process-output nil 0.05)))
      (should (zerop (remote-exec-result-status result)))
      (should (equal (remote-exec-result-stdout result) "output"))
      (should (equal (remote-exec-result-stderr result) "error")))))

(ert-deftest remote-exec-async-cleans-captures-when-stderr-pipe-startup-fails ()
  (remote-test-with-registry
    (let ((before
           (seq-filter
            (lambda (buffer)
              (string-match-p
               "\\` \\*remote-exec-async-\\(?:stdout\\|stderr\\)\\*"
               (buffer-name buffer)))
            (buffer-list))))
      (cl-letf (((symbol-function 'make-pipe-process)
                 (lambda (&rest _arguments)
                   (error "pipe startup failed"))))
        (should-error
         (remote-exec-async
          "sh"
          :args '("-c" "true")
          :context
          (remote-context
           (remote-canonicalize-file-name temporary-file-directory)))))
      (should
       (equal
        before
        (seq-filter
         (lambda (buffer)
           (string-match-p
            "\\` \\*remote-exec-async-\\(?:stdout\\|stderr\\)\\*"
            (buffer-name buffer)))
         (buffer-list)))))))

(ert-deftest remote-exec-async-contains-callback-errors-and-cleans-buffers ()
  (remote-test-with-registry
    (let ((process
           (remote-exec-async
            "sh"
            :args '("-c" "printf output")
            :context
            (remote-context
             (remote-canonicalize-file-name temporary-file-directory))
            :callback (lambda (_result) (error "consumer failed")))))
      (while (process-live-p process)
        (accept-process-output process 0.1))
      (while (not (process-get process 'remote-exec-callback-done))
        (accept-process-output nil 0.05))
      (while (buffer-live-p (process-buffer process))
        (accept-process-output nil 0.05))
      (should-not (buffer-live-p (process-buffer process)))
      (should
       (seq-some
        (lambda (entry)
          (and
           (eq (plist-get entry :kind) 'process-callback-error)
           (string-match-p
            "consumer failed"
            (plist-get entry :error))))
        remote-route-log)))))

(ert-deftest remote-internal-process-buffer-skips-user-kill-hooks ()
  (let ((buffer
         (generate-new-buffer " *remote-internal-process-cleanup*"))
        (hook-ran nil))
    (with-current-buffer buffer
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (setq hook-ran t)
         ;; Reproduce global cleanup integrations which assume a file buffer.
         (file-name-directory buffer-file-name))
       nil t))
    (should (remote--kill-internal-process-buffer buffer))
    (should-not (buffer-live-p buffer))
    (should-not hook-ran)))

(ert-deftest remote-official-make-process-is-decorated-at-fs-boundary ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (buffer (generate-new-buffer " *remote-official-process*"))
           (process
            (make-process
             :name "remote-official-process"
             :buffer buffer
             :command '("sh" "-c" "printf official")
             :file-handler t
             :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (with-current-buffer buffer
              (should (string-prefix-p "official" (buffer-string))))
            (should
             (equal
              (remote-route-link-plugin-id
               (process-get process 'remote-route))
              "native"))
            (should
             (equal
              (remote-backend-execution-backend-id
               (process-get process 'remote-backend-execution))
              "native")))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest remote-process-boundary-reenables-the-physical-tramp-handler ()
  (let ((inhibit-file-name-handlers
         '(tramp-file-name-handler another-handler))
        (inhibit-file-name-operation 'make-process)
        seen)
    (cl-letf
        (((symbol-function 'remote-make-process)
          (lambda (&rest _)
            (setq seen
                  (list inhibit-file-name-operation
                        (copy-sequence inhibit-file-name-handlers)))
            'mock-process)))
      (should
       (eq
        (remote-fs-handle-make-process
         :name "mock" :command '("mock"))
        'mock-process)))
    (should-not (car seen))
    (should-not (memq #'tramp-file-name-handler (cadr seen)))
    (should (memq 'another-handler (cadr seen)))))

(ert-deftest remote-exec-projects-route-into-its-output-buffer ()
  (remote-test-with-registry
    (remote-register-link-plugin
     "mock-process"
     :capabilities '(process-sync)
     :project-file-name
     (lambda (_file _link _route) "/mock-physical/"))
    (remote-register-target "mock" :trusted t)
    (remote-register-link "mock" "only" "mock-process")
    (remote-register-adapter
     "mock-exec" :capabilities '(process-sync)
     :preferences '((default . ("mock-process"))))
    (let ((context
           (remote-context-create
            :target-id "mock"
            :localname "/workspace/a"
            :workspace-root "/fs:mock:/workspace/"))
          seen-directory)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _args)
                   (setq seen-directory default-directory)
                   (insert "mock")
                   0)))
        (let ((result
               (remote-exec
                "demo" :context context :adapter "mock-exec")))
          (should (zerop (remote-exec-result-status result)))
          (should (equal (remote-exec-result-stdout result) "mock"))
          (should (equal seen-directory "/mock-physical/")))))))

(ert-deftest remote-path-probe-discovers-real-target-facts ()
  (remote-test-with-registry
    (let* ((default-directory
            (remote-canonicalize-file-name temporary-file-directory))
           (remote-path-facts-cache (make-hash-table :test #'equal))
           (facts (remote-path-probe nil t)))
      (should (equal (remote-path-facts-target-id facts) "local"))
      (should (stringp (remote-path-facts-system facts)))
      (should (consp (remote-path-facts-path facts)))
      (should
       (equal (car (remote-path-candidates))
              (car (remote-path-facts-path facts)))))))

(ert-deftest remote-path-native-facts-keep-emacs-process-environment ()
  "A native target keeps Emacs' PATH first and adds its shell's directories."
  (remote-test-with-registry
    (let* ((context (remote-context "/fs:local:/tmp/"))
           (remote-path-facts-cache (make-hash-table :test #'equal))
           (route (remote-route-create
                   :target-id "local" :link-plugin-id "native"))
           (output
            (concat remote-path--probe-marker
                    "Darwin\0arm64\0/bin/zsh\0/home/login\0/usr/bin:/bin\0")))
      (cl-letf (((symbol-function 'remote-exec)
                 (lambda (&rest _arguments)
                   (remote-exec-result-create
                    :status 0 :stdout output :route route)))
                ((symbol-function 'remote-client-process-environment)
                 (lambda ()
                   '("PATH=/tmp/venv/bin:/usr/bin"
                     "HOME=/home/client" "SHELL=/bin/fish"))))
        (let ((facts (remote-path--probe-sync context)))
          (should (equal (remote-path-facts-path facts)
                         '("/tmp/venv/bin" "/usr/bin" "/bin")))
          (should (equal (remote-path-facts-home facts) "/home/client"))
          (should (equal (remote-path-facts-shell facts) "/bin/fish")))))))

(ert-deftest remote-path-deferred-facts-stay-within-recovery-attempt ()
  "Background recovery reuses facts without publishing stale global state."
  (remote-test-with-registry
    (let* ((context
            (remote-context
             (remote-canonicalize-file-name temporary-file-directory)))
           (remote-background-defer-commit t)
           (remote-path-facts-cache (make-hash-table :test #'equal))
           (remote-path--deferred-cache (make-hash-table :test #'equal))
           (first (remote-path-probe context))
           (second (remote-path-probe context)))
      (should (eq first second))
      (should-not (gethash "local" remote-path-facts-cache))
      (remote-path-invalidate "local")
      (should-not (gethash "local" remote-path--deferred-cache))
      (should-not (eq first (remote-path-probe context)))
      (should-not (gethash "local" remote-path-facts-cache)))))

(ert-deftest remote-path-deferred-probe-rejects-changed-target-epoch ()
  "A probe completed after invalidation cannot enter recovery's cache."
  (remote-test-with-registry
    (let* ((context
            (remote-context
             (remote-canonicalize-file-name temporary-file-directory)))
           (remote-background-target-epochs (make-hash-table :test #'equal))
           (remote-background--current-job
            (remote-background-job-create
             :target-id "local" :epoch 0))
           (remote-background-defer-commit t)
           (remote-path-facts-cache (make-hash-table :test #'equal))
           (remote-path--deferred-cache (make-hash-table :test #'equal))
           (parse (symbol-function 'remote-path--parse-probe-output)))
      (cl-letf (((symbol-function 'remote-path--parse-probe-output)
                 (lambda (output)
                   (remote-background-invalidate-target "local")
                   (funcall parse output))))
        (should-error (remote-path-probe context)
                      :type 'remote-connection-cancelled))
      (should-not (gethash "local" remote-path-facts-cache))
      (should-not (gethash "local" remote-path--deferred-cache)))))

(ert-deftest remote-path-probe-script-uses-posix-login-shell-path ()
  "PATH comes from the target's login shell, past its startup output."
  (let* ((directory (make-temp-file "remote-login-shell-" t))
         (shell (expand-file-name "zsh" directory))
         (probe
          (lambda (login-shell)
            (with-temp-buffer
              (let ((process-environment
                     (append (list (concat "SHELL=" login-shell)
                                   "PATH=/usr/bin:/bin")
                             process-environment)))
                (call-process "/bin/sh" nil t nil
                              "-c" remote-path--probe-script))
              (nth 4 (remote-path--parse-probe-output
                      (buffer-string)))))))
    (unwind-protect
        (progn
          (with-temp-file shell
            (insert "#!/bin/sh\necho 'login banner'\n"
                    "shift; PATH=/login/bin:$PATH exec /bin/sh -c \"$1\"\n"))
          (set-file-modes shell #o755)
          (should (equal (funcall probe shell) "/login/bin:/usr/bin:/bin"))
          ;; The interactive login PATH (~/.zshrc) wins; a shell that
          ;; refuses interactive mode still yields its login PATH.
          (with-temp-file shell
            (insert "#!/bin/sh\n"
                    "case \"$1\" in -ilc) extra=/rc/bin: ;; *) extra= ;; esac\n"
                    "shift; PATH=${extra}/login/bin:$PATH exec /bin/sh -c \"$1\"\n"))
          (should (equal (funcall probe shell) "/rc/bin:/login/bin:/usr/bin:/bin"))
          (with-temp-file shell
            (insert "#!/bin/sh\n"
                    "[ \"$1\" = -ilc ] && exit 1\n"
                    "shift; PATH=/login/bin:$PATH exec /bin/sh -c \"$1\"\n"))
          (should (equal (funcall probe shell) "/login/bin:/usr/bin:/bin"))
          ;; A failing or non-POSIX login shell keeps the `sh' PATH.
          (should (equal (funcall probe (expand-file-name "fish" directory))
                         "/usr/bin:/bin"))
          (should (equal (funcall probe (expand-file-name "missing/zsh"
                                                         directory))
                         "/usr/bin:/bin")))
      (delete-directory directory t))))

(ert-deftest remote-path-probe-ignores-remote-login-banner ()
  (should
   (equal
    (remote-path--parse-probe-output
     (concat
      "Wi-Fi is currently blocked by rfkill.\n"
      remote-path--probe-marker
      "Linux\0armv7l\0/bin/bash\0/home/hc\0/usr/bin:/bin\0"))
    '("Linux" "armv7l" "/bin/bash" "/home/hc" "/usr/bin:/bin"))))

(ert-deftest remote-environment-is-buffer-local-and-target-native ()
  (remote-test-with-registry
    (remote-register-environment-provider
     "test"
     :priority 10
     :predicate (lambda (_context) t)
     :fingerprint (lambda (_context) 1)
     :load (lambda (_context)
             '(("PATH" . "/target/bin:/usr/bin")
               ("REMOTE_TEST" . "yes"))))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let ((environment (remote-environment-ensure)))
        (should (remote-environment-p environment))
        (should (string-prefix-p "local@" (remote-environment-id environment)))
        (should (equal (getenv "REMOTE_TEST") "yes"))
        (should (equal (car exec-path) "/target/bin"))))))

(ert-deftest remote-environment-nested-envrcs-stay-buffer-local ()
  "Two buffers under one workspace can keep different direnv capsules."
  (remote-test-with-registry
    (let* ((workspace "/fs:local:/tmp/multi-env/")
           (one "/fs:local:/tmp/multi-env/one/")
           (two "/fs:local:/tmp/multi-env/two/")
           (context (remote-context-create
                     :target-id "local" :workspace-id "broad"
                     :workspace-root workspace))
           (first (generate-new-buffer " *remote-env-one*"))
           (second (generate-new-buffer " *remote-env-two*"))
           (direnv-mode t))
      (unwind-protect
          (progn
            (remote-register-environment-provider
             "nested"
             :scope 'workspace
             :priority 10
             :predicate (lambda (_context) t)
             :fingerprint (lambda (_context) 1)
             :load (lambda (seen)
                     (list (cons "REMOTE_BRANCH"
                                 (remote-context-workspace-root seen)))))
            (cl-letf (((symbol-function 'direnv--envrc-root)
                       (lambda (path)
                         (cond
                          ((string-prefix-p one path) one)
                          ((string-prefix-p two path) two)))))
              (with-current-buffer first
                (setq-local buffer-file-name (concat one "a.py")
                            default-directory one)
                (should (equal (remote-environment-workspace-root
                                (remote-environment-resolve context))
                               one))
                (should (equal (cdr (assoc "REMOTE_BRANCH"
                                           (remote--environment-vars context)))
                               one))
                (let ((environment (remote-environment-ensure context)))
                  (should (equal (getenv "REMOTE_BRANCH") one))
                  (should (equal (remote-environment-workspace-root environment)
                                 one))
                  (should-not (remote-environment-workspace-id environment))))
              (with-current-buffer second
                (setq-local buffer-file-name (concat two "b.py")
                            default-directory two)
                (should (equal (remote-environment-workspace-root
                                (remote-environment-resolve context))
                               two))
                (should (equal (cdr (assoc "REMOTE_BRANCH"
                                           (remote--environment-vars context)))
                               two))
                (let ((environment (remote-environment-ensure context)))
                  (should (equal (getenv "REMOTE_BRANCH") two))
                  (should (equal (remote-environment-workspace-root environment)
                                 two))
                  (should-not (remote-environment-workspace-id environment))))
              (with-current-buffer first
                (should (equal (getenv "REMOTE_BRANCH") one))
                (should-not (eq remote-buffer-environment
                                (buffer-local-value 'remote-buffer-environment
                                                    second))))
              (with-current-buffer second
                (setq-local buffer-file-name nil)
                (should (equal (remote-context-workspace-root
                                (direnv--environment-context-for-buffer context))
                               two)))
              (should (equal (remote-context-workspace-root context) workspace))
              (should (equal (remote-context-workspace-id context) "broad"))))
        (kill-buffer first)
        (kill-buffer second)))))

(ert-deftest remote-direnv-export-context-uses-envrc-root ()
  "The async export context must not inherit a broader workspace root."
  (let* ((root "/fs:local:/tmp/multi-env/one/")
         (broad (remote-context-create
                 :target-id "local" :workspace-id "broad"
                 :workspace-root "/fs:local:/tmp/multi-env/")))
    (cl-letf (((symbol-function 'remote-context)
               (lambda (&optional _path) broad)))
      (let ((specific (direnv--context-for-root root)))
        (should (equal (remote-context-workspace-root specific) root))
        (should-not (remote-context-workspace-id specific))
        (should-not (equal (remote-environment-instance-id specific)
                           (remote-environment-instance-id broad)))
        (should (equal (remote-context-workspace-root broad)
                       "/fs:local:/tmp/multi-env/"))))))

(ert-deftest remote-environment-resolve-does-not-mutate-caller-buffer ()
  (remote-test-with-registry
    (remote-register-environment-provider
     "resolve-only"
     :priority 10
     :predicate (lambda (_context) t)
     :fingerprint (lambda (_context) 1)
     :load (lambda (_context)
             '(("REMOTE_RESOLVE_ONLY" . "yes"))))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let ((before (copy-sequence process-environment))
            (environment (remote-environment-resolve)))
        (should
         (equal
          (cdr
           (assoc-string
            "REMOTE_RESOLVE_ONLY"
            (remote-environment-vars environment)
            t))
          "yes"))
        (should (equal process-environment before))
        (should-not (local-variable-p 'process-environment))
        (should-not remote-buffer-environment)))))

(ert-deftest remote-path-layers-inherit-and-decorate-without-mutation ()
  (remote-test-with-registry
    (remote-register-target
     "local"
     :trusted t
     :environment
     '((providers "base" "workspace")
       (path (prepend "/target/bin"))))
    (remote-register-environment-provider
     "base" :scope 'host :priority 0
     :load
     (lambda (_context)
       '(:vars (("BASE" . "yes"))
         :path ("/base/bin" "/usr/bin")
         :path-mode replace)))
    (remote-register-environment-provider
     "workspace" :scope 'workspace :priority 10
     :load
     (lambda (_context)
       '(:vars (("WORKSPACE" . "yes"))
         :path ("/workspace/bin" "/target/bin" "/usr/bin")
         :path-mode replace)))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (let* ((base (remote-environment-ensure))
             (toolchain
              (remote-environment-derive
               base "lean"
               :scope 'toolchain
               :path-prepend '("/lake/bin")))
             (invocation
              (remote-path-decorate
               toolchain "test-run"
               :path-remove '("/usr/bin")
               :path-append '("/test/bin"))))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state base))
          '("/workspace/bin" "/target/bin" "/usr/bin")))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state invocation))
          '("/lake/bin" "/workspace/bin" "/target/bin" "/test/bin")))
        (should
         (equal (remote-environment-parent-id invocation)
                (remote-environment-id toolchain)))
        (should (eq (remote-get-environment
                     (remote-environment-id invocation))
                    invocation))
        (should
         (equal
          (remote-path-state-resolved
           (remote-environment-path-state base))
          '("/workspace/bin" "/target/bin" "/usr/bin")))))))

(ert-deftest remote-provider-path-layers-decorate-lower-layers ()
  "A provider's delta keeps the host PATH it did not start from."
  (remote-test-with-registry
    (remote-register-target
     "local" :trusted t
     :environment '((providers "host" "project")))
    (remote-register-environment-provider
     "host" :scope 'host
     :load (lambda (_context)
             '(:path ("/home/.local/bin" "/usr/bin" "/old/bin")
               :path-mode replace)))
    (remote-register-environment-provider
     "project" :scope 'workspace
     :load (lambda (_context)
             '(:path-layers ((remove "/old/bin")
                             (prepend "/project/bin")
                             (append "/tail/bin")))))
    (with-temp-buffer
      (setq default-directory
            (remote-canonicalize-file-name temporary-file-directory))
      (should
       (equal (remote-path-state-resolved
               (remote-environment-path-state (remote-environment-ensure)))
              '("/project/bin" "/home/.local/bin" "/usr/bin" "/tail/bin"))))))

(ert-deftest remote-direnv-export-is-a-path-delta ()
  "direnv's PATH becomes the change recorded in DIRENV_DIFF, not a whole PATH."
  (let* ((diff
          ;; zlib(JSON) of p.PATH=/client/bin:/usr/bin:/gone/bin and
          ;; n.PATH=/p/bin:/client/bin:/usr/bin:/tail/bin, as direnv encodes it.
          "eJyrVipQslKoVgpwDPEAMpT0k3MyU_NK9JMy86z0S4uLIIz0_LxUEEupVkdBKQ9VQwFECVZ9JYmZORB9tQCaGSEl")
         (export
          (direnv--make-export
           (json-serialize
            `((PATH . "/p/bin:/client/bin:/usr/bin:/tail/bin")
              (DIRENV_DIFF . ,diff) (FOO . "bar")))
           nil "/fs:local:/p/")))
    (should-not (assoc "PATH" (plist-get export :vars)))
    (should (equal (cdr (assoc "FOO" (plist-get export :vars))) "bar"))
    (should (equal (plist-get export :path-layers)
                   '((remove "/gone/bin") (prepend "/p/bin")
                     (append "/tail/bin"))))
    (should-error
     (direnv--make-export (json-serialize '((PATH . "/p/bin"))) nil "/p/"))))

(ert-deftest remote-environment-id-is-workspace-scoped-not-link-scoped ()
  (remote-test-with-registry
    (remote-register-target
     "lab"
     :trusted t
     :workspaces
     '(((id . "one") (path . "/work/one"))
       ((id . "two") (path . "/work/two")))
     :environment '((providers "workspace")))
    (remote-register-environment-provider
     "workspace"
     :scope 'workspace
     :load
     (lambda (context)
       (list
        :vars
        (list
         (cons "WORKSPACE"
               (remote-context-workspace-id context)))
        :path
        (list
         (concat
          (remote-file-local-name
           (remote-context-workspace-root context))
          "bin"))
        :path-mode 'replace)))
    (let* ((one-context (remote-context "/fs:lab:/work/one/a"))
           (two-context (remote-context "/fs:lab:/work/two/b"))
           (one
            (with-temp-buffer
              (remote-environment-ensure one-context)))
           (two
            (with-temp-buffer
              (remote-environment-ensure two-context))))
      (should (equal (remote-environment-id one) "lab@one"))
      (should (equal (remote-environment-id two) "lab@two"))
      (should-not (eq one two))
      (should
       (equal
        (remote-path-state-resolved
         (remote-environment-path-state one))
        '("/work/one/bin")))
      (should
       (equal
        (remote-path-state-resolved
         (remote-environment-path-state two))
        '("/work/two/bin"))))))

(ert-deftest remote-direnv-adapter-prefers-rpc ()
  (let ((preferences
         (remote-adapter-preferences
          (remote-get-adapter "direnv"))))
    (should
     (equal (cdr (assq 'default preferences))
            '("tramp-rpc" "tramp" "native")))))

(ert-deftest remote-direnv-export-waiters-keep-process-boundary-callbacks ()
  (let ((direnv--export-waiters (make-hash-table :test #'equal))
        (root "/fs:local:/tmp/project/")
        (context 'context)
        (environment 'environment)
        (events nil)
        (buffer (generate-new-buffer " *remote-direnv-waiter*")))
    (unwind-protect
        (progn
          ;; Eglot may register first and the automatic file/window refresh may
          ;; enqueue the same buffer immediately afterwards.  The latter must
          ;; not erase the callback which resumes process startup.
          (direnv--queue-export-waiter
           root buffer
           (lambda (result error)
             (push (list 'eglot result error) events)))
          (direnv--queue-export-waiter root buffer nil)
          ;; Independent process boundaries in one buffer must both resume.
          (direnv--queue-export-waiter
           root buffer
           (lambda (result error)
             (push (list 'task result error) events)))
          (cl-letf (((symbol-function 'remote-environment-ensure)
                     (lambda (seen-context &optional _force)
                       (should (eq seen-context context))
                       environment)))
            (direnv--apply-export-waiters root context nil))
          (should
           (equal
            (sort events
                  (lambda (left right)
                    (string< (symbol-name (car left))
                             (symbol-name (car right)))))
            '((eglot environment nil)
              (task environment nil))))
          (should-not (gethash root direnv--export-waiters)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest remote-direnv-process-boundary-completion-does-not-wait-for-idle ()
  "A completed export must resume LSP even while process traffic stays active."
  (let (scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (time repeat function &rest arguments)
                 (setq scheduled
                       (list time repeat function arguments))
                 'wall-clock-timer))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _arguments)
                 (ert-fail "process-boundary completion used an idle timer"))))
      (should
       (eq
        (direnv--finish-export-waiters
         "/fs:box:/work/" 'context)
        'wall-clock-timer)))
    (should
     (equal
      scheduled
      '(0.01 nil direnv--apply-export-waiters
             ("/fs:box:/work/" context nil))))))

(ert-deftest remote-direnv-lifecycle-coalesces-enter-and-reports-leave ()
  (let ((direnv--reported-selection nil)
        (messages nil)
        (root "/fs:local:/tmp/project/")
        (environment
         (remote-environment-create
          :id "local@project"
          :key '(local project)
          :target-id "local"
          :workspace-id "project"
          :workspace-root "/fs:local:/tmp/project/"
          :vars '(("DEMO" . "yes"))
          :sources
          '((host-path "native")
            (direnv "/fs:local:/tmp/project/" "native" "local/native")))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'direnv--selected-buffer-p)
                 (lambda () t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (direnv--announce-environment environment)
        (direnv--announce-environment environment)
        (should (equal direnv--active-root root))
        (should (= (length messages) 1))
        (should (string-match-p "direnv: entered" (car messages)))
        (direnv-clear-environment)
        (should-not direnv--active-root)
        (should-not direnv--reported-selection)
        (should (= (length messages) 2))
        (should (string-match-p
                 "direnv: left"
                 (car messages)))))))

(ert-deftest remote-direnv-refresh-clears-buffer-outside-envrc-tree ()
  (let ((clears 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'direnv--transport-connection-path-p)
                 (lambda (_path) nil))
                ((symbol-function 'direnv--transport-busy-p)
                 (lambda () nil))
                ((symbol-function 'direnv--envrc-root)
                 (lambda (&optional _path) nil))
                ((symbol-function 'direnv-clear-environment)
                 (lambda () (cl-incf clears))))
        (direnv--refresh-buffer (current-buffer))
        (should (= clears 1))))))

(ert-deftest remote-direnv-export-uses-routed-process-api ()
  (let* ((root (make-temp-file "remote-direnv-" t))
         (envrc (expand-file-name ".envrc" root))
         (default-directory
          (remote-canonicalize-file-name
           (file-name-as-directory root)))
         seen-adapter)
    (unwind-protect
        (progn
          (with-temp-file envrc (insert "export DEMO=yes\n"))
          (cl-letf
              (((symbol-function 'remote-executable-find)
                (lambda (_program _context) "/usr/bin/direnv"))
               ((symbol-function 'remote-exec)
                (lambda (_program &rest options)
                  (setq seen-adapter (plist-get options :adapter))
                  (remote-exec-result-create
                   :status 0
                   :stdout
                   (concat "{\"PATH\":\"/remote/bin:/usr/bin\",\"DEMO\":\"yes\","
                           "\"DIRENV_DIFF\":\"eJyrVipQslKoVgpwDPEAMpT0S4uL9JMy85RqdRSU8lClilJz80tSQbJWcGVAVS6uvv4g-crUYqXaWgCiaRdQ\"}")
                   :stderr ""))))
            (let* ((result (direnv--export (remote-context)))
                   (vars (plist-get result :vars)))
              (should (equal seen-adapter "direnv"))
              (should (equal (cdr (assoc "DEMO" vars)) "yes"))
              (should-not (assoc "PATH" vars))
              (should (equal (plist-get result :path-layers)
                             '((prepend "/remote/bin")))))))
      (when (file-exists-p envrc) (delete-file envrc))
      (when (file-directory-p root) (delete-directory root)))))

(ert-deftest remote-fs-substitute-restarts-leave-or-switch-target ()
  "Minibuffer `/~' and `//' restarts follow the client/target boundary.
Tilde is the Emacs client's home; another logical or TRAMP spelling names
that file; a plain absolute path stays on the current target."
  (let ((base "/fs:box:/home/remote/work/"))
    (should (equal (substitute-in-file-name (concat base "~/.emacs.d/"))
                   "~/.emacs.d/"))
    (should (equal (substitute-in-file-name (concat base "~")) "~"))
    (should (equal (substitute-in-file-name (concat base "/etc/hosts"))
                   "/fs:box:/etc/hosts"))
    (should (equal (substitute-in-file-name
                    (concat base "/fs:local:/Users/me/x"))
                   "/fs:local:/Users/me/x"))
    (should (equal (substitute-in-file-name
                    (concat base "/fs:other:/tmp/x"))
                   "/fs:other:/tmp/x"))
    (should (equal (substitute-in-file-name (concat base "/ssh:other:/tmp/"))
                   "/ssh:other:/tmp/"))
    (should (equal (substitute-in-file-name (concat base "src/a.c"))
                   (concat base "src/a.c")))))

(ert-deftest remote-environment-apply-keeps-client-home ()
  "A projected target capsule must not change what `~' means in Emacs.
Routed processes still receive the capsule's target HOME."
  (with-temp-buffer
    (let ((process-environment
           (list "HOME=/Users/client" "PATH=/client/bin"))
          (environment
           (remote-environment-create
            :id "box@test" :target-id "box"
            :vars '(("HOME" . "/home/remote")
                    ("PATH" . "/home/remote/bin:/usr/bin")
                    ("TARGET_ONLY" . "yes")))))
      (remote-environment-apply environment)
      (should (equal (getenv "HOME") "/Users/client"))
      (should (equal (getenv "TARGET_ONLY") "yes"))
      (should (equal (getenv "PATH") "/home/remote/bin:/usr/bin"))
      (should (equal (expand-file-name "~/x") "/Users/client/x"))
      (should (equal (cdr (assoc "HOME" (remote-environment-vars
                                         remote-buffer-environment)))
                     "/home/remote"))
      (should (equal (getenv-internal
                      "HOME"
                      (remote--apply-environment
                       process-environment
                       (remote-environment-vars remote-buffer-environment)))
                     "/home/remote")))))

(provide 'remote-tests)
;;; remote-tests.el ends here
