;;; init-lsp-remote-tests.el --- Logical LSP URI and remote-parity tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-lsp)
(require 'init-python)
(require 'init-js2)
(require 'init-java)
(require 'lsp-mode)

(defun my/lsp-test-registration (method)
  "Return an lsp-mode registration object for METHOD."
  (if lsp-use-plists
      (list :method method)
    (let ((registration (make-hash-table :test #'equal)))
      (puthash "method" method registration)
      registration)))

(defun my/lsp-test-object (&rest pairs)
  "Return an LSP hash object initialized from keyword/value PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (while pairs
      (puthash (substring (symbol-name (pop pairs)) 1) (pop pairs) object))
    object))

(ert-deftest lsp-mode-local-logical-path-does-not-leak-fs-syntax ()
  (let ((default-directory "/fs:local:/tmp/"))
    (should
     (equal
      (lsp--path-to-uri "/fs:local:/tmp/A.java")
      "file:///tmp/A.java"))
    (should
     (equal
      (lsp--uri-to-path "file:///tmp/A.java")
      "/fs:local:/tmp/A.java"))))

(ert-deftest lsp-mode-remote-uri-returns-to-current-logical-target ()
  (let ((default-directory "/fs:box:/work/"))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-uri-prefers-workspace-target-over-current-buffer ()
  "Async callbacks must not borrow an unrelated buffer's target."
  (let ((default-directory "/fs:local:/tmp/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-local-workspace-uses-the-same-uri-projection ()
  (let ((default-directory "/fs:box:/work/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/tmp/project/Main.java")
       "file:///tmp/project/Main.java")
      "/fs:local:/tmp/project/Main.java"))))

(ert-deftest language-server-executable-lookup-uses-shared-adapter ()
  (let (adapter)
    (cl-letf (((symbol-function 'remote-executable-find)
               (lambda (_program &optional _context)
                 (setq adapter remote-current-adapter-id)
                 "/target/bin/server")))
      (should
       (equal
        (my/language-server-executable-find "server")
        "/target/bin/server")))
    (should (equal adapter "language-server"))))

(ert-deftest language-server-auto-selector-is-target-neutral ()
  (dolist (directory
           '("/fs:local:/tmp/project/"
             "/fs:box:/work/project/"))
    (let ((default-directory directory)
          selected)
      (cl-letf
          (((symbol-function 'my/language-server-preferred-backend)
            (lambda () 'lsp-mode))
           ((symbol-function 'my/lsp-mode-ensure)
            (lambda () (setq selected 'lsp-mode))))
        (my/language-server--ensure-after-runtime))
      (should (eq selected 'lsp-mode)))))

(ert-deftest language-server-direnv-failure-does-not-disable-client ()
  (let ((started nil))
    (setq my/lsp-mode--waiting-for-direnv t)
    (cl-letf
        (((symbol-function 'my/language-server-preferred-backend)
          (lambda () 'lsp-mode))
         ((symbol-function 'my/lsp-mode-start-now)
          (lambda () (setq started t)))
         ((symbol-function 'message) #'ignore))
      (my/lsp-mode--direnv-ready nil '(error "broken envrc")))
    (should-not my/lsp-mode--waiting-for-direnv)
    (should started)))

(ert-deftest language-server-network-contact-routes-on-local-and-remote ()
  (dolist (directory
           '("/fs:local:/tmp/project/"
             "/fs:box:/work/project/"))
    (let ((default-directory directory)
          (remote-current-adapter-id "language-server")
          captured)
      (cl-letf
          (((symbol-function 'remote-open-network-stream)
            (lambda (name buffer host service &rest parameters)
              (setq captured
                    (list name buffer host service parameters))
              'routed-process)))
        (should
         (eq
          (my/language-server--open-network-stream-a
           (lambda (&rest _) (error "native path was used"))
           "server" nil "127.0.0.1" 2087 :type 'plain)
          'routed-process)))
      (should
       (equal
        (remote-context-target-id
         (plist-get (nth 4 captured) :remote-context))
        (remote-file-name-target directory)))
      (should
       (equal
        (plist-get (nth 4 captured) :remote-adapter)
        "language-server")))))

(ert-deftest language-server-shell-boundary-is-target-neutral ()
  ;; `my/language-server-apply-lsp-local-settings' also resolves any
  ;; project-local `:lsp-workspace' override; stub that lookup rather than
  ;; exercising real project/file-truename plumbing for a target this test
  ;; never links a route for.
  (cl-letf (((symbol-function 'my/language-server-project-workspace-configuration)
             (lambda () nil)))
    (dolist (directory
             '("/fs:local:/tmp/project/"
               "/fs:box:/work/project/"))
      (with-temp-buffer
        (let ((default-directory directory)
              (shell-file-name "/client/bin/zsh")
              (explicit-shell-file-name "/client/bin/zsh")
              (shell-command-switch "-c")
              (my/language-server-lsp-local-settings-hook nil))
          (my/language-server-apply-lsp-local-settings)
          ;; Shell placement belongs to the selected process backend.  Neither
          ;; a local nor a remote logical root may mutate client shell
          ;; globals.
          (should (equal shell-file-name "/client/bin/zsh"))
          (should (equal explicit-shell-file-name "/client/bin/zsh"))
          (should (equal shell-command-switch "-c")))))))

(ert-deftest python-language-server-command-is-selected-by-target-capability-only ()
  (cl-letf
      (((symbol-function 'my/language-server-executable-find)
        (lambda (program)
          (and (equal program "pylsp") "/target/bin/pylsp")))
       ((symbol-function 'my/lsp-python--provisioned-command)
        (lambda () nil)))
    (dolist (directory
             '("/fs:local:/tmp/project/"
               "/fs:box:/work/project/"))
      (let ((default-directory directory))
        (should
         (equal
          (my/python-language-server-command)
          '("/target/bin/pylsp")))))))

(ert-deftest js-project-bin-uses-the-environment-capsule-for-every-target ()
  (let (prepends)
    (cl-letf
        (((symbol-function 'locate-dominating-file)
          (lambda (directory _name) directory))
         ((symbol-function 'file-directory-p) (lambda (_file) t))
         ((symbol-function 'remote-environment-ensure)
          (lambda (&rest _arguments) 'base))
         ((symbol-function 'remote-environment-derive)
          (lambda (_environment _id &rest properties)
            (push (plist-get properties :path-prepend) prepends)
            'derived))
         ((symbol-function 'remote-environment-apply)
          (lambda (environment &optional _buffer) environment)))
      (dolist (directory
               '("/fs:local:/tmp/project/"
                 "/fs:box:/tmp/project/"))
        (with-temp-buffer
          (setq default-directory directory)
          (my/js-setup-project-node-bin))))
    (should
     (equal
      prepends
      '(("/tmp/project/node_modules/.bin")
        ("/tmp/project/node_modules/.bin"))))))

(ert-deftest java-command-and-debug-channel-use-one-target-projection ()
  (should
   (equal
    (my/lsp-java--target-command-a
     (lambda ()
       '("/fs:box:/opt/jdtls/bin/jdtls"
         "-data" "/fs:box:/work/.cache/")))
    '("/opt/jdtls/bin/jdtls" "-data" "/work/.cache/")))
  (let ((my/java-debug--forwards (make-hash-table :test #'equal))
        calls)
    (cl-letf
        (((symbol-function 'remote-port-forward)
          (lambda (remote-endpoint &rest arguments)
            (push (list remote-endpoint arguments) calls)
            (remote-forward-create
             :handle nil
             :state 'open
             :remote-endpoint remote-endpoint
             :local-endpoint '(:host "127.0.0.1" :port 41000)))))
      (dolist (root
               '("/fs:local:/tmp/project/"
                 "/fs:box:/work/project/"))
        (should (= (my/java-debug--access-port root 5005) 41000))))
    (should (= (length calls) 2))))

(ert-deftest java-native-command-adds-per-root-workspace-and-config ()
  "In native-launcher mode, `-data'/`-configuration' must be appended.
`bin/jdtls' derives its own workspace directory from
`sha1(basename(cwd))' when `-data' is absent, so two Java projects that
merely share a directory basename would otherwise collide on one shared,
possibly half-imported JDTLS workspace."
  (require 'lsp-java)
  (let ((lsp-java-jdt-ls-prefer-native-command t)
        (lsp-java-workspace-dir "/fs:box:/work/.cache/jdtls-workspace/")
        (lsp-java-server-config-dir "/fs:box:/opt/jdtls/config_linux/"))
    (should
     (equal
      (my/lsp-java--target-command-a
       (lambda ()
         '("/fs:box:/opt/jdtls/bin/jdtls" "--jvm-arg=-Dlog.level=ALL")))
      '("/opt/jdtls/bin/jdtls" "--jvm-arg=-Dlog.level=ALL"
        "-data" "/work/.cache/jdtls-workspace/"
        "-configuration" "/opt/jdtls/config_linux/")))))

(ert-deftest java-non-native-command-does-not-add-data-flags ()
  "Jar-mode commands already carry their own `-data'/`-configuration';
this advice must not duplicate them when native mode is off."
  (require 'lsp-java)
  (let ((lsp-java-jdt-ls-prefer-native-command nil)
        (lsp-java-workspace-dir "/fs:box:/work/.cache/jdtls-workspace/"))
    (should
     (equal
      (my/lsp-java--target-command-a
       (lambda ()
         '("/fs:box:/opt/jdtls/bin/jdtls" "-data" "/fs:box:/work/.cache/")))
      '("/opt/jdtls/bin/jdtls" "-data" "/work/.cache/")))))

(ert-deftest java-buffers-select-only-one-jdtls-workspace-owner ()
  "One JDTLS client id serves every target now that
`lsp-auto-register-remote-clients' is nil, so a single root/folder wipe
must cover it without a `-tramp' counterpart."
  (require 'lsp-java)
  (let* ((clients (make-hash-table :test #'eq))
         (folders (make-hash-table :test #'eq))
         (session
          (make-lsp-session
           :folders nil
           :folders-blocklist nil
           :server-id->folders folders))
         (client (copy-lsp--client (gethash 'jdtls lsp-clients)))
         (lsp-clients clients)
         persisted)
    (puthash 'jdtls client clients)
    (puthash 'jdtls '("/old/root") folders)
    (with-temp-buffer
      (setq major-mode 'java-mode)
      (let ((lsp-enabled-clients nil))
        (cl-letf
            (((symbol-function 'lsp-session) (lambda () session))
             ((symbol-function 'lsp--persist-session)
              (lambda (_session) (setq persisted t))))
          (my/lsp-java--enforce-single-root))
        (should (equal lsp-enabled-clients '(jdtls)))))
    (should-not (lsp--client-multi-root client))
    (should-not (gethash 'jdtls folders))
    (should persisted)))

(ert-deftest lsp-workspace-is-one-recoverable-remote-workspace-resource ()
  (let ((remote-workspaces (make-hash-table :test #'equal))
        (remote-workspace--resource-counter 0)
        (lsp-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/"))
        restarted)
    (cl-letf
        (((symbol-function 'remote-workspace-track-route)
          (lambda (workspace &rest _arguments) workspace))
         ((symbol-function 'my/language-server--lsp-workspace-id)
          (lambda (_workspace) 'test-server))
         ((symbol-function 'lsp-workspace-restart)
          (lambda (workspace) (setq restarted workspace))))
      (let ((lsp--cur-workspace lsp-workspace))
        (my/language-server-register-lsp-resource)
        (my/language-server-register-lsp-resource))
      (let* ((owner (car (hash-table-values remote-workspaces)))
             (resource
              (remote-workspace-find-resource
               owner 'lsp
               '(lsp-mode test-server "/fs:local:/tmp/project/"))))
        (should resource)
        (should (= (length (remote-workspace-resources owner)) 1))
        (remote-workspace-recover-resource owner resource)
        (should (eq restarted lsp-workspace))
        (should
         (eq (remote-workspace-resource-value resource)
             lsp-workspace))))))

(ert-deftest lsp-runtime-workspaces-own-distinct-remote-resources ()
  "Kernel identity separates handles without changing their Remote owner."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (remote-workspace--resource-counter 0)
         (root "/fs:local:/tmp/project/")
         (first (make-lsp--workspace :root root))
         (second (make-lsp--workspace :root root)))
    (puthash my/language-server-runtime--workspace-metadata-key "kernel-one"
             (lsp--workspace-metadata first))
    (puthash my/language-server-runtime--workspace-metadata-key "kernel-two"
             (lsp--workspace-metadata second))
    (cl-letf
        (((symbol-function 'remote-workspace-track-route)
          (lambda (workspace &rest _arguments) workspace))
         ((symbol-function 'my/language-server--lsp-workspace-id)
          (lambda (_workspace) 'my-python)))
      (let ((lsp--cur-workspace first))
        (my/language-server-register-lsp-resource))
      (let ((lsp--cur-workspace second))
        (my/language-server-register-lsp-resource)))
    (let ((owner (car (hash-table-values remote-workspaces))))
      (should owner)
      (should (= (length (remote-workspace-resources owner)) 2))
      (should
       (remote-workspace-find-resource
        owner 'lsp
        '(lsp-mode my-python "/fs:local:/tmp/project/" "kernel-one")))
      (should
       (remote-workspace-find-resource
        owner 'lsp
        '(lsp-mode my-python "/fs:local:/tmp/project/" "kernel-two"))))))

(ert-deftest lsp-mode-target-only-root-uses-remote-watch-capability ()
  "Target-only roots keep watcher parity when Remote can own the watches."
  (let ((my/language-server-file-watch-policy 'auto)
        (lsp-enable-file-watchers t)
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/project/"))
        watcher-value
        called)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil))
         ((symbol-function 'remote-routes)
          (lambda (&rest _)
            '(remote-watch-route))))
      (should
       (eq
        (my/lsp-mode--register-capability-via-remote-a
         (lambda (_registration)
           (setq called t
                 watcher-value lsp-enable-file-watchers)
           'registered)
         (my/lsp-test-registration
          "workspace/didChangeWatchedFiles"))
        'registered)))
    (should called)
    (should watcher-value)))

(ert-deftest lsp-mode-target-only-root-declines-watch-without-route ()
  (let ((my/language-server-file-watch-policy 'auto))
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil))
         ((symbol-function 'remote-routes)
          (lambda (&rest _) nil)))
      (should
       (my/language-server--skip-file-watch-p
        "/fs:box:/work/project/")))))

(ert-deftest lsp-mode-client-accessible-root-keeps-native-watchers ()
  (let ((my/language-server-file-watch-policy 'auto)
        (lsp-enable-file-watchers t)
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/"))
        watcher-value)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) "/tmp/project/")))
      (my/lsp-mode--register-capability-via-remote-a
       (lambda (_registration)
         (setq watcher-value lsp-enable-file-watchers))
       (my/lsp-test-registration
        "workspace/didChangeWatchedFiles")))
    (should watcher-value)))

(ert-deftest lsp-mode-target-watch-root-is-logical-and-workspace-owned ()
  (let (seen-directory seen-owner seen-adapter seen-metadata)
    (cl-letf
        (((symbol-function 'my/language-server--canonical-root)
          (lambda (_directory) "/fs:box:/work/project/"))
         ((symbol-function 'remote-file-name-target)
          (lambda (_directory) "box"))
         ((symbol-function 'my/language-server--connect-workspace)
          (lambda (_root) 'owner)))
      (should
       (eq
        (my/lsp-mode--watch-root-via-remote-a
         (lambda (directory &rest _arguments)
           (setq seen-directory directory
                 seen-owner remote-file-watch-workspace
                 seen-adapter remote-current-adapter-id
                 seen-metadata remote-file-watch-metadata)
           'watch)
         "/ssh:box:/work/project/" #'ignore nil nil)
        'watch)))
    (should (equal seen-directory "/fs:box:/work/project/"))
    (should (eq seen-owner 'owner))
    (should (equal seen-adapter "language-server"))
    (should (eq (plist-get seen-metadata :owner) 'lsp-mode))))

(ert-deftest lsp-mode-bounded-shutdown-is-idempotent-and-keeps-source-buffer ()
  "Stopping a language server must never kill a user source buffer."
  (let* ((source (generate-new-buffer " *remote-java-source*"))
         (workspace
          (make-lsp--workspace
           :root "/fs:box:/work/project/"
           :buffers (list source)
           :shutdown-action nil))
         (shutdown-count 0)
         (killed-processes nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'lsp-workspace-shutdown)
              (lambda (_workspace)
                (cl-incf shutdown-count)
                ;; This is the strongest source-side operation performed by
                ;; upstream shutdown: remove managed-mode state in-place.
                (with-current-buffer source
                  (setq-local lsp-managed-mode nil))))
             ((symbol-function 'lsp-process-kill)
              (lambda (process)
                (push process killed-processes))))
          (should
           (my/lsp-mode-shutdown-workspace workspace 'test-shutdown))
          (should (buffer-live-p source))
          (should
           (my/lsp-mode-shutdown-workspace workspace 'repeated-shutdown))
          (should (buffer-live-p source))
          (should (= shutdown-count 1))
          (should-not killed-processes))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest lsp-mode-remote-shutdown-allows-route-latency ()
  (let ((lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/project/"))
        (lsp-response-timeout 0.5)
        (my/lsp-mode-remote-shutdown-response-timeout 2)
        seen)
    (my/lsp-mode--remote-shutdown-timeout-a
     (lambda (_method _params &rest _keys)
       (setq seen lsp-response-timeout))
     "shutdown" nil)
    (should (= seen 2))))

;;; ── New in the single-backend migration ─────────────────────────────────────

(ert-deftest language-server-booster-wraps-resolved-command-per-target ()
  "The booster path must resolve on the same target that runs the server,
never the client, and must fail loudly rather than silently skip when
required and missing."
  (dolist (case '(("/fs:local:/tmp/project/" . "/local/bin/emacs-lsp-booster")
                  ("/fs:box:/work/project/" . "/box/bin/emacs-lsp-booster")))
    (let ((default-directory (car case))
          (my/language-server-booster-required t)
          seen-context)
      (cl-letf
          (((symbol-function 'lsp-resolve-final-command)
            (lambda (command _test) command))
           ((symbol-function 'remote-fs-file-name-p) (lambda (_path) t))
           ((symbol-function 'my/language-server-executable-find)
            (lambda (program)
              (setq seen-context default-directory)
              (and (equal program "emacs-lsp-booster") (cdr case)))))
        (should
         (equal
          (my/lsp-mode--resolve-logical-command-a
           #'lsp-resolve-final-command
           '("clangd" "--foo"))
          (list (cdr case) "--json-false-value" ":json-false" "--"
                "clangd" "--foo"))))
      (should (equal seen-context default-directory)))))

(ert-deftest language-server-booster-errors-when-required-and-missing ()
  (let ((my/language-server-booster-required t))
    (cl-letf
        (((symbol-function 'lsp-resolve-final-command)
          (lambda (command _test) command))
         ((symbol-function 'my/language-server-executable-find)
          (lambda (_program) nil)))
      (should-error
       (my/lsp-mode--resolve-logical-command-a
        #'lsp-resolve-final-command '("clangd"))))))

(ert-deftest language-server-booster-optional-falls-back-silently ()
  (let ((my/language-server-booster-required nil))
    (cl-letf
        (((symbol-function 'lsp-resolve-final-command)
          (lambda (command _test) command))
         ((symbol-function 'my/language-server-executable-find)
          (lambda (_program) nil)))
      (should
       (equal
        (my/lsp-mode--resolve-logical-command-a
         #'lsp-resolve-final-command '("clangd"))
        '("clangd"))))))

(ert-deftest language-server-executable-find-advice-is-scoped-to-adapter ()
  "Stock `lsp-clients-*' definitions call bare `executable-find'.  Inside
the language-server adapter's dynamic extent that must resolve through the
target; outside it, ordinary client-side lookups must be untouched."
  (cl-letf
      (((symbol-function 'remote-executable-find)
        (lambda (program &optional _context)
          (and (equal program "clangd") "/target/bin/clangd"))))
    (let ((remote-current-adapter-id "language-server"))
      (should
       (equal
        (my/language-server--executable-find-a
         (lambda (_program &optional _remote) nil)
         "clangd" nil)
        "/target/bin/clangd")))
    (let ((remote-current-adapter-id "exec"))
      (should-not
       (my/language-server--executable-find-a
        (lambda (_program &optional _remote) nil)
        "clangd" nil)))))

(ert-deftest language-server-executable-find-guard-prevents-remote-recursion ()
  "A local Remote provider may itself use `executable-find'."
  (let ((remote-current-adapter-id "language-server")
        (native-calls 0))
    (cl-labels ((native-find (_command &optional _remote)
                  (cl-incf native-calls)
                  "/native/bin/clangd"))
      (cl-letf (((symbol-function 'remote-executable-find)
                 (lambda (command &optional _context)
                   (my/language-server--executable-find-a
                    #'native-find command))))
        (should
         (equal
          (my/language-server--executable-find-a
           #'native-find "clangd")
          "/native/bin/clangd"))))
    (should (= native-calls 1))))

(ert-deftest language-server-deep-configuration-merge-preserves-explicit-false ()
  (let* ((analysis
          (my/lsp-test-object
           :autoSearchPaths t
           :diagnosticMode "workspace"
           :nested (my/lsp-test-object :enabled t :level "strict")))
         (base (my/lsp-test-object :analysis analysis))
         (override
          '(:analysis
            (:diagnosticMode nil
             :nested (:enabled :json-false))))
         (merged (my/language-server--merge-values base override))
         (merged-analysis
          (my/language-server--mapping-ref merged :analysis 'missing))
         (nested
          (my/language-server--mapping-ref merged-analysis :nested 'missing)))
    (should (eq (my/language-server--mapping-ref
                 merged-analysis :autoSearchPaths 'missing)
                t))
    (should-not (my/language-server--mapping-ref
                 merged-analysis :diagnosticMode 'missing))
    (should (eq (my/language-server--mapping-ref nested :enabled 'missing)
                :json-false))
    (should (equal (my/language-server--mapping-ref nested :level 'missing)
                   "strict"))))

(ert-deftest language-server-workspace-configuration-is-section-and-owner-scoped ()
  (let* ((params
          (my/lsp-test-object
           :items
           (vector
            (my/lsp-test-object :section "python.analysis")
            (my/lsp-test-object :section "pylsp.plugins"))))
         (first
          (my/lsp-test-object
           :autoSearchPaths t :diagnosticMode "workspace"))
         (second '(:jedi (:enabled t :fuzzy nil)))
         (response (vector first second))
         (override
          '(:python (:analysis (:diagnosticMode nil))
            :pylsp (:plugins (:jedi (:enabled :json-false))))))
    (let ((lsp--cur-workspace 'owning-workspace))
      (cl-letf
          (((symbol-function
             'my/language-server--workspace-configuration-override)
            (lambda (workspace)
              (should (eq workspace 'owning-workspace))
              override)))
        (let* ((merged
                (my/language-server--workspace-configuration-response-a
                 (lambda (_params) response) params))
               (analysis (aref merged 0))
               (plugins (aref merged 1))
               (jedi (my/language-server--mapping-ref
                      plugins :jedi 'missing)))
          (should (eq (my/language-server--mapping-ref
                       analysis :autoSearchPaths 'missing)
                      t))
          (should-not (my/language-server--mapping-ref
                       analysis :diagnosticMode 'missing))
          (should (eq (my/language-server--mapping-ref
                       jedi :enabled 'missing)
                      :json-false))
          (should-not (my/language-server--mapping-ref
                       jedi :fuzzy 'missing)))))))

(ert-deftest language-server-custom-activation-cannot-leak-across-modes ()
  (let (client)
    (cl-letf (((symbol-function 'lsp-stdio-connection) #'identity)
              ((symbol-function 'make-lsp-client) (lambda (&rest args) args))
              ((symbol-function 'lsp-register-client)
               (lambda (value) (setq client value))))
      (my/register-language-server
       '(latex-mode) '("texlab")
       :server-id 'test-latex
       :activation-fn (lambda (&rest _) t)))
    (let ((activation (plist-get client :activation-fn)))
      (with-temp-buffer
        (setq major-mode 'c-mode)
        (should-not (funcall activation "file.c" 'c-mode)))
      (with-temp-buffer
        (setq major-mode 'latex-mode)
        (should (funcall activation "file.tex" 'latex-mode))))))

(ert-deftest language-server-logical-support-check-restores-client-slot ()
  (require 'lsp-java)
  (dolist (server-id '(my-clangd my-python jdtls))
    (let* ((client (gethash server-id lsp-clients))
           (original (lsp--client-remote? client)))
      (should client)
      (with-temp-buffer
        (setq buffer-file-name "/fs:box:/work/project/source")
        (should
         (my/lsp-mode--supports-logical-buffer-a
          (lambda (candidate)
            (lsp--client-remote? candidate))
          client)))
      (with-temp-buffer
        (setq buffer-file-name "/ssh:box:/work/project/source")
        (should
         (my/lsp-mode--supports-logical-buffer-a
          (lambda (candidate)
            (lsp--client-remote? candidate))
          client)))
      (should (eq (lsp--client-remote? client) original)))))

(ert-deftest language-server-stdio-connect-projects-tramp-root-through-remote ()
  (let* ((workspace
          (make-lsp--workspace :root "/ssh:box:/work/project/"))
         (owner
          (remote-workspace-create
           :id "box/work" :target-id "box"
           :root "/fs:box:/work/project/" :context 'remote-context))
         (connection
          (my/lsp-mode--stdio-connect-via-remote-a
           (list
            :connect
            (lambda (_filter _sentinel _name _environment-fn _workspace)
              (list
               default-directory
               lsp-use-workspace-root-for-server-default-directory
               remote-current-adapter-id
               remote-current-workspace)))))
         applied-context)
    (cl-letf (((symbol-function 'my/language-server--connect-workspace)
               (lambda (root)
                 (should (equal root "/fs:box:/work/project/"))
                 owner))
              ((symbol-function 'remote-environment-ensure)
               (lambda (context &rest _)
                 (setq applied-context context))))
      (should
       (equal
        (funcall (plist-get connection :connect)
                 #'ignore #'ignore "clangd" nil workspace)
        (list "/fs:box:/work/project/" nil "language-server" owner))))
    (should (eq applied-context 'remote-context))))

(ert-deftest language-server-core-languages-select-one-authoritative-client ()
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (my/cpp-language-server-setup-h)
    (should (equal lsp-enabled-clients '(my-clangd))))
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (cl-letf (((symbol-function
                'my/language-server-set-workspace-configuration)
               #'ignore))
      (my/python-language-server-setup-h))
    (should (equal lsp-enabled-clients '(my-python))))
  (should-not lsp-auto-register-remote-clients)
  (should (memq #'my/language-server-ensure-deferred prog-mode-hook)))

(ert-deftest language-server-missing-binary-is-quiet-only-for-auto-start ()
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (let (messages started)
      (cl-letf (((symbol-function 'my/lsp-mode-supported-p) (lambda () t))
                ((symbol-function 'my/language-server-apply-process-environment)
                 #'ignore)
                ((symbol-function 'my/language-server-apply-lsp-local-settings)
                 #'ignore)
                ((symbol-function
                  'my/language-server-runtime-register-lsp-configuration)
                 #'ignore)
                ((symbol-function 'my/language-server-contact-available-p)
                 (lambda () nil))
                ((symbol-function 'lsp-deferred)
                 (lambda () (setq started t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format format-string arguments) messages))))
        (setq my/language-server--manual-start nil)
        (my/lsp-mode-start-now)
        (should-not messages)
        (setq my/language-server--manual-start t)
        (my/lsp-mode-start-now)
        (should (= (length messages) 1))
        (should-not started)))))

(provide 'init-lsp-remote-tests)
;;; init-lsp-remote-tests.el ends here
