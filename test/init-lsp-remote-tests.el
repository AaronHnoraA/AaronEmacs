;;; init-lsp-remote-tests.el --- Logical LSP URI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-lsp)
(require 'init-python)
(require 'init-js2)
(require 'init-java)
(require 'eglot)
(require 'lsp-mode)

(defun my/lsp-test-registration (method)
  "Return an lsp-mode registration object for METHOD."
  (if lsp-use-plists
      (list :method method)
    (let ((registration (make-hash-table :test #'equal)))
      (puthash "method" method registration)
      registration)))

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

(ert-deftest eglot-uri-prefers-server-project-over-current-buffer ()
  (let ((default-directory "/fs:local:/tmp/"))
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'server))
              ((symbol-function 'eglot--project)
               (lambda (_server) 'project))
              ((symbol-function 'project-root)
               (lambda (_project) "/fs:box:/work/")))
      (should
       (equal
        (my/eglot--uri-to-logical-a
         (lambda (_uri) "/work/src/Main.java")
         "file:///work/src/Main.java")
        "/fs:box:/work/src/Main.java")))))

(ert-deftest eglot-uri-prefers-dynamically-cached-server ()
  (let ((default-directory "/fs:local:/tmp/")
        (eglot--cached-server 'owning-server))
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'unrelated-server))
              ((symbol-function 'eglot--project)
               (lambda (server)
                 (pcase server
                   ('owning-server 'owning-project)
                   (_ 'unrelated-project))))
              ((symbol-function 'project-root)
               (lambda (project)
                 (pcase project
                   ('owning-project "/fs:box:/work/")
                   (_ "/fs:local:/other/")))))
      (should
       (equal
        (my/eglot--uri-to-logical-a
         (lambda (_uri) "/work/src/Main.java")
         "file:///work/src/Main.java")
        "/fs:box:/work/src/Main.java")))))

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
    (dolist (backend '(eglot lsp-mode))
      (let ((default-directory directory)
            selected)
        (cl-letf
            (((symbol-function 'my/language-server-preferred-backend)
              (lambda () backend))
             ((symbol-function 'my/eglot-ensure)
              (lambda () (setq selected 'eglot)))
             ((symbol-function 'my/lsp-mode-ensure)
              (lambda () (setq selected 'lsp-mode))))
          (my/language-server-ensure))
        (should (eq selected backend))))))

(ert-deftest language-server-direnv-failure-does-not-disable-client ()
  (dolist (case
           '((eglot my/eglot--waiting-for-direnv
                    my/eglot--direnv-ready my/eglot-start-now)
             (lsp-mode my/lsp-mode--waiting-for-direnv
                       my/lsp-mode--direnv-ready my/lsp-mode-start-now)))
    (pcase-let ((`(,backend ,waiting ,callback ,starter) case))
      (let ((started nil))
        (set waiting t)
        (cl-letf
            (((symbol-function 'my/language-server-preferred-backend)
              (lambda () backend))
             ((symbol-function starter)
              (lambda () (setq started t)))
             ((symbol-function 'message) #'ignore))
          (funcall callback nil '(error "broken envrc")))
        (should-not (symbol-value waiting))
        (should started)))))

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
        ;; Shell placement belongs to the selected process backend.  Neither a
        ;; local nor a remote logical root may mutate client shell globals.
        (should (equal shell-file-name "/client/bin/zsh"))
        (should (equal explicit-shell-file-name "/client/bin/zsh"))
        (should (equal shell-command-switch "-c"))))))

(ert-deftest python-eglot-contact-is-selected-by-target-capability-only ()
  (cl-letf
      (((symbol-function 'my/language-server-executable-find)
        (lambda (program)
          (and (equal program "pylsp") "/target/bin/pylsp"))))
    (dolist (directory
             '("/fs:local:/tmp/project/"
               "/fs:box:/work/project/"))
      (let ((default-directory directory))
        (should
         (equal
          (my/python-eglot-contact)
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

(ert-deftest java-buffers-select-only-one-jdtls-workspace-owner ()
  (require 'lsp-java)
  (let* ((clients (make-hash-table :test #'eq))
         (folders (make-hash-table :test #'eq))
         (session
          (make-lsp-session
           :folders nil
           :folders-blocklist nil
           :server-id->folders folders))
         (local-client
          (copy-lsp--client (gethash 'jdtls lsp-clients)))
         (remote-client
          (copy-lsp--client (gethash 'jdtls-tramp lsp-clients)))
         (lsp-clients clients)
         persisted)
    (puthash 'jdtls local-client clients)
    (puthash 'jdtls-tramp remote-client clients)
    (puthash 'jdtls '("/old/local/root") folders)
    (puthash 'jdtls-tramp '("/old/remote/root") folders)
    (with-temp-buffer
      (setq major-mode 'java-mode)
      (let ((lsp-enabled-clients nil))
        (cl-letf
            (((symbol-function 'lsp-session) (lambda () session))
             ((symbol-function 'lsp--persist-session)
              (lambda (_session) (setq persisted t))))
          (my/lsp-java--enforce-single-root))
        (should
         (equal lsp-enabled-clients '(jdtls jdtls-tramp)))))
    (should-not (lsp--client-multi-root local-client))
    (should-not (lsp--client-multi-root remote-client))
    (should-not (gethash 'jdtls folders))
    (should-not (gethash 'jdtls-tramp folders))
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

(ert-deftest eglot-servers-replace-one-keyed-workspace-resource ()
  (let ((remote-workspaces (make-hash-table :test #'equal))
        (remote-workspace--resource-counter 0))
    (cl-letf
        (((symbol-function 'my/language-server--eglot-root)
          (lambda (_server) "/fs:local:/tmp/project/"))
         ((symbol-function 'remote-workspace-track-route)
          (lambda (workspace &rest _arguments) workspace))
         ((symbol-function 'eglot--managed-buffers)
          (lambda (_server) (list (current-buffer))))
         ;; Registration reads the server's project to key the resource by
         ;; runtime; without this stub it bails before creating a workspace
         ;; and the assertions below have nothing to look at.
         ((symbol-function 'eglot--project)
          (lambda (_server) 'test-project)))
      (my/language-server-register-eglot-resource 'server-1)
      (my/language-server-register-eglot-resource 'server-2)
      (should (= (hash-table-count remote-workspaces) 1))
      (let* ((owner (car (hash-table-values remote-workspaces)))
             (resource
              (remote-workspace-find-resource
               owner 'lsp
               '(eglot "/fs:local:/tmp/project/"))))
        (should resource)
        (should (= (length (remote-workspace-resources owner)) 1))
        (should
         (eq (remote-workspace-resource-value resource)
             'server-2))))))

(ert-deftest eglot-target-only-root-declines-directory-watch-explosion ()
  (let ((my/language-server-file-watch-policy 'auto)
        (remote-route-log nil)
        called)
    (cl-letf
        (((symbol-function 'my/language-server--eglot-root)
          (lambda (_server) "/fs:box:/work/project/"))
         ((symbol-function 'my/language-server--resource-owner)
          (lambda (_root) 'workspace))
         ((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil)))
      (should-not
       (my/eglot-register-capability-via-remote-a
        (lambda (&rest _)
          (setq called t))
        'server 'workspace/didChangeWatchedFiles "watch-1"
        :watchers [(:globPattern "**/*.lean")]))
      (should-not called)
      (let ((event (car remote-route-log)))
        (should
         (eq (plist-get event :kind)
             'lsp-watch-registration-declined))
        (should
         (equal (plist-get event :root)
                "/fs:box:/work/project/"))
        (should (= (plist-get event :watcher-count) 1))))))

(ert-deftest eglot-client-accessible-root-keeps-native-watch-behavior ()
  (let ((my/language-server-file-watch-policy 'auto)
        captured)
    (cl-letf
        (((symbol-function 'my/language-server--eglot-root)
          (lambda (_server) "/fs:mounted:/work/project/"))
         ((symbol-function 'my/language-server--resource-owner)
          (lambda (_root) 'workspace))
         ((symbol-function 'remote-client-file-name)
          (lambda (_root &optional _adapter)
            "/Volumes/work/project/")))
      (should
       (eq
        (my/eglot-register-capability-via-remote-a
         (lambda (server method id &rest params)
           (setq captured
                 (list
                  server method id params
                  remote-current-adapter-id
                  remote-current-workspace))
           'registered)
         'server 'workspace/didChangeWatchedFiles "watch-1"
         :watchers [(:globPattern "**/*.lean")])
        'registered))
      (should
       (equal
        captured
        '(server workspace/didChangeWatchedFiles "watch-1"
                 (:watchers [(:globPattern "**/*.lean")])
                 "language-server" workspace))))))

(ert-deftest lsp-mode-target-only-root-retains-registration-without-watchers ()
  "Target-only roots must not create recursive client-side file watches."
  (let ((my/language-server-file-watch-policy 'auto)
        (lsp-enable-file-watchers t)
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/project/"))
        watcher-value
        called)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil)))
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
    (should-not watcher-value)))

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

(ert-deftest eglot-non-watch-callback-inherits-language-server-owner ()
  (let (captured)
    (cl-letf
        (((symbol-function 'my/language-server--eglot-root)
          (lambda (_server) "/fs:box:/work/project/"))
         ((symbol-function 'my/language-server--resource-owner)
          (lambda (_root) 'workspace)))
      (should
       (eq
        (my/eglot-register-capability-via-remote-a
         (lambda (&rest _)
           (setq captured
                 (list
                  remote-current-adapter-id
                  remote-current-workspace))
           'registered)
         'server 'workspace/symbol "symbol-1")
        'registered))
      (should
       (equal captured '("language-server" workspace))))))

(provide 'init-lsp-remote-tests)
;;; init-lsp-remote-tests.el ends here
