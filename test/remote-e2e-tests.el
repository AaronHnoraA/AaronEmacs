;;; remote-e2e-tests.el --- Opt-in real SSH remote tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run only through `make remote-e2e'.  The suite imports the normal SSH
;; configuration, chooses an Aaron-* target unless REMOTE_E2E_TARGET is set,
;; and confines all target mutations to a fresh /tmp directory.

;;; Code:

(require 'ert)
(require 'seq)
(require 'remote-framework)
(require 'remote-config)
(require 'remote-board)
(require 'remote-transport)
(require 'remote-search)

(defun remote-e2e--enabled-p ()
  "Return non-nil when destructive temporary E2E checks are opted in."
  (equal (getenv "REMOTE_E2E") "1"))

(defun remote-e2e--ssh-config-args (target)
  "Return client SSH config arguments for TARGET's imported pipeline."
  (when-let* ((pipeline
               (car (remote-pipelines-for-target (remote-target-id target))))
              (file (remote-transport-ssh-config-file pipeline)))
    (list "-F" file)))

(defun remote-e2e--target ()
  "Return the explicitly selected or first reachable Aaron-* target."
  (remote-config-load)
  (let* ((requested (getenv "REMOTE_E2E_TARGET"))
         (candidates
          (sort
           (seq-filter
            (lambda (target)
              (and
               (not (equal (remote-target-id target) "local"))
               (string-match-p
                "\\`Aaron-"
                (or (remote-target-label target) ""))
               (remote-pipelines-for-target
                (remote-target-id target))))
            (hash-table-values remote-targets))
           (lambda (left right)
             (string-lessp
              (remote-target-label left)
              (remote-target-label right)))))
         (selected
          (if requested
              (or
               (remote-get-target requested)
               (seq-find
                (lambda (target)
                  (equal (remote-target-label target) requested))
                candidates))
            (seq-find
             (lambda (target)
               (let ((default-directory temporary-file-directory))
                 (zerop
                  (apply
                   #'call-process
                   "ssh" nil nil nil
                   (append
                    (list "-T" "-o" "BatchMode=yes"
                          "-o" "ConnectTimeout=2"
                          "-o" "ConnectionAttempts=1")
                    (remote-e2e--ssh-config-args target)
                    (list (remote-target-label target) "true"))))))
             candidates))))
    (when selected
      (message "Remote E2E target: %s (%s)"
               (remote-target-label selected)
               (remote-target-id selected)))
    selected))

(defun remote-e2e--ssh-port (target)
  "Return the configured SSH service port for TARGET."
  (with-temp-buffer
    (unless (zerop
             (apply
              #'call-process "ssh" nil t nil
              (append
               (list "-G")
               (remote-e2e--ssh-config-args target)
               (list (remote-target-label target)))))
      (ert-fail "Cannot inspect the target SSH port"))
    (goto-char (point-min))
    (if (re-search-forward "^port[[:space:]]+\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))
      22)))

(defun remote-e2e--ssh-banner (port)
  "Read the SSH banner through local listener PORT."
  (let ((client
         (make-network-process
          :name "remote-e2e-forward-client"
          :host "127.0.0.1" :service port
          :coding 'utf-8-unix :noquery t))
        banner)
    (unwind-protect
        (progn
          (set-process-filter
           client (lambda (_process string)
                    (setq banner (concat banner string))))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (string-match-p "SSH-2.0" (or banner "")))
                        (< (float-time) deadline))
              (accept-process-output client 0.1)))
          banner)
      (when (process-live-p client)
        (delete-process client)))))

(ert-deftest remote-e2e-managed-search-worker-reports-target-tool ()
  "The client-local worker must return a usable target-native rg path."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context (remote-make-file-name target-id "/tmp/")))
         (remote-search--cache (make-hash-table :test #'equal))
         (remote-search--workers (make-hash-table :test #'equal)))
    (unless (remote-target-trusted target)
      (ert-skip "Managed search requires a trusted target"))
    (unless (or (remote-executable-find "rg" context)
                (remote-search--candidate context))
      (ert-skip "No supported ripgrep release for this target"))
    (let* ((worker (remote-search--start-worker target))
           (deadline (+ (float-time) 120)))
      (should (processp worker))
      (while (and (process-live-p worker)
                  (< (float-time) deadline))
        (accept-process-output worker 0.1))
      (should-not (process-live-p worker))
      (let ((program (nth 3 (gethash target-id remote-search--cache))))
        (should (stringp program))
        (should (file-name-absolute-p program))
        (should (remote-executable-find program context))))))

(ert-deftest remote-e2e-missing-java-runtime-does-not-start-jdtls ()
  "A target without Java must decline JDTLS before its command is built."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (unless (and (locate-library "init-lsp") (locate-library "lsp-java"))
    (ert-skip "JDTLS preflight requires the full Emacs init"))
  (require 'init-lsp)
  (require 'init-java)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (logical (remote-make-file-name target-id "/tmp/")))
    (when (remote-executable-find "java" logical)
      (ert-skip "Selected target already has Java"))
    (with-temp-buffer
      (setq default-directory logical
            buffer-file-name (expand-file-name "RemotePreflight.java" logical)
            major-mode 'java-mode)
      (let ((my/language-server--manual-start t))
        (cl-letf (((symbol-function 'lsp-deferred)
                   (lambda (&rest _arguments)
                     (ert-fail "JDTLS started without target Java"))))
          (should-not (my/lsp-mode-start-now)))))))

(ert-deftest remote-e2e-service-validator-repairs-ready-cache ()
  "A corrupt but executable SSH cache leaf must be replaced after validation."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target)))
    (unless (remote-target-trusted target)
      (ert-skip "Provisioning requires a trusted target"))
    (let* ((context (remote-context
                     (remote-make-file-name target-id "/tmp/")))
           (native-root
            (string-trim
             (remote-exec-output
              "mktemp" :args '("-d" "/tmp/emacs-service-e2e.XXXXXX")
              :context context :adapter "exec" :check t)))
           (install
            (remote-make-file-name
             target-id (concat native-root "/tool-v1/")))
           (source (make-temp-file "remote-service-e2e-source-" t))
           (client-tool (expand-file-name "bin/tool" source))
           (content "#!/bin/sh\nprintf ready\n")
           (validate
            (lambda (candidate-context candidate-directory)
              (equal
               (remote-exec-output
                "cat"
                :args
                (list
                 (concat (remote-file-local-name candidate-directory)
                         "bin/tool"))
                :context candidate-context :adapter "exec" :check t)
               content))))
      (unwind-protect
          (progn
            (make-directory (file-name-directory client-tool) t)
            (with-temp-file client-tool (insert content))
            (set-file-modes client-tool #o755)
            (remote-service-provision-directory
             "e2e-tool" source install
             :context context :adapter "exec"
             :ready-file "bin/tool" :ready-kind 'executable
             :validate validate)
            (should (funcall validate context install))
            (remote-exec
             "sh" :args
             (list "-c"
                   "printf '#!/bin/sh\\nprintf stale\\n' > \"$1\"; chmod 755 \"$1\""
                   "e2e-corrupt"
                   (concat (remote-file-local-name install) "bin/tool"))
             :context context :adapter "exec" :check t)
            (should-not (funcall validate context install))
            (remote-service-provision-directory
             "e2e-tool" source install
             :context context :adapter "exec"
             :ready-file "bin/tool" :ready-kind 'executable
             :validate validate)
            (should (funcall validate context install)))
        (delete-directory source t)
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" native-root)
           :context context :adapter "exec" :check t))))))

(ert-deftest remote-e2e-project-search-and-magit-worktree-identity ()
  "Search and Magit must reach the target without duplicating source buffers."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  ;; The isolated -Q SSH suite lacks package load paths.  The Makefile also
  ;; runs this integration check under the actual init with Consult and Magit.
  (unless (and (locate-library "consult") (locate-library "magit"))
    (ert-skip "Consult and Magit require the full Emacs init"))
  (require 'init-project)
  (require 'init-git-core)
  (require 'consult)
  (require 'magit)
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (bootstrap-context
          (remote-context (remote-make-file-name target-id "/tmp/")))
         (native-root
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-search-e2e.XXXXXX")
            :context bootstrap-context :adapter "exec" :check t)))
         (logical-root
          (file-name-as-directory
           (remote-make-file-name target-id native-root)))
         (logical-file (expand-file-name "note.txt" logical-root))
         folder-buffer status-buffer source-buffer visit-buffer
         selected selected-args rg-path initial-rg-path)
    (unwind-protect
        (progn
          (remote-exec "git" :args '("init" "-q")
                       :context logical-root :adapter "exec" :check t)
          (with-temp-file logical-file
            (insert "REMOTE_SEARCH_NEEDLE from target\n"))
          (cl-letf (((symbol-function 'consult-ripgrep)
                     (lambda (root &optional _initial)
                       (setq selected (list 'rg root)
                             selected-args consult-ripgrep-args)))
                    ((symbol-function 'consult-grep)
                     (lambda (root &optional _initial)
                       (setq selected (list 'grep root)))))
            (my/project-ripgrep logical-root))
          (setq rg-path (remote-search-ripgrep logical-root)
                initial-rg-path rg-path)
          (should
           (equal selected
                  (list (if initial-rg-path 'rg 'grep) logical-root)))
          (when initial-rg-path
            (should
             (equal selected-args
                    (remote-search-consult-ripgrep-args
                     consult-ripgrep-args initial-rg-path))))
          (when-let* ((worker (gethash target-id remote-search--workers)))
            (let ((deadline (+ (float-time) 120)))
              (while (and (process-live-p worker)
                          (< (float-time) deadline))
                (accept-process-output worker 0.1)))
            (should-not (process-live-p worker))
            (setq rg-path (remote-search-ripgrep logical-root)))
          (when (equal target-id "aaron-pc")
            (should rg-path))
          (should (remote-executable-find "grep" logical-root))
          (with-temp-buffer
            (let ((default-directory logical-root))
              (should
               (zerop
                (process-file
                 "grep" nil t nil "--null" "--line-buffered"
                 "--color=never" "--ignore-case" "--with-filename"
                 "--line-number" "-I" "-r" "-e"
                 "REMOTE_SEARCH_NEEDLE" ".")))
              (should
               (string-match-p
                (concat "note\\.txt" (string 0)
                        "[0-9]+:REMOTE_SEARCH_NEEDLE")
                (buffer-string)))))
          ;; Exercise Consult's real async process/parse path as well as the
          ;; project command dispatch.  Its candidate must retain the
          ;; worktree-relative filename so selection opens the same target.
          (with-temp-buffer
            (let* ((default-directory logical-root)
                   (consult-ripgrep-args
                    (if rg-path
                        (remote-search-consult-ripgrep-args
                         consult-ripgrep-args rg-path)
                      consult-ripgrep-args))
                   (builder
                    (if rg-path
                        (consult--ripgrep-make-builder '("."))
                      (consult--grep-make-builder '("."))))
                   (pipeline
                    (consult--process-collection
                     builder :transform (consult--grep-format builder)
                     :file-handler t))
                   candidates
                   (driver
                    (funcall pipeline
                             (lambda (event)
                               (when (listp event)
                                 (dolist (item event)
                                   (when (stringp item)
                                     (push item candidates)))))))
                   (deadline (+ (float-time) 3)))
              (funcall driver 'setup)
              (unwind-protect
                  (progn
                    (funcall driver "REMOTE_SEARCH_NEEDLE")
                    (while (and (not candidates)
                                (< (float-time) deadline))
                      (accept-process-output nil 0.05))
                    (should
                     (seq-some
                      (lambda (item)
                        (string-match-p
                         "note\\.txt:1:REMOTE_SEARCH_NEEDLE" item))
                      candidates)))
                (funcall driver 'destroy))))
          (setq source-buffer (find-file-noselect logical-file))
          (should-not (remote-get-workspace logical-root))
          (setq status-buffer (magit-status-setup-buffer logical-root))
          (should (with-current-buffer status-buffer
                    (derived-mode-p 'magit-status-mode)))
          (setq visit-buffer
                (with-current-buffer status-buffer
                  (magit-find-file-noselect "{worktree}" "note.txt")))
          (should (eq visit-buffer source-buffer))
          (should (equal (buffer-local-value 'buffer-file-name visit-buffer)
                         logical-file))
          (setq folder-buffer (remote-open-folder target native-root))
          (should (remote-workspace-live-p
                   (remote-get-workspace logical-root)))
          (should (eq
                   (with-current-buffer status-buffer
                     (magit-find-file-noselect "{worktree}" "note.txt"))
                   source-buffer))
          (with-current-buffer status-buffer
            (magit-stage-files '("note.txt")))
          (let ((deadline (+ (float-time) 5)) status)
            (while (and (< (float-time) deadline)
                        (not (string-prefix-p "A " (or status ""))))
              (setq status
                    (remote-exec-output
                     "git" :args '("status" "--porcelain" "--" "note.txt")
                     :context logical-root :adapter "exec" :check t))
              (accept-process-output nil 0.05))
            (should (string-prefix-p "A  note.txt" (or status ""))))
          (with-current-buffer status-buffer
            (magit-unstage-files '("note.txt")))
          (let ((deadline (+ (float-time) 5)) status)
            (while (and (< (float-time) deadline)
                        (not (string-prefix-p "??" (or status ""))))
              (setq status
                    (remote-exec-output
                     "git" :args '("status" "--porcelain" "--" "note.txt")
                     :context logical-root :adapter "exec" :check t))
              (accept-process-output nil 0.05))
            (should (string-prefix-p "?? note.txt" (or status "")))))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (when (and (buffer-live-p visit-buffer)
                 (not (eq visit-buffer source-buffer)))
        (kill-buffer visit-buffer))
      (when (buffer-live-p status-buffer)
        (kill-buffer status-buffer))
      (when-let* ((workspace (remote-get-workspace logical-root)))
        (remote-workspace-close workspace 'e2e-cleanup))
      (when (buffer-live-p folder-buffer)
        (kill-buffer folder-buffer))
      (when (string-match-p
             "\\`/tmp/emacs-search-e2e\\.[[:alnum:]]+\\'"
             native-root)
        (ignore-errors
          (remote-exec "rm" :args (list "-rf" native-root)
                       :context bootstrap-context :adapter "exec"
                       :check t))))))

(ert-deftest remote-e2e-board-folder-lifecycle-and-target-disconnect ()
  "A board folder owns a workspace and releases its SSH session on demand."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context
                   (remote-make-file-name target-id "/tmp/")))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-remote-folder.XXXXXX")
            :context context :adapter "exec" :check t)))
         (logical (file-name-as-directory
                   (remote-make-file-name target-id native-directory)))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (remote-open-folder target native-directory))
          (should (buffer-live-p buffer))
          (should (equal (buffer-local-value 'default-directory buffer)
                         logical))
          (should (remote-workspace-live-p
                   (remote-get-workspace logical)))
          (should (member logical remote-board-recent-folders))
          (let ((result (remote-board-disconnect-target target)))
            (should (>= (plist-get result :workspaces) 1))
            (should (>= (plist-get result :sessions) 1)))
          (should-not (remote-get-workspace logical))
          (should-not
           (seq-some
            (lambda (session)
              (equal (plist-get session :target) target-id))
            (remote-session-list)))
          (should-not (buffer-live-p buffer))
          (should (file-directory-p logical))
          (setq buffer (remote-open-folder target native-directory))
          (should (buffer-live-p buffer))
          (should (remote-workspace-live-p
                   (remote-get-workspace logical))))
      (when-let* ((workspace (remote-get-workspace logical)))
        (remote-workspace-close workspace 'e2e-cleanup))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and native-directory
                 (string-match-p
                  "\\`/tmp/emacs-remote-folder\\.[[:alnum:]]+\\'"
                  native-directory))
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" native-directory)
           :context context :adapter "exec" :check t)))
      (remote-session-clear t))))

(ert-deftest remote-e2e-attribute-cache-preserves-acl-on-repeated-saves ()
  "Repeated RPC saves must retain a target file's named ACL entry."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context
                   (remote-make-file-name target-id "/tmp/")))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-remote-acl.XXXXXX")
            :context context :adapter "exec" :check t)))
         (native-file (expand-file-name "acl.txt" native-directory))
         (logical-file (remote-make-file-name target-id native-file))
         (logical-directory
          (file-name-as-directory
           (remote-make-file-name target-id native-directory))))
    (unwind-protect
        (progn
          (unless (and (remote-executable-find "getfacl" context)
                       (remote-executable-find "setfacl" context))
            (ert-skip "Target has no getfacl/setfacl"))
          (with-temp-file logical-file
            (insert "int main(void) { return 0; }\n"))
          (remote-exec-output
           "setfacl" :args (list "-m" "u:65534:r--" native-file)
           :context context :adapter "exec" :check t)
          (let ((file-before (file-attributes logical-file 'integer))
                (modes-before (file-modes logical-file))
                (before
                 (string-trim
                  (remote-exec-output
                   "getfacl" :args (list "-cpn" native-file)
                   :context context :adapter "exec" :check t))))
            (should (string-match-p "user:65534:r--" before))
            (dotimes (_ 3)
              (with-temp-file logical-file
                (insert "int main(void) { return 1; }\n")))
            (let ((buffer (find-file-noselect logical-file))
                  (make-backup-files nil))
              (unwind-protect
                  (with-current-buffer buffer
                    (goto-char (point-max))
                    (insert "saved from a visited buffer\n")
                    (save-buffer))
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))))
            (should
             (equal
              before
              (string-trim
               (remote-exec-output
                "getfacl" :args (list "-cpn" native-file)
                :context context :adapter "exec" :check t))))
            (let ((file-after (file-attributes logical-file 'integer)))
              (should
               (equal (file-attribute-inode-number file-before)
                      (file-attribute-inode-number file-after)))
              (should
               (equal (file-attribute-user-id file-before)
                      (file-attribute-user-id file-after)))
              (should
               (equal (file-attribute-group-id file-before)
                      (file-attribute-group-id file-after)))
              (should (= modes-before (file-modes logical-file))))))
      (when (and native-directory
                 (string-match-p
                  "\\`/tmp/emacs-remote-acl\\.[[:alnum:]]+\\'"
                  native-directory))
        (ignore-errors (delete-directory logical-directory t)))
      (remote-session-clear t))))

(ert-deftest remote-e2e-executable-lookup-honors-workspace-path ()
  "A tramp-rpc executable probe must use the target environment capsule."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context
                   (remote-make-file-name target-id "/tmp/")))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-remote-path.XXXXXX")
            :context context :adapter "exec" :check t)))
         (logical-directory
          (file-name-as-directory
           (remote-make-file-name target-id native-directory)))
         (program "emacs-remote-e2e-path-tool")
         (native-program (expand-file-name program native-directory))
         (logical-program (expand-file-name program logical-directory)))
    (unwind-protect
        (progn
          (with-temp-file logical-program
            (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes logical-program #o755)
          (let ((remote-environment-providers
                 remote-environment-providers)
                (remote-environment-cache
                 (make-hash-table :test #'equal))
                (remote-environments-by-id
                 (make-hash-table :test #'equal))
                (default-directory logical-directory))
            (remote-register-environment-provider
             "e2e-custom-path" :scope 'workspace :priority 1000
             :predicate
             (lambda (candidate)
               (equal (remote-context-target-id candidate) target-id))
             :fingerprint (lambda (_candidate) native-directory)
             :load
             (lambda (_candidate)
               `(("PATH" . ,(mapconcat
                             #'identity
                             (list native-directory "/usr/bin" "/bin")
                             path-separator)))))
            (should (equal (remote-executable-find program)
                           native-program))
            (should (equal (remote-executable-find native-program)
                           native-program))
            (should-not
             (remote-executable-find
              "emacs-remote-e2e-definitely-missing"))))
      (when (and native-directory
                 (string-match-p
                  "\\`/tmp/emacs-remote-path\\.[[:alnum:]]+\\'"
                  native-directory))
        (ignore-errors (delete-directory logical-directory t)))
      (remote-session-clear t))))

(ert-deftest remote-e2e-rpc-path-batch-preserves-directory-filter ()
  "The RPC PATH accelerator keeps upstream order and removes absent dirs."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (logical-root (remote-make-file-name target-id "/tmp/"))
         (context (remote-context logical-root))
         (route (car (remote-routes "environment" 'process-sync context nil))))
    (unless (equal (remote-route-link-plugin-id route) "tramp-rpc")
      (ert-skip "Target has no selected tramp-rpc route"))
    (let* ((_connection (remote-connection-ensure route context))
           (physical (remote-project-file-name logical-root route))
           (vector (tramp-dissect-file-name physical nil))
           (present
            (string-trim
             (remote-exec-output
              "mktemp" :args '("-d" "/tmp/emacs-remote-path-batch.XXXXXX")
              :context context :adapter "exec" :check t)))
           (absent (concat present "/not-here")))
      (unwind-protect
          (cl-letf (((symbol-function 'tramp-rpc--effective-remote-path-spec)
                     (lambda (_vector)
                       (list "/usr/bin" present absent "/usr/bin"))))
            (should
             (equal (tramp-rpc--compute-remote-path vector)
                    (list "/usr/bin" present))))
        (when (string-match-p
               "\\`/tmp/emacs-remote-path-batch\\.[[:alnum:]]+\\'"
               present)
          (ignore-errors
            (delete-directory
             (remote-make-file-name target-id present) t)))
        (remote-session-clear t)))))

(ert-deftest remote-e2e-ssh-file-process-and-session-contract ()
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target
          (or (remote-e2e--target)
              (ert-fail
               "No imported Aaron-* target; set REMOTE_E2E_TARGET")))
         (target-id (remote-target-id target))
         (bootstrap-context
          (remote-context
           (remote-make-file-name target-id "/tmp/")))
         (remote-directory
          (string-trim
           (remote-exec-output
            "mktemp"
            :args '("-d" "/tmp/emacs-remote-e2e.XXXXXX")
            :context bootstrap-context
            :adapter "exec"
            :check t)))
         (logical-directory
          (file-name-as-directory
           (remote-make-file-name target-id remote-directory)))
         (remote-file
          (expand-file-name "roundtrip.txt" logical-directory))
         (local-directory
          (make-temp-file "emacs-remote-e2e-" t))
         (local-file
          (expand-file-name "source.txt" local-directory))
         (payload
          (format "remote-e2e:%s:%s\n"
                  target-id (float-time))))
    (unwind-protect
        (progn
          (write-region payload nil local-file nil 'silent)
          (copy-file local-file remote-file)
          (should (file-exists-p remote-file))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents remote-file)
              (buffer-string))
            payload))
          (should
           (member remote-file
                   (directory-files logical-directory t
                                    "\\`roundtrip\\.txt\\'")))
          (let ((pwd
                 (string-trim
                  (remote-exec-output
                   "pwd" :context logical-directory
                   :adapter "exec" :check t))))
            (should
             (equal
              (directory-file-name pwd)
              (directory-file-name remote-directory))))
          (let* ((context (remote-context remote-file))
                 (first
                  (remote-session-warm
                   context "process" 'process-sync))
                 (second
                  (remote-session-warm
                   context "process" 'process-sync)))
            (should (eq first second))
            (should (> (remote-session-use-count second) 1)))
          (let (async-result)
            (let ((process
                   (remote-exec-async
                    "sh"
                    :args '("-c"
                            "printf async-stdout; printf async-stderr >&2")
                    :context logical-directory
                    :adapter "exec"
                    :callback
                    (lambda (result)
                      (setq async-result result)))))
              (while (process-live-p process)
                (accept-process-output process 0.1))
              (while (null async-result)
                (accept-process-output nil 0.05)))
            (should (zerop (remote-exec-result-status async-result)))
            (should
             (equal (remote-exec-result-stdout async-result)
                    "async-stdout"))
            (should
             (equal (remote-exec-result-stderr async-result)
                    "async-stderr")))
          ;; A routed listener remains an ordinary Emacs server process while
          ;; its advertised contact is the target-side dynamic SSH -R port.
          (when-let* ((python
                       (remote-executable-find
                        "python3" logical-directory)))
            (let (received listener)
              (unwind-protect
                  (progn
                    (setq listener
                          (remote-make-network-process
                           :name "remote-e2e-listener"
                           :server t
                           :host "127.0.0.1" :service t
                           :coding 'binary :noquery t
                           :filter
                           (lambda (_process string)
                             (setq received
                                   (concat received string)))
                           :remote-context
                           (remote-context logical-directory)))
                    (let ((port (process-contact listener :service)))
                      (should (integerp port))
                      (remote-exec
                       python
                       :args
                       (list
                        "-c"
                        (concat
                         "import socket;"
                         "s=socket.create_connection(('127.0.0.1',"
                         (number-to-string port)
                         "));s.sendall(b'reverse-ok');s.close()"))
                       :context logical-directory
                       :adapter "exec" :check t))
                    (let ((deadline (+ (float-time) 3)))
                      (while (and (not (equal received "reverse-ok"))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.05)))
                    (should (equal received "reverse-ok")))
                (when listener
                  (remote-close-channel listener))))))
      (when (and remote-directory
                 (string-match-p
                  "\\`/tmp/emacs-remote-e2e\\.[[:alnum:]]+\\'"
                  remote-directory))
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" remote-directory)
           :context bootstrap-context :adapter "exec")))
      (when (file-directory-p local-directory)
        (delete-directory local-directory t))
      (remote-session-clear t))))

(ert-deftest remote-e2e-recursive-watch-delivers-nested-and-newline-names ()
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context
                   (remote-make-file-name target-id "/tmp/")))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-remote-watch.XXXXXX")
            :context context :adapter "exec" :check t)))
         (root (file-name-as-directory
                (remote-make-file-name target-id native-directory)))
         (nested (expand-file-name "nested" root))
         (file (expand-file-name "line\nbreak.txt" nested))
         (after-reconnect (expand-file-name "after-reconnect.txt" nested))
         workspace descriptor events physical-before)
    (unwind-protect
        (progn
          (setq workspace (remote-workspace-open root :connect t))
          (setq descriptor
                (remote-watch-tree
                 root '(change)
                 (lambda (event) (push event events))))
          (let ((deadline (+ (float-time) 10)))
            (while (and (not (file-notify-valid-p descriptor))
                        (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (should (file-notify-valid-p descriptor))
          (let* ((watch (remote-get-file-watch descriptor))
                 (physical
                  (and watch
                       (remote-file-watch-physical-descriptor watch))))
            (should (processp physical))
            (setq physical-before physical)
            (should (memq (process-get physical
                                       'remote-file-watch-provider)
                          '(python-inotify inotifywait))))
          (remote-exec
           "mkdir" :args (list (remote-file-local-name nested))
           :context context :adapter "exec" :check t)
          (remote-exec
           "python3"
           :args (list "-c" "import pathlib,sys; pathlib.Path(sys.argv[1]).write_text('watch')"
                       (remote-file-local-name file))
           :context context :adapter "exec" :check t)
          (let ((deadline (+ (float-time) 10)))
            (while (and (not (seq-some
                              (lambda (event)
                                (and (equal (nth 2 event) file)
                                     (memq (nth 1 event)
                                           '(created changed))))
                              events))
                        (< (float-time) deadline))
              (accept-process-output nil 0.1)
              (ignore-errors (read-event nil nil 0.05))))
          (should (seq-some
                   (lambda (event)
                     (and (equal (nth 2 event) file)
                          (memq (nth 1 event) '(created changed))))
                   events))
          (remote-workspace-reconnect workspace)
          (should (remote-workspace-live-p workspace))
          (should (file-notify-valid-p descriptor))
          (should-not
           (eq physical-before
               (remote-file-watch-physical-descriptor
                (remote-get-file-watch descriptor))))
          (setq events nil)
          (remote-exec
           "python3"
           :args (list "-c" "import pathlib,sys; pathlib.Path(sys.argv[1]).write_text('recovered')"
                       (remote-file-local-name after-reconnect))
           :context context :adapter "exec" :check t)
          (let ((deadline (+ (float-time) 10)))
            (while (and (not (seq-some
                              (lambda (event)
                                (and (equal (nth 2 event) after-reconnect)
                                     (memq (nth 1 event)
                                           '(created changed))))
                              events))
                        (< (float-time) deadline))
              (accept-process-output nil 0.1)
              (ignore-errors (read-event nil nil 0.05))))
          (should (seq-some
                   (lambda (event)
                     (and (equal (nth 2 event) after-reconnect)
                          (memq (nth 1 event) '(created changed))))
                   events)))
      (when descriptor
        (ignore-errors (file-notify-rm-watch descriptor)))
      (when workspace
        (remote-workspace-close workspace 'e2e-cleanup))
      (when (string-match-p
             "\\`/tmp/emacs-remote-watch\\.[[:alnum:]]+\\'"
             native-directory)
        (ignore-errors
          (remote-exec "rm" :args (list "-rf" native-directory)
                       :context context :adapter "exec" :check t)))
      (remote-session-clear t))))

(ert-deftest remote-e2e-ssh-workspace-reconnect-survives-session-invalidation ()
  "A real SSH reconnect must complete after invalidating its old session."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (root (remote-make-file-name target-id "/tmp/"))
         (workspace (remote-workspace-open root :connect t))
         (initial-epoch (remote-background-target-epoch target-id))
         result failure job)
    (unwind-protect
        (progn
          (should (remote-workspace-live-p workspace))
          (setq job
                (remote-workspace-reconnect-async
                 workspace :force t
                 :callback (lambda (value) (setq result value))
                 :error-callback (lambda (error) (setq failure error))))
          (let ((deadline (+ (float-time) 35)))
            (while (and (not result) (not failure)
                        (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (when failure
            (ert-fail (error-message-string failure)))
          (should (eq result workspace))
          (should (eq (remote-background-job-state job) 'complete))
          (should (remote-workspace-live-p workspace))
          (should (> (remote-background-target-epoch target-id)
                     initial-epoch))
          (should (equal (string-trim
                          (remote-exec-output
                           "pwd" :context root :adapter "exec" :check t))
                         "/tmp")))
      (remote-workspace-close workspace 'e2e-cleanup)
      (remote-session-clear t))))

(ert-deftest remote-e2e-board-forward-reaches-target-ssh-and-closes ()
  "Forward a real target port, read its banner, and close it from the board."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (ssh-port (remote-e2e--ssh-port target))
         (forward (remote-board-forward-port target-id ssh-port))
         (channel (remote-channel-of forward))
         (endpoint (remote-channel-endpoint forward 'local))
         (local-port (plist-get endpoint :port))
         client banner old-forward)
    (unwind-protect
        (progn
          (should (and (integerp local-port) (> local-port 0)))
          (setq client
                (make-network-process
                 :name "remote-e2e-forward-client"
                 :host "127.0.0.1" :service local-port
                 :coding 'utf-8-unix :noquery t
                 :filter
                 (lambda (_process string)
                   (setq banner (concat banner string)))))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (string-match-p
                              "SSH-2.0" (or banner "")))
                        (< (float-time) deadline))
              (accept-process-output client 0.1)))
          (should (string-match-p "SSH-2.0" (or banner "")))
          (delete-process client)
          (setq client nil)
          (with-current-buffer (get-buffer-create "*Remote*")
            (remote-board-mode)
            (tabulated-list-print t)
            (let ((wanted
                   (list 'forward target-id
                         (remote-channel-id channel))))
              (goto-char (point-min))
              (while (and (not (equal (tabulated-list-get-id) wanted))
                          (not (eobp)))
                (forward-line 1))
              (should (equal (tabulated-list-get-id) wanted)))
            (remote-board-rename-forward "target ssh")
            (setq old-forward forward
                  forward (remote-board-change-local-port 0)
                  channel (remote-channel-of forward)
                  local-port
                  (plist-get (remote-channel-endpoint forward 'local)
                             :port))
            (should (eq (remote-forward-state old-forward) 'closed))
            (should (equal (plist-get
                            (remote-channel-metadata channel) :name)
                           "target ssh"))
            (should (string-match-p
                     "SSH-2.0"
                     (or (remote-e2e--ssh-banner local-port) "")))
            (tabulated-list-print t)
            (let ((wanted
                   (list 'forward target-id
                         (remote-channel-id channel))))
              (goto-char (point-min))
              (while (and (not (equal (tabulated-list-get-id) wanted))
                          (not (eobp)))
                (forward-line 1))
              (should (equal (tabulated-list-get-id) wanted)))
            (remote-copy-target-uri)
            (should (equal (current-kill 0)
                           (format "127.0.0.1:%d" local-port)))
            (remote-board-close-forward))
          (should (eq (remote-forward-state forward) 'closed))
          (should-not (gethash (remote-channel-id channel)
                               remote-channels)))
      (when client
        (delete-process client))
      (ignore-errors (remote-close-channel forward))
      (when-let* ((board (get-buffer "*Remote*")))
        (kill-buffer board))
      (remote-workspace-clear 'e2e-cleanup)
      (remote-session-clear t))))

(ert-deftest remote-e2e-forward-reconnect-keeps-local-address ()
  "A forced SSH reconnect preserves a board forward's working local port."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (forward
          (remote-board-forward-port
           (remote-target-id target)
           (remote-e2e--ssh-port target)))
         (port (plist-get (remote-channel-endpoint forward 'local)
                          :port))
         (workspace
          (seq-find
           (lambda (candidate)
             (seq-some
              (lambda (resource)
                (eq (remote-workspace-resource-value resource)
                    forward))
              (remote-workspace-resources candidate)))
           (hash-table-values remote-workspaces)))
         (resource
          (and workspace
               (seq-find
                (lambda (candidate)
                 (eq (remote-workspace-resource-value candidate)
                      forward))
                (remote-workspace-resources workspace))))
         replacement)
    (unwind-protect
        (progn
          (should workspace)
          (should resource)
          (with-temp-buffer
            (remote-board-mode)
            (cl-letf (((symbol-function 'remote-board--forward-at-point)
                       (lambda () (remote-channel-of forward))))
              (remote-board-rename-forward "recovered ssh")))
          (should (string-match-p "SSH-2.0"
                                  (or (remote-e2e--ssh-banner port) "")))
          (remote-workspace-reconnect workspace)
          (setq replacement (remote-workspace-resource-value resource))
          (should (remote-workspace-live-p workspace))
          (should (eq (remote-workspace-resource-state resource) 'open))
          (should (eq (remote-forward-state forward) 'closed))
          (should (not (eq forward replacement)))
          (should (= (plist-get
                      (remote-channel-endpoint replacement 'local)
                      :port)
                     port))
          (should (equal
                   (plist-get
                    (remote-channel-metadata
                     (remote-channel-of replacement)) :name)
                   "recovered ssh"))
          (should (string-match-p "SSH-2.0"
                                  (or (remote-e2e--ssh-banner port) ""))))
      (remote-workspace-clear 'e2e-cleanup)
      (remote-session-clear t))))

(ert-deftest remote-e2e-target-home-and-symlinks-keep-logical-identity ()
  "SSH home expansion and symlink traversal stay in the owning target."
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target (or (remote-e2e--target)
                     (ert-fail "No reachable Remote target")))
         (target-id (remote-target-id target))
         (context (remote-context (remote-make-file-name target-id "/tmp/")))
         (home (string-trim
                (remote-exec-output
                 "sh" :args '("-c" "printf '%s' \"$HOME\"")
                 :context context :adapter "exec" :check t)))
         (root-entry
          (ignore-errors
            (string-trim
             (remote-exec-output
              "getent" :args '("passwd" "root")
              :context context :adapter "exec" :check t))))
         (user (string-trim
                (remote-exec-output
                 "id" :args '("-un")
                 :context context :adapter "exec" :check t)))
         (native-root
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-path-e2e.XXXXXX")
            :context context :adapter "exec" :check t)))
         (logical-root (file-name-as-directory
                        (remote-make-file-name target-id native-root)))
         (native-file (concat native-root "/target.txt"))
         (logical-file (remote-make-file-name target-id native-file))
         (relative-link (concat logical-root "sub/relative-link"))
         (absolute-link (concat logical-root "absolute-link"))
         (dangling-link (concat logical-root "dangling-link")))
    (unwind-protect
        (progn
          (should (equal
                   (directory-file-name
                    (remote-file-local-name
                     (remote-expand-file-name "~/" nil target-id)))
                   (directory-file-name home)))
          (should (equal
                   (directory-file-name
                    (remote-file-local-name
                     (remote-expand-file-name
                      (format "~%s/" user) nil target-id)))
                   (directory-file-name home)))
          (when-let* ((fields (and root-entry
                                  (split-string root-entry ":")))
                      ((>= (length fields) 6)))
            (should
             (equal
              (directory-file-name
               (remote-file-local-name
                (remote-expand-file-name "~root/" nil target-id)))
              (directory-file-name (nth 5 fields)))))
          (with-temp-file logical-file (insert "remote-target\n"))
          (make-directory (concat logical-root "sub/"))
          (make-symbolic-link "../target.txt" relative-link)
          (make-symbolic-link logical-file absolute-link)
          (make-symbolic-link "missing.txt" dangling-link)
          (should (equal (file-symlink-p relative-link) "../target.txt"))
          (should (equal (file-symlink-p absolute-link) native-file))
          (should (equal (file-symlink-p dangling-link) "missing.txt"))
          (should-not (file-exists-p dangling-link))
          (should (equal (file-truename relative-link) logical-file))
          (should (equal (file-truename absolute-link) logical-file))
          (should (file-equal-p relative-link logical-file))
          (should (file-equal-p absolute-link logical-file)))
      (ignore-errors (delete-directory logical-root t)))))

(provide 'remote-e2e-tests)
;;; remote-e2e-tests.el ends here
