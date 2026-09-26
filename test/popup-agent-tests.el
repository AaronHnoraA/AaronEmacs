;;; popup-agent-tests.el --- Shared terminal/ACP popup regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-vterm-popup)
(require 'noema-agent-acp)
(require 'init-ai-ide)

(ert-deftest agent-shell-close-does-not-prompt-for-txt-or-disable-other-queries ()
  (dolist (popup '(nil t))
    (let ((buffer (generate-new-buffer " *popup-transcript-test*"))
          (shell-maker-prompt-before-killing-buffer t)
          (queries 0))
      (unwind-protect
          (with-current-buffer buffer
            (setq major-mode 'agent-shell-mode)
            (setq-local shell-maker--config 'test
                        kill-buffer-query-functions
                        (list #'shell-maker-kill-buffer-query
                              (lambda () (cl-incf queries) t)))
            (insert "Agent output and an unsent prompt")
            (run-hooks 'agent-shell-mode-hook)
            (when popup (my/vterm-popup-apply-ui buffer))
            (should (buffer-modified-p))
            (should (local-variable-p 'shell-maker-prompt-before-killing-buffer))
            (should-not shell-maker-prompt-before-killing-buffer)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Asked to save transcript")))
                      ((symbol-function 'shell-maker-save-session-transcript) (lambda () (ert-fail "Exported transcript"))))
              (should (kill-buffer buffer)))
            (should (= queries 1)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-transcripts-disabled-for-new-and-existing-sessions ()
  (should-not (agent-shell--transcript-file-path))
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    ;; Existing sessions retain their old path until the policy is applied.
    (setq-local agent-shell--transcript-file "/unused/transcript.md")
    (my/agent-shell-disable-transcripts)
    (cl-letf (((symbol-function 'write-region)
               (lambda (&rest _) (ert-fail "Wrote a transcript")))
              ((symbol-function 'make-directory)
               (lambda (&rest _) (ert-fail "Created a transcript directory"))))
      (should-not (agent-shell--ensure-transcript-file))
      (agent-shell--append-transcript :text "Agent output"
                                      :file-path agent-shell--transcript-file))))

(ert-deftest popup-agent-transcript-policy-does-not-affect-other-buffers ()
  (let ((shell-maker-prompt-before-killing-buffer t))
    (with-temp-buffer
      (my/vterm-popup-apply-ui (current-buffer))
      (should shell-maker-prompt-before-killing-buffer)
      (should-not (local-variable-p 'shell-maker-prompt-before-killing-buffer)))
    (should shell-maker-prompt-before-killing-buffer)))

(defmacro popup-agent-test-with-placement (client &rest body)
  "Run BODY with every logical directory client-accessible when CLIENT."
  (declare (indent 1))
  `(cl-letf* ((directory-p (symbol-function 'file-directory-p))
              ((symbol-function 'remote-client-file-name)
               (lambda (path)
                 (and ,client (string-prefix-p "/fs:local:" path)
                      (substring path (length "/fs:local:")))))
              ;; Only the tests' fake workspaces; everything else is real.
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (cond ((string-match-p "unreachable" path) nil)
                       ((string-match-p "\\`\\(?:/Users/test/\\|/fs:\\)" path) t)
                       (t (funcall directory-p path))))))
     ,@body))

(ert-deftest agent-shell-opencode-prefers-verified-binary-for-client-agents ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (my/agent-shell-opencode-executable "/test/official/opencode")
        (default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (path) (equal path "/test/official/opencode"))))
        (should (equal (my/agent-shell-use-official-opencode
                        '(:command "opencode" :command-params ("acp") :context-buffer nil))
                       '(:command "/test/official/opencode" :command-params ("acp")
                                  :context-buffer nil)))
        ;; The global setting itself is never rewritten.
        (should (equal agent-shell-opencode-acp-command '("opencode" "acp")))))))

(ert-deftest agent-shell-opencode-remote-agent-uses-target-path ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (my/agent-shell-opencode-executable "/test/official/opencode")
        (default-directory "/fs:server:/home/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t)))
        (should (equal (my/agent-shell-use-official-opencode
                        '(:command "opencode" :command-params ("acp")))
                       '(:command "opencode" :command-params ("acp"))))))))

(ert-deftest agent-shell-opencode-preserves-custom-launchers-and-missing-install ()
  (let ((default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (let ((agent-shell-opencode-acp-command '("/custom/opencode" "acp" "--pure")))
        (cl-letf (((symbol-function 'file-executable-p)
                   (lambda (_) (ert-fail "Custom launcher was inspected"))))
          (should (equal (my/agent-shell-use-official-opencode
                          '(:command "/custom/opencode" :command-params ("acp" "--pure")))
                         '(:command "/custom/opencode" :command-params ("acp" "--pure"))))))
      (let ((agent-shell-opencode-acp-command '("opencode" "acp")))
        (cl-letf (((symbol-function 'file-executable-p) (lambda (_) nil)))
          (should (equal (my/agent-shell-use-official-opencode
                          '(:command "opencode" :command-params ("acp")))
                         '(:command "opencode" :command-params ("acp")))))))))

(ert-deftest agent-shell-opencode-expands-config-store-abbreviated-paths ()
  (let ((agent-shell-opencode-acp-command '("opencode" "acp"))
        (my/agent-shell-opencode-executable "~/.config/emacs/var/opencode")
        (default-directory "/Users/test/project/"))
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t)))
        (should (equal (plist-get (my/agent-shell-use-official-opencode
                                   '(:command "opencode" :command-params ("acp")))
                                  :command)
                       (expand-file-name my/agent-shell-opencode-executable)))))))

(ert-deftest agent-shell-native-entrypoint-projects-logical-cwd ()
  ;; Call upstream's public CWD directly, without the Noema start wrapper.
  (let ((agent-shell-cwd-function (lambda () "/fs:local:/Users/test/project/")))
    (popup-agent-test-with-placement t
      (should (equal (agent-shell-cwd) "/Users/test/project/")))))

(ert-deftest agent-shell-process-directory-is-native-for-local-target ()
  (popup-agent-test-with-placement t
    (should (equal (my/agent-shell-process-directory "/Users/test/project")
                   "/Users/test/project/"))
    (should (equal (my/agent-shell-process-directory "/fs:local:/Users/test/project/")
                   "/Users/test/project/"))))

(ert-deftest agent-shell-process-directory-routes-remote-target ()
  ;; A target that shares nothing with the client keeps its logical identity,
  ;; so acp.el's `make-process' enters the /fs: handler and runs it there.
  (popup-agent-test-with-placement t
    (should (equal (my/agent-shell-process-directory "/fs:server:/home/test/project")
                   "/fs:server:/home/test/project/"))))

(ert-deftest agent-shell-path-resolver-is-identity-on-local-target ()
  (popup-agent-test-with-placement t
    (let ((default-directory "/Users/test/project/"))
      (should (equal (my/agent-shell-resolve-path "/Users/test/project/a.el")
                     "/Users/test/project/a.el"))
      (should (equal (my/agent-shell-resolve-path "/fs:local:/Users/test/project/a.el")
                     "/Users/test/project/a.el"))
      (should (equal (my/agent-shell-resolve-path "relative.el") "relative.el")))))

(ert-deftest agent-shell-path-resolver-maps-remote-session-both-ways ()
  (popup-agent-test-with-placement t
    (let ((default-directory "/fs:server:/home/test/project/"))
      ;; Emacs -> agent: session/new cwd, mentions, diffs.
      (should (equal (my/agent-shell-resolve-path "/fs:server:/home/test/project/")
                     "/home/test/project/"))
      ;; Agent -> Emacs: fs/read_text_file and fs/write_text_file.
      (should (equal (my/agent-shell-resolve-path "/home/test/project/a.py")
                     "/fs:server:/home/test/project/a.py")))))

(ert-deftest agent-shell-requests-resolve-in-their-own-session ()
  (let ((session (generate-new-buffer " *agent-session-test*"))
        seen)
    (unwind-protect
        (progn
          (with-current-buffer session
            (setq default-directory "/fs:server:/home/test/"))
          (with-temp-buffer
            (my/agent-shell-in-session-buffer-a
             (lambda (&rest _) (setq seen default-directory))
             :state (list (cons :buffer session)) :acp-request nil))
          (should (equal seen "/fs:server:/home/test/")))
      (kill-buffer session))))

(ert-deftest agent-shell-context-files-map-into-the-session-machine ()
  (popup-agent-test-with-placement t
    (let ((remote (generate-new-buffer " *remote-session*"))
          (local (generate-new-buffer " *local-session*")))
      (unwind-protect
          (progn
            (with-current-buffer remote
              (setq default-directory "/fs:server:/home/test/project/"))
            (with-current-buffer local
              (setq default-directory "/Users/test/project/"))
            ;; Same machine: the agent gets that machine's native path.
            (should (equal (my/agent-shell-agent-file-name
                            "/fs:server:/home/test/project/a.py" remote)
                           "/home/test/project/a.py"))
            (should (equal (my/agent-shell-agent-file-name
                            "/Users/test/project/a.py" local)
                           "/Users/test/project/a.py"))
            (should (equal (my/agent-shell-agent-file-name
                            "/Users/test/project/a.py" nil)
                           "/Users/test/project/a.py"))
            ;; Different machine: unreachable, never a wrong path.
            (should-not (my/agent-shell-agent-file-name
                         "/Users/test/project/a.py" remote))
            (should-not (my/agent-shell-agent-file-name
                         "/fs:server:/home/test/project/a.py" local)))
        (kill-buffer remote)
        (kill-buffer local)))))

(ert-deftest popup-agent-accepts-a-remote-workspace ()
  ;; Placement belongs to the agent boundary; the popup does not branch on it.
  (let (started)
    (cl-letf (((symbol-function 'noema-agent-acp-config-for) (lambda (_) 'config))
              ((symbol-function 'noema-agent-acp-start)
               (lambda (&rest args) (setq started (plist-get args :directory))
                 (generate-new-buffer " *popup-remote-agent*")))
              ((symbol-function 'noema-agent-acp-adopt) #'ignore)
              ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore)
              ((symbol-function 'my/vterm-popup-display-buffer) #'ignore)
              ((symbol-function 'my/vterm-popup--requested-workspace-id) #'ignore)
              ((symbol-function 'my/project-current-root)
               (lambda () "/fs:server:/home/test/project/")))
      ;; From a file below the project, the agent starts at the project root.
      (let ((default-directory "/fs:server:/home/test/project/src/"))
        (let ((buffer (my/vterm-popup-agent 'codex)))
          (ignore buffer)))
      (should (equal started "/fs:server:/home/test/project/"))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p " *popup-remote-agent*" (buffer-name buffer))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-process-directory-refuses-an-unreachable-workspace ()
  ;; shell-maker would silently fall back to the local home directory.
  (popup-agent-test-with-placement t
    (should-error (my/agent-shell-process-directory "/fs:unreachable:/srv/")
                  :type 'user-error)))

(ert-deftest agent-shell-client-gets-its-workspace-environment ()
  "A natively spawned agent gets its workspace capsule in its buffer.
A routed agent gets it from the process route instead: its buffer keeps this
machine's HOME, because agent-shell's caches expand `~' there."
  (let (ensured my/agent-shell--lookup-target)
    (popup-agent-test-with-placement t
      (cl-letf (((symbol-function 'remote-environment-ensure)
                 (lambda (context &rest _) (setq ensured context)))
                ((symbol-function 'remote-context)
                 (lambda (dir)
                   (remote-context-create
                    :target-id (if (string-prefix-p "/fs:server:" dir) "server" "local")
                    :localname dir))))
        (with-temp-buffer
          (setq default-directory "/Users/test/project/")
          (my/agent-shell-apply-workspace-environment)
          (should (equal (remote-context-localname ensured) "/Users/test/project/"))
          (should (equal my/agent-shell--lookup-target "local"))
          ;; Already projected: not resolved again.
          (setq ensured nil)
          (setq-local remote-buffer-environment 'capsule)
          (my/agent-shell-apply-workspace-environment)
          (should-not ensured))
        (with-temp-buffer
          (setq default-directory "/fs:server:/home/test/project/")
          (my/agent-shell-apply-workspace-environment)
          (should-not ensured)
          (should (equal my/agent-shell--lookup-target "server")))))
      ;; The missing-executable report names that target even after
      ;; agent-shell has killed the shell buffer.
      (should (string-match-p "target `server'"
                              (my/agent-shell-missing-executable-a "not found")))))

(ert-deftest popup-agent-outside-a-project-starts-here ()
  (let (started)
    (cl-letf (((symbol-function 'noema-agent-acp-config-for) (lambda (_) 'config))
              ((symbol-function 'noema-agent-acp-start)
               (lambda (&rest args) (setq started (plist-get args :directory))
                 (generate-new-buffer " *popup-remote-agent*")))
              ((symbol-function 'noema-agent-acp-adopt) #'ignore)
              ((symbol-function 'noema-agent-acp-tabs-mode) #'ignore)
              ((symbol-function 'my/vterm-popup-display-buffer) #'ignore)
              ((symbol-function 'my/vterm-popup--requested-workspace-id) #'ignore)
              ((symbol-function 'my/project-current-root) #'ignore))
      (let ((default-directory "/fs:server:/home/test/scratch/"))
        (my/vterm-popup-agent 'codex))
      (should (equal started "/fs:server:/home/test/scratch/"))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p " *popup-remote-agent*" (buffer-name buffer))
          (kill-buffer buffer))))))

(ert-deftest popup-agent-launchers-are-memory-only ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) (ert-fail "Header checked executables")))
            ((symbol-function 'noema-agent-acp-start) (lambda (&rest _) (ert-fail "Header started agent"))))
    (let* ((term (my/vterm-popup--new-tab-segment))
           (agent (my/vterm-popup--agent-tab-segment))
           (term-map (get-text-property 0 'local-map term))
           (agent-map (get-text-property 0 'local-map agent)))
      (should (eq (lookup-key term-map [header-line mouse-3]) #'my/vterm-popup-menu))
      (should (eq (lookup-key agent-map [tab-line mouse-1]) #'my/vterm-popup-agent-menu))
      (should (equal (mapcar (lambda (item) (aref item 0)) (my/vterm-popup--agent-menu-items))
                     '("Claude" "Codex" "OpenCode"))))))

(ert-deftest popup-agent-menu-includes-configured-apps-and-agent-submenu ()
  (let ((my/project-popup-vterm-apps '(("yazi" . "yazi") ("btop" . "btop"))) items)
    (cl-letf (((symbol-function 'easy-menu-create-menu) (lambda (_title entries) (setq items entries)))
              ((symbol-function 'popup-menu) #'ignore))
      (my/vterm-popup-menu nil))
    (should (equal (mapcar (lambda (item) (aref item 0)) (cdr (assoc "Applications" items))) '("yazi" "btop")))
    (should (= (length (cdr (assoc "Agent" items))) 3))))

(ert-deftest popup-agent-starts-acp-not-vterm-and-shares-the-real-popup ()
  (save-window-excursion
    (let ((my/vterm-popup-buffers nil) (my/vterm-popup-current-buffer nil)
          (my/vterm-popup--displaying t) (my/vterm-popup-window-height 0.3)
          (agent (generate-new-buffer " *popup-test-agent*"))
          (term (generate-new-buffer " *popup-test-terminal*")) started)
      (unwind-protect
          (cl-letf (((symbol-function 'my/vterm-workspace-id) (lambda (&rest _) "local"))
                    ((symbol-function 'noema-agent-acp-config-for) (lambda (id) (list id)))
                    ((symbol-function 'noema-agent-acp-start)
                     (lambda (&rest args)
                       (setq started args)
                       (with-current-buffer agent
                         (setq major-mode 'agent-shell-mode)
                         (setq-local header-line-format "native ACP header")
                         (noema-agent-acp-tabs-mode 1))
                       agent))
                    ((symbol-function 'my/vterm-popup--create-buffer) (lambda (&rest _) (ert-fail "Agent used vterm")))
                    ((symbol-function 'noema-agent-render-flush) #'ignore))
            (my/vterm-popup-display-buffer term)
            (let ((window (my/vterm-popup--window)))
              (should (eq (my/vterm-popup-agent 'codex) agent))
              (should (equal (plist-get started :config) '(codex)))
              (should-not (plist-get started :focus))
              (should (eq (my/vterm-popup--window) window))
              (should (eq (window-buffer window) agent))
              (with-current-buffer agent
                (should (eq major-mode 'agent-shell-mode))
                (should my/vterm-popup-instance-p)
                (should-not noema-agent-acp-tabs-mode)
                (should (equal header-line-format "native ACP header"))
                (should (eq (key-binding (kbd "C-c C-e")) #'vterm-toggle))
                (should (eq (key-binding (kbd "C-c E")) #'my/vterm-popup-cycle))
                (should (eq (key-binding (kbd "C-c M-e")) #'my/vterm-toggle-fixed)))
              ;; ACP redisplay must not move the popup session to the project window.
              (noema-agent-acp-show-buffer agent)
              (should (eq (my/vterm-popup--window) window))
              (my/vterm-popup-cycle nil)
              (should (eq (window-buffer window) term))
              (my/vterm-popup-cycle nil)
              (should (eq (window-buffer window) agent))
              (vterm-toggle)
              (should-not (my/vterm-popup--window))
              (should (buffer-live-p agent))
              (vterm-toggle)
              (should (eq (window-buffer (my/vterm-popup--window)) agent))
              (my/vterm-toggle-fixed)
              (should (buffer-local-value 'my/vterm-popup-fixed agent))))
        (my/vterm-hide-popup)
        (kill-buffer agent)
        (kill-buffer term)))))

(ert-deftest popup-agent-entrypoints-select-the-corresponding-adapter ()
  (let (started)
    (cl-letf (((symbol-function 'my/vterm-popup-agent) (lambda (id) (push id started))))
      (my/vterm-popup-agent-claude)
      (my/vterm-popup-agent-codex)
      (my/vterm-popup-agent-opencode))
    (should (equal (nreverse started) '(claude codex opencode)))))

(ert-deftest agent-shell-adapters-run-the-workspace-cli-without-fallback ()
  "Claude/Codex adapters get the CLI from the workspace PATH, or fail."
  (let ((my/agent-shell--lookup-target "local"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &optional _remote)
                 (and (equal program "claude") "/home/me/.local/bin/claude"))))
      (should (equal (plist-get (my/agent-shell-use-workspace-cli
                                 '(:command "claude-agent-acp"
                                   :environment-variables ("A=1")))
                                :environment-variables)
                     '("CLAUDE_CODE_EXECUTABLE=/home/me/.local/bin/claude" "A=1")))
      (should-error (my/agent-shell-use-workspace-cli '(:command "codex-acp"))
                    :type 'user-error)
      (should (equal (my/agent-shell-use-workspace-cli '(:command "pi-acp"))
                     '(:command "pi-acp"))))))

;;; popup-agent-tests.el ends here
