;;; jupyter-project-live-smoke.el --- Real remote project acceptance -*- lexical-binding: t; -*-
;; Opt-in, isolated files and shell/LSP processes; existing kernels stay alive.
(require 'init-aaronnote-jupyter-lsp)
(require 'init-lsp)
(load (expand-file-name "test/lsp-live-smoke.el" user-emacs-directory) nil t)
(let* ((target (or (getenv "JUPYTER_PROJECT_TARGET") "aaron-pc"))
       (python (or (getenv "JUPYTER_PROJECT_PYTHON")
                   "/home/aaron/Desktop/UNSW/COMP9444/.conda/bin/python"))
       (server (or (getenv "JUPYTER_PROJECT_LSP") "/snap/bin/pyright-langserver"))
       (root (file-name-as-directory (make-temp-file (remote-make-file-name target "/tmp/noema-project-audit-") t)))
       (native (file-local-name root))
       (source (expand-file-name "train.py" root))
       (entry `((name . "project-live-test")
                (spec . ((argv . ["python" "-m" "remote_ikernel" "--interface" "ssh"
                                  "--host" ,target "--workdir" ,native
                                  "--kernel_cmd" ,(concat python " -m ipykernel -f {host_connection_file}")])
                         (metadata . ((aaron . ((project . ((direnv . t)
                                                           (lsp . ((server . [,server "--stdio"])))))))))))))
       (my/noema-jupyter-project--bindings (make-hash-table :test #'equal))
       shell-buffer source-buffer directory-buffer)
  (princ (format "Remote fixture: %s\n" root))
  (unwind-protect
      (progn
        (with-temp-file (expand-file-name ".envrc" root)
          (insert (format "PATH_add %s\nexport NOEMA_PROJECT_AUDIT=remote-envrc\n"
                          (file-name-directory python))))
        ;; Only this freshly created test script is authorized by the test.
        ;; Product commands never perform direnv allow.
        (let ((default-directory root) (remote-environment-inhibit t))
          (unless (zerop (process-file "direnv" nil nil nil "allow" "."))
            (error "Cannot authorize disposable test envrc")))
        (with-temp-file source (insert "import numpy as np\nanswer = np.array([1, 2])\n"))
        (with-temp-file (expand-file-name "pyproject.toml" root) (insert "[project]\nname = \"noema-audit\"\nversion = \"0.0.0\"\n"))
        (with-temp-buffer
          (setq buffer-file-name "/tmp/local-audit.ipynb"
                my/noema-jupyter-cell-kernel "project-live-test"
                my/noema-jupyter-cell-kernel-spec entry)
          (my/noema-jupyter-open-project-directory)
          (setq directory-buffer (current-buffer))
          (unless (derived-mode-p 'dired-mode) (error "Files did not open Dired")))
        (setq source-buffer (find-file-noselect source))
        (with-current-buffer source-buffer
          (unless (equal (remote-file-name-target buffer-file-name) target) (error "Source became local"))
          (goto-char (point-max)) (insert "# saved through normal Emacs APIs\n") (save-buffer))
        (with-temp-buffer
          (insert-file-contents source)
          (unless (string-match-p "saved through normal" (buffer-string)) (error "Remote save was lost")))
        (princ "PASS Dired, remote visit/read/save\n")
        (with-temp-buffer
          (setq buffer-file-name "/tmp/local-audit.ipynb"
                my/noema-jupyter-cell-kernel "project-live-test"
                my/noema-jupyter-cell-kernel-spec entry)
          (my/noema-jupyter-open-project-shell)
          (setq shell-buffer (current-buffer)))
        (with-current-buffer shell-buffer
          (let ((deadline (+ (float-time) 30)))
            (while (and my/noema-jupyter-project--shell-pending (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (let ((process (get-buffer-process shell-buffer)))
            (unless (process-live-p process) (error "Remote shell did not start"))
            (process-send-string process "printf '\\nNOEMA_PWD=%s\\nENVRC=%s\\n' \"$PWD\" \"$NOEMA_PROJECT_AUDIT\"\n")
            (let ((deadline (+ (float-time) 20)))
              (while (and (< (float-time) deadline)
                          (not (string-match-p (regexp-quote (concat "NOEMA_PWD=" (directory-file-name native))) (buffer-string))))
                (accept-process-output process 0.1)))
            (unless (string-match-p (regexp-quote (concat "NOEMA_PWD=" (directory-file-name native))) (buffer-string))
              (error "Wrong shell cwd: %s" (buffer-string)))
            (unless (string-match-p "ENVRC=remote-envrc" (buffer-string))
              (error "Shell lost project direnv: %s" (buffer-string)))))
        (princ "PASS shell starts on target at project root\n")
        (switch-to-buffer source-buffer)
        (setq-local lsp-auto-guess-root t lsp-guess-root-without-session t
                    my/language-server--manual-start t)
        (set-buffer-modified-p t)
        (my/language-server-ensure)
        (when (and (bound-and-true-p lsp--buffer-deferred) (fboundp 'lsp--init-if-visible)) (lsp--init-if-visible))
        (when (and (bound-and-true-p lsp--buffer-deferred) (fboundp 'lsp--init-if-visible))
          (lsp--init-if-visible))
        (unless (my/lsp-live-smoke--wait 45)
          (error "LSP failed in the source environment: %S" my/lsp-mode--waiting-for-direnv))
        (unless (equal (plist-get (my/language-server-current-toolchain-profile)
                                  :executable) python)
          (error "LSP analyzed the wrong Python: %S"
                 (my/language-server-current-toolchain-profile)))
        (goto-char (point-min)) (search-forward "np.array") (backward-char 2)
        (let ((hover (lsp-request "textDocument/hover" (lsp--text-document-position-params))))
          (unless (string-match-p "array" (format "%S" hover)) (error "Remote numpy hover missing: %S" hover))
          (princ "PASS remote LSP initialized and resolves numpy array using project Python\n"))
        (unless (equal (getenv "NOEMA_PROJECT_AUDIT") "remote-envrc")
          (error "LSP lost the source direnv environment"))
        (let ((default-directory user-emacs-directory))
          (with-temp-buffer
            (let ((status (process-file my/jupyter-board-python-command nil t nil
                                        (expand-file-name "test/jupyter-project-kernel-live.py" user-emacs-directory)
                                        (remote-target-label (remote-get-target target)) native python)))
              (princ (buffer-string))
              (unless (zerop status) (error "Real remote kernel failed"))))))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (dolist (workspace (lsp-workspaces)) (ignore-errors (my/lsp-mode-shutdown-workspace workspace 'live-smoke)))
        (set-buffer-modified-p nil))
      (kill-buffer source-buffer))
    (when (buffer-live-p shell-buffer)
      (when-let* ((process (get-buffer-process shell-buffer)))
        (set-process-query-on-exit-flag process nil) (delete-process process))
      (kill-buffer shell-buffer))
    (when (buffer-live-p directory-buffer) (kill-buffer directory-buffer))
    (let ((default-directory root) (remote-environment-inhibit t))
      (ignore-errors (process-file "direnv" nil nil nil "deny" ".")))
    (delete-directory root t)))
