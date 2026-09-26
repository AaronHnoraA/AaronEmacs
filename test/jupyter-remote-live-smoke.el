;;; jupyter-remote-live-smoke.el --- Opt-in real Jupyter routing -*- lexical-binding: t; -*-
;; JUPYTER_LIVE_TARGET=aaron-pc JUPYTER_LIVE_ROOT=/tmp/... make jupyter-live-smoke
;; ROOT must contain an isolated venv with ipykernel/Jupyter Server, runtime/
;; and work/. A running test server's jpserver-*.json must be in runtime/.
;; This probe never persists target environment/profile changes.
(require 'init-aaronnote)
(require 'init-aaronnote-jupyter-server)
(require 'remote-config)
(require 'remote-framework)

(defun my/jupyter-live--client (payload)
  "Run the Node client with secret PAYLOAD on stdin, servicing Emacs channels."
  (let* ((buffer (generate-new-buffer " *jupyter-live-client*"))
         (default-directory user-emacs-directory)
         (process-environment (remote-client-process-environment))
         (proc (make-process
                :name "jupyter-live-client" :buffer buffer :connection-type 'pipe
                :command (list (or (getenv "JUPYTER_LIVE_NODE") (executable-find "node"))
                               (expand-file-name "test/jupyter-remote-live-client.mjs" user-emacs-directory))
                :noquery t))
         (deadline (+ (float-time) 75)))
    (unwind-protect
        (progn
          (process-send-string proc (json-serialize payload))
          (process-send-eof proc)
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (unless (and (not (process-live-p proc)) (= (process-exit-status proc) 0))
            (error "Jupyter client failed: %s" (with-current-buffer buffer (buffer-string))))
          (princ (with-current-buffer buffer (buffer-string))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buffer))))

(defun my/jupyter-live-run ()
  "Exercise broker launch, channel recovery, attach and HTTP server ownership."
  (remote-config-load)
  (remote-fs-install)
  (let* ((target-id (or (getenv "JUPYTER_LIVE_TARGET") (error "Set JUPYTER_LIVE_TARGET")))
         (root (or (getenv "JUPYTER_LIVE_ROOT") (error "Set JUPYTER_LIVE_ROOT")))
         (target (or (remote-get-target target-id) (error "Unknown target")))
         (node (or (getenv "JUPYTER_LIVE_NODE") (executable-find "node")))
         (ssh (getenv "JUPYTER_LIVE_SSH"))
         (exec-path (if ssh (cons (file-name-directory ssh) exec-path) exec-path))
         (remote--client-exec-path exec-path)
         (process-environment (copy-sequence process-environment))
         (remote--client-process-environment process-environment)
         (old-environment (remote-target-environment target))
         (old-workspaces (remote-target-workspaces target))
         (my/noema-jupyter-server--credentials (make-hash-table :test #'equal))
         (file (remote-make-file-name target-id (concat root "/work/audit.ipynb")))
         runtime attachment workspace)
    (when ssh (setenv "PATH" (concat (file-name-directory ssh) ":" (getenv "PATH"))))
    (setenv "JUPYTER_LIVE_NODE" node)
    (setf (remote-target-environment target)
          `((vars . (("PATH" . ,(concat root "/venv/bin:/usr/local/bin:/usr/bin:/bin"))
                     ("JUPYTER_RUNTIME_DIR" . ,(concat root "/runtime"))))
            (providers . ()))
          (remote-target-workspaces target)
          `(((id . "jupyter-audit") (path . ,(concat root "/work/")))))
    (unwind-protect
        (progn
          (setq runtime (my/noema-jupyter--launch-runtime
                         `((kernelId . "live-audit") (sourceFile . ,file) (kernelName . "python3")))
                workspace (my/noema-jupyter-runtime-workspace runtime))
          (my/jupyter-live--client
           `((mode . "raw") (label . "broker launch + completion")
             (connectionInfo . ,(my/noema-jupyter-runtime-client-connection runtime))
             (code . "noema_audit_state = 41; print(noema_audit_state + 1)") (expected . "42")))
          (let* ((old (my/noema-jupyter-runtime-channel-group runtime))
                 (ports (remote-channel-group-endpoints old 'local))
                 (replacement (remote-channel-group-recover old)))
            (my/noema-jupyter--update-group runtime old replacement)
            (unless (equal ports (remote-channel-group-endpoints replacement 'local))
              (error "Recovery changed client endpoints")))
          (my/jupyter-live--client
           `((mode . "raw") (label . "five-port recovery preserves state")
             (connectionInfo . ,(my/noema-jupyter-runtime-client-connection runtime))
             (code . "print(noema_audit_state + 1)") (expected . "42")))
          ;; Discovery expects connection files in Jupyter's runtime directory.
          (let ((copy (remote-make-file-name target-id (concat root "/runtime/kernel-audit.json"))))
            (copy-file (my/noema-jupyter-runtime-connection-file runtime) copy t)
            (unwind-protect
                (progn
                  (setq attachment (my/noema-jupyter--attach-runtime
                                    `((sourceFile . ,file) (token . "kernel-audit.json"))))
                  (my/jupyter-live--client
                   `((mode . "raw") (label . "remote connection-file attach")
                     (connectionInfo . ,(my/noema-jupyter-attachment-client-connection attachment))
                     (code . "print(noema_audit_state + 1)") (expected . "42")))
                  (my/noema-jupyter--close-attachment attachment)
                  (unless (eq t (my/noema-jupyter--alive-p runtime))
                    (error "Detaching killed the owning kernel")))
              (delete-file copy)))
          (let* ((runtime-dir (remote-make-file-name target-id (concat root "/runtime/")))
                 (info-file (car (directory-files runtime-dir t "\\`jpserver-.*\\.json\\'")))
                 (info (json-parse-string (with-temp-buffer (insert-file-contents info-file) (buffer-string))
                                          :object-type 'alist))
                 (entry `(:id "live-audit" :url ,(alist-get 'url info) :target ,target-id :auth token)))
            (puthash (my/noema-jupyter-server--credential-key entry) (alist-get 'token info)
                     my/noema-jupyter-server--credentials)
            (unwind-protect
                (my/jupyter-live--client
                 `((mode . "server") (label . "server auth, contents, execution, adopt, restart, shutdown")
                   (server . ,(my/noema-jupyter-server--resolve-entry entry))))
              (my/noema-jupyter-server--close-resource entry "live-audit" 'test-cleanup)))
          (princ "PASS real Remote / Jupyter integration\n"))
      (when attachment (ignore-errors (my/noema-jupyter--close-attachment attachment)))
      (when runtime (ignore-errors (my/noema-jupyter--shutdown-runtime runtime)))
      (when workspace (ignore-errors (remote-workspace-close workspace 'test-cleanup)))
      (setf (remote-target-environment target) old-environment
            (remote-target-workspaces target) old-workspaces))))

(my/jupyter-live-run)
