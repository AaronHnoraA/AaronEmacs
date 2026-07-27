;;; remote-framework.el --- Public logical remote development API -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is the library entry point.  It loads identities and routing first,
;; then pipelines, backend/session lifecycle, file/process/channel boundaries,
;; and finally environment services.  UI and persistent configuration remain
;; optional integration modules.

;;; Code:

(require 'remote-core)
(require 'remote-pipeline)
(require 'remote-transport)
(require 'remote-backend)
(require 'remote-connection)
(require 'remote-session)
(require 'remote-fs)

(remote-backend-register-builtins)

(require 'remote-process)
(require 'remote-channel)
(require 'remote-environment)
(require 'remote-path)
(require 'remote-service)
(require 'remote-workspace)
(require 'remote-terminal)
(require 'remote-doctor)

(defun remote-framework-register-adapters ()
  "Register the framework's built-in caller adapters.
This function is idempotent and is useful after
`remote-reset-registries'."
  (remote-register-adapter
   "emacs-file"
   :capabilities '(file-read file-write directory metadata
                   process-sync process-async watch environment)
   :preferences '((default . ("native" "tramp" "tramp-rpc"))))
  (remote-register-adapter
   "process"
   :capabilities '(process-sync process-async pty environment)
   :preferences '((default . ("native" "tramp-rpc" "tramp"))))
  (remote-register-adapter
   "exec"
   :capabilities '(process-sync process-async environment)
   :preferences '((default . ("tramp-rpc" "tramp" "native"))))
  (remote-register-adapter
   "environment"
   :capabilities '(process-sync environment)
   :preferences '((default . ("tramp-rpc" "tramp" "native"))))
  (remote-channel-register-adapter))

(defun remote-framework-bootstrap ()
  "Idempotently register built-in backends and adapters."
  (remote-transport-register-builtins)
  (remote-backend-register-builtins)
  (remote-framework-register-adapters))

(defun remote-framework-reset ()
  "Reset volatile framework state and restore built-in registrations.
Persisted targets are intentionally not reloaded; configuration ownership
  belongs to `remote-config'."
  (interactive)
  (when (fboundp 'remote-workspace-clear)
    (remote-workspace-clear 'framework-reset))
  (when (fboundp 'remote-service-clear)
    (remote-service-clear 'framework-reset))
  (when (fboundp 'remote-channel-clear)
    (remote-channel-clear))
  (when (fboundp 'remote-connection-pool-clear)
    (remote-connection-pool-clear t))
  (remote-pipeline-runtime-clear 'framework-reset)
  (when (boundp 'remote-fs-path-expansion-cache)
    (clrhash remote-fs-path-expansion-cache))
  (remote-reset-registries)
  (clrhash remote-transports)
  (clrhash remote-backends)
  (remote-framework-bootstrap))

(remote-framework-register-adapters)

(provide 'remote-framework)
;;; remote-framework.el ends here
