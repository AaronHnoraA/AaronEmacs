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
(require 'remote-background)
(require 'remote-fs)
(require 'remote-accelerator)

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

(defun remote-framework--connection-closed
    (_connection route _reason)
  "Invalidate observations which belonged to the closed session ROUTE."
  (let ((target-id (remote-route-target-id route)))
    (remote-background-invalidate-target target-id)
    (when (fboundp 'remote-backend-contract-clear)
      (remote-backend-contract-clear route))
    (when (fboundp 'remote-accelerator-clear-route)
      (remote-accelerator-clear-route route))
    (when (fboundp 'remote-fs-clear-target-cache)
      (remote-fs-clear-target-cache target-id))
    (when (fboundp 'remote-path-invalidate)
      (remote-path-invalidate target-id))
    (when (fboundp 'remote-environment-invalidate)
      (remote-environment-invalidate target-id))))

(add-hook 'remote-connection-closed-hook
          #'remote-framework--connection-closed)

(defun remote-framework-bootstrap ()
  "Idempotently register built-in backends and adapters."
  (remote-transport-register-builtins)
  (remote-backend-register-builtins)
  (remote-framework-register-adapters)
  (remote-accelerator-register-builtins))

(defun remote-framework-reset ()
  "Reset volatile framework state and restore built-in registrations.
Persisted targets are intentionally not reloaded; configuration ownership
  belongs to `remote-config'."
  (interactive)
  (when (fboundp 'remote-workspace-clear)
    (remote-workspace-clear 'framework-reset))
  (when (fboundp 'remote-background-clear)
    (remote-background-clear 'framework-reset))
  (when (fboundp 'remote-file-watch-clear)
    (remote-file-watch-clear 'framework-reset))
  (when (fboundp 'remote-service-clear)
    (remote-service-clear 'framework-reset))
  (when (fboundp 'remote-channel-clear)
    (remote-channel-clear))
  (when (fboundp 'remote-connection-pool-clear)
    (remote-connection-pool-clear t))
  (remote-pipeline-runtime-clear 'framework-reset)
  (when (boundp 'remote-fs-path-expansion-cache)
    (remote-fs-clear-target-cache))
  (when (boundp 'remote-accelerator-probe-cache)
    (clrhash remote-accelerator-probe-cache))
  (when (fboundp 'remote-backend-contract-clear)
    (remote-backend-contract-clear))
  (remote-reset-registries)
  (clrhash remote-transports)
  (clrhash remote-backends)
  (remote-framework-bootstrap))

(remote-framework-register-adapters)

(provide 'remote-framework)
;;; remote-framework.el ends here
