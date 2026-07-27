;;; init-direnv.el --- Routed project environment integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Compatibility and UI hooks for the in-tree implementation in
;; `lisp/direnv.el'.  No third-party direnv.el package is used.

;;; Code:

(require 'config)
(require 'direnv)

(config-defvar my/enable-direnv nil
  "Whether to enable routed `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(defvar my/direnv-subprocess-sync-inhibited nil)
(defvar my/direnv--compile-advice-installed nil)

(defun my/direnv-update-environment-maybe (&optional path callback)
  "Refresh the current buffer's environment for PATH.
With CALLBACK, perform slow export work asynchronously and call CALLBACK with
the environment and an error.  Without CALLBACK, preserve the synchronous
contract required by commands which are about to start a subprocess."
  (when my/enable-direnv
    (if callback
        (direnv-environment-ensure-async path callback)
      (if (direnv--transport-busy-p)
          (progn (direnv--maybe-update-environment) 'pending)
        (if (direnv--envrc-root path)
            (progn
              (direnv-update-directory-environment
               (direnv--directory path) nil)
              'ready)
          (direnv-clear-environment)
          'ready)))))

(defun my/direnv-schedule-current-buffer ()
  "Schedule non-blocking direnv synchronization for the current buffer."
  (when my/enable-direnv
    (direnv--maybe-update-environment)))

(defun my/direnv--sync-before-subprocess (orig-fn &rest args)
  "Refresh the routed environment before calling ORIG-FN with ARGS."
  (unless my/direnv-subprocess-sync-inhibited
    (my/direnv-update-environment-maybe default-directory))
  (apply orig-fn args))

(defun my/direnv--settle-env-after-dir-locals ()
  "Schedule an environment rebuild after directory-local variables.
Environment providers may run direnv, Nix, or a remote process, so this hook
must never block file visiting.  Consumers which require a ready environment
before process startup use `my/direnv-update-environment-maybe' explicitly."
  (when (and my/enable-direnv
             (bound-and-true-p dir-local-variables-alist))
    (if (direnv--transport-busy-p)
        (direnv--maybe-update-environment)
      (if (direnv--envrc-root)
          (direnv--maybe-update-environment)
        (direnv-clear-environment)))))

(when my/enable-direnv
  (add-hook 'find-file-hook #'my/direnv-schedule-current-buffer)
  ;; Dired buffers have no `buffer-file-name', so `find-file-hook' never
  ;; observes them.  Refresh from their established `default-directory'
  ;; when Dired initializes the buffer.
  (add-hook 'dired-mode-hook #'my/direnv-schedule-current-buffer)
  (add-hook 'hack-local-variables-hook
            #'my/direnv--settle-env-after-dir-locals)
  (unless my/direnv--compile-advice-installed
    (advice-add 'compile :around #'my/direnv--sync-before-subprocess)
    (setq my/direnv--compile-advice-installed t))
  (direnv-mode 1))

(provide 'init-direnv)
;;; init-direnv.el ends here
