;;; tools/direnv/config.el --- Routed project environment integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Compatibility and UI hooks for the in-tree implementation in
;; `lisp/tools/direnv/direnv.el'.  No third-party direnv.el package is used.

;;; Code:

(require 'config)
(require 'tools/direnv/direnv)

(config-defvar +enable-direnv nil
  "Whether to enable routed `direnv-mode' automatically."
  :type 'boolean
  :group 'environment)

(defvar +direnv-subprocess-sync-inhibited nil)
(defvar +direnv--compile-advice-installed nil)

(defun +direnv-update-environment-maybe (&optional path callback)
  "Refresh the current buffer's environment for PATH.
With CALLBACK, perform slow export work asynchronously and call CALLBACK with
the environment and an error.  Without CALLBACK, preserve the synchronous
contract required by commands which are about to start a subprocess."
  (when +enable-direnv
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

(defun +direnv-schedule-current-buffer ()
  "Schedule non-blocking direnv synchronization for the current buffer."
  (when +enable-direnv
    (direnv--maybe-update-environment)))

(defun +direnv--sync-before-subprocess (orig-fn &rest args)
  "Refresh the routed environment before calling ORIG-FN with ARGS."
  (unless +direnv-subprocess-sync-inhibited
    (+direnv-update-environment-maybe default-directory))
  (apply orig-fn args))

(defun +direnv--settle-env-after-dir-locals ()
  "Schedule an environment rebuild after directory-local variables.
Environment providers may run direnv, Nix, or a remote process, so this hook
must never block file visiting.  Consumers which require a ready environment
before process startup use `+direnv-update-environment-maybe' explicitly."
  (when (and +enable-direnv
             (bound-and-true-p dir-local-variables-alist))
    (if (direnv--transport-busy-p)
        (direnv--maybe-update-environment)
      (if (direnv--envrc-root)
          (direnv--maybe-update-environment)
        (direnv-clear-environment)))))

(when +enable-direnv
  (add-hook 'find-file-hook #'+direnv-schedule-current-buffer)
  ;; Dired buffers have no `buffer-file-name', so `find-file-hook' never
  ;; observes them.  Refresh from their established `default-directory'
  ;; when Dired initializes the buffer.
  (add-hook 'dired-mode-hook #'+direnv-schedule-current-buffer)
  (add-hook 'hack-local-variables-hook
            #'+direnv--settle-env-after-dir-locals)
  (unless +direnv--compile-advice-installed
    (advice-add 'compile :around #'+direnv--sync-before-subprocess)
    (setq +direnv--compile-advice-installed t))
  (direnv-mode 1))

(provide 'tools/direnv/config)
;;; tools/direnv/config.el ends here
