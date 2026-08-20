;;; init-lsp-runtime.el --- Runtime contexts for language servers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language servers normally key one process by project and major mode.  That
;; is insufficient for buffers whose real execution environment is selected by
;; another runtime (a Jupyter kernel is the first consumer).  This module adds
;; a small provider layer which supplies a stable runtime identity, the owning
;; Remote context, and a language toolchain profile without changing the
;; ordinary `project-current' result outside Eglot.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(cl-defstruct (my/language-server-runtime
               (:constructor my/language-server-runtime-create))
  id label provider family context root tool-environment environment profile
  workspace-configuration idle-timeout metadata)

(cl-defstruct (my/language-server-runtime-fallback
               (:constructor my/language-server-runtime-fallback-create))
  reason expected)

(defun my/language-server-runtime-fallback-text (fallback)
  "Return the user-facing reason stored in FALLBACK.
Plain strings remain valid for providers whose fallback is unexpected."
  (if (my/language-server-runtime-fallback-p fallback)
      (my/language-server-runtime-fallback-reason fallback)
    fallback))

(defun my/language-server-runtime-report-fallback (fallback)
  "Report FALLBACK unless it is an expected provider limitation.
Expected fallbacks remain available to diagnostics through
`my/language-server-runtime-error'."
  (unless (and (my/language-server-runtime-fallback-p fallback)
               (my/language-server-runtime-fallback-expected fallback))
    (message "Language-server runtime fallback: %s"
             (my/language-server-runtime-fallback-text fallback))))

(defvar my/language-server-runtime-providers nil
  "Registered buffer runtime providers, ordered by descending priority.")

(defvar-local my/language-server-runtime-current nil
  "Resolved `my/language-server-runtime' for the current buffer.")

(defvar-local my/language-server-runtime-state 'idle
  "Runtime preparation state: idle, pending, ready, unsupported, or error.")

(defvar-local my/language-server-runtime-error nil
  "Structured reason for the current runtime fallback, when any.")

(defvar-local my/language-server-runtime--generation 0)
(defvar-local my/language-server-runtime--callbacks nil)
(defvar-local my/language-server-runtime--cleanup nil)

(defvar my/language-server-runtime--project-contexts
  (make-hash-table :test #'equal)
  "Runtime contexts keyed by their Eglot project objects.")

(defvar my/language-server-runtime--eglot-configurations
  (make-hash-table :test #'equal)
  "Effective workspace settings keyed by Eglot project object.")

(defvar my/language-server-runtime--eglot-idle-timers
  (make-hash-table :test #'eq)
  "Warm-shutdown timers keyed by Eglot server object.")

(defvar-local my/language-server-runtime--eglot-server nil)

(declare-function remote-context-workspace-root "remote-core" (context))
(declare-function eglot--current-project "eglot" ())
(declare-function eglot--managed-buffers "eglot" (server))
(declare-function eglot--project "eglot" (server))
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-shutdown
                  "eglot" (server &optional interactive timeout preserve-buffers))
(declare-function jsonrpc-running-p "jsonrpc" (connection))
(declare-function my/language-server--merge-values
                  "init-lsp" (base override))

(defun my/register-language-server-runtime-provider
    (name resolver &rest properties)
  "Register runtime provider NAME using RESOLVER.

RESOLVER is called as (RESOLVER BUFFER CALLBACK).  It returns nil when it does
not apply, a `my/language-server-runtime', the symbol `pending', or a plist
`(:unsupported REASON)'.  A pending resolver must eventually call CALLBACK as
(CALLBACK RUNTIME ERROR); it may return a cleanup function in the
`:cleanup-function' property stored by `my/language-server-runtime-prepare'.

PROPERTIES accepts `:priority', `:modes', `:source', and `:cleanup-function'."
  (let ((entry (append (list :name name :resolver resolver) properties)))
    (setq my/language-server-runtime-providers
          (sort
           (cons entry
                 (seq-remove
                  (lambda (known) (eq (plist-get known :name) name))
                  my/language-server-runtime-providers))
           (lambda (left right)
             (> (or (plist-get left :priority) 0)
                (or (plist-get right :priority) 0)))))
    entry))

(defun my/language-server-runtime--provider-applies-p (provider)
  "Return non-nil when PROVIDER applies to the current major mode."
  (let ((modes (plist-get provider :modes)))
    (or (null modes)
        (apply #'derived-mode-p (if (listp modes) modes (list modes))))))

(defun my/language-server-runtime--finish (generation runtime error &optional state)
  "Finish runtime GENERATION with RUNTIME or ERROR."
  (when (= generation my/language-server-runtime--generation)
    (setq my/language-server-runtime-current runtime
          my/language-server-runtime-error error
          my/language-server-runtime-state
          (or state
              (cond (runtime 'ready)
                ((and (my/language-server-runtime-fallback-p error)
                      (my/language-server-runtime-fallback-expected error))
                 'unsupported)
                (error 'error)
                (t 'unsupported))))
    (let ((callbacks (nreverse my/language-server-runtime--callbacks)))
      (setq my/language-server-runtime--callbacks nil
            my/language-server-runtime--cleanup nil)
      (dolist (callback callbacks)
        (when (functionp callback)
          (funcall callback runtime error))))))

(defun my/language-server-runtime-prepare (&optional callback force)
  "Prepare the current buffer's runtime context.

CALLBACK receives (RUNTIME ERROR) after preparation.  Return a runtime, the
symbol `pending', or nil when no provider applies.  With FORCE, invalidate the
current result first."
  (when force (my/language-server-runtime-invalidate))
  (when (functionp callback)
    (push callback my/language-server-runtime--callbacks))
  (pcase my/language-server-runtime-state
    ('ready
     (let ((runtime my/language-server-runtime-current))
       (when callback
         (setq my/language-server-runtime--callbacks
               (delq callback my/language-server-runtime--callbacks))
         (funcall callback runtime nil))
       runtime))
    ('pending 'pending)
    ((or 'unsupported 'error)
     (when callback
       (setq my/language-server-runtime--callbacks
             (delq callback my/language-server-runtime--callbacks))
       (funcall callback nil my/language-server-runtime-error))
     nil)
    (_
     (let ((providers
            (seq-filter #'my/language-server-runtime--provider-applies-p
                        my/language-server-runtime-providers))
           (generation (cl-incf my/language-server-runtime--generation))
           matched)
       (while (and providers (not matched))
         (let* ((provider (pop providers))
                (resolver (plist-get provider :resolver))
                (buffer (current-buffer))
                (done
                 (lambda (runtime error)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (my/language-server-runtime--finish
                        generation runtime error)))))
                (value (and (functionp resolver)
                            (funcall resolver buffer done))))
           (pcase value
             ((pred my/language-server-runtime-p)
              (setq matched t)
              (my/language-server-runtime--finish generation value nil))
             ('pending
              (setq matched t
                    my/language-server-runtime-state 'pending
                    my/language-server-runtime--cleanup
                    (plist-get provider :cleanup-function)))
             (`(:unsupported ,reason)
              (setq matched t
                    my/language-server-runtime-state 'unsupported
                    my/language-server-runtime-error reason)
              (my/language-server-runtime--finish
               generation nil reason 'unsupported)))))
       (unless matched
         (setq my/language-server-runtime-state 'unsupported
               my/language-server-runtime-error nil)
         (my/language-server-runtime--finish generation nil nil))
       (if (eq my/language-server-runtime-state 'pending)
           'pending
         my/language-server-runtime-current)))))

(defun my/language-server-runtime-invalidate ()
  "Forget the current buffer's runtime context and cancel preparation."
  (interactive)
  (cl-incf my/language-server-runtime--generation)
  (when (functionp my/language-server-runtime--cleanup)
    (ignore-errors (funcall my/language-server-runtime--cleanup)))
  (setq my/language-server-runtime-current nil
        my/language-server-runtime-state 'idle
        my/language-server-runtime-error nil
        my/language-server-runtime--callbacks nil
        my/language-server-runtime--cleanup nil))

(defun my/language-server-runtime-refresh ()
  "Refresh the current buffer's runtime context."
  (interactive)
  (my/language-server-runtime-prepare
   (lambda (_runtime error)
     (if error
         (my/language-server-runtime-report-fallback error)
       (message "Language-server runtime refreshed")))
   t))

(defun my/language-server-runtime-current-profile (&optional buffer)
  "Return BUFFER's authoritative runtime toolchain profile."
  (with-current-buffer (or buffer (current-buffer))
    (and (my/language-server-runtime-p my/language-server-runtime-current)
         (my/language-server-runtime-profile
          my/language-server-runtime-current))))

(defun my/language-server-runtime-description (&optional buffer)
  "Return a concise runtime description for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((my/language-server-runtime-p my/language-server-runtime-current)
      (or (my/language-server-runtime-label my/language-server-runtime-current)
          (my/language-server-runtime-id my/language-server-runtime-current)))
     ((eq my/language-server-runtime-state 'pending) "preparing")
     (my/language-server-runtime-error
      (format "fallback — %s"
              (my/language-server-runtime-fallback-text
               my/language-server-runtime-error)))
     (t "project default"))))

(defun my/language-server-runtime-project-object (&optional runtime)
  "Return the Eglot project object for RUNTIME or the current runtime."
  (when-let* ((runtime (or runtime my/language-server-runtime-current))
              ((my/language-server-runtime-p runtime))
              (root (my/language-server-runtime-root runtime))
              (id (my/language-server-runtime-id runtime)))
    (let ((project (list 'my/language-server-runtime-project root id)))
      (puthash project runtime my/language-server-runtime--project-contexts)
      project)))

(cl-defmethod project-root ((project (head my/language-server-runtime-project)))
  "Return the real workspace root for runtime PROJECT."
  (nth 1 project))

(defun my/language-server-runtime-project-context (project)
  "Return the runtime context registered for PROJECT."
  (gethash project my/language-server-runtime--project-contexts))

(defun my/language-server-runtime-register-eglot-configuration ()
  "Remember this buffer's effective settings for its exact Eglot project.

Eglot normally reconstructs workspace settings in a temporary buffer, which
cannot see runtime-selected buffer locals.  This registry preserves those
settings without changing ordinary dir-local behavior."
  (when (and (boundp 'eglot-workspace-configuration)
             (local-variable-p 'eglot-workspace-configuration)
             (require 'eglot nil t))
    (when-let* ((project (ignore-errors (eglot--current-project))))
      (puthash project eglot-workspace-configuration
               my/language-server-runtime--eglot-configurations))))

(defun my/language-server-runtime--eglot-configuration-a
    (fn server &optional path)
  "Merge registered runtime settings around FN for SERVER and PATH."
  (let ((configuration (funcall fn server path)))
    (if-let* ((registered
               (gethash (eglot--project server)
                        my/language-server-runtime--eglot-configurations)))
        (my/language-server--merge-values configuration registered)
      configuration)))

(defun my/language-server-runtime--cancel-idle-timer (server)
  "Cancel SERVER's pending warm-shutdown timer."
  (when-let* ((timer (gethash server
                              my/language-server-runtime--eglot-idle-timers)))
    (cancel-timer timer)
    (remhash server my/language-server-runtime--eglot-idle-timers)))

(defun my/language-server-runtime--shutdown-if-idle (server)
  "Shut down SERVER if it is still live and has no managed buffers."
  (remhash server my/language-server-runtime--eglot-idle-timers)
  (when (and server
             (ignore-errors (jsonrpc-running-p server))
             (null (ignore-errors (eglot--managed-buffers server))))
    (ignore-errors (eglot-shutdown server))))

(defun my/language-server-runtime--schedule-idle-shutdown (server timeout)
  "Keep SERVER warm for TIMEOUT seconds, then shut it down if unused."
  (when (and server (numberp timeout) (> timeout 0))
    (my/language-server-runtime--cancel-idle-timer server)
    (puthash
     server
     (run-at-time timeout nil
                  #'my/language-server-runtime--shutdown-if-idle server)
     my/language-server-runtime--eglot-idle-timers)))

(defun my/language-server-runtime--eglot-buffer-leaving-h ()
  "Arm warm shutdown before the current runtime buffer leaves Eglot."
  (when-let* ((runtime my/language-server-runtime-current)
              (timeout (my/language-server-runtime-idle-timeout runtime))
              (server (or my/language-server-runtime--eglot-server
                          (ignore-errors (eglot-current-server)))))
    (my/language-server-runtime--schedule-idle-shutdown server timeout)))

(defun my/language-server-runtime-eglot-managed-h ()
  "Apply runtime-specific lifecycle policy to an Eglot-managed buffer."
  (when (my/language-server-runtime-p my/language-server-runtime-current)
    (if (bound-and-true-p eglot--managed-mode)
        (when-let* ((server (ignore-errors (eglot-current-server))))
          (setq-local my/language-server-runtime--eglot-server server)
          ;; Runtime servers are explicitly warmed below; do not let Eglot's
          ;; last-buffer autoshutdown race the warm timer.
          (setq-local eglot-autoshutdown nil)
          (my/language-server-runtime--cancel-idle-timer server)
          (add-hook 'kill-buffer-hook
                    #'my/language-server-runtime--eglot-buffer-leaving-h
                    -90 t))
      (my/language-server-runtime--eglot-buffer-leaving-h))))

(defun my/language-server-runtime-eglot-shutdown-h (server)
  "Clear runtime state associated with stopped Eglot SERVER."
  (my/language-server-runtime--cancel-idle-timer server)
  (let ((project (ignore-errors (eglot--project server))))
    (when project
      (remhash project my/language-server-runtime--eglot-configurations)
      (remhash project my/language-server-runtime--project-contexts))))

(defun my/language-server-runtime--jsonrpc-receive-live-a
    (fn connection message)
  "Ignore a queued Eglot MESSAGE after CONNECTION has already stopped.

`jsonrpc--process-filter' dispatches decoded inbound messages through
zero-delay timers, but jsonrpc.el does not retain those timers for shutdown
cleanup.  A server can therefore exit before a queued server request is
handled, and the handler then tries to reply on a dead process."
  (let ((eglot-server-p
         (condition-case nil
             (progn (eglot--project connection) t)
           (error nil))))
    (unless (and eglot-server-p
                 (not (ignore-errors (jsonrpc-running-p connection))))
      (funcall fn connection message))))

(defun my/language-server-runtime--jsonrpc-send-live-a
    (fn connection &rest arguments)
  "Do not send ARGUMENTS through a stopped Eglot CONNECTION.

This is the final guard for inbound request timers compiled into jsonrpc.el:
some Emacs builds call the receive subr directly and bypass symbol advice,
but replies still cross the public send generic."
  (let ((eglot-server-p
         (condition-case nil
             (progn (eglot--project connection) t)
           (error nil))))
    (unless (and eglot-server-p
                 (not (ignore-errors (jsonrpc-running-p connection))))
      (apply fn connection arguments))))

(defun my/language-server-runtime--project-find (_directory)
  "Return a runtime-specific project while Eglot is finding its root."
  (when (and (bound-and-true-p eglot-lsp-context)
             (my/language-server-runtime-p my/language-server-runtime-current))
    (my/language-server-runtime-project-object)))

(defun my/language-server-runtime--eglot-current-project-a (fn)
  "Return the runtime project around Eglot project lookup FN.

This explicit Eglot boundary is authoritative even when Projectile or another
package later prepends its own finder to `project-find-functions'."
  (or (and (my/language-server-runtime-p
            my/language-server-runtime-current)
           (my/language-server-runtime-project-object))
      (funcall fn)))

(defun my/language-server-runtime-install-project-finder ()
  "Install the runtime project finder before general project backends."
  (setq project-find-functions
        (cons #'my/language-server-runtime--project-find
              (remove #'my/language-server-runtime--project-find
                      project-find-functions))))

(with-eval-after-load 'project
  (my/language-server-runtime-install-project-finder))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            #'my/language-server-runtime-eglot-managed-h)
  (unless (advice-member-p
           #'my/language-server-runtime--eglot-current-project-a
           'eglot--current-project)
    (advice-add 'eglot--current-project :around
                #'my/language-server-runtime--eglot-current-project-a))
  (unless (advice-member-p
           #'my/language-server-runtime--eglot-configuration-a
           'eglot--workspace-configuration-plist)
    (advice-add 'eglot--workspace-configuration-plist :around
                #'my/language-server-runtime--eglot-configuration-a))
  (unless (advice-member-p
           #'my/language-server-runtime-eglot-shutdown-h
           'eglot--on-shutdown)
    (advice-add 'eglot--on-shutdown :after
                #'my/language-server-runtime-eglot-shutdown-h))
  (unless (advice-member-p
           #'my/language-server-runtime--jsonrpc-receive-live-a
           'jsonrpc-connection-receive)
    (advice-add 'jsonrpc-connection-receive :around
                #'my/language-server-runtime--jsonrpc-receive-live-a))
  (unless (advice-member-p
           #'my/language-server-runtime--jsonrpc-send-live-a
           'jsonrpc-connection-send)
    (advice-add 'jsonrpc-connection-send :around
                #'my/language-server-runtime--jsonrpc-send-live-a)))

(add-hook 'kill-buffer-hook #'my/language-server-runtime-invalidate)

(provide 'init-lsp-runtime)
;;; init-lsp-runtime.el ends here
