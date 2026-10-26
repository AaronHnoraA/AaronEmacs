;;; tools/lsp/runtime.el --- Runtime contexts for language servers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language servers normally key one process by project and major mode.  That
;; is insufficient for buffers whose real execution environment is selected by
;; another runtime (a Jupyter kernel is the first consumer).  This module adds
;; a small provider layer which supplies a stable runtime identity, the owning
;; Remote context, and a language toolchain profile without changing the
;; ordinary `project-current' result outside the language server's own root
;; resolution.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(cl-defstruct (+language-server-runtime
               (:constructor +language-server-runtime-create))
  id label provider family context root tool-environment environment profile
  workspace-configuration idle-timeout metadata)

(cl-defstruct (+language-server-runtime-fallback
               (:constructor +language-server-runtime-fallback-create))
  reason expected)

(defun +language-server-runtime-fallback-text (fallback)
  "Return the user-facing reason stored in FALLBACK.
Plain strings remain valid for providers whose fallback is unexpected."
  (if (+language-server-runtime-fallback-p fallback)
      (+language-server-runtime-fallback-reason fallback)
    fallback))

(defun +language-server-runtime-report-fallback (fallback)
  "Report FALLBACK unless it is an expected provider limitation.
Expected fallbacks remain available to diagnostics through
`+language-server-runtime-error'."
  (unless (and (+language-server-runtime-fallback-p fallback)
               (+language-server-runtime-fallback-expected fallback))
    (message "Language-server runtime fallback: %s"
             (+language-server-runtime-fallback-text fallback))))

(defvar +language-server-runtime-providers nil
  "Registered buffer runtime providers, ordered by descending priority.")

(defvar-local +language-server-runtime-current nil
  "Resolved `+language-server-runtime' for the current buffer.")

(defvar-local +language-server-runtime-required nil
  "Non-nil when a provider forbids starting with the ordinary local runtime.
Providers must reset this when the buffer no longer needs their runtime.")

(defvar-local +language-server-runtime-state 'idle
  "Runtime preparation state: idle, pending, ready, unsupported, or error.")

(defvar-local +language-server-runtime-error nil
  "Structured reason for the current runtime fallback, when any.")

(defvar-local +language-server-runtime--generation 0)
(defvar-local +language-server-runtime--callbacks nil)
(defvar-local +language-server-runtime--cleanup nil)

(defvar +language-server-runtime--project-contexts
  (make-hash-table :test #'equal)
  "Runtime contexts keyed by their synthetic project objects.")

(defvar +language-server-runtime--configurations
  (make-hash-table :test #'equal)
  "Effective workspace settings keyed by runtime project object.")

(defvar +language-server-runtime--idle-timers
  (make-hash-table :test #'eq)
  "Warm-shutdown timers keyed by lsp-mode workspace object.")

(defvar-local +language-server-runtime--workspace nil
  "The lsp-mode workspace this runtime buffer is attached to.")

(defvar lsp--cur-workspace)

(defconst +language-server-runtime--workspace-metadata-key
  '+language-server-runtime-id
  "lsp-mode workspace metadata key containing the owning runtime ID.")

(defvar +language-server-runtime--resolving-root nil
  "Non-nil while lsp-mode is calculating a workspace root.
It scopes the runtime project finder to root resolution so `project-current',
Projectile and ordinary file navigation keep seeing the real project.")

(declare-function remote-context-workspace-root "remote-core" (context))
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp--workspace-client "lsp-mode" (workspace))
(declare-function lsp--workspace-metadata "lsp-mode" (workspace))
(declare-function lsp--workspace-root "lsp-mode" (workspace))
(declare-function lsp--client-server-id "lsp-mode" (client))
(declare-function lsp-session-folder->servers "lsp-mode" (session))
(declare-function lsp--open-in-workspace "lsp-mode" (workspace))
(declare-function lsp-workspaces "lsp-mode" ())
(declare-function +lsp-mode-shutdown-workspace "tools/lsp/config" (workspace &optional reason))
(declare-function +language-server-set-workspace-configuration
                  "tools/lsp/config" (configuration))
(declare-function +language-server--merge-values
                  "tools/lsp/config" (base override))

(defun set-language-server-runtime-provider!
    (name resolver &rest properties)
  "Register runtime provider NAME using RESOLVER.

RESOLVER is called as (RESOLVER BUFFER CALLBACK).  It returns nil when it does
not apply, a `+language-server-runtime', the symbol `pending', or a plist
`(:unsupported REASON)'.  A pending resolver must eventually call CALLBACK as
(CALLBACK RUNTIME ERROR); it may return a cleanup function in the
`:cleanup-function' property stored by `+language-server-runtime-prepare'.

PROPERTIES accepts `:priority', `:modes', `:source', and `:cleanup-function'."
  (let ((entry (append (list :name name :resolver resolver) properties)))
    (setq +language-server-runtime-providers
          (sort
           (cons entry
                 (seq-remove
                  (lambda (known) (eq (plist-get known :name) name))
                  +language-server-runtime-providers))
           (lambda (left right)
             (> (or (plist-get left :priority) 0)
                (or (plist-get right :priority) 0)))))
    entry))

(defun +language-server-runtime--provider-applies-p (provider)
  "Return non-nil when PROVIDER applies to the current major mode."
  (let ((modes (plist-get provider :modes)))
    (or (null modes)
        (apply #'derived-mode-p (if (listp modes) modes (list modes))))))

(defun +language-server-runtime--finish (generation runtime error &optional state)
  "Finish runtime GENERATION with RUNTIME or ERROR."
  (when (= generation +language-server-runtime--generation)
    (setq +language-server-runtime-current runtime
          +language-server-runtime-error error
          +language-server-runtime-state
          (or state
              (cond (runtime 'ready)
                ((and (+language-server-runtime-fallback-p error)
                      (+language-server-runtime-fallback-expected error))
                 'unsupported)
                (error 'error)
                (t 'unsupported))))
    (let ((callbacks (nreverse +language-server-runtime--callbacks)))
      (setq +language-server-runtime--callbacks nil
            +language-server-runtime--cleanup nil)
      (dolist (callback callbacks)
        (when (functionp callback)
          (funcall callback runtime error))))))

(defun +language-server-runtime-prepare (&optional callback force)
  "Prepare the current buffer's runtime context.

CALLBACK receives (RUNTIME ERROR) after preparation.  Return a runtime, the
symbol `pending', or nil when no provider applies.  With FORCE, invalidate the
current result first."
  (when force (+language-server/runtime-invalidate))
  (when (functionp callback)
    (push callback +language-server-runtime--callbacks))
  (pcase +language-server-runtime-state
    ('ready
     (let ((runtime +language-server-runtime-current))
       (when callback
         (setq +language-server-runtime--callbacks
               (delq callback +language-server-runtime--callbacks))
         (funcall callback runtime nil))
       runtime))
    ('pending 'pending)
    ((or 'unsupported 'error)
     (when callback
       (setq +language-server-runtime--callbacks
             (delq callback +language-server-runtime--callbacks))
       (funcall callback nil +language-server-runtime-error))
     nil)
    (_
     (let ((providers
            (seq-filter #'+language-server-runtime--provider-applies-p
                        +language-server-runtime-providers))
           (generation (cl-incf +language-server-runtime--generation))
           matched)
       (while (and providers (not matched))
         (let* ((provider (pop providers))
                (resolver (plist-get provider :resolver))
                (buffer (current-buffer))
                (done
                 (lambda (runtime error)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (+language-server-runtime--finish
                        generation runtime error)))))
                (value (and (functionp resolver)
                            (funcall resolver buffer done))))
           (pcase value
             ((pred +language-server-runtime-p)
              (setq matched t)
              (+language-server-runtime--finish generation value nil))
             ('pending
              (setq matched t)
              ;; A cached target probe may invoke DONE before its resolver
              ;; returns.  In that case `--finish' already installed the
              ;; result; marking it pending here would strand the header at
              ;; "preparing" and suppress the language server.
              (when (and (= generation +language-server-runtime--generation)
                         (eq +language-server-runtime-state 'idle))
                (setq +language-server-runtime-state 'pending
                      +language-server-runtime--cleanup
                      (plist-get provider :cleanup-function))))
             (`(:unsupported ,reason)
              (setq matched t
                    +language-server-runtime-state 'unsupported
                    +language-server-runtime-error reason)
              (+language-server-runtime--finish
               generation nil reason 'unsupported)))))
       (unless matched
         (setq +language-server-runtime-state 'unsupported
               +language-server-runtime-error nil)
         (+language-server-runtime--finish generation nil nil))
       (if (eq +language-server-runtime-state 'pending)
           'pending
         +language-server-runtime-current)))))

(defun +language-server/runtime-invalidate ()
  "Forget the current buffer's runtime context and cancel preparation."
  (interactive)
  (cl-incf +language-server-runtime--generation)
  (when (functionp +language-server-runtime--cleanup)
    (ignore-errors (funcall +language-server-runtime--cleanup)))
  (setq +language-server-runtime-current nil
        +language-server-runtime-state 'idle
        +language-server-runtime-error nil
        +language-server-runtime--callbacks nil
        +language-server-runtime--cleanup nil))

(defun +language-server/runtime-refresh ()
  "Refresh the current buffer's runtime context."
  (interactive)
  (+language-server-runtime-prepare
   (lambda (_runtime error)
     (if error
         (+language-server-runtime-report-fallback error)
       (message "Language-server runtime refreshed")))
   t))

(defun +language-server-runtime-current-profile (&optional buffer)
  "Return BUFFER's authoritative runtime toolchain profile."
  (with-current-buffer (or buffer (current-buffer))
    (and (+language-server-runtime-p +language-server-runtime-current)
         (+language-server-runtime-profile
          +language-server-runtime-current))))

(defun +language-server-runtime-description (&optional buffer)
  "Return a concise runtime description for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((+language-server-runtime-p +language-server-runtime-current)
      (or (+language-server-runtime-label +language-server-runtime-current)
          (+language-server-runtime-id +language-server-runtime-current)))
     ((eq +language-server-runtime-state 'pending) "preparing")
     (+language-server-runtime-error
      (format "%s — %s" (if +language-server-runtime-required "unavailable" "fallback")
              (+language-server-runtime-fallback-text
               +language-server-runtime-error)))
     (t "project default"))))

(defun +language-server-runtime-project-object (&optional runtime)
  "Return the synthetic project object for RUNTIME or the current runtime."
  (when-let* ((runtime (or runtime +language-server-runtime-current))
              ((+language-server-runtime-p runtime))
              (root (+language-server-runtime-root runtime))
              (id (+language-server-runtime-id runtime)))
    (let ((project (list '+language-server-runtime-project root id)))
      (puthash project runtime +language-server-runtime--project-contexts)
      project)))

(cl-defmethod project-root ((project (head +language-server-runtime-project)))
  "Return the real workspace root for runtime PROJECT."
  (nth 1 project))

(defun +language-server-runtime-project-context (project)
  "Return the runtime context registered for PROJECT."
  (gethash project +language-server-runtime--project-contexts))

(defun +language-server-runtime-current-id (&optional runtime)
  "Return RUNTIME's stable ID, or that of the current buffer runtime."
  (when-let* ((runtime (or runtime +language-server-runtime-current))
              ((+language-server-runtime-p runtime)))
    (+language-server-runtime-id runtime)))

(defun +language-server-runtime-workspace-id (workspace)
  "Return the runtime ID recorded on lsp-mode WORKSPACE, or nil.
Nil deliberately denotes an ordinary project workspace.  This distinction
prevents a non-Jupyter Python buffer from attaching to a warm kernel-specific
server merely because both use the same root and client ID."
  (when-let* ((metadata (ignore-errors
                          (lsp--workspace-metadata workspace))))
    (gethash +language-server-runtime--workspace-metadata-key metadata)))

(defun +language-server-runtime--tag-current-workspace-h ()
  "Record the current buffer's runtime identity on `lsp--cur-workspace'.
The metadata is client-side only: the real logical root sent to the server,
its URI projection, Remote owner, process placement and environment remain
unchanged."
  (when-let* ((workspace (and (boundp 'lsp--cur-workspace)
                              lsp--cur-workspace))
              (metadata (ignore-errors
                          (lsp--workspace-metadata workspace))))
    (if-let* ((runtime-id (+language-server-runtime-current-id)))
        (puthash +language-server-runtime--workspace-metadata-key
                 runtime-id metadata)
      (remhash +language-server-runtime--workspace-metadata-key metadata))))

(defun +language-server-runtime--same-root-p (left right)
  "Return non-nil when logical directory names LEFT and RIGHT are equal."
  (and (stringp left)
       (stringp right)
       (equal (file-name-as-directory left)
              (file-name-as-directory right))))

(defun +language-server-runtime--find-workspace-a
    (fn session client project-root)
  "Find only the lsp-mode workspace for this buffer's runtime around FN.

Upstream keys workspaces by PROJECT-ROOT and CLIENT.  Runtime-controlled
buffers need the Eglot-era third identity component as well: the stable kernel
runtime ID.  Multiple matching workspaces may therefore remain registered
under the same real logical root, while only the one with the current runtime
ID is opened.  Ordinary project buffers match only untagged workspaces."
  (if (not (and (fboundp 'lsp-session-folder->servers)
                (fboundp 'lsp--workspace-client)
                (fboundp 'lsp--client-server-id)
                (fboundp 'lsp--open-in-workspace)))
      (funcall fn session client project-root)
    (let* ((runtime-id (+language-server-runtime-current-id))
           (server-id (lsp--client-server-id client))
           (workspaces
            (gethash project-root
                     (lsp-session-folder->servers session)))
           (workspace
            (seq-find
             (lambda (candidate)
               (and
                (eq (lsp--client-server-id
                     (lsp--workspace-client candidate))
                    server-id)
                (equal
                 (+language-server-runtime-workspace-id candidate)
                 runtime-id)))
             workspaces)))
      (when workspace
        (lsp--open-in-workspace workspace)
        workspace))))

(defun +language-server-runtime-configuration-for-workspace (workspace)
  "Return the registered runtime configuration owned by WORKSPACE."
  (when-let* ((runtime-id
               (+language-server-runtime-workspace-id workspace))
              (root (ignore-errors (lsp--workspace-root workspace))))
    (catch 'configuration
      (maphash
       (lambda (project configuration)
         (when (and (eq (car-safe project)
                        '+language-server-runtime-project)
                    (equal (nth 2 project) runtime-id)
                    (+language-server-runtime--same-root-p
                     (nth 1 project) root))
           (throw 'configuration configuration)))
       +language-server-runtime--configurations)
      nil)))

(defun +language-server-runtime-register-lsp-configuration ()
  "Remember this buffer's effective settings for its exact runtime project.

lsp-mode answers `workspace/configuration' from the workspace rather than
from the requesting buffer, so a runtime-selected buffer-local value would
otherwise be invisible.  Registering it per runtime project preserves those
settings without changing ordinary dir-local behavior."
  (when-let* ((configuration +language-server--workspace-configuration)
              (project (+language-server-runtime-project-object)))
    (puthash project configuration
             +language-server-runtime--configurations)))

(defun +language-server-runtime-configuration (project)
  "Return the registered runtime workspace configuration for PROJECT."
  (gethash project +language-server-runtime--configurations))

(defun +language-server-runtime--cancel-idle-timer (workspace)
  "Cancel WORKSPACE's pending warm-shutdown timer."
  (when-let* ((timer (gethash workspace
                              +language-server-runtime--idle-timers)))
    (cancel-timer timer)
    (remhash workspace +language-server-runtime--idle-timers)))

(defun +language-server-runtime--shutdown-if-idle (workspace)
  "Shut down WORKSPACE if it is still live and has no managed buffers."
  (remhash workspace +language-server-runtime--idle-timers)
  (when (and workspace
             (null (seq-filter
                    #'buffer-live-p
                    (ignore-errors (lsp--workspace-buffers workspace)))))
    (ignore-errors
      (+lsp-mode-shutdown-workspace workspace 'runtime-idle))))

(defun +language-server-runtime--schedule-idle-shutdown (workspace timeout)
  "Keep WORKSPACE warm for TIMEOUT seconds, then shut it down if unused."
  (when (and workspace (numberp timeout) (> timeout 0))
    (+language-server-runtime--cancel-idle-timer workspace)
    (puthash
     workspace
     (run-at-time timeout nil
                  #'+language-server-runtime--shutdown-if-idle workspace)
     +language-server-runtime--idle-timers)))

(defun +language-server-runtime--buffer-leaving-h ()
  "Arm warm shutdown before the current runtime buffer leaves its workspace."
  (when-let* ((runtime +language-server-runtime-current)
              (timeout (+language-server-runtime-idle-timeout runtime))
              (workspace (or +language-server-runtime--workspace
                             (car (ignore-errors (lsp-workspaces))))))
    (+language-server-runtime--schedule-idle-shutdown workspace timeout)))

(defun +language-server-runtime-managed-h ()
  "Apply runtime-specific lifecycle policy to an lsp-mode-managed buffer."
  (when (+language-server-runtime-p +language-server-runtime-current)
    (if (bound-and-true-p lsp-managed-mode)
        (when-let* ((workspace (car (ignore-errors (lsp-workspaces)))))
          ;; Reassert the tag when attaching to a warm workspace.  New
          ;; workspaces receive it earlier from `lsp-before-initialize-hook',
          ;; before any asynchronous initialization callback can race another
          ;; runtime buffer at the same root.
          (let ((lsp--cur-workspace workspace))
            (+language-server-runtime--tag-current-workspace-h))
          (setq-local +language-server-runtime--workspace workspace)
          ;; Runtime servers are explicitly warmed below; do not let lsp-mode's
          ;; last-buffer teardown race the warm timer.
          (setq-local lsp-keep-workspace-alive t)
          (+language-server-runtime--cancel-idle-timer workspace)
          (add-hook 'kill-buffer-hook
                    #'+language-server-runtime--buffer-leaving-h
                    -90 t))
      (+language-server-runtime--buffer-leaving-h))))

(defun +language-server-runtime-uninitialized-h (workspace)
  "Clear runtime state associated with stopped lsp-mode WORKSPACE."
  (+language-server-runtime--cancel-idle-timer workspace)
  (when-let* ((runtime-id
               (+language-server-runtime-workspace-id workspace))
              (root (ignore-errors (lsp--workspace-root workspace))))
    (maphash
     (lambda (project _runtime)
       (when (and (eq (car-safe project)
                      '+language-server-runtime-project)
                  (equal (nth 2 project) runtime-id)
                  (+language-server-runtime--same-root-p
                   (nth 1 project) root))
         (remhash project +language-server-runtime--configurations)
         (remhash project +language-server-runtime--project-contexts)))
     (copy-hash-table +language-server-runtime--project-contexts))))

(defun +language-server-runtime--project-find (_directory)
  "Return a runtime-specific project while lsp-mode is finding its root."
  (when (and +language-server-runtime--resolving-root
             (+language-server-runtime-p +language-server-runtime-current))
    (+language-server-runtime-project-object)))

(defun +language-server-runtime--calculate-root-a (fn session file-name)
  "Return the runtime root around lsp-mode root lookup FN.

This explicit boundary is authoritative even when Projectile or another
package later prepends its own finder to `project-find-functions'.  SESSION
and FILE-NAME are lsp-mode's own arguments."
  (or (and (+language-server-runtime-p
            +language-server-runtime-current)
           (+language-server-runtime-root
            +language-server-runtime-current))
      (let ((+language-server-runtime--resolving-root t))
        (funcall fn session file-name))))

(defun +language-server-runtime-install-project-finder ()
  "Install the runtime project finder before general project backends."
  (setq project-find-functions
        (cons #'+language-server-runtime--project-find
              (remove #'+language-server-runtime--project-find
                      project-find-functions))))

(with-eval-after-load 'project
  (+language-server-runtime-install-project-finder))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-before-initialize-hook
            #'+language-server-runtime--tag-current-workspace-h)
  (add-hook 'lsp-managed-mode-hook
            #'+language-server-runtime-managed-h)
  (add-hook 'lsp-after-uninitialized-functions
            #'+language-server-runtime-uninitialized-h)
  (unless (advice-member-p
           #'+language-server-runtime--calculate-root-a
           'lsp--calculate-root)
    (advice-add 'lsp--calculate-root :around
                #'+language-server-runtime--calculate-root-a))
  (unless (advice-member-p
           #'+language-server-runtime--find-workspace-a
           'lsp--find-workspace)
    (advice-add 'lsp--find-workspace :around
                #'+language-server-runtime--find-workspace-a)))

(add-hook 'kill-buffer-hook #'+language-server/runtime-invalidate)

(provide 'tools/lsp/runtime)
;;; tools/lsp/runtime.el ends here
