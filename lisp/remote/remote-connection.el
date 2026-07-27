;;; remote-connection.el --- Persistent routed connection pool -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A physical link can expose multiple backend sessions.  Pool entries are
;; therefore keyed by target/link/backend, never by adapter or capability.
;; TRAMP and tramp-rpc retain ownership of their actual processes; this layer
;; owns routing identity, liveness, reuse, and invalidation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)
(require 'remote-transport)

(declare-function remote-context "remote-fs" (&optional path))

(define-error 'remote-connection-timeout
              "Remote connection establishment timed out"
              'remote-transport-error)
(define-error 'remote-connection-busy
              "Remote connection establishment is already in progress")
(define-error 'remote-connection-cancelled
              "Remote connection establishment was cancelled")

(defcustom remote-connection-open-timeout 8
  "Maximum seconds allowed to establish a pooled backend connection.
This deadline is enforced at the framework boundary, so a backend cannot
leave Emacs waiting indefinitely even when its own transport timeout is
missing or ineffective.  Nil or a non-positive value disables the deadline."
  :type '(choice (const :tag "No framework deadline" nil)
                 (number :tag "Seconds"))
  :group 'remote)

;; Version-1 callers used "connection" for the cached backend attachment now
;; represented by `remote-session'.  Preserve its layout and vocabulary for
;; already compiled callers, but keep one real session object and registry.
(defvaralias 'cl-struct-remote-connection-tags
  'cl-struct-remote-session-tags)

(cl-defstruct (remote-session
               (:constructor remote-session-create))
  key target-id pipeline-id backend-id pipeline-runtime handle state
  opened-at last-used-at use-count error)

(defalias 'remote-connection-p #'remote-session-p)
(cl-defun remote-connection-create
    (&key key target-id pipeline-id backend-id link-id plugin-id
          pipeline-runtime handle state opened-at last-used-at use-count error)
  "Create a session using v2 names or v1 connection keywords."
  (remote-session-create
   :key key
   :target-id target-id
   :pipeline-id (or pipeline-id link-id)
   :backend-id (or backend-id plugin-id)
   :pipeline-runtime pipeline-runtime
   :handle handle
   :state state
   :opened-at opened-at
   :last-used-at last-used-at
   :use-count use-count
   :error error))
(defalias 'remote-connection-key #'remote-session-key)
(defalias 'remote-connection-target-id #'remote-session-target-id)
(defalias 'remote-connection-link-id #'remote-session-pipeline-id)
(defalias 'remote-connection-plugin-id #'remote-session-backend-id)
(defalias 'remote-connection-pipeline-runtime
  #'remote-session-pipeline-runtime)
(defalias 'remote-connection-handle #'remote-session-handle)
(defalias 'remote-connection-state #'remote-session-state)
(defalias 'remote-connection-opened-at #'remote-session-opened-at)
(defalias 'remote-connection-last-used-at #'remote-session-last-used-at)
(defalias 'remote-connection-use-count #'remote-session-use-count)
(defalias 'remote-connection-error #'remote-session-error)
(gv-define-setter remote-connection-key (value object)
  `(setf (remote-session-key ,object) ,value))
(gv-define-setter remote-connection-target-id (value object)
  `(setf (remote-session-target-id ,object) ,value))
(gv-define-setter remote-connection-link-id (value object)
  `(setf (remote-session-pipeline-id ,object) ,value))
(gv-define-setter remote-connection-plugin-id (value object)
  `(setf (remote-session-backend-id ,object) ,value))
(gv-define-setter remote-connection-pipeline-runtime (value object)
  `(setf (remote-session-pipeline-runtime ,object) ,value))
(gv-define-setter remote-connection-handle (value object)
  `(setf (remote-session-handle ,object) ,value))
(gv-define-setter remote-connection-state (value object)
  `(setf (remote-session-state ,object) ,value))
(gv-define-setter remote-connection-opened-at (value object)
  `(setf (remote-session-opened-at ,object) ,value))
(gv-define-setter remote-connection-last-used-at (value object)
  `(setf (remote-session-last-used-at ,object) ,value))
(gv-define-setter remote-connection-use-count (value object)
  `(setf (remote-session-use-count ,object) ,value))
(gv-define-setter remote-connection-error (value object)
  `(setf (remote-session-error ,object) ,value))

(defvaralias 'remote-connection-pool 'remote-sessions)

(defvar remote-sessions (make-hash-table :test #'equal)
  "Live routed sessions keyed by target/pipeline/backend.")

(defvaralias 'remote-current-connection 'remote-current-session)

(defvar remote-current-session nil
  "Dynamically bound pooled session for the current routed operation.")

(defun remote-connection-route-key (route)
  "Return the stable pool key for ROUTE."
  (list (remote-route-target-id route)
        (remote-route-link-id route)
        (remote-route-link-plugin-id route)))

(defun remote-connection-cached-p (route)
  "Return non-nil when ROUTE has a pooled session.
This is a pure hash lookup used by route scoring; liveness is checked when the
session is acquired."
  (and (remote-route-p route)
       (gethash (remote-connection-route-key route)
                remote-connection-pool)))

(defun remote-connection--take-pipeline-runtime (connection)
  "Detach and return CONNECTION's owned pipeline runtime.
Taking ownership before releasing the runtime makes cancellation idempotent:
an opener and an invalidator may both resume after the same event-loop yield,
but only one of them can observe a non-nil owned reference."
  (when (remote-connection-p connection)
    (prog1
        (remote-connection-pipeline-runtime connection)
      (setf (remote-connection-pipeline-runtime connection) nil))))

(defun remote-connection--live-p (connection route context)
  "Return whether CONNECTION remains usable for ROUTE and CONTEXT."
  (and
   (eq (remote-connection-state connection) 'open)
   (remote-pipeline-runtime-live-p
    (remote-connection-pipeline-runtime connection))
   (let* ((plugin (remote-route-plugin route))
          (predicate
           (and plugin
                (remote-link-plugin-connection-live-p plugin))))
     (if predicate
         (condition-case nil
             (funcall predicate connection route context)
           (error nil))
       t))))

(defun remote-connection--open-backend
    (opener route context pipeline-runtime)
  "Call OPENER for ROUTE within the framework connection deadline.
CONTEXT and PIPELINE-RUNTIME are dynamically visible to the backend."
  (let ((remote-current-pipeline-runtime pipeline-runtime))
    (if (and (numberp remote-connection-open-timeout)
             (> remote-connection-open-timeout 0))
        (with-timeout
            (remote-connection-open-timeout
             (signal
              'remote-connection-timeout
              (list
               (format
                "Connection to %s via %s/%s exceeded %.1fs"
                (remote-route-target-id route)
                (remote-route-link-id route)
                (remote-route-link-plugin-id route)
                remote-connection-open-timeout))))
          (if opener
              (funcall opener route context)
            t))
      (if opener
          (funcall opener route context)
        t))))

(defun remote-connection-ensure (route &optional context)
  "Return an open pooled connection for ROUTE and CONTEXT.
The backend's connect function performs the initial handshake.  Subsequent
requests only validate and reuse the retained session."
  (unless (remote-route-p route)
    (error "Connection pool needs a resolved route: %S" route))
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((stringp context) (remote-context context))
           (t (remote-context))))
         (key (remote-connection-route-key route))
         (existing (gethash key remote-connection-pool)))
    (cond
     ((and existing
           (eq (remote-connection-state existing) 'opening))
      ;; Backend connection functions may run timers and package hooks.
      ;; Returning the half-built session lets a nested operation use a nil
      ;; handle; opening a replacement leaks the first handle and one shared
      ;; pipeline reference.  Fail explicitly so the caller can defer.
      (remote-log
       'connection-busy
       :target (remote-route-target-id route)
       :link (remote-route-link-id route)
       :plugin (remote-route-link-plugin-id route))
      (signal
       'remote-connection-busy
       (list
        (format "Connection to %s via %s/%s is already opening"
                (remote-route-target-id route)
                (remote-route-link-id route)
                (remote-route-link-plugin-id route)))))
     ((and existing
           (remote-connection--live-p existing route context))
      (setf (remote-session-last-used-at existing) (current-time)
            (remote-session-use-count existing)
            (1+ (remote-session-use-count existing)))
      (remote-log
       'connection-reuse
       :target (remote-route-target-id route)
       :link (remote-route-link-id route)
       :plugin (remote-route-link-plugin-id route)
       :uses (remote-connection-use-count existing))
      existing)
     (t
      (when existing
        (remhash key remote-connection-pool)
        (let* ((plugin (remote-route-plugin route))
               (closer
                (and plugin
                     (remote-link-plugin-disconnect plugin))))
          (when closer
            (ignore-errors
              (funcall closer existing route))))
        (remote-pipeline-release
         (remote-connection-pipeline-runtime existing)
         nil 'stale))
      (let* ((opened-at (current-time))
             (connection
             (remote-connection-create
               :key key
               :target-id (remote-route-target-id route)
               :link-id (remote-route-link-id route)
               :plugin-id (remote-route-link-plugin-id route)
               :state 'opening
               :opened-at opened-at
               :last-used-at opened-at
               :use-count 0))
             pipeline-runtime
             plugin
             closer
             handle
             backend-opened
             pending-runtime)
        ;; Claim the key before either transport or backend startup.  Both
        ;; boundaries can yield to Emacs and re-enter this function.
        (puthash key connection remote-connection-pool)
        (condition-case err
            (progn
              ;; The acquire result belongs to this stack frame until it is
              ;; installed on the session.  Invalidation can run while a
              ;; transport stage is opening, before the session has any
              ;; runtime to release.
              (setq pending-runtime
                    (remote-pipeline-acquire route context))
              (unless
                  (and
                   (eq (gethash key remote-connection-pool) connection)
                   (eq (remote-connection-state connection) 'opening))
                (remote-pipeline-release
                 pending-runtime nil 'connection-opening-cancelled)
                (setq pending-runtime nil)
                (signal
                 'remote-connection-cancelled
                 (list
                  (format "Connection to %s via %s/%s was cancelled"
                          (remote-route-target-id route)
                          (remote-route-link-id route)
                          (remote-route-link-plugin-id route)))))
              (setf
               (remote-connection-pipeline-runtime connection)
               pending-runtime)
              ;; Ownership has moved to CONNECTION.  Every later cleanup path
              ;; takes the field before releasing it.
              (setq pipeline-runtime pending-runtime
                    pending-runtime nil)
              (setq plugin (remote-route-plugin route)
                    closer
                    (and plugin
                         (remote-link-plugin-disconnect plugin))
                    handle
                    (remote-connection--open-backend
                     (and plugin
                          (remote-link-plugin-connect plugin))
                     route context pipeline-runtime)
                    backend-opened t)
              (setf (remote-connection-handle connection) handle)
              ;; Pool clear, config reload, or explicit invalidation can run
              ;; while the opener yields.  A cancelled placeholder must never
              ;; be resurrected as an unpooled live session.
              (unless
                  (and
                   (eq (gethash key remote-connection-pool) connection)
                   (eq (remote-connection-state connection) 'opening))
                (when closer
                  (ignore-errors
                    (funcall closer connection route)))
                (setq backend-opened nil)
                ;; Invalidation takes this field before releasing it.  If a
                ;; caller merely replaced the pool entry, the opener still
                ;; takes and releases the reference here.
                (when-let* ((owned
                             (remote-connection--take-pipeline-runtime
                              connection)))
                  (remote-pipeline-release
                   owned nil 'connection-opening-cancelled))
                (setq pipeline-runtime nil)
                (signal
                 'remote-connection-cancelled
                 (list
                  (format "Connection to %s via %s/%s was cancelled"
                          (remote-route-target-id route)
                          (remote-route-link-id route)
                          (remote-route-link-plugin-id route)))))
              (setf
               (remote-connection-state connection) 'open
               (remote-connection-last-used-at connection) (current-time)
               (remote-connection-use-count connection) 1)
              (remote-log
               'connection-open
               :target (remote-route-target-id route)
               :link (remote-route-link-id route)
               :plugin (remote-route-link-plugin-id route))
              connection)
          (error
           (when (eq (gethash key remote-connection-pool) connection)
             (remhash key remote-connection-pool))
           (when (and backend-opened closer)
             (ignore-errors
               (funcall closer connection route)))
           (setf (remote-connection-state connection) 'failed
                 (remote-connection-error connection) err)
           (when pending-runtime
             (remote-pipeline-release pending-runtime nil err)
             (setq pending-runtime nil))
           (when-let* ((owned
                        (remote-connection--take-pipeline-runtime
                         connection)))
             (remote-pipeline-release owned nil err))
           (remote-log
            'connection-error
            :target (remote-route-target-id route)
            :link (remote-route-link-id route)
            :plugin (remote-route-link-plugin-id route)
            :error (error-message-string err))
           (signal (car err) (cdr err)))))))))

(defalias 'remote-connection-acquire #'remote-connection-ensure)

(defun remote-connection-invalidate
    (route &optional disconnect reason)
  "Remove ROUTE's pool entry.
When DISCONNECT is non-nil, ask the backend to close its retained process.
REASON is recorded for observability."
  (when (remote-route-p route)
    (let* ((key (remote-connection-route-key route))
           (connection (gethash key remote-connection-pool))
           (plugin (remote-route-plugin route))
           (closer (and plugin
                        (remote-link-plugin-disconnect plugin)))
           (opening
            (and connection
                 (eq (remote-connection-state connection) 'opening))))
      (when connection
        (remhash key remote-connection-pool)
        (let ((pipeline-runtime
               (remote-connection--take-pipeline-runtime connection)))
          (setf (remote-session-state connection) 'closed
                (remote-session-error connection) reason)
        ;; An opener which is currently yielding still owns any handle it
        ;; returns.  It observes the cancelled placeholder and closes that
        ;; handle itself; calling a backend closer now would receive nil.
          (when (and disconnect closer (not opening))
            (ignore-errors
              (funcall closer connection route)))
          (remote-pipeline-release pipeline-runtime nil reason))
        (remote-log
         'connection-close
         :target (remote-route-target-id route)
         :link (remote-route-link-id route)
         :plugin (remote-route-link-plugin-id route)
         :reason
         (cond
          ((null reason) nil)
          ((symbolp reason) (symbol-name reason))
          (t (error-message-string reason)))))
      connection)))

(defun remote-connection-invalidate-link
    (link-id &optional disconnect reason)
  "Remove all backend sessions belonging to LINK-ID."
  (let (routes)
    (maphash
     (lambda (_key connection)
       (when (equal (remote-connection-link-id connection) link-id)
         (push
          (remote-route-create
           :target-id (remote-connection-target-id connection)
           :link-id link-id
           :link-plugin-id (remote-connection-plugin-id connection))
          routes)))
     remote-connection-pool)
    (dolist (route routes)
      (remote-connection-invalidate route disconnect reason))
    (length routes)))

(defun remote-connection-prune ()
  "Close pool entries whose configured pipeline or backend disappeared."
  (let (routes)
    (maphash
     (lambda (_key connection)
       (let ((link (remote-get-link
                    (remote-connection-link-id connection))))
         (unless
             (and link
                  (member (remote-connection-plugin-id connection)
                          (remote-link-plugin-ids link)))
           (push
            (remote-route-create
             :target-id (remote-connection-target-id connection)
             :pipeline-id (remote-connection-link-id connection)
             :backend-id (remote-connection-plugin-id connection))
            routes))))
     remote-connection-pool)
    (dolist (route routes)
      ;; `remote-connection-invalidate' still releases the shared pipeline
      ;; runtime when the removed backend is no longer registered.
      (remote-connection-invalidate route t 'configuration-removed))
    (length routes)))

(defun remote-connection-pool-clear (&optional disconnect)
  "Clear every pooled session, optionally asking backends to DISCONNECT."
  (interactive "P")
  (let (routes)
    (maphash
     (lambda (_key connection)
       (push
        (remote-route-create
         :target-id (remote-connection-target-id connection)
         :link-id (remote-connection-link-id connection)
         :link-plugin-id (remote-connection-plugin-id connection))
        routes))
     remote-connection-pool)
    (dolist (route routes)
      (remote-connection-invalidate route disconnect 'pool-clear))
    (length routes)))

(defun remote-connection-warm
    (&optional context adapter capability constraints)
  "Resolve and establish one pooled session for CONTEXT.
ADAPTER defaults to `process'; CAPABILITY defaults to `process-sync'."
  (interactive)
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((stringp context) (remote-context context))
           (t (remote-context))))
         (route
          (remote-resolve
           (or adapter "process")
           (or capability 'process-sync)
           context constraints)))
    (remote-connection-ensure route context)))

(defun remote-connection-pool-status ()
  "Return a stable summary of pooled routed sessions."
  (let (result)
    (maphash
     (lambda (_key connection)
       (push
        (list
         :target (remote-connection-target-id connection)
         :link (remote-connection-link-id connection)
         :plugin (remote-connection-plugin-id connection)
         :pipeline-state
         (when-let* ((runtime
                      (remote-connection-pipeline-runtime connection)))
           (remote-pipeline-runtime-state runtime))
         :stages
         (when-let* ((runtime
                      (remote-connection-pipeline-runtime connection)))
           (mapcar
            #'remote-stage-runtime-transport-id
            (remote-pipeline-runtime-stages runtime)))
         :state (remote-connection-state connection)
         :uses (remote-connection-use-count connection)
         :opened-at (remote-connection-opened-at connection)
         :last-used-at (remote-connection-last-used-at connection))
        result))
     remote-connection-pool)
    (sort result
          (lambda (left right)
            (string-lessp
             (format "%s/%s"
                     (plist-get left :link)
                     (plist-get left :plugin))
             (format "%s/%s"
                     (plist-get right :link)
                     (plist-get right :plugin)))))))

(add-hook 'remote-config-after-load-hook #'remote-connection-prune)

(provide 'remote-connection)
;;; remote-connection.el ends here
