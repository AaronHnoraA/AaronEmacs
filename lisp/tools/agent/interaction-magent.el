;;; tools/agent/interaction-magent.el --- Magent runtime integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is the +agent-interaction control plane.  The embedded Magent runtime owns
;; queueing, durable sessions, cancellation, lifecycle events, and API agent
;; execution.  Structured external CLI samplers plug into the same runtime.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tools/agent/interaction-backend)
(require 'tools/agent/interaction-output)
(require 'tools/agent/interaction-session)

(declare-function magent-runtime-ensure-initialized "magent-runtime" ())
(declare-function magent-runtime-session-current "magent-runtime-api" (&optional scope))
(declare-function magent-runtime-submit "magent-runtime-api" (runtime-session prompt &rest args))
(declare-function magent-runtime-cancel "magent-runtime-api" (runtime-session))
(declare-function magent-runtime-session-clear "magent-runtime-api" (runtime-session))
(declare-function magent-runtime-pending-count "magent-runtime-api" (&optional runtime-session))
(declare-function magent-runtime-queue-session-busy-p "magent-runtime-queue" (session))
(declare-function magent-runtime-session-magent-session "magent-runtime-api" (runtime-session))
(declare-function magent-agent-result-content-string "magent-protocol" (result))
(declare-function +agent-interaction-magent-cli-sampler "tools/agent/interaction-magent-cli"
                  (engine root runtime-session))
(declare-function magent-start "magent-agent-shell" ())

(defvar +agent-interaction-magent--runtime-sessions (make-hash-table :test #'equal)
  "Magent runtime sessions keyed by canonical project roots.")

(defcustom +agent-interaction-magent-max-prompt-bytes (* 2 1024 1024)
  "Maximum user/profile prompt size accepted by the Magent bridge."
  :type 'integer
  :group '+agent-interaction)

(defcustom +agent-interaction-magent-max-pending-per-project 32
  "Maximum queued turns retained for one project runtime session."
  :type 'integer
  :group '+agent-interaction)

(defun +agent-interaction-magent--root (root)
  "Return canonical directory form of ROOT."
  (file-name-as-directory (file-truename (expand-file-name root))))

(defun +agent-interaction-magent--load ()
  "Load the embedded Magent runtime and initialize it once."
  (require 'magent)
  (require 'magent-runtime-api)
  (require 'magent-runtime-queue)
  (require 'magent-protocol)
  (magent-runtime-ensure-initialized))

(defun +agent-interaction-magent-runtime-session (&optional project-root)
  "Return the Magent runtime session for PROJECT-ROOT, creating it lazily."
  (+agent-interaction-magent--load)
  (let* ((root (+agent-interaction-magent--root
                (or project-root (+agent-interaction-project-root))))
         (cached (gethash root +agent-interaction-magent--runtime-sessions)))
    (or cached
        (let ((runtime (magent-runtime-session-current root)))
          (puthash root runtime +agent-interaction-magent--runtime-sessions)
          runtime))))

(defun +agent-interaction-magent-session-live-p (&optional project-root)
  "Return non-nil when PROJECT-ROOT has a Magent-owned runtime session."
  (let* ((root (+agent-interaction-magent--root
                (or project-root (+agent-interaction-project-root))))
         (runtime (gethash root +agent-interaction-magent--runtime-sessions)))
    (and runtime t)))

(defun +agent-interaction-magent-busy-p (&optional project-root)
  "Return non-nil when PROJECT-ROOT is active or queued in Magent."
  (when-let* ((runtime (gethash
                        (+agent-interaction-magent--root
                         (or project-root (+agent-interaction-project-root)))
                        +agent-interaction-magent--runtime-sessions)))
    (or (> (magent-runtime-pending-count runtime) 0)
        (magent-runtime-queue-session-busy-p
         (magent-runtime-session-magent-session runtime)))))

(defun +agent-interaction-magent--observer (backend root)
  "Return a bounded UI observer for BACKEND at ROOT."
  (let ((stream-marker nil)
        (reasoning-active nil))
    (lambda (event)
      (pcase (plist-get event :type)
        ('turn-start
         (setq stream-marker
               (+agent-interaction-output-stream-start
                'answer
                (format "backend: %s\nproject: %s"
                        backend (abbreviate-file-name root))
                root)))
        ('assistant-delta
         (unless stream-marker
           (setq stream-marker
                 (+agent-interaction-output-stream-start 'answer nil root)))
         (setq reasoning-active nil)
         (+agent-interaction-output-stream-append stream-marker
                                            (or (plist-get event :text) "")))
        ('reasoning-delta
         ;; Keep reasoning out of the transcript, but retain an inexpensive
         ;; state signal instead of appending every private reasoning token.
         (unless reasoning-active
           (setq reasoning-active t)
           (+agent-interaction-session-set-last-status
            (format "%s reasoning" backend) root)))
        ('tool-call-start
         (+agent-interaction-output-append
          'tool
          (format "%s: %s"
                  (or (plist-get event :name) "tool")
                  (or (plist-get event :summary) "running"))
          root))
        ((or 'turn-complete 'turn-failed 'turn-cancelled)
         (when stream-marker
           (+agent-interaction-output-stream-finish stream-marker)
           (setq stream-marker nil)))))))

(cl-defun +agent-interaction-magent-submit
    (backend prompt &optional project-root on-success on-error)
  "Submit PROMPT through Magent using BACKEND for PROJECT-ROOT.
API uses Magent's native gptel sampler.  CLI backends use their structured
native protocols while retaining their own tools and permissions."
  (let* ((root (+agent-interaction-magent--root
                (or project-root (+agent-interaction-project-root))))
         (runtime (+agent-interaction-magent-runtime-session root))
         (sampler (unless (eq backend 'api)
                    (require 'tools/agent/interaction-magent-cli)
                    (+agent-interaction-magent-cli-sampler backend root runtime)))
         (observer (+agent-interaction-magent--observer backend root)))
    (when (> (string-bytes prompt) +agent-interaction-magent-max-prompt-bytes)
      (user-error "Prompt exceeds +agent-interaction's %d-byte limit"
                  +agent-interaction-magent-max-prompt-bytes))
    (when (>= (magent-runtime-pending-count runtime)
              +agent-interaction-magent-max-pending-per-project)
      (user-error "Magent queue is full for %s" (abbreviate-file-name root)))
    (magent-runtime-submit
     runtime prompt
     :sampler sampler
     :observer observer
     :turn-metadata (list :+agent-interaction-backend backend)
     :on-complete
     (lambda (status result)
       (pcase status
         ('completed
          (+agent-interaction-session-mark-profile-bootstrap-sent backend root)
          (+agent-interaction-session-mark-profile-injected backend root)
          (+agent-interaction-session-set-last-status
           (format "%s turn completed" backend) root)
          (+agent-interaction-output-append 'status
                                      (format "%s turn completed" backend) root)
          (when on-success (funcall on-success)))
         ('cancelled
          (+agent-interaction-session-set-last-status
           (format "%s turn cancelled" backend) root))
         (_
          (let ((message (magent-agent-result-content-string result)))
            (+agent-interaction-session-set-last-error message root)
            (+agent-interaction-session-set-last-status
             (format "%s turn failed" backend) root)
            (+agent-interaction-output-append 'error message root)
            (when on-error (funcall on-error message)))))))
    runtime))

(defun +agent-interaction-magent-cancel (&optional project-root)
  "Cancel active and queued Magent work for PROJECT-ROOT."
  (interactive)
  (when-let* ((runtime (gethash
                        (+agent-interaction-magent--root
                         (or project-root (+agent-interaction-project-root)))
                        +agent-interaction-magent--runtime-sessions)))
    (magent-runtime-cancel runtime)))

(defun +agent-interaction-magent-clear (&optional project-root)
  "Cancel and clear Magent state for PROJECT-ROOT."
  (interactive)
  (let* ((root (+agent-interaction-magent--root
                (or project-root (+agent-interaction-project-root))))
         (runtime (gethash root +agent-interaction-magent--runtime-sessions)))
    (when runtime
      (magent-runtime-session-clear runtime)
      (remhash root +agent-interaction-magent--runtime-sessions))
    t))

(defun +agent-interaction-magent-open (&optional backend project-root)
  "Open the Magent UI for BACKEND at PROJECT-ROOT."
  (interactive)
  (let* ((root (+agent-interaction-magent--root
                (or project-root (+agent-interaction-project-root))))
         (engine (or backend (+agent-interaction-session-backend root))))
    (+agent-interaction-magent-runtime-session root)
    (let ((default-directory root))
      (if (eq engine 'api)
          (magent-start)
        (pop-to-buffer (+agent-interaction-output-buffer root))))))

(defun +agent-interaction-magent--api-available-p ()
  "Return non-nil when the embedded API runtime dependencies are visible."
  (and (locate-library "magent") (locate-library "gptel")))

(+agent-interaction-register-backend
 'api
 :label "API · Magent/gptel"
 :generation '+agent-interaction-magent-api-v1
 :capabilities '(:session :send :draft :stop :cancel :headless)
 :authority '(:kind magent-native :sandboxed t)
 :operations
 (list
  :available-p #'+agent-interaction-magent--api-available-p
  :live-p #'+agent-interaction-magent-session-live-p
  :ensure #'+agent-interaction-magent-runtime-session
  :open (lambda (root) (+agent-interaction-magent-open 'api root))
  :send (lambda (prompt root on-success on-error)
          (+agent-interaction-magent-submit 'api prompt root on-success on-error))
  :draft (lambda (_prompt _root _on-success _on-error) t)
  :stop #'+agent-interaction-magent-cancel
  :cancel #'+agent-interaction-magent-cancel))

(provide 'tools/agent/interaction-magent)
;;; tools/agent/interaction-magent.el ends here
