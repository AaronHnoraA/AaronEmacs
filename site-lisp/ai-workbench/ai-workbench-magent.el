;;; ai-workbench-magent.el --- Magent runtime integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is the ai-workbench control plane.  The embedded Magent runtime owns
;; queueing, durable sessions, cancellation, lifecycle events, and API agent
;; execution.  Structured external CLI samplers plug into the same runtime.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-workbench-backend)
(require 'ai-workbench-output)
(require 'ai-workbench-session)

(declare-function magent-runtime-ensure-initialized "magent-runtime" ())
(declare-function magent-runtime-session-current "magent-runtime-api" (&optional scope))
(declare-function magent-runtime-submit "magent-runtime-api" (runtime-session prompt &rest args))
(declare-function magent-runtime-cancel "magent-runtime-api" (runtime-session))
(declare-function magent-runtime-session-clear "magent-runtime-api" (runtime-session))
(declare-function magent-runtime-pending-count "magent-runtime-api" (&optional runtime-session))
(declare-function magent-runtime-queue-session-busy-p "magent-runtime-queue" (session))
(declare-function magent-runtime-session-magent-session "magent-runtime-api" (runtime-session))
(declare-function magent-agent-result-content-string "magent-protocol" (result))
(declare-function ai-workbench-magent-cli-sampler "ai-workbench-magent-cli"
                  (engine root runtime-session))
(declare-function magent-start "magent-agent-shell" ())

(defvar ai-workbench-magent--runtime-sessions (make-hash-table :test #'equal)
  "Magent runtime sessions keyed by canonical project roots.")

(defcustom ai-workbench-magent-max-prompt-bytes (* 2 1024 1024)
  "Maximum user/profile prompt size accepted by the Magent bridge."
  :type 'integer
  :group 'ai-workbench)

(defcustom ai-workbench-magent-max-pending-per-project 32
  "Maximum queued turns retained for one project runtime session."
  :type 'integer
  :group 'ai-workbench)

(defun ai-workbench-magent--root (root)
  "Return canonical directory form of ROOT."
  (file-name-as-directory (file-truename (expand-file-name root))))

(defun ai-workbench-magent--load ()
  "Load the embedded Magent runtime and initialize it once."
  (require 'magent)
  (require 'magent-runtime-api)
  (require 'magent-runtime-queue)
  (require 'magent-protocol)
  (magent-runtime-ensure-initialized))

(defun ai-workbench-magent-runtime-session (&optional project-root)
  "Return the Magent runtime session for PROJECT-ROOT, creating it lazily."
  (ai-workbench-magent--load)
  (let* ((root (ai-workbench-magent--root
                (or project-root (ai-workbench-project-root))))
         (cached (gethash root ai-workbench-magent--runtime-sessions)))
    (or cached
        (let ((runtime (magent-runtime-session-current root)))
          (puthash root runtime ai-workbench-magent--runtime-sessions)
          runtime))))

(defun ai-workbench-magent-session-live-p (&optional project-root)
  "Return non-nil when PROJECT-ROOT has a Magent-owned runtime session."
  (let* ((root (ai-workbench-magent--root
                (or project-root (ai-workbench-project-root))))
         (runtime (gethash root ai-workbench-magent--runtime-sessions)))
    (and runtime t)))

(defun ai-workbench-magent-busy-p (&optional project-root)
  "Return non-nil when PROJECT-ROOT is active or queued in Magent."
  (when-let* ((runtime (gethash
                        (ai-workbench-magent--root
                         (or project-root (ai-workbench-project-root)))
                        ai-workbench-magent--runtime-sessions)))
    (or (> (magent-runtime-pending-count runtime) 0)
        (magent-runtime-queue-session-busy-p
         (magent-runtime-session-magent-session runtime)))))

(defun ai-workbench-magent--observer (backend root)
  "Return a bounded UI observer for BACKEND at ROOT."
  (let ((stream-marker nil)
        (reasoning-active nil))
    (lambda (event)
      (pcase (plist-get event :type)
        ('turn-start
         (setq stream-marker
               (ai-workbench-output-stream-start
                'answer
                (format "backend: %s\nproject: %s"
                        backend (abbreviate-file-name root))
                root)))
        ('assistant-delta
         (unless stream-marker
           (setq stream-marker
                 (ai-workbench-output-stream-start 'answer nil root)))
         (setq reasoning-active nil)
         (ai-workbench-output-stream-append stream-marker
                                            (or (plist-get event :text) "")))
        ('reasoning-delta
         ;; Keep reasoning out of the transcript, but retain an inexpensive
         ;; state signal instead of appending every private reasoning token.
         (unless reasoning-active
           (setq reasoning-active t)
           (ai-workbench-session-set-last-status
            (format "%s reasoning" backend) root)))
        ('tool-call-start
         (ai-workbench-output-append
          'tool
          (format "%s: %s"
                  (or (plist-get event :name) "tool")
                  (or (plist-get event :summary) "running"))
          root))
        ((or 'turn-complete 'turn-failed 'turn-cancelled)
         (when stream-marker
           (ai-workbench-output-stream-finish stream-marker)
           (setq stream-marker nil)))))))

(cl-defun ai-workbench-magent-submit
    (backend prompt &optional project-root on-success on-error)
  "Submit PROMPT through Magent using BACKEND for PROJECT-ROOT.
API uses Magent's native gptel sampler.  CLI backends use their structured
native protocols while retaining their own tools and permissions."
  (let* ((root (ai-workbench-magent--root
                (or project-root (ai-workbench-project-root))))
         (runtime (ai-workbench-magent-runtime-session root))
         (sampler (unless (eq backend 'api)
                    (require 'ai-workbench-magent-cli)
                    (ai-workbench-magent-cli-sampler backend root runtime)))
         (observer (ai-workbench-magent--observer backend root)))
    (when (> (string-bytes prompt) ai-workbench-magent-max-prompt-bytes)
      (user-error "Prompt exceeds ai-workbench's %d-byte limit"
                  ai-workbench-magent-max-prompt-bytes))
    (when (>= (magent-runtime-pending-count runtime)
              ai-workbench-magent-max-pending-per-project)
      (user-error "Magent queue is full for %s" (abbreviate-file-name root)))
    (magent-runtime-submit
     runtime prompt
     :sampler sampler
     :observer observer
     :turn-metadata (list :ai-workbench-backend backend)
     :on-complete
     (lambda (status result)
       (pcase status
         ('completed
          (ai-workbench-session-mark-profile-bootstrap-sent backend root)
          (ai-workbench-session-mark-profile-injected backend root)
          (ai-workbench-session-set-last-status
           (format "%s turn completed" backend) root)
          (ai-workbench-output-append 'status
                                      (format "%s turn completed" backend) root)
          (when on-success (funcall on-success)))
         ('cancelled
          (ai-workbench-session-set-last-status
           (format "%s turn cancelled" backend) root))
         (_
          (let ((message (magent-agent-result-content-string result)))
            (ai-workbench-session-set-last-error message root)
            (ai-workbench-session-set-last-status
             (format "%s turn failed" backend) root)
            (ai-workbench-output-append 'error message root)
            (when on-error (funcall on-error message)))))))
    runtime))

(defun ai-workbench-magent-cancel (&optional project-root)
  "Cancel active and queued Magent work for PROJECT-ROOT."
  (interactive)
  (when-let* ((runtime (gethash
                        (ai-workbench-magent--root
                         (or project-root (ai-workbench-project-root)))
                        ai-workbench-magent--runtime-sessions)))
    (magent-runtime-cancel runtime)))

(defun ai-workbench-magent-clear (&optional project-root)
  "Cancel and clear Magent state for PROJECT-ROOT."
  (interactive)
  (let* ((root (ai-workbench-magent--root
                (or project-root (ai-workbench-project-root))))
         (runtime (gethash root ai-workbench-magent--runtime-sessions)))
    (when runtime
      (magent-runtime-session-clear runtime)
      (remhash root ai-workbench-magent--runtime-sessions))
    t))

(defun ai-workbench-magent-open (&optional backend project-root)
  "Open the Magent UI for BACKEND at PROJECT-ROOT."
  (interactive)
  (let* ((root (ai-workbench-magent--root
                (or project-root (ai-workbench-project-root))))
         (engine (or backend (ai-workbench-session-backend root))))
    (ai-workbench-magent-runtime-session root)
    (let ((default-directory root))
      (if (eq engine 'api)
          (magent-start)
        (pop-to-buffer (ai-workbench-output-buffer root))))))

(defun ai-workbench-magent--api-available-p ()
  "Return non-nil when the embedded API runtime dependencies are visible."
  (and (locate-library "magent") (locate-library "gptel")))

(ai-workbench-register-backend
 'api
 :label "API · Magent/gptel"
 :generation 'ai-workbench-magent-api-v1
 :capabilities '(:session :send :draft :stop :cancel :headless)
 :authority '(:kind magent-native :sandboxed t)
 :operations
 (list
  :available-p #'ai-workbench-magent--api-available-p
  :live-p #'ai-workbench-magent-session-live-p
  :ensure #'ai-workbench-magent-runtime-session
  :open (lambda (root) (ai-workbench-magent-open 'api root))
  :send (lambda (prompt root on-success on-error)
          (ai-workbench-magent-submit 'api prompt root on-success on-error))
  :draft (lambda (_prompt _root _on-success _on-error) t)
  :stop #'ai-workbench-magent-cancel
  :cancel #'ai-workbench-magent-cancel))

(provide 'ai-workbench-magent)
;;; ai-workbench-magent.el ends here
