;;; tools/agent/interaction-backend.el --- Backend registry for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Internal backend contribution registry.  Adapters register capabilities and
;; operation functions here; the UI and dispatch layer derive their choices
;; from this registry instead of maintaining a parallel hardcoded stack.

;;; Code:

(require 'cl-lib)

(defvar +agent-interaction-backend--registry (make-hash-table :test 'eq)
  "Hash table mapping backend ids to spec plists.")

(defun +agent-interaction-backend--validate (id spec)
  "Validate backend ID and SPEC."
  (unless (symbolp id)
    (error "Backend id must be a symbol: %S" id))
  (unless (stringp (plist-get spec :label))
    (error "Backend %s missing string :label" id))
  (dolist (cap (plist-get spec :capabilities))
    (unless (memq cap '(:session :send :draft :cancel :stop :headless))
      (error "Backend %s has unsupported capability %S" id cap)))
  (let ((ops (plist-get spec :operations)))
    (dolist (op '(:available-p :live-p :ensure :open))
      (unless (functionp (plist-get ops op))
        (error "Backend %s missing operation %S" id op)))
    (dolist (pair '((:send . :send) (:draft . :draft) (:stop . :stop) (:cancel . :cancel)))
      (when (memq (car pair) (plist-get spec :capabilities))
        (unless (functionp (plist-get ops (cdr pair)))
          (error "Backend %s missing operation %S" id (cdr pair))))))
  t)

;;;###autoload
(defun +agent-interaction-register-backend (id &rest spec)
  "Register backend ID with SPEC and return a retractor function."
  (+agent-interaction-backend--validate id spec)
  (puthash id spec +agent-interaction-backend--registry)
  (let ((generation (plist-get spec :generation)))
    (lambda ()
      (when (eq generation (plist-get (gethash id +agent-interaction-backend--registry)
                                      :generation))
        (remhash id +agent-interaction-backend--registry)))))

(defun +agent-interaction-backend-spec (id)
  "Return backend ID's spec plist, or nil."
  (gethash id +agent-interaction-backend--registry))

(defun +agent-interaction-backend-ids (&optional capability)
  "Return registered backend ids, optionally filtered by CAPABILITY."
  (let (ids)
    (maphash
     (lambda (id spec)
       (when (or (null capability)
                 (memq capability (plist-get spec :capabilities)))
         (push id ids)))
     +agent-interaction-backend--registry)
    (sort ids (lambda (a b)
                (string< (plist-get (gethash a +agent-interaction-backend--registry) :label)
                         (plist-get (gethash b +agent-interaction-backend--registry) :label))))))

(defun +agent-interaction-backend-label (id)
  "Return backend ID's display label."
  (or (plist-get (+agent-interaction-backend-spec id) :label)
      (symbol-name id)))

(defun +agent-interaction-backend-call (id operation &rest args)
  "Call OPERATION for backend ID with ARGS."
  (let* ((spec (or (+agent-interaction-backend-spec id)
                   (error "Unknown backend: %s" id)))
         (fn (plist-get (plist-get spec :operations) operation)))
    (unless (functionp fn)
      (error "Backend %s does not support %S" id operation))
    (apply fn args)))

(defun +agent-interaction-backend-live-p (id project-root)
  "Return non-nil when backend ID has a live session for PROJECT-ROOT."
  (ignore-errors
    (+agent-interaction-backend-call id :live-p project-root)))

(provide 'tools/agent/interaction-backend)
;;; tools/agent/interaction-backend.el ends here
