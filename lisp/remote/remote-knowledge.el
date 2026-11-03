;;; remote-knowledge.el --- Session and project knowledge registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Remote answers ordinary Emacs questions -- does this file exist, which
;; program is `git', can this directory be entered -- from what it already
;; knows about a target, so packages need no Remote code of their own.
;; That knowledge lives with its owner (the /fs: layer, a backend adapter),
;; but it has one lifecycle: a source registers here, and every source of a
;; target is forgotten together when the target's session closes or Remote
;; is reset.  Remote Doctor lists the sources from the same table.
;;
;; A source's scope says what its answers belong to: `session' answers stay
;; true for one transport generation unless the server reports a change;
;; `project' answers also depend on a workspace environment or project
;; root.  See "以 session 与 project 为单位的知识层" in
;; docs/design/remote-framework.md.

;;; Code:

(require 'cl-lib)

(cl-defstruct (remote-knowledge-source
               (:constructor remote-knowledge-source-create)
               (:copier nil))
  "One kind of knowledge Remote keeps about targets."
  name scope forget report)

(defvar remote-knowledge-sources nil
  "Registered `remote-knowledge-source' records, newest first.")

(cl-defun remote-knowledge-register (name &key (scope 'session) forget report)
  "Register knowledge source NAME and return it.
SCOPE is `session' or `project'.  FORGET is called with a target ID, or
nil for every target, and drops what the source knows about it.  REPORT
returns a plist describing the source for Remote Doctor.  Registering NAME
again replaces the earlier record, so reloading an owner is safe."
  (unless (memq scope '(session project))
    (error "Unknown knowledge scope for %s: %S" name scope))
  (let ((source (remote-knowledge-source-create
                 :name name :scope scope :forget forget :report report)))
    (setq remote-knowledge-sources
          (cons source
                (cl-remove name remote-knowledge-sources
                           :key #'remote-knowledge-source-name
                           :test #'equal)))
    source))

(defun remote-knowledge-unregister (name)
  "Remove knowledge source NAME."
  (setq remote-knowledge-sources
        (cl-remove name remote-knowledge-sources
                   :key #'remote-knowledge-source-name :test #'equal)))

(defun remote-knowledge-forget (&optional target-id)
  "Forget what every source knows about TARGET-ID, or about every target.
One failing source never keeps the others from forgetting."
  (dolist (source remote-knowledge-sources)
    (when-let* ((forget (remote-knowledge-source-forget source)))
      (condition-case err
          (funcall forget target-id)
        (error
         (message "Remote knowledge %s could not forget %s: %s"
                  (remote-knowledge-source-name source)
                  (or target-id "all targets")
                  (error-message-string err)))))))

(defun remote-knowledge-report ()
  "Return (NAME SCOPE PLIST) for each registered source, oldest first."
  (mapcar (lambda (source)
            (list (remote-knowledge-source-name source)
                  (remote-knowledge-source-scope source)
                  (when-let* ((report (remote-knowledge-source-report source)))
                    (condition-case err
                        (funcall report)
                      (error (list :error (error-message-string err)))))))
          (reverse remote-knowledge-sources)))

(provide 'remote-knowledge)
;;; remote-knowledge.el ends here
