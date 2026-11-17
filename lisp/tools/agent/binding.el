;;; tools/agent/binding.el --- Document collaborators: a session's project file -*- lexical-binding: t; -*-

;;; Commentary:
;; A collaborator is an ordinary pool session bound to one primary file of its
;; project (C65).  The binding is one more permanent-local fact on the session
;; buffer, read through `+agent-session-snapshot' like every other; there is no
;; second registry.  A nil file binds the session at project level.  The agent
;; itself still reads and writes any file; the files it changed besides its
;; primary one come from the C44 change record.
;;
;; A binding survives resuming the conversation: it is kept by native session
;; ID in a small bounded table under var/agent/, written once the provider has
;; given the session its ID and read back when a session with that ID appears
;; without a binding.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Loaded at startup by the agent module, so it must not load the runtime:
;; every runtime call below runs only once an agent buffer exists.
(defvar +agent-acp-session-file)
(defvar +agent-acp-session-root)
(defvar +agent-acp-session-name)
(defvar +agent-acp--changes)
(defvar +agent-acp-changed-functions)
(declare-function remote-canonicalize-emacs-file-name "remote" (file))
(declare-function +agent-acp-state-value "tools/agent/acp" (buffer path &optional default))
(declare-function +agent-acp-agent-buffer-p "tools/agent/acp" (buffer))
(declare-function +agent-acp--unique-name "tools/agent/acp" (base root))
(declare-function +agent-pool-sessions "tools/agent/acp" (&optional root target kind))
(declare-function +agent-session-rename "tools/agent/sessions" (&optional buffer name))

(defcustom +agent-binding-store-file (locate-user-emacs-file "var/agent/bindings.eld")
  "Where session bindings are kept by native session ID."
  :type 'file :group '+agent-session)

(defcustom +agent-binding-store-limit 500
  "Most bindings the store keeps; the oldest are dropped first."
  :type 'natnum :group '+agent-session)

(defvar +agent-binding--cache nil
  "The store as last read or written, or nil when it must be read again.")

(defvar +agent-document-file-functions nil
  "Queries receiving a BUFFER and returning the document file it shows, or nil.
A surface whose buffer is not the file itself (a Noema page) registers here, so
the agent layer never depends on it.")

(defun +agent-binding-canonical (file)
  "Return FILE's framework identity, or nil when FILE is nil or empty."
  (when (and (stringp file) (not (string-empty-p file)))
    (if (fboundp 'remote-canonicalize-emacs-file-name)
        (remote-canonicalize-emacs-file-name (expand-file-name file))
      (expand-file-name file))))

(defun +agent-binding--native-id (buffer)
  "Return BUFFER's native session ID, or nil before the provider gave one."
  (let ((id (+agent-acp-state-value buffer '(:session :id))))
    (and (stringp id) (not (string-empty-p id)) id)))

;;;; Store

(defun +agent-binding--read ()
  "Return the store as an alist (NATIVE-ID . PLIST), newest first."
  (or +agent-binding--cache
      (setq +agent-binding--cache
            (condition-case nil
                (when (file-readable-p +agent-binding-store-file)
                  (with-temp-buffer
                    (insert-file-contents +agent-binding-store-file)
                    (let ((value (read (current-buffer))))
                      (and (listp value) (seq-filter #'consp value)))))
              (error nil)))))

(defun +agent-binding--write (entries)
  "Keep ENTRIES, newest first, bounded by `+agent-binding-store-limit'."
  (setq entries (seq-take entries +agent-binding-store-limit)
        +agent-binding--cache entries)
  (condition-case err
      (progn
        (make-directory (file-name-directory +agent-binding-store-file) t)
        (let ((print-length nil) (print-level nil))
          (with-temp-file +agent-binding-store-file
            (prin1 entries (current-buffer)))))
    (error (message "Could not save agent bindings: %s" (error-message-string err)))))

(defun +agent-binding-lookup (native-id)
  "Return the stored (:file :root :name) of NATIVE-ID, or nil."
  (cdr (assoc native-id (+agent-binding--read))))

(defun +agent-binding--save (buffer)
  "Store BUFFER's binding when its native ID is known."
  (require 'tools/agent/acp)
  (when-let* ((id (+agent-binding--native-id buffer)))
    (with-current-buffer buffer
      (+agent-binding--write
       (cons (cons id (list :file +agent-acp-session-file :root +agent-acp-session-root
                            :name +agent-acp-session-name :at (float-time)))
             (assoc-delete-all id (copy-sequence (+agent-binding--read))))))))

;;;; Binding

(defvar-local +agent-binding--settled nil
  "Non-nil once this session's binding was set or restored; restore stops then.")
(put '+agent-binding--settled 'permanent-local t)

(defvar-local +agent-binding--saved-id nil
  "Native ID under which this session's binding was last stored.")
(put '+agent-binding--saved-id 'permanent-local t)

(defun +agent-binding-file (buffer)
  "Return BUFFER's primary file, or nil at project level."
  (and (buffer-live-p buffer) (featurep 'tools/agent/acp) (buffer-local-value '+agent-acp-session-file buffer)))

(defun +agent-binding-set (buffer file)
  "Bind session BUFFER to FILE, or to its project level when FILE is nil.
Return the stored file identity."
  (require 'tools/agent/acp)
  (let ((file (+agent-binding-canonical file)))
    (with-current-buffer buffer
      (setq-local +agent-acp-session-file file)
      (setq-local +agent-binding--settled t))
    (+agent-binding--save buffer)
    (with-current-buffer buffer (run-hook-with-args '+agent-acp-changed-functions buffer))
    file))

(defun +agent-binding--restore-h (buffer)
  "Restore or store BUFFER's binding once its native ID is known.
A session that already has a binding stores it under a newly arrived ID; one
without takes the stored binding and name of a conversation it resumes."
  (when-let* (((+agent-acp-agent-buffer-p buffer))
              (id (+agent-binding--native-id buffer))
              ((not (equal id (buffer-local-value '+agent-binding--saved-id buffer)))))
    (with-current-buffer buffer
      (setq-local +agent-binding--saved-id id)
      (if +agent-binding--settled
          (+agent-binding--save buffer)
        (when-let* ((entry (+agent-binding-lookup id)))
          (setq-local +agent-acp-session-file (plist-get entry :file)
                      +agent-binding--settled t)
          (when-let* ((name (plist-get entry :name))
                      ((not (equal name +agent-acp-session-name))))
            ;; The ordinary rename, so the buffer name and any domain
            ;; registration follow.  This runs from a state event: a refused
            ;; name is reported, never raised.
            (condition-case err
                (progn (require 'tools/agent/sessions)
                       (+agent-session-rename buffer (+agent-acp--unique-name name +agent-acp-session-root)))
              (error (message "Agent binding: kept the session name (%s)"
                              (error-message-string err)))))
          (+agent-binding--save buffer))))))

(add-hook '+agent-acp-changed-functions #'+agent-binding--restore-h)

;;;; Announcing a document's collaborators

(defvar +agent-binding-changed-functions nil
  "Functions called with a document FILE after its collaborators changed.
A collaborator was created, ended, rebound or changed state.  Calls are
coalesced per file; a surface showing the document's collaborators (a Noema
page's header) refreshes from here.")

(defvar +agent-binding--announce-pending nil
  "Files whose collaborator change is waiting to be announced.")
(defvar +agent-binding--announce-timer nil
  "Timer that announces the pending files.")

(defun +agent-binding--announce ()
  "Announce every pending file once."
  (let ((files (prog1 +agent-binding--announce-pending
                 (setq +agent-binding--announce-pending nil +agent-binding--announce-timer nil))))
    (dolist (file files) (run-hook-with-args '+agent-binding-changed-functions file))))

(defun +agent-binding--announce-h (buffer)
  "Schedule the announcement of BUFFER's primary file, coalesced."
  (when-let* (((buffer-live-p buffer))
              (file (buffer-local-value '+agent-acp-session-file buffer)))
    (cl-pushnew file +agent-binding--announce-pending :test #'equal)
    (unless +agent-binding--announce-timer
      (setq +agent-binding--announce-timer (run-with-idle-timer 0.3 nil #'+agent-binding--announce)))))

(add-hook '+agent-acp-changed-functions #'+agent-binding--announce-h)

;;;; The document the person works on

(defvar +agent-binding-last-document nil
  "The document file the person focused last (C75).
`H-T' binds a new agent to it and the panel speaks about it, even when they
are invoked from a chat or the panel itself.")

(defun +agent-binding--track-document-h (&rest _)
  "Remember the selected window's document, when it shows one."
  (when-let* ((file (+agent-document-file)))
    (setq +agent-binding-last-document file)))

(add-hook 'aaron-switch-buffer-hook #'+agent-binding--track-document-h)

;;;; Queries

(defun +agent-binding-sessions (file &optional root)
  "Return the public sessions whose primary file is FILE, newest first.
ROOT limits them to one project; background workers are left out."
  (require 'tools/agent/acp)
  (let ((file (+agent-binding-canonical file)))
    (sort (seq-filter (lambda (session)
                        (and file (equal (plist-get session :file) file)
                             (not (plist-get session :background))))
                      (+agent-pool-sessions root))
          (lambda (a b) (> (or (plist-get a :last-used) 0) (or (plist-get b :last-used) 0))))))

(defun +agent-binding-touched (buffer)
  "Return the files BUFFER's session changed besides its primary file, newest first."
  (require 'tools/agent/acp)
  (let ((primary (+agent-binding-file buffer)))
    (delete-dups
     (seq-remove (lambda (file) (or (null file) (equal file primary)))
                 (mapcar (lambda (entry) (+agent-binding-canonical (car entry)))
                         (buffer-local-value '+agent-acp--changes buffer))))))

(defun +agent-document-file (&optional buffer)
  "Return the document file BUFFER shows, or nil when it shows none.
Agent buffers and special-mode buffers are not documents."
  (let ((buffer (or buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (not (and (fboundp '+agent-acp-agent-buffer-p) (+agent-acp-agent-buffer-p buffer))))
      (or (run-hook-with-args-until-success '+agent-document-file-functions buffer)
          (with-current-buffer buffer
            (and (not (derived-mode-p 'special-mode)) buffer-file-name))))))

(provide 'tools/agent/binding)
;;; binding.el ends here
