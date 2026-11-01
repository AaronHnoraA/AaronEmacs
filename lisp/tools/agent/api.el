;;; api.el --- Semantic access to existing Agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; No registry or execution state lives here.  The Noema gateway exposes this
;; adapter to Pi through the same progressively disclosed API as editor actions.
;; Contract: docs/design/agent-management.md C45.

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(declare-function +agent-context--reference "tools/agent/context" (file root &optional buffer region session))
(declare-function +agent-context-content-blocks "tools/agent/context" (prompt references))
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))

(defun +agent-api--value (body key)
  "Read JSON KEY from BODY, accepting gateway hashes and Lisp alists."
  (if (hash-table-p body) (gethash key body)
    (cdr (or (assoc key body) (assq (intern key) body)))))

(defun +agent-api--string (body key)
  "Read an optional nonempty string KEY from BODY."
  (let ((value (+agent-api--value body key)))
    (cond ((memq value '(nil :null)) nil)
          ((and (stringp value) (not (string-empty-p value))) value)
          (t (user-error "%s must be a nonempty string" key)))))

(defun +agent-api--public (snapshot)
  "Project SNAPSHOT without prompt text, drafts or private runtime objects."
  (mapcar
   (lambda (field)
     (let ((value (plist-get snapshot (cdr field))))
       (cons (car field) (if (symbolp value) (if value (symbol-name value) :null) value))))
   '((name . :name) (agent . :agent) (origin . :origin) (root . :root)
     (target . :target) (workspace . :workspace) (nativeSessionId . :native-session-id)
     (processState . :process-state) (turnState . :turn-state)
     (attention . :attention-kind) (stopReason . :stop-reason)
     (queued . :queued) (held . :queue-held) (changes . :changes))))

(defun +agent-api--describe (snapshot)
  "Return a JSON description of SNAPSHOT's exact physical buffer."
  (append `((buffer . ,(buffer-name (plist-get snapshot :buffer)))
            (busy . ,(if (plist-get snapshot :busy) t :false)))
          (+agent-api--public snapshot)))

(defun +agent-api--matches-p (snapshot body)
  "Check optional pool fences on SNAPSHOT against BODY."
  (and (or (not (+agent-api--string body "root"))
           (equal (+agent-pool-key snapshot)
                  (+agent-pool-key
                   (list :root (+agent-api--string body "root")
                         :target (or (+agent-api--string body "target")
                                     (remote-file-name-target (+agent-api--string body "root")))))))
       (or (not (+agent-api--string body "target"))
           (equal (plist-get snapshot :target) (+agent-api--string body "target")))))

;;;###autoload
(cl-defun +agent/api (body)
  "Query or explicitly operate on existing Agent sessions with JSON BODY.
All operations other than list/create require a buffer and its native ID.
Sending is asynchronous; status and bounded read report actual progress."
  (let* ((action (or (+agent-api--string body "action") "list"))
         (offset (or (+agent-api--value body "offset") 0))
         (limit (or (+agent-api--value body "limit") (if (equal action "read") 12000 20))))
    (when-let* ((root (+agent-api--string body "root")))
      (unless (file-name-absolute-p root) (user-error "root must be absolute")))
    (when (equal action "list")
      (unless (and (integerp offset) (>= offset 0) (integerp limit) (<= 1 limit 100))
        (user-error "offset must be >= 0 and limit must be 1..100"))
      (let* ((rows (seq-filter (lambda (row) (+agent-api--matches-p row body))
                               (+agent-pool-sessions)))
             (total (length rows)))
        (cl-return-from +agent/api
          `((ok . t) (total . ,total)
            (sessions . ,(vconcat (mapcar #'+agent-api--describe
                                         (seq-subseq rows (min offset total) (min total (+ offset limit))))))
            (nextOffset . ,(when (< (+ offset limit) total) (+ offset limit)))))))
    (when (equal action "create")
      (let* ((root (+agent-api--string body "root"))
             (agent (+agent-api--string body "agent")))
        (unless (and root (file-name-absolute-p root) agent)
          (user-error "create requires an absolute root and agent identifier"))
        (setq root (remote-resolve-emacs-file-name root))
        (unless (file-directory-p root) (user-error "Project directory does not exist"))
        (let* ((directory (+agent-workspace root))
               ;; Provider packages are Emacs client code, even for remote work.
               (config (let ((default-directory temporary-file-directory))
                         (+agent-acp-config-for agent))))
          (when (and (+agent-api--string body "target")
                     (not (equal (+agent-api--string body "target") (remote-file-name-target directory))))
            (user-error "Project workspace does not belong to the requested execution target"))
          (unless config (user-error "Agent %s has no available ACP configuration" agent))
          (let ((buffer (+agent-acp-start :config config :directory directory :focus nil :origin 'manual)))
            (+agent-acp-adopt buffer :agent agent :origin 'manual :root root)
            (cl-return-from +agent/api
              `((ok . t) (accepted . t) (session . ,(+agent-api--describe (+agent-session-snapshot buffer)))))))))
    (unless (member action '("status" "send" "read" "stop" "show"))
      (user-error "Unknown Agent operation: %s" action))
    (let* ((name (+agent-api--string body "buffer"))
           (native (+agent-api--string body "nativeSessionId"))
           (snapshot (seq-find (lambda (row) (equal name (buffer-name (plist-get row :buffer))))
                               (+agent-pool-sessions)))
           (buffer (plist-get snapshot :buffer)))
      (unless (and snapshot native (equal native (plist-get snapshot :native-session-id))
                   (+agent-api--matches-p snapshot body))
        (user-error "Session identity no longer matches; list sessions again"))
      (pcase action
        ("read"
         (cl-return-from +agent/api
           (append `((ok . t) (session . ,(+agent-api--describe snapshot)))
                   (+agent-acp-read-transcript buffer offset limit))))
        ("send"
         (unless (and (eq (plist-get snapshot :process-state) 'live)
                      (not (plist-get snapshot :retired))
                      (eq (+agent-acp-state-value buffer '(:init-finished)) t))
           (user-error "Session is not ready; explicitly resume it before sending"))
         (let ((prompt (+agent-api--string body "prompt"))
               (files (+agent-api--value body "files")))
           (unless (and prompt (not (string-empty-p (string-trim prompt))))
             (user-error "send requires a prompt"))
           (unless (or (null files) (and (sequencep files) (not (stringp files))))
             (user-error "files must be an array of absolute saved file references"))
           (remote-require-on-client 'tools/agent/context)
           (let ((references
                  (mapcar (lambda (file)
                            (unless (and (stringp file) (file-name-absolute-p file) (file-readable-p file))
                              (user-error "Unreadable file reference: %s" file))
                            (+agent-context--reference file (plist-get snapshot :root) nil nil buffer))
                          (append files nil))))
             (+agent-context--deliver buffer (+agent-context-content-blocks prompt references) nil t))))
        ("stop" (+agent-acp-stop buffer))
        ("show" (+agent-acp-show-buffer buffer)))
      `((ok . t) (accepted . ,(if (equal action "status") :false t))
        (session . ,(+agent-api--describe (+agent-session-snapshot buffer)))))))

(provide 'tools/agent/api)
;;; api.el ends here
