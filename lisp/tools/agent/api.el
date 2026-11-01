;;; api.el --- Semantic access to existing Agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; No registry or execution state lives here.  The Noema gateway exposes this
;; adapter to Pi through the same progressively disclosed API as editor actions.
;; Contract: docs/design/agent-management.md C45, C50.

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(declare-function +agent-context--reference "tools/agent/context" (file root &optional buffer region session))
(declare-function +agent-context-content-blocks "tools/agent/context" (prompt references))
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))
(declare-function +agent-worktree--start "tools/agent/worktree" (agent name &rest keys))

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
       (cons (car field) (if (and value (symbolp value)) (symbol-name value) value))))
   '((name . :name) (agent . :agent) (origin . :origin) (root . :root)
     (target . :target) (workspace . :workspace) (nativeSessionId . :native-session-id)
     (processState . :process-state) (turnState . :turn-state)
     (attention . :attention-kind) (stopReason . :stop-reason)
     (queued . :queued) (held . :queue-held) (changes . :changes))))

(defun +agent-api--describe (snapshot)
  "Return a JSON description of SNAPSHOT's exact physical buffer."
  (append `((buffer . ,(buffer-name (plist-get snapshot :buffer)))
            (busy . ,(if (plist-get snapshot :busy) t :json-false)))
          (+agent-api--public snapshot)))

(defun +agent-api--facts (snapshot)
  "Return what the provider reported for SNAPSHOT: capabilities and usage.
Usage stays null until the provider has reported any, so unknown never reads
as zero."
  (let* ((buffer (plist-get snapshot :buffer))
         (capabilities (plist-get snapshot :capabilities))
         (usage (+agent-acp-usage buffer))
         (reported (or (> (or (plist-get usage :report-seq) 0) 0)
                       (> (or (plist-get usage :context-size) 0) 0))))
    `((capabilities
       . ,(mapcar (lambda (entry)
                    (cons (car entry) (if (plist-get capabilities (cdr entry)) t :json-false)))
                  '((list . :list) (load . :load) (resume . :resume)
                    (fork . :fork) (steer . :steer))))
      (usage
       . ,(when reported
            (mapcar (lambda (entry) (cons (car entry) (plist-get usage (cdr entry))))
                    '((model . :model) (contextUsed . :context-used) (contextSize . :context-size)
                      (compactions . :compactions) (input . :input) (output . :output)
                      (cachedRead . :cached-read) (cachedWrite . :cached-write)
                      (cost . :cost) (currency . :currency) (turns . :turns))))))))

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

;; One function per operation.  `+agent/api' checks the shared fences and
;; dispatches; a new operation is a function and one row there.

(defun +agent-api--list (body offset limit)
  "Return the page of public sessions BODY's fences select."
  (unless (and (integerp offset) (>= offset 0) (integerp limit) (<= 1 limit 100))
    (user-error "offset must be >= 0 and limit must be 1..100"))
  (let* ((rows (seq-filter (lambda (row) (+agent-api--matches-p row body))
                           (+agent-pool-sessions nil nil 'all)))
         (total (length rows)))
    `((ok . t) (total . ,total)
      (sessions . ,(vconcat (mapcar #'+agent-api--describe
                                    (seq-subseq rows (min offset total) (min total (+ offset limit))))))
      (nextOffset . ,(when (< (+ offset limit) total) (+ offset limit))))))

(defun +agent-api--create-in-worktree (root agent body)
  "Start AGENT in a new worktree of ROOT named by BODY (C50)."
  (remote-require-on-client 'tools/agent/worktree)
  (let* ((default-directory root)
         (started (+agent-worktree--start agent (+agent-api--string body "worktree")
                                          :group (+agent-api--string body "group"))))
    `((ok . t) (accepted . t)
      (worktree . ((branch . ,(plist-get started :branch))
                   (path . ,(plist-get started :path))))
      (session . ,(+agent-api--describe (+agent-session-snapshot (plist-get started :buffer)))))))

(defun +agent-api--create-in-workspace (root agent directory)
  "Start AGENT in ROOT's workspace DIRECTORY, without a prompt."
  (let* (;; Provider packages are Emacs client code, even for remote work.
         (config (let ((default-directory temporary-file-directory))
                   (+agent-acp-config-for agent))))
    (unless config (user-error "Agent %s has no available ACP configuration" agent))
    (let ((buffer (+agent-acp-start :config config :directory directory :focus nil :origin 'manual)))
      (+agent-acp-adopt buffer :agent agent :origin 'manual :root root)
      `((ok . t) (accepted . t) (session . ,(+agent-api--describe (+agent-session-snapshot buffer)))))))

(defun +agent-api--create (body)
  "Create a session from BODY: in the workspace, or in its own worktree."
  (let ((root (+agent-api--string body "root"))
        (agent (+agent-api--string body "agent")))
    (unless (and root (file-name-absolute-p root) agent)
      (user-error "create requires an absolute root and agent identifier"))
    (setq root (remote-resolve-emacs-file-name root))
    (unless (file-directory-p root) (user-error "Project directory does not exist"))
    (let ((directory (+agent-workspace root)))
      (when (and (+agent-api--string body "target")
                 (not (equal (+agent-api--string body "target") (remote-file-name-target directory))))
        (user-error "Project workspace does not belong to the requested execution target"))
      (if (+agent-api--string body "worktree")
          (+agent-api--create-in-worktree root agent body)
        (+agent-api--create-in-workspace root agent directory)))))

(defun +agent-api-identified (body)
  "Return BODY's session row, proven by its buffer and native ID."
  (let* ((name (+agent-api--string body "buffer"))
         (native (+agent-api--string body "nativeSessionId"))
         (snapshot (seq-find (lambda (row) (equal name (buffer-name (plist-get row :buffer))))
                             (+agent-pool-sessions nil nil 'all))))
    (unless (and snapshot native (equal native (plist-get snapshot :native-session-id))
                 (+agent-api--matches-p snapshot body))
      (user-error "Session identity no longer matches; list sessions again"))
    snapshot))

(defun +agent-api--send (snapshot body)
  "Deliver BODY's prompt and file references to SNAPSHOT's session."
  (let ((buffer (plist-get snapshot :buffer))
        (prompt (+agent-api--string body "prompt"))
        (files (+agent-api--value body "files")))
    (unless (and (eq (plist-get snapshot :process-state) 'live)
                 (not (plist-get snapshot :retired))
                 (eq (+agent-acp-state-value buffer '(:init-finished)) t))
      (user-error "Session is not ready; explicitly resume it before sending"))
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

(defun +agent-api--acknowledge (buffer action)
  "Return the reply to ACTION on BUFFER, with the state it left."
  (let ((snapshot (+agent-session-snapshot buffer)))
    `((ok . t) (accepted . ,(if (equal action "status") :json-false t))
      (session . ,(append (+agent-api--describe snapshot)
                          (and (equal action "status") (+agent-api--facts snapshot)))))))

;;;###autoload
(defun +agent/api (body)
  "Query or explicitly operate on existing Agent sessions with JSON BODY.
All operations other than list/create require a buffer and its native ID.
Sending is asynchronous; status and bounded read report actual progress."
  (let* ((action (or (+agent-api--string body "action") "list"))
         (offset (or (+agent-api--value body "offset") 0))
         (limit (or (+agent-api--value body "limit") (if (equal action "read") 12000 20))))
    (when-let* ((root (+agent-api--string body "root")))
      (unless (file-name-absolute-p root) (user-error "root must be absolute")))
    (pcase action
      ("list" (+agent-api--list body offset limit))
      ("create" (+agent-api--create body))
      ((or "status" "send" "read" "stop" "show")
       (let* ((snapshot (+agent-api-identified body))
              (buffer (plist-get snapshot :buffer)))
         (pcase action
           ("read" (append `((ok . t) (session . ,(+agent-api--describe snapshot)))
                           (+agent-acp-read-transcript buffer offset limit)))
           (_ (pcase action
                ("send" (+agent-api--send snapshot body))
                ("stop" (+agent-acp-stop buffer))
                ("show" (+agent-acp-show-buffer buffer)))
              (+agent-api--acknowledge buffer action)))))
      (_ (user-error "Unknown Agent operation: %s" action)))))

(provide 'tools/agent/api)
;;; api.el ends here
