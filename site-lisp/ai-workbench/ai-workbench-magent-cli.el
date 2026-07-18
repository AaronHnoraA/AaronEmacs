;;; ai-workbench-magent-cli.el --- Structured CLI samplers for Magent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Let Magent own session/queue/lifecycle state while Codex, Claude Code, and
;; OpenCode keep ownership of their native tools and permission models.  Each
;; CLI is consumed through its newline-delimited JSON protocol; no terminal
;; scraping and no Magent tool calls are involved on this path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magent-llm)
(require 'magent-runtime-api)
(require 'magent-session)

(defgroup ai-workbench-magent-cli nil
  "External coding-agent samplers managed by Magent."
  :group 'ai-workbench
  :prefix "ai-workbench-magent-cli-")

(defcustom ai-workbench-magent-cli-max-json-line-bytes (* 1024 1024)
  "Maximum buffered bytes for one CLI JSON line."
  :type 'integer
  :group 'ai-workbench-magent-cli)

(defcustom ai-workbench-magent-cli-max-answer-bytes (* 8 1024 1024)
  "Maximum assistant text accepted from one CLI turn."
  :type 'integer
  :group 'ai-workbench-magent-cli)

(defcustom ai-workbench-magent-cli-max-diagnostic-bytes (* 256 1024)
  "Maximum stderr and malformed-output diagnostic bytes retained per turn."
  :type 'integer
  :group 'ai-workbench-magent-cli)

(defcustom ai-workbench-magent-cli-max-prompt-bytes (* 4 1024 1024)
  "Maximum combined Magent context and user prompt sent to one CLI turn."
  :type 'integer
  :group 'ai-workbench-magent-cli)

(defcustom ai-workbench-claude-extra-args nil
  "Additional arguments passed to structured Claude Code requests."
  :type '(repeat string)
  :group 'ai-workbench-magent-cli)

(cl-defstruct (ai-workbench-magent-cli-run
               (:constructor ai-workbench-magent-cli-run-create)
               (:copier nil))
  engine root runtime-session request process stderr-buffer pending
  diagnostic-chunks diagnostic-bytes answer-chunks answer-bytes
  text-seen reasoning-open terminal-seen session-id)

(defun ai-workbench-magent-cli--json-get (object key)
  "Return KEY from JSON OBJECT represented as a plist, alist, or hash table."
  (let* ((name (if (symbolp key) (symbol-name key) key))
         (keyword (intern (concat ":" name)))
         (symbol (intern name)))
    (cond
     ((hash-table-p object)
      (or (gethash name object) (gethash symbol object) (gethash keyword object)))
     ((and (listp object) (keywordp (car-safe object)))
      (plist-get object keyword))
     ((listp object)
      (or (cdr (assoc name object))
          (cdr (assq symbol object))
          (cdr (assq keyword object)))))))

(defun ai-workbench-magent-cli--json-path (object &rest keys)
  "Return the value below KEYS in JSON OBJECT."
  (dolist (key keys object)
    (setq object (and object (ai-workbench-magent-cli--json-get object key)))))

(defun ai-workbench-magent-cli--string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun ai-workbench-magent-cli--content-string (content)
  "Return a compact textual representation of prompt CONTENT."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat #'ai-workbench-magent-cli--content-string (append content nil) "\n"))
   ((and (listp content)
         (ai-workbench-magent-cli--json-get content "text"))
    (or (ai-workbench-magent-cli--string
         (ai-workbench-magent-cli--json-get content "text"))
        ""))
   ((listp content)
    (mapconcat #'ai-workbench-magent-cli--content-string content "\n"))
   ((null content) "")
   (t (format "%s" content))))

(defun ai-workbench-magent-cli--latest-prompt (request)
  "Return the most recent user prompt from Magent REQUEST."
  (let ((prompt (magent-llm-request-prompt request)))
    (if (stringp prompt)
        prompt
      (or (cl-loop for entry in (reverse (append prompt nil))
                   when (eq (car-safe entry) 'prompt)
                   return (ai-workbench-magent-cli--content-string (cdr entry)))
          (ai-workbench-magent-cli--content-string prompt)))))

(defun ai-workbench-magent-cli--effective-prompt (request engine)
  "Build the external coding-agent prompt for REQUEST and ENGINE."
  (let* ((prompt (string-trim (ai-workbench-magent-cli--latest-prompt request)))
         (system (ai-workbench-magent-cli--string
                  (magent-llm-request-system request)))
         (combined
          (concat
           (when system
             (concat "--- Magent-managed context ---\n" system
                     "\n\n--- User request ---\n"))
           prompt
           "\n\n--- ai-workbench runtime boundary ---\n"
           (format "You are running through the %s CLI. " engine)
           "Use your own native tools and permission system. "
           "Do not emit Magent tool-call syntax; return a normal final answer.")))
    (when (> (string-bytes combined)
             ai-workbench-magent-cli-max-prompt-bytes)
      (error "Combined Magent/CLI prompt exceeds %d bytes"
             ai-workbench-magent-cli-max-prompt-bytes))
    combined))

(defun ai-workbench-magent-cli--metadata-key (engine)
  "Return the persisted upstream session metadata key for ENGINE."
  (intern (format "ai-workbench-%s-session-id" engine)))

(defun ai-workbench-magent-cli--session (run)
  "Return RUN's underlying persistent Magent session."
  (magent-runtime-session-magent-session
   (ai-workbench-magent-cli-run-runtime-session run)))

(defun ai-workbench-magent-cli--persist-session-id (run session-id)
  "Persist SESSION-ID for RUN when it changed."
  (when-let* ((id (ai-workbench-magent-cli--string session-id))
              (session (ai-workbench-magent-cli--session run)))
    (unless (equal id (ai-workbench-magent-cli-run-session-id run))
      (setf (ai-workbench-magent-cli-run-session-id run) id)
      (magent-session-set-metadata-value
       session (ai-workbench-magent-cli--metadata-key
                (ai-workbench-magent-cli-run-engine run))
       id)
      (magent-session-save-deferred-for-session
       session
       (magent-runtime-session-scope
        (ai-workbench-magent-cli-run-runtime-session run))))))

(defun ai-workbench-magent-cli--callback (run event)
  "Send normalized EVENT to RUN's request callback."
  (when-let* ((callback (magent-llm-request-callback
                         (ai-workbench-magent-cli-run-request run))))
    (funcall callback event)))

(defun ai-workbench-magent-cli--finish-error (run message &optional metadata)
  "Finish RUN with MESSAGE and optional METADATA."
  (unless (ai-workbench-magent-cli-run-terminal-seen run)
    (setf (ai-workbench-magent-cli-run-terminal-seen run) t)
    (ai-workbench-magent-cli--callback
     run (magent-llm-error-event message metadata))))

(defun ai-workbench-magent-cli--finish-success (run &optional usage)
  "Finish RUN successfully with optional USAGE."
  (unless (ai-workbench-magent-cli-run-terminal-seen run)
    (setf (ai-workbench-magent-cli-run-terminal-seen run) t)
    (when (ai-workbench-magent-cli-run-reasoning-open run)
      (setf (ai-workbench-magent-cli-run-reasoning-open run) nil)
      (ai-workbench-magent-cli--callback run (magent-llm-reasoning-end-event)))
    (ai-workbench-magent-cli--callback
     run (magent-llm-completed-event nil usage 'stop
                                     (list :engine
                                           (ai-workbench-magent-cli-run-engine run))))))

(defun ai-workbench-magent-cli--answer-delta (run text &optional reasoning)
  "Emit TEXT for RUN, as REASONING when non-nil, enforcing the answer cap."
  (when-let* ((value (ai-workbench-magent-cli--string text)))
    (let ((new-size (+ (ai-workbench-magent-cli-run-answer-bytes run)
                       (string-bytes value))))
      (if (> new-size ai-workbench-magent-cli-max-answer-bytes)
          (progn
            (ai-workbench-magent-cli--finish-error
             run "CLI response exceeded ai-workbench's per-turn size limit"
             (list :status 'response-too-large :bytes new-size))
            (when-let* ((process (ai-workbench-magent-cli-run-process run)))
              (when (process-live-p process) (delete-process process))))
        ;; Magent's loop and output marker already own the streamed text.  Keep
        ;; only a byte counter here rather than retaining a second large copy.
        (setf (ai-workbench-magent-cli-run-answer-bytes run) new-size)
        (if reasoning
            (progn
              (setf (ai-workbench-magent-cli-run-reasoning-open run) t)
              (ai-workbench-magent-cli--callback
               run (magent-llm-reasoning-delta-event value)))
          (setf (ai-workbench-magent-cli-run-text-seen run) t)
          (when (ai-workbench-magent-cli-run-reasoning-open run)
            (setf (ai-workbench-magent-cli-run-reasoning-open run) nil)
            (ai-workbench-magent-cli--callback run (magent-llm-reasoning-end-event)))
          (ai-workbench-magent-cli--callback
           run (magent-llm-text-delta-event value)))))))

(defun ai-workbench-magent-cli--heartbeat (run object &optional label)
  "Emit a bounded progress heartbeat for RUN based on OBJECT and LABEL."
  (ai-workbench-magent-cli--callback
   run (magent-llm-event-create
        'usage :usage (list :engine (ai-workbench-magent-cli-run-engine run)
                            :progress (or label "event"))
        :raw object)))

(defun ai-workbench-magent-cli--diagnostic (run text)
  "Retain a bounded diagnostic tail TEXT for RUN."
  (when (stringp text)
    (let* ((limit ai-workbench-magent-cli-max-diagnostic-bytes)
           (old (or (car (ai-workbench-magent-cli-run-diagnostic-chunks run))
                    ""))
           (combined (concat old text))
           (tail
            (if (<= (string-bytes combined) limit)
                combined
              ;; Find the earliest character whose suffix fits the byte cap.
              ;; This preserves valid multibyte text and keeps the diagnostic
              ;; limit honest for non-ASCII provider output.
              (let ((low 0)
                    (high (length combined)))
                (while (< low high)
                  (let ((mid (/ (+ low high) 2)))
                    (if (> (string-bytes (substring combined mid)) limit)
                        (setq low (1+ mid))
                      (setq high mid))))
                (substring combined low)))))
      (setf (ai-workbench-magent-cli-run-diagnostic-chunks run) (list tail)
            (ai-workbench-magent-cli-run-diagnostic-bytes run) (string-bytes tail)))))

(defun ai-workbench-magent-cli--codex-event (run object)
  "Map one Codex JSON OBJECT into normalized events for RUN."
  (let* ((type (ai-workbench-magent-cli--json-get object "type"))
         (item (ai-workbench-magent-cli--json-get object "item"))
         (item-type (ai-workbench-magent-cli--json-get item "type")))
    (pcase type
      ("thread.started"
       (ai-workbench-magent-cli--persist-session-id
        run (ai-workbench-magent-cli--json-get object "thread_id")))
      ("item.completed"
       (pcase item-type
         ((or "agent_message" "message")
          (ai-workbench-magent-cli--answer-delta
           run (or (ai-workbench-magent-cli--json-get item "text")
                   (ai-workbench-magent-cli--json-path item "content" "text"))))
         ((or "reasoning" "analysis")
          (ai-workbench-magent-cli--answer-delta
           run (or (ai-workbench-magent-cli--json-get item "text")
                   (ai-workbench-magent-cli--json-path item "content" "text")) t))
         (_ (ai-workbench-magent-cli--heartbeat run object item-type))))
      ("turn.completed"
       (ai-workbench-magent-cli--finish-success
        run (ai-workbench-magent-cli--json-get object "usage")))
      ((or "turn.failed" "error")
       (ai-workbench-magent-cli--finish-error
        run (or (ai-workbench-magent-cli--json-get object "message")
                (ai-workbench-magent-cli--json-path object "error" "message")
                "Codex CLI reported an error") object))
      (_ (ai-workbench-magent-cli--heartbeat run object type)))))

(defun ai-workbench-magent-cli--claude-content-block (run block)
  "Map a Claude content BLOCK for RUN."
  (let ((type (ai-workbench-magent-cli--json-get block "type")))
    (pcase type
      ((or "text" "text_delta")
       (ai-workbench-magent-cli--answer-delta
        run (ai-workbench-magent-cli--json-get block "text")))
      ((or "thinking" "thinking_delta")
       (ai-workbench-magent-cli--answer-delta
        run (or (ai-workbench-magent-cli--json-get block "thinking")
                (ai-workbench-magent-cli--json-get block "text")) t))
      (_ nil))))

(defun ai-workbench-magent-cli--claude-event (run object)
  "Map one Claude stream JSON OBJECT into normalized events for RUN."
  (let* ((type (ai-workbench-magent-cli--json-get object "type"))
         (event (ai-workbench-magent-cli--json-get object "event"))
         (event-type (ai-workbench-magent-cli--json-get event "type")))
    (ai-workbench-magent-cli--persist-session-id
     run (or (ai-workbench-magent-cli--json-get object "session_id")
             (ai-workbench-magent-cli--json-get object "sessionId")))
    (cond
     ((equal type "stream_event")
      (pcase event-type
        ("content_block_delta"
         (ai-workbench-magent-cli--claude-content-block
          run (ai-workbench-magent-cli--json-get event "delta")))
        (_ (ai-workbench-magent-cli--heartbeat run object event-type))))
     ((equal type "assistant")
      ;; Claude emits this full message as well as stream deltas.  Use it only
      ;; when no streamed text was seen, preventing duplicated transcript text.
      (unless (ai-workbench-magent-cli-run-text-seen run)
        (dolist (block (append (ai-workbench-magent-cli--json-path
                                object "message" "content") nil))
          (ai-workbench-magent-cli--claude-content-block run block))))
     ((equal type "result")
      (if (ai-workbench-magent-cli--json-get object "is_error")
          (ai-workbench-magent-cli--finish-error
           run (or (ai-workbench-magent-cli--json-get object "result")
                   "Claude CLI reported an error") object)
        (unless (ai-workbench-magent-cli-run-text-seen run)
          (ai-workbench-magent-cli--answer-delta
           run (ai-workbench-magent-cli--json-get object "result")))
        (ai-workbench-magent-cli--finish-success
         run (ai-workbench-magent-cli--json-get object "usage"))))
     (t (ai-workbench-magent-cli--heartbeat run object type)))))

(defun ai-workbench-magent-cli--opencode-event (run object)
  "Map one OpenCode JSON OBJECT into normalized events for RUN."
  (let* ((type (ai-workbench-magent-cli--json-get object "type"))
         (part (or (ai-workbench-magent-cli--json-get object "part") object))
         (part-type (or (ai-workbench-magent-cli--json-get part "type") type)))
    (ai-workbench-magent-cli--persist-session-id
     run (or (ai-workbench-magent-cli--json-get object "sessionID")
             (ai-workbench-magent-cli--json-get object "session_id")
             (ai-workbench-magent-cli--json-get object "sessionId")
             (ai-workbench-magent-cli--json-get part "sessionID")))
    (pcase part-type
      ((or "text" "text_delta")
       (ai-workbench-magent-cli--answer-delta
        run (or (ai-workbench-magent-cli--json-get part "text")
                (ai-workbench-magent-cli--json-get object "text"))))
      ((or "reasoning" "thinking" "analysis")
       (ai-workbench-magent-cli--answer-delta
        run (or (ai-workbench-magent-cli--json-get part "text")
                (ai-workbench-magent-cli--json-get part "thinking")) t))
      ((or "error" "session.error")
       (ai-workbench-magent-cli--finish-error
        run (or (ai-workbench-magent-cli--json-get object "message")
                (ai-workbench-magent-cli--json-path object "error" "message")
                "OpenCode reported an error") object))
      (_ (ai-workbench-magent-cli--heartbeat run object part-type)))))

(defun ai-workbench-magent-cli--parse-line (run line)
  "Parse and dispatch one JSON LINE for RUN."
  (unless (string-empty-p (string-trim line))
    (condition-case err
        (let ((object (json-parse-string line :object-type 'plist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)))
          (pcase (ai-workbench-magent-cli-run-engine run)
            ('codex (ai-workbench-magent-cli--codex-event run object))
            ('claude (ai-workbench-magent-cli--claude-event run object))
            ('opencode (ai-workbench-magent-cli--opencode-event run object))))
      (error
       (ai-workbench-magent-cli--diagnostic
        run (format "Malformed CLI JSON: %s\n%s\n"
                    (error-message-string err) line))))))

(defun ai-workbench-magent-cli--filter (run chunk)
  "Consume a process output CHUNK for RUN incrementally."
  (unless (ai-workbench-magent-cli-run-terminal-seen run)
    (let ((pending (concat (ai-workbench-magent-cli-run-pending run) chunk))
          (start 0)
          newline)
      ;; Scan with an offset and slice the remainder once.  Repeatedly slicing
      ;; a shrinking tail makes one large multi-line chunk quadratic.
      (while (and (not (ai-workbench-magent-cli-run-terminal-seen run))
                  (setq newline (string-search "\n" pending start)))
        (ai-workbench-magent-cli--parse-line
         run (substring pending start newline))
        (setq start (1+ newline)))
      (setq pending (substring pending start))
      (if (> (string-bytes pending) ai-workbench-magent-cli-max-json-line-bytes)
          (progn
            (setf (ai-workbench-magent-cli-run-pending run) "")
            (ai-workbench-magent-cli--finish-error
             run "CLI JSON line exceeded ai-workbench's parser limit"
             (list :status 'json-line-too-large))
            (when-let* ((process (ai-workbench-magent-cli-run-process run)))
              (when (process-live-p process) (delete-process process))))
        (setf (ai-workbench-magent-cli-run-pending run) pending)))))

(defun ai-workbench-magent-cli--stderr-tail (run)
  "Return bounded stderr text retained for RUN."
  (when-let* ((buffer (ai-workbench-magent-cli-run-stderr-buffer run))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-workbench-magent-cli--cleanup (run)
  "Release transient buffers owned by RUN."
  (when-let* ((buffer (ai-workbench-magent-cli-run-stderr-buffer run)))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (setf (ai-workbench-magent-cli-run-stderr-buffer run) nil
        (ai-workbench-magent-cli-run-pending run) ""
        (ai-workbench-magent-cli-run-diagnostic-chunks run) nil
        (ai-workbench-magent-cli-run-answer-chunks run) nil))

(defun ai-workbench-magent-cli--sentinel (run process _event)
  "Finalize RUN when PROCESS exits."
  (when (memq (process-status process) '(exit signal failed))
    (let ((pending (ai-workbench-magent-cli-run-pending run)))
      (when (and (not (ai-workbench-magent-cli-run-terminal-seen run))
                 (not (string-empty-p (string-trim pending))))
        (ai-workbench-magent-cli--parse-line run pending)))
    (unless (ai-workbench-magent-cli-run-terminal-seen run)
      (if (and (eq (process-status process) 'exit)
               (= (process-exit-status process) 0)
               (ai-workbench-magent-cli-run-text-seen run))
          (ai-workbench-magent-cli--finish-success run)
        (let* ((stderr (string-trim (or (ai-workbench-magent-cli--stderr-tail run) "")))
               (diagnostic (string-trim
                            (or (car (ai-workbench-magent-cli-run-diagnostic-chunks run))
                                "")))
               (detail (or (ai-workbench-magent-cli--string stderr)
                           (ai-workbench-magent-cli--string diagnostic)
                           "no structured assistant output")))
          (ai-workbench-magent-cli--finish-error
           run
           (format "%s CLI exited with status %s: %s"
                   (capitalize (symbol-name (ai-workbench-magent-cli-run-engine run)))
                   (process-exit-status process) detail)
           (list :status 'cli-exit :exit-code (process-exit-status process))))))
    (ai-workbench-magent-cli--cleanup run)))

(defun ai-workbench-magent-cli--trim-stderr ()
  "Keep the current transient stderr buffer within its fixed cap."
  (let ((bytes (1- (position-bytes (point-max)))))
    (when (> bytes ai-workbench-magent-cli-max-diagnostic-bytes)
      (let* ((inhibit-modification-hooks t)
             (first-kept-byte
              (- (position-bytes (point-max))
                 ai-workbench-magent-cli-max-diagnostic-bytes))
             (first-kept-position
              (or (byte-to-position first-kept-byte) (point-min))))
        (delete-region (point-min) first-kept-position)))))

(defun ai-workbench-magent-cli--command (run prompt)
  "Return the structured CLI command for RUN and PROMPT."
  (let* ((engine (ai-workbench-magent-cli-run-engine run))
         (root (ai-workbench-magent-cli-run-root run))
         (session-id (ai-workbench-magent-cli-run-session-id run)))
    (pcase engine
      ('codex
       (if session-id
           (append (list (or (and (boundp 'ai-workbench-codex-executable)
                                  ai-workbench-codex-executable)
                             "codex")
                         "exec" "resume" "--json" "--skip-git-repo-check")
                   (and (boundp 'ai-workbench-codex-extra-args)
                        ai-workbench-codex-extra-args)
                   (list session-id prompt))
         (append (list (or (and (boundp 'ai-workbench-codex-executable)
                                ai-workbench-codex-executable)
                           "codex")
                       "exec" "--json" "--skip-git-repo-check" "--color" "never"
                       "-s" "workspace-write" "-C" root)
                 (and (boundp 'ai-workbench-codex-extra-args)
                      ai-workbench-codex-extra-args)
                 (list prompt))))
      ('claude
       (append (list (or (and (boundp 'ai-workbench-claude-executable)
                              ai-workbench-claude-executable)
                         "claude")
                     "-p" "--output-format" "stream-json"
                     "--include-partial-messages" "--verbose")
               (and session-id (list "--resume" session-id))
               ai-workbench-claude-extra-args
               (list prompt)))
      ('opencode
       (append (list (or (and (boundp 'ai-workbench-opencode-executable)
                              ai-workbench-opencode-executable)
                         "opencode")
                     "run" "--format" "json" "--dir" root)
               (and session-id (list "-s" session-id))
               (and (boundp 'ai-workbench-opencode-extra-args)
                    ai-workbench-opencode-extra-args)
               (list prompt)))
      (_ (error "Unsupported Magent CLI engine: %S" engine)))))

(defun ai-workbench-magent-cli--start (engine root runtime-session request)
  "Start ENGINE for ROOT and RUNTIME-SESSION using Magent REQUEST."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (session-id (magent-session-metadata-value
                      session (ai-workbench-magent-cli--metadata-key engine)))
         (stderr-buffer (generate-new-buffer " *ai-workbench-cli-stderr*"))
         (run (ai-workbench-magent-cli-run-create
               :engine engine :root root :runtime-session runtime-session
               :request request :stderr-buffer stderr-buffer :pending ""
               :diagnostic-bytes 0 :answer-bytes 0 :session-id session-id)))
    (with-current-buffer stderr-buffer
      (add-hook 'after-change-functions
                (lambda (&rest _) (ai-workbench-magent-cli--trim-stderr))
                nil t))
    (condition-case err
        (let* ((prompt (ai-workbench-magent-cli--effective-prompt request engine))
               (command (ai-workbench-magent-cli--command run prompt))
               (default-directory root)
               (process
                (make-process
                 :name (format "ai-workbench-%s" engine)
                 :command command
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :stderr stderr-buffer
                 :filter (lambda (_process chunk)
                           (ai-workbench-magent-cli--filter run chunk))
                 :sentinel (lambda (process event)
                             (ai-workbench-magent-cli--sentinel run process event)))))
          (setf (ai-workbench-magent-cli-run-process run) process)
          process)
      (error
       (ai-workbench-magent-cli--finish-error
        run (format "Cannot start %s CLI: %s"
                    (capitalize (symbol-name engine))
                    (error-message-string err))
        (list :status 'process-start-error))
       (ai-workbench-magent-cli--cleanup run)
       nil))))

(defun ai-workbench-magent-cli-sampler (engine root runtime-session)
  "Return a Magent sampler backed by ENGINE at ROOT for RUNTIME-SESSION."
  (unless (memq engine '(codex claude opencode))
    (error "Unsupported Magent CLI engine: %S" engine))
  (let ((canonical-root
         (file-name-as-directory (file-truename (expand-file-name root)))))
    (lambda (request)
      (ai-workbench-magent-cli--start
       engine canonical-root runtime-session request))))

(provide 'ai-workbench-magent-cli)
;;; ai-workbench-magent-cli.el ends here
