;;; tools/agent/interaction-magent-cli.el --- Structured CLI samplers for Magent -*- lexical-binding: t; -*-

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

(defgroup +agent-interaction-magent-cli nil
  "External coding-agent samplers managed by Magent."
  :group '+agent-interaction
  :prefix "+agent-interaction-magent-cli-")

(defcustom +agent-interaction-magent-cli-max-json-line-bytes (* 1024 1024)
  "Maximum buffered bytes for one CLI JSON line."
  :type 'integer
  :group '+agent-interaction-magent-cli)

(defcustom +agent-interaction-magent-cli-max-answer-bytes (* 8 1024 1024)
  "Maximum assistant text accepted from one CLI turn."
  :type 'integer
  :group '+agent-interaction-magent-cli)

(defcustom +agent-interaction-magent-cli-max-diagnostic-bytes (* 256 1024)
  "Maximum stderr and malformed-output diagnostic bytes retained per turn."
  :type 'integer
  :group '+agent-interaction-magent-cli)

(defcustom +agent-interaction-magent-cli-max-prompt-bytes (* 4 1024 1024)
  "Maximum combined Magent context and user prompt sent to one CLI turn."
  :type 'integer
  :group '+agent-interaction-magent-cli)

(defcustom +agent-interaction-claude-extra-args nil
  "Additional arguments passed to structured Claude Code requests."
  :type '(repeat string)
  :group '+agent-interaction-magent-cli)

(cl-defstruct (+agent-interaction-magent-cli-run
               (:constructor +agent-interaction-magent-cli-run-create)
               (:copier nil))
  engine root runtime-session request process stderr-buffer pending
  diagnostic-chunks diagnostic-bytes answer-chunks answer-bytes
  text-seen reasoning-open terminal-seen session-id)

(defun +agent-interaction-magent-cli--json-get (object key)
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

(defun +agent-interaction-magent-cli--json-path (object &rest keys)
  "Return the value below KEYS in JSON OBJECT."
  (dolist (key keys object)
    (setq object (and object (+agent-interaction-magent-cli--json-get object key)))))

(defun +agent-interaction-magent-cli--string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun +agent-interaction-magent-cli--content-string (content)
  "Return a compact textual representation of prompt CONTENT."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat #'+agent-interaction-magent-cli--content-string (append content nil) "\n"))
   ((and (listp content)
         (+agent-interaction-magent-cli--json-get content "text"))
    (or (+agent-interaction-magent-cli--string
         (+agent-interaction-magent-cli--json-get content "text"))
        ""))
   ((listp content)
    (mapconcat #'+agent-interaction-magent-cli--content-string content "\n"))
   ((null content) "")
   (t (format "%s" content))))

(defun +agent-interaction-magent-cli--latest-prompt (request)
  "Return the most recent user prompt from Magent REQUEST."
  (let ((prompt (magent-llm-request-prompt request)))
    (if (stringp prompt)
        prompt
      (or (cl-loop for entry in (reverse (append prompt nil))
                   when (eq (car-safe entry) 'prompt)
                   return (+agent-interaction-magent-cli--content-string (cdr entry)))
          (+agent-interaction-magent-cli--content-string prompt)))))

(defun +agent-interaction-magent-cli--effective-prompt (request engine)
  "Build the external coding-agent prompt for REQUEST and ENGINE."
  (let* ((prompt (string-trim (+agent-interaction-magent-cli--latest-prompt request)))
         (system (+agent-interaction-magent-cli--string
                  (magent-llm-request-system request)))
         (combined
          (concat
           (when system
             (concat "--- Magent-managed context ---\n" system
                     "\n\n--- User request ---\n"))
           prompt
           "\n\n--- +agent-interaction runtime boundary ---\n"
           (format "You are running through the %s CLI. " engine)
           "Use your own native tools and permission system. "
           "Do not emit Magent tool-call syntax; return a normal final answer.")))
    (when (> (string-bytes combined)
             +agent-interaction-magent-cli-max-prompt-bytes)
      (error "Combined Magent/CLI prompt exceeds %d bytes"
             +agent-interaction-magent-cli-max-prompt-bytes))
    combined))

(defun +agent-interaction-magent-cli--metadata-key (engine)
  "Return the persisted upstream session metadata key for ENGINE."
  (intern (format "+agent-interaction-%s-session-id" engine)))

(defun +agent-interaction-magent-cli--session (run)
  "Return RUN's underlying persistent Magent session."
  (magent-runtime-session-magent-session
   (+agent-interaction-magent-cli-run-runtime-session run)))

(defun +agent-interaction-magent-cli--persist-session-id (run session-id)
  "Persist SESSION-ID for RUN when it changed."
  (when-let* ((id (+agent-interaction-magent-cli--string session-id))
              (session (+agent-interaction-magent-cli--session run)))
    (unless (equal id (+agent-interaction-magent-cli-run-session-id run))
      (setf (+agent-interaction-magent-cli-run-session-id run) id)
      (magent-session-set-metadata-value
       session (+agent-interaction-magent-cli--metadata-key
                (+agent-interaction-magent-cli-run-engine run))
       id)
      (magent-session-save-deferred-for-session
       session
       (magent-runtime-session-scope
        (+agent-interaction-magent-cli-run-runtime-session run))))))

(defun +agent-interaction-magent-cli--callback (run event)
  "Send normalized EVENT to RUN's request callback."
  (when-let* ((callback (magent-llm-request-callback
                         (+agent-interaction-magent-cli-run-request run))))
    (funcall callback event)))

(defun +agent-interaction-magent-cli--finish-error (run message &optional metadata)
  "Finish RUN with MESSAGE and optional METADATA."
  (unless (+agent-interaction-magent-cli-run-terminal-seen run)
    (setf (+agent-interaction-magent-cli-run-terminal-seen run) t)
    (+agent-interaction-magent-cli--callback
     run (magent-llm-error-event message metadata))))

(defun +agent-interaction-magent-cli--finish-success (run &optional usage)
  "Finish RUN successfully with optional USAGE."
  (unless (+agent-interaction-magent-cli-run-terminal-seen run)
    (setf (+agent-interaction-magent-cli-run-terminal-seen run) t)
    (when (+agent-interaction-magent-cli-run-reasoning-open run)
      (setf (+agent-interaction-magent-cli-run-reasoning-open run) nil)
      (+agent-interaction-magent-cli--callback run (magent-llm-reasoning-end-event)))
    (+agent-interaction-magent-cli--callback
     run (magent-llm-completed-event nil usage 'stop
                                     (list :engine
                                           (+agent-interaction-magent-cli-run-engine run))))))

(defun +agent-interaction-magent-cli--answer-delta (run text &optional reasoning)
  "Emit TEXT for RUN, as REASONING when non-nil, enforcing the answer cap."
  (when-let* ((value (+agent-interaction-magent-cli--string text)))
    (let ((new-size (+ (+agent-interaction-magent-cli-run-answer-bytes run)
                       (string-bytes value))))
      (if (> new-size +agent-interaction-magent-cli-max-answer-bytes)
          (progn
            (+agent-interaction-magent-cli--finish-error
             run "CLI response exceeded +agent-interaction's per-turn size limit"
             (list :status 'response-too-large :bytes new-size))
            (when-let* ((process (+agent-interaction-magent-cli-run-process run)))
              (when (process-live-p process) (delete-process process))))
        ;; Magent's loop and output marker already own the streamed text.  Keep
        ;; only a byte counter here rather than retaining a second large copy.
        (setf (+agent-interaction-magent-cli-run-answer-bytes run) new-size)
        (if reasoning
            (progn
              (setf (+agent-interaction-magent-cli-run-reasoning-open run) t)
              (+agent-interaction-magent-cli--callback
               run (magent-llm-reasoning-delta-event value)))
          (setf (+agent-interaction-magent-cli-run-text-seen run) t)
          (when (+agent-interaction-magent-cli-run-reasoning-open run)
            (setf (+agent-interaction-magent-cli-run-reasoning-open run) nil)
            (+agent-interaction-magent-cli--callback run (magent-llm-reasoning-end-event)))
          (+agent-interaction-magent-cli--callback
           run (magent-llm-text-delta-event value)))))))

(defun +agent-interaction-magent-cli--heartbeat (run object &optional label)
  "Emit a bounded progress heartbeat for RUN based on OBJECT and LABEL."
  (+agent-interaction-magent-cli--callback
   run (magent-llm-event-create
        'usage :usage (list :engine (+agent-interaction-magent-cli-run-engine run)
                            :progress (or label "event"))
        :raw object)))

(defun +agent-interaction-magent-cli--diagnostic (run text)
  "Retain a bounded diagnostic tail TEXT for RUN."
  (when (stringp text)
    (let* ((limit +agent-interaction-magent-cli-max-diagnostic-bytes)
           (old (or (car (+agent-interaction-magent-cli-run-diagnostic-chunks run))
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
      (setf (+agent-interaction-magent-cli-run-diagnostic-chunks run) (list tail)
            (+agent-interaction-magent-cli-run-diagnostic-bytes run) (string-bytes tail)))))

(defun +agent-interaction-magent-cli--codex-event (run object)
  "Map one Codex JSON OBJECT into normalized events for RUN."
  (let* ((type (+agent-interaction-magent-cli--json-get object "type"))
         (item (+agent-interaction-magent-cli--json-get object "item"))
         (item-type (+agent-interaction-magent-cli--json-get item "type")))
    (pcase type
      ("thread.started"
       (+agent-interaction-magent-cli--persist-session-id
        run (+agent-interaction-magent-cli--json-get object "thread_id")))
      ("item.completed"
       (pcase item-type
         ((or "agent_message" "message")
          (+agent-interaction-magent-cli--answer-delta
           run (or (+agent-interaction-magent-cli--json-get item "text")
                   (+agent-interaction-magent-cli--json-path item "content" "text"))))
         ((or "reasoning" "analysis")
          (+agent-interaction-magent-cli--answer-delta
           run (or (+agent-interaction-magent-cli--json-get item "text")
                   (+agent-interaction-magent-cli--json-path item "content" "text")) t))
         (_ (+agent-interaction-magent-cli--heartbeat run object item-type))))
      ("turn.completed"
       (+agent-interaction-magent-cli--finish-success
        run (+agent-interaction-magent-cli--json-get object "usage")))
      ((or "turn.failed" "error")
       (+agent-interaction-magent-cli--finish-error
        run (or (+agent-interaction-magent-cli--json-get object "message")
                (+agent-interaction-magent-cli--json-path object "error" "message")
                "Codex CLI reported an error") object))
      (_ (+agent-interaction-magent-cli--heartbeat run object type)))))

(defun +agent-interaction-magent-cli--claude-content-block (run block)
  "Map a Claude content BLOCK for RUN."
  (let ((type (+agent-interaction-magent-cli--json-get block "type")))
    (pcase type
      ((or "text" "text_delta")
       (+agent-interaction-magent-cli--answer-delta
        run (+agent-interaction-magent-cli--json-get block "text")))
      ((or "thinking" "thinking_delta")
       (+agent-interaction-magent-cli--answer-delta
        run (or (+agent-interaction-magent-cli--json-get block "thinking")
                (+agent-interaction-magent-cli--json-get block "text")) t))
      (_ nil))))

(defun +agent-interaction-magent-cli--claude-event (run object)
  "Map one Claude stream JSON OBJECT into normalized events for RUN."
  (let* ((type (+agent-interaction-magent-cli--json-get object "type"))
         (event (+agent-interaction-magent-cli--json-get object "event"))
         (event-type (+agent-interaction-magent-cli--json-get event "type")))
    (+agent-interaction-magent-cli--persist-session-id
     run (or (+agent-interaction-magent-cli--json-get object "session_id")
             (+agent-interaction-magent-cli--json-get object "sessionId")))
    (cond
     ((equal type "stream_event")
      (pcase event-type
        ("content_block_delta"
         (+agent-interaction-magent-cli--claude-content-block
          run (+agent-interaction-magent-cli--json-get event "delta")))
        (_ (+agent-interaction-magent-cli--heartbeat run object event-type))))
     ((equal type "assistant")
      ;; Claude emits this full message as well as stream deltas.  Use it only
      ;; when no streamed text was seen, preventing duplicated transcript text.
      (unless (+agent-interaction-magent-cli-run-text-seen run)
        (dolist (block (append (+agent-interaction-magent-cli--json-path
                                object "message" "content") nil))
          (+agent-interaction-magent-cli--claude-content-block run block))))
     ((equal type "result")
      (if (+agent-interaction-magent-cli--json-get object "is_error")
          (+agent-interaction-magent-cli--finish-error
           run (or (+agent-interaction-magent-cli--json-get object "result")
                   "Claude CLI reported an error") object)
        (unless (+agent-interaction-magent-cli-run-text-seen run)
          (+agent-interaction-magent-cli--answer-delta
           run (+agent-interaction-magent-cli--json-get object "result")))
        (+agent-interaction-magent-cli--finish-success
         run (+agent-interaction-magent-cli--json-get object "usage"))))
     (t (+agent-interaction-magent-cli--heartbeat run object type)))))

(defun +agent-interaction-magent-cli--opencode-event (run object)
  "Map one OpenCode JSON OBJECT into normalized events for RUN."
  (let* ((type (+agent-interaction-magent-cli--json-get object "type"))
         (part (or (+agent-interaction-magent-cli--json-get object "part") object))
         (part-type (or (+agent-interaction-magent-cli--json-get part "type") type)))
    (+agent-interaction-magent-cli--persist-session-id
     run (or (+agent-interaction-magent-cli--json-get object "sessionID")
             (+agent-interaction-magent-cli--json-get object "session_id")
             (+agent-interaction-magent-cli--json-get object "sessionId")
             (+agent-interaction-magent-cli--json-get part "sessionID")))
    (pcase part-type
      ((or "text" "text_delta")
       (+agent-interaction-magent-cli--answer-delta
        run (or (+agent-interaction-magent-cli--json-get part "text")
                (+agent-interaction-magent-cli--json-get object "text"))))
      ((or "reasoning" "thinking" "analysis")
       (+agent-interaction-magent-cli--answer-delta
        run (or (+agent-interaction-magent-cli--json-get part "text")
                (+agent-interaction-magent-cli--json-get part "thinking")) t))
      ((or "error" "session.error")
       (+agent-interaction-magent-cli--finish-error
        run (or (+agent-interaction-magent-cli--json-get object "message")
                (+agent-interaction-magent-cli--json-path object "error" "message")
                "OpenCode reported an error") object))
      (_ (+agent-interaction-magent-cli--heartbeat run object part-type)))))

(defun +agent-interaction-magent-cli--parse-line (run line)
  "Parse and dispatch one JSON LINE for RUN."
  (unless (string-empty-p (string-trim line))
    (condition-case err
        (let ((object (json-parse-string line :object-type 'plist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)))
          (pcase (+agent-interaction-magent-cli-run-engine run)
            ('codex (+agent-interaction-magent-cli--codex-event run object))
            ('claude (+agent-interaction-magent-cli--claude-event run object))
            ('opencode (+agent-interaction-magent-cli--opencode-event run object))))
      (error
       (+agent-interaction-magent-cli--diagnostic
        run (format "Malformed CLI JSON: %s\n%s\n"
                    (error-message-string err) line))))))

(defun +agent-interaction-magent-cli--filter (run chunk)
  "Consume a process output CHUNK for RUN incrementally."
  (unless (+agent-interaction-magent-cli-run-terminal-seen run)
    (let ((pending (concat (+agent-interaction-magent-cli-run-pending run) chunk))
          (start 0)
          newline)
      ;; Scan with an offset and slice the remainder once.  Repeatedly slicing
      ;; a shrinking tail makes one large multi-line chunk quadratic.
      (while (and (not (+agent-interaction-magent-cli-run-terminal-seen run))
                  (setq newline (string-search "\n" pending start)))
        (+agent-interaction-magent-cli--parse-line
         run (substring pending start newline))
        (setq start (1+ newline)))
      (setq pending (substring pending start))
      (if (> (string-bytes pending) +agent-interaction-magent-cli-max-json-line-bytes)
          (progn
            (setf (+agent-interaction-magent-cli-run-pending run) "")
            (+agent-interaction-magent-cli--finish-error
             run "CLI JSON line exceeded +agent-interaction's parser limit"
             (list :status 'json-line-too-large))
            (when-let* ((process (+agent-interaction-magent-cli-run-process run)))
              (when (process-live-p process) (delete-process process))))
        (setf (+agent-interaction-magent-cli-run-pending run) pending)))))

(defun +agent-interaction-magent-cli--stderr-tail (run)
  "Return bounded stderr text retained for RUN."
  (when-let* ((buffer (+agent-interaction-magent-cli-run-stderr-buffer run))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun +agent-interaction-magent-cli--cleanup (run)
  "Release transient buffers owned by RUN."
  (when-let* ((buffer (+agent-interaction-magent-cli-run-stderr-buffer run)))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (setf (+agent-interaction-magent-cli-run-stderr-buffer run) nil
        (+agent-interaction-magent-cli-run-pending run) ""
        (+agent-interaction-magent-cli-run-diagnostic-chunks run) nil
        (+agent-interaction-magent-cli-run-answer-chunks run) nil))

(defun +agent-interaction-magent-cli--sentinel (run process _event)
  "Finalize RUN when PROCESS exits."
  (when (memq (process-status process) '(exit signal failed))
    (let ((pending (+agent-interaction-magent-cli-run-pending run)))
      (when (and (not (+agent-interaction-magent-cli-run-terminal-seen run))
                 (not (string-empty-p (string-trim pending))))
        (+agent-interaction-magent-cli--parse-line run pending)))
    (unless (+agent-interaction-magent-cli-run-terminal-seen run)
      (if (and (eq (process-status process) 'exit)
               (= (process-exit-status process) 0)
               (+agent-interaction-magent-cli-run-text-seen run))
          (+agent-interaction-magent-cli--finish-success run)
        (let* ((stderr (string-trim (or (+agent-interaction-magent-cli--stderr-tail run) "")))
               (diagnostic (string-trim
                            (or (car (+agent-interaction-magent-cli-run-diagnostic-chunks run))
                                "")))
               (detail (or (+agent-interaction-magent-cli--string stderr)
                           (+agent-interaction-magent-cli--string diagnostic)
                           "no structured assistant output")))
          (+agent-interaction-magent-cli--finish-error
           run
           (format "%s CLI exited with status %s: %s"
                   (capitalize (symbol-name (+agent-interaction-magent-cli-run-engine run)))
                   (process-exit-status process) detail)
           (list :status 'cli-exit :exit-code (process-exit-status process))))))
    (+agent-interaction-magent-cli--cleanup run)))

(defun +agent-interaction-magent-cli--trim-stderr ()
  "Keep the current transient stderr buffer within its fixed cap."
  (let ((bytes (1- (position-bytes (point-max)))))
    (when (> bytes +agent-interaction-magent-cli-max-diagnostic-bytes)
      (let* ((inhibit-modification-hooks t)
             (first-kept-byte
              (- (position-bytes (point-max))
                 +agent-interaction-magent-cli-max-diagnostic-bytes))
             (first-kept-position
              (or (byte-to-position first-kept-byte) (point-min))))
        (delete-region (point-min) first-kept-position)))))

(defun +agent-interaction-magent-cli--command (run prompt)
  "Return the structured CLI command for RUN and PROMPT."
  (let* ((engine (+agent-interaction-magent-cli-run-engine run))
         (root (+agent-interaction-magent-cli-run-root run))
         (session-id (+agent-interaction-magent-cli-run-session-id run)))
    (pcase engine
      ('codex
       (if session-id
           (append (list (or (and (boundp '+agent-interaction-codex-executable)
                                  +agent-interaction-codex-executable)
                             "codex")
                         "exec" "resume" "--json" "--skip-git-repo-check")
                   (and (boundp '+agent-interaction-codex-extra-args)
                        +agent-interaction-codex-extra-args)
                   (list session-id prompt))
         (append (list (or (and (boundp '+agent-interaction-codex-executable)
                                +agent-interaction-codex-executable)
                           "codex")
                       "exec" "--json" "--skip-git-repo-check" "--color" "never"
                       "-s" "workspace-write" "-C" root)
                 (and (boundp '+agent-interaction-codex-extra-args)
                      +agent-interaction-codex-extra-args)
                 (list prompt))))
      ('claude
       (append (list (or (and (boundp '+agent-interaction-claude-executable)
                              +agent-interaction-claude-executable)
                         "claude")
                     "-p" "--output-format" "stream-json"
                     "--include-partial-messages" "--verbose")
               (and session-id (list "--resume" session-id))
               +agent-interaction-claude-extra-args
               (list prompt)))
      ('opencode
       (append (list (or (and (boundp '+agent-interaction-opencode-executable)
                              +agent-interaction-opencode-executable)
                         "opencode")
                     "run" "--format" "json" "--dir" root)
               (and session-id (list "-s" session-id))
               (and (boundp '+agent-interaction-opencode-extra-args)
                    +agent-interaction-opencode-extra-args)
               (list prompt)))
      (_ (error "Unsupported Magent CLI engine: %S" engine)))))

(defun +agent-interaction-magent-cli--start (engine root runtime-session request)
  "Start ENGINE for ROOT and RUNTIME-SESSION using Magent REQUEST."
  (let* ((session (magent-runtime-session-magent-session runtime-session))
         (session-id (magent-session-metadata-value
                      session (+agent-interaction-magent-cli--metadata-key engine)))
         (stderr-buffer (generate-new-buffer " *+agent-interaction-cli-stderr*"))
         (run (+agent-interaction-magent-cli-run-create
               :engine engine :root root :runtime-session runtime-session
               :request request :stderr-buffer stderr-buffer :pending ""
               :diagnostic-bytes 0 :answer-bytes 0 :session-id session-id)))
    (with-current-buffer stderr-buffer
      (add-hook 'after-change-functions
                (lambda (&rest _) (+agent-interaction-magent-cli--trim-stderr))
                nil t))
    (condition-case err
        (let* ((prompt (+agent-interaction-magent-cli--effective-prompt request engine))
               (command (+agent-interaction-magent-cli--command run prompt))
               (default-directory root)
               (process
                (make-process
                 :name (format "+agent-interaction-%s" engine)
                 :command command
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :stderr stderr-buffer
                 :filter (lambda (_process chunk)
                           (+agent-interaction-magent-cli--filter run chunk))
                 :sentinel (lambda (process event)
                             (+agent-interaction-magent-cli--sentinel run process event)))))
          (setf (+agent-interaction-magent-cli-run-process run) process)
          process)
      (error
       (+agent-interaction-magent-cli--finish-error
        run (format "Cannot start %s CLI: %s"
                    (capitalize (symbol-name engine))
                    (error-message-string err))
        (list :status 'process-start-error))
       (+agent-interaction-magent-cli--cleanup run)
       nil))))

(defun +agent-interaction-magent-cli-sampler (engine root runtime-session)
  "Return a Magent sampler backed by ENGINE at ROOT for RUNTIME-SESSION."
  (unless (memq engine '(codex claude opencode))
    (error "Unsupported Magent CLI engine: %S" engine))
  (let ((canonical-root
         (file-name-as-directory (file-truename (expand-file-name root)))))
    (lambda (request)
      (+agent-interaction-magent-cli--start
       engine canonical-root runtime-session request))))

(provide 'tools/agent/interaction-magent-cli)
;;; tools/agent/interaction-magent-cli.el ends here
