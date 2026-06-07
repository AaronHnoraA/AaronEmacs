;;; ai-workbench-engine-cli.el --- Use CLI agents as ai-workbench-engine backends -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Bridges the ai-workbench CLI agents (CC/Claude Code, Codex, OpenCode) into
;; ai-workbench-engine as first-class backends.  ai-workbench-engine is
;; natively HTTP-only; this module defines an `ai-workbench-cli' backend type
;; and a custom transport that drives the tool's headless one-shot exec
;; (`ai-workbench-cli-exec') instead of an HTTP request, then feeds the result
;; back through ai-workbench-engine's normal response pipeline.
;;
;; With this loaded, CC/Codex/OpenCode appear in `ai-workbench--known-backends'
;; alongside any other registered backends, so they can be selected from the
;; ai-workbench Hub and used directly from any ai-workbench-engine buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-workbench-engine)
(require 'ai-workbench-request)
(require 'ai-workbench-openai)
(require 'ai-workbench-cli)
(require 'ai-workbench-answer)
;; Adapters register their tool specs (codex/opencode/claude exec args).
(require 'ai-workbench-adapter-claude)
(require 'ai-workbench-adapter-codex)
(require 'ai-workbench-adapter-opencode)

(declare-function ai-workbench--insert-response "ai-workbench-engine" (response info &optional raw))
(declare-function ai-workbench--process-models  "ai-workbench-request" (models))
(declare-function ai-workbench--fsm-transition  "ai-workbench-request" (machine &optional new-state))
(declare-function ai-workbench-fsm-info         "ai-workbench-request" (fsm))
(declare-function ai-workbench-project-root     "ai-workbench-session" ())
(declare-function ai-workbench-cli-session-live-p "ai-workbench-cli" (id &optional project-root))
(declare-function ai-workbench-cli-buffer          "ai-workbench-cli" (id &optional project-root))
(declare-function ai-workbench-cli-send-prompt     "ai-workbench-cli" (id prompt &optional project-root))
(defvar ai-workbench--known-backends)
(defvar ai-workbench--request-alist)

;; ── Backend type ──────────────────────────────────────────────────────────────
;; Include ai-workbench-openai so the prompt-parsing / request-data machinery
;; (`ai-workbench--parse-buffer', `ai-workbench--request-data', …) is reused;
;; we only override the transport (`ai-workbench--get-response') and read the
;; resulting :messages.
(cl-defstruct (ai-workbench-cli (:constructor ai-workbench--make-cli)
                                (:copier nil)
                                (:include ai-workbench-openai))
  (tool-id nil :documentation "ai-workbench-cli tool id symbol (claude/codex/opencode)."))

(cl-defun ai-workbench-make-cli (name &key tool-id (models '(default)) (stream nil))
  "Register an ai-workbench-engine backend named NAME backed by CLI tool TOOL-ID.
MODELS is a list of model symbols (mostly cosmetic for CLI tools).
STREAM is accepted for interface symmetry but ignored; CLI exec is
one-shot and delivers the whole response at once."
  (let ((backend (ai-workbench--make-cli
                  :name name
                  :host "local"
                  :protocol "cli"
                  :endpoint ""
                  ;; Dummy key prevents the engine from querying auth-source.
                  ;; CLI backends never use HTTP authentication.
                  :key "cli-no-http-auth"
                  :stream stream
                  :models (ai-workbench--process-models models)
                  :url "cli://local"
                  :tool-id tool-id)))
    (prog1 backend
      (setf (alist-get name ai-workbench--known-backends nil nil #'equal) backend))))

;; ── Prompt assembly ───────────────────────────────────────────────────────────

(defun ai-workbench-cli--data->prompt (data)
  "Flatten the ai-workbench-engine request DATA (:messages list) into one text prompt.
System and assistant turns are labeled so the CLI tool has conversation
context; a lone user turn is passed through verbatim."
  (let ((messages (plist-get data :messages))
        (parts nil))
    (dolist (m (append messages nil))
      (let ((role (plist-get m :role))
            (content (plist-get m :content)))
        (when (and (stringp content) (not (string-empty-p content)))
          (push (if (and (stringp role) (not (equal role "user")))
                    (format "[%s]\n%s" role content)
                  content)
                parts))))
    (string-join (nreverse parts) "\n\n")))

(defun ai-workbench-cli--root (info)
  "Return the working directory for the request described by INFO."
  (let ((buf (plist-get info :buffer)))
    (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
      (or (ignore-errors (ai-workbench-project-root))
          default-directory))))

;; ── Transport ─────────────────────────────────────────────────────────────────

(defun ai-workbench-cli--finish (fsm proc callback result error)
  "Deliver RESULT (or ERROR) for FSM and advance the state machine.
PROC, when a live process, is unregistered from `ai-workbench--request-alist'.
CALLBACK is ai-workbench-engine's response insertion callback."
  (when (processp proc)
    (setf (alist-get proc ai-workbench--request-alist nil 'remove) nil))
  (let ((info (ai-workbench-fsm-info fsm)))
    (if error
        (progn
          (plist-put info :http-status "500")
          (plist-put info :status (if (stringp error) error "error"))
          (plist-put info :error (list :message (format "%s" error))))
      (plist-put info :http-status "200")
      (plist-put info :status "OK"))
    (ai-workbench--fsm-transition fsm)         ;WAIT -> TYPE
    (with-demoted-errors "ai-workbench cli callback error: %S"
      (funcall callback (and (not error) result) info))
    (ai-workbench--fsm-transition fsm)))       ;TYPE -> DONE / ERRS

(defun ai-workbench-cli--extract-answer (raw-output tool-id)
  "Return the answer-block content from RAW-OUTPUT, or RAW-OUTPUT on failure.
Parse failures are logged as warnings; raw output is preserved for debug."
  (let ((result (ai-workbench-parse-answer-block raw-output)))
    (pcase result
      (`(:ok . ,content) content)
      (`(:error . ,_)
       (display-warning
        '(ai-workbench ai-workbench-cli)
        (format "ai-workbench [%s]: no #+begin answer block in CLI output.\nRaw:\n%s"
                tool-id raw-output)
        :warning)
       raw-output))))

;; ── ANSI stripping ─────────────────────────────────────────────────────────────

(defun ai-workbench--strip-ansi (text)
  "Strip ANSI escape sequences from TEXT.
Vterm buffers contain terminal escape codes that interfere with
answer-block regex matching; this makes session-mode parsing reliable."
  (replace-regexp-in-string "\033\\[[0-9;]*[A-Za-z]" "" text))

;; ── Session routing ───────────────────────────────────────────────────────────

(defvar ai-workbench-engine-cli--session-timeout 180
  "Seconds to wait for an answer block from a managed vterm session.")

(defvar ai-workbench-engine-cli--session-idle-cycles 3
  "Consecutive polls with no buffer growth to declare output stable.")

(defun ai-workbench-engine-cli--session-request (tool-id prompt root fsm callback)
  "Pipe PROMPT into the live TOOL-ID vterm session and poll for answer block.
Calls FSM/CALLBACK when the answer block appears or the timeout expires.

Detection strategy (in order):
1. Polling for a `.done` file created by the agent in `var/ai-workbench/`.
2. `#+end answer` block in the vterm buffer (primary, with ANSI stripping).
3. Output stability: buffer unchanged for `ai-workbench-engine-cli--session-idle-cycles`
   polls, meaning the agent has likely finished.
4. Hard timeout (`ai-workbench-engine-cli--session-timeout`)."
  (let* ((session-buf (ai-workbench-cli-buffer tool-id root))
         (start-pos (with-current-buffer session-buf (point-max)))
         (timeout ai-workbench-engine-cli--session-timeout)
         (poll-interval 2.0)
         (prev-size 0)
         (idle-count 0)
         (elapsed 0)
         (temp-dir (locate-user-emacs-file "var/ai-workbench/"))
         (output-file (make-temp-file (expand-file-name "session-out-" temp-dir) nil ".txt"))
         (done-file (concat output-file ".done"))
         timer)
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (ignore-errors (delete-file output-file))
    (ignore-errors (delete-file done-file))

    ;; Inject file-based completion instructions
    (let ((injected-prompt
           (concat prompt
                   (format "\n\n[SYSTEM: When you have finished your response, you MUST write your full final response (including the #+begin answer block) to the file %s and then create an empty file at %s to signal completion. Both files must be written. Do not ask for confirmation.]"
                           (shell-quote-argument output-file)
                           (shell-quote-argument done-file)))))
      (ai-workbench-cli-send-prompt tool-id injected-prompt root))

    (setq timer
          (run-with-timer
           poll-interval poll-interval
           (lambda ()
             (setq elapsed (+ elapsed poll-interval))
             (let (result raw-text)
               ;; 1. Check for the file-based completion signal
               (if (file-exists-p done-file)
                   (progn
                     (when (file-exists-p output-file)
                       (with-temp-buffer
                         (insert-file-contents output-file)
                         (setq raw-text (buffer-string)))
                       (let* ((clean (ai-workbench--strip-ansi raw-text))
                              (parsed (ai-workbench-parse-answer-block clean)))
                         (if (and parsed (eq (car parsed) :ok))
                             (setq result (cdr parsed))
                           (setq result raw-text))))
                     (ignore-errors (delete-file done-file))
                     (ignore-errors (delete-file output-file)))
                 ;; 2. Fallback to scraping the vterm buffer
                 (when (buffer-live-p session-buf)
                   (with-current-buffer session-buf
                     (let* ((end (point-max))
                            (size (- end start-pos))
                            (text (when (> size 0)
                                    (buffer-substring-no-properties start-pos end))))
                       (when text
                         (setq raw-text text)
                         (let ((clean (ai-workbench--strip-ansi text))
                               (parsed (ai-workbench-parse-answer-block clean)))
                           (when (and parsed (eq (car parsed) :ok))
                             (setq result (cdr parsed)))))

                       ;; Stability detection: buffer hasn't grown for N cycles
                       (if (and (> size 0) (= size prev-size))
                           (setq idle-count (1+ idle-count))
                         (setq idle-count 0))
                       (setq prev-size size)

                       ;; Fallback: stable output without answer block
                       (when (and (not result)
                                  (> size 0)
                                  (>= idle-count ai-workbench-engine-cli--session-idle-cycles))
                         (setq result raw-text))))))

               (cond
                (result
                 (cancel-timer timer)
                 (ignore-errors (delete-file done-file))
                 (ignore-errors (delete-file output-file))
                 (ai-workbench-cli--finish fsm nil callback result nil))
                ((>= elapsed timeout)
                 (cancel-timer timer)
                 (ignore-errors (delete-file done-file))
                 (ignore-errors (delete-file output-file))
                 ;; Last-resort: use whatever text we have
                 (if raw-text
                     (ai-workbench-cli--finish fsm nil callback raw-text nil)
                   (ai-workbench-cli--finish
                    fsm nil callback nil
                    (format "Session timeout after %ds - no output seen" timeout)))))))))))

(cl-defmethod ai-workbench--get-response ((backend ai-workbench-cli) fsm)
  "Drive the request in FSM through BACKEND.
When a vterm session is live for this backend's tool-id, pipe the
prompt into it and poll the buffer for an answer block (session mode).
Otherwise fall back to a headless one-shot exec."
  (let* ((info (ai-workbench-fsm-info fsm))
         (tool-id (ai-workbench-cli-tool-id backend))
         (raw-prompt (ai-workbench-cli--data->prompt (plist-get info :data)))
         (prompt (ai-workbench-wrap-prompt-with-output-contract raw-prompt))
         (callback (or (plist-get info :callback) #'ai-workbench--insert-response))
         (root (ai-workbench-cli--root info)))
    (plist-put info :callback callback)
    (if (ai-workbench-cli-session-live-p tool-id root)
        ;; SESSION mode: reuse the running vterm, extract answer from buffer.
        (condition-case err
            (ai-workbench-engine-cli--session-request tool-id prompt root fsm callback)
          (error
           (ai-workbench-cli--finish fsm nil callback nil (error-message-string err))))
      ;; ONE-SHOT mode: headless subprocess, no interactive session needed.
      (let (proc)
        (condition-case err
            (setq proc
                  (ai-workbench-cli-exec
                   tool-id prompt
                   :root root
                   :callback
                   (lambda (result)
                     (ai-workbench-cli--finish
                      fsm proc callback
                      (ai-workbench-cli--extract-answer result tool-id)
                      nil))
                   :on-error
                   (lambda (event details)
                     (ai-workbench-cli--finish fsm proc callback nil
                                               (if (and details (not (string-empty-p details)))
                                                   details event)))))
          (error
           (ai-workbench-cli--finish fsm nil callback nil (error-message-string err))))
        (when (processp proc)
          (setf (alist-get proc ai-workbench--request-alist)
                (cons fsm
                      (lambda ()
                        (plist-put info :callback #'ignore)
                        (when (process-live-p proc) (delete-process proc))))))))))

;; ── Registration ──────────────────────────────────────────────────────────────

(defconst ai-workbench-engine-cli-backends
  '(("CC – Claude Code (CLI)" . claude)
    ("Codex (CLI)"            . codex)
    ("OpenCode (CLI)"         . opencode))
  "Alist of ai-workbench-engine backend display name → ai-workbench-cli tool id.")

(defun ai-workbench-engine-cli-register ()
  "Register CLI agents as ai-workbench-engine backends and set CC as the default.
This replaces the built-in default (OpenAI HTTP) so that no API key
is needed and all requests go through CLI exec."
  (dolist (entry ai-workbench-engine-cli-backends)
    (ai-workbench-make-cli (car entry)
      :tool-id (cdr entry)
      :models (list (intern (format "%s-cli" (cdr entry))))))
  ;; Default to CC, then restore the last-selected backend from var/ if any.
  (when-let* ((cc (alist-get "CC – Claude Code (CLI)"
                             ai-workbench--known-backends nil nil #'equal)))
    (setq-default ai-workbench-backend cc)
    (when-let* ((models (ai-workbench-backend-models cc))
                (first  (car models)))
      (setq-default ai-workbench-model first)))
  (ai-workbench-engine-cli-restore-backend))

(defun ai-workbench-engine-cli-backend-p (name)
  "Return non-nil when the ai-workbench-engine backend named NAME is a CLI bridge backend."
  (let ((backend (alist-get name ai-workbench--known-backends nil nil #'equal)))
    (and backend (ai-workbench-cli-p backend))))

;; ── Backend persistence ───────────────────────────────────────────────────────

(defconst ai-workbench-engine-cli--state-file
  (locate-user-emacs-file "var/ai-workbench/engine-backend.eld")
  "File persisting the last selected CLI engine backend tool-id.")

(defun ai-workbench-engine-cli--save-backend (tool-id)
  "Write TOOL-ID to the persistence file."
  (let ((dir (file-name-directory ai-workbench-engine-cli--state-file)))
    (unless (file-exists-p dir) (make-directory dir t)))
  (with-temp-file ai-workbench-engine-cli--state-file
    (prin1 tool-id (current-buffer))))

(defun ai-workbench-engine-cli--load-backend ()
  "Return the persisted tool-id, or nil."
  (when (file-exists-p ai-workbench-engine-cli--state-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents ai-workbench-engine-cli--state-file)
        (read (current-buffer))))))

(defun ai-workbench-engine-cli-activate-backend (tool-id)
  "Set `ai-workbench-backend' to the CLI backend for TOOL-ID and persist.
TOOL-ID is one of: claude, codex, opencode.
Also sets `ai-workbench-default-backend' so the Hub and new sessions
reflect the selection immediately."
  (let ((entry (cl-find tool-id ai-workbench-engine-cli-backends :key #'cdr)))
    (when-let* ((name (car entry))
                (backend (alist-get name ai-workbench--known-backends nil nil #'equal)))
      (setq-default ai-workbench-backend backend)
      (when-let* ((models (ai-workbench-backend-models backend))
                  (first  (car models)))
        (setq-default ai-workbench-model first))
      ;; Sync the session layer default so Hub shows the right ●active marker.
      (when (boundp 'ai-workbench-default-backend)
        (setq ai-workbench-default-backend tool-id))
      (ai-workbench-engine-cli--save-backend tool-id)
      backend)))

(defun ai-workbench-engine-cli-restore-backend ()
  "Restore the last-persisted CLI backend, or keep CC as default."
  (when-let* ((tool-id (ai-workbench-engine-cli--load-backend)))
    (ai-workbench-engine-cli-activate-backend tool-id)))

(provide 'ai-workbench-engine-cli)
;;; ai-workbench-engine-cli.el ends here
