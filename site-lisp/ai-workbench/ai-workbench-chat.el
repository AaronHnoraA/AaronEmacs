;;; ai-workbench-chat.el --- HTTP chat engine for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Integrates gptel as the Emacs-native HTTP chat layer for ai-workbench.
;; gptel is the integration mechanism (HTTP transport + Emacs buffer glue),
;; not a user-visible backend name.  This module exposes an `ai-workbench-chat'
;; engine that lets users pick an HTTP model (ChatGPT, Claude-API, etc.)
;; through the unified ai-workbench picker.
;;
;; HTTP model backends are configured via etc/ai-workbench/backends.json.
;; Adding a new HTTP endpoint requires only a JSON edit, no Elisp change.
;;
;; The gptel library itself (vendored under site-lisp/ai-workbench/vendor/gptel)
;; is an internal dependency — its symbols are intentionally not re-exported.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)
(require 'ai-workbench-vendor)

(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-system-prompt)
(declare-function gptel-backend-name "gptel-request" (backend))
(declare-function gptel-backend-models "gptel-request" (backend))
(declare-function gptel "gptel" (name &optional _ initial interactivep))
(declare-function gptel-send "gptel" (&optional arg))
(declare-function gptel-mode "gptel" (&optional arg))
(declare-function gptel-abort "gptel" ())

;; ── Config reading ────────────────────────────────────────────────────────────

(defcustom ai-workbench-chat-config-file
  (expand-file-name "etc/ai-workbench/backends.json" user-emacs-directory)
  "JSON file defining HTTP chat backends for ai-workbench."
  :type 'file
  :group 'ai-workbench)

(defcustom ai-workbench-chat-default-backend "ChatGPT"
  "Name of the default HTTP chat backend when none is explicitly selected."
  :type 'string
  :group 'ai-workbench)

(defun ai-workbench-chat--read-config ()
  "Read and return the chat backend config alist from JSON.
Returns nil when the file is missing or malformed."
  (when (file-exists-p ai-workbench-chat-config-file)
    (with-temp-buffer
      (insert-file-contents ai-workbench-chat-config-file)
      (condition-case nil
          (let ((json-array-type 'list))
            (json-read))
        (json-error
         (display-warning
          '(ai-workbench chat)
          (format "Malformed JSON in %s" ai-workbench-chat-config-file))
         nil)))))

;; ── Backend registration ──────────────────────────────────────────────────────

(defconst ai-workbench-chat--type-map
  '((openai             . gptel-make-openai)
    (anthropic          . gptel-make-anthropic)
    (gemini             . gptel-make-gemini)
    (ollama             . gptel-make-ollama)
    (azure              . gptel-make-azure)
    (deepseek           . gptel-make-deepseek)
    (perplexity         . gptel-make-perplexity)
    (privategpt         . gptel-make-privategpt)
    (xai                . gptel-make-xai)
    (kagi               . gptel-make-kagi)
    (openai-responses   . gptel-make-openai-responses)
    (gh-copilot         . gptel-make-gh-copilot)
    (bedrock            . gptel-make-bedrock))
  "Map from JSON \\='type\\=' strings to gptel-make-* constructor symbols.
Contains HTTP API types only.  CLI tools (Codex, CC, OpenCode) are
managed by `ai-workbench-cli.el' and do not appear here.")

(defun ai-workbench-chat--load-backend (entry)
  "Register one HTTP chat backend from JSON config ENTRY (alist)."
  (let* ((name     (alist-get 'name entry))
         (type     (alist-get 'type entry))
         (maker    (alist-get (intern type) ai-workbench-chat--type-map))
         (host     (alist-get 'host entry))
         (key      (alist-get 'key entry))
         (protocol (or (alist-get 'protocol entry) "https"))
         (endpoint (alist-get 'endpoint entry))
         (stream   (alist-get 'stream entry))
         (models   (mapcar #'intern (alist-get 'models entry)))
         (req-params (alist-get 'request-params entry)))
    (if maker
        (apply maker name
               (append
                (when host `(:host ,host))
                (when key `(:key ,key))
                `(:protocol ,protocol)
                (when endpoint `(:endpoint ,endpoint))
                `(:stream ,(if stream :json t))
                `(:models ,models)
                (when req-params `(:request-params ,req-params))))
      (display-warning
       '(ai-workbench chat)
       (format "Unknown backend type \"%s\" for \"%s\"" type name)))))

(defun ai-workbench-chat-register-backends ()
  "Read backends.json and register all HTTP chat backends with gptel."
  (interactive)
  (when-let* ((config (ai-workbench-chat--read-config))
              (backends (alist-get 'gptel-backends config)))
    (dolist (entry backends)
      (ai-workbench-chat--load-backend entry))
    (let ((default-name (or (alist-get 'default-backend config)
                            ai-workbench-chat-default-backend)))
      (when-let* ((backend (ai-workbench-chat-get-backend default-name)))
        (setq-default gptel-backend backend)
        (when-let* ((models (gptel-backend-models backend))
                    (first-model (car models)))
          (setq-default gptel-model first-model))))))

(defun ai-workbench-chat-get-backend (name)
  "Return the gptel backend struct for NAME, or nil."
  (require 'gptel-request)
  (alist-get name gptel--known-backends nil nil #'equal))

(defun ai-workbench-chat-backend-names ()
  "Return list of registered HTTP chat backend names."
  (require 'gptel-request)
  (mapcar #'car gptel--known-backends))

;; ── Loading ───────────────────────────────────────────────────────────────────

(defun ai-workbench-chat-available-p ()
  "Return non-nil when the vendored gptel directory is present."
  (ai-workbench-vendor-package-present-p 'gptel))

(defun ai-workbench-chat-load ()
  "Load the vendored gptel package and register HTTP backends."
  (unless (ai-workbench-chat-available-p)
    (error "Vendored package not present: gptel"))
  (ai-workbench-add-vendor-to-load-path 'gptel)
  (require 'gptel)
  (require 'gptel-request)
  (require 'gptel-transient)
  (ai-workbench-chat-register-backends))

;; ── Buffer naming ─────────────────────────────────────────────────────────────

(defun ai-workbench-chat--buffer-name (backend-name)
  "Return the chat buffer name for BACKEND-NAME."
  (format "*ai-workbench chat: %s*" backend-name))

;; ── Adapter interface ─────────────────────────────────────────────────────────

(defun ai-workbench-chat-buffer (&optional _project-root)
  "Return an existing chat buffer for the current default gptel backend, or nil."
  (cl-find-if
   (lambda (buf)
     (with-current-buffer buf
       (and (bound-and-true-p gptel-mode)
            (eq gptel-backend (default-value 'gptel-backend)))))
   (buffer-list)))

(defun ai-workbench-chat-open-buffer ()
  "Open a chat buffer for the current default HTTP backend."
  (interactive)
  (ai-workbench-chat-load)
  (let ((backend-name (gptel-backend-name (default-value 'gptel-backend))))
    (gptel (ai-workbench-chat--buffer-name backend-name))))

(defalias 'ai-workbench-chat #'ai-workbench-chat-open-buffer)

(defun ai-workbench-chat-session-live-p (&optional _project-root)
  "Return non-nil when a chat buffer exists for the default HTTP backend."
  (and (featurep 'gptel)
       (not (null (ai-workbench-chat-buffer)))))

(defun ai-workbench-chat-stop (&optional _project-root)
  "Abort the current HTTP chat request."
  (interactive)
  (ai-workbench-chat-load)
  (when (fboundp 'gptel-abort)
    (call-interactively #'gptel-abort)))

(defun ai-workbench-chat-ensure-session (&optional _project-root)
  "Ensure gptel is loaded and HTTP backends are registered."
  (ai-workbench-chat-load))

(defun ai-workbench-chat-prime-session (&optional project-root)
  "Activate the selected chat backend and inject profile as system prompt."
  (ai-workbench-chat-load)
  (let ((root (or project-root default-directory)))
    ;; Activate the backend the user selected via the picker
    (when-let* ((name (ai-workbench-session-chat-backend root))
                (backend (ai-workbench-chat-get-backend name)))
      (setq-default gptel-backend backend)
      (when-let* ((models (gptel-backend-models backend))
                  (first-model (car models)))
        (setq-default gptel-model first-model)))
    ;; Inject profile as gptel system prompt (once per session)
    (unless (ai-workbench-session-profile-injected-p 'chat root)
      (setq-default gptel-system-prompt
                    (ai-workbench-profile-build-prompt root))
      (ai-workbench-session-mark-profile-bootstrap-sent 'chat root)
      (ai-workbench-session-mark-profile-injected 'chat root)
      (ai-workbench-session-set-last-status "chat profile injected" root))))

(defun ai-workbench-chat--send-internal (prompt)
  "Insert PROMPT into the current backend's chat buffer and send."
  (let* ((backend (default-value 'gptel-backend))
         (buf-name (ai-workbench-chat--buffer-name (gptel-backend-name backend)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (bound-and-true-p gptel-mode)
        (text-mode)
        (gptel-mode 1))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert prompt)
      (gptel-send))
    (display-buffer buf)))

(defun ai-workbench-chat-send-prompt (prompt &optional project-root)
  "Send PROMPT to the HTTP chat backend for PROJECT-ROOT."
  (ai-workbench-chat-load)
  (let ((default-directory (or project-root default-directory)))
    (ai-workbench-chat-ensure-session project-root)
    (ai-workbench-chat--send-internal prompt)))

(defun ai-workbench-chat-draft-prompt (prompt &optional _project-root)
  "Insert PROMPT into the chat buffer without sending."
  (ai-workbench-chat-load)
  (let* ((backend (default-value 'gptel-backend))
         (buf-name (ai-workbench-chat--buffer-name (gptel-backend-name backend)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (bound-and-true-p gptel-mode)
        (text-mode)
        (gptel-mode 1))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert prompt))
    (display-buffer buf)
    (message "chat prompt drafted; press C-c RET to send")))

;; ── Backward-compat aliases (old ai-workbench-gptel-* names) ─────────────────

(defalias 'ai-workbench-gptel-register-backends #'ai-workbench-chat-register-backends)
(defalias 'ai-workbench-gptel-get-backend        #'ai-workbench-chat-get-backend)
(defalias 'ai-workbench-gptel-backend-names      #'ai-workbench-chat-backend-names)
(defalias 'ai-workbench-gptel-available-p        #'ai-workbench-chat-available-p)
(defalias 'ai-workbench-load-gptel               #'ai-workbench-chat-load)
(defalias 'ai-workbench-gptel-buffer             #'ai-workbench-chat-buffer)
(defalias 'ai-workbench-gptel-open-buffer        #'ai-workbench-chat-open-buffer)
(defalias 'ai-workbench-gptel-session-live-p     #'ai-workbench-chat-session-live-p)
(defalias 'ai-workbench-gptel-stop               #'ai-workbench-chat-stop)
(defalias 'ai-workbench-gptel-ensure-session     #'ai-workbench-chat-ensure-session)
(defalias 'ai-workbench-gptel-prime-session      #'ai-workbench-chat-prime-session)
(defalias 'ai-workbench-gptel-send-prompt        #'ai-workbench-chat-send-prompt)
(defalias 'ai-workbench-gptel-draft-prompt       #'ai-workbench-chat-draft-prompt)

(provide 'ai-workbench-chat)
;; Also satisfy (require 'ai-workbench-adapter-gptel) for any code that
;; still uses the old feature name.
(provide 'ai-workbench-adapter-gptel)
;;; ai-workbench-chat.el ends here
