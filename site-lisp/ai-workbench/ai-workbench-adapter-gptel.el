;;; ai-workbench-adapter-gptel.el --- gptel adapter for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Integrates gptel as an ai-workbench backend.  Backend configuration
;; is driven by etc/ai-workbench/backends.json so adding a new LLM
;; endpoint does not require Elisp changes.

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

;; ── Config reading ──────────────────────────────────────────────────────────

(defcustom ai-workbench-gptel-config-file
  (expand-file-name "etc/ai-workbench/backends.json" user-emacs-directory)
  "JSON file defining gptel backends for ai-workbench."
  :type 'file
  :group 'ai-workbench)

(defcustom ai-workbench-gptel-default-backend "ChatGPT"
  "Name of the default gptel backend when none is selected."
  :type 'string
  :group 'ai-workbench)

(defun ai-workbench-gptel--read-config ()
  "Read and return the gptel backend config alist.
Returns nil when the file is missing or unreadable."
  (when (file-exists-p ai-workbench-gptel-config-file)
    (with-temp-buffer
      (insert-file-contents ai-workbench-gptel-config-file)
      (condition-case nil
          (json-read)
        (json-error
         (display-warning
          '(ai-workbench gptel)
          (format "Malformed JSON in %s" ai-workbench-gptel-config-file))
         nil)))))

;; ── Backend registration ────────────────────────────────────────────────────

(defconst ai-workbench-gptel--type-map
  '((openai . gptel-make-openai)
    (anthropic . gptel-make-anthropic)
    (gemini . gptel-make-gemini)
    (ollama . gptel-make-ollama)
    (azure . gptel-make-azure)
    (deepseek . gptel-make-deepseek)
    (perplexity . gptel-make-perplexity)
    (privategpt . gptel-make-privategpt)
    (xai . gptel-make-xai)
    (kagi . gptel-make-kagi)
    (openai-responses . gptel-make-openai-responses)
    (gh-copilot . gptel-make-gh-copilot)
    (bedrock . gptel-make-bedrock))
  "Mapping from JSON type strings to gptel-make-* constructor names.")

(defun ai-workbench-gptel--load-backend (entry)
  "Register one gptel backend from JSON config ENTRY (alist)."
  (let* ((name    (alist-get 'name entry))
         (type    (alist-get 'type entry))
         (maker   (alist-get (intern type) ai-workbench-gptel--type-map))
         (host    (alist-get 'host entry))
         (key     (alist-get 'key entry))
         (protocol (or (alist-get 'protocol entry) "https"))
         (endpoint (alist-get 'endpoint entry))
         (stream  (alist-get 'stream entry))
         (models  (mapcar #'intern (alist-get 'models entry)))
         (request-params (alist-get 'request-params entry)))
    (if maker
        (apply maker name
               (append
                (when host `(:host ,host))
                (when key `(:key ,key))
                `(:protocol ,protocol)
                (when endpoint `(:endpoint ,endpoint))
                `(:stream ,(if stream :json t))
                `(:models ,models)
                (when request-params `(:request-params ,request-params))))
      (display-warning
       '(ai-workbench gptel)
       (format "Unknown backend type \"%s\" for \"%s\"" type name)))))

(defun ai-workbench-gptel-register-backends ()
  "Read backends.json and register all gptel backends."
  (interactive)
  (when-let* ((config (ai-workbench-gptel--read-config))
              (backends (alist-get 'gptel-backends config)))
    (dolist (entry backends)
      (ai-workbench-gptel--load-backend entry))
    ;; Set default backend
    (let ((default-name (or (alist-get 'default-backend config)
                            ai-workbench-gptel-default-backend)))
      (when-let* ((backend (ai-workbench-gptel-get-backend default-name)))
        (setq-default gptel-backend backend)
        (when-let* ((models (gptel-backend-models backend))
                    (first-model (car models)))
          (setq-default gptel-model first-model))))))

(defun ai-workbench-gptel-get-backend (name)
  "Return the gptel backend struct for NAME, or nil."
  (require 'gptel-request)
  (alist-get name gptel--known-backends nil nil #'equal))

(defun ai-workbench-gptel-backend-names ()
  "Return list of registered gptel backend names."
  (require 'gptel-request)
  (mapcar #'car gptel--known-backends))

;; ── Loading ─────────────────────────────────────────────────────────────────

(defun ai-workbench-gptel-available-p ()
  "Return non-nil when the vendored gptel directory is present."
  (ai-workbench-vendor-package-present-p 'gptel))

(defun ai-workbench-load-gptel ()
  "Load the vendored gptel package and register backends."
  (unless (ai-workbench-gptel-available-p)
    (error "Vendored package not present: gptel"))
  (ai-workbench-add-vendor-to-load-path 'gptel)
  (require 'gptel)
  (require 'gptel-request)
  (require 'gptel-transient)
  (ai-workbench-gptel-register-backends))

;; ── Adapter interface ───────────────────────────────────────────────────────

(defun ai-workbench-gptel-buffer (&optional _project-root)
  "Return an existing gptel chat buffer, or nil."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (bound-and-true-p gptel-mode)
                       (eq gptel-backend
                           (default-value 'gptel-backend)))))
              (buffer-list)))

(defun ai-workbench-gptel-open-buffer ()
  "Open a gptel chat buffer for the current default backend."
  (interactive)
  (ai-workbench-load-gptel)
  (let ((backend-name (gptel-backend-name (default-value 'gptel-backend))))
    (gptel (format "*gptel %s*" backend-name))))

(defun ai-workbench-gptel-session-live-p (&optional _project-root)
  "Return non-nil when a gptel chat buffer exists for the default backend."
  (and (featurep 'gptel)
       (not (null (ai-workbench-gptel-buffer)))))

(defun ai-workbench-gptel-stop (&optional _project-root)
  "Abort the current gptel request."
  (interactive)
  (ai-workbench-load-gptel)
  (when (fboundp 'gptel-abort)
    (call-interactively #'gptel-abort)))

(defun ai-workbench-gptel-ensure-session (&optional _project-root)
  "Ensure gptel is loaded and backends are registered."
  (ai-workbench-load-gptel))

(defun ai-workbench-gptel--profile-prompt (&optional project-root)
  "Return the profile prompt for PROJECT-ROOT."
  (ai-workbench-profile-build-prompt project-root))

(defun ai-workbench-gptel-prime-session (&optional project-root)
  "Inject the profile as gptel system prompt for PROJECT-ROOT."
  (ai-workbench-load-gptel)
  (let ((root (or project-root default-directory)))
    (unless (ai-workbench-session-profile-injected-p 'gptel root)
      (setq-default gptel-system-prompt
                    (ai-workbench-gptel--profile-prompt root))
      (ai-workbench-session-mark-profile-bootstrap-sent 'gptel root)
      (ai-workbench-session-mark-profile-injected 'gptel root)
      (ai-workbench-session-set-last-status "gptel profile injected" root))))

(defun ai-workbench-gptel-send-prompt (prompt &optional project-root)
  "Send PROMPT to gptel for PROJECT-ROOT."
  (ai-workbench-load-gptel)
  (let ((default-directory (or project-root default-directory)))
    (ai-workbench-gptel-ensure-session project-root)
    (ai-workbench-gptel--send-prompt-internal prompt)))

(defun ai-workbench-gptel--send-prompt-internal (prompt)
  "Insert PROMPT into a gptel buffer and send it."
  (let* ((backend (default-value 'gptel-backend))
         (buf-name (format "*gptel %s*" (gptel-backend-name backend)))
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

(defun ai-workbench-gptel-draft-prompt (prompt &optional _project-root)
  "Insert PROMPT into a gptel buffer without sending."
  (ai-workbench-load-gptel)
  (let* ((backend (default-value 'gptel-backend))
         (buf-name (format "*gptel %s*" (gptel-backend-name backend)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (bound-and-true-p gptel-mode)
        (text-mode)
        (gptel-mode 1))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert prompt))
    (display-buffer buf)
    (message "gptel prompt drafted; press C-c RET to send")))

(provide 'ai-workbench-adapter-gptel)
;;; ai-workbench-adapter-gptel.el ends here
