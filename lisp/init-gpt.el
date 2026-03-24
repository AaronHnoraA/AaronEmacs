;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;; Configure gptel and Copilot.
;;
;; `var/mygpt.json' supports both the old single-backend format and a richer
;; multi-backend format.  When the file is absent, gptel falls back to its
;; default ChatGPT backend instead of warning on every startup.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'transient)
(require 'init-funcs)

(defgroup my/gptel nil
  "Local gptel helpers and workflow."
  :group 'applications)

(defcustom my/gptel-config-files '("var/mygpt.json")
  "Relative config files tried by `my/gptel-load-config'."
  :type '(repeat string)
  :group 'my/gptel)

(defcustom my/gptel-default-chat-preset 'coding
  "Preset applied by `my/gptel-chat'."
  :type 'symbol
  :group 'my/gptel)

(defcustom my/gptel-log-level 'debug
  "Default gptel logging verbosity."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Info" info)
                 (const :tag "Debug" debug))
  :group 'my/gptel)

(defconst my/gptel--config-template
  (concat
   "{\n"
   "  \"default_backend\": \"OpenAI\",\n"
   "  \"default_model\": \"your-model-name\",\n"
   "  \"backends\": [\n"
   "    {\n"
   "      \"name\": \"OpenAI\",\n"
   "      \"type\": \"openai\",\n"
   "      \"key\": \"env:OPENAI_API_KEY\",\n"
   "      \"model\": \"your-model-name\",\n"
   "      \"models\": [\"your-model-name\"],\n"
   "      \"stream\": true\n"
   "    }\n"
   "  ]\n"
   "}\n")
  "Template inserted by `my/gptel-open-config' for a new config file.")

(defvar my/gptel--loaded-config-file nil
  "Absolute path of the last loaded gptel config file.")

;; Forward-declare external package variables without binding them.
;; Binding them to nil here prevents the packages from installing their own
;; default values and keymaps during load.
(defvar gptel-backend)
(defvar gptel-api-key)
(defvar gptel-default-mode)
(defvar gptel-model)
(defvar gptel-directives)
(defvar gptel--known-backends)
(defvar gptel--known-presets)
(defvar gptel-expert-commands)
(defvar gptel-mode-map)
(defvar gptel-rewrite-default-action)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar gptel-use-context)
(defvar gptel-log-level)
(defvar copilot-completion-map)

(declare-function gptel "gptel")
(declare-function gptel-abort "gptel")
(declare-function gptel-highlight-mode "gptel")
(declare-function gptel-send "gptel")
(declare-function gptel-menu "gptel-transient")
(declare-function gptel-system-prompt "gptel-transient")
(declare-function gptel-tools "gptel-transient")
(declare-function gptel-add "gptel-context")
(declare-function gptel-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(declare-function gptel-rewrite "gptel-rewrite")
(declare-function gptel-org-set-topic "gptel-org")
(declare-function gptel-org-set-properties "gptel-org")
(declare-function gptel-get-backend "gptel-request")
(declare-function gptel-make-openai "gptel-openai")
(declare-function gptel-make-gemini "gptel-gemini")
(declare-function gptel-make-anthropic "gptel-anthropic")
(declare-function gptel-make-ollama "gptel-ollama")
(declare-function gptel-make-gh-copilot "gptel-gh")
(declare-function gptel-make-preset "gptel")
(declare-function gptel--apply-preset "gptel")
(declare-function copilot--request "copilot")
(declare-function copilot-login "copilot")
(declare-function copilot-logout "copilot")
(declare-function copilot-accept-completion "copilot")
(declare-function copilot-accept-completion-to-char "copilot")

(defun my/gptel--expand-config-path (path)
  "Expand gptel config PATH relative to `user-emacs-directory'."
  (expand-file-name path user-emacs-directory))

(defun my/gptel--locate-config-file ()
  "Return the first existing gptel config file."
  (cl-find-if #'file-exists-p
              (mapcar #'my/gptel--expand-config-path my/gptel-config-files)))

(defun my/gptel-open-config ()
  "Open the primary gptel config file, inserting a starter template if needed."
  (interactive)
  (let* ((file (my/gptel--expand-config-path (car my/gptel-config-files)))
         (new-file-p (not (file-exists-p file))))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (and new-file-p (zerop (buffer-size)))
      (insert my/gptel--config-template)
      (goto-char (point-min))
      (search-forward "your-model-name" nil t))))

(defun my/gptel-show-log ()
  "Display the gptel log buffer in the configured debug window."
  (interactive)
  (let ((buffer (get-buffer-create "*gptel-log*")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-max)))))

(defun my/gptel--json-object-p (value)
  "Return non-nil when VALUE looks like a JSON object alist."
  (and (listp value)
       value
       (cl-every #'consp value)))

(defun my/gptel--plistify-key (key)
  "Turn KEY into a plist keyword."
  (intern (format ":%s"
                  (replace-regexp-in-string "_" "-"
                                            (format "%s" key)))))

(defun my/gptel--plistify (value)
  "Convert JSON VALUE objects to plists recursively."
  (cond
   ((my/gptel--json-object-p value)
    (cl-loop for (key . item) in value
             append (list (my/gptel--plistify-key key)
                          (my/gptel--plistify item))))
   ((listp value)
    (mapcar #'my/gptel--plistify value))
   (t value)))

(defun my/gptel--symbolify (value)
  "Convert VALUE to a symbol when appropriate."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t value)))

(defun my/gptel--config-bool (spec key default)
  "Read boolean KEY from backend SPEC, falling back to DEFAULT."
  (if (assq key spec)
      (not (eq (alist-get key spec) :false))
    default))

(defun my/gptel--resolve-secret (value)
  "Resolve VALUE to a gptel key source.

Supported prefixes:
- `env:NAME' reads from an environment variable.
- `file:PATH' reads the first line from PATH."
  (cond
   ((not (stringp value)) value)
   ((string-prefix-p "env:" value)
    (let ((env-name (string-trim (substring value 4))))
      (lambda ()
        (or (getenv env-name)
            (user-error "Environment variable %s is not set" env-name)))))
   ((string-prefix-p "file:" value)
    (let ((path (expand-file-name (string-trim (substring value 5))
                                  user-emacs-directory)))
      (lambda ()
        (unless (file-exists-p path)
          (user-error "GPT key file not found: %s" path))
        (string-trim
         (with-temp-buffer
           (insert-file-contents path)
           (buffer-string))))))
   (t value)))

(defun my/gptel--model-name (entry)
  "Return the model symbol represented by model ENTRY."
  (if (consp entry) (car entry) entry))

(defun my/gptel--normalize-model-entry (entry)
  "Convert JSON model ENTRY into a gptel model spec."
  (cond
   ((or (stringp entry) (symbolp entry))
    (my/gptel--symbolify entry))
   ((and (listp entry) (alist-get 'name entry))
    (let ((name (my/gptel--symbolify (alist-get 'name entry)))
          (description (alist-get 'description entry))
          (capabilities (mapcar #'my/gptel--symbolify
                                (alist-get 'capabilities entry)))
          (mime-types (alist-get 'mime_types entry))
          (context-window (alist-get 'context_window entry))
          (input-cost (alist-get 'input_cost entry))
          (output-cost (alist-get 'output_cost entry))
          (cutoff-date (alist-get 'cutoff_date entry)))
      (append (list name)
              (when description (list :description description))
              (when capabilities (list :capabilities capabilities))
              (when mime-types (list :mime-types mime-types))
              (when context-window (list :context-window context-window))
              (when input-cost (list :input-cost input-cost))
              (when output-cost (list :output-cost output-cost))
              (when cutoff-date (list :cutoff-date cutoff-date)))))
   (t
    (user-error "Unsupported gptel model entry: %S" entry))))

(defun my/gptel--models-from-spec (spec)
  "Build a gptel model list from backend SPEC."
  (let* ((model (my/gptel--symbolify (alist-get 'model spec)))
         (models-raw (alist-get 'models spec))
         (models
          (cond
           ((null models-raw) nil)
           ((listp models-raw)
            (mapcar #'my/gptel--normalize-model-entry models-raw))
           (t
            (list (my/gptel--normalize-model-entry models-raw))))))
    (when (and model
               (not (cl-some (lambda (entry)
                               (eq (my/gptel--model-name entry) model))
                             models)))
      (setq models (append models (list model))))
    models))

(defun my/gptel--backend-name-from-spec (spec)
  "Return the display name of backend SPEC."
  (format "%s"
          (or (alist-get 'name spec)
              (alist-get 'backend_name spec)
              "LLM")))

(defun my/gptel--backend-type-from-spec (spec)
  "Return backend type string extracted from SPEC."
  (downcase (format "%s" (or (alist-get 'type spec) "openai"))))

(defun my/gptel--make-backend (spec)
  "Construct a gptel backend from JSON SPEC."
  (let* ((name (my/gptel--backend-name-from-spec spec))
         (type (my/gptel--backend-type-from-spec spec))
         (host (alist-get 'host spec))
         (protocol (alist-get 'protocol spec))
         (endpoint (alist-get 'endpoint spec))
         (models (my/gptel--models-from-spec spec))
         (stream (my/gptel--config-bool spec 'stream t))
         (key (my/gptel--resolve-secret (alist-get 'key spec)))
         (curl-args (alist-get 'curl_args spec))
         (request-params (my/gptel--plistify (alist-get 'request_params spec))))
    (pcase type
      ((or "openai" "openai-compatible" "compatible")
       (apply #'gptel-make-openai
              name
              (append
               (when host (list :host host))
               (when protocol (list :protocol protocol))
               (when endpoint (list :endpoint endpoint))
               (when key (list :key key))
               (when models (list :models models))
               (list :stream stream)
               (when curl-args (list :curl-args curl-args))
               (when request-params (list :request-params request-params)))))
      ("gemini"
       (apply #'gptel-make-gemini
              name
              (append
               (when host (list :host host))
               (when protocol (list :protocol protocol))
               (when endpoint (list :endpoint endpoint))
               (when key (list :key key))
               (when models (list :models models))
               (list :stream stream)
               (when curl-args (list :curl-args curl-args))
               (when request-params (list :request-params request-params)))))
      ("anthropic"
       (apply #'gptel-make-anthropic
              name
              (append
               (when host (list :host host))
               (when protocol (list :protocol protocol))
               (when endpoint (list :endpoint endpoint))
               (when key (list :key key))
               (when models (list :models models))
               (list :stream stream)
               (when curl-args (list :curl-args curl-args))
               (when request-params (list :request-params request-params)))))
      ("ollama"
       (apply #'gptel-make-ollama
              name
              (append
               (when host (list :host host))
               (when protocol (list :protocol protocol))
               (when endpoint (list :endpoint endpoint))
               (when key (list :key key))
               (when models (list :models models))
               (list :stream stream)
               (when curl-args (list :curl-args curl-args))
               (when request-params (list :request-params request-params)))))
      ((or "gh-copilot" "github-copilot" "copilot")
       (apply #'gptel-make-gh-copilot
              name
              (append
               (when host (list :host host))
               (when protocol (list :protocol protocol))
               (when endpoint (list :endpoint endpoint))
               (when models (list :models models))
               (list :stream stream)
               (when curl-args (list :curl-args curl-args))
               (when request-params (list :request-params request-params)))))
      (_
       (user-error "Unsupported gptel backend type: %s" type)))))

(defun my/gptel--normalize-config (config)
  "Normalize CONFIG so it always has a `backends' list."
  (if (alist-get 'backends config)
      config
    `((backends . (,config))
      (default_backend . ,(my/gptel--backend-name-from-spec config))
      (default_model . ,(alist-get 'model config)))))

(defun my/gptel--backend-spec-by-name (config name)
  "Return backend spec from CONFIG whose name matches NAME."
  (cl-find-if (lambda (spec)
                (string= (my/gptel--backend-name-from-spec spec)
                         (format "%s" name)))
              (alist-get 'backends config)))

(defun my/gptel--set-buffer-local (sym val)
  "Set SYM to VAL buffer-locally."
  (set (make-local-variable sym) val))

(defun my/gptel-apply-preset (preset &optional global)
  "Apply gptel PRESET locally, or globally when GLOBAL is non-nil."
  (interactive
   (list
    (intern
     (completing-read
      "Preset: "
      (mapcar (lambda (entry) (symbol-name (car entry)))
              gptel--known-presets)
      nil t))
    current-prefix-arg))
  (require 'gptel)
  (gptel--apply-preset preset
                       (if global #'set #'my/gptel--set-buffer-local))
  (message "Applied gptel preset %s%s"
           preset
           (if global " globally" " locally")))

(defun my/gptel-chat (&optional skip-preset)
  "Open a gptel chat buffer.

Unless SKIP-PRESET is non-nil, apply `my/gptel-default-chat-preset'
buffer-locally."
  (interactive "P")
  (call-interactively #'gptel)
  (when (and (not skip-preset) my/gptel-default-chat-preset)
    (my/gptel-apply-preset my/gptel-default-chat-preset)))

(defun my/gptel--rewrite-bounds ()
  "Return rewrite bounds near point."
  (cond
   ((use-region-p)
    (cons (region-beginning) (region-end)))
   ((derived-mode-p 'prog-mode)
    (bounds-of-thing-at-point 'defun))
   ((derived-mode-p 'org-mode 'markdown-mode 'text-mode)
    (bounds-of-thing-at-point 'paragraph))
   (t
    (bounds-of-thing-at-point 'sentence))))

(defun my/gptel-rewrite-dwim ()
  "Rewrite the active region, current defun or current paragraph."
  (interactive)
  (unless (use-region-p)
    (pcase-let ((`(,beg . ,end)
                 (or (my/gptel--rewrite-bounds)
                     (cons (point-min) (point-max)))))
      (goto-char beg)
      (push-mark end nil t)
      (activate-mark)))
  (call-interactively #'gptel-rewrite))

(defun my/gptel-clear-context ()
  "Clear all active gptel context."
  (interactive)
  (require 'gptel-context)
  (gptel-context-remove-all t))

(defun my/gptel-org-set-topic-dwim ()
  "Set the current Org subtree as a gptel topic."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org buffers"))
  (call-interactively #'gptel-org-set-topic))

(defun my/gptel-org-set-properties-dwim ()
  "Write current gptel settings into Org heading properties."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org buffers"))
  (call-interactively #'gptel-org-set-properties))

(defun my/gptel-register-default-backends ()
  "Register default backends that are safe to keep available."
  (unless (alist-get "ChatGPT" gptel--known-backends nil nil #'equal)
    (gptel-make-openai "ChatGPT" :key 'gptel-api-key :stream t))
  (when (fboundp 'gptel-make-gh-copilot)
    (unless (alist-get "GitHub Copilot" gptel--known-backends nil nil #'equal)
      (gptel-make-gh-copilot "GitHub Copilot"))))

(defun my/gptel-load-config ()
  "Load and register gptel backends from `var/mygpt.json'."
  (setq my/gptel--loaded-config-file nil)
  (when-let* ((config-file (my/gptel--locate-config-file)))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           (config (my/gptel--normalize-config (json-read-file config-file)))
           (default-backend-name (or (alist-get 'default_backend config)
                                     (my/gptel--backend-name-from-spec
                                      (car (alist-get 'backends config))))))
      (setq my/gptel--loaded-config-file config-file)
      (dolist (spec (alist-get 'backends config))
        (condition-case err
            (my/gptel--make-backend spec)
          (error
           (display-warning
            'init-gpt
            (format "Failed to register gptel backend %s: %s"
                    (my/gptel--backend-name-from-spec spec)
                    (error-message-string err))
            :warning))))
      (when-let* ((backend (ignore-errors
                             (gptel-get-backend
                              (format "%s" default-backend-name)))))
        (setq gptel-backend backend)
        (let* ((backend-spec (my/gptel--backend-spec-by-name
                              config default-backend-name))
               (default-model
                (or (my/gptel--symbolify (alist-get 'default_model config))
                    (and backend-spec
                         (my/gptel--symbolify (alist-get 'model backend-spec))))))
          (when default-model
            (setq gptel-model default-model))))
      t)))

(defun my/gptel-register-directives ()
  "Register task-specific gptel directives."
  (dolist (entry
           '((coding . "You are a senior software engineer. Prefer correct, maintainable solutions. Keep explanations concise and lead with the answer.")
             (review . "You are a rigorous code reviewer. Prioritize bugs, regressions, edge cases and missing tests. Lead with concrete findings.")
             (explain . "You are a senior engineer explaining code to another engineer. Start with the high-level purpose, then key mechanics and tradeoffs. Keep it concise.")
             (commit . "Write a concise git commit message. Prefer Conventional Commits when appropriate. Return only the final commit message.")
             (translate . "Translate accurately and naturally. Preserve structure, formatting and technical meaning. Return only the translation unless asked otherwise.")))
    (setf (alist-get (car entry) gptel-directives) (cdr entry))))

(defun my/gptel-register-presets ()
  "Register local gptel presets."
  (gptel-make-preset
   'coding
   :description "Coding assistant"
   :system 'coding
   :temperature 0.2)
  (gptel-make-preset
   'review
   :description "Code review findings"
   :system 'review
   :temperature 0.1)
  (gptel-make-preset
   'explain
   :description "Explain code and design"
   :system 'explain
   :temperature 0.3)
  (gptel-make-preset
   'commit
   :description "Write commit messages"
   :system 'commit
   :temperature 0.2)
  (gptel-make-preset
   'translate
   :description "Translate text"
   :system 'translate
   :temperature 0.2))

(transient-define-prefix my/gptel-dispatch ()
  "LLM workflow menu."
  [["Session"
    ("c" "chat" my/gptel-chat)
    ("s" "send" gptel-send)
    ("m" "menu" gptel-menu)
    ("p" "prompt" gptel-system-prompt)
    ("@" "preset" my/gptel-apply-preset)]
   ["Edit"
    ("r" "rewrite dwim" my/gptel-rewrite-dwim)
    ("R" "rewrite" gptel-rewrite)
    ("t" "tools" gptel-tools)]
   ["Context"
    ("a" "add context" gptel-add)
    ("f" "add file" gptel-add-file)
    ("x" "clear context" my/gptel-clear-context)]
   ["Org/Config"
    ("o" "open config" my/gptel-open-config)
    ("T" "org topic" my/gptel-org-set-topic-dwim)
    ("P" "org props" my/gptel-org-set-properties-dwim)]])

(defvar my/gptel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'my/gptel-dispatch)
    (define-key map (kbd "c") #'my/gptel-chat)
    (define-key map (kbd "d") #'my/gptel-show-log)
    (define-key map (kbd "s") #'gptel-send)
    (define-key map (kbd "m") #'gptel-menu)
    (define-key map (kbd "p") #'gptel-system-prompt)
    (define-key map (kbd "r") #'my/gptel-rewrite-dwim)
    (define-key map (kbd "a") #'gptel-add)
    (define-key map (kbd "f") #'gptel-add-file)
    (define-key map (kbd "t") #'gptel-tools)
    (define-key map (kbd "x") #'my/gptel-clear-context)
    (define-key map (kbd "o") #'my/gptel-open-config)
    (define-key map (kbd "T") #'my/gptel-org-set-topic-dwim)
    (define-key map (kbd "P") #'my/gptel-org-set-properties-dwim)
    map)
  "Global prefix map for gptel helpers.")

(define-key global-map (kbd "C-c l") my/gptel-prefix-map)
(my/leader-key-label "l" "llm")
(my/evil-global-leader-set "l l" #'my/gptel-dispatch "llm menu")
(my/evil-global-leader-set "l c" #'my/gptel-chat "chat")
(my/evil-global-leader-set "l d" #'my/gptel-show-log "debug log")
(my/evil-global-leader-set "l s" #'gptel-send "send")
(my/evil-global-leader-set "l m" #'gptel-menu "menu")
(my/evil-global-leader-set "l p" #'gptel-system-prompt "prompt")
(my/evil-global-leader-set "l r" #'my/gptel-rewrite-dwim "rewrite")
(my/evil-global-leader-set "l a" #'gptel-add "add context")
(my/evil-global-leader-set "l f" #'gptel-add-file "add file")
(my/evil-global-leader-set "l t" #'gptel-tools "tools")
(my/evil-global-leader-set "l x" #'my/gptel-clear-context "clear context")
(my/evil-global-leader-set "l o" #'my/gptel-open-config "open config")
(my/evil-global-leader-set "l T" #'my/gptel-org-set-topic-dwim "org topic")
(my/evil-global-leader-set "l P" #'my/gptel-org-set-properties-dwim "org props")

(defun my/copilot-check-status ()
  "Report current `copilot.el' authentication status.

Compatibility wrapper for old `lsp-copilot-check-status' workflows."
  (interactive)
  (let* ((response (copilot--request 'checkStatus nil))
         (status (plist-get response :status))
         (user (plist-get response :user)))
    (message "%s"
             (cond
              ((and (stringp user) (not (string-empty-p user)))
               (format "Copilot is signed in as %s%s"
                       user
                       (if (and (stringp status) (not (string-empty-p status)))
                           (format " [%s]" status)
                         "")))
              ((and (stringp status) (not (string-empty-p status)))
               (format "Copilot status: %s" status))
              (t
               (format "Copilot status response: %S" response))))))

(defun my/copilot-check-quota ()
  "Report quota or entitlement information from the Copilot server."
  (interactive)
  (message "Copilot quota: %S" (copilot--request 'checkQuota nil)))

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu gptel-rewrite
                   gptel-system-prompt gptel-tools gptel-add gptel-add-file)
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-abort))
  :init
  (setq gptel-default-mode 'markdown-mode)
  :config
  (setq gptel-expert-commands t
        gptel-log-level my/gptel-log-level
        gptel-rewrite-default-action 'dispatch
        gptel-prompt-prefix-alist
        '((markdown-mode . "### User\n\n")
          (org-mode . "*** User\n")
          (text-mode . "### User\n\n"))
        gptel-response-prefix-alist
        '((markdown-mode . "### Assistant\n\n")
          (org-mode . "*** Assistant\n")
          (text-mode . "### Assistant\n\n")))
  (my/gptel-register-default-backends)
  (unless gptel-backend
    (setq gptel-backend (gptel-get-backend "ChatGPT")))
  (my/gptel-register-directives)
  (my/gptel-register-presets)
  (my/gptel-load-config))

(use-package copilot
  :ensure t
  :hook ((prog-mode . copilot-mode)
         (org-mode . copilot-mode))
  :custom
  (copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory))
  (copilot-idle-delay 0.30)
  (copilot-indent-offset-warning-disable t)
  (copilot-lsp-settings '(:github (:copilot ())))
  :config
  (when (and (fboundp 'my/copilot--suppress-cancelled-errors)
             (advice-member-p #'my/copilot--suppress-cancelled-errors
                             'copilot--log))
    (advice-remove 'copilot--log #'my/copilot--suppress-cancelled-errors))
  (defalias 'lsp-copilot-check-status #'my/copilot-check-status)
  (defalias 'lsp-copilot-login #'copilot-login)
  (defalias 'lsp-copilot-logout #'copilot-logout)
  (define-key copilot-completion-map (kbd "M-]") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-}") #'copilot-accept-completion-to-char))

(provide 'init-gpt)
;;; init-gpt.el ends here
