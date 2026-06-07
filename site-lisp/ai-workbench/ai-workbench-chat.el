;;; ai-workbench-chat.el --- ai-workbench-engine frontend for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads the vendored ai-workbench-engine package and wires the CLI agents (CC,
;; Codex, OpenCode) as ai-workbench-engine backends via
;; `ai-workbench-engine-cli'.  HTTP model backends have been removed;
;; the CLI agents are the only backends.
;;
;; ai-workbench-engine is the Emacs-native frontend (buffer, HCI, context
;; management).  Actual AI execution is performed by the CLI agents.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)
(require 'ai-workbench-vendor)

(defvar ai-workbench--known-backends)
(defvar ai-workbench-backend)
(defvar ai-workbench-model)
(defvar ai-workbench-system-prompt)
(declare-function ai-workbench-backend-name    "ai-workbench-request" (backend))
(declare-function ai-workbench-backend-models  "ai-workbench-request" (backend))
(declare-function ai-workbench-engine          "ai-workbench-engine" (name &optional _ initial interactivep))
(declare-function ai-workbench-send            "ai-workbench-engine" (&optional arg))
(declare-function ai-workbench-mode            "ai-workbench-engine" (&optional arg))
(declare-function ai-workbench-abort           "ai-workbench-engine" ())
(declare-function ai-workbench-engine-cli-register "ai-workbench-engine-cli" ())

;; ── Loading ───────────────────────────────────────────────────────────────────

(defun ai-workbench-chat-available-p ()
  "Return non-nil when the vendored ai-workbench-engine directory is present."
  (ai-workbench-vendor-package-present-p 'ai-workbench-engine))

(defun ai-workbench-chat-load ()
  "Load the vendored ai-workbench-engine package and register CLI agent backends."
  (unless (ai-workbench-chat-available-p)
    (error "Vendored package not present: ai-workbench-engine"))
  (ai-workbench-add-vendor-to-load-path 'ai-workbench-engine)
  (require 'ai-workbench-engine)
  (require 'ai-workbench-request)
  (require 'ai-workbench-transient)
  ;; Bridge CLI agents (CC/Codex/OpenCode) in as ai-workbench-engine backends
  ;; and set the default backend to CC so the engine never falls back to HTTP.
  (require 'ai-workbench-engine-cli)
  (ai-workbench-engine-cli-register))

;; ── Backend queries ───────────────────────────────────────────────────────────

(defun ai-workbench-chat-get-backend (name)
  "Return the ai-workbench-engine backend struct for NAME, or nil."
  (require 'ai-workbench-request)
  (alist-get name ai-workbench--known-backends nil nil #'equal))

(defun ai-workbench-chat-backend-names ()
  "Return list of registered ai-workbench-engine backend names (CLI agents only)."
  (require 'ai-workbench-request)
  (mapcar #'car ai-workbench--known-backends))

;; ── Buffer naming ─────────────────────────────────────────────────────────────

(defun ai-workbench-chat--buffer-name (backend-name)
  "Return the chat buffer name for BACKEND-NAME."
  (format "*ai-workbench chat: %s*" backend-name))

;; ── Adapter interface ─────────────────────────────────────────────────────────

(defun ai-workbench-chat-buffer (&optional _project-root)
  "Return an existing chat buffer for the current default backend, or nil."
  (cl-find-if
   (lambda (buf)
     (with-current-buffer buf
       (and (bound-and-true-p ai-workbench-mode)
            (eq ai-workbench-backend (default-value 'ai-workbench-backend)))))
   (buffer-list)))

(defun ai-workbench-chat-open-buffer ()
  "Open a chat buffer for the current active CLI backend."
  (interactive)
  (ai-workbench-chat-load)
  (let ((backend-name (ai-workbench-backend-name (default-value 'ai-workbench-backend))))
    (ai-workbench-engine (ai-workbench-chat--buffer-name backend-name))))

(defalias 'ai-workbench-chat #'ai-workbench-chat-open-buffer)

(defun ai-workbench-chat-session-live-p (&optional _project-root)
  "Return non-nil when a chat buffer exists for the default backend."
  (and (featurep 'ai-workbench-engine)
       (not (null (ai-workbench-chat-buffer)))))

(defun ai-workbench-chat-stop (&optional _project-root)
  "Abort the current chat request."
  (interactive)
  (ai-workbench-chat-load)
  (when (fboundp 'ai-workbench-abort)
    (call-interactively #'ai-workbench-abort)))

(defun ai-workbench-chat-ensure-session (&optional _project-root)
  "Ensure ai-workbench-engine is loaded and CLI backends are registered."
  (ai-workbench-chat-load))

(defun ai-workbench-chat-prime-session (&optional project-root)
  "Inject profile as ai-workbench-engine system prompt for PROJECT-ROOT."
  (ai-workbench-chat-load)
  (let ((root (or project-root default-directory)))
    (unless (ai-workbench-session-profile-injected-p 'chat root)
      (setq-default ai-workbench-system-prompt
                    (ai-workbench-profile-build-prompt root))
      (ai-workbench-session-mark-profile-bootstrap-sent 'chat root)
      (ai-workbench-session-mark-profile-injected 'chat root)
      (ai-workbench-session-set-last-status "chat profile injected" root))))

(defun ai-workbench-chat--send-internal (prompt)
  "Insert PROMPT into the active backend's chat buffer and send."
  (let* ((backend  (default-value 'ai-workbench-backend))
         (buf-name (ai-workbench-chat--buffer-name (ai-workbench-backend-name backend)))
         (buf      (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (bound-and-true-p ai-workbench-mode)
        (text-mode)
        (ai-workbench-mode 1))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert prompt)
      (ai-workbench-send))
    (display-buffer buf)))

(defun ai-workbench-chat-send-prompt (prompt &optional project-root)
  "Send PROMPT to the active CLI chat backend for PROJECT-ROOT."
  (ai-workbench-chat-load)
  (let ((default-directory (or project-root default-directory)))
    (ai-workbench-chat-ensure-session project-root)
    (ai-workbench-chat--send-internal prompt)))

(defun ai-workbench-chat-draft-prompt (prompt &optional _project-root)
  "Insert PROMPT into the chat buffer without sending."
  (ai-workbench-chat-load)
  (let* ((backend  (default-value 'ai-workbench-backend))
         (buf-name (ai-workbench-chat--buffer-name (ai-workbench-backend-name backend)))
         (buf      (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (bound-and-true-p ai-workbench-mode)
        (text-mode)
        (ai-workbench-mode 1))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert prompt))
    (display-buffer buf)
    (message "chat prompt drafted; press C-c RET to send")))

(provide 'ai-workbench-chat)
;;; ai-workbench-chat.el ends here
