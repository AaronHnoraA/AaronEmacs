;;; tools/lsp/completion.el --- Completion never waits on the server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; lsp-mode waits synchronously on the server in two completion paths:
;; running the command an accepted item carries, and fetching the selected
;; candidate's signature while the user moves through the list.  Locally a
;; round trip is milliseconds; on a remote target it stalls every accepted
;; completion and every arrow key.  Contracts A1-A3 in
;; docs/design/lsp-workflow.md ("Editing Never Waits On The Server").
;;
;; A1 follows neo-emacs, which sent one JDT command asynchronously; here every
;; server-side command of an accepted item is, including one that only
;; arrives through an asynchronous resolve.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar company-candidates)
(defvar company-selection)
(declare-function company-call-frontends "company" (command))
(declare-function lsp-request-async "lsp-mode" (method params callback &rest args))
(declare-function lsp-log "lsp-mode" (format &rest args))
(declare-function lsp-completion--get-label-detail "lsp-completion" (item &optional omit-description))
(declare-function lsp-completion--resolve-async "lsp-completion" (item callback &optional cleanup-fn))


;;; A1: commands of accepted completions

(defvar +lsp--completion-command-async nil
  "Non-nil while the commands being run belong to an accepted completion.")

(defun +lsp--send-execute-command-async (command &optional args)
  "Send `workspace/executeCommand' COMMAND with ARGS without waiting.
A failure is logged to `*lsp-log*', not signalled."
  (lsp-request-async
   "workspace/executeCommand"
   (if args (list :command command :arguments args) (list :command command))
   #'ignore
   :mode 'detached
   :error-handler (lambda (error)
                    (lsp-log "Completion command %s failed: %S" command error))))

(defun +lsp-completion--exit-a (orig &rest args)
  "Run ORIG, `lsp-completion--exit-fn' with ARGS, sending its commands async."
  (let ((+lsp--completion-command-async t))
    (apply orig args)))

(defun +lsp-completion--resolve-async-a (orig item callback &optional cleanup-fn)
  "Run ORIG for ITEM; keep an accepted completion's CALLBACK async too.
The exit function runs the resolved item's command from CALLBACK, after
`+lsp-completion--exit-a' has returned.  CLEANUP-FN is passed through."
  (funcall orig item
           (if +lsp--completion-command-async
               (lambda (&rest values)
                 (let ((+lsp--completion-command-async t))
                   (apply callback values)))
             callback)
           cleanup-fn))

(defun +lsp-send-execute-command-a (orig command &optional args)
  "Send COMMAND with ARGS async for an accepted completion, else call ORIG."
  (if +lsp--completion-command-async
      (+lsp--send-execute-command-async command args)
    (funcall orig command args)))


;;; A2: browsing candidates

(defvar +lsp--docsig-details (make-hash-table :test 'eq :weakness 'key)
  "Resolved signatures by completion item; `:none' when there is none.")

(defvar-local +lsp--docsig-pending nil
  "The completion item whose signature is being resolved, or nil.")

(defun +lsp--nonblank (string)
  "Return STRING trimmed, or nil when it is empty."
  (when string
    (let ((trimmed (string-trim string)))
      (unless (string-empty-p trimmed) (concat " " trimmed)))))

(defun +lsp-completion--refresh-docsig (buffer item)
  "Redisplay Company's metadata in BUFFER if ITEM is still selected."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq +lsp--docsig-pending item)
        (setq +lsp--docsig-pending nil))
      (when-let* (((bound-and-true-p company-candidates))
                  ((integerp company-selection))
                  (selected (nth company-selection company-candidates))
                  ((eq (get-text-property 0 'lsp-completion-item selected) item)))
        (company-call-frontends 'post-command)))))

(defun +lsp-completion--docsig-a (_orig candidate)
  "Return CANDIDATE's signature without waiting on the server.
The unresolved item's detail is used when present; otherwise the item is
resolved asynchronously and Company redisplays when it arrives."
  (when-let* ((item (get-text-property 0 'lsp-completion-item candidate)))
    (or (+lsp--nonblank (lsp-completion--get-label-detail item t))
        (let ((known (gethash item +lsp--docsig-details)))
          (cond
           ((eq known :none) nil)
           (known)
           ((eq +lsp--docsig-pending item) nil)
           (t
            (let ((buffer (current-buffer)))
              (setq +lsp--docsig-pending item)
              (lsp-completion--resolve-async
               candidate
               (lambda (resolved)
                 (puthash item
                          (or (+lsp--nonblank
                               (lsp-completion--get-label-detail resolved t))
                              :none)
                          +lsp--docsig-details)
                 (+lsp-completion--refresh-docsig buffer item))
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (eq +lsp--docsig-pending item)
                       (setq +lsp--docsig-pending nil)))))))
            nil))))))


(with-eval-after-load 'lsp-completion
  (advice-add 'lsp-completion--exit-fn :around #'+lsp-completion--exit-a)
  (advice-add 'lsp-completion--resolve-async :around #'+lsp-completion--resolve-async-a)
  (advice-add 'lsp-completion--company-docsig :around #'+lsp-completion--docsig-a))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp-send-execute-command :around #'+lsp-send-execute-command-a))

(provide 'tools/lsp/completion)

;;; tools/lsp/completion.el ends here
