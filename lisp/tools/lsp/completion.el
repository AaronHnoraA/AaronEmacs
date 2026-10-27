;;; tools/lsp/completion.el --- Completion never waits on the server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; lsp-mode can wait synchronously while running an accepted item's command,
;; fetching a candidate's signature, or resolving a TypeScript function-call
;; snippet.  A remote target can stall completion or candidate browsing on
;; every round trip.  Contracts A1-A4 in
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
(defvar lsp-enable-snippet)
(defvar lsp-inhibit-lsp-hooks)
(defvar lsp-completion--cache)
(defvar lsp/insert-text-format-snippet)
(declare-function company-call-frontends "company" (command))
(declare-function lsp-request-async "lsp-mode" (method params callback &rest args))
(declare-function lsp-log "lsp-mode" (format &rest args))
(declare-function lsp-feature? "lsp-mode" (method))
(declare-function lsp-completion--clear-cache "lsp-completion" (&optional keep-last-result))
(declare-function lsp-completion--find-workspace "lsp-completion" (server-id))
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

(defun +lsp-completion--typescript-snippet-needs-resolve-p (candidate)
  "Whether CANDIDATE needs TypeScript's insert text from an async resolve."
  (when (and (stringp candidate)
             (not (get-text-property 0 'lsp-completion-resolved candidate))
             lsp-enable-snippet
             (lsp-completion--find-workspace '+typescript)
             (lsp-feature? "completionItem/resolve"))
    (when-let* ((item (get-text-property 0 'lsp-completion-item candidate)))
      (and (eql (lsp:completion-item-insert-text-format? item)
                lsp/insert-text-format-snippet)
           (not (lsp:completion-item-insert-text? item))
           (let ((edit (lsp:completion-item-text-edit? item)))
             (or (not edit)
                 ;; TypeScript sends a plain-label InsertReplaceEdit first,
                 ;; then supplies the parameter snippet on resolve.
                 (equal (lsp-get edit :newText)
                        (lsp:completion-item-label item))))))))

(defun +lsp-completion--exit-typescript-async (orig args)
  "Finish accepted TypeScript snippet ARGS with ORIG after an async resolve.
Company has already inserted the candidate label.  Keep that label if the
buffer changes before the server answers; never overwrite subsequent typing."
  (let* ((candidate (car args))
         (markers (get-text-property 0 'lsp-completion-markers candidate))
         (start (car-safe markers))
         (end (cadr markers)))
    (if (not (and (number-or-marker-p start)
                  (number-or-marker-p end)
                  (<= (point-min) start end (point-max))))
        (apply orig args)
      ;; `lsp-completion--clear-cache' detaches its end marker.  The accepted
      ;; candidate needs a separate marker until the resolve has answered.
      (let* ((end-marker (copy-marker end t))
             (buffer (current-buffer))
             (workspace (lsp-completion--find-workspace '+typescript))
             (window (and (eq (window-buffer (selected-window)) buffer)
                          (selected-window)))
             (tick (buffer-chars-modified-tick))
             (cursor (point))
             (inserted (buffer-substring-no-properties start end)))
        (put-text-property 0 (length candidate) 'lsp-completion-markers
                           (list start end-marker) candidate)
        (lsp-completion--clear-cache)
        (setq-local lsp-inhibit-lsp-hooks nil)
        (lsp-completion--resolve-async
         candidate
         (lambda (resolved)
           (unwind-protect
               (when (and resolved (buffer-live-p buffer))
                 (with-current-buffer buffer
                   (when (and (= tick (buffer-chars-modified-tick))
                              (= cursor (point))
                              (or (not window)
                                  (and (eq (selected-window) window)
                                       (eq (window-buffer window) buffer)))
                              (not buffer-read-only)
                              (null lsp-completion--cache)
                              (eq (marker-buffer end-marker) buffer)
                              (<= (point-min) start end-marker (point-max))
                              (equal inserted
                                     (buffer-substring-no-properties
                                      start end-marker))
                              (eq workspace
                                  (lsp-completion--find-workspace '+typescript)))
                     (put-text-property 0 (length candidate)
                                        'lsp-completion-item resolved candidate)
                     (put-text-property 0 (length candidate)
                                        'lsp-completion-resolved t candidate)
                     (apply orig args))))
             (set-marker end-marker nil)))
         (lambda () (set-marker end-marker nil)))))))

(defun +lsp-completion--exit-a (orig &rest args)
  "Run ORIG with ARGS, sending completion commands and TS resolves async."
  (let ((+lsp--completion-command-async t))
    (if (+lsp-completion--typescript-snippet-needs-resolve-p (car args))
        (+lsp-completion--exit-typescript-async orig args)
      (apply orig args))))

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
