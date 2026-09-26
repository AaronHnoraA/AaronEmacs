;;; init-lsp-prewarm.el --- Remote LSP completion warmup -*- lexical-binding: t; -*-

;;; Commentary:
;; Language servers can spend seconds on the first completion request in
;; an existing remote project.  Prime that server path after initialization
;; without putting the request on Company's synchronous keystroke path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'remote-core)

(declare-function my/language-server--lsp-workspace-id "init-lsp" (workspace))
(declare-function lsp--workspace-buffers "lsp-mode" (workspace))
(declare-function lsp--workspace-cmd-proc "lsp-mode" (workspace))
(declare-function lsp--workspace-status "lsp-mode" (workspace))
(declare-function lsp--text-document-position-params "lsp-mode"
                  (&optional identifier position))
(declare-function lsp-request-async "lsp-mode"
                  (method params callback &rest keys))
(declare-function lsp-workspaces "lsp-mode" ())
(defvar lsp--cur-workspace)
(defvar lsp-managed-mode)
(defvar company-idle-delay)
(defvar company-mode)
(declare-function company--should-begin "company" ())
(declare-function company-idle-begin "company" (buffer window tick point))

(defcustom my/lsp-completion-prewarm t
  "Issue one asynchronous remote completion request after LSP initializes.
The request leaves the source buffer unchanged and primes the server's first
completion path before Company needs a synchronous answer."
  :type 'boolean
  :group 'my/language-server)

(defcustom my/lsp-completion-prewarm-delay 0.1
  "Seconds to defer the first remote completion request after initialization."
  :type 'number
  :group 'my/language-server)

(defcustom my/lsp-completion-ready-idle-delay 0.12
  "Maximum Company idle delay after a remote server's completion is warm.
Cold, failed, and replaced server generations retain the buffer's ordinary
Company delay.  Set to nil to keep that delay even after successful warmup."
  :type '(choice (const :tag "Keep ordinary delay" nil)
                 (number :tag "Seconds"))
  :group 'my/language-server)

(defcustom my/lsp-completion-prewarm-slow-after 2.0
  "Seconds before a pending warmup stops suppressing automatic completion.
The asynchronous request remains in flight and can still restore Company when
its reply arrives."
  :type 'number
  :group 'my/language-server)

(defvar my/lsp-completion--prewarm-state
  (make-hash-table :test #'eq :weakness 'key)
  "One completion warmup state per live lsp-mode workspace.")

(defvar-local my/lsp-completion--normal-idle-delay nil
  "Company's idle delay before installing the remote warmup guard.")

(defvar-local my/lsp-completion--pending-company nil
  "Workspace, buffer tick and point of a Company start deferred by warmup.")

(defun my/lsp-completion--company-idle-delay ()
  "Keep automatic Company completion off a cold remote server path."
  (let* ((workspace
          (and my/lsp-completion-prewarm
               (bound-and-true-p my/lsp-remote-change--eligible)
               (bound-and-true-p lsp-managed-mode)
               (seq-find
                (lambda (item)
                  (eq (ignore-errors (lsp--workspace-status item))
                      'initialized))
                (ignore-errors (lsp-workspaces)))))
         (entry (and workspace
                     (gethash workspace
                              my/lsp-completion--prewarm-state))))
    (if (and entry
             (memq (plist-get entry :state) '(scheduled sent))
             (eq (plist-get entry :process)
                 (ignore-errors (lsp--workspace-cmd-proc workspace))))
        (progn
          (setq my/lsp-completion--pending-company
                (and (fboundp 'company--should-begin)
                     (company--should-begin)
                     (list workspace (buffer-chars-modified-tick) (point))))
          nil)
      (unless (eq (plist-get entry :state) 'slow)
        (setq my/lsp-completion--pending-company nil))
      (let ((normal
             (if (functionp my/lsp-completion--normal-idle-delay)
                 (funcall my/lsp-completion--normal-idle-delay)
               my/lsp-completion--normal-idle-delay)))
        (if (and entry
                 (eq (plist-get entry :state) 'ready)
                 (eq (plist-get entry :process)
                     (ignore-errors (lsp--workspace-cmd-proc workspace)))
                 (numberp normal)
                 (numberp my/lsp-completion-ready-idle-delay)
                 (> my/lsp-completion-ready-idle-delay 0))
            (min normal my/lsp-completion-ready-idle-delay)
          normal)))))

(defun my/lsp-completion--company-resume (workspace)
  "Resume an idle Company start deferred for WORKSPACE if typing has stopped."
  (when (fboundp 'company-idle-begin)
    (dolist (buffer (let ((attached
                           (ignore-errors (lsp--workspace-buffers workspace))))
                      (and (listp attached) attached)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
        (when-let* ((pending my/lsp-completion--pending-company)
                    ((eq (car pending) workspace)))
          (setq my/lsp-completion--pending-company nil)
          (when-let* ((window (selected-window))
                      ((eq (window-buffer window) buffer))
                      ((bound-and-true-p company-mode))
                      ((= (nth 1 pending) (buffer-chars-modified-tick)))
                      ((= (nth 2 pending) (point))))
            (run-at-time
             0 nil
             (lambda (source visible tick position)
               (when (and (buffer-live-p source)
                          (window-live-p visible)
                          (eq visible (selected-window))
                          (eq source (window-buffer visible)))
                 (with-current-buffer source
                   (company-idle-begin source visible tick position))))
             buffer window (nth 1 pending) (nth 2 pending)))))))))

(defun my/lsp-completion--company-install-h ()
  "Guard remote automatic Company completion until server warmup ends."
  (when (and (bound-and-true-p lsp-managed-mode)
             (bound-and-true-p my/lsp-remote-change--eligible)
             (bound-and-true-p company-mode)
             (not (eq company-idle-delay
                      #'my/lsp-completion--company-idle-delay)))
    (setq-local my/lsp-completion--normal-idle-delay company-idle-delay
                company-idle-delay
                #'my/lsp-completion--company-idle-delay)))

(defun my/lsp-completion--company-forget (workspace)
  "Forget automatic Company starts deferred for WORKSPACE."
  (when (fboundp 'lsp--workspace-buffers)
    (dolist (buffer (let ((attached
                           (ignore-errors (lsp--workspace-buffers workspace))))
                      (and (listp attached) attached)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq (car-safe my/lsp-completion--pending-company)
                    workspace)
            (setq my/lsp-completion--pending-company nil)))))))

(defun my/lsp-completion--prewarm-clear (workspace)
  "Cancel and forget WORKSPACE's pending completion warmup."
  (when-let* ((entry (gethash workspace
                             my/lsp-completion--prewarm-state)))
    (when-let* ((timer (plist-get entry :timer)))
      (when (timerp timer) (cancel-timer timer)))
    (remhash workspace my/lsp-completion--prewarm-state))
  (my/lsp-completion--company-forget workspace))

(defun my/lsp-completion--prewarm-finish (workspace process state)
  "Record STATE only if WORKSPACE still owns PROCESS."
  (when-let* ((entry (gethash workspace
                             my/lsp-completion--prewarm-state)))
    (when (eq process (plist-get entry :process))
      (when-let* ((timer (plist-get entry :timer)))
        (when (timerp timer) (cancel-timer timer)))
      (puthash workspace (list :process process :state state)
               my/lsp-completion--prewarm-state)
      (if (eq state 'ready)
          (my/lsp-completion--company-resume workspace)
        (my/lsp-completion--company-forget workspace))
      (remote-log
       'lsp-completion-prewarm
       :server (ignore-errors
                 (my/language-server--lsp-workspace-id workspace))
       :state state
       :elapsed-ms
       (and (numberp (plist-get entry :started))
            (* 1000 (- (float-time) (plist-get entry :started))))))))

(defun my/lsp-completion--prewarm-slow (workspace process)
  "Release WORKSPACE's Company guard while a slow request remains in flight."
  (when-let* ((entry (gethash workspace
                             my/lsp-completion--prewarm-state)))
    (when (and (eq process (plist-get entry :process))
               (eq 'sent (plist-get entry :state)))
      (puthash workspace
               (list :process process :state 'slow
                     :started (plist-get entry :started))
               my/lsp-completion--prewarm-state)
      (remote-log 'lsp-completion-prewarm-slow
                  :server (ignore-errors
                            (my/language-server--lsp-workspace-id workspace))))))

(defun my/lsp-completion--prewarm-buffer (workspace)
  "Find one managed remote source buffer attached to WORKSPACE."
  (let ((buffers (ignore-errors (lsp--workspace-buffers workspace))))
    (when (listp buffers)
      (seq-find
       (lambda (buffer)
         (and (bufferp buffer)
              (buffer-live-p buffer)
              (with-current-buffer buffer
                (and buffer-file-name
                     (bound-and-true-p lsp-managed-mode)
                     (bound-and-true-p
                      my/lsp-remote-change--eligible)))))
       buffers))))

(defun my/lsp-completion--prewarm-dispatch
    (workspace process attempt)
  "Send a nonblocking completion request for WORKSPACE's PROCESS.
ATTEMPT lets buffer attachment finish without starting duplicate requests."
  (when-let* ((entry (gethash workspace
                             my/lsp-completion--prewarm-state)))
    (when (and (eq process (plist-get entry :process))
               (eq (plist-get entry :state) 'scheduled))
      (let ((buffer (my/lsp-completion--prewarm-buffer workspace)))
        (cond
         ((or (not (process-live-p process))
              (not (eq (ignore-errors (lsp--workspace-status workspace))
                       'initialized)))
          (my/lsp-completion--prewarm-clear workspace))
         ((not (and (fboundp 'lsp-request-async)
                    (fboundp 'lsp--text-document-position-params)))
          (my/lsp-completion--prewarm-finish
           workspace process 'unsupported))
         (buffer
          (with-current-buffer buffer
            (let ((lsp--cur-workspace workspace))
              (save-excursion
                (goto-char (point-max))
                (condition-case error-data
                    (let ((params (lsp--text-document-position-params)))
                      (puthash
                       workspace
                       (list :process process :state 'sent
                             :started (float-time)
                             :timer
                             (run-at-time
                              (if (and (numberp
                                        my/lsp-completion-prewarm-slow-after)
                                       (> my/lsp-completion-prewarm-slow-after 0))
                                  my/lsp-completion-prewarm-slow-after
                                2.0)
                              nil #'my/lsp-completion--prewarm-slow
                              workspace process))
                       my/lsp-completion--prewarm-state)
                      (lsp-request-async
                       "textDocument/completion" params
                       (lambda (&rest _response)
                         (my/lsp-completion--prewarm-finish
                          workspace process 'ready))
                       :mode 'alive
                       :error-handler
                       (lambda (&rest _error)
                         (my/lsp-completion--prewarm-finish
                          workspace process 'failed))))
                  (error
                   (my/lsp-completion--prewarm-finish
                    workspace process 'failed)
                   (remote-log
                    'lsp-completion-prewarm-error
                    :server (ignore-errors
                              (my/language-server--lsp-workspace-id
                               workspace))
                    :error (error-message-string error-data))))))))
         ((< attempt 5)
          (puthash
           workspace
           (list :process process :state 'scheduled
                 :timer (run-at-time
                         0.2 nil
                         #'my/lsp-completion--prewarm-dispatch
                         workspace process (1+ attempt)))
           my/lsp-completion--prewarm-state))
         (t
          (my/lsp-completion--prewarm-finish
           workspace process 'no-buffer)))))))

(defun my/lsp-completion--prewarm-schedule (workspace)
  "Schedule one completion warmup for WORKSPACE's current process."
  (when (and my/lsp-completion-prewarm
             workspace
             (my/lsp-completion--prewarm-buffer workspace))
    (when-let* ((process (ignore-errors
                           (lsp--workspace-cmd-proc workspace)))
                ((processp process))
                ((process-live-p process)))
      (unless (eq process
                  (plist-get
                   (gethash workspace
                            my/lsp-completion--prewarm-state)
                   :process))
        (my/lsp-completion--prewarm-clear workspace)
        (puthash
         workspace
         (list :process process :state 'scheduled
               :timer
               (run-at-time
                (if (numberp my/lsp-completion-prewarm-delay)
                    (max 0 my/lsp-completion-prewarm-delay)
                  0.1)
                nil #'my/lsp-completion--prewarm-dispatch
                workspace process 0))
         my/lsp-completion--prewarm-state)))))

(defun my/lsp-completion--prewarm-initialized-h ()
  "Schedule warmup after lsp-mode initializes a remote workspace."
  (when (bound-and-true-p lsp--cur-workspace)
    (my/lsp-completion--prewarm-schedule lsp--cur-workspace)))

(defun my/lsp-completion--prewarm-managed-h ()
  "Schedule warmup when a remote buffer attaches after initialization."
  (when (and my/lsp-completion-prewarm
             (bound-and-true-p lsp-managed-mode)
             (bound-and-true-p my/lsp-remote-change--eligible))
    (dolist (workspace (ignore-errors (lsp-workspaces)))
      (when (eq (ignore-errors (lsp--workspace-status workspace))
                'initialized)
        (my/lsp-completion--prewarm-schedule workspace)))))

(add-hook 'lsp-after-initialize-hook
          #'my/lsp-completion--prewarm-initialized-h)
(add-hook 'lsp-after-uninitialized-functions
          #'my/lsp-completion--prewarm-clear)
(add-hook 'lsp-managed-mode-hook
          #'my/lsp-completion--prewarm-managed-h)
(add-hook 'lsp-managed-mode-hook
          #'my/lsp-completion--company-install-h)
(add-hook 'company-mode-hook
          #'my/lsp-completion--company-install-h)

(provide 'init-lsp-prewarm)
;;; init-lsp-prewarm.el ends here
