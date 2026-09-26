;;; remote-latency-profile.el --- Opt-in ELP timings for live remote LSP -*- lexical-binding: t; -*-

;; Load after init.el and before lsp-remote-live-smoke.el.  This file only
;; instruments the current batch process; it changes no runtime defaults.

(require 'elp)
(require 'seq)

(defvar remote-latency-profile--started (float-time))
(defvar remote-latency-profile--rpc-methods (make-hash-table :test #'equal))
(defvar remote-latency-profile--rpc-requests (make-hash-table :test #'equal))
(defvar remote-latency-profile--locate-callers (make-hash-table :test #'equal))
(defvar remote-latency-profile--stat-callers (make-hash-table :test #'equal))
(defvar remote-latency-profile--process-callers (make-hash-table :test #'equal))
(defvar remote-latency-profile--executables (make-hash-table :test #'equal))

(defun remote-latency-profile--count (table key)
  "Count KEY in TABLE without storing paths or request payloads."
  (puthash key (1+ (gethash key table 0)) table))

(defun remote-latency-profile--rpc-call-a (function _vec method &rest args)
  "Count RPC METHOD and call FUNCTION with ARGS."
  (remote-latency-profile--count remote-latency-profile--rpc-methods method)
  (when (member method '("file.stat" "highlevel.locate_dominating_file_multi"))
    (remote-latency-profile--count
     remote-latency-profile--rpc-requests
     (cons method (car args))))
  (when (equal method "highlevel.locate_dominating_file_multi")
    (remote-latency-profile--count
     remote-latency-profile--locate-callers
     (seq-take
      (seq-filter #'symbolp (mapcar #'cadr (backtrace-frames))) 60)))
  (when (equal method "file.stat")
    (remote-latency-profile--count
     remote-latency-profile--stat-callers
     (seq-take
      (seq-filter #'symbolp (mapcar #'cadr (backtrace-frames))) 75)))
  (when (equal method "process.run")
    (remote-latency-profile--count
     remote-latency-profile--process-callers
     (seq-take
      (seq-filter #'symbolp (mapcar #'cadr (backtrace-frames))) 75)))
  (apply function _vec method args))

(defun remote-latency-profile--executable-a (function program &optional context)
  "Count PROGRAM lookups and call FUNCTION with CONTEXT."
  (remote-latency-profile--count remote-latency-profile--executables program)
  (funcall function program context))

(defun remote-latency-profile--client-require-a (function feature)
  "Time client-side FEATURE loading without changing its result."
  (let ((started (float-time)))
    (prog1 (funcall function feature)
      (let ((elapsed (- (float-time) started)))
        (when (> elapsed 0.002)
          (message "[remote-latency] +%.3fs client require %S %.3fs"
                   (- (float-time) remote-latency-profile--started)
                   feature elapsed))))))

(defun remote-latency-profile--process-write-a
    (function vec pid data &optional owner)
  "Timestamp a target-process write without exposing DATA."
  (message "[remote-latency] +%.3fs process write %d bytes"
           (- (float-time) remote-latency-profile--started)
           (length data))
  (funcall function vec pid data owner))

(defun remote-latency-profile--async-read-a
    (function process response)
  "Timestamp an async target-process read without exposing RESPONSE."
  (let* ((result (plist-get response :result))
         (stdout (alist-get 'stdout result))
         (stderr (alist-get 'stderr result)))
    (when (or stdout stderr)
      (message "[remote-latency] +%.3fs process read stdout=%d stderr=%d"
               (- (float-time) remote-latency-profile--started)
               (length stdout) (length stderr))))
  (funcall function process response))

(defun remote-latency-profile--stamp-advice (name)
  "Return an advice that timestamps NAME."
  (lambda (function &rest arguments)
    (message "[remote-latency] +%.3fs enter %S"
             (- (float-time) remote-latency-profile--started) name)
    (prog1 (apply function arguments)
      (message "[remote-latency] +%.3fs leave %S"
               (- (float-time) remote-latency-profile--started) name))))

(dolist (function
         '(my/language-server-runtime-prepare
           my/language-server-runtime--finish
           my/lsp-mode-ensure
           my/lsp-mode--workspace-initialized-h
           my/lsp-managed-mode-setup
           my/language-server-performance-sync-h
           my/lsp-tab-line-sync-h
           my/direnv-update-environment-maybe
           my/lsp-mode--direnv-ready
           my/lsp-mode-start-now
           my/language-server-apply-process-environment
           my/language-server-project-environment
           my/language-server-process-environment
           my/project-local-root
           my/project-local-entry
           my/project-local-env
           my/language-server-toolchain-apply-environment
           my/language-server-current-toolchain-profile
           my/language-server-toolchain--canonical-root
           my/python-toolchain-discover
           my/python-toolchain--project-venvs
           my/python-toolchain--conda-profiles
           my/python-toolchain--sage-profile
           my/python-toolchain--path-profiles
           my/python-toolchain--command-json
           my/language-server-apply-lsp-local-settings
           my/language-server-runtime-register-lsp-configuration
           my/language-server-contact-available-p
           remote-environment-ensure
           remote-environment-resolve
           remote-path--probe-sync
           remote-make-process))
  (when (fboundp function)
    (advice-add function :around
                (remote-latency-profile--stamp-advice function))))

(with-eval-after-load 'lsp-mode
  (dolist (function
           '(lsp-deferred lsp--init-if-visible lsp lsp-mode
             lsp--require-packages lsp--filter-clients
             lsp--try-project-root-workspaces lsp--calculate-root
             lsp--ensure-lsp-servers
             lsp--start-workspace
             lsp--open-in-workspace
             lsp--text-document-did-open
             lsp-managed-mode
             lsp-configure-buffer
             lsp--auto-configure
             lsp-enable-imenu
             lsp-inlay-hints-mode
             lsp-lens--enable
             lsp-semantic-tokens--enable
             lsp-headerline-breadcrumb-mode
             lsp-modeline-code-actions-mode
             lsp-modeline-diagnostics-mode
             lsp-modeline-workspace-status-mode
             my/language-server--push-workspace-configuration-h
             lsp-inline-completion-mode
             dap-auto-configure-mode
             lsp-completion--enable
             lsp-diagnostics--enable
             lsp-diagnostics--request-pull-diagnostics
             lsp-notify))
    (when (fboundp function)
      (advice-add function :around
                  (remote-latency-profile--stamp-advice function))))
  (dolist (function lsp-mode-hook)
    (when (and (symbolp function) (fboundp function))
      (advice-add function :around
                  (remote-latency-profile--stamp-advice function)))))

(with-eval-after-load 'lsp-semantic-tokens
  (when (fboundp 'lsp-semantic-tokens--enable)
    (advice-add 'lsp-semantic-tokens--enable :around
                (remote-latency-profile--stamp-advice
                 'lsp-semantic-tokens--enable))))

(dolist (function
         '(remote-fs--call-routed-1
           remote-exec-output
           remote-executable-find
           remote-make-process
           my/language-server--connect-workspace
           tramp-rpc--call
           tramp-rpc--call-with-timeout
           tramp-rpc--start-async-read
           tramp-rpc--handle-async-read-response
           tramp-rpc-handle-make-process))
  (when (fboundp function)
    (elp-instrument-function function)))

(when (fboundp 'remote-executable-find)
  (advice-add 'remote-executable-find :around
              #'remote-latency-profile--executable-a))
(when (fboundp 'my/language-server--require-on-client)
  (advice-add 'my/language-server--require-on-client :around
              #'remote-latency-profile--client-require-a))
(when (fboundp 'my/language-server-runtime-current-profile)
  (advice-add
   'my/language-server-runtime-current-profile :filter-return
   (lambda (profile)
     (message "[remote-latency] runtime profile %S"
              (and profile (plist-get profile :id)))
     profile)))
(with-eval-after-load 'tramp-rpc
  (when (fboundp 'tramp-rpc--call)
    (advice-add 'tramp-rpc--call :around
                #'remote-latency-profile--rpc-call-a)))
(with-eval-after-load 'tramp-rpc-process
  (when (fboundp 'tramp-rpc--write-remote-process)
    (advice-add 'tramp-rpc--write-remote-process :around
                #'remote-latency-profile--process-write-a))
  (when (fboundp 'tramp-rpc--handle-async-read-response)
    (advice-add 'tramp-rpc--handle-async-read-response :around
                #'remote-latency-profile--async-read-a)))

(add-hook
 'kill-emacs-hook
 (lambda ()
   (elp-results)
   (princ "\nRPC method counts:\n")
   (maphash
    (lambda (method count)
      (princ (format "%s %d\n" method count)))
    remote-latency-profile--rpc-methods)
   (princ "\nRepeated metadata RPC requests (method, total, distinct, largest repeat):\n")
   (dolist (method '("file.stat" "highlevel.locate_dominating_file_multi"))
     (let ((total 0) (distinct 0) (largest 0))
       (maphash
        (lambda (request count)
          (when (equal (car request) method)
            (setq total (+ total count)
                  distinct (1+ distinct)
                  largest (max largest count))))
        remote-latency-profile--rpc-requests)
       (princ (format "%s %d %d %d\n" method total distinct largest))))
   (princ "\nLocate call stacks (count and outer functions):\n")
   (maphash
    (lambda (stack count)
      (princ (format "%d %S\n" count stack)))
    remote-latency-profile--locate-callers)
   (princ "\nTop file.stat call stacks:\n")
   (let (entries)
     (maphash
      (lambda (stack count)
        (push (cons count stack) entries))
      remote-latency-profile--stat-callers)
     (dolist (entry (seq-take (sort entries (lambda (a b) (> (car a) (car b)))) 8))
       (princ (format "%d %S\n" (car entry) (cdr entry)))))
   (princ "\nTop process.run call stacks:\n")
   (let (entries)
     (maphash
      (lambda (stack count)
        (push (cons count stack) entries))
      remote-latency-profile--process-callers)
     (dolist (entry (seq-take (sort entries (lambda (a b) (> (car a) (car b)))) 12))
       (princ (format "%d %S\n" (car entry) (cdr entry)))))
   (princ "\nExecutable lookup counts:\n")
   (maphash
    (lambda (program count)
      (princ (format "%s %d\n" program count)))
    remote-latency-profile--executables)
   (when (get-buffer "*ELP Profiling Results*")
     (princ "\nRemote LSP profile:\n")
     (princ (with-current-buffer "*ELP Profiling Results*"
              (buffer-string))))))

;;; remote-latency-profile.el ends here
