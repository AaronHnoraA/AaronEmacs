;;; remote-typing-profile.el --- Opt-in live typing hook profile -*- lexical-binding: t; -*-

;; Load after lsp-remote-live-smoke.el and before its batch entry point.
;; This instruments only the synthetic keypress probe in this Emacs process.

(require 'cl-lib)
(require 'seq)

(defvar my/remote-typing-profile--totals (make-hash-table :test #'equal))
(defvar my/remote-typing-profile--in-redisplay nil)

(defun my/remote-typing-profile--redisplay-a (function &rest arguments)
  "Observe RPC calls made while FUNCTION redraws the current frame."
  (let ((my/remote-typing-profile--in-redisplay t))
    (apply function arguments)))

(defun my/remote-typing-profile--rpc-a (function vector method params
                                                &optional connection)
  "Count METHOD if it runs inside a terminal redisplay."
  (when my/remote-typing-profile--in-redisplay
    (my/remote-typing-profile--record (format "redisplay-rpc/%s" method) 0))
  (funcall function vector method params connection))

(defun my/remote-typing-profile--file-handler-a
    (function operation &rest arguments)
  "Count file OPERATION calls made while GUI/terminal redisplay runs."
  (let ((started (float-time)))
    (unwind-protect
        (apply function operation arguments)
      (when my/remote-typing-profile--in-redisplay
        (my/remote-typing-profile--record
         (format "redisplay-file/%s" operation)
         (- (float-time) started))))))

(defun my/remote-typing-profile--record (label elapsed)
  "Add ELAPSED seconds to LABEL's count and total."
  (let ((entry (gethash label my/remote-typing-profile--totals '(0 . 0.0))))
    (puthash label (cons (1+ (car entry)) (+ elapsed (cdr entry)))
             my/remote-typing-profile--totals)))

(defun my/remote-typing-profile--timed (label)
  "Return an around advice that times LABEL."
  (lambda (function &rest arguments)
    (let ((started (float-time)))
      (unwind-protect
          (apply function arguments)
        (my/remote-typing-profile--record
         label (- (float-time) started))))))

(defun my/remote-typing-profile--hook-functions (hook)
  "Return symbol functions installed globally or locally in HOOK."
  (let ((functions (append (if (listp (symbol-value hook))
                               (symbol-value hook)
                             (list (symbol-value hook)))
                           (if (listp (default-value hook))
                               (default-value hook)
                             (list (default-value hook))))))
    (delete-dups
     (seq-filter (lambda (function)
                   (and (symbolp function)
                        (not (eq function t))
                        (fboundp function)))
                 functions))))

(defun my/remote-typing-profile--around-probe (function rounds)
  "Profile the real hook chain while FUNCTION inserts ROUNDS keys."
  (clrhash my/remote-typing-profile--totals)
  (let (advices result)
    (unwind-protect
        (progn
          (advice-add 'redisplay :around
                      #'my/remote-typing-profile--redisplay-a)
          (push (cons 'redisplay
                      #'my/remote-typing-profile--redisplay-a)
                advices)
          (when (fboundp 'tramp-rpc--call)
            (advice-add 'tramp-rpc--call :around
                        #'my/remote-typing-profile--rpc-a)
            (push (cons 'tramp-rpc--call
                        #'my/remote-typing-profile--rpc-a)
                  advices))
          (dolist (handler '(remote-fs-file-name-handler
                              tramp-file-name-handler))
            (when (fboundp handler)
              (advice-add handler :around
                          #'my/remote-typing-profile--file-handler-a)
              (push (cons handler
                          #'my/remote-typing-profile--file-handler-a)
                    advices)))
          (dolist (hook '(pre-command-hook post-command-hook
                           before-change-functions after-change-functions))
            (dolist (name (my/remote-typing-profile--hook-functions hook))
              (let ((advice (my/remote-typing-profile--timed
                             (format "%s/%s" hook name))))
                (advice-add name :around advice)
                (push (cons name advice) advices))))
          (dolist (name '(self-insert-command run-hooks run-hook-with-args
                           lsp-notify lsp--send-notification lsp--send-no-wait
                           lsp-process-send lsp--workspace-sync-method
                           lsp--text-document-content-change-event
                           lsp--versioned-text-document-identifier
                           lsp-diagnostics--request-pull-diagnostics
                           lsp--remove-overlays lsp--after-change
                           tramp-rpc--write-remote-process))
            (when (fboundp name)
              (let ((advice (my/remote-typing-profile--timed
                             (symbol-name name))))
                (advice-add name :around advice)
                (push (cons name advice) advices))))
          (setq result (funcall function rounds)))
      (dolist (entry advices)
        (advice-remove (car entry) (cdr entry))))
    (let ((redisplay-rpc-count 0) rows)
      (maphash (lambda (label count-and-seconds)
                 (when (string-prefix-p "redisplay-rpc/" label)
                   (cl-incf redisplay-rpc-count (car count-and-seconds)))
                 (push (list label (car count-and-seconds)
                             (* 1000 (cdr count-and-seconds)))
                       rows))
               my/remote-typing-profile--totals)
      (let ((report
             (concat
              "\nTyping hook profile (inclusive totals; instrumentation adds overhead):\n"
              (format "RPC calls during redisplay: %d\n"
                      redisplay-rpc-count)
              (mapconcat
               (lambda (row)
                 (format "%6.3f ms  %3d calls  %s\n"
                         (nth 2 row) (nth 1 row) (car row)))
               (seq-take (sort rows (lambda (left right)
                                      (> (nth 2 left) (nth 2 right))))
                         35)
               ""))))
        (princ report)
        (when-let* ((output (getenv "REMOTE_TYPING_PROFILE_OUTPUT")))
          (let ((default-directory user-emacs-directory))
            (with-temp-file output (insert report))))))
    result))

(advice-add 'my/lsp-remote-live-smoke--typing-probe :around
            #'my/remote-typing-profile--around-probe)

(provide 'remote-typing-profile)
;;; remote-typing-profile.el ends here
