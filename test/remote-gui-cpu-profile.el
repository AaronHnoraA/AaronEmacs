;;; remote-gui-cpu-profile.el --- Opt-in GUI typing CPU sample report -*- lexical-binding: t; -*-

;; Load after lsp-remote-live-smoke.el.  Set REMOTE_GUI_CPU_PROFILE_OUTPUT
;; to a client-local output path and use REMOTE_LSP_E2E_TYPING_ROUNDS=400.

(require 'cl-lib)
(require 'profiler)
(require 'seq)

(defun my/remote-gui-cpu-profile--name (item)
  "Return a compact stable name for profiler stack ITEM."
  (cond
   ((symbolp item) (symbol-name item))
   ((byte-code-function-p item) "<byte-code>")
   ((subrp item) (format "<subr %s>" (subr-name item)))
   (t (truncate-string-to-width (format "%s" item) 100 nil nil "..."))))

(defun my/remote-gui-cpu-profile--around (function rounds)
  "Profile FUNCTION for ROUNDS and write a flat inclusive CPU sample report."
  (profiler-reset)
  (let (result)
    (unwind-protect
        (progn
          (profiler-start 'cpu)
          (setq result (funcall function rounds)))
      (profiler-stop))
    (when-let* ((output (getenv "REMOTE_GUI_CPU_PROFILE_OUTPUT")))
      (let ((totals (make-hash-table :test #'equal))
            (samples 0))
        (maphash
         (lambda (stack weight)
           (cl-incf samples weight)
           (dolist (function-name
                    (delete-dups
                     (mapcar #'my/remote-gui-cpu-profile--name
                             (append stack nil))))
             (puthash function-name
                      (+ weight (gethash function-name totals 0)) totals)))
         profiler-cpu-log)
        (let (rows)
          (maphash (lambda (name weight)
                     (push (cons name weight) rows)) totals)
          (setq rows (sort rows (lambda (left right)
                                  (> (cdr left) (cdr right)))))
          (with-temp-file output
            (insert (format "samples=%d rounds=%d window-system=%S frame=%S\n"
                            samples rounds window-system
                            (cons (frame-width) (frame-height))))
            (dolist (row (seq-take rows 45))
              (insert (format "%8d  %s\n" (cdr row) (car row))))))))
    result))

(advice-add 'my/lsp-remote-live-smoke--typing-probe :around
            #'my/remote-gui-cpu-profile--around)

(provide 'remote-gui-cpu-profile)
;;; remote-gui-cpu-profile.el ends here
