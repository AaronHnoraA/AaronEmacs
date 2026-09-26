;;; remote-local-visit-benchmark.el --- Balanced native/logical visits -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in latency sample for the local target after one warm-up visit of each
;; spelling.  Run through `make remote-local-visit-benchmark'.  It visits
;; distinct but identical C files in ABBA order inside one Emacs process.

;;; Code:

(require 'cl-lib)
(require 'remote-board)
(require 'remote-config)
(require 'remote-framework)
(require 'seq)

(defun remote-local-visit-benchmark--visit (file)
  "Visit FILE once and return the elapsed seconds."
  (garbage-collect)
  (let* ((start (float-time))
         (buffer (find-file-noselect file))
         (elapsed (- (float-time) start)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))
    elapsed))

(defun remote-local-visit-benchmark--median (values)
  "Return the median of numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (count (length sorted)))
    (if (cl-oddp count)
        (nth (/ count 2) sorted)
      (/ (+ (nth (1- (/ count 2)) sorted)
            (nth (/ count 2) sorted))
         2.0))))

(defun remote-local-visit-benchmark--check-ratio (results)
  "Check optional logical/native latency budget in RESULTS."
  (when-let* ((configured (getenv "REMOTE_VISIT_MAX_RATIO")))
    (let* ((limit (string-to-number configured))
           (native (remote-local-visit-benchmark--median
                    (alist-get 'native results)))
           (logical (remote-local-visit-benchmark--median
                     (alist-get 'logical results)))
           (ratio (/ logical native)))
      (unless (> limit 0)
        (error "REMOTE_VISIT_MAX_RATIO must be positive"))
      (princ (format "logical/native ratio=%.3fx budget=%.3fx\n"
                     ratio limit))
      (when (> ratio limit)
        (error "Local logical visit regression: %.3fx exceeds %.3fx"
               ratio limit)))))

(defun remote-local-visit-benchmark-run ()
  "Compare native and explicit `/fs:local:' source visits in one process."
  (remote-config-load)
  (remote-fs-install)
  (let* ((rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_LOCAL_VISIT_ROUNDS") "4"))))
         (directory (make-temp-file "emacs-local-visit-" t))
         (logical-directory
          (file-name-as-directory (remote-make-file-name "local" directory)))
         (target (remote-get-target "local"))
         (warmup-order
          (if (equal (getenv "REMOTE_LOCAL_VISIT_WARMUP") "logical")
              '(logical native)
            '(native logical)))
         (order (cl-loop repeat rounds append '(native logical logical native)))
         (results '((native . nil) (logical . nil)))
         folder-buffer
         (handler-counts (make-hash-table :test #'eq))
         (handler-callers (make-hash-table :test #'equal))
         (count-advice
          (lambda (function operation &rest arguments)
            (let ((count (1+ (gethash operation handler-counts 0))))
              (puthash operation count handler-counts)
              (when (and (eq operation 'expand-file-name)
                         (zerop (% count 1000)))
                (let ((stack
                       (seq-take
                        (seq-filter
                         #'symbolp
                         (mapcar #'cadr (backtrace-frames)))
                        45)))
                  (puthash stack
                           (1+ (gethash stack handler-callers 0))
                           handler-callers))))
            (apply function operation arguments))))
    (unwind-protect
        (progn
          (dotimes (index (+ 2 (length order)))
            (with-temp-file
                (expand-file-name (format "unit-%02d.c" index) directory)
              (insert "int main(void) { return 0; }\n")))
          (setq folder-buffer (remote-open-folder target directory))
          (when (equal (getenv "REMOTE_LOCAL_VISIT_COUNT") "1")
            (advice-add 'remote-fs--direct-file-name-handler
                        :around count-advice))
          (when (equal (getenv "REMOTE_LOCAL_VISIT_PROFILE") "1")
            (require 'elp)
            (dolist (function
                     '(find-file-noselect after-find-file normal-mode
                       set-auto-mode
                       remote-fs-file-name-handler
                       remote-fs--direct-file-name-handler
                       remote-fs--call-routed-1
                       remote-context remote-connection-ensure
                       remote-environment-ensure remote-path-probe
                       my/copilot-auto-enable-h my/copilot-available-p
                       my/yas-enable-for-source-buffer
                       my/language-server-ensure-deferred
                       my/language-server-runtime-prepare
                       my/direnv-update-environment-maybe))
              (when (fboundp function)
                (elp-instrument-function function))))
          ;; Load major-mode and project hooks before timed visits.  Report
          ;; both first visits separately to expose package-load order effects.
          (cl-loop for spelling in warmup-order
                   for index from 0
                   for root = (if (eq spelling 'native)
                                  directory logical-directory)
                   do (princ
                       (format "warmup %s=%.3f ms\n" spelling
                               (* 1000
                                  (remote-local-visit-benchmark--visit
                                   (expand-file-name
                                    (format "unit-%02d.c" index) root)))))
                   when (and (= index 0)
                             (equal (getenv "REMOTE_LOCAL_VISIT_PROFILE") "1"))
                   do (progn
                        (elp-results)
                        (princ "\nCold first-visit profile:\n")
                        (princ (with-current-buffer
                                   "*ELP Profiling Results*"
                                 (buffer-string))))
                   when (and (= index 0)
                             (equal (getenv "REMOTE_LOCAL_VISIT_COUNT") "1"))
                   do (let (entries)
                        (maphash
                         (lambda (operation count)
                           (push (cons operation count) entries))
                         handler-counts)
                        (princ
                         (format "direct-handler calls=%d operations=%S\n"
                                 (apply #'+ (mapcar #'cdr entries))
                                 (sort entries
                                       (lambda (left right)
                                         (> (cdr left) (cdr right)))))))
                        (let (callers)
                          (maphash
                           (lambda (stack count)
                             (push (cons count stack) callers))
                           handler-callers)
                          (dolist (entry
                                   (seq-take
                                    (sort callers
                                          (lambda (left right)
                                            (> (car left) (car right))))
                                    6))
                            (princ (format "caller samples=%d %S\n"
                                           (car entry) (cdr entry)))))
                        (advice-remove 'remote-fs--direct-file-name-handler
                                       count-advice))
          (cl-loop for spelling in order
                   for index from 2
                   for filename = (format "unit-%02d.c" index)
                   for root = (if (eq spelling 'native)
                                  directory logical-directory)
                   for elapsed = (remote-local-visit-benchmark--visit
                                  (expand-file-name filename root))
                   do (push elapsed (alist-get spelling results)))
          (dolist (spelling '(native logical))
            (let* ((values (alist-get spelling results))
                   (count (length values)))
              (princ
               (format "%s count=%d median=%.3f ms mean=%.3f ms\n"
                       spelling count
                       (* 1000 (remote-local-visit-benchmark--median values))
                       (* 1000 (/ (apply #'+ values) count))))))
          (remote-local-visit-benchmark--check-ratio results))
      (when-let* ((workspace (remote-get-workspace logical-directory)))
        (remote-workspace-close workspace 'benchmark-cleanup))
      (when (buffer-live-p folder-buffer)
        (kill-buffer folder-buffer))
      (delete-directory directory t))))

(remote-local-visit-benchmark-run)

(provide 'remote-local-visit-benchmark)
;;; remote-local-visit-benchmark.el ends here
