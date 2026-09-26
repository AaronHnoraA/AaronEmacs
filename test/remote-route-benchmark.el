;;; remote-route-benchmark.el --- Opt-in warm SSH route benchmark -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run through `REMOTE_BENCHMARK_TARGET=aaron-pc make remote-route-benchmark'.
;; The benchmark only reads /tmp on the selected target.  It compares the
;; physical backend name with the corresponding stable /fs: identity in the
;; same Emacs process; neither figure includes a cold SSH connection.

;;; Code:

(require 'remote-framework)
(require 'remote-config)
(require 'subr-x)

(defun remote-route-benchmark--seconds (operation path count)
  "Return seconds for COUNT warm OPERATION calls on PATH."
  (funcall operation path)
  (garbage-collect)
  ;; The first successful backend request can leave an SSH ControlMaster in
  ;; its lazy state.  Prime its one-time liveness check after GC so this
  ;; measurement describes an already warm route rather than that check.
  (funcall operation path)
  (let ((start (float-time)))
    (dotimes (_ count)
      (funcall operation path))
    (- (float-time) start)))

(defun remote-route-benchmark-run ()
  "Compare physical and logical hot file-query costs on one selected target."
  (let* ((target-id (getenv "REMOTE_BENCHMARK_TARGET"))
         (count (string-to-number
                 (or (getenv "REMOTE_BENCHMARK_COUNT") "30"))))
    (unless (and target-id (not (string-empty-p target-id)))
      (error "Set REMOTE_BENCHMARK_TARGET explicitly"))
    (unless (> count 0)
      (error "REMOTE_BENCHMARK_COUNT must be positive"))
    (remote-config-load)
    (remote-fs-install)
    (unless (remote-get-target target-id)
      (error "Unknown target: %s" target-id))
    (let* ((logical (remote-make-file-name target-id "/tmp/"))
           (context (remote-context logical)))
      (princ (format "target=%s count=%d path=%s\n"
                     target-id count logical))
      (dolist (operation
               '(file-exists-p file-attributes directory-files))
        (let* ((spec (gethash operation remote-file-operations))
               (capability (remote-file-operation-spec-capability spec))
               (route (remote-resolve "emacs-file" capability context))
               (physical (remote-project-file-name logical route))
               (direct (remote-route-benchmark--seconds
                        operation physical count))
               (routed (remote-route-benchmark--seconds
                        operation logical count)))
          (princ
           (format "%s backend=%s direct=%.1f us/op logical=%.1f us/op ratio=%.2fx\n"
                   operation (remote-route-link-plugin-id route)
                   (* 1e6 (/ direct count)) (* 1e6 (/ routed count))
                   (/ routed direct))))))))

(remote-route-benchmark-run)

(provide 'remote-route-benchmark)
;;; remote-route-benchmark.el ends here
