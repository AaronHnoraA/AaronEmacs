;;; remote-ssh-visit-benchmark.el --- Balanced SSH source visits -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in, same-process comparison of physical TRAMP and logical Remote file
;; visits.  The fixture is a fresh target-side /tmp directory, and LSP auto
;; start is disabled so this measures source visits rather than server setup.
;; Run with REMOTE_BENCHMARK_TARGET=target make remote-ssh-visit-benchmark.

;;; Code:

(require 'cl-lib)
(require 'remote-board)
(require 'remote-config)
(require 'remote-framework)
(require 'subr-x)

(defun remote-ssh-visit-benchmark--median (values)
  "Return the median of numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (count (length sorted)))
    (if (cl-oddp count)
        (nth (/ count 2) sorted)
      (/ (+ (nth (1- (/ count 2)) sorted)
            (nth (/ count 2) sorted))
         2.0))))

(defun remote-ssh-visit-benchmark--check-ratio (results)
  "Check optional logical/physical latency budget in RESULTS."
  (when-let* ((configured (getenv "REMOTE_VISIT_MAX_RATIO")))
    (let* ((limit (string-to-number configured))
           (physical (remote-ssh-visit-benchmark--median
                      (alist-get 'physical results)))
           (logical (remote-ssh-visit-benchmark--median
                     (alist-get 'logical results)))
           (ratio (/ logical physical)))
      (unless (> limit 0)
        (error "REMOTE_VISIT_MAX_RATIO must be positive"))
      (princ (format "logical/physical ratio=%.3fx budget=%.3fx\n"
                     ratio limit))
      (when (> ratio limit)
        (error "SSH logical visit regression: %.3fx exceeds %.3fx"
               ratio limit)))))

(defun remote-ssh-visit-benchmark--visit (file spelling)
  "Visit FILE through SPELLING and return elapsed seconds."
  (garbage-collect)
  (let* ((start (float-time))
         (buffer (find-file-noselect file))
         (elapsed (- (float-time) start)))
    (unwind-protect
        (progn
          (unless (buffer-live-p buffer)
            (error "Visit did not create a source buffer: %s" file))
          (with-current-buffer buffer
            (unless (eq (and (remote-fs-file-name-p buffer-file-name) t)
                        (eq spelling 'logical))
              (error "Visit spelling changed: %s -> %s"
                     file buffer-file-name)))
          elapsed)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defun remote-ssh-visit-benchmark-run ()
  "Measure warm physical versus logical C source visits on one SSH target."
  (let* ((target-id (getenv "REMOTE_BENCHMARK_TARGET"))
         (rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_VISIT_ROUNDS") "8"))))
         (target (progn
                   (remote-config-load)
                   (and target-id (remote-get-target target-id))))
         (bootstrap-context
          (and target
               (remote-context
                (remote-make-file-name target-id "/tmp/"))))
         (directory
          (and target
               (string-trim
                (remote-exec-output
                 "mktemp" :args '("-d" "/tmp/emacs-visit-bench.XXXXXX")
                 :context bootstrap-context :check t))))
         (logical-directory
          (and directory
               (file-name-as-directory
                (remote-make-file-name target-id directory))))
         (physical-directory
          (and logical-directory
               (file-name-as-directory
                (remote-project-file-name
                 logical-directory nil 'file-read "emacs-file"))))
         (warmup-order
          (if (equal (getenv "REMOTE_VISIT_WARMUP") "logical")
              '(logical physical)
            '(physical logical)))
         (order (cl-loop repeat rounds
                         append '(physical logical logical physical)))
         (results '((physical . nil) (logical . nil)))
         folder-buffer)
    (unless target
      (error "Set REMOTE_BENCHMARK_TARGET to a configured SSH target"))
    (unless (and directory
                 (string-match-p
                  "\\`/tmp/emacs-visit-bench\\.[[:alnum:]]+\\'"
                  directory))
      (error "Unexpected target benchmark directory: %S" directory))
    (unwind-protect
        (progn
          (remote-fs-install)
          (dotimes (index (+ 2 (length order)))
            (with-temp-file
                (expand-file-name
                 (format "unit-%02d.c" index) logical-directory)
              (insert "int main(void) { return 0; }\n")))
          (with-temp-file
              (expand-file-name ".projectile" logical-directory))
          (setq folder-buffer (remote-open-folder target directory))
          ;; A deferred LSP start may run while Emacs waits on target I/O;
          ;; the language-server smoke measures that separate path.
          (remove-hook 'prog-mode-hook #'my/language-server-ensure-deferred)
          (unwind-protect
              (progn
                (cl-loop for spelling in warmup-order
                         for index from 0
                         for root = (if (eq spelling 'physical)
                                        physical-directory
                                      logical-directory)
                         do (princ
                             (format "warmup %s=%.3f ms\n"
                                     spelling
                                     (* 1000
                                        (remote-ssh-visit-benchmark--visit
                                         (expand-file-name
                                         (format "unit-%02d.c" index)
                                          root)
                                         spelling)))))
                (cl-loop for spelling in order
                         for index from 2
                         for root = (if (eq spelling 'physical)
                                        physical-directory
                                      logical-directory)
                         for file = (expand-file-name
                                     (format "unit-%02d.c" index) root)
                         do (push (remote-ssh-visit-benchmark--visit
                                   file spelling)
                                  (alist-get spelling results)))
                (dolist (spelling '(physical logical))
                  (let* ((values (alist-get spelling results))
                         (count (length values)))
                    (princ
                     (format "%s count=%d median=%.3f ms mean=%.3f ms\n"
                             spelling count
                             (* 1000
                                (remote-ssh-visit-benchmark--median values))
                             (* 1000 (/ (apply #'+ values) count))))))
                (remote-ssh-visit-benchmark--check-ratio results))
            (add-hook 'prog-mode-hook
                      #'my/language-server-ensure-deferred)))
      (when-let* ((workspace (remote-get-workspace logical-directory)))
        (remote-workspace-close workspace 'benchmark-cleanup))
      (when (buffer-live-p folder-buffer)
        (kill-buffer folder-buffer))
      (when (and directory
                 (string-match-p
                  "\\`/tmp/emacs-visit-bench\\.[[:alnum:]]+\\'"
                  directory))
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" directory)
           :context bootstrap-context :check t))))))

(remote-ssh-visit-benchmark-run)

(provide 'remote-ssh-visit-benchmark)
;;; remote-ssh-visit-benchmark.el ends here
