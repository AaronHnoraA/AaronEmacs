;;; remote-ssh-write-benchmark.el --- Repeated SSH file-write probe -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in live benchmark for repeated writes to one temporary target file.
;; REMOTE_BENCHMARK_TARGET=target make remote-ssh-write-benchmark
;; REMOTE_WRITE_CACHE=off disables the adapter's attribute-probe cache.
;; REMOTE_WRITE_MODE=save-buffer measures an already-visited source buffer.
;; REMOTE_WRITE_VISIT=physical measures the selected backend name directly.
;; REMOTE_WRITE_METADATA=off restores generic TRAMP metadata round trips.

;;; Code:

(require 'cl-lib)
(require 'remote-config)
(require 'remote-framework)
(require 'tramp-rpc)

(defvar remote-ssh-write-benchmark--methods (make-hash-table :test #'equal))
(defvar remote-ssh-write-benchmark--commands (make-hash-table :test #'equal))

(defun remote-ssh-write-benchmark--call-a
    (function vec method params &optional connection)
  "Count METHOD and its process command before calling FUNCTION."
  (cl-incf (gethash method remote-ssh-write-benchmark--methods 0))
  (when (equal method "process.run")
    (cl-incf (gethash (alist-get 'cmd params)
                     remote-ssh-write-benchmark--commands 0)))
  (funcall function vec method params connection))

(defun remote-ssh-write-benchmark--median (values)
  "Return the median in numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (count (length sorted)))
    (if (cl-oddp count)
        (nth (/ count 2) sorted)
      (/ (+ (nth (1- (/ count 2)) sorted)
            (nth (/ count 2) sorted))
         2.0))))

(defun remote-ssh-write-benchmark--entry-less-p (left right)
  "Sort command or method count entries LEFT and RIGHT by name."
  (string< (format "%s" (car left)) (format "%s" (car right))))

(defun remote-ssh-write-benchmark--write (file)
  "Overwrite FILE and return elapsed seconds."
  (let ((started (float-time)))
    (with-temp-buffer
      (insert "int main(void) { return 0; }\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (- (float-time) started)))

(defun remote-ssh-write-benchmark--save-buffer (buffer index)
  "Save already visited BUFFER with fresh INDEX content and return seconds."
  (with-current-buffer buffer
    (erase-buffer)
    (insert (format "save %d\n" index))
    (let ((make-backup-files nil)
          (started (float-time)))
      (save-buffer)
      (- (float-time) started))))

(defun remote-ssh-write-benchmark-run ()
  "Measure repeated target file saves after warming one SSH connection."
  (remote-config-load)
  (remote-fs-install)
  (when (equal (getenv "REMOTE_WRITE_METADATA") "off")
    (setq remote-backend-tramp-rpc-skip-in-place-metadata-roundtrip nil)
    (remote-backend-tramp-rpc-install))
  (let* ((target-id (or (getenv "REMOTE_BENCHMARK_TARGET")
                        (error "Set REMOTE_BENCHMARK_TARGET")))
         (target (or (remote-get-target target-id)
                     (error "Unknown target %s" target-id)))
         (rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_WRITE_ROUNDS") "8"))))
         (save-mode (equal (getenv "REMOTE_WRITE_MODE") "save-buffer"))
         (logical-file
          (remote-make-file-name
           (remote-target-id target)
           (format "/tmp/emacs-write-benchmark-%d.%s"
                   (emacs-pid) (if save-mode "txt" "c"))))
         (file
          (if (equal (getenv "REMOTE_WRITE_VISIT") "physical")
              (remote-project-file-name
               logical-file nil 'file-write "emacs-file")
            logical-file))
         (remote-backend-tramp-rpc-attribute-probe-ttl
          (if (equal (getenv "REMOTE_WRITE_CACHE") "off")
              0
            remote-backend-tramp-rpc-attribute-probe-ttl))
         buffer values)
    (unless (plist-get (remote-backend-tramp-rpc-compat-report)
                       :attribute-probe-cache)
      (error "Installed tramp-rpc has no verified attribute-probe cache"))
    (advice-add 'tramp-rpc--call :around
                #'remote-ssh-write-benchmark--call-a)
    (unwind-protect
        (progn
          (remote-ssh-write-benchmark--write file)
          (when save-mode
            (setq buffer (find-file-noselect file)))
          (clrhash remote-ssh-write-benchmark--methods)
          (clrhash remote-ssh-write-benchmark--commands)
          (dotimes (index rounds)
            (push (if save-mode
                      (remote-ssh-write-benchmark--save-buffer buffer index)
                    (remote-ssh-write-benchmark--write file))
                  values))
          (princ
           (format "writes=%d mode=%s visit=%s median=%.3f ms mean=%.3f ms cache=%s metadata=%s ttl=%S\n"
                   rounds
                   (if save-mode "save-buffer" "write-region")
                   (if (equal (getenv "REMOTE_WRITE_VISIT") "physical")
                       "physical" "logical")
                   (* 1000 (remote-ssh-write-benchmark--median values))
                   (* 1000 (/ (apply #'+ values) rounds))
                   (if (equal (getenv "REMOTE_WRITE_CACHE") "off")
                       "off" "on")
                   (if (equal (getenv "REMOTE_WRITE_METADATA") "off")
                       "off" "on")
                   remote-backend-tramp-rpc-attribute-probe-ttl))
          (let (methods commands)
            (maphash (lambda (key count) (push (cons key count) methods))
                     remote-ssh-write-benchmark--methods)
            (maphash (lambda (key count) (push (cons key count) commands))
                     remote-ssh-write-benchmark--commands)
            (princ (format "rpc-methods=%S\n"
                           (sort methods
                                 #'remote-ssh-write-benchmark--entry-less-p)))
            (princ (format "process-commands=%S\n"
                           (sort commands
                                 #'remote-ssh-write-benchmark--entry-less-p)))))
      (advice-remove 'tramp-rpc--call
                     #'remote-ssh-write-benchmark--call-a)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(remote-ssh-write-benchmark-run)

(provide 'remote-ssh-write-benchmark)
;;; remote-ssh-write-benchmark.el ends here
