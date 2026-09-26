;;; remote-directory-benchmark.el --- Directory browsing latency -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in same-process Dired/Dirvish comparison.  The SSH fixture is created
;; by one target-side shell command, so setup does not bias per-entry latency.
;; REMOTE_BENCHMARK_TARGET=target make remote-directory-benchmark

;;; Code:

(require 'cl-lib)
(require 'remote-board)
(require 'remote-config)
(require 'remote-framework)
(require 'subr-x)

(defvar remote-directory-benchmark--rpc-count 0)
(defvar remote-directory-benchmark--operations nil)

(defun remote-directory-benchmark--rpc-a (function &rest args)
  "Count RPC invocations while calling FUNCTION with ARGS."
  (cl-incf remote-directory-benchmark--rpc-count)
  (apply function args))

(defun remote-directory-benchmark--handler-a (function operation &rest args)
  "Count logical file OPERATION before calling FUNCTION with ARGS."
  (puthash operation (1+ (gethash operation
                                 remote-directory-benchmark--operations 0))
           remote-directory-benchmark--operations)
  (apply function operation args))

(defun remote-directory-benchmark--populate-local (directory count)
  "Put COUNT empty source files in DIRECTORY."
  (dotimes (index count)
    (write-region "" nil
                  (expand-file-name (format "unit-%04d.c" index) directory)
                  nil 'silent)))

(defun remote-directory-benchmark--populate-target (context directory count)
  "Put COUNT empty source files in target DIRECTORY using CONTEXT."
  (remote-exec-output
   "/bin/sh"
   :args (list "-c"
               "d=$1; n=$2; i=0; while [ \"$i\" -lt \"$n\" ]; do : > \"$d/unit-$(printf '%04d' \"$i\").c\"; i=$((i+1)); done"
               "sh" directory (number-to-string count))
   :context context :check t))

(defun remote-directory-benchmark--operation-summary ()
  "Return a descending summary of counted logical file operations."
  (let (entries)
    (maphash (lambda (key value) (push (cons key value) entries))
             remote-directory-benchmark--operations)
    (cl-subseq (sort entries (lambda (a b) (> (cdr a) (cdr b))))
               0 (min 8 (length entries)))))

(defun remote-directory-benchmark--listed-file-count (buffer)
  "Count fixture source files currently visible in Dired BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (how-many "unit-[0-9]+\\.c" (point-min) (point-max)))))

(defun remote-directory-benchmark--measure (label directory expected-count)
  "Measure opening and refreshing DIRECTORY, identified by LABEL."
  (garbage-collect)
  (setq remote-directory-benchmark--rpc-count 0
        remote-directory-benchmark--operations (make-hash-table :test 'eq))
  (let* ((start (float-time))
         (buffer (find-file-noselect directory))
         (open-ms (* 1000 (- (float-time) start)))
         (open-rpc remote-directory-benchmark--rpc-count)
         (open-operations (remote-directory-benchmark--operation-summary))
         (open-count (remote-directory-benchmark--listed-file-count buffer))
         refresh-ms refresh-rpc refresh-operations refresh-count)
    (unwind-protect
        (progn
          (unless (with-current-buffer buffer (derived-mode-p 'dired-mode))
            (error "%s opened in %s, expected Dired" label
                   (buffer-local-value 'major-mode buffer)))
          (setq remote-directory-benchmark--rpc-count 0
                remote-directory-benchmark--operations
                (make-hash-table :test 'eq))
          (setq start (float-time))
          (with-current-buffer buffer (revert-buffer t t))
          (setq refresh-ms (* 1000 (- (float-time) start))
                refresh-rpc remote-directory-benchmark--rpc-count
                refresh-operations
                (remote-directory-benchmark--operation-summary)
                refresh-count
                (remote-directory-benchmark--listed-file-count buffer))
          (princ (format "%s open=%.1fms files=%d rpc=%d ops=%S refresh=%.1fms files=%d rpc=%d ops=%S\n"
                         label open-ms open-count open-rpc open-operations
                         refresh-ms refresh-count refresh-rpc refresh-operations))
          (unless (and (= open-count expected-count)
                       (= refresh-count expected-count))
            (error "%s listing incomplete: open=%d refresh=%d expected=%d"
                   label open-count refresh-count expected-count)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun remote-directory-benchmark--measure-managed-open
    (target directory logical-directory expected-count)
  "Measure the full managed folder open on TARGET for DIRECTORY."
  (garbage-collect)
  (setq remote-directory-benchmark--rpc-count 0
        remote-directory-benchmark--operations (make-hash-table :test 'eq))
  (let ((remote-board-recent-folders nil)
        (start (float-time))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (remote-open-folder target directory))
          (let ((elapsed (* 1000 (- (float-time) start)))
                (listed (remote-directory-benchmark--listed-file-count buffer)))
            (princ
             (format "managed-folder-open=%.1fms files=%d rpc=%d ops=%S\n"
                     elapsed listed remote-directory-benchmark--rpc-count
                     (remote-directory-benchmark--operation-summary)))
            (unless (= listed expected-count)
              (error "Managed folder listing incomplete: %d expected=%d"
                     listed expected-count))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let* ((workspace (remote-get-workspace logical-directory)))
        (remote-workspace-close workspace 'benchmark-cleanup)))))

(defun remote-directory-benchmark-run ()
  "Compare local and SSH directory browsing with native and logical paths."
  (let* ((target-id (getenv "REMOTE_BENCHMARK_TARGET"))
         (count (max 1 (string-to-number
                        (or (getenv "REMOTE_DIRECTORY_FILES") "400"))))
         (rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_DIRECTORY_ROUNDS") "3"))))
         (target (progn (remote-config-load)
                        (and target-id (remote-get-target target-id)))))
    (unless target
      (error "Set REMOTE_BENCHMARK_TARGET to a configured SSH target"))
    (let* ((bootstrap-context
            (remote-context (remote-make-file-name target-id "/tmp/")))
           (local-directory
            (file-name-as-directory
             (make-temp-file "emacs-dir-bench." t)))
           (target-directory
            (string-trim
             (remote-exec-output
              "mktemp" :args '("-d" "/tmp/emacs-dir-bench.XXXXXX")
              :context bootstrap-context :check t)))
           (logical-local
            (remote-make-file-name "local" local-directory))
           (logical-ssh
            (file-name-as-directory
             (remote-make-file-name target-id target-directory)))
           (physical-ssh
            (file-name-as-directory
             (remote-project-file-name
              logical-ssh nil 'file-read "emacs-file"))))
      (unless (string-match-p
               "\\`/tmp/emacs-dir-bench\\.[[:alnum:]]+\\'"
               target-directory)
        (error "Unexpected target benchmark directory: %S"
               target-directory))
      (unwind-protect
          (progn
            (remote-fs-install)
            (remote-directory-benchmark--populate-local
             local-directory count)
            (remote-directory-benchmark--populate-target
             bootstrap-context target-directory count)
            (when (fboundp 'tramp-rpc--call)
              (advice-add 'tramp-rpc--call :around
                          #'remote-directory-benchmark--rpc-a))
            (advice-add 'remote-fs-file-name-handler :around
                        #'remote-directory-benchmark--handler-a)
            (princ (format "files=%d rounds=%d\n" count rounds))
            (dotimes (round rounds)
              (princ (format "round=%d\n" (1+ round)))
              (dolist (entry
                       `((native-local . ,local-directory)
                         (logical-local . ,logical-local)
                         (physical-ssh . ,physical-ssh)
                         (logical-ssh . ,logical-ssh)))
                (remote-directory-benchmark--measure
                 (symbol-name (car entry)) (cdr entry) count)))
            (remote-directory-benchmark--measure-managed-open
             target target-directory logical-ssh count))
        (advice-remove 'remote-fs-file-name-handler
                       #'remote-directory-benchmark--handler-a)
        (when (fboundp 'tramp-rpc--call)
          (advice-remove 'tramp-rpc--call
                         #'remote-directory-benchmark--rpc-a))
        (ignore-errors (delete-directory local-directory t))
        (when (string-match-p
               "\\`/tmp/emacs-dir-bench\\.[[:alnum:]]+\\'"
               target-directory)
          (ignore-errors
            (remote-exec "rm" :args (list "-rf" target-directory)
                         :context bootstrap-context :check t)))))))

(remote-directory-benchmark-run)

(provide 'remote-directory-benchmark)
;;; remote-directory-benchmark.el ends here
