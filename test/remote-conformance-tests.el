;;; remote-conformance-tests.el --- Native versus logical file oracle -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'remote-framework)
(require 'vc)
(require 'vc-git)

(defun remote-conformance--normalize (value)
  "Normalize logical file identities inside VALUE to native local names."
  (cond
   ((stringp value)
    (if (remote-fs-file-name-p value)
        (remote-file-local-name value)
      value))
   ((consp value)
    (cons (remote-conformance--normalize (car value))
          (remote-conformance--normalize (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'remote-conformance--normalize value)))
   (t value)))

(defun remote-conformance--outcome (function &rest arguments)
  "Return normalized success or structural error from FUNCTION ARGUMENTS."
  (condition-case error
      (list :ok
            (remote-conformance--normalize
             (apply function arguments)))
    (error
     (list :error
           (seq-intersection
            (get (car error) 'error-conditions)
            '(file-error file-missing file-already-exists
              permission-denied remote-file-error))))))

(ert-deftest remote-conformance-safe-query-surface-matches-native ()
  (remote-fs-install)
  (let* ((root (make-temp-file "remote-conformance-query-" t))
         (directory (expand-file-name "directory" root))
         (file (expand-file-name "directory/value.txt" root))
         (logical-root (remote-make-file-name "local" root))
         (logical-directory
          (remote-make-file-name "local" directory))
         (logical-file (remote-make-file-name "local" file)))
    (unwind-protect
        (progn
          (make-directory directory)
          (write-region "value\n" nil file nil 'silent)
          (dolist (entry
                   `((file-exists-p ,file ,logical-file)
                     (file-directory-p ,directory ,logical-directory)
                     (file-regular-p ,file ,logical-file)
                     (file-readable-p ,file ,logical-file)
                     (file-writable-p ,file ,logical-file)
                     (file-modes ,file ,logical-file)
                     (file-name-nondirectory ,file ,logical-file)
                     (directory-files ,root ,logical-root)))
            (pcase-let ((`(,function ,native ,logical) entry))
              (should
               (equal
                (remote-conformance--outcome function native)
                (remote-conformance--outcome function logical)))))
          (should
           (equal (file-truename file)
                  (remote-file-local-name
                   (file-truename logical-file)))))
      (delete-directory root t))))

(ert-deftest remote-conformance-mutation-and-symlink-semantics-match-native ()
  (remote-fs-install)
  (let* ((root (make-temp-file "remote-conformance-write-" t))
         (native-root (expand-file-name "native" root))
         (logical-native-root (expand-file-name "logical" root))
         (logical-root
          (remote-make-file-name "local" logical-native-root))
         (native-file (expand-file-name "value.txt" native-root))
         (logical-file (expand-file-name "value.txt" logical-root))
         (native-copy (expand-file-name "copy.txt" native-root))
         (logical-copy (expand-file-name "copy.txt" logical-root))
         (native-link (expand-file-name "link.txt" native-root))
         (logical-link (expand-file-name "link.txt" logical-root)))
    (unwind-protect
        (progn
          (make-directory native-root)
          (make-directory logical-root)
          (write-region "payload\n" nil native-file nil 'silent)
          (write-region "payload\n" nil logical-file nil 'silent)
          (copy-file native-file native-copy)
          (copy-file logical-file logical-copy)
          (make-symbolic-link "value.txt" native-link)
          (make-symbolic-link "value.txt" logical-link)
          (should (equal (file-symlink-p native-link)
                         (file-symlink-p logical-link)))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents native-copy)
              (buffer-string))
            (with-temp-buffer
              (insert-file-contents logical-copy)
              (buffer-string))))
          (delete-file native-copy)
          (delete-file logical-copy)
          (should-not (file-exists-p native-copy))
          (should-not (file-exists-p logical-copy)))
      (delete-directory root t))))

(ert-deftest remote-conformance-missing-file-errors-match-native ()
  (remote-fs-install)
  (let* ((root (make-temp-file "remote-conformance-error-" t))
         (native (expand-file-name "missing" root))
         (logical (remote-make-file-name "local" native)))
    (unwind-protect
        (should
         (equal
          (remote-conformance--outcome #'delete-file native)
          (remote-conformance--outcome #'delete-file logical)))
      (delete-directory root t))))

(ert-deftest remote-conformance-vc-registered-caches-under-the-logical-name ()
  "VC caches under the exact name it is handed.
A logical name must therefore be answered as itself, or `vc-backend' would
keep returning nil for a buffer that `vc-registered' has already resolved."
  (remote-fs-install)
  (skip-unless (executable-find "git"))
  (let* ((root (make-temp-file "remote-conformance-vc-" t))
         (file (expand-file-name "tracked.txt" root))
         (logical (remote-make-file-name "local" file))
         (vc-handled-backends '(Git)))
    (unwind-protect
        (let ((default-directory root)
              (process-environment
               (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                         "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                       process-environment)))
          (write-region "tracked\n" nil file nil 'silent)
          (call-process "git" nil nil nil "init" "--quiet" root)
          (call-process "git" nil nil nil "-C" root "add" "tracked.txt")
          (call-process "git" nil nil nil "-C" root "commit" "--quiet" "-m" "x")
          (vc-file-clearprops file)
          (vc-file-clearprops logical)
          (should (vc-registered logical))
          (should (eq (vc-file-getprop logical 'vc-backend) 'Git))
          (should (eq (vc-backend logical) 'Git)))
      (delete-directory root t))))

(ert-deftest remote-conformance-exec-path-matches-native ()
  "`exec-path' answers with target-native names, like Tramp's own handler.
Its callers add the remote prefix themselves, so projecting the result would
hand them a doubly prefixed directory and every probe below it would be a
round trip that cannot succeed."
  (remote-fs-install)
  (let ((default-directory
         (remote-make-file-name "local" temporary-file-directory)))
    (should (equal (exec-path) (default-value 'exec-path)))
    (should-not
     (seq-find #'remote-fs-file-name-p (exec-path)))))

(ert-deftest remote-conformance-operation-effects-are-total ()
  (dolist (spec (remote-file-operation-list))
    (should
     (memq (remote-file-operation-spec-filesystem-effects spec)
           '(none metadata content unknown))))
  (should
   (eq (remote-file-operation-spec-filesystem-effects
        (remote-get-file-operation 'file-exists-p))
       'none))
  (should
   (eq (remote-file-operation-spec-filesystem-effects
        (remote-get-file-operation 'write-region))
       'content))
  (should
   (eq (remote-file-operation-spec-filesystem-effects
        (remote-get-file-operation 'process-file))
       'unknown)))

(provide 'remote-conformance-tests)
;;; remote-conformance-tests.el ends here
