;;; remote-source-tests.el --- Routed source lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'remote-framework)
(require 'remote-source)

(defun remote-source-test-await (start)
  (let (done result problem)
    (funcall start (lambda (value error-object) (setq result value problem error-object done t)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (not done) (< (float-time) deadline)) (accept-process-output nil 0.02)))
    (should done)
    (when problem (error "Source error: %S" problem))
    result))

(ert-deftest remote-source-local-process-is-workspace-owned-and-versioned ()
  (let* ((root (make-temp-file "remote-source-ert-" t))
         (workspace (remote-workspace-open (remote-make-file-name "local" root) :connect nil))
         source)
    (unwind-protect
        (progn
          (remote-source-test-await
           (lambda (callback)
             (setq source (remote-source-open workspace (remote-make-file-name "local" root)
                                             :options '((hidden . :json-false) (extensions . [".md"]))
                                             :callback callback))))
          (should (eq (remote-source-state source) 'open))
          (remote-source-test-await
           (lambda (callback) (remote-source-request source '((op . "write") (path . "task.md")
                                                              (content . "中文🚀") (expectedRevision . nil)) callback)))
          (should (equal (alist-get 'content (remote-source-test-await
                                              (lambda (callback) (remote-source-request source '((op . "read") (path . "task.md")) callback))))
                         "中文🚀"))
          (should (memq (remote-source-resource source) (remote-workspace-resources workspace)))
          (remote-workspace-close workspace)
          (should-not (process-live-p (remote-source-process source)))
          (should (= (hash-table-count (remote-source-pending source)) 0))
          (let (error-object)
            (remote-source-request source '((op . "list")) (lambda (_ error) (setq error-object error)))
            (should (equal (alist-get 'code error-object) "ECLOSED"))))
      (when source (remote-source-close source))
      (remote-workspace-close workspace)
      (delete-directory root t))))

(ert-deftest remote-source-nonlocal-context-reaches-process-boundary ()
  ;; Contract test: no SSH connection is fabricated or silently run locally.
  (let* ((context (remote-context-create :target-id "source-lab" :localname "/work/"
                                         :workspace-root "/fs:source-lab:/work/"))
         (workspace (remote-workspace-open context :connect nil))
         launch)
    (unwind-protect
        (cl-letf (((symbol-function 'remote-make-process)
                   (lambda (&rest arguments) (setq launch arguments) (error "test boundary"))))
          (should-error (remote-source-open workspace "/fs:source-lab:/work/"))
          (should (equal (remote-context-target-id (plist-get launch :remote-context)) "source-lab"))
          (should (equal (plist-get launch :remote-adapter) "source-files"))
          (should (equal (seq-take (plist-get launch :command) 2) '("node" "-e")))
          (should-not (remote-workspace-resources workspace)))
      (remote-workspace-close workspace))))

(ert-deftest remote-source-recovery-rejects-pending-and-ignores-old-generation ()
  (let* ((root (make-temp-file "remote-source-recovery-" t))
         (workspace (remote-workspace-open (remote-make-file-name "local" root) :connect nil))
         source failures)
    (unwind-protect
        (progn
          (remote-source-test-await
           (lambda (callback)
             (setq source (remote-source-open workspace (remote-make-file-name "local" root) :callback callback))))
          (let* ((generation (remote-source-generation source))
                 (old-process (remote-source-process source))
                 (timer (run-at-time 60 nil #'ignore)))
            (puthash 900 (cons (lambda (_ error-object) (push error-object failures)) timer)
                     (remote-source-pending source))
            (remote-workspace-recover-resource workspace (remote-source-resource source))
            (should-not (process-live-p old-process))
            (should (= (length failures) 1))
            (should-not (memq timer timer-list))
            (remote-source--filter source generation old-process "{\"id\":900,\"result\":{}}\n")
            (should (= (length failures) 1))
            ;; Requests queue behind the new handshake in the target process.
            (should (alist-get 'files
                               (remote-source-test-await
                                (lambda (callback)
                                  (remote-source-request source '((op . "write") (path . "restored.md")
                                                                 (content . "恢复") (expectedRevision . nil))
                                                         (lambda (_ error-object)
                                                           (if error-object (funcall callback nil error-object)
                                                             (remote-source-request source '((op . "list")) callback))))))))
            (should (eq (remote-source-state source) 'open))
            (should (= (hash-table-count (remote-source-pending source)) 0))))
      (when source (remote-source-close source))
      (remote-workspace-close workspace)
      (delete-directory root t))))

(provide 'remote-source-tests)
