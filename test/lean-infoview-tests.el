;;; lean-infoview-tests.el --- Lean Infoview lifecycle tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'init-lean)
(require 'init-lean-infoview)

(ert-deftest lean-infoview-proxy-port-files-are-instance-isolated ()
  (let ((lean--proxy-port-files (make-hash-table :test #'equal))
        (lean--proxy-port-sequence 0)
        (root (file-name-as-directory user-emacs-directory)))
    (let ((first (lean--proxy-allocate-port-file root))
          (second (lean--proxy-allocate-port-file root)))
      (should-not (equal first second))
      (should (equal (lean--proxy-port-file root) second)))))

(ert-deftest lean-infoview-project-root-shares-eglot-proxy-identity ()
  (let* ((directory (make-temp-file "lean-infoview-root-" t))
         (default-directory (file-name-as-directory directory))
         (buffer-file-name (expand-file-name "Main.lean" directory))
         (toolchain (expand-file-name "lean-toolchain" directory))
         (lean--proxy-port-files (make-hash-table :test #'equal))
         (lean--proxy-port-sequence 0))
    (unwind-protect
        (progn
          (with-temp-file toolchain
            (insert "leanprover/lean4:stable\n"))
          (let* ((eglot-root (lean-project-root))
                 (port-file (lean--proxy-allocate-port-file eglot-root))
                 (infoview-root (lean--iv-project-root)))
            (should (equal infoview-root eglot-root))
            (should (lean--proxy-port-file-allocated-p infoview-root))
            (should (equal (lean--proxy-port-file infoview-root)
                           port-file))))
      (delete-directory directory t))))

(ert-deftest lean-infoview-manual-start-cancels-automatic-timer ()
  (with-temp-buffer
    (let ((buffer-file-name
           (expand-file-name "Main.lean" temporary-file-directory))
          (default-directory temporary-file-directory)
          (lean--eglot-waiting-for-environment nil)
          (lean-eglot-connect-timeout nil)
          (starts 0))
      (setq lean--eglot-start-timer
            (run-at-time 60 nil #'ignore))
      (unwind-protect
          (cl-letf (((symbol-function 'eglot-managed-p)
                     (lambda () nil))
                    ((symbol-function 'my/direnv-update-environment-maybe)
                     (lambda (&optional _path _callback) nil))
                    ((symbol-function 'my/eglot-start-now)
                     (lambda (&optional _interactive)
                       (cl-incf starts))))
            (lean--ensure-eglot)
            (should (= starts 1))
            (should-not lean--eglot-start-timer))
        (lean--cancel-eglot-start-timer)))))

(ert-deftest lean-infoview-rejects-a-dead-local-port-owner ()
  (let ((port-file
         (make-temp-file "lean-infoview-dead-owner-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file port-file
            (insert
             (json-encode
              '(:port 65511 :pid 2147483000))))
          (should-not (lean--iv-read-port port-file)))
      (when (file-exists-p port-file)
        (delete-file port-file)))))

(ert-deftest lean-infoview-accepts-a-live-local-port-owner ()
  (let ((port-file
         (make-temp-file "lean-infoview-live-owner-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file port-file
            (insert
             (json-encode
              `(:port 43123 :pid ,(emacs-pid)))))
          (should (= (lean--iv-read-port port-file) 43123)))
      (when (file-exists-p port-file)
        (delete-file port-file)))))

(ert-deftest lean-infoview-port-wait-follows-a-new-eglot-instance ()
  (let* ((pending (make-temp-name
                   (expand-file-name "lean-infoview-pending-" temporary-file-directory)))
         (active (make-temp-file "lean-infoview-active-" nil ".json"))
         (current pending)
         (lean-iv-port-wait-timeout 2)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'lean--proxy-port-file)
                   (lambda (_root) current)))
          (lean--iv-wait-for-port
           user-emacs-directory
           (lambda (port) (setq result port)))
          (with-temp-file active
            (insert
             (json-encode
              `(:port 43124 :pid ,(emacs-pid)))))
          (setq current active)
          (let ((deadline (+ (float-time) 1.5)))
            (while (and (not result)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (= result 43124)))
      (when (file-exists-p active)
        (delete-file active)))))

(ert-deftest lean-infoview-reconnect-uses-eglots-interactive-server-lookup ()
  (let (seen)
    (cl-letf (((symbol-function 'eglot-reconnect)
               (lambda (server &optional interactive)
                 (interactive (list 'current-server t))
                 (setq seen (list server interactive)))))
      (lean--iv-reconnect-eglot)
      (should (equal seen '(current-server t))))))

(provide 'lean-infoview-tests)
;;; lean-infoview-tests.el ends here
