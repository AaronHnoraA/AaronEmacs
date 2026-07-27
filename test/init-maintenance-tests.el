;;; init-maintenance-tests.el --- Maintenance isolation tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-maintenance)

(ert-deftest maintenance-cleanup-stays-on-client-from-a-remote-buffer ()
  (let* ((expected (my/maintenance-config-root))
         (default-directory "/fs:box:/home/me/project/")
         (process-environment (copy-sequence process-environment))
         seen-directory
         seen-home)
    (setenv "HOME" "/home/me")
    (cl-letf (((symbol-function 'my/maintenance-var-cleanup--read-state)
               (lambda ()
                 (setq seen-directory default-directory
                       seen-home (getenv "HOME"))
                 (list :last-run (float-time)))))
      (my/maintenance-var-cleanup-maybe)
      (should (equal seen-directory expected))
      (should-not (equal seen-home "/home/me"))
      (should (equal (my/maintenance-config-root) expected)))))

(provide 'init-maintenance-tests)
;;; init-maintenance-tests.el ends here
