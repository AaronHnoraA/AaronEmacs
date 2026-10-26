;;; init-clutch-tests.el --- Tests for the clutch database client setup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; SQL and Redis files open in clutch's modes when clutch is installed and in
;; plain modes otherwise; clutch's limits are config settings.  Run with the
;; full configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defmacro init-clutch-tests--visiting (suffix &rest body)
  "Run BODY in a buffer visiting a new temporary file ending in SUFFIX."
  (declare (indent 1))
  `(let ((file (make-temp-file "clutch-" nil ,suffix "select 1;\n")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (unwind-protect (progn ,@body)
             (set-buffer-modified-p nil)
             (kill-buffer)))
       (delete-file file))))

(ert-deftest clutch-sql-files-open-in-clutch-mode ()
  (skip-unless (locate-library "clutch"))
  (init-clutch-tests--visiting ".sql"
    (should (eq major-mode 'clutch-mode))
    (should (derived-mode-p 'sql-mode))
    (should-not indent-tabs-mode)))

(ert-deftest clutch-sql-files-fall-back-to-sql-mode-without-clutch ()
  (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
    (init-clutch-tests--visiting ".sql"
      (should (eq major-mode 'sql-mode))
      (should-not indent-tabs-mode))))

(ert-deftest clutch-redis-files-open-in-clutch-redis-mode ()
  (skip-unless (locate-library "clutch"))
  (init-clutch-tests--visiting ".redis"
    (should (eq major-mode 'clutch-redis-mode))))

(ert-deftest clutch-limits-are-config-settings ()
  (dolist (setting '((clutch-connect-timeout-seconds . 10)
                     (clutch-read-idle-timeout-seconds . 30)
                     (clutch-query-timeout-seconds . 30)
                     (clutch-jdbc-rpc-timeout-seconds . 30)
                     (clutch-result-max-rows . 500)))
    (should (gethash (car setting) config--registry))
    (should (eql (config-get (car setting)) (cdr setting)))))

(provide 'init-clutch-tests)
;;; init-clutch-tests.el ends here
