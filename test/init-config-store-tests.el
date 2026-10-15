;;; init-config-store-tests.el --- Config store failure tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration, as with init-interaction-utils-tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-tools)

(ert-deftest config-store-write-failure-stays-visible-until-retried ()
  "A failed save must not report success or permit discarding unsaved edits."
  (let* ((directory (make-temp-file "config-save-test-" t))
         (file (expand-file-name "config-store.el" directory))
         (config-store-file file)
         (config-extra-store-files nil)
         (config--configured-store-files-cache nil)
         (config--overrides nil)
         (config--override-index (make-hash-table :test 'equal))
         (config--override-store-files (make-hash-table :test 'equal))
         (config--loaded-store-files (make-hash-table :test 'equal))
         (config--persist-failed nil)
         (original-write (symbol-function 'config--write-store-file)))
    (unwind-protect
        (progn
          (config--put-override 'config-test-sample 42)
          (puthash 'config-test-sample file config--override-store-files)
          (cl-letf (((symbol-function 'config--store-files)
                     (lambda () (list file)))
                    ((symbol-function 'config--write-store-file)
                     (lambda (_file _overrides)
                       (error "simulated disk failure"))))
            (should-error (config-save) :type 'file-error)
            (should config--persist-failed)
            (should (cl-some (lambda (issue)
                               (string-match-p "unsaved changes" issue))
                             (config--integrity-issues)))
            (should-error (config-refresh-store-files) :type 'user-error))
          (cl-letf (((symbol-function 'config--store-files)
                     (lambda () (list file)))
                    ((symbol-function 'config--write-store-file) original-write))
            (config-save))
          (should-not config--persist-failed)
          (should (string-match-p "config-test-sample"
                                  (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest config-refresh-preserves-state-on-malformed-store ()
  "Partial reads must not replace working overrides or claim refresh success."
  (let* ((directory (make-temp-file "config-refresh-test-" t))
         (file (expand-file-name "config-store.el" directory))
         (config-store-file file)
         (config-extra-store-files nil)
         (config--configured-store-files-cache nil)
         (config--overrides nil)
         (config--override-index (make-hash-table :test 'equal))
         (config--override-store-files (make-hash-table :test 'equal))
         (config--loaded-store-files (make-hash-table :test 'equal))
         (config--persist-failed nil))
    (unwind-protect
        (progn
          (config--put-override 'config-test-old 7)
          (puthash 'config-test-old file config--override-store-files)
          (with-temp-file file
            (insert "; first form is valid\n"
                    "(config-store-set '((config-test-new . 8)))\n"
                    "(config-store-set '((broken . 9))\n"))
          (should-error (config-refresh-store-files) :type 'end-of-file)
          (should (equal config--overrides '((config-test-old . 7))))
          (should (gethash 'config-test-old config--override-index))
          (should-not (gethash 'config-test-new config--override-index))
          (should (equal (gethash 'config-test-old config--override-store-files)
                         file))
          (should (cl-some (lambda (issue)
                             (string-match-p "fails to parse" issue))
                           (config--integrity-issues)))
          (with-temp-file file
            (insert "; repaired\n"
                    "(config-store-set '((config-test-new . 8)))\n"
                    "; trailing comment\n"))
          (config-refresh-store-files)
          (should (equal config--overrides '((config-test-new . 8))))
          (should-not (gethash 'config-test-old config--override-index))
          (should (gethash 'config-test-new config--override-index)))
      (delete-directory directory t))))

(ert-deftest config-custom-save-all-restores-native-state-on-write-failure ()
  "A failed store write must not leave Customize's saved state half-cleared."
  (let* ((symbol 'config-test-custom-save-symbol)
         (config-custom--managed (make-hash-table :test 'eq))
         (saved '((quote saved-value)))
         (customized '((quote edited-value))))
    (unwind-protect
        (progn
          (puthash symbol t config-custom--managed)
          (put symbol 'saved-value saved)
          (put symbol 'customized-value customized)
          (cl-letf (((symbol-function 'config-custom--persist-symbol)
                     (lambda (_symbol) (error "simulated store write failure"))))
            (should-error
             (config-custom--around-custom-save-all
              (lambda (&rest _) (ert-fail "native save should not run")))))
          (should (equal (get symbol 'saved-value) saved))
          (should (equal (get symbol 'customized-value) customized)))
      (put symbol 'saved-value nil)
      (put symbol 'customized-value nil))))

(provide 'init-config-store-tests)
;;; init-config-store-tests.el ends here
