;;; init-utils-tests.el --- Utility tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with:
;;   emacs --batch -Q -L lisp -l test/init-utils-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-utils)

(ert-deftest my/ghostel-wrap-quotes-argv ()
  (should
   (equal (my/ghostel-wrap--shell-command
           '("tool" "plain" "two words" "$(unsafe)"))
          "exec tool plain two\\ words \\$\\(unsafe\\)")))

(ert-deftest my/ghostel-wrap-preserves-shell-command ()
  (let ((shell-file-name "/bin/test shell"))
    (should
     (equal (my/ghostel-wrap--shell-command "first | second")
            "exec /bin/test\\ shell -lc first\\ \\|\\ second"))))

(ert-deftest my/ghostel-wrap-rejects-invalid-command ()
  (should-error (my/ghostel-wrap--shell-command nil) :type 'user-error)
  (should-error (my/ghostel-wrap--shell-command '()) :type 'user-error)
  (should-error (my/ghostel-wrap--shell-command '("")) :type 'user-error)
  (should-error (my/ghostel-wrap--shell-command '("tool" 1)) :type 'user-error))

(ert-deftest my/ghostel-wrap-creates-self-cleaning-ghostel ()
  (let (sent-command created-directory buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (let ((original-require (symbol-function 'require)))
                     (lambda (feature &optional filename noerror)
                       (if (eq feature 'ghostel)
                           t
                         (funcall original-require feature filename noerror)))))
                  ((symbol-function 'ghostel-create)
                   (lambda (&optional name _display _identity)
                     (setq created-directory default-directory)
                     (setq buffer (get-buffer-create name))))
                  ((symbol-function 'my/ghostel-send-command)
                   (lambda (target command &optional _retries)
                     (should (eq target buffer))
                     (setq sent-command command))))
          (let ((result (my/ghostel-wrap '("tool" "two words")
                                       :directory temporary-file-directory
                                       :display nil)))
            (should (eq result buffer))
            (should (equal sent-command "exec tool two\\ words"))
            (should (equal created-directory
                           (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
            (should (buffer-live-p buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'init-utils-tests)

;;; init-utils-tests.el ends here
