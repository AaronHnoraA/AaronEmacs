;;; init-exit-save-tests.el --- A failed save does not trap Emacs open -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; While Emacs exits, a buffer whose save fails (an unreachable target, a
;; read-only directory) can be discarded instead of aborting the exit; see
;; the failure matrix in docs/design/remote-framework.md.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defmacro init-exit-save-tests--unsavable (&rest body)
  "Run BODY in a modified buffer whose file cannot be written."
  (declare (indent 0))
  `(with-temp-buffer
     (setq buffer-file-name "/nonexistent-directory-for-test/a.txt")
     (insert "changed")
     (unwind-protect (progn ,@body)
       (set-buffer-modified-p nil)
       (setq buffer-file-name nil))))

(ert-deftest exit-save-failure-can-be-discarded ()
  (init-exit-save-tests--unsavable
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?d "discard")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (let ((aaron--exiting t))
        (save-buffer))
      (should-not (buffer-modified-p)))))

(ert-deftest exit-save-failure-can-cancel-the-exit ()
  (init-exit-save-tests--unsavable
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?c "cancel")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (let ((aaron--exiting t))
        (should-error (save-buffer)))
      (should (buffer-modified-p)))))

(ert-deftest exit-save-failure-outside-exit-is-an-ordinary-error ()
  (init-exit-save-tests--unsavable
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (error "Must not ask")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (save-buffer))
      (should (buffer-modified-p)))))

(ert-deftest exit-save-failure-is-scoped-to-exiting ()
  (let (inside)
    (cl-letf (((symbol-function 'kill-emacs) (lambda (&rest _) (setq inside aaron--exiting)))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              (confirm-kill-emacs nil)
              (kill-emacs-query-functions nil))
      (save-buffers-kill-emacs t)
      (should inside)
      (should-not aaron--exiting))))

(provide 'init-exit-save-tests)
;;; init-exit-save-tests.el ends here
