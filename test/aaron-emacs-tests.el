;;; aaron-emacs-tests.el --- Tests for lifecycle hooks -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make core-test'.  Batch Emacs is noninteractive, so the module
;; installs none of its triggers; each test wires the piece it checks.

;;; Code:

(require 'ert)
(require 'aaron-emacs)

(defvar aaron-emacs-tests--log nil)
(defvar aaron-emacs-tests-mode-local-vars-hook nil)

(define-derived-mode aaron-emacs-tests-mode fundamental-mode "LT")

(ert-deftest aaron-emacs-local-vars-hook-runs-once-after-locals ()
  (let ((aaron-emacs-tests--log nil)
        (aaron-emacs-tests-mode-local-vars-hook
         (list (lambda () (push 'ran aaron-emacs-tests--log))))
        (hack-local-variables-hook (list #'aaron-run-local-var-hooks-h)))
    (with-temp-buffer
      (rename-buffer "aaron-emacs-test" t)
      (aaron-emacs-tests-mode)
      (run-hooks 'hack-local-variables-hook)
      (run-hooks 'hack-local-variables-hook)
      (should (equal aaron-emacs-tests--log '(ran))))))

(ert-deftest aaron-emacs-local-vars-hook-skips-internal-buffers ()
  (let ((aaron-emacs-tests--log nil)
        (aaron-emacs-tests-mode-local-vars-hook
         (list (lambda () (push 'ran aaron-emacs-tests--log)))))
    (with-temp-buffer                   ; " *temp*"
      (aaron-emacs-tests-mode)
      (aaron-run-local-var-hooks-h)
      (should (null aaron-emacs-tests--log)))))

(ert-deftest aaron-emacs-local-vars-hook-without-local-variables ()
  (let ((aaron-emacs-tests--log nil)
        (enable-local-variables nil)
        (aaron-emacs-tests-mode-local-vars-hook
         (list (lambda () (push 'ran aaron-emacs-tests--log)))))
    (with-temp-buffer
      (rename-buffer "aaron-emacs-test" t)
      (aaron-emacs-tests-mode)
      (aaron-run-local-var-hooks-maybe-h)
      (should (equal aaron-emacs-tests--log '(ran))))))

(ert-deftest aaron-emacs-incremental-loading-registers-without-now ()
  (let ((aaron-incremental-packages '(a)))
    (aaron-load-packages-incrementally '(b c))
    (should (equal aaron-incremental-packages '(a b c)))))

(ert-deftest aaron-emacs-incremental-loading-loads-and-chains ()
  (let ((loaded nil)
        (scheduled nil)
        (aaron--incremental-timer nil)
        (aaron-incremental-first-idle-timer 2.0))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 5)))
              ((symbol-function 'aaron--incremental-require)
               (lambda (f) (push f loaded) t))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args)
                 (setq scheduled (cons fn args))
                 'timer)))
      (aaron-load-packages-incrementally '(aaron-emacs-tests--a aaron-emacs-tests--b) t)
      (should (equal loaded '(aaron-emacs-tests--a)))
      (should (eq (car scheduled) #'aaron-load-packages-incrementally))
      (should (equal (cadr scheduled) '(aaron-emacs-tests--b)))
      (should (eq aaron--incremental-timer 'timer)))))

(ert-deftest aaron-emacs-incremental-loading-skips-loaded-features ()
  (let ((loaded nil)
        (aaron--incremental-timer nil))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 5)))
              ((symbol-function 'aaron--incremental-require)
               (lambda (f) (push f loaded) t)))
      (aaron-load-packages-incrementally '(cl-lib aaron-lib) t)
      (should (null loaded))
      (should (null aaron--incremental-timer)))))

(ert-deftest aaron-emacs-incremental-loading-waits-while-busy ()
  (let ((loaded nil)
        (scheduled nil)
        (aaron--incremental-timer nil)
        (aaron-incremental-first-idle-timer 2.0))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 0.5)))
              ((symbol-function 'aaron--incremental-require)
               (lambda (f) (push f loaded) t))
              ((symbol-function 'run-at-time)
               (lambda (time _repeat _fn &rest args)
                 (setq scheduled (cons time args))
                 'timer)))
      (aaron-load-packages-incrementally '(aaron-emacs-tests--a) t)
      (should (null loaded))
      (should (equal scheduled '(0.75 (aaron-emacs-tests--a) t))))))

(ert-deftest aaron-emacs-incremental-require-uses-client-directory ()
  (let (seen)
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _)
                 (setq seen (list default-directory file-name-handler-alist)))))
      (with-temp-buffer
        (setq default-directory "/fs:somewhere:/tmp/")
        (aaron--incremental-require 'aaron-emacs-tests--a)))
    (should (equal (car seen) (expand-file-name user-emacs-directory)))
    (should (cl-every (lambda (cell) (eq (cdr cell) 'jka-compr-handler))
                      (cadr seen)))))

(ert-deftest aaron-emacs-switch-buffer-hook-ignores-unchanged-buffer ()
  (let ((aaron-emacs-tests--log nil)
        (aaron-switch-buffer-hook (list (lambda () (push 'switched aaron-emacs-tests--log)))))
    (cl-letf (((symbol-function 'window-old-buffer) (lambda (&rest _) (current-buffer))))
      (aaron-run-switch-buffer-hooks-h)
      (should (null aaron-emacs-tests--log)))
    (cl-letf (((symbol-function 'window-old-buffer) (lambda (&rest _) nil)))
      (aaron-run-switch-buffer-hooks-h)
      (should (equal aaron-emacs-tests--log '(switched))))))

(ert-deftest aaron-emacs-finalize-catches-up-on-command-line-files ()
  (let ((aaron-emacs-tests--log nil)
        (aaron-incremental-first-idle-timer nil)
        (aaron-first-file-hook (list (lambda () (push 'file aaron-emacs-tests--log))))
        (aaron-first-buffer-hook (list (lambda () (push 'buffer aaron-emacs-tests--log)))))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/aaron-emacs-test.txt")
      (unwind-protect (aaron-finalize-h)
        (setq buffer-file-name nil)))
    (should (equal aaron-emacs-tests--log '(buffer file)))
    (should (null aaron-first-file-hook))
    (should (null aaron-first-buffer-hook))))

(provide 'aaron-emacs-tests)
;;; aaron-emacs-tests.el ends here
