;;; init-compile-context-tests.el --- Client ownership of config builds -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-compile-context-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-compile)

(ert-deftest my/compile-config-runs-on-client-from-remote-buffer ()
  "Config compilation must use client paths and environment."
  (let (seen)
    (with-temp-buffer
      (setq-local default-directory "/fs:compile-test:/project/")
      (setq-local remote-buffer-target-id "compile-test")
      (setq-local process-environment '("REMOTE_ONLY=1"))
      (setq-local exec-path '("/remote/bin"))
      (cl-letf (((symbol-function 'my/compile--root-file-paths)
                 (lambda () '("/tmp/test-config.el")))
                ((symbol-function 'my/compile--directory-paths)
                 (lambda () nil))
                ((symbol-function 'my/compile--maybe-byte-compile-file)
                 (lambda (_file _force)
                   (setq seen (list default-directory process-environment
                                    exec-path remote-buffer-target-id))
                   nil))
                ((symbol-function 'my/compile--refresh-board-if-visible)
                 #'ignore))
        (my/byte-compile-config)))
    (should (equal (nth 0 seen) my/compile-config-root))
    (should (equal (nth 1 seen) (default-value 'process-environment)))
    (should (equal (nth 2 seen) (default-value 'exec-path)))
    (should-not (nth 3 seen))))

(ert-deftest my/compile-board-uses-client-context-from-remote-buffer ()
  "The board keeps local config state after opening from a Remote source."
  (let ((my/compile-board-buffer-name "*Compile Context Test*")
        seen)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:compile-test:/project/")
          (setq-local remote-buffer-target-id "compile-test")
          (setq-local process-environment '("REMOTE_ONLY=1"))
          (setq-local exec-path '("/remote/bin"))
          (cl-letf (((symbol-function 'my/compile-board-refresh)
                     (lambda ()
                       (setq seen (list default-directory process-environment
                                        exec-path remote-buffer-target-id))))
                    ((symbol-function 'pop-to-buffer) #'ignore))
            (my/compile-board))
          (should (equal (nth 0 seen) my/compile-config-root))
          (should (equal (nth 1 seen) (default-value 'process-environment)))
          (should (equal (nth 2 seen) (default-value 'exec-path)))
          (should-not (nth 3 seen)))
      (when-let* ((buffer (get-buffer my/compile-board-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest my/compile-rejects-remote-path-before-client-file-check ()
  "A target file with a similar suffix is not a managed local config file."
  (with-temp-buffer
    (setq-local default-directory "/fs:compile-test:/project/")
    (should-not
     (my/compile--path-in-config-p
      (concat "/fs:compile-test:" my/compile-config-root "lisp/init.el")))))

(provide 'init-compile-context-tests)
;;; init-compile-context-tests.el ends here
