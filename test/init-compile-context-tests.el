;;; init-compile-context-tests.el --- Client ownership of config builds -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-compile-context-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config/compile/config)

(ert-deftest +compile-config-runs-on-client-from-remote-buffer ()
  "Config compilation must use client paths and environment."
  (let (seen)
    (with-temp-buffer
      (setq-local default-directory "/fs:compile-test:/project/")
      (setq-local remote-buffer-target-id "compile-test")
      (setq-local process-environment '("REMOTE_ONLY=1"))
      (setq-local exec-path '("/remote/bin"))
      (cl-letf (((symbol-function '+compile--root-file-paths)
                 (lambda () '("/tmp/test-config.el")))
                ((symbol-function '+compile--directory-paths)
                 (lambda () nil))
                ((symbol-function '+compile--maybe-byte-compile-file)
                 (lambda (_file _force)
                   (setq seen (list default-directory process-environment
                                    exec-path remote-buffer-target-id))
                   nil))
                ((symbol-function '+compile--refresh-board-if-visible)
                 #'ignore))
        (+compile/byte-compile-config)))
    (should (equal (nth 0 seen) +compile-config-root))
    (should (equal (nth 1 seen) (default-value 'process-environment)))
    (should (equal (nth 2 seen) (default-value 'exec-path)))
    (should-not (nth 3 seen))))

(ert-deftest +compile-board-uses-client-context-from-remote-buffer ()
  "The board keeps local config state after opening from a Remote source."
  (let ((+compile-board-buffer-name "*Compile Context Test*")
        seen)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:compile-test:/project/")
          (setq-local remote-buffer-target-id "compile-test")
          (setq-local process-environment '("REMOTE_ONLY=1"))
          (setq-local exec-path '("/remote/bin"))
          (cl-letf (((symbol-function '+compile-board/refresh)
                     (lambda ()
                       (setq seen (list default-directory process-environment
                                        exec-path remote-buffer-target-id))))
                    ((symbol-function 'pop-to-buffer) #'ignore))
            (+compile/board))
          (should (equal (nth 0 seen) +compile-config-root))
          (should (equal (nth 1 seen) (default-value 'process-environment)))
          (should (equal (nth 2 seen) (default-value 'exec-path)))
          (should-not (nth 3 seen)))
      (when-let* ((buffer (get-buffer +compile-board-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest +compile-rejects-remote-path-before-client-file-check ()
  "A target file with a similar suffix is not a managed local config file."
  (with-temp-buffer
    (setq-local default-directory "/fs:compile-test:/project/")
    (should-not
     (+compile--path-in-config-p
      (concat "/fs:compile-test:" +compile-config-root "lisp/init.el")))))

(ert-deftest +native-comp-purge-keeps-only-the-running-build ()
  "Purging deletes other builds' cache directories and keeps this build's."
  (let* ((cache (make-temp-file "eln-cache" t))
         (+native-comp-cache-dir (file-name-as-directory cache))
         (comp-native-version-dir "31_1-current")
         (current (expand-file-name "31_1-current" cache))
         (old (expand-file-name "31_0_91-old" cache)))
    (unwind-protect
        (progn
          (dolist (dir (list current old))
            (make-directory dir t)
            (write-region "" nil (expand-file-name "a.eln" dir)))
          (write-region "" nil (expand-file-name "README" cache))
          (should (equal (+native-comp--stale-version-dirs) (list old)))
          (should (= (+compile--count-cache-eln-files) 1))
          (cl-letf (((symbol-function '+compile--refresh-board-if-visible)
                     #'ignore))
            (should (= (+native-comp/purge-stale-versions) 1)))
          (should (file-exists-p (expand-file-name "a.eln" current)))
          (should-not (file-exists-p old))
          (should (file-exists-p (expand-file-name "README" cache)))
          (should-not (+native-comp--stale-version-dirs)))
      (delete-directory cache t))))

(provide 'init-compile-context-tests)
;;; init-compile-context-tests.el ends here
