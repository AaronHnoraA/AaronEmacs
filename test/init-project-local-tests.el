;;; init-project-local-tests.el --- Project-local setting tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-project-local)
(require 'init-dir-locals)

(ert-deftest my/project-local-toolchain-defaults-are-safe-but-commands-are-not ()
  (should
   (my/project-local-settings-safe-p
    '(:toolchain ((python . sage))
      :noema-jupyter (:language sage :kernel sagemath :session default))))
  (should-not
   (my/project-local-settings-safe-p
    '(:toolchain-profiles
      ((local . (:server-program ("custom-language-server")))))))
  (should-not (my/project-local-settings-safe-p '(:toolchain))))

(ert-deftest my/project-local-dir-locals-opens-on-its-own-machine ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function 'my/project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (my/project-local-open-dir-locals))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(ert-deftest my/dir-locals-edit-uses-local-project-root-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function 'my/project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (my/dir-locals-edit))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(provide 'init-project-local-tests)
;;; init-project-local-tests.el ends here
