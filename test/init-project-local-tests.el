;;; init-project-local-tests.el --- Project-local setting tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/project-local/config)
(require 'emacs/dir-locals/config)

(ert-deftest +project-local-toolchain-defaults-are-safe-but-commands-are-not ()
  (should
   (+project-local-settings-safe-p
    '(:toolchain ((python . sage))
      :noema-jupyter (:language sage :kernel sagemath :session default))))
  (should-not
   (+project-local-settings-safe-p
    '(:toolchain-profiles
      ((local . (:server-program ("custom-language-server")))))))
  (should-not (+project-local-settings-safe-p '(:toolchain))))

(ert-deftest +project-local-dir-locals-opens-on-its-own-machine ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function '+project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+project-local/open-dir-locals))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(ert-deftest +dir-locals-edit-uses-local-project-root-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function '+project-local-root)
               (lambda () "/Users/test/project/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+dir-locals/edit))
    (should (equal visited "/Users/test/project/.dir-locals.el"))
    (should-not (file-remote-p context))))

(provide 'init-project-local-tests)
;;; init-project-local-tests.el ends here
