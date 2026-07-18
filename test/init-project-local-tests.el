;;; init-project-local-tests.el --- Project-local setting tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-project-local)

(ert-deftest my/project-local-toolchain-defaults-are-safe-but-commands-are-not ()
  (should
   (my/project-local-settings-safe-p
    '(:toolchain ((python . sage))
      :aaronnote-jupyter (:language sage :kernel sagemath :session default))))
  (should-not
   (my/project-local-settings-safe-p
    '(:toolchain-profiles
      ((local . (:server-program ("custom-language-server")))))))
  (should-not (my/project-local-settings-safe-p '(:toolchain))))

(provide 'init-project-local-tests)
;;; init-project-local-tests.el ends here
