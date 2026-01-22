(unless (package-installed-p 'sage-shell)
  (package-vc-install
   '(sage-shell
     :url "https://github.com/sagemath/sage-shell-mode.git"
     :rev :last-release)))


(require 'sage-shell-mode)

(provide 'init-sage)
