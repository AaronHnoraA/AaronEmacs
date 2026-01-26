;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; asdadasdasd asdas
;;

;;; Code:
;; ---------------------------
;; Vale + Flymake (Spell Check)
;; ---------------------------
(unless (package-installed-p 'flymake-vale) 
  (package-vc-install '
                      (flymake-vale
                      :url "https://github.com/tpeacock19/flymake-vale.git" 
                      :rev 
                      :last-release)
                      )
) 


;; flymake-vale
(use-package flymake-vale
  :ensure t
  :after flymake
  :config
  (setq flymake-vale-executable "vale")

  :hook
  ((text-mode . my/flymake-vale-setup)
   (org-mode . my/flymake-vale-setup)
   (markdown-mode . my/flymake-vale-setup)
   (latex-mode . my/flymake-vale-setup)))

(setq flymake-fringe-indicator-position 'right-fringe)

(provide 'init-vale)

;;; init-elisp.el ends here
