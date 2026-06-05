;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; asdadasdasd asdas
;;

;;; Code:
;; ---------------------------
;; Vale + Flymake (Spell Check)
;; ---------------------------
(my/package-ensure-vc 'flymake-vale "https://github.com/tpeacock19/flymake-vale.git")

(defun my/flymake-vale-setup ()
  "Enable Flymake and the Vale backend for eligible local buffers."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (executable-find "vale")
             (require 'flymake-vale nil t))
    (flymake-mode 1)
    (flymake-vale-load)))

;; flymake-vale
(use-package flymake-vale
  :ensure t
  :after flymake
  :config
  (setq flymake-vale-program "vale"
        flymake-vale-modes '(text-mode latex-mode org-mode message-mode))

  :hook
  ((text-mode . my/flymake-vale-setup)
   (org-mode . my/flymake-vale-setup)
   (latex-mode . my/flymake-vale-setup)
   (LaTeX-mode . my/flymake-vale-setup)
   (tex-mode . my/flymake-vale-setup)
   (TeX-mode . my/flymake-vale-setup)
   (plain-tex-mode . my/flymake-vale-setup)
   (plain-TeX-mode . my/flymake-vale-setup)))

(setq flymake-fringe-indicator-position 'right-fringe)


(provide 'init-vale)

;;; init-elisp.el ends here
