;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; asdadasdasd asdas
;;

;;; Code:
;; ---------------------------
;; Vale + Flymake (Spell Check)
;; ---------------------------
(my/package-ensure-vc 'flymake-vale "https://github.com/tpeacock19/flymake-vale.git")


;; flymake-vale
(use-package flymake-vale
  :ensure t
  :after flymake
  :config
  (setq flymake-vale-program "vale"
        flymake-vale-modes '(text-mode latex-mode org-mode markdown-mode
                                        message-mode typst-ts-mode typst-mode
                                        my/typst-mode)
        flymake-vale-mode-file-exts
        (append '((typst-ts-mode . "typ")
                  (typst-mode . "typ")
                  (my/typst-mode . "typ"))
                flymake-vale-mode-file-exts))

  ;; 一个统一的入口：本地文件才启用
  (defun my/flymake-vale-setup ()
    "Enable Flymake + Vale backend for local buffers."
      (flymake-mode 1)        ;; 关键：确保 flymake 初始化
      (flymake-vale-load))   ;; 再加载 vale backend

  :hook
  ((text-mode . my/flymake-vale-setup)
   (org-mode . my/flymake-vale-setup)
   (markdown-mode . my/flymake-vale-setup)
   (latex-mode . my/flymake-vale-setup)
   (LaTeX-mode . my/flymake-vale-setup)
   (tex-mode . my/flymake-vale-setup)
   (TeX-mode . my/flymake-vale-setup)
   (plain-tex-mode . my/flymake-vale-setup)
   (plain-TeX-mode . my/flymake-vale-setup)
   (typst-ts-mode . my/flymake-vale-setup)
   (typst-mode . my/flymake-vale-setup)
   (my/typst-mode . my/flymake-vale-setup)))

(setq flymake-fringe-indicator-position 'right-fringe)


(provide 'init-vale)

;;; init-elisp.el ends here
