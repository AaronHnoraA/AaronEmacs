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

(require 'flymake-vale)

(use-package flymake-vale
  :hook
  (
(text-mode markdown-mode org-mode latex-mode) . flymake-vale-load
)
  :config
  ;; vale 可执行文件路径（如果服务器和本地不一样）
  (setq flymake-vale-executable "vale"))

;; 如果你是远程编辑（TRAMP），但 vale 只在本地
(defun my/flymake-vale-disable-on-remote ()
  (when (file-remote-p default-directory)
    (flymake-mode -1)))


(add-hook 'text-mode-hook #'flymake-vale-load)
(add-hook 'latex-mode-hook #'flymake-vale-load)
(add-hook 'org-mode-hook #'flymake-vale-load)
(add-hook 'markdown-mode-hook #'flymake-vale-load)
(add-hook 'message-mode-hook #'flymake-vale-load)

(add-hook 'find-file-hook 'flymake-vale-maybe-load)
;; flymake-vale-modes defaults to: 
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)

(add-to-list 'flymake-vale-modes 'adoc-mode)

(add-hook 'find-file-hook #'my/flymake-vale-disable-on-remote)
(setq flymake-fringe-indicator-position 'right-fringe)

(provide 'init-vale)

;;; init-elisp.el ends here
