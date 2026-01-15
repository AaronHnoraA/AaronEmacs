;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package yasnippet
  :ensure t
  :init
  ;; 关键：用 Emacs 的配置根目录，而不是写死 ~/.emacs.d
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(provide 'init-snippets)



;;; init-base.el ends here
