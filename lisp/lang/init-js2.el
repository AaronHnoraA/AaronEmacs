(use-package js2-mode
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init-js2)
