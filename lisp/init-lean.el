;;; init-lean.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'magit-section)

;;git clone https://github.com/leanprover-community/lean4-mode.git ~/path/to/lean4-mode

(unless (package-installed-p 'lean4-mode)
  (package-vc-install
   '(lean4-mode
     :url "https://github.com/leanprover-community/lean4-mode.git"
     :rev :last-release)))

(use-package lean4-mode
  :mode "\\.lean\\'")

(provide 'init-lean)



;;; init-base.el ends here
