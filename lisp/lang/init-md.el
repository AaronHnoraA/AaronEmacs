;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(unless (package-installed-p 'markdown-mode)
  (package-vc-install
   '(markdown-mode
     :url "https://github.com/jrblevin/markdown-mode.git"
     :rev :last-release)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'init-md)

;;; init-bazel.el ends here
