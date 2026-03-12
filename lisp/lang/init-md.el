;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(my/package-ensure-vc 'markdown-mode "https://github.com/jrblevin/markdown-mode.git")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'init-md)

;;; init-bazel.el ends here
