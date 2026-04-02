;;; init-org-code.el --- Org code execution extras -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-babel)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(provide 'init-org-code)
;;; init-org-code.el ends here
