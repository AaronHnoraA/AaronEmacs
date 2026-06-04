;;; init-babel.el --- Standalone Org Babel configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-babel-core)
(require 'init-babel-mermaid)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(provide 'init-babel)
;;; init-babel.el ends here
