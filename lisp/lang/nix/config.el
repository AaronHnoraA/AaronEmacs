;;; lang/nix/config.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package nix-mode
  :ensure t
  ;; Its autoloads open the files; nothing here needs it before then.
  :defer t)


(provide 'lang/nix/config)
