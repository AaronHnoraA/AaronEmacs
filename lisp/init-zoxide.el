;;; init-zoxide.el --- zoxide integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Extra helpers built on top of `zoxide.el'.

;;; Code:

(declare-function zoxide-open-with "zoxide" (query action &optional dir-only))
(declare-function dired-jump "dired-x" (&optional other-window file-name))

(my/package-ensure-vc 'zoxide "https://github.com/emacsmirror/zoxide.git")

(use-package zoxide
  :commands (zoxide-open-with))

(use-package dired-x
  :ensure nil)

(defun dired-jump-with-zoxide (&optional other-window)
  "Jump to a zoxide directory with `dired-jump'."
  (interactive "P")
  (zoxide-open-with nil
                    (lambda (file)
                      (dired-jump other-window file))
                    t))

(provide 'init-zoxide)
;;; init-zoxide.el ends here
