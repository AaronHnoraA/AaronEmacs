;;; init-rename.el --- Safer file rename helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(defun my/rename-current-file-dwim (newname)
  "Rename current file to NEWNAME, preferring VC-aware rename when possible."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Rename to: " nil buffer-file-name 'confirm))))
  (if (and (fboundp 'vc-backend)
           (vc-backend buffer-file-name)
           (fboundp 'vc-rename-file))
      (vc-rename-file buffer-file-name newname)
    (+rename-current-file newname)))

(my/leader!
  "f m" '(:def my/rename-current-file-dwim :which-key "rename file dwim"))

(provide 'init-rename)
;;; init-rename.el ends here
