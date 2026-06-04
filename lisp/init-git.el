;;; init-git.el --- Load Git configuration modules -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (and this-file (file-name-directory this-file)))
       (git-dir (and this-dir (expand-file-name "git" this-dir))))
  (when (and git-dir (file-directory-p git-dir))
    (add-to-list 'load-path (file-name-as-directory git-dir))))

(require 'init-git-core)
(require 'init-git-board)
(require 'init-git-diff)
(require 'init-git-tree)

(provide 'init-git)
;;; init-git.el ends here
