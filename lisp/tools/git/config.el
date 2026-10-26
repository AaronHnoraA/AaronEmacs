;;; tools/git/config.el --- Load Git configuration modules -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(dolist (command '(+github/topic-open
                   +github/topic-insert
                   +github/topic-open-by-author
                   +github/contributor-insert
                   +github/pr-patch-preview
                   +github/refresh))
  (autoload command "tools/git/github" nil t))

(require 'tools/git/core)
(require 'tools/git/board)
(require 'tools/git/diff)
(require 'tools/git/tree)

(provide 'tools/git/config)
;;; tools/git/config.el ends here
