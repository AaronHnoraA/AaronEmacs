;;; init-joplin.el --- Joplin integration -*- lexical-binding: t; -*-

(my/package-ensure-vc 'joplin-mode "https://github.com/cinsk/joplin-mode.git")

(defvar joplin-token-file)

(use-package joplin-mode
  :commands joplin-note-mode
  :init
  (setq joplin-token-file
        (expand-file-name "etc/joplin.token" user-emacs-directory)))

(provide 'init-joplin)
