
(unless (package-installed-p 'joplin-mode)
  (package-vc-install
   '(joplin-mode
     :url "https://github.com/cinsk/joplin-mode.git"
     :rev :last-release)))

(use-package joplin-mode
  :requires markdown-mode
  :config
  (add-to-list 'markdown-mode-hook 'joplin-note-mode))

(provide 'init-joplin)
