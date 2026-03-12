(my/package-ensure-vc 'joplin-mode "https://github.com/cinsk/joplin-mode.git")

(use-package joplin-mode
  :requires markdown-mode
  :commands joplin-note-mode
  :hook (markdown-mode . joplin-note-mode))

(provide 'init-joplin)
