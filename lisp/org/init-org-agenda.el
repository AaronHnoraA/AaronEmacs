;;; init-org-agenda.el --- Org agenda views -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-files nil)
  (setq org-agenda-diary-file nil)

  :custom
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-outline-path t)
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " %i %-12:c %s")
     (tags   . " %i %-12:c %s")
     (search . " %i %-12:c %s")))
  (org-agenda-hide-tags-regexp ".")

  (org-agenda-custom-commands
   '(("o" "Overview / Dashboard"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-overriding-header "⚡ Today's Schedule & Deadlines")))
       (tags-todo "+uni/!TODO|NEXT"
                  ((org-agenda-overriding-header "🎓 University Tasks")))
       (tags-todo "+math+cs+qc+research/!TODO|NEXT"
                  ((org-agenda-overriding-header "🔬 Research & Gaps")))
       (todo "NEXT"
             ((org-agenda-overriding-header "🚀 Next Actions")))
       (todo "WAIT"
             ((org-agenda-overriding-header "⏳ Waiting")))
       (tags "inbox"
             ((org-agenda-overriding-header "📥 Unprocessed Inbox"))))))))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
