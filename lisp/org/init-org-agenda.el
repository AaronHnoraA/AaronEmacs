;;; init-org-agenda.el --- Org agenda and task workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)
(require 'calendar)
(require 'org)
(require 'org-clock)
(require 'project nil t)
(require 'transient)

(defcustom my/org-task-tag-alist
  '(("inbox" . ?i)
    ("draft" . ?d)
    ("working" . ?w)
    ("review" . ?r)
    ("stable" . ?s)
    ("blocked" . ?b)
    ("followup" . ?f)
    ("confusion" . ?u)
    ("shush" . ?y)
    ("question" . ?q)
    ("idea" . ?I)
    ("concept" . ?c)
    ("method" . ?m)
    ("definition" . nil)
    ("theorem" . nil)
    ("proof" . ?p)
    ("example" . ?x)
    ("counterexample" . nil)
    ("summary" . ?S)
    ("paper" . ?P)
    ("book" . nil)
    ("dataset" . nil)
    ("experiment" . nil)
    ("research" . ?R)
    ("learning" . ?l)
    ("writing" . nil)
    ("teaching" . nil)
    ("work" . ?W)
    ("dev" . ?D)
    ("meeting" . ?M)
    ("project" . nil)
    ("reading" . nil)
    ("math" . nil)
    ("tcs" . ?T)
    ("cs" . nil)
    ("qc" . nil)
    ("crypto" . nil)
    ("complexity" . nil)
    ("algorithm" . nil)
    ("logic" . nil)
    ("philosophy" . nil)
    ("physics" . nil))
  "Fast Org tags for task state, study, research, work and development."
  :type '(repeat (cons string (choice character (const nil))))
  :group 'org)

(defconst my/org-task-tag-face-alist
  `(("confusion" . (:foreground ,(aaron-ui-color 'accent-yellow)
                    :weight semibold :slant italic))
    ("shush" . (:foreground ,(aaron-ui-color 'accent-green)
                :weight semibold))
    ("blocked" . (:foreground ,(aaron-ui-color 'accent-red-strong)
                  :weight semibold))
    ("followup" . (:foreground ,(aaron-ui-color 'accent-orange)
                   :weight semibold))
    ("review" . (:foreground ,(aaron-ui-color 'accent-mauve)
                 :weight semibold))
    ("working" . (:foreground ,(aaron-ui-color 'accent-cyan)
                  :weight semibold))
    ("stable" . (:foreground ,(aaron-ui-color 'accent-green-soft)
                 :weight semibold))
    ("draft" . (:foreground ,(aaron-ui-color 'fg-dim)))
    ("inbox" . (:foreground ,(aaron-ui-color 'accent-blue)
                :weight semibold))
    ("idea" . (:foreground ,(aaron-ui-color 'accent-yellow-soft)
               :weight semibold))
    ("question" . (:foreground ,(aaron-ui-color 'accent-orange)
                   :weight semibold))
    ("concept" . (:foreground ,(aaron-ui-color 'accent-teal)))
    ("method" . (:foreground ,(aaron-ui-color 'accent-cyan)))
    ("definition" . (:foreground ,(aaron-ui-color 'accent-blue)))
    ("theorem" . (:foreground ,(aaron-ui-color 'accent-mauve)
                  :weight semibold))
    ("proof" . (:foreground ,(aaron-ui-color 'accent-cyan)
                :weight semibold))
    ("example" . (:foreground ,(aaron-ui-color 'accent-green-soft)))
    ("counterexample" . (:foreground ,(aaron-ui-color 'accent-red-soft)
                         :weight semibold))
    ("summary" . (:foreground ,(aaron-ui-color 'accent-sand)))
    ("paper" . (:foreground ,(aaron-ui-color 'accent-lavender)))
    ("research" . (:foreground ,(aaron-ui-color 'accent-mauve)))
    ("learning" . (:foreground ,(aaron-ui-color 'accent-blue)))
    ("work" . (:foreground ,(aaron-ui-color 'accent-orange)))
    ("dev" . (:foreground ,(aaron-ui-color 'accent-cyan)))
    ("math" . (:foreground ,(aaron-ui-color 'accent-teal)))
    ("tcs" . (:foreground ,(aaron-ui-color 'accent-mauve)
              :weight semibold))
    ("cs" . (:foreground ,(aaron-ui-color 'accent-blue)))
    ("qc" . (:foreground ,(aaron-ui-color 'accent-cyan))))
  "Static Org tag faces for task, study and research tags.")

(defcustom my/org-agenda-scope-excluded-directory-names
  '(".git" ".cache" ".direnv" ".venv" "node_modules" "public" "ltximg")
  "Directory names skipped by scoped Org agenda scans."
  :type '(repeat string)
  :group 'org)

(defvar my/org-agenda--last-scope nil
  "Most recent scoped agenda root and file count.")

(defun my/org-agenda--default-directory ()
  "Return a safe local directory for scoped Org agenda scans."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory
        my-org-root))))

(defun my/org-agenda--inside-org-root-p (dir)
  "Return non-nil when DIR is inside `my-org-root'."
  (file-in-directory-p (file-truename dir)
                       (file-name-as-directory (file-truename my-org-root))))

(defun my/org-agenda--project-root ()
  "Return the current project root, or nil."
  (when (fboundp 'project-current)
    (when-let* ((project (project-current nil)))
      (file-name-as-directory (expand-file-name (project-root project))))))

(defun my/org-agenda-scope-root ()
  "Return the root used by scoped Org agenda and tags views.
Inside `my-org-root', scan the whole Org library.  Outside it, scan the
current project.  If there is no project, scan the current directory."
  (let ((dir (my/org-agenda--default-directory)))
    (cond
     ((my/org-agenda--inside-org-root-p dir)
      (file-name-as-directory (file-truename my-org-root)))
     ((my/org-agenda--project-root))
     (t dir))))

(defun my/org-agenda--excluded-dir-p (dir)
  "Return non-nil when DIR should be skipped by agenda scans."
  (member (file-name-nondirectory (directory-file-name dir))
          my/org-agenda-scope-excluded-directory-names))

(defun my/org-agenda--org-files-recursive (dir)
  "Return Org files below DIR, pruning ignored directories."
  (let (files)
    (when (and (file-directory-p dir)
               (not (file-remote-p dir)))
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (cond
         ((and (file-directory-p entry)
               (not (file-symlink-p entry))
               (not (my/org-agenda--excluded-dir-p entry)))
          (setq files
                (nconc (my/org-agenda--org-files-recursive entry) files)))
         ((and (file-regular-p entry)
               (string-match-p "\\.org\\'" entry))
          (push (file-truename entry) files)))))
    files))

(defun my/org-agenda-scoped-files ()
  "Scan and return Org files for the current agenda scope.
This is intentionally called only by agenda/tag entry points, not by hooks or
idle timers."
  (let* ((root (my/org-agenda-scope-root))
         (files (sort (delete-dups (my/org-agenda--org-files-recursive root))
                      #'string<)))
    (setq my/org-agenda--last-scope (list :root root :count (length files)))
    files))

(defmacro my/org-agenda-with-scoped-files (&rest body)
  "Run BODY with `org-agenda-files' bound to the current scoped scan."
  (declare (indent 0) (debug t))
  `(let ((org-agenda-files (my/org-agenda-scoped-files)))
     ,@body))

(defun my/org-agenda (&optional arg keys restriction)
  "Open `org-agenda' with files scoped to Org root, project or current dir.
The scan happens only when this command is invoked."
  (interactive "P")
  (my/org-agenda-with-scoped-files
    (org-agenda arg keys restriction)))

(defun my/org-refile (&optional arg default-buffer rfloc msg)
  "Run `org-refile' with files scoped to Org root, project or current dir.
The scan happens only when this command is invoked."
  (interactive "P")
  (my/org-agenda-with-scoped-files
    (org-refile arg default-buffer rfloc msg)))

(defun my/org-task--ensure-org ()
  "Ensure the current buffer is an Org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer")))

(defun my/org-task--ensure-heading ()
  "Move to the current Org heading, or signal a user error."
  (my/org-task--ensure-org)
  (unless (org-at-heading-p)
    (org-back-to-heading t)))

(defun my/org-task-set-state (state)
  "Set current heading TODO state to STATE."
  (my/org-task--ensure-heading)
  (org-todo state))

(defun my/org-task-set-todo () (interactive) (my/org-task-set-state "TODO"))
(defun my/org-task-set-next () (interactive) (my/org-task-set-state "NEXT"))
(defun my/org-task-set-wip () (interactive) (my/org-task-set-state "WIP"))
(defun my/org-task-set-wait () (interactive) (my/org-task-set-state "WAIT"))
(defun my/org-task-set-hold () (interactive) (my/org-task-set-state "HOLD"))
(defun my/org-task-set-review () (interactive) (my/org-task-set-state "REVIEW"))
(defun my/org-task-set-done () (interactive) (my/org-task-set-state "DONE"))
(defun my/org-task-set-cancelled () (interactive) (my/org-task-set-state "CANCELLED"))
(defun my/org-task-set-dropped () (interactive) (my/org-task-set-state "DROPPED"))

(defun my/org-task-toggle-tag (tag)
  "Toggle TAG on the current Org heading."
  (interactive
   (list (completing-read "Tag: " (mapcar #'car my/org-task-tag-alist) nil t)))
  (my/org-task--ensure-heading)
  (org-toggle-tag tag)
  (message "Toggled Org tag: %s" tag))

(defun my/org-task-toggle-confusion () (interactive) (my/org-task-toggle-tag "confusion"))
(defun my/org-task-toggle-shush () (interactive) (my/org-task-toggle-tag "shush"))
(defun my/org-task-toggle-blocked () (interactive) (my/org-task-toggle-tag "blocked"))
(defun my/org-task-toggle-followup () (interactive) (my/org-task-toggle-tag "followup"))
(defun my/org-task-toggle-review-tag () (interactive) (my/org-task-toggle-tag "review"))

(defun my/org-task-agenda-overview ()
  "Open the personal Org agenda overview."
  (interactive)
  (my/org-agenda nil "o"))

(defun my/org-task-tags-view (match)
  "Open an Org tags view for MATCH."
  (my/org-agenda-with-scoped-files
    (org-tags-view nil match)))

(defun my/org-task-show-confusion () (interactive) (my/org-task-tags-view "confusion"))
(defun my/org-task-show-shush () (interactive) (my/org-task-tags-view "shush"))
(defun my/org-task-show-blocked () (interactive) (my/org-task-tags-view "blocked"))
(defun my/org-task-show-followup () (interactive) (my/org-task-tags-view "followup"))
(defun my/org-task-show-review () (interactive) (my/org-task-tags-view "review"))

(defun my/org-task-open-docs ()
  "Open the Org task workflow documentation."
  (interactive)
  (find-file (expand-file-name "docs/org-task-workflow.md" user-emacs-directory)))

(defun my/org-task-insert-active-timestamp ()
  "Insert an active Org timestamp."
  (interactive)
  (my/org-task--ensure-org)
  (call-interactively #'org-time-stamp))

(defun my/org-task-insert-inactive-timestamp ()
  "Insert an inactive Org timestamp."
  (interactive)
  (my/org-task--ensure-org)
  (call-interactively #'org-time-stamp-inactive))

(defun my/org-task-clock-in ()
  "Clock into the current Org heading."
  (interactive)
  (my/org-task--ensure-heading)
  (call-interactively #'org-clock-in))

(defun my/org-task-clock-out ()
  "Clock out of the current Org clock."
  (interactive)
  (call-interactively #'org-clock-out))

;;; ----------------------------------------------------------------------------
;;; Task states, refiling, tags and archive behavior
;;; ----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :bind (("C-c a" . my/org-agenda))
  :custom
  ;; --- Todo Keywords & Faces ---
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WIP(i!)" "WAIT(w@/!)" "HOLD(h@/!)"
               "REVIEW(r!)" "|"
               "DONE(d!)" "CANCELLED(c@/!)" "DROPPED(k@/!)")))

  (org-todo-keyword-faces
   `(("TODO" . (:foreground ,(aaron-ui-color 'accent-red-strong)
               :background ,(aaron-ui-color 'bg-danger)
               :weight semibold
               :box (:line-width -1 :color ,(aaron-ui-color 'bg-danger-strong))))
     ("NEXT" . (:foreground ,(aaron-ui-color 'accent-green)
               :background ,(aaron-ui-color 'bg-success)
               :weight semibold
               :box (:line-width -1 :color ,(aaron-ui-color 'bg-success-strong))))
     ("WIP" . (:foreground ,(aaron-ui-color 'accent-cyan)
              :background ,(aaron-ui-color 'bg-meta)
              :weight semibold
              :box (:line-width -1 :color ,(aaron-ui-color 'bg-meta-strong))))
     ("WAIT" . (:foreground ,(aaron-ui-color 'accent-yellow)
               :background ,(aaron-ui-color 'bg-surface)
               :weight medium
               :box (:line-width -1 :color ,(aaron-ui-color 'bg-surface-strong))))
     ("HOLD" . (:foreground ,(aaron-ui-color 'accent-orange)
               :background ,(aaron-ui-color 'bg-surface)
               :weight medium
               :box (:line-width -1 :color ,(aaron-ui-color 'bg-surface-strong))))
     ("REVIEW" . (:foreground ,(aaron-ui-color 'accent-mauve)
                 :background ,(aaron-ui-color 'bg-overlay)
                 :weight semibold
                 :box (:line-width -1 :color ,(aaron-ui-color 'border-subtle))))
     ("DONE" . (:foreground ,(aaron-ui-color 'accent-green-soft)
               :background ,(aaron-ui-color 'bg-success)
               :weight medium
               :box (:line-width -1 :color ,(aaron-ui-color 'bg-success-strong))))
     ("CANCELLED" . (:foreground ,(aaron-ui-color 'fg-faint)
                    :background ,(aaron-ui-color 'bg-surface)
                    :weight medium
                    :strike-through t
                    :box (:line-width -1 :color ,(aaron-ui-color 'border-subtle))))
     ("DROPPED" . (:foreground ,(aaron-ui-color 'fg-faint)
                  :background ,(aaron-ui-color 'bg-surface)
                  :weight medium
                  :strike-through t
                  :box (:line-width -1 :color ,(aaron-ui-color 'border-subtle))))))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)
  (org-priority-faces
   `((?A :foreground ,(aaron-ui-color 'accent-red) :weight medium)
     (?B :foreground ,(aaron-ui-color 'accent-orange))
     (?C :foreground ,(aaron-ui-color 'accent-yellow))))

  ;; --- Properties ---
  (org-global-properties
   '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("STYLE_ALL" . "habit")))
  (org-cycle-hide-drawer-startup t)

  ;; --- Refiling / Archiving ---
  (org-archive-location "%s_archive::datetree/")
  (org-refile-use-cache nil)
  (org-refile-targets '((nil . (:maxlevel . 9))
                        (org-agenda-files . (:maxlevel . 9))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; --- Tags ---
  (org-tag-alist my/org-task-tag-alist)
  (org-tag-faces my/org-task-tag-face-alist)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t))

;;; ----------------------------------------------------------------------------
;;; Agenda views
;;; ----------------------------------------------------------------------------

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
       (todo "WIP"
             ((org-agenda-overriding-header "🔧 In Progress")))
       (todo "REVIEW"
             ((org-agenda-overriding-header "🔎 Review")))
       (todo "WAIT"
             ((org-agenda-overriding-header "⏳ Waiting")))
       (tags-todo "blocked"
                  ((org-agenda-overriding-header "⛔ Blocked")))
       (tags "confusion"
             ((org-agenda-overriding-header "❓ Confusion / Unclear")))
       (tags "shush"
             ((org-agenda-overriding-header "💡 Remember This")))
       (tags "inbox"
             ((org-agenda-overriding-header "📥 Unprocessed Inbox"))))))))

;;; ----------------------------------------------------------------------------
;;; Interactive task/status surface
;;; ----------------------------------------------------------------------------

(transient-define-prefix my/org-task-dispatch ()
  "Org task, status and marker tools."
  [["State"
    ("t" "TODO" my/org-task-set-todo)
    ("n" "NEXT" my/org-task-set-next)
    ("i" "WIP" my/org-task-set-wip)
    ("r" "REVIEW" my/org-task-set-review)
    ("w" "WAIT" my/org-task-set-wait)
    ("h" "HOLD" my/org-task-set-hold)
    ("d" "DONE" my/org-task-set-done)
    ("c" "CANCELLED" my/org-task-set-cancelled)
    ("k" "DROPPED" my/org-task-set-dropped)]
   ["Plan / Time"
    ("s" "schedule" org-schedule)
    ("D" "deadline" org-deadline)
    ("T" "timestamp" my/org-task-insert-active-timestamp)
    ("U" "inactive stamp" my/org-task-insert-inactive-timestamp)
    ("K" "calendar" calendar)
    ("I" "clock in" my/org-task-clock-in)
    ("O" "clock out" my/org-task-clock-out)
    ("G" "clock goto" org-clock-goto)
    ("e" "effort" org-set-effort)
    ("p" "priority" org-priority)
    ("P" "property" org-set-property)
    ("R" "refile" my/org-refile)
    ("A" "archive" org-archive-subtree)]
   ["Markers"
    ("?" "confusion" my/org-task-toggle-confusion)
    ("!" "shush" my/org-task-toggle-shush)
    ("b" "blocked" my/org-task-toggle-blocked)
    ("f" "followup" my/org-task-toggle-followup)
    ("v" "review tag" my/org-task-toggle-review-tag)
    ("q" "all tags" org-set-tags-command)]
   ["Views"
    ("o" "overview" my/org-task-agenda-overview)
    ("a" "agenda" my/org-agenda)
    ("C" "confusion" my/org-task-show-confusion)
    ("S" "shush" my/org-task-show-shush)
    ("B" "blocked" my/org-task-show-blocked)
    ("F" "followup" my/org-task-show-followup)
    ("V" "review" my/org-task-show-review)
    ("g" "docs" my/org-task-open-docs)]])

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
