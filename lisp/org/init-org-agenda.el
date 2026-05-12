;;; init-org-agenda.el --- Org agenda and task workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-org-core)
(require 'calendar)
(require 'org)
(require 'org-clock)
(require 'project nil t)
(require 'seq)
(require 'transient)
(require 'subr-x)

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

(defcustom my/org-agenda-fast-backend t
  "Whether interactive Org agenda entry points use the shell-backed fast view."
  :type 'boolean
  :group 'org)

(defvar my/org-agenda--last-scope nil
  "Most recent scoped agenda root and file count.")

(defvar my/org-agenda--native-call nil
  "Non-nil while intentionally calling the original Org agenda implementation.")

(defconst my/org-task--todo-keywords
  '("TODO" "NEXT" "WIP" "WAIT" "HOLD" "REVIEW" "DONE" "CANCELLED" "DROPPED")
  "TODO keywords used by the fast shell-backed task views.")

(defconst my/org-task--active-todo-keywords
  '("TODO" "NEXT" "WIP" "WAIT" "HOLD" "REVIEW")
  "Non-terminal TODO keywords used by the fast shell-backed overview.")

(defconst my/org-task-fast-view-buffer-name "*Org Fast Overview*"
  "Fallback buffer name for shell-backed Org task views.")

(defvar-local my/org-agenda--temporary-source-buffers nil
  "Org source buffers opened only to build the current agenda buffer.")

(defvar-local my/org-task-fast-view--entries nil
  "Vector of entries displayed in the current fast Org task view.")

(defvar-local my/org-task-fast-view--refresh-function nil
  "Function used to refresh the current fast Org task view.")

(defvar-local my/org-task-fast-view--window-configuration nil
  "Window configuration to restore when quitting the fast Org task view.")

(defvar-local my/org-agenda--temporary-source-buffer nil
  "Non-nil when this Org buffer was opened only as an agenda source.")

(defvar-local my/org-agenda--temporary-source-buffer-tick nil
  "Modification tick recorded after agenda generation for a temporary source.")

(defvar-local my/org-agenda--temporary-source-user-edited nil
  "Non-nil when a temporary agenda source buffer was edited after scanning.")

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

(defun my/org-agenda--external-globs ()
  "Return rg/fd glob arguments for scoped Org scans."
  (append
   '("--glob" "*.org")
   (mapcan (lambda (name)
             (list "--glob" (format "!**/%s/**" name)))
           my/org-agenda-scope-excluded-directory-names)))

(defun my/org-agenda--process-lines (program args &optional ok-statuses)
  "Run PROGRAM with ARGS and return stdout lines.
OK-STATUSES defaults to allowing only exit status 0."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (memq status (or ok-statuses '(0)))
        (error "%s failed with status %s: %s"
               program status (string-trim (buffer-string))))
      (split-string (buffer-string) "\n" t))))

(defun my/org-agenda--external-org-files-rg (dir)
  "Return Org files below DIR using ripgrep."
  (when (executable-find "rg")
    (let ((default-directory dir)
          (args (append '("--files" "--hidden")
                        (my/org-agenda--external-globs)
                        '("."))))
      (mapcar (lambda (file)
                (file-truename (expand-file-name file dir)))
              (my/org-agenda--process-lines "rg" args '(0 1))))))

(defun my/org-agenda--external-org-files-fd (dir)
  "Return Org files below DIR using fd."
  (when (executable-find "fd")
    (let ((args (append '("--hidden" "--type" "f"
                         "--extension" "org")
                       (mapcan (lambda (name)
                                 (list "--exclude" name))
                               my/org-agenda-scope-excluded-directory-names)
                       '("."))))
      (mapcar (lambda (file)
                (file-truename (expand-file-name file dir)))
              (let ((default-directory dir))
                (my/org-agenda--process-lines "fd" args '(0)))))))

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

(defun my/org-agenda--org-files-external (dir)
  "Return Org files below DIR using external tools when available."
  (condition-case nil
      (or (my/org-agenda--external-org-files-rg dir)
          (my/org-agenda--external-org-files-fd dir))
    (error nil)))

(defun my/org-agenda-scoped-files ()
  "Scan and return Org files for the current agenda scope.
This is intentionally called only by agenda/tag entry points, not by hooks or
idle timers."
  (let* ((root (my/org-agenda-scope-root))
         (files (sort (delete-dups
                       (or (my/org-agenda--org-files-external root)
                           (my/org-agenda--org-files-recursive root)))
                      #'string<)))
    (setq my/org-agenda--last-scope (list :root root :count (length files)))
    files))

(defun my/org-task-fast-view--rg-lines (root pattern)
  "Return ripgrep matches for PATTERN below ROOT without visiting files."
  (unless (executable-find "rg")
    (user-error "rg is required for fast Org task views"))
  (let ((default-directory root)
        (args (append '("--no-heading" "--line-number" "--color" "never"
                       "--hidden")
                      (my/org-agenda--external-globs)
                      (list "--" pattern "."))))
    (my/org-agenda--process-lines "rg" args '(0 1))))

(defun my/org-task-fast-view--parse-rg-line (root line)
  "Parse one ripgrep LINE relative to ROOT."
  (when (string-match "\\`\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)\\'" line)
    (list :file (file-truename (expand-file-name (match-string 1 line) root))
          :line (string-to-number (match-string 2 line))
          :text (string-trim (match-string 3 line)))))

(defun my/org-task-fast-view--parse-heading (entry)
  "Return ENTRY augmented with Org heading metadata."
  (let ((text (plist-get entry :text)))
    (when (string-match
           (concat "\\`\\(\\*+\\)[ \t]+"
                   "\\(?:" (regexp-opt my/org-task--todo-keywords t) "[ \t]+\\)?"
                   "\\(.*?\\)"
                   "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ \t]*\\'")
           text)
      (let ((todo (match-string 2 text))
            (title (string-trim (match-string 3 text)))
            (tags (match-string 4 text)))
        (append (list :level (length (match-string 1 text))
                      :todo todo
                      :title title
                      :tags (and tags (split-string (string-trim tags ":") ":" t)))
                entry)))))

(defun my/org-task-fast-view--heading-entries (root)
  "Return Org heading entries below ROOT using rg."
  (delq nil
        (mapcar (lambda (line)
                  (when-let* ((entry (my/org-task-fast-view--parse-rg-line root line)))
                    (my/org-task-fast-view--parse-heading entry)))
                (my/org-task-fast-view--rg-lines root "^\\*+ "))))

(defun my/org-task-fast-view--timestamp-entries (root)
  "Return dated schedule/deadline entries below ROOT using rg."
  (sort
   (delq nil
         (mapcar (lambda (line)
                   (when-let* ((entry (my/org-task-fast-view--parse-rg-line root line))
                               (text (plist-get entry :text)))
                     (when (and (not (string-match-p "\\`[ \t]*#\\+" text))
                                (or (string-match-p
                                     "\\b\\(DEADLINE\\|SCHEDULED\\):[ \t]*<[^>\n]+>"
                                     text)
                                    (string-match-p
                                     "\\`\\*+[ \t]+.*<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>\n]*>"
                                     text)))
                       (append (list :title text
                                     :date (my/org-task-fast-view--entry-date text))
                               entry))))
                 (my/org-task-fast-view--rg-lines
                  root "DEADLINE:|SCHEDULED:|^\\*+ .*<[0-9]{4}-[0-9]{2}-[0-9]{2}")))
   (lambda (a b)
     (string< (or (plist-get a :date) "9999-99-99")
              (or (plist-get b :date) "9999-99-99")))))

(defun my/org-task-fast-view--entry-date (text)
  "Return first ISO date in TEXT, or nil."
  (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" text)
    (match-string 1 text)))

(defun my/org-task-fast-view--entry-file-label (root entry)
  "Return a compact file label for ENTRY relative to ROOT."
  (file-relative-name (plist-get entry :file) root))

(defun my/org-task-fast-view-open-at-point ()
  "Open the Org file entry at point."
  (interactive)
  (let ((index (get-text-property (point) 'my/org-task-entry-index)))
    (unless index
      (user-error "No Org task entry at point"))
    (let* ((entry (aref my/org-task-fast-view--entries index))
           (file (plist-get entry :file))
           (line (plist-get entry :line)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun my/org-task-fast-view-refresh ()
  "Refresh the current fast Org task view."
  (interactive)
  (let ((refresh (bound-and-true-p my/org-task-fast-view--refresh-function)))
    (unless refresh
      (user-error "This fast Org view has no refresh function"))
    (funcall refresh)))

(defun my/org-task-fast-view-quit ()
  "Quit the fast Org task view and restore the previous window layout."
  (interactive)
  (let ((buffer (current-buffer))
        (configuration my/org-task-fast-view--window-configuration))
    (if (window-configuration-p configuration)
        (set-window-configuration configuration)
      (quit-window t))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun my/org-task-fast-view-next-line ()
  "Move to the next line in a fast Org task view."
  (interactive)
  (forward-line 1))

(defun my/org-task-fast-view-previous-line ()
  "Move to the previous line in a fast Org task view."
  (interactive)
  (forward-line -1))

(define-derived-mode my/org-task-fast-view-mode special-mode "Org Fast View"
  "Major mode for shell-backed Org task views."
  (setq-local truncate-lines t)
  (define-key my/org-task-fast-view-mode-map (kbd "RET")
              #'my/org-task-fast-view-open-at-point)
  (define-key my/org-task-fast-view-mode-map (kbd "o")
              #'my/org-task-fast-view-open-at-point)
  (define-key my/org-task-fast-view-mode-map (kbd "g")
              #'my/org-task-fast-view-refresh)
  (define-key my/org-task-fast-view-mode-map (kbd "q")
              #'my/org-task-fast-view-quit)
  (define-key my/org-task-fast-view-mode-map (kbd "Q")
              #'my/org-task-fast-view-quit)
  (define-key my/org-task-fast-view-mode-map (kbd "j")
              #'my/org-task-fast-view-next-line)
  (define-key my/org-task-fast-view-mode-map (kbd "k")
              #'my/org-task-fast-view-previous-line)
  (define-key my/org-task-fast-view-mode-map (kbd "h")
              #'beginning-of-line)
  (define-key my/org-task-fast-view-mode-map (kbd "l")
              #'my/org-task-fast-view-open-at-point)
  (define-key my/org-task-fast-view-mode-map (kbd "/")
              #'isearch-forward))

(with-eval-after-load 'evil
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'my/org-task-fast-view-mode 'emacs)))

(defun my/org-task-fast-view--todo-face (todo)
  "Return face for TODO keyword in fast views."
  (or (cdr (assoc todo org-todo-keyword-faces))
      'org-todo))

(defun my/org-task-fast-view--insert-section (root title entries &optional limit)
  "Insert TITLE and ENTRIES into the current fast task view buffer."
  (insert (propertize title 'face 'org-agenda-structure) "\n")
  (if entries
      (let ((count 0))
        (dolist (entry entries)
          (when (or (null limit) (< count limit))
            (let* ((index (length my/org-task-fast-view--entries))
                   (todo (plist-get entry :todo))
                   (tags (plist-get entry :tags))
                   (label (my/org-task-fast-view--entry-file-label root entry))
                   (line (plist-get entry :line))
                   (title-text (or (plist-get entry :title)
                                   (plist-get entry :text))))
              (setq my/org-task-fast-view--entries
                    (vconcat my/org-task-fast-view--entries (vector entry)))
              (let ((start (point)))
                (insert-text-button
                 (format "  %-8s %-54s %s:%d%s\n"
                         (or todo "")
                         (truncate-string-to-width title-text 54 nil nil t)
                         label
                         line
                         (if tags
                             (concat "  :" (string-join tags ":") ":")
                           ""))
                 'follow-link t
                 'my/org-task-entry-index index
                 'action (lambda (button)
                           (goto-char (button-start button))
                           (my/org-task-fast-view-open-at-point)))
                (when todo
                  (add-text-properties
                   start (+ start (length (format "  %-8s" todo)))
                   `(face ,(my/org-task-fast-view--todo-face todo))))))
            (setq count (1+ count)))))
    (insert "  No matches\n"))
  (insert "\n"))

(defun my/org-task-fast-view--render (title sections &optional refresh-function)
  "Render shell-backed Org task SECTIONS with TITLE."
  (let* ((root (my/org-agenda-scope-root))
         (buffer (get-buffer-create
                  (or (and (stringp title)
                           (format "*%s*" title))
                      my/org-task-fast-view-buffer-name)))
         (origin-configuration
          (unless (eq (current-buffer) buffer)
            (current-window-configuration))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (my/org-task-fast-view-mode)
        (setq-local my/org-task-fast-view--entries [])
        (setq-local my/org-task-fast-view--refresh-function refresh-function)
        (when origin-configuration
          (setq-local my/org-task-fast-view--window-configuration
                      origin-configuration))
        (setq my/org-agenda--last-scope nil)
        (insert (propertize title 'face '(:height 1.25 :weight bold)) "\n")
        (insert (propertize (format "Root: %s  Backend: rg  Keys: RET/l open, j/k move, / search, g refresh, q quit\n\n" root)
                            'face 'shadow))
        (dolist (section sections)
          (my/org-task-fast-view--insert-section
           root (plist-get section :title) (plist-get section :entries)
           (plist-get section :limit)))))
    (switch-to-buffer buffer)))

(defun my/org-task-fast-view--tagged (entries tag)
  "Return ENTRIES tagged with TAG."
  (seq-filter (lambda (entry)
                (member tag (plist-get entry :tags)))
              entries))

(defun my/org-task-fast-view--todo (entries todo)
  "Return ENTRIES whose TODO keyword is TODO."
  (seq-filter (lambda (entry)
                (string= (plist-get entry :todo) todo))
              entries))

(defun my/org-task-fast-view--tag-all (entries tags)
  "Return ENTRIES containing every tag in TAGS."
  (seq-filter (lambda (entry)
                (let ((entry-tags (plist-get entry :tags)))
                  (cl-every (lambda (tag) (member tag entry-tags)) tags)))
              entries))

(defun my/org-task-fast-overview ()
  "Open a shell-backed Org overview without visiting source files."
  (interactive)
  (let* ((root (my/org-agenda-scope-root))
         (headings (my/org-task-fast-view--heading-entries root))
         (active (seq-filter (lambda (entry)
                               (member (plist-get entry :todo)
                                       my/org-task--active-todo-keywords))
                             headings))
         (timestamps (my/org-task-fast-view--timestamp-entries root)))
    (my/org-task-fast-view--render
     "Org Fast Overview"
     (list
      (list :title "⚡ Upcoming Schedule & Deadlines" :entries timestamps :limit 80)
      (list :title "🎓 University Tasks" :entries (my/org-task-fast-view--tagged active "uni"))
      (list :title "🔬 Research & Gaps" :entries (my/org-task-fast-view--tag-all active '("math" "cs" "qc" "research")))
      (list :title "📌 Open TODO" :entries (my/org-task-fast-view--todo active "TODO"))
      (list :title "🚀 Next Actions" :entries (my/org-task-fast-view--todo active "NEXT"))
      (list :title "🔧 In Progress" :entries (my/org-task-fast-view--todo active "WIP"))
      (list :title "🔎 Review" :entries (my/org-task-fast-view--todo active "REVIEW"))
      (list :title "⏳ Waiting" :entries (my/org-task-fast-view--todo active "WAIT"))
      (list :title "⏸ On Hold" :entries (my/org-task-fast-view--todo active "HOLD"))
      (list :title "⛔ Blocked" :entries (my/org-task-fast-view--tagged active "blocked"))
      (list :title "❓ Confusion / Unclear" :entries (my/org-task-fast-view--tagged headings "confusion"))
      (list :title "💡 Remember This" :entries (my/org-task-fast-view--tagged headings "shush"))
      (list :title "📥 Unprocessed Inbox" :entries (my/org-task-fast-view--tagged headings "inbox")))
     #'my/org-task-fast-overview)))

(defun my/org-task-fast-agenda ()
  "Open a shell-backed dated Org agenda without visiting source files."
  (interactive)
  (let* ((root (my/org-agenda-scope-root))
         (timestamps (my/org-task-fast-view--timestamp-entries root))
         (headings (my/org-task-fast-view--heading-entries root))
         (active (seq-filter (lambda (entry)
                               (member (plist-get entry :todo)
                                       my/org-task--active-todo-keywords))
                             headings)))
    (my/org-task-fast-view--render
     "Org Fast Agenda"
     (list
      (list :title "⚡ Upcoming Schedule & Deadlines" :entries timestamps :limit 160)
      (list :title "🚀 Next Actions" :entries (my/org-task-fast-view--todo active "NEXT"))
      (list :title "🔧 In Progress" :entries (my/org-task-fast-view--todo active "WIP"))
      (list :title "📌 Open TODO" :entries (my/org-task-fast-view--todo active "TODO")))
     #'my/org-task-fast-agenda)))

(defun my/org-task-fast-status-table ()
  "Open a shell-backed Org TODO status table without visiting source files."
  (interactive)
  (let* ((root (my/org-agenda-scope-root))
         (headings (my/org-task-fast-view--heading-entries root)))
    (my/org-task-fast-view--render
     "Org Fast Status Table"
     (mapcar (lambda (todo)
               (list :title todo
                     :entries (seq-filter (lambda (entry)
                                            (string= (plist-get entry :todo) todo))
                                          headings)))
             my/org-task--todo-keywords))))

(defun my/org-task-fast-tags-view (tag)
  "Open a shell-backed Org tag view for TAG without visiting source files."
  (interactive
   (list (completing-read "Tag: " (mapcar #'car my/org-task-tag-alist) nil t)))
  (let* ((root (my/org-agenda-scope-root))
         (headings (my/org-task-fast-view--heading-entries root)))
    (my/org-task-fast-view--render
     (format "Org Fast Tag: %s" tag)
     (list (list :title tag
                 :entries (seq-filter (lambda (entry)
                                        (member tag (plist-get entry :tags)))
                                      headings))))))

(defun my/org-agenda--visible-buffers ()
  "Return buffers visible in live windows."
  (delq nil (mapcar #'window-buffer (window-list nil 'no-minibuf))))

(defun my/org-agenda--temporary-source-buffers (before files)
  "Return source buffers opened after BEFORE for agenda FILES."
  (let ((visible (my/org-agenda--visible-buffers))
        (file-table (make-hash-table :test #'equal))
        buffers)
    (dolist (file files)
      (puthash (file-truename file) t file-table))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (when (and (not (memq buffer before))
                   (not (memq buffer visible))
                   (gethash (file-truename file) file-table))
          (push buffer buffers))))
    buffers))

(defun my/org-agenda--cleanup-temporary-source-buffers ()
  "Kill temporary Org source buffers owned by the current agenda buffer."
  (let ((visible (my/org-agenda--visible-buffers)))
    (dolist (buffer my/org-agenda--temporary-source-buffers)
      (when (and (buffer-live-p buffer)
                 (not (memq buffer visible)))
        (with-current-buffer buffer
          (if my/org-agenda--temporary-source-user-edited
              (setq-local buffer-offer-save t)
            (setq-local buffer-offer-save nil)
            (set-buffer-modified-p nil)
            (kill-buffer buffer))))))
  (setq my/org-agenda--temporary-source-buffers nil))

(defun my/org-agenda--temporary-source-after-change (&rest _)
  "Mark a temporary agenda source buffer as user-edited."
  (when (and my/org-agenda--temporary-source-buffer
             my/org-agenda--temporary-source-buffer-tick
             (/= (buffer-chars-modified-tick)
                 my/org-agenda--temporary-source-buffer-tick))
    (setq-local my/org-agenda--temporary-source-user-edited t)
    (setq-local buffer-offer-save t)
    (remove-hook 'after-change-functions
                 #'my/org-agenda--temporary-source-after-change t)))

(defun my/org-agenda--mark-temporary-source-buffer (buffer)
  "Mark BUFFER as owned by the current agenda buffer."
  (with-current-buffer buffer
    (setq-local my/org-agenda--temporary-source-buffer t)
    (setq-local my/org-agenda--temporary-source-user-edited nil)
    (setq-local my/org-agenda--temporary-source-buffer-tick
                (buffer-chars-modified-tick))
    (setq-local buffer-offer-save nil)
    (add-hook 'after-change-functions
              #'my/org-agenda--temporary-source-after-change nil t)))

(defun my/org-agenda--remember-temporary-source-buffers (before files)
  "Attach temporary source buffers opened after BEFORE to the agenda buffer."
  (let ((buffers (my/org-agenda--temporary-source-buffers before files)))
    (dolist (buffer buffers)
      (my/org-agenda--mark-temporary-source-buffer buffer))
    (cond
     ((and buffers (derived-mode-p 'org-agenda-mode))
      (setq-local my/org-agenda--temporary-source-buffers
                  (delete-dups
                   (append my/org-agenda--temporary-source-buffers buffers)))
      (add-hook 'kill-buffer-hook
                #'my/org-agenda--cleanup-temporary-source-buffers nil t))
     (buffers
      (let ((my/org-agenda--temporary-source-buffers buffers))
        (my/org-agenda--cleanup-temporary-source-buffers))))))

(defun my/org-agenda--quit-around (orig-fn &rest args)
  "Clean temporary source buffers before running ORIG-FN with ARGS."
  (when (derived-mode-p 'org-agenda-mode)
    (my/org-agenda--cleanup-temporary-source-buffers))
  (apply orig-fn args))

(defmacro my/org-agenda-with-scoped-files (&rest body)
  "Run BODY with `org-agenda-files' bound to the current scoped scan."
  (declare (indent 0) (debug t))
  `(let* ((org-agenda-files (my/org-agenda-scoped-files))
          (before (buffer-list)))
     (prog1 (progn ,@body)
       (my/org-agenda--remember-temporary-source-buffers
        before org-agenda-files))))

(defun my/org-agenda--fast-supported-p (arg keys restriction)
  "Return non-nil when ARG KEYS RESTRICTION can use the fast backend."
  (and my/org-agenda-fast-backend
       (not my/org-agenda--native-call)
       (null arg)
       (null restriction)
       (or (null keys)
           (member keys '("o" "S")))))

(defun my/org-agenda-fast-dispatch (&optional arg keys restriction)
  "Open a shell-backed Org agenda view when supported.
Unsupported argument combinations fall back to native Org agenda."
  (if (my/org-agenda--fast-supported-p arg keys restriction)
      (cond
       ((equal keys "o") (my/org-task-fast-overview))
       ((equal keys "S") (my/org-task-fast-status-table))
       (t (my/org-task-fast-agenda)))
    (my/org-native-agenda arg keys restriction)))

(defun my/org-native-agenda (&optional arg keys restriction)
  "Open native `org-agenda' with scoped files.
This is the escape hatch when exact Org agenda semantics are needed."
  (interactive "P")
  (let ((my/org-agenda--native-call t))
    (my/org-agenda-with-scoped-files
      (org-agenda arg keys restriction))))

(defun my/org-agenda (&optional arg keys restriction)
  "Open the fast shell-backed Org agenda when possible."
  (interactive "P")
  (my/org-agenda-fast-dispatch arg keys restriction))

(defun my/org-agenda--around (orig-fn &optional arg keys restriction)
  "Route simple `org-agenda' calls through the fast backend."
  (if (my/org-agenda--fast-supported-p arg keys restriction)
      (my/org-agenda-fast-dispatch arg keys restriction)
    (apply orig-fn (list arg keys restriction))))

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
  "Open the personal Org agenda overview using the fast shell backend."
  (interactive)
  (my/org-task-fast-overview))

(defun my/org-task-status-table ()
  "Open the personal Org TODO status table using the fast shell backend."
  (interactive)
  (my/org-task-fast-status-table))

(defun my/org-task-tags-view (match)
  "Open an Org tags view for MATCH."
  (my/org-agenda-with-scoped-files
    (org-tags-view nil match)))

(defun my/org-task-show-confusion () (interactive) (my/org-task-fast-tags-view "confusion"))
(defun my/org-task-show-shush () (interactive) (my/org-task-fast-tags-view "shush"))
(defun my/org-task-show-blocked () (interactive) (my/org-task-fast-tags-view "blocked"))
(defun my/org-task-show-followup () (interactive) (my/org-task-fast-tags-view "followup"))
(defun my/org-task-show-review () (interactive) (my/org-task-fast-tags-view "review"))

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
               ((org-agenda-span 'week)
                (org-agenda-overriding-header "⚡ Upcoming Schedule & Deadlines")))
       (tags-todo "+uni"
                  ((org-agenda-overriding-header "🎓 University Tasks")))
       (tags-todo "+math+cs+qc+research"
                  ((org-agenda-overriding-header "🔬 Research & Gaps")))
       (todo "TODO"
             ((org-agenda-overriding-header "📌 Open TODO")))
       (todo "NEXT"
             ((org-agenda-overriding-header "🚀 Next Actions")))
       (todo "WIP"
             ((org-agenda-overriding-header "🔧 In Progress")))
       (todo "REVIEW"
             ((org-agenda-overriding-header "🔎 Review")))
       (todo "WAIT"
             ((org-agenda-overriding-header "⏳ Waiting")))
       (todo "HOLD"
             ((org-agenda-overriding-header "⏸ On Hold")))
       (tags-todo "blocked"
                  ((org-agenda-overriding-header "⛔ Blocked")))
       (tags "confusion"
             ((org-agenda-overriding-header "❓ Confusion / Unclear")))
       (tags "shush"
             ((org-agenda-overriding-header "💡 Remember This")))
       (tags "inbox"
             ((org-agenda-overriding-header "📥 Unprocessed Inbox")))))
     ("S" "Status Table"
      ((todo "TODO"
             ((org-agenda-overriding-header "📌 TODO")))
       (todo "NEXT"
             ((org-agenda-overriding-header "🚀 NEXT")))
       (todo "WIP"
             ((org-agenda-overriding-header "🔧 WIP")))
       (todo "REVIEW"
             ((org-agenda-overriding-header "🔎 REVIEW")))
       (todo "WAIT"
             ((org-agenda-overriding-header "⏳ WAIT")))
       (todo "HOLD"
             ((org-agenda-overriding-header "⏸ HOLD")))
       (todo "DONE"
             ((org-agenda-overriding-header "✓ DONE")))
       (todo "CANCELLED"
             ((org-agenda-overriding-header "× CANCELLED")))
       (todo "DROPPED"
             ((org-agenda-overriding-header "↓ DROPPED")))))))

  :config
  (define-key org-agenda-mode-map (kbd "j") #'org-agenda-next-line)
  (define-key org-agenda-mode-map (kbd "k") #'org-agenda-previous-line)
  (define-key org-agenda-mode-map (kbd "h") #'beginning-of-line)
  (define-key org-agenda-mode-map (kbd "l") #'org-agenda-goto)
  (unless (advice-member-p #'my/org-agenda--around 'org-agenda)
    (advice-add 'org-agenda :around #'my/org-agenda--around))
  (unless (advice-member-p #'my/org-agenda--quit-around 'org-agenda-quit)
    (advice-add 'org-agenda-quit :around #'my/org-agenda--quit-around)))

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
    ("x" "status table" my/org-task-status-table)
    ("a" "agenda" my/org-agenda)
    ("C" "confusion" my/org-task-show-confusion)
    ("S" "shush" my/org-task-show-shush)
    ("B" "blocked" my/org-task-show-blocked)
    ("F" "followup" my/org-task-show-followup)
    ("V" "review" my/org-task-show-review)
    ("g" "docs" my/org-task-open-docs)]])

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
