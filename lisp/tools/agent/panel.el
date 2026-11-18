;;; tools/agent/panel.el --- The collaboration panel: tasks, Pi, topology -*- lexical-binding: t; -*-

;;; Commentary:
;; `H-t' (C75) opens a side window on the right that stays until `H-t' or `q'
;; hides it.  Its three tabs are three buffers shown in that one window:
;;
;;   Tasks  the side comments (`@@scomment', C79) of the focused document, its
;;          folder, the folder and its subfolders, or the whole Project, as a
;;          task list; a comment is handed to an agent from here.
;;   Pi     the Project's own Pi, its chat embedded here: a project Pi is shown
;;          only in this panel, never among the other sessions (C78).
;;   Graph  the interactive graph of Pi, projects, documents and agents
;;          (`tools/agent/board', C78).
;;
;; The panel listens to state events only while it is shown and redraws once
;; per burst; closed, it costs nothing.  A wider scope finds its notes with
;; ripgrep when it is drawn; nothing is cached or watched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(require 'tools/agent/binding)
(require 'tools/agent/collab)

(declare-function +agent-board-buffer "tools/agent/board" ())
(declare-function +agent-board-refresh "tools/agent/board" (&optional keep-view))
(declare-function +agent-board-changed-h "tools/agent/board" (&rest _))
(declare-function +noema/open-file "noema/bridge/config" (file &optional origin-window))
(declare-function +noema/command "noema/bridge/config" (command &optional params))
(declare-function noema-pi-project-ensure "noema-pi-router" (directory))
(declare-function noema-pi-emacs-open "noema-pi-router" (&rest args))
(declare-function +agent-shell/here "tools/agent/config" (agent &optional file))

(defgroup +agent-panel nil
  "The collaboration side panel."
  :group '+agent-session)

(defcustom +agent-panel-width 0.34
  "Width of the collaboration panel, as a fraction of the frame or columns."
  :type 'number :group '+agent-panel)

(defcustom +agent-panel-skip-directories '(".git" "node_modules" ".agent" ".cell" "dist" "build" "var")
  "Directory names a folder-tree task scan does not enter."
  :type '(repeat string) :group '+agent-panel)

(defconst +agent-panel-buffer-name "*Collaboration*")

(defconst +agent-panel-scopes
  '((file . "this file") (dir . "this folder") (tree . "this folder and below") (project . "the project"))
  "Task scopes, narrowest first; the file is the default.")

(defface +agent-panel-tab '((t :inherit shadow))
  "An inactive tab of the collaboration panel." :group '+agent-panel)
(defface +agent-panel-tab-active '((t :inherit (bold font-lock-keyword-face) :underline t))
  "The active tab of the collaboration panel." :group '+agent-panel)
(defface +agent-panel-heading '((t :inherit (bold font-lock-function-name-face)))
  "A file heading in the task list." :group '+agent-panel)
(defface +agent-panel-open '((t :inherit warning))
  "The mark of an open task." :group '+agent-panel)
(defface +agent-panel-done '((t :inherit (shadow success)))
  "The mark of a resolved task." :group '+agent-panel)
(defface +agent-panel-quote '((t :inherit font-lock-string-face :slant italic))
  "The commented text of a task." :group '+agent-panel)
(defface +agent-panel-meta '((t :inherit shadow))
  "Authors, times and places in the task list." :group '+agent-panel)

(defvar +agent-panel--tab 'tasks "The shown tab: tasks, pi or graph.")
(defvar +agent-panel--document nil "The document the panel speaks about.")
(defvar-local +agent-panel--scope 'file "The task scope.")
(defvar-local +agent-panel--show-resolved nil "Whether resolved tasks are listed.")
(defvar-local +agent-panel--timer nil "Pending coalesced redraw.")

(defun +agent-panel--project-root (file)
  "Return the Project directory of FILE."
  (file-name-as-directory (or (+agent-acp-project-root (file-name-directory file))
                              (file-name-directory file))))

(defun +agent-panel--scope-files (root recursive)
  "Return the files under ROOT with comment tasks: notes holding a side
comment, and the files a task journal names.  Below ROOT only when RECURSIVE."
  (let* ((default-directory root)
         (depth (unless recursive '("--max-depth" "1")))
         (skip (mapcan (lambda (dir) (list "--glob" (concat "!" dir))) +agent-panel-skip-directories))
         (rg (executable-find "rg"))
         (notes (when rg
                  (ignore-errors
                    (apply #'process-lines rg "-l" "-F" "--glob" "*.md" "--glob" "*.markdown"
                           (append depth skip (list "@@scomment" "."))))))
         (journals (when rg
                     (ignore-errors
                       (apply #'process-lines rg "--files" "--hidden" "--glob" +agent-collab-tasks-file
                              (append depth skip (list ".")))))))
    (unless rg
      (setq notes (seq-filter (lambda (file) (with-temp-buffer (insert-file-contents file) (search-forward "@@scomment" nil t)))
                              (if recursive (directory-files-recursively root "\\.md\\'")
                                (directory-files root t "\\.md\\'")))))
    (delete-dups
     (append (mapcar (lambda (file) (expand-file-name file root)) notes)
             (mapcan (lambda (journal)
                       (let ((directory (file-name-directory (expand-file-name journal root))))
                         (with-temp-buffer
                           (insert-file-contents (expand-file-name journal root))
                           (let (files)
                             (dolist (line (split-string (buffer-string) "\n" t) files)
                               (when-let* ((event (ignore-errors (json-parse-string line :object-type 'alist)))
                                           ((alist-get 'text event))
                                           (note (alist-get 'note event)))
                                 (cl-pushnew (expand-file-name note directory) files :test #'equal)))))))
                     journals)))))

(defun +agent-panel-tasks (file scope)
  "Return the comment tasks of SCOPE around document FILE, as ((FILE . COMMENTS) ...)."
  (setq file (+agent-collab--client-file file))
  (let* ((directory (file-name-directory (expand-file-name file)))
         (files (pcase scope
                  ('file (list file))
                  ('dir (cons file (+agent-panel--scope-files directory nil)))
                  ('tree (cons file (+agent-panel--scope-files directory t)))
                  ('project (cons file (+agent-panel--scope-files (+agent-panel--project-root file) t))))))
    (sort (delq nil (mapcar (lambda (each) (when-let* ((comments (+agent-collab-comments each))) (cons each comments)))
                            (delete-dups (mapcar #'expand-file-name files))))
          (lambda (a b) (string< (car a) (car b))))))

(defconst +agent-panel--tabs '((tasks "1" "Tasks") (pi "2" "Pi") (graph "3" "Graph"))
  "The tabs: symbol, key and title.")

(defun +agent-panel--tab-map (tab)
  "Return the header keymap that shows TAB on a click."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] (lambda () (interactive) (+agent-panel--show-tab tab)))
    map))

(defun +agent-panel--status ()
  "Return the shown buffer's status for the tab line."
  (cond ((derived-mode-p '+agent-board-mode) (+agent-board--status))
        ((derived-mode-p 'agent-shell-mode)
         (format "%s · %s" (or (bound-and-true-p +agent-acp-session-name) "Pi")
                 (if (+agent-acp-busy-p (current-buffer)) "working" "idle")))))

(declare-function +agent-board--status "tools/agent/board" ())

(defun +agent-panel-header (&optional status)
  "Return the panel's tab line: the tabs, then STATUS."
  (concat " "
          (mapconcat (pcase-lambda (`(,tab ,key ,title))
                       (propertize (format " %s %s " key title)
                                   'face (if (eq tab +agent-panel--tab) '+agent-panel-tab-active '+agent-panel-tab)
                                   'mouse-face 'highlight 'keymap (+agent-panel--tab-map tab)
                                   'help-echo (format "%s (C-c %s)" title key)))
                     +agent-panel--tabs "  ")
          (if status (propertize (concat "   " status) 'face '+agent-panel-meta) "")))

(defun +agent-panel--age (at)
  "Return how long ago AT was, briefly."
  (let ((time (ignore-errors (date-to-time at))))
    (if (not time) ""
      (let ((minutes (/ (float-time (time-subtract nil time)) 60)))
        (cond ((< minutes 1) "now")
              ((< minutes 60) (format "%dm" minutes))
              ((< minutes 1440) (format "%dh" (/ minutes 60)))
              (t (format "%dd" (/ minutes 1440))))))))

(defun +agent-panel--insert-task (comment)
  "Insert COMMENT as one task."
  (let* ((start (point))
         (done (alist-get 'done comment))
         (width (max 20 (- (window-width (get-buffer-window (current-buffer))) 8)))
         (assignee (alist-get 'assignee comment)))
    (insert "  " (propertize (if done "✓" "●") 'face (if done '+agent-panel-done '+agent-panel-open)) " "
            (propertize (truncate-string-to-width (string-trim (or (alist-get 'text comment) "")) width nil nil "…")
                        'face (if done '+agent-panel-done 'default))
            "\n")
    (unless (string-empty-p (or (alist-get 'quote comment) ""))
      (insert "    " (propertize (truncate-string-to-width (format "after «%s»" (alist-get 'quote comment)) width nil nil "…")
                                 'face '+agent-panel-quote)
              "\n"))
    (insert "    "
            (propertize (concat (if assignee (format "→ %s" (car assignee)) "not handed over")
                                (when-let* ((line (alist-get 'line comment))) (format " · line %d" line)))
                        'face '+agent-panel-meta)
            "\n")
    (put-text-property start (point) '+agent-panel-task (cons (alist-get 'file comment) (alist-get 'id comment)))
    (put-text-property start (point) '+agent-panel-comment comment)))

(defun +agent-panel--draw-tasks ()
  "Draw the task tab."
  ;; A Noema page names its document `/fs:local:...'; tasks name client paths.
  (let* ((file (and +agent-panel--document (+agent-collab--client-file +agent-panel--document))))
    (if (not file)
        (insert "\n  Open a document (a note or a file) to see its tasks.\n")
      (let* ((groups (+agent-panel-tasks file +agent-panel--scope))
             (open 0) (resolved 0))
        (dolist (group groups)
          (dolist (comment (cdr group))
            (if (alist-get 'done comment) (cl-incf resolved) (cl-incf open))))
        (insert (propertize (format "\n  %s · %d open" (alist-get +agent-panel--scope +agent-panel-scopes) open)
                            'face 'bold)
                (propertize (format "%s   s scope  R done\n"
                                    (if (> resolved 0) (format ", %d done%s" resolved
                                                               (if +agent-panel--show-resolved "" " (hidden)"))
                                      ""))
                            'face '+agent-panel-meta))
        (if (zerop (+ open (if +agent-panel--show-resolved resolved 0)))
            (insert "\n  No tasks.  Select text in a note and comment (⌘⌥M), or C-c A d.\n")
          (dolist (group groups)
            (let ((shown (seq-filter (lambda (comment) (or +agent-panel--show-resolved (not (alist-get 'done comment))))
                                     (cdr group))))
              (when shown
                (insert "\n " (propertize (file-relative-name (car group) (file-name-directory file))
                                          'face '+agent-panel-heading)
                        "\n")
                (mapc #'+agent-panel--insert-task shown)))))
        (insert (propertize "\n  RET open  a hand to an agent  c done/reopen  o chat  g refresh\n"
                            'face '+agent-panel-meta))))))

;;;; The panel window and its tabs

(defun +agent-panel--window ()
  "Return the panel's side window, or nil when it is hidden."
  (seq-find (lambda (window) (window-parameter window '+agent-panel))
            (window-list-1 nil 'nomini t)))

(defun +agent-panel--visible-p ()
  "Whether the panel is on screen."
  (+agent-panel--window))

(defun +agent-panel--tasks-buffer ()
  "Return the task list buffer."
  (let ((buffer (get-buffer-create +agent-panel-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-panel-mode) (+agent-panel-mode)))
    buffer))

(defun +agent-panel--open-window (buffer)
  "Show the panel's side window with BUFFER and return it."
  (or (when-let* ((window (+agent-panel--window)))
        (set-window-dedicated-p window nil)
        (set-window-buffer window buffer)
        window)
      (display-buffer-in-side-window
       buffer `((side . right) (slot . 0) (window-width . ,+agent-panel-width)
                (preserve-size . (t . nil))
                ;; The tabs belong to the window, so no shown buffer (an
                ;; agent-shell rewrites its own header line) can take them.
                (window-parameters . ((no-delete-other-windows . t) (+agent-panel . t)
                                      (tab-line-format . (:eval (+agent-panel-header (+agent-panel--status))))))))))

(defun +agent-panel--put (buffer)
  "Show BUFFER in the panel, open it when hidden, and select it."
  (let ((window (+agent-panel--open-window buffer)))
    (set-window-dedicated-p window t)
    (+agent-panel--listen t)
    (select-window window)
    window))

(defun +agent-panel--pi-directory ()
  "Return the directory whose project Pi the panel shows."
  (if +agent-panel--document (file-name-directory (+agent-collab--client-file +agent-panel--document))
    default-directory))

(defun +agent-panel--show-tab (tab)
  "Show TAB in the panel."
  (setq +agent-panel--tab tab)
  (pcase tab
    ('tasks (+agent-panel--put (+agent-panel--tasks-buffer)) (+agent-panel-redraw))
    ('pi (require 'noema-pi-router)
         ;; The display function routes the project Pi into this panel (C78).
         (let ((buffer (noema-pi-project-ensure (+agent-panel--pi-directory))))
           (+agent-panel-display-pi buffer)))
    ('graph (require 'tools/agent/board)
            (+agent-panel--put (+agent-board-buffer))
            (+agent-board-refresh t)))
  (force-mode-line-update t))

(defun +agent-panel-redraw ()
  "Redraw the task list."
  (interactive)
  (when-let* ((buffer (get-buffer +agent-panel-buffer-name)))
    (with-current-buffer buffer
      (when (timerp +agent-panel--timer) (cancel-timer +agent-panel--timer))
      (setq +agent-panel--timer nil)
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (condition-case failure
            (+agent-panel--draw-tasks)
          ;; Never a silently half-drawn list: say what failed.
          (error (insert (propertize (format "\n  The task list failed: %s\n" (error-message-string failure))
                                     'face 'error))))
        (goto-char (point-min))
        (forward-line (1- line))))))

;;;; The project Pi, embedded (C78)

(defvar-keymap +agent-panel-pi-mode-map
  "C-c 1" #'+agent-panel-tasks-tab
  "C-c 2" #'+agent-panel-pi-tab
  "C-c 3" #'+agent-panel-graph-tab)

(define-minor-mode +agent-panel-pi-mode
  "A project Pi shown in the collaboration panel."
  :lighter nil)

(defvar +portal-popup-buffers)
(declare-function +portal-popup-agent-keys-mode "term/portal/popup" (&optional arg))

(defun +agent-panel--adopt-pi (buffer)
  "Make BUFFER the panel's Pi: shown only by the panel, never by the popup."
  (with-current-buffer buffer
    (setq-local +agent-acp-display-buffer-function #'+agent-panel-display-pi)
    (when (bound-and-true-p +portal-popup-instance-p)
      (setq-local +portal-popup-instance-p nil)
      (when (bound-and-true-p +portal-popup-agent-keys-mode) (+portal-popup-agent-keys-mode -1))
      (when (boundp '+portal-popup-buffers)
        (setq +portal-popup-buffers (delq buffer +portal-popup-buffers))))
    (unless +agent-panel-pi-mode (+agent-panel-pi-mode 1))))

;;;###autoload
(defun +agent-panel-display-pi (buffer)
  "Show the project Pi BUFFER in the collaboration panel's Pi tab (C78).
This is a project Pi's `+agent-acp-display-buffer-function': it lives in the
panel and never in the ordinary agent views."
  (+agent-panel--adopt-pi buffer)
  (setq +agent-panel--tab 'pi)
  (+agent-panel--put buffer)
  (with-current-buffer buffer (goto-char (point-max)))
  (force-mode-line-update t)
  buffer)

;;;; Following state, only while shown

(defun +agent-panel--changed-h (&rest _)
  "Redraw the shown tab once after a burst of events."
  (when (+agent-panel--visible-p)
    (pcase +agent-panel--tab
      ('tasks (when-let* ((buffer (get-buffer +agent-panel-buffer-name)))
                (with-current-buffer buffer
                  (unless +agent-panel--timer
                    (setq +agent-panel--timer (run-with-idle-timer 0.4 nil #'+agent-panel-redraw))))))
      ('graph (+agent-board-changed-h)))))

(defun +agent-panel--comments-changed-h (&rest _)
  "A comment or task changed: redraw the shown tab."
  (+agent-panel--changed-h))

(defun +agent-panel--listen (on)
  "Start following state events when ON, stop otherwise."
  (dolist (spec '((+agent-collab-changed-functions . +agent-panel--comments-changed-h)
                  (+agent-binding-changed-functions . +agent-panel--changed-h)
                  (+agent-acp-changed-functions . +agent-panel--changed-h)
                  (window-buffer-change-functions . +agent-panel--window-change-h)))
    (if on (add-hook (car spec) (cdr spec)) (remove-hook (car spec) (cdr spec)))))

(defun +agent-panel--teardown-h ()
  "Stop following state when the task list goes."
  (when (timerp +agent-panel--timer) (cancel-timer +agent-panel--timer))
  (+agent-panel--listen nil))

(defun +agent-panel--window-change-h (_frame)
  "Follow the focused document, and stop listening while the panel is hidden."
  (if (not (+agent-panel--visible-p))
      (+agent-panel--listen nil)
    (when-let* ((document +agent-binding-last-document)
                ((not (equal document +agent-panel--document))))
      (setq +agent-panel--document document)
      (when (eq +agent-panel--tab 'tasks) (+agent-panel--changed-h)))))

;;;; Task commands

(defun +agent-panel--at (property)
  "Return PROPERTY at point or on the nearest item line above."
  (or (get-text-property (point) property)
      (save-excursion
        (let ((found nil))
          (while (and (not found) (zerop (forward-line -1)))
            (setq found (get-text-property (point) property)))
          found))
      (user-error "Nothing here")))

(defun +agent-panel--task ()
  "Return the comment task at point, read again from its file."
  (let* ((stale (+agent-panel--at '+agent-panel-comment))
         (fresh (and (not (string-empty-p (or (alist-get 'id stale) "")))
                     (+agent-collab-comment (alist-get 'file stale) (alist-get 'id stale)))))
    (or fresh
        (seq-find (lambda (comment) (equal (alist-get 'source comment) (alist-get 'source stale)))
                  (+agent-collab-comments (alist-get 'file stale)))
        (user-error "The comment is gone"))))

(defun +agent-panel--editing-window ()
  "Return a window to open documents in: not the panel."
  (or (get-mru-window nil nil t)
      (split-window (selected-window) nil 'left)))

(defun +agent-panel-visit ()
  "Open the task at point at its place."
  (interactive)
  (let* ((comment (+agent-panel--task))
         (file (alist-get 'file comment))
         (line (alist-get 'line comment)))
    (select-window (+agent-panel--editing-window))
    (if (and (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file) (fboundp '+noema/open-file))
        (progn (+noema/open-file file)
               (when (and line (fboundp '+noema/command))
                 (run-at-time 0.3 nil #'+agent-panel--goto-page-line line)))
      (find-file file)
      (when line (goto-char (point-min)) (forward-line (1- line))))))

(defun +agent-panel--goto-page-line (line)
  "Move the selected Noema page to LINE."
  (ignore-errors (+noema/command "goto-line" `((value . ,line)))))

(defun +agent-panel-chat ()
  "Open the agent of the task at point."
  (interactive)
  (let ((session (+agent-collab--comment-session (+agent-panel--task))))
    (select-window (+agent-panel--editing-window))
    (+agent-acp-show-buffer session)))

(defun +agent-panel--choose-agent (file)
  "Read which session takes a task on FILE: its collaborators first."
  (let* ((collaborators (mapcar (lambda (s) (plist-get s :buffer)) (+agent-binding-sessions file)))
         (others (seq-remove (lambda (b) (memq b collaborators))
                             (seq-filter #'buffer-live-p
                                         (mapcar (lambda (s) (plist-get s :buffer)) (+agent-pool-sessions)))))
         (choices (append (mapcar (lambda (b) (cons (format "◆ %s" (buffer-local-value '+agent-acp-session-name b)) b))
                                  collaborators)
                          (mapcar (lambda (b) (cons (format "  %s" (buffer-local-value '+agent-acp-session-name b)) b))
                                  others)
                          (mapcar (lambda (agent) (cons (format "＋ New %s for %s" (car agent) (file-name-nondirectory file))
                                                        (car agent)))
                                  (bound-and-true-p +agent-shell-agents))))
         (choice (cdr (assoc (completing-read "Hand to: " choices nil t) choices))))
    (if (bufferp choice) choice
      (save-window-excursion
        (with-current-buffer (find-file-noselect file)
          (+agent-shell/here choice (+agent-binding-canonical file)))))))

(defun +agent-panel-dispatch ()
  "Hand the comment at point to an agent; it changes the file through the API."
  (interactive)
  (let ((comment (+agent-panel--task)))
    (+agent-collab-dispatch comment (+agent-panel--choose-agent (alist-get 'file comment)))
    (+agent-panel-redraw)))

(defun +agent-panel-resolve ()
  "Mark the comment at point done, or open it again."
  (interactive)
  (let ((comment (+agent-panel--task)))
    (+agent-collab-set-done comment (not (alist-get 'done comment)))
    (+agent-panel-redraw)))

(defun +agent-panel-cycle-scope ()
  "Widen the task scope: file, folder, folder and below, project, file."
  (interactive)
  (let* ((scopes (mapcar #'car +agent-panel-scopes))
         (next (or (cadr (memq +agent-panel--scope scopes)) (car scopes))))
    (setq +agent-panel--scope next)
    (+agent-panel-redraw)
    (message "Tasks of %s" (alist-get next +agent-panel-scopes))))

(defun +agent-panel-toggle-resolved ()
  "Show or hide resolved tasks."
  (interactive)
  (setq +agent-panel--show-resolved (not +agent-panel--show-resolved))
  (+agent-panel-redraw))

(defun +agent-panel-refresh ()
  "Read every comment again."
  (interactive)
  (+agent-panel-redraw))

(defun +agent-panel-show-tasks (file scope)
  "Show the tasks of SCOPE around FILE in the panel."
  (setq +agent-panel--document file)
  (with-current-buffer (+agent-panel--tasks-buffer) (setq +agent-panel--scope scope))
  (+agent-panel--show-tab 'tasks))

(defun +agent-panel-open-global-pi ()
  "Open the global Pi, outside the panel."
  (interactive)
  (require 'noema-pi-router)
  (select-window (+agent-panel--editing-window))
  (call-interactively #'noema-pi-emacs-open))

(defun +agent-panel-tasks-tab () "Show the tasks." (interactive) (+agent-panel--show-tab 'tasks))
(defun +agent-panel-pi-tab () "Show the project's Pi." (interactive) (+agent-panel--show-tab 'pi))
(defun +agent-panel-graph-tab () "Show the graph." (interactive) (+agent-panel--show-tab 'graph))

(defun +agent-panel-next ()
  "Move to the next task."
  (interactive)
  (let* ((here (get-text-property (point) '+agent-panel-task))
         (next (next-single-property-change (point) '+agent-panel-task)))
    (while (and next (equal (get-text-property next '+agent-panel-task) here))
      (setq next (next-single-property-change next '+agent-panel-task)))
    (when-let* ((target (and next (if (get-text-property next '+agent-panel-task) next
                                     (next-single-property-change next '+agent-panel-task)))))
      (goto-char target))))

(defun +agent-panel-previous ()
  "Move to the previous task."
  (interactive)
  (let ((start (previous-single-property-change (point) '+agent-panel-task)))
    (when (and start (get-text-property (point) '+agent-panel-task))
      (setq start (previous-single-property-change start '+agent-panel-task)))
    (when start
      (goto-char (if (get-text-property start '+agent-panel-task) start
                   (or (previous-single-property-change start '+agent-panel-task) (point-min)))))))

(defvar-keymap +agent-panel-mode-map
  :parent special-mode-map
  "1" #'+agent-panel-tasks-tab
  "2" #'+agent-panel-pi-tab
  "3" #'+agent-panel-graph-tab
  "C-c 1" #'+agent-panel-tasks-tab
  "C-c 2" #'+agent-panel-pi-tab
  "C-c 3" #'+agent-panel-graph-tab
  "RET" #'+agent-panel-visit
  "S-<return>" #'+agent-panel-chat
  "n" #'+agent-panel-next
  "p" #'+agent-panel-previous
  "j" #'+agent-panel-next
  "k" #'+agent-panel-previous
  "a" #'+agent-panel-dispatch
  "o" #'+agent-panel-chat
  "c" #'+agent-panel-resolve
  "s" #'+agent-panel-cycle-scope
  "R" #'+agent-panel-toggle-resolved
  "E" #'+agent-panel-open-global-pi
  "g" #'+agent-panel-refresh
  "q" #'+agent/panel)

(define-derived-mode +agent-panel-mode special-mode "Collaboration"
  "The collaboration panel's task list (C75)."
  (setq truncate-lines t)
  (setq-local cursor-type 'bar)
  (add-hook 'kill-buffer-hook #'+agent-panel--teardown-h nil t))

(when (fboundp 'set-evil-initial-state!) (set-evil-initial-state! '+agent-panel-mode 'emacs))

(with-eval-after-load 'tools/agent/board
  (define-key +agent-board-mode-map (kbd "C-c 1") #'+agent-panel-tasks-tab)
  (define-key +agent-board-mode-map (kbd "C-c 2") #'+agent-panel-pi-tab)
  (define-key +agent-board-mode-map (kbd "C-c 3") #'+agent-panel-graph-tab)
  (define-key +agent-board-mode-map (kbd "1") #'+agent-panel-tasks-tab)
  (define-key +agent-board-mode-map (kbd "2") #'+agent-panel-pi-tab)
  (define-key +agent-board-mode-map (kbd "3") #'+agent-panel-graph-tab)
  (define-key +agent-board-mode-map (kbd "q") #'+agent/panel))

(defvar +agent-board-mode-map)

;;;###autoload
(defun +agent/panel ()
  "Show or hide the collaboration panel on the right (C75)."
  (interactive)
  (if-let* ((window (+agent-panel--window)))
      (progn (delete-window window) (+agent-panel--listen nil))
    (setq +agent-panel--document (or (+agent-document-file) +agent-binding-last-document
                                     +agent-panel--document))
    (+agent-panel--show-tab (if (eq +agent-panel--tab 'pi) 'pi +agent-panel--tab))))

(provide 'tools/agent/panel)
;;; panel.el ends here
