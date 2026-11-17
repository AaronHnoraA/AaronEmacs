;;; tools/agent/panel.el --- The collaboration panel: tasks, Pi, topology -*- lexical-binding: t; -*-

;;; Commentary:
;; `H-t' (C75) opens a side window on the right that stays until `H-t' or `q'
;; hides it.  Three tabs:
;;
;;   Tasks  the comments of the focused document, its folder, the folder and
;;          its subfolders, or the whole Project, as a task list.  New comments
;;          land here; a task is handed to an agent from here (C73).
;;   Pi     the Project's Pi, the project-level agent.
;;   Graph  Pi, projects, documents and their agents (the C69 topology).
;;
;; The panel listens to state events only while it is shown and redraws once
;; per burst; closed, it costs nothing.  The task list reads the folders'
;; comment journals (C70); a folder tree's journals are found once and kept
;; until a comment appears in a folder not yet known.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(require 'tools/agent/binding)
(require 'tools/agent/collab)

(declare-function +agent-topology-model "tools/agent/topology" (sessions counts))
(declare-function +agent-topology--sessions "tools/agent/topology" ())
(declare-function +agent-topology--layout "tools/agent/topology" (model))
(declare-function +agent-topology--render "tools/agent/topology" ())
(declare-function +agent-topology-file-counts "tools/agent/topology" (file))
(declare-function +agent-topology-visit "tools/agent/topology" ())
(declare-function +agent-topology-chat "tools/agent/topology" ())
(declare-function +agent-topology-next "tools/agent/topology" ())
(declare-function +agent-topology-previous "tools/agent/topology" ())
(declare-function +noema/open-file "noema/bridge/config" (file &optional origin-window))
(declare-function +noema/command "noema/bridge/config" (command &optional params))
(declare-function noema-pi-open "noema-pi-router" (&rest args))
(declare-function noema-pi-emacs-open "noema-pi-router" (&rest args))
(defvar +agent-topology--model)
(defvar +agent-topology--layout)
(defvar +agent-topology--selected)

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

(defvar-local +agent-panel--tab 'tasks "The shown tab: tasks, pi or graph.")
(defvar-local +agent-panel--scope 'file "The task scope.")
(defvar-local +agent-panel--show-resolved nil "Whether resolved tasks are listed.")
(defvar-local +agent-panel--document nil "The document the panel speaks about.")
(defvar-local +agent-panel--timer nil "Pending coalesced redraw.")

(defvar +agent-panel--journal-cache (make-hash-table :test #'equal)
  "Folder tree -> the comment journals found under it.")

;;;; Collecting tasks

(defun +agent-panel--project-root (file)
  "Return the Project directory of FILE."
  (file-name-as-directory (or (+agent-acp-project-root (file-name-directory file))
                              (file-name-directory file))))

(defun +agent-panel--journals-under (root &optional rescan)
  "Return the comment journals under ROOT, from the cache unless RESCAN."
  (or (and (not rescan) (gethash root +agent-panel--journal-cache))
      (puthash root
               (when (file-directory-p root)
                 (directory-files-recursively
                  root (concat "\\`" (regexp-quote +agent-collab-comments-file) "\\'") nil
                  (lambda (directory)
                    (not (member (file-name-nondirectory directory) +agent-panel-skip-directories)))))
               +agent-panel--journal-cache)))

(defun +agent-panel--folder-threads (directory)
  "Return every thread in DIRECTORY's journal, each with its file."
  (let (threads)
    (maphash (lambda (_id thread)
               (let ((file (expand-file-name (alist-get 'note thread) directory)))
                 (push (+agent-collab--with-file thread file) threads)))
             (+agent-collab--fold (expand-file-name "x" directory)))
    threads))

(defun +agent-panel-tasks (file scope)
  "Return the tasks of SCOPE around document FILE, grouped as ((FILE . THREADS) ...)."
  (let* ((directory (file-name-directory (expand-file-name file)))
         (threads
          (pcase scope
            ('file (+agent-collab-threads file))
            ('dir (+agent-panel--folder-threads directory))
            ((or 'tree 'project)
             (let ((root (if (eq scope 'tree) directory (+agent-panel--project-root file))))
               (mapcan (lambda (journal) (+agent-panel--folder-threads (file-name-directory journal)))
                       (+agent-panel--journals-under root)))))))
    (let (groups)
      (dolist (thread (sort threads (lambda (a b) (string< (or (alist-get 'createdAt a) "")
                                                            (or (alist-get 'createdAt b) "")))))
        (let ((cell (assoc (alist-get 'file thread) groups)))
          (if cell (setcdr cell (append (cdr cell) (list thread)))
            (push (cons (alist-get 'file thread) (list thread)) groups))))
      (sort groups (lambda (a b) (string< (car a) (car b)))))))

;;;; Drawing

(defun +agent-panel--tabs ()
  "Return the header line with the tabs."
  (concat " "
          (mapconcat (lambda (tab)
                       (propertize (format " %s %s " (car tab) (cdr tab))
                                   'face (if (eq +agent-panel--tab (cadr (assoc (car tab) '(("1" tasks) ("2" pi) ("3" graph)))))
                                             '+agent-panel-tab-active '+agent-panel-tab)))
                     '(("1" . "Tasks") ("2" . "Pi") ("3" . "Graph"))
                     "  ")))

(defun +agent-panel--age (at)
  "Return how long ago AT was, briefly."
  (let ((time (ignore-errors (date-to-time at))))
    (if (not time) ""
      (let ((minutes (/ (float-time (time-subtract nil time)) 60)))
        (cond ((< minutes 1) "now")
              ((< minutes 60) (format "%dm" minutes))
              ((< minutes 1440) (format "%dh" (/ minutes 60)))
              (t (format "%dd" (/ minutes 1440))))))))

(defun +agent-panel--insert-task (thread)
  "Insert THREAD as one task."
  (let* ((start (point))
         (resolved (equal (alist-get 'status thread) "resolved"))
         (messages (alist-get 'messages thread))
         (first (car messages))
         (last (car (last messages)))
         (width (max 20 (- (window-width (get-buffer-window (current-buffer))) 8))))
    (insert "  " (propertize (if resolved "✓" "●") 'face (if resolved '+agent-panel-done '+agent-panel-open)) " "
            (propertize (truncate-string-to-width (string-trim (or (alist-get 'text first) "")) width nil nil "…")
                        'face (if resolved '+agent-panel-done 'default))
            "\n")
    (unless (string-empty-p (or (alist-get 'quote thread) ""))
      (insert "    " (propertize (truncate-string-to-width (format "«%s»" (alist-get 'quote thread)) width nil nil "…")
                                 'face '+agent-panel-quote)
              "\n"))
    (insert "    "
            (propertize (concat (alist-get 'name (alist-get 'author first))
                                (when-let* ((assignee (alist-get 'assignee thread)))
                                  (concat " → " (alist-get 'name assignee)))
                                (when (> (length messages) 1)
                                  (format " · %d repl%s, last %s" (1- (length messages))
                                          (if (= (length messages) 2) "y" "ies")
                                          (alist-get 'name (alist-get 'author last))))
                                " · " (+agent-panel--age (alist-get 'at last))
                                (when-let* ((line (+agent-collab--thread-line thread))) (format " · line %d" line)))
                        'face '+agent-panel-meta)
            "\n")
    (put-text-property start (point) '+agent-panel-task (cons (alist-get 'file thread) (alist-get 'id thread)))))

(defun +agent-panel--draw-tasks ()
  "Draw the task tab."
  (let* ((file +agent-panel--document))
    (if (not file)
        (insert "\n  Open a document (a note or a file) to see its tasks.\n")
      (let* ((groups (+agent-panel-tasks file +agent-panel--scope))
             (open 0) (resolved 0))
        (dolist (group groups)
          (dolist (thread (cdr group))
            (if (equal (alist-get 'status thread) "resolved") (cl-incf resolved) (cl-incf open))))
        (insert (propertize (format "\n  %s · %d open" (alist-get +agent-panel--scope +agent-panel-scopes) open)
                            'face 'bold)
                (propertize (format "%s   s scope  R resolved\n"
                                    (if (> resolved 0) (format ", %d resolved%s" resolved
                                                               (if +agent-panel--show-resolved "" " (hidden)"))
                                      ""))
                            'face '+agent-panel-meta))
        (if (zerop (+ open (if +agent-panel--show-resolved resolved 0)))
            (insert "\n  No tasks.  Select text in a note and comment (⌘⌥M), or C-c A d.\n")
          (dolist (group groups)
            (let ((shown (seq-filter (lambda (thread) (or +agent-panel--show-resolved
                                                          (not (equal (alist-get 'status thread) "resolved"))))
                                     (cdr group))))
              (when shown
                (insert "\n " (propertize (file-relative-name (car group) (file-name-directory file))
                                          'face '+agent-panel-heading)
                        "\n")
                (mapc #'+agent-panel--insert-task shown)))))
        (insert (propertize "\n  RET open  a hand to an agent  r reply  c resolve  g refresh\n"
                            'face '+agent-panel-meta))))))

(defun +agent-panel--pi-sessions ()
  "Return the Pi sessions of the document's project and the global one."
  (seq-filter (lambda (session) (eq (+agent-session-kind (plist-get session :buffer)) 'pi))
              (+agent-pool-sessions nil nil 'all)))

(defun +agent-panel--draw-pi ()
  "Draw the Pi tab."
  (insert (propertize "\n  Pi — the project's agent\n" 'face 'bold))
  (let ((sessions (+agent-panel--pi-sessions)))
    (if (not sessions)
        (insert "\n  No Pi is running.\n")
      (dolist (session sessions)
        (let ((start (point)))
          (insert (format "\n  %s  %s\n" (or (plist-get session :name) "pi")
                          (propertize (format "%s · %s" (or (plist-get session :root) "")
                                              (symbol-name (or (plist-get session :turn-state) 'idle)))
                                      'face '+agent-panel-meta)))
          (put-text-property start (point) '+agent-panel-pi (plist-get session :buffer))))))
  (insert (propertize "\n  RET open the project's Pi   E the global Pi\n" 'face '+agent-panel-meta)))

(defun +agent-panel--draw-graph ()
  "Draw the topology tab."
  (require 'tools/agent/topology)
  (setq +agent-topology--model (+agent-topology-model (+agent-topology--sessions) #'+agent-topology-file-counts)
        +agent-topology--layout (+agent-topology--layout +agent-topology--model))
  (unless (seq-find (lambda (node) (equal (plist-get node :id) +agent-topology--selected))
                    (plist-get +agent-topology--model :nodes))
    (setq +agent-topology--selected (plist-get (car (plist-get +agent-topology--model :nodes)) :id)))
  (if (plist-get +agent-topology--model :nodes)
      (+agent-topology--render)
    (insert "\n  No agents yet.  H-T starts one for the document you are on.\n")))

(defun +agent-panel-redraw ()
  "Redraw the panel."
  (interactive)
  (when-let* ((buffer (get-buffer +agent-panel-buffer-name)))
    (with-current-buffer buffer
      (when (timerp +agent-panel--timer) (cancel-timer +agent-panel--timer))
      (setq +agent-panel--timer nil)
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (setq header-line-format (+agent-panel--tabs))
        (erase-buffer)
        (pcase +agent-panel--tab
          ('tasks (+agent-panel--draw-tasks))
          ('pi (+agent-panel--draw-pi))
          ('graph (+agent-panel--draw-graph)))
        (unless (eq +agent-panel--tab 'graph)
          (goto-char (point-min))
          (forward-line (1- line)))))))

;;;; Following state, only while shown

(defun +agent-panel--visible-p ()
  "Whether the panel is on screen."
  (when-let* ((buffer (get-buffer +agent-panel-buffer-name))) (get-buffer-window buffer t)))

(defun +agent-panel--changed-h (&rest _)
  "Redraw the panel once after a burst of events, when it is shown."
  (when-let* ((buffer (get-buffer +agent-panel-buffer-name))
              ((+agent-panel--visible-p)))
    (with-current-buffer buffer
      (unless +agent-panel--timer
        (setq +agent-panel--timer (run-with-idle-timer 0.4 nil #'+agent-panel-redraw))))))

(defun +agent-panel--comments-changed-h (file)
  "A comment changed in FILE: forget a tree cache that does not know its folder."
  (when (stringp file)
    (let ((journal (expand-file-name +agent-collab-comments-file
                                     (file-name-directory (+agent-collab--client-file file)))))
      (maphash (lambda (root journals)
                 (when (and (string-prefix-p root journal) (not (member journal journals)))
                   (remhash root +agent-panel--journal-cache)))
               +agent-panel--journal-cache)))
  (+agent-panel--changed-h))

(defun +agent-panel--listen (on)
  "Start following state events when ON, stop otherwise."
  (dolist (spec '((+agent-collab-changed-functions . +agent-panel--comments-changed-h)
                  (+agent-binding-changed-functions . +agent-panel--changed-h)
                  (+agent-acp-changed-functions . +agent-panel--changed-h)
                  (window-buffer-change-functions . +agent-panel--window-change-h)))
    (if on (add-hook (car spec) (cdr spec)) (remove-hook (car spec) (cdr spec)))))

(defun +agent-panel--teardown-h ()
  "Stop following state when the panel buffer goes."
  (when (timerp +agent-panel--timer) (cancel-timer +agent-panel--timer))
  (+agent-panel--listen nil))

(defun +agent-panel--window-change-h (_frame)
  "Follow the focused document, and stop listening while the panel is hidden."
  (if (not (+agent-panel--visible-p))
      (+agent-panel--listen nil)
    (when-let* ((buffer (get-buffer +agent-panel-buffer-name))
                (document +agent-binding-last-document))
      (with-current-buffer buffer
        (unless (equal document +agent-panel--document)
          (setq +agent-panel--document document)
          (+agent-panel--changed-h))))))

;;;; Commands

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
  "Return the thread at point."
  (pcase-let ((`(,file . ,id) (+agent-panel--at '+agent-panel-task)))
    (or (+agent-collab-thread file id) (user-error "The task is gone"))))

(defun +agent-panel--editing-window ()
  "Return a window to open documents in: not the panel."
  (or (get-mru-window nil nil t)
      (let ((window (selected-window))) (split-window window nil 'left))))

(defun +agent-panel-visit ()
  "Open the item at point."
  (interactive)
  (pcase +agent-panel--tab
    ('graph (+agent-topology-visit))
    ('pi (let ((buffer (ignore-errors (+agent-panel--at '+agent-panel-pi))))
           (if (buffer-live-p buffer) (+agent-acp-show-buffer buffer) (+agent-panel-open-pi))))
    (_ (let* ((thread (+agent-panel--task))
              (file (alist-get 'file thread))
              (line (+agent-collab--thread-line thread)))
         (select-window (+agent-panel--editing-window))
         (if (and (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file) (fboundp '+noema/open-file))
             (progn (+noema/open-file file)
                    (when (and line (fboundp '+noema/command))
                      (run-at-time 0.3 nil (lambda () (ignore-errors (+noema/command "goto-line" `((value . ,line))))))))
           (find-file file)
           (when line (goto-char (point-min)) (forward-line (1- line))))))))

(defun +agent-panel-chat ()
  "Open the agent of the item at point for a longer discussion."
  (interactive)
  (pcase +agent-panel--tab
    ('graph (+agent-topology-chat))
    (_ (let ((session (+agent-collab--thread-session (+agent-panel--task))))
         (select-window (+agent-panel--editing-window))
         (+agent-acp-show-buffer session)))))

(defun +agent-panel--choose-agent (file)
  "Read which session takes a task on FILE: its collaborators first."
  (let* ((collaborators (mapcar (lambda (s) (plist-get s :buffer)) (+agent-binding-sessions file)))
         (others (seq-remove (lambda (b) (memq b collaborators))
                             (seq-filter #'buffer-live-p
                                         (mapcar (lambda (s) (plist-get s :buffer))
                                                 (seq-remove (lambda (s) (eq (+agent-session-kind (plist-get s :buffer)) 'pi))
                                                             (+agent-pool-sessions nil nil 'all))))))
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

(declare-function +agent-shell/here "tools/agent/config" (agent &optional file))

(defun +agent-panel-dispatch ()
  "Hand the task at point to an agent; its work comes back through the API."
  (interactive)
  (let* ((thread (+agent-panel--task))
         (file (alist-get 'file thread)))
    (+agent-collab-dispatch file (alist-get 'id thread) (+agent-panel--choose-agent file))
    (+agent-panel-redraw)))

(defun +agent-panel-reply (text)
  "Reply TEXT in the task at point."
  (interactive (list (read-string "Reply: ")))
  (let ((thread (+agent-panel--task)))
    (+agent-collab--add-message (alist-get 'file thread) (alist-get 'id thread) nil text)
    (+agent-panel-redraw)))

(defun +agent-panel-resolve ()
  "Resolve the task at point, or reopen it."
  (interactive)
  (let ((thread (+agent-panel--task)))
    (+agent-collab--set-resolved (alist-get 'file thread) (alist-get 'id thread)
                                 (not (equal (alist-get 'status thread) "resolved")))
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
  "Re-read everything, including the folder tree's journals."
  (interactive)
  (clrhash +agent-panel--journal-cache)
  (+agent-panel-redraw))

(defun +agent-panel-open-pi ()
  "Open the document's project Pi."
  (interactive)
  (require 'noema-pi-router nil t)
  (let ((default-directory (if +agent-panel--document (file-name-directory +agent-panel--document) default-directory)))
    (select-window (+agent-panel--editing-window))
    (call-interactively #'noema-pi-open)))

(defun +agent-panel-open-global-pi ()
  "Open the global Pi."
  (interactive)
  (require 'noema-pi-router nil t)
  (select-window (+agent-panel--editing-window))
  (call-interactively #'noema-pi-emacs-open))

(defun +agent-panel--select (tab)
  "Show TAB."
  (setq +agent-panel--tab tab)
  (+agent-panel-redraw))

(defun +agent-panel-tasks-tab () "Show the tasks." (interactive) (+agent-panel--select 'tasks))
(defun +agent-panel-pi-tab () "Show Pi." (interactive) (+agent-panel--select 'pi))
(defun +agent-panel-graph-tab () "Show the topology." (interactive) (+agent-panel--select 'graph))

(defun +agent-panel-next-tab ()
  "Show the next tab."
  (interactive)
  (+agent-panel--select (pcase +agent-panel--tab ('tasks 'pi) ('pi 'graph) (_ 'tasks))))

(defun +agent-panel-next ()
  "Move to the next item."
  (interactive)
  (if (eq +agent-panel--tab 'graph) (+agent-topology-next)
    (let ((property (if (eq +agent-panel--tab 'pi) '+agent-panel-pi '+agent-panel-task))
          (here (get-text-property (point) (if (eq +agent-panel--tab 'pi) '+agent-panel-pi '+agent-panel-task))))
      (let ((next (next-single-property-change (point) property)))
        (while (and next (equal (get-text-property next property) here) (setq next (next-single-property-change next property))))
        (when-let* ((target (and next (if (get-text-property next property) next
                                         (next-single-property-change next property)))))
          (goto-char target))))))

(defun +agent-panel-previous ()
  "Move to the previous item."
  (interactive)
  (if (eq +agent-panel--tab 'graph) (+agent-topology-previous)
    (let* ((property (if (eq +agent-panel--tab 'pi) '+agent-panel-pi '+agent-panel-task))
           (here (get-text-property (point) property))
           (prev (previous-single-property-change (point) property)))
      (while (and prev (> prev (point-min)) (equal (get-text-property (1- prev) property) here))
        (setq prev (previous-single-property-change prev property)))
      (when prev
        (let ((item (get-text-property (max (point-min) (1- prev)) property)))
          (when item
            (goto-char (or (previous-single-property-change prev property) (point-min)))))))))

(defvar-keymap +agent-panel-mode-map
  :parent special-mode-map
  "1" #'+agent-panel-tasks-tab
  "2" #'+agent-panel-pi-tab
  "3" #'+agent-panel-graph-tab
  "TAB" #'+agent-panel-next-tab
  "RET" #'+agent-panel-visit
  "S-<return>" #'+agent-panel-chat
  "n" #'+agent-panel-next
  "p" #'+agent-panel-previous
  "j" #'+agent-panel-next
  "k" #'+agent-panel-previous
  "a" #'+agent-panel-dispatch
  "r" #'+agent-panel-reply
  "c" #'+agent-panel-resolve
  "s" #'+agent-panel-cycle-scope
  "R" #'+agent-panel-toggle-resolved
  "E" #'+agent-panel-open-global-pi
  "g" #'+agent-panel-refresh
  "q" #'+agent/panel)

(define-derived-mode +agent-panel-mode special-mode "Collaboration"
  "The collaboration panel: tasks, Pi and topology (C75)."
  (setq truncate-lines t)
  (setq-local cursor-type 'bar)
  (add-hook 'kill-buffer-hook #'+agent-panel--teardown-h nil t))

(when (fboundp 'set-evil-initial-state!) (set-evil-initial-state! '+agent-panel-mode 'emacs))

;;;###autoload
(defun +agent/panel ()
  "Show or hide the collaboration panel on the right (C75)."
  (interactive)
  (if-let* ((window (+agent-panel--visible-p)))
      (progn (delete-window window) (+agent-panel--listen nil))
    (let ((buffer (get-buffer-create +agent-panel-buffer-name))
          (document (or (+agent-document-file) +agent-binding-last-document)))
      (with-current-buffer buffer
        (unless (derived-mode-p '+agent-panel-mode) (+agent-panel-mode))
        (setq +agent-panel--document document))
      (let ((window (display-buffer-in-side-window
                     buffer `((side . right) (slot . 0) (window-width . ,+agent-panel-width)
                              (preserve-size . (t . nil))
                              (window-parameters . ((no-delete-other-windows . t)))))))
        (set-window-dedicated-p window t)
        (+agent-panel--listen t)
        (+agent-panel-redraw)
        (select-window window)))))


(provide 'tools/agent/panel)
;;; panel.el ends here
