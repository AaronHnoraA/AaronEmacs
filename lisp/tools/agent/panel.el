;;; tools/agent/panel.el --- The collaboration panel: tasks, Pi, topology -*- lexical-binding: t; -*-

;;; Commentary:
;; `H-t' (C75) opens a side window on the right that stays until `H-t' or `q'
;; hides it.  Its three tabs are three buffers shown in that one window:
;;
;;   Tasks  the comments of the focused document, its folder, the folder and
;;          its subfolders, or the whole Project, as a task list.  New comments
;;          land here; a task is handed to an agent from here (C73).
;;   Pi     the Project's own Pi, its chat embedded here: a project Pi is shown
;;          only in this panel, never among the other sessions (C78).
;;   Graph  the interactive graph of Pi, projects, documents and agents
;;          (`tools/agent/board', C78).
;;
;; The panel listens to state events only while it is shown and redraws once
;; per burst; closed, it costs nothing.  The task list reads the folders'
;; comment journals (C70); a folder tree's journals are found once and kept
;; until a comment appears in a folder not yet known.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
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
(declare-function evil-define-minor-mode-key "evil-core" (state mode key def &rest bindings))

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
  (setq file (+agent-collab--client-file file))
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

(defconst +agent-panel--tabs '((tasks "1" "Tasks") (pi "2" "Pi") (graph "3" "Graph"))
  "The tabs: symbol, key and title.")

(defun +agent-panel--tab-map (tab)
  "Return the header keymap that shows TAB on a click."
  (let ((map (make-sparse-keymap)))
    ;; The tabs are the window's tab line.
    (define-key map [tab-line mouse-1] (lambda () (interactive) (+agent-panel--show-tab tab)))
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

;;;; The board (C81): comments as cards in lanes, learnt from SharpMD
;;
;; A comment is a card.  Its lane is not stored: it follows from the comment
;; and the live state of the agent it was handed to, so the board is always
;; current.  Moving a card is the action that lane means: into In progress
;; hands it to an agent, into Done marks it done, back into To do takes it from
;; its agent.  The lanes stack top to bottom because the panel is narrow.

(defconst +agent-panel-lanes
  '((todo "To do" "not handed to an agent yet")
    (doing "In progress" "an agent works on it")
    (waiting "Waiting for you" "its agent needs your answer or permission")
    (paused "Paused" "its agent's session is gone")
    (done "Done" "marked done"))
  "The board's lanes, in order: symbol, title, meaning.")

(defface +agent-panel-lane-todo '((t :inherit font-lock-comment-face :weight bold)) "To do." :group '+agent-panel)
(defface +agent-panel-lane-doing '((t :inherit font-lock-function-name-face :weight bold)) "In progress." :group '+agent-panel)
(defface +agent-panel-lane-waiting '((t :inherit error :weight bold)) "Waiting for you." :group '+agent-panel)
(defface +agent-panel-lane-paused '((t :inherit warning :weight bold)) "Paused." :group '+agent-panel)
(defface +agent-panel-lane-done '((t :inherit success :weight bold)) "Done." :group '+agent-panel)

(defvar-local +agent-panel--folded '(done) "Lanes shown folded, as a count.")

(defun +agent-panel--lane-face (lane)
  "Return LANE's face."
  (intern (format "+agent-panel-lane-%s" lane)))

(defun +agent-panel--agent-state (snapshot)
  "Return (WORD . MARK) for the session SNAPSHOT: what it does now."
  (cond ((null snapshot) '("gone" . "×"))
        ((memq (plist-get snapshot :process-state) '(dead disconnected)) '("disconnected" . "×"))
        ((plist-get snapshot :attention-kind) '("needs you" . "!"))
        ((memq (plist-get snapshot :turn-state) '(running stopping)) '("working" . "●"))
        (t '("idle" . "◌"))))

(defun +agent-panel--card-lane (thread)
  "Return THREAD's lane and its agent's snapshot, as (LANE . SNAPSHOT)."
  (let* ((token (alist-get 'session (alist-get 'assignee thread)))
         (buffer (and token (+agent-acp-session-buffer-for-token token)))
         (snapshot (and (buffer-live-p buffer) (+agent-session-snapshot buffer))))
    (cons (cond ((equal (alist-get 'status thread) "resolved") 'done)
                ((null token) 'todo)
                ((or (null snapshot) (memq (plist-get snapshot :process-state) '(dead disconnected))) 'paused)
                ((plist-get snapshot :attention-kind) 'waiting)
                (t 'doing))
          snapshot)))

(defun +agent-panel--insert-card (thread lane snapshot)
  "Insert THREAD as a card of LANE; SNAPSHOT is its agent's."
  (let* ((start (point))
         (bar (propertize "▌" 'face (+agent-panel--lane-face lane)))
         (width (max 20 (- (window-width (get-buffer-window (current-buffer))) 6)))
         (first (car (alist-get 'messages thread)))
         (assignee (alist-get 'name (alist-get 'assignee thread)))
         (line (+agent-collab--thread-line thread)))
    (insert " " bar " " (propertize (truncate-string-to-width (string-trim (or (alist-get 'text first) "")) width nil nil "…")
                                    'face (if (eq lane 'done) '+agent-panel-done 'bold))
            "\n")
    (unless (string-empty-p (or (alist-get 'quote thread) ""))
      (insert " " bar " " (propertize (truncate-string-to-width (format "«%s»" (alist-get 'quote thread)) width nil nil "…")
                                      'face '+agent-panel-quote)
              "\n"))
    (insert " " bar " "
            (propertize (concat (file-name-nondirectory (alist-get 'file thread)) (if line (format ":%d" line) ""))
                        'face '+agent-panel-meta)
            (if assignee
                (pcase-let ((`(,word . ,mark) (+agent-panel--agent-state snapshot)))
                  (concat "  " (propertize (format "%s %s %s" mark assignee word)
                                           'face (if (member word '("working" "needs you")) (+agent-panel--lane-face lane) '+agent-panel-meta))))
              "")
            (propertize (concat "  " (+agent-panel--age (alist-get 'at (car (last (alist-get 'messages thread)))))) 'face '+agent-panel-meta)
            "\n")
    (when-let* (((eq lane 'waiting))
                (request (or (plist-get snapshot :request) (plist-get snapshot :last-error)))
                ((stringp request))
                ((not (string-empty-p request))))
      (insert " " bar " "
              (propertize (truncate-string-to-width
                           (concat "Needs: " (string-join (split-string request) " "))
                           width nil nil "…")
                          'face '+agent-panel-meta 'help-echo request)
              "\n"))
    (put-text-property start (point) '+agent-panel-task (cons (alist-get 'file thread) (alist-get 'id thread)))))

(defun +agent-panel--project-sessions (file)
  "Return the agent sessions of FILE's project, Pi first."
  (let ((root (+agent-panel--project-root file)))
    (sort (seq-filter (lambda (session) (equal (file-name-as-directory (or (plist-get session :root) "")) root))
                      (+agent-pool-sessions nil nil 'all))
          (lambda (a b) (eq (+agent-session-kind (plist-get a :buffer)) 'pi)))))

(defun +agent-panel--insert-agents (file)
  "Insert the row of FILE's project agents and what each does now."
  (let ((sessions (+agent-panel--project-sessions file)))
    (insert (propertize "  Agents  " 'face 'bold))
    (if (null sessions)
        (insert (propertize "none yet — H-T starts one" 'face '+agent-panel-meta))
      (dolist (session sessions)
        (pcase-let* ((`(,word . ,mark) (+agent-panel--agent-state session))
                     (start (point)))
          (insert (propertize (format "%s %s" mark (or (plist-get session :name) "agent"))
                              'face (pcase word ("working" '+agent-panel-lane-doing) ("needs you" '+agent-panel-lane-waiting)
                                           ("idle" 'default) (_ '+agent-panel-meta))
                              'help-echo (format "%s — RET opens its chat" word))
                  "   ")
          (put-text-property start (point) '+agent-panel-agent (plist-get session :buffer)))))
    (insert "\n")))

(defun +agent-panel--draw-tasks ()
  "Draw the board."
  ;; A Noema page names its document `/fs:local:...'; threads name client paths.
  (let* ((file (and +agent-panel--document (+agent-collab--client-file +agent-panel--document))))
    (if (not file)
        (insert "\n  Open a document (a note or a file) to see its tasks.\n")
      (let ((cards (mapcar (lambda (thread) (cons thread (+agent-panel--card-lane thread)))
                           (apply #'append (mapcar #'cdr (+agent-panel-tasks file +agent-panel--scope))))))
        (insert "\n")
        (+agent-panel--insert-agents file)
        (insert (propertize (format "  Tasks of %s · %d open"
                                    (alist-get +agent-panel--scope +agent-panel-scopes)
                                    (seq-count (lambda (card) (not (eq (cadr card) 'done))) cards))
                            'face '+agent-panel-meta)
                "\n")
        (if (null cards)
            (insert "\n  No comments.  Select text in a note and comment (⌘⌥M), or C-c A d.\n")
          (pcase-dolist (`(,lane ,title ,meaning) +agent-panel-lanes)
            (let ((here (seq-filter (lambda (card) (eq (cadr card) lane)) cards))
                  (start (point)))
              (when (or here (memq lane '(todo doing)))
                (insert "\n " (propertize (format "%s %s" (if (memq lane +agent-panel--folded) "▸" "▾") title)
                                          'face (+agent-panel--lane-face lane))
                        (propertize (format "  %d" (length here)) 'face '+agent-panel-meta)
                        "\n")
                (put-text-property start (point) '+agent-panel-lane lane)
                (put-text-property start (point) 'help-echo meaning)
                (unless (memq lane +agent-panel--folded)
                  (if here
                      (let ((first t))
                        (dolist (card here)
                          (unless first (insert "\n"))
                          (setq first nil)
                          (+agent-panel--insert-card (car card) lane (cddr card))))
                    (insert (propertize "   —\n" 'face '+agent-panel-meta))))))))
        (insert (propertize "\n  RET open  a hand over  > < move  d done  u take back  o chat\n  TAB fold lane  s scope  g refresh\n"
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
  "Show TAB in the panel.  Nothing here waits: a Pi still to start shows a
starting page at once and replaces it when it is there."
  (pcase tab
    ('tasks (setq +agent-panel--tab 'tasks)
            (+agent-panel--put (+agent-panel--tasks-buffer)) (+agent-panel-redraw))
    ('pi (require 'noema-pi-router)
         (let* ((directory (+agent-panel--pi-directory))
                (live (noema-pi-router-buffer directory)))
           ;; The display function routes the project Pi into this panel (C78).
           (if live (+agent-panel-display-pi live)
             (setq +agent-panel--tab 'pi)
             (+agent-panel--put (+agent-panel--pi-starting directory))
             (run-with-timer 0.05 nil #'+agent-panel--start-pi directory))))
    ('graph (require 'tools/agent/board)
            (setq +agent-panel--tab 'graph)
            (+agent-panel--put (+agent-board-buffer))
            (+agent-board-refresh t)))
  (force-mode-line-update t))

(defconst +agent-panel--pi-starting-name " *Pi starting*")

(defvar-keymap +agent-panel-starting-mode-map
  :parent special-mode-map
  "1" #'+agent-panel-tasks-tab "2" #'+agent-panel-pi-tab "3" #'+agent-panel-graph-tab
  "C-c 1" #'+agent-panel-tasks-tab "C-c 2" #'+agent-panel-pi-tab "C-c 3" #'+agent-panel-graph-tab
  "q" #'+agent/panel)

(define-derived-mode +agent-panel-starting-mode special-mode "Pi"
  "The panel's page while the project Pi starts.")

(defun +agent-panel--pi-starting (directory)
  "Return the page the Pi tab shows while DIRECTORY's Pi starts."
  (with-current-buffer (get-buffer-create +agent-panel--pi-starting-name)
    (unless (derived-mode-p '+agent-panel-starting-mode) (+agent-panel-starting-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "\n  Starting this project's Pi…\n" 'face 'bold)
              (propertize (format "\n  %s\n" (abbreviate-file-name directory)) 'face '+agent-panel-meta)))
    (current-buffer)))

(defun +agent-panel--start-pi (directory)
  "Start DIRECTORY's Pi and show it, if the panel still waits for it."
  (condition-case failure
      (let ((buffer (noema-pi-project-ensure directory)))
        (when (and (eq +agent-panel--tab 'pi)
                   (when-let* ((window (+agent-panel--window)))
                     (equal (buffer-name (window-buffer window)) +agent-panel--pi-starting-name)))
          (+agent-panel-display-pi buffer)))
    (error
     (when-let* ((page (get-buffer +agent-panel--pi-starting-name)))
       (with-current-buffer page
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert (propertize (format "\n  Pi could not start: %s\n" (error-message-string failure)) 'face 'error))))))))

(defun +agent-panel-redraw ()
  "Redraw the task list."
  (interactive)
  (when-let* ((buffer (get-buffer +agent-panel-buffer-name)))
    (with-current-buffer buffer
      (when (timerp +agent-panel--timer) (cancel-timer +agent-panel--timer))
      (setq +agent-panel--timer nil)
      (let ((inhibit-read-only t)
            (identity (cond ((get-text-property (point) '+agent-panel-agent)
                             (cons '+agent-panel-agent (get-text-property (point) '+agent-panel-agent)))
                            ((get-text-property (point) '+agent-panel-lane)
                             (cons '+agent-panel-lane (get-text-property (point) '+agent-panel-lane)))
                            ((ignore-errors (+agent-panel--at '+agent-panel-task))
                             (cons '+agent-panel-task (+agent-panel--at '+agent-panel-task)))))
            (line (line-number-at-pos)))
        (erase-buffer)
        (condition-case failure
            (+agent-panel--draw-tasks)
          ;; Never a silently half-drawn list: say what failed.
          (error (insert (propertize (format "\n  The task list failed: %s\n" (error-message-string failure))
                                     'face 'error))))
        (goto-char (point-min))
        (if identity
            (when-let* ((match (text-property-search-forward (car identity) (cdr identity) #'equal)))
              (goto-char (prop-match-beginning match)))
          (forward-line (1- line)))))))

;;;; The project Pi, embedded (C78)

(defvar-keymap +agent-panel-pi-mode-map
  "C-c 1" #'+agent-panel-tasks-tab
  "C-c 2" #'+agent-panel-pi-tab
  "C-c 3" #'+agent-panel-graph-tab)

(define-minor-mode +agent-panel-pi-mode
  "A project Pi shown in the collaboration panel."
  :lighter nil)

(after! evil
  (evil-define-minor-mode-key 'normal '+agent-panel-pi-mode
    (kbd "1") #'+agent-panel-tasks-tab
    (kbd "2") #'+agent-panel-pi-tab
    (kbd "3") #'+agent-panel-graph-tab))

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
  "Return PROPERTY at point or on the nearest item above in the same lane."
  (or (get-text-property (point) property)
      (save-excursion
        (let ((found nil))
          (while (and (not found)
                      (not (get-text-property (point) '+agent-panel-lane))
                      (not (get-text-property (point) '+agent-panel-agent))
                      (zerop (forward-line -1)))
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
      (split-window (selected-window) nil 'left)))

(defun +agent-panel-visit ()
  "Open the task at point at its place."
  (interactive)
  (let* ((thread (+agent-panel--task))
         (file (alist-get 'file thread))
         (line (+agent-collab--thread-line thread)))
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
  (let ((session (+agent-collab--thread-session (+agent-panel--task))))
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
  "Hand the task at point to an agent; its work comes back through the API."
  (interactive)
  (let* ((thread (+agent-panel--task))
         (file (alist-get 'file thread)))
    (+agent-collab-dispatch file (alist-get 'id thread) (+agent-panel--choose-agent file))
    (+agent-panel-redraw)))

(defun +agent-panel-resolve ()
  "Resolve the task at point, or reopen it."
  (interactive)
  (let ((thread (+agent-panel--task)))
    (+agent-collab--set-resolved (alist-get 'file thread) (alist-get 'id thread)
                                 (not (equal (alist-get 'status thread) "resolved")))
    (+agent-panel-redraw)))

(defun +agent-panel--thread-lane (thread)
  "Return THREAD's lane."
  (car (+agent-panel--card-lane thread)))

(defun +agent-panel--move-to (thread lane)
  "Move THREAD's card to LANE by doing what LANE means."
  (let ((file (alist-get 'file thread)) (id (alist-get 'id thread)))
    (pcase lane
      ('done (+agent-collab--set-resolved file id t))
      ('todo (when (equal (alist-get 'status thread) "resolved") (+agent-collab--set-resolved file id nil))
             (when (alist-get 'assignee thread) (+agent-collab-unassign file id)))
      ((or 'doing 'waiting 'paused)
       (when (equal (alist-get 'status thread) "resolved") (+agent-collab--set-resolved file id nil))
       (+agent-collab-dispatch file id (+agent-panel--choose-agent file))))))

(defun +agent-panel-move-right ()
  "Move the card at point on: To do → In progress → Done."
  (interactive)
  (let ((thread (+agent-panel--task)))
    (+agent-panel--move-to thread (pcase (+agent-panel--thread-lane thread)
                                    ('todo 'doing) ('done (user-error "Already done")) (_ 'done)))
    (+agent-panel-redraw)))

(defun +agent-panel-move-left ()
  "Move the card at point back: Done → In progress → To do."
  (interactive)
  (let ((thread (+agent-panel--task)))
    (+agent-panel--move-to thread (pcase (+agent-panel--thread-lane thread)
                                    ('todo (user-error "Already in To do"))
                                    ('done (if (alist-get 'assignee thread) 'doing 'todo))
                                    (_ 'todo)))
    (+agent-panel-redraw)))

(defun +agent-panel-take-back ()
  "Take the card at point back from its agent, into To do."
  (interactive)
  (+agent-panel--move-to (+agent-panel--task) 'todo)
  (+agent-panel-redraw))

(defun +agent-panel-toggle-lane ()
  "Fold or unfold the lane at point."
  (interactive)
  (let ((lane (or (get-text-property (point) '+agent-panel-lane)
                  (and (get-text-property (point) '+agent-panel-task)
                       (+agent-panel--thread-lane (+agent-panel--task)))
                  (user-error "No lane here"))))
    (setq +agent-panel--folded (if (memq lane +agent-panel--folded) (delq lane +agent-panel--folded)
                                 (cons lane +agent-panel--folded)))
    (+agent-panel-redraw)))

(defun +agent-panel-ret ()
  "Open the card, the agent's chat, or fold the lane at point."
  (interactive)
  (cond ((get-text-property (point) '+agent-panel-agent)
         (let ((buffer (get-text-property (point) '+agent-panel-agent)))
           (select-window (+agent-panel--editing-window))
           (+agent-acp-show-buffer buffer)))
        ((get-text-property (point) '+agent-panel-lane) (+agent-panel-toggle-lane))
        (t (+agent-panel-visit))))

(defun +agent-panel-cycle-scope ()
  "Widen the task scope: file, folder, folder and below, project, file."
  (interactive)
  (let* ((scopes (mapcar #'car +agent-panel-scopes))
         (next (or (cadr (memq +agent-panel--scope scopes)) (car scopes))))
    (setq +agent-panel--scope next)
    (+agent-panel-redraw)
    (message "Tasks of %s" (alist-get next +agent-panel-scopes))))

(defun +agent-panel-refresh ()
  "Re-read everything, including the folder tree's journals."
  (interactive)
  (clrhash +agent-panel--journal-cache)
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
  "RET" #'+agent-panel-ret
  ">" #'+agent-panel-move-right
  "<" #'+agent-panel-move-left
  "d" #'+agent-panel-resolve
  "u" #'+agent-panel-take-back
  "TAB" #'+agent-panel-toggle-lane
  "S-<return>" #'+agent-panel-chat
  "n" #'+agent-panel-next
  "p" #'+agent-panel-previous
  "j" #'+agent-panel-next
  "k" #'+agent-panel-previous
  "a" #'+agent-panel-dispatch
  "c" #'+agent-panel-resolve
  "s" #'+agent-panel-cycle-scope
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
