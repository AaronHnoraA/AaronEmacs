;;; manager.el --- One front door for agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; ECA's workspace organization and HQ's real-buffer navigation, on Aaron's
;; retained UI.  This view owns no execution, transcript or polling loop.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/sessions)
(require 'aaron-ui-etaf)
(require 'tools/agent/ui)

(autoload '+agent-shell/resume "tools/agent/config" nil t)
(autoload '+agent-top "tools/agent/top" nil t)
(declare-function aaron-ui-float-show "aaron-ui-float" (buffer &rest options))
(declare-function aaron-ui-float-panel-for-buffer "aaron-ui-float" (buffer))
(declare-function aaron-ui-float-panel-frame "aaron-ui-float" (panel))
(declare-function aaron-ui-float-visible-p "aaron-ui-float" (panel))
(declare-function aaron-ui-float-hide "aaron-ui-float" (&optional panel))
(declare-function aaron-ui-float-preview-buffer "aaron-ui-float" (panel buffer &rest options))
(declare-function aaron-ui-float-clear-preview "aaron-ui-float" (panel))
(autoload '+agent-session-menu "tools/agent/menu" nil t)

(defconst +agent-manager-buffer-name "*Agent Manager*")
(defvar-local +agent-manager--model nil)
(defvar-local +agent-manager--timer nil)
(defvar-local +agent-manager--origin nil)
(defvar-local +agent-manager--root nil)
(defvar-local +agent-manager--target nil)
(defvar-local +agent-manager--directory nil)
(defvar-local +agent-manager--scope 'all)
(defvar-local +agent-manager--query "")
(defvar-local +agent-manager--selected nil)
(defvar-local +agent-manager--watching nil)
(defvar-local +agent-manager--entity-ref nil)
(defvar-local +agent-manager--view 'sessions)
(defvar-local +agent-manager--collapsed nil)

(defun +agent-manager--visible-sessions (sessions)
  "SESSIONS whose workspace is open, including search matches."
  (seq-remove (lambda (session)
                (and (string-empty-p +agent-manager--query)
                     (member (+agent-manager--group session) +agent-manager--collapsed)))
              sessions))

(defun +agent-manager--preview ()
  "Preview the selected real buffer; never replace its display owner."
  (when-let* (((featurep 'aaron-ui-float))
              (panel (aaron-ui-float-panel-for-buffer (current-buffer)))
              ((aaron-ui-float-visible-p panel)))
    (if (or (eq +agent-manager--view 'usage)
            (not (+agent-acp-agent-buffer-p +agent-manager--selected)))
        (aaron-ui-float-clear-preview panel)
      (let ((owner (current-buffer)) (+agent-acp-previewing t))
        (aaron-ui-float-preview-buffer
         panel +agent-manager--selected :sidebar-width 46
         :on-hide (lambda () (with-current-buffer owner (+agent-manager-hide))))
        (+agent-render-flush +agent-manager--selected)))))

(defun +agent-manager--ref (buffer)
  "A retained Host identity independent of BUFFER's mutable display name."
  (with-current-buffer buffer
    (or +agent-manager--entity-ref
        (setq +agent-manager--entity-ref (make-symbol "agent-session")))))

(defun +agent-manager--needs-attention-p (session)
  "Whether SESSION has an unresolved request, error or unread result."
  (+agent-session-attention-rank session))

(defun +agent-manager--group (session)
  "Group SESSION by target and workspace, never by basename alone."
  (+agent-pool-key session))

(defun +agent-manager--sessions (sessions scope root query &optional target)
  "Filter and stably group SESSIONS using SCOPE, ROOT and literal QUERY."
  (let ((needle (downcase query))
        (inbox (eq scope 'attention)))
    (sort
     (seq-filter
      (lambda (session)
        (and (eq (+agent-session-kind session) 'agent)
             (not (+agent-session-private-p session))
             (pcase scope
               ('project (equal (+agent-pool-key (list :root root :target target))
                                (+agent-pool-key session)))
               ('attention (+agent-manager--needs-attention-p session))
               (_ t))
             (or (string-empty-p needle)
                 (string-match-p
                  (regexp-quote needle)
                  (downcase (format "%s %s %s %s %s %s %s"
                                    (plist-get session :name)
                                    (plist-get session :agent)
                                    (plist-get session :root)
                                    (plist-get (plist-get session :group) :label)
                                    (plist-get (plist-get session :usage) :model)
                                    (or (plist-get session :title) "")
                                    (or (plist-get session :last-prompt) "")))))))
      sessions)
     (lambda (a b)
       (let ((ga (+agent-manager--group a)) (gb (+agent-manager--group b))
             (ra (and inbox (+agent-session-attention-rank a)))
             (rb (and inbox (+agent-session-attention-rank b))))
         (cond ((and ra rb (/= ra rb)) (< ra rb))
               ((equal ga gb)
                (string-lessp (or (plist-get a :name) "")
                              (or (plist-get b :name) "")))
               (t (string-lessp (format "%s" ga) (format "%s" gb)))))))))

(defun +agent-manager--selected-session ()
  "Read the selected live entity again before executing an action."
  (unless (+agent-acp-agent-buffer-p +agent-manager--selected)
    (user-error "Select an agent session first"))
  +agent-manager--selected)

(defun +agent-manager-refresh ()
  "Refresh retained state without starting processes or querying a host."
  (interactive)
  (when +agent-manager--model
    (let* ((all (+agent-manager--sessions (+agent-acp-sessions) 'all nil ""))
           (sessions (+agent-manager--sessions
                      all +agent-manager--scope +agent-manager--root
                      +agent-manager--query +agent-manager--target)))
      (unless (seq-find (lambda (session)
                         (eq (plist-get session :buffer) +agent-manager--selected))
                       (+agent-manager--visible-sessions sessions))
        (setq +agent-manager--selected
              (plist-get (car (+agent-manager--visible-sessions sessions)) :buffer)))
      (etaf-set-value
       +agent-manager--model
       (list :sessions sessions :selected +agent-manager--selected
             :scope +agent-manager--scope :query +agent-manager--query
             :view +agent-manager--view :collapsed +agent-manager--collapsed
             :total (length all)
             :attention (cl-count-if #'+agent-manager--needs-attention-p all)))
      (+agent-manager--preview))))

(defun +agent-manager--visible-p ()
  (when-let* ((buffer (get-buffer +agent-manager-buffer-name)))
    (get-buffer-window buffer 'visible)))

(defun +agent-manager--cancel-timer ()
  (when (timerp +agent-manager--timer) (cancel-timer +agent-manager--timer))
  (setq +agent-manager--timer nil))

(defun +agent-manager--changed-h (_source)
  "Coalesce state changes only while the manager is visible."
  (when-let* ((buffer (get-buffer +agent-manager-buffer-name)))
    (with-current-buffer buffer
      (if (not (+agent-manager--visible-p))
          (+agent-manager--suspend)
        (unless (timerp +agent-manager--timer)
          (setq +agent-manager--timer
                (run-at-time
                 0.15 nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq +agent-manager--timer nil)
                       (when (+agent-manager--visible-p)
                         (+agent-manager-refresh))))))))))))

(defun +agent-manager--suspend (&optional _panel)
  "Release UI work, keeping every agent and the retained view intact."
  (when-let* ((buffer (get-buffer +agent-manager-buffer-name)))
    (with-current-buffer buffer
      (+agent-manager--cancel-timer)
      (remove-hook '+agent-acp-changed-functions #'+agent-manager--changed-h)
      (setq +agent-manager--watching nil))))

(defun +agent-manager--killed-h ()
  (+agent-manager--suspend)
  (setq +agent-manager--model nil +agent-manager--selected nil
        +agent-manager--origin nil))

(defun +agent-manager-hide ()
  "Dismiss the manager and return to its source window; agents keep running."
  (interactive)
  (let ((origin +agent-manager--origin)
        (buffer (current-buffer)))
    (+agent-manager--suspend)
    (if-let* (((featurep 'aaron-ui-float))
              (panel (aaron-ui-float-panel-for-buffer buffer)))
        (aaron-ui-float-hide panel)
      (quit-window))
    (when (window-live-p origin) (select-window origin))))

(defun +agent-manager-select (buffer)
  "Select and preview BUFFER without marking it read or sending input."
  (unless (eq buffer +agent-manager--selected)
    (setq +agent-manager--selected buffer)
    (+agent-manager-refresh)))

(defun +agent-manager-next (&optional count)
  "Move COUNT entities through the current filtered list."
  (interactive "p")
  (let* ((sessions (+agent-manager--visible-sessions
                    (plist-get (etaf-value +agent-manager--model) :sessions)))
         (buffers (mapcar (lambda (s) (plist-get s :buffer)) sessions))
         (index (or (seq-position buffers +agent-manager--selected) 0)))
    (when buffers
      (+agent-manager-select
       (nth (mod (+ index (or count 1)) (length buffers)) buffers))
      (etaf-runtime-flush (etaf-runtime-for-buffer (current-buffer)))
      (etaf-focus (etaf-runtime-for-buffer (current-buffer))
                  (+agent-manager--ref +agent-manager--selected))
      (etaf-scroll-into-view
       (etaf-runtime-for-buffer (current-buffer))
       (+agent-manager--ref +agent-manager--selected)))))

(defun +agent-manager-previous () (interactive) (+agent-manager-next -1))

(defun +agent-manager-scope (scope)
  (setq +agent-manager--scope scope)
  (+agent-manager-refresh))

(defun +agent-manager-cycle-scope ()
  (interactive)
  (+agent-manager-scope
   (pcase +agent-manager--scope ('all 'project) ('project 'attention) (_ 'all))))

(defun +agent-manager-view (view)
  "Switch the shared surface to VIEW without changing its source window."
  (setq +agent-manager--view view
        +agent-manager--scope (if (eq view 'attention) 'attention 'all))
  (+agent-manager-refresh))

(defun +agent-manager-toggle-group (&optional group)
  "Fold GROUP, or the selected workspace, retaining fold state on refresh."
  (interactive)
  (let ((group (or group
                   (+agent-manager--group
                    (+agent-session-snapshot (+agent-manager--selected-session))))))
    (setq +agent-manager--collapsed
          (if (member group +agent-manager--collapsed)
              (remove group +agent-manager--collapsed)
            (cons group +agent-manager--collapsed)))
    (+agent-manager-refresh)))

(defun +agent-manager-toggle-project ()
  (interactive)
  (+agent-manager-scope (if (eq +agent-manager--scope 'project) 'all 'project)))

(defun +agent-manager-search (query)
  (interactive (list (read-string "Filter agents: " +agent-manager--query)))
  (setq +agent-manager--query query)
  (+agent-manager-refresh))

(defun +agent-manager-open (&optional input selected-buffer)
  "Open the selected real buffer, optionally focusing INPUT."
  (interactive)
  (let ((buffer (or selected-buffer (+agent-manager--selected-session))))
    (+agent-manager-hide)
    (if input (+agent-acp-focus-input buffer) (+agent-acp-show-buffer buffer))))

(defun +agent-manager-input () (interactive) (+agent-manager-open t))

(defun +agent-manager-new ()
  "Start in the source workspace, never the manager's incidental directory."
  (interactive)
  (let ((source +agent-manager--selected)
        (directory (if (buffer-live-p +agent-manager--selected)
                       (buffer-local-value 'default-directory +agent-manager--selected)
                     +agent-manager--directory)))
    (+agent-manager-hide)
    (if (+agent-acp-agent-buffer-p source)
        (with-current-buffer source (call-interactively #'+agent/start))
      (let ((default-directory directory)) (call-interactively #'+agent/start)))))

(defun +agent-manager-resume ()
  "Open native history, with no automatic replay.
Without a live selection a new client lists the provider's saved conversations."
  (interactive)
  (let ((buffer +agent-manager--selected)
        (directory +agent-manager--directory))
    (+agent-manager-hide)
    (if (+agent-acp-agent-buffer-p buffer)
        (progn (+agent-acp-show-buffer buffer)
               (with-current-buffer buffer (call-interactively #'+agent-shell/resume)))
      (let ((default-directory directory))
        (call-interactively #'+agent/resume-saved)))))

(defun +agent-manager-queue ()
  "Manage the selected session's queued prompts."
  (interactive)
  (let ((buffer (+agent-manager--selected-session)))
    (+agent-manager-hide)
    (+agent/queue buffer)))

(defun +agent-manager-changes ()
  "Open a file the selected session changed."
  (interactive)
  (let ((buffer (+agent-manager--selected-session)))
    (+agent-manager-hide)
    (+agent-session-changes buffer)))

(defun +agent-manager-review ()
  "Enter the selected session at its pending native request."
  (interactive)
  (let ((buffer (+agent-manager--selected-session)))
    (+agent-manager-hide)
    (+agent-acp-review-request buffer)))

(defun +agent-manager-action (operation)
  "Dispatch OPERATION to the selected session through its shared adapter."
  (let ((buffer (+agent-manager--selected-session)))
    (pcase operation
      ('stop (+agent-acp-stop buffer))
      ('close (+agent-acp-close buffer))
      ('rename (+agent-session-rename buffer))
      ('restart (+agent-manager-hide) (+agent-session-restart buffer))
      ('fork (+agent-manager-hide) (+agent-session-fork buffer))
      ('model (+agent-manager-hide) (+agent-acp-show-buffer buffer)
              (with-current-buffer buffer (call-interactively #'agent-shell-set-session-model)))
      ('queue (+agent-manager-hide) (+agent-acp-show-buffer buffer)
              (+agent-queue-resume buffer)))
    (when (eq (current-buffer) (get-buffer +agent-manager-buffer-name))
      (+agent-manager-refresh))))

(dolist (pair '((stop . +agent-manager-stop) (close . +agent-manager-end)
                (rename . +agent-manager-rename) (restart . +agent-manager-restart)))
  (let ((operation (car pair)))
    (defalias (cdr pair) (lambda () (interactive) (+agent-manager-action operation)))))

(defun +agent-manager--state-label (session)
  "A quiet, human readable label for SESSION's separate state facts."
  (let ((state (+agent-sessions--state session)))
    (or (cdr (assoc state '(("approval" . "Approval needed") ("answer" . "Answer needed")
                           ("stop-unconfirmed" . "Stop unconfirmed")
                           ("running" . "Working") ("starting" . "Starting")
                           ("stopping" . "Stopping") ("dead" . "Process ended")
                           ("disconnected" . "Disconnected") ("failed" . "Failed")
                           ("idle" . "Ready")))) state)))

(defun +agent-manager--state-color (session)
  (aaron-ui-color
   (pcase (+agent-sessions--state session)
     ((or "approval" "answer" "stopping") 'accent-yellow)
     ((or "dead" "disconnected" "failed" "stop-unconfirmed") 'accent-red-strong)
     ("running" 'accent-cyan) (_ 'fg-muted))))

(defun +agent-manager--activity (session)
  "One line saying what SESSION is about, from its reported title or last prompt."
  (or (plist-get session :title) (plist-get session :last-prompt)))

(defun +agent-manager--status-line (session)
  "SESSION's state with its elapsed time and queue."
  (string-join (cons (+agent-manager--state-label session)
                     (+agent-session-progress session))
               " · "))

(defun +agent-manager--usage-line (session)
  "Display only reported usage, without treating unknown values as zero."
  (let* ((usage (plist-get session :usage))
         (size (plist-get usage :context-size))
         (used (plist-get usage :context-used))
         (cost (plist-get usage :cost)))
    (format "Context %s%s"
            (if (and (numberp size) (> size 0) (numberp used))
                (format "%d%%" (round (* 100.0 (/ (float used) size)))) "—")
            (if (numberp cost) (format "  ·  %s %.2f" (or (plist-get usage :currency) "Cost") cost) ""))))

(etaf-define-component +agent-manager-row (&key session selected owner usage-view)
  "A selectable native-buffer preview; keyboard activation confirms entry."
  :render
  (let* ((buffer (plist-get session :buffer))
         (active (eq selected buffer))
         (usage (plist-get session :usage))
         (activity (+agent-manager--activity session)))
    (etaf-view
     (column :width 'stretch
             :on-focus-enter (lambda () (with-current-buffer owner
                                          (unless (eq buffer +agent-manager--selected)
                                            (+agent-manager-select buffer))))
      (etaf-button
      :ref (+agent-manager--ref buffer) :class "agent-session" :variant 'ghost
      :aria-selected active :padding '((lh 0.5) (ch 1))
      :bgcolor (aaron-ui-color (if active 'bg-surface-strong 'bg-base))
      :label (or (plist-get session :name) (buffer-name buffer))
      :on-press (lambda ()
                  (let ((pointer (etaf-event-pointer-p)))
                    (with-current-buffer owner
                      (+agent-manager-select buffer)
                      (unless pointer (+agent-manager-open nil buffer)))))
      (column :width 'stretch :item-gap '(lh 0)
        (row :width 'stretch :item-gap '(ch 1)
          (etaf-label :text (if (plist-get session :unread-kind) "●" "·")
                      :color (+agent-manager--state-color session))
          (etaf-label :text (or (plist-get session :name) (buffer-name buffer))
                      :variant 'strong :wrap-mode 'word))
        (etaf-label :text (format "  %s · %s"
                                 (or (plist-get session :agent) "agent")
                                 (+agent-manager--status-line session))
                    :color (+agent-manager--state-color session) :wrap-mode 'word)
        (expr (when activity
                (etaf-view
                 (etaf-label :text (concat "  " (truncate-string-to-width activity 40 nil nil "…"))
                             :wrap-mode 'word))))
        (etaf-label :text (concat "  "
                                 (if usage-view (+agent-manager--usage-line session)
                                   (concat (or (plist-get (plist-get session :group) :label) "Interactive")
                                           " · " (or (plist-get usage :model) "Model pending"))))
                    :variant 'muted :wrap-mode 'word))))))
  :styles ((".agent-session" :width stretch)))

(etaf-define-component +agent-manager-usage-detail (&key session)
  "A read-only usage page sourced from the selected session snapshot."
  :render
  (let ((usage (plist-get session :usage)))
    (etaf-view
     (column :width 'stretch :item-gap '(lh 1) :padding '((lh 0.5) (ch 2))
       (etaf-label :text (or (plist-get session :name) "Choose a session") :variant 'heading)
       (etaf-label :text (or (plist-get usage :model) (plist-get usage :model-id) "Model not reported")
                   :variant 'muted :wrap-mode 'word)
       (+agent-ui-context :usage usage)
       (flex :width 'stretch :flex-wrap 'wrap :gap '((lh 0.5) (ch 1))
         (+agent-ui-metric :label "Input tokens" :value (+agent-ui-usage-count usage :input))
         (+agent-ui-metric :label "Output tokens" :value (+agent-ui-usage-count usage :output))
         (+agent-ui-metric :label "Cache read" :value (+agent-ui-usage-count usage :cached-read))
         (+agent-ui-metric :label "Cache write" :value (+agent-ui-usage-count usage :cached-write))
         (+agent-ui-metric :label "Reported cost" :value (+agent-ui-cost usage))
         (+agent-ui-metric :label "Turns" :value (+agent-ui-count (plist-get usage :turns))))
       (etaf-panel :title "Session details" :variant 'flat
         (column :width 'stretch :item-gap '(lh 0.5)
           (etaf-label :text (+agent-manager--status-line session)
                       :color (+agent-manager--state-color session) :wrap-mode 'word)
           (etaf-label :text (or (plist-get session :root) "Workspace not reported")
                       :variant 'muted :wrap-mode 'word)
           (etaf-label :text (format "Context peak %s · Compactions %s"
                                    (+agent-ui-usage-count usage :context-peak)
                                    (+agent-ui-count (plist-get usage :compactions)))
                       :variant 'muted :wrap-mode 'word)))
       (etaf-label :text "Reported by this session. — means unavailable; quotas remain in Detailed usage."
                   :variant 'muted :wrap-mode 'word)))))

(etaf-define-component +agent-manager-page (&key model owner)
  "Compact workspace navigation beside the original interactive chat."
  :setup (aaron-ui-etaf-provide-theme)
  :render
  (let* ((_theme (etaf-value (aaron-ui-etaf-theme-ref)))
         (data (etaf-value model))
         (sessions (plist-get data :sessions))
         (selected (plist-get data :selected))
         (session (seq-find (lambda (s) (eq selected (plist-get s :buffer))) sessions))
         (groups (seq-group-by #'+agent-manager--group sessions))
         (view (or (plist-get data :view) 'sessions))
         (height (max 4 (- (or (cdr (etaf-viewport)) 30) 12))))
    (etaf-view
     (column :width '(vw 100) :height '(vh 100) :overflow 'hidden
             :padding '((lh 1) (ch 1)) :item-gap '(lh 0.5)
             :on-dismiss (lambda () (with-current-buffer owner (+agent-manager-hide)))
             :help "Select a session to preview its original chat. RET enters it; q returns to the editor."
             :keys '(("j" +agent-manager-next "Next session")
                     ("k" +agent-manager-previous "Previous session")
                     ("q" +agent-manager-hide "Return to editor")
                     ("n" +agent-manager-new "New session in selected workspace")
                     ("i" +agent-manager-input "Write in selected session")
                     ("a" +agent-manager-review "Review pending request")
                     ("." +agent-session-menu "Session actions")
                     ("C-c C-z" +agent-session-menu "Session actions")
                     ("/" +agent-manager-search "Filter sessions")
                     ("p" +agent-manager-toggle-project "Current project / all")
                     ("z" +agent-manager-toggle-group "Fold selected workspace")
                     ("g" +agent-manager-refresh "Refresh")
                     ("h" +agent-manager-resume "Native history")
                     ("Q" +agent-manager-queue "Manage queued prompts")
                     ("l" +agent-manager-changes "Open a changed file")
                     ("s" +agent-manager-stop "Stop turn")
                     ("r" +agent-manager-rename "Rename session")
                     ("R" +agent-manager-restart "Restart session")
                     ("x" +agent-manager-end "End session"))
       (row :width 'stretch :item-gap '(ch 4)
         (etaf-label :text (pcase view ('usage "Usage") ('attention "Inbox") (_ "Sessions"))
                     :variant 'heading)
         (etaf-button :label "+ New · n" :variant 'secondary
                      :on-press (lambda () (with-current-buffer owner (+agent-manager-new)))))
       (etaf-segmented
        :value view :options `((sessions . ,(format "All %d" (plist-get data :total)))
                              (attention . ,(format "Inbox %d" (plist-get data :attention)))
                              (usage . "Usage"))
        :on-change (lambda (next) (with-current-buffer owner (+agent-manager-view next))))
       (etaf-button :ref 'agent-manager-filter
                    :label (if (string-empty-p (plist-get data :query))
                               "Filter sessions…  /"
                             (concat "Filter: " (plist-get data :query)))
                    :variant 'ghost
                    :on-press
                    (lambda ()
                      (with-current-buffer owner
                        (+agent-manager-search
                         (etaf-call-with-input
                          (lambda () (read-string "Filter agents: " +agent-manager--query)))))))
       (expr (when (eq (plist-get data :scope) 'project)
               (etaf-view (etaf-label :text "This project · p to show all" :variant 'muted))))
       (row :width 'stretch :height (list 'lh height) :item-gap '(ch 2) :overflow 'hidden
       (column :ref 'agent-manager-list :width (if (eq view 'usage) '(ch 38) 'stretch)
               :height 'stretch :overflow 'scroll :item-gap '(lh 1)
         (expr
          (if (null sessions)
              (etaf-view
               (etaf-empty :title (if (eq view 'attention) "All caught up" "No sessions here")
                           :description (if (eq view 'attention)
                                            "Requests, failures and unread results appear here."
                                          "Start a session with n, or change the filter.")))
            (mapcar
             (lambda (group)
               (let* ((key (car group))
                      (root (or (cadr key) "workspace"))
                      (target (car key))
                      (folded (and (string-empty-p (plist-get data :query))
                                   (member key (plist-get data :collapsed))))
                      (title (file-name-nondirectory (directory-file-name root))))
                 (etaf-view
                  (column :key (format "%S" key) :width 'stretch :item-gap '(lh 0)
                    (etaf-button
                     :label (format "%s %s  %d" (if folded "▸" "▾") title (length (cdr group)))
                     :variant 'ghost :font-weight 'bold
                     :help-echo (format "%s · %s" target root)
                     :on-press (lambda () (with-current-buffer owner (+agent-manager-toggle-group key))))
                    (etaf-label :text (truncate-string-to-width (format "  %s · %s" (or target "local") (abbreviate-file-name root)) 40 nil nil "…")
                                :variant 'muted :wrap-mode 'word)
                    (expr (unless folded
                            (mapcar (lambda (s)
                                      (etaf-view (+agent-manager-row
                                                  :key (+agent-manager--ref (plist-get s :buffer))
                                                  :session s :selected selected :owner owner
                                                  :usage-view (eq view 'usage))))
                                    (cdr group)))))))) groups))))
       (expr
        (when (eq view 'usage)
          (etaf-view
           (column :width 'stretch :height 'stretch :overflow 'scroll
             (expr (if session
                       (etaf-view (+agent-manager-usage-detail :session session))
                     (etaf-view
                      (etaf-empty :title "No usage to display"
                                  :description "Choose a live session to see its reported usage.")))))))))
       (expr
        (when (and session (not (eq view 'usage)))
          (etaf-view
           (column :width 'stretch :item-gap '(lh 0)
             (etaf-label :text (if (eq view 'usage) (+agent-manager--usage-line session)
                                (+agent-manager--status-line session))
                         :color (+agent-manager--state-color session) :wrap-mode 'word)
             (expr
              (when-let* ((detail (or (plist-get session :request)
                                      (+agent-session-error-text session))))
                (etaf-view (etaf-label :text detail :wrap-mode 'word))))
             (expr
              (when (memq (plist-get session :attention-kind) '(permission input))
                (etaf-view
                 (etaf-button :label "Review request · a" :variant 'ghost
                              :on-press (lambda () (with-current-buffer owner (+agent-manager-review)))))))))))
       (etaf-button :label "Actions  ·" :variant 'ghost :disabled (not selected)
                    :on-press (lambda () (with-current-buffer owner (call-interactively #'+agent-session-menu))))
       (etaf-label :text (if (eq view 'usage) "j/k select · RET open · q return"
                          "j/k preview · RET open · q return") :variant 'muted)))))

(defun +agent-manager-show (&optional view project)
  "Show VIEW in the shared manager, optionally restricted to PROJECT."
  (let* ((visible (+agent-manager--visible-p))
         (panel (and (featurep 'aaron-ui-float)
                     (when-let* ((buffer (get-buffer +agent-manager-buffer-name)))
                       (aaron-ui-float-panel-for-buffer buffer))))
         (fresh-source (not (and panel (eq (selected-frame) (aaron-ui-float-panel-frame panel)))))
         (origin (selected-window))
         (source-agent (and (+agent-acp-agent-buffer-p (current-buffer)) (current-buffer)))
         (directory default-directory)
         (target (remote-file-name-target directory))
         (root (or (and source-agent (buffer-local-value '+agent-acp-session-root source-agent))
                   (+agent-acp-project-root))))
    (if (not (and (display-graphic-p)
                  (require 'aaron-ui-float nil t) (posframe-workable-p)))
        (+agent/sessions-table project (eq view 'attention))
      (let* ((existing (get-buffer +agent-manager-buffer-name))
             (buffer (or existing (get-buffer-create +agent-manager-buffer-name))))
        (condition-case err
            (progn
              (with-current-buffer buffer
                (unless (and +agent-manager--model (etaf-runtime-for-buffer buffer))
                  (let ((model (etaf-ref (list :scope 'all :query "" :total 0 :attention 0))))
                    (etaf-mount buffer (etaf-view (+agent-manager-page :model model :owner buffer)))
                    (setq-local +agent-manager--model model)
                    (add-hook 'kill-buffer-hook #'+agent-manager--killed-h nil t)))
                (when (or (not visible) fresh-source)
                  (unless (and (equal root +agent-manager--root) (equal target +agent-manager--target))
                    (setq +agent-manager--query "" +agent-manager--selected nil))
                  (setq-local +agent-manager--origin origin +agent-manager--root root
                              +agent-manager--target target
                              +agent-manager--directory directory default-directory directory)
                  (setq +agent-manager--selected
                        (or source-agent
                            (and (+agent-acp-agent-buffer-p +agent-manager--selected)
                                 (equal root (buffer-local-value '+agent-acp-session-root +agent-manager--selected))
                                 +agent-manager--selected)
                            (plist-get (car (+agent-pool-sessions root target)) :buffer))))
                (when view
                  (setq +agent-manager--view view
                        +agent-manager--scope (cond ((eq view 'attention) 'attention)
                                                   (project 'project) (t 'all))))
                (+agent-manager-refresh))
              (let ((panel (aaron-ui-float-show
                            buffer :key 'agent-manager :title "Agents"
                            :width '(fraction 0.9) :height '(fraction 0.84)
                            :owned t :on-hide #'+agent-manager--suspend)))
                (with-current-buffer buffer
                  (add-hook '+agent-acp-changed-functions #'+agent-manager--changed-h)
                  (setq +agent-manager--watching t)
                  (+agent-manager--preview)
                  (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
                  (when +agent-manager--selected
                    (etaf-focus (etaf-runtime-for-buffer buffer)
                                (+agent-manager--ref +agent-manager--selected))))
                panel))
          (error
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (+agent-manager--suspend))
             (when-let* ((panel (aaron-ui-float-panel-for-buffer buffer)))
               (aaron-ui-float-hide panel)))
           (unless existing (when (buffer-live-p buffer) (kill-buffer buffer)))
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun +agent/manager (&optional all-projects)
  "Open the project pool manager; toggle it when it already has focus.
With prefix ALL-PROJECTS start in the cross-project overview."
  (interactive "P")
  (let* ((buffer (get-buffer +agent-manager-buffer-name))
         (panel (and buffer (featurep 'aaron-ui-float)
                     (aaron-ui-float-panel-for-buffer buffer))))
    (if (and (+agent-manager--visible-p) panel
             (eq (selected-frame) (aaron-ui-float-panel-frame panel)))
        (with-current-buffer buffer (+agent-manager-hide))
      (+agent-manager-show 'sessions (not all-projects)))))

;;;###autoload
(defun +agent/usage (&optional project)
  "Show session usage in the shared daily management surface."
  (interactive "P")
  (+agent-manager-show 'usage project))

(defalias 'agent-manager #'+agent/manager)

(provide 'tools/agent/manager)
;;; manager.el ends here
