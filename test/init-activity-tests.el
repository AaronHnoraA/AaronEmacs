;;; init-activity-tests.el --- Activity lifecycle regressions -*- lexical-binding: t -*-

(require 'ert)
(require 'compile)
(require 'comp-run)
(require 'aaron-ui-activity)
(require 'noema/activity/config)
(require 'config/performance/config)
(require 'remote-task)
(require 'remote-transfer)
(require 'remote-board)

(defvar +noema--ready nil)
(defvar +noema--process nil)
(defvar remote-tasks nil)
(defvar remote-transfer-jobs nil)
(defvar features nil)

(defmacro +activity-test-isolated (&rest body)
  "Run BODY without starting Noema or retaining indicator timers."
  (declare (indent 0))
  `(let ((+activity-indicator-mode nil)
         (+activity--registered nil)
         (+activity--timer nil)
         (+activity--count 0)
         (+activity--frame-index 0)
         (+activity--last-poll 0)
         (+activity--provider-errors (make-hash-table :test #'equal))
         (+activity-poll-functions nil)
         (+activity-noema--agents (make-hash-table :test #'eq))
         (+activity-noema--tasks nil)
         (+activity-noema--pending nil)
         (compilation-in-progress nil)
         (comp-async-compilations nil)
         (comp-files-queue nil)
         (remote-tasks nil)
         (remote-transfer-jobs nil)
         (+noema--ready nil)
         (+noema--process nil))
     (unwind-protect (progn ,@body)
       (when (timerp +activity--timer)
         (cancel-timer +activity--timer)))))

(ert-deftest +activity-row-keys-work-including-unbound-help ()
  (+activity-test-isolated
    (with-temp-buffer
      (+activity-board-mode)
      (let ((inhibit-read-only t))
        (aaron-ui-board-insert-row :id "process" :title "Process"
                                   :detail "Command" :action #'ignore))
      (goto-char (point-min))
      (should buffer-read-only)
      (should (eq (key-binding "j") #'next-line))
      (should (eq (key-binding "k") #'previous-line))
      (should (eq (key-binding "x") #'+activity-board/stop))
      (should (eq (key-binding "z") #'+activity-board/help))
      (let ((inhibit-read-only nil))
        (call-interactively (key-binding "j"))
        (call-interactively (key-binding "z")))
      (should (= (line-number-at-pos) 2)))))

(ert-deftest +activity-dead-board-cancels-its-own-refresh-timer ()
  "A queued callback releases its timer even after the buffer is gone."
  (let ((buffer (generate-new-buffer " *activity timer test*"))
        timer)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (+activity-board-mode)
            (+activity-board/toggle-auto-refresh)
            (setq timer +activity-board--auto-timer)
            ;; Simulate teardown that bypassed the normal kill hook.
            (remove-hook 'kill-buffer-hook #'+activity-board--cleanup t))
          (kill-buffer buffer)
          (should (memq timer timer-list))
          (funcall (timer--function timer))
          (should-not (memq timer timer-list)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (timerp timer)
        (cancel-timer timer)))))

(ert-deftest +activity-hidden-board-stops-auto-refresh ()
  "A board hidden with native `quit-window' stops waking up on a timer."
  (+activity-test-isolated
    (with-temp-buffer
      (+activity-board-mode)
      (+activity-board/toggle-auto-refresh)
      (let ((timer +activity-board--auto-timer))
        (should (memq timer timer-list))
        (funcall (timer--function timer))
        (should-not +activity-board--auto-timer)
        (should-not (memq timer timer-list))))))

(ert-deftest +activity-and-monitor-share-one-buffer ()
  "The two entry points select pages in one live monitor buffer."
  (+activity-test-isolated
    (let ((source (generate-new-buffer " *shared monitor source*"))
          board)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (+performance/watch)
            (setq board (current-buffer))
            (should (eq (etaf-value +performance--tab-state) 'overview))
            (+activity/board)
            (should (eq (current-buffer) board))
            (should (eq (etaf-value +performance--tab-state) 'activity))
            (should (string-match-p "Built-in managers" (buffer-string))))
        (when (buffer-live-p board) (kill-buffer board))
        (kill-buffer source)))))

(ert-deftest +activity-pikachu-opens-monitor-in-shared-float ()
  "The mode-line click selects Activity in the monitor's Aaron UI float."
  (require 'aaron-ui-float)
  (let ((source (generate-new-buffer " *activity float source*"))
        shown board)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'posframe-workable-p) (lambda () t))
                  ((symbol-function 'aaron-ui-float-show)
                   (lambda (item &rest options)
                     (setq shown (cons item options))))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (&rest _) (ert-fail "Activity replaced a window"))))
          (should (eq (lookup-key +activity--mode-line-map
                                  [mode-line mouse-1])
                      #'+activity/board))
          (with-current-buffer source
            (+activity/board))
          (setq board (car shown))
          (should (eq board (get-buffer +performance-buffer-name)))
          (should (plist-get (cdr shown) :owned)))
      (when (buffer-live-p board) (kill-buffer board))
      (kill-buffer source))))

(ert-deftest +activity-visible-float-keeps-auto-refresh-until-hidden ()
  "The refresh timer treats a visible child frame as an open Activity page."
  (require 'aaron-ui-float)
  (+activity-test-isolated
    (with-temp-buffer
      (+activity-board-mode)
      (let ((visible t)
            (refreshed 0))
        (cl-letf (((symbol-function 'aaron-ui-float-panel-for-buffer)
                   (lambda (_buffer) 'activity-panel))
                  ((symbol-function 'aaron-ui-float-visible-p)
                   (lambda (_panel) visible))
                  ((symbol-function '+activity-board/refresh)
                   (lambda () (cl-incf refreshed))))
          (+activity-board/toggle-auto-refresh)
          (let ((timer +activity-board--auto-timer))
            (unwind-protect
                (progn
                  (funcall (timer--function timer))
                  (should (= refreshed 1))
                  (should (eq +activity-board--auto-timer timer))
                  (setq visible nil)
                  (funcall (timer--function timer))
                  (should-not +activity-board--auto-timer))
              (cancel-timer timer))))))))

(ert-deftest +activity-native-manager-opens-after-floating-monitor-hides ()
  "A manager opened from Activity returns to the parent frame first."
  (require 'aaron-ui-float)
  (let (hidden)
    (cl-letf (((symbol-function 'aaron-ui-float-panel-for-buffer)
               (lambda (_buffer) 'shared-monitor))
              ((symbol-function 'aaron-ui-float-visible-p)
               (lambda (_panel) t))
              ((symbol-function 'aaron-ui-float-hide)
               (lambda (_panel) (setq hidden t))))
      (with-temp-buffer
        (+activity-board--open-native-manager
         (lambda () (interactive) (should hidden)))))))

(ert-deftest +activity-board-quit-returns-and-reopen-keeps-filter ()
  "TTY window return keeps the shared monitor and its task filter."
  (+activity-test-isolated
    (let ((source (generate-new-buffer " *activity source*"))
          board)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (+activity/board)
            (setq board (current-buffer))
            (should (equal (buffer-name board) +performance-buffer-name))
            (should (etaf-runtime-for-buffer board))
            (should (eq (etaf-value +performance--tab-state) 'activity))
            (should (eq (key-binding "j") #'etaf-focus-next))
            (should (string-match-p "Built-in managers" (buffer-string)))
            (setq-local +activity-board--filter "process")
            (call-interactively (key-binding "q"))
            (should (eq (window-buffer (selected-window)) source))
            (+activity/board)
            (should (eq (current-buffer) board))
            (should (equal +activity-board--filter "process")))
        (when (buffer-live-p board) (kill-buffer board))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest +activity-board-first-render-failure-releases-new-buffer ()
  "A failed first open must not leave a stale shared monitor buffer."
  (+activity-test-isolated
    (let ((+performance-buffer-name " *failed shared monitor*")
          created)
      (cl-letf (((symbol-function 'etaf-mount)
                 (lambda (buffer &rest _)
                   (setq created buffer)
                   (error "render failed"))))
        (should-error (+activity/board)))
      (should created)
      (should-not (buffer-live-p created))
      (should-not (get-buffer +performance-buffer-name)))))

(ert-deftest +activity-process-details-quit-returns-to-board-and-kills-page ()
  "The custom details page follows native `quit-window' without leaking."
  (+activity-test-isolated
    (let ((source (generate-new-buffer " *activity detail source*"))
          (process (make-pipe-process :name "activity-detail-probe" :noquery t))
          board detail)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (+activity/board)
            (setq board (current-buffer))
            (let ((ref (intern (format "activity-process-%s"
                                       (process-name process)))))
              (should (eq (gethash ref +activity-board--entries) process))
              (etaf-focus nil ref)
              (should (eq (key-binding "x") #'+activity-board/stop))
              (should (eq (key-binding "j") #'etaf-focus-next))
              (+activity-board/describe))
            (setq detail (current-buffer))
            (should (eq detail
                        (buffer-local-value '+activity-board--detail-buffer board)))
            (should (buffer-local-value 'quit-window-kill-buffer detail))
            (should-not (buffer-modified-p detail))
            (should (eq (key-binding "q") #'quit-window))
            (call-interactively (key-binding "q"))
            (should (eq (current-buffer) board))
            (should-not (buffer-live-p detail)))
        (when (buffer-live-p detail) (kill-buffer detail))
        (when (buffer-live-p board) (kill-buffer board))
        (when (buffer-live-p source) (kill-buffer source))
        (delete-process process)))))

(ert-deftest +activity-process-details-are-owned-by-their-board ()
  "Two Activity views cannot overwrite one another's details page."
  (let ((first (generate-new-buffer " *activity details first*"))
        (second (generate-new-buffer " *activity details second*"))
        first-detail second-detail)
    (unwind-protect
        (progn
          (with-current-buffer first
            (+activity-board-mode)
            (setq first-detail (+activity-board--ensure-detail-buffer)))
          (with-current-buffer second
            (+activity-board-mode)
            (setq second-detail (+activity-board--ensure-detail-buffer)))
          (should-not (eq first-detail second-detail))
          (kill-buffer first)
          (should-not (buffer-live-p first-detail))
          (should (buffer-live-p second-detail)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (when (buffer-live-p first-detail) (kill-buffer first-detail))
      (when (buffer-live-p second-detail) (kill-buffer second-detail)))))

(ert-deftest +activity-transfer-pages-keep-queue-keys-under-evil ()
  "The transfer queue and log must resolve native exit and job actions."
  (with-temp-buffer
    (remote-transfer-list-mode)
    (should (eq (key-binding "q") #'quit-window))
    (should (eq (key-binding (kbd "RET")) #'remote-transfer-show-log))
    (should (eq (key-binding "k") #'remote-transfer-list-cancel)))
  (with-temp-buffer
    (remote-transfer-log-mode)
    (should (eq (key-binding "q") #'quit-window))))

(ert-deftest +activity-transfer-queue-and-log-quit-return-under-evil ()
  "The actual q key returns through queue and log in the full config."
  (let ((source (generate-new-buffer " *transfer full-init source*"))
        queue log)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (remote-transfer-list)
          (setq queue (current-buffer))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (remote-transfer-list)
          (should (eq (current-buffer) queue))
          (setq log (generate-new-buffer " *transfer full-init log*"))
          (with-current-buffer log (remote-transfer-log-mode))
          (pop-to-buffer log)
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) queue)))
      (when (buffer-live-p log) (kill-buffer log))
      (when (buffer-live-p queue) (kill-buffer queue))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +activity-remote-board-keys-and-reopen-under-evil ()
  "Remote board actions resolve in Emacs state and q returns to the source."
  (let ((source (generate-new-buffer " *remote board source*"))
        board)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (remote-board)
          (setq board (current-buffer))
          (should (eq (key-binding "q") #'quit-window))
          (should (eq (key-binding (kbd "RET")) #'remote-open-target))
          (should (eq (key-binding "c") #'remote-board-close-workspace))
          (setq-local tabulated-list-sort-key '("ID" . nil))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (remote-board)
          (should (eq (current-buffer) board))
          (should (equal tabulated-list-sort-key '("ID" . nil))))
      (when (buffer-live-p board) (kill-buffer board))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +activity-remote-logs-quit-under-evil ()
  "Remote history logs return to their caller and release temporary views."
  (let ((source (generate-new-buffer " *remote log source*"))
        (routes nil)
        (connection nil))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (remote-route-log-buffer)
          (setq routes (current-buffer))
          (should (eq major-mode 'remote-board-log-mode))
          (should (eq (key-binding "q") #'quit-window))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (should-not (buffer-live-p routes))
          (remote-board-connection-log "local")
          (setq connection (current-buffer))
          (should (eq (key-binding "q") #'quit-window))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (should-not (buffer-live-p connection))
          (with-temp-buffer
            (remote-board-log-mode)
            (should (eq (key-binding "q") #'quit-window))))
      (when (buffer-live-p routes) (kill-buffer routes))
      (when (buffer-live-p connection) (kill-buffer connection))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +activity-remote-machines-keys-and-reopen-under-evil ()
  "The temporary-machine list uses its own keys and preserves frame sort."
  (let ((source (generate-new-buffer " *remote machines source*"))
        machines)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (remote-machines)
          (setq machines (current-buffer))
          (should (eq (key-binding "q") #'quit-window))
          (should (eq (key-binding (kbd "RET")) #'remote-machines-open))
          (should (eq (key-binding "e") #'remote-machines-edit))
          (should (eq (key-binding "p")
                      #'remote-machines-forget-password))
          (should (eq (key-binding "g")
                      #'remote-machines-reload-from-disk))
          (setq-local tabulated-list-sort-key '("ID" . nil))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (remote-machines)
          (should (eq (current-buffer) machines))
          (should (equal tabulated-list-sort-key '("ID" . nil))))
      (when (buffer-live-p machines) (kill-buffer machines))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +activity-remote-doctor-refresh-and-quit-under-evil ()
  "Doctor opens at the top, refreshes in place and q returns to its source."
  (let ((source (generate-new-buffer " *remote doctor source*"))
        doctor)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (remote-doctor "local")
          (setq doctor (current-buffer))
          (should (eq major-mode 'remote-doctor-mode))
          (should (eq (key-binding "q") #'quit-window))
          (should (eq (key-binding "g") #'remote-doctor-refresh))
          (should (= (point) (point-min)))
          (goto-char (point-max))
          (call-interactively (key-binding "g"))
          (should (eq (current-buffer) doctor))
          (should (= (point) (point-min)))
          (call-interactively (key-binding "q"))
          (should (eq (current-buffer) source))
          (should-not (buffer-live-p doctor)))
      (when (buffer-live-p doctor) (kill-buffer doctor))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest +activity-board-stop-waits-for-async-owner-cleanup ()
  "Requesting cancellation must not hide work before its owner finishes."
  (+activity-test-isolated
    (let (cancelled)
      (let ((activity
             (+activity-register
              "Export" :cancel (lambda () (setq cancelled t)))))
        (with-temp-buffer
          (insert (propertize "Export" '+activity-handle activity))
          (goto-char (point-min))
          (cl-letf (((symbol-function '+activity-board/refresh) #'ignore))
            (+activity-board/stop)))
        (should cancelled)
        (should (= 1 (+activity-count)))
        (should (memq activity +activity--registered))
        (+activity-unregister activity)
        (should (zerop (+activity-count)))))))

(ert-deftest +activity-claims-bulk-transfers-as-tasks ()
  "A transfer's task row uses its owner for logging and cancellation."
  (+activity-test-isolated
    (let* ((temporary (generate-new-buffer " *activity temp stdout*"))
           (process (make-process :name "transfer" :buffer temporary
                                  :command '("sleep" "30") :noquery t))
           (log (generate-new-buffer " *activity transfer log*"))
           (job (remote-transfer-job--create
                 :process process :name "Upload a.bin" :percent 42
                 :log-buffer log))
           (cancelled 0))
      (unwind-protect
          (cl-letf (((symbol-function 'remote-transfer-cancel)
                     (lambda (owner)
                       (should (eq owner job))
                       (cl-incf cancelled)))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'signal-process)
                     (lambda (&rest _) (ert-fail "Owner process was killed directly"))))
            ;; Unclaimed, the process is only a service.
            (should (eq (+activity-process-kind process) 'service))
            (setq remote-transfer-jobs (list job))
            (should (eq (+activity-process-kind process) 'task))
            (should (equal (+activity--process-label process)
                           "42% Upload a.bin"))
            (with-temp-buffer
              (insert (propertize "row" '+activity-process process))
              (goto-char (point-min))
              (cl-letf (((symbol-function '+activity-board/refresh)
                         #'ignore))
                (+activity-board/stop)
                (+activity-board/interrupt)
                (+activity-board/kill))
              (should (= cancelled 3))
              (save-window-excursion
                (+activity-board/visit)
                (should (eq (current-buffer) log)))))
        (delete-process process)
        (kill-buffer temporary)
        (kill-buffer log)))))

(ert-deftest +activity-remote-task-interrupt-uses-owner ()
  "The remote task owner must cancel target work, not only its relay."
  (+activity-test-isolated
    (let* ((process (make-process :name "remote-task" :buffer nil
                                  :command '("sleep" "30") :noquery t))
           (task (remote-task-create :process process :name "Build"))
           (remote-tasks (make-hash-table :test #'equal))
           (cancelled 0))
      (unwind-protect
          (progn
            (puthash "build" task remote-tasks)
            (with-temp-buffer
              (insert (propertize "row" '+activity-process process))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'remote-task-cancel)
                         (lambda (owner &optional _)
                           (should (eq owner task))
                           (cl-incf cancelled)))
                        ((symbol-function '+activity-board/refresh) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'signal-process)
                         (lambda (&rest _)
                           (ert-fail "Remote relay was killed directly"))))
                (+activity-board/interrupt)
                (+activity-board/kill)))
            (should (= cancelled 2)))
        (delete-process process)))))

(ert-deftest +activity-registration-cleans-up-on-return-and-error ()
  (+activity-test-isolated
    (should (= 42 (+with-activity "work"
                    (should (= 1 (+activity-count))) 42)))
    (should (= 0 (+activity-count)))
    (should-error (+with-activity "failed work" (error "failed")))
    (should-not +activity--registered)
    (should (= 0 +activity--count))))

(ert-deftest +activity-animation-advances-and-returns-to-rest ()
  (+activity-test-isolated
    (let ((+activity-indicator-mode t)
          (+activity-indicator-fps 20))
      (let* ((handle (+activity-register "animation probe"))
             (initial +activity--frame-index))
        (should (timerp +activity--timer))
        (sleep-for 0.18)
        (should (> +activity--frame-index initial))
        (should (= 1 +activity--count))
        (+activity-unregister handle)
        (sleep-for 0.18)
        (should (= 0 +activity--count))
        ;; A rest glyph ignores the former frame number immediately.
        (should (equal (substring-no-properties (+activity-mode-line-text))
                       " ᕕ( ᴗ )ᕗ"))))))

(ert-deftest +activity-empty-task-snapshot-does-not-rescan-per-process ()
  (+activity-test-isolated
    (let ((process (make-pipe-process :name "activity-service" :noquery t))
          (scans 0))
      (unwind-protect
          (cl-letf (((symbol-function '+activity--owned-task-processes)
                     (lambda () (cl-incf scans) nil)))
            (dotimes (_ 10)
              (should (eq (+activity-process-kind process nil) 'service)))
            (should (= scans 0))
            (should (eq (+activity-process-kind process) 'service))
            (should (= scans 1)))
        (delete-process process)))))

(ert-deftest +activity-noema-counts-busy-agents-without-idle-or-export-duplicates ()
  (+activity-test-isolated
    (let ((features (cons 'noema-agent-acp features))
          (worker (generate-new-buffer " *activity-worker*"))
          (export (generate-new-buffer " *activity-export*"))
          (idle (generate-new-buffer " *activity-idle*"))
          (busy t)
          (stops nil))
      (unwind-protect
          (cl-letf (((symbol-function 'noema-agent-acp-sessions)
                     (lambda (&optional _)
                       (list (list :buffer worker :name "codex/work" :busy busy)
                             (list :buffer export :origin 'latex-export :busy t)
                             (list :buffer idle :busy nil))))
                    ((symbol-function 'noema-agent-acp-stop)
                     (lambda (buffer) (push buffer stops))))
            (+activity-noema--update-agents)
            (should (= 1 (+activity-count)))
            (let* ((activity (gethash worker +activity-noema--agents))
                   (since (+activity-started-at activity)))
              (funcall (+activity-cancel activity))
              (should (equal stops (list worker)))
              (+activity-noema--update-agents)
              (should (= since (+activity-started-at
                                (gethash worker +activity-noema--agents)))))
            (setq busy nil)
            (+activity-noema--update-agents)
            (should (= 0 (+activity-count))))
        (mapc #'kill-buffer (list worker export idle))))))

(ert-deftest +activity-noema-polls-asynchronously-and-filters-terminal-tasks ()
  (+activity-test-isolated
    (let ((+noema--ready t)
          (+noema--process 'host)
          (calls 0) callback channel arguments)
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (method args cb &optional _timeout)
                   (cl-incf calls)
                   (setq channel method arguments args callback cb))))
        (+activity-noema--poll)
        (+activity-noema--poll)
        (should (= calls 1))
        (should (equal channel "noema:api:tasks:list"))
        (should (equal arguments [((activeOnly . t))]))
        (funcall callback
                 '((tasks . [((id . "latex") (title . "Export note")
                               (kind . "latex-export") (status . "running")
                               (phase . "Compiling") (cancellable . t))
                              ((id . "done") (status . "completed"))])) nil)
        (should-not +activity-noema--pending)
        (should (= 1 (+activity-count)))
        (should (string-match-p "Compiling"
                                (+activity-detail (car +activity-noema--tasks))))
        (funcall (+activity-cancel (car +activity-noema--tasks)))
        (should (equal channel "noema:api:tasks:cancel"))
        (should (equal arguments [((id . "latex"))]))
        (funcall callback '((ok . t)) nil)
        (should (equal channel "noema:api:tasks:list"))
        (funcall callback '((tasks . [])) nil)
        (should (= 0 (+activity-count)))))))

(ert-deftest +activity-noema-host-stop-and-disable-ignore-stale-replies ()
  (+activity-test-isolated
    (let ((+noema--ready t)
          (+noema--process 'old-host)
          callback)
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (_method _args cb &optional _timeout) (setq callback cb))))
        (+activity-noema--poll)
        (let ((old callback))
          (setq +noema--ready nil)
          (+activity-noema--host-stopped)
          (setq +noema--ready t +noema--process 'new-host)
          (+activity-noema--poll)
          (funcall old '((tasks . [((id . "stale") (status . "running"))])) nil)
          (should +activity-noema--pending)
          (should-not +activity-noema--tasks))
        (+activity-noema--mode-changed)
        (funcall callback '((tasks . [((id . "disabled") (status . "running"))])) nil)
        (should-not +activity-noema--tasks)))))

(ert-deftest +activity-noema-offline-poll-does-not-call-or-start-host ()
  (+activity-test-isolated
    (cl-letf (((symbol-function '+noema-api-call)
               (lambda (&rest _) (ert-fail "Offline poll called the host"))))
      (+activity-noema--poll)
      (should (= 0 (+activity-count))))))

(ert-deftest +activity-noema-unchanged-agent-snapshot-does-not-redraw-board ()
  (+activity-test-isolated
    (let ((features (cons 'noema-agent-acp features))
          (worker (generate-new-buffer " *activity-stable-worker*"))
          (changes 0))
      (unwind-protect
          (cl-letf (((symbol-function 'noema-agent-acp-sessions)
                     (lambda (&optional _)
                       (list (list :buffer worker :name "work" :busy t))))
                    ((symbol-function '+activity-noema--changed)
                     (lambda () (cl-incf changes))))
            (+activity-noema--update-agents)
            (dotimes (_ 5) (+activity-noema--update-agents))
            (should (= 1 changes)))
        (kill-buffer worker)))))

(ert-deftest +activity-noema-updates-visible-shared-monitor ()
  "Noema changes redraw Activity and Processes in the shared monitor."
  (require 'config/performance/config)
  (let ((buffer (get-buffer-create "*Performance Watch*"))
        (refreshes 0))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (setq-local +performance--tab-state (etaf-ref 'activity)))
          (cl-letf (((symbol-function '+activity--refresh) #'ignore)
                    ((symbol-function '+activity-board/refresh)
                     (lambda () (cl-incf refreshes))))
            (+activity-noema--changed)
            (with-current-buffer buffer
              (etaf-set-value +performance--tab-state 'processes))
            (+activity-noema--changed)
            (should (= refreshes 2))))
      (kill-buffer buffer))))

(ert-deftest +activity-noema-hidden-float-does-not-redraw ()
  "A hidden posframe window does not make Noema repaint the monitor."
  (require 'config/performance/config)
  (let ((buffer (get-buffer-create "*Performance Watch*"))
        (features (cons 'aaron-ui-float features)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (setq-local +performance--tab-state (etaf-ref 'activity)))
          (cl-letf (((symbol-function 'aaron-ui-float-panel-for-buffer)
                     (lambda (&rest _) 'hidden-panel))
                    ((symbol-function 'aaron-ui-float-visible-p)
                     (lambda (&rest _) nil))
                    ((symbol-function '+activity--refresh) #'ignore)
                    ((symbol-function '+activity-board/refresh)
                     (lambda () (ert-fail "Hidden monitor repainted"))))
            (+activity-noema--changed)))
      (kill-buffer buffer))))

(ert-deftest +activity-pause-only-signals-local-owned-pid ()
  "Pause and resume signal only a local process without a remote owner."
  (let ((process (make-pipe-process :name "activity-pause-test" :noquery t))
        (status 'run)
        (owner nil)
        signals)
    (unwind-protect
        (cl-letf (((symbol-function '+activity-board--process-at-point)
                   (lambda () process))
                  ((symbol-function 'process-id) (lambda (_) 4312))
                  ((symbol-function 'process-status) (lambda (_) status))
                  ((symbol-function '+activity--remote-task-for)
                   (lambda (_) owner))
                  ((symbol-function '+activity--transfer-job-for)
                   (lambda (_) nil))
                  ((symbol-function 'signal-process)
                   (lambda (_ signal) (push signal signals)))
                  ((symbol-function '+activity-board/refresh) #'ignore))
          (+activity-board/toggle-pause)
          (setq status 'stop)
          (+activity-board/toggle-pause)
          (should (equal signals '(SIGCONT SIGSTOP)))
          (setq owner 'remote)
          (should-error (+activity-board/toggle-pause) :type 'user-error)
          (should (= (length signals) 2)))
      (delete-process process))))

(ert-deftest +activity-noema-failed-request-releases-pending-and-can-recover ()
  (+activity-test-isolated
    (let ((+noema--ready t)
          (+noema--process 'host)
          (calls 0) callback)
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (_method _args cb &optional _timeout)
                   (cl-incf calls) (setq callback cb))))
        (+activity-noema--poll)
        (funcall callback nil '((message . "timeout")))
        (should-not +activity-noema--pending)
        (+activity-noema--poll)
        (should (= 2 calls))
        (funcall callback '((tasks . [((id . "recovered") (status . "running"))])) nil)
        (should (= 1 (+activity-count)))))))

(ert-deftest +activity-providers-poll-at-idle-rate-during-animation ()
  (+activity-test-isolated
    (let* ((calls 0)
          (+activity-poll-functions (list (lambda () (cl-incf calls))))
          (+activity--last-poll (float-time))
          (+activity-poll-interval 2))
      (dotimes (_ 8) (+activity--poll))
      (should (= calls 0))
      ;; Move the recorded poll time back rather than replacing `float-time',
      ;; a C primitive whose mock triggers native trampoline compilation.
      (setq +activity--last-poll (- (float-time) 2.5))
      (+activity--poll)
      (should (= calls 1))
      (dotimes (_ 8) (+activity--poll))
      (should (= calls 1)))))

(ert-deftest +activity-provider-failure-is-visible-and-does-not-stop-peers ()
  "One bad provider cannot hide other work or silently poison the board."
  (+activity-test-isolated
    (let* ((broken t)
           (polled 0)
           (poll (lambda () (when broken (error "poll offline"))))
           (snapshot (lambda () (when broken (error "snapshot offline")) nil))
           (+activity-poll-functions
            (list poll (lambda () (cl-incf polled))))
           (+activity-provider-functions
            (list snapshot (lambda ()
                             (list (+activity--create
                                    :label "Other task" :kind 'task))))))
      (+activity--poll t)
      (should (= polled 1))
      (should (= 1 (+activity-count)))
      (should (= 2 (hash-table-count +activity--provider-errors)))
      (with-temp-buffer
        (+activity-board-mode)
        (+activity-board/refresh)
        (should (string-match-p "Provider problems" (buffer-string)))
        (should (string-match-p "snapshot offline" (buffer-string))))
      (setq broken nil)
      (+activity--poll t)
      (+activity-count)
      (should (zerop (hash-table-count +activity--provider-errors))))))

(ert-deftest +activity-tick-rearms-after-an-unexpected-scan-error ()
  "An unexpected scan exception must not permanently stop the indicator."
  (+activity-test-isolated
    (let ((+activity-indicator-mode t)
          (scheduled 0))
      (cl-letf (((symbol-function '+activity-count)
                 (lambda () (error "scan failed")))
                ((symbol-function '+activity--schedule)
                 (lambda () (cl-incf scheduled))))
        (should-error (+activity--tick))
        (should (= scheduled 1))))))

(provide 'init-activity-tests)
;;; init-activity-tests.el ends here
