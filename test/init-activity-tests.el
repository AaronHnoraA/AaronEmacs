;;; init-activity-tests.el --- Activity lifecycle regressions -*- lexical-binding: t -*-

(require 'ert)
(require 'compile)
(require 'comp-run)
(require 'aaron-ui-activity)
(require 'init-activity-noema)
(require 'remote-task)
(require 'remote-transfer)
(require 'remote-board)

(defvar my/noema--ready nil)
(defvar my/noema--process nil)
(defvar remote-tasks nil)
(defvar remote-transfer-jobs nil)
(defvar features nil)

(defmacro my/activity-test-isolated (&rest body)
  "Run BODY without starting Noema or retaining indicator timers."
  (declare (indent 0))
  `(let ((my/activity-indicator-mode nil)
         (my/activity--registered nil)
         (my/activity--timer nil)
         (my/activity--count 0)
         (my/activity--frame-index 0)
         (my/activity--last-poll 0)
         (my/activity--provider-errors (make-hash-table :test #'equal))
         (my/activity-poll-functions nil)
         (my/activity-noema--agents (make-hash-table :test #'eq))
         (my/activity-noema--tasks nil)
         (my/activity-noema--pending nil)
         (compilation-in-progress nil)
         (comp-async-compilations nil)
         (comp-files-queue nil)
         (remote-tasks nil)
         (remote-transfer-jobs nil)
         (my/noema--ready nil)
         (my/noema--process nil))
     (unwind-protect (progn ,@body)
       (when (timerp my/activity--timer)
         (cancel-timer my/activity--timer)))))

(ert-deftest my/activity-row-keys-work-including-unbound-help ()
  (my/activity-test-isolated
    (with-temp-buffer
      (my/activity-board-mode)
      (let ((inhibit-read-only t))
        (aaron-ui-board-insert-row :id "process" :title "Process"
                                   :detail "Command" :action #'ignore))
      (goto-char (point-min))
      (should buffer-read-only)
      (should (eq (key-binding "j") #'next-line))
      (should (eq (key-binding "k") #'previous-line))
      (should (eq (key-binding "x") #'my/activity-board-stop))
      (should (eq (key-binding "z") #'my/activity-board-help))
      (let ((inhibit-read-only nil))
        (call-interactively (key-binding "j"))
        (call-interactively (key-binding "z")))
      (should (= (line-number-at-pos) 2)))))

(ert-deftest my/activity-dead-board-cancels-its-own-refresh-timer ()
  "A queued callback releases its timer even after the buffer is gone."
  (let ((buffer (generate-new-buffer " *activity timer test*"))
        timer)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (my/activity-board-mode)
            (my/activity-board-toggle-auto-refresh)
            (setq timer my/activity-board--auto-timer)
            ;; Simulate teardown that bypassed the normal kill hook.
            (remove-hook 'kill-buffer-hook #'my/activity-board--cleanup t))
          (kill-buffer buffer)
          (should (memq timer timer-list))
          (funcall (timer--function timer))
          (should-not (memq timer timer-list)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (timerp timer)
        (cancel-timer timer)))))

(ert-deftest my/activity-hidden-board-stops-auto-refresh ()
  "A board hidden with native `quit-window' stops waking up on a timer."
  (my/activity-test-isolated
    (with-temp-buffer
      (my/activity-board-mode)
      (my/activity-board-toggle-auto-refresh)
      (let ((timer my/activity-board--auto-timer))
        (should (memq timer timer-list))
        (funcall (timer--function timer))
        (should-not my/activity-board--auto-timer)
        (should-not (memq timer timer-list))))))

(ert-deftest my/activity-board-view-state-is-frame-owned ()
  "Each frame owns one panel, and closing it releases only that panel."
  (let ((parameters (make-hash-table :test #'equal))
        first second)
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (frame key)
                 (gethash (cons frame key) parameters)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame key value)
                 (puthash (cons frame key) value parameters))))
      (unwind-protect
          (progn
            (setq first (my/activity-board--buffer 'first-frame)
                  second (my/activity-board--buffer 'second-frame))
            (should (buffer-live-p first))
            (should (buffer-live-p second))
            (should-not (eq first second))
            (should (eq first (my/activity-board--buffer 'first-frame)))
            (my/activity-board--frame-deleted 'first-frame)
            (should-not (buffer-live-p first))
            (should (buffer-live-p second))
            (should (eq second (my/activity-board--buffer 'second-frame))))
        (when (buffer-live-p first) (kill-buffer first))
        (when (buffer-live-p second) (kill-buffer second))))))

(ert-deftest my/activity-board-quit-returns-and-reopen-keeps-this-frame-filter ()
  "The board uses native window return and retains only this frame's view."
  (my/activity-test-isolated
    (let ((source (generate-new-buffer " *activity source*"))
          (frame (selected-frame))
          board)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (my/activity-board)
            (setq board (current-buffer))
            (should (eq board (frame-parameter frame 'my/activity-board-buffer)))
            (should (etaf-runtime-for-buffer board))
            (should (eq (key-binding "j") #'etaf-focus-next))
            (should (string-match-p "Built-in managers" (buffer-string)))
            (setq-local my/activity-board--filter "process")
            (call-interactively (key-binding "q"))
            (should (eq (window-buffer (selected-window)) source))
            (my/activity-board)
            (should (eq (current-buffer) board))
            (should (equal my/activity-board--filter "process")))
        (when (buffer-live-p board) (kill-buffer board))
        (when (buffer-live-p source) (kill-buffer source))
        (should-not (frame-parameter frame 'my/activity-board-buffer))))))

(ert-deftest my/activity-board-first-render-failure-releases-new-buffer ()
  "A failed first open must not leave a stale frame-owned panel."
  (my/activity-test-isolated
    (let* ((frame (selected-frame))
           (previous (frame-parameter frame 'my/activity-board-buffer))
           created)
      (unwind-protect
          (progn
            (set-frame-parameter frame 'my/activity-board-buffer nil)
            (cl-letf (((symbol-function 'etaf-mount)
                       (lambda (&rest _)
                         (setq created
                               (frame-parameter frame 'my/activity-board-buffer))
                         (error "render failed"))))
              (should-error (my/activity-board)))
            (should created)
            (should-not (buffer-live-p created))
            (should-not (frame-parameter frame 'my/activity-board-buffer)))
        (set-frame-parameter frame 'my/activity-board-buffer previous)))))

(ert-deftest my/activity-process-details-quit-returns-to-board-and-kills-page ()
  "The custom details page follows native `quit-window' without leaking."
  (my/activity-test-isolated
    (let ((source (generate-new-buffer " *activity detail source*"))
          (process (make-pipe-process :name "activity-detail-probe" :noquery t))
          board detail)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (my/activity-board)
            (setq board (current-buffer))
            (let ((ref (intern (format "activity-process-%s"
                                       (process-name process)))))
              (should (eq (gethash ref my/activity-board--entries) process))
              (etaf-focus nil ref)
              (should (eq (key-binding "x") #'my/activity-board-stop))
              (should (eq (key-binding "j") #'etaf-focus-next))
              (my/activity-board-describe))
            (setq detail (current-buffer))
            (should (eq detail
                        (buffer-local-value 'my/activity-board--detail-buffer board)))
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

(ert-deftest my/activity-process-details-are-owned-by-their-board ()
  "Two Activity views cannot overwrite one another's details page."
  (let ((first (generate-new-buffer " *activity details first*"))
        (second (generate-new-buffer " *activity details second*"))
        first-detail second-detail)
    (unwind-protect
        (progn
          (with-current-buffer first
            (my/activity-board-mode)
            (setq first-detail (my/activity-board--ensure-detail-buffer)))
          (with-current-buffer second
            (my/activity-board-mode)
            (setq second-detail (my/activity-board--ensure-detail-buffer)))
          (should-not (eq first-detail second-detail))
          (kill-buffer first)
          (should-not (buffer-live-p first-detail))
          (should (buffer-live-p second-detail)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (when (buffer-live-p first-detail) (kill-buffer first-detail))
      (when (buffer-live-p second-detail) (kill-buffer second-detail)))))

(ert-deftest my/activity-transfer-pages-keep-queue-keys-under-evil ()
  "The transfer queue and log must resolve native exit and job actions."
  (with-temp-buffer
    (remote-transfer-list-mode)
    (should (eq (key-binding "q") #'quit-window))
    (should (eq (key-binding (kbd "RET")) #'remote-transfer-show-log))
    (should (eq (key-binding "k") #'remote-transfer-list-cancel)))
  (with-temp-buffer
    (remote-transfer-log-mode)
    (should (eq (key-binding "q") #'quit-window))))

(ert-deftest my/activity-transfer-queue-and-log-quit-return-under-evil ()
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

(ert-deftest my/activity-remote-board-keys-and-reopen-under-evil ()
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

(ert-deftest my/activity-remote-logs-quit-under-evil ()
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

(ert-deftest my/activity-remote-machines-keys-and-reopen-under-evil ()
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

(ert-deftest my/activity-remote-doctor-refresh-and-quit-under-evil ()
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

(ert-deftest my/activity-board-stop-waits-for-async-owner-cleanup ()
  "Requesting cancellation must not hide work before its owner finishes."
  (my/activity-test-isolated
    (let (cancelled)
      (let ((activity
             (my/activity-register
              "Export" :cancel (lambda () (setq cancelled t)))))
        (with-temp-buffer
          (insert (propertize "Export" 'my/activity-handle activity))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'my/activity-board-refresh) #'ignore))
            (my/activity-board-stop)))
        (should cancelled)
        (should (= 1 (my/activity-count)))
        (should (memq activity my/activity--registered))
        (my/activity-unregister activity)
        (should (zerop (my/activity-count)))))))

(ert-deftest my/activity-claims-bulk-transfers-as-tasks ()
  "A transfer's task row uses its owner for logging and cancellation."
  (my/activity-test-isolated
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
            (should (eq (my/activity-process-kind process) 'service))
            (setq remote-transfer-jobs (list job))
            (should (eq (my/activity-process-kind process) 'task))
            (should (equal (my/activity--process-label process)
                           "42% Upload a.bin"))
            (with-temp-buffer
              (insert (propertize "row" 'my/activity-process process))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'my/activity-board-refresh)
                         #'ignore))
                (my/activity-board-stop)
                (my/activity-board-interrupt)
                (my/activity-board-kill))
              (should (= cancelled 3))
              (save-window-excursion
                (my/activity-board-visit)
                (should (eq (current-buffer) log)))))
        (delete-process process)
        (kill-buffer temporary)
        (kill-buffer log)))))

(ert-deftest my/activity-remote-task-interrupt-uses-owner ()
  "The remote task owner must cancel target work, not only its relay."
  (my/activity-test-isolated
    (let* ((process (make-process :name "remote-task" :buffer nil
                                  :command '("sleep" "30") :noquery t))
           (task (remote-task-create :process process :name "Build"))
           (remote-tasks (make-hash-table :test #'equal))
           (cancelled 0))
      (unwind-protect
          (progn
            (puthash "build" task remote-tasks)
            (with-temp-buffer
              (insert (propertize "row" 'my/activity-process process))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'remote-task-cancel)
                         (lambda (owner &optional _)
                           (should (eq owner task))
                           (cl-incf cancelled)))
                        ((symbol-function 'my/activity-board-refresh) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'signal-process)
                         (lambda (&rest _)
                           (ert-fail "Remote relay was killed directly"))))
                (my/activity-board-interrupt)
                (my/activity-board-kill)))
            (should (= cancelled 2)))
        (delete-process process)))))

(ert-deftest my/activity-registration-cleans-up-on-return-and-error ()
  (my/activity-test-isolated
    (should (= 42 (my/with-activity "work"
                    (should (= 1 (my/activity-count))) 42)))
    (should (= 0 (my/activity-count)))
    (should-error (my/with-activity "failed work" (error "failed")))
    (should-not my/activity--registered)
    (should (= 0 my/activity--count))))

(ert-deftest my/activity-animation-advances-and-returns-to-rest ()
  (my/activity-test-isolated
    (let ((my/activity-indicator-mode t)
          (my/activity-indicator-fps 20))
      (let* ((handle (my/activity-register "animation probe"))
             (initial my/activity--frame-index))
        (should (timerp my/activity--timer))
        (sleep-for 0.18)
        (should (> my/activity--frame-index initial))
        (should (= 1 my/activity--count))
        (my/activity-unregister handle)
        (sleep-for 0.18)
        (should (= 0 my/activity--count))
        ;; A rest glyph ignores the former frame number immediately.
        (should (equal (substring-no-properties (my/activity-mode-line-text))
                       " ᕕ( ᴗ )ᕗ"))))))

(ert-deftest my/activity-empty-task-snapshot-does-not-rescan-per-process ()
  (my/activity-test-isolated
    (let ((process (make-pipe-process :name "activity-service" :noquery t))
          (scans 0))
      (unwind-protect
          (cl-letf (((symbol-function 'my/activity--owned-task-processes)
                     (lambda () (cl-incf scans) nil)))
            (dotimes (_ 10)
              (should (eq (my/activity-process-kind process nil) 'service)))
            (should (= scans 0))
            (should (eq (my/activity-process-kind process) 'service))
            (should (= scans 1)))
        (delete-process process)))))

(ert-deftest my/activity-noema-counts-busy-agents-without-idle-or-export-duplicates ()
  (my/activity-test-isolated
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
            (my/activity-noema--update-agents)
            (should (= 1 (my/activity-count)))
            (let* ((activity (gethash worker my/activity-noema--agents))
                   (since (my/activity-started-at activity)))
              (funcall (my/activity-cancel activity))
              (should (equal stops (list worker)))
              (my/activity-noema--update-agents)
              (should (= since (my/activity-started-at
                                (gethash worker my/activity-noema--agents)))))
            (setq busy nil)
            (my/activity-noema--update-agents)
            (should (= 0 (my/activity-count))))
        (mapc #'kill-buffer (list worker export idle))))))

(ert-deftest my/activity-noema-polls-asynchronously-and-filters-terminal-tasks ()
  (my/activity-test-isolated
    (let ((my/noema--ready t)
          (my/noema--process 'host)
          (calls 0) callback channel arguments)
      (cl-letf (((symbol-function 'my/noema-api-call)
                 (lambda (method args cb &optional _timeout)
                   (cl-incf calls)
                   (setq channel method arguments args callback cb))))
        (my/activity-noema--poll)
        (my/activity-noema--poll)
        (should (= calls 1))
        (should (equal channel "noema:api:tasks:list"))
        (should (equal arguments [((activeOnly . t))]))
        (funcall callback
                 '((tasks . [((id . "latex") (title . "Export note")
                               (kind . "latex-export") (status . "running")
                               (phase . "Compiling") (cancellable . t))
                              ((id . "done") (status . "completed"))])) nil)
        (should-not my/activity-noema--pending)
        (should (= 1 (my/activity-count)))
        (should (string-match-p "Compiling"
                                (my/activity-detail (car my/activity-noema--tasks))))
        (funcall (my/activity-cancel (car my/activity-noema--tasks)))
        (should (equal channel "noema:api:tasks:cancel"))
        (should (equal arguments [((id . "latex"))]))
        (funcall callback '((ok . t)) nil)
        (should (equal channel "noema:api:tasks:list"))
        (funcall callback '((tasks . [])) nil)
        (should (= 0 (my/activity-count)))))))

(ert-deftest my/activity-noema-host-stop-and-disable-ignore-stale-replies ()
  (my/activity-test-isolated
    (let ((my/noema--ready t)
          (my/noema--process 'old-host)
          callback)
      (cl-letf (((symbol-function 'my/noema-api-call)
                 (lambda (_method _args cb &optional _timeout) (setq callback cb))))
        (my/activity-noema--poll)
        (let ((old callback))
          (setq my/noema--ready nil)
          (my/activity-noema--host-stopped)
          (setq my/noema--ready t my/noema--process 'new-host)
          (my/activity-noema--poll)
          (funcall old '((tasks . [((id . "stale") (status . "running"))])) nil)
          (should my/activity-noema--pending)
          (should-not my/activity-noema--tasks))
        (my/activity-noema--mode-changed)
        (funcall callback '((tasks . [((id . "disabled") (status . "running"))])) nil)
        (should-not my/activity-noema--tasks)))))

(ert-deftest my/activity-noema-offline-poll-does-not-call-or-start-host ()
  (my/activity-test-isolated
    (cl-letf (((symbol-function 'my/noema-api-call)
               (lambda (&rest _) (ert-fail "Offline poll called the host"))))
      (my/activity-noema--poll)
      (should (= 0 (my/activity-count))))))

(ert-deftest my/activity-noema-unchanged-agent-snapshot-does-not-redraw-board ()
  (my/activity-test-isolated
    (let ((features (cons 'noema-agent-acp features))
          (worker (generate-new-buffer " *activity-stable-worker*"))
          (changes 0))
      (unwind-protect
          (cl-letf (((symbol-function 'noema-agent-acp-sessions)
                     (lambda (&optional _)
                       (list (list :buffer worker :name "work" :busy t))))
                    ((symbol-function 'my/activity-noema--changed)
                     (lambda () (cl-incf changes))))
            (my/activity-noema--update-agents)
            (dotimes (_ 5) (my/activity-noema--update-agents))
            (should (= 1 changes)))
        (kill-buffer worker)))))

(ert-deftest my/activity-noema-failed-request-releases-pending-and-can-recover ()
  (my/activity-test-isolated
    (let ((my/noema--ready t)
          (my/noema--process 'host)
          (calls 0) callback)
      (cl-letf (((symbol-function 'my/noema-api-call)
                 (lambda (_method _args cb &optional _timeout)
                   (cl-incf calls) (setq callback cb))))
        (my/activity-noema--poll)
        (funcall callback nil '((message . "timeout")))
        (should-not my/activity-noema--pending)
        (my/activity-noema--poll)
        (should (= 2 calls))
        (funcall callback '((tasks . [((id . "recovered") (status . "running"))])) nil)
        (should (= 1 (my/activity-count)))))))

(ert-deftest my/activity-providers-poll-at-idle-rate-during-animation ()
  (my/activity-test-isolated
    (let* ((calls 0)
          (my/activity-poll-functions (list (lambda () (cl-incf calls))))
          (my/activity--last-poll (float-time))
          (my/activity-poll-interval 2))
      (dotimes (_ 8) (my/activity--poll))
      (should (= calls 0))
      ;; Move the recorded poll time back rather than replacing `float-time',
      ;; a C primitive whose mock triggers native trampoline compilation.
      (setq my/activity--last-poll (- (float-time) 2.5))
      (my/activity--poll)
      (should (= calls 1))
      (dotimes (_ 8) (my/activity--poll))
      (should (= calls 1)))))

(ert-deftest my/activity-provider-failure-is-visible-and-does-not-stop-peers ()
  "One bad provider cannot hide other work or silently poison the board."
  (my/activity-test-isolated
    (let* ((broken t)
           (polled 0)
           (poll (lambda () (when broken (error "poll offline"))))
           (snapshot (lambda () (when broken (error "snapshot offline")) nil))
           (my/activity-poll-functions
            (list poll (lambda () (cl-incf polled))))
           (my/activity-provider-functions
            (list snapshot (lambda ()
                             (list (my/activity--create
                                    :label "Other task" :kind 'task))))))
      (my/activity--poll t)
      (should (= polled 1))
      (should (= 1 (my/activity-count)))
      (should (= 2 (hash-table-count my/activity--provider-errors)))
      (with-temp-buffer
        (my/activity-board-mode)
        (my/activity-board-refresh)
        (should (string-match-p "Provider problems" (buffer-string)))
        (should (string-match-p "snapshot offline" (buffer-string))))
      (setq broken nil)
      (my/activity--poll t)
      (my/activity-count)
      (should (zerop (hash-table-count my/activity--provider-errors))))))

(ert-deftest my/activity-tick-rearms-after-an-unexpected-scan-error ()
  "An unexpected scan exception must not permanently stop the indicator."
  (my/activity-test-isolated
    (let ((my/activity-indicator-mode t)
          (scheduled 0))
      (cl-letf (((symbol-function 'my/activity-count)
                 (lambda () (error "scan failed")))
                ((symbol-function 'my/activity--schedule)
                 (lambda () (cl-incf scheduled))))
        (should-error (my/activity--tick))
        (should (= scheduled 1))))))

(provide 'init-activity-tests)
;;; init-activity-tests.el ends here
