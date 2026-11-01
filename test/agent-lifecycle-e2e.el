;;; agent-lifecycle-e2e.el --- Real stdio lifecycle facts -*- lexical-binding: t; -*-

;; Every test here runs a real ACP process through the native agent-shell and
;; asserts what a person would see.  `y-or-n-p' and `yes-or-no-p' are spies
;; that fail the test: an event callback must never ask a question.

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/manager)
(require 'tools/agent/menu)
(require 'tools/agent/queue)
(load (expand-file-name "test/agent-flow-e2e.el" user-emacs-directory) nil t)

(defvar agent-lifecycle-e2e--asked nil)

(defmacro agent-lifecycle-e2e--with-session (spec &rest body)
  "Bind (BUFFER STORE DIRECTORY) from SPEC to a real fixture session for BODY."
  (declare (indent 1))
  (pcase-let ((`(,buffer ,store ,directory) spec))
    `(save-window-excursion
       (let* ((,directory (file-name-as-directory (make-temp-file "agent-lifecycle-" t)))
              (,store (expand-file-name "sessions.json" ,directory))
              (agent-shell-confirm-interrupt nil)
              (agent-shell-busy-submit-default-function #'agent-shell-busy-submit-queue)
              (+agent-acp-notify-function nil)
              (agent-lifecycle-e2e--asked nil)
              (,buffer (+agent-acp-start :config (agent-flow-e2e--config ,store)
                                         :directory ,directory :focus t :origin 'manual)))
         (unwind-protect
             (cl-letf (((symbol-function 'y-or-n-p)
                        (lambda (prompt &rest _) (push prompt agent-lifecycle-e2e--asked) nil))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (prompt &rest _) (push prompt agent-lifecycle-e2e--asked) nil)))
               (agent-flow-e2e--wait
                (lambda () (+agent-acp-state-value ,buffer '(:init-finished))))
               ,@body)
           (when (buffer-live-p ,buffer)
             (with-current-buffer ,buffer
               (let ((kill-buffer-query-functions nil)) (kill-buffer ,buffer))))
           (delete-directory ,directory t))))))

(defun agent-lifecycle-e2e--state (buffer)
  (+agent-sessions--state (+agent-session-snapshot buffer)))

(defun agent-lifecycle-e2e--process (buffer)
  (+agent-acp-state-value buffer '(:client :process)))

(defun agent-lifecycle-e2e--events (store)
  (with-temp-buffer
    (insert-file-contents (concat store ".events"))
    (split-string (buffer-string) "\n" t)))

(ert-deftest agent-queue-c37-stopping-holds-the-queue-without-asking ()
  "Stopping a turn never asks from the response callback, nor sends the queue."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "hold")
    (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
    (agent-flow-e2e--send buffer "later one")
    (agent-flow-e2e--send buffer "later two")
    (should (= (+agent-acp-pending-prompt-count buffer) 2))
    (+agent-acp-stop buffer)
    (agent-flow-e2e--idle buffer)
    (should-not agent-lifecycle-e2e--asked)
    (should (= (+agent-acp-pending-prompt-count buffer) 2))
    (should (= (plist-get (+agent-session-snapshot buffer) :queue-held) 2))
    ;; A later ordinary turn does not launch prompts the person stopped.
    (agent-flow-e2e--send buffer "ordinary")
    (agent-flow-e2e--idle buffer)
    (should (= (+agent-acp-pending-prompt-count buffer) 2))
    (should-not (cl-some (lambda (line) (string-match-p "later one" line))
                         (agent-lifecycle-e2e--events store)))
    ;; Resuming is explicit, and sends them once.
    (+agent-queue-resume buffer)
    (agent-flow-e2e--idle buffer)
    (should (zerop (+agent-acp-pending-prompt-count buffer)))
    (should (= 1 (cl-count-if (lambda (line) (string-match-p "later one" line))
                              (agent-lifecycle-e2e--events store))))))

(ert-deftest agent-queue-c37-abnormal-stop-holds-the-queue-and-reports-failure ()
  "A refused turn is a fact, not a completion, and does not advance the queue."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "refuse")
    (agent-flow-e2e--idle buffer)
    (let ((snapshot (+agent-session-snapshot buffer)))
      (should (equal (plist-get snapshot :stop-reason) "refusal"))
      (should (eq (plist-get snapshot :attention-kind) 'failed)))
    ;; The next turn clears the fact.
    (agent-flow-e2e--send buffer "fine")
    (agent-flow-e2e--idle buffer)
    (let ((snapshot (+agent-session-snapshot buffer)))
      (should (equal (plist-get snapshot :stop-reason) "end_turn"))
      (should-not (plist-get snapshot :attention-kind)))))

(ert-deftest agent-process-c38-an-idle-crash-is-reported-at-once ()
  "A client that exits without a request still changes the visible state."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (let* ((changes 0)
           (reasons nil)
           (listener (lambda (source) (when (eq source buffer) (cl-incf changes))))
           (notifier (lambda (source reason) (when (eq source buffer) (push reason reasons)) nil)))
      (add-hook '+agent-acp-changed-functions listener)
      (add-hook '+agent-acp-notification-functions notifier)
      (unwind-protect
          (progn
            (delete-process (agent-lifecycle-e2e--process buffer))
            (agent-flow-e2e--wait (lambda () (> changes 0)) 3)
            (should (equal (agent-lifecycle-e2e--state buffer) "dead"))
            (should (memq 'failed reasons))
            (should (plist-get (+agent-session-snapshot buffer) :last-error)))
        (remove-hook '+agent-acp-changed-functions listener)
        (remove-hook '+agent-acp-notification-functions notifier)))))

(ert-deftest agent-process-c38-exit-settles-stale-requests ()
  "Requests of an exited client are not pending, and nothing claims it runs."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "permission")
    (agent-flow-e2e--wait
     (lambda () (eq (plist-get (+agent-session-snapshot buffer) :attention-kind) 'permission)))
    (delete-process (agent-lifecycle-e2e--process buffer))
    (agent-flow-e2e--wait (lambda () (not (+agent-session-live-p buffer))))
    (accept-process-output nil 0.1)
    (let ((snapshot (+agent-session-snapshot buffer)))
      (should (eq (plist-get snapshot :attention-kind) 'failed))
      (should-not (plist-get snapshot :busy))
      (should (eq (plist-get snapshot :turn-state) 'interrupted))
      (should (equal (+agent-sessions--state snapshot) "dead")))
    (should-error (+agent-acp-review-request buffer) :type 'user-error)
    ;; Stopping what has already ended is refused plainly.
    (should-error (+agent-acp-stop buffer) :type 'user-error)
    ;; An ended client does not make closing the tab ask about work.
    (should (+agent-acp-confirm-stop buffer "close"))
    (should-not agent-lifecycle-e2e--asked)))

(ert-deftest agent-process-c38-a-deliberate-shutdown-is-not-a-failure ()
  "The session's own shutdown is not reported as a lost client."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (let* ((reasons nil)
           (notifier (lambda (source reason) (when (eq source buffer) (push reason reasons)) nil)))
      (add-hook '+agent-acp-notification-functions notifier)
      (unwind-protect
          (progn
            (+agent-acp-shutdown buffer)
            (accept-process-output nil 0.3)
            (should-not reasons)
            (should-not (plist-get (+agent-session-snapshot buffer) :last-error)))
        (remove-hook '+agent-acp-notification-functions notifier)))))

(defun agent-lifecycle-e2e--synthetic (name &rest locals)
  "A real buffer and pipe for NAME, with LOCALS as (SYMBOL VALUE ...)."
  (let* ((buffer (generate-new-buffer (concat " *agent-lifecycle-" name "*")))
         (process (make-pipe-process :name (buffer-name buffer) :noquery t)))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode)
      (setq-local default-directory temporary-file-directory
                  +agent-acp-session-root default-directory
                  +agent-acp-session-name name +agent-acp-session-agent "codex"
                  +agent-acp-session-origin 'run
                  agent-shell--state
                  (agent-shell--make-state :buffer buffer :agent-config '((:identifier . codex))))
      (setf (map-elt agent-shell--state :client) (list (cons :process process))
            (map-elt agent-shell--state :init-finished) t
            (map-elt (map-elt agent-shell--state :session) :id) name)
      (cl-loop for (symbol value) on locals by #'cddr
               do (set (make-local-variable symbol) value))
      (add-hook 'kill-buffer-hook (lambda () (when (process-live-p process) (delete-process process))) nil t))
    buffer))

(defun agent-lifecycle-e2e--dispose (&rest buffers)
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(defun agent-lifecycle-e2e--await-permission (buffer name)
  "Make synthetic BUFFER wait on a permission for tool NAME."
  (with-current-buffer buffer
    (setf (map-elt agent-shell--state :tool-calls)
          (list (cons name (list (cons :title name) (cons :permission-request-id 7)))))))

(ert-deftest agent-process-c38-an-explicit-remote-disconnect-is-not-a-failure ()
  "Remote already told the person; the framework owns that lifecycle."
  (let* ((agent (agent-lifecycle-e2e--synthetic "remote"))
         (process (agent-lifecycle-e2e--process agent))
         (state (buffer-local-value 'agent-shell--state agent))
         reasons)
    (unwind-protect
        (let ((+agent-acp-notification-functions
               (list (lambda (_buffer reason) (push reason reasons) t))))
          (delete-process process)
          (with-current-buffer agent (setq-local remote-buffer-disconnected-p t))
          (+agent-acp--client-exited agent state process "deleted" nil)
          (should-not reasons)
          (should-not (buffer-local-value '+agent-acp-last-error agent))
          (with-current-buffer agent (setq-local remote-buffer-disconnected-p nil))
          (+agent-acp--client-exited agent state process "deleted" nil)
          (should (equal reasons '(failed))))
      (agent-lifecycle-e2e--dispose agent))))

(ert-deftest agent-attention-c40-a-domain-reason-stays-in-the-inbox ()
  (should (= 3 (+agent-session-attention-rank (list :attention-kind 'proposal))))
  (should (< (+agent-session-attention-rank (list :attention-kind 'failed))
             (+agent-session-attention-rank (list :attention-kind 'proposal))
             (+agent-session-attention-rank (list :process-state 'dead))
             (+agent-session-attention-rank (list :unread-kind 'done))))
  (should-not (+agent-session-attention-rank (list :process-state 'live))))

(ert-deftest agent-queue-c37-a-run-leaves-the-queue-held ()
  "A structured turn never sends the queue, so it must not claim it will."
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (+agent-acp-prompt :buffer buffer :run-id "run-1"
                       :content (list '((type . "text") (text . "hold"))))
    (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
    (agent-flow-e2e--send buffer "later")
    (should (= 1 (length (+agent-acp-state-value buffer '(:pending-prompts)))))
    (+agent-acp-interrupt buffer t)
    (agent-flow-e2e--idle buffer)
    (should (= 1 (+agent-acp-held-prompt-count buffer)))
    (should-not (+agent-acp-state-value buffer '(:pending-prompts)))
    (should-not agent-lifecycle-e2e--asked)
    (should-not (cl-some (lambda (line) (string-match-p "\"text\":\"later\"" line))
                         (agent-lifecycle-e2e--events store)))))

(ert-deftest agent-queue-c39-edits-the-held-and-native-queue-as-one-list ()
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "hold")
    (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
    (dolist (text '("first" "second" "third")) (agent-flow-e2e--send buffer text))
    (+agent-acp-stop buffer)
    (agent-flow-e2e--idle buffer)
    (should (equal (+agent-queue-items buffer) '("first" "second" "third")))
    (should (= (+agent-queue-move buffer 2 -2) 0))
    (should (equal (+agent-queue-items buffer) '("third" "first" "second")))
    (+agent-queue-edit buffer 1 "first, edited")
    (+agent-queue-remove buffer 2)
    (should (equal (+agent-queue-items buffer) '("third" "first, edited")))
    (should (= (+agent-acp-held-prompt-count buffer) 2))
    (should-error (+agent-queue-remove buffer 5) :type 'user-error)
    ;; Send now takes one prompt; the rest stay held.
    (+agent-queue-send-now buffer 1)
    (agent-flow-e2e--idle buffer)
    (should (equal (+agent-queue-items buffer) '("third")))
    (should (cl-some (lambda (line) (string-match-p "first, edited" line))
                     (agent-lifecycle-e2e--events store)))
    (should-not (cl-some (lambda (line) (string-match-p "\"text\":\"third\"" line))
                         (agent-lifecycle-e2e--events store)))
    (+agent-queue-clear buffer)
    (should (zerop (+agent-acp-pending-prompt-count buffer)))))

(ert-deftest agent-queue-c39-send-now-refuses-to-interrupt-an-agent-that-cannot-steer ()
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "hold")
    (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
    (agent-flow-e2e--send buffer "later")
    (should-error (+agent-queue-send-now buffer 0) :type 'user-error)
    (should (equal (+agent-queue-items buffer) '("later")))
    (+agent-queue-resume buffer)
    (should (equal (+agent-acp-state-value buffer '(:pending-prompts)) '("later")))
    (+agent-acp-stop buffer)
    (agent-flow-e2e--idle buffer)
    (should (equal (+agent-acp-state-value buffer '(:pending-prompts)) nil))
    (should (= (+agent-acp-held-prompt-count buffer) 1))))

(ert-deftest agent-queue-c39-resume-refuses-an-exited-client ()
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "hold")
    (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
    (agent-flow-e2e--send buffer "later")
    (delete-process (agent-lifecycle-e2e--process buffer))
    (agent-flow-e2e--wait (lambda () (= (+agent-acp-held-prompt-count buffer) 1)) 3)
    (should-error (+agent-queue-resume buffer) :type 'user-error)
    (should (equal (+agent-queue-items buffer) '("later")))))

(ert-deftest agent-queue-c39-panel-shows-the-queue-and-closing-changes-nothing ()
  (let ((agent (agent-lifecycle-e2e--synthetic "panel" '+agent-acp--held-prompts '("kept prompt")))
        (panel nil))
    (unwind-protect
        (progn
          (setq panel (+agent/queue agent))
          (with-current-buffer panel
            (etaf-runtime-flush (etaf-runtime-for-buffer panel))
            (should (string-match-p "kept prompt" (buffer-string)))
            (should (string-match-p "held" (buffer-string)))
            (should (memq #'+agent-queue--changed-h +agent-acp-changed-functions))
            (+agent-queue-hide))
          (should-not (memq #'+agent-queue--changed-h +agent-acp-changed-functions))
          (should (equal (+agent-queue-items agent) '("kept prompt"))))
      (remove-hook '+agent-acp-changed-functions #'+agent-queue--changed-h)
      (agent-lifecycle-e2e--dispose panel agent))))

(ert-deftest agent-attention-c40-cycles-by-urgency-and-lands-on-the-request ()
  (let* ((approve (agent-lifecycle-e2e--synthetic "approve"))
         (failed (agent-lifecycle-e2e--synthetic "failed" '+agent-acp-last-error "boom"))
         (unread (agent-lifecycle-e2e--synthetic "unread" '+agent-acp-attention 'done))
         (idle (agent-lifecycle-e2e--synthetic "idle"))
         reviewed shown)
    (agent-lifecycle-e2e--await-permission approve "Run tests")
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-review-request) (lambda (buffer) (push buffer reviewed)))
                  ((symbol-function '+agent-acp-show-buffer) (lambda (buffer) (push buffer shown))))
          (should (equal (mapcar (lambda (s) (plist-get s :buffer))
                                 (+agent-session-attention-queue (+agent-acp-sessions)))
                         (list approve failed unread)))
          (with-temp-buffer (+agent/next-attention))
          (should (equal reviewed (list approve)))
          (with-current-buffer approve (+agent/next-attention))
          (should (equal shown (list failed)))
          (with-current-buffer unread (+agent/next-attention))
          (should (equal reviewed (list approve approve))))
      (agent-lifecycle-e2e--dispose approve failed unread idle))))

(ert-deftest agent-attention-c40-prefix-stays-in-the-project ()
  (let* ((other (make-temp-file "agent-other-" t))
         (here (agent-lifecycle-e2e--synthetic "here" '+agent-acp-attention 'done))
         (away (agent-lifecycle-e2e--synthetic "away" '+agent-acp-attention 'done
                                               '+agent-acp-session-root (file-name-as-directory other)))
         shown)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-show-buffer) (lambda (buffer) (push buffer shown))))
          (with-current-buffer here (+agent/next-attention t))
          (should (equal shown (list here))))
      (agent-lifecycle-e2e--dispose here away)
      (delete-directory other t))))

(ert-deftest agent-attention-c40-nothing-waiting-says-so ()
  (let ((idle (agent-lifecycle-e2e--synthetic "idle")))
    (unwind-protect
        (should-error (+agent/next-attention) :type 'user-error)
      (agent-lifecycle-e2e--dispose idle))))

(ert-deftest agent-attention-c40-notification-click-lands-on-a-pending-request ()
  (let ((agent (agent-lifecycle-e2e--synthetic "click")) reviewed shown)
    (agent-lifecycle-e2e--await-permission agent "Delete files")
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-review-request) (lambda (buffer) (push buffer reviewed)))
                  ((symbol-function 'agent-shell--display-buffer) (lambda (buffer) (push buffer shown))))
          (+agent-notify--visit agent)
          (should (equal reviewed (list agent)))
          (with-current-buffer agent
            (setf (map-elt agent-shell--state :tool-calls) nil))
          (+agent-notify--visit agent)
          (should (equal shown (list agent)))
          (should (equal reviewed (list agent))))
      (agent-lifecycle-e2e--dispose agent))))

(ert-deftest agent-attention-c40-merged-notification-opens-the-inbox ()
  (let (opened)
    (cl-letf (((symbol-function '+agent/inbox) (lambda () (setq opened t))))
      (+agent-notify--open-inbox)
      (should opened))))

(ert-deftest agent-attention-c40-inbox-puts-the-most-urgent-first ()
  (let* ((unread (agent-lifecycle-e2e--synthetic "a-unread" '+agent-acp-attention 'done))
         (approve (agent-lifecycle-e2e--synthetic "b-approve")))
    (agent-lifecycle-e2e--await-permission approve "Push")
    (unwind-protect
        (should (equal (mapcar (lambda (s) (plist-get s :name))
                               (+agent-manager--sessions (+agent-acp-sessions) 'attention nil ""))
                       '("b-approve" "a-unread")))
      (agent-lifecycle-e2e--dispose unread approve))))

(ert-deftest agent-manager-c41-snapshot-says-what-the-session-is-doing ()
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (agent-flow-e2e--send buffer "titled")
    (agent-flow-e2e--idle buffer)
    (let ((snapshot (+agent-session-snapshot buffer)))
      (should (equal (plist-get snapshot :title) "Fixture title"))
      (should (equal (plist-get snapshot :last-prompt) "titled"))
      (should-not (plist-get snapshot :turn-started-at)))
    (agent-flow-e2e--send buffer "permission")
    (agent-flow-e2e--wait
     (lambda () (eq (plist-get (+agent-session-snapshot buffer) :attention-kind) 'permission)))
    (let ((snapshot (+agent-session-snapshot buffer)))
      (should (equal (plist-get snapshot :request) "Read fixture.txt"))
      (should (numberp (plist-get snapshot :turn-started-at)))
      (should (member (+agent-session-duration 75) '("1m"))))
    (should (+agent-manager--sessions (list (+agent-session-snapshot buffer)) 'all nil "fixture title"))
    (should-not (+agent-manager--sessions (list (+agent-session-snapshot buffer)) 'all nil "no such words"))
    (+agent-acp-stop buffer)
    (agent-flow-e2e--idle buffer)))

(ert-deftest agent-manager-c41-progress-reads-only-reported-facts ()
  (should (equal (+agent-session-progress
                  (list :busy t :turn-started-at (- (float-time) 90) :queued 3 :queue-held 2))
                 '("1m" "3 queued (2 held)")))
  (should-not (+agent-session-progress (list :busy nil :turn-started-at 1 :queued 0)))
  (should (equal (+agent-session-error-text (list :last-error "boom")) "boom"))
  (should (equal (+agent-session-error-text (list :last-error '((:message . "lost")))) "lost"))
  (should-not (+agent-session-error-text nil)))

(defun agent-lifecycle-e2e--restorable (store directory)
  "Leave one saved conversation in STORE, then end its client."
  (let ((first (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                 :focus nil :origin 'manual)))
    (agent-flow-e2e--wait (lambda () (+agent-acp-state-value first '(:init-finished))))
    (agent-flow-e2e--send first "hello")
    (agent-flow-e2e--idle first)
    (prog1 (+agent-acp-state-value first '(:session :id))
      (+agent-acp-kill first))))

(ert-deftest agent-resume-c42-restores-a-chosen-conversation-without-a-live-session ()
  (let* ((directory (file-name-as-directory (make-temp-file "agent-resume-" t)))
         (store (expand-file-name "sessions.json" directory))
         (+agent-acp-notify-function nil)
         (id (agent-lifecycle-e2e--restorable store directory))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices))))
          (setq buffer (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                         :focus nil :origin 'manual :choose-saved t))
          (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))))
          (should (equal id (+agent-acp-state-value buffer '(:session :id))))
          (should (string-match-p "Restored: hello" (agent-flow-e2e--text buffer)))
          (should (= 1 (cl-count-if (lambda (line) (string-match-p "session/new" line))
                                    (agent-lifecycle-e2e--events store)))))
      (agent-lifecycle-e2e--dispose buffer)
      (delete-directory directory t))))

(ert-deftest agent-resume-c42-cancelling-leaves-nothing-behind ()
  (let* ((directory (file-name-as-directory (make-temp-file "agent-resume-" t)))
         (store (expand-file-name "sessions.json" directory))
         (+agent-acp-notify-function nil)
         (id (agent-lifecycle-e2e--restorable store directory))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
          (should id)
          (setq buffer (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                         :focus nil :origin 'manual :choose-saved t))
          (agent-flow-e2e--wait (lambda () (not (buffer-live-p buffer))))
          (should (= 1 (cl-count-if (lambda (line) (string-match-p "session/new" line))
                                    (agent-lifecycle-e2e--events store)))))
      (agent-lifecycle-e2e--dispose buffer)
      (delete-directory directory t))))

(ert-deftest agent-resume-c42-nothing-saved-opens-no-blank-conversation ()
  (let* ((directory (file-name-as-directory (make-temp-file "agent-resume-" t)))
         (store (expand-file-name "sessions.json" directory))
         (+agent-acp-notify-function nil)
         (buffer (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                   :focus nil :origin 'manual :choose-saved t)))
    (unwind-protect
        (progn
          (agent-flow-e2e--wait (lambda () (not (buffer-live-p buffer))))
          (should-not (cl-some (lambda (line) (string-match-p "session/new" line))
                               (agent-lifecycle-e2e--events store))))
      (agent-lifecycle-e2e--dispose buffer)
      (delete-directory directory t))))

(ert-deftest agent-resume-c42-the-choice-is-reused-if-already-open ()
  (let* ((directory (file-name-as-directory (make-temp-file "agent-resume-" t)))
         (store (expand-file-name "sessions.json" directory))
         (+agent-acp-notify-function nil)
         (live (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                 :focus nil :origin 'manual))
         buffer shown)
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function '+agent-acp-show-buffer)
                   (lambda (target) (setq shown target))))
          (agent-flow-e2e--wait (lambda () (+agent-acp-state-value live '(:init-finished))))
          (setq buffer (+agent-acp-start :config (agent-flow-e2e--config store) :directory directory
                                         :focus nil :origin 'manual :choose-saved t))
          (agent-flow-e2e--wait (lambda () (not (buffer-live-p buffer))))
          (should (eq shown live)))
      (agent-lifecycle-e2e--dispose buffer live)
      (delete-directory directory t))))

(ert-deftest agent-exit-c43-asks-once-and-only-about-real-work ()
  (let* ((working (agent-lifecycle-e2e--synthetic "working" '+agent-acp--held-prompts '("later")
                                                  '+agent-acp-session-origin 'manual))
         (side (agent-lifecycle-e2e--synthetic "side" '+agent-acp--held-prompts '("later")
                                               '+agent-acp-session-origin 'side))
         (idle (agent-lifecycle-e2e--synthetic "idle" '+agent-acp-session-origin 'manual))
         (noninteractive nil)
         asked)
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) nil)))
          (should (equal (+agent-acp--unfinished-sessions) (list working)))
          (should-not (+agent-acp--query-kill-emacs-h))
          (should (= (length asked) 1))
          (should (string-match-p "working" (car asked)))
          (agent-lifecycle-e2e--dispose working)
          (should (+agent-acp--query-kill-emacs-h))
          (should (= (length asked) 1)))
      (agent-lifecycle-e2e--dispose working side idle))))

(ert-deftest agent-exit-c43-batch-never-asks ()
  (let ((working (agent-lifecycle-e2e--synthetic "working" '+agent-acp--held-prompts '("later")
                                                 '+agent-acp-session-origin 'manual))
        (noninteractive t))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (should (+agent-acp--query-kill-emacs-h)))
      (agent-lifecycle-e2e--dispose working))))

(ert-deftest agent-changes-c44-records-completed-edits-once-and-opens-them ()
  (agent-lifecycle-e2e--with-session (buffer store directory)
    (let ((file (expand-file-name "changed.txt" directory)) opened)
      (with-temp-file file (insert "one\n"))
      (agent-flow-e2e--send buffer "edit")
      (agent-flow-e2e--idle buffer)
      (should (= 1 (plist-get (+agent-session-snapshot buffer) :changes)))
      (should (= 1 (plist-get (cdar (buffer-local-value '+agent-acp--changes buffer)) :count)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (caar choices)))
                ((symbol-function 'find-file) (lambda (target) (setq opened target)))
                ((symbol-function 'find-file-other-window) (lambda (target) (setq opened target))))
        (with-temp-buffer (+agent-session-changes buffer)))
      (should (equal (file-truename opened) (file-truename file))))))

(ert-deftest agent-changes-c44-the-file-opens-on-the-session-target ()
  (let ((agent (agent-lifecycle-e2e--synthetic "remote")) opened)
    (with-current-buffer agent
      (+agent-acp--note-change "/srv/app/main.py")
      (setq-local agent-shell-path-resolver-function
                  (lambda (path) (concat "/fs:box:" path))))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function 'find-file) (lambda (target) (setq opened target))))
          (with-temp-buffer (+agent-session-changes agent))
          (should (equal opened "/fs:box:/srv/app/main.py")))
      (agent-lifecycle-e2e--dispose agent))))

(ert-deftest agent-changes-c44-is-bounded-and-ignores-unfinished-tools ()
  (let ((agent (agent-lifecycle-e2e--synthetic "bounded"))
        (+agent-acp-changes-limit 3))
    (unwind-protect
        (with-current-buffer agent
          (dotimes (index 5) (+agent-acp--note-change (format "/tmp/%d" index)))
          (should (equal (mapcar #'car +agent-acp--changes) '("/tmp/4" "/tmp/3" "/tmp/2")))
          (setq +agent-acp--changes nil)
          (let ((diffs (list (list (cons :file "/tmp/x")))))
            (+agent-acp--note-tool-change "a" (list (cons :status "in_progress") (cons :diffs diffs)))
            (should-not +agent-acp--changes)
            (+agent-acp--note-tool-change "a" (list (cons :status "completed") (cons :diffs diffs)))
            (+agent-acp--note-tool-change "a" (list (cons :status "completed") (cons :diffs diffs)))
            (should (= 1 (plist-get (cdr (assoc "/tmp/x" +agent-acp--changes)) :count)))))
      (agent-lifecycle-e2e--dispose agent))))

(defun agent-lifecycle-e2e--target-config (node fixture store)
  "Return a fixture agent whose program and files are native target paths."
  (agent-shell-make-agent-config
   :identifier 'flow-fixture :mode-line-name "Fixture"
   :buffer-name "Agent Routed" :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker (lambda (buffer)
                   (agent-shell--make-acp-client
                    :command node :command-params (list fixture store)
                    :context-buffer buffer))))

(defun agent-lifecycle-e2e--register-box ()
  "Register target box: Remote routes it, and no client path reaches it."
  (remote-fs-install)
  (remote-register-backend
   "agent-box" :capabilities '(metadata file-read file-write directory process-sync process-async)
   :project (lambda (file _link _route) (remote-file-local-name file))
   :connect (lambda (_route _context) "/")
   :live (lambda (_connection _route _context) t)
   :program-form 'search
   :describe (lambda ()
               '(:kind agent-box :session-owner emacs :file-operation-cost batched
                 :file-watch-cost push :mtime-compare exact :process-write-cost direct)))
  (remote-register-target "agent-box" :label "Agent box")
  (remote-register-pipeline "agent-box" "api" '("agent-box")))

(ert-deftest agent-remote-routed-lifecycle-on-a-target-the-client-cannot-reach ()
  "The client runs through the Remote process route, with no network or host."
  (skip-unless (executable-find "node"))
  (agent-lifecycle-e2e--register-box)
  (let* ((native (file-name-as-directory (make-temp-file "agent-routed-" t)))
         (root (remote-make-file-name "agent-box" native))
         (store (concat native "sessions.json"))
         (fixture (concat native "fixture.mjs"))
         (node (executable-find "node"))
         (agent-shell-confirm-interrupt nil)
         (agent-shell-busy-submit-default-function #'agent-shell-busy-submit-queue)
         (+agent-acp-notify-function nil)
         (asked nil) buffer restored opened)
    (copy-file (expand-file-name "test/fixtures/agent-flow-acp.mjs" user-emacs-directory) fixture)
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt &rest _) (push prompt asked) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function 'find-file) (lambda (file) (setq opened file)))
                  ((symbol-function 'find-file-other-window) (lambda (file) (setq opened file))))
          (should-not (remote-client-file-name root))
          (setq buffer (+agent-acp-start :config (agent-lifecycle-e2e--target-config node fixture store)
                                         :directory root :focus t :origin 'manual))
          (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))) 20)
          (should (equal (plist-get (+agent-session-snapshot buffer) :target) "agent-box"))
          (agent-flow-e2e--send buffer "hold")
          (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)) 10)
          (agent-flow-e2e--send buffer "later")
          (+agent-acp-stop buffer)
          (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 10)
          (should-not asked)
          (should (= 1 (+agent-acp-held-prompt-count buffer)))
          (+agent-queue-resume buffer)
          (agent-flow-e2e--wait (lambda () (and (zerop (+agent-acp-pending-prompt-count buffer))
                                                (not (+agent-acp-busy-p buffer))))
                                10)
          (agent-flow-e2e--send buffer "edit")
          (agent-flow-e2e--wait (lambda () (and (not (+agent-acp-busy-p buffer))
                                                (buffer-local-value '+agent-acp--changes buffer)))
                                10)
          (with-temp-buffer (+agent-session-changes buffer))
          (should (equal (remote-file-name-target opened) "agent-box"))
          (let ((id (+agent-acp-state-value buffer '(:session :id))))
            (agent-flow-e2e--send buffer "crash-later")
            (agent-flow-e2e--wait
             (lambda () (equal (+agent-sessions--state (+agent-session-snapshot buffer)) "dead")) 10)
            (let ((snapshot (+agent-session-snapshot buffer)))
              (should (eq (plist-get snapshot :turn-state) 'interrupted))
              (should (plist-get snapshot :last-error))
              (should-not (plist-get snapshot :busy)))
            (+agent-acp-kill buffer)
            (setq restored (+agent-acp-start :config (agent-lifecycle-e2e--target-config node fixture store)
                                             :directory root :focus nil :origin 'manual
                                             :choose-saved t))
            (agent-flow-e2e--wait
             (lambda () (+agent-acp-state-value restored '(:init-finished))) 20)
            (should (equal id (+agent-acp-state-value restored '(:session :id))))))
      (agent-lifecycle-e2e--dispose buffer restored)
      (delete-directory native t))))

(ert-deftest agent-keys-c39-c44-entries-are-bound ()
  (with-temp-buffer
    (should (eq (key-binding (kbd "C-c A j")) #'+agent/next-attention))
    (should (eq (key-binding (kbd "C-c A q")) #'+agent/queue))
    (should (eq (key-binding (kbd "C-c A h")) #'+agent/resume-saved))
    (should (eq (key-binding (kbd "C-c A l")) #'+agent/changes))))

(provide 'agent-lifecycle-e2e)
;;; agent-lifecycle-e2e.el ends here
