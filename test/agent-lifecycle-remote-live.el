;;; agent-lifecycle-remote-live.el --- The lifecycle on a real target -*- lexical-binding: t; -*-

;; Opt-in: AGENT_REMOTE_TARGET names a target with node on PATH.  The ACP fixture
;; runs on that target, under /tmp, and is removed afterwards.  No provider,
;; credential or model request is involved.

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/manager)
(require 'tools/agent/queue)
(load (expand-file-name "test/agent-flow-e2e.el" user-emacs-directory) nil t)

(defun agent-remote-live--config (fixture store)
  "Return a fixture agent whose process and files are native target paths."
  (agent-shell-make-agent-config
   :identifier 'flow-fixture :mode-line-name "Fixture"
   :buffer-name "Agent Remote" :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker (lambda (buffer)
                   (agent-shell--make-acp-client
                    :command "node" :command-params (list fixture store)
                    :context-buffer buffer))))

(defun agent-remote-live--dispose (&rest buffers)
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(ert-deftest agent-remote-live-queue-exit-changes-and-restore ()
  (let ((target (getenv "AGENT_REMOTE_TARGET")))
    (unless target (ert-skip "Set AGENT_REMOTE_TARGET to a target with node"))
    (let* ((native (format "/tmp/agent-live-%s/" (format-time-string "%s%N")))
           (root (remote-make-file-name target native))
           (fixture (concat native "fixture.mjs"))
           (store (concat native "sessions.json"))
           (agent-shell-confirm-interrupt nil)
           (agent-shell-busy-submit-default-function #'agent-shell-busy-submit-queue)
           (+agent-acp-notify-function nil)
           (asked nil) buffer restored opened)
      (make-directory root t)
      (unwind-protect
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt &rest _) (push prompt asked) nil))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _) (caar choices)))
                    ((symbol-function 'find-file) (lambda (file) (setq opened file)))
                    ((symbol-function 'find-file-other-window) (lambda (file) (setq opened file))))
            (copy-file (expand-file-name "test/fixtures/agent-flow-acp.mjs" user-emacs-directory)
                       (concat root "fixture.mjs"))
            (setq buffer (+agent-acp-start :config (agent-remote-live--config fixture store)
                                           :directory root :focus t :origin 'manual))
            (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))) 40)
            (should (equal (plist-get (+agent-session-snapshot buffer) :target) target))
            ;; C37/C39: stopping holds the queue; resuming sends it.
            (agent-flow-e2e--send buffer "hold")
            (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)) 20)
            (agent-flow-e2e--send buffer "later")
            (+agent-acp-stop buffer)
            (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer))) 20)
            (should-not asked)
            (should (= 1 (+agent-acp-held-prompt-count buffer)))
            (+agent-queue-resume buffer)
            (agent-flow-e2e--wait (lambda () (and (zerop (+agent-acp-pending-prompt-count buffer))
                                                  (not (+agent-acp-busy-p buffer))))
                                  20)
            (should (string-match-p "Reply: later" (agent-flow-e2e--text buffer)))
            ;; C44: a change the agent reports opens on the session's target.
            (agent-flow-e2e--send buffer "edit")
            (agent-flow-e2e--wait (lambda () (and (not (+agent-acp-busy-p buffer))
                                                  (buffer-local-value '+agent-acp--changes buffer)))
                                  20)
            (should (string-suffix-p "changed.txt"
                                     (caar (buffer-local-value '+agent-acp--changes buffer))))
            (with-temp-buffer (+agent-session-changes buffer))
            (should (equal (remote-file-name-target opened) target))
            ;; C38: the remote process exits and the session says so at once.
            (let ((id (+agent-acp-state-value buffer '(:session :id))))
              (agent-flow-e2e--send buffer "crash-later")
              (agent-flow-e2e--wait
               (lambda () (equal (+agent-sessions--state (+agent-session-snapshot buffer)) "dead"))
               20)
              (let ((snapshot (+agent-session-snapshot buffer)))
                (should (eq (plist-get snapshot :turn-state) 'interrupted))
                (should (plist-get snapshot :last-error))
                (should-not (plist-get snapshot :busy)))
              ;; C42: the saved conversation returns with no live session.
              (+agent-acp-kill buffer)
              (setq restored (+agent-acp-start :config (agent-remote-live--config fixture store)
                                               :directory root :focus nil :origin 'manual
                                               :choose-saved t))
              (agent-flow-e2e--wait
               (lambda () (+agent-acp-state-value restored '(:init-finished))) 40)
              (should (equal id (+agent-acp-state-value restored '(:session :id))))))
        (agent-remote-live--dispose buffer restored)
        (ignore-errors (delete-directory root t))
        (ignore-errors (remote-workspace-disconnect-target target 'live-test-cleanup))))))

(provide 'agent-lifecycle-remote-live)
;;; agent-lifecycle-remote-live.el ends here
