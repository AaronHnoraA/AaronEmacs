;;; agent-flow-e2e.el --- Real stdio and native controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/manager)
(require 'tools/agent/menu)

(defun agent-flow-e2e--wait (predicate &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 8))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.03))
    (unless (funcall predicate) (error "Timed out waiting for native ACP state"))))

(defun agent-flow-e2e--config (store)
  (let ((fixture (expand-file-name "test/fixtures/agent-flow-acp.mjs" user-emacs-directory)))
    (agent-shell-make-agent-config
     :identifier 'flow-fixture :mode-line-name "Fixture"
     :buffer-name "Agent Flow" :shell-prompt "Fixture> "
     :shell-prompt-regexp "Fixture> "
     :client-maker (lambda (buffer)
                     (agent-shell--make-acp-client
                      :command (executable-find "node") :command-params (list fixture store)
                      :context-buffer buffer)))))

(defun agent-flow-e2e--send (buffer text)
  (with-current-buffer buffer
    (goto-char (point-max)) (insert text)
    (agent-shell-submit)))

(defun agent-flow-e2e--idle (buffer)
  (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p buffer)))))

(defun agent-flow-e2e--text (buffer)
  (+agent-render-flush buffer)
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

(defun agent-flow-e2e--control (buffer property predicate)
  "Activate an actual native control by its text identity and local RET map."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (< (point) (point-max)))
        (when (funcall predicate (get-text-property (point) property)) (setq found (point)))
        (unless found (goto-char (or (next-single-property-change (point) property) (point-max)))))
      (unless found (error "Native control not rendered: %s" property))
      (goto-char found)
      (let ((command (key-binding (kbd "RET"))))
        (unless (commandp command) (error "Control has no native RET action"))
        (call-interactively command)))))

(ert-deftest agent-flow-e2e-native-transport-requests-queue-cancel-restart-and-crash ()
  (save-window-excursion
    (let* ((directory (file-name-as-directory (make-temp-file "agent-flow-e2e-" t)))
           (store (expand-file-name "sessions.json" directory))
           (agent-shell-confirm-interrupt nil)
           (+agent-acp-notify-function nil)
           (agent-shell-elicitation--experimental-feature-enabled t)
           (buffer (+agent-acp-start :config (agent-flow-e2e--config store)
                                     :directory directory :focus t :origin 'manual))
           (name (buffer-name buffer)) id)
      (unwind-protect
          (progn
            (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))))
            (setq id (+agent-acp-state-value buffer '(:session :id)))
            (should (+agent-session-capability-p buffer :list))
            (should (+agent-session-capability-p buffer :load))
            (agent-flow-e2e--send buffer "hello")
            (agent-flow-e2e--idle buffer)
            (should (string-match-p "Reply: hello" (agent-flow-e2e--text buffer)))
            ;; Native permission buttons, with navigation through our adapter.
            (agent-flow-e2e--send buffer "permission")
            (agent-flow-e2e--wait
             (lambda () (eq (plist-get (+agent-session-snapshot buffer) :attention-kind) 'permission)))
            (+agent-acp-review-request buffer)
            (should (with-current-buffer buffer (get-text-property (point) 'agent-shell-permission-button)))
            (agent-flow-e2e--control buffer 'agent-shell-permission-button #'identity)
            (agent-flow-e2e--idle buffer)
            (should (string-match-p "APPROVAL_OK" (agent-flow-e2e--text buffer)))
            ;; Native elicitation: submit the provider's default enum value.
            (agent-flow-e2e--send buffer "question")
            (agent-flow-e2e--wait
             (lambda () (eq (plist-get (+agent-session-snapshot buffer) :attention-kind) 'input)))
            (+agent-acp-review-request buffer)
            (agent-flow-e2e--control buffer 'agent-shell-elicitation-control
                                    (lambda (control) (eq (map-elt control :action) 'submit)))
            (agent-flow-e2e--idle buffer)
            (should (string-match-p "ANSWER_accept_focused" (agent-flow-e2e--text buffer)))
            ;; Queue is the upstream queue; cancellation does not submit it.
            (agent-flow-e2e--send buffer "hold")
            (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p buffer)))
            (agent-flow-e2e--send buffer "queued after cancel")
            (should (= (+agent-acp-pending-prompt-count buffer) 1))
            (+agent-acp-stop buffer)
            (+agent-acp-stop buffer)
            (agent-flow-e2e--idle buffer)
            (should (= (+agent-acp-pending-prompt-count buffer) 1))
            (+agent-queue-resume buffer)
            (agent-flow-e2e--idle buffer)
            (should (string-match-p "Reply: queued after cancel" (agent-flow-e2e--text buffer)))
            ;; Unsaved draft and original session survive a real process restart.
            (agent-shell-insert :text "unsent draft" :submit nil :no-focus t :shell-buffer buffer)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (setq buffer (+agent-acp-restart-native buffer)))
            (should (buffer-live-p buffer))
            (agent-flow-e2e--wait (lambda () (+agent-acp-state-value buffer '(:init-finished))))
            (should (equal id (+agent-acp-state-value buffer '(:session :id))))
            (should (equal "unsent draft" (string-trim (with-current-buffer buffer (agent-shell--prompt-input)))))
            (should (string-match-p "Restored: hello" (agent-flow-e2e--text buffer)))
            ;; Clear the draft deliberately, then lose transport mid-turn.
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (delete-region (+agent-acp--input-start buffer) (point-max))))
            (agent-flow-e2e--send buffer "crash")
            (agent-flow-e2e--wait (lambda () (not (+agent-session-live-p buffer))))
            (should (memq (plist-get (+agent-session-snapshot buffer) :process-state) '(dead disconnected)))
            (let* ((events (with-temp-buffer
                             (insert-file-contents (concat store ".events")) (buffer-string))))
              (should (= 1 (cl-count-if (lambda (line) (string-match-p "\"method\":\"session/cancel\"" line))
                                        (split-string events "\n" t))))
              (should (= 1 (cl-count-if (lambda (line) (string-match-p "\"text\":\"queued after cancel\"" line))
                                        (split-string events "\n" t))))))
        (dolist (candidate (delete-dups (list buffer (get-buffer name))))
          (when (buffer-live-p candidate)
            (with-current-buffer candidate
              (let ((kill-buffer-query-functions nil)) (kill-buffer candidate)))))
        (delete-directory directory t)))))

(provide 'agent-flow-e2e)
