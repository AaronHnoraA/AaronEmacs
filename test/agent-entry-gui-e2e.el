;;; agent-entry-gui-e2e.el --- The daily path through `agent-shell' -*- lexical-binding: t; -*-

;; The owner enters agents through the `agent-shell' command.  This walks that
;; path with real keys in a real GUI frame against a real ACP client.

(require 'agent-flow-e2e)
(require 'aaron-ui-float)
(require 'tools/agent/queue)

(defun agent-entry-gui-e2e--key (keys)
  (execute-kbd-macro (kbd keys)))

(defun agent-entry-gui-e2e--type (text)
  "Type TEXT at the selected chat's input and press RET."
  (goto-char (point-max))
  (execute-kbd-macro (concat text "\r")))

(defun agent-entry-gui-e2e-run ()
  "Return a report of the daily path, from an isolated GUI frame."
  (unless (display-graphic-p) (error "This probe requires a GUI frame"))
  (let* ((directory (file-name-as-directory (make-temp-file "agent-entry-gui-" t)))
         (store (expand-file-name "sessions.json" directory))
         (config (agent-flow-e2e--config store))
         (agent-shell-busy-submit-default-function #'agent-shell-busy-submit-queue)
         (agent-shell-confirm-interrupt nil)
         (+agent-acp-notify-function nil)
         (source (get-buffer-create "Entry GUI source"))
         chat picks result)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-config-for) (lambda (_agent) (copy-tree config)))
                  ((symbol-function '+agent-acp-project-root) (lambda (&rest _) directory))
                  ((symbol-function 'completing-read)
                   (lambda (prompt choices &rest _)
                     (push prompt picks)
                     (let ((names (mapcar (lambda (c) (if (consp c) (car c) c)) choices)))
                       (or (seq-find (lambda (n) (string-match-p "New agent" n)) names)
                           (car names))))))
          (set-frame-size nil 150 48)
          (delete-other-windows)
          (switch-to-buffer source)
          (setq default-directory directory)
          (let ((origin (selected-window)))
            ;; C-u: a new session here.
            (let ((current-prefix-arg '(4))) (call-interactively #'agent-shell))
            (setq chat (window-buffer (selected-window)))
            (set-buffer chat)
            (should (+agent-acp-agent-buffer-p chat))
            (agent-flow-e2e--wait (lambda () (+agent-acp-state-value chat '(:init-finished))))
            ;; Inside a chat the same command returns.
            (call-interactively #'agent-shell)
            (should (eq (window-buffer (selected-window)) source))
            (set-buffer source)
            (should (+agent-session-live-p chat))
            ;; And from the editor it recalls that session, creating nothing.
            (let ((sessions (length (+agent-pool-sessions directory))))
              (call-interactively #'agent-shell)
              (should (eq (window-buffer (selected-window)) chat))
              (set-buffer chat)
              (should (= sessions (length (+agent-pool-sessions directory)))))
            ;; Local commands at the real input, typed, during a turn.
            (agent-entry-gui-e2e--type "hold")
            (agent-flow-e2e--wait (lambda () (+agent-acp-busy-p chat)))
            (agent-entry-gui-e2e--type "later")
            (should (= 1 (+agent-acp-pending-prompt-count chat)))
            (agent-entry-gui-e2e--type "/queue")
            (let ((panel (aaron-ui-float-panel-for-buffer (get-buffer +agent-queue-buffer-name))))
              (should (aaron-ui-float-visible-p panel))
              (select-window (aaron-ui-float-content-window panel))
              (agent-entry-gui-e2e--key "q")
              (should-not (aaron-ui-float-visible-p panel)))
            (should (eq (window-buffer (selected-window)) chat))
            (set-buffer chat)
            ;; The native interrupt is the one stop, and it holds the queue.
            (should (eq (key-binding (kbd "C-c C-c")) #'+agent-acp-stop))
            (agent-entry-gui-e2e--key "C-c C-c")
            (agent-flow-e2e--wait (lambda () (not (+agent-acp-busy-p chat))))
            (should (= 1 (+agent-acp-held-prompt-count chat)))
            ;; `?' types at the input and opens every action elsewhere.
            (goto-char (point-max))
            (should (eq (key-binding (kbd "?")) #'+agent-acp-help-or-insert))
            (goto-char (point-min))
            (agent-entry-gui-e2e--key "?")
            (should (eq (transient-scope '+agent-session-menu) chat))
            (should (transient-get-suffix '+agent-session-menu "q"))
            (agent-entry-gui-e2e--key "C-g")
            (set-buffer chat)
            (goto-char (point-max))
            ;; Agent-shell keeps its own key.
            (should (eq (key-binding (kbd "C-c C-t")) #'agent-shell-set-session-thought-level))
            ;; /agents reaches the manager; the provider never saw any of this.
            (agent-entry-gui-e2e--type "/agents")
            (let ((panel (aaron-ui-float-panel-for-buffer (get-buffer +agent-manager-buffer-name))))
              (should (aaron-ui-float-visible-p panel))
              (select-window (aaron-ui-float-content-window panel))
              (agent-entry-gui-e2e--key "q"))
            (set-buffer (window-buffer (selected-window)))
            (let ((events (with-temp-buffer (insert-file-contents (concat store ".events"))
                                            (buffer-string))))
              (should-not (string-match-p "/queue\\|/agents" events)))
            ;; C-u C-u offers the pool, new and saved conversations in one list.
            (select-window origin)
            (switch-to-buffer source)
            (let ((current-prefix-arg '(16)) (choices nil))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt candidates &rest _)
                           (setq choices (mapcar #'car candidates))
                           (caar candidates))))
                (call-interactively #'agent-shell))
              (should (eq (window-buffer (selected-window)) chat))
              (should (seq-find (lambda (c) (string-match-p "New agent" c)) choices))
              (should (seq-find (lambda (c) (string-match-p "Resume saved" c)) choices)))
            (setq result (list :passed t :new-here t :return t :recall t :slash-during-turn t
                               :native-stop t :question-mark t :native-key t :manager t
                               :one-picker t))))
      (when-let* ((buffer (get-buffer +agent-manager-buffer-name))) (kill-buffer buffer))
      (when-let* ((buffer (get-buffer +agent-queue-buffer-name))) (kill-buffer buffer))
      (when (buffer-live-p chat)
        (with-current-buffer chat
          (let ((kill-buffer-query-functions nil)) (kill-buffer chat))))
      (kill-buffer source)
      (delete-directory directory t))
    result))

(provide 'agent-entry-gui-e2e)
;;; agent-entry-gui-e2e.el ends here
