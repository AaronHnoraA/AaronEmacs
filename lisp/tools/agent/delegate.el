;;; delegate.el --- Background workers through the shared Agent API -*- lexical-binding: t; -*-
(require 'tools/agent/api)
(require 'tools/agent/sessions)
(require 'tools/agent/menu)

;;;###autoload
(defun +agent/spawn (agent name prompt root)
  "Run PROMPT in a background AGENT named NAME under ROOT."
  (interactive
   (list (completing-read "Worker provider: " '("codex" "claude" "opencode" "pi") nil t nil nil "codex")
         (read-string "Worker name: ")
         (read-string "Task: ")
         (read-directory-name "Task workspace: "
                              (with-current-buffer (or (+agent-menu--scope) (current-buffer))
                                (or +agent-acp-session-root default-directory)) nil t)))
  (let* ((source (+agent-menu--scope))
         (native (and source (+agent-acp-state-value source '(:session :id))))
         (reply (+agent/api `((action . "spawn") (root . ,root) (agent . ,agent)
                              ,@(unless (string-empty-p name) `((name . ,name))) (prompt . ,prompt)
                              ,@(when native `((notify . ((buffer . ,(buffer-name source)) (nativeSessionId . ,native)))))))))
    (message "Background task %s · %s; open it with agent-open-worker"
             (alist-get 'requestId reply) (alist-get 'state reply))
    reply))

;;;###autoload
(defun +agent/open-worker ()
  "Open an existing background worker without starting or resuming anything."
  (interactive)
  (let ((choices
         (mapcar (lambda (s)
                   (cons (format "%s · %s · %s" (plist-get s :name)
                                 (plist-get s :root) (+agent-sessions--state s)) (plist-get s :buffer)))
                 (seq-filter (lambda (s) (plist-get s :background)) (+agent-pool-sessions nil nil 'agent)))))
    (unless choices (user-error "No background worker buffers"))
    (+agent-menu--show (cdr (assoc (completing-read "Open worker: " choices nil t) choices)))))

;;;###autoload
(defalias 'agent-spawn #'+agent/spawn)
;;;###autoload
(defalias 'agent-open-worker #'+agent/open-worker)
(provide 'tools/agent/delegate)
;;; delegate.el ends here
