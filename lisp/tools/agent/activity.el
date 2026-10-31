;;; tools/agent/activity.el --- Shared agent activity provider -*- lexical-binding: t; -*-
(require 'ui/activity/config)
(declare-function +agent-acp-sessions "tools/agent/acp" (&optional root))
(declare-function +agent-acp-show-buffer "tools/agent/acp" (buffer))
(declare-function +agent-acp-stop "tools/agent/acp" (&optional buffer))

(defvar +agent-activity--agents (make-hash-table :test #'eq :weakness 'key)
  "Busy agent buffers mapped to their cached activity.")
(defun +agent-activity--snapshot ()
  "Return cached activity without scanning buffers."
  (hash-table-values +agent-activity--agents))

(defun +agent-activity--changed ()
  "Recount and update a visible monitor after the agent snapshot changes."
  (+activity-source-changed))

(defun +agent-activity--update (&optional _buffer)
  "Refresh busy sessions through the shared ACP boundary.
The abnormal changed hook passes _BUFFER; polling also catches buffer death
and a Run acquiring a session before agent-shell updates its header."
  (let ((previous +agent-activity--agents)
        (current (make-hash-table :test #'eq :weakness 'key)))
    (when (featurep 'tools/agent/acp)
      (dolist (session (+agent-acp-sessions))
        (let ((buffer (plist-get session :buffer)))
          (when (and (buffer-live-p buffer) (plist-get session :busy)
                     (not (eq (plist-get session :origin) 'latex-export)))
            (puthash
             buffer
             (+activity--create
              :label (format "Agent: %s" (or (plist-get session :name)
                                            (plist-get session :agent) "agent"))
              :kind 'task :buffer buffer
              :started-at (if-let* ((activity (gethash buffer previous)))
                              (+activity-started-at activity)
                            (float-time))
              :detail (format "%s · %s" (or (plist-get session :agent) "agent")
                              (or (plist-get session :root) ""))
              :visit (apply-partially #'+agent-acp-show-buffer buffer)
              :cancel (apply-partially #'+agent-acp-stop buffer))
             current)))))
    (setq +agent-activity--agents current)
    (unless (equal (hash-table-values previous) (hash-table-values current))
      (+agent-activity--changed))))

(add-hook '+activity-provider-functions #'+agent-activity--snapshot)
(add-hook '+activity-poll-functions #'+agent-activity--update)
(with-eval-after-load 'tools/agent/acp
  (add-hook '+agent-acp-changed-functions #'+agent-activity--update)
  (+agent-activity--update))
(provide 'tools/agent/activity)
