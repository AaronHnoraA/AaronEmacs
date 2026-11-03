;;; tools/agent/activity.el --- Shared agent activity provider -*- lexical-binding: t; -*-
(require 'ui/activity/config)
(declare-function +agent-acp-agent-buffer-p "tools/agent/acp" (buffer))
(declare-function +agent-acp-busy-p "tools/agent/acp" (buffer))
(declare-function +agent-acp-show-buffer "tools/agent/acp" (buffer))
(declare-function +agent-acp-stop "tools/agent/acp" (&optional buffer))
(defvar +agent-acp-session-name)
(defvar +agent-acp-session-agent)
(defvar +agent-acp-session-root)
(defvar +agent-acp-session-origin)

(defvar +agent-activity--agents (make-hash-table :test #'eq :weakness 'key)
  "Busy agent buffers mapped to their cached activity.")
(defun +agent-activity--snapshot ()
  "Return cached activity without scanning buffers."
  (hash-table-values +agent-activity--agents))

(defun +agent-activity--changed ()
  "Recount and update a visible monitor after the agent snapshot changes."
  (+activity-source-changed))

(defun +agent-activity--update-buffer (buffer)
  "Update only BUFFER's cached activity; return non-nil if it changed.
Busy state comes from ACP's execution facts, never a full session snapshot."
  (let* ((old (gethash buffer +agent-activity--agents))
         (busy (and (featurep 'tools/agent/acp)
                    (+agent-acp-agent-buffer-p buffer)
                    (not (eq (buffer-local-value '+agent-acp-session-origin buffer)
                             'latex-export))
                    (+agent-acp-busy-p buffer))))
    (if (not busy)
        (when old (remhash buffer +agent-activity--agents) t)
      (with-current-buffer buffer
        (let ((label (format "Agent: %s" (or +agent-acp-session-name
                                             +agent-acp-session-agent "agent")))
              (detail (format "%s · %s" (or +agent-acp-session-agent "agent")
                              (or +agent-acp-session-root ""))))
          (unless (and old (equal label (+activity-label old))
                       (equal detail (+activity-detail old)))
            (puthash
             buffer
             (+activity--create
              :label label :detail detail :kind 'task :buffer buffer
              :started-at (if old (+activity-started-at old) (float-time))
              :visit (apply-partially #'+agent-acp-show-buffer buffer)
              :cancel (apply-partially #'+agent-acp-stop buffer))
             +agent-activity--agents)
            t))))))

(defun +agent-activity--update (&optional buffer)
  "Refresh BUFFER on an ACP event, or reconcile minimal facts when polling.
One event never scans other sessions.  Polling still catches buffer death and
a Run acquiring a session before its next native header update."
  (let (changed)
    (if buffer
        (setq changed (+agent-activity--update-buffer buffer))
      (dolist (candidate (delete-dups
                          (append (hash-table-keys +agent-activity--agents)
                                  (buffer-list))))
        (when (+agent-activity--update-buffer candidate) (setq changed t))))
    (when changed (+agent-activity--changed))))

(add-hook '+activity-provider-functions #'+agent-activity--snapshot)
(add-hook '+activity-poll-functions #'+agent-activity--update)
(with-eval-after-load 'tools/agent/acp
  (add-hook '+agent-acp-changed-functions #'+agent-activity--update)
  (+agent-activity--update))
(provide 'tools/agent/activity)
