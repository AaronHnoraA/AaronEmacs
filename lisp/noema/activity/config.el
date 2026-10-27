;;; noema/activity/config.el --- Noema activity snapshots -*- lexical-binding: t -*-

;;; Commentary:
;; Read Noema's existing owners: active Core tasks over the asynchronous API,
;; and busy agent sessions through the ACP boundary.  The Node host and idle
;; ACP processes remain services.  An export's private ACP turn belongs to
;; the export task and is never counted a second time.

;;; Code:

(require 'ui/activity/config)

(declare-function +noema-api-call "noema/bridge/config"
                  (channel args callback &optional timeout))
(declare-function +noema/open-file "noema/bridge/config" (file))
(declare-function noema-agent-acp-sessions "noema-agent-acp" (&optional root))
(declare-function noema-agent-acp-show-buffer "noema-agent-acp" (buffer))
(declare-function noema-agent-acp-stop "noema-agent-acp" (&optional buffer))
(declare-function aaron-ui-float-panel-for-buffer "aaron-ui-float" (buffer))
(declare-function aaron-ui-float-visible-p "aaron-ui-float" (panel))

(defvar +noema--ready)
(defvar +noema--process)

(defvar +activity-noema--tasks nil
  "Cached activities from Noema's active Core task pool.")

(defvar +activity-noema--agents (make-hash-table :test #'eq :weakness 'key)
  "Busy agent buffers mapped to their cached activity.")

(defvar +activity-noema--pending nil
  "Identity of the outstanding task-list request, or nil.")

(defun +activity-noema--snapshot ()
  "Return cached Noema work, without IO or session enumeration."
  (append +activity-noema--tasks (hash-table-values +activity-noema--agents)))

(defun +activity-noema--changed ()
  "Recount and update a visible monitor after a Noema snapshot changes."
  (+activity--refresh)
  (when-let* ((buffer (get-buffer "*Performance Watch*")))
    (let ((panel (and (featurep 'aaron-ui-float)
                      (aaron-ui-float-panel-for-buffer buffer))))
      (when (and (if panel
                     (aaron-ui-float-visible-p panel)
                   (get-buffer-window buffer t))
                 (with-current-buffer buffer
                   (and (bound-and-true-p +performance--tab-state)
                        (memq (etaf-value +performance--tab-state)
                              '(activity processes)))))
        (with-current-buffer buffer
          (+activity-board/refresh))))))

(defun +activity-noema--update-agents (&optional _buffer)
  "Refresh busy sessions through Noema's ACP boundary.
The abnormal changed hook passes _BUFFER; polling also catches buffer death
and a Run acquiring a session before agent-shell updates its header."
  (let ((previous +activity-noema--agents)
        (current (make-hash-table :test #'eq :weakness 'key)))
    (when (featurep 'noema-agent-acp)
      (dolist (session (noema-agent-acp-sessions))
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
              :visit (apply-partially #'noema-agent-acp-show-buffer buffer)
              :cancel (apply-partially #'noema-agent-acp-stop buffer))
             current)))))
    (setq +activity-noema--agents current)
    (unless (equal (hash-table-values previous) (hash-table-values current))
      (+activity-noema--changed))))

(defun +activity-noema--cancel-task (id)
  "Cancel Core task ID through its owner, then refresh the snapshot."
  (+noema-api-call
   "noema:api:tasks:cancel" (vector `((id . ,id)))
   (lambda (result error-object)
     (if (or error-object (not (eq (alist-get 'ok result) t)))
         (message "Noema task cancellation failed: %s"
                  (or (alist-get 'message error-object)
                      (alist-get 'message result) "Noema is unavailable"))
       (+activity-noema--poll)))
   5))

(defun +activity-noema--task-activity (task)
  "Project Core TASK onto an activity with native cancellation."
  (let* ((id (alist-get 'id task))
         (file (alist-get 'file (alist-get 'metadata task)))
         (since (or (alist-get 'startedAt task) (alist-get 'createdAt task))))
    (+activity--create
     :label (format "Noema: %s" (or (alist-get 'title task) "Task"))
     :kind 'task
     :started-at (and (stringp since) (not (string-empty-p since))
                      (ignore-errors (float-time (date-to-time since))))
     :detail (string-join
              (delq nil (list (alist-get 'kind task) (alist-get 'status task)
                              (alist-get 'phase task))) " · ")
     :visit (when (and (stringp file) (not (string-empty-p file)))
              (apply-partially #'+noema/open-file file))
     :cancel (when (eq (alist-get 'cancellable task) t)
               (apply-partially #'+activity-noema--cancel-task id)))))

(defun +activity-noema--poll ()
  "Refresh sessions and fetch Core tasks at most once per outstanding request.
This never starts Noema and never waits on the gateway.  Responses from a
previous host or a disabled indicator cannot resurrect old work."
  (+activity-noema--update-agents)
  (cond
   ((not (and (bound-and-true-p +noema--ready)
              (fboundp '+noema-api-call)))
    (setq +activity-noema--pending nil)
    (when +activity-noema--tasks
      (setq +activity-noema--tasks nil)
      (+activity-noema--changed)))
   ((not +activity-noema--pending)
    (let ((token (gensym "noema-activity"))
          (host +noema--process))
      (setq +activity-noema--pending token)
      (condition-case nil
          (+noema-api-call
           "noema:api:tasks:list" [((activeOnly . t))]
           (lambda (result error-object)
             (when (eq token +activity-noema--pending)
               (setq +activity-noema--pending nil)
               (when (and (eq host +noema--process)
                          (bound-and-true-p +noema--ready))
                 (let ((previous +activity-noema--tasks))
                   (setq +activity-noema--tasks
                         (unless error-object
                           (mapcar #'+activity-noema--task-activity
                                   (seq-filter
                                    (lambda (task)
                                      (member (alist-get 'status task)
                                              '("queued" "running" "canceling")))
                                    (append (alist-get 'tasks result) nil)))))
                   (unless (equal previous +activity-noema--tasks)
                     (+activity-noema--changed))))))
           5)
        (error (setq +activity-noema--pending nil
                     +activity-noema--tasks nil)
               (+activity-noema--changed)))))))

(defun +activity-noema--host-stopped (&rest _)
  "Discard the host snapshot and invalidate outstanding replies."
  (unless (bound-and-true-p +noema--ready)
    (setq +activity-noema--tasks nil
          +activity-noema--pending nil)
    (+activity-noema--changed)))

(defun +activity-noema--mode-changed ()
  "Invalidate pending replies when the indicator is disabled."
  (unless +activity-indicator-mode
    (setq +activity-noema--pending nil)))

(add-hook '+activity-provider-functions #'+activity-noema--snapshot)
(add-hook '+activity-poll-functions #'+activity-noema--poll)
(add-hook '+activity-indicator-mode-hook #'+activity-noema--mode-changed)

(with-eval-after-load 'noema-agent-acp
  (add-hook 'noema-agent-acp-changed-functions #'+activity-noema--update-agents)
  (+activity-noema--update-agents))

(with-eval-after-load 'noema/bridge/config
  (add-hook '+noema-host-ready-functions #'+activity-noema--poll)
  (advice-add '+noema/stop :after #'+activity-noema--host-stopped)
  (advice-add '+noema--sentinel :after #'+activity-noema--host-stopped))

(provide 'noema/activity/config)
;;; noema/activity/config.el ends here
