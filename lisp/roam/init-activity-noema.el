;;; init-activity-noema.el --- Noema activity snapshots -*- lexical-binding: t -*-

;;; Commentary:
;; Read Noema's existing owners: active Core tasks over the asynchronous API,
;; and busy agent sessions through the ACP boundary.  The Node host and idle
;; ACP processes remain services.  An export's private ACP turn belongs to
;; the export task and is never counted a second time.

;;; Code:

(require 'init-activity)

(declare-function my/noema-api-call "init-aaronnote"
                  (channel args callback &optional timeout))
(declare-function my/noema-open-file "init-aaronnote" (file))
(declare-function noema-agent-acp-sessions "noema-agent-acp" (&optional root))
(declare-function noema-agent-acp-show-buffer "noema-agent-acp" (buffer))
(declare-function noema-agent-acp-stop "noema-agent-acp" (&optional buffer))

(defvar my/noema--ready)
(defvar my/noema--process)

(defvar my/activity-noema--tasks nil
  "Cached activities from Noema's active Core task pool.")

(defvar my/activity-noema--agents (make-hash-table :test #'eq :weakness 'key)
  "Busy agent buffers mapped to their cached activity.")

(defvar my/activity-noema--pending nil
  "Identity of the outstanding task-list request, or nil.")

(defun my/activity-noema--snapshot ()
  "Return cached Noema work, without IO or session enumeration."
  (append my/activity-noema--tasks (hash-table-values my/activity-noema--agents)))

(defun my/activity-noema--changed ()
  "Recount and update a visible activity board after a snapshot changes."
  (my/activity--refresh)
  (when-let* ((buffer (get-buffer "*Activity*"))
              ((get-buffer-window buffer t)))
    (with-current-buffer buffer
      (my/activity-board-refresh))))

(defun my/activity-noema--update-agents (&optional _buffer)
  "Refresh busy sessions through Noema's ACP boundary.
The abnormal changed hook passes _BUFFER; polling also catches buffer death
and a Run acquiring a session before agent-shell updates its header."
  (let ((previous my/activity-noema--agents)
        (current (make-hash-table :test #'eq :weakness 'key)))
    (when (featurep 'noema-agent-acp)
      (dolist (session (noema-agent-acp-sessions))
        (let ((buffer (plist-get session :buffer)))
          (when (and (buffer-live-p buffer) (plist-get session :busy)
                     (not (eq (plist-get session :origin) 'latex-export)))
            (puthash
             buffer
             (my/activity--create
              :label (format "Agent: %s" (or (plist-get session :name)
                                            (plist-get session :agent) "agent"))
              :kind 'task :buffer buffer
              :started-at (if-let* ((activity (gethash buffer previous)))
                              (my/activity-started-at activity)
                            (float-time))
              :detail (format "%s · %s" (or (plist-get session :agent) "agent")
                              (or (plist-get session :root) ""))
              :visit (apply-partially #'noema-agent-acp-show-buffer buffer)
              :cancel (apply-partially #'noema-agent-acp-stop buffer))
             current)))))
    (setq my/activity-noema--agents current)
    (unless (equal (hash-table-values previous) (hash-table-values current))
      (my/activity-noema--changed))))

(defun my/activity-noema--cancel-task (id)
  "Cancel Core task ID through its owner, then refresh the snapshot."
  (my/noema-api-call
   "aaronnote:api:tasks:cancel" (vector `((id . ,id)))
   (lambda (result error-object)
     (if (or error-object (not (eq (alist-get 'ok result) t)))
         (message "Noema task cancellation failed: %s"
                  (or (alist-get 'message error-object)
                      (alist-get 'message result) "Noema is unavailable"))
       (my/activity-noema--poll)))
   5))

(defun my/activity-noema--task-activity (task)
  "Project Core TASK onto an activity with native cancellation."
  (let* ((id (alist-get 'id task))
         (file (alist-get 'file (alist-get 'metadata task)))
         (since (or (alist-get 'startedAt task) (alist-get 'createdAt task))))
    (my/activity--create
     :label (format "Noema: %s" (or (alist-get 'title task) "Task"))
     :kind 'task
     :started-at (and (stringp since) (not (string-empty-p since))
                      (ignore-errors (float-time (date-to-time since))))
     :detail (string-join
              (delq nil (list (alist-get 'kind task) (alist-get 'status task)
                              (alist-get 'phase task))) " · ")
     :visit (when (and (stringp file) (not (string-empty-p file)))
              (apply-partially #'my/noema-open-file file))
     :cancel (when (eq (alist-get 'cancellable task) t)
               (apply-partially #'my/activity-noema--cancel-task id)))))

(defun my/activity-noema--poll ()
  "Refresh sessions and fetch Core tasks at most once per outstanding request.
This never starts Noema and never waits on the gateway.  Responses from a
previous host or a disabled indicator cannot resurrect old work."
  (my/activity-noema--update-agents)
  (cond
   ((not (and (bound-and-true-p my/noema--ready)
              (fboundp 'my/noema-api-call)))
    (setq my/activity-noema--pending nil)
    (when my/activity-noema--tasks
      (setq my/activity-noema--tasks nil)
      (my/activity-noema--changed)))
   ((not my/activity-noema--pending)
    (let ((token (gensym "noema-activity"))
          (host my/noema--process))
      (setq my/activity-noema--pending token)
      (condition-case nil
          (my/noema-api-call
           "aaronnote:api:tasks:list" [((activeOnly . t))]
           (lambda (result error-object)
             (when (eq token my/activity-noema--pending)
               (setq my/activity-noema--pending nil)
               (when (and (eq host my/noema--process)
                          (bound-and-true-p my/noema--ready))
                 (let ((previous my/activity-noema--tasks))
                   (setq my/activity-noema--tasks
                         (unless error-object
                           (mapcar #'my/activity-noema--task-activity
                                   (seq-filter
                                    (lambda (task)
                                      (member (alist-get 'status task)
                                              '("queued" "running" "canceling")))
                                    (append (alist-get 'tasks result) nil)))))
                   (unless (equal previous my/activity-noema--tasks)
                     (my/activity-noema--changed))))))
           5)
        (error (setq my/activity-noema--pending nil
                     my/activity-noema--tasks nil)
               (my/activity-noema--changed)))))))

(defun my/activity-noema--host-stopped (&rest _)
  "Discard the host snapshot and invalidate outstanding replies."
  (unless (bound-and-true-p my/noema--ready)
    (setq my/activity-noema--tasks nil
          my/activity-noema--pending nil)
    (my/activity-noema--changed)))

(defun my/activity-noema--mode-changed ()
  "Invalidate pending replies when the indicator is disabled."
  (unless my/activity-indicator-mode
    (setq my/activity-noema--pending nil)))

(add-hook 'my/activity-provider-functions #'my/activity-noema--snapshot)
(add-hook 'my/activity-poll-functions #'my/activity-noema--poll)
(add-hook 'my/activity-indicator-mode-hook #'my/activity-noema--mode-changed)

(with-eval-after-load 'noema-agent-acp
  (add-hook 'noema-agent-acp-changed-functions #'my/activity-noema--update-agents)
  (my/activity-noema--update-agents))

(with-eval-after-load 'init-aaronnote
  (add-hook 'my/noema-host-ready-functions #'my/activity-noema--poll)
  (advice-add 'my/noema-stop :after #'my/activity-noema--host-stopped)
  (advice-add 'my/noema--sentinel :after #'my/activity-noema--host-stopped))

(provide 'init-activity-noema)
;;; init-activity-noema.el ends here
