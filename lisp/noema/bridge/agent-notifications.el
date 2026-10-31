;;; noema/bridge/agent-notifications.el --- Research events to Emacs notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Noema emits domain events; Emacs owns delivery, coalescing and UI policy.
;; Loading this bridge does not load Research or start a host/agent.

;;; Code:

(require 'aaron-lib)
(require 'tools/agent/notifications)

(defvar +agent-acp-notify-function)
(defvar +agent-acp-ephemeral-origins)
(defvar noema-agent-worker-notify-function)
(declare-function noema-agent-worker-owns-buffer-p "noema-agent-worker" (buffer))
(declare-function noema-agent-worker-buffer "noema-agent-worker" (worker))
(declare-function noema-agent-worker-run-id "noema-agent-worker" (worker))
(declare-function noema-agent-worker-pending-permissions "noema-agent-worker" (worker))
(declare-function noema-agent-worker-pending-inputs "noema-agent-worker" (worker))
(declare-function noema-agent-worker--session-name "noema-agent-worker" (worker))

(defun +noema-agent-notifications--inhibit (buffer _kind)
  "A Research Run owns its semantic events until terminal acknowledgement."
  (and (fboundp 'noema-agent-worker-owns-buffer-p)
       (noema-agent-worker-owns-buffer-p buffer)))

(defun +noema-agent-notifications--worker (worker reason)
  "Adapt WORKER's semantic REASON after its own state transition."
  (when (memq noema-agent-worker-notify-function '(nil noema-agent-worker-notify-default))
    (when (and noema-agent-worker-notify-function
               (buffer-live-p (noema-agent-worker-buffer worker)))
      (+agent-notify-event
       (noema-agent-worker-buffer worker) (if (eq reason 'interrupted) 'failed reason)
       :id (noema-agent-worker-run-id worker)
       :label (noema-agent-worker--session-name worker)
       :source "Noema Research"))
    t))

(add-hook '+agent-notify-inhibit-functions #'+noema-agent-notifications--inhibit)

(after! noema-agent-worker
  (add-hook 'noema-agent-worker-notification-functions #'+noema-agent-notifications--worker))

(provide 'noema/bridge/agent-notifications)
;;; noema/bridge/agent-notifications.el ends here
