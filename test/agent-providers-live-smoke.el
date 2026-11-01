;;; agent-providers-live-smoke.el --- Native startup without model prompts -*- lexical-binding: t; -*-
;; Run with the repository init loaded; writes only a bounded report to var/.
(require 'tools/agent/acp)
(require 'json)
(let ((directory (file-name-as-directory (make-temp-file "agent-provider-startup-" t))) results)
  (unwind-protect
      (dolist (provider '(codex claude opencode))
        (let (buffer)
          (unwind-protect
              (condition-case err
                  (progn
                    (setq buffer (+agent-acp-start :config (+agent-acp-config-for provider)
                                                  :directory directory :focus nil :origin 'manual))
                    (let ((deadline (+ (float-time) 30)))
                      (while (and (< (float-time) deadline)
                                  (not (+agent-acp-state-value buffer '(:init-finished))))
                        (accept-process-output nil 0.05)))
                    (push (list :provider provider :initialized (and (+agent-acp-state-value buffer '(:init-finished)) t)
                                :live (and (+agent-acp-state-value buffer '(:client :process))
                                           (process-live-p (+agent-acp-state-value buffer '(:client :process))) t)
                                :models (length (+agent-acp-available-model-ids buffer))
                                :capabilities (plist-get (+agent-session-snapshot buffer) :capabilities)
                                :prompts-sent 0) results))
                (error (push (list :provider provider :error (error-message-string err) :prompts-sent 0) results)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))))
    (delete-directory directory t))
  (with-temp-file (expand-file-name "var/agent-flow-providers.json" user-emacs-directory)
    (insert (json-encode (vconcat (nreverse results))))))
