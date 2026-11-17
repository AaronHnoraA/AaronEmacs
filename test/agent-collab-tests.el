;;; agent-collab-tests.el --- Document collaboration: authorship, threads, direct changes -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/acp)
(require 'tools/agent/sessions)
(require 'tools/agent/binding)

(defmacro agent-collab-tests--agent-buffer (var &rest body)
  "Bind VAR to a fake agent buffer that `+agent-acp-agent-buffer-p' accepts."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer "*agent collab fixture*")))
     (unwind-protect
         (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                    (lambda (buffer) (eq buffer ,var))))
           (with-current-buffer ,var
             (setq-local default-directory "/tmp/" +agent-acp-session-root "/tmp/"
                         +agent-acp-session-name "claude·SAI" +agent-acp-session-agent "claude"))
           ,@body)
       (kill-buffer ,var))))

;;;; C66: an Agent process knows which session it is.

(ert-deftest agent-collab-c66-client-environment-names-the-session ()
  (agent-collab-tests--agent-buffer buffer
    (let* ((args (+agent-acp--session-environment-a
                  (list :context-buffer buffer :command "claude" :environment-variables '("A=1"))))
           (token (+agent-acp-session-token buffer)))
      (should (stringp token))
      (should (member (concat "NOEMA_AGENT_SESSION=" token) (plist-get args :environment-variables)))
      (should (member "A=1" (plist-get args :environment-variables)))
      ;; A restart builds a new client for the same session: same token.
      (should (equal (+agent-acp-session-token buffer) token))
      (should (equal (+agent-acp-session-buffer-for-token token) buffer))))
  (with-temp-buffer
    (should (equal (+agent-acp--session-environment-a
                    (list :context-buffer (current-buffer) :command "x"))
                   (list :context-buffer (current-buffer) :command "x")))))

(ert-deftest agent-collab-c66-latex-repair-names-the-document-tools ()
  "The export-repair session edits the note through Noema, so its page follows."
  (require 'noema/bridge/config)
  (let ((prompt (+noema--latex-intervention-prompt
                 '((file . "/tmp/n.md") (outputPath . "/tmp/n.tex") (error . "boom")))))
    (should (string-match-p "agent:document:edit" prompt))
    (should (string-match "node \\([^ `]+agent-api\\.mjs\\)" prompt))
    (should (file-exists-p (match-string 1 prompt)))))

(provide 'agent-collab-tests)
;;; agent-collab-tests.el ends here
