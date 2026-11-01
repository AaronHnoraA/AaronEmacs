;;; tools/agent/compose.el --- Agent document actions -*- lexical-binding: t; -*-
(require 'tools/agent/context)

;;;###autoload
(defun +agent/compose (&optional _name)
  "Ask the project Pi to create and open an ordinary Markdown task document."
  (interactive)
  (unless (+agent-editor-action 'compose)
    (require 'noema-pi-router)
    (noema-pi-request-prompt-document
     (or (bound-and-true-p +project-active-root) default-directory) buffer-file-name)))

;;;###autoload
(defun +agent/compose-send (&optional pick)
  "Send the saved Markdown task document to an existing Agent."
  (interactive "P")
  (unless (+agent-editor-action 'compose-send pick)
    (unless (and buffer-file-name (derived-mode-p 'markdown-mode))
      (user-error "Open a Markdown task document first"))
    (+agent-context--send
     :prompt "Carry out the task in this Markdown document, using its referenced context."
     :pick pick
     :references (+agent-context--file-references
                  (or (+agent-context--buffer-file (current-buffer))
                      (user-error "Save the task document first"))))))

;;;###autoload
(defun +agent/compose-menu ()
  "Open the ordinary Agent session menu."
  (interactive)
  (require 'tools/agent/menu)
  (call-interactively #'+agent-session-menu))

;;;###autoload
(defun +agent/compose-add-context (&optional arg)
  "Select editor context, using the active domain's range when available."
  (interactive "P")
  (unless (+agent-editor-action 'add-context arg)
    (+agent-selection-add arg)))

;;;###autoload
(defun +agent/compose-rewrite ()
  "Ask an Agent to revise the selected source. Noema uses API review cards."
  (interactive)
  (unless (+agent-editor-action 'rewrite)
    (+agent-context-send-region)))

(provide 'tools/agent/compose)
