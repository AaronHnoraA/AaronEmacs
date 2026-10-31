;;; tools/agent/compose.el --- Shared gptel composition surface -*- lexical-binding: t; -*-
(require 'tools/agent/upstream)
(require 'gptel)
(require 'tools/agent/context)

;;;###autoload
(defun +agent/compose (&optional name)
  "Open the shared gptel writing surface, with optional domain context."
  (interactive)
  (unless (and (called-interactively-p 'any) (+agent-editor-action 'compose))
    (gptel (or name "Agent Compose") nil nil t)))

;;;###autoload
(defun +agent/compose-send (&optional arg)
  "Send from this buffer through gptel or its domain editor."
  (interactive "P")
  (unless (+agent-editor-action 'compose-send arg) (gptel-send arg)))

;;;###autoload
(defun +agent/compose-menu ()
  "Open gptel's complete settings transient."
  (interactive) (require 'gptel-transient) (gptel-menu))

;;;###autoload
(defun +agent/compose-add-context (&optional arg)
  "Add to the one shared gptel selection, with optional domain adaptation."
  (interactive "P")
  (unless (+agent-editor-action 'add-context arg) (require 'gptel-context) (gptel-add arg)))

;;;###autoload
(defun +agent/compose-rewrite ()
  "Review a gptel rewrite using the existing native diff surface."
  (interactive)
  (unless (+agent-editor-action 'rewrite) (require 'gptel-rewrite) (gptel-rewrite)))

(provide 'tools/agent/compose)
