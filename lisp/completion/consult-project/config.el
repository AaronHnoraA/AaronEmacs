;;; completion/consult-project/config.el --- Consult + custom project roots -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(declare-function +project-current-root "tools/project/config")
(declare-function +project-read-known-project "tools/project/config" (&optional prompt))
(defvar consult-project-function)

(defun +consult-project-function (may-prompt)
  "Return the current project root for Consult.
When MAY-PROMPT is non-nil and there is no current project, prompt using the
custom project workflow."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      (when (and may-prompt
                 (fboundp '+project-read-known-project))
        (+project-read-known-project "Consult project: "))))

(with-eval-after-load 'consult
  (setq consult-project-function #'+consult-project-function))

(provide 'completion/consult-project/config)
;;; completion/consult-project/config.el ends here
