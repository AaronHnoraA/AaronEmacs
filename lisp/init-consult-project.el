;;; init-consult-project.el --- Consult + custom project roots -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/project-current-root "init-project")
(declare-function my/project-read-known-project "init-project" (&optional prompt))
(defvar consult-project-function)

(defun my/consult-project-function (may-prompt)
  "Return the current project root for Consult.
When MAY-PROMPT is non-nil and there is no current project, prompt using the
custom project workflow."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when (and may-prompt
                 (fboundp 'my/project-read-known-project))
        (my/project-read-known-project "Consult project: "))))

(with-eval-after-load 'consult
  (setq consult-project-function #'my/consult-project-function))

(provide 'init-consult-project)
;;; init-consult-project.el ends here
