;;; tools/agent/upstream.el --- Emacs-owned agent implementation sources -*- lexical-binding: t; -*-

;;; Code:

(defconst +agent-upstream-root
  (expand-file-name "../../../site-lisp/" (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the complete gptel and Magent sources maintained by Emacs.")

(defun +agent-upstream-activate ()
  "Expose the complete shared implementations without loading Noema."
  (dolist (relative '("magent/lisp" "gptel"))
    (let ((directory (file-name-as-directory (expand-file-name relative +agent-upstream-root))))
      (when (file-directory-p directory)
        (add-to-list 'load-path directory)))))

(+agent-upstream-activate)
(provide 'tools/agent/upstream)
;;; tools/agent/upstream.el ends here
