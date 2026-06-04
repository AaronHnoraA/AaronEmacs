;;; init-jupyter-core.el --- Compatibility entry for moved Jupyter core -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(unless (featurep 'init-jupyter-core)
  (load (expand-file-name
         "../init-jupyter-core.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil 'nomessage))

(provide 'init-jupyter-core)
;;; init-jupyter-core.el ends here
