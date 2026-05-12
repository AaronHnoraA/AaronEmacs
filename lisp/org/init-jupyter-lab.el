;;; init-jupyter-lab.el --- Compatibility entry for moved JupyterLab config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(unless (featurep 'init-jupyter-lab)
  (load (expand-file-name
         "../init-jupyter-lab.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil 'nomessage))

(provide 'init-jupyter-lab)
;;; init-jupyter-lab.el ends here
