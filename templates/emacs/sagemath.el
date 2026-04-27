;; SageMath project — Sage scripts, doctests, and Jupyter/Org Babel integration
;; Adjust SAGE_ROOT to wherever SageMath is installed (nix: $(which sage | head -1)).
((nil . ((my/project-local-settings
          . (:env  (("SAGE_ROOT" . "/usr/local/share/sage"))
             :test "sage -tp ."
             :task (("doctest"    . "sage -tp .")
                   ("build-dist" . "sage setup.py build_dist")
                   ("notebook"   . "sage --notebook=jupyter"))
             :run  (("sage" . "sage"))))))
 (python-ts-mode
  . ((python-shell-interpreter . "sage")
     (python-shell-interpreter-args . "--simple-prompt")
     (eglot-workspace-configuration
      . (:python (:pythonPath "sage")))))
 (python-mode
  . ((python-shell-interpreter . "sage")
     (python-shell-interpreter-args . "--simple-prompt"))))
