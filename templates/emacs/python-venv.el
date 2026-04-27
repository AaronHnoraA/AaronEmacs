;; Python project using a local .venv virtualenv
((nil . ((my/project-local-settings
          . (:env (("PYTHONPATH" . "src"))
             :test "pytest -x"))))
 (python-ts-mode
  . ((python-shell-interpreter . ".venv/bin/python3")
     (python-shell-interpreter-args . "")
     (eglot-workspace-configuration
      . (:python (:pythonPath ".venv/bin/python3")
         :pylsp (:plugins (:jedi (:environment ".venv")))))))
 (python-mode
  . ((python-shell-interpreter . ".venv/bin/python3"))))
