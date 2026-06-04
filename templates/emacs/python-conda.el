;; Python project using a conda environment
;; Replace "myenv" with your actual conda environment name.
((nil . ((my/project-local-settings
          . (:env (("CONDA_DEFAULT_ENV" . "myenv")
                   ("PYTHONPATH"        . "src"))
             :test "conda run -n myenv pytest -x"
             :task (("activate" . "conda activate myenv"))))))
 (python-ts-mode
  . ((python-shell-interpreter . "conda")
     (python-shell-interpreter-args . "run -n myenv python3")
     (eglot-workspace-configuration
      . (:python (:pythonPath "~/miniconda3/envs/myenv/bin/python3")))))
 (python-mode
  . ((python-shell-interpreter . "conda")
     (python-shell-interpreter-args . "run -n myenv python3"))))
