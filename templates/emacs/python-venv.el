;; Python project using a local .venv virtualenv
((nil . ((+project-local-settings
          . (:toolchain ((python . "venv:.venv"))
             :env (("PYTHONPATH" . "src"))
             :test "pytest -x"))))
 )
