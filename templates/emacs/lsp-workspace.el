;; Custom LSP/Eglot workspace configuration
;; Adjust :eglot-workspace to match the server's expected schema.
((nil . ((my/project-local-settings
          . (:language-server eglot
             :eglot-workspace
             (:pyright (:pythonPath ".venv/bin/python3"
                        :venvPath   "."
                        :venv       ".venv")))))))
