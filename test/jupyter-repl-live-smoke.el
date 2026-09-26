;;; jupyter-repl-live-smoke.el --- Exercise the Board's actual REPL launcher -*- lexical-binding: t; -*-
(require 'init-jupyter-management)
(require 'jupyter-repl)
(let ((result-file (make-temp-file "jupyter-repl-live-")) client)
  (unwind-protect
      (with-temp-buffer
        (python-mode)
        (setq client (my/jupyter-management-run-repl
                      (list :kind 'kernelspec :target-id "local"
                            :name (or (getenv "JUPYTER_REPL_LIVE_KERNEL") "python3"))
                      (current-buffer)))
        (let ((request
               (with-current-buffer (oref client buffer)
                 (goto-char (point-max))
                 (jupyter-repl-replace-cell-code
                  (format "from pathlib import Path; Path(%s).write_text(str(6 * 7))"
                          (json-encode-string result-file)))
                 (jupyter-repl-execute-cell))))
          (jupyter-wait-until-idle request 30))
        (unless (equal (with-temp-buffer (insert-file-contents result-file) (buffer-string)) "42")
          (error "The Board REPL did not execute code"))
        (princ "PASS Board Open REPL, kernel startup, execution and shutdown\n"))
    (when client (ignore-errors (jupyter-shutdown-kernel client)))
    (ignore-errors (jupyter--gc-kernel-processes))
    (delete-file result-file)))
