;;; beancount-tool.el --- bean-tool subprocess plumbing -*- lexical-binding: t -*-

;;; Commentary:
;; Async/sync wrappers around the repo-local `bean-tool` CLI (see
;; tools/cli.py in the ledger repo).  Every subcommand prints exactly one
;; JSON object to stdout ({"error": "..."} + exit 1 on failure); this file
;; is the only place that knows how to invoke it and parse the result.
;; beancount-dashboard.el / beancount-entry.el / beancount-frame.el all go
;; through `my/beancount-run-tool' or `my/beancount-run-tool-sync'.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup my/beancount-tool nil
  "bean-tool subprocess integration."
  :group 'my/beancount)

(defcustom my/beancount-ledger-root "~/Documents/Beancount"
  "Root directory of the Beancount ledger repo."
  :type 'directory
  :group 'my/beancount-tool)

(defcustom my/beancount-fava-url "http://10.31.2.53:5558/我的账本/income_statement/"
  "URL of the NAS-hosted Fava report shown in the left pane of `my/beancount'."
  :type 'string
  :group 'my/beancount-tool)

(defun my/beancount--root ()
  "Return the expanded ledger root directory."
  (expand-file-name my/beancount-ledger-root))

(defun my/beancount--tool-executable ()
  "Return the path to bean-tool inside the ledger's venv, or signal a user-error."
  (let ((exe (expand-file-name ".venv/bin/bean-tool" (my/beancount--root))))
    (unless (file-executable-p exe)
      (user-error "bean-tool not found at %s -- run `make venv` in %s"
                  exe (my/beancount--root)))
    exe))

(defun my/beancount--parse-json (text)
  "Parse TEXT (bean-tool's stdout) into an alist, or signal a user-error."
  (condition-case err
      (json-parse-string text :object-type 'alist :array-type 'list :null-object nil)
    (error (user-error "bean-tool returned invalid JSON: %s (%s)" err text))))

(cl-defun my/beancount-run-tool (args callback &key stdin)
  "Run bean-tool ARGS asynchronously, calling CALLBACK with the parsed result.
ARGS is a list of CLI argument strings (e.g. (\"summary\" \"--month\" \"2025-06\")).
When STDIN is non-nil, it is written to the process and then EOF is sent."
  (let* ((exe (my/beancount--tool-executable))
         (default-directory (my/beancount--root))
         (buf (generate-new-buffer " *bean-tool*"))
         (proc (make-process
                :name "bean-tool"
                :buffer buf
                :command (cons exe args)
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (proc _event)
                  (unless (process-live-p proc)
                    (let ((output (with-current-buffer (process-buffer proc)
                                    (buffer-string))))
                      (kill-buffer (process-buffer proc))
                      (funcall callback (my/beancount--parse-json output))))))))
    (when stdin
      (process-send-string proc stdin)
      (process-send-eof proc))
    proc))

(cl-defun my/beancount-run-tool-sync (args &key stdin)
  "Synchronous variant of `my/beancount-run-tool', returning the parsed result."
  (let ((exe (my/beancount--tool-executable))
        (default-directory (my/beancount--root)))
    (with-temp-buffer
      (if stdin
          (progn
            (insert stdin)
            (apply #'call-process-region (point-min) (point-max) exe t t nil args))
        (apply #'call-process exe nil t nil args))
      (my/beancount--parse-json (buffer-string)))))

(provide 'beancount-tool)
;;; beancount-tool.el ends here
