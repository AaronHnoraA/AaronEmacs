;;; jupyter-debug-live-smoke.el --- Real kernel, Noema and Dape debugging -*- lexical-binding: t; -*-
(require 'init-aaronnote-jupyter-debug)
(require 'dape)
(defvar my/jupyter-contents-live-extra nil)

(defun my/jupyter-debug-live--wait (predicate label)
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil .03))
    (unless (funcall predicate) (error "Timed out: %s" label))))

(defun my/jupyter-debug-live--request (conn command arguments)
  (let (settled reply failure)
    (dape-request conn command arguments
                  (lambda (body error) (setq reply body failure error settled t)))
    (my/jupyter-debug-live--wait (lambda () settled) (format "%s" command))
    (when failure (error "DAP %s: %s" command failure))
    reply))

(defun my/jupyter-debug-live--scenario (file)
  (let* ((origin (current-buffer))
         (cell (car (my/noema-jupyter-notebook-projection-cells)))
         (start (plist-get cell :body-beg))
         (line (plist-get cell :line))
         (dape-start-hook nil)
         (dape-stopped-hook nil))
    (delete-region start (plist-get cell :body-end))
    (goto-char start)
    (insert "audit_value = 10\naudit_value += 2\nprint(audit_value)")
    (save-buffer)
    (goto-char start)
    (forward-line 1)
    (dape-breakpoint-toggle)
    (message "Debug live: starting breakpoint session")
    (my/noema-jupyter-debug-start)
    (let ((conn (car dape--connections)))
      (my/jupyter-debug-live--wait
       (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) (1+ line)))
       "notebook breakpoint")
      (let* ((frame (dape--current-stack-frame conn))
             (scopes (my/jupyter-debug-live--request conn :scopes `(:frameId ,(plist-get frame :id))))
             (reference (plist-get (aref (plist-get scopes :scopes) 0) :variablesReference))
             (variables (my/jupyter-debug-live--request conn :variables `(:variablesReference ,reference))))
        (unless (equal (plist-get (plist-get frame :source) :path) file)
          (error "Stack frame did not map to the notebook"))
        (unless (cl-find-if (lambda (item) (and (equal (plist-get item :name) "audit_value")
                                               (equal (plist-get item :value) "10")))
                            (append (plist-get variables :variables) nil))
          (error "Paused-frame variables are wrong")))
      (dape-next conn)
      (my/jupyter-debug-live--wait
       (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) (+ 2 line)))
       "next statement")
      (let ((value (my/jupyter-debug-live--request
                    conn :evaluate `(:expression "audit_value" :context "repl"
                                     :frameId ,(plist-get (dape--current-stack-frame conn) :id)))))
        (unless (equal (plist-get value :result) "12") (error "Step/evaluate failed: %S" value)))
      (dape-continue conn)
      (my/jupyter-debug-live--wait
       (lambda () (not (buffer-local-value 'my/noema-jupyter-debug--id origin))) "debug completion"))
    (with-current-buffer origin
      (unless (not buffer-read-only) (error "Debugger left notebook read-only"))
      (dape-breakpoint-remove-all)
      (goto-char start)
      (message "Debug live: starting Run by Line")
      (my/noema-jupyter-run-by-line)
      (let ((conn (car dape--connections)))
        (my/jupyter-debug-live--wait
         (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) line)) "entry stop")
        (with-current-buffer origin (my/noema-jupyter-run-by-line))
        (my/jupyter-debug-live--wait
         (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) (1+ line))) "Run by Line next")
        (with-current-buffer origin (my/noema-jupyter-debug-continue))
        (my/jupyter-debug-live--wait
         (lambda () (not (buffer-local-value 'my/noema-jupyter-debug--id origin))) "Run by Line completion"))
      (let ((raw (my/noema-jupyter-notebook--read-raw file)))
        (unless (string-match-p "12" (prin1-to-string (gethash "outputs" (aref (gethash "cells" raw) 0))))
          (error "Debugger execution output was not persisted")))
      (goto-char start)
      (message "Debug live: stop preserves kernel")
      (my/noema-jupyter-run-by-line)
      (let ((conn (car dape--connections)))
        (my/jupyter-debug-live--wait
         (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) line)) "entry before stop")
        (with-current-buffer origin (my/noema-jupyter-debug-stop)))
      (when buffer-read-only (error "Stop left notebook read-only"))
      (erase-buffer)
      (insert "# %% id=audit-setup\n"
              "assert audit_value == 12, 'kernel state was lost'\n"
              "from pathlib import Path\n"
              "Path('audit_debug_module.py').write_text('def twice(value):\\n    answer = value * 2\\n    return answer\\n')\n"
              "import audit_debug_module\n"
              "def audit_twice(value):\n"
              "    return audit_debug_module.twice(value)\n\n"
              "# %% id=audit-call\n"
              "audit_result = audit_twice(7)\n"
              "print(audit_result)\n")
      (save-buffer)
      (my/noema-jupyter-cell--api-sync "aaronnote:api:jupyter:document-execute"
                                      `((scriptFile . ,file) (cellId . "audit-setup")) 60)
      (goto-char (point-min))
      (search-forward "    return audit_debug_module")
      (beginning-of-line)
      (let ((function-line (line-number-at-pos)))
        (dape-breakpoint-toggle)
        (search-forward "audit_result =")
        (message "Debug live: cross-cell breakpoint and module step")
        (my/noema-jupyter-debug-start)
        (let ((conn (car dape--connections)))
          (my/jupyter-debug-live--wait
           (lambda () (equal (plist-get (dape--current-stack-frame conn) :line) function-line))
           "breakpoint in previously executed cell")
          (dape-step-in conn)
          (my/jupyter-debug-live--wait
           (lambda () (let ((source (plist-get (dape--current-stack-frame conn) :source)))
                        (string-suffix-p "/audit_debug_module.py" (or (plist-get source :path) ""))))
           "step into remote module")
          (let* ((source (plist-get (dape--current-stack-frame conn) :source))
                 (reference (plist-get source :sourceReference))
                 (reply (my/jupyter-debug-live--request conn :source
                                                       `(:source ,source :sourceReference ,reference))))
            (unless (and (> reference 0) (string-match-p "answer = value \\* 2" (plist-get reply :content)))
              (error "Remote module source did not come from the kernel")))
          (dape-continue conn)
          (my/jupyter-debug-live--wait
           (lambda () (not (buffer-local-value 'my/noema-jupyter-debug--id origin))) "cross-cell completion")))
      (dape-breakpoint-remove-all)
      (let ((raw (my/noema-jupyter-notebook--read-raw file)))
        (unless (string-match-p "14" (prin1-to-string (gethash "outputs" (aref (gethash "cells" raw) 1))))
          (error "Cross-cell/module execution failed"))))
    (princ "PASS real Dape breakpoint, notebook source map, scopes/variables, next/evaluate, Run by Line, stop preserves kernel, cross-cell/module stepping, persisted output\n")))

(let ((my/jupyter-contents-live-extra #'my/jupyter-debug-live--scenario))
  (load (expand-file-name "test/jupyter-contents-live-smoke.el" user-emacs-directory) nil t))
