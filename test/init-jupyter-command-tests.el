;;; init-jupyter-command-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/jupyter/command)

(defconst +jupyter-command-test-context
  (remote-context-create :target-id "test-remote" :localname "/project/a.ipynb"
                         :workspace-root "/fs:test-remote:/project/"))

(ert-deftest +jupyter-command-uses-target-project-environment-before-path ()
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program context)
               (should (equal (remote-context-target-id context) "test-remote"))
               (pcase program
                 ("/project/.conda/bin/jupyter" program)
                 ("jupyter" "/system/bin/jupyter")))))
    (should (equal (+jupyter-target-command +jupyter-command-test-context)
                   '("/project/.conda/bin/jupyter")))))

(ert-deftest +jupyter-command-reuses-conda-registry-without-client-home ()
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program _context)
               (and (equal program "/remote/conda/bin/jupyter") program)))
            ((symbol-function 'file-readable-p)
             (lambda (path)
               (should (equal path "/fs:test-remote:/home/user/.conda/environments.txt")) t))
            ((symbol-function 'remote-expand-file-name)
             (lambda (path _directory context)
               (should (equal path "~/.conda/environments.txt"))
               (should (equal (remote-context-target-id context) "test-remote"))
               "/fs:test-remote:/home/user/.conda/environments.txt"))
            ((symbol-function 'insert-file-contents)
             (lambda (_path &rest _) (insert "/remote/conda\n"))))
    (should (equal (+jupyter-target-command +jupyter-command-test-context)
                   '("/remote/conda/bin/jupyter")))))

(ert-deftest +jupyter-command-supports-target-python-module-without-cli ()
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program _context) (and (equal program "python3") "/target/bin/python3")))
            ((symbol-function 'file-readable-p) (lambda (_) nil))
            ((symbol-function 'remote-expand-file-name) (lambda (&rest _) "/fs:test-remote:/home/user/.conda/environments.txt"))
            ((symbol-function 'process-file)
             (lambda (program &rest _)
               (should (equal program "/target/bin/python3"))
               (should (equal default-directory "/fs:test-remote:/project/")) 0)))
    (should (equal (+jupyter-target-command +jupyter-command-test-context)
                   (list "/target/bin/python3" "-c"
                         +jupyter--python-catalog-helper)))))

(ert-deftest +jupyter-command-broken-system-dispatcher-uses-python-api ()
  "A bare jupyter executable is not enough to list kernelspecs."
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program _context)
               (pcase program
                 ("jupyter" "/usr/bin/jupyter")
                 ("python3" "/usr/bin/python3"))))
            ((symbol-function 'file-readable-p) (lambda (_) nil))
            ((symbol-function 'remote-expand-file-name)
             (lambda (&rest _) "/fs:test-remote:/home/user/.conda/environments.txt"))
            ((symbol-function 'process-file)
             (lambda (program &rest _)
               (should (equal program "/usr/bin/python3")) 0)))
    (should (equal (+jupyter-target-command +jupyter-command-test-context)
                   (list "/usr/bin/python3" "-c"
                         +jupyter--python-catalog-helper)))))

(ert-deftest +jupyter-command-prefers-complete-system-dispatcher ()
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program _context)
               (pcase program
                 ("jupyter" "/usr/bin/jupyter")
                 ("jupyter-kernelspec" "/usr/bin/jupyter-kernelspec")))))
    (should (equal (+jupyter-target-command +jupyter-command-test-context)
                   '("/usr/bin/jupyter")))))

(ert-deftest +jupyter-command-missing-installation-is-actionable ()
  (cl-letf (((symbol-function 'remote-executable-find) (lambda (&rest _) nil))
            ((symbol-function 'file-readable-p) (lambda (_) nil))
            ((symbol-function 'remote-expand-file-name) (lambda (&rest _) "/fs:test-remote:/home/user/.conda/environments.txt")))
    (let ((err (should-error (+jupyter-target-command +jupyter-command-test-context)
                             :type 'user-error)))
      (should (string-match-p "test-remote.*project.*jupyter_client" (error-message-string err))))))

(ert-deftest +jupyter-command-does-not-replace-explicit-configuration ()
  (cl-letf (((symbol-function 'remote-executable-find)
             (lambda (program _context)
               (should (equal program "/configured/jupyter")) nil)))
    (should-error (+jupyter-target-command +jupyter-command-test-context "/configured/jupyter")
                  :type 'user-error)))
