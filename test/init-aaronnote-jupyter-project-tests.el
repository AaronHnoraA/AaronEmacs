;;; init-aaronnote-jupyter-project-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'init-aaronnote-jupyter-lsp)
(require 'init-project)

(ert-deftest my/jupyter-ssh-project-entry-keeps-normal-remote-filesystem ()
  (let (opened switched registered)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
              ((symbol-function 'my/project-register-root)
               (lambda (root) (setq registered root)))
              ((symbol-function 'remote-workspace-open)
               (lambda (root &rest args)
                 (setq opened (cons root args))))
              ((symbol-function 'my/project-switch)
               (lambda (root &optional _arg) (setq switched root))))
      (my/jupyter-ssh-open-project
       '(:name "COMP9444" :target "aaron-pc"
         :root "/home/aaron/Desktop/UNSW/COMP9444/"))
      (should (equal (car opened)
                     "/fs:aaron-pc:/home/aaron/Desktop/UNSW/COMP9444/"))
      (should (equal (plist-get (cdr opened) :adapter) "emacs-file"))
      (should (plist-get (cdr opened) :load-environment))
      (should (equal switched (car opened)))
      (should (equal registered (car opened))))))

(defconst my/jupyter-project-test-entry
  '((name . "remote-project-test")
    (spec . ((argv . ["python" "-m" "remote_ikernel" "--interface" "ssh"
                     "--host" "project-test" "--workdir" "/work/course"
                     "--kernel_cmd" "/work/course/.conda/bin/python -m ipykernel -f {host_connection_file}"])
             (metadata . ((aaron . ((project . ((lsp . ((server . ["pyright-langserver" "--stdio"])))))))))))))

(defmacro my/jupyter-project-test-with-target (&rest body)
  (declare (indent 0))
  `(let ((remote-targets (copy-hash-table remote-targets))
         (my/noema-jupyter-project--bindings (make-hash-table :test #'equal)))
     (remote-register-target "project-test" :label "Project Test")
     ,@body))

(ert-deftest my/jupyter-project-resolves-legacy-and-metadata-without-copying-ssh ()
  (my/jupyter-project-test-with-target
    (let ((project (my/noema-jupyter-project-from-entry my/jupyter-project-test-entry "/tmp/local.ipynb")))
      (should (equal (my/noema-jupyter-project-root project) "/fs:project-test:/work/course/"))
      (should (equal (my/noema-jupyter-project-interpreter project) "/work/course/.conda/bin/python"))
      (should (equal (my/noema-jupyter-project-target project) "project-test"))
      (puthash (my/noema-jupyter-project-root project) my/jupyter-project-test-entry my/noema-jupyter-project--bindings)
      (should (my/noema-jupyter-project-entry-for-source "/fs:project-test:/work/course/train.py"))
      (should-not (my/noema-jupyter-project-entry-for-source "/work/course/train.py"))
      (should-not (my/noema-jupyter-project-entry-for-source "/fs:project-test:/work/course-other/train.py")))))

(ert-deftest my/jupyter-project-shell-reuses-loaded-direnv ()
  "A project shell starts from the workspace capsule without a second export."
  (my/jupyter-project-test-with-target
    (let* ((root "/fs:project-test:/work/course/")
           (project (my/noema-jupyter-project-create
                     :target "project-test" :root root
                     :interpreter "/work/course/.conda/bin/python"
                     :entry my/jupyter-project-test-entry))
           (my/enable-direnv t)
           shell-buffer shell-command)
      (unwind-protect
          (cl-letf (((symbol-function 'my/noema-jupyter-project-current)
                     (lambda () project))
                    ((symbol-function 'my/noema-jupyter-project--directory)
                     (lambda (_) root))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer) (set-buffer buffer)))
                    ((symbol-function 'comint-check-proc)
                     (lambda (_) nil))
                    ((symbol-function 'remote-environment-resolve)
                     (lambda (&rest _) 'loaded-environment))
                    ((symbol-function 'direnv--environment-source)
                     (lambda (_) (list 'direnv root)))
                    ((symbol-function 'direnv-environment-ensure-async)
                     (lambda (&rest _) (ert-fail "Project shell exported direnv twice")))
                    ((symbol-function 'remote-environment-derive)
                     (lambda (environment &rest _)
                       (should (eq environment 'loaded-environment))
                       'shell-environment))
                    ((symbol-function 'remote-environment-apply) #'ignore)
                    ((symbol-function 'shell)
                     (lambda (buffer command)
                       (setq shell-command command shell-buffer buffer))))
            (setq shell-buffer (my/noema-jupyter-open-project-shell))
            (should shell-command)
            (should-not (buffer-local-value
                         'my/noema-jupyter-project--shell-pending shell-buffer)))
        (when (buffer-live-p shell-buffer)
          (kill-buffer shell-buffer))))))

(ert-deftest my/jupyter-project-navigation-uses-normal-dired-and-completion ()
  (my/jupyter-project-test-with-target
    (with-temp-buffer
      (setq buffer-file-name "/tmp/local.ipynb"
            my/noema-jupyter-cell-kernel "remote-project-test"
            my/noema-jupyter-cell-kernel-spec my/jupyter-project-test-entry)
      (let (opened)
        (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'dired) (lambda (path &rest _) (setq opened path)))
                  ((symbol-function 'read-file-name)
                   (lambda (_prompt directory &rest _)
                     (should (equal directory "/fs:project-test:/work/course/"))
                     (concat directory "train.py")))
                  ((symbol-function 'find-file) (lambda (path &rest _) (setq opened path))))
          (my/noema-jupyter-open-project-directory)
          (should (equal opened "/fs:project-test:/work/course/"))
          (my/noema-jupyter-open-project-file)
          (should (equal opened "/fs:project-test:/work/course/train.py"))
          (should (equal buffer-file-name "/tmp/local.ipynb")))))))

(ert-deftest my/jupyter-project-missing-root-is-explicit ()
  (my/jupyter-project-test-with-target
    (let ((project (my/noema-jupyter-project-from-entry my/jupyter-project-test-entry "/tmp/a.ipynb")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_) nil)))
        (should-error (my/noema-jupyter-project--directory project) :type 'user-error)))))


(ert-deftest my/jupyter-project-local-context-keeps-local-python ()
  (let* ((entry '((name . "python3")
                  (spec . ((argv . ["/usr/bin/python3" "-m" "ipykernel"])))))
         (project (my/noema-jupyter-project-from-entry entry "/tmp/a.ipynb")))
    (should (equal (my/noema-jupyter-project-target project) "local"))
    (should (equal (my/noema-jupyter-project-interpreter project)
                   "/usr/bin/python3"))))

(ert-deftest my/jupyter-lsp-uses-file-environment-even-with-remote-kernel ()
  "Kernel host and profile must not register an LSP runtime provider."
  (should-not (seq-find (lambda (entry)
                          (eq (plist-get entry :name) 'noema-jupyter))
                        my/language-server-runtime-providers))
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/local.ipynb"
                my/noema-jupyter-cell-mode t
                my/noema-jupyter-cell-kernel "remote-project-test"
                my/noema-jupyter-cell-kernel-spec my/jupyter-project-test-entry)
    (should-not my/language-server-runtime-required)
    (should-not my/language-server-runtime-current)))

(ert-deftest my/jupyter-unrelated-ssh-folder-cannot-inherit-project-lsp-root ()
  "A Desktop notebook must not inherit COMP9444's active environment."
  (my/jupyter-project-test-with-target
    (let ((my/enable-direnv t)
          (course "/fs:project-test:/home/aaron/Desktop/COMP9444/")
          (desktop "/fs:project-test:/home/aaron/Desktop/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'direnv--envrc-root)
                 (lambda (directory)
                   (and (string-prefix-p course directory) course))))
        (with-temp-buffer
          (setq-local buffer-file-name (concat course "test.ipynb")
                      default-directory course)
          (should (equal (my/language-server--project-root-for-buffer)
                         course)))
        (with-temp-buffer
          (setq-local buffer-file-name (concat desktop "a.ipynb")
                      default-directory desktop)
          (should (equal (my/language-server--project-root-for-buffer)
                         desktop))
          (should (equal (my/language-server-toolchain--canonical-root)
                         desktop)))))))

(ert-deftest my/jupyter-project-lsp-restarts-the-visiting-file ()
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/local.ipynb")
    (let (events)
      (cl-letf (((symbol-function 'lsp-workspaces) (lambda () '(old)))
                ((symbol-function 'lsp-disconnect)
                 (lambda () (push 'disconnect events)))
                ((symbol-function 'my/language-server-runtime-invalidate)
                 (lambda () (push 'invalidate events)))
                ((symbol-function 'my/language-server-ensure)
                 (lambda () (push 'ensure events)))
                ((symbol-function 'my/noema-jupyter-project-current)
                 (lambda () (ert-fail "LSP consulted the kernel project"))))
        (my/noema-jupyter-project-lsp))
      (should (equal (nreverse events)
                     '(disconnect invalidate ensure))))))

(provide 'init-aaronnote-jupyter-project-tests)
;;; init-aaronnote-jupyter-project-tests.el ends here
