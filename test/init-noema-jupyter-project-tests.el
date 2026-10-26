;;; init-noema-jupyter-project-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'noema/jupyter/lsp)
(require 'tools/project/config)

(ert-deftest +jupyter-ssh-project-entry-keeps-normal-remote-filesystem ()
  (let (opened switched registered)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
              ((symbol-function '+project-register-root)
               (lambda (root) (setq registered root)))
              ((symbol-function 'remote-workspace-open)
               (lambda (root &rest args)
                 (setq opened (cons root args))))
              ((symbol-function '+project/switch)
               (lambda (root &optional _arg) (setq switched root))))
      (+jupyter/ssh-open-project
       '(:name "COMP9444" :target "aaron-pc"
         :root "/home/aaron/Desktop/UNSW/COMP9444/"))
      (should (equal (car opened)
                     "/fs:aaron-pc:/home/aaron/Desktop/UNSW/COMP9444/"))
      (should (equal (plist-get (cdr opened) :adapter) "emacs-file"))
      (should (plist-get (cdr opened) :load-environment))
      (should (equal switched (car opened)))
      (should (equal registered (car opened))))))

(defconst +jupyter-project-test-entry
  '((name . "remote-project-test")
    (spec . ((argv . ["python" "-m" "remote_ikernel" "--interface" "ssh"
                     "--host" "project-test" "--workdir" "/work/course"
                     "--kernel_cmd" "/work/course/.conda/bin/python -m ipykernel -f {host_connection_file}"])
             (metadata . ((aaron . ((project . ((lsp . ((server . ["pyright-langserver" "--stdio"])))))))))))))

(defmacro +jupyter-project-test-with-target (&rest body)
  (declare (indent 0))
  `(let ((remote-targets (copy-hash-table remote-targets))
         (+noema-jupyter-project--bindings (make-hash-table :test #'equal)))
     (remote-register-target "project-test" :label "Project Test")
     ,@body))

(ert-deftest +jupyter-project-resolves-legacy-and-metadata-without-copying-ssh ()
  (+jupyter-project-test-with-target
    (let ((project (+noema-jupyter-project-from-entry +jupyter-project-test-entry "/tmp/local.ipynb")))
      (should (equal (+noema-jupyter-project-root project) "/fs:project-test:/work/course/"))
      (should (equal (+noema-jupyter-project-interpreter project) "/work/course/.conda/bin/python"))
      (should (equal (+noema-jupyter-project-target project) "project-test"))
      (puthash (+noema-jupyter-project-root project) +jupyter-project-test-entry +noema-jupyter-project--bindings)
      (should (+noema-jupyter-project-entry-for-source "/fs:project-test:/work/course/train.py"))
      (should-not (+noema-jupyter-project-entry-for-source "/work/course/train.py"))
      (should-not (+noema-jupyter-project-entry-for-source "/fs:project-test:/work/course-other/train.py")))))

(ert-deftest +jupyter-project-shell-reuses-loaded-direnv ()
  "A project shell starts from the workspace capsule without a second export."
  (+jupyter-project-test-with-target
    (let* ((root "/fs:project-test:/work/course/")
           (project (+noema-jupyter-project-create
                     :target "project-test" :root root
                     :interpreter "/work/course/.conda/bin/python"
                     :entry +jupyter-project-test-entry))
           (+enable-direnv t)
           shell-buffer shell-command)
      (unwind-protect
          (cl-letf (((symbol-function '+noema-jupyter-project-current)
                     (lambda () project))
                    ((symbol-function '+noema-jupyter-project--directory)
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
            (setq shell-buffer (+noema-jupyter/open-project-shell))
            (should shell-command)
            (should-not (buffer-local-value
                         '+noema-jupyter-project--shell-pending shell-buffer)))
        (when (buffer-live-p shell-buffer)
          (kill-buffer shell-buffer))))))

(ert-deftest +jupyter-project-navigation-uses-normal-dired-and-completion ()
  (+jupyter-project-test-with-target
    (with-temp-buffer
      (setq buffer-file-name "/tmp/local.ipynb"
            +noema-jupyter-cell-kernel "remote-project-test"
            +noema-jupyter-cell-kernel-spec +jupyter-project-test-entry)
      (let (opened)
        (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'dired) (lambda (path &rest _) (setq opened path)))
                  ((symbol-function 'read-file-name)
                   (lambda (_prompt directory &rest _)
                     (should (equal directory "/fs:project-test:/work/course/"))
                     (concat directory "train.py")))
                  ((symbol-function 'find-file) (lambda (path &rest _) (setq opened path))))
          (+noema-jupyter/open-project-directory)
          (should (equal opened "/fs:project-test:/work/course/"))
          (+noema-jupyter/open-project-file)
          (should (equal opened "/fs:project-test:/work/course/train.py"))
          (should (equal buffer-file-name "/tmp/local.ipynb")))))))

(ert-deftest +jupyter-project-missing-root-is-explicit ()
  (+jupyter-project-test-with-target
    (let ((project (+noema-jupyter-project-from-entry +jupyter-project-test-entry "/tmp/a.ipynb")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_) nil)))
        (should-error (+noema-jupyter-project--directory project) :type 'user-error)))))


(ert-deftest +jupyter-project-local-context-keeps-local-python ()
  (let* ((entry '((name . "python3")
                  (spec . ((argv . ["/usr/bin/python3" "-m" "ipykernel"])))))
         (project (+noema-jupyter-project-from-entry entry "/tmp/a.ipynb")))
    (should (equal (+noema-jupyter-project-target project) "local"))
    (should (equal (+noema-jupyter-project-interpreter project)
                   "/usr/bin/python3"))))

(ert-deftest +jupyter-lsp-uses-file-environment-even-with-remote-kernel ()
  "Kernel host and profile must not register an LSP runtime provider."
  (should-not (seq-find (lambda (entry)
                          (eq (plist-get entry :name) 'noema-jupyter))
                        +language-server-runtime-providers))
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/local.ipynb"
                +noema-jupyter-cell-mode t
                +noema-jupyter-cell-kernel "remote-project-test"
                +noema-jupyter-cell-kernel-spec +jupyter-project-test-entry)
    (should-not +language-server-runtime-required)
    (should-not +language-server-runtime-current)))

(ert-deftest +jupyter-unrelated-ssh-folder-cannot-inherit-project-lsp-root ()
  "A Desktop notebook must not inherit COMP9444's active environment."
  (+jupyter-project-test-with-target
    (let ((+enable-direnv t)
          (course "/fs:project-test:/home/aaron/Desktop/COMP9444/")
          (desktop "/fs:project-test:/home/aaron/Desktop/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'direnv--envrc-root)
                 (lambda (directory)
                   (and (string-prefix-p course directory) course))))
        (with-temp-buffer
          (setq-local buffer-file-name (concat course "test.ipynb")
                      default-directory course)
          (should (equal (+language-server--project-root-for-buffer)
                         course)))
        (with-temp-buffer
          (setq-local buffer-file-name (concat desktop "a.ipynb")
                      default-directory desktop)
          (should (equal (+language-server--project-root-for-buffer)
                         desktop))
          (should (equal (+language-server-toolchain--canonical-root)
                         desktop)))))))

(ert-deftest +jupyter-project-lsp-restarts-the-visiting-file ()
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/local.ipynb")
    (let (events)
      (cl-letf (((symbol-function 'lsp-workspaces) (lambda () '(old)))
                ((symbol-function 'lsp-disconnect)
                 (lambda () (push 'disconnect events)))
                ((symbol-function '+language-server/runtime-invalidate)
                 (lambda () (push 'invalidate events)))
                ((symbol-function '+language-server/ensure)
                 (lambda () (push 'ensure events)))
                ((symbol-function '+noema-jupyter-project-current)
                 (lambda () (ert-fail "LSP consulted the kernel project"))))
        (+noema-jupyter/project-lsp))
      (should (equal (nreverse events)
                     '(disconnect invalidate ensure))))))

(provide 'init-noema-jupyter-project-tests)
;;; init-noema-jupyter-project-tests.el ends here
