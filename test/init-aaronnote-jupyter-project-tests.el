;;; init-aaronnote-jupyter-project-tests.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'init-aaronnote-jupyter-lsp)

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

(ert-deftest my/jupyter-project-local-notebook-never-starts-local-fallback ()
  (my/jupyter-project-test-with-target
    (with-temp-buffer
      (let (received started)
        (cl-letf (((symbol-function 'my/noema-jupyter-cell--lsp-callback-later)
                   (lambda (callback runtime error) (funcall callback runtime error)))
                  ((symbol-function 'remote-exec-async) (lambda (&rest _) (ert-fail "Probe ran on wrong host")))
                  ((symbol-function 'my/lsp-mode-ensure) (lambda () (setq started t))))
          (my/noema-jupyter-cell--lsp-start-runtime-probe
           (current-buffer) (remote-context "/tmp/local.ipynb") "/tmp/"
           "remote-project-test" "default" my/jupyter-project-test-entry nil
           (lambda (_runtime error) (setq received error)))
          (should (my/language-server-runtime-fallback-p received))
          (should my/language-server-runtime-required)
          (setq my/language-server-runtime-state 'unsupported
                my/language-server-runtime-error received)
          (my/language-server--ensure-after-runtime)
          (should-not started))))))

(ert-deftest my/jupyter-project-direnv-failure-blocks-probe ()
  (my/jupyter-project-test-with-target
    (with-temp-buffer
      (let* ((entry (copy-tree my/jupyter-project-test-entry t))
             (meta (my/noema-jupyter-project-metadata (alist-get 'spec entry)))
             received)
        (setf (alist-get 'direnv meta) t)
        (cl-letf (((symbol-function 'direnv-environment-ensure-async)
                   (lambda (root callback)
                     (should (equal root "/fs:project-test:/work/course/"))
                     (funcall callback nil '(error "envrc is blocked")) 'pending))
                  ((symbol-function 'remote-exec-async) (lambda (&rest _) (ert-fail "Probe ignored direnv failure"))))
          (my/noema-jupyter-cell--lsp-start-runtime-probe
           (current-buffer) (remote-context "/fs:project-test:/work/course/test.py")
           "/fs:project-test:/work/course/" "remote-project-test" nil entry nil
           (lambda (_runtime error) (setq received error)))
          (should (string-match-p "envrc is blocked" received)))))))

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
  (let* ((entry '((name . "python3") (spec . ((argv . ["/usr/bin/python3" "-m" "ipykernel"])))))
         (project (my/noema-jupyter-project-from-entry entry "/tmp/a.ipynb")))
    (should (equal (my/noema-jupyter-project-target project) "local"))
    (should (equal (my/noema-jupyter-project-interpreter project) "/usr/bin/python3"))
    (should-not (my/noema-jupyter-cell--lsp-remote-placement (remote-context "/tmp/a.ipynb") entry))))

(ert-deftest my/jupyter-project-late-direnv-callback-does-not-start-obsolete-probe ()
  (my/jupyter-project-test-with-target
    (with-temp-buffer
      (let ((my/enable-direnv t) pending)
        (cl-letf (((symbol-function 'direnv-environment-ensure-async)
                   (lambda (_root callback) (setq pending callback) 'pending))
                  ((symbol-function 'remote-exec-async) (lambda (&rest _) (ert-fail "Obsolete probe started"))))
          (my/noema-jupyter-cell--lsp-start-runtime-probe
           (current-buffer) (remote-context "/fs:project-test:/work/course/train.py")
           "/fs:project-test:/work/course/" "remote-project-test" nil my/jupyter-project-test-entry nil #'ignore)
          (cl-incf my/language-server-runtime--generation)
          (funcall pending nil nil))))))

(ert-deftest my/jupyter-project-deferred-start-cannot-bypass-required-runtime ()
  (with-temp-buffer
    (setq my/language-server-runtime-required t
          my/language-server-runtime-state 'pending)
    (cl-letf (((symbol-function 'my/language-server--project-root-for-buffer)
               (lambda () (ert-fail "An obsolete LSP request selected a workspace")))
              ((symbol-function 'my/lsp-mode-supported-p)
               (lambda () (ert-fail "An obsolete LSP request selected a client"))))
      (my/lsp-mode-start-now)
      (my/lsp-mode--connect-via-remote-a
       (lambda () (ert-fail "An obsolete LSP request started a server"))))))

(ert-deftest my/jupyter-project-runtime-ready-detaches-a-stale-workspace ()
  (let ((runtime (my/language-server-runtime-create :id "new-runtime")) detached ensured)
    (cl-letf (((symbol-function 'lsp-workspaces) (lambda () '(old-workspace)))
              ((symbol-function 'my/language-server-runtime-workspace-id) (lambda (_) "old-runtime"))
              ((symbol-function 'lsp-disconnect) (lambda () (setq detached t)))
              ((symbol-function 'my/language-server--ensure-after-runtime)
               (lambda () (should detached) (setq ensured t))))
      (my/language-server--runtime-ready runtime nil)
      (should ensured))))
