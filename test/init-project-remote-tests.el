;;; init-project-remote-tests.el --- Treemacs remote path tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-project)

(ert-deftest treemacs-local-logical-project-uses-native-model-path ()
  (should
   (equal
    (my/treemacs-project-path "/fs:local:/tmp/project/")
    "/tmp/project")))

(ert-deftest treemacs-target-only-project-uses-file-handler-model-path ()
  (cl-letf
      (((symbol-function 'remote-client-file-name)
        (lambda (&rest _) nil))
       ((symbol-function 'remote-project-file-name)
        (lambda (path &rest _)
          (concat
           "/ssh:box:"
           (remote-file-local-name path)))))
    (should
     (equal
      (my/treemacs-project-path "/fs:box:/work/project/")
      "/ssh:box:/work/project"))))

(ert-deftest treemacs-persistence-migrates-only-path-records ()
  (cl-letf
      (((symbol-function 'my/treemacs-project-path)
        (lambda (path)
          (pcase path
            ("/fs:local:/tmp/project" "/tmp/project")
            ("/fs:box:/work/project" "/ssh:box:/work/project")
            (_ path)))))
    (should
     (equal
      (my/treemacs-normalize-persist-lines-a
       '("  - path :: /fs:local:/tmp/project"
         "    - name :: Local"
         "  - path :: /fs:box:/work/project"))
      '("  - path :: /tmp/project"
        "    - name :: Local"
        "  - path :: /ssh:box:/work/project")))))

(ert-deftest treemacs-visits-return-to-buffer-facing-namespace ()
  (cl-letf
      (((symbol-function 'remote-canonicalize-file-name)
        (lambda (path &optional _directory)
          (pcase path
            ("/ssh:box:/work/Main.java" "/fs:box:/work/Main.java")
            (_ path))))
       ((symbol-function 'remote-client-file-name)
        (lambda (logical &optional _adapter)
          (and (string-prefix-p "/fs:local:" logical)
               (remote-file-local-name logical)))))
    (should
     (equal
      (my/treemacs-visit-path "/ssh:box:/work/Main.java")
      "/fs:box:/work/Main.java"))
    (should
     (equal
      (my/treemacs-visit-path "/fs:local:/tmp/Main.java")
      "/tmp/Main.java"))))

(ert-deftest treemacs-imenu-reads-the-existing-logical-source-buffer ()
  (let ((source (generate-new-buffer " *treemacs-logical-source*"))
        captured)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer-file-name "/fs:box:/work/Main.java"))
          (cl-letf
              (((symbol-function 'my/treemacs-visit-path)
                (lambda (_path) "/fs:box:/work/Main.java")))
            (my/treemacs-get-imenu-index-a
             (lambda (file)
               (setq captured file)
               nil)
             "/ssh:box:/work/Main.java")
            (should
             (eq
              (my/treemacs-visit-logical-path-a
               (lambda ()
                 (get-file-buffer "/ssh:box:/work/Main.java")))
              source)))
          (should
           (equal captured "/fs:box:/work/Main.java")))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest treemacs-file-events-ignore-a-directory-deletion-race ()
  (should-not
   (my/treemacs-process-file-events-safely-a
    (lambda ()
      (signal
       'file-missing
       '("Opening directory" "No such file or directory"
         "/fs:local:/tmp/project/build/classes"))))))

(ert-deftest treemacs-file-events-preserve-other-missing-file-errors ()
  (should-error
   (my/treemacs-process-file-events-safely-a
    (lambda ()
      (signal
       'file-missing
       '("Opening input file" "No such file or directory"
         "/fs:local:/tmp/project/MISSING"))))
   :type 'file-missing))

(provide 'init-project-remote-tests)
;;; init-project-remote-tests.el ends here
