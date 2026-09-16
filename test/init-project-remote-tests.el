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
  (let* ((source (generate-new-buffer " *treemacs-logical-source*"))
        (native-file (make-temp-file "treemacs-logical-" nil ".java"))
        (logical-file (remote-make-file-name "local" native-file))
        (physical-file (concat "/ssh:box:" native-file))
        captured)
    (unwind-protect
        (progn
          (with-current-buffer source
            (set-visited-file-name logical-file t))
          (cl-letf
              (((symbol-function 'my/treemacs-visit-path)
                (lambda (_path) logical-file)))
            (my/treemacs-get-imenu-index-a
             (lambda (file)
               (setq captured file)
               nil)
             physical-file)
            (should
             (eq
              (my/treemacs-visit-logical-path-a
               (lambda ()
                 (get-file-buffer "/ssh:box:/work/Main.java")))
              source)))
          (should
           (equal captured logical-file)))
      (when (buffer-live-p source)
        (kill-buffer source))
      (delete-file native-file))))

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

(ert-deftest my/project-activation-is-canonical-idempotent-and-does-no-source-io ()
  (let ((my/project-active-root nil)
        (my/project--perspective-roots (make-hash-table :test #'eq))
        events)
    (let ((my/project-activated-hook (list (lambda (root) (push root events)))))
      (cl-letf (((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "source IO")))
                ((symbol-function 'remote-session-acquire) (lambda (&rest _) (ert-fail "connection"))))
        (my/project-activate "/tmp/project")
        (my/project-activate "/fs:local:/tmp/project/")
        (my/project-leave)
        (my/project-leave)
        (my/project-activate "/fs:box:/work/project/")
        (my/project-activate "/fs:box:/work/project")
        (my/project-leave)
        (should (equal (reverse events)
                       '("/fs:local:/tmp/project/" nil "/fs:box:/work/project/" nil)))))))

(ert-deftest my/project-navigation-activates-after-success-and-not-after-cancellation ()
  (let ((my/project-active-root nil)
        (my/project--perspective-roots (make-hash-table :test #'eq))
        events)
    (let ((my/project-activated-hook (list (lambda (root) (push root events)))))
      (cl-letf (((symbol-function 'my/direnv-update-environment-maybe) #'ignore)
                ((symbol-function 'projectile-find-file-in-directory) #'ignore)
                ((symbol-function 'projectile-recentf) (lambda () (user-error "cancel"))))
        (my/project-find-file "/tmp/first/")
        (should-error (my/project-recent-file "/tmp/second/") :type 'user-error)
        (my/with-project-root-context "/tmp/background/" nil)
        (should (equal events '("/fs:local:/tmp/first/")))))))

(ert-deftest my/project-perspective-switches-follow-recorded-user-navigation ()
  ;; Exercise the installed package, including its temporary NORECORD switches.
  (require 'perspective)
  (let ((my/project-active-root nil)
        (my/project--perspective-roots (make-hash-table :test #'eq))
        (original (persp-current-name))
        (first (make-temp-name "agenda-persp-a-"))
        (second (make-temp-name "agenda-persp-b-"))
        events)
    (let ((my/project-activated-hook (list (lambda (root) (push root events)))))
      (unwind-protect
          (progn
            (persp-switch first)
            (my/project-activate "/tmp/a/" t)
            (persp-switch second)
            (should-not my/project-active-root)
            (my/project-activate "/tmp/b/" t)
            (let ((before (copy-sequence events)))
              (with-perspective first (should (equal my/project-active-root "/fs:local:/tmp/b/")))
              (should (equal events before)))
            (persp-switch first)
            (should (equal my/project-active-root "/fs:local:/tmp/a/"))
            (my/project-leave)
            (persp-switch second)
            (persp-switch first)
            (should-not my/project-active-root))
        (persp-switch original)
        (dolist (name (list first second))
          (when (member name (persp-names)) (persp-kill name)))))))

(ert-deftest my/project-workspace-close-releases-only-associated-project ()
  (require 'remote-workspace)
  (let ((my/project-active-root nil)
        (my/project--perspective-roots (make-hash-table :test #'eq))
        (remote-workspaces (make-hash-table :test #'equal))
        events)
    (let ((my/project-activated-hook (list (lambda (root) (push root events)))))
      (let ((one (remote-workspace-open "/fs:local:/tmp/agenda-one/" :connect nil))
            (two (remote-workspace-open "/fs:local:/tmp/agenda-two/" :connect nil)))
        (should-not events) ; Workspace opening alone is not user navigation.
        (my/project-activate "/tmp/agenda-one/" t)
        (remote-workspace-close two)
        (should my/project-active-root)
        (remote-workspace-close one)
        (should-not my/project-active-root)
        (should (equal (reverse events) '("/fs:local:/tmp/agenda-one/" nil)))))))

(ert-deftest my/project-exit-releases-the-native-agenda-lease ()
  (require 'noema-agenda)
  (let ((my/project-active-root nil)
        (my/project-activated-hook '(noema-agenda-activate-project))
        (noema-agenda--project-root nil)
        (noema-agenda--project-scope nil)
        (noema-agenda--project-lease nil)
        (noema-agenda--project-request 0)
        calls)
    (cl-letf (((symbol-function 'noema-agenda--update-scopes) #'ignore)
              ((symbol-function 'noema-agenda--call)
               (lambda (operation body callback)
                 (push (cons operation body) calls)
                 (funcall callback '((id . "scope-one")) nil))))
      (my/project-activate "/fs:local:/tmp/agenda-enter/")
      (should (equal (car calls) '("enter" (root . "/tmp/agenda-enter/") (lease . "emacs-agenda:1"))))
      (my/project-leave)
      (should (equal (car calls) '("leave" (id . "scope-one") (lease . "emacs-agenda:1"))))
      (should-not noema-agenda--project-root)
      (should-not noema-agenda--project-scope))))

(provide 'init-project-remote-tests)
;;; init-project-remote-tests.el ends here
