;;; init-project-remote-tests.el --- Treemacs remote path tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-project)
(require 'init-project-local)
(require 'remote-board)
(require 'remote-search)

(ert-deftest project-managed-ripgrep-preserves-consult-options ()
  (should
   (equal
    (split-string-and-unquote
     (remote-search-consult-ripgrep-args
      "rg --null --smart-case" "/tmp/with space/rg"))
    '("/tmp/with space/rg" "--null" "--smart-case")))
  (should
   (equal
    (remote-search-consult-ripgrep-args
     '("rg" "--null" (custom-options)) "/tmp/rg")
    '("/tmp/rg" "--null" (custom-options))))
  (should-not
   (remote-search-consult-ripgrep-args
    "env RG_CONFIG_PATH=/tmp/config rg --null" "/tmp/rg")))

(ert-deftest project-managed-ripgrep-verifies-archive-before-use ()
  (let* ((directory (make-temp-file "remote-rg-cache-test-" t))
         (payload "verified release archive")
         (release (list :name "asset.bin"
                        :sha256 (secure-hash 'sha256 payload)))
         (archive (expand-file-name "asset.bin" directory))
         (downloads 0))
    (unwind-protect
        (cl-letf (((symbol-function
                    'remote-search--archive-cache-directory)
                   (lambda () directory))
                  ((symbol-function 'remote-search--client-command)
                   (lambda (_directory _program &rest args)
                     (cl-incf downloads)
                     (with-temp-file (cadr (member "--output" args))
                       (insert payload)))))
          (should (equal (remote-search--verified-archive release) archive))
          (should (= downloads 1))
          (with-temp-file archive (insert "tampered"))
          (should (equal (remote-search--verified-archive release) archive))
          (should (= downloads 2))
          (should (equal (remote-search--sha256-file archive)
                         (plist-get release :sha256))))
      (delete-directory directory t))))

(ert-deftest project-managed-ripgrep-keeps-client-cache-local ()
  (with-temp-buffer
    (let ((default-directory "/fs:somewhere:/work/"))
      (should-not
       (file-remote-p (remote-search--archive-cache-directory))))))

(ert-deftest project-managed-ripgrep-does-not-provision-untrusted-target ()
  (let* ((target (remote-get-target "local"))
         (trusted (remote-target-trusted target))
         (remote-search-auto-provision-ripgrep t)
         (remote-search--cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (setf (remote-target-trusted target) nil)
          (cl-letf (((symbol-function 'remote-executable-find)
                     (lambda (&rest _arguments) nil))
                    ((symbol-function 'remote-search--provision)
                     (lambda (&rest _arguments)
                       (ert-fail "Untrusted target was provisioned"))))
            (should-not
             (remote-search-ripgrep "/fs:local:/tmp/"))))
      (setf (remote-target-trusted target) trusted))))

(ert-deftest project-managed-ripgrep-prepares-without-blocking-first-search ()
  "A missing tool starts one worker while the first search uses grep."
  (let* ((target (remote-get-target "local"))
         (trusted (remote-target-trusted target))
         (remote-search-auto-provision-ripgrep t)
         (remote-search--cache (make-hash-table :test #'equal))
         (remote-search--workers (make-hash-table :test #'equal))
         (starts 0)
         (probes 0))
    (unwind-protect
        (progn
          (setf (remote-target-trusted target) t)
          (cl-letf (((symbol-function 'remote-executable-find)
                     (lambda (&rest _arguments)
                       (cl-incf probes)
                       nil))
                    ((symbol-function 'remote-search--candidate)
                     (lambda (_context)
                       (ert-fail "Platform probe blocked the first search")))
                    ((symbol-function 'remote-search--installed)
                     (lambda (&rest _arguments)
                       (ert-fail "Cache validation blocked the first search")))
                    ((symbol-function 'remote-search--start-worker)
                     (lambda (_target) (cl-incf starts))))
            (should-not (remote-search-ripgrep "/fs:local:/tmp/"))
            (should-not (remote-search-ripgrep "/fs:local:/tmp/"))
            (should (= starts 1))
            (should (= probes 1))
            (should (eq (nth 3 (gethash "local" remote-search--cache))
                        :pending))))
      (setf (remote-target-trusted target) trusted))))

(ert-deftest project-managed-ripgrep-refreshes-without-interrupting-search ()
  "An expired verified path stays available during background validation."
  (let* ((target (remote-get-target "local"))
         (trusted (remote-target-trusted target))
         (remote-search-auto-provision-ripgrep t)
         (remote-search--cache (make-hash-table :test #'equal))
         (remote-search--workers (make-hash-table :test #'equal))
         (program "/tmp/managed/bin/rg")
         (refreshes 0)
         (probes 0))
    (unwind-protect
        (progn
          (setf (remote-target-trusted target) t)
          (remote-search--cache-put target program -1)
          (cl-letf (((symbol-function 'remote-executable-find)
                     (lambda (&rest _arguments)
                       (cl-incf probes)
                       nil))
                    ((symbol-function 'remote-search--start-worker)
                     (lambda (_target) (cl-incf refreshes))))
            (should (equal (remote-search-ripgrep "/fs:local:/tmp/")
                           program))
            (should (= refreshes 1))
            (should (= probes 0))
            (setq remote-search-auto-provision-ripgrep nil)
            (should-not (remote-search-ripgrep "/fs:local:/tmp/"))
            (should (= probes 1))))
      (setf (remote-target-trusted target) trusted))))

(ert-deftest project-search-selects-available-target-tool-and-native-root ()
  "Search must work on an SSH host without rg and retain native local speed."
  (require 'consult)
  (let (seen probes available-rg)
    (cl-letf (((symbol-function 'my/direnv-update-environment-maybe) #'ignore)
              ((symbol-function 'remote-executable-find)
               (lambda (program context)
                 (push (list program (remote-context-target-id context)) probes)
                 (cond
                  ((and available-rg (equal program "rg")) "/usr/bin/rg")
                  ((equal program "grep") "/usr/bin/grep"))))
              ((symbol-function 'remote-client-file-name)
               (lambda (logical &optional _adapter)
                 (and (string-prefix-p "/fs:local:" logical)
                      (remote-file-local-name logical))))
              ((symbol-function 'consult-ripgrep)
               (lambda (root &optional initial)
                 (setq seen (list 'rg root initial))))
              ((symbol-function 'consult-grep)
               (lambda (root &optional initial)
                 (setq seen (list 'grep root initial)))))
      (my/project-ripgrep "/fs:box:/work/project/" "needle")
      (should (equal seen '(grep "/fs:box:/work/project/" "needle")))
      (should (equal (reverse probes)
                     '(("rg" "box") ("grep" "box"))))
      (setq seen nil probes nil available-rg t)
      (my/project-ripgrep "/fs:local:/tmp/project/")
      (should (equal seen '(rg "/tmp/project/" nil)))
      (should (equal (reverse probes)
                     '(("rg" "local")))))))

(ert-deftest project-search-keeps-custom-consult-command ()
  "A changed Consult command shape must keep the user's chosen search tool."
  (require 'consult)
  (let ((consult-ripgrep-args "env SEARCH_MODE=custom rg --null")
        selected)
    (cl-letf (((symbol-function 'my/direnv-update-environment-maybe) #'ignore)
              ((symbol-function 'remote-search-ripgrep)
               (lambda (&rest _arguments)
                 (ert-fail "Managed rg replaced a custom Consult command")))
              ((symbol-function 'consult-ripgrep)
               (lambda (root &optional initial)
                 (setq selected (list root initial consult-ripgrep-args)))))
      (my/project-ripgrep "/tmp/project/" "needle")
      (should
       (equal selected
              '("/tmp/project/" "needle"
                "env SEARCH_MODE=custom rg --null"))))))

(ert-deftest project-search-shortcut-keeps-rg-menu-and-remote-fallback ()
  (require 'rg)
  (should (eq (lookup-key global-map (kbd "C-c s"))
              #'my/rg-menu-or-project-search))
  (let (available selected)
    (cl-letf (((symbol-function 'my/project-current-root)
               (lambda () "/fs:box:/work/"))
              ((symbol-function 'remote-executable-find)
               (lambda (_program _context) available))
              ((symbol-function 'rg-menu)
               (lambda () (setq selected 'rg-menu)))
              ((symbol-function 'my/project-ripgrep)
               (lambda (root &optional _initial rg-unavailable)
                 (setq selected (list 'grep root rg-unavailable)))))
      (my/rg-menu-or-project-search)
      (should (equal selected '(grep "/fs:box:/work/" t)))
      (setq available t selected nil)
      (my/rg-menu-or-project-search)
      (should (eq selected 'rg-menu)))))

(ert-deftest project-magit-explicit-root-owns-status-identity ()
  "An explicit logical repo must win over the caller's different target."
  (let ((status (generate-new-buffer " *remote-magit-origin-test*")))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory "/fs:caller:/tmp/"))
            (cl-letf (((symbol-function 'magit-toplevel)
                       (lambda () "/tmp/repo/")))
              (should (eq (my/magit-status-logical-root-a
                           (lambda (&rest _arguments) status)
                           "/fs:chosen:/tmp/repo/")
                          status))
              (should (equal (buffer-local-value 'my/magit--logical-root status)
                             "/fs:chosen:/tmp/repo/")))))
      (kill-buffer status))))

(ert-deftest project-magit-logical-local-workspace-visits-native-buffer ()
  "A local workspace must keep Magit and worktree buffers on native paths."
  (require 'magit)
  (let* ((directory (make-temp-file "remote-magit-local-" t))
         (logical-root
          (file-name-as-directory
           (remote-make-file-name "local" directory)))
         (native-file (expand-file-name "note" directory))
         folder-buffer status-buffer source-buffer visit-buffer seen-root)
    (unwind-protect
        (progn
          (let ((default-directory directory))
            (should (zerop (call-process "git" nil nil nil "init" "-q"))))
          (with-temp-file native-file (insert "local worktree\n"))
          (setq folder-buffer (remote-open-folder "local" directory)
                source-buffer (find-file-noselect native-file))
          (let ((original (symbol-function 'magit-status-setup-buffer)))
            (cl-letf (((symbol-function 'my/project-activate) #'ignore)
                      ((symbol-function 'magit-status-setup-buffer)
                       (lambda (&rest arguments)
                         (setq seen-root (car arguments)
                               status-buffer (apply original arguments)))))
              (my/project-magit-status logical-root)))
          (should (equal seen-root (file-name-as-directory directory)))
          (setq visit-buffer
                (with-current-buffer status-buffer
                  (magit-find-file-noselect "{worktree}" "note")))
          (should (eq visit-buffer source-buffer))
          (should-not
           (file-remote-p (buffer-local-value 'buffer-file-name visit-buffer)))
          (should (file-equal-p
                   (buffer-local-value 'buffer-file-name visit-buffer)
                   native-file)))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (when (and (buffer-live-p visit-buffer)
                 (not (eq visit-buffer source-buffer)))
        (kill-buffer visit-buffer))
      (when (buffer-live-p status-buffer)
        (kill-buffer status-buffer))
      (when-let* ((workspace (remote-get-workspace logical-root)))
        (remote-workspace-close workspace 'test-cleanup))
      (when (buffer-live-p folder-buffer)
        (kill-buffer folder-buffer))
      ;; Magit's asynchronous Git process can have exited while its sentinel
      ;; is still queued.  Let the sentinel finish before removing its cwd.
      (let* ((token (file-name-nondirectory
                     (directory-file-name directory)))
             (owned
              (lambda ()
                (cl-remove-if-not
                 (lambda (process)
                   (when-let* ((buffer (process-buffer process))
                               ((buffer-live-p buffer))
                               (cwd (buffer-local-value
                                     'default-directory buffer)))
                     (and (string-prefix-p "git" (process-name process))
                          (string-search token cwd))))
                 (process-list))))
             (deadline (+ (float-time) 2)))
        (while (and (< (float-time) deadline) (funcall owned))
          (accept-process-output nil 0.05))
        (dolist (process (funcall owned))
          (ignore-errors (delete-process process)))
        (accept-process-output nil 0.05))
      (delete-directory directory t))))

(ert-deftest dirvish-logical-local-folder-skips-metadata-helper ()
  "Opening a local logical workspace must not spawn Dirvish's child Emacs."
  (require 'dired)
  (require 'dirvish)
  (let* ((directory (make-temp-file "remote-dirvish-local-" t))
         (logical (file-name-as-directory
                   (remote-make-file-name "local" directory)))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'dirvish--make-proc)
                   (lambda (&rest _)
                     (ert-fail "Dirvish spawned a metadata helper"))))
          (setq buffer (remote-open-folder "local" directory))
          (should (buffer-live-p buffer))
          (should (remote-workspace-live-p
                   (remote-get-workspace logical))))
      (when-let* ((workspace (remote-get-workspace logical)))
        (remote-workspace-close workspace 'test-cleanup))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest project-local-root-is-pinned-only-for-current-operation ()
  (let ((default-directory "/tmp/")
        (my/project-local--scoped-root nil)
        (current "/tmp/first/"))
    (cl-letf (((symbol-function 'my/project-current-root)
               (lambda () current)))
      (let ((my/project-local--scoped-root
             (cons (current-buffer) (my/project-local-root))))
        (setq current "/tmp/second/")
        (should (equal (my/project-local-root) "/tmp/first/"))
        (with-temp-buffer
          (setq default-directory "/tmp/")
          (should (equal (my/project-local-root) "/tmp/second/"))))
      (should (equal (my/project-local-root) "/tmp/second/")))))

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

(ert-deftest my/project-roots-have-one-spelling-per-target ()
  "Local roots stay native; remote roots are `/fs:' whatever spelled them."
  (should (equal (my/project-normalize-root "/fs:local:/tmp/project")
                 "/tmp/project/"))
  (should (equal (my/project-normalize-root "/tmp/project") "/tmp/project/"))
  (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil)))
    (should (equal (my/project-normalize-root "/fs:box:/work/project")
                   "/fs:box:/work/project/"))
    (should (equal (my/project-normalize-root "/ssh:box:/work/project/")
                   "/fs:box:/work/project/"))))

(ert-deftest my/project-known-remote-projects-are-listed-without-probing ()
  "Listing projects never dials a target nor drops an unreachable one."
  (let ((local (make-temp-file "project-known-" t))
        probed)
    (unwind-protect
        (cl-letf* ((original-client (symbol-function 'remote-client-file-name))
                   ((symbol-function 'remote-client-file-name)
                    (lambda (name &rest args)
                      (unless (string-prefix-p "/fs:box:" name)
                        (apply original-client name args))))
                   (original-directory-p (symbol-function 'file-directory-p))
                   ((symbol-function 'file-directory-p)
                    (lambda (name)
                      (when (string-prefix-p "/fs:box:" name) (setq probed t))
                      (funcall original-directory-p name)))
                   ((symbol-function 'projectile-relevant-known-projects)
                    (lambda ()
                      (list "/fs:box:/work/project/" "/ssh:box:/work/project/"
                            local "/fs:local:/nowhere-at-all/"))))
          (should (equal (my/project-known-projects)
                         (list "/fs:box:/work/project/"
                               (file-name-as-directory local))))
          (should-not probed))
      (delete-directory local))))

(ert-deftest my/project-remote-root-registers-through-its-workspace ()
  (let (opened added)
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil))
              ((symbol-function 'remote-workspace-open)
               (lambda (root &rest _) (setq opened root)))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'projectile-project-p) (lambda (&rest _) t))
              ((symbol-function 'my/project-unignore-root) #'ignore)
              ((symbol-function 'projectile-add-known-project)
               (lambda (root) (setq added root)))
              ((symbol-function 'project--remember-dir) #'ignore))
      (should (equal (my/project-register-root "/ssh:box:/work/project")
                     "/fs:box:/work/project/"))
      (should (equal opened "/fs:box:/work/project/"))
      (should (equal added "/fs:box:/work/project/")))))

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

(ert-deftest treemacs-imenu-never-kills-the-visiting-source-buffer ()
  "Indexing a file whose buffer visits another spelling keeps that buffer.
Treemacs kills what it indexed unless `get-file-buffer' found it first."
  (let* ((native-file (make-temp-file "treemacs-kill-" nil ".py" "x = 1\n"))
         (logical-file (remote-make-file-name "local" native-file))
         (source (find-file-noselect logical-file)))
    (unwind-protect
        (progn
          (require 'treemacs-tags)
          ;; The indexer is handed the native spelling of a `/fs:' buffer.
          (cl-letf (((symbol-function 'my/treemacs-visit-path)
                     (lambda (_path) native-file)))
            (my/treemacs-get-imenu-index-a #'treemacs--get-imenu-index
                                           native-file))
          (should (buffer-live-p source)))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
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

(ert-deftest my/project-reentry-keeps-visible-editor-instead-of-opening-dired ()
  "Opening the SSH project or its tree must not replace a visible notebook."
  (let* ((root (make-temp-file "project-reentry-" t))
         (source (generate-new-buffer " *project-notebook*"))
         (tree (generate-new-buffer " *project-tree*"))
         opened)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer source
            (setq-local buffer-file-name (expand-file-name "test.ipynb" root)))
          (switch-to-buffer source)
          (select-window
           (display-buffer-in-side-window
            tree '((side . left) (slot . 0) (window-width . 20))))
          (cl-letf (((symbol-function 'my/project-switch-perspective) #'ignore)
                    ((symbol-function 'my/direnv-update-environment-maybe) #'ignore)
                    ((symbol-function 'my/project-activate) #'ignore)
                    ((symbol-function 'dired)
                     (lambda (&rest _) (setq opened t))))
            (my/project-switch root)
            (should-not opened)
            (should (eq (window-buffer (selected-window)) source))))
      (kill-buffer source)
      (kill-buffer tree)
      (delete-directory root t))))

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

(ert-deftest my/project-directory-marker-answers-wildcards-from-one-listing ()
  "A project marker must cost a listing per directory, not one per marker.
Projectile ships eight wildcard markers, and expanding each one separately is
a round trip per marker per level on a target."
  (require 'projectile)
  (let ((root (make-temp-file "marker-" t))
        (listings 0))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "thing.sln" root) nil 'silent)
          (write-region "" nil (expand-file-name "Makefile" root) nil 'silent)
          (make-directory (expand-file-name "src" root))
          (let ((counted
                 (lambda (orig &rest args)
                   (setq listings (1+ listings))
                   (apply orig args))))
            (advice-add 'directory-files :around counted)
            (unwind-protect
                (progn
                  ;; Like upstream, the matching MARKER is returned.
                  (should (equal (my/project--directory-marker
                                  root '("?*.sln") 'files-only)
                                 "?*.sln"))
                  (should (equal (my/project--directory-marker
                                  root '("Makefile"))
                                 "Makefile"))
                  (should-not (my/project--directory-marker
                               root '("?*.xcodeproj" "?*.csproj")))
                  ;; A marker naming a directory is not a file marker.
                  (should-not (my/project--directory-marker
                               root '("src") 'files-only))
                  (should (equal (my/project--directory-marker root '("src"))
                                 "src"))
                  ;; One listing per call, never one per marker.
                  (should (= listings 5)))
              (advice-remove 'directory-files counted))))
      (delete-directory root t))))

(ert-deftest my/project-directory-marker-override-is-installed ()
  "Installation implies the shape check passed.
`func-arity' reports the advice once one is installed, so the check itself
cannot be repeated here."
  (require 'projectile)
  (should (advice-member-p #'my/project--directory-marker
                           'projectile--directory-marker)))

(provide 'init-project-remote-tests)
;;; init-project-remote-tests.el ends here
