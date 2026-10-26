;;; init-project-remote-tests.el --- Treemacs remote path tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'tools/project/config)
(require 'tools/project-local/config)
(require 'tools/git/board)
(require 'remote-board)
(require 'remote-search)

(ert-deftest git-board-visits-local-repository-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        (+git-board-repo-root "/Users/test/project/")
        visited context)
    (cl-letf (((symbol-function '+git-board-current-file)
               (lambda () "src/main.el"))
              ((symbol-function 'file-exists-p) (lambda (_file) t))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+git-board/visit))
    (should (equal visited "/Users/test/project/src/main.el"))
    (should-not (file-remote-p context))))

(ert-deftest +project-local-root-stays-local-from-a-remote-buffer ()
  "Switching to a native project must not inherit the source buffer's target."
  (let* ((local (file-name-as-directory
                 (make-temp-file "project-local-switch-" t)))
         (+project-active-root nil)
         (+project-activated-hook nil)
         (+project--perspective-roots (make-hash-table :test #'eq))
         opened-in)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:box:/work/")
          (should (equal (+project-normalize-root local) local))
          (should (equal (+project--canonical-file-name local)
                         (concat "/fs:local:" local)))
          (cl-letf (((symbol-function '+project-switch-perspective) #'ignore)
                    ((symbol-function '+direnv-update-environment-maybe)
                     #'ignore)
                    ((symbol-function 'dired)
                     (lambda (_path) (setq opened-in default-directory))))
            (+project/switch local)
            (should (equal opened-in local))
            (should (equal +project-active-root
                           (concat "/fs:local:" local)))
            (+project/switch "/fs:lab:/work/")
            (should (equal opened-in "/fs:lab:/work/"))
            (should (equal +project-active-root "/fs:lab:/work/"))))
      (delete-directory local t))))

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
    (cl-letf (((symbol-function '+direnv-update-environment-maybe) #'ignore)
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
      (+project/ripgrep "/fs:box:/work/project/" "needle")
      (should (equal seen '(grep "/fs:box:/work/project/" "needle")))
      (should (equal (reverse probes)
                     '(("rg" "box") ("grep" "box"))))
      (setq seen nil probes nil available-rg t)
      (+project/ripgrep "/fs:local:/tmp/project/")
      (should (equal seen '(rg "/tmp/project/" nil)))
      (should (equal (reverse probes)
                     '(("rg" "local")))))))

(ert-deftest project-search-keeps-custom-consult-command ()
  "A changed Consult command shape must keep the user's chosen search tool."
  (require 'consult)
  (let ((consult-ripgrep-args "env SEARCH_MODE=custom rg --null")
        selected)
    (cl-letf (((symbol-function '+direnv-update-environment-maybe) #'ignore)
              ((symbol-function 'remote-search-ripgrep)
               (lambda (&rest _arguments)
                 (ert-fail "Managed rg replaced a custom Consult command")))
              ((symbol-function 'consult-ripgrep)
               (lambda (root &optional initial)
                 (setq selected (list root initial consult-ripgrep-args)))))
      (+project/ripgrep "/tmp/project/" "needle")
      (should
       (equal selected
              '("/tmp/project/" "needle"
                "env SEARCH_MODE=custom rg --null"))))))

(ert-deftest project-search-shortcut-keeps-rg-menu-and-remote-fallback ()
  (require 'rg)
  (should (eq (lookup-key global-map (kbd "C-c s"))
              #'+menus/rg-menu-or-project-search))
  (let (available selected)
    (cl-letf (((symbol-function '+project-current-root)
               (lambda () "/fs:box:/work/"))
              ((symbol-function 'remote-executable-find)
               (lambda (_program _context) available))
              ((symbol-function 'rg-menu)
               (lambda () (setq selected 'rg-menu)))
              ((symbol-function '+project/ripgrep)
               (lambda (root &optional _initial rg-unavailable)
                 (setq selected (list 'grep root rg-unavailable)))))
      (+menus/rg-menu-or-project-search)
      (should (equal selected '(grep "/fs:box:/work/" t)))
      (setq available t selected nil)
      (+menus/rg-menu-or-project-search)
      (should (eq selected 'rg-menu)))))

(ert-deftest project-magit-explicit-root-owns-status-identity ()
  "An explicit logical repo must win over the caller's different target."
  (let ((status (generate-new-buffer " *remote-magit-origin-test*")))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory "/fs:caller:/tmp/"))
            (cl-letf (((symbol-function 'magit-toplevel)
                       (lambda () "/tmp/repo/")))
              (should (eq (+magit-status-logical-root-a
                           (lambda (&rest _arguments) status)
                           "/fs:chosen:/tmp/repo/")
                          status))
              (should (equal (buffer-local-value '+magit--logical-root status)
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
            (cl-letf (((symbol-function '+project-activate) #'ignore)
                      ((symbol-function 'magit-status-setup-buffer)
                       (lambda (&rest arguments)
                         (setq seen-root (car arguments)
                               status-buffer (apply original arguments)))))
              (+project/magit-status logical-root)))
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
        (+project-local--scoped-root nil)
        (current "/tmp/first/"))
    (cl-letf (((symbol-function '+project-current-root)
               (lambda () current)))
      (let ((+project-local--scoped-root
             (cons (current-buffer) (+project-local-root))))
        (setq current "/tmp/second/")
        (should (equal (+project-local-root) "/tmp/first/"))
        (with-temp-buffer
          (setq default-directory "/tmp/")
          (should (equal (+project-local-root) "/tmp/second/"))))
      (should (equal (+project-local-root) "/tmp/second/")))))

(ert-deftest treemacs-local-logical-project-uses-native-model-path ()
  (should
   (equal
    (+treemacs-project-path "/fs:local:/tmp/project/")
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
      (+treemacs-project-path "/fs:box:/work/project/")
      "/ssh:box:/work/project"))))

(ert-deftest +project-roots-have-one-spelling-per-target ()
  "Local roots stay native; remote roots are `/fs:' whatever spelled them."
  (should (equal (+project-normalize-root "/fs:local:/tmp/project")
                 "/tmp/project/"))
  (should (equal (+project-normalize-root "/tmp/project") "/tmp/project/"))
  (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil)))
    (should (equal (+project-normalize-root "/fs:box:/work/project")
                   "/fs:box:/work/project/"))
    (should (equal (+project-normalize-root "/ssh:box:/work/project/")
                   "/fs:box:/work/project/"))))

(ert-deftest +project-known-remote-projects-are-listed-without-probing ()
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
          (should (equal (+project-known-projects)
                         (list "/fs:box:/work/project/"
                               (file-name-as-directory local))))
          (should-not probed))
      (delete-directory local))))

(ert-deftest +project-remote-root-registers-through-its-workspace ()
  (let ((projectile-known-projects nil)
        opened added remembered)
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil))
              ((symbol-function 'remote-workspace-open)
               (lambda (root &rest _) (setq opened root)))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'projectile-project-p) (lambda (&rest _) t))
              ((symbol-function '+project-unignore-root) #'ignore)
              ((symbol-function 'projectile-known-projects)
               (lambda () projectile-known-projects))
              ((symbol-function 'projectile-add-known-project)
               (lambda (root)
                 (setq added root)
                 (push root projectile-known-projects)))
              ((symbol-function 'project-remember-project)
               (lambda (project) (setq remembered project))))
      (should (equal (+project-register-root "/ssh:box:/work/project")
                     "/fs:box:/work/project/"))
      (should (equal opened "/fs:box:/work/project/"))
      (should (equal added "/fs:box:/work/project/"))
      (should (equal remembered
                     '(transient . "/fs:box:/work/project/"))))))

(ert-deftest +project-registration-failure-closes-only-its-new-workspace ()
  "A rejected remote root must not leave a newly opened workspace behind."
  (let ((new-workspace (remote-workspace-create :root "/fs:box:/missing/"))
        (old-workspace (remote-workspace-create :root "/fs:box:/missing/"))
        existing closed)
    (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil))
              ((symbol-function 'remote-get-workspace)
               (lambda (&rest _) existing))
              ((symbol-function 'remote-workspace-open)
               (lambda (&rest _) (or existing new-workspace)))
              ((symbol-function 'remote-workspace-close)
               (lambda (workspace &rest _) (push workspace closed)))
              ((symbol-function 'file-directory-p) (lambda (&rest _) nil)))
      (should-error (+project-register-root "/fs:box:/missing/")
                    :type 'user-error)
      (should (equal closed (list new-workspace)))
      (setq existing old-workspace
            closed nil)
      (should-error (+project-register-root "/fs:box:/missing/")
                    :type 'user-error)
      (should-not closed))))

(ert-deftest +project-registration-rolls-back-partial-owner-state ()
  "A later project.el failure restores the prior ignore and Projectile state."
  (let* ((directory (make-temp-file "project-register-rollback-" t))
         (root (file-name-as-directory
                (expand-file-name "repository" directory)))
         (+project-ignored-roots-file
          (expand-file-name "ignored-roots.el" directory))
         (+project-ignored-roots (list root))
         (+project-ignored-roots-load-error nil)
         (projectile-known-projects nil)
         (native-known nil))
    (unwind-protect
        (progn
          (make-directory root)
          (+project-save-ignored-roots)
          (cl-letf (((symbol-function 'projectile-project-p) (lambda (&rest _) t))
                    ((symbol-function 'projectile-known-projects)
                     (lambda () projectile-known-projects))
                    ((symbol-function 'projectile-ignored-project-p)
                     (lambda (candidate)
                       (+project-ignored-root-p candidate)))
                    ((symbol-function 'projectile-add-known-project)
                     (lambda (candidate)
                       (push candidate projectile-known-projects)))
                    ((symbol-function 'projectile-merge-known-projects)
                     #'ignore)
                    ((symbol-function 'project-known-project-roots)
                     (lambda () native-known))
                    ((symbol-function 'project-remember-project)
                     (lambda (_project)
                       (push root native-known)
                       (error "simulated native state write failure")))
                    ((symbol-function 'project-forget-project)
                     (lambda (candidate)
                       (setq native-known (delete candidate native-known)))))
            (should-error (+project-register-root root))
            (should-not projectile-known-projects)
            (should-not native-known)
            (should (equal +project-ignored-roots (list root)))
            (should (equal (+project-load-ignored-roots) (list root)))))
      (delete-directory directory t))))

(ert-deftest +project-registration-persists-through-native-package-apis ()
  "A real local project is remembered by each owner in isolated state files."
  (require 'project)
  (require 'projectile)
  (let* ((directory (make-temp-file "project-register-live-" t))
         (root (file-name-as-directory
                (expand-file-name "repository" directory)))
         (project-list-file (expand-file-name "projects.eld" directory))
         (project--list nil)
         (projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" directory))
         (projectile-known-projects nil)
         (projectile-known-projects-on-file nil)
         (projectile-auto-discover-projects nil)
         (projectile-auto-cleanup-known-projects nil)
         (+project-ignored-roots-file
          (expand-file-name "ignored-roots.el" directory))
         (+project-ignored-roots nil)
         (+project-ignored-roots-load-error nil))
    (unwind-protect
        (progn
          (make-directory root)
          (with-temp-file (expand-file-name ".projectile" root))
          (should (equal (+project-register-root root) root))
          (should (+project-root-member-p root projectile-known-projects))
          (should (+project-root-member-p
                   root (project-known-project-roots)))
          (should (file-exists-p projectile-known-projects-file))
          (should (file-exists-p project-list-file)))
      (delete-directory directory t))))

(ert-deftest +project-registration-respects-projectile-exclusions ()
  "A Projectile exclusion must not silently report registration success."
  (let* ((root (make-temp-file "project-register-excluded-" t))
         (+project-ignored-roots (list (file-name-as-directory root)))
         (+project-ignored-roots-load-error nil)
         (projectile-known-projects nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-project-p) (lambda (&rest _) t))
                  ((symbol-function 'projectile-known-projects)
                   (lambda () projectile-known-projects))
                  ((symbol-function 'projectile-ignored-project-p)
                   (lambda (&rest _) t))
                  ((symbol-function '+project-unignore-root)
                   (lambda (&rest _) (ert-fail "exclusion changed ignore state"))))
          (should-error (+project-register-root root) :type 'user-error)
          (should (equal +project-ignored-roots
                         (list (file-name-as-directory root)))))
      (delete-directory root t))))

(ert-deftest +project-registration-waits-for-damaged-ignore-file-repair ()
  "Neither manual nor batch registration may overwrite unknown ignore rules."
  (let ((+project-ignored-roots-load-error "state file is truncated"))
    (cl-letf (((symbol-function 'remote-workspace-open)
               (lambda (&rest _) (ert-fail "opened a workspace")))
              ((symbol-function 'projectile-discover-projects-in-directory)
               (lambda (&rest _) (ert-fail "started discovery"))))
      (should-error (+project-register-root "/fs:box:/work/project/")
                    :type 'user-error)
      (should-error (+project/discover-projects-in-directory
                     "/tmp/projects/" 2)
                    :type 'user-error))))

(ert-deftest +project-discovery-removes-only-failed-new-roots ()
  "Batch discovery keeps successful roots and reports a failed registration."
  (let* ((directory (make-temp-file "project-discovery-" t))
         (first (file-name-as-directory
                 (expand-file-name "first" directory)))
         (second (file-name-as-directory
                  (expand-file-name "second" directory)))
         (existing (file-name-as-directory
                    (expand-file-name "existing" directory)))
         (projectile-known-projects (list existing))
         (+project-ignored-roots-load-error nil)
         (attempted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-known-projects)
                   (lambda () projectile-known-projects))
                  ((symbol-function 'projectile-discover-projects-in-directory)
                   (lambda (&rest _)
                     (setq projectile-known-projects
                           (append projectile-known-projects
                                   (list first second)))))
                  ((symbol-function '+project-register-root)
                   (lambda (root)
                     (push root attempted)
                     (when (equal root first)
                       (error "first project failed"))))
                  ((symbol-function 'projectile-merge-known-projects) #'ignore))
          (should-error
           (+project/discover-projects-in-directory directory 2)
           :type 'user-error)
          (should (equal (nreverse attempted) (list first second)))
          (should (equal projectile-known-projects (list existing second))))
      (delete-directory directory t))))

(ert-deftest +project-discovery-reports-incomplete-cleanup ()
  "An owner that retains a failed root is named in the discovery error."
  (let* ((directory (make-temp-file "project-discovery-stuck-" t))
         (root (file-name-as-directory
                (expand-file-name "stuck" directory)))
         (projectile-known-projects nil)
         (+project-ignored-roots-load-error nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-known-projects)
                   (lambda () projectile-known-projects))
                  ((symbol-function 'projectile-discover-projects-in-directory)
                   (lambda (&rest _)
                     (setq projectile-known-projects (list root))))
                  ((symbol-function '+project-register-root)
                   (lambda (&rest _) (error "registration failed")))
                  ((symbol-function
                    '+project-remove-from-projectile-known-projects)
                   #'ignore))
          (let ((error-text
                 (condition-case err
                     (progn
                       (+project/discover-projects-in-directory directory 2)
                       nil)
                   (user-error (error-message-string err)))))
            (should (string-match-p "registration failed" error-text))
            (should (string-match-p "Projectile still remembers" error-text))))
      (delete-directory directory t))))

(ert-deftest +project-discovery-cancel-cleans-unfinished-roots ()
  "A cancelled scan preserves older entries and removes this scan's entries."
  (let* ((directory (make-temp-file "project-scan-cancel-" t))
         (existing (file-name-as-directory
                    (expand-file-name "existing" directory)))
         (new (file-name-as-directory
               (expand-file-name "new" directory)))
         (projectile-known-projects (list existing))
         (+project-ignored-roots-load-error nil)
         (cancelled nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-known-projects)
                   (lambda () projectile-known-projects))
                  ((symbol-function 'projectile-discover-projects-in-directory)
                   (lambda (&rest _)
                     (push new projectile-known-projects)
                     (signal 'quit nil)))
                  ((symbol-function 'projectile-merge-known-projects) #'ignore))
          (condition-case nil
              (+project/discover-projects-in-directory directory 2)
            (quit (setq cancelled t)))
          (should cancelled)
          (should (equal projectile-known-projects (list existing))))
      (delete-directory directory t))))

(ert-deftest +project-discovery-cancel-keeps-earlier-successes ()
  "Cancelling one registration clears it and pending roots, not past success."
  (let* ((directory (make-temp-file "project-register-cancel-" t))
         (first (file-name-as-directory
                 (expand-file-name "first" directory)))
         (second (file-name-as-directory
                  (expand-file-name "second" directory)))
         (third (file-name-as-directory
                 (expand-file-name "third" directory)))
         (projectile-known-projects nil)
         (+project-ignored-roots-load-error nil)
         (attempted nil)
         (cancelled nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-known-projects)
                   (lambda () projectile-known-projects))
                  ((symbol-function 'projectile-discover-projects-in-directory)
                   (lambda (&rest _)
                     (setq projectile-known-projects
                           (list first second third))))
                  ((symbol-function '+project-register-root)
                   (lambda (root)
                     (push root attempted)
                     (when (equal root second) (signal 'quit nil))))
                  ((symbol-function 'projectile-merge-known-projects) #'ignore))
          (condition-case nil
              (+project/discover-projects-in-directory directory 2)
            (quit (setq cancelled t)))
          (should cancelled)
          (should (equal (nreverse attempted) (list first second)))
          (should (equal projectile-known-projects (list first))))
      (delete-directory directory t))))

(ert-deftest treemacs-persistence-migrates-only-path-records ()
  (cl-letf
      (((symbol-function '+treemacs-project-path)
        (lambda (path)
          (pcase path
            ("/fs:local:/tmp/project" "/tmp/project")
            ("/fs:box:/work/project" "/ssh:box:/work/project")
            (_ path)))))
    (should
     (equal
      (+treemacs-normalize-persist-lines-a
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
      (+treemacs-visit-path "/ssh:box:/work/Main.java")
      "/fs:box:/work/Main.java"))
    (should
     (equal
      (+treemacs-visit-path "/fs:local:/tmp/Main.java")
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
              (((symbol-function '+treemacs-visit-path)
                (lambda (_path) logical-file)))
            (+treemacs-get-imenu-index-a
             (lambda (file)
               (setq captured file)
               nil)
             physical-file)
            (should
             (eq
              (+treemacs-visit-logical-path-a
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
          (cl-letf (((symbol-function '+treemacs-visit-path)
                     (lambda (_path) native-file)))
            (+treemacs-get-imenu-index-a #'treemacs--get-imenu-index
                                           native-file))
          (should (buffer-live-p source)))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source))
      (delete-file native-file))))

(ert-deftest treemacs-file-events-ignore-a-directory-deletion-race ()
  (should-not
   (+treemacs-process-file-events-safely-a
    (lambda ()
      (signal
       'file-missing
       '("Opening directory" "No such file or directory"
         "/fs:local:/tmp/project/build/classes"))))))

(ert-deftest treemacs-file-events-preserve-other-missing-file-errors ()
  (should-error
   (+treemacs-process-file-events-safely-a
    (lambda ()
      (signal
       'file-missing
       '("Opening input file" "No such file or directory"
         "/fs:local:/tmp/project/MISSING"))))
   :type 'file-missing))

(ert-deftest +project-activation-is-canonical-idempotent-and-does-no-source-io ()
  (let ((+project-active-root nil)
        (+project--perspective-roots (make-hash-table :test #'eq))
        events)
    (let ((+project-activated-hook (list (lambda (root) (push root events)))))
      (cl-letf (((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "source IO")))
                ((symbol-function 'remote-session-acquire) (lambda (&rest _) (ert-fail "connection"))))
        (+project-activate "/tmp/project")
        (+project-activate "/fs:local:/tmp/project/")
        (+project/leave)
        (+project/leave)
        (+project-activate "/fs:box:/work/project/")
        (+project-activate "/fs:box:/work/project")
        (+project/leave)
      (should (equal (reverse events)
                       '("/fs:local:/tmp/project/" nil "/fs:box:/work/project/" nil)))))))

(ert-deftest +project-membership-uses-logical-directory-boundaries ()
  "Buffer cleanup must not probe a target or include a sibling root."
  (cl-letf (((symbol-function 'file-in-directory-p)
             (lambda (&rest _) (ert-fail "project membership did file I/O"))))
    (should (+project-path-inside-root-p
             "/fs:box:/work/app/file.el" "/fs:box:/work/app/"))
    (should (+project-path-inside-root-p
             "/fs:box:/work/app/" "/fs:box:/work/app/"))
    (should-not (+project-path-inside-root-p
                 "/fs:box:/work/app-other/file.el" "/fs:box:/work/app/"))))

(ert-deftest +project-native-remembering-allows-unmanaged-projects ()
  "Emacs project.el may remember a project before Projectile knows it."
  (require 'project)
  (let* ((directory (make-temp-file "native-project-" t))
         (project-list-file (expand-file-name "projects.eld" directory))
         (project--list nil)
         (project-list-exclude nil)
         (root (file-name-as-directory directory)))
    (unwind-protect
        (progn
          (should-not (advice-member-p
                       '+project--remember-dir-visible-only
                       'project--remember-dir))
          (project-remember-project (cons 'transient root))
          (should (assoc (abbreviate-file-name root) project--list)))
      (delete-directory directory t))))

(ert-deftest +project-removal-uses-native-forget-across-symlinks ()
  "A remembered alias is removed through project.el's public command."
  (require 'project)
  (let* ((directory (make-temp-file "native-forget-" t))
         (root (expand-file-name "repository" directory))
         (alias (expand-file-name "repository-link" directory))
         (project-list-file (expand-file-name "projects.eld" directory))
         (project--list nil))
    (unwind-protect
        (progn
          (make-directory root)
          (make-symbolic-link root alias)
          (setq project--list (list (list (file-name-as-directory alias))))
          (should (+project-remove-from-project-list root))
          (should-not project--list)
          (should-not (project-known-project-roots)))
      (delete-directory directory t))))

(ert-deftest +project-prune-uses-native-forget-only-for-hidden-roots ()
  "Startup cleanup preserves project.el's ordinary remembered projects."
  (require 'project)
  (let* ((directory (make-temp-file "native-prune-" t))
         (hidden (file-name-as-directory (expand-file-name "hidden" directory)))
         (visible (file-name-as-directory (expand-file-name "visible" directory)))
         (project-list-file (expand-file-name "projects.eld" directory))
         (project--list (list (list hidden) (list visible)))
         (projectile-known-projects nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-load-known-projects) #'ignore)
                  ((symbol-function '+project-hidden-root-p)
                   (lambda (root) (equal root hidden))))
          (+project/prune-hidden-project-state)
          (should (equal (project-known-project-roots) (list visible))))
      (delete-directory directory t))))

(ert-deftest +project-ignored-root-save-failure-restores-memory ()
  "A failed state write leaves the project available for a later retry."
  (let ((+project-ignored-roots nil)
        (root "/fs:local:/tmp/ignored-save-test/"))
    (cl-letf (((symbol-function '+project-save-ignored-roots)
               (lambda () (error "simulated write failure"))))
      (should-error (+project-ignore-root root))
      (should-not +project-ignored-roots))
    (setq +project-ignored-roots (list root))
    (cl-letf (((symbol-function '+project-save-ignored-roots)
               (lambda () (error "simulated write failure"))))
      (should-error (+project-unignore-root root))
      (should (equal +project-ignored-roots (list root))))))

(ert-deftest +project-damaged-ignored-roots-require-repair-before-writing ()
  "A damaged state file survives startup and cannot be overwritten by cleanup."
  (let* ((directory (make-temp-file "ignored-project-state-" t))
         (+project-ignored-roots-file
          (expand-file-name "ignored-roots.el" directory))
         (+project-ignored-roots '("/fs:local:/existing/"))
         (+project-ignored-roots-load-error nil)
         (warning-minimum-level :emergency)
         (pruned nil))
    (unwind-protect
        (progn
          (with-temp-file +project-ignored-roots-file
            (insert "(\"/fs:local:/hidden/\""))
          (should-not (+project-load-ignored-roots))
          (should (stringp +project-ignored-roots-load-error))
          (cl-letf (((symbol-function 'projectile-load-known-projects)
                     (lambda () (setq pruned t))))
            (+project/prune-hidden-project-state)
            (should-not pruned))
          (should-error (+project-ignore-root "/fs:local:/new/")
                        :type 'user-error)
          (should (equal +project-ignored-roots
                         '("/fs:local:/existing/")))
          (should (equal (with-temp-buffer
                           (insert-file-contents +project-ignored-roots-file)
                           (buffer-string))
                         "(\"/fs:local:/hidden/\""))
          (with-temp-file +project-ignored-roots-file
            (insert "(\"/fs:local:/hidden/\")\n"))
          (cl-letf (((symbol-function '+project/prune-hidden-project-state)
                     (lambda () (setq pruned t))))
            (+project/reload-ignored-roots))
          (should pruned)
          (should-not +project-ignored-roots-load-error)
          (should (equal +project-ignored-roots
                         '("/fs:local:/hidden/"))))
      (delete-directory directory t))))

(ert-deftest +project-ignored-roots-reject-extra-forms ()
  "Extra forms in the state file must not be silently discarded."
  (let* ((directory (make-temp-file "ignored-project-extra-" t))
         (+project-ignored-roots-file
          (expand-file-name "ignored-roots.el" directory))
         (+project-ignored-roots-load-error nil)
         (warning-minimum-level :emergency))
    (unwind-protect
        (progn
          (with-temp-file +project-ignored-roots-file
            (insert "(\"/fs:local:/hidden/\")\n(unexpected)\n"))
          (should-not (+project-load-ignored-roots))
          (should (string-match-p "extra forms"
                                  +project-ignored-roots-load-error)))
      (delete-directory directory t))))

(ert-deftest +project-remove-preserves-vetoed-buffers ()
  "Project cleanup counts a kill-query veto as a preserved buffer."
  (let* ((root (make-temp-file "project-remove-veto-" t))
         (kept (generate-new-buffer " *project-kept*"))
         (closed (generate-new-buffer " *project-closed*")))
    (unwind-protect
        (progn
          (with-current-buffer kept
            (setq-local buffer-file-name (expand-file-name "kept.el" root))
            (add-hook 'kill-buffer-query-functions (lambda () nil) nil t))
          (with-current-buffer closed
            (setq-local buffer-file-name (expand-file-name "closed.el" root)))
          (let ((result (+project-kill-associated-buffers root)))
            (should (= (plist-get result :killed) 1))
            (should (= (plist-get result :preserved) 1))
            (should (buffer-live-p kept))
            (should-not (buffer-live-p closed))))
      (when (buffer-live-p kept)
        (with-current-buffer kept (setq kill-buffer-query-functions nil))
        (kill-buffer kept))
      (when (buffer-live-p closed) (kill-buffer closed))
      (delete-directory root t))))

(ert-deftest +project-remove-continues-after-component-failure ()
  "An incomplete removal cleans the other components and permits retry."
  (let* ((root "/fs:local:/tmp/remove-retry-test/")
         (+project-active-root root)
         (+project-ignored-roots (list root))
         (native-comp-enable-subr-trampolines nil)
         calls candidates
         (completing-read-function
          (lambda (_prompt choices &rest _)
            (setq candidates choices)
            root)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function '+project-perspective-name)
               (lambda (&rest _) "test"))
              ((symbol-function '+project-ignore-root)
               (lambda (&rest _) (push 'ignore calls)))
              ((symbol-function '+project-remove-from-projectile-known-projects)
               (lambda (&rest _) (push 'projectile calls)))
              ((symbol-function '+project-remove-from-project-list)
               (lambda (&rest _) (push 'native calls)
                 (error "simulated project.el failure")))
              ((symbol-function '+project-remove-projectile-cache)
               (lambda (&rest _) (push 'cache calls)))
              ((symbol-function '+project-remove-from-treemacs-workspaces)
               (lambda (&rest _) (push 'tree calls)))
              ((symbol-function '+project-kill-perspective-if-exists)
               (lambda (&rest _) (push 'perspective calls)))
              ((symbol-function '+project-kill-associated-buffers)
               (lambda (&rest _) (push 'buffers calls)
                 '(:killed 0 :preserved 1 :errors nil)))
              ((symbol-function '+project/leave)
               (lambda () (push 'leave calls)))
              ((symbol-function '+project-known-projects) (lambda () nil))
              ((symbol-function '+project-current-root) (lambda () nil)))
      (should-error (+project/remove-known-project root) :type 'user-error)
      (should (equal (nreverse calls)
                     '(ignore projectile native cache tree perspective buffers leave)))
      (should (equal (+project-read-removable-root)
                     (+project-normalize-root root)))
      (should (member root candidates)))))

(ert-deftest +project-reentry-keeps-visible-editor-instead-of-opening-dired ()
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
          (cl-letf (((symbol-function '+project-switch-perspective) #'ignore)
                    ((symbol-function '+direnv-update-environment-maybe) #'ignore)
                    ((symbol-function '+project-activate) #'ignore)
                    ((symbol-function 'dired)
                     (lambda (&rest _) (setq opened t))))
            (+project/switch root)
            (should-not opened)
            (should (eq (window-buffer (selected-window)) source))))
      (kill-buffer source)
      (kill-buffer tree)
      (delete-directory root t))))

(ert-deftest +project-navigation-activates-after-success-and-not-after-cancellation ()
  (let ((+project-active-root nil)
        (+project--perspective-roots (make-hash-table :test #'eq))
        events)
    (let ((+project-activated-hook (list (lambda (root) (push root events)))))
      (cl-letf (((symbol-function '+direnv-update-environment-maybe) #'ignore)
                ((symbol-function 'projectile-find-file-in-directory) #'ignore)
                ((symbol-function '+project-recent-file-choices)
                 (lambda (&rest _)
                   '(("file.el" . "/tmp/second/file.el"))))
                ((symbol-function 'projectile-completing-read)
                 (lambda (&rest _) (user-error "cancel"))))
        (+project/find-file "/tmp/first/")
        (should-error (+project/recent-file "/tmp/second/") :type 'user-error)
        (+with-project-root-context "/tmp/background/" nil)
        (should (equal events '("/fs:local:/tmp/first/")))))))

(ert-deftest +project-recent-files-use-logical-membership-without-target-probes ()
  "Recent-file choices exclude sibling roots and never stat the remote root."
  (let ((recentf-list '("/fs:box:/work/app/a.py"
                        "/fs:box:/work/app/sub/c.py"
                        "/fs:box:/work/app-other/b.py"
                        "/fs:other:/work/app/x.py"))
        (+project-active-root nil)
        (+project--perspective-roots (make-hash-table :test #'eq))
        (original-truename (symbol-function 'file-truename))
        offered opened)
    (cl-letf (((symbol-function 'remote-client-file-name)
               (lambda (&rest _) nil))
              ((symbol-function 'file-truename)
               (lambda (path &rest args)
                 (if (and (stringp path) (file-remote-p path))
                     (ert-fail "recent files probed a target")
                   (apply original-truename path args))))
              ((symbol-function '+direnv-update-environment-maybe) #'ignore)
              ((symbol-function 'projectile-completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered choices)
                 "sub/c.py"))
              ((symbol-function 'find-file)
               (lambda (file &rest _) (setq opened file))))
      (+project/recent-file "/fs:box:/work/app/")
      (should (equal offered '("a.py" "sub/c.py")))
      (should (equal opened "/fs:box:/work/app/sub/c.py"))
      (should (equal +project-active-root "/fs:box:/work/app/")))))

(ert-deftest +project-recent-files-keep-native-local-visit-paths ()
  "The project picker presents relative names but visits local files natively."
  (let* ((directory (make-temp-file "project-recent-local-" t))
         (root (file-name-as-directory
                (expand-file-name "repository" directory)))
         (file (expand-file-name "source.el" root))
         (sibling (expand-file-name "repository-other/source.el" directory)))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory (file-name-directory sibling))
          (with-temp-file file (insert "project file"))
          (with-temp-file sibling (insert "other project file"))
          (let ((recentf-list (list file sibling))
                (default-directory "/fs:box:/work/"))
            (should (equal (+project-recent-file-choices root)
                           (list (cons "source.el" file))))))
      (delete-directory directory t))))

(ert-deftest +project-all-files-open-local-choice-from-remote-buffer ()
  "A Projectile candidate cannot borrow the source buffer's target."
  (let* ((directory (make-temp-file "project-all-local-" t))
         (file (expand-file-name "source.el" directory))
         (projectile-known-projects (list directory))
         visited visit-directory)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:box:/work/")
          (cl-letf (((symbol-function 'projectile-project-files)
                     (lambda (root)
                       (should (equal root directory))
                       '("source.el")))
                    ((symbol-function 'projectile-completing-read)
                     (lambda (_prompt choices &rest _)
                       (should (equal choices (list file)))
                       file))
                    ((symbol-function 'find-file)
                     (lambda (chosen &rest _)
                       (setq visited chosen
                             visit-directory default-directory))))
            (projectile-find-file-in-known-projects)
            (should (equal visited file))
            (should (equal visit-directory temporary-file-directory))))
      (delete-directory directory t))))

(ert-deftest +project-perspective-switches-follow-recorded-user-navigation ()
  ;; Exercise the installed package, including its temporary NORECORD switches.
  (require 'perspective)
  (let ((+project-active-root nil)
        (+project--perspective-roots (make-hash-table :test #'eq))
        (original (persp-current-name))
        (first (make-temp-name "agenda-persp-a-"))
        (second (make-temp-name "agenda-persp-b-"))
        events)
    (let ((+project-activated-hook (list (lambda (root) (push root events)))))
      (unwind-protect
          (progn
            (persp-switch first)
            (+project-activate "/tmp/a/" t)
            (persp-switch second)
            (should-not +project-active-root)
            (+project-activate "/tmp/b/" t)
            (let ((before (copy-sequence events)))
              (with-perspective first (should (equal +project-active-root "/fs:local:/tmp/b/")))
              (should (equal events before)))
            (persp-switch first)
            (should (equal +project-active-root "/fs:local:/tmp/a/"))
            (+project/leave)
            (persp-switch second)
            (persp-switch first)
            (should-not +project-active-root))
        (persp-switch original)
        (dolist (name (list first second))
          (when (member name (persp-names)) (persp-kill name)))))))

(ert-deftest +project-workspace-close-releases-only-associated-project ()
  (require 'remote-workspace)
  (let ((+project-active-root nil)
        (+project--perspective-roots (make-hash-table :test #'eq))
        (remote-workspaces (make-hash-table :test #'equal))
        events)
    (let ((+project-activated-hook (list (lambda (root) (push root events)))))
      (let ((one (remote-workspace-open "/fs:local:/tmp/agenda-one/" :connect nil))
            (two (remote-workspace-open "/fs:local:/tmp/agenda-two/" :connect nil)))
        (should-not events) ; Workspace opening alone is not user navigation.
        (+project-activate "/tmp/agenda-one/" t)
        (remote-workspace-close two)
        (should +project-active-root)
        (remote-workspace-close one)
        (should-not +project-active-root)
        (should (equal (reverse events) '("/fs:local:/tmp/agenda-one/" nil)))))))

(ert-deftest +project-exit-releases-the-native-agenda-lease ()
  (require 'noema-agenda)
  (let ((+project-active-root nil)
        (+project-activated-hook '(noema-agenda-activate-project))
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
      (+project-activate "/fs:local:/tmp/agenda-enter/")
      (should (equal (car calls) '("enter" (root . "/tmp/agenda-enter/") (lease . "emacs-agenda:1"))))
      (+project/leave)
      (should (equal (car calls) '("leave" (id . "scope-one") (lease . "emacs-agenda:1"))))
      (should-not noema-agenda--project-root)
      (should-not noema-agenda--project-scope))))

(ert-deftest +project-directory-marker-answers-wildcards-from-one-listing ()
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
                  (should (equal (+project--directory-marker
                                  root '("?*.sln") 'files-only)
                                 "?*.sln"))
                  (should (equal (+project--directory-marker
                                  root '("Makefile"))
                                 "Makefile"))
                  (should-not (+project--directory-marker
                               root '("?*.xcodeproj" "?*.csproj")))
                  ;; A marker naming a directory is not a file marker.
                  (should-not (+project--directory-marker
                               root '("src") 'files-only))
                  (should (equal (+project--directory-marker root '("src"))
                                 "src"))
                  ;; One listing per call, never one per marker.
                  (should (= listings 5)))
              (advice-remove 'directory-files counted))))
      (delete-directory root t))))

(ert-deftest +project-directory-marker-override-is-installed ()
  "Installation implies the shape check passed.
`func-arity' reports the advice once one is installed, so the check itself
cannot be repeated here."
  (require 'projectile)
  (should (advice-member-p #'+project--directory-marker
                           'projectile--directory-marker)))

(provide 'init-project-remote-tests)
;;; init-project-remote-tests.el ends here
