;;; init-health-tests.el --- Health board entry and lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-health-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-health)

(ert-deftest health-board-refresh-does-not-run-batch-checks ()
  "Opening or refreshing the board must not start nested Emacs processes."
  (let ((my/health--batch-results nil))
    (cl-letf (((symbol-function 'my/health--run-batch)
               (lambda (&rest _) (ert-fail "health board started a batch check"))))
      (unwind-protect
          (progn
            (my/health-report t)
            (with-current-buffer "*Health*"
              (goto-char (point-min))
              (should (search-forward "NOT RUN" nil t))
              (aaron-ui-board-refresh)))
        (when-let* ((buffer (get-buffer "*Health*")))
          (kill-buffer buffer))))))

(ert-deftest health-explicit-check-records-result-for-board ()
  "An explicit check is shown by the board without being run again."
  (let ((my/health--batch-results nil)
        (runs 0))
    (cl-letf (((symbol-function 'my/health--stale-startup-bytecode-files)
               (lambda () nil))
              ((symbol-function 'my/health--run-batch)
               (lambda (&rest _)
                 (cl-incf runs)
                 (list :ok t :status 0 :output "INIT-OK"))))
      (unwind-protect
          (progn
            (should (plist-get (my/health-startup-check) :ok))
            (my/health-report t)
            (should (= runs 1))
            (with-current-buffer "*Health*"
              (goto-char (point-min))
              (should (search-forward "INIT-OK" nil t))
              (should (eq (lookup-key my/health-mode-map (kbd "s"))
                          'my/health-startup-check))))
        (when-let* ((buffer (get-buffer "*Health*")))
          (kill-buffer buffer))))))

(ert-deftest health-public-commands-are-discoverable ()
  "Purpose-first aliases stay interactive while old entry points work."
  (dolist (command '(config-health config-health-menu
                     config-compile config-compile-menu
                     project-workbench project-menu
                     project-add-to-workbench project-discover-in-directory
                     project-remove-from-workbench project-reload-ignored-roots
                     project-run project-rerun project-run-menu
                     bazel-target-menu bazel-target-build bazel-target-test
                     bazel-target-run bazel-target-insert bazel-target-jump
                     task-run task-rerun task-build task-build-rerun task-menu
                     output-menu output-switch
                     activity-board jupyter-board noema-menu noema-move-note
                     noema-trash-note
                     noema-reports-menu noema-report-tag-overlap
                     command-hub code-actions-menu diagnostics-menu
                     merge-conflict-menu bookmark-actions-menu
                     debug-profile-menu copy-as-format-menu mouse-actions-menu
                     keybinding-guide font-refresh structure-up
                     open-file-location file-open
                     activity-reload-sprites latex-preview-doctor))
    (should (commandp command)))
  (should (member "noema-trash-note"
                  (all-completions "noema-trash-" obarray #'commandp))))

(ert-deftest health-public-menus-own-their-layouts ()
  "The searchable menu name must also be its Transient layout identity."
  (dolist (entry '((command-hub "p" project-menu)
                   (command-hub "w" workspace-menu)
                   (project-menu "A" project-add-to-workbench)
                   (workspace-menu "n" my/workspace-new)
                   (project-run-menu "r" project-run)
                   (bazel-target-menu "b" bazel-target-build)
                   (task-menu "z" bazel-target-menu)
                   (task-menu "t" task-run)
                   (test-menu "n" my/test-nearest)
                   (git-menu "g" magit-status)
                   (git-menu "m" merge-conflict-menu)
                   (language-server-menu "h" my/language-server-manager)
                   (diagnostics-menu "p" my/diagnostics-project-ui)
                   (code-actions-menu "a" my/language-server-code-actions)
                   (debug-menu "d" dape)
                   (debug-menu "p" debug-profile-menu)
                   (editing-menu "c" copy-as-format-menu)
                   (editing-menu "p" image-crop)
                   (copy-as-format-menu "m" my/copy-as-format-markdown)
                   (mouse-actions-menu "b" ibuffer)
                   (output-menu "o" my/output-switch)
                   (config-compile-menu "b" my/compile-board)
                   (config-health-menu "h" my/health-report)
                   (jupyter-board-menu "j" my/jupyter-board)
                   (noema-menu "o" my/noema-open-current-note)
                   (noema-menu "m" noema-move-note)
                   (noema-menu "!d" noema-trash-note)
                   (noema-menu "!n" noema-reports-menu)
                   (noema-ai-menu "a" noema-context-send-region)
                   (noema-wiki-menu "h" my/noema-wiki-home)
                   (noema-wiki-menu "D" noema-reports-menu)
                   (noema-reports-menu "v" my/noema-wiki-reports)
                   (noema-reports-menu "t" noema-report-tag-overlap)
                   (bookmark-actions-menu "m" bookmark-set)
                   (file-menu "o" file-open)))
    (pcase-let ((`(,menu ,key ,command) entry))
      (should (member (symbol-name menu)
                      (all-completions (symbol-name menu)
                                       obarray #'commandp)))
      (should (eq (plist-get (cdr (transient-get-suffix menu key)) :command)
                  command)))))

(ert-deftest health-leader-routes-do-not-shadow-each-other ()
  "The primary entry keys must resolve to the intended live commands."
  (with-temp-buffer
    (evil-normal-state)
    (dolist (entry '(("SPC h c" . config-dispatch)
                    ("SPC h C" . helpful-command)
                    ("SPC h K" . my/open-keybinding-guide)
                    ("SPC h H" . config-health-menu)
                    ("SPC c ?" . diagnostics-menu)
                    ("SPC c ." . code-actions-menu)
                    ("SPC c B" . config-compile-menu)
                    ("SPC c s" . language-server-menu)
                    ("SPC c S" . my/language-server-workspace-symbol)
                    ("SPC r r" . project-run-menu)
                    ("SPC r R" . project-rerun)
                    ("SPC r k" . task-menu)
                    ("SPC r K" . task-rerun)
                    ("SPC r V" . my/maintenance-state-restore)
                    ("SPC r d" . debug-profile-menu)
                    ("SPC r o" . output-menu)
                    ("SPC r O" . output-switch)
                    ("SPC N o" . my/noema-open-current-note)
                    ("SPC N m" . noema-wiki-menu)
                    ("SPC n a" . my/navigation-beginning-of-defun)
                    ("SPC n d" . my/navigation-find-definition)
                    ("SPC n e" . my/navigation-end-of-defun)
                    ("SPC n u" . structure-up)
                    ("SPC n l" . open-file-location)
                    ("SPC f f" . file-open)
                    ("SPC SPC" . telescope)
                    ("SPC b x" . my/scratch-toggle)
                    ("SPC b ." . bookmark-actions-menu)
                    ("SPC p x" . my/scratch-project-toggle)
                    ("SPC t d" . tab-close)
                    ("SPC t D" . tab-close-other)
                    ("SPC t t" . tab-switch)
                    ("SPC w x" . kill-buffer-and-window)
                    ("SPC g m" . merge-conflict-menu)
                    ("SPC p l" . my/project-local-describe)
                    ("<f9>" . noema-agenda)))
      (should (eq (key-binding (kbd (car entry))) (cdr entry))))))

(ert-deftest health-hyper-project-and-agenda-routes-exist ()
  "macOS's advertised Hyper shortcuts must resolve after full startup."
  (when (eq system-type 'darwin)
    (should (eq (key-binding (kbd "H-d")) 'diagnostics-menu))
    (should (eq (key-binding (kbd "H-e")) 'code-actions-menu))
    (should (eq (key-binding (kbd "H-D")) 'debug-profile-menu))
    (should (eq (key-binding (kbd "H-o")) 'noema-menu))
    (should (eq (key-binding (kbd "H-p")) 'project-menu))
    (should (eq (key-binding (kbd "H-a")) 'noema-agenda))
    (should (eq (key-binding (kbd "H-N")) 'output-menu))))

(ert-deftest health-failed-optional-module-is-recorded-and-cleared-on-retry ()
  "A recovered optional module should disappear from the health report."
  (let* ((aaron-modules (make-hash-table :test 'equal))
         (aaron-module-load-failures nil)
         (aaron-module-optional-modules nil)
         (aaron-module-ledger-enabled nil)
         (warning-minimum-level :emergency)
         (module 'emacs-audit-test-module)
         (key (cons :health-test module))
         (directory (make-temp-file "emacs-audit-module-" t))
         (file (expand-file-name "emacs-audit-test-module.el" directory))
         (load-path (cons directory load-path)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(error \"test load failure\")\n"))
          (aaron-modules--declare `(:health-test (,module :optional t)))
          (aaron-module--load-config (aaron-module-get key))
          (should (equal aaron-module-optional-modules (list module)))
          (should (equal (alist-get module aaron-module-load-failures)
                         "test load failure"))
          (should (eq (aaron-module-state (aaron-module-get key)) 'failed))
          (with-temp-file file
            (insert "(provide 'emacs-audit-test-module)\n"))
          (aaron/load-module key)
          (should (eq (aaron-module-state (aaron-module-get key)) 'loaded))
          (should-not aaron-module-load-failures))
      (put :health-test module nil)
      (setq features (delq module features))
      (delete-directory directory t))))

(ert-deftest health-board-shows-optional-module-failures ()
  "Loaded, waiting and failed optional modules have distinct board states."
  (let ((aaron-module-optional-modules
         '(subr-x init-pending-example init-optional-example))
        (aaron-module-deferred-triggers
         '((init-pending-example . (org tex))))
        (aaron-module-load-failures '((init-optional-example . "load failed"))))
    (unwind-protect
        (progn
          (my/health-report t)
          (with-current-buffer "*Health*"
            (goto-char (point-min))
            (should (search-forward "Modules" nil t))
            (should (search-forward "subr-x" nil t))
            (should (search-forward "loaded" nil t))
            (should (search-forward "init-pending-example" nil t))
            (should (search-forward "waiting for org, tex" nil t))
            (should (search-forward "init-optional-example" nil t))
            (should (search-forward "load failed" nil t))))
      (when-let* ((buffer (get-buffer "*Health*")))
        (kill-buffer buffer)))))

(ert-deftest health-board-shares-settings-integrity-status ()
  "A failed settings save is visible from the main health entry."
  (let ((config--persist-failed t))
    (unwind-protect
        (progn
          (my/health-report t)
          (with-current-buffer "*Health*"
            (goto-char (point-min))
            (should (search-forward "Settings" nil t))
            (should (search-forward "1 issue(s)" nil t))))
      (when-let* ((buffer (get-buffer "*Health*")))
        (kill-buffer buffer)))))

(ert-deftest health-board-reports-damaged-project-state ()
  "A project state load failure has a repair path from the health board."
  (let ((my/project-ignored-roots-load-error "malformed ignored roots")
        (my/project-ignored-roots-file "/tmp/emacs-ignored-roots-test.el"))
    (unwind-protect
        (progn
          (my/health-report t)
          (with-current-buffer "*Health*"
            (goto-char (point-min))
            (should (search-forward "Project State" nil t))
            (should (search-forward "malformed ignored roots" nil t))
            (should (search-forward "emacs-ignored-roots-test.el" nil t))
            (should (search-forward "Reload" nil t))
            (cl-letf (((symbol-function 'project-reload-ignored-roots)
                       (lambda ()
                         (setq my/project-ignored-roots-load-error nil))))
              (my/health-reload-project-state))
            (goto-char (point-min))
            (should-not (search-forward "Project State" nil t))))
      (when-let* ((buffer (get-buffer "*Health*")))
        (kill-buffer buffer)))))

(ert-deftest health-command-report-requires-interactive-commands ()
  "A defined helper must not be reported as a working M-x command."
  (let ((my/health-important-commands '(health-test-helper)))
    (cl-letf (((symbol-function 'health-test-helper) (lambda () t)))
      (should-not (cdar (my/health--command-report))))))

(ert-deftest health-batch-check-uses-client-environment-from-remote-buffer ()
  "A remote buffer's PATH and HOME must not reach editor-owned batch Emacs."
  (with-temp-buffer
    (setq-local default-directory "/fs:box:/home/me/project/")
    (setq-local process-environment '("HOME=/home/me" "PATH=/remote/bin"))
    (setq-local exec-path '("/remote/bin"))
    (let (seen-directory seen-environment seen-exec-path)
      (cl-letf (((symbol-function 'my/health--emacs-program)
                 (lambda ()
                   (setq seen-directory default-directory
                         seen-environment process-environment
                         seen-exec-path exec-path)
                   "/usr/bin/true")))
        (should (plist-get (my/health--run-batch "--batch") :ok)))
      (should (equal seen-directory (my/health-config-root)))
      (should (equal seen-environment (default-value 'process-environment)))
      (should (equal seen-exec-path (default-value 'exec-path))))))

(ert-deftest health-board-uses-client-environment-from-remote-buffer ()
  "The health buffer itself belongs to the editor, even when opened remotely."
  (unwind-protect
      (with-temp-buffer
        (setq-local default-directory "/fs:box:/home/me/project/")
        (setq-local process-environment '("HOME=/home/me" "PATH=/remote/bin"))
        (setq-local exec-path '("/remote/bin"))
        (my/health-report t)
        (with-current-buffer "*Health*"
          (should (equal default-directory (my/health-config-root)))
          (should (equal process-environment
                         (default-value 'process-environment)))
          (should (equal exec-path (default-value 'exec-path)))))
    (when-let* ((buffer (get-buffer "*Health*")))
      (kill-buffer buffer))))

(ert-deftest init-reload-reports-a-load-failure ()
  "The top-level reload command must not turn a failed load into success."
  (let ((user-init-file (make-temp-file "broken-emacs-init-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file user-init-file
            (insert "(unfinished-form\n"))
          (should-error (my/reload-init) :type 'end-of-file))
      (delete-file user-init-file))))

(provide 'init-health-tests)
;;; init-health-tests.el ends here
