;;; init-pi-workflow-tests.el --- Pi and startup visibility -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'noema-pi-router)

(ert-deftest pi-workflow-c3-cold-app-chooses-dashboard-without-taking-file-launches ()
  (let ((noninteractive nil) (initial-buffer-choice nil)
        (command-line-args '("Emacs")))
    (+dashboard-configure-startup-choice)
    (let ((home (+window-dashboard-buffer)))
      (should (eq (funcall initial-buffer-choice) home))))
  (let ((noninteractive nil) (initial-buffer-choice nil)
        (command-line-args '("Emacs" "/tmp/requested.md")))
    (+dashboard-configure-startup-choice)
    (should-not initial-buffer-choice)))

(ert-deftest pi-workflow-c4-background-pi-is-discoverable-in-ibuffer ()
  (let ((pi (generate-new-buffer "*Pi ibuffer visibility test*")) listing)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer pi
            (setq major-mode 'agent-shell-mode)
            (setq-local +agent-acp-session-origin 'pi-emacs))
          (should (aaron-ibuffer-pi-buffer-p pi))
          (ibuffer)
          (setq listing (current-buffer))
          (ibuffer-update nil t)
          (goto-char (point-min))
          (should (search-forward (buffer-name pi) nil t))
          (beginning-of-line)
          (should (eq (ibuffer-current-buffer) pi))
          (should (equal (caar (ibuffer-current-filter-groups-with-position)) "Pi")))
      (when (buffer-live-p listing) (kill-buffer listing))
      (kill-buffer pi))))

(ert-deftest pi-workflow-c3-background-start-preserves-the-selected-buffer ()
  (save-window-excursion
    (let ((source (window-buffer (selected-window))) (noema-pi-assistant-mode t)
          (noninteractive nil) (main-called nil))
      (cl-letf (((symbol-function 'noema-pi-emacs-ensure)
                 (lambda (&optional focus) (should-not focus) (setq main-called t)))
                ((symbol-function 'noema-pi-router--project-activated) #'ignore))
        (noema-pi-router--startup)
        (should main-called)
        (should (eq (window-buffer (selected-window)) source))))))

(ert-deftest pi-workflow-c2-failed-restore-does-not-fall-back-to-a-new-session ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                +agent-acp-strict-restore t
                agent-shell--state (let ((state (make-hash-table)))
                                      (puthash :buffer (current-buffer) state)
                                      (puthash :supports-session-load t state)
                                      state))
    (let (failed)
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                ((symbol-function 'agent-shell-cwd) (lambda () temporary-file-directory))
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args) (funcall (plist-get args :on-failure) 'missing nil)))
                ((symbol-function '+agent-acp--restore-failed)
                 (lambda (_buffer id) (setq failed id)))
                ((symbol-function 'agent-shell--initiate-session-list-and-load)
                 (lambda (&rest _) (ert-fail "Strict restore must not select another conversation"))))
        (agent-shell--initiate-session-resume-by-id
         :session-id "saved-id" :shell-buffer (current-buffer) :on-session-init #'ignore)
        (should (equal failed "saved-id"))))))

(ert-deftest pi-workflow-c2-unsupported-restore-is-reported-before-session-creation ()
  (with-temp-buffer
    (setq-local +agent-acp-strict-restore t
                agent-shell--state '((:resume-session-id . "saved-id")))
    (let (failed started)
      (cl-letf (((symbol-function '+agent-acp--restore-failed) (lambda (_ id) (setq failed id))))
        (+agent-acp--strict-initiate-session (lambda (&rest _) (setq started t)) :shell-buffer (current-buffer))
        (should (equal failed "saved-id"))
        (should-not started)))))

(provide 'init-pi-workflow-tests)
