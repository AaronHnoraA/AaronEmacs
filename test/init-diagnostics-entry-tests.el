;;; init-diagnostics-entry-tests.el --- Diagnostics entry tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-diagnostics-ui)
(require 'init-diagnostics-extra)
(require 'init-problems)

(ert-deftest diagnostics-buffer-entries-reuse-one-frame-panel ()
  "Severity entries change one panel, and a new source resets its filter."
  (let ((source-a (generate-new-buffer " *diagnostics-source-a*"))
        (source-b (generate-new-buffer " *diagnostics-source-b*"))
        panel)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source-a)
          (cl-letf (((symbol-function 'my/diagnostics-refresh) #'ignore))
            (my/diagnostics-buffer-errors)
            (setq panel (current-buffer))
            (should (eq my/diagnostics-buffer-filter :error))
            (should (eq my/diagnostics-origin-buffer source-a))
            (should (my/diagnostics--open-buffer-p))
            (quit-window)
            (should (eq (current-buffer) source-a))
            (should-not (my/diagnostics--open-buffer-p))
            (my/diagnostics-buffer-warnings)
            (should (eq (current-buffer) panel))
            (should (eq my/diagnostics-buffer-filter :warning))
            ;; Re-entering from the panel preserves its source and view.
            (my/diagnostics-buffer-ui)
            (should (eq my/diagnostics-origin-buffer source-a))
            (should (eq my/diagnostics-buffer-filter :warning))
            (quit-window)
            (switch-to-buffer source-b)
            (my/diagnostics-buffer-ui)
            (should (eq (current-buffer) panel))
            (should (eq my/diagnostics-origin-buffer source-b))
            (should-not my/diagnostics-buffer-filter)))
      (when (buffer-live-p panel) (kill-buffer panel))
      (when (buffer-live-p source-a) (kill-buffer source-a))
      (when (buffer-live-p source-b) (kill-buffer source-b)))))

(ert-deftest diagnostics-auto-refresh-follows-visible-source-and-position ()
  "Only the shown source schedules work, and a refresh keeps the reading row."
  (let ((source (generate-new-buffer " *diagnostics-refresh-source*"))
        (other (generate-new-buffer " *diagnostics-refresh-other*"))
        panel)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'my/diagnostics-refresh)
                     (lambda ()
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (dotimes (index 40)
                           (insert (format "Diagnostic %02d\n" index)))
                         (goto-char (point-min))))))
            (my/diagnostics-buffer-ui)
            (setq panel (current-buffer))
            (forward-line 20)
            (let ((before (window-point)))
              (with-current-buffer other
                (my/diagnostics--schedule-refresh-open-buffers))
              (should-not my/diagnostics--refresh-timer)
              (with-current-buffer source
                (my/diagnostics--schedule-refresh-open-buffers))
              (should (timerp my/diagnostics--refresh-timer))
              (should (equal my/diagnostics--pending-refresh-sources
                             (list source)))
              (my/diagnostics--refresh-open-buffers (list source))
              (should (= (window-point) before)))
            (quit-window)
            (with-current-buffer source
              (my/diagnostics--schedule-refresh-open-buffers))
            (should-not (my/diagnostics--open-buffer-p source))))
      (when (timerp my/diagnostics--refresh-timer)
        (cancel-timer my/diagnostics--refresh-timer))
      (setq my/diagnostics--refresh-timer nil
            my/diagnostics--pending-refresh-sources nil)
      (when (buffer-live-p panel) (kill-buffer panel))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest diagnostics-panel-closes-when-its-source-is-killed ()
  "A buffer-specific panel cannot outlive the buffer it describes."
  (let ((source (generate-new-buffer " *diagnostics-dying-source*"))
        panel)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'my/diagnostics-refresh) #'ignore))
            (my/diagnostics-buffer-ui)
            (setq panel (current-buffer))
            (kill-buffer source)
            (should-not (buffer-live-p panel))))
      (when (buffer-live-p panel) (kill-buffer panel))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest diagnostics-panel-releases-its-frame-buffer-on-close ()
  "The frame-owned panel does not survive the frame deletion hook."
  (let ((source (generate-new-buffer " *diagnostics-frame-source*"))
        panel)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'my/diagnostics-refresh) #'ignore))
            (my/diagnostics-buffer-ui)
            (setq panel (current-buffer))
            (should (eq my/diagnostics-owner-frame (selected-frame)))
            (my/diagnostics--frame-deleted (selected-frame))
            (should-not (buffer-live-p panel))))
      (when (buffer-live-p panel) (kill-buffer panel))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest diagnostics-project-entry-keeps-project-buffers-distinct ()
  "Opening project B must not close project A's native Flymake listing."
  (let ((a (get-buffer-create " *diagnostics-project-A*"))
        (b (get-buffer-create " *diagnostics-project-B*"))
        (current-project 'b)
        opened)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer a)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) current-project))
                    ((symbol-function 'project-root)
                     (lambda (project) (symbol-name project)))
                    ((symbol-function 'flymake--project-diagnostics-buffer)
                     (lambda (root) (if (equal root "a") a b)))
                    ((symbol-function 'my/problems-project-full)
                     (lambda () (push current-project opened))))
            (my/diagnostics-project-ui)
            (should (equal opened '(b)))
            (should (get-buffer-window a))
            (setq current-project 'a)
            (my/diagnostics-project-ui)
            (should (equal opened '(b)))
            (should-not (get-buffer-window a))
            (setq current-project nil)
            (should-error (my/diagnostics-project-ui) :type 'user-error)))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b)))))

(ert-deftest diagnostics-project-bridge-excludes-sibling-roots ()
  "LSP diagnostics under a similarly named sibling are not project data."
  (let* ((root "/tmp/emacs-diagnostics-project")
         (inside (concat root "/main.el"))
         (outside (concat root "-other/main.el"))
         (table (make-hash-table :test 'equal))
         (flymake-list-only-diagnostics nil)
         (my/problems--lsp-list-only-owned nil))
    (puthash inside (list '(:message "inside")) table)
    (puthash outside (list '(:message "outside")) table)
    (cl-letf (((symbol-function 'lsp-diagnostics)
               (lambda (&rest _) table))
              ((symbol-function 'remote-canonicalize-file-name)
               (lambda (file &rest _) file))
              ((symbol-function 'remote-client-file-name)
               (lambda (file &rest _) file)))
      (my/problems--sync-lsp-list-only-diagnostics root))
    (should (assoc inside flymake-list-only-diagnostics))
    (should-not (assoc outside flymake-list-only-diagnostics))))

(ert-deftest diagnostics-project-bridge-separates-local-and-remote-machines ()
  "A native local diagnostic cannot enter a remote project by path spelling."
  (let* ((root "/fs:box:/work/project/")
         (inside "/fs:box:/work/project/main.el")
         (local "/work/project/main.el")
         (other "/fs:other:/work/project/main.el")
         (table (make-hash-table :test 'equal))
         (default-directory root)
         (flymake-list-only-diagnostics nil)
         (my/problems--lsp-list-only-owned nil))
    (dolist (file (list inside local other))
      (puthash file (list '(:message "diagnostic")) table))
    (cl-letf (((symbol-function 'lsp-diagnostics)
               (lambda (&rest _) table))
              ((symbol-function 'my/problems--lsp-diagnostic-to-flymake)
               (lambda (&rest _) 'diagnostic)))
      (my/problems--sync-lsp-list-only-diagnostics root))
    (should (assoc inside flymake-list-only-diagnostics))
    (should-not (assoc local flymake-list-only-diagnostics))
    (should-not (assoc other flymake-list-only-diagnostics))))

(ert-deftest diagnostics-location-opens-local-file-from-remote-panel ()
  (let ((panel (generate-new-buffer " *remote-diagnostics-panel*"))
        (destination (generate-new-buffer " *local-diagnostics-file*"))
        opened visit-directory)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer panel
            (setq default-directory "/fs:box:/work/"))
          (switch-to-buffer panel)
          (cl-letf (((symbol-function 'find-file)
                     (lambda (file &rest _)
                       (setq opened file visit-directory default-directory)
                       (switch-to-buffer destination))))
            (my/diagnostics--goto-location
             "/Users/test/project/main.el" nil 1))
          (should (equal opened "/Users/test/project/main.el"))
          (should (equal visit-directory temporary-file-directory))
          (should (equal (buffer-local-value 'default-directory panel)
                         "/fs:box:/work/")))
      (kill-buffer panel)
      (kill-buffer destination))))

(provide 'init-diagnostics-entry-tests)
;;; init-diagnostics-entry-tests.el ends here
