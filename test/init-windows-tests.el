;;; init-windows-tests.el --- Window and buffer fallback tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-windows-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'init-windows)

(ert-deftest my/window-quit-temporary-buffer-restores-xwidget ()
  "The buffer-close shortcut must honor a popup's exact return target."
  (save-window-excursion
    (let ((source (generate-new-buffer " *quit-source*"))
          (output (generate-new-buffer " *quit-output*"))
          (picker (generate-new-buffer "*quit-skill-picker*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer source)
            (with-current-buffer output
              (setq-local major-mode 'xwidget-webkit-mode))
            (with-current-buffer picker (special-mode))
            (let* ((left (selected-window))
                   (right (split-window-right))
                   (edges (window-edges right)))
              (set-window-buffer right output)
              (select-window right)
              (pop-to-buffer picker '(display-buffer-same-window))
              (should (or (eq (nth 3 (window-parameter right 'quit-restore)) picker)
                          (eq (nth 3 (window-parameter right 'quit-restore-prev)) picker)))
              (my/kill-buffer-dwim)
              (should (window-live-p right))
              (should (eq (window-buffer right) output))
              (should (eq (window-buffer left) source))
              (should (equal (window-edges right) edges))))
        (dolist (buffer (list source output picker))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest my/window-ibuffer-q-returns-to-previous-page ()
  "The actual Ibuffer q binding should recover the page it replaced."
  (let ((source (generate-new-buffer "ibuffer-return-source")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (ibuffer)
          (call-interactively (key-binding (kbd "q")))
          (should (eq (window-buffer) source)))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest my/window-close-last-page-opens-dashboard ()
  "A sole window with no previous work should show the dashboard."
  (let ((page (generate-new-buffer "close-last-page"))
        (scratch (get-buffer-create "*scratch*"))
        dashboard)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer scratch)
          (switch-to-buffer page)
          (set-window-prev-buffers (selected-window)
                                   (list (list scratch 1 1)))
          (cl-letf (((symbol-function 'dashboard-open)
                     (lambda (&rest _)
                       (setq dashboard
                             (get-buffer-create "*dashboard-close-test*"))
                       (with-current-buffer dashboard
                         (setq-local major-mode 'dashboard-mode))
                       (switch-to-buffer dashboard))))
            (kill-current-buffer)
            (sit-for 0.02))
          (should (eq (window-buffer) dashboard)))
      (dolist (buffer (list page dashboard))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-bury-last-page-opens-dashboard ()
  "The one-frame close action buries its page without showing scratch."
  (let ((page (generate-new-buffer "bury-last-page"))
        (scratch (get-buffer-create "*scratch*"))
        dashboard)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer scratch)
          (switch-to-buffer page)
          (set-window-prev-buffers (selected-window)
                                   (list (list scratch 1 1)))
          (cl-letf (((symbol-function 'dashboard-open)
                     (lambda (&rest _)
                       (setq dashboard
                             (get-buffer-create "*dashboard-bury-test*"))
                       (with-current-buffer dashboard
                         (setq-local major-mode 'dashboard-mode))
                       (switch-to-buffer dashboard))))
            (my/delete-frame-dwim))
          (should (buffer-live-p page))
          (should (eq (window-buffer) dashboard)))
      (dolist (buffer (list page dashboard))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-bury-returns-to-previous-page ()
  "Burying keeps the old buffer alive and restores the prior page."
  (let ((source (generate-new-buffer "bury-return-source"))
        (page (generate-new-buffer "bury-return-page")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (switch-to-buffer page)
          (my/window-bury-current-buffer)
          (should (buffer-live-p page))
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source page))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-close-select-key-keeps-picker-and-window-history ()
  "Cmd+Shift+W asks which buffer to kill, then restores its window."
  (let ((source (generate-new-buffer "close-key-source"))
        (page (generate-new-buffer "close-key-page"))
        prompted)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (switch-to-buffer page)
          (should (eq (key-binding (kbd "M-W")) #'persp-kill-buffer*))
          (should (eq (key-binding (kbd "C-x k")) #'persp-kill-buffer*))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       (setq prompted t)
                       (buffer-name page))))
            (call-interactively (key-binding (kbd "M-W"))))
          (should prompted)
          (should-not (buffer-live-p page))
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source page))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-close-picker-can-kill-another-buffer ()
  "The original Cmd+Shift+W picker can kill a different buffer."
  (let ((page (generate-new-buffer "close-picker-current"))
        (other (generate-new-buffer "close-picker-other"))
        prompted)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer other)
          (switch-to-buffer page)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       (setq prompted t)
                       (buffer-name other))))
            (call-interactively (key-binding (kbd "M-W"))))
          (should prompted)
          (should-not (buffer-live-p other))
          (should (eq (window-buffer) page)))
      (dolist (buffer (list page other))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-xwidget-after-kill ()
  (let ((normal (generate-new-buffer "normal-fallback-test"))
        (xwidget (generate-new-buffer "xwidget-fallback-test"))
        (victim (generate-new-buffer "victim-fallback-test")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer xwidget
            (setq-local major-mode 'xwidget-webkit-mode))
          ;; Make xwidget the most recent fallback, followed by NORMAL.
          (switch-to-buffer normal)
          (switch-to-buffer xwidget)
          (switch-to-buffer victim)
          (kill-buffer victim)
          (should (eq (window-buffer) normal)))
      (dolist (buffer (list normal xwidget victim))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-keeps-xwidget-for-explicit-navigation ()
  (let ((xwidget (generate-new-buffer "xwidget-navigation-test")))
    (unwind-protect
        (with-current-buffer xwidget
          (setq-local major-mode 'xwidget-webkit-mode)
          (should-not
           (my/window-skip-dashboard-and-xwidget-p nil xwidget nil)))
      (when (buffer-live-p xwidget)
        (kill-buffer xwidget)))))

(ert-deftest my/window-repair-candidate-respects-native-frame-predicate ()
  "Fallback repair must not resurrect a buffer excluded by this frame."
  (let ((closing (generate-new-buffer "repair-predicate-closing"))
        (foreign (generate-new-buffer "repair-predicate-foreign"))
        (allowed (generate-new-buffer "repair-predicate-allowed"))
        (frame (selected-frame))
        (original (frame-parameter nil 'buffer-predicate)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer closing)
          (set-window-prev-buffers
           (selected-window)
           (list (list foreign 1 1) (list allowed 1 1)))
          (set-frame-parameter
           frame 'buffer-predicate
           (lambda (buffer) (not (eq buffer foreign))))
          (should (eq (my/window--return-buffer
                       (selected-window) closing)
                      allowed)))
      (set-frame-parameter frame 'buffer-predicate original)
      (dolist (buffer (list closing foreign allowed))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-repair-candidate-skips-xwidget-history ()
  "Only the native exact popup return may restore a Noema xwidget page."
  (let ((closing (generate-new-buffer "repair-xwidget-closing"))
        (xwidget (generate-new-buffer "repair-xwidget-page"))
        (allowed (generate-new-buffer "repair-xwidget-allowed")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer xwidget
            (setq-local major-mode 'xwidget-webkit-mode))
          (switch-to-buffer closing)
          (set-window-prev-buffers
           (selected-window)
           (list (list xwidget 1 1) (list allowed 1 1)))
          (should (eq (my/window--return-buffer
                       (selected-window) closing)
                      allowed)))
      (dolist (buffer (list closing xwidget allowed))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-dashboard-on-quit ()
  "Quitting a popup must not resurrect the startup dashboard."
  (let ((source (generate-new-buffer "dashboard-fallback-source"))
        (dashboard (generate-new-buffer "*dashboard-fallback-test*"))
        (popup (generate-new-buffer "*dashboard-fallback-popup*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer source)
          (switch-to-buffer dashboard)
          (switch-to-buffer popup)
          (quit-window)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard popup))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-dashboard-on-kill ()
  "Killing a terminal buffer must restore useful work, not dashboard."
  (let ((source (generate-new-buffer "dashboard-kill-source"))
        (dashboard (generate-new-buffer "*dashboard-kill-test*"))
        (terminal (generate-new-buffer "*dashboard-kill-terminal*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer source)
          (switch-to-buffer dashboard)
          (switch-to-buffer terminal)
          (kill-buffer terminal)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard terminal))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-keeps-dashboard-for-explicit-navigation ()
  "Explicit previous-buffer navigation can still reach the dashboard."
  (let ((dashboard (generate-new-buffer "*dashboard-navigation-test*")))
    (unwind-protect
        (with-current-buffer dashboard
          (setq-local major-mode 'dashboard-mode)
          (should-not
           (my/window-skip-dashboard-and-xwidget-p nil dashboard nil)))
      (when (buffer-live-p dashboard)
        (kill-buffer dashboard)))))

(ert-deftest my/window-ghostel-popup-exit-keeps-work-buffer ()
  "Closing a managed ghostel popup leaves its work window in place."
  (require 'init-ghostel-popup)
  (let ((source (generate-new-buffer "ghostel-popup-work-test"))
        (dashboard (generate-new-buffer "*ghostel-popup-dashboard-test*"))
        (popup (generate-new-buffer "*ghostel-popup-exit-test*"))
        (my/ghostel-popup-buffers nil)
        (my/ghostel-popup-current-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer dashboard)
          (switch-to-buffer source)
          (my/ghostel-popup-display-buffer popup)
          (should (eq (window-buffer) popup))
          (kill-buffer popup)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard popup))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'init-windows-tests)

;;; init-windows-tests.el ends here
