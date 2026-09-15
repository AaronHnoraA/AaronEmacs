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
           (my/window-skip-xwidget-fallback-p nil xwidget nil)))
      (when (buffer-live-p xwidget)
        (kill-buffer xwidget)))))

(provide 'init-windows-tests)

;;; init-windows-tests.el ends here
