;;; beancount-frame.el --- Dedicated Beancount frame -*- lexical-binding: t -*-

;;; Commentary:
;; `my/beancount' opens (or refocuses) a dedicated frame: NAS Fava on the
;; left via xwidget-webkit, the local dashboard (beancount-dashboard.el)
;; on the right.

;;; Code:

(require 'cl-lib)
(require 'beancount-tool)
(require 'beancount-dashboard)
(require 'beancount-entry)

(declare-function my/xwidget-open-url "init-browser")
(declare-function my/xwidget-reload "init-browser")
(declare-function my/xwidget-session-buffer "init-browser" (id))

(defconst my/beancount--fava-xwidget-id "beancount-fava")

(defun my/beancount--existing-frame ()
  "Return the live dedicated Beancount frame, if one exists."
  (cl-find-if (lambda (frame) (frame-parameter frame 'my/beancount-frame))
              (frame-list)))

(defun my/beancount--prepare-fava-window (window)
  "Show the NAS Fava report in WINDOW, reusing any existing xwidget session."
  (unless (fboundp 'my/xwidget-open-url)
    (user-error "xwidget support (init-browser) is not available"))
  (let* ((existing (my/xwidget-session-buffer my/beancount--fava-xwidget-id))
         (other-window (and existing (get-buffer-window existing t))))
    (when (and other-window (not (eq other-window window)))
      (delete-window other-window)))
  (select-window window)
  (my/xwidget-open-url my/beancount-fava-url
                       :id my/beancount--fava-xwidget-id
                       :display 'current
                       :reuse-selected t))

;;;###autoload
(defun my/beancount ()
  "Open (or focus) the dedicated Beancount frame.
Left: the NAS-hosted Fava report.  Right: the local dashboard, backed by
`bean-tool summary --json'."
  (interactive)
  (my/beancount--tool-executable) ; fail fast with a clear message if venv missing
  (let ((frame (my/beancount--existing-frame)))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (my/beancount-dashboard-refresh)
          (my/beancount-completions-refresh))
      (setq frame (make-frame '((name . "Beancount") (my/beancount-frame . t)
                                 (width . 220) (height . 55))))
      (select-frame-set-input-focus frame)
      (delete-other-windows)
      (let* ((left (selected-window))
             (right (split-window left nil 'right)))
        (my/beancount--prepare-fava-window left)
        (select-window right)
        (switch-to-buffer (my/beancount-dashboard-buffer))
        (my/beancount-dashboard-refresh)
        (my/beancount-completions-refresh)
        (select-window right)))))

(defun my/beancount-fava-reload ()
  "Reload the Fava xwidget session shown in the Beancount frame."
  (interactive)
  (let ((buf (my/xwidget-session-buffer my/beancount--fava-xwidget-id)))
    (if buf
        (with-current-buffer buf (my/xwidget-reload))
      (user-error "No Fava xwidget session yet -- run `my/beancount' first"))))

(defun my/beancount-sync ()
  "Run `make sync' in the ledger root and refresh the dashboard when done."
  (interactive)
  (let* ((root (my/beancount--root))
         (buf (get-buffer-create "*beancount-sync*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t)) (erase-buffer)))
    (display-buffer buf)
    (let ((default-directory root))
      (make-process
       :name "beancount-sync"
       :buffer buf
       :command (list "make" "sync")
       :noquery t
       :sentinel
       (lambda (proc _event)
         (unless (process-live-p proc)
           (message "beancount sync: %s"
                    (if (zerop (process-exit-status proc))
                        "done"
                      "failed, see *beancount-sync*"))
           (my/beancount-dashboard-refresh)))))))

(provide 'beancount-frame)
;;; beancount-frame.el ends here
