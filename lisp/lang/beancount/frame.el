;;; lang/beancount/frame.el --- Dedicated Beancount frame -*- lexical-binding: t -*-

;;; Commentary:
;; `+beancount/beancount' opens (or refocuses) a dedicated frame: NAS Fava on the
;; left via xwidget-webkit, the local dashboard (lang/beancount/dashboard.el)
;; on the right.

;;; Code:

(require 'cl-lib)
(require 'lang/beancount/tool)
(require 'lang/beancount/dashboard)
(require 'lang/beancount/entry)

(declare-function +xwidget/open-url "app/browser/config")
(declare-function +xwidget/reload "app/browser/config")
(declare-function +xwidget-session-buffer "app/browser/config" (id))

(defconst +beancount--fava-xwidget-id "beancount-fava")

(defun +beancount--existing-frame ()
  "Return the live dedicated Beancount frame, if one exists."
  (cl-find-if (lambda (frame) (frame-parameter frame '+beancount-frame))
              (frame-list)))

(defun +beancount--prepare-fava-window (window)
  "Show the NAS Fava report in WINDOW, reusing any existing xwidget session."
  (unless (fboundp '+xwidget/open-url)
    (user-error "xwidget support (init-browser) is not available"))
  (let* ((existing (+xwidget-session-buffer +beancount--fava-xwidget-id))
         (other-window (and existing (get-buffer-window existing t))))
    (when (and other-window (not (eq other-window window)))
      (delete-window other-window)))
  (select-window window)
  (+xwidget/open-url +beancount-fava-url
                       :id +beancount--fava-xwidget-id
                       :display 'current
                       :reuse-selected t))

;;;###autoload
(defun +beancount/beancount ()
  "Open (or focus) the dedicated Beancount frame.
Left: the NAS-hosted Fava report.  Right: the local dashboard, backed by
`bean-tool summary --json'."
  (interactive)
  (+beancount--tool-executable) ; fail fast with a clear message if venv missing
  (let ((frame (+beancount--existing-frame)))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (+beancount/dashboard-refresh)
          (+beancount/completions-refresh))
      (setq frame (make-frame '((name . "Beancount") (+beancount-frame . t)
                                 (width . 220) (height . 55))))
      (select-frame-set-input-focus frame)
      (delete-other-windows)
      (let* ((left (selected-window))
             (right (split-window left nil 'right)))
        (+beancount--prepare-fava-window left)
        (select-window right)
        (switch-to-buffer (+beancount-dashboard-buffer))
        (+beancount/dashboard-refresh)
        (+beancount/completions-refresh)
        (select-window right)))))

(defun +beancount/fava-reload ()
  "Reload the Fava xwidget session shown in the Beancount frame."
  (interactive)
  (let ((buf (+xwidget-session-buffer +beancount--fava-xwidget-id)))
    (if buf
        (with-current-buffer buf (+xwidget/reload))
      (user-error "No Fava xwidget session yet -- run `+beancount/beancount' first"))))

(defun +beancount/sync ()
  "Run `make sync' in the ledger root and refresh the dashboard when done."
  (interactive)
  (let* ((root (+beancount--root))
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
           (+beancount/dashboard-refresh)))))))

(provide 'lang/beancount/frame)
;;; lang/beancount/frame.el ends here
