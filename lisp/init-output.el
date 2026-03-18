;;; init-output.el --- Unified output buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'subr-x)
(require 'transient)

(declare-function compilation-find-buffer "compile")
(declare-function compilation-next-error "compile" (n &optional reset))
(declare-function compilation-previous-error "compile" (n &optional reset))
(declare-function first-error "compile")
(declare-function kill-compilation "compile")
(declare-function recompile "compile" (&optional edit-command))

(defvar compilation-mode-map)
(defvar compilation-scroll-output)

(defgroup my/output nil
  "Unified output helpers for compilation-like buffers."
  :group 'tools)

(defvar my/output-last-buffer nil
  "Most recent compilation-like output buffer.")

(defconst my/output--interesting-buffer-regexp
  "\\*\\(?:compilation\\|test\\|task\\|run\\)\\*"
  "Regexp matching output buffers managed by `my/output'.")

(defun my/output-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a managed output buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'compilation-mode)
         (string-match-p my/output--interesting-buffer-regexp
                         (buffer-name)))))

(defun my/output--buffer ()
  "Return the active output buffer or signal an error."
  (cond
   ((my/output-buffer-p (current-buffer))
    (current-buffer))
   ((buffer-live-p my/output-last-buffer)
    my/output-last-buffer)
   (t
    (user-error "No output buffer is available"))))

(defun my/output--kind (&optional buffer)
  "Return a short kind label for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((string-match-p "\\*test\\*" (buffer-name)) "test")
     ((string-match-p "\\*task\\*" (buffer-name)) "task")
     ((string-match-p "\\*run\\*" (buffer-name)) "run")
     (t "compile"))))

(defun my/output--status (&optional buffer)
  "Return a readable process status string for BUFFER."
  (let ((proc (get-buffer-process (or buffer (current-buffer)))))
    (if (and proc (process-live-p proc))
        "running"
      "idle")))

(defun my/output--header-line ()
  "Return the header line for the current output buffer."
  (format " %s [%s]  %s  |  R rerun  K stop  s auto-scroll  n/p next-prev error"
          (capitalize (my/output--kind))
          (my/output--status)
          (abbreviate-file-name default-directory)))

(defun my/output--setup-buffer ()
  "Track and decorate the current compilation buffer."
  (when (my/output-buffer-p)
    (setq my/output-last-buffer (current-buffer))
    (setq-local header-line-format '(:eval (my/output--header-line)))
    (use-local-map (copy-keymap compilation-mode-map))
    (local-set-key (kbd "R") #'my/output-rerun)
    (local-set-key (kbd "K") #'my/output-stop)
    (local-set-key (kbd "s") #'my/output-toggle-scroll)
    (local-set-key (kbd "n") #'my/output-next-error)
    (local-set-key (kbd "p") #'my/output-previous-error)))

(defun my/output-switch ()
  "Switch to the most recent output buffer."
  (interactive)
  (pop-to-buffer (my/output--buffer)))

(defun my/output-rerun ()
  "Re-run the current or most recent output command."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (recompile)))

(defun my/output-stop ()
  "Stop the current or most recent output process."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (if-let* ((proc (get-buffer-process (current-buffer)))
              ((process-live-p proc)))
        (kill-compilation)
      (user-error "No active output process"))))

(defun my/output-next-error ()
  "Jump to the next error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (compilation-next-error 1)))

(defun my/output-previous-error ()
  "Jump to the previous error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (compilation-previous-error 1)))

(defun my/output-first-error ()
  "Jump to the first error from the current or most recent output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (first-error)))

(defun my/output-toggle-scroll ()
  "Toggle `compilation-scroll-output' for the active output buffer."
  (interactive)
  (with-current-buffer (my/output--buffer)
    (setq-local compilation-scroll-output
                (not (if (local-variable-p 'compilation-scroll-output)
                         compilation-scroll-output
                       nil)))
    (message "Output auto-scroll %s"
             (if compilation-scroll-output "enabled" "disabled"))))

(transient-define-prefix my/output-dispatch ()
  "Unified output workflow."
  [["Output"
    ("o" "latest output" my/output-switch)
    ("r" "rerun" my/output-rerun)
    ("k" "stop" my/output-stop)
    ("n" "next error" my/output-next-error)
    ("p" "previous error" my/output-previous-error)
    ("f" "first error" my/output-first-error)
    ("s" "toggle auto scroll" my/output-toggle-scroll)]])

(add-hook 'compilation-mode-hook #'my/output--setup-buffer)

(my/evil-global-leader-set "r o" #'my/output-dispatch "output")
(my/evil-global-leader-set "r O" #'my/output-switch "latest output")

(provide 'init-output)
;;; init-output.el ends here
