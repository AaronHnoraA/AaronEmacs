;;; org-latex-preview-test-harness.el --- Test harness for Org LaTeX preview -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-element)
(require 'ox-latex)

(defconst my/org-latex-test-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root used by the Org LaTeX preview tests.")

(defmacro use-package (&rest _args)
  "Stub `use-package' for isolated batch tests."
  nil)

(unless (fboundp 'my/open-file)
  (defun my/open-file (&rest _args)
    "Stub `my/open-file' used by `init-org-latex'."
    nil))

(unless (fboundp 'my/shell-command-executable)
  (defun my/shell-command-executable (command)
    "Return COMMAND unchanged in the test harness."
    command))

(unless (fboundp 'my/org-special-block-background-at-point)
  (defun my/org-special-block-background-at-point (&optional _pos)
    "Return nil for special-block background lookups in tests."
    nil))

(unless (fboundp 'my/org--unspecified-color-p)
  (defun my/org--unspecified-color-p (value)
    "Return non-nil when VALUE is an unspecified face color."
    (or (null value)
        (eq value 'unspecified)
        (and (stringp value)
             (string-prefix-p "unspecified" value)))))

(provide 'init-open)
(provide 'init-org-core)
(provide 'init-org-ui)

(unless (featurep 'init-org-latex)
  (let ((user-emacs-directory (file-name-as-directory my/org-latex-test-root)))
    (load-file (expand-file-name "lisp/org/init-org-latex.el" my/org-latex-test-root))))

;; Tests drive the preview engine directly and do not need background hooks.
(remove-hook 'org-mode-hook #'my/org-latex-enable-scroll-preview)
(remove-hook 'org-mode-hook #'my/org-enable-org-fragtog-maybe)
(remove-hook 'org-mode-hook #'org-cdlatex-mode)

(defun my/org-latex-test--preview-overlays (&optional beg end)
  "Return Org LaTeX preview overlays between BEG and END."
  (cl-remove-if-not
   (lambda (overlay)
     (eq (overlay-get overlay 'org-overlay-type) 'org-latex-overlay))
   (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun my/org-latex-test--write-file (file contents)
  "Write CONTENTS to FILE, creating parent directories when needed."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert contents))
  file)

(defun my/org-latex-test--wait-for-renders (&optional timeout)
  "Wait until all preview render processes finish or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 30.0))))
    (while (and my/org-latex--render-processes
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (when my/org-latex--render-processes
      (ert-fail
       (format "Timed out waiting for preview renders: %S"
               my/org-latex--render-processes)))))

(defmacro my/org-latex-test-with-org-buffer (content &rest body)
  "Evaluate BODY in a temporary Org buffer initialized with CONTENT."
  (declare (indent 1) (debug t))
  `(let* ((temp-dir (make-temp-file "org-latex-preview-test" t))
          (buffer (generate-new-buffer " *org-latex-preview-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (setq-local default-directory (file-name-as-directory temp-dir))
           (insert ,content)
           (org-mode)
           (my/org-latex--ensure-state)
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (ignore-errors
             (my/org-latex-cancel-pending-renders)
             (my/org-latex--clear-preview-range (point-min) (point-max))))
         (kill-buffer buffer))
       (ignore-errors (delete-directory temp-dir t)))))

(provide 'org-latex-preview-test-harness)

;;; org-latex-preview-test-harness.el ends here
