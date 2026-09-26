;;; remote-gui-file-redraw.el --- Matched GUI redraw without LSP -*- lexical-binding: t; -*-

;; Set REMOTE_GUI_FILE=/fs:target:/existing/file.py and load this after init.el.
;; The remote file is edited only in memory and is never saved.

(require 'cl-lib)
(require 'remote-config)
(require 'remote-framework)
(require 'seq)

(defconst my/remote-gui-file-redraw--source
  "name = 42\n\ndef main():\n    return name\n\nif __name__ == '__main__':\n    print(main())\n")

(defun my/remote-gui-file-redraw--median (values)
  "Return the median of numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (length (length sorted)))
    (if (cl-oddp length)
        (nth (/ length 2) sorted)
      (/ (+ (nth (1- (/ length 2)) sorted)
            (nth (/ length 2) sorted))
         2.0))))

(defun my/remote-gui-file-redraw--measure (buffer label rounds)
  "Measure GUI redraw in BUFFER for ROUNDS simulated keys under LABEL."
  (switch-to-buffer buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'python-mode 'python-ts-mode)
      (error "Python mode is inactive for %s" label))
    (when (bound-and-true-p lsp-managed-mode)
      (error "LSP unexpectedly managed %s" label))
    (erase-buffer)
    (insert my/remote-gui-file-redraw--source)
    (set-buffer-modified-p nil)
    (font-lock-ensure)
    (redisplay t)
    (goto-char (point-max))
    (let ((begin (point)) edits redraws)
      (unwind-protect
          (progn
            (insert "\n# ")
            (dotimes (index rounds)
              (let ((last-command-event (+ ?a (mod index 26)))
                    (this-command 'self-insert-command)
                    (started (float-time)))
                (run-hooks 'pre-command-hook)
                (self-insert-command 1)
                (run-hooks 'post-command-hook)
                (push (* 1000 (- (float-time) started)) edits)
                (setq started (float-time))
                (redisplay t)
                (push (* 1000 (- (float-time) started)) redraws))
              (accept-process-output nil 0.02))
            (list :label label :rounds rounds
                  :frame-size (cons (frame-width) (frame-height))
                  :window-system window-system
                  :edit-median-ms (my/remote-gui-file-redraw--median edits)
                  :redraw-median-ms
                  (my/remote-gui-file-redraw--median redraws)
                  :redraw-max-ms (apply #'max redraws)))
        (delete-region begin (point-max))
        (set-buffer-modified-p nil)))))

(defun my/remote-gui-file-redraw-run ()
  "Compare native and target-only Python buffers in one GUI process."
  (remote-config-load)
  (remote-fs-install)
  (unless (display-graphic-p)
    (error "This benchmark requires a GUI frame"))
  (let* ((remote-file (or (getenv "REMOTE_GUI_FILE")
                          (error "Set REMOTE_GUI_FILE")))
         (local-directory (make-temp-file "emacs-gui-redraw-" t))
         (local-file (expand-file-name "source.py" local-directory))
         (rounds (max 1 (string-to-number
                         (or (getenv "REMOTE_GUI_REDRAW_ROUNDS") "40"))))
         (columns (max 40 (string-to-number
                           (or (getenv "REMOTE_GUI_FRAME_COLUMNS") "180"))))
         (rows (max 20 (string-to-number
                        (or (getenv "REMOTE_GUI_FRAME_ROWS") "55"))))
         (my/language-server-disabled-modes '(python-mode python-ts-mode))
         local-buffer remote-buffer results)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" local-directory))
          (with-temp-file local-file
            (insert my/remote-gui-file-redraw--source))
          (unless (file-readable-p remote-file)
            (error "Remote file is not readable: %s" remote-file))
          (set-frame-size (selected-frame) columns rows)
          (setq local-buffer (find-file-noselect local-file)
                remote-buffer (find-file-noselect remote-file))
          (dolist (entry `((,local-buffer . native)
                           (,remote-buffer . remote)
                           (,remote-buffer . remote)
                           (,local-buffer . native)))
            (push (my/remote-gui-file-redraw--measure
                   (car entry) (cdr entry) rounds)
                  results))
          (setq results (nreverse results))
          (with-temp-file
              (or (getenv "REMOTE_GUI_REDRAW_RESULT")
                  (error "Set REMOTE_GUI_REDRAW_RESULT"))
            (prin1 results (current-buffer))))
      (dolist (buffer (list local-buffer remote-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory local-directory t))))

(provide 'remote-gui-file-redraw)
;;; remote-gui-file-redraw.el ends here
