;;; org-latex-preview-tests.el --- Unit tests for Org LaTeX preview -*- lexical-binding: t; -*-

;;; Code:

(require 'org-latex-preview-test-harness)

(ert-deftest my/org-latex-preview-clear-range-removes-indexed-and-stray-overlays ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((file-a (expand-file-name "a.svg" default-directory))
          (file-b (expand-file-name "b.svg" default-directory)))
      (my/org-latex-test--write-file file-a "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (my/org-latex-test--write-file file-b "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (my/org-latex--make-preview-overlay 1 6 file-a "svg")
      (org--make-preview-overlay 1 6 file-b "svg")
      (should (= (length (my/org-latex-test--preview-overlays 1 6)) 2))
      (should (= (hash-table-count my/org-latex--overlay-table) 1))
      (let ((removed (my/org-latex--clear-preview-range 1 6)))
        (should (= (length removed) 2))
        (should-not (my/org-latex--lookup-overlay 1 6))
        (should-not (my/org-latex-test--preview-overlays 1 6))
        (should (= (hash-table-count my/org-latex--overlay-table) 0))))))

(ert-deftest my/org-latex-preview-place-preview-reuses-overlay ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((file (expand-file-name "cached.svg" default-directory)))
      (my/org-latex-test--write-file file "<svg xmlns='http://www.w3.org/2000/svg'/>")
      (goto-char (point-max))
      (my/org-latex--place-preview 1 6 "\\(x\\)" file "svg")
      (let ((first (my/org-latex--lookup-overlay 1 6)))
        (should (overlayp first))
        (should (= (length (my/org-latex-test--preview-overlays 1 6)) 1))
        (my/org-latex--place-preview 1 6 "\\(x\\)" file "svg")
        (should (eq first (my/org-latex--lookup-overlay 1 6)))
        (should (= (length (my/org-latex-test--preview-overlays 1 6)) 1))
        (should (equal (overlay-get first 'my/org-latex-file) file))))))

(ert-deftest my/org-fragtog-disable-frag-preserves-point ()
  (my/org-latex-test-with-org-buffer "\\[\nas\n\\]"
    (goto-char 5)
    (let ((origin (point))
          called)
      (my/org-fragtog-disable-frag-preserve-point-a
       (lambda (_frag _renew)
         (setq called t)
         (goto-char (point-max)))
       nil nil)
      (should called)
      (should (= (point) origin)))))

(ert-deftest my/org-latex-preview-enqueue-fragment-deduplicates-waiters ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let* ((target (expand-file-name "ltximg/shared.svg" default-directory))
           (spec (list :beg 1
                       :end 6
                       :value "\\(x\\)"
                       :render-value "\\(x\\)"
                       :dir default-directory
                       :file target
                       :imagetype "svg"
                       :options '(:scale 1.0)
                       :background nil
                       :processing-type 'xdvisvgm-hires-script)))
      (cl-letf (((symbol-function 'my/org-latex--pump-render-queue) #'ignore))
        (my/org-latex--enqueue-fragment spec)
        (my/org-latex--enqueue-fragment spec))
      (should (= (hash-table-count my/org-latex--pending-renders) 1))
      (let ((job (gethash target my/org-latex--pending-renders)))
        (should job)
        (should (= (length (plist-get job :waiters)) 1))
        (should (= (hash-table-count (plist-get job :waiter-index)) 1))
        (should (equal my/org-latex--render-queue (list job)))
        (should (eq (car my/org-latex--render-queue-tail) job))))))

(ert-deftest my/org-latex-preview-render-sentinel-ignores-stale-generation ()
  (my/org-latex-test-with-org-buffer ""
    (let* ((log-buffer (generate-new-buffer " *org-latex-stale-sentinel*"))
           (process (make-process :name "org-latex-stale-sentinel"
                                  :buffer log-buffer
                                  :command (list shell-file-name shell-command-switch "exit 0")
                                  :noquery t))
           (target (expand-file-name "ltximg/shared.svg" default-directory))
           (old-job (list :file target
                          :image-output-file (expand-file-name "tmp/stale-output.svg"
                                                               default-directory)
                          :generation 1
                          :waiters (list (list :beg (copy-marker (point-min))
                                               :end (copy-marker (point-min))
                                               :value "\\(x\\)"))
                          :post-clean nil)))
      (while (eq (process-status process) 'run)
        (accept-process-output process 0.05))
      (setq-local my/org-latex--render-generation 2)
      (setq-local my/org-latex--render-running 7)
      (setq-local my/org-latex--render-processes nil)
      (puthash target :new-job my/org-latex--pending-renders)
      (process-put process 'my/org-latex-buffer (current-buffer))
      (process-put process 'my/org-latex-job old-job)
      (my/org-latex--render-sentinel process "finished\n")
      (should (= my/org-latex--render-running 7))
      (should (eq (gethash target my/org-latex--pending-renders) :new-job))
      (kill-buffer log-buffer))))

(ert-deftest my/org-latex-preview-start-render-refreshes-pending-prepared-job ()
  (my/org-latex-test-with-org-buffer ""
    (let* ((target (expand-file-name "ltximg/prepared.svg" default-directory))
           (job (list :dir default-directory
                      :file target
                      :generation my/org-latex--render-generation
                      :waiters nil
                      :post-clean nil)))
      (puthash target job my/org-latex--pending-renders)
      (cl-letf (((symbol-function 'my/org-latex--prepare-render)
                 (lambda (queued-job)
                   (append (list :command "sleep 30") queued-job))))
        (unwind-protect
            (progn
              (my/org-latex--start-render job)
              (let* ((process (car my/org-latex--render-processes))
                     (prepared-job (process-get process 'my/org-latex-job)))
                (should (process-live-p process))
                (should (eq (gethash target my/org-latex--pending-renders)
                            prepared-job))
                (should-not (eq prepared-job job))))
          (dolist (process my/org-latex--render-processes)
            (when (process-live-p process)
              (process-put process 'my/org-latex-cancelled t)
              (delete-process process))))))))

(ert-deftest my/org-latex-preview-scroll-hook-ignores-ratex-suppression ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((window (display-buffer (current-buffer) '(display-buffer-same-window)))
          scheduled)
      (unwind-protect
          (let ((ratex--suppress-scroll-side-effects t))
            (cl-letf (((symbol-function 'my/org-latex-preview-visible-debounced)
                       (lambda (&optional _window)
                         (setq scheduled t))))
              (my/org-latex--window-scroll-preview-hook window (window-start window))
              (should-not scheduled)))
        (delete-other-windows)))))

(ert-deftest my/org-latex-preview-scroll-hook-uses-window-buffer ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((org-buffer (current-buffer))
          (window (display-buffer (current-buffer) '(display-buffer-same-window)))
          scheduled)
      (unwind-protect
          (with-temp-buffer
            (cl-letf (((symbol-function 'my/org-latex-preview-visible-debounced)
                       (lambda (&optional target-window)
                         (setq scheduled
                               (and (eq target-window window)
                                    (eq (current-buffer) org-buffer))))))
              (my/org-latex--window-scroll-preview-hook window
                                                        (window-start window))
              (should scheduled)))
        (delete-other-windows)))))

(ert-deftest my/org-latex-preview-visible-debounced-skips-active-ratex-edit-session ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((window (display-buffer (current-buffer) '(display-buffer-same-window)))
          scheduled)
      (unwind-protect
          (progn
            (setq-local ratex-mode t)
            (setq-local ratex--active-fragment '(:begin 1 :end 6 :content "x"))
            (cl-letf (((symbol-function 'my/org-latex--async-preview-active-p)
                       (lambda () t))
                      ((symbol-function 'my/org-latex--buffer-visible-p)
                       (lambda (&optional _buffer) t))
                      ((symbol-function 'my/org-latex--visible-range)
                       (lambda (&optional _window) (cons 1 6)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (setq scheduled t)
                         'fake-timer)))
              (my/org-latex-preview-visible-debounced window)
              (should-not scheduled)))
        (delete-other-windows)))))

(ert-deftest my/org-latex-preview-visible-debounced-skips-edit-preview-timer ()
  (my/org-latex-test-with-org-buffer "\\(x\\)"
    (let ((window (display-buffer (current-buffer) '(display-buffer-same-window)))
          (edit-timer (run-at-time 60 nil #'ignore))
          scheduled)
      (unwind-protect
          (progn
            (setq-local my/org-latex--edit-preview-timer edit-timer)
            (cl-letf (((symbol-function 'my/org-latex--async-preview-active-p)
                       (lambda () t))
                      ((symbol-function 'my/org-latex--buffer-visible-p)
                       (lambda (&optional _buffer) t))
                      ((symbol-function 'my/org-latex--visible-range)
                       (lambda (&optional _window) (cons 1 6)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (setq scheduled t)
                         'fake-timer)))
              (my/org-latex-preview-visible-debounced window)
              (should-not scheduled)))
        (when (timerp edit-timer)
          (cancel-timer edit-timer))
        (delete-other-windows)))))

(ert-deftest my/org-latex-preview-visible-slots-cancel-offscreen-prefetch ()
  (my/org-latex-test-with-org-buffer "\\(old\\)\n\n\\(new\\)"
    (let* ((old-waiter (list :beg (copy-marker 1)
                             :end (copy-marker 8 t)
                             :value "\\(old\\)"
                             :origin 'prefetch))
           (old-job (list :file (expand-file-name "ltximg/old.svg"
                                                  default-directory)
                          :origin 'prefetch
                          :prunable t
                          :waiters (list old-waiter)))
           (new-spec (list :file (expand-file-name "ltximg/new.svg"
                                                   default-directory)))
           (process (make-process :name "org-latex-prefetch-test"
                                  :command (list shell-file-name
                                                 shell-command-switch
                                                 "sleep 30")
                                  :noquery t)))
      (unwind-protect
          (progn
            (process-put process 'my/org-latex-job old-job)
            (setq-local my/org-latex--render-processes (list process))
            (setq-local my/org-latex--render-running 1)
            (let ((my/org-latex-preview-max-concurrency 1))
              (my/org-latex--ensure-visible-render-slots
               (list new-spec) 10 17))
            (should (= my/org-latex--render-running 0))
            (should-not (process-live-p process)))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest my/org-latex-preview-visible-slots-preserve-edit-precompile ()
  (my/org-latex-test-with-org-buffer "\\(old\\)\n\n\\(new\\)"
    (let* ((edit-job (list :file (expand-file-name "ltximg/edit.svg"
                                                   default-directory)
                           :origin 'preedit
                           :prunable nil
                           :waiters nil))
           (new-spec (list :file (expand-file-name "ltximg/new.svg"
                                                   default-directory)))
           (process (make-process :name "org-latex-preedit-test"
                                  :command (list shell-file-name
                                                 shell-command-switch
                                                 "sleep 30")
                                  :noquery t)))
      (unwind-protect
          (progn
            (process-put process 'my/org-latex-job edit-job)
            (setq-local my/org-latex--render-processes (list process))
            (setq-local my/org-latex--render-running 1)
            (let ((my/org-latex-preview-max-concurrency 1))
              (my/org-latex--ensure-visible-render-slots
               (list new-spec) 10 17))
            (should (= my/org-latex--render-running 1))
            (should (process-live-p process)))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest my/org-latex-preview-place-preview-preserves-window-state ()
  (my/org-latex-test-with-org-buffer "\\(x\\)\n\n\nline-4\nline-5\nline-6\nline-7\nline-8\nline-9\nline-10\n"
    (let ((file (expand-file-name "stable.svg" default-directory))
          (window (display-buffer (current-buffer) '(display-buffer-same-window))))
      (unwind-protect
          (progn
            (my/org-latex-test--write-file file "<svg xmlns='http://www.w3.org/2000/svg'/>")
            (goto-char (point-max))
            (set-window-start window (point-min))
            (set-window-point window (point-min))
            (let ((before-start (window-start window))
                  (before-point (window-point window))
                  (orig (symbol-function 'my/org-latex--make-preview-overlay)))
              (cl-letf (((symbol-function 'my/org-latex--make-preview-overlay)
                         (lambda (beg end overlay-file imagetype)
                           (set-window-start window (point-max) t)
                           (set-window-point window (point-max))
                           (funcall orig beg end overlay-file imagetype))))
                (my/org-latex--place-preview 1 6 "\\(x\\)" file "svg")
                (should (= (window-start window) before-start))
                (should (= (window-point window) before-point)))))
        (delete-other-windows)))))

(ert-deftest my/org-latex-preview-clear-range-preserves-window-state ()
  (my/org-latex-test-with-org-buffer "\\(x\\)\n\n\nline-4\nline-5\nline-6\nline-7\nline-8\nline-9\nline-10\n"
    (let ((file (expand-file-name "stable-clear.svg" default-directory))
          (window (display-buffer (current-buffer) '(display-buffer-same-window))))
      (unwind-protect
          (progn
            (my/org-latex-test--write-file file "<svg xmlns='http://www.w3.org/2000/svg'/>")
            (my/org-latex--make-preview-overlay 1 6 file "svg")
            (set-window-start window (point-min))
            (set-window-point window (point-min))
            (let ((before-start (window-start window))
                  (before-point (window-point window))
                  (orig (symbol-function 'my/org-latex--delete-overlay)))
              (cl-letf (((symbol-function 'my/org-latex--delete-overlay)
                         (lambda (overlay)
                           (set-window-start window (point-max) t)
                           (set-window-point window (point-max))
                           (funcall orig overlay))))
                (my/org-latex--clear-preview-range 1 6)
                (should (= (window-start window) before-start))
                (should (= (window-point window) before-point)))))
        (delete-other-windows)))))

(provide 'org-latex-preview-tests)

;;; org-latex-preview-tests.el ends here
