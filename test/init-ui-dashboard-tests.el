;;; init-ui-dashboard-tests.el --- Startup dashboard Agenda card tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-ui-dashboard-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ui/theme/config)

(defvar +dashboard-test-read-eval nil)

;; Never touch the real session cache from tests.
(setq +dashboard-agenda-cache-file
      (make-temp-file "dashboard-agenda-test-" nil ".eld"))

(ert-deftest +dashboard-config-button-opens-native-customize ()
  "The visible config navigator action keeps the native Customize route."
  (let ((button (seq-find (lambda (item) (equal (nth 1 item) "config"))
                          (car dashboard-navigator-buttons)))
        (opened nil))
    (should button)
    (should (eq (nth 3 button) #'+dashboard/open-config-board))
    (cl-letf (((symbol-function 'config-board)
               (lambda () (setq opened t))))
      (funcall (nth 3 button) nil)
      (should opened))))

(ert-deftest +dashboard-documentation-button-routes-to-docs ()
  "Dashboard exposes Docs, with agent questions inside the reader."
  (let* ((items (car dashboard-navigator-buttons))
         (docs (seq-find (lambda (item) (equal (nth 1 item) "docs")) items))
         opened)
    (should (eq (nth 3 docs) #'+dashboard/open-docs))
    (should-not (seq-find (lambda (item) (equal (nth 1 item) "ask")) items))
    (cl-letf (((symbol-function 'aaron-ui-docs-home)
               (lambda () (push 'docs opened))))
      (funcall (nth 3 docs) nil)
      (should (equal opened '(docs))))))

(ert-deftest +dashboard-navigator-centers-visible-glyphs ()
  "The late pass uses actual glyph positions without scrolling the window."
  (with-temp-buffer
    (insert "[config] [docs]\n")
    (setq-local +dashboard--navigator-region
                (cons (copy-marker (point-min)) (copy-marker (point-max) t)))
    ;; Dashboard enters its major mode after inserting the navigator.
    (dashboard-mode)
    (should +dashboard--navigator-region)
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (let (widths)
        (cl-letf (((symbol-function 'window-text-pixel-size)
                   (lambda (&rest _) (ert-fail "Visible glyph positions should win")))
                  ((symbol-function 'frame-char-width)
                   (lambda (&rest _) 10))
                  ((symbol-function 'redisplay)
                   (lambda (&rest _) (ert-fail "Centering must not force scrolling")))
                  ((symbol-function 'posn-at-point)
                   (lambda (position &rest _)
                     (list nil nil (cons (if (= position (point-min)) 300 850) 0))))
                  ((symbol-function '+dashboard--center-line)
                   (lambda (_start _end width) (push width widths))))
          (+dashboard--recenter-navigator-visible (current-buffer))
          (should (equal widths '(55.0))))))))

(ert-deftest +dashboard-items-center-measured-boxes ()
  "Each section's longest row sets its box; headings stay centered."
  (should-not dashboard-center-content)
  (should (eq dashboard-path-style 'truncate-middle))
  (with-temp-buffer
    (insert (propertize "Recent Files:" 'face 'dashboard-heading)
            "\n  long/path/to/a/file.md\n  short.md\n\n"
            (propertize "Projects:" 'face 'dashboard-heading)
            "\n  project/\n")
    (cl-letf (((symbol-function '+dashboard--line-align-width)
               (lambda (start end)
                 (string-width (buffer-substring-no-properties start end)))))
      (+dashboard--center-item-region (point-min) (point-max)))
    (let (heading recent-one recent-two projects-heading project)
      (goto-char (point-min))
      (setq heading (get-text-property (point) 'line-prefix))
      (forward-line 1)
      (setq recent-one (get-text-property (point) 'line-prefix))
      (forward-line 1)
      (setq recent-two (get-text-property (point) 'line-prefix))
      (forward-line 2)
      (setq projects-heading (get-text-property (point) 'line-prefix))
      (forward-line 1)
      (setq project (get-text-property (point) 'line-prefix))
      (should (equal (get-text-property 0 'display recent-one)
                     (get-text-property 0 'display recent-two)))
      (should (equal (get-text-property 0 'display recent-one)
                     '(space . (:align-to (- center 12.0)))))
      (should (equal (get-text-property 0 'display project)
                     '(space . (:align-to (- center 5.0)))))
      (should-not (equal (get-text-property 0 'display heading)
                         (get-text-property 0 'display recent-one)))
      (should-not (equal (get-text-property 0 'display projects-heading)
                         (get-text-property 0 'display project)))))
  (with-temp-buffer
    (insert "Agenda · next 7 days\n1 open\n")
    (cl-letf (((symbol-function '+dashboard--line-align-width)
               (lambda (start end)
                 (string-width (buffer-substring-no-properties start end)))))
      (+dashboard--center-item-region (point-min) (point-max)))
    (goto-char (point-min))
    (let* ((heading (get-text-property (point) 'line-prefix))
           (row (progn (forward-line 1)
                       (get-text-property (point) 'line-prefix))))
      (should-not (equal (get-text-property 0 'display heading)
                         (get-text-property 0 'display row))))))

(ert-deftest +dashboard-item-box-measures-visible-glyph-width ()
  "Rendered row width wins over an underestimated string pixel width."
  (with-temp-buffer
    (insert (propertize "Recent Files:" 'face 'dashboard-heading)
            "\n  icon long filename\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (cl-letf (((symbol-function 'window-text-pixel-size)
                 (lambda (&rest _) '(300 . 20)))
                ((symbol-function 'frame-char-width)
                 (lambda (&rest _) 10)))
        (+dashboard--center-item-region (point-min) (point-max)))
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (get-text-property
                      0 'display (get-text-property (point) 'line-prefix))
                     '(space . (:align-to (- center 15.0))))))))

(ert-deftest +dashboard-truncated-recent-path-opens-original-file ()
  "Display shortening keeps the original file behind a Dashboard item."
  (let* ((dashboard-recentf-alist nil)
         (path (concat "/tmp/very-long-research-project/attachments/results/2026/"
                       "an-even-longer-filename-than-the-panel-can-show.md")))
    (let ((shown (car (dashboard-shorten-paths
                       (list path) 'dashboard-recentf-alist 'recents))))
      (should (< (length shown) (length path)))
      (should (equal (dashboard-expand-path-alist shown dashboard-recentf-alist)
                     path)))))

(ert-deftest +dashboard-navigator-region-stops-before-later-sections ()
  "Navigator recentering must never consume the items inserted after it."
  (with-temp-buffer
    (+dashboard-center-navigator-a (lambda () (insert "[config] [docs]\n")))
    (let ((navigator-end (marker-position (cdr +dashboard--navigator-region))))
      (insert "Recent Files:\n    item.md\n")
      (should (= (marker-position (cdr +dashboard--navigator-region))
                 navigator-end))
      (should (< navigator-end (point-max))))))

(ert-deftest +dashboard-agenda-reports-a-retryable-host-failure ()
  "A failed Agenda answer must replace `loading' with a retryable line."
  (with-temp-buffer
    (+dashboard--insert-agenda-card-content nil '((message . "host down")))
    (goto-char (point-min))
    (should (search-forward "Agenda unavailable · host down" nil t))
    (should (search-forward "[retry]" nil t))
    (should (button-at (1- (point))))))

(ert-deftest +dashboard-agenda-insertion-only-reads-the-complete-cache ()
  "Dashboard construction must not start a request or rewrite itself later."
  (require 'noema-agenda)
  (let ((+dashboard--agenda-snapshot
         '((stats . ((open . 3) (doing . 1) (blocked . 0) (overdue . 2)))
           (todos . []) (days . [])))
        (+dashboard--agenda-error nil)
        (queries 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
                 (lambda (_callback) (cl-incf queries))))
        (+dashboard-insert-agenda)
        (should (= queries 0))
        (goto-char (point-min))
        (should (search-forward "3 open   1 doing   0 blocked   2 overdue" nil t))))))

(ert-deftest +dashboard-agenda-includes-unscheduled-roam-work ()
  "The compact card falls back to open todos outside the date buckets."
  (with-temp-buffer
    (+dashboard--insert-agenda-card-content
     '((stats . ((open . 1) (doing . 0) (blocked . 0) (overdue . 0)))
       (todos . [((uid . "roam") (text . "Read the Roam task")
                  (status . "todo") (effectiveStatus . "todo"))])
       (days . [((date . "2026-09-16") (entries . []))]))
     nil)
    (goto-char (point-min))
    (should (search-forward "open  TODO" nil t))
    (should (search-forward "Read the Roam task" nil t))))

(ert-deftest +dashboard-agenda-cache-refresh-coalesces-callers ()
  "One host query publishes a complete snapshot to every waiting caller."
  (require 'noema-agenda)
  (let ((+dashboard--agenda-snapshot nil)
        (+dashboard--agenda-error nil)
        (+dashboard--agenda-request-pending nil)
        (+dashboard--agenda-waiters nil)
        (queries 0)
        callback
        answers)
    (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
               (lambda (cb) (cl-incf queries) (setq callback cb))))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'first changed) answers)))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (push (cons 'second changed) answers)))
      (should (= queries 1))
      (funcall callback
               '((stats . ((open . 1))) (todos . []) (days . [])) nil)
      (should (equal (mapcar #'car answers) '(second first)))
      (should (cl-every #'cdr answers))
      (should-not +dashboard--agenda-request-pending)
      (should (= (alist-get 'open
                            (alist-get 'stats +dashboard--agenda-snapshot))
                 1)))))

(ert-deftest +dashboard-unchanged-agenda-is-not-rendered-twice ()
  "An equal host answer keeps the rendered snapshot, so nothing rebuilds."
  (require 'noema-agenda)
  (let* ((snapshot '((stats . ((open . 1))) (todos . []) (days . [])))
         (+dashboard--agenda-snapshot snapshot)
         (+dashboard--agenda-error nil)
         (+dashboard--agenda-request-pending nil)
         (+dashboard--agenda-waiters nil)
         (+dashboard--agenda-dirty t)
         (+dashboard--agenda-cache-loaded t)
         (+dashboard--rendered-agenda 'unrendered)
         callback changed-answer)
    (with-temp-buffer (+dashboard-insert-agenda))
    (should (+dashboard--rendered-current-p))
    (cl-letf (((symbol-function 'noema-agenda-dashboard-query)
               (lambda (cb) (setq callback cb))))
      (+dashboard--refresh-agenda-cache
       (lambda (changed) (setq changed-answer (list changed))))
      (funcall callback (copy-tree snapshot) nil))
    (should (equal changed-answer '(nil)))
    (should (eq +dashboard--agenda-snapshot snapshot))
    (should (+dashboard--rendered-current-p))))

(ert-deftest +dashboard-startup-renders-the-last-session-agenda ()
  "The first render uses the saved snapshot; a changed answer is saved."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-seed-" nil ".eld"))
         (saved '((stats . ((open . 7) (doing . 0) (blocked . 0) (overdue . 0)))
                  (todos . []) (days . [])))
         (+dashboard--agenda-snapshot nil)
         (+dashboard--agenda-error nil)
         (+dashboard--agenda-cache-loaded nil)
         (+dashboard--rendered-agenda 'unrendered))
    (unwind-protect
        (progn
          (+dashboard--save-agenda-cache saved)
          (with-temp-buffer
            (+dashboard-insert-agenda)
            (goto-char (point-min))
            (should (search-forward "7 open" nil t)))
          (should (equal +dashboard--agenda-snapshot saved))
          ;; A different answer replaces the file for the next session.
          (let ((+dashboard--agenda-request-pending nil))
            (+dashboard--finish-agenda-refresh
             '((stats . ((open . 2))) (todos . []) (days . [])) nil))
          (should (= 2 (alist-get 'open
                                  (alist-get 'stats
                                             (with-temp-buffer
                                               (insert-file-contents
                                                +dashboard-agenda-cache-file)
                                               (read (current-buffer))))))))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-agenda-cache-rejects-reader-code-and-trailing-data ()
  "A damaged cache cannot evaluate Lisp or supply a partial snapshot."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-untrusted-" nil ".eld"))
         (+dashboard--agenda-cache-loaded nil)
         (+dashboard--agenda-snapshot nil)
         (+dashboard-test-read-eval nil)
         (read-eval t))
    (unwind-protect
        (progn
          (with-temp-file +dashboard-agenda-cache-file
            (insert "#.(progn (setq +dashboard-test-read-eval t) '((stats . nil)))"))
          (+dashboard--load-agenda-cache)
          (should-not +dashboard-test-read-eval)
          (should-not +dashboard--agenda-snapshot)
          (with-temp-file +dashboard-agenda-cache-file
            (insert "((stats . nil)) ((stats . ((open . 99))))"))
          (setq +dashboard--agenda-cache-loaded nil)
          (+dashboard--load-agenda-cache)
          (should-not +dashboard--agenda-snapshot))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-agenda-cache-rename-failure-preserves-last-good-file ()
  "A failed cache replacement leaves the previous session snapshot intact."
  (let* ((+dashboard-agenda-cache-file
          (make-temp-file "dashboard-agenda-atomic-" nil ".eld"))
         (old '((stats . ((open . 7))) (todos . []) (days . [])))
         (new '((stats . ((open . 2))) (todos . []) (days . []))))
    (unwind-protect
        (progn
          (+dashboard--save-agenda-cache old)
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (&rest _) (error "simulated rename failure"))))
            (+dashboard--save-agenda-cache new))
          (with-temp-buffer
            (insert-file-contents +dashboard-agenda-cache-file)
            (should (equal (read (current-buffer)) old)))
          (should-not
           (directory-files
            (file-name-directory +dashboard-agenda-cache-file)
            nil
            (concat "^" (regexp-quote
                           (file-name-nondirectory +dashboard-agenda-cache-file))
                    "\\.tmp-"))))
      (delete-file +dashboard-agenda-cache-file))))

(ert-deftest +dashboard-refresh-restores-chunlian-after-content ()
  "A complete Dashboard refresh clears old overlays and restores them last."
  (let (calls)
    (with-temp-buffer
      (dashboard-mode)
      (setq-local chunlian-mode t)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (cl-letf (((symbol-function 'chunlian--clear-display)
                   (lambda () (push 'clear calls)))
                  ((symbol-function 'dashboard-refresh-buffer)
                   (lambda () (push 'refresh calls)))
                  ((symbol-function 'chunlian-mode)
                   (lambda (&optional _arg) (push 'chunlian calls))))
          (+dashboard--refresh-buffer-completely (current-buffer))
          (should (equal (nreverse calls) '(clear refresh chunlian))))))))

(ert-deftest +dashboard-agenda-change-does-no-io-while-hidden ()
  "A hidden Dashboard only marks its cache dirty."
  (let ((+dashboard--agenda-dirty nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer) (lambda (_name) (current-buffer)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function '+dashboard--refresh-agenda-cache)
                 (lambda (&rest _) (ert-fail "hidden Dashboard queried Agenda"))))
        (+dashboard-agenda-handle-change)
        (should +dashboard--agenda-dirty)))))

(ert-deftest +dashboard-agenda-host-stop-releases-request-resources ()
  "Stopping the host cancels the deadline and drops retained callbacks."
  (let ((+dashboard--agenda-request-generation 2)
        (+dashboard--agenda-request-pending t)
        (+dashboard--agenda-dirty nil)
        (+dashboard--agenda-waiters (list (lambda (_) nil)))
        (+dashboard--agenda-request-timer (run-at-time 60 nil #'ignore)))
    (+dashboard-agenda-host-stopped)
    (should (= +dashboard--agenda-request-generation 3))
    (should-not +dashboard--agenda-request-pending)
    (should-not +dashboard--agenda-request-timer)
    (should-not +dashboard--agenda-waiters)
    (should +dashboard--agenda-dirty)))

(ert-deftest +dashboard-agenda-is-the-last-section-before-the-footer ()
  "The Agenda card stays at the bottom of the startup dashboard."
  (let* ((list (remq 'dashboard-insert-newline dashboard-startupify-list))
         (tail (last list 2)))
    (should (equal tail '(+dashboard-insert-agenda dashboard-insert-footer)))))

(provide 'init-ui-dashboard-tests)
;;; init-ui-dashboard-tests.el ends here
