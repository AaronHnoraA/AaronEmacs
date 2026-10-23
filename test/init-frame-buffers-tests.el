;;; init-frame-buffers-tests.el --- Frame buffer UI boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'perspective)
(require 'consult)
(require 'init-tabbar)
(require 'init-doom-extra)
(require 'init-minibuffer)

(defun my/test-ibuffer-goto-buffer (buffer)
  "Move point to BUFFER's row in the current Ibuffer listing."
  (goto-char (point-min))
  (unless (search-forward (buffer-name buffer) nil t)
    (ert-fail (format "Ibuffer did not list %s" (buffer-name buffer))))
  (beginning-of-line)
  (should (eq (ibuffer-current-buffer) buffer)))

(ert-deftest ibuffer-noema-canonical-visit-preserves-native-single-window ()
  "Opening a duplicate should keep Ibuffer's mark and prefix behavior."
  (let ((duplicate (generate-new-buffer "noema-duplicate-visit"))
        (canonical (generate-new-buffer "noema-canonical-visit"))
        listing)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (split-window-right)
          (cl-letf (((symbol-function 'my/ibuffer-noema-canonical-buffer)
                     (lambda (buffer)
                       (if (eq buffer duplicate) canonical buffer))))
            (ibuffer)
            (setq listing (current-buffer))
            (ibuffer-update nil t)
            (my/test-ibuffer-goto-buffer duplicate)
            (ibuffer-set-mark ibuffer-marked-char)
            (ibuffer-visit-buffer t)
            (should (eq (window-buffer) canonical))
            (should (= (length (window-list)) 1))
            (with-current-buffer listing
              (my/test-ibuffer-goto-buffer duplicate)
              (should (memq duplicate (ibuffer-get-marked-buffers))))))
      (dolist (buffer (list duplicate canonical listing))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ibuffer-noema-canonical-other-window-preserves-noselect ()
  "The native noselect variant keeps Ibuffer selected."
  (let ((duplicate (generate-new-buffer "noema-duplicate-other"))
        (canonical (generate-new-buffer "noema-canonical-other"))
        listing)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'my/ibuffer-noema-canonical-buffer)
                     (lambda (buffer)
                       (if (eq buffer duplicate) canonical buffer))))
            (ibuffer)
            (setq listing (current-buffer))
            (ibuffer-update nil t)
            (my/test-ibuffer-goto-buffer duplicate)
            (ibuffer-visit-buffer-other-window-noselect)
            (should (eq (window-buffer) listing))
            (should (eq (selected-window) (get-buffer-window listing)))
            (should (get-buffer-window canonical))
            (should (eq (key-binding (kbd "M-RET"))
                        #'my/ibuffer-visit-below))
            (call-interactively (key-binding (kbd "M-RET")))
            (should (eq (window-buffer) canonical))))
      (dolist (buffer (list duplicate canonical listing))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ibuffer-return-splits-preserve-list-and-placement ()
  "Shift-Return opens right and Command-Return opens below."
  (let ((first (generate-new-buffer "ibuffer-split-right"))
        (second (generate-new-buffer "ibuffer-split-below"))
        listing)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (ibuffer)
          (setq listing (current-buffer))
          (ibuffer-update nil t)
          (let ((source (selected-window)))
            (my/test-ibuffer-goto-buffer first)
            (my/ibuffer-visit-right)
            (let ((right (selected-window)))
              (should (eq (window-buffer source) listing))
              (should (eq (window-buffer right) first))
              (should (> (car (window-edges right))
                         (car (window-edges source)))))
            (select-window source)
            (my/test-ibuffer-goto-buffer second)
            (my/ibuffer-visit-below)
            (let ((below (selected-window)))
              (should (eq (window-buffer source) listing))
              (should (eq (window-buffer below) second))
              (should (> (cadr (window-edges below))
                         (cadr (window-edges source)))))))
      (dolist (buffer (list first second listing))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ibuffer-command-shift-return-visits-canonical-buffer-in-frame ()
  (let ((row (generate-new-buffer "ibuffer-frame-row"))
        (canonical (generate-new-buffer "ibuffer-frame-canonical"))
        listing opened)
    (unwind-protect
        (save-window-excursion
          (ibuffer)
          (setq listing (current-buffer))
          (ibuffer-update nil t)
          (my/test-ibuffer-goto-buffer row)
          (dolist (key '("M-S-RET" "M-S-<return>"))
            (should (eq (lookup-key ibuffer-mode-map (kbd key))
                        #'my/ibuffer-visit-new-frame)))
          (cl-letf (((symbol-function 'my/ibuffer-noema-canonical-buffer)
                     (lambda (_buffer) canonical))
                    ((symbol-function 'my/buffer-display-new-frame)
                     (lambda (buffer &optional _norecord)
                       (setq opened buffer))))
            (my/ibuffer-visit-new-frame))
          (should (eq opened canonical)))
      (dolist (buffer (list row canonical listing))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest frame-tabs-use-visited-buffers-not-global-buffer-list ()
  (let ((visited (generate-new-buffer "frame-tabs-visited"))
        (unvisited (generate-new-buffer "frame-tabs-unvisited")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer visited)
          (when (bound-and-true-p persp-mode)
            (persp-add-buffer unvisited))
          (should (memq visited (my/tab-line-buffer-list)))
          (should-not (memq unvisited (my/frame-buffer-list)))
          (should-not (memq unvisited (my/tab-line-buffer-list))))
      (kill-buffer visited)
      (kill-buffer unvisited))))

(ert-deftest frame-tabs-include-noema-markdown-page ()
  (let ((note (generate-new-buffer "*Noema: frame-note.md*"))
        (file "/tmp/frame-note.md"))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer note
            (setq-local my/noema-buffer-file-name file))
          (switch-to-buffer note)
          (should (my/tab-line-visible-buffer-p note))
          (should (memq note (my/tab-line-buffer-list)))
          (should (string-match-p "F[0-9]+"
                                  (my/ibuffer-frame-tags note)))
          (should (equal (get-text-property
                          0 'help-echo (my/tab-line-tab-string note))
                         (format "%s\nmouse-1: switch\nmouse-2: close\nmouse-3: menu"
                                 file)))
          (should (equal (my/ibuffer-frame-sort-key note)
                         (list 0 (my/frame-short-id (selected-frame))
                               (buffer-name note)))))
      (kill-buffer note))))

(ert-deftest frame-tabs-hide-buffers-open-in-other-panes ()
  "Noema tabs share one pool without offering another pane's open page."
  (let ((first (generate-new-buffer "*Noema: first.md*"))
        (second (generate-new-buffer "*Noema: second.md*"))
        (free (generate-new-buffer "*Noema: free.md*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((left (selected-window))
                 (right (split-window-right)))
            (set-window-buffer left first)
            (set-window-buffer right second)
            (cl-letf (((symbol-function 'my/frame-workspace-buffers)
                       (lambda (&optional _buffers)
                         (list first second free)))
                      ((symbol-function 'my/tab-line-visible-buffer-p)
                       (lambda (buffer)
                         (memq buffer (list first second free))))
                      ((symbol-function 'my/tab-line-refresh) #'ignore))
              (with-selected-window left
                (should (equal (my/tab-line-buffer-list)
                               (list first free))))
              (with-selected-window right
                (should (equal (my/tab-line-buffer-list)
                               (list second free))))
              (my/tab-line-switch-to-buffer free right)
              (should (eq (window-buffer left) first))
              (should (eq (window-buffer right) free))
              (with-selected-window left
                (should (equal (my/tab-line-buffer-list)
                               (list first second)))))))
      (dolist (buffer (list first second free))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest frame-tabs-keep-ordinary-buffers-available-across-panes ()
  "The Noema uniqueness rule does not change normal file tabs."
  (let ((note (generate-new-buffer "*Noema: current.md*"))
        (plain (generate-new-buffer "ordinary.txt")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((left (selected-window))
                 (right (split-window-right)))
            (set-window-buffer left note)
            (set-window-buffer right plain)
            (cl-letf (((symbol-function 'my/frame-workspace-buffers)
                       (lambda (&optional _buffers) (list note plain)))
                      ((symbol-function 'my/tab-line-visible-buffer-p)
                       (lambda (buffer) (memq buffer (list note plain)))))
              (with-selected-window left
                (should (memq plain (my/tab-line-buffer-list)))))))
      (dolist (buffer (list note plain))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest frame-workspace-bar-does-not-leak-into-ordinary-pages ()
  "The terminal-only tab line must not reveal a workspace row elsewhere."
  (let ((dashboard (generate-new-buffer "*dashboard-tab-test*"))
        (plain (generate-new-buffer "ordinary-tab-test.txt"))
        (lines 1))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard (dashboard-mode))
          (set-window-buffer (selected-window) dashboard)
          (cl-letf (((symbol-function 'frame-list)
                     (lambda () (list (selected-frame))))
                    ((symbol-function 'display-graphic-p)
                     (lambda (&optional _frame) t))
                    ((symbol-function 'frame-parameter)
                     (lambda (_frame parameter)
                       (and (eq parameter 'tab-bar-lines) lines)))
                    ((symbol-function 'set-frame-parameter)
                     (lambda (_frame _parameter value) (setq lines value)))
                    ((symbol-function 'redraw-frame) #'ignore))
            (my/tab-bar-update-terminal-visibility)
            (should (zerop lines))
            (set-window-buffer (selected-window) plain)
            (my/tab-bar-update-terminal-visibility)
            (should (zerop lines))))
      (dolist (buffer (list dashboard plain))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest terminal-tabs-share-order-without-switching-other-panes ()
  "A terminal tab remains visible only in its pane or while hidden."
  (let ((first (generate-new-buffer "*terminal-test-1*"))
        (second (generate-new-buffer "*terminal-test-2*"))
        (free (generate-new-buffer "*terminal-test-3*"))
        (my/tab-line-terminal-order nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((left (selected-window))
                 (right (split-window-right)))
            (set-window-buffer left first)
            (set-window-buffer right second)
            (cl-letf (((symbol-function 'my/tab-line-ordinary-terminal-p)
                       (lambda (buffer)
                         (memq buffer (list first second free))))
                      ((symbol-function 'portal-terminal-buffers)
                       (lambda (&rest _) (list first second free)))
                      ((symbol-function 'my/tab-line-refresh) #'ignore))
              (with-selected-window left
                (should (equal (my/tab-line-buffer-list)
                               (list first free))))
              (with-selected-window right
                (should (equal (my/tab-line-buffer-list)
                               (list second free))))
              (cl-letf (((symbol-function 'get-buffer-window)
                         (lambda (buffer &optional all-frames)
                           (should (eq all-frames t))
                           (when (eq buffer second) right))))
                (with-selected-window left
                  (should (equal (my/tab-line-buffer-list)
                                 (list first free)))))
              (my/tab-line-switch-to-buffer free right)
              (should (eq (window-buffer left) first))
              (should (eq (window-buffer right) free))
              (with-selected-window left
                (should (equal (my/tab-line-buffer-list)
                               (list first second))))
              (should (equal (my/tab-line-all-terminal-buffers)
                             (list first second free))))))
      (dolist (buffer (list first second free))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ibuffer-frame-column-skips-infrastructure ()
  (let ((helper (generate-new-buffer "*frame-helper*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer helper)
          (should (equal (my/ibuffer-frame-tags helper) ""))
          (should (= (car (my/ibuffer-frame-sort-key helper)) 2)))
      (kill-buffer helper))))

(ert-deftest ibuffer-frame-sorter-renders-a-real-list ()
  (let ((user-buffer (generate-new-buffer "frame-sort-visited")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer user-buffer)
          (ibuffer)
          (ibuffer-do-sort-by-frame)
          (ibuffer-update nil t)
          (should (equal (mapcar #'car ibuffer-filter-groups)
                         '("Noema" "xwidget" "Code" "Dired"
                           "Shell" "Emacs")))
          (should (eq ibuffer-sorting-mode 'frame))
          (should (string-match-p
                   "frame-sort-visited[[:space:]]+F[0-9]+"
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer user-buffer))))

(ert-deftest consult-buffer-default-scope-and-global-escape ()
  (let ((local (generate-new-buffer "consult-frame-local"))
        (other (generate-new-buffer "consult-frame-other")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer local)
          (evil-normal-state)
          (should (eq consult-buffer-list-function #'buffer-list))
          (should (equal consult-narrow-key "<"))
          (should (memq local (my/consult-buffer-list)))
          (should-not (memq other (my/consult-buffer-list)))
          (let (scoped-source scoped-other project-source)
            (cl-letf (((symbol-function 'consult--multi)
                       (lambda (_sources &rest _args)
                         (if (eq consult-buffer-list-function
                                 #'my/consult-buffer-list)
                             (setq scoped-source consult-buffer-list-function
                                   scoped-other
                                   (funcall (plist-get consult-source-other-buffer
                                                       :enabled)))
                           (setq project-source consult-buffer-list-function))
                         (cons "unused" '(:match t)))))
              (consult-buffer)
              (consult-buffer '(consult-source-project-buffer)))
            (should (eq scoped-source #'my/consult-buffer-list))
            (should scoped-other)
            (should (eq project-source #'buffer-list)))
          (let ((consult-buffer-list-function #'my/consult-buffer-list))
            (should (assoc (buffer-name other)
                           (funcall (plist-get consult-source-other-buffer
                                                :items)))))
          (should (eq (key-binding (kbd "C-x b")) #'consult-buffer))
          (should (eq (key-binding (kbd "SPC b b")) #'consult-buffer)))
      (kill-buffer local)
      (kill-buffer other))))

(ert-deftest consult-buffer-respects-current-perspective ()
  (persp-mode 1)
  (let ((original (persp-current-name))
        (first (make-temp-name "consult-workspace-a-"))
        (second (make-temp-name "consult-workspace-b-"))
        (first-buffer (generate-new-buffer "consult-first-workspace"))
        (second-buffer (generate-new-buffer "consult-second-workspace")))
    (unwind-protect
        (progn
          (persp-switch first)
          (switch-to-buffer first-buffer)
          (persp-switch second)
          (switch-to-buffer second-buffer)
          (persp-switch first)
          (should (memq first-buffer (my/consult-buffer-list)))
          (should-not (memq second-buffer (my/consult-buffer-list)))
          (let ((consult-buffer-list-function #'my/consult-buffer-list))
            (should (assoc (buffer-name second-buffer)
                           (funcall (plist-get consult-source-other-buffer
                                                :items))))))
      (persp-switch original)
      (dolist (name (list first second))
        (when (member name (persp-names))
          (persp-kill name)))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest frame-buried-buffer-keeps-tag-and-picker-entry ()
  (let ((buffer (generate-new-buffer "frame-buried-buffer")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (bury-buffer)
          (should-not (memq buffer (my/frame-buffer-list)))
          (should (memq buffer (my/frame-buffer-list nil t)))
          (should-not (memq buffer (my/tab-line-buffer-list)))
          (should (memq buffer (my/consult-buffer-list)))
          (should (string-match-p "F[0-9]+"
                                  (my/ibuffer-frame-tags buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest frame-tab-order-is-scoped-to-perspective ()
  (persp-mode 1)
  (let ((original (persp-current-name))
        (first (make-temp-name "frame-order-a-"))
        (second (make-temp-name "frame-order-b-"))
        first-scope)
    (unwind-protect
        (progn
          (persp-switch first)
          (setq first-scope (persp-curr))
          (my/tab-line-set-order '("first.md"))
          (persp-switch second)
          (should-not (equal (my/tab-line-get-order) '("first.md")))
          (my/tab-line-set-order '("second.md"))
          (persp-switch first)
          (should (equal (my/tab-line-get-order) '("first.md")))
          (persp-switch second)
          (should (equal (my/tab-line-get-order) '("second.md"))))
      (persp-switch original)
      (dolist (name (list first second))
        (when (member name (persp-names))
          (persp-kill name))))
    (should-not (assq first-scope
                      (frame-parameter nil 'my/tab-line-orders)))))

(provide 'init-frame-buffers-tests)
;;; init-frame-buffers-tests.el ends here
