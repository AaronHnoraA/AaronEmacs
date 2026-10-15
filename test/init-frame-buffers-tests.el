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
                        #'ibuffer-visit-buffer-other-window))
            (call-interactively (key-binding (kbd "M-RET")))
            (should (eq (window-buffer) canonical))))
      (dolist (buffer (list duplicate canonical listing))
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
