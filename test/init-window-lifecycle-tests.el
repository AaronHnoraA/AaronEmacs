;;; init-window-lifecycle-tests.el --- Contracts of window-lifecycle.md -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One test per contract of docs/design/window-lifecycle.md; the test name
;; starts with the contract number.  Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-window-lifecycle-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'init-windows)

(defmacro window-lifecycle-tests--with-buffers (bindings &rest body)
  "Bind each (VAR NAME [MODE]) in BINDINGS to a fresh buffer, run BODY.
MODE is set as the buffer's `major-mode' symbol only; no mode body runs."
  (declare (indent 1))
  `(let ,(mapcar (lambda (b) `(,(car b) (generate-new-buffer ,(cadr b)))) bindings)
     ,@(delq nil (mapcar (lambda (b)
                           (when (nth 2 b)
                             `(with-current-buffer ,(car b)
                                (setq-local major-mode ',(nth 2 b)))))
                         bindings))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           ,@body)
       (dolist (buffer (list ,@(mapcar #'car bindings)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))))))

(ert-deftest c6-close-view-is-one-transition-for-every-kind ()
  "Closing a view keeps its buffer alive and shows the previous one."
  (dolist (mode '(fundamental-mode portal-terminal-mode xwidget-webkit-mode))
    (window-lifecycle-tests--with-buffers ((previous "c6-previous")
                                           (closing "c6-closing"))
      (with-current-buffer closing (setq-local major-mode mode))
      (switch-to-buffer previous)
      (switch-to-buffer closing)
      (my/close-view)
      (should (buffer-live-p closing))
      (should (eq (window-buffer) previous)))))

(ert-deftest c6-close-view-closes-a-spare-window-without-history ()
  (window-lifecycle-tests--with-buffers ((left "c6-left") (right "c6-right"))
    (switch-to-buffer left)
    (let ((window (split-window-right)))
      (set-window-buffer window right)
      (set-window-prev-buffers window nil)
      (select-window window)
      (my/close-view)
      (should (buffer-live-p right))
      (should-not (window-live-p window)))))

(ert-deftest c7-only-ending-actions-end-buffers ()
  "Closing a view or a window never kills; H-q ends a single-instance buffer."
  (window-lifecycle-tests--with-buffers ((previous "c7-previous")
                                         (terminal "*c7-terminal*" portal-terminal-mode))
    (switch-to-buffer previous)
    (let ((window (split-window-right)))
      (set-window-buffer window terminal)
      (delete-window window))
    (should (buffer-live-p terminal))
    (switch-to-buffer terminal)
    (my/close-view)
    (should (buffer-live-p terminal))
    (switch-to-buffer terminal)
    (my/kill-buffer-dwim)
    (should-not (buffer-live-p terminal))))

(ert-deftest c7-ending-a-special-buffer-only-closes-its-view ()
  (window-lifecycle-tests--with-buffers ((previous "c7-previous-2")
                                         (special "*c7-special*"))
    (switch-to-buffer previous)
    (switch-to-buffer special)
    (my/kill-buffer-dwim)
    (should (buffer-live-p special))))

(ert-deftest c14-cmd-w-closes-pane-and-keeps-buffer ()
  (window-lifecycle-tests--with-buffers ((left "c14-left")
                                         (right "c14-right"))
    (switch-to-buffer left)
    (let ((window (split-window-right)))
      (set-window-buffer window right)
      (select-window window)
      (should (eq (key-binding (kbd "M-w")) #'my/close-window))
      (call-interactively (key-binding (kbd "M-w")))
      (should-not (window-live-p window))
      (should (buffer-live-p right))
      (should (eq (window-buffer) left)))))

(ert-deftest c14-cmd-w-keeps-frame-for-sole-pane ()
  (window-lifecycle-tests--with-buffers ((previous "c14-previous")
                                         (only "c14-only"))
    (switch-to-buffer previous)
    (switch-to-buffer only)
    (let ((frame (selected-frame)) closed)
      (cl-letf (((symbol-function 'delete-frame)
                 (lambda (&rest _) (setq closed t))))
        (call-interactively (key-binding (kbd "M-w"))))
      (should-not closed)
      (should (frame-live-p frame))
      (should (eq (window-buffer) previous))
      (should (buffer-live-p only)))))

(ert-deftest c14-cmd-w-dismisses-minibuffer-before-changing-layout ()
  (let (dismissed)
    (with-current-buffer (window-buffer (minibuffer-window))
      (cl-letf (((symbol-function 'abort-recursive-edit)
                 (lambda () (setq dismissed t)))
                ((symbol-function 'delete-window)
                 (lambda (&rest _) (ert-fail "Cmd-W deleted a window in minibuffer"))))
        (my/close-window)))
    (should dismissed)))

(ert-deftest c8-fallback-never-takes-a-single-instance-buffer-shown-elsewhere ()
  (dolist (mode my/single-instance-modes)
    (window-lifecycle-tests--with-buffers ((surface "c8-surface")
                                           (other "c8-other"))
      (with-current-buffer surface (setq-local major-mode mode))
      (switch-to-buffer other)
      (let ((window (split-window-right)))
        (set-window-buffer window surface)
        ;; Every fallback path, not only bury/kill/quit.
        (should (my/window-skip-dashboard-and-xwidget-p (selected-window) surface nil))
        (should (my/window-skip-dashboard-and-xwidget-p (selected-window) surface t))
        (delete-window window)))))

(ert-deftest c8-an-undisplayed-terminal-may-come-back ()
  (window-lifecycle-tests--with-buffers ((terminal "c8-terminal" portal-terminal-mode))
    (should-not (my/window-skip-dashboard-and-xwidget-p (selected-window) terminal nil))
    (should-not (my/window-skip-dashboard-and-xwidget-p (selected-window) terminal t))))

(ert-deftest c8-a-buffer-can-declare-itself-single-instance ()
  (window-lifecycle-tests--with-buffers ((custom "c8-custom"))
    (should-not (my/buffer-single-instance-p custom))
    (with-current-buffer custom (setq-local my/buffer-single-instance t))
    (should (my/buffer-single-instance-p custom))))

(ert-deftest c10-last-window-without-candidates-stays-open ()
  (window-lifecycle-tests--with-buffers ((only "c10-only"))
    (switch-to-buffer only)
    (set-window-prev-buffers (selected-window) nil)
    (my/close-view)
    (should (window-live-p (selected-window)))
    (should (buffer-live-p only))))

(ert-deftest q3-detached-pages-beyond-the-limit-are-ended ()
  (require 'init-noema)
  (let ((pages (cl-loop for i below 4
                        collect (generate-new-buffer (format "*q3-page-%d*" i))))
        (my/noema-detached-page-limit 2)
        (my/noema--app-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema--browser-buffers)
                   (lambda () (seq-filter #'buffer-live-p pages))))
          ;; Most recently used first: page 3, 2, 1, 0.
          (dolist (page pages) (bury-buffer page))
          (dolist (page pages) (switch-to-buffer page))
          (switch-to-buffer (get-buffer-create "*q3-other*"))
          (my/noema--trim-detached-pages)
          (should (equal (mapcar #'buffer-live-p pages) '(nil nil t t))))
      (dolist (buffer (cons (get-buffer "*q3-other*") pages))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; C12, C13: who has the keyboard in a native view

(ert-deftest c12-a-handed-off-prompt-leaves-emacs-the-keyboard ()
  "A chord whose command reads the minibuffer does not resume the view."
  (require 'init-portal)
  (require 'portal-input)
  (window-lifecycle-tests--with-buffers ((terminal " *c12-terminal*"))
    (switch-to-buffer terminal)
    (setq-local portal-input-mode t
                portal-view--target "terminal:c12")
    (let (resumed)
      (cl-letf (((symbol-function 'portal-view-buffers)
                 (lambda (&rest _) (list terminal)))
                ((symbol-function 'portal-input-focus)
                 (lambda () (setq resumed t)))
                ((symbol-function 'portal--input) (lambda (&rest _) t)))
        (let ((unread-command-events nil))
          ;; A direct shortcut gives the view its keyboard back.
          (portal-input--run "terminal:c12" 1 (kbd "s-j")
                             (lambda () (interactive)))
          (should resumed)
          (setq resumed nil)
          ;; A prompt, as M-x opens, keeps Emacs in control.
          (portal-input--run "terminal:c12" 2 (kbd "M-x")
                             (lambda () (interactive)
                               (should (memq #'my/portal--keep-emacs-after-minibuffer-h
                                             minibuffer-setup-hook))
                               (my/portal--keep-emacs-after-minibuffer-h))))
        (should-not resumed)
        (should (buffer-local-value 'my/portal--explicitly-unfocused
                                    terminal))))))

(ert-deftest c13-emacs-keeps-the-keyboard-until-the-pane-changes ()
  "After Emacs takes the keyboard, its next commands stay in Emacs."
  (require 'init-portal)
  (window-lifecycle-tests--with-buffers ((terminal " *c13-terminal*")
                                         (ordinary " *c13-ordinary*"))
    (switch-to-buffer terminal)
    (setq-local portal-input-mode t
                portal-view--target "terminal:c13"
                my/portal--explicitly-unfocused t)
    (let ((other (split-window-right))
          (my/portal--selected-pane-state nil)
          (my/portal--selected-view-state nil)
          (focuses 0))
      (set-window-buffer other ordinary)
      (cl-letf (((symbol-function 'portal-input--action) #'ignore)
                ((symbol-function 'portal-input-focus)
                 (lambda () (cl-incf focuses))))
        (setq my/portal--selected-pane-state
              (list (selected-frame) (selected-window) terminal))
        ;; Returning from the minibuffer reselects the same pane, and the
        ;; frame may change layout: neither gives the view the keyboard.
        (my/portal--focus-selected-window (selected-frame) t)
        (setq-local portal-view--geometry '(resized))
        (my/portal--focus-selected-window (selected-frame))
        (should (= focuses 0))
        ;; Moving away and back does.
        (select-window other)
        (my/portal--focus-after-command)
        (select-window (get-buffer-window terminal))
        (my/portal--focus-after-command)
        (should (= focuses 1))))))

(provide 'init-window-lifecycle-tests)

;;; init-window-lifecycle-tests.el ends here
