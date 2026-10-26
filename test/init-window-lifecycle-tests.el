;;; init-window-lifecycle-tests.el --- Contracts of window-lifecycle.md -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One test per contract of docs/design/window-lifecycle.md; the test name
;; starts with the contract number.  Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-window-lifecycle-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ui/windows/config)

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
      (+windows/close-view)
      (should (buffer-live-p closing))
      (should (eq (window-buffer) previous)))))

(ert-deftest c6-close-view-closes-a-spare-window-without-history ()
  (window-lifecycle-tests--with-buffers ((left "c6-left") (right "c6-right"))
    (switch-to-buffer left)
    (let ((window (split-window-right)))
      (set-window-buffer window right)
      (set-window-prev-buffers window nil)
      (select-window window)
      (+windows/close-view)
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
    (+windows/close-view)
    (should (buffer-live-p terminal))
    (switch-to-buffer terminal)
    (+windows/kill-buffer-dwim)
    (should-not (buffer-live-p terminal))))

(ert-deftest c7-ending-a-special-buffer-only-closes-its-view ()
  (window-lifecycle-tests--with-buffers ((previous "c7-previous-2")
                                         (special "*c7-special*"))
    (switch-to-buffer previous)
    (switch-to-buffer special)
    (+windows/kill-buffer-dwim)
    (should (buffer-live-p special))))

(ert-deftest c14-cmd-w-closes-pane-and-keeps-buffer ()
  (window-lifecycle-tests--with-buffers ((left "c14-left")
                                         (right "c14-right"))
    (switch-to-buffer left)
    (let ((window (split-window-right)))
      (set-window-buffer window right)
      (select-window window)
      (should (eq (key-binding (kbd "M-w")) #'+windows/close-window))
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

(ert-deftest c14-cmd-w-closes-sole-main-view-beside-side-window ()
  "A Treemacs-style side window does not make the main window deletable."
  (window-lifecycle-tests--with-buffers ((previous "c14-side-previous")
                                         (page "c14-side-page" xwidget-webkit-mode)
                                         (sidebar "c14-sidebar"))
    (switch-to-buffer previous)
    (switch-to-buffer page)
    (let* ((main (selected-window))
           (side (display-buffer-in-side-window
                  sidebar '((side . left) (slot . 0)))))
      (should (window-parent main))
      (should (eq main (window-main-window)))
      (call-interactively (key-binding (kbd "M-w")))
      (should (window-live-p main))
      (should (window-live-p side))
      (should (eq (window-buffer main) previous))
      (should (buffer-live-p page)))))

(ert-deftest c10-sole-main-view-beside-side-window-has-no-history ()
  "The fallback does not try to delete the frame's sole main window."
  (window-lifecycle-tests--with-buffers ((page "c10-side-page" xwidget-webkit-mode)
                                         (sidebar "c10-sidebar"))
    (switch-to-buffer page)
    (let* ((main (selected-window))
           (side (display-buffer-in-side-window
                  sidebar '((side . left) (slot . 0)))))
      (set-window-prev-buffers main nil)
      (+windows/close-view)
      (should (window-live-p main))
      (should (window-live-p side))
      (should-not (eq (window-buffer main) page))
      (should (buffer-live-p page)))))

(ert-deftest c14-cmd-w-dismisses-minibuffer-before-changing-layout ()
  (let (dismissed)
    (with-current-buffer (window-buffer (minibuffer-window))
      (cl-letf (((symbol-function 'abort-recursive-edit)
                 (lambda () (setq dismissed t)))
                ((symbol-function 'delete-window)
                 (lambda (&rest _) (ert-fail "Cmd-W deleted a window in minibuffer"))))
        (+windows/close-window)))
    (should dismissed)))

(ert-deftest c8-fallback-never-takes-a-single-instance-buffer-shown-elsewhere ()
  (dolist (mode +single-instance-modes)
    (window-lifecycle-tests--with-buffers ((surface "c8-surface")
                                           (other "c8-other"))
      (with-current-buffer surface (setq-local major-mode mode))
      (switch-to-buffer other)
      (let ((window (split-window-right)))
        (set-window-buffer window surface)
        ;; Every fallback path, not only bury/kill/quit.
        (should (+window-skip-dashboard-and-xwidget-p (selected-window) surface nil))
        (should (+window-skip-dashboard-and-xwidget-p (selected-window) surface t))
        (delete-window window)))))

(ert-deftest c8-an-undisplayed-terminal-may-come-back ()
  (window-lifecycle-tests--with-buffers ((terminal "c8-terminal" portal-terminal-mode))
    (should-not (+window-skip-dashboard-and-xwidget-p (selected-window) terminal nil))
    (should-not (+window-skip-dashboard-and-xwidget-p (selected-window) terminal t))))

(ert-deftest c8-a-buffer-can-declare-itself-single-instance ()
  (window-lifecycle-tests--with-buffers ((custom "c8-custom"))
    (should-not (+buffer-single-instance-p custom))
    (with-current-buffer custom (setq-local +buffer-single-instance t))
    (should (+buffer-single-instance-p custom))))

(ert-deftest c10-last-window-without-candidates-stays-open ()
  (window-lifecycle-tests--with-buffers ((only "c10-only"))
    (switch-to-buffer only)
    (set-window-prev-buffers (selected-window) nil)
    (+windows/close-view)
    (should (window-live-p (selected-window)))
    (should (buffer-live-p only))))

(ert-deftest q3-detached-pages-beyond-the-limit-are-ended ()
  (require 'noema/bridge/config)
  (let ((pages (cl-loop for i below 4
                        collect (generate-new-buffer (format "*q3-page-%d*" i))))
        (+noema-detached-page-limit 2)
        (+noema--app-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function '+noema--browser-buffers)
                   (lambda () (seq-filter #'buffer-live-p pages))))
          ;; Most recently used first: page 3, 2, 1, 0.
          (dolist (page pages) (bury-buffer page))
          (dolist (page pages) (switch-to-buffer page))
          (switch-to-buffer (get-buffer-create "*q3-other*"))
          (+noema--trim-detached-pages)
          (should (equal (mapcar #'buffer-live-p pages) '(nil nil t t))))
      (dolist (buffer (cons (get-buffer "*q3-other*") pages))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; C12, C13: who has the keyboard in a native view

(ert-deftest c12-a-handed-off-prompt-leaves-emacs-the-keyboard ()
  "A chord whose command reads the minibuffer does not resume the view."
  (require 'term/portal/config)
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
                               (should (memq #'+portal--keep-emacs-after-minibuffer-h
                                             minibuffer-setup-hook))
                               (+portal--keep-emacs-after-minibuffer-h))))
        (should-not resumed)
        (should (buffer-local-value '+portal--explicitly-unfocused
                                    terminal))))))

(ert-deftest c13-emacs-keeps-the-keyboard-until-the-pane-changes ()
  "After Emacs takes the keyboard, its next commands stay in Emacs."
  (require 'term/portal/config)
  (window-lifecycle-tests--with-buffers ((terminal " *c13-terminal*")
                                         (ordinary " *c13-ordinary*"))
    (switch-to-buffer terminal)
    (setq-local portal-input-mode t
                portal-view--target "terminal:c13"
                +portal--explicitly-unfocused t)
    (let ((other (split-window-right))
          (+portal--selected-pane-state nil)
          (+portal--selected-view-state nil)
          (focuses 0))
      (set-window-buffer other ordinary)
      (cl-letf (((symbol-function 'portal-input--action) #'ignore)
                ((symbol-function 'portal-input-focus)
                 (lambda () (cl-incf focuses))))
        (setq +portal--selected-pane-state
              (list (selected-frame) (selected-window) terminal))
        ;; Returning from the minibuffer reselects the same pane, and the
        ;; frame may change layout: neither gives the view the keyboard.
        (+portal--focus-selected-window (selected-frame) t)
        (setq-local portal-view--geometry '(resized))
        (+portal--focus-selected-window (selected-frame))
        (should (= focuses 0))
        ;; Moving away and back does.
        (select-window other)
        (+portal--focus-after-command)
        (select-window (get-buffer-window terminal))
        (+portal--focus-after-command)
        (should (= focuses 1))))))

(provide 'init-window-lifecycle-tests)

;;; init-window-lifecycle-tests.el ends here
