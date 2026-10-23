;;; init-windows-tests.el --- Window and buffer fallback tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-windows-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'init-windows)

(ert-deftest my/window-quit-temporary-buffer-restores-xwidget ()
  "The buffer-close shortcut must honor a popup's exact return target."
  (save-window-excursion
    (let ((source (generate-new-buffer " *quit-source*"))
          (output (generate-new-buffer " *quit-output*"))
          (picker (generate-new-buffer "*quit-skill-picker*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer source)
            (with-current-buffer output
              (setq-local major-mode 'xwidget-webkit-mode))
            (with-current-buffer picker (special-mode))
            (let* ((left (selected-window))
                   (right (split-window-right))
                   (edges (window-edges right)))
              (set-window-buffer right output)
              (select-window right)
              (pop-to-buffer picker '(display-buffer-same-window))
              (should (or (eq (nth 3 (window-parameter right 'quit-restore)) picker)
                          (eq (nth 3 (window-parameter right 'quit-restore-prev)) picker)))
              (my/kill-buffer-dwim)
              (should (window-live-p right))
              (should (eq (window-buffer right) output))
              (should (eq (window-buffer left) source))
              (should (equal (window-edges right) edges))))
        (dolist (buffer (list source output picker))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest my/window-ibuffer-q-returns-to-previous-page ()
  "The actual Ibuffer q binding should recover the page it replaced."
  (let ((source (generate-new-buffer "ibuffer-return-source")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (ibuffer)
          (call-interactively (key-binding (kbd "q")))
          (should (eq (window-buffer) source)))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest my/window-ibuffer-q-avoids-duplicate-visible-buffer ()
  "A Portal-created picker does not restore a page shown elsewhere."
  (require 'portal-input)
  (let ((source (generate-new-buffer "portal-picker-source")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((origin (selected-window)))
            (portal-input-split-right)
            (should (derived-mode-p 'ibuffer-mode))
            (call-interactively (key-binding (kbd "q")))
            (should (eq (window-buffer origin) source))
            (when (= (count-windows) 2)
              (should-not (eq (window-buffer (next-window origin)) source)))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest my/window-ordinary-ibuffer-q-avoids-duplicate-visible-buffer ()
  "The same fallback rule applies to an ordinary buffer split."
  (let ((source (generate-new-buffer "ordinary-picker-source")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((origin (selected-window)))
            (select-window (split-window-right))
            (ibuffer)
            (call-interactively (key-binding (kbd "q")))
            (should (eq (window-buffer origin) source))
            (when (= (count-windows) 2)
              (should-not (eq (window-buffer (next-window origin)) source)))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest my/window-portal-command-keys-use-normal-buffer-actions ()
  "Portal's native Command keys run the same bindings as other buffers."
  (require 'init-portal)
  (require 'portal-terminal-bindings)
  (should (eq (lookup-key portal-terminal-native-map (kbd "s-w"))
              (key-binding (kbd "M-w"))))
  (should (eq (lookup-key portal-terminal-native-map (kbd "s-W"))
              (key-binding (kbd "M-W")))))

(ert-deftest my/window-portal-focus-follows-selected-pane-and-buffer ()
  "Portal gives up focus on ordinary panes and follows view changes."
  (require 'init-portal)
  (require 'portal-input)
  (let* ((first (generate-new-buffer "portal-focus-first"))
        (second (generate-new-buffer "portal-focus-second"))
        (ordinary (generate-new-buffer "treemacs-focus-test"))
        (focused (make-hash-table :test #'equal))
        (focuses nil)
        (frame-focuses nil)
        (unfocuses nil)
        (gained nil)
        (my/portal-focus-gained-hook
         (list (lambda () (push (current-buffer) gained))))
        (my/portal--selected-pane-state nil)
        (my/portal--selected-view-state nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first)
          (let* ((first-window (selected-window))
                 (ordinary-window (split-window-right))
                 (second-window (split-window ordinary-window nil 'right)))
            (set-window-buffer ordinary-window ordinary)
            (set-window-buffer second-window second)
            (with-current-buffer first
              (setq-local portal-input-mode t
                          portal-view--target "web:first"
                          my/portal--explicitly-unfocused t))
            (with-current-buffer second
              (setq-local portal-input-mode t
                          portal-view--target "web:second"))
          (cl-letf (((symbol-function 'portal-input--action)
                     (lambda (action target &rest _)
                       (pcase action
                         ("focused" (gethash target focused))
                         ("unfocus"
                          (push target unfocuses)
                          (puthash target nil focused)))))
                    ((symbol-function 'portal-input-focus)
                     (lambda ()
                       (push portal-view--target focuses)
                       (puthash portal-view--target t focused)))
                    ((symbol-function 'frame-focus-state)
                     (lambda (&rest _) t))
                    ((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame) (push frame frame-focuses))))
            (my/portal--focus-selected-window (selected-frame))
            (my/portal--focus-selected-window (selected-frame))
            (with-current-buffer first
              (should-not my/portal--explicitly-unfocused))
            (should (equal focuses '("web:first")))
            (should (equal gained (list first)))
            ;; Explicit escape stays in Emacs until a real window change.
            (puthash "web:first" nil focused)
            (my/portal--focus-selected-window (selected-frame))
            (should (equal focuses '("web:first")))
            (with-current-buffer first
              (setq-local portal-view--geometry '(resized)))
            (my/portal--focus-selected-window (selected-frame))
            (should (equal focuses '("web:first" "web:first")))
            (should (equal gained (list first first)))
            (select-window ordinary-window)
            (my/portal--focus-after-command)
            (should (equal unfocuses '("web:first")))
            (should (equal frame-focuses (list (selected-frame))))
            (select-window second-window)
            (my/portal--focus-after-command)
            (should (equal (car focuses) "web:second"))
            (select-window first-window)
            (my/portal--focus-after-command)
            (should (equal (car unfocuses) "web:second"))
            (should (equal (car focuses) "web:first"))
            ;; A frame can retain its native first responder while another
            ;; frame is key; activation must reassert its actual keyboard.
            (let ((count (length focuses)))
              (my/portal--focus-selected-window (selected-frame) t)
              (should (= (length focuses) (1+ count))))
            ;; Replacing a selected Portal buffer also returns focus.
            (set-window-buffer first-window ordinary)
            (my/portal--focus-after-command)
            (should (equal (car unfocuses) "web:first")))))
      (dolist (buffer (list first second ordinary))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-moving-buffer-vacates-its-old-window ()
  "Moving a native view leaves exactly one window showing its buffer."
  (require 'init-portal)
  (let ((terminal (generate-new-buffer "portal-move-terminal"))
        (other (generate-new-buffer "portal-move-other")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer terminal)
          (let ((old (selected-window))
                (new (split-window-right)))
            (set-window-buffer new other)
            (my/portal-move-buffer-to-window terminal new)
            (should (eq (window-buffer new) terminal))
            (should-not (eq (window-buffer old) terminal))
            (should (= (length (get-buffer-window-list terminal nil t)) 1))))
      (dolist (buffer (list terminal other))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-native-sync-vacates-duplicate-before-placement ()
  "A generic buffer switch cannot leave a second host for one native view."
  (require 'init-portal)
  (require 'portal-view)
  (let ((terminal (generate-new-buffer "portal-sync-terminal"))
        placed)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer terminal)
          (let ((old (selected-window))
                (new (split-window-right)))
            (set-window-buffer new terminal)
            (select-window new)
            (cl-letf (((symbol-function 'portal-view--with-frame-title)
                       (lambda (_frame callback) (funcall callback "host")))
                      ((symbol-function 'portal--place)
                       (lambda (&rest _) (setq placed t))))
              (with-current-buffer terminal
                (setq-local portal-view--target "terminal:test"))
              (portal-view--sync-buffer terminal))
            (should placed)
            (should (eq (window-buffer new) terminal))
            (should-not (eq (window-buffer old) terminal))
            (should (= (length (get-buffer-window-list terminal nil t)) 1))))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/portal-native-sync-restores-hidden-view-with-cached-geometry ()
  "Ibuffer returning to a terminal must replace a hidden native view."
  (require 'init-portal)
  (require 'portal-view)
  (let ((terminal (generate-new-buffer "portal-hidden-native-view"))
        placed)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer terminal)
          (let ((window (selected-window))
                (frame (selected-frame)))
            (with-current-buffer terminal
              (setq-local portal-view--target "terminal:hidden-native"
                          portal-view--window window
                          portal-view--geometry
                          (list frame (frame-pixel-height frame)
                                (vconcat (window-inside-pixel-edges window)))))
            (cl-letf (((symbol-function 'portal--inspect)
                       (lambda (_target) '(:visible nil)))
                      ((symbol-function 'portal-view--with-frame-title)
                       (lambda (_frame callback) (funcall callback "host")))
                      ((symbol-function 'portal--place)
                       (lambda (&rest _) (setq placed t))))
              (portal-view--sync-buffer terminal))
            (should placed)))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/portal-close-removes-terminal-without-native-view ()
  "A closed Ghostty session must not remain as a black Ibuffer candidate."
  (require 'init-portal)
  (let ((terminal (generate-new-buffer "portal-closed-shell"))
        cleanup)
    (with-current-buffer terminal
      (setq-local major-mode 'portal-terminal-mode
                  portal-view--target "terminal:closed-shell"))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function &rest args)
                     (setq cleanup (cons function args)))))
          (with-current-buffer terminal
            (my/portal-close-terminal-buffer-a
             (lambda () (setq-local portal-view--target nil))))
          (should (buffer-live-p terminal))
          (apply (car cleanup) (cdr cleanup))
          (should-not (buffer-live-p terminal)))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/portal-terminal-exit-does-not-reuse-another-visible-terminal ()
  "Closing one terminal keeps the other terminal alive in its own window."
  (require 'init-portal)
  (let ((closing (generate-new-buffer "portal-exit-closing"))
        (remaining (generate-new-buffer "portal-exit-remaining"))
        (stale (generate-new-buffer "portal-exit-stale")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer closing)
          (let ((left (selected-window))
                (right (split-window-right)))
            (set-window-buffer right remaining)
            ;; Both a terminal shown in another pane and an old terminal
            ;; whose native target is gone may be in this pane's history.
            (set-window-buffer left remaining)
            (set-window-buffer left stale)
            (set-window-buffer left closing)
            (with-current-buffer stale
              (setq-local major-mode 'portal-terminal-mode))
            (with-current-buffer closing
              (setq-local major-mode 'portal-terminal-mode)
              (add-hook 'kill-buffer-hook #'my/portal-terminal-kill-h nil t))
            (kill-buffer closing)
            (should (buffer-live-p remaining))
            (should (eq (window-buffer right) remaining))
            (should-not (eq (window-buffer left) remaining))
            (should-not (eq (window-buffer left) stale))
            (should (= (length (get-buffer-window-list remaining nil t)) 1))))
      (dolist (buffer (list closing remaining stale))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-deleting-frame-keeps-terminal-session ()
  "Deleting a frame cannot remove its terminal from Ibuffer or the selector."
  (require 'init-portal)
  (require 'portal-view)
  (let ((terminal (generate-new-buffer "portal-frame-session"))
        (frame (selected-frame))
        hidden)
    (unwind-protect
        (progn
          (should-not (memq 'my/portal--delete-frame-terminal
                            delete-frame-functions))
          (with-current-buffer terminal
            (setq-local portal-view--target "terminal:frame-session"
                        portal-view--geometry (list frame 1 [0 0 1 1])))
          (let ((portal-view--buffers (list terminal)))
            (cl-letf (((symbol-function 'portal--hide)
                       (lambda (target) (setq hidden target))))
              (portal-view--frame-deleted frame)))
          (should (equal hidden "terminal:frame-session"))
          (should (buffer-live-p terminal))
          (with-current-buffer terminal
            (should (equal portal-view--target "terminal:frame-session"))
            (should-not portal-view--geometry)))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/window-fallback-never-duplicates-visible-portal-view ()
  "A native view is ineligible as implicit fallback even without bury mode."
  (require 'portal-view)
  (let ((terminal (generate-new-buffer "portal-fallback-occupied"))
        (ordinary (generate-new-buffer "portal-fallback-ordinary")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer terminal)
          (with-current-buffer terminal
            (setq-local portal-view--target "terminal:fallback"))
          (let ((other-window (split-window-right)))
            (set-window-buffer other-window ordinary)
            (should (my/window-skip-dashboard-and-xwidget-p
                     other-window terminal nil))
            (should-not (my/window-skip-dashboard-and-xwidget-p
                         other-window ordinary nil))))
      (dolist (buffer (list terminal ordinary))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-hidden-owner-exit-keeps-current-frame-content ()
  "A hidden former owner cannot close the frame's currently shown page."
  (require 'init-portal)
  (let ((hidden (generate-new-buffer "portal-hidden-owner"))
        (shown (generate-new-buffer "portal-owner-survivor"))
        (frame (selected-frame))
        (old-owner (frame-parameter (selected-frame) 'my/portal-terminal)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer shown)
          (set-frame-parameter frame 'my/portal-terminal hidden)
          (with-current-buffer hidden
            (setq-local major-mode 'portal-terminal-mode)
            (add-hook 'kill-buffer-hook #'my/portal-terminal-kill-h nil t))
          (kill-buffer hidden)
          (should (buffer-live-p shown))
          (should (eq (window-buffer) shown))
          (should-not (frame-parameter frame 'my/portal-terminal)))
      (set-frame-parameter frame 'my/portal-terminal old-owner)
      (dolist (buffer (list hidden shown))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-owner-exit-keeps-other-pane-in-same-frame ()
  "Exiting one owned terminal cannot hide a frame with another live pane."
  (require 'init-portal)
  (let ((closing (generate-new-buffer "portal-owner-closing"))
        (remaining (generate-new-buffer "portal-owner-remaining"))
        (frame (selected-frame))
        (old-owner (frame-parameter (selected-frame) 'my/portal-terminal))
        closed)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer closing)
          (let ((left (selected-window))
                (right (split-window-right)))
            (set-window-buffer right remaining)
            (set-frame-parameter frame 'my/portal-terminal closing)
            (with-current-buffer closing
              (setq-local major-mode 'portal-terminal-mode)
              (add-hook 'kill-buffer-hook #'my/portal-terminal-kill-h nil t))
            (cl-letf (((symbol-function 'delete-frame)
                       (lambda (&rest _) (setq closed t)))
                      ((symbol-function 'my/background-hide)
                       (lambda (&rest _) (setq closed t))))
              (kill-buffer closing))
            (should-not closed)
            (should (eq (window-buffer right) remaining))
            (should-not (eq (window-buffer left) remaining))))
      (set-frame-parameter frame 'my/portal-terminal old-owner)
      (dolist (buffer (list closing remaining))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/portal-terminal-kill-hook-keeps-its-current-buffer ()
  "Frame deletion must not redirect later buffer-local Portal cleanup hooks."
  (require 'init-portal)
  (require 'portal-view)
  (let ((closing (generate-new-buffer "portal-hook-closing"))
        (survivor (generate-new-buffer "portal-hook-survivor"))
        (frame (selected-frame))
        (old-owner (frame-parameter (selected-frame) 'my/portal-terminal))
        deleted closed)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer closing)
          (set-frame-parameter frame 'my/portal-terminal closing)
          (with-current-buffer closing
            (add-hook 'kill-buffer-hook #'my/portal-terminal-kill-h nil t)
            (add-hook 'kill-buffer-hook #'portal-view-close 90 t))
          (cl-letf (((symbol-function 'frame-list)
                     (lambda () (list frame frame)))
                    ((symbol-function 'delete-frame)
                     (lambda (&rest _)
                       (setq deleted t)
                       (set-buffer survivor)))
                    ((symbol-function 'portal-view-close)
                     (lambda () (setq closed (current-buffer))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _) nil)))
            (kill-buffer closing)
            (should deleted)
            (should (eq closed closing))
            (should (buffer-live-p survivor))))
      (set-frame-parameter frame 'my/portal-terminal old-owner)
      (dolist (buffer (list closing survivor))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest terminal-selector-command-shift-return-requests-new-frame ()
  (require 'init-portal)
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (my/terminal-selector-setup)
    (dolist (key '("M-S-RET" "M-S-<return>"))
      (let (placement)
        (cl-letf (((symbol-function 'my/terminal-selector-exit)
                   (lambda (chosen) (setq placement chosen))))
          (call-interactively (lookup-key (current-local-map) (kbd key))))
        (should (eq placement 'frame))))))

(ert-deftest terminal-selector-frame-choice-routes-new-and-existing ()
  (require 'init-portal)
  (let ((buffer (generate-new-buffer "terminal-selector-frame-existing"))
        (default-directory "/tmp/")
        opened moved)
    (unwind-protect
        (cl-letf (((symbol-function 'my/terminal-selector-candidates)
                   (lambda (&rest _)
                     (list (cons "existing" buffer)
                           (cons "new" :new))))
                  ((symbol-function 'my/portal-open-external)
                   (lambda (directory &optional _command)
                     (setq opened directory)))
                  ((symbol-function 'my/terminal-selector-move-to-new-frame)
                   (lambda (target) (setq moved target))))
          (dolist (choice '("new" "existing"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq my/terminal-selector-placement 'frame)
                         choice)))
              (my/terminal-selector)))
          (should (equal opened "/tmp/"))
          (should (eq moved buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/portal-terminal-uses-physical-local-workspace-directory ()
  "A local Remote workspace path reaches Ghostty as a real directory."
  (require 'init-portal)
  (let ((terminal (generate-new-buffer " *portal-directory-test*"))
        (logical "/fs:local:/tmp/portal-project/")
        (physical "/tmp/portal-project/")
        started-in)
    (unwind-protect
        (cl-letf (((symbol-function 'my/terminal-normalize-directory)
                   (lambda (directory) directory))
                  ((symbol-function 'remote-client-file-name)
                   (lambda (_directory) physical))
                  ((symbol-function 'my/portal--ensure) #'ignore)
                  ((symbol-function 'portal-terminal-new)
                   (lambda ()
                     (setq started-in default-directory)
                     terminal)))
          (should (eq (my/portal-create :directory logical :display nil)
                      terminal))
          (should (equal started-in physical)))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/portal-windmove-boundary-restores-native-focus ()
  "A missing neighboring window reports its warning and returns focus."
  (require 'init-portal)
  (require 'portal-input)
  (let ((terminal (generate-new-buffer " *portal-windmove-test*"))
        resumed warning)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer terminal)
          (with-current-buffer terminal (setq-local portal-input-mode t))
          (cl-letf (((symbol-function 'portal-view-buffers)
                     (lambda (&rest _) (list terminal)))
                    ((symbol-function 'portal-input-focus)
                     (lambda () (setq resumed t)))
                    ((symbol-function 'portal--input)
                     (lambda (&rest _) t))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq warning (apply #'format format-string args)))))
            (let ((unread-command-events nil))
              (portal-input--run
               "terminal:test" 1 (kbd "s-j")
               (lambda () (interactive) (windmove-left))))
          (should resumed)
          (should (string-match-p "No window left" warning))
          (setq resumed nil warning nil)
          (cl-letf (((symbol-function 'windmove-find-other-window)
                     (lambda (&rest _) (minibuffer-window))))
            (let ((unread-command-events nil))
              (portal-input--run
               "terminal:test" 2 (kbd "s-j")
               (lambda () (interactive) (windmove-down)))))
          (should resumed)
          (should (equal warning "Minibuffer is inactive"))))
      (when (buffer-live-p terminal) (kill-buffer terminal)))))

(ert-deftest my/portal-windmove-direct-web-boundary-restores-page-focus ()
  "A Web pane's direct Windmove shortcut also returns focus at an edge."
  (require 'init-portal)
  (let ((page (generate-new-buffer " *portal-web-boundary-test*"))
        reclaimed focused)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer page)
          (with-current-buffer page
            (setq-local portal-input-mode t
                        portal-view--target "web:test"))
          (cl-letf (((symbol-function 'my/portal--focus-selected-window)
                     (lambda (_frame &optional force)
                       (setq reclaimed force)))
                    ((symbol-function 'portal-input--action)
                     (lambda (&rest _) focused)))
            (let ((portal-input--resume nil))
              (windmove-right)
              (should reclaimed)
              (setq reclaimed nil)
              (my/portal--record-explicit-unfocus-a #'ignore)
              (should my/portal--explicitly-unfocused)
              (should-error (windmove-right) :type 'user-error)
              (should-not reclaimed)
              ;; Clicking the page after an explicit escape makes it eligible
              ;; for focus restoration again.
              (setq focused t)
              (windmove-right)
              (should reclaimed))))
      (when (buffer-live-p page) (kill-buffer page)))))

(define-minor-mode my/portal-test-plugin-mode
  "A local editor plugin used only by Portal isolation tests.")

(ert-deftest my/portal-buffer-disables-editor-plugins ()
  "A Portal buffer retains native input without editor completion workers."
  (with-temp-buffer
    (setq-local portal-input-mode t
                completion-at-point-functions '(ignore)
                eldoc-documentation-functions '(ignore))
    (my/portal-test-plugin-mode 1)
    (my/portal--quiet-buffer)
    (should portal-input-mode)
    (should-not my/portal-test-plugin-mode)
    (should-not completion-at-point-functions)
    (should-not eldoc-documentation-functions)))

(ert-deftest my/window-ibuffer-from-portal-returns-to-live-view ()
  "Ordinary Ibuffer q restores the same Portal session."
  (let ((portal (generate-new-buffer "portal-list-view")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer portal)
          (with-current-buffer portal (setq-local portal-input-mode t))
          (ibuffer)
          (should (buffer-live-p portal))
          (should (derived-mode-p 'ibuffer-mode))
          (call-interactively (key-binding (kbd "q")))
          (should (eq (window-buffer) portal))
          (should (buffer-live-p portal)))
      (when (buffer-live-p portal) (kill-buffer portal)))))

(ert-deftest my/window-close-last-page-opens-project-directory ()
  "A sole window with no previous work should show its project directory."
  (let ((page (generate-new-buffer "close-last-page"))
        (scratch (get-buffer-create "*scratch*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer scratch)
          (switch-to-buffer page)
          (set-window-prev-buffers (selected-window)
                                   (list (list scratch 1 1)))
          (kill-current-buffer)
          (sit-for 0.02)
          (with-current-buffer (window-buffer)
            (should (derived-mode-p 'dired-mode))))
      (when (buffer-live-p page) (kill-buffer page)))))

(ert-deftest my/window-quit-home-fallback-opens-dashboard ()
  "Quitting a sole page returns to Dashboard instead of Dired home."
  (let ((page (generate-new-buffer "quit-home-page"))
        (scratch (get-buffer-create "*scratch*"))
        (home (file-name-as-directory (expand-file-name "~"))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer scratch)
          (switch-to-buffer page)
          (with-current-buffer page (setq-local default-directory home))
          (set-window-prev-buffers (selected-window)
                                   (list (list scratch 1 1)))
          (quit-window)
          (should (eq (window-buffer) (my/window-dashboard-buffer))))
      (when (buffer-live-p page) (kill-buffer page)))))

(ert-deftest my/window-quit-home-dired-returns-to-dashboard ()
  "An old Dired home entry does not become a page after q."
  (let ((page (generate-new-buffer "quit-home-dired-page"))
        (home-dired (generate-new-buffer "quit-home-dired"))
        (home (file-name-as-directory (expand-file-name "~"))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer home-dired
            (setq-local major-mode 'dired-mode
                        default-directory home))
          (switch-to-buffer home-dired)
          (switch-to-buffer page)
          (set-window-prev-buffers (selected-window)
                                   (list (list home-dired 1 1)))
          (quit-window)
          (should (eq (window-buffer) (my/window-dashboard-buffer))))
      (dolist (buffer (list page home-dired))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-bury-last-page-opens-project-directory ()
  "The one-frame close action buries its page into its project directory."
  (let ((page (generate-new-buffer "bury-last-page"))
        (scratch (get-buffer-create "*scratch*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer scratch)
          (switch-to-buffer page)
          (set-window-prev-buffers (selected-window)
                                   (list (list scratch 1 1)))
          (my/delete-frame-dwim)
          (should (buffer-live-p page))
          (with-current-buffer (window-buffer)
            (should (derived-mode-p 'dired-mode))))
      (when (buffer-live-p page) (kill-buffer page)))))

(ert-deftest my/window-bury-returns-to-previous-page ()
  "Burying keeps the old buffer alive and restores the prior page."
  (let ((source (generate-new-buffer "bury-return-source"))
        (page (generate-new-buffer "bury-return-page")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (switch-to-buffer page)
          (my/window-bury-current-buffer)
          (should (buffer-live-p page))
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source page))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-close-select-key-keeps-picker-and-window-history ()
  "Cmd+Shift+W asks which buffer to kill, then restores its window."
  (let ((source (generate-new-buffer "close-key-source"))
        (page (generate-new-buffer "close-key-page"))
        prompted)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (switch-to-buffer page)
          (should (eq (key-binding (kbd "M-W")) #'persp-kill-buffer*))
          (should (eq (key-binding (kbd "C-x k")) #'persp-kill-buffer*))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       (setq prompted t)
                       (buffer-name page))))
            (call-interactively (key-binding (kbd "M-W"))))
          (should prompted)
          (should-not (buffer-live-p page))
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source page))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-close-picker-can-kill-another-buffer ()
  "The original Cmd+Shift+W picker can kill a different buffer."
  (let ((page (generate-new-buffer "close-picker-current"))
        (other (generate-new-buffer "close-picker-other"))
        prompted)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer other)
          (switch-to-buffer page)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args)
                       (setq prompted t)
                       (buffer-name other))))
            (call-interactively (key-binding (kbd "M-W"))))
          (should prompted)
          (should-not (buffer-live-p other))
          (should (eq (window-buffer) page)))
      (dolist (buffer (list page other))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-xwidget-after-kill ()
  (let ((normal (generate-new-buffer "normal-fallback-test"))
        (xwidget (generate-new-buffer "xwidget-fallback-test"))
        (victim (generate-new-buffer "victim-fallback-test")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer xwidget
            (setq-local major-mode 'xwidget-webkit-mode))
          ;; Make xwidget the most recent fallback, followed by NORMAL.
          (switch-to-buffer normal)
          (switch-to-buffer xwidget)
          (switch-to-buffer victim)
          (kill-buffer victim)
          (should (eq (window-buffer) normal)))
      (dolist (buffer (list normal xwidget victim))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-keeps-xwidget-for-explicit-navigation ()
  (let ((xwidget (generate-new-buffer "xwidget-navigation-test")))
    (unwind-protect
        (with-current-buffer xwidget
          (setq-local major-mode 'xwidget-webkit-mode)
          (should-not
           (my/window-skip-dashboard-and-xwidget-p nil xwidget nil)))
      (when (buffer-live-p xwidget)
        (kill-buffer xwidget)))))

(ert-deftest my/window-repair-candidate-respects-native-frame-predicate ()
  "Fallback repair must not resurrect a buffer excluded by this frame."
  (let ((closing (generate-new-buffer "repair-predicate-closing"))
        (foreign (generate-new-buffer "repair-predicate-foreign"))
        (allowed (generate-new-buffer "repair-predicate-allowed"))
        (frame (selected-frame))
        (original (frame-parameter nil 'buffer-predicate)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer closing)
          (set-window-prev-buffers
           (selected-window)
           (list (list foreign 1 1) (list allowed 1 1)))
          (set-frame-parameter
           frame 'buffer-predicate
           (lambda (buffer) (not (eq buffer foreign))))
          (should (eq (my/window--return-buffer
                       (selected-window) closing)
                      allowed)))
      (set-frame-parameter frame 'buffer-predicate original)
      (dolist (buffer (list closing foreign allowed))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-repair-candidate-skips-xwidget-history ()
  "Only the native exact popup return may restore a Noema xwidget page."
  (let ((closing (generate-new-buffer "repair-xwidget-closing"))
        (xwidget (generate-new-buffer "repair-xwidget-page"))
        (allowed (generate-new-buffer "repair-xwidget-allowed")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer xwidget
            (setq-local major-mode 'xwidget-webkit-mode))
          (switch-to-buffer closing)
          (set-window-prev-buffers
           (selected-window)
           (list (list xwidget 1 1) (list allowed 1 1)))
          (should (eq (my/window--return-buffer
                       (selected-window) closing)
                      allowed)))
      (dolist (buffer (list closing xwidget allowed))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-dashboard-on-quit ()
  "Quitting a popup must not resurrect the startup dashboard."
  (let ((source (generate-new-buffer "dashboard-fallback-source"))
        (dashboard (generate-new-buffer "*dashboard-fallback-test*"))
        (popup (generate-new-buffer "*dashboard-fallback-popup*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer source)
          (switch-to-buffer dashboard)
          (switch-to-buffer popup)
          (quit-window)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard popup))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-skips-dashboard-on-kill ()
  "Killing a terminal buffer must restore useful work, not dashboard."
  (let ((source (generate-new-buffer "dashboard-kill-source"))
        (dashboard (generate-new-buffer "*dashboard-kill-test*"))
        (terminal (generate-new-buffer "*dashboard-kill-terminal*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer source)
          (switch-to-buffer dashboard)
          (switch-to-buffer terminal)
          (kill-buffer terminal)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard terminal))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-fallback-keeps-dashboard-for-explicit-navigation ()
  "Explicit previous-buffer navigation can still reach the dashboard."
  (let ((dashboard (generate-new-buffer "*dashboard-navigation-test*")))
    (unwind-protect
        (with-current-buffer dashboard
          (setq-local major-mode 'dashboard-mode)
          (should-not
           (my/window-skip-dashboard-and-xwidget-p nil dashboard nil)))
      (when (buffer-live-p dashboard)
        (kill-buffer dashboard)))))

(ert-deftest my/window-portal-popup-exit-keeps-work-buffer ()
  "Closing a managed portal popup leaves its work window in place."
  (require 'init-portal-popup)
  (let ((source (generate-new-buffer "portal-popup-work-test"))
        (dashboard (generate-new-buffer "*portal-popup-dashboard-test*"))
        (popup (generate-new-buffer "*portal-popup-exit-test*"))
        (my/portal-popup-buffers nil)
        (my/portal-popup-current-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer dashboard
            (setq-local major-mode 'dashboard-mode))
          (switch-to-buffer dashboard)
          (switch-to-buffer source)
          (my/portal-popup-display-buffer popup)
          (should (eq (window-buffer) popup))
          (kill-buffer popup)
          (should (eq (window-buffer) source)))
      (dolist (buffer (list source dashboard popup))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/window-portal-popup-exit-closes-unselected-window ()
  "A popup closes when its buffer exits while another pane is selected."
  (require 'init-portal-popup)
  (let ((source (generate-new-buffer "portal-popup-other-work"))
        (popup (generate-new-buffer "*portal-popup-unselected-test*"))
        (my/portal-popup-buffers nil)
        (my/portal-popup-current-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (my/portal-popup-display-buffer popup)
          (let ((popup-window (selected-window)))
            (select-window (get-buffer-window source))
            (kill-buffer popup)
            (should-not (window-live-p popup-window))
            (should (eq (window-buffer) source))))
      (dolist (buffer (list source popup))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'init-windows-tests)

;;; init-windows-tests.el ends here
