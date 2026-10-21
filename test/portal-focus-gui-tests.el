;;; portal-focus-gui-tests.el --- Native focus across Emacs panes -*- lexical-binding: t; -*-

(require 'portal-tests)
(require 'init-portal)
(require 'windmove)
(require 'noema-portal)
(require 'noema-xwidget-keys)

(declare-function portal-test-native-post-key "test-terminal-input"
                  (code flags text &optional windowless))
(declare-function portal-test-native-key "test-terminal-input"
                  (code flags text &optional windowless))
(declare-function portal-test-native-click "test-terminal-input" (x y))
(declare-function portal-test-native-target-frame "test-terminal-input" (title))
(declare-function portal-test-native-focus-target "test-terminal-input" ())

(ert-deftest my/portal-gui-focus-follows-web-and-ordinary-panes ()
  "Two native pages and an ordinary pane hand the keyboard to one another."
  (portal-test--with-web
    (select-frame-set-input-focus (selected-frame))
    (portal-open-url "data:text/html,<title>Focus one</title><input autofocus>")
    (let* ((first-window (selected-window))
           (first-target (portal-view-target))
           (ordinary (generate-new-buffer " *portal-focus-ordinary*")))
      (unwind-protect
          (progn
            (portal-test--wait-for-page "Focus one" first-target)
            (let* ((ordinary-window (split-window-right))
                   (second-window (split-window ordinary-window nil 'right)))
              (set-window-buffer ordinary-window ordinary)
              (select-window second-window)
              (portal-open-url "data:text/html,<title>Focus two</title><input autofocus>")
              (let ((second-target (portal-view-target)))
                (portal-test--wait-for-page "Focus two" second-target)
                (my/portal--focus-selected-window (selected-frame) t)
                (should (portal-input--action "focused" second-target))
                (select-window first-window)
                (my/portal--focus-selected-window (selected-frame) t)
                (should (portal-input--action "focused" first-target))
                (should-not (portal-input--action "focused" second-target))
                (select-window ordinary-window)
                (my/portal--focus-selected-window (selected-frame) t)
                (should-not (portal-input--action "focused" first-target))
                (should-not (portal-input--action "focused" second-target))
                (select-window second-window)
                (my/portal--focus-selected-window (selected-frame) t)
                (should (portal-input--action "focused" second-target)))))
        (when (buffer-live-p ordinary) (kill-buffer ordinary))))))

(ert-deftest my/portal-gui-switch-restores-clicked-web-input ()
  "A pane switch returns keystrokes to the field selected by a real click."
  (skip-unless (and (fboundp 'portal-test-native-click)
                    (fboundp 'portal-test-native-key)))
  (portal-test--with-web
    (select-frame-set-input-focus (selected-frame))
    (portal-open-url
     "data:text/html,<title>Left input</title><input spellcheck=false oninput=\"document.title='Left:'+this.value\">")
    (setq-local my/noema--client-id "gui-click-left")
    (let ((left (selected-window))
          (left-target (portal-view-target)))
      (portal-test--wait-for-page "Left input" left-target)
      (should (portal-view--with-frame-title
               (selected-frame) #'portal-test-native-target-frame))
      (unwind-protect
          (progn
            (should (portal-test-native-focus-target))
            (redisplay t)
            (should (portal-test-native-click 15 20))
            (should (portal-test-native-key 0 0 "a"))
            (portal-test--wait-for-page "Left:a" left-target)
            (select-window (split-window-right))
            (portal-open-url "data:text/html,<title>Right input</title><input spellcheck=false>")
            (setq-local my/noema--client-id "gui-click-right")
            (let ((right-target (portal-view-target)))
              (portal-test--wait-for-page "Right input" right-target)
              (my/portal--focus-selected-window (selected-frame) t)
              (should (portal-input--action "focused" right-target))
              (select-window left)
              (my/portal--focus-selected-window (selected-frame) t)
              (should (portal-input--action "focused" left-target))
              (should (portal-test-native-key 11 0 "b"))
              (portal-test--wait-for-page "Left:ab" left-target)))
        (portal-test-native-target-frame nil)))))

(ert-deftest my/portal-gui-command-left-reaches-other-web-pane ()
  "A native Command+Left reaches Emacs, then focuses the selected page."
  (skip-unless (fboundp 'portal-test-native-post-key))
  (portal-test--with-web
    (select-frame-set-input-focus (selected-frame))
    (portal-open-url
     "data:text/html,<title>Left page</title><input autofocus spellcheck=false oninput=\"document.title='Left:'+this.value\">")
    (setq-local my/noema--client-id "gui-focus-left")
    (let ((left (selected-window))
          (left-target (portal-view-target))
          (mac-command-modifier 'meta))
      (portal-test--wait-for-page "Left page" left-target)
      (should (portal-view--with-frame-title
               (selected-frame) #'portal-test-native-target-frame))
      (should (portal-test-native-focus-target))
      (should (portal-test-native-click 15 20))
      (should (portal-test-native-key 0 0 "a"))
      (portal-test--wait-for-page "Left:a" left-target)
      (select-window (split-window-right))
      (portal-open-url "data:text/html,<title>Right page</title><input autofocus>")
      (setq-local my/noema--client-id "gui-focus-right")
      (let ((right-target (portal-view-target)))
        (portal-test--wait-for-page "Right page" right-target)
        (my/portal--focus-selected-window (selected-frame) t)
        (should (portal-input--action "focused" right-target))
        (should (portal-view--with-frame-title
                 (selected-frame) #'portal-test-native-target-frame))
        (unwind-protect
            (let ((old-binding (lookup-key (current-global-map) (kbd "M-<left>"))))
              (unwind-protect
                  (progn
                    (global-set-key (kbd "M-<left>") #'windmove-left)
                    (should (portal-test-native-focus-target))
                    ;; The native view returns this real AppKit event to Emacs.
                    (portal-test-native-post-key 123 (ash 1 20) (string #xf702))
                    (let (keys received)
                      (with-timeout
                          (5 (ert-fail (format "Command+Left did not reach Emacs; saw %S"
                                               (nreverse received))))
                        (while (not (equal keys (kbd "M-<left>")))
                          (setq keys (read-key-sequence-vector nil))
                          (push keys received)))
                      (should (eq (key-binding keys) #'windmove-left))
                      (command-execute (key-binding keys)))
                    (my/portal--focus-after-command)
                    ;; A selected mode line is insufficient: the WebKit field
                    ;; must receive a real key after the direction command.
                    (should (portal-test-native-key 11 0 "b"))
                    (portal-test--wait-for-page "Left:ab" left-target))
                (define-key (current-global-map) (kbd "M-<left>") old-binding)))
          (portal-test-native-target-frame nil))
        (should (eq (selected-window) left))
        (should (portal-input--action "focused" left-target))
        (should-not (portal-input--action "focused" right-target))))))

(provide 'portal-focus-gui-tests)
;;; portal-focus-gui-tests.el ends here
