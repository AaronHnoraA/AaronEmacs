;;; macos-boundary-tests.el --- Contracts of the macOS boundary -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; docs/design/macos-boundary.md.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'remote-fs)
(require 'os/macos/system)
(require 'dired)

(define-derived-mode macos-boundary-test-shell-mode fundamental-mode "Test")
(define-derived-mode macos-boundary-test-child-mode
  macos-boundary-test-shell-mode "Child")

(defmacro macos-boundary-test--with-buffer (mode &rest body)
  "Run BODY in a selected temporary buffer in MODE."
  (declare (indent 1))
  `(with-temp-buffer
     (,mode)
     (set-window-buffer (selected-window) (current-buffer))
     ,@body))

(ert-deftest macos-boundary-c2-open-and-quit-present-first ()
  (let* ((order nil)
         (+macos-present-hook (list (lambda () (push 'present order)))))
    (cl-letf (((symbol-function 'save-buffers-kill-emacs)
               (lambda (&rest _) (push 'quit order))))
      (+macos/system-quit))
    (should (equal order '(quit present))))
  (should (advice-member-p #'+macos--present-before-open-a 'ns-find-file))
  (should (eq (keymap-lookup global-map "<ns-power-off>") #'+macos/system-quit)))

(ert-deftest macos-boundary-c4-terminal-gets-paths-in-order ()
  (let* ((directory (make-temp-file "macos boundary 中文" t))
         (first (expand-file-name "a b.txt" directory))
         (second (expand-file-name "b'c.txt" directory))
         (received nil)
         (+macos-file-drop-handlers
          `((macos-boundary-test-shell-mode
             . ,(lambda (files) (setq received files))))))
    (unwind-protect
        (macos-boundary-test--with-buffer macos-boundary-test-child-mode
          (write-region "" nil first)
          (write-region "" nil second)
          (let ((buffer (current-buffer)))
            (dnd-handle-multiple-urls
             (selected-window)
             (list (concat "file:" first) (concat "file:" second)) 'private)
            (should (eq (window-buffer) buffer))
            (should (equal received (list first second)))))
      (delete-directory directory t))))

(ert-deftest macos-boundary-c4-paths-are-one-quoted-line ()
  (should (equal (+macos-drop-text '("/a/b.txt")) "/a/b.txt "))
  (should (equal (+macos-drop-text '("/a b" "/c'd"))
                 "'/a b' '/c'\\''d' "))
  (should-not (string-search "\n" (+macos-drop-text '("/a" "/b"))))
  (should (equal (+macos-quote-path "/文档/x") "/文档/x")))

(ert-deftest macos-boundary-c5-shell-elsewhere-gets-the-files-first ()
  (with-temp-buffer
    (let* (received
           (+macos-drop-receive-function
            (lambda (files method directory)
              (setq received (list files method directory)))))
      (setq default-directory "/fs:aaron-pc:/home/a/")
      (cl-letf (((symbol-function 'remote-client-file-name) #'ignore)
                ((symbol-function 'y-or-n-p) #'ignore))
        (should-error (+macos-drop-insert-paths '("/l/a b")) :type 'user-error)
        (should (= (buffer-size) 0)))
      (cl-letf (((symbol-function 'remote-client-file-name) #'ignore)
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (+macos-drop-insert-paths '("/l/a b"))
        (should (equal (buffer-string) "'a b' "))
        (should (equal received '(("/l/a b") copy "/fs:aaron-pc:/home/a/")))))
    (erase-buffer)
    (cl-letf (((symbol-function 'remote-client-file-name) #'identity))
      (insert "$ ")
      (goto-char (point-min))
      (+macos-drop-insert-paths '("/a"))
      (should (equal (buffer-string) "$ /a ")))))

(ert-deftest macos-boundary-c9-drop-opens-without-asking ()
  (let ((file (make-temp-file "macos-boundary"))
        (+macos-file-drop-handlers nil)
        opened)
    (unwind-protect
        (macos-boundary-test--with-buffer fundamental-mode
          (cl-letf (((symbol-function 'aaron/open-file)
                     (lambda (name &rest _)
                       (setq opened (list name default-directory))))
                    ((symbol-function 'read-multiple-choice)
                     (lambda (&rest _) (error "Asked")))
                    ((symbol-function 'remote-client-file-name) #'identity))
            (setq default-directory "/fs:aaron-pc:/home/")
            (let ((+macos-file-drop-ask 'never))
              (dnd-handle-multiple-urls
               (selected-window) (list (concat "file:" file)) 'private)))
          (should (equal opened (list file temporary-file-directory))))
      (delete-file file))))

(ert-deftest macos-boundary-c9-folder-opens-in-dired ()
  (let ((directory (make-temp-file "macos-boundary" t)) opened)
    (unwind-protect
        (cl-letf (((symbol-function 'dired) (lambda (name) (setq opened name))))
          (+macos--drop-open (list directory))
          (should (equal opened directory)))
      (delete-directory directory))))

(ert-deftest macos-boundary-c10-when-a-drop-asks ()
  (cl-letf (((symbol-function 'remote-client-file-name) #'identity))
    (with-temp-buffer
      (let ((+macos-file-drop-ask 'auto) (+macos--drop-operations nil))
        (should-not (+macos--drop-ask-p '("/a")))
        (should (+macos--drop-ask-p (make-list 9 "/a"))))
      (let ((+macos-file-drop-ask 'always))
        (should (+macos--drop-ask-p '("/a"))))
      (let ((+macos-file-drop-ask 'never)
            (+macos--drop-operations '(ns-drag-operation-copy)))
        (should (+macos--drop-ask-p '("/a"))))
      (let ((+macos-file-drop-ask 'never)
            (+macos--drop-operations
             '(ns-drag-operation-copy ns-drag-operation-generic)))
        (should-not (+macos--drop-ask-p '("/a"))))))
  (cl-letf (((symbol-function 'remote-client-file-name) #'ignore))
    (with-temp-buffer
      (let ((+macos-file-drop-ask 'auto) (+macos--drop-operations nil))
        (should (+macos--drop-ask-p '("/a")))))))

(ert-deftest macos-boundary-c10-actions-fit-the-place ()
  (cl-letf (((symbol-function 'remote-client-file-name) #'identity))
    (with-temp-buffer
      (should (equal (mapcar #'car (+macos--drop-actions '("/a")))
                     '(?o ?c ?m ?l ?i ?r ?q))))
    (with-temp-buffer
      (setq buffer-read-only t)
      (should (equal (mapcar #'car (+macos--drop-actions '("/a")))
                     '(?o ?c ?m ?l ?q)))))
  (cl-letf (((symbol-function 'remote-client-file-name) #'ignore))
    (with-temp-buffer
      (should (equal (mapcar #'car (+macos--drop-actions '("/a")))
                     '(?o ?c ?i ?q))))))

(ert-deftest macos-boundary-c10-copy-goes-to-the-file-manager ()
  (let* (received
         (+macos-drop-receive-function
          (lambda (&rest arguments) (setq received arguments))))
    (with-temp-buffer
      (setq default-directory "/here/")
      (+macos--drop-do ?m '("/a"))
      (should (equal received '(("/a") move "/here/"))))))

(ert-deftest macos-boundary-c11-file-prompt-moves-to-the-drop ()
  (let ((directory (make-temp-file "macos-boundary" t))
        (minibuffer (get-buffer-create " *macos-boundary-mini*")))
    (unwind-protect
        (with-current-buffer minibuffer
          (insert "typed")
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     #'selected-window)
                    ((symbol-function 'delete-minibuffer-contents)
                     #'erase-buffer)
                    ((symbol-function 'run-at-time) #'ignore))
            (set-window-buffer (selected-window) minibuffer)
            (+macos--drop-minibuffer-file directory)
            (should (equal (buffer-string)
                           (file-name-as-directory directory)))))
      (kill-buffer minibuffer)
      (delete-directory directory))))

(ert-deftest macos-boundary-c9-side-window-keeps-what-it-shows ()
  (let ((main (selected-window)) target)
    (cl-letf (((symbol-function 'window-dedicated-p) (lambda (&rest _) t))
              ((symbol-function 'get-mru-window)
               (lambda (&rest _) (setq target main)))
              ((symbol-function 'select-window) #'ignore))
      (+macos--drop-open-window))
    (should (eq target main))))

(ert-deftest macos-boundary-c6-url-uses-open-route ()
  (let (opened)
    (macos-boundary-test--with-buffer fundamental-mode
      (cl-letf (((symbol-function 'aaron/open-url)
                 (lambda (url &rest _) (setq opened url))))
        (dnd-handle-multiple-urls
         (selected-window) (list "https://example.com/a") 'private)))
    (should (equal opened "https://example.com/a"))
    (should-not (bound-and-true-p url-handler-mode))))

(ert-deftest macos-boundary-c7-actions-run-on-this-machine ()
  (let (directory)
    (with-temp-buffer
      (setq default-directory "/fs:aaron-pc:/home/")
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _) (setq directory default-directory) 0)))
        (should (+macos-call "open" "-R" "/a"))))
    (should (equal directory temporary-file-directory)))
  (cl-letf (((symbol-function 'remote-client-file-name) #'ignore))
    (should-error (+macos-client-path "/fs:aaron-pc:/a") :type 'user-error)))

(defmacro macos-boundary-test--notify (here &rest body)
  "Run BODY with an empty history and the user HERE or away."
  (declare (indent 1))
  `(let ((aaron-notify-history nil) (aaron-notify-unread 0)
         (aaron-notify-unread-functions nil) sent)
     (cl-letf (((symbol-function 'aaron-notify--user-here-p) (lambda () ,here)))
       (let ((aaron-notify-system-function (lambda (entry) (push entry sent))))
         ,@body))))

(ert-deftest macos-boundary-c12-notification-lands-where-the-user-is ()
  (macos-boundary-test--notify t
    (aaron-notify "Here" :body "b")
    (should-not sent)
    (should (= aaron-notify-unread 0))
    (should (= (length aaron-notify-history) 1)))
  (macos-boundary-test--notify nil
    (aaron-notify "Away")
    (should (= (length sent) 1))
    (should (= aaron-notify-unread 1))
    (should (= (length aaron-notify-history) 1))))

(ert-deftest macos-boundary-c12-batch-never-reaches-the-system ()
  (let* ((noninteractive t) sent
         (aaron-notify-history nil) (aaron-notify-unread 0)
         (aaron-notify-system-function (lambda (entry) (push entry sent))))
    (aaron-notify "Test run")
    (should-not sent)
    (should (= aaron-notify-unread 0))))

(ert-deftest macos-boundary-c12-same-id-replaces ()
  (macos-boundary-test--notify nil
    (aaron-notify "One" :id "job")
    (aaron-notify "Two" :id "job")
    (should (equal (mapcar #'aaron-notify-entry-title aaron-notify-history)
                   '("Two")))))

(ert-deftest macos-boundary-c12-history-is-bounded ()
  (macos-boundary-test--notify t
    (dotimes (_ (+ aaron-notify-history-limit 5))
      (aaron-notify "x"))
    (should (= (length aaron-notify-history) aaron-notify-history-limit))))

(ert-deftest macos-boundary-c13-click-runs-the-action ()
  (macos-boundary-test--notify nil
    (let* (ran
           (id (aaron-notify "T" :action (lambda () (setq ran t)))))
      (aaron-notify-activate id)
      (should ran))
    (cl-letf (((symbol-function 'aaron/notifications)
               (lambda () (setq sent 'history))))
      (aaron-notify-activate (aaron-notify "Plain"))
      (should (eq sent 'history)))))

(ert-deftest macos-boundary-c13-clicked-ids-present-emacs-first ()
  (let* ((order nil)
         (+macos-present-hook (list (lambda () (push 'present order)))))
    (cl-letf (((symbol-function 'aaron-notify-activate)
               (lambda (id) (push id order))))
      (+macos--notify-clicked nil "a\nb\n"))
    (should (equal (reverse order) '(present "a" "b")))))

(ert-deftest macos-boundary-c8-script-needs-no-installed-tool ()
  (let (command)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buffer program &rest arguments)
                 (setq command (cons program arguments)))))
      (+macos--notify-script
       (aaron-notify-entry--create :title "T" :body "B")))
    (should (equal (car command) "osascript"))
    (should (equal (last command 2) '("T" "B")))))

(ert-deftest macos-boundary-c8-send-keeps-the-old-interface ()
  (macos-boundary-test--notify nil
    (notify-send :title "T" :body "B" :urgency 'critical)
    (should (eq (aaron-notify-entry-level (car sent)) 'error))))

;;; macos-boundary-tests.el ends here
