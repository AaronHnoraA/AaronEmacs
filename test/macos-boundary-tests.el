;;; macos-boundary-tests.el --- Contracts of the macOS boundary -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; docs/design/macos-boundary.md.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'remote-fs)
(require 'os/macos/system)

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

(ert-deftest macos-boundary-c5-other-target-refuses-paths ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'remote-client-file-name) #'ignore))
      (should-error (+macos-drop-insert-paths '("/a")) :type 'user-error)
      (should (= (buffer-size) 0)))
    (cl-letf (((symbol-function 'remote-client-file-name) #'identity))
      (insert "$ ")
      (goto-char (point-min))
      (+macos-drop-insert-paths '("/a"))
      (should (equal (buffer-string) "$ /a ")))))

(ert-deftest macos-boundary-default-drop-visits-the-file ()
  (let ((file (make-temp-file "macos-boundary"))
        (+macos-file-drop-handlers nil)
        visited)
    (unwind-protect
        (macos-boundary-test--with-buffer fundamental-mode
          (cl-letf (((symbol-function 'find-file)
                     (lambda (name &rest _) (setq visited name))))
            (dnd-handle-multiple-urls
             (selected-window) (list (concat "file:" file)) 'private))
          (should (equal visited file)))
      (delete-file file))))

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

(ert-deftest macos-boundary-c8-notification-needs-no-installed-tool ()
  (let (command)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buffer program &rest arguments)
                 (setq command (cons program arguments)))))
      (notify-send :title "T" :body "B"))
    (should (equal (car command) "osascript"))
    (should (equal (last command 2) '("T" "B")))))

;;; macos-boundary-tests.el ends here
