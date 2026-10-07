;;; init-browser-tests.el --- Browser integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-browser)

(defmacro my/xwidget-test--with-mocks (&rest body)
  "Run BODY with mocked xwidget primitives."
  (declare (indent 0))
  `(let ((my/xwidget--sessions (make-hash-table :test #'equal))
         (my/xwidget--session-counter 0))
     (cl-letf (((symbol-function 'xwidget-webkit-browse-url)
                (lambda (_url &optional _new-session)
                  (switch-to-buffer (generate-new-buffer "*mock-xwidget*"))))
               ((symbol-function 'xwidget-webkit-current-session)
                (lambda () (current-buffer)))
               ((symbol-function 'xwidget-webkit-uri)
                (lambda (buffer)
                  (with-current-buffer buffer
                    my/xwidget-session-url)))
               ((symbol-function 'xwidget-webkit-goto-uri)
                (lambda (buffer url)
                  (with-current-buffer buffer
                    (setq-local my/xwidget-session-url url))))
               ((symbol-function 'xwidget-buffer)
                (lambda (buffer) buffer)))
       ,@body)))

(ert-deftest my/xwidget-open-url-reuses-stable-id ()
  (my/xwidget-test--with-mocks
    (let* ((buf1 (my/xwidget-open-url
                  "example.com"
                  :id "demo"
                  :display 'current))
           (buf2 (my/xwidget-open-url
                  "example.org"
                  :id "demo"
                  :display 'none)))
      (unwind-protect
          (progn
            (should (eq buf1 buf2))
            (should (buffer-live-p buf1))
            (should (equal (my/xwidget-current-url buf1)
                           "https://example.org")))
        (when (buffer-live-p buf1)
          (kill-buffer buf1))))))

(ert-deftest my/xwidget-open-url-keeps-distinct-ids-separate ()
  (my/xwidget-test--with-mocks
    (let (buffers)
      (unwind-protect
          (let ((buf1 (my/xwidget-open-url
                       "example.com"
                       :id "one"
                       :display 'current))
                (buf2 (my/xwidget-open-url
                       "example.org"
                       :id "two"
                       :display 'current)))
            (setq buffers (list buf1 buf2))
            (should-not (eq buf1 buf2))
            (should (buffer-live-p buf1))
            (should (buffer-live-p buf2))
            (should (eq (my/xwidget-session-buffer "one") buf1))
            (should (eq (my/xwidget-session-buffer "two") buf2)))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest my/xwidget-open-url-force-new-replaces-old-buffer ()
  (my/xwidget-test--with-mocks
    (let (buf2)
      (let ((buf1 (my/xwidget-open-url
                   "example.com"
                   :id "demo"
                   :display 'current)))
        (setq buf2 (my/xwidget-open-url
                    "example.org"
                    :id "demo"
                    :display 'current
                    :force-new t))
        (unwind-protect
            (progn
              (should-not (eq buf1 buf2))
              (should-not (buffer-live-p buf1))
              (should (buffer-live-p buf2))
              (should (eq (my/xwidget-session-buffer "demo") buf2)))
          (when (buffer-live-p buf2)
            (kill-buffer buf2)))))))

(ert-deftest my/xwidget-session-registry-rejects-reassigned-buffer ()
  (my/xwidget-test--with-mocks
    (let ((buffer (my/xwidget-open-url "example.com" :id "old"
                                       :display 'current)))
      (unwind-protect
          (progn
            (my/xwidget--record-buffer buffer "new" "https://example.org")
            (should-not (my/xwidget-session-buffer "old"))
            (should (eq (my/xwidget-session-buffer "new") buffer))
            ;; A stale old entry must not be removed by a later buffer kill.
            (puthash "old" buffer my/xwidget--sessions)
            (kill-buffer buffer)
            (should (eq (gethash "old" my/xwidget--sessions) buffer))
            (should-not (gethash "new" my/xwidget--sessions)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest my/xwidget-delayed-focus-does-not-steal-another-window ()
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *xwidget-focus*"))
           (other (generate-new-buffer " *xwidget-other*"))
           (window (selected-window))
           (my/xwidget-auto-focus-on-load t)
           focused)
      (unwind-protect
          (progn
            (set-window-buffer window buffer)
            (cl-letf (((symbol-function 'my/xwidget-focus)
                       (lambda (&optional _buffer) (setq focused t))))
              (my/xwidget--focus-if-selected buffer window)
              (should focused)
              (setq focused nil)
              (set-window-buffer window other)
              (my/xwidget--focus-if-selected buffer window)
              (should-not focused)))
        (kill-buffer buffer)
        (kill-buffer other)))))

(ert-deftest my/xwidget-webpage-navigation-right-click-opens-noema-buffer-menu ()
  (let ((buttons (my/xwidget--header-browser-buttons))
        events)
    (cl-letf (((symbol-function 'my/xwidget--select-event-window) #'ignore)
              ((symbol-function 'my/xwidget-noema-buffer-menu)
               (lambda (event) (push event events))))
      (dolist (button (seq-take buttons 2))
        (let* ((map (get-text-property 0 'local-map button))
               (right-click (lookup-key map [header-line mouse-3])))
          (should (functionp right-click))
          (funcall right-click 'clicked))))
    (should (equal events '(clicked clicked)))))

(ert-deftest my/xwidget-update-buffer-name-prefers-title ()
  (let ((buffer (generate-new-buffer "*xwidget*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local my/xwidget-session-url "https://example.com/path/page.html")
          (setq-local my/xwidget-session-title "Readable Page")
          (my/xwidget-update-buffer-name buffer)
          (should (string-match-p "\\`\\*xwidget: Readable Page\\*" (buffer-name buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/xwidget-update-buffer-name-falls-back-to-url ()
  (let ((buffer (generate-new-buffer "*xwidget*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local my/xwidget-session-url "https://example.com/docs/index.html")
          (my/xwidget-update-buffer-name buffer)
          (should (string-match-p "\\`\\*xwidget: index\\.html\\*" (buffer-name buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/xwidget-update-buffer-name-skips-aaronnote-owned-buffer ()
  (let ((buffer (generate-new-buffer "*Noema: note.md*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local my/noema--xwidget-forced-name "*Noema: note.md*")
          (setq-local my/xwidget-session-title "Browser Title")
          (my/xwidget-update-buffer-name buffer)
          (should (equal (buffer-name buffer) "*Noema: note.md*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'init-browser-tests)
;;; init-browser-tests.el ends here
