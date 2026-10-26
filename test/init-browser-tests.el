;;; init-browser-tests.el --- Browser integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'app/browser/config)

(defmacro +xwidget-test--with-mocks (&rest body)
  "Run BODY with mocked xwidget primitives."
  (declare (indent 0))
  `(let ((+xwidget--sessions (make-hash-table :test #'equal))
         (+xwidget--session-counter 0))
     (cl-letf (((symbol-function 'xwidget-webkit-browse-url)
                (lambda (_url &optional _new-session)
                  (switch-to-buffer (generate-new-buffer "*mock-xwidget*"))))
               ((symbol-function 'xwidget-webkit-current-session)
                (lambda () (current-buffer)))
               ((symbol-function 'xwidget-webkit-uri)
                (lambda (buffer)
                  (with-current-buffer buffer
                    +xwidget-session-url)))
               ((symbol-function 'xwidget-webkit-goto-uri)
                (lambda (buffer url)
                  (with-current-buffer buffer
                    (setq-local +xwidget-session-url url))))
               ((symbol-function 'xwidget-buffer)
                (lambda (buffer) buffer)))
       ,@body)))

(ert-deftest +xwidget-open-url-reuses-stable-id ()
  (+xwidget-test--with-mocks
    (let* ((buf1 (+xwidget/open-url
                  "example.com"
                  :id "demo"
                  :display 'current))
           (buf2 (+xwidget/open-url
                  "example.org"
                  :id "demo"
                  :display 'none)))
      (unwind-protect
          (progn
            (should (eq buf1 buf2))
            (should (buffer-live-p buf1))
            (should (equal (+xwidget-current-url buf1)
                           "https://example.org")))
        (when (buffer-live-p buf1)
          (kill-buffer buf1))))))

(ert-deftest +xwidget-open-url-keeps-distinct-ids-separate ()
  (+xwidget-test--with-mocks
    (let (buffers)
      (unwind-protect
          (let ((buf1 (+xwidget/open-url
                       "example.com"
                       :id "one"
                       :display 'current))
                (buf2 (+xwidget/open-url
                       "example.org"
                       :id "two"
                       :display 'current)))
            (setq buffers (list buf1 buf2))
            (should-not (eq buf1 buf2))
            (should (buffer-live-p buf1))
            (should (buffer-live-p buf2))
            (should (eq (+xwidget-session-buffer "one") buf1))
            (should (eq (+xwidget-session-buffer "two") buf2)))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest +xwidget-open-url-force-new-replaces-old-buffer ()
  (+xwidget-test--with-mocks
    (let (buf2)
      (let ((buf1 (+xwidget/open-url
                   "example.com"
                   :id "demo"
                   :display 'current)))
        (setq buf2 (+xwidget/open-url
                    "example.org"
                    :id "demo"
                    :display 'current
                    :force-new t))
        (unwind-protect
            (progn
              (should-not (eq buf1 buf2))
              (should-not (buffer-live-p buf1))
              (should (buffer-live-p buf2))
              (should (eq (+xwidget-session-buffer "demo") buf2)))
          (when (buffer-live-p buf2)
            (kill-buffer buf2)))))))

(ert-deftest +xwidget-session-registry-rejects-reassigned-buffer ()
  (+xwidget-test--with-mocks
    (let ((buffer (+xwidget/open-url "example.com" :id "old"
                                       :display 'current)))
      (unwind-protect
          (progn
            (+xwidget--record-buffer buffer "new" "https://example.org")
            (should-not (+xwidget-session-buffer "old"))
            (should (eq (+xwidget-session-buffer "new") buffer))
            ;; A stale old entry must not be removed by a later buffer kill.
            (puthash "old" buffer +xwidget--sessions)
            (kill-buffer buffer)
            (should (eq (gethash "old" +xwidget--sessions) buffer))
            (should-not (gethash "new" +xwidget--sessions)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +xwidget-delayed-focus-does-not-steal-another-window ()
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *xwidget-focus*"))
           (other (generate-new-buffer " *xwidget-other*"))
           (window (selected-window))
           (+xwidget-auto-focus-on-load t)
           focused)
      (unwind-protect
          (progn
            (set-window-buffer window buffer)
            (cl-letf (((symbol-function '+xwidget/focus)
                       (lambda (&optional _buffer) (setq focused t))))
              (+xwidget--focus-if-selected buffer window)
              (should focused)
              (setq focused nil)
              (set-window-buffer window other)
              (+xwidget--focus-if-selected buffer window)
              (should-not focused)))
        (kill-buffer buffer)
        (kill-buffer other)))))

(ert-deftest +xwidget-header-combines-history-and-popup ()
  (let ((buttons (+xwidget--header-browser-buttons))
        events history opened)
    (cl-letf (((symbol-function '+xwidget--select-event-window) #'ignore)
              ((symbol-function '+xwidget/show-popup)
               (lambda () (interactive) (setq opened t)))
              ((symbol-function '+xwidget/noema-buffer-menu)
               (lambda (event) (push event events)))
              ((symbol-function 'easy-menu-create-menu)
               (lambda (_title items) (setq history items)))
              ((symbol-function 'popup-menu) #'ignore))
      (let* ((history-map (get-text-property 0 'local-map (car buttons)))
             (popup-map (get-text-property 0 'local-map (cadr buttons))))
        (should (string-match-p "hist" (car buttons)))
        (should (string-match-p "pop" (cadr buttons)))
        (funcall (lookup-key history-map [header-line mouse-3]) 'clicked)
        (should (eq (aref (car history) 1) #'+xwidget/back))
        (should (eq (aref (cadr history) 1) #'+xwidget/forward))
        (funcall (lookup-key popup-map [header-line mouse-1]) 'clicked)
        (funcall (lookup-key popup-map [header-line mouse-3]) 'clicked)))
    (should opened)
    (should (equal events '(clicked)))))

(ert-deftest +xwidget-update-buffer-name-prefers-title ()
  (let ((buffer (generate-new-buffer "*xwidget*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local +xwidget-session-url "https://example.com/path/page.html")
          (setq-local +xwidget-session-title "Readable Page")
          (+xwidget-update-buffer-name buffer)
          (should (string-match-p "\\`\\*xwidget: Readable Page\\*" (buffer-name buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +xwidget-update-buffer-name-falls-back-to-url ()
  (let ((buffer (generate-new-buffer "*xwidget*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local +xwidget-session-url "https://example.com/docs/index.html")
          (+xwidget-update-buffer-name buffer)
          (should (string-match-p "\\`\\*xwidget: index\\.html\\*" (buffer-name buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +xwidget-update-buffer-name-skips-noema-owned-buffer ()
  (let ((buffer (generate-new-buffer "*Noema: note.md*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq-local +noema--xwidget-forced-name "*Noema: note.md*")
          (setq-local +xwidget-session-title "Browser Title")
          (+xwidget-update-buffer-name buffer)
          (should (equal (buffer-name buffer) "*Noema: note.md*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'init-browser-tests)
;;; init-browser-tests.el ends here
