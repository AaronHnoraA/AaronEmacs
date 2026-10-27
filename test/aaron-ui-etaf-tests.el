;;; aaron-ui-etaf-tests.el --- Aaron ETAF integration checks -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'aaron-ui-etaf)
(require 'aaron-ui-docs)
(require 'aaron-ui-float)
(require 'aaron-ui-activity)

(etaf-define-component aaron-ui-etaf-test-host (&key surface)
  "Mount the preview under SURFACE as a floating host would."
  :setup (etaf-surface-provide surface)
  :view (aaron-ui-etaf-preview-page))

(ert-deftest aaron-ui-etaf-theme-follows-every-aaron-variant ()
  "Catalog controls and the View background use the selected Aaron palette."
  (dolist (variant '(wave dragon lotus))
    (let ((theme (aaron-ui-etaf-theme variant)))
      (should (equal (plist-get theme :bgcolor)
                     (aaron-ui-color 'bg-base nil variant)))
      (should (equal (plist-get theme :ui-button-primary-bg)
                     (aaron-ui-color 'accent-cyan nil variant)))
      (should (equal (plist-get theme :ui-panel-bg)
                     (aaron-ui-color 'bg-panel nil variant)))
      (should (eq (plist-get theme :ui-mode)
                  (if (eq variant 'lotus) 'light 'dark))))))

(ert-deftest aaron-ui-etaf-open-pages-read-updated-theme ()
  "A mounted View's shared Theme changes when the Aaron variant changes."
  (let ((previous-variant aaron-ui-current-variant)
        (ref (aaron-ui-etaf-theme-ref)))
    (unwind-protect
        (dolist (variant '(lotus wave))
          (setq aaron-ui-current-variant variant)
          (aaron-ui-etaf--refresh-theme)
          (should (equal (plist-get (etaf-value ref) :ui-panel-bg)
                         (aaron-ui-color 'bg-panel nil variant))))
      (setq aaron-ui-current-variant previous-variant)
      (aaron-ui-etaf--refresh-theme))))

(ert-deftest aaron-ui-etaf-panel-chrome-uses-the-view-theme ()
  "A posframe host receives the same Theme ref as its child controls."
  (let* ((surface (etaf-surface-create))
         (buffer (etaf-mount
                  "*Aaron Surface Check*"
                  (etaf-view (aaron-ui-etaf-test-host :surface surface)))))
    (unwind-protect
        (should (eq (etaf-surface-theme surface)
                    (aaron-ui-etaf-theme-ref)))
      (kill-buffer buffer))))

(ert-deftest aaron-ui-config-manual-renders-and-scrolls ()
  "The Config manual has a contents list and a scrollable document body."
  (let* ((file (expand-file-name "docs/design/config-management.md"
                                 user-emacs-directory))
         (document (etaf-ui-markdown-parse
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
         (entry (etaf-ref (list :document document :file file
                                :title "我的配置文档"
                                :actions (list (cons "Customize" #'ignore)))))
         (buffer (etaf-mount
                  "*Aaron Config Manual Test*"
                  (etaf-view (aaron-ui-docs-page :entry entry))
                  '(:viewport-width 1200 :viewport-height 32))))
    (unwind-protect
        (with-current-buffer buffer
          (should (string-match-p "目录" (buffer-string)))
          (should (string-match-p "Config" (buffer-string)))
          (should (string-match-p "Customize" (buffer-string)))
          (let ((runtime (etaf-runtime-for-buffer buffer)))
            (should (> (etaf-scroll-to runtime 'aaron-ui-docs-scroll 'end) 0))))
      (kill-buffer buffer))))

(ert-deftest aaron-ui-activity-long-page-scrolls-inside-viewport ()
  "A long Activity page scrolls within a short floating viewport."
  (let* ((rows (cl-loop for index below 30
                        collect (list :ref (intern (format "activity-row-%d" index))
                                      :kind 'service :badge "run"
                                      :title (format "Process %d" index)
                                      :detail "Command")))
         (state (etaf-ref (list :summary "30 processes"
                                :sections (list (list :key 'services
                                                      :title "Services"
                                                      :rows rows)))))
         (buffer (generate-new-buffer " *activity scroll test*")))
    (unwind-protect
        (progn
          (etaf-mount buffer (aaron-ui-activity-view state buffer)
                      '(:viewport-width 700 :viewport-height 12))
          (with-current-buffer buffer
            (let ((runtime (etaf-runtime-for-buffer buffer)))
              (goto-char (point-min))
              (ebox-scroll-down 3)
              (should (> (etaf-scroll-offset runtime 'activity-scroll) 0))
              (ebox-scroll-up 3)
              (should (= (etaf-scroll-offset runtime 'activity-scroll) 0))
              (should (eq (key-binding (kbd "<next>"))
                          #'ebox-scroll-page-down))
              (should (eq (key-binding [wheel-down])
                          #'ebox-wheel-scroll-down))
              (should (> (etaf-scroll-to runtime 'activity-scroll 'end) 0))
              (should (= (etaf-scroll-to runtime 'activity-scroll 0) 0)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest aaron-ui-documentation-home-links-are-local-and-rendered ()
  "The Emacs/Noema documentation map links to readable repository files."
  (let* ((file (expand-file-name "docs/guide/documentation-center.md"
                                 user-emacs-directory))
         (source (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (document (etaf-ui-markdown-parse source))
         (entry (etaf-ref (list :document document :file file
                                :title "Emacs · Noema 文档")))
         (buffer (etaf-mount
                  "*Aaron Docs Home Test*"
                  (etaf-view (aaron-ui-docs-page :entry entry))
                  '(:viewport-width 1200 :viewport-height 32))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (string-match-p "Emacs 配置" (buffer-string)))
            (should (string-match-p "Noema" (buffer-string))))
          (with-temp-buffer
            (insert source)
            (goto-char (point-min))
            (while (re-search-forward "\\]([^#)]+\\.md)" nil t)
              (let* ((relative (substring (match-string 0) 2 -1))
                     (target (expand-file-name relative
                                               (file-name-directory file))))
                (should (file-readable-p target))))))
      (kill-buffer buffer))))

(ert-deftest aaron-ui-documentation-history-returns-without-remounting ()
  "Previous and next update the mounted page through its ETAF ref."
  (let* ((first '(:file "first.md"))
         (second '(:file "second.md"))
         (aaron-ui-docs--history (list first second))
         (aaron-ui-docs--history-index 1)
         (aaron-ui-docs--entry (etaf-ref second)))
    (aaron-ui-docs--navigate -1)
    (should (= aaron-ui-docs--history-index 0))
    (should (equal (etaf-value aaron-ui-docs--entry) first))
    (aaron-ui-docs--navigate 1)
    (should (= aaron-ui-docs--history-index 1))
    (should (equal (etaf-value aaron-ui-docs--entry) second))))

(ert-deftest aaron-ui-documentation-next-opens-the-first-linked-page ()
  "The green title control works on the documentation home page."
  (let* ((home (expand-file-name "docs/guide/documentation-center.md"
                                 user-emacs-directory))
         (first (expand-file-name "docs/architecture.md"
                                  user-emacs-directory))
         (aaron-ui-docs--catalog (list home first))
         (aaron-ui-docs--history (list (list :file home)))
         (aaron-ui-docs--history-index 0)
         (aaron-ui-docs--entry (etaf-ref (list :file home)))
         opened)
    (cl-letf (((symbol-function 'aaron-ui-document-panel)
               (lambda (file &rest _args) (setq opened file))))
      (aaron-ui-docs--navigate 1))
    (should (equal opened first))))

(ert-deftest aaron-ui-documentation-title-has-shared-traffic-lights ()
  "The Aaron UI host keeps clickable lights and document navigation."
  (let ((aaron-ui-float--current
         (aaron-ui-float--make
          :title aaron-ui-docs--panel-title
          :green-left #'ignore :green-right #'ignore)))
    (let ((bar (aaron-ui-float--header)))
      (should-not (string-match-p "上一页\|下一页" bar))
      (let ((start 0) positions)
        (while (string-match "●" bar start)
          (push (match-beginning 0) positions)
          (setq start (match-end 0)))
        (setq positions (nreverse positions))
        (should (= (length positions) 3))
        (should (equal (mapcar (lambda (position)
                                 (plist-get (get-text-property position 'face bar)
                                            :foreground))
                               positions)
                       '("#FF5F57" "#FEBC2E" "#28C840")))
        (dolist (position positions)
          (should (commandp
                   (lookup-key (get-text-property position 'keymap bar)
                               [header-line mouse-1]))))
        (should (commandp
                 (lookup-key (get-text-property (nth 2 positions) 'keymap bar)
                             [header-line mouse-3])))))))

(ert-deftest aaron-ui-float-hide-keeps-buffer-close-obeys-owner ()
  "The generic host separates hiding from ending a caller's buffer."
  (let ((buffer (generate-new-buffer " *Aaron Float Lifecycle*"))
        panel shown-args)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
                  ((symbol-function 'posframe-workable-p) (lambda () t))
                  ((symbol-function 'posframe-show)
                   (lambda (&rest args)
                     (setq shown-args args)
                     (selected-frame)))
                  ((symbol-function 'posframe-hide) (lambda (&rest _) nil))
                  ((symbol-function 'posframe-delete-frame) (lambda (&rest _) nil))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (&rest _) nil)))
          (setq panel (aaron-ui-float-show
                       buffer :title "Lifecycle" :kill-on-close t))
          (should (eq (plist-get (cdr shown-args) :cursor) 'bar))
          (should (eql (plist-get (cdr shown-args) :position) 1))
          (should (aaron-ui-float-live-p panel))
          (should (aaron-ui-float-hide panel))
          (should (buffer-live-p buffer))
          (should (aaron-ui-float-close panel))
          (should-not (buffer-live-p buffer))
          (should-not (memq panel aaron-ui-float--panels)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'aaron-ui-etaf-tests)
;;; aaron-ui-etaf-tests.el ends here
