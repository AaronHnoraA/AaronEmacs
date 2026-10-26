;;; xenodium-workbench-tests.el --- Focused workbench regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/xenodium-workbench-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'copy-as-format)
(require 'ert)
(require 'lang/bazel/config)
(require 'app/calendar/config)
(require 'editor/editing/config)
(require 'emacs/files/config)
(require 'tools/git/github)
(require 'os/macos-tools/config)
(require 'editor/navigation/config)
(require 'lang/org/config)

(defmacro +workbench-test-with-region (beg end &rest body)
  "Activate BEG through END while evaluating BODY."
  (declare (indent 2))
  `(let ((transient-mark-mode t))
     (goto-char ,beg)
     (push-mark ,end t t)
     ,@body))

(ert-deftest +transpose-regions-dwim-swaps-disjoint-regions ()
  (with-temp-buffer
    (insert "one middle two")
    (+workbench-test-with-region 1 4
      (+editing/transpose-regions-dwim nil))
    (+workbench-test-with-region 12 15
      (+editing/transpose-regions-dwim nil))
    (should (equal (buffer-string) "two middle one"))
    (should-not +transpose-regions--buffer)))

(ert-deftest +transpose-regions-reset-releases-markers-and-overlay ()
  (with-temp-buffer
    (insert "alpha beta")
    (+workbench-test-with-region 1 6
      (+editing/transpose-regions-dwim nil))
    (should (overlayp +transpose-regions--overlay))
    (+editing/transpose-regions-reset)
    (should-not +transpose-regions--buffer)
    (should-not +transpose-regions--overlay)))

(ert-deftest +hash-dwim-hashes-the-requested-bounds ()
  (with-temp-buffer
    (insert "abc")
    (should
     (equal (+editing/hash-dwim 'sha256 (point-min) (point-max))
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (should (equal (current-kill 0)
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))))

(ert-deftest +copy-as-format-uses-the-whole-buffer-without-a-region ()
  (let ((copy-as-format-default "slack"))
    (with-temp-buffer
      (insert "alpha\nbeta\n")
      (goto-char (point-min))
      (+copy-as/format-markdown)
      (should (string-match-p "alpha" (current-kill 0)))
      (should (string-match-p "beta" (current-kill 0)))
      (should (= (point) (point-min)))
      (should (equal (buffer-string) "alpha\nbeta\n"))
      (should (equal copy-as-format-default "slack")))))

(ert-deftest +copy-as-format-preserves-an-active-region-and-default ()
  (let ((copy-as-format-default "slack"))
    (with-temp-buffer
      (insert "alpha beta")
      (let ((transient-mark-mode t))
        (goto-char (point-min))
        (push-mark 6 t t)
        (+copy-as/format-html)
        (should (equal (current-kill 0) "<pre><code>\nalpha\n</code></pre>\n"))
        (should (= (point) (point-min)))
        (should (= (mark) 6))
        (should mark-active)
        (should (equal copy-as-format-default "slack"))))))

(ert-deftest +narrow-or-widen-dwim-prefers-the-active-region ()
  (with-temp-buffer
    (insert "before middle after")
    (+workbench-test-with-region 8 14
      (+editing/narrow-or-widen-dwim nil))
    (should (buffer-narrowed-p))
    (should (equal (buffer-string) "middle"))
    (+editing/narrow-or-widen-dwim nil)
    (should-not (buffer-narrowed-p))))

(ert-deftest +narrow-or-widen-stays-in-org-for-source-and-quote-blocks ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n"
            "#+begin_quote\nquoted words\n#+end_quote\n* Other heading\n")
    (let ((original-buffer (current-buffer)))
      (goto-char (point-min))
      (search-forward "(+ 1 2)")
      (narrow-or-widen nil)
      (should (eq (current-buffer) original-buffer))
      (should (buffer-narrowed-p))
      (should (string-search "(+ 1 2)" (buffer-string)))
      (should-not (string-match-p "quoted words" (buffer-string)))
      (narrow-or-widen nil)
      (should-not (buffer-narrowed-p))
      (goto-char (point-min))
      (search-forward "quoted words")
      (narrow-or-widen nil)
      (should (eq (current-buffer) original-buffer))
      (should (buffer-narrowed-p))
      (should (string-match-p "quoted words" (buffer-string)))
      (should-not (string-search "(+ 1 2)" (buffer-string)))
      (narrow-or-widen nil)
      (should-not (buffer-narrowed-p))
      (goto-char (point-min))
      (search-forward "Heading")
      (narrow-or-widen nil)
      (should (buffer-narrowed-p))
      (should (string-match-p "quoted words" (buffer-string))))))

(ert-deftest +navigation-back-and-forward-use-the-recorded-jump ()
  (require 'better-jumper)
  (let ((file (make-temp-file "navigation-history-" nil ".txt"
                              "first line\nsecond line\n")))
    (unwind-protect
        (save-window-excursion
          (let ((buffer (find-file-noselect file)))
            (switch-to-buffer buffer)
            (better-jumper-clear-jumps)
            (goto-char (point-min))
            (+navigation--push-jump)
            (goto-char (point-max))
            (+navigation/back)
            (should (= (point) (point-min)))
            (+navigation/forward)
            (should (= (point) (point-max)))
            (better-jumper-clear-jumps)))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest +navigation-history-uses-native-xref-when-jump-list-is-empty ()
  (require 'better-jumper)
  (require 'xref)
  (let ((buffer (generate-new-buffer " *navigation xref fallback*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (let* ((window (selected-window))
                 (old-history (window-parameter window 'xref--history))
                 (xref-history-storage #'xref-window-local-history))
            (unwind-protect
                (progn
                  (set-window-parameter window 'xref--history (cons nil nil))
                  (better-jumper-clear-jumps)
                  (insert "first line\nsecond line\n")
                  (goto-char (point-min))
                  (xref-push-marker-stack)
                  (goto-char (point-max))
                  (+navigation/back)
                  (should (= (point) (point-min)))
                  (+navigation/forward)
                  (should (= (point) (point-max))))
              (set-window-parameter window 'xref--history old-history))))
      (kill-buffer buffer))))

(ert-deftest +structure-up-falls-back-to-native-list-navigation ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(outer (inner value))")
    (search-backward "value")
    (structure-up)
    (should (= (point) (save-excursion
                         (goto-char (point-min))
                         (search-forward "(inner")
                         (- (point) (length "(inner")))))
    (structure-up)
    (should (= (point) (point-min)))
    (should-error (structure-up) :type 'user-error)
    (should (= (point) (point-min)))))

(ert-deftest +structure-up-uses-the-nearest-c-tree-sitter-statement ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'c)))
  (with-temp-buffer
    (c-ts-mode)
    (insert "int outer(void) { if (1) { return 2; } }\n")
    (let ((inner-block (save-excursion
                         (goto-char (point-min))
                         (search-forward "{ return")
                         (- (point) (length "{ return")))))
      (search-backward "return")
      (structure-up)
      (should (= (point) inner-block)))))

(ert-deftest +last-change-mode-records-and-releases-its-marker ()
  (let ((buffer (generate-new-buffer " *last-change-test*"))
        (was-enabled +last-change-mode))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (text-mode)
          (+last-change-mode 1)
          (insert "changed")
          (should (eq (marker-buffer +last-change-marker) buffer))
          (+last-change-mode -1)
          (should-not +last-change-marker))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when was-enabled
        (+last-change-mode 1)))))

(ert-deftest +ediff-last-two-yanks-owns-its-own-cleanup ()
  "Another Ediff session must not clean up the yank comparison."
  (require 'ediff)
  (let ((kill-ring '("new text" "old text"))
        (ediff-cleanup-hook nil)
        control a b other)
    (unwind-protect
        (cl-letf (((symbol-function 'ediff-buffers)
                   (lambda (left right &optional startup-hooks _job-name)
                     (setq a left b right
                           control (generate-new-buffer "*Ediff yank test*"))
                     (with-current-buffer control
                       (mapc #'funcall startup-hooks))
                     control)))
          (+ediff/last-two-yanks)
          (should (buffer-live-p a))
          (should (buffer-live-p b))
          (should-not ediff-cleanup-hook)
          (setq other (generate-new-buffer "*Other Ediff test*"))
          (with-current-buffer other
            (run-hooks 'ediff-cleanup-hook))
          (should (buffer-live-p a))
          (should (buffer-live-p b))
          (with-current-buffer control
            (run-hooks 'ediff-cleanup-hook))
          (should-not (buffer-live-p a))
          (should-not (buffer-live-p b)))
      (dolist (buffer (list control a b other))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest +ediff-dwim-compares-the-visible-buffers ()
  "Two visible file buffers should be compared without rereading disk."
  (require 'ediff)
  (let ((first (generate-new-buffer "*Ediff visible A*"))
        (second (generate-new-buffer "*Ediff visible B*"))
        compared)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first)
          (with-current-buffer first
            (setq buffer-file-name "/tmp/ediff-visible-a.txt"))
          (split-window-right)
          (other-window 1)
          (switch-to-buffer second)
          (with-current-buffer second
            (setq buffer-file-name "/tmp/ediff-visible-b.txt"))
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (a b &rest _)
                       (setq compared (list a b))))
                    ((symbol-function 'ediff-files)
                     (lambda (&rest _)
                       (error "Ediff reread files instead of visible buffers"))))
            (+ediff/dwim))
          (should (equal (sort compared
                               (lambda (a b)
                                 (string-lessp (buffer-name a) (buffer-name b))))
                         (list first second))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest +ediff-window-layout-restores-after-last-session ()
  "Overlapping Ediff sessions on one frame share one original layout."
  (require 'ediff)
  (let* ((frame (selected-frame))
         (previous (frame-parameter frame '+ediff-window-state))
         first second restored baseline)
    (unwind-protect
        (progn
          (should (advice-member-p #'+ediff--setup-windows-a 'ediff-setup))
          (setq first
                (+ediff--setup-windows-a
                 (lambda (&rest _)
                   (generate-new-buffer "*Ediff layout first*"))))
          (setq baseline
                (plist-get (frame-parameter frame '+ediff-window-state)
                           :layout))
          (setq second
                (+ediff--setup-windows-a
                 (lambda (&rest _)
                   (generate-new-buffer "*Ediff layout second*"))))
          (should (eq (plist-get
                       (frame-parameter frame '+ediff-window-state)
                       :layout)
                      baseline))
          (cl-letf (((symbol-function 'set-window-configuration)
                     (lambda (layout &rest _)
                       (push layout restored))))
            (with-current-buffer first
              (run-hooks 'ediff-after-quit-hook-internal))
            (should-not restored)
            (with-current-buffer second
              (run-hooks 'ediff-after-quit-hook-internal))
            (should (equal restored (list baseline))))
          (should-not (frame-parameter frame '+ediff-window-state)))
      (set-frame-parameter frame '+ediff-window-state previous)
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest +ediff-window-layout-rolls-back-failed-setup ()
  "A failed Ediff setup restores the frame without leaving session state."
  (save-window-excursion
    (delete-other-windows)
    (let* ((frame (selected-frame))
           (previous (frame-parameter frame '+ediff-window-state)))
      (unwind-protect
          (progn
            (should-error
             (+ediff--setup-windows-a
              (lambda (&rest _)
                (split-window-right)
                (error "Ediff setup failed"))))
            (should (= (length (window-list)) 1))
            (should (eq (frame-parameter frame '+ediff-window-state)
                        previous)))
        (set-frame-parameter frame '+ediff-window-state previous)))))

(ert-deftest +ediff-window-layout-cleans-when-control-is-killed ()
  "Closing an Ediff control buffer directly releases its frame state."
  (let* ((frame (selected-frame))
         (previous (frame-parameter frame '+ediff-window-state))
         control)
    (unwind-protect
        (progn
          (setq control
                (+ediff--setup-windows-a
                 (lambda (&rest _)
                   (generate-new-buffer "*Ediff killed control*"))))
          (should (frame-parameter frame '+ediff-window-state))
          (kill-buffer control)
          (sleep-for 0.01)
          (should-not (frame-parameter frame '+ediff-window-state)))
      (set-frame-parameter frame '+ediff-window-state previous)
      (when (buffer-live-p control)
        (kill-buffer control)))))

(ert-deftest +files-create-parent-directories-honors-always-policy ()
  (let* ((root (make-temp-file "file-workbench-" t))
         (parent (expand-file-name "nested/deeper/" root))
         (file (expand-file-name "file.txt" parent)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (let ((+files-create-parent-directories 'always))
            (+files-create-parent-directories-h))
          (should (file-directory-p parent)))
      (delete-directory root t))))

(ert-deftest +files-copy-paths-joins-one-kill-ring-entry ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.txt")
    (should (equal (+files/copy-paths nil) "/tmp/example.txt"))
    (should (equal (current-kill 0) "/tmp/example.txt"))))

(ert-deftest +file-menu-uses-searchable-actions-and-native-image-crop ()
  "File actions have M-x names; crop remains Emacs's editing action."
  (should (member "file-menu"
                  (all-completions "file-menu" obarray #'commandp)))
  (should (eq (plist-get (cdr (transient-get-suffix
                              'command-hub "f")) :command)
              #'file-menu))
  (with-current-buffer (get-buffer "*scratch*")
    (evil-normal-state)
    (should (eq (key-binding (kbd "SPC f .")) #'file-menu)))
  (dolist (entry '(("o" . file-open)
                   ("O" . file-open-new-frame)
                   ("y" . file-copy-paths)
                   ("Y" . file-copy-parent-paths)
                   ("s" . dired-selected-size)
                   ("v" . dired-selection)))
    (let ((command (cdr entry)))
      (should (commandp command))
      (should (member (symbol-name command)
                      (all-completions (symbol-name command)
                                       obarray #'commandp)))
      (should (eq (plist-get
                   (cdr (transient-get-suffix 'file-menu
                                              (car entry)))
                   :command)
                  command))))
  (should-error (transient-get-suffix 'file-menu "c"))
  (should (eq (plist-get (cdr (transient-get-suffix
                              'editing-menu "p")) :command)
              #'image-crop)))

(ert-deftest +dired-selection-keeps-source-directory-list ()
  "A selected-file view should use a real Dired directory and keep its source."
  (let* ((root (make-temp-file "dired-selection-" t))
         (first (expand-file-name "first.txt" root))
         (second (expand-file-name "second.txt" root))
         (source nil)
         (selection nil))
    (unwind-protect
        (progn
          (with-temp-file first (insert "first"))
          (with-temp-file second (insert "second"))
          (setq source (dired-noselect root))
          (save-window-excursion
            (switch-to-buffer source)
            (dired-goto-file first)
            (dired-mark 1)
            (goto-char (point-min))
            (should (= (length (+files-selected-paths)) 1))
            (should (file-equal-p (car (+files-selected-paths)) first))
            (setq selection (+files/dired-selection))
            (should (eq (window-buffer) selection))
            (should-not (eq source selection))
            (with-current-buffer selection
              (should (file-equal-p (car dired-directory) root))
              (should (= (length (dired-get-marked-files)) 1))
              (should (file-equal-p (car (dired-get-marked-files)) first)))
            (with-current-buffer selection
              (call-interactively (lookup-key dired-mode-map (kbd "q"))))
            (should (eq (window-buffer) source)))
          (should (buffer-live-p source))
          (should-not (buffer-live-p selection))
          (with-current-buffer source
            (should (file-equal-p dired-directory root))
            (should (dired-goto-file (file-truename second)))))
      (dolist (buffer (list source selection))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest +dired-parse-du-kib-sums-each-entry ()
  (should (= (+dired--parse-du-kib "4\t/tmp/a\n12\t/tmp/b\n") 16)))

(ert-deftest +bazel-workspace-root-recognizes-bzlmod ()
  (let* ((root (make-temp-file "bazel-workbench-" t))
         (child (expand-file-name "pkg/nested/" root)))
    (unwind-protect
        (progn
          (make-directory child t)
          (with-temp-file (expand-file-name "MODULE.bazel" root))
          (should (equal (file-name-as-directory root)
                         (+bazel-workspace-root child))))
      (delete-directory root t))))

(ert-deftest +bazel-parse-targets-sorts-and-deduplicates ()
  (should (equal (+bazel--parse-targets "//z:t\n//a:t\n//z:t\n")
                 '("//a:t" "//z:t"))))

(ert-deftest +bazel-workbench-commands-are-top-level-definitions ()
  (dolist (command '(bazel-target-build bazel-target-test bazel-target-run
                     bazel-target-insert bazel-target-jump))
    (should (fboundp command))))

(ert-deftest +bazel-target-query-uses-the-logical-target ()
  (let ((cache-root (make-temp-file "bazel-cache-" t))
        (context (remote-context-create
                  :target-id "test-target"
                  :localname "/workspace/"
                  :workspace-root "/workspace/"
                  :source 'test))
        captured
        callback-value)
    (unwind-protect
        (let ((+bazel-cache-directory cache-root))
          (cl-letf (((symbol-function '+bazel-workspace-root)
                     (lambda (&optional _directory) "/workspace/"))
                    ((symbol-function '+bazel--context)
                     (lambda (&optional _root) context))
                    ((symbol-function '+bazel-executable)
                     (lambda (&optional _context) "/usr/bin/bazelisk"))
                    ((symbol-function 'remote-exec-async)
                     (lambda (program &rest options)
                       (setq captured (cons program options))
                       (funcall
                        (plist-get options :callback)
                        (remote-exec-result-create
                         :status 0 :stdout "//b:t\n//a:t\n" :stderr "")))))
            (+bazel-with-targets
             (lambda (root context executable targets)
               (setq callback-value (list root context executable targets)))
             t)
            (should (equal (car captured) "/usr/bin/bazelisk"))
            (should (equal (plist-get (cdr captured) :args)
                           '("query" "//..." "--output=label")))
            (should (eq (plist-get (cdr captured) :context) context))
            (should (equal callback-value
                           (list "/workspace/" context "/usr/bin/bazelisk"
                                 '("//a:t" "//b:t"))))))
      (delete-directory cache-root t))))

(ert-deftest +github-parses-and-labels-topic-json ()
  (let* ((items (+github--parse-topics
                 "[{\"number\":7,\"title\":\"Fix\",\"author\":{\"login\":\"sam\"},\"isPullRequest\":true,\"url\":\"https://example.test/7\"}]"))
         (item (car items)))
    (should (= (alist-get 'number item) 7))
    (should (string-match-p "PR.*#7.*Fix.*sam"
                            (+github--topic-candidate item)))))

(ert-deftest +github-repository-resolution-runs-on-the-logical-target ()
  (let (captured)
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/workspace/"))
              ((symbol-function 'remote-context)
               (lambda (_path) 'test-context))
              ((symbol-function 'remote-executable-find)
               (lambda (program context)
                 (should (equal program "gh"))
                 (should (eq context 'test-context))
                 "/usr/bin/gh"))
              ((symbol-function 'remote-exec)
               (lambda (program &rest options)
                 (setq captured (cons program options))
                 (remote-exec-result-create
                  :status 0 :stdout "owner/repo" :stderr ""))))
      (should (equal (+github--repo-info)
                     '("/workspace/" test-context "/usr/bin/gh" "owner/repo")))
      (should (equal (car captured) "/usr/bin/gh"))
      (should (equal (plist-get (cdr captured) :args)
                     '("repo" "view" "--json" "nameWithOwner"
                       "--jq" ".nameWithOwner")))
      (should (eq (plist-get (cdr captured) :context) 'test-context)))))

(ert-deftest +dired-marked-size-routes-du-and-target-native-paths ()
  (with-temp-buffer
    (setq major-mode 'dired-mode
          default-directory "/ssh:test:/workspace/")
    (let (captured)
      (cl-letf (((symbol-function '+files-selected-paths)
                 (lambda () '("/ssh:test:/workspace/a"
                              "/ssh:test:/workspace/b")))
                ((symbol-function 'remote-context)
                 (lambda (_path) 'test-context))
                ((symbol-function 'remote-executable-find)
                 (lambda (_program _context) "/usr/bin/du"))
                ((symbol-function 'remote-exec)
                 (lambda (program &rest options)
                   (setq captured (cons program options))
                   (remote-exec-result-create
                    :status 0 :stdout "4\ta\n12\tb\n" :stderr ""))))
        (should (= (+files/dired-marked-size) (* 16 1024)))
        (should (equal (car captured) "/usr/bin/du"))
        (should (equal (plist-get (cdr captured) :args)
                       '("-sk" "/workspace/a" "/workspace/b")))
        (should (eq (plist-get (cdr captured) :context) 'test-context))))))

(ert-deftest +macos-ns-color-conversion-uses-eight-bit-components ()
  (should (equal (+macos--ns-color-to-hex "65535,32768,0") "#ff8000")))

(ert-deftest macos-tools-menu-has-searchable-actions ()
  (skip-unless (eq system-type 'darwin))
  (dolist (command '(macos-tools-menu macos-pick-color macos-open-at-point))
    (should (commandp command))
    (should (member (symbol-name command)
                    (all-completions (symbol-name command) obarray #'commandp))))
  (should (eq (plist-get (cdr (transient-get-suffix 'macos-tools-menu "c"))
                        :command)
              'macos-pick-color))
  (should (eq (plist-get (cdr (transient-get-suffix 'macos-tools-menu "o"))
                        :command)
              'macos-open-at-point))
  (with-temp-buffer
    (evil-normal-state)
    (should (eq (key-binding (kbd "SPC a m")) 'macos-tools-menu))))

(ert-deftest macos-pick-color-reports-script-and-insertion-errors ()
  (skip-unless (eq system-type 'darwin))
  (cl-letf (((symbol-function 'do-applescript)
             (lambda (&rest _) (error "AppleScript bridge unavailable"))))
    (let ((err (should-error (macos-pick-color nil) :type 'error)))
      (should (string-match-p "AppleScript bridge unavailable"
                              (error-message-string err)))))
  (cl-letf (((symbol-function 'do-applescript)
             (lambda (&rest _) (error "User canceled. (-128)"))))
    (let ((err (should-error (macos-pick-color nil) :type 'user-error)))
      (should (string-match-p "Color selection cancelled"
                              (error-message-string err)))))
  (cl-letf (((symbol-function 'do-applescript)
             (lambda (&rest _) "65535,32768,0")))
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil))
      (with-temp-buffer
        (setq buffer-read-only t)
        (should-error (macos-pick-color t) :type 'buffer-read-only)
        (should (equal (current-kill 0) "#ff8000"))))))

(ert-deftest +org-link-dwim-wraps-a-region-with-the-clipboard-url ()
  (with-temp-buffer
    (org-mode)
    (insert "Link title")
    (kill-new "https://example.test/page")
    (+workbench-test-with-region (point-min) (point-max)
      (+org/insert-link-dwim))
    (should (equal (buffer-string)
                   "[[https://example.test/page][Link title]]"))))

(ert-deftest +year-calendar-renders-first-and-last-month ()
  (save-window-excursion
    (let ((buffer (+calendar/calendar-year 2026)))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "2026 January" (buffer-string)))
            (should (string-match-p "2026 December" (buffer-string)))
            (should +year-calendar-mode))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'xenodium-workbench-tests)
;;; xenodium-workbench-tests.el ends here
