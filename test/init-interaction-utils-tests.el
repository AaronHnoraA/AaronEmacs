;;; init-interaction-utils-tests.el --- Small interaction command tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-interaction-utils-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aaron-open)
(require 'config-tools)
(require 'editor/smartparens/config)
(require 'editor/navigation-extra/config)

(ert-deftest +pairs-cycle-delimiters-forward-and-backward ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(alpha [beta])")
    (goto-char (+ (point-min) 3))
    (+pairs/cycle-delimiters)
    (should (equal (buffer-string) "[alpha [beta]]"))
    (+pairs/cycle-delimiters)
    (should (equal (buffer-string) "{alpha [beta]}"))
    (+pairs/cycle-delimiters -1)
    (should (equal (buffer-string) "[alpha [beta]]"))))

(ert-deftest +pairs-cycle-delimiters-works-at-closing-edge ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(value)")
    (goto-char (point-max))
    (+pairs/cycle-delimiters)
    (should (equal (buffer-string) "[value]"))))

(ert-deftest +pairs-cycle-delimiters-rejects-string-content ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"(text)\"")
    (goto-char (+ (point-min) 2))
    (should-error (+pairs/cycle-delimiters) :type 'user-error)))

(ert-deftest +navigation-parse-location-formats ()
  (should
   (equal (+navigation-parse-location "src/main.el:12:3")
          '(:file "src/main.el" :line 12 :column 3)))
  (should
   (equal (+navigation-parse-location "/ssh:user@host:/tmp/main.el:8:2")
          '(:file "/ssh:user@host:/tmp/main.el" :line 8 :column 2)))
  (should
   (equal (+navigation-parse-location "file:///tmp/main.el#L7C4")
          '(:file "/tmp/main.el" :line 7 :column 4)))
  (should
   (equal (+navigation-parse-location "plain.el")
          '(:file "plain.el" :line nil :column nil))))

(ert-deftest +navigation-parse-location-rejects-zero-position ()
  (should-error (+navigation-parse-location "main.el:0") :type 'user-error)
  (should-error (+navigation-parse-location "main.el:2:0") :type 'user-error))

(ert-deftest +navigation-open-location-visits-position ()
  (let* ((directory (make-temp-file "emacs-location-test-" t))
         (file (expand-file-name "sample.el" directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "first\nsecond line\nthird\n"))
          (let ((default-directory directory))
            (setq buffer (+navigation/open-location "sample.el:2:4")))
          (with-current-buffer buffer
            (should (= (line-number-at-pos) 2))
            (should (= (current-column) 3))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest +navigation-open-location-distinguishes-local-and-source-target ()
  (with-temp-buffer
    (setq-local default-directory "/fs:location-audit:/work/")
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'file-exists-p) (lambda (_path) t))
              ((symbol-function 'file-directory-p) (lambda (_path) nil)))
      (should (equal (+navigation--resolve-location-file "/tmp/example.el")
                     "/tmp/example.el"))
      (should (equal (+navigation--resolve-location-file "src/example.el")
                     "/fs:location-audit:/work/src/example.el")))))

(ert-deftest +navigation-open-location-visits-a-local-file-from-remote ()
  (let* ((directory (make-temp-file "emacs-logical-location-" t))
         (file (expand-file-name "sample.el" directory))
         (source-directory "/fs:location-audit:/work/")
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "first\nsecond line\nthird\n"))
          (with-temp-buffer
            (setq default-directory source-directory)
            (let ((source (current-buffer)))
              (setq buffer
                    (open-file-location (format "%s:2:4" file)))
              (should (equal (buffer-local-value 'default-directory source)
                             source-directory))))
          (with-current-buffer buffer
            (should (= (line-number-at-pos) 2))
            (should (= (current-column) 3))
            (should-not (file-remote-p buffer-file-name))
            (should (file-equal-p buffer-file-name file))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest +symbols-preview-keeps-local-file-out-of-remote-context ()
  "A project-symbol preview must open a native file on the client."
  (require 'tools/symbols/config)
  (let ((default-directory "/fs:box:/work/")
        (candidate (+symbols-make-file-line-candidate
                    nil "/Users/test/project/main.lean" 3 "main"))
        opened visit-directory)
    (cl-letf (((symbol-function 'consult--marker-from-line-column)
               (lambda (buffer _line _column) buffer)))
      (+symbols--file-line-marker
       candidate
       (lambda (file)
         (setq opened file visit-directory default-directory)
         'preview-buffer)))
    (should (equal opened "/Users/test/project/main.lean"))
    (should (equal visit-directory temporary-file-directory))
    (should (equal default-directory "/fs:box:/work/"))))

(ert-deftest +open-routes-keep-native-local-target-from-remote-buffer ()
  "Central Emacs and system openers must use the chosen file's machine."
  (let ((default-directory "/fs:box:/work/")
        opened visit-directory)
    (cl-letf (((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq opened file visit-directory default-directory))))
      (aaron-open-file-with-backend "/Users/test/project/note.md" 'emacs))
    (should (equal opened "/Users/test/project/note.md"))
    (should (equal visit-directory temporary-file-directory))
    (should (equal (aaron-open-normalize-target opened) opened))))

(ert-deftest +harpoon-separates-machine-keys-and-opens-local-slot ()
  (require 'editor/harpoon/config)
  (let ((+harpoon-state
         '(("/fs:box:/work/" "/fs:local:/Users/test/project/main.el")))
        (default-directory "/fs:box:/work/")
        opened visit-directory)
    (should (equal (+harpoon--root-key "/Users/test/project")
                   "/fs:local:/Users/test/project/"))
    (should (equal (+harpoon--root-key "/fs:box:/Users/test/project")
                   "/fs:box:/Users/test/project/"))
    (cl-letf (((symbol-function '+harpoon-project-root)
               (lambda () "/fs:box:/work/"))
              ((symbol-function 'find-file)
               (lambda (file &rest _)
                 (setq opened file visit-directory default-directory))))
      (+harpoon/jump 1))
    (should (equal opened "/Users/test/project/main.el"))
    (should (equal visit-directory temporary-file-directory))))

(ert-deftest +macos-open-keeps-native-local-path-from-remote-buffer ()
  (require 'os/macos/config)
  (let ((default-directory "/fs:box:/work/"))
    (should (equal (+macos-open--normalize-target
                    "/Users/test/project/document.pdf")
                   "/Users/test/project/document.pdf"))))

(ert-deftest recentf-native-local-history-stays-local-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/"))
    (should (equal (+recentf-logical-file-name
                    "/Users/test/project/document.pdf")
                   "/Users/test/project/document.pdf"))
    (should (equal (+recentf-logical-file-name
                    "/fs:lab:/work/document.pdf")
                   "/fs:lab:/work/document.pdf"))))

(ert-deftest +navigation-open-location-invalid-line-keeps-the-current-window ()
  (let* ((file (make-temp-file "emacs-invalid-location-" nil ".txt"
                               "first line\n"))
         (source (generate-new-buffer " *invalid location source*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (let ((window-count (length (window-list))))
            (dolist (other-window '(nil t))
              (should-error
               (open-file-location (format "%s:99" file) other-window)
               :type 'user-error)
              (should (eq (window-buffer) source))
              (should (= (length (window-list)) window-count))
              (should-not (get-file-buffer file)))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when-let* ((target (get-file-buffer file)))
        (kill-buffer target))
      (delete-file file))))

(ert-deftest +navigation-open-location-invalid-line-preserves-existing-buffer ()
  (let* ((file (make-temp-file "emacs-existing-location-" nil ".txt"
                               "first line\nsecond line\n"))
         (target (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer target
          (goto-char (point-min))
          (forward-line 1)
          (let ((original-point (point)))
            (narrow-to-region original-point (point-max))
            (should-error
             (open-file-location (format "%s:99" file))
             :type 'user-error)
            (should (buffer-live-p target))
            (should (buffer-narrowed-p))
            (should (= (point) original-point))))
      (when (buffer-live-p target)
        (with-current-buffer target (widen))
        (kill-buffer target))
      (delete-file file))))

(ert-deftest +open-keybinding-guide-inhibits-markdown-redirect ()
  (let (redirect-inhibited buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'find-file)
                   (lambda (_file)
                     (setq redirect-inhibited +noema--inhibit-redirect
                           buffer (generate-new-buffer " *keybinding-guide-test*"))
                     buffer)))
          (should (eq (aaron/open-keybinding-guide) buffer))
          (should redirect-inhibited)
          (should (buffer-local-value 'view-mode buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +open-keybinding-guide-uses-client-context ()
  "The guide resolves on the client even from a Remote source buffer."
  (let (checked buffer)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:guide-test:/project/")
          (setq-local remote-buffer-target-id "guide-test")
          (cl-letf (((symbol-function 'find-file)
                     (lambda (_file)
                       (setq checked
                             (and (equal default-directory
                                         (file-name-as-directory user-emacs-directory))
                                  (not (bound-and-true-p remote-buffer-target-id))
                                  (equal process-environment
                                         (default-value 'process-environment))
                                  (equal exec-path (default-value 'exec-path))))
                       (setq buffer (generate-new-buffer " *guide-client-test*")))))
            (should (eq (aaron/open-keybinding-guide) buffer))
            (should checked)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +elisp-local-leader-has-one-visible-eval-route ()
  "The Emacs Lisp leader exposes the editing workbench's eval commands."
  (with-temp-buffer
    (emacs-lisp-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "SPC m e d")) 'eros-eval-defun))
    (should (eq (key-binding (kbd "SPC m e e")) 'eros-eval-last-sexp))
    (should (eq (key-binding (kbd "SPC m g f")) 'find-function))
    (should (eq (key-binding (kbd "SPC h m")) '+menus/mode-keybindings))))

(ert-deftest +workflow-docs-open-on-client-from-remote-buffer ()
  "Compile and LSP docs use client paths and remain editable."
  (let (buffers)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:guide-test:/project/")
          (setq-local remote-buffer-target-id "guide-test")
          (cl-letf (((symbol-function 'find-file)
                     (lambda (file)
                       (should (file-in-directory-p file user-emacs-directory))
                       (should (equal default-directory
                                      (file-name-as-directory user-emacs-directory)))
                       (should-not (bound-and-true-p remote-buffer-target-id))
                       (let ((buffer (generate-new-buffer " *workflow-doc-test*")))
                         (push buffer buffers)
                         buffer))))
            (dolist (command '(+compile/open-docs
                               +language-server/manager-open-docs))
              (let ((buffer (funcall command)))
                (should (buffer-live-p buffer))
                (should-not (buffer-local-value 'view-mode buffer))))))
      (mapc #'kill-buffer buffers))))

(ert-deftest config-store-opens-on-client-from-remote-buffer ()
  "The settings menu must not interpret its local store on a Remote target."
  (let (checked buffer)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:guide-test:/project/")
          (setq-local remote-buffer-target-id "guide-test")
          (setq-local process-environment '("PATH=/remote/bin"))
          (setq-local exec-path '("/remote/bin"))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (file)
                       (setq checked
                             (and (equal file config-store-file)
                                  (equal default-directory
                                         (file-name-as-directory user-emacs-directory))
                                  (not (bound-and-true-p remote-buffer-target-id))
                                  (equal process-environment
                                         (default-value 'process-environment))
                                  (equal exec-path (default-value 'exec-path))
                                  +noema--inhibit-redirect))
                       (setq buffer (generate-new-buffer " *config-store-test*")))))
            (should (eq (config-open-store) buffer))
            (should checked)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest config-file-item-uses-native-open-and-client-context ()
  "Registered files remain editable, including a file not created yet."
  (let* ((directory (make-temp-file "config-file-item-" t))
         (file (expand-file-name "new-setting.txt" directory))
         (config--registry (make-hash-table :test 'eq))
         (opened nil)
         (reloaded nil))
    (puthash 'test-file
             (list :name 'test-file :kind 'file :path file
                   :loader (lambda ()
                             (setq reloaded
                                   (and (equal default-directory
                                               (file-name-as-directory
                                                user-emacs-directory))
                                        (not (bound-and-true-p
                                              remote-buffer-target-id))))))
             config--registry)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/fs:file-item-test:/project/")
          (setq-local remote-buffer-target-id "file-item-test")
          (setq-local process-environment '("PATH=/remote/bin"))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (path)
                       (setq opened
                             (and (equal path file)
                                  (not (file-exists-p path))
                                  (equal default-directory
                                         (file-name-as-directory
                                          user-emacs-directory))
                                  (not (bound-and-true-p
                                        remote-buffer-target-id))
                                  (equal process-environment
                                         (default-value 'process-environment))))
                       (current-buffer))))
            (should (commandp 'config-file-open))
            (config-file-open 'test-file)
            (should opened))
          (config-file-reload 'test-file)
          (should reloaded))
      (delete-directory directory t))))

(provide 'init-interaction-utils-tests)

;;; init-interaction-utils-tests.el ends here
