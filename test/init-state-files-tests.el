;;; init-state-files-tests.el --- Auto-save, lock and backup names -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-state-files-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'aaron-base)

(defun init-state-files-tests--deep-file (root)
  "Create a file under ROOT whose full path passes 255 characters."
  (let ((dir (expand-file-name
              (string-join (make-list 12 "研究笔记目录名称比较长的一层中文目录") "/")
              root)))
    (make-directory dir t)
    (let ((file (expand-file-name "关于量子计算和理论计算机科学的一篇很长的研究笔记.md" dir)))
      (write-region "hi\n" nil file nil 'silent)
      file)))

(ert-deftest aaron-state-file-name-fit-keeps-short-names ()
  "A name within the limit is returned unchanged."
  (should (equal (aaron-state-file-name-fit "/tmp/state/#!a!b.txt#")
                 "/tmp/state/#!a!b.txt#")))

(ert-deftest aaron-state-file-name-fit-hashes-long-names ()
  "A long name is hashed, keeping its markers and a recognizable tail."
  (let* ((long (concat "/tmp/state/#" (make-string 200 ?中) "note.md#"))
         (fit (aaron-state-file-name-fit long))
         (base (file-name-nondirectory fit)))
    (should (equal (file-name-directory fit) "/tmp/state/"))
    (should (<= (length base) aaron-state-file-name-max-length))
    (should (auto-save-file-name-p base))
    (should (string-suffix-p "note.md#" base))
    (should (equal fit (aaron-state-file-name-fit long)))
    (should (string-prefix-p ".#" (file-name-nondirectory
                                   (aaron-state-file-name-fit
                                    (concat "/tmp/" ".#" (make-string 300 ?x))))))))

(ert-deftest aaron-state-files-are-written-for-deep-paths ()
  "A file with a very long path still gets its auto-save, lock and backup."
  (let* ((root (make-temp-file "state-files-" t))
         (auto-save-file-name-transforms
          `((".*" ,(file-name-as-directory (expand-file-name "as" root)) t)))
         (lock-file-name-transforms
          `((".*" ,(file-name-as-directory (expand-file-name "lk" root)) t)))
         (backup-directory-alist `(("." . ,(expand-file-name "bk" root))))
         (create-lockfiles t)
         (make-backup-files t)
         (auto-save-default t)
         buffer)
    (unwind-protect
        (progn
          (dolist (dir '("as" "lk" "bk"))
            (make-directory (expand-file-name dir root) t))
          (setq buffer (find-file-noselect
                        (init-state-files-tests--deep-file root)))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "edit\n")
            ;; A lock is a dangling symlink named after the file's truename.
            (should (directory-files (expand-file-name "lk" root) nil "\\`\\.#"))
            (auto-save-mode 1)
            (do-auto-save t)
            (should (file-exists-p buffer-auto-save-file-name))
            (save-buffer)
            (should (directory-files (expand-file-name "bk" root) nil "~\\'"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest aaron-savehist-keeps-kill-ring-with-unprintable-properties ()
  "One killed string with an unprintable property must not lose the kill ring."
  (require 'savehist)
  (let* ((file (make-temp-file "savehist-" nil ".el"))
         (savehist-file file)
         (savehist-minibuffer-history-variables nil)
         (savehist-additional-variables '((kill-ring . 50)))
         (marker (with-temp-buffer (point-marker)))
         (kill-ring (list (propertize "with marker" 'aaron-test marker)
                          (propertize "plain" 'face 'bold))))
    (unwind-protect
        (progn
          (savehist-save)
          (let (saved)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (condition-case nil
                  (while t
                    (pcase (read (current-buffer))
                      (`(setq kill-ring ',value) (setq saved value))))
                (end-of-file nil)))
            (should (equal saved '("with marker" "plain"))))
          ;; The session's kill ring keeps its properties.
          (should (eq (get-text-property 0 'aaron-test (car kill-ring)) marker)))
      (delete-file file))))

(ert-deftest aaron-recentf-autosave-follows-the-setting ()
  "The recent-file list is saved on a timer that follows its setting."
  (require 'recentf)
  (let ((old (config-get 'recentf-autosave-interval))
        (recentf-mode-was recentf-mode))
    (unwind-protect
        (progn
          (recentf-mode 1)
          (config-set 'recentf-autosave-interval 900 t t)
          (should (timerp recentf--autosave-timer))
          (should (= (timer--repeat-delay recentf--autosave-timer) 900))
          (config-set 'recentf-autosave-interval nil t t)
          (should-not recentf--autosave-timer))
      (config-set 'recentf-autosave-interval old t t)
      (unless recentf-mode-was (recentf-mode -1)))))

(ert-deftest +files-guess-mode-after-saving-a-shebang ()
  "A shown fundamental-mode file gets its mode once a shebang is saved."
  (require 'emacs/files/config)
  (let* ((dir (make-temp-file "guess-mode-" t))
         (file (expand-file-name "tool" dir))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (should (eq major-mode 'fundamental-mode))
          (insert "#!/bin/sh\necho hi\n")
          (save-buffer)
          (should (derived-mode-p 'sh-base-mode)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-directory dir t))))

(ert-deftest +files-guess-mode-leaves-background-buffers ()
  "A buffer saved while not shown keeps its mode."
  (require 'emacs/files/config)
  (let* ((dir (make-temp-file "guess-mode-" t))
         (file (expand-file-name "tool" dir))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "#!/bin/sh\n")
          (save-buffer)
          (should (eq major-mode 'fundamental-mode)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-directory dir t))))

(ert-deftest aaron-auto-save-survives-a-large-deletion ()
  "Deleting most of a buffer must not switch its auto-save off."
  (let* ((root (make-temp-file "big-deletion-" t))
         (auto-save-file-name-transforms
          `((".*" ,(file-name-as-directory root) t)))
         (file (expand-file-name "note.txt" root))
         buffer)
    (unwind-protect
        (progn
          (write-region (make-string 20000 ?x) nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (auto-save-mode 1)
            (insert "y")
            (do-auto-save t)
            (delete-region (point-min) (- (point-max) 10))
            (do-auto-save)
            (should buffer-auto-save-file-name)
            (should (> buffer-saved-size -1))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest +files-copy-project-paths-relative-to-root ()
  "Paths are copied relative to the project, whatever spelling the root has."
  (require 'emacs/files/config)
  (require 'tools/project/config)
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "proj-" t))))
         (file (expand-file-name "src/a.el" dir))
         (kill-ring nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (write-region "" nil file nil 'silent)
          (dolist (root (list dir (concat "/fs:local:" dir)))
            (cl-letf (((symbol-function '+project-current-root) (lambda () root)))
              (with-temp-buffer
                (setq buffer-file-name file)
                (should (equal (+files/copy-project-paths) "src/a.el"))
                (setq buffer-file-name nil))))
          (cl-letf (((symbol-function '+project-current-root) #'ignore))
            (with-temp-buffer
              (setq buffer-file-name file)
              (should-error (+files/copy-project-paths) :type 'user-error)
              (setq buffer-file-name nil))))
      (delete-directory dir t))))

(ert-deftest +delete-current-file-trashes-and-forgets-recent-entry ()
  "Deleting the visited file trashes it and drops it from recent files."
  (require 'recentf)
  (let* ((dir (file-name-as-directory
               (file-truename (make-temp-file "delete-file-" t))))
         (file (expand-file-name "note.txt" dir))
         (delete-by-moving-to-trash t)
         trashed
         (recentf-list nil)
         (recentf-exclude nil)
         (recentf-mode-was recentf-mode))
    (unwind-protect
        (progn
          (recentf-mode 1)
          (write-region "x" nil file nil 'silent)
          (recentf-add-file file)
          (with-current-buffer (find-file-noselect file)
            ;; Never reach the real system trash from a test.
            (cl-letf (((symbol-function 'y-or-n-p) #'always)
                      ((symbol-function 'system-move-file-to-trash)
                       (lambda (name) (push name trashed) (delete-file name))))
              (+delete-current-file buffer-file-name)))
          (should-not (file-exists-p file))
          (should (equal trashed (list file)))
          (should-not (member (abbreviate-file-name file) recentf-list)))
      (unless recentf-mode-was (recentf-mode -1))
      (delete-directory dir t))))

(ert-deftest +rename-current-file-moves-the-recent-entry ()
  "Renaming the visited file replaces its recent-file entry."
  (require 'recentf)
  (let* ((dir (file-name-as-directory
               (file-truename (make-temp-file "rename-file-" t))))
         (old (expand-file-name "old.txt" dir))
         (new (expand-file-name "new.txt" dir))
         (recentf-list nil)
         (recentf-exclude nil)
         (recentf-mode-was recentf-mode)
         buffer)
    (unwind-protect
        (progn
          (recentf-mode 1)
          (write-region "x" nil old nil 'silent)
          (recentf-add-file old)
          (setq buffer (find-file-noselect old))
          (with-current-buffer buffer
            (+rename-current-file new))
          (should (file-exists-p new))
          (with-current-buffer buffer
            (should (equal (buffer-name) "new.txt"))
            (should-not (buffer-modified-p)))
          (should-not (member (abbreviate-file-name old) recentf-list))
          (should (member (abbreviate-file-name new) recentf-list)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (unless recentf-mode-was (recentf-mode -1))
      (delete-directory dir t))))

(ert-deftest aaron-undo-tree-history-skips-encrypted-and-git-message-files ()
  "No undo history is written for encrypted files or Git message files."
  (require 'undo-tree)
  (should (aaron-undo-tree-history-enabled-p "/a/notes.md"))
  (dolist (file '("/a/authinfo.gpg" "/a/secret.age" "/a/x.gpg~"
                  "/r/.git/COMMIT_EDITMSG" "/r/.git/rebase-merge/git-rebase-todo"))
    (should-not (aaron-undo-tree-history-enabled-p file)))
  (let* ((dir (file-name-as-directory (make-temp-file "undo-gpg-" t)))
         (aaron-undo-tree-history-dir (expand-file-name "history/" dir))
         (written nil))
    (unwind-protect
        (dolist (name '("notes.md" "secret.gpg"))
          (setq written nil)
          (with-temp-buffer
            ;; `with-temp-buffer' turns undo off; there must be history.
            (buffer-enable-undo)
            (setq buffer-file-name (expand-file-name name dir))
            (undo-tree-mode 1)
            (insert "decrypted secret")
            (undo-boundary)
            (insert ".")
            (undo-boundary)
            (cl-letf (((symbol-function 'write-region)
                       (lambda (&rest args) (push args written))))
              (undo-tree-save-history nil t))
            (if (equal name "notes.md")
                (should written)
              (should-not written))
            (setq buffer-file-name nil)))
      (delete-directory dir t))))

(ert-deftest aaron-enabling-a-command-writes-custom-file-not-init ()
  "Enabling a disabled command for good is recorded in `custom-file'."
  ;; novice loads after startup; the advice must still apply.
  (require 'novice)
  (let* ((dir (file-name-as-directory (make-temp-file "novice-" t)))
         (init (expand-file-name "init.el" dir))
         (custom-file (expand-file-name "custom.el" dir))
         (user-init-file init))
    (unwind-protect
        (progn
          (write-region ";; init\n" nil init nil 'silent)
          (write-region ";; custom\n" nil custom-file nil 'silent)
          (defalias 'aaron-test-disabled-command (lambda () (interactive)))
          (put 'aaron-test-disabled-command 'disabled t)
          (en/disable-command 'aaron-test-disabled-command nil)
          (should (string-match-p "aaron-test-disabled-command"
                                  (with-temp-buffer
                                    (insert-file-contents custom-file)
                                    (buffer-string))))
          (should (equal (with-temp-buffer
                           (insert-file-contents init)
                           (buffer-string))
                         ";; init\n")))
      (put 'aaron-test-disabled-command 'disabled nil)
      (dolist (buffer (buffer-list))
        (when (member (buffer-file-name buffer) (list init custom-file))
          (kill-buffer buffer)))
      (delete-directory dir t))))


;;; Startup and font bookkeeping

(ert-deftest aaron-bytecode-sweep-skips-node-trees ()
  "The startup `.elc' sweep descends Elisp trees and skips Node trees."
  (should (aaron-bytecode-scan-directory-p "/x/site-lisp/noema/lisp"))
  (dolist (name aaron-bytecode-scan-skipped-directories)
    (should-not (aaron-bytecode-scan-directory-p
                 (concat "/x/site-lisp/noema/" name)))))

(ert-deftest aaron-startup-restore-keeps-configured-gc ()
  "Restoring after init leaves GC settings the registry or gcmh have set."
  (let ((gc-cons-threshold 1234567)
        (gc-cons-percentage 0.3)
        (file-name-handler-alist nil))
    (aaron-restore-startup-runtime-settings)
    (should (= gc-cons-threshold 1234567))
    (should (= gc-cons-percentage 0.3)))
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage aaron-startup-gc-cons-percentage)
        (aaron-original-gc-cons-percentage 0.1)
        (file-name-handler-alist nil))
    (aaron-restore-startup-runtime-settings)
    (should (< gc-cons-threshold most-positive-fixnum))
    (should (= gc-cons-percentage 0.1))))

(ert-deftest aaron-suppressed-warnings-buffer-is-bounded ()
  "The suppressed-warning log cannot grow for the life of the session."
  (let ((aaron-suppressed-warnings-buffer " *suppressed-test*")
        (aaron-suppressed-warnings-max-lines 5))
    (unwind-protect
        (progn
          (dotimes (i 20)
            (aaron-log-to-suppressed-warnings 'test (format "line %d" i) :warning))
          (with-current-buffer aaron-suppressed-warnings-buffer
            (should (<= (count-lines (point-min) (point-max)) 6))
            (should (string-match-p "line 19" (buffer-string)))))
      (when-let* ((buffer (get-buffer aaron-suppressed-warnings-buffer)))
        (kill-buffer buffer)))))

(ert-deftest aaron-font-fontset-addition-happens-once ()
  "Re-applying fonts adds each family to one frame font's fontset once."
  (let ((aaron-font--fontset-bindings (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'set-fontset-font)
               (lambda (&rest _) (setq calls (1+ calls)))))
      (dotimes (_ 3) (aaron-font--fontset-add 'han "Family" 'prepend))
      (should (= calls 1))
      (aaron-font--fontset-add 'han "Other" 'prepend)
      (should (= calls 2)))))

(ert-deftest aaron-font-rescale-replaces-its-entry ()
  "Changing the Chinese scale replaces the string-keyed rescale entry."
  (let ((aaron-font-cn "Family")
        (face-font-rescale-alist nil))
    (dolist (aaron-scale-cn '(1.0 1.1))
      (aaron-font--apply-rescale))
    (should (equal face-font-rescale-alist '(("Family" . 1.1))))))

(ert-deftest aaron-editorconfig-is-a-prefetched-ancestor-marker ()
  "The per-visit `.editorconfig' walk is answered by the framework's scan."
  (require 'remote-fs)
  (should (member ".editorconfig" remote-ancestor-marker-names)))

(ert-deftest tldr-treats-a-nil-status-as-success ()
  "`url-retrieve' passes a nil status on success, and :error on failure."
  (require 'aaron-utils)
  (defvar url-http-end-of-headers)
  (let ((requests nil)
        (url-http-end-of-headers nil))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (push url requests)
                 (with-temp-buffer
                   (let ((buffer (generate-new-buffer " *tldr-response*")))
                     (with-current-buffer buffer
                       (insert "{\"encoding\":\"base64\",\"content\":\""
                               (base64-encode-string
                                (encode-coding-string "# 标题" 'utf-8) t)
                               "\"}")
                       (setq url-http-end-of-headers (point-min))
                       (funcall callback nil)))))))
      (unwind-protect
          (progn
            (tldr "ls")
            (should (= (length requests) 1))
            (with-current-buffer tldr-buffer-name
              (should (equal (buffer-string) "# 标题"))))
        (when-let* ((buffer (get-buffer tldr-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest +theme-nil-stays-meaningful-on-box-and-underline ()
  "A nil `:box' switches the box off; only invalid nils become `unspecified'."
  (let ((face (make-symbol "theme-nil-test")))
    (make-face face)
    (set-face-attribute face nil :box t :weight nil)
    (set-face-attribute face nil :box nil)
    (should-not (face-attribute face :box))
    (should (eq (face-attribute face :weight) 'unspecified))))

(ert-deftest +dir-locals-merge-keeps-subdirectories-and-eval-forms ()
  "Subdirectory keys are strings, and `eval' entries are not unique."
  (require 'emacs/dir-locals/config)
  (let ((merged (+dir-locals--merge
                 '(("src" . ((a . 1))) (nil . ((eval . (one)) (b . 1))))
                 '(("src" . ((a . 2))) (nil . ((eval . (two)) (b . 2)))))))
    (should (= (length merged) 2))
    (should (equal (cdr (assoc "src" merged)) '((a . 2))))
    (should (equal (cdr (assq nil merged))
                   '((eval . (one)) (eval . (two)) (b . 2))))))

(ert-deftest +dir-locals-read-does-not-evaluate ()
  "Reading a project's `.dir-locals.el' never runs its read-time forms."
  (require 'emacs/dir-locals/config)
  (let ((file (make-temp-file "dir-locals" nil ".el"
                              "((nil . ((a . #.(setq +dir-locals-test-ran t)))))"))
        (+dir-locals-test-ran nil))
    (defvar +dir-locals-test-ran)
    (unwind-protect
        (progn (+dir-locals--read-file file)
               (should-not (bound-and-true-p +dir-locals-test-ran)))
      (delete-file file))))

(ert-deftest +dired-directory-line-needs-no-stat-for-plain-rows ()
  "The listing answers for directories and files without touching the disk."
  (require 'emacs/dired/config)
  (with-temp-buffer
    (insert "  drwxr-xr-x  2 u g 4096 2026-01-01 00:00 dir\n"
            "  -rw-r--r--  1 u g 1 2026-01-01 00:00 file\n")
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (&rest _) (error "Touched the disk"))))
      (goto-char (point-min))
      (should (+dired--directory-line-p))
      (forward-line 1)
      (should-not (+dired--directory-line-p)))))

(ert-deftest shell-sentinel-closes-the-shell-window-not-the-selected-one ()
  "A shell that ends while another window is selected closes its own window."
  (let* ((selected (selected-window))
         (other (split-window selected))
         (shell (generate-new-buffer " *shell-sentinel*"))
         (process (start-process "shell-sentinel" shell "true")))
    (unwind-protect
        (progn
          (set-window-buffer other shell)
          (set-process-sentinel process #'ignore)
          (while (process-live-p process) (accept-process-output process 0.05))
          (shell-self-destroy-sentinel process "finished\n")
          (should (window-live-p selected))
          (should-not (window-live-p other))
          (should-not (buffer-live-p shell)))
      (when (window-live-p other) (delete-window other))
      (when (buffer-live-p shell) (kill-buffer shell)))))

(ert-deftest +diagnostics-panels-refresh-when-flymake-publishes ()
  "Flymake runs no hook after a save; publication drives the open panels."
  (should-not (boundp 'flymake-after-save-hook))
  (should (advice-member-p #'+diagnostics--clear-modeline-cache-a
                           'flymake--publish-diagnostics))
  (let ((scheduled 0)
        (+diagnostics-auto-refresh-mode t))
    (cl-letf (((symbol-function '+diagnostics--schedule-refresh-open-buffers)
               (lambda (&rest _) (setq scheduled (1+ scheduled)))))
      (with-temp-buffer (+diagnostics--clear-modeline-cache-a))
      (should (= scheduled 1))
      (let ((+diagnostics-auto-refresh-mode nil))
        (with-temp-buffer (+diagnostics--clear-modeline-cache-a)))
      (should (= scheduled 1)))))

(ert-deftest +diagnostics-panel-names-match-their-popup-rule ()
  "The popup rule describes the buffer name `+diagnostics-open' creates."
  (should (seq-some (lambda (rule)
                      (and (stringp (car rule))
                           (plist-get (cdr rule) :regexp)
                           (string-match-p (car rule) "*Diagnostics F12*")))
                    +popup-rules)))

(ert-deftest filetypes-send-archive-mode-only-what-it-reads ()
  "Tar and compressed names keep their own handling; bat files keep theirs."
  (dolist (name '("a.tar" "a.tgz" "a.txt.gz" "a.bz2" "a.xz" "a.zst"))
    (should-not (eq (assoc-default name auto-mode-alist #'string-match)
                    'archive-mode)))
  (should (eq (assoc-default "a.zip" auto-mode-alist #'string-match)
              'archive-mode))
  (should (eq (assoc-default "a.cmd" auto-mode-alist #'string-match)
              'bat-mode))
  (should (eq (assoc-default "a.md" auto-mode-alist #'string-match)
              '+noema-markdown-redirect-mode)))

(ert-deftest git-diff-keys-stay-out-of-the-shared-diff-map ()
  "A Git diff binds `q' for itself, not for every diff buffer."
  (require 'diff-mode)
  (require 'tools/git/diff)
  (let ((before (keymap-lookup diff-mode-map "q")))
    (with-temp-buffer
      (diff-mode)
      (cl-letf (((symbol-function '+git-tool-prepare-buffer) #'ignore)
                ((symbol-function '+git-tool-register-cleanup) #'ignore))
        (+git-diff--setup-buffer nil default-directory "/tmp/none"))
      (should (eq (keymap-lookup (current-local-map) "q") #'+git-diff/quit)))
    (should (eq (keymap-lookup diff-mode-map "q") before))))

;;; init-state-files-tests.el ends here
