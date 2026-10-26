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

;;; init-state-files-tests.el ends here
