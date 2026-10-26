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

;;; init-state-files-tests.el ends here
