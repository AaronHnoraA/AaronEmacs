;;; tools/git/diff.el --- File history diff tools -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function magit-read-branch-or-commit "magit-git" (prompt &optional secondary-default exclude))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function +git-tool-prepare-buffer "tools/git/core" (&optional origin-buffer))
(declare-function +git-tool-quit-buffer "tools/git/core" (origin-buffer))
(declare-function +git-tool-register-cleanup "tools/git/core" (fn))

(defvar-local +git-diff-origin-buffer nil
  "Origin buffer for the current Git diff buffer.")

(defun +git-diff--delete-file-if-exists (path)
  "Delete PATH if it exists."
  (when (and path (file-exists-p path))
    (delete-file path)))

(defun +git-diff--cleanup-temp-files-on-kill (paths)
  "Delete PATHS when the current buffer is killed."
  (+git-tool-register-cleanup
   (lambda ()
     (mapc #'+git-diff--delete-file-if-exists paths))))

(defun +git-diff/quit ()
  "Quit the current Git diff buffer and return to its origin buffer."
  (interactive)
  (+git-tool-quit-buffer +git-diff-origin-buffer))

(defun +git-diff--setup-buffer (origin-buffer repo-root revision-file)
  "Configure the current Git diff buffer.

ORIGIN-BUFFER is the source buffer to return to on quit.  REPO-ROOT is used as
the local `default-directory'.  REVISION-FILE is cleaned up when this buffer is
killed."
  (setq-local default-directory repo-root)
  (setq-local revert-buffer-function nil)
  (setq-local +git-diff-origin-buffer origin-buffer)
  (+git-tool-prepare-buffer origin-buffer)
  (+git-diff--cleanup-temp-files-on-kill (list revision-file))
  (local-set-key (kbd "q") #'+git-diff/quit)
  (when (featurep 'evil)
    (dolist (state '(normal motion visual))
      (evil-local-set-key state (kbd "q") #'+git-diff/quit))))

(defun +git-diff--branch-base-revision ()
  "Return the base revision for the current branch.

Prefer the merge-base with the upstream branch.  If the current branch has no
upstream, fall back to the repository root commit."
  (or (magit-git-string "merge-base" "HEAD" "@{upstream}")
      (magit-git-string "rev-list" "--max-parents=0" "--first-parent" "HEAD")
      (user-error "Unable to determine branch base revision")))

(defun +git-diff--show-file-against-revision (revision label)
  "Show a unified diff between current file and REVISION.

LABEL is displayed in the diff header for the historical side."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (require 'magit-process)
  (require 'magit-git)
  (let* ((origin-buffer (current-buffer))
         (file (file-truename buffer-file-name))
         (default-directory (file-name-directory file))
         (repo-root (magit-toplevel default-directory)))
    (unless repo-root
      (user-error "Current file is not inside a Git repository"))
    (unless (file-in-directory-p file repo-root)
      (user-error "Current file is outside the repository root"))
    (let* ((relative-path (file-relative-name file repo-root))
           (revision-file (make-temp-file "emacs-git-revision-"))
           (diff-buffer-name (format "*git-diff:%s:%s*" label relative-path))
           (diff-buffer (get-buffer-create diff-buffer-name)))
      (with-temp-file revision-file
        (let ((default-directory repo-root))
          (condition-case err
              (magit-git-insert "show" (format "%s:%s" revision relative-path))
            (error
             (+git-diff--delete-file-if-exists revision-file)
             (signal (car err) (cdr err))))))
      (setq diff-buffer
            (diff-no-select
             revision-file
             origin-buffer
             `("-u"
               ,(format "--label=%s:%s" label relative-path)
               ,(format "--label=current:%s" relative-path))
             'no-async
             diff-buffer))
      (with-current-buffer diff-buffer
        (+git-diff--setup-buffer origin-buffer repo-root revision-file))
      (switch-to-buffer diff-buffer))))

(defun +git-diff/file-with-revision (revision)
  "Show a unified diff between current file and REVISION in the same window."
  (interactive
   (progn
     (require 'magit-process)
     (require 'magit-git)
     (list (magit-read-branch-or-commit "Compare current file with revision"))))
  (+git-diff--show-file-against-revision revision revision))

(defun +git-diff/file-with-head ()
  "Show a unified diff between current file and `HEAD'."
  (interactive)
  (+git-diff--show-file-against-revision "HEAD" "HEAD"))

(defun +git-diff/file-with-branch-base ()
  "Show a unified diff between current file and the current branch base."
  (interactive)
  (require 'magit-process)
  (require 'magit-git)
  (let ((revision (+git-diff--branch-base-revision)))
    (+git-diff--show-file-against-revision revision "branch-base")))

(define-leader-key!
  "g d" '(:def +git-diff/file-with-revision :which-key "diff file vs revision")
  "g =" '(:def +git-diff/file-with-head :which-key "diff file vs HEAD")
  "g b" '(:def +git-diff/file-with-branch-base :which-key "diff file vs branch base"))

(provide 'tools/git/diff)
;;; tools/git/diff.el ends here
