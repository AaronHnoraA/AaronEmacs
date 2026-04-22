;;; init-git-board.el --- Git status board -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'tabulated-list)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function magit-stage-files "magit-apply" (files &optional force))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-unstage-files "magit-apply" (files))
(declare-function my/git-tool-prepare-buffer "init-git-core" ())
(declare-function my/git-tool-quit-buffer "init-git-core" (origin-buffer))
(declare-function my/git-diff-file-with-head "init-git-diff" ())
(declare-function my/git-file-blame-toggle "init-git-core" ())
(declare-function my/git-file-log "init-git-core" ())

(defvar my/git-board-buffer-name "*Git Board*"
  "Buffer name used by the Git status board.")

(defvar-local my/git-board-origin-buffer nil
  "Origin buffer for the current Git board.")

(defvar-local my/git-board-repo-root nil
  "Repository root displayed by the current Git board buffer.")

(defvar-local my/git-board-branch-summary nil
  "Status summary for the current Git board.")

(define-derived-mode my/git-board-mode tabulated-list-mode "Git-Board"
  "Major mode for a compact Git status board."
  (setq tabulated-list-format [("X" 2 nil)
                               ("Y" 2 nil)
                               ("Path" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Path" nil))
  (tabulated-list-init-header))

(defun my/git-board--repo-root ()
  "Return the current repository root or signal a user error."
  (require 'magit-process)
  (require 'magit-git)
  (let ((repo-root (magit-toplevel default-directory)))
    (unless repo-root
      (user-error "Current buffer is not inside a Git repository"))
    repo-root))

(defun my/git-board--status-face (code)
  "Return a face appropriate for Git status CODE."
  (pcase code
    ((or "M" "A") 'diff-added)
    ((or "D" "U") 'diff-removed)
    ("R" 'font-lock-keyword-face)
    ("C" 'font-lock-constant-face)
    ("?" 'font-lock-warning-face)
    (_ 'shadow)))

(defun my/git-board--display-path (path)
  "Return a human-readable display string for PATH."
  (if (string-match-p " -> " path)
      path
    (abbreviate-file-name path)))

(defun my/git-board--entry-file (path)
  "Return the best file path to operate on for status PATH."
  (if (string-match "\\(.+\\) -> \\(.+\\)" path)
      (match-string 2 path)
    path))

(defun my/git-board--entries (repo-root)
  "Build tabulated list entries for REPO-ROOT."
  (let ((default-directory repo-root)
        (lines (process-lines "git" "status" "--short" "--branch" "--untracked-files=all"))
        (branch-summary ""))
    (when lines
      (when (string-prefix-p "## " (car lines))
        (setq branch-summary (substring (car lines) 3))
        (setq lines (cdr lines))))
    (setq-local my/git-board-branch-summary branch-summary)
    (mapcar
     (lambda (line)
       (let* ((x (substring line 0 1))
              (y (substring line 1 2))
              (path (string-trim-left (substring line 3)))
              (file (my/git-board--entry-file path)))
         (list
          file
          (vector
           (propertize x 'face (my/git-board--status-face x))
           (propertize y 'face (my/git-board--status-face y))
           (propertize (my/git-board--display-path path)
                       'face (if (or (string= x "?") (string= y "?"))
                                 'font-lock-warning-face
                               'default))))))
     lines)))

(defun my/git-board-refresh ()
  "Refresh the Git board."
  (interactive)
  (unless my/git-board-repo-root
    (setq-local my/git-board-repo-root (my/git-board--repo-root)))
  (setq tabulated-list-entries (my/git-board--entries my/git-board-repo-root))
  (setq header-line-format
        (format " Git: %s  [%s]  Keys: RET open, d diff, l log, B blame, s/u stage, g refresh, q quit"
                (abbreviate-file-name my/git-board-repo-root)
                (if (string-empty-p (or my/git-board-branch-summary ""))
                    "detached"
                  my/git-board-branch-summary)))
  (tabulated-list-print t))

(defun my/git-board-current-file ()
  "Return the file associated with the current board row."
  (or (tabulated-list-get-id)
      (user-error "No file at point")))

(defun my/git-board-visit ()
  "Visit the file at point."
  (interactive)
  (let* ((file (my/git-board-current-file))
         (path (expand-file-name file my/git-board-repo-root)))
    (unless (file-exists-p path)
      (user-error "File no longer exists: %s" file))
    (find-file path)))

(defun my/git-board--with-file-buffer (fn)
  "Call FN in a buffer visiting the current board file."
  (let* ((file (my/git-board-current-file))
         (path (expand-file-name file my/git-board-repo-root)))
    (unless (file-exists-p path)
      (user-error "File no longer exists: %s" file))
    (with-current-buffer (find-file-noselect path)
      (funcall fn))))

(defun my/git-board-diff ()
  "Diff the file at point against HEAD."
  (interactive)
  (my/git-board--with-file-buffer #'my/git-diff-file-with-head))

(defun my/git-board-log ()
  "Show log for the file at point."
  (interactive)
  (my/git-board--with-file-buffer #'my/git-file-log))

(defun my/git-board-blame ()
  "Toggle blame for the file at point."
  (interactive)
  (my/git-board--with-file-buffer #'my/git-file-blame-toggle))

(defun my/git-board-stage ()
  "Stage the file at point."
  (interactive)
  (let ((file (my/git-board-current-file)))
    (require 'magit-apply)
    (let ((default-directory my/git-board-repo-root))
      (magit-stage-files (list file)))
    (my/git-board-refresh)
    (message "Staged %s" file)))

(defun my/git-board-unstage ()
  "Unstage the file at point."
  (interactive)
  (let ((file (my/git-board-current-file)))
    (require 'magit-apply)
    (let ((default-directory my/git-board-repo-root))
      (magit-unstage-files (list file)))
    (my/git-board-refresh)
    (message "Unstaged %s" file)))

(defun my/git-board-quit ()
  "Quit the Git board and return to its origin buffer."
  (interactive)
  (my/git-tool-quit-buffer my/git-board-origin-buffer))

(defun my/git-board ()
  "Open the Git board for the current repository."
  (interactive)
  (let* ((origin-buffer (current-buffer))
         (repo-root (my/git-board--repo-root))
         (buffer (get-buffer-create my/git-board-buffer-name)))
    (with-current-buffer buffer
      (my/git-board-mode)
      (setq-local default-directory repo-root)
      (setq-local my/git-board-repo-root repo-root)
      (setq-local my/git-board-origin-buffer origin-buffer)
      (my/git-tool-prepare-buffer)
      (my/git-board-refresh))
    (switch-to-buffer buffer)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/git-board-mode 'normal)
  (evil-define-key* 'normal my/git-board-mode-map
    (kbd "RET") #'my/git-board-visit
    (kbd "<return>") #'my/git-board-visit
    (kbd "d") #'my/git-board-diff
    (kbd "l") #'my/git-board-log
    (kbd "B") #'my/git-board-blame
    (kbd "s") #'my/git-board-stage
    (kbd "u") #'my/git-board-unstage
    (kbd "g") #'my/git-board-refresh
    (kbd "q") #'my/git-board-quit))

(provide 'init-git-board)
;;; init-git-board.el ends here
