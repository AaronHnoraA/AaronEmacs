;;; tools/git/board.el --- Git status board -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'tabulated-list)
(require 'aaron-open)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function magit-stage-files "magit-apply" (files &optional force))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-unstage-files "magit-apply" (files))
(declare-function +git-tool-prepare-buffer "tools/git/core" (&optional origin-buffer))
(declare-function +git-tool-quit-buffer "tools/git/core" (origin-buffer))
(declare-function +git-diff/file-with-head "tools/git/diff" ())
(declare-function +git/file-blame-toggle "tools/git/core" ())
(declare-function +git/file-log "tools/git/core" ())

(defvar +git-board-buffer-name "*Git Board*"
  "Buffer name used by the Git status board.")

(defvar-local +git-board-origin-buffer nil
  "Origin buffer for the current Git board.")

(defvar-local +git-board-repo-root nil
  "Repository root displayed by the current Git board buffer.")

(defvar-local +git-board-branch-summary nil
  "Status summary for the current Git board.")

(define-derived-mode +git-board-mode tabulated-list-mode "Git-Board"
  "Major mode for a compact Git status board."
  (setq tabulated-list-format [("X" 2 nil)
                               ("Y" 2 nil)
                               ("Path" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Path" nil))
  (tabulated-list-init-header))

(defun +git-board--repo-root ()
  "Return the current repository root or signal a user error."
  (require 'magit-process)
  (require 'magit-git)
  (let ((repo-root (magit-toplevel default-directory)))
    (unless repo-root
      (user-error "Current buffer is not inside a Git repository"))
    repo-root))

(defun +git-board--status-face (code)
  "Return a face appropriate for Git status CODE."
  (pcase code
    ((or "M" "A") 'diff-added)
    ((or "D" "U") 'diff-removed)
    ("R" 'font-lock-keyword-face)
    ("C" 'font-lock-constant-face)
    ("?" 'font-lock-warning-face)
    (_ 'shadow)))

(defun +git-board--display-path (path)
  "Show control characters in PATH without changing its row ID."
  (string-replace "\n" "\\n"
                  (string-replace "\r" "\\r"
                                  (string-replace "\t" "\\t" path))))

(defun +git-board--entries (repo-root)
  "Build tabulated list entries for REPO-ROOT."
  (let* ((records
          (with-temp-buffer
            (let ((default-directory repo-root)
                  (coding-system-for-read 'utf-8-unix))
              (unless (zerop (process-file "git" nil t nil "status" "--short"
                                           "--branch" "-z" "--untracked-files=all"))
                (error "Git status failed: %s" (buffer-string))))
            (split-string (buffer-string) "\0" t)))
         (branch-summary "")
         entries)
    (when (and records (string-prefix-p "## " (car records)))
      (setq branch-summary (substring (pop records) 3)))
    (setq-local +git-board-branch-summary branch-summary)
    (while records
      (let* ((record (pop records))
             (_ (unless (and (>= (length record) 4)
                             (eq (aref record 2) ?\s))
                  (error "Malformed Git status record: %S" record)))
             (x (substring record 0 1))
             (y (substring record 1 2))
             (path (substring record 3))
             (old-path (when (or (member x '("R" "C"))
                                 (member y '("R" "C")))
                         (or (pop records)
                             (error "Missing Git rename source: %S" record))))
             (display-path (if old-path
                               (format "%s -> %s" old-path path)
                             path)))
        (push
         (list path
               (vector
                (propertize x 'face (+git-board--status-face x))
                (propertize y 'face (+git-board--status-face y))
                (propertize (+git-board--display-path display-path)
                            'face (if (or (string= x "?") (string= y "?"))
                                      'font-lock-warning-face
                                    'default))))
         entries)))
    (nreverse entries)))

(defun +git-board/refresh ()
  "Refresh the Git board."
  (interactive)
  (unless +git-board-repo-root
    (setq-local +git-board-repo-root (+git-board--repo-root)))
  (setq tabulated-list-entries (+git-board--entries +git-board-repo-root))
  (setq header-line-format
        (format " Git: %s  [%s]  Keys: RET open, d diff, l log, B blame, s/u stage, g refresh, q quit"
                (abbreviate-file-name +git-board-repo-root)
                (if (string-empty-p (or +git-board-branch-summary ""))
                    "detached"
                  +git-board-branch-summary)))
  (tabulated-list-print t))

(defun +git-board-current-file ()
  "Return the file associated with the current board row."
  (or (tabulated-list-get-id)
      (user-error "No file at point")))

(defun +git-board/visit ()
  "Visit the file at point."
  (interactive)
  (let* ((file (+git-board-current-file))
         (path (expand-file-name file +git-board-repo-root)))
    (unless (file-exists-p path)
      (user-error "File no longer exists: %s" file))
    (aaron-open-visit-file (aaron-open-resolve-file-name path))))

(defun +git-board--with-file-buffer (fn)
  "Call FN in a buffer visiting the current board file."
  (let* ((file (+git-board-current-file))
         (path (expand-file-name file +git-board-repo-root)))
    (unless (file-exists-p path)
      (user-error "File no longer exists: %s" file))
    (with-current-buffer (aaron-open-visit-file-noselect
                          (aaron-open-resolve-file-name path))
      (funcall fn))))

(defun +git-board/diff ()
  "Diff the file at point against HEAD."
  (interactive)
  (+git-board--with-file-buffer #'+git-diff/file-with-head))

(defun +git-board/log ()
  "Show log for the file at point."
  (interactive)
  (+git-board--with-file-buffer #'+git/file-log))

(defun +git-board/blame ()
  "Toggle blame for the file at point."
  (interactive)
  (+git-board--with-file-buffer #'+git/file-blame-toggle))

(defun +git-board/stage ()
  "Stage the file at point."
  (interactive)
  (let ((file (+git-board-current-file)))
    (require 'magit-apply)
    (let ((default-directory +git-board-repo-root))
      (magit-stage-files (list file)))
    (+git-board/refresh)
    (message "Staged %s" file)))

(defun +git-board/unstage ()
  "Unstage the file at point."
  (interactive)
  (let ((file (+git-board-current-file)))
    (require 'magit-apply)
    (let ((default-directory +git-board-repo-root))
      (magit-unstage-files (list file)))
    (+git-board/refresh)
    (message "Unstaged %s" file)))

(defun +git-board/quit ()
  "Quit the Git board and return to its origin buffer."
  (interactive)
  (+git-tool-quit-buffer +git-board-origin-buffer))

(defun +git/board ()
  "Open the Git board for the current repository."
  (interactive)
  (let* ((origin-buffer (current-buffer))
         (repo-root (+git-board--repo-root))
         (buffer (get-buffer-create +git-board-buffer-name)))
    (with-current-buffer buffer
      (+git-board-mode)
      (setq-local default-directory repo-root)
      (setq-local +git-board-repo-root repo-root)
      (setq-local +git-board-origin-buffer origin-buffer)
      (+git-tool-prepare-buffer origin-buffer)
      (+git-board/refresh))
    (switch-to-buffer buffer)))

(set-evil-initial-state! '+git-board-mode 'normal)
(with-eval-after-load 'evil
  (evil-define-key* 'normal +git-board-mode-map
    (kbd "RET") #'+git-board/visit
    (kbd "<return>") #'+git-board/visit
    (kbd "d") #'+git-board/diff
    (kbd "l") #'+git-board/log
    (kbd "B") #'+git-board/blame
    (kbd "s") #'+git-board/stage
    (kbd "u") #'+git-board/unstage
    (kbd "g") #'+git-board/refresh
    (kbd "q") #'+git-board/quit))

(provide 'tools/git/board)
;;; tools/git/board.el ends here
